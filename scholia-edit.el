;;; scholia-edit.el --- Temporary inline annotation editor  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/scholia
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A real, temporary input region below the annotated line, decorated with
;; overlays.  Corfu optionally supplies completion, reserving space only
;; while its popup is visible.
;; The reader cancels its change group before returning any annotation text.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'org-faces)
(require 'project)
(require 'subr-x)
(require 'text-mode)
(require 'scholia-vars)

(declare-function corfu-mode "ext:corfu" (&optional arg))
(declare-function evil-insert-state "ext:evil-states" (&optional arg))
(declare-function evil-change-state "ext:evil-core" (state &optional message))
(declare-function evil-normalize-keymaps "ext:evil-core" (&optional state))

(defvar corfu-mode)
(defvar corfu-map)
(defvar corfu-auto-delay)
(defvar evil-state)

(defface scholia-edit-body
  '((t (:inherit org-block :extend t)))
  "Face of the inline annotation field.
When its background is unspecified, darken the current theme's background."
  :group 'scholia)

(defface scholia-edit-border
  '((t (:inherit org-block-begin-line)))
  "Face of the bracket connecting the source line to its annotation input."
  :group 'scholia)

(defface scholia-edit-source
  '((t (:underline t)))
  "Face of the selected source text while an annotation is being written."
  :group 'scholia)

(cl-defstruct (scholia-edit--session
               (:constructor scholia-edit--make-session))
  begin end origin tail table file-table overlays spacer
  show-advice hide-advice timer accepted)

(defvar-local scholia-edit--session nil
  "The inline annotation reader active in this buffer, or nil.")

(defvar-local scholia-edit--source nil
  "Pair of markers bounding the source text of the active annotation.")

(defvar-local scholia-edit--source-map nil
  "Original local keymap, wrapped in a list, when borrowing a text input map.")

(defun scholia-edit--file-bounds (begin end)
  "Return the filename mention bounds at point within BEGIN and END.
The leading @ must follow whitespace or an opening delimiter, or start
the field.  Keep it outside the completion bounds, and skip email addresses."
  (save-excursion
    (skip-chars-backward "^ \t\n\r@\"'`()[]{}<>" begin)
    (when (and (> (point) begin)
               (eq (char-before) ?@)
               (or (= (1- (point)) begin)
                   (memq (char-before (1- (point)))
                         '(?\s ?\t ?\n ?\r ?\( ?\[ ?\{))))
      (let ((start (point)))
        (skip-chars-forward "^ \t\n\r@\"'`()[]{}<>" end)
        (cons start (point))))))

(defun scholia-edit--file-table (session)
  "Return SESSION's filename completion table, collecting project files once.
Project candidates are relative to their root.  Outside a project, use
ordinary filename completion relative to the source's current directory."
  (or (scholia-edit--session-file-table session)
      (let* ((project (project-current nil))
             (directory (if project (project-root project) default-directory))
             (files (when project
                      (mapcar (lambda (file) (file-relative-name file directory))
                              (project-files project)))))
        (setf (scholia-edit--session-file-table session)
              (lambda (string predicate action)
                (if project
                    (if (eq action 'metadata)
                        '(metadata (category . project-file))
                      (complete-with-action action files string predicate))
                  (let ((default-directory directory))
                    (completion-file-name-table string predicate action))))))))

(defun scholia-edit--capf ()
  "Complete files automatically after @, and history on manual invocation."
  (when-let* ((session scholia-edit--session)
              (begin (marker-position (scholia-edit--session-begin session)))
              (end (marker-position (scholia-edit--session-end session)))
              ((<= begin (point) end)))
    (if-let* ((bounds (scholia-edit--file-bounds begin end)))
        (list (car bounds) (cdr bounds) (scholia-edit--file-table session)
              :exclusive 'no :company-prefix-length t)
      (list begin end (scholia-edit--session-table session)
            :exclusive 'no :company-prefix-length 0))))

(defun scholia-edit--guard (begin end)
  "Reject a modification between BEGIN and END outside the annotation input."
  (when-let* ((session scholia-edit--session))
    (unless (<= (scholia-edit--session-begin session)
                begin end (scholia-edit--session-end session))
      (user-error "Only the annotation field is editable"))))

(defun scholia-edit--block-save (&rest _)
  "Prevent saving or killing a buffer during annotation input."
  (user-error "An annotation editor is still active"))

(defun scholia-edit--overlay (session begin end &rest properties)
  "Decorate BEGIN through END with PROPERTIES owned by SESSION."
  (let ((overlay (make-overlay begin end nil nil t)))
    (overlay-put overlay 'priority 1001)
    (overlay-put overlay 'scholia-edit t)
    (while properties
      (overlay-put overlay (pop properties) (pop properties)))
    (push overlay (scholia-edit--session-overlays session))
    overlay))

(defun scholia-edit--prefix (part)
  "Return PART of the bracket beside the source and annotation text."
  (propertize (pcase part ('start "╭ ") ('end "╰ ") (_ "│ "))
              'face 'scholia-edit-border))

(defun scholia-edit--draw ()
  "Refresh the source bracket and the active input's block face."
  (when-let* ((session scholia-edit--session))
    (let ((space (when-let* ((spacer (scholia-edit--session-spacer session)))
                   (overlay-get spacer 'after-string))))
      (mapc #'delete-overlay (scholia-edit--session-overlays session))
      (setf (scholia-edit--session-overlays session) nil)
      (let* ((begin (marker-position (scholia-edit--session-begin session)))
             (end (marker-position (scholia-edit--session-end session)))
             (origin (scholia-edit--session-origin session))
             (body (scholia-edit--body-face)))
        (save-excursion
          (goto-char origin)
          (while (< (point) begin)
            (let ((next (min begin (1+ (line-end-position))))
                  (prefix (get-char-property (point) 'line-prefix))
                  (wrap (get-char-property (point) 'wrap-prefix)))
              (scholia-edit--overlay
               session (point) next
               'line-prefix (concat (scholia-edit--prefix
                                     (if (= (point) origin) 'start 'middle))
                                    prefix)
               'wrap-prefix (concat (scholia-edit--prefix 'middle) wrap))
              (goto-char next)))
          (when scholia-edit--source
            (goto-char (car scholia-edit--source))
            (while (< (point) (cdr scholia-edit--source))
              (let ((eol (min (cdr scholia-edit--source) (line-end-position))))
                (when (< (point) eol)
                  (scholia-edit--overlay session (point) eol
                                         'face 'scholia-edit-source))
                (goto-char (min (cdr scholia-edit--source) (1+ eol))))))
          (goto-char begin)
          (while (<= (point) end)
            (let ((eol (min end (line-end-position))))
              (scholia-edit--overlay
               session (point) (1+ eol)
               'face body
               'line-prefix (scholia-edit--prefix (if (= eol end) 'end 'middle))
               'wrap-prefix (scholia-edit--prefix 'middle))
              (goto-char (1+ eol)))))
        (setf (scholia-edit--session-spacer session)
              (scholia-edit--overlay session end (1+ end) 'after-string space))))))

(defun scholia-edit--body-face ()
  "Return the input face with a subtly darkened theme background."
  (let ((background (face-background 'default nil t)))
    (if (and (eq (face-attribute 'scholia-edit-body :background) 'unspecified)
             (color-defined-p background))
        `(:inherit scholia-edit-body
                   :background ,(color-darken-name background 20))
      'scholia-edit-body)))

(defun scholia-edit--reserve-space (session lines)
  "Reserve LINES of display space below SESSION's input."
  (when-let* ((spacer (scholia-edit--session-spacer session))
              ((overlay-buffer spacer)))
    (overlay-put spacer 'after-string
                 (when (> lines 0)
                   (propertize (make-string lines ?\n) 'face 'default)))))

(defun scholia-edit--track-popup (session)
  "Track Corfu's visible popup size for SESSION's display spacer."
  (let ((buffer (current-buffer)))
    (setf (scholia-edit--session-show-advice session)
          (lambda (_pos _off _width lines &rest _)
            (when (eq buffer (current-buffer))
              (scholia-edit--reserve-space session (length lines))))
          (scholia-edit--session-hide-advice session)
          (lambda (&rest _) (scholia-edit--reserve-space session 0)))
    (advice-add 'corfu--popup-show :before
                (scholia-edit--session-show-advice session))
    (advice-add 'corfu--popup-hide :after
                (scholia-edit--session-hide-advice session))))

(defun scholia-edit--after-change (&rest _)
  "Refresh the input's decoration after a change."
  (scholia-edit--draw))

(defun scholia-edit--pre-command ()
  "Keep normal editing commands within the annotation field."
  (when-let* ((session scholia-edit--session))
    ;; Emacs can remove a change hook after it signals an error.
    (add-hook 'before-change-functions #'scholia-edit--guard -90 t)
    (goto-char (max (scholia-edit--session-begin session)
                    (min (point) (scholia-edit--session-end session))))))

(defun scholia-edit-accept ()
  "Save the current annotation input and close the inline editor."
  (interactive)
  (unless scholia-edit--session (user-error "No inline annotation editor"))
  (setf (scholia-edit--session-accepted scholia-edit--session) t)
  (exit-recursive-edit))

(defun scholia-edit-cancel ()
  "Discard the annotation input and close the inline editor."
  (interactive)
  (unless scholia-edit--session (user-error "No inline annotation editor"))
  (setf (scholia-edit--session-accepted scholia-edit--session) nil)
  (exit-recursive-edit))

(defun scholia-edit--without-completion (command)
  "Use COMMAND only when the completion menu is closed."
  (unless completion-in-region-mode command))

(defvar-keymap scholia-edit-mode-map
  :doc "Keys active while reading an inline annotation."
  "RET" '(menu-item "" scholia-edit-accept
                    :filter scholia-edit--without-completion)
  "<return>" '(menu-item "" scholia-edit-accept
                         :filter scholia-edit--without-completion)
  "C-c C-c" #'scholia-edit-accept
  "C-g" #'scholia-edit-cancel
  "C-SPC" '(menu-item "" completion-at-point
                      :filter scholia-edit--without-completion))

(defvar-local scholia-edit--emulation-map-alist nil
  "Save and cancel bindings active above the source's editing maps.")

(define-minor-mode scholia-edit-mode
  "Indicate that a temporary inline annotation reader is active.
Use `scholia-annotate' or `scholia-edit-annotation' with the global option
`scholia-annotation-editor' set to `inline' to open it."
  :lighter " Comment"
  :group 'scholia
  :keymap scholia-edit-mode-map)

(defconst scholia-edit--local-variables
  '(scholia-edit--session scholia-edit--source scholia-edit--source-map
                          scholia-edit-mode scholia-edit--emulation-map-alist
                          buffer-undo-list buffer-read-only buffer-auto-save-file-name
                          auto-save-visited-mode before-change-functions after-change-functions
                          first-change-hook before-save-hook write-file-functions
                          write-contents-functions kill-buffer-query-functions
                          change-major-mode-hook pre-command-hook post-command-hook
                          completion-at-point-functions completion-in-region-function
                          emulation-mode-map-alists minor-mode-overriding-map-alist
                          corfu-mode corfu-auto corfu-auto-prefix corfu-auto-trigger
                          corfu-quit-at-boundary
                          evil-previous-state evil-previous-state-alist evil-next-state)
  "Buffer-local settings borrowed by the inline annotation reader.")

(defun scholia-edit--remember-locals ()
  "Return the original bindings of the reader's buffer-local settings."
  (mapcar (lambda (symbol)
            (list symbol (local-variable-p symbol)
                  (and (boundp symbol) (symbol-value symbol))))
          scholia-edit--local-variables))

(defun scholia-edit--restore-locals (bindings)
  "Restore the buffer-local BINDINGS borrowed by the reader."
  (when scholia-edit--source-map
    (use-local-map (car scholia-edit--source-map))
    (kill-local-variable 'scholia-edit--source-map)
    (when (bound-and-true-p evil-local-mode)
      (evil-normalize-keymaps)))
  ;; Detach these here as well as restoring the binding, so that an active
  ;; reader can safely pick up new source decoration when this file is reloaded.
  (when scholia-edit--source
    (set-marker (car scholia-edit--source) nil)
    (set-marker (cdr scholia-edit--source) nil)
    (kill-local-variable 'scholia-edit--source))
  (dolist (binding bindings)
    (if (nth 1 binding)
        (set (make-local-variable (car binding)) (nth 2 binding))
      (kill-local-variable (car binding)))))

(defun scholia-edit--insert (session initial)
  "Insert SESSION's INITIAL text below the last selected source line."
  (when scholia-edit--source (goto-char (car scholia-edit--source)))
  (setf (scholia-edit--session-origin session)
        (copy-marker (line-beginning-position)))
  (when scholia-edit--source
    ;; A selection ending at the next line's beginning excludes that line.
    (goto-char (max (car scholia-edit--source) (1- (cdr scholia-edit--source)))))
  (end-of-line)
  (if (eq (char-after) ?\n) (forward-char) (insert "\n"))
  (setf (scholia-edit--session-begin session) (copy-marker (point)))
  (insert (or initial ""))
  (setf (scholia-edit--session-end session) (copy-marker (point)))
  (insert "\n")
  (setf (scholia-edit--session-tail session) (copy-marker (point) t))
  ;; The input's final newline belongs to the temporary UI, not the field.
  (set-marker-insertion-type (scholia-edit--session-end session) t)
  (set-text-properties (scholia-edit--session-begin session) (point) nil)
  (goto-char (scholia-edit--session-end session)))

(defun scholia-edit--enable-input-map ()
  "Borrow text editing keys in buffers whose source map suppresses insertion."
  (when (derived-mode-p 'special-mode)
    (unless scholia-edit--source-map
      (setq-local scholia-edit--source-map (list (current-local-map))))
    ;; Special modes and their Evil auxiliary maps disable text entry.  Keep
    ;; global editing customizations without inheriting source action keys.
    (use-local-map text-mode-map)
    (when (bound-and-true-p evil-local-mode)
      (evil-normalize-keymaps))))

(defun scholia-edit--setup (session)
  "Install SESSION's input guard, completion, and modal key bindings."
  (setq-local scholia-edit--session session
              buffer-read-only nil
              buffer-auto-save-file-name nil
              auto-save-visited-mode nil
              first-change-hook nil
              before-change-functions '(scholia-edit--guard)
              after-change-functions '(scholia-edit--after-change)
              before-save-hook '(scholia-edit--block-save)
              write-file-functions '(scholia-edit--block-save)
              write-contents-functions '(scholia-edit--block-save)
              kill-buffer-query-functions '(scholia-edit--block-save)
              change-major-mode-hook '(scholia-edit--block-save)
              completion-at-point-functions '(scholia-edit--capf)
              minor-mode-overriding-map-alist
              (copy-tree minor-mode-overriding-map-alist)
              scholia-edit--emulation-map-alist
              `((scholia-edit-mode . ,scholia-edit-mode-map))
              emulation-mode-map-alists
              (cons 'scholia-edit--emulation-map-alist emulation-mode-map-alists))
  (add-hook 'pre-command-hook #'scholia-edit--pre-command -90 t)
  (when (featurep 'corfu)
    (setq-local corfu-auto t
                ;; The CAPF supplies an automatic prefix only for filenames.
                corfu-auto-prefix 1
                corfu-auto-trigger ""
                corfu-quit-at-boundary t)
    (scholia-edit--track-popup session)
    (corfu-mode 1))
  (scholia-edit-mode 1)
  (scholia-edit--enable-input-map)
  (when (and (bound-and-true-p evil-local-mode)
             (not (eq evil-state 'emacs)))
    (evil-insert-state))
  (scholia-edit--draw))

(defun scholia-edit--start-completion (buffer session)
  "Start filename completion for SESSION if BUFFER still has focus."
  (when (and (buffer-live-p buffer)
             (eq buffer (window-buffer (selected-window))))
    (with-current-buffer buffer
      (when (eq scholia-edit--session session)
        ;; Source command hooks belong to real user commands.  Only Corfu
        ;; should see this initial request for automatic completion.
        (let ((this-command 'self-insert-command)
              (corfu-auto-delay 0))
          (run-hook-wrapped
           'post-command-hook
           (lambda (function)
             (when (and (symbolp function)
                        (string-prefix-p "corfu" (symbol-name function)))
               (funcall function))
             nil)))))))

(defun scholia-edit-read (table &optional initial bounds)
  "Read annotation text inline using completion TABLE and INITIAL input.
BOUNDS is the source's (BEGIN . END) range, defaulting to the active region.
Underline that text and connect its first line to the field below its last
line.  Without a range, put the field below the current document line.
\\<scholia-edit-mode-map>\\[scholia-edit-accept] saves literal input,
and \\[scholia-edit-cancel] quits.  While completion is active, its own key
bindings apply, including RET.  The direct save binding remains available.
Other editing keys are inherited from the source buffer, using a temporary
text map in special modes; Evil users start in insert state.
Corfu is optional; otherwise use ordinary completion at point.
Use C-SPC to request annotation history.  Typing @ starts file completion.

Temporary text is rolled back before returning.  Point, narrowing, undo,
modification status, hooks, and borrowed local settings are restored on
save, quit, and error.  Cancellation signals `quit', like a minibuffer."
  (when scholia-edit--session (user-error "An annotation editor is already open"))
  (require 'corfu nil t)
  ;; New Corfu versions keep their automatic completion options here.
  (when (featurep 'corfu) (require 'corfu-auto nil t))
  (when completion-in-region-mode (completion-in-region-mode -1))
  (let* ((buffer (current-buffer))
         (bindings (scholia-edit--remember-locals))
         (modified (buffer-modified-p))
         (corfu-was-enabled (bound-and-true-p corfu-mode))
         (evil-state-to-restore (and (bound-and-true-p evil-local-mode) evil-state))
         (session (scholia-edit--make-session :table table))
         (undo-limit most-positive-fixnum)
         (undo-strong-limit most-positive-fixnum)
         (undo-outer-limit nil)
         (amalgamating-undo-limit 0)
         group result)
    (save-window-excursion
      (save-mark-and-excursion
        (save-restriction
          (unwind-protect
              (progn
                ;; A private undo log prevents input undo from reaching the
                ;; document's history, even when undo was originally disabled.
                (setq-local buffer-undo-list nil)
                (when-let* ((range (or bounds
                                       (and (use-region-p)
                                            (cons (region-beginning) (region-end))))))
                  (setq-local scholia-edit--source
                              (cons (copy-marker (car range))
                                    (copy-marker (cdr range)))))
                (setq group (prepare-change-group))
                (activate-change-group group)
                (let ((inhibit-read-only t)
                      (inhibit-modification-hooks t))
                  (scholia-edit--insert session initial))
                (undo-boundary)
                (deactivate-mark)
                (scholia-edit--setup session)
                ;; Start after the recursive command loop's initial hooks:
                ;; Corfu cancels pending automatic completion on entry.
                ;; Only an initial filename mention can open automatically.
                (when (featurep 'corfu)
                  (setf (scholia-edit--session-timer session)
                        (run-at-time 0.1 nil #'scholia-edit--start-completion
                                     buffer session)))
                (recursive-edit)
                (when (scholia-edit--session-accepted session)
                  (with-current-buffer buffer
                    (setq result
                          (buffer-substring-no-properties
                           (scholia-edit--session-begin session)
                           (scholia-edit--session-end session))))))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (unwind-protect
                    (progn
                      (when (scholia-edit--session-timer session)
                        (cancel-timer (scholia-edit--session-timer session)))
                      (when completion-in-region-mode
                        (completion-in-region-mode -1))
                      (when (and (featurep 'corfu) (not corfu-was-enabled))
                        (corfu-mode -1))
                      (when evil-state-to-restore
                        (evil-change-state evil-state-to-restore)))
                  (unwind-protect
                      (let ((inhibit-read-only t)
                            (inhibit-modification-hooks t))
                        (mapc #'delete-overlay
                              (scholia-edit--session-overlays session))
                        (when (scholia-edit--session-show-advice session)
                          (advice-remove 'corfu--popup-show
                                         (scholia-edit--session-show-advice session)))
                        (when (scholia-edit--session-hide-advice session)
                          (advice-remove 'corfu--popup-hide
                                         (scholia-edit--session-hide-advice session)))
                        (when group (cancel-change-group group)))
                    (scholia-edit--restore-locals bindings)
                    (set-buffer-modified-p modified)
                    (dolist (marker
                             (list (scholia-edit--session-begin session)
                                   (scholia-edit--session-end session)
                                   (scholia-edit--session-origin session)
                                   (scholia-edit--session-tail session)))
                      (when marker (set-marker marker nil)))))))))))
    (or result (signal 'quit nil))))

(provide 'scholia-edit)
;;; scholia-edit.el ends here
