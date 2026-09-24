;;; scholia-edit-test.el --- Inline annotation lifecycle tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'scholia-test-helper)
(require 'scholia-core)
(require 'scholia-edit)
(require 'scholia-ui)

(when (require 'corfu nil t) (require 'corfu-auto nil t))

(defvar corfu-auto)
(defvar corfu-auto-delay)
(defvar corfu-auto-trigger)
(defvar corfu-map)
(defvar corfu--index)
(defvar corfu--candidates)
(defvar corfu-count)
(defvar corfu-min-width)
(defvar corfu-max-width)
(defvar evil-state)
(defvar evil-insert-state-map)
(defvar evil-mode-map-alist)
(declare-function corfu-mode "ext:corfu" (&optional arg))
(declare-function corfu-next "ext:corfu" (&optional n))
(declare-function corfu-previous "ext:corfu" (&optional n))
(declare-function corfu-insert "ext:corfu" ())
(declare-function corfu-quit "ext:corfu" ())
(declare-function corfu--setup "ext:corfu" (beg end table pred))
(declare-function corfu--exhibit "ext:corfu" ())
(declare-function evil-local-mode "ext:evil-core" (&optional arg))
(declare-function evil-normal-state "ext:evil-states" (&optional arg))
(declare-function evil-define-key* "ext:evil-core" (state keymap key def &rest bindings))

(defun scholia-edit-test--input ()
  "Return the active editor's literal input."
  (buffer-substring-no-properties
   (cera--session-begin cera--active)
   (cera--session-end cera--active)))

(defmacro scholia-edit-test--reading (interaction &rest body)
  "Run BODY with INTERACTION standing in for recursive keyboard input."
  (declare (indent 1) (debug (form body)))
  `(cl-letf (((symbol-function 'recursive-edit) ,interaction)
             ((symbol-function 'exit-recursive-edit) #'ignore))
     ,@body))

(defun scholia-edit-test--source-ranges ()
  "Return the sorted ranges the active reader marks as its source.
The reader's own overlays are asked for by the property it labels them
with, rather than by the shape of the face they wear, since scholia
marks them in its session colour and hands that face to the reader."
  (sort (cl-loop for overlay in (overlays-in (point-min) (point-max))
                 when (overlay-get overlay 'cera-source-mark)
                 collect (cons (overlay-start overlay) (overlay-end overlay)))
        (lambda (a b) (< (car a) (car b)))))

(defun scholia-edit-test--draw-allocations ()
  "Return how many overlays one redraw of the active field allocates."
  (let ((calls 0)
        (original (symbol-function 'make-overlay)))
    (cl-letf (((symbol-function 'make-overlay)
               (lambda (&rest arguments)
                 (setq calls (1+ calls))
                 (apply original arguments))))
      (cera--draw))
    calls))

(ert-deftest scholia-edit-at-completes-project-files-without-replacing-prose ()
  "A mention uses project-relative filenames and leaves surrounding prose intact."
  (skip-unless (executable-find "git"))
  (scholia-test-with-session-directory
    (let ((default-directory scholia-session-directory))
      (should (zerop (call-process "git" nil nil nil "init" "--quiet")))
      (make-directory "src")
      (with-temp-file "src/engine.el" (insert ";; engine\n"))
      (with-temp-file ".gitignore" (insert "ignored.el\n"))
      (with-temp-file "ignored.el" (insert "ignored\n"))
      (with-temp-buffer
        (setq default-directory (expand-file-name "src/"))
        (insert "source\nnext")
        (goto-char 2)
        (scholia-edit-test--reading
            (lambda ()
              (goto-char (+ (cera--session-begin cera--active)
                            (length "See @src/eng")))
              (let* ((capf (cera--capf))
                     (table (nth 2 capf))
                     (files (all-completions "" table)))
                (should (member "src/engine.el" files))
                (should-not (member "ignored.el" files))
                (should (eq (completion-metadata-get
                             (completion-metadata "" table nil) 'category)
                            'project-file))
                (should (equal (buffer-substring-no-properties (nth 0 capf) (nth 1 capf))
                               "src/eng"))
                (should (equal (car (completion-try-completion "engine" table nil 6))
                               "src/engine.el"))
                (backward-char 2)
                (should (equal (seq-take (cera--capf) 2) (seq-take capf 2)))
                (forward-char 2))
              (completion-at-point)
              (should (equal (scholia-edit-test--input) "See @src/engine.el for context"))
              (cera-accept))
          (should (equal (scholia-edit-read '("prior note") "See @src/eng for context")
                         "See @src/engine.el for context")))
        (should (equal (buffer-string) "source\nnext"))))))

(ert-deftest scholia-edit-at-completes-local-files-and-leaves-email-alone ()
  "An empty @ offers local files and directories; email remains ordinary text."
  (scholia-test-with-session-directory
    (let ((default-directory scholia-session-directory)
          (history '("prior note")))
      (make-directory "docs")
      (with-temp-file "docs/guide.md" (insert "guide\n"))
      (with-temp-file "notes.md" (insert "notes\n"))
      (with-temp-buffer
        (insert "source")
        (scholia-edit-test--reading
            (lambda ()
              (let ((capf (cera--capf)))
                (should (= (nth 0 capf) (nth 1 capf)))
                (should (member "notes.md" (all-completions "" (nth 2 capf))))
                (should (member "docs/" (all-completions "" (nth 2 capf)))))
              (insert "docs/gu")
              (completion-at-point)
              (should (equal (scholia-edit-test--input) "Read @docs/guide.md"))
              (insert " or contact name@example.com")
              (should (eq (nth 2 (cera--capf)) history))
              (cera-accept))
          (should (equal (scholia-edit-read history "Read @")
                         "Read @docs/guide.md or contact name@example.com")))))))

(ert-deftest scholia-edit-global-setting-selects-inline-in-every-buffer ()
  "The global option routes the regular add command through inline input."
  (scholia-test-with-session-directory
    (let ((original (default-value 'scholia-annotation-editor)))
      (unwind-protect
          (progn
            (set-default 'scholia-annotation-editor 'inline)
            (dolist (word '("alpha" "gamma"))
              (let ((source (concat word " beta\nnext\n")))
                (scholia-test-with-temp-file-buffer buffer source
                  (let ((scholia-project-root-function (lambda () nil))
                        (scholia-autosave nil)
                        (scholia-session "inline"))
                    (scholia-mode 1)
                    (goto-char 1)
                    (set-buffer-modified-p nil)
                    (should-not (local-variable-p 'scholia-annotation-editor))
                    (scholia-edit-test--reading
                        (lambda ()
                          (should (equal (scholia-edit-test--source-ranges) '((1 . 6))))
                          (insert "new note")
                          (cera-accept))
                      (call-interactively #'scholia-annotate))
                    (should-not (buffer-modified-p))
                    (should (equal (buffer-string) source))
                    (let* ((record (scholia-db-record (scholia-session-file)
                                                      (buffer-file-name buffer)))
                           (note (car (scholia-db-record-annotations record))))
                      (should (equal (scholia-db-annotation-text note) "new note"))
                      (should (equal (scholia-db-annotation-annotated-text note) word))
                      (should (equal (scholia-db-annotation-interval note) '(1 . 6)))))))))
        (set-default 'scholia-annotation-editor original)))))

(ert-deftest scholia-edit-existing-note-retains-identity-owner-and-replies ()
  "Editing updates text in place and stores it without breaking its thread."
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer "alpha"
      (let ((scholia-project-root-function (lambda () nil))
            (scholia-autosave nil)
            (scholia-annotation-editor 'inline)
            (scholia-session "inline"))
        (scholia-mode 1)
        (goto-char 1)
        (scholia-annotate "old note")
        (scholia-reply-to "reply")
        (let* ((chain (scholia-core--select-chain))
               (id (scholia-core--chain-id chain))
               (color (scholia-chain-color-index chain))
               (owner (scholia-chain-owner chain)))
          (scholia-edit-test--reading
              (lambda ()
                (should (equal (scholia-edit-test--input) "old note"))
                (should (equal (scholia-edit-test--source-ranges) '((1 . 6))))
                (delete-region (cera--session-begin cera--active)
                               (cera--session-end cera--active))
                (insert "edited note")
                (cera-accept))
            (call-interactively #'scholia-edit-annotation))
          (should (equal chain (scholia-core--select-chain)))
          (should (equal (scholia-core--chain-id chain) id))
          (should (= (scholia-chain-color-index chain) color))
          (should (equal (scholia-chain-owner chain) owner))
          (let ((rendered (scholia-render-note chain)))
            (should (string-match-p (regexp-quote "edited note") rendered))
            (should (string-match-p (regexp-quote "reply") rendered))
            (should-not (string-match-p (regexp-quote "old note") rendered)))
          (let* ((record (scholia-db-record (scholia-session-file)
                                            (buffer-file-name buffer)))
                 (notes (scholia-db-record-annotations record))
                 (root (seq-find (lambda (note) (equal (scholia-db-annotation-id note) id))
                                 notes))
                 (reply (seq-find #'scholia-db-annotation-reply-p notes)))
            (should (equal (scholia-db-annotation-text root) "edited note"))
            (should (equal (scholia-db-annotation-text reply) "reply"))
            (should (equal (scholia-db-annotation-reply-to reply) id))))))))

(ert-deftest scholia-edit-cancel-or-empty-note-does-not-change-annotations ()
  "Cancel and empty input preserve a note, and cancel creates no new note."
  (with-temp-buffer
    (insert "alpha beta")
    (goto-char 1)
    (let ((scholia-project-root-function (lambda () nil))
          (scholia-annotation-editor 'inline))
      (scholia-annotate "original")
      (let ((chain (scholia-core--select-chain)))
        (dolist (accept '(nil t))
          (scholia-edit-test--reading
              (lambda ()
                (delete-region (cera--session-begin cera--active)
                               (cera--session-end cera--active))
                (if accept (cera-accept) (cera-cancel)))
            (condition-case nil (scholia-edit-annotation) (quit nil)))
          (should (equal (overlay-get (car chain) 'scholia-annotation) "original"))))
      (goto-char 7)
      (scholia-edit-test--reading #'cera-cancel
        (condition-case nil (scholia-annotate) (quit nil)))
      (should (= (length (scholia-buffer-chains)) 1)))))

(ert-deftest scholia-edit-minibuffer-edit-prefills-existing-note ()
  "The original interface remains available for both adding and editing."
  (with-temp-buffer
    (insert "alpha")
    (goto-char 1)
    (let ((scholia-annotation-editor 'minibuffer)
          (scholia-project-root-function (lambda () nil)))
      (scholia-annotate "initial")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _table _pred _require-match initial &rest _)
                   (should (equal initial "initial"))
                   "replacement")))
        (scholia-edit-annotation))
      (should (equal (overlay-get (car (scholia-core--select-chain))
                                  'scholia-annotation)
                     "replacement")))))

(ert-deftest scholia-edit-corfu-switches-to-filenames-when-at-is-typed ()
  "Typing @ opens file completion while ordinary prose leaves history closed."
  (skip-unless (featurep 'corfu))
  (scholia-test-with-session-directory
    (let ((default-directory scholia-session-directory)
          (history '("See this" "See that")))
      (with-temp-file "notes.md" (insert "notes\n"))
      (save-window-excursion
        (with-temp-buffer
          (set-window-buffer (selected-window) (current-buffer))
          (insert "source\nnext")
          (goto-char 2)
          (cl-letf (((symbol-function 'corfu--popup-support-p) #'always)
                    ((symbol-function 'corfu--popup-show) #'ignore)
                    ((symbol-function 'corfu--popup-hide) #'ignore))
            (scholia-edit-test--reading
                (lambda ()
                  (sit-for 0.15)
                  (should-not completion-in-region-mode)
                  (let ((this-command 'self-insert-command)
                        (corfu-auto-delay 0))
                    (insert "@")
                    (run-hooks 'post-command-hook)
                    (should completion-in-region-mode)
                    (should (= (car completion-in-region--data) (point)))
                    (should (eq (completion-metadata-get
                                 (completion-metadata
                                  "" (nth 2 completion-in-region--data) nil)
                                 'category)
                                'file))
                    (insert "notes.md ")
                    (run-hooks 'post-command-hook)
                    (should-not completion-in-region-mode)
                    (should (eq (nth 2 (cera--capf)) history)))
                  (cera-accept))
              (should (equal (scholia-edit-read history "See ")
                             "See @notes.md ")))))))))

(ert-deftest scholia-edit-marks-the-field-with-a-glyph-the-frame-can-draw ()
  "The field is marked by the Nerd Font glyph, or by a quotation mark without it.
Both wear the session's colour.  A frame with neither the font nor the
fallback character is given nothing rather than a box."
  (let ((color "#F1D063"))
    (cl-letf (((symbol-function 'scholia-edit--nerd-font-p) (lambda () t))
              ((symbol-function 'nerd-icons-mdicon)
               (lambda (name &rest arguments)
                 (propertize "*" 'glyph name
                             'face (plist-get arguments :face)
                             'height (plist-get arguments :height)))))
      (let ((drawn (scholia-edit--icon color)))
        (should (equal (get-text-property 0 'glyph drawn) scholia-edit-icon))
        (should (equal (get-text-property 0 'face drawn)
                       (list :foreground color)))
        (should (equal (get-text-property 0 'height drawn)
                       scholia-edit-icon-height))))
    (cl-letf (((symbol-function 'scholia-edit--nerd-font-p) #'ignore)
              ((symbol-function 'char-displayable-p) (lambda (&rest _) t)))
      (let ((drawn (scholia-edit--icon color)))
        (should (equal (substring-no-properties drawn)
                       scholia-edit-icon-fallback))
        (should (equal (get-text-property 0 'face drawn)
                       (list :foreground color)))))
    (cl-letf (((symbol-function 'scholia-edit--nerd-font-p) #'ignore)
              ((symbol-function 'char-displayable-p) #'ignore))
      (should-not (scholia-edit--icon color)))
    (let ((scholia-edit-icon nil))
      (should-not (scholia-edit--icon color)))))

(provide 'scholia-edit-test)
