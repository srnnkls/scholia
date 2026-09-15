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
   (scholia-edit--session-begin scholia-edit--session)
   (scholia-edit--session-end scholia-edit--session)))

(defmacro scholia-edit-test--reading (interaction &rest body)
  "Run BODY with INTERACTION standing in for recursive keyboard input."
  (declare (indent 1) (debug (form body)))
  `(cl-letf (((symbol-function 'recursive-edit) ,interaction)
             ((symbol-function 'exit-recursive-edit) #'ignore))
     ,@body))

(defun scholia-edit-test--source-ranges ()
  "Return the sorted ranges visibly underlined by the active input reader."
  (sort (cl-loop for overlay in (overlays-in (point-min) (point-max))
                 when (eq (overlay-get overlay 'face) 'scholia-edit-source)
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
      (scholia-edit--draw))
    calls))

(ert-deftest scholia-edit-redraw-allocation-is-independent-of-field-length ()
  "A redraw allocates no more overlays for a long field than a short one.
Every line of the field is bracketed the same way but its last, so the
decoration is bounded whatever the note runs to, and a keystroke in a
long note costs what a keystroke in a short one does."
  (let ((allocations nil))
    (dolist (lines '(2 20))
      (with-temp-buffer
        (insert "alpha\nbravo\n")
        (goto-char 1)
        (let ((calls 0))
          (scholia-edit-test--reading
              (lambda ()
                (dotimes (_ lines) (insert "word word word\n"))
                (setq calls (scholia-edit-test--draw-allocations))
                (scholia-edit-cancel))
            (condition-case nil (scholia-edit-read nil "") (quit nil)))
          (push calls allocations))))
    (should (= (nth 0 allocations) (nth 1 allocations)))))

(ert-deftest scholia-edit-previews-exact-selection-from-its-first-line ()
  "Partial words and multiline selections are previewed before any input."
  (dolist (fixture '(("before words after\nnext\n" (8 . 13)
                      ((8 . 13)) "before words after\nnote\nnext\n" (1))
                     ("first words\nmiddle text\nlast words\nnext\n" (7 . 29)
                      ((7 . 12) (13 . 24) (25 . 29))
                      "first words\nmiddle text\nlast words\nnote\nnext\n" (1 13 25))
                     ("first words\nmiddle text\nlast words\nnext\n" (7 . 25)
                      ((7 . 12) (13 . 24))
                      "first words\nmiddle text\nnote\nlast words\nnext\n" (1 13))))
    (pcase-let ((`(,text ,bounds ,ranges ,expected ,lines) fixture))
      (dolist (reverse '(nil t))
        (with-temp-buffer
          (insert text)
          (let ((transient-mark-mode t))
            (goto-char (if reverse (car bounds) (cdr bounds)))
            (set-mark (if reverse (cdr bounds) (car bounds)))
            (setq mark-active t)
            (scholia-edit-test--reading
                (lambda ()
                  (should (equal (buffer-string) expected))
                  (should (equal (scholia-edit-test--source-ranges) ranges))
                  (dolist (line lines)
                    (should (equal (get-char-property line 'line-prefix)
                                   (if (= line 1) "╭ " "│ "))))
                  (should (equal (get-char-property (point) 'line-prefix) "╰ "))
                  (insert " extra")
                  (should (equal (scholia-edit-test--source-ranges) ranges))
                  (scholia-edit-accept))
              (should (equal (scholia-edit-read nil "note") "note extra")))
            (should (equal (buffer-string) text))
            (should-not (overlays-in (point-min) (point-max)))))))))

(ert-deftest scholia-edit-special-mode-field-supports-typing-and-evil-reentry ()
  "Status buffer maps do not suppress text entry, even after leaving insert."
  (dolist (evil '(nil t))
    (when evil (skip-unless (require 'evil nil t)))
    (dolist (cancel '(nil t))
      (save-window-excursion
        (with-temp-buffer
          (set-window-buffer (selected-window) (current-buffer))
          (insert "source\nnext")
          (special-mode)
          (use-local-map (copy-keymap (current-local-map)))
          (goto-char 2)
          (let ((source-map (current-local-map))
                (undo buffer-undo-list)
                (evil-insert-state-map
                 (and evil (copy-keymap evil-insert-state-map)))
                result)
            (when evil
              (setq-local emulation-mode-map-alists
                          (cons 'evil-mode-map-alist emulation-mode-map-alists))
              (keymap-set evil-insert-state-map "C-h" #'delete-backward-char)
              (evil-define-key* 'normal source-map
                                [remap evil-insert] #'ignore [remap evil-append] #'ignore)
              (evil-local-mode 1)
              (evil-normal-state)
              (should (eq (key-binding "i") #'ignore)))
            (local-set-key [f5]
                           (lambda () (interactive)
                             (condition-case nil
                                 (setq result (scholia-edit-read nil))
                               (quit (setq result 'cancelled)))))
            (unwind-protect
                (progn
                  (execute-kbd-macro
                   (vconcat (if evil [f5 ?a ?b ?\C-h ?c escape ?a ?d escape ?i ?x]
                              [f5 ?a ?c ?d])
                            (if cancel [?\C-g] [return])))
                  (should (equal result (cond (cancel 'cancelled) (evil "acxd") (t "acd"))))
                  (should (eq (current-local-map) source-map))
                  (should (eq major-mode 'special-mode))
                  (should buffer-read-only)
                  (should (eq undo buffer-undo-list))
                  (should (equal (buffer-string) "source\nnext"))
                  (when evil
                    (should (eq evil-state 'normal))
                    (should (eq (key-binding "i") #'ignore))))
              (when evil (evil-local-mode -1)))))))))

(ert-deftest scholia-edit-save-restores-source-and-borrowed-state ()
  "A real input region rolls back without touching source, hooks, or undo."
  (dolist (readonly '(nil t))
    (dolist (undo-enabled '(nil t))
      (with-temp-buffer
        (insert (propertize "alpha\nnext\n" 'face 'bold))
        (when undo-enabled
          (buffer-enable-undo)
          (insert "last"))
        (goto-char 2)
        (set-mark 4)
        (setq mark-active t)
        (let* ((original (buffer-string))
               (undo buffer-undo-list)
               (modified (buffer-modified-p))
               (chain (scholia-create-chain 1 (point-max) "existing" 2 "owner"))
               (ranges (mapcar (lambda (ov) (cons (overlay-start ov) (overlay-end ov)))
                               chain))
               (changes 0)
               (before (lambda (&rest _) (cl-incf changes)))
               (after (lambda (&rest _) (cl-incf changes))))
          (setq-local buffer-read-only readonly
                      before-change-functions (list before)
                      after-change-functions (list after))
          (let ((locals (scholia-edit--remember-locals)))
            (scholia-edit-test--reading
                (lambda ()
                  (should (equal (scholia-edit-test--input) "initial"))
                  (insert " text\nsecond line")
                  (should (equal (scholia-edit-test--input) "initial text\nsecond line"))
                  (should (= (line-number-at-pos
                              (save-excursion (search-forward "next") (point)))
                             4))
                  (scholia-edit-accept))
              (should (equal (scholia-edit-read '("initial candidate") "initial")
                             "initial text\nsecond line")))
            (should (equal locals (scholia-edit--remember-locals))))
          (should (equal-including-properties original (buffer-string)))
          (should (eq undo buffer-undo-list))
          (should (eq modified (buffer-modified-p)))
          (should (= (point) 2))
          (should (= (mark) 4))
          (should mark-active)
          (should (= changes 0))
          (should (equal ranges
                         (mapcar (lambda (ov) (cons (overlay-start ov) (overlay-end ov)))
                                 chain)))
          (should (equal chain (car (scholia-buffer-chains))))
          (should-not (seq-some (lambda (ov) (overlay-get ov 'scholia-edit))
                                (overlays-in (point-min) (point-max)))))))))

(ert-deftest scholia-edit-cancel-and-error-leave-no-temporary-state ()
  "Both cancellation paths and unexpected errors unwind the full reader."
  (dolist (finish '(scholia-edit-cancel keyboard-quit error))
    (with-temp-buffer
      (insert "last line without newline")
      (set-buffer-modified-p nil)
      (goto-char (point-max))
      (let ((source (buffer-string))
            (locals (scholia-edit--remember-locals))
            outcome)
        (scholia-edit-test--reading
            (lambda ()
              (insert "discard this")
              (if (eq finish 'error)
                  (error "Reader failure")
                (funcall finish)))
          (condition-case nil
              (scholia-edit-read nil)
            (quit (setq outcome 'quit))
            (error (setq outcome 'error))))
        (should (eq outcome (if (eq finish 'error) 'error 'quit)))
        (should (equal source (buffer-string)))
        (should (equal locals (scholia-edit--remember-locals)))
        (should-not (buffer-modified-p))
        (should-not (overlays-in (point-min) (point-max)))))))

(ert-deftest scholia-edit-rejects-document-edits-and-saving ()
  "The field is the only writable range, including at both empty boundaries."
  (with-temp-buffer
    (insert "alpha\nnext\n")
    (goto-char 2)
    (scholia-edit-test--reading
        (lambda ()
          (let ((begin (scholia-edit--session-begin scholia-edit--session))
                (end (scholia-edit--session-end scholia-edit--session)))
            (should (= begin end))
            (insert "abc")
            (should (equal (scholia-edit-test--input) "abc"))
            (dolist (range (list (cons (1- begin) begin)
                                 (cons end (1+ end))
                                 (cons (point-min) (point-max))))
              (should-error (delete-region (car range) (cdr range)) :type 'user-error)
              (scholia-edit--pre-command))
            (goto-char begin)
            (insert "prefix ")
            (goto-char end)
            (insert " suffix")
            (should (equal (scholia-edit-test--input) "prefix abc suffix"))
            (should-error (run-hooks 'before-save-hook) :type 'user-error)
            (should-error (run-hook-with-args-until-success 'write-contents-functions)
                          :type 'user-error)
            (should-error (run-hook-with-args-until-failure 'kill-buffer-query-functions)
                          :type 'user-error)
            (should-error (fundamental-mode) :type 'user-error)
            (should-error (scholia-save-annotations) :type 'user-error)
            (scholia-edit-accept)))
      (should (equal (scholia-edit-read nil) "prefix abc suffix")))
    (should (equal (buffer-string) "alpha\nnext\n"))))

(ert-deftest scholia-edit-preserves-narrowing-and-indirect-source ()
  "A narrowed indirect source gets its original text and restriction back."
  (with-temp-buffer
    (insert "outside\nalpha\nnext\noutside")
    (let* ((base (current-buffer))
           (source (buffer-string))
           (indirect (clone-indirect-buffer " *scholia-inline-test*" nil)))
      (unwind-protect
          (with-current-buffer indirect
            (narrow-to-region 9 20)
            (goto-char 10)
            (let ((undo buffer-undo-list))
              (scholia-edit-test--reading
                  (lambda () (insert "note") (scholia-edit-accept))
                (should (equal (scholia-edit-read nil) "note")))
              (should (eq undo buffer-undo-list)))
            (should (= (point-min) 9))
            (should (= (point-max) 20))
            (should (= (point) 10))
            (should (equal (with-current-buffer base (buffer-string)) source)))
        (kill-buffer indirect)))))

(ert-deftest scholia-edit-capf-honors-table-protocol-and-input-bounds ()
  "Completion keeps its category and affixes and never includes the chrome."
  (with-temp-buffer
    (insert "source\nnext")
    (goto-char 2)
    (let* ((annotation (lambda (_) "  ask for clarification"))
           (table (lambda (string predicate action)
                    (if (eq action 'metadata)
                        `(metadata (category . scholia-annotation)
                                   (annotation-function . ,annotation))
                      (complete-with-action action '("Question" "Suggestion")
                                            string predicate)))))
      (scholia-edit-test--reading
          (lambda ()
            (pcase-let ((`(,begin ,end ,collection . ,properties) (scholia-edit--capf)))
              (should (equal (buffer-substring-no-properties begin end) "Que"))
              (should (eq (plist-get properties :exclusive) 'no))
              (should (equal (all-completions "Que" collection) '("Question")))
              (should (eq (completion-metadata-get
                           (completion-metadata "" collection nil) 'annotation-function)
                          annotation))
              (goto-char (1- begin))
              (should-not (scholia-edit--capf))
              (goto-char end))
            (scholia-edit-accept))
        (should (equal (scholia-edit-read table "Que") "Que"))))))

(ert-deftest scholia-edit-input-undo-cannot-consume-document-history ()
  "Undoing input and a rejected undo at the field boundary still roll back."
  (with-temp-buffer
    (buffer-enable-undo)
    (insert "source\nnext")
    (undo-boundary)
    (let ((undo buffer-undo-list)
          (source (buffer-string)))
      (goto-char 2)
      (scholia-edit-test--reading
          (lambda ()
            (insert "draft")
            (undo-boundary)
            (let ((last-command nil)) (undo-only 1))
            (should (equal (scholia-edit-test--input) ""))
            (let ((last-command 'undo))
              (should-error (undo-only 1) :type 'user-error))
            (scholia-edit--pre-command)
            (insert "saved")
            (scholia-edit-accept))
        (should (equal (scholia-edit-read nil) "saved")))
      (should (eq undo buffer-undo-list))
      (should (equal source (buffer-string))))))

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
              (goto-char (+ (scholia-edit--session-begin scholia-edit--session)
                            (length "See @src/eng")))
              (let* ((capf (scholia-edit--capf))
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
                (should (equal (seq-take (scholia-edit--capf) 2) (seq-take capf 2)))
                (forward-char 2))
              (completion-at-point)
              (should (equal (scholia-edit-test--input) "See @src/engine.el for context"))
              (scholia-edit-accept))
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
              (let ((capf (scholia-edit--capf)))
                (should (= (nth 0 capf) (nth 1 capf)))
                (should (member "notes.md" (all-completions "" (nth 2 capf))))
                (should (member "docs/" (all-completions "" (nth 2 capf)))))
              (insert "docs/gu")
              (completion-at-point)
              (should (equal (scholia-edit-test--input) "Read @docs/guide.md"))
              (insert " or contact name@example.com")
              (should (eq (nth 2 (scholia-edit--capf)) history))
              (scholia-edit-accept))
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
                          (scholia-edit-accept))
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
                (delete-region (scholia-edit--session-begin scholia-edit--session)
                               (scholia-edit--session-end scholia-edit--session))
                (insert "edited note")
                (scholia-edit-accept))
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
                (delete-region (scholia-edit--session-begin scholia-edit--session)
                               (scholia-edit--session-end scholia-edit--session))
                (if accept (scholia-edit-accept) (scholia-edit-cancel)))
            (condition-case nil (scholia-edit-annotation) (quit nil)))
          (should (equal (overlay-get (car chain) 'scholia-annotation) "original"))))
      (goto-char 7)
      (scholia-edit-test--reading #'scholia-edit-cancel
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

(ert-deftest scholia-edit-corfu-keeps-configured-keys-and-return-inserts ()
  "Corfu owns completion keys, while the editor keeps direct save and cancel."
  (skip-unless (featurep 'corfu))
  (dolist (apply-candidate '(nil t))
    (save-window-excursion
      (with-temp-buffer
        (set-window-buffer (selected-window) (current-buffer))
        (insert "source\nnext")
        (goto-char 2)
        (let ((corfu-map (copy-keymap corfu-map))
              selected)
          (keymap-set corfu-map "TAB" #'corfu-next)
          (keymap-set corfu-map "C-j" #'corfu-next)
          (keymap-set corfu-map "C-k" #'corfu-previous)
          (keymap-set corfu-map "RET" #'corfu-insert)
          (let ((original-map (copy-keymap corfu-map)))
            ;; Keep Corfu's filtering, selection, insertion, and change groups.
            ;; Only its graphical drawing is replaced in this batch test.
            (cl-letf (((symbol-function 'corfu--popup-show) #'ignore)
                      ((symbol-function 'corfu--popup-hide) #'ignore))
              (scholia-edit-test--reading
                  (lambda ()
                    (should (eq (key-binding (kbd "RET")) #'scholia-edit-accept))
                    (should (eq (key-binding (kbd "C-g")) #'scholia-edit-cancel))
                    (let ((capf (scholia-edit--capf))
                          (completion-in-region-mode-predicate #'always))
                      (apply #'corfu--setup (append (seq-take capf 3) '(nil))))
                    (corfu--exhibit)
                    (should (equal (scholia-edit-test--input) "What"))
                    (should (eq (key-binding (kbd "TAB")) #'corfu-next))
                    (should (eq (key-binding (kbd "C-j")) #'corfu-next))
                    (should (eq (key-binding (kbd "C-k")) #'corfu-previous))
                    (should (eq (key-binding (kbd "RET")) #'corfu-insert))
                    (should (eq (key-binding (kbd "C-g")) #'scholia-edit-cancel))
                    (call-interactively (key-binding (kbd "TAB")))
                    (setq selected (nth corfu--index corfu--candidates))
                    (when apply-candidate
                      (call-interactively (key-binding (kbd "RET")))
                      (should-not completion-in-region-mode)
                      (should (eq (key-binding (kbd "RET")) #'scholia-edit-accept)))
                    (call-interactively (key-binding (kbd "C-c C-c"))))
                (should (equal (scholia-edit-read '("What next?" "What now?") "What")
                               (if apply-candidate selected "What")))))
            (should-not (bound-and-true-p corfu-mode))
            (should-not completion-in-region-mode)
            (should (equal original-map corfu-map))
            (should (equal (buffer-string) "source\nnext"))))))))

(ert-deftest scholia-edit-minimal-field-reserves-only-visible-popup-lines ()
  "Only the input occupies buffer lines; Corfu's visible rows add display space."
  (skip-unless (featurep 'corfu))
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (insert "source\nnext")
      (goto-char 2)
      (let ((corfu-count 4)
            (corfu-min-width 17)
            (corfu-max-width 43))
        (cl-letf (((symbol-function 'corfu--popup-show) #'ignore)
                  ((symbol-function 'corfu--popup-hide) #'ignore))
          (let ((show (symbol-function 'corfu--popup-show))
                (hide (symbol-function 'corfu--popup-hide)))
            (scholia-edit-test--reading
                (lambda ()
                  (should (equal (buffer-string) "source\nWhat\nnext"))
                  (should-not
                   (overlay-get (scholia-edit--session-spacer scholia-edit--session)
                                'after-string))
                  (should (= corfu-count 4))
                  (should (= corfu-min-width 17))
                  (should (= corfu-max-width 43))
                  (let ((capf (scholia-edit--capf))
                        (completion-in-region-mode-predicate #'always))
                    (apply #'corfu--setup (append (seq-take capf 3) '(nil))))
                  (corfu--exhibit)
                  (should (equal (overlay-get
                                  (scholia-edit--session-spacer scholia-edit--session)
                                  'after-string)
                                 "\n\n"))
                  (should (equal (buffer-string) "source\nWhat\nnext"))
                  (corfu-quit)
                  (should-not
                   (overlay-get (scholia-edit--session-spacer scholia-edit--session)
                                'after-string))
                  (scholia-edit-accept))
              (should (equal (scholia-edit-read '("What next?" "What now?") "What")
                             "What")))
            (should (eq show (symbol-function 'corfu--popup-show)))
            (should (eq hide (symbol-function 'corfu--popup-hide)))))))))

(ert-deftest scholia-edit-preserves-editing-maps-hooks-and-escape-prefix ()
  "Local editing keys and command hooks continue to run during input."
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (insert "source\nnext")
      (goto-char 2)
      (let ((before 0) (after 0) result)
        (local-set-key (kbd "C-h") #'delete-backward-char)
        (local-set-key (kbd "M-!") #'backward-char)
        (add-hook 'pre-command-hook (lambda () (cl-incf before)) nil t)
        (add-hook 'post-command-hook (lambda () (cl-incf after)) nil t)
        (local-set-key [f5]
                       (lambda () (interactive)
                         (setq result (scholia-edit-read nil))))
        (execute-kbd-macro [f5 ?a ?b ?\C-h ?c ?\M-! ?x return])
        (should (equal result "axc"))
        (should (> before 5))
        (should (> after 5))
        (should (equal (buffer-string) "source\nnext"))))))

(ert-deftest scholia-edit-evil-inherits-insert-keys-and-restores-state ()
  "Evil insert bindings work, ESC changes state, and C-g cancels the reader."
  (skip-unless (require 'evil nil t))
  (dolist (cancel '(nil t))
    (save-window-excursion
      (with-temp-buffer
        (set-window-buffer (selected-window) (current-buffer))
        (insert "source\nnext")
        (buffer-enable-undo)
        (goto-char 2)
        (let ((evil-insert-state-map (copy-keymap evil-insert-state-map))
              (undo buffer-undo-list)
              result)
          (setq-local emulation-mode-map-alists
                      (cons 'evil-mode-map-alist emulation-mode-map-alists))
          (keymap-set evil-insert-state-map "C-h" #'delete-backward-char)
          (evil-local-mode 1)
          (evil-normal-state)
          (local-set-key [f5]
                         (lambda () (interactive)
                           (condition-case nil
                               (setq result (scholia-edit-read nil))
                             (quit (setq result 'cancelled)))))
          (unwind-protect
              (progn
                (execute-kbd-macro
                 (vconcat [f5 ?a ?b ?\C-h ?c escape ?a ?d]
                          (if cancel [?\C-g] [return])))
                (should (equal result (if cancel 'cancelled "acd")))
                (should (eq evil-state 'normal))
                (should (eq undo buffer-undo-list))
                (should (equal (buffer-string) "source\nnext")))
            (evil-local-mode -1)))))))

(ert-deftest scholia-edit-restores-preexisting-corfu-configuration ()
  "An existing Corfu setup remains enabled with its original local options."
  (skip-unless (featurep 'corfu))
  (with-temp-buffer
    (insert "source")
    (let ((corfu-auto nil)) (corfu-mode 1))
    (setq-local corfu-auto-prefix 4
                corfu-auto-trigger "."
                corfu-map (copy-keymap corfu-map))
    (keymap-set corfu-map "RET" #'ignore)
    (let ((locals (scholia-edit--remember-locals)))
      (scholia-edit-test--reading #'scholia-edit-accept
        (should (equal (scholia-edit-read nil "note") "note")))
      (should (equal locals (scholia-edit--remember-locals)))
      (should (bound-and-true-p corfu-mode))
      (should (eq (keymap-lookup corfu-map "RET") #'ignore)))))

(ert-deftest scholia-edit-evil-corfu-keyboard-inserts-then-saves ()
  "RET completes and then saves in a real Evil and Corfu command loop."
  (skip-unless (and (featurep 'corfu) (require 'evil nil t)))
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (insert "source\nnext")
      (goto-char 2)
      (setq-local emulation-mode-map-alists
                  (cons 'evil-mode-map-alist emulation-mode-map-alists))
      (let ((evil-insert-state-map (copy-keymap evil-insert-state-map))
            (corfu-map (copy-keymap corfu-map))
            (corfu-auto-delay 0)
            result)
        (keymap-set evil-insert-state-map "C-h" #'delete-backward-char)
        (keymap-set corfu-map "TAB" #'corfu-next)
        (keymap-set corfu-map "RET" #'corfu-insert)
        (evil-local-mode 1)
        (evil-normal-state)
        (local-set-key [f5]
                       (lambda () (interactive)
                         (setq result
                               (scholia-edit-read '("What next?" "What now?") "What"))))
        (local-set-key [f6]
                       (lambda () (interactive)
                         (should completion-in-region-mode)
                         (should (eq (key-binding (kbd "C-h") nil t)
                                     #'delete-backward-char))
                         (should (eq (key-binding (kbd "C-g")) #'scholia-edit-cancel))
                         (should (eq (key-binding (kbd "TAB")) #'corfu-next))
                         (should (eq (key-binding (kbd "RET")) #'corfu-insert))))
        (unwind-protect
            (cl-letf (((symbol-function 'corfu--popup-support-p) #'always)
                      ((symbol-function 'corfu--popup-show) #'ignore)
                      ((symbol-function 'corfu--popup-hide) #'ignore))
              (execute-kbd-macro
               (vconcat [f5 ?x ?\C-h] (kbd "C-SPC") [f6 tab return return]))
              (should (member result '("What next?" "What now?")))
              (should-not completion-in-region-mode)
              (should (eq evil-state 'normal))
              (should (equal (buffer-string) "source\nnext")))
          (evil-local-mode -1))))))

(ert-deftest scholia-edit-corfu-history-waits-for-manual-completion ()
  "Opening and typing keep history closed until C-SPC requests completion."
  (skip-unless (featurep 'corfu))
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (insert "source\nnext")
      (goto-char 2)
      (let ((source-commands 0))
        (add-hook 'post-command-hook (lambda () (cl-incf source-commands)) nil t)
        (cl-letf (((symbol-function 'corfu--popup-support-p) #'always)
                  ((symbol-function 'corfu--popup-show) #'ignore)
                  ((symbol-function 'corfu--popup-hide) #'ignore))
          (scholia-edit-test--reading
              (lambda ()
                (let ((this-command 'scholia-annotate))
                  (run-hooks 'post-command-hook))
                (sit-for 0.15)
                (should-not completion-in-region-mode)
                (should (= source-commands 1))
                (should (equal (scholia-edit-test--input) "What"))
                (let ((this-command 'self-insert-command)
                      (corfu-auto-delay 0))
                  (insert " n")
                  (run-hooks 'post-command-hook))
                (should-not completion-in-region-mode)
                (should-not (overlay-get
                             (scholia-edit--session-spacer scholia-edit--session)
                             'after-string))
                (should-not (eq (key-binding (kbd "TAB")) #'completion-at-point))
                (let ((this-command 'completion-at-point))
                  (call-interactively (key-binding (kbd "C-SPC")))
                  (run-hooks 'post-command-hook))
                (should completion-in-region-mode)
                (should (equal (sort (copy-sequence corfu--candidates) #'string<)
                               '("What next?" "What now?")))
                (scholia-edit-accept))
            (should (equal (scholia-edit-read '("What next?" "What now?") "What")
                           "What n"))))))))

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
                    (should (eq (nth 2 (scholia-edit--capf)) history)))
                  (scholia-edit-accept))
              (should (equal (scholia-edit-read history "See ")
                             "See @notes.md ")))))))))

(ert-deftest scholia-edit-keyboard-accepts-multiline-input ()
  "The actual recursive command loop supports typing, newlines, and RET."
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (insert "source\nnext\n")
      (goto-char 2)
      (let (result)
        (local-set-key [f5]
                       (lambda () (interactive)
                         (setq result (scholia-edit-read nil))))
        (execute-kbd-macro [f5 ?f ?i ?r ?s ?t ?\C-j ?l ?i ?n ?e return])
        (should (equal result "first\nline"))
        (should (equal (buffer-string) "source\nnext\n"))
        (should (= (point) 2))))))

(provide 'scholia-edit-test)
;;; scholia-edit-test.el ends here
