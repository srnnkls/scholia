;;; scholia-vars-test.el --- Tests for the scholia-vars leaf  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Covers `scholia-vars', the leaf every other module requires.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'cus-face)
(require 'project)
(require 'seq)
(require 'scholia-test-helper)

(let ((load-prefer-newer t))
  (require 'scholia-vars nil t))

(defun scholia-vars-test--emacs ()
  "Return the absolute path of the running Emacs executable."
  (expand-file-name invocation-name invocation-directory))

(defun scholia-vars-test--run (form &rest args)
  "Evaluate FORM in a vanilla batch Emacs and return (STATUS STDOUT STDERR).
ARGS are extra command line arguments inserted before the evaluation.
Stderr is captured separately so a diagnostic line can never be read as
part of the value FORM prints."
  (let ((buffer (generate-new-buffer " *scholia-vars-probe*"))
        (errfile (make-temp-file "scholia-vars-probe-stderr-")))
    (unwind-protect
        (let ((status (apply #'call-process
                             (scholia-vars-test--emacs) nil (list buffer errfile) nil
                             (append '("-Q" "--batch"
                                       "--eval" "(setq load-prefer-newer t)")
                                     args
                                     (list "--eval" (prin1-to-string form))))))
          (list status
                (with-current-buffer buffer (buffer-string))
                (with-temp-buffer
                  (insert-file-contents errfile)
                  (buffer-string))))
      (kill-buffer buffer)
      (delete-file errfile))))

(defun scholia-vars-test--probe (form &rest args)
  "Return the sexp FORM prints when evaluated in a vanilla batch Emacs.
ARGS are extra command line arguments inserted before the evaluation.
A non-zero exit or unreadable output fails the calling test with both
streams in the report."
  (pcase-let* ((`(,status ,stdout ,stderr) (apply #'scholia-vars-test--run form args))
               (sexp (car (ignore-errors (read-from-string stdout)))))
    (should (equal (list status stdout stderr) (list 0 stdout stderr)))
    (should (equal (list (and sexp t) stdout stderr) (list t stdout stderr)))
    sexp))

(defun scholia-vars-test--face-attribute-plist-p (value)
  "Return non-nil when VALUE is a non-empty plist keyed by face attributes."
  (and (consp value)
       (proper-list-p value)
       (zerop (mod (length value) 2))
       (cl-loop for (key _attribute) on value by #'cddr
                always (and (keywordp key)
                            (assq key custom-face-attributes)))))


;;;; Structure: the leaf stands alone, the entry point holds nothing

(defconst scholia-vars-test--keymap-probe
  '(progn
     (require 'scholia-vars)
     (let* ((entry (assq 'scholia-mode minor-mode-map-alist))
            (registered (cdr entry)))
       (prin1 (list :entry (and entry t)
                    :registered-keymap (and (keymapp registered) t)
                    :registered-is-mode-map (and (boundp 'scholia-mode-map)
                                                 (eq registered scholia-mode-map)
                                                 t)
                    :bindings (and (keymapp registered)
                                   (mapcar (lambda (key)
                                             (cons key (keymap-lookup registered key)))
                                           '("C-c C-a" "C-c C-d" "C-c C-r" "C-c C-s")))))))
  "Form reporting the keymap `scholia-mode' registers.
Reported for a probe loading the leaf on its own.")

(ert-deftest scholia-vars-mode-registers-the-keymap-when-the-leaf-loads-alone ()
  (should (equal (scholia-vars-test--probe
                  scholia-vars-test--keymap-probe
                  "-L" (expand-file-name scholia-test-project-root))
                 '(:entry t
                   :registered-keymap t
                   :registered-is-mode-map t
                   :bindings (("C-c C-a" . scholia-annotate)
                              ("C-c C-d" . scholia-delete-annotation)
                              ("C-c C-r" . scholia-reply-to)
                              ("C-c C-s" . scholia-status))))))

(ert-deftest scholia-vars-byte-compiles-standalone-with-warnings-as-errors ()
  (let ((source (scholia-test-project-file "scholia-vars.el")))
    (should (file-exists-p source))
    (let* ((dir (file-name-as-directory (make-temp-file "scholia-vars-compile-" t)))
           (copy (expand-file-name "scholia-vars.el" dir)))
      (unwind-protect
          (progn
            (copy-file source copy t)
            (pcase-let ((`(,status ,stdout ,stderr)
                         (scholia-vars-test--run
                          `(progn
                             (setq byte-compile-error-on-warn t)
                             (unless (byte-compile-file ,copy)
                               (kill-emacs 1))))))
              (should (equal (list status stdout stderr) (list 0 stdout stderr)))
              (should (file-exists-p (expand-file-name "scholia-vars.elc" dir)))))
        (delete-directory dir t)))))

(defconst scholia-vars-test--entry-point-probe
  '(progn
     (require 'scholia-vars)
     (let ((module (lambda (file)
                     (file-name-nondirectory
                      (file-name-sans-extension (if (stringp file) file "")))))
           (bound-scholia-symbols
            (lambda ()
              (let ((collected nil))
                (mapatoms (lambda (symbol)
                            (when (and (boundp symbol)
                                       (string-prefix-p "scholia" (symbol-name symbol)))
                              (push symbol collected))))
                collected)))
           (before nil) (added nil) (required nil) (loaded nil))
       (setq before (funcall bound-scholia-symbols))
       (require 'scholia)
       (dolist (symbol (funcall bound-scholia-symbols))
         (unless (memq symbol before)
           (push (symbol-name symbol) added)))
       (dolist (entry load-history)
         (when (equal "scholia" (funcall module (car entry)))
           (dolist (cell (cdr entry))
             (when (eq (car-safe cell) 'require)
               (push (symbol-name (cdr cell)) required)))))
       (dolist (feature features)
         (when (string-prefix-p "scholia" (symbol-name feature))
           (push (symbol-name feature) loaded)))
       (prin1
        (list :features (sort loaded #'string<)
              :entry-point-requires (sort required #'string<)
              :bound-by-the-entry-point (sort added #'string<)
              :defined-in
              (mapcar (lambda (symbol)
                        (cons symbol
                              (funcall module
                                       (if (fboundp symbol)
                                           (symbol-file symbol 'defun)
                                         (find-lisp-object-file-name symbol 'defvar)))))
                      '(scholia-mode scholia-mode-map
                        scholia-export-format scholia-session))))))
  "Form reporting what loading the entry point pulls in and where it comes from.
Each origin is reported as a base name without its extension, so a source
load and a bytecode load are indistinguishable to the assertion.")

(defconst scholia-vars-test--autoloaded-commands
  '(scholia-mode scholia-session-switch scholia-export scholia-search
                 scholia-export-session scholia-org-remark-export)
  "One command per feature module the entry point must publish.
Command `scholia-mode' requires `scholia-core' from its own body, so the
annotation commands arrive with it; a module nothing requires reaches the
user only through a cookie of its own.")

(defun scholia-vars-test--autoload-probe (generated)
  "Return a form reporting which commands GENERATED can autoload.
Every root Elisp file but `scholia.el' is kept out of the scan, so only its
cookies can make a command autoloadable."
  (let ((root (expand-file-name scholia-test-project-root)))
    `(progn
       (require 'loaddefs-gen)
       (loaddefs-generate
        ,root ,generated
        (seq-remove (lambda (file)
                      (file-equal-p file
                                    (expand-file-name "scholia.el" ,root)))
                    (directory-files ,root t "\\.el\\'")))
       (load ,generated t t)
       (prin1 (list :autoloaded
                    (seq-remove
                     (lambda (command)
                       (and (fboundp command)
                            (autoloadp (symbol-function command))
                            (commandp command)))
                     ',scholia-vars-test--autoloaded-commands)
                    :without-loading-scholia (not (featurep 'scholia)))))))

(ert-deftest scholia-vars-entry-point-requires-the-leaf-and-holds-no-state ()
  (should (equal (scholia-vars-test--probe
                  scholia-vars-test--entry-point-probe
                  "-L" (expand-file-name scholia-test-project-root))
                 '(:features ("scholia" "scholia-vars")
                   :entry-point-requires ("scholia-vars")
                   :bound-by-the-entry-point ()
                   :defined-in ((scholia-mode . "scholia-vars")
                                (scholia-mode-map . "scholia-vars")
                                (scholia-export-format . "scholia-vars")
                                (scholia-session . "scholia-vars")))))
  (let ((dir (file-name-as-directory (make-temp-file "scholia-autoloads-" t))))
    (unwind-protect
        (should (equal (scholia-vars-test--probe
                        (scholia-vars-test--autoload-probe
                         (expand-file-name "scholia-autoloads.el" dir)))
                       '(:autoloaded nil :without-loading-scholia t)))
      (delete-directory dir t))))


;;;; The customization surface

(ert-deftest scholia-vars-defines-the-customization-surface ()
  (should (get 'scholia 'custom-group))
  (dolist (symbol '(scholia-session-directory
                    scholia-session
                    scholia-session-state-file
                    scholia-project-sessions
                    scholia-project-root-function
                    scholia-export-format
                    scholia-herdr-default-target
                    scholia-herdr-send-format
                    scholia-annotation-history-limit
                    scholia-highlight-faces
                    scholia-annotation-text-faces
                    scholia-use-messages
                    scholia-annotation-column
                    scholia-search-region-lines-delta))
    (should (custom-variable-p symbol)))
  (let ((directory (default-value 'scholia-session-directory)))
    (should (stringp directory))
    (should (string-suffix-p "scholia/sessions"
                             (directory-file-name directory))))
  (should-not (default-value 'scholia-session))
  (should-not (default-value 'scholia-project-sessions))
  (should (functionp (default-value 'scholia-project-root-function)))
  (should (eq (default-value 'scholia-export-format) 'rustc))
  (should-not (default-value 'scholia-herdr-default-target))
  (should-not (default-value 'scholia-herdr-send-format))
  (should (equal (default-value 'scholia-annotation-history-limit) 200))
  (should (eq (default-value 'scholia-use-messages) t))
  (should (equal (default-value 'scholia-annotation-column) 85))
  (should (equal (default-value 'scholia-search-region-lines-delta) 2))
  (should (facep 'scholia-prefix))
  (dolist (symbol '(scholia-highlight-faces scholia-annotation-text-faces))
    (let ((value (default-value symbol)))
      (should (consp value))
      (should (equal (cons symbol
                           (seq-remove #'scholia-vars-test--face-attribute-plist-p
                                       value))
                     (list symbol))))))

(ert-deftest scholia-vars-project-root-prefers-projectile-and-normalizes ()
  (let ((alpha (file-name-as-directory (expand-file-name "~/alpha")))
        (beta (file-name-as-directory (expand-file-name "~/beta")))
        (projectile (lambda (&optional _dir) "~/alpha"))
        (project (lambda (&rest _) (cons 'transient "~/beta"))))
    (cl-letf (((symbol-function 'projectile-project-root) projectile)
              ((symbol-function 'project-current) nil))
      (should (equal (scholia-project-root) alpha)))
    (cl-letf (((symbol-function 'projectile-project-root) projectile)
              ((symbol-function 'project-current) project))
      (should (equal (scholia-project-root) alpha)))
    (cl-letf (((symbol-function 'projectile-project-root) nil)
              ((symbol-function 'project-current) project))
      (should (equal (scholia-project-root) beta)))
    (cl-letf (((symbol-function 'projectile-project-root)
               (lambda (&optional _dir) nil))
              ((symbol-function 'project-current) (lambda (&rest _) nil)))
      (should-not (scholia-project-root)))))

(ert-deftest scholia-vars-session-is-buffer-local-when-set-in-a-buffer ()
  (let ((global (default-value 'scholia-session)))
    (with-temp-buffer
      (setq scholia-session "alpha")
      (should (local-variable-p 'scholia-session))
      (should (equal scholia-session "alpha"))
      (should (equal (default-value 'scholia-session) global)))
    (with-temp-buffer
      (should (equal scholia-session global)))))


;;;; Errors

(ert-deftest scholia-vars-db-format-error-is-a-catchable-error ()
  (should (stringp (get 'scholia-db-format-error 'error-message)))
  (should (< 0 (length (get 'scholia-db-format-error 'error-message))))
  (should (eq 'specific
              (condition-case nil
                  (signal 'scholia-db-format-error (list "no version tag"))
                (scholia-db-format-error 'specific)
                (error 'generic))))
  (should (eq 'generic
              (condition-case nil
                  (signal 'scholia-db-format-error nil)
                (error 'generic)))))


;;;; The annotation predicate and its macro

(ert-deftest scholia-vars-annotation-p-accepts-only-overlays-carrying-an-annotation ()
  (should (fboundp 'scholia-annotation-p))
  (with-temp-buffer
    (insert "0123456789\n")
    (should-not (scholia-annotation-p nil))
    (should-not (scholia-annotation-p "not an overlay"))
    (should-not (scholia-annotation-p (make-overlay 2 5)))
    (let ((decoy (make-overlay 5 8)))
      (overlay-put decoy 'face 'highlight)
      (overlay-put decoy 'help-echo "a comment, not an annotation")
      (should-not (scholia-annotation-p decoy)))
    (let ((annotation (make-overlay 2 5)))
      (overlay-put annotation 'scholia-annotation "the annotation text")
      (should (scholia-annotation-p annotation))
      (overlay-put annotation 'scholia-annotation nil)
      (should-not (scholia-annotation-p annotation)))
    (let ((mistyped (make-overlay 2 5)))
      (overlay-put mistyped 'scholia-annotation t)
      (should-not (scholia-annotation-p mistyped)))
    (let ((deleted (make-overlay 2 5)))
      (overlay-put deleted 'scholia-annotation "deleted annotation")
      (delete-overlay deleted)
      (should-not (scholia-annotation-p deleted))))
  (let ((orphan (with-temp-buffer
                  (insert "0123456789\n")
                  (let ((overlay (make-overlay 2 5)))
                    (overlay-put overlay 'scholia-annotation "orphaned")
                    overlay))))
    (should-not (scholia-annotation-p orphan))))

(ert-deftest scholia-vars-ensure-annotation-is-a-macro-gating-on-the-predicate ()
  (should (macrop 'scholia-ensure-annotation))
  (with-temp-buffer
    (insert "0123456789\n")
    (let* ((overlay (make-overlay 2 5))
           (evaluations 0)
           (probe (lambda () (setq evaluations (1+ evaluations)) overlay))
           (ran nil))
      (cl-letf (((symbol-function 'scholia-annotation-p)
                 (lambda (candidate) (should (eq candidate overlay)) nil)))
        (scholia-ensure-annotation ((funcall probe))
          (setq ran t))
        (should-not ran))
      (cl-letf (((symbol-function 'scholia-annotation-p)
                 (lambda (candidate) (should (eq candidate overlay)) t)))
        (scholia-ensure-annotation ((funcall probe))
          (setq ran t))
        (should ran))
      (should (equal evaluations 2)))))

(provide 'scholia-vars-test)
;;; scholia-vars-test.el ends here
