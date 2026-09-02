;;; scholia-smoke-test.el --- Vanilla-Emacs load guard for scholia  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Guards that scholia loads in an `emacs -Q' session with no optional
;; dependency installed.

;;; Code:

(require 'ert)
(require 'scholia-test-helper)

(defconst scholia-smoke-test--load-guard
  '(progn
     (require 'scholia)
     (unless (featurep 'scholia) (kill-emacs 3))
     (when (featurep 'magit-section) (kill-emacs 4))
     (when (featurep 'org-remark) (kill-emacs 5))
     (when (featurep 'scholia-core) (kill-emacs 9))
     (let ((scholia-session-directory (make-temp-file "scholia-smoke-" t))
           (file (make-temp-file "scholia-smoke-" nil ".txt" "alpha beta\n")))
       (unwind-protect
           (with-current-buffer (find-file-noselect file)
             (scholia-mode 1)
             (unless (featurep 'scholia-core) (kill-emacs 10))
             (unless scholia-mode (kill-emacs 6))
             (goto-char (point-min))
             (scholia-annotate "note")
             (unless (scholia-annotation-at (point-min)) (kill-emacs 7))
             (scholia-mode -1)
             (when scholia-mode (kill-emacs 8))
             (set-buffer-modified-p nil)
             (kill-buffer))
         (delete-file file)
         (delete-directory scholia-session-directory t)))
     (kill-emacs 0))
  "Form a vanilla subprocess evaluates to prove scholia works on its own.
Loading the facade leaves the lifecycle unloaded; enabling `scholia-mode'
loads it and exercises annotation through shutdown.")

(ert-deftest scholia-smoke-package-loads-without-optional-dependencies ()
  (let ((emacs (expand-file-name invocation-name invocation-directory))
        (buffer (generate-new-buffer " *scholia-smoke*")))
    (should (file-executable-p emacs))
    (unwind-protect
        (let* ((status (call-process
                        emacs nil buffer nil
                        "-Q" "--batch"
                        "-L" (expand-file-name scholia-test-project-root)
                        "--eval" (prin1-to-string scholia-smoke-test--load-guard)))
               (output (with-current-buffer buffer (buffer-string))))
          (should (equal (cons status output) (cons 0 output))))
      (kill-buffer buffer))))

(ert-deftest scholia-smoke-scholia-mode-is-a-command ()
  (require 'scholia)
  (should (commandp 'scholia-mode)))

(provide 'scholia-smoke-test)
;;; scholia-smoke-test.el ends here
