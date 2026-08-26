;;; scholia-test-helper.el --- Fixtures for the scholia test suites  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Shared fixtures loaded by every scholia ERT suite.  Requires neither
;; `magit-section' nor `org-remark', so the smoke test stays runnable under
;; a vanilla `emacs -Q'.

;;; Code:

(require 'ert)
(require 'seq)

(defconst scholia-test-project-root
  (file-name-as-directory
   (expand-file-name
    ".."
    (file-name-directory (or load-file-name buffer-file-name default-directory))))
  "Absolute path of the scholia repository root.")

(defun scholia-test-project-file (relative)
  "Return the absolute path of RELATIVE inside the scholia repository."
  (expand-file-name relative scholia-test-project-root))

(defun scholia-test--write-temp-file (content)
  "Write CONTENT into a fresh temporary file and return its name."
  (make-temp-file "scholia-test-" nil ".txt" content))

(defconst scholia-test-sqlite-magic "SQLite format 3\0"
  "The sixteen bytes every SQLite database file opens with.")

(defun scholia-test-file-magic (file)
  "Return the first sixteen bytes of FILE as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil 0 16)
    (buffer-string)))

(defmacro scholia-test-with-temp-file-buffer (var content &rest body)
  "Evaluate BODY with VAR bound to a buffer visiting a temporary file.
CONTENT is written to the file before it is visited, and BODY runs with
that buffer current.  The buffer is killed and the file deleted when BODY
exits, however it exits."
  (declare (indent 2) (debug (symbolp form body)))
  (let ((file (make-symbol "file"))
        (buffer (make-symbol "buffer")))
    `(let* ((,file (scholia-test--write-temp-file ,content))
            (,buffer (find-file-noselect ,file)))
       (unwind-protect
           (with-current-buffer ,buffer
             (let ((,var ,buffer))
               ,@body))
         (when (buffer-live-p ,buffer)
           (with-current-buffer ,buffer (set-buffer-modified-p nil))
           (kill-buffer ,buffer))
         (when (file-exists-p ,file)
           (delete-file ,file))))))

(defmacro scholia-test-with-session-directory (&rest body)
  "Evaluate BODY with `scholia-session-directory' set to a fresh directory.
The directory exists for the duration of BODY and is deleted afterwards.
Any previous value of `scholia-session-directory' is restored, and the
symbol is left unbound again when it was unbound before, so the macro
works both before and after the variable is defined."
  (declare (indent 0) (debug body))
  (let ((dir (make-symbol "dir"))
        (bound (make-symbol "bound"))
        (previous (make-symbol "previous")))
    `(let* ((,dir (file-name-as-directory
                   (make-temp-file "scholia-session-" t)))
            (,bound (boundp 'scholia-session-directory))
            (,previous (and ,bound (default-value 'scholia-session-directory))))
       (set-default 'scholia-session-directory ,dir)
       (unwind-protect
           (progn ,@body)
         (if ,bound
             (set-default 'scholia-session-directory ,previous)
           (makunbound 'scholia-session-directory))
         (when (file-directory-p ,dir)
           (delete-directory ,dir t))))))

(defun scholia-test-should-overlay-range (overlay beg end)
  "Assert that OVERLAY is a live overlay spanning BEG to END.
Signal an ERT test failure when it is not."
  (should (overlayp overlay))
  (should (buffer-live-p (overlay-buffer overlay)))
  (should (equal (cons beg end)
                 (cons (overlay-start overlay) (overlay-end overlay)))))

(provide 'scholia-test-helper)
;;; scholia-test-helper.el ends here
