;;; scholia-ediff-test.el --- Tests for Ediff locations  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'subr-x)
(require 'scholia-test-helper)

(let ((load-prefer-newer t))
  (require 'scholia-locate nil t)
  (require 'scholia-ediff nil t))

(defun scholia-ediff-test--git (directory &rest arguments)
  "Run git with ARGUMENTS in DIRECTORY and return its output."
  (with-temp-buffer
    (let ((default-directory directory))
      (unless (zerop (apply #'process-file "git" nil t nil arguments))
        (error "%s" (buffer-string)))
      (string-trim (buffer-string)))))

(ert-deftest scholia-ediff-resolves-revision-sides-after-startup ()
  (let* ((repository (make-temp-file "scholia-ediff-" t))
         (file (expand-file-name "sample.txt" repository))
         (source nil)
         (control nil)
         (side-a nil)
         (side-b nil))
    (unwind-protect
        (progn
          (scholia-ediff-test--git repository "init" "-q")
          (scholia-ediff-test--git repository "config" "user.email" "test@example.invalid")
          (scholia-ediff-test--git repository "config" "user.name" "Test")
          (with-temp-file file
            (insert "before\nalpha beta\n"))
          (scholia-ediff-test--git repository "add" "sample.txt")
          (scholia-ediff-test--git repository "commit" "-qm" "first")
          (let ((first-revision (scholia-ediff-test--git repository "rev-parse" "HEAD")))
            (with-temp-file file
              (insert "before\nalpha gamma\n"))
            (scholia-ediff-test--git repository "commit" "-am" "second" "-q")
            (let ((second-revision (scholia-ediff-test--git repository "rev-parse" "HEAD")))
              (setq source (find-file-noselect file))
              (with-current-buffer source
                (require 'ediff)
                (require 'ediff-vers)
                (ediff-vc-internal
                 (substring first-revision 0 7) (substring second-revision 0 7)
                 (list (lambda ()
                         (setq control (current-buffer))
                         (setq side-a ediff-buffer-A)
                         (setq side-b ediff-buffer-B))))
                (should (fboundp 'scholia-ediff-location))
                (when (fboundp 'scholia-ediff-location)
                  (goto-char (point-min))
                  (search-forward "gamma")
                  (goto-char (match-beginning 0))
                  (should-not (funcall 'scholia-ediff-location (point)))
                  (should (equal (run-hook-with-args-until-success
                                  'scholia-location-functions (point))
                                 (list :file file :line 2 :column 6 :end-column 11
                                       :revision nil)))
                  (with-current-buffer side-a
                    (goto-char (point-min))
                    (search-forward "beta")
                    (goto-char (match-beginning 0))
                    (should (equal (run-hook-with-args-until-success
                                    'scholia-location-functions (point))
                                   (list :file file :line 2 :column 6 :end-column 10
                                         :revision first-revision))))
                  (with-current-buffer side-b
                    (goto-char (point-min))
                    (search-forward "gamma")
                    (goto-char (match-beginning 0))
                    (should (equal (run-hook-with-args-until-success
                                    'scholia-location-functions (point))
                                   (list :file file :line 2 :column 6 :end-column 11
                                         :revision second-revision)))))))))
      (when (buffer-live-p control)
        (with-current-buffer control
          (let ((ediff-keep-variants t))
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (&rest _) t)))
              (ediff-quit nil)))))
      (dolist (buffer (list side-a side-b source))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (when (file-directory-p repository)
        (delete-directory repository t)))))

(provide 'scholia-ediff-test)
;;; scholia-ediff-test.el ends here
