;;; scholia-magit-test.el --- Tests for Magit diff locations  -*- lexical-binding: t; -*-

(require 'ert)
(require 'magit-diff)

(load "scholia-locate" nil t)

(ert-deftest scholia-magit-resolves-real-diff-positions ()
  (should (require 'scholia-magit nil t))
  (let* ((repo (make-temp-file "scholia-magit-" t))
         (old-file (expand-file-name "old-name.txt" repo))
         (new-file (expand-file-name "renamed-name.txt" repo))
         (second-file (expand-file-name "second.txt" repo))
         revision first-diff second-diff)
    (unwind-protect
        (progn
          (process-file "git" nil nil nil "-C" repo "init")
          (process-file "git" nil nil nil "-C" repo "config" "user.email" "test@example.com")
          (process-file "git" nil nil nil "-C" repo "config" "user.name" "Scholia Test")
          (with-temp-file old-file
            (insert "context-before\nremoved-value\ncontext-after\n"))
          (with-temp-file second-file
            (insert "one\ntwo\n"))
          (process-file "git" nil nil nil "-C" repo "add" ".")
          (process-file "git" nil nil nil "-C" repo "commit" "-m" "initial")
          (with-temp-buffer
            (process-file "git" nil t nil "-C" repo "rev-parse" "HEAD")
            (setq revision (string-trim (buffer-string))))
          (rename-file old-file new-file)
          (with-temp-file new-file
            (insert "context-before\nadded-value\ncontext-after\n"))
          (process-file "git" nil nil nil "-C" repo "add" "-N" new-file)
          (with-temp-file second-file
            (insert "one\ntwo\nthird-added\n"))
          (let ((default-directory repo))
            (setq first-diff (magit-diff-working-tree "HEAD" '("-M"))))
          (with-current-buffer first-diff
            (goto-char (point-min))
            (should-not (run-hook-with-args-until-success
                         'scholia-location-functions (point)))
            (search-forward "+added-value")
            (goto-char (+ (line-beginning-position) 3))
            (let ((location (run-hook-with-args-until-success
                             'scholia-location-functions (point))))
              (should (equal (plist-get location :file) new-file))
              (should (equal (plist-get location :line) 2))
              (should (equal (plist-get location :column) 2))
              (should (equal (plist-get location :end-column) 11))
              (should-not (plist-get location :revision)))
            (goto-char (point-min))
            (search-forward " context-before")
            (goto-char (+ (line-beginning-position) 6))
            (let ((location (run-hook-with-args-until-success
                             'scholia-location-functions (point))))
              (should (equal (plist-get location :file) new-file))
              (should (equal (plist-get location :line) 1))
              (should (equal (plist-get location :column) 5))
              (should (equal (plist-get location :end-column) 14))
              (should-not (plist-get location :revision)))
            (search-forward "-removed-value")
            (goto-char (+ (line-beginning-position) 4))
            (let ((magit-diff-visit-previous-blob t))
              (should (magit-diff-on-removed-line-p))
              (let ((location (run-hook-with-args-until-success
                               'scholia-location-functions (point))))
                (should (equal (plist-get location :file) old-file))
                (should (equal (plist-get location :line) 2))
                (should (equal (plist-get location :column) 3))
                (should (equal (plist-get location :end-column) 13))
                (should (equal (plist-get location :revision) revision))))
            (let ((magit-diff-visit-previous-blob nil))
              (should-not (magit-diff-on-removed-line-p))
              (should-not (run-hook-with-args-until-success
                           'scholia-location-functions (point))))
            (search-forward "+third-added")
            (goto-char (+ (line-beginning-position) 2))
            (let ((location (run-hook-with-args-until-success
                             'scholia-location-functions (point))))
              (should (equal (plist-get location :file) second-file))
              (should (equal (plist-get location :line) 3))
              (should (equal (plist-get location :column) 1))
              (should (equal (plist-get location :end-column) 11))
              (should-not (plist-get location :revision))))
          (process-file "git" nil nil nil "-C" repo "add" ".")
          (with-temp-file second-file
            (insert "one\nthird-added\n"))
          (let ((default-directory repo))
            (setq second-diff (magit-diff-unstaged)))
          (with-current-buffer second-diff
            (search-forward "-two")
            (goto-char (+ (line-beginning-position) 2))
            (let ((magit-diff-visit-previous-blob t))
              (should (magit-diff-on-removed-line-p))
              (should-not (run-hook-with-args-until-success
                           'scholia-location-functions (point))))))
      (when (buffer-live-p first-diff)
        (kill-buffer first-diff))
      (when (buffer-live-p second-diff)
        (kill-buffer second-diff))
      (when (file-directory-p repo)
        (delete-directory repo t)))))

(provide 'scholia-magit-test)
;;; scholia-magit-test.el ends here
