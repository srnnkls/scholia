;;; scholia-timemachine-test.el --- Tests for git-timemachine locations  -*- lexical-binding: t; -*-

(require 'ert)
(require 'magit-diff)
(require 'git-timemachine)
(require 'scholia-test-helper)

(eval-and-compile
  (setq load-prefer-newer t)
  (require 'scholia-vars nil t)
  (require 'scholia-db nil t)
  (require 'scholia-core nil t)
  (load "scholia-locate" nil t)
  (require 'scholia-magit nil t))

(eval-when-compile
  (when (bound-and-true-p byte-compile-current-file)
    (require 'scholia-timemachine)))

(declare-function scholia-timemachine-locate-position "scholia-timemachine")
(declare-function scholia-timemachine--redraw "scholia-timemachine")
(declare-function scholia-timemachine--select-buffer "scholia-timemachine")
(declare-function scholia-timemachine-setup "scholia-timemachine")
(declare-function scholia-timemachine-teardown "scholia-timemachine")

(defun scholia-timemachine-test--git (repository &rest arguments)
  "Run git ARGUMENTS in REPOSITORY and return its standard output."
  (with-temp-buffer
    (unless (zerop (apply #'process-file "git" nil t nil "-C" repository arguments))
      (error "git %s failed: %s" (string-join arguments " ") (buffer-string)))
    (string-trim (buffer-string))))

(ert-deftest scholia-timemachine-integration-lifecycle-is-reversible ()
  (when (featurep 'scholia-timemachine)
    (unload-feature 'scholia-timemachine t))
  (should (require 'scholia-timemachine))
  (should-not (memq #'scholia-timemachine-locate-position
                    scholia-location-functions))
  (should-not (advice-member-p #'scholia-timemachine--select-buffer
                               'git-timemachine--start))
  (should-not (advice-member-p #'scholia-timemachine--redraw
                               'git-timemachine-show-revision))
  (scholia-timemachine-setup)
  (scholia-timemachine-setup)
  (should (= (seq-count (lambda (function)
                          (eq function #'scholia-timemachine-locate-position))
                        scholia-location-functions)
             1))
  (should (advice-member-p #'scholia-timemachine--select-buffer
                           'git-timemachine--start))
  (should (advice-member-p #'scholia-timemachine--redraw
                           'git-timemachine-show-revision))
  (scholia-timemachine-teardown)
  (should-not (memq #'scholia-timemachine-locate-position
                    scholia-location-functions))
  (should-not (advice-member-p #'scholia-timemachine--select-buffer
                               'git-timemachine--start))
  (should-not (advice-member-p #'scholia-timemachine--redraw
                               'git-timemachine-show-revision))
  (scholia-timemachine-setup)
  (unload-feature 'scholia-timemachine t)
  (should-not (memq 'scholia-timemachine-locate-position
                    scholia-location-functions))
  (should-not (advice-member-p 'scholia-timemachine--select-buffer
                               'git-timemachine--start))
  (should-not (advice-member-p 'scholia-timemachine--redraw
                               'git-timemachine-show-revision))
  (should (require 'scholia-timemachine))
  (scholia-timemachine-setup)
  (should (memq #'scholia-timemachine-locate-position
                scholia-location-functions))
  (scholia-timemachine-teardown))

(ert-deftest scholia-timemachine-round-trips-renamed-revision-annotations ()
  (should (require 'scholia-timemachine nil t))
  (scholia-magit-setup)
  (scholia-timemachine-setup)
  (when (featurep 'scholia-timemachine)
    (scholia-test-with-session-directory
      (let* ((repository (make-temp-file "scholia-timemachine-" t))
             (old-file (expand-file-name "old-name.txt" repository))
             (new-file (expand-file-name "new-name.txt" repository))
             first-revision second-revision diff-buffer timemachine-buffer)
        (unwind-protect
            (progn
              (scholia-timemachine-test--git repository "init")
              (scholia-timemachine-test--git repository "config" "user.email" "test@example.com")
              (scholia-timemachine-test--git repository "config" "user.name" "Scholia Test")
              (with-temp-file old-file
                (insert "unchanged one\nunchanged two\ntarget revision one\nunchanged three\nunchanged four\n"))
              (scholia-timemachine-test--git repository "add" ".")
              (scholia-timemachine-test--git repository "commit" "-m" "first revision")
              (setq first-revision
                    (scholia-timemachine-test--git repository "rev-parse" "HEAD"))
              (scholia-timemachine-test--git repository "mv" "old-name.txt" "new-name.txt")
              (with-temp-file new-file
                (insert "unchanged one\nunchanged two\ntarget revision two\nunchanged three\nunchanged four\n"))
              (scholia-timemachine-test--git repository "add" ".")
              (scholia-timemachine-test--git repository "commit" "-m" "renamed revision")
              (setq second-revision
                    (scholia-timemachine-test--git repository "rev-parse" "HEAD"))
              (let ((default-directory repository))
                (setq diff-buffer
                      (magit-diff-range (format "%s..%s" first-revision second-revision)
                                        '("-M"))))
              (with-current-buffer diff-buffer
                (search-forward "-target revision one")
                (goto-char (+ (line-beginning-position) 3))
                (setq-local scholia-session "timemachine")
                (scholia-mode 1)
                (scholia-annotate "from Magit at the first revision")
                (scholia-save-annotations)
                (let* ((record (scholia-db-record (scholia-session-file) old-file))
                       (annotation (car (scholia-db-record-annotations record)))
                       (other (copy-tree annotation)))
                  (should record)
                  (should (equal (scholia-db-annotation-revision annotation)
                                 first-revision))
                  (plist-put other :id "second-revision-decoy")
                  (plist-put other :text "from the wrong revision")
                  (plist-put other :revision second-revision)
                  (scholia-db-store-record
                   (scholia-session-file)
                   (scholia-db-make-record
                    old-file
                    (list annotation other)
                    (scholia-db-record-checksum record)))))
              (let ((default-directory repository))
                (find-file new-file)
                (git-timemachine)
                (setq timemachine-buffer (current-buffer)))
              (with-current-buffer timemachine-buffer
                (should (equal (car git-timemachine-revision) second-revision))
                (git-timemachine-show-previous-revision)
                (should (equal (car git-timemachine-revision) first-revision))
                (setq-local scholia-session "timemachine")
                (scholia-mode 1)
                (should (equal (mapcar (lambda (chain)
                                         (overlay-get (car chain) 'scholia-annotation))
                                       (scholia-buffer-chains))
                               '("from Magit at the first revision")))
                (git-timemachine-show-next-revision)
                (should (equal (car git-timemachine-revision) second-revision))
                (should-not (scholia-buffer-chains))
                (search-forward "target revision two")
                (goto-char (+ (line-beginning-position) 1))
                (scholia-annotate "from timemachine at the second revision")
                (should (equal (mapcar (lambda (chain)
                                         (overlay-get (car chain) 'scholia-annotation))
                                       (scholia-buffer-chains))
                               '("from timemachine at the second revision")))
                (git-timemachine-show-previous-revision)
                (should (equal (car git-timemachine-revision) first-revision))
                (let* ((record (scholia-db-record (scholia-session-file) new-file))
                       (annotation (car (scholia-db-record-annotations record))))
                  (should record)
                  (should (equal (scholia-db-annotation-revision annotation)
                                 second-revision)))
                (should (equal (mapcar (lambda (chain)
                                         (overlay-get (car chain) 'scholia-annotation))
                                       (scholia-buffer-chains))
                               '("from Magit at the first revision")))
                (git-timemachine-show-next-revision)
                (should (equal (car git-timemachine-revision) second-revision))
                (should (equal (mapcar (lambda (chain)
                                         (overlay-get (car chain) 'scholia-annotation))
                                       (scholia-buffer-chains))
                               '("from timemachine at the second revision")))))
          (when (buffer-live-p diff-buffer)
            (kill-buffer diff-buffer))
          (when (buffer-live-p timemachine-buffer)
            (with-current-buffer timemachine-buffer
              (set-buffer-modified-p nil))
            (kill-buffer timemachine-buffer))
          (when (file-directory-p repository)
            (delete-directory repository t))
          (scholia-timemachine-teardown)
          (scholia-magit-teardown))))))

(provide 'scholia-timemachine-test)
;;; scholia-timemachine-test.el ends here
