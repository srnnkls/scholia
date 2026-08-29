;;; scholia-locate-test.el --- Tests for buffer location resolution  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'scholia-test-helper)

(let ((load-prefer-newer t))
  (require 'scholia-vars nil t)
  (require 'scholia-db nil t)
  (require 'scholia-core nil t))

(defun scholia-locate-test--report (thunk)
  "Return the messages THUNK reports."
  (let ((reported nil))
    (cl-letf (((symbol-function 'message)
               (lambda (format &rest arguments)
                 (when format
                   (push (apply #'format-message format arguments) reported)))))
      (funcall thunk))
    (nreverse reported)))

(ert-deftest scholia-locate-resolves-specialized-and-file-visiting-positions ()
  (scholia-test-with-temp-file-buffer buffer "alpha beta\n"
    (let* ((file (buffer-file-name buffer))
           (specialized (lambda (position)
                          (when (and (eq buffer (current-buffer))
                                     (= position (point-min)))
                            '(:file "/specialized.txt" :line 7 :column 2
                              :end-column 5 :revision "abc123")))))
      (cl-progv '(scholia-location-functions) (list (list specialized))
        (load "scholia-locate" nil t)
        (should (equal (run-hook-with-args-until-success
                        'scholia-location-functions (point-min))
                       '(:file "/specialized.txt" :line 7 :column 2
                         :end-column 5 :revision "abc123")))
        (goto-char 7)
        (let ((location (run-hook-with-args-until-success
                         'scholia-location-functions (point))))
          (should (equal (plist-get location :file) file))
          (should (equal (plist-get location :line) 1))
          (should (equal (plist-get location :column) 6))
          (should (equal (plist-get location :end-column) 10))
          (should-not (plist-get location :revision)))
        (with-temp-buffer
          (insert "unclaimed\n")
          (should-not (run-hook-with-args-until-success
                       'scholia-location-functions (point-min))))))))

(ert-deftest scholia-locate-saves-transient-chains-additively-by-resolved-file ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer first "alpha beta\n"
      (scholia-test-with-temp-file-buffer second "alpha beta\n"
        (let* ((first-file (buffer-file-name first))
               (second-file (buffer-file-name second))
               (session (expand-file-name "transient.eld" scholia-session-directory))
               (hook-bound (boundp 'scholia-location-functions))
               (saved-hook (and hook-bound scholia-location-functions))
               (resolver
                (lambda (position)
                  (if (< position 7)
                      (list :file first-file :line 19 :column 4 :end-column 10
                            :revision "resolved-first-revision")
                    (list :file second-file :line 1 :column 6 :end-column 10
                          :revision nil)))))
          (scholia-db-store-record
           session
           (scholia-db-make-record
            first-file
            (list (scholia-db-make-annotation
                   "id-prior-one" "already on alpha" 1 6 "alpha"))
            "first-file-checksum"))
          (scholia-db-store-record
           session
           (scholia-db-make-record
            second-file
            (list (scholia-db-make-annotation
                   "id-prior-two" "already on beta" 7 11 "beta"))
            "second-file-checksum"))
          (unwind-protect
              (progn
                (set 'scholia-location-functions (list resolver))
                (with-temp-buffer
                  (insert "alpha beta\n")
                  (setq-local scholia-session "transient")
                  (scholia-mode 1)
                  (goto-char 2)
                  (scholia-annotate "new for the first file")
                  (goto-char 8)
                  (scholia-annotate "new for the second file")
                  (scholia-save-annotations)
                  (let ((first-record (scholia-db-record session first-file))
                        (second-record (scholia-db-record session second-file)))
                    (should (equal (sort (mapcar #'scholia-db-annotation-text
                                                  (scholia-db-record-annotations first-record))
                                         #'string<)
                                   '("already on alpha" "new for the first file")))
                    (should (equal (sort (mapcar #'scholia-db-annotation-text
                                                  (scholia-db-record-annotations second-record))
                                         #'string<)
                                   '("already on beta" "new for the second file")))
                    (should (equal (scholia-db-record-checksum first-record)
                                   "first-file-checksum"))
                    (should (equal (scholia-db-record-checksum second-record)
                                   "second-file-checksum"))
                    (let ((annotation
                           (seq-find (lambda (candidate)
                                       (equal (scholia-db-annotation-text candidate)
                                              "new for the first file"))
                                     (scholia-db-record-annotations first-record))))
                      (should annotation)
                      (should (equal (plist-get annotation :line) 19))
                      (should (equal (plist-get annotation :column) 4))
                      (should (equal (plist-get annotation :end-column) 10))
                      (should (equal (plist-get annotation :revision)
                                     "resolved-first-revision"))))
                  (let ((scholia-autosave nil))
                    (scholia-mode -1))))
            (if hook-bound
                (set 'scholia-location-functions saved-hook)
              (makunbound 'scholia-location-functions))))))))

(ert-deftest scholia-locate-persists-and-displays-revision-annotations ()
  (should (fboundp 'scholia-db-annotation-revision))
  (when (fboundp 'scholia-db-annotation-revision)
    (should (custom-variable-p 'scholia-show-revision-annotations))
    (scholia-test-with-session-directory
      (scholia-test-with-temp-file-buffer buffer "alpha beta\n"
        (let* ((file (buffer-file-name buffer))
               (session (expand-file-name "revision.eld" scholia-session-directory))
               (revision "0123456789abcdef0123456789abcdef01234567")
               (annotation (list :id "id-revision" :beg 1 :end 6
                                 :text "from the revision" :annotated-text "alpha"
                                 :line 1 :line-text "alpha beta"
                                 :column 0 :end-column 5 :color 0 :position :margin
                                 :reply-to nil :sends nil :revision revision)))
          (setq-local scholia-session "revision")
          (scholia-db-store-record
           session (scholia-db-make-record file (list annotation)
                                           (scholia-buffer-checksum)))
          (should (equal (scholia-db-annotation-revision
                          (car (scholia-db-record-annotations
                                (scholia-db-record session file))))
                         revision))
          (let ((scholia-show-revision-annotations nil)
                (scholia-autosave nil))
            (let ((first-open (scholia-locate-test--report
                               (lambda () (scholia-mode 1)))))
              (should (seq-some (lambda (reported)
                                  (string-match-p
                                   "1.*scholia-show-revision-annotations"
                                   reported))
                                first-open))
              (should-not (scholia-buffer-chains)))
            (scholia-mode -1)
            (let ((second-open (scholia-locate-test--report
                                (lambda () (scholia-mode 1)))))
              (should-not (seq-some (lambda (reported)
                                      (string-match-p "changed on disk" reported))
                                    second-open)))
            (scholia-mode -1))
          (let ((scholia-show-revision-annotations t)
                (scholia-autosave nil))
            (scholia-mode 1)
            (let ((rendered (overlay-get (car (car (scholia-buffer-chains)))
                                         'after-string)))
              (should (stringp rendered))
              (should (string-match-p (regexp-quote revision) rendered)))
            (scholia-mode -1)))))))

(provide 'scholia-locate-test)
;;; scholia-locate-test.el ends here
