;;; scholia-harness-test.el --- Tests for the scholia test harness  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Covers the fixtures in `scholia-test-helper' and the harness deliverables
;; they depend on: the Eask test-only dependencies and the herdr exclusion.

;;; Code:

(require 'ert)
(require 'seq)
(require 'scholia-test-helper)

(defun scholia-harness-test--read-forms (path)
  "Return the list of top-level forms read from PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (let ((forms nil)
          (done nil))
      (while (not done)
        (condition-case nil
            (push (read (current-buffer)) forms)
          (end-of-file (setq done t))))
      (nreverse forms))))

(defun scholia-harness-test--depended-package-names (forms)
  "Return the package names named by `depends-on' entries in FORMS."
  (delq nil
        (mapcar (lambda (form)
                  (and (consp form)
                       (eq (car form) 'depends-on)
                       (nth 1 form)))
                forms)))

(defun scholia-harness-test--eask-path ()
  "Return the absolute path of the repository Eask file."
  (scholia-test-project-file "Eask"))


;;;; Temporary file buffer factory

(ert-deftest scholia-harness-temp-file-buffer-visits-a-real-file ()
  (let ((file nil)
        (buffer nil))
    (scholia-test-with-temp-file-buffer buf "alpha\nbeta\n"
      (setq buffer buf
            file (buffer-file-name buf))
      (should (eq buf (current-buffer)))
      (should (stringp file))
      (should (file-exists-p file))
      (should (equal (buffer-string) "alpha\nbeta\n"))
      (should (equal (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))
                     "alpha\nbeta\n")))
    (should (stringp file))
    (should-not (file-exists-p file))
    (should-not (buffer-live-p buffer))))

(ert-deftest scholia-harness-temp-file-buffer-cleans-up-after-a-failure ()
  (let ((file nil)
        (buffer nil))
    (should-error
     (scholia-test-with-temp-file-buffer buf "gamma"
       (setq buffer buf
             file (buffer-file-name buf))
       (error "Boom"))
     :type 'error)
    (should (stringp file))
    (should-not (file-exists-p file))
    (should-not (buffer-live-p buffer))))

(ert-deftest scholia-harness-temp-file-buffer-cleanup-ignores-a-reassigned-var ()
  (let ((created nil)
        (bystander (generate-new-buffer "*scholia-bystander*")))
    (unwind-protect
        (progn
          (with-current-buffer bystander (insert "keep me"))
          (scholia-test-with-temp-file-buffer buf "delta"
            (setq created buf)
            (setq buf bystander))
          (should-not (buffer-live-p created))
          (should (buffer-live-p bystander))
          (should (equal (with-current-buffer bystander (buffer-string))
                         "keep me")))
      (when (buffer-live-p created)
        (with-current-buffer created (set-buffer-modified-p nil))
        (kill-buffer created))
      (when (buffer-live-p bystander)
        (kill-buffer bystander)))))


;;;; Session directory fixture

(ert-deftest scholia-harness-session-directory-exists-then-is-removed ()
  (let ((inner nil))
    (scholia-test-with-session-directory
      (setq inner (default-value 'scholia-session-directory))
      (should (stringp inner))
      (should (file-directory-p inner))
      (write-region "x" nil (expand-file-name "sentinel" inner) nil 'silent))
    (should (stringp inner))
    (should-not (file-exists-p inner))))

(ert-deftest scholia-harness-session-directory-restores-the-outer-value ()
  (let* ((had (boundp 'scholia-session-directory))
         (previous (and had (default-value 'scholia-session-directory)))
         (outer "/scholia-outer-sentinel/")
         (inner nil))
    (unwind-protect
        (progn
          (set-default 'scholia-session-directory outer)
          (scholia-test-with-session-directory
            (setq inner (default-value 'scholia-session-directory))
            (should-not (equal inner outer)))
          (should (equal (default-value 'scholia-session-directory) outer)))
      (if had
          (set-default 'scholia-session-directory previous)
        (makunbound 'scholia-session-directory)))))

(ert-deftest scholia-harness-session-directory-leaves-symbol-unbound-when-it-was ()
  (let* ((had (boundp 'scholia-session-directory))
         (previous (and had (default-value 'scholia-session-directory))))
    (unwind-protect
        (progn
          (makunbound 'scholia-session-directory)
          (scholia-test-with-session-directory
            (should (stringp (default-value 'scholia-session-directory))))
          (should-not (boundp 'scholia-session-directory)))
      (if had
          (set-default 'scholia-session-directory previous)
        (makunbound 'scholia-session-directory)))))


;;;; Overlay range assertion

(ert-deftest scholia-harness-overlay-range-assertion-accepts-a-matching-overlay ()
  (with-temp-buffer
    (insert "0123456789")
    (scholia-test-should-overlay-range (make-overlay 3 7) 3 7)))

(ert-deftest scholia-harness-overlay-range-assertion-signals-on-a-mismatch ()
  (with-temp-buffer
    (insert "0123456789")
    (let ((overlay (make-overlay 3 7)))
      (should-error (scholia-test-should-overlay-range overlay 3 8)
                    :type 'ert-test-failed)
      (should-error (scholia-test-should-overlay-range overlay 2 7)
                    :type 'ert-test-failed))
    (should-error (scholia-test-should-overlay-range "not an overlay" 3 7)
                  :type 'ert-test-failed)))


;;;; Eask declarations

(ert-deftest scholia-harness-eask-declares-magit-section-and-org-remark-as-test-only ()
  (let* ((forms (scholia-harness-test--read-forms (scholia-harness-test--eask-path)))
         (development (seq-find (lambda (form)
                                  (and (consp form) (eq (car form) 'development)))
                                forms)))
    (should development)
    (let ((development-packages
           (scholia-harness-test--depended-package-names (cdr development)))
          (runtime-packages
           (scholia-harness-test--depended-package-names forms)))
      (dolist (package '("magit-section" "org-remark"))
        (should (member package development-packages))
        (should-not (member package runtime-packages))))))

(ert-deftest scholia-harness-eask-resolves-the-herdr-exclusion ()
  (skip-unless (fboundp 'eask-expand-file-specs))
  (let* ((default-directory scholia-test-project-root)
         (test-files (mapcar #'file-name-nondirectory
                             (eask-expand-file-specs scholia-test-file-spec)))
         (package-files (mapcar #'file-name-nondirectory
                                (eask-expand-file-specs (eask-files-spec)))))
    (should (member "scholia-smoke-test.el" test-files))
    (should-not (member "scholia-herdr-test.el" test-files))
    (should (member "scholia.el" package-files))
    (should-not (member "scholia-herdr.el" package-files))))

(provide 'scholia-harness-test)
;;; scholia-harness-test.el ends here
