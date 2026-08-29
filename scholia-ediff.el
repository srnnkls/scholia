;;; scholia-ediff.el --- Ediff locations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Keywords: convenience, tools

;;; Commentary:

;; Resolves annotations made in Ediff revision buffers.

;;; Code:

(require 'ediff-vers)
(require 'scholia-locate)
(require 'subr-x)

(defvar ediff-buffer-A)
(defvar ediff-buffer-B)
(defvar ediff-merge-job)
(defvar ediff-this-buffer-ediff-sessions)

(defvar-local scholia-ediff--file nil
  "File captured for this Ediff revision buffer.")
(defvar-local scholia-ediff--revision nil
  "Revision captured for this Ediff revision buffer.")

(defun scholia-ediff--full-revision (file revision)
  "Return REVISION's full Git hash for FILE."
  (with-temp-buffer
    (let ((default-directory (file-name-directory file)))
      (when (zerop (process-file "git" nil t nil "rev-parse" "--verify"
                                 revision))
        (string-trim (buffer-string))))))

(defun scholia-ediff--stamp-side (buffer file revision)
  "Stamp BUFFER with FILE and REVISION."
  (when revision
    (with-current-buffer buffer
      (setq-local scholia-ediff--file file)
      (setq-local scholia-ediff--revision revision)
      (setq-local scholia-locate--terminally-unresolved t))))

(defun scholia-ediff--capture-revisions
    (function revision-a revision-b &optional startup-hooks)
  "Run FUNCTION after capturing REVISION-A and REVISION-B with STARTUP-HOOKS."
  (let* ((file (expand-file-name (buffer-file-name)))
         (revision-a-hash
          (scholia-ediff--full-revision
           file
           (if (string-empty-p revision-a)
               (ediff-vc-latest-version file)
             revision-a)))
         (revision-b-hash (unless (string-empty-p revision-b)
                            (scholia-ediff--full-revision file revision-b))))
    (funcall function revision-a-hash
             (if (string-empty-p revision-b) revision-b revision-b-hash)
             (cons (lambda ()
                     (scholia-ediff--stamp-side ediff-buffer-A
                                                file revision-a-hash)
                     (scholia-ediff--stamp-side ediff-buffer-B
                                                file revision-b-hash))
                   startup-hooks))))

(defun scholia-ediff-location (position)
  "Return the revision location at POSITION in an Ediff side buffer."
  (when (and scholia-ediff--file
             scholia-ediff--revision
             (= (length ediff-this-buffer-ediff-sessions) 1))
    (let ((control (car ediff-this-buffer-ediff-sessions))
          (buffer (current-buffer))
          (location (save-excursion
                      (goto-char position)
                      (list :file scholia-ediff--file
                            :line (line-number-at-pos position t)
                            :column (- position (line-beginning-position))
                            :end-column (- (line-end-position)
                                           (line-beginning-position))
                            :revision scholia-ediff--revision))))
      (when (buffer-live-p control)
        (with-current-buffer control
          (when (and (not ediff-merge-job)
                     (or (eq buffer ediff-buffer-A)
                         (eq buffer ediff-buffer-B)))
            location))))))

(advice-add 'ediff-vc-internal :around #'scholia-ediff--capture-revisions)
(add-hook 'scholia-location-functions #'scholia-ediff-location)

(provide 'scholia-ediff)
;;; scholia-ediff.el ends here
