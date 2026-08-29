;;; scholia-locate.el --- Resolve source locations  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Keywords: convenience, tools

;;; Commentary:

;; Turns buffer positions into the source locations annotations persist.

;;; Code:

(require 'scholia-db)

(defvar scholia-location-functions nil
  "Abnormal hook resolving a buffer position to a source location.
Each function receives a position and returns a location plist or nil.")

(defvar-local scholia-locate--terminally-unresolved nil
  "Whether this buffer must not use the default file resolver.")

(defun scholia-locate-file-position (position)
  "Return the source location at POSITION in a file-visiting buffer."
  (unless scholia-locate--terminally-unresolved
    (when-let ((file (buffer-file-name (or (buffer-base-buffer)
                                           (current-buffer)))))
      (save-excursion
        (goto-char position)
        (list :file (expand-file-name file)
              :line (line-number-at-pos position t)
              :column (- position (line-beginning-position))
              :end-column (- (line-end-position) (line-beginning-position))
              :revision nil)))))

(add-hook 'scholia-location-functions #'scholia-locate-file-position t)

(defun scholia-locate-revision (annotation)
  "Return the revision stored for ANNOTATION."
  (plist-get annotation :revision))

(defun scholia-locate--revision-buffer (file revision)
  "Return a buffer holding FILE at REVISION, or nil."
  (with-temp-buffer
    (let ((default-directory (file-name-directory file)))
      (when (zerop (process-file "git" nil t nil "rev-parse" "--show-toplevel"))
        (let ((root (string-trim (buffer-string))))
          (erase-buffer)
          (when (zerop
                 (process-file "git" nil t nil "-C" root "show"
                               (concat revision ":"
                                       (file-relative-name (file-truename file)
                                                           root))))
            (let ((source (buffer-string))
                  (buffer (generate-new-buffer
                           (format "*scholia %s:%s*"
                                   revision (file-name-nondirectory file)))))
              (with-current-buffer buffer
                (insert source)
                (set-buffer-modified-p nil))
              buffer)))))))

(defun scholia-locate--materialize (annotation)
  "Return ANNOTATION with its line and columns turned into bounds."
  (if (or (scholia-db-annotation-reply-p annotation)
          (scholia-db-annotation-beg annotation))
      annotation
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- (scholia-db-annotation-line annotation)))
        (let ((bol (line-beginning-position)))
          (scholia-db-annotation-set-bounds
           annotation
           (+ bol (scholia-db-annotation-column annotation))
           (+ bol (scholia-db-annotation-end-column annotation))))))))

(defun scholia-locate-materialize (file annotation)
  "Return ANNOTATION with bounds resolved against its source view of FILE."
  (if (or (scholia-db-annotation-reply-p annotation)
          (scholia-db-annotation-beg annotation))
      annotation
    (let* ((revision (scholia-locate-revision annotation))
           (buffer (or (and revision
                            (scholia-locate--revision-buffer file revision))
                       (let ((source (generate-new-buffer " *scholia-source*")))
                         (with-current-buffer source
                           (insert-file-contents file))
                         source))))
      (unwind-protect
          (condition-case nil
              (with-current-buffer buffer
                (scholia-locate--materialize annotation))
            (error annotation))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(defun scholia-locate-open (file annotation)
  "Open FILE at ANNOTATION's stored location."
  (let ((revision (scholia-locate-revision annotation)))
    (if-let ((buffer (and revision
                          (scholia-locate--revision-buffer file revision))))
        (let ((annotation
               (condition-case nil
                   (with-current-buffer buffer
                     (scholia-locate--materialize annotation))
                 (error annotation))))
          (unless (scholia-db-annotation-beg annotation)
            (user-error "Annotation has no stored position"))
          (switch-to-buffer buffer)
          (goto-char (scholia-db-annotation-beg annotation)))
      (find-file file)
      (if revision
          (progn
            (goto-char (point-min))
            (message "Could not read %s at revision %s; opened working tree"
                     file revision))
        (setq annotation (scholia-locate-materialize file annotation))
        (unless (scholia-db-annotation-beg annotation)
          (user-error "Annotation has no stored position"))
        (goto-char (scholia-db-annotation-beg annotation))))))

(provide 'scholia-locate)
;;; scholia-locate.el ends here
