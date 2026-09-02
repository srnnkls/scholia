;;; scholia-magit.el --- Magit locations  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Keywords: convenience, tools

;;; Commentary:

;; Resolves Magit diff positions to the source locations annotations persist.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'magit-diff)
(require 'magit-git)
(require 'scholia-locate)

(defun scholia-magit-locate-position (position)
  "Return the Magit diff source location at POSITION."
  (save-excursion
    (goto-char position)
    (let ((section (magit-current-section)))
      (when (and (cl-typep section 'magit-hunk-section)
                 (not (oref section combined)))
        (let ((from (magit-diff-on-removed-line-p))
              (removed (eq (char-after (line-beginning-position)) ?-)))
          (when (or from (not removed))
            (let* ((file-section (magit-diff--file-section))
                   (file (if from
                             (or (oref file-section source)
                                 (magit-file-at-point t))
                           (magit-file-at-point t)))
                   (sides (magit-diff-visit--sides))
                   (old-revision
                    (magit-commit-oid (or (caar sides)
                                          magit-buffer-diff-range)
                                      t))
                   (new-revision (magit-commit-oid (caadr sides) t)))
              (when (or (not from) old-revision)
                (scholia-locate-make-location
                 (expand-file-name file)
                 (magit-diff-hunk-line section from)
                 (magit-diff-hunk-column section from)
                 (save-excursion
                   (end-of-line)
                   (magit-diff-hunk-column section from))
                 (if from old-revision new-revision))))))))))

(defun scholia-magit-setup ()
  "Enable Scholia source locations in Magit diff buffers."
  (add-hook 'scholia-location-functions #'scholia-magit-locate-position))

(defun scholia-magit-teardown ()
  "Disable Scholia source locations in Magit diff buffers."
  (remove-hook 'scholia-location-functions #'scholia-magit-locate-position))

(defun scholia-magit-unload-function ()
  "Remove global state installed by `scholia-magit-setup'."
  (scholia-magit-teardown)
  nil)

(provide 'scholia-magit)
;;; scholia-magit.el ends here
