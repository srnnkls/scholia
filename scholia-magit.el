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
                (list :file (expand-file-name file)
                      :line (magit-diff-hunk-line section from)
                      :column (magit-diff-hunk-column section from)
                      :end-column (save-excursion
                                    (end-of-line)
                                    (magit-diff-hunk-column section from))
                      :revision (if from old-revision new-revision))))))))))

(add-hook 'scholia-location-functions #'scholia-magit-locate-position)

(provide 'scholia-magit)
;;; scholia-magit.el ends here
