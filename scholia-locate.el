;;; scholia-locate.el --- Resolve source locations  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Keywords: convenience, tools

;;; Commentary:

;; Turns buffer positions into the source locations annotations persist.

;;; Code:

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

(provide 'scholia-locate)
;;; scholia-locate.el ends here
