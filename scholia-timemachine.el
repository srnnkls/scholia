;;; scholia-timemachine.el --- Timemachine locations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Keywords: convenience, tools

;;; Commentary:

;; Resolves git-timemachine positions to the source locations they show.

;;; Code:

(require 'git-timemachine)
(require 'scholia-locate)
(require 'scholia-vars)

(declare-function scholia-initialize "scholia-core")
(declare-function scholia-shutdown "scholia-core")

(defun scholia-timemachine-locate-position (position)
  "Return the `git-timemachine' source location at POSITION."
  (when (and git-timemachine-mode git-timemachine-revision)
    (save-excursion
      (goto-char position)
      (list :file (expand-file-name (cadr git-timemachine-revision)
                                    git-timemachine-directory)
            :line (line-number-at-pos position t)
            :column (- position (line-beginning-position))
            :end-column (- (line-end-position) (line-beginning-position))
            :revision (car git-timemachine-revision)))))

(defun scholia-timemachine--select-buffer (&rest _)
  "Make the timemachine buffer current after startup."
  (set-buffer (get-buffer (format "timemachine:%s" (buffer-name)))))

(defun scholia-timemachine--redraw (function &rest arguments)
  "Save annotations before calling FUNCTION with ARGUMENTS, then redraw them."
  (if (and scholia-mode git-timemachine-mode)
      (progn
        (scholia-shutdown t)
        (prog1 (apply function arguments)
          (scholia-initialize)))
    (apply function arguments)))

(add-hook 'scholia-location-functions #'scholia-timemachine-locate-position)
(advice-add 'git-timemachine--start :after #'scholia-timemachine--select-buffer)
(advice-add 'git-timemachine-show-revision :around
            #'scholia-timemachine--redraw)

(provide 'scholia-timemachine)
;;; scholia-timemachine.el ends here
