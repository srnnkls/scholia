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

(defvar scholia-mode)

(defun scholia-timemachine-locate-position (position)
  "Return the `git-timemachine' source location at POSITION."
  (when (and git-timemachine-mode git-timemachine-revision)
    (save-excursion
      (goto-char position)
      (scholia-locate-make-location
       (expand-file-name (cadr git-timemachine-revision)
                         git-timemachine-directory)
       (line-number-at-pos position t)
       (- position (line-beginning-position))
       (- (line-end-position) (line-beginning-position))
       (car git-timemachine-revision)))))

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

(defun scholia-timemachine-setup ()
  "Enable Scholia source locations in `git-timemachine' buffers.
Advice is required because `git-timemachine' exposes no revision-change hooks."
  (add-hook 'scholia-location-functions #'scholia-timemachine-locate-position)
  (unless (advice-member-p #'scholia-timemachine--select-buffer
                           'git-timemachine--start)
    (advice-add 'git-timemachine--start :after
                #'scholia-timemachine--select-buffer))
  (unless (advice-member-p #'scholia-timemachine--redraw
                           'git-timemachine-show-revision)
    (advice-add 'git-timemachine-show-revision :around
                #'scholia-timemachine--redraw)))

(defun scholia-timemachine-teardown ()
  "Disable Scholia source locations in `git-timemachine' buffers."
  (remove-hook 'scholia-location-functions #'scholia-timemachine-locate-position)
  (advice-remove 'git-timemachine--start #'scholia-timemachine--select-buffer)
  (advice-remove 'git-timemachine-show-revision #'scholia-timemachine--redraw))

(defun scholia-timemachine-unload-function ()
  "Remove global state installed by `scholia-timemachine-setup'."
  (scholia-timemachine-teardown)
  nil)

(provide 'scholia-timemachine)
;;; scholia-timemachine.el ends here
