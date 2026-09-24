;;; scholia.el --- Annotations as work items  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/scholia
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (cera "0.1.0"))
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Annotate a file without changing it, group the annotations into named
;; sessions, and send them where the work happens.
;;
;; annotate.el by Bastian Bechtold and contributors is prior art.  Its source
;; informed edge cases covered by scholia's tests; no annotate.el code is copied
;; or retained here.  See LICENSE.

;;; Code:

(require 'scholia-vars)

(declare-function scholia-initialize "scholia-core")
(declare-function scholia-shutdown "scholia-core")
(declare-function scholia-session-name "scholia-core")

;;;###autoload
(autoload 'scholia-save-annotations "scholia-core" nil t)
;;;###autoload
(autoload 'scholia-annotate "scholia-core" nil t)
;;;###autoload
(autoload 'scholia-edit-annotation "scholia-core" nil t)
;;;###autoload
(autoload 'scholia-delete-annotation "scholia-core" nil t)
;;;###autoload
(autoload 'scholia-reply-to "scholia-core" nil t)
;;;###autoload
(autoload 'scholia-goto-next-annotation "scholia-core" nil t)
;;;###autoload
(autoload 'scholia-goto-previous-annotation "scholia-core" nil t)
;;;###autoload
(autoload 'scholia-session-create "scholia-session" nil t)
;;;###autoload
(autoload 'scholia-session-switch "scholia-session" nil t)
;;;###autoload
(autoload 'scholia-session-show "scholia-session" nil t)
;;;###autoload
(autoload 'scholia-session-hide "scholia-session" nil t)
;;;###autoload
(autoload 'scholia-session-toggle "scholia-session" nil t)
;;;###autoload
(autoload 'scholia-session-rename "scholia-session" nil t)
;;;###autoload
(autoload 'scholia-session-delete "scholia-session" nil t)
;;;###autoload
(autoload 'scholia-session-import "scholia-session" nil t)
;;;###autoload
(autoload 'scholia-session-export "scholia-session" nil t)
;;;###autoload
(autoload 'scholia-session-assign-project "scholia-session" nil t)
;;;###autoload
(autoload 'scholia-session-load-assignments "scholia-session" nil t)
;;;###autoload
(autoload 'scholia-export "scholia-export" nil t)
;;;###autoload
(autoload 'scholia-export-session "scholia-export" nil t)
;;;###autoload
(autoload 'scholia-search "scholia-search" nil t)
;;;###autoload
(autoload 'scholia-search-sends "scholia-search" nil t)

(defvar scholia-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-c C-a" #'scholia-annotate)
    (keymap-set map "C-c C-c" #'scholia-edit-annotation)
    (keymap-set map "C-c C-d" #'scholia-delete-annotation)
    (keymap-set map "C-c C-r" #'scholia-reply-to)
    (keymap-set map "C-c C-e" #'scholia-export)
    (keymap-set map "C-c C-f" #'scholia-search)
    (keymap-set map "C-c C-n" #'scholia-goto-next-annotation)
    (keymap-set map "C-c C-p" #'scholia-goto-previous-annotation)
    map)
  "Keymap of command `scholia-mode'.")

;;;###autoload
(define-minor-mode scholia-mode
  "Toggle Scholia mode.
Annotations are shown alongside the buffer text and stored in a session
database, leaving the file itself unchanged.

\\{scholia-mode-map}"
  :lighter (:eval (format " Sch:%s" (scholia-session-name)))
  (require 'scholia-core)
  (if scholia-mode
      (scholia-initialize)
    (scholia-shutdown scholia-autosave)))

(provide 'scholia)
;;; scholia.el ends here
