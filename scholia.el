;;; scholia.el --- Annotations as work items  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/scholia
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Annotate a file without changing it, group the annotations into named
;; sessions, and send them where the work happens.
;;
;; Based on annotate.el by Bastian Bechtold and contributors, from which
;; scholia takes its annotation engine.  See LICENSE.

;;; Code:

(require 'scholia-vars)

;;;###autoload (autoload 'scholia-mode "scholia-vars" nil t)
;;;###autoload (autoload 'scholia-session-create "scholia-session" nil t)
;;;###autoload (autoload 'scholia-session-switch "scholia-session" nil t)
;;;###autoload (autoload 'scholia-session-rename "scholia-session" nil t)
;;;###autoload (autoload 'scholia-session-delete "scholia-session" nil t)
;;;###autoload (autoload 'scholia-session-import "scholia-session" nil t)
;;;###autoload
(autoload 'scholia-session-assign-project "scholia-session" nil t)
;;;###autoload
(autoload 'scholia-session-load-assignments "scholia-session" nil t)
;;;###autoload (autoload 'scholia-export "scholia-export" nil t)

(provide 'scholia)
;;; scholia.el ends here
