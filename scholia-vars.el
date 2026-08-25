;;; scholia-vars.el --- Shared scholia definitions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/scholia
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The leaf every other scholia module requires.  It requires no scholia
;; module itself, so the halves above it never require each other.

;;; Code:

(declare-function scholia-annotate "scholia-core")
(declare-function scholia-delete-annotation "scholia-core")
(declare-function scholia-reply-to "scholia-core")
(declare-function scholia-initialize "scholia-core")
(declare-function scholia-shutdown "scholia-core")
(declare-function scholia-status "scholia-status")
(declare-function project-current "project")
(declare-function project-root "project")
(declare-function projectile-project-root "ext:projectile")

(defgroup scholia nil
  "Annotate files without changing them."
  :group 'text
  :prefix "scholia-")


;;;; Sessions

(defcustom scholia-session-directory
  (locate-user-emacs-file "scholia/sessions/")
  "Directory holding the session databases."
  :type 'directory)

(defcustom scholia-session nil
  "Name of the session this buffer annotates into.
Nil means the session resolved from `scholia-project-sessions', or the
global default when no project matches."
  :type '(choice (const :tag "Resolve from project or default" nil)
                 (string :tag "Session name"))
  :local t)

(defcustom scholia-project-sessions nil
  "Alist mapping a project root to the session name used inside it."
  :type '(alist :key-type (directory :tag "Project root")
                :value-type (string :tag "Session name")))

(defun scholia-project-root ()
  "Return the root of the current project, or nil when there is none.
Asks projectile when it is loaded and project.el otherwise.  The root is
returned absolute and slash-terminated, so roots from either backend key
the same entry of `scholia-project-sessions'."
  (let ((root (or (and (fboundp 'projectile-project-root)
                       (projectile-project-root))
                  (let ((project (and (fboundp 'project-current)
                                      (project-current))))
                    (and project (project-root project))))))
    (and root (file-name-as-directory (expand-file-name root)))))

(defcustom scholia-project-root-function #'scholia-project-root
  "Function returning the root of the current project, or nil.
Its value decides which entry of `scholia-project-sessions' applies."
  :type 'function)

(defcustom scholia-autosave t
  "Whether scholia stores the annotations before it lets go of a buffer.
Turning command `scholia-mode' off reads it, and so does killing an
annotated buffer.  Nil leaves storing them to the caller, which then
owns whatever is on screen when the mode goes down or the buffer dies."
  :type 'boolean)


;;;; Export and sending

(defcustom scholia-export-format 'rustc
  "Format `scholia-export' produces."
  :type '(choice (const :tag "rustc diagnostics" rustc)
                 (const :tag "Unified diff" diff)
                 (const :tag "Annotations integrated into the text" integrate)))

(defcustom scholia-herdr-default-target nil
  "Herdr target sends go to without asking.
Nil prompts for a target instead."
  :type '(choice (const :tag "Ask" nil)
                 (string :tag "Target")))

(defcustom scholia-herdr-send-format nil
  "Format annotations are rendered in when sent to herdr.
Nil falls back to `scholia-export-format'."
  :type '(choice (const :tag "Follow `scholia-export-format'" nil)
                 (const :tag "rustc diagnostics" rustc)
                 (const :tag "Unified diff" diff)
                 (const :tag "Annotations integrated into the text" integrate)))


;;;; Appearance and behaviour

(defcustom scholia-annotation-history-limit 200
  "How many past annotations are offered as recurring candidates."
  :type 'natnum)

(defcustom scholia-highlight-faces '((:underline "#EEF192")
                                     (:underline "#92EEF1")
                                     (:underline "#F192EE"))
  "Face attribute plists cycled over annotated text."
  :type '(repeat plist))

(defcustom scholia-annotation-text-faces
  '((:background "#EEF192" :foreground "black")
    (:background "#92EEF1" :foreground "black")
    (:background "#F192EE" :foreground "black"))
  "Face attribute plists cycled over annotation text.
Each entry pairs with the entry of `scholia-highlight-faces' at the same
position."
  :type '(repeat plist))

(defface scholia-prefix
  '((t (:inherit default)))
  "Face of the padding between a text line and its annotation.")

(defcustom scholia-use-messages t
  "Whether scholia reports what it did in the echo area."
  :type 'boolean)

(defcustom scholia-annotation-column 85
  "Column where annotation text starts."
  :type 'natnum)

(defcustom scholia-search-region-lines-delta 2
  "How many lines around its stored position annotated text is searched.
Applies when a file changed while `scholia-mode' was off."
  :type 'natnum)


;;;; Errors

(define-error 'scholia-error "Scholia error")

(define-error 'scholia-db-format-error
              "Session file carries no scholia version tag"
              'scholia-error)


;;;; Buffer-local state

(defvar-local scholia--colors-index-counter 0
  "Always increasing index into the annotation face lists.
Addresses `scholia-highlight-faces' and `scholia-annotation-text-faces'.")

(defvar-local scholia--unplaced-annotations nil
  "Stored annotations this buffer could not be shown holding.
Nothing on screen stands for them, so every save folds them back into
the record as they are rather than writing a record without them.")


;;;; The mode

(defvar scholia-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-c C-a" #'scholia-annotate)
    (keymap-set map "C-c C-d" #'scholia-delete-annotation)
    (keymap-set map "C-c C-r" #'scholia-reply-to)
    (keymap-set map "C-c C-s" #'scholia-status)
    map)
  "Keymap of command `scholia-mode'.")

(define-minor-mode scholia-mode
  "Toggle Scholia mode.
Annotations are shown alongside the buffer text and stored in a session
database, leaving the file itself unchanged.

\\{scholia-mode-map}"
  :lighter " Sch"
  (require 'scholia-core)
  (if scholia-mode
      (scholia-initialize)
    (scholia-shutdown scholia-autosave)))


;;;; Annotations

(defun scholia-annotation-p (overlay)
  "Return non-nil when OVERLAY is an annotation.
An overlay is an annotation exactly when it still lives in a buffer and
carries annotation text in its `scholia-annotation' property."
  (and (overlayp overlay)
       (buffer-live-p (overlay-buffer overlay))
       (stringp (overlay-get overlay 'scholia-annotation))))

(defmacro scholia-ensure-annotation (spec &rest body)
  "Evaluate BODY only when SPEC's overlay is an annotation.
SPEC is a one-element list holding a form evaluating to an overlay,
which is evaluated exactly once and tested with `scholia-annotation-p'."
  (declare (indent 1) (debug ((form) body)))
  `(when (scholia-annotation-p ,(car spec))
     ,@body))

(provide 'scholia-vars)
;;; scholia-vars.el ends here
