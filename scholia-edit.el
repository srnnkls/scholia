;;; scholia-edit.el --- Inline annotation input  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/scholia
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Scholia reads a note through `cera', completing the project's files
;; after `@' and the note history everywhere else.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'subr-x)
(require 'scholia-vars)
(require 'scholia-color)
(require 'cera)

(declare-function scholia-session-name "scholia-core")
(declare-function nerd-icons-mdicon "nerd-icons")

;; Corfu and Evil, where they are installed, complete in and type into the
;; field.  Each integration is its own file, so it is loaded only when the
;; package it integrates is there.
(when (require 'corfu nil t) (require 'corfu-cera nil t))
(when (require 'evil nil t) (require 'evil-cera nil t))

(defcustom scholia-edit-icon "nf-md-comment_quote_outline"
  "Nerd Font glyph drawn where the field's bracket closes, or nil for none.
A scholium is a remark set against a passage, which is the quoted speech
bubble this names.  It is drawn in the session's own colour, so the field
wears the colour of the note before a word of it is typed."
  :type '(choice (const :tag "None" nil) string)
  :group 'scholia)

(defcustom scholia-edit-icon-fallback "❝"
  "Character drawn in front of the field where the Nerd Font is missing.
Nil draws nothing there.  A character the frame cannot display is not
drawn either, so a terminal without it is left with a bare bracket rather
than a box."
  :type '(choice (const :tag "None" nil) string)
  :group 'scholia)

(defcustom scholia-edit-icon-height 0.75
  "Height the field's glyph takes against the text around it."
  :type 'float
  :group 'scholia)

(defvar scholia-edit--file-cache nil
  "Cons of a directory and the completion table collected for it.
The project's files are collected once per directory rather than once per
keystroke, and the reader moves between directories rarely enough that one
entry is enough.")


;;;; Completion

(defun scholia-edit--file-bounds (begin end)
  "Return the filename mention bounds at point within BEGIN and END.
The leading @ must follow whitespace or an opening delimiter, or start
the field.  Keep it outside the completion bounds, and skip email addresses."
  (save-excursion
    (skip-chars-backward "^ \t\n\r@\"'`()[]{}<>" begin)
    (when (and (> (point) begin)
               (eq (char-before) ?@)
               (or (= (1- (point)) begin)
                   (memq (char-before (1- (point)))
                         '(?\s ?\t ?\n ?\r ?\( ?\[ ?\{))))
      (let ((start (point)))
        (skip-chars-forward "^ \t\n\r@\"'`()[]{}<>" end)
        (cons start (point))))))

(defun scholia-edit--collect-files (directory)
  "Return a completion table over the files of the project at DIRECTORY.
Project candidates are relative to their root; outside a project, use
ordinary filename completion relative to DIRECTORY."
  (let* ((project (project-current nil))
         (root (if project (project-root project) directory))
         (files (when project
                  (mapcar (lambda (file) (file-relative-name file root))
                          (project-files project)))))
    (lambda (string predicate action)
      (if project
          (if (eq action 'metadata)
              '(metadata (category . project-file))
            (complete-with-action action files string predicate))
        (let ((default-directory root))
          (completion-file-name-table string predicate action))))))

(defun scholia-edit--file-table ()
  "Return the file completion table, collecting the project's files once."
  (let ((directory default-directory))
    (unless (equal (car scholia-edit--file-cache) directory)
      (setq scholia-edit--file-cache
            (cons directory (scholia-edit--collect-files directory))))
    (cdr scholia-edit--file-cache)))

(defun scholia-edit--completion (bounds table)
  "Complete a filename after `@' and TABLE anywhere else in BOUNDS."
  (if-let* ((file (scholia-edit--file-bounds (car bounds) (cdr bounds))))
      (list (car file) (cdr file) (scholia-edit--file-table)
            :exclusive 'no :company-prefix-length t)
    (list (car bounds) (cdr bounds) table
          :exclusive 'no :company-prefix-length 0)))

(defun scholia-edit--nerd-font-p ()
  "Return non-nil when this frame can draw the Nerd Font glyphs."
  (when-let* (((require 'nerd-icons nil t))
              (family (bound-and-true-p nerd-icons-font-family)))
    (find-font (font-spec :family family))))

(defun scholia-edit--centred (glyph)
  "Return GLYPH raised to sit centred on the line rather than on its baseline.
A glyph drawn at `scholia-edit-icon-height' of the text around it gives
up the rest of the line's height above itself; raising it by half of
that, in its own height, is what centres it."
  (let ((height scholia-edit-icon-height))
    (if (< 0 height 1)
        (propertize glyph 'display `(raise ,(/ (- 1.0 height) (* 2 height))))
      glyph)))

(defun scholia-edit-glyph (name fallback &optional color)
  "Return the Nerd Font glyph NAME, or FALLBACK where the font is missing.
The glyph is drawn at `scholia-edit-icon-height' in COLOR and centred on
the line, and nil comes back for a nil NAME or a FALLBACK the frame
cannot display."
  (let ((face (and color (list :foreground color))))
    (when name
      (if (scholia-edit--nerd-font-p)
          (scholia-edit--centred
           (nerd-icons-mdicon name :face face :height scholia-edit-icon-height))
        (when (and fallback (char-displayable-p (string-to-char fallback)))
          (propertize fallback 'face face))))))

(defun scholia-edit--icon (color)
  "Return the glyph drawn in front of the field in COLOR, or nil for none.
The Nerd Font glyph where that font is installed and a plain quotation
mark where it is not, so a frame without the font marks the field rather
than drawing a box for a character it has no glyph for."
  (scholia-edit-glyph scholia-edit-icon scholia-edit-icon-fallback color))

(defun scholia-edit-read (table &optional initial bounds)
  "Read annotation text inline using completion TABLE and INITIAL input.
BOUNDS is the source's (BEGIN . END) range, defaulting to the active
region.  The source is underlined in its session's colour, which is the
colour the note will wear, and its first line is connected to the field
below its last line; without a range the field goes below the current
document line.
\\<cera-mode-map>\\[cera-accept] keeps the note, and
\\[cera-cancel] discards it.  Use C-SPC to request annotation history and
type @ to complete a filename.  Both are offered through the field's
completion, so an active completion menu keeps its own RET.

Temporary text is rolled back before returning.  Point, narrowing, undo,
modification status, hooks and borrowed local settings are restored on
accept, quit and error.  Cancelling signals `quit', like a minibuffer."
  (let* ((color (scholia-color-on-theme
                 (scholia-color-for-index
                  (scholia-session-color-index (scholia-session-name)))))
         (cera-completion-function #'scholia-edit--completion)
         (cera-input-prefix (scholia-edit--icon color)))
    (cera-read table initial bounds (scholia-color-highlight-face color))))

(provide 'scholia-edit)
;;; scholia-edit.el ends here
