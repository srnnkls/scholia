;;; scholia-render.el --- Note display  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/scholia
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A chain's note is a read-only `cera' pane shown beside the chain's
;; last line, from `scholia-annotation-column' on, with each reply a pane
;; of its own under it.
;; Nothing is written to the buffer, so an annotated file stays untouched
;; and undo never sees a note.

;;; Code:

(require 'seq)
(require 'cera)
(require 'scholia-vars)
(require 'scholia-color)
(require 'scholia-db)
(require 'scholia-edit)

(defvar-local scholia-render--shown nil
  "Alist of chain ids and the panes showing their notes.")


;;;; Finding a note

(defun scholia-render--notes ()
  "Return every note shown in the current buffer."
  (mapcar #'cdr scholia-render--shown))


;;;; Laying out the text

(defun scholia-render--author-pane (id author indent)
  "Return the pane ID naming AUTHOR, set in by INDENT, or nil for none.
It carries no face, so it reads as a byline under what was written
rather than as more of it."
  (when (and scholia-annotation-authors author)
    (cera-pane :id id :kind 'readonly :bracket nil :indent indent
               :text (concat (when-let* ((glyph (scholia-edit-glyph
                                                 scholia-author-icon
                                                 scholia-author-icon-fallback)))
                               (concat glyph " "))
                             author))))

(defun scholia-render--panes (chain)
  "Return the panes showing CHAIN's note and each reply under it.
A reply is set in by `scholia-render-reply-indent' columns per level,
and each of them is followed by a pane naming its author while
`scholia-annotation-authors' is on."
  (let* ((head (car chain))
         (chain-id (overlay-get head 'scholia--chain-id))
         (color (scholia-render-color (overlay-get head 'scholia--owner)
                                      chain-id))
         (revision (overlay-get head 'scholia-core--revision)))
    (delq nil
          (append
           (list (cera-pane :id chain-id :kind 'readonly :bracket nil
                            :face (scholia-color-note-face color 0)
                            :text (concat (overlay-get head 'scholia-annotation)
                                          (and revision (format " [%s]" revision))))
                 (scholia-render--author-pane (cons chain-id 'author)
                                              (get chain-id 'scholia-author) 0))
           (apply
            #'append
            (seq-map-indexed
             (pcase-lambda (`(,depth . ,reply) index)
               (let ((indent (* depth scholia-render-reply-indent)))
                 (list (cera-pane :id (cons chain-id index) :kind 'readonly
                                  :bracket nil :indent indent
                                  :face (scholia-color-note-face
                                         (scholia-render-color
                                          (overlay-get head 'scholia--owner)
                                          chain-id reply)
                                         depth)
                                  :text (scholia-db-annotation-text reply))
                       (scholia-render--author-pane
                        (list chain-id index 'author)
                        (scholia-db-annotation-author reply) indent))))
             (alist-get chain-id scholia--replies)))))))


;;;; Drawing

(defun scholia-render-chain (chain)
  "Show CHAIN's note beside the last line it covers, replacing any shown.
Return the shown note, or nil for an empty CHAIN."
  (when chain
    (let ((chain-id (overlay-get (car chain) 'scholia--chain-id)))
      (scholia-render-forget chain-id)
      (let ((shown (cera-pane-show (scholia-render--panes chain)
                                   (overlay-end (car (last chain)))
                                   scholia-annotation-column)))
        (push (cons chain-id shown) scholia-render--shown)
        shown))))

(defun scholia-render-note (chain)
  "Return the string drawn for CHAIN's note, or nil when nothing is."
  (when-let* ((shown (alist-get (overlay-get (car chain) 'scholia--chain-id)
                                scholia-render--shown))
              (overlay (car (cera-shown-overlays shown))))
    (or (overlay-get overlay 'before-string)
        (overlay-get overlay 'after-string))))

(defun scholia-render-forget (chain-id)
  "Remove the note shown for CHAIN-ID."
  (when-let* ((shown (alist-get chain-id scholia-render--shown)))
    (cera-pane-remove shown)
    (setq scholia-render--shown (assq-delete-all chain-id scholia-render--shown))))

(defun scholia-render-clear ()
  "Remove every note of the current buffer."
  (mapc #'cera-pane-remove (scholia-render--notes))
  (setq scholia-render--shown nil))

(defvar-local scholia-render--emphasized nil
  "Chain ids drawn as the annotations point is in.")

(defvar-local scholia-render-color-function nil
  "Function choosing the colour a note or reply is drawn in, or nil.
Called with the OWNER, CHAIN-ID and REPLY `scholia-render-color' is,
it returns a colour or nil for the session's own.  A buffer whose notes
are coloured by something other than the session they belong to, such
as their author, sets it.")

(defun scholia-render-color (owner chain-id &optional reply)
  "Return the colour a chain of OWNER named CHAIN-ID is drawn in.
REPLY is the annotation of a reply under the chain, drawn in its own
colour, or nil for the chain's note and the text it underlines.  The
colour is what `scholia-render-color-function' gives, or the session's
own, lit a step off the theme while point is in the annotation, so the
underline on the text and the note beside it move together."
  (let ((own (scholia-color-on-theme
              (or (and scholia-render-color-function
                       (funcall scholia-render-color-function
                                owner chain-id reply))
                  (scholia-color-for-index (scholia-session-color-index owner))))))
    (if (scholia-render-emphasized-p chain-id)
        (scholia-color-emphasis-color own)
      own)))

(defun scholia-render-emphasized-p (chain-id)
  "Return non-nil when CHAIN-ID's note is drawn as one point is in."
  (and (member chain-id scholia-render--emphasized) t))

(defun scholia-render-set-emphasis (chain-ids)
  "Draw the notes of CHAIN-IDS as the ones point is in.
Return the ids whose state changed, which are the notes a caller has to
draw again; point staying inside the same annotations changes nothing."
  (let ((changed (append (seq-difference chain-ids scholia-render--emphasized)
                         (seq-difference scholia-render--emphasized chain-ids))))
    (setq scholia-render--emphasized chain-ids)
    changed))

(defun scholia-render-emphasis-clear ()
  "Forget which notes were drawn as the ones point is in."
  (setq scholia-render--emphasized nil))

(provide 'scholia-render)
;;; scholia-render.el ends here
