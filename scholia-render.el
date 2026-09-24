;;; scholia-render.el --- Note display  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/scholia
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A chain's note is a read-only `cera' pane shown under the chain's last
;; line, bracketed and marked the way the field it was written in is.
;; Nothing is written to the buffer, so an annotated file stays untouched
;; and undo never sees a note.

;;; Code:

(require 'seq)
(require 'cera)
(require 'scholia-vars)
(require 'scholia-color)
(require 'scholia-edit)

(defvar-local scholia-render--shown nil
  "Alist of chain ids and the panes showing their notes.")


;;;; Finding a note

(defun scholia-render--notes ()
  "Return every note shown in the current buffer."
  (mapcar #'cdr scholia-render--shown))


;;;; Laying out the text

(defun scholia-render--reply-text (chain-id color)
  "Return the replies stored under CHAIN-ID as the text drawn below the note.
COLOR is the owning session's colour.  Each reply is set in by its depth
and wears that depth's tint of COLOR."
  (mapconcat
   (pcase-lambda (`(,depth . ,text))
     (propertize (concat (make-string (* depth scholia-render-reply-indent) ?\s)
                         text)
                 'face (scholia-color-note-face color depth)))
   (alist-get chain-id scholia--replies)
   "\n"))

(defun scholia-render--pane (chain)
  "Return the pane showing CHAIN's note and the replies under it."
  (let* ((head (car chain))
         (chain-id (overlay-get head 'scholia--chain-id))
         (color (scholia-render-color (overlay-get head 'scholia--owner)
                                      chain-id))
         (revision (overlay-get head 'scholia-core--revision))
         (note (propertize (concat (overlay-get head 'scholia-annotation)
                                   (and revision (format " [%s]" revision)))
                           'face (scholia-color-note-face color 0)))
         (replies (scholia-render--reply-text chain-id color)))
    (cera-pane :id chain-id :kind 'readonly
               :text (if (string-empty-p replies) note (concat note "\n" replies))
               :prefix (scholia-edit--icon color)
               :prefix-position 'top)))


;;;; Drawing

(defun scholia-render-chain (chain)
  "Show CHAIN's note under the last line it covers, replacing any shown.
Return the shown note, or nil for an empty CHAIN."
  (when chain
    (let ((chain-id (overlay-get (car chain) 'scholia--chain-id)))
      (scholia-render-forget chain-id)
      (let ((shown (cera-pane-show (list (scholia-render--pane chain))
                                   (overlay-end (car (last chain))))))
        (push (cons chain-id shown) scholia-render--shown)
        shown))))

(defun scholia-render-note (chain)
  "Return the string drawn for CHAIN's note, or nil when nothing is."
  (when-let* ((shown (alist-get (overlay-get (car chain) 'scholia--chain-id)
                                scholia-render--shown))
              (overlay (car (cera-shown-overlays shown))))
    (overlay-get overlay 'before-string)))

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

(defun scholia-render-color (owner chain-id)
  "Return the colour a chain of OWNER named CHAIN-ID is drawn in.
The session's own colour, lit a step off the theme while point is in the
annotation, so the underline on the text and the note beside it move
together."
  (let ((own (scholia-color-on-theme
              (scholia-color-for-index (scholia-session-color-index owner)))))
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
