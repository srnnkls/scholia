;;; scholia-render.el --- Note display  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/scholia
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A chain's note is drawn by one overlay of its own, covering the line
;; terminator after the chain's last line and carrying the note in a
;; `before-string'.  Nothing is written to the buffer, so an annotated
;; file stays untouched and undo never sees a note.

;;; Code:

(require 'seq)
(require 'scholia-vars)
(require 'scholia-color)

(defconst scholia-render--priority 90
  "Priority given to every note overlay.")


;;;; Finding a note

(defun scholia-render--notes ()
  "Return every note overlay of the current buffer."
  (seq-filter (lambda (overlay) (overlay-get overlay 'scholia-render))
              (save-restriction
                (widen)
                (overlays-in (point-min) (point-max)))))

(defun scholia-render--note-of (chain-id)
  "Return the note overlay drawn for CHAIN-ID, or nil when none is."
  (seq-find (lambda (overlay)
              (eq (overlay-get overlay 'scholia--chain-id) chain-id))
            (scholia-render--notes)))


;;;; Laying out the text

(defun scholia-render--width ()
  "Return the columns a note line may take, or nil when it may take any."
  (when-let* ((window (get-buffer-window (current-buffer)))
              (available (- (window-body-width window)
                            scholia-annotation-column))
              ((> available 8)))
    available))

(defun scholia-render--wrap (text width)
  "Return TEXT as lines of at most WIDTH columns, or its own lines for a nil WIDTH.
A word wider than WIDTH takes a line of its own and overruns it rather
than being broken."
  (let ((paragraphs (split-string text "\n")))
    (if (null width)
        paragraphs
      (let ((lines nil))
        (dolist (paragraph paragraphs)
          (let ((current nil))
            (dolist (word (split-string paragraph "[[:space:]]+" t))
              (let ((joined (if current (concat current " " word) word)))
                (if (or (null current) (<= (string-width joined) width))
                    (setq current joined)
                  (push current lines)
                  (setq current word))))
            (push (or current "") lines)))
        (nreverse lines)))))

(defun scholia-render--reply-lines (chain-id color width)
  "Return the display lines of the replies stored under CHAIN-ID.
COLOR is the owning session's colour and WIDTH the room a line has.
Each line is a cons of its text and the face attributes it wears."
  (let ((lines nil))
    (pcase-dolist (`(,depth . ,text) (alist-get chain-id scholia--replies))
      (let ((indent (make-string (* depth scholia-render-reply-indent) ?\s))
            (face (scholia-color-note-face color depth)))
        (dolist (line (scholia-render--wrap
                       text
                       (and width (max 1 (- width (length indent))))))
          (push (cons (concat indent line) face) lines))))
    (nreverse lines)))

(defun scholia-render--lines (chain)
  "Return the display lines of CHAIN's note and the replies under it.
Each line is a cons of its text and the face attributes it wears."
  (let* ((head (car chain))
         (chain-id (overlay-get head 'scholia--chain-id))
         (color (scholia-render-color (overlay-get head 'scholia--owner)
                                      chain-id))
         (width (scholia-render--width))
         (revision (overlay-get head 'scholia-core--revision))
         (text (concat (overlay-get head 'scholia-annotation)
                       (and revision (format " [%s]" revision))))
         (face (scholia-color-note-face color 0)))
    (append (mapcar (lambda (line) (cons line face))
                    (scholia-render--wrap text width))
            (scholia-render--reply-lines chain-id color width))))

(defun scholia-render--string (lines column)
  "Return LINES as one display string starting at COLUMN.
The first line is set out to `scholia-annotation-column' from COLUMN and
every line after it from the start of its own line, so the note reads as
a block however long the text beside it is.

The run before the first note carries no face, so the line the note is
drawn beside shows through it: a note beside a magit diff line would
otherwise paint the frame's own background across the diff.  The runs
opening the lines below are faced, since display-only lines have nothing
behind them to show."
  (let ((opening (make-string (max 1 (- scholia-annotation-column column)) ?\s))
        (continuation (propertize
                       (concat "\n" (make-string scholia-annotation-column ?\s))
                       'face 'scholia-prefix))
        (rendered nil))
    (dolist (line lines)
      (setq rendered
            (concat rendered
                    (if rendered continuation opening)
                    (propertize (car line) 'face (cdr line)))))
    (or rendered "")))


;;;; Drawing

(defun scholia-render-chain (chain)
  "Draw CHAIN's note beside the last line it covers, replacing any drawn.
Return the note overlay, or nil for an empty CHAIN."
  (when chain
    (let ((chain-id (overlay-get (car chain) 'scholia--chain-id)))
      (when-let* ((drawn (scholia-render--note-of chain-id)))
        (delete-overlay drawn))
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (overlay-end (car (last chain))))
          (let* ((eol (line-end-position))
                 (column (progn (goto-char eol) (current-column)))
                 (ending (= eol (point-max)))
                 (overlay (make-overlay eol (if ending eol (1+ eol)) nil t nil))
                 (text (scholia-render--string
                        (scholia-render--lines chain) column)))
            (overlay-put overlay 'scholia-render t)
            (overlay-put overlay 'scholia--chain-id chain-id)
            (overlay-put overlay 'priority scholia-render--priority)
            (overlay-put overlay (if ending 'after-string 'before-string) text)
            overlay))))))

(defun scholia-render-note (chain)
  "Return the string drawn beside CHAIN, or nil when nothing is."
  (when-let* ((note (scholia-render--note-of
                     (overlay-get (car chain) 'scholia--chain-id))))
    (or (overlay-get note 'before-string)
        (overlay-get note 'after-string))))

(defun scholia-render-forget (chain-id)
  "Remove the note drawn for CHAIN-ID."
  (when-let* ((drawn (scholia-render--note-of chain-id)))
    (delete-overlay drawn)))

(defun scholia-render-clear ()
  "Remove every note of the current buffer."
  (mapc #'delete-overlay (scholia-render--notes)))

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
