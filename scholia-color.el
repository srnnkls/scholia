;;; scholia-color.el --- Session colours  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/scholia
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A session owns one colour.  Every annotation it holds wears that hue,
;; and a reply wears the same hue at a lower saturation, one step per
;; depth.

;;; Code:

(require 'color)
(require 'scholia-vars)

(defconst scholia-color--fallback "#EEF192"
  "Colour taken when `scholia-session-colors' is empty.")

(defconst scholia-color--hue-turn 0.6180339887498949
  "Fraction of the hue wheel each colour past the configured ones turns.
An irrational turn lands on a hue no earlier one took and spreads any
number of them as widely as that number allows.")

(defun scholia-color--rgb (color)
  "Return COLOR as red, green and blue between 0 and 1, or nil for none.
A hexadecimal spelling is read as it is written rather than through the
display, which on a terminal answers the nearest colour it can show and
would give two hues of the palette the same tints."
  (if (and (stringp color)
           (string-match "\\`#\\([[:xdigit:]]+\\)\\'" color))
      (let* ((digits (match-string 1 color))
             (width (/ (length digits) 3)))
        (when (and (> width 0) (= (* width 3) (length digits)))
          (let ((maximum (float (1- (expt 16 width)))))
            (mapcar (lambda (index)
                      (/ (string-to-number
                          (substring digits (* index width) (* (1+ index) width))
                          16)
                         maximum))
                    '(0 1 2)))))
    (and (stringp color) (color-name-to-rgb color))))

(defun scholia-color--hsl (color)
  "Return COLOR as a hue, saturation and lightness list, or nil for none."
  (when-let* ((rgb (scholia-color--rgb color)))
    (apply #'color-rgb-to-hsl rgb)))

(defun scholia-color-tint (color depth)
  "Desaturate COLOR by DEPTH applications of `scholia-reply-tint-step'.
Depth 0 answers COLOR itself, and a colour that cannot be read answers
unchanged rather than becoming one that can."
  (if (<= depth 0)
      color
    (if-let* ((hsl (scholia-color--hsl color)))
        (pcase-let* ((`(,hue ,saturation ,lightness) hsl)
                     (factor (expt (max 0.0 (- 1.0 scholia-reply-tint-step))
                                   depth))
                     (`(,red ,green ,blue)
                      (color-hsl-to-rgb
                       hue
                       (* saturation factor)
                       (+ lightness (* (- 1.0 lightness) 0.4 (- 1.0 factor))))))
          (color-rgb-to-hex red green blue 2))
      color)))

(defconst scholia-color--turn-limit 4096
  "Turns of the wheel taken before a hue is accepted however near it lands.")

(defun scholia-color--hue-distance (one other)
  "Return how far apart hues ONE and OTHER lie on the wheel."
  (let ((raw (abs (- one other))))
    (min raw (- 1.0 raw))))

(defun scholia-color--turned (offset)
  "Return the colour OFFSET colours past the configured ones.
The hue turns until it lands far enough from every hue already taken to
be told apart from it, and keeps the saturation and lightness of the
first configured colour, so it reads as one of them rather than as an
intruder.  The room demanded shrinks as the hues taken multiply, since
past some number of them no hue is far from all the rest."
  (pcase-let* ((`(,hue ,saturation ,lightness)
                (or (scholia-color--hsl (car scholia-session-colors))
                    (scholia-color--hsl scholia-color--fallback)))
               (taken (or (delq nil (mapcar (lambda (color)
                                              (car (scholia-color--hsl color)))
                                            scholia-session-colors))
                          (list hue)))
               (candidate hue)
               (accepted 0)
               (turns 0))
    (while (<= accepted offset)
      (let ((separation (/ 0.5 (1+ (length taken)))))
        (setq candidate (mod (+ candidate scholia-color--hue-turn) 1.0)
              turns (1+ turns))
        (when (or (> turns scholia-color--turn-limit)
                  (not (seq-some
                        (lambda (other)
                          (< (scholia-color--hue-distance candidate other)
                             separation))
                        taken)))
          (push candidate taken)
          (setq accepted (1+ accepted)
                turns 0))))
    (pcase-let ((`(,red ,green ,blue)
                 (color-hsl-to-rgb candidate saturation lightness)))
      (color-rgb-to-hex red green blue 2))))

(defun scholia-color-for-index (index)
  "Return the colour of the session at INDEX, which no other index wears.
The colours of `scholia-session-colors' answer in order, and an INDEX
past them turns the hue wheel on rather than starting the list again."
  (let ((listed scholia-session-colors))
    (cond
     ((null listed) (scholia-color--turned index))
     ((< index (length listed)) (nth index listed))
     (t (scholia-color--turned (- index (length listed)))))))

(defun scholia-color-foreground (color)
  "Return the foreground legible on COLOR."
  (if-let* ((hsl (scholia-color--hsl color)))
      (if (> (nth 2 hsl) 0.5) "black" "white")
    "black"))

(defun scholia-color-highlight-face (color)
  "Return the face attributes marking text annotated in COLOR.
The annotated text is underlined, leaving the text itself readable; the
note drawn beside it carries the colour as its background."
  (list :underline color))

(defun scholia-color-note-face (color depth)
  "Return the face attributes of note text in COLOR at reply DEPTH."
  (let ((tinted (scholia-color-tint color depth)))
    (list :background tinted
          :foreground (scholia-color-foreground tinted))))

(defcustom scholia-dark-theme-shade 12
  "Percent of its lightness a session colour loses on a dark theme.
The session colours are chosen pale, which reads on a light theme and
glares on a dark one.  Deepening them there also leaves the room the
colour needs to lift by `scholia-emphasis-shade' while point is in the
annotation.  The step is small: a note is a block of colour carrying dark
text, so it is read the way a theme's own keyword colours are, and those
sit high."
  :type 'natnum
  :group 'scholia)

(defcustom scholia-emphasis-shade 30
  "Percent the annotation under point moves away from its own colour.
Deeper and richer, on either theme, so the annotation point is in reads
as the same hue with the light turned up rather than as a different one."
  :type 'natnum
  :group 'scholia)

(defun scholia-color-on-theme (color)
  "Return COLOR as a dark theme wears it, or COLOR itself on a light one.
A colour or a theme that cannot be read answers COLOR, so a palette
without a display behind it is drawn as it is written."
  (if-let* ((hsl (scholia-color--hsl color))
            (theme (scholia-color--hsl (face-background 'default nil t)))
            ((<= (nth 2 theme) 0.5)))
      (pcase-let* ((`(,hue ,saturation ,lightness) hsl)
                   (`(,red ,green ,blue)
                    (color-hsl-to-rgb
                     hue saturation
                     (* lightness (- 1.0 (/ scholia-dark-theme-shade 100.0))))))
        (color-rgb-to-hex red green blue 2))
    color))

(defun scholia-color--moved-away (color)
  "Return COLOR deepened by `scholia-emphasis-shade' percent.
A session colour is pale on either theme, so down is the only direction
with room in it: a step towards white lands on white and leaves nothing
to read.  The lightness gives up that part of itself and the saturation
takes the same part of the room it has left, which keeps the hue and
makes it richer rather than merely darker.  Both steps are proportional,
so a colour near an extreme moves less than one in the middle.  Nil when
the colour cannot be read."
  (when-let* ((hsl (scholia-color--hsl color)))
    (pcase-let* ((`(,hue ,saturation ,lightness) hsl)
                 (step (/ scholia-emphasis-shade 100.0))
                 (`(,red ,green ,blue)
                  (color-hsl-to-rgb hue
                                    (+ saturation (* step (- 1.0 saturation)))
                                    (- lightness (* step lightness)))))
      (color-rgb-to-hex red green blue 2))))

(defun scholia-color-emphasis-color (color)
  "Return COLOR as the note of the annotation point is in wears it.
The hue is kept and the colour deepens by `scholia-emphasis-shade'
percent, so the annotation point is in reads as the same one with the
light turned up rather than as another.  A colour that cannot be read
answers COLOR, leaving the note as it was drawn."
  (or (scholia-color--moved-away color) color))

(provide 'scholia-color)
;;; scholia-color.el ends here
