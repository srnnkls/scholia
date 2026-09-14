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
  "Return the face attributes marking text annotated in COLOR."
  (list :underline color))

(defun scholia-color-note-face (color depth)
  "Return the face attributes of note text in COLOR at reply DEPTH."
  (let ((tinted (scholia-color-tint color depth)))
    (list :background tinted
          :foreground (scholia-color-foreground tinted))))

(provide 'scholia-color)
;;; scholia-color.el ends here
