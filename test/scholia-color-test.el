;;; scholia-color-test.el --- Tests for scholia session colours  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Two sessions must never be drawn in one colour, however many of them a
;; buffer shows and however short `scholia-session-colors' is.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'scholia-color)

(defun scholia-color-test--range (count)
  "Return the colours of the first COUNT session indices."
  (mapcar #'scholia-color-for-index (number-sequence 0 (1- count))))

(ert-deftest scholia-color-gives-every-session-index-a-colour-of-its-own ()
  (dolist (colors (list (default-value 'scholia-session-colors)
                        '("#EEF192")
                        nil))
    (let* ((scholia-session-colors colors)
           (drawn (scholia-color-test--range 64)))
      (should (= (length drawn) 64))
      (should (seq-every-p (lambda (color)
                             (string-match-p "\\`#[[:xdigit:]]\\{6\\}\\'" color))
                           drawn))
      (should (equal drawn (delete-dups (copy-sequence drawn)))))))

(ert-deftest scholia-color-keeps-the-session-colours-far-enough-apart-to-see ()
  (dolist (count '(6 12 24))
    (let* ((hues (sort (mapcar (lambda (color) (car (scholia-color--hsl color)))
                               (scholia-color-test--range count))
                       #'<))
           (gaps (cons (- 1.0 (- (car (last hues)) (car hues)))
                       (cl-loop for (one other) on hues while other
                                collect (- other one)))))
      (should (> (apply #'min gaps) (/ 0.5 count))))))

(ert-deftest scholia-color-takes-the-configured-colours-in-order-first ()
  (let ((scholia-session-colors '("#EEF192" "#92EEF1" "#F192EE")))
    (should (equal (scholia-color-test--range 3) scholia-session-colors))
    (should-not (member (scholia-color-for-index 3) scholia-session-colors))
    (should-not (member (downcase (scholia-color-for-index 3))
                        (mapcar #'downcase scholia-session-colors)))))

(ert-deftest scholia-color-keeps-a-generated-colour-in-the-configured-family ()
  (let* ((scholia-session-colors '("#EEF192"))
         (template (scholia-color--hsl "#EEF192"))
         (generated (scholia-color--hsl (scholia-color-for-index 4))))
    (should (< (abs (- (nth 1 generated) (nth 1 template))) 0.01))
    (should (< (abs (- (nth 2 generated) (nth 2 template))) 0.01))
    (should (> (abs (- (nth 0 generated) (nth 0 template))) 0.01))))

(ert-deftest scholia-color-reads-a-hexadecimal-colour-without-the-display ()
  (should (equal (scholia-color--rgb "#ff8000") '(1.0 0.5019607843137255 0.0)))
  (should (equal (scholia-color--rgb "#f80") '(1.0 0.5333333333333333 0.0)))
  (should-not (scholia-color--rgb "#12345"))
  (should (equal (scholia-color-foreground "#EEF192") "black"))
  (should (equal (scholia-color-foreground "#101010") "white")))

(ert-deftest scholia-color-fades-a-reply-once-per-level-of-depth ()
  (let* ((base "#EEF192")
         (tints (mapcar (lambda (depth) (scholia-color-tint base depth))
                        '(0 1 2 3))))
    (should (equal (car tints) base))
    (should (equal tints (delete-dups (copy-sequence tints))))
    (should (apply #'> (mapcar (lambda (color) (nth 1 (scholia-color--hsl color)))
                               tints)))
    (should (equal (scholia-color-tint "not a colour" 2) "not a colour"))))

(ert-deftest scholia-color-emphasis-deepens-far-enough-to-see-on-either-theme ()
  "The emphasis colour deepens and enriches, by the same step on either theme.
A session colour is pale on both, so up is the direction without room in
it: a step towards white washes the note out.  The step has to be large
enough to see, which a colour already near white would not manage by
lifting."
  (let* ((colour (scholia-color-for-index 0))
         (scholia-emphasis-shade 20)
         (lightness (lambda (hex) (nth 2 (scholia-color--hsl hex))))
         (saturation (lambda (hex) (nth 1 (scholia-color--hsl hex))))
         (on (lambda (background)
               (cl-letf (((symbol-function 'face-background)
                          (lambda (&rest _) background)))
                 (scholia-color--moved-away colour))))
         (own (funcall lightness colour))
         (light (funcall on "#fafafa"))
         (dark (funcall on "#202020")))
    (should (equal light dark))
    (should (< (funcall lightness light) (- own 0.08)))
    (should (> (funcall lightness light) 0.35))
    (should (> (funcall saturation light) (funcall saturation colour)))
    (should (<= (funcall saturation light) 1.0))))

(ert-deftest scholia-color-deepens-the-palette-on-a-dark-theme ()
  "A session colour is deepened on a dark theme and left alone on a light one.
Deepening leaves the room the colour needs to lift while point is in the
annotation, which a pale colour on a dark theme does not have."
  (let* ((colour (scholia-color-for-index 0))
         (scholia-dark-theme-shade 12)
         (scholia-emphasis-shade 20)
         (lightness (lambda (hex) (nth 2 (scholia-color--hsl hex))))
         (on (lambda (background function)
               (cl-letf (((symbol-function 'face-background)
                          (lambda (&rest _) background)))
                 (funcall function colour))))
         (own (funcall lightness colour))
         (light (funcall on "#fafafa" #'scholia-color-on-theme))
         (dark (funcall on "#202020" #'scholia-color-on-theme)))
    (should (equal light colour))
    (should (< (funcall lightness dark) own))
    (should (> (funcall lightness dark) 0.2))
    (let ((lit (cl-letf (((symbol-function 'face-background)
                          (lambda (&rest _) "#202020")))
                 (scholia-color-emphasis-color dark))))
      (should (< (funcall lightness lit) (- (funcall lightness dark) 0.08)))
      (should (> (funcall lightness lit) 0.35)))))

(provide 'scholia-color-test)
;;; scholia-color-test.el ends here
