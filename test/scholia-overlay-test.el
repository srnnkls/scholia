;;; scholia-overlay-test.el --- Tests for the scholia overlay and chain engine  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Covers `scholia-overlay', where one logical annotation is a chain of
;; overlays, one per line of the annotated region.

;;; Code:

(require 'ert)
(require 'seq)
(require 'scholia-test-helper)

(eval-and-compile
  (setq load-prefer-newer t)
  (require 'scholia-vars nil t)
  (require 'scholia-overlay nil t))

(defconst scholia-overlay-test--three-lines
  "alpha beta\ngamma delta\nepsilon zeta\n"
  "Three annotatable lines.
Line one holds positions 1-10 and its terminator 11, line two 12-22 and
its terminator 23, line three 24-35 and its terminator 36.")

(defun scholia-overlay-test--text (overlay)
  "Return the buffer text OVERLAY covers."
  (buffer-substring-no-properties (overlay-start overlay) (overlay-end overlay)))

(defun scholia-overlay-test--faces (chain)
  "Return the highlight face of every overlay in CHAIN."
  (mapcar (lambda (overlay) (overlay-get overlay 'face)) chain))


;;;; Chain creation over a multi-line region

(ert-deftest scholia-overlay-chains-one-overlay-per-line-with-single-endpoints ()
  (scholia-test-with-temp-file-buffer _buffer scholia-overlay-test--three-lines
    (scholia-mode 1)
    (let ((chain (scholia-create-chain 7 30 "check this")))
      (should (equal (length chain) 3))
      (scholia-test-should-overlay-range (nth 0 chain) 7 11)
      (scholia-test-should-overlay-range (nth 1 chain) 12 23)
      (scholia-test-should-overlay-range (nth 2 chain) 24 30)
      (should (equal (mapcar #'scholia-overlay-test--text chain)
                     '("beta" "gamma delta" "epsilo")))
      (should (seq-every-p #'scholia-annotation-p chain))
      (should (equal (mapcar (lambda (overlay)
                               (overlay-get overlay 'scholia-annotation))
                             chain)
                     (make-list 3 "check this")))
      (should (equal (seq-count #'scholia-chain-first-p chain) 1))
      (should (equal (seq-count #'scholia-chain-last-p chain) 1))
      (should (scholia-chain-first-p (nth 0 chain)))
      (should (scholia-chain-last-p (nth 2 chain)))
      (should (equal (scholia-chain-at 15) chain))
      (should (equal (scholia-chain-at 8) chain))
      (should (eq (scholia-annotation-at 24) (nth 2 chain)))
      (should-not (scholia-chain-at 5))
      (should-not (scholia-chain-at 11))
      (should-not (scholia-annotation-at 11))
      (should (equal (buffer-string) scholia-overlay-test--three-lines))
      (should-not (buffer-modified-p)))))


;;;; Chain deletion

(ert-deftest scholia-overlay-delete-chain-removes-every-overlay-and-nothing-else ()
  (scholia-test-with-temp-file-buffer _buffer scholia-overlay-test--three-lines
    (scholia-mode 1)
    (let ((bystander (scholia-create-chain 1 6 "left alone"))
          (doomed (scholia-create-chain 7 30 "goes away")))
      (should (equal (length doomed) 3))
      (scholia-delete-chain (scholia-annotation-at 15))
      (should-not (scholia-chain-at 8))
      (should-not (scholia-chain-at 15))
      (should-not (scholia-chain-at 25))
      (should (seq-every-p (lambda (overlay) (null (overlay-buffer overlay)))
                           doomed))
      (should (equal (scholia-chain-at 3) bystander))
      (scholia-test-should-overlay-range (car bystander) 1 6)
      (should (equal (buffer-string) scholia-overlay-test--three-lines))
      (should-not (buffer-modified-p)))))


;;;; Chain navigation

(ert-deftest scholia-overlay-navigation-crosses-chains-rather-than-rings ()
  (scholia-test-with-temp-file-buffer _buffer scholia-overlay-test--three-lines
    (scholia-mode 1)
    (let ((first (scholia-create-chain 1 6 "one"))
          (middle (scholia-create-chain 7 23 "two"))
          (last (scholia-create-chain 24 30 "three")))
      (should (equal (length middle) 2))
      (should (equal (scholia-next-annotation 8) last))
      (should (equal (scholia-next-annotation 13) last))
      (should (equal (scholia-previous-annotation 8) first))
      (should (equal (scholia-previous-annotation 13) first))
      (should (equal (scholia-next-annotation 3) middle))
      (should (equal (scholia-previous-annotation 25) middle))
      (should (equal (scholia-next-annotation 6) middle))
      (should (equal (scholia-previous-annotation 6) first))
      (should-not (scholia-previous-annotation 3))
      (should-not (scholia-next-annotation 25)))))


;;;; Face and priority assignment

(ert-deftest scholia-overlay-cycles-paired-faces-and-assigns-a-uniform-priority ()
  (scholia-test-with-temp-file-buffer _buffer scholia-overlay-test--three-lines
    (scholia-mode 1)
    (let* ((chains (mapcar (lambda (region)
                             (scholia-create-chain (car region) (cdr region) "note"))
                           '((1 . 6) (7 . 11) (12 . 18) (19 . 30))))
           (indices (mapcar (lambda (chain)
                              (seq-position scholia-highlight-faces
                                            (overlay-get (car chain) 'face)))
                            chains))
           (priorities (mapcar (lambda (overlay) (overlay-get overlay 'priority))
                               (apply #'append chains))))
      (should (equal (length (nth 3 chains)) 2))
      (should (seq-every-p #'integerp indices))
      (should (equal (cdr indices)
                     (mapcar (lambda (index)
                               (mod (1+ index) (length scholia-highlight-faces)))
                             (butlast indices))))
      (should (seq-every-p #'integerp priorities))
      (should (seq-every-p (lambda (priority) (> priority 0)) priorities))
      (dolist (chain chains)
        (should (equal (scholia-overlay-test--faces chain)
                       (make-list (length chain) (overlay-get (car chain) 'face))))
        (should (equal (mapcar (lambda (overlay) (overlay-get overlay 'priority))
                               chain)
                       (make-list (length chain)
                                  (overlay-get (car chain) 'priority)))))))
  (scholia-test-with-temp-file-buffer _buffer scholia-overlay-test--three-lines
    (scholia-mode 1)
    (let ((chain (scholia-create-chain 1 6 "restored" 1)))
      (should (equal (overlay-get (car chain) 'face)
                     (nth 1 scholia-highlight-faces))))))


;;;; A chain ending at point-max with no trailing newline

(ert-deftest scholia-overlay-renders-a-chain-ending-at-point-max-without-a-newline ()
  (scholia-test-with-temp-file-buffer _buffer "alpha beta"
    (scholia-mode 1)
    (should (equal (point-max) 11))
    (let ((chain (scholia-create-chain 7 11 "at the very end")))
      (should (equal (length chain) 1))
      (scholia-test-should-overlay-range (car chain) 7 11)
      (should (scholia-chain-first-p (car chain)))
      (should (scholia-chain-last-p (car chain)))
      (should (equal (buffer-string) "alpha beta"))
      (should-not (buffer-modified-p))
      (let* ((rendered (overlay-get (car chain) 'after-string))
             (start (and (stringp rendered)
                         (string-match (regexp-quote "at the very end") rendered)))
             (index (seq-position scholia-highlight-faces
                                  (overlay-get (car chain) 'face))))
        (should (stringp rendered))
        (should (integerp start))
        (should (integerp index))
        (should (equal (get-text-property start 'face rendered)
                       (nth index scholia-annotation-text-faces))))
      (goto-char (point-max))
      (insert "x")
      (should-not (overlay-get (car (scholia-chain-at 8)) 'after-string))))
  (scholia-test-with-temp-file-buffer _buffer "alpha beta\n"
    (scholia-mode 1)
    (should-not (overlay-get (car (scholia-create-chain 7 11 "the note"))
                             'after-string))
    (delete-region 11 12)
    (let ((rendered (overlay-get (car (scholia-chain-at 8)) 'after-string)))
      (should (stringp rendered))
      (should (string-match-p (regexp-quote "the note") rendered)))))


;;;; A line split by RET in the middle of the annotated range, then rejoined

(ert-deftest scholia-overlay-rechains-when-an-edit-splits-then-rejoins-the-line ()
  (scholia-test-with-temp-file-buffer _buffer "0123456789\nnext line\n"
    (scholia-mode 1)
    (let ((created (scholia-create-chain 3 9 "spans the split")))
      (should (equal (length created) 1))
      (should (equal (scholia-overlay-test--text (car created)) "234567")))
    (goto-char 6)
    (insert "\n")
    (let ((chain (scholia-chain-at 4)))
      (should (equal (length chain) 2))
      (scholia-test-should-overlay-range (nth 0 chain) 3 6)
      (scholia-test-should-overlay-range (nth 1 chain) 7 10)
      (should (equal (mapconcat #'scholia-overlay-test--text chain "") "234567"))
      (should (equal (seq-count #'scholia-chain-first-p chain) 1))
      (should (equal (seq-count #'scholia-chain-last-p chain) 1))
      (should (scholia-chain-first-p (nth 0 chain)))
      (should (scholia-chain-last-p (nth 1 chain)))
      (should (equal (scholia-chain-at 8) chain)))
    (delete-region 6 7)
    (should (equal (buffer-string) "0123456789\nnext line\n"))
    (let ((chain (scholia-chain-at 4)))
      (should (equal (length chain) 1))
      (scholia-test-should-overlay-range (car chain) 3 9)
      (should (equal (scholia-overlay-test--text (car chain)) "234567"))
      (should (equal (seq-count #'scholia-chain-first-p chain) 1))
      (should (equal (seq-count #'scholia-chain-last-p chain) 1))
      (should (equal (scholia-chain-at 8) chain))))
  (scholia-test-with-temp-file-buffer _buffer "aa\n\nbb\n"
    (scholia-mode 1)
    (should (equal (length (scholia-create-chain 1 7 "spans a blank line")) 2))
    (goto-char 4)
    (insert "x")
    (let ((chain (scholia-chain-at 2)))
      (should (equal (length chain) 3))
      (scholia-test-should-overlay-range (nth 0 chain) 1 3)
      (scholia-test-should-overlay-range (nth 1 chain) 4 5)
      (scholia-test-should-overlay-range (nth 2 chain) 6 8)
      (should (eq (scholia-annotation-at 4) (nth 1 chain))))))


;;;; Every chain operation reaching past the current restriction

(ert-deftest scholia-overlay-chain-operations-see-past-a-narrowing ()
  (scholia-test-with-temp-file-buffer _buffer scholia-overlay-test--three-lines
    (scholia-mode 1)
    (scholia-create-chain 7 30 "narrowed edit")
    (save-restriction
      (narrow-to-region 15 (point-max))
      (goto-char 20)
      (insert "x"))
    (let ((chain (scholia-chain-at 8)))
      (should (equal (length chain) 3))
      (scholia-test-should-overlay-range (nth 0 chain) 7 11)
      (scholia-test-should-overlay-range (nth 1 chain) 12 24)
      (scholia-test-should-overlay-range (nth 2 chain) 25 31)
      (should (equal (mapcar #'scholia-overlay-test--text chain)
                     '("beta" "gamma dexlta" "epsilo")))))
  (scholia-test-with-temp-file-buffer _buffer scholia-overlay-test--three-lines
    (scholia-mode 1)
    (let ((chain (scholia-create-chain 7 30 "narrowed lookup")))
      (save-restriction
        (narrow-to-region 12 23)
        (should-not (scholia-chain-first-p (nth 1 chain)))
        (should-not (scholia-chain-last-p (nth 1 chain)))
        (should (scholia-chain-first-p (nth 0 chain)))
        (should (scholia-chain-last-p (nth 2 chain))))
      (save-restriction
        (narrow-to-region 12 30)
        (goto-char 18)
        (insert "x"))
      (should-not (seq-some (lambda (overlay)
                              (overlay-get overlay 'after-string))
                            (scholia-chain-at 8)))
      (save-restriction
        (narrow-to-region 12 (point-max))
        (scholia-delete-chain (scholia-annotation-at 15)))
      (should-not (scholia-annotation-at 8))
      (should-not (seq-filter #'scholia-annotation-p
                              (overlays-in (point-min) (point-max))))))
  (scholia-test-with-temp-file-buffer _buffer scholia-overlay-test--three-lines
    (scholia-mode 1)
    (let ((chain (scholia-create-chain 7 30 "past the narrowing")))
      (save-restriction
        (narrow-to-region 12 23)
        (should (eq (scholia-annotation-at 25) (nth 2 chain)))
        (should (equal (scholia-chain-at 25) chain))
        (scholia-delete-chain (scholia-annotation-at 25)))
      (should-not (scholia-annotation-at 8))
      (should-not (seq-filter #'scholia-annotation-p
                              (overlays-in (point-min) (point-max)))))))


;;;; The annotated text deleted entirely

(ert-deftest scholia-overlay-deleting-the-annotated-text-removes-the-chain ()
  (scholia-test-with-temp-file-buffer _buffer "alpha beta\ngamma delta\n"
    (scholia-mode 1)
    (let ((chain (scholia-create-chain 7 17 "about to vanish")))
      (should (equal (length chain) 2))
      (delete-region 7 17)
      (should (equal (buffer-string) "alpha  delta\n"))
      (should-not (scholia-chain-at 7))
      (should-not (scholia-annotation-at 7))
      (should-not (seq-filter #'scholia-annotation-p
                              (overlays-in (point-min) (point-max))))
      (should (seq-every-p (lambda (overlay) (null (overlay-buffer overlay)))
                           chain)))))


;;;; Two annotations that touch

(ert-deftest scholia-overlay-touching-chains-do-not-absorb-each-other ()
  (scholia-test-with-temp-file-buffer _buffer "0123456789\n"
    (scholia-mode 1)
    (let ((left (scholia-create-chain 3 6 "first"))
          (right (scholia-create-chain 6 9 "second")))
      (should (equal (length left) 1))
      (should (equal (length right) 1))
      (scholia-test-should-overlay-range (car left) 3 6)
      (scholia-test-should-overlay-range (car right) 6 9)
      (should (eq (scholia-annotation-at 5) (car left)))
      (should (eq (scholia-annotation-at 6) (car right)))
      (should (equal (scholia-chain-at 4) left))
      (should (equal (scholia-chain-at 7) right))
      (should (equal (seq-count #'scholia-chain-last-p left) 1))
      (should (equal (seq-count #'scholia-chain-last-p right) 1))
      (should (scholia-chain-first-p (car left)))
      (should (scholia-chain-first-p (car right)))
      (should (equal (overlay-get (car left) 'scholia-annotation) "first"))
      (should (equal (overlay-get (car right) 'scholia-annotation) "second"))
      (scholia-delete-chain (car left))
      (should-not (scholia-chain-at 4))
      (scholia-test-should-overlay-range (car right) 6 9)
      (should (equal (scholia-chain-at 7) right)))))

(provide 'scholia-overlay-test)
;;; scholia-overlay-test.el ends here
