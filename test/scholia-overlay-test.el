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
  (require 'scholia-overlay nil t)
  (require 'scholia-render nil t))

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

(ert-deftest scholia-overlay-draws-every-chain-of-a-session-in-one-colour ()
  (scholia-test-with-temp-file-buffer _buffer scholia-overlay-test--three-lines
    (scholia-mode 1)
    (let* ((chains (mapcar (lambda (region)
                             (scholia-create-chain (car region) (cdr region) "note"))
                           '((1 . 6) (7 . 11) (12 . 18) (19 . 30))))
           (expected (scholia-color-highlight-face (scholia-color-for-index 0)))
           (priorities (mapcar (lambda (overlay) (overlay-get overlay 'priority))
                               (apply #'append chains))))
      (should (equal (length (nth 3 chains)) 2))
      (should (seq-every-p #'integerp priorities))
      (should (seq-every-p (lambda (priority) (> priority 0)) priorities))
      (dolist (chain chains)
        (should (equal (overlay-get (car chain) 'face) expected))
        (should (equal (scholia-overlay-test--faces chain)
                       (make-list (length chain) expected)))
        (should (equal (mapcar (lambda (overlay) (overlay-get overlay 'priority))
                               chain)
                       (make-list (length chain)
                                  (overlay-get (car chain) 'priority)))))))
  (scholia-test-with-temp-file-buffer _buffer scholia-overlay-test--three-lines
    (scholia-mode 1)
    (let ((chain (scholia-create-chain 1 6 "restored" 1)))
      (should (equal (scholia-chain-color-index chain) 1))
      (should (equal (overlay-get (car chain) 'face)
                     (scholia-color-highlight-face
                      (scholia-color-for-index 0))))))
  (scholia-test-with-temp-file-buffer _buffer scholia-overlay-test--three-lines
    (scholia-mode 1)
    (should-error (scholia-create-chain 1 6 "malformed" "not an index")
                  :type 'wrong-type-argument)))

(ert-deftest scholia-overlay-never-draws-two-sessions-in-one-colour ()
  (scholia-test-with-temp-file-buffer _buffer scholia-overlay-test--three-lines
    (scholia-mode 1)
    (let ((owners '("one" "two" "three" "four")))
      (setq scholia--session-state
            (seq-map-indexed (lambda (owner index)
                               (list owner :color-offset index))
                             owners))
      (let ((faces (seq-map-indexed
                    (lambda (owner index)
                      (overlay-get
                       (car (scholia-create-chain (+ 1 index) (+ 2 index)
                                                  "note" 0 owner))
                       'face))
                    owners)))
        (should (equal (length (delete-dups (copy-sequence faces)))
                       (length owners)))))))

(ert-deftest scholia-overlay-numbers-a-session-no-state-names-past-the-rest ()
  "A session this buffer holds no state for still gets a colour of its own."
  (scholia-test-with-temp-file-buffer _buffer scholia-overlay-test--three-lines
    (scholia-mode 1)
    (setq scholia--session-state nil)
    (let ((faces (mapcar (pcase-lambda (`(,beg ,end ,owner))
                           (overlay-get
                            (car (scholia-create-chain beg end "note" 0 owner))
                            'face))
                         '((1 6 "ghost-a")
                           (7 11 "ghost-b")
                           (12 18 "ghost-c")))))
      (should (equal (length (delete-dups (copy-sequence faces))) 3)))))

(ert-deftest scholia-overlay-the-run-before-a-note-leaves-the-line-visible ()
  "The run between a line and the note beside it carries no face.
That run is drawn as part of the note's string rather than as buffer
text, so a face there paints over whatever the line itself wears — a
note beside a magit diff line would lay the frame's background across
the diff.  The runs opening the note's own lower lines keep their face,
since display-only lines have nothing behind them."
  (let* ((lines (list (cons "one" '(:background "red"))
                      (cons "two" '(:background "red"))))
         (rendered (scholia-render--string lines 4))
         (note-start (- scholia-annotation-column 4)))
    (should-not (get-text-property 0 'face rendered))
    (should (equal '(:background "red")
                   (get-text-property note-start 'face rendered)))
    (should (eq 'scholia-prefix
                (get-text-property (+ note-start 3) 'face rendered)))))

(ert-deftest scholia-overlay-tints-a-reply-below-the-note-it-answers ()
  (scholia-test-with-temp-file-buffer _buffer scholia-overlay-test--three-lines
    (scholia-mode 1)
    (let* ((chain (scholia-create-chain 1 6 "the note"))
           (key (overlay-get (car chain) 'scholia--chain-id))
           (color (scholia-color-for-index 0)))
      (setf (alist-get key scholia--replies) '((1 . "the answer")))
      (scholia-render-chain chain)
      (let ((rendered (scholia-render-note chain)))
        (should (string-match-p (regexp-quote "the note") rendered))
        (let ((start (string-match (regexp-quote "the answer") rendered)))
          (should (integerp start))
          (should (equal (get-text-property start 'face rendered)
                         (scholia-color-note-face color 1)))
          (should-not (equal (scholia-color-note-face color 1)
                             (scholia-color-note-face color 0))))))))


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
      (let* ((rendered (scholia-render-note chain))
             (start (and (stringp rendered)
                         (string-match (regexp-quote "at the very end") rendered))))
        (should (stringp rendered))
        (should (integerp start))
        (should (equal (get-text-property start 'face rendered)
                       (scholia-color-note-face (scholia-color-for-index 0) 0))))
      (goto-char (point-max))
      (insert "x")
      (should (string-match-p (regexp-quote "at the very end")
                              (scholia-render-note (scholia-chain-at 8))))))
  (scholia-test-with-temp-file-buffer _buffer "alpha beta\n"
    (scholia-mode 1)
    (should (string-match-p (regexp-quote "the note")
                            (scholia-render-note
                             (scholia-create-chain 7 11 "the note"))))
    (delete-region 11 12)
    (let ((rendered (scholia-render-note (scholia-chain-at 8))))
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
      (should (string-match-p (regexp-quote "narrowed lookup")
                              (scholia-render-note (scholia-chain-at 8))))
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
                           chain))
      (should-not (scholia-render--notes)))))

(ert-deftest scholia-overlay-deleting-a-chain-takes-its-note-with-it ()
  (scholia-test-with-temp-file-buffer _buffer "alpha beta\ngamma delta\n"
    (scholia-mode 1)
    (let ((kept (scholia-create-chain 1 6 "kept"))
          (going (scholia-create-chain 7 11 "going")))
      (should (equal (length (scholia-render--notes)) 2))
      (scholia-delete-chain (car going))
      (should (equal (length (scholia-render--notes)) 1))
      (should (string-match-p (regexp-quote "kept")
                              (scholia-render-note kept))))))


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

(ert-deftest scholia-overlay-lights-every-annotation-point-is-in ()
  "Point in overlapping annotations lights both, and neither once away.
The note beside the text and the underline under it move together, so an
annotation reads as one thing whichever half of it is looked at."
  (scholia-test-with-temp-file-buffer _buffer scholia-overlay-test--three-lines
    (scholia-mode 1)
    (should (memq 'scholia-core--emphasize-at-point post-command-hook))
    (cl-letf (((symbol-function 'scholia-color--moved-away)
               (lambda (_color) "#123456")))
      (let* ((outer (scholia-create-chain 1 6 "the whole word"))
             (inner (scholia-create-chain 3 6 "the tail of it"))
             (underline (overlay-get (car outer) 'face))
             (note-face (lambda (chain)
                          (let* ((note (scholia-render-note chain))
                                 (at (string-match (regexp-quote "the whole") note)))
                            (and at (get-text-property at 'face note)))))
             (resting (funcall note-face outer)))
        (should (equal (plist-get resting :background) (scholia-color-for-index 0)))
        (goto-char 4)
        (scholia-core--emphasize-at-point)
        (should (= 2 (length scholia-render--emphasized)))
        (should (scholia-render-emphasized-p
                 (overlay-get (car inner) 'scholia--chain-id)))
        (should (equal (plist-get (funcall note-face outer) :background) "#123456"))
        (should (equal (overlay-get (car outer) 'face) '(:underline "#123456")))
        (goto-char 12)
        (scholia-core--emphasize-at-point)
        (should-not scholia-render--emphasized)
        (should (equal (funcall note-face outer) resting))
        (should (equal (overlay-get (car outer) 'face) underline))))))

(provide 'scholia-overlay-test)
;;; scholia-overlay-test.el ends here
