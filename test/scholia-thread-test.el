;;; scholia-thread-test.el --- Tests for the scholia reply thread  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; A reply whose parent is outside the candidate set heads a thread of its
;; own rather than vanishing, and a `:reply-to' cycle an imported session
;; carries must terminate the walk.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'scholia-test-helper)

(let ((load-prefer-newer t))
  (require 'scholia-vars nil t)
  (require 'scholia-db nil t)
  (require 'scholia-thread nil t))

(defun scholia-thread-test--annotation (id text)
  "Return a root annotation with ID carrying the note TEXT."
  (list :id id
        :beg 10 :end 20
        :text text
        :annotated-text "alpha"
        :line 1 :line-text "alpha beta" :column 0 :end-column 10
        :color 0
        :position :margin
        :reply-to nil
        :sends nil))

(defun scholia-thread-test--reply (id text parent-id)
  "Return a reply with ID carrying TEXT and answering PARENT-ID."
  (list :id id
        :beg nil :end nil
        :text text
        :annotated-text nil
        :line nil :line-text nil :column nil :end-column nil
        :color 0
        :position :margin
        :reply-to parent-id
        :sends nil))

(defun scholia-thread-test--thread ()
  "Return a candidate set nesting a reply to a reply.
The root carries two replies and the first of them carries one, so a
depth-first order and a breadth-first one differ."
  (list (scholia-thread-test--annotation "id-a" "note on alpha")
        (scholia-thread-test--reply "id-b" "first reply" "id-a")
        (scholia-thread-test--reply "id-c" "second reply" "id-a")
        (scholia-thread-test--reply "id-d" "nested reply" "id-b")))

(defun scholia-thread-test--orphaned-thread ()
  "Return a candidate set whose second thread lost its parent.
The orphan carries a reply of its own, so re-rooting it must keep its
subtree rather than only the orphan itself."
  (list (scholia-thread-test--annotation "id-a" "note on alpha")
        (scholia-thread-test--reply "id-b" "first reply" "id-a")
        (scholia-thread-test--reply "id-z" "orphaned note" "id-gone")
        (scholia-thread-test--reply "id-y" "under the orphan" "id-z")))

(defun scholia-thread-test--ids (annotations)
  "Return the ids of ANNOTATIONS in order."
  (mapcar #'scholia-db-annotation-id annotations))

(defun scholia-thread-test--walk-ids-and-depths (annotations)
  "Return the id and depth of every annotation the walk over ANNOTATIONS visits."
  (scholia-thread-walk annotations
                       (lambda (annotation depth)
                         (cons (scholia-db-annotation-id annotation) depth))))

(defun scholia-thread-test--text-count (text)
  "Return how often TEXT occurs in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (search-forward text nil t) count t)))

(defun scholia-thread-test--text-column (text)
  "Return the column TEXT begins at in the current buffer.
Signal an ERT test failure when TEXT was not rendered."
  (save-excursion
    (goto-char (point-min))
    (should (search-forward text nil t))
    (- (match-beginning 0) (line-beginning-position))))


;;;; The tree model

(ert-deftest scholia-thread-answers-roots-children-and-depth-first-order ()
  (let* ((annotations (scholia-thread-test--thread))
         (root (nth 0 annotations))
         (first-reply (nth 1 annotations))
         (second-reply (nth 2 annotations))
         (nested-reply (nth 3 annotations)))
    (should (scholia-thread-root-p root annotations))
    (should (scholia-thread-root-p root))
    (should-not (scholia-thread-root-p first-reply annotations))
    (should-not (scholia-thread-root-p nested-reply annotations))
    (should (equal (scholia-thread-test--ids
                    (scholia-thread-children root annotations))
                   '("id-b" "id-c")))
    (should (equal (scholia-thread-test--ids
                    (scholia-thread-children first-reply annotations))
                   '("id-d")))
    (should (null (scholia-thread-children second-reply annotations)))
    (should (null (scholia-thread-children nested-reply annotations)))
    (should (equal (scholia-thread-test--walk-ids-and-depths annotations)
                   '(("id-a" . 0) ("id-b" . 1) ("id-d" . 2) ("id-c" . 1))))))

(ert-deftest scholia-thread-orphan-replies-re-root-instead-of-vanishing ()
  (let* ((annotations (scholia-thread-test--orphaned-thread))
         (orphan (nth 2 annotations))
         (orphan-reply (nth 3 annotations)))
    (should (scholia-thread-root-p orphan annotations))
    (should-not (scholia-thread-root-p orphan))
    (should-not (scholia-thread-root-p orphan-reply annotations))
    (should (equal (scholia-thread-test--ids
                    (scholia-thread-children orphan annotations))
                   '("id-y")))
    (should (equal (scholia-thread-test--walk-ids-and-depths annotations)
                   '(("id-a" . 0) ("id-b" . 1) ("id-z" . 0) ("id-y" . 1))))))

(ert-deftest scholia-thread-walk-terminates-on-a-reply-to-cycle ()
  (let* ((root (scholia-thread-test--annotation "id-a" "note on alpha"))
         (reply (scholia-thread-test--reply "id-b" "first reply" "id-a"))
         (cycle-head (scholia-thread-test--reply "id-p" "points at q" "id-q"))
         (cycle-tail (scholia-thread-test--reply "id-q" "points at p" "id-p"))
         (annotations (list root reply cycle-head cycle-tail))
         (calls 0))
    (should (equal (scholia-thread-walk
                    annotations
                    (lambda (annotation depth)
                      (when (> (cl-incf calls) (length annotations))
                        (error "The walk over a cycle did not terminate"))
                      (cons (scholia-db-annotation-id annotation) depth)))
                   '(("id-a" . 0) ("id-b" . 1) ("id-p" . 0) ("id-q" . 1))))))


;;;; The renderer

(ert-deftest scholia-thread-render-indents-every-reply-past-its-parent ()
  (let ((annotations (scholia-thread-test--thread)))
    (with-temp-buffer
      (scholia-thread-render annotations)
      (let ((root (scholia-thread-test--text-column "note on alpha"))
            (first-reply (scholia-thread-test--text-column "first reply"))
            (second-reply (scholia-thread-test--text-column "second reply"))
            (nested-reply (scholia-thread-test--text-column "nested reply")))
        (should (< root first-reply))
        (should (< first-reply nested-reply))
        (should (equal first-reply second-reply)))))
  (let ((annotations
         (list (scholia-thread-test--annotation "id-m" "the root")
               (scholia-thread-test--reply "id-n" "reply one\nreply two" "id-m")
               (scholia-thread-test--reply "id-o" "under the reply" "id-n"))))
    (with-temp-buffer
      (scholia-thread-render annotations)
      (should (equal (scholia-thread-test--text-column "reply two")
                     (scholia-thread-test--text-column "reply one")))
      (should (< (scholia-thread-test--text-column "reply two")
                 (scholia-thread-test--text-column "under the reply")))
      (goto-char (point-min))
      (should (search-forward "reply two" nil t))
      (should (equal (scholia-db-annotation-id
                      (button-get (button-at (match-beginning 0))
                                  'scholia-annotation))
                     "id-n")))))

(ert-deftest scholia-thread-render-re-roots-an-orphan-instead-of-dropping-it ()
  (let ((annotations (scholia-thread-test--orphaned-thread)))
    (with-temp-buffer
      (scholia-thread-render annotations)
      (dolist (text '("note on alpha" "first reply" "orphaned note" "under the orphan"))
        (should (equal (scholia-thread-test--text-count text) 1)))
      (should (equal (scholia-thread-test--text-column "orphaned note")
                     (scholia-thread-test--text-column "note on alpha")))
      (should (< (scholia-thread-test--text-column "orphaned note")
                 (scholia-thread-test--text-column "under the orphan"))))))

(ert-deftest scholia-thread-render-buttons-carry-the-annotation-they-point-at ()
  (let ((annotations (scholia-thread-test--thread)))
    (with-temp-buffer
      (scholia-thread-render annotations)
      (dolist (expected '(("note on alpha" . "id-a")
                          ("nested reply" . "id-d")
                          ("second reply" . "id-c")))
        (goto-char (point-min))
        (should (search-forward (car expected) nil t))
        (let ((button (button-at (match-beginning 0))))
          (should button)
          (should (equal (scholia-db-annotation-id
                          (button-get button 'scholia-annotation))
                         (cdr expected)))
          (should (equal (scholia-db-annotation-text
                          (button-get button 'scholia-annotation))
                         (car expected))))))))

(ert-deftest scholia-thread-render-tells-a-continued-note-from-a-sibling-reply ()
  (let ((annotations
         (list (scholia-thread-test--annotation "id-a" "root note")
               (scholia-thread-test--reply "id-b" "first line\nsecond line"
                                           "id-a")
               (scholia-thread-test--reply "id-c" "second line" "id-a"))))
    (with-temp-buffer
      (scholia-thread-render annotations)
      (let ((lines (split-string (buffer-string) "\n" t)))
        (should (equal (length lines) 4))
        (should-not (equal (nth 2 lines) (nth 3 lines)))))))

(provide 'scholia-thread-test)
;;; scholia-thread-test.el ends here
