;;; scholia-thread.el --- Reply threads  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/scholia
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Replies form a tree over a candidate set of annotations: a reply names
;; the annotation it answers by id and holds no position of its own.  The
;; tree is read from the candidate set alone, so a reply whose parent is
;; not in it heads a thread of its own rather than being dropped.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'scholia-db)

(defconst scholia-thread--indent-width 2
  "Columns a reply is set in past the annotation it answers.")


;;;; The tree model

(defun scholia-thread-root-p (annotation &optional annotations)
  "Return non-nil when ANNOTATION heads a thread.
An annotation answering none of them does.  Given the candidate set
ANNOTATIONS, one answering an annotation outside it does as well, so an
orphaned reply heads a thread of its own instead of leaving with the
parent it lost.  Without ANNOTATIONS the parent is taken to be there."
  (let ((parent (scholia-db-annotation-reply-to annotation)))
    (or (null parent)
        (and annotations
             (not (seq-find (lambda (candidate)
                              (equal (scholia-db-annotation-id candidate)
                                     parent))
                            annotations))))))

(defun scholia-thread-children (annotation annotations)
  "Return the replies among ANNOTATIONS answering ANNOTATION directly.
They come in the order ANNOTATIONS holds them, and the replies they
carry themselves are not among them."
  (let ((id (scholia-db-annotation-id annotation)))
    (seq-filter (lambda (candidate)
                  (and (scholia-db-annotation-reply-p candidate)
                       (equal (scholia-db-annotation-reply-to candidate) id)))
                annotations)))

(defun scholia-thread-walk (annotations function)
  "Return the results of FUNCTION over the threads of ANNOTATIONS.
FUNCTION is called with an annotation and its depth, depth-first in
pre-order over each thread ANNOTATIONS heads, in the order ANNOTATIONS
holds them and each root at depth 0.  An annotation no root reaches is
walked as a root of its own once the roots are done, so nothing is left
unvisited, and no annotation is visited twice however its `:reply-to'
points, so a cycle an imported session carries terminates."
  (let ((visited nil)
        (results nil))
    (cl-labels
        ((descend (annotation depth)
           (let ((id (scholia-db-annotation-id annotation)))
             (unless (member id visited)
               (push id visited)
               (push (funcall function annotation depth) results)
               (dolist (child (scholia-thread-children annotation annotations))
                 (descend child (1+ depth)))))))
      (dolist (annotation annotations)
        (when (scholia-thread-root-p annotation annotations)
          (descend annotation 0)))
      (dolist (annotation annotations)
        (descend annotation 0)))
    (nreverse results)))


;;;; The renderer

(defun scholia-thread-render (annotations)
  "Insert the threads of ANNOTATIONS at point, a line per line of note.
Every note is set in one step past the note it answers, and a note
carrying several lines sets each of them there, so its own lines stay
flush with one another and every reply still reads as the deeper one.
Each line is a button of its own carrying the annotation in the
`scholia-annotation' property, so a multi-line note answers wherever it
is clicked."
  (scholia-thread-walk
   annotations
   (lambda (annotation depth)
     (let ((indent (make-string (* depth scholia-thread--indent-width) ?\s))
           (lines (split-string (scholia-db-annotation-text annotation) "\n")))
       (dolist (line lines)
         (insert indent)
         (insert-text-button line 'scholia-annotation annotation)
         (insert "\n"))))))

(provide 'scholia-thread)
;;; scholia-thread.el ends here
