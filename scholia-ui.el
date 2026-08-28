;;; scholia-ui.el --- Annotation completion  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/scholia
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Collects recurring annotation text for completion.

;;; Code:

(require 'seq)
(require 'scholia-search)

(defun scholia-ui--annotation-entries (text)
  "Return stored annotation entries whose text equals TEXT."
  (seq-filter
   (lambda (entry)
     (equal text
            (scholia-db-annotation-text (plist-get entry :annotation))))
   (scholia-search-annotations)))

(defun scholia-ui--annotation-candidates ()
  "Return the globally collected annotation texts offered for completion."
  (seq-take
   (delete-dups
    (mapcar (lambda (entry)
              (scholia-db-annotation-text (plist-get entry :annotation)))
            (scholia-search-annotations)))
   scholia-annotation-history-limit))

(defun scholia-ui-read-annotation ()
  "Read an annotation text with recurring annotations as candidates."
  (let ((candidates (scholia-ui--annotation-candidates)))
    (completing-read
     "Annotation: "
     (lambda (string predicate action)
       (if (eq action 'metadata)
           '(metadata (category . scholia-annotation))
         (complete-with-action action candidates string predicate)))
     nil nil)))

(defun scholia-ui-marginalia-annotator (candidate)
  "Return prior-use metadata for annotation CANDIDATE."
  (let* ((entries (scholia-ui--annotation-entries candidate))
         (last-entry (car (last entries))))
    (format " %d %s" (length entries) (plist-get last-entry :file))))

(with-eval-after-load 'marginalia
  (let ((entry '(scholia-annotation scholia-ui-marginalia-annotator
                                      builtin none))
        (registry (intern "marginalia-annotators")))
    (set registry
         (cons entry (delete entry (symbol-value registry))))))

(provide 'scholia-ui)
;;; scholia-ui.el ends here
