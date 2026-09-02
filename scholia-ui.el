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

(eval-when-compile
  (defvar marginalia-annotators))

(defconst scholia-ui--marginalia-entry
  '(scholia-annotation scholia-ui-marginalia-annotator builtin none)
  "Marginalia registry entry for Scholia annotation completion.")

(defun scholia-ui--annotation-entries (text)
  "Return stored annotation entries whose text equals TEXT."
  (seq-filter
   (lambda (entry)
     (equal text
            (scholia-db-annotation-text
             (scholia-db-entry-annotation entry))))
   (scholia-search-annotations)))

(defun scholia-ui--annotation-candidates ()
  "Return the globally collected annotation texts offered for completion."
  (seq-take
   (delete-dups
    (mapcar (lambda (entry)
              (scholia-db-annotation-text
               (scholia-db-entry-annotation entry)))
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
    (format " %d %s" (length entries) (scholia-db-entry-file last-entry))))

(defun scholia-ui-marginalia-setup ()
  "Register Scholia annotation completion with Marginalia."
  (require 'marginalia)
  (add-to-list 'marginalia-annotators scholia-ui--marginalia-entry))

(defun scholia-ui-marginalia-teardown ()
  "Remove Scholia annotation completion from Marginalia."
  (when (boundp 'marginalia-annotators)
    (setq marginalia-annotators
          (delete scholia-ui--marginalia-entry marginalia-annotators))))

(defun scholia-ui-unload-function ()
  "Remove global state installed by `scholia-ui-marginalia-setup'."
  (scholia-ui-marginalia-teardown)
  nil)

(provide 'scholia-ui)
;;; scholia-ui.el ends here
