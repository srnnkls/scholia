;;; scholia-filter.el --- Narrowing a candidate set  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/scholia
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The dashboard narrows its candidates with a filter:
;;
;;   FILTER    := nil | (FIELD . REGEXP) | (and FILTER...)
;;                    | (or FILTER...) | (not FILTER)
;;   FIELD     in file, text, annotated-text, session, send
;;   CANDIDATE := (:file FILE :session SESSION :annotation ANNOTATION)
;;
;; Every combinator answers for one candidate at a time, so `not' keeps
;; the candidates its term rejects rather than emptying the set.

;;; Code:

(require 'seq)
(require 'scholia-db)

(defconst scholia-filter--fields
  '(file text annotated-text session send)
  "The fields a bare regexp is matched against, in the order tried.")

(defconst scholia-filter--leaf-regexp
  (concat "\\`\\("
          (regexp-opt (mapcar #'symbol-name scholia-filter--fields))
          "\\):\\(.*\\)\\'")
  "Regexp matching a term that names the field it narrows.")


;;;; Reading a field off a candidate

(defun scholia-filter--values (field candidate)
  "Return what CANDIDATE carries under FIELD, as a list of strings.
An annotation is read through the db accessors rather than through its
stored representation, and a field CANDIDATE carries nothing under
answers no string at all.  A send answers both the destination it went
to and the label the dashboard shows it under, so a query typed against
what is on screen narrows as one against the destination does."
  (let ((annotation (plist-get candidate :annotation)))
    (pcase field
      ('file (list (plist-get candidate :file)))
      ('session (list (plist-get candidate :session)))
      ('text (list (scholia-db-annotation-text annotation)))
      ('annotated-text
       (list (scholia-db-annotation-annotated-text annotation)))
      ('send (seq-mapcat (lambda (send)
                           (list (scholia-db-send-target send)
                                 (scholia-db-send-label send)))
                         (scholia-db-annotation-sends annotation))))))


;;;; Matching

(defun scholia-filter-match-p (filter candidate)
  "Return non-nil when CANDIDATE satisfies FILTER.
A nil FILTER matches every candidate."
  (pcase filter
    ('nil t)
    (`(and . ,terms)
     (seq-every-p (lambda (term) (scholia-filter-match-p term candidate))
                  terms))
    (`(or . ,terms)
     (seq-some (lambda (term) (scholia-filter-match-p term candidate))
               terms))
    (`(not ,term) (not (scholia-filter-match-p term candidate)))
    (`(,field . ,regexp)
     (seq-some (lambda (value) (and value (string-match-p regexp value)))
               (scholia-filter--values field candidate)))))

(defun scholia-filter-apply (filter candidates)
  "Return the CANDIDATES satisfying FILTER, in their original order."
  (seq-filter (lambda (candidate) (scholia-filter-match-p filter candidate))
              candidates))


;;;; Parsing a query

(defun scholia-filter--parse-term (term)
  "Return the filter TERM denotes.
A leading `!' negates, a `FIELD:REGEXP' term narrows that field, and a
bare regexp narrows any of `scholia-filter--fields'."
  (cond
   ((string-prefix-p "!" term)
    (list 'not (scholia-filter--parse-term (substring term 1))))
   ((string-match scholia-filter--leaf-regexp term)
    (cons (intern (match-string 1 term)) (match-string 2 term)))
   (t (cons 'or (mapcar (lambda (field) (cons field term))
                        scholia-filter--fields)))))

(defun scholia-filter-parse (query)
  "Return the filter the whitespace-separated terms of QUERY denote.
Several terms narrow together; a blank QUERY answers nil, which every
candidate satisfies."
  (pcase (mapcar #'scholia-filter--parse-term
                 (split-string (or query "") nil t))
    ('nil nil)
    (`(,term) term)
    (terms (cons 'and terms))))

(provide 'scholia-filter)
;;; scholia-filter.el ends here
