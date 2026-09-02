;;; scholia-filter-test.el --- Tests for the scholia annotation filter  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; A candidate pairs one annotation with the file and session it was found
;; under, and the filter reads the annotation through the db API rather than
;; its stored representation (INV-12).

;;; Code:

(require 'ert)
(require 'scholia-test-helper)

(eval-and-compile
  (setq load-prefer-newer t)
  (require 'scholia-vars nil t)
  (require 'scholia-db nil t)
  (require 'scholia-filter nil t))

(defun scholia-filter-test--annotation (id text annotated-text &optional sends)
  "Return an annotation with ID, TEXT, ANNOTATED-TEXT and SENDS."
  (list :id id
        :beg nil :end nil
        :text text
        :annotated-text annotated-text
        :line nil :line-text nil :column nil :end-column nil
        :color 0
        :position :margin
        :reply-to nil
        :sends sends))

(defun scholia-filter-test--send (target)
  "Return a send record dispatched to TARGET."
  (list :at "2026-08-25T12:03:11+0200"
        :kind 'agent
        :target target
        :label "claude · foo"
        :herdr-session "shared"
        :format 'rustc
        :scope 'annotation))

(defun scholia-filter-test--candidate (file session annotation)
  "Return the candidate holding ANNOTATION found in FILE under SESSION."
  (list :file file :session session :annotation annotation))

(defconst scholia-filter-test--handler
  (scholia-filter-test--candidate
   "/w/src/auth.el" "review-2"
   (scholia-filter-test--annotation
    "id-handler" "rebinds the handler on every render" "(setq handler ctx)"
    (list (scholia-filter-test--send "claude-2"))))
  "An annotation on auth.el that was sent to an agent.")

(defconst scholia-filter-test--guard
  (scholia-filter-test--candidate
   "/w/src/render.el" "review-2"
   (scholia-filter-test--annotation
    "id-guard" "todo: extract the guard" "(when guard"))
  "An unsent annotation on render.el.")

(defconst scholia-filter-test--fixture
  (scholia-filter-test--candidate
   "/w/test/session-test.el" "sweep"
   (scholia-filter-test--annotation
    "id-fixture" "the fixture drifts" "(ert-deftest session"))
  "An unsent annotation in another session.")

(defconst scholia-filter-test--reply
  (scholia-filter-test--candidate
   "/w/src/render.el" "review-2"
   (scholia-filter-test--annotation "id-reply" "handler note is stale" nil))
  "A reply, which carries no annotated text.")

(defconst scholia-filter-test--candidates
  (list scholia-filter-test--handler
        scholia-filter-test--guard
        scholia-filter-test--fixture
        scholia-filter-test--reply)
  "The candidate set every filter in this suite narrows.")

(defun scholia-filter-test--ids (candidates)
  "Return the annotation ids of CANDIDATES, in order."
  (mapcar (lambda (candidate)
            (scholia-db-annotation-id (plist-get candidate :annotation)))
          candidates))

(defun scholia-filter-test--narrow (filter)
  "Return the annotation ids the candidate set retains under FILTER."
  (scholia-filter-test--ids
   (scholia-filter-apply filter scholia-filter-test--candidates)))


(ert-deftest scholia-filter-parses-a-flat-query-into-combinators ()
  (should (null (scholia-filter-parse "")))
  (should (null (scholia-filter-parse "   ")))
  (should (equal (scholia-filter-parse "text:handler") '(text . "handler")))
  (should (equal (scholia-filter-parse "file:auth text:handler")
                 '(and (file . "auth") (text . "handler"))))
  (should (equal (scholia-filter-parse "!send:claude") '(not (send . "claude"))))
  (should (equal (scholia-filter-parse "handler")
                 '(or (file . "handler")
                      (text . "handler")
                      (annotated-text . "handler")
                      (session . "handler")
                      (send . "handler")))))

(ert-deftest scholia-filter-combines-terms-per-candidate ()
  (should (equal (scholia-filter-apply nil scholia-filter-test--candidates)
                 scholia-filter-test--candidates))
  (should (equal (scholia-filter-test--narrow (scholia-filter-parse ""))
                 '("id-handler" "id-guard" "id-fixture" "id-reply")))
  (should (equal (scholia-filter-test--narrow
                  '(and (file . "auth") (text . "handler")))
                 '("id-handler")))
  (should (equal (scholia-filter-test--narrow
                  '(or (session . "sweep") (send . "claude-2")))
                 '("id-handler" "id-fixture")))
  (should (equal (scholia-filter-test--narrow
                  '(not (or (file . "auth") (text . "todo"))))
                 '("id-fixture" "id-reply")))
  (should (equal (scholia-filter-test--narrow (scholia-filter-parse "sweep"))
                 '("id-fixture")))
  (should (scholia-filter-match-p '(file . "auth") scholia-filter-test--handler))
  (should-not (scholia-filter-match-p '(file . "auth")
                                      scholia-filter-test--fixture)))

(ert-deftest scholia-filter-skips-candidates-a-field-is-absent-from ()
  (should (equal (scholia-filter-test--narrow '(annotated-text . "handler"))
                 '("id-handler")))
  (should (equal (scholia-filter-test--narrow '(not (annotated-text . "handler")))
                 '("id-guard" "id-fixture" "id-reply")))
  (should (equal (scholia-filter-test--narrow '(send . "claude-2"))
                 '("id-handler")))
  (should (equal (scholia-filter-test--narrow '(not (send . "claude-2")))
                 '("id-guard" "id-fixture" "id-reply"))))

(ert-deftest scholia-filter-matches-a-send-on-its-label-as-well-as-its-target ()
  (should (equal (scholia-filter-test--narrow '(send . "foo"))
                 '("id-handler")))
  (should (equal (scholia-filter-test--narrow '(not (send . "foo")))
                 '("id-guard" "id-fixture" "id-reply"))))

(provide 'scholia-filter-test)
;;; scholia-filter-test.el ends here
