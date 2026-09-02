;;; scholia-ui-test.el --- Tests for completion-based annotation entry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'scholia-test-helper)

(eval-when-compile
  (defvar marginalia-annotators))

(eval-and-compile
  (setq load-prefer-newer t)
  (require 'scholia-vars nil t)
  (require 'scholia-overlay nil t)
  (require 'scholia-db nil t)
  (require 'scholia-store nil t)
  (require 'scholia-core nil t)
  (require 'scholia-session nil t)
  (require 'scholia-search nil t)
  (with-demoted-errors "scholia-ui unloadable: %S"
    (require 'scholia-ui nil t)))

(defun scholia-ui-test--require ()
  "Load the UI inside a test so a load failure cannot skip registration."
  (should (locate-library "scholia-ui"))
  (let ((load-prefer-newer t))
    (require 'scholia-ui)))

(defun scholia-ui-test--source (name content)
  "Write CONTENT to NAME in the current temporary session directory."
  (let ((file (expand-file-name name scholia-session-directory)))
    (with-temp-file file (insert content))
    file))

(defun scholia-ui-test--annotation (id text beg end annotated-text)
  "Return a stored annotation with ID and TEXT over ANNOTATED-TEXT."
  (list :id id :text text :beg beg :end end :annotated-text annotated-text
        :line 1 :line-text annotated-text :column (1- beg) :end-column (1- end)
        :color 0 :position :margin :reply-to nil :sends nil))

(defun scholia-ui-test--seed (session file annotations)
  "Store ANNOTATIONS for FILE in SESSION."
  (let ((session-file (scholia-session-file session)))
    (scholia-db-create-session session-file)
    (scholia-db-store-record
     session-file (scholia-db-make-record file annotations "seeded"))))

(defmacro scholia-ui-test--with-state (&rest body)
  "Evaluate BODY over an isolated set of sessions."
  (declare (indent 0) (debug body))
  `(scholia-test-with-session-directory
     (scholia-ui-test--require)
     (let ((session-default (default-value 'scholia-session))
           (active-default (copy-sequence (default-value 'scholia-active-sessions))))
       (unwind-protect
           (progn
             (set-default 'scholia-session nil)
             (set-default 'scholia-active-sessions nil)
             (let ((current-prefix-arg nil)
                   (scholia-session nil)
                   (scholia-active-sessions nil)
                   (scholia-project-sessions nil)
                   (scholia-project-root-function (lambda () nil))
                   (scholia-autosave nil)
                   (scholia-session-state-file
                    (expand-file-name "assignments.eld" scholia-session-directory)))
               ,@body))
         (set-default 'scholia-session session-default)
         (set-default 'scholia-active-sessions active-default)))))

(ert-deftest scholia-ui-offers-each-global-annotation-text-once ()
  "Completion aggregates recurring texts from every session."
  (scholia-ui-test--with-state
    (let ((first (scholia-ui-test--source "first.txt" "first words\n"))
          (last (scholia-ui-test--source "last.txt" "last words\n"))
          (candidates nil))
      (scholia-ui-test--seed
       "alpha" first
       (list (scholia-ui-test--annotation "a" "recurring note" 1 6 "first")))
      (scholia-ui-test--seed
       "beta" last
       (list (scholia-ui-test--annotation "b" "recurring note" 1 5 "last")
             (scholia-ui-test--annotation "c" "other note" 6 10 "words")))
      (scholia-test-with-temp-file-buffer _buffer "target words\n"
        (setq-local scholia-session "target")
        (scholia-mode 1)
        (goto-char 1)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest arguments)
                     (should-not (nth 1 arguments))
                     (should (eq (completion-metadata-get
                                  (completion-metadata "" collection nil)
                                  'category)
                                 'scholia-annotation))
                     (setq candidates (all-completions "" collection))
                     "recurring note")))
          (call-interactively #'scholia-annotate))
        (should (equal 1 (seq-count (lambda (candidate)
                                      (equal candidate "recurring note"))
                                    candidates)))
        (should (member "other note" candidates))))))

(ert-deftest scholia-ui-caps-globally-deduplicated-history ()
  "The configured limit retains two globally collected texts without ranking them."
  (scholia-ui-test--with-state
    (let ((one (scholia-ui-test--source "one.txt" "one two three\n"))
          (two (scholia-ui-test--source "two.txt" "four five six\n"))
          (candidates nil)
          (scholia-annotation-history-limit 2))
      (scholia-ui-test--seed
       "one" one
       (list (scholia-ui-test--annotation "a" "reused note" 1 4 "one")
             (scholia-ui-test--annotation "b" "reused note" 5 8 "two")
             (scholia-ui-test--annotation "c" "one-only note" 9 14 "three")))
      (scholia-ui-test--seed
       "two" two
       (list (scholia-ui-test--annotation "d" "reused note" 1 5 "four")
             (scholia-ui-test--annotation "e" "reused note" 6 10 "five")
             (scholia-ui-test--annotation "f" "two-only note" 11 14 "six")))
      (scholia-test-with-temp-file-buffer _buffer "target words\n"
        (setq-local scholia-session "target")
        (scholia-mode 1)
        (goto-char 1)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _)
                     (setq candidates (all-completions "" collection))
                     (car candidates))))
          (call-interactively #'scholia-annotate))
        (should (= (length candidates) 2))
        (should (= (length (delete-dups (copy-sequence candidates))) 2))
        (should (seq-every-p (lambda (candidate)
                               (member candidate '("reused note" "one-only note" "two-only note")))
                             candidates))))))

(ert-deftest scholia-ui-reuses-completion-and-accepts-new-free-text ()
  "Both a selected candidate and an unmatched string use the normal lifecycle."
  (scholia-ui-test--with-state
    (let ((history (scholia-ui-test--source "history.txt" "history\n")))
      (scholia-ui-test--seed
       "previous" history
       (list (scholia-ui-test--annotation "a" "reused note" 1 8 "history")))
      (scholia-test-with-temp-file-buffer buffer "alpha beta\n"
        (setq-local scholia-session "target")
        (scholia-mode 1)
        (goto-char 1)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) "reused note")))
          (call-interactively #'scholia-annotate))
        (goto-char 7)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) "brand new note")))
          (call-interactively #'scholia-annotate))
        (scholia-save-annotations)
        (should (equal (sort (mapcar #'scholia-db-annotation-text
                                     (scholia-db-record-annotations
                                      (scholia-db-record (scholia-session-file)
                                                         (buffer-file-name buffer))))
                             #'string<)
                       '("brand new note" "reused note")))))))

(ert-deftest scholia-ui-registers-its-marginalia-annotator-explicitly ()
  "Marginalia registration is explicit, idempotent, and reversible."
  (let ((without (generate-new-buffer " *scholia-ui-no-marginalia*"))
        (root (expand-file-name scholia-test-project-root))
        (entry '(scholia-annotation scholia-ui-marginalia-annotator
                                    builtin none)))
    (unwind-protect
        (progn
          (should (zerop
                   (call-process invocation-name nil without nil
                                 "-Q" "--batch" "-L" root "--eval"
                                 (concat
                                  "(progn (require 'scholia-ui) "
                                  "(prin1 (list (featurep 'marginalia) "
                                  "(boundp 'marginalia-annotators) "
                                  "(and (boundp 'marginalia-annotators) "
                                  "(assq 'scholia-annotation "
                                  "marginalia-annotators)))))"))))
          (with-current-buffer without
            (goto-char (point-min))
            (should (equal (read (current-buffer)) '(nil nil nil))))
          (scholia-ui-test--require)
          (scholia-ui-marginalia-setup)
          (scholia-ui-marginalia-setup)
          (should (= (seq-count (lambda (candidate) (equal candidate entry))
                                marginalia-annotators)
                     1))
          (scholia-ui-test--with-state
            (let ((first (scholia-ui-test--source "first.txt" "first words\n"))
                  (last (scholia-ui-test--source "last 2.txt" "last words\n")))
              (scholia-ui-test--seed
               "alpha" first
               (list (scholia-ui-test--annotation
                      "a" "recurring note" 1 6 "first")))
              (scholia-ui-test--seed
               "beta" last
               (list (scholia-ui-test--annotation
                      "b" "recurring note" 1 5 "last")))
              (let* ((annotator (nth 1 (car (member entry marginalia-annotators))))
                     (output (funcall annotator "recurring note"))
                     (count-output
                      (replace-regexp-in-string
                       (regexp-quote last) "" output t t)))
                (should (functionp annotator))
                (should
                 (string-match-p
                  "\\(?:\\`\\|[^[:digit:]]\\)2\\(?:\\'\\|[^[:digit:]]\\)"
                  count-output))
                (should (string-match-p (regexp-quote last) output)))))
          (scholia-ui-marginalia-teardown)
          (should-not (member entry marginalia-annotators))
          (scholia-ui-marginalia-setup)
          (unload-feature 'scholia-ui t)
          (should-not (member entry marginalia-annotators))
          (should (require 'scholia-ui))
          (should-not (member entry marginalia-annotators))
          (scholia-ui-marginalia-setup)
          (should (member entry marginalia-annotators)))
      (when (fboundp 'scholia-ui-marginalia-teardown)
        (scholia-ui-marginalia-teardown))
      (kill-buffer without))))

(provide 'scholia-ui-test)
;;; scholia-ui-test.el ends here
