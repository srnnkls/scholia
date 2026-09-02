;;; scholia-send-test.el --- Tests for the scholia send-record write path  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Covers the `:sends' surface of the annotation record and its all-or-nothing
;; batch write path (INV-2).  Every assertion goes through the db API rather
;; than the stored representation (INV-12).

;;; Code:

(require 'ert)
(require 'scholia-test-helper)

(eval-and-compile
  (setq load-prefer-newer t)
  (require 'scholia-vars nil t)
  (require 'scholia-db nil t))

(define-error 'scholia-send-test-refused
	      "The sender refused the payload")

(defconst scholia-send-test--source
  "alpha one\nbeta two\ngamma three\ndelta four\n"
  "Buffer contents the fixtures annotate.")

(defun scholia-send-test--bounds (needle)
  "Return the cons of start and end of the first NEEDLE in this buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward needle)
    (cons (match-beginning 0) (match-end 0))))

(defun scholia-send-test--annotation (id bounds)
  "Return an annotation with ID covering BOUNDS in this buffer."
  (list :id id
        :beg (car bounds)
        :end (cdr bounds)
        :text (concat "note on " id)
        :annotated-text (buffer-substring-no-properties (car bounds) (cdr bounds))
        :line nil :line-text nil :column nil :end-column nil
        :color 0
        :position :margin
        :reply-to nil
        :sends nil))

(defun scholia-send-test--annotations ()
  "Return one annotation per word of `scholia-send-test--source'.
Each already carries a send of its own to a target no other one went to."
  (mapcar (lambda (word)
            (plist-put (scholia-send-test--annotation
                        (concat "id-" word) (scholia-send-test--bounds word))
                       :sends
                       (list (scholia-send-test--send (concat "earlier-" word)))))
          '("alpha" "beta" "gamma" "delta")))

(defconst scholia-send-test--earlier-targets
  '("earlier-alpha" "earlier-beta" "earlier-gamma" "earlier-delta")
  "The targets `scholia-send-test--annotations' were already sent to.")

(defun scholia-send-test--send (target)
  "Return a send record aimed at the agent TARGET."
  (scholia-db-make-send :kind 'agent
                        :target target
                        :label (concat "claude · " target)
                        :herdr-session "shared"
                        :format 'rustc
                        :scope 'file))

(defun scholia-send-test--send-counts (annotations)
  "Return how many send records each of ANNOTATIONS carries."
  (mapcar (lambda (annotation)
            (length (scholia-db-annotation-sends annotation)))
          annotations))

(defun scholia-send-test--targets (annotations)
  "Return the target of every send record ANNOTATIONS carry, in order."
  (mapcan (lambda (annotation)
            (mapcar #'scholia-db-send-target
                    (scholia-db-annotation-sends annotation)))
          annotations))

(defun scholia-send-test--ids (annotations)
  "Return the ids of ANNOTATIONS in order."
  (mapcar #'scholia-db-annotation-id annotations))


;;;; The send record and the appender

(ert-deftest scholia-send-appender-returns-a-new-annotation-and-keeps-the-order ()
  (with-temp-buffer
    (insert scholia-send-test--source)
    (let* ((annotation (scholia-send-test--annotation
                        "id-gamma" (scholia-send-test--bounds "gamma")))
           (first (scholia-send-test--send "claude-2"))
           (second (scholia-send-test--send "claude-9")))
      (should (equal (scholia-db-send-kind first) 'agent))
      (should (equal (scholia-db-send-target first) "claude-2"))
      (should (equal (scholia-db-send-label first) "claude · claude-2"))
      (should (equal (scholia-db-send-herdr-session first) "shared"))
      (should (equal (scholia-db-send-format first) 'rustc))
      (should (equal (scholia-db-send-scope first) 'file))
      (should (string-match-p "\\`[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]T"
                              (scholia-db-send-at first)))
      (should-not (scholia-db-annotation-sends annotation))
      (let ((once (scholia-db-annotation-add-send annotation first)))
        (should-not (scholia-db-annotation-sends annotation))
        (should (equal (scholia-db-annotation-id once) "id-gamma"))
        (should (equal (scholia-db-annotation-text once) "note on id-gamma"))
        (should (equal (scholia-send-test--targets (list once)) '("claude-2")))
        (let ((twice (scholia-db-annotation-add-send once second)))
          (should (equal (scholia-send-test--targets (list once)) '("claude-2")))
          (should (equal (scholia-send-test--targets (list twice))
                         '("claude-2" "claude-9"))))))))


;;;; The batch write path (INV-2)

(ert-deftest scholia-send-batch-makes-one-call-and-records-on-every-annotation ()
  (with-temp-buffer
    (insert scholia-send-test--source)
    (let* ((annotations (scholia-send-test--annotations))
           (send (scholia-send-test--send "claude-2"))
           (calls 0)
           (recorded nil)
           (listener (lambda (sent record) (push (cons sent record) recorded))))
      (unwind-protect
          (progn
            (add-hook 'scholia-send-functions listener)
            (let ((result (scholia-db-send-batch
                           annotations send (lambda () (setq calls (1+ calls))))))
              (should (equal calls 1))
              (should (equal (scholia-send-test--ids result)
                             '("id-alpha" "id-beta" "id-gamma" "id-delta")))
              (should (equal (scholia-send-test--send-counts result) '(2 2 2 2)))
              (should (equal (scholia-send-test--targets result)
                             '("earlier-alpha" "claude-2"
                               "earlier-beta" "claude-2"
                               "earlier-gamma" "claude-2"
                               "earlier-delta" "claude-2")))
              (should (equal (scholia-send-test--send-counts annotations)
                             '(1 1 1 1)))
              (should (equal (scholia-send-test--targets annotations)
                             scholia-send-test--earlier-targets))
              (should (equal (length recorded) 1))
              (should (equal (scholia-send-test--ids (car (car recorded)))
                             (scholia-send-test--ids result)))
              (should (equal (scholia-send-test--send-counts (car (car recorded)))
                             '(2 2 2 2)))
              (should (equal (scholia-db-send-target (cdr (car recorded)))
                             "claude-2"))))
        (remove-hook 'scholia-send-functions listener)))))

(ert-deftest scholia-send-batch-writes-nothing-when-the-call-signals ()
  (with-temp-buffer
    (insert scholia-send-test--source)
    (let* ((annotations (scholia-send-test--annotations))
           (send (scholia-send-test--send "claude-2"))
           (calls 0)
           (recorded nil)
           (listener (lambda (sent record) (push (cons sent record) recorded))))
      (unwind-protect
          (progn
            (add-hook 'scholia-send-functions listener)
            (should-error
             (scholia-db-send-batch annotations send
                                    (lambda ()
                                      (setq calls (1+ calls))
                                      (signal 'scholia-send-test-refused
                                              (list "claude-2"))))
             :type 'scholia-send-test-refused)
            (should (equal calls 1))
            (should (equal (scholia-send-test--send-counts annotations)
                           '(1 1 1 1)))
            (should (equal (scholia-send-test--targets annotations)
                           scholia-send-test--earlier-targets))
            (should-not recorded))
        (remove-hook 'scholia-send-functions listener)))))

(ert-deftest scholia-send-batch-keeps-the-record-when-an-observer-signals ()
  "The annotations carrying a new send are the only copy of it.
`debug-on-error' is bound here because an observer signalling must be
reported rather than propagated whatever the user has set."
  (with-temp-buffer
    (insert scholia-send-test--source)
    (let* ((debug-on-error t)
           (annotations (scholia-send-test--annotations))
           (send (scholia-send-test--send "claude-2"))
           (seen nil)
           (logger (lambda (&rest _) (push 'logger seen)))
           (capture (lambda (&rest _)
                      (push 'capture seen)
                      (signal 'scholia-send-test-refused (list "locked"))))
           (notifier (lambda (&rest _) (push 'notifier seen))))
      (unwind-protect
          (progn
            (add-hook 'scholia-send-functions notifier)
            (add-hook 'scholia-send-functions capture)
            (add-hook 'scholia-send-functions logger)
            (let ((result (scholia-db-send-batch annotations send #'ignore)))
              (should (equal (scholia-send-test--ids result)
                             '("id-alpha" "id-beta" "id-gamma" "id-delta")))
              (should (equal (scholia-send-test--send-counts result)
                             '(2 2 2 2)))
              (should (equal (scholia-send-test--targets result)
                             '("earlier-alpha" "claude-2"
                               "earlier-beta" "claude-2"
                               "earlier-gamma" "claude-2"
                               "earlier-delta" "claude-2")))
              (should (equal (nreverse seen) '(logger capture)))))
        (remove-hook 'scholia-send-functions logger)
        (remove-hook 'scholia-send-functions capture)
        (remove-hook 'scholia-send-functions notifier)))))

(ert-deftest scholia-send-batch-persists-before-propagating-observer-quit ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-send-test--source
      (let* ((file (buffer-file-name buffer))
             (session (expand-file-name "quit.eld" scholia-session-directory))
             (annotations (scholia-send-test--annotations))
             (send (scholia-send-test--send "claude-2"))
             (observed-counts nil)
             (quit-propagated nil)
             (observer
              (lambda (&rest _)
                (setq observed-counts
                      (scholia-send-test--send-counts
                       (scholia-db-record-annotations
                        (scholia-db-record session file))))
                (signal 'quit nil))))
        (unwind-protect
            (progn
              (add-hook 'scholia-send-functions observer)
              (condition-case nil
                  (scholia-db-send-batch
                   annotations send #'ignore
                   (lambda (sent _record)
                     (scholia-db-save session file sent "checksum-one")))
                (quit (setq quit-propagated t)))
              (should quit-propagated)
              (should (equal observed-counts '(2 2 2 2)))
              (let ((loaded (scholia-db-record-annotations
                             (scholia-db-record session file))))
                (should (equal (scholia-send-test--send-counts loaded)
                               '(2 2 2 2)))
                (should (equal (scholia-send-test--targets loaded)
                               '("earlier-alpha" "claude-2"
                                 "earlier-beta" "claude-2"
                                 "earlier-gamma" "claude-2"
                                 "earlier-delta" "claude-2")))))
          (remove-hook 'scholia-send-functions observer))))))


;;;; Persistence

(ert-deftest scholia-send-records-survive-a-save-and-load-round-trip ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-send-test--source
      (let* ((file (buffer-file-name buffer))
             (session (expand-file-name "sends.eld" scholia-session-directory))
             (annotations (scholia-send-test--annotations))
             (send (scholia-send-test--send "claude-2"))
             (sent (scholia-db-send-batch annotations send #'ignore)))
        (scholia-db-save session file sent "checksum-one")
        (let* ((loaded (scholia-db-record-annotations
                        (scholia-db-record session file)))
               (gamma (nth 2 loaded))
               (record (cadr (scholia-db-annotation-sends gamma))))
          (should (equal (scholia-send-test--ids loaded)
                         '("id-alpha" "id-beta" "id-gamma" "id-delta")))
          (should (equal (scholia-send-test--send-counts loaded) '(2 2 2 2)))
          (should (equal (scholia-db-send-target
                          (car (scholia-db-annotation-sends gamma)))
                         "earlier-gamma"))
          (should (equal (scholia-db-annotation-id gamma) "id-gamma"))
          (should (equal (scholia-db-annotation-line-text gamma) "gamma three"))
          (should (equal (scholia-db-send-kind record) 'agent))
          (should (equal (scholia-db-send-target record) "claude-2"))
          (should (equal (scholia-db-send-label record) "claude · claude-2"))
          (should (equal (scholia-db-send-herdr-session record) "shared"))
          (should (equal (scholia-db-send-format record) 'rustc))
          (should (equal (scholia-db-send-scope record) 'file))
          (should (equal (scholia-db-send-at record) (scholia-db-send-at send)))
          (scholia-db-save session file loaded "checksum-two")
          (should (equal (scholia-send-test--send-counts
                          (scholia-db-record-annotations
                           (scholia-db-record session file)))
                         '(2 2 2 2)))
          (scholia-db-save session file annotations "checksum-three")
          (let ((merged (scholia-db-record-annotations
                         (scholia-db-record session file))))
            (should (equal (scholia-send-test--send-counts merged) '(2 2 2 2)))
            (should (equal (scholia-send-test--targets merged)
                           '("earlier-alpha" "claude-2"
                             "earlier-beta" "claude-2"
                             "earlier-gamma" "claude-2"
                             "earlier-delta" "claude-2")))))))))

(provide 'scholia-send-test)
;;; scholia-send-test.el ends here
