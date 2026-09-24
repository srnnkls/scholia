;;; scholia-core-test.el --- Tests for the scholia annotation lifecycle  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Covers `scholia-core', the layer joining the overlay engine to the record
;; store.  Persisted state is read back through the db API rather than
;; through the stored representation (INV-12).

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'scholia-test-helper)

(eval-and-compile
  (setq load-prefer-newer t)
  (require 'scholia-vars nil t)
  (require 'scholia-overlay nil t)
  (require 'scholia-render nil t)
  (require 'scholia-core nil t))

(defconst scholia-core-test--source
  "alpha beta\ngamma delta\n\nepsilon zeta\n"
  "Four lines the fixtures annotate.
Line one holds \"alpha\" at 1-5 and \"beta\" at 7-10 with its terminator
at 11, line two \"gamma\" at 12-16 and \"delta\" at 18-22 with its
terminator at 23, line three is empty and carries only its terminator at
24, and line four holds \"epsilon\" at 25-31 and \"zeta\" at 33-36 with
its terminator at 37.")

(defun scholia-core-test--chain-text (chain)
  "Return the annotation text CHAIN carries."
  (overlay-get (car chain) 'scholia-annotation))

(defun scholia-core-test--stored (session file)
  "Return the annotations stored for FILE in the session at SESSION."
  (scholia-db-record-annotations
   (scholia-db-record session file)))

(defun scholia-core-test--texts (annotations)
  "Return the notes ANNOTATIONS carry, sorted."
  (sort (mapcar #'scholia-db-annotation-text annotations) #'string<))

(defun scholia-core-test--with-id (id annotations)
  "Return the annotation of ANNOTATIONS carrying ID."
  (seq-find (lambda (annotation)
              (equal (scholia-db-annotation-id annotation) id))
            annotations))

(defun scholia-core-test--change-hooks ()
  "Return the scholia entries of the buffer-local `after-change-functions'."
  (seq-filter (lambda (entry)
                (and (symbolp entry)
                     (string-prefix-p "scholia-" (symbol-name entry))))
              after-change-functions))

(defmacro scholia-core-test--reported (&rest body)
  "Evaluate BODY and return the last thing it reported through `message'."
  (declare (indent 0) (debug body))
  (let ((reported (make-symbol "reported")))
    `(let ((,reported nil))
       (cl-letf (((symbol-function 'message)
                  (lambda (format &rest arguments)
                    (when format
                      (setq ,reported (apply #'format-message format arguments))))))
         ,@body)
       ,reported)))


;;;; The annotation commands

(ert-deftest scholia-core-annotates-the-region-then-the-symbol-at-point ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer _buffer scholia-core-test--source
      (setq-local transient-mark-mode t)
      (setq-local scholia-session "annotate")
      (setq-local scholia-use-messages t)
      (scholia-mode 1)
      (goto-char 11)
      (set-mark 7)
      (activate-mark)
      (scholia-annotate "on the region")
      (should-not (region-active-p))
      (deactivate-mark)
      (goto-char 13)
      (scholia-annotate "on the symbol")
      (let ((reported (scholia-core-test--reported
                        (goto-char 24)
                        (scholia-annotate "on nothing at all"))))
        (should (stringp reported))
        (should (> (length reported) 0)))
      (goto-char 26)
      (scholia-annotate "on the last symbol")
      (let ((chains (scholia-buffer-chains)))
        (should (equal (mapcar #'length chains) '(1 1 1)))
        (scholia-test-should-overlay-range (car (nth 0 chains)) 7 11)
        (scholia-test-should-overlay-range (car (nth 1 chains)) 12 17)
        (scholia-test-should-overlay-range (car (nth 2 chains)) 25 32)
        (should (equal (scholia-core-test--chain-text (nth 1 chains))
                       "on the symbol"))
        (should (equal (scholia-chain-color-index (nth 2 chains)) 2)))
      (let ((reported (scholia-core-test--reported
                        (goto-char 19)
                        (scholia-annotate ""))))
        (should (string-match-p "empty" reported)))
      (goto-char 13)
      (scholia-annotate "over the symbol again")
      (should (equal (sort (mapcar #'scholia-core-test--chain-text
                                   (scholia-buffer-chains))
                           #'string<)
                     '("on the last symbol" "on the region"
                       "over the symbol again")))
      (goto-char 11)
      (set-mark 1)
      (activate-mark)
      (scholia-annotate "over a region already annotated")
      (deactivate-mark)
      (should (= 4 (length (scholia-buffer-chains))))
      (let ((reported (scholia-core-test--reported
                        (goto-char 25)
                        (set-mark 24)
                        (activate-mark)
                        (scholia-annotate "on nothing but a line break"))))
        (deactivate-mark)
        (should (string-match-p "no text" reported)))
      (should (equal (mapcar #'length (scholia-buffer-chains)) '(1 1 1 1)))
      (goto-char 19)
      (scholia-annotate "on delta")
      (should (equal (scholia-chain-color-index (scholia-chain-at 19)) 4)))))

(ert-deftest scholia-core-deletes-the-whole-chain-at-point ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-core-test--source
      (let ((file (buffer-file-name buffer)))
        (setq-local transient-mark-mode t)
        (setq-local scholia-session "delete")
        (scholia-mode 1)
        (goto-char 17)
        (set-mark 7)
        (activate-mark)
        (scholia-annotate "spanning two lines")
        (deactivate-mark)
        (goto-char 26)
        (scholia-annotate "kept")
        (should (equal (mapcar #'length (scholia-buffer-chains)) '(2 1)))
        (scholia-save-annotations)
        (let ((stored (scholia-core-test--stored (scholia-session-file) file)))
          (should (equal (scholia-core-test--texts stored)
                         '("kept" "spanning two lines")))
          (should (equal (scholia-db-annotation-interval
                          (seq-find (lambda (annotation)
                                      (equal (scholia-db-annotation-text annotation)
                                             "spanning two lines"))
                                    stored))
                         '(7 . 17))))
        (goto-char 13)
        (scholia-delete-annotation)
        (should (equal (mapcar #'length (scholia-buffer-chains)) '(1)))
        (scholia-save-annotations)
        (should (equal (scholia-core-test--texts
                        (scholia-core-test--stored (scholia-session-file) file))
                       '("kept")))))))

(ert-deftest scholia-core-deleting-an-annotation-removes-the-note-drawn-for-it ()
  "A deleted annotation leaves no note behind.
The note is its own overlay, found by chain id rather than reached
through the chain, so deleting the chain alone leaves the space it draws
still taken in the buffer."
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer _buffer scholia-core-test--source
      (setq-local scholia-session "note-delete")
      (scholia-mode 1)
      (goto-char 1)
      (scholia-annotate "a note drawn beside the annotation")
      (should (= 1 (length (scholia-render--notes))))
      (scholia-delete-annotation)
      (should (= 0 (length (scholia-render--notes)))))))

(defun scholia-core-test--notes ()
  "Return the notes the buffer's chains carry."
  (mapcar (lambda (chain)
            (overlay-get (car chain) 'scholia-annotation))
          (scholia-buffer-chains)))

(ert-deftest scholia-core-annotate-edits-the-annotation-point-stands-on ()
  "With no region, annotating over an annotation replaces its note.
Nothing is added, so the buffer still holds one annotation."
  (with-temp-buffer
    (insert "alpha beta gamma\n")
    (goto-char 1)
    (let ((scholia-project-root-function (lambda () nil)))
      (scholia-annotate "first note")
      (should (equal (scholia-core-test--notes) '("first note")))
      (goto-char 3)
      (scholia-annotate "second note")
      (should (equal (scholia-core-test--notes) '("second note"))))))

(ert-deftest scholia-core-annotate-of-a-subregion-makes-a-second-annotation ()
  "A selected region annotates whether or not it overlaps one already."
  (with-temp-buffer
    (insert "alpha beta gamma\n")
    (goto-char 1)
    (let ((scholia-project-root-function (lambda () nil))
          (transient-mark-mode t))
      (scholia-annotate "the whole word")
      (goto-char 1)
      (set-mark 4)
      (setq mark-active t)
      (scholia-annotate "the first part of it")
      (should (equal (sort (scholia-core-test--notes) #'string<)
                     '("the first part of it" "the whole word")))
      (should (= 2 (length (scholia-buffer-chains)))))))

(ert-deftest scholia-core-annotate-reports-instead-of-signalling ()
  "Annotating nowhere reports and returns, leaving the buffer alone."
  (with-temp-buffer
    (goto-char (point-min))
    (let ((scholia-project-root-function (lambda () nil))
          (scholia-use-messages nil))
      (should-not (scholia-annotate "nowhere"))
      (should-not (scholia-buffer-chains)))))

(ert-deftest scholia-core-replies-point-at-the-annotation-they-answer ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-core-test--source
      (let ((file (buffer-file-name buffer)))
        (setq-local scholia-session "reply")
        (scholia-mode 1)
        (goto-char 13)
        (scholia-annotate "the parent note")
        (scholia-save-annotations)
        (goto-char 13)
        (scholia-reply-to "the answer")
        (scholia-save-annotations)
        (let* ((stored (scholia-core-test--stored (scholia-session-file) file))
               (parent (seq-find (lambda (annotation)
                                   (equal (scholia-db-annotation-text annotation)
                                          "the parent note"))
                                 stored))
               (reply (seq-find #'scholia-db-annotation-reply-p stored)))
          (should (equal (scholia-core-test--texts stored)
                         '("the answer" "the parent note")))
          (should (equal (scholia-db-annotation-text reply) "the answer"))
          (should (equal (scholia-db-annotation-reply-to reply)
                         (scholia-db-annotation-id parent)))
          (should-not (scholia-db-annotation-beg reply))
          (should-not (scholia-db-annotation-end reply))
          (should-not (scholia-db-annotation-line reply))
          (should-not (scholia-db-annotation-line-text reply))
          (should-not (scholia-db-annotation-column reply))
          (should-not (scholia-db-annotation-end-column reply)))))))

(ert-deftest scholia-core-moves-point-between-annotations ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer _buffer scholia-core-test--source
      (setq-local scholia-session "navigate")
      (scholia-mode 1)
      (goto-char 8)
      (scholia-annotate "on beta")
      (goto-char 13)
      (scholia-annotate "on gamma")
      (goto-char 26)
      (scholia-annotate "on epsilon")
      (goto-char (point-min))
      (scholia-goto-next-annotation)
      (should (equal (point) 7))
      (scholia-goto-next-annotation)
      (should (equal (point) 12))
      (scholia-goto-next-annotation)
      (should (equal (point) 25))
      (scholia-goto-next-annotation)
      (should (equal (point) 25))
      (goto-char (point-max))
      (scholia-goto-previous-annotation)
      (should (equal (point) 25))
      (scholia-goto-previous-annotation)
      (should (equal (point) 12))
      (scholia-goto-previous-annotation)
      (should (equal (point) 7))
      (scholia-goto-previous-annotation)
      (should (equal (point) 7)))))


;;;; The checksum

(ert-deftest scholia-core-checksums-the-widened-buffer ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-core-test--source
      (let ((file (buffer-file-name buffer))
            (whole (md5 scholia-core-test--source)))
        (setq-local scholia-session "checksum")
        (scholia-mode 1)
        (goto-char 13)
        (scholia-annotate "on gamma")
        (narrow-to-region 25 37)
        (should (equal (scholia-buffer-checksum) whole))
        (dolist (coding '(utf-8-dos utf-8-mac utf-8-unix))
          (let ((buffer-file-coding-system coding))
            (should (equal (scholia-buffer-checksum) whole))))
        (should-not (equal (scholia-buffer-checksum)
                           (md5 (buffer-substring-no-properties
                                 (point-min) (point-max)))))
        (scholia-save-annotations)
        (widen)
        (let ((record (scholia-db-record (scholia-session-file)
                                         file)))
          (should (equal (scholia-db-record-checksum record) whole))
          (should (equal (scholia-core-test--texts
                          (scholia-db-record-annotations record))
                         '("on gamma"))))))))


;;;; Indirect buffers

(ert-deftest scholia-core-annotates-an-indirect-buffer-against-its-base-file ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer base scholia-core-test--source
      (let ((file (buffer-file-name base))
            (indirect (make-indirect-buffer base "scholia-core-test-clone" t)))
        (unwind-protect
            (progn
              (with-current-buffer indirect
                (setq-local scholia-session "indirect")
                (scholia-mode 1)
                (goto-char 13)
                (scholia-annotate "from the clone")
                (scholia-save-annotations)
                (should (equal (scholia-core-test--texts
                                (scholia-core-test--stored (scholia-session-file)
                                                           file))
                               '("from the clone")))
                (let ((chains (scholia-buffer-chains)))
                  (should (equal (mapcar #'length chains) '(1)))
                  (scholia-test-should-overlay-range (car (nth 0 chains)) 12 17))
                (let ((scholia-autosave t))
                  (scholia-mode -1))
                (should-not (scholia-buffer-chains))
                (scholia-mode 1)
                (let ((chains (scholia-buffer-chains)))
                  (should (equal (mapcar #'length chains) '(1)))
                  (scholia-test-should-overlay-range (car (nth 0 chains)) 12 17)
                  (should (equal (scholia-core-test--chain-text (nth 0 chains))
                                 "from the clone"))))
              (with-current-buffer base
                (setq-local scholia-session "indirect")
                (scholia-mode 1)
                (let ((chains (scholia-buffer-chains)))
                  (should (equal (mapcar #'length chains) '(1)))
                  (scholia-test-should-overlay-range (car (nth 0 chains)) 12 17)
                  (should (equal (scholia-core-test--chain-text (nth 0 chains))
                                 "from the clone")))))
          (when (buffer-live-p indirect)
            (kill-buffer indirect)))))))


;;;; The mode's initialize and shutdown paths

(ert-deftest scholia-core-mode-toggle-saves-and-restores-annotations ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-core-test--source
      (let ((file (buffer-file-name buffer)))
        (setq-local scholia-session "toggle")
        (scholia-mode 1)
        (goto-char 8)
        (scholia-annotate "on beta")
        (goto-char 13)
        (scholia-annotate "on gamma")
        (goto-char 26)
        (scholia-annotate "on epsilon")
        (goto-char 13)
        (scholia-delete-annotation)
        (should (file-equal-p (file-name-directory (scholia-session-file))
                              scholia-session-directory))
        (should (equal (file-name-base (scholia-session-file)) "toggle"))
        (let ((scholia-autosave t))
          (scholia-mode -1))
        (should-not (scholia-buffer-chains))
        (should (equal (scholia-core-test--texts
                        (scholia-core-test--stored (scholia-session-file) file))
                       '("on beta" "on epsilon")))
        (setq scholia--colors-index-counter 0)
        (scholia-mode 1)
        (let ((chains (scholia-buffer-chains)))
          (should (equal (mapcar #'length chains) '(1 1)))
          (scholia-test-should-overlay-range (car (nth 0 chains)) 7 11)
          (scholia-test-should-overlay-range (car (nth 1 chains)) 25 32)
          (should (equal (scholia-core-test--chain-text (nth 1 chains))
                         "on epsilon"))
          (should (equal (scholia-chain-color-index (nth 0 chains)) 0))
          (should (equal (scholia-chain-color-index (nth 1 chains)) 2)))
        (scholia-mode 1)
        (scholia-mode 1)
        (should (equal (mapcar #'length (scholia-buffer-chains)) '(1 1)))
        (scholia-save-annotations)
        (let ((stored (scholia-core-test--stored (scholia-session-file) file)))
          (should (equal (scholia-core-test--texts stored)
                         '("on beta" "on epsilon")))
          (should (equal (length (delete-dups
                                  (mapcar #'scholia-db-annotation-id stored)))
                         2)))
        (goto-char 19)
        (scholia-annotate "on delta")
        (should (equal (scholia-chain-color-index (scholia-chain-at 19)) 3))))))

(ert-deftest scholia-core-shutdown-saves-only-when-told-to-and-drops-the-kill-hook ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-core-test--source
      (setq-local scholia-session "shutdown")
      (let ((file (buffer-file-name buffer))
            (session (scholia-session-file)))
        (should (eq (default-value 'scholia-autosave) t))
        (scholia-mode 1)
        (goto-char 8)
        (scholia-annotate "dropped by the caller")
        (scholia-shutdown nil)
        (should-not (scholia-buffer-chains))
        (should-not (scholia-core-test--stored session file))
        (scholia-mode 1)
        (goto-char 13)
        (scholia-annotate "kept by the caller")
        (should (scholia-core-test--change-hooks))
        (cl-letf (((symbol-function 'scholia-db-save)
                   (lambda (&rest _)
                     (signal 'scholia-error '("session directory is read-only")))))
          (ignore-errors (scholia-shutdown t))
          (should (equal (mapcar #'length (scholia-buffer-chains)) '(1)))
          (should (scholia-core-test--change-hooks))
          (should-not (scholia-core-test--stored session file))
          (ignore-errors (scholia-mode -1))
          (should scholia-mode)
          (should (equal (mapcar #'length (scholia-buffer-chains)) '(1)))
          (should (scholia-core-test--change-hooks))
          (should (memq #'scholia-core--save-on-kill kill-buffer-hook)))
        (should (string-match-p
                 "kill"
                 (documentation-property 'scholia-autosave
                                         'variable-documentation)))
        (scholia-shutdown t)
        (should-not (scholia-buffer-chains))
        (should-not (scholia-core-test--change-hooks))
        (should (equal (scholia-core-test--texts
                        (scholia-core-test--stored session file))
                       '("kept by the caller")))
        (scholia-mode 1)
        (should (equal (mapcar #'length (scholia-buffer-chains)) '(1)))
        (goto-char 26)
        (scholia-annotate "dropped by the mode")
        (let ((scholia-autosave nil))
          (scholia-mode -1))
        (should-not (scholia-buffer-chains))
        (should (equal (scholia-core-test--texts
                        (scholia-core-test--stored session file))
                       '("kept by the caller")))
        (kill-buffer buffer)
        (should (equal (scholia-core-test--texts
                        (scholia-core-test--stored session file))
                       '("kept by the caller")))
        (scholia-test-with-temp-file-buffer killed scholia-core-test--source
          (setq-local scholia-session "shutdown")
          (let ((killed-file (buffer-file-name killed)))
            (scholia-mode 1)
            (goto-char 13)
            (scholia-annotate "kept by the kill hook")
            (kill-buffer killed)
            (should (equal (scholia-core-test--texts
                            (scholia-core-test--stored session killed-file))
                           '("kept by the kill hook")))))))))

(ert-deftest scholia-core-saving-carries-the-stored-send-history-forward ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-core-test--source
      (setq-local scholia-session "sends")
      (let ((file (buffer-file-name buffer))
            (session (scholia-session-file)))
        (scholia-mode 1)
        (goto-char 13)
        (scholia-annotate "on gamma")
        (scholia-save-annotations)
        (let ((stored (scholia-core-test--stored session file)))
          (should (equal (length stored) 1))
          (should-not (scholia-db-annotation-sends (car stored)))
          (scholia-db-save session file
                           (list (scholia-db-annotation-add-send
                                  (car stored)
                                  (scholia-db-make-send :kind 'agent
                                                        :target "claude-2"
                                                        :label "claude · claude-2"
                                                        :herdr-session "shared"
                                                        :format 'rustc
                                                        :scope 'file)))
                           (scholia-buffer-checksum)))
        (goto-char 26)
        (scholia-annotate "on epsilon")
        (scholia-save-annotations)
        (let* ((stored (scholia-core-test--stored session file))
               (gamma (seq-find (lambda (annotation)
                                  (equal (scholia-db-annotation-text annotation)
                                         "on gamma"))
                                stored)))
          (should (equal (scholia-core-test--texts stored)
                         '("on epsilon" "on gamma")))
          (should (equal (mapcar #'scholia-db-send-target
                                 (scholia-db-annotation-sends gamma))
                         '("claude-2"))))))))


(ert-deftest scholia-core-quitting-emacs-stores-every-annotated-buffer ()
  "A buffer that cannot be stored must not keep Emacs from quitting.
`debug-on-error' is bound here because the guard has to hold whatever the
user has set: `with-demoted-errors' expands to `condition-case-unless-debug'
and is inert under it, which is how this passed everywhere except the one
Emacs whose ERT sets the variable."
  (scholia-test-with-session-directory
    (let ((kill-emacs-hook nil)
          (debug-on-error t))
      (scholia-test-with-temp-file-buffer one scholia-core-test--source
        (setq-local scholia-session "quit")
        (scholia-mode 1)
        (goto-char 8)
        (scholia-annotate "in the buffer left open")
        (let ((session (scholia-session-file))
              (first-file (buffer-file-name one)))
          (scholia-test-with-temp-file-buffer two scholia-core-test--source
            (setq-local scholia-session "quit")
            (scholia-mode 1)
            (goto-char 13)
            (scholia-annotate "in the other buffer left open")
            (let ((second-file (buffer-file-name two)))
              (should (memq #'scholia-core--save-all kill-emacs-hook))
              (should-not (file-exists-p session))
              (let ((scholia-autosave nil))
                (scholia-core--save-all))
              (should-not (file-exists-p session))
              (scholia-core--save-all)
              (should (equal (scholia-core-test--texts
                              (scholia-core-test--stored session first-file))
                             '("in the buffer left open")))
              (should (equal (scholia-core-test--texts
                              (scholia-core-test--stored session second-file))
                             '("in the other buffer left open")))
              (cl-letf (((symbol-function 'scholia-db-save)
                         (lambda (&rest _)
                           (signal 'scholia-error '("read-only")))))
                (scholia-core--save-all))
              (let ((scholia-autosave nil))
                (scholia-core--save-all))
              (scholia-mode -1)
              (should (memq #'scholia-core--save-all kill-emacs-hook))))
          (scholia-mode -1)
          (should-not (memq #'scholia-core--save-all kill-emacs-hook))
          (scholia-mode 1)
          (should (memq #'scholia-core--save-all kill-emacs-hook))
          (kill-buffer one)
          (should-not (memq #'scholia-core--save-all kill-emacs-hook)))))))

(ert-deftest scholia-core-a-signalling-load-keeps-the-record-and-the-save ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-core-test--source
      (setq-local scholia-session "signalling-load")
      (let ((file (buffer-file-name buffer))
            (session (scholia-session-file)))
        (scholia-db-save session file
                         (list (scholia-db-make-annotation
                                "id-gamma" "on gamma" 12 17 "gamma")
                               (scholia-db-make-annotation
                                "id-reply" "the answer" nil nil nil nil nil
                                "id-gamma"))
                         "a checksum this buffer does not have")
        (cl-letf (((symbol-function 'scholia-db-buffer-annotations)
                   (lambda (&rest _)
                     (signal 'scholia-error '("relocation failed")))))
          (should-error (scholia-mode 1)))
        (should (memq #'scholia-core--save-on-kill kill-buffer-hook))
        (should-not (scholia-buffer-chains))
        (scholia-save-annotations)
        (let ((stored (scholia-core-test--stored session file)))
          (should (equal (scholia-core-test--texts stored)
                         '("on gamma" "the answer")))
          (should (equal (length stored) 2)))))))

(ert-deftest scholia-core-a-restore-that-signals-keeps-what-it-did-not-draw ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-core-test--source
      (setq-local scholia-session "signalling-restore")
      (let ((file (buffer-file-name buffer))
            (session (scholia-session-file)))
        (scholia-db-save
         session file
         (list (scholia-db-make-annotation "id-a" "on alpha" 1 6 "alpha" 0)
               (scholia-db-make-annotation "id-b" "on gamma" 12 17 "gamma"
                                           "not an index")
               (scholia-db-make-annotation "id-c" "on epsilon" 25 32 "epsilon" 2))
         (scholia-buffer-checksum))
        (should-error (scholia-mode 1))
        (should (equal (mapcar #'scholia-core-test--chain-text
                               (scholia-buffer-chains))
                       '("on alpha")))
        (scholia-save-annotations)
        (should (equal (scholia-core-test--texts
                        (scholia-core-test--stored session file))
                       '("on alpha" "on epsilon" "on gamma")))))))

(ert-deftest scholia-core-a-save-keeps-what-drift-could-not-place ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-core-test--source
      (setq-local scholia-session "drift")
      (setq-local scholia-use-messages t)
      (let ((file (buffer-file-name buffer))
            (session (scholia-session-file)))
        (scholia-mode 1)
        (goto-char 8)
        (scholia-annotate "on beta")
        (goto-char 13)
        (scholia-annotate "on gamma")
        (let ((scholia-autosave t))
          (scholia-mode -1))
        (delete-region 7 11)
        (goto-char 7)
        (insert "iota")
        (let ((reported (scholia-core-test--reported (scholia-mode 1))))
          (should (stringp reported))
          (should (string-match-p "beta" reported)))
        (should (equal (mapcar #'length (scholia-buffer-chains)) '(1)))
        (scholia-save-annotations)
        (should (equal (scholia-core-test--texts
                        (scholia-core-test--stored session file))
                       '("on beta" "on gamma")))))))

(ert-deftest scholia-core-a-buffer-visiting-no-file-stores-a-generic-source ()
  (scholia-test-with-session-directory
    (with-temp-buffer
      (insert "alpha beta\n")
      (setq-local scholia-session "no-file")
      (let ((session (scholia-session-file)))
        (scholia-mode 1)
        (goto-char (point-min))
        (scholia-annotate "on alpha")
        (scholia-save-annotations)
        (should (file-exists-p session))
        (let ((sources (scholia-db-files session)))
          (should (= (length sources) 1))
          (should (equal (scholia-core-test--texts
                          (scholia-core-test--stored session (car sources)))
                         '("on alpha"))))
        (let ((scholia-autosave t))
          (scholia-mode -1))))))


;;;; The identity a chain keeps

(ert-deftest scholia-core-mints-a-distinct-id-for-every-annotation-of-a-burst ()
  (let ((ids (cl-loop repeat 2000 collect (scholia-core--make-id))))
    (should (equal (length ids) 2000))
    (should (equal (length (delete-dups (copy-sequence ids))) 2000))))


;;;; Reading overlay state back

(ert-deftest scholia-core-reads-back-chains-and-colors-and-disarms-rechaining ()
  (scholia-test-with-temp-file-buffer _buffer scholia-core-test--source
    (let ((spanning (scholia-create-chain 7 17 "spanning"))
          (single (scholia-create-chain 25 32 "single"))
          (restored (scholia-create-chain 33 37 "restored" 4)))
      (should (equal (scholia-chain-color-index spanning) 0))
      (should (equal (scholia-chain-color-index single) 1))
      (should (equal (scholia-chain-color-index restored) 4))
      (let ((chains (scholia-buffer-chains)))
        (should (equal (mapcar #'length chains) '(2 1 1)))
        (should (equal (nth 0 chains) spanning))
        (should (equal (nth 1 chains) single))
        (should (equal (nth 2 chains) restored)))
      (save-excursion
        (goto-char 28)
        (insert "\n"))
      (should (equal (mapcar #'length (scholia-buffer-chains)) '(2 2 1)))
      (scholia-disarm-rechaining)
      (save-excursion
        (goto-char 14)
        (insert "\n"))
      (should (equal (mapcar #'length (scholia-buffer-chains)) '(2 2 1))))))


;;;; The stored annotation the lifecycle hands the record store

(ert-deftest scholia-core-db-annotations-carry-color-and-position ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-core-test--source
      (let* ((file (buffer-file-name buffer))
             (session (expand-file-name "made.eld" scholia-session-directory))
             (annotation (scholia-db-make-annotation
                          "id-one" "on gamma" 12 17 "gamma" 3 :inline))
             (reply (scholia-db-make-annotation
                     "id-two" "the answer" nil nil nil 3 :inline "id-one"))
             (plain (scholia-db-make-annotation
                     "id-three" "on alpha" 1 6 "alpha")))
        (should (equal (scholia-db-annotation-id annotation) "id-one"))
        (should (equal (scholia-db-annotation-text annotation) "on gamma"))
        (should (equal (scholia-db-annotation-interval annotation) '(12 . 17)))
        (should (equal (scholia-db-annotation-annotated-text annotation) "gamma"))
        (should (equal (scholia-db-annotation-color annotation) 3))
        (should (equal (scholia-db-annotation-position annotation) :inline))
        (should-not (scholia-db-annotation-reply-p annotation))
        (should (equal (scholia-db-annotation-color plain) 0))
        (should (equal (scholia-db-annotation-position plain) :margin))
        (should (equal (scholia-db-annotation-reply-to reply) "id-one"))
        (should (scholia-db-annotation-reply-p reply))
        (scholia-db-save session file (list annotation reply) "checksum-one")
        (let* ((stored (scholia-core-test--stored session file))
               (loaded (scholia-core-test--with-id "id-one" stored)))
          (should (equal (scholia-core-test--texts stored)
                         '("on gamma" "the answer")))
          (should (equal (scholia-db-annotation-color loaded) 3))
          (should (equal (scholia-db-annotation-position loaded) :inline))
          (should (equal (scholia-db-annotation-line loaded) 2))
          (should (equal (scholia-db-annotation-column loaded) 0))
          (should (equal (scholia-db-annotation-line-text loaded) "gamma delta")))))))

(ert-deftest scholia-multisession-non-file-source-saves-reloads-and-exports-fallbacks ()
  (let* ((globals '(scholia-session
                    scholia-visible-sessions
                    scholia-autosave
                    scholia-source-snapshot-mode
                    scholia-source-snapshot-limit))
         (snapshot (mapcar (lambda (symbol)
                             (list symbol
                                   (boundp symbol)
                                   (and (boundp symbol) (default-value symbol))))
                           globals)))
    (unwind-protect
        (progn
          (set-default 'scholia-session nil)
          (set-default 'scholia-visible-sessions nil)
          (set-default 'scholia-autosave nil)
          (set-default 'scholia-source-snapshot-mode 'bounded-full)
          (cl-labels
              ((exercise (name source limit expected-status omitted)
                 (scholia-test-with-session-directory
                   (set-default 'scholia-source-snapshot-limit limit)
                   (let (session)
                     (cl-letf (((symbol-function 'completing-read)
                                (lambda (&rest _)
                                  (error "A zero-config source asked for a session")))
                               ((symbol-function 'read-string)
                                (lambda (&rest _)
                                  (error "A zero-config source asked for input")))
                               ((symbol-function 'y-or-n-p)
                                (lambda (&rest _)
                                  (error "A zero-config source asked for confirmation"))))
                       (with-temp-buffer
                         (rename-buffer name t)
                         (insert source)
                         (text-mode)
                         (scholia-mode 1)
                         (goto-char (point-min))
                         (scholia-annotate "memory note")
                         (scholia-save-annotations)
                         (setq session (scholia-session-file))
                         (should (= 1 (length (scholia-db-files session))))
                         (scholia-mode -1)
                         (should-not (scholia-buffer-chains))
                         (scholia-mode 1)
                         (should (equal (mapcar #'scholia-core-test--chain-text
						(scholia-buffer-chains))
                                        '("memory note")))
                         (scholia-mode -1)))
                     (let ((output (scholia-export-session "default" nil 'integrate)))
                       (should (string-match-p (regexp-quote name) output))
                       (should (string-match-p "text-mode" output))
                       (should (string-match-p expected-status output))
                       (should (string-match-p "alpha beta" output))
                       (if omitted
                           (should-not (string-match-p (regexp-quote omitted) output))
                         (should (string-match-p "FULL-SNAPSHOT-TAIL" output))))))))
            (exercise "scholia-full-memory"
                      "alpha beta\nFULL-SNAPSHOT-TAIL\n"
                      1024 "\\bfull\\b" nil)
            (exercise "scholia-excerpt-memory"
                      "alpha beta\ncontent beyond the bounded snapshot\nEXCERPT-OMITTED-TAIL\n"
                      16 "\\bexcerpt\\b.*\\btruncated\\b"
                      "EXCERPT-OMITTED-TAIL")))
      (dolist (entry snapshot)
        (if (nth 1 entry)
            (set-default (nth 0 entry) (nth 2 entry))
          (makunbound (nth 0 entry)))))))

(ert-deftest scholia-core-annotating-enables-the-mode ()
  "Annotating a buffer turns the mode on, so the buffer is tracked.
The mode owns saving on kill, re-chaining as the file is edited and the
mark on the annotation point is in, so an annotation made without it
would be a note nothing looks after."
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer _buffer scholia-core-test--source
      (setq-local scholia-session "annotate-enables")
      (setq-local scholia-project-root-function (lambda () nil))
      (should-not scholia-mode)
      (goto-char 1)
      (scholia-annotate "a note")
      (should scholia-mode)
      (should (scholia-buffer-chains)))))

(ert-deftest scholia-core-replies-to-a-reply-once-there-is-one ()
  "The first reply answers the annotation; later ones ask what they answer.
A reply to a reply is stored against it and drawn a level deeper, after
the replies already under it."
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer _buffer scholia-core-test--source
      (setq-local scholia-session "nested")
      (setq-local scholia-project-root-function (lambda () nil))
      (scholia-mode 1)
      (goto-char 1)
      (scholia-annotate "the note")
      (let* ((chain (scholia-core--select-chain))
             (root (scholia-core--chain-id chain))
             (key (overlay-get (car chain) 'scholia--chain-id))
             (asked 0))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt candidates &rest _)
                     (setq asked (1+ asked))
                     (car (nth 1 candidates))))
                  ((symbol-function 'read-string) (lambda (&rest _) "first")))
          (call-interactively #'scholia-reply-to)
          (should (= asked 0))
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "second")))
            (call-interactively #'scholia-reply-to))
          (should (= asked 1)))
        (let ((replies (alist-get key scholia--replies)))
          (should (equal (mapcar (lambda (entry)
                                   (cons (car entry)
                                         (scholia-db-annotation-text (cdr entry))))
                                 replies)
                         '((1 . "first") (2 . "second"))))
          (should (equal (scholia-db-annotation-reply-to (cdr (nth 0 replies))) root))
          (should (equal (scholia-db-annotation-reply-to (cdr (nth 1 replies)))
                         (scholia-db-annotation-id (cdr (nth 0 replies))))))
        (scholia-reply-to "third" root)
        (should (equal (mapcar #'car (alist-get key scholia--replies)) '(1 2 1)))
        (scholia-mode -1)
        (scholia-mode 1)
        (let ((chain (car (scholia-buffer-chains))))
          (should (equal (mapcar (lambda (entry)
                                   (cons (car entry)
                                         (scholia-db-annotation-text (cdr entry))))
                                 (alist-get (overlay-get (car chain) 'scholia--chain-id)
                                            scholia--replies))
                         '((1 . "first") (2 . "second") (1 . "third")))))))))

(defun scholia-core-test--pane-texts (chain)
  "Return the text and face of every pane shown for CHAIN."
  (mapcar (lambda (pane) (list (cera-pane-text pane) (cera-pane-face pane)))
          (cera-shown-panes
           (alist-get (overlay-get (car chain) 'scholia--chain-id)
                      scholia-render--shown))))

(ert-deftest scholia-core-authors-are-recorded-and-shown-only-when-on ()
  "Authors are stored and drawn, without a face, only while the flag is on.
The toggle flips it everywhere and draws the buffer again."
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer _buffer scholia-core-test--source
      (setq-local scholia-session "authors")
      (setq-local scholia-project-root-function (lambda () nil))
      (let ((scholia-annotation-authors nil)
            (scholia-author-icon nil)
            (scholia-author-icon-fallback nil))
        (cl-letf (((symbol-function 'scholia-core--author)
                   (lambda () "Ada <ada@example.org>")))
          (scholia-mode 1)
          (goto-char 1)
          (scholia-annotate "unsigned")
          (let ((chain (scholia-core--select-chain)))
            (should (= 1 (length (scholia-core-test--pane-texts chain))))
            (should-not (scholia-db-annotation-author
                         (scholia-core--chain-annotation chain)))
            (scholia-delete-annotation))
          (setq scholia-annotation-authors t)
          (scholia-annotate "signed")
          (let ((chain (scholia-core--select-chain)))
            (scholia-reply-to "answered")
            (should (equal (scholia-db-annotation-author
                            (scholia-core--chain-annotation chain))
                           "Ada <ada@example.org>"))
            (should (equal (scholia-core-test--pane-texts chain)
                           `(("signed" ,(cadr (car (scholia-core-test--pane-texts chain))))
                             ("Ada <ada@example.org>" nil)
                             ("answered" ,(cadr (nth 2 (scholia-core-test--pane-texts chain))))
                             ("Ada <ada@example.org>" nil))))
            (scholia-mode -1)
            (scholia-mode 1)
            (let ((chain (car (scholia-buffer-chains))))
              (should (= 4 (length (scholia-core-test--pane-texts chain))))
              (scholia-toggle-annotation-authors)
              (should-not scholia-annotation-authors)
              (should (equal (mapcar #'car (scholia-core-test--pane-texts chain))
                             '("signed" "answered"))))))))))

(provide 'scholia-core-test)
;;; scholia-core-test.el ends here
