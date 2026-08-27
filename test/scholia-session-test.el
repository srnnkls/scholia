;;; scholia-session-test.el --- Tests for the scholia session store  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Covers `scholia-session', the named session store, and the three public
;; entry points `scholia-db' grows for it: a row writer that persists the
;; record it is handed, a session-file constructor, and a header name
;; setter.  Persisted state is read back through the db API rather than
;; through the stored representation (INV-12).

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'scholia-test-helper)

(let ((load-prefer-newer t))
  (require 'scholia-vars nil t)
  (require 'scholia-overlay nil t)
  (require 'scholia-db nil t)
  (require 'scholia-store nil t)
  (require 'scholia-core nil t)
  (require 'scholia-session nil t))

(defconst scholia-session-test--source
  "alpha beta\ngamma delta\n"
  "Two lines the buffer fixtures annotate.
Point at 1 sits on \"alpha\" and point at 12 on \"gamma\".")

(defconst scholia-session-test--globals
  '(scholia-session
    scholia-project-sessions
    scholia-project-root-function
    scholia-autosave
    scholia-session-switch-hook
    scholia-session-state-file)
  "Global state the session fixtures overwrite and put back.")

(defun scholia-session-test--snapshot ()
  "Return the current value of every symbol of `scholia-session-test--globals'.
A symbol that is unbound is recorded as unbound, so a fixture running
before the module defines it leaves it undefined again."
  (mapcar (lambda (symbol)
            (list symbol
                  (boundp symbol)
                  (and (boundp symbol) (default-value symbol))))
          scholia-session-test--globals))

(defun scholia-session-test--restore (snapshot)
  "Put every symbol SNAPSHOT records back the way it found it."
  (dolist (entry snapshot)
    (if (nth 1 entry)
        (set-default (nth 0 entry) (nth 2 entry))
      (makunbound (nth 0 entry)))))

(defmacro scholia-session-test--with-state (&rest body)
  "Evaluate BODY with a session directory and the session globals of its own.
The project bindings start empty, the project root function answers with
nothing, `scholia-autosave' is nil so that saving is only ever something
a command did on purpose, and the state file lives inside the session
directory.  Everything is put back when BODY exits, however it exits."
  (declare (indent 0) (debug body))
  (let ((snapshot (make-symbol "snapshot")))
    `(scholia-test-with-session-directory
       (let ((,snapshot (scholia-session-test--snapshot)))
         (unwind-protect
             (progn
               (set-default 'scholia-session nil)
               (set-default 'scholia-project-sessions nil)
               (set-default 'scholia-project-root-function (lambda () nil))
               (set-default 'scholia-autosave nil)
               (set-default 'scholia-session-switch-hook nil)
               (set-default 'scholia-session-state-file
                            (expand-file-name "assignments.eld"
                                              scholia-session-directory))
               ,@body)
           (scholia-session-test--restore ,snapshot))))))

(defmacro scholia-session-test--reported (&rest body)
  "Evaluate BODY and return everything it warned or reported, oldest first."
  (declare (indent 0) (debug body))
  (let ((collected (make-symbol "collected")))
    `(let ((,collected nil))
       (cl-letf (((symbol-function 'message)
                  (lambda (format &rest arguments)
                    (when format
                      (push (apply #'format-message format arguments)
                            ,collected))))
                 ((symbol-function 'display-warning)
                  (lambda (_type warning &rest _)
                    (push (format "%s" warning) ,collected))))
         ,@body)
       (nreverse ,collected))))

(defmacro scholia-session-test--without-prompting (&rest body)
  "Evaluate BODY with every prompting entry point turned into a failure."
  (declare (indent 0) (debug body))
  `(cl-letf (((symbol-function 'completing-read)
              (lambda (&rest _) (error "Resolution asked the user")))
             ((symbol-function 'read-string)
              (lambda (&rest _) (error "Resolution asked the user")))
             ((symbol-function 'y-or-n-p)
              (lambda (&rest _) (error "Resolution asked the user"))))
     ,@body))

(defun scholia-session-test--annotation (id text &optional beg end)
  "Return a stored annotation carrying ID and TEXT over BEG to END.
BEG and END default to the bounds of \"alpha\".  Its source context names
a line no fixture buffer holds, so a write that snapshots it afresh
against the current buffer is visible in the values that come back."
  (list :id id
        :text text
        :beg (or beg 1)
        :end (or end 6)
        :annotated-text "alpha"
        :line 42
        :line-text "the whole stored line"
        :column 7
        :end-column 12
        :color 0
        :position :margin
        :reply-to nil
        :sends nil))

(defun scholia-session-test--seed (name file annotations)
  "Store ANNOTATIONS as the record FILE keys in the session called NAME."
  (let ((session-file (scholia-session-file name)))
    (scholia-db-create-session session-file)
    (scholia-db-store-record
     session-file (scholia-db-make-record file annotations "seeded"))))

(defun scholia-session-test--stored (name file)
  "Return the annotations the session called NAME carries for FILE."
  (scholia-db-record-annotations
   (scholia-db-record (scholia-session-file name) file)))

(defun scholia-session-test--stamp-created (session-file moment)
  "Set SESSION-FILE's header `:created' to MOMENT, leaving the rest alone.
`:created' is written by `format-time-string' at one-second resolution, so
a header minted fresh during a test compares `equal' to the one the test
started with.  A value from outside the run is what tells a header that
survived from one that was rebuilt underneath the assertion."
  (let ((store (scholia-store-open session-file)))
    (unwind-protect
        (scholia-store-with-transaction store
          (scholia-store-put-session
           store
           (plist-put (plist-get (scholia-store-read store) :session)
                      :created moment)))
      (scholia-store-close store))))

(defun scholia-session-test--journals ()
  "Return the WAL and shared-memory files left in the session directory."
  (directory-files scholia-session-directory nil "-\\(wal\\|shm\\)\\'"))

(defun scholia-session-test--texts (annotations)
  "Return the notes ANNOTATIONS carry, sorted."
  (sort (mapcar #'scholia-db-annotation-text annotations) #'string<))

(defun scholia-session-test--chain-texts ()
  "Return the notes the chains of this buffer carry, in buffer order."
  (mapcar (lambda (chain) (overlay-get (car chain) 'scholia-annotation))
          (scholia-buffer-chains)))

(defun scholia-session-test--first-overlay ()
  "Return the first overlay of the first chain of this buffer."
  (car (car (scholia-buffer-chains))))


;;;; What scholia-db grows for the session store

(ert-deftest scholia-session-db-stores-the-record-it-is-handed ()
  (scholia-session-test--with-state
    (scholia-test-with-temp-file-buffer _buffer scholia-session-test--source
      (let ((file "/nowhere/that/exists/gone.txt")
            (session-file (scholia-session-file "kept")))
        (scholia-db-create-session session-file)
        (scholia-db-store-record
         session-file
         (scholia-db-make-record
          file
          (list (scholia-session-test--annotation "one" "held as it was"))
          "the stored checksum"))
        (let* ((record (scholia-db-record session-file file))
               (stored (car (scholia-db-record-annotations record))))
          (should (equal (scholia-db-annotation-line stored) 42))
          (should (equal (scholia-db-annotation-line-text stored)
                         "the whole stored line"))
          (should (equal (scholia-db-annotation-column stored) 7))
          (should (equal (scholia-db-annotation-end-column stored) 12))
          (should (equal (scholia-db-record-checksum record)
                         "the stored checksum"))
          (should (equal (scholia-db-session-name session-file) "kept")))
        (scholia-db-store-record (scholia-session-file "orphan")
                                 (scholia-db-make-record file nil "none"))
        (should (equal (scholia-db-session-name
                        (scholia-session-file "orphan"))
                       "orphan"))))))

(ert-deftest scholia-session-db-mints-and-renames-a-session-header ()
  "Creating twice keeps the first moment, and renaming changes only the name.
`:created' is stamped from outside the run rather than read back and
compared, because the timestamp has one-second resolution and this test
takes tens of milliseconds: a header rebuilt wholesale mid-test carries a
string `equal' to the one it replaced, so comparing a header to itself
admits the writer it exists to refuse."
  (scholia-session-test--with-state
    (let ((session-file (scholia-session-file "fresh"))
          (born "2020-01-01T00:00:00+0000"))
      (scholia-db-create-session session-file)
      (should (file-exists-p session-file))
      (should (equal (scholia-db-session-name session-file) "fresh"))
      (should (stringp (scholia-db-session-created session-file)))
      (should-not (scholia-db-files session-file))
      (scholia-session-test--stamp-created session-file born)
      (scholia-session-test--seed
       "fresh" "/nowhere/kept.txt"
       (list (scholia-session-test--annotation "one" "already stored")))
      (scholia-db-create-session session-file)
      (should (equal (scholia-session-test--texts
                      (scholia-session-test--stored "fresh"
                                                    "/nowhere/kept.txt"))
                     '("already stored")))
      (should (equal (scholia-db-session-created session-file) born))
      (let ((files (scholia-db-files session-file)))
        (should (equal (scholia-db-session-name session-file) "fresh"))
        (scholia-db-set-session-name session-file "other")
        (should (equal (scholia-db-session-name session-file) "other"))
        (should (equal (scholia-db-session-created session-file) born))
        (should (equal (scholia-db-files session-file) files))
        (should (equal (scholia-session-test--texts
                        (scholia-session-test--stored "fresh"
                                                      "/nowhere/kept.txt"))
                       '("already stored")))))))


;;;; Resolution

(ert-deftest scholia-session-resolves-buffer-local-then-project-then-default ()
  (scholia-session-test--with-state
    (let ((root (file-name-as-directory (make-temp-file "scholia-root-" t))))
      (unwind-protect
          (let ((detour (concat root ".." "/"
                                (file-name-nondirectory
                                 (directory-file-name root)))))
            (set-default 'scholia-project-root-function (lambda () root))
            (set-default 'scholia-project-sessions (list (cons detour "project")))
            (set-default 'scholia-session "global")
            (with-temp-buffer
              (setq-local scholia-session "local")
              (should (equal (scholia-session-name) "local"))
              (should (equal (scholia-session-file)
                             (expand-file-name "local.eld"
                                               scholia-session-directory)))
              (kill-local-variable 'scholia-session)
              (should (equal (scholia-session-name) "project"))
              (set-default 'scholia-project-sessions
                           (list (cons (directory-file-name root) "project")))
              (should (equal (scholia-session-name) "project"))
              (set-default 'scholia-project-sessions nil)
              (should (equal (scholia-session-name) "global"))
              (set-default 'scholia-session nil)
              (should (equal (scholia-session-name) "default"))))
        (delete-directory root t)))))

(ert-deftest scholia-session-project-binding-to-a-missing-file-falls-back ()
  "Opening a file whose project names a session that is gone falls back.
The fallback belongs to the open, not to resolution: `scholia-session-name'
answers with the binding it was given, and only enabling the mode — which
is what opening the file does — finds the session file missing, reports it
by name and binds this buffer to the global default instead.  Putting the
check in the resolver would stat the session file and repeat the warning on
every save and every load, since `scholia-session-file' resolves each time."
  (scholia-session-test--with-state
    (let ((root (file-name-as-directory (make-temp-file "scholia-root-" t))))
      (unwind-protect
          (progn
            (set-default 'scholia-project-root-function (lambda () root))
            (set-default 'scholia-project-sessions (list (cons root "vanished")))
            (set-default 'scholia-session "global")
            (scholia-session-create "global")
            (scholia-test-with-temp-file-buffer buffer scholia-session-test--source
              (should (equal (scholia-session-name) "vanished"))
              (let ((reported (scholia-session-test--reported
                                (scholia-session-test--without-prompting
                                  (scholia-mode 1)))))
                (should (seq-find (lambda (line)
                                    (string-match-p "vanished" line))
                                  reported)))
              (should (equal (scholia-session-name) "global"))
              (should (local-variable-p 'scholia-session))
              (should (equal (default-value 'scholia-session) "global"))
              (goto-char 1)
              (scholia-annotate "lands in the default")
              (scholia-save-annotations)
              (should (equal (scholia-session-test--texts
                              (scholia-session-test--stored
                               "global" (buffer-file-name buffer)))
                             '("lands in the default")))
              (should-not (file-exists-p (scholia-session-file "vanished")))))
        (delete-directory root t)))))


;;;; Creating

(ert-deftest scholia-session-create-writes-a-header-and-nothing-else ()
  (scholia-session-test--with-state
    (set-default 'scholia-session "global")
    (with-temp-buffer
      (scholia-session-create "notes")
      (should (file-exists-p (scholia-session-file "notes")))
      (should (equal (scholia-db-session-name (scholia-session-file "notes"))
                     "notes"))
      (should-not (scholia-db-files (scholia-session-file "notes")))
      (should (equal (default-value 'scholia-session) "global"))
      (should (equal (scholia-session-name) "global"))
      (should (member "notes" (scholia-session-list)))
      (should-error (scholia-session-create "sub/escaped") :type 'scholia-error)
      (should-not (file-exists-p (expand-file-name "sub/escaped.eld"
                                                   scholia-session-directory)))
      (scholia-session-test--seed
       "notes" "/nowhere/kept.txt"
       (list (scholia-session-test--annotation "one" "already stored")))
      (should-error (scholia-session-create "notes") :type 'scholia-error)
      (should (equal (scholia-session-test--texts
                      (scholia-session-test--stored "notes" "/nowhere/kept.txt"))
                     '("already stored"))))))


;;;; Switching

(ert-deftest scholia-session-switch-saves-and-redraws-both-halves ()
  (scholia-session-test--with-state
    (set-default 'scholia-session "alpha")
    (scholia-session-create "alpha")
    (scholia-session-create "beta")
    (scholia-test-with-temp-file-buffer outgoing scholia-session-test--source
      (scholia-test-with-temp-file-buffer incoming scholia-session-test--source
        (let ((outgoing-file (buffer-file-name outgoing))
              (incoming-file (buffer-file-name incoming))
              (defaults-at-hook nil)
              (incoming-overlay nil))
          (with-current-buffer outgoing
            (scholia-mode 1)
            (goto-char 1)
            (scholia-annotate "on the outgoing buffer"))
          (with-current-buffer incoming
            (setq-local scholia-session "beta")
            (scholia-mode 1)
            (goto-char 1)
            (scholia-annotate "on the incoming buffer")
            (setq incoming-overlay (scholia-session-test--first-overlay)))
          (add-hook 'scholia-session-switch-hook
                    (lambda ()
                      (push (default-value 'scholia-session) defaults-at-hook)))
          (should-error (scholia-session-switch "never-created")
                        :type 'scholia-error)
          (should (equal (default-value 'scholia-session) "alpha"))
          (should-not defaults-at-hook)
          (scholia-session-switch "beta")
          (should (equal (default-value 'scholia-session) "beta"))
          (should (equal defaults-at-hook '("beta")))
          (should (equal (scholia-session-test--texts
                          (scholia-session-test--stored "beta" incoming-file))
                         '("on the incoming buffer")))
          (should (equal (scholia-session-test--texts
                          (scholia-session-test--stored "alpha" outgoing-file))
                         '("on the outgoing buffer")))
          (with-current-buffer incoming
            (should (equal (scholia-session-test--chain-texts)
                           '("on the incoming buffer")))
            (should-not (scholia-annotation-p incoming-overlay))
            (should (local-variable-p 'scholia-session))
            (should (equal scholia-session "beta"))
            (should scholia-mode))
          (with-current-buffer outgoing
            (should-not (scholia-buffer-chains))
            (should scholia-mode)))))))

(ert-deftest scholia-session-switch-leaves-a-project-bound-buffer-alone ()
  (scholia-session-test--with-state
    (let ((root (file-name-as-directory (make-temp-file "scholia-root-" t))))
      (unwind-protect
          (progn
            (set-default 'scholia-session "alpha")
            (set-default 'scholia-project-root-function (lambda () root))
            (set-default 'scholia-project-sessions (list (cons root "gamma")))
            (scholia-session-create "alpha")
            (scholia-session-create "beta")
            (scholia-session-create "gamma")
            (scholia-test-with-temp-file-buffer bound scholia-session-test--source
              (let ((file (buffer-file-name bound)))
                (scholia-mode 1)
                (goto-char 1)
                (scholia-annotate "on the project buffer")
                (let ((overlay (scholia-session-test--first-overlay)))
                  (scholia-session-switch "beta")
                  (should (equal (default-value 'scholia-session) "beta"))
                  (should (equal (scholia-session-name) "gamma"))
                  (should (scholia-annotation-p overlay))
                  (should (eq overlay (scholia-session-test--first-overlay)))
                  (should (equal (scholia-session-test--chain-texts)
                                 '("on the project buffer")))
                  (should-not (scholia-session-test--stored "gamma" file))))))
        (delete-directory root t)))))


;;;; Renaming

(ert-deftest scholia-session-rename-rewrites-every-reference-or-fails ()
  "A rename reaches buffers other than the one that asked for it.
The second buffer here is bound to the old name and is not current when
the rename runs, and its annotation is unsaved.  A rename that rewrites
only the invoking buffer leaves it resolving to a session that is gone,
and its next save mints the old name afresh."
  (scholia-session-test--with-state
    (scholia-session-test--seed
     "alpha" "/nowhere/kept.txt"
     (list (scholia-session-test--annotation "one" "carried across")))
    (set-default 'scholia-session "alpha")
    (scholia-test-with-temp-file-buffer elsewhere scholia-session-test--source
      (setq-local scholia-session "alpha")
      (scholia-mode 1)
      (goto-char 1)
      (scholia-annotate "unsaved elsewhere")
      (with-temp-buffer
        (setq-local scholia-session "alpha")
        (scholia-session-rename "alpha" "omega")
        (should (file-exists-p (scholia-session-file "omega")))
        (should-not (file-exists-p (scholia-session-file "alpha")))
        (should (equal (scholia-db-session-name
                        (scholia-session-file "omega"))
                       "omega"))
        (should (equal (scholia-session-test--texts
                        (scholia-session-test--stored "omega" "/nowhere/kept.txt"))
                       '("carried across")))
        (should (equal (default-value 'scholia-session) "omega"))
        (should (equal scholia-session "omega"))
        (scholia-session-create "taken")
        (should-error (scholia-session-rename "omega" "taken") :type 'scholia-error)
        (should (file-exists-p (scholia-session-file "omega")))
        (should (equal (scholia-session-test--texts
                        (scholia-session-test--stored "omega" "/nowhere/kept.txt"))
                       '("carried across")))
        (should-not (scholia-db-files
                     (scholia-session-file "taken")))
        (should (equal (default-value 'scholia-session) "omega"))
        (should (equal scholia-session "omega")))
      (with-current-buffer elsewhere
        (should (equal scholia-session "omega"))
        (should (equal (scholia-session-name) "omega"))
        (should (equal (scholia-session-test--texts
                        (scholia-session-test--stored
                         "omega" (buffer-file-name elsewhere)))
                       '("unsaved elsewhere")))
        (should-not (file-exists-p (scholia-session-file "alpha")))))))

(ert-deftest scholia-session-rename-rewrites-the-persisted-project-assignment ()
  (scholia-session-test--with-state
    (let ((root (file-name-as-directory (make-temp-file "scholia-root-" t))))
      (unwind-protect
          (progn
            (set-default 'scholia-project-root-function (lambda () root))
            (set-default 'scholia-session "global")
            (scholia-session-create "global")
            (scholia-session-create "proj")
            (with-temp-buffer
              (scholia-session-assign-project "proj")
              (should (equal (scholia-session-name) "proj"))
              (set-default 'scholia-project-sessions nil)
              (scholia-session-load-assignments)
              (should (equal (scholia-session-name) "proj"))
              (scholia-session-rename "proj" "proj-renamed")
              (should (equal (scholia-session-name) "proj-renamed"))
              (set-default 'scholia-project-sessions nil)
              (scholia-session-load-assignments)
              (should (equal (scholia-session-name) "proj-renamed"))))
        (delete-directory root t)))))


;;;; Deleting

(ert-deftest scholia-session-delete-refuses-a-live-session-until-forced ()
  (scholia-session-test--with-state
    (let ((root (file-name-as-directory (make-temp-file "scholia-root-" t))))
      (unwind-protect
          (progn
            (set-default 'scholia-project-root-function (lambda () root))
            (set-default 'scholia-session "keep")
            (scholia-session-create "keep")
            (scholia-test-with-temp-file-buffer live scholia-session-test--source
              (let ((file (buffer-file-name live)))
                (scholia-session-assign-project "keep")
                (scholia-mode 1)
                (goto-char 1)
                (scholia-annotate "still only on screen")
                (scholia-test-with-temp-file-buffer bound
                    scholia-session-test--source
                  (setq-local scholia-session "keep")
                  (with-current-buffer live
                    (let ((overlay (scholia-session-test--first-overlay)))
                      (should-error (scholia-session-delete "keep")
                                    :type 'scholia-error)
                      (should (file-exists-p (scholia-session-file "keep")))
                      (should (scholia-annotation-p overlay))
                      (should (eq overlay (scholia-session-test--first-overlay)))
                      (scholia-session-delete "keep" t)
                      (should (scholia-annotation-p overlay))
                      (should (equal (scholia-session-test--chain-texts)
                                     '("still only on screen")))))
                  (with-current-buffer bound
                    (should-not (equal (scholia-session-name) "keep"))))
                (should-not (file-exists-p (scholia-session-file "keep")))
                (should-not (scholia-session-test--stored "keep" file))
                (should-not (member "keep" (scholia-session-list)))
                (with-temp-buffer
                  (should-not (equal (scholia-session-name) "keep")))
                (set-default 'scholia-project-sessions nil)
                (scholia-session-load-assignments)
                (with-temp-buffer
                  (should-not (equal (scholia-session-name) "keep"))))))
        (delete-directory root t)))))


;;;; Importing

(ert-deftest scholia-session-import-merges-into-a-name-already-taken ()
  (scholia-session-test--with-state
    (let ((outside (make-temp-file "scholia-outside-" nil ".eld"))
          (nonsense (make-temp-file "scholia-nonsense-" nil ".eld"
                                    "(:records nil)\n")))
      (unwind-protect
          (progn
            (scholia-db-create-session outside)
            (scholia-db-store-record
             outside
             (scholia-db-make-record
              "/nowhere/shared.txt"
              (list (scholia-session-test--annotation "two" "from outside"
                                                      7 11))
              "seeded"))
            (scholia-session-test--seed
             "target" "/nowhere/shared.txt"
             (list (scholia-session-test--annotation "one" "already here")))
            (scholia-session-import outside "target")
            (should (equal (scholia-session-test--texts
                            (scholia-session-test--stored "target"
                                                          "/nowhere/shared.txt"))
                           '("already here" "from outside")))
            (should (equal (scholia-db-session-name
                            (scholia-session-file "target"))
                           "target"))
            (scholia-session-import outside "brought-in")
            (should (equal (scholia-session-test--texts
                            (scholia-session-test--stored "brought-in"
                                                          "/nowhere/shared.txt"))
                           '("from outside")))
            (should (equal (scholia-db-session-name
                            (scholia-session-file "brought-in"))
                           "brought-in"))
            (should-error (scholia-session-import nonsense "refused")
                          :type 'scholia-db-format-error)
            (should-not (file-exists-p (scholia-session-file "refused"))))
        (delete-file outside)
        (delete-file nonsense)))))

(ert-deftest scholia-session-import-stamps-a-reply-that-arrived-without-its-parent ()
  (scholia-session-test--with-state
    (let ((outside (make-temp-file "scholia-outside-" nil ".eld")))
      (unwind-protect
          (progn
            (scholia-db-create-session outside)
            (scholia-db-store-record
             outside
             (scholia-db-make-record
              "/nowhere/stray.txt"
              (list (scholia-session-test--annotation "kept" "a root")
                    (scholia-db-make-annotation "stray" "answers nothing"
                                                nil nil nil nil nil "gone"))
              "seeded"))
            (scholia-session-import outside "imported")
            (let* ((stored (scholia-session-test--stored "imported"
                                                         "/nowhere/stray.txt"))
                   (stray (seq-find #'scholia-db-annotation-reply-p stored))
                   (kept (seq-find (lambda (annotation)
                                     (equal (scholia-db-annotation-id annotation)
                                            "kept"))
                                   stored)))
              (should (equal (scholia-db-annotation-orphaned-from stray) "gone"))
              (should (string-match-p "\\`[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]T"
                                      (scholia-db-annotation-orphaned-at stray)))
              (should (equal (scholia-db-annotation-text stray)
                             "answers nothing"))
              (should-not (scholia-db-annotation-orphaned-from kept))))
        (delete-file outside)))))

(ert-deftest scholia-session-export-writes-an-eld-that-import-reads-back ()
  "A session exports as an interchange `.eld' and imports back unchanged.
The export is what keeps a session something a user can diff, commit and
hand to a colleague once the store itself is a binary database, so it
carries the printed `:scholia' tag rather than the stored bytes."
  (scholia-session-test--with-state
    (let ((exported (make-temp-file "scholia-exported-" nil ".eld")))
      (unwind-protect
          (progn
            (scholia-session-test--seed
             "source" "/nowhere/shared.txt"
             (list (scholia-session-test--annotation "one" "a root" 1 6)
                   (scholia-session-test--annotation "two" "a second" 7 11)))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (scholia-session-export "source" exported))
            (should-not (equal (scholia-test-file-magic exported)
                               scholia-test-sqlite-magic))
            (should (eq (car (with-temp-buffer
                               (insert-file-contents exported)
                               (read (current-buffer))))
                        :scholia))
            (scholia-session-import exported "copy")
            (should (equal (scholia-session-test--stored
                            "copy" "/nowhere/shared.txt")
                           (scholia-session-test--stored
                            "source" "/nowhere/shared.txt")))
            (should (equal (scholia-db-files
                            (scholia-session-file "copy"))
                           (scholia-db-files
                            (scholia-session-file "source"))))
            (should (equal (scholia-db-session-name
                            (scholia-session-file "copy"))
                           "copy")))
        (delete-file exported)))))

(ert-deftest scholia-session-export-asks-before-it-replaces-a-file ()
  "Exporting onto a path that exists asks, and a refusal leaves it whole.
`read-file-name' completes onto existing files, so the one command that
writes where the user points had been replacing them without a word."
  (scholia-session-test--with-state
    (let ((target (make-temp-file "scholia-target-" nil ".eld" "keep me\n")))
      (unwind-protect
          (progn
            (scholia-session-test--seed
             "source" "/nowhere/shared.txt"
             (list (scholia-session-test--annotation "one" "a root")))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
              (should-error (scholia-session-export "source" target)
                            :type 'scholia-session-error))
            (should (equal (with-temp-buffer
                             (insert-file-contents target)
                             (buffer-string))
                           "keep me\n"))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (scholia-session-export "source" target))
            (should (eq (car (with-temp-buffer
                               (insert-file-contents target)
                               (read (current-buffer))))
                        :scholia)))
        (delete-file target)))))

(ert-deftest scholia-session-reads-an-interchange-file-without-rewriting-it ()
  "Listing and importing leave the interchange file they were handed alone.
An exported `.eld' is something to diff, commit and hand to a colleague,
and migrating whatever path a caller points at turned a git-tracked file
into a binary blob the moment a session prompt was opened."
  (scholia-session-test--with-state
    (let ((parked (expand-file-name "parked.eld" scholia-session-directory)))
      (scholia-session-test--seed
       "source" "/nowhere/shared.txt"
       (list (scholia-session-test--annotation "one" "a root")))
      (scholia-session-export "source" parked)
      (should (member "parked" (scholia-session-list)))
      (should-not (equal (scholia-test-file-magic parked)
                         scholia-test-sqlite-magic))
      (scholia-session-import parked "copy")
      (should-not (equal (scholia-test-file-magic parked)
                         scholia-test-sqlite-magic))
      (should (equal (scholia-session-test--texts
                      (scholia-session-test--stored "copy"
                                                    "/nowhere/shared.txt"))
                     '("a root"))))))


;;;; The lifecycle of a store on disk

(ert-deftest scholia-session-delete-and-rename-leave-nothing-of-the-old-name ()
  "A deleted name comes back empty and a renamed one leaves no journals.
Unlinking the `.eld' alone orphans the WAL and shared-memory files beside
it, and the next session of the same name then either fails to open or
comes back holding the deleted session's annotations verbatim."
  (scholia-session-test--with-state
    (scholia-session-test--seed
     "notes" "/nowhere/kept.txt"
     (list (scholia-session-test--annotation "one" "deleted with the session")))
    (should (equal (scholia-test-file-magic (scholia-session-file "notes"))
                   scholia-test-sqlite-magic))
    (scholia-session-delete "notes")
    (should-not (file-exists-p (scholia-session-file "notes")))
    (should-not (scholia-session-test--journals))
    (scholia-session-create "notes")
    (should-not (scholia-db-files
                 (scholia-session-file "notes")))
    (should-not (scholia-session-test--stored "notes" "/nowhere/kept.txt"))
    (scholia-session-test--seed
     "notes" "/nowhere/kept.txt"
     (list (scholia-session-test--annotation "two" "carried to the new name")))
    (scholia-session-rename "notes" "archive")
    (should-not (file-exists-p (scholia-session-file "notes")))
    (should-not (scholia-session-test--journals))
    (should (equal (scholia-session-test--texts
                    (scholia-session-test--stored
                     "archive" "/nowhere/kept.txt"))
                   '("carried to the new name")))
    (let ((store (scholia-store-open (scholia-session-file "archive"))))
      (unwind-protect
          (progn
            (scholia-store-put-record
             store (scholia-db-make-record
                    "/nowhere/second.txt"
                    (list (scholia-session-test--annotation "three" "written"))
                    "checksum"))
            (should (scholia-session-test--journals))
            (should (equal (scholia-session-list) '("archive"))))
        (scholia-store-close store)))
    (should-not (scholia-session-test--journals))))

(provide 'scholia-session-test)
;;; scholia-session-test.el ends here
