;;; scholia-store-test.el --- Tests for the scholia SQLite store  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Covers the `.eld' boundary `scholia-store' owns: a session file that
;; still holds a printed plist migrates into SQLite in place on first open,
;; a session file that already holds a store opens straight through, and a
;; migration that dies before it lands leaves the readable plist whole.
;; Everything is read back through the db API rather than through the
;; stored representation (INV-12).

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'scholia-test-helper)

(eval-and-compile
  (setq load-prefer-newer t)
  (require 'scholia-vars nil t)
  (require 'scholia-db nil t)
  (require 'scholia-store nil t)
  (require 'scholia-session nil t))

(defvar scholia-store-test--read-eval-sentinel)

(defconst scholia-store-test--legacy
  '(:scholia 1
	     :records ((:file "/nowhere/legacy.txt"
			      :annotations ((:id "id-one" :beg 1 :end 6 :text "first note"
						 :annotated-text "alpha" :line 1 :line-text ""
						 :column 0 :end-column 0 :color 0
						 :position :margin :reply-to nil
						 :sends ((:at "2026-01-01T00:00:00+0000"
							      :kind agent :target "codex"
							      :label "Codex" :herdr-session nil
							      :format rustc :scope buffer)))
					    (:id "id-two" :beg 1 :end 6 :text "a reply"
						 :annotated-text "alpha" :line 1 :line-text ""
						 :column 0 :end-column 0 :color 0
						 :position :margin :reply-to "id-one"
						 :sends nil))
			      :checksum "legacy-checksum"))
	     :session (:name "legacy" :created "2026-01-01T00:00:00+0000"
			     :project nil :description nil))
  "A session as `scholia-db' printed it before the store moved to SQLite.
It carries a send and a reply because migration is the one-shot path every
existing session takes, and a mapping that drops either loses it silently.")

(defun scholia-store-test--session-file (name)
  "Return the path of session NAME inside `scholia-session-directory'."
  (expand-file-name (concat name ".eld") scholia-session-directory))

(defun scholia-store-test--write-legacy (session)
  "Write the pre-SQLite printed plist of a session to the path SESSION.
Its header carries the name SESSION is called on disk."
  (with-temp-file session
    (let ((print-length nil))
      (prin1 (plist-put (copy-sequence scholia-store-test--legacy)
                        :session (list :name (file-name-base session)
                                       :created "2026-01-01T00:00:00+0000"
                                       :project nil :description nil))
             (current-buffer)))))

(defun scholia-store-test--bytes (file)
  "Return the whole of FILE as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun scholia-store-test--note (session file)
  "Return the note the annotation \"id-one\" carries in SESSION for FILE.
Selected by id rather than by position: the fixture's two annotations
share a beginning, so a store free to return rows in any order would
fail a migration that is correct."
  (scholia-db-annotation-text
   (seq-find (lambda (annotation)
               (equal (scholia-db-annotation-id annotation) "id-one"))
             (scholia-db-record-annotations
              (scholia-db-record session file)))))


;;;; The `.eld' boundary

(ert-deftest scholia-store-load-dispatches-on-the-sqlite-magic ()
  "A printed-plist session migrates in place; a store opens straight through.
The migration happens once, at the same path, so a later open finds a
database rather than a plist and nothing stale is left for it to find.

The first read here goes through `scholia-db-record', because that is the
reader production reaches first: `scholia-initialize' takes it when a user
turns the mode on in a buffer.  Migrating only for `scholia-db-files',
which no production caller takes, would leave that user's annotations
silently absent."
  (scholia-test-with-session-directory
    (let ((session (scholia-store-test--session-file "legacy")))
      (scholia-store-test--write-legacy session)
      (should-not (equal (scholia-test-file-magic session)
                         scholia-test-sqlite-magic))
      (should (equal (scholia-store-test--note session "/nowhere/legacy.txt")
                     "first note"))
      (should (equal (scholia-test-file-magic session)
                     scholia-test-sqlite-magic))
      (should (equal (scholia-db-files session) '("/nowhere/legacy.txt")))
      (should (equal (scholia-db-session-name session) "legacy"))
      (should (equal (scholia-db-record-checksum
                      (scholia-db-record session
                                         "/nowhere/legacy.txt"))
                     "legacy-checksum"))
      (let* ((migrated (scholia-db-record-annotations
                        (scholia-db-record session
                                           "/nowhere/legacy.txt")))
             (with-id (lambda (id)
                        (seq-find (lambda (annotation)
                                    (equal (scholia-db-annotation-id annotation)
                                           id))
                                  migrated))))
        (should (equal (scholia-db-send-target
                        (car (scholia-db-annotation-sends
                              (funcall with-id "id-one"))))
                       "codex"))
        (should (equal (scholia-db-annotation-reply-to
                        (funcall with-id "id-two"))
                       "id-one")))
      (should (equal (scholia-test-file-magic session)
                     scholia-test-sqlite-magic)))))

(ert-deftest scholia-store-an-interrupted-migration-leaves-the-eld-readable ()
  "A migration that never lands leaves the printed plist byte for byte.
A migration that rewrote the session in place instead would leave half a
database at a path with no readable predecessor to fall back to."
  (scholia-test-with-session-directory
    (let* ((session (scholia-store-test--session-file "interrupted"))
           (before (progn (scholia-store-test--write-legacy session)
                          (scholia-store-test--bytes session))))
      (cl-letf (((symbol-function 'rename-file)
                 (lambda (&rest _) (error "Migration interrupted"))))
        (ignore-errors (scholia-db-files session)))
      (should (equal (scholia-store-test--bytes session) before))
      (should-not (equal (scholia-test-file-magic session)
                         scholia-test-sqlite-magic))
      (should (equal (scholia-db-files session) '("/nowhere/legacy.txt")))
      (should (equal (scholia-db-session-name session) "interrupted"))
      (should (equal (scholia-test-file-magic session)
                     scholia-test-sqlite-magic))
      (should (equal (scholia-store-test--note session "/nowhere/legacy.txt")
                     "first note")))))


;;;; What a row carries

(ert-deftest scholia-store-carries-a-carriage-return-through-a-row ()
  "A carriage return in a stored value comes back as itself.
`scholia-db--relocate' searches the buffer for the stored
`:annotated-text' verbatim, so a return the store hands back as a
newline makes the annotation permanently unplaceable and nothing warns."
  (scholia-test-with-session-directory
    (let* ((session (scholia-store-test--session-file "returns"))
           (file "/nowhere/crlf.txt")
           (annotation (list :id "cr" :annotated-text "beta\rgamma"))
           (record (list :file file
                         :annotations (list annotation)
                         :checksum "returned")))
      (let ((store (scholia-store-open session)))
        (unwind-protect
            (scholia-store-with-transaction store
              (scholia-store-put-record store record))
          (scholia-store-close store)))
      (should (equal (scholia-db-record session file)
                     record)))))


(ert-deftest scholia-store-carries-a-carriage-return-through-a-file-key ()
  "A record keyed by a file name holding a carriage return stays keyed by it.
The key column decodes a return as a newline, so `scholia-db-files'
answered a path no row is keyed by while `scholia-db-record' still found
the record by the path it was stored under, and the export that maps the
one onto the other carried a record that was nil."
  (scholia-test-with-session-directory
    (let* ((session (scholia-store-test--session-file "returned-key"))
           (file "/nowhere/we\rird.txt")
           (record (list :file file :annotations nil :checksum "keyed")))
      (scholia-db-store-record session record)
      (should (equal (scholia-db-files session) (list file)))
      (should (equal (scholia-db-record session file) record))
      (should (equal (plist-get (scholia-db-interchange session) :records)
                     (list record))))))


;;;; A file the store was only pointed at

(ert-deftest scholia-store-refuses-a-database-that-is-not-its-own ()
  "A foreign database and a file that only looks like one are both refused.
Declaring scholia's schema into a file it was merely handed writes the
`session' and `records' tables into someone else's database, and a file
carrying the magic but no readable database raised the raw
`sqlite-error' of the failed read rather than a format error."
  (scholia-test-with-session-directory
    (let ((foreign (scholia-store-test--session-file "payroll"))
          (broken (scholia-store-test--session-file "broken")))
      (let ((connection (sqlite-open foreign)))
        (sqlite-execute connection "CREATE TABLE payroll (name TEXT)")
        (sqlite-close connection))
      (let ((before (scholia-store-test--bytes foreign)))
        (should-error (scholia-db-files foreign) :type 'scholia-db-format-error)
        (should-error (scholia-db-record foreign "/nowhere/payroll.txt")
                      :type 'scholia-db-format-error)
        (should (equal (scholia-store-test--bytes foreign) before)))
      (let ((coding-system-for-write 'binary))
        (with-temp-file broken
          (set-buffer-multibyte nil)
          (insert scholia-test-sqlite-magic "nothing readable follows")))
      (should-error (scholia-db-files broken) :type 'scholia-db-format-error))))

(ert-deftest scholia-store-a-held-session-is-not-reported-as-malformed ()
  "A session another connection holds reaches the caller as held.
Every error while opening was answered with `scholia-db-format-error',
the signal that says the file is not scholia's, so a peer holding the
database past `scholia-store-busy-timeout' reported a healthy session as
malformed.  `scholia-session-list' answers nil to anything the probe
raises, so there the same session simply left the completion table, and
a user who cannot see a session is one keystroke from making a second
one under its name."
  (scholia-test-with-session-directory
    (let* ((session (scholia-store-test--session-file "held-exclusively"))
           (file "/nowhere/held-exclusively.txt")
           (record (list :file file :annotations nil :checksum "intact"))
           (holder nil))
      (scholia-db-store-record session record)
      (setq holder (sqlite-open session))
      (unwind-protect
          (let ((scholia-store-busy-timeout 50))
            (sqlite-pragma holder "journal_mode=wal")
            (sqlite-pragma holder "locking_mode=exclusive")
            (sqlite-execute holder "BEGIN IMMEDIATE")
            (sqlite-execute holder "REPLACE INTO session VALUES ('held', 'x')")
            (should-error (scholia-db-files session) :type 'sqlite-error))
        (sqlite-close holder))
      (should (equal (scholia-db-record session file) record)))))


;;;; The lifecycle of a store on disk

(ert-deftest scholia-store-a-first-save-that-fails-takes-the-name-with-it ()
  "A first save that dies leaves no database at the path it was taking.
The schema alone is enough to make the name look taken while
`scholia-session-list', which reads a header, cannot see it: a session
the user can neither open nor create."
  (scholia-test-with-session-directory
    (let ((session (scholia-store-test--session-file "unborn")))
      (cl-letf (((symbol-function 'scholia-store-put-session)
                 (lambda (&rest _) (error "Save interrupted"))))
        (should-error (scholia-db-create-session session)))
      (should-not (file-exists-p session)))))

(ert-deftest scholia-store-a-commit-that-fails-is-not-reported-as-a-save ()
  "A commit that does not land raises rather than returning as a save.
`sqlite-commit' answers nil rather than signalling when the commit
fails, so the write reached its caller as a success that stored nothing:
the rollback was skipped, and the store still being made landed its
temporary at the session path although nothing had committed."
  (scholia-test-with-session-directory
    (let ((session (scholia-store-test--session-file "unlanded"))
          (record (list :file "/nowhere/unlanded.txt"
                        :annotations nil :checksum "lost")))
      (cl-letf (((symbol-function 'sqlite-commit) (lambda (&rest _) nil)))
        (should-error (scholia-db-store-record session record)
                      :type 'scholia-error))
      (should-not (file-exists-p session)))))

(ert-deftest scholia-store-delete-takes-the-rollback-journal-with-it ()
  "Deleting a session takes the rollback journal beside it too.
A `-journal' is what a database in delete mode leaves when a process
dies mid-transaction, and delete mode is what every close leaves it in.
Named by nothing, it survived the deletion, the session made at that
path afterwards, and every open and close of that one."
  (scholia-test-with-session-directory
    (let* ((session (scholia-store-test--session-file "journalled"))
           (journal (concat session "-journal")))
      (scholia-db-create-session session)
      (write-region "" nil journal nil 'silent)
      (scholia-store-delete session)
      (should-not (file-exists-p journal)))))

(ert-deftest scholia-store-rename-refuses-while-another-connection-holds-it ()
  "A rename that cannot check the write-ahead log back in refuses to move.
Deleting the journals beside OLD once `rename-file' has moved the
database alone destroys every commit still in the log, and the rename
reports success."
  (scholia-test-with-session-directory
    (let* ((old (scholia-store-test--session-file "held"))
           (new (scholia-store-test--session-file "moved"))
           (file "/nowhere/held.txt")
           (record (list :file file :annotations nil :checksum "committed")))
      (scholia-db-create-session old)
      (let ((store (scholia-store-open old)))
        (unwind-protect
            (progn
              (scholia-store-with-transaction store
                (scholia-store-put-record store record))
              (should-error (scholia-store-rename old new)
                            :type 'scholia-error))
          (scholia-store-close store)))
      (should-not (file-exists-p new))
      (should (equal (scholia-db-record old file) record)))))


;;;; A second Emacs writing the same session

(defconst scholia-store-test--source
  "alpha one\nbeta two\ngamma three\n"
  "Buffer contents the concurrent fixtures annotate.")

(defconst scholia-store-test--emacs
  (expand-file-name invocation-name invocation-directory)
  "The Emacs running this suite, which a second process is started from.")

(defun scholia-store-test--parent (id text)
  "Return a placed annotation carrying ID and TEXT over \"alpha\"."
  (list :id id :text text :beg 1 :end 6
        :annotated-text "alpha"
        :line nil :line-text nil :column nil :end-column nil
        :color 0 :position :margin :reply-to nil :sends nil))

(defun scholia-store-test--appends (session file tag count)
  "Return a form appending COUNT replies tagged TAG to FILE in SESSION.
Each reply carries an id of its own, so a writer that replaced the row it
found rather than adding one annotation to it comes back short by however
many replies it wrote over."
  `(dotimes (index ,count)
     (scholia-db-add-reply
      ,session ,file
      (list :id (format "%s-%d" ,tag index)
            :text (format "%s-%d" ,tag index)
            :beg nil :end nil :annotated-text nil
            :line nil :line-text nil :column nil :end-column nil
            :color 0 :position :margin
            :reply-to "id-parent" :sends nil))
     (sleep-for 0.02)))

(defconst scholia-store-test--ready "scholia-store-test-ready"
  "What a second Emacs prints once it is loaded and about to write.")

(defun scholia-store-test--start (form)
  "Start a second Emacs evaluating FORM and return the process.
FORM runs once `scholia-db' has been required out of this worktree, so
the second process writes through the same code as the first.  The child
announces itself first, so a caller can tell a writer that is blocked
from one that is still starting Emacs.  It announces through `message'
rather than `princ' because Emacs block-buffers batch stdout to a pipe on
Windows, where a child that then blocks holds the marker until it exits;
batch `message' goes to stderr, which Emacs flushes on every write."
  (let* ((buffer (generate-new-buffer " *scholia-store-test*"))
         (process
          (start-process
           "scholia-store-test" buffer scholia-store-test--emacs
           "-Q" "--batch" "--eval"
           (let ((print-length nil)
                 (print-level nil))
             (prin1-to-string
              `(progn (setq load-prefer-newer t)
                      (push ,scholia-test-project-root load-path)
                      (require 'scholia-db)
                      (message ,scholia-store-test--ready)
                      ,form))))))
    (set-process-query-on-exit-flag process nil)
    process))

(defun scholia-store-test--announced-p (process)
  "Return non-nil once PROCESS has said it is loaded."
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (point-min))
      (search-forward scholia-store-test--ready nil t))))

(defun scholia-store-test--await-ready (process)
  "Wait until PROCESS has loaded and is about to write.
Signal an ERT test failure when it never says so, which is a child that
died on the way up rather than one this test can reason about.  The
failure carries what the child said and whether it is still alive,
because a bare timeout does not tell a child that died on the way up from
one whose announcement never crossed the pipe."
  (let ((deadline (+ (float-time) 30)))
    (while (and (process-live-p process)
                (< (float-time) deadline)
                (not (scholia-store-test--announced-p process)))
      (accept-process-output process 0.01)))
  (unless (scholia-store-test--announced-p process)
    (ert-fail (list "the second Emacs never announced itself"
                    :emacs scholia-store-test--emacs
                    :live (process-live-p process)
                    :status (process-status process)
                    :said (with-current-buffer (process-buffer process)
                            (buffer-string))))))

(defun scholia-store-test--outcome (process)
  "Return 0 when PROCESS exited cleanly, and what it said when it did not."
  (if (equal (process-exit-status process) 0)
      0
    (with-current-buffer (process-buffer process) (buffer-string))))

(defun scholia-store-test--while-writing (processes body)
  "Call BODY over and over until every one of PROCESSES has exited.
Signal an ERT test failure when one is still running after thirty
seconds, which is a writer that never came back rather than one that
waited."
  (let ((deadline (+ (float-time) 30)))
    (while (and (seq-find #'process-live-p processes)
                (< (float-time) deadline))
      (funcall body)
      (accept-process-output nil 0.01)))
  (should-not (seq-find #'process-live-p processes))
  (dolist (process processes)
    (should (equal (scholia-store-test--outcome process) 0))))

(defun scholia-store-test--replies (session file)
  "Return the replies SESSION carries in its record for FILE."
  (seq-filter #'scholia-db-annotation-reply-p
              (scholia-db-record-annotations
               (scholia-db-record session file))))

(defun scholia-store-test--holds-p (session file id)
  "Return non-nil when SESSION carries an annotation ID for FILE."
  (and (seq-find (lambda (annotation)
                   (equal (scholia-db-annotation-id annotation) id))
                 (scholia-db-record-annotations
                  (scholia-db-record session file)))
       t))

(ert-deftest scholia-store-a-second-emacs-appending-a-reply-loses-nothing ()
  "Two other Emacs processes reply while this one saves, and all of it lands.
One process cannot race itself, so this is the only shape that reaches the
shared write path.  Every writer here reads the record, adds to it and
writes it back: under a writer that is not serialized, a reply committed
between another writer's read and its write is written back out of
existence with nothing raised anywhere, which is what the printed-plist
store did across whole session files.

Twenty replies per writer rather than a handful: a lost update needs the
parent's read-and-write window to close over a child's commit, and at five
apiece a stale whole-database rewrite survived two runs in five.  The count
is what makes this an oracle instead of a coin flip."
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-store-test--source
      (let* ((file (buffer-file-name buffer))
             (session (scholia-store-test--session-file "concurrent"))
             (parent (scholia-store-test--parent "id-parent" "the parent"))
             (each 20)
             (writers nil))
        (scholia-db-save session file (list parent) "checksum-one")
        (setq writers
              (list (scholia-store-test--start
                     (scholia-store-test--appends session file "first" each))
                    (scholia-store-test--start
                     (scholia-store-test--appends session file "second" each))))
        (scholia-store-test--while-writing
         writers
         (lambda ()
           (scholia-db-save session file (list parent) "checksum-one")))
        (should (scholia-store-test--holds-p session file "id-parent"))
        (should (equal (sort (mapcar #'scholia-db-annotation-id
                                     (scholia-store-test--replies session file))
                             #'string<)
                       (sort (append
                              (mapcar (lambda (index) (format "first-%d" index))
                                      (number-sequence 0 (1- each)))
                              (mapcar (lambda (index) (format "second-%d" index))
                                      (number-sequence 0 (1- each))))
                             #'string<)))))))

(ert-deftest scholia-store-a-save-leaves-the-row-another-writer-changed ()
  "Saving one file does not revert the row a second Emacs wrote for another.
Under the printed-plist store the blast radius of a save was the whole
session: every writer serialized the database it had loaded, so an agent's
reply to one file was reverted by Emacs saving a buffer visiting a
different one.

Thirty replies for the same reason the sibling test writes twenty: at six
the stale rewrite this exists to catch survived two runs in five."
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-store-test--source
      (let* ((file (buffer-file-name buffer))
             (other "/nowhere/elsewhere.txt")
             (session (scholia-store-test--session-file "elsewhere"))
             (parent (scholia-store-test--parent "id-parent" "the parent"))
             (appended 30)
             (writer nil))
        (scholia-db-save session file (list parent) "checksum-one")
        (scholia-db-store-record
         session (scholia-db-make-record other (list parent) "checksum-other"))
        (setq writer (scholia-store-test--start
                      (scholia-store-test--appends
                       session other "outside" appended)))
        (scholia-store-test--while-writing
         (list writer)
         (lambda ()
           (scholia-db-save session file (list parent) "checksum-one")))
        (should (scholia-store-test--holds-p session other "id-parent"))
        (should (equal (length (scholia-store-test--replies session other))
                       appended))
        (should (scholia-store-test--holds-p session file "id-parent"))))))

(ert-deftest scholia-store-a-second-writer-waits-rather-than-signalling ()
  "A writer that finds the session held waits for it instead of failing.
Without `BEGIN IMMEDIATE' and a `busy_timeout' the second writer either
signals that the database is locked or reads a snapshot it cannot upgrade,
and the reply an agent made is lost with an error nobody sees.

The wait is measured from the child's own announcement rather than from
`start-process', because a child still starting Emacs is alive for a
reason this test is not asking about.  Timed from the wrong end, a store
whose transactions take no write lock passes wherever Emacs takes longer
to boot than the hold lasts — which is a green here and a green nowhere
that matters."
  (scholia-test-with-session-directory
    (let* ((session (scholia-store-test--session-file "held-open"))
           (file "/nowhere/contended.txt")
           (parent (scholia-store-test--parent "id-parent" "the parent"))
           (writer nil))
      (scholia-db-create-session session)
      (scholia-db-store-record
       session (scholia-db-make-record file (list parent) "checksum"))
      (let ((store (scholia-store-open session)))
        (unwind-protect
            (scholia-store-with-transaction store
              (scholia-store-put-record
               store
               (scholia-db-make-record "/nowhere/blocking.txt" nil "held"))
              (setq writer
                    (scholia-store-test--start
                     (scholia-store-test--appends session file "waited" 1)))
              (scholia-store-test--await-ready writer)
              (sleep-for 1)
              (should (process-live-p writer)))
          (scholia-store-close store)))
      (scholia-store-test--while-writing (list writer) #'ignore)
      (should (scholia-store-test--holds-p session file "id-parent"))
      (should (equal (mapcar #'scholia-db-annotation-id
                             (scholia-store-test--replies session file))
                     '("waited-0"))))))


;;;; A session that is not there yet, and two processes taking it

(defmacro scholia-store-test--with-flags (names &rest body)
  "Evaluate BODY with each of NAMES bound to a flag file that is not there.
A flag is how this test and a child process tell each other where they
are: the one that waits polls for the file, the one that arrives writes
it.  They are made outside `scholia-session-directory' so that a test
asserting what a session directory holds is not reading its own
scaffolding back."
  (declare (indent 1) (debug (listp body)))
  `(let ,(mapcar (lambda (name)
                   `(,name (make-temp-name
                            (expand-file-name "scholia-store-test-flag-"
                                              temporary-file-directory))))
                 names)
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (name)
                   `(when (file-exists-p ,name) (delete-file ,name)))
                 names))))

(defun scholia-store-test--raise (flag)
  "Write FLAG, releasing whoever waits on it."
  (write-region "" nil flag nil 'silent))

(defun scholia-store-test--held-until (flag form)
  "Return FORM held back until FLAG appears.
Two children racing for one first write have to be let go together.
Released at `start-process' instead, the one that boots first can be
finished before the other has opened the session at all, and the second
then adopts a database that is already there: the race the test exists
for never happens and the green says nothing."
  `(progn (while (not (file-exists-p ,flag)) (sleep-for 0.005))
          ,form))

(defun scholia-store-test--await-flag (flag process)
  "Wait until FLAG appears, PROCESS dies, or thirty seconds pass.
Signal an ERT test failure when the flag never arrives, which is a child
that died on the way rather than one this test can reason about."
  (let ((deadline (+ (float-time) 30)))
    (while (and (process-live-p process)
                (< (float-time) deadline)
                (not (file-exists-p flag)))
      (accept-process-output process 0.01)))
  (should (file-exists-p flag)))

(defun scholia-store-test--kill (process)
  "Kill PROCESS outright and wait for it to be gone."
  (signal-process (process-id process) 'SIGKILL)
  (let ((deadline (+ (float-time) 30)))
    (while (and (process-live-p process) (< (float-time) deadline))
      (accept-process-output process 0.01)))
  (should-not (process-live-p process)))

(defun scholia-store-test--stalling-first-write (session flag)
  "Return a form writing into SESSION uncommitted, raising FLAG, then waiting.
The child is killed where it stands, so SESSION is left exactly as a
process that died inside its first transaction leaves it."
  `(let ((store (scholia-store-open ,session)))
     (scholia-store-with-transaction store
       (scholia-store-put-record
        store (list :file "/nowhere/uncommitted.txt"
                    :annotations nil :checksum "never"))
       (write-region "" nil ,flag nil 'silent)
       (sleep-for 300))))

(defun scholia-store-test--strays (session)
  "Return what SESSION's directory holds besides SESSION and its journals.
`scholia-session-list' globs \"\\\\.eld\\\\='\", so a temporary left
beside a session is invisible to the user rather than harmless."
  (let ((base (file-name-nondirectory session)))
    (seq-remove (lambda (entry) (string-prefix-p base entry))
                (directory-files (file-name-directory session) nil
                                 directory-files-no-dot-files-regexp))))

(defun scholia-store-test--reply-ids (session file)
  "Return the ids of the replies SESSION carries for FILE, sorted."
  (sort (mapcar #'scholia-db-annotation-id
                (scholia-store-test--replies session file))
        #'string<))

(ert-deftest scholia-store-two-first-writers-of-one-session-both-land ()
  "Two Emacs processes first-writing one absent session keep both writes.
Neither may report success while the replies it committed are gone.  A
create path that builds a database beside the destination and lands it
at close serialises nothing: the two `BEGIN IMMEDIATE's are taken on two
different files, both writers commit, and whichever renames last
replaces the other's session whole -- not one row short, the entire
file.  The first write to a fresh session is this package's motivating
scenario, an agent replying while Emacs saves."
  (scholia-test-with-session-directory
    (scholia-store-test--with-flags (go)
      (let* ((session (scholia-store-test--session-file "unborn-race"))
             (file "/nowhere/race.txt")
             (each 3)
             (writers
              (list (scholia-store-test--start
                     (scholia-store-test--held-until
                      go (scholia-store-test--appends
                          session file "first" each)))
                    (scholia-store-test--start
                     (scholia-store-test--held-until
                      go (scholia-store-test--appends
                          session file "second" each))))))
        (mapc #'scholia-store-test--await-ready writers)
        (scholia-store-test--raise go)
        (scholia-store-test--while-writing writers #'ignore)
        (should (equal (scholia-db-session-name session) "unborn-race"))
        (should (equal (scholia-store-test--reply-ids session file)
                       (sort (mapcan
                              (lambda (tag)
                                (mapcar (lambda (index)
                                          (format "%s-%d" tag index))
                                        (number-sequence 0 (1- each))))
                              (list "first" "second"))
                             #'string<)))))))

(ert-deftest scholia-store-a-headerless-database-at-a-session-path-is-free ()
  "A first write killed outright leaves a name the user can still take.
Making the database at the session path is what lets `BEGIN IMMEDIATE'
serialise a first write, and it costs the guarantee that a first write
which never landed left nothing at all: a process killed inside its
first transaction leaves a database carrying no session header.
`scholia-session-list' filters on a readable header and cannot see that
name, while `scholia-session-create' refuses it because the path exists
-- a session the user can neither open nor create.

So the header-less database has to read as absent rather than as
malformed, and creating over it has to be allowed.  A database that is
somebody else's is the other half of the same question and stays
refused: a name that frees itself for anything it cannot read would
empty a file scholia never made."
  (scholia-test-with-session-directory
    (scholia-store-test--with-flags (writing)
      (let* ((session (scholia-store-test--session-file "ghost"))
             (file "/nowhere/ghost.txt")
             (record (list :file file :annotations nil :checksum "after"))
             (foreign (scholia-store-test--session-file "payroll"))
             (child (scholia-store-test--start
                     (scholia-store-test--stalling-first-write
                      session writing))))
        (scholia-store-test--await-ready child)
        (scholia-store-test--await-flag writing child)
        (scholia-store-test--kill child)
        (should (file-exists-p session))
        (should-not (scholia-store-test--strays session))
        (should-not (scholia-db-files session))
        (should-not (member "ghost" (scholia-session-list)))
        (let ((target (default-value 'scholia-session))
              (active (copy-sequence (default-value 'scholia-visible-sessions))))
          (unwind-protect
              (progn
                (scholia-session-create "ghost")
                (should (member "ghost" (scholia-session-list))))
            (set-default 'scholia-session target)
            (set-default 'scholia-visible-sessions active)))
        (scholia-db-store-record session record)
        (should (equal (scholia-db-record session file) record))
        (let ((connection (sqlite-open foreign)))
          (sqlite-execute connection "CREATE TABLE payroll (name TEXT)")
          (sqlite-close connection))
        (should-error (scholia-session-create "payroll")
                      :type 'scholia-error)))))

(ert-deftest scholia-store-a-first-save-that-fails-leaves-no-journal-either ()
  "A first save that dies takes the write-ahead log with the name.
The database being made at the session path rather than beside it, what
a failed first save has to unlink is the file and its journals both.  A
close that takes the database and leaves the `-wal' and `-shm' leaves a
session directory that lists as empty and litters for good, since
`scholia-session-list' globs \"\\\\.eld\\\\='\" and never sees them."
  (scholia-test-with-session-directory
    (let ((session (scholia-store-test--session-file "unborn-clean")))
      (cl-letf (((symbol-function 'scholia-store-put-session)
                 (lambda (&rest _) (error "Save interrupted"))))
        (should-error (scholia-db-create-session session)))
      (should-not (directory-files scholia-session-directory nil
                                   directory-files-no-dot-files-regexp))
      (let ((target (default-value 'scholia-session))
            (active (copy-sequence (default-value 'scholia-visible-sessions))))
        (unwind-protect
            (progn
              (scholia-session-create "unborn-clean")
              (should (member "unborn-clean" (scholia-session-list))))
          (set-default 'scholia-session target)
          (set-default 'scholia-visible-sessions active))))))

(ert-deftest scholia-store-two-migrations-of-one-eld-keep-both-results ()
  "Two Emacs processes migrating one printed session keep every write.
A migration is the same transition as a first write and races it the
same way: each process reads the plist, builds a database of its own and
renames it over the session path, so the second landing replaces the
first whole.  It is the loud half of the defect too -- the loser reads
an inode that has been renamed away and fails, either with the raw
`sqlite-error' of the failed read or by reporting a session whose
records are intact as malformed."
  (scholia-test-with-session-directory
    (scholia-store-test--with-flags (go)
      (let* ((session (scholia-store-test--session-file "legacy-race"))
             (file "/nowhere/legacy.txt")
             (writers nil))
        (scholia-store-test--write-legacy session)
        (setq writers
              (list (scholia-store-test--start
                     (scholia-store-test--held-until
                      go (scholia-store-test--appends session file "first" 1)))
                    (scholia-store-test--start
                     (scholia-store-test--held-until
                      go (scholia-store-test--appends
                          session file "second" 1)))))
        (mapc #'scholia-store-test--await-ready writers)
        (scholia-store-test--raise go)
        (scholia-store-test--while-writing writers #'ignore)
        (should (equal (scholia-db-session-name session) "legacy-race"))
        (should (equal (sort (mapcar #'scholia-db-annotation-id
                                     (scholia-db-record-annotations
                                      (scholia-db-record session file)))
                             #'string<)
                       '("first-0" "id-one" "id-two" "second-0")))))))

(ert-deftest scholia-store-a-reader-leaves-a-database-it-did-not-make ()
  "A read that finds a half-made database leaves it to the Emacs making it.
`sqlite-open' and the `journal_mode=wal' pragma leave a file carrying the
magic and no table at all, and that is where a first writer sits until
its schema commits.  A reader arriving there takes the same create branch
and carried `created' on a file it did not make, so its close unlinked
the database and the write-ahead log and shared memory beside it.
`scholia-session-list' probes every `.eld' in the directory, so opening
the session prompt was enough to destroy a session another Emacs was
creating, and the creating Emacs then died on its next statement."
  (scholia-test-with-session-directory
    (let ((session (scholia-store-test--session-file "half-made")))
      (let ((connection (sqlite-open session)))
        (sqlite-pragma connection "journal_mode=wal")
        (sqlite-close connection))
      (should (equal (scholia-test-file-magic session)
                     scholia-test-sqlite-magic))
      (should-not (scholia-db-files session))
      (should (file-exists-p session))
      (should-not (member "half-made" (scholia-session-list)))
      (should (file-exists-p session))
      (scholia-db-create-session session)
      (should (member "half-made" (scholia-session-list))))))

(ert-deftest scholia-store-a-migration-that-does-not-land-leaves-the-file ()
  "A migration that refuses or is raced leaves the session at its own path.
The plist is moved aside before it is read, so content the reader will
not parse — a kill mid-sync, a partly fetched checkout, a file of the
user's own that merely sits in the session directory — was renamed away
and the signal escaped with the restore never run.  Reading such a file
moved it, and `scholia-session-list' globs \"\\\\.eld\\\\='\", so it left
view as well as its path.

Two claimers sharing one aside name is the same wound from the other
side: the second renames the database the first made onto the first's
copy, and the first's cleanup then deletes it, so nothing is left at
either name.  Neither claimer may be able to name the other's copy."
  (scholia-test-with-session-directory
    (let ((mine (scholia-store-test--session-file "shopping")))
      (with-temp-file mine (insert "milk, eggs, bread\n"))
      (let ((before (scholia-store-test--bytes mine)))
        (should-error (scholia-db-files mine) :type 'scholia-db-format-error)
        (should (equal (scholia-store-test--bytes mine) before))))
    (let* ((session (scholia-store-test--session-file "raced"))
           (file "/nowhere/legacy.txt")
           (journals (scholia-store--journals session))
           (held nil))
      (scholia-store-test--write-legacy session)
      (cl-letf* ((remove (symbol-function 'delete-file))
                 ((symbol-function 'delete-file)
                  (lambda (path &rest arguments)
                    (if (member path journals)
                        (apply remove path arguments)
                      (push path held)))))
        (should (equal (scholia-db-files session) (list file))))
      (cl-letf* ((interchange (symbol-function 'scholia-store-interchange-p))
                 ((symbol-function 'scholia-store-interchange-p)
                  (lambda (path)
                    (dolist (stale (prog1 held (setq held nil)))
                      (when (file-exists-p stale) (delete-file stale)))
                    (funcall interchange path))))
        (scholia-store--migrate session))
      (should (equal (scholia-db-files session) (list file)))
      (should (equal (scholia-db-session-name session) "raced")))))

(ert-deftest scholia-store-a-migrated-session-keeps-what-it-carried ()
  "A session migrated onto a peer's header keeps the fields the peer lacked.
A peer writing in the window where the claim has moved the plist away and
the database is not there yet mints a header naming the file's base and
dated now.  The fold found a header and, all or nothing, wrote none, so
the migrated session's project and description went without a word."
  (scholia-test-with-session-directory
    (let ((session (scholia-store-test--session-file "identity")))
      (with-temp-file session
        (let ((print-length nil))
          (prin1 '(:scholia 1
			    :records ((:file "/nowhere/identity.txt"
					     :annotations nil :checksum "carried"))
			    :session (:name "work"
					    :created "2024-01-01T00:00:00+0000"
					    :project "/nowhere/project"
					    :description "the notes that matter"))
                 (current-buffer))))
      (cl-letf* ((scholia-project-root-function #'ignore)
                 (read (symbol-function 'scholia-store-read-interchange))
                 ((symbol-function 'scholia-store-read-interchange)
                  (lambda (path)
                    (prog1 (funcall read path)
                      (scholia-db-create-session session)))))
        (should (equal (scholia-db-files session)
                       '("/nowhere/identity.txt"))))
      (let ((header (plist-get (scholia-db-interchange session) :session)))
        (should (equal (plist-get header :project) "/nowhere/project"))
        (should (equal (plist-get header :description)
                       "the notes that matter"))))))

(ert-deftest scholia-store-a-bare-string-sqlite-error-is-raised-as-it-came ()
  "A filesystem answering back with a bare string reaches the caller as it is.
Emacs raises `sqlite-error' with data in two shapes, the message, code
and extended code list and a bare string, and the bare one is what a
statement on a database whose file has been taken away raises.  Indexing
it signalled `wrong-type-argument' out of the handler that exists to tell
a corrupt file from a filesystem answering back, so the classification
never happened at all: the user was shown a Lisp type error rather than
anything about the session, and a file that really was corrupt reached
the caller as that same type error instead of as a format error."
  (scholia-test-with-session-directory
    (let* ((session (scholia-store-test--session-file "answering-back"))
           (file "/nowhere/answering-back.txt")
           (record (list :file file :annotations nil :checksum "kept")))
      (scholia-db-store-record session record)
      (cl-letf (((symbol-function 'scholia-store--table-names)
                 (lambda (&rest _)
                   (signal 'sqlite-error (list "disk I/O error")))))
        (should-error (scholia-db-files session) :type 'sqlite-error))
      (should (equal (scholia-db-record session file) record)))))

(ert-deftest scholia-store-rename-keeps-a-subprocess-from-opening-the-old-path ()
  "An opener cannot enter at the old-to-new rename boundary."
  (scholia-test-with-session-directory
    (scholia-store-test--with-flags (opened)
      (let* ((old (scholia-store-test--session-file "race-old"))
             (new (scholia-store-test--session-file "race-new"))
             (rename (symbol-function 'rename-file))
             (writer nil))
        (scholia-db-create-session old)
        (unwind-protect
            (cl-letf (((symbol-function 'rename-file)
                       (lambda (source target &optional overwrite)
                         (if (and (equal source old) (equal target new))
                             (progn
                               (setq writer
                                     (scholia-store-test--start
                                      `(let ((store (scholia-store-open ,old)))
                                         (unwind-protect
                                             (progn
                                               (write-region "" nil ,opened nil 'silent)
                                               (sleep-for 300))
                                           (scholia-store-close store)))))
                               (scholia-store-test--await-ready writer)
                               (sleep-for 0.1)
                               (should-not (file-exists-p opened))
                               (funcall rename source target overwrite))
                           (funcall rename source target overwrite)))))
              (scholia-store-rename old new))
          (when (and writer (process-live-p writer))
            (scholia-store-test--kill writer)))))))

(ert-deftest scholia-store-rejects-evaluating-or-unknown-interchange ()
  "Persisted readers neither evaluate forms nor accept another schema."
  (scholia-test-with-session-directory
    (let ((hostile (scholia-store-test--session-file "hostile"))
          (unknown (scholia-store-test--session-file "unknown"))
          (hostile-row "#.(set 'scholia-store-test--read-eval-sentinel t)\n")
          (assignments (expand-file-name "assignments.eld"
                                         scholia-session-directory))
          (sentinel 'scholia-store-test--read-eval-sentinel))
      (unwind-protect
          (progn
            (set sentinel nil)
            (with-temp-file hostile
              (insert "#.(set 'scholia-store-test--read-eval-sentinel t)\n"))
            (with-temp-file unknown
              (insert "(:scholia 999 :records nil :session nil)\n"))
            (with-temp-file assignments
              (insert "#.(set 'scholia-store-test--read-eval-sentinel t)\n"))
            (should-error (scholia-store-read-interchange hostile)
                          :type 'scholia-db-format-error)
            (should-not (symbol-value sentinel))
            (should-error (scholia-store--parse hostile-row))
            (should-not (symbol-value sentinel))
            (let ((scholia-session-state-file assignments)
                  (scholia-project-sessions nil))
              (scholia-session-load-assignments))
            (should-not (symbol-value sentinel))
            (should-error (scholia-store-read-interchange unknown)
                          :type 'scholia-db-format-error))
        (makunbound sentinel)))))

(provide 'scholia-store-test)
;;; scholia-store-test.el ends here
