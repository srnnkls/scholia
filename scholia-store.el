;;; scholia-store.el --- The store under a session  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/scholia
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The session file is a SQLite database and this owns it: the
;; connection, the schema, the write-ahead log, the migration off the
;; printed plist scholia stored before, the rows a record and its
;; annotations live in, and the lifecycle of the file on disk.  The
;; database is made at the `.eld' path itself rather than beside it, so
;; the journals are the only files the layout adds while a connection is
;; open, and the one a migration moves the printed plist to while it
;; reads it.
;;
;; A writer takes the write lock for the whole of its transaction and
;; waits `scholia-store-busy-timeout' for one another writer holds, so
;; two Emacs processes on one session serialize rather than overwrite.
;; The first write to a session that is not there yet serializes with
;; them, which is what making the database at its own path buys.
;;
;; This is the one place that maps the stored plist onto rows; every
;; module above it reaches a session through `scholia-db'.

;;; Code:

(require 'cl-lib)
(require 'sqlite)
(require 'scholia-vars)

(unless (sqlite-available-p)
  (error "Scholia needs an Emacs where `sqlite-available-p' answers non-nil"))

(defconst scholia-store-format-version 1
  "Format version tagging a session database.")

(defconst scholia-store--magic "SQLite format 3\0"
  "The sixteen bytes every SQLite database file opens with.")

(defconst scholia-store-busy-timeout 5000
  "Milliseconds a writer waits for a session another writer holds.
A second Emacs replying while this one saves is the designed workflow, so
the wait outlasts a whole session write rather than a statement.")

(defconst scholia-store--schema
  '("CREATE TABLE IF NOT EXISTS session (key TEXT PRIMARY KEY, value TEXT)"
    "CREATE TABLE IF NOT EXISTS records (file TEXT PRIMARY KEY, checksum TEXT)"
    "CREATE TABLE IF NOT EXISTS annotations \
(file TEXT NOT NULL, id TEXT, reply_to TEXT, annotation TEXT NOT NULL)"
    "CREATE INDEX IF NOT EXISTS annotations_by_file ON annotations (file)"
    "CREATE INDEX IF NOT EXISTS annotations_by_id ON annotations (id)"
    "CREATE INDEX IF NOT EXISTS annotations_by_reply_to \
ON annotations (reply_to)")
  "The statements a store is declared by: its tables and the indexes over them.
Declared at every open.  An annotation is a row rather than a field of
the record plist, so appending a reply writes one row and the id and
`:reply-to' a thread is walked by are indexed columns.")

(defconst scholia-store--tables '("session" "records" "annotations")
  "The tables a database of scholia's is known by.
A database at a session path carrying every one of them is scholia's to
write in, one carrying none is a database being made, and one carrying
other tables is somebody else's and is refused.")


;;;; Values

(defun scholia-store--print (value)
  "Return VALUE as the text of a row.
Control characters are printed escaped rather than raw: the column is
decoded with the end of line left undecided, so a carriage return would
come back a newline and silently alter what an annotation carries, or
the path a record is keyed by, which is a record no listing can find and
no export can carry."
  (let ((print-length nil)
        (print-level nil)
        (print-escape-control-characters t))
    (prin1-to-string value)))

(defun scholia-store--parse (text)
  "Return the value the row text TEXT carries."
  (let ((read-circle nil))
    (car (read-from-string text))))


;;;; The file on disk

(defun scholia-store--journals (session-file)
  "Return the journal paths beside SESSION-FILE.
The write-ahead log and the shared memory beside it, and the rollback
journal a database in delete mode keeps a transaction in: delete mode is
what every close leaves a session in, so a process killed in that window
leaves one behind and nothing else in the package ever names it."
  (list (concat session-file "-wal")
        (concat session-file "-shm")
        (concat session-file "-journal")))

(defun scholia-store--discard (session-file)
  "Delete SESSION-FILE and the journals beside it, skipping what is absent."
  (dolist (path (cons session-file (scholia-store--journals session-file)))
    (when (file-exists-p path)
      (delete-file path))))

(defun scholia-store--database-p (session-file)
  "Return non-nil when SESSION-FILE opens with the SQLite magic.
A file that is not there answers no, and so does one taken away between
the test and the read rather than signalling: the plist a session still
holds is renamed out of the way by the Emacs migrating it, which a
second Emacs may be reading the head of at that moment."
  (equal (condition-case nil
             (with-temp-buffer
               (set-buffer-multibyte nil)
               (insert-file-contents-literally session-file nil 0 16)
               (buffer-string))
           (file-missing nil))
         scholia-store--magic))

(defun scholia-store-interchange-p (session-file)
  "Return non-nil when SESSION-FILE carries a printed database.
A file that is not there and one holding nothing yet answer no: neither
has a format to be wrong about, and both open as an empty database.  The
size is read off one stat rather than a test and a read of it, since a
session is renamed away by the Emacs migrating it."
  (let ((size (file-attribute-size (file-attributes session-file))))
    (and size
         (not (zerop size))
         (not (scholia-store--database-p session-file)))))


;;;; The interchange plist

(defun scholia-store-read-interchange (session-file)
  "Return the database the printed plist in SESSION-FILE carries.
SESSION-FILE is left byte for byte, so a file the caller merely points
at is read rather than taken over.  One carrying content but no
`:scholia' version tag, and one the reader cannot parse, signal
`scholia-db-format-error' rather than being guessed at.  Circular `#N='
references are refused rather than read, so an imported session cannot
hand the accessors a list they never return from."
  (let ((db (with-temp-buffer
              (insert-file-contents session-file)
              (goto-char (point-min))
              (let ((read-circle nil))
                (condition-case nil
                    (read (current-buffer))
                  (error
                   (signal 'scholia-db-format-error (list session-file))))))))
    (unless (and (plistp db) (plist-get db :scholia))
      (signal 'scholia-db-format-error (list session-file)))
    db))

(defun scholia-store-write-interchange (session-file db)
  "Write DB into SESSION-FILE as the printed plist a session travels as.
DB is written to a temporary file beside SESSION-FILE and renamed over
it, which within one directory is atomic, so a write cut short leaves
whatever was there before it whole."
  (let* ((target (expand-file-name session-file))
         (directory (file-name-directory target)))
    (make-directory directory t)
    (let ((temporary (make-temp-file
                      (expand-file-name "scholia-store-" directory)))
          (print-length nil)
          (print-level nil))
      (unwind-protect
          (progn
            (with-temp-file temporary
              (prin1 db (current-buffer))
              (insert "\n"))
            (rename-file temporary target t))
        (when (file-exists-p temporary)
          (delete-file temporary))))))


;;;; The connection

(cl-defstruct (scholia-store (:constructor scholia-store--make)
                             (:conc-name scholia-store--)
                             (:copier nil))
  "A connection open on a session database.
FILE is the session path the connection is on.  CREATED records that the
database was made by this open and COMMITTED that a transaction of it
reached its commit: a database made here that committed nothing is one
no session was ever stored in, which `scholia-store-close' unlinks."
  file connection created committed)

(defun scholia-store--sqlite (session-file)
  "Return a connection on SESSION-FILE that waits out another writer's lock.
The `busy_timeout' is set before anything else runs on the connection: a
writer whose first statement meets `SQLITE_BUSY' reaches its caller as
`scholia-db-format-error' rather than as the wait it was meant to be."
  (let ((connection (sqlite-open session-file)))
    (sqlite-pragma connection
                   (format "busy_timeout=%d" scholia-store-busy-timeout))
    connection))

(defconst scholia-store--unreadable '(11 26)
  "The SQLite result codes of a file carrying no database to read.
`SQLITE_CORRUPT' and `SQLITE_NOTADB'.  Every other code comes off a
database that reads: one a peer holds, or a filesystem answering back.")

(defun scholia-store--unreadable-p (failure)
  "Return non-nil when FAILURE names a file carrying no database to read.
The schema probe's own `scholia-db-format-error' says so too: it is the
one thing that reads the database and then refuses it.

Emacs raises `sqlite-error' carrying either the message, code and
extended code list or a bare string, and the bare one is what a statement
on a database whose file has been taken away raises.  Indexed rather than
tested, it signalled `wrong-type-argument' out of the handler that calls
this, so nothing was classified at all and a file that really was corrupt
reached the caller as a Lisp type error."
  (let ((data (cadr failure)))
    (or (eq (car failure) 'scholia-db-format-error)
        (and (memq 'sqlite-error (get (car failure) 'error-conditions))
             (consp data)
             (memq (nth 2 data) scholia-store--unreadable)))))

(defun scholia-store--opening (connection session-file open)
  "Return what OPEN takes from CONNECTION on SESSION-FILE, or refuse the file.
A failure saying SESSION-FILE carries no database reaches the caller as
`scholia-db-format-error', and every other one as it was raised: a peer
holding the write lock past `scholia-store-busy-timeout' and a
filesystem answering back are not a file that is scholia's to refuse,
and reported as one they take a healthy session out of
`scholia-session-list' rather than into it.  CONNECTION is closed either
way rather than left open on a file nothing came of."
  (condition-case failure
      (funcall open)
    (error
     (sqlite-close connection)
     (if (scholia-store--unreadable-p failure)
         (signal 'scholia-db-format-error (list session-file))
       (signal (car failure) (cdr failure))))))

(defun scholia-store--table-names (connection)
  "Return the tables the database CONNECTION is open on carries."
  (mapcar #'car
          (sqlite-select connection
                         "SELECT name FROM sqlite_master \
WHERE type = 'table'")))

(defun scholia-store--declare (connection session-file)
  "Declare scholia's schema in the database CONNECTION is on for SESSION-FILE.
The statements run as one transaction, so a second Emacs making the same
session at the same moment reads either none of the tables or all of
them.  Carrying some of them, a database of scholia's is one nothing can
tell from a stranger's."
  (sqlite-execute connection "BEGIN IMMEDIATE")
  (dolist (statement scholia-store--schema)
    (sqlite-execute connection statement))
  (scholia-store--must (sqlite-commit connection)
                       "Could not commit to %s" session-file))

(defun scholia-store--adopt (connection session-file)
  "Return a store reading the database CONNECTION found at SESSION-FILE."
  (sqlite-pragma connection "journal_mode=wal")
  (scholia-store--make :file session-file :connection connection))

(defun scholia-store--create (connection session-file made)
  "Return a store on the database CONNECTION is making at SESSION-FILE.
MADE says this open is what put the file there, which is the one thing
`scholia-store-close' may unlink on.  A database carrying no table at all
is also what another Emacs leaves between its `journal_mode=wal' pragma
and its schema commit, and what one killed in that window leaves behind,
so the branch is taken on files this open did not make."
  (sqlite-pragma connection "journal_mode=wal")
  (scholia-store--declare connection session-file)
  (scholia-store--make :file session-file
                       :connection connection
                       :created made))

(defun scholia-store--open-database (session-file)
  "Return a store on the database at SESSION-FILE, making it when there is none.
The tables are read before any statement is run, so a database that is
not scholia's signals `scholia-db-format-error' rather than having
scholia's schema declared into it, and so does a file that opens with
the magic but holds no database to read.  One carrying no table at all
is a database being made, here or by another Emacs at this moment, and
scholia's schema is declared into it.

A statement that fails takes the connection it was run on down with it
rather than leaving it open, and the file with it when this open is what
made it: a session path holding a database no schema could be declared
in carries nothing anybody stored, and holds a name against its owner."
  (let ((absent (not (file-exists-p session-file)))
        (connection (scholia-store--sqlite session-file))
        (making nil))
    (condition-case failure
        (scholia-store--opening
         connection session-file
         (lambda ()
           (let ((tables (scholia-store--table-names connection)))
             (cond ((cl-subsetp scholia-store--tables tables :test #'equal)
                    (scholia-store--adopt connection session-file))
                   (tables
                    (signal 'scholia-db-format-error (list session-file)))
                   (t (setq making absent)
                      (scholia-store--create connection session-file
                                             making))))))
      (error
       (when making
         (scholia-store--discard session-file))
       (signal (car failure) (cdr failure))))))

(defun scholia-store-open (session-file)
  "Return a store on SESSION-FILE, whatever it carries now.
A SESSION-FILE already holding a database of scholia's is opened as it
stands, one still holding the printed plist scholia stored before is
migrated into a database at that same path first, and one that is not
there yet is made at the path itself rather than beside it: the write
lock a transaction takes is then on the session for the first writer of
it as much as for the twentieth, and two Emacs processes first-writing
one session serialize rather than land one over the other.  Any other
file signals `scholia-db-format-error'.  The connection is the caller's
to close with `scholia-store-close'.

Migrating is what a path gets for being the session's own; one a caller
merely points at is read through `scholia-store-read-interchange',
which leaves it as it found it."
  (let ((file (expand-file-name session-file)))
    (make-directory (file-name-directory file) t)
    (when (scholia-store-interchange-p file)
      (scholia-store--migrate file))
    (scholia-store--open-database file)))

(defun scholia-store--sweep-journals (connection file)
  "Take the journals beside FILE out, with CONNECTION alone on the database.
Leaving the log mode checks the log back into the database and unlinks
the log; the shared memory beside it survives that, and nothing but this
takes it away.

CONNECTION claims the database before the mode changes and holds the
claim until it closes, which is what exclusive locking mode buys: the
file lock is kept between statements rather than released, so no log can
be opened again between the check-in and the sweep and whatever is swept
is stale by construction.  Unlinking a log another connection is writing
into loses everything that connection committed, silently, with the
database left claiming to hold it, which is the very failure this store
exists to end.  The claim is therefore made with no wait at all: a
database anything else is on keeps its journals until whoever holds it
closes in turn."
  (condition-case nil
      (progn
        (sqlite-pragma connection "busy_timeout=0")
        (sqlite-pragma connection "locking_mode=exclusive")
        (sqlite-execute connection "BEGIN IMMEDIATE")
        (sqlite-commit connection)
        (when (sqlite-pragma connection "journal_mode=delete")
          (dolist (journal (scholia-store--journals file))
            (when (file-exists-p journal)
              (delete-file journal)))))
    (error nil)))

(defun scholia-store--unborn-p (store)
  "Return non-nil when STORE is a database made here holding no session.
Nothing committed and no header in it: nothing was ever stored under the
name it holds, which `scholia-session-list' cannot show and
`scholia-session-create' would find taken.  A header this cannot read
answers no, since a close unlinks nothing it could not read."
  (and (scholia-store--created store)
       (not (scholia-store--committed store))
       (condition-case nil
           (not (scholia-store-session store))
         (error nil))))

(defun scholia-store-close (store)
  "Close STORE, taking the write-ahead log beside it along.
The log is checked back into the database and the journals beside it go,
which only happens while nothing else holds the database open: a second
connection still reading keeps them until it closes in turn.

A database made by this open that nothing committed to goes with its
journals rather than staying as a session file carrying no session.  It
is what a first save that fails leaves, and the name it would hold is
one `scholia-session-list' cannot show and `scholia-session-create'
finds taken: a session the user can neither open nor create.  Another
Emacs first-writing the same session at the same moment is what the
header is read for, its commit being the one thing that makes the file
worth more than this store put in it."
  (let ((unborn (scholia-store--unborn-p store)))
    (scholia-store--sweep-journals (scholia-store--connection store)
                                   (scholia-store--file store))
    (sqlite-close (scholia-store--connection store))
    (when unborn
      (scholia-store--discard (scholia-store--file store)))))

(defun scholia-store--must (result format file)
  "Return RESULT, signalling FORMAT filled with FILE when it is nil.
The SQLite calls a transaction ends with answer nil rather than
signalling when they fail, so a caller that does not read them is told a
transaction landed by the very call that says it did not."
  (or result
      (signal 'scholia-error (list (format format file)))))

(defmacro scholia-store-with-transaction (store &rest body)
  "Evaluate BODY as one transaction on STORE and return its value.
A non-local exit out of BODY rolls the transaction back and carries on
out, so a write cut short leaves the store exactly as BODY found it and
a database made for this write is unlinked rather than left holding a
session name.

The transaction opens `BEGIN IMMEDIATE', which takes the write lock
before BODY reads anything: a second writer then waits out
`scholia-store-busy-timeout' rather than reading a snapshot it cannot
upgrade, and the read and the write BODY does are one step nothing
commits between.  It is issued as a statement because the TYPE argument
of `sqlite-transaction' only exists from Emacs 30, and 29.1 is the floor
this package declares.

The commit and the rollback are issued here rather than through
`with-sqlite-transaction' because that macro commits from its own
cleanup form on Emacs 29.1, whichever way its body exited.  Substituting
it back reads as a simplification and passes on Emacs 30, where the
macro rolls back.

Both are read rather than issued and forgotten: `sqlite-commit' answers
nil rather than signalling when the commit fails, which a full disk
alone is enough for, so a transaction that stored nothing would return
to its caller as a write that landed, with the rollback skipped and the
database it never wrote left holding the session name."
  (declare (indent 1) (debug (form body)))
  (let ((opened (make-symbol "store"))
        (done (make-symbol "done")))
    `(let ((,opened ,store)
           (,done nil))
       (sqlite-execute (scholia-store--connection ,opened) "BEGIN IMMEDIATE")
       (unwind-protect
           (prog1 (progn ,@body)
             (scholia-store--must
              (sqlite-commit (scholia-store--connection ,opened))
              "Could not commit to %s" (scholia-store--file ,opened))
             (setf (scholia-store--committed ,opened) t)
             (setq ,done t))
         (unless ,done
           (scholia-store--must
            (sqlite-rollback (scholia-store--connection ,opened))
            "Could not roll back %s" (scholia-store--file ,opened)))))))

(defmacro scholia-store-with-snapshot (store &rest body)
  "Evaluate BODY on one snapshot of STORE's database and return its value.
BODY reads inside a transaction, so every statement it runs answers out
of the database as the first of them found it.  A record read as a
checksum and then as the annotations under it therefore cannot come back
carrying the checksum one writer committed and the annotations of the
next: that record reads as one whose stored positions hold, and the
annotations of the other commit are then placed against content they
were never made against, silently.

The transaction is rolled back whichever way BODY exits, a read having
nothing to land, and it ends before the caller closes: the journal sweep
a close runs cannot claim a database its own connection still holds a
transaction open on.  `BEGIN' is issued as a statement for the reason
`scholia-store-with-transaction' issues its own."
  (declare (indent 1) (debug (form body)))
  (let ((opened (make-symbol "store")))
    `(let ((,opened ,store))
       (sqlite-execute (scholia-store--connection ,opened) "BEGIN")
       (unwind-protect
           (progn ,@body)
         (sqlite-rollback (scholia-store--connection ,opened))))))


;;;; Rows

(defun scholia-store-session (store)
  "Return the session header STORE carries, or nil when it carries none."
  (let ((row (car (sqlite-select
                   (scholia-store--connection store)
                   "SELECT value FROM session WHERE key = 'header'"))))
    (and row (scholia-store--parse (car row)))))

(defun scholia-store-put-session (store session)
  "Store SESSION as the header STORE's database carries."
  (sqlite-execute (scholia-store--connection store)
                  "REPLACE INTO session VALUES ('header', ?)"
                  (list (scholia-store--print session))))

(defun scholia-store-files (store)
  "Return the files STORE carries a record for, oldest row first."
  (mapcar (lambda (row) (scholia-store--parse (car row)))
          (sqlite-select (scholia-store--connection store)
                         "SELECT file FROM records ORDER BY rowid")))

(defun scholia-store-annotations (store file)
  "Return the annotations STORE carries for FILE, oldest row first."
  (mapcar (lambda (row) (scholia-store--parse (car row)))
          (sqlite-select (scholia-store--connection store)
                         "SELECT annotation FROM annotations \
WHERE file = ? ORDER BY rowid"
                         (list (scholia-store--print file)))))

(defun scholia-store-record (store file)
  "Return the record STORE carries for FILE, or nil when it carries none."
  (let ((row (car (sqlite-select
                   (scholia-store--connection store)
                   "SELECT checksum FROM records WHERE file = ?"
                   (list (scholia-store--print file))))))
    (when row
      (list :file file
            :annotations (scholia-store-annotations store file)
            :checksum (car row)))))

(defun scholia-store-add-annotation (store file annotation)
  "Append ANNOTATION to the annotations STORE carries for FILE.
One row is added rather than the record rewritten, so an annotation
another writer filed under FILE meanwhile stays where it is."
  (sqlite-execute (scholia-store--connection store)
                  "INSERT INTO annotations VALUES (?, ?, ?, ?)"
                  (list (scholia-store--print file)
                        (plist-get annotation :id)
                        (plist-get annotation :reply-to)
                        (scholia-store--print annotation))))

(defun scholia-store-remove-record (store file)
  "Take the record FILE keys, and the annotations under it, out of STORE."
  (let ((connection (scholia-store--connection store))
        (key (list (scholia-store--print file))))
    (sqlite-execute connection "DELETE FROM annotations WHERE file = ?" key)
    (sqlite-execute connection "DELETE FROM records WHERE file = ?" key)))

(defun scholia-store-put-record (store record)
  "Write RECORD into STORE, replacing the rows its file keys.
The rows are taken out and put back rather than written over, so a
record that returns is filed behind the ones that stayed, which is the
order records and annotations come back in."
  (let ((file (plist-get record :file)))
    (scholia-store-remove-record store file)
    (sqlite-execute (scholia-store--connection store)
                    "INSERT INTO records VALUES (?, ?)"
                    (list (scholia-store--print file)
                          (plist-get record :checksum)))
    (dolist (annotation (plist-get record :annotations))
      (scholia-store-add-annotation store file annotation))))

(defun scholia-store-read (store)
  "Return everything STORE carries, as the plist a session travels as."
  (list :scholia scholia-store-format-version
        :records (mapcar (lambda (file) (scholia-store-record store file))
                         (scholia-store-files store))
        :session (scholia-store-session store)))

(defun scholia-store--fold-header (stored header)
  "Return HEADER folded into STORED field by field.
A field STORED carries stands and one it leaves nil takes HEADER's, so a
header a peer minted while the session path stood empty keeps the name
and the moment it wrote and gives back the project and the description it
had no way to know."
  (let ((folded (copy-sequence stored)))
    (while header
      (let ((key (pop header))
            (value (pop header)))
        (unless (plist-get folded key)
          (setq folded (plist-put folded key value)))))
    folded))

(defun scholia-store--fold (store db)
  "Fold DB into STORE, keeping everything STORE already carries.
The header is folded key by key, a record lands whole only where STORE
has none for its file, and where it has one the annotations DB carries
that are not in it by id are appended.

A migration is not the only writer at the path it lands on: the Emacs
that did not migrate is writing there meanwhile, with nothing to say
whether its reply commits before this fold or after it, and a wholesale
write would take that reply out either way.  Written all or nothing, the
header that peer minted on the empty path stood alone and the migrated
session's project and description went with the fold that found it."
  (let* ((stored (scholia-store-session store))
         (folded (scholia-store--fold-header stored (plist-get db :session))))
    (unless (equal folded stored)
      (scholia-store-put-session store folded)))
  (dolist (record (plist-get db :records))
    (let* ((file (plist-get record :file))
           (stored (scholia-store-record store file)))
      (if (not stored)
          (scholia-store-put-record store record)
        (let ((ids (mapcar (lambda (annotation) (plist-get annotation :id))
                           (plist-get stored :annotations))))
          (dolist (annotation (plist-get record :annotations))
            (unless (member (plist-get annotation :id) ids)
              (scholia-store-add-annotation store file annotation))))))))


;;;; The lifecycle of a store on disk

(defun scholia-store--claim (session-file aside)
  "Move the printed plist at SESSION-FILE to ASIDE, and say whether it moved.
The move is what decides which of two Emacs processes migrates one
session: a file that is there can be renamed by one of them only, and
the other finds it gone and opens the database that takes its place.

What moved is read back as a plist and put where it was when it is not
one, since the other process may have finished migrating between the
check and the rename, and a database moved aside is a session gone.

Every `file-error' answers that the file was not taken, not
`file-missing' alone: a rename onto a file another process holds open is
a plain `file-error' on Windows, and caught nowhere it escaped
`scholia-store-open' rather than reaching the open of the database that
took the session's place."
  (and (condition-case nil
           (progn (rename-file session-file aside t) t)
         (file-error nil))
       (or (scholia-store-interchange-p aside)
           (progn (rename-file aside session-file t) nil))))

(defun scholia-store--migrate (session-file)
  "Turn the printed plist in SESSION-FILE into a database at that path.
The plist is moved aside and the database made at the path it held, so a
migration takes the write lock every other writer takes, on the session
itself.  Migrating to that same path is what keeps a session one path:
there is no second file for a later open to find stale.

The copy moved aside goes once the fold has committed and is put back
when it has not, so a migration that fails leaves the plist for the next
open to migrate again.  It is left where it is rather than put back over
a session another Emacs made at the path meanwhile.  The plist is read
inside that same guard: refused by the reader — truncated, carrying no
`:scholia' tag, a file of the user's own that merely sits in the session
directory — it was renamed away and the signal left with the copy
stranded under a name nothing reads and `scholia-session-list' cannot
glob.

The aside is named for this claimer alone rather than after the session,
so no second claimer can name it.  Sharing one name, the loser's rename
landed on the winner's copy once the winner had made the database at the
session path, and the winner's cleanup then deleted the database: nothing
at the path and nothing at the aside."
  (let ((aside (make-temp-name (concat session-file "-migrating-"))))
    (when (scholia-store--claim session-file aside)
      (let ((db nil)
            (folded nil))
        (unwind-protect
            (progn
              (setq db (scholia-store-read-interchange aside))
              (let ((store (scholia-store--open-database session-file)))
                (unwind-protect
                    (progn (scholia-store-with-transaction store
                             (scholia-store--fold store db))
                           (setq folded t))
                  (scholia-store-close store))))
          (cond (folded (delete-file aside))
                ((not (file-exists-p session-file))
                 (rename-file aside session-file t))))))))

(defun scholia-store-delete (session-file)
  "Delete the database SESSION-FILE and the journals beside it.
Unlinking the database alone orphans them, and the next store made at
the same path then either fails to open or comes back holding what the
deleted one held."
  (scholia-store--discard session-file))

(defun scholia-store-rename (old new)
  "Move the database OLD to NEW, leaving nothing of OLD behind.
Opening and closing it once first checks the write-ahead log back into
the database, so the single file that moves carries all of it and no
journal is left pointing at a name that is gone.  A log surviving that
close is another connection holding OLD open: the move is refused and
OLD left whole, since `rename-file' moves the database alone and the
commits still in the log would go with the journals behind it."
  (when (scholia-store--database-p old)
    (scholia-store-close (scholia-store-open old))
    (when (file-exists-p (car (scholia-store--journals old)))
      (signal 'scholia-error
              (list (format "Another connection still holds %s" old)))))
  (rename-file old new)
  (scholia-store--discard old))

(provide 'scholia-store)
;;; scholia-store.el ends here
