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
;; printed plist scholia stored before, the row a record lives in, and
;; the lifecycle of the file on disk.  The database sits at the `.eld'
;; path itself, so the journals beside it are the only files the layout
;; adds and they only exist while a connection is open.
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

(defconst scholia-store--schema
  '("CREATE TABLE IF NOT EXISTS session (key TEXT PRIMARY KEY, value TEXT)"
    "CREATE TABLE IF NOT EXISTS records (file TEXT PRIMARY KEY, record TEXT)")
  "The tables a store is made of, declared at every open.")


;;;; Values

(defun scholia-store--print (value)
  "Return VALUE as the text of a row.
Control characters are printed escaped rather than raw: the column is
decoded with the end of line left undecided, so a carriage return would
come back a newline and silently alter what an annotation carries."
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
  "Return the write-ahead log and shared memory paths beside SESSION-FILE."
  (list (concat session-file "-wal") (concat session-file "-shm")))

(defun scholia-store--discard (session-file)
  "Delete SESSION-FILE and the journals beside it, skipping what is absent."
  (dolist (path (cons session-file (scholia-store--journals session-file)))
    (when (file-exists-p path)
      (delete-file path))))

(defun scholia-store--database-p (session-file)
  "Return non-nil when SESSION-FILE opens with the SQLite magic."
  (and (file-exists-p session-file)
       (equal (with-temp-buffer
                (set-buffer-multibyte nil)
                (insert-file-contents-literally session-file nil 0 16)
                (buffer-string))
              scholia-store--magic)))

(defun scholia-store-interchange-p (session-file)
  "Return non-nil when SESSION-FILE carries a printed database.
A file that is not there and one holding nothing yet answer no: neither
has a format to be wrong about, and both open as an empty database."
  (and (file-exists-p session-file)
       (not (zerop (file-attribute-size (file-attributes session-file))))
       (not (scholia-store--database-p session-file))))


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
FILE is the path the connection is on, which for a store still being
made is a temporary beside DESTINATION rather than DESTINATION itself.
COMMITTED records that a transaction of it reached its commit, which is
what tells `scholia-store-close' the temporary is worth landing."
  file connection destination committed)

(defun scholia-store--connect (session-file)
  "Return a connected store on SESSION-FILE, declaring scholia's schema in it.
SESSION-FILE is scholia's own: one being made, or the temporary a
migration builds.  A statement that fails takes the connection it was
run on down with it rather than leaving it open."
  (let ((connection (sqlite-open session-file)))
    (condition-case nil
        (progn
          (sqlite-pragma connection "journal_mode=wal")
          (dolist (statement scholia-store--schema)
            (sqlite-execute connection statement))
          (scholia-store--make :file session-file :connection connection))
      (error
       (sqlite-close connection)
       (signal 'scholia-db-format-error (list session-file))))))

(defun scholia-store--schema-p (connection)
  "Return non-nil when the database CONNECTION is open on carries both tables."
  (= 2 (caar (sqlite-select
              connection
              "SELECT count(*) FROM sqlite_master WHERE type = 'table' \
AND name IN ('session', 'records')"))))

(defun scholia-store--adopt (session-file)
  "Return a store on the database already at SESSION-FILE.
The tables are read before any statement is run, so a database that is
not scholia's signals `scholia-db-format-error' rather than having
scholia's schema declared into it, and so does a file that opens with
the magic but holds no database to read."
  (let ((connection (sqlite-open session-file)))
    (condition-case nil
        (progn
          (unless (scholia-store--schema-p connection)
            (signal 'scholia-db-format-error (list session-file)))
          (sqlite-pragma connection "journal_mode=wal")
          (scholia-store--make :file session-file :connection connection))
      (error
       (sqlite-close connection)
       (signal 'scholia-db-format-error (list session-file))))))

(defun scholia-store--create (session-file)
  "Return a store on a temporary database that becomes SESSION-FILE.
It is built beside the path it is taking and moved onto it by
`scholia-store-close', so a first save that fails leaves no file at all
rather than a headerless database holding the name against its owner."
  (let ((store (scholia-store--connect
                (make-temp-file
                 (expand-file-name "scholia-store-"
                                   (file-name-directory session-file))))))
    (setf (scholia-store--destination store) session-file)
    store))

(defun scholia-store-open (session-file)
  "Return a store on SESSION-FILE, whatever it carries now.
A SESSION-FILE already holding a database of scholia's is opened as it
stands, one still holding the printed plist scholia stored before is
migrated in place first, and one that is not there yet is made beside
its path and moved onto it once a transaction commits.  Any other file
signals `scholia-db-format-error'.  The connection is the caller's to
close with `scholia-store-close'.

Migrating is what a path gets for being the session's own; one a caller
merely points at is read through `scholia-store-read-interchange',
which leaves it as it found it."
  (let ((file (expand-file-name session-file)))
    (make-directory (file-name-directory file) t)
    (cond ((scholia-store-interchange-p file)
           (scholia-store--migrate file)
           (scholia-store--adopt file))
          ((scholia-store--database-p file)
           (scholia-store--adopt file))
          (t (scholia-store--create file)))))

(defun scholia-store-close (store)
  "Close STORE, taking the write-ahead log beside it along.
Checking the log back into the database is what removes it, and that
only succeeds while nothing else holds the database open, so a second
connection still reading keeps the journals until it closes in turn.

A store still being made lands at the path it was taking when a
transaction of it committed, and goes with its temporary when none did."
  (let ((alone (sqlite-pragma (scholia-store--connection store)
                              "journal_mode=delete"))
        (file (scholia-store--file store))
        (destination (scholia-store--destination store)))
    (sqlite-close (scholia-store--connection store))
    (when alone
      (dolist (journal (scholia-store--journals file))
        (when (file-exists-p journal)
          (delete-file journal))))
    (when destination
      (if (scholia-store--committed store)
          (rename-file file destination t)
        (scholia-store--discard file)))))

(defmacro scholia-store-with-transaction (store &rest body)
  "Evaluate BODY as one transaction on STORE and return its value.
A non-local exit out of BODY rolls the transaction back and carries on
out, so a write cut short leaves the store exactly as BODY found it and
a store still being made is discarded rather than landed.

The commit and the rollback are issued here rather than through
`with-sqlite-transaction' because that macro commits from its own
cleanup form on Emacs 29.1, whichever way its body exited, and 29.1 is
the floor this package declares.  Substituting it back reads as a
simplification and passes on Emacs 30, where the macro rolls back."
  (declare (indent 1) (debug (form body)))
  (let ((opened (make-symbol "store"))
        (done (make-symbol "done")))
    `(let ((,opened ,store)
           (,done nil))
       (sqlite-transaction (scholia-store--connection ,opened))
       (unwind-protect
           (prog1 (progn ,@body)
             (sqlite-commit (scholia-store--connection ,opened))
             (setf (scholia-store--committed ,opened) t)
             (setq ,done t))
         (unless ,done
           (sqlite-rollback (scholia-store--connection ,opened)))))))


;;;; Rows

(defun scholia-store--session (store)
  "Return the session header STORE carries, or nil when it carries none."
  (let ((row (car (sqlite-select
                   (scholia-store--connection store)
                   "SELECT value FROM session WHERE key = 'header'"))))
    (and row (scholia-store--parse (car row)))))

(defun scholia-store-read (store)
  "Return everything STORE carries, as the plist the db API takes.
The records come back in the order they were written, which is where
`scholia-store-put-record' files a replaced one."
  (list :scholia scholia-store-format-version
        :records (mapcar (lambda (row) (scholia-store--parse (car row)))
                         (sqlite-select (scholia-store--connection store)
                                        "SELECT record FROM records \
ORDER BY rowid"))
        :session (scholia-store--session store)))

(defun scholia-store-put-session (store session)
  "Store SESSION as the header STORE's database carries."
  (sqlite-execute (scholia-store--connection store)
                  "REPLACE INTO session VALUES ('header', ?)"
                  (list (scholia-store--print session))))

(defun scholia-store-put-record (store record)
  "Write RECORD into STORE, replacing the row its file keys.
The row is taken out and put back rather than written over, so a record
that returns is filed behind the ones that stayed, which is the order
the whole-value API hands records back in."
  (let ((connection (scholia-store--connection store))
        (file (plist-get record :file)))
    (sqlite-execute connection "DELETE FROM records WHERE file = ?"
                    (list file))
    (sqlite-execute connection "INSERT INTO records VALUES (?, ?)"
                    (list file (scholia-store--print record)))))

(defun scholia-store-write (store db)
  "Store DB in STORE as it stands, as one transaction.
Records STORE holds that DB does not go, so what comes back afterwards
is exactly what DB carried."
  (scholia-store-with-transaction store
    (sqlite-execute (scholia-store--connection store) "DELETE FROM records")
    (scholia-store-put-session store (plist-get db :session))
    (dolist (record (plist-get db :records))
      (scholia-store-put-record store record))))


;;;; The lifecycle of a store on disk

(defun scholia-store--migrate (session-file)
  "Turn the printed plist in SESSION-FILE into a database at that path.
The database is built beside SESSION-FILE and renamed over it, so a
migration that dies before the rename leaves the plist it read byte for
byte and the next open migrates it again.  Migrating in place is what
keeps a session one path: there is no second file for a later open to
find stale."
  (let* ((db (scholia-store-read-interchange session-file))
         (temporary (make-temp-file
                     (expand-file-name "scholia-store-"
                                       (file-name-directory session-file)))))
    (unwind-protect
        (progn
          (let ((store (scholia-store--connect temporary)))
            (unwind-protect
                (scholia-store-write store db)
              (scholia-store-close store)))
          (rename-file temporary session-file t))
      (scholia-store--discard temporary))))

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
