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

(let ((load-prefer-newer t))
  (require 'scholia-vars nil t)
  (require 'scholia-db nil t)
  (require 'scholia-store nil t))

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
              (scholia-db-record (scholia-db-load session) file)))))


;;;; The `.eld' boundary

(ert-deftest scholia-store-load-dispatches-on-the-sqlite-magic ()
  "A printed-plist session migrates in place; a store opens straight through.
The migration happens once, at the same path, so a later open finds a
database rather than a plist and nothing stale is left for it to find."
  (scholia-test-with-session-directory
    (let ((session (scholia-store-test--session-file "legacy")))
      (scholia-store-test--write-legacy session)
      (should-not (equal (scholia-test-file-magic session)
                         scholia-test-sqlite-magic))
      (let ((db (scholia-db-load session)))
        (should (equal (scholia-db-files db) '("/nowhere/legacy.txt")))
        (should (equal (scholia-db-session-name db) "legacy")))
      (should (equal (scholia-test-file-magic session)
                     scholia-test-sqlite-magic))
      (should (equal (scholia-store-test--note session "/nowhere/legacy.txt")
                     "first note"))
      (should (equal (scholia-db-record-checksum
                      (scholia-db-record (scholia-db-load session)
                                         "/nowhere/legacy.txt"))
                     "legacy-checksum"))
      (let* ((migrated (scholia-db-record-annotations
                        (scholia-db-record (scholia-db-load session)
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
        (ignore-errors (scholia-db-load session)))
      (should (equal (scholia-store-test--bytes session) before))
      (should-not (equal (scholia-test-file-magic session)
                         scholia-test-sqlite-magic))
      (let ((db (scholia-db-load session)))
        (should (equal (scholia-db-files db) '("/nowhere/legacy.txt")))
        (should (equal (scholia-db-session-name db) "interrupted")))
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
      (should (equal (scholia-db-record (scholia-db-load session) file)
                     record)))))


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
        (should-error (scholia-db-load foreign) :type 'scholia-db-format-error)
        (should (equal (scholia-store-test--bytes foreign) before)))
      (let ((coding-system-for-write 'binary))
        (with-temp-file broken
          (set-buffer-multibyte nil)
          (insert scholia-test-sqlite-magic "nothing readable follows")))
      (should-error (scholia-db-load broken) :type 'scholia-db-format-error))))


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
        (should-error (scholia-db-write
                       session
                       (list :scholia scholia-store-format-version
                             :records nil))))
      (should-not (file-exists-p session)))))

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
      (scholia-db-write old (list :scholia scholia-store-format-version
                                  :records nil))
      (let ((store (scholia-store-open old)))
        (unwind-protect
            (progn
              (scholia-store-with-transaction store
                (scholia-store-put-record store record))
              (should-error (scholia-store-rename old new)
                            :type 'scholia-error))
          (scholia-store-close store)))
      (should-not (file-exists-p new))
      (should (equal (scholia-db-record (scholia-db-load old) file) record)))))

(provide 'scholia-store-test)
;;; scholia-store-test.el ends here
