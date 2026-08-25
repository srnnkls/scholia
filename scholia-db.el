;;; scholia-db.el --- The scholia record store  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/scholia
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Reads and writes the session databases scholia annotates into.  The
;; stored form is a plist tagged with its format version.

;;; Code:

(require 'seq)
(require 'scholia-vars)

(defconst scholia-db--version 1
  "Format version tagging a session database.")

(defun scholia-db--with-fields (plist &rest fields)
  "Return a copy of PLIST carrying the key and value pairs of FIELDS."
  (let ((copy (copy-sequence plist)))
    (while fields
      (setq copy (plist-put copy (pop fields) (pop fields))))
    copy))


;;;; Sessions

(defun scholia-db-session-name (db)
  "Return the name of the session DB belongs to."
  (plist-get (plist-get db :session) :name))

(defun scholia-db-session-created (db)
  "Return when the session DB belongs to was first written."
  (plist-get (plist-get db :session) :created))

(defun scholia-db--publish-session (db session-file)
  "Return DB with a session header naming it after SESSION-FILE.
A header DB already carries is left alone, so `:created' keeps the
moment of the first write."
  (if (plist-get db :session)
      db
    (scholia-db--with-fields
     db :session (list :name (file-name-base session-file)
                       :created (format-time-string "%FT%T%z")
                       :project (funcall scholia-project-root-function)
                       :description nil))))


;;;; Records

(defun scholia-db-make-record (file annotations checksum)
  "Return a record holding ANNOTATIONS of FILE fingerprinted by CHECKSUM."
  (list :file file :annotations annotations :checksum checksum))

(defun scholia-db-record-file (record)
  "Return the absolute file name RECORD annotates."
  (plist-get record :file))

(defun scholia-db-record-checksum (record)
  "Return the fingerprint the file had when RECORD was written."
  (plist-get record :checksum))

(defun scholia-db-record-annotations (record)
  "Return the annotations RECORD holds."
  (plist-get record :annotations))

(defun scholia-db-files (db)
  "Return the files DB holds a record for."
  (mapcar #'scholia-db-record-file (plist-get db :records)))

(defun scholia-db-record (db file)
  "Return the record DB holds for FILE, or nil when it holds none."
  (seq-find (lambda (record)
              (equal (scholia-db-record-file record) file))
            (plist-get db :records)))

(defun scholia-db--records-without (db file)
  "Return the records of DB except the one keyed by FILE."
  (seq-remove (lambda (record)
                (equal (scholia-db-record-file record) file))
              (plist-get db :records)))

(defun scholia-db-put-record (db record)
  "Return DB with RECORD replacing the record for the same file."
  (scholia-db--with-fields
   db :records (append (scholia-db--records-without
                        db (scholia-db-record-file record))
                       (list record))))

(defun scholia-db-remove-record (db file)
  "Return DB without its record for FILE."
  (scholia-db--with-fields db :records (scholia-db--records-without db file)))


;;;; Annotations

(defun scholia-db-annotation-id (annotation)
  "Return the id of ANNOTATION."
  (plist-get annotation :id))

(defun scholia-db-annotation-beg (annotation)
  "Return where the text ANNOTATION covers begins."
  (plist-get annotation :beg))

(defun scholia-db-annotation-end (annotation)
  "Return where the text ANNOTATION covers ends."
  (plist-get annotation :end))

(defun scholia-db-annotation-text (annotation)
  "Return the note ANNOTATION carries."
  (plist-get annotation :text))

(defun scholia-db-annotation-annotated-text (annotation)
  "Return the buffer text ANNOTATION covers."
  (plist-get annotation :annotated-text))

(defun scholia-db-annotation-line (annotation)
  "Return the line ANNOTATION was written against."
  (plist-get annotation :line))

(defun scholia-db-annotation-line-text (annotation)
  "Return the source line ANNOTATION was written against."
  (plist-get annotation :line-text))

(defun scholia-db-annotation-column (annotation)
  "Return the column within its line where ANNOTATION begins."
  (plist-get annotation :column))

(defun scholia-db-annotation-end-column (annotation)
  "Return the column within its line where ANNOTATION ends."
  (plist-get annotation :end-column))

(defun scholia-db-annotation-reply-to (annotation)
  "Return the id of the annotation ANNOTATION answers, or nil."
  (plist-get annotation :reply-to))

(defun scholia-db-annotation-reply-p (annotation)
  "Return non-nil when ANNOTATION answers another annotation."
  (and (scholia-db-annotation-reply-to annotation) t))

(defun scholia-db-annotation-interval (annotation)
  "Return the half-open interval ANNOTATION covers as a cons."
  (cons (scholia-db-annotation-beg annotation)
        (scholia-db-annotation-end annotation)))

(defun scholia-db-annotation-set-text (annotation text)
  "Return ANNOTATION carrying TEXT as its note."
  (scholia-db--with-fields annotation :text text))

(defun scholia-db-annotation-set-bounds (annotation beg end)
  "Return ANNOTATION covering BEG to END."
  (scholia-db--with-fields annotation :beg beg :end end))


;;;; The I/O entry points

(defun scholia-db--empty ()
  "Return a database holding no records."
  (list :scholia scholia-db--version :records nil))

(defun scholia-db-load (session-file)
  "Return the database stored in SESSION-FILE.
A SESSION-FILE that does not exist reads as an empty database.  One
carrying no `:scholia' version tag, and one the reader cannot parse,
signal `scholia-db-format-error' rather than being guessed at.  Circular
`#N=' references are refused rather than read, so an imported session
cannot hand the accessors a list they never return from."
  (if (not (file-exists-p session-file))
      (scholia-db--empty)
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
      db)))

(defun scholia-db--write (session-file db)
  "Write DB into SESSION-FILE.
DB is written to a temporary file beside SESSION-FILE and renamed over
it, which within one directory is atomic, so a write cut short leaves
the session that was there before it whole."
  (let* ((target (expand-file-name session-file))
         (directory (file-name-directory target)))
    (make-directory directory t)
    (let ((temporary (make-temp-file
                      (expand-file-name "scholia-db-" directory)))
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

(defun scholia-db--snapshot (annotation)
  "Return ANNOTATION with its source context taken from the current buffer.
The context is `:line', `:line-text', `:column' and `:end-column', which
together locate the annotation once the file itself is gone.  Both
columns index into `:line-text', so an annotation reaching further down
ends where that line ends.  Replies hold no position and pass through
untouched."
  (if (scholia-db-annotation-reply-p annotation)
      annotation
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (scholia-db-annotation-beg annotation))
        (let ((bol (line-beginning-position)))
          (scholia-db--with-fields
           annotation
           :line (line-number-at-pos (point) t)
           :line-text (buffer-substring-no-properties bol (line-end-position))
           :column (- (point) bol)
           :end-column (- (min (scholia-db-annotation-end annotation)
                               (line-end-position))
                          bol)))))))

(defun scholia-db-save (session-file file annotations checksum)
  "Store ANNOTATIONS of FILE with CHECKSUM into SESSION-FILE.
Source context is snapshot from the current buffer, which is the one
visiting FILE.  Replies already stored for FILE are folded back in as
they are, since they carry no position to recompute."
  (let* ((db (scholia-db--publish-session (scholia-db-load session-file)
                                          session-file))
         (snapshots (mapcar #'scholia-db--snapshot annotations))
         (ids (mapcar #'scholia-db-annotation-id snapshots))
         (replies (seq-filter
                   (lambda (annotation)
                     (and (scholia-db-annotation-reply-p annotation)
                          (not (member (scholia-db-annotation-id annotation)
                                       ids))))
                   (scholia-db-record-annotations
                    (scholia-db-record db file)))))
    (scholia-db--write session-file
                       (scholia-db-put-record
                        db (scholia-db-make-record
                            file (append snapshots replies) checksum)))))


;;;; The checksum-drift re-search

(defun scholia-db--search-window (anchor)
  "Return the region around ANCHOR a match may begin in, its end excluded.
The region reaches `scholia-search-region-lines-delta' lines either way,
so its end is the start of the first line beyond them."
  (cons (save-excursion
          (goto-char anchor)
          (forward-line (- scholia-search-region-lines-delta))
          (point))
        (save-excursion
          (goto-char anchor)
          (forward-line (1+ scholia-search-region-lines-delta))
          (point))))

(defun scholia-db--begins-within-p (position window)
  "Return non-nil when POSITION lies in WINDOW, whose end is excluded."
  (and (<= (car window) position)
       (< position (cdr window))))

(defun scholia-db--nearest-match (text anchor window)
  "Return the bounds of the TEXT nearest ANCHOR beginning within WINDOW.
WINDOW bounds where a match begins, so a multi-line one may end past it."
  (let ((before (save-excursion
                  (goto-char (min (point-max) (+ anchor (length text))))
                  (and (search-backward text (car window) t)
                       (scholia-db--begins-within-p (match-beginning 0) window)
                       (cons (match-beginning 0) (match-end 0)))))
        (after (save-excursion
                 (goto-char anchor)
                 (and (search-forward text
                                      (min (point-max)
                                           (+ (cdr window) (length text)))
                                      t)
                      (scholia-db--begins-within-p (match-beginning 0) window)
                      (cons (match-beginning 0) (match-end 0))))))
    (cond ((null before) after)
          ((null after) before)
          ((<= (- anchor (car before)) (- (car after) anchor)) before)
          (t after))))

(defun scholia-db--relocate (annotation)
  "Return ANNOTATION moved onto its text in the current buffer.
The text is searched within `scholia-search-region-lines-delta' lines of
the stored position and the occurrence nearest it is taken, so a text
repeated in the window keeps every annotation on its own occurrence.
Nil is returned when the text is not there.  The id is kept, so send
records still reference the annotation they were made for.  Replies hold
no position and pass through untouched."
  (if (scholia-db-annotation-reply-p annotation)
      annotation
    (let* ((anchor (min (max (scholia-db-annotation-beg annotation)
                             (point-min))
                        (point-max)))
           (bounds (scholia-db--nearest-match
                    (scholia-db-annotation-annotated-text annotation)
                    anchor
                    (scholia-db--search-window anchor))))
      (when bounds
        (scholia-db-annotation-set-bounds
         annotation (car bounds) (cdr bounds))))))

(defun scholia-db-buffer-annotations (db file checksum)
  "Return the annotations DB holds for FILE, placed in the current buffer.
CHECKSUM fingerprints the buffer as it is now.  While it matches the one
stored with the record the buffer is the file the annotations were made
against and the stored positions hold.  Once it differs the file drifted
while scholia was not watching, and every annotation is relocated with
`scholia-db--relocate' or dropped."
  (let ((record (scholia-db-record db file)))
    (if (equal (scholia-db-record-checksum record) checksum)
        (scholia-db-record-annotations record)
      (delq nil (mapcar #'scholia-db--relocate
                        (scholia-db-record-annotations record))))))


;;;; Merging

(defun scholia-db-merge-interval (a b)
  "Return the interval spanning both interval A and interval B."
  (cons (min (car a) (car b))
        (max (cdr a) (cdr b))))

(defun scholia-db-annotations-overlap-p (a b)
  "Return non-nil when annotation A and annotation B share a character.
Replies cover no text and so never overlap anything."
  (and (not (scholia-db-annotation-reply-p a))
       (not (scholia-db-annotation-reply-p b))
       (< (scholia-db-annotation-beg a) (scholia-db-annotation-end b))
       (< (scholia-db-annotation-beg b) (scholia-db-annotation-end a))))

(defun scholia-db-merge-annotations (host guest)
  "Return HOST widened over GUEST, or nil when the two do not overlap.
HOST keeps its id and the merged annotated text is read from the current
buffer, which is the one both annotate."
  (when (scholia-db-annotations-overlap-p host guest)
    (let ((interval (scholia-db-merge-interval
                     (scholia-db-annotation-interval host)
                     (scholia-db-annotation-interval guest))))
      (scholia-db--with-fields
       host
       :beg (car interval)
       :end (cdr interval)
       :text (concat (scholia-db-annotation-text host)
                     " "
                     (scholia-db-annotation-text guest))
       :annotated-text (buffer-substring-no-properties (car interval)
                                                       (cdr interval))))))

(defun scholia-db-remove-overlaps (annotations)
  "Return ANNOTATIONS with every overlapping pair merged into one."
  (let ((rest annotations)
        (collapsed nil))
    (while rest
      (let* ((probe (pop rest))
             (overlapping (seq-find (lambda (annotation)
                                      (scholia-db-annotations-overlap-p
                                       probe annotation))
                                    rest)))
        (if overlapping
            (setq rest (cons (scholia-db-merge-annotations probe overlapping)
                             (remq overlapping rest)))
          (push probe collapsed))))
    (nreverse collapsed)))

(defun scholia-db--merge-records (host guest)
  "Return the record HOST holding the annotations of record GUEST as well.
Annotations GUEST shares with HOST by id are the same annotation and are
taken once."
  (let ((ids (mapcar #'scholia-db-annotation-id
                     (scholia-db-record-annotations host))))
    (scholia-db-make-record
     (scholia-db-record-file host)
     (append (scholia-db-record-annotations host)
             (seq-remove (lambda (annotation)
                           (member (scholia-db-annotation-id annotation) ids))
                         (scholia-db-record-annotations guest)))
     (scholia-db-record-checksum host))))

(defun scholia-db-merge (db-a db-b)
  "Return DB-A holding the records of DB-B as well, joined file by file."
  (seq-reduce (lambda (db record)
                (let ((host (scholia-db-record
                             db (scholia-db-record-file record))))
                  (scholia-db-put-record
                   db (if host
                          (scholia-db--merge-records host record)
                        record))))
              (plist-get db-b :records)
              db-a))

(provide 'scholia-db)
;;; scholia-db.el ends here
