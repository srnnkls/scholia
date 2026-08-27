;;; scholia-db.el --- The scholia record store  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/scholia
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Reads and writes the session databases scholia annotates into.  A
;; consumer names the session file and the record it wants, and each
;; entry point is one transaction on that session: a save, a reply or a
;; removal reaches the rows it names and leaves every other row where
;; another writer left it.  The whole-value plist survives as the
;; interchange a session is exported and imported as.

;;; Code:

(require 'seq)
(require 'scholia-vars)
(require 'scholia-store)

(defun scholia-db--with-fields (plist &rest fields)
  "Return a copy of PLIST carrying the key and value pairs of FIELDS."
  (let ((copy (copy-sequence plist)))
    (while fields
      (setq copy (plist-put copy (pop fields) (pop fields))))
    copy))


;;;; The store a session is reached through

(defun scholia-db--present-p (session-file)
  "Return non-nil when SESSION-FILE carries a session to read or to write.
A file that is not there and one holding nothing yet both answer no: a
kill mid-sync or a partly fetched checkout leave a file holding no
database to be wrong about, and the next write fills it.  The size is
read off one stat rather than a test and a read of it, since a session
is renamed away by the Emacs migrating it off the printed plist."
  (let ((size (file-attribute-size (file-attributes session-file))))
    (and size (not (zerop size)))))

(defun scholia-db--reading (session-file read)
  "Return what READ takes from a store opened on SESSION-FILE.
A SESSION-FILE holding no session yet reads as nothing and is not made by
being read.  One still holding the printed plist scholia stored before is
migrated into a database at that same path on the way through, and one
carrying a database that is not scholia's signals
`scholia-db-format-error' rather than being guessed at.

READ takes everything it takes off one snapshot of the database, so a
record it assembles from more than one statement carries one commit
rather than the checksum of one and the annotations of the next."
  (when (scholia-db--present-p session-file)
    (let ((store (scholia-store-open session-file)))
      (unwind-protect
          (scholia-store-with-snapshot store
            (funcall read store))
        (scholia-store-close store)))))

(defun scholia-db--writing (session-file write)
  "Call WRITE on a store opened on SESSION-FILE, as one transaction.
The transaction takes the write lock before WRITE reads anything, so a
read and the write that follows it are one step no other Emacs commits
between.  A SESSION-FILE that is not there yet is made at its own path,
so a second Emacs first-writing it waits on the same lock, and goes
again when the transaction commits nothing."
  (let ((store (scholia-store-open session-file)))
    (unwind-protect
        (scholia-store-with-transaction store
          (funcall write store))
      (scholia-store-close store))))


;;;; Sessions

(defun scholia-db--publish (store session-file)
  "Give STORE a session header naming it after SESSION-FILE when it has none.
A header already there is left alone, so `:created' keeps the moment of
the first write."
  (unless (scholia-store-session store)
    (scholia-store-put-session
     store (list :name (file-name-base session-file)
                 :created (format-time-string "%FT%T%z")
                 :project (funcall scholia-project-root-function)
                 :description nil))))

(defun scholia-db-session-name (session-file)
  "Return the name of the session stored in SESSION-FILE."
  (plist-get (scholia-db--reading session-file #'scholia-store-session)
             :name))

(defun scholia-db-session-created (session-file)
  "Return when the session stored in SESSION-FILE was first written."
  (plist-get (scholia-db--reading session-file #'scholia-store-session)
             :created))

(defun scholia-db-set-session-name (session-file name)
  "Call the session stored in SESSION-FILE NAME from now on.
Only the name is written: the rest of the header, and every record under
it, is left exactly as it stands.  A SESSION-FILE that is not there yet
is given a whole header before the name is put on it, since a header
holding a name alone answers nil to `scholia-db-session-created' for as
long as that session lasts, and it is listed."
  (scholia-db--writing
   session-file
   (lambda (store)
     (scholia-db--publish store session-file)
     (scholia-store-put-session
      store (scholia-db--with-fields (scholia-store-session store)
                                     :name name)))))

(defun scholia-db-create-session (session-file)
  "Give SESSION-FILE a session header when it carries none yet.
A SESSION-FILE already holding one is left as it stands, so its
`:created' keeps the moment the session was first made and its records
stay where they are."
  (scholia-db--writing
   session-file
   (lambda (store) (scholia-db--publish store session-file))))


;;;; Records

(defun scholia-db-make-record (file annotations checksum)
  "Return a record holding ANNOTATIONS of FILE fingerprinted by CHECKSUM."
  (list :file file :annotations annotations :checksum checksum))

(defun scholia-db-record-file (record)
  "Return the absolute file name RECORD annotates."
  (plist-get record :file))

(defun scholia-db-record-checksum (record)
  "Return the fingerprint of the buffer the positions in RECORD hold in.
Usually that is the file as RECORD was written, but a save that had to
fold annotations back in unplaced keeps the fingerprint the record had
before it, since the buffer just saved is one those annotations were
never placed in.  Reading it as anything but \"the positions hold when
the buffer fingerprints like this\" therefore misleads."
  (plist-get record :checksum))

(defun scholia-db-record-annotations (record)
  "Return the annotations RECORD carries."
  (plist-get record :annotations))

(defun scholia-db-files (session-file)
  "Return the files SESSION-FILE carries a record for."
  (scholia-db--reading session-file #'scholia-store-files))

(defun scholia-db-record (session-file file)
  "Return the record SESSION-FILE carries for FILE, or nil when it has none.
This is the reader the mode-enable path takes, so a session still stored
as the printed plist is migrated here rather than read as absent."
  (scholia-db--reading session-file
                       (lambda (store) (scholia-store-record store file))))

(defun scholia-db-store-record (session-file record)
  "Store RECORD in SESSION-FILE, replacing the record its file keys.
Every other record is left where it is, so storing one file cannot
revert what another writer stored for a different one."
  (scholia-db--writing
   session-file
   (lambda (store)
     (scholia-db--publish store session-file)
     (scholia-store-put-record store record))))

(defun scholia-db-remove-record (session-file file)
  "Take the record SESSION-FILE carries for FILE out of it.
A SESSION-FILE holding no session yet carries no record to take out and
is left alone rather than made: a removal names no session to create, and
the one it would create carries no header, which
`scholia-session-list' cannot see, `scholia-session-create' refuses as
taken and `scholia-session-switch' opens all at once."
  (when (scholia-db--present-p session-file)
    (scholia-db--writing
     session-file
     (lambda (store) (scholia-store-remove-record store file)))))

(defun scholia-db-add-reply (session-file file reply)
  "Append REPLY to the record FILE keys in SESSION-FILE.
One annotation is added to one record: every other annotation of that
record, and every other record, is left as it stands.  A reply an agent
makes into the session Emacs has open therefore cannot be reverted by the
save Emacs is about to make, nor revert it."
  (scholia-db--writing
   session-file
   (lambda (store)
     (scholia-db--publish store session-file)
     (unless (scholia-store-record store file)
       (scholia-store-put-record store (scholia-db-make-record file nil nil)))
     (scholia-store-add-annotation store file reply))))


;;;; Annotations

(defun scholia-db-make-annotation (id text beg end annotated-text
                                      &optional color position reply-to)
  "Return the annotation ID carrying TEXT against BEG to END.
ANNOTATED-TEXT is the buffer text the annotation covers, COLOR the index
it is drawn with and POSITION where it is drawn, defaulting to 0 and
`:margin'.  REPLY-TO names the annotation this one answers; a reply
covers no text, so it passes nil for BEG, END and ANNOTATED-TEXT."
  (list :id id
        :text text
        :beg beg
        :end end
        :annotated-text annotated-text
        :color (or color 0)
        :position (or position :margin)
        :reply-to reply-to))

(defun scholia-db-annotation-id (annotation)
  "Return the id of ANNOTATION."
  (plist-get annotation :id))

(defun scholia-db-annotation-color (annotation)
  "Return the index ANNOTATION is drawn with."
  (plist-get annotation :color))

(defun scholia-db-annotation-position (annotation)
  "Return where ANNOTATION is drawn."
  (plist-get annotation :position))

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

(defun scholia-db-annotation-orphaned-from (annotation)
  "Return the id ANNOTATION hung off before it went missing, or nil."
  (plist-get annotation :orphaned-from))

(defun scholia-db-annotation-orphaned-at (annotation)
  "Return when ANNOTATION was first found to have lost its parent, or nil."
  (plist-get annotation :orphaned-at))

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


;;;; Sends

(defvar scholia-send-functions nil
  "Functions run once a batch of annotations has reached its destination.
Each is called with the annotations carrying the new send record and
with that record.")

(defun scholia-db--timestamp ()
  "Return the moment this is called, as an ISO-8601 string."
  (format-time-string "%FT%T%z"))

(defun scholia-db-make-send (&rest fields)
  "Return a send record carrying FIELDS, stamped with the moment it is made."
  (scholia-db--with-fields fields :at (scholia-db--timestamp)))

(defun scholia-db-send-at (send)
  "Return when SEND was made."
  (plist-get send :at))

(defun scholia-db-send-kind (send)
  "Return whether SEND went to an agent or to a pane."
  (plist-get send :kind))

(defun scholia-db-send-target (send)
  "Return the destination SEND was aimed at."
  (plist-get send :target))

(defun scholia-db-send-label (send)
  "Return the display name of the destination SEND was aimed at."
  (plist-get send :label))

(defun scholia-db-send-herdr-session (send)
  "Return the herdr session SEND went through."
  (plist-get send :herdr-session))

(defun scholia-db-send-format (send)
  "Return the format SEND rendered its annotations in."
  (plist-get send :format))

(defun scholia-db-send-scope (send)
  "Return how much of the session SEND covered."
  (plist-get send :scope))

(defun scholia-db-annotation-sends (annotation)
  "Return the sends ANNOTATION was part of, oldest first."
  (plist-get annotation :sends))

(defun scholia-db-annotation-add-send (annotation send)
  "Return ANNOTATION with SEND appended to the sends it carried."
  (scholia-db--with-fields
   annotation :sends (append (scholia-db-annotation-sends annotation)
                             (list send))))

(defun scholia-db-send-batch (annotations send sender)
  "Return ANNOTATIONS carrying SEND once SENDER has taken them.
SENDER is called once and with no arguments, and only a return of its
own records anything: a signal it raises reaches the caller as it was
raised, with ANNOTATIONS and the sends they already hold untouched and
`scholia-send-functions' unrun.  Its members observe a send that already
happened, so one of them signalling is reported rather than propagated:
the annotations carrying the new record are the only copy of it and are
returned either way."
  (funcall sender)
  (let ((sent (mapcar (lambda (annotation)
                        (scholia-db-annotation-add-send annotation send))
                      annotations)))
    (condition-case failure
        (run-hook-with-args 'scholia-send-functions sent send)
      (error (message "scholia: send observer failed: %S" failure)))
    sent))


;;;; The interchange a session travels as

(defun scholia-db-interchange (session-file)
  "Return the database SESSION-FILE carries, writing nothing to SESSION-FILE.
A file still holding the printed plist is read as it stands rather than
migrated: listing the sessions and importing one both reach files that
are the caller's rather than the store's, so an exported `.eld' parked in
the session directory would be turned into a binary database the moment a
session prompt was opened, and a colleague's export could not be imported
at all from a directory scholia cannot write to.

A SESSION-FILE holding no session yet carries the empty database rather
than nothing: handed on to the writer, nothing is written out as the
literal \"nil\", which is an export reported as made that neither scholia
nor the colleague it was made for can read back."
  (if (scholia-store-interchange-p session-file)
      (scholia-store-read-interchange session-file)
    (or (scholia-db--reading session-file #'scholia-store-read)
        (list :scholia scholia-store-format-version :records nil))))

(defun scholia-db-session-file-p (session-file)
  "Return non-nil when SESSION-FILE carries a session of scholia's.
SESSION-FILE is left as it stands, so asking this of every file in the
session directory cannot migrate one that only travels through it.  A
database answers out of its header row and a printed plist out of the
whole value, which is the only place its header is."
  (and (if (scholia-store-interchange-p session-file)
           (plist-get (plist-get (scholia-store-read-interchange session-file)
                                 :session)
                      :name)
         (scholia-db-session-name session-file))
       t))


;;;; The I/O entry points

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

(defun scholia-db--merge-sends (stored incoming)
  "Return the sends of STORED with those of INCOMING it does not hold.
A send is the one thing an annotation carries that its buffer cannot
rebuild, so the history a save finds only ever grows (AD-3)."
  (append stored (seq-remove (lambda (send) (member send stored)) incoming)))

(defun scholia-db--carry-forward (annotation stored)
  "Return ANNOTATION holding the fields only its entry in STORED has.
An annotation rebuilt from a buffer carries whatever overlays hold and
nothing else, so replacing the stored annotation of the same id wholesale
would drop the `:sends' it earned.  Those sends are merged rather than
taken over, so an incoming annotation carrying an emptied or outdated
history cannot shorten the stored one."
  (let ((previous (seq-find (lambda (candidate)
                              (equal (scholia-db-annotation-id candidate)
                                     (scholia-db-annotation-id annotation)))
                            stored)))
    (if previous
        (scholia-db--with-fields
         (apply #'scholia-db--with-fields previous annotation)
         :sends (scholia-db--merge-sends
                 (scholia-db-annotation-sends previous)
                 (scholia-db-annotation-sends annotation)))
      annotation)))

(defun scholia-db--lost-parents (annotations)
  "Return the id that went missing beneath each orphaned reply of ANNOTATIONS.
The result is an alist keyed by reply id.  A reply is orphaned when its
`:reply-to' names no annotation of ANNOTATIONS, and so is every reply
answering an orphaned one, however deep, since a subtree that lost its
root lost it whole.  One pass cannot see that far down, so the pass runs
again until it finds nothing new.  The whole subtree is keyed to the id
that is actually absent rather than to the reply above it, which is
still there and would tell a reader nothing.

What ANNOTATIONS hold now is the only evidence read: a stamp an earlier
save left is not, since a parent that came back through a merge leaves
the reply below it stamped while its whole ancestry is present again,
and taking that stamp as evidence would mint a fresh one on a reply that
never lost anything.  Whether a stamp still fits is a separate question,
asked in `scholia-db--stamp-orphans' and only to decide against writing
a second one."
  (let ((ids (mapcar #'scholia-db-annotation-id annotations))
        (lost nil)
        (growing t))
    (while growing
      (setq growing nil)
      (dolist (annotation annotations)
        (let ((id (scholia-db-annotation-id annotation))
              (parent (scholia-db-annotation-reply-to annotation)))
          (when (and parent (not (assoc id lost)))
            (let ((missing (if (member parent ids)
                               (cdr (assoc parent lost))
                             parent)))
              (when missing
                (push (cons id missing) lost)
                (setq growing t)))))))
    lost))

(defun scholia-db--stamp-orphans (annotations)
  "Return ANNOTATIONS with every reply that lost its parent stamped.
`:orphaned-from' names the annotation that went missing and
`:orphaned-at' when the loss was first found.  A reply already carrying
the pair keeps the one it has, so the moment recorded is the one of the
loss rather than the one of the latest save.  An orphan is kept and
marked rather than swept away: a reply is the only copy of what was
said, and a parent absent from this record may have been deleted or may
simply never have arrived from the session the reply came from, which
nothing here can tell apart.  A stamp is never lifted either; a parent
returning through a merge is for the merge path to answer."
  (let ((lost (scholia-db--lost-parents annotations))
        (at (scholia-db--timestamp)))
    (mapcar (lambda (annotation)
              (let ((missing (cdr (assoc (scholia-db-annotation-id annotation)
                                         lost))))
                (if (and missing
                         (not (scholia-db-annotation-orphaned-from annotation)))
                    (scholia-db--with-fields annotation
                                             :orphaned-from missing
                                             :orphaned-at at)
                  annotation)))
            annotations)))

(defun scholia-db-save (session-file file annotations checksum
                                     &optional preserve)
  "Store ANNOTATIONS of FILE with CHECKSUM into SESSION-FILE.
Source context is snapshot from the current buffer, which is the one
visiting FILE.  Fields an incoming annotation does not carry are taken
from the stored annotation of the same id, so a send history survives
every later save.  Replies already stored for FILE are folded back in as
they are, since they carry no position to recompute.  A reply reaches
here among ANNOTATIONS only on the save that makes it and comes back
from the record on every save after, so it is filed behind the replies
already stored rather than ahead of them and a thread keeps the order it
was written in.

PRESERVE holds stored annotations the caller could not rebuild from the
buffer, folded back in exactly as they are and so neither snapshot nor
searched for: the positions they carry are the ones this buffer no
longer has.  Without them a save would read as a deletion, since an
annotation the caller cannot place and one the user removed both reach
here as an absence from ANNOTATIONS.  An entry already carried by
ANNOTATIONS or by a refolded reply is left to that carrier, so passing
the stored annotations back wholesale cannot duplicate them.

A record that ends up holding a preserved annotation keeps the CHECKSUM
it had rather than taking the new one, because CHECKSUM fingerprints a
buffer those annotations were never placed in and storing it would tell
the next load their stale positions hold.  The next load therefore finds
a mismatch and re-searches, which is what gives them their one chance of
being found again; the placed annotations are re-searched with it at no
distance, having just been snapshot here.  Once nothing is preserved
CHECKSUM is stored again and the record heals.

A reply in the record being written whose parent is not in it is stamped
by `scholia-db--stamp-orphans' rather than dropped.  That reaches this
record and no other, so an orphan arriving through an import is stamped
by `scholia-db-import' at the seam it merges on rather than waiting here
for a save of the file it answers.

Reading the record, folding it and writing it back happen as one
transaction, and only the record for FILE is written, so a save cut short
leaves the record stored before it whole and a save that finishes leaves
every other record where another writer left it."
  (scholia-db--writing
   session-file
   (lambda (store)
     (scholia-db--fold store session-file file annotations checksum
                       preserve))))

(defun scholia-db--fold (store session-file file annotations checksum preserve)
  "Fold ANNOTATIONS of FILE into the record STORE carries for it.
SESSION-FILE names the session STORE was opened on, CHECKSUM
fingerprints the buffer and PRESERVE holds the stored annotations the
caller could not rebuild from it, all as `scholia-db-save' takes them."
  (scholia-db--publish store session-file)
  (let* ((record (scholia-store-record store file))
         (stored (scholia-db-record-annotations record))
         (snapshots (mapcar (lambda (annotation)
                              (scholia-db--carry-forward
                               (scholia-db--snapshot annotation) stored))
                            annotations))
         (placed (seq-remove #'scholia-db-annotation-reply-p snapshots))
         (made (seq-filter #'scholia-db-annotation-reply-p snapshots))
         (ids (mapcar #'scholia-db-annotation-id snapshots))
         (replies (seq-filter
                   (lambda (annotation)
                     (and (scholia-db-annotation-reply-p annotation)
                          (not (member (scholia-db-annotation-id annotation)
                                       ids))))
                   stored))
         (carried (append ids (mapcar #'scholia-db-annotation-id replies)))
         (unplaced (seq-remove
                    (lambda (annotation)
                      (member (scholia-db-annotation-id annotation) carried))
                    preserve)))
    (scholia-store-put-record
     store (scholia-db-make-record
            file (scholia-db--stamp-orphans
                  (append placed replies made unplaced))
            (if unplaced
                (scholia-db-record-checksum record)
              checksum)))))


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

(defun scholia-db-buffer-annotations (record checksum &optional unplaced)
  "Return the annotations RECORD carries, placed in the current buffer.
CHECKSUM fingerprints the buffer as it is now.  While it matches the one
stored with RECORD the buffer is the file the annotations were made
against and the stored positions hold.  Once it differs the file drifted
while scholia was not watching, and every annotation is relocated with
`scholia-db--relocate'.

UNPLACED, when given, is a function called once with the stored
annotations the re-search could not place, and with nil when it placed
every one.  They keep the `:line' and `:line-text' snapshot locating
them once the text they covered is gone, so a caller reports them and
hands them to `scholia-db-save' as its PRESERVE argument instead of
letting the next save read them as deleted."
  (if (equal (scholia-db-record-checksum record) checksum)
      (progn
        (when unplaced (funcall unplaced nil))
        (scholia-db-record-annotations record))
    (let* ((stored (scholia-db-record-annotations record))
           (placed (delq nil (mapcar #'scholia-db--relocate stored)))
           (ids (mapcar #'scholia-db-annotation-id placed)))
      (when unplaced
        (funcall unplaced
                 (seq-remove
                  (lambda (annotation)
                    (member (scholia-db-annotation-id annotation) ids))
                  stored)))
      placed)))


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

(defun scholia-db-widen-over (host guest)
  "Return HOST spanning GUEST and carrying both notes, or nil when disjoint.
HOST keeps its id and the `:annotated-text' it was stored with, which
after the widening covers less than the interval does.  Reading the text
the wider interval now holds takes the buffer annotating it, and a merge
of records is done wherever the user ran it rather than in that buffer,
so the reading is left to the next save that has it."
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
                     (scholia-db-annotation-text guest))))))

(defun scholia-db-merge-annotations (host guest)
  "Return HOST widened over GUEST, or nil when the two do not overlap.
HOST keeps its id and the merged annotated text is read from the current
buffer, which is the one both annotate."
  (let ((merged (scholia-db-widen-over host guest)))
    (when merged
      (scholia-db--with-fields
       merged
       :annotated-text (buffer-substring-no-properties
                        (scholia-db-annotation-beg merged)
                        (scholia-db-annotation-end merged))))))

(defun scholia-db-remove-overlaps (annotations &optional merge)
  "Return ANNOTATIONS with every overlapping pair merged into one.
MERGE folds a pair and defaults to `scholia-db-merge-annotations', which
reads the merged text from the current buffer; a caller folding a record
of a file no buffer visits passes `scholia-db-widen-over' instead."
  (let ((rest annotations)
        (fold (or merge #'scholia-db-merge-annotations))
        (collapsed nil))
    (while rest
      (let* ((probe (pop rest))
             (overlapping (seq-find (lambda (annotation)
                                      (scholia-db-annotations-overlap-p
                                       probe annotation))
                                    rest)))
        (if overlapping
            (setq rest (cons (funcall fold probe overlapping)
                             (remq overlapping rest)))
          (push probe collapsed))))
    (nreverse collapsed)))

(defun scholia-db--merge-records (host guest)
  "Return the record HOST holding the annotations of record GUEST as well.
Annotations GUEST shares with HOST by id are the same annotation and are
taken once.  Two that cover the same characters are one annotation the
two sessions each hold their own of, and they are folded into one
spanning both and carrying both notes: `scholia-annotate' refuses to make
an overlapping pair, so a record holding one would leave every annotation
underneath unreachable by `scholia-annotation-at' and so by every command
that reads it."
  (let ((ids (mapcar #'scholia-db-annotation-id
                     (scholia-db-record-annotations host))))
    (scholia-db-make-record
     (scholia-db-record-file host)
     (scholia-db-remove-overlaps
      (append (scholia-db-record-annotations host)
              (seq-remove (lambda (annotation)
                            (member (scholia-db-annotation-id annotation) ids))
                          (scholia-db-record-annotations guest)))
      #'scholia-db-widen-over)
     (scholia-db-record-checksum host))))

(defun scholia-db-import (session-file db)
  "Merge the database DB into the session stored in SESSION-FILE.
Record by record: one keyed by a file the session already carries is
joined into the record stored for it, annotations shared by id counting
once, and one keyed by a file it does not know lands whole.  A
SESSION-FILE holding no session yet is given a header named after it, and
one already holding a session keeps the header it has.

A reply arriving without the annotation it answers is stamped as its
record lands, since that record may belong to a file no buffer here
visits and its next save is the only other thing that would reach it."
  (scholia-db--writing
   session-file
   (lambda (store)
     (scholia-db--publish store session-file)
     (dolist (record (plist-get db :records))
       (let* ((file (scholia-db-record-file record))
              (host (scholia-store-record store file))
              (joined (if host
                          (scholia-db--merge-records host record)
                        record)))
         (scholia-store-put-record
          store (scholia-db-make-record
                 file
                 (scholia-db--stamp-orphans
                  (scholia-db-record-annotations joined))
                 (scholia-db-record-checksum joined))))))))

(provide 'scholia-db)
;;; scholia-db.el ends here
