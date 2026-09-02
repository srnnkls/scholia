;;; scholia-export.el --- Export formats  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/scholia
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Renders annotations as rustc diagnostics, as a unified diff, or as a
;; commented copy of the source.  Position, source line and columns come
;; from the context each annotation carries rather than from the buffer a
;; formatter happens to be called in, so an annotation reads the same
;; wherever it is rendered and long after its file changed.

;;; Code:

(require 'seq)
(require 'scholia-vars)
(require 'scholia-db)
(require 'scholia-thread)
(require 'scholia-core)
(require 'scholia-locate)
(require 'scholia-session)

(define-error 'scholia-export-unknown-format
              "No such export format"
              'scholia-error)

(defconst scholia-export--underline ?~
  "Character underlining annotated text in an integrated export.")

(defconst scholia-export--reply-indent 2
  "Columns a reply is set in past the note it answers.")

(defconst scholia-export--fallback-comment "#"
  "Comment syntax used where the source buffer names none.")

(defconst scholia-export-buffer-name "*scholia-export*"
  "Buffer a `buffer' export is shown in.")

(defconst scholia-export--no-newline "\\ No newline at end of file"
  "How a patch marks a source whose last line ends in no newline.")


;;;; What a formatter renders against

(defun scholia-export--source-buffer (file-or-buffer)
  "Return the buffer holding the source text FILE-OR-BUFFER names.
Only a buffer names one; reading a file into one is the session
export's, so anything else answers with the current buffer."
  (if (bufferp file-or-buffer) file-or-buffer (current-buffer)))

(defun scholia-export--buffer-lines ()
  "Return the lines of the current buffer, its trailing empty one among them.
The whole buffer is read whatever it is narrowed to: an annotation
carries absolute positions, so a restriction would renumber the source
out from under it."
  (split-string (save-restriction
                  (widen)
                  (buffer-substring-no-properties (point-min) (point-max)))
                "\n"))

(defun scholia-export--name (file-or-buffer)
  "Return the name a rendering gives FILE-OR-BUFFER.
A buffer visiting no file answers with its own name, so an annotation
made in a scratch buffer still renders somewhere."
  (if (stringp file-or-buffer)
      file-or-buffer
    (with-current-buffer (scholia-export--source-buffer file-or-buffer)
      (or (buffer-file-name) (buffer-name)))))


;;;; Threads

(defun scholia-export--root (thread)
  "Return the annotation THREAD hangs off."
  (car (car thread)))

(defun scholia-export--placed-p (thread)
  "Return non-nil when THREAD heads a source line to render against."
  (and (scholia-db-annotation-line (scholia-export--root thread)) t))

(defun scholia-export--threads (annotations)
  "Return ANNOTATIONS grouped into the threads they form, placed ones first.
Each thread is a list of annotation and depth pairs in the order
`scholia-thread-walk' visits them, its root first and at depth 0.  The
car holds the threads a formatter renders against a line and the cdr
those it cannot: a reply the walk re-rooted carries no source context,
and a reply is never a diagnostic of its own (INV-14).  Every formatter
reads the split from here, so the two halves cannot drift apart."
  (let ((threads nil))
    (scholia-thread-walk
     annotations
     (lambda (annotation depth)
       (if (zerop depth)
           (push (list (cons annotation depth)) threads)
         (push (cons annotation depth) (car threads)))))
    (let ((walked (mapcar #'nreverse (nreverse threads))))
      (cons (seq-filter #'scholia-export--placed-p walked)
            (seq-remove #'scholia-export--placed-p walked)))))

(defun scholia-export--lines (text)
  "Return the physical lines TEXT carries, the empty ones among them.
A note is free text and may hold newlines, which every formatter must
break before it prefixes anything: a line commented once leaves the rest
of the note reading as source."
  (split-string (or text "") "\n"))

(defun scholia-export--reply (annotation depth)
  "Return the lines for ANNOTATION at DEPTH, its id ahead of its note.
The id is the handle an agent answering the annotation quotes back.  A
note carrying several lines sets each of them at DEPTH, so its own lines
stay flush with one another."
  (let ((indent (make-string (* depth scholia-export--reply-indent) ?\s))
        (lines (scholia-export--lines
                (scholia-db-annotation-text annotation))))
    (cons (concat indent
                  (format "[%s] %s"
                          (scholia-db-annotation-id annotation)
                          (car lines)))
          (mapcar (lambda (line) (concat indent line)) (cdr lines)))))

(defun scholia-export--replies (thread)
  "Return the lines the replies of THREAD read as."
  (mapcan (lambda (entry) (scholia-export--reply (car entry) (cdr entry)))
          (cdr thread)))

(defun scholia-export--orphan (annotation)
  "Return the lines for the orphaned reply ANNOTATION.
The line names the annotation it answers, which nothing else in the
rendering carries: that annotation is not in this export."
  (let ((lines (scholia-export--lines
                (scholia-db-annotation-text annotation))))
    (cons (format "[%s] in reply to %s: %s"
                  (scholia-db-annotation-id annotation)
                  (or (scholia-db-annotation-orphaned-from annotation)
                      (scholia-db-annotation-reply-to annotation))
                  (car lines))
          (cdr lines))))

(defun scholia-export--orphan-block (threads)
  "Return the lines the orphaned THREADS trail the rendering with.
Nil when there are none, so a rendering without orphans reads exactly as
it did.  The block carries no caret, no header and no hunk: a reply
holds no position to point at, and INV-14 leaves it no diagnostic of its
own.  The replies an orphan gathered nest under it as they do anywhere
else."
  (when threads
    (cons "note: replies whose annotation is not in this export"
          (mapcan (lambda (thread)
                    (append (scholia-export--orphan
                             (scholia-export--root thread))
                            (scholia-export--replies thread)))
                  threads))))

(defun scholia-export--width (annotation)
  "Return how many columns the marked run of ANNOTATION covers."
  (max 1 (- (scholia-db-annotation-end-column annotation)
            (scholia-db-annotation-column annotation))))


;;;; The rustc format

(defun scholia-export--diagnostic (thread name)
  "Return the diagnostic for THREAD against the file NAME.
The caret run sits at the column the annotation was taken at, counted
over the raw source line, so it stays put whatever mode renders the
source (INV-6).  A chain spanning several lines is pointed at where it
begins rather than where it ends."
  (let* ((annotation (scholia-export--root thread))
         (line (scholia-db-annotation-line annotation))
         (column (scholia-db-annotation-column annotation))
         (width (scholia-export--width annotation))
         (revision (scholia-locate-revision annotation))
         (gutter (make-string (length (number-to-string line)) ?\s))
         (prefix (concat gutter " | "))
         (note-column (+ column width 1))
         (note (scholia-export--lines
                (scholia-db-annotation-text annotation))))
    (string-join
     (append
      (list (format "%s--> %s:%d:%d [%s]%s" gutter name line (1+ column)
                    (scholia-db-annotation-id annotation)
                    (if revision (format " [%s]" revision) ""))
            (concat gutter " |")
            (format "%d | %s" line
                    (scholia-db-annotation-line-text annotation))
            (concat prefix
                    (make-string column ?\s)
                    (make-string width ?^)
                    " "
                    (car note)))
      (mapcar (lambda (payload)
                (concat prefix (make-string note-column ?\s) payload))
              (append (cdr note) (scholia-export--replies thread)))
      (list (concat gutter " |")))
     "\n")))

(defun scholia-export-rustc (annotations &optional file-or-buffer)
  "Return ANNOTATIONS as rustc diagnostics against FILE-OR-BUFFER.
One diagnostic per thread, its replies nested under the note they
answer, and a trailing note carrying the replies no diagnostic could
hold."
  (let* ((name (scholia-export--name file-or-buffer))
         (threads (scholia-export--threads annotations))
         (orphans (scholia-export--orphan-block (cdr threads))))
    (string-join
     (append (mapcar (lambda (thread)
                       (scholia-export--diagnostic thread name))
                     (car threads))
             (and orphans (list (string-join orphans "\n"))))
     "\n\n")))


;;;; Commented source

(defun scholia-export--comment (payload column start end)
  "Return PAYLOAD commented by START and END and beginning at COLUMN.
The padding is measured against the width of START, so the payload
begins at COLUMN whatever the comment syntax costs (INV-9).  No comment
syntax reaches left of its own prefix: for a COLUMN under the width of
START the padding clamps to zero and the payload begins at that width
instead, which is as near the column as a comment gets."
  (concat (make-string (max 0 (- column (string-width start))) ?\s)
          start payload end))

(defun scholia-export--comment-block (thread start end)
  "Return the lines for THREAD, commented by START and END.
The underline comes first, then the id of the annotation, then its note
and the replies it carries."
  (let* ((annotation (scholia-export--root thread))
         (column (scholia-db-annotation-column annotation))
         (revision (scholia-locate-revision annotation)))
    (mapcar (lambda (payload)
              (scholia-export--comment payload column start end))
            (append (list (make-string (scholia-export--width annotation)
                                       scholia-export--underline)
                          (format "[%s]%s"
                                  (scholia-db-annotation-id annotation)
                                  (if revision
                                      (format " [%s]" revision)
                                    "")))
                    (scholia-export--lines
                     (scholia-db-annotation-text annotation))
                    (scholia-export--replies thread)))))

(defun scholia-export--threads-on (threads line)
  "Return the entries of THREADS taken against LINE."
  (seq-filter (lambda (thread)
                (equal (scholia-db-annotation-line
                        (scholia-export--root thread))
                       line))
              threads))

(defun scholia-export-integrate (annotations &optional file-or-buffer)
  "Return the source of FILE-OR-BUFFER carrying ANNOTATIONS as comments.
Each annotation is written below the line it was taken against, leaving
the source itself as it stands, and the replies no line holds trail the
source as comments of their own."
  (with-current-buffer (scholia-export--source-buffer file-or-buffer)
    (let* ((start (or comment-start scholia-export--fallback-comment))
           (end (or comment-end ""))
           (threads (scholia-export--threads annotations))
           (number 0)
           (output nil))
      (dolist (line (scholia-export--buffer-lines))
        (setq number (1+ number))
        (push line output)
        (dolist (thread (scholia-export--threads-on (car threads) number))
          (dolist (comment (scholia-export--comment-block thread start end))
            (push comment output))))
      (dolist (line (scholia-export--orphan-block (cdr threads)))
        (push (scholia-export--comment line 0 start end) output))
      (string-join (nreverse output) "\n"))))


;;;; The diff format

(defun scholia-export--source-lines (threads)
  "Return the source lines THREADS are taken against, once each, ascending.
A hunk answers for a line, not for a thread, and the hunks of a patch
count forward, so the order the annotations arrive in is not the order
the hunks go out in."
  (sort (seq-uniq (mapcar (lambda (thread)
                            (scholia-db-annotation-line
                             (scholia-export--root thread)))
                          threads))
        #'<))

(defun scholia-export--runs (lines)
  "Return LINES gathered into the neighbouring groups they form.
A hunk quotes the line after the last one it annotates, so two annotated
lines that are neighbours would each quote the other and neither
`git apply' nor patch(1) takes two hunks quoting one line.  Neighbours
therefore go out as a single hunk."
  (let ((runs nil))
    (dolist (line lines)
      (if (and runs (= line (1+ (car (car runs)))))
          (push line (car runs))
        (push (list line) runs)))
    (mapcar #'nreverse (nreverse runs))))

(defun scholia-export--context (lines)
  "Return LINES as the context a hunk quotes the source by.
Each answers for one line of the source and counts as one line of a
hunk, the last of them carrying the marker a patch reads a source ending
in no newline by."
  (let* ((terminated (equal (car (last lines)) ""))
         (quoted (mapcar (lambda (line) (concat " " line))
                         (if terminated (butlast lines) lines))))
    (if (or terminated (null quoted))
        quoted
      (append (butlast quoted)
              (list (concat (car (last quoted))
                            "\n" scholia-export--no-newline))))))

(defun scholia-export--added (body)
  "Return how many lines of BODY are additions to the source it quotes."
  (seq-count (lambda (line) (string-prefix-p "+" line)) body))

(defun scholia-export--hunk (threads run start end trailing offset)
  "Return the hunk adding the THREADS taken against RUN.
The comments are commented by START and END, TRAILING is the context
line the hunk closes on or nil where RUN ends the file, and OFFSET is
how many lines the hunks ahead of this one added."
  (let ((body nil))
    (dolist (line run)
      (let ((on (scholia-export--threads-on threads line)))
        (push (concat " " (scholia-db-annotation-line-text
                           (scholia-export--root (car on))))
              body)
        (dolist (thread on)
          (dolist (comment (scholia-export--comment-block thread start end))
            (push (concat "+" comment) body)))))
    (when trailing
      (push trailing body))
    (setq body (nreverse body))
    (let ((quoted (+ (length run) (if trailing 1 0))))
      (cons (format "@@ -%d,%d +%d,%d @@"
                    (car run) quoted (+ (car run) offset)
                    (+ quoted (scholia-export--added body)))
            body))))

(defun scholia-export--hunks (threads start end context)
  "Return the hunks adding THREADS, commented by START and END.
CONTEXT holds the source read as the lines a hunk quotes it by, one for
each line of the file.  A hunk quotes the line it annotates and the line
after it: without that trailing line both `git apply' and patch(1) read
the hunk as anchored to the end of the file and reject every export
whose last annotation is not on the last line.  Two annotations on one
line therefore share a hunk, and so do two annotated lines that are
neighbours.  Each hunk's new starting line counts the lines the hunks
ahead of it added, which is what the new file is numbered in."
  (let ((offset 0)
        (hunks nil))
    (dolist (run (scholia-export--runs
                  (scholia-export--source-lines threads)))
      (let ((hunk (scholia-export--hunk threads run start end
                                        (nth (car (last run)) context)
                                        offset)))
        (push hunk hunks)
        (setq offset (+ offset (scholia-export--added (cdr hunk))))))
    (apply #'append (nreverse hunks))))

(defun scholia-export-diff (annotations &optional file-or-buffer)
  "Return ANNOTATIONS as a unified diff adding them to FILE-OR-BUFFER.
The headers carry no timestamp, so the same annotations render the same
diff however often they are exported.  The source is read for the
context its hunks quote it by as well as for its comment syntax, so a
rendering answers for the file as it stands rather than for the snapshot
alone.  The replies no line holds trail the last hunk as comments, past
where the patch ends: a note is free text, and a raw line of one reading
as `---' or `@@' would be parsed as patch of its own."
  (let ((name (scholia-export--name file-or-buffer))
        (threads (scholia-export--threads annotations))
        (start nil)
        (end nil)
        (context nil))
    (with-current-buffer (scholia-export--source-buffer file-or-buffer)
      (setq start (or comment-start scholia-export--fallback-comment))
      (setq end (or comment-end ""))
      (setq context (scholia-export--context
                     (scholia-export--buffer-lines))))
    (string-join
     (append (list (concat "--- " name) (concat "+++ " name))
             (scholia-export--hunks (car threads) start end context)
             (mapcar (lambda (line)
                       (scholia-export--comment line 0 start end))
                     (scholia-export--orphan-block (cdr threads))))
     "\n")))


;;;; Dispatch

(defcustom scholia-export-functions
  '((rustc . scholia-export-rustc)
    (diff . scholia-export-diff)
    (integrate . scholia-export-integrate))
  "Alist mapping a format symbol to the function rendering it.
Each function takes the annotations to render and, optionally, the file
or buffer they were taken against, and returns a string."
  :type '(alist :key-type symbol :value-type function)
  :group 'scholia)

(defun scholia-export-render (annotations &optional format file-or-buffer)
  "Return ANNOTATIONS rendered as FORMAT against FILE-OR-BUFFER.
FORMAT defaults to `scholia-export-format'.  ANNOTATIONS are rendered as
they come: narrowing them to one file or one session is the caller's.
An unregistered FORMAT signals `scholia-export-unknown-format'."
  (let* ((format (or format scholia-export-format))
         (formatter (alist-get format scholia-export-functions)))
    (unless formatter
      (signal 'scholia-export-unknown-format (list format)))
    (funcall formatter annotations file-or-buffer)))


;;;; The command

(defun scholia-export--payload (&optional session)
  "Return this buffer's roots and stored replies owned by SESSION."
  (let* ((session (or session (scholia-session-name)))
         (annotations
          (mapcar #'scholia-db--snapshot
                  (scholia-core--buffer-annotations session)))
         (location (scholia-locate-source 'capture (point-min)))
         (source (scholia-locate-location-source-id location)))
    (append annotations
            (and source
                 (seq-filter
                  #'scholia-db-annotation-reply-p
                  (scholia-db-record-annotations
                   (scholia-db-record (scholia-session-file session)
                                      source)))))))

(defun scholia-export--disk-source (file cache)
  "Return FILE's disk contents from CACHE, or `missing'."
  (let ((source (gethash file cache :unread)))
    (if (eq source :unread)
        (puthash file
                 (condition-case nil
                     (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))
                   (error 'missing))
                 cache)
      source)))

(defun scholia-export--stale (annotation)
  "Return ANNOTATION marked as stale."
  (scholia-db-annotation-set-text
   annotation
   (concat (scholia-db-annotation-text annotation) " (stale)")))

(defun scholia-export--source-groups (record)
  "Return RECORD's relocated and snapshot threads for the current buffer."
  (let* ((unplaced nil)
         (placed (scholia-db-buffer-annotations
                  record
                  (scholia-buffer-checksum)
                  (lambda (entries) (setq unplaced entries))))
         (threads (scholia-export--threads
                   (scholia-db-record-annotations record)))
         (live nil)
         (stale nil))
    (dolist (thread (car threads))
      (let ((root (scholia-export--root thread)))
        (if (seq-find
             (lambda (annotation)
               (equal (scholia-db-annotation-id annotation)
                      (scholia-db-annotation-id root)))
             unplaced)
            (push (cons (cons (scholia-export--stale root) 0)
                        (cdr thread))
                  stale)
          (push (mapcar (lambda (entry)
                          (let ((annotation
                                 (seq-find
                                  (lambda (candidate)
                                    (equal (scholia-db-annotation-id candidate)
                                           (scholia-db-annotation-id
                                            (car entry))))
                                  placed)))
                            (cons (if annotation
                                      (scholia-db--snapshot annotation)
                                    (car entry))
                                  (cdr entry))))
                        thread)
                live))))
    (cons (append (mapcan (lambda (thread) (mapcar #'car thread))
                          (nreverse live))
                  (mapcan (lambda (thread) (mapcar #'car thread))
                          (cdr threads)))
          (mapcan (lambda (thread) (mapcar #'car thread)) (nreverse stale)))))

(defun scholia-export--snapshot-source (annotations)
  "Return the smallest source carrying ANNOTATIONS' saved line context."
  (let ((placed (seq-filter (lambda (annotation)
                              (scholia-db-annotation-line annotation))
                            annotations)))
    (if (null placed)
        ""
      (let* ((last-line
              (apply #'max (mapcar #'scholia-db-annotation-line placed)))
             (lines (make-list last-line "")))
        (dolist (annotation placed)
          (setf (nth (1- (scholia-db-annotation-line annotation)) lines)
                (scholia-db-annotation-line-text annotation)))
        (string-join lines "\n")))))

(defun scholia-export--session-records (session)
  "Return SESSION's records in file order from one database snapshot."
  (scholia-db--reading
   (scholia-session-file session)
   (lambda (store)
     (mapcar (lambda (file) (scholia-store-record store file))
             (sort (copy-sequence (scholia-store-files store)) #'string<)))))

(defun scholia-export--source-views (record)
  "Return RECORD's annotations grouped by their root source view."
  (let ((annotations (scholia-db-record-annotations record))
        (groups nil))
    (dolist (annotation annotations)
      (let* ((revision (scholia-db--source-view annotation annotations))
             (group (assoc revision groups)))
        (if group
            (setcdr group (append (cdr group) (list annotation)))
          (setq groups (append groups (list (list revision annotation)))))))
    groups))

(defun scholia-export--revision-source (file revision cache)
  "Return FILE at REVISION from CACHE, or `missing'."
  (let* ((key (list file revision))
         (source (gethash key cache :unread)))
    (if (eq source :unread)
        (puthash
         key
         (let ((buffer (scholia-locate--revision-buffer file revision)))
           (if buffer
               (unwind-protect
                   (with-current-buffer buffer (buffer-string))
                 (kill-buffer buffer))
             'missing))
         cache)
      source)))

(defun scholia-export--access-view (record annotations format fallback)
  "Render ANNOTATIONS from RECORD through their source access.
FORMAT and FALLBACK select the presentation."
  (let* ((root (car annotations))
         (stored-access (scholia-db-annotation-access root))
         (access (scholia-locate-annotation-access root fallback))
         (annotations (if stored-access
                          annotations
                        (cons (scholia-db-annotation-set-access root access)
                              (cdr annotations))))
         (retrieved (scholia-locate-source 'retrieve access annotations))
         (text (scholia-locate-retrieved-text retrieved))
         (status (scholia-locate-retrieved-status retrieved))
         (truncated (scholia-locate-retrieved-truncated-p retrieved))
         (name (or (scholia-locate-source 'describe access root) fallback)))
    (with-temp-buffer
      (insert text)
      (let ((buffer-file-name (scholia-locate-access-path access))
            (view (scholia-db-make-record
                   fallback annotations (scholia-db-record-checksum record))))
        (if-let* ((mode (scholia-locate-access-mode access)))
            (when (fboundp mode) (delay-mode-hooks (funcall mode)))
          (when buffer-file-name (delay-mode-hooks (set-auto-mode))))
        (pcase-let ((`(,live . ,stale)
                     (if (eq status 'excerpt)
                         (cons nil
                               (mapcar (lambda (annotation)
                                         (if (scholia-db-annotation-reply-p annotation)
                                             annotation
                                           (scholia-export--stale annotation)))
                                       annotations))
                       (scholia-export--source-groups view))))
          (let ((live-output
                 (and live
                      (format "Access: %s [%s%s]\n\n%s"
                              name status (if truncated ", truncated" "")
                              (scholia-export-render live format name))))
                (stale-output
                 (and stale
                      (let* ((snapshot (seq-some #'scholia-db-annotation-snapshot
                                                 stale))
                             (source (or snapshot
                                         (scholia-export--snapshot-source stale)))
                             (provenance (if snapshot 'full 'excerpt)))
                        (with-temp-buffer
                          (insert source)
                          (let ((buffer-file-name (scholia-locate-access-path access)))
                            (if-let* ((mode (scholia-locate-access-mode access)))
                                (when (fboundp mode)
                                  (delay-mode-hooks (funcall mode)))
                              (when buffer-file-name
                                (delay-mode-hooks (set-auto-mode))))
                            (format "Access: %s [%s%s]\n\n%s"
                                    name provenance
                                    (if (and (eq provenance 'excerpt) truncated)
                                        ", truncated" "")
                                    (scholia-export-render stale format name))))))))
            (string-join (delq nil (list live-output stale-output)) "\n\n")))))))

(defun scholia-export--session-record (record _cache format)
  "Render RECORD through the source access of each source view as FORMAT."
  (let ((file (scholia-db-record-file record)))
    (string-join
     (mapcar (lambda (view)
               (scholia-export--access-view record (cdr view) format file))
             (scholia-export--source-views record))
     "\n\n")))

(defun scholia-export--session-payload (sessions format)
  "Return SESSIONS rendered in session and file order as FORMAT."
  (let* ((cache (make-hash-table :test #'equal))
         (sessions (if (listp sessions) sessions (list sessions)))
         (multiple (cdr sessions)))
    (string-join
     (mapcar
      (lambda (session)
        (let ((records (scholia-export--session-records session)))
          (concat (and multiple (concat "Session: " session "\n\n"))
                  (string-join
                   (mapcar
                    (lambda (record)
                      (scholia-export--session-record record cache format))
                    records)
                   "\n\n"))))
      sessions)
     "\n\n")))

(defun scholia-export--show (output)
  "Show OUTPUT in `scholia-export-buffer-name'."
  (let ((buffer (get-buffer-create scholia-export-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert output))
    (display-buffer buffer)))

(defun scholia-export--visible-sessions ()
  "Return effective sessions represented by chains in this buffer."
  (let ((owners
         (delete-dups
          (mapcar (lambda (chain)
                    (or (scholia-chain-owner chain)
                        (scholia-session-name)))
                  (scholia-buffer-chains)))))
    (or (seq-filter (lambda (session) (member session owners))
                    (scholia-effective-sessions))
        (list (scholia-session-name)))))

(defun scholia-export--buffer-access ()
  "Return the current buffer source's compact live access preamble."
  (let* ((location (scholia-locate-source 'capture (point-min)))
         (access (scholia-locate-location-access location))
         (description (scholia-locate-source 'describe access))
         (retrieved (scholia-locate-source 'retrieve access)))
    (format "Access: %s [%s]"
            description
            (or (scholia-locate-retrieved-status retrieved) 'excerpt))))

(defun scholia-export--buffer-sessions (sessions format headers)
  "Render buffer SESSIONS as FORMAT, adding HEADERS when non-nil."
  (string-join
   (mapcar
    (lambda (session)
      (let ((rendered
             (scholia-export-render (scholia-export--payload session) format)))
        (if headers
            (format "Session: %s\n\n%s\n\n%s"
                    session (scholia-export--buffer-access) rendered)
          rendered)))
    sessions)
   "\n\n"))

(defun scholia-export (&optional target format session)
  "Render visible buffer annotations and deliver them to TARGET.
FORMAT defaults to `scholia-export-format'.  SESSION selects one visible
owner; otherwise every visible owner is rendered."
  (interactive
   (list 'buffer nil
         (and current-prefix-arg
              (cdr (scholia-export--visible-sessions))
              (scholia-core--read-owner "Export session: "))))
  (let* ((sessions (if session (list session)
                     (scholia-export--visible-sessions)))
         (output (scholia-export--buffer-sessions
                  sessions format t)))
    (cond ((eq target 'kill-ring) (kill-new output))
          ((eq target 'buffer) (scholia-export--show output))
          ((stringp target) (write-region output nil target nil 'silent)))
    output))

(defun scholia-export-session (sessions &optional target format)
  "Render SESSIONS and put their diagnostics where TARGET points.
SESSIONS is one session name or a list of names.  TARGET is nil to return
the rendering, `kill-ring' to copy it, `buffer' to show it, or a file
name to write it to.  FORMAT defaults to `scholia-export-format'."
  (interactive (list (completing-read "Session: " (scholia-session-list) nil t)
                     'buffer))
  (let ((output (scholia-export--session-payload sessions format)))
    (cond ((eq target 'kill-ring) (kill-new output))
          ((eq target 'buffer) (scholia-export--show output))
          ((stringp target) (write-region output nil target nil 'silent)))
    output))

(provide 'scholia-export)
;;; scholia-export.el ends here
