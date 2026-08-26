;;; scholia-core.el --- The annotation lifecycle  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/scholia
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Joins the overlay engine to the record store.

;;; Code:

(require 'seq)
(require 'thingatpt)
(require 'scholia-vars)
(require 'scholia-overlay)
(require 'scholia-db)


;;;; What a buffer annotates, and into which session

(defun scholia-core--base-buffer ()
  "Return the buffer whose file this one annotates.
An indirect buffer annotates the file of its base, so both halves of a
clone reach the same record."
  (or (buffer-base-buffer) (current-buffer)))

(defun scholia-buffer-file ()
  "Return the file this buffer annotates, or nil when it visits none."
  (buffer-file-name (scholia-core--base-buffer)))

(defun scholia-session-default-name ()
  "Return the session a buffer annotates into when nothing else binds one."
  (or (default-value 'scholia-session) "default"))

(defun scholia-core--project-session ()
  "Return the session assigned to the project this buffer belongs to.
The assignments configured in `scholia-project-sessions' are searched
before the ones `scholia-session-assign-project' stored, so a project
answers to its configuration whatever a past assignment left behind.
Both sides of the lookup are expanded and slash-terminated, so a root
configured as \"~/src/foo\" answers for a project rooted at
\"/home/me/src/foo/\"."
  (let ((root (scholia--directory-name
               (funcall scholia-project-root-function))))
    (when root
      (cdr (seq-find (lambda (entry)
                       (equal (scholia--directory-name (car entry)) root))
                     (append scholia-project-sessions
                             (scholia-stored-assignments)))))))

(defun scholia-session-name ()
  "Return the name of the session this buffer annotates into.
A binding the buffer itself made answers first and one its base buffer
made next, then the entry `scholia-project-sessions' holds for the
project root, and `scholia-session-default-name' when nothing else does.
Only a binding actually made answers: `scholia-session' is a defcustom,
so reading it where no buffer made one hands back the global default and
the project rung would never be reached."
  (let ((base (scholia-core--base-buffer)))
    (or (and (local-variable-p 'scholia-session) scholia-session)
        (and (local-variable-p 'scholia-session base)
             (buffer-local-value 'scholia-session base))
        (scholia-core--project-session)
        (scholia-session-default-name))))

(defun scholia-session-name-p (name)
  "Return non-nil when NAME names a file of `scholia-session-directory'.
A name carrying a directory part resolves outside that directory, where
scholia would read and overwrite a file that is none of its business."
  (and (stringp name)
       (not (string-empty-p name))
       (equal name (file-name-nondirectory name))
       (not (member name '("." "..")))))

(defun scholia-session-file (&optional name)
  "Return the file holding the session called NAME.
NAME defaults to the session this buffer annotates into, which answers
before command `scholia-mode' is enabled as well as after.  A name that
would resolve outside `scholia-session-directory' is refused here rather
than in the commands, since `scholia-session' and
`scholia-project-sessions' are set in configuration and reach the load
and save paths without passing one."
  (let ((name (or name (scholia-session-name))))
    (unless (scholia-session-name-p name)
      (signal 'scholia-error (list (format "Not a session name: %S" name))))
    (expand-file-name (concat name ".eld") scholia-session-directory)))

(defun scholia-buffer-checksum ()
  "Return the fingerprint of this buffer as it stands.
Taken over the whole buffer, so a narrowing does not make the file look
like one the stored annotations were never written against."
  (save-restriction
    (widen)
    (md5 (current-buffer))))


;;;; The identity a chain keeps

(defconst scholia-core--gregorian-offset #x01b21dd213814000
  "Hundreds of nanoseconds between 1582-10-15 and the Unix epoch.
RFC 9562 counts a version 1 timestamp from the Gregorian reform, which
`time-convert' does not know about.")

(defun scholia-core--make-id ()
  "Return an id no other annotation of any session carries.
A version 1 UUID as RFC 9562 defines it: a timestamp of hundreds of
nanoseconds, a random clock sequence and a random node with the
multicast bit set, which the RFC reserves for nodes that are not a
hardware address.  Ids are minted as annotations are stored, so a first
save mints a whole session's worth of them under one reading of the
clock and the random part alone has to keep them apart."
  (let ((now (+ (car (time-convert nil 10000000))
                scholia-core--gregorian-offset))
        (node (logior (ash 1 40) (random (ash 1 40)))))
    (format "%08x-%04x-%04x-%04x-%012x"
            (logand #xffffffff now)
            (logand #xffff (ash now -32))
            (logior #x1000 (logand #x0fff (ash now -48)))
            (logior #x8000 (random #x4000))
            node)))

(defun scholia-core--chain-id (chain)
  "Return the stored id of CHAIN, giving it one when it has none yet.
The id hangs off the symbol the overlays of a chain share, which
re-chaining after an edit passes on, so a chain answers with the same id
however often it is stored and whatever the edits in between."
  (let ((chain-id (overlay-get (car chain) 'scholia--chain-id)))
    (or (get chain-id 'scholia-core--id)
        (put chain-id 'scholia-core--id (scholia-core--make-id)))))


;;;; Chains and stored annotations

(defun scholia-core--chain-annotation (chain)
  "Return the annotation CHAIN stands for, spanning all of its overlays."
  (save-restriction
    (widen)
    (let ((beg (overlay-start (car chain)))
          (end (overlay-end (car (last chain)))))
      (scholia-db-make-annotation
       (scholia-core--chain-id chain)
       (overlay-get (car chain) 'scholia-annotation)
       beg
       end
       (buffer-substring-no-properties beg end)
       (scholia-chain-color-index chain)))))

(defun scholia-core--buffer-annotations ()
  "Return one annotation per chain of this buffer, in buffer order."
  (mapcar #'scholia-core--chain-annotation (scholia-buffer-chains)))

(defun scholia-core--restore (annotation)
  "Render ANNOTATION in this buffer with the colour and id it carries.
Returns the chain drawn, or nil when the range it covers left nothing to
draw on."
  (let ((chain (scholia-create-chain (scholia-db-annotation-beg annotation)
                                     (scholia-db-annotation-end annotation)
                                     (scholia-db-annotation-text annotation)
                                     (scholia-db-annotation-color annotation))))
    (when chain
      (put (overlay-get (car chain) 'scholia--chain-id)
           'scholia-core--id
           (scholia-db-annotation-id annotation))
      chain)))


;;;; Storing

(defun scholia-core--store (annotations)
  "Store ANNOTATIONS of this buffer, replacing whatever its record had.
The annotations of `scholia--unplaced-annotations' are folded back into
the record as they are, so a save never drops what could not be shown.
A buffer visiting no file keys no record and is reported instead, which
every path storing a buffer inherits."
  (let ((file (scholia-buffer-file)))
    (if (not file)
        (scholia-core--report
         "Annotations can not be saved: unable to find a file for buffer %S"
         (buffer-name))
      (scholia-db-save (scholia-session-file)
                       file
                       annotations
                       (scholia-buffer-checksum)
                       scholia--unplaced-annotations))))

(defun scholia-save-annotations ()
  "Store the annotations of this buffer into its session.
A buffer left without a chain stores a record holding none rather than
losing its record, which a later session would read as a file that was
never annotated."
  (interactive)
  (scholia-core--store (scholia-core--buffer-annotations)))

(defun scholia-core--annotated-buffers ()
  "Return the buffers command `scholia-mode' is on in."
  (seq-filter (lambda (buffer)
                (buffer-local-value 'scholia-mode buffer))
              (buffer-list)))

(defun scholia-core--disarm-quit-save ()
  "Drop the quit save once this buffer is the last that needed it.
The `kill-emacs-hook' entry is one for all of Emacs, so removing it
while another buffer still carries annotations would cost that buffer
its session.  Both ways out of an annotated buffer come through here:
turning the mode off reaches `scholia-shutdown', and killing the buffer
never does."
  (unless (remq (current-buffer) (scholia-core--annotated-buffers))
    (remove-hook 'kill-emacs-hook #'scholia-core--save-all)))

(defun scholia-core--save-on-kill ()
  "Store the annotations of this buffer while there still is one."
  (when scholia-autosave
    (scholia-save-annotations))
  (scholia-core--disarm-quit-save))

(defun scholia-core--save-all ()
  "Store every buffer command `scholia-mode' is on in.
Runs from `kill-emacs-hook', which `kill-buffer-hook' never reaches, so
quitting Emacs costs no annotation.  A buffer whose session cannot be
written is reported and passed over rather than raising: a signal here
would keep Emacs from quitting, and by then there is nothing the user
can do about the session and no later run to retry it in, so one that
cannot be written costs the others nothing."
  (dolist (buffer (scholia-core--annotated-buffers))
    (with-current-buffer buffer
      (with-demoted-errors "Scholia could not store a buffer: %S"
        (when scholia-autosave
          (scholia-save-annotations))))))


;;;; The mode's two paths

(defun scholia-core--advance-colors (chains)
  "Move `scholia--colors-index-counter' past the colours CHAINS carry.
A restored chain is drawn with the colour it was stored with rather than
the next one the counter offers, so a buffer visited afresh would hand
its first new annotation a colour a restored one already wears, and the
two read as a single underline where they meet."
  (dolist (chain chains)
    (setq scholia--colors-index-counter
          (max scholia--colors-index-counter
               (1+ (scholia-chain-color-index chain))))))

(defun scholia-core--restore-placed (placed)
  "Render the annotations of PLACED, one leaving the preserve list per draw.
An annotation leaves `scholia--unplaced-annotations' only once a chain
of it stands in the buffer, so a render that signals partway leaves
everything it did not reach preserved and the next save keeps it.
Replies carry no position and so nothing to render; the record store
folds them back on its own and they never enter the preserve list."
  (dolist (annotation (seq-remove #'scholia-db-annotation-reply-p placed))
    (when (scholia-core--restore annotation)
      (let ((id (scholia-db-annotation-id annotation)))
        (setq scholia--unplaced-annotations
              (seq-remove (lambda (candidate)
                            (equal (scholia-db-annotation-id candidate) id))
                          scholia--unplaced-annotations))))))

(defun scholia-core--fall-back-to-default ()
  "Bind this buffer to the default session when the one it names is unusable.
A session is unusable when nobody made it or when its name would leave
`scholia-session-directory', and a project assignment is the only rung
that can name either.  Both are answered for here rather than in
`scholia-session-name' because that resolves afresh on every save and
every load and would stat the file and repeat the report each time.
Falling back rather than signalling is what keeps a typo in
`scholia-project-sessions' from making a file unannotatable; a caller
naming the session itself still gets the signal from
`scholia-session-file'."
  (let ((name (scholia-session-name))
        (fallback (scholia-session-default-name)))
    (unless (or (local-variable-p 'scholia-session)
                (equal name fallback))
      (cond
       ((not (scholia-session-name-p name))
        (scholia-core--report
         "Session %s is not a session name: annotating into %s instead"
         name fallback)
        (setq-local scholia-session fallback))
       ((not (file-exists-p (scholia-session-file name)))
        (scholia-core--report "Session %s is gone: annotating into %s instead"
                              name fallback)
        (setq-local scholia-session fallback))))))

(defun scholia-initialize ()
  "Render the annotations stored for this buffer and arm the save on kill.
The save is armed before anything is read, so a read that signals still
leaves the buffer able to store what it holds.  A buffer already holding
chains is left alone, so enabling the mode twice does not draw a second
copy of every annotation over the first.  Every stored annotation counts
as unplaced until a chain of it stands in the buffer, so whatever the
render did not reach is reported by name and kept in the record."
  (scholia-core--fall-back-to-default)
  (add-hook 'kill-buffer-hook #'scholia-core--save-on-kill nil t)
  (add-hook 'kill-emacs-hook #'scholia-core--save-all)
  (let ((file (scholia-buffer-file)))
    (when (and file (not (scholia-buffer-chains)))
      (let ((db (scholia-db-load (scholia-session-file))))
        (setq scholia--unplaced-annotations
              (seq-remove #'scholia-db-annotation-reply-p
                          (scholia-db-record-annotations
                           (scholia-db-record db file))))
        (scholia-core--restore-placed
         (scholia-db-buffer-annotations db file (scholia-buffer-checksum)))
        (when scholia--unplaced-annotations
          (scholia-core--report
           "%s changed on disk: %s kept but not shown"
           (file-name-nondirectory file)
           (mapconcat
            (lambda (annotation)
              (format "%S (its text %S is gone)"
                      (scholia-db-annotation-text annotation)
                      (scholia-db-annotation-annotated-text annotation)))
            scholia--unplaced-annotations
            ", ")))
        (scholia-core--advance-colors (scholia-buffer-chains))))))

(defun scholia-shutdown (save)
  "Take the annotations of this buffer down, storing them first when SAVE.
A store that fails takes nothing down: the annotations stay on screen,
re-chaining stays armed, the `kill-buffer-hook' entry stays in place and
command `scholia-mode' is left on, since the buffer is still the
annotated buffer it was.  The signal reaches the caller as it was
raised.  The `kill-emacs-hook' entry is one for all of Emacs, so it goes
only once no annotated buffer is left to need it."
  (when save
    (condition-case failure
        (scholia-save-annotations)
      (error (setq scholia-mode t)
             (signal (car failure) (cdr failure)))))
  (dolist (chain (scholia-buffer-chains))
    (dolist (overlay chain)
      (delete-overlay overlay)))
  (scholia-disarm-rechaining)
  (remove-hook 'kill-buffer-hook #'scholia-core--save-on-kill t)
  (scholia-core--disarm-quit-save))


;;;; The commands

(defun scholia-core--report (format &rest arguments)
  "Report FORMAT filled with ARGUMENTS unless `scholia-use-messages' is nil."
  (when scholia-use-messages
    (apply #'message format arguments)))

(defun scholia-core--bounds ()
  "Return the range to annotate as a cons, or nil when there is none.
The region answers while it is active and the symbol at point otherwise."
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (bounds-of-thing-at-point 'symbol)))

(defun scholia-core--annotated-range-p (beg end)
  "Return non-nil when an annotation of this buffer reaches into BEG to END."
  (seq-find #'scholia-annotation-p (overlays-in beg end)))

(defun scholia-annotate (&optional text)
  "Annotate the region, or the symbol at point when no region is active.
TEXT is the note, asked for when it is not given.  Nothing to annotate
at point, a range holding no line text to hang a chain on, a range an
annotation already reaches into, and an empty note each create no
annotation, spend no colour, and report instead: the lookup
`scholia-annotation-at' does reads one annotation per position, and an
empty note would hold a range against that lookup while showing nothing
and exporting as an empty diagnostic.  A region annotated is
deactivated, so the chord pressed twice cannot annotate it twice."
  (interactive)
  (let ((bounds (scholia-core--bounds)))
    (cond
     ((not bounds)
      (scholia-core--report "Nothing to annotate at point"))
     ((not (scholia-overlay--line-segments (car bounds) (cdr bounds)))
      (scholia-core--report "Nothing to annotate: no text on any line here"))
     ((scholia-core--annotated-range-p (car bounds) (cdr bounds))
      (scholia-core--report "Annotations can not overlap: %s is annotated"
                            (buffer-substring-no-properties (car bounds)
                                                            (cdr bounds))))
     (t
      (let ((note (or text (read-string "Annotation: "))))
        (if (string= note "")
            (scholia-core--report "Annotation text is empty")
          (scholia-create-chain (car bounds) (cdr bounds) note)
          (deactivate-mark)))))))

(defun scholia-delete-annotation ()
  "Delete the annotation at point, every overlay of its chain with it."
  (interactive)
  (let ((overlay (scholia-annotation-at)))
    (if overlay
        (scholia-delete-chain overlay)
      (scholia-core--report "No annotation at point"))))

(defun scholia-reply-to (&optional text)
  "Answer the annotation at point with TEXT, asked for when not given.
A reply holds no position of its own, so nothing on screen would keep it
and it is stored as it is made."
  (interactive)
  (let ((chain (scholia-chain-at (point))))
    (if (not chain)
        (scholia-core--report "No annotation at point")
      (scholia-core--store
       (append (scholia-core--buffer-annotations)
               (list (scholia-db-make-annotation
                      (scholia-core--make-id)
                      (or text (read-string "Reply: "))
                      nil nil nil nil nil
                      (scholia-core--chain-id chain))))))))

(defun scholia-goto-next-annotation ()
  "Move point to the annotation after it, staying put when there is none."
  (interactive)
  (let ((chain (scholia-next-annotation (point))))
    (if chain
        (goto-char (overlay-start (car chain)))
      (scholia-core--report "No annotation after point"))))

(defun scholia-goto-previous-annotation ()
  "Move point to the annotation before it, staying put when there is none."
  (interactive)
  (let ((chain (scholia-previous-annotation (point))))
    (if chain
        (goto-char (overlay-start (car chain)))
      (scholia-core--report "No annotation before point"))))

(provide 'scholia-core)
;;; scholia-core.el ends here
