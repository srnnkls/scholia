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

(require 'cl-lib)
(require 'seq)
(require 'thingatpt)
(require 'scholia-vars)
(require 'scholia-overlay)
(require 'scholia-render)
(require 'scholia-db)
(require 'scholia-thread)
(require 'scholia-locate)

(declare-function scholia-ui-read-annotation "scholia-ui")
(declare-function scholia-session-restore-visibility "scholia-session")
(declare-function scholia-mode "scholia")

(defvar scholia-mode)
(defvar cera--active)


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

(defun scholia-effective-sessions ()
  "Return this buffer's target and existing additional active sessions."
  (let ((target (scholia-session-name)))
    (delete-dups
     (cons target
           (seq-filter
            (lambda (name)
              (and (scholia-session-name-p name)
                   (file-exists-p (scholia-session-file name))))
            scholia-visible-sessions)))))

(defun scholia-core--color-offset (session)
  "Return SESSION's colour index, claiming one when it has none yet.
The index lives in the session's own header rather than in where it
stands among the others, so it survives making, renaming and removing
sessions beside it, and survives Emacs."
  (let* ((session (or session (scholia-session-name)))
         (file (condition-case nil (scholia-session-file session) (error nil))))
    (or (scholia-core--stored-color file)
        (scholia-core--claim-color file)
        0)))

(defvar scholia-core--claimed-colors nil
  "Alist of session file to the colour index claimed for it.
A session annotated into before it has a file of its own claims its
index here, so the file that appears later is written with the colour
already on screen rather than with whichever one is free by then.")

(defun scholia-core--stored-color (file)
  "Return the colour index stored in session FILE, or nil when it has none."
  (let ((color (and file
                    (condition-case nil
                        (scholia-db-session-color file)
                      (error nil)))))
    (and (integerp color) color)))

(defun scholia-core--claim-color (file)
  "Return the lowest colour index no other session took, storing it in FILE.
A session claims one the first time it is drawn and reads it back after,
so making, renaming or removing another session never moves it.  One
with no file of its own yet only registers the claim, since drawing a
buffer must leave the directory as it found it, and the file that
appears later is written with that same index."
  (let ((claimed (cdr (assoc file scholia-core--claimed-colors))))
    (unless claimed
      (let ((taken (append
                    (delq nil (mapcar #'scholia-core--stored-color
                                      (remove file
                                              (scholia-core--session-files))))
                    (delq nil (mapcar (lambda (entry)
                                        (unless (equal (car entry) file)
                                          (cdr entry)))
                                      (scholia-core--claims-here)))))
            (index 0))
        (while (memq index taken)
          (setq index (1+ index)))
        (setq claimed index)
        (push (cons file claimed) scholia-core--claimed-colors)))
    (when (and file (file-exists-p file))
      (condition-case nil
          (scholia-db-set-session-color file claimed)
        (error nil)))
    claimed))

(defun scholia-core--session-files ()
  "Return the session files of `scholia-session-directory'."
  (mapcar #'scholia-session-file (scholia-core--session-names)))

(defun scholia-core--claims-here ()
  "Return the claims made for sessions of `scholia-session-directory'.
A claim names the file it was made for, and a colour is only ever free or
taken within one directory, so claims left over from another one say
nothing about this one."
  (let ((directory (scholia--directory-name scholia-session-directory)))
    (seq-filter (lambda (entry)
                  (equal (scholia--directory-name
                          (file-name-directory (car entry)))
                         directory))
                scholia-core--claimed-colors)))

(defun scholia-core--session-names ()
  "Return the names of the sessions in `scholia-session-directory'.
A file of that directory holding no session header is none of ours and
is passed over rather than raised."
  (when (file-directory-p scholia-session-directory)
    (seq-filter
     (lambda (name)
       (condition-case nil
           (scholia-db-session-file-p (scholia-session-file name))
         (error nil)))
     (mapcar #'file-name-base
             (directory-files scholia-session-directory nil "\\.eld\\'")))))

(setq scholia-session-color-index-function #'scholia-core--color-offset)

(defun scholia-core--session-state (session)
  "Return the mutable state plist for SESSION in this buffer."
  (or (cdr (assoc-string session scholia--session-state))
      (let ((state (list :unplaced nil :hidden nil
                         :color-offset
                         (scholia-core--color-offset session))))
        (push (cons session state) scholia--session-state)
        state)))

(defun scholia-core--state-get (session property)
  "Return PROPERTY from SESSION's buffer state."
  (plist-get (scholia-core--session-state session) property))

(defun scholia-core--state-put (session property value)
  "Set PROPERTY to VALUE in SESSION's buffer state."
  (let* ((entry (assoc-string session scholia--session-state))
         (state (or (cdr entry) (scholia-core--session-state session))))
    (setq state (plist-put state property value))
    (if entry
        (setcdr entry state)
      (setcdr (assoc-string session scholia--session-state) state))
    value))

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
like one the stored annotations were never written against, and over the
text as the buffer holds it rather than as the platform would write it,
so a session written on one system still matches on another."
  (save-restriction
    (widen)
    (md5 (current-buffer) nil nil 'utf-8-unix t)))


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
    (let* ((beg (overlay-start (car chain)))
           (end (overlay-end (car (last chain))))
           (annotation (scholia-db-make-annotation
                        (scholia-core--chain-id chain)
                        (overlay-get (car chain) 'scholia-annotation)
                        beg end
                        (buffer-substring-no-properties beg end)
                        (scholia-chain-color-index chain)))
           (revision (overlay-get (car chain) 'scholia-core--revision))
           (author (get (overlay-get (car chain) 'scholia--chain-id)
                        'scholia-author))
           (annotation (if author
                           (scholia-db--with-fields annotation :author author)
                         annotation)))
      (if revision
          (scholia-db--with-fields annotation :revision revision)
        annotation))))

(defun scholia-core--author ()
  "Return who is annotating, as the git user of this buffer's repository.
The name and email come from `git config', falling back to the
variables `user-full-name' and `user-mail-address' where git has none,
and nil comes back when neither says who."
  (let* ((config (lambda (key)
                   (ignore-errors
                     (car (process-lines-ignore-status "git" "config" key)))))
         (name (or (funcall config "user.name")
                   (and (not (string-empty-p user-full-name)) user-full-name)))
         (email (or (funcall config "user.email") user-mail-address)))
    (cond ((and name email) (format "%s <%s>" name email))
          ((or name email)))))

(defun scholia-core--buffer-annotations (&optional owner)
  "Return one annotation per chain, optionally restricted to OWNER."
  (mapcar #'scholia-core--chain-annotation
          (if owner
              (seq-filter (lambda (chain)
                            (equal (or (scholia-chain-owner chain)
                                       (scholia-session-name))
                                   owner))
                          (scholia-buffer-chains))
            (scholia-buffer-chains))))

(defun scholia-core--restore (annotation owner)
  "Render ANNOTATION for OWNER with the colour and id it carries."
  (let ((chain (scholia-create-chain
                (scholia-db-annotation-beg annotation)
                (scholia-db-annotation-end annotation)
                (scholia-db-annotation-text annotation)
                (scholia-db-annotation-color annotation)
                owner)))
    (when chain
      (put (overlay-get (car chain) 'scholia--chain-id)
           'scholia-core--id
           (scholia-db-annotation-id annotation))
      (put (overlay-get (car chain) 'scholia--chain-id)
           'scholia-author
           (scholia-db-annotation-author annotation))
      (when-let* ((revision (scholia-db-annotation-revision annotation)))
        (overlay-put (car chain) 'scholia-core--revision revision)
        (scholia-render-chain chain))
      chain)))


;;;; Storing

(defun scholia-core--store (annotations &optional session)
  "Store ANNOTATIONS for their source views in SESSION."
  (let* ((session (or session (scholia-session-name)))
         (groups nil)
         (unresolved nil)
         (replies (seq-filter #'scholia-db-annotation-reply-p annotations))
         (current (scholia-locate-source 'capture (point-min)))
         (current-source (scholia-locate-location-source-id current)))
    (dolist (annotation
             (seq-remove #'scholia-db-annotation-reply-p annotations))
      (let ((location (scholia-locate-source
                       'capture (scholia-db-annotation-beg annotation))))
        (if-let* ((source (scholia-locate-location-source-id location)))
            (let ((fields (copy-sequence location)))
              (when (and (scholia-db-annotation-revision annotation)
                         (not (scholia-locate-location-revision location)))
                (cl-remf fields :revision))
              (cl-remf fields :file)
              (setq annotation
                    (apply #'scholia-db--with-fields annotation fields))
              (setq annotation
                    (apply #'scholia-db--with-fields annotation
                           (scholia-locate-source 'context location)))
              (let* ((source-view
                      (list source
                            (scholia-db-annotation-revision annotation)))
                     (group (or (assoc source-view groups)
                                (car (push (list source-view) groups)))))
                (when (or (not (equal source current-source))
                          (scholia-db-annotation-revision annotation))
                  (setq annotation
                        (scholia-db-annotation-set-bounds
                         annotation nil nil)))
                (setcdr group (cons annotation (cdr group)))))
          (push annotation unresolved))))
    (if unresolved
        (scholia-core--report
         "Annotations can not be saved: no source claimed buffer %S"
         (buffer-name))
      (unless groups
        (when current-source
          (push (list (list current-source
                            (scholia-locate-location-revision current)))
                groups)))
      (dolist (reply replies)
        (let ((group
               (or (seq-find
                    (lambda (candidate)
                      (seq-find
                       (lambda (annotation)
                         (equal (scholia-db-annotation-id annotation)
                                (scholia-db-annotation-reply-to reply)))
                       (cdr candidate)))
                    groups)
                   (and (= (length groups) 1) (car groups)))))
          (when group (setcdr group (cons reply (cdr group))))))
      (if (not groups)
          (scholia-core--report
           "Annotations can not be saved: no source claimed buffer %S"
           (buffer-name))
        (dolist (group groups)
          (let* ((source-view (car group))
                 (source (car source-view))
                 (revision (cadr source-view))
                 (additive (not (equal source current-source)))
                 (roots (nreverse (cdr group)))
                 (snapshot (and roots
                                (scholia-db-annotation-beg (car roots))
                                (scholia-locate-source
                                 'snapshot
                                 (scholia-locate-source
                                  'capture
                                  (scholia-db-annotation-beg (car roots))))))
                 (roots (if snapshot
                            (cons (apply #'scholia-db--with-fields
                                         (car roots) snapshot)
                                  (cdr roots))
                          roots))
                 (hidden (scholia-core--state-get session :hidden))
                 (unplaced (scholia-core--state-get session :unplaced))
                 (located (append roots
                                  (and (equal source current-source) hidden))))
            (when (equal session (scholia-session-name))
              (setq hidden (or hidden scholia--hidden-revision-annotations)
                    unplaced (or unplaced scholia--unplaced-annotations))
              (setq located (append roots
                                    (and (equal source current-source)
                                         hidden))))
            (let ((scholia-db--source-context-p t))
              (scholia-db-save
               (scholia-session-file session) source located
               (scholia-buffer-checksum)
               (unless additive unplaced) additive revision))))))))

(defun scholia-save-annotations ()
  "Store every visible session's annotations under their owners."
  (interactive)
  (when (bound-and-true-p cera--active)
    (user-error "Finish the annotation first: RET saves, ESC cancels"))
  (dolist (session (scholia-effective-sessions))
    (scholia-core--store
     (scholia-core--buffer-annotations session) session)))

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
      (condition-case failure
          (when scholia-autosave
            (scholia-save-annotations))
        (error
         (message "Scholia could not store a buffer: %S" failure))))))


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

(defun scholia-core--restore-placed (placed owner)
  "Render PLACED for OWNER and update its preservation state."
  (dolist (annotation (seq-remove #'scholia-db-annotation-reply-p placed))
    (when (scholia-core--restore annotation owner)
      (let ((id (scholia-db-annotation-id annotation)))
        (scholia-core--state-put
         owner :unplaced
         (seq-remove
          (lambda (candidate)
            (equal (scholia-db-annotation-id candidate) id))
          (scholia-core--state-get owner :unplaced)))))))

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

(defun scholia-core--initialize-session (session source revision checksum)
  "Restore SESSION's annotations for SOURCE at REVISION and CHECKSUM."
  (let* ((record (or (scholia-db-record (scholia-session-file session) source)
                     (scholia-db-make-record source nil nil)))
         (annotations (seq-remove #'scholia-db-annotation-reply-p
                                  (scholia-db-record-annotations record)))
         (hidden (and (not revision)
                      (not scholia-show-revision-annotations)
                      (seq-filter #'scholia-db-annotation-revision
                                  annotations)))
         (visible (if revision
                      (seq-filter
                       (lambda (annotation)
                         (equal (scholia-db-annotation-revision annotation)
                                revision))
                       annotations)
                    (seq-difference annotations hidden))))
    (scholia-core--session-state session)
    (scholia-core--state-put session :hidden hidden)
    (scholia-core--state-put session :unplaced visible)
    (scholia-core--restore-placed
     (scholia-db-buffer-annotations
      (scholia-db-make-record
       source visible
       (if revision checksum (scholia-db-record-checksum record)))
      checksum)
     session)
    (scholia-core--cache-replies (scholia-db-record-annotations record))
    (when hidden
      (scholia-core--report
       (concat "%d revision annotations hidden; set "
               "scholia-show-revision-annotations to show them")
       (length hidden)))
    (let ((unplaced (scholia-core--state-get session :unplaced)))
      (when unplaced
        (scholia-core--report
         "%s changed on disk: %s kept but not shown"
         source
         (mapconcat
          (lambda (annotation)
            (format "%S (its text %S is gone)"
                    (scholia-db-annotation-text annotation)
                    (scholia-db-annotation-annotated-text annotation)))
          unplaced ", "))))
    (when (equal session (scholia-session-name))
      (setq scholia--hidden-revision-annotations hidden
            scholia--unplaced-annotations
            (scholia-core--state-get session :unplaced)))))

(defun scholia-core--emphasize-at-point (&rest _)
  "Draw the note of every annotation point is in as the one it is in.
An overlap leaves point in more than one, and each note is drawn that
way.  Only the notes whose state changed are drawn again."
  (let ((changed (scholia-render-set-emphasis
                  (mapcar (lambda (chain)
                            (overlay-get (car chain) 'scholia--chain-id))
                          (scholia-chains-at)))))
    (dolist (chain (scholia-buffer-chains))
      (when (member (overlay-get (car chain) 'scholia--chain-id) changed)
        (scholia-refresh-chain-face chain)
        (scholia-render-chain chain)))))

(defun scholia-core--watch-point ()
  "Mark the annotations point is in after every command."
  (add-hook 'post-command-hook #'scholia-core--emphasize-at-point -90 t))

(defun scholia-core--unwatch-point ()
  "Stop marking the annotations point is in."
  (remove-hook 'post-command-hook #'scholia-core--emphasize-at-point t))

(defun scholia-core--cache-replies (annotations)
  "Hold the replies among ANNOTATIONS against the chains they answer."
  (dolist (chain (scholia-buffer-chains))
    (let* ((id (scholia-core--chain-id chain))
           (root (seq-find (lambda (candidate)
                             (equal (scholia-db-annotation-id candidate) id))
                           annotations)))
      (when root
        (let ((replies
               (delq nil
                     (scholia-thread-walk
                      (cons root (seq-filter #'scholia-db-annotation-reply-p
                                             annotations))
                      (lambda (annotation depth)
                        (unless (zerop depth)
                          (cons depth annotation)))))))
          (setf (alist-get (overlay-get (car chain) 'scholia--chain-id)
                           scholia--replies)
                replies)
          (scholia-render-chain chain))))))

(defvar scholia-core--visibility-restored nil
  "Whether the stored visibility has been read in this Emacs yet.
Read once, before the first buffer is drawn, so a session shown and then
hidden during a sitting is not brought back by the next buffer opened.")

(defun scholia-core--restore-visibility-once ()
  "Read the stored visibility the first time a buffer is drawn."
  (unless scholia-core--visibility-restored
    (setq scholia-core--visibility-restored t)
    (when scholia-persist-visibility
      (require 'scholia-session)
      (condition-case failure
          (scholia-session-restore-visibility)
        (error (scholia-core--report "Scholia could not read its state: %S"
                                     failure))))))

(defun scholia-initialize ()
  "Render every effective session and arm saving for this buffer."
  (scholia-core--restore-visibility-once)
  (scholia-core--fall-back-to-default)
  (scholia-core--watch-point)
  (add-hook 'kill-buffer-hook #'scholia-core--save-on-kill nil t)
  (add-hook 'kill-emacs-hook #'scholia-core--save-all)
  (unless (scholia-buffer-chains)
    (setq scholia--session-state nil)
    (let* ((location (scholia-locate-source 'capture (point-min)))
           (source (scholia-locate-location-source-id location))
           (revision (scholia-locate-location-revision location))
           (checksum (scholia-buffer-checksum)))
      (when source
        (dolist (session (scholia-effective-sessions))
          (scholia-core--initialize-session
           session source revision checksum))
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
  (scholia-render-clear)
  (scholia-render-emphasis-clear)
  (scholia-disarm-rechaining)
  (scholia-core--unwatch-point)
  (setq scholia--session-state nil
        scholia--replies nil
        scholia--unplaced-annotations nil
        scholia--hidden-revision-annotations nil)
  (remove-hook 'kill-buffer-hook #'scholia-core--save-on-kill t)
  (scholia-core--disarm-quit-save))


;;;; The commands

(defun scholia-core--report (format &rest arguments)
  "Report FORMAT filled with ARGUMENTS unless `scholia-use-messages' is nil."
  (when scholia-use-messages
    (apply #'message format arguments)))

(defun scholia-core--read-owner (prompt &optional alternate)
  "Read a session owner using PROMPT.
When ALTERNATE is non-nil, omit the current write target."
  (let ((sessions (if alternate
                      (delete (scholia-session-name)
                              (copy-sequence (scholia-effective-sessions)))
                    (scholia-effective-sessions))))
    (unless sessions (user-error "No alternate session is active"))
    (completing-read prompt sessions nil t)))

(defun scholia-core--annotate-range (bounds text owner)
  "Annotate BOUNDS with TEXT for OWNER.
BOUNDS is a cons of positions.  Without TEXT the note is read through
`scholia-annotation-editor'.  Report rather than signal when BOUNDS
holds no text on any line, or when the note read is empty."
  (cond
   ((not (scholia-overlay--line-segments (car bounds) (cdr bounds)))
    (scholia-core--report "Nothing to annotate: no text on any line here"))
   (t
    (let ((note (or text
                    (progn
                      (require 'scholia-ui)
                      (scholia-ui-read-annotation nil bounds)))))
      (cond
       ((string= note "")
        (scholia-core--report "Annotation text is empty"))
       (t
        ;; The mode owns saving on kill and re-chaining, so annotate turns it on.
        (unless (bound-and-true-p scholia-mode)
          (require 'scholia)
          (scholia-mode 1))
        (scholia-core--session-state owner)
        (let ((chain (scholia-create-chain (car bounds) (cdr bounds) note nil owner)))
          (when (and chain scholia-annotation-authors)
            (put (overlay-get (car chain) 'scholia--chain-id)
                 'scholia-author (scholia-core--author))
            (scholia-render-chain chain)))
        (deactivate-mark)
        (when (and (not text) (eq scholia-annotation-editor 'inline))
          (scholia-save-annotations))))))))

(defun scholia-annotate (&optional text owner)
  "Annotate the region, or the symbol at point, with TEXT for OWNER.
OWNER defaults to the buffer's resolved write target.  Interactively, a
prefix selects another effective session.

An active region makes a new annotation, overlapping whatever is already
there.  With no region, an annotation at point is edited instead, and
anything else annotates the symbol at point.  Nothing here signals: what
cannot be annotated is reported.

Annotating turns `scholia-mode' on, since the mode owns saving on kill
and re-chaining as the file is edited.

`scholia-annotation-editor' selects the input interface.  The inline
editor stores annotations after accepting input and removing its field."
  (interactive
   (list nil (and current-prefix-arg
                  (scholia-core--read-owner "Annotate in session: " t))))
  (let* ((region (and (use-region-p)
                      (cons (region-beginning) (region-end))))
         (symbol (and (not region) (bounds-of-thing-at-point 'symbol)))
         (owner (or owner (scholia-session-name))))
    (cond
     (region (scholia-core--annotate-range region text owner))
     ((scholia-chains-at) (scholia-edit-annotation text))
     (symbol (scholia-core--annotate-range symbol text owner))
     (t (scholia-core--report "Nothing to annotate at point")))))

(defun scholia-core--chain-candidate (chain)
  "Return the completion candidate identifying CHAIN."
  (format "%s — %s — [%s]"
          (scholia-chain-owner chain)
          (overlay-get (car chain) 'scholia-annotation)
          (scholia-core--chain-id chain)))

(defun scholia-core--select-chain ()
  "Return the chain selected among annotations at point."
  (let ((chains (scholia-chains-at)))
    (cond
     ((null chains) nil)
     ((null (cdr chains)) (car chains))
     (t
      (let* ((candidates
              (mapcar (lambda (chain)
                        (cons (scholia-core--chain-candidate chain) chain))
                      chains))
             (selected
              (completing-read "Annotation: " candidates nil t)))
        (cdr (assoc-string selected candidates)))))))

(defun scholia-delete-annotation ()
  "Delete the selected annotation at point and the note drawn for it.
Once the annotation has replies, which of them goes is asked for as
`scholia-reply-to' asks what it answers: the annotation itself, or one
reply with the replies under it."
  (interactive)
  (if-let* ((chain (scholia-core--select-chain)))
      (let ((target (scholia-core--read-in-thread chain "Delete: ")))
        (if (equal target (scholia-core--chain-id chain))
            (progn
              (scholia-render-forget (overlay-get (car chain) 'scholia--chain-id))
              (mapc #'delete-overlay chain))
          (scholia-core--delete-reply chain target)))
    (scholia-core--report "No annotation at point")))

(defun scholia-core--read-over (chain)
  "Read a new note for CHAIN, starting from the one it holds.
The inline field opens on the lines CHAIN's note is shown on, so the
note steps aside while it is open and comes back however it closes."
  (require 'scholia-ui)
  (let ((inline (eq scholia-annotation-editor 'inline)))
    (when inline
      (scholia-render-forget (overlay-get (car chain) 'scholia--chain-id)))
    (unwind-protect
        (scholia-ui-read-annotation
         (overlay-get (car chain) 'scholia-annotation)
         (cons (overlay-start (car chain))
               (overlay-end (car (last chain)))))
      (when inline
        (scholia-render-chain chain)))))

(defun scholia-edit-annotation (&optional text)
  "Replace the selected annotation at point with TEXT.
Without TEXT, use `scholia-annotation-editor' with the existing note as
initial input.  Preserve its owner, bounds, identity, color, and replies.
The inline editor stores the change after removing its temporary field."
  (interactive)
  (if-let* ((chain (scholia-core--select-chain)))
      (let ((note (or text (scholia-core--read-over chain))))
        (if (string-empty-p note)
            (scholia-core--report "Annotation text is empty")
          (scholia-set-chain-text chain note)
          (when (and (not text) (eq scholia-annotation-editor 'inline))
            (scholia-save-annotations))))
    (scholia-core--report "No annotation at point")))

(defun scholia-core--read-in-thread (chain prompt)
  "Return the id of the annotation of CHAIN's thread a command acts on.
It is asked for with PROMPT once CHAIN has replies: the annotation
itself is offered first, then each reply in thread order, set in by its
depth.  With no replies the annotation is the only one there is."
  (let ((replies (alist-get (overlay-get (car chain) 'scholia--chain-id)
                            scholia--replies))
        (root (scholia-core--chain-id chain)))
    (if (null replies)
        root
      (let* ((candidates
              (cons (cons (overlay-get (car chain) 'scholia-annotation) root)
                    (mapcar (pcase-lambda (`(,depth . ,reply))
                              (cons (concat (make-string (* 2 depth) ?\s)
                                            (scholia-db-annotation-text reply))
                                    (scholia-db-annotation-id reply)))
                            replies)))
             (choice (completing-read prompt candidates nil t nil nil
                                      (caar candidates))))
        (cdr (assoc choice candidates))))))

(defun scholia-core--thread-remove (replies id)
  "Return REPLIES without the reply ID and the replies under it.
REPLIES are conses of depth and annotation in thread order.  The
answer is a cons of the ids taken out and the replies left."
  (let* ((at (seq-position replies id
                           (lambda (entry id)
                             (equal (scholia-db-annotation-id (cdr entry)) id))))
         (depth (and at (car (nth at replies))))
         (end (and at
                   (let ((index (1+ at)))
                     (while (and (< index (length replies))
                                 (> (car (nth index replies)) depth))
                       (setq index (1+ index)))
                     index))))
    (if (null at)
        (cons nil replies)
      (cons (mapcar (lambda (entry) (scholia-db-annotation-id (cdr entry)))
                    (seq-subseq replies at end))
            (append (seq-take replies at) (seq-drop replies end))))))

(defun scholia-core--delete-reply (chain id)
  "Delete the reply ID under CHAIN, and the replies under it, from its session."
  (let* ((key (overlay-get (car chain) 'scholia--chain-id))
         (removal (scholia-core--thread-remove (alist-get key scholia--replies) id))
         (session-file (scholia-session-file
                        (or (scholia-chain-owner chain) (scholia-session-name))))
         (source (scholia-locate-location-source-id
                  (scholia-locate-source 'capture (overlay-start (car chain))))))
    (dolist (removed (car removal))
      (scholia-db-remove-annotation session-file source removed))
    (setf (alist-get key scholia--replies) (cdr removal))
    (scholia-render-chain chain)))

(defun scholia-core--thread-insert (replies parent reply)
  "Return REPLIES with REPLY placed as the last answer to PARENT.
REPLIES are conses of depth and annotation in thread order; a PARENT
none of them carries is the annotation they hang off."
  (let* ((at (seq-position replies parent
                           (lambda (entry id)
                             (equal (scholia-db-annotation-id (cdr entry)) id))))
         (depth (if at (car (nth at replies)) 0))
         (end (if at
                  (let ((index (1+ at)))
                    (while (and (< index (length replies))
                                (> (car (nth index replies)) depth))
                      (setq index (1+ index)))
                    index)
                (length replies))))
    (append (seq-take replies end)
            (list (cons (1+ depth) reply))
            (seq-drop replies end))))

(defun scholia-reply-to (&optional text parent)
  "Store a reply with TEXT under the selected annotation at point.
PARENT is the id of the annotation or reply it answers.  Interactively,
once the annotation has replies, it is read with completion; with none,
the reply answers the annotation."
  (interactive)
  (if-let* ((chain (scholia-core--select-chain)))
      (let* ((owner (scholia-chain-owner chain))
             (parent (or parent (scholia-core--read-in-thread chain "Reply to: ")))
             (note (or text (read-string "Reply: ")))
             (reply (scholia-db-make-annotation
                     (scholia-core--make-id) note
                     nil nil nil nil nil parent))
             (reply (if scholia-annotation-authors
                        (scholia-db--with-fields reply :author (scholia-core--author))
                      reply)))
        (scholia-core--store
         (append (scholia-core--buffer-annotations owner) (list reply))
         owner)
        (let ((key (overlay-get (car chain) 'scholia--chain-id)))
          (setf (alist-get key scholia--replies)
                (scholia-core--thread-insert
                 (alist-get key scholia--replies) parent reply)))
        (scholia-render-chain chain))
    (scholia-core--report "No annotation at point")))

(defun scholia-toggle-annotation-authors ()
  "Turn recording and showing annotation authors on or off everywhere.
Every buffer showing annotations is drawn again to match."
  (interactive)
  (setq scholia-annotation-authors (not scholia-annotation-authors))
  (dolist (buffer (buffer-list))
    (when (buffer-local-value 'scholia-mode buffer)
      (with-current-buffer buffer
        (mapc #'scholia-render-chain (scholia-buffer-chains)))))
  (scholia-core--report "Annotation authors %s"
                        (if scholia-annotation-authors "on" "off")))

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
