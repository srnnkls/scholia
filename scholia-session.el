;;; scholia-session.el --- The named session store  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/scholia
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Makes, switches, renames, deletes and imports the named sessions
;; buffers annotate into, and holds the project assignments that route a
;; buffer to one of them across restarts.

;;; Code:

(require 'seq)
(require 'scholia-vars)
(require 'scholia-store)
(require 'scholia-db)
(require 'scholia-core)

(declare-function scholia-mode "scholia")

(define-error 'scholia-session-error "Scholia session error" 'scholia-error)

(defcustom scholia-session-switch-hook nil
  "Functions run once `scholia-session-switch' has redrawn every buffer."
  :type 'hook
  :group 'scholia)


;;;; Names

(defun scholia-session--refuse (format &rest arguments)
  "Signal `scholia-session-error' carrying FORMAT filled with ARGUMENTS."
  (signal 'scholia-session-error (list (apply #'format format arguments))))

(defun scholia-session--check-name (name)
  "Return NAME, signalling unless it names a file of the session directory."
  (unless (scholia-session-name-p name)
    (scholia-session--refuse "Not a session name: %S" name))
  name)

(defun scholia-session--existing-file (name)
  "Return the real session file NAME names, or signal."
  (let ((file (scholia-session-file name)))
    (unless (and (file-exists-p file) (scholia-db-session-file-p file))
      (scholia-session--refuse "There is no session called %s" name))
    file))

(defun scholia-session--bindings ()
  "Return the buffer-local session bindings currently in force."
  (delq nil
        (mapcar (lambda (buffer)
                  (when (local-variable-p 'scholia-session buffer)
                    (cons buffer (buffer-local-value 'scholia-session buffer))))
                (buffer-list))))

(defun scholia-session--restore-bindings (bindings)
  "Restore the buffer-local session BINDINGS exactly."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (kill-local-variable 'scholia-session)))
  (dolist (binding bindings)
    (when (buffer-live-p (car binding))
      (with-current-buffer (car binding)
        (setq-local scholia-session (cdr binding))))))

(defun scholia-session--restore-header (file header)
  "Restore FILE's session HEADER."
  (scholia-db--writing
   file
   (lambda (store) (scholia-store-put-session store header))))

(defun scholia-session--session-file-p (name)
  "Return non-nil when NAME answers to a database carrying a session header.
Reading the header is what tells a session apart from the other files
that may share `scholia-session-directory', such as the assignment
store.  Anything the read raises answers no, so a file that is
unreadable, a directory, or no session name at all costs the caller the
one entry rather than the whole listing."
  (condition-case nil
      (scholia-db-session-file-p (scholia-session-file name))
    (error nil)))

(defun scholia-session-list ()
  "Return the names of the sessions in `scholia-session-directory'.
A session answers to the name of its file, which is what resolution
keys on."
  (when (file-directory-p scholia-session-directory)
    (seq-filter #'scholia-session--session-file-p
                (mapcar #'file-name-base
                        (directory-files scholia-session-directory
                                         nil "\\.eld\\'")))))

(defun scholia-session--read-name (prompt)
  "Read the name of a session that exists, showing PROMPT."
  (completing-read prompt (scholia-session-list) nil t))

(defun scholia-session--buffers-of (names)
  "Return annotated buffers showing one of the sessions NAMES."
  (seq-filter
   (lambda (buffer)
     (with-current-buffer buffer
       (seq-some (lambda (name)
                   (member name (scholia-effective-sessions)))
                 names)))
   (scholia-core--annotated-buffers)))

(defun scholia-session--save-buffers (buffers)
  "Save every annotated buffer in BUFFERS."
  (dolist (buffer buffers)
    (with-current-buffer buffer (scholia-save-annotations))))

(defun scholia-session--redraw-buffers (buffers)
  "Redraw every annotated buffer in BUFFERS."
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (scholia-shutdown nil)
        (scholia-mode 1)))))

(defun scholia-session--all-buffers ()
  "Return all buffers currently displaying annotations."
  (scholia-core--annotated-buffers))


;;;; Project assignments

(defun scholia-session--root (&optional root)
  "Return ROOT normalized, or the root of the project this buffer is in.
Nil comes back when there is no project here."
  (scholia--directory-name
   (or root (funcall scholia-project-root-function))))

(defun scholia-session--write-assignments (assignments)
  "Store ASSIGNMENTS in `scholia-session-state-file'.
Written beside the store and renamed over it, which within one directory
is atomic, so a write cut short leaves the assignments that were there
before it whole.  The store is created at the mode `make-temp-file'
gives it rather than at the umask."
  (when scholia-session-state-file
    (let* ((target (expand-file-name scholia-session-state-file))
           (directory (file-name-directory target)))
      (make-directory directory t)
      (let ((temporary (make-temp-file
                        (expand-file-name "scholia-assignments-" directory)))
            (print-length nil)
            (print-level nil))
        (unwind-protect
            (progn
              (with-temp-file temporary
                (prin1 assignments (current-buffer))
                (insert "\n"))
              (rename-file temporary target t))
          (when (file-exists-p temporary)
            (delete-file temporary)))))
    (setq scholia--assignment-cache
          (cons scholia-session-state-file assignments))))

(defun scholia-session--without-root (assignments root)
  "Return ASSIGNMENTS without the entry keying the directory ROOT."
  (seq-remove (lambda (entry)
                (equal (scholia-session--root (car entry)) root))
              assignments))

(defun scholia-session--map-assignments (function)
  "Pass every live and every stored assignment through FUNCTION.
An entry FUNCTION answers nil for is dropped.  The store is only
rewritten when it holds something, so reading a session's name never
mints a state file."
  (set-default 'scholia-project-sessions
               (delq nil (mapcar function
                                 (default-value 'scholia-project-sessions))))
  (let ((stored (scholia-stored-assignments)))
    (when stored
      (scholia-session--write-assignments
       (delq nil (mapcar function stored))))))

(defun scholia-session-load-assignments ()
  "Read `scholia-session-state-file' again and return its assignments.
Resolution consults the store itself, so a session name is found without
this command; it is here for a state file edited or replaced from outside
Emacs, which the cache would otherwise not notice.  A configured entry in
`scholia-project-sessions' answers before a stored one either way, so
rereading the store cannot take a project away from the assignment
`init.el' makes for it."
  (interactive)
  (setq scholia--assignment-cache nil)
  (scholia-stored-assignments))

(defun scholia-session-assign-project (name &optional root)
  "Annotate every buffer of the project at ROOT into the session NAME.
ROOT defaults to the project this buffer belongs to.  The assignment is
written to `scholia-session-state-file' and read back from there by
resolution, so it holds across restarts."
  (interactive (list (scholia-session--read-name "Session for this project: ")))
  (scholia-session--check-name name)
  (let ((root (scholia-session--root root)))
    (unless root
      (scholia-session--refuse "This buffer belongs to no project"))
    (set-default 'scholia-project-sessions
                 (cons (cons root name)
                       (scholia-session--without-root
                        (default-value 'scholia-project-sessions) root)))
    (scholia-session--write-assignments
     (cons (cons root name)
           (scholia-session--without-root (scholia-stored-assignments)
                                          root)))
    name))


;;;; The references a session is reached through

(defun scholia-session--rebind-buffers (old new)
  "Point every buffer bound to the session OLD at NEW.
A nil NEW drops the binding rather than replacing it, leaving the buffer
to resolve afresh.  Buffers command `scholia-mode' is off in are reached
as well, since their binding decides where their next save lands."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (local-variable-p 'scholia-session)
                 (equal scholia-session old))
        (if new
            (setq-local scholia-session new)
          (kill-local-variable 'scholia-session))))))

(defun scholia-session--rebind (old new)
  "Point the global default, every buffer and every assignment at NEW.
OLD names the session left behind, and a nil NEW drops the references to
it instead of pointing them somewhere.  The default is compared as
`scholia-session-default-name' resolves it, since a default left nil
answers to the name every buffer then annotates into."
  (when (equal (scholia-session-default-name) old)
    (set-default 'scholia-session new))
  (set-default
   'scholia-active-sessions
   (delete-dups
    (delq nil
          (mapcar (lambda (session)
                    (if (equal session old) new session))
                  scholia-active-sessions))))
  (scholia-session--rebind-buffers old new)
  (scholia-session--map-assignments
   (lambda (entry)
     (cond ((not (equal (cdr entry) old)) entry)
           (new (cons (car entry) new))))))


;;;; The commands

(defun scholia-session--taken-p (file)
  "Return non-nil when FILE is one `scholia-session-create' must not take.
A file carrying a session is taken, and so is one that cannot be read as
scholia's at all: freeing a name for anything unreadable would empty a
file scholia never made.  A database carrying no session header is
neither, and is what a first write killed outright leaves behind: the
name it sits at answers to no session, so the next create writes its
header into it rather than reporting a session nothing can open."
  (condition-case nil
      (scholia-db-session-file-p file)
    (error t)))

(defun scholia-session-activate (name)
  "Show session NAME in every annotated buffer."
  (interactive (list (scholia-session--read-name "Activate session: ")))
  (scholia-session--check-name name)
  (scholia-session--existing-file name)
  (unless (member name scholia-active-sessions)
    (let ((buffers (scholia-session--all-buffers)))
      (scholia-session--save-buffers buffers)
      (set-default 'scholia-active-sessions
                   (append scholia-active-sessions (list name)))
      (scholia-session--redraw-buffers buffers)))
  name)

(defun scholia-session-deactivate (name)
  "Stop showing session NAME in annotated buffers."
  (interactive
   (list (completing-read "Deactivate session: "
                          scholia-active-sessions nil t)))
  (when (member name scholia-active-sessions)
    (let ((buffers (scholia-session--all-buffers)))
      (scholia-session--save-buffers buffers)
      (set-default 'scholia-active-sessions
                   (delete name (copy-sequence scholia-active-sessions)))
      (scholia-session--redraw-buffers buffers)))
  name)

(defun scholia-session-create (name)
  "Make and activate an empty session called NAME."
  (interactive (list (read-string "New session: ")))
  (scholia-session--check-name name)
  (let ((file (scholia-session-file name)))
    (when (scholia-session--taken-p file)
      (scholia-session--refuse "A session called %s already exists" name))
    (scholia-db-create-session file)
    (set-default 'scholia-session name)
    (scholia-session-activate name)))

(defun scholia-session-switch (name)
  "Use NAME as the global target and keep it visible."
  (interactive (list (scholia-session--read-name "Switch to session: ")))
  (scholia-session--check-name name)
  (scholia-session--existing-file name)
  (let* ((old (scholia-session-default-name))
         (buffers (scholia-session--all-buffers)))
    (scholia-session--save-buffers buffers)
    (set-default 'scholia-session name)
    (set-default 'scholia-active-sessions
                 (delete-dups
                  (append scholia-active-sessions (list old name))))
    (scholia-session--redraw-buffers buffers))
  (run-hooks 'scholia-session-switch-hook)
  name)

(defun scholia-session-rename (old new)
  "Rename the session called OLD to NEW.
Every buffer annotating into OLD stores what it holds before the file
moves, and the global default, the buffer-local bindings and the project
assignments naming OLD are pointed at NEW afterwards, so nothing is left
resolving to a session that is gone.  A NEW already taken is refused
before anything is saved or moved.

The database itself is moved by `scholia-store-rename', which takes the
journals beside it along; unlinking the session file alone would leave
them orphaned at a name nothing answers to."
  (interactive (list (scholia-session--read-name "Rename session: ")
                     (read-string "New name: ")))
  (scholia-session--check-name old)
  (scholia-session--check-name new)
  (let* ((source (scholia-session--existing-file old))
         (target (scholia-session-file new))
         (header (plist-get (scholia-db-interchange source) :session))
         (default (default-value 'scholia-session))
         (active (copy-sequence scholia-active-sessions))
         (bindings (scholia-session--bindings))
         (projects (copy-tree (default-value 'scholia-project-sessions)))
         (cache (copy-tree scholia--assignment-cache))
         (moved nil))
    (when (file-exists-p target)
      (scholia-session--refuse "A session called %s already exists" new))
    (let ((affected (scholia-session--buffers-of (list old))))
      (scholia-session--save-buffers affected)
      (condition-case failure
          (progn
            (scholia-store-rename source target)
            (setq moved t)
            (scholia-db-set-session-name target new)
            (scholia-session--rebind old new)
            (scholia-session--redraw-buffers affected)
            new)
        (error
         (when moved
           (scholia-store-rename target source)
           (scholia-session--restore-header source header))
         (set-default 'scholia-session default)
         (set-default 'scholia-active-sessions active)
         (scholia-session--restore-bindings bindings)
         (set-default 'scholia-project-sessions projects)
         (setq scholia--assignment-cache cache)
         (ignore-errors (scholia-session--redraw-buffers affected))
         (signal (car failure) (cdr failure)))))))

(defun scholia-session-delete (name &optional force)
  "Delete the session called NAME.
A session a buffer still annotates into is refused unless FORCE, since
what stands on screen there may be the only copy of it.  Forcing stores
nothing and takes no buffer down: the file goes, and the global default,
the buffer-local bindings and the project assignments naming NAME are
dropped, so what those buffers hold is theirs to save somewhere else.

The database goes through `scholia-store-delete', which takes the
journals beside it along; unlinking the session file alone would leave
them for the next session of the same name to open."
  (interactive (list (scholia-session--read-name "Delete session: ")
                     current-prefix-arg))
  (scholia-session--check-name name)
  (let* ((file (scholia-session--existing-file name))
         (active (member name scholia-active-sessions))
         (affected (scholia-session--buffers-of (list name)))
         (redraw (if active (scholia-session--all-buffers) affected)))
    (when (and (not force) affected)
      (scholia-session--refuse "Buffers still annotate into %s" name))
    (scholia-store-delete file)
    (scholia-session--rebind name nil)
    (when active (scholia-session--redraw-buffers redraw))
    name))

(defun scholia-session-export (name file)
  "Write the session called NAME out to FILE.
FILE carries the printed plist under the `:scholia' version tag rather
than the bytes the session is stored as, so a session stays something to
diff, commit and hand to a colleague, and `scholia-session-import' reads
back what this writes.  The arguments read the way
`scholia-session-rename' does, from the session towards where it goes;
`scholia-session-import' takes the outside file first because it names
where that file lands.  A FILE already there is replaced only once the
user has said so, the way every other command writing to a path the
user named asks."
  (interactive (list (scholia-session--read-name "Export session: ")
                     (read-file-name "Write to: ")))
  (scholia-session--check-name name)
  (let ((source (scholia-session--existing-file name)))
    (when (and (file-exists-p file)
               (not (yes-or-no-p (format "Replace %s? " file))))
      (scholia-session--refuse "Not replacing %s" file))
    (scholia-store-write-interchange file (scholia-db-interchange source))
    file))

(defun scholia-session-import (file name)
  "Take the session stored in FILE into the session called NAME.
FILE is read before anything is written, so one carrying no scholia
version tag signals `scholia-db-format-error' and leaves NAME unmade.  A
NAME already taken keeps its header and everything it holds, and the
records of FILE are merged into it, annotations shared by id counting
once.  A reply arriving without the annotation it answers is stamped
before the merge is written, since the record it lands in may belong to
a file no buffer here visits and nothing else would reach it.

Every buffer resolving to NAME stores what it holds before the merge
reads NAME and is redrawn after it is written, the way
`scholia-session-switch' does: a buffer holds the record it will next
write whole, so a merged annotation it knows nothing about would be
written back out of existence by its next save."
  (interactive (list (read-file-name "Session file: " nil nil t)
                     (read-string "Import as: ")))
  (scholia-session--check-name name)
  (let* ((incoming (scholia-db-interchange file))
         (target (scholia-session-file name))
         (affected (scholia-session--buffers-of (list name))))
    (dolist (buffer affected)
      (with-current-buffer buffer
        (scholia-save-annotations)))
    (scholia-db-import target incoming)
    (dolist (buffer affected)
      (with-current-buffer buffer
        (scholia-shutdown nil)
        (scholia-mode 1)))
    name))

(provide 'scholia-session)
;;; scholia-session.el ends here
