;;; scholia-vars.el --- Shared scholia definitions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/scholia
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The leaf every other scholia module requires.  It requires no scholia
;; module itself, so the halves above it never require each other.

;;; Code:

(declare-function project-current "project")
(declare-function project-root "project")
(declare-function projectile-project-root "ext:projectile")

(defgroup scholia nil
  "Annotate files without changing them."
  :group 'text
  :prefix "scholia-")


;;;; Sessions

(defcustom scholia-session-directory
  (locate-user-emacs-file "scholia/sessions/")
  "Directory holding the session databases."
  :type 'directory)

(defcustom scholia-session nil
  "Name of the session this buffer annotates into.
Nil means the session resolved from `scholia-project-sessions', or the
global default when no project matches."
  :type '(choice (const :tag "Resolve from project or default" nil)
                 (string :tag "Session name"))
  :local t)

(defcustom scholia-visible-sessions nil
  "Sessions drawn in a buffer besides the one it annotates into.
Visibility and target are separate: this list says what is drawn,
`scholia-session' says where a new annotation goes.  The target is drawn
whether or not it is listed here."
  :type '(repeat string))

(defcustom scholia-project-sessions nil
  "Alist mapping a project root to the session name used inside it."
  :type '(alist :key-type (directory :tag "Project root")
                :value-type (string :tag "Session name")))

(defun scholia--directory-name (directory)
  "Return DIRECTORY expanded and slash-terminated, or nil for a nil DIRECTORY.
Project roots and the keys they are looked up against are compared as
strings, so an assignment only ever matches when both sides are spelled
here."
  (and directory (file-name-as-directory (expand-file-name directory))))

(defun scholia-project-root ()
  "Return the root of the current project, or nil when there is none.
Asks projectile when it is loaded and project.el otherwise.  The root is
returned absolute and slash-terminated, so roots from either backend key
the same entry of `scholia-project-sessions'."
  (scholia--directory-name
   (or (and (fboundp 'projectile-project-root)
            (projectile-project-root))
       (let ((project (and (fboundp 'project-current)
                           (project-current))))
         (and project (project-root project))))))

(defcustom scholia-project-root-function #'scholia-project-root
  "Function returning the root of the current project, or nil.
Its value decides which entry of `scholia-project-sessions' applies."
  :type 'function)

(defcustom scholia-session-state-file
  (locate-user-emacs-file "scholia/project-sessions.eld")
  "File the state that outlives Emacs is kept in.
Holds the assignments made with `scholia-session-assign-project', and the
sessions shown when `scholia-persist-visibility' says so.  Point this at
your setup's durable state directory when `locate-user-emacs-file' lands
in a cache that is wiped.  Assignments written in configuration through
`scholia-project-sessions' need no file, and answer before the stored
ones."
  :type 'file)

(defcustom scholia-persist-visibility nil
  "Whether the sessions being shown outlive Emacs.
Nil makes visibility a matter of the sitting: a restart draws the target
alone and `scholia-session-show' is how the rest come back.  Non-nil
keeps the shown sessions and the global target in
`scholia-session-state-file' and reads them back the first time an
annotated buffer is drawn."
  :type 'boolean)

(defvar scholia--state-cache nil
  "Cons of the state file last read and the state it held.")

(defun scholia--read-state-file ()
  "Return the state stored in `scholia-session-state-file', or nil for none."
  (when (and scholia-session-state-file
             (file-readable-p scholia-session-state-file))
    (with-temp-buffer
      (insert-file-contents scholia-session-state-file)
      (condition-case nil
          (let ((stored (read (current-buffer))))
            (and (listp stored) stored))
        (error nil)))))

(defun scholia--state-normalize (stored)
  "Return STORED as a state plist, whatever shape it was written in.
A file holding a bare alist is one written before scholia kept anything
besides the project assignments in it, and answers as those assignments."
  (if (or (null stored) (keywordp (car stored)))
      stored
    (list :assignments stored)))

(defun scholia-stored-state ()
  "Return the state plist saved in `scholia-session-state-file'.
The file is read again whenever `scholia-session-state-file' names
another one, and the writer hands its result over as it stores it, so
resolving a session name costs no read."
  (unless (equal (car scholia--state-cache) scholia-session-state-file)
    (setq scholia--state-cache
          (cons scholia-session-state-file
                (scholia--state-normalize (scholia--read-state-file)))))
  (cdr scholia--state-cache))

(defun scholia-stored-assignments ()
  "Return the project assignments saved in `scholia-session-state-file'."
  (plist-get (scholia-stored-state) :assignments))

(defcustom scholia-autosave t
  "Whether scholia stores the annotations before it lets go of a buffer.
Turning command `scholia-mode' off reads it, and so does killing an
annotated buffer.  Nil leaves storing them to the caller, which then
owns whatever is on screen when the mode goes down or the buffer dies."
  :type 'boolean)


;;;; Export and sending

(defcustom scholia-export-format 'rustc
  "Format `scholia-export' produces."
  :type '(choice (const :tag "rustc diagnostics" rustc)
                 (const :tag "Unified diff" diff)
                 (const :tag "Annotations integrated into the text" integrate)))

(defcustom scholia-herdr-default-target nil
  "Herdr target sends go to without asking.
Nil prompts for a target instead."
  :type '(choice (const :tag "Ask" nil)
                 (string :tag "Target")))

(defcustom scholia-herdr-send-format nil
  "Format annotations are rendered in when sent to herdr.
Nil falls back to `scholia-export-format'."
  :type '(choice (const :tag "Follow `scholia-export-format'" nil)
                 (const :tag "rustc diagnostics" rustc)
                 (const :tag "Unified diff" diff)
                 (const :tag "Annotations integrated into the text" integrate)))


;;;; Appearance and behaviour

(defcustom scholia-annotation-editor 'minibuffer
  "Global input interface for `scholia-annotate' and `scholia-edit-annotation'.
`inline' opens a temporary field below the current document line, with
optional Corfu completion.  `minibuffer' uses `completing-read'."
  :type '(choice (const :tag "Minibuffer" minibuffer)
                 (const :tag "Inline field with optional Corfu" inline)))

(defcustom scholia-annotation-history-limit 200
  "How many past annotations are offered as recurring candidates."
  :type 'natnum)

(defcustom scholia-session-colors '("#F5DD8E" "#8EF5DD" "#DD8EF5"
                                    "#CFF58E" "#F5988E" "#8EADF5")
  "Colours sessions are drawn in, taken in order, one session each.
Every annotation of a session wears its colour, whichever of them it is.
A buffer showing more sessions than there are colours here turns the hue
wheel on for the rest rather than giving two sessions one colour."
  :type '(repeat color))

(defcustom scholia-reply-tint-step 0.35
  "How much of its saturation a reply loses against the note it answers.
Applied once per level of depth, so a reply to a reply fades twice."
  :type 'float)

(defcustom scholia-render-reply-indent 2
  "Columns a reply is set in past the note it answers."
  :type 'natnum)


(defcustom scholia-source-snapshot-mode 'bounded-full
  "Policy used to retain source text for later access.
`bounded-full' retains complete source that fits the configured limit.
`excerpts' retains only the line context already stored with annotations."
  :type '(choice (const bounded-full) (const excerpts)))

(defcustom scholia-source-snapshot-limit (* 256 1024)
  "Maximum number of bytes retained in a complete source snapshot."
  :type 'natnum)

(defcustom scholia-use-messages t
  "Whether scholia reports what it did in the echo area."
  :type 'boolean)

(defcustom scholia-show-revision-annotations nil
  "Whether working-tree buffers show annotations made against revisions."
  :type 'boolean)

(defcustom scholia-annotation-authors nil
  "Whether annotations record and show who wrote them.
When non-nil, a new annotation or reply stores its author, the git
`user.name' and `user.email' of the buffer's repository, and every
stored author is drawn in a pane of its own under what they wrote.
Nil records and draws none.  `scholia-toggle-annotation-authors' flips
it in every buffer."
  :type 'boolean)

(defcustom scholia-author-icon "nf-md-at"
  "Nerd Font glyph drawn in front of an author, or nil for none."
  :type '(choice (const :tag "None" nil) string))

(defcustom scholia-author-icon-fallback "@"
  "Character drawn in front of an author where the Nerd Font is missing."
  :type '(choice (const :tag "None" nil) string))

(defcustom scholia-annotation-column 85
  "Column where annotation text starts."
  :type 'natnum)

(defcustom scholia-search-region-lines-delta 2
  "How many lines around its stored position annotated text is searched.
Applies when a file changed while `scholia-mode' was off."
  :type 'natnum)


;;;; Errors

(define-error 'scholia-error "Scholia error")

(define-error 'scholia-db-format-error
              "Session file carries no scholia version tag"
              'scholia-error)


;;;; Buffer-local state

(defvar-local scholia--session-state nil
  "Alist of per-session rendering and preservation state.")

(defvar-local scholia--colors-index-counter 0
  "Always increasing index stored with each annotation this buffer makes.
Kept for the record an annotation round-trips through; a session's colour
is what draws it.")

(defvar-local scholia--replies nil
  "Alist mapping a chain id to the replies drawn under its note.
Each entry is a list of conses of a reply's depth and its annotation,
in thread order.")

(defvar-local scholia--unplaced-annotations nil
  "Stored annotations this buffer could not be shown holding.
Nothing on screen stands for them, so every save folds them back into
the record as they are rather than writing a record without them.")

(defvar-local scholia--hidden-revision-annotations nil
  "Revision annotations intentionally not drawn in this buffer.")


;;;; Annotations

(defvar scholia-session-color-index-function #'ignore
  "Function numbering a session this buffer holds no state for.
Set by `scholia-core' to the numbering the effective sessions give.
Without it a session no state names would take the number of the first,
and the two would be drawn in one colour.")

(defun scholia-session-color-index (session)
  "Return SESSION's index into `scholia-session-colors' in this buffer."
  (let ((entry (assoc-string session scholia--session-state)))
    (or (and entry (plist-get (cdr entry) :color-offset))
        (funcall scholia-session-color-index-function session)
        0)))

(defun scholia-annotation-p (overlay)
  "Return non-nil when OVERLAY is an annotation.
An overlay is an annotation exactly when it still lives in a buffer and
carries annotation text in its `scholia-annotation' property."
  (and (overlayp overlay)
       (buffer-live-p (overlay-buffer overlay))
       (stringp (overlay-get overlay 'scholia-annotation))))

(defmacro scholia-ensure-annotation (spec &rest body)
  "Evaluate BODY only when SPEC's overlay is an annotation.
SPEC is a one-element list holding a form evaluating to an overlay,
which is evaluated exactly once and tested with `scholia-annotation-p'."
  (declare (indent 1) (debug ((form) body)))
  `(when (scholia-annotation-p ,(car spec))
     ,@body))

(provide 'scholia-vars)
;;; scholia-vars.el ends here
