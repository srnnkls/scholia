;;; scholia-forge.el --- GitHub pull request threads  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Keywords: convenience, tools

;;; Commentary:

;; Shows the comments of a GitHub pull request as scholia threads in a
;; Magit diff buffer of it, coloured by author.  Comments written there
;; are drafts, kept in a session of the pull request's own until they are
;; pushed to GitHub in one batch.  GitHub is reached through the `gh'
;; command line tool; forge is only needed to open the pull request at
;; point.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'diff-mode)
(require 'magit-diff)
(require 'magit-section)
(require 'scholia-vars)
(require 'scholia-color)
(require 'scholia-db)
(require 'scholia-locate)
(require 'scholia-overlay)
(require 'scholia-render)
(require 'scholia-core)

(declare-function forge-current-pullreq "ext:forge-pullreq" (&optional demand))
(declare-function forge--pullreq-range "ext:forge-pullreq"
                  (pullreq &optional endpoints))
(declare-function forge-get-repository "ext:forge-repo" (&rest args))
(declare-function forge-visit-topic "ext:forge-commands" (topic))
(declare-function evil-make-intercept-map "ext:evil-core"
                  (keymap &optional state aux))
(declare-function evil-insert-state "ext:evil-states" (&optional arg))

(defvar forge-buffer-topic)


;;;; Customization

(defcustom scholia-forge-gh-program "gh"
  "The GitHub command line tool pull requests are read and written with."
  :type 'string
  :group 'scholia)

(defcustom scholia-forge-display-buffer-function nil
  "Function displaying a pull request's diff buffer, or nil for Magit's own.
It is bound as `magit-display-buffer-function' while the buffer is set
up, so it is called with the buffer and displays it."
  :type '(choice (const :tag "Magit's own" nil) function)
  :group 'scholia)

(defcustom scholia-forge-show-authors t
  "Whether a pull request's diff buffer draws who wrote each comment."
  :type 'boolean
  :group 'scholia)

(defcustom scholia-forge-reply-tint-step 0
  "How much of its saturation a reply loses per level in a pull request.
Replies wear their own author's colour, so they are not faded by default."
  :type 'float
  :group 'scholia)

(defcustom scholia-forge-author-colors nil
  "Alist of GitHub logins and the colours their comments are drawn in.
A login it does not name is given a colour of its own, the same in
every buffer and every Emacs."
  :type '(alist :key-type string :value-type color)
  :group 'scholia)

(defcustom scholia-forge-author-palette-size 12
  "How many colours the logins not in `scholia-forge-author-colors' share."
  :type 'natnum
  :group 'scholia)

(defcustom scholia-forge-author-saturation 0.4
  "Share of its saturation a palette colour keeps when an author wears it.
Authors are told apart by hue, so their colours can stay muted; 1.0
wears the palette as it is.  Colours in `scholia-forge-author-colors'
are worn as given."
  :type 'float
  :group 'scholia)

(defcustom scholia-forge-note-placement 'below
  "Where a pull request's diff buffer draws its comments.
It is `scholia-note-placement' for those buffers."
  :type '(choice (const beside) (const below))
  :group 'scholia)

(defcustom scholia-forge-note-lines 12
  "How many lines of a comment its note shows before it is cut short.
A note taller than the window cannot be scrolled through, so a long
comment shows its start, and `scholia-forge-show-thread' shows it all."
  :type 'natnum
  :group 'scholia)

(defcustom scholia-forge-note-replies 4
  "How many of a thread's replies its note shows, the latest ones.
The earlier ones are counted under the first comment and shown in full
by `scholia-forge-show-thread'."
  :type 'natnum
  :group 'scholia)

(defcustom scholia-forge-conversation-lines 4
  "How many lines of the description the conversation note shows.
The conversation's comments are counted there and shown in full by
`scholia-forge-show-thread'."
  :type 'natnum
  :group 'scholia)

(defcustom scholia-forge-expand-commented-files t
  "Whether the file sections holding comments are expanded to show them."
  :type 'boolean
  :group 'scholia)

(define-error 'scholia-forge-error "GitHub request failed" 'scholia-error)


;;;; State

(defvar-local scholia-forge--state nil
  "Plist of the pull request this diff buffer shows.
It holds where the pull request lives, the session its drafts are kept
in, and what GitHub answered for it.  It survives Magit setting the
buffer up again, which kills every other buffer-local variable.")
(put 'scholia-forge--state 'permanent-local t)

(defvar scholia-forge--viewers nil
  "Alist of GitHub hosts and the login `gh' is authenticated as there.")

(defun scholia-forge--get (key)
  "Return KEY of this buffer's pull request state."
  (plist-get scholia-forge--state key))

(defun scholia-forge--put (key value)
  "Set KEY of this buffer's pull request state to VALUE."
  (setq scholia-forge--state (plist-put scholia-forge--state key value)))

(defun scholia-forge--ensure ()
  "Signal unless this buffer shows a pull request that has been fetched."
  (unless (bound-and-true-p scholia-forge-mode)
    (user-error "Not a pull request diff buffer"))
  (unless (eq (scholia-forge--get :status) 'ready)
    (user-error "The pull request is still being fetched")))


;;;; Talking to GitHub

(defun scholia-forge--api-args (endpoint &rest options)
  "Return the `gh api' arguments reaching ENDPOINT with OPTIONS.
OPTIONS are :method, :paginate, :input and :host; a host other than
github.com is passed on as `--hostname'."
  (let ((host (plist-get options :host)))
    (append (list "api" endpoint)
            (when-let* ((method (plist-get options :method)))
              (list "--method" method))
            (when (plist-get options :paginate) (list "--paginate" "--slurp"))
            (when (plist-get options :input) (list "--input" "-"))
            (when (and host (not (equal host "github.com")))
              (list "--hostname" host)))))

(defun scholia-forge--parse (paginated)
  "Return the JSON in the current buffer, the pages joined when PAGINATED."
  (goto-char (point-min))
  (skip-chars-forward " \t\n")
  (unless (eobp)
    (let ((data (json-parse-buffer :object-type 'plist :array-type 'list
                                   :null-object nil :false-object nil)))
      (if paginated (apply #'append data) data))))

(defun scholia-forge--error-message (stderr host)
  "Return what went wrong according to STDERR, for requests to HOST."
  (cond
   ((string-match-p "auth login\\|HTTP 401" stderr)
    (format "gh is not authenticated for %s: run `gh auth login --hostname %s'"
            host host))
   ((string-match-p "HTTP 404" stderr) "Pull request not found, or not visible")
   (t (string-trim stderr))))

(defun scholia-forge--program ()
  "Return the `gh' executable, signalling when there is none."
  (or (executable-find scholia-forge-gh-program)
      (user-error "Cannot find `%s'; install the GitHub CLI"
                  scholia-forge-gh-program)))

(defun scholia-forge--call (args &optional input callback)
  "Run `gh' with ARGS and return what it prints, parsed as JSON.
INPUT is written to its standard input.  With CALLBACK the call runs in
the background and CALLBACK is called with an error message or nil, and
the parsed answer; without one it waits, and signals
`scholia-forge-error' when `gh' fails."
  (let ((program (scholia-forge--program))
        (paginated (member "--paginate" args))
        (host (or (cadr (member "--hostname" args)) "github.com")))
    (if callback
        (let* ((out (generate-new-buffer " *scholia-forge-gh*"))
               (err (generate-new-buffer " *scholia-forge-gh-err*"))
               (process
                (make-process
                 :name "scholia-forge-gh" :buffer out :stderr err
                 :command (cons program args) :noquery t
                 :connection-type 'pipe
                 :sentinel
                 (lambda (process _event)
                   (unless (process-live-p process)
                     (unwind-protect
                         (if (zerop (process-exit-status process))
                             (funcall callback nil
                                      (with-current-buffer out
                                        (scholia-forge--parse paginated)))
                           (when-let* ((stderr (get-buffer-process err)))
                             (accept-process-output stderr 0.1))
                           (funcall callback
                                    (scholia-forge--error-message
                                     (with-current-buffer err (buffer-string))
                                     host)
                                    nil))
                       (kill-buffer out)
                       (kill-buffer err)))))))
          (when input
            (process-send-string process input)
            (process-send-eof process))
          process)
      (let ((stderr (make-temp-file "scholia-forge-gh")))
        (unwind-protect
            (with-temp-buffer
              (let ((status (apply #'call-process-region (or input "") nil program
                                   nil (list t stderr) nil args)))
                (unless (eq status 0)
                  (signal 'scholia-forge-error
                          (list (scholia-forge--error-message
                                 (with-temp-buffer
                                   (insert-file-contents stderr)
                                   (buffer-string))
                                 host))))
                (scholia-forge--parse paginated)))
          (delete-file stderr))))))

(defun scholia-forge--endpoint (&rest parts)
  "Return the API endpoint of this buffer's pull request followed by PARTS."
  (mapconcat (lambda (part) (format "%s" part))
             (append (list "repos" (scholia-forge--get :owner)
                           (scholia-forge--get :repo))
                     parts)
             "/"))


;;;; What GitHub answered, as annotations

(defun scholia-forge--gh-id (id)
  "Return the annotation id of the GitHub comment ID."
  (format "gh:%s" id))

(defun scholia-forge--byline (login time)
  "Return the byline of LOGIN writing at the ISO 8601 TIME, or as a draft."
  (format "%s · %s" login (if time (substring time 0 10) "draft")))

(defun scholia-forge--text (body)
  "Return BODY as a note shows it."
  (string-trim (string-replace "\r" "" (or body ""))))

(defun scholia-forge--clip (text lines)
  "Return TEXT cut to its first LINES lines, saying how many more there are."
  (let ((all (split-string text "\n")))
    (if (<= (length all) lines)
        text
      (concat (string-join (seq-take all lines) "\n")
              (format "\n… %d more line%s: %s" (- (length all) lines)
                      (if (= (- (length all) lines) 1) "" "s")
                      (substitute-command-keys
                       "\\<scholia-forge-mode-map>\\[scholia-forge-show-thread]"))))))

(defun scholia-forge--clipped (annotation)
  "Return ANNOTATION with its text cut to `scholia-forge-note-lines'."
  (plist-put (copy-sequence annotation) :text
             (scholia-forge--clip (plist-get annotation :text)
                                  scholia-forge-note-lines)))

(defun scholia-forge--remote-annotation (comment)
  "Return the review COMMENT GitHub answered as an annotation."
  (let ((login (plist-get (plist-get comment :user) :login))
        (reply-to (plist-get comment :in_reply_to_id)))
    (list :id (scholia-forge--gh-id (plist-get comment :id))
          :text (scholia-forge--text (plist-get comment :body))
          :reply-to (and reply-to (scholia-forge--gh-id reply-to))
          :author (scholia-forge--byline login (plist-get comment :created_at))
          :login login
          :created (plist-get comment :created_at)
          :forge comment)))

(defun scholia-forge--draft-p (annotation)
  "Return non-nil when ANNOTATION is a draft not yet on GitHub."
  (not (string-prefix-p "gh:" (plist-get annotation :id))))

(defun scholia-forge--draft-kind (annotation)
  "Return what kind of draft ANNOTATION is."
  (plist-get (plist-get annotation :forge) :kind))

(defun scholia-forge--summary (pull)
  "Return the line saying how PULL stands: state, author, branches and so on."
  (string-join
   (delq nil
         (list (cond ((plist-get pull :merged_at) "merged")
                     ((plist-get pull :draft) (format "draft, %s" (plist-get pull :state)))
                     (t (plist-get pull :state)))
               (plist-get (plist-get pull :user) :login)
               (format "%s ← %s" (plist-get (plist-get pull :base) :ref)
                       (plist-get (plist-get pull :head) :ref))
               (when-let* ((labels (plist-get pull :labels)))
                 (mapconcat (lambda (label) (plist-get label :name)) labels ", "))
               (when-let* ((reviewers (append (plist-get pull :requested_reviewers)
                                              (plist-get pull :requested_teams))))
                 (concat "review: "
                         (mapconcat (lambda (reviewer)
                                      (or (plist-get reviewer :login)
                                          (plist-get reviewer :slug)))
                                    reviewers ", ")))))
   " · "))

(defun scholia-forge--conversation ()
  "Return the conversation of the pull request as a root and its replies.
The root is the pull request's description; issue comments, the bodies
of submitted reviews and conversation drafts answer it in the order
they were written, the drafts last."
  (let* ((pull (scholia-forge--get :pull))
         (author (plist-get (plist-get pull :user) :login))
         (root (list :id "gh:pr"
                     :text (let ((body (scholia-forge--text (plist-get pull :body))))
                             (concat (scholia-forge--summary pull) "\n"
                                     (if (string-empty-p body) "(no description)" body)))
                     :author (scholia-forge--byline author (plist-get pull :created_at))
                     :login author))
         (comments
          (mapcar (lambda (comment)
                    (let ((login (plist-get (plist-get comment :user) :login)))
                      (list :id (scholia-forge--gh-id (plist-get comment :id))
                            :text (scholia-forge--text (plist-get comment :body))
                            :author (scholia-forge--byline
                                     login (plist-get comment :created_at))
                            :login login
                            :created (plist-get comment :created_at))))
                  (scholia-forge--get :issue-comments)))
         (reviews
          (delq nil
                (mapcar (lambda (review)
                          (let ((body (scholia-forge--text (plist-get review :body)))
                                (login (plist-get (plist-get review :user) :login)))
                            (unless (string-empty-p body)
                              (list :id (scholia-forge--gh-id (plist-get review :id))
                                    :text (format "[%s] %s"
                                                  (plist-get review :state) body)
                                    :author (scholia-forge--byline
                                             login (plist-get review :submitted_at))
                                    :login login
                                    :created (plist-get review :submitted_at)))))
                        (scholia-forge--get :reviews))))
         (drafts (seq-filter (lambda (draft)
                               (eq (scholia-forge--draft-kind draft) 'conversation))
                             (scholia-forge--drafts))))
    (cons root
          (mapcar (lambda (reply) (cons 1 reply))
                  (append (sort (append comments reviews)
                                (lambda (a b)
                                  (string< (plist-get a :created)
                                           (plist-get b :created))))
                          drafts)))))

(defun scholia-forge--replies (root annotations)
  "Return the replies among ANNOTATIONS to ROOT, in the order written.
A review thread on GitHub is flat: every reply answers its first comment."
  (let ((id (plist-get root :id)))
    (mapcar (lambda (reply) (cons 1 reply))
            (seq-filter (lambda (annotation)
                          (equal (plist-get annotation :reply-to) id))
                        annotations))))


;;;; Drafts

(defun scholia-forge--session-name (host owner repo number)
  "Return the session the drafts on HOST's OWNER/REPO pull NUMBER are kept in."
  (replace-regexp-in-string
   "[^A-Za-z0-9._-]" "-"
   (format "%s%s-%s-pr-%d"
           (if (equal host "github.com") "" (concat host "-"))
           owner repo number)))

(defun scholia-forge--record-key ()
  "Return the key of the record this buffer's drafts are kept under."
  (format "https://%s/%s/%s/pull/%d"
          (scholia-forge--get :host) (scholia-forge--get :owner)
          (scholia-forge--get :repo) (scholia-forge--get :number)))

(defun scholia-forge--session-file ()
  "Return the file of the session this buffer's drafts are kept in."
  (scholia-session-file (scholia-forge--get :session)))

(defun scholia-forge--drafts ()
  "Return the drafts of this buffer's pull request, in the order written."
  (scholia-db-record-annotations
   (scholia-db-record (scholia-forge--session-file) (scholia-forge--record-key))))

(defun scholia-forge--draft (forge text &optional reply-to &rest fields)
  "Return a draft of TEXT that FORGE says how to push, answering REPLY-TO.
FIELDS are further keys and values the draft carries."
  (let ((viewer (scholia-forge--get :viewer)))
    (apply #'scholia-db--with-fields
           (scholia-db-make-annotation (scholia-core--make-id) text
                                       nil nil nil nil nil reply-to)
           :author (scholia-forge--byline viewer nil)
           :login viewer
           :forge forge
           fields)))

(defun scholia-forge--draft-add (draft)
  "Keep DRAFT with the drafts of this buffer's pull request."
  (scholia-db-add-reply (scholia-forge--session-file)
                        (scholia-forge--record-key) draft))

(defun scholia-forge--draft-update (id text)
  "Make TEXT what the draft ID says."
  (let* ((file (scholia-forge--session-file))
         (key (scholia-forge--record-key))
         (record (scholia-db-record file key)))
    (scholia-db-store-record
     file
     (scholia-db-make-record
      key
      (mapcar (lambda (draft)
                (if (equal (plist-get draft :id) id)
                    (plist-put (copy-sequence draft) :text text)
                  draft))
              (scholia-db-record-annotations record))
      (scholia-db-record-checksum record)))))

(defun scholia-forge--draft-remove (id)
  "Remove the draft ID, and the drafts answering it."
  (let ((file (scholia-forge--session-file))
        (key (scholia-forge--record-key)))
    (dolist (draft (scholia-forge--drafts))
      (when (equal (plist-get draft :reply-to) id)
        (scholia-db-remove-annotation file key (plist-get draft :id))))
    (scholia-db-remove-annotation file key id)))


;;;; Where a comment goes in the diff

(defun scholia-forge--file-section (path)
  "Return the section of the diff showing PATH, under its old name or new."
  (seq-find (lambda (section)
              (and (cl-typep section 'magit-file-section)
                   (or (equal (oref section value) path)
                       (equal (oref section source) path))))
            (oref magit-root-section children)))

(defun scholia-forge--line-position (path side line)
  "Return where LINE of PATH's SIDE starts in the diff, or nil.
SIDE is \"LEFT\" for the file before the change and \"RIGHT\" for after,
as GitHub names them; lines only the other side has are passed over."
  (when-let* ((file (scholia-forge--file-section path)))
    (let* ((left (equal side "LEFT"))
           (skip (if left ?+ ?-)))
      (seq-some
       (lambda (hunk)
         (when-let* (((cl-typep hunk 'magit-hunk-section))
                     ((not (oref hunk combined)))
                     (range (if left (oref hunk from-range) (oref hunk to-range)))
                     ((> (cadr range) 0))
                     ((<= (car range) line (+ (car range) (cadr range) -1))))
           (save-excursion
             (goto-char (oref hunk content))
             (let ((current (car range))
                   (end (oref hunk end)))
               (while (and (< (point) end)
                           (or (memq (char-after) (list skip ?\\))
                               (< current line)))
                 (unless (memq (char-after) (list skip ?\\))
                   (cl-incf current))
                 (forward-line 1))
               (and (< (point) end) (point))))))
       (oref file children)))))

(defun scholia-forge--range (path side line &optional start-line start-side)
  "Return the range a comment on PATH's SIDE from START-LINE to LINE covers.
It runs from the start of its first line, the prefix included so that an
empty line has something to cover, to the end of its last, and is nil
when LINE is not in the diff."
  (when-let* ((last (scholia-forge--line-position path side line)))
    (let ((first (or (and start-line
                          (scholia-forge--line-position
                           path (or start-side side) start-line))
                     last)))
      (cons (min first last)
            (save-excursion (goto-char last) (line-end-position))))))

(defun scholia-forge--heading (path)
  "Return the range of the heading of PATH's section, or nil."
  (when-let* ((file (scholia-forge--file-section path)))
    (save-excursion
      (goto-char (oref file start))
      (cons (point) (line-end-position)))))

(defun scholia-forge--first-line ()
  "Return the range of the diff buffer's first line."
  (save-excursion
    (goto-char (point-min))
    (cons (point) (line-end-position))))

(defun scholia-forge--location (pos)
  "Return where the diff line at POS is, as GitHub names places.
The answer carries the :path, :side and :line of the line, and the
:hunk it is in."
  (save-excursion
    (goto-char pos)
    (let ((section (magit-current-section)))
      (unless (and (cl-typep section 'magit-hunk-section)
                   (not (oref section combined))
                   (>= (line-beginning-position) (oref section content)))
        (user-error "Not on a line of the diff"))
      (let* ((removed (eq (char-after (line-beginning-position)) ?-))
             (file (oref section parent)))
        (list :path (if removed
                        (or (oref file source) (oref file value))
                      (oref file value))
              :side (if removed "LEFT" "RIGHT")
              :line (magit-diff-hunk-line section removed)
              :hunk section)))))

(defun scholia-forge--region-location ()
  "Return where a comment on the line at point, or the region, goes.
A region has to stay inside one hunk, as a comment on GitHub does."
  (if (use-region-p)
      (let* ((beg (region-beginning))
             (end (save-excursion
                    (goto-char (region-end))
                    (if (and (bolp) (> (point) beg)) (1- (point)) (point))))
             (first (scholia-forge--location beg))
             (last (scholia-forge--location end)))
        (unless (eq (plist-get first :hunk) (plist-get last :hunk))
          (user-error "A comment cannot span more than one hunk"))
        (if (equal (plist-get first :line) (plist-get last :line))
            last
          (append last (list :start-line (plist-get first :line)
                             :start-side (plist-get first :side)))))
    (scholia-forge--location (point))))


;;;; Colours

(defun scholia-forge-author-color (login)
  "Return the colour the comments of LOGIN are drawn in."
  (or (cdr (assoc login scholia-forge-author-colors))
      (scholia-forge--muted
       (scholia-color-for-index
        (mod (string-to-number (substring (md5 (or login "")) 0 6) 16)
             (max 1 scholia-forge-author-palette-size))))))

(defun scholia-forge--muted (color)
  "Return COLOR keeping `scholia-forge-author-saturation' of its saturation."
  (if-let* ((hsl (scholia-color--hsl color)))
      (pcase-let ((`(,hue ,saturation ,lightness) hsl))
        (apply #'color-rgb-to-hex
               (append (color-hsl-to-rgb hue
                                         (* saturation scholia-forge-author-saturation)
                                         lightness)
                       '(2))))
    color))

(defun scholia-forge--color (_owner chain-id reply)
  "Return the colour of REPLY, or of the root of CHAIN-ID, by its author.
A note not yet made is the viewer's own."
  (scholia-forge-author-color
   (or (plist-get reply :login)
       (and chain-id (plist-get (get chain-id 'scholia-forge-root) :login))
       (scholia-forge--get :viewer))))


;;;; Drawing

(defun scholia-forge--place (range root replies kind &optional threads)
  "Draw ROOT over RANGE with REPLIES under it, as a thread of KIND.
KIND is `remote', `draft', `conversation' or `outdated'.  THREADS are
the threads the note stands for, shown in full by
`scholia-forge-show-thread'; they default to ROOT and REPLIES, which the
note shows cut to `scholia-forge-note-lines' each."
  (when-let* ((hidden (max 0 (- (length replies) scholia-forge-note-replies)))
              (chain (scholia-create-chain
                      (car range) (cdr range)
                      (concat (plist-get (scholia-forge--clipped root) :text)
                              (when (> hidden 0)
                                (format "\n… %d earlier repl%s: %s" hidden
                                        (if (= hidden 1) "y" "ies")
                                        (substitute-command-keys
                                         "\\<scholia-forge-mode-map>\\[scholia-forge-show-thread]"))))
                      nil "github")))
    (let ((id (overlay-get (car chain) 'scholia--chain-id)))
      (put id 'scholia-core--id (plist-get root :id))
      (put id 'scholia-author (plist-get root :author))
      (put id 'scholia-forge-root root)
      (put id 'scholia-forge-kind kind)
      (put id 'scholia-forge-threads (or threads (list (cons root replies))))
      (setf (alist-get id scholia--replies)
            (mapcar (lambda (entry)
                      (cons (car entry) (scholia-forge--clipped (cdr entry))))
                    (nthcdr hidden replies)))
      (scholia-refresh-chain-face chain)
      (scholia-render-chain chain)
      chain)))

(defun scholia-forge--undecorate ()
  "Take every thread off this diff buffer."
  (scholia-core--unwatch-point)
  (scholia-render-clear)
  (scholia-render-emphasis-clear)
  (remove-overlays (point-min) (point-max) 'cera t)
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (scholia-annotation-p overlay)
      (delete-overlay overlay)))
  (scholia-disarm-rechaining)
  (setq scholia--replies nil))

(defun scholia-forge--expand (paths)
  "Show the sections of PATHS, so their hunks are there to be found."
  (dolist (path (delete-dups (delq nil paths)))
    (when-let* ((file (scholia-forge--file-section path)))
      (when (oref file hidden)
        (magit-section-show file)))))

(defun scholia-forge--review-anchor (forge)
  "Return the range a review comment placed as FORGE says covers, or nil.
A comment on a whole file covers its heading; one whose line the diff no
longer has is outdated, and nil."
  (let ((path (plist-get forge :path)))
    (if (equal (plist-get forge :subject_type) "file")
        (scholia-forge--heading path)
      (when-let* ((line (plist-get forge :line)))
        (scholia-forge--range path (or (plist-get forge :side) "RIGHT") line
                              (plist-get forge :start_line)
                              (plist-get forge :start_side))))))

(defun scholia-forge--draft-anchor (draft)
  "Return the range review DRAFT covers, or nil when the diff lost its line."
  (let ((forge (plist-get draft :forge)))
    (scholia-forge--range (plist-get forge :path) (plist-get forge :side)
                          (plist-get forge :line) (plist-get forge :start-line)
                          (plist-get forge :start-side))))

(defun scholia-forge--decorate ()
  "Draw every thread of the pull request this diff buffer shows."
  (scholia-forge--undecorate)
  (when (eq (scholia-forge--get :status) 'ready)
    (setq-local scholia-annotation-authors scholia-forge-show-authors)
    (setq-local scholia-reply-tint-step scholia-forge-reply-tint-step)
    (setq-local scholia-render-color-function #'scholia-forge--color)
    (setq-local scholia-note-placement scholia-forge-note-placement)
    (let* ((remote (mapcar #'scholia-forge--remote-annotation
                           (scholia-forge--get :comments)))
           (drafts (scholia-forge--drafts))
           (annotations (append remote drafts))
           (reviews (seq-filter (lambda (draft)
                                  (eq (scholia-forge--draft-kind draft) 'review))
                                drafts))
           (outdated nil))
      (when scholia-forge-expand-commented-files
        (scholia-forge--expand
         (append (mapcar (lambda (annotation)
                           (plist-get (plist-get annotation :forge) :path))
                         annotations))))
      (let* ((conversation (scholia-forge--conversation))
             (root (car conversation))
             (count (length (cdr conversation))))
        (scholia-forge--place
         (scholia-forge--first-line)
         (plist-put (copy-sequence root) :text
                    (concat (scholia-forge--clip (plist-get root :text)
                                                 scholia-forge-conversation-lines)
                            (format "\n%d comment%s in the conversation: %s"
                                    count (if (= count 1) "" "s")
                                    (substitute-command-keys
                                     "\\<scholia-forge-mode-map>\\[scholia-forge-show-thread]"))))
         nil 'conversation (list conversation)))
      (dolist (root (seq-remove (lambda (annotation) (plist-get annotation :reply-to))
                                remote))
        (let ((replies (scholia-forge--replies root annotations))
              (forge (plist-get root :forge)))
          (if-let* ((range (scholia-forge--review-anchor forge)))
              (scholia-forge--place range root replies 'remote)
            (push (cons root replies)
                  (alist-get (plist-get forge :path) outdated nil nil #'equal)))))
      (pcase-dolist (`(,path . ,threads) outdated)
        (scholia-forge--place
         (or (scholia-forge--heading path) (scholia-forge--first-line))
         (list :id (format "outdated:%s" path)
               :text (format "⟲ %d outdated thread%s on %s: %s"
                             (length threads) (if (cdr threads) "s" "") path
                             (substitute-command-keys
                              "\\<scholia-forge-mode-map>\\[scholia-forge-show-thread]"))
               :login (scholia-forge--get :viewer))
         nil 'outdated (reverse threads)))
      (dolist (draft reviews)
        (scholia-forge--place
         (or (scholia-forge--draft-anchor draft) (scholia-forge--first-line))
         draft nil 'draft)))
    (scholia-disarm-rechaining)
    (scholia-core--watch-point)))

(defun scholia-forge--refresh-hook ()
  "Draw the threads again after Magit has drawn a pull request's diff anew."
  (when (and (bound-and-true-p scholia-forge-mode)
             (derived-mode-p 'magit-diff-mode))
    (scholia-forge--claim-topic)
    (scholia-forge--decorate)))

(defun scholia-forge--claim-topic ()
  "Tell forge which pull request this buffer shows, when forge opened it.
Setting the buffer up again kills `forge-buffer-topic', so it is set
again after every refresh from the state that survives."
  (when-let* ((topic (scholia-forge--get :topic)))
    (setq-local forge-buffer-topic topic)))


;;;; The mode

(defvar-keymap scholia-forge-mode-map
  :doc "Keymap of `scholia-forge-mode'."
  "<remap> <scholia-annotate>" #'scholia-forge-comment
  "<remap> <scholia-reply-to>" #'scholia-forge-reply
  "<remap> <scholia-edit-annotation>" #'scholia-forge-edit
  "<remap> <scholia-delete-annotation>" #'scholia-forge-delete
  "C-c C-a" #'scholia-forge-comment
  "C-c C-r" #'scholia-forge-reply
  "C-c C-c" #'scholia-forge-edit
  "C-c C-d" #'scholia-forge-delete
  "C-c C-g" #'scholia-forge-refetch
  "C-c C-p" #'scholia-forge-push
  "C-c C-o" #'scholia-forge-show-thread
  "C-c C-t" #'scholia-forge-visit-topic)

(defun scholia-forge--intercept (map)
  "Put MAP ahead of the keys evil gives Magit in normal state, when evil is here."
  (when (fboundp 'evil-make-intercept-map)
    (evil-make-intercept-map map 'normal)))

(defun scholia-forge--lighter ()
  "Return the mode line lighter saying how the pull request stands."
  (pcase (scholia-forge--get :status)
    ('fetching " PR…")
    ('error " PR!")
    (_ " PR")))

;;;###autoload
(define-minor-mode scholia-forge-mode
  "Show a GitHub pull request's comments in its Magit diff buffer.
Comments are drawn as scholia threads in their authors' colours.
Comments and replies written here are drafts, kept in a session of the
pull request's own until `scholia-forge-push' sends them to GitHub.

\\{scholia-forge-mode-map}"
  :lighter (:eval (scholia-forge--lighter))
  :keymap scholia-forge-mode-map
  (if scholia-forge-mode
      (progn
        (scholia-forge--intercept scholia-forge-mode-map)
        (add-hook 'magit-refresh-buffer-hook #'scholia-forge--refresh-hook))
    (scholia-forge--undecorate)))
(put 'scholia-forge-mode 'permanent-local t)

(defun scholia-forge-unload-function ()
  "Remove the global state scholia-forge installs."
  (remove-hook 'magit-refresh-buffer-hook #'scholia-forge--refresh-hook)
  nil)


;;;; Opening a pull request

;;;###autoload
(defun scholia-forge-open (host owner repo number range title)
  "Show HOST's OWNER/REPO pull request NUMBER, titled TITLE, with its comments.
RANGE is the revision range the diff shows."
  (let* ((name (format "*magit-diff: %s/%s #%d %s*" owner repo number title))
         (magit-generate-buffer-name-function (lambda (_mode _value) name))
         (magit-display-buffer-function (or scholia-forge-display-buffer-function
                                            magit-display-buffer-function)))
    (with-current-buffer (magit-diff-setup-buffer range nil nil nil 'committed t)
      (setq scholia-forge--state
            (list :host host :owner owner :repo repo :number number
                  :title title
                  :session (scholia-forge--session-name host owner repo number)
                  :viewer (alist-get host scholia-forge--viewers nil nil #'equal)))
      (scholia-forge-mode 1)
      (scholia-forge-refetch)
      (current-buffer))))

;;;###autoload
(defun scholia-forge-diff-pullreq ()
  "Show the pull request at point in a Magit diff buffer, with its comments."
  (interactive)
  (require 'forge)
  (let* ((pullreq (forge-current-pullreq t))
         (range (or (forge--pullreq-range pullreq t)
                    (user-error "PR head ref not fetched; run forge-pull")))
         (repository (forge-get-repository pullreq)))
    (with-current-buffer
        (scholia-forge-open (scholia-forge--slot repository 'githost)
                            (scholia-forge--slot repository 'owner)
                            (scholia-forge--slot repository 'name)
                            (scholia-forge--slot pullreq 'number)
                            range
                            (scholia-forge--slot pullreq 'title))
      (scholia-forge--put :topic pullreq)
      (scholia-forge--claim-topic))))

(defun scholia-forge-visit-topic ()
  "Show forge's own buffer of the pull request this diff shows."
  (interactive)
  (forge-visit-topic (or (scholia-forge--get :topic)
                         (user-error "This diff was not opened through forge"))))

(defun scholia-forge--slot (object slot)
  "Return SLOT of the forge OBJECT.
Forge is loaded when the command runs rather than when this file is
compiled, so its classes are unknown to the compiler."
  (slot-value object slot))

(defun scholia-forge-refetch ()
  "Fetch the pull request's comments from GitHub again and draw them."
  (interactive)
  (unless (bound-and-true-p scholia-forge-mode)
    (user-error "Not a pull request diff buffer"))
  (let* ((buffer (current-buffer))
         (host (scholia-forge--get :host))
         (requests
          `((:pull ,(scholia-forge--endpoint "pulls" (scholia-forge--get :number)) nil)
            (:comments ,(scholia-forge--endpoint "pulls" (scholia-forge--get :number)
                                                 "comments")
                       t)
            (:issue-comments ,(scholia-forge--endpoint "issues"
                                                       (scholia-forge--get :number)
                                                       "comments")
                             t)
            (:reviews ,(scholia-forge--endpoint "pulls" (scholia-forge--get :number)
                                                "reviews")
                      t)
            ,@(unless (scholia-forge--get :viewer) '((:viewer "user" nil)))))
         (pending (length requests))
         (failure nil))
    (scholia-forge--put :status 'fetching)
    (force-mode-line-update)
    (dolist (request requests)
      (scholia-forge--call
       (scholia-forge--api-args (nth 1 request) :paginate (nth 2 request) :host host)
       nil
       (lambda (error data)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (if error
                 (setq failure error)
               (if (eq (car request) :viewer)
                   (let ((login (plist-get data :login)))
                     (setf (alist-get host scholia-forge--viewers nil nil #'equal)
                           login)
                     (scholia-forge--put :viewer login))
                 (scholia-forge--put (car request) data)))
             (when (zerop (cl-decf pending))
               (if failure
                   (progn (scholia-forge--put :status 'error)
                          (message "scholia-forge: %s" failure))
                 (scholia-forge--put :status 'ready)
                 (scholia-forge--warn-stale)
                 (scholia-forge--decorate))
               (force-mode-line-update)))))))))

(defun scholia-forge--warn-stale ()
  "Say so when the fetched pull request ref lags behind GitHub's head."
  (let ((head (plist-get (plist-get (scholia-forge--get :pull) :head) :sha))
        (local (magit-rev-parse
                (format "refs/pullreqs/%d" (scholia-forge--get :number)))))
    (when (and head local (not (equal head local)))
      (message "The pull request ref is stale; run forge-pull, or comments may sit on the wrong lines"))))


;;;; Picking a thread

(defun scholia-forge--chain-at ()
  "Return the thread at point, choosing when there are several."
  (or (scholia-core--select-chain)
      (user-error "No comment here")))

(defun scholia-forge--chain-bounds (chain)
  "Return the range CHAIN covers."
  (cons (overlay-start (car chain)) (overlay-end (car (last chain)))))

(defun scholia-forge--chain-kind (chain)
  "Return the kind of thread CHAIN draws."
  (get (overlay-get (car chain) 'scholia--chain-id) 'scholia-forge-kind))

(defun scholia-forge--chain-root (chain)
  "Return the comment CHAIN draws first."
  (get (overlay-get (car chain) 'scholia--chain-id) 'scholia-forge-root))

(defun scholia-forge--first-line-of (text)
  "Return the first line of TEXT, for choosing among comments."
  (car (split-string text "\n")))

(defun scholia-forge--choose (prompt annotations)
  "Return one of ANNOTATIONS, asking with PROMPT when there is a choice."
  (if (cdr annotations)
      (let* ((candidates
              (seq-map-indexed
               (lambda (annotation index)
                 (cons (format "%d. %s — %s" (1+ index)
                               (plist-get annotation :author)
                               (scholia-forge--first-line-of
                                (plist-get annotation :text)))
                       annotation))
               annotations))
             (choice (completing-read prompt candidates nil t)))
        (cdr (assoc choice candidates)))
    (car annotations)))

(defun scholia-forge--read (prompt &optional initial bounds)
  "Read the text of a comment, starting from INITIAL.
On a graphic display it is written in a cera field in a child frame
under BOUNDS, the lines it is about, which leaves the diff's own text
alone; elsewhere it is read in the minibuffer with PROMPT."
  (let ((text (string-trim
               (if (and (display-graphic-p) (not noninteractive)
                        (require 'cera-frame nil t))
                   (let ((cera-input-backend 'frame)
                         (cera-input-prefix
                          (scholia-edit--icon
                           (scholia-forge-author-color (scholia-forge--get :viewer)))))
                     (cera-read nil initial
                                (or bounds
                                    (cons (line-beginning-position) (line-end-position)))
                                nil))
                 (read-string prompt initial)))))
    (when (string-empty-p text)
      (user-error "A comment needs text"))
    text))


;;;; Commands

(defun scholia-forge-comment (&optional text)
  "Draft a comment with TEXT on the diff line at point, or on the region."
  (interactive)
  (scholia-forge--ensure)
  (let* ((location (scholia-forge--region-location))
         (text (or text (scholia-forge--read
                         "Comment: " nil
                         (and (use-region-p)
                              (cons (region-beginning) (region-end))))))
         (pull (scholia-forge--get :pull))
         (head (plist-get (plist-get pull :head) :sha))
         (side (plist-get location :side))
         (revision (if (equal side "LEFT")
                       (plist-get (plist-get pull :base) :sha)
                     head)))
    (scholia-forge--draft-add
     (scholia-forge--draft
      (list :kind 'review
            :path (plist-get location :path)
            :side side
            :line (plist-get location :line)
            :start-line (plist-get location :start-line)
            :start-side (plist-get location :start-side)
            :commit head)
      text nil
      :line (plist-get location :line)
      :revision revision
      :access (scholia-locate-make-git-access
               (expand-file-name (plist-get location :path) (magit-toplevel))
               revision)))
    (deactivate-mark)
    (scholia-forge--decorate)))

(defun scholia-forge-reply (&optional text)
  "Draft a reply with TEXT to the thread at point."
  (interactive)
  (scholia-forge--ensure)
  (let* ((chain (scholia-forge--chain-at))
         (root (scholia-forge--chain-root chain)))
    (pcase (scholia-forge--chain-kind chain)
      ('draft (user-error "Edit the draft instead; it is not on GitHub yet"))
      ('remote
       (scholia-forge--reply-to-review root text (scholia-forge--chain-bounds chain)))
      ('outdated
       (scholia-forge--reply-to-review
        (car (scholia-forge--choose
              "Reply to: "
              (mapcar #'car (get (overlay-get (car chain) 'scholia--chain-id)
                                 'scholia-forge-threads))))
        text (scholia-forge--chain-bounds chain)))
      ('conversation
       (let* ((thread (car (get (overlay-get (car chain) 'scholia--chain-id)
                                'scholia-forge-threads)))
              (quoted (scholia-forge--choose
                       "Reply to: "
                       (cons (car thread)
                             (seq-remove #'scholia-forge--draft-p
                                         (mapcar #'cdr (cdr thread))))))
              (text (or text (scholia-forge--read
                              "Reply: " nil (scholia-forge--chain-bounds chain)))))
         (scholia-forge--draft-add
          (scholia-forge--draft
           (if (eq quoted (car thread))
               (list :kind 'conversation)
             (list :kind 'conversation :quote (plist-get quoted :text)))
           text "gh:pr")))))
    (scholia-forge--decorate)))

(defun scholia-forge--reply-to-review (root text &optional bounds)
  "Draft TEXT as a reply to the review thread ROOT starts, drawn over BOUNDS."
  (let ((text (or text (scholia-forge--read "Reply: " nil bounds))))
    (scholia-forge--draft-add
     (scholia-forge--draft
      (list :kind 'reply
            :in-reply-to (plist-get (plist-get root :forge) :id))
      text (plist-get root :id)))))

(defun scholia-forge--drafts-at ()
  "Return the drafts in the threads at point."
  (seq-filter
   #'scholia-forge--draft-p
   (seq-mapcat (lambda (chain)
                 (seq-mapcat (lambda (thread)
                               (cons (car thread) (mapcar #'cdr (cdr thread))))
                             (get (overlay-get (car chain) 'scholia--chain-id)
                                  'scholia-forge-threads)))
               (scholia-chains-at))))

(defun scholia-forge--draft-at (verb)
  "Return the draft at point to VERB, choosing when there are several."
  (or (scholia-forge--choose (format "%s draft: " verb) (scholia-forge--drafts-at))
      (user-error "No draft here; comments already on GitHub cannot be changed here")))

(defun scholia-forge-edit ()
  "Change what the draft at point says."
  (interactive)
  (scholia-forge--ensure)
  (let ((draft (scholia-forge--draft-at "Edit")))
    (scholia-forge--draft-update
     (plist-get draft :id)
     (scholia-forge--read "Comment: " (plist-get draft :text)
                          (scholia-forge--chain-bounds (car (scholia-chains-at)))))
    (scholia-forge--decorate)))

(defun scholia-forge-delete ()
  "Delete the draft at point, and the drafts answering it."
  (interactive)
  (scholia-forge--ensure)
  (let ((draft (scholia-forge--draft-at "Delete")))
    (when (y-or-n-p (format "Delete draft \"%s\"? "
                            (scholia-forge--first-line-of (plist-get draft :text))))
      (scholia-forge--draft-remove (plist-get draft :id))
      (scholia-forge--decorate))))


;;;; Pushing drafts

(defun scholia-forge--review-payload (drafts commit event body)
  "Return the review submitting DRAFTS against COMMIT as EVENT with BODY."
  (list :commit_id commit
        :event (or event "COMMENT")
        :body (or body "")
        :comments
        (vconcat
         (mapcar (lambda (draft)
                   (let ((forge (plist-get draft :forge)))
                     (append (list :path (plist-get forge :path)
                                   :line (plist-get forge :line)
                                   :side (plist-get forge :side)
                                   :body (plist-get draft :text))
                             (when-let* ((start (plist-get forge :start-line)))
                               (list :start_line start
                                     :start_side (plist-get forge :start-side))))))
                 drafts))))

(defun scholia-forge--reply-requests (drafts)
  "Return the requests posting the reply DRAFTS, as endpoints and payloads."
  (mapcar (lambda (draft)
            (cons (scholia-forge--endpoint
                   "pulls" (scholia-forge--get :number) "comments"
                   (plist-get (plist-get draft :forge) :in-reply-to) "replies")
                  (list :body (plist-get draft :text))))
          drafts))

(defun scholia-forge--conversation-requests (drafts)
  "Return the requests posting the conversation DRAFTS."
  (mapcar (lambda (draft)
            (cons (scholia-forge--endpoint
                   "issues" (scholia-forge--get :number) "comments")
                  (list :body
                        (concat
                         (when-let* ((quote (plist-get (plist-get draft :forge) :quote)))
                           (concat (mapconcat (lambda (line) (concat "> " line))
                                              (split-string quote "\n")
                                              "\n")
                                   "\n\n"))
                         (plist-get draft :text)))))
          drafts))

(defun scholia-forge--post (endpoint payload)
  "Post PAYLOAD to ENDPOINT, signalling when GitHub refuses it."
  (scholia-forge--call
   (scholia-forge--api-args endpoint :method "POST" :input t
                            :host (scholia-forge--get :host))
   (json-serialize payload)))

(defun scholia-forge--submit (event body)
  "Send every draft of this pull request to GitHub, then fetch it again.
Comments on lines go in one review with BODY as its summary, submitted
as EVENT; replies and conversation comments follow one by one.  A draft
is dropped once GitHub has it, so what fails is still there to send."
  (let* ((drafts (scholia-forge--drafts))
         (by-kind (lambda (kind)
                    (seq-filter (lambda (draft)
                                  (eq (scholia-forge--draft-kind draft) kind))
                                drafts)))
         (reviews (funcall by-kind 'review))
         (replies (funcall by-kind 'reply))
         (conversation (funcall by-kind 'conversation))
         (head (plist-get (plist-get (scholia-forge--get :pull) :head) :sha)))
    (when (and (null drafts) (string-empty-p body) (equal event "COMMENT"))
      (user-error "Nothing to submit"))
    (when (or reviews (not (string-empty-p body)) (not (equal event "COMMENT")))
      (scholia-forge--post
       (scholia-forge--endpoint "pulls" (scholia-forge--get :number) "reviews")
       (scholia-forge--review-payload reviews head event body))
      (dolist (draft reviews)
        (scholia-forge--draft-remove (plist-get draft :id))))
    (cl-loop for draft in (append replies conversation)
             for request in (append (scholia-forge--reply-requests replies)
                                    (scholia-forge--conversation-requests
                                     conversation))
             do (scholia-forge--post (car request) (cdr request))
             (scholia-forge--draft-remove (plist-get draft :id)))
    (message "Submitted to %s/%s#%d" (scholia-forge--get :owner)
             (scholia-forge--get :repo) (scholia-forge--get :number))
    (scholia-forge-refetch)))


;;;; Submitting a review

(defvar-local scholia-forge--review-origin nil
  "The pull request diff buffer this review is written for.")

(defvar-local scholia-forge--review-end nil
  "Marker where the summary ends and the overview of the drafts begins.")

(defvar-keymap scholia-forge-review-mode-map
  :doc "Keymap of `scholia-forge-review-mode'."
  "C-c C-c" #'scholia-forge-review-submit
  "C-c C-k" #'scholia-forge-review-cancel)

(define-derived-mode scholia-forge-review-mode text-mode "Review"
  "Mode of the buffer a pull request review's summary is written in.
Submitting it sends the summary with every draft; cancelling leaves the
drafts as they are.

\\{scholia-forge-review-mode-map}"
  (scholia-forge--intercept scholia-forge-review-mode-map))

(defun scholia-forge--overview (drafts)
  "Return the overview of DRAFTS shown under a review's summary."
  (concat
   (substitute-command-keys
    "\n\\<scholia-forge-review-mode-map>\
Write the review's summary above; it may stay empty.
\\[scholia-forge-review-submit] submits it with the drafts below, \\[scholia-forge-review-cancel] leaves them.\n")
   (if drafts
       (mapconcat
        (lambda (draft)
          (let ((forge (plist-get draft :forge)))
            (format "\n- %s\n    %s"
                    (pcase (plist-get forge :kind)
                      ('review (format "comment on %s:%s"
                                       (plist-get forge :path)
                                       (plist-get forge :line)))
                      ('reply "reply")
                      (_ "conversation comment"))
                    (string-join (split-string (plist-get draft :text) "\n")
                                 "\n    "))))
        drafts "\n")
     "\nNo drafts: the summary goes alone.")
   "\n"))

(defun scholia-forge-push ()
  "Write a review of this pull request and submit it with every draft.
The summary is written in a buffer of its own, over an overview of the
drafts that go with it."
  (interactive)
  (scholia-forge--ensure)
  (let ((origin (current-buffer))
        (drafts (scholia-forge--drafts))
        (buffer (get-buffer-create
                 (format "*scholia-forge-review: %s/%s #%d*"
                         (scholia-forge--get :owner) (scholia-forge--get :repo)
                         (scholia-forge--get :number)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (scholia-forge-review-mode)
        (setq scholia-forge--review-origin origin)
        (insert "\n")
        (setq scholia-forge--review-end (copy-marker (1- (point)) t))
        (insert (propertize (scholia-forge--overview drafts)
                            'read-only t 'face 'shadow
                            'front-sticky '(read-only)))
        (goto-char (point-min))))
    (pop-to-buffer buffer)
    (when (fboundp 'evil-insert-state)
      (evil-insert-state))))

(defun scholia-forge-review-submit (event)
  "Submit the review written here as EVENT, with every draft.
EVENT is read with completion, a plain comment unless chosen otherwise."
  (interactive
   (list (completing-read "Submit review as (default COMMENT): "
                          '("COMMENT" "APPROVE" "REQUEST_CHANGES")
                          nil t nil nil "COMMENT")))
  (let ((body (string-trim (buffer-substring-no-properties
                            (point-min) scholia-forge--review-end)))
        (origin scholia-forge--review-origin))
    (unless (buffer-live-p origin)
      (user-error "The pull request's diff buffer is gone"))
    (with-current-buffer origin
      (scholia-forge--submit event body))
    (quit-window t)))

(defun scholia-forge-review-cancel ()
  "Leave the review unsubmitted; the drafts stay as they are."
  (interactive)
  (quit-window t))


;;;; A thread in full

(define-derived-mode scholia-forge-thread-mode special-mode "Scholia-Thread"
  "Mode of the buffer showing pull request threads in full.")

(defun scholia-forge--insert-comment (annotation depth)
  "Insert ANNOTATION set in by DEPTH, under its byline in its author's colour."
  (let ((indent (make-string (* 2 depth) ?\s))
        (color (scholia-forge-author-color (plist-get annotation :login))))
    (insert indent
            (propertize (or (plist-get annotation :author) "")
                        'face (list :foreground color :weight 'bold))
            "\n")
    (dolist (line (split-string (plist-get annotation :text) "\n"))
      (insert indent line "\n"))
    (insert "\n")))

(defun scholia-forge--insert-thread (thread)
  "Insert THREAD, a root and its replies, under the hunk it was made on."
  (let* ((root (car thread))
         (forge (plist-get root :forge)))
    (if (not (plist-get forge :path))
        (insert (propertize "Conversation\n\n" 'face 'magit-section-heading))
      (scholia-forge--insert-hunk forge))
    (scholia-forge--insert-comment root 0)
    (pcase-dolist (`(,depth . ,reply) (cdr thread))
      (scholia-forge--insert-comment reply depth))))

(defun scholia-forge--insert-hunk (forge)
  "Insert where the review comment FORGE was made, and the hunk it was made on."
  (let ((commit (or (plist-get forge :original_commit_id) "unknown")))
    (insert (propertize (format "%s:%s at %s\n"
                                (plist-get forge :path)
                                (or (plist-get forge :original_line) "?")
                                (substring commit 0 (min 7 (length commit))))
                        'face 'magit-section-heading)))
  (dolist (line (split-string (or (plist-get forge :diff_hunk) "") "\n"))
    (insert (propertize line 'face (pcase (and (> (length line) 0) (aref line 0))
                                     (?+ 'diff-added)
                                     (?- 'diff-removed)
                                     (?@ 'diff-hunk-header)
                                     (_ 'diff-context)))
            "\n"))
  (insert "\n"))

(defun scholia-forge-show-thread ()
  "Show the threads the note at point stands for in full."
  (interactive)
  (scholia-forge--ensure)
  (let* ((chain (scholia-forge--chain-at))
         (id (overlay-get (car chain) 'scholia--chain-id))
         (threads (get id 'scholia-forge-threads))
         (buffer (get-buffer-create
                  (format "*scholia-forge: %s/%s #%d*" (scholia-forge--get :owner)
                          (scholia-forge--get :repo) (scholia-forge--get :number)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (scholia-forge-thread-mode)
        (dolist (thread threads)
          (scholia-forge--insert-thread thread))
        (goto-char (point-min))))
    (display-buffer buffer)))

(provide 'scholia-forge)
;;; scholia-forge.el ends here
