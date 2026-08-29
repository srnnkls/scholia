;;; scholia-search-test.el --- Tests for cross-session search  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Covers `scholia-search', the cross-session walk and the command that
;; jumps to what the walk finds.  The walk is the reusable half: it visits
;; every session in `scholia-session-directory' and hands each annotation
;; back together with the session, file and record it was read from, which
;; is what the recurring-annotation prompt, the send-history search and the
;; dashboard all consume.  The command keys completion candidates on all
;; four searchable fields and, on selection, switches session, opens the
;; file and puts point on the annotation.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'scholia-test-helper)

(let ((load-prefer-newer t))
  (require 'scholia-vars nil t)
  (require 'scholia-overlay nil t)
  (require 'scholia-db nil t)
  (require 'scholia-store nil t)
  (require 'scholia-core nil t)
  (require 'scholia-session nil t)
  (with-demoted-errors "scholia-search unloadable: %S"
    (require 'scholia-search nil t)))

(defun scholia-search-test--require ()
  "Load `scholia-search' inside the test rather than beside it.
The require at the top of the file demotes its errors on purpose.  A
NOERROR require covers a missing file and nothing else, so a module that
signals while loading takes the whole file down with it, and eask runs
the suite with `eask--ignore-error-p' bound, which swallows that and
reports green over tests that never registered.  Loading from inside the
fixture turns the same signal into a test that fails."
  (when (locate-library "scholia-search")
    (let ((load-prefer-newer t))
      (require 'scholia-search))))

(defmacro scholia-search-test--with-state (&rest body)
  "Evaluate BODY over a session directory and session globals of its own.
The globals are bound rather than written, so a session a test switches
into cannot leak into the next one."
  (declare (indent 0) (debug body))
  `(scholia-test-with-session-directory
     (scholia-search-test--require)
     (let ((scholia-session nil)
           (scholia-project-sessions nil)
           (scholia-project-root-function (lambda () nil))
           (scholia-autosave nil)
           (scholia-session-switch-hook nil)
           (scholia-session-state-file
            (expand-file-name "assignments.eld" scholia-session-directory)))
       ,@body)))

(defmacro scholia-search-test--choosing (needle &rest body)
  "Evaluate BODY with `completing-read' answering the candidate matching NEEDLE.
Nothing stands in for the completion machinery itself: the collection the
command offers is read back with `all-completions', so a command whose
candidates carry too little to tell one annotation from another offers no
match and BODY sees that as the failure it is."
  (declare (indent 1) (debug (form body)))
  `(cl-letf (((symbol-function 'completing-read)
              (lambda (_prompt collection &rest _)
                (or (seq-find (lambda (candidate)
                                (string-match-p ,needle candidate))
                              (all-completions "" collection))
                    (error "No candidate matches %s" ,needle)))))
     ,@body))

(defun scholia-search-test--source (name content)
  "Write CONTENT to NAME inside the session directory and return its path.
Fixture sources live beside the sessions that point at them so that
`scholia-test-with-session-directory' deletes both together."
  (let ((file (expand-file-name name scholia-session-directory)))
    (with-temp-file file (insert content))
    file))

(defun scholia-search-test--annotation (id text annotated-text beg end)
  "Return the stored annotation ID carrying TEXT over BEG to END.
ANNOTATED-TEXT is the buffer text the annotation covers."
  (list :id id
        :text text
        :beg beg
        :end end
        :annotated-text annotated-text
        :line 1
        :line-text annotated-text
        :column (1- beg)
        :end-column (1- end)
        :color 0
        :position :margin
        :reply-to nil
        :sends nil))

(defun scholia-search-test--reply (id text parent-id)
  "Return the stored reply ID carrying TEXT under PARENT-ID.
A reply holds no position of its own: it carries nil for `:beg' and
`:end', which is what leaves its parent's location as the only one it
has."
  (list :id id
        :text text
        :beg nil
        :end nil
        :annotated-text nil
        :line nil
        :line-text nil
        :column nil
        :end-column nil
        :color 0
        :position :margin
        :reply-to parent-id
        :sends nil))

(defun scholia-search-test--seed (session file annotations)
  "Store ANNOTATIONS as the record FILE keys in the session called SESSION."
  (let ((session-file (scholia-session-file session)))
    (scholia-db-create-session session-file)
    (scholia-db-store-record
     session-file (scholia-db-make-record file annotations "seeded"))))

(defun scholia-search-test--origin (entry)
  "Return ENTRY as the list of its session, its file and its annotation text."
  (list (plist-get entry :session)
        (plist-get entry :file)
        (scholia-db-annotation-text (plist-get entry :annotation))))

(defun scholia-search-test--forget (&rest files)
  "Kill the buffers visiting FILES, discarding whatever they hold."
  (dolist (file files)
    (let ((buffer (and file (find-buffer-visiting file))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest scholia-search-walk-spans-every-session-carrying-its-context ()
  "The walk visits every session and keeps each annotation's origin.
It is the reusable half the recurring-annotation prompt, the send-history
search and the dashboard consume, so an annotation that comes back
without the session, file and record it was read from is of no use to
them.  The assignment store shares the session directory and is no
session: an entry for it means the walk read the directory listing rather
than the sessions.  Each annotation comes back whole rather than reduced
to the fields this command happens to read: the send history is off
`:sends' and the reply thread is off `:id' and `:reply-to', and a walk
that projects those away serves the command alone."
  (scholia-search-test--with-state
    (let ((alpha-one (scholia-search-test--source "alpha-one.txt" "one\n"))
          (alpha-two (scholia-search-test--source "alpha-two.txt" "two\n"))
          (beta-one (scholia-search-test--source "beta-one.txt" "three\n"))
          (gamma-one (scholia-search-test--source "gamma-one.txt" "four\n")))
      (with-temp-file scholia-session-state-file
        (insert "((\"/elsewhere/\" . \"alpha\"))\n"))
      (scholia-search-test--seed
       "alpha" alpha-one
       (list (scholia-search-test--annotation "id-1" "note one" "one" 1 4)))
      (scholia-search-test--seed
       "alpha" alpha-two
       (list (scholia-search-test--annotation "id-2" "note two" "two" 1 4)))
      (scholia-search-test--seed
       "beta" beta-one
       (list (scholia-search-test--annotation "id-3" "note three" "three" 1 6)))
      (scholia-search-test--seed
       "gamma" gamma-one
       (list (scholia-search-test--annotation "id-4" "note four" "four" 1 5)))
      (let* ((entries (scholia-search-annotations))
             (origins (mapcar #'scholia-search-test--origin entries)))
        (should (equal 4 (length origins)))
        (should (member (list "alpha" alpha-one "note one") origins))
        (should (member (list "alpha" alpha-two "note two") origins))
        (should (member (list "beta" beta-one "note three") origins))
        (should (member (list "gamma" gamma-one "note four") origins))
        (should (seq-every-p
                 (lambda (entry)
                   (let ((record (plist-get entry :record)))
                     (and (equal (plist-get entry :file)
                                 (scholia-db-record-file record))
                          (equal "seeded"
                                 (scholia-db-record-checksum record))
                          (member (plist-get entry :annotation)
                                  (scholia-db-record-annotations record)))))
                 entries))))))

(ert-deftest scholia-search-candidate-carries-all-four-search-keys ()
  "A candidate reads as its annotation text, annotated text, file and session.
`completing-read' matches against the candidate string and nothing else,
so a key the string leaves out is a key nobody can ever search on."
  (scholia-search-test--with-state
    (let ((file (scholia-search-test--source "widget.txt"
                                             "the covered words\n")))
      (scholia-search-test--seed
       "hedgerow" file
       (list (scholia-search-test--annotation
              "id-1" "revisit this" "the covered words" 1 18)))
      (let ((candidate (scholia-search-candidate-string
                        (car (scholia-search-annotations)))))
        (should (string-match-p "revisit this" candidate))
        (should (string-match-p "the covered words" candidate))
        (should (string-match-p "widget\\.txt" candidate))
        (should (string-match-p "hedgerow" candidate))))))

(ert-deftest scholia-search-jumps-to-the-candidate-not-to-its-namesake ()
  "Selecting a candidate opens its own file and puts point on its annotation.
Two annotations carry the same text in different sessions and files, so a
command keying candidates on annotation text alone cannot tell them
apart: it lands on the wrong file, or the two candidates collapse into
one and it lands nowhere."
  (scholia-search-test--with-state
    (let ((decoy (scholia-search-test--source "decoy.txt" "alpha beta gamma\n"))
          (wanted (scholia-search-test--source "wanted.txt" "delta epsilon\n")))
      (scholia-search-test--seed
       "one" decoy
       (list (scholia-search-test--annotation
              "id-decoy" "shared note" "alpha" 1 6)))
      (scholia-search-test--seed
       "two" wanted
       (list (scholia-search-test--annotation
              "id-wanted" "shared note" "epsilon" 7 14)))
      (unwind-protect
          (progn
            (scholia-search-test--choosing "wanted\\.txt" (scholia-search))
            (should (equal wanted (buffer-file-name)))
            (should (equal 7 (point)))
            (should-not (find-buffer-visiting decoy)))
        (scholia-search-test--forget decoy wanted)))))

(ert-deftest scholia-search-switches-session-only-when-the-jump-needs-it ()
  "A jump out of the current session switches; a jump inside it does not.
Opening the file without switching leaves the buffer drawing from a
session that does not hold the annotation, and switching to the session
already current redraws every buffer bound to it for nothing."
  (scholia-search-test--with-state
    (let ((here (scholia-search-test--source "here.txt" "alpha beta\n"))
          (there (scholia-search-test--source "there.txt" "gamma delta\n"))
          (switches 0))
      (scholia-search-test--seed
       "here" here
       (list (scholia-search-test--annotation "id-here" "the near note"
                                              "alpha" 1 6)))
      (scholia-search-test--seed
       "there" there
       (list (scholia-search-test--annotation "id-there" "the far note"
                                              "gamma" 1 6)))
      (setq scholia-session "here")
      (add-hook 'scholia-session-switch-hook
                (lambda () (setq switches (1+ switches))))
      (unwind-protect
          (progn
            (scholia-search-test--choosing "the far note" (scholia-search))
            (should (equal "there" (default-value 'scholia-session)))
            (should (equal 1 switches))
            (goto-char 7)
            (scholia-annotate "the unsaved note")
            (scholia-search-test--choosing "the far note" (scholia-search))
            (should (equal 1 (point)))
            (should (equal "there" (default-value 'scholia-session)))
            (should (equal 1 switches))
            (should (equal '("the far note" "the unsaved note")
                           (sort (mapcar #'scholia-db-annotation-text
                                         (scholia-db-record-annotations
                                          (scholia-db-record
                                           (scholia-session-file "there") there)))
                                 #'string<))))
        (scholia-search-test--forget here there)))))

(ert-deftest scholia-search-announces-a-completion-category ()
  "The collection the command offers is categorised as `scholia-annotation'.
marginalia and consult attach to a completion category and to nothing
else, so a bare list of strings is a collection neither can ever enhance,
however right the rest of the command is.  The symbol itself is the
contract: marginalia picks its annotator by that symbol, so a candidate
announced as `file' is annotated with the file attributes of a path
parsed out of the candidate string, and the recurring-annotation prompt
and the dashboard have nothing of their own to key on."
  (scholia-search-test--with-state
    (let ((file (scholia-search-test--source "solo.txt" "alpha beta\n"))
          (metadata nil))
      (scholia-search-test--seed
       "solo" file
       (list (scholia-search-test--annotation "id-1" "a note" "alpha" 1 6)))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _)
                         (setq metadata
                               (completion-metadata "" collection nil))
                         (car (all-completions "" collection)))))
              (scholia-search))
            (should (eq 'scholia-annotation
                        (completion-metadata-get metadata 'category))))
        (scholia-search-test--forget file)))))

(ert-deftest scholia-search-offers-a-reply-and-lands-on-its-parent ()
  "A reply is a candidate, and choosing it puts point on its parent.
A reply carries nil for `:beg' and `:end', so its parent's location is
the only position it has.  Leaving replies out of the candidates makes
text the user wrote unfindable, and landing anywhere but the parent
sends the reader to a place nobody annotated."
  (scholia-search-test--with-state
    (let ((file (scholia-search-test--source "thread.txt"
                                             "alpha beta gamma\n")))
      (scholia-search-test--seed
       "threaded" file
       (list (scholia-search-test--annotation
              "id-parent" "the parent note" "beta" 7 11)
             (scholia-search-test--reply
              "id-reply" "the answering note" "id-parent")))
      (unwind-protect
          (progn
            (should (member "id-reply"
                            (mapcar
                             (lambda (entry)
                               (scholia-db-annotation-id
                                (plist-get entry :annotation)))
                             (scholia-search-annotations))))
            (scholia-search-test--choosing "the answering note"
              (scholia-search))
            (should (equal file (buffer-file-name)))
            (should (equal 7 (point))))
        (scholia-search-test--forget file)))))

(ert-deftest scholia-search-distinguishes-identical-annotations-by-id ()
  "Two otherwise identical annotations remain independently selectable."
  (scholia-search-test--with-state
    (let ((file (scholia-search-test--source "identical.txt" "alpha beta\n")))
      (scholia-search-test--seed
       "alpha" file
       (list (scholia-search-test--annotation "first" "same" "alpha" 1 6)
             (scholia-search-test--annotation "second" "same" "alpha" 7 11)))
      (unwind-protect
          (let* ((entries (scholia-search-annotations))
                 (candidates (mapcar #'scholia-search-candidate-string entries)))
            (should (= 2 (length (delete-dups candidates))))
            (scholia-search-test--choosing "\\[second\\]" (scholia-search))
            (should (= 7 (point))))
        (scholia-search-test--forget file)))))

(ert-deftest scholia-search-rejects-unpositioned-results-before-side-effects ()
  "Orphans, cycles, and empty input never visit a source or switch sessions."
  (scholia-search-test--with-state
    (let ((file (expand-file-name "missing.txt" scholia-session-directory))
          (switched nil)
          (visited nil))
      (scholia-search-test--seed
       "alpha" file
       (list (scholia-search-test--reply "orphan" "orphan reply" "absent")))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "orphan reply"))
                ((symbol-function 'scholia-session-switch)
                 (lambda (&rest _) (setq switched t)))
                ((symbol-function 'find-file)
                 (lambda (&rest _) (setq visited t))))
        (should-error (scholia-search) :type 'user-error))
      (should-not switched)
      (should-not visited)
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) ""))
                ((symbol-function 'scholia-session-switch)
                 (lambda (&rest _) (setq switched t)))
                ((symbol-function 'find-file)
                 (lambda (&rest _) (setq visited t))))
        (should-not (scholia-search)))
      (should-not switched)
      (should-not visited)
      (let* ((first (scholia-search-test--reply "first" "first reply" "second"))
             (second (scholia-search-test--reply "second" "second reply" "first"))
             (entry (list :record (scholia-db-make-record file (list first second) "seed")
                          :annotation first)))
        (should-error (scholia-search--jump-annotation entry)
                      :type 'user-error)))))

(ert-deftest scholia-search-binds-visited-project-buffer-to-selected-session ()
  "A selected result outranks the session assigned to its project."
  (scholia-search-test--with-state
    (let* ((file (scholia-search-test--source "project.txt" "alpha beta gamma\n"))
           (root (file-name-directory file))
           (scholia-project-sessions (list (cons root "beta")))
           (scholia-project-root-function (lambda () root)))
      (scholia-search-test--seed
       "alpha" file
       (list (scholia-search-test--annotation "alpha-id" "alpha note" "alpha" 1 6)))
      (scholia-search-test--seed
       "beta" file
       (list (scholia-search-test--annotation "beta-id" "beta note" "beta" 7 11)))
      (setq scholia-session "beta")
      (unwind-protect
          (progn
            (find-file file)
            (scholia-mode 1)
            (should (equal '("beta note")
                           (mapcar (lambda (chain)
                                     (overlay-get (car chain) 'scholia-annotation))
                                   (scholia-buffer-chains))))
            (goto-char 13)
            (scholia-annotate "beta unsaved")
            (scholia-search-test--choosing "alpha note" (scholia-search))
            (should (equal "alpha" (scholia-session-name)))
            (should (= 1 (point)))
            (should (equal '("alpha note")
                           (mapcar (lambda (chain)
                                     (overlay-get (car chain) 'scholia-annotation))
                                   (scholia-buffer-chains))))
            (scholia-save-annotations)
            (should (equal '("alpha note")
                           (mapcar #'scholia-db-annotation-text
                                   (scholia-db-record-annotations
                                    (scholia-db-record (scholia-session-file "alpha") file)))))
            (should (equal '("beta note" "beta unsaved")
                           (sort (mapcar #'scholia-db-annotation-text
                                         (scholia-db-record-annotations
                                          (scholia-db-record (scholia-session-file "beta") file)))
                                 #'string<))))
        (scholia-search-test--forget file)))))

(ert-deftest scholia-search-and-status-open-revision-records-and-degrade-safely ()
  "Search and status share revision-aware opening rather than stored offsets."
  (scholia-search-test--with-state
    (let* ((file (scholia-search-test--source
                  "revision.txt" "before\nrevision target\n"))
           (directory (file-name-directory file))
           (revision nil)
           (annotation (scholia-search-test--annotation
                        "revision-id" "revision note" "revision" 8 16)))
      (should (zerop (process-file "git" nil nil nil "-C" directory "init" "-q")))
      (should (zerop (process-file "git" nil nil nil "-C" directory "config"
                                  "user.email" "test@example.invalid")))
      (should (zerop (process-file "git" nil nil nil "-C" directory "config"
                                  "user.name" "Scholia Test")))
      (should (zerop (process-file "git" nil nil nil "-C" directory "add"
                                  (file-name-nondirectory file))))
      (should (zerop (process-file "git" nil nil nil "-C" directory "commit" "-q"
                                  "-m" "revision fixture")))
      (setq revision
            (with-temp-buffer
              (should (zerop (process-file "git" nil (current-buffer) nil "-C"
                                           directory "rev-parse" "HEAD")))
              (string-trim (buffer-string))))
      (with-temp-file file
        (insert "before\nworking tree line\nrevision target\n"))
      (plist-put annotation :revision revision)
      (scholia-search-test--seed "revision" file (list annotation))
      (let ((entry (car (scholia-search-annotations)))
            (status-loaded (featurep 'scholia-status)))
        (unwind-protect
            (progn
              (scholia-search--jump entry)
              (should (equal (buffer-string) "before\nrevision target\n"))
              (should (= (point) 8))
              (let ((revision-buffer (current-buffer)))
                (with-current-buffer revision-buffer (set-buffer-modified-p nil))
                (kill-buffer revision-buffer))
              (require 'scholia-status)
              (scholia-status-jump entry)
              (should (equal (buffer-string) "before\nrevision target\n"))
              (should (= (point) 8))
              (let ((revision-buffer (current-buffer)))
                (with-current-buffer revision-buffer (set-buffer-modified-p nil))
                (kill-buffer revision-buffer))
              (plist-put (plist-get entry :annotation) :revision (make-string 40 ?0))
              (let (messages)
                (cl-letf (((symbol-function 'message)
                           (lambda (format &rest arguments)
                             (push (apply #'format-message format arguments) messages))))
                  (scholia-search--jump entry))
                (should (equal (buffer-string)
                               "before\nworking tree line\nrevision target\n"))
                (should (= (point) (point-min)))
                (should (seq-some (lambda (message)
                                    (string-match-p "working tree" message))
                                  messages))))
          (scholia-search-test--forget file)
          (unless status-loaded
            (unload-feature 'scholia-status t)))))))

(ert-deftest scholia-search-reads-each-session-through-one-snapshot ()
  "The walk opens one read snapshot per session and keeps full values."
  (scholia-search-test--with-state
    (let* ((first (scholia-search-test--source "one.txt" "one\n"))
           (second (scholia-search-test--source "two.txt" "two\n"))
           (annotation (scholia-search-test--annotation "id" "note" "one" 1 4))
           (record (scholia-db-make-record first (list annotation) "seeded"))
           (reads 0)
           (reading (symbol-function 'scholia-db--reading)))
      (scholia-search-test--seed "alpha" first (list annotation))
      (scholia-search-test--seed
       "alpha" second
       (list (scholia-search-test--annotation "other" "other" "two" 1 4)))
      (cl-letf (((symbol-function 'scholia-session-list) (lambda () '("alpha")))
                ((symbol-function 'scholia-db--reading)
                 (lambda (session-file read)
                   (setq reads (1+ reads))
                   (funcall reading session-file read))))
        (let ((entry (seq-find (lambda (item)
                                 (equal first (plist-get item :file)))
                               (scholia-search-annotations))))
          (should (equal record (plist-get entry :record)))
          (should (equal annotation (plist-get entry :annotation)))))
      (should (= 1 reads)))))

(ert-deftest scholia-search-sends-to-keeps-matching-entries-whole ()
  "Filtering history by destination keeps the collector's full entries."
  (scholia-search-test--with-state
    (let* ((alpha-file (scholia-search-test--source "alpha.txt" "alpha\n"))
           (beta-file (scholia-search-test--source "beta.txt" "beta\n"))
           (alpha-send (scholia-db-make-send :kind 'agent
                                              :target "claude-alpha"
                                              :label "Claude · Alpha"
                                              :herdr-session "review"
                                              :format 'rustc
                                              :scope 'file))
           (beta-send (scholia-db-make-send :kind 'agent
                                             :target "claude-beta"
                                             :label "Claude · Beta"
                                             :herdr-session "review"
                                             :format 'rustc
                                             :scope 'file))
           (alpha (plist-put (scholia-search-test--annotation
                              "alpha-id" "alpha note" "alpha" 1 6)
                             :sends (list alpha-send)))
           (beta (plist-put (scholia-search-test--annotation
                             "beta-id" "beta note" "beta" 1 5)
                            :sends (list beta-send))))
      (scholia-search-test--seed "alpha" alpha-file (list alpha))
      (scholia-search-test--seed "beta" beta-file (list beta))
      (let ((entries (scholia-search-sends-to "claude-alpha")))
        (should (= 1 (length entries)))
        (should (equal (list "alpha" alpha-file "alpha note")
                       (scholia-search-test--origin (car entries))))
        (should (equal (list alpha)
                       (scholia-db-record-annotations
                        (plist-get (car entries) :record))))))))

(ert-deftest scholia-search-sends-groups-search-keys-and-jumps-reply-to-root ()
  "Send completion groups destinations, finds every key, and follows replies."
  (scholia-search-test--with-state
    (let* ((file (scholia-search-test--source "thread.txt" "alpha beta\n"))
           (send (scholia-db-make-send :kind 'agent
                                        :target "claude-planner"
                                        :label "Claude · Planner"
                                        :herdr-session "sprint-7"
                                        :format 'rustc
                                        :scope 'file))
           (parent (scholia-search-test--annotation
                    "parent" "parent note" "alpha" 1 6))
           (reply (plist-put (scholia-search-test--reply
                              "reply" "reply note" "parent")
                             :sends (list send))))
      (scholia-search-test--seed "planning" file (list parent reply))
      (unwind-protect
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _)
                       (let* ((metadata (completion-metadata "" collection nil))
                              (group (completion-metadata-get metadata 'group-function))
                              (candidate (seq-find
                                          (lambda (item)
                                            (string-match-p
                                             (regexp-quote (scholia-db-send-at send)) item))
                                          (all-completions "" collection))))
                         (should (string-match-p "Claude · Planner" candidate))
                         (should (string-match-p "sprint-7" candidate))
                         (should (equal "Claude · Planner"
                                        (funcall group candidate nil)))
                         candidate))))
            (scholia-search-sends)
            (should (equal "planning" (scholia-session-name)))
            (should (equal file (buffer-file-name)))
            (should (= 1 (point))))
        (scholia-search-test--forget file)))))

(provide 'scholia-search-test)
;;; scholia-search-test.el ends here
