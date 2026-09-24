;;; scholia-forge-test.el --- Tests for GitHub pull request threads  -*- lexical-binding: t; -*-

(require 'ert)
(require 'seq)
(require 'magit-diff)
(require 'scholia-test-helper)
(require 'scholia-forge)

(defvar forge-buffer-topic)

(defvar scholia-forge-test--posts nil
  "Requests the stubbed `gh' was asked to post, as endpoints and payloads.")

(defun scholia-forge-test--git (repo &rest args)
  "Run git with ARGS in REPO and return what it printed, trimmed."
  (with-temp-buffer
    (apply #'process-file "git" nil t nil "-C" repo args)
    (string-trim (buffer-string))))

(defun scholia-forge-test--repo ()
  "Return a repository whose `head' branch changes its `base' branch.
a.txt has line 3 changed, line 7 removed and a line added after line 9;
b.txt has a line added."
  (let ((repo (make-temp-file "scholia-forge-" t)))
    (scholia-forge-test--git repo "init" "-b" "base")
    (scholia-forge-test--git repo "config" "user.email" "test@example.com")
    (scholia-forge-test--git repo "config" "user.name" "Scholia Test")
    (with-temp-file (expand-file-name "a.txt" repo)
      (dotimes (index 10) (insert (format "line %d\n" (1+ index)))))
    (with-temp-file (expand-file-name "b.txt" repo)
      (insert "keep\n"))
    (scholia-forge-test--git repo "add" ".")
    (scholia-forge-test--git repo "commit" "-m" "base")
    (scholia-forge-test--git repo "switch" "-c" "head")
    (with-temp-file (expand-file-name "a.txt" repo)
      (insert "line 1\nline 2\nchanged 3\nline 4\nline 5\nline 6\n"
              "line 8\nline 9\nadded\nline 10\n"))
    (with-temp-file (expand-file-name "b.txt" repo)
      (insert "keep\nmore\n"))
    (scholia-forge-test--git repo "commit" "-am" "head")
    repo))

(defun scholia-forge-test--fixtures (repo)
  "Return what GitHub answers for the pull request REPO's branches make."
  (let ((head (scholia-forge-test--git repo "rev-parse" "head"))
        (base (scholia-forge-test--git repo "rev-parse" "base")))
    `((:pull . (:head (:sha ,head :ref "feature") :base (:sha ,base :ref "main")
                      :user (:login "alice") :state "open"
                      :labels ((:name "bug") (:name "ui"))
                      :requested_reviewers ((:login "carol"))
                      :body "The description" :created_at "2026-09-01T10:00:00Z"))
      (:comments
       . ((:id 1 :path "a.txt" :line 3 :side "RIGHT" :subject_type "line"
               :user (:login "bob") :body "Why this?" :created_at "2026-09-02T10:00:00Z")
          (:id 2 :in_reply_to_id 1 :path "a.txt" :line 3 :side "RIGHT"
               :user (:login "alice") :body "Because." :created_at "2026-09-02T11:00:00Z")
          (:id 3 :path "a.txt" :line 7 :side "LEFT" :subject_type "line"
               :user (:login "carol") :body "Keep this line" :created_at "2026-09-02T12:00:00Z")
          (:id 4 :path "a.txt" :line nil :original_line 5 :subject_type "line"
               :original_commit_id "0123456789abcdef" :diff_hunk "@@ -5,1 +5,1 @@\n-old\n+new"
               :user (:login "bob") :body "Stale remark" :created_at "2026-09-01T12:00:00Z")
          (:id 5 :path "b.txt" :subject_type "file"
               :user (:login "dave") :body "Whole file" :created_at "2026-09-03T10:00:00Z")))
      (:issue-comments
       . ((:id 10 :user (:login "erin") :body "Looks good" :created_at "2026-09-04T10:00:00Z")))
      (:reviews
       . ((:id 20 :user (:login "bob") :state "APPROVED" :body "Ship it"
               :submitted_at "2026-09-03T12:00:00Z")
          (:id 21 :user (:login "carol") :state "COMMENTED" :body ""
               :submitted_at "2026-09-02T12:00:00Z"))))))

(defun scholia-forge-test--stub (fixtures)
  "Return a stand-in for `scholia-forge--call' answering from FIXTURES."
  (lambda (args &optional input callback)
    (let* ((endpoint (nth 1 args))
           (data (cond
                  ((equal endpoint "user") '(:login "me"))
                  ((string-suffix-p "/pulls/42" endpoint) (alist-get :pull fixtures))
                  ((string-suffix-p "/pulls/42/comments" endpoint)
                   (alist-get :comments fixtures))
                  ((string-suffix-p "/issues/42/comments" endpoint)
                   (if input nil (alist-get :issue-comments fixtures)))
                  ((string-suffix-p "/pulls/42/reviews" endpoint)
                   (if input nil (alist-get :reviews fixtures))))))
      (when input
        (push (cons endpoint (json-parse-string input :object-type 'plist :array-type 'list))
              scholia-forge-test--posts))
      (if callback (funcall callback nil data) data))))

(defmacro scholia-forge-test--with-pull (&rest body)
  "Run BODY in the diff buffer of a stubbed pull request, then clean up."
  (declare (indent 0) (debug body))
  `(scholia-test-with-session-directory
     (let* ((repo (scholia-forge-test--repo))
            (fixtures (scholia-forge-test--fixtures repo))
            (scholia-forge--viewers nil)
            (scholia-forge-test--posts nil)
            (default-directory repo)
            (buffer nil))
       (cl-letf (((symbol-function 'scholia-forge--call)
                  (scholia-forge-test--stub fixtures))
                 ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
         (unwind-protect
             (progn
               (setq buffer (scholia-forge-open "github.com" "owner" "repo" 42
                                                "base...head" "The title"))
               (with-current-buffer buffer ,@body))
           (when (buffer-live-p buffer) (kill-buffer buffer))
           (delete-directory repo t))))))

(defun scholia-forge-test--goto (text)
  "Move to the start of the diff line reading TEXT."
  (goto-char (point-min))
  (search-forward (concat "\n" text "\n"))
  (forward-line -1))

(defun scholia-forge-test--chains (kind)
  "Return the chains of KIND in the current buffer."
  (seq-filter (lambda (chain)
                (eq (get (overlay-get (car chain) 'scholia--chain-id)
                         'scholia-forge-kind)
                    kind))
              (scholia-buffer-chains)))

(defun scholia-forge-test--replies (chain)
  "Return the texts of the replies drawn under CHAIN."
  (mapcar (lambda (entry) (plist-get (cdr entry) :text))
          (alist-get (overlay-get (car chain) 'scholia--chain-id) scholia--replies)))

(defun scholia-forge-test--line (chain)
  "Return the diff line CHAIN's last overlay ends on."
  (save-excursion
    (goto-char (overlay-end (car (last chain))))
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))


(ert-deftest scholia-forge-names-the-buffer-and-the-draft-session ()
  (scholia-forge-test--with-pull
    (should (equal (buffer-name) "*magit-diff: owner/repo #42 The title*"))
    (should scholia-forge-mode)
    (should (eq (scholia-forge--get :status) 'ready))
    (should (equal (scholia-forge--get :viewer) "me"))
    (should (equal (scholia-forge--get :session) "owner-repo-pr-42"))
    (should (eq scholia-note-placement 'below))
    (should (string-prefix-p
             "\n" (scholia-render-note (car (scholia-forge-test--chains 'remote)))))
    (should (equal (scholia-forge--session-name "git.corp" "o w" "r/x" 7)
                   "git.corp-o-w-r-x-pr-7"))))

(ert-deftest scholia-forge-finds-lines-on-both-sides-of-the-diff ()
  (scholia-forge-test--with-pull
    (let ((line (lambda (path side number)
                  (when-let* ((pos (scholia-forge--line-position path side number)))
                    (save-excursion
                      (goto-char pos)
                      (buffer-substring-no-properties pos (line-end-position)))))))
      (should (equal (funcall line "a.txt" "RIGHT" 3) "+changed 3"))
      (should (equal (funcall line "a.txt" "LEFT" 3) "-line 3"))
      (should (equal (funcall line "a.txt" "LEFT" 7) "-line 7"))
      (should (equal (funcall line "a.txt" "RIGHT" 9) "+added"))
      (should (equal (funcall line "a.txt" "RIGHT" 6) " line 6"))
      (should-not (funcall line "a.txt" "RIGHT" 100))
      (should-not (funcall line "missing.txt" "RIGHT" 1)))))

(ert-deftest scholia-forge-reads-back-where-a-line-is ()
  (scholia-forge-test--with-pull
    (scholia-forge-test--goto "+changed 3")
    (let ((location (scholia-forge--location (point))))
      (should (equal (list (plist-get location :path) (plist-get location :side)
                           (plist-get location :line))
                     '("a.txt" "RIGHT" 3))))
    (scholia-forge-test--goto "-line 7")
    (let ((location (scholia-forge--location (point))))
      (should (equal (list (plist-get location :side) (plist-get location :line))
                     '("LEFT" 7))))
    (scholia-forge-test--goto " line 5")
    (let ((start (point)))
      (scholia-forge-test--goto " line 6")
      (set-mark start)
      (activate-mark)
      (goto-char (line-end-position))
      (let ((location (scholia-forge--region-location)))
        (should (equal (list (plist-get location :start-line) (plist-get location :line))
                       '(5 6))))
      (deactivate-mark))))

(ert-deftest scholia-forge-draws-every-kind-of-thread ()
  (scholia-forge-test--with-pull
    (let ((conversation (car (scholia-forge-test--chains 'conversation)))
          (remote (scholia-forge-test--chains 'remote))
          (outdated (scholia-forge-test--chains 'outdated)))
      (should conversation)
      (should (= (overlay-start (car conversation)) (point-min)))
      (should-not (scholia-forge-test--replies conversation))
      (should (equal (mapcar (lambda (entry) (plist-get (cdr entry) :text))
                             (cdr (car (get (overlay-get (car conversation)
                                                         'scholia--chain-id)
                                            'scholia-forge-threads))))
                     '("[APPROVED] Ship it" "Looks good")))
      (should (= 3 (length remote)))
      (let ((threads (mapcar (lambda (chain)
                               (cons (scholia-forge-test--line chain)
                                     (scholia-forge-test--replies chain)))
                             remote)))
        (should (member '("+changed 3" "Because.") threads))
        (should (assoc "-line 7" threads))
        (should (seq-find (lambda (thread) (string-match-p "b\\.txt" (car thread)))
                          threads)))
      (should (= 1 (length outdated)))
      (should (string-match-p "a\\.txt" (scholia-forge-test--line (car outdated))))
      (should (= 1 (length (get (overlay-get (car (car outdated)) 'scholia--chain-id)
                                'scholia-forge-threads)))))))

(ert-deftest scholia-forge-draws-each-thread-once-however-often-magit-refreshes ()
  (scholia-forge-test--with-pull
    (let ((count (length (scholia-buffer-chains)))
          (shown (lambda ()
                   (seq-count (lambda (overlay) (overlay-get overlay 'cera))
                              (overlays-in (point-min) (point-max))))))
      (let ((panes (funcall shown)))
        (magit-refresh)
        (should (= count (length (scholia-buffer-chains))))
        (should (= panes (funcall shown)))
        (magit-diff-setup-buffer "base...head" nil nil nil 'committed t)
        (should (= count (length (scholia-buffer-chains))))
        (should (= panes (funcall shown)))))))

(ert-deftest scholia-forge-keeps-drafts-in-the-pull-requests-session ()
  (scholia-forge-test--with-pull
    (scholia-forge-test--goto "+added")
    (scholia-forge-comment "A new remark")
    (scholia-forge-test--goto "+changed 3")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt candidates &rest _)
                 (car (seq-find (lambda (candidate)
                                  (string-match-p "Why this" (car candidate)))
                                candidates)))))
      (scholia-forge-reply "My answer"))
    (let ((drafts (scholia-forge--drafts)))
      (should (= 2 (length drafts)))
      (should (equal (mapcar #'scholia-forge--draft-kind drafts) '(review reply)))
      (should (equal (plist-get (plist-get (car drafts) :forge) :line) 9))
      (should (equal (plist-get (cadr drafts) :reply-to) "gh:1")))
    (should-not (bound-and-true-p scholia-mode))
    (let ((draft (car (scholia-forge-test--chains 'draft))))
      (should (equal (scholia-forge-test--line draft) "+added")))
    (should (member "My answer"
                    (apply #'append
                           (mapcar #'scholia-forge-test--replies
                                   (scholia-forge-test--chains 'remote)))))
    (let ((id (plist-get (car (scholia-forge--drafts)) :id)))
      (scholia-forge--draft-update id "Changed remark")
      (should (equal (plist-get (car (scholia-forge--drafts)) :text) "Changed remark"))
      (scholia-forge--draft-remove id)
      (should (= 1 (length (scholia-forge--drafts)))))))

(ert-deftest scholia-forge-pushes-drafts-in-one-batch ()
  (scholia-forge-test--with-pull
    (scholia-forge-test--goto "+added")
    (scholia-forge-comment "A new remark")
    (scholia-forge-test--goto "+changed 3")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt candidates &rest _) (car (cadr candidates)))))
      (scholia-forge-reply "My answer"))
    (goto-char (point-min))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt candidates &rest _) (car (cadr candidates)))))
      (scholia-forge-reply "Agreed"))
    (scholia-forge-push)
    (let ((posts (reverse scholia-forge-test--posts)))
      (should (= 3 (length posts)))
      (should (string-suffix-p "/pulls/42/reviews" (car (nth 0 posts))))
      (let ((review (cdr (nth 0 posts))))
        (should (equal (plist-get review :event) "COMMENT"))
        (should (equal (plist-get (car (plist-get review :comments)) :line) 9))
        (should (equal (plist-get (car (plist-get review :comments)) :side) "RIGHT")))
      (should (string-suffix-p "/pulls/42/comments/1/replies" (car (nth 1 posts))))
      (should (string-suffix-p "/issues/42/comments" (car (nth 2 posts))))
      (should (string-prefix-p "> " (plist-get (cdr (nth 2 posts)) :body))))
    (should-not (scholia-forge--drafts))))

(ert-deftest scholia-forge-keeps-drafts-a-refused-push-did-not-send ()
  (scholia-forge-test--with-pull
    (scholia-forge-test--goto "+added")
    (scholia-forge-comment "A new remark")
    (cl-letf (((symbol-function 'scholia-forge--call)
               (lambda (&rest _) (signal 'scholia-forge-error '("HTTP 422")))))
      (should-error (scholia-forge-push) :type 'scholia-forge-error))
    (should (= 1 (length (scholia-forge--drafts))))))

(ert-deftest scholia-forge-colours-each-comment-by-its-author ()
  (scholia-forge-test--with-pull
    (should (equal (scholia-forge-author-color "bob") (scholia-forge-author-color "bob")))
    (should-not (equal (scholia-forge-author-color "bob")
                       (scholia-forge-author-color "carol")))
    (let ((saturation (lambda (color) (nth 1 (scholia-color--hsl color)))))
      (should (< (funcall saturation (scholia-forge-author-color "bob"))
                 (let ((scholia-forge-author-saturation 1.0))
                   (funcall saturation (scholia-forge-author-color "bob")))))
      (let ((scholia-forge-author-colors '(("bob" . "#ff0000"))))
        (should (equal (scholia-forge-author-color "bob") "#ff0000"))))
    (let* ((chain (seq-find (lambda (chain)
                              (equal (scholia-forge-test--line chain) "+changed 3"))
                            (scholia-forge-test--chains 'remote)))
           (id (overlay-get (car chain) 'scholia--chain-id))
           (reply (cdr (car (alist-get id scholia--replies)))))
      (should (equal (scholia-render-color nil id)
                     (scholia-color-on-theme (scholia-forge-author-color "bob"))))
      (should (equal (scholia-render-color nil id reply)
                     (scholia-color-on-theme (scholia-forge-author-color "alice")))))))

(ert-deftest scholia-forge-explains-what-gh-refused ()
  (should (string-match-p "gh auth login --hostname h"
                          (scholia-forge--error-message "HTTP 401: Bad credentials" "h")))
  (should (string-match-p "not found"
                          (scholia-forge--error-message "HTTP 404: Not Found" "h")))
  (let ((scholia-forge-gh-program "scholia-forge-no-such-program"))
    (should-error (scholia-forge--program) :type 'user-error)))

(ert-deftest scholia-forge-loads-without-forge-and-unloads-cleanly ()
  (should-not (featurep 'forge))
  (add-hook 'magit-refresh-buffer-hook #'scholia-forge--refresh-hook)
  (unload-feature 'scholia-forge t)
  (should-not (memq 'scholia-forge--refresh-hook magit-refresh-buffer-hook))
  (require 'scholia-forge))

(ert-deftest scholia-forge-heads-the-conversation-with-how-the-pull-request-stands ()
  (scholia-forge-test--with-pull
    (let ((conversation (car (scholia-forge-test--chains 'conversation))))
      (should (string-prefix-p
               (concat "open · alice · main ← feature · bug, ui · review: carol\n"
                       "The description\n2 comments in the conversation")
               (overlay-get (car conversation) 'scholia-annotation))))
    (should (equal (scholia-forge--summary '(:state "closed" :merged_at "x"
                                                    :user (:login "a")
                                                    :base (:ref "b") :head (:ref "h")))
                   "merged · a · b ← h"))))

(ert-deftest scholia-forge-keeps-telling-forge-which-pull-request-it-shows ()
  (scholia-forge-test--with-pull
    (should-not (local-variable-p 'forge-buffer-topic))
    (scholia-forge--put :topic 'the-topic)
    (magit-refresh)
    (should (eq forge-buffer-topic 'the-topic))
    (magit-diff-setup-buffer "base...head" nil nil nil 'committed t)
    (should (local-variable-p 'forge-buffer-topic))
    (should (eq forge-buffer-topic 'the-topic))))

(ert-deftest scholia-forge-shows-only-a-threads-latest-replies ()
  (scholia-forge-test--with-pull
    (let ((scholia-forge-note-replies 0))
      (scholia-forge--decorate)
      (let ((chain (seq-find (lambda (chain)
                               (equal (scholia-forge-test--line chain) "+changed 3"))
                             (scholia-forge-test--chains 'remote))))
        (should-not (scholia-forge-test--replies chain))
        (should (string-match-p "… 1 earlier reply"
                                (overlay-get (car chain) 'scholia-annotation)))
        (should (= 1 (length (cdr (car (get (overlay-get (car chain) 'scholia--chain-id)
                                            'scholia-forge-threads))))))))))

(ert-deftest scholia-forge-cuts-long-comments-short-in-their-notes ()
  "A note shows the start of a long comment; the thread keeps all of it."
  (should (equal (scholia-forge--clip "one\ntwo\nthree" 5) "one\ntwo\nthree"))
  (should (string-prefix-p "one\ntwo\n… 1 more line"
                           (scholia-forge--clip "one\ntwo\nthree" 2)))
  (scholia-forge-test--with-pull
    (let ((scholia-forge-note-lines 1))
      (scholia-forge-test--goto "+added")
      (scholia-forge-comment "first line\nsecond line")
      (let* ((chain (car (scholia-forge-test--chains 'draft)))
             (id (overlay-get (car chain) 'scholia--chain-id)))
        (should (string-prefix-p "first line\n… 1 more line"
                                 (overlay-get (car chain) 'scholia-annotation)))
        (should (equal (plist-get (car (car (get id 'scholia-forge-threads))) :text)
                       "first line\nsecond line"))))))

(ert-deftest scholia-forge-decorates-the-buffer-magit-set-up ()
  "A stray buffer under the pull request's name does not take its threads."
  (let ((stray (get-buffer-create "*magit-diff: owner/repo #42 The title*")))
    (unwind-protect
        (scholia-forge-test--with-pull
          (should-not (eq (current-buffer) stray))
          (should (derived-mode-p 'magit-diff-mode))
          (should scholia-forge-mode)
          (should (scholia-buffer-chains))
          (should-not (buffer-local-value 'scholia-forge-mode stray)))
      (kill-buffer stray))))

(provide 'scholia-forge-test)
;;; scholia-forge-test.el ends here
