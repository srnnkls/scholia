;;; scholia-org-remark-test.el --- Tests for the org-remark export  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Covers `scholia-org-remark', which writes one or several sessions out
;; as an org-remark notes file.  Acceptance is structural: one level-one
;; headline per source file, one level-two headline per annotation, and
;; the properties org-remark itself reads back.  Those four names are
;; taken from the installed org-remark 1.3.0, org-remark.el lines
;; 238-241, and read back at lines 1623-1647:
;;
;;   org-remark-file  on the file headline, matched by `org-find-property'
;;   org-remark-id    on the annotation headline
;;   org-remark-beg   on the annotation headline, a numeric string
;;   org-remark-end   on the annotation headline, a numeric string
;;
;; The last test hands the export to `org-remark-highlights-get', so
;; org-remark's own reader is the oracle rather than this file's idea of
;; one.

;;; Code:

(require 'ert)
(require 'org)
(require 'seq)
(require 'subr-x)
(require 'scholia-test-helper)

(eval-and-compile
  (setq load-prefer-newer t)
  (require 'scholia-vars nil t)
  (require 'scholia-store nil t)
  (require 'scholia-db nil t)
  (require 'scholia-core nil t)
  (require 'scholia-export nil t))

(eval-when-compile
  (require 'org-remark)
  (require 'scholia-org-remark))

(declare-function org-remark-highlights-get "org-remark")
(declare-function scholia-org-remark-export "scholia-org-remark")

(defconst scholia-org-remark-test--source
  "alpha beta\n    gamma delta\nepsilon zeta\n"
  "Three annotatable lines.
The word \"alpha\" spans positions 1-6 and \"delta\" spans 22-27.")

(defmacro scholia-org-remark-test--with-sources (var &rest body)
  "Evaluate BODY with VAR bound to a fresh directory of fixture sources.
The directory holds \"aaa.txt\" and \"bbb.txt\", both carrying
`scholia-org-remark-test--source', and is deleted when BODY exits."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (file-name-as-directory
                (file-truename (make-temp-file "scholia-org-remark-" t)))))
     (unwind-protect
         (progn
           (require 'scholia-org-remark)
           (require 'org-remark)
           (dolist (name '("aaa.txt" "bbb.txt"))
             (with-temp-file (expand-file-name name ,var)
               (insert scholia-org-remark-test--source)))
           ,@body)
       (when (file-directory-p ,var)
         (delete-directory ,var t)))))

(defun scholia-org-remark-test--session (name records)
  "Create the session NAME holding RECORDS and return its file."
  (let ((session (scholia-session-file name)))
    (scholia-db-create-session session)
    (dolist (record records session)
      (scholia-db-store-record session record))))

(defun scholia-org-remark-test--record (file annotations)
  "Return a record carrying ANNOTATIONS against FILE."
  (scholia-db-make-record file annotations
                          (md5 scholia-org-remark-test--source)))

(defun scholia-org-remark-test--reply (id text parent-id)
  "Return a reply carrying TEXT under PARENT-ID."
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

(defun scholia-org-remark-test--outline (org)
  "Return each headline of ORG as (LEVEL FILE ID BEG END).
FILE, ID, BEG and END are the org-remark properties of that headline,
read with Org's own accessors, and are nil when the headline carries
none."
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (insert org)
    (org-map-entries
     (lambda ()
       (list (org-current-level)
             (org-entry-get (point) "org-remark-file")
             (org-entry-get (point) "org-remark-id")
             (org-entry-get (point) "org-remark-beg")
             (org-entry-get (point) "org-remark-end"))))))


;;;; The headline hierarchy

(ert-deftest scholia-org-remark-export-nests-annotations-under-their-file ()
  "Every file is one level-one headline and every annotation one below it.
A flat list of annotation headlines, or a file headline nested under
another, breaks the tree `org-remark-highlights-get' narrows to."
  (scholia-org-remark-test--with-sources directory
    (scholia-test-with-session-directory
      (let ((aaa (expand-file-name "aaa.txt" directory))
            (bbb (expand-file-name "bbb.txt" directory)))
        (scholia-org-remark-test--session
         "work"
         (list (scholia-org-remark-test--record
                aaa (list (scholia-db-make-annotation
                           "id-a" "first note" 1 6 "alpha")
                          (scholia-db-make-annotation
                           "id-b" "second note" 22 27 "delta")))
               (scholia-org-remark-test--record
                bbb (list (scholia-db-make-annotation
                           "id-c" "third note" 22 27 "delta")))))
        (let* ((org (scholia-org-remark-export "work"))
               (outline (scholia-org-remark-test--outline org)))
          (should (equal (mapcar #'car outline) '(1 2 2 1 2)))
          (should (equal (mapcar (lambda (entry)
                                   (or (nth 2 entry)
                                       (and (nth 1 entry) 'file)))
                                 outline)
                         '(file "id-a" "id-b" file "id-c")))
          (dolist (note '("first note" "second note" "third note"))
            (should (string-match-p (regexp-quote note) org))))))))


;;;; The properties org-remark reads

(ert-deftest scholia-org-remark-export-writes-the-properties-org-remark-reads ()
  "The file headline names its source and each annotation its id and span.
`org-remark-highlights-get' calls `string-to-number' on beg and end, so
they have to be numeric strings holding the annotation's own buffer
positions -- not its line, column or a zero-based offset."
  (scholia-org-remark-test--with-sources directory
    (scholia-test-with-session-directory
      (let ((aaa (expand-file-name "aaa.txt" directory)))
        (scholia-org-remark-test--session
         "work"
         (list (scholia-org-remark-test--record
                aaa (list (scholia-db-make-annotation
                           "id-a" "first note" 1 6 "alpha")
                          (scholia-db-make-annotation
                           "id-b" "second note" 22 27 "delta")))))
        (let ((outline (scholia-org-remark-test--outline
                        (scholia-org-remark-export "work"))))
          (should (equal (length outline) 3))
          (should (file-equal-p (nth 1 (nth 0 outline)) aaa))
          (should-not (nth 2 (nth 0 outline)))
          (should (equal (nthcdr 2 (nth 1 outline)) '("id-a" "1" "6")))
          (should (equal (nthcdr 2 (nth 2 outline)) '("id-b" "22" "27")))
          (should-not (nth 1 (nth 1 outline)))
          (should-not (nth 1 (nth 2 outline))))))))


;;;; Existing notes buffers

(ert-deftest scholia-org-remark-export-keeps-a-modified-notes-buffer-intact ()
  "Direct export does not erase or save a buffer already visiting NOTES.
Interactive replacement asks before it overwrites that existing notes file."
  (scholia-org-remark-test--with-sources directory
    (scholia-test-with-session-directory
      (let* ((aaa (expand-file-name "aaa.txt" directory))
             (notes (expand-file-name "marginalia.org" directory))
             (refreshed-notes (expand-file-name "refresh.org" directory))
             (equivalent-notes (expand-file-name "refresh-alias.org" directory))
             (notes-buffer nil)
             (refreshed-buffer nil)
             (confirmed nil))
        (scholia-org-remark-test--session
         "work"
         (list (scholia-org-remark-test--record
                aaa (list (scholia-db-make-annotation
                           "id-a" "note" 1 6 "alpha")))))
        (with-temp-file notes (insert "old notes\n"))
        (with-temp-file refreshed-notes (insert "old refresh\n"))
        (add-name-to-file refreshed-notes equivalent-notes)
        (setq notes-buffer (find-file-noselect notes))
        (setq refreshed-buffer (find-file-noselect refreshed-notes))
        (unwind-protect
            (progn
              (with-current-buffer notes-buffer
                (erase-buffer)
                (insert "unsaved notes\n"))
              (let ((exported (scholia-org-remark-export "work" notes)))
                (should (equal (with-temp-buffer
                                 (insert-file-contents-literally notes)
                                 (buffer-string))
                               exported)))
              (with-current-buffer notes-buffer
                (should (equal (buffer-string) "unsaved notes\n"))
                (should (buffer-modified-p))
                (should-not (verify-visited-file-modtime))
                (should (equal buffer-file-truename (abbreviate-file-name (file-truename notes))))
                (should (eq notes-buffer
                            (find-buffer-visiting buffer-file-truename))))
              (should-not (get-file-buffer equivalent-notes))
              (should (eq refreshed-buffer
                          (find-buffer-visiting equivalent-notes)))
              (let ((exported (scholia-org-remark-export "work" equivalent-notes)))
                (with-current-buffer refreshed-buffer
                  (should (equal (buffer-string) exported))
                  (should-not (buffer-modified-p))
                  (should (verify-visited-file-modtime))))
              (let ((disk-sentinel "refused disk sentinel\n"))
                (cl-letf (((symbol-function 'userlock--ask-user-about-supersession-threat)
                           (lambda (&rest _))))
                  (with-temp-file notes
                    (insert disk-sentinel)))
                (cl-labels ((refuse (&rest _)
                              (setq confirmed t)
                              nil))
                  (cl-letf (((symbol-function 'completing-read)
                             (lambda (_prompt _collection _predicate require-match &rest _)
                               (should require-match)
                               "work"))
                            ((symbol-function 'read-file-name)
                             (lambda (&rest _) notes))
                            ((symbol-function 'yes-or-no-p) #'refuse)
                            ((symbol-function 'y-or-n-p) #'refuse))
                    (condition-case nil
                        (call-interactively #'scholia-org-remark-export)
                      (user-error nil)
                      (quit nil)))
                  (should confirmed)
                  (should (equal (with-temp-buffer
                                   (insert-file-contents-literally notes)
                                   (buffer-string))
                                 disk-sentinel)))))
          (dolist (buffer (list notes-buffer refreshed-buffer))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))


;;;; Thread bodies

(ert-deftest scholia-org-remark-export-escapes-headline-looking-thread-bodies ()
  "Thread body lines beginning with stars stay body text for org-remark."
  (scholia-org-remark-test--with-sources directory
    (scholia-test-with-session-directory
      (let* ((aaa (expand-file-name "aaa.txt" directory))
             (notes (expand-file-name "marginalia.org" directory))
             (root-body "* root body\n*literal root\n*emphasis* root")
             (reply-body "** reply body\n**literal reply\n**emphasis* reply")
             (exported nil)
             (source nil)
             (notes-buffer nil))
        (scholia-org-remark-test--session
         "work"
         (list (scholia-org-remark-test--record
                aaa (list (scholia-db-make-annotation
                           "id-a" root-body 1 6 "alpha")
                          (scholia-org-remark-test--reply
                           "reply-a" reply-body "id-a")
                          (scholia-db-make-annotation
                           "id-b" "later note" 22 27 "delta")))))
        (setq exported (scholia-org-remark-export "work" notes))
        (unwind-protect
            (let ((org-remark-notes-file-name notes))
              (setq source (find-file-noselect aaa))
              (let ((revert-without-query (list (regexp-quote notes))))
                (setq notes-buffer (find-file-noselect notes)))
              (with-current-buffer source
                (let* ((found (org-remark-highlights-get notes-buffer))
                       (parent (seq-find (lambda (highlight)
                                           (equal "id-a" (plist-get highlight :id)))
                                         found))
                       (later (seq-find (lambda (highlight)
                                          (equal "id-b" (plist-get highlight :id)))
                                        found))
                       (parent-body (plist-get (plist-get parent :props) :body))
                       (later-body (plist-get (plist-get later :props) :body)))
                  (should (string-match-p "^,\\* root body" exported))
                  (should (string-match-p "^\\*literal root" exported))
                  (should-not (string-match-p "^,\\*literal root" exported))
                  (should (string-match-p "^[ \t]+\\*\\*literal reply" exported))
                  (should-not (string-match-p "^[ \t]+,\\*\\*literal reply" exported))
                  (should (equal (sort (mapcar (lambda (highlight)
                                                 (plist-get highlight :id))
                                               found)
                                       #'string<)
                                 '("id-a" "id-b")))
                  (should (string-match-p (regexp-quote root-body) parent-body))
                  (dolist (line (split-string reply-body "\n"))
                    (should (string-match-p (concat "^[ \t]+" (regexp-quote line) "$")
                                            parent-body)))
                  (should (string-match-p (regexp-quote "reply body") parent-body))
                  (should-not (string-match-p (regexp-quote "reply body") later-body)))))
          (dolist (buffer (list source notes-buffer))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))


;;;; Several sessions

(ert-deftest scholia-org-remark-export-takes-one-session-or-several ()
  "Sessions sharing a source file coalesce onto one file headline.
`org-remark-highlights-get' narrows with `org-find-property', which
selects a single subtree, so a second headline naming a file already
present hides every annotation under it.  Two sessions annotating one
file therefore yield one level-one headline carrying both ids, and the
single-session export of the same file carries only its own."
  (scholia-org-remark-test--with-sources directory
    (scholia-test-with-session-directory
      (let ((aaa (expand-file-name "aaa.txt" directory)))
        (scholia-org-remark-test--session
         "work"
         (list (scholia-org-remark-test--record
                aaa (list (scholia-db-make-annotation
                           "id-a" "first note" 1 6 "alpha")))))
        (scholia-org-remark-test--session
         "review"
         (list (scholia-org-remark-test--record
                aaa (list (scholia-db-make-annotation
                           "id-b" "second note" 22 27 "delta")))))
        (let ((both (scholia-org-remark-test--outline
                     (scholia-org-remark-export '("work" "review"))))
              (one (scholia-org-remark-test--outline
                    (scholia-org-remark-export "work"))))
          (should (equal (mapcar #'car both) '(1 2 2)))
          (should (equal (sort (delq nil (mapcar (lambda (entry)
                                                   (nth 2 entry))
                                                 both))
                               #'string<)
                         '("id-a" "id-b")))
          (should (equal (mapcar #'car one) '(1 2)))
          (should (equal (delq nil (mapcar (lambda (entry) (nth 2 entry)) one))
                         '("id-a"))))))))


(ert-deftest scholia-org-remark-export-keeps-same-id-replies-with-their-record ()
  "Records sharing a parent id retain their own thread bodies after coalescing."
  (scholia-org-remark-test--with-sources directory
    (scholia-test-with-session-directory
      (let* ((aaa (expand-file-name "aaa.txt" directory))
             (notes (expand-file-name "marginalia.org" directory))
             (source nil)
             (notes-buffer nil))
        (scholia-org-remark-test--session
         "work"
         (list (scholia-org-remark-test--record
                aaa (list (scholia-db-make-annotation
                           "same-parent" "work root" 1 6 "alpha")
                          (scholia-org-remark-test--reply
                           "work-reply" "work reply" "same-parent")))))
        (scholia-org-remark-test--session
         "review"
         (list (scholia-org-remark-test--record
                aaa (list (scholia-db-make-annotation
                           "same-parent" "review root" 22 27 "delta")
                          (scholia-org-remark-test--reply
                           "review-reply" "review reply" "same-parent")))))
        (scholia-org-remark-export '("work" "review") notes)
        (unwind-protect
            (let ((org-remark-notes-file-name notes))
              (setq source (find-file-noselect aaa))
              (let ((revert-without-query (list (regexp-quote notes))))
                (setq notes-buffer (find-file-noselect notes)))
              (with-current-buffer source
                (let* ((found (org-remark-highlights-get notes-buffer))
                       (first (seq-find (lambda (highlight)
                                          (equal '(1 . 6)
                                                 (plist-get highlight :location)))
                                        found))
                       (second (seq-find (lambda (highlight)
                                           (equal '(22 . 27)
                                                  (plist-get highlight :location)))
                                         found)))
                  (should (= 2 (length found)))
                  (should (string-match-p (regexp-quote "work reply")
                                          (plist-get (plist-get first :props) :body)))
                  (should-not (string-match-p (regexp-quote "review reply")
                                              (plist-get (plist-get first :props) :body)))
                  (should (string-match-p (regexp-quote "review reply")
                                          (plist-get (plist-get second :props) :body)))
                  (should-not (string-match-p (regexp-quote "work reply")
                                              (plist-get (plist-get second :props) :body))))))
          (dolist (buffer (list source notes-buffer))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))

(ert-deftest scholia-org-remark-export-reads-a-session-through-one-snapshot ()
  "Each selected session is read once while org-remark keeps its id bodies."
  (scholia-org-remark-test--with-sources directory
    (scholia-test-with-session-directory
      (let* ((aaa (expand-file-name "aaa.txt" directory))
             (bbb (expand-file-name "bbb.txt" directory))
             (notes (expand-file-name "marginalia.org" directory))
             (source-a nil)
             (source-b nil)
             (notes-buffer nil)
             (reads 0)
             (reading (symbol-function 'scholia-db--reading)))
        (scholia-org-remark-test--session
         "work"
         (list (scholia-org-remark-test--record
                aaa (list (scholia-db-make-annotation
                           "id-a" "first note" 1 6 "alpha")))
               (scholia-org-remark-test--record
                bbb (list (scholia-db-make-annotation
                           "id-b" "second note" 22 27 "delta")))))
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'scholia-db--reading)
                         (lambda (session-file read)
                           (setq reads (1+ reads))
                           (funcall reading session-file read))))
                (scholia-org-remark-export "work" notes))
              (setq source-a (find-file-noselect aaa))
              (setq source-b (find-file-noselect bbb))
              (let ((revert-without-query (list (regexp-quote notes))))
                (setq notes-buffer (find-file-noselect notes)))
              (let ((org-remark-notes-file-name notes))
                (let ((notes-by-id
                       (sort (append
                              (with-current-buffer source-a
                                (mapcar (lambda (highlight)
                                          (cons (plist-get highlight :id)
                                                (string-trim
                                                 (plist-get
                                                  (plist-get highlight :props)
                                                  :body))))
                                        (org-remark-highlights-get notes-buffer)))
                              (with-current-buffer source-b
                                (mapcar (lambda (highlight)
                                          (cons (plist-get highlight :id)
                                                (string-trim
                                                 (plist-get
                                                  (plist-get highlight :props)
                                                  :body))))
                                        (org-remark-highlights-get notes-buffer))))
                             (lambda (a b) (string< (car a) (car b))))))
                  (should (= 1 reads))
                  (should (equal notes-by-id
                                 '(("id-a" . "first note")
                                   ("id-b" . "second note")))))))
          (dolist (buffer (list source-a source-b notes-buffer))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))

(ert-deftest scholia-org-remark-export-interactive-session-requires-a-listed-name ()
  "An unknown interactive session is rejected before export can render it."
  (scholia-org-remark-test--with-sources directory
    (ignore directory)
    (scholia-test-with-session-directory
      (scholia-org-remark-test--session "known" nil)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection _predicate require-match &rest _)
                   (should (equal (all-completions "" collection) '("known")))
                   (should require-match)
                   (user-error "unknown session"))))
        (should-error (call-interactively #'scholia-org-remark-export)
                      :type 'user-error)))))


;;;; What org-remark itself reads back

(ert-deftest scholia-org-remark-export-is-loadable-by-org-remark ()
  "org-remark finds every exported annotation and its own note body.
The oracle here is `org-remark-highlights-get', org-remark's own reader,
which locates the file headline by `org-find-property' and then walks its
children.  A headline hierarchy or a property name org-remark does not
recognize comes back empty however well-formed the Org looks, and a body
attached to the wrong annotation comes back mispaired.

`org-remark-source-file-name' keeps its shipped default,
`file-relative-name', so the exported path is only found when it is the
one `org-remark-source-get-file-name' produces."
  (scholia-org-remark-test--with-sources directory
    (should (featurep 'org-remark))
    (should (cadr (interactive-form 'scholia-org-remark-export)))
    (should (commandp 'scholia-org-remark-export))
    (scholia-test-with-session-directory
      (let* ((aaa (expand-file-name "aaa.txt" directory))
             (notes (expand-file-name "marginalia.org" directory))
             (source nil)
             (notes-buffer nil))
        (scholia-org-remark-test--session
         "work"
         (list (scholia-org-remark-test--record
                aaa (list (scholia-db-make-annotation
                           "id-a" "first note" 1 6 "alpha")
                          (scholia-db-make-annotation
                           "id-b" "second note" 22 27 "delta")))))
        (scholia-org-remark-export "work" notes)
        (should (file-exists-p notes))
        (unwind-protect
            (let ((org-remark-notes-file-name notes))
              (setq source (find-file-noselect aaa))
              (setq notes-buffer (find-file-noselect notes))
              (with-current-buffer source
                (let* ((found (org-remark-highlights-get notes-buffer))
                       (notes-by-id
                        (sort (mapcar
                               (lambda (h)
                                 (cons (plist-get h :id)
                                       (string-trim
                                        (or (plist-get (plist-get h :props)
                                                       :body)
                                            ""))))
                               found)
                              (lambda (a b) (string< (car a) (car b)))))
                       (spans (mapcar (lambda (h) (plist-get h :location))
                                      found)))
                  (should (equal (length found) 2))
                  (should (equal notes-by-id
                                 '(("id-a" . "first note")
                                   ("id-b" . "second note"))))
                  (should (equal (sort spans #'car-less-than-car)
                                 '((1 . 6) (22 . 27)))))))
          (dolist (buffer (list source notes-buffer))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))

(ert-deftest scholia-org-remark-export-materializes-revision-locations-from-committed-source ()
  "A revision location resolves its bounds against its own committed bytes."
  (scholia-org-remark-test--with-sources directory
    (scholia-test-with-session-directory
      (let* ((aaa (expand-file-name "aaa.txt" directory))
             (revision nil)
             (annotation (scholia-db-make-annotation
                          "id-materialized" "revision note" nil nil "delta")))
        (should (zerop (process-file "git" nil nil nil "-C" directory "init" "-q")))
        (should (zerop (process-file "git" nil nil nil "-C" directory "config"
                                     "user.email" "test@example.invalid")))
        (should (zerop (process-file "git" nil nil nil "-C" directory "config"
                                     "user.name" "Scholia Test")))
        (should (zerop (process-file "git" nil nil nil "-C" directory "add" "aaa.txt")))
        (should (zerop (process-file "git" nil nil nil "-C" directory "commit" "-q"
                                     "-m" "revision fixture")))
        (setq revision
              (with-temp-buffer
                (should (zerop (process-file "git" nil (current-buffer) nil "-C"
                                             directory "rev-parse" "HEAD")))
                (string-trim (buffer-string))))
        (with-temp-file aaa
          (insert "working tree only\n"))
        (plist-put annotation :line 2)
        (plist-put annotation :column 10)
        (plist-put annotation :end-column 15)
        (plist-put annotation :revision revision)
        (scholia-org-remark-test--session
         "work" (list (scholia-org-remark-test--record aaa (list annotation))))
        (let ((outline (scholia-org-remark-test--outline
                        (scholia-org-remark-export "work"))))
          (should (equal (nthcdr 2 (nth 1 outline))
                         '("id-materialized" "22" "27"))))))))

(provide 'scholia-org-remark-test)
;;; scholia-org-remark-test.el ends here
