;;; scholia-export-test.el --- Tests for the scholia export framework  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Covers `scholia-export', where serialized annotations become rustc
;; diagnostics, a unified diff, or a commented copy of the source.  A
;; rustc caret column counts from raw buffer text and stays put under any
;; major mode (INV-6); an integrated annotation lands under the text it
;; annotates whatever the comment syntax (INV-9).

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'scholia-test-helper)

(eval-and-compile
  (setq load-prefer-newer t)
  (require 'scholia-vars nil t)
  (require 'scholia-db nil t)
  (require 'scholia-overlay nil t)
  (require 'scholia-thread nil t)
  (require 'scholia-core nil t)
  (require 'scholia-export nil t))

(eval-when-compile
  (when (bound-and-true-p byte-compile-current-file)
    (require 'scholia-search)
    (require 'scholia-status)))

(declare-function scholia-org-remark-export "scholia-org-remark")
(declare-function scholia-search-annotations "scholia-search")
(declare-function scholia-search-candidate-string "scholia-search")
(declare-function scholia-status "scholia-status")

(defconst scholia-export-test--source
  "alpha beta\n    gamma delta\nepsilon zeta\n"
  "Three annotatable lines.
Line one holds positions 1-10, line two 12-26 and line three 28-39.  The
word \"delta\" of line two spans positions 22-27, columns 10 to 15.")

(defun scholia-export-test--annotation (id text)
  "Return the annotation ID carrying TEXT against \"delta\" of the fixture.
Its source-context snapshot deliberately disagrees with the fixture
buffer: the same word sits at column 10 of \"    gamma delta\" live and at
column 12 of \"int gamma = delta;\" as stored.  A formatter that recomputes
the context from whatever buffer is current instead of reading the
snapshot therefore renders different text and a different column."
  (list :id id
        :text text
        :beg 22 :end 27
        :annotated-text "delta"
        :line 2 :line-text "int gamma = delta;" :column 12 :end-column 17
        :color 0
        :position :margin
        :reply-to nil
        :sends nil))

(defun scholia-export-test--on-line (id line text)
  "Return the annotation ID carrying TEXT against LINE of the fixture.
Its source context agrees with the fixture buffer, which is what an
annotation taken against a file on disk carries and what a patch quoting
that file has to read as."
  (let ((annotation (scholia-export-test--annotation id text)))
    (plist-put annotation :line line)
    (plist-put annotation :line-text
               (nth (1- line)
                    (split-string scholia-export-test--source "\n")))))

(defun scholia-export-test--reply (id text parent-id)
  "Return a reply with ID carrying TEXT and answering PARENT-ID."
  (list :id id
        :text text
        :beg nil :end nil
        :annotated-text nil
        :line nil :line-text nil :column nil :end-column nil
        :color 0
        :position :margin
        :reply-to parent-id
        :sends nil))

(defun scholia-export-test--thread ()
  "Return one annotation carrying two replies, the first of them answered."
  (list (scholia-export-test--annotation "id-a" "check this")
        (scholia-export-test--reply "id-b" "first reply" "id-a")
        (scholia-export-test--reply "id-c" "second reply" "id-a")
        (scholia-export-test--reply "id-d" "nested reply" "id-b")))

(defun scholia-export-test--render-in (mode annotations &optional format)
  "Return ANNOTATIONS rendered as FORMAT against the fixture under MODE."
  (with-temp-buffer
    (insert scholia-export-test--source)
    (funcall mode)
    (scholia-export-render annotations format)))

(defun scholia-export-test--integrate (start end annotations)
  "Return ANNOTATIONS integrated into a fixture commented by START and END."
  (with-temp-buffer
    (insert scholia-export-test--source)
    (setq-local comment-start start)
    (setq-local comment-end end)
    (scholia-export-render annotations 'integrate)))

(defun scholia-export-test--count (needle output)
  "Return how often NEEDLE occurs in OUTPUT."
  (cl-loop with start = 0
           for index = (string-search needle output start)
           while index
           do (setq start (1+ index))
           count t))

(defun scholia-export-test--column (needle output)
  "Return the column NEEDLE begins at on its line of OUTPUT.
Signal an ERT test failure when OUTPUT does not carry NEEDLE."
  (let ((index (string-search needle output)))
    (should index)
    (- index (1+ (or (cl-position ?\n output :end index :from-end t) -1)))))

(defun scholia-export-test--caret-column (output)
  "Return how far past its gutter the caret run of OUTPUT sits.
That distance is the column the diagnostic claims, counted from the
first character of the source line above the carets."
  (should (string-match "| \\( *\\)\\^" output))
  (length (match-string 1 output)))

(defun scholia-export-test--caret-lines (output)
  "Return the lines of OUTPUT carrying a caret run."
  (seq-filter (lambda (line) (string-match-p "\\^" line))
              (split-string output "\n")))

(defun scholia-export-test--hunks (output)
  "Return the hunks of OUTPUT, each its header ahead of its body."
  (let ((hunks nil))
    (dolist (line (split-string (string-trim-right output "\n+") "\n"))
      (cond ((string-prefix-p "@@" line) (push (list line) hunks))
            (hunks (push line (car hunks)))))
    (mapcar #'nreverse (nreverse hunks))))

(defun scholia-export-test--source-lines (output)
  "Return the lines of OUTPUT that carry no `;;' comment."
  (seq-remove (lambda (line) (string-match-p ";;" line))
              (split-string (string-trim-right output "\n+") "\n")))


;;;; Sessions on disk

(defmacro scholia-export-test--with-directory (var &rest body)
  "Evaluate BODY with VAR bound to a fresh directory, deleted afterwards.
Source files a session export reads live there, so nothing the export
touches is a file the user keeps."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (file-name-as-directory (make-temp-file "scholia-source-" t))))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p ,var)
         (delete-directory ,var t)))))

(defun scholia-export-test--write (directory name content)
  "Write CONTENT into NAME under DIRECTORY and return the file name."
  (let ((file (expand-file-name name directory)))
    (with-temp-file file (insert content))
    file))

(defun scholia-export-test--checksum (content)
  "Return the fingerprint a buffer holding CONTENT answers with."
  (with-temp-buffer
    (insert content)
    (scholia-buffer-checksum)))

(defun scholia-export-test--record (file id text content)
  "Return the record for FILE, annotated ID carrying TEXT, taken over CONTENT."
  (scholia-db-make-record file
                          (list (scholia-export-test--annotation id text))
                          (scholia-export-test--checksum content)))

(defun scholia-export-test--session (name &rest records)
  "Create the session NAME carrying RECORDS and return its file."
  (let ((session (scholia-session-file name)))
    (scholia-db-create-session session)
    (dolist (record records)
      (scholia-db-store-record session record))
    session))

(defun scholia-export-test--diagnostic (file output)
  "Return the diagnostic of OUTPUT naming FILE, its `-->' gutter stripped."
  (seq-find (lambda (block) (string-prefix-p file block))
            (split-string output " --> ")))


;;;; The rustc format

(ert-deftest scholia-export-rustc-renders-a-diagnostic-carrying-the-id ()
  (scholia-test-with-temp-file-buffer _buffer scholia-export-test--source
    (should (equal (scholia-export-render
                    (list (scholia-export-test--annotation "id-a" "check this")))
                   (concat " --> " (buffer-file-name) ":2:13 [id-a]\n"
                           "  |\n"
                           "2 | int gamma = delta;\n"
                           "  |             ^^^^^ check this\n"
                           "  |")))))

(ert-deftest scholia-export-rustc-caret-column-does-not-move-with-the-major-mode ()
  (let* ((annotation (scholia-export-test--annotation "id-a" "check this"))
         (elisp (scholia-export-test--render-in #'emacs-lisp-mode (list annotation)))
         (c (scholia-export-test--render-in #'c-mode (list annotation))))
    (should (equal (scholia-export-test--caret-column elisp)
                   (scholia-db-annotation-column annotation)))
    (should (equal (scholia-export-test--caret-column c)
                   (scholia-db-annotation-column annotation)))
    (should (string-match-p ":2:13 " elisp))
    (should (string-match-p ":2:13 " c))
    (should (equal (cdr (split-string elisp "\n"))
                   (cdr (split-string c "\n"))))))


;;;; Chains

(ert-deftest scholia-export-emits-one-diagnostic-per-chain-in-buffer-order ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer _buffer scholia-export-test--source
      (scholia-mode 1)
      (scholia-create-chain 1 6 "first note")
      (let ((spanning (scholia-create-chain 16 32 "spanning note")))
        (should (equal (length spanning) 2))
        (let ((output (scholia-export)))
          (should (equal (scholia-export-test--count "-->" output) 2))
          (should (equal (scholia-export-test--count "spanning note" output) 1))
          (should (equal (length (scholia-export-test--caret-lines output)) 2))
          (should (string-match-p "--> [^\n]+:1:1 \\[[^] \n]+\\]" output))
          (should (string-match-p "--> [^\n]+:2:5 \\[[^] \n]+\\]" output))
          (should (string-match-p
                   "\n2 |     gamma delta\n  |     \\^\\{11\\} spanning note"
                   output))
          (should (< (string-search "first note" output)
                     (string-search "spanning note" output))))))))


;;;; Replies

(ert-deftest scholia-export-nests-replies-under-the-parent-diagnostic ()
  (let* ((annotations (scholia-export-test--thread))
         (rustc (scholia-export-test--render-in #'emacs-lisp-mode annotations))
         (integrated (scholia-export-test--integrate ";;" "" annotations)))
    (should (equal (scholia-export-test--count "-->" rustc) 1))
    (should (equal (length (scholia-export-test--caret-lines rustc)) 1))
    (should-not (string-match-p "\\bnil\\b" rustc))
    (dolist (reply '(("first reply" . "id-b")
                     ("second reply" . "id-c")
                     ("nested reply" . "id-d")))
      (should (equal (scholia-export-test--count (car reply) rustc) 1))
      (should (string-match-p (concat "[^\n]*" (regexp-quote (cdr reply))
                                      "[^\n]*" (regexp-quote (car reply)))
                              rustc))
      (should (equal (scholia-export-test--count (car reply) integrated) 1)))
    (should (< (scholia-export-test--column "check this" rustc)
               (scholia-export-test--column "first reply" rustc)))
    (should (equal (scholia-export-test--column "first reply" rustc)
                   (scholia-export-test--column "second reply" rustc)))
    (should (< (scholia-export-test--column "first reply" rustc)
               (scholia-export-test--column "nested reply" rustc)))
    (should (equal (scholia-export-test--count "~" integrated) 5))))


;;;; The integrate format

(ert-deftest scholia-export-integrate-comments-the-annotation-into-the-source ()
  (let* ((annotation (scholia-export-test--annotation "id-a" "check this"))
         (output (scholia-export-test--integrate ";;" "" (list annotation)))
         (lines (split-string (string-trim-right output "\n+") "\n")))
    (should (equal (scholia-export-test--source-lines output)
                   '("alpha beta" "    gamma delta" "epsilon zeta")))
    (should (equal (nth 1 lines) "    gamma delta"))
    (should (string-match-p "\\`\\( *\\);;~\\{5\\}\\'" (nth 2 lines)))
    (should (equal (scholia-export-test--count "~" output)
                   (- (scholia-db-annotation-end-column annotation)
                      (scholia-db-annotation-column annotation))))
    (should (equal (scholia-export-test--count "id-a" output) 1))
    (should (< (string-search "id-a" output)
               (string-search "check this" output)))
    (should (< (string-search "check this" output)
               (string-search "epsilon zeta" output)))))

(ert-deftest scholia-export-integrate-pads-past-the-comment-prefix ()
  (let* ((annotation (scholia-export-test--annotation "id-a" "check this"))
         (column (scholia-db-annotation-column annotation))
         (annotations (list annotation))
         (semicolons (scholia-export-test--integrate ";;" "" annotations))
         (block-comment (scholia-export-test--integrate "/* " " */" annotations)))
    (should (equal (scholia-export-test--column "check this" semicolons)
                   (scholia-export-test--column "check this" block-comment)))
    (should (equal (scholia-export-test--column "check this" semicolons) column))
    (should (equal (scholia-export-test--column "~~~~~" semicolons) column))
    (should (equal (scholia-export-test--column "~~~~~" block-comment) column))
    (should (string-match-p "~~~~~ \\*/" block-comment))
    (should (string-match-p "check this \\*/" block-comment))))


;;;; The diff format

(ert-deftest scholia-export-diff-adds-the-annotation-as-commented-lines ()
  (scholia-test-with-temp-file-buffer _buffer scholia-export-test--source
    (setq-local comment-start ";;")
    (setq-local comment-end "")
    (let* ((file (buffer-file-name))
           (output (scholia-export-render
                    (list (scholia-export-test--annotation "id-a" "check this"))
                    'diff))
           (lines (split-string (string-trim-right output "\n+") "\n"))
           (body (nthcdr 3 lines))
           (added (seq-filter (lambda (line) (string-prefix-p "+" line)) body)))
      (should (equal (nth 0 lines) (concat "--- " file)))
      (should (equal (nth 1 lines) (concat "+++ " file)))
      (should (string-match "\\`@@ -2,2 \\+2,\\([0-9]+\\) @@\\'" (nth 2 lines)))
      (should (equal (string-to-number (match-string 1 (nth 2 lines)))
                     (+ 2 (length added))))
      (should (equal (nth 3 lines) " int gamma = delta;"))
      (should (seq-every-p (lambda (line) (memq (aref line 0) '(?\s ?+ ?-))) body))
      (should (seq-every-p (lambda (line) (string-match-p ";;" line)) added))
      (should (seq-find (lambda (line) (string-match-p "check this" line)) added))
      (should (seq-find (lambda (line) (string-match-p "id-a" line)) added)))))

(ert-deftest scholia-export-diff-quotes-the-line-after-what-it-annotates ()
  (let* ((apart (scholia-export-test--render-in
                 #'emacs-lisp-mode
                 (list (scholia-export-test--on-line "id-a" 1 "first")
                       (scholia-export-test--on-line "id-c" 3 "third"))
                 'diff))
         (hunks (scholia-export-test--hunks apart))
         (neighbours (scholia-export-test--hunks
                      (scholia-export-test--render-in
                       #'emacs-lisp-mode
                       (list (scholia-export-test--on-line "id-a" 1 "first")
                             (scholia-export-test--on-line "id-b" 2 "second"))
                       'diff))))
    (should (equal (mapcar #'car hunks) '("@@ -1,2 +1,5 @@" "@@ -3,1 +6,4 @@")))
    (should (equal (car (last (nth 0 hunks))) "     gamma delta"))
    (should (equal (nth 1 (nth 0 hunks)) " alpha beta"))
    (should (string-prefix-p "+" (car (last (nth 1 hunks)))))
    (should (equal (nth 1 (nth 1 hunks)) " epsilon zeta"))
    (should (equal (mapcar #'car neighbours) '("@@ -1,3 +1,9 @@")))
    (should (equal (seq-filter (lambda (line) (string-prefix-p " " line))
                               (cdr (car neighbours)))
                   '(" alpha beta" "     gamma delta" " epsilon zeta")))))


;;;; Dispatch

(ert-deftest scholia-export-dispatches-through-scholia-export-functions ()
  (let* ((annotations (list (scholia-export-test--annotation "id-a" "check this")))
         (calls nil)
         (probe (lambda (candidates &optional file-or-buffer)
                  (push (cons candidates file-or-buffer) calls)
                  "probe output")))
    (dolist (format '(rustc diff integrate))
      (let ((formatter (alist-get format scholia-export-functions)))
        (should (functionp formatter))
        (should (equal (func-arity formatter) '(1 . 2)))))
    (let ((scholia-export-functions (cons (cons 'probe probe) scholia-export-functions))
          (scholia-export-format 'probe))
      (should (equal (scholia-export-render annotations) "probe output"))
      (should (equal (car calls) (cons annotations nil)))
      (should (equal (scholia-export-render annotations 'probe "src/thing.el")
                     "probe output"))
      (should (equal (car calls) (cons annotations "src/thing.el")))
      (should-not (equal (scholia-export-render annotations 'rustc) "probe output")))))

(ert-deftest scholia-export-unregistered-format-fails-by-name ()
  (let ((annotations (list (scholia-export-test--annotation "id-a" "check this"))))
    (should-error (scholia-export-render annotations 'no-such-format)
                  :type 'scholia-export-unknown-format)
    (let ((scholia-export-format 'no-such-format))
      (should-error (scholia-export-render annotations)
                    :type 'scholia-export-unknown-format))
    (should (string-match-p
             "no-such-format"
             (error-message-string
              (should-error (scholia-export-render annotations 'no-such-format)))))))


;;;; Targets

(ert-deftest scholia-export-writes-to-a-buffer-a-file-and-the-kill-ring ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer _buffer scholia-export-test--source
      (scholia-mode 1)
      (scholia-create-chain 22 27 "check this")
      (goto-char 22)
      (scholia-reply-to "the answer")
      (let* ((kill-ring nil)
             (interprogram-cut-function nil)
             (interprogram-paste-function nil)
             (target (make-temp-file "scholia-export-" nil ".txt"))
             (expected (scholia-export)))
        (unwind-protect
            (progn
              (should (string-match-p "check this" expected))
              (should (string-match-p "the answer" expected))
              (should-not (get-buffer "*scholia-export*"))
              (should-not kill-ring)
              (should (equal (scholia-export 'kill-ring) expected))
              (should (equal (current-kill 0) expected))
              (should (equal (scholia-export target) expected))
              (should (equal (with-temp-buffer
                               (insert-file-contents target)
                               (buffer-string))
                             expected))
              (should (equal (scholia-export 'buffer) expected))
              (should (equal (with-current-buffer "*scholia-export*" (buffer-string))
                             expected)))
          (when (get-buffer "*scholia-export*")
            (kill-buffer "*scholia-export*"))
          (when (file-exists-p target)
            (delete-file target)))))))


;;;; What the payload reads

(ert-deftest scholia-export-carries-a-reply-that-reached-the-store-from-outside ()
  "An export folds in a reply no chain on screen holds.
A reply is kept by the record alone, so an export built from the buffer
drops every one, and the reply an agent made by id into the live session
while Emacs was open is exactly the reply this export is supposed to
carry.  A payload reading the interchange path instead of the store finds
something that is not the session at all and comes back empty."
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-export-test--source
      (scholia-mode 1)
      (scholia-create-chain 22 27 "check this")
      (scholia-save-annotations)
      (let* ((session (scholia-session-file))
             (file (buffer-file-name buffer))
             (stored (scholia-db-record-annotations
                      (scholia-db-record session file)))
             (parent (scholia-db-annotation-id (car stored)))
             (output nil))
        (should (equal (length stored) 1))
        (should parent)
        (scholia-db-add-reply
         session file
         (scholia-export-test--reply "id-outside" "from the agent" parent))
        (should-not (seq-find #'scholia-db-annotation-reply-p
                              (scholia-core--buffer-annotations)))
        (setq output (scholia-export))
        (should (equal (scholia-export-test--count "from the agent" output) 1))
        (should (equal (scholia-export-test--count "check this" output) 1))
        (should (equal (scholia-export-test--count "-->" output) 1))))))

;;;; Session-scoped export


(ert-deftest scholia-export-session-renders-live-first-three-ways ()
  "A session export prefers the file on disk and says when it could not.
Every annotation carries the same snapshot, which disagrees with the
fixture on disk, so a diagnostic quoting \"int gamma = delta;\" read the
snapshot and one quoting \"    gamma delta\" read the file.  The drifted
file repeats the annotated text in the line prepended to it, so a plain
scan lands on line one where SCH-004's windowed nearest-match keeps the
diagnostic on line three.  The merged file carries what
`scholia-db-widen-over' leaves behind, a matching checksum over an
`:annotated-text' shorter than the interval, so relocating without
reading the checksum shrinks the caret run to the shorter match.  The
file the annotated text was cut from is readable, mismatched and has
nowhere to relocate to, and the deleted file is not readable at all, so
a missing source degrades its own diagnostic and leaves the rest alone."
  (scholia-test-with-session-directory
    (scholia-export-test--with-directory directory
      (let* ((present (scholia-export-test--write
                       directory "present.txt" scholia-export-test--source))
             (drifted (scholia-export-test--write
                       directory "drifted.txt"
                       (concat "# delta notes\n" scholia-export-test--source)))
             (merged (scholia-export-test--write
                      directory "merged.txt" scholia-export-test--source))
             (cut (scholia-export-test--write
                   directory "cut.txt"
                   "alpha live\n    gamma\nepsilon zeta\n"))
             (gone (expand-file-name "gone.txt" directory))
             (source scholia-export-test--source)
             (relocated (list :id "id-f"
                              :text "live note"
                              :beg 1 :end 6
                              :annotated-text "alpha"
                              :line 1 :line-text "alpha beta" :column 0 :end-column 5
                              :color 0
                              :position :margin
                              :reply-to nil
                              :sends nil))
             (widened (scholia-db-annotation-set-bounds
                       (scholia-export-test--annotation "id-d" "widened note")
                       16 27)))
        (scholia-export-test--session
         "live"
         (scholia-export-test--record present "id-a" "check this" source)
         (scholia-export-test--record drifted "id-b" "moved note" source)
         (scholia-export-test--record gone "id-c" "lost note" source)
         (scholia-db-make-record merged (list widened)
                                 (scholia-export-test--checksum source))
         (scholia-db-make-record
          cut
          (list relocated (scholia-export-test--annotation "id-e" "cut note"))
          (scholia-export-test--checksum source)))
        (let* ((output (scholia-export-session "live"))
               (integrated (scholia-export-session "live" nil 'integrate))
               (fresh (scholia-export-test--diagnostic present output))
               (moved (scholia-export-test--diagnostic drifted output))
               (wide (scholia-export-test--diagnostic merged output))
               (lost (scholia-export-test--diagnostic gone output)))
          (should fresh)
          (should moved)
          (should wide)
          (should lost)
          (should (string-prefix-p (concat present ":2:11 [id-a]") fresh))
          (should (string-match-p "\n2 |     gamma delta\n" fresh))
          (should-not (string-match-p "int gamma = delta;" fresh))
          (should (equal (scholia-export-test--caret-column fresh) 10))
          (should-not (string-match-p "stale" fresh))
          (should (string-prefix-p (concat drifted ":3:11 [id-b]") moved))
          (should (string-match-p "\n3 |     gamma delta\n" moved))
          (should-not (string-match-p "int gamma = delta;" moved))
          (should-not (string-match-p "stale" moved))
          (should (string-prefix-p (concat merged ":2:5 [id-d]") wide))
          (should (equal (scholia-export-test--count "^" wide)
                         (- (scholia-db-annotation-end widened)
                            (scholia-db-annotation-beg widened))))
          (should-not (string-match-p "stale" wide))
          (should (string-match-p
                   (concat (regexp-quote cut) ":1:1 " (regexp-quote "[id-f]")
                           "\n  |\n1 | alpha live\n")
                   output))
          (should-not (string-match-p
                       (concat (regexp-quote cut) ":1:1 " (regexp-quote "[id-f]")
                               "\n.*\n.*\n.*stale")
                       output))
          (should (string-match-p
                   (concat (regexp-quote cut) ":2:13 " (regexp-quote "[id-e]")
                           "\n  |\n2 | int gamma = delta;\n")
                   output))
          (should (string-match-p
                   (concat (regexp-quote cut) ":2:13 " (regexp-quote "[id-e]")
                           "\n.*\n.*\n.*stale")
                   output))
          (should (string-prefix-p (concat gone ":2:13 [id-c]") lost))
          (should (string-match-p "\n2 | int gamma = delta;\n" lost))
          (should (string-match-p "stale" lost))
          (should (string-match-p (concat "Access: " (regexp-quote present) " \\[live\\]")
                                  output))
          (should (string-match-p (concat "Access: " (regexp-quote drifted) " \\[live\\]")
                                  output))
          (should (string-match-p (concat "Access: " (regexp-quote merged) " \\[live\\]")
                                  output))
          (should (string-match-p (concat "Access: " (regexp-quote cut) " \\[excerpt\\]")
                                  integrated))
          (should (string-match-p (concat "Access: " (regexp-quote gone) " \\[excerpt\\]")
                                  output))
          (should (string-match-p
                   (regexp-quote "alpha live\n#~~~~~\n#[id-f]\n#live note")
                   integrated))
          (should (string-match-p
                   (regexp-quote
                    "int gamma = delta;\n           #~~~~~\n           #[id-e]\n           #cut note (stale)")
                   integrated)))))))

(ert-deftest scholia-export-session-reads-the-file-not-the-buffer-visiting-it ()
  "The rendering carries the text on disk while a buffer holds edits on top.
`find-file-noselect' would hand back the visiting buffer, so an export
built on it emits an unsaved edit as though it were the file and
fingerprints a state nobody else can see."
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-export-test--source
      (let ((file (buffer-file-name buffer)))
        (scholia-export-test--session
         "unsaved"
         (scholia-export-test--record file "id-a" "check this"
                                      scholia-export-test--source))
        (goto-char (point-min))
        (should (search-forward "gamma" nil t))
        (replace-match "sigma")
        (should (buffer-modified-p buffer))
        (let ((output (scholia-export-session "unsaved")))
          (should (string-match-p (concat (regexp-quote file) ":2:11 ") output))
          (should (string-match-p "\n2 |     gamma delta\n" output))
          (should-not (string-match-p "sigma" output))
          (should-not (string-match-p "stale" output))
          (should (buffer-modified-p buffer)))))))

(ert-deftest scholia-export-session-nests-replies-under-the-parent ()
  "A reply holds no position, so it renders inside its parent or nowhere.
The file drifted, so the replies travel the relocating path with their
parent.  Nothing about a reply is there to recompute from disk, which is
where one gets dropped or handed a diagnostic of its own."
  (scholia-test-with-session-directory
    (scholia-export-test--with-directory directory
      (let ((file (scholia-export-test--write
                   directory "source.txt"
                   (concat "# header\n" scholia-export-test--source))))
        (scholia-export-test--session
         "threaded"
         (scholia-db-make-record
          file
          (list (scholia-export-test--annotation "id-a" "check this")
                (scholia-export-test--reply "id-b" "first reply" "id-a")
                (scholia-export-test--reply "id-c" "second reply" "id-a"))
          (scholia-export-test--checksum scholia-export-test--source)))
        (let ((output (scholia-export-session "threaded")))
          (should (equal (scholia-export-test--count "-->" output) 1))
          (should (string-match-p (concat (regexp-quote file) ":3:11 ") output))
          (should (equal (length (scholia-export-test--caret-lines output)) 1))
          (should (equal (scholia-export-test--count "first reply" output) 1))
          (should (equal (scholia-export-test--count "second reply" output) 1))
          (should (< (scholia-export-test--column "check this" output)
                     (scholia-export-test--column "first reply" output)))
          (should-not (string-match-p "\\bnil\\b" output)))))))

(ert-deftest scholia-export-session-orders-by-session-then-file ()
  "Several sessions render one after another, each naming itself.
The files are dealt out so that ordering them by name alone crosses the
session boundary: a rendering grouped by session reads f2 f4 f1 f3, one
sorted globally reads f1 f2 f3 f4."
  (scholia-test-with-session-directory
    (scholia-export-test--with-directory directory
      (let* ((source scholia-export-test--source)
             (files (mapcar (lambda (name)
                              (scholia-export-test--write
                               directory name source))
                            '("f1.txt" "f2.txt" "f3.txt" "f4.txt")))
             (markers nil))
        (scholia-export-test--session
         "sess-uno"
         (scholia-export-test--record (nth 1 files) "id-b" "note b" source)
         (scholia-export-test--record (nth 3 files) "id-d" "note d" source))
        (scholia-export-test--session
         "sess-duo"
         (scholia-export-test--record (nth 0 files) "id-a" "note a" source)
         (scholia-export-test--record (nth 2 files) "id-c" "note c" source))
        (let ((output (scholia-export-session '("sess-uno" "sess-duo"))))
          (setq markers (list "sess-uno" (nth 1 files) (nth 3 files)
                              "sess-duo" (nth 0 files) (nth 2 files)))
          (dolist (marker markers)
            (should (string-search marker output)))
          (should (apply #'<
                         (mapcar (lambda (marker) (string-search marker output))
                                 markers))))))))

(ert-deftest scholia-export-session-puts-a-rendering-where-the-target-points ()
  "The rendering comes back whatever the target, and the format is a caller's."
  (scholia-test-with-session-directory
    (scholia-export-test--with-directory directory
      (let ((file (scholia-export-test--write
                   directory "source.txt" scholia-export-test--source)))
        (scholia-export-test--session
         "targets"
         (scholia-export-test--record file "id-a" "check this"
                                      scholia-export-test--source))
        (let* ((kill-ring nil)
               (interprogram-cut-function nil)
               (interprogram-paste-function nil)
               (target (expand-file-name "export.txt" directory))
               (expected (scholia-export-session "targets")))
          (unwind-protect
              (progn
                (should (string-match-p "check this" expected))
                (should-not (get-buffer scholia-export-buffer-name))
                (should-not kill-ring)
                (should (equal (scholia-export-session "targets" 'kill-ring)
                               expected))
                (should (equal (current-kill 0) expected))
                (should (equal (scholia-export-session "targets" target)
                               expected))
                (should (equal (with-temp-buffer
                                 (insert-file-contents target)
                                 (buffer-string))
                               expected))
                (should (equal (scholia-export-session "targets" 'buffer)
                               expected))
                (should (equal (with-current-buffer scholia-export-buffer-name
                                 (buffer-string))
                               expected))
                (let ((diff (scholia-export-session "targets" nil 'diff)))
                  (should (string-prefix-p
                           (concat "Access: " file " [live]\n\n--- " file)
                           diff))
                  (should-not (string-match-p "int gamma = delta;" diff))))
            (when (get-buffer scholia-export-buffer-name)
              (kill-buffer scholia-export-buffer-name))))))))


(ert-deftest scholia-export-session-keeps-source-comments-and-missing-roots ()
  (scholia-test-with-session-directory
    (scholia-export-test--with-directory directory
      (let* ((source scholia-export-test--source)
             (present (scholia-export-test--write directory "present.el" source))
             (gone (expand-file-name "gone.el" directory))
             (missing (scholia-export-test--on-line "id-gone" 3 "lost note")))
        (scholia-export-test--session
         "sources"
         (scholia-export-test--record present "id-present" "live note" source)
         (scholia-db-make-record
          gone (list missing (scholia-export-test--reply "id-reply" "nested" "id-gone"))
          (scholia-export-test--checksum source)))
        (dolist (format '(integrate diff))
          (let ((output (scholia-export-session "sources" nil format)))
            (should (string-match-p ";.*live note" output))
            (should-not (string-match-p "#.*live note" output))
            (should (string-match-p "epsilon zeta" output))
            (should (string-match-p "lost note (stale)" output))
            (should (string-match-p "nested" output))))
        (let ((output (scholia-export-session "sources")))
          (should (string-match-p (concat (regexp-quote gone) ":3:13 \\[id-gone\\]")
                                  output))
          (should (string-match-p "3 | epsilon zeta" output))
          (should (string-match-p "lost note (stale)" output)))))))

(ert-deftest scholia-export-consumers-render-the-stored-revision ()
  "Export, Org, status, and search retain a revision annotation's identity."
  (scholia-test-with-session-directory
    (scholia-export-test--with-directory directory
      (let* ((file (scholia-export-test--write directory "revision.txt"
                                               scholia-export-test--source))
             (revision "0123456789abcdef0123456789abcdef01234567")
             (annotation (scholia-export-test--annotation "revision-id"
                                                          "revision note"))
             (record nil))
        (plist-put annotation :revision revision)
        (setq record (scholia-db-make-record
                      file (list annotation)
                      (scholia-export-test--checksum scholia-export-test--source)))
        (scholia-export-test--session "revision" record)
        (require 'scholia-search)
        (let ((status-loaded (featurep 'scholia-status)))
          (unwind-protect
              (progn
                (require 'scholia-status)
                (require 'scholia-org-remark)
                (let ((entry (car (scholia-search-annotations)))
                      (org (scholia-org-remark-export "revision")))
                  (should (string-match-p revision
                                          (scholia-export-render (list annotation))))
                  (should (string-match-p revision
                                          (scholia-search-candidate-string entry)))
                  (should (string-match-p revision org))
                  (let ((buffer (scholia-status)))
                    (unwind-protect
                        (with-current-buffer buffer
                          (should (string-match-p revision (buffer-string))))
                      (when (buffer-live-p buffer)
                        (kill-buffer buffer))))))
            (unless status-loaded
              (unload-feature 'scholia-status t))))))))

(ert-deftest scholia-export-session-reads-each-session-once ()
  (scholia-test-with-session-directory
    (scholia-export-test--with-directory directory
      (let* ((source scholia-export-test--source)
             (first (scholia-export-test--write directory "first.txt" source))
             (second (scholia-export-test--write directory "second.txt" source))
             (original (symbol-function 'scholia-db--reading))
             (reads 0))
        (scholia-export-test--session
         "one-read"
         (scholia-export-test--record first "id-a" "first" source)
         (scholia-export-test--record second "id-b" "second" source))
        (cl-letf (((symbol-function 'scholia-db--reading)
                   (lambda (&rest arguments)
                     (setq reads (1+ reads))
                     (apply original arguments))))
          (scholia-export-session "one-read"))
        (should (= reads 1))))))

(ert-deftest scholia-export-session-interactively-completes-session-names ()
  (let ((arguments nil))
    (cl-letf (((symbol-function 'scholia-session-list)
               (lambda () '("known")))
              ((symbol-function 'completing-read)
               (lambda (&rest input)
                 (setq arguments input)
                 "known"))
              ((symbol-function 'scholia-export--session-payload)
               (lambda (&rest _) "output"))
              ((symbol-function 'scholia-export--show)
               (lambda (&rest _))))
      (call-interactively #'scholia-export-session))
    (should (equal (nth 1 arguments) '("known")))
    (should (nth 3 arguments))))

(ert-deftest scholia-multisession-export-selects-visible-owners-and-subdues-replies ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-export-test--source
      (let* ((globals '(scholia-session scholia-active-sessions scholia-autosave))
             (snapshot (mapcar (lambda (symbol)
                                 (list symbol
                                       (boundp symbol)
                                       (and (boundp symbol) (default-value symbol))))
                               globals))
             (file (buffer-file-name buffer))
             (checksum (scholia-buffer-checksum))
             (alpha (scholia-export-test--annotation "alpha-root" "alpha visible"))
             (beta (plist-put
                    (scholia-export-test--annotation "beta-root" "beta visible")
                    :color 2))
             (reply (scholia-export-test--reply
                     "beta-reply" "beta reply" "beta-root"))
             (prompted 0)
             offered)
        (unwind-protect
            (progn
              (set-default 'scholia-session "alpha")
              (set-default 'scholia-active-sessions '("beta"))
              (set-default 'scholia-autosave nil)
              (scholia-db-save (scholia-session-file "alpha") file
                               (list alpha) checksum)
              (scholia-db-save (scholia-session-file "beta") file
                               (list beta reply) checksum)
              (scholia-mode 1)
              (let* ((output (scholia-export))
                     (path (regexp-quote file)))
                (should (string-match-p "Session: alpha" output))
                (should (string-match-p "Session: beta" output))
                (should (string-match-p "alpha visible" output))
                (should (string-match-p "beta visible" output))
                (should (string-match-p "beta reply" output))
                (should (string-match-p
                         (format "\\(?:%s[^\n]*live\\|live[^\n]*%s\\)" path path)
                         output)))
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (_prompt collection &rest _)
                           (setq prompted (1+ prompted)
                                 offered (all-completions "" collection))
                           (or (seq-find (lambda (candidate)
                                           (string-match-p "beta" candidate))
                                         offered)
                               (error "No beta export owner")))))
                (let* ((current-prefix-arg '(4))
                       (selected
                        (progn
                          (call-interactively #'scholia-export)
                          (with-current-buffer scholia-export-buffer-name
                            (buffer-string)))))
                  (should (= prompted 1))
                  (should (seq-some (lambda (candidate)
                                      (string-match-p "alpha" candidate))
                                    offered))
                  (should (string-match-p "Session: beta" selected))
                  (should (string-match-p "beta visible" selected))
                  (should-not (string-match-p "alpha visible" selected))))
              (let ((scholia-annotation-text-faces
                     '((:foreground "red" :weight bold)
                       (:foreground "green" :weight bold)
                       (:foreground "blue" :weight bold))))
                (cl-labels
                    ((attribute (face name)
                       (cond
                        ((and (listp face) (plist-member face name))
                         (plist-get face name))
                        ((symbolp face)
                         (face-attribute face name nil 'default))
                        ((listp face)
                         (seq-some (lambda (part)
                                     (let ((value (attribute part name)))
                                       (unless (memq value '(nil unspecified)) value)))
                                   face)))))
                  (with-temp-buffer
                    (scholia-thread-render (list beta reply))
                    (goto-char (point-min))
                    (should (search-forward "beta visible" nil t))
                    (let ((root-face (button-get
                                      (button-at (match-beginning 0)) 'face)))
                      (goto-char (point-min))
                      (should (search-forward "beta reply" nil t))
                      (let ((reply-face (button-get
                                         (button-at (match-beginning 0)) 'face)))
                        (should (equal (attribute root-face :foreground) "blue"))
                        (should (equal (attribute reply-face :foreground) "blue"))
                        (should (eq (attribute root-face :weight) 'bold))
                        (should-not (eq (attribute reply-face :weight) 'bold))
                        (should-not (equal root-face reply-face))))))))
          (when (get-buffer scholia-export-buffer-name)
            (kill-buffer scholia-export-buffer-name))
          (dolist (entry snapshot)
            (if (nth 1 entry)
                (set-default (nth 0 entry) (nth 2 entry))
              (makunbound (nth 0 entry)))))))))

(provide 'scholia-export-test)
;;; scholia-export-test.el ends here
