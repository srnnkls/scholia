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

(let ((load-prefer-newer t))
  (require 'scholia-vars nil t)
  (require 'scholia-db nil t)
  (require 'scholia-overlay nil t)
  (require 'scholia-thread nil t)
  (require 'scholia-core nil t)
  (require 'scholia-export nil t))

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

(provide 'scholia-export-test)
;;; scholia-export-test.el ends here
