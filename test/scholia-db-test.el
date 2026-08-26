;;; scholia-db-test.el --- Tests for the scholia record store  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Covers `scholia-db', the versioned plist record store.  Every assertion
;; goes through the db API rather than the stored representation (INV-12).

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'scholia-test-helper)

(let ((load-prefer-newer t))
  (require 'scholia-vars nil t)
  (require 'scholia-db nil t)
  (require 'scholia-store nil t))

(defconst scholia-db-test--source
  "alpha one\nbeta two\ngamma three\ndelta four\n"
  "Buffer contents the file-backed fixtures annotate.")

(defconst scholia-db-test--overlap-source
  "alpha beta gamma delta\n"
  "Single-line contents the merge-helper fixture annotates.")

(defun scholia-db-test--bounds (needle &optional count)
  "Return the cons of start and end of the COUNTth NEEDLE in this buffer.
COUNT defaults to the first occurrence."
  (save-excursion
    (goto-char (point-min))
    (search-forward needle nil nil count)
    (cons (match-beginning 0) (match-end 0))))

(defun scholia-db-test--annotation (id text bounds)
  "Return an annotation with ID and TEXT covering BOUNDS in this buffer."
  (list :id id
        :beg (car bounds)
        :end (cdr bounds)
        :text text
        :annotated-text (buffer-substring-no-properties (car bounds) (cdr bounds))
        :line nil :line-text nil :column nil :end-column nil
        :color 0
        :position :margin
        :reply-to nil
        :sends nil))

(defun scholia-db-test--reply (id text parent-id)
  "Return a reply with ID and TEXT answering the annotation PARENT-ID."
  (list :id id
        :beg nil :end nil
        :text text
        :annotated-text nil
        :line nil :line-text nil :column nil :end-column nil
        :color 0
        :position :margin
        :reply-to parent-id
        :sends nil))

(defun scholia-db-test--session-file (name)
  "Return the path of session NAME inside `scholia-session-directory'."
  (expand-file-name (concat name ".eld") scholia-session-directory))

(defun scholia-db-test--ids (annotations)
  "Return the sorted ids of ANNOTATIONS."
  (sort (mapcar #'scholia-db-annotation-id annotations) #'string<))

(defun scholia-db-test--with-id (id annotations)
  "Return the annotation of ANNOTATIONS carrying ID."
  (seq-find (lambda (annotation)
              (equal (scholia-db-annotation-id annotation) id))
            annotations))

(defun scholia-db-test--annotations (session file)
  "Return the annotations stored for FILE in the session at SESSION."
  (scholia-db-record-annotations
   (scholia-db-record (scholia-db-load session) file)))


;;;; The I/O entry points

(ert-deftest scholia-db-round-trips-annotations-through-a-session-file ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-db-test--source
      (let* ((file (buffer-file-name buffer))
             (session (scholia-db-test--session-file "round-trip"))
             (gamma-bounds (scholia-db-test--bounds "gamma"))
             (gamma (scholia-db-test--annotation "id-gamma" "on gamma" gamma-bounds))
             (delta (scholia-db-test--annotation "id-delta" "on delta"
                                                 (scholia-db-test--bounds "delta"))))
        (scholia-db-save session file (list gamma delta) "checksum-one")
        (should (file-exists-p session))
        (let* ((db (scholia-db-load session))
               (record (scholia-db-record db file))
               (loaded (scholia-db-record-annotations record))
               (restored (scholia-db-test--with-id "id-gamma" loaded)))
          (should (equal (scholia-db-files db) (list file)))
          (should (equal (scholia-db-session-name db) "round-trip"))
          (should (scholia-db-session-created db))
          (should (equal (scholia-db-record-file record) file))
          (should (equal (scholia-db-record-checksum record) "checksum-one"))
          (should (equal (scholia-db-test--ids loaded) '("id-delta" "id-gamma")))
          (should (equal (scholia-db-annotation-text restored) "on gamma"))
          (should (equal (scholia-db-annotation-annotated-text restored) "gamma"))
          (should (equal (cons (scholia-db-annotation-beg restored)
                               (scholia-db-annotation-end restored))
                         gamma-bounds))
          (should (equal (scholia-db-annotation-line restored) 3))
          (should (equal (scholia-db-annotation-line-text restored) "gamma three")))
        (let ((default-directory scholia-session-directory))
          (scholia-db-save "relative.eld" file (list gamma) "checksum-relative")
          (should (file-exists-p (expand-file-name "relative.eld"
                                                   scholia-session-directory)))
          (should (equal (scholia-db-test--ids
                          (scholia-db-test--annotations "relative.eld" file))
                         '("id-gamma"))))))))

(ert-deftest scholia-db-repeated-save-replaces-the-file-record-instead-of-duplicating-it ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-db-test--source
      (let* ((file (buffer-file-name buffer))
             (session (scholia-db-test--session-file "repeated"))
             (gamma (scholia-db-test--annotation "id-gamma" "on gamma"
                                                 (scholia-db-test--bounds "gamma")))
             (delta (scholia-db-test--annotation "id-delta" "on delta"
                                                 (scholia-db-test--bounds "delta"))))
        (scholia-db-save session file (list gamma) "checksum-one")
        (let ((published (scholia-db-load session)))
          (should (scholia-db-session-created published))
          (scholia-db-save session file (list gamma delta) "checksum-two")
          (let* ((db (scholia-db-load session))
                 (record (scholia-db-record db file))
                 (loaded (scholia-db-record-annotations record)))
            (should (equal (scholia-db-files db) (list file)))
            (should (equal (scholia-db-session-name db) "repeated"))
            (should (equal (scholia-db-session-created db)
                           (scholia-db-session-created published)))
            (should (equal (length loaded) 2))
            (should (equal (scholia-db-test--ids loaded) '("id-delta" "id-gamma")))
            (should (equal (scholia-db-record-checksum record) "checksum-two")))
          (let ((put (symbol-function 'scholia-store-put-record))
                (called nil))
            (cl-letf (((symbol-function 'scholia-store-put-record)
                       (lambda (&rest arguments)
                         (setq called t)
                         (apply put arguments)
                         (error "Write interrupted"))))
              (should-error (scholia-db-save session file (list gamma)
                                             "checksum-three"))
              (should called)))
          (let ((record (scholia-db-record (scholia-db-load session) file)))
            (should (equal (scholia-db-test--ids
                            (scholia-db-record-annotations record))
                           '("id-delta" "id-gamma")))
            (should (equal (scholia-db-record-checksum record) "checksum-two"))))))))

(ert-deftest scholia-db-load-refuses-a-session-file-without-a-version-tag ()
  (scholia-test-with-session-directory
    (let ((session (scholia-db-test--session-file "untagged")))
      (with-temp-file session
        (prin1 '(("/tmp/legacy.txt"
                  ((1 5 "note" "alpha" 0 :margin "id-legacy" nil))
                  "checksum"))
               (current-buffer)))
      (should-error (scholia-db-load session) :type 'scholia-db-format-error)
      (with-temp-file session
        (prin1 '(:session (:name "untagged" :created nil :project nil :description nil)
                 :records nil)
               (current-buffer)))
      (should-error (scholia-db-load session) :type 'scholia-db-format-error)
      (with-temp-file session
        (insert "(:scholia 1 :records ((:file \"x\" :annotations ("))
      (should-error (scholia-db-load session) :type 'scholia-db-format-error)
      (with-temp-file session
        (insert "(:scholia 1 :records #1=((:file \"x\") . #1#))"))
      (should-error (scholia-db-load session) :type 'scholia-db-format-error))))

(ert-deftest scholia-db-load-reads-a-zero-length-session-file-as-empty ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-db-test--source
      (let ((file (buffer-file-name buffer))
            (session (scholia-db-test--session-file "interrupted")))
        (write-region "" nil session nil 'silent)
        (should (equal (file-attribute-size (file-attributes session)) 0))
        (should-not (scholia-db-files (scholia-db-load session)))
        (scholia-db-save session file
                         (list (scholia-db-test--annotation
                                "id-gamma" "on gamma"
                                (scholia-db-test--bounds "gamma")))
                         "checksum-one")
        (should (equal (scholia-db-test--ids
                        (scholia-db-test--annotations session file))
                       '("id-gamma")))
        (with-temp-file session (insert "()"))
        (should-error (scholia-db-load session)
                      :type 'scholia-db-format-error)))))


;;;; The source-context snapshot

(ert-deftest scholia-db-save-snapshots-absolute-source-context-from-a-narrowed-buffer ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-db-test--source
      (let* ((file (buffer-file-name buffer))
             (session (scholia-db-test--session-file "narrowed"))
             (beta (scholia-db-test--annotation "id-beta" "on beta"
                                                (scholia-db-test--bounds "beta")))
             (spanning (scholia-db-test--annotation
                        "id-spanning" "on two lines"
                        (scholia-db-test--bounds "two\ngamma")))
             (below (car (scholia-db-test--bounds "delta"))))
        (save-restriction
          (narrow-to-region below (point-max))
          (scholia-db-save session file (list beta spanning) "checksum-one"))
        (delete-file file)
        (let* ((stored (scholia-db-test--annotations session file))
               (loaded (scholia-db-test--with-id "id-beta" stored))
               (multi-line (scholia-db-test--with-id "id-spanning" stored)))
          (should (equal (scholia-db-annotation-line loaded) 2))
          (should (equal (scholia-db-annotation-line-text loaded) "beta two"))
          (should (equal (substring (scholia-db-annotation-line-text loaded)
                                    (scholia-db-annotation-column loaded)
                                    (scholia-db-annotation-end-column loaded))
                         "beta"))
          (should (equal (scholia-db-annotation-line multi-line) 2))
          (should (equal (scholia-db-annotation-line-text multi-line) "beta two"))
          (should (equal (substring (scholia-db-annotation-line-text multi-line)
                                    (scholia-db-annotation-column multi-line)
                                    (scholia-db-annotation-end-column multi-line))
                         "two")))))))

(ert-deftest scholia-db-replies-carry-no-position-and-fold-back-into-a-later-save ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-db-test--source
      (let* ((file (buffer-file-name buffer))
             (session (scholia-db-test--session-file "replies"))
             (gamma (scholia-db-test--annotation "id-gamma" "on gamma"
                                                 (scholia-db-test--bounds "gamma")))
             (reply (scholia-db-test--reply "id-reply" "a reply" "id-gamma")))
        (scholia-db-save session file (list gamma reply) "checksum-one")
        (let* ((stored (scholia-db-test--annotations session file))
               (stored-reply (seq-find #'scholia-db-annotation-reply-p stored)))
          (should (equal (length stored) 2))
          (should (equal (scholia-db-annotation-id stored-reply) "id-reply"))
          (should (equal (scholia-db-annotation-reply-to stored-reply) "id-gamma"))
          (should-not (scholia-db-annotation-beg stored-reply))
          (should-not (scholia-db-annotation-end stored-reply))
          (should-not (scholia-db-annotation-line stored-reply))
          (should-not (scholia-db-annotation-line-text stored-reply))
          (should-not (scholia-db-annotation-column stored-reply))
          (should-not (scholia-db-annotation-end-column stored-reply))
          (scholia-db-save session file (list gamma) "checksum-two")
          (let* ((refolded-all (scholia-db-test--annotations session file))
                 (refolded (seq-find #'scholia-db-annotation-reply-p refolded-all)))
            (should (equal (length refolded-all) 2))
            (should (equal (scholia-db-annotation-id refolded) "id-reply"))
            (should (equal (scholia-db-annotation-text refolded) "a reply"))
            (should (equal (scholia-db-annotation-reply-to refolded) "id-gamma"))
            (should-not (scholia-db-annotation-beg refolded))
            (should-not (scholia-db-annotation-end refolded))
            (should-not (scholia-db-annotation-line refolded))
            (should-not (scholia-db-annotation-line-text refolded))
            (should-not (scholia-db-annotation-column refolded))
            (should-not (scholia-db-annotation-end-column refolded))
            (let ((carried refolded-all))
              (scholia-db-save session file (list gamma) "checksum-three"
                               carried)
              (let ((once (scholia-db-test--annotations session file)))
                (should (equal (scholia-db-test--ids once)
                               '("id-gamma" "id-reply")))
                (scholia-db-save session file (list gamma) "checksum-four"
                                 once)
                (should (equal (scholia-db-test--ids
                                (scholia-db-test--annotations session file))
                               '("id-gamma" "id-reply")))))))))))


(ert-deftest scholia-db-save-files-a-new-reply-after-the-ones-already-stored ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-db-test--source
      (let* ((file (buffer-file-name buffer))
             (session (scholia-db-test--session-file "reply-order"))
             (gamma (scholia-db-test--annotation
                     "id-gamma" "on gamma" (scholia-db-test--bounds "gamma"))))
        (dolist (made '(("id-one" . "ONE")
                        ("id-two" . "TWO")
                        ("id-three" . "THREE")))
          (scholia-db-save session file
                           (list gamma
                                 (scholia-db-test--reply (car made) (cdr made)
                                                         "id-gamma"))
                           "checksum-one"))
        (should (equal (mapcar #'scholia-db-annotation-text
                               (scholia-db-test--annotations session file))
                       '("on gamma" "ONE" "TWO" "THREE")))))))


(ert-deftest scholia-db-save-stamps-a-reply-that-lost-its-parent ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-db-test--source
      (let* ((file (buffer-file-name buffer))
             (session (scholia-db-test--session-file "orphans"))
             (gamma (scholia-db-test--annotation
                     "id-gamma" "on gamma" (scholia-db-test--bounds "gamma")))
             (delta (scholia-db-test--annotation
                     "id-delta" "on delta" (scholia-db-test--bounds "delta")))
             (reply (scholia-db-test--reply "id-reply" "a reply" "id-gamma"))
             (nested (scholia-db-test--reply "id-nested" "deeper" "id-reply"))
             (kept (scholia-db-test--reply "id-kept" "on delta" "id-delta")))
        (scholia-db-save session file
                         (list gamma delta nested reply kept)
                         "checksum-one")
        (let ((stored (scholia-db-test--annotations session file)))
          (should-not (scholia-db-annotation-orphaned-from
                       (scholia-db-test--with-id "id-reply" stored)))
          (should-not (scholia-db-annotation-orphaned-at
                       (scholia-db-test--with-id "id-nested" stored))))
        (scholia-db-save session file (list delta) "checksum-two")
        (let* ((stored (scholia-db-test--annotations session file))
               (orphan (scholia-db-test--with-id "id-reply" stored))
               (deep (scholia-db-test--with-id "id-nested" stored))
               (attached (scholia-db-test--with-id "id-kept" stored)))
          (should (equal (scholia-db-test--ids stored)
                         '("id-delta" "id-kept" "id-nested" "id-reply")))
          (should (equal (scholia-db-annotation-orphaned-from orphan)
                         "id-gamma"))
          (should (equal (scholia-db-annotation-orphaned-from deep)
                         "id-gamma"))
          (should (string-match-p "\\`[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]T"
                                  (scholia-db-annotation-orphaned-at orphan)))
          (should (equal (scholia-db-annotation-orphaned-at deep)
                         (scholia-db-annotation-orphaned-at orphan)))
          (should-not (scholia-db-annotation-orphaned-from attached))
          (should-not (scholia-db-annotation-orphaned-at attached))
          (should (equal (scholia-db-annotation-text orphan) "a reply"))
          (should (equal (scholia-db-annotation-reply-to orphan) "id-gamma"))
          (let ((stamped-at (scholia-db-annotation-orphaned-at orphan)))
            (sleep-for 1)
            (scholia-db-save session file (list delta) "checksum-three")
            (let ((again (scholia-db-test--annotations session file)))
              (should (equal (scholia-db-annotation-orphaned-at
                              (scholia-db-test--with-id "id-reply" again))
                             stamped-at))
              (should (equal (scholia-db-annotation-orphaned-at
                              (scholia-db-test--with-id "id-nested" again))
                             stamped-at))
              (should (equal (scholia-db-annotation-orphaned-from
                              (scholia-db-test--with-id "id-nested" again))
                             "id-gamma")))
            (let ((late (scholia-db-test--reply "id-late" "later"
                                                "id-reply")))
              (scholia-db-save session file (list gamma delta late)
                               "checksum-four")
              (let* ((back (scholia-db-test--annotations session file))
                     (fresh (scholia-db-test--with-id "id-late" back))
                     (historical (scholia-db-test--with-id "id-reply" back)))
                (should (equal (scholia-db-test--ids back)
                               '("id-delta" "id-gamma" "id-kept" "id-late"
                                 "id-nested" "id-reply")))
                (should-not (scholia-db-annotation-orphaned-from fresh))
                (should-not (scholia-db-annotation-orphaned-at fresh))
                (should (equal (scholia-db-annotation-orphaned-at historical)
                               stamped-at))
                (should (equal (scholia-db-annotation-orphaned-from historical)
                               "id-gamma"))))))))))


;;;; The record and annotation surface

(ert-deftest scholia-db-record-surface-looks-up-adds-replaces-and-removes-by-file ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-db-test--source
      (let* ((file (buffer-file-name buffer))
             (other (expand-file-name "other.txt" scholia-session-directory))
             (session (scholia-db-test--session-file "records"))
             (gamma (scholia-db-test--annotation "id-gamma" "on gamma"
                                                 (scholia-db-test--bounds "gamma")))
             (delta (scholia-db-test--annotation "id-delta" "on delta"
                                                 (scholia-db-test--bounds "delta")))
             (db nil))
        (write-region scholia-db-test--source nil other nil 'silent)
        (scholia-db-save session file (list gamma) "checksum-one")
        (setq db (scholia-db-load session))
        (should (equal (scholia-db-record-file (scholia-db-record db file)) file))
        (should-not (scholia-db-record db other))
        (setq db (scholia-db-put-record
                  db (scholia-db-make-record other (list delta) "checksum-other")))
        (should (equal (sort (scholia-db-files db) #'string<)
                       (sort (list file other) #'string<)))
        (should (equal (scholia-db-test--ids
                        (scholia-db-record-annotations (scholia-db-record db other)))
                       '("id-delta")))
        (setq db (scholia-db-put-record
                  db (scholia-db-make-record other (list gamma delta) "checksum-again")))
        (should (equal (length (scholia-db-files db)) 2))
        (should (equal (scholia-db-test--ids
                        (scholia-db-record-annotations (scholia-db-record db other)))
                       '("id-delta" "id-gamma")))
        (should (equal (scholia-db-record-checksum (scholia-db-record db other))
                       "checksum-again"))
        (setq db (scholia-db-remove-record db file))
        (should (equal (scholia-db-files db) (list other)))
        (should-not (scholia-db-record db file))))))

(ert-deftest scholia-db-annotation-accessors-and-keyed-mutators-survive-a-round-trip ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-db-test--source
      (let* ((file (buffer-file-name buffer))
             (session (scholia-db-test--session-file "mutation"))
             (bounds (scholia-db-test--bounds "gamma"))
             (gamma (scholia-db-test--annotation "id-gamma" "on gamma" bounds))
             (reply (scholia-db-test--reply "id-reply" "a reply" "id-gamma")))
        (should (equal (scholia-db-annotation-id gamma) "id-gamma"))
        (should (equal (scholia-db-annotation-beg gamma) (car bounds)))
        (should (equal (scholia-db-annotation-end gamma) (cdr bounds)))
        (should (equal (scholia-db-annotation-text gamma) "on gamma"))
        (should (equal (scholia-db-annotation-annotated-text gamma) "gamma"))
        (should-not (scholia-db-annotation-reply-p gamma))
        (should (scholia-db-annotation-reply-p reply))
        (should (equal (scholia-db-annotation-reply-to reply) "id-gamma"))
        (let* ((shuffled (list :sends nil :reply-to nil :position :margin :color 0
                              :end-column nil :column nil :line-text nil :line nil
                              :annotated-text "gamma" :text "on gamma"
                              :end (cdr bounds) :beg (car bounds) :id "id-shuffled"))
               (renamed (scholia-db-annotation-set-text shuffled "only the text")))
          (should (equal (scholia-db-annotation-id shuffled) "id-shuffled"))
          (should (equal (scholia-db-annotation-beg shuffled) (car bounds)))
          (should (equal (scholia-db-annotation-end shuffled) (cdr bounds)))
          (should (equal (scholia-db-annotation-annotated-text shuffled) "gamma"))
          (should (equal (scholia-db-annotation-text renamed) "only the text"))
          (should (equal (scholia-db-annotation-id renamed) "id-shuffled"))
          (should (equal (scholia-db-annotation-beg renamed) (car bounds)))
          (should (equal (scholia-db-annotation-end renamed) (cdr bounds)))
          (should (equal (scholia-db-annotation-annotated-text renamed) "gamma"))
          (should-not (scholia-db-annotation-reply-p renamed)))
        (let* ((retexted (scholia-db-annotation-set-text gamma "revised note"))
               (moved (scholia-db-annotation-set-bounds retexted 4 9)))
          (should (equal (scholia-db-annotation-text retexted) "revised note"))
          (should (equal (scholia-db-annotation-id retexted) "id-gamma"))
          (should (equal (scholia-db-annotation-beg moved) 4))
          (should (equal (scholia-db-annotation-end moved) 9))
          (should (equal (scholia-db-annotation-text moved) "revised note"))
          (scholia-db-save session file (list moved) "checksum-one")
          (let ((loaded (car (scholia-db-test--annotations session file))))
            (should (equal (scholia-db-annotation-id loaded) "id-gamma"))
            (should (equal (scholia-db-annotation-text loaded) "revised note"))
            (should (equal (scholia-db-annotation-beg loaded) 4))))))))


;;;; The checksum-drift re-search

(ert-deftest scholia-db-buffer-annotations-relocate-within-the-search-window-on-drift ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-db-test--source
      (let* ((file (buffer-file-name buffer))
             (session (scholia-db-test--session-file "drift"))
             (gamma-beg (car (scholia-db-test--bounds "gamma")))
             (gamma (scholia-db-test--annotation "id-gamma" "on gamma"
                                                 (scholia-db-test--bounds "gamma")))
             (beta-beg (car (scholia-db-test--bounds "beta")))
             (beta (scholia-db-test--annotation "id-beta" "on beta"
                                                (scholia-db-test--bounds "beta")))
             (db nil))
        (scholia-db-save session file (list gamma beta) "checksum-one")
        (setq db (scholia-db-load session))
        (let* ((kept (scholia-db-buffer-annotations db file "checksum-one"))
               (unmoved (scholia-db-test--with-id "id-gamma" kept)))
          (should (equal (scholia-db-test--ids kept) '("id-beta" "id-gamma")))
          (should (equal (scholia-db-annotation-beg unmoved) gamma-beg)))
        (save-excursion
          (goto-char (point-min))
          (insert "xx ")
          (search-forward "beta")
          (replace-match "zeta")
          (goto-char (point-max))
          (insert "filler five\nfiller six\nfiller seven\nan echo of beta again\n"))
        (let ((far (save-excursion
                     (goto-char (point-min))
                     (search-forward "beta" nil t))))
          (should far)
          (should (> (- (line-number-at-pos far t) (line-number-at-pos beta-beg t))
                     scholia-search-region-lines-delta)))
        (let* ((unplaced nil)
               (relocated (scholia-db-buffer-annotations
                           db file "checksum-two"
                           (lambda (dropped) (setq unplaced dropped))))
               (moved (car relocated)))
          (should (equal (scholia-db-test--ids relocated) '("id-gamma")))
          (should (equal (buffer-substring-no-properties
                          (scholia-db-annotation-beg moved)
                          (scholia-db-annotation-end moved))
                         "gamma"))
          (should-not (equal (scholia-db-annotation-beg moved) gamma-beg))
          (should (equal (scholia-db-test--ids unplaced) '("id-beta")))
          (should (equal (scholia-db-annotation-line-text (car unplaced))
                         "beta two"))
          (scholia-db-save session file relocated "checksum-two" unplaced)
          (let* ((stored (scholia-db-test--annotations session file))
                 (kept (scholia-db-test--with-id "id-beta" stored)))
            (should (equal (scholia-db-test--ids stored)
                           '("id-beta" "id-gamma")))
            (should (equal (scholia-db-annotation-line kept) 2))
            (should (equal (scholia-db-annotation-line-text kept) "beta two"))
            (should (equal (scholia-db-annotation-annotated-text kept)
                           "beta"))
            (should (equal (scholia-db-record-checksum
                            (scholia-db-record (scholia-db-load session) file))
                           "checksum-one"))
            (let* ((still nil)
                   (reopened (scholia-db-buffer-annotations
                              (scholia-db-load session) file "checksum-two"
                              (lambda (dropped) (setq still dropped)))))
              (should (equal (scholia-db-test--ids reopened) '("id-gamma")))
              (should (equal (scholia-db-test--ids still) '("id-beta")))
              (scholia-db-save session file reopened "checksum-two")
              (should (equal (scholia-db-record-checksum
                              (scholia-db-record (scholia-db-load session)
                                                 file))
                             "checksum-two")))))))))

(ert-deftest scholia-db-drift-re-search-never-mints-a-new-annotation-id ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-db-test--source
      (let* ((file (buffer-file-name buffer))
             (session (scholia-db-test--session-file "stable-id"))
             (gamma (scholia-db-test--annotation "id-gamma" "on gamma"
                                                 (scholia-db-test--bounds "gamma"))))
        (scholia-db-save session file (list gamma) "checksum-one")
        (save-excursion
          (goto-char (point-min))
          (insert "xx "))
        (let ((relocated (scholia-db-buffer-annotations
                          (scholia-db-load session) file "checksum-two")))
          (should (equal (scholia-db-test--ids relocated) '("id-gamma")))
          (scholia-db-save session file relocated "checksum-two")
          (should (equal (scholia-db-test--ids (scholia-db-test--annotations session file))
                         '("id-gamma"))))))))


(ert-deftest scholia-db-drift-re-search-takes-the-hit-nearest-the-stored-position ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer "header\ntarget\nmiddle\ntarget\nfooter\n"
      (let* ((file (buffer-file-name buffer))
             (session (scholia-db-test--session-file "nearest"))
             (first-bounds (scholia-db-test--bounds "target"))
             (second-bounds (scholia-db-test--bounds "target" 2))
             (first (scholia-db-test--annotation "id-first" "on the first" first-bounds))
             (second (scholia-db-test--annotation "id-second" "on the second"
                                                  second-bounds))
             (db nil))
        (scholia-db-save session file (list first second) "checksum-one")
        (setq db (scholia-db-load session))
        (save-excursion
          (goto-char (point-max))
          (insert "an unrelated trailing line\n"))
        (let ((relocated (scholia-db-buffer-annotations db file "checksum-two")))
          (should (equal (scholia-db-test--ids relocated) '("id-first" "id-second")))
          (should (equal (scholia-db-annotation-interval
                          (scholia-db-test--with-id "id-first" relocated))
                         first-bounds))
          (should (equal (scholia-db-annotation-interval
                          (scholia-db-test--with-id "id-second" relocated))
                         second-bounds)))))
    (scholia-test-with-temp-file-buffer buffer "x\nx\nfoo\nbar\n"
      (let* ((file (buffer-file-name buffer))
             (session (scholia-db-test--session-file "past-the-limit"))
             (span (scholia-db-test--bounds "foo\nbar"))
             (spanning (scholia-db-annotation-set-bounds
                        (scholia-db-test--annotation "id-span" "on the span" span)
                        (point-min) (point-min))))
        (should (equal scholia-search-region-lines-delta 2))
        (scholia-db-save session file (list spanning) "checksum-one")
        (let ((relocated (scholia-db-buffer-annotations
                          (scholia-db-load session) file "checksum-two")))
          (should (equal (scholia-db-test--ids relocated) '("id-span")))
          (should (equal (scholia-db-annotation-interval (car relocated)) span)))))
    (scholia-test-with-temp-file-buffer buffer "one\ntwo\nedge\ntarget\nfive\n"
      (let* ((file (buffer-file-name buffer))
             (session (scholia-db-test--session-file "past-the-window"))
             (edge-bounds (scholia-db-test--bounds "edge"))
             (edge (scholia-db-annotation-set-bounds
                    (scholia-db-test--annotation "id-edge" "on the last line in range"
                                                 edge-bounds)
                    (point-min) (point-min)))
             (beyond (scholia-db-annotation-set-bounds
                      (scholia-db-test--annotation "id-beyond" "one line out of range"
                                                   (scholia-db-test--bounds "target"))
                      (point-min) (point-min))))
        (scholia-db-save session file (list edge beyond) "checksum-one")
        (let ((relocated (scholia-db-buffer-annotations
                          (scholia-db-load session) file "checksum-two")))
          (should (equal (scholia-db-test--ids relocated) '("id-edge")))
          (should (equal (scholia-db-annotation-interval (car relocated))
                         edge-bounds)))))))


;;;; The merge helpers session import consumes

(ert-deftest scholia-db-merge-helpers-expand-overlaps-and-leave-disjoint-annotations-alone ()
  (with-temp-buffer
    (insert scholia-db-test--overlap-source)
    (let* ((first-bounds (scholia-db-test--bounds "alpha beta"))
           (second-bounds (scholia-db-test--bounds "beta gamma"))
           (span (scholia-db-test--bounds "alpha beta gamma"))
           (first (scholia-db-test--annotation "id-1" "first note" first-bounds))
           (second (scholia-db-test--annotation "id-2" "second note" second-bounds))
           (third (scholia-db-test--annotation "id-3" "third note"
                                               (scholia-db-test--bounds "delta")))
           (reply (scholia-db-test--reply "id-r" "a reply" "id-1")))
      (should (equal (scholia-db-annotation-interval first) first-bounds))
      (should (equal (scholia-db-merge-interval (scholia-db-annotation-interval first)
                                                (scholia-db-annotation-interval second))
                     span))
      (should (scholia-db-annotations-overlap-p first second))
      (should-not (scholia-db-annotations-overlap-p first third))
      (should-not (scholia-db-annotations-overlap-p first reply))
      (should-not (scholia-db-merge-annotations first third))
      (let ((merged (scholia-db-merge-annotations first second)))
        (should (equal (scholia-db-annotation-id merged) "id-1"))
        (should (equal (cons (scholia-db-annotation-beg merged)
                             (scholia-db-annotation-end merged))
                       span))
        (should (equal (scholia-db-annotation-annotated-text merged) "alpha beta gamma"))
        (should (string-match-p "first note" (scholia-db-annotation-text merged)))
        (should (string-match-p "second note" (scholia-db-annotation-text merged))))
      (let ((collapsed (scholia-db-remove-overlaps (list first second third))))
        (should (equal (length collapsed) 2))
        (should (equal (sort (mapcar #'scholia-db-annotation-annotated-text collapsed)
                             #'string<)
                       '("alpha beta gamma" "delta")))))))

(ert-deftest scholia-db-merge-joins-two-sessions-per-file ()
  (scholia-test-with-session-directory
    (scholia-test-with-temp-file-buffer buffer scholia-db-test--source
      (let* ((file (buffer-file-name buffer))
             (session-a (scholia-db-test--session-file "import-a"))
             (session-b (scholia-db-test--session-file "import-b"))
             (gamma (scholia-db-test--annotation "id-gamma" "on gamma"
                                                 (scholia-db-test--bounds "gamma")))
             (delta (scholia-db-test--annotation "id-delta" "on delta"
                                                 (scholia-db-test--bounds "delta"))))
        (scholia-db-save session-a file (list gamma) "checksum-a")
        (scholia-db-save session-b file (list delta) "checksum-b")
        (scholia-test-with-temp-file-buffer other-buffer scholia-db-test--source
          (let ((other (buffer-file-name other-buffer)))
            (scholia-db-save session-b other
                             (list (scholia-db-test--annotation
                                    "id-other" "on beta"
                                    (scholia-db-test--bounds "beta")))
                             "checksum-other")
            (let ((merged (scholia-db-merge (scholia-db-load session-a)
                                            (scholia-db-load session-b))))
              (should (equal (sort (scholia-db-files merged) #'string<)
                             (sort (list file other) #'string<)))
              (should (equal (scholia-db-test--ids
                              (scholia-db-record-annotations
                               (scholia-db-record merged file)))
                             '("id-delta" "id-gamma")))
              (should (equal (scholia-db-test--ids
                              (scholia-db-record-annotations
                               (scholia-db-record merged other)))
                             '("id-other"))))))))))

(ert-deftest scholia-db-merge-folds-an-overlapping-guest-into-the-host ()
  (let* ((file "/nowhere/shared.txt")
         (host (scholia-db-make-annotation "id-host" "host note" 5 10 "hello"))
         (guest (scholia-db-make-annotation "id-guest" "guest note" 5 12
                                            "hello wor"))
         (reply (scholia-db-make-annotation "id-reply" "under the guest"
                                            nil nil nil nil nil "id-guest"))
         (elsewhere (scholia-db-make-annotation "id-far" "far note" 40 44
                                                "away"))
         (merged (scholia-db-merge
                  (scholia-db-put-record
                   (list :scholia 1 :records nil)
                   (scholia-db-make-record file (list host) "host-sum"))
                  (scholia-db-put-record
                   (list :scholia 1 :records nil)
                   (scholia-db-make-record file (list guest reply elsewhere)
                                           "guest-sum"))))
         (stored (scholia-db-record-annotations (scholia-db-record merged file)))
         (folded (scholia-db-test--with-id "id-host" stored)))
    (should (equal (scholia-db-test--ids stored)
                   '("id-far" "id-host" "id-reply")))
    (should (equal (scholia-db-annotation-interval folded) '(5 . 12)))
    (should (string-match-p "host note" (scholia-db-annotation-text folded)))
    (should (string-match-p "guest note" (scholia-db-annotation-text folded)))
    (should (equal (scholia-db-annotation-annotated-text folded) "hello"))
    (should (equal (scholia-db-annotation-interval
                    (scholia-db-test--with-id "id-far" stored))
                   '(40 . 44)))))

(provide 'scholia-db-test)
;;; scholia-db-test.el ends here
