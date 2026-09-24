;;; scholia-herdr-test.el --- Tests for Herdr dispatch -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'scholia-test-helper)

(eval-and-compile
  (setq load-prefer-newer t)
  (require 'scholia-vars)
  (require 'scholia-db)
  (require 'scholia-core)
  (require 'scholia-export)
  (require 'scholia-herdr))

(define-error 'scholia-herdr-test-refused "The target refused the payload")

(defconst scholia-herdr-test--source
  "alpha beta\ngamma delta\nepsilon zeta\neta theta\n")

(defmacro scholia-herdr-test--with-buffer (&rest body)
  (declare (indent 0) (debug body))
  `(scholia-test-with-session-directory
     (scholia-test-with-temp-file-buffer _buffer scholia-herdr-test--source
       (setq-local scholia-session "selected")
       (scholia-mode 1)
       ,@body)))

(defun scholia-herdr-test--sends ()
  (mapcar #'scholia-db-annotation-sends
          (scholia-db-record-annotations
           (scholia-db-record (scholia-session-file) (buffer-file-name)))))

(ert-deftest scholia-herdr-send-at-point-uses-the-default-agent-and-session-socket ()
  (scholia-herdr-test--with-buffer
    (scholia-create-chain 1 6 "alpha")
    (goto-char 2)
    (let ((call nil)
          (calls 0)
          (herdr-socket-path "/stale/herdr.sock")
          (scholia-herdr-send-format 'integrate)
          (scholia-herdr-default-target "agent-1"))
      (cl-letf (((symbol-function 'herdr-agent-sessions)
                 (lambda ()
                   (list '((kind . "herdr") (session . "herdr-selected")
                           (agent . "claude") (name . "agent-1")
                           (label . "Agent One")))))
                ((symbol-function 'herdr-panes) (lambda () nil))
                ((symbol-function 'herdr-read-entry)
                 (lambda (&rest _) (error "The default must skip the picker")))
                ((symbol-function 'herdr-api-agent-prompt)
                 (lambda (target text &rest _)
                   (cl-incf calls)
                   (setq call (list target text herdr-session herdr-socket-path))))
                ((symbol-function 'herdr-api-pane-send-text)
                 (lambda (&rest _) (error "The agent target used a pane API"))))
        (scholia-herdr-send))
      (should (= calls 1))
      (should (equal (car call) "agent-1"))
      (should (string-match-p "alpha" (nth 1 call)))
      (should (equal (nth 2 call) "herdr-selected"))
      (should-not (nth 3 call))
      (let ((send (car (car (scholia-herdr-test--sends)))))
        (should (equal (scholia-db-send-kind send) 'agent))
        (should (equal (scholia-db-send-target send) "agent-1"))
        (should (equal (scholia-db-send-label send) "Agent One"))
        (should (equal (scholia-db-send-herdr-session send) "herdr-selected"))
        (should (equal (scholia-db-send-format send) 'integrate))
        (should (equal (scholia-db-send-scope send) 'point))
        (should (string-match-p "\\`[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]T"
                                (scholia-db-send-at send)))))))

(ert-deftest scholia-herdr-send-dispatches-a-herdr-pane-without-an-agent ()
  (scholia-herdr-test--with-buffer
    (scholia-create-chain 7 11 "beta")
    (goto-char 8)
    (let ((agent-calls 0)
          (pane-calls 0)
          (pane-call nil)
          (herdr-session "herdr-pane-selected")
          (herdr-socket-path "/stale/herdr.sock")
          (scholia-herdr-default-target "pane-1"))
      (cl-letf (((symbol-function 'herdr-agent-sessions) (lambda () nil))
                ((symbol-function 'herdr-panes)
                 (lambda () (list '((pane_id . "pane-1") (label . "Pane")))))
                ((symbol-function 'herdr-read-entry)
                 (lambda (&rest _) (error "The default must skip the picker")))
                ((symbol-function 'herdr-api-agent-prompt)
                 (lambda (&rest _) (cl-incf agent-calls)))
                ((symbol-function 'herdr-api-pane-send-text)
                 (lambda (pane-id text)
                   (cl-incf pane-calls)
                   (setq pane-call (list pane-id text herdr-session herdr-socket-path)))))
        (scholia-herdr-send))
      (should (zerop agent-calls))
      (should (= pane-calls 1))
      (should (equal (car pane-call) "pane-1"))
      (should (string-match-p "beta" (cadr pane-call)))
      (should (equal (nth 2 pane-call) "herdr-pane-selected"))
      (should-not (nth 3 pane-call))
      (let ((send (car (car (scholia-herdr-test--sends)))))
        (should (equal (scholia-db-send-kind send) 'pane))
        (should (equal (scholia-db-send-target send) "pane-1"))
        (should (equal (scholia-db-send-herdr-session send)
                       "herdr-pane-selected"))))))

(ert-deftest scholia-herdr-send-offers-decorated-agents-and-panes-to-one-picker ()
  (scholia-herdr-test--with-buffer
    (scholia-create-chain 1 6 "alpha")
    (goto-char 2)
    (let ((picked nil)
          (picker-calls 0)
          (herdr-session "selected")
          (agent '((kind . "herdr") (session . "selected")
                   (agent . "claude") (name . "agent-1")))
          (pane '((pane_id . "pane-1") (label . "Pane"))))
      (cl-letf (((symbol-function 'herdr-agent-sessions) (lambda () (list agent)))
                ((symbol-function 'herdr-panes) (lambda () (list pane)))
                ((symbol-function 'herdr-read-entry)
                 (lambda (_prompt entries &rest _)
                   (cl-incf picker-calls)
                   (setq picked entries)
                   (cl-find-if (lambda (entry)
                                 (equal (alist-get 'pane_id entry) "pane-1"))
                               entries)))
                ((symbol-function 'herdr-api-agent-prompt)
                 (lambda (&rest _) (error "The picker selected a pane")))
                ((symbol-function 'herdr-api-pane-send-text) (lambda (&rest _) nil)))
        (scholia-herdr-send))
      (should (= picker-calls 1))
      (should (member agent picked))
      (let ((decorated-pane
             (cl-find-if (lambda (entry)
                           (equal (alist-get 'pane_id entry) "pane-1"))
                         picked)))
        (should decorated-pane)
        (should (equal (alist-get 'kind decorated-pane) "herdr"))
        (should (equal (alist-get 'session decorated-pane) "selected"))
        (should (equal (alist-get 'pane_id decorated-pane) "pane-1"))
        (should (equal (alist-get 'label decorated-pane) "Pane"))
        (should (= (cl-count 'kind decorated-pane :key #'car :test #'eq) 1))
        (should (= (cl-count 'session decorated-pane :key #'car :test #'eq) 1))))))

(ert-deftest scholia-herdr-send-region-renders-once-and-records-every-annotation ()
  (scholia-herdr-test--with-buffer
    (scholia-create-chain 1 6 "first annotation note")
    (scholia-create-chain 7 11 "second annotation note")
    (goto-char 1)
    (set-mark 11)
    (activate-mark)
    (let ((calls nil)
          (scholia-herdr-default-target "agent-1"))
      (cl-letf (((symbol-function 'herdr-agent-sessions)
                 (lambda ()
                   (list '((kind . "herdr") (session . "herdr-selected")
                           (agent . "claude") (name . "agent-1")
                           (label . "Agent One")))))
                ((symbol-function 'herdr-panes) (lambda () nil))
                ((symbol-function 'herdr-api-agent-prompt)
                 (lambda (target text &rest _)
                   (push (list target text) calls))))
        (scholia-herdr-send-region))
      (should (= (length calls) 1))
      (should (string-match-p "first annotation note" (cadar calls)))
      (should (string-match-p "second annotation note" (cadar calls)))
      (should (equal (mapcar #'length (scholia-herdr-test--sends)) '(1 1))))))

(ert-deftest scholia-herdr-send-file-uses-the-export-format-for-one-batch ()
  (scholia-herdr-test--with-buffer
    (dolist (bounds '((1 . 6) (7 . 11) (12 . 17) (18 . 23)))
      (scholia-create-chain (car bounds) (cdr bounds) "note"))
    (let ((calls nil)
          (scholia-export-format 'diff)
          (scholia-herdr-send-format nil)
          (scholia-herdr-default-target "agent-1"))
      (cl-letf (((symbol-function 'herdr-agent-sessions)
                 (lambda ()
                   (list '((kind . "herdr") (session . "herdr-selected")
                           (agent . "claude") (name . "agent-1")
                           (label . "Agent One")))))
                ((symbol-function 'herdr-panes) (lambda () nil))
                ((symbol-function 'herdr-api-agent-prompt)
                 (lambda (_target text &rest _) (push text calls))))
        (scholia-herdr-send-file))
      (should (= (length calls) 1))
      (should (string-prefix-p "--- " (car calls)))
      (should (equal (mapcar #'length (scholia-herdr-test--sends)) '(1 1 1 1))))))

(ert-deftest scholia-herdr-send-file-does-not-record-a-signalling-send ()
  (scholia-herdr-test--with-buffer
    (scholia-create-chain 1 6 "alpha")
    (scholia-create-chain 7 11 "beta")
    (scholia-save-annotations)
    (let ((before (scholia-herdr-test--sends))
          (calls 0)
          (scholia-herdr-default-target "agent-1"))
      (should (= (length before) 2))
      (cl-letf (((symbol-function 'herdr-agent-sessions)
                 (lambda ()
                   (list '((kind . "herdr") (session . "herdr-selected")
                           (agent . "claude") (name . "agent-1")
                           (label . "Agent One")))))
                ((symbol-function 'herdr-panes) (lambda () nil))
                ((symbol-function 'herdr-api-agent-prompt)
                 (lambda (&rest _)
                   (cl-incf calls)
                   (signal 'scholia-herdr-test-refused '("agent-1")))))
        (should-error (scholia-herdr-send-file) :type 'scholia-herdr-test-refused))
      (should (= calls 1))
      (should (equal (scholia-herdr-test--sends) before)))))

(ert-deftest scholia-herdr-send-session-uses-the-live-first-session-renderer ()
  (scholia-herdr-test--with-buffer
    (scholia-create-chain 1 6 "alpha")
    (scholia-save-annotations)
    (let ((payload nil)
          (calls 0)
          (other "/other/session-file.txt")
          (scholia-herdr-default-target "agent-1"))
      (scholia-db-store-record
       (scholia-session-file "selected")
       (scholia-db-make-record
        other
        (list (scholia-db-make-annotation "id-other" "other note" 1 6 "other"))
        "other-checksum"))
      (cl-letf (((symbol-function 'herdr-agent-sessions)
                 (lambda ()
                   (list '((kind . "herdr") (session . "herdr-selected")
                           (agent . "claude") (name . "agent-1")
                           (label . "Agent One")))))
                ((symbol-function 'herdr-panes) (lambda () nil))
                ((symbol-function 'scholia-export-session)
                 (lambda (session &optional _target format)
                   (should (equal session "selected"))
                   (should (equal format 'integrate))
                   "live session payload"))
                ((symbol-function 'scholia-export)
                 (lambda (&rest _) (error "The current-file renderer was used")))
                ((symbol-function 'herdr-api-agent-prompt)
                 (lambda (_target text &rest _)
                   (cl-incf calls)
                   (setq payload text))))
        (let ((scholia-herdr-send-format 'integrate))
          (scholia-herdr-send-session "selected")))
      (should (= calls 1))
      (should (equal payload "live session payload"))
      (should (equal (mapcar #'length (scholia-herdr-test--sends)) '(1)))
      (should (= (length (scholia-db-annotation-sends
                          (car (scholia-db-record-annotations
                                (scholia-db-record (scholia-session-file "selected") other)))))
                 1)))))

(ert-deftest scholia-herdr-send-file-keeps-one-membership-snapshot-and-unplaced-annotations ()
  (scholia-herdr-test--with-buffer
    (scholia-create-chain 1 6 "visible")
    (scholia-save-annotations)
    (let* ((file (buffer-file-name))
           (session-file (scholia-session-file))
           (record (scholia-db-record session-file file))
           (visible (car (scholia-db-record-annotations record)))
           (unplaced (scholia-db-make-annotation
                      "unplaced" "unplaceable" 1 6 "alpha"))
           (late-reply (scholia-db-make-annotation
                        "late-reply" "late reply" nil nil nil nil nil
                        (scholia-db-annotation-id visible)))
           (collect (symbol-function 'scholia-export--payload))
           (renderer (symbol-function 'scholia-export))
           (snapshot-complete nil)
           (calls 0)
           (collections 0)
           (payload nil)
           (scholia-herdr-default-target "agent-1"))
      (scholia-db-store-record
       session-file
       (scholia-db-make-record
        file (append (scholia-db-record-annotations record) (list unplaced))
        "checksum"))
      (setq-local scholia--unplaced-annotations (list unplaced))
      (cl-letf (((symbol-function 'herdr-agent-sessions)
                 (lambda ()
                   (list '((kind . "herdr") (session . "herdr-selected")
                           (agent . "claude") (name . "agent-1")))))
                ((symbol-function 'herdr-panes) (lambda () nil))
                ((symbol-function 'scholia-export--payload)
                 (lambda (&rest arguments)
                   (cl-incf collections)
                   (prog1 (apply collect arguments)
                     (setq snapshot-complete t)
                     (when (= collections 1)
                       (scholia-db-add-reply session-file file late-reply)))))
                ((symbol-function 'scholia-export)
                 (lambda (&rest arguments)
                   (should snapshot-complete)
                   (apply renderer arguments)))
                ((symbol-function 'herdr-api-agent-prompt)
                 (lambda (_target text &rest _)
                   (cl-incf calls)
                   (setq payload text))))
        (scholia-herdr-send-file))
      (let ((annotations
             (scholia-db-record-annotations
              (scholia-db-record session-file file))))
        (should
         (equal
          (list calls collections
                (and (string-match-p "visible" payload)
                     (string-match-p "unplaceable" payload)
                     (not (string-match-p "late reply" payload)))
                (sort (mapcar #'scholia-db-annotation-id annotations) #'string<)
                (mapcar (lambda (id)
                          (length (scholia-db-annotation-sends
                                   (cl-find id annotations
                                            :key #'scholia-db-annotation-id
                                            :test #'equal))))
                        (list (scholia-db-annotation-id visible) "late-reply" "unplaced")))
          (list 1 1 t
                (sort (list (scholia-db-annotation-id visible)
                            "unplaced" "late-reply")
                      #'string<)
                '(1 0 1))))))))

(ert-deftest scholia-herdr-send-session-preserves-concurrent-rows-and-rolls-back-history ()
  (scholia-herdr-test--with-buffer
    (scholia-create-chain 1 6 "root")
    (scholia-save-annotations)
    (let* ((selected-session-file (scholia-session-file "selected"))
           (selected-file (buffer-file-name))
           (selected-root
            (car (scholia-db-record-annotations
                  (scholia-db-record selected-session-file selected-file))))
           (selected-prior
            (list (scholia-db-make-send :kind 'agent
                                        :target "previous-selected"
                                        :label "previous selected"
                                        :herdr-session "previous"
                                        :format 'integrate
                                        :scope 'session)))
           (other-file "/other/session-file.txt")
           (other-prior
            (list (scholia-db-make-send :kind 'agent
                                        :target "previous-other"
                                        :label "previous other"
                                        :herdr-session "previous"
                                        :format 'integrate
                                        :scope 'session)))
           (other (scholia-db-annotation-add-send
                   (scholia-db-make-annotation
                    "other" "other session note" 1 6 "other")
                   (car other-prior)))
           (concurrent (scholia-db-make-annotation
                        "concurrent" "concurrent reply" nil nil nil nil nil "other"))
           (session-records (symbol-function 'scholia-herdr--session-records))
           (renderer (symbol-function 'scholia-export-session))
           (snapshot-complete nil)
           (calls 0)
           (collections 0)
           (payload nil)
           (scholia-herdr-default-target "agent-1"))
      (scholia-db-store-record
       selected-session-file
       (scholia-db-make-record
        selected-file
        (list (scholia-db-annotation-add-send selected-root (car selected-prior)))
        "checksum"))
      (scholia-db-store-record
       selected-session-file
       (scholia-db-make-record other-file (list other) "other-checksum"))
      (cl-letf (((symbol-function 'herdr-agent-sessions)
                 (lambda ()
                   (list '((kind . "herdr") (session . "herdr-selected")
                           (agent . "claude") (name . "agent-1")))))
                ((symbol-function 'herdr-panes) (lambda () nil))
                ((symbol-function 'scholia-herdr--session-records)
                 (lambda (&rest arguments)
                   (cl-incf collections)
                   (let ((records (apply session-records arguments)))
                     (setq snapshot-complete t)
                     (when (= collections 1)
                       (scholia-db-add-reply
                        selected-session-file other-file concurrent))
                     records)))
                ((symbol-function 'scholia-export-session)
                 (lambda (&rest arguments)
                   (should snapshot-complete)
                   (apply renderer arguments)))
                ((symbol-function 'herdr-api-agent-prompt)
                 (lambda (_target text &rest _)
                   (should snapshot-complete)
                   (cl-incf calls)
                   (setq payload text))))
        (scholia-herdr-send-session "selected"))
      (let ((annotations
             (append
              (scholia-db-record-annotations
               (scholia-db-record selected-session-file selected-file))
              (scholia-db-record-annotations
               (scholia-db-record selected-session-file other-file)))))
        (should
         (equal
          (sort (mapcar #'scholia-db-annotation-id annotations) #'string<)
          (sort (list (scholia-db-annotation-id selected-root)
                      "other" "concurrent")
                #'string<)))
        (let ((selected-sends
               (scholia-db-annotation-sends
                (cl-find (scholia-db-annotation-id selected-root) annotations
                         :key #'scholia-db-annotation-id :test #'equal)))
              (other-sends
               (scholia-db-annotation-sends
                (cl-find "other" annotations
                         :key #'scholia-db-annotation-id :test #'equal))))
          (should (equal (cl-subseq selected-sends 0 (length selected-prior))
                         selected-prior))
          (should (equal (cl-subseq other-sends 0 (length other-prior))
                         other-prior))
          (should (= (length selected-sends) (1+ (length selected-prior))))
          (should (= (length other-sends) (1+ (length other-prior))))
          (should (equal (car (last selected-sends)) (car (last other-sends))))))
      (setq-local scholia-session "failing")
      (scholia-save-annotations)
      (let* ((failing-session-file (scholia-session-file))
             (failing-root
              (car (scholia-db-record-annotations
                    (scholia-db-record failing-session-file selected-file))))
             (failing-prior
              (list (scholia-db-make-send :kind 'agent
                                          :target "previous-failing-root"
                                          :label "previous failing root"
                                          :herdr-session "previous"
                                          :format 'integrate
                                          :scope 'session)))
             (failing-other-prior
              (list (scholia-db-make-send :kind 'agent
                                          :target "previous-failing-other"
                                          :label "previous failing other"
                                          :herdr-session "previous"
                                          :format 'integrate
                                          :scope 'session)))
             (failing-other (scholia-db-annotation-add-send
                             (scholia-db-make-annotation
                              "failing-other" "other" 1 6 "other")
                             (car failing-other-prior)))
             (put-record (symbol-function 'scholia-store-put-record))
             (stores 0)
             (failure nil)
             (failure-calls 0))
        (scholia-db-store-record
         failing-session-file
         (scholia-db-make-record
          selected-file
          (list (scholia-db-annotation-add-send failing-root (car failing-prior)))
          "checksum"))
        (scholia-db-store-record
         failing-session-file
         (scholia-db-make-record
          other-file (list failing-other) "other-checksum"))
        (cl-letf (((symbol-function 'herdr-agent-sessions)
                   (lambda ()
                     (list '((kind . "herdr") (session . "herdr-selected")
                             (agent . "claude") (name . "agent-1")))))
                  ((symbol-function 'herdr-panes) (lambda () nil))
                  ((symbol-function 'scholia-export-session)
                   (lambda (&rest _) "failing session payload"))
                  ((symbol-function 'herdr-api-agent-prompt)
                   (lambda (&rest _) (cl-incf failure-calls)))
                  ((symbol-function 'scholia-store-put-record)
                   (lambda (&rest arguments)
                     (cl-incf stores)
                     (if (= stores 2)
                         (signal 'scholia-herdr-test-refused '("persistence"))
                       (apply put-record arguments)))))
          (condition-case error
              (scholia-herdr-send-session "failing")
            (scholia-herdr-test-refused
             (setq failure (car error)))))
        (let* ((selected-annotations
                (append
                 (scholia-db-record-annotations
                  (scholia-db-record selected-session-file selected-file))
                 (scholia-db-record-annotations
                  (scholia-db-record selected-session-file other-file))))
               (failing-annotations
                (append
                 (scholia-db-record-annotations
                  (scholia-db-record failing-session-file selected-file))
                 (scholia-db-record-annotations
                  (scholia-db-record failing-session-file other-file)))))
          (should
           (equal
            (sort (mapcar #'scholia-db-annotation-id failing-annotations) #'string<)
            (sort (list (scholia-db-annotation-id failing-root) "failing-other")
                  #'string<)))
          (should
           (equal
            (list calls collections
                  (and (string-match-p "root" payload)
                       (string-match-p "other session note" payload)
                       (not (string-match-p "concurrent reply" payload)))
                  (mapcar (lambda (id)
                            (length (scholia-db-annotation-sends
                                     (cl-find id selected-annotations
                                              :key #'scholia-db-annotation-id
                                              :test #'equal))))
                          (list (scholia-db-annotation-id selected-root)
                                "other" "concurrent"))
                  failure failure-calls stores
                  (list
                   (scholia-db-annotation-sends
                    (cl-find (scholia-db-annotation-id failing-root)
                             failing-annotations
                             :key #'scholia-db-annotation-id :test #'equal))
                   (scholia-db-annotation-sends
                    (cl-find "failing-other" failing-annotations
                             :key #'scholia-db-annotation-id :test #'equal))))
            (list 1 1 t '(2 2 0) 'scholia-herdr-test-refused 1 2
                  (list failing-prior failing-other-prior)))))))))

(provide 'scholia-herdr-test)
;;; scholia-herdr-test.el ends here
