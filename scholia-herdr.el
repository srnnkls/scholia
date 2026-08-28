;;; scholia-herdr.el --- Send annotations to herdr  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/scholia
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Sends rendered annotations to an agent or pane managed by herdr.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'herdr)
(require 'scholia-core)
(require 'scholia-db)
(require 'scholia-export)

(defun scholia-herdr--format ()
  "Return the format used for a herdr send."
  (or scholia-herdr-send-format scholia-export-format))

(defun scholia-herdr--entries ()
  "Return the agents and panes available to receive a send."
  (append (herdr-agent-sessions)
          (mapcar (lambda (pane)
                    (append `((kind . "herdr") (session . ,herdr-session))
                            pane))
                  (herdr-panes))))

(defun scholia-herdr--entry ()
  "Return the target entry chosen for a send."
  (let ((entries (scholia-herdr--entries)))
    (if scholia-herdr-default-target
        (or (cl-find-if
             (lambda (entry)
               (equal scholia-herdr-default-target
                      (or (alist-get 'name entry)
                          (alist-get 'pane_id entry))))
             entries)
            (user-error "No herdr target named %s"
                        scholia-herdr-default-target))
      (herdr-read-entry "Send annotations to herdr: " entries))))

(defun scholia-herdr--agent-p (entry)
  "Return non-nil when ENTRY represents an agent."
  (assq 'agent entry))

(defun scholia-herdr--target (entry)
  "Return the API target named by ENTRY."
  (if (scholia-herdr--agent-p entry)
      (or (alist-get 'name entry) (alist-get 'pane_id entry))
    (alist-get 'pane_id entry)))

(defun scholia-herdr--send-record (entry format scope)
  "Return the send record for ENTRY, FORMAT, and SCOPE."
  (scholia-db-make-send
   :kind (if (scholia-herdr--agent-p entry) 'agent 'pane)
   :target (scholia-herdr--target entry)
   :label (alist-get 'label entry)
   :herdr-session (alist-get 'session entry)
   :format format
   :scope scope))

(defun scholia-herdr--replace-sent (annotations sent)
  "Return ANNOTATIONS with the entries of SENT replacing matching ones."
  (mapcar
   (lambda (annotation)
     (or (seq-find (lambda (updated)
                     (equal (scholia-db-annotation-id updated)
                            (scholia-db-annotation-id annotation)))
                   sent)
         annotation))
   annotations))

(defun scholia-herdr--dispatch (entry text)
  "Send TEXT to ENTRY through its herdr session."
  (herdr-with-session (alist-get 'session entry)
    (if (scholia-herdr--agent-p entry)
        (herdr-api-agent-prompt (scholia-herdr--target entry) text)
      (herdr-api-pane-send-text (scholia-herdr--target entry) text))))

(defun scholia-herdr--send (annotations payload format scope persist)
  "Send ANNOTATIONS as PAYLOAD in FORMAT and SCOPE.
Call PERSIST after success."
  (let* ((entry (scholia-herdr--entry))
         (send (scholia-herdr--send-record entry format scope))
         (sent (scholia-db-send-batch
                annotations send
                (lambda () (scholia-herdr--dispatch entry payload)))))
    (funcall persist sent send)))

(defun scholia-herdr--store-buffer (sent send)
  "Store this buffer after recording SENT with SEND."
  (scholia-db-add-send-batch
   (scholia-session-file)
   (mapcar #'scholia-db-annotation-id sent)
   send)
  (setq-local scholia--unplaced-annotations
              (scholia-herdr--replace-sent scholia--unplaced-annotations sent))
  (scholia-core--store
   (scholia-herdr--replace-sent (scholia-core--buffer-annotations) sent)))

(defun scholia-herdr--buffer-payload ()
  "Return the annotations exported from this buffer."
  (scholia-export--payload))

(defun scholia-herdr--send-buffer (select payload scope)
  "Send the annotations SELECT takes from PAYLOAD with SCOPE."
  (let* ((format (scholia-herdr--format))
         (annotations (funcall select payload)))
    (unless annotations
      (user-error "No annotations to send"))
    (scholia-herdr--send
     annotations
     (scholia-export-render annotations format)
     format scope
     (lambda (sent send) (scholia-herdr--store-buffer sent send)))))

;;;###autoload
(defun scholia-herdr-send ()
  "Send the annotation at point to a herdr target."
  (interactive)
  (let* ((chain (scholia-chain-at (point)))
         (annotation (and chain (scholia-core--chain-annotation chain))))
    (unless annotation
      (user-error "No annotation at point"))
    (let ((id (scholia-db-annotation-id annotation)))
      (scholia-herdr--send-buffer
       (lambda (payload)
         (seq-filter (lambda (candidate)
                       (equal id (scholia-db-annotation-id candidate)))
                     payload))
       (scholia-herdr--buffer-payload)
       'point))))

;;;###autoload
(defun scholia-herdr-send-region (&optional beg end)
  "Send annotations overlapping BEG to END to a herdr target."
  (interactive "r")
  (let ((beg (or beg (region-beginning)))
        (end (or end (region-end))))
    (scholia-herdr--send-buffer
     (lambda (payload)
       (seq-filter
        (lambda (annotation)
          (and (not (scholia-db-annotation-reply-p annotation))
               (< (scholia-db-annotation-beg annotation) end)
               (< beg (scholia-db-annotation-end annotation))))
        payload))
     (scholia-herdr--buffer-payload)
     'region)))

;;;###autoload
(defun scholia-herdr-send-file ()
  "Send all annotations of this file to a herdr target."
  (interactive)
  (let* ((format (scholia-herdr--format))
         (annotations (append (scholia-herdr--buffer-payload)
                              scholia--unplaced-annotations)))
    (unless annotations
      (user-error "No annotations to send"))
    (scholia-herdr--send
     annotations
     (scholia-export-render annotations format)
     format 'file
     (lambda (sent send) (scholia-herdr--store-buffer sent send)))))

(defun scholia-herdr--session-records (session)
  "Return the records stored in SESSION."
  (delq nil
        (mapcar (lambda (file)
                  (scholia-db-record (scholia-session-file session) file))
                (scholia-db-files (scholia-session-file session)))))

(defun scholia-herdr--store-session (session sent send)
  "Store SESSION after recording SENT with SEND."
  (scholia-db-add-send-batch
   (scholia-session-file session)
   (mapcar #'scholia-db-annotation-id sent)
   send))

;;;###autoload
(defun scholia-herdr-send-session (session)
  "Send all annotations of SESSION to a herdr target."
  (interactive
   (list (completing-read "Session: " (scholia-session-list) nil t)))
  (let* ((format (scholia-herdr--format))
         (records (scholia-herdr--session-records session))
         (annotations (apply #'append
                             (mapcar #'scholia-db-record-annotations records))))
    (unless annotations
      (user-error "No annotations to send"))
    (cl-letf (((symbol-function 'scholia-export--session-records)
               (lambda (_session) records)))
      (scholia-herdr--send
       annotations
       (scholia-export-session session nil format)
       format 'session
       (lambda (sent send) (scholia-herdr--store-session session sent send))))))

(provide 'scholia-herdr)
;;; scholia-herdr.el ends here
