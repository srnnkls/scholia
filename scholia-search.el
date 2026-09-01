;;; scholia-search.el --- Cross-session search  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/scholia
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Finds annotations stored across scholia sessions and opens their source
;; locations.

;;; Code:

(require 'seq)
(require 'scholia-db)
(require 'scholia-locate)
(require 'scholia-session)

(defun scholia-search-annotations ()
  "Return the annotations stored in every session with their context.
Each entry carries `:session', `:file', `:record' and `:annotation'."
  (apply #'append
         (mapcar
          (lambda (session)
            (let ((session-file (scholia-session-file session)))
              (scholia-db--reading
               session-file
               (lambda (store)
                 (apply #'append
                        (mapcar
                         (lambda (file)
                           (let ((record (scholia-store-record store file)))
                             (mapcar (lambda (annotation)
                                       (list :session session
                                             :file file
                                             :record record
                                             :annotation annotation))
                                     (scholia-db-record-annotations record))))
                         (scholia-store-files store)))))))
          (scholia-session-list))))

(defun scholia-search-candidate-string (entry)
  "Return the completion candidate string for annotation ENTRY."
  (let ((annotation (plist-get entry :annotation)))
    (format "%s — %s — %s — %s%s — [%s]"
            (scholia-db-annotation-text annotation)
            (scholia-db-annotation-annotated-text annotation)
            (plist-get entry :file)
            (plist-get entry :session)
            (if-let ((revision (scholia-locate-revision annotation)))
                (format " — [%s]" revision)
              "")
            (scholia-db-annotation-id annotation))))

(defun scholia-search--candidate-entry (candidate candidates)
  "Return the entry CANDIDATE names in CANDIDATES."
  (cdr (assoc-string candidate candidates)))

(defun scholia-search--jump-annotation (entry)
  "Return the annotation ENTRY should open at.
Replies follow their `:reply-to' parents in the same record to a position."
  (let ((annotation (plist-get entry :annotation))
        (annotations (scholia-db-record-annotations
                      (plist-get entry :record)))
        (seen nil))
    (while (and annotation
                (not (scholia-db-annotation-beg annotation))
                (scholia-db-annotation-reply-to annotation))
      (let ((id (scholia-db-annotation-id annotation)))
        (when (member id seen)
          (user-error "Reply thread contains a cycle"))
        (push id seen)
        (setq annotation
              (seq-find
               (lambda (candidate)
                 (equal (scholia-db-annotation-reply-to annotation)
                        (scholia-db-annotation-id candidate)))
               annotations))))
    (unless annotation
      (user-error "Annotation has no stored position"))
    (setq annotation
          (scholia-locate-materialize (plist-get entry :file) annotation))
    (unless (scholia-db-annotation-beg annotation)
      (user-error "Annotation has no stored position"))
    annotation))

(defun scholia-search-sends-to (destination)
  "Return collector entries sent to DESTINATION."
  (seq-filter
   (lambda (entry)
     (seq-some (lambda (send)
                 (equal destination (scholia-db-send-target send)))
               (scholia-db-annotation-sends
                (plist-get entry :annotation))))
   (scholia-search-annotations)))

(defun scholia-search--send-candidates (entries)
  "Return completion candidates for sends in ENTRIES."
  (apply #'append
         (mapcar
          (lambda (entry)
            (mapcar
             (lambda (send)
               (cons
                (format "%s — %s — %s — [%s]"
                        (scholia-db-send-label send)
                        (scholia-db-send-herdr-session send)
                        (scholia-db-send-at send)
                        (scholia-db-annotation-id
                         (plist-get entry :annotation)))
                (list entry send)))
             (scholia-db-annotation-sends
              (plist-get entry :annotation))))
          entries)))

(defun scholia-search--jump (entry)
  "Visit ENTRY, activating its owner in a multi-session display."
  (let* ((session (plist-get entry :session))
         (annotation (scholia-search--jump-annotation entry))
         (file (plist-get entry :file)))
    (when-let ((buffer (find-buffer-visiting file)))
      (with-current-buffer buffer
        (when scholia-mode (scholia-save-annotations))))
    (scholia-session-activate session)
    (setq annotation (scholia-locate-open file annotation))
    (if scholia-mode
        (progn
          (scholia-shutdown nil)
          (scholia-mode 1))
      (scholia-mode 1))
    (unless (seq-some
             (lambda (chain)
               (equal (scholia-core--chain-id chain)
                      (scholia-db-annotation-id annotation)))
             (scholia-buffer-chains))
      (scholia-core--restore annotation session))))

(defun scholia-search-sends ()
  "Select a send across sessions and visit its annotation."
  (interactive)
  (let* ((candidates (scholia-search--send-candidates
                      (scholia-search-annotations)))
         (table
          (lambda (string predicate action)
            (if (eq action 'metadata)
                (list 'metadata
                      '(category . scholia-send)
                      (cons 'group-function
                            (lambda (candidate transform)
                              (if transform
                                  candidate
                                (scholia-db-send-label
                                 (caddr
                                  (assoc-string candidate candidates)))))))
              (complete-with-action action candidates string predicate))))
         (selected (completing-read "Send: " table nil t)))
    (unless (equal selected "")
      (scholia-search--jump
       (car (scholia-search--candidate-entry selected candidates))))))

(defun scholia-search ()
  "Select an annotation across sessions and visit its source location."
  (interactive)
  (let* ((entries (scholia-search-annotations))
         (candidates (mapcar (lambda (entry)
                               (cons (scholia-search-candidate-string entry)
                                     entry))
                             entries))
         (table (lambda (string predicate action)
                  (if (eq action 'metadata)
                      '(metadata (category . scholia-annotation))
                    (complete-with-action action candidates string
                                          predicate))))
         (selected (completing-read "Annotation: " table nil t)))
    (unless (equal selected "")
      (scholia-search--jump
       (scholia-search--candidate-entry selected candidates)))))

(provide 'scholia-search)
;;; scholia-search.el ends here
