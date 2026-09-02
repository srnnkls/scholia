;;; scholia-org-remark.el --- Org export  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/scholia
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Exports named sessions as an Org file org-remark can read.

;;; Code:

(require 'org-remark)
(require 'seq)
(require 'scholia-core)
(require 'scholia-db)
(require 'scholia-locate)
(require 'scholia-session)
(require 'scholia-thread)

(defun scholia-org-remark--sessions (sessions)
  "Return SESSIONS as a list of session names."
  (if (listp sessions) sessions (list sessions)))

(defun scholia-org-remark--records (sessions)
  "Return the records in SESSIONS, grouped by source file."
  (let (records)
    (dolist (session (scholia-org-remark--sessions sessions))
      (dolist (record
               (plist-get (scholia-db-interchange
                           (scholia-session-file session))
                          :records))
        (let* ((file (scholia-db-record-file record))
               (group (assoc-string file records)))
          (if group
              (setcdr group (append (cdr group) (list record)))
            (push (cons file (list record)) records)))))
    (nreverse records)))

(defun scholia-org-remark--text (text)
  "Return TEXT with Org headlines escaped."
  (replace-regexp-in-string "^\\*+[ \t]" ",\\&" text))

(defun scholia-org-remark--reply-text (annotation annotations depth)
  "Return the replies to ANNOTATION nested at DEPTH in ANNOTATIONS."
  (mapconcat
   (lambda (reply)
     (let ((indent (make-string (* depth 2) ?\s)))
       (concat "\n\n" indent
               (replace-regexp-in-string
                "\n" (concat "\n" indent)
                (scholia-db-annotation-text reply))
               (scholia-org-remark--reply-text reply annotations (1+ depth)))))
   (scholia-thread-children annotation annotations) ""))

(defun scholia-org-remark--annotation (annotation annotations)
  "Return ANNOTATION as an org-remark headline from ANNOTATIONS."
  (let ((revision (scholia-locate-revision annotation)))
    (concat "** " (scholia-db-annotation-id annotation) "\n"
            ":PROPERTIES:\n"
            ":org-remark-id: " (scholia-db-annotation-id annotation) "\n"
            ":org-remark-beg: "
            (number-to-string (scholia-db-annotation-beg annotation)) "\n"
            ":org-remark-end: "
            (number-to-string (scholia-db-annotation-end annotation)) "\n"
            (if revision (concat ":scholia-revision: " revision "\n") "")
            ":END:\n"
            (scholia-org-remark--text
             (scholia-db-annotation-text annotation))
            (scholia-org-remark--reply-text annotation annotations 1) "\n")))

(defun scholia-org-remark--file (file records)
  "Return FILE and its positioned RECORDS as org-remark headings."
  (concat "* " file "\n"
          ":PROPERTIES:\n"
          ":org-remark-file: " file "\n"
          ":END:\n"
          (mapconcat
           (lambda (record)
             (let ((annotations
                    (mapcar (lambda (annotation)
                              (scholia-locate-materialize file annotation))
                            (scholia-db-record-annotations record))))
               (mapconcat
                (lambda (annotation)
                  (scholia-org-remark--annotation annotation annotations))
                (seq-remove #'scholia-db-annotation-reply-p annotations) "")))
           records "\n")
          "\n"))

(defun scholia-org-remark--render (sessions output-file)
  "Return SESSIONS as org-remark text for OUTPUT-FILE."
  (let ((org-remark-notes-file-name output-file))
    (mapconcat
     (lambda (record)
       (let ((file (car record)))
         (scholia-org-remark--file
          (if output-file
              (org-remark-source-get-file-name file)
            file)
          (cdr record))))
     (scholia-org-remark--records sessions) "")))

(defun scholia-org-remark--write (output file)
  "Write OUTPUT to FILE without changing its visiting buffer."
  (write-region output nil file nil 'silent)
  (when-let* ((buffer (find-buffer-visiting file)))
    (with-current-buffer buffer
      (unless (buffer-modified-p)
        (revert-buffer t t)))))

(defun scholia-org-remark-export (sessions &optional output-file)
  "Export SESSIONS as org-remark text, optionally writing OUTPUT-FILE."
  (interactive
   (let ((sessions (completing-read "Session: " (scholia-session-list) nil t))
         (output-file (read-file-name "Notes file: ")))
     (when (and (file-exists-p output-file)
                (not (yes-or-no-p (format "Replace %s? " output-file))))
       (user-error "Not replacing %s" output-file))
     (list sessions output-file)))
  (let ((output (scholia-org-remark--render sessions output-file)))
    (when output-file
      (scholia-org-remark--write output output-file))
    output))

(provide 'scholia-org-remark)
;;; scholia-org-remark.el ends here
