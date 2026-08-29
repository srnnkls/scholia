;;; scholia-status.el --- Status dashboard  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/scholia
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Shows annotations across sessions in a collapsible section buffer.

;;; Code:

(require 'magit-section)
(require 'seq)
(require 'scholia-db)
(require 'scholia-export)
(require 'scholia-filter)
(require 'scholia-locate)
(require 'scholia-search)
(require 'scholia-session)
(require 'scholia-thread)

(declare-function scholia-herdr--send "scholia-herdr")
(declare-function magit-section-value "magit-section")
(declare-function magit-section-parent "magit-section")
(declare-function magit-section-type "magit-section")

(unless (fboundp 'magit-section-value)
  (defalias 'magit-section-value (lambda (section) (oref section value))))
(unless (fboundp 'magit-section-parent)
  (defalias 'magit-section-parent (lambda (section) (oref section parent))))
(unless (fboundp 'magit-section-type)
  (defalias 'magit-section-type (lambda (section) (oref section type))))

(defconst scholia-status-buffer-name "*scholia-status*"
  "Name of the scholia status buffer.")

(defvar scholia-status--marked-sessions nil
  "Sessions selected for a combined export.")

(defvar-local scholia-status--marked-hunks nil
  "Annotation keys selected for a combined send.")

(defvar-local scholia-status--query nil
  "Filter query represented by the current status buffer.")

(defun scholia-status--entry (section)
  "Return the dashboard entry represented by hunk SECTION."
  (let* ((annotation (magit-section-value section))
         (file-section (magit-section-parent section))
         (file (magit-section-value file-section))
         (session (magit-section-value (magit-section-parent file-section))))
    (list :session session
          :file file
          :record (scholia-db-record (scholia-session-file session) file)
          :annotation annotation)))

(defun scholia-status--entry-key (entry)
  "Return the stable key identifying ENTRY."
  (list (plist-get entry :session)
        (plist-get entry :file)
        (scholia-db-annotation-id (plist-get entry :annotation))))

(defun scholia-status--marked-entries ()
  "Return the current entries selected by marked hunk keys."
  (when scholia-status--marked-hunks
    (let ((entries (scholia-search-annotations)))
      (delq nil
            (mapcar (lambda (key)
                      (seq-find
                       (lambda (entry)
                         (equal key (scholia-status--entry-key entry)))
                       entries))
                    scholia-status--marked-hunks)))))

(defun scholia-status--hunk-at-point ()
  "Return the hunk section at point, if any."
  (let ((section (magit-current-section)))
    (while (and section (not (eq (magit-section-type section) 'hunk)))
      (setq section (magit-section-parent section)))
    section))

(defun scholia-status--session-at-point ()
  "Return the session section at point, if any."
  (let ((section (magit-current-section)))
    (while (and section (not (eq (magit-section-type section) 'session)))
      (setq section (magit-section-parent section)))
    section))

(defun scholia-status--hunks ()
  "Return the selected hunk sections, or the hunk at point."
  (or (magit-region-sections 'hunk t)
      (when-let ((section (scholia-status--hunk-at-point))) (list section))))

(defun scholia-status--selected-entries ()
  "Return marked entries, or entries represented by the current hunks."
  (or (scholia-status--marked-entries)
      (mapcar #'scholia-status--entry (scholia-status--hunks))))

(defun scholia-status--groups (entries)
  "Return ENTRIES grouped by session and file in their original order."
  (let (sessions)
    (dolist (entry entries)
      (let* ((session (plist-get entry :session))
             (file (plist-get entry :file))
             (session-group (assoc-string session sessions))
             (files (cdr session-group))
             (file-group (assoc-string file files)))
        (unless session-group
          (setq session-group (list session))
          (setq sessions (append sessions (list session-group)))
          (setq files (cdr session-group)))
        (unless file-group
          (setq file-group (list file))
          (setcdr session-group (append files (list file-group))))
        (setcdr file-group (append (cdr file-group) (list entry)))))
    sessions))

(defun scholia-status--insert (entries)
  "Insert ENTRIES into the current status buffer."
  (dolist (session-group (scholia-status--groups entries))
    (let ((session (car session-group)))
      (magit-insert-section (session session t)
        (magit-insert-heading "%s\n" session)
        (dolist (file-group (cdr session-group))
          (let ((file (car file-group))
                (entries (cdr file-group)))
            (magit-insert-section (file file)
              (magit-insert-heading "  %s\n" file)
              (scholia-thread-walk
               (mapcar (lambda (entry) (plist-get entry :annotation)) entries)
               (lambda (annotation depth)
                 (magit-insert-section (hunk annotation)
                   (insert (make-string (* depth 2) ?\s))
                   (insert (scholia-db-annotation-text annotation)
                           (if-let ((revision
                                     (scholia-locate-revision annotation)))
                               (format " [%s]" revision)
                             "")
                           "\n")))))))))))

(defun scholia-status--render (query)
  "Render QUERY in the status buffer and return that buffer."
  (let* ((entries (scholia-search-annotations))
         (filtered (if query
                       (scholia-filter-apply (scholia-filter-parse query)
                                             entries)
                     entries))
         (buffer (get-buffer-create scholia-status-buffer-name))
         (marked (and (local-variable-p 'scholia-status--marked-hunks buffer)
                      (buffer-local-value 'scholia-status--marked-hunks
                                          buffer))))
    (with-current-buffer buffer
      (scholia-status-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq-local scholia-status--marked-hunks marked)
        (setq-local scholia-status--query query)
        (magit-insert-section (status nil)
          (scholia-status--insert filtered))
        (goto-char (point-min))))
    (display-buffer buffer)
    buffer))

;;;###autoload
(defun scholia-status ()
  "Show annotations across all sessions."
  (interactive)
  (scholia-status--render nil))

(defun scholia-status-filter (query)
  "Show annotations matching QUERY."
  (interactive (list (read-string "Filter: " scholia-status--query)))
  (scholia-status--render query))

(defun scholia-status-switch-session (session)
  "Switch to SESSION."
  (interactive (list (magit-section-value
                      (or (scholia-status--session-at-point)
                          (user-error "No session at point")))))
  (scholia-session-switch session))

(defun scholia-status--refresh-after-action ()
  "Refresh an interactive action's live status buffer."
  (when (and (called-interactively-p 'interactive)
             (derived-mode-p 'scholia-status-mode))
    (scholia-status--render scholia-status--query)))

(defun scholia-status-rename-session (old new)
  "Rename OLD session to NEW."
  (interactive
   (let ((old (magit-section-value
               (or (scholia-status--session-at-point)
                   (user-error "No session at point")))))
     (list old (read-string (format "Rename %s to: " old)))))
  (scholia-session-rename old new)
  (setq scholia-status--marked-sessions
        (mapcar (lambda (session) (if (equal session old) new session))
                scholia-status--marked-sessions))
  (when (derived-mode-p 'scholia-status-mode)
    (setq-local scholia-status--marked-hunks
                (mapcar (lambda (key)
                          (if (equal (car key) old)
                              (cons new (cdr key))
                            key))
                        scholia-status--marked-hunks)))
  (scholia-status--refresh-after-action))

(defun scholia-status-mark-hunk (entry)
  "Toggle ENTRY in the annotations selected for sending."
  (interactive (list (scholia-status--entry
                      (or (scholia-status--hunk-at-point)
                          (user-error "No annotation at point")))))
  (let ((key (scholia-status--entry-key entry)))
    (if (member key scholia-status--marked-hunks)
        (setq-local scholia-status--marked-hunks
                    (delete key scholia-status--marked-hunks))
      (setq-local scholia-status--marked-hunks
                  (append scholia-status--marked-hunks (list key))))))

(defun scholia-status-mark-session (session)
  "Toggle SESSION in the sessions selected for export."
  (interactive (list (magit-section-value
                      (or (scholia-status--session-at-point)
                          (user-error "No session at point")))))
  (if (member session scholia-status--marked-sessions)
      (setq scholia-status--marked-sessions
            (delete session scholia-status--marked-sessions))
    (setq scholia-status--marked-sessions
          (append scholia-status--marked-sessions (list session)))))

(defun scholia-status-export-marked-sessions ()
  "Export marked sessions as one document."
  (interactive)
  (unless scholia-status--marked-sessions
    (user-error "No sessions marked"))
  (scholia-export-session scholia-status--marked-sessions 'buffer))

(defun scholia-status-jump (entry)
  "Jump to the source location of ENTRY."
  (interactive (list (scholia-status--entry
                      (or (scholia-status--hunk-at-point)
                          (user-error "No annotation at point")))))
  (scholia-search--jump entry))

(defun scholia-status-show-send-history (_entry)
  "Show send history for an annotation."
  (interactive (list (scholia-status--entry
                      (or (scholia-status--hunk-at-point)
                          (user-error "No annotation at point")))))
  (scholia-search-sends))

(defun scholia-status-delete (entry)
  "Delete ENTRY from its stored record."
  (interactive (list (scholia-status--entry
                      (or (scholia-status--hunk-at-point)
                          (user-error "No annotation at point")))))
  (let* ((session-file (scholia-session-file (plist-get entry :session)))
         (record (plist-get entry :record))
         (file (or (plist-get entry :file)
                   (and record (scholia-db-record-file record))))
         (id (scholia-db-annotation-id (plist-get entry :annotation))))
    (when file
      (scholia-db-remove-annotation session-file file id))
    (when (derived-mode-p 'scholia-status-mode)
      (setq-local scholia-status--marked-hunks
                  (delete (scholia-status--entry-key entry)
                          scholia-status--marked-hunks)))
    (scholia-status--refresh-after-action)))

(defun scholia-status--persist-sent (entries _sent send)
  "Persist SEND for the annotations carried by ENTRIES."
  (dolist (session-group
           (scholia-status--groups entries))
    (let ((session (car session-group))
          (ids (mapcar (lambda (file-group)
                         (mapcar (lambda (entry)
                                   (scholia-db-annotation-id
                                    (plist-get entry :annotation)))
                                 (cdr file-group)))
                       (cdr session-group))))
      (scholia-db-add-send-batch
       (scholia-session-file session) (apply #'append ids) send))))

(defun scholia-status--file-groups (entries)
  "Return ENTRIES grouped by source file in their original order."
  (let (groups)
    (dolist (entry entries)
      (let* ((file (plist-get entry :file))
             (group (assoc-string file groups)))
        (unless group
          (setq group (list file))
          (setq groups (append groups (list group))))
        (setcdr group (append (cdr group) (list entry)))))
    groups))

(defun scholia-status--render-file (entries format)
  "Render ENTRIES against its file's disk source or saved context using FORMAT."
  (let* ((file (plist-get (car entries) :file))
         (annotations (mapcar (lambda (entry) (plist-get entry :annotation))
                              entries)))
    (with-temp-buffer
      (let ((buffer-file-name file))
        (condition-case nil
            (insert-file-contents file)
          (error (insert (scholia-export--snapshot-source annotations))))
        (delay-mode-hooks (set-auto-mode))
        (scholia-export-render annotations format file)))))

(defun scholia-status--payload (entries format)
  "Render ENTRIES grouped by their source files using FORMAT."
  (string-join
   (mapcar (lambda (group)
             (scholia-status--render-file (cdr group) format))
           (scholia-status--file-groups entries))
   "\n\n"))

(defun scholia-status-send (entries)
  "Send ENTRIES to one herdr target."
  (interactive (list (scholia-status--selected-entries)))
  (unless entries
    (user-error "No annotations selected"))
  (unless (fboundp 'scholia-herdr--send)
    (require 'scholia-herdr nil t))
  (unless (fboundp 'scholia-herdr--send)
    (user-error "Scholia-herdr is unavailable"))
  (let* ((annotations (mapcar (lambda (entry) (plist-get entry :annotation))
                              entries))
         (format (or scholia-herdr-send-format scholia-export-format))
         (payload (scholia-status--payload entries format)))
    (scholia-herdr--send
     annotations payload format 'status
     (lambda (sent send) (scholia-status--persist-sent entries sent send)))))

(defvar scholia-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (keymap-set map "s" #'scholia-status-send)
    (keymap-set map "d" #'scholia-status-delete)
    (keymap-set map "g" #'scholia-status-jump)
    (keymap-set map "h" #'scholia-status-show-send-history)
    (keymap-set map "f" #'scholia-status-filter)
    (keymap-set map "M" #'scholia-status-mark-hunk)
    (keymap-set map "m" #'scholia-status-mark-session)
    (keymap-set map "e" #'scholia-status-export-marked-sessions)
    map)
  "Keymap for `scholia-status-mode'.")

(define-derived-mode scholia-status-mode magit-section-mode "Scholia Status"
  "Major mode for the scholia annotation dashboard.")

(easy-menu-define scholia-status-mode-menu scholia-status-mode-map
  "Menu for `scholia-status-mode'."
  (append
   '("Scholia Status"
     ["Send" scholia-status-send]
     ["Delete" scholia-status-delete]
     ["Jump" scholia-status-jump]
     ["Send history" scholia-status-show-send-history]
     ["Filter" scholia-status-filter]
     ["Mark annotation" scholia-status-mark-hunk]
     ["Mark session" scholia-status-mark-session]
     ["Export marked sessions" scholia-status-export-marked-sessions])
   (when (fboundp 'scholia-org-remark-export)
     '(["Export org-remark" scholia-org-remark-export]))))

(provide 'scholia-status)
;;; scholia-status.el ends here
