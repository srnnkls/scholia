;;; scholia-locate.el --- Resolve source locations  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Keywords: convenience, tools

;;; Commentary:

;; Turns buffer positions and stored access descriptors into source views.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'scholia-db)

(defvar scholia-location-functions nil
  "Abnormal hook resolving a buffer position to a source location.
Each function receives a position and returns a location plist or nil.")

(defvar scholia-source-functions nil
  "Abnormal hook dispatching operations on source access descriptors.
Each function receives an operation, an access plist and an annotation.")

(defvar-local scholia-locate--terminally-unresolved nil
  "Whether this buffer must not use the default file resolver.")

(defvar-local scholia-locate--buffer-id nil
  "Opaque identity used to retrieve this buffer as a source.")

(defun scholia-locate-file-position (position)
  "Return the source location at POSITION in a file-visiting buffer."
  (unless scholia-locate--terminally-unresolved
    (when-let ((file (buffer-file-name (or (buffer-base-buffer)
                                           (current-buffer)))))
      (save-excursion
        (goto-char position)
        (list :file (expand-file-name file)
              :line (line-number-at-pos position t)
              :column (- position (line-beginning-position))
              :end-column (- (line-end-position) (line-beginning-position))
              :revision nil)))))

(add-hook 'scholia-location-functions #'scholia-locate-file-position t)

(defun scholia-locate--make-buffer-id ()
  "Return an opaque identity for the current buffer."
  (concat "buffer:"
          (md5 (format "%S:%S:%S:%S"
                       (current-buffer) (float-time) (random) (buffer-name)))))

(defun scholia-locate--generic-position (position)
  "Return a generic buffer source location at POSITION."
  (setq scholia-locate--buffer-id
        (or scholia-locate--buffer-id (scholia-locate--make-buffer-id)))
  (save-excursion
    (goto-char position)
    (list :file scholia-locate--buffer-id
          :source-id scholia-locate--buffer-id
          :line (line-number-at-pos position t)
          :column (- position (line-beginning-position))
          :end-column (- (line-end-position) (line-beginning-position))
          :revision nil
          :access (list :kind 'buffer
                        :id scholia-locate--buffer-id
                        :name (buffer-name)
                        :mode major-mode))))

(defun scholia-locate--access (location)
  "Return the serializable access descriptor for LOCATION."
  (or (plist-get location :access)
      (let ((file (plist-get location :file))
            (revision (plist-get location :revision)))
        (if revision
            (list :kind 'git :path file :revision revision)
          (list :kind 'file :path file)))))

(defun scholia-locate-position (position)
  "Capture the source location at buffer POSITION."
  (let* ((location
          (or (run-hook-with-args-until-success
               'scholia-location-functions position)
              (scholia-locate--generic-position position)))
         (access (and location (scholia-locate--access location))))
    (when location
      (scholia-db--with-fields
       location :source-id (or (plist-get location :source-id)
                               (plist-get location :file))
       :access access))))

(defun scholia-locate--annotation (annotations)
  "Return the first annotation represented by ANNOTATIONS."
  (if (and (listp annotations) (keywordp (car annotations)))
      annotations
    (car annotations)))

(defun scholia-locate--annotations (annotations)
  "Return ANNOTATIONS as a list."
  (if (and (listp annotations) (keywordp (car annotations)))
      (list annotations)
    annotations))

(defun scholia-locate--snapshot (location)
  "Return serialized snapshot fields captured from LOCATION's source."
  (let* ((access (scholia-locate--access location))
         (retrieved (run-hook-with-args-until-success
                     'scholia-source-functions 'retrieve access nil))
         (text (plist-get retrieved :text)))
    (when text
      (if (and (eq scholia-source-snapshot-mode 'bounded-full)
               (<= (string-bytes text) scholia-source-snapshot-limit))
          (list :access access
                :source-id (plist-get location :source-id)
                :snapshot text
                :snapshot-status 'full
                :snapshot-truncated nil)
        (list :access access
              :source-id (plist-get location :source-id)
              :snapshot nil
              :snapshot-status 'excerpt
              :snapshot-truncated
              (> (string-bytes text) scholia-source-snapshot-limit))))))

(defun scholia-locate--context (location)
  "Return LOCATION's line context from its exact source."
  (when-let* ((access (scholia-locate--access location))
              (retrieved (run-hook-with-args-until-success
                          'scholia-source-functions 'retrieve access nil))
              (text (plist-get retrieved :text)))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (when (zerop (forward-line (1- (plist-get location :line))))
        (let ((bol (line-beginning-position)))
          (list :line-text
                (buffer-substring-no-properties bol (line-end-position))))))))

(defun scholia-locate--excerpt (annotations)
  "Return line-context source text retained by ANNOTATIONS."
  (let* ((placed
          (seq-filter #'scholia-db-annotation-line
                      (scholia-locate--annotations annotations)))
         (last-line (and placed
                         (apply #'max
                                (mapcar #'scholia-db-annotation-line placed))))
         (lines (and last-line (make-list last-line ""))))
    (dolist (annotation placed)
      (setf (nth (1- (scholia-db-annotation-line annotation)) lines)
            (scholia-db-annotation-line-text annotation)))
    (string-join lines "\n")))

(defun scholia-locate--buffer (id)
  "Return the live source buffer identified by ID."
  (seq-find
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (with-current-buffer buffer
            (equal scholia-locate--buffer-id id))))
   (buffer-list)))

(defun scholia-locate--file-source (operation access _annotation)
  "Apply OPERATION to file ACCESS, or return nil when it is not a file."
  (when (eq (plist-get access :kind) 'file)
    (let ((file (plist-get access :path)))
      (pcase operation
        ('retrieve
         (condition-case nil
             (with-temp-buffer
               (insert-file-contents file)
               (list :text (buffer-string) :status 'live))
           (error nil)))
        ('open (when (file-readable-p file) (find-file-noselect file)))
        ('describe file)))))

(defun scholia-locate--git-source (operation access _annotation)
  "Apply OPERATION to Git ACCESS, or return nil when it is not Git source."
  (when (eq (plist-get access :kind) 'git)
    (let ((file (plist-get access :path))
          (revision (plist-get access :revision)))
      (pcase operation
        ('retrieve
         (when-let ((buffer (scholia-locate--revision-buffer file revision)))
           (unwind-protect
               (with-current-buffer buffer
                 (list :text (buffer-string) :status 'live))
             (kill-buffer buffer))))
        ('open (scholia-locate--revision-buffer file revision))
        ('describe (format "%s at %s" file revision))))))

(defun scholia-locate--buffer-source (operation access _annotation)
  "Apply OPERATION to buffer ACCESS, or return nil for another source kind."
  (when (eq (plist-get access :kind) 'buffer)
    (let ((buffer (scholia-locate--buffer (plist-get access :id))))
      (pcase operation
        ('retrieve
         (when buffer
           (with-current-buffer buffer
             (list :text (save-restriction
                           (widen)
                           (buffer-substring-no-properties
                            (point-min) (point-max)))
                   :status 'live))))
        ('open buffer)
        ('describe
         (format "%s (%s; buffer %s)"
                 (plist-get access :name)
                 (plist-get access :mode)
                 (plist-get access :id)))))))

(add-hook 'scholia-source-functions #'scholia-locate--file-source t)
(add-hook 'scholia-source-functions #'scholia-locate--git-source t)
(add-hook 'scholia-source-functions #'scholia-locate--buffer-source t)

(defun scholia-locate-source (operation subject &optional annotations)
  "Apply source OPERATION to SUBJECT with optional ANNOTATIONS.
SUBJECT is a position for `capture' and `snapshot', otherwise an access
plist.  Retrieval falls back from a live adapter to a full snapshot and
then to saved line excerpts."
  (pcase operation
    ('capture (scholia-locate-position subject))
    ('snapshot (scholia-locate--snapshot subject))
    ('context (scholia-locate--context subject))
    (_
     (let* ((annotation (scholia-locate--annotation annotations))
            (live (run-hook-with-args-until-success
                   'scholia-source-functions operation subject annotation)))
       (if (or live (not (eq operation 'retrieve)))
           live
         (if-let ((snapshot (seq-some (lambda (candidate)
                                        (plist-get candidate :snapshot))
                                      (scholia-locate--annotations annotations))))
             (list :text snapshot :status 'full
                   :truncated (plist-get annotation :snapshot-truncated))
           (list :text (scholia-locate--excerpt annotations)
                 :status 'excerpt
                 :truncated
                 (plist-get annotation :snapshot-truncated))))))))

(defun scholia-locate-revision (annotation)
  "Return the revision stored for ANNOTATION."
  (or (plist-get annotation :revision)
      (plist-get (plist-get annotation :access) :revision)))

(defun scholia-locate--revision-buffer (file revision)
  "Return a buffer holding FILE at REVISION, or nil."
  (with-temp-buffer
    (let ((default-directory (file-name-directory file)))
      (when (zerop (process-file "git" nil t nil "rev-parse" "--show-toplevel"))
        (let ((root (string-trim (buffer-string))))
          (erase-buffer)
          (when (zerop
                 (process-file
                  "git" nil t nil "-C" root "show"
                  (concat revision ":"
                          (file-relative-name (file-truename file) root))))
            (let ((source (buffer-string))
                  (buffer (generate-new-buffer
                           (format "*scholia %s:%s*"
                                   revision
                                   (file-name-nondirectory file)))))
              (with-current-buffer buffer
                (insert source)
                (set-buffer-modified-p nil))
              buffer)))))))

(defun scholia-locate--materialize (annotation &optional rematerialize)
  "Return ANNOTATION with its line and columns turned into bounds.
When REMATERIALIZE is non-nil, replace stored bounds against this source."
  (if (or (scholia-db-annotation-reply-p annotation)
          (and (not rematerialize) (scholia-db-annotation-beg annotation)))
      annotation
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (when (zerop (forward-line (1- (scholia-db-annotation-line annotation))))
          (let* ((bol (line-beginning-position))
                 (eol (line-end-position))
                 (beg (min (+ bol (scholia-db-annotation-column annotation)) eol))
                 (end (min (+ bol (scholia-db-annotation-end-column annotation))
                           eol)))
            (scholia-db-annotation-set-bounds annotation beg (max beg end))))))))

(defun scholia-locate--in-source (access annotations function)
  "Call FUNCTION in source ACCESS resolved for ANNOTATIONS."
  (let ((retrieved (scholia-locate-source 'retrieve access annotations)))
    (when retrieved
      (with-temp-buffer
        (insert (plist-get retrieved :text))
        (funcall function)))))

(defun scholia-locate-materialize (file annotation)
  "Return ANNOTATION with bounds resolved against its source view of FILE."
  (if (or (scholia-db-annotation-reply-p annotation)
          (scholia-db-annotation-beg annotation))
      annotation
    (let ((access (or (plist-get annotation :access)
                      (if-let ((revision
                                (scholia-locate-revision annotation)))
                          (list :kind 'git :path file :revision revision)
                        (list :kind 'file :path file)))))
      (or (condition-case nil
              (scholia-locate--in-source
               access annotation
               (lambda () (scholia-locate--materialize annotation)))
            (error annotation))
          annotation))))

(defun scholia-locate-open (file annotation)
  "Open FILE at ANNOTATION's stored source location."
  (let* ((access (or (plist-get annotation :access)
                     (if-let ((revision
                               (scholia-locate-revision annotation)))
                         (list :kind 'git :path file :revision revision)
                       (list :kind 'file :path file))))
         (buffer (scholia-locate-source 'open access annotation))
         (retrieved nil)
         (fallback nil))
    (unless buffer
      (setq retrieved (scholia-locate-source 'retrieve access annotation))
      (when retrieved
        (setq fallback (not (eq (plist-get retrieved :status) 'live))
              buffer
              (generate-new-buffer
               (format "*scholia %s*" (or (plist-get access :name) file))))
        (with-current-buffer buffer
          (when (eq (plist-get access :kind) 'buffer)
            (setq-local scholia-locate--buffer-id (plist-get access :id)))
          (insert (plist-get retrieved :text))
          (when-let ((mode (plist-get access :mode)))
            (when (fboundp mode) (delay-mode-hooks (funcall mode))))
          (set-buffer-modified-p nil))))
    (unless buffer
      (when (file-readable-p file)
        (setq buffer (find-file-noselect file) fallback t)))
    (unless buffer (user-error "Source is unavailable"))
    (switch-to-buffer buffer)
    (setq annotation
          (if fallback
              (or (scholia-db--relocate annotation)
                  (scholia-locate--materialize annotation t))
            (scholia-locate--materialize annotation)))
    (unless (scholia-db-annotation-beg annotation)
      (user-error "Annotation has no stored position"))
    (goto-char (scholia-db-annotation-beg annotation))
    annotation))

(provide 'scholia-locate)
;;; scholia-locate.el ends here
