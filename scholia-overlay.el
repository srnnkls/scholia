;;; scholia-overlay.el --- Overlay chains  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Maintainer: Sören Nikolaus <soeren@code17.io>
;; URL: https://github.com/srnnkls/scholia
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; One logical annotation is a chain: a list of overlays in buffer order,
;; one per annotated line, none of them covering a line terminator.  Every
;; overlay of a chain carries the same `scholia--chain-id' identity, so the
;; chain survives the edits that split and rejoin its lines.

;;; Code:

(require 'seq)
(require 'scholia-vars)

(defconst scholia-overlay--priority 100
  "Priority given to every annotation overlay.")

(defun scholia-overlay--buffer-end ()
  "Return the end of the buffer, whatever it is narrowed to."
  (save-restriction
    (widen)
    (point-max)))


;;;; Chain identity

(defun scholia-overlay--sort-overlays (overlays)
  "Return OVERLAYS sorted by buffer position."
  (sort overlays (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun scholia-overlay--annotations ()
  "Return every annotation overlay of the current buffer, in buffer order.
A narrowing leaves the overlays outside it just as much part of their
chain, so the scan reaches the whole buffer."
  (scholia-overlay--sort-overlays
   (seq-filter #'scholia-annotation-p
               (save-restriction
                 (widen)
                 (overlays-in (point-min) (point-max))))))

(defun scholia-overlay--chain-of (overlay)
  "Return the chain OVERLAY belongs to, in buffer order."
  (with-current-buffer (overlay-buffer overlay)
    (let ((chain-id (overlay-get overlay 'scholia--chain-id)))
      (seq-filter (lambda (other)
                    (eq (overlay-get other 'scholia--chain-id) chain-id))
                  (scholia-overlay--annotations)))))

(defun scholia-buffer-chains ()
  "Return every chain of the current buffer, ordered by where each begins.
Chains outside the current restriction answer as much as those within.
Groups the annotations of the whole buffer in one pass, so the cost of
asking stays that of a single scan however many chains answer."
  (let ((buckets nil))
    (dolist (overlay (scholia-overlay--annotations))
      (let* ((chain-id (overlay-get overlay 'scholia--chain-id))
             (bucket (assq chain-id buckets)))
        (if bucket
            (push overlay (cdr bucket))
          (push (list chain-id overlay) buckets))))
    (mapcar (lambda (bucket) (nreverse (cdr bucket))) (nreverse buckets))))


;;;; Building a chain

(defun scholia-overlay--line-segments (beg end)
  "Return the ranges one chain covering BEG to END is made of.
Each element is a cons of start and end, one per line, stopping short of
the line terminator.  Empty lines contribute no range.  BEG and END are
reached whatever the buffer is narrowed to."
  (let ((segments nil))
    (save-restriction
      (widen)
      (let ((minimum (point-min))
            (maximum (point-max)))
        (setq beg (max minimum (min beg maximum)))
        (setq end (max beg (min end maximum)))
        (save-excursion
          (goto-char beg)
          (while (< (point) end)
            (let* ((eol (line-end-position))
                   (stop (min end eol)))
              (when (< (point) stop)
                (push (cons (point) stop) segments))
              (goto-char (if (< eol end) (1+ eol) end))))))
    (nreverse segments))))

(defun scholia-overlay--owner-offset (owner)
  "Return OWNER's configured colour offset in this buffer."
  (or (plist-get (cdr (assoc-string owner scholia--session-state))
                 :color-offset)
      0))

(defun scholia-overlay--build-chain (segments text chain-id index
                                              &optional owner)
  "Create one overlay per entry of SEGMENTS and return them in buffer order.
TEXT is the annotation text, CHAIN-ID the identity shared by the
overlays, INDEX its stored colour index, and OWNER its session."
  (let* ((display-index (+ index (scholia-overlay--owner-offset owner)))
         (face (nth (mod display-index (length scholia-highlight-faces))
                    scholia-highlight-faces))
         (text-face
          (nth (mod display-index (length scholia-annotation-text-faces))
               scholia-annotation-text-faces))
        (final (car (last segments))))
    (mapcar (lambda (segment)
              (let ((overlay (make-overlay (car segment) (cdr segment) nil t)))
                (overlay-put overlay 'scholia-annotation text)
                (overlay-put overlay 'scholia--chain-id chain-id)
                (overlay-put overlay 'scholia--color-index index)
                (overlay-put overlay 'scholia--owner owner)
                (overlay-put overlay 'face face)
                (overlay-put overlay 'priority scholia-overlay--priority)
                (when (and (eq segment final)
                           (= (cdr segment) (scholia-overlay--buffer-end)))
                  (overlay-put overlay 'after-string
                               (concat "\n" (propertize text 'face text-face))))
                overlay))
            segments)))

(defun scholia-create-chain (beg end annotation-text
                                 &optional color-index owner)
  "Annotate BEG to END with ANNOTATION-TEXT and return the chain.
COLOR-INDEX is the stored colour index and OWNER is its session.
Without COLOR-INDEX the next buffer-local index is taken and advanced."
  (let ((index (or color-index scholia--colors-index-counter)))
    (unless color-index
      (setq scholia--colors-index-counter (1+ scholia--colors-index-counter)))
    (add-hook 'after-change-functions #'scholia-overlay--after-change nil t)
    (scholia-overlay--build-chain
     (scholia-overlay--line-segments beg end)
     annotation-text (gensym "scholia-chain-") index owner)))


;;;; What a chain carries

(defun scholia-chain-color-index (chain)
  "Return the stored colour index of CHAIN."
  (overlay-get (car chain) 'scholia--color-index))

(defun scholia-chain-owner (chain)
  "Return the session owning CHAIN."
  (overlay-get (car chain) 'scholia--owner))


;;;; Looking a chain up

(defun scholia-annotation-at (&optional pos)
  "Return the annotation overlay covering POS, or nil when none does.
POS defaults to point.  An overlay covers POS when it starts at or before
POS and ends after it, so of two annotations that touch at POS only the
one starting there is found.  POS is answered for wherever it lies,
inside the current restriction or outside it."
  (let ((pos (or pos (point))))
    (seq-find (lambda (overlay)
                (and (scholia-annotation-p overlay)
                     (<= (overlay-start overlay) pos)
                     (< pos (overlay-end overlay))))
              (save-restriction
                (widen)
                (overlays-in pos (1+ pos))))))

(defun scholia-chains-at (&optional pos)
  "Return all annotation chains covering POS, which defaults to point."
  (let ((pos (or pos (point))))
    (seq-filter
     (lambda (chain)
       (seq-some (lambda (overlay)
                   (and (<= (overlay-start overlay) pos)
                        (< pos (overlay-end overlay))))
                 chain))
     (scholia-buffer-chains))))

(defun scholia-chain-at (pos)
  "Return the first chain covering POS, or nil when none does."
  (car (scholia-chains-at pos)))

(defun scholia-chain-first-p (overlay)
  "Return non-nil when OVERLAY begins the chain it belongs to."
  (scholia-ensure-annotation (overlay)
    (eq overlay (car (scholia-overlay--chain-of overlay)))))

(defun scholia-chain-last-p (overlay)
  "Return non-nil when OVERLAY ends the chain it belongs to.
Export and send paths filter on this, so exactly one overlay per chain
answers non-nil."
  (scholia-ensure-annotation (overlay)
    (eq overlay (car (last (scholia-overlay--chain-of overlay))))))

(defun scholia-delete-chain (overlay)
  "Delete every overlay of the chain OVERLAY belongs to."
  (scholia-ensure-annotation (overlay)
    (mapc #'delete-overlay (scholia-overlay--chain-of overlay))))


;;;; Navigation

(defun scholia-next-annotation (pos)
  "Return the first chain starting after POS, or nil when none does.
The chain covering POS is never returned, however many overlays of it
start after POS."
  (seq-find (lambda (chain) (> (overlay-start (car chain)) pos))
            (scholia-buffer-chains)))

(defun scholia-previous-annotation (pos)
  "Return the last chain ending at or before POS, or nil when none does.
The chain covering POS is never returned, however many overlays of it
end before POS."
  (car (last (seq-filter (lambda (chain)
                           (<= (overlay-end (car (last chain))) pos))
                         (scholia-buffer-chains)))))


;;;; Re-chaining after an edit

(defun scholia-overlay--rechain (chain)
  "Rebuild CHAIN so it carries one overlay per line again.
An edit can leave a chain spanning a newline it did not span before, or
leave nothing of the annotated text at all, in which case CHAIN goes away
rather than lingering as a zero-length overlay.  A chain whose lines came
through an edit unchanged is rebuilt too when it renders its text in an
`after-string' without ending the buffer any more, or ends it without
rendering anything."
  (let* ((template (car chain))
         (final (car (last chain)))
         (segments (scholia-overlay--line-segments
                    (overlay-start template)
                    (seq-max (mapcar #'overlay-end chain)))))
    (unless (and (equal segments
                        (mapcar (lambda (overlay)
                                  (cons (overlay-start overlay)
                                        (overlay-end overlay)))
                                chain))
                 (eq (and segments
                          (= (cdar (last segments))
                             (scholia-overlay--buffer-end)))
                     (and (overlay-get final 'after-string) t)))
      (let ((text (overlay-get template 'scholia-annotation))
            (chain-id (overlay-get template 'scholia--chain-id))
            (index (overlay-get template 'scholia--color-index))
            (owner (overlay-get template 'scholia--owner))
            (revision (overlay-get template 'scholia-core--revision)))
        (mapc #'delete-overlay chain)
        (let ((rebuilt (scholia-overlay--build-chain
                        segments text chain-id index owner)))
          (when (and rebuilt revision)
            (overlay-put (car rebuilt) 'scholia-core--revision revision)))))))

(defun scholia-overlay--after-change (beg end _length)
  "Re-chain every annotation the change between BEG and END touched.
A chain is taken as touched when the change falls between its first and
its last overlay, so an edit on a line the chain covers without holding
an overlay on it re-chains as any other does.  Runs from
`after-change-functions' rather than `post-command-hook', so a
programmatic edit or an undo re-chains just as typing does."
  (dolist (chain (scholia-buffer-chains))
    (when (and (<= (overlay-start (car chain)) end)
               (<= beg (overlay-end (car (last chain)))))
      (scholia-overlay--rechain chain))))

(defun scholia-disarm-rechaining ()
  "Stop re-chaining the annotations of this buffer after an edit.
Undoes the arming `scholia-create-chain' does, so a buffer whose
annotations have been taken down carries no hook of ours any more."
  (remove-hook 'after-change-functions #'scholia-overlay--after-change t))

(provide 'scholia-overlay)
;;; scholia-overlay.el ends here
