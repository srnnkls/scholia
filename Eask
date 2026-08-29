;; -*- mode: eask; lexical-binding: t -*-

(package "scholia"
         "0.1.0"
         "Annotations as work items")

(website-url "https://github.com/srnnkls/scholia")

(keywords "convenience" "tools")

(package-file "scholia.el")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "29.1")

(development
 (depends-on "git-timemachine")
 (depends-on "magit")
 (depends-on "magit-section")
 (depends-on "org-remark"))

(load-paths "./test")

;; herdr.el is unpublished: no git remote, no Eask, no recipe, so CI cannot
;; obtain it and `scholia-herdr.el' fails at its `require' before a test stub
;; can load.  Its gate therefore runs locally only.  Publishing herdr.el
;; reverses this: add a `depends-on' for it and drop the exclusion.
(files "scholia*.el" '(:exclude "scholia-herdr.el"))

(defconst scholia-test-file-spec
  '("test/scholia*.el" (:exclude "test/scholia-herdr-test.el"))
  "File spec for the ERT suites, which `files' deliberately does not ship.")

(script "test"
        ;; `eask-file-root' is only assigned after this file finishes loading,
        ;; so the Eask file's own directory has to come from `load-file-name'.
        (let* ((default-directory (file-name-directory load-file-name))
               (files (eask-expand-file-specs scholia-test-file-spec)))
          (unless files
            (error "No test file matches %S under %s"
                   scholia-test-file-spec default-directory))
          (mapconcat #'identity
                     (cons "eask test ert"
                           (mapcar #'shell-quote-argument files))
                     " ")))

(add-hook 'eask-before-compile-hook
          (lambda ()
            (setq byte-compile-error-on-warn t)))
