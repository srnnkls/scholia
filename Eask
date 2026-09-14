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
 (depends-on "corfu")
 (depends-on "evil")
 (depends-on "git-timemachine")
 (depends-on "magit")
 (depends-on "magit-section")
 (depends-on "marginalia")
 (depends-on "org-remark"))

(load-paths "./test")

;; herdr.el is unpublished: no git remote, no Eask, no recipe, so CI cannot
;; obtain it and `scholia-herdr.el' fails at its `require' before a test stub
;; can load.  Its gate therefore runs locally only.  Publishing herdr.el
;; reverses this: add a `depends-on' for it and drop the exclusion.
(defconst scholia-source-file-spec
  '("scholia*.el" (:exclude "scholia-herdr.el"))
  "File spec for the shipped package sources.")

(apply #'files scholia-source-file-spec)

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

(defun scholia-eask-command (command files)
  "Return COMMAND followed by shell-quoted FILES."
  (mapconcat #'identity
             (cons command (mapcar #'shell-quote-argument files))
             " "))

(script "compile-tests"
        (scholia-eask-command
         "eask compile --strict"
         (eask-expand-file-specs scholia-test-file-spec)))

(script "indent"
        (scholia-eask-command
         "eask lint indent --strict"
         (append (eask-expand-file-specs scholia-source-file-spec)
                 (eask-expand-file-specs scholia-test-file-spec))))

(script "checkdoc"
        (scholia-eask-command
         "eask lint checkdoc --strict"
         (eask-expand-file-specs scholia-source-file-spec)))

(script "package"
        (scholia-eask-command
         "eask lint package --strict"
         (eask-expand-file-specs scholia-source-file-spec)))

(script "clean-generated"
        (concat
         "eask clean all && emacs -Q --batch --eval "
         (shell-quote-argument
          (prin1-to-string
           '(dolist (directory (list default-directory "test"))
              (dolist (file (directory-files directory t "\\.elc\\'"))
                (delete-file file)))))))

(add-hook 'eask-before-lint/indent-hook
          (lambda ()
            (mapc #'load (eask-expand-file-specs scholia-test-file-spec))))

(add-hook 'eask-before-compile-hook
          (lambda ()
            (setq byte-compile-error-on-warn t)))
