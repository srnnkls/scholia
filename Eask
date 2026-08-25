;; -*- mode: eask; lexical-binding: t -*-

(package "scholia"
         "0.1.0"
         "Annotations as work items")

(website-url "https://github.com/srnnkls/scholia")

(keywords "convenience" "tools")

(package-file "scholia.el")

(script "test" "eask test ert")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "29.1")

(add-hook 'eask-before-compile-hook
          (lambda ()
            (setq byte-compile-error-on-warn t)))
