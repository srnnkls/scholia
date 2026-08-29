# scholia

scholia keeps annotations beside source files, in named sessions, and sends or exports them
where work happens.

## Install and start

```sh
git clone https://github.com/srnnkls/scholia.git ~/.emacs.d/site-lisp/scholia
```

```emacs-lisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/scholia")
(use-package magit-section :ensure t)
(use-package scholia
  :ensure nil
  :demand t
  :config
  (require 'scholia-status))
```

Enable `scholia-mode` in a buffer to annotate it. No customization is required. The first
optional setting is `scholia-session-directory`, which chooses where named sessions live:

```emacs-lisp
(setq scholia-session-directory "~/.emacs.d/scholia/")
```

## Workflow

`scholia-annotate` creates an annotation for the active region, or the symbol at point.
`scholia-delete-annotation` removes it; `scholia-reply-to` adds a reply. Replies form threads,
rendered in `scholia-status` and in buffer and session exports.

Create, change, or import a session with `scholia-session-create`, `scholia-session-switch`, and
`scholia-session-import`. Find annotations across sessions with `scholia-search`, or sent work
with `scholia-search-sends`. Use `scholia-export` for the current buffer and
`scholia-export-session` for one or more sessions. `scholia-status` is the session dashboard.

## Optional integrations

Load the location resolvers, dashboard, and Org Remark export explicitly:

```emacs-lisp
(use-package magit :ensure t)
(use-package git-timemachine :ensure t)
(use-package org-remark :ensure t)
(require 'ediff-vers)
(require 'scholia-magit)
(require 'scholia-ediff)
(require 'scholia-timemachine)
(require 'scholia-status)
(require 'scholia-org-remark)
```

Magit, Ediff, and git-timemachine are the supported location resolvers. Fileless-buffer reopen
support was cut: the supported transient-buffer workflows are covered by those resolvers.

`scholia-herdr` is source-tree/local-only: both it and its unpublished `herdr` dependency are
excluded from the package. When both are available locally, load it with `(require 'scholia-herdr)`.
`scholia-org-remark-export` exports sessions to Org Remark.

If an old Doom configuration defines `+annotate-export-rustc` or
`+annotate--fix-integrate-padding`, delete both. `scholia-export` replaces the former, and
`scholia-export-integrate` replaces the latter.

## Doom

`SPC a` is unclaimed by Doom's default leader and is a suggestion, not a required prefix.
Add this to a Doom configuration if it suits your leader layout:

```emacs-lisp
(map! :leader
      :prefix ("a" . "scholia")
      :desc "Annotate" "a" #'scholia-annotate
      :desc "Delete annotation" "d" #'scholia-delete-annotation
      :desc "Reply" "r" #'scholia-reply-to
      :desc "Next annotation" "n" #'scholia-goto-next-annotation
      :desc "Previous annotation" "p" #'scholia-goto-previous-annotation
      :desc "Create session" "s c" #'scholia-session-create
      :desc "Switch session" "s s" #'scholia-session-switch
      :desc "Import session" "s i" #'scholia-session-import
      :desc "Search annotations" "f" #'scholia-search
      :desc "Search sends" "F" #'scholia-search-sends
      :desc "Export buffer" "e b" #'scholia-export
      :desc "Export sessions" "e s" #'scholia-export-session
      :desc "Export Org Remark" "e o" #'scholia-org-remark-export
      :desc "Send annotation" "h a" #'scholia-herdr-send
      :desc "Send region" "h r" #'scholia-herdr-send-region
      :desc "Send file" "h f" #'scholia-herdr-send-file
      :desc "Send session" "h s" #'scholia-herdr-send-session
      :desc "Status" "S" #'scholia-status)
```

scholia ships no Doom binding file: `map!` is a Doom macro, and package CI cannot byte-compile a
file that calls it without Doom.

## Credit

[annotate.el](https://github.com/bastibe/annotate.el), by Bastian Bechtold and contributors, is
prior art for scholia. Its source was consulted for edge cases covered by scholia's tests; no
annotate.el code is copied or retained here.

## License

MIT. See [LICENSE](LICENSE).
