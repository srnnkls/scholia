# scholia

scholia keeps annotations beside source files, in named sessions, and sends or exports them
where work happens.

## Install and start

```sh
git clone https://github.com/srnnkls/scholia.git ~/.emacs.d/site-lisp/scholia
```

```emacs-lisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/scholia")
(require 'scholia)
```

Enable `scholia-mode` in a buffer to annotate immediately into the resolved default session. No
integration package or customization is required. The first optional setting is
`scholia-session-directory`, which chooses where named sessions live:

```emacs-lisp
(setq scholia-session-directory "~/.emacs.d/scholia/")
```

## Workflow

`scholia-annotate` creates an annotation for the active region, or the symbol at point.
`scholia-delete-annotation` removes it; `scholia-reply-to` adds a reply. Replies form threads,
rendered in `scholia-status` and in buffer and session exports.

A buffer's write target is its buffer-local `scholia-session`, its project assignment, or the
global default. `scholia-session-create` and `scholia-session-switch` change the global target.
`scholia-session-activate` shows another session in every annotated buffer without changing that
target; `scholia-session-deactivate` hides it. Visible sessions use offsets from
`scholia-session-color-cycle` so their annotations remain distinguishable.

A new annotation goes to the write target. With a prefix argument, `scholia-annotate` selects
another visible session. Replies and deletes always follow the selected annotation's owning
session. `scholia-export` exports every visible session in the current buffer; with a prefix
argument, it selects one. Use `scholia-export-session` for one or more complete sessions.

Import a session with `scholia-session-import`. Find annotations across sessions with
`scholia-search`, or sent work with `scholia-search-sends`. `scholia-status` is the session
dashboard.

## Sources and recovery

Files and arbitrary live buffers work without an integration module. Each annotation records a
serializable access descriptor; file, Git revision, and buffer descriptors identify how to reopen
or describe the source. Magit, Ediff, and git-timemachine supply revision-aware locations after
their optional resolver setup functions are enabled.

`scholia-source-snapshot-mode` controls retained source text. The default, `bounded-full`, keeps a
complete snapshot up to `scholia-source-snapshot-limit`; larger sources fall back to saved line
excerpts and are marked truncated. Set the mode to `excerpts` to retain excerpts only. Session
exports label every source view with its access description and whether the rendered source is
`live`, `full`, or `excerpt`; truncated excerpt fallback is labeled too.

## Optional integrations

Load and enable only the integrations you use:

```emacs-lisp
(require 'scholia-magit)
(scholia-magit-setup)

(require 'scholia-ediff)
(scholia-ediff-setup)

(require 'scholia-timemachine)
(scholia-timemachine-setup)

(require 'scholia-ui)
(scholia-ui-marginalia-setup)
```

These setup functions are idempotent. Their matching teardown functions remove Scholia's hooks,
advice, or Marginalia registration:

```emacs-lisp
(scholia-magit-teardown)
(scholia-ediff-teardown)
(scholia-timemachine-teardown)
(scholia-ui-marginalia-teardown)
```

The dashboard and Org Remark exporter are commands from separate modules:

```emacs-lisp
(require 'scholia-status)
(require 'scholia-org-remark)
```

Generic buffers and ordinary files use the built-in source adapters. Magit, Ediff,
git-timemachine, Marginalia, the dashboard, and Org Remark remain optional.

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
      :desc "Activate session" "s a" #'scholia-session-activate
      :desc "Deactivate session" "s d" #'scholia-session-deactivate
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
