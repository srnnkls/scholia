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

Annotate in a buffer and the note goes into the resolved default session; annotating also turns
`scholia-mode` on, which is what saves the note on kill and keeps it placed as the file is edited.
No integration package or customization is required. The first optional setting is
`scholia-session-directory`, which chooses where named sessions live:

```emacs-lisp
(setq scholia-session-directory "~/.emacs.d/scholia/")
```

## Workflow

`scholia-annotate` creates an annotation for the active region, or the symbol at point.
`scholia-edit-annotation` (`C-c C-c`) edits the annotation at point;
`scholia-delete-annotation` removes it; `scholia-reply-to` adds a reply. Replies form threads,
rendered in `scholia-status` and in buffer and session exports.

### Annotation editor

The global setting `scholia-annotation-editor` selects the input interface for
`scholia-annotate` (`C-c C-a`) and `scholia-edit-annotation` (`C-c C-c`). Set it once
in your Emacs configuration to use inline input in every buffer:

```emacs-lisp
(setq scholia-annotation-editor 'inline)
```

Set it to `minibuffer` to use the original interface; this remains the default.
Both interfaces offer previously stored annotation text
as completion candidates and accept new text freely. Editing keeps the annotation's identity,
owning session, source range, and thread.

```text
╭ Document line at the annotation point
╰ What is the expected behav█
  ┌─────────────────────────────────────┐
  │ What is the expected behavior here? │
  │ What is the intended output?        │
  └─────────────────────────────────────┘
Next document line
```

The temporary input uses your theme's Org block face on a subtly darkened background,
with a small bracket beside the text connecting it to the first selected line. Selected
words are underlined immediately, and the input appears after the last selected line.
There is no header
or keybinding banner. Display space below the input follows the visible rows of the
ordinary [Corfu popup](https://github.com/minad/corfu) and disappears when it closes.
Corfu retains your configured width, faces, and candidate count.
Install Corfu to get the popup. Without it, C-SPC uses standard Emacs
completion at point. Corfu supplies GUI rendering and native terminal support on Emacs 31;
older terminal Emacs needs [corfu-terminal](https://codeberg.org/akib/emacs-corfu-terminal).

- **RET** uses your normal completion binding while the popup is open and saves the
  annotation when it is closed. **C-c C-c** saves directly.
- **C-g** cancels, leaving an existing annotation intact.
- **C-SPC** opens annotation history on demand. Opening the field and typing prose
  leave history suggestions closed; the popup keeps your configured Corfu keys.
- Other editing bindings remain active, including **ESC** and Meta prefixes. Evil users
  start in insert state and return to their previous state when the editor closes.
  Status buffers temporarily use a text editing map so typing and reentering insert
  state work there too; their original map returns when input closes.

Type **@** to open filename suggestions automatically, for example `See @src/parser.el`. In a project,
the candidates are project files, inserted as paths relative to its root. Elsewhere,
completion uses the current directory and supports browsing subdirectories. The `@`
stays in the note. Outside a filename mention, C-SPC offers recurring annotation text.

Accepting input first rolls back the temporary text, then creates or updates and stores the
annotation. The document text, undo history, modified flag, and local completion settings
are restored on save, cancellation, and errors. The document is protected from edits and
saves while the field is open. Read-only source buffers are supported.

Customize `scholia-edit-body` and `scholia-edit-border` for the input's appearance.

### How annotations appear

An annotated range is underlined in its session's colour, and the note is drawn beside the
last line it covers, starting at `scholia-annotation-column` and wrapping at the window
edge. A note steps aside while the field editing it is open. Replies
are drawn under the note, set in by `scholia-render-reply-indent` columns per
level and losing `scholia-reply-tint-step` of the colour's saturation at each, so a thread
reads as one hue fading inwards. With `scholia-annotation-authors` on, new annotations and
replies record their author (the repository's git `user.name` and `user.email`), and each
stored author is drawn under what they wrote; `scholia-toggle-annotation-authors` flips it
everywhere. Nothing is written to the buffer: the file is untouched,
the modified flag is unmoved, and undo never sees a note.

### Sessions

Two things are kept apart: the *target*, where a new annotation goes, and *visibility*,
which sessions are drawn.

The target is the buffer-local `scholia-session`, its project assignment, or the global
default. `scholia-session-switch` moves the global target and draws that session alone;
with a prefix argument the sessions drawn until now stay drawn beside it.
`scholia-session-create` makes an empty session and switches to it.

Visibility is `scholia-session-hide`, `scholia-session-show` and `scholia-session-toggle`,
none of which touch the target. The target is always drawn, so hiding it is refused —
switch away from it first.

By default visibility belongs to the sitting: a restart draws the target alone. Set
`scholia-persist-visibility` to keep it, and the shown sessions and the global target are
written to `scholia-session-state-file` and read back the first time an annotated buffer is
drawn:

```emacs-lisp
(setq scholia-persist-visibility t)
```

Each session takes a colour of its own, claimed from `scholia-session-colors` the first
time it is drawn and stored in its own header, so it keeps that colour across restarts and
however many sessions are made, renamed or removed beside it. Two sessions are never drawn
in one colour: past the configured colours the hue wheel turns on for the rest.

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
      :desc "Edit annotation" "c" #'scholia-edit-annotation
      :desc "Delete annotation" "d" #'scholia-delete-annotation
      :desc "Reply" "r" #'scholia-reply-to
      :desc "Next annotation" "n" #'scholia-goto-next-annotation
      :desc "Previous annotation" "p" #'scholia-goto-previous-annotation
      :desc "Create session" "s c" #'scholia-session-create
      :desc "Switch session" "s s" #'scholia-session-switch
      :desc "Show session" "s v" #'scholia-session-show
      :desc "Hide session" "s h" #'scholia-session-hide
      :desc "Toggle session" "s t" #'scholia-session-toggle
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
