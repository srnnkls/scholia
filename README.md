# scholia

*σχόλιον • a remark set against a passage*

## About

scholia annotates files in Emacs without changing them. You mark a region, write a note, and the
note is drawn beside the line it belongs to, in the colour of the session it went into. The file on
disk, its modified flag and its undo history stay untouched. Replies turn a note into a thread.

Annotations live in named *sessions*, one SQLite database each, so a review, a refactoring plan
and a list of questions for a colleague stay apart and can be shown side by side. Every annotation
records how to reach its source again: a file, a file at a Git revision, or a live buffer. That
makes a session something to work through later: search it, browse it in a dashboard, export it as
rustc-style diagnostics, a diff or commented source, or send it to an agent.

Reach for scholia when you read code and want to leave remarks you will act on later, when you
review a change and want notes tied to the lines they discuss, or when you hand a set of findings to
another person or tool. `scholia-forge` applies the same model to GitHub pull requests: their
comments appear as threads in a Magit diff, and your replies wait as drafts until you submit them.

## Installation

scholia needs Emacs 29.1 or newer, built with SQLite support, and
[cera](https://github.com/srnnkls/cera), which draws the notes and the input field. Clone both and
put them on the load path:

```sh
git clone https://github.com/srnnkls/cera.git ~/.emacs.d/site-lisp/cera
git clone https://github.com/srnnkls/scholia.git ~/.emacs.d/site-lisp/scholia
```

```emacs-lisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/cera")
(add-to-list 'load-path "~/.emacs.d/site-lisp/scholia")
(require 'scholia)
```

Nothing else is required. Sessions are stored under `scholia-session-directory`, which defaults to
`scholia/sessions/` in your Emacs directory.

## Getting started

Open any file, select a few words, and run `M-x scholia-annotate`. Type a note and press `RET`. The
selected text is underlined and the note appears to the right of the line, from column 85. Annotating
turns on `scholia-mode`, whose keys are active from then on.

Put point in the underlined text and press `C-c C-r` to reply. The reply is drawn under the note, set
in by two columns. `C-c C-c` changes the note, `C-c C-d` deletes it, and `C-c C-n` and `C-c C-p`
move between annotations.

Kill the buffer and open the file again: the note is back, because `scholia-mode` stores the
buffer's annotations when the buffer is killed and when Emacs exits.

Press `C-c C-e` to see the buffer's annotations as a rustc-style diagnostic:

```text
 --> /home/me/src/greet.el:2:4 [c89c2e42-b8a6-11f1-b5a7-01a6851d9858]
  |
2 |   (message "Hello, %s" name))
  |    ^^^^^^^ Use format-message here?
  |              [c89c2e6a-b8a6-11f1-9e4c-010cee6f04ed] Agreed, it quotes properly.
  |
```

Everything so far went into the session called `default`. Run `M-x scholia-session-create` to start
another one; new annotations go there until you switch back with `M-x scholia-session-switch`.

## Commands and keys

`scholia-mode-map`, active in annotated buffers:

| Key | Command | Does |
| --- | --- | --- |
| `C-c C-a` | `scholia-annotate` | annotate the region or the symbol at point |
| `C-c C-c` | `scholia-edit-annotation` | change the note at point |
| `C-c C-d` | `scholia-delete-annotation` | delete the annotation at point |
| `C-c C-r` | `scholia-reply-to` | reply to the annotation at point |
| `C-c C-e` | `scholia-export` | export the buffer's annotations |
| `C-c C-f` | `scholia-search` | find an annotation in any session and visit it |
| `C-c C-n` | `scholia-goto-next-annotation` | move to the next annotation |
| `C-c C-p` | `scholia-goto-previous-annotation` | move to the previous annotation |

Commands without a key:

| Command | Does |
| --- | --- |
| `scholia-session-create` | make a session and annotate into it |
| `scholia-session-switch` | annotate into another session |
| `scholia-session-show`, `-hide`, `-toggle` | draw or stop drawing a session beside the target |
| `scholia-session-rename`, `-delete` | rename or delete a session |
| `scholia-session-export`, `-import` | write a session to a file, or merge one in |
| `scholia-session-assign-project` | tie a project to a session |
| `scholia-export-session` | export whole sessions |
| `scholia-search-sends` | find an annotation by where it was sent |
| `scholia-toggle-annotation-authors` | record and show authors, or stop |
| `scholia-status` | open the dashboard of every session |
| `scholia-forge-diff-pullreq` | show the forge pull request at point with its comments |

[REFERENCE.md](REFERENCE.md) lists every command, key and option.

## Concepts

| Term | Meaning |
| --- | --- |
| *annotation* | a note attached to a range of text, with an id, an owner session and a source |
| *chain* | the overlays that mark one annotation's range, one per line it spans |
| *note* | the annotation's text as drawn beside or below its last line |
| *reply* | an annotation that answers another annotation or reply; together they form a *thread* |
| *session* | a named collection of annotations, stored as one database |
| *write target* | the session a new annotation in this buffer goes to |
| *visible sessions* | the sessions drawn in annotated buffers besides the write target |
| *source* | where an annotation's text lives: a file, a file at a revision, or a buffer |
| *snapshot* | source text kept with a session so an export can show it after the source changed |
| *send* | a record, kept on an annotation, of an export delivered to a herdr agent or pane |

## Documentation

- [GUIDE.md](GUIDE.md) explains how scholia works and walks through each task, starting at
  [How scholia works](GUIDE.md#how-scholia-works).
- [REFERENCE.md](REFERENCE.md) lists every command, key binding, option, hook and variable.
- [CHANGELOG.md](CHANGELOG.md) records what changed in each release.

## Development

The package is built and tested with [Eask](https://emacs-eask.github.io/):

```sh
eask install-deps --dev          # cera plus the optional integrations the tests load
eask compile --strict            # byte-compile the sources, warnings as errors
eask run script test             # the ERT suites under test/
eask run script compile-tests    # byte-compile the suites
eask run script indent           # check indentation of sources and tests
eask run script checkdoc         # check docstrings
eask run script package          # package lint
```

Each module is one file:

- `scholia.el`: `scholia-mode`, its keymap and the autoloads.
- `scholia-vars.el`: the customization group and the shared options.
- `scholia-core.el`: session resolution, the annotation commands, saving and restoring.
- `scholia-overlay.el`, `scholia-render.el`, `scholia-color.el`: chains, notes and colours.
- `scholia-edit.el`, `scholia-ui.el`: reading a note inline or in the minibuffer.
- `scholia-session.el`: the session commands and project assignments.
- `scholia-db.el`, `scholia-store.el`: the session database.
- `scholia-locate.el`: source access for files, revisions and buffers.
- `scholia-export.el`, `scholia-search.el`, `scholia-filter.el`, `scholia-status.el`,
  `scholia-thread.el`: export, search, the dashboard and thread order.
- `scholia-magit.el`, `scholia-ediff.el`, `scholia-timemachine.el`, `scholia-org-remark.el`,
  `scholia-herdr.el`, `scholia-forge.el`: the integrations.

`scholia-herdr.el` and its tests are left out of the package and of CI, because Eask cannot fetch
herdr.el; they run locally where herdr.el is on the load path.

## Credit

[annotate.el](https://github.com/bastibe/annotate.el), by Bastian Bechtold and contributors, is
prior art for scholia. Its source informed edge cases covered by scholia's tests; no annotate.el
code is copied or retained here.

## License

MIT. See [LICENSE](LICENSE).
