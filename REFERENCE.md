# scholia reference

Every command, key binding, user option, hook and user-facing variable, grouped by module. For the
model behind them and worked examples, read [GUIDE.md](GUIDE.md).

## Contents

- [Modes and keymaps](#modes-and-keymaps)
- [Annotation commands](#annotation-commands)
- [Session commands](#session-commands)
- [Search and export commands](#search-and-export-commands)
- [Dashboard](#dashboard)
- [Org Remark](#org-remark)
- [herdr](#herdr)
- [Integration setup](#integration-setup)
- [Pull requests](#pull-requests)
- [Options](#options)
- [Hooks and variables](#hooks-and-variables)
- [Faces](#faces)
- [Files](#files)

## Modes and keymaps

| Mode | Module | Keymap |
| --- | --- | --- |
| `scholia-mode` | `scholia` | `scholia-mode-map` |
| `scholia-status-mode` | `scholia-status` | `scholia-status-mode-map` |
| `scholia-forge-mode` | `scholia-forge` | `scholia-forge-mode-map` |
| `scholia-forge-review-mode` | `scholia-forge` | `scholia-forge-review-mode-map` |
| `scholia-forge-thread-mode` | `scholia-forge` | inherits `special-mode-map` |

`scholia-mode` is a buffer-local minor mode. Its lighter is `Sch:NAME`, where `NAME` is the
buffer's write target. Turning it on draws every visible session's annotations; turning it off
stores them first when `scholia-autosave` is non-nil. `scholia-annotate` turns it on.

`scholia-mode-map`:

| Key | Command |
| --- | --- |
| `C-c C-a` | `scholia-annotate` |
| `C-c C-c` | `scholia-edit-annotation` |
| `C-c C-d` | `scholia-delete-annotation` |
| `C-c C-r` | `scholia-reply-to` |
| `C-c C-e` | `scholia-export` |
| `C-c C-f` | `scholia-search` |
| `C-c C-n` | `scholia-goto-next-annotation` |
| `C-c C-p` | `scholia-goto-previous-annotation` |

The inline field that `scholia-annotation-editor` `inline` opens is cera's and uses
`cera-mode-map`: `RET` and `C-c C-c` keep the note, `C-g` and `C-c C-k` discard it, and `C-SPC`
completes.

## Annotation commands

Module `scholia-core`, autoloaded from `scholia`.

### scholia-annotate

`C-c C-a`. Annotate the active region, or the symbol at point, in the buffer's write target. With
no region and point in an annotation, edit that annotation instead. With a prefix argument, ask for
another visible session. Reads the note through `scholia-annotation-editor`. Turns `scholia-mode`
on.

### scholia-edit-annotation

`C-c C-c`. Replace the note of the annotation at point, starting from its current text. Keeps the
annotation's id, range, session, colour and replies. Asks which annotation when point is in
several.

### scholia-delete-annotation

`C-c C-d`. Delete the annotation at point and its note. Asks which annotation when point is in
several.

### scholia-reply-to

`C-c C-r`. Read a reply in the minibuffer and store it under the annotation at point, in the
session that owns the annotation. Once the annotation has replies, ask what the reply answers: the
annotation or one of its replies.

### scholia-toggle-annotation-authors

Flip `scholia-annotation-authors` and redraw every annotated buffer.

### scholia-goto-next-annotation, scholia-goto-previous-annotation

`C-c C-n`, `C-c C-p`. Move point to the start of the next or previous annotation. Point stays when
there is none.

### scholia-save-annotations

Store every visible session's annotations for the current buffer. Refused while an inline field is
open.

## Session commands

Module `scholia-session`, autoloaded from `scholia`. Session names cannot be empty, `.`, `..`, or
contain a directory part.

| Command | Does |
| --- | --- |
| `scholia-session-create` | make an empty session and switch to it; refused when the name is taken |
| `scholia-session-switch` | make a session the global write target and draw it alone; with a prefix argument, keep the sessions drawn until now |
| `scholia-session-show` | draw a session beside the target |
| `scholia-session-hide` | stop drawing a session; refused for the target |
| `scholia-session-toggle` | show a hidden session or hide a shown one |
| `scholia-session-rename` | rename a session and every reference to it; refused when the new name is taken |
| `scholia-session-delete` | delete a session; refused while a buffer annotates into it, unless given a prefix argument |
| `scholia-session-export` | write a session to a file as a printed plist; asks before replacing a file |
| `scholia-session-import` | read a file written by `scholia-session-export` into a session, merging by annotation id |
| `scholia-session-assign-project` | tie the current project to a session and store the assignment |
| `scholia-session-load-assignments` | reread `scholia-session-state-file` |
| `scholia-session-restore-visibility` | draw the sessions stored in `scholia-session-state-file`, when `scholia-persist-visibility` is non-nil |

## Search and export commands

| Command | Key | Module | Does |
| --- | --- | --- | --- |
| `scholia-search` | `C-c C-f` | `scholia-search` | choose an annotation from every session and visit it |
| `scholia-search-sends` | | `scholia-search` | choose a recorded send and visit its annotation |
| `scholia-export` | `C-c C-e` | `scholia-export` | render the buffer's annotations from every visible session into `*scholia-export*`; with a prefix argument, one session |
| `scholia-export-session` | | `scholia-export` | render a whole session into `*scholia-export*` |

From Lisp, `(scholia-export TARGET FORMAT SESSION)` and
`(scholia-export-session SESSIONS TARGET FORMAT)` return the rendering. `TARGET` is nil to return it
only, `kill-ring` to copy it, `buffer` to show it, or a file name to write it. `FORMAT` is a key of
`scholia-export-functions`. `SESSIONS` is a name or a list of names.

## Dashboard

Module `scholia-status`, loaded with `(require 'scholia-status)`. Requires `magit-section`.

| Key | Command | Does |
| --- | --- | --- |
| | `scholia-status` | open `*scholia-status*` with every session's annotations |
| `f` | `scholia-status-filter` | show the annotations matching a query |
| `g` | `scholia-status-jump` | visit the annotation at point |
| `d` | `scholia-status-delete` | delete the annotation at point from its session |
| `M` | `scholia-status-mark-hunk` | mark or unmark the annotation at point for sending |
| `s` | `scholia-status-send` | send the marked annotations, or the selected ones, to herdr |
| `h` | `scholia-status-show-send-history` | run `scholia-search-sends` |
| `m` | `scholia-status-mark-session` | mark or unmark the session at point for export |
| `e` | `scholia-status-export-marked-sessions` | export the marked sessions as one document |
| | `scholia-status-switch-session` | switch to the session at point |
| | `scholia-status-rename-session` | rename the session at point |

`scholia-status-mode-map` inherits `magit-section-mode-map`.

Filter queries are whitespace-separated terms, all of which must match:

| Term | Matches |
| --- | --- |
| `REGEXP` | the file, note text, annotated text, session or send |
| `file:REGEXP` | the file |
| `text:REGEXP` | the note text |
| `annotated-text:REGEXP` | the annotated text |
| `session:REGEXP` | the session name |
| `send:REGEXP` | a send's destination or label |
| `!TERM` | what `TERM` does not |

## Org Remark

Module `scholia-org-remark`, loaded with `(require 'scholia-org-remark)`. Requires `org-remark`.

`scholia-org-remark-export`: write a session as an Org Remark notes file. Asks for the session and
the file, and before replacing an existing file. From Lisp, `(scholia-org-remark-export SESSIONS
OUTPUT-FILE)` returns the text and writes it when `OUTPUT-FILE` is non-nil.

## herdr

Module `scholia-herdr`, loaded with `(require 'scholia-herdr)`. Requires
[herdr.el](https://github.com/srnnkls/herdr.el). Not part of the package.

| Command | Sends |
| --- | --- |
| `scholia-herdr-send` | the annotation at point |
| `scholia-herdr-send-region` | the annotations overlapping the region |
| `scholia-herdr-send-file` | every annotation of the current file, placed or not |
| `scholia-herdr-send-session` | every annotation of a session |

## Integration setup

Each setup function can run more than once; its teardown undoes it. None is interactive.

| Module | Setup | Teardown | Effect |
| --- | --- | --- | --- |
| `scholia-magit` | `scholia-magit-setup` | `scholia-magit-teardown` | annotations in Magit diffs record their file and revision |
| `scholia-ediff` | `scholia-ediff-setup` | `scholia-ediff-teardown` | annotations in VC Ediff revision buffers record their file and revision |
| `scholia-timemachine` | `scholia-timemachine-setup` | `scholia-timemachine-teardown` | annotations in git-timemachine record their revision and follow revision changes |
| `scholia-ui` | `scholia-ui-marginalia-setup` | `scholia-ui-marginalia-teardown` | Marginalia annotates note candidates with use count and last file |

Unloading a module with `unload-feature` runs its teardown.

## Pull requests

Module `scholia-forge`. Requires Magit and the GitHub CLI; forge only for
`scholia-forge-diff-pullreq` and `scholia-forge-visit-topic`.

### Opening

| Command | Does |
| --- | --- |
| `scholia-forge-diff-pullreq` | show the forge pull request at point in a Magit diff with its comments; autoloaded |

`(scholia-forge-open HOST OWNER REPO NUMBER RANGE TITLE)` opens the diff of `RANGE` in
`*magit-diff: OWNER/REPO #NUMBER TITLE*`, turns on `scholia-forge-mode` and fetches the comments. It
is autoloaded and not interactive.

`scholia-forge-mode` is the minor mode of that buffer. Its lighter is ` PR…` while fetching, ` PR`
when ready and ` PR!` after a failed fetch. Drafts are stored in the session
`[HOST-]OWNER-REPO-pr-NUMBER`, with the host omitted for `github.com` and characters outside
`A-Za-z0-9._-` replaced by `-`.

### scholia-forge-mode-map

| Key | Command | Does |
| --- | --- | --- |
| `C-c C-a` | `scholia-forge-comment` | draft a comment on the line at point or the region |
| `C-c C-r` | `scholia-forge-reply` | draft a reply to the thread at point |
| `C-c C-c` | `scholia-forge-edit` | change the draft at point |
| `C-c C-d` | `scholia-forge-delete` | delete the draft at point and the drafts answering it, after confirmation |
| `C-c C-g` | `scholia-forge-refetch` | fetch the pull request again and redraw |
| `C-c C-p` | `scholia-forge-push` | open the review buffer |
| `C-c C-o` | `scholia-forge-show-thread` | show the threads behind the note at point in full |
| `C-c C-t` | `scholia-forge-visit-topic` | open forge's buffer for the pull request |

The map also remaps `scholia-annotate`, `scholia-reply-to`, `scholia-edit-annotation` and
`scholia-delete-annotation` to the four draft commands. With Evil, it is an intercept map for normal
state.

### scholia-forge-review-mode-map

The review buffer is `*scholia-forge-review: OWNER/REPO #NUMBER*`, in `scholia-forge-review-mode`,
derived from `text-mode`.

| Key | Command | Does |
| --- | --- | --- |
| `C-c C-c` | `scholia-forge-review-submit` | submit the summary and every draft as `COMMENT`, `APPROVE` or `REQUEST_CHANGES` |
| `C-c C-k` | `scholia-forge-review-cancel` | close the buffer and keep the drafts |

`scholia-forge-show-thread` shows threads in `*scholia-forge: OWNER/REPO #NUMBER*`, in
`scholia-forge-thread-mode`.

## Options

All options belong to the customization group `scholia`.

### Sessions

| Option | Type | Default | Meaning |
| --- | --- | --- | --- |
| `scholia-session-directory` | directory | `scholia/sessions/` in `user-emacs-directory` | where session databases live |
| `scholia-session` | nil or string, buffer-local when set | nil | the write target; nil resolves from the project, then `default` |
| `scholia-visible-sessions` | list of strings | nil | sessions drawn besides the target |
| `scholia-project-sessions` | alist of directory and session name | nil | project roots and the session each annotates into |
| `scholia-project-root-function` | function | `scholia-project-root` | returns the current project root, or nil |
| `scholia-session-state-file` | file | `scholia/project-sessions.eld` in `user-emacs-directory` | where assignments and persisted visibility are stored |
| `scholia-persist-visibility` | boolean | nil | keep the shown sessions and the target across restarts |
| `scholia-autosave` | boolean | t | store annotations when the mode turns off or the buffer is killed |

### Drawing

| Option | Type | Default | Meaning |
| --- | --- | --- | --- |
| `scholia-note-placement` | `beside` or `below` | `beside` | draw notes to the right of the last annotated line, or under it |
| `scholia-annotation-column` | natural number | 85 | column a note starts at when placed `beside` |
| `scholia-note-width` | nil or natural number | 80 | widest a note line may be, indentation included; nil for the window's width |
| `scholia-session-colors` | list of colours | `#F5DD8E` `#8EF5DD` `#DD8EF5` `#CFF58E` `#F5988E` `#8EADF5` | session colours, claimed in order |
| `scholia-dark-theme-shade` | natural number | 12 | percent of lightness session colours lose on a dark theme |
| `scholia-emphasis-shade` | natural number | 30 | percent the annotation under point deepens |
| `scholia-reply-tint-step` | float | 0.35 | share of saturation a reply loses per level |
| `scholia-render-reply-indent` | natural number | 2 | columns a reply is set in per level |
| `scholia-annotation-authors` | boolean | nil | record and draw who wrote each annotation and reply |
| `scholia-author-icon` | nil or string | `"nf-md-at"` | Nerd Font glyph in front of an author |
| `scholia-author-icon-fallback` | nil or string | `"@"` | character in front of an author without the Nerd Font |
| `scholia-show-revision-annotations` | boolean | nil | draw annotations made against revisions in working-tree buffers |

### Input

| Option | Type | Default | Meaning |
| --- | --- | --- | --- |
| `scholia-annotation-editor` | `minibuffer` or `inline` | `minibuffer` | how notes are read for annotating and editing |
| `scholia-annotation-history-limit` | natural number | 200 | earlier notes offered as candidates |
| `scholia-edit-icon` | nil or string | `"nf-md-comment_quote_outline"` | Nerd Font glyph marking the inline field |
| `scholia-edit-icon-fallback` | nil or string | `"❝"` | character marking the field without the Nerd Font |
| `scholia-edit-icon-height` | float | 0.75 | height of the field's glyph relative to the text |

### Sources and relocation

| Option | Type | Default | Meaning |
| --- | --- | --- | --- |
| `scholia-source-snapshot-mode` | `bounded-full` or `excerpts` | `bounded-full` | keep whole sources up to the limit, or only annotated lines |
| `scholia-source-snapshot-limit` | natural number | 262144 | largest source, in bytes, kept whole |
| `scholia-search-region-lines-delta` | natural number | 2 | lines around its old position an annotation's text is looked for after the file changed |

### Export and sending

| Option | Type | Default | Meaning |
| --- | --- | --- | --- |
| `scholia-export-format` | `rustc`, `diff` or `integrate` | `rustc` | format of `scholia-export` and `scholia-export-session` |
| `scholia-export-functions` | alist of symbol and function | `rustc`, `diff`, `integrate` renderers | format symbols and the functions rendering them |
| `scholia-herdr-default-target` | nil or string | nil | herdr agent or pane to send to without asking |
| `scholia-herdr-send-format` | nil or format symbol | nil | format for herdr sends; nil follows `scholia-export-format` |
| `scholia-use-messages` | boolean | t | report actions in the echo area |

A function in `scholia-export-functions` takes the annotations and, optionally, the file or buffer
they belong to, and returns a string.

### Pull requests

| Option | Type | Default | Meaning |
| --- | --- | --- | --- |
| `scholia-forge-gh-program` | string | `"gh"` | GitHub CLI executable |
| `scholia-forge-display-buffer-function` | nil or function | nil | how the diff buffer is displayed; nil uses Magit's |
| `scholia-forge-show-authors` | boolean | t | draw a byline under each comment |
| `scholia-forge-reply-tint-step` | float | 0 | share of saturation a reply loses per level |
| `scholia-forge-author-colors` | alist of login and colour | nil | colours pinned to logins |
| `scholia-forge-author-palette-size` | natural number | 12 | colours shared by the other logins |
| `scholia-forge-author-saturation` | float | 0.4 | share of saturation a palette colour keeps |
| `scholia-forge-note-placement` | `beside` or `below` | `below` | `scholia-note-placement` for pull request diffs |
| `scholia-forge-note-lines` | natural number | 12 | lines of a comment its note shows |
| `scholia-forge-note-replies` | natural number | 4 | latest replies a note shows |
| `scholia-forge-conversation-lines` | natural number | 4 | lines of the description the conversation note shows |
| `scholia-forge-expand-commented-files` | boolean | t | expand file sections that hold comments |

## Hooks and variables

| Name | Kind | Meaning |
| --- | --- | --- |
| `scholia-session-switch-hook` | hook (option) | run after `scholia-session-switch` has redrawn every buffer |
| `scholia-send-functions` | abnormal hook | called with the sent annotations and the send record after a send is stored |
| `scholia-location-functions` | abnormal hook | called with a buffer position; returns a source location plist or nil |
| `scholia-source-functions` | abnormal hook | called with an operation, an access plist and an annotation; handles one source kind |
| `scholia-render-color-function` | buffer-local variable | called with the owner, chain id and reply; returns a note's colour, or nil for the session's |

The source operations are `retrieve`, `open` and `describe`. The built-in access kinds are `file`,
`git` and `buffer`.

## Faces

scholia defines no faces of its own. Underlines and notes are drawn in computed session or author
colours, configured through the options above. The inline field and its bracket use cera's faces
`cera-body`, `cera-border` and `cera-source`. The pull request thread buffer uses Magit's and
`diff-mode`'s faces.

## Files

| Path | Contents |
| --- | --- |
| `scholia-session-directory`/`NAME.eld` | the SQLite database of session `NAME` |
| `NAME.eld-journal`, `NAME.eld-wal`, `NAME.eld-shm` | SQLite journals beside an open database |
| `scholia-session-state-file` | project assignments and, with `scholia-persist-visibility`, the shown sessions and target |

Two Emacs processes writing one session take turns: a writer waits up to five seconds for the
other to finish.

Sessions written by `scholia-session-export` are printed plists tagged `:scholia`. A session file
from an older release, stored as a printed plist, is migrated to SQLite the first time it is opened.
scholia needs an Emacs where `sqlite-available-p` returns non-nil.
