# The scholia guide

This guide explains how scholia works and walks through each task in the order you meet it. Every
command, key and option is listed in [REFERENCE.md](REFERENCE.md).

## Contents

- [How scholia works](#how-scholia-works)
- [Annotating](#annotating)
- [How notes are drawn](#how-notes-are-drawn)
- [Writing a note](#writing-a-note)
- [Editing and deleting](#editing-and-deleting)
- [Replies and threads](#replies-and-threads)
- [Authors](#authors)
- [Sessions](#sessions)
- [Saving and changed files](#saving-and-changed-files)
- [Sources and revisions](#sources-and-revisions)
- [Finding annotations](#finding-annotations)
- [The dashboard](#the-dashboard)
- [Exporting](#exporting)
- [Sending to herdr](#sending-to-herdr)
- [Integrations](#integrations)
- [GitHub pull requests](#github-pull-requests)
- [When something looks wrong](#when-something-looks-wrong)
- [Where to look next](#where-to-look-next)

## How scholia works

scholia keeps notes about text outside the text. A handful of terms carry the model:

- An *annotation* is a note attached to a range of text. It has an id, the text it annotates, the
  session that owns it, and a description of its source.
- A *chain* is the set of overlays marking one annotation's range, one overlay per line the range
  spans. The range is underlined in the owner session's colour.
- A *note* is the annotation's text as scholia draws it: a read-only pane beside the chain's last
  line, or below it. Notes are display only; nothing is inserted into the buffer.
- A *reply* is an annotation that answers another annotation or another reply. An annotation and
  its replies form a *thread*.
- A *session* is a named collection of annotations, stored as one SQLite database in
  `scholia-session-directory`. A session called `review` lives in `review.eld`.
- The *write target* is the session a new annotation in the current buffer goes to. The *visible
  sessions* are the ones drawn besides it. Target and visibility are set separately.
- The *source* of an annotation says how to reach its text again: a file, a file at a Git revision,
  or a live buffer. A *snapshot* is source text a session keeps, so an export can still show the
  annotated lines after the source changed or went away.

`scholia-mode` is the minor mode of an annotated buffer. Turning it on reads every visible
session's annotations for the buffer's source and draws them; turning it off, or killing the
buffer, stores them first. `scholia-annotate` turns the mode on for you.

## Annotating

`scholia-annotate` (`C-c C-a`) annotates the active region. Without a region it annotates the
symbol at point, and if point is already in an annotation it edits that annotation instead. A
region may span several lines and may overlap other annotations; scholia draws each one.

The new annotation goes to the buffer's write target. With a prefix argument, `C-u C-c C-a` asks
for another visible session to put it in.

When point enters an annotation, its underline and note deepen by `scholia-emphasis-shade` percent,
so you can see which note belongs to the text under point. `scholia-goto-next-annotation`
(`C-c C-n`) and `scholia-goto-previous-annotation` (`C-c C-p`) move point to the start of the
next or previous annotation.

`scholia-mode-map` holds these keys, so they work once the mode is on. To annotate a buffer for the
first time from a key, bind `scholia-annotate` globally, or use a leader layout such as the
[Doom example](#doom-leader-keys).

## How notes are drawn

A note is a read-only [cera](https://github.com/srnnkls/cera) pane. Where it goes depends on
`scholia-note-placement`:

- `beside`, the default, draws the note to the right of the chain's last line, starting at
  `scholia-annotation-column` (85).
- `below` draws it on lines of its own under the chain's last line, from the left edge.

A note wraps at the window's edge or at `scholia-note-width` columns (80), whichever comes first. Set
`scholia-note-width` to nil to wrap at the window's edge only. The buffer text, the modified flag
and the undo history never see a note.

Every session has a colour of its own. The first time a session is drawn it claims the first colour
of `scholia-session-colors` that no other session holds, and stores it in the session's database.
It keeps that colour across restarts, whatever other sessions are created, renamed or deleted. Two
sessions are never drawn in one colour: once the listed colours run out, scholia picks further hues
around the colour wheel. On a dark theme the colours lose `scholia-dark-theme-shade` percent of
their lightness.

A buffer can colour its notes by something other than their session. `scholia-render-color-function`
is a buffer-local function called with the owner session, the chain id and, for a reply, the reply
itself; it returns a colour, or nil for the session's own. `scholia-forge` uses it to colour pull
request comments by author.

## Writing a note

`scholia-annotation-editor` chooses how a note is read, for both annotating and editing:

- `minibuffer`, the default, reads the note with `completing-read`. Earlier notes from every session
  are offered as candidates, up to `scholia-annotation-history-limit`, and any new text is accepted.
- `inline` opens a cera field under the annotated lines. The annotated text is underlined in the
  session's colour, and a bracket connects its first line to the field.

```emacs-lisp
(setq scholia-annotation-editor 'inline)
```

In the inline field:

- `RET` or `C-c C-c` keeps the note. While a completion popup is open, `RET` belongs to the popup.
- `C-g` or `C-c C-k` discards it and leaves an existing annotation as it was.
- `C-SPC` completes from earlier notes, as the minibuffer does.
- `@` followed by a name completes a file name, for example `See @src/parser.el`. In a project the
  candidates are the project's files, relative to its root; elsewhere they are files relative to
  the current directory. The `@` stays in the note.

The field is cera's, so its faces and keys are cera's too; see cera's documentation to change them.
When Corfu is installed, completion in the field uses it. When Evil is installed, the field starts
in insert state. The field is marked with the glyph `scholia-edit-icon`, drawn in the session's
colour, or `scholia-edit-icon-fallback` where the Nerd Font is missing.

The inline editor stores the session as soon as the note is accepted. The minibuffer editor leaves
storing to `scholia-mode`, which saves when the buffer is killed or Emacs exits;
`scholia-save-annotations` stores the buffer's annotations at any time.

## Editing and deleting

`scholia-edit-annotation` (`C-c C-c`) reads a new text for the annotation at point, starting from the
old one. The annotation keeps its id, range, session, colour and replies. With the inline editor,
the note steps aside while the field is open and returns however the field closes.

`scholia-delete-annotation` (`C-c C-d`) deletes the annotation at point and its note.

When point is in several annotations, both commands ask which one you mean, showing each
candidate's session, text and id. Once the annotation has replies, they ask which part of the
thread they act on, the way `scholia-reply-to` asks what a reply answers: the annotation itself,
offered first, or any reply, indented by its depth. Editing a reply opens it with its own text and
keeps its place in the thread; deleting one takes the replies under it with it.

## Replies and threads

`scholia-reply-to` (`C-c C-r`) reads a reply in the minibuffer and stores it under the annotation at
point. Once the annotation has replies, it asks what the new reply answers: the annotation itself,
offered first, or any reply, indented by its depth. The reply lands after the last answer to its
parent, so the thread reads in order.

Replies are drawn under the note, each set in by `scholia-render-reply-indent` columns (2) per level
of depth. A reply loses `scholia-reply-tint-step` (0.35) of its colour's saturation per level, so a
thread reads as one hue fading inwards.

A reply always goes to the session that owns the annotation, whatever the write target is. Exports,
the dashboard and search show threads with the same nesting.

## Authors

With `scholia-annotation-authors` on, every new annotation and reply records who wrote it: the
`user.name` and `user.email` from `git config` in the buffer's repository, falling back to
`user-full-name` and `user-mail-address`. Each stored author is drawn as a byline under what they
wrote, behind the glyph `scholia-author-icon` (or `scholia-author-icon-fallback`).

`scholia-toggle-annotation-authors` turns recording and drawing on or off and redraws every
annotated buffer. It is off by default.

## Sessions

A buffer's write target is resolved in this order:

1. a buffer-local value of `scholia-session`;
2. the session assigned to the buffer's project, first in `scholia-project-sessions`, then among
   the assignments made with `scholia-session-assign-project`;
3. the global value of `scholia-session`, or `default` when that is nil.

The target is always drawn. `scholia-visible-sessions` lists the sessions drawn besides it.

### Creating and switching

`scholia-session-create` makes an empty session and switches to it. `scholia-session-switch`
makes another session the global target and draws it alone; with a prefix argument, the sessions
drawn until then stay drawn beside it, including the one you switched away from. Both store every
annotated buffer before they redraw. `scholia-session-switch-hook` runs after a switch.

### Showing and hiding

`scholia-session-show`, `scholia-session-hide` and `scholia-session-toggle` change which sessions are
drawn without touching the target. Hiding the target is refused; switch away from it first.

By default, visibility lasts for the Emacs session: after a restart only the target is drawn. With
`scholia-persist-visibility` set, the shown sessions and the global target are written to
`scholia-session-state-file` and read back the first time an annotated buffer is drawn:

```emacs-lisp
(setq scholia-persist-visibility t)
```

### Sessions per project

To annotate a project into its own session, either configure it:

```emacs-lisp
(setq scholia-project-sessions '(("~/src/parser/" . "parser-review")))
```

or run `scholia-session-assign-project` in a buffer of the project, which stores the assignment
in `scholia-session-state-file`. Configured entries win over stored ones. The project root comes
from `scholia-project-root-function`, which asks projectile when it is loaded and `project.el`
otherwise. `scholia-session-load-assignments` rereads the state file after you edit it outside
Emacs.

If an assignment names a session that does not exist, the buffer falls back to the default session
and says so.

### Renaming, deleting, moving

`scholia-session-rename` renames a session and points the global target, buffer-local bindings,
visible sessions and project assignments at the new name. `scholia-session-delete` deletes a
session's database; it refuses while a buffer annotates into the session, unless you give it a
prefix argument.

`scholia-session-export` writes a session to a file you name, as a printed Lisp plist you can diff,
commit or hand to someone. `scholia-session-import` reads such a file into a session: into a new
one, or merged into an existing one, where annotations with the same id count once.

## Saving and changed files

`scholia-mode` stores a buffer's annotations when the mode is turned off, when the buffer is killed
and when Emacs exits. Set `scholia-autosave` to nil to leave storing to you, through
`scholia-save-annotations`.

While the mode is on, annotations move with the text as you edit. When a file changed while its
annotations were not shown, scholia looks for each annotation's text within
`scholia-search-region-lines-delta` lines (2) of where it was. An annotation whose text is gone is
kept in the session but not drawn, and scholia reports it by text. Session exports render it from
the session's snapshot and mark it `(stale)`.

## Sources and revisions

Every annotation records a serializable description of its source:

- a file, reopened by its path;
- a file at a Git revision, reopened from Git;
- a live buffer that visits no file, found again by its identity while it lives.

Files and plain buffers need no setup. Revision sources come from the Magit, Ediff and
git-timemachine integrations described under [Integrations](#integrations).

`scholia-source-snapshot-mode` controls how much source text a session keeps. The default,
`bounded-full`, keeps the whole source when it fits in `scholia-source-snapshot-limit` bytes
(256 KiB); a larger source keeps only the lines around its annotations and is marked truncated.
`excerpts` always keeps only those lines. Session exports label every source with its access
description and with how its text was obtained: `live`, `full` or `excerpt`.

### Revisions

An annotation made in a revision buffer carries that revision, shown as `[REVISION]` after its
note and in exports. A revision buffer draws only the annotations of its own revision. A buffer
visiting the working tree hides annotations made against revisions and reports how many it hid;
set `scholia-show-revision-annotations` to draw them there too.

## Finding annotations

`scholia-search` (`C-c C-f`) offers every annotation in every session, showing its note, the
annotated text, the file, the session, its revision and its id. Choosing one opens its source, shows
its session, and puts point on it. Choosing a reply takes you to the annotation it answers.

`scholia-search-sends` offers every recorded send, grouped by the destination's label, and visits
the annotation that was sent.

With `scholia-ui-marginalia-setup`, Marginalia annotates the candidates of the note prompt with how
often each note was used and the file it was last used in.

## The dashboard

`scholia-status` opens `*scholia-status*`, a Magit-section buffer listing every session, the files
it annotates, and each thread under its file. It needs `magit-section`. Load it with
`(require 'scholia-status)`.

| Key | Command | Does |
| --- | --- | --- |
| `g` | `scholia-status-jump` | visit the annotation at point |
| `d` | `scholia-status-delete` | delete the annotation at point from its session |
| `f` | `scholia-status-filter` | show only the annotations matching a query |
| `M` | `scholia-status-mark-hunk` | mark or unmark the annotation at point for sending |
| `s` | `scholia-status-send` | send the marked annotations, or those at point, to herdr |
| `h` | `scholia-status-show-send-history` | browse recorded sends |
| `m` | `scholia-status-mark-session` | mark or unmark the session at point for export |
| `e` | `scholia-status-export-marked-sessions` | export the marked sessions as one document |

`g` here visits an annotation; it does not refresh the buffer. `scholia-status-switch-session` and
`scholia-status-rename-session` act on the session at point and have no key.

A filter query is a list of terms separated by whitespace, and an annotation must match every term:

- `REGEXP` matches the file, note, annotated text, session or send destination;
- `FIELD:REGEXP` matches one field: `file`, `text`, `annotated-text`, `session` or `send`;
- `!TERM` matches what `TERM` does not.

For example, `session:review !text:done file:\.el$` shows the annotations of sessions matching
`review` in Emacs Lisp files whose note does not mention `done`.

## Exporting

`scholia-export` (`C-c C-e`) renders the current buffer's annotations, from every visible session,
into `*scholia-export*`. With a prefix argument it asks for one session. Each session starts with a
`Session:` line and an `Access:` line describing the source.

`scholia-export-format` chooses the format:

- `rustc`, the default, renders one diagnostic per thread, with carets under the annotated text and
  the replies nested under the note.
- `diff` renders a unified diff that adds each note as a comment under its line.
- `integrate` renders the source with each note inserted as a comment under its line.

Every rendering carries each annotation's id in brackets, so a reader can refer to it. Replies whose
annotation is not in the export are listed at the end. This is the `integrate` format for the
example in the README:

```text
(defun greet (name)
  (message "Hello, %s" name))
  ;~~~~~~~
  ;[c89c2e42-b8a6-11f1-b5a7-01a6851d9858]
  ;Use format-message here?
  ;  [c89c2e6a-b8a6-11f1-9e4c-010cee6f04ed] Agreed, it quotes properly.
```

`scholia-export-session` renders every file of a session, from its snapshots when the source is not
at hand. From Lisp it takes a list of sessions and a target: nil returns the string, `kill-ring`
copies it, `buffer` shows it, and a file name writes it. `scholia-export-functions` maps each format
symbol to its renderer, so you can add formats.

### Org Remark

`scholia-org-remark-export` writes a session as an Org file that
[Org Remark](https://github.com/nobiot/org-remark) reads. Load it with
`(require 'scholia-org-remark)`; it needs `org-remark`. It asks for the notes file and asks before
replacing an existing one.

## Sending to herdr

`scholia-herdr` sends rendered annotations to an agent or pane managed through
[herdr.el](https://github.com/srnnkls/herdr.el), which controls persistent herdr terminal
workspaces. It is not part of the package; with herdr.el on the load path, load it from a checkout
with `(require 'scholia-herdr)`.

| Command | Sends |
| --- | --- |
| `scholia-herdr-send` | the annotation at point |
| `scholia-herdr-send-region` | the annotations overlapping the region |
| `scholia-herdr-send-file` | every annotation of the current file |
| `scholia-herdr-send-session` | every annotation of a session |

Each command asks for a destination unless `scholia-herdr-default-target` names one, and renders in
`scholia-herdr-send-format`, or in `scholia-export-format` when that is nil. After a successful
send, every annotation sent records where it went and when; `scholia-search-sends` and the
dashboard read those records. `scholia-send-functions` runs after each recorded send.

## Integrations

Each integration is a separate module, inert until its setup function runs. Setup functions can
run more than once, and each has a teardown that removes its hooks and advice.

```emacs-lisp
(with-eval-after-load 'magit
  (require 'scholia-magit)
  (scholia-magit-setup))

(with-eval-after-load 'ediff
  (require 'scholia-ediff)
  (scholia-ediff-setup))

(with-eval-after-load 'git-timemachine
  (require 'scholia-timemachine)
  (scholia-timemachine-setup))

(with-eval-after-load 'marginalia
  (require 'scholia-ui)
  (scholia-ui-marginalia-setup))
```

- Magit: annotating a line of a diff records the file and revision that line comes from, the
  old side for a removed line and the new side otherwise. Lines of combined diffs record no
  revision.
- Ediff: annotating either side of a two-revision comparison started through VC records that
  side's file and revision.
- git-timemachine: annotations record the revision shown, and moving to another revision stores
  the annotations and draws the new revision's.
- Marginalia: see [Finding annotations](#finding-annotations).

The teardowns are `scholia-magit-teardown`, `scholia-ediff-teardown`,
`scholia-timemachine-teardown` and `scholia-ui-marginalia-teardown`.

`scholia-location-functions` and `scholia-source-functions` are the hooks these modules use, and
the place to add another source kind.

### Doom leader keys

scholia ships no Doom bindings. This block puts the commands under `SPC a`, which Doom leaves free:

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

## GitHub pull requests

`scholia-forge` shows a GitHub pull request's comments as threads in a Magit diff of the pull
request, and keeps your new comments as drafts until you submit them as one review. It needs Magit
and the GitHub CLI `gh`, authenticated for the pull request's host. Forge is needed only to open the
pull request at point.

### Opening a pull request

In a forge buffer, with point on a pull request, run `scholia-forge-diff-pullreq`. It opens the diff
of the pull request's fetched head in a buffer named `*magit-diff: OWNER/REPO #N TITLE*` and fetches
the comments from GitHub; the mode line shows ` PR…` while they load, ` PR` when they are drawn, and
` PR!` when the fetch failed. The pull request's head must have been fetched with `forge-pull`. If
GitHub's head is newer than the local ref, scholia says so, because comments may then sit on the
wrong lines.

Without forge, call `scholia-forge-open` from Lisp with the host, owner, repository, number, revision
range and title. `scholia-forge-display-buffer-function` chooses how the diff buffer is displayed.

### Reading the threads

- A review comment is drawn on the lines it was made on, with its replies under it.
- A comment on a whole file is drawn on the file's heading.
- The description, conversation comments and review summaries share one note on the first line. It
  shows the first `scholia-forge-conversation-lines` lines of the description and counts the
  comments.
- Threads whose line the diff no longer has are gathered in one outdated marker, `⟲`, on their
  file's heading.

Notes are drawn below their lines (`scholia-forge-note-placement`). A comment shows at most
`scholia-forge-note-lines` lines, and a thread shows its latest `scholia-forge-note-replies` replies
and counts the earlier ones. `scholia-forge-show-thread` (`C-c C-o`) opens every thread behind the
note at point in full, each under the diff hunk it was made on. Files holding comments are expanded
while `scholia-forge-expand-commented-files` is on.

Every comment is drawn in its author's colour, with a byline naming the author while
`scholia-forge-show-authors` is on. `scholia-forge-author-colors` pins colours to logins; any other
login gets one of `scholia-forge-author-palette-size` colours, chosen from its name so it is the same
everywhere, and muted to `scholia-forge-author-saturation` of its saturation. Replies keep their own
author's colour and, with `scholia-forge-reply-tint-step` at 0, are not faded.

### Writing drafts

In the diff buffer, the annotation keys write drafts instead of annotations:

| Key | Command | Does |
| --- | --- | --- |
| `C-c C-a` | `scholia-forge-comment` | draft a comment on the line at point or the region |
| `C-c C-r` | `scholia-forge-reply` | draft a reply to the thread at point |
| `C-c C-c` | `scholia-forge-edit` | change the draft at point |
| `C-c C-d` | `scholia-forge-delete` | delete the draft at point and the drafts answering it |

`scholia-annotate`, `scholia-reply-to`, `scholia-edit-annotation` and `scholia-delete-annotation` are
remapped to these commands, so bindings you made for them work here too. With Evil, the keys take
precedence over Evil's normal-state keys.

On a graphic display a draft is written in a cera field in a child frame over the lines it is about,
which leaves the diff's text alone. Elsewhere it is read in the minibuffer. Replying to the
conversation note asks which comment to answer and quotes it. Replying to an outdated marker asks
which thread to answer. A draft cannot be replied to; edit it instead. Comments already on GitHub
cannot be edited or deleted here.

Drafts are kept in a session of the pull request's own, named `OWNER-REPO-pr-N`, prefixed by the
host when it is not `github.com`. They survive closing the buffer and restarting Emacs.

### Submitting

`scholia-forge-push` (`C-c C-p`) opens a review buffer. Write the review's summary at the top, above
an overview of the drafts; the summary may stay empty.

| Key | Command | Does |
| --- | --- | --- |
| `C-c C-c` | `scholia-forge-review-submit` | submit the review with every draft |
| `C-c C-k` | `scholia-forge-review-cancel` | close the buffer and keep the drafts |

Submitting asks for the review's kind, `COMMENT` by default, `APPROVE` or `REQUEST_CHANGES`. The
comments on lines go to GitHub as one review with the summary, then the replies and conversation
comments follow one by one. Each draft is dropped once GitHub accepted it, so after a failure the
rest are still there to submit. The pull request is then fetched again.

### Other keys

| Key | Command | Does |
| --- | --- | --- |
| `C-c C-g` | `scholia-forge-refetch` | fetch the comments again and redraw them |
| `C-c C-o` | `scholia-forge-show-thread` | show the threads behind the note at point in full |
| `C-c C-t` | `scholia-forge-visit-topic` | open forge's own buffer for the pull request |

A diff opened through forge tells forge which pull request it shows, so forge commands that act on
the current topic work in it.

## When something looks wrong

- A note does not appear after reopening a file. Check the mode line for `Sch:NAME`: the buffer may
  resolve to another session than the one you annotated in. `scholia-session-show` draws the other
  session beside it.
- scholia reports that a file changed on disk and an annotation is kept but not shown. Its text is
  no longer within `scholia-search-region-lines-delta` lines of where it was. The annotation is
  still in the session; `scholia-search` and the dashboard find it, and session exports mark it
  `(stale)`.
- scholia reports revision annotations hidden. They were made against a Git revision; open that
  revision, or set `scholia-show-revision-annotations`.
- A session that should come back after a restart does not. Set `scholia-persist-visibility`, and
  check that `scholia-session-state-file` is not in a directory your setup wipes.
- `scholia-save-annotations` says to finish the annotation first. An inline field is still open;
  keep or discard the note, then save.
- `scholia-forge` says `gh` is not authenticated. Run `gh auth login --hostname HOST`.
- `scholia-forge-diff-pullreq` says the head ref is not fetched. Run `forge-pull`.
- Loading scholia fails with a message about `sqlite-available-p`. Your Emacs was built without
  SQLite; scholia needs it.

## Where to look next

- [REFERENCE.md](REFERENCE.md) lists every command, key binding, option, hook and variable.
- [CHANGELOG.md](CHANGELOG.md) records what changed in each release.
- [cera](https://github.com/srnnkls/cera) documents the input field and the panes notes are drawn
  in.
- The ERT suites under [test/](test/) exercise every module; `eask run script test` runs them.
