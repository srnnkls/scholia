# Changelog

All notable changes to scholia are recorded here.

## Unreleased

### Added

- `scholia-forge` shows a GitHub pull request's comments as threads in a Magit diff of it. Each
  comment wears its author's colour and byline, and `scholia-forge-author-colors` pins colours to
  logins. Threads whose line the diff no longer has gather in an outdated marker on their file's
  heading, and `scholia-forge-show-thread` shows threads in full under the hunk they were made on.
- New comments and replies on a pull request are drafts, typed in a cera field in a child frame and
  kept in the session `OWNER-REPO-pr-N`. `scholia-forge-push` opens a buffer for the review's
  summary over an overview of the drafts, and submits them as a comment, an approval or a change
  request.
- A diff opened through forge tells forge which pull request it shows; `scholia-forge-visit-topic`
  opens forge's own buffer for it.
- Notes are drawn as cera panes, beside the annotated line from `scholia-annotation-column`, or
  below it with `scholia-note-placement` set to `below`. They wrap at `scholia-note-width`.
- A reply can answer a reply: `scholia-reply-to` asks what it answers once an annotation has
  replies.
- `scholia-annotation-authors` records the git user of new annotations and replies and draws a
  byline under each; `scholia-toggle-annotation-authors` turns it on or off everywhere.
- `scholia-render-color-function` lets a buffer colour notes and replies by its own rule.
- `scholia-annotation-editor` set to `inline` reads notes in a field under the annotated lines,
  with completion of earlier notes and of file names after `@`. `scholia-edit-annotation` changes
  a note in place.
- The annotation under point deepens its colour by `scholia-emphasis-shade`; session colours are
  shaded on dark themes by `scholia-dark-theme-shade`.
- `scholia-persist-visibility` keeps the shown sessions and the target across restarts.

### Changed

- scholia depends on [cera](https://github.com/srnnkls/cera), which draws notes and the input
  field.
- Every session has one colour, claimed from `scholia-session-colors` the first time it is drawn
  and stored in the session, instead of colours cycling per annotation.
- The write target and the drawn sessions are separate. `scholia-session-switch` draws its session
  alone, or beside the others with a prefix argument, and `scholia-session-create` switches to the
  session it made. `scholia-session-show`, `scholia-session-hide` and `scholia-session-toggle`
  replace `scholia-session-activate` and `scholia-session-deactivate`, and
  `scholia-visible-sessions` replaces `scholia-active-sessions`.
- An active region always makes a new annotation, overlapping any already there. Without a region,
  `scholia-annotate` edits the annotation at point, or annotates the symbol at point.
- Annotating turns `scholia-mode` on, so the note is stored when the buffer is killed and follows
  the text as the file is edited.

### Fixed

- Deleting an annotation removes its note too.
- `scholia-session-delete` redraws every buffer that annotated into the deleted session.
- `scholia-session-create` stores the session it leaves before switching.

## 0.1.0 — 2026-08-29

Initial release: annotations kept in named sessions, threaded replies, export, search, a status
dashboard, and optional Org Remark export. Several sessions can be shown together, each in its own
colours, while each buffer writes into one target. Annotations record serializable source access
for files, Git revisions and arbitrary buffers, with bounded full snapshots and an excerpt fallback
for recovery. The herdr integration runs from a source checkout only.

The package entry point owns `scholia-mode`; loading the definitions module has no mode or keymap
side effects. The Magit, Ediff, git-timemachine and Marginalia integrations do nothing until their
setup functions run, and each setup has a teardown. The dashboard and the Org Remark exporter are
loaded explicitly. A send's metadata is stored before a pending keyboard quit takes effect.
