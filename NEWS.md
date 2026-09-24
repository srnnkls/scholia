# News

## Unreleased

`scholia-forge` shows a GitHub pull request's comments as threads in a Magit diff of it, coloured by
author, and keeps new comments and replies as drafts in a session of the pull request's own until
they are pushed to GitHub in one batch. A buffer can colour notes and replies by its own
`scholia-render-color-function`. Notes are cera panes beside the annotated line, a reply can answer
a reply, and `scholia-annotation-authors` records and shows who wrote each annotation.

## 0.1.0 — 2026-08-29

Initial release of session-based source annotations, threaded replies, export, search, status,
and optional Org Remark integration. Multiple sessions can be shown together with distinct color
cycles while each buffer retains one write target. Serializable source access supports files, Git
revisions, and arbitrary buffers, with bounded full snapshots and excerpt fallback for recovery.
Herdr integration is source-tree/local-only.

The package entry point now owns `scholia-mode`; loading the definitions leaf has no mode or keymap
side effects. Magit, Ediff, git-timemachine, and Marginalia integrations are inert when required and
are enabled through explicit, reversible setup functions. The dashboard and Org Remark exporter
remain explicit module loads. Successful sends persist their metadata before a pending keyboard
quit propagates.
