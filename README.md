# scholia

Annotate a file without changing it, group the annotations into named sessions, and send
them where the work happens.

Greek σχόλια — the annotations scholars wrote in the margins of manuscripts.

## Status

Nothing is implemented yet. The repository holds package metadata and this README.

## Credit

scholia is a hard fork of [annotate.el](https://github.com/bastibe/annotate.el) by
**Bastian Bechtold**, maintained since 2015 with **cage** as co-maintainer and with
contributions from Naoya Yamashita, Universitá degli Studi di Palermo, and others.

The fork is not a criticism of annotate.el — it exists because the things scholia wants to
add (a session-scoped database, a different serialization format, a replacement for the
summary window) all change things annotate.el is right to keep stable for its own users.
Taking those changes upstream would have meant asking a mature package to become a
different one.

What scholia keeps is the part that took years to get right: the overlay and chain engine,
the re-search that relocates an annotation when a file changed while the mode was off,
indirect-buffer handling, and the newline accounting at the end of a buffer. Every one of
those encodes a bug someone already found and fixed. Rewriting them from scratch would have
bought nothing except the chance to rediscover them.

If you want annotations in Emacs today, use annotate.el. It is stable, it is maintained,
and it does what it says.

## License

MIT, inherited from annotate.el. See [LICENSE](LICENSE) — the original copyright notice is
kept alongside scholia's.
