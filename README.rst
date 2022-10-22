CONFIG FILES
============

This is a non-comprehensive private repository of my config
files. This differs from ``gh://matthmr/dotfiles`` in the sense that
it probably won't be shared in github, and if it is, it will be
privitated out.

Files here are **hard-linked** to my home partition. They *can* have a
a ``gh://matthmr/dotfiles`` mirror and they *can* be backed-up to
``~/Backup``, but the main version-controlled version of these files
stays here.

These files may not have any connection with my own projects. That
includes:

- scripts
- languages
- other C projects

Internal Usage
--------------

This repository may be used to track the version of files that go into
the ``gh://matthmr/dotfiles`` repository and/or get backed-up in
``~/Backup``, instead of the usual way of tracking them down through
my home partition.

Naming convention
-----------------

Any *proper dotfile* is to have their preceeding dot cut off. As for
files under ``XDG`` directories, they don't have any specific naming
convention as long as they're also not *proper* dotfiles.

Linkage
-------

Any file that already has a hard link is not to be linked again. This
goes specially for files that are tracked by ``p://Scripts`` or other
repositories.

NOTE
----

DO **NOT** change any attribute about the files here; it would also
change their atributes for the programs that use them.
