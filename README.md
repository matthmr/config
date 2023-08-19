# Config files

This is a non-comprehensive repository of my config files.

# Incompatibility

Most likely, these scripts won't work on your machine if you just copy and paste
them. This is by design: most config files can take system specific options (like
full file paths, home directories etc), which most likely need to be changed on
your machine. Whenever you see a variable enclosed with at-signs `@like-this@`,
it means you can change them for your machine. I suggest you run this:

```sh
grep '@.*\?@' <file>
```

before copying a file.

TL;DR: *read the files before using them!*

TODO: In the future I'll write a system that you can run and interactively
change this to suit your machine.
