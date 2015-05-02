# hnop - A minimal Haskell project structure

This is based on [the minimal Haskell project][semantichnop],
but with values that I think I'll use for most of my Haskell play projects.

If by some off chance anybody other than myself comes across this:
feel free to fork it and customise it to your own liking.
If you think I missed some cool default, please let me know :-)

## Building

Needed only once after cloning the repo:

```bash
$ cabal sandbox init
$ cabal update
```

Compile and install after each code change:

```bash
$ cabal install -j
```

## Running the command-line executable

The binary is placed in this repository checkout under *.cabal-sandbox/bin/hnop*

```
$ .cabal-sandbox/bin/hnop
```

[semantichnop]: http://semantic.org/hnop/
