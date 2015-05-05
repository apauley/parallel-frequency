# Count the frequency in which elements in a list occurs

An excuse to learn parallel Haskell.

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

The binary is placed in this repository checkout under *.cabal-sandbox/bin/frequency*

```
$ ./run.sh /path/to/file.txt
```
