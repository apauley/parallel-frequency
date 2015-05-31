# Counts the number of times each element occurs in a list

This project implements the same frequency counter a number of times,
trying to systematically increase the parallelism of the program with each implementation.

It tries to implement parallel techniques from chapter 2 and 3 in
[Simon Marlow's book](http://chimera.labs.oreilly.com/books/1230000000929/index.html).

The frequency counter accepts a text file as input from the command-line, and then prints the frequency
that each character occurs.

[Claude Heiland-Allen](http://mathr.co.uk/blog/) was kind enough to implement this problem,
along with detailed explanations of how he achieved better performance.

Thanks Claude!

I have been improving a lot of my code based on his implementation:
http://code.mathr.co.uk/word-histogram

## Sample Text Files

[Flatland](https://en.wikipedia.org/wiki/Flatland) by Edwin A. Abbott is a small text and included in this repository.

I mostly ran my programs against [the longest book](https://en.wikipedia.org/wiki/List_of_longest_novels) I could find:
A French novel called [Artamène](https://en.wikipedia.org/wiki/Artam%C3%A8ne).

You can download it with the following command:
```
curl http://www.artamene.org/documents/cyrus[1-10].txt | iconv --from-code=ISO-8859-1 --to-code=UTF8 > artamene.txt
```

## Benchmarks

The following benchmarks have been run on a 2012 Macbook Pro with 8 cores.

```
$ ./summary.sh artamene.txt
f1-seq-datalist.sh: Sequential. Frequency implementation based on list grouping/sorting.
            1192 MB total memory in use (0 MB lost due to fragmentation)
  Total   time   26.600s  ( 27.285s elapsed)

f2-seq-datamap.sh: Sequential. Frequency implementation based on Map.fromListWith
               2 MB total memory in use (0 MB lost due to fragmentation)
  Total   time    2.056s  (  2.096s elapsed)

f3-par-divide-conquer.sh: Parallel. Split list in half recursively, with each half being run in parallel.
             645 MB total memory in use (0 MB lost due to fragmentation)
  Parallel GC work balance: 0.90% (serial 0%, perfect 100%)
  TASKS: 6 (1 bound, 5 peak workers (5 total), using -N2)
  SPARKS: 4 (2 converted, 0 overflowed, 0 dud, 0 GC'd, 2 fizzled)
  Total   time    5.194s  (  3.350s elapsed)

f4-seq-chunked.sh: Sequential. Split file into chunks so we can parallelise the chunks later.
f4-seq-chunked.sh: Speedup mostly due to using Data.ByteString and Data.Text for chunking instead of lists.
              19 MB total memory in use (0 MB lost due to fragmentation)
  Total   time    2.645s  (  2.713s elapsed)

f5-par-parmap.sh: Parallel. Use parMap which creates a spark for each list element.
              26 MB total memory in use (0 MB lost due to fragmentation)
  Parallel GC work balance: 32.12% (serial 0%, perfect 100%)
  TASKS: 10 (1 bound, 9 peak workers (9 total), using -N4)
  SPARKS: 5 (4 converted, 0 overflowed, 0 dud, 0 GC'd, 1 fizzled)
  Total   time    3.518s  (  0.977s elapsed)

f6-par-using-strategy.sh: Parallel. Use the parList strategy instead of a custom parMap.
              25 MB total memory in use (0 MB lost due to fragmentation)
  Parallel GC work balance: 70.26% (serial 0%, perfect 100%)
  TASKS: 10 (1 bound, 9 peak workers (9 total), using -N4)
  SPARKS: 10 (8 converted, 0 overflowed, 0 dud, 1 GC'd, 1 fizzled)
  Total   time    3.294s  (  0.905s elapsed)
```

Full output when running just one program:
```
$ ./fastest-par.sh artamene.txt
fastest-par.sh: Parallel. Use the parList strategy instead of a custom parMap.
T0: start calculating frequency of elements in a list.
After freq return. Time since start of program: 0.01s

Top 10 characters in artamene.txt:
" ":	1988692
"e":	1446254
"s":	731644
"i":	623145
"u":	603616
"t":	599321
"a":	584293
"n":	568182
"r":	559543
"o":	533967

After character frequency print. Time since start of program: 0.88s
   6,553,746,816 bytes allocated in the heap
      86,856,816 bytes copied during GC
      11,764,368 bytes maximum residency (2 sample(s))
         194,000 bytes maximum slop
              22 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      3627 colls,  3627 par    2.133s   0.084s     0.0000s    0.0018s
  Gen  1         2 colls,     1 par    0.001s   0.001s     0.0005s    0.0008s

  Parallel GC work balance: 85.37% (serial 0%, perfect 100%)

  TASKS: 10 (1 bound, 9 peak workers (9 total), using -N4)

  SPARKS: 10 (8 converted, 0 overflowed, 0 dud, 1 GC'd, 1 fizzled)

  INIT    time    0.001s  (  0.001s elapsed)
  MUT     time    1.101s  (  0.791s elapsed)
  GC      time    2.134s  (  0.085s elapsed)
  EXIT    time    0.000s  (  0.001s elapsed)
  Total   time    3.238s  (  0.878s elapsed)

  Alloc rate    5,951,415,914 bytes per MUT second

  Productivity  34.1% of total user, 125.6% of total elapsed

gc_alloc_block_sync: 22890
whitehole_spin: 0
gen[0].sync: 7
gen[1].sync: 8
```

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

## Threadscope

All of the executables are compiled with *-eventlog* and run using *+RTS -ls*, and should therefore
produce eventlog output files that can be inspected using threadscope.
