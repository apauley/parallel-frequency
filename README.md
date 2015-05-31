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
  Total   time   26.605s  ( 27.561s elapsed)

f2-seq-datamap.sh: Sequential. Frequency implementation based on Map.fromListWith
               2 MB total memory in use (0 MB lost due to fragmentation)
  Total   time    2.085s  (  2.134s elapsed)

f3-par-divide-conquer.sh: Parallel. Split list in half recursively, with each half being run in parallel.
             645 MB total memory in use (0 MB lost due to fragmentation)
  Parallel GC work balance: 0.45% (serial 0%, perfect 100%)
  TASKS: 6 (1 bound, 5 peak workers (5 total), using -N2)
  SPARKS: 4 (2 converted, 0 overflowed, 0 dud, 0 GC'd, 2 fizzled)
  Total   time    5.236s  (  3.381s elapsed)

f4-seq-chunked.sh: Sequential. Split file into chunks so we can parallelise the chunks later.
f4-seq-chunked.sh: Speedup mostly due to using Data.ByteString and Data.Text for chunking instead of lists.
              78 MB total memory in use (0 MB lost due to fragmentation)
  Total   time    1.954s  (  2.015s elapsed)

f5-par-parmap.sh: Parallel. Use parMap which creates a spark for each list element.
              69 MB total memory in use (0 MB lost due to fragmentation)
  Parallel GC work balance: 83.76% (serial 0%, perfect 100%)
  TASKS: 8 (1 bound, 7 peak workers (7 total), using -N3)
  SPARKS: 3 (2 converted, 0 overflowed, 0 dud, 0 GC'd, 1 fizzled)
  Total   time    2.246s  (  0.836s elapsed)

f6-par-using-strategy.sh: Parallel. Use the parList strategy instead of a custom parMap.
              93 MB total memory in use (0 MB lost due to fragmentation)
  Parallel GC work balance: 69.10% (serial 0%, perfect 100%)
  TASKS: 10 (1 bound, 9 peak workers (9 total), using -N4)
  SPARKS: 10 (8 converted, 0 overflowed, 0 dud, 1 GC'd, 1 fizzled)
  Total   time    2.617s  (  0.791s elapsed)
```

Full output when running just one program:
```
$ ./fastest-par.sh artamene.txt
fastest-par.sh: Parallel. Use the parList strategy instead of a custom parMap.
T0: start calculating frequency of elements in a list.
After freq return. Time since start of program: 0.02s

Top 10 characters in artamene.txt:
"de":	87178
"que":	67057
"et":	47254
":":	44569
"la":	42343
"\224":	34063
"ne":	32839
"vous":	29862
"le":	29532
"je":	29121

After character frequency print. Time since start of program: 0.76s
   1,403,308,664 bytes allocated in the heap
     353,968,280 bytes copied during GC
      41,435,408 bytes maximum residency (6 sample(s))
       3,401,824 bytes maximum slop
              91 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       740 colls,   740 par    1.603s   0.222s     0.0003s    0.0018s
  Gen  1         6 colls,     5 par    0.060s   0.022s     0.0036s    0.0068s

  Parallel GC work balance: 74.99% (serial 0%, perfect 100%)

  TASKS: 10 (1 bound, 9 peak workers (9 total), using -N4)

  SPARKS: 10 (8 converted, 0 overflowed, 0 dud, 1 GC'd, 1 fizzled)

  INIT    time    0.001s  (  0.010s elapsed)
  MUT     time    0.848s  (  0.521s elapsed)
  GC      time    1.663s  (  0.244s elapsed)
  EXIT    time    0.000s  (  0.005s elapsed)
  Total   time    2.513s  (  0.779s elapsed)

  Alloc rate    1,653,963,528 bytes per MUT second

  Productivity  33.8% of total user, 109.1% of total elapsed

gc_alloc_block_sync: 46099
whitehole_spin: 0
gen[0].sync: 766
gen[1].sync: 2249
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
