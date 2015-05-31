# Counts the number of times each element occurs in a list

This project implements the same frequency counter a number of times,
trying to systematically increase the parallelism of the program with each implementation.

It tries to implement parallel techniques from chapter 2 and 3 in
[Simon Marlow's book](http://chimera.labs.oreilly.com/books/1230000000929/index.html).

The frequency counter accepts a text file as input from the command-line, and then prints the frequency
that each character occurs.

## Sample Text Files

[Flatland](https://en.wikipedia.org/wiki/Flatland) by Edwin A. Abbott is a small text and included in this repository.

I mostly ran my programs against [the longest book](https://en.wikipedia.org/wiki/List_of_longest_novels) I could find:
A French novel called [Artamène](https://en.wikipedia.org/wiki/Artam%C3%A8ne).

You can download it with the following command:
```
curl http://www.artamene.org/documents/cyrus[1-10].txt | iconv --from-code=ISO-8859-1 --to-code=UTF8 > artamene.txt
```

## Results so far

I've managed to get more than twice the speed in my fastest parallel implementation.
But looking at the threadscope images, it does seem that there are still significant portions that
does not run in parallel.

[Claude Heiland-Allen](http://mathr.co.uk/blog/) was kind enough to implement this problem,
along with detailed explanations of how he achieved better performance.

Thanks Claude!

I have been improving some of my code based on his:
http://code.mathr.co.uk/word-histogram

## Running the command-line executables

You can run all of these with summarised output by using the following script:
```
$ ./summary.sh flatland.txt
1-seq-datalist.sh
  Total   time    0.534s  (  0.549s elapsed)

f2-par-datalist.sh
  TASKS: 6 (1 bound, 5 peak workers (5 total), using -N2)
  SPARKS: 2 (2 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)
  Total   time    0.756s  (  0.445s elapsed)

f3-seq-datamap.sh
  Total   time    0.168s  (  0.178s elapsed)

f4-par-divide-conquer.sh
  TASKS: 6 (1 bound, 5 peak workers (5 total), using -N2)
  SPARKS: 8 (4 converted, 0 overflowed, 0 dud, 0 GC'd, 4 fizzled)
  Total   time    0.259s  (  0.166s elapsed)

f5-seq-chunkedlist.sh
  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)
  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)
  Total   time    0.194s  (  0.205s elapsed)

f6-par-parmap.sh
  TASKS: 8 (1 bound, 7 peak workers (7 total), using -N3)
  SPARKS: 51 (50 converted, 0 overflowed, 0 dud, 0 GC'd, 1 fizzled)
  Total   time    0.250s  (  0.107s elapsed)

f7-par-using-strategy.sh
  TASKS: 10 (1 bound, 9 peak workers (9 total), using -N4)
  SPARKS: 106 (96 converted, 0 overflowed, 0 dud, 0 GC'd, 10 fizzled)
  Total   time    0.269s  (  0.085s elapsed)

f8-par-parunion.sh
  TASKS: 10 (1 bound, 9 peak workers (9 total), using -N4)
  SPARKS: 157 (96 converted, 0 overflowed, 0 dud, 1 GC'd, 60 fizzled)
  Total   time    0.265s  (  0.085s elapsed)
```

Or you can run them individually to see detailed output:
```
$ ./fastest-par.sh artamene.txt
T0: start calculating frequency of elements in a list.
After parCount return. Time since start of program: 0.00s

Top 10 words in artamene.txt:
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

After word frequency print. Time since start of program: 4.64s

Top 10 characters in artamene.txt:
' ':	1988692
'e':	1446254
's':	731644
'i':	623145
'u':	603616
't':	599321
'a':	584293
'n':	568182
'r':	559543
'o':	533967

After char frequency print. Time since start of program: 4.64s
   7,776,976,904 bytes allocated in the heap
   3,998,967,376 bytes copied during GC
     341,213,360 bytes maximum residency (15 sample(s))
       8,798,480 bytes maximum slop
             947 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      6070 colls,  6070 par    8.915s   1.695s     0.0003s    0.0017s
  Gen  1        15 colls,    14 par    3.025s   0.935s     0.0623s    0.2119s

  Parallel GC work balance: 54.78% (serial 0%, perfect 100%)

  TASKS: 10 (1 bound, 9 peak workers (9 total), using -N4)

  SPARKS: 5344 (4298 converted, 0 overflowed, 0 dud, 2 GC'd, 1044 fizzled)

  INIT    time    0.001s  (  0.001s elapsed)
  MUT     time    2.540s  (  2.014s elapsed)
  GC      time   11.940s  (  2.630s elapsed)
  EXIT    time    0.003s  (  0.022s elapsed)
  Total   time   14.485s  (  4.668s elapsed)

  Alloc rate    3,061,624,742 bytes per MUT second

  Productivity  17.6% of total user, 54.5% of total elapsed

gc_alloc_block_sync: 583302
whitehole_spin: 0
gen[0].sync: 744
gen[1].sync: 187665
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
