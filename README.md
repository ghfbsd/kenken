KENKEN
======

This repository contains a few simple tools for those who like to solve Kenken
puzzles (or grids).  A puzzle is represented graphically as a character tableau
that you can either type in or download to a file.
You can make an .eps file of the puzzle grid and print it for manual solving.
From the same representation, you can also use the brute-force recursive
back-tracking solver to find the solution to the puzzle if you're stuck.

GRID REPRESENTATION
===================

Puzzle grids up to 9x9 can be represented.  The scheme is simple.
A grid of letters defines each group of cells that yields a value under the
designated operation, addition +, subtraction -, multiplication x, and
division /.  A sample of a 4x4 grid is:

```
ABBC
AACC
D1CE
DFFE

A 8+
B 5+
C 11+
D 2-
E 6+
F 1-
```
which when printed looks like ![this grid image](/4x4.png).

If there is more than one puzzle in a file, use a header line to introduce
each new puzzle grid, for example:
```

.KK "4x4 grid"
ABBC
AACC
D1CE
DFFE

A 8+
B 5+
C 11+
D 2-
E 6+
F 1-

.KK "6x6 grid"
6AABBC
DEEFFC
DHEIIJ
DHKK4J
LMNNOO
LMM6PP

A 2/
B 15x
C 2/
D 12+
E 12+
F 3-
H 5-
I 3-
J 1-
K 2/
L 2/
M 12+
N 3-
O 2/
P 7+
```

PRINTING GRIDS
==============

Use the `kenken.sh` shell script to translate the grid into a
[`groff`](https://www.gnu.org/software/groff/) table.  (`groff` is pre-installed
on pretty much any modern Unix system.)
If you install `kenken.sh` in your search path, say /usr/local/bin, by
```

    install -c -m 755 kenken.sh /usr/local/bin/kenken

```
then you can print out a puzzle grid in the file `trial-grids` via,
```

    kenken < trial-grids | groff -me -t > /tmp/grid.eps


```
and then print `/tmp/grid.eps` for your solution enjoyment.

GETTING GRIDS
=============

Transcribe them from your local newspaper into the grid format, or, if you
know a puzzle number, download them from the puzzle creation site based on the
puzzle number.  Install the download tool in a directory accessible in your
search path, say /usr/local/bin, by
```

    install -c -m 755 kload.sh /usr/local/bin/kload

```
Then, to obtain the puzzle numbered N (usually five or six digits), download it:
```

    kload N > todays-grid

```
and print it and/or solve it.  `kload` will do a couple of other things.
To print out a puzzle's solution, try `kload N solved`.  If you want to
look at various internal representations of the grid, look at the shell script
code and try the other options.

SOLVING GRIDS
=============

The file `kenken-solver.R` is simple puzzle solver written in
[`R`](https://cran.r-project.org).  It uses a brute-force backtracking algorithm
to recursively solve a puzzle, *not at all* the way a human solves one; there
are simply too many possibilities.  (For guidance on improving your personal
solving skills, refer to the end of the README.)

|Grid size|  Possibilities    |
|---------|-------------------|
|   2x2   | $2$               |
|   3x3   | $12$              |
|   4x4   | $144$             |
|   5x5   | $34560$           |
|   6x6   | $\sim 25$ million |
|   7x7   | $\sim 125$ billion|
|   8x8   | $5\times 10^{15}$ |
|   9x9   | $5\times 10^{19}$ |

To use it, first start `R` and load the solver code.  Then you can solve any
puzzle (or
puzzles) by using the function `ksolve` to read a file and solve a puzzle in it.
By default, the function will solve the first puzzle it finds in the file.
If the file
contains more than one puzzle, provide the puzzle number (starting from 1) as
the second arg after the file name.
```
    #R                            # invoke R, command line input
    > source('kenken-solver.R')   # load solver code
    > ksolve('trial-grids')       # solve the puzzle grid in "trial-grid"

    > ksolve('trial-grids',2)     # solve the 2nd puzzle in the file

    > q()                         # this is how you quit R
```

If you don't want to see the progress odometer, add `odo=FALSE` to the
`ksolve` args, e.g.
```
    ksolve('trial-grids',odo=FALSE)
```

Depending on the structure of the puzzle, it will take up to 10 or so trials
to solve a 4x4 puzzle, and up to 100,000 or so to solve a 6x6 (though most are
solved with ~1000).
Even though this is a brute-force solution method, it avoids
futile trial solutions, considerably reducing the search space.

If you think that the puzzle might have more than one solution (good ones
don't), you can ask the solver to find them by including the keyword,
`all=TRUE` in the `ksolve` args, e.g.
```
    ksolve('trial-grids',all=TRUE)
```

If you are curious about the solution method, you can invoke a trace option to
see the progress of the backtracking search by adding a third arg to `ksolve`:

```
    ksolve('trial-grids',1,TRUE)
```
(Be ready for a lot of output.)

TRIAL GRIDS
===========

See `trial-grids` for a few grids to print and solve manually,
or to try the solver on.

References
----------

1. Watkins, J. (2012).  Triangular numbers, Gaussian integers and KenKen,
*The College Mathematics Journal*, v.43(1), 37-42.
