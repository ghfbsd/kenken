KENKEN
======

This repository contains a few simple tools for those who like to solve Kenken
puzzles (or grids).  A puzzle is represented graphically as a character tableau
that you can either type in or download to a file.
You can make an .eps file of the puzzle grid and print it for manual solving.
From the same representation, you can also use one of the puzzle solvers to
give you the solution if you're stuck.  The two solvers are a brute-force
recursive back-tracking solver and a logic-based forward solver.  The
brute-force gives you an answer (usually quickly, but some times not), whereas
the logic-based solver gives you a step-by-step solution much the way a human
would solve the puzzle.


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

The solvers, `kenken-solver.R` and `kenken-logic.R` are both written in
[`R`](https://cran.r-project.org).  One, `kenken-solver.R`, uses a brute-force
backtracking algorithm to recursively solve a puzzle, *not at all* the way a
human solves one; there are simply too many possibilities.  The other,
`kenken-logic.R`, is a solver based on logical rules and represents much the
way a human would solve the puzzle.  You can run logic-based solver in a couple
of different ways.  One just gives you the answer (not very interesting, but
for the impatient, it's what you want).  Another draws a representation of the
grid and shows you how it changes after each move (more instructive).  The
third way shows you the grid, tells you what deductive rule it will use to
eliminate one of the possiblities, and then shows you the changes to the grid -
a tutorial mode for improving your skills.  (For other guidance on improving
your personal solving skills, refer to the end of the README.)

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

To use it, first start `R` and load one of the solver codes.  Then you can
solve any puzzle (or puzzles) by using the function `ksolve` to read a file
and solve a puzzle in it.
By default, the function will solve the first puzzle it finds in the file.
If the file
contains more than one puzzle, provide the puzzle number (starting from 1) as
the second arg after the file name.
```
    #R                            # invoke R, command line input
    > source('kenken-solver.R')   # load brute-force solver code
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

If you want to learn how to solve puzzles, use the logic-based solver:
```
    #R                            # invoke R, command line input
    > source('kenken-logic.R')    # load logic-based solver code
    > ksolve('trial-grids')       # solve the puzzle grid in "trial-grid"

    > ksolve('trial-grids',2)     # solve the 2nd puzzle in the file

    > q()                         # this is how you quit R
```
It shows you each step along the way, telling you what it will do, and then
shows you the grid after it does what it said.  You hit return after each step
to set the pace of the solution.  If you've seen enough and want to quit, type
`q` and hit return, which aborts the solution.

If you're not interested in learning but simply want to pasively enjoy the
solution progress, watch the solution movie by typing,
```
    > ksolve('trial-grids',trc=FALSE)  # turn off the pause between steps
```

Alternatively, if you just want the answer, dammit, try
```
    > ksolve('trial-grids',trc=NA)  # Enough with the words, cut to the chase
```

TRIAL GRIDS
===========

See `trial-grids` for a few grids to print and solve manually,
or to try the solver on.

You don't even have to download the solver to try it out.  After you start `R`
on your own machine, type
```
source('https://raw.githubusercontent.com/ghfbsd/kenken/main/kenken-solver.R')
```
or
```
source('https://raw.githubusercontent.com/ghfbsd/kenken/main/kenken-logic.R')
```
to define the solver functions, and then type
```
ksolve('https://raw.githubusercontent.com/ghfbsd/kenken/main/trial-grids')
```
to solve the trial grids.  You can also solve your own grids in a local file
this way by giving the local file name to `ksolve`.

References
----------

- Watkins, J. (2012).  Triangular numbers, Gaussian integers and KenKen,
*The College Mathematics Journal*, v.43(1), 37-42.

- [Wolfram community discussion](https://community.wolfram.com/groups/-/m/t/613040) on solving Kenken puzzles.
