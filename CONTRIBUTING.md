# Contributing to Percolisp (PCL)

Issues and pull requests are welcome at
<https://github.com/Percolisp/pcl>.

## Reporting a bug

The most useful bug report in this project is **one small program, run twice**
— once by `perl` and once by `pcl` — because that is exactly the comparison
PCL is judged by, and it turns a report into a test case:

```console
$ cat bug.pl
my @a = (3, 1, 2);
print join ",", sort { $a <=> $b } @a;
print "\n";

$ perl bug.pl
1,2,3
$ pcl bug.pl
3,1,2          # <-- wrong
```

Please include:

* **the smallest program that shows it** (cut everything that still leaves
  the difference visible);
* **perl's output beside PCL's**, as above — including the exit status if
  that is the difference, and any message on stderr;
* **`pcl --version`** (it prints the PCL version, the SBCL version and the
  PPI version);
* **your OS**.

`pl2cl bug.pl` prints the Common Lisp PCL made of your program; pasting the
relevant few lines often shortens the diagnosis, but it is never required.

Two things are worth a look before filing, because they may already answer
it: [`docs/not-supported.md`](docs/not-supported.md) lists everything PCL
deliberately does not do and why, and [`docs/STATUS.md`](docs/STATUS.md) says
what currently passes, measured.

## Sending a change

```bash
tools/prove-core            # the whole regression gate, on a fresh saved core
tools/prove-core Pl/t/sort-01.t    # or one file; it takes any prove arguments
```

The gate must be green, and CI runs the same suite on a stock Ubuntu machine
for every push and pull request, so a red gate here is a red gate there.

A change that fixes a bug should come with a row that fails without it —
either added to an existing `Pl/t/*.t` file that covers the area, or a new
`Pl/t/<topic>-01.t`. What matters for a test file is its **wall time**, not
its number of rows: `prove --timer Pl/t/<file>` says what yours costs.

If your change alters what the compiler emits, `tools/corpus-diff.pl` shows
the difference over a corpus of real Perl; every differing file should be
explained in the pull request. (CI runs this against the base of a pull
request and fails when it differs, so the explanation belongs in the
description.)

[`CLAUDE.md`](CLAUDE.md) records the working rules the project follows in
much more detail. It is written as instructions for the AI sessions that do
much of the development, so it is dense reading — but it is an honest account
of how changes are made and verified here.

## License

PCL is free software under the same terms as Perl itself: the Artistic
License 1.0 or the GNU GPL version 1 or later (see [`LICENSE`](LICENSE)). By
contributing you agree that your contribution is offered under those same
terms. Every PCL source file carries the licence header; `tools/tag-license
FILE` adds it to a new one, and the gate checks that none is missing.
