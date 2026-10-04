# Contributing to Percolisp (PCL)

Issues and pull requests are welcome at
<https://github.com/Percolisp/pcl>. This page says what makes a bug report
useful and how to test a change before you send it.

## Reporting a bug

The most useful bug report is **one small program, run twice**: once by
`perl` and once by `pcl`. That is the comparison PCL is judged by, and it
turns the report straight into a test case:

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

`pcl --check bug.pl` does both runs for you and prints the first line where
they differ, perl's beside PCL's, with the two exit statuses. Its output is
most of a good report ([`docs/pcl-check.md`](docs/pcl-check.md)). It runs
the program twice, so do not use it on a program whose side effects must
happen only once.

Please include:

* **the smallest program that shows it** (cut everything that still leaves
  the difference visible);
* **perl's output beside PCL's**, as above, including the exit status if
  that is the difference, and any message on stderr;
* **`pcl --version`** (it prints the PCL, SBCL and PPI versions);
* **your operating system**.

`pl2cl bug.pl` prints the Common Lisp PCL made of your program. Pasting the
relevant few lines often shortens the diagnosis, but it is never required.

Before filing, two pages may already answer it:
[`docs/not-supported.md`](docs/not-supported.md) lists everything PCL
deliberately does not do and why, and [`docs/STATUS.md`](docs/STATUS.md)
says what currently passes, measured.

## Sending a change

Run the regression suite first:

```bash
tools/prove-core                   # the whole suite, on a freshly built runtime core
tools/prove-core Pl/t/sort-01.t    # or one file; it takes any prove arguments
```

It must pass. CI runs the same suite on a stock Ubuntu machine for every
push to `main` and every pull request, so a failure here is a failure there.

A change that fixes a bug should come with a test that fails without it:
either a new assertion in an existing `Pl/t/*.t` file that covers the area,
or a new `Pl/t/<topic>-01.t`. What matters for a test file is how long it
takes to run, not how many assertions it has; `prove --timer Pl/t/<file>`
tells you.

If your change alters what the compiler emits, `tools/corpus-diff.pl` shows
the difference over a corpus of real Perl. Explain every differing file in
the pull request: CI runs the same comparison against the pull request's
base, and fails when the output differs.

[`CLAUDE.md`](CLAUDE.md) records the project's working rules in much more
detail. It is written as instructions for the AI sessions that do much of
the development, so it is dense reading, but it is an honest account of how
changes are made and verified here.

The task list those sessions work from, and the notes they keep between
sessions, are in the repository too, in
[`dot-claude-in-home-dir/`](dot-claude-in-home-dir/README.md): every `#NNNN` in
the documentation is a file there.  Its README says how to read a task and how
to link the directory into your own `~/.claude/` if you take up the work with
Claude Code.

## License

PCL is free software under the same terms as Perl itself: the Artistic
License 1.0 or the GNU GPL version 1 or later (see [`LICENSE`](LICENSE)). By
contributing, you agree that your contribution is offered under those same
terms. Every PCL source file carries the licence header;
`tools/tag-license FILE` adds it to a new one, and the regression suite
checks that none is missing.
