# `pcl --check` — does PCL agree with perl on YOUR program?

PCL is judged by one comparison: the same program, run by `perl` and by
`pcl`, must print the same thing.  The project runs that comparison over
thousands of programs; `pcl --check` hands it to the user for the one program
they care about.  Without it, a wrong answer is usually SILENT — in the
Rosetta census (#2104) 77 of the 177 programs that differ exit 0 with nothing
on stderr.

```console
$ pcl --check ref-types.pl
pcl --check: DIFFERENT OUTPUT — first difference at line 3
  perl: - - - - - Regexp - Foo Bar
  pcl:  - - - - - - - Foo Bar
  (byte column 11 of that line is the first difference)
  exit: perl exit 0, pcl exit 0
  stderr: perl 0 lines, pcl 0 lines (stderr is not compared -- only counted)
  captured: perl.out perl.err pcl.out pcl.err in /tmp/pcl-check-Ab12Cd
If perl is right and PCL is wrong, this is a bug worth reporting: https://github.com/Percolisp/pcl/issues
```

(A real run, on the everyday battery's `programs/ref-types`: a `qr//` object
is not blessed into `Regexp` under PCL — task #2051.)

## What it does

`pcl --check [options] script.pl [args...]` or `pcl --check [options] -e 'CODE' [args...]`:

1. runs the program under **the perl that runs `pcl` itself** (`$^X`, never
   a `perl` found on PATH), with the same arguments;
2. runs it under **PCL exactly as a plain `pcl` run would** — the same
   driver, so the script cache, the saved core and every option behave as in
   real use;
3. compares.

`-I`, `-M` and `-w` are given to perl too (they mean the same thing there);
`-E` stays `-E` for perl.  STDIN is `/dev/null` for both runs unless
`--check-stdin FILE` names a file, which each side then reads from the
start.  Each child sees `PCL_CHECK_SIDE=perl` or `PCL_CHECK_SIDE=pcl` in its
environment (harmless; `tools/t/pcl-check.t` uses it to make a difference on
purpose).

**Compared:** STDOUT byte for byte, and the exit status by CLASS — success,
a non-zero exit, or death by a signal.  **Not compared:** STDERR.  The
USER's bar for errors (DECIDED `## s494`) is that PCL fails in the same
PLACES as perl, not with the same words, so only each side's stderr LINE
COUNT is shown: "perl warned or died and PCL said nothing" is exactly the
failing-in-a-different-place case.

## The verdicts

The first line is the verdict; details follow.  Exit status of `pcl --check`
in brackets.

| first line | meaning | exit |
|---|---|---|
| `IDENTICAL — N bytes of output, exit 0` | same output, both succeeded | 0 |
| `SAME FAILURE — output identical (N bytes), both failed (perl exit A, pcl exit B)` | same output, both failed; the codes may differ (perl's `die` is 255, the text behind it is free) | 0 |
| `DIFFERENT OUTPUT — first difference at line L` | the first differing line from each side, non-printable bytes as `\xNN`, long lines cut to a 200-byte window, the byte column of the first difference; both statuses, both stderr counts, the first stderr line of a side that failed | 1 |
| `ONLY PERL FAILED` / `ONLY PCL FAILED` | same output, one side failed: both statuses and the failing side's first stderr line | 1 |
| `DIFFERENT FAILURE` | same output, both failed, but only one was killed by a signal | 1 |
| `cannot check — …` | no script, the script or the `--check-stdin` file is missing, `-c` was given, perl or pcl could not be started | 2 |

After a difference, the four captured files (`perl.out`, `perl.err`,
`pcl.out`, `pcl.err`) are kept in a temp directory that is named, followed
by the invitation to report it with the project's issues URL — read from
`README.md`, the one place it is written (the installer copies `README.md`
into an installed tree for this).  `--check-keep DIR` keeps the four files
in DIR always, whatever the verdict.

## Three cautions

* **The program runs twice.**  Do not use `--check` on a program whose side
  effects must happen once — it sends mail, charges a card, deletes or
  rewrites its input.  Files the program writes are not compared, and both
  runs share the current directory.
* **Nondeterminism is not a PCL bug.**  Output that depends on hash order,
  the clock, process ids or random numbers differs between ANY two runs,
  even two perl runs.  Sort your keys, seed your `rand`, leave out the time.
* **STDIN is `/dev/null`** unless you pass `--check-stdin FILE`.  A program
  that reads its terminal cannot be checked interactively.

## Not in scope (file a task if a user story appears)

A timeout (a hang is visible), comparing files the program writes, running
each side in an isolated copy of the working directory, comparing stderr
text.

## Where it lives

`pcl` parses the three options (`--check`, `--check-stdin`, `--check-keep`)
and, only when `--check` is given, `require`s `tools/lib/PCLCheck.pm`, which
holds all of it — a plain run pays nothing.  Core modules only.  Test:
`prove tools/t/pcl-check.t` (not in the Pl/t gate).  Smoke-tested at s494k
over the 122 everyday programs: its agree/differ answer matched the scratch
prototype `diffrun.pl` on 122 of 122.
