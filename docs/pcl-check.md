# `pcl --check`: does PCL agree with perl on your program?

PCL is judged by one comparison: the same program, run by `perl` and by
`pcl`, must print the same thing. `pcl --check` makes that comparison for
the one program you care about. You need it because a wrong answer from
PCL is usually silent: most programs PCL gets wrong still exit 0 with
nothing on stderr.

```perl
# bump.pl
sub bump { $_[0]++ }
my %count = (a => 1);
my $ref   = { a => 1 };
bump($count{a});
bump($ref->{a});
print "hash: $count{a}\n";
print "ref:  $ref->{a}\n";
```

```console
$ pcl --check bump.pl
pcl --check: DIFFERENT OUTPUT — first difference at line 2
  perl: ref:  2
  pcl:  ref:  1
  (byte column 7 of that line is the first difference)
  exit: perl exit 0, pcl exit 0
  stderr: perl 0 lines, pcl 0 lines (stderr is not compared -- only counted)
  captured: perl.out perl.err pcl.out pcl.err in /tmp/pcl-check-MgEfxl
If perl is right and PCL is wrong, this is a bug worth reporting: https://github.com/Percolisp/pcl/issues
```

(A real run, 2026-09-27. This difference is a known one: writing through
`$_[0]` does not reach a hash element passed through a reference. It is
listed under "`@_` argument aliasing" in
[`not-supported.md`](not-supported.md), which is worth checking before you
report a difference.)

## What it does

`pcl --check [options] script.pl [args...]` or
`pcl --check [options] -e 'CODE' [args...]`:

1. runs the program under **the perl that runs `pcl` itself** (`$^X`, never
   a `perl` found on `PATH`), with the same arguments;
2. runs it under **PCL exactly as a plain `pcl` run would**: the same
   driver, so the script cache, the saved core and every option behave as in
   real use;
3. compares the two.

`-I`, `-M` and `-w` are given to perl too, since they mean the same thing
there; `-E` stays `-E` for perl. STDIN is `/dev/null` for both runs unless
`--check-stdin FILE` names a file, which each side then reads from the
start. Each child sees `PCL_CHECK_SIDE=perl` or `PCL_CHECK_SIDE=pcl` in its
environment (harmless; the tool's own test uses it to make a difference on
purpose).

**Compared:** STDOUT, byte for byte, and the exit status by class: success,
a non-zero exit, or death by a signal.

**Not compared:** STDERR. PCL aims to fail in the same *places* as perl,
not with the same words, so only each side's stderr line count is shown.
"perl warned or died and PCL said nothing" is exactly the case that count
reveals.

## The verdicts

The first line is the verdict; details follow. The last column is the exit
status of `pcl --check` itself.

| first line | meaning | exit |
|---|---|---|
| `IDENTICAL — N bytes of output, exit 0` | same output, both succeeded | 0 |
| `SAME FAILURE — output identical (N bytes), both failed (perl exit A, pcl exit B)` | same output, both failed; the exit codes may differ (perl's `die` exits 255, and the message text is not compared) | 0 |
| `DIFFERENT OUTPUT — first difference at line L` | the first differing line from each side (non-printable bytes as `\xNN`, long lines cut to a 200-byte window), the byte column of the first difference, both exit statuses, both stderr counts, and the first stderr line of a side that failed | 1 |
| `ONLY PERL FAILED` / `ONLY PCL FAILED` | same output, one side failed: both exit statuses and the failing side's first stderr line | 1 |
| `DIFFERENT FAILURE` | same output, both failed, but only one was killed by a signal | 1 |
| `cannot check — …` | no script was given, the script or the `--check-stdin` file is missing, `-c` was given, or perl or pcl could not be started | 2 |

After a difference, the four captured files (`perl.out`, `perl.err`,
`pcl.out`, `pcl.err`) are kept in a temporary directory, which is named.
The last line invites a report, with the project's issues URL.
`--check-keep DIR` keeps the four files in DIR whatever the verdict.

## Three cautions

* **The program runs twice.** Do not use `--check` on a program whose side
  effects must happen only once: one that sends mail, charges a card, or
  deletes or rewrites its input. Files the program writes are not
  compared, and both runs share the current directory.
* **Nondeterminism is not a PCL bug.** Output that depends on hash order,
  the clock, process ids or random numbers differs between any two runs,
  even two perl runs. Sort your keys, seed your `rand`, and leave out the
  time.
* **STDIN is `/dev/null`** unless you pass `--check-stdin FILE`. A program
  that reads from the terminal cannot be checked interactively.

## Not in scope

A timeout (a hang is visible anyway), comparing the files a program
writes, running each side in its own copy of the working directory, and
comparing stderr text.

## For maintainers

`pcl` parses the three options (`--check`, `--check-stdin`,
`--check-keep`) and, only when `--check` is given, loads
`tools/lib/PCLCheck.pm`, which holds everything else, so a plain run pays
nothing for it. It uses core modules only. The issues URL is read from
`README.md`, the one place it is written: keep the sentence "Issues and
pull requests are welcome at <https://github.com/Percolisp/pcl>" there
verbatim, angle brackets included (the installer copies `README.md` into an
installed tree for this reason). The test is `prove tools/t/pcl-check.t`;
it is not part of the `Pl/t/` suite.
