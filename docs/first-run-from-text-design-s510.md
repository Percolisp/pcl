# The first run of a program: run it from text, build the compiled file afterwards

*Design note, s510 (Fable, 2026-10-06), for task #2702.  Status: **DESIGN, measured with a
prototype; nothing is built.**  The prototype is twelve lines in a scratch copy of the tree
(`~/pcl-agent-scratch/s510/review/proto/`), never committed.*

## 1. The problem

`pcl prog.pl` keeps a compiled file (a fasl) per program.  On the run that finds none — the
first run, and the first run after every edit — it compiles the program's text into that
file and then loads the file.  Compiling executes the program's `use` statements (that is
what creates a module's package before the reader meets a symbol in it), but nothing else:
BEGIN blocks and the rest wait for the load.  So on that one run:

* a `use`d module's load-time output is lost (the compile is muffled);
* a printing `use` runs before an earlier BEGIN block;
* CHECK and INIT blocks of different files run in the wrong order;
* `M->import` runs twice (once at the compile, once at the load).

Every later run loads the compiled file and matches perl.  Measured on the task's table of
13 programs, each run twice against perl: **the building run matches perl in 4 of 13, the
second run in 11 of 13.**

An earlier attempt (s508) built the file in a forked child before the run.  It was rejected
in s509: the child runs every module's BODY, so a module with an external side effect (a
pidfile, a prompt on STDIN) does it twice, and a program that works today failed on its
first run.  The ruling: **only a first run in which every module body, every BEGIN block and
every line of the program runs exactly once is acceptable.**

## 2. The design

**On a run that finds no valid compiled file: run the program from its transpiled TEXT, and
build the compiled file after the program has ended, in the same process.**

The text path is not new.  It is what `pcl --no-cache`, `./runpcl` and almost every row of
the gate use, and it already runs everything once and in perl's order.  After the program
has ended, the image has every module loaded, so compiling the text there runs no module
body again: a compile-time `use M` finds `M` loaded and only calls `M->import` — in memory,
with output discarded, in a process that is about to exit.

The steps, in `%p-run-script-cached-1` (step 3 of that function today):

1. Transpile as today (the `.lisp` text and its `.deps` file are written before the run).
2. Register the deferred build, then load the text (`%p-load-script-text`).
3. The deferred build is the LAST exit hook, after the END phase.  It runs
   `%p-build-module-fasl` as it is today (it already discards `*standard-output*` and
   `*error-output*`), and only when ALL of these hold:
   * **same process** — the pid is the one that started the program.  Measured: without
     this, each of three forked children that called `exit` built the file again.
   * **the compile phase completed** (`*p-compile-phase-done*`).  Measured:
     `BEGIN { exit 4 } use Side;` — perl never loads `Side`; a build after that exit loads
     it.  A program that leaves during its compile phase stays uncached, which is correct.
   * **the exit is an ordinary one**: the end of the program, `exit N`, or an uncaught die
     at run time.  A signal, `POSIX::_exit` and `exec` run no exit hooks, so nothing is
     built and the program stays uncached.
   * the fasl cache is on, the entry is not marked failed, and the heap has room (skip the
     build above a stated fraction of the dynamic space; SBCL's heap here is 1 GB).
4. The build never changes the outcome: it runs inside a handler for every serious
   condition (not only `error`: heap exhaustion is not one), an `exit` called by an import
   during the build ends the build and nothing else, and the process leaves with the
   status the program set.
5. Before the build starts, standard output and standard error are flushed and pointed at
   `/dev/null` (standard error is kept under `PCL_FASL_DEBUG`).  A reader on a pipe then
   sees end-of-file when the program is done, not when the build is.

**Precondition, from the s508a batch:** a die during a text load prints SBCL's "While
evaluating the form starting at line N" lines (#2492).  s508a's #2764 removes them for a
module by loading its text from a plain stream; the script's text needs the same loader.
Without it two rows of the table (`bang-die`, `bang-die-use`) get WORSE on the first run.
So this is built after s508a is merged.

**What stays different from perl, to be written into `docs/not-supported.md`:**

* an `import` with an external side effect happens twice on the building run (as today —
  but now after the program, not before it);
* a program that never ends in an ordinary way (a daemon stopped by a signal, a wrapper
  that always `exec`s, a program that always leaves in BEGIN) is never cached and starts
  from text every time.

**Not in scope:** the eval-string cache, `pcl -e` (#1862), and the die-message rows that
belong to other tasks (`die-mod`: #2740 / #2764; `mod-eval-in`: #2762).

## 3. What was measured (prototype against perl 5.40.3 and against today's behaviour)

The prototype has step 2 and an unguarded step 3 only; `PCL_PROTO_OFF=1` gives today's
behaviour in the same tree.

**The 13-program table** (`proto/fr.pl`; each program in a fresh directory with a fresh
cache, perl once, PCL twice; STDOUT + STDERR + status compared):

| | first run = perl | second run = perl |
|---|---|---|
| today | 4 of 13 | 11 of 13 |
| prototype | 9 of 13 | 10 of 13 |

All seven rows that differ today on the first run only (`print-mod`, `warn-mod`,
`begin-before-use`, `mod-use`, `mod-nested`, `mod-two`, `mod-twice`) match perl on both
runs.  The four that still differ on the first run: `bang-die` and `bang-die-use` (the
precondition above), `die-mod` and `mod-eval-in` (other tasks, they differ today too).
`bang-die`'s second-run difference is a property of the harness, not of the design: the
file is written and run within one second, the cache counts a compiled file that is not
strictly newer than its source as stale, and the second run is therefore a building run
again.

**The three programs that decided the s509 ruling** (`proto/fr/s.pl`, `proto/fr2/pid.pl`,
`proto/fr2/ask.pl`): the pidfile module and the STDIN-asking module give perl's answer on
both runs.  The side-effect log of the first run:

    perl        BEGIN | M body | M import
    prototype   BEGIN | M body | M import | M import
    today       M body | M import | BEGIN | M import        (and M's STDOUT lost)

**Is a file compiled in a used image the same program?**  This is the design's one new
risk: macros that expand differently once the program's subs, packages and variables exist.
The everyday battery (122 programs) was run twice on one fresh cache under the prototype:
run A is every program's building run, run B loads the files built at exit.  **Both:
`EVERYDAY: 114 of 122`, every bucket 0** — the same line as main.

**The exit paths** (`proto/op/`): `exit 3` in the main code — status 3 on both runs, file
built; an uncaught die — status preserved, file built; `END { exit 5 }` — same as perl;
`use Test::More tests => 1` — the plan is printed once; the two shapes that need a guard
(forked children, `exit` in BEGIN) are listed in §2.

**Cost** (`proto/cost/`, wall seconds, three fresh copies each, box under load):

| program | today, first run | prototype, first run | second run (both) |
|---|---|---|---|
| 60 lines, two `use`s | 1.56 – 1.67 | 0.61 – 0.70 | 0.05 – 0.07 |
| 1026 lines, two `use`s | 8.39 – 8.61 | 8.87 – 9.64 | 0.06 – 0.07 |
| 3 lines, `use Getopt::Long` | 6.25 – 7.16 | 0.43 – 0.44 | 0.09 – 0.11 |

The program's output appears at once instead of after the compile; the wait moves to the
end of the run.  A hot loop is as fast on the first run as on later ones (30 million
iterations: 0.41 s first run, 0.21 s second, 0.45 s today's first run) — the text path runs
compiled code.

**Why the first run gets FASTER when the program uses modules** (time-stamped with
`PCL_FASL_DEBUG=1`): today the `use` statements run inside the compile of the program's
file, and a module loaded there is loaded from its TEXT (`module POSIX.pm -> TEXT`), every
time, although its compiled file is in the cache.  That is the first-run cost the README
describes ("5.3 seconds for a script using Getopt::Long") and the parked task #2420.  A
text run loads modules the ordinary way (`module POSIX.pm -> FASL HIT`), and the build at
exit finds them loaded.  So the design removes that cost as a side effect, whenever the
modules' own compiled files exist.  The 1026-line row shows the other side: a program
that is large itself pays its own compile twice in effect (a cheap text load, then the
file compile), about +10 %.

**Memory**: programs holding 50 – 230 MB of live strings exit with status 0 and a built
file under the prototype, as today.  A program that exhausts the 1 GB heap dies either way.

## 4. Decided against

* **A forked child that builds while the parent exits.**  It would hide the end-of-run wait
  and isolate the exit status completely, but the child inherits every open file, lock and
  socket (a `flock` taken by the program would stay held until the child closed it), and a
  process outliving the program makes tests that remove the cache directory race.  The wait
  it hides is the same wait today's first run has at its start.  Revisit only with a
  measurement that says the end-of-run wait hurts.
* **A fresh process that builds the file** (before or after the run): it has to load the
  modules, so their bodies run twice — the s509 ruling.
* **Keeping the in-process compile and skipping the load-time import**: counter-example in
  the task (`sub first {…}` before `use List::Util 'first'`).

## 5. The bar for the batch that builds it

Guard rows that fail before the change: the seven first-run rows; the forked-children, the
`BEGIN { exit }` and the pidfile shapes; a pipe reader seeing end-of-file before the build
(time-stamped); status preserved for `exit N`, a die and `END { $? = N }`.  Then the full
gate; the sweep and the companion's `run/` and `io/` directories (a runtime change);
`tools/everyday-smoke.pl` twice on one fresh cache (run A and run B, both equal to main's
line); `Pl/t/script-cache-01.t` read row by row for assertions about the building run; the
first-run cost table above re-measured on a quiet box.  `docs/ir-spec.md`'s load model and
`docs/not-supported.md` change in the same batch.
