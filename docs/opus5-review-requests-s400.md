# Review request — s400 (Opus 5), for Fable

Four commits.  Two finish work s399 left open (#324's verification, #344's
drift), two are the v0.1 hygiene item (#207/#278).  Everything is measured.

The parts worth your time: **§2**, where a measurement trap falsified a
"population is CLOSED" claim I made yesterday (and two more claims the same
day), and **§3**, where a fix was implemented, measured, and then deliberately
NOT landed — the USER ruled hygiene-only, and I want your read on the
condition for landing the rest.

Gate **143 files / 5278 tests** (+1 file / +3 rows = the new guard; failures
exactly the 13 pclxs xs rows the user said to ignore).  Full sweep run this
session; with the landed change it is baseline-clean (verified per-file:
perl-tests/closure.t 272 with its four blessed fails, pack.t 5636/89).
Cache generation unchanged (nothing emission-facing moved).

| commit | item |
|---|---|
| `b2cf620` s400a | #344 — ONE builder for the SBCL command line |
| `13b6e1a` s400b | #324 verification finished; #323's census corrected |
| `732ba36` s400c | #207 — which_perl stops naming the author's perl |
| `89cf15f` s400d | #278 — derived paths + a gate row that keeps them derived |

Filed: **#346** (run/cloexec.t hangs under a PCL child), **#347** (named sub
in anon sub does not capture the enclosing foreach variable — 24 rows),
**#348** (the held which_perl switch, blocked by both).

---

## 1. #344 — the drift that made #324 possible is now structurally impossible

FIVE runners spawn SBCL, not the four the task counted (`tools/pclperl-for-tests`
is the fifth, and it already went through `PCLCore`).  They now share
`tools/lib/PCLSbcl.pm`; a runner chooses WHAT to load, never the stack size,
the banner flags or the `--core` placement.

The bar was "no command line changes", and I want to flag HOW that was met,
because it is reusable: a `PCL_SHOW_SBCL=1` hook prints the exact string a
runner spawns, so before/after is a diff of six captured shapes rather than an
argument about equivalence.  The hook stays in the tree.  Empty diff once the
random temp paths are normalized.

One difference the move exposed, preserved deliberately: the sweep and
`./runpcl` never shell-quoted their paths, `run-perl-suite.pl` did.  `quote =>
0` keeps the two byte-identical.  Nothing in the repo has a path with a shell
metacharacter, so all three work — but two would break on one.  **Ask (small):
is preserving that worth it, or should the next filler unify to quoted with the
diff being exactly the backslashes?**

## 2. #324 finished — and the trap that has now falsified three measurements

s399 stopped the confirmation run at 391 of 521 files.  The last 135 (re/,
run/, uni/) now run with the flag: **125 identical**, 9 TIMEOUT-shaped (not
comparable — #326's six re/regexp*.t + re/overload.t, plus re/pat_psycho.t and
re/speed.t, which no longer crash and therefore now RUN the pathological
patterns they exist to time), **1 real mover**: re/pat_advanced.t 937/732 →
927/751.

I pinned the mover with two cheap measurements before bisecting: re-run at
`PCL_SUITE_STACK_MB=2` (the old default) → same 927/751, and diff the emitted
CL s398-vs-HEAD → byte-identical.  So not the flag and not the compiler.  The
bisect then lands on `6f04839` — **#323**, which s399 declared closed:

> "the population is CLOSED — eight files call these helpers anywhere under
> perl's t/, and all eight run"

The census used `grep -l`.  **`grep` prints NOTHING for a file it decides is
binary**, and perl's regex test files are full of control bytes.  The real
population is **eleven**; the three missed are re/pat_advanced.t (moved, same
cause as the other four), re/pat.t and re/reg_mesg.t (produce no rows at all).
The same trap hit the #278 survey in §4 (22 hits by plain grep, 31 by `grep
-a` — `cl/pcl-pack.lisp`), and it is the trap CLAUDE.md already documents for
`.tsv`.  DECIDED now carries it as a rule about CENSUSES, not just about
reading files: *anything that decides scope uses `grep -a` or perl, and a
guard that greps reads bytes.*

**No ask here** — I record it because it is the second time in two sessions
that a "population is closed" claim was the thing that broke, and the fix is
procedural.

## 3. #207 — implemented, measured, and half of it deliberately not landed (USER call)

`which_perl()` returned the literal `/home/bernt/…/bin/perl`.  Two separate
problems live in that one line:

1. **the machine-specific path** — fixed: it returns `$^X`, which the runtime
   derives, and dies rather than guess.  Byte-identical here, zero rows move.
2. **it names REAL perl**, so every child a test spawns through it runs perl
   and the assertions about that child compare perl to perl.  That is the
   vacuous-child problem #90 removed for `fresh_perl_*`/`runperl` via
   `$PCLPERL`.

I implemented (2) and measured it over all 19 companion callers plus the full
sweep:

| | before | after |
|---|---|---|
| 17 of 19 companion callers | — | unmoved |
| op/closure.t | 267/3 | **235/27** — honest |
| perl-tests/closure.t | OK 272/0 | **PARTIAL 240/28**, 64 s |
| run/cloexec.t | 16/6 | **HANG** (still hanging at `--timeout 400`) |

The honest failures are ONE family and the compiler announces it itself:
`Parser2 TODO: lexical 'foreach' possibly captured by nested sub named_57` —
a NAMED sub inside an ANON sub closing over the enclosing foreach/filescope
variable (task #347, the gap CLAUDE.md's TODO has carried since session 63,
now with a 24-row measurement).

The USER ruled **hygiene only**: two measurement HOLES bought with ~56 vacuous
rows made honest is the wrong trade while #176/#204 stand (a file that aborts
or times out contributes no rows at all).  Task **#348** holds the switch,
blocked by #346 (the hang) and #347 (the closure gap), with the diff described
in four lines so it is cheap to land later.

**Ask.** Is "#346 and #347 are fixed" the right release condition for #348, or
would you rather land the switch earlier and register the two files (the way a
crashing file is registered today) so the vacuous rows stop counting sooner?
My read is that #347 is a real compiler gap worth its own session and #348
should follow it, not lead it — but the argument for landing sooner is that
every session until then reads those ~56 rows as passing.

## 4. #278 — derived paths, and a guard that reads bytes

`grep -ran '/home/'` over Pl/ tools/ cl/ lib/: **31 hits, four places**.  One
was live (§3), three were tool defaults (now `PCLPaths::perl_suite_t` —
`$PCL_PERL_SUITE_T`, else `$PERLBREW_ROOT`, else `%Config{prefix}`, else a die
naming the override; `--tdir` still wins because it is resolved after the
argument loop), and 27 are the transpiled artifacts' preambles, which are
#217's emission change and untouched.

`Pl/t/no-hardcoded-paths-01.t` is in the gate: reads bytes, excludes the three
artifacts BY THEIR gen stamp (same discovery rule as
`artifact-staleness-01.t`) and COUNTS them so the exclusion cannot widen
silently, reports file:line, verified both ways with a scratch offender.

**Ask (small):** the exclusion is "line 1 says `;;; pcl: pipeline=… gen=…`".
That is the same rule two gate files now depend on.  Worth stating in
`ir-spec.md` as a promise about emitted files, rather than living twice in
`Pl/t`?

## 5. Where the queue stands

Done this session: #344, #324 (verification), #207, #278.
Filed: #346, #347, #348.
The v0.1 mechanical track still has **#277** (installer), **#279** (repo
hygiene — has a PENDING USER DECISION in it), **#280** (README/CHANGELOG),
**#282** (fresh-machine install test), **#283** (CI).  #279 and #277 are the
ones with user-facing calls in them.

Unchanged from s399: the three sized singles (#340 op/try.t, #341 op/lexsub.t
behind #337, #342 base/lex.t), the fillers (#337, #338, #339, #343, #330,
#321, #322, #326, #328), and your own next item (#281 macro vocabulary →
boxed aggregates).  The user's portfolio ASK (#345) is still open and s400
added one data point to it: a companion `--all` of 135 files at `--jobs 4`
took about 25 minutes, of which the nine TIMEOUT-shaped files are most of the
tail.
