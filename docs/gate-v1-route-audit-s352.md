# The Pl/t gate's live v1 routes (s352) — guardrail §5a.2, re-measured

**Verdict: the E4.1 precondition "the live v1 share must be ZERO" is NOT
satisfied.  The gate itself still depends on v1 in six TODO families,
27 events.**

## Why this was not known

`docs/v1-live-share-audit.md` (s342c, task #225) measured two populations:
the full `perl-tests` sweep and the four-dist CPAN board.  It never
measured **`Pl/t/` — our own gate**.  Every family below is invisible to
that audit for exactly that reason, which is also why `fable-answers-s345.md`
could record the multi-segment refusal as having **"ZERO measured events"**:
zero in the two populations it looked at, fifteen in the one it did not.

Method (same side-channel as s342c — a `pipeline=v1` cache grep under-counts
badly, see that doc's method note):

```
PCLXS_DIR=~/pclxs PCL_V2_AUDIT_LOG=<path> tools/prove-core
```

Run at `ef1b3de` + the F8 deletion; gate green (131 files / 4652 tests,
`Result: PASS`) — these are routes taken by a PASSING gate, i.e. rows that
pass *because v1 answered*.

## The inventory — 27 events

| n | class | route text | source |
|---|-------|-----------|--------|
| 15 | TODO | `eval-mode multi-segment (top-level package statement)` | **Moo / Sub::Quote** generated evals |
| 5 | DIE | `PCL: Can't modify non-lvalue subroutine call in assignment` | self-resolving (see below) |
| 2 | TODO | `poisoned condition-my $name / $asserter (brace-deref)` | **Moo**: `Method::Generate::Constructor`, `::Accessor` (real site_perl) |
| 2 | TODO | `bare $# magic` | test-generated `.pl` |
| 1 | TODO | `my-lexical 'mix' spans a package boundary` | `Pl/t/transpile-test-02.t:572` |
| 1 | TODO | `lexical 'x' possibly captured by nested sub capinner` | `Pl/t/transpile-test-01b.t:501` |
| 1 | TODO | `file lexical 'e' captured by sub loop` | `Pl/t/writes-args-01.t:87` |

**The 5 DIE events need no work.**  They are v2 correctly raising a
Perl-level error that the fallback then pointlessly retries and v1 raises
identically (the s342c F7 finding).  At the flip they simply become `$@`,
unchanged.

**The 22 TODO events are all load-bearing.**  The three named Pl/t rows are
*value* assertions (`test_transpile` / `run_cl` with expected output), so
the correct output they check is produced by v1 today; at the flip the
refusal becomes a hard error and the row fails.  The Moo families are worse
than a test row: they are the OO framework itself.

## Family notes

**M1 — eval-mode multi-segment (15).**  This is the refusal `#226` left
behind, and `fable-answers-s345.md` §2 ruled it "residual … ZERO measured
events", to be rephrased perl-shaped in the step-2 commit.  The measurement
says otherwise: it is **Sub::Quote's code generator**, the mechanism Moo
builds every accessor and constructor with.  The eval text looks like

```perl
{ my $_UNQUOTED = ${$_[1]->{"$_UNQUOTED"}}; my $_QUOTED = …; package Foo; … }
```

— a `my` line FIRST, then the `package`.  `#226` collapses only the shape
where `package X;` **leads** the eval, so Sub::Quote misses it by one
statement.  Rephrasing this refusal instead of fixing it would take Moo
off the supported list, so the s345 ruling cannot be executed as written.

**M2 — poisoned condition-my, brace-deref (2).**  Moo's own
`Method::Generate::Constructor` / `::Accessor` gate to v1 as whole FILES.
Same class as `#229` (F5, two perl CORE modules), which was ruled
pre-work and fixed; these two are the same finding one population over.

**M3 — bare `$#` magic (2).**  A v2 refusal (`Pl/Parser2.pm:507`) on
test-generated files.

**M4/M5/M6 — the three capture/span refusals.**  `my`-lexical spanning a
package boundary, lexical possibly captured by a nested sub, file lexical
captured by a sub.  These are the E1-era refusals; the tests that exercise
them were written to prove PCL gets the VALUE right, and they do — via v1.

## What this changes

Guardrail §5a.2 is explicit: *"Every v1 hit found is PRE-WORK to fix before
step 2 — never an acceptable loss."*  On that rule, step 2 (the gate flip)
cannot land until M1-M6 are closed or re-ruled.  M1 alone is a compiler
feature (multi-segment eval regions), not a session of cleanup, and M4-M6
sit in the capture/span machinery that `#153`/Option B was scoped to make
tractable.

The alternative readings — flip anyway and accept the gate rows, or rule
some of these refusals the way multi-switch and F6 were ruled — are policy
calls above this seat.  They are written up as an ask in
`docs/opus5-review-requests-s352.md` §1.

## Reproducing

```bash
rm -f /tmp/gate-audit.log
PCLXS_DIR=~/pclxs PCL_V2_AUDIT_LOG=/tmp/gate-audit.log tools/prove-core
perl -F'\t' -lane 'print "$F[0]\t$F[1]\t$F[3]"' /tmp/gate-audit.log | sort | uniq -c | sort -rn
```

`PCL_V2_AUDIT_KEEP=<dir>` additionally copies each transient `/tmp/*.pl`
aside before its test unlinks it — needed to see the M3-M6 sources as text.
