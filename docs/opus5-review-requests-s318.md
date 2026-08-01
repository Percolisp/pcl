# Review requests for Fable — from Opus 5, s318 (2026-08-01)

*Written at gen v2-91, gate 123/4452, sweep 0-new vs the re-blessed 698
baseline, census 111/111 v2-native.  Seven commits landed, each verified by
the full quadruple; the tree is shippable.  Nothing here blocks R1.*

*Session context: I worked the W1 queue from `v2-opus5-execution-plan.md` §4
in order — #142, then #154 (which #142 exposed), #151, #150 part 1, #149,
and the first near-green family.  The asks below are what I hit that I
should not decide alone.*

**Index — what each section wants from you**

| § | topic | the ask |
|---|---|---|
| 1 | the box has no referent-kind word (#154 residue, #155, #159) | **design: one model, or three point fixes?** |
| 2 | read-only aggregates (#159) | **design: which of three, given the `push` hot path?** |
| 3 | #149 in practice — it is per-ROW, not per-file | policy: confirm the strict reading + is the rest worth the sessions? |
| 4 | `do subname()` executes (#158) | design: confirm the fix layer (parser, not runtime) |
| 5 | avhv.t t13/t39 pass in isolation, fail in file (#156) | priority: worth chasing, or park? |
| 6 | the suite tsv is now incomplete by construction (#150) | process: when to regenerate, and does it gate R1? |
| 7 | what this queue taught about the queue itself | priority: the near-green list is mostly silent-wrong |

---

## 1. Three of this session's findings are the same missing thing: the box
##    has nowhere to record what KIND of referent it holds

This is the one I most want your read, because I hit it three times from
three directions and each time the local fix was worse than the gap.

**The evidence, in the order it appeared.**

1. **#154.** `$aryref->{k}` must die `Not a HASH reference`.  I got 12 of 14
   shapes matching perl by guarding the deref funnels.  Two shapes I could
   NOT do: `$$aryref` (must be `Not a SCALAR reference`) and
   `$scalarref->{k}` (must be `Not a HASH reference`).  Both fail for the
   same reason — the guard has to ask "is this remaining `p-box` a scalar
   ref, or a representation layer?" and **there is no way to tell**.  PCL's
   `\%h` is a DOUBLE box, so `unbox` peeling one layer legitimately leaves a
   box; `p-ensure-hashref` returns it and `(setf p-gethash)` unboxes it
   again.  I learned this the hard way: counting a box as a mismatch broke
   `$refref->{k} = $v` and cost me a gate + sweep cycle.
2. **#155.** `tie %h, 'Tie::StdHash'` is a SILENT no-op.  `p-tie` starts
   `(unless (p-box-p box) (return *p-undef*))`, and an aggregate arrives raw
   (a hash-table / vector), so the tie is dropped and the program runs on the
   untied aggregate.  Scalar tie is correct end to end.  The runtime says so
   itself: *"Phase 1: scalars only.  Arrays/hashes require boxing those types
   first."*
3. **#159.** `Internals::SvREADONLY(@a, 1)` reports success and does nothing,
   because a raw CL vector has nowhere to hang the flag.

Three symptoms, one cause: **scalars carry a box (with `sv-ok`, `nv-ok`,
`class`, `is-ref` …) and aggregates carry nothing.**  Every per-site
workaround I considered was either a hot-path cost or a lie.

**The ask.**  Is there a model you want here, or should these stay three
separate blessed gaps?  What I can see:

- **(A) A referent-kind tag on the box.**  Cheap for #154's two shapes (one
  more slot, written at refgen, read only on the already-failed path).  Does
  nothing for #155/#159, which need the AGGREGATE to carry state.
- **(B) Boxed aggregates** — the thing the `p-tie` comment anticipates.
  Solves all three, and is the biggest change in the runtime's data model; I
  would not start it without your design.  It also lands squarely on Target A
  (every array/hash access grows an indirection).
- **(C) A side table** (weak EQ hash) keyed by aggregate, holding tie-proxy +
  read-only + whatever else.  No representation change, no cost on READS,
  but a probe on every mutation (`push`/`STORE`/…) — see §2 for why that
  worries me.
- **(D) Leave all three as documented gaps.**  Honest, but #155 and #159 are
  currently SILENT, which is the failure mode CLAUDE.md rule 12 exists to
  stop; at minimum they should announce themselves.

My instinct is (D)+loud for R1, and (B) as a real E5-era design item — but
the data-model question is yours, and I have not written any of it.

---

## 2. #159 read-only aggregates: the three fixes cost different things, and
##    the cheap one changes array representation

Concretely, from three near-green files that share ONE mechanism:

```
op/push.t t32   "croak when pushing onto readonly array"
op/splice.t t33 "croak when splicing into readonly array"
op/unshift.t t19 "croak when unshifting onto readonly array"

my @a = (10,11); Internals::SvREADONLY(@a, 1); eval { push @a, 9 };
perl: $@ = "Modification of a read-only value attempted", @a unchanged
PCL : SvREADONLY succeeds, push SUCCEEDS, @a = 10 11 9      (probe s317/ro.pl)
```

- **(a) weak-hash probe in `p-push`/`p-unshift`/`p-splice`.**  A `gethash`
  per push.  push is ~50-100ns, a weak-hash probe ~20-30ns — order 30% on a
  very common op.  Against Target A; I would not ship this without you
  saying the correctness is worth it.
- **(b) `SvREADONLY` swaps the storage for a SIMPLE vector** (no fill
  pointer, not adjustable), so `vector-push-extend` fails BY CONSTRUCTION.
  **Zero hot-path cost**, and it converts a silent-wrong into a real die.
  Costs: the message is SBCL's, not perl's (which would then make these three
  rows legitimate #149 material); anything assuming adjustability must be
  audited; `SvREADONLY(@a, 0)` has to restore.
- **(c) bless as not-supported** — extend `not-supported.md`'s "Read-only
  constants" section, which today covers only `\undef` stash tricks on
  SCALARS, and register the three rows.  Needs sign-off (CLAUDE.md 4), and it
  is NOT #149 material as it stands: the assertion is "must die at all", not
  "must say X".

**I recommend (b)** — it is the only one that keeps `push` fast while making
the behaviour right.  But it changes how a read-only array is represented, so
I want your call before implementing.

---

## 3. #149 in practice: the category is approved, the APPLICATION is per-ROW,
##    and the first file proved why

You approved the blanket category in §6d and I have landed the first
instalment (a83a99a).  Two things worth your eyes.

**do.t looked like 4 clean rows and was 6.**  t64/66/67/68 are pure
rejection assertions (`like $@, qr/\Asyntax error/`, do.t:280) — registered.
But the same family has t63/t65, "do subname(arg) called" / "do subname()
called": the file's `fail()` guards, and they fire because **PCL actually
EXECUTES** the form perl removed in 5.20 (§4).  Registering those under an
approved category would have buried a real behavioural divergence.  I left
them failing.

**Ask (a):** confirm the reading I applied — *a row that asserts a
side-effect did not happen is never error-text material, even when it sits
inside an otherwise-qualifying family.*  I think that is what §6d's "a test
that also checks a value never qualifies" means, extended to "checks a
behaviour".

**Ask (b):** is the rest of #149 worth the sessions?  A scan of the 698-row
baseline for `/error|die|Can't|syntax|invalid|Missing/` returns ~45 rows, but
most check a VALUE or behaviour, not text — eval.t's "No segfault inside
sort …" rows are behaviour; "eval syntax error in list context" checks the
CONTEXT of the return.  Each needs the test source read.  My estimate is
several sessions for pure bookkeeping.  I would rather interleave it than
run it as a campaign; say if you disagree.

---

## 4. #158: PCL executes `do subname()` — fix in the parser, not the runtime?

```
sub subname { print "CALLED\n" }
eval 'do subname("arg")';
perl: $@ = 'syntax error … near "do subname("'   (removed in 5.20)
PCL : prints CALLED, $@ empty                    (probe s317/do1.pl)
```

Perl's own message is not required (your 2026-07-28 ruling); **not executing
the sub is the point**.  So the fix is to stop accepting the form where it is
parsed, and the layer question is whether that is the `do` statement handler
in `Pl/Parser.pm` or the `do` term in PExpr.

**Ask:** confirm parser-layer, and — more importantly — confirm this is NOT
in the `$end_pars` region you fenced off in `pexpr-term-parsing-review.md`.
I have not touched it pending your answer.  If it IS in that region, this
becomes another E5.0 consumer rather than a W1 item.

---

## 5. #156: two avhv.t rows behave differently in-file than in isolation

After #154, `t/op/avhv.t` is **38 pass / 2 fail** (was 0/40).  The two
survivors:

```
t13 (avhv.t:130)  eval { my $slice = join('', 'x', @$a{'abc','def'}, 'x') };
t39 (avhv.t:275)  eval { (%$avhv, @extra) = (foo => 42, …) };
```

Both must set `$@` to `/^Not a HASH reference /`, and **both do exactly that
when I probe the expressions standalone** (scratchpad s317/av3.pl matches
perl).  Inside the file they do not.  Candidates I did not chase: the
container is a TIED array at that point (and tie-on-aggregate is #155's
no-op, so it may not be what the test thinks it is); t39's LHS is a
list-assignment to `%$avhv` which may never reach `p-cast-%`; t13 wraps the
slice in `join` inside `eval`, so context/flatten may differ.

**Ask:** worth a session, or park?  It is 2 rows, but "same expression,
different answer depending on surrounding code" is the kind of thing that
usually means a context/lowering bug with a wider blast radius than the rows
suggest.  That is why I am asking rather than parking it myself.

---

## 6. #150 part 1 landed — the checked-in suite tsv is now incomplete by
##    construction, and I did not regenerate it

Dropping the copied-file skip made **91 files** visible that the runner had
been skipping because `perl-tests/` had a copy (op 82, base 5, opbasic 3,
io 1).  It was hiding real failures — against copies the sweep calls clean:

```
op/tr.t    P:317/0  C:270/46      op/state.t  P:166/0  C:125/37
io/scalar.t P:128/0 C: 94/34      op/chop.t   P:148/0  C: 96/4
```
(re-verified at `--jobs 1` on a quiet machine: identical, so not artefacts of
the loaded run — see §7.)

88 of the 90 runnable ones: **29 OK, 59 DIFF**, with **16 near-green files
≤4 rows off perl**, listed in #150 as the worklist.

**`docs/perl-suite-run.tsv` predates this and has no rows for those 91** —
incomplete, not wrong.  A full re-run grows it by ~91 rows.

**Ask:** does that regeneration gate R1, or is it E4.1-time work?  It wants
per-dir FOREGROUND chunks (the 10-min background cap kills `--all` before it
writes `--tsv`), and on this machine it wants `--jobs 2-4` (§7).  I did not
start it because a partial regeneration is worse than none.

---

## 7. What the queue taught about the queue

Not an ask, a data point for how you order the remaining W1/W2.5 work.

**Four of the six items I worked were SILENT-wrong, not the visible failure
the task described.**  Deref answering `undef` where perl dies; `tie` on an
aggregate doing nothing; `SvREADONLY` reporting success and doing nothing;
`localtime` ignoring `$ENV{TZ}` *while a code comment claimed it handled TZ
env vars*.  In each case the suite row was the symptom and the real gap was
"programs get wrong answers with no error".

That suggests the near-green list is a better bug-finder than its row counts
imply, and it is why I would rather spend the remaining W1 window there than
on #149 bookkeeping.

**Two mechanical notes for the record:**

- The laptop was short on memory during the big runs (user).  An OOM-killed
  SBCL emits PARTIAL TAP, which the runner scores as a normal DIFF with
  depressed counts — so any numbers taken from a loaded run need re-checking
  before they are believed.  I re-ran the four headline files at `--jobs 1`;
  they reproduce exactly.  Separately the runner ends such a run with **no
  row, no summary, exit 0** (#157) — indistinguishable from a run never
  requested.  I plan to fix that with an END block that prints a KILLED row
  for everything still queued and exits nonzero; say if you want a different
  shape.
- I put an untested runtime into a commit by `cp`-ing a pre-fix snapshot back
  over the live file to undo a HEAD comparison, then quoting the fixed
  version's numbers in the message.  Caught it re-running avhv.t; amended
  after full re-verification (1c9148e → 2d70df3).  Rule now in memory and the
  session log: compare with `git show HEAD:file` or a worktree, never
  overwrite the working file.

---

## Verification standard used for the seven commits

corpus-diff (emission-changing commits only — four of the seven are
runtime-or-tooling-only and touch no `Pl/` file), `tools/prove-core` full
gate, full sweep + `sweep-diff` vs baseline, census.  Every commit: gate
123/4452, sweep 0 new.  The baseline moved 702 → 698 exactly once, for the
four #149 rows, by EDITING those rows out rather than re-blessing from a run
— a wholesale re-bless would have written 695 and silently dropped the 3
eval.t/postfixderef.t rows that merely did not run.
