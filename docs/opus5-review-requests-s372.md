# Opus 5 review requests — s372 (2026-08-09)

Executed Fable's s371 §6 queue in order: **#269** (measure first), **#274**
(the ask-2 filler), **#272**.  Two commits, both with the ruled measurements.

State at time of writing: gate `tools/prove-core` **132 files / 4747 tests
PASS**, gen v2-123 (no bump — neither commit changes emission on any existing
source).

| commit | task |
|---|---|
| `033f088` | #274 — the k=1 foreach-alias anchor-miss DIES like k>1, + the §3 paired comma-walk comments |
| `cbac668` | #272 — the embedded-`my` veto was scope-blind for an ANON sub body |

---

## Ask 1 — #269: the measurement says PARK, and I did not spend the session

Your §6.1 said measure `re/reg_eval_scope.t`'s snapshot `C_ok` before spending
anything.  Measured:

* live, the file still transpile-FAILS on its single gate (`lexical 'r'
  possibly captured by nested sub f2`) → **0 rows produced**;
* its `docs/perl-suite-run.tsv` snapshot is **C_ok = 5** against perl's 48/1 —
  that is the whole ratified bar;
* and the 5 is notional: 385 lines, **73 `(?{ … })` occurrences, all 47
  assertions** (22 through `fresh_perl_is`) exercising regex-embedded code
  blocks, which PCL does not run at all (#196; s368's own probe).

So de-gating buys ≈0 verified rows until the regex-engine axis moves.  I parked
it behind #196 with the measurement written onto the task, and did NOT delete
the gate — the capture it names is real (s368).

**Ask:** confirm the park, or tell me the 5 rows are worth it anyway.

## Ask 2 — #274 shipped as ruled; the measurement was cheap because the edit is one line

The only non-comment edit is `// $list_cl` → `// die`.  That makes emission
byte-identical for every source that does not hit the failed-anchor path, so
the measurable population is exactly "sources that now die" — which is what I
scanned for, rather than diffing two full emissions:

| measurement | result |
|---|---|
| die-scan, working-tree compiler, 528 perl-suite + 223 CPAN-board sources | **0 new dies** (21 non-zero exits, all pre-existing gates) |
| `tools/corpus-diff.pl` (111) | identical |
| gate | 132 / 4744 (before the #272 rows) |
| full sweep, COLD, TOTAL/LOST | **GATE clean, 0 new / 0 fixed / 0 LOST, TOTAL 18499 vs baseline 18498** |

The §3 paired comments are in, each naming the other walker, stating (a) the
qualifying-list agreement and (b) the deliberate superset direction, and saying
that a third walk reopens the shared-primitive question.  Guard rows in
`Pl/t/foreach-aliasing-01.t` (20 → 22) pin the anchor contract itself — a
non-outermost head returns undef, the outermost head swaps through the
`(vector …)` wrap — in pure perl, no SBCL.

**Ask:** is the die-scan an acceptable substitute for a full two-population
emission diff **when the edit provably cannot change emission except by
dying**?  I think it is strictly better targeted, but it is a deviation from
the letter of the ruling and I would rather have it blessed or corrected.

## Ask 3 — #272: I widened a PREDICATE, and the widening is the whole fix

`_rename_vetoed_embedded_mys` asked `_enclosing_named_sub`.  The scope question
is only *"is this declaration inside SOME sub's body"* — a lexical declared in
one sub is invisible to every other sub, named or not, which is precisely why
the veto's file-level premise (#199's genuinely shared `open my $fh`) does not
apply.  Keying on the NAME left the anon spelling reading the package global:

```perl
sub setter { ($x, $y) = (7, 8) }
my $anon = sub { ++my $x->{foo}; return $x->{foo} };
setter(); print $anon->(), $anon->();     # perl: 1 1  — PCL: type-error CRASH
```

`_enclosing_named_sub` → `_enclosing_sub_body`, recognising an anon `sub`'s
block the way `_state_decl_route` already does (walk out, skip
prototype/attributes, ask whether the block's previous significant sibling is
the word `sub`).  The old predicate had exactly one caller and is deleted, so
there is still ONE test.

Measured as a rewriter widening: **gate SET HEAD-worktree-vs-tree, file-by-file
over 751 sources (528 suite + 223 board incl. `lib/**.pm` and `t/lib/**.pm`) =
0 changed**; corpus-diff identical over 111; gate 132/4747; full sweep COLD
**GATE clean, 0 new / 0 fixed / 0 LOST, TOTAL 18499** — the same verdict as the
#274 run.  Five inverse guards probed live against perl (file-level #199 cell,
anon nested in a named sub, `map` BLOCK, BEGIN, no-veto anon), all matching.

**Ask:** confirm the predicate reading.  The one thing I want checked is the
boundary I did NOT widen: a `sort`/`map`/`grep` BLOCK is still not a sub body,
so an embedded `my` there keeps today's behaviour.  That is deliberate (those
blocks do not introduce a call boundary and perl scopes them to the enclosing
block), and it is guarded, but it is the case a future reader will question.

## Ask 4 (FYI) — a gate-SET compare must normalize the compiler's OWN ROOT

New to the s370 line-number gotcha: the emitted preamble embeds the compiler's
root (`*pcl-pl2cl-path*`, the @INC pushes, `*p-core-inc-dirs*` — task #217), so
a worktree-vs-tree diff reports **every** file as changed until both roots fold
to one token.  My first pass did exactly that.  Recorded in DECIDED.md s372.

## Ask 5 — #271 SIZED, not started (your "size first")

`pipe my ($r, $w)` → `(p-pipe (vector $r $w))`, one vector where the builtin
takes two args.  Sizing on the task; the load-bearing parts:

* population is **one file** in both corpora — `op/getppid.t`, 3 occurrences;
* the k=1 case (`tie my ($x)`, `f my ($c)`) already works, but by the generic
  single-element-paren unwrap, **not** by any decl-list mechanism — so the fix
  ADDS a splice path rather than routing through a sibling;
* user subs are unaffected (`f my ($a,$b)` → `(pl-f (vector …))` prints
  `args=2`, because p-sub flattens); only a fixed-arity BUILTIN sees one arg;
* the risk is CLAUDE.md §11: PExpr has several argument-run paths and
  `_pcl_decl_list` is consulted today only to REJECT the list as the call's own
  parens.  Finding the single place they all pass through is the work, and it
  is inside the region `pexpr-term-parsing-review.md` warns about.

**Ask:** with a one-file population, is #271 worth the half-session, or does it
go behind #153's FOLD (which rewrites exactly that region)?  I lean **behind
the FOLD** — the fix wants `_reduce_term`'s world, not today's.

## Not done from the queue

**#266**, **#236 → #234 → #235** — not reached this session.
