# #252 phase 2 — plan for the two big suite families (written s358, Fable)

Status: **APPROVED (s360, 2026-08-08)** — user: "#254 is approved, so we
can get a functioning compiler."  The fix scope (§§1–3, families A and B)
is authorized; Opus executes it (task #254 carries the session-1 recipe).
**§4's residue registration still needs its own sign-off** — bring it back
with the real shapes after the measurement session.  History: user
directive on #252 was "Do 1. Then write a plan for 2."; phase 1
(Text::Balanced) shipped in s358.

## 0. What is at stake

Of the 28 suite files the flip took dark (`docs/e41-step4-verification-s357.md`
§2.4), the two biggest families are:

| family | files | snapshot C_ok |
|---|---|---|
| capture / package-spanning lexical | 9 | io/through.t 942, op/attrproto.t 48, op/getppid.t 4, io/shm.t 21, op/exec.t 36, op/sub_lval.t 195, op/svleak.t 574, op/taint.t ~10k*, re/reg_eval_scope.t 44 |
| poisoned condition-`my` | 4 | op/my.t 156, op/while.t 27, re/pat_advanced.t 441, re/regexp_unicode_prop.t 660 |

*op/taint.t is the whale; measure its real row count when it transpiles —
the snapshot P_ok is 20,431-row-scale for the whole 28 and taint.t carries
the bulk after through.t/svleak.t.

The remaining 15 files (8 smaller families) are NOT in this plan's fix
scope; §4 proposes registering them.

## 1. Family A: capture / package-spanning (9 files, 3 spellings)

Gate messages: `file lexical captured by sub NAME`, `my-lexical spans a
package boundary`, `possibly captured by nested sub`.

These are E1-era M-work areas with EXISTING machinery: the shadow-aware
interp rewrite (#125), the spanning rename (M4, s354 — "resolves variables
canonically"), capture promotion (#43/#44).  The suite files carry shapes
the perl-tests/ copies don't.  Session 1 is MEASUREMENT, not fixing:

1. For each of the 9, capture the exact gate line + the source shape it
   fires on (variable, sub, package layout).  Expect the 9 to collapse to
   2–4 distinct SHAPES (the family messages already suggest 3).
2. For each shape, answer: does the existing rename/promotion machinery
   refuse it by PREDICATE (a conservative veto that can be widened with a
   probe of the breaking case) or by MECHANISM GAP (a genuinely unhandled
   layout)?  Predicate-wides are the cheap wins; mechanism gaps get named
   and sized, not fixed on the spot.
3. Ship the predicate-wides that fall out (with the
   probe-the-breaking-case rule); re-run the 9 + the quadruple.

Stop-rule: if after session 1 the mechanism-gap residue needs a NEW
promotion pass (not a widened predicate), stop and bring the sizing back
as an ask — that work would compete with E5, which rebuilds this layer
anyway.

## 2. Family B: poisoned condition-`my` (4 files, 2 spellings)

Gate messages: `poisoned condition-my $x (string eval)` and the plain
poisoned-condition shape.  The "poison" veto exists to protect the
section-let machinery from a condition-declared `my` visible to a string
eval; the M5/#247 work (whitelist predicates on #226's collapse) is the
sibling mechanism.  Same procedure as family A: measure the 4 files'
actual shapes first, widen predicates where a probe of the breaking case
passes, name anything needing new mechanism.  Note #205 (pending) is the
known adjacent bug — check whether any of the 4 actually needs #205 fixed
first.

## 3. Verification (per the s358-ratified rules)

- Per-family: re-run the affected suite files per-dir
  (`tools/run-perl-suite.pl --dir D`) against `docs/perl-suite-run.tsv` —
  a fixed file must reach its snapshot C_ok, not merely transpile.
- Per-session: the quadruple (corpus-diff explained-or-identical, gate,
  sweep gate, census) + generation bump on emission changes.
- Population rule: any "family is done" claim re-measures on a COLD cache.

## 4. The residue (15 files, ~? rows) — proposal, needs user sign-off

Register the 15 with their family causes in the suite expected/registry
mechanism and re-bless `docs/perl-suite-run.tsv` (rows edited in with
causes, per the #223 hygiene rule — never re-blessed wholesale from a
run).  Two of them deserve named tasks rather than registration:
`op/lexsub.t` (dies in v1's still-live EXPRESSION seam — that seam is
shared infrastructure, so its crash is reachable from supported code) and
`opbasic/cmp.t` (the #138 depth-0 comma-tail my-decl shape — a known
family with an owner).  `op/try.t` (compound `try`) is an unimplemented
feature, not a gap, and belongs in `docs/todo-features.md`.

## 5. Cost estimate

Session 1 (measure + predicate-wides) is bounded at one session.  If the
families collapse the way the messages suggest, total is 2–3 sessions for
~12k of the 15.1k rows.  The whale files (taint.t, through.t, svleak.t)
are all in family A, so family A first.
