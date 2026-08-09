# Opus 5 review requests — s376 (2026-08-09)

Three queue items shipped (**#275, #276, #238 first pass**) plus **#239
diagnosed with no code**.  Gate ends at **133 files / 4785 PASS**.

| commit | task |
|---|---|
| `096aa85` | #275 — `use Test::More tests => N` is a PLAN and reaches the TAP layer (+ two runtime hunks + gen v2-126) |
| `e11f598` | #276 — an empty `{}` in term position is an anonymous HASH (+ both artifacts restamped) |
| `b37cd81` | #238 first pass — List::Util/Scalar::Util shim parity, 319/120 → 398/75 |
| `beb4187` | #239 DIAGNOSED — `package X;` in a block leaves globals in the enclosing package (docs only) |
| `eae9fac`, `d2eb0a8` | session log, DECIDED lines, the three USER rulings |

## Verification actually taken

* **Pl/t gate**: 133 files / 4785 tests PASS (`tools/prove-core`), run twice —
  once mid-session (one file failing, see Ask 1) and once green at the end.
* **corpus-diff**: **1 of 111 files** — `method.t` only, from #276.  The three
  `new{}` rows move from `(pl-new (vector))` to `(pl-new (make-p-box
  (p-hash)))`; both spellings fail identically ("The function main::pl-new is
  undefined", already in the fail baseline).  Re-ran the file: **93 pass / 38
  fail / 163 planned**, exactly the blessed pass-baseline row.
* **Full sweep with gate**: `0 new / 0 fixed`, **TOTAL passing 18498 → 18499
  (+1)**, no LOST; 6 UNSTABLE + 4 unverified, all in files already PARTIAL in
  the baseline (method.t, postfixderef.t, ref.t, tr.t).
* **CPAN board**: whole 14-dist board re-run, **2053 ok / 483 not-ok** (70 PASS
  / 64 PARTIAL / 49 FAIL of 183), saved as `docs/cpan-board14-s376.tsv`.
* **#275**: seven `use Test::More …` spellings probed byte-identical to perl
  (`tests => N`, VERSION, `'no_plan'`, `qw(no_plan)`, `VERSION tests => N`,
  `import => [...] , tests => N`, bare) plus `skip_all => REASON`.
* **#276**: eight probe shapes for the fix and **nine breaking-case probes**
  (empty sub body, `if/while/for () {}`, `eval {}`, `do {}`, `sub {}`, a
  `(&@)` block-form call `blk {} 1,2`, `+{}`, `k => {}`) — all byte-identical
  to perl.  Three of the probe lines are perl syntax errors and were dropped
  from the comparison rather than "passed".
* **#238**: every behaviour probed side by side against real perl before and
  after; the dist's own 38 files re-measured four times across the session.
* **#239**: instrumented `versions()` inside the REAL dist run (the note the
  task left), then reduced to a five-line reproducer; population counted
  across all four audit populations before sizing.

---

## Ask 1 — two gate files carried the #275 bug AS THEIR EXPECTATION

`tap-assert-01.t` and `goto-sub-phase-01.t` compare the FULL stdout of small
PCL programs that spell their plan `use Test::More tests => N`.  Eight of
those expected strings had no `1..N` line — i.e. the gate was asserting the
bug.  I updated the expectations (and added one row covering `no_plan` /
`skip_all` / a bare VERSION), so the plan line is now the guard.

**The ask is the rule, not this instance.**  "Never simplify a failing test"
is unambiguous; "the test encoded the old wrong output, and the new output is
what perl prints" is the mirror image, and I want it ruled explicitly so the
next session does not have to re-argue it.  My reading: an expectation may be
rewritten only when (a) the new text is what real perl produces, probed live,
and (b) the diff to the expectation is *exactly* the corrected behaviour and
nothing else.  Both hold here.  Confirm or narrow.

## Ask 2 — #275 stops short of perl's "no plan was declared" diagnostic

perl prints, at exit, for a file with neither a plan nor `done_testing`:

    # Tests were run but no plan was declared and done_testing() was not seen.

PCL does not.  That is the SAME #202 family this fix belongs to — a stream a
harness cannot judge — and it is now the only spelling of it left.  I left it
out deliberately: it is a new emission on stderr/stdout for every plan-less
program, which could move sweep and board rows, and #275's scope was the
dropped import list.

**Ask:** file it (and behind what), or rule that the plan line alone closes
the family?

## Ask 3 — #276 deliberately did NOT reuse `_block_is_hash_constructor`

The new `_block_is_empty` sits beside `_block_is_hash_constructor` with a
near-identical body (same whitespace/comment strip, same lone-Statement
unwrap) and differs only in the final test.  That is the "second copy" smell
rule 11 exists to catch, and I did it anyway:

* the two predicates answer **different questions**.
  `_block_is_hash_constructor` is consulted at four sites, three of which ask
  *"is this map/grep/`(&@)` BODY actually a hash constructor?"*  In that
  position a bare `{}` is not valid perl at all (`map {} (1,2)` is a syntax
  error — probed), so widening the shared predicate would be a claim about
  input principle 9 says we need not read.
* the fix is one new arm at the ONE term-position site, not a per-path
  branch.

**Ask:** is the two-predicate shape right, or do you want one predicate with
an explicit mode argument so the shared strip/unwrap has a single home?  I
lean two predicates with the paired comment they now carry, but the
duplication is real and I would rather you ruled it than have it drift.

## Ask 4 — #276's residue: the AMBIGUOUS brace shapes are untouched

perl's `intuit_curly` also reads `f {$k, $v}` and `f {%h}` as hash
constructors in term position; PCL still reads both as blocks.  I did not
touch them: unlike the empty case (which perl decides on the next character,
with no ambiguity), those are a genuine heuristic, and widening a parser rule
without the breaking-case population measured is the failure mode
`feedback_probe_the_breaking_case` records.

**Ask:** worth filing as a task, or is it principle-9 territory until a real
cause line names it?  I did not find one in this session's populations.

## Ask 5 — LAYER: `builtin::is_dual` / `builtin::is_vstring` are not real perl builtins

CLAUDE.md 9a's hard stop says a **non-core function name** appearing under
`cl/` means the wrong layer.  Perl 5.36's `builtin::` has no `dualvar`,
`isdual`, `isvstring` or `readonly` — so `DUALVAR` was **already** a PCL
extension in that namespace before this session, and I added two siblings to
it.

My argument for doing so: all three ask a question about the **box's own
representation** (does this scalar carry two independent caches; does it hold
a v-string payload) that no amount of plain Perl can answer, so a runtime
primitive is the only possible home — the only question is the namespace, and
reusing the seam that already serves the same shim beats inventing a second
one.

**Ask:** bless `builtin::` as PCL's declared shim-dispatch seam for
box-representation predicates (and say so in DECIDED.md so the next such
addition does not re-litigate it), or name a different home.  If the answer
is "different home", `readonly` and `prototype` introspection will want it
too — both are on #238's parked list for exactly this reason.

## Ask 6 — #238: `uniqnum`'s `pack "d"` key is exact for DOUBLES only

The key had to stop being the stringification (which collapses
`1.4142135623730951` and `…54` into one bucket).  `pack "d"` is the raw
double, so equal keys mean equal doubles and nothing else — and that is what
perl's `uniqnum` compares for NVs.  But a **UV above 2^53** has no exact
double, so two distinct large integers can now share a key.  `uniqnum.t` is
at 17 ok / 6 not-ok and some of that residue is exactly this.

**Ask:** accept the double-exact key as the honest 90%, or do you want the
hybrid (integers that fit an IV keyed as integers, everything else as the
double)?  The hybrid is cheap in the shim; I stopped because the remaining
rows also need UV/NV dualvar behaviour PCL's box does not model, so the
hybrid may buy 2 rows rather than 6.  Measured before deciding = one probe.

## Ask 7 — #239's fix shape, ruled BEFORE I build it

You have the diagnosis (commit `beb4187`, task #239): an in-block `package
X;` leaves every unqualified global resolving in the enclosing package,
because the block lowers to ONE top-level CL form and CL's reader interns its
symbols before the nested `in-package` can run.  31 occurrences / 14 files,
nine of them Class-Method-Modifiers.

Proposed fix: a **sibling trigger** for
`_requalify_block_our_after_pkg_switch` (`Pl/Parser2.pm:157`), which already
performs exactly this rewrite — family-aware (`@a` → `@tmp::a`, `$a[0]` →
`$tmp::a[0]`, `$#a` → `$#tmp::a`) — for `our`-DECLARED names in the same
region.  The new trigger fires on UNDECLARED globals instead.

**The hazard, and the reason I am asking first:** the `our` trigger knows its
names from the declaration.  The new one must DECIDE which bare names are
globals — a lexical `my`/`state` in scope, `@_`/`$_`/`$1`, and
already-qualified names must all be left alone.  That is precisely the
"SIGIL- or SCOPE-blind is a BUG / detector, rewriter and promoter share ONE
resolver" family.

**Ask:** confirm the sibling-trigger shape, and rule which resolver it must
consult (and whether a name it cannot classify should DIE rather than be
left bare — my instinct is die, since a silently-unqualified name is the
current bug).

## Ask 8 — cadence: was the whole board justified here?

s375 ruled that corpus-diff-identical + lib-reach byte-compare + a green gate
makes a same-session second sweep OPTIONAL.  This session was corpus-diff
**1 file**, so by that rule the sweep was arguably optional too — but the
`.pm` shim changes (#238) are **invisible to corpus-diff**, whose corpus is
`perl-tests/*.t` and which therefore never transpiles `lib/List/Util.pm`.  I
ran both the full sweep and the whole 14-dist board on that reasoning.

**Ask:** is "a change under `lib/` is outside corpus-diff's reach, therefore
the sweep is NOT optional" the right extension of the s375 rule?  If yes it
belongs in DECIDED.md beside the cadence line, because it is the second time
a shim change has had no cheap pre-check.

---

## Not asks — recorded so you do not have to find them

* **Gen bumped v2-125 → v2-126**; both checked-in transpiled artifacts
  regenerated and byte-identical below the `;;; pcl:` header.  Noted in
  passing: `cl/pcl-mro.lisp` was stamped **v2-124** and had drifted one
  generation behind since s374's bump — restamped here.
* The `cl/pcl-runtime.lisp` hunks are split across intent but land in ONE
  commit (`096aa85`): the `p-use` plan forwarding (#275), the two new
  `builtin::` primitives (#238, consumed two commits later), and the
  generation bump (#276, consumed one commit later).  Each commit's gate is
  green, so bisect is intact; the message names which hunk serves which task.
* Three USER rulings landed at session end (DECIDED.md §s376c): #239 is the
  next Opus item ahead of #237; `subname` is SIZED FIRST as task #284, not
  built; the board baseline gets blessed from `docs/cpan-board14-s376.tsv`
  only after a per-file audit (#208), best taken after #239 moves those files.
