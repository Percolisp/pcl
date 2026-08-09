# Fable answers — s377 review of the s376 batch (2026-08-09)

Review of `docs/opus5-review-requests-s376.md` (commits `096aa85` #275,
`e11f598` #276, `b37cd81` #238 first pass, `beb4187` #239 diagnosis).

**Verdict: all four commits APPROVED as shipped.**  Every ask below is ruled.
Three new reviewer findings are recorded in §9 — none blocks the approval.

## 0. Verification independently taken (this review)

* **Pl/t gate re-run** (`tools/prove-core`, fresh core): **133 files / 4785
  tests, Result: PASS** — matches the claim exactly.
* **#276 probe**, six shapes vs real perl, byte-identical: the fix itself
  (`f {}, "x"` → `HASH/2`), the `(&@)` block-form call (`blk {} 1,2` →
  `CODE/3`), `do { }` → undef, `+{}` → HASH, `k => {}` → HASH.
* **#275 probe**: the one spelling I picked to re-verify was the hardest —
  `use Test::More import => ['ok'], tests => 2` — plan line and rows
  identical to perl (modulo the pre-existing `# PCL Test library loaded`
  comment line, which is TAP-comment-shaped and predates this batch).
* **#239 reproducer reproduced**: `eval { package Foo; $z = "Z" }` — perl
  `Foo::z=Z`, PCL `main::z=Z`.  Plus one NEW probe beyond the diagnosis —
  see §7 and §9.1: the `our`-alias case diverges in the OPPOSITE direction.
* **Ask 2 probe**: real perl prints the no-plan diagnostic on **stderr** and
  exits **254** (`# Tests were run but no plan was declared and
  done_testing() was not seen.`).
* **Ask 6 measurement** (the one probe the ask said would decide it): ran
  the dist's `t/uniqnum.t` under PCL and classified all six not-ok rows —
  see §6.
* **#238 spot-probe**: `looks_like_number` edge spellings, real XS vs the
  shim's logic — one residue found (`nanq`), §9.2.

## 1. Ask 1 — RULED: when an expectation may be rewritten

Confirmed, with the rule narrowed to four conjuncts.  A gate expectation may
be rewritten **only when ALL of**:

* **(a)** the new expected text is what real perl produces, probed live in
  the same session, and the commit says so;
* **(b)** the diff to the expectation is *exactly* the corrected divergence —
  no other text moves in the same edit;
* **(c)** the edit STRENGTHENS the assertion: it adds output that was
  missing, or replaces a wrong claim with perl's.  An edit that deletes or
  loosens any assertion is never this rule — that stays under "never
  simplify a failing test" and needs a USER/Fable call;
* **(d)** when the old expectation encoded the bug (as here), the same
  commit adds or keeps a row that guards the FIXED behaviour explicitly, so
  the correction is load-bearing and not incidental (done here:
  tap-assert-01.t §6b).

Both s376 instances satisfy all four.  This is the mirror image of "never
simplify", not an exception to it: simplifying weakens a test to dodge a
bug; this strengthens a test to stop asserting one.

## 2. Ask 2 — FILE IT; the plan line does NOT close the family

The no-plan ending diagnostic is the last spelling of #202's "a stream a
harness cannot judge must say so", and perl's behaviour is now probed: the
diagnostic goes to **stderr** (so it adds no TAP rows — sweep/board row
counts cannot move from the text itself) and the exit code becomes **254**
(which CAN move classifications in any consumer that reads exit status).
Filed as **task #285**, filler-sized, interleave — behind nothing.

Implementation bar for whoever picks it up: emit at process end when tests
ran with neither a plan nor `done_testing`; stderr comment + exit 254, both
matching perl.  The exit-code half makes it a detection-widening change →
the s372 Ask-2 standing narrowing applies (both-population scan of what the
new exit code flips, corpus-diff, sweep TOTAL/LOST).

## 3. Ask 3 — two predicates CONFIRMED; no mode argument, no refactor

Same ruling as s370's two-comma-walks ask, and for the same reason: the two
predicates answer provably different questions (`_block_is_hash_constructor`
is a claim about map/grep/`(&@)` BODIES, where a bare `{}` is a perl syntax
error — probed in the commit), and folding them would make the shared
predicate express a mode it has no caller for.  The paired comments they
carry are the drift control, exactly as ruled for the comma walks.

One standing trigger: the moment a THIRD consumer of the strip/unwrap shape
appears, extraction of a shared `_block_significant_children` helper stops
being optional — three copies is past the rule-11 line no matter how good
the comments are.

## 4. Ask 4 — file as a deferred task (the #191 pattern)

Filed as **task #286**, deferred, re-raise on a real cause line.  Reasoning:
this is a SILENT-WRONG family (the call's argument count changes with no
diagnostic), which is too dangerous to leave undocumented — but widening a
parser heuristic with zero population behind it is exactly the
`feedback_probe_the_breaking_case` failure mode, and unlike the empty case
perl itself decides `f {$k, $v}` / `f {%h}` by HEURISTIC (`intuit_curly`),
so the fix's bar is matching toke.c's actual rule, not guessing one.  The
task records the two shapes and that bar; nobody builds it until a sweep,
board, or CPAN cause line names it.

## 5. Ask 5 — `builtin::` BLESSED as the shim-dispatch seam, with constraints

Blessed, and recorded in DECIDED.md.  The argument is accepted in full: a
box-representation fact has no possible plain-Perl home, so the only
question was ever the namespace, and the seam that already dispatches
`DUALVAR`/`BLESSED`/`REFADDR`/`REFTYPE` beats inventing a second one.

Constraints that come with the blessing:

* **(1)** A name that real perl's `builtin::` also exports (`blessed`,
  `reftype`, `refaddr`, `is_weak`, …) must match perl's semantics exactly —
  those are reachable from user code as perl spells them.
* **(2)** A PCL-only name in the namespace is allowed **only** for a
  box-representation fact or constructor that plain Perl cannot express,
  and its intended consumers are `lib/` shims — it is never documented as a
  user-facing API.
* **(3)** Revisit trigger: if a future perl claims one of PCL's invented
  names with different semantics, perl wins and PCL renames its primitive.

On the two parked wants: **`readonly` — yes, same seam** when a scalar
read-only storage fact exists to ask about (it is a box fact, the #159
family).  **`prototype` introspection — NO**: `prototype` is a CORE perl
builtin, i.e. language, so it belongs in the runtime as the ordinary
builtin implementation, not behind the shim seam.

## 6. Ask 6 — HYBRID, and the measurement says it buys 4 of 6, not 2

The probe the ask deferred is now taken.  The dist's `t/uniqnum.t` six
not-ok rows classify as:

| rows | cause | verdict |
|---|---|---|
| 7, 10, 11 | UV/IV above 2^53 collapsing through `pack "d"` | **the hybrid fixes these** |
| 15 | `uniqnum undef` must RETURN `0`, not undef (perl coerces in the output) | one-line companion fix |
| 14 | warning on undef — the warnings model | stays behind #221 |
| 22 (unnamed) | in the `$Config{ivsize}` 1e17 block | re-measure after the hybrid |

So: **do the hybrid**, spec'd as — an argument that is integer-valued AND
within the exact-integer range (|v| < 2^64, i.e. fits IV/UV) keys as its
exact integer text; everything else keys as `pack "d"`.  The range bound is
load-bearing: an *integral NV* beyond 2^64 must stay on the `pack "d"` key,
because stringification at that magnitude prints ~15 significant digits and
would collapse distinct doubles — the exact bug the `pack "d"` key fixed.
Ship the row-15 undef→0 output coercion with it.  Expected result 4 of the
6, with row 22 re-measured after.  It is a `lib/` change, so §8's rule
applies to the session that ships it.

## 7. Ask 7 — sibling trigger CONFIRMED; the resolver contract, and DIE = yes

**Shape confirmed**: a second trigger beside
`_requalify_block_our_after_pkg_switch`, reusing `_rewrite_var_uses` (the
family-aware rewriter), firing on the switched region's UNDECLARED bare
variable names.

**The resolver contract.**  For every bare symbol in the switched region the
trigger needs a four-way classification, and it must come from the SAME
scope walk the rename/capture machinery already uses — not a new regex walk
beside it (the standing one-resolver rule):

* **(a) lexical** — a `my`/`state` binding in scope at the use, INCLUDING
  declarations outside the block (file lexicals), **and an `our` alias in
  scope** — whose home is the *declaring* package, never X and never the
  enclosing package.  Left alone (or requalified to its declaring package
  if the emission would otherwise mis-home it).
* **(b) magic/special** — `@_`, `$_`, `$1`…, punctuation vars, and the
  always-main set the emitter already owns (`ENV`/`INC`/`ARGV`/`SIG`/STD*).
  Left alone.
* **(c) already qualified** — left alone.
* **(d) none of the above** → a global of X → requalify to `X::name`,
  sigil-family-aware.

The `our` half of (a) is not a theoretical nicety — my probe found it live
(§9.1): `our $x; { package Bar; $x = "X" }` — perl writes `main::x`
(the alias holds across the package switch), PCL today writes `Bar::x`.
That is the same family diverging in the OPPOSITE direction in the
already-working bare-block path.  The #239 fix must get this right inside
its blocks, its verification must probe the file-level sibling, and if the
sibling shares the bug it gets FILED as a companion, not silently absorbed
into #239's scope.

**DIE on unclassifiable: YES.**  A name the resolver cannot classify would
otherwise receive a silently-wrong package home — a value the program then
consumes, which is rule 12's DIE side, and the same self-inconsistency
status as #274's anchor-miss.  The die can only fire where the trigger
fires, so its blast radius is inside the currently-broken population — but
it is still a decline→die edit, so the s372 Ask-2 bar applies in full:
both-population die-scan + corpus-diff + sweep TOTAL/LOST.

**Scope: variable symbols only.**  Bareword CALL resolution inside the
region is a separate axis.  The verification must include one probe of a
sub called inside the region (the Sort::Versions `versions()` shape) to
confirm variables-only clears the population; if it does not, that is a new
ask, not silent scope growth.  Symbolic refs (`$$name`) and `local` stay
untouched — `local` operates on the requalified global, exactly as the
`our` trigger already handles (probe-verified there).

## 8. Ask 8 — CONFIRMED: a `lib/` change makes the sweep non-optional

The extension is right, and the reasoning generalizes cleanly: the
s374/s375 optionality rule holds only when every changed file is visible to
one of its attestation legs.  A changed `lib/` shim is visible to none —
corpus-diff's corpus is `perl-tests/*.t` and never transpiles a shim; the
lib-reach byte-compare differs *by design* when the shim's source changed
(it attests reach, not correctness); and the Pl/t gate exercises only a few
shims incidentally.  So: **a session that changes a file under `lib/` runs
the full sweep (plus the owning dist's files); the second-sweep optionality
never applies to it.**  Same logic already covers `cl/` runtime changes —
corpus-diff is emission-only — which is why the gate+sweep habit exists
there; the DECIDED line says both.

Opus's decision to run both the sweep and the whole board this session was
correct, not over-caution.

## 9. Reviewer findings (none blocks approval)

* **9.1 — live `our`-alias mis-homing in the bare-block path** (found by
  the §7 probe): `our $x; { package Bar; $x = "X" }; print $main::x` —
  perl prints X (`our` aliases `main::x` for the rest of the scope), PCL
  leaves `main::x` UNDEF and writes `Bar::x`.  Pre-existing, opposite
  direction from #239's bug, same family.  Recorded on task #239 as a
  mandatory guard probe + potential companion filing (see §7).
* **9.2 — `looks_like_number` residue**: real perl accepts `nanq`/`nans`
  (quiet/signalling NaN spellings); the shim's `NaN` alternative does not.
  One character class.  Added to #238's checklist — it is a `lib/` change,
  so it rides the next #238 batch rather than shipping alone (§8).
* **9.3 — uniqnum row classification** (§6) recorded on #238 so the next
  session sizes the hybrid from data, not from the ask's guess.
* **9.4 — style nit, no action**: `%test-import`'s `loop while vals for a =
  (pop vals)` puts a variable clause after a termination clause; SBCL
  accepts it (gate is green), but `loop for a = (pop vals) while t` — or a
  plain `do`/`when` body — is the conventional order if the function is
  ever touched again.

## 10. Queue after this review

Unchanged from the USER's s376 rulings: **#239 next** (Opus, with §7's
contract), then **#237**, then the v0.1 track (#277–#283); `subname` is
sized first as #284; the board baseline re-bless waits for the #208
per-file audit after #239 moves its files.  #285/#286 are fillers on cause
lines.  #153 FOLD chunks 2–3 remain mine.
