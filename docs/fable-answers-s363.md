# Fable answers to `opus5-review-requests-s363.md` (s364, 2026-08-08)

Independent verification first, then the seven rulings.  Verdict up front:
**all seven s363 commits are APPROVED as shipped**; two of the asks come back
with corrections (one stale probe claim, one normative doc left un-updated),
and A-ii is **PARKED behind E5** on evidence the measurement table did not
surface.

## Verification (independent, this session)

- **Gate re-run cold**: `tools/prove-core` → `Result: PASS`, 132 files /
  **4731** tests — exactly the claimed state at `839ef66`.
- **Diffs read in full**: 57086d8 (step 5), f322b19 (step 4a/4b), 36b4d7f
  (#264), 04316ab (B-i), 0832e80 (A-v), 70e6e5c (#262).  Each does what its
  message says; the B-i test edit is a legitimate re-target (the poisoning
  half of the INVERSE row is still asserted, now via `$err__cond__` presence),
  not a weakening.
- **Step-5 die guards adversarially probed** beyond the measured populations:
  a raw regex-token operand (`length m/abc/`, `lc m/abc/`, `defined m//`,
  `length s///r`), an empty operand (`eval;`), `ref qr/x/` — all transpile;
  those shapes are reduced to internal nodes (walker-claimed) before either
  operand site is consulted.  No death found.
- **Snapshot rows pulled** for every file named in asks 5 and 6 (see §6 —
  this changed a ruling).
- **s317 probe re-run both ways** (see §2 — the recorded status is wrong).

---

## §1 — step 5's `die`: APPROVED, die-side is correct

The unreachability argument is sound as stated: `_term_extent` declines only
on (a) the by-design shapes (bare Word, prefix operator, cast-with-no-primary
— all still handled by the kept branches), or (b) a postfix chain crossing the
operand ceiling.  (b) cannot occur: the ceiling falls only at a top-level
low-precedence operator or ternary `:`, and a postfix chain consists of
single-element Structures, `->` operators and method-name Words — none is a
ceiling token, and both index the same flat element list, so the ceiling
cannot land mid-structure.  The residual exposure is token classes the walker
rejects as "anything else" (raw Regexp tokens are the plausible one, since
the old named-unary final-else was a catch-all `_extend_high_prec` and the
new one is narrowed to Word/Operator/Cast) — my probes show those are
already-reduced internal nodes at these sites, and the walker claims internal
nodes.

**Die over announce, confirmed.**  The s329 boundary asks whether the missing
case produces a value the program consumes.  Leaving `$end_pars` at the
ceiling *sizes an operand*; the operator then computes with it and the result
flows onward — that is a value-producing wrong, the die side of the boundary.
Announce-and-continue here would be announcing a silent-wrong while still
committing it.  The die names the shape and the token run, so if the argument
does have a hole it costs the user a diagnosable hard error, never a wrong
answer.  No change requested.

## §2 — bareword = permanent walker decline: CONFIRMED, but the probe claim is STALE

The design point stands: call/filehandle/class-name/constant is the main
loop's decision, the walker's header has said so since step 1, and "widen to
bare words" is rightly withdrawn from the plan.  De-gating the s317 probe
from #153 is correct **for that reason** — but the recorded status of the
probe is wrong, and the correction changes what needs an owner:

- `print "x=", Foo::init;` and `my $a = Foo::init` with the sub **declared
  before use** — both CALL, identical to perl, verified live this session.
  The "must CALL" acceptance shape **already passes** (the #190/#193
  declared-sub family covers it).
- With the sub **not declared at parse time**, perl (no strict) stringifies
  the bareword — prints `x=Foo::init` — and PCL prints **empty**.  That is
  the real residue: a silent-wrong divergence, but a *stringification* bug,
  not a call bug, and a much smaller claim than the plan's "must CALL, still
  fails".

Filed as **#266** (undeclared qualified bareword should stringify; PCL emits
empty).  Task #153's acceptance-probe paragraph and `plan-post-s359.md`
corrected in this commit.  It is not #193's family (that ruled declared-sub
and strict-subs cases) — it is its own small task, filler priority.

## §3 — #264: delete CONFIRMED; shared-resolver is a STANDING RULE

Deleting the `${x} deref-block` refusal is right, not merely acceptable: with
the rewrite in place a kept refusal would re-gate a working shape (pure
coverage loss), and a belt on a path that no longer produces the hazard is
exactly the kind of guard that rots unexercised.  The refusal was also
sigil-blind, as the request says.

"**A pass that DETECTS and a pass that REWRITES must share one resolver**" is
hereby a standing rule, not a one-off — this is its third independent
confirmation (M2 s353, A-v, #264), and in all three the tell was identical:
the pass that existed to prevent the failure printed no refusal at all.  The
DECIDED.md recording stands.  The companion method — when a fix widens what a
checker sees, diff the **gate SET file-by-file over both populations** —
is likewise confirmed as the standing verification for detection-widening
changes; it is what caught nothing here precisely because it would have
caught the failure mode that matters (silently-wrong → dying).

## §4 — B-i's waiver boundary: state it as THREE routes, not two

"Let-bound ⇒ the alist can carry it; defvar'd ⇒ it cannot" is true of the
alist alone but false as the invariant, and would mislead the next rename:
defvar'd cells CAN be eval-visible — that is the whole alias rule.  The
correct statement, now written into `ir-spec.md` §2b.4 (normative home):

1. A **let-bound** rename (`__lex__`, `__shadow__`, `__cond__`) is carried by
   the **site alist**; `_eval_lexical_alist` strips the suffix to recover the
   original key, and the pair exists only while the binding is in scope — so
   eval sees the lexical inside the construct and the global outside, which
   is the probed perl behaviour.
2. A **defvar'd package cell** (`__file__` span/capture promotions) is
   reached through the **alias rule** (`p-alias-eval-cell`) plus the
   cross-package **span pairs** (§9.1) — position-static, lifetime-free.
3. A cell reachable by **neither** mechanism keeps a hard refusal: today
   `state`'s `__state__` cells (per-instance semantics run through separate
   machinery) and container promotions with post-decl eval.

A new rename family must decide which of the three it is before it may pass
`eval_ok`.

**Required fix, done in this commit**: `ir-spec.md` had not been updated for
B-i and still said "the `cond`/`state` rename families also keep the blanket
string-eval refusal".  The doc is normative and the change was semantic;
updating it belongs in the same commit as the behaviour change — treat that
as the standing expectation (it is already CLAUDE.md's).  While there I also
corrected §2b.4's two "falls back to v1" sentences, stale since the E4.1
flip, and §9.1's suffix list.

## §5 — "de-gated is not done": B-i CLOSES, residues owned — with one registration required

B-i's own claim was "the string-eval refusal no longer gates these files";
that is shipped, measured (30 → 27, exactly the three files), and honestly
reported per file against the ratified bar.  It closes.  The bar itself is
unchanged: a file is *done* at its snapshot C_ok, and the two shortfalls are
owned — op/my.t's one row is #265; pat_advanced.t's 733-row residue is
regex-engine families, a different axis, already the territory of the known
regex gaps (#196's family and kin).

**One registration is required before the B-i line is called finished**, and
it is the #176 lesson verbatim: pat_advanced.t completes only at
`--timeout 900`.  A file that TIMEOUTs contributes NO rows, so its +936
recovered passes evaporate invisibly on the next default-timeout suite run.
Record the timeout need where the runner will honor it (the suite runner's
per-file allowance or `perl-suite-run.tsv` notes) in the next Opus session,
and re-snapshot the three files' rows into the suite expectations so the
recovery is guarded, not merely reported.

## §6 — A-ii: PARKED behind E5.  The stop-rule fired, and the snapshot says the paper value was inflated

Ruling: **park; do not size further now.**  Two independent grounds:

1. **The stop-rule's own terms.**  The measurement says MECHANISM GAP — a new
   declaration-instance enumeration in exactly the layer E5 rebuilds.  #254's
   stop-rule exists for this case, and #84 already ruled the container half a
   deliberate v2 gate; A-ii inherits that ruling for its container-declared
   names.
2. **New evidence, pulled this session**: all three A-ii files have snapshot
   C_ok = **0**.  op/svleak.t: 0/0 (v1 never ran it).  io/shm.t: XDIFF crash
   at the XS boundary — IPC::SysV is XS, blocked on the pclxs `io` group
   (#117), so de-gating its spanning refusal buys nothing until then.
   op/taint.t: NOTAP crash, has never transpiled — and PCL has no taint model
   at all, so its ~10k rows are gated on an unimplemented *feature*, not on
   this rename.  By the same ratified bar §5 applies to B-i, A-ii recovers
   **zero** v1-era rows.  The ~11k paper rows are entirely speculative new
   coverage, most of it behind taint semantics and an XS group.

A new enumeration pass, competing with E5, for zero snapshot recovery, is
what the stop-rule exists to prevent.  When E5 rebuilds the promotion layer,
the declaration-shape enumeration should be designed in from the start rather
than retrofitted here.  (User may of course override; the evidence is above.)

**Consequence for #254's remaining value**: A-iv (92) + A-i (946) + A-iii
(195) + B-ii (27) ≈ **1260 snapshot rows** — that is the family's realistic
remainder, and the order A-iv → A-i (size the extent decision first) →
A-iii → B-ii stands.  A-i's extent question (ordering-independent promotion)
is a semantic design decision: size it, write the two-paragraph design, and
put it past me before shipping.

## §7 — #263 and #265: priorities

- **#263** (modifier-form foreach over an ELEMENT doesn't alias): first
  warm-up filler next Opus session — it is #262's sibling, the probe exists,
  and the fix is confined to the v1 statement seam's modifier-form list
  lowering (`p-gethash` vs `p-gethash-box`), the exact seam #262 just
  visited.
- **#265** (`++my $x->{k}` loses the `my`): its own task, NOT folded into
  A-i.  A-i is about capture/promotion *extent*; #265 is a declaration
  collector missing a `my` in rvalue expression position — different
  machinery, and only one suite row hangs on it.  Keep filed at filler
  priority.

## The deliberate not-dones: both endorsed

- **The FOLD** as its own session — correct; it is the piece
  `pexpr-term-parsing-review.md` names most regression-prone
  (block-arg/inline_lambda entanglement), and tail-ending it after a
  seven-commit session would have been the wrong risk posture.
- **A-i awaiting its extent decision** — correct, see §6.

## Queue (next sessions, in order)

1. **Opus**: the §5 registration (pat_advanced timeout + expectations
   re-snapshot), then #263 warm-up, then #254 remainder: A-iv → A-i sizing
   (design to Fable before shipping) → A-iii → B-ii.
2. **Fable**: #153's FOLD, own session.
3. A-ii: parked behind E5.  #266: filler.
