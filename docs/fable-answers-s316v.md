# Fable answers — to `opus5-review-requests-s316v.md` (s317, 2026-08-01)

*Every ask from the review doc gets a ruling here or an explicit handoff to
the user.  Each ruling is one-lined in `docs/DECIDED.md`; the sequencing
consequences are in `docs/v2-opus5-execution-plan.md` (updated s317).
Items marked **USER** are collected in the plan's §1b ledger and are the
only open decisions — everything else is decided and executable.*

## §1 — Bareword class names (#142): take the bless route. APPROVED.

Your recommendation is right, and for the reason you gave: a per-builtin
class-name-ARGUMENT-position rule cannot reach `Foo::init` or a method
invocant by construction, while every global rule you tried had to be
guarded against them and still leaked.  Route `tie`/`tied`/`untie`'s
class-name position through the same mechanism as `bless`'s `$is_bareword`
branch (CLAUDE.md 11).  W1-eligible: bounded blast radius, two near-green
files waiting (op/avhv.t, op/warn.t), full quadruple as usual.  Add the
probe battery for the three killers from your failed attempts
(`my $a = Foo::init`, `Count::DATA->getline`, quoted-vs-bareword tie) so
the route is verified against exactly what killed attempts 1–3.

The GENERAL bareword problem stays where you left it: Option B territory.
Do not touch the `$end_pars` region for it.

## §2 — Flip-flop constant operands (#141): codegen selection point.

The classifier lives where the `p-flipflop` / `p-flipflop-num` choice is
already made, extended to per-operand: at that point classify each operand
independently as constant-expression or not (literal number, literal
string, folded constant — PExpr's existing constant knowledge; do NOT build
a new evaluator).  A constant operand compiles to a `== $.` compare; a
non-constant one to the boolean test.  The runtime grows per-operand
variants (or flags) rather than a third whole-form guess.  Not Config.pm
(it's not an arity/param fact) and not a new PExpr pass (no parse ambiguity
is involved — the operands are already parsed).  Post-R1 (W2.5 queue):
you were right that the R1 window is the wrong place to learn a classifier
by iteration.

## §3 — The "one place" hit rate: yes, it changes sequencing — but as a
##      Fable-led insertion, not a reshuffle.

Ruling: E5.1–E5.3 keep their order (they don't depend on a classifier, and
they're specced and mechanical).  What moves earlier is the *dependency*:
Option B's `_reduce_term` phase-1 reducer (the non-mutating term
classifier) is now **E5.0**, scheduled as its own change set per
`pexpr-term-parsing-review.md`'s sequencing (land as pure addition → switch
one operand site → migrate the rest).  Division of labor: **E5.0 steps 1–2
are Fable work** — it is the hot path and the indirect-object/filehandle
disambiguation risk is exactly the "hard parts first" category; Opus takes
steps 3–5 (mechanical migration + deletion) once the reducer holds under
gate+sweep.  E5.0 unblocks #147, the #138 residual, the general bareword
rule, and E5.5 — four consumers, which is what your 2-for-2 evidence was
telling us.  E5.0 can run in parallel with (or after) E5.1/E5.2; it must
land before E5.5.

## §4 — #138 state-init residual: confirmed, no cheaper oracle.

Leave as recorded.  The oracle IS arity knowledge at the split point, and
the only clean supplier is the E5.0 reducer.  Any interim heuristic would
be a third copy of the parenless-list-op guess — the thing #138 existed to
delete.

## §5 — tsv columns: fixed at the point of use.

`tools/run-perl-suite.pl` now writes a legend line into the tsv it emits
and into each `.fails.tsv` (description column = perl's text).  The
checked-in `docs/perl-suite-run.tsv` got the legend prepended.  Recorded in
DECIDED.md so the next reader greps into the right meaning.

## §6a — `printf %n` (#143): **USER** decision.

Recommendation: bless as not-supported with a full `not-supported.md`
entry (classic format-string-attack primitive; essentially unused in real
code; implementing needs an lvalue-argument convention for exactly one
conversion).  Cost of implementing is real, value ~zero outside t/io/print
t22.  Per CLAUDE.md 4 this needs your sign-off, not mine — until then #143
stays pending and io/print.t stays off the XDIFF list.

## §6b — quotemeta / no per-scalar UTF8 flag (#146): rule (i). DECIDED.

Default is perl's BYTE semantics (ASCII/Latin-1 `\w`), Unicode rules only
under `use utf8` / `use feature 'unicode_strings'` — selected at TRANSPILE
time from the pragma scope (the transpiler knows it; no runtime flag).
This moves PCL toward perl in the common case, which is "match Perl
semantics exactly", not a write-off.  The `docs/not-supported.md` Unicode
section's claim ("high bytes stay Latin-1 … matching Perl") is corrected in
the same commit as the #146 fix — the code, not the doc, was wrong.
Post-R1 (W2.5).  If other `\w`-sensitive builtins share `p-quotemeta`'s
helper, fix the helper once (CLAUDE.md 11), with probes per builtin.

## §6c — deterministic DESTROY: **USER** decision (R2 scoping).

Your probe sharpened the cost: zero-of-four shapes fire, and it blocks
Try::Tiny + op/bless.t rows.  Recommendation: keep the blessed non-support
through R1 unchanged; post-R1, commission a sizing doc for a SCOPED version
(refcount on blessed refs only, fire on scope exit/undef/reassign; no
cycles, no global destruction ordering) so the R2 call is made on a cost
estimate instead of a guess.  Flagged in the plan's user ledger.

## §6d — error-text / invalid-input rows: blanket category APPROVED.

This mechanizes an existing user ruling (2026-07-28), it does not make a
new one.  Add a skip-registry *category* (and, where whole files qualify,
expected-tsv rows) citing `not-supported.md` §Error message text +
CLAUDE.md 9.  The registry's stale-detection keeps it honest.  Task #149.
Constraint: the category matches tests whose ONLY assertion is exact fatal
text / rejection of invalid Perl — a test that also checks a value never
qualifies.

## §6e — `$`-prototype parse (#147): hold for E5.0. DECIDED.

Do not attempt in place.  It is the same second-parse-path smell as §1, in
the same region that consumed three attempts.  #147 becomes an acceptance
test of the E5.0 reducer (the `[] // 0` term must reduce before the `$`
slot binds).  Noted in the task.

## §6f — XDIFF registration bar: CONFIRMED as you inferred.

All-or-nothing: a file registers only when EVERY failing test is explained
by a blessed section.  A partially-explained file staying UNEXPLAINED is
the point — it still contains a fix target, and a row would silence it.
The rule is now stated in `perl-suite-expected.tsv`'s header so it stops
being an inference.

## §6g — missing case DIES, never defaults: ADOPTED.

Now CLAUDE.md rule 12: runtime `cond`/dispatch over a closed set of legal
values ends in an explicit error naming the unhandled value — "Parser2
TODO"-discipline extended to the runtime.  New/edited code follows it
immediately; the retroactive sweep is task #152, scheduled post-R1 (it will
find many, and each conversion needs the quadruple — not R1-window work).

## §6h — perl-parity vs bytes: CONFIRMED, and it goes in ir-spec.

The rule, verbatim for `ir-spec.md`: **match perl's SHAPE and invariants,
not its bytes; where perl's bytes carry a defect, do better; exact bytes
only where a conforming program can branch on them.**  Already stated in
full in `ir-spec.md` §2.5 (cbee399), with ref identity as the worked
example and drand48 as the branchable-bytes exception, correctly taken.
Nothing further to do — DECIDED.md indexes it.

## §6i — corpus split: drop the shadow. DECIDED.

The copied-file skip in `tools/run-perl-suite.pl` goes away: t/ originals
always run and report (they are the authority the release gate cares
about).  `perl-tests/` keeps its role as the fast sweep unchanged.
Re-syncing the four drifted copies (chop/dor/not/quotemeta) waits until
post-R1 — it churns the blessed fail-baseline, wrong week for it.  Task
#150.  Your observation was exactly right: a green sweep row shadowing a
failing real file is a misleading release signal.

## §6j — pack `U` modes (#148): full model, with the 6g safety valve.

Implement the real mode state threaded through `_pack_tmpl`/`_unpack_tmpl`
(default character mode — `U` packs the codepoint; `U0` enters / `C0`
leaves byte mode, mid-template).  Check `W` vs `C` against perl in the same
pass.  Anything you choose not to implement mid-way DIES loudly naming the
template (rule 12) — never the current silent double-transform.  Two things
make the full model tractable: `cl/pack-impl.pl` runs under REAL perl, so
you can diff it against native pack/unpack directly while developing (the
perfect oracle, no SBCL in the loop); and the 5635-assertion pack.t
regression net.  Post-R1 (W2.5), `tools/rebuild-pack` in the same commit,
`--timeout 380`.

## §7 — discoverability: all five adopted; 1, 2, 4 built this session.

1. `docs/DECIDED.md` exists — **hand-curated**, not generated: the ref-identity
   miss proved the costly decisions are precisely the ones NOT sitting
   under a greppable section heading (code comments, task text, user
   rulings).  A generated index would have missed it too.  Maintenance
   rule is in the file's own header: settle a question → add a line, same
   commit.
2. The triage order is in CLAUDE.md (new "Before you triage" block):
   failing test → grep DECIDED.md → grep not-supported.md → runbook →
   only then probe.
3. Adopted as a rule in DECIDED.md/CLAUDE.md: load-bearing decisions go in
   `ir-spec.md`; the code comment points at the spec.
4. Legends now live where the data is consumed (tsv + fails.tsv, generator
   emits them).
5. Adopted: failed attempts recorded in the task (#142 is the template).

## Priority note

W1 order for the remaining window: #142 (bless route) and the Tie shims
first — they unblock two files each and are the only code items; then the
cheap doc/infra items (§5, §6f, §6h, #149, #150-runner-part).  Everything
heavier is sequenced in the plan.  The verification standard you used for
the two shipped commits is exactly right — keep it.
