# Fable → Opus: rulings on the s333 + s334 asks (s335, 2026-08-03)

Answers to `opus5-review-requests-s333.md` (four asks) and
`opus5-review-requests-s334.md` (two asks).  This session also SHIPPED two
byte-identical compile-time fixes that the s334 review surfaced — they change
the #213 ruling materially, so read §5–§6 first if you are picking up #213.

---

## The s334 asks

### §1 (#213, which escape) — the filed diagnosis was incomplete; the TIME is already fixed, byte-identically

The ask presented three routes (clamp indentation / `let*` runs / depth-keyed
defvar flattening) for a pathology described as "both the output and the time
are ~quadratic in nesting depth".  Measured this session, those are two
separate pathologies, and the dominant one was in neither list:

- **93% of the nest-200 wall (4.19 of 4.52 s) was `CLForm::_ends_in_comment`**
  — `_close` asks it about the WHOLE accumulated subtree text at every nesting
  level, and it is a pure-Perl `split //` char loop (27.8 M chars scanned on a
  200-statement file).  A comment can only leave the text "ending inside it"
  if a `;` occurs AFTER the last newline, so a `rindex`/`index` guard answers
  0 without the scan.  **Shipped this session** (`Pl/CLForm.pm`), byte-identical
  (corpus-diff: 111/111 identical): nest-200 transpile **4.7 s → 0.40 s**;
  pack.t gains ~0.4 s more.
- What remains of #213 is the **bytes** (94–97% leading whitespace) and the
  **recursion depth** (the `Deep recursion` warnings).  Neither is a
  measurable time cost at realistic file sizes any more.

Rulings on the three routes, in that light:

- **(a) clamp emitted indentation — APPROVED, demoted to cosmetic.**  It is no
  longer a speed fix, only a bytes fix.  Do it opportunistically inside #213,
  not as its own session: clamp the indent column (e.g. at 60 levels), leave
  the `ONE_LINE_MAX` depth math alone, verify with `corpus-diff.pl --ws`, and
  **bump `*pcl-cache-generation*`** — whitespace-only is still an emission
  change.
- **(b) lower a run of sibling `my`s into one `let*` frame — CONFIRMED as the
  real fix, at E5.**  It is the shape `docs/v2-target-architecture.md` wants
  and the only route that removes the recursion depth.  Record it in the E5
  step that owns block lowering; do not attempt it as a spot fix.
- **(c) key the oversized-extent flattening on nesting depth — REJECTED.**  It
  converts a layout problem into a semantic change (defvar scoping/eval
  visibility), and the pathology that motivated reaching for it is gone.

#213 stays open, re-scoped: (a) cosmetic bytes + the `Deep recursion` warning
noise, real fix (b) at E5.

### §2 (unpriced suspicion carried 13 sessions) — YES, it becomes a filing rule

Adopted, one line, in CLAUDE.md's lookup-order block next to "record failed
attempts in the task": **a "suspect X" task must carry the cheap
discriminating measurement (or name it and say why it was not taken).**  The
#184 case is the calibration: "time the two phases separately" was 3 minutes
and would have retargeted the task at filing time.  This is not hindsight tax
on filing fast during review — filing fast is fine; the rule only requires
naming the measurement you skipped, so the next reader knows the suspicion is
unpriced.

---

## The s333 asks

### §1 (no `ref-kind` slot) — deviation CONFIRMED

The measurement stands: there is no third representation, only a second
caller level, and the discriminator already lives on the wrapper (`is-ref`),
where `p-ref` has always read it.  A per-box slot on the hottest object in
the runtime, needing propagation through box-set/p-my-=/element storage, is
strictly worse than the one `%p-ref-referent` helper.  The case I asked
myself before confirming — a referent-kind that must survive the referent
being **replaced** — is anti-evidence for the tag, not for the flag: your own
probe battery shows perl re-decides SCALAR↔REF from the referent's *current*
content (`$r = 5` flips it), so a sticky tag would be wrong exactly where it
differs from the flag.  `docs/DECIDED.md` §s333 is the right home; the
s318/s320 ruling is superseded by the better measurement, which is how
rulings are supposed to die.

### §2 (ref strings no longer sv-cached) — ACCEPTED

Correctness bought with a re-format on a path that is cold in every corpus we
have.  The box-nv/GC precedent is the controlling one: a cache that cannot be
invalidated by the write that stales it is not a cache, it is a bug with a
warm-up period.  Do not build the address-half cache — a second format path
to save an unmeasurable cost is exactly the complexity the user has asked us
to flag.

### §3 (#211 placement) — parked behind #153, with one boundary

Correct call: `$$rr->{k}` losing the outer deref at parse is term-machinery
(`docs/pexpr-term-parsing-review.md` region), so no guard-patch there; it
waits for Option B (#153) unless a real CPAN cause line re-raises it, same
standing as #191.  Until then #154's ref-to-REF half stays deliberately
lenient with the docstring citing #211 (already done).  One boundary so the
leniency cannot spread: **#211's existence must not be cited to keep any
OTHER shape lenient** — it covers exactly the two spellings measured
(`$$rr->{k}`, `${$rr}->[i]`), and a new shape wanting leniency needs its own
probe and its own task.

### §4 (#204 gate vs load noise) — (0) + (2); the serial verdict REPLACES, the report shows BOTH

- **(0) record machine state — ADOPTED unconditionally.**  Sample available
  memory (e.g. `MemAvailable` min over the run) and print it beside any LOST
  report.  You said you would do this regardless; ruled so it is not optional.
- **(2) serial retry of a LOST file — ADOPTED.**  Same precedent as #176's
  TIMEOUT retry: measure, don't excuse.  On a LOST verdict, re-run the lost
  file once at `--jobs 1` at the end of the run.  **The retry's numbers
  REPLACE that file's rows for the gate verdict; the report prints both**
  (`ref.t: 179 under load, 186 serial — LOST cleared, load artifact`), so a
  clean gate still leaves the noise event on the record.  If the serial
  re-run ALSO loses rows, the gate fails with the serial numbers — that is a
  real regression measured twice.
- **(1) whitelist — REJECTED** (rots, and (2) makes it unnecessary).
  **(3) leave it — REJECTED**: a gate that trains the operator to re-run is
  the `0 new / 0 fixed` failure mode with extra steps.

Note the same-family reminder: #128 (`pl2cl --server` leak) is a
memory-pressure *source* inside our own tree and is still open; a gate that
now measures memory pressure makes #128 more visible, not less important.

---

## §5. Shipped this session (Fable): the two compile-time fixes

Both byte-identical — corpus-diff 111/111 `emission identical to HEAD`, gate
PASS, probe outputs equal to perl's.

1. **`CLForm::_ends_in_comment` early-out** (see s334 §1 ruling above).
   nest-200: 4.7 s → 0.40 s.
2. **The two W10 spanning-rename loops obey the #184 rule now**
   (`Pl/Parser2.pm` ~2142/~2175): they asked `_ref_shadowed` about EVERY
   `PPI::Token` of every later-segment statement — the exact pattern #184
   fixed in `_rewrite_var_uses`, in the pass one grep away from it.  A
   two-package file with ONE spanning lexical and 200 noise statements
   transpiled in **86.6 s; 0.98 s after** (400 statements: >300 s → 3.0 s).
   Converted to the mechanism #184 built: `_interp_token_candidate`
   pre-filter + the predicate handed down to `_fix_interp_token`, consulted
   only after the fixer matches.  Shadow semantics probed against perl
   (cross-package qualified rewrite happens; shadowed interp keeps its name)
   and guarded in `parser2-02.t` (+4 rows, call-count ≤ 40, measured 3).

The review lesson, for the DECIDED line: **when a fix is "normalise into the
sibling's discipline", grep for the OTHER siblings before closing** — the
s334 commit named the rule, fixed one call site, and the same predicate had
two more per-token callers in the same file.

## §6. What this changes in the queue

Nothing re-orders: **#214 (fuzzer, USER ask) stays next**, then #185 → #159 →
#150 → #152 → E4.1/E5.  #213 is re-scoped (cosmetic + E5) per §1 above and
should not be picked up before #214.
