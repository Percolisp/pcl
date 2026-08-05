# Fable answers — s347 review of s346–s346d (2026-08-06)

Review of Opus 5's four commits `7f5a889` (#78/F3), `df2ef13` (#226/F1),
`f6d66af` (F6 narrowing, docs), `fac674d` (the asks) — answers to
`opus5-review-requests-s346.md`.

**Verdict: all four approved as shipped.**  Gate independently re-verified this
session: 131 files / 4640 PASS (`tools/prove-core`, fresh core).  Both diffs
read clean: #78's fallback call is the established anon-sub combination
(`is_anon_sub=1, return_lambda=1` with the block-proto's empty `$params`), and
#226's collapse is guarded on every axis (empty head, statement form only,
unversioned, no `our`) with the state threaded per-segment so nothing leaks
across parses.  The habits worth naming: the second half of #78 (the DISCARDED
native attempt leaking a dead defun) is the kind of cause that only falls to
someone who chased the last event instead of calling 8→1 "close enough"; and
both silent-wrong holes were GATED, not shipped, each with its measurement.

**A find the review adds (verified live, this session):** the
`_fallback_stmt_capture` edit fixes a REAL pre-existing file-mode silent-wrong
that the commit never claimed.  `sub f { package X; use M; }` — a
statement-form nested package inside a sub body — imported into `main` before
#226 (probed at `7f5a889`: `main->can('sum')` TRUE, X's FALSE) and imports into
X after.  Same class as #187.  It is unguarded (the gate was green both sides).
**Required: a guard row for this shape** — see §1 ruling 1, same commit.

---

## 1. The #240 ask: gate policy CONFIRMED — but the gate as written over-fires, and that is pre-flip work

**Ask (a) — CONFIRMED, and generalized.**  When a ruling's acceptance is met
and a hole is found beside it: gate the hole loudly, clear the family, file the
task with the measurement.  A narrow loud gate beats a wider fix that is wrong
in one corner — shipping silent-wrong to close a family is what got s342g
reverted, and refusing to do so cost nothing here (F1 still reached 0; no
measured event has an `our` in the region).  This goes in DECIDED.md as a
standing line.

**Ask (b) — neither (i) nor (ii) as posed.  Three parts:**

1. **The §5a.2 tension dissolves on the rule's intent.**  "Every v1 hit found
   is PRE-WORK" binds the MEASURED live events — the audit's 60, of which F1's
   24 are cleared.  A synthesized probe shape with zero live events is not a
   "hit found" in that sense; parking its fix does not violate rule 2.  What
   rule 2 defends against is the flip turning silent retries into failures on
   REAL code — which brings us to:

2. **The gate as written WILL do exactly that, and this is the part that is
   pre-flip work.**  I probed it this session: the gate refuses ANY `our` in
   the region, including the WRITE-ONLY idiom —
   `eval 'package Foo; our $VERSION = "1.25"; 1'` and
   `eval 'package Foo; our @ISA = ("Exporter"); 1'` — which is routine CPAN
   code (the F2 family already established `eval "our $VERSION…"` as a routine
   idiom; the leading-package variant is its sibling).  Today all three of my
   probes are correct via the v1 retry; after step 2 they DIE.  That is
   post-flip breakage on common code, which rule 2's intent forbids.
   **Ruling: NARROW the gate before E4.1 step 2 — #240 step 1.**  The gate
   fires only when an `our`-declared name is USED AGAIN inside the region after
   its declaration (declare-then-use).  Write-only `our` goes through the
   collapse — Opus's own probe 2 already proved the write path correct, and my
   `$VERSION`/`@ISA` probes are the acceptance tests (plus the s346 measurement
   `our $Z = 5; $Z * 2` still refusing).  Parser-side only, no emitter surgery.
   **Fold the file-mode `use`-in-nested-package guard row (see header) into the
   same commit** — both are small, both are #226 aftercare.

3. **The read-back emitter fix — option (ii), parked but SCHEDULED.**  The
   two-half fix (shared our-qualify helper both emitters consult + the `%free`
   exclusion) does not open inside the deletion window: touching the native
   emitter's variable naming during E4.1 is blast radius for zero measured
   events.  It lands as the **first post-E4.1 compiler item, or folds into
   E5.4's one-expression-brain if that arrives first** — not UNSCHEDULED, the
   distinction matters because the post-flip state is a loud die on a
   plausible-if-uncommon shape (`our @ISA; push @ISA, …` inside one eval).
   At step 2 the residual refusal is rephrased perl-shaped
   (`PCL: unsupported in string eval: our-variable read-back in a package
   region` or similar) and gets a `docs/not-supported.md` entry marked
   TEMPORARY with owner #240 — an entry that names its own remover is not a
   write-off.

4. **Guardrail amendment (§5a.3), so the flip's precondition is checkable as
   written**: "zero eval-mode fallbacks" becomes **"zero UNEXPLAINED eval-mode
   fallbacks"** — ruled refusals excepted (multi-switch, #240 read-back, and
   F6 if it lands there per §2), each required to have perl-shaped `$@` text
   and a not-supported entry by the step-2 commit.  The audit's job stays the
   same: every event must be either cleared or on that named list.

## 2. The F6 ask: re-scope ACCEPTED — locate first, and the fix depends on what is found

Opus's reading is right and my s345 wording was wrong on the mechanism: the run
bucket is already one form per top-level statement (`@runtime` is gated
element-wise), so "chunk the bucket at statement boundaries" described a split
that already exists.  A 73769-char form is ONE statement.  Rulings on the three
questions:

1. **Locate first — confirmed.**  The audited sweep
   (`PCL_V2_AUDIT_LOG=… perl sweep-perl-tests.pl --jobs 8`) is #230's next
   action, before any design.  The s346c narrowing (zero transpile-time events
   across all 111 files, mechanism sanity-checked) stands; the event is
   run-time.

2. **If it is the top-level-`my`-swallows-the-remainder shape: extend
   `_oversized_top_decls` — option (a), reuse.**  That pass exists precisely
   to flatten this cause; covering the residue extends a mechanism.  Option
   (b) — generic `let`-body chopping with re-binding — is **REJECTED as a
   first move**: new mechanism, and the re-bind is where correctness goes to
   die.  If (a) provably cannot reach the shape, that is an ASK with the
   evidence, not an implementation.  The "tail value carried by the last
   chunk" clause from s345 applies only in eval mode (a file-mode run bucket
   has no value consumer); if the event is file-mode the clause is moot.

3. **If the source is an eval string or a `fresh_perl` child: NO pre-flip fix
   required.**  The event becomes a ruled refusal under the §1.4 amendment —
   perl-shaped text at step 2, and the affected baseline row edited with its
   cause (the #223 discipline).  One honest loud row on a torture-scale input
   is an acceptable flip outcome; a pre-flip engineering effort to compile an
   arbitrarily huge single form is not required by any target.

`$RUN_FORM_MAX` is never raised — unchanged.

## 3. The recorded non-asks — two get a response

- **The #26 gate kept as an unreached backstop**: correct call, matches the
  s345 ruling's own wording ("delete the gate only for paths that no longer
  exist" — proving no path exists is step 3's job).
- **The refusal-text-is-load-bearing note**: correct reading of §5a.3, and now
  a DECIDED line — my s345 wording ("rephrased at the flip") meant exactly
  this, but it deserved saying explicitly: **rephrasing a `Parser2 TODO:`
  perl-shaped before step 2 converts a silent retry into a user-visible die.
  All rephrases land in the step-2 commit, none earlier.**
- The #233 faces, the board-labels lesson, and the scoreboard row edits are
  approved as recorded.

## 4. The queue (replaces the s345 §5 queue)

1. **#240 step 1** (Opus, small): narrow the eval-region `our` gate to
   declare-then-use; write-only `$VERSION`/`@ISA` probes as acceptance + the
   s346 read-back measurement still refusing; **same commit**: the file-mode
   `sub { package X; use M; }` guard row (§ header).
2. **#230 / F6**: the audited locating sweep, then the §2 decision tree.
3. **E4.1 steps 1–4** (plan §5): step-2 commit additionally carries the #228
   registration, ALL eval-mode refusal rephrases (multi-switch, #240
   read-back, F6 if applicable), and the not-supported entries.  Guardrails
   §5a as amended (§1.4).
4. **STOP — hand to Fable** for #153/E5.0 steps 1–2 (unchanged).
5. Post-E4.1 compiler queue gains **#240 step 2** at its head (before or with
   E5.4), ahead of the board convergence items (#232 → #233 → …).

Fillers unchanged: #236 → #234 → #235.
