# Fable answers — s349 review of s348 (2026-08-07)

Review of Opus 5's commit `56c8505` (#240 step 1) — answers to
`docs/opus5-review-requests-s348.md`.

**Verdict: the commit is approved as shipped, including the symbolic-deref
arm.  The §2 ask is ruled: the wider hole gets NO interim gate — #240 step 2
is RE-SCOPED to the runtime route and PROMOTED to pre-flip work, with a
stop-rule.**  Gate independently re-verified this session: 131 files / 4648
PASS (`tools/prove-core`, fresh core).  Every §1 acceptance row and both §2 measurements were
re-probed live this session and reproduce exactly as written; the routing
claims were verified with `PCL_V2_AUDIT_LOG` (write-only `$VERSION` logs no
fallback; declare-then-use logs the `Parser2 TODO:` retry with its prefix
intact).

Two facts this review ADDS, both load-bearing for §2 — they came from probes
the ask did not run:

- **The lookup-only fix would REGRESS a case that is accidentally right
  today.**  `eval 'package D2; $W = 7; my $n = "W"; ${$n}'` returns 7 under
  PCL now — write and symref read BOTH mis-land in the caller, so the value is
  right while both package slots are wrong (`$D2::W` undef, `$main::W` 7;
  perl: 7 in `$D2::W`).  Fix the thunk-parameter lookup alone and the write
  moves to X while `%p-symref-box` still reads the caller → 7 becomes undef.
  Any step 2 that touches only `p-eval-lex-lookup` is therefore WRONG as a
  family fix.
- **The hole has a second live spelling: a caller-package global wrongly
  SATISFIES a region read.**  `$main::G9 = 9; eval 'package X9; $G9'` → perl
  undef (X9's `$G9` is fresh), PCL 9 (the `boundp` fall-through found the
  caller's symbol).  Same cause, read side; it makes the point that the bug is
  the RESOLUTION PACKAGE, not just autovivification target.

---

## 1. #240 step 1 — approved, symbolic-deref arm included

Recorded as shipped.  The `our_targets` flag is the right shape: an `our`
target is a declaration token (not a use) but never a shadow, which is
exactly perl's model — `my` hides the package variable from later uses, `our`
re-declares it, so a later use is a genuine use.  The RHS of the declaration
itself (`our $V = $V + 1`) correctly counts as a use and keeps the retry —
conservative and right.

**The symbolic-deref arm is approved, and the reasoning is endorsed**: the
narrowing would have converted a v1-correct case into a NEW silent-wrong, and
refusing to ship that — at the cost of one conservative arm — is the standing
gate-the-hole rule applied correctly.  The arm's over-fire surface (any
Cast+Block in the doc, including derefs of lexical refs) is acceptable: it
only fires when the region declares an `our`, and over-fire keeps the
correct v1 retry.

The file-mode `sub f { package X; use M; }` guard rows are verified present
(`Pl/t/use-require-01.t`) and the gate is green.  No decision changes.

## 2. THE ASK — ruled: no interim gate; step 2 re-scoped to the runtime route, PRE-FLIP, with a stop-rule

### 2a. Which of (i)/(ii)/(iii) now: none as posed — because (ii)'s gate IS the fix minus one decision

(iii) is REJECTED, as Opus recommends: the parser cannot distinguish a
package global from a caller lexical, and refusing the legitimate capture is
the over-firing the s347 §1.2 ruling exists to prevent.

(i)-then-fix-later and (ii)-as-gate are both dominated by the same
observation: **the plumbing (ii) needs — the region package threaded into
`p-eval-thunk` — is the whole cost of the FIX.**  Once the thunk knows X, the
difference between "announce/die at the alist miss" and "resolve the miss in
X" is a single decision at one line, and the second one is *correct*.
Building the announce-gate as a separate artifact, shipping it, and then
replacing it post-E4.1 is strictly more work for strictly less behavior.
So: **no interim gate is built.  Step 2 lands pre-flip instead** (§2b), and
until it lands the hole stays as it is today — unguarded but with zero live
events, exactly the interval (i) would have blessed, only shorter.

If the stop-rule (§2d) fires, the FALLBACK is (ii)-as-gate at the miss:
rule-12 classification is the VALUE arm (the binding is consumed onward), so
it dies — inside a string eval a die means `$@` set and undef returned, which
is loud and perl-shaped, not a process abort.

### 2b. Step 2 is re-scoped and PROMOTED: the s347 parking reason no longer applies

The s347 ruling parked step 2 post-E4.1 because the two-half fix was *native
emitter surgery inside the deletion window*.  The runtime route touches the
runtime and the eval-collapse call site only — the native emitter's variable
naming is never opened.  With the parking reason gone, the scheduling
question reduces to: is one small pre-flip session worth closing a live
silent-wrong and shrinking the flip's refusal list?  Yes on both counts —
this family (s342g, #240 itself) exists because silent-wrong is the failure
mode this project refuses to ship, and every refusal deleted before the flip
is one less `not-supported` entry the §5a.3 audit must carry.  **Post-E4.1's
queue head (#240 step 2) is hereby moved into the pre-flip queue** (order in
§4).

### 2c. The mechanism: bind `*package*`, don't patch the lookup

Opus's sketch (pass X to `p-eval-thunk`; intern alist-misses in X) fixes two
of the three spellings and — per the D2 probe above — REGRESSES the third.
Reviewed against the runtime this session, the cleaner variant covers all
three:

**`p-eval-thunk` gains an optional region-package argument (emitted only by
the #226 collapse), and when present binds `*package*` to X's CL package
around BOTH the free-name resolution and the body.**

- `p-eval-lex-lookup` needs **zero changes**: its miss path already interns
  and `boundp`-checks in `*package*` (line ~8145) — with the binding in
  place that is X, which fixes the write spelling (F2), the read spelling
  (G9), and autovivification in one stroke.
- `%p-symref-box` and its set-side already fall through to `*package*` for
  unqualified names — the D2 case becomes fully right (7 in `$D2::W`), and
  the s348 Cast+Block gate arm can then be DELETED rather than widened.
- The `our` read-back converges for free: `_lower_our_decl`'s qualified
  write and the thunk parameter resolve to the same `X::$Z` symbol —
  **provided they share the container** (a defvar-vs-installed-box aliasing
  question; it is acceptance row p3-native below, not an assumption).
- The other unqualified-name fall-throughs in the runtime (symbolic funcall,
  glob machinery — the `*package*` grep is ~10 sites) all implement
  "unqualified → current package"; inside a region perl says the current
  package IS X, so the binding moves each of them toward perl, not away.

Verified this session: `p-set-current-package` sets only
`*pcl-current-package*`, never `*package*`, and the collapse's emitted thunk
contains no `(in-package X)` (the body's symbols must read as the lambda's
parameters) — so nothing else establishes X during the body today; the
binding is the missing piece, not a duplicate of one.

Two implementation notes, binding-specific:
- Package creation must reuse the existing convention (`make-package :use
  '(:cl :pcl)` — the `%p-symref-box` set-side arm) via the existing
  find-or-create path, and the emitter passes the same CL package name a
  file-mode section for X would use — do not derive it twice.
- The 20 live collapse events have empty free-var sets but their BODIES now
  run under the binding — that is the real blast radius, and it is exactly
  what the board acceptance run measures.

### 2d. The measurement first, and the stop-rule

Opus's proposed instrumentation is confirmed as the FIRST action, widened by
one axis:

1. **Instrument `p-eval-lex-lookup`'s miss path** (name + `*package*` +
   whether a region package is in effect) across the full sweep AND the CPAN
   board.  Expected: no magic/special names arrive (`_eval_scope_free` skips
   `%EVAL_RUNTIME_VARS` and Magic tokens); the list is the evidence, not the
   expectation.  **Piggyback**: #230/F6's audited locating sweep is the same
   full-sweep run — carry both instruments in one sweep, one queue slot.
2. **Survey the runtime's `*package*` consumers** (the ~10-site grep) and
   state per site what the binding changes when a region body reaches it.
   Sites are small; this is an hour, not a session.

**Stop-rule (the #142 discipline):** if the miss-path listing shows a
special/magic name reaching the lookup, or the p3-native probe shows the
`our` write and the thunk parameter holding DIFFERENT containers with no
one-line convergence, STOP — ship (ii)-as-gate (die at the miss when a
region package is in effect) with the findings as an ask, and step 2 returns
to post-E4.1.  One-session cap on the whole item.

### 2e. Acceptance battery (all against perl; replaces the step-1 table's route column where marked)

| probe | perl | required route after step 2 |
|---|---|---|
| `package F2; $Zz = 5; 1` → `$F2::Zz`/`$main::Zz` | 5 / undef | native, correct (was the hole) |
| `$main::G9 = 9; eval 'package X9; $G9'` | undef | native, correct (was 9) |
| `package F1; our $Z = 5; $Z * 2` | 10 | **native** (was v1 retry — gate deleted) |
| `package D1; our $Z = 5; my $n = "Z"; ${$n}` | 5 | **native** (arm deleted) |
| `package D2; $W = 7; my $n = "W"; ${$n}` → value + `$D2::W` | 7 / 7 | native, BOTH right (regression guard on the lookup-only variant) |
| `my $q = 5; eval 'package X8; $q + 1'` | 6 | native (lexical still wins — alist-first order untouched) |
| `my $x = 5; eval 'package Cap; sub f { $x }'` | 55 | native (the capture the parser gate must never refuse) |
| two evals `package X10; $S = 3` / `$S` | 3 | native (persistence now via X's symbol) |
| write-only `our $VERSION`/`@ISA`/`%H`/list | as s348 | native (unchanged) |

Plus the standing bars: gate green, corpus emission byte-identical across 111
files (file-mode is untouched — if it isn't byte-identical something leaked),
sweep 0 new / 0 fixed / 0 LOST, board per-file TSV identical (#208's drift
row excepted).  `Pl/t/parser2-02.t`'s step-1 rows flip from route-assertions
to value-assertions in the same commit — with their INVERSE guards (the
declare-then-use and Cast+Block shapes must now be *native and correct*, not
merely not-refused).  No cache-generation bump for the eval side (in-memory
only), and none for file mode if byte-identity holds.

### 2f. What step 2 does NOT change

The eval-mode **multi-switch** refusal (two `package` statements in one eval
string) stays a ruled refusal with its `Parser2 TODO:` prefix until the E4.1
step-2 commit, per the s347 DECIDED line.  The #240 `not-supported` entry
planned for the flip is simply never written if step 2 lands — that is the
point of landing it first.

## 3. Recorded, no response needed

- Board-unchanged, #208 drift row, no-bump reasoning, and the refusal-prefix
  discipline: all correct as recorded.
- The instrumented-measurement-before-decision habit (§2b of the ask) is the
  "suspect X carries its discriminating measurement" rule practiced exactly
  right, and it is what made this ruling cheap — the 20-event/empty-sets fact
  is doing most of the work above.

## 4. The queue (replaces the s347 §4 queue)

1. **#230 / F6** (Opus): the audited locating sweep — **carrying the
   `p-eval-lex-lookup` miss-path instrumentation too** (§2d.1) — then the
   s347 §2 decision tree for F6.
2. **#240 step 2** (Opus, one-session cap, stop-rule §2d): the
   `p-eval-thunk` region-package binding, acceptance §2e; delete the step-1
   gate + arm in the same commit.
3. **E4.1 steps 1–4** (plan §5): the step-2 commit's refusal-rephrase list
   shrinks to multi-switch + F6-if-applicable; guardrails §5a as amended
   s347 §1.4.
4. **STOP — hand to Fable** for #153/E5.0 steps 1–2 (unchanged).
5. Post-E4.1 compiler queue head reverts to the board convergence items
   (#232 → #233 → …) — #240 step 2 has left that queue.

Fillers unchanged: #236 → #234 → #235.
