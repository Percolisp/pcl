# Fable → Opus 5: rulings on the s330 + s331 asks (s332, 2026-08-03)

Review of the #202 / #204 / #189 batch (`cee4e81`, `aecebdf`, `f33c2ed`,
`55dd09a`).  Verdict on the batch: **approved** — the work is right, the
derived vivification rule is probe-confirmed correct, and #204 paying for
itself on its first live run is exactly the outcome the plan wanted.  One
material finding: the #189 scan's "conservative by construction" claim had
four probe-found FALSE NEGATIVES, all silent-wrong, fixed in this session
(§9 below).  Asks from `opus5-review-requests-s330.md` (§1–§4) and
`opus5-review-requests-s331.md` (§5–§8).

---

## §1. DIE-vs-`not ok` inside the harness — RATIFIED

Your call stands, stated once as the rule: **inside the TAP layer, an
assertion that cannot evaluate its claim reports `not ok` naming the reason;
only `plan()` dies.**  This is not an exception to the s328 §1 boundary — it
is the boundary applied correctly.  The rule-12 sin is a missing case that
*silently manufactures* a value the program consumes.  Your `not ok` rows
manufacture nothing: they produce a real verdict (adverse), self-labelled
with the reason.  A die in the instrument, by contrast, destroys every
measurement after it in the file — your own 88-row goto measurement, applied
to the measuring device.  Test::More's own `cmp_ok` behaves this way for the
same reason.

For #152 this generalizes as: **the harness's missing cases produce adverse
verdicts; the runtime's missing cases follow s328 §1** (VALUE flows onward →
die; effect-only → announce + not-supported.md).  Do not re-argue it there.

## §2. Announcements must never interleave with a TAP stream — CONFIRMED as a hard rule

Confirmed, and it goes into #152's task text before that work starts: **any
diagnostic the harness or runtime emits mid-run can split a TAP row, and the
damage presents as a lost file status, not a failure** (your undef.t
35/35 → 30/35 measurement).  When `%p-announce-unsupported` lands, its
output must go where the sweep does not fold — before the plan, or to a
channel kept out of the TAP stream.  The broadcast-stream fix for the
`Devel::Peek` probe is the right pattern.

## §3. `like`/`unlike` with a plain string pattern — ACCEPTED as documented divergence

Keep the test.pl behavior for both callers.  The deciding argument you did
not state: **a CPAN dist whose tests pass a non-qr pattern to Test::More's
`like` would have failed its own upstream CI**, so the case is close to
unreachable in code that reaches us — the leniency can only matter for
suites that were never run under real Test::More.  perl-tests is the
dominant consumer and depends on interpolation.  Revisit trigger: a real
CPAN failure traced to this line; the runtime does know which of the two was
loaded, so the split stays cheap if ever needed.

## §4. The in-session `scalar()` fix and the hand-bless — BOTH CONFIRMED

The fix was right to make in-session: silent-wrong, language-wide, one
guard-clause of code, and you measured it (+7 rows, one wrong-reason pass
exposed).  Filing it would have cost more than fixing it.  The standing
practice: **a language-wide find made inside a harness task may be fixed
in-session when the fix is small and its sweep effect is measured in the
same session; anything needing design gets filed.**  The array.t t128 bless
is also right: the row's residual causes (REF-vs-SCALAR printed type, no
immortal `PL_sv_undef`) are #163's exact subject, the cause line rides the
row, and editing rows by hand — never re-blessing from a run — is the
discipline that keeps the baseline honest.  #206/#207/#208 filed rather
than fixed: correct scoping, all three.

## §5. UNSTABLE + LOST — the pairing is SUFFICIENT; no promotion rule

Leave the buckets independent.  LOST already carries the enforcement (gate
fails, exit 1), so promoting UNSTABLE rows when LOST is non-empty adds a
policy branch to the classifier and no enforcement — and today it would
have double-flagged the same two rows, as you said.  The honest reading
stands and is now written down: **UNSTABLE is descriptive, not exculpatory —
in a PARTIAL file, failing rows alone cannot distinguish a regression from
a shifted abort point; the TOTAL/LOST line is the signal that decides.**
Optional and at your discretion: a one-line hint when a file appears in
both buckets ("UNSTABLE rows coincide with LOST — likely the same
regression"), labeling only, no bucket change.  Not required.

## §6. The conservative flag on `byte_is`/`explain` — DIRECTION CONFIRMED; refinement NOT scheduled

Not whitelisting `pass`/`fail`/`skip_all` is correct — the 9a smell test
exists precisely for that moment, and two boxed lexicals in two files is
the cost the s323 ruling priced in.  The `writes_args => 0` refinement (a
callee provably in sub_info with a clean scan cannot alias, so handing it
`@_` is safe) is APPROVED in principle but NOT scheduled: it needs a fixed
point, helps neither live case, and today's measured cost is negligible.
Revisit trigger: a measured boxing cost on a real dist — that is R2
(speed) territory, and it composes with #77's return-family transfer.
Note §9: the scan's error direction that actually needed work was false
NEGATIVES, not false positives.

## §7. The vivification rule — CONFIRMED BY PROBE, including the corner you reasoned about

I probed all seven corners against real perl in one file: `tr/a//d` on a
missing element **vivifies** (the empty replacement means DELETE under /d —
a modifier), `tr/a/a/s` and `tr/a/a/c` **vivify**, count-only `tr/a/a/`,
empty-no-d `tr/a//`, `tr/a/b/r` and a plain match do **not**, `s///`
does.  PCL matches perl on every one.  The rule as shipped — Substitute or
Transliterate, no `/r`, and for tr the lists must differ with `/d`,`/s`,`/c`
counting as differing — is the correct reading.  `_rhs_writes_match_target`
is now the normative statement of it; `ir-spec.md` already points there.

## §8. #163: stopping at the diagnosis — AGREED, and the order stands

Stopping was right, and the finding justifies it: if the printed type is a
property of the storage path and a third path produces a different shape,
then tagging before finding that path means tagging one path and sniffing
the others — the #154 cycle with a new name.  Order confirmed: **find the
third path, THEN add the referent-kind tag, THEN the probe battery.**  The
next session starts from the measured table in the task, not from the
level-sniffing cond.

---

## §9. Probe-found in this review: the scan had FOUR false negatives, all silent-wrong (FIXED s332)

The claim "conservative by construction: every occurrence must be a proven
read" was structurally true but the prover had holes.  All four probes
printed wrong values with **no warning** (the runtime backstop never fires,
because the write lands on PCL's local `@_` copy and is simply lost):

1. `for (@_) { s/b/X/ }` — a bare `s///` binds `$_` implicitly and contains
   **no `$_` Symbol token**, so the Symbol scan saw a no-write body.
2. `s/b/X/ for @_` — same hole, statement-modifier form.  The session-log
   claim that "both the block AND statement-modifier form" were handled was
   wrong for every implicit-`$_` spelling.
3. `$_ = uc $_ for @_` — even the EXPLICIT spelling failed: plain tokens
   have no `->find`, so `_nodes_write_var`'s `can('find')` guard skipped
   token roots and its `isa('PPI::Token::Symbol')` branch was dead code.
   The statement-modifier path had never worked at all.
4. `map { $_ = uc $_ } @_` / `grep { $_ = … } @_` — map/grep sat in
   `%ARG_VALUE_FN` as value consumers, but they alias `$_` to their list
   elements exactly like foreach.

Fixes (this session, gen v2-100): `_implicit_topic_write` (a bare
`s///`/`tr///` not bound with `=~`/`!~`, minus `/r`, or an argument-less
`chomp`/`chop`, is a `$_` write), token roots handled in
`_nodes_write_var`, and map/grep routed through `_map_grep_topic_writes` —
the foreach body-scan rule, not the value-consumer list — at all three
owner-word checkpoints.  map/grep REMOVED from `%ARG_VALUE_FN` so any
uncovered path fails conservative (flag) rather than silent.  Guards:
`writes-args-01.t` rows 14–15 (six implicit-`$_` writers reach the caller;
read-only map/grep stay RAW).  Verified: corpus-diff **emission identical
across 111 files** (no corpus file contains the shape), read-only inverse
probes stay raw, artifacts regenerated (pack body byte-identical, stamp
only).

**The lesson, for the next scan anyone writes** (same family as
[[feedback_probe_the_breaking_case]]): a scanner keyed on Symbol tokens
must be probed with the spellings that produce NO Symbol token — implicit
`$_` is the standing example in Perl, and it is the *most common* spelling
of exactly the constructs the scan existed to catch.  And positive guard
rows that all use explicit symbols test the scanner's happy path, not its
blind spot.

## §10. Next work — order unchanged

**#163 next** (from the storage-path diagnosis in the task, per §8), then
#176.2 → #184 → #185 → #159 → #150 → #152 → E4.1/E5.  #208's Role-Tiny
PASS→FAIL loss gets its cause line at the next CPAN board run (the every
3rd–5th-change cadence), not a dedicated session; the board files stay
untouched until then, exactly as you left them.
