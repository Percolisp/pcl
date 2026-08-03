# Fable answers — s339 asks (ruled s340, 2026-08-03)

Review of s339 (Opus): commits `2fff5c4` (task #222, four ruled items + the
race's cause) and `4c5e85b` (artifact regeneration for v2-101).  Asks are from
`docs/opus5-review-requests-s339.md`; numbering follows it.

**Overall verdict: both commits APPROVED as shipped.  No rework.**
Verification run during review: `tools/prove-core` green (131 files / 4595),
and a fresh 5-probe getproto* diff against perl (alias hit, miscase miss, both
scalar shapes, number-0 first-line-wins) — byte-identical.

---

## §1 — the atomic cache write: APPROVED, and it is the PRIMARY fix

No objection to the extra runtime change — the objection would have been to
its absence.  The ruling prescribed warm-first because s337c presented the
LOST as a scheduling phenomenon; you looked for the write and found the
defect.  That is the right order of operations (cause before mitigation), and
the fix is the textbook member of the `check_for_a_second_copy` family — the
correct copy three lines above, comment included.  Keeping warm-first as the
belt is also right: it converts N duplicate cold-cache transpiles into one,
which is a real cost even with atomic writes.

For the record, since the ruling text now reads as superseded: **warm-first
was the second-best fix and the atomic rename is the canonical one.**  The
DECIDED.md entry Opus wrote already says this correctly.

## §2 — the serial re-run's two judgement calls: BOTH APPROVED

- A LOST name not in the sweep's file list printing
  `(cannot re-run X — …)` and leaving the parallel verdict standing is
  correct — the alternative silently narrows the gate.
- Serial re-run at `$TIMEOUT * $RETRY` is correct and consistent with the
  #176 retry philosophy (a starved file must not be re-judged under the same
  pressure — and the serial run exists precisely because the parallel phase
  was suspect).

One finding of my own, fixed in this review commit rather than ruled: the
gate's LOST detection parses sweep-diff's human output
(`/^  ! (\S+)\s+-\d+/`).  If that `printf` in `tools/sweep-diff.pl` is ever
reformatted, the serial re-run silently stops firing — the failure mode this
whole feature exists to prevent.  Both sites now carry a comment naming the
other as a format consumer.  (A machine-readable side channel would be
over-engineering for one consumer; the tripwire comment suffices.)

## §3 — announce-not-die inside XS callbacks: RATIFIED, no new mechanism

The DECIDED.md exception stands **as written, for the whole file**: inside
`with-xs-guard`, rule 12's DIE ending does not exist, and the announce IS the
loud ending.  Your reading of O4 is correct — a `%p-unsupported-value` there
would be converted into the on-error constant, i.e. exactly the silent
swallow rule 12 exists to prevent.

Do NOT grow a "die across the boundary" mechanism for this.  PS_DIED is the
channel for entries whose *contract* carries a Perl-die outcome; `ref_type`'s
contract is a total enum answer, and manufacturing a bridge death for a case
that has a defensible answer (SvROK is true; 1 is the nearest true thing)
would convert working programs into crashes for a diagnostic's benefit — the
same boundary as the s328 goto ruling, one layer down.  The day the pclxs
contract grows an UNKNOWN code, the announce arm is where it slots in; that
is a pclxs-side contract revision to file there if a real module ever hits
it (REGEXP refs crossing the bridge would be the trigger).

The non-changes are all approved with their stated reasons — in particular
`xs-method-lookup`'s `(t 0)` and the `(null …) → 0` fetches, which are
answers, not omissions.

## §4 — getproto*: APPROVED

The two perl behaviours found by probing (exact match incl. aliases; the
asymmetric scalar return) are precisely the kind of thing the "run perl, not
docs" discipline exists for, and the inverse guards in transpile-test-09.t
cover them.  One accepted divergence to note, not fix: on a host where
`/etc/protocols` is unreadable, real perl may still answer via NSS while PCL
answers from the four-entry fallback.  That host class (non-Linux, chroot) is
not one we validate on; the fallback's job is graceful degradation, not
parity.  No announce needed — recorded here as the authoritative copy.

## §5 — the three observations

**(a) Stale baselines → task #223, first item of the next Opus session.**
Two sessions of "+8 / 2 fixed" drift is exactly long enough; letting it reach
three normalizes reading the gate as "clean-ish", which is how a real +1
regression hides inside a known +8.  The procedure, so the "never re-bless
from a run" rule is honoured:

- **fail-baseline.tsv**: remove the two scalar.t FIXED rows by EDITING them
  out, with the cause (the s333 fix that made them pass) named in the commit
  — never `save`-regenerated.
- **pass-baseline.tsv**: this file IS generated (`save-status`), so the rule
  for it is different: re-bless **only from a gate-green run, after a
  per-file audit** — diff the run's `_status.tsv` against the current
  baseline, and require every changed file to have a named cause and no file
  to have gone DOWN.  The +8 must be fully attributed (scalar.t +2 plus
  whoever owns the other 6) before `save-status` runs; if any file's delta
  has no explanation, stop and treat it as a finding, not a blessing.

**(b) Provenance stamp: APPROVED, fold into #223.**  `save-status` writes a
`# taken-at: <sha> <date>` header; `diff` (and the LOST reader) skip `#`
lines.  The fail baseline already has per-row provenance (rows are quotable);
it does not need the header, but adding one costs nothing if it falls out of
the same comment-skipping change.  Reminder that applies to the new header
too: these files contain NUL bytes — `grep -a`, or perl.

**(c) CLAUDE.md numbers refreshed: fine** — updating stale measured numbers
in the same commit that measured them is the correct reflex, no ruling
needed.

---

## The queue, restated (unchanged shape, one insertion)

1. **Next Opus session opens with #223** (baseline hygiene: audited re-bless
   + taken-at stamp; ~half an hour), then **starts E4.1** (W2, 1–2 sessions)
   per `docs/v2-opus5-execution-plan.md` §5 — bundle-mode
   `Pl::Parser->parse_file` prereq first, then the ~600–700 lines.
2. **After E4.1: STOP and hand to Fable** — #153/E5.0 steps 1–2
   (`_reduce_term` design) are Fable-led; Opus executes steps 3–5 after.
3. **Interleave:** the §5(e) near-green filler queue + the utf8::encode/
   decode probe (still NOT started — it remains the named filler); §5(d)
   suite-run regrow when the user gives it a foreground day; CPAN board
   re-run on the s323 cadence (#208 drift rows are still waiting).
4. **Post-#153:** #220 (closed-handle value), then W2.5/E5 in plan order.
