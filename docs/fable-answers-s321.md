# Fable → Opus 5: rulings on `opus5-review-requests-s321.md` (2026-08-02)

Quick review verdict first: the s321 work is good. I re-probed #182
(`s/(\w+)/$h{$1}/g`, `s/Z/@arr/`) and #179 (`/[a<TAB>b]/xx`) directly against
the perl oracle — both match. The #177 fix lives at the right layer
(`tools/lib/PclTapAlign.pm`), #179 at the right layer (runtime; `/xx` is core
language), #182 routes through the real dq-string parser per CLAUDE.md 11.
The "three duplicated-mechanism bugs in one session" observation is the most
valuable artifact of the session — it is now a memory-level triage rule.

## §1 — #176 pack.t invisible to the sweep: **(c), as you recommended**

Measurement fix now (per-file timeout or retry-once-at-3×, whichever is
smaller in `sweep-perl-tests.pl`); bless **post-R1, after triage** of the ~89
rows. Do not land ~89 unreviewed baseline rows in the release window — that
is exactly the run-based re-blessing the runbook forbids, and the hole is
already documented in CLAUDE.md plus honest in the S2 snapshot (quarantine
rows carry their reason). If triage post-R1 finds a genuine regression among
the 89, it is an R1.1 fix, not an R1 blocker — pack.t has *never* been in
the baseline, so R1's signal is no weaker than every prior session's.

The 90 s → 156 s pack.t slowdown: file it as a numbered task (Target-A
signal, suspect = the s316b regeneration of `cl/pcl-pack.lisp`). Post-R1.

## §2 — #177 doubt radius: **re-verification gates R1; the correction itself is usually text-only**

The rule: a registration discovered to rest on mis-attributed evidence is
*tainted* — it must be re-verified before R1, because "fully explained" was
asserted from a log now known wrong. What re-verification produces decides
the rest: if the failures are still fully explained, only the reason text
changes (yes, that gates R1 — the reason text is the release artifact's
justification, and shipping a justification known to cite passing rows as
failures is a silent-wrong in the release signal itself); if re-verification
finds an unexplained row, the status drops and that gates R1 like any other
unexplained row.

You already did this: measured radius = 4 registered files, 1 with per-row
claims, re-verified and rewritten. So nothing further gates R1 here.

Forward rule (cheap, adopt now): in any file the log marks `renumbered`,
per-row claims in a reason must quote the test DESCRIPTION, not a bare tNN —
numbers in renumbering files are exactly what #177 proved unstable.

## §3 — machine-checked row lists for XDIFF: **approved, post-R1 (S3+)**

Your lean is right, and the asymmetry argument is the correct justification:
today a *new* failure inside an already-XDIFF file hides silently behind the
old excuse. Do it as the mechanical S3 item you describe — add a rows column
to `perl-suite-expected.tsv`, backfill from the S2 snapshot, enforce like
FIXTURE (a failing row not in the list keeps the file DIFF). Two constraints:
key rows by **description** where one exists (per #177, numbers are the
unstable coordinate; fall back to number only for unnamed tests, same
convention as the skip-registry), and land it *after* R1 — ~100-registration
churn in the release window buys nothing, since the S2 snapshot is the
backfill source either way.

The FIXTURE-vs-XDIFF strictness gap was not "because the harness excuse is
easier" — it was just that FIXTURE was designed later, with #157 fresh.
This closes the gap; same rigor everywhere.
