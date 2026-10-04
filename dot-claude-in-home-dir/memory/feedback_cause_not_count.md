---
name: feedback_cause_not_count
description: A pass/fail COUNT is not a finding — re-run each failure for its cause line; a status can improve while its grade drops
metadata:
  type: feedback
---

**A test-file STATUS without a CAUSE is worthless, and can point the wrong
way.** Adopted s322, proven twice in s323.

**Why:** the CPAN scoreboard classifies a t-file PASS when it has "at least one
ok and zero not-ok".  A file that CRASHES after its first assertion meets that
definition exactly.  So in s323 `File-Which/file_which.t` went **PASS →
PARTIAL** — which reads as a regression and was the opposite: the file had been
dying after assertion 1 and now runs 19 with 7 honest failures.  **Getting
further LOWERED its grade.**  Symmetrically, the s322 board's 61/101 FAIL
looked like a codegen disaster; the causes said 23 were ONE missing shim and 12
were ONE harness bug, and fixing two PCL bugs took it to 48.

**How to apply:** whenever you report a per-file tally, re-run every FAIL and
capture its first cause line into a tsv alongside the counts
(`docs/cpan-widen-causes-*.tsv` is the artifact; the scratch driver is a
one-off, not tooling).  Picking that line is the hard part — SBCL's
"Unhandled … in thread" banner WRAPS onto a second line, and pl2cl may have
written warnings to stderr first, so take the message that follows the banner,
skipping the `{ADDRESS}>:` continuation.  Group the causes before drawing any
conclusion: the shape of the failure set is the finding, the count is not.

Same family as [[feedback_probe_the_breaking_case]] and the #176/#177
measurement artifacts in [[project_v2_session_state]].
