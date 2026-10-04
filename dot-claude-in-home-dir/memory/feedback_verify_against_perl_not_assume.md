---
name: feedback_verify_against_perl_not_assume
description: "Verify every change against real Perl (compatibility is the target) — don't assume/reason; and cut bookkeeping ceremony"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 5de546c0-c36d-48a2-8086-3d89be0a5915
---

The user (2026-07-07) pushed back that a run of work was "more bookkeeping than
development" and that I "verify everything and not just assume — the target is
compatibility."

**Why:** PCL's whole point is Perl/CPAN compatibility. Compatibility is only
*established* by comparing PCL's actual output to real `perl`'s on the same
input — never by reasoning that a change "should" be right or "can't regress."
A passing test with a hardcoded expected string is weaker than a live diff
against `perl`. And time spent on commits/docs/session-logs/task-tracking is not
development.

**How to apply:**
- **Diff against `perl`, not against my expectations.** For any behavioral
  change: run the same input through `perl` and through PCL and compare bytes.
- **Shared-runtime changes → run the perl-tests sweep** (`perl
  sweep-perl-tests.pl`), the corpus-wide PCL-vs-Perl oracle, and check the
  fully-passing count didn't drop. A change to `stringify-value` /
  `p-method-call` / `p-print` affects the whole corpus; a targeted Pl/t test
  does not verify it.
- **No write-offs by reasoning.** "pre-existing", "unrelated", "can't regress"
  are hypotheses to TEST (I called a real `#<fd-stream>` stringify bug
  "pre-existing and unrelated"; it was a 3-line fix). See
  [[feedback_dont_write_off_fixable]].
- **Cut ceremony.** Fewer doc/log/commit cycles; spend the budget on the fix and
  its verification. Strengthen `tools/difftest-ops.pl` coverage rather than
  writing more prose.
