---
name: project_error_message_fidelity_not_required
description: "User decision (2026-07-28) — matching perl's exact fatal-error TEXT is not a goal, \"unless it is really cheap\""
metadata: 
  node_type: memory
  type: project
  originSessionId: 0d9783df-2dca-4bf1-b95f-27a7daeef04f
  modified: 2026-07-27T22:45:48.983Z
---

**User decision, 2026-07-28 (s316e):** PCL does not need to reproduce perl's
exact fatal-error message text ("Can't call method \"go\" on an undefined
value at - line 1.", "BEGIN failed--compilation aborted", read-only
enforcement wording) — "unless it is really cheap".

**Why:** PCL targets correct execution of valid CPAN code; per-message
wording would mean duplicating perl's error-reporting infrastructure for
rows no CPAN module depends on.

**How to apply:**
- Do NOT start a perl-style top-level error formatter / per-error-class
  wording project. The ~30 message-fidelity rows of run/fresh_perl.t are
  blessed: expected-tsv XDIFF row citing not-supported.md §Error message
  text and format (where the decision is recorded).
- If a specific CPAN module pattern-matches a specific message it actually
  triggers, fix that ONE message at the point that raises it — that is the
  "really cheap" carve-out.
- Related out-of-scope: invalid-perl detection (CLAUDE.md §9), "at FILE
  line N" suffixes (§Error messages: no location info).

Closed task [[project_v2_state_ledger]] item #127 with this; see the s316e
session-log entry.

**REFINED by the USER, s494 (2026-09-21):** "It is OK if errors aren't the same as Perl, as long
as they fail in the same places. But we don't want it to be horribly messy either. :-)"
Three bars, in this order:
1. **WHERE it fails is the bar** — a program must die at the statement perl dies at (non-zero
   status).  So the cases where perl dies and PCL LIVES (#2103: strict refs, read-only
   modification, `@$undef` rvalue … 16 measured) outrank every message-text row.
2. **TEXT is free** — any readable one-line message will do; never chase perl's wording.
3. **TIDY is a bar** — an uncaught error of ANY kind is ONE readable line on stderr, never a
   30-line SBCL backtrace, a `#S(p-box …)` struct dump, a "While evaluating the form…" load
   note, or 1012 lines of guard-page chatter (#2108, #1595, #1970, #1929).  The backtrace goes
   behind `PCL_BACKTRACE=1`, which the test runners set through tools/lib/PCLSbcl.pm.
**How to apply:** `pcl --check` (#2194) compares exit status by CLASS and never compares stderr
text, for this reason; a review probe for an error path asserts the status and "one line", not
the wording.
