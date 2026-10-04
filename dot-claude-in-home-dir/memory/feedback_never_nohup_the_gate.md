---
name: feedback_never_nohup_the_gate
description: "Never run the PCL Pl/t gate (or any suite run) under nohup — SIGHUP is then ignored and transpile-test-06.t's %SIG row fails falsely"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 8c025beb-d9e6-4338-a6fc-172d211bcac0
  modified: 2026-08-08T19:33:43.816Z
---

Running `tools/prove-core` (or `prove Pl/t/`) under `nohup` makes
`Pl/t/transpile-test-06.t` test 37 FAIL: `defined $SIG{HUP}` — perl reports
`$SIG{HUP}` as defined when the process was started with SIGHUP ignored (which
is exactly what nohup does), while PCL's `%SIG` pre-population correctly knows
nothing about an inherited disposition.  The oracle and PCL then disagree for
an environmental reason.

**Why:** the test compares PCL against a live perl oracle run in the SAME
process environment, so any inherited signal disposition is part of the input.

**How to apply:** start long gate/suite runs with the Bash tool's
`run_in_background`, or plain `cmd > log 2>&1 &` — never `nohup`.  If a gate
run shows exactly one failure and it is the `%SIG pre-populated` row, re-run
that file alone in the foreground before believing it (s365 lost an
investigation to this).

Related: [[project_test_core_fast_path]].
