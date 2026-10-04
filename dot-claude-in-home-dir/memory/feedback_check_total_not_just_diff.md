---
name: feedback_check_total_not_just_diff
description: a sweep-diff bucket count is meaningless without the file's row TOTAL, in BOTH directions — and a TAP row NUMBER is only meaningful within the run that produced it
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 307b1277-8d12-44d4-94ac-cb63a2fed1d7
  modified: 2026-08-12T06:26:54.399Z
---

`tools/sweep-diff.pl` answers "which FAILING rows moved". It cannot see a
change that removes **passing** rows: a file that aborts earlier than before
contributes fewer rows of both kinds, and if it was already PARTIAL the tool
reports `0 new, 0 fixed` and says nothing.

**Why:** caught in s328. Making the unsupported computed-`goto` form DIE
instead of silently no-op'ing looked clean and rule-12 correct, and
`sweep-diff` said 0 new / 0 fixed. The sweep TOTAL had gone 18462 → 18374
passing: `perl-tests/state.t` dropped 157/166 → 69/166, because its one
`goto state $flower = $f` sits two thirds up a file that is otherwise about
`state`, so the die truncated everything after it.

**How to apply:** after any change to how an unimplemented construct behaves
(die vs warn vs no-op), compare the sweep's `TOTAL: N passing` line against
the previous run, not only `sweep-diff`. A drop with 0 new fails means a file
is aborting earlier — find it (`Partial (early stop)` list) and re-measure
that file against HEAD in a worktree before accepting the trade. The usual
resolution is the #155 shape: **announce on stderr and fall through**, not
die. The sin of a silent no-op is the silence, not the fall-through.

**Both directions + the join key (adopted s386, `fable-answers-s385.md` §3):**
the rule cuts the other way too — "N new fails" can be PHANTOM COST when the
file's row total went UP (s384 read #299's enabler as "costs 5 rows" when it
was +19 rows, 14 ok / 5 honest-fail, net +13).  And a row NUMBER is only
meaningful within the run that produced it: join TAP by DESCRIPTION; for
unnamed rows, re-derive number→source from the CURRENT tree's own TAP, never
from the other tree's numbering (s386: #296-B2's rows 79/81 were mapped
through stale numbering onto a region that in fact passes, which produced a
false "not isolated" conclusion).

Related: [[feedback_cause_not_count]], [[feedback_fully_passing_regression]],
[[project_v2_session_state]].
