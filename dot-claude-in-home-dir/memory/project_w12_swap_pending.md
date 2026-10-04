---
name: project_w12_swap_pending
description: W12 tree VarAnnotator swap — DONE s276 (2026-07-06); tree is default, PCL_W12_OLD text escape hatch must be DELETED next session
metadata: 
  node_type: memory
  type: project
  originSessionId: 10e88e95-1231-4e3c-ad81-6cb79494c0e8
---

**W12 SWAP COMPLETE (session 276, 2026-07-06).** Tree annotator is the
default in `Pl::VarAnnotator::analyze`; `_analyze_text` remains only as the
parse-failure/no-host fallback and behind `PCL_W12_OLD=1`. Cache gen v2-6.

**Cleanup owed next session: delete the `PCL_W12_OLD` escape hatch** (reduce
`_analyze_text` to the per-statement parse-failure fallback only) — the
module header and session-log §276 both say "one session then delete".

Key lesson (recorded in plan §W12 log + parser2-prototype §s275–276): PExpr
stores parse-state keys ON shared PPI elements (`_bareword_string` = "word
unknown AT PARSE TIME", also read back as parse input; `_has_match_context`;
`_pcl_decl_list`). Any analysis-time parse that runs before registrations
must snapshot/restore them — `Pl::Parser2::_ppi_state_snapshot/_restore` is
the shared helper (used by VarAnnotator analysis parses and `_lower_expr`'s
native attempt). The split.t `nought` crash was this class.

Related: [[project_parser2_prototype]].
