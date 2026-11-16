---
name: reference-compiler-and-measurement-rules
description: Compiler, emission, scoping and measurement rules that sat in the MEMORY.md index (eval-cache keying, one pipeline, InterpScan, corpus-diff first, capture refusals, artifacts, cached core, head-my scoping, callable names, gate-SET diffs) -- moved out at s543; most are also CLAUDE.md rules
metadata:
  type: reference
---

Moved VERBATIM from the MEMORY.md index on 2026-11-16 (s543).  Where a bullet and CLAUDE.md differ, CLAUDE.md is the newer text.  Read before touching name resolution, renames, capture, the eval cache or a gate-set measurement.

- **CLAUDE.md HAS A 40,000-CHARACTER LIMIT (s541): each rule stated shortly; EXACTLY TWO companion files (`docs/claude-md/REFERENCE.md` same headings, `HISTORY.md`), never a third, never rename a section; numbers paragraph REPLACED** [feedback_claude_md_size_limit](feedback_claude_md_size_limit.md)
- **A vanished sweep VERDICT: rebuild from `.faillog/_status.tsv` with `tools/sweep-diff.pl`** (#273).
- **A capture-dependent EMISSION must key the eval CACHE** (s387). **Block-scoped vs FILE-level lexicals reach an eval by DIFFERENT mechanisms** — capture alist vs package cell.
- **Loop/condition-HEAD `my` needs its own `let` (#297); exception-partition `my` = a different SYMBOL (#296); a rename region ENDS at the next same-name declaration (#296-B2); for a rename/scoping change the full sweep IS the gate.**
- **PIPELINE: v2 is the ONLY one (s356; [project_parser2_prototype](project_parser2_prototype.md)); ONE expression compiler + ONE seam `capture_v1`.** BUMP `*pcl-cache-generation*` on emission changes.
- **Interpolation scanning happens in `Pl/InterpScan.pm` or not at all** (standing rule §8); contract `docs/interp-scan.md`.
- **A bare NAME is a CALL only where it is CALLABLE; the two negatives differ** (#266). **Grep for the sibling copy before fixing an autoquote/callable site.**
- **Run `tools/corpus-diff.pl` BEFORE spending a full sweep** — identical emission proves the `.t` half cannot move. **Normalize compiler LINE NUMBERS in any gate-SET stderr diff.**
- **A SIGIL- or SCOPE-blind capture refusal is a BUG** — delete/rename, never narrow; **detector/rewriter/promoter share ONE resolver**. **Never `nohup` the gate** [feedback_never_nohup_the_gate](feedback_never_nohup_the_gate.md).
- **When a fix widens what a CHECKER sees, diff the GATE SET file-by-file over BOTH populations** → [project_v2_gate_set_measurement_rules](project_v2_gate_set_measurement_rules.md).
- **De-gated ≠ done**: the bar is the file's `perl-suite-run.tsv` snapshot C_ok. **CPAN board PASS/PARTIAL labels are not the measure — read ROWS.** **The term walker declines bare words and prefix ops BY DESIGN.**
- **BOXED AGGREGATES: ALL PHASES SHIPPED — elements RAW by default; PCL_RAW_ELEMS=0 = the all-boxed A/B world** (`docs/boxed-aggregates-design-s455.md`).
- **THREE CHECKED-IN TRANSPILED ARTIFACTS (`cl/pcl-pack/-mro/-warnings.lisp`) — regenerate after emission changes (`artifact-staleness-01.t` gates it).** **A worktree compare drops the 14 xs rows — set PCLXS_DIR.** Write inverse guards; never `cp` a snapshot over a live file [feedback_no_stash_when_stash_exists](feedback_no_stash_when_stash_exists.md).
- **`docs/ir-spec.md` = NORMATIVE semantics manual — UPDATE on semantic emission/runtime changes.** CPAN suite baselines in [project_s304_state](project_s304_state.md).
- **RUNTIME KEPT COMPILED AND CACHED (USER s439b)**: content-keyed core under ~/.pcl-cache/core/ (PCL_NO_CORE=1 = source; a worktree gets its OWN core) — [project_test_core_fast_path](project_test_core_fast_path.md). **Keep docs/test-failures-categorized.md current** [feedback_update_test_log](feedback_update_test_log.md).
