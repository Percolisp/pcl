---
name: project_fable_track_s407
description: "Fable's own queue after the s407 batch review — Option B phase 2 SIZED (mostly Opus refusal work + one grammar piece), #281 IR design is next; the census-text method and the standing review rules it produced"
metadata: 
  node_type: memory
  type: project
  originSessionId: e7b6d798-4ed9-43e4-9f93-77a9d60e9645
  modified: 2026-08-16T12:49:20.581Z
---

**State (2026-08-16, s407, Fable):** the s404+s405+s406 review batch is ruled
(`docs/fable-answers-s406.md`) and Option B phase 2 is SIZED
(`docs/option-b-phase2-plan.md`).  Fable's next design item is **#281 (the IR
pass: re-measure `generated-cl-ir-review.md`'s friction list against current
emission, macro vocabulary at zero speed cost, ir-spec normative)**; boxed
aggregates stay after v0.1; #221 post-release.

**Why phase 2 shrank:** the #138 drop census read as TEXT (new
`tools/drop-harvest.pl` after `tools/drop-census.pl`) is ~300 feature absences
(given/when, class, defer, hexfloat, formats, unicode stash names, `~~`,
indirect object) + ~40 term-grammar shapes + ~15 lexer bugs.  The `$end_pars`
collapse buys ~10% of the metric — so: Track A refusals (#371, Opus), B1 named-
unary-operand-may-begin-with-named-unary (#372, Fable-designed grammar, A/B by
the s398 fold recipe), B2 = #343's shape, fillers #369 (`qx{}` delimiters
DROPPED) / #370 (term-initial `~~` PPI mis-lex), then the announce→DIE flip at
≤ ~30 all-explained.  **Do NOT rewrite `parse()`'s main loop for this.**

**How to apply (review method that keeps finding regressions):** for any
token-stream repair or parser widening, probe the TERM FORMS it must NOT fire
on (method call `->name`, subscript `}`, `)`, quote, number, declared term) —
s407 found two regressions (#361 `->name x 3`, #351 `->w / ->h /`) that 17
probes + a 28-site population scan missed because all of them asked only where
the repair SHOULD fire.  And a SILENT-WRONG task must carry the EMISSION of its
reproducer (`pl2cl` output): #362's real cause (`%to-number-raw` had no
`functionp` arm; the compared-only side was type-flow-frozen to a raw numeric
slot) was one emitted line, invisible to any perl-level probe.

Related: [[project_parser2_prototype]], [[project_pcl_measurement_traps]],
[[feedback_probe_the_breaking_case]], [[feedback_check_for_a_second_copy]].
