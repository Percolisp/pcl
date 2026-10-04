---
name: project_cpan_board_state
description: "State of the 14-dist CPAN board — both halves classified (s343 FAILs, s344 PARTIALs), the two oracle rules, and the bug families the rows collapse to"
metadata: 
  node_type: memory
  type: project
  originSessionId: 9a4cd67d-a635-4ee5-9d46-cffb894a25b8
  modified: 2026-08-05T20:35:51.986Z
---

# The 14-dist CPAN board — where it stands

**s344 (`fd20bb9`): both halves classified.** 183 files: 65 PASS / 65 PARTIAL /
53 FAIL; 1794 ok / 674 not-ok. Authoritative reading lives in the repo:
`docs/cpan-board14-survey-s343.md` (FAILs) and
`docs/cpan-board14-partials-s344.md` (PARTIALs), with per-file data in
`baselines/cpan-board14-s343.tsv`, `-fail-causes-s343.tsv`,
`-partial-causes-s344.tsv`. Read those before triaging anything CPAN.

**FAIL half (s343): 41 real, 12 artifact.** ~4 causes — Capture-Tiny ≈ one bug
(#201, highest-value single fix), role/method-modifier cluster 11 (#135, incl.
`read error during load` = unreadable emission), XS boundary 6 (deliberate),
singles (sqrt-of-negative, `AF_UNIX`, Test::More `subtest` unimplemented).

**PARTIAL half (s344): 635 of the 674 rows, 8 families, 85% in three dists** —
Text-Balanced 300, Sub-Uplevel/`caller` 127, Scalar-List-Utils 110. Nothing
there is a board artifact.

## The two oracle rules (both learned the hard way)

1. **Always run real perl beside the board.** Its FAIL rule is "zero ok", so a
   file perl itself SKIPs (author tests, `*-report-prereqs.t`) counts as a PCL
   failure — that inflated the FAIL column 41 → 53 (s343).
2. **Never pass `-I<dist>/lib` for an XS dist whose `.so` is unbuilt.** perl
   then dies at `use` and emits no TAP, so real failures read as artifacts —
   that made 21 Scalar-List-Utils files (110 real rows) look like noise (s344).
   Use the installed module and note it may be an older version.

## Bugs the s344 pass probed down to repros

- **#232** `goto LABEL` emits a `go` outside its tagbody when the same label is
  targeted from inside a loop body AND outside it, with another label between
  (155 rows). LATENT: SBCL signals only when that branch executes.
- **#233** `caller` fidelity: returns 4 elements not 3, filename is the
  generated `.lisp`, `$0` is `sbcl`, `#line` ignored, no `*CORE::GLOBAL::caller`
  override, PCL frames visible to a caller walk (127 rows).
- **#234 SILENT WRONG** `(-f => 4, abc => 3)` → `{3=>undef, 4=>'abc'}`: a
  filetest LETTER before `=>` parses as the operator and eats the next element.
  Plain `-bareword` is fine.
- **#235** `use lib "$ENV{HOME}/x"` is not interpolated (plain strings are).
- **#236** TAP `explain()` stringifies instead of dumping — blinds ~40
  `is_deeply` rows; fix before the next triage pass.
- **#237** Text::Balanced extract offsets (one pos()/`\G` question).
  **#238** List::Util/Scalar::Util shim parity checklist.
  **#239** Sort::Versions foreign-package `versions()`, 31 rows, cause NOT
  found — `(caller)[0]` and the symbolic-ref read were probed and RULED OUT.

Related: [[project_cpan_module_log]], [[project_cpan_module_survey]],
[[project_cpan_test_suites]].
