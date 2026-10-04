# The op/ rounds (t6a–t6c) — #1501's census residue, designed s485 (Fable, 2026-09-14)

After the six t5 rounds the census residue is `op/` (t5f's measurement: 2,402 causeless
blessed fail rows, 2,375 of them op/; re/ comp/ uni/ run/ io/ class/ mro/ at ZERO).
Measured on main edd6dc78 before today's merges: **op/ = 2,375 causeless fail rows in
91 files** (`baselines/perl-suite-fails.tsv`, sixth field empty) **+ 45 files / 519 rows
UNEXPLAINED** in `baselines/row-shortfall.tsv` (CHECK 1's population).  t5a (the 50–199
shortfall band, 8 files) and t5b (six clusters, 786 rows) already took op/'s largest
single facts; what is left is WIDE, not deep — 91 files, median 7 rows — so the rounds
are cut by fail-row band, biggest first, three rounds of ~1 agent-day each:

| round | band | files | causeless rows | the files (rows; +N = UNEXPLAINED shortfall rows) |
|---|---|---|---|---|
| **t6a** | ≥ 50 | 12 | 1,138 | decl-refs.t 270 (90 already caused by t5a — read them first), tie_fetch_count.t 212, caller.t 83, lexsub.t 79 (+15), inccode.t 76 (+48), require_errors.t 68, filetest.t 68 (+2), method.t 60 (+37), universal.t 60, ref.t 56 (+15), tiehandle.t 56 (+23), tiearray.t 50 |
| **t6b** | 20–49 | 21 | 782 | substr.t 49 (+46), tr.t 48 (+2), gmagic.t 47 (+45), coreamp.t 47 (454 caused), bop.t 41 (+13), lex.t 41 (+1), split_unicode.t 40, magic.t 39 (+1), filetest_stack_ok.t 38, eval.t 35 (+1), override.t 34 (+16), write.t 33 (468 caused), each.t 31 (+18), lc.t 31, postfixderef.t 27 (+7), fork.t 27 (+27, #1770), sprintf2.t 25, sprintf.t 25, array.t 24, sort.t 22, die.t 22 (+6) |
| **t6c** | < 20 | 58 | 455 | the tail (reset.t 19, pos.t 19 (+3), exec.t 18 (+16), kvhslice.t 18, local.t 16, … 31 files with 1–2 rows) + the shortfall-only files (runlevel.t +24 #1769, lfs.t +17, inc.t +18, svflags.t +16, signame_canonical.t +15, numify_chkflags.t +14, hash-clear-placeholders.t +9, getppid.t +8, dbm.t +5, lock.t +5, require_37033.t +4, attrhand.t +4, …) |

Recipe per round = the t5 recipe unchanged (CHECK 1 on the shortfall files: the aborting
FORM + the recovery line; CHECK 2 per file: shapes, rows per shape, ONE representative vs
perl; FIX ≤ 1 h / FILE / CITE; the cause into EVERY row; movers serial + A/B before a
splice; the t5 bar; records).  What t6 adds: (1) the population is re-measured on the
launch tree FIRST (s484c/s484d moved op/ rows today — tie.t, anonsub.t, gv.t, sub.t) and
the table above is corrected in the round's session-log section; (2) op/ files already
carry per-file owners from earlier sessions — grep `docs/DECIDED.md` + the task store for
the FILE NAME before probing (tie* → #155 + the tie/get-magic class; caller.t →
`docs/caller-implementation.md`; lexsub.t → #376/#377/#374; require_errors.t → #1688 +
s438's snapshot; filetest.t → #403/#404 (a filetest's FALSE is a defined "");
method.t/universal.t → #1664 (typeglob model) + #1737 + #266; ref.t/decl-refs.t → #332
(refaliasing spellings) + #1664; inccode.t → #1708 + `@INC` hooks); a ruled class is
CITED, never re-probed.  Order t6a → t6b → t6c; each launches in the free slot as merges
allow (two agents at a time); t6b/t6c briefs are copies of t6a's with their band.
