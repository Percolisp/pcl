# s542 -- everything launched is MERGED; nothing runs; the USER's last word: do not start more subtasks (2026-11-16 ~04:5x)

Read this FIRST at the next session start; then `s542/PAUSE-s542.md` bottom-up for the detail.  **ASK THE USER whether new subtasks may be started** before launching anything: the word "do not start more subtasks" (01:3x) was not lifted.

## Main
**MAIN-READY-98 `a3d00a12` + the round's baselines + s541a's measurement + records** (CODE sha `c5f618f2`, generation **v2-15000**).  Pushed; CI GREEN on the final push `063e1437`.
* Gate **379 files / 12,303 rows PASS**; sweep **18,803**, GATE clean; EVERYDAY **115 of 122**; ir-conform 328 / 0; ir-host-leak 31; corpus-diff population 111.
* CPAN board, **round of s542 on MAIN-READY-95**: 661 PASS / 144 PARTIAL / 24 FAIL / 67 SKIP of 896 files, 109,714 ok / 1,225 not ok.  The companion snapshot is current as of MAIN-READY-95 (the full run: 0 NEW / 0 FIXED / 0 LOST).  The three code merges after -95 (s540e, s539c, s539b) are the next round's.
* PPI `pcl` = `66e690d2` on the fork AND in `vendor/PPI`; upstream pull request #459 refreshed (133 commits, 53 groups, 459 cases).
* The board's RUN phase alone: 4.2x perl (`docs/cpan-runtime-comparison-s541.md`).

## What s542 did
| merge | batch | what |
|---|---|---|
| MAIN-READY-93 `0df490cf` | s540c | a dropped escaping filehandle is recorded and written out before a child process / at a flock or descriptor trigger / at exit; `local` is not a declarator |
| MAIN-READY-94 `de3bf103` | s539a | Test::Builder's methods; the import-directive fact (`BEGIN { use_ok(...) }` imports for the parse); a tied element copied through its wrapper; Encode lookups; the TAP layer through the extension fasl cache |
| MAIN-READY-95 `75146bf6` | s540d | `tr///`, `$|`, `{...}`, the constant fold (`const-fold`), every module compiled once |
| MAIN-READY-96 `7e29313c` | s540e | Storable `dclone`, Digest::MD5, MIME::Base64 on runtime primitives; `canonpath` fast path; `strftime` by arithmetic |
| MAIN-READY-97 `62677d00` | s539c | PPI batch 9: `x<digits>` as a word, `{` first in a constructor, `keep_line_endings` + perl's CR rules |
| MAIN-READY-98 `a3d00a12` | s539b | binder port 4: declaration renames by the binder's uses; the region walk deleted; #4822 |
| (no code) | s542r | THE ROUND on -95: the board + the companion, baselines row by row with causes |
| (no code) | s541a | #4791: the board's run phase timed, perl vs PCL |

## THE NEXT BRIEF -- candidates, in my order (NOT written into a launch)
1. **#5082 -- a REGRESSION of s538a (MAIN-READY-91)**: `opendir $d, DIR` on a variable that held a dirhandle before (opened, read, closed) dies "Not a GLOB reference".  Ordinary code.  Bisected by the round to #3618's place check.
2. **#5083 + #4904 -- heap headroom**: since s539a (MAIN-READY-94) HTTP-Message's zip-bomb file dies before its first row at the 1 GB default (was 5 of 9); and ONE full gate on a cold cache lost a row to "Heap exhausted" (not reproduced in my cold gate on merged main).  The measurement that discriminates is in #5083.
3. **The silent wrongs the reviews of this session filed** (each with a reproducer and its cheap measurement): #4792 `defined CONST` is a filehandle test; #4795 a constant used in its own package is the string "X" when a later package defines `sub X`; #4796 a string eval's `my $x = $x + 1` over a captured lexical; #5005 `local $/, <$fh>`; #5021 `tr` reads `\-` as a range; #4794 `undef $fh` on an escaped handle writes nothing (a flush at the statement); #4887 `BEGIN { use_ok('constant', ...) }`; #5081 `select` / `open` through a symbolic name; #5061 a lone surrogate in an eval's text breaks later uncached evals; #4793 a `()` sub installed by a BEGIN glob assignment.
4. Speed, when its turn comes (the list is ranked in `docs/cpan-runtime-comparison-s541.md` section 4): the string-eval compile policy (#2421: 28 s of the board's run), run-time `require` (#5065), JSON::PP (#4967), `Symbol::gensym` (#4964); and the residue #5024 (two `bless` rows with a DESTROY class ~7 % slower since s540d, cause not found).
5. #5046 (Clone onto the deep-copy primitive), #4922 (blockers the binder made unnecessary), #4790 (the FILE of a call site without line-track; never started).

## The box
* No leg, no agent, `HEAVY.holder` empty.  **`heavy.sh` is the THREE-slot script again** (with the s542 changes: every file argument is read for the exclusive test; `HEAVY_BOUNDED=1`).  The four-slot one of this sitting is `heavy.sh.four-slot-s542` beside it: use it only on the USER's word that nothing else runs.
* Slot rules: `s473/COMMON.md` "HEAVY SLOTS".  Runner gaps reported by the batches, NOT patched: `tools/cpan-board.sh` cannot pass `--refresh-oracle`; `heavy.sh`'s exclusive-waiting marker misses an exclusive leg that has not got slot 1 yet; a batch's back-to-back benches starve waiting bounded legs (cut timings into legs with a gap, or queue bounded legs first).
* The worktrees of this session's batches are clean and can go at the next tidy-up (their `scratch/` holds the logs the records cite).

## Standing at each merge (as done all session)
REPLACE the paragraph "Test Status -- CURRENT NUMBERS" in CLAUDE.md, move the old one to the top of `docs/claude-md/HISTORY.md` part 1; a rule that changes is changed in CLAUDE.md AND `docs/claude-md/REFERENCE.md`; STATUS.md beside it; tasks closed only with a DONE note and no baseline row keyed on them.

## Open USER questions carried
s534a method-lookup batch #4523-#4525; #4503; `docs/eval-in-cl-plan.md` section 7 (#4480); #1836; #4624; whether `tools/rebuild-pack` should regenerate all three artifacts; and from this session: the deep-copy depth limit (1,000 levels, past it the old path answers and crashes near 5,000: #5041), and whether the first run of a program with its own module may cost +0.8 s once (s540d: every module compiled once -- the USER's s540 ruling; the number is new).
