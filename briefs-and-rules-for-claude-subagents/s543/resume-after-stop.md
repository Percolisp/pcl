# s543 -- HAND-OVER (rewritten 2026-11-28 00:3x by the reviewing session, while three batches RUN) -- NEXT ACTION for whoever continues

**If you read this in a NEW session: something was running when the last one ended.**  First find out what is alive
(`pgrep -af 'sbcl|prove|sweep'`, `cat ~/pcl-agent-scratch/s513/HEAVY.holder*`, the tail of `HEAVY.queue`, `git worktree list`),
then read `s543/PAUSE-s543.md` BOTTOM-UP (its last section is the state; the sections from "2026-11-27 21:3x" on are this
session's evening).  The hand-over of 2026-11-25 that stood in this file (three stopped batches) is DONE and in git:
`git show 5d511ff8:briefs-and-rules-for-claude-subagents/s543/resume-after-stop.md`.

## Where main is
`origin/main` = CODE **MAIN-READY-114 `2f85209e`** (generation v2-16500) + record commits.  Numbers = CLAUDE.md "Test Status":
gate 380 files / 12,558 rows, sweep 18,808, EVERYDAY 116 of 122, **ROSETTA 547 of 650 (84.2 %)**, 41 ordinary + 61 rare.
CI: check the newest `CI` run (`gh run list --limit 3`); the install matrix did not need to run (no change to its paths).

## What merged on 2026-11-27 / 28
| MAIN-READY | sha | batch | what |
|---|---|---|---|
| 111 | `0ab08642` | s543n | the filehandle family (one rule names a handle's glob, `<>` ends its pass, an emulated exec's signal death, `close` resets `$.`) |
| 112 | `27e061e1` | s543o | THE ROSETTA INSTRUMENT: `tools/rosetta-corpus` (build / verify / run / classify), the cause table `baselines/rosetta-causes.tsv`, the manifest, the history; the corpus RULED 650 programs |
| 113 | `caf9c693` | s543m | #5310's file-level face, the string-eval residue, the drop flush for a cleared container (+ the review's F1: `our` / qualified / sub-captured holders), a qr's `u` |
| 114 | `2f85209e` | s543l | the `local` residue (brace names, a fresh element's undef, the valued modifier, `local %INC` / `%SIG` + the review's L1: the view), a signal handler's `die` through a blocking read |
| (records) | `64ceec94`, `8d7a22c9` | s543p, s543q | THE CENSUS: all 103 differing Rosetta programs carry a cause (`docs/rosetta-plan.md` 6a / 6b): 42 silent = 21 causes, 44 loud / dropped / resource = 30 causes, 17 on a not-supported id |

## THE USER's standing words
(1) The default is TWO subtasks at a time; another number holds for the session it is given in (2026-11-27: three, for that
session).  (2) Rosetta is the MAIN work (`docs/rosetta-plan.md`); after it the work goes on, perhaps with other priorities.
(3) Exit criterion 1 counts silent wrongs of ORDINARY code; a probe-found neighbour does not extend a batch unless it is
ordinary; one real program is the evidence.  (4) Correctness over a fix's speed cost: keep the fix, FLAG the number.

## What was RUNNING at 00:3x (three ROSETTA FIX BATCHES, all Opus 5.5 / xhigh, new worktrees under `.claude/worktrees/`)
| label | brief | generation | IDs | what | programs |
|---|---|---|---|---|---|
| **s543t** | `s543/s543t-prompt.md` + two messages (in its STOP.md) | v2-16600 | 5541-5559 | overloading as the big-number classes use it: #5465 a handler's context, #5468 an array operand, #5467 `int`'s key, #2560 `Math::BigRat->new("N/D")`, #5500 the bigint / bignum import options (a shim), #5501 an explicit `+=` handler (#1001) | 14; expected 547 -> 561 |
| **s543u** | `s543/s543u-prompt.md` + one message | v2-16700 | 5563-5579 | regex escapes: #5560 `\K` (the keep position), #5461 `\G` in a substitution, #5561 `\b{wb}`, #5562 `\g{-N}`, and a rule-12 read of the rewriter against perlre's escape list | 19; 15 by it alone: 547 -> 562 |
| **s543w** | `s543/s543w-prompt.md` (with "MEMBERS ADDED AT LAUNCH") | v2-16900 | 5600-5619 | the lexer's term position: #650 a word list written as a glob, ON THE PPI BRANCH (batch 13), + #5508 a hash constructor after `not`, #5509 `NAME*$var`, #1292 a glob of three pieces | 11; +8 by it alone |

Find each worktree by its scratch directory (`ls -d .claude/worktrees/*/scratch/s543[tuw]`); each keeps `STOP.md` current.
* **If one is dead mid-work:** RESUME it in the SAME worktree (no new one), as `s543/resume-1127.md` did: a new Opus 5.5 /
  xhigh agent started from the main checkout, told to read its STOP.md, its brief and its WIP commits and to finish the OWED
  list; `MODEL.txt` gets a `resumed` line.  A log without the `heavy.sh` END line is VOID.
* **At each MERGE-READY (the first one first):** review probes perl -> main -> its tree, INCLUDING the shape the fix would
  break (s543u: every reader of `$&` / `@-` / `pos` and a pattern without `\K`; s543t: a METHOD call keeps its caller's
  context, `int` of a plain number; s543w: a `<` that must stay a comparison); fast-forward; the FULL gate on merged main
  through `heavy.sh` (`tools/prove-core -j8 Pl/t/`; a flaked row is re-run alone AND the gate re-run); `tools/rosetta-corpus
  run` (the number; rows that turned `same` must have left the table: NEW 0 / FIXED 0); records (CLAUDE.md Test Status with
  the previous paragraph moved to `docs/claude-md/HISTORY.md`, `docs/STATUS.md`, session-log, the PAUSE notes, memory STATE);
  CLOSE-WITH-ROWS; ONE message "main is now <sha>" to each other batch (it keeps its generation, renumbering ABOVE main's if
  main passed it).  **After s543w's merge: push the PPI fork and refresh upstream pull request #459** (never the agent).

## THE NEXT BRIEFS (the census's fix plan, by programs broken)
1. **WRITTEN, waiting for a slot: `s543/s543v-prompt.md`** -- the slice family (#5466 a slice under `++` / `+=` / `undef`,
   #5464 an element `+=` a slice, #5469 a slice assignment from the same array): 5 programs; v2-16800, IDs 5580-5599.
2. **A LIST OPERATOR'S FIRST OPERAND** -- #5510 (`sort $s OP ...` takes the scalar as the comparator), #5504 + #5513 (map /
   grep's EXPR form), #5515 (a declarator keyword before `=>` in a paren-less list): 5 programs; PExpr's list-operator
   grammar (`docs/option-b-phase2-plan.md`, `docs/pexpr-term-parsing-review.md`: no guard in the `$end_pars` region); AFTER
   s543w merges (the same files).
3. **UNICODE IN PATTERNS, STRINGS AND NAMES** -- #2381 `\X` as UAX #29, #1494 `\N{NAME}`, #2440 the POSIX classes on wide
   strings, #5470 a high byte after an interpolated name, #5514 a non-ASCII lexical's write sites: 9 programs; AFTER s543u
   merges (the same rewriter).
4. **THE SHIMS' MISSING NAMES + THE SINGLES** -- #5502 `builtin::indexed`, #5506 POSIX `getenv` / `INFINITY` (4 programs);
   #5517 a foreach range built as a list (`for 1..Inf`: 2); #5460 #5463 #1285 #5471 #2644 #1716 #1475 and census B's
   one-program causes (6b's last row).
5. **Then the plan's step 5:** when no Rosetta program inside the subset is silently wrong and every other differing one
   carries a cause, the FRESH sample (Perl Weekly Challenge) is fetched and run ONCE -- not before.

## Rulings of this session the USER has not yet seen confirmed (flagged in the session's messages)
* **`\K`, `\b{wb}`, `\g{-N}` are IN** (DECIDED s543, 2026-11-28): the subset says "regexes within cl-ppcre's reach"; the
  not-supported row was an audit, not a ruling.  If the USER rules them OUT: stop s543u's member 1 and re-key the rows.
* **PPI batch 13 runs** (s543w, #650) on the standing "PPI until done" word and eight programs of evidence; #1686 is a
  measurement only.  If the USER wants it in the queue instead: stop s543w after its PCL-side members.
* **The corpus is 650** (two broken probes left: one answers by hash order, one holds a date).
* #3451's remembered-path route is not platform-touching (no install-matrix or macOS leg owed).

## Cost flags of this session (kept by the s536 rule; all in CLAUDE.md "Test Status")
s543n bareword `print OUT` +9.5 %, `<IN>` +6.7 %; s543m `@a = ()` +81 %, `%h = ()` +12 %, `$h{k} = undef` +36..47 % (#5540);
s543l `local $a[1]` +12 %, a bare `local $h{k}` as a sub's list value +23 %, `local %INC` +23 %.

## Owed beside that
* Tooling debts: #5163 #5164 #5190 #5291 #5335, #4904 (the gate flake), #5440 (`--bless-expect` can admit a clock- or
  chance-dependent program).  `./runpcl` recompiles big modules from text on every run where `./pcl` does not (s543q's note:
  28-54 s for Math::BigInt): s543t was asked to say whether that is a bug worth a task.
* For the USER, open: #1876 (`extra_paired_delimiters`: s543i proposes OUT); the 39 `decision` tasks -- three hold 955
  failing rows (#3670, #1664, #332); 6c / 6d / 6e of `docs/supported-subset.md`; the 17 out-of-subset Rosetta programs as
  evidence for those boundaries (10 regex code blocks, 2 `given` / `when`, 4 XS `Unicode::Normalize`, 1 `bigrat`), the
  three unbounded-integer programs and the two `:lvalue` ones.
* After a crash: remove every `~/.pcl-cache` file written in the last minutes and every zero-size one (not the `.lock`
  files) BEFORE any leg (`s543/resume-s543.md`).
