# s543 -- HAND-OVER at the session's end (2026-11-28 10:4x; the USER ended the session) -- NEXT ACTION for the next reviewing session

Nothing runs (verify: `pgrep -af 'sbcl|prove|sweep'`, `cat ~/pcl-agent-scratch/s513/HEAVY.holder*`).  Main = `origin/main`
= CODE **MAIN-READY-118 `96bf714c`** (generation v2-17100) + record commits.  Numbers = CLAUDE.md "Test Status".  Read
`s543/PAUSE-s543.md` BOTTOM-UP for the night (the sections from "2026-11-27 21:3x" on).  The hand-over of 2026-11-25 (three
stopped batches) is DONE: `git show 5d511ff8:briefs-and-rules-for-claude-subagents/s543/resume-after-stop.md`.

## THE USER's standing words
(1) **The default is TWO subtasks at a time; another number holds for the session it is given in** (2026-11-27: three, for
that session -- and three xhigh agents beside the reviewing session used the account's 5-hour window up twice that night).
(2) Rosetta is the MAIN work (`docs/rosetta-plan.md`); the next session resumes and launches without asking.  (3) Exit
criterion 1 counts silent wrongs of ORDINARY code; a probe-found neighbour does not extend a batch unless it is ordinary;
one real program is the evidence.  (4) Correctness over a fix's speed cost: keep the fix, FLAG the number.

## What merged on 2026-11-27 / 28 (all pushed; CI green on every merge push from -115 on)
| MAIN-READY | sha | batch | what | ROSETTA |
|---|---|---|---|---:|
| 111 | `0ab08642` | s543n | the filehandle family | 547 |
| 112 | `27e061e1` | s543o | THE ROSETTA INSTRUMENT (`tools/rosetta-corpus`, the cause table, the manifest, the history); the corpus RULED 650 | 547 of 650 |
| 113 | `caf9c693` | s543m | #5310, the string-eval residue, the drop flush for a cleared container, a qr's `u` | 547 |
| 114 | `2f85209e` | s543l | the `local` residue, a signal handler's `die` through a blocking read | 547 |
| (records) | `64ceec94` `8d7a22c9` | s543p, s543q | THE CENSUS: all 103 differing programs carry a cause -- 51 causes, 30 of the 45 that are ours in nobody's list (`docs/rosetta-plan.md` 6a 6b 6c) | |
| 115 | `5f3d1cb3` | s543t | overloading as the big-number classes use it; captures restored at a sub's return (#932's sub half) | 561 |
| 116 | `ac66f5b4` | s543w | the lexer's term position: a word list written as a glob, ON THE PPI BRANCH (batch 13: groups BJ..BM, `pcl` = `eca93377`; the fork pushed, pull request #459 refreshed: 65 groups / 157 commits), a hash constructor after `not`, `NAME*$var`, a three-piece glob | 569 |
| 117 | `f61cbf04` | s543u | regex escapes: `\K`, `\G` in a substitution, `\b{wb}`, `\g{-N}`; the escape audit has ONE silent cell left (`\N{NAME}`) | 586 |
| 118 | `96bf714c` | s543v | the slice family; the right side of every `OP=` in scalar context (236 sites) | **591 (90.9 %)** |

Gate 380 files / 12,608 rows; sweep TOTAL 18,809 (one flip.t row given up by a ruled trade-off); EVERYDAY 116 of 122; 29
ordinary + 63 rare live IN silent wrongs.  CI was RED from -111 to -114 on one environment-dependent row (fixed in
`394a0012`): **read the CI run of a merge push before the next merge** (DECIDED s543, memory).

## TWO batches are STOPPED ON ORDER (worktrees under `.claude/worktrees/`; each `scratch/<label>/STOP.md` is the resume recipe)
| label | worktree | generation | state |
|---|---|---|---|
| **s543t2** (#5545: the capture scope only for a sub body that can match -- the cure for s543t's cost flag, EVERY sub call +3..8 %) | `agent-a086ae292a8fd6503` (the SAME agent and worktree as s543t; its brief is the go message, `scratch/s543t2/prompt.md`) | v2-17300 | HEAD `f67ea907` on `96bf714c`, clean.  TAKEN on the rebased tree (logs in `scratch/s543t2/`): the writers re-derived on s543u`s code (still only `p-=~` / `p-!~` reach `%p-match-record`), s543t`s probes and the review`s `t-01.pl` row for row as on main, corpus-diff (silent drops 5; 53 of 111 differ), the lib + wide A/B (2,100 pairs, RCDIFF 0: 11,435 named subs and 3,134 anonymous frames LOSE the scope, 1,274 and 330 keep it; twenty sampled keepers each hold a match / `require` / string eval), full gate 381 files / 12,611 PASS (two `parser2-02.t` rows that asserted the old frame spelling strengthened), sweep 18,809 +0 GATE clean, ir-conform 337 / 0, ir-host-leak 31, `ROSETTA: 591`, EVERYDAY 116 (warm).  **OWED: THE TIMING LEG, the point of the batch (`bench.sh` is ready: against `394a0012` and `96bf714c`; the bar is a sub without a match back in noise against `394a0012`), the session-log section (a draft is in its scratch) and #5545`s DONE note, the records-only rebase onto main, a warm run, `--record`, MERGE-READY.**  NOT REVIEWED yet: probe that a sub WITHOUT a match keeps its caller`s `$1`, and one whose only match sits in a `map` block / a string eval / an `s///e` replacement restores it. |
| **s543x** (a list operator's first operand: #5510 `sort $s OP ...`, #5504 `map(sub {...}, LIST)`, #5513 `map split, LIST`, #5515 a declarator keyword before `=>`; five programs) | `agent-a19e17bde52967185`, brief `s543/s543x-prompt.md` | v2-17200 (renumber ABOVE main's at its merge turn) | HEAD `28253c74` on `96bf714c` (rebased at 10:0x), clean; ALL FOUR members DONE with guard rows; the five programs `same` on its tree.  #5510: after `sort` a scalar is the comparator only when a term follows -- ONE predicate, reusing print`s filehandle-slot test, which also learned perl`s sign rule (`print $fh -$n` and `print $fh +(...)[0]` now go to the handle: #405`s residue; the three print controls of the brief unchanged) -- and **a PPI branch fix, group BN, section 80, on its clone`s `pcl-s543x` (head `a8178635`): the bare mirror is NOT fast-forwarded yet**; #5504 `map(sub {...}, LIST)`; #5513 split`s defaults from one place; #5515 one fat-comma predicate (the silent vanish: the strip deleted `state`, `=>` then quoted the SUB NAME, and the call lowered as a constant list in void context).  TAKEN: full gate 380 files / 12,618 PASS on the rebased tree, corpus-diff identical, gate-set scan 0 lines, drop census = blessed, ir-host-leak 31, wide A/B 2,694 SAME / 9 DIFF / RCDIFF 0 (the five programs + four real finds: `ExtUtils/Embed.pm` a `sort $var =~` drop, `File/Fetch.pm` and `t/comp/parser_run.t` a `print $fh +(...)` that went to STDOUT, Test-Simple `Hub.t` a `tests state => sub {...}` group silently skipped; comparator-form sites moved: ZERO), the member-1 sweep 18,810 +0 (pre-rebase).  VOID: `sweep-final.log` (killed on the order).  **OWED, in order: the records rebase onto main, the full sweep (18,809), companion BEFORE / AFTER, Rosetta (expect 596), everyday, the bench leg, the records (session-log, the plan`s line, the DONE notes, triage), THE PPI MIRROR FAST-FORWARD and the re-vendor as `pcl`, `--record`, MERGE-READY.**  NOT REVIEWED yet.  Filed #5620 #5621 #5622 (rare).  After its merge: push the PPI fork and refresh pull request #459 again (group BN). |

* **RESUME** = a new Opus 5.5 / xhigh agent IN THE SAME WORKTREE per batch (no new worktree), started from the main checkout,
  as `s543/resume-1127.md` did: "read `scratch/<label>/STOP.md`, the brief, the WIP commits; finish the OWED list";
  `MODEL.txt` gets a `resumed` line -- check it.  If the same agent is still addressable, a message continues it.
* **MERGE ORDER: s543t2 first** (it is in its merge turn; it removes a cost main carries today), then s543x.  At each merge:
  review probes perl -> main -> its tree with the shape the fix would break (s543t2: `~/pcl-agent-scratch/s543/review/
  probes-1128/rt/p/t-01.pl` must answer as on `5f3d1cb3`, and a sub WITHOUT a match must keep its caller's `$1`; s543x:
  `sort $by_num @list`, the comparator form, must not move), fast-forward, the FULL gate on merged main through `heavy.sh`
  WITH ITS WHOLE OUTPUT KEPT (a flaked row is run alone three times AND the gate re-run: `Pl/t/use-base-phase-01.t` row 2
  flaked once under load at -118, #4904), `tools/rosetta-corpus run` (NEW 0 / FIXED 0), records (CLAUDE.md Test Status with
  the previous paragraph moved to `docs/claude-md/HISTORY.md`; `docs/STATUS.md`; session-log; the PAUSE notes; memory STATE),
  CLOSE-WITH-ROWS, push, THE CI RUN, "main is now <sha>" to the other.

## BRIEFS WRITTEN, NOT LAUNCHED (the census's fix plan, by programs broken)
1. **`s543/s543y-prompt.md` -- UNICODE in names, strings and patterns** (#5514 a non-ASCII lexical's write sites, #5470 a high
   byte after an interpolated name, #1494 `\N{NAME}`, #2440 the POSIX classes and `\d \s \w`, #2381 `\X` as UAX #29): nine
   programs; v2-17400, IDs 5640-5659.  Its rule: Unicode facts from PCL's own tables generated from perl 5.40, never from the
   SBCL's Unicode version; its guard rows pass on the floor SBCL.  It also measures whether s543u's merged `\b{wb}`
   (`sb-unicode:words`) differs across SBCL versions.
2. **`s543/s543z-prompt.md` -- THE RUNTIME AND SHIM SINGLES** (#5502 `builtin::indexed`, #5506 POSIX `getenv` / `INFINITY`,
   #5516 `++vec`, #5471 `ref(\$s)`, #1716 `seek DATA`, #1475 `sprintf` of -0.0, #5511 a default inside a modifying
   dereference): nine programs; v2-17500, IDs 5660-5679.
3. **NOT WRITTEN: THE PARSER AND SCOPING SINGLES** -- #5517 (a foreach range built as a list: `for 1..Inf`), #5463 (a builtin
   name as an array subscript), #1285 (`f { ... }` as an anonymous hash), #2644 (a no-initialiser `my` re-entered), #5507 (a
   `state` variable of a signature sub in a nested named sub), #5503 (a nested paren group on the left of a list
   assignment), #5505 (`my $x, $y;` refused), #5512 (a `local` in a ternary arm: #5330's design), #4807 (`&$_()`), #1594 (an
   undeclared bareword in a list), #5460 (a comma operand's context): about eleven programs; AFTER s543x merges (the same
   files).
With TWO at a time: resume s543t2 + s543x; as each merges, launch s543y, then s543z; write and launch (3) after s543x.

## What is left of the corpus after those (59 programs differ on -118)
22 are keyed on a not-supported id -- **the evidence for the USER's boundary questions**: 10 regex code blocks `(?{...})`,
4 XS (`Unicode::Normalize`), 3 unbounded integers (by design, #1513), 2 `given` / `when`, 2 `:lvalue` subs, 1 `bigrat`.
2 are resource limits outside the correctness claim (#1929 64K-deep recursion and SBCL's binding stack; #5518 the fixed
1 GB heap).  The other 35 are covered by s543x (5), s543y (10), s543z (9) and the unwritten brief (about 11).  When no
program inside the subset is silently wrong and every other differing one carries a cause: **the plan's step 5 -- fetch the
FRESH sample (Perl Weekly Challenge) and run it ONCE** -- not before.

## Rulings of this session the USER has not yet confirmed (each flagged in the session's messages; DECIDED s543)
* `\K`, `\b{wb}`, `\g{-N}` are IN (the subset's "regexes within cl-ppcre's reach"; the not-supported row was an audit).
* PPI batch 13 ran (s543w, #650) on the standing "PPI until done" word and eight programs of evidence; #1686 only measured.
* The corpus is 650 (two broken probes left: one answers by hash order, one holds a date).
* The flip.t trade-off (an always-on "uninitialized `$.`" warning removed; one sweep row joins the warnings family, #221).
* The copy constructor of an overloaded `=` runs whenever the class has one (perl: only for a shared referent; no
  reference counts in PCL): three counting rows of perl's `lib/overload.t`, outside the companion's baselines (#1006).

## Cost flags of this session (kept by the s536 rule; CLAUDE.md "Test Status" has the list)
s543n bareword `print OUT` +9.5 %, `<IN>` +6.7 %; s543m `@a = ()` +81 %, `%h = ()` +12 %, `$h{k} = undef` +36..47 %
(#5540); s543l `local $a[1]` +12 %, a bare `local $h{k}` as a list value +23 %, `local %INC` +23 %; **s543t every sub call
+3..8 % (#5545: s543t2's whole point)**; s543v a slice store from its own array +15.5 %.  s543u's two flagged rows were
fixed before its merge.

## Owed beside that
* Tooling debts: #5163 #5164 #5190 #5291 #5335, #4904 (gate flakes: two rows now), #5440 (`--bless-expect` and a clock- or
  chance-dependent program), #5127 (the first everyday run after a rebase is COLD: the module cache key holds `Pl/*.pm`
  mtimes -- a warm run before `--record`).  The reviewing session's merge-gate leg script must keep the gate's whole output.
* For the USER, open: #1876 (`extra_paired_delimiters`: s543i proposes OUT); the 39 `decision` tasks -- three hold 955
  failing rows (#3670, #1664, #332); 6c / 6d / 6e of `docs/supported-subset.md`.
* THE ROUND (the CPAN board's tree half and the companion `--all --quick`, then the full companion with stamps) has not run
  since MAIN-READY-108 / -110: eight code merges ago.  It is a standing duty "after every 3-4 merges or at session end":
  OWED at the next session's start or as its first batch.
* After a crash: remove every `~/.pcl-cache` file written in the last minutes and every zero-size one (not the `.lock`
  files) BEFORE any leg (`s543/resume-s543.md`).
