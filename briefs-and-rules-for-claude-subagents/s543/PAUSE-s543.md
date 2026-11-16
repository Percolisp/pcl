# s543 -- running notes of the reviewing session (2026-11-16)

Read bottom-up for the newest state.  The record is `docs/session-log.md` `## Session 543` and `docs/DECIDED.md` `## s543`.

## Start
* main `cbf9c350` clean (MAIN-READY-98, code `c5f618f2`, v2-15000), CI green, nothing running, `HEAVY.holder` empty.
* The session began as questions from the USER: the state of the PPI move (64 of 66 registry rows fixed on the vendored branch, one upstream, §11 open; the report 453 ok / 6 not ok), the state against CPAN and perl's own suite, and what it takes to make PCL usable for non-XS production code.

## The decisions (all the USER's; DECIDED `## s543`)
1. **The supported-subset FREEZE**, until it is seen to work: `docs/supported-subset.md` (DRAFT; open: scope-exit DESTROY for the service claim, Moo, the module lists).
2. **The UTF-8 flag is done LAST**; the content rule is rejected as wrong (perl flags a decoded pure-ASCII string).
3. **Line tracking ON by default** -> batch s543a.
4. **Subtasks may be started again** (the s542 stop is lifted for this work); TWO at a time.

## Launched
| label | what | generation | task IDs | brief |
|---|---|---|---|---|
| s543a | line tracking on by default: the flip, the emission bar (tree default = base + `PCL_OPT=line-track`), the rows it fixes edited out, the cost table | v2-15100 | 5100-5119 | `s543/s543a-prompt.md` |
| s543b | seven filed silent wrongs: #5082 (regression) #5021 #4792 #4795 #5005 #4794 #4796 | v2-15200 | 5120-5139 | `s543/s543b-prompt.md` |

The reviewing session's own task IDs: 5084-5099.  Both batches: Opus 5.5 / xhigh, a new isolated worktree each, base = main at the launch commit.  Merge order: whoever is MERGE-READY first; the other rebases.

## Owed by the reviewing session
* The TRIAGE of the open tasks against `docs/supported-subset.md` (917 open on 2026-11-16): each `in` / `out` / `tooling` / `decision` with a reason; the silent-wrong tasks first, because exit criterion 1 counts them and the next brief is drawn from them.
* The next brief's candidates after s543b: #4887 #5081 #5061 #4793 (the rest of the s542 review findings), #5083 + #4904 + #4643 (heap exhaustion: exit criterion 4), the soak test #2945 (criterion 3), the Rosetta census #2104 (criterion 2).
* The ROUND for MAIN-READY-96..-98 + this session's merges (the board + the companion on merged main).
* For the USER: the PPI fork's default branch is `master`, so its front page does not show the 133 commits of branch `pcl`; changing the default branch is a repository setting.  Whether upstream pull request #459 should be split into small per-group requests is the USER's call; nothing was sent.
