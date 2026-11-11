# PAUSE-s538.md -- the reviewing session's running notes (s538, started 2026-11-10 23:4x)

RESUME RECIPE after a cut: read this file bottom-up, then `s538/resume-s538.md`, `git worktree list`, each batch's `scratch/<label>/STOP.md` first line and its newest logs' END lines.

## 23:4x -- START.  The USER: continue.
* The box was rebooted (up 23:22); `/tmp` empty; no heavy leg, no SBCL, no `HEAVY.holder*`; 7 GB available.  heavy.sh = three slots.  `git worktree prune` removed the dead `/tmp` ref-tree.
* Main `9cd2f311` = MAIN-READY-85 `bdd36d06` + records (code `4a39a1c7`, v2-13300).  origin/main = `9cd2f311`; CI GREEN on it (public API, 23:4x); install-matrix GREEN on `4b1f7493` (its last trigger).
* Written: `s538/resume-s538.md` (the delta since `s537/resume-after-stop.md`).
* RELAUNCHED, each in its existing worktree (no isolation), model opus / effort xhigh (the USER's s537 word, standing): **s536i**, **s536j**, **s537a**.
* ROUND BOARD resumed at chunk b (`~/pcl-agent-scratch/s537/round/round-s538.sh`, label `s538-round`, chunks b-f on the `7284c977` extraction; the killed `board-b.log` renamed `board-b.void-0117.log`).
* NEXT: merge in MERGE-READY order with the usual review; s537b into the first freed slot; the companion `--all --quick` on merged main when no agent has a leg queued.

## 23:5x -- s536i PRE-REVIEWED while it takes its legs (tree `88108e97`, code final unless a leg fails).
* `Pl/t` diff read: only five plan counts removed; rows added to autoviv-02, blessed-copy-01, elem-assign-lvalue-01, list-slice-01, lvalue-ref-01, proto-alias-01.  Nothing weakened.
* Probes perl -> main -> tree (`~/pcl-agent-scratch/s538/review/i/p1..p6.pl`, `p3b.pl`): tree = perl on p1 (#4682 list slices), p2 (#4684 magic lvalues), p4 (#4683 blessed hash spread), p5 (#4681 `++` on an assignment), p6 (#4283 + #4741, incl. the breaking shapes `$a[$k++] = 1`, `$g{$key .= "x"}++`, `\$b[$#b + 1]`, nested subscripts); main wrong on all six files.  ONE miss in p3 (#4685): `\(scalar($x))` / `\(scalar($a[0]))` -- the parenthesised one-element form -- still references a copy; every other spelling is right.  Pre-existing on main: **#4783 FILED**, not held against the batch.
* Left for its merge: the bars in its report, ff, CLAUDE.md + STATUS numbers, its tasks COMPLETED by me.

## 23:5x (2) -- s536j PRE-REVIEWED while it establishes its logs (tree `094108ec`).
* Probes perl -> main -> tree (`~/pcl-agent-scratch/s538/review/j/p1..p4.pl`, `q1..q13.pl`): tree = perl on p1 (#4701 `-Foo::bar`, `-CORE::log(1)`, hash keys, `5 -Foo::bar`) and p3 (#4482 `__DATA__ =>` / `__END__ =>` at a line start, the real `__DATA__` section still read); main wrong on both.  #4542: the statement-start shapes `x3();` and `x3;` are fixed (main drops `x3;`), eight control shapes identical on both.  p4 (#4302 CR in literals) tree = main, as its STOP.md says (NOT fixed -> #4761).  No regression found.
* Residue, pre-existing on both: **#4784 FILED** -- `x<N>` in a list-operator argument slot (`print x64()`, `print x3() x2` drop loudly; `print STDOUT x3()` prints NOTHING, a silent wrong); PPI lexes Operator `x` + Number there.  For the next PPI batch.
* Note for the #4302 / #4761 owner: perl itself reads a CR LF pair in a source FILE as LF inside `q{}` and a here-doc (probed: `q{a<CR><LF>b}` is length 3) and keeps a lone CR, a CR in an eval STRING and a CR in `__DATA__`; the fix is not "keep every CR".

## 00:4x (2026-11-11) -- TWO USER RULINGS, both on PPI (DECIDED ## s538)
* The PPI fork push is at my discretion from now on.  DONE at once: `pcl` = `86623347` (batch 7, what main vendors) pushed fast-forward to `github.com/Percolisp/PPI` (was `a376a6d8`, 14 commits).  Batch 8 (s536j) is pushed after its merge -- by me, never by the agent.
* #4761 (PPI rewrites CR LF / CR to LF): fine for pure code, not inside strings.  Recorded in the task with today's perl probe (perl itself reads CR LF as LF inside `q{}` / a here-doc in a source FILE; keeps a lone CR, an eval string's CR, `__DATA__`'s): the bar is perl shape by shape.  Owner: the next PPI batch.
* STATE 00:3x: s536i and s536j both wait for their exclusive bench leg (the last measurement); the round board is on chunk c (IO-Compress), chunks a + b done; s537a has its full gate + A/B running, its sweep queued.

## 00:5x -- **s536j MERGED = MAIN-READY-86 `d2e4ba52`** (code sha `3d524ad7`, generation v2-13500 on main)
* ff 8 commits.  Its bars (report): gate 369 / 12,009 PASS (worktree checkout `998f79e0`; the extraction gate fails ONE row, `pcl-runtime-env-01.t` row 1 = #4763, on main too), sweep 18,795 +0, install-pcl 61, verdict PASS / PASS, ir 31 / 328-0-17-0, corpus-diff IDENTICAL, wide 1 DIFF (Math/Complex.pm), gate-SET identical, everyday 116 + `--record`, Calc.pm 4.58 -> 4.43 s.  Code unchanged since my pre-review (`git diff 094108ec d2e4ba52 -- Pl cl lib vendor tools` empty); `Pl/t` diff = +29 lines in `ppi-token-rows-01.t`, nothing removed.
* QUEUED behind s537a's exclusive bench (label s538-merge, `~/pcl-agent-scratch/s538/merge/`): `j-check.sh` (six guard files on merged main + probes p1 p3 q5) and `prove tools/t/install-container.t` -> `j-container.log`.  **The push of main and of the PPI fork (`c2f21706`) waits for both.**
* COMPLETED by me: #4701 #4542 #4482.  #4302 stays open (= #4761, with the USER's ruling).  CLAUDE.md + STATUS numbers.  Filed by s537a meanwhile: #4803 #4804 (committed with this).
* s536i + s537a TOLD (SendMessage 00:5x): rebase, re-take corpus-diff + wide A/B vs `3d524ad7`, gate, sweep, ir; **s536i takes v2-13700** (its v2-13400 is below main now), s537a keeps v2-13600, the s537b brief moves to **v2-13800**.  s537a warned that its #4762 / #4703 edits sit in the same two files and topic as s536j's qualified-autoquote change.
* Round board: chunks a b c done (c at 00:39); d waits behind the bench.

## 01:1x -- THE USER APPROVED THE COMPATIBILITY PLAN (six points; DECIDED ## s538; memory `project_compat_plan_s538.md`) and asked to be told when point 4 (the three decisions #2370, #233 / #1361, #1389) is reached.
* **s537b LAUNCHED** in the slot s536j freed (a new isolated worktree; point 2, silent wrongs / refusals in name resolution; v2-13800).  RUNNING: s536i (rebasing over -86, v2-13700), s537a (v2-13600), s537b.
* NEXT BRIEFS to write: **s538a** = point 1, the board's whole-file losses, from the round board's `--diff` + the `*PLAN*` / `*FILE*` rows of `baselines/cpan-board14-fails.tsv` (pick by assertions hidden, one cause each); **s538b** = point 3, parser port 4 (the renames through `_rename_decl_within`; #3694's "NEXT, port 4" note) or the next seam kind.

## 01:3x -- brief `s538/s538a-prompt.md` WRITTEN (point 1, board whole-file losses part 1), not launched: it takes the next freed slot.
* Members: #660 (Path-Tiny input_output x3), #3554 (Path-Tiny filesystem + rel-abs), #3781 (URI punycode + iri), #3615 + #3618 (Text-CSV 91_csv_cb, 90_csv); IDs 4841-4859; generation v2-13900.  Picked from the 129 `*PLAN*` / `*FILE*` rows of `baselines/cpan-board14-fails.tsv`: 67 PERL-SKIP, 19 ns-xs, **6 ns-destroy-gc + Path-Tiny sig_die.t (#2006 PART 2) = POINT 4**, the rest single causes.
* Part 2 (a later brief, the harness half -- needs the companion `--all --quick` per batch): #3823 (Test::Builder parent / expected_tests), #3495 (Test::Builder->details), #3852 (reduce), #4167 (Encode big5 aliases), #3619; #3555 goes to the next PPI batch with #4784; #4643 / #4322 are heap questions.
* **POINT 4 IS REACHED ON PAPER:** scope-exit destruction already holds 7 whole board files (Try-Tiny context.t + finally.t, IO-Compress 104destroy-* x4, Path-Tiny sig_die.t).  To raise with the USER with the board numbers once the round board is read.

## 01:5x -- THE USER SCHEDULED `DESTROY` AT PROGRAM EXIT (Stage 0, #2371) and asked for README.md etc. to be updated when it is done.  Stage 2 (#2370) waits for the point-4 write-up.
* Brief `s538/s538b-prompt.md` WRITTEN (IDs 4861-4879; generation v2-14000 only if emission changes; the documents are part of the batch; measurements A / B / C required).  **It takes the NEXT freed slot, ahead of s538a** (the USER asked for it directly); s538a the one after.
* For the review of s538b: read the README bullet + STATUS row it writes against its own measurement A (what a WEAK registry leaves out); check `bless` cost on a no-DESTROY program; `Test::More` exit codes unchanged.

## 01:4x -- s536j's merge bars DONE on merged main: `j-check.log` (6 guard files / 147 rows PASS; probes p1 p3 q5 = perl) and `j-container.log` (`prove tools/t/install-container.t` 17/17 PASS, END rc=0 01:40).  PUSHED: main, and PPI `pcl` = `c2f21706` to the fork.

## 02:2x -- **s536i MERGED = MAIN-READY-87 `5ca8e976`** (code sha `23f0f917`, generation v2-13700 on main)
* ff 14 commits.  Its bars on the combined tree `8db886e9` (report): gate 369 / 12,018 PASS, verdict PASS / PASS, sweep 18,795 +0 GATE clean, corpus-diff 10 / 111, wide 112 DIFF all explained (`scratch/s536i/eab-explained.txt`), RCDIFF 0, ir 31 / 328-0-17-0, YAML-PP 32.cyclic-refs.t 8/8, everyday 116 + `--record`, Calc.pm 5.54 -> 5.52 s.  FLAGGED bench: `mapmulti` +2.7 % (twice), `pushloc` +5.1 % (unclear).  Batch code unchanged since my pre-review at `88108e97` (ExprToCL / VarAnnotator diff empty; the runtime differs by the generation string only).
* QUEUED: `i-check.sh` on merged main (9 guard files + my probes i/p1..p6, j/p1 p3).  **The push waits for it.**
* COMPLETED by me: #4682 #4684 #4685 #4683 #4681 #4283 #4741.  CLAUDE.md + STATUS numbers.  #4742 (filed by it) and #4783 (mine) stay open.
* s537a + s537b TOLD (02:2x): rebase over `23f0f917`; **s537a takes v2-13750** (its v2-13600 fell below main), s537b keeps v2-13800.  Earlier (01:5x) s537b had wrongly concluded that s536i was merged from main's records commits: corrected by message; every agent prompt now says "check the code sha before concluding main moved".
* **s538b LAUNCHED** (DESTROY at program exit + the documents; a new isolated worktree) in the slot s536i freed.  RUNNING: s537a, s537b, s538b.  NEXT slot: s538a (point 1).
* Round board: chunks a-d done (d at 01:39); e, f remain.

## 02:2x (2) -- s536i's merge bars DONE on merged main: `i-check.log` 9 guard files / 156 rows PASS; probes i/p1 p2 p4 p5 p6 + j/p1 p3 = perl; i/p3 differs in the ONE line that is #4783 (`\ (scalar($a[1]))`), as at the pre-review.  PUSHED main.  CI green on the earlier push `6cf7d9d1` (the s536j merge).
