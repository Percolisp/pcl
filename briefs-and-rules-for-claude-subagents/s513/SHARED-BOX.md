# s513 shared box (2026-10-08 evening, Fable) -- the three s512 checkpoints are RESUMED and ONE new small batch (s513a, #2860) is added, TWO agents at a time (the default cap; the USER said only "Please continue").  MERGE ORDER: s510f -> s513a -> s510c -> s512p (a perf round takes its FINAL whole-table bench after the last merge it crosses).

**ADDENDUM 2026-10-09 00:18 (Fable): THE CAP IS THREE (USER: "Please start an extra. :-)").  s510f and s513a are MERGED (`MAIN-READY-1`, `MAIN-READY-2`: main `42fafb08`, code `5adcc156`, gen v2-4580, gate 284 files / 9,904 rows, sweep 18736, EVERYDAY 114 of 122).  RUNNING: s510c (merges THIRD = `MAIN-READY-3`), s512p (perf round 41), and NEW **s513b** = the static-parsing batch (#2871 position-aware prototypes, then #2873 `use autodie`; brief `s513/s513b-prompt.md`; task IDs 2920-2939; gen v2-4680, renumbered above main at its final rebase).  MERGE ORDER from here: s510c -> then s513b and s512p in the order they become ready (a perf round re-takes its final bench across whatever merges before it).  Every rule below applies to s513b as to a NEW batch (with isolation).**

**ADDENDUM 2026-10-09 00:31 (Fable): THE 00:21 OOM, AND THE PROBE RULE.**  At 00:21:20 the kernel's OOM killer took a `perl` at 7.9 GB -- a s513b PROBE (`scratch/s513b/pr/rec.pl`: `sub g ($) { ... g(@a) }` passes the COUNT and recurses without bound; real perl has NO recursion limit) run directly in the terminal's scope -- and systemd-oomd then failed that whole scope: the reviewing session and all three agents died with it.  **RULE: every direct probe or light run that is not a registered runner (`perl`, `./pcl`, `./runpcl`, `./pl2cl`, a one-file `prove`, a one-file `tools/run-dist-t.pl`, a hand-replaced sbcl load) is started as `~/pcl-agent-scratch/s513/probe.sh CMD [ARG...]`** -- its own scope capped at 2 GB RAM / 256 MB swap and 120 s wall clock (verified: an unbounded recursion dies at 2 GB after 2.2 s and nothing else on the box notices).  The registered runners are unchanged (`tools/run-perl-suite.pl` re-execs itself under `MemoryMax=10G`; the sweep, `prove-core`, the board, the bench go through `heavy.sh`).  A probe whose EXPECTED behaviour is deep recursion (a `$` prototype that turns a list into its count is exactly that shape) carries a depth guard in its source (`our $d; die "deep\n" if ++$d > 20;`) BEFORE it runs under perl.  s512p's companion of 00:17:56 SURVIVED (its own scope) and holds the box's lock through a stand-in holder `s512p-orphan` until it ends.  All three batches resume as FRESH agents from `s513/resume2-s510c.md`, `s513/resume2-s512p.md`, `s513/resume-s513b.md`.

FIRST ACTION, before anything else: write the EXACT model id you are running as (from your own system
prompt, e.g. `claude-opus-5-5`) into `$W/scratch/<label>/MODEL.txt` (overwrite the old one: you are a
fresh agent).  The USER requires Opus 5.5 -- if your model id is anything else, write it there, STOP,
and report only that.

A RESUMED batch (s510f, s510c, s512p) is launched WITHOUT worktree isolation: your launch directory is
main's checkout `/home/bernt/pcl`, where you run NOTHING and write NOTHING outside
`dot-claude-in-home-dir/tasks/pcl/` (through `~/.claude/tasks/pcl/NNNN.json`, your own ID range).  Your
batch lives in an EXISTING worktree `$W` that your resume brief names; `$W/scratch/<label>/STOP.md` is
the state the previous agent left (a resume recipe at its top), and your resume brief says what has
changed since and what is RULED.  A NEW batch (s513a) is launched WITH isolation: `$W` is its launch
directory.  Every command is `env -C "$W" CMD`, `git -C "$W" ...`, or an absolute path under `$W` (your
cwd resets between bash calls).  16 cores, 12 GB RAM.  A session can be cut at any time -- keep STOP.md
current after EVERY step (a resume recipe at its TOP, the OWED list with what is done), commit finished
members as you go, and prefer finishing one owed thing over starting three.  BE ECONOMICAL: every long
agent of s510 ran out of its token budget at ~250-285k.  Read only what your brief names; do not re-read
logs you have already summarised into STOP.md; quote tails, not files.

MAIN = `416c710a` when this was written (read `git -C /home/bernt/pcl log --oneline -1` yourself); its last CODE commit is
**`8480f3f9`** (= s510b, merged in s512; merge tip `e4331e9a`) -- every commit above it is records / tasks / briefs.
Main's numbers: gen **v2-4480**; gate `Result: PASS` **282 files / 9,848 rows**; sweep GATE clean, TOTAL passing **18735**
(drops 5 = census); `EVERYDAY: 114 of 122 identical to perl (93.4 %)`; CI green on `416c710a`.  A `git -C "$W" rebase main`
from a tree already on `e4331e9a` crosses only docs / tasks / briefs (no code): expect no conflict; if `docs/DECIDED.md` or
`docs/session-log.md` conflict, keep both sides.  `corpus-diff` / `emission-ab` take **`8480f3f9`** as their reference
until a MAIN-READY file names a newer code sha.  A bar already on disk against the right sha that your resume brief says
STANDS keeps standing.

**CI's perl is 5.38.2** (this box: 5.40.3): a new gate row whose perl oracle needs perl >= 5.40 carries a PROBED
expected output.  Say in STOP.md that you checked your new rows.

Read through MAIN's checkout, never through your worktree's copy:
- `/home/bernt/pcl/briefs-and-rules-for-claude-subagents/` -- the rulebook `s473/COMMON.md` (every rule applies,
  INCLUDING its last section THE EVERYDAY NUMBER), this file, your original brief (still the definition of the batch)
  and this session's `s513/resume-<label>.md` or `s513/<label>-prompt.md` (what is owed NOW and what is RULED; where
  they differ, the newest wins).
- `/home/bernt/pcl/dot-claude-in-home-dir/` -- the task store and Claude's notes.  Never commit a change under
  `dot-claude-in-home-dir/` or `briefs-and-rules-for-claude-subagents/` in your branch.

## The batches and the MERGE ORDER
- **s510f** (worktree `/home/bernt/pcl/.claude/worktrees/agent-a9518b536c5798126`, HEAD `9bc09c51` = `e4331e9a` + 6;
  task IDs 2830-2849; runtime-only, no generation string; rebased across s510b and fully reviewed).  RUNS NOW; owes only
  its four legs + records; merges FIRST (`MAIN-READY-1`).
- **s513a** (NEW, with isolation; task IDs 2900-2919; generation **v2-4580** at its first emitting commit -- a gap above
  main's v2-4480 and below s510c's v2-4780..4783, so s510c never has to renumber across it).  #2860: a `$`-prototype slot
  must ALIAS a scalar lvalue argument (brief `s513/s513a-prompt.md`).  RUNS NOW beside s510f; merges SECOND
  (`MAIN-READY-2`).  It is the batch that ENDS the Text-Balanced 05_extmul.t loop s510c's board tripped over.
- **s510c** (worktree `/home/bernt/pcl/.claude/worktrees/agent-a0324b48405eea0b6`, HEAD `82163819` on code `02d531de`;
  task IDs 2860-2869 -- #2860 itself is s513a's now; generations v2-4780..4783).  Starts when a slot frees (after
  s510f merges); rebases across `MAIN-READY-1` and `-2`; merges THIRD (`MAIN-READY-3`).  Its board must read 0 NEW /
  0 LOST against a base extraction of main AFTER s513a (05_extmul.t then runs on both).
- **s512p** = perf round 41 (worktree `/home/bernt/pcl/.claude/worktrees/agent-ad4d28a21ff7f772a`, HEAD `9e3d66bf` WIP
  on `300305cd`; task IDs 2880-2899; runtime-only, v2-4880 only if it emits).  Starts when a slot frees after s510c
  has started; merges LAST.
When a batch is merged Fable writes `~/pcl-agent-scratch/s513/MAIN-READY-<N>` (sha + main's numbers + what crosses a
rebase) and tells the running agents; each rebases across it KEEPING BOTH sides (`cl/pcl-runtime.lisp`, `Pl/*.pm`, the
baselines, `docs/DECIDED.md`, `docs/session-log.md`, `docs/ir-spec.md`, `docs/not-supported.md`) and re-takes ONLY the
full gate, the sweep, corpus-diff (against the code sha the file names) and everyday (+ `--record`, last) -- plus the
board when the file says the merged batch moved it, and the final whole-table bench for a perf round.  s510f is
runtime-only, so a rebase across it changes no emission; s513a CHANGES EMISSION (prototyped-sub call sites), so a
rebase across it moves corpus-diff's reference and needs the three artifacts regenerated if
`Pl/t/artifact-staleness-01.t` says so.  Never wait for another batch.

## HEAVY LEGS SERIALIZE -- through ONE lock script (it replaces every process-name check)
At most ONE gate / sweep / companion / board / bench / everyday / gate-set-scan / whole-population emission-ab /
ir-conform / install-container run on the box at a time, Fable's merge legs included.  Every such run is started ONLY as

    env -C "$W" [VAR=value ...] ~/pcl-agent-scratch/s513/heavy.sh <label> leg   <logfile> CMD [ARG...]
    env -C "$W" [VAR=value ...] ~/pcl-agent-scratch/s513/heavy.sh <label> bench <logfile> CMD [ARG...]

in the BACKGROUND (your Bash tool's `run_in_background`; you are re-invoked when it exits).  The script takes the box's
one lock (`flock`), waits up to 60 minutes for it (exit status 75 = it never got the lock and did NOT run the command:
note it in STOP.md and start it again), runs CMD with stdin from `/dev/null` and stdout + stderr APPENDED to the log
between a START and an END line carrying `uptime`, and exits with CMD's status.  `bench` additionally waits for a
1-minute load below 2 before it starts.  `cat ~/pcl-agent-scratch/s513/HEAVY.holder` shows who holds the box (empty =
free).  A leg that is a pipeline or a sequence goes in a script FILE under `$W/scratch/<label>/` and that file is CMD.
**The s512 copy (`~/pcl-agent-scratch/s512/heavy.sh`) is RETIRED: it locks a different file.**  Every leg script of
yours that names it must be edited to the s513 path before use (`grep -l 's512/heavy' "$W"/scratch/<label>/*.sh
"$W"/scratch/<label>/legs/*.sh`; `perl -pi -e 's{s512/heavy}{s513/heavy}g' <those files>` -- they are plain files).
- Nothing greps process names: no `pgrep` waiters, no `until ! pgrep`; a leg is finished when its log has its END line.
- There is NO BENCH-WANTED file: a `bench` holds the lock for its whole length.  While HEAVY.holder says `bench` for
  ANOTHER label, keep your light work to ONE process at a time (no parallel proves, no `-j`).
- LIGHT (no lock): one `prove Pl/t/<file>`, one probe program, a single-file `tools/run-perl-suite.pl --jobs 1 <file>
  < /dev/null`, ONE `tools/run-dist-t.pl <dist> <one .t file>`, one `tools/rebuild-pack`, `tools/corpus-diff.pl`
  (~2 min, alone, not beside another light leg of yours), `tools/tag-license --check`, `tools/ir-host-leak.pl`.  While a
  heavy leg of yours waits or runs, do your light work -- do not idle, and never arm a Monitor and stop.
- One leg per lock: do not chain your whole bar list inside ONE heavy.sh call; queue the legs one after another.

The Edit tool on a `.lisp` file runs the project hook `.claude/hooks/format-lisp.sh`, which re-indents the WHOLE file.
Main's `cl/pcl-runtime.lisp` is in the hook's indentation, so an Edit should change only your lines -- verify after the
first one: `git diff --stat cl/` shows ONLY your lines, and `sbcl --script tools/check-parens.lisp cl/pcl-runtime.lisp`
says balanced.

## Records placement
`docs/DECIDED.md` is newest-first and opens with the Fable sections (`## s513`, `## s512`, `## s511`, ...), then the Opus
sections.  The Opus sections of this round go directly BELOW `## s510b` in MERGE order: `## s510f` below `## s510b`,
`## s513a` below `## s510f`, `## s510c` below `## s513a`, `## s512p` below `## s510c` -- a section whose predecessor is
not on main yet goes directly below the newest one that IS (the rebase then re-sorts nothing: keep both sides).
`docs/session-log.md`: the same rule for `## Session s510f` / `## Session s513a` / `## Session s510c` / `## Session
s512p` (directly below `## Session s510b`).  Never write or edit a Fable section.  DECIDED is an INDEX: one line per
ruling.

## Rules (the rulebook `s473/COMMON.md` has the rest)
NEVER run anything in `/home/bernt/pcl` itself, never touch main, never push, never merge, never `git stash`, never
`pcl --clear-cache`; no subagents; no `unless`; Perl for scripting (never python); `grep -a` on `.tsv`; quote shell
variables; never `nohup` the gate; never weaken or delete a test; never re-bless a baseline from a run (rows move BY
EDIT with their cause); task JSON through `JSON::PP->new->utf8` to a `:raw` handle, never a wide character through
`perl -pi`; `git status` in W must show ONLY `scratch/` untracked when you finish.
`MERGE-READY: <sha>` becomes the FIRST line of STOP.md only when every owed bar is done on the final rebased tree, and
never for a bar whose log is not on disk (verify each cited log's mtime is AFTER the last CODE commit it claims to
measure -- `git log -1 --format=%ci -- Pl cl lib tools pcl pl2cl`).  Count new gate rows by RUNNING the files.
`Pl/t/glob-01.t` rows 29-30 are the known #2384 flake -- re-run that file alone.  Use `date` for every time you write
down (never an estimate).  Fable REVIEWS your batch with probes (perl -> base -> your tree) while you work: a finding
reaches you as a message naming a probe file; fix it with a guard row, do not argue it in STOP.md.  If you have a
SendMessage tool, `MERGE-READY <sha>` goes to `main` as one line the moment STOP.md says it.

**ADDENDUM 2026-10-09 02:09 (Fable): s510c and s512p are MERGED (`MAIN-READY-3` = main `d90548c4`, `MAIN-READY-4` = main `3a925fa3`, code `33721af7`, gen v2-4784, gate 286 files / 9,986 rows, sweep 18,740, EVERYDAY 114 of 122).  The USER (02:09): "Please keep running two tasks."  RUNNING: s513b (the static-parsing batch; merges when ready) and NEW **s513c = PERF ROUND 42** (#2880 the undef-declared accumulator gets the str-buffer slot, then #2881 a read of a growing accumulator does not copy it; brief `s513/s513c-prompt.md`; WITH isolation; task IDs 2940-2959; gen v2-4984 only if it emits -- expected for member 1; its final whole-table bench on a 33721af7 base).  MERGE ORDER: s513b and s513c in the order they become ready; a merge of one reaches the other as `MAIN-READY-5` and a message (keep both sides; the later one renumbers its generation above main's).  Every rule above, the PROBE RULE included, applies to s513c as to a NEW batch.**

**ADDENDUM 2026-10-09 04:03 (Fable): s513b is MERGED (`MAIN-READY-5` = main `bc0189ba`, code `26712581`, gen v2-4884, gate 288 files / 10,010 rows, sweep 18,740, EVERYDAY 114 of 122).  RUNNING: s513c (perf round 42; merges when ready, member 2 held for the USER if its methret +12 % is a real cost) and NEW **s513d** = the class a static parse cannot know: #2870 the string eval's sub table (a real fix), then #2610's first LOG-only measurement over the four populations (brief `s513/s513d-prompt.md`; WITH isolation; task IDs 2960-2979; gen v2-5084).  MERGE ORDER: s513c and s513d in the order they become ready; a merge reaches the other as `MAIN-READY-6` and a message.  Every rule above, the PROBE RULE included, applies to s513d.**

**ADDENDUM 2026-10-09 04:54 (Fable): s513c is MERGED (`MAIN-READY-6` = main `b8885fab`, code `462169f6`, gen v2-4984, gate 289 files / 10,020 rows, sweep 18,740, EVERYDAY 114 of 122).  RUNNING: s513d (the eval sub table + the detector measurement) and NEW **s513e = PERF ROUND 43** (the split family, then s///e or byte-string lc by measured gain; brief `s513/s513e-prompt.md`; WITH isolation; task IDs 2980-2999; gen v2-5184 only if it emits).  MERGE ORDER: s513d and s513e in the order they become ready; a merge reaches the other as `MAIN-READY-7` and a message.  MAIN-READY-6 carries the BENCH-READING RULE for the saved-core layout artefact (a row moving 8-16 % with none of your code on its hot path: the dummy-defun test decides).  Every rule above, the PROBE RULE included, applies to s513e.**

**ADDENDUM 2026-10-09 08:29 (Fable): s513d is MERGED (`MAIN-READY-7` = main `11f4a100`, code `ec8ef671`, gen v2-5084, gate 291 files / 10,052 rows, sweep 18,740, EVERYDAY 114 of 122).  RUNNING: s513e only.  THE USER (08:29): "Don't start more subjobs." -- s513e is the LAST batch of this round; nothing is launched after it merges; `s513/s513f-prompt.md` is briefed but NOT launched.**

**ADDENDUM 2026-10-09 09:45 (Fable): s513e is MERGED (`MAIN-READY-8` = main `fbdda6ab`, code `f762553c`, gen v2-5084, gate 292 files / 10,060 rows, sweep 18,740, EVERYDAY 114 of 122).  THE s513 ROUND IS CLOSED: eight merges (s510f s513a s510c s512p s513b s513c s513d s513e), nothing running, nothing launched (USER).  Briefed and waiting for a word: `s513/s513f-prompt.md` (#2872 #2874 + fillers) and task #2878 (the facts overlay).**

**ADDENDUM 2026-10-09 23:15 (Fable): THE CAP IS THREE (USER 23:07: "Please continue. Run three jobs in parallel.").  The box rebooted at 23:06 -- HEAVY.holder empty, nothing running.  main = origin/main (`git -C /home/bernt/pcl log --oneline -1` at your launch; MAIN-READY-8's numbers still hold: code `f762553c`, gen v2-5084, gate 292 files / 10,060 rows, sweep 18,740, EVERYDAY 114 of 122; CI green on main).  THREE NEW batches, all WITH isolation, launched together: **s513f** (`s513/s513f-prompt.md`: #2872 signature-vs-prototype + #2874 `use bigint` + fillers; gen **v2-5184**; IDs 3000-3019), **s513g** (`s513/s513g-prompt.md`: #2878 the FACTS OVERLAY + the six overlays; gen **v2-5284**; IDs 3020-3039), **s513h** (`s513/s513h-prompt.md`: PERF ROUND 44 -- #2771's compile-time print arm, then the sub-call family; gen **v2-5384**, member 1 emits; IDs 3040-3059).  MERGE ORDER: in the order they become MERGE-READY; a merge reaches the others as `~/pcl-agent-scratch/s513/MAIN-READY-<N>` (N from 9) and a message; the perf round re-takes its final bench across whatever merges before it.  Every rule above applies, the PROBE RULE included.  NEW since 10:09 -- THE TIMED-ROW RULE (DECIDED ## s513; CI went red on an absolute 1.5 s bound at s513e's merge): a timed guard row's bound is RELATIVE to perl's own time on the same program measured in the run (`8 * perl + 0.5 s`), never an absolute number of seconds; CI's runner is ~3x slower than this box; the lever's inverse guard is a MECHANISM row, not the timed row.**

**ADDENDUM 2026-10-10 03:50 (Fable): s513f is MERGED (`MAIN-READY-9` = main `dbcf19dc`, code `e0b61a99`, gen v2-5184, gate 295 files / 10,091 rows, sweep 18,740, board 1,941, EVERYDAY 114 of 122).  RUNNING: s513g (rebasing onto MAIN-READY-9 after its companion), s513h (MERGE-READY 33b0554a reviewed: one review fix -- a macroexpansion-time fallback in `%p-cell-set` -- then rebase + re-take).  NEW, the third slot: **s513i = THE ENCODE SHIM** (USER 03:45: "Put the Encode shim high in priority"; task #2946; brief `s513/s513i-prompt.md`; WITH isolation; task IDs 3060-3079; NO generation -- a shim + two runtime primitives; the full sweep is mandatory).  MERGE ORDER: in the order they become MERGE-READY; a merge reaches the others as `MAIN-READY-<N>` (N from 10) and a message.  Every rule above applies, the PROBE RULE and the TIMED-ROW RULE included.**

**ADDENDUM 2026-10-10 09:52 (Fable, SESSION s514 -- the s513 scratch dir, lock, probe rule and MAIN-READY numbering CONTINUE here): THE CAP IS TWO (the default; the USER said only "Please continue").  The box rebooted at 09:48; HEAVY.holder empty, nothing running.  main = origin/main `bef84f86` (notes only above MAIN-READY-9: code `e0b61a99`, gen v2-5184, gate 295 files / 10,091 rows, sweep 18,740, board 1,941, EVERYDAY 114 of 122; CI green).  RUNNING: NEW **s513i = THE ENCODE SHIM** (`s513/s513i-prompt.md`; WITH isolation; IDs 3060-3079; no generation) and RESUMED **s513h** (perf round 44; `s513/resume-s513h.md`; WITHOUT isolation in `agent-aad35215869d98fab`; owes the arith dummy-defun reading, `everyday --record`, MERGE-READY).  **s513g is NOT running** (resumes from `s513/resume-s513g.md` when s513h's slot frees) -- so "if s513g merges first" does not apply to s513h, and s513i's rebases cross s513h (runtime-only, `cl/pcl-runtime.lisp` near `%p-cell-set` / the print entries) before anything else.  MERGE ORDER: readiness; a merge reaches the other as `~/pcl-agent-scratch/s513/MAIN-READY-<N>` (N from 10) and a message.  Every rule above applies, the PROBE RULE and the TIMED-ROW RULE included.  The s514 running notes: `s514/PAUSE-s514.md`.**

**ADDENDUM 2026-10-10 10:14 (Fable, s514): THE CAP IS THREE (USER 10:0x: "Run three jobs in parallel please.").  s513h is MERGED (`MAIN-READY-10` = main `8c171855`, code `4d2fc9f0`, gen v2-5184 unchanged, gate 296 files / 10,100 rows, sweep 18,740, board 1,941, EVERYDAY 114 of 122).  RUNNING: s513i (the Encode shim), s513g (the facts overlay, resumed, rebasing across MAIN-READY-10) and NEW **s514a = SHIM ROUND 2** (`s514/s514a-prompt.md`: Digest::MD5 + Digest::SHA pure-Perl shims first -- the everyday row `modules/Digest-SHA-MD5` -- then Storable in perl's binary format; fillers Sys::Hostname, File::Glob; WITH isolation; task IDs 3080-3099; NO generation; umbrella tasks #2947 #2948).  MERGE ORDER: readiness; a merge reaches the others as `MAIN-READY-<N>` (N from 11) and a message.  Every rule above applies, the PROBE RULE and the TIMED-ROW RULE included.**

**ADDENDUM 2026-10-10 11:14 (Fable, s514): THE CAP RETURNS TO TWO AFTER THE NEXT MERGE (USER: "After the next one finishes, please do two agents at a time").**  The three running batches (s513i, s513g, s514a) all finish; when the FIRST of them merges, NO replacement is launched, and from then on at most two run.  Also RULED by the USER: the pure-Perl digest speed (#2949) is NOTED ONLY -- no runtime digest primitive; XS is looked at after PCL is less buggy.  s514a: the ratio goes into docs/shipped-modules.md as planned, nothing more.

**ADDENDUM 2026-10-10 11:46 (Fable, s514): the 11:14 addendum overstated it -- the NUMBER OF SUBJOBS VARIES (the USER says when), it is NOT a hard rule; the DEFAULT is TWO; no running batch is ever stopped; after one of the three closes, keep TWO going as the default (a replacement is launched when the count drops BELOW two).**

**ADDENDUM 2026-10-10 14:44 (Fable, s514 afternoon): THE CAP IS THREE (USER: "Please continue. Run 3 subjobs to start.").  The box is idle (HEAVY.holder empty, load 0.6); main = origin/main `1b6849b7` = MAIN-READY-11 + notes (code `38b4192a`, gen v2-5284, gate 297 files / 10,110 rows, sweep 18,740, EVERYDAY 114 of 122).  RUNNING: RESUMED **s513i** (the Encode shim, `s514/resume-s513i.md`, WITHOUT isolation in `agent-abee999f28ae9ef84`), RESUMED **s514a** (Digest + Storable shims, `s514/resume-s514a.md`, WITHOUT isolation in `agent-a758d6e41f15d3c26`) and NEW **s514b = THE CORRECTNESS BATCH** (`s514/s514b-prompt.md`: #2953 empty list slice, #2943 buffer place twice, #3081 raw-slot deref argument, #3080 qualified global shadowed by a lexical; fillers #2954, #2951; WITH isolation; task IDs 3100-3119; gen **v2-5384** at its first emitting commit).  MERGE ORDER: readiness; a merge reaches the others as `MAIN-READY-<N>` (N from 12) and a message.  The 11:46 rule stands: default TWO, the count varies by the USER's word, no running batch is ever stopped; after one of the three closes nothing is launched unless the USER says so.  Every rule above applies, the PROBE RULE and the TIMED-ROW RULE included.**
