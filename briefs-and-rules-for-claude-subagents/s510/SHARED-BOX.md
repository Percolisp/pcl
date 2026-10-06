# s510 shared box (2026-10-06 evening, Fable) -- the three batches checkpointed at the end of s509 are RESUMED, TWO at a time (the default cap), all on Opus 5.5: s507c PART TWO (what the switches work exposed) and s508a (silent wrongs) now, s507p (perf round 39) when PART TWO is merged; a low-priority analysis job (#2775) after them

FIRST ACTION, before anything else: write the EXACT model id you are running as (from your own system
prompt, e.g. `claude-opus-5-5`) into `$W/scratch/<label>/MODEL.txt` (overwrite the old one: you are a
fresh agent).  The USER requires Opus 5.5 — if your model id is anything else, write it there, STOP,
and report only that.

Every batch here is RESUMED and was launched WITHOUT worktree isolation: your launch directory is main's
checkout `/home/bernt/pcl`, where you run NOTHING and write NOTHING outside
`dot-claude-in-home-dir/tasks/pcl/` (through `~/.claude/tasks/pcl/NNNN.json`, your own ID range).  Your
batch lives in an EXISTING worktree `$W` that your resume brief names; `$W/scratch/<label>/STOP.md` is
the state the previous agent left (a resume recipe at its top), and your resume brief says what has
changed since and what is RULED.  Every command is `env -C "$W" CMD`, `git -C "$W" …`, or an absolute
path under `$W` (your cwd resets between bash calls).  16 cores, 12 GB RAM.  A session can be cut at any
time — keep STOP.md current after EVERY step (a resume recipe at its TOP), commit finished members as
you go, and prefer finishing one owed thing over starting three.

MAIN = `bd1b9c9f` + this session's briefs commit when this was written (read `git -C /home/bernt/pcl log --oneline -1` yourself); its last
CODE commit is **`ce6aa437`** = s507c PART ONE, merged in s509 (the commits above it are a test-file fix
`145ab739`, briefs, tasks, records and docs).  Main's numbers: gen **v2-4280**; gate `Result: PASS`
**279 files / 9,770 rows**; sweep GATE clean, TOTAL passing **18731** (647 failing, drops 5 = census);
`EVERYDAY: 114 of 122 identical to perl (93.4 %)`.  A batch that already rebased across `82e2cab3`
(s507c, s507p) crosses only docs / tasks / briefs in its first `git -C "$W" rebase main`; s508a crosses
PART ONE's CODE (its resume brief says how).  Keep both sides if `docs/DECIDED.md` or
`docs/session-log.md` conflict with the Fable sections (yours stay where the records rule below puts
them); re-run `Pl/t/pcl-doc-examples-01.t` after a rebase (it reads the README).  `corpus-diff` /
`emission-ab` take **`ce6aa437`** as their reference (a bar already on disk against `fee16466` that
your resume brief says STANDS keeps standing).

**CI's perl is 5.38.2** (this box: 5.40.3): a new gate row whose perl oracle needs perl >= 5.40
(`use v5.40`, a `:5.40` feature bundle, a 5.40-only builtin) carries a PROBED expected output — the
s509 merge failed on GitHub only, on exactly that.  Say in STOP.md that you checked your new rows.

Read through MAIN's checkout, never through your worktree's copy:
- `/home/bernt/pcl/briefs-and-rules-for-claude-subagents/` — the rulebook `s473/COMMON.md` (every rule
  applies, INCLUDING its last section THE EVERYDAY NUMBER), this file, your original brief
  (`s507/<label>-prompt.md` or `s508/s508a-prompt.md` — still the definition of the batch), last
  session's resume brief where one exists (`s508/resume-<label>.md`, `s509/resume-<label>.md`) and this session's
  `s510/resume-<label>.md` (what is owed NOW and what is RULED; where they differ, the newest wins).
- `/home/bernt/pcl/dot-claude-in-home-dir/` — the task store and Claude's notes.  Never commit a change
  under `dot-claude-in-home-dir/` or `briefs-and-rules-for-claude-subagents/` in your branch.

## The batches and the MERGE ORDER
- **s507c PART TWO** (worktree `/home/bernt/pcl/.claude/worktrees/agent-a5c8a09980aa19ae8`, HEAD `81860ac2`;
  task IDs 2740–2759; generation **v2-4281**).  Committed and reviewed; a few bars and the records are
  owed.  RUNS NOW and merges FIRST.
- **s508a** (worktree `/home/bernt/pcl/.claude/worktrees/agent-a587431876e18282b`, HEAD `cfbbcd34`; task
  IDs 2780–2799, used to 2781; generation **v2-4380**).  RUNS NOW.
- **s507p** (worktree `/home/bernt/pcl/.claude/worktrees/agent-a55609aa75faa5780`, HEAD `54560f98`; task
  IDs 2720–2739, used to 2723; runtime-only, no generation string).  NOT running yet: it is launched
  when PART TWO is merged, so that its final whole-table bench is taken ONCE, on the rebased tree.
- **a2775** (analysis only, no code change, low priority; brief `s510/analysis-2775-prompt.md`): takes a
  slot after the three above.
MERGE ORDER = READINESS ORDER (s507c PART TWO first).  When a batch is merged, Fable writes
`~/pcl-agent-scratch/s510/MAIN-READY-<N>` (sha + main's numbers) and tells the running agents; each
rebases across it KEEPING BOTH sides (`cl/pcl-runtime.lisp`, the baselines, `docs/DECIDED.md`,
`docs/session-log.md`, `docs/ir-spec.md`), renumbers its generation string above main's if it emits,
regenerates the three artifacts and re-takes ONLY the full gate, the sweep, corpus-diff and everyday
(+ `--record`, last) — s507p also its final whole-table bench.  Never wait for another batch.

## HEAVY LEGS SERIALIZE — through ONE lock script (since s509; it replaces every process-name check)
At most ONE gate / sweep / companion / board / bench / everyday / gate-set-scan / whole-population
emission-ab / ir-conform run on the box at a time, Fable's merge legs included.  Every such run is
started ONLY as

    env -C "$W" [VAR=value …] ~/pcl-agent-scratch/s510/heavy.sh <label> leg   <logfile> CMD [ARG…]
    env -C "$W" [VAR=value …] ~/pcl-agent-scratch/s510/heavy.sh <label> bench <logfile> CMD [ARG…]

in the BACKGROUND (your Bash tool's `run_in_background`; you are re-invoked when it exits).  The script
takes the box's one lock (`flock`), waits up to 60 minutes for it (exit status 75 = it never got the
lock and did NOT run the command: note it in STOP.md and start it again), runs CMD with stdin from
`/dev/null` and stdout + stderr APPENDED to the log between a START and an END line carrying `uptime`,
and exits with CMD's status.  `bench` additionally waits for a 1-minute load below 2 before it starts.
`cat ~/pcl-agent-scratch/s510/HEAVY.holder` shows who holds the box (empty = free).  A leg that is a
pipeline or a sequence goes in a script FILE under `$W/scratch/<label>/` and that file is CMD.
- Your old waiters (`waitheavy.pl`, `wait-quiet.sh`, `busy2.sh`, `run-heavy.pl`, the pgrep loops inside
  `bars.sh` / `leg6.sh` / `chain6.sh`) are RETIRED: twice in s508 a waiter matched its own command line
  and blocked its batch, and once a leg slipped into the gap of a poll.  Do not call them; if one of
  your leg scripts waits by itself, delete that line and run each of its heavy tools through heavy.sh.
- There is NO BENCH-WANTED file this session: a `bench` holds the lock for its whole length.  While
  HEAVY.holder says `bench` for ANOTHER label, keep your light work to ONE process at a time (no
  parallel proves, no `-j`).
- LIGHT (no lock): one `prove Pl/t/<file>`, one probe program, a single-file
  `tools/run-perl-suite.pl --jobs 1 <file> < /dev/null`, one `tools/rebuild-pack`, `tools/corpus-diff.pl`
  (~2 min, but run it alone, not beside another light leg of yours), `tools/tag-license --check`,
  `tools/ir-host-leak.pl`.  While a heavy leg of yours waits or runs, do your light work — do not idle,
  and never arm a Monitor and stop.
- One leg per lock: do not chain your whole bar list inside ONE heavy.sh call (it would hold the box
  for an hour and starve the other batch and the merge); queue the legs one after another instead —
  the lock is fair enough at this size.

After a rebase across an emission change regenerate the three compiler-built artifacts
(`tools/rebuild-pack`; `./pl2cl --extension lib/mro.pm > cl/pcl-mro.lisp && tools/tag-license
cl/pcl-mro.lisp`; same for lib/warnings.pm → cl/pcl-warnings.lisp; `cl/pcl-uniprops.lisp` is data, never
regenerated) and `tools/ir-inventory.pl` if an export moved.

The Edit tool on a `.lisp` file runs the project hook `.claude/hooks/format-lisp.sh`, which re-indents
the WHOLE file.  Main's `cl/pcl-runtime.lisp` is in the hook's indentation, so an Edit should change
only your lines — verify after the first one: `git diff --stat cl/` shows ONLY your lines, and
`sbcl --script tools/check-parens.lisp cl/pcl-runtime.lisp` says balanced.

## Records placement
`docs/DECIDED.md` is newest-first and opens with the Fable sections (`## s509`, `## s508`, `## s507`, …), then the
Opus sections `## s501t`, `## s504c`, `## s502e`, `## s506f`, `## s507d`, `## s507b`, `## s501q`, ….
`## s507c`, `## s507p` and `## s508a` go directly BELOW `## s507b` (above `## s501q`); among them the one
that merged FIRST sits highest.  `docs/session-log.md`: the same rule for `## Session s507c` /
`## Session s507p` / `## Session s508a` (directly below `## Session s507b`).  Never write or edit a
Fable section.

## Rules (the rulebook `s473/COMMON.md` has the rest)
NEVER run anything in `/home/bernt/pcl` itself, never touch main, never push, never merge, never
`git stash`, never `pcl --clear-cache`; no subagents; no `unless`; Perl for scripting (never python);
`grep -a` on `.tsv`; quote shell variables; never `nohup` the gate; never weaken or delete a test;
never re-bless a baseline from a run (rows move BY EDIT with their cause); task JSON through
`JSON::PP->new->utf8` to a `:raw` handle, never a wide character through `perl -pi` (it re-encodes the
whole file); `git status` in W must show ONLY `scratch/` untracked when you finish.
`MERGE-READY: <sha>` becomes the FIRST line of STOP.md only when every owed bar is done on the final
rebased tree, and never for a bar whose log is not on disk (verify each cited log's mtime is AFTER the
last CODE commit it claims to measure — `git log -1 --format=%ci -- Pl cl lib tools pcl pl2cl`).  Count
new gate rows by RUNNING the files.  `Pl/t/glob-01.t` rows 29–30 are the known #2384 flake — re-run
that file alone.  Use `date` for every time you write down (never an estimate).
Fable REVIEWS your batch with probes (perl → base → your tree) while you work: a finding reaches you
as a message naming a probe file; fix it with a guard row, do not argue it in STOP.md.
