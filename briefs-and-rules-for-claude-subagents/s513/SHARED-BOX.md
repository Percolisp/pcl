# s513 shared box (2026-10-08 evening, Fable) -- the three s512 checkpoints are RESUMED and ONE new small batch (s513a, #2860) is added, TWO agents at a time (the default cap; the USER said only "Please continue").  MERGE ORDER: s510f -> s513a -> s510c -> s512p (a perf round takes its FINAL whole-table bench after the last merge it crosses).

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
