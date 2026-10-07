# s512 shared box (2026-10-07 evening, Fable) -- the three batches checkpointed at the end of s510 are RESUMED, TWO at a time (the default cap; the USER said only "Please continue"), all on Opus 5.5: s510b (binmode in place + `$, = 0`) and s510f (the first run from text, #2702) now; s510c (the list-context fallback bind goes) when s510b is merged.  MERGE ORDER: s510b -> s510f -> s510c.

FIRST ACTION, before anything else: write the EXACT model id you are running as (from your own system
prompt, e.g. `claude-opus-5-5`) into `$W/scratch/<label>/MODEL.txt` (overwrite the old one: you are a
fresh agent).  The USER requires Opus 5.5 -- if your model id is anything else, write it there, STOP,
and report only that.

Every batch here is RESUMED and is launched WITHOUT worktree isolation: your launch directory is main's
checkout `/home/bernt/pcl`, where you run NOTHING and write NOTHING outside
`dot-claude-in-home-dir/tasks/pcl/` (through `~/.claude/tasks/pcl/NNNN.json`, your own ID range).  Your
batch lives in an EXISTING worktree `$W` that your resume brief names; `$W/scratch/<label>/STOP.md` is
the state the previous agent left (a resume recipe at its top), and your resume brief says what has
changed since and what is RULED.  Every command is `env -C "$W" CMD`, `git -C "$W" ...`, or an absolute
path under `$W` (your cwd resets between bash calls).  16 cores, 12 GB RAM.  A session can be cut at any
time -- keep STOP.md current after EVERY step (a resume recipe at its TOP, the OWED list with what is
done), commit finished members as you go, and prefer finishing one owed thing over starting three.
BE ECONOMICAL: every long agent of s510 ran out of its token budget at ~250-285k.  Read only what your
brief names; do not re-read logs you have already summarised into STOP.md; quote tails, not files.

MAIN = `e5a7ad37` + this session's briefs commit when this was written (read `git -C /home/bernt/pcl log --oneline -1`
yourself); its last CODE commit is **`3ea47a41`** = s510p (perf round 40), merged in s510 -- every commit above it is
records / tasks / briefs.  Main's numbers: gen **v2-4480**; gate `Result: PASS` **282 files / 9,840 rows**; sweep GATE
clean, TOTAL passing **18735** (drops 5 = census); `EVERYDAY: 114 of 122 identical to perl (93.4 %)`; CI green through
`d2f7e757`.  Your first `git -C "$W" rebase main` therefore crosses only docs / tasks / briefs (no code): expect no
conflict; if `docs/DECIDED.md` or `docs/session-log.md` conflict, keep both sides (yours stay where the records rule
below puts them).  `corpus-diff` / `emission-ab` take **`3ea47a41`** as their reference until a MAIN-READY file names a
newer code sha.  A bar already on disk against `3ea47a41` that your resume brief says STANDS keeps standing.

**CI's perl is 5.38.2** (this box: 5.40.3): a new gate row whose perl oracle needs perl >= 5.40 carries a PROBED
expected output (the s509 merge failed on GitHub only, on exactly that).  Say in STOP.md that you checked your new rows.

Read through MAIN's checkout, never through your worktree's copy:
- `/home/bernt/pcl/briefs-and-rules-for-claude-subagents/` -- the rulebook `s473/COMMON.md` (every rule applies,
  INCLUDING its last section THE EVERYDAY NUMBER), this file, your original brief (`s510/<label>-prompt.md` -- still
  the definition of the batch) and this session's `s512/resume-<label>.md` (what is owed NOW and what is RULED; where
  they differ, the newest wins).
- `/home/bernt/pcl/dot-claude-in-home-dir/` -- the task store and Claude's notes.  Never commit a change under
  `dot-claude-in-home-dir/` or `briefs-and-rules-for-claude-subagents/` in your branch.

## The batches and the MERGE ORDER
- **s510b** (worktree `/home/bernt/pcl/.claude/worktrees/agent-acb992c6718ea4a5d`, HEAD `3ae84709`; task IDs
  2850-2859, used to 2853; runtime-only, no generation string).  RUNS NOW and merges FIRST (smallest, fully reviewed).
- **s510f** (worktree `/home/bernt/pcl/.claude/worktrees/agent-a9518b536c5798126`, HEAD `4440871c`; task IDs
  2830-2849; runtime-only, no generation string).  RUNS NOW, merges SECOND.
- **s510c** (worktree `/home/bernt/pcl/.claude/worktrees/agent-a0324b48405eea0b6`, HEAD `23fb812b`, code `b702dccf`;
  task IDs 2860-2869; generations v2-4780..4783 -- ABOVE main's, and main cannot pass them this session, so no renumber).
  NOT running yet: it takes the slot s510b frees; it has an OPEN REGRESSION to fix first (its brief); merges LAST.
When a batch is merged Fable writes `~/pcl-agent-scratch/s512/MAIN-READY-<N>` (sha + main's numbers) and tells the
running agents; each rebases across it KEEPING BOTH sides (`cl/pcl-runtime.lisp`, the baselines, `docs/DECIDED.md`,
`docs/session-log.md`, `docs/ir-spec.md`, `docs/not-supported.md`) and re-takes ONLY the full gate, the sweep,
corpus-diff and everyday (+ `--record`, last).  s510b and s510f are both runtime-only, so neither rebase changes
emission: the three artifacts stay as they are unless `Pl/t/artifact-staleness-01.t` says otherwise.  Never wait for
another batch.

## HEAVY LEGS SERIALIZE -- through ONE lock script (since s509; it replaces every process-name check)
At most ONE gate / sweep / companion / board / bench / everyday / gate-set-scan / whole-population emission-ab /
ir-conform / install-container run on the box at a time, Fable's merge legs included.  Every such run is started ONLY as

    env -C "$W" [VAR=value ...] ~/pcl-agent-scratch/s512/heavy.sh <label> leg   <logfile> CMD [ARG...]
    env -C "$W" [VAR=value ...] ~/pcl-agent-scratch/s512/heavy.sh <label> bench <logfile> CMD [ARG...]

in the BACKGROUND (your Bash tool's `run_in_background`; you are re-invoked when it exits).  The script takes the box's
one lock (`flock`), waits up to 60 minutes for it (exit status 75 = it never got the lock and did NOT run the command:
note it in STOP.md and start it again), runs CMD with stdin from `/dev/null` and stdout + stderr APPENDED to the log
between a START and an END line carrying `uptime`, and exits with CMD's status.  `bench` additionally waits for a
1-minute load below 2 before it starts.  `cat ~/pcl-agent-scratch/s512/HEAVY.holder` shows who holds the box (empty =
free).  A leg that is a pipeline or a sequence goes in a script FILE under `$W/scratch/<label>/` and that file is CMD.
The s510 copy of the script (`~/pcl-agent-scratch/s510/heavy.sh`) is RETIRED: it locks a different file.  Your old
leg scripts that call it must be edited to the s512 path before use (`grep -l 's510/heavy' "$W"/scratch/<label>/*.sh`).
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
`docs/DECIDED.md` is newest-first and opens with the Fable sections (`## s511`, `## s510`, `## s509`, ...), then the Opus
sections.  `## s510b`, `## s510f` and `## s510c` go directly BELOW `## s510p`; among them the one that merged FIRST sits
highest (so s510b directly below s510p, s510f below s510b, s510c below s510f).  `docs/session-log.md`: the same rule for
`## Session s510b` / `## Session s510f` / `## Session s510c` (directly below `## Session s510p`).  Never write or edit a
Fable section.  DECIDED is an INDEX: one line per ruling.

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
reaches you as a message naming a probe file; fix it with a guard row, do not argue it in STOP.md.
