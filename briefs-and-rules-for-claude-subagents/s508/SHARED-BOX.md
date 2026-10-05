# s508 shared box (2026-10-05 night, Fable; revised when the USER raised the cap: "Run three parallel subjobs now, please") -- THREE subjobs on the box, all on Opus 5.5: s507p (perf round 39, resumed), s507c (what the switches work exposed, resumed) and s508a (silent wrongs and two everyday rows, new)

FIRST ACTION, before anything else: write the EXACT model id you are running as (from your own system
prompt, e.g. `claude-opus-5-5`) into `$W/scratch/<label>/MODEL.txt` (overwrite the old one: you are a
fresh agent).  The USER requires Opus 5.5 — if your model id is anything else, write it there, STOP,
and report only that.

A RESUMED batch (s507p, s507c) was launched WITHOUT worktree isolation: your launch directory is main's
checkout `/home/bernt/pcl`, where you run NOTHING and write NOTHING outside
`dot-claude-in-home-dir/tasks/pcl/` (through `~/.claude/tasks/pcl/NNNN.json`, your own ID range).  Your
batch lives in an EXISTING worktree `$W` that your resume brief names; `$W/scratch/<label>/STOP.md` is
the state the previous agent left, and your resume brief says what has changed since.  A NEW batch
(s508a) runs in a NEW git worktree `$W` that the launcher cut from an OLDER commit: `git -C "$W" rebase
main` first; the harness confines you to that worktree for git and for file writes (reading elsewhere
is fine).  For everyone: every command is `env -C "$W" CMD`, `git -C "$W" …`, or an absolute path under
`$W` (your cwd resets between bash calls).  16 cores, 12 GB RAM.

MAIN = origin/main = `16cf9642`; its last CODE commit is **`fee16466`** (records-only commits on top).
Main's numbers: gen **v2-4080**; gate `Result: PASS` **278 files / 9,740 rows**; sweep GATE clean, TOTAL
passing **18731** (647 failing, drops 5 = census); `EVERYDAY: 114 of 122 identical to perl (93.4 %)`.
Since both checkpoints main gained the fix batch s507b (`~/pcl-agent-scratch/s507/MAIN-READY-3` lists
what it changed: the sub-body lowering in `Pl/Parser2.pm` — an empty-valued body is `(p-return-empty)`;
a literal `require "./file"` is a run-time statement; `p-split`'s string pattern is a regex; `close ARGV`
resets `$.`; `local` on a tied scalar goes through the tie; `lib/English.pm` TIES the separator aliases
and `$|`; `tools/bench-exec.pl` has a new row `localvar`).  `corpus-diff` / `emission-ab` take `fee16466`
as their reference after your rebase; a BASE extraction for probes is
`git -C /home/bernt/pcl archive fee16466 | tar -x -C "$W/scratch/<label>/base-fee1"` (keep your older
extraction too where your STOP.md's measurements name it).

Read through MAIN's checkout, never through your worktree's copy:
- `/home/bernt/pcl/briefs-and-rules-for-claude-subagents/` — the rulebook `s473/COMMON.md` (every rule
  applies, INCLUDING its last section THE EVERYDAY NUMBER), this file, your original brief
  `s507/<label>-prompt.md` (still the definition of the batch) and your resume brief
  `s508/resume-<label>.md` (what is owed now; where the two differ, the resume brief wins).
- `/home/bernt/pcl/dot-claude-in-home-dir/` — the task store and Claude's notes.  Never commit a change
  under `dot-claude-in-home-dir/` or `briefs-and-rules-for-claude-subagents/` in your branch.

## The three batches and the MERGE ORDER
- **s507p** (perf round 39: #2539, #2637, #2111, #2115, the fibret flag; task IDs 2720–2739; worktree
  `/home/bernt/pcl/.claude/worktrees/agent-a55609aa75faa5780`, HEAD `fcfd7144`).  It needs the QUIET box
  more than anyone and goes FIRST on it: its whole-table bench (~80–90 min) starts as soon as it is
  ready.  Generation string: none needed while it stays runtime-only; if it emits: **v2-4180** when it
  merges before s507c, else the next free string above main's.
- **s507c** (#2690 #2692 #2704 #2691 #2693 done = PART ONE; then #2700 #2701, #2703 #2666 #2669, #2702
  #2689 = PART TWO; task IDs 2740–2759; worktree
  `/home/bernt/pcl/.claude/worktrees/agent-a5c8a09980aa19ae8`, HEAD `0229e0ec`; generation **v2-4280**).
  PART ONE is merged ALONE and FIRST (finished work goes live before new work starts): rebase, the
  review findings, then its bars as soon as the bench reservation is released; PART TWO starts on top
  of the merged main.
- **s508a** (silent wrongs and two everyday rows: #2681 `use constant` evaluates its value once, #2682 a
  filehandle's numeric identity, #1361 croak / carp name the caller's location, #2740 the exit status of
  a die after a `use`, #2764 stray SBCL lines for a caught die in a module; task IDs 2780–2799; a NEW
  worktree; generation **v2-4380**).  Brief: `s508/s508a-prompt.md`.  Touches `Pl/Parser.pm`'s constant
  lowering, the runtime's numification of a handle, `lib/Carp.pm` and `caller`, the require path.
MERGE ORDER = READINESS ORDER.  When one batch (or part) is merged, Fable writes
`~/pcl-agent-scratch/s508/MAIN-READY-<N>` (sha + main's numbers) and tells the other agents; the others
rebase across it KEEPING BOTH sides (`cl/pcl-runtime.lisp`, the baselines, `docs/DECIDED.md`,
`docs/session-log.md`, `docs/ir-spec.md`), renumbers its generation string above main's if it emits,
regenerates the three artifacts and re-takes ONLY the full gate, the sweep, corpus-diff and everyday
(+ `--record`, last) — s507p also its final whole-table bench.  Never wait for another batch.

## HEAVY LEGS SERIALIZE
At most ONE gate / sweep / companion / board / bench / everyday / gate-set-scan / whole-population
emission-ab / ir-conform run on the box at a time — Fable runs legs too (the merge legs).  Before every
heavy leg: `uptime` (load < 6) AND
  pgrep -af 'tools/[s]weep-perl-tests|tools/[p]rove-core|[p]rove -j|tools/[r]un-perl-suite|tools/[e]veryday-smoke|tools/[b]ench-exec|tools/[b]ench-multi|[c]pan-scoreboard|[g]ate-set-scan|[e]mission-ab|tools/[i]r-conform'
(the bracket spelling keeps pgrep from matching its own pattern; match the TOOL's process, never a
shell wrapper's text — put the pattern in a script FILE so your own waiting shell does not match it,
and never `pgrep -f` a pattern your own command line contains).  If another leg is running: `sleep 60`
in a BOUNDED loop inside ONE background command or a script (at most 40 minutes, then note the wait in
STOP.md and try again); never `until ! pgrep`; never arm a Monitor and stop.  A single-file
`tools/run-perl-suite.pl --jobs 1 <file> < /dev/null`, one `prove Pl/t/<file>`, one probe program, one
`tools/rebuild-pack` is LIGHT.  While you wait, do your light work — do not idle.  Priority when two
want the box: a standing BENCH-WANTED reservation, then Fable's merge legs, then whoever asked first.
EVERY companion leg and every gate runs `< /dev/null`.

THE BENCH RESERVATION: a bench is a MEASUREMENT; it needs load < 2 and NO other heavy leg, and `uptime`
printed beside every number.  When you are ready to bench, write one line `<label> <HH:MM> <expected
minutes>` to `~/pcl-agent-scratch/s508/BENCH-WANTED` and wait for quiet; delete the file the moment the
bench ends (or, when quiet does not come within 45 minutes: delete it, note it in STOP.md, try again
later).  While that file exists for ANOTHER label, do not START a heavy leg — and keep your light work
to ONE process at a time (no parallel proves, no `-j`), with THREE agents on the box: prefer reading,
designing and perl-side probing while a bench stands, and run PCL probes singly.  Fable reads it before starting any leg of its
own.  A whole-table bench is long: its reservation stands for its whole expected length, not 45 minutes.

After a rebase across an emission change regenerate the three compiler-built artifacts
(`tools/rebuild-pack`; `./pl2cl --extension lib/mro.pm > cl/pcl-mro.lisp && tools/tag-license
cl/pcl-mro.lisp`; same for lib/warnings.pm → cl/pcl-warnings.lisp; `cl/pcl-uniprops.lisp` is data, never
regenerated) and `tools/ir-inventory.pl` if an export moved.

The Edit tool on a `.lisp` file runs the project hook `.claude/hooks/format-lisp.sh`, which re-indents
the WHOLE file.  Main's `cl/pcl-runtime.lisp` is in the hook's indentation, so an Edit should change
only your lines — verify after the first one: `git diff --stat cl/` shows ONLY your lines, and
`sbcl --script tools/check-parens.lisp cl/pcl-runtime.lisp` says balanced.

## Records placement
`docs/DECIDED.md` is newest-first and opens with the Fable sections `## s507` … , then the Opus sections
`## s501t`, `## s504c`, `## s502e`, `## s506f`, `## s507d`, `## s507b`, `## s501q`, ….  `## s507c`, `## s507p`
and `## s508a` go directly BELOW `## s507b` (above `## s501q`); among them the one that merged FIRST
sits highest.  `docs/session-log.md`: the same rule for `## Session s507c` / `## Session s507p` /
`## Session s508a` (directly below `## Session s507b`).  Never write or edit a Fable section.

## Rules (the rulebook `s473/COMMON.md` has the rest)
NEVER run anything in `/home/bernt/pcl` itself, never touch main, never push, never merge, never
`git stash`, never `pcl --clear-cache`; no subagents; no `unless`; Perl for scripting (never python);
`grep -a` on `.tsv`; quote shell variables; never `nohup` the gate; never weaken or delete a test;
never re-bless a baseline from a run (rows move BY EDIT with their cause); task JSON through
`JSON::PP->new->utf8` to a `:raw` handle, never a wide character through `perl -pi` (it re-encodes the
whole file); `git status` in W must show ONLY `scratch/` untracked when you finish.  Keep
`$W/scratch/<label>/STOP.md` current after EVERY step — the session can be cut off at any time.
`MERGE-READY: <sha>` becomes the FIRST line of STOP.md only when every owed bar is done on the final
rebased tree, and never for a bar whose log is not on disk (verify each cited log's mtime is AFTER the
last CODE commit it claims to measure — `git log -1 --format=%ci -- Pl cl lib tools pcl pl2cl`).  Count
new gate rows by RUNNING the files.  `Pl/t/glob-01.t` rows 29–30 are the known #2384 flake — re-run
that file alone.  A brand-new tree's first companion runs can read a wrong child exit status (#2689):
your worktree is not new, but a fresh extraction is — run one companion file in it first.
Fable REVIEWS your batch with probes (perl → base → your tree) while you work: a finding reaches you
as a message naming a probe file; fix it with a guard row, do not argue it in STOP.md.
