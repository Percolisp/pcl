# s507 shared box (2026-10-04 night, Fable; revised when s507d merged and s507p launched) -- TWO subjobs on the box, both on Opus 5.5: s507b (the fix batch) and s507p (perf round 39)

(Merged earlier in this session: s502e, s506f, and s507d -- the `pcl` documentation.  USER: "Please keep running
two subjobs", so a slot that frees is refilled.  This file is the template for the next session's protocol.)

FIRST ACTION, before anything else: write the EXACT model id you are running as (from your own system
prompt, e.g. `claude-opus-5-5`) into `$W/scratch/<label>/MODEL.txt`.  The USER requires Opus 5.5 — if
your model id is anything else, write it there, STOP, and report only that.

You run in a NEW git worktree `$W` that the launcher cut from an OLDER commit: `git -C "$W" rebase main`
first.  The harness confines you to that worktree for git and for file writes; reading elsewhere is
fine.  16 cores, 12 GB RAM.

MAIN = origin/main = `08de9e4f` (the last CODE commit) plus records-only commits on top.  It holds everything merged today: `tie` on arrays and hashes,
file-private cells, everyday singles 3 (4-arg `select`, `sysread`, list-valued `use constant`,
`set_prototype`, a blessed filehandle), and perl's command-line switches in `pcl`
(`tools/lib/PCLSwitches.pm`; a script's own `#!` line is honoured; `local` on a caret variable binds
the runtime's symbol).  Main's numbers: gen **v2-3980**; gate `Result: PASS` **278 files / 9,730 rows**; sweep GATE
clean, TOTAL passing **18731**; `EVERYDAY: 113 of 122 identical to perl (92.6 %)`.  `corpus-diff` / `emission-ab` take `08de9e4f` as
their reference; your BASE extraction for probes is
`git -C /home/bernt/pcl archive 08de9e4f | tar -x -C "$W/scratch/<label>/base"`.

Two directories are NEW in the repository since today and are read through MAIN's checkout, never
through your worktree's copy (which is as old as your base commit):
- `/home/bernt/pcl/briefs-and-rules-for-claude-subagents/` — the rulebook (`s473/COMMON.md`), this
  file, and the briefs (`s507/<label>-prompt.md`).  `~/pcl-agent-scratch/` now holds only measurement
  output and the review probes.
- `/home/bernt/pcl/dot-claude-in-home-dir/` — the task store and Claude's notes.  You read and write
  tasks through `~/.claude/tasks/pcl/NNNN.json` exactly as before (it is a symlink into main's copy;
  globs, `ls` and `grep -r` follow it, `find` needs a trailing slash).  Never commit a change under
  `dot-claude-in-home-dir/` or `briefs-and-rules-for-claude-subagents/` in your branch.

## The two batches and the MERGE ORDER
- **s507b** (silent-wrongs: #2688 first, then #2680 #2661 #2686 + #2492 #2687 + #2684 #2084(2); gen **v2-4080**; task
  IDs 2700-2719).  Brief: `briefs-and-rules-for-claude-subagents/s507/s507b-prompt.md`.  Touches
  `Pl/Parser2.pm` / the sub-body lowering, `cl/pcl-runtime.lisp` (`p-cast-$`, `p-split`, require, ARGV), `pcl`,
  `tools/lib/PCLSwitches.pm` (the `-F` workaround leaves), `lib/English.pm` perhaps, the baselines, the records.
  Its member 0 (#2688) is being taken to main AHEAD of the batch by Fable (a cherry-pick; its own rebase then drops it).
- **s507p** (perf round 39: #2539, #2637, #2111, #2115, the fibret flag; gen **v2-4180**; task IDs 2720-2739).
  Brief: `briefs-and-rules-for-claude-subagents/s507/s507p-prompt.md`.  Touches `cl/pcl-runtime.lisp` (the scalar
  store paths, the box constructor, `p-.=`, the in-memory filehandle, the tie census macro), `Pl/t/perf-levers-08.t`,
  `docs/faster-codegen-suggestions.md`, the records.  It needs the QUIET box (benches) more than anyone.
MERGE ORDER = READINESS ORDER.  When one is merged, Fable writes `~/pcl-agent-scratch/s507/MAIN-READY-3`
(sha + main's numbers) and the OTHER rebases across it KEEPING BOTH sides (`cl/pcl-runtime.lisp`, the baselines,
`docs/DECIDED.md`, `docs/session-log.md`, `docs/ir-spec.md`), renumbers its generation string above main's,
regenerates the three artifacts and re-takes ONLY the full gate, the sweep, corpus-diff and everyday (+ `--record`,
last) -- s507p also its final whole-table bench.  `~/pcl-agent-scratch/s507/MAIN-READY-2` describes main as it is
now (the documentation batch is in it: `docs/pcl-commands.md` and `Pl/t/pcl-doc-examples-01.t` are the user's
reference for `pcl` -- whoever changes what `pcl` prints keeps them true).  Never wait for the other batch.

## HEAVY LEGS SERIALIZE
At most ONE gate / sweep / companion / board / bench / everyday / gate-set-scan / whole-population
emission-ab run on the box at a time — Fable runs legs too (the merge legs).  Before every heavy leg:
`uptime` (load < 6) AND
  pgrep -af 'tools/[s]weep-perl-tests|tools/[p]rove-core|[p]rove -j|tools/[r]un-perl-suite|tools/[e]veryday-smoke|tools/[b]ench-exec|[c]pan-scoreboard|[g]ate-set-scan|[e]mission-ab'
(the bracket spelling keeps pgrep from matching its own pattern; match the TOOL's process, never a
shell wrapper's text — put the pattern in a script FILE so your own waiting shell does not match it).
If another leg is running: `sleep 60` in a BOUNDED loop inside ONE background command or a script (at
most 40 minutes, then note the wait in STOP.md and try again); never `until ! pgrep`; never arm a
Monitor and stop.  A single-file `tools/run-perl-suite.pl --jobs 1 <file> < /dev/null` or one
`prove Pl/t/<file>` is LIGHT.  While you wait, do your light work — do not idle.  Priority when two
want the box: Fable's merge legs, then whoever asked first; a BENCH-WANTED reservation (s507p's benches) is honoured by
everyone -- no heavy leg STARTS while it stands.  EVERY companion leg
and every gate runs `< /dev/null`.

THE BENCH RESERVATION: a bench is a MEASUREMENT; it needs load < 2 and NO other heavy leg, and `uptime`
printed beside every number.  When you are ready to bench, write one line `<label> <HH:MM>` to
`~/pcl-agent-scratch/s507/BENCH-WANTED` and wait for quiet; delete the file the moment the bench ends
(or after 45 minutes without quiet: delete it, note it in STOP.md, try again later).  If that file
exists for ANOTHER label and is younger than 45 minutes, do not START a heavy leg.  Fable reads it
before starting any leg of its own.

After a rebase across an emission change regenerate the three compiler-built artifacts
(`tools/rebuild-pack`; `./pl2cl --extension lib/mro.pm > cl/pcl-mro.lisp && tools/tag-license
cl/pcl-mro.lisp`; same for lib/warnings.pm → cl/pcl-warnings.lisp; `cl/pcl-uniprops.lisp` is data, never
regenerated) and `tools/ir-inventory.pl` if an export moved.

The Edit tool on a `.lisp` file runs the project hook `.claude/hooks/format-lisp.sh`, which re-indents
the WHOLE file.  Main's `cl/pcl-runtime.lisp` is in the hook's indentation since today, so an Edit
should change only your lines — verify after the first one: `git diff --stat cl/` shows ONLY your
lines, and `sbcl --script tools/check-parens.lisp cl/pcl-runtime.lisp` says balanced.

## Records placement
`docs/DECIDED.md` is newest-first and opens with the Fable sections `## s507` … `## s501`, then the Opus
sections `## s501t`, `## s504c`, `## s502e`, `## s506f`, `## s501q`, ….  `## s507d` (merged) sits directly below `## s506f`; `## s507b` and `## s507p` go
directly BELOW `## s507d` (above `## s501q`); when both are present the one that merged FIRST sits
above the other.  `docs/session-log.md`: the same rule for `## Session s507b` / `## Session s507p`
(directly below `## Session s507d`).  Never write or edit a Fable section.

## Rules (the rulebook `s473/COMMON.md` has the rest, INCLUDING its last section THE EVERYDAY NUMBER)
cwd RESETS between bash calls — `env -C "$W" CMD`, `git -C "$W" …`, absolute paths; NEVER run anything
in `/home/bernt/pcl` itself, never touch main, never push, never merge, never `git stash`, never
`pcl --clear-cache`; no subagents; no `unless`; Perl for scripting (never python); `grep -a` on `.tsv`;
quote shell variables; never `nohup` the gate; never weaken or delete a test; never re-bless a baseline
from a run (rows move BY EDIT with their cause); task JSON through `JSON::PP->new->utf8` to a `:raw`
handle, never a wide character through `perl -pi` (it re-encodes the whole file); `git status` in W
must show ONLY `scratch/` untracked when you finish.  Keep `$W/scratch/<label>/STOP.md` current after
EVERY step — the session can be cut off at any time.  `MERGE-READY: <sha>` becomes the FIRST line of
STOP.md only when every owed bar is done on the final rebased tree, and never for a bar whose log is
not on disk (verify each cited log's mtime is AFTER the last CODE commit it claims to measure —
`git log -1 --format=%ci -- Pl cl lib tools pcl pl2cl`).  Count new gate rows by RUNNING the files.
`Pl/t/glob-01.t` rows 29–30 are the known #2384 flake — re-run that file alone.
Fable REVIEWS your batch with probes (perl → base → your tree) while you work: a finding reaches you as
a message naming a probe file; fix it with a guard row, do not argue it in STOP.md.
