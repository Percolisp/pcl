# s507 — RESUME s502e (everyday singles 3: #2537 #2538 #2056 #2559 #2084(1)) — all five members are built; what is left is the final rebase and the bars

You are an EXECUTION agent (Opus 5.5) on the PCL project (Perl -> Common Lisp transpiler).  You RESUME a
checkpointed batch in its EXISTING worktree:

    W=/home/bernt/pcl/.claude/worktrees/agent-aefe0849c7b133e5f      (HEAD 2a698dc9, 9 commits on 618db500, tree clean)

Your launch cwd may be a different, fresh worktree — IGNORE it; all work happens in `$W` (`env -C "$W" CMD`,
`git -C "$W" …`, absolute paths; never run anything in `/home/bernt/pcl` itself).

Read, in this order, IN FULL:
1. `~/pcl-agent-scratch/s507/SHARED-BOX.md` — the protocol of THIS session (its FIRST ACTION: your model
   id into `$W/scratch/s502e/MODEL.txt`).  It REPLACES `s506/SHARED-BOX.md`.
2. `$W/scratch/s502e/STOP.md` — the predecessor's state: members, measurements, the OWED list.
3. `~/pcl-agent-scratch/s506/s502e-prompt.md` — the original brief: the bars and the FINAL REPORT
   format (still in force; where it says `s506/…` for SHARED-BOX, BENCH-WANTED or MAIN-READY read
   `s507/…`; where it says `e5d72352` as the reference read `2e71d4aa`).
4. `~/pcl-agent-scratch/s473/COMMON.md`.

## The work, in order
1. `git -C "$W" rebase main` onto `2e71d4aa` (618db500 + s504c #2633 + one docs commit; numbers in
   SHARED-BOX).  Conflicts: the generation string (take **v2-3880**), the three artifacts (take either
   side, they are regenerated), `baselines/perl-suite-run.tsv` / `perl-suite-fails.tsv` (KEEP BOTH: s504c
   spliced the eight re/regexp.t wrappers; yours are op/sselect.t rows).
2. Regenerate the three compiler-built artifacts; `prove Pl/t/artifact-staleness-01.t`; commit.
3. New base extraction `$W/scratch/s502e/base-2e71d4aa`; re-run ALL your probes (`scratch/s502e/p/*.pl`)
   perl → that base → your tree, and `prove Pl/t/transpile-test-10.t` on both (rows 75–85 must still FAIL
   on the base: s504c changed how a promoted file lexical is named in non-program units — if a row's
   verdict on the base changed, say which and why).  Seam with s504c: a list-valued `use constant` and
   `set_prototype` inside a MODULE (`use`d file) and inside an `eval STRING` — one probe each vs perl.
4. **The #2056 bench RE-TAKEN on a quiet box** (the predecessor's was at load 0.9–3 — FLAGGED): the
   BENCH-WANTED reservation, load < 2, `fhread fhprint textproc` A/B vs the `2e71d4aa` base, interleaved,
   `BENCH_K=5`, a control pair, `uptime` beside every number.  FLAG anything > 1 % outside the control band.
   Do this EARLY (before the other agent's long legs start), right after step 2.
5. `tools/corpus-diff.pl 2e71d4aa` (READ the SILENT-DROP line; expected: readline.t + scalar.t =
   `(p-scalar-ctx (p-select …))`, plus nothing else) + `tools/emission-ab.pl --ref 2e71d4aa --shapes --list lib/**/*.pm`.
6. The FULL sweep `--jobs 4 < /dev/null` (you rebased across a name-resolution change: the sweep IS the
   gate) — GATE clean, TOTAL vs 18728, movers explained, baselines row by row.
7. The FULL gate `PCLXS_DIR=~/pclxs tools/prove-core < /dev/null` — it has NEVER been run on this batch.
   Expect `Result: PASS`, 276 files, 9,576 + your 11 rows (say the arithmetic).  `prove --timer
   Pl/t/transpile-test-10.t` — its wall time vs the slowest gate file; if your 11 rows (two with
   `alarm 30`) make it the slowest, move them to `transpile-test-11.t`.
8. `tools/everyday-smoke.pl --jobs 2`: BEFORE 110 (main's last history row) → AFTER expected 113;
   NEW 0 / UNEXPLAINED 0 / STALE 0.
9. `tools/ir-conform --jobs 2`, `tools/ir-host-leak.pl`, `tools/tag-license --check`,
   `sbcl --script tools/check-parens.lisp cl/pcl-runtime.lisp`.
10. Records (`scratch/s502e/insert-records.pl` — fix its placement rule to SHARED-BOX's: directly below
    `## s504c` / `## Session s504c`), commit; `--record` from the clean committed tree, commit; MERGE-READY.
Before the gate / sweep / everyday / records (steps 6–10) read `ls ~/pcl-agent-scratch/s507/MAIN-READY`
and main's sha: if the other batch (s506f, perl switches: `pcl`, `pl2cl`, `tools/lib/PCLSwitches.pm`,
`tools/pclperl-for-tests`, `perl-tests/t/test.pl`, `cl/pcl-runtime.lisp` `$^W` / `${^TAINT}` /
`${^UNICODE}`, sweep baselines for reset.t / split.t / ref.t) merged first, rebase across it KEEPING
BOTH sides and take the next free generation string.  It is NOT expected to: you are expected to merge
first — do not wait for it.

## Things the reviewing model will check (do them before it asks)
- #2537: `use constant X => (1 + 2)` (scalar 3), `use constant E => ()` (empty; `scalar(E)` 0;
  `my @a = (E, 1)` has 1 element), a one-value constant unchanged, `L + 1` still a term, a list constant
  in BOOLEAN context (`if (L)`), as a hash-slice index (`@h{(L)}`), in `scalar(@{[L]})`, `(L)[1]`,
  `L->[0]`-style misuse left as perl leaves it, and the `use constant { A => 1, B => 2 }` hash form
  unchanged — each vs perl.
- #2538: argument order `(\&code, $proto)`; returns the code ref; `undef` clears; the prototype is
  visible to `prototype(\&f)` afterwards; Sub::Util's `set_prototype($proto, \&code)` still its own order.
- #2056: `*p-handle-stash*` — is it weak (a handle that is closed and dropped must not stay in the
  table for the life of the image)?  If it is a strong table say how entries leave; if they never do,
  that is a leak to FLAG with its size per handle, or to fix with a weak table (`:weakness :key`).
- #2559: rule 12 — the select error arm DIES naming the condition, never answers 0; EINTR (a signal
  during select) answers -1 with `$!` set as perl; `select(undef,undef,undef,0.25)` still sleeps.
- Net runtime line count for the batch (`git diff --stat 2e71d4aa..HEAD -- cl/pcl-runtime.lisp`).  The ~60
  re-indented lines in the tie section (`%p-when-tied-hash` / `%p-when-tied-array` bodies) are the project's
  format hook normalising s501t's code: ACCEPTED (Fable, s507) — but PROVE they are whitespace-only:
  `git diff -w 2e71d4aa..HEAD -- cl/pcl-runtime.lisp` must show only your semantic hunks, and no changed
  line may sit inside a string literal (a docstring line's leading blanks are DATA).

Final report: the format in the original brief's last section, SHORT.  `MERGE-READY: <sha>` as the
first line of `$W/scratch/s502e/STOP.md`, or `NOT MERGE-READY` + the exact reason.

## FABLE'S REVIEW PROBES — already run on your checkpoint `2a698dc9` (perl → base `2e71d4aa` → tree)
Files `~/pcl-agent-scratch/s507/review/e*.pl`, outputs `review/out/*.perl` / `.base` / `.tree1`; re-run
them on your FINAL tree: `perl ~/pcl-agent-scratch/s507/review/run.pl "$W/pcl" final` and diff each
`out/NAME.final` against `out/NAME.perl` — the differences must be exactly the ones listed here.
- **e1-const** (56 rows): identical to perl EXCEPT rows 04 / 44 (`(E, 1)` has 2 elements: an empty
  constant is one undef element in a list — PRE-EXISTING, the general empty-sub bug, FILED **#2680**,
  not yours) and rows 31 / 32 (`use constant R => 1 .. 5`, `=> map {…}`: a SINGLE list-yielding
  expression is still compiled in scalar context — PRE-EXISTING, FILED **#2681** together with "the
  value is re-evaluated at every use").  Your comma-list arm is ACCEPTED as shipped.  In your records
  say plainly what #2537 covers (a comma list / qw of >= 2 elements) and name #2680 + #2681 as the
  residue; append one line to task #2537's DONE section pointing at them.  Do NOT widen the fix.
- **e2-proto** (14 rows): identical EXCEPT row 09 — **YOURS, fix it**: perl's `set_prototype` DIES
  for a non-reference (`set_prototype: not a reference`) and for a reference that is not CODE
  (`set_prototype: not a subroutine reference`); your shim answers quietly (e10-fhnum.pl rows 07 show
  all three).  Two lines in `lib/Scalar/Util.pm` (no `unless`), one guard row, inverse-verified.
  Probe Sub::Util's `set_prototype` under perl for the same three arguments and mirror what it does.
- **e3-ftemp** (32 rows): identical EXCEPT row 26 (`$o == $o2` for two different blessed handles is
  true: a filehandle numifies to 0 — PRE-EXISTING, FILED **#2682**, not yours).
- **e4-select** (32 rows): identical EXCEPT rows 21 / 24 / 25 (1-arg `select` returns the unqualified
  name of a bareword-selected handle — PRE-EXISTING, FILED **#2683**, not yours).
- **e5-memo** (12 rows): identical to perl.
- e6 … e10 are the classification probes behind the four filed tasks; nothing to do with them.
