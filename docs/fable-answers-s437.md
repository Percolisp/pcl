# Fable answers — s437 (2026-08-23): the s434 + s435 + s436 batch REVIEWED

The three Opus sessions since the s433 review, taken as ONE batch because
s435 and s436 are halves of the same work (Q2 shipped in s435, closed in
s436) and s434 is the instrument both were measured through:

* **s434** — Q1: the two instruments (#467 recovery load in the companion
  runner, #462 five census populations, the snapshot-hole report).
  `docs/opus5-review-requests-s434.md`, DECIDED §s434.
* **s435** — Q2: THE FLIP in the tree (a dropped statement DIES when reached),
  the `p-die` format-control fix, the cost model.  `…-s435.md`, DECIDED §s435.
* **s436** — Q2 closed (four bar legs), #471 (compiler-side memory cap), #457
  promoted + fixed (`)-name`), then **Q3 = the PHASE MODEL** (#456(b) + #469).
  `…-s436.md`, DECIDED §s436 + §s436b.

Verdict first, then what was re-run, then the asks, then what this session
changed (one review fix + two filed findings), then the queue.

## 0. VERDICT: all three sessions APPROVED as shipped

Nothing to revert, nothing to re-do.  Every claim I could re-measure
reproduced; the review fix (§3) is a pre-existing placement bug that ask 6 of
s436 asked about, not a regression of the batch.

| leg | s436 claimed | s437 re-measured (COLD cache, this tree) |
|---|---|---|
| gate `tools/prove-core` | 163 / 5739, only the 13 pclxs xs rows | **163 files / 5739 rows; failures = xs-01 ×5, xs-02 ×4, xs-03 ×4 and nothing else** |
| full sweep (`--jobs 8`, own gate) | TOTAL 18312, 0 new / 0 fixed, drops 5 = census, GATE clean | **TOTAL passing baseline 18312, current 18312 (+0); 0 new / 0 fixed; drops census 5 = current 5; GATE clean** (7 unstable / 10 unverified = the usual PARTIAL-file noise; state.t TRANSPILE_FAIL = the ruled given/when refusal) |
| companion `--all --quick --jobs 4` | NET +36 C_ok, nothing lost, six rows edited | see §1.4 |
| emission A/B, lib population vs `f332682` (pre-phase-model) | 22 DIFF: 21 pure permutation + 1 added `(in-package …)` | **re-run and re-checked with my own line-multiset walker: 21 pure permutation, 1 add-only (`IO/Handle.pm`: two `(in-package …)`), 0 other** |
| `docs/ppi-bug-report.t` §25 rows | bug row fails on 1.291, control passes | **row 30 FAILS (`Word[-length]`), row 31 passes**; PPI 1.291 |
| probes vs live perl | the s436 lists | 16 phase-model shapes, 12 `-name` shapes, 3 flip shapes (module / load-time call / string eval), 5 `#474` exposure shapes, 6 memory-cap shapes — §1 |

## 1. What was independently re-run, and what it found

### 1.1 The phase model (Q3) — 16 probes, one pre-existing finding

All of these agree with perl on HEAD: a later section's `use constant` read
by an earlier section's run code; a file lexical assigned in BEGIN and closed
over by a later section's sub; a later BEGIN writing `$main::v` that earlier
run code reads (`v=fromB`); CHECK/INIT in a later section before section 0's
run (`check init run`); END order across sections (`r e2 e1`);
`package Foo 1.5` read from section 0's run phase (`V=1.5`); `__PACKAGE__` in
a later BEGIN; `caller(0)` from a later section's sub called by section 0
(`main` — the section-0 `p-set-current-package` the caller.t finding forced
is doing its job); `sub f` redefined by a later `sub main::f` (perl 2, PCL 2 —
before, PCL printed 1); `eval "Foo::g()"` before `package Foo; sub g`; the
caller.t shape (`eval 'pb()'` after a foreign-package BEGIN → `ok ok`); a
BEGIN calling a file-scope sub before the `my` it bumps is initialised
(`c=0 c=1`); a package block with a closed-over lexical; a `use strict` +
cross-section `our %h` read through a qualified name.

**One DIFF, and it is PRE-EXISTING (identical on a `4356e77` worktree), filed
as #475**: a FILE-level `our` alias is not requalified across a TOP-level
`package` statement — `our $t = "m"; package A; $t .= "A"; package main;
print $t` prints `m` (perl `mA`); `our @o = (1); package A; print scalar @o`
prints 0 (perl 1).  The in-BLOCK spelling (`{ our @o; package A; push @o…}`)
is right (#239/#251).  Not the phase model's doing: the base tree gives the
same wrong answer, only in a different order.

### 1.2 `)-name` (#457) — 12 probes, one pre-existing finding

The repaired shapes all agree with perl (`$x-foo`, `"x"-foo`, `$a[0]-foo`,
`$#a-foo`, `$h{k}-foo`, `(7)-foo`, `qw(a b)-foo`, `@{$r}-foo`, `${$r}[0]-foo`,
`$o->v-v()`, `$o->v -1`, `$i++-foo`, `length($s)-foo`, `(length $s)-foo`) and
so do the negatives (`-foo => 1`, `$h{-b}`, `? -foo : -bar`, `map { -$_ }`,
`return -foo`, `print STDERR -foo`).  The base tree DROPPED the statement in
every positive case.

**One DIFF, PRE-EXISTING, filed as #476**: a BARE `-foo` where `sub foo` is
DECLARED — perl negates the CALL (`sub foo {8} print -foo` → `-8`), PCL
answers the string `-foo`.  It became visible only because #457 made the
surrounding statement run; the base dropped it whole.  Rare; filler.

### 1.3 The flip (Q2) — three shapes, all as ruled

A module with a drop in a sub body LOADS and the sub dies trappably with the
`file line N: text -- reason` message (the message carries the MODULE's path
and line, as it must: `DropMod.pm line 3`); a module whose top level CALLS the
sub dies at `use`, trapped by `eval "use …; 1"` (`ok=0`, program continues —
exactly the Text::Balanced shape, §2 ask 3); a string-eval drop still dies at
transpile with perl's `at (eval 1) line 1.` (#363).  The memory cap: 4 GB
reported by `PCL_SHOW_MEM_CAP` under `./pl2cl`, under `perl -I… pl2cl` (the
runner spelling) and on stdin; `PCL_NO_MEM_CAP` → `none`; a 16 MB cap kills
perl itself (exit 139); string eval through the capped `--server` works.

### 1.4 Companion `--all --quick --jobs 4` (the batch touched the harness loader and statement ordering)

**523 files, ZERO real movers.**  The runner's own #366 pass named four:
io/open.t (parallel 129/32, serial = snapshot — contention); uni/variables.t
(TIMEOUT, the known unstable flood, "rows unstable" printed); **io/pvbm.t 20/8
in the parallel AND the serial pass, 23/5 ALONE on the quiet machine** — the
FOURTH time that file has fooled the serial re-run (s435, s436 ×2, s437); and
**op/utf8cache.t DIFF 2/0 → TIMEOUT 2/0**, reproduced alone.  C_ok is
unchanged there; the STATUS moved because the #467 recovery load carries the
file past its first dying form into the test whose whole point is "this must
not go quadratic" — `"\x{100}" x 1000000` + `while ($x =~ /./g) { pos($x) }` —
and under PCL it DOES: 100k chars 4.1 s net, 200k 15.7 s (≈3.8× for 2×), perl
1M in 0.09 s; pos() and wide characters ruled out by probe, the MATCH is the
cost.  **Filed #477 (Target A)** with the discriminating measurements taken
and the next one named (sb-sprof on the ASCII shape).  The snapshot row is
edited by hand to the measured TIMEOUT with that cause.  Every one of s436's
six hand-edited rows (op/gmagic.t 30, op/gv.t 134, uni/gv.t 55, comp/hints.t
16, op/sort.t 182, op/cond.t) reproduces — they are not in the mover list.

### 1.5 s434's Ask 2, the six-file `--bless-rows` — I READ the rows

op/current_sub.t's 15 newly-blessed diverging rows are all `__SUB__` /
forward-declaration shapes behind its three aborted forms (the registered
reason); io/shm.t's 22 are all IPC::SysV (XS, the registered reason);
mro/basic.t and mro/basic_utf8.t SHRANK (rows now agree with perl);
next_skip ×2 gained `*summary*`.  The bless is right.  **Standing rule from
it**: a `--bless-rows` after an INSTRUMENT change is accompanied by one line
per file saying the new rows fall under the file's registered reason — a read,
not a count (it took ten minutes here).

## 2. The asks, ruled

### s434

**Ask 1 (the five never-refreshed snapshot rows): (b), narrowed by
measurement.**  A need-harness file that RUNS when named (op/lex.t gives 13/39)
joins the `--all` scan; one that does not gets a NOT-RUN row naming the rule
(the #345 shape) so the hole is COUNTED on every run instead of inferred.  The
file count may move 523 → 528; say so in the snapshot header.  Filler, with
the instruments.

**Ask 2 (six-file `--bless-rows`): RATIFIED** — §1.5, with the standing rule.

### s435 — no numbered asks; the in-session rulings are RATIFIED here

Accept the price, no narrowing; the loss unit is the top-level form and perl's
own is the program; a loss out of proportion PROMOTES the owner task; the
`p-die` `"~A"` fix; the fix shape for #471 at the `pl2cl` seam.  All stand.

### s436

**Ask 1 (the gate's in-process `Pl/t` loaders are uncapped): IN** — wrap the
`prove` in `tools/prove-core` in the same `systemd-run --user --scope -p
MemoryMax=…` the suite runner uses.  Tools-only filler.  Conditions: MEASURE
the gate's peak first (the scope's `memory.peak` after one run) and set the cap
at ≥3× it; when `systemd-run` is absent or the user bus is down, run uncapped
and SAY so on stderr (CI containers); `PCL_SHOW_MEM_CAP=1` prints the cap in
force, like `PCL_SHOW_SBCL`.  The ruled seam was "every compiler-side tool
spawns pl2cl"; the in-process loaders are the residue the ruling did not
cover, and an RSS scope is the only shape that covers them without touching
~40 test files.

**Ask 2 (#473): YES** — `cpan-tests/modules/**/t/*.t` becomes a census
population, PROGRAM mode (they are a dist's own test suite and run as
programs).  `t/japh`: one header sentence (it is excluded from the runner's
`@DEFAULT_DIRS` by design — not a measurement population).  Do #473 and the
#472 instrument (ask 5) together, FIRST in the next Opus session: the census
is the flip's price sheet and it is short 94 counted sites.

**Ask 3 (the second half of "a module with a drop still loads"): ir-spec
§9.3**, one sentence + the Text::Balanced example — the normative doc is
where the load-model rule lives; the census header points at it.  Wording:
*"A module whose drop sits in a sub body loads; its load-time code is a run
phase like any other, so a load-time CALL into that sub dies at `require`
time — trappable by the `use`'s own eval, as perl's compile error would be."*
(Verified live, §1.3.)  Done in this session's commit.

**Ask 4 (a promoted fix in the same session as the promotion): YES, under the
s366 filler-scope rule** — same mechanism (the fix was a sibling of
`_repair_glob_multiply` on the shared `_ends_term` oracle) + its own bar met in
that session (probes, four-population A/B, the board leg) + new axes filed.  A
promotion whose fix needs a NEW mechanism waits for its own session.  #457
qualified on all three; shipping a known 958-row hole on purpose would have
been the wrong reading of the rule.

**Ask 5 (#472, the fresh_perl/runperl CHILD population): the instrument is a
SIDE CHANNEL in the ONE announcer.**  `Pl::Parser::_announce_dropped_statement`
gains one arm: when `PCL_DROP_LOG` names a file, append
`FILE<TAB>LINE<TAB>TEXT<TAB>REASON` (the `_drop_site` tuple) to it — always,
regardless of the stderr gate, since the child's stderr IS the row's observed
output and must not change.  `sweep-perl-tests.pl` sets it per test file
(`.faillog/<name>.childdrops`) and prints a `child-drops: N` line per file and
a total; `sweep-diff` MEASURES first (a census of the sixth population, rows
by hand) and gates only after one blessed run.  ~20 lines compiler-side, no
emission change (no generation bump).  With #473, first in the next session.

**Ask 6 (`@ver_run` placement): it was WRONG, and it is fixed in this
session (§3).**  Measured before ruling: `package Foo 1.5; BEGIN { print
"V=[$Foo::VERSION]" }` prints `V=[1.5]` in perl and `V=[]` in PCL — on HEAD
(assignment at the END of the section's compile phase, after the BEGIN) and on
the base (front of the run phase, later still).  perl sets `$VERSION` as it
compiles the `package NAME VERSION` statement, i.e. BEFORE anything else in
the section, so the assignment belongs at the HEAD of the section's compile
phase, immediately after its own defvar.  One line moves; two `both_agree`
guard rows (statement form and block form).

**Ask 7 (#474, p-BEGIN never restores the package): FOLD** into whatever next
touches scheduled-block emission; it stays filed.  I probed five exposure
shapes (`use POSIX` after a foreign BEGIN; CHECK/INIT `__PACKAGE__`; `use
constant` + `caller`; `eval "sub { __PACKAGE__ }"`; `require` + `import`
inside BEGIN) and none diverges today — the run-group restatement closed every
measurable path.  The fix shape in the task (bind, don't setf) is right; it
is a `cl/` change, so the sweep is its gate when it happens.

**Ask 8 (the on-demand stub block is partly redundant): LEAVE IT.**  A
`p-declare-sub` no-op stub is harmless, it still serves an earlier section's
BEGIN, and narrowing it buys a diff class for nothing.  When the block is next
touched, its comment should say the remaining reason is the BEGIN case.

## 3. What this session changed

1. **Review fix — `package NAME VERSION` sets `$VERSION` at the HEAD of its
   section's compile phase** (`Pl::Parser2::parse`, the `@ver_run` list is
   gone: the assignment is pushed right after its defvar).  Guards: two
   `both_agree` rows in `Pl/t/decl-ordering-02.t` (19 → 21).  Emission moves
   only for files with that spelling: corpus-diff IDENTICAL over 111 files, lib
   A/B 22/22 SAME, and the one population file with the spelling
   (`t/comp/package_block.t`, found by grep over all four populations) SAME.  Generation
   **v2-181**, the three artifacts regenerated (stamp only).
2. **ir-spec §9.3** gains the load-time-call sentence (ask 3).
3. **Filed #475** (file-level `our` alias across a top-level `package`
   statement, silent wrong, pre-existing) and **#476** (`-NAME` with a
   declared sub, pre-existing).
4. **Filed #477** (scalar-context `m//g` loop is quadratic — §1.4), and the
   `op/utf8cache.t` snapshot row edited by hand to its measured TIMEOUT.

## 4. The queue after this review

`docs/plan-post-s433.md` stands; Q1–Q3 are DONE.  Next Opus session:

* **open with the two census instruments** — #473 (cpan `.t` population,
  program mode; japh header sentence) + #472 (`PCL_DROP_LOG` side channel +
  the sweep's `child-drops` line, MEASURE first) + s434 ask 1 (never-refreshed
  rows → NOT-RUN rows / joined scan); ~half a session, tools + one announcer
  arm;
* **then Q4** = #453 + #365 as planned (P3 of plan-post-s430: the two
  named-unary operand sites become one; the `()`-prototype bareword);
* Q5, Q6 as planned;
* **Q7 fillers, re-ordered by rows behind them**: the PROMOTED #463 item 2
  first (`++${"23::foo"}` — 18 op/universal.t rows behind ONE drop; the
  comp/parser.t family-6 rows have no owner and stay), then #464 → #466 →
  #465, #468, #470, the prove-core scope (ask 1), #474 when scheduled-block
  emission is next touched, #475, #476, and the former flip-gap list.

Fable: the next review when Q4 lands (the operand-site merge is the next
emission-wide change); #281 with the tag; post-v0.1 unchanged.
