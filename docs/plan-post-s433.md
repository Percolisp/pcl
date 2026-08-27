# Plan after s433 (Fable, s433, 2026-08-22) — THE LIVE QUEUE after the s431/s432 review

*Supersedes `docs/plan-post-s430.md` §1 as the Opus queue (its §2 Fable
queue, §3 recipes and §4 USER decisions still stand — copy recipes from
there, do not re-derive).  Written at the end of s433, the session that
ruled the s431 flip re-census and the s432 #456 half (a)
(`docs/fable-answers-s433.md`).  Read CLAUDE.md's lookup order first
(DECIDED.md → not-supported.md → runbook → probe), then this.*

## 0. Where the project is (measured s433)

* main `a5e1609`, generation **v2-177**; cold gate **160 files / 5705 rows**
  (only the 13 pclxs xs rows fail — `~/pclxs` is at abi 8, the pin says 6;
  user-deferred, ignore); full sweep **TOTAL 18366, GATE clean, drops 5 =
  census**; drop census **27 files / 82 drops**, all 82 classified and PRICED
  (`docs/drop-census-s431-flip-gate.md`): exempt 39 / registered 7 /
  deliberate 1 / indirect object 4 / gap 31, every gap owned.
* **The flip has a SHAPE now (RULED s433, fable-answers-s433 §A.1): a
  perl-shaped, trappable RUN-TIME die at the drop site, one shape for every
  drop in every mode, no classifier.**  Its unit is the statement, not the
  file — so the 5300/3022/441-row prices of the transpile shape do not
  apply, the module-mode increment is dissolved (a module with a drop still
  loads), and the flip no longer waits for the 31 gaps.  It waits only for
  the two instruments (#467, #462) that let its price be measured.
* **#456 half (b) is RULED as perl's PHASE model across sections** (§A.4 of
  the answers doc; #469 is the general shape): compile-phase forms of every
  section before run-phase forms of any section.  The "hoist one
  definition" sketch is unsafe (symbol-macro cells).
* Release: the push week is **2026-08-24 (USER executes: force-push main,
  keep `snapshot-2026-05`)** → first CI run (`.github/workflows/ci.yml`, the
  #282 container half) → the **v0.1 tag, DECOUPLED from the flip** (DECIDED
  s425: tag after the first green CI run).  The flip is v0.2's headline.
  Nothing in Q1–Q3 is a precondition of the push or the tag.

## 1. Opus queue, in order

**s437 (Fable review of s434–s436, `docs/fable-answers-s437.md`): Q1, Q2 and Q3 are DONE and APPROVED.  The next Opus session OPENS with the census instruments — #473 (cpan `.t` population, program mode), #472 (`PCL_DROP_LOG` side channel + the sweep's `child-drops` line, measure first) and s434 ask 1 (the five never-refreshed rows → NOT-RUN rows or joined scan, by measurement) — then Q4 below.  Q7 is re-ordered: the PROMOTED #463 item 2 (`++${"23::foo"}`, 18 op/universal.t rows behind one drop) first, then #464 → #466 → #465; new fillers #475, #476, #477 and the prove-core MemoryMax scope (s436 ask 1, IN) join the Q7 list.**

**s438 (Opus) DID the census instruments: #473 (a sixth census population — `cpan-tests/modules/**/t/**/*.t`, 42 files / 83 drops, blessed), #472 (`PCL_DROP_LOG` — the seventh population measured for the first time: 241 drops in 98 files, TEN sites) and s434 ask 1 (the companion `--all` scan is 528 files, not 523; both SNAPSHOT holes now read zero).  No product change; six findings filed (#478–#483).  Record: DECIDED §s438, `docs/opus5-review-requests-s438.md`.  THE NEXT SESSION IS Q4 = #453 + #365 (P3 of plan-post-s430: the two named-unary operand sites become one; the `()`-prototype bareword).**

**s438b + s438c (Opus) DID Q4: #453 (the two named-unary operand sites become ONE — `is_named_unary` answers for a declared sub whose prototype is perl's named-unary class) and #365 (an imported `()`-prototype sub is a TERM — the prototype crosses the `use` on its own shape, not on the literal-`qw()` export scan).  Both emission-IDENTICAL over the four populations (951 files A/B, 0 DIFF, twice), so the guards ARE the bar: `Pl/t/user-unary-01.t` + `Pl/t/imported-term-01.t`, inverse-guarded.  Gate 165/5760; sweep TOTAL 18312 (+0) GATE clean; companion 528 files, ZERO real movers.  #484 filed (the #351 repair cannot see an IMPORTED term — the pre-merge runs after the repairs).  Record: DECIDED §s438b+c.  **NEXT IS Q5 (P4: #454, #435, #455)**, then Q6, then the Q7 fillers.**

**s438d + s438e + s438f (Opus) DID Q5 (P4): #454 (a signature PARAMETER shadows a same-named file lexical — the detector AND the rewriter both ask one helper), #455 (the signatures feature is in force on the pragma's OWN line; the repair is the BOUNDARY, not the text — corpus-diff proved a textual rule wrong against signatures.t), #435 (ONE `Pl::Parser::fragment_doc`, nine fragment re-parse sites routed through it).  All three emission-IDENTICAL over the four populations (951-file A/B each), so the guards are the bar; #454 also ran the gate-SET scan over both populations, IDENTICAL.  Gate 166/5771; sweep TOTAL 18312 (+0) GATE clean.  #485 + #486 filed (both pre-existing).  Record: DECIDED §s438d+e+f.  **NEXT IS Q6 (P5: #451 / #452 / #449 / #450)**, then the Q7 fillers.**

**s438g + s438h + s438i (Opus) DID three of Q6 (P5): #452 (a package-qualified bareword filehandle is a NAME — one predicate, two sites, both a load-time crash), #451 (a punctuation ARRAY element interpolates; the set is NOT the token repair's — `^` differs), #450 (a metacharacter-free glob pattern is ITSELF, and a pattern is perl's whitespace-separated LIST).  **#449 stays with #418 by its own ruling** — the CL-unsafe punctuation arrays need a pipe-quoted emission that does not exist yet, and keep dropping loudly.  Gate 166/5779; sweep TOTAL 18312 (+0) GATE clean.  Filed #487–#490.  Record: DECIDED §s438g+h+i.  **NEXT IS Q7 — the module fillers, #463 item 2 FIRST (18 op/universal.t rows behind one drop), then #464 → #466 → #465, #468, #470, and the new fillers #475–#490.**

**s439 (Fable) REVIEWED s438 → s438i: ALL APPROVED as shipped** (cold gate 166/5779, sweep TOTAL 18312 (+0) GATE clean, companion 528 files; `docs/fable-answers-s439.md`, DECIDED §s439).  One review fix (#455's `_signatures_enabled_at` now recognises `qw(signatures)` / `:5.36` / `no feature`).  Filed #491 (qualified handle NAME canonicalisation — the three spellings #452 left), #492 (s/// replacement side vs non-ASCII identifiers), #493 (pointer record), #494 (v0.1 doc refresh AT TAG TIME), #495 (ask 7 — bareword operand of a prototyped sub, #266's family), #496 (ask 10 — the SHAPES corpus).  **The twelve asks ruled** (answers §2): child-drop sites → own file gated on the SITE SET, blessed after #479's harness half; the companion runner gets `PCL_DROP_LOG` in that same session; #478 MEASURED, default = the `Test::`/`Test2::` name list goes (a BUDGET if the cost is real); #479 harness FIRST and ALONE; #365 over-import accepted as is; #484 = shape (a); #486 cheap filler; #485 PROMOTE; #489 accepted for v0.1 (not-supported entry); runbook §4b.  **NEXT IS Q7 as ordered below, then the s439 fillers in the order they pay: #479 + ask 2 (one session, one `--all --quick`) → #478 measured → #491 → #485 → #492 → #484(a) → #495 → #496; #494 at tag time.  THE RELEASE (answers §4): USER push → first CI run → #282 green → #494 → tag v0.1.0; #281 items 4/5 are post-v0.1 (recommendation).**

**s440 (Fable, 2026-08-23) — CI GREEN (#282 + #283 DONE; the tag waits only on #494) and Q7 under way with parallel Opus agents (the s421 pattern, round 3+4).**  Fable: #496 (the SHAPES corpus; its first run found #497 / #498 / #499, all three fixed the same session), #497 (a signature PARAMETER is a declaration to every rewriter), #498 (punctuation arrays runtime-owned, main:: everywhere), #499 (list-context glob stateful — every second call empty).  Agents: **A** = #463 items 2 + 1 (s441a+b) MERGED (#505–#507 filed), **B** = #466 + #464 (s441b) MERGED (#508–#513 filed), **C** = #465 + #468 (s441c) MERGED (#500–#504 filed); **D** = #500 + #501 + #503 (s442d, v2-200) — in flight.  **D MERGED too; final tree gen v2-201: gate 171/5846, sweep 18313 (+0) clean, companion blessed; #494 DONE; v0.1.0 TAGGED `29c2cf3`.**  Round 4 in flight: E #470 (v2-205), F #491+#495 (v2-210), G #485+#484+#492 (v2-215), H #516+#515+#511 (v2-220); then #502, I #508–#513, J #505–#507+#514+#517, K #463 items 3–5 + #479 compiler half + #478; #504; #486–#489.  Standing from s440: agents run NO sweeps — Fable runs ONE sweep + the companion legs over the merged batch, renumbers the generation ONCE on the final tree (above every agent string) and regenerates the artifacts; the review-doc families are gone (a review session writes into DECIDED + the session log + this plan).  **Next rounds, in the order they pay: E = #502 (English shim) + #484(a); F = #491 + #495 (the #266/#452 family); G = #485 (PROMOTE) + #492; H = #470 (sweep IS the gate + gate-SET, Fable-run on merge); then #479 harness half (Fable, alone: sweep + `--all --quick`) → #479 compiler half + #478 measured (Opus); #463 items 1 and 3–5 (glob-value family: measure the glob representation first).**

**s444 (Fable, 2026-08-24) — ROUND 4 DONE: all four agents restarted in their kept worktrees, finished, REVIEWED and ff-MERGED (E #470; G #485 + #484(a) + #492; H #516 + #515 + #511; F #491 + #495 shapes (a)+(c), (b) waits for #266 by measurement).**  Final tree `c42cc8a`, gen **v2-221** (F's post-rebase renumber adopted — above every agent string; artifacts regenerated on exactly this tree).  Legs all clean: cold gate 171/5924 (13 xs rows), sweep TOTAL 18313 (+0) GATE clean drops 5 = census child 9/6, gate-SET 638x2 ZERO diff, companion 528 files (subval + leaky-magic edited by cause, both s443f; pvbm/variables/utf8cache the known noise).  #518 fixed (`c76875a`, cached-core prefix + exact row; CI half rides the push).  Closed #470 #484 #485 #491 #492 #511 #515 #516; FILED **#519–#532**.  **Queue now: #502, I = #508 + #509 + #510 + #512 + #513, J = #505–#507 + #514 + #517, K = #463 items 3–5 + #479 compiler half + #478; #504; the #519–#532 fillers slot by what they block (#525 is one line + sweep; #527 is measured and shaped; #520 runtime half first).**  Record: session-log §444, DECIDED §s444.

**s444 evening (USER rulings before the break): Target A opened a crack —
#73 was PROFILED (sb-sprof: ~45% of a hot method loop was re-FINALIZING the
CLOS class per call under a lock; only ~10–15% the package lookup) and the
USER ruled CACHE-FREE FIRST.  The finalize-once guard SHIPPED (`81c17ea`,
both sites, 2.2× on the loop, ovlsub 7.27×→5.28×, sweep +0, companion
mro/class/method legs identical).  #73's REMAINDER JOINS THE NEXT AGENT
ROUND (USER): stash-in-box at bless → own-package fast path → pre-built
pl-NAME from codegen; a per-CLASS stash table (perl's own shape) only if
inherited dispatch still lags — NEVER a per-call-site cell.  The measured
breakdown and plan are in task #73.  #533 filed (p-super-call has no
UNIVERSAL fallback).  **The push HAPPENED the same evening (USER):
`e9cc3a7..0cede82`, 12 commits — CI runs on `0cede82` and its verdict is
TOMORROW's first check (`gh` is now installed on this machine; the old
"no gh here" note is obsolete).  A green run is the first of #518's
"twice in a row".**

**s445 (Fable, 2026-08-25) — CI on `0cede82` GREEN (first of #518's two);
the README-referenced docs refreshed to measured state and pushed
(`ad6553d`); ROUND 5 LAUNCHED off `ad6553d`** — five Opus agents in
worktrees: **I** = #508 + #509 + #510 + #512 + #513 (gen v2-230, new-task
IDs 540–549), **J** = #505 + #506 + #507 + #514 + #517 (v2-235, 550–559),
**K** = #463 items 3–5 + #479 compiler half + #478 (v2-240, 560–569),
**L** = #502 English shim + #504 (+#525 if room) (v2-245, 570–579),
**M** = #73 remainder cache-free + #533 (v2-250, 580–589; USER s444 routed
#73 to this round).  Standing rules as rounds 3–4: agents run no sweeps and
write no review-request docs; the merge review owes ONE full sweep +
companion legs (op/+io/ for I; mro/+class/+method for M; the sweep is
NON-OPTIONAL for L's lib/ change; gate-SET where a refusal/drop stops
firing — K), generation renumbered ONCE above v2-250 + artifacts
regenerated on the merged tree.

**s445 later (Fable, 2026-08-25) — ROUND 5 COMPLETE: all five agents
reviewed with independent probes and ff-MERGED; final tree `f95fa97`, gen
v2-290; every batch leg run and clean** (full sweep ×2 GATE clean TOTAL
18319 +4, both baselines edited row by row; gate-SET vs `ad6553d` = exactly
2 drop→OK lines; companion op/io/re/mro/class with NINE movers each
re-measured alone and spliced with causes; cold gate on main; narrative in
session-log §445).  Shipped: #73-remainder cache-free (2.62×/4.74× of
perl) + #533 (M); #502 English + #504 runpcl streams + #525 (L); #506 +
#507 + #514 + #517 + #505-part (J); #508 + #509 + #510 + #512 + #513 (I);
#463 items 3–5 + #479 both halves + #478 with the census 73/167 → 34/89
(K).  Closed also #572 (dup of #512).  **Queue now — the filler pool by
what it blocks: #543 (~6 lines, wants the next sweep), #535 (the
STDOUT-restore half #513 exposed — io/open.t's 9 rows sit behind it),
#534 (import/unimport empty-list family), #560 (the #478 cost mitigation:
disk-cached prototype facts — new cache family, Fable-sized), #561
(punct-magic glob alias empty slot — op/gv.t's 4 rows), #551 (numeric
symbolic-ref, needs a design ruling), #541 (false-conditioned local
write-through, needs a design ruling), then the older #519–#532 / #504
leftovers (#527 measured and shaped; #520 runtime half first) and
#540/#542/#550/#562–#565/#570–#576/#580–#581.  #582 post-v0.1.  The
remaining plan-post-s433 queue (Q7 fillers #457/#464–#466/#468/#470 — some
now overtaken by this round; re-check each against the task store before
scheduling) stands after those.
runs; every probe-found silent-wrong is FILED with its reproducer; a
review-request doc per session (`docs/opus5-review-requests-sNNN.md`) — write
one even if it says "no asks"; grep `Pl/t` for the message text of ANY
behaviour a fix removes (the s416 stale-guard rule); a filler is "same
mechanism + gate-SET measured + new axes filed" (s366); **a companion row
count is not comparable to a sweep row count for a change that makes
something die, until Q1 lands** (s433).

**s448 (Fable, 2026-08-27/28) — ROUND 6 COMPLETE: three Opus agents (N =
#543 + #535 + #529 io/dup + std descriptors; O = #565 + #562 + #571 + #573
caret magics; P = #570 + #527 + #534 compiler silent-wrongs) reviewed with
independent probes and ff-merged; final tree `866830c`, gen v2-321; every
batch leg clean** (sweep TOTAL 18321 (+2), gate-SET 638×2 ZERO, companion
nine movers all attributed row-level, cold gate 179/6054 xs-only; records in
DECIDED §s448 + session-log §448).  Filed this round: #590–#594 (N),
#600–#602 (O), #610–#612 (P), #620–#621 (Fable review).  `docs/
js-target-sketch.md` saved (fun/parked, #622).

**THE QUEUE FOR THE COMING SESSIONS (written s448):**

1. **Fable design session(s)** — the five rulings damming the pool:
   **#561** (where COMPUTED magics `$!`/`%!` live in the box model so glob
   aliases reach them; decides `%!` the errno hash — reg_namedcapture.t sits
   on it; O's #602 slot-clearing folds into the same design), **#551**
   (numeric symbolic-ref vs hard ref), **#541** (false-conditioned `local`
   write-through), **#542** (stdio buffering policy — isatty-dependent,
   global, own sweep price), **#560** (disk-cached prototype facts — design
   here, Opus builds).
2. **Opus round 7** (3 agents + merge review): *io residue* #591 + #621 +
   #590 (+#592 if the fcntl shape is settled); *scoping* **#593** (`my`
   before an in-block `package NAME;` loses the lexical — the io/open.t
   nondeterminism root cause; sweep IS the gate) + #594 + #530/#532 (the
   bareword-handle-in-expression family); *parser fillers* #563 + #564 +
   #550.
3. **Opus round 8**: the s/// replacement family #520 + #521 + #522 (one
   bundle); P's residue #610 + #620 + #611/#612; implementations of the
   step-1 rulings — #561/%! and #542 (each its own session-sized change,
   #542 with the full-sweep price).
4. **Then**: #560 BUILT (the compile-speed lever — the pre-scan is 4.3×
   intrinsic); Target-A residue #477 (quadratic pos) + #582 (per-class
   stash cache, only if inherited dispatch still lags); the census push
   toward zero (34 files / 88 drops, all owned; child-drop sites #483,
   #480–#482); re-check the old Q7 leftovers (#457, #464–#466, #468, #470)
   against the task store — several overtaken by rounds 5–6.
5. **Parked on standing rulings**: boxed aggregates (Fable design first),
   #221 warnings model, E5.3 `local`, indirect object (USER: maybe later),
   #622 JS target (fun).
6. **USER decision open: the v0.2 headline** — the flip shipped BEFORE the
   v0.1 tag, so v0.2's story is unclaimed; recommendation: "census to zero
   + compile speed (#560) + %!/errno completeness" = what items 1–4
   produce.

**Session Q1 — the two instruments: #467 + #462 (+ the missing-row report).  No product change.**  **DONE s434** (`docs/opus5-review-requests-s434.md`, DECIDED §s434): both runners load with recovery and the snapshot is re-blessed (+2325 C_ok, 0 rows lost); the census has five populations and the blessed file gained the 12 module rows (39 files / 102 drops); the snapshot hole is printed from BOTH sides.  **Q2 is unblocked.**

1. **#467**: `tools/run-perl-suite.pl` loads the emitted CL with
   `pcl::p-load-with-recovery` like the sweep does (one-line change at the
   sbcl command, ~line 601; the runner already loads `cl/pcl-test.lisp`,
   where the function lives).  Then RE-BLESS `baselines/perl-suite-run.tsv` in
   ONE measured pass (`--all`, not `--quick`, once — the files that died
   mid-way are exactly the ones that change) and land the re-bless in the
   SAME commit, every moved file explained in the header block (expected:
   files that died mid-way report MORE rows; a file that reports FEWER rows
   is a finding, not a re-bless).  Write the rule into
   `docs/test-infrastructure.md`: both runners load with recovery; a
   form-abort is counted and printed; users (runpcl) are plain load.
   `PCL_SHOW_SBCL=1` before/after diff of the spawned command (the runner
   row of the WHAT-TO-RUN table).
2. **The missing-row report** (s431 Ask 4): the runner prints, at the end of
   every run, the suite files that have NO snapshot row (count + names) as a
   named discrepancy.  The five s431 rows were found by counting; the next
   ones must not need counting.
3. **#462**: `tools/drop-census.pl` gains the `cpan-tests/modules`
   population (each `.pm` transpiled with `--module`, so the emission is the
   one the runtime caches) and the 14-dist board's `lib/` behind a flag
   (`~/.cpan/build` is outside the repo); prints the module population
   separately; the blessed census gains those rows with their causes (the
   s431 doc §5 table is the measurement: 3 board modules / 5 drops, 9
   cpan-tests modules / 15).  `tools/corpus-diff.pl`'s SILENT-DROP counter
   stays perl-tests-only (per-change instrument); the census is the
   baseline.
   Bar for Q1: gate; the runner's own before/after on a handful of files
   incl. one that dies mid-way (op/method.t, op/sort.t are the s432 cases);
   the census tool's row total equals the blessed TOTAL line.

**Session Q2 — THE FLIP, in the ruled shape (fable-answers-s433 §A.1).**

4. In `Pl/Parser.pm`'s two `PARSE ERROR` emitters
   (`_parse_expression_internal`, `_parse_expression_form`) the comment stays
   byte-identical and the `nil` becomes
   `(pcl:p-die "PCL: statement not supported at FILE line N: <text> -- <reason>\n")`.
   ONE helper builds the message for both the stderr announcement and the
   emitted die (factor it out of `_announce_dropped_statement`; rule 11);
   the source text goes through the CLForm string escaper.  The
   announcement, `#363`'s eval-string die, `--module`'s silence and the
   Track A `_ruled_refusal_for_drop` transpile refusals all stay as they
   are.  `not-supported.md` §Lvalue subroutines: the s400 §6.3 sentence
   becomes "dies when the statement runs (trappable)".  `docs/ir-spec.md`
   load-model: what the drop form means at run time.  Generation bump + the
   three artifacts.  Guard: a `Pl/t` row of the `$@` form on a shape that
   stays a drop (`eval { f() = 7 }; like $@, qr/statement not supported
   at/`), plus the inverse that a file with no drop is byte-identical.
   BAR (all legs, in this order): `corpus-diff` — diffs in exactly the four
   perl-tests census files, one line each, SILENT-DROP count unchanged;
   four-population A/B — diffs = exactly the 27 census files + the 12 module
   files, every one the same one-line shape; full sweep TOTAL/LOST per file
   — a lost row is accepted only as a row AFTER the dying statement in its
   top-level form or a row that ran ON the dropped statement's `nil` (the
   accidental-pass kind), edited into `pass-baseline` with that cause,
   anything else is a finding; companion `--all --quick` on the Q1 runner
   with the A/B attribution recipe (plan-post-s430 §3); the board re-run for
   the three dists with drops (Text::Balanced's rows will move where line
   118/397 is reached — that is #457's price; explain per row); gate; DECIDED
   line.  Then update `baselines/parse-error-drop-census-s399.tsv`'s header note:
   the census now counts LOUD-AT-RUN drops.

**Session Q3 — #456 half (b) = #469, the PHASE model across sections (fable-answers-s433 §B.4).**

5. Measure first: what the five section lists hold (`Pl/Parser2.pm`
   ~1770–1830 assembly + the "Cross-section forward sub calls" block
   ~1845); how many files are multi-section; whether any `sched` form is
   ENTITLED to an earlier section's run-time state (perl: no — probe
   `our $x = 5; { package Q; sub q1 {1} } BEGIN { print "B=[$main::x]" }`,
   perl `B=[]`, PCL `B=[5]`); compile time (MOVE, never copy — nothing is
   compiled twice).  Then emit every section's compile-phase forms (decls,
   captured, defs, sched — source order kept) before any section's
   run-phase forms, each group under its own `(in-package …)` switches; the
   on-demand stub block becomes unnecessary for the cases it served (keep
   it only if a def still needs an earlier-section reader — measure).
   NEVER the def alone: a body above its section's decls compiles a
   `p-defcell` symbol-macro as a free variable (probed s433).
   Bar: the #456 reproducer + the BEGIN probe + the inverses
   (single-section BEGIN, same-section call, sub-first, no-package-switch)
   vs perl; four-population A/B with every diff explained (expected class:
   ordering only); full sweep TOTAL/LOST; companion on the Q1 runner; gate;
   generation bump + artifacts; ir-spec load-model line; sort.t's bug-36430
   row returns to `pass-baseline` by EDIT; the two `decl-ordering-02.t`
   guard rows become one `both_agree`; #456 + #469 close together.  If the
   two-phase A/B shows a diff class the session cannot explain, STOP and
   bring the classes as an ask — the fallback is the on-demand hoist of the
   needed section's decls+defs.

**Session Q4 — P3 of plan-post-s430: #453 + #365** (the two named-unary
operand sites become ONE; an imported `()`-prototype sub is a bareword
string in operator positions).  Text and bar unchanged from plan-post-s430
§1 items 4–5 (the s429 16×10 matrix; four-population A/B; gate-SET scan
both populations).

**Session Q5 — P4: #454 (signature param vs a later file-level `my` →
false capture REFUSAL; scoping ⇒ the sweep IS the gate), #435 (the `"$Ｘ[$ｉ]"`
fragment re-parse skips the #410 repair), #455 (low value, if room).**

**Session Q6 — P5: #451 / #452 / #449 / #450** (punctuation-array
interpolation twin; `<main::FH2>` bare symbol; the CL-unsafe punctuation
arrays — decide loudly; `glob("/nope")` returns the pattern — `cl/` ⇒ sweep).

**Session Q7 — the module fillers, by rows behind them: #457 → #464 → #466
→ #465**, then **#468** (the never-declared plain call routes through
`%p-call-of-undefined-sub`: measure where the program's dynamic extent is
established before choosing handler-vs-stub-emission), then **#470** (the
identity-promoted file lexical aliases its package variable; scoping ⇒ the
sweep IS the gate + gate-SET scan), then the former flip-gap list as
ordinary fillers (#458, #460, #461, #463, #415 rows) — after Q2 they are
loud at run time, not blockers.

**The release gate runs independently of Q1–Q3** — push (USER, 2026-08-24)
→ first CI run (#282's container half; fix what the runner finds —
`tools/t/install-pcl.t` is the local rehearsal) → the v0.1 tag.  #359 stays
behind the release; #221 (warnings model) first post-release; #409 (server
RSS) and #326 (the hang set) are infrastructure items for a session with
nothing rows-shaped.

## 2. Fable queue

1. Rule the asks as they come (Q1–Q7 each end with a review-request doc);
   the Q2 flip and the Q3 phase model each get a review session with the
   full bar independently re-run (they are the two emission-wide changes).
2. **#281** (v0.1 IR pass, in_progress) — finish with the tag.
3. Post-v0.1: boxed aggregates (design — do not start before), #221, E5.3
   `local`, the #399 scalar-invocant spelling if the USER re-raises it.

## 3. Recipes — see `docs/plan-post-s430.md` §3 (unchanged).

## 4. USER decisions open

None new.  Standing: the push week (2026-08-24, USER executes); the tag
after the first green CI run (DECIDED s425); indirect object MAYBE LATER
(USER s425).

### 4a. The push — ONE checkout, and the exact commands (s439c, 2026-08-23)

**`~/pcl` is the repository of record and the only checkout that needs to
exist.**  `~/testgit/pcl` was a second clone of the OLD public history
(tip `54b2aa4` = today's `origin/main`) with nine uncommitted files — all
nine byte-identical to `~/pcl` at `74cf2b1` (2026-07-04 21:50), so it holds
nothing `~/pcl` does not.  Its one useful property — the SSH remote
(`git@github.com:Percolisp/pcl.git`, the key that authenticates as
Percolisp/pcl) — was moved onto `~/pcl` in s439c (`git remote set-url`;
`git fetch` verified).  `~/testgit/pcl` can be removed.

The two histories share `059c0cb` and DIVERGE there: `origin/main` has 69
summary commits on top of it, local `main` 1270 detailed ones.  `~/pcl`'s
branch `snapshot-2026-05` IS `54b2aa4`, the old public tip, so pushing it
keeps the old history reachable on GitHub after the force-push.  Repo size:
pack 13.8 MiB, 1237 files, three fixture tarballs (cpan-tests/modules, 0.5 MB).

```bash
cd ~/pcl
git fetch origin                                        # origin/main must still be 54b2aa4
git push origin snapshot-2026-05                        # old public history stays reachable as a branch
git push --force-with-lease=main:origin/main origin main   # the rewrite becomes main; refuses if origin moved
git push origin R1                                      # the R1 tag (optional)
```

**PUSHED 2026-08-23 (s439c, on the USER's instruction, a day ahead of the
planned week)**: `snapshot-2026-05` created on GitHub at `54b2aa4`; `main`
force-updated `54b2aa4` → `917e1a9` (with `--force-with-lease`, origin
verified unchanged first); tag `R1` pushed.  Pre-push scan: no secrets in
the tracked tree, no blob over 2 MB in the whole history.  The first CI run
started on the push: https://github.com/Percolisp/pcl/actions/runs/32648385694
(#283 / #282's container half).  **CI RESULT: RED (run 32648385694, conclusion failure).**  Steps 1-5 GREEN
(apt PPI/Moo, sbcl.org 2.6.0 binary, Quicklisp+cl-ppcre); step 6
"Fresh-machine install (tools/install-pcl)" FAILED in ~3 s with exit code 2 —
BEFORE the core build (too fast for it), and the three later steps skipped.
The log is not readable without repo-admin auth (`403 Must have admin
rights`), so the exact error is not yet in hand.  NOT reproduced locally: the
installer PASSES with system perl + apt PPI 1.277 + a fresh Quicklisp in a
sanitized HOME (measured s439c), so the blocker is CI-image-specific.  Prime
suspect = the sbcl.org TARBALL binary + its `SBCL_HOME=$HOME/sbcl/lib/sbcl`
(a fresh Quicklisp `.sbclrc` may not make cl-ppcre visible to the runtime's
`asdf:load-system` under that binary, which the core build needs) — the exact
reproduction (install the sbcl.org 2.6.0 binary, not the debian one) was the
next step, interrupted for a break.  #494 stays behind a green run.

**s440 (2026-08-23): CAUSE FOUND AND FIXED — not SBCL.**  The installed `pl2cl`
died at compile: `Can't locate Data/Dump.pm` — three `Pl/*.pm` files imported
the non-core module for debug dumps, which every dev perl here has and a stock
runner lacks (reproduced on a bare perl 5.38.2 + `cpanm PPI Moo`; the
sbcl.org + Quicklisp setup PASSES).  Removed (core `Data::Dumper` helpers);
PPI ≥ 1.291 enforced by the installer and installed by CI with `cpanm`
(apt's is 1.277); CPAN test fixtures (Data::Dump, Try::Tiny) skip-guarded and
installed by CI; guard `Pl/t/core-deps-01.t`; `tools/ci-step` turns a failing
step's tail into a public annotation.  Stock-machine gate 166/5780 green
(bar the local xs rows); DECIDED §s440, session-log s440.  **THE SECOND CI RUN IS GREEN**: run 32650698636 on `7e6d1eb` (2026-08-23) — every
step, including the full gate (`tools/prove-core`) on the stock runner.  That is
**#282 (the fresh-machine property) DONE and #283 (CI) DONE**; the v0.1 tag now
waits only on #494 (the doc refresh on the tagged tree).  Watch a run without
`gh`: `curl -s https://api.github.com/repos/Percolisp/pcl/actions/runs?per_page=3`.

Then: fix what CI finds, #494's doc refresh on the tagged tree, tag
`v0.1.0` (`git tag -a v0.1.0 -m ... && git push origin v0.1.0`).
