---
name: project_v2_session_state
description: Narrative state of the PCL v2 compiler work — where the current task stands and what the next session should pick up
metadata: 
  node_type: memory
  type: project
  originSessionId: 0cfb3270-d22a-4912-972f-cbf0f09c97d5
  modified: 2026-08-13T21:34:08.258Z
---

**s395 (2026-08-15, Opus 5) — Fable's s394 queue worked in order; 4 commits,
+2730 companion-suite rows across 14 files.**  Review request:
`docs/opus5-review-requests-s395.md`.  Gate **140/5168** (only the ignored
pclxs xs rows), corpus-diff IDENTICAL at every step, cache gen bumped
**v2-145**, full sweep run (verdict in the review doc).  #314's F-B and F-A2
closed plus its biggest single; #320/#319/#317/#316 all closed.

- **F-B** `our $count++` — same shape as F-A1; the gate demanded an ASSIGNMENT
  operator.  op/repeat.t 0 -> 47, op/inccode.t compiles (-> #321).
- **F-A2** attributed declarations — PPI spells them Operator(':') + Words in a
  Statement::Variable, so `my $x : shared = 1` matched the `my VAR <tail>`
  shape and printed EMPTY.  ONE pre-pass strips both decorations.  op/attrs.t
  0 -> 28, uni/attrs.t 0 -> 8; the protocol itself is #322.
- **`@{+}` is the variable `@+`** — the 2513-row re/pat_rt_report.t was held by
  four assertions; 0 -> 2431.  `$#-`/`$#+` were the second half.  #324 covers
  the `(?{ })` stack blowout that still truncates its last ~82 rows.
- **Fillers**: version::is_strict/is_lax (packagev 5 -> 198), plan's args
  flatten through p-flatten-args (select 0 -> 3), glob case both halves
  (uni/parser 17 -> 23), capture_warnings + the two XDIFF registrations.

**NEXT**: #314's residue re-grouped — REFALIASING is ~1400 rows across four
files (re/opt.t, op/const-optree.t, op/lvref.t, op/decl-refs.t) and looks cheap
in PCL's model (a scalar IS a box, so aliasing = sharing the box object); then
F-D spanning / F-E our-shadows-my / F-F state; op/for-many.t is its own feature
(perl 5.36 n-at-a-time foreach, with ALIASING loop variables).  run/runenv.t is
blocked on #318.  Two asks for Fable in the review doc: #323's scheduling, and
whether refaliasing jumps the queue.


**s394 (2026-08-15, Fable) — s393 review DONE: all six commits APPROVED,
both asks RULED.**  Cold gate **139/5148** (only the 8 user-ignored pclxs xs
rows; user s394: pclxs under separate work, ignore XS), gen still v2-144
(bump skipped WITH justification — corpus-diff identical, no cacheable input
moved), sweep TOTAL 18535.  Rulings: `docs/fable-answers-s393.md`; DECIDED
s394 section added.

**Ask 3 (F-A1 absorbed collector fix): IN SCOPE** — boundary: absorb only a
second consumer answering the SAME question about the SAME shape via ONE
predicate; a different question = its own commit.  **Ask 4: REGISTER**
script_run + regex_sets' engine half as XDIFF + not-supported (owners
#196/#71; PCRE2/#71 LIFTS script_run, `(?[ … ])` is perl-only and survives);
capture_warnings test.pl-stub gap fixed FIRST (mechanics: task #320).

**Filed s394**: #318 (`my VAR, <tail>;` — tail reads FRESH binding, perl
reads OLD; probed both sigils; pre-existing, no population hit, unscheduled),
#319 (`version::is_strict` shim — op/packagev.t −17), #320 (registrations).
Also fixed leaked tool-call syntax in #316's task JSON.

**Opus queue next session**: #314's six remaining families (F-A1 method:
sibling shape → widen ONE predicate → ten-shape probe vs perl; per-family
commits) → #320 → #316/#317/#319 → v0.1 track.  Fable keeps #153 FOLD
chunks 2–3, #271, #281, boxed aggregates.

**s391 (2026-08-14, Fable) — s390 review DONE: all five commits APPROVED,
every #303 judgment item RULED.**  Gate 138 / **5128** (cold-verified; only
the 8 user-ignored pclxs-ABI-drift xs rows fail), gen **v2-144**, sweep
TOTAL 18535.  Review + rulings: `docs/fable-answers-s390.md`; same verdicts
on task #303; DECIDED s391 section added.

**The s391 rulings in one breath** (#303, cheapest first, Opus executes):
DEBUG→constant is **GO** (the "21 live SET_DEBUG calls" blocker dissolved —
the one non-zero call is inside `if (0) {}`; bar = corpus-diff + gate);
`gen_anon_sub_form` delete-but-keep-`%NAMED_TYPE`-row (arrivals die via the
rule-12 arm); `ExprToCL2::generate` delete after auditing every polymorphic
`->generate(` receiver; `OpcodeTree::extras` settled as a writer+reader
PAIR; **W12 text annotator and `_gen_interp_replacement_simple` = DELETE +
rule-12 DIE, measurement-first** (instrument the fallback paths over
corpus + gate + sweep; zero events ⇒ delete under the s373 gate-SET bar;
non-zero ⇒ per-event verdicts first; `_text_gate_tags` STAYS);
`_tok_run_desc` **KEEP** (handoff corrected — it is `_term_probe`'s own
helper; a delete list must be closed under who-calls-whom against its own
KEEP list); Parser.pm's v1 state handlers stay with **#153 FOLD** (Fable).

**s390 recap (Opus):** #303 chunks 1–3 (~2,140 lines: 24 pre-E2 text
emitters, 21 both-legs-dead subs, BlockAnalyzer's never-wired
pexpr_factory path) + **#305 CLOSED** (`$$` PID mis-lex token pre-pass +
cast-RUN consumption at both sites; ref.t +3 recovered statements).

**NEXT: Opus = #304** (companion-suite snapshot 191 commits stale — PER-FILE
audit like #223, never `--bless-rows`, each of the 44 C_ok decreases gets a
verdict), then #303 in the ruled order.  **Fable = FOLD chunk 3** (design on
task #153 metadata: instrument the legacy opportunistic arrow/subscript
branches fired-on-claimed vs fired-on-declined over corpus + suite + board;
the legacy reduction is NOT wholesale-deletable — it IS `_reduce_term`'s
reducer for the whole-array case), #271 behind it, #281 + boxed aggregates.
Then §7 hoisting + v0.1 track (#277–#283).
