# Opus 5 work instructions — PCL v2, from s316t onward

*Written by Fable, s316t (2026-08-01), at gen v2-88, gate 123/4432, sweep
baseline 702, census 111/111 v2-native.  Division of labor (user,
2026-07-25): Fable does planning/review/hard parts; Opus 5 executes.
This document is the execution side of `docs/v2-endgame-plan.md` (the
plan), `docs/v2-code-review.md` (the gap analysis), and
`docs/v2-target-architecture.md` (the destination).  Read those three
first, in that order — the review's findings ARE the coming sessions.
The pclxs/XS track is separate (`~/pclxs` repo, its own plan.md — RULES
first); do not mix the tracks in one session.*

**Context you must hold: R1 (correctness release) is expected ~2026-08-03.
Until the user says R1 has shipped, the tree must stay shippable: no
half-landed refactors on main, every commit leaves gate+sweep at
baseline.**

---

## 1. Hard stops — ask the user, do not decide

- Marking anything not-supported / adding a skip-registry row / weakening
  or simplifying ANY test (CLAUDE.md rules 4, 5; the registry cites a
  blessed `docs/not-supported.md` entry or it doesn't exist).
- Task #139 (:crlf layer model) and #132 (source-echo comments): design
  decisions, user's call — do not start them.
- Anything that adds complexity or slows generated code: flag it in the
  session summary (user directive s308).
- Starting E4.1 (§4) before the user confirms R1 shipped.
- `eval $str` must always work (hard requirement) — nothing may gate it.

## 2. Per-session checklist (mechanical — run it in order)

1. **Sync state**: read the STATE line in memory + the newest
   `docs/session-log.md` entry.  `git log --oneline -5`.  Task list =
   the work queue; this doc = the order.
2. **Baseline before touching anything**: for the shape you're about to
   change, record what HEAD emits (`./pl2cl < probe`) and what perl does
   (`perl probe`).  For divergences: **v1 is still the oracle until
   E4.1** — check `PCL_V1=1 ./pl2cl` and copy its shape when it is
   correct; do not invent one.
3. **Probes first**: minimal scratch files, perl vs `./runpl`, BEFORE
   editing (the s316t bugs were all found/verified this way, minutes
   each).
4. **Implement.**  Reuse the sibling mechanism (CLAUDE.md 11) — grep for
   the existing helper before writing a new branch.  Anything you cannot
   lower fully correctly dies `Parser2 TODO: <precise reason>` — never a
   silent wrong shape.
5. **Verification quadruple**, in this order, all green before commit:
   a. `perl tools/corpus-diff.pl` — every changed file EXPLAINED;
   b. `tools/prove-core` — full gate (123 files), not a subset;
   c. full sweep `perl sweep-perl-tests.pl --jobs 8 --timeout 380` then
      `perl tools/sweep-diff.pl diff docs/fail-baseline.tsv .faillog` —
      0 new (pack.t needs the 380; at 150 it TIMEOUTs even idle);
   d. `perl tools/v2-census.pl` — still 111/111.
   **Never run two sweeps concurrently — a sweep CLEARS `.faillog` at
   start** (s316t lost a run to this).  Don't run prove-core while a
   sweep runs either (pack.t contention).
6. **Regression guards**: behavior → a `test_transpile` battery in the
   smallest `transpile-test-NN.t` (currently -07; cap ~50/file, never
   -01); emission shapes → `Pl/t/parser2-01.t` (grep it first for stale
   guards your change flips).
7. **If emission changed**: bump `*pcl-cache-generation*`
   (cl/pcl-runtime.lisp), regenerate artifacts (`tools/rebuild-pack`;
   `./pl2cl lib/mro.pm > cl/pcl-mro.lisp` — non-empty check: pl2cl exits
   0 with EMPTY output on compile errors), verify both are header-only
   or explain the diff.
8. **Lisp edits**: `sbcl --script tools/check-parens.lisp FILE` after
   every Write/Edit; 2-space depth discipline.
9. **Close out**: one commit per feature to main; session-log § at top;
   memory STATE updated IN PLACE (no session log in MEMORY.md); found-
   but-not-fixed problems become tasks, never TODO comments.

## 3. W0 — DONE (s316u, gen v2-89): task #138, the shared splitter

Landed with one deviation from the scope below, and two sites the review
had not found.  **Deviation:** step 1 asked for a span-returning
`_split_at_lowprec`.  Splitting requires re-deciding the parenless
list-operator ambiguity (`my $c = h 1, 2` really does pass both args to
h), which is PExpr's knowledge — so the shipped fix hands the WHOLE
`$x = …` run to the expression machinery at every site that can, exactly
as the s316t statement fast path does, and splits only where code must be
interposed between head and tail (the state once-guard).  The table itself
now lives once, in `Pl::PExpr::TokenUtils::lowprec_idx` /
`lowprec_split_safe` — plain subs, because v1's `local` needs them too and
neither statement parser may depend on the other.  **Extra sites:** the
single-`my` C-for init (`for (my $i = 0, $j = 9; …)` ran ZERO iterations)
and v1's `local` handler (live in v2 as well — `local` routes through the
v1 seam).  Residual recorded in review §2 and #138: an ambiguous comma in
a *state* init still folds.

Original scope, kept for the record:

1. **Extract the helper.**  Generalize s316t's `_tail_below_assign_prec`
   (Parser2.pm ~5056) into ONE table-owning helper (suggested:
   `_split_at_lowprec($toks, $from_idx)` returning `(head_span,
   tail_span)` or undef-when-clean).  Depth-0 set: `, => or and xor`.
   Keep the existing two statement-fast-path guards on it.
2. **Fix the three v2 sites**:
   - `_single_scalar_decl` (~6370): `my $x = 1, $y = 2;` must lower as
     decl+init `1`, then the tail as a separate void expression
     (perl: x=1, $y global =2).
   - state scalar init peel (~4270): same split before the `box-set`.
   - `_extract_params` (~4060): make the match EXACT arity
     (`my (LIST) = @_;` and nothing else — @k==4 modulo semicolon).
     Anything else falls through to normal lowering.  **This is the
     silent-drop bug (`my ($a) = @_, g();` deletes the g() call) — it
     is why W0 is release-blocking.**
3. **Probe the v1 reach**: the same decl shape through a v1 seam (e.g.
   `my $x = 1, $y = 2 if $c;` routes via `_fallback_stmt` 4324).  If
   v1's `_process_variable_statement` shows the same fold and the fix is
   small, take it; if not, record the residual precisely in #138 and
   move on — v1's copy dies at E5.3 anyway.
4. **Watch the annotator**: comma-tail decls must leave the declared var
   BOXED (the write is not a clean native-root event).  If corpus-diff
   shows a raw `setf` appearing/disappearing anywhere, stop and check
   `PCL_W12_DIFF=1` on that file.
5. Battery in -07 (decl comma, decl or-tail, param exact-arity incl. the
   g() shape, state-init comma).  Full checklist §2.  Est: 1 session.

## 3b. Open review requests back to Fable

**`docs/opus5-review-requests-s316v.md`** — four items raised from execution,
each with the evidence: (1) bareword class names / task #142, where three
attempts failed and the failure shape points at
`pexpr-term-parsing-review.md` Option B — asks whether to take the
`bless`-class-name route now or hold; (2) task #141, where the flip-flop
constant-operand classifier should live; (3) a priority call on E5.5 given
that both "one place" consolidations so far found live bugs 2-for-2;
(4) task #138's residual (ambiguous comma in a `state` init).

## 4. W1 — the R1 window (until the user ships)

- Stay on #25 support: if the user's release checks surface divergences,
  they preempt everything.  Triage per `docs/test-debugging-runbook.md`.
- Idle capacity goes to more suite families from
  `docs/perl-suite-run.tsv` (the s310–s316t pattern: pick near-green
  DIFF files, fix the family at the right layer).  Good next candidates:
  the remaining `undef-fn` rows (`Tie::StdHash`/`Tie::StdScalar` shims →
  op/avhv.t + op/warn.t; `Internals::getcwd` → io/getcwd.t).  Shim
  layer, not runtime (CLAUDE.md 9a).
- No structural work in this window.

## 5. W2 — E4.1, only after R1 ships (1–2 sessions)

Re-scoped in the plan (E4 section) and review §4.  Order:
1. Port bundle mode off `Pl::Parser->parse_file` (`pl2cl:283`) — the one
   v1-only bypass.
2. Flip gates to hard errors: remove `parse_with_fallback` (`pl2cl:51`),
   `PCL_V1`, `PCL_V1_FILES`; purge the consumer list (review §4 names
   them: two Pl/t tests, skip-registry:315, census tooling, the
   runtime cache-key branch `p-compute-cache-path`).
3. Delete v1's now-unreachable file-level chunks (~550 lines:
   `parse()` entry, `_assemble_output`,
   `_insert_variable_forward_declarations`, `parse_file/parse_code`,
   lenient-ppi path).  Verify each is truly unreachable (grep + gate)
   before deleting — the review's reachability table
   (`docs/v2-code-review.md` §4) lists lookalikes that are STILL LIVE.
4. Full verification incl. one FULL suite re-run
   (`tools/run-perl-suite.pl` per-dir foreground chunks — background
   `--all` dies at the 10-min cap before writing --tsv) + CPAN
   scoreboard vs `docs/cpan-module-log.md`.  Re-bless nothing without
   noting it in the ledger.

## 6. W3+ — E5 in plan order (specs in the plan's E5 section)

Execute strictly in order E5.1 → E5.5; each step lands whole (green
quadruple) before the next starts.  Summary of what each means
operationally — details and acceptance in `docs/v2-endgame-plan.md` §E5:

- **E5.1 SeamSession (1–2)**: one guard object replacing the two
  bucket-dance copies (Parser2 5826-5851, 6303-6363) and the eight
  `_let_bound_vars` save/restore pairs (inventory in review §3 — use it
  as the site list).  Restore must run on die (guard object / local),
  not by reaching the restore line.  Pure refactor: corpus-diff must be
  EMPTY.
- **E5.2 embed totality (1–2)**: `lower_embedded_block` stops declining;
  then delete `parse_block_to_cl_string` / `parse_block_as_function` /
  `parse_hash_block_*` + the 4 raw sites (ExprToCL 1872, 1980, 5557,
  5719).  This is task #78's tail — read its history first.
- **E5.3 fallback burn-down (4–8)**: retire the 12 `_fallback_stmt`
  classes.  Suggested order, mechanical→hairy: loop statement-modifiers;
  multi-element `return`; goto/next/last/redo; anon-hash-as-bare-block;
  `local`/`delete local`; my-with-fallback-modifier; use/require/no;
  BEGIN/END scheduled; prototype/signature subs; the two nested-sub
  shapes; eval-mode residue.  One class per commit; each retires its v1
  `_process_*` handler — delete the handler in the SAME commit or it
  will rot half-referenced.
- **E5.4 one expression brain (2–3)**: fold ExprToCL2 into the emitter
  as early-return branches sharing one BINOP/ctx table; then close the
  two structural decline paths (ExprToCL 546, 571) and delete
  `gen_node` + the string emitters + `raw`/`raw_wrap`.
- **E5.5 shared predicates + shape (1–2)**: `native_root_write` shared
  with VarAnnotator; one context-constant set; Parser2 phase file-split;
  rewrite `CODEGEN_DESIGN.md`; ir-spec to single-dialect.

## 7. Estimates and cadence

W0 1; W2 1–2; E5 9–17; per the architecture doc the full v2-final is
~16–29 post-R1.  Calibration: the burn-down families have historically
run 2–3 classes/session when the checklist is followed and ~0 when a
regression escapes to the next session — the quadruple is cheaper than
any shortcut.  When a step stalls twice on the same cause, stop and
write the failure up for Fable review instead of pushing a third time.
