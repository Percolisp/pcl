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
  blessed `docs/not-supported.md` entry or it doesn't exist).  Exception:
  rows/entries a Fable answer in `fable-answers-s316v.md` explicitly
  approves (e.g. the #149 error-text category) — those are pre-authorized.
- Anything that adds complexity or slows generated code: flag it in the
  session summary (user directive s308).
- Starting E4.1 (§5) before the user confirms R1 shipped.
- `eval $str` must always work (hard requirement) — nothing may gate it.
- Before writing ANY question up as open: run the CLAUDE.md lookup order
  (grep `docs/DECIDED.md`, then `docs/not-supported.md`).  Most questions
  are settled; the s316v review doc's §7 documents what re-deriving them
  costs.

## 1b. The open-decision ledger — ONLY these need a human; everything
##     else in this plan is decided and executable

| id | decision | state |
|---|---|---|
| U1 | R1 has shipped (gates E4.1 and all post-R1 work) | user call, expected ~2026-08-03 |
| U2 | `printf %n` (#143): implement vs bless | user; Fable recommends bless |
| U3 | deterministic DESTROY for R2 (§6c): commission sizing doc post-R1? | user; Fable recommends sizing doc, no code |
| U4 | #139 :crlf layer model | user-held, do not start |
| U5 | #132 source-echo comments | user-held, do not start |

When you hit one of these, note it and move to the next queue item — do
NOT stall the session on it.  Everything else that looks like a decision
has an answer in `docs/fable-answers-s316v.md` (s317) or `docs/DECIDED.md`.

## 2. Per-session checklist (mechanical — run it in order)

1. **Sync state**: read the STATE line in memory + the newest
   `docs/session-log.md` entry.  `git log --oneline -5`.  Task list =
   the work queue; this doc = the order.  Before probing or designing
   anything: the CLAUDE.md lookup order (`docs/DECIDED.md`, then
   `docs/not-supported.md`) — one grep each.
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

## 3b. Review requests: ANSWERED (s317, s320)

**`docs/opus5-review-requests-s318.md` §1–§11 is now RULED ON in
`docs/fable-answers-s318.md` (s320)** — one-liners indexed in
`docs/DECIDED.md`.  Headlines: aggregate-state model split (side table
REJECTED, boxed aggregates = E5-era Fable design, referent-kind tag
APPROVED post-R1 → #163 + #154's shapes); #155 → loud WARN now (never a
die pre-R1); #159 → storage-swap (b) post-R1; #158 → NO fix, principle 9
(pre-5.20 semantics kept), register do.t t63/t65; chr.t >U+10FFFF blessed;
FIXTURE status + registry approved; **the full tsv regeneration GATES R1**
(last pre-R1 act, foreground chunks).  The ordered S1/S2/S3+ worklist is at
the end of the answers doc — follow that over §4 below until it is spent.

Every ask in `docs/opus5-review-requests-s316v.md` is ruled on in
**`docs/fable-answers-s316v.md`**; the one-liners are indexed in
`docs/DECIDED.md`.  Highlights: #142 bless route APPROVED (W1); #141
classifier placement decided (W2.5); the "one place" evidence created
E5.0 (task #153, the `_reduce_term` reducer, Fable-led start); #147 and
the #138 residual are BLOCKED on #153 — do not attempt them in place.
Only the §1b ledger items still need a human.

## 4. W1 — the R1 window (until the user says R1 shipped)

> **CURRENT POSITION (s321).**  The list below is the *original* W1 ordering;
> items 1-5 are all DONE and the window is now driven by Fable's S1/S2/S3+
> worklist at the end of `docs/fable-answers-s318.md`.  Where we are:
>
> - **S1 (gate hygiene) — DONE, commit `0758d5a`.**  FIXTURE status +
>   `docs/perl-suite-fixture.tsv` (per-ROW, all-or-nothing, both inverse
>   guards probed); op/chr.t → XDIFF; #158 closed no-fix; op/list.t +
>   op/pack.t QUARANTINED as NOT-RUN-with-reason; #155 aggregate tie now
>   announces itself on stderr.
> - **Unplanned but blocking, DONE, commit `4a1bf98`: #177.**  The runner
>   joined the two TAP streams BY TEST NUMBER, so any file where PCL emits
>   extra/missing rows mid-file mis-attributed every later row — in both
>   directions.  It accused two op/do.t rows that PASS.  Now paired by
>   description (`tools/lib/PclTapAlign.pm` + `tools/t/tap-align.t`).  This
>   had to precede S2 because the FIXTURE registry matches per-ROW against
>   that log.  op/do.t went XDIFF once the pairing was honest, after fixing
>   its one real bug (`do DIR` now reports `$!` = EISDIR).
> - **S2 (the full `docs/perl-suite-run.tsv` regeneration, which GATES R1)
>   — running.**  Per-dir foreground chunks, `--jobs 3`, each with its own
>   faillog.  On completion: install the merged snapshot, then run the #177
>   cross-check (files whose log carries a `renumbered` marker AND a
>   registration whose reason makes per-row claims — those registrations
>   were reasoned from the old, possibly mis-joined log).
> - **S3+**: near-green silent-wrong families from the refreshed list.
> - **Open asks for Fable**: `docs/opus5-review-requests-s321.md` (#176
>   pack.t invisible to the sweep gate; the #177 blast radius; whether XDIFF
>   reasons should carry machine-checked row lists like FIXTURE does).

Order within the window; #25 support preempts everything (if the user's
release checks surface divergences, triage those first, per
`docs/test-debugging-runbook.md`).

1. **#142 — tie/tied/untie bareword class via the bless mechanism**
   (APPROVED, see the task for the required probe battery: `Foo::init`,
   `Count::DATA->getline`, quoted-vs-bareword).  Unblocks op/avhv.t +
   op/warn.t.
2. **Tie::StdHash / Tie::StdScalar shims** (`lib/Tie/…`) if #142 alone
   doesn't clear those files; `Internals::getcwd` → io/getcwd.t.  Shim
   layer, not runtime (CLAUDE.md 9a).
3. **#151 — NOTAP drift**: find out why perl produces no TAP for
   io/defout.t / op/localref.t / uni/bless.t under the runner; refresh
   the rows.  Do this BEFORE picking new near-green targets from the
   snapshot — the near-green list is only as real as its rows.
4. **#150 part 1** — drop the copied-file skip in
   `tools/run-perl-suite.pl` (tooling only; part 2 re-sync is post-R1).
5. **#149 — error-text skip category** (pre-authorized, see task).
6. **More near-green DIFF families** from the refreshed
   `docs/perl-suite-run.tsv` (the s310–s316t pattern: fix the family at
   the right layer).
7. No structural work in this window.  #66 (forward-decl scan false
   positives) is W1-eligible if the queue empties: it is a contained,
   probe-verifiable fix.

## 5. W2 — E4.1, only after R1 ships (U1) (1–2 sessions)

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

## 5b. W2.5 — the decided post-R1 backlog (order within is free; each
##     item is small enough to interleave with E5 steps; ~3–5 sessions)

All decided s317 (`fable-answers-s316v.md`); each task carries its spec:

- **#150 part 2**: re-sync the four drifted `perl-tests/` copies
  (chop/dor/not/quotemeta) + re-bless.
- **#146 quotemeta**: byte-semantics default, Unicode under
  utf8/unicode_strings, transpile-time selection; fix the
  `not-supported.md` Unicode sentence in the same commit.
- **#141 flip-flop**: per-operand constant classifier at the existing
  `p-flipflop*` selection point.
- **#148 pack U modes**: full mode model; `cl/pack-impl.pl` under real
  perl is the dev oracle; `tools/rebuild-pack` same commit; rule-12
  loud-die for anything left out.
- **#152 rule-12 audit**: sweep runtime dispatch defaults → explicit
  errors.
- **#144 `\$!` magic-cell box** (the remaining decided suite blocker for
  op/bless.t + uni/bless.t; box-magic hook per
  `reference_box_magic_hook`).

## 6. W3+ — E5 in plan order (specs in the plan's E5 section)

Execute in order E5.1 → E5.5; each step lands whole (green quadruple)
before the next starts.  **E5.0 (task #153, the `_reduce_term`
non-mutating term reducer) is a separate change set: Fable lands steps
1–2 (hot path + indirect-object risk); Opus takes the mechanical
migration steps 3–5 afterward.**  E5.0 may run before, between, or
parallel to E5.1/E5.2 sessions but MUST land before E5.5; it unblocks
#147, the #138 state-init residual, and the general bareword rule.
Summary of what each step means operationally — details and acceptance
in `docs/v2-endgame-plan.md` §E5:

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

## 7. Estimates and cadence — the runway

W1 ~2–3 sessions; then the only gate is U1 (user says R1 shipped); then
W2 (E4.1) 1–2 + W2.5 ~3–5 + E5.1/E5.2 2–4 + E5.3 4–8 — all decided and
specced.  That is **roughly 12–20 sessions of executable work with
exactly one human gate (U1) in the middle**; the next genuinely open
decisions are the §1b ledger (U2/U3 can be answered any time without
blocking the queue) and the E5.0 handback points (Fable lands the
reducer, Opus continues).  Per the architecture doc the full v2-final
remains ~16–29 post-R1.

Calibration: the burn-down families have historically run 2–3
classes/session when the checklist is followed and ~0 when a regression
escapes to the next session — the quadruple is cheaper than any
shortcut.  When a step stalls twice on the same cause, stop and write
the failure up for Fable review instead of pushing a third time — #142's
three recorded attempts are the model of what that write-up buys.
