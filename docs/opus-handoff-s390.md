# Opus → Fable handoff, end of s390 (2026-08-13)

**Read this first.** It replaces `docs/fable-handoff-s389.md` as the entry
point (that file stays as history; its §4 "things worth knowing before
touching Pl/" is still current).

---

## 1. State

Gate **138 files / 5128 tests**, cache gen **v2-144**, tree clean, all on
`main`.  Full sweep at the end of the session: **GATE clean, TOTAL passing
18532 → 18535 (+3)**.  Baselines untouched.

**The 8 failing xs rows are NOT PCL's.**  `Pl/t/xs-02.t` + `xs-03.t` fail
because the sibling `~/pclxs` has advanced to ABI 7 while this checkout's
`xs-pin` says 6 — the parallel pclxs work.  Verified present at HEAD in a
worktree *before* any s390 commit, and the user has asked that XS problems be
ignored for now.  Every other file is green.  When reading a gate result this
session, "identical to HEAD" means those 8 and nothing else.

| commit | what |
|---|---|
| `7285ccc` | #303 chunk 1 — ExprToCL's 24 unreachable pre-E2 TEXT emitters + 8 helpers (6593 → 4547 lines) |
| `609b20a` | #303 chunk 2 — 21 subs dead by BOTH grep and coverage (Parser/Parser2/PExpr/TokenUtils/Environment) |
| `65816cd` | docs — the census rulings + the s386 review correction |
| `8385780` | **#305 CLOSED** — the deref cast RUN + the `$$` PID mis-lex repair; recovers 3 dropped `ref.t` statements |
| `30b4bf9` | #303 chunk 3 — BlockAnalyzer's never-wired `$pexpr_factory` path (70 lines) |

~2,140 compiler lines deleted; one silent-statement-drop family closed.

## 2. The thing to carry forward: **the s386 dead-code map is not safe to
delete from**

`docs/compiler-duplication-review-s386.md` §2 now carries a CORRECTION block,
and task **#303** holds the per-candidate verdicts.  Three of its claims were
false, each in a way that would have shipped a deletion of live code:

1. **`Pl::BlockAnalyzer` is LIVE** — claimed "whole module, 0 of 11 subs
   called"; it fires **1244× per corpus transpile**.  Parser.pm `require`s it
   LAZILY at runtime, after a load-time wrap-all-subs tracer has already
   installed its wrappers.  (Audited: the only lazily-required `Pl::` module,
   so that blind spot is closed.)
2. **Moo's `is => 'lazy'` names `_build_X` IMPLICITLY** — zero textual
   references, called on every object.  `_build_fallback_parser` builds the v1
   fallback parser.
3. **`^sub (\w+)` matches POD** — `Environment::body`, the review's largest
   single Environment claim (157 lines), is a POD line reading "sub body, or
   direct value of a return statement)".

**The bar that replaced it — both legs, every candidate:** STATIC
(`grep -rn NAME`, no `| head`, whole output read) AND DYNAMIC (source-level
counter in every column-0 named sub, run over the corpus AND the Pl/t gate,
because the gate is what covers eval mode and module transpiles).  Tooling is
in the session scratchpad and described on #303: `instrument.pl` (`--undo`
restores from git), `census.pl`, `covreport.pl`, `delsub.pl`, `delpod.pl`.

**Known limit of the dynamic leg:** a ONE-LINE `sub f { ... }` gets its
counter inserted on the *following* line, i.e. at file scope, so it records as
called at load.  That biases toward LIVE — it can hide a dead sub, never
invent one — so settle one-liners by grep (that is how the
`is_list_parentheses` / `_is_block` delegation pairs were established).

## 3. What is left on #303, and why each is a judgment call

Everything below is textually REFERENCED but never called in either
population, so the *caller* has to go too.  None is a mechanical delete.

- **VarAnnotator's W12 text annotator** (`_analyze_text`/`_scan`/
  `_diff_report`/`_arith_rhs`, ~201 lines).  `analyze()` reaches it three
  ways: an unreachable `!$host` guard (all 6 Parser2 callers pass `$self`),
  **a SILENT FALLBACK when `_analyze_tree` dies**, and `PCL_W12_DIFF`.
  RECOMMENDATION: delete it and make both the `!$host` case and the tree
  crash DIE — a variable annotation decides BOXING, so a second annotator
  silently substituting is a silent-wrong generator, exactly the E4.1 lesson.
  Behaviour change ⇒ needs the s373 gate-SET bar (both-populations die-scan +
  corpus-diff + sweep TOTAL/LOST).
- **Parser.pm's v1 state handlers** (`_process_toplevel_state_declaration`
  109, `_process_state_declaration` 93, `_process_trailing_tokens`,
  `_block_has_inner_named_subs`) — their CALL SITES never fire.  Deleting
  them means deleting v1 statement paths, i.e. **#153 FOLD territory**; size
  it there rather than as a dead-code sweep.
- **ExprToCL** `_gen_interp_replacement_simple` (70) and `gen_anon_sub_form`
  (11 — in the form_handlers table, but the `anon_sub` node type never reaches
  the seam, so the whole table entry may be dead).  Singles: `ExprToCL2::generate`,
  `OpcodeTree::extras`, `PExpr::_tok_run_desc`.
- **STEP 0, DEBUG→constant** — unchanged and still blocked on a DECISION, not
  on work.  `use constant DEBUG => $ENV{PCL_PEXPR_DEBUG} // 0` is
  corpus-diff-identical and removes 4.3M calls per transpile, but there are
  **21 live `SET_DEBUG` calls across four Pl/t files and `expr-02.t:63` passes
  4** — non-zero — so the constant would silently kill that region's output.
  Port all 21, or keep a setter.

**KEEP, deliberately** (do not "clean up"): `CLForm::to_program` (bundle
mode), `CLForm::_raw_census`, `Parser2::_seam_*` ×5 (`PCL_V2_SEAM_CENSUS` is
#153's own progress metric), `PExpr::_term_probe`,
`Environment::get_caret_globals`.

## 4. #305's residue, for whoever touches PExpr's term machinery

- The cast-run rule is now stated once, at the Case 0–3 dispatch comment, and
  read through `_cast_run_start` / `_all_scalar_casts`.  With 0 or 1 casts the
  behaviour is bit-for-bit pre-#305; 2+ is the new ground.
- **Probe the MIXED-sigil spellings** (`@$$arr[0,1]`, `%$$hrr{"a"}`) on any
  change here.  Folding casts into the base makes `$pre_n` a cast node, and an
  `is_var($pre_n)` guard on a type decision then silently falls through — that
  cost one probe round and turned a crash into a one-element silent-wrong.
- Incidentally found and NOT fixed: PCL does not raise `Not a SCALAR
  reference` when a `${…}` deref targets a hash/array ref (perl dies).  That
  is the #163 referent-kind family, already tracked.

## 5. Queue

**#304** (companion-suite snapshot, 191 commits stale) is the untouched item
from the s389 queue — a PER-FILE audit like #223, never `--bless-rows`; each
of the 44 C_ok decreases gets a verdict.  Then #303's judgment items above.

Fable's own items are unchanged: **FOLD chunk 3** (design on task #153
metadata; the legacy reduction is NOT wholesale-deletable — it IS
`_reduce_term`'s reducer for the whole-array case), **#271** (behind the
FOLD), **#281** and boxed aggregates.

New task filed this session: **#307** — the `_form` suffix on ExprToCL's
emitters is now historical (their text twins are gone) and should be renamed
away; IR-neutral, sequence with the v0.1 track.
