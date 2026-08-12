# Fable → Opus handoff, end of s389 (2026-08-13)

**Read this first.** It replaces `docs/opus-handoff-s388.md` as the entry
point (that file stays as history; its §4 census lesson is restated below
because task #303 depends on it).

---

## 1. What closed this session (Fable, s389)

| what | where |
|---|---|
| **#153 chunk 0** — Parser2 owns the lexical registries; v1 + ExprToCL read through ONE accessor `lex_home` | `f9d4ac5` + `76d7dd7` |
| **#153 FOLD chunk 2** — `$deref_skip` deleted (both copies + the #78 `has_deref` gate); 3 probe-found silent-wrongs fixed (eval result-deref, sort ctor-deref ×2 spellings); block-shaped `{…}->` dies perl-shaped | `00eb2ba` |
| **#211 CLOSED** — `$$rr->{k}` / `${$rr}->[i]` keep the outer deref level (Case 2's cast-binds-with-target rule applied at the X[] branch) | `00b8f09` |
| **#305 FILED** — triple-cast `$$$rrr->{k}` silently prints nothing (pre-existing; single-cast limit shared by Case 2 and Case 3) | task #305 |

**State:** gate **138 files / 5125 tests PASS**, cache gen **v2-143**,
tree clean, all on `main`.  Two full sweeps this session, both **GATE
clean, TOTAL 18532 = baseline** (chunk 0 warm; chunk 2 COLD after the gen
bump).  Baselines untouched — nothing moved.  Detail:
`docs/session-log.md` s389, `docs/DECIDED.md` s389.

## 2. The Opus queue, in order (task bodies are authoritative)

1. **#303 — the dead-code deletion batch** (~3.5k lines; map in
   `docs/compiler-duplication-review-s386.md` §2).  **Step 0 was attempted
   in s388 and REVERTED twice on a bad caller census** — the rule that came
   out of it: for EVERY "never called" candidate,
   `grep -rn NAME --include='*.pm' --include='*.t' .` with **no `| head`**,
   and read the whole output.  The real SET_DEBUG census is in the task:
   21 live calls across four Pl/t files, and `expr-01/02/03` +
   `phase1-01`'s `expr-02.t:63` is `SET_DEBUG(4)` — **non-zero**, so the
   DEBUG→constant conversion needs a decision about that call (recorded in
   the task) before it ships.  Bar per candidate: pure deletion,
   corpus-diff byte-identical, gate; full sweep for the batch.
2. **#305 — multi-cast deref before `->`** (small, recipe in the task):
   generalize BOTH cast-consuming sites (Case 2 ~PExpr.pm:975, Case 3 X[]
   branch ~1200) from one leading `$` cast to a run, via a shared helper
   (rule 11).  Rows FIRST in `transpile-test-10.t`; probe file
   `probes/p15.pl` shape in the task.  Also check whether the current
   silent-empty is a PARSE-ERROR statement drop (#138 family) — if so that
   path must die/announce regardless.
3. **#304 — companion-suite snapshot re-bless** (191 commits stale;
   measurement in `docs/opus-handoff-s388.md` §5).  A PER-FILE audit like
   #223 gave the sweep baselines — **never `--bless-rows`**, which would
   bless the whole TRANSPILE cluster as expected.  Each of the 44 C_ok
   decreases gets a verdict: deliberate E4.1 refusal (bless with cause) or
   unexamined loss (file a task).
4. Fillers if blocked: #257 (fail-baseline cause column), near-green list.

## 3. NOT Opus's — do not start

- **FOLD chunk 3** (Fable-led, design in task #153 metadata): instrument
  the legacy "4 arrow cases" branches fired-on-claimed vs
  fired-on-declined over corpus + suite + board, widen or delete,
  `PCL_NO_FOLD` dies with the deletions.  The legacy reduction is NOT
  wholesale-deletable — it IS the reducer `_reduce_term`'s recursive
  parse invokes for the whole-array case.
- **#271** (pipe-my) — ruled BEHIND the FOLD; it is a FOLD acceptance
  probe, not a standalone fix.
- **#281** (IR macro pass) and **boxed aggregates** — standing rulings
  unchanged.

## 4. Things worth knowing before touching Pl/

- **The lexical registries live on Parser2 now.**  Any new site reads
  them through `$parser->lex_home->{...}` (v1/ExprToCL side) or
  `$self->{...}` (Parser2 side) — never through `fallback_parser->{...}`,
  which no longer holds them.  A seam parser whose owner is gone DIES in
  `lex_home`; that is deliberate (rule 12), not a bug to soften.
- **`_ctor_deref_verdict` owns the intuit_curly boundary** for
  grep/map/sort + `{…}->`.  Empty `{}` counts as a ctor (`grep {}->{a}`
  is valid perl).  Don't add a third copy of that decision.
- **eval/do + `->` chains stay IN the token stream** and bind on the
  funcall node — no branch may consume them into a body again.
- The 5 chunk-2 rows + 2 #211 rows live in `transpile-test-10.t` (now 40
  rows, ~46 s — fine, but it is growing; the next new file is `-11`).
