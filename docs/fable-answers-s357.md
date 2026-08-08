# s357 review + rulings (s358, Fable, 2026-08-08)

Review of s357 (`b8369cf`, Opus 5 — E4.1 step 4 / #244) and the rulings on
its two asks, with the user's decisions recorded.  The #252 phase-1 fix
shipped in this same session; see §4.

## 1. s357 verification — APPROVED, independently reproduced

Every load-bearing claim of `docs/e41-step4-verification-s357.md` was
re-measured, not read:

- **Gate**: re-run, **131 files / 4658 tests PASS** (before this session's
  additions; 4660 after).
- **Cost arithmetic**: the 28 flip-lost suite files sum to **exactly
  15,129** snapshot C_ok (+1,307 not-ok; perl runs 20,431) — matches §2.3
  to the row.
- **Attribution**: a fresh worktree at `26ce393` prints `pipeline=v1` for
  sampled members of the 28 (io/through.t, op/my.t, comp/our.t) and for
  `Text/Balanced.pm`; at HEAD each dies with exactly the family-table
  error.  `op/for.t` confirmed already PPI-dead pre-flip (correctly
  excluded — now task #253).
- **Board**: 1794 → 1028 ok, 62/60/61, all nine Text-Balanced files FAIL 0
  — reproduced from the checked-in TSVs.
- **#243 blocker**: 27 Pl/t files call `Pl::Parser->new` directly; the only
  remaining `PCL_V1`/`parse_with_fallback` mentions in code are comments.

**One arithmetic nit**: the nine Text-Balanced files carried **780** ok
rows, not 766 — 766 is the *net* board delta after the +14 of expected
gains.  Cosmetic; the picture is unchanged.

s357 was a model verification session: no code, honest findings, measured
attribution.

## 2. The two durable amendments — RATIFIED

1. **The audited populations are FOUR**: sweep + CPAN board + Pl/t gate +
   **the perl suite** (`tools/run-perl-suite.pl`).  §5a.2 as amended by
   s352 §1 is amended again.  This is the same miss twice (s342c missed
   Pl/t; s355 missed the suite): the fix is a closed LIST, not vigilance —
   any future "zero events across the populations" claim must name all
   four.
2. **A live-v1 style audit must run on a COLD cache** (`rm -rf
   ~/.pcl-cache/*` or a generation bump first).  A transpile-time
   instrument reports zero for exactly the already-compiled modules that
   are most load-bearing — that is how the audited board missed
   Text::Balanced.

Both recorded in `docs/DECIDED.md`.

## 3. The asks — USER DECISIONS (2026-08-08)

- **#252** (fix / register / split, ~15.9k rows): **"Do 1. Then write a
  plan for 2."** — i.e. the split, with phase 1 (Text::Balanced) executed
  now and phase 2 (the two big suite families) PLANNED, not yet executed.
  Plan: `docs/e41-suite-families-plan.md` (task #254).  Residue
  registration is part of that plan's scope decision, not pre-decided.
- **#243** (v1's file-level entry, 27 gate files): **PORT the 27 files
  now** — rewrite their assertions against v2 emission so the entry can be
  deleted.  This overrides the keep-as-test-only recommendation; step 3's
  deletion becomes reachable again.  Task #255 (Opus).  Porting rule:
  assertions must stay semantically equivalent (principle: never weaken a
  test) — a v1-CL-shape assertion becomes the corresponding v2-shape or
  behavioural assertion, and any row that only ever asserted a v1
  IMPLEMENTATION DETAIL (not Perl behaviour) is dropped with a note, not
  translated into vacuity.

## 4. #252 phase 1 — SHIPPED this session (s358)

Three compiler fixes, found by burning down Text::Balanced end to end:

1. **General forward-goto shape** (`Pl/Parser2.pm` `_lower_block`):
   `_match_tagged` has three standalone labels (`short:`/`matched:`/
   `failed:`) with forward gotos that CROSS intervening labels.  New
   branch: nested catches, `(catch :L3 (catch :L2 (catch :L1 P0…) P1…)
   P2…) P3…` — each label's catch encloses everything before it, so a
   throw from any depth lands at its label and fallthrough runs segments in
   source order.  Leading declarations are NOT hoisted: the ordinary
   `my`/`local` branches consume them and nest the remainder (labels
   included) in their let, recursing back — so the branch fires at the
   level where only code remains, and array.t's existing single-label
   emission stays byte-identical.  Backward/mixed gotos keep the classic
   tagbody machinery and its gate.  The existing single-label wraps gained
   a cross-label guard (they would have emitted an unreachable `(go)`).
2. **List-decl false self-reference** (`_reads_name_rx`): `my ($class,
   $func) = ($class[$i], $func[$i]);` — the text scan saw `$class[` and
   flagged scalar `$class` as self-referential, binding `(p-box-init
   $class)`, a READ of a scalar that doesn't exist → `|Text::Balanced|::
   $class is unbound` at load (05_extmul.t).  `$name[`/`$name{` are
   elements of `@name`/`%name`, never the scalar; the shared helper now
   excludes them in all four scans.
3. **Interpolated subscript chains** (`StringInterpolation.pm`):
   `"$_[0]->{error}"` / `"$a[0]{k}"` stopped after the first subscript and
   left the tail LITERAL — silent wrong, and inside
   Text::Balanced::ErrorMsg's overloaded `""` the self-stringification
   recursed until the binding stack died (04_extdel.t, the file the board
   reported as TIMEOUT).  When (and only when) a chain continues past the
   first subscript, the whole chain now parses via PPI; lone
   `$var[…]`/`$var{…}` keep the legacy path byte-for-byte.

**Result**: Text-Balanced restored **better than it ever was under v1** —
933 ok / 161 not-ok across the 9 runnable files vs the v1-era baseline's
780 ok / 300 not-ok (03/04/05/06/07 all up; 04_extdel 89/1 vs 88/0 with
the 1 being the pre-existing #237 `extract_multiple` offsets divergence,
now confirmed live).  Verification: corpus-diff = 1 of 111 differs
(aassign.t — the interp-chain reroute; its probe divergence reproduced
byte-for-byte at HEAD, #169 family, and its sweep rows are 0 new / 0 fixed
vs baseline), gate 131/4660 PASS, full sweep gate run (see session log),
`*pcl-cache-generation*` bumped v2-112 → v2-113, pack/mro artifacts
regenerated (marker-only diffs — this change does not touch their
emission).  Regression rows: `Pl/t/transpile-test-09.t` +2 (multi-label
goto incl. backward-goto inverse guard; interp chains + element
self-ref).

**Noted, not fixed** (pre-existing, exonerated by HEAD re-runs):
- array.t single-file cold-cache sweep shows 6 new / 2 fixed vs baseline
  — identical fail rows at HEAD, so a single-file-vs-full-sweep
  measurement artifact (ref-address numbering + rows 109–114), not a
  regression from this change.  The full sweep is the authority.
- `extract_multiple` returns wrong fields (`['a']` vs `['a',"'x b'",'c']`)
  — #237's family, now with a minimal probe in its task.

## 5. Queue after this session

1. **#255** — port the 27 Pl/t files off v1's file entry, then complete
   the #243 step-3 deletion (Opus; USER-decided).
2. **#254** — the phase-2 plan (`docs/e41-suite-families-plan.md`) goes to
   the user for scope approval, then execution (capture/spanning +
   poisoned-`my` families ≈ 12k rows).
3. **#253** — op/for.t PPI-parse regression (pre-existing, between s323e
   and s355).
4. Then the STOP holds: #153/E5.0 (Fable-led) next.
