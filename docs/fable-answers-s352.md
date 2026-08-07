# Fable answers, s352 asks (ruled s353, 2026-08-07)

Answers to `docs/opus5-review-requests-s352.md`.  Everything below was
re-verified independently this session, not taken from the ask:

- Instrumented gate re-run at HEAD `5c8752d`
  (`PCLXS_DIR=~/pclxs PCL_V2_AUDIT_LOG=… tools/prove-core`): **green,
  131 files / 4652 tests, `Result: PASS`, and exactly the same 27 events
  in the same six TODO families + 5 DIE** as
  `docs/gate-v1-route-audit-s352.md` reports.  The measurement stands.
- All 15 multi-segment events read back from the audit log ARE Sub::Quote's
  canonical shape: `{ my $A = ${$_[1]->{"\$A"}}; …; package X;
  no warnings 'closure'; sub NAME { … } }` — every leading statement is a
  `my` whose initializer touches only `$_[1]` (magic `@_`) and string
  literals.  **Zero free names in 15/15.**
- The two M2 (poisoned condition-my brace-deref) events come from the real
  site_perl `Method::Generate::Constructor`/`::Accessor`, and the `${name}`
  / `${asserter}` text that trips the blocker is **string interpolation**
  (`"_set_${name}"`, `"${into}::${asserter}"`), not a code-level deref.

## §1 — RULING: option (b+), and the flip stays HELD until three populations measure zero

**(c) is rejected** — it contradicts §5a.2, and "Moo's accessors silently
die at the flip" is the exact failure mode the guardrail was written
against.  **(a)-as-#153-first is not required** — none of M1–M6 lives in
PExpr's `$end_pars` term machinery (the #142 minefield); they live in
Parser2's collapse/rename/capture passes, which have been safely extended
all through E1–E4 pre-work.  So: **fix all six families pre-flip, cheapest
first, with a stop-rule on the one genuine feature (M5).**

### 1.1 The s345 §2 premise is SUPERSEDED; standing amendment to §5a.2

s345 §2's "residual multi-segment … ZERO measured events, rephrase
perl-shaped" conflated two shapes behind one die text: the true
multi-switch (`package A; …; package B;`) and the leading-statements shape.
The ruling survives ONLY for the true multi-switch.  Root cause: the s342c
audit measured sweep + board and never `Pl/t/`.

**Amendment (binding, goes with §5a.2):** the live-v1 audit's populations
are **sweep + CPAN board + the Pl/t gate itself**, and the step-2 flip's
precondition is **zero TODO-class events across all three**, re-measured in
the step-2 session.  DIE-class events are exempt (they become `$@`,
unchanged — concur with the audit doc, no work).  Corollary already in
memory: point `PCL_V2_AUDIT_LOG` at the population you are claiming about.

### 1.2 M1 (#247) — APPROVED as the narrow predicate, whitelist-shaped

Widen the #226 collapse to accept segment 0 when **every** statement in it
is a `my` declaration whose initializer is built ONLY from whitelisted
material: lexical symbols, magic variables (`@_`/`$_[n]`, `$_`), literals,
operators/casts/subscripts over those.  **Anything unrecognized refuses**
(rule 12: the missing case announces, never falls through) — the predicate
is a whitelist, not a blacklist of known-unsafe things, because the
#240-style residue risk here is a scanner that MISSES a spelling (the s332
§9 writes_args lesson), and a whitelist converts that miss into a loud
refusal instead of a silent wrong.

Mechanics: the swept-in `my` statements execute inside the region thunk,
i.e. under `*package*` = X — the predicate exists precisely to make that
binding unobservable.  Do NOT build multi-section eval assembly; the true
multi-switch shape stays refused.

Probe battery (acceptance):
1. The live Sub::Quote shape end-to-end (Moo accessor + constructor gate
   rows) — value-correct natively; **gate audit events 15 → 0**.
2. INVERSE (the breaking case, per the standing rule): a leading `my` with
   a free package variable — `eval 'my $v = $V; package X; our $V = 9;
   $v'` — must still refuse (perl resolves `$V` in the CALLER's package;
   sweeping it into the region would resolve it in X = silent wrong).
3. INVERSE: leading `my` with a bareword call — `my $v = f(); package X;
   …` — must still refuse.
4. Caller-lexical capture through the alist still works from a swept-in
   initializer (Sub::Quote's `${$_[1]->{…}}` is args-only, so add an
   explicit probe: single-quoted eval whose leading `my` reads a captured
   caller lexical, value vs perl).
5. `use` inside the region still imports into X when leading `my`
   statements precede the package statement (the #226 "package enter forms
   lead the BODY / `:into` X" ordering must survive the widening) —
   `eval 'my $x = 1; package X; use constant K => 5; K'` → 5.
6. Re-run the five #226 blast-radius probes + the s342g INVERSE guard
   (region-less eval gets no region argument).

### 1.3 M2 — pre-flip, the #229/F5 family, fix BOTH halves

Measured: the blocker is a whole-content text regex and the two Moo hits
are the braced **interpolation** spelling inside string literals.  Fix at
the mechanism, not the symptom:
- Teach `_interp_fixer` (M-A) the `"${name}"` braced-interpolation
  spelling — it is the same rewrite it already does for `"$name"`, plus
  braces.
- Narrow the blocker from the text regex to a PPI-level test for a REAL
  code-level brace-deref (Cast followed by a Block containing the bare
  name) — the same precision upgrade `_block_captures_name` made for the
  capture gates.  A real `${name}` deref that the rename cannot rewrite
  keeps the refusal.

Both `_shadow_rename_blocker` and `_state_container_blocker` carry the
same textual check (Parser2.pm ~3000/~3036) — fix the shared logic once
(CLAUDE.md 11), not two copies.

### 1.4 M3 — pre-flip, small

v2 already parses `$#[0]` as element access on `@#`; it only never
forward-declares `@#`.  Add `@#` to the punctuation-array
forward-declaration set and delete the gate (Parser2.pm:507).  The two
`transpile-test-04.t` rows are value assertions, so a refusal ruling is
not available here; if this turns out to be more than the missing
forward-declare, stop and ask.

### 1.5 M6 — pre-flip, capture-scan false positive

`sub loop { for my $e (@_) { … } }` has no capture: the `for my $e`
loop-head declaration is the sub's own shadow.  Teach
`_block_captures_name`'s shadow discount that a foreach-loop-head `my`
declares for the loop body.  INVERSE probe (the breaking case): a file
lexical used in a foreach BODY **without** a loop-head `my` of that name
must still gate.

### 1.6 M4 — pre-flip, canonicalize the rename like the checker

`_check_my_spanning` already resolves uses canonically (`$mix{k}` → `%mix`
via PPI `->symbol`, `_canon_refs_in`); the spanning RENAME still refuses on
a bare-name collision.  Route the rename's use-collection through the same
canonical resolution so `$mix` (scalar) and `%mix` (hash, reached via
`$mix{k}`/`@mix[…]`) rename independently.  Note the test row's own
comment calls itself a guard edge — "an edge the rename must REFUSE still
runs correctly via the v1 fallback"; post-flip that premise is void, so
the row's DESCRIPTION should be updated in the same commit to say the edge
now renames (the assertion itself — value vs perl — is unchanged; this is
not a weakening).

### 1.7 M5 — the one genuine feature; one-session cap + stop-rule

The static-variable idiom: a block-scoped `my` captured by a nested named
sub (`{ my $x = 2; sub capinner { $x } }`), with string evals reading `$x`
in both scopes in the guard row.  v2 hoists named subs outside the `let`s,
so the capture needs a cell.  **Route through the existing promotion
machinery** — the `_file_lex_renamed` cell-promotion path that
`_check_sub_captures`/`_hoist_nested_sub` already whitelist — extended to
block scope, rather than any new mechanism.  The risk is the eval-alist
interplay (the alist must answer the INNER cell inside the block and the
outer lexical after it); the transpile-test-01b row is exactly that probe.

**Cap: one session.  Stop-rule: if the promotion route needs new rename
machinery beyond the existing passes, or touches PExpr's term region at
all, STOP and write the ask** — at that point (and only then) the
"#153 first, queue reversed" option is live.

### 1.8 Order and re-measurement

Fix order: **M3 → M6 → M2 → M1 → M4 → M5** — cheapest first so the audit
shrinks monotonically and a stall strands the least.  Re-run the
instrumented gate after each family; a family is closed when its events
read 0 AND its named rows pass with the family's gate deleted.  M1
additionally re-runs the board's Moo dists under the audit log (the board
never showed these events — Pl/t's moo-01.t was the only population
exercising Sub::Quote under instrumentation; confirm the board agrees
after the fix).

Step-2 rephrase list after this work: **true multi-switch + F6 + the M1
predicate's refusals (unsafe leading statements)** — each perl-shaped
(`PCL: unsupported in string eval: …`) with a `docs/not-supported.md`
entry naming its owner task, per §5a.3.  Nothing else may remain.

## §2 — bundle mode (step 1): APPROVED as shipped

`ef1b3de` reviewed: one-line port through `parse_with_fallback`, quadruple
green, and the stale "recursively processes dependencies" comment
corrected.  No conditions.

## §3 — F8 stale-gate deletion + #246: APPROVED as shipped

The gate claimed a divergence the carve-out had already fixed (`68ab668`),
and the shape it missed (lowercase bareword subscript) is wrong
IDENTICALLY under v1 — a fallback that buys nothing is exactly what E4.1
deletes.  #246 is correctly filed as non-blocking: no v1 dependency, and
the faithful fix (resolve at eval runtime: call the sub if one is visible,
else string) is a runtime-resolution design, not a transpile-time guess.

## §4 — flaky glob-01.t row: noted

#180/#215 load-noise family; standalone-green + non-reproducing.  No
action, correctly not chased.

## Execution note (added later in s353)

M3, M6, M2, and M1 were executed the same session — see
`docs/e41-m-work-handoff-s353.md` for what shipped, the two extra layers
M2 unmasked (the self-referential-my-init false positive and the
destructive fat-comma tree rewrite), the M1 flattened-block second arm the
real Sub::Quote shape required, and the pre-existing #163-family
divergences the probes surfaced.  Gate TODO events 22 → 2 (M4 + M5).

## Queue after this ruling

#248 M3/M6/M2 (cheap trio) → #247 M1 → #248 M4 → #248 M5 (cap +
stop-rule) → three-population zero re-measure → #242 step 2 → #243 →
#244 → STOP → Fable #153/E5.0.
