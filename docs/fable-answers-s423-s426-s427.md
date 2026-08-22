# Fable rulings on the round-2 Opus batches — s423 (#418 finish), s426 (O2), s427 (O3) — written in s425 (2026-08-22)

Three Opus 5 agents ran in parallel worktrees (the s421 pattern; launch
record in `docs/session-log.md` s425 and DECIDED s425).  Each section is
written when that agent's branch was reviewed; the verdict is per batch.

## s426 — #388 consumer 3 + #420 + #422.1 (`docs/opus5-review-requests-s426.md`): APPROVED as shipped, merged `c1983e1` (fast-forward)

**Independently re-verified:** the diff read end to end (ExprToCL's
`_slice_container_form` + its five call sites; InterpScan's braced-EXPRESSION
chain, `_braced_magic_name`, `$#-`/`$#+`; the 664-line StringInterpolation
dispatcher with its three arms); the guard file grew 10 → 12 rows with
nothing weakened; a 52-shape probe of my own vs perl 5.40.3 — the five #420
spellings and their compounds, `qw()` inside a slice subscript, symbolic-ref
slices under `no strict 'refs'`, nested `$$aoa[1]->[0]` / `${$hoh}{a}->{b}[1]`
chains, kv-slices, lvalue and `delete` slices through a reference, `local $"`,
the capture arrays, the in-the-wild `"$$_{code}"` shape — **51 of 51 output
lines identical to perl** in the s426 tree (the only diff is perl's own
stderr warnings, which PCL does not emit — the known #221 class) against 66
differing lines on main before the merge.  The "baseline 18365" in the doc
where s421 measured 18364 is explained: C's s424 hand-edited substr.t 374 →
375 into `docs/pass-baseline.tsv`.

**Ask 1 — was the slice-emitter half (§2) in scope?  YES, ratified as shipped,
and it is now a standing rule:** an acceptance row that cannot close without
a fix in a SECOND mechanism pulls that fix into scope *only* when (a) the fix
is measured to the full bar of ITS OWN change class — here a `Pl/` emission
change: every corpus-diff hunk explained per file and probed vs perl, the
emission A/B over the four populations with every differing file named and
attributed, the full sweep — and (b) it REMOVES a copy rather than adding one
(`gen_kv_array_slice_form`'s `(unbox $r)` was the shape-blind second copy of
the same rule; rule 11).  Either condition missing → file it and leave the
row failing, as the s366 filler rule says.  The s379 "no new scanner fixes"
rule was honoured (the port IS the fix; the escape hatch was not used).

**Ask 2 — the `@{ EXPR }` / `${ EXPR }` unescape asymmetry: CONFIRMED,
preserved.**  "No unmeasured semantic change inside a structural port" is the
right call; #444 carries the discriminating measurement (`"${\ (2*3) }"`
breaks under a blanket unescape).  Its fix is a filler with that shape as the
inverse guard.

**Ask 3 — consumer 2 (the Parser2 rename machinery's private name walks,
`_interp_names` 1.65 s of compile time): its OWN Opus session, scheduled as
O4 right after the O3 merge — NOT behind B3.**  The files are disjoint from
B3's (`Pl/Parser2.pm` + `Pl/VarAnnotator.pm` vs PExpr's term region), so the
two run in parallel.  Its change class is name resolution: the full sweep IS
the gate, plus the gate-SET scan over both populations, plus a compile-time
measurement before/after (the 1.65 s is the metric).  Acceptance = the s379
direction-B bar — the private walks are DELETED, not bypassed, and
`docs/interp-scan.md`'s divergence table for consumer 2 closes row by row.
Added to `docs/plan-post-s420.md` §1 as item 4b.

**Ask 4 — an `ir-spec.md` line: YES, added in the merge commit** as §3.2b
"Interpolation extent" — the braced-NAME-closes / braced-EXPRESSION-continues
rule, the `$#` no-chain rule, the deref-base rule and the "value = what the
equivalent CODE gives" contract.  `interp-scan.md` stays the contract for the
scanner's events; `ir-spec.md` is where a translator reads the semantics.

**Filed by the session, kept:** #443 (wrong-kind deref leniency, shared by
code and strings — a runtime `p-cast-$` / `p-*-deref` referent check, the
#163 family), #444 (the unescape asymmetry).  **Lesson recorded** (the doc's
own): the gate's #355 stderr-aware helper caught a scanner arm the port had
not carried (`"@{+}"`) — the #314 shape — which is exactly what that helper
was built for.
