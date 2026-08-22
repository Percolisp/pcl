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

## s423 — #418 widened, the B-finisher (`docs/opus5-review-requests-s423.md`): APPROVED as shipped, merged `f02fe2a` (fast-forward); two review fixes on top

**Independently re-verified:** every diff hunk read (CLForm's `cl_sym`/`cl_pkg`
— identity on ASCII, the correctness condition —, the runtime's
`%pcl-invert-case` guard, the `pl2cl` eval-preamble twin, the `is_filehandle`
leaf gate, the emission sites in ExprToCL/Parser/Parser2/GlobalPartition);
B's 21-row guard passes in its tree; the registry edits are pure removals (0
additions); a probe of my own vs perl 5.40.3 — NFKC ligature `$ﬁ` vs `$fi`,
Greek case pair `$Φ`/`$φ`, Cyrillic scalar+hash, fullwidth subs called bare /
`&{"…"}` / `\&` / `goto &`, methods via string class and `->$m`, `can`/`isa`,
symrefs, stash `exists`, string eval inside the fullwidth package, `sub
ＦＯＯ::ｅｖ` defined by eval, `local`, fullwidth label `next`/`last`, bareword
`ＦＨ` open/print/readline/close, in-memory `$ｆｈ`, closures, heredoc, qualified
`$main::Ｖ` — **22 of 24 lines identical** after the two fixes below; the two
remaining lines are pre-existing and ASCII-identical.  (My first probe file
could not be transpiled at all: a fullwidth `for my $ｉ (…)` variable is the
known #422.3 PPI LEXER failure — pre-existing on both trees, F's item.)

**The two pre-existing lines, attributed:** (1) `Foo->can("nope"); exists
$Foo::{nope}` is 1 in perl (a failed method lookup VIVIFIES the stash entry)
and 0 in PCL — perl-internals class, ASCII-identical, recorded on #430 (the
stash-snapshot task) rather than filed anew.  (2) `"$Ｘ[$ｉ]"` — see #435.

**Review fixes (mine, in the merge commit), both "a non-ASCII name behaves
exactly like its ASCII twin" residues the probes found:**
- **`_array_index_container` (ExprToCL) built the `$#NAME` array token BARE** —
  `$#Ｘ` read back NFKC-folded as `@X` ("unbound") in code and strings alike
  (the one emission site B's grep did not reach, because it builds the token
  from `$#…` text instead of a Symbol).  Spelled through `cl_pkg`/`cl_sym`.
  **Bonus, ASCII and pre-existing: `$#Foo::Bar::x` emitted `Foo::Bar::@x`,
  which SBCL cannot READ — the file died at load.**  Now `|Foo::Bar|::@x`
  like every other emitter; 0 files in the four populations carry the shape
  (measured), single-segment/bare emission byte-identical.  Guard rows 23–24.
- **The interpolated hash-key autoquote was ASCII-only** (`_interp_hash_key`:
  `/^-?[a-zA-Z_]\w*$/`, the same class the OLD scanner had at three sites), so
  `"$ｈ{ｋ}"` went to the expression path and CALLED sub `ｋ` while `$ｈ{ｋ}` in
  code was right.  Widened to `[^\W\d]` — perl's own identifier class under
  `use utf8`, which the code-side twin (any PPI Word) already accepts.  Guard
  row 22.

**Ask 1 — the generation (`v2-171`, not the brief's `v2-165`): ACCEPTED, and
the reasoning is right** — a rebased branch must carry a number ABOVE main's,
or the cache key is re-mintable.  The collision it caused is MINE: the launch
brief gave F `v2-171` without foreseeing the rebase renumber; F was told to
move to `v2-173` before its final gate.  **Standing rule for the next round:**
the launcher reserves per-agent strings with a GAP above main (e.g. +10, +20,
+30), so a rebase renumber never lands on a sibling's key; the final merge
still renumbers ONCE to a fresh string.

**Ask 2 — the `is_filehandle` gate: the NARROW gate STANDS.**  The mixed
leaf (a PPI Word whose content was overwritten in place with CL text and is
later fed back to `cl_name` as a perl name) is the real defect and is
E5/#243-shaped; filed as **#434** with the acceptance (the gate becomes a plain
`cl_sym`; emission A/B byte-identical), to be done when that seam is touched,
not as a filler.

**Ask 3 — no `not-supported.md` entry: CONFIRMED.**  After this change a
non-ASCII name behaves as its ASCII twin wherever the twin is right and
wherever it is wrong; the remaining absences (#430, #431) are ASCII bugs with
their own tasks.

**Filed by the review, not fixed — #435:** the #410 PPI token repair
(`_merge_unicode_symbols`, Cast+Word → Symbol) runs only on the DOCUMENT
parse; every FRAGMENT re-parse (StringInterpolation `_interp_reparse` /
`_parse_postfix_deref`, ExprToCL's regex consumer, the Parser/Parser2
mini-parses — eleven `PPI::Document->new` sites) skips it, so a non-ASCII
variable inside an interpolated subscript or `@{[ … ]}` is mis-read:
`"$Ｘ[$ｉ]"` SILENTLY reads element 0, `"$Ｈ{$ｋ}"` is empty, `"$Ｘ[$ｉ+1]"` dies
"undefined function pl-ｉ".  Pre-existing; one shared fragment-document helper
that applies the repairs (rule 11); land AFTER F (#422.2 is in the same family).

**The one companion loss — `uni/gv.t` 53/28 → 50/31 — is ACCEPTED as spliced**:
three accidental passes on `local *Ｊ = *Ｊ` (PCL's `local *NAME` loses the
glob's slots, ASCII-identical, **#433**); the same family as s418's bless.t /
split.t un-drops, where a fix stops two wrongs from cancelling.  The fifteen
gains (three mro utf8 files fully passing, four uni files running at all)
are the row prize O1.2 was sized for.  **#431** (AUTOLOAD not consulted for a
qualified call) and **#432** (runpcl's spurious blank line — a measurement
trap) stay filed.
