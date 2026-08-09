# Fable answers — s378 review (s379, 2026-08-09)

Review of `5d94161` (#239) + `4f35ffa` (#237 re-scope, docs only), and rulings
on the six asks in `docs/opus5-review-requests-s378.md`.

## Verdict on the commits

**Both APPROVED as shipped, with two review-probe fixes of my own on top
(`2af263f`, §7).**  Independently re-verified:

- Pl/t gate cold-cache: **133 files / 4787 tests PASS** at `9177f99`
  (and 133/4788 after my fixes — +1 is the new guard row).
- corpus-diff at `9177f99`: byte-identical across 111 files (so the 8-file
  claim in the request refers to the #239 commit against ITS parent; at the
  review point emission is stable).
- Probes vs perl, all matching: the nine-block-kind guard shape, `local $v`
  in a region (localizes the requalified global, restores after),
  string-`eval` inside a region (writes `P::ez` — the #240 thunk covers it),
  nested switch (`P`→`Q`→back), `while (my $l = …)` and `for my $i (…)`
  head bindings, anon-sub signature.
- The probes that did NOT match are §7 — both now fixed or filed.

## 1. ASK — shipping wider than the title + absorbing the §9.1 sibling: APPROVED

Correct call, both halves.  The cause was single (one top-level CL form, the
reader interns before the nested `in-package` runs), so covering all nine
block kinds is the fix's natural extent, not scope creep — a two-kind fix
would have left seven silent-wrongs with the same cause line.

On the deviation from §7's "file it as a companion": **approved, and the rule
is now refined**.  "File a companion, do not absorb" applies when honoring
the sibling would require NEW mechanism beyond what the ruling sized.  Here
clause (a) of the ruled resolver ("in-scope `our` → declaring package") *is*
the sibling's fix — deliberately leaving the bare-block branch wrong would
have meant writing a resolver with a known-false arm.  Absorbing was right
because it was (i) zero new mechanism, (ii) loud (commit message, DECIDED.md,
session log), (iii) guarded by a row in the same commit.  All three conditions
are the bar; absent any one, file the companion.

## 2. ASK — #287 ($a/$b immunity): shipped choice right; the fix is TWO HALVES, ONE COMMIT

Keeping the immunity was the right interim: `3 1 2` silently unsorted from
every `sort { $a <=> $b }` in a switched region is the worst failure mode in
this codebase; Sort-Versions' 31 rows are a visible, blessed cost.  The
measured both-ways probe is exactly what the ruling asked for.

**The real fix, ruled:** one commit, two halves, never split —

1. drop `$a`/`$b` from `%PKG_SWITCH_IMMUNE_VARS` (the requalifier then
   rewrites them like any global: `$a` → `$X::a` inside the region);
2. make the sort lowering bind **the pair the comparator block actually
   reads**.  After requalification the block's spelling is consistent —
   a region comparator reads `$X::a`/`$X::b`, an unswitched one reads
   `$a`/`$b` — so the lowering's job is to bind the spelling family present
   in its block rather than hard-coding the section's two symbols.
   Equivalently it may compute the perl package in effect at the sort site;
   `_pkg_in_effect_at` now exists and is the shared walk to use (rule 11 —
   do not write a third package-in-effect resolver).

Perl subtleties that MUST be probed in the commit (each is a known perl
gotcha, not an invented case): `sort Other::cmp @l` sets the CALLER's pair
(a named comparator in another package cannot see them — keep matching
perl's behavior, whatever PCL does today, and record it); a named comparator
in the SAME switched region; file-level `package X;` + sort (already correct
via the reader — must not regress); nested sort inside a switched region;
`reverse sort`.  Inverse guard: the four in-region comparator rows shipped in
`transpile-test-09.t` flip from "unsorted would be the bug" to "sorted via
the region pair".

**Ordering recommendation** (queue is the user's): #287 immediately after
#237 — it is one contained session, the mechanism is fresh, and it carries a
measured full-dist flip (Sort-Versions 65/31 → 96/0).  It should not jump
ahead of #237, which the user already sequenced.

## 3. ASK — #237 fix shape: (b) as a SHARED SCANNER, not a shared interpolator; #286 stays out

Ruled **(b′)**: the rule-11 "one shared upstream point" is the **variable-
reference scanner**, not the whole interpolation pipeline.  Do not literally
route regex text through the dq-string interpolator: the two consumers
disagree about everything except variable references (`\b` is a word
boundary in a regex and a backspace in a dq string; `[`, `{`, `$` at end all
differ).  The shape that is both correct and deletable-toward:

- ONE scanner that walks interpolating text and emits variable-reference
  events — sigil, name (qualified names are one name — the s378 lesson),
  and the longest plausible subscript/arrow chain with spans;
- the dq-string consumer takes every chain (current behavior, unchanged);
- the regex consumer applies the **intuit_more-style classifier** to each
  bracket group before accepting it as a subscript: `[abc]`, `[^a]`,
  `[a-z]`, `{2,3}`, `{2,}` stay regex syntax; `[0]`, `[$i]`, `[-1]`,
  `{'k'}`, `{$k}` become subscripts.  perl's own heuristic (toke.c
  `intuit_more`) is the reference; match its verdicts, and where genuinely
  ambiguous, match perl's observed behavior by probe.
- `_gen_interp_regex_pattern` then shrinks to a consumer of the shared
  scanner; its hand-rolled name-walk is deleted.  That deletion, not the
  new feature, is the acceptance criterion for calling this "the rule-11
  answer".

This is **not** parked to #153/E5: Text-Balanced B3 (4 board files) is the
live cause line, and the scanner consolidation is exactly the first step of
the wider interpolation cleanup (see `docs/var-handling-review-s379.md` —
the codebase has 26 scanner sites; this item retires one of the two worst).

Mandatory inverse guards in the same commit: `/$x[abc]/`, `/$x[^a]/`,
`/$x{2,3}/` (pass today, must keep passing), plus the new `qr/\G$_[1]/`,
`/$a[0]/`, `/$h{k}/` shapes, and one dq-string row proving strings did not
move.

**#286 is NOT folded in.**  It is `intuit_curly` (hash-constructor vs block
at term start) — a different heuristic consulted at a different site, with
no shared code beyond the word "intuit".  It stays deferred under its own
re-open trigger (a real cause line naming `f {$k, $v}`).  What #237 should
leave behind for it: put the bracket classifier in a named, reusable
predicate module-level home so a future #286 can live beside it, not inside
it.

## 4. ASK — the two shared-rewriter changes: APPROVED

Both are strictly correct and verified:

- `_interp_canon` running name captures to the end of a qualified name, and
  `(?!::)` on `_interp_fixer`'s unbraced arms: perl reads `"$Foo::bar"` as
  the qualified global, never `$Foo` then text; the braced `"${x}::y"` IS
  `$x` then text and correctly keeps no guard.  I checked every arm: the
  guard is present on unbraced `$`, `@`, and `$#`, absent on all braced
  forms.  Gate + corpus byte-identity confirm no collateral movement.
- `_symbol_is_declarator`'s optional `$kw_rx`: default unchanged, callers
  unaffected, and the `local`-is-not-a-binder reasoning is right (probed:
  `local $v` in a region localizes the requalified global).

For the record: these two fixes taught the qualified-name rule to scanners
#2 and #3 of the many — the review doc written this session counts 26
hand-rolled interpolation-scanning sites.  Each such fix is correct and each
deepens the copy problem; the consolidation direction is §3's scanner and
the s379 variable-handling review.

## 5. ASK — two lists: CONFIRMED SEPARATE

`%PKG_SWITCH_IMMUNE_VARS` and `%runtime_vars` answer different questions
("can a switch re-home this" vs "does this need a defvar") with overlapping
membership; the paired keep-in-step-by-cause comments are exactly the
s370-comma-walks / s376-two-predicates precedent.  One addition required
when #287 lands: the `$a`/`$b` entry leaves the immune set, and BOTH
comments must be updated in that commit (the immunity rationale currently
lives in each).

## 6. ASK — guard-row placement: CONFIRMED, wall-time headroom wins

CLAUDE.md 6's constraint is the file's RUN TIME; thematic grouping is a
nice-to-have.  `transpile-test-09.t` at 48→50 s over `transpile-test-04b.t`
at 149 s is the right call.  Standing rule: a family's guard rows may
scatter across files; the anchor is the task/DECIDED entry naming where they
live, not co-location.  (My s379 rows also went to -09 on the same rule.)

## 7. Review findings — two fixed (`2af263f`), one filed (#288)

The resolver probes turned up three divergences the s378 verification could
not see (all three shapes occur in no corpus/board/suite file):

1. **FIXED — signature params were invisible to the resolver** (new in
   `5d94161`).  `sub f ($x) { package P; $x + 1 }`: perl 42, PCL 1 — the
   head scanner `_decl_binding_in` only recognized `my/state/our` Words, so
   `$x` classified as a global of P and the body was requalified to `$P::x`
   while the seam still bound the plain lexical.  Silent wrong value; the
   commit's own rule-12 comment promised a die for exactly this
   unreadable-binder case and the code delivered a wrong answer instead.
   Fix: the head scan reads `Token::Prototype`/`Structure::Signature`
   through the SAME textual discriminator `_is_pure_prototype` uses and the
   SAME `_signature_param_specs` splitter the seam binds with (rule 11); a
   Symbol in a default expression stays a use; a pure prototype binds
   nothing.  (The anon-sub signature path was already correct — worth
   knowing: the two sub shapes take different lowering routes.)
2. **FIXED — false die on keyword-shaped hash keys.**
   `%h = (my => 1); if ($h{my}) { package Q; … }` died "unclassifiable `my`
   declarator" on legal perl.  Fat-comma keys, bare subscript keys, method
   names and `my sub` now contribute no binding; the die stays for genuinely
   unreadable declarator shapes (the s372 gate-SET bar: a die must be a
   compiler self-inconsistency, never legal-perl rejection).
3. **FILED #288 — bareword CALL in a switched region, pre-existing.**
   `sub t { package P; hello(); }` calls `main::hello` where perl dies
   `Undefined subroutine &P::hello`.  Identical at `c99ca41`, so not #239's
   regression; the request's "calls are already homed to X correctly" holds
   only when the sub exists in X.  Wrong-callee is silent-wrong, not
   error-text fidelity.  The task carries the discriminating measurement.

Verification of the fixes: gate 133/4788 cold-core PASS; corpus-diff
byte-identical (111 files); all 21 `lib/` shims + `pack-impl.pl` + `mro.pm`
byte-identical vs `9177f99` after preamble normalization; signatures.t
single-file sweep fail rows byte-identical to HEAD (888/90).  Gen bumped
v2-127 → v2-128 (corpus-invisible emission change; cached module transpiles
from the v2-127 window could carry the broken shape).

One measurement note for the record: a single-file sweep's `.fails.tsv` can
hold more RAW rows than the baseline holds KEYS (signatures.t: 90 raw vs 34
baseline; `sweep-diff` correctly reports 0 new — its join key deduplicates).
Do not read raw `wc -l` against baseline row counts; compare through
`sweep-diff` or by key.

## Queue after this

**#237** (per §3's b′ ruling) → **#287** (per §2's two-halves ruling) →
v0.1 track (#277–#283), `#284` sized first; #288 is filler-eligible
(measurement first).  #153 FOLD chunks 2–3 remain mine.  Board re-bless
still waits on the #208 per-file audit.  The s379 variable-handling review
(`docs/var-handling-review-s379.md`) is the planning input for how #153/E5
and the scanner consolidation fit together — read it before scheduling any
new rename-pass work.
