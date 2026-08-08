# #254 session 1 — measurement of the two big suite families (s363, Opus)

Task #254 step 1–2: for each of the 13 files, the exact gate line, the source
shape it fires on, and the classification **PREDICATE veto** (a conservative
refusal to widen) vs **MECHANISM GAP** (a genuinely unhandled layout).  No
fixes here — step 3 ships from this table.

Method: `./pl2cl < FILE` at `094e6cd` for the gate line, then the pass's own
refusal channel — `PCL_SPAN_DEBUG=1` prints `SPANREFUSE` (the spanning rename,
`_rename_spanning_lexicals`) **and** `CAPREFUSE` (capture promotion), plus
`SPANHIT` from the CHECKER (`_canon_refs_in`) showing which use it saw.  All 13
reproduce, and they collapse to **4 gate messages / 6 distinct causes**.

## Family A — capture / package-spanning (9 files)

| file | rows | gate message | refusal trace | cause |
|---|---|---|---|---|
| io/through.t | 942 | file lexical 'fh' captured by sub testfile | `CAPREFUSE $fh: not captured by a named sub after the decl` | A-i |
| op/svleak.t | 574 | my-lexical 'a' spans a package boundary | `SPANREFUSE a@seg0: sdecls=0 dc=0` | A-ii |
| op/sub_lval.t | 195 | my-lexical 'x' spans a package boundary | `SPANREFUSE x@seg9: blockform decl segment` | A-iii |
| op/attrproto.t | 48 | file lexical 'attrs' captured by sub Q::MODIFY_CODE_ATTRIBUTES | `CAPREFUSE $attrs: family use (@x/%x/$#x) in extent` then `CAPREFUSE @attrs: not captured by a named sub after the decl` | A-iv + A-i |
| re/reg_eval_scope.t | 44 | lexical 'c' possibly captured by nested sub m1 | same pair as attrproto.t | A-iv + A-i |
| op/exec.t | 36 | my-lexical 'quote' spans a package boundary | **no SPANREFUSE at all**; checker says `SPANHIT interp $quote line=215` | **A-v** |
| io/shm.t | 21 | my-lexical 'fetch' spans a package boundary | `SPANREFUSE fetch@seg0: sdecls=0 dc=1` | A-ii |
| op/getppid.t | 4 | file lexical 'first' captured by sub fork_and_retrieve | `CAPREFUSE $first: not captured by a named sub after the decl` | A-i |
| op/taint.t | ~10k | my-lexical 'old_env_path' spans a package boundary | `SPANREFUSE old_env_path@seg0: sdecls=0 dc=1` | A-ii |

### A-v — the one INVARIANT BREAK, and the cheapest win (op/exec.t)

`_rename_spanning_lexicals` pre-filters candidate names with a **text regex**,
`/(?:[\$\@\%]|\$\#)\Q$bare\E\b/`, while the CHECKER resolves uses through
`_canon_refs_in` (PPI symbols + interpolation).  exec.t:215 spells its use
**braced**:

    exec $Perl, '-le', qq{${quote}print 'ok $test - exec PROG, LIST'${quote}};

`${quote}` does not match `\$quote\b`, so `quote` never entered `%spanning`,
the rename never ran, and no `SPANREFUSE` was printed — the checker then died
on a name the rename pass had never even considered.  That violates the
invariant the pass's own comment states ("the rename never refuses a name the
checker will die on", s354 M4).

**PREDICATE.**  Same blind spot M2 fixed for `#226`'s collapse in s353 —
braced interpolation — in a second copy of the idea.  Per CLAUDE.md 11 the fix
is to stop having a second copy: drive the pre-filter from the checker's own
resolver (`_canon_refs_in`), or, if the per-name cost matters, widen the regex
to `\{?\Q$bare\E` and let the existing CANON span test (`$scalar_spans`) throw
out the false positives it lets in.  Probe the breaking case: a name that
appears only as `${bare}` inside a **string literal that is not interpolating**
(`'${quote}'`) must not be renamed.

### A-i — "not captured by a named sub after the decl" (3–5 files)

The promotion only claims a lexical captured by a named sub that appears
**after** the declaration.  through.t/getppid.t capture from a sub defined
*earlier* in the file (perl does not care — a named sub's body is compiled with
the file lexical in scope whichever way the text runs).  **PREDICATE**, but
widening it means the promotion must be ordering-independent, which is a real
change to what "extent" means for the promoted cell — size it before shipping.

### A-ii — `sdecls=0` (svleak.t, shm.t, taint.t; 3 files, ~11k rows)

The rename enumerates **single-scalar top-level `my $x`** declaration
instances; `sdecls=0` means the declaration is not that shape.  In all three
the name is declared some other way (a list `my ($a, $b)`, a container `my @a`,
or not at segment top level).  This is the biggest bucket by rows (taint.t
alone dominates the family).  **MECHANISM GAP** — needs the instance
enumeration to cover the other declaration shapes, which is more than a
predicate tweak.  Size it before touching it; note #84 (container spanning) is
the same question one sigil over and was ruled a v2 gate deliberately.

### A-iii — `blockform decl segment` (sub_lval.t)

The declaring segment is a `package NAME { ... }` block-form segment, which the
rename refuses outright.  **PREDICATE** in shape, but the refusal exists
because a block-form package's lexical scope is genuinely narrower — check
whether the D1-lite block-form lowering (#48) makes the hoisted defvar correct
here before widening.

### A-iv — `family use (@x/%x/$#x) in extent` (attrproto.t, reg_eval_scope.t)

The scalar promotion refuses when a same-named container is used in the extent
(`$attrs` and `@attrs` both live).  Both files then fail again on the CONTAINER
under A-i.  **PREDICATE** — the sigil-exact resolver (`_canon_refs_in`) already
distinguishes `$x` from `@x`; the refusal predates it (the same "sigil-blind
refusal" that s354's M4 note calls out one layer up).

## Family B — poisoned condition-my (4 files, 1284 rows)

| file | rows | gate message | cause |
|---|---|---|---|
| re/regexp_unicode_prop.t | 660 | poisoned condition-my $class (string eval) | B-i |
| re/pat_advanced.t | 441 | poisoned condition-my $match (string eval) | B-i |
| op/my.t | 156 | poisoned condition-my $i (string eval) | B-i |
| op/while.t | 27 | poisoned condition-my $i (multiple declarations) | B-ii |

Both come from `_shadow_rename_blocker`.

### B-i — "string eval" (3 files, 1257 rows)

The cond-rename would produce `$x__cond__N`, and a string eval in the same
region looks the original name up in the s250 capture alist.  **The waiver
already exists for a sibling**: the seam my-shadow rename is exempt
(`$eval_ok`) because `_eval_lexical_alist` strips `$x__shadow__N` back to the
original key, and M-F extended the same idea to spanning renames ("a mangled
rename now registers an original-name → cell pair on each extent segment").
**MECHANISM, but an EXISTING one** — route the cond rename through that same
registration instead of refusing.  This is the highest-value single item in
either family: 1257 rows, and the mechanism is already written and tested for
two other renames.  Check #205 (poisoned-name veto fallback) first — it is
listed as possibly a prerequisite.

### B-ii — "multiple declarations" (op/while.t, 27 rows)

Nested re-shadow of the same name; a single positional rename would merge the
two scopes.  **MECHANISM GAP** — per-declaration (scoped) renaming.  Small file,
lowest priority; the same "process instances innermost/latest first" trick the
spanning rename uses (M-B) is the obvious model.

## Step 3 progress — SHIPPED so far: A-v, #264, B-i

**#264 SHIPPED (`36b4d7f`)** — the span detector now resolves a CODE-level
`${x}` (Cast + Block-holding-one-Word) to `$x`, through ONE helper
(`_brace_name_refs`) that the RENAMER consumes too, so the two agree about what
a use is.  The renamer sets the Word inside the Block, so the shape is handled
rather than merely gated, and the old `${x} deref-block` refusal is deleted.
Gate SET diffed file-by-file over both populations (715 files): 30 failures
before, the same 30 after — **zero new gates**, which is the risk that mattered
when adding detection.

**B-i SHIPPED (`04316ab`)** — 1257 rows, no new mechanism: a cond rename mints
a LET-BOUND `$i__cond__N`, so teaching `_eval_lexical_alist`'s key function the
fourth suffix (beside `__lex__`/`__shadow__`/`__file__`) and passing `eval_ok`
at the cond site is the whole fix.  `state` keeps the refusal (its `__state__`
cell is a defvar, never a let).  Gate set 30 → 27, exactly the three B-i files.

**Per-file, against the v1-era snapshot** (the ratified bar — reported as
measured, not as a family claim; all three scored ZERO before, so the recovery
is +1765 rows):

| file | now | snapshot | verdict |
|---|---|---|---|
| re/regexp_unicode_prop.t | 778/332 | 778/332 | **at snapshot** |
| op/my.t | 51/8 | 52/7 | one row short — t47, filed **#265** |
| re/pat_advanced.t | 936/733 | 1073/596 | short; needs `--timeout 900` to finish |

op/my.t's single differing row is `++my $x->{foo}` inside a sub: PCL emits no
binding for `$x`, so with an outer `$x` in the file the hash persists across
calls (**#265**, pre-existing, visible only because the file now runs).
re/pat_advanced.t's 733 failures are regex-engine families — named captures,
final-sigma casefolding, `\X`, `$REGMARK`, recursion, `(?{ })` — a different
axis from #254 entirely.

**B-i's file set is therefore de-gated but NOT "done" by the snapshot bar**, and
op/while.t (B-ii) still refuses as "multiple declarations".

## Earlier step-3 progress

**A-v SHIPPED (s363, `0832e80`)** — and it was TWO stale text scans, not one:
the pre-filter (fixed with `\{?`) and then, once the name was admitted, the
`${x} deref-block` eligibility check, which was still the whole-file text scan
that s353's M2 had already replaced with the PPI-narrowed
`_has_code_brace_deref` in `_shadow_rename_blocker`.  Both copies now agree
with the checker.  op/exec.t transpiles and reaches **C: 15/0 — exactly its
`perl-suite-run.tsv` snapshot row** (the file's snapshot C_ok is 15, not the 36
this doc's table carried from task #254's list; 15 is the measured number).
The other 12 files are unmoved.

The breaking-case probe for that fix found a PRE-EXISTING silent wrong, filed
**#264**: a CODE-level `${x}` deref of a file lexical across a package boundary
reads EMPTY, because `_canon_refs_in` does not resolve Cast+Block to the symbol
— so the checker never dies and the deref-block guard is never reached.  The
span detector and the eligibility check disagree about what counts as a use.
**#264 should be fixed before A-i/A-ii**: it may change which files gate.

## Recommended order for the rest of step 3
1. **#264** — the span detector must see `${x}`; it is a silent wrong today and
   it can change which files gate, so it comes before the A-family widenings.
2. **B-i** (1257 rows) — route the cond rename through M-F's existing
   original-name→cell registration.
3. **A-iv** (92 rows) — sigil-exact the family-use refusal.
4. **A-i** — needs an extent decision first; size it.
5. **A-ii** (~11k rows, the whales) — MECHANISM; size it and bring the sizing
   back before starting.  **The #254 stop-rule points here**: if it needs a new
   promotion pass rather than a widened predicate, STOP and ask, because that
   work competes with E5, which rebuilds this layer.
6. **A-iii**, **B-ii** — singles, last.

Row counts are the snapshot C_ok values from task #254 / `docs/perl-suite-run.tsv`;
taint.t's is unmeasured (it has never transpiled) and is the reason the family
is worth ~12k rows on paper.
