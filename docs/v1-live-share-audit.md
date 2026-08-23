# The live v1 share — E4.1 rule-2 audit (task #225, measured s342)

`docs/v2-opus5-execution-plan.md` §5a.2 makes one demand before E4.1 step 2
turns the v2 gate into a hard error: **the live v1 share must be ZERO, and
every `pipeline=v1` hit found is pre-work, never an acceptable loss.**

This file is the measurement. It is not a plan — the scheduling call belongs
to the planning session.

## How it was measured (and why the earlier method under-counted)

s341b answered the question with a **cache grep** (`grep -ar pipeline=v1
~/.pcl-cache` on a cold cache) and found exactly two hits. That method is
blind by construction to every v1 route whose output never becomes a cache
entry — which is most of them: `eval $str` transpiles, `fresh_perl`/`runperl`
child scripts, and any `.t` file the runner transpiles straight to a temp file.

s342 replaced it with a **file side-channel at the single fallback point**
(`pl2cl`'s `parse_with_fallback`): with `PCL_V2_AUDIT_LOG=<path>` set, every
v1 route appends one line — `class`, `method`, `subject`, `error`. It records
what the cache cannot see, and unlike `PCL_V2_VERBOSE` it does not write to
stderr, which a sweep folds into TAP (the s333 banner lesson).

Two classes, and the distinction matters:

- **TODO** — the die came from Parser2 about *itself*: `Parser2 TODO: …` or
  `Parser2: PPI parse failed`.  v2 could not take the file; this is the real
  gap, and after the flip it becomes a user-visible error.  (The classifier
  keys on the `Parser2` prefix, not on `TODO:` alone — s342d, when the PPI
  failures of F4 were landing on the wrong side of the split.)
- **DIE** — v2 raised a *Perl-level* error (e.g. "Can't modify non-lvalue
  subroutine call in assignment") and the fallback retried it on v1, which
  raises the same thing. Not a gap: after the flip the same text simply
  reaches `$@` one pipeline earlier.

Both runs below were on a **wiped `~/.pcl-cache`**, at `46f8a38`.

## What is still on v1

Full perl-tests sweep (108 files): **24 events**.
Four-dist CPAN board (Try-Tiny, Role-Tiny, Sub-Uplevel, Scalar-List-Utils):
**36 events**. Seven distinct families — **F5 cleared in s342d, six left**:

| # | Family | Events (sweep + board) | Kind | Where it shows |
|---|---|---|---|---|
| F1 | `eval-mode multi-segment (top-level package statement)` — **CLEARED s346 (task #226)** | 6 + 18 = **24** → board **0** | TODO | was the **Role::Tiny idiom**: `eval "package X; use Role::Tiny; …"` (all 18 board events) |
| F2 | `eval-mode trailing my/our declaration (value-losing let)` — **CLEARED s342f** except one shape | 6 + 4 = **10** | TODO | `eval "our $VERSION = '1.01'"` (the whole CPAN half); `eval 'my ()'`; perl-syntax-error probes |
| F3 | `block-form arg body captures live lexical` — **CLEARED s346 (task #78)** | 0 + 8 = **8** → **0** | TODO | was: Try-Tiny `basic/finally/named/when.t`, S-L-U `first/pair/reduce/rt-96343.t` |
| F4 | `Parser2: PPI parse failed` | 6 + 0 = **6** | TODO | `/tmp/pcl_fp_*.pl` — `fresh_perl`/`runperl` children |
| F5 | perl **core modules** still gating — **CLEARED s342d** | 0 + 5 = **5** | TODO | `CPAN::Meta::Requirements::Range` (poisoned cond-my `$err`, ×3); `ExtUtils::MM_Unix` (self-referential my-init with a below-assignment tail, ×2) |
| F6 | `oversized top-level run form (73769 chars > 64000)` | 1 + 0 = **1** | TODO | a deliberate v2 refusal — the form would exhaust the SBCL compiler heap |
| F7 | `PCL: Can't modify non-lvalue subroutine call in assignment` | 5 + 0 = **5** | **DIE** | chop.t's four `eval 'chop($x) = 1'` rows (+1 inline) |

### Notes per family

- **F1 is NARROWED (s342g, task #226) to one missing piece, and it is NOT what
  the E3 comment assumed.**  All 18 board events are the Role::Tiny idiom
  (`eval "package X; use Role::Tiny; …"`); **23 of the 24 are exactly one
  LEADING `package` statement with no further switch** — an EMPTY first segment
  plus X's, not a real multi-section assembly.  Dropping the empty head makes
  the assembler accept it, and that was **tried and reverted**: the result is
  silently wrong (`sub f` emits as `pl-f`, read in `:pcl`, while the caller
  looks up `X::pl-f`).  The missing piece is making the section's symbols
  resolve in X.  `(in-package |X|)` reaches only the free-variable-free case
  (otherwise the body is inside the thunk's lambda); the reuse path is the
  QUALIFIED emission E1.5/D1-lite already built for nested packages.  Needs a
  design call on that blast radius before implementing.
- **F1 is CLEARED (s346, task #226) — board events 18 → 0.**  The design call
  came in `fable-answers-s345.md` §2, and the implementation is the reuse it
  named: the leading `package X;` is simply NOT CONSUMED at segment level, so
  it reaches `_lower_block`'s D1-lite nested-package path, which pushes X onto
  the Environment while the SECTION package stays the eval's root — exactly the
  `current ne cur_pkg` condition `_sub_name_for_emission` and `_lower_our_decl`
  already qualify on.  Three supporting facts had to be supplied, each by
  feeding an EXISTING mechanism rather than adding one:
  1. the segment-level sub extraction is skipped for such a segment, so the
     subs stay in the statement stream and are named through
     `_sub_name_for_emission` after the push (this is the s342g silent-wrong);
  2. the package ENTER forms lead the eval body, ahead of the defs/sched
     interleave — a `use` lowers into sched, and its import records the package
     in effect (Role-Tiny `create-hook.t` recorded `main` until this moved);
  3. that `use` gets `:into "X"`, because the eval body is READ in `:pcl` and
     `p-use` takes its target from `*package*` — supplied by setting the two
     facts v1's existing `:into` branch keys on (`_seam_outer_pkg` +
     `_block_depth`), not by a second predicate.
  **Residue, GATED not shipped**: an `our` declared in the region and read back
  UNQUALIFIED is silent-wrong (v2's native emitter has no equivalent of
  ExprToCL.pm ~900's our-qualify branch), so that shape keeps the v1 retry —
  task **#240**, with the measurement and the rejected rename approach recorded.
- **s348 (#240 step 1)**: that residue gate was narrowed to DECLARE-THEN-USE
  (RULED s347 §1.2) — a WRITE-ONLY `our` (`our $VERSION = …`, `our @ISA = …`)
  now collapses natively, since only a *use* can mis-resolve.  Added in the
  same predicate: `our` + a symbolic (`Cast`+`Block`) deref, which the
  narrowing would otherwise have made a NEW silent-wrong (a symbolic ref names
  the variable without a sigil, so no token scan sees it).
  **And the residue is WIDER than #240 recorded**: EVERY unqualified package
  global in the region binds to the CALLER's package
  (`eval 'package F2; $Zz = 5; 1'` → PCL `$main::Zz`, perl `$F2::Zz`), because
  the free-var scan makes it a thunk parameter and `p-eval-lex-lookup` resolves
  an alist-miss with `(intern … *package*)`.  Verified identical at `41907a9`,
  so it is s346's, not step 1's.  **Measured: ZERO live events** — all 20
  eval-region collapses across the whole F1 source (Role-Tiny + Try-Tiny) have
  an EMPTY free-variable set.  Not gated (the only compile-time predicate also
  refuses the legitimate caller-lexical capture); escalated with the
  measurement and a possible cheaper step-2 route in
  `docs/opus5-review-requests-s348.md` §2.
- **F2 is CLEARED (s342f, task #227) except one shape**, and its CPAN half was
  never a syntax probe: all four board events are `eval "our $VERSION = '…'"`,
  a routine module idiom.  The gate refused **every** `our`, though
  `_lower_our_decl` already returns the assignment expression as its only form
  — so the value was the tail all along.  A no-init `our` now emits the READ
  too (v1 answered that one with the emitted variable NAME, a silent-wrong),
  and `my ()` — legal Perl that declares nothing, which my.t asserts leaves
  `$@` empty — lowers in both statement and tail position.  A bare multi
  (`my ($c,$d)`) tail is the LIST of its names, lowered through the ordinary
  expression machinery so both context rules come for free.
  **Residual, recorded not fixed:** `my($a,$b),$x,my($c,$d)` (RT #126844) —
  a declaration buried in a comma expression, which is the #138 family.  Its
  test asserts nothing (`pass()`), and *both* pipelines get the value wrong
  today (perl 5 elements, v1 0), so the flip costs nothing there.
- **F3 is CLEARED (s346, task #78) — 8 board events → 0.**  It was a DELIBERATE
  gate (task #26, to avoid the Try::Tiny catch-block miscompile), guarding the
  v1-seam HOIST of a block-form arg's `--anon-block-N--` defun out of its
  lexical `let`.  The #78 re-host removes the hoist: the `&`-prototype block
  now lowers as an inline lambda AT THE CALL SITE (the `_v2_embedded_body`
  route the anon-`sub {}` sibling already used), and when the embed hook
  declines it takes v1's `$return_lambda=1` text — also in place.  So no defun
  reaches the drain and the gate has no producer left; it stays as the drain's
  backstop until E4.1 step 3's reachability pass retires it.
  *Measured*: the two probes that used to gate now match perl; board file
  statuses identical to the s343 snapshot except S-L-U `reduce.t`
  21 ok/11 → **23 ok/9** (t25 "reduce in list context yields only final
  answer", t32 "missing SMG rt#121992" — both v1 defects, both agree with perl
  now).  `baselines/cpan-scoreboard.tsv` edited for that one row.
- **F6 is NARROWED (s346b): its one event is NOT a file-level transpile.**
  Transpiling all 111 `perl-tests/*.t` with `PCL_V2_AUDIT_LOG` set produced
  **zero** v1 routes of any family (the log file is not even created; the
  mechanism was sanity-checked in the same run against a known gating input,
  and through `xargs`, so this is a real negative, not a plumbing failure).
  So the oversized run form arises at RUN time — a string `eval` or a
  `fresh_perl`/`runperl` child during the sweep.  To locate it, the sweep
  itself is needed:
  `rm -rf ~/.pcl-cache/* && PCL_V2_AUDIT_LOG=/tmp/f6.tsv perl sweep-perl-tests.pl --jobs 8`
  then `grep -a oversized /tmp/f6.tsv` — column 3 is the source.  Do that
  BEFORE designing the chunking: the ruling's "split at top-level statement
  boundaries" assumes the run bucket has several forms, but the bucket is
  ALREADY one form per top-level statement (`@runtime` is mapped through
  `_gate_oversized_run_form` element-wise), so a single statement lowering to
  one 73k form — the top-level-`my`-swallows-the-rest shape that
  `_oversized_top_decls` flattens for the common case — would need a different
  split than the one the ruling describes.
- **F5 is CLEARED (s342d, task #229)** — it was two false positives of the same
  kind: a check treating a *binding* as evidence of a live global / a real read.
  `ExtUtils::MM_Unix` gated because the self-init check scanned TEXT, and
  `$attrs{$_}` (a slot of `%attrs`) contains the string `$attrs`; it now asks
  PPI's `->symbol`, which canonicalises an element access to its container.
  `CPAN::Meta::Requirements::Range` gated because the cond-my poison test
  counted `my ($vobj, $err);` as a use; a plain `my`/`state` declaration now
  binds the name, exactly as `foreach my` does after s342b (`our` still
  counts — it *does* create the global).  Board re-measured: 36 → **31**
  events, output byte-identical.
- **F4 is guardrail §5a.4's subject, and it is DIAGNOSED (s342d, #228) —
  5 of the 6 need no work, the 6th is an ASK.**  All six children contain a
  literal **NUL byte**: they are perl's own "sigil `\0` ident" lexer tests in
  `lex.t` (`print $\0eq`, `@\0eq`, `%\0eq`, `&\0eq`, `${*\0eq{SCALAR}}`) plus
  `do\0000000` (`[perl #129069]`).  PPI cannot tokenize a NUL — verified:
  `PPI::Document->new` fails with "Encountered unexpected character '0'" and
  parses the identical source with the NUL stripped; perl runs it and prints
  `ok`.  Five of the six are **already registered skips** keyed on their
  descriptions (`cl/skip-registry.lisp` → `docs/not-supported.md`, "NUL bytes
  (and other control characters) in identifiers"), so the flip changes nothing
  for them: the die is loud, the row still reports skip.
  **The ASK is the sixth**, `[perl #129069] - no output and valgrind clean`.
  It currently PASSES, and it passes *because* v1's `--lenient-ppi` truncates
  the unparseable source to nothing and the assertion expects empty output.
  After the flip v2 dies, the child prints an error, and the row fails.  That
  is a live dependency on lenient truncation — which §5a.4 says is **an ASK,
  not a judgement call**: register it beside its five siblings, or make v2
  tolerate NUL in source.
- **F7 needs no work.** It is worth recording only because it shows
  `parse_with_fallback` catches *every* die, so a raw fallback count
  over-states the gap. The audit's TODO/DIE split is what corrects for it.

## What this does NOT say

The counts are **events, not files**: one `.t` file that evals in a loop
contributes many. And the two runs cover the perl-tests corpus and four CPAN
dists — a wider CPAN board would very likely add families, so "seven" is a
floor, not a total.

## Reproducing

```bash
rm -rf ~/.pcl-cache/*
PCL_V2_AUDIT_LOG=/tmp/audit.tsv perl sweep-perl-tests.pl --jobs 8
rm -rf ~/.pcl-cache/*
PCL_V2_AUDIT_LOG=/tmp/audit-cpan.tsv perl tools/cpan-scoreboard.pl --jobs 4 \
  --timeout 120 ~/.cpan/build/Try-Tiny-0.32-0 ~/.cpan/build/Role-Tiny-2.002004-0 \
  ~/.cpan/build/Sub-Uplevel-0.2800-0 --no-dist-lib ~/.cpan/build/Scalar-List-Utils-1.70-0
cut -f1,4 /tmp/audit.tsv | sed 's/ at .*//' | sort | uniq -c | sort -rn
```

**Trap (s341b, still true):** a `git worktree` at an older commit shares
`~/.pcl-cache`, so its entries pollute a marker grep — filter on the current
`gen=`. The audit log has no such problem: it records only the process that
wrote it.
