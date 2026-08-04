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
| F1 | `eval-mode multi-segment (top-level package statement)` — **NARROWED s342g** | 6 + 18 = **24** | TODO | the **Role::Tiny idiom**: `eval "package X; use Role::Tiny; …"` (all 18 board events) |
| F2 | `eval-mode trailing my/our declaration (value-losing let)` — **CLEARED s342f** except one shape | 6 + 4 = **10** | TODO | `eval "our $VERSION = '1.01'"` (the whole CPAN half); `eval 'my ()'`; perl-syntax-error probes |
| F3 | `block-form arg body captures live lexical` | 0 + 8 = **8** | TODO | Try-Tiny `basic/finally/named/when.t`, S-L-U `first/pair/reduce/rt-96343.t` |
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
- **F3 is a DELIBERATE gate, not a defect** — task #26 shipped it to avoid the
  Try::Tiny catch-block miscompile. It is nonetheless a live v1 dependency:
  eight `.t` files in the CPAN board transpile through v1 today, so removing
  v1 turns the gate into a hard failure for all of them.
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
