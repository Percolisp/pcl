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
| F1 | `eval-mode multi-segment (top-level package statement)` | 6 + 18 = **24** | TODO | `eval 'package X; …'` |
| F2 | `eval-mode trailing my/our declaration (value-losing let)` | 6 + 4 = **10** | TODO | `eval 'my $$x'`, `eval 'my ()'` — perl-syntax-error probes |
| F3 | `block-form arg body captures live lexical` | 0 + 8 = **8** | TODO | Try-Tiny `basic/finally/named/when.t`, S-L-U `first/pair/reduce/rt-96343.t` |
| F4 | `Parser2: PPI parse failed` | 6 + 0 = **6** | TODO | `/tmp/pcl_fp_*.pl` — `fresh_perl`/`runperl` children |
| F5 | perl **core modules** still gating — **CLEARED s342d** | 0 + 5 = **5** | TODO | `CPAN::Meta::Requirements::Range` (poisoned cond-my `$err`, ×3); `ExtUtils::MM_Unix` (self-referential my-init with a below-assignment tail, ×2) |
| F6 | `oversized top-level run form (73769 chars > 64000)` | 1 + 0 = **1** | TODO | a deliberate v2 refusal — the form would exhaust the SBCL compiler heap |
| F7 | `PCL: Can't modify non-lvalue subroutine call in assignment` | 5 + 0 = **5** | **DIE** | chop.t's four `eval 'chop($x) = 1'` rows (+1 inline) |

### Notes per family

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
- **F4 is guardrail §5a.4's subject.** These are PPI failures, not v2 gaps:
  today they land in v1 (which runs with `--lenient-ppi`), and after the flip
  they must die loudly naming the file — never become a silent no-op.
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
