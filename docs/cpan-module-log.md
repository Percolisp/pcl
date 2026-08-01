# CPAN Module Test Log

A running log of CPAN / core modules tried through PCL (`./runpl`), the outcome,
and the bugs each one surfaced. Newest entries first. The point is twofold:
(1) track which modules work end-to-end, and (2) test the "do problems converge
to a finite shared bucket of bugs?" hypothesis — see
`project_cpan_convergence_survey` in memory.

Status legend: ✅ works · 🟡 partial · ❌ blocked · 🔧 fixed-this-session

---

## CPAN suite scoreboard 2026-08-02 (s322, gen v2-92) — RE-RUN: zero drift, and a finer baseline

Re-run of the s316p baseline dists after 9 sessions of compiler work, most
importantly s321's **#182** (every `s///` with an interpolated replacement now
routes through the real dq-string parser — an emission change touching an
idiom CPAN uses far harder than perl's own suite does).

| dist | PASS | PARTIAL | FAIL | vs s316p |
|------|------|---------|------|----------|
| Try-Tiny-0.32 | 4 | 4 | 3 | **=** |
| Role-Tiny-2.002004 | 11 | 8 | 4 | **=** |
| Scalar-List-Utils-1.70 | 12 | 20 | 6 | **=** |
| Sub-Uplevel-0.2800 | 2 | 5 | 3 | **=** |

Command (`--no-dist-lib` must precede Scalar-List-Utils and applies to every
dist after it):

```
perl tools/cpan-scoreboard.pl --jobs 4 --timeout 120 --tsv docs/cpan-scoreboard.tsv \
  ~/.cpan/build/Try-Tiny-0.32-0 ~/.cpan/build/Role-Tiny-2.002004-0 \
  ~/.cpan/build/Sub-Uplevel-0.2800-0 --no-dist-lib ~/.cpan/build/Scalar-List-Utils-1.70-0
```

**The per-dist tally is too coarse to be a gate, so it is no longer the
baseline.**  A PARTIAL file can lose rows and keep its status: the whole
class "this dist still fails the same number of *files*, but 4 more
*assertions*" was invisible.  `docs/cpan-scoreboard.tsv` (new, `--tsv`) is now
the artifact — one sorted line per t-file, `dist file status ok notok rc`,
diffed with plain `diff`.  Same reasoning as #185 for the perl suite.
(The committed s322 TSV was converted from this run's stdout, field for field,
rather than re-run; every later one comes straight from `--tsv`.)

Nothing here is a release verdict: it says the four dists did not move.  The
widened scoreboard is the part that finds new bugs.

---

## CPAN suite scoreboard 2026-07-30 (s316p, gen v2-83) — MECHANICAL BASELINE

First run of `tools/cpan-scoreboard.pl` (new; wraps run-dist-t.pl over a
dist's whole t/, classifies PASS = ok>0 && notok==0 && rc==0, PARTIAL = ok>0
otherwise, FAIL = ok==0).  These counts supersede the hand-tallied s304/s276b
baselines — per-file drift-checks against a worktree at the s304 commit
(57013d7) showed **zero per-file regressions**; every count difference vs the
old tallies is classification methodology (e.g. skip-all / 0-ok files).

| dist | PASS | PARTIAL | FAIL | notes |
|------|------|---------|------|-------|
| Try-Tiny-0.32 (post-shim) | 4 | 4 | 3 | 🔧 `lib/Try/Tiny.pm` shim (below); finally.t 11 ok→29/1, context.t 13→25/0, basic.t 24/1. Residual: named.t (subname introspection), given_when (§given/when), global_destruction_load (GC DESTROY), 00-report-prereqs (CPAN::Meta), erroneous_usage t6 (misuse-detection parse shape), finally.t t30 (skip = non-local exit, see shim header). |
| Role-Tiny-2.002004 | 11 | 8 | 4 | 🔧 s316q: use/BEGIN compile-stream ordering fix recovered role-basic-basic (0→PASS) + create-hook; basic `with 'Role'` method install WORKS (cluster C's core was already fixed). Residual FAILs: concrete-methods (stash-forms torture), subclass (subclassing Role::Tiny itself), proto, namespace-clean (XS dep) — task #135 |
| Scalar-List-Utils-1.70 | 12 | 20 | 6 | was 8/22/8 at s304 — improved; subname.t = timeout (rc 124) |
| Sub-Uplevel-0.2800 | 2 | 5 | 3 | was 2/2/6 at s304 — improved (3 FAIL→PARTIAL) |

**🔧 Try::Tiny shimmed (`lib/Try/Tiny.pm`)**: upstream runs `finally` from a
scope-guard DESTROY that PCL's GC never fires — the finallys silently never
ran.  The shim calls them directly (success/failure/catch-died paths, `$@`
untouched after try, exceptions in finally warned not propagated).  Known
limit: a non-local exit out of try (Test::More `skip` = `last SKIP`) skips
the finallys — only DESTROY can see that unwind.  Guard test in
`Pl/t/transpile-test-07.t`.

---

## Module survey 2026-06-23 (session after s264) — 3 general bugs fixed

Batch-tested core/CPAN modules through `./runpl`. Most work
(List::Util incl. `pairs`/`reduce`/`uniq`, Scalar::Util, POSIX, Getopt::Long,
overload, `use constant`, Data::Dump). Surfaced **three general bugs**, all
fixed (none module-specific):

1. **`print FOO, LIST` swallowed `FOO` as a filehandle.** An ALL-CAPS bareword
   right after `print`/`say`/`printf` was always treated as a filehandle, even
   when a comma followed it (`print FOO, $x` where FOO is a constant). In Perl
   the filehandle form has NO separator between handle and list; a comma means
   FOO is a list element. **Fix:** `Pl/PExpr.pm` — a `,`/`=>` immediately after
   the bareword blocks the filehandle interpretation. Tests: `constants-01.t`.

2. **Failed `=~` returned `undef` instead of `''`.** In scalar context a
   non-matching `m//` returns Perl's defined-false `''`, not undef; PCL returned
   nil → undef, so `defined($x =~ /no/)` was false (perl: true). Surfaced via
   `Scalar::Util::looks_like_number("xx")` returning undef. **Fix:**
   `do-regex-match` in `cl/pcl-runtime.lisp` returns `""` on no-match in
   scalar/void context (list context still returns the empty list).

3. **`pcl -MModule=imports` ignored the import list.** The `pcl` runner turned
   `-MData::Dump=dump` into `use Data::Dump=dump;` (invalid) so `dump` was never
   imported → "function pl-dump is undefined". **Fix:** `pcl` now parses perl's
   `-M` syntax (`-MMod=a,b` → `use Mod (a, b)`, `-M-Mod` → `no Mod`). Test:
   `pcl-dash-m-01.t`.

### Not fixed (out of scope / deep)
- **Storable / Time::HiRes / Hash::Util** — XS modules, no pure-Perl fallback
  (correctly die "Can't locate loadable object", per not-supported.md XS).
- **Text::Wrap** — dies "This shouldn't happen". Root cause: cl-ppcre has **no
  `\p{...}` / `\pL` / `\PM` Unicode-property support** (verified `"a"=~/\pL/`
  fails). Text::Wrap's wrap regex uses `\PM\pM*`. Broad regex-engine gap; left
  as a known limitation for now (would need a property resolver / cl-unicode).

## Data::Dumper — ✅ WORKS (as of 2026-06-22, session after s264)

`use Data::Dumper; print Dumper($ref)` now produces **byte-identical** output to
real perl 5.40 for nested hash/array structures, `$Sortkeys`, `$Terse`,
`$Indent`. Previously crashed. Fixing it surfaced **three independent, general
bugs** (all fixed — none are Data::Dumper-specific):

1. **`XSLoader::load` silently succeeded** → dual-life modules never fell back to
   pure Perl. The standard idiom is
   `eval { require XSLoader; XSLoader::load('M'); 1 } or $Useperl = 1;`. PCL's
   stub returned nil (success), so `$Useperl` stayed 0 and Data::Dumper called
   the nonexistent XS sub `Dumpxs`. **Fix:** `XSLoader::pl-load` now `p-die`s
   ("Can't locate loadable object …"), exactly as on a system missing the `.so`,
   so every dual-life module falls back. (`cl/pcl-runtime.lisp`)

2. **`local($ref->{key}) = …` (parenthesized list-form) clobbered the base
   scalar.** The pre-unwrap in `_process_local_declaration` only unwrapped a
   single *bare* symbol in parens, not a subscripted lvalue, so
   `local($s->{apad}) = $s->{apad}` fell through to the generic list-local path
   and overwrote `$s` with the value. **Fix:** generalized the pre-unwrap to
   unwrap a single comma-free subscripted lvalue. (`Pl/Parser.pm`)

3. **`BEGIN` inside an expression-level `do{}`/`eval{}` corrupted the enclosing
   form.** Inside a named sub (non-main package), a `do { BEGIN {…} EXPR }` in an
   `elsif` *condition* hoisted its `BEGIN` straight into the `definitions`
   bucket — the very bucket the sub body is incrementally emitted into since
   s253b — dropping a stray `(p-BEGIN …)` between two `p-if` branches
   ("too many elements … p-if"). **Fix:** when the current bucket is
   `definitions`, defer the hoisted BEGIN into a pending buffer that
   `_process_children` flushes at the top-level statement boundary (after the
   enclosing sub, where the constants it references already exist).
   (`Pl/Parser.pm`)

Note: PCL defines `builtin::is_bool`, so Data::Dumper's `SUPPORTS_CORE_BOOLS`
branch is live and exercised.

---

## How to add an entry

```
echo 'use Some::Module; ...' > /tmp/m.pl
./runpl /tmp/m.pl                 # PCL
perl /tmp/m.pl                    # oracle
diff <(./runpl /tmp/m.pl) <(perl /tmp/m.pl)
```

Record: module, status, what worked, what broke, and whether the bug was
module-specific or a general PCL bug (the latter is the valuable kind).
