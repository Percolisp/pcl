# Differential Fuzzer — `tools/difftest-ops.pl`

A **proactive** bug-finding tool: it generates small Perl snippets over
enumerable language axes, runs each through real `perl` (the *oracle*) **and**
through PCL (`./runpl`), and reports only the mismatches. Every mismatch is a
PCL bug (or a deliberately-documented divergence); the snippet doubles as a
ready-made regression test.

> **Why it exists.** Most PCL bugs were historically found *reactively* — one
> per CPAN-module crash, or whatever a hand-written `perl-tests/*.t` happened to
> exercise. That leaves holes: a ternary-associativity bug survived ~5 months
> because `perl-tests/cond.t` never tested `?:` at all. Differential fuzzing
> against a reference implementation is how you find those holes *before* a real
> module trips over them. (The user's framing: "this is how security people find
> holes.")

---

## Running it

```bash
perl tools/difftest-ops.pl [--jobs N] [--limit N] [--show-ok]
```

- `--jobs N`   — PCL worker fork-pool size (default 8).
- `--limit N`  — only run the first N generated snippets (fast smoke test).
- `--show-ok`  — also print the matching snippets (default: mismatches only).

Exit code is non-zero iff there were mismatches. A full run is ~760 snippets and
takes a few minutes at `--jobs 8` (each PCL run spawns a fresh SBCL).

Typical output:

```
RESULT: 761 valid snippets, 757 match, 4 MISMATCH in 2 clusters

### [3] numeric format (float vs int/precision)
  length-plus            perl=[1]   pcl=[5]
  ctx-count split /,/,$s perl=[1]   pcl=[3]
  num 0.1 + 0.2          perl=[0.3] pcl=[0.30000000000000004]

### [1] float (**) vs exact bigint
  binop 2 ** 3 ** 4      perl=[2.41785163922926e+24] pcl=[2417851639229258349412352]
```

---

## How it works

1. **Generate** templated (not random) snippets across the axes below. Each
   snippet is a complete program that ends by printing exactly `[VALUE]\n`
   (undef → `[undef]`), so extraction is robust against surrounding noise.
2. **Oracle**: run each snippet through `perl FILE`. If perl itself rejects it
   (`$? != 0`), the snippet is **skipped** — PCL is a transpiler for *valid*
   Perl, not a validator (CLAUDE.md principle 9), so invalid input is out of
   scope.
3. **PCL**: run the same file through `./runpl` in a fork pool (parallel-safe —
   `runpl` uses `$$`-based temp names).
4. **Normalize** each program's stdout to its `[...]` payload via `extract()`,
   which also collapses reference hex addresses (`ARRAY(0x55f..)` → `0xADDR`) so
   only the ref *type* is compared, and maps PCL parse/runtime failures to the
   markers `PARSE-ERROR` / `CL-ERROR`.
5. **Cluster** mismatches by a root-cause `signature()` so a single underlying
   bug doesn't flood the report (e.g. "PCL parse error", "PCL runtime error",
   "false comparison: '' vs undef", "float (**) vs exact bigint", "numeric
   format", "other: perl=[..] pcl=[..]").

---

## The axes

| # | Axis | What it stresses |
|---|------|------------------|
| 1 | binary operator precedence **pairs** `2 OP1 3 OP2 4` | precedence/associativity across every op pair (shifts excluded — large-shift is documented non-support) |
| 2 | string/relational ops with mixed numeric-string operands | string-vs-numeric coercion, `cmp`/`eq` chains |
| 3 | ternary nesting/associativity shapes × all condition truth assignments | `? :` parsing (true-nest, false-nest, chains, deep, binop-in-branch) |
| 4 | named-unary combined with binary/ternary | `ref $h eq "X" ? .. : ..`, `length $s == 3 ? ..`, etc. |
| 5 | context (list / count / scalar) | the *same* expr in `my @r=(E)`, goatse `my $n=()=(E)`, `scalar(E)` — flatten/wantarray bugs |
| 6 | builtins × call-forms | `NAME(a)` / `NAME a` / `CORE::NAME(a)` / `CORE::NAME a` / no-arg `$_`-default |
| 7 | deref / sigil / slices / postfix deref | `$$sr`, `${$ar}[1]`, `$ar->@*`, `@$hr{qw(a c)}`, `$#$ar`, … |
| 8 | OO dispatch | override / inherited / `SUPER::` / `can` / `isa` / method-name-in-var / class-method / chained |
| 9 | string builtins & sprintf | `substr`/`index`/`rindex` (negative offsets), `x`, `join`, and a broad `sprintf` format sweep |
| 10 | regex match / substitution / tr | `m//`, captures, `s///g` count, `tr///`, named captures `%+`, `split` variants |
| 11 | numeric edge cases | negative modulo, bit ops + `~` (unsigned-64), string↔number coercion, **magic string auto-increment** (`"Az"++`), ranges |
| 12 | compound assignment & inc/dec | `+= -= *= /= %= **= .= x= \|\|= //= &&=` etc., return value *and* final variable; `$x++`/`++$x` |
| 13 | array/hash builtins | push/pop/shift/unshift/splice return-value + mutation, `scalar(@a)`/`scalar(%h)`, keys/values/exists/delete, grep/map in scalar |
| 14 | list construction | list-repeat `(1,2)x3`, swap, list-assign count, nested aref/href, slices, `qw` |
| 15 | closures & lexical capture | counter/accumulator closures, independent vs shared state, capture-by-ref, nested closures, escape-from-block (foreach per-iteration capture omitted — documented non-support) |
| 16 | local / dynamic scoping | `local` on package scalars/arrays/hashes + elements, the value a called sub sees during the extent, restoration afterwards |
| 17 | sort variants | string vs numeric, descending, reverse-sort, multi-key tie-break, sort-by-hash-value, Schwartzian, named comparator, sorted slice |
| 18 | regex features | `\b`, lookahead/lookbehind, non-greedy, `/g` list & count, `/i /m /s`, backreferences, `/e` with nested calls, alternation, capturing split |
| 19 | autovivification & nested data | deep hash/array autoviv, `push @{$h{k}}`, AoH/HoA, building structures in a loop |
| 20 | numeric stringification & sprintf precision | float printing (`%.15g`), IV/NV boundary (`2**53`), `%.Nf` rounding (half-to-even), `%g`/`%e`/`%f` |
| 21 | short-circuit / defined-or | `\|\| && //`, `\|\|= &&= //=`, and the exact set of operands evaluated (side-effect ordering via a logged closure) |

> **A fuzzer finding is a *candidate*, not a verdict.** Some mismatches are
> **undefined behavior** in Perl itself — e.g. `$x += $x += 1` modifies a
> variable twice in one statement, which perlop explicitly leaves undefined
> ("Perl will not guarantee what the result is"). PCL is free to diverge there
> (principle 9). Such snippets are removed from the axes, not "fixed."

Adding an axis is just more `add($desc, prog($expr, $prelude))` calls before the
`$LIMIT and ...` line. Use `prog` for scalar-context results, `prog_list` for
list-context results, `prog_count`/`prog_scalar` for the context axis.

---

## Bugs found so far

This is the running ledger; the authoritative narrative lives in
`docs/session-log.md` and the `project_difftest_fuzzer` memory note.

**Session 240–241 (Axes 1–8), all fixed unless noted:**
- chained string-compare crash (`eq`→nonexistent `p-eq`); fixed to `p-str-*`.
- bitwise `& | ^` signed→unsigned-64 (`%pcl-to-u64`).
- equality-vs-relational precedence (`2 != 3 > 4`): relational binds tighter.
- false comparison returns `""` (defined) not nil (`p-bool`) — fixes
  `defined(2==3)`, `2==3 // 4`.
- braced block-deref subscript `${$ar}[1]` / `@{$ar}[0,2]` returned undef/empty
  (PPI mis-tags the `[...]` as a Constructor); fixed via
  `_retag_braced_deref_subscript`.

**Session 242 (Axes 9–11):**
- **float stringification** — `0.1+0.2` printed `0.30000000000000004` instead of
  Perl's `0.3`. PCL was emitting SBCL's shortest-round-trip form; Perl uses plain
  `%.15g` (15 significant digits, then strip trailing zeros — *lossy* by design).
  Fixed in `stringify-value` (`cl/pcl-runtime.lisp`): the fixed-notation branch
  now formats with `(14 - exp10)` fraction digits and the exponential branch uses
  `~,14E`. See the "deliberate divergences" note below for why sprintf `%g` is a
  *separate*, already-correct path.

**Session 242 (Axes 12–14):**
- **`/=` and `**=` leaked a CL ratio** — `$x /= 2` → `"7/2"`, `$x **= -1` →
  `"1/2"`. The macros divided/exponentiated raw (CL int/int → ratio) while plain
  `p-/` / `p-**` coerce ratio → float. Fixed by delegating `p-/=` → `p-/` and
  `p-**=` → `p-**` (also gains overload dispatch).
- **array/hash slice in string interpolation leaked scalar context** —
  `my $s = "@a[1..2]"` reduced to the last element instead of joining `"2 3"`.
  A single-slice string bypassed the join wrapper (`StringInterpolation.pm`), and
  even wrapped slices inherited the outer scalar context (`gen_string_concat`).
  Both forced to list context. Whole-array `"@a"` was always fine.
- **anon arrayref `[...]` leaked scalar context + tail_position into its
  contents** — `do { ...; "@{[reverse @a]}" }` ran `reverse` in scalar context,
  reversing the joined string (`"321"`) instead of the list (`"3 2 1"`). Fixed in
  `gen_array_init` (force list context, clear tail_position — bracket contents are
  never the enclosing sub's tail call). sort/map were unaffected; reverse exposed
  it.

**Session 244 (Axes 15–21):**
- **closure-captured `my @a`/`my %h` never populated** (axis 15) — a captured
  array/hash is renamed to a let-bound lexical, but its init went through `p-my-=`
  (`box-set`), a no-op on a non-box aggregate. Whole-aggregate reads saw an empty
  array. Fixed via shared `p-array-fill`/`p-hash-fill` (fill the adjustable lexical
  in place, no proclaim-special) + LIST-context RHS. `cl/pcl-runtime.lisp`,
  `Pl/Parser.pm`.
- **`%.Nf` rounded half-AWAY-from-zero** (axis 20) — `sprintf("%.0f",2.5)`→`3`,
  `("%.0f",0.5)`→`1`. C/Perl round half-to-EVEN (→`2`,`0`). `sprintf-format-float-f`
  now rounds the *exact rational* of the double with CL `ROUND` (itself half-even),
  so the scale-by-10^prec is exact. Also lifted sprintf.t +54 / sprintf2.t +42.

**Documented / deferred divergences (the fuzzer keeps reporting these; they are
intentional):**
- `**` always yields a float in Perl; PCL returns an exact bignum (differs only
  past 2^53 — `2**53` prints `9.00719925474099e+15` vs `9007199254740992`).
  Representation choice — `docs/sweep-bug-catalog.md`.
- `%.17g` (and precision > 15) prints only ~15 sig-digits: `sprintf("%.17g",0.1)`
  is `0.1` in PCL vs `0.10000000000000001` in Perl. PCL's float→decimal is the
  lossy `%.15g`-style path; full 17-digit round-trip is the same representation
  call as the `**` item. Deferred — `docs/sweep-bug-catalog.md`.
- named-unary precedence: `length $s + 1` is `length($s+1)` in Perl. Real
  fix-target in `Pl/PExpr.pm` (~2784), deferred.
- `() = split` LHS-arity LIMIT: `my $n = () = split/,/,"a,b,c"` is `1` in Perl
  (implicit LIMIT), `3` in PCL. Niche — `docs/not-supported.md`.

---

## Using a finding

Each `other` / `PCL parse error` / `PCL runtime error` cluster is a candidate
real bug: triage it, fix the root cause, then drop the failing snippet into a
`Pl/t/` regression test (smallest `transpile-test-NN.t`, or `misc-fixes-NN.t`).
The `float (**)` and similar representation/non-support clusters are *expected* —
they should be triaged once and left documented, not re-investigated each run.
