# Plan: resolving Perl/CL identifier case clashes

**Status:** plan only (2026-06-16; updated s264 2026-06-21). Targeted
collision-only fix shipped s252; the general fix below is deferred to the
compiler rewrite. See `memory/project_case_sensitivity_general_fix.md` for the
s252 details, and the **s264 addendum at the bottom** for a new real-CPAN
datapoint (Getopt::Long) that the s252 rename *cannot* reach, plus a refinement
to the `:invert` analysis (the runtime symbolic-ref resolvers are a third
identifier→symbol chokepoint).

## The problem, precisely

The Perl-name → CL-symbol mapping happens in the **CL reader** when SBCL reads
the generated `.lisp`. Default `readtable-case` is `:upcase`, so `$base_len` and
`$BASE_LEN` both read as one symbol `$BASE_LEN` — the mapping is non-injective.

Collisions are only possible **within one CL package** (each Perl package = its
own CL package via `(p-defpackage :|Pkg|)` + `in-package`), which is why they're
rare in practice — but real: `Math::BigInt::Calc` has file-`my $BASE_LEN` + a
lexical param `my $base_len`, and the fold broke `_base_len` (was the pack.t
TIMEOUT root cause).

There are exactly two structural ways to make the mapping injective.

## What the codebase already does (the key context)

- **Multi-segment packages already preserve case via bar-escape:** `package
  Foo::Bar` → `:|Foo::Bar|`. Bar-escape is proven in production for one
  identifier class. (Single-segment `Foo` → `:FOO`, with orig case carried
  out-of-band.)
- `sub FOO` emits `pl-FOO`; `sub foo` emits `pl-foo`. Note `pl-FOO` is a
  **mixed-case** token.
- Class names (`plc-Foo`), method names, and `ref()` output go through an
  **out-of-band orig-case string table** (`p-register-pkg-name … "Foo::Bar"`),
  so user-visible strings are already decoupled from CL symbol case.
- The s252 targeted rename (`Pl/Parser.pm::_compute_and_apply_case_renames` +
  `ExprToCL::_case_renamed`) handles real **variable** collisions by renaming
  all-but-one colliding spelling to `<name>__pcl_ci_N`. **Subs are NOT
  disambiguated.**

The `p-`/`pl-`/`plc-` prefix discipline turns every upper-case Perl name into a
**mixed-case** CL token — which matters a lot for approach (b).

## Approach (a): explicit `cl:intern` / bar-escape

**Mechanism.** Route every user identifier through one transform `M(name)` and
emit it case-preserving in source — in practice `|name|` (bar-escape), the
read-syntax equivalent of `(intern "name")`. Wherever the *runtime* builds a
symbol from a Perl name string (method dispatch, `*glob` install, `can`, stash),
call `(intern (M name) pkg)` with the **same** `M`. `M` can be identity (exact
case) since `||` preserves case.

```
$base_len      → |$base_len|        $BASE_LEN → |$BASE_LEN|     distinct
sub foo / FOO  → |pl-foo| / |pl-FOO|                            distinct
```

**Pros**
- Surgical: only *our* user-identifier emissions change. Standard CL forms
  (`defun`, `let`, `car`) stay bare and read normally. No global mode switch.
- No interaction with cl-ppcre/ASDF load; FASLs unaffected.
- Already proven for packages.

**Cons**
- **Pervasive across codegen**: every site that emits a var/sub/class/filehandle
  symbol must wrap in `||`, *unconditionally* (conditional = today's targeted
  rename, which can't see cross-file collisions).
- **Large test churn**: hundreds of `Pl/t/transpile-test-*.t` string-match
  assertions match `\$base_len`, `pl-foo`, etc. All would need `||`.
- Readability cost: `(box-set |$base_len| 1)`.
- Requires a crisp "user identifier vs PCL-internal symbol" boundary at every
  emission site, maintained forever.

## Approach (b): change the reader to not upcase — `:invert`

**Mechanism.** Read PCL's own source (runtime + generated + test lib) under
`(setf (readtable-case *readtable*) :invert)`. Per CLHS, `:invert` flips the
case of a token **only if all its letters are the same case**; mixed-case tokens
are left untouched:

```
foo  → FOO        (lower → upper; standard CL still works)
FOO  → foo        (upper → lower)
Foo  → Foo        (mixed, preserved)
```

Leveraging the existing prefixes:

| Perl            | emitted token | case  | reads to            |              |
|-----------------|---------------|-------|---------------------|--------------|
| `my $base_len`  | `$base_len`   | lower | `$BASE_LEN`         |              |
| `my $BASE_LEN`  | `$BASE_LEN`   | upper | `$base_len`         | distinct     |
| `sub foo`       | `pl-foo`      | lower | `PL-FOO`            |              |
| `sub FOO`       | `pl-FOO`      | mixed | `pl-FOO` (preserved)| distinct — **free, fixes subs** |
| builtin         | `p-print`     | lower | `P-PRINT`           | matches runtime |

**Crucial insight:** the `p-`/`pl-`/`plc-` prefix turns every upper-case Perl
name into a *mixed-case* CL token, which `:invert` preserves. So `:invert` fixes
**subs and classes for free**, with no rename machinery — exactly the gap the
s252 targeted fix left open.

**Pros**
- **Zero per-site codegen change**, zero test churn on the common (lower-case)
  path. A configuration + audit task, concentrated in one place rather than
  smeared across the codebase.
- Fixes subs/classes automatically.
- Composes with the compiler rewrite instead of fighting it.

**Cons / risk (the real work is here)**
- It's a **global mode**. Every all-uppercase or mixed-case symbol *shared
  between runtime and generated code* must be spelled identically on both sides
  (they mostly already are — codegen emits `STDERR`, runtime must too, both
  invert to `stderr`). Needs a **full audit** of `cl/pcl-runtime.lisp` +
  `cl/pcl-test.lisp` for all-caps/mixed symbols (constants like `+FOO+`,
  exported keywords).
- **Load orchestration**: cl-ppcre/ASDF must load *first* under the standard
  readtable; `:invert` must be bound only while reading PCL files. Every entry
  point (`pl2cl` load + FASL compile, `runpl`, `cl/test-runtime.lisp`, the `pcl`
  runner) needs the scoped binding.
- `:invert` is bijective and round-trips on print, and `ref()`/method-name
  strings are already orig-case out-of-band, so **user-visible output is
  unaffected**.

## Recommendation

**On "how": (b) `:invert` is the better target.** The prefix discipline already
does 90% of the work; `:invert` is the natural completion of it, fixes
subs/classes (the open gap) for free, and its cost is a bounded one-time audit
rather than permanent per-site overhead and test churn. Approach (a)/bar-escape
is the right tool for the *narrow* cases where you can't prefix — which is
exactly why packages already use it.

**On "if": defer the general fix.** The targeted s252 rename covers every real
collision observed. Sub/class case-collisions and cross-file same-package
globals have produced **zero** real CPAN failures to date. Don't pay the
`:invert` audit cost until either (1) a real module breaks on a sub/class case
collision, or (2) you're already in the compiler rewrite, where the reader-setup
audit is cheap to fold in and there's a single identifier→symbol chokepoint.

## If/when green-lit — phased implementation of (b)

1. **Audit** — script over `cl/*.lisp` (+ generated samples) listing every
   symbol with all-caps or mixed case; classify each as runtime-internal (must
   agree across sides) vs incidental. Output a normalization punch-list.
2. **Reader plumbing** — create one invert-readtable; load 3rd-party libs under
   the standard readtable first, then `(let ((*readtable* …invert…)) (load
   runtime/generated))`. Wire into all four entry points + the FASL compile step.
3. **Normalize** the punch-list so codegen and runtime spell shared
   all-caps/mixed symbols identically.
4. **Gate** — run `prove -j8 Pl/t/` (6+ min) and a full sweep; re-bless the fail
   baseline only from a clean sweep. Retire `_compute_and_apply_case_renames`
   and the `__pcl_ci_N` machinery once green.
5. **Regression test** in `Pl/t/misc-fixes-02.t`: `$base_len`/`$BASE_LEN` **and**
   `sub foo`/`sub FOO` both round-trip.

---

## s264 addendum (2026-06-21): the runtime symbolic-ref path is a third chokepoint

### New real-CPAN datapoint — Getopt::Long (the s252 rename can't reach this)

Running Getopt::Long's own test suite through PCL, `gol-basic.t`/`gol-oo.t`
tests 2/3/5 fail. The legacy "no linkage" mode stores results into package
globals `$main::opt_<name>` built **at runtime by symbolic reference**:

```perl
# inside Getopt::Long, roughly:
${ $pkg . "::opt_" . $name } = $value;   # $name comes from @ARGV at runtime
```

With options `"foo"` and `"Foo=s"` this writes `${"main::opt_foo"}` and
`${"main::opt_Foo"}`. `%p-symref-box` does `(string-upcase "opt_foo")` →
`$OPT_FOO` for **both** → they collide on one CL symbol; last write wins.

Why s252's compile-time rename does **not** help here:
- The s252 rename rewrites the *test's direct token* refs: `$opt_foo` →
  `$opt_foo__pcl_ci_1`, `$opt_Foo` kept. So the two *reads* in the test are now
  distinct symbols. Good.
- But the *writes* come from Getopt::Long via a **runtime-constructed string**
  (`"opt_$name"`), which `%p-symref-box` upcases — it has never heard of the
  rename table. Both writes still land on the unrenamed `$OPT_FOO`.
- Net: the renamed var (`opt_foo__pcl_ci_1`) is never written (reads undef →
  tests 2/3 fail), and the kept var (`$OPT_FOO`) is clobbered by *both* writes,
  so it holds `1` instead of `"-baR"` (test 5 fails).

**This is the first observed CPAN case where the collision lives in the runtime
symbolic-ref path, not in direct token refs.** It proves the symref resolvers
are an independent identifier→symbol site that any complete fix must cover.

### The identifier→symbol chokepoints (complete list)

1. **The CL reader** — direct token refs in generated code (`$opt_foo`,
   `pl-foo`). Governed by `readtable-case`.
2. **Runtime symbolic-ref / introspection resolvers** — build a CL symbol from a
   *runtime string*: `%p-symref-box` / `%p-symref-array` / `%p-symref-hash`
   (all `string-upcase`), `p-get-coderef`, plus method dispatch, `*glob`
   install, `can`, stash walking. These do **not** go through the reader.
3. **Compile-time deliberate-upcase sites** — package names, `use constant`
   (`+PI+`), glob slot names, `%SPECIAL_VARS`.

The s252 rename touches only (1) (via token mutation). The Getopt::Long bug is
in (2).

### Refinement to the `:invert` analysis: it is NOT "zero runtime change"

The body of this doc says `:invert` needs "zero per-site codegen change." True
for chokepoint (1). But chokepoint (2) is a real, bounded amount of runtime
work under `:invert`:

- The symref resolvers currently do `(string-upcase var-str)`. Under `:invert`,
  a direct ref `$opt_Foo` reads to the symbol **named** `$opt_Foo` (mixed case
  preserved), so the runtime must produce that *same* name from the string
  `"opt_Foo"` — i.e. replace `string-upcase` with an **`invert-case`** transform
  (flip case iff the letters are uniformly one case; else preserve). Equivalent
  to `(read-from-string (concatenate 'string "|...|"))`-style exact interning
  *only* for mixed-case; cleanest is a small `%pcl-invert-case` string helper
  applied uniformly at all chokepoint-(2) sites.
- Empirically verified (s264) that this is consistent: all-lowercase names are
  invariant between `:upcase` and `:invert` (`opt_foo`→`OPT_FOO` under both),
  so the bulk of the runtime (lowercase symbols) is untouched; only the names
  carrying uppercase need `invert-case`, and those are exactly the collisions.

| token     | `:upcase` | `:invert` |
|-----------|-----------|-----------|
| `opt_foo` | `OPT_FOO` | `OPT_FOO`  (lower → upper, same as today) |
| `opt_Foo` | `OPT_FOO` | `opt_Foo`  (mixed preserved → **distinct**) |
| `pl-foo`  | `PL-FOO`  | `PL-FOO`   (runtime agrees) |

So the `:invert` phased plan above gains a step: **2b. Swap `string-upcase` →
`%pcl-invert-case` at every chokepoint-(2) resolver**, and add a Getopt::Long-
style symref-collision regression.

### Cheap interim (if a fix is needed before the rewrite)

If Getopt::Long-class breakage needs fixing *without* committing to `:invert`:
extend the existing s252 mechanism into chokepoint (2). The compiler already
computes `environment->case_renames` (`opt_foo → opt_foo__pcl_ci_1`); emit it
into the generated output as a **package-scoped runtime registration**
(`(p-register-case-renames "main" '(("opt_foo" . "opt_foo__pcl_ci_1") …)))`) and
have the symref resolvers consult that map *before* `string-upcase`. Localized to
the four resolvers + one emit; reuses the rename machinery. Covers the common
case (a collision pair that *also* appears as direct refs — exactly
Getopt::Long, since the test reads both `$opt_foo` and `$opt_Foo`). It does
**not** cover a collision that exists *only* in runtime symref strings (no direct
ref to trigger a rename) — that residual case still wants `:invert`.

### Recommendation (unchanged, sharpened)

`:invert` remains the right end-state and the right home is the compiler rewrite,
now with chokepoint (2) explicitly in scope (step 2b). Until then: if
Getopt::Long-class failures must go green sooner, do the **cheap interim** above;
otherwise leave it. Do **not** flip the global readtable as a standalone change —
the deliberate-upcase audit (chokepoint 3) and the load/eval/saved-core readtable
orchestration are exactly the cross-cutting work the rewrite is structured to
absorb.
