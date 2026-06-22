# CPAN Module Test Log

A running log of CPAN / core modules tried through PCL (`./runpl`), the outcome,
and the bugs each one surfaced. Newest entries first. The point is twofold:
(1) track which modules work end-to-end, and (2) test the "do problems converge
to a finite shared bucket of bugs?" hypothesis — see
`project_cpan_convergence_survey` in memory.

Status legend: ✅ works · 🟡 partial · ❌ blocked · 🔧 fixed-this-session

---

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
