# Changelog

Distilled from the development log (`docs/session-log.md`, 400+ working
sessions); dates are development-time, not release-time.

## v0.1.0 — unreleased (first public version, pending)

Initial release of PCL (Percolisp): a from-scratch Perl 5 → Common Lisp
compiler with a CL runtime that reproduces Perl's semantics.

### Compiler
- Full expression grammar: all precedence levels, ternary, ranges, string
  ops, chained/stacked filetests (`-f -d $x` lowers to perl's `_`-chain),
  named-unary operand rules, list operators, `wantarray`-correct context
  propagation (three-valued: scalar/list/void).
- Statements: `if`/`unless`/`while`/`until`/C-style and list `for`/
  `foreach` (with element aliasing), `sub` (signatures, defaults,
  prototypes), `package` (block and statement), `use constant`, `BEGIN`,
  perl 5.34 `try`/`catch`/`finally`, `local` (scalars, aggregates,
  elements, typeglobs), loop labels and `next`/`last`/`redo`/`goto &sub`.
- Closures with correct lexical capture (including loop-variable
  per-iteration bindings and `state` variables), `__SUB__` (named and
  anonymous subs).
- `eval BLOCK` and `eval STRING` — string eval is transpiled at runtime
  with **lexical capture**: eval'd code reads and writes enclosing `my`,
  `our`, and `state` variables, and closures built inside the eval keep
  working; a construct string eval cannot compile fails into `$@`,
  perl-shaped.
- OO: `bless`, method resolution with C3 MRO (CLOS-backed), multiple
  inheritance, `SUPER::`, `AUTOLOAD`, `use overload`, `ref`,
  `Scalar::Util`-style introspection.
- Regexes via CL-PPCRE: `m//`, `s///` (incl. `/e`), `tr///`, named
  captures, `%+`/`$1…`, modifier semantics; `use utf8` source decoding.
- Refaliasing (`use feature 'refaliasing'`), postfix deref, lvalue refs
  (`\substr`, `\pos`, `\vec`, `\$#array`).
- A statement the compiler cannot lower is **announced and dropped**, never
  silently lost; the population-wide drop census is tracked in-repo and
  gated (`docs/drop-census-s419-flip-gate.md`).
- Named optimization registry (`PCL_OPT`): every speed transform is a
  named, fact-licensed emission that can be switched off
  (`PCL_OPT=none` = the general-form compiler, verified to run
  identically).

### Runtime (`cl/pcl-runtime.lisp`, ~17,000 lines of CL)
- The box data model (scalars carry numeric and string views, are
  referenceable, and support magic), Perl coercion and truthiness rules,
  `local` dynamic scoping, tied scalars, filehandles/IO (incl. in-memory
  handles, `open` modes, `sprintf`/`printf` formats), `sort`/`map`/`grep`,
  `pack`/`unpack` (written in Perl, self-hosted: PCL transpiles its own
  `pack` implementation), `%ENV`/`@ARGV`/`$0`/`$!`/`$@` and the magic
  globals, signal handlers, `fork`/`wait`/`system`/backticks.
- Module system: `use`/`require` with a transpile cache, `Exporter`,
  shipped pure-Perl shims for core modules (List::Util, Scalar::Util,
  Data::Dumper family, File::* basics, Carp, …) transpiled like user code.
- `caller()` with correct package reporting; `mro::get_linear_isa` (C3).

### Tooling
- `pl2cl` (transpiler, with `--server` mode used by runtime string eval),
  `runpcl` (transpile-and-run), `pcl` (perl-like CLI: `-e`/`-E`/`-M`).
- `tools/install-pcl`: installs the runtime tree and compiles a saved SBCL
  core at install time (the XS model — never at first use).
- Test infrastructure: 155-file regression gate with a fresh-core runner
  (`tools/prove-core`), Perl-suite sweep runners with blessed row-level
  baselines, a drop census, an emission A/B differ, and a differential
  fuzzer (PCL vs perl).

### XS (experimental, separate project)
- **pclxs**: a `libperl`-ABI shim letting unmodified compiled XS modules
  call into PCL's runtime.  Conformance corpus 398/398 against real perl;
  `Digest::MD5` works end-to-end (its own test file passes 256/256).
  Not bundled with v0.1.

### Known limitations
See [`docs/STATUS.md`](docs/STATUS.md) (summary tables) and
[`docs/not-supported.md`](docs/not-supported.md) (every deliberate
non-support, with rationale).
