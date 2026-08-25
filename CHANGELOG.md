# Changelog

Distilled from the development log (`docs/session-log.md`, 400+ working
sessions); dates are development-time, not release-time.

## Unreleased

- Speed: method dispatch no longer re-finalizes the CLOS class on every
  call (2.2× on a method-call loop; overload-heavy bench 7.27× → 4.98× of
  perl).  Counting loops, recursion and integer math beat perl
  (`docs/faster-codegen-suggestions.md` §0.1 is the current measured table).
- Speed, round 5: dispatch is now cache-free-fast — an own-package fast
  path plus stash/name memos take a monomorphic method loop to 2.62× of
  perl (was 9×) and inherited dispatch to 4.74×; the overload-heavy bench
  is 3.44×.
- `use English` works (a pure-Perl shim with live aliases, `-no_match_vars`
  included); `SUPER::` calls finish their lookup exactly like ordinary
  method calls (UNIVERSAL, the builtins, parent AUTOLOAD, perl-shaped die).
- `local` fixes: a statement-modifier on a bare `local` is honored; an
  element or slice target in a `local` LIST is localized *and* assigned
  (`$!` included); hash slices in plain list assignment get their values.
- `$0` is writable (and names the script); `open($fh, ">&", \*STDOUT)`
  filehandle dup works in every spelling with perl's failure shapes.
- Symbolic refs read the magic scalars (`${"1"}`); every punctuation
  container is defined; `sort NAME` resolves the comparator on entry;
  `\&$name` on a not-yet-defined sub is a late-bound CODE ref; the perl-4
  `'` package separator parses.
- The compiler's dropped-statement census fell from 167 statements in 73
  files to 89 in 34: block-prototype calls from Test::/Test2:: dists now
  parse (the name-based scan skip is gone), `<$fh> //` in a ternary
  compiles, and glob aliases to punctuation globs (`*X = *-`) lower.
- `runpcl` keeps program stdout and stderr apart (byte-compares against
  perl are honest).
- Correctness (the s444 batch): bareword filehandle names canonicalised at
  one seam (qualified handles, the `*` prototype slot's two halves);
  identity-promoted file lexicals no longer alias a spelled package
  variable; signature defaults join the capture set; s/// replacement-side
  fixes for non-ASCII identifiers; an anon sub binds its *defining*
  package; `%INC` records modules PCL resolves but never loads.
- The regression gate grew to 171 files / 5,924 assertions; CI runs the
  full gate on a stock Ubuntu runner (green).
- Docs refreshed to current measurements (`README`, `docs/STATUS.md`,
  the two speed docs, `docs/extensions.md`).

## v0.1.0 — 2026-08-23 (first public version)

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
- A statement the compiler cannot translate is **announced at compile time and
  dies, perl-shaped and trappable, when reached** — never silently lost; the
  population-wide census is tracked in-repo and gated
  (`baselines/parse-error-drop-census-s399.tsv`).
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
- Landed for the tag (2026-08-23): a call to a never-declared sub reaches the
  package's `AUTOLOAD` or dies perl's death; `$\` and `$,` are undef until
  set; `say` appends `"\n"` instead of `$\`; a symbolic sub name resolves in
  the current package; punctuation arrays (`@?` …) are `main::` everywhere;
  a list-context `glob` returns the full list on every call; `++${"name"}`,
  `$${EXPR}`, `print $_ LIST`, `require $m if …`, `local(LIST) = … if …`
  and a signature sub of the same name in two packages all lower.

### Tooling
- `pl2cl` (transpiler, with `--server` mode used by runtime string eval),
  `runpcl` (transpile-and-run), `pcl` (perl-like CLI: `-e`/`-E`/`-M`).
- `tools/install-pcl`: installs the runtime tree and compiles a saved SBCL
  core at install time (the XS model — never at first use).
  It checks the dependencies first — perl ≥ 5.20, **PPI ≥ 1.291**, Moo,
  SBCL ≥ 2.5.2 — and refuses to install until they are present.
- Dependencies are exactly PPI and Moo; every other Perl module PCL uses is
  core (the gate guards this: `Pl/t/core-deps-01.t`).  A non-core
  `Data::Dump` import in the compiler's debug dumps, invisible on a dev
  machine, broke the first CI run and was removed (s440).
- CI (GitHub Actions, `.github/workflows/ci.yml`): a stock Ubuntu runner
  installs PCL with `tools/install-pcl` and runs the full gate — the
  fresh-machine test, on every push.
- Test infrastructure: 171-file regression gate with a fresh-core runner
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
