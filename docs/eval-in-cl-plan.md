# String eval with no Perl subprocess: PCL's compiler inside the image -- the plan

**Status:** PLAN (s533, 2026-11-04, the reviewing session on Opus 5.5; the
USER asked for a plan to run string eval in CL alone, with no Perl
subprocess -- PCL plus PPI in the image -- and whether more XS modules than
`Clone` would be needed).  Planning only --
nothing here is implemented.  Owner task **#4480**.  Filed from the
measurements below: **#4481** (PCL), **#4482** (PPI).
**Related:** `docs/single-binary-plan.md` §3.5 (it ruled "a binary dies at a
string eval" and called this plan "a different project"); `docs/eval-string-plan.md`
(the eval semantics, unchanged here); #1200 (the eval disk cache); #3961
(emission nondeterminism); #3694 / #3697 (the parser program, PCL's PPI).

## 0. The answer in nine lines

1. Today every `eval $str` is transpiled by a `pl2cl --server` **perl** child, and every
   uncached `require` / `use` by a one-shot `pl2cl` perl child (§1).
2. The plan: PCL transpiles **its own compiler** (`Pl/`, 27 files, 59,018 lines) and
   **its vendored PPI** (95 files, 20,544 lines) to CL **once, at install time**, into the
   saved core; `p-transpile-string` / `p-transpile-file` become function calls (§4).
3. Perl stays for two things only: **stage 0 of the bootstrap** (install / build time)
   and **the test oracle**.  Nothing at run time.
4. **XS (the question):** the closure loads 17 more XS modules besides `Clone` (15 core, 2 CPAN), and every one
   already has a PCL path (a `lib/` shim or its own pure-Perl half) **except `Params::Util`**,
   which needs one ~10-line shim (§2).  Nothing needs pclxs.
5. **Measured today (§3):** 119 of 122 files transpile with no drop; **PPI runs under PCL**
   after ONE PCL compiler bug is fixed (#4481) and gives perl's exact answer: 19,983 tokens,
   59 subs on `Calc.pm`, parse 0.66 s vs perl's 0.19 s.
6. **The bar (§4.3):** the in-image compiler's output is **byte-identical** to the
   perl-hosted compiler's over the four populations -- corpus-diff with two compilers
   instead of two shas, plus a stage-2 fixed point.
7. **The prerequisite (§4.4):** emission must not depend on hash order (#3961) -- perl's
   and PCL's `keys` orders differ, so every order-dependent site is a host-dependent diff.
8. **Speed (§4.6):** no 0.35 s server start and no pipe; each eval transpile is roughly 3×
   slower than in perl (~16 ms -> ~50 ms, to be measured); the eval disk cache is unchanged.
9. **Size: 10-16 sessions in seven phases (§5)**; P0 and P1 are worth doing on their own
   (determinism; PPI's 57,396-assertion suite as a conformance test of PCL).

## 1. What spawns perl at run time today (measured at `0f2db817`)

| entry | runtime function (`cl/pcl-runtime.lisp`) | perl child | cost |
|---|---|---|---|
| `eval $str` (and the other run-time-compiled text: `s///ee`, a run-time `(?{ })`) | `p-transpile-string` -> `p-ensure-transpiler` -> `pl2cl --server`, wire `PCL-EVAL-WIRE 2` | one per process, started at the first eval | first eval **~0.35 s** (perl loads compiler + PPI: 0.33-0.39 s, 44 MB, measured); then **~16 ms per distinct text** (s473p); results cached per process and on disk (#1200) |
| `require` / `use` of a module with no valid cache entry | `p-transpile-file` (`--module`), `%p-run-transpiler-perl` | one per module | perl start + transpile; then the module cache |
| the program's own first run (`pcl prog.pl`) | `%p-transpile-script` | one | as above |

What the server DOES per request is one function call (`pl2cl` ~887):
`parse_source('parse_code', $code, eval_mode => 1, eval_pkg, eval_captures,
eval_features, eval_protos)`, then `build_eval_preamble($pkg)` and
`Pl::ProtoCache::write_deps_file`.  Seven inputs, one string out.  In the
image these are ARGUMENTS -- the wire protocol, its version tag and its
desync handling disappear.

The compiler itself (`Pl/`) never shells out and never uses string eval
(grepped); only the `pl2cl` DRIVER spawns things (`--xserver`, `sbcl`), and
the in-image entry does not use the driver.

## 2. The dependency closure, and the XS question

Measured: `%INC` after `pl2cl` transpiled a program (with an eval) and
`pl2cl --module lib/Math/BigInt/Calc.pm`: **177 files = 23 `Pl/` + 90 PPI + 1
PCL tool + 62 others** (core and CPAN).  The XS ones, and their state under PCL:

| module | core? | loaded by | under PCL today | needed |
|---|---|---|---|---|
| `Clone` | CPAN, XS only | PPI (`Element`, `Token::Whitespace`, `_QuoteEngine::Full`) | **`lib/Clone.pm` pure-Perl shim exists** (#3826) | check PPI's `clone` + its `%_PARENT` relink under the shim (P1) |
| `Params::Util` | CPAN, XS + its own `Params::Util::PP` | PPI (22 files) | **dies**: its `XSLoader::load` is unconditional unless `PERL_PARAMS_UTIL_PP=1` -- measured: "Can't locate Params/Util.pm ... XS and has no PCL build" | **one new shim** `lib/Params/Util.pm` (must-win, loads the PP half; rule 9a); with the env var it loads today |
| `Class::XSAccessor` | CPAN, optional | Moo (when installed) | Moo's optional-XS probe reads "not installed" (#1917) and builds Perl accessors | nothing; gone if Moo leaves the compiler (§4.7) |
| `Scalar::Util`, `List::Util`, `Sub::Util` | core | compiler, PPI, Moo | `lib/` shims | -- |
| `Digest::MD5` | core | `Pl/ProtoCache.pm`, `Pl/Parser2.pm`, PPI | `lib/Digest/MD5.pm` shim | -- |
| `Time::HiRes` | core | `Pl/Binder.pm` (stats) | shim | -- |
| `POSIX`, `Cwd`, `Fcntl`, `B`, `mro`, `IO`, `IO::Handle` (+`IO::File`, `IO::Seekable`) | core | driver, Moo/Sub::Quote (`B`), file I/O | shims | -- |
| `Data::Dumper` | core, XS + its own pure-Perl path | `Pl/Environment.pm`, `Pl/PExpr.pm` -- diagnostics only | the PP path | load it lazily |
| `Storable` / `Class::XSAccessor` via `PPI::XSAccessor` | -- | `PPI::Cache` / opt-in only | **never loaded** | -- |

**Answer: one more module needs work -- `Params::Util`, and only a tiny shim.**
Everything else XS in the closure is core and already shimmed, or has its own
pure-Perl half, or is never loaded.  The non-XS rest is plain Perl that PCL
already runs (Moo, Sub::Quote, Sub::Defer, Safe::Isa, JSON::PP, Exporter,
constant, overload, version, File::Spec, Getopt::Long ...).

## 3. Feasibility, measured (s533; under load 15 -- wall times about 2x inflated)

**Transpile census** -- every compiler and PPI file through `pl2cl --module`
with `PCL_DROP_ANNOUNCE=all` (122 files, 79,562 lines, 554 s):
* 119 files: no drop, no refusal (`Pl/Parser.pm` 13,107 lines, `Pl/Parser2.pm`, `Pl/ExprToCL.pm` included).
* `vendor/PPI/lib/PPI/Lexer.pm:715` DROPPED: `my $Structure = $self->$method($Statement)->new($Token);`
  -- a method call by NAME IN A VARIABLE followed by `->`.  **A PCL compiler bug (#4481)**,
  minimal: `$o->$m(1)->new(7)` -- perl `Q 7`, PCL a dropped statement.  It is what made PPI fail.
* `Pl/PPIExpect.pm:106` DROPPED: `our %KEYWORD = ( ... __DATA__ => [...], ...)` with
  `__DATA__ =>` at the start of a line -- **a PPI bug (#4482)**, even on PCL's branch:
  PPI starts a DATA section there and swallows the rest of the file; perl reads a hash key.
* `Pl/PPIBugs.pm` REFUSED: one top-level form of 85,620 chars exceeds the 64,000-char limit
  (the registry's data table) -- split the table or emit it as data (P3).

**PPI running under PCL** (`use PPI; PPI::Document->new(\$src)`):
* plain: dies at `Params::Util` (above).  With `PERL_PARAMS_UTIL_PP=1`: all ~90 modules load,
  0 run-time drops, then `Lexer failed` -- #4481's dropped line.
* with that one line split in a scratch copy: **identical to perl** on the probe (24
  tokens, 3 statement classes, sub names) and on `lib/Math/BigInt/Calc.pm`:
  **19,983 tokens, 59 subs; parse 0.66 s vs perl 0.19 s (3.5x); `find('PPI::Statement::Sub')`
  0.17 s vs 0.02 s (8x)**.
* process cost: cold (module cache empty) 117 s; warm (`pcl`, fasl cache) 7.5 s, of which
  ~6 s is LOADING the ~90 module fasls -- a saved core removes that entirely.

## 4. Design

### 4.1 The bootstrap
* **Stage 0** = today's perl-hosted `pl2cl`.  At install time (the XS model, like the
  runtime core) it transpiles `Pl/*.pm`, `Pl/PExpr/*.pm` and `vendor/PPI/lib/**` (plus the
  shims they need) and loads them into the saved core.  Moo's load-time string evals
  (Sub::Quote's accessors) are served by the stage-0 server during this build and are
  baked into the core -- nothing of that runs again.
* **Stage 1** = that core.  `p-transpile-string` calls the transpiled `parse_source`.
* **Stage 2** = stage 1 transpiling the compiler again.  Its output must equal stage 0's
  output for the same files (the GCC three-stage check).

### 4.2 The in-image entry
* One CL function, `%p-transpile-in-image (text &key mode pkg captures features protos deps-path)`,
  calling the transpiled `parse_source` / `build_eval_preamble` / `write_deps_file` (the
  same three calls the server makes), `mode` = `:eval` | `:module` | `:program`.
* A switch, `PCL_TRANSPILER=image|perl`: `perl` stays the default until P5; `image` is
  used when the core carries the compiler; a development tree without it keeps the server.
* Cache keys: the eval / module caches already key on `*pcl-cache-generation*` and the
  compiler stamp (#1843); the in-image compiler gets its own stamp (the core's hash), so
  the two hosts never read each other's entries until P5 proves them identical.
* Errors: the server's `err` status becomes a Lisp condition carrying the same text;
  the eval-site die (`$@`) is unchanged.

### 4.3 The bar: two compilers, one output
`tools/corpus-diff.pl` already compares two emissions of the same inputs.  A
`--compiler image` mode runs the stage-1 core on the same inputs; the bar is
**byte-identical over the four populations** (perl-tests 111, perl's own `t/`,
`lib/**/*.pm` + core lib, the CPAN board) **and** stage 2 == stage 0 on the
compiler's own files.  A difference is always a PCL bug (or a host leak, §4.5) --
the most sensitive PCL test there is, since the compiler is ~80,000 lines of
real, string-heavy, OO Perl.

### 4.4 The prerequisite: hash-order independence (#3961)
Perl randomizes `keys` order; PCL has its own order.  `Pl/` has 158
`keys`/`values`/`each` sites, 55 of them sorted.  Any emission that depends on an
unsorted order differs between the two hosts and fails §4.3.  #3961 already found
one (CPAN/FirstTime.pm's `prompt()` call kind) by running `pl2cl` under
`PERL_HASH_SEED=0..7`.  **P0 = that measurement over the four populations, every
differing site sorted or given a declared order** -- a correctness fix on its own
(corpus-diff reports false DIFFs today; s533b met two).

### 4.5 What the compiler reads from its host
| fact | today | in the image |
|---|---|---|
| `$^X` (`Pl/ProtoCache.pm` cache key) | the perl binary | the core's identity |
| `@INC`, `%ENV`, `%Config` | the transpiling perl's | the PROGRAM's (`@INC` is what `require` searches anyway); `%Config` = PCL's shim |
| the prototype cache on disk (JSON::PP + Digest::MD5 files) | written by perl | same files, same format -- shared by both hosts only after P5 |
| `Time::HiRes` timings (Binder stats) | real | real (shim) |

### 4.6 Performance
* Gone: the 0.35 s first-eval start, the pipe, the second process's memory (44 MB).
* Slower: the transpile itself.  PPI under PCL parses at ~3.5x perl's time and
  `find` at ~8x; the rest of the compiler is likely similar (the README's whole-program
  ratio is 1.9-3.4x).  Estimate: a typical small eval ~16 ms -> ~50 ms.  Cache hits
  (most evals: the same accessor text every run, #1200) cost the same as today.
* Two cheap levers, measured first: (a) PPI's `PPI::Node::_wanted` builds every
  `find('Class')` matcher with a STRING EVAL (`Node.pm:648`) -- replace it with a closure
  on the `pcl` branch (faster in perl too; upstreamable); (b) the compiler's own hot
  paths, profiled under PCL once it runs.
* Core size: ~60 MB today; +80,000 lines of transpiled Perl -- measure in P3
  (`save-lisp-and-die :compression` if needed).

### 4.7 Moo in the compiler
Ten compiler files `use Moo` (90 `has` declarations).  It works under PCL, but it
brings Sub::Quote's load-time string evals, `B`, and the `Class::XSAccessor`
probe into the image.  Option (recommended, P2a): replace `has` with plain
constructors + accessors -- a mechanical edit, emission-neutral for user code,
fewer dependencies in BOTH hosts and a faster compiler start in perl too.  Not
required: §4.1's build-time baking also works with Moo.

## 5. Phases (each with its bar)

| phase | what | bar | size |
|---|---|---|---|
| **P0** determinism | #3961 over the four populations: `pl2cl` under `PERL_HASH_SEED=0..7`, every differing site fixed at its ONE table | all seeds byte-identical; corpus-diff IDENTICAL at seed 0 | 1 session |
| **P1** PPI under PCL | fix #4481 (PCL) + #4482 (PPI branch); `lib/Params/Util.pm`; PPI's OWN test suite (the `pcl` branch's `t/`, 71 files / 57,396 assertions under perl -- `vendor/PPI` carries `lib/` only, so P1 runs it from the bare repo `~/pcl-agent-scratch/ppi/PPI.git`) under PCL as a conformance target; then the token stream of every population source under PCL-PPI == perl-PPI | PPI suite pass rate (target 100 % of what perl passes); token streams identical | 2-4 sessions -- every PCL bug it finds is a real CPAN bug |
| **P2** the compiler under PCL | (P2a Moo out, optional) `PCL_TRANSPILER=image` prototype: a PCL program that loads the transpiled compiler and transpiles a file | §4.3 over corpus-diff's 111, then the four populations | 3-6 sessions |
| **P3** the core | build the compiler into the saved core at install (`tools/install-pcl`, `PCLSbcl`); `Pl/PPIBugs.pm`'s oversized form; core size + start-up measured | `prove tools/t/install-pcl.t`; the container test | 1-2 sessions |
| **P4** eval in the image | `p-transpile-string` -> `%p-transpile-in-image`; the gate's eval rows, the sweep and the everyday battery under BOTH engines | gate PASS, sweep TOTAL +0, EVERYDAY equal, both engines; eval latency table | 1-2 sessions |
| **P5** modules + default flip | `p-transpile-file` too; a container run with NO perl on the PATH (`require` of an uncached module, `eval "use Foo"`); `image` becomes the default when present | the no-perl container leg green; `docs/single-binary-plan.md` §3.5 rewritten (a binary's eval WORKS) | 1-2 sessions |
| **P6** fixed point in CI | stage 2 == stage 0 on the compiler's files, run in CI | CI green | 1 session |

**Total ~10-16 sessions.**  Order with the parser program (#3694): P0 and P1 can run
now (P1 sits next to the PPI batches).  P2 is easier after C2 deletes v1's
`Pl/Parser.pm` (13,107 lines less to self-host; it does transpile cleanly today, so
this is cost, not a blocker).  From P6 on, **every compiler change must also
compile under PCL** -- the CI check is what keeps the compiler inside the subset PCL
supports.

## 6. Risks
1. **A PCL bug that miscompiles the compiler** emits wrong CL for user code silently.
   Mitigation: §4.3 (byte-identical over four populations) + P6 in CI; the perl engine
   stays available as the A/B reference.
2. **Speed**: if in-image transpiles are much slower than ~3x perl, cold runs of
   eval-heavy programs get slower (warm runs do not: caches).  Measured in P4 before P5.
3. **Two hosts**: the compiler must stay inside what PCL compiles.  Mitigation: P6.
4. **Memory**: PPI trees under SBCL (PPI's `%_PARENT` weak links); measured in P1 (the
   probe peaked at 186 MB RSS).
5. **Hash order** (§4.4) is the likeliest source of P2 diffs -- hence P0 first.

## 7. Open questions for the USER
1. Moo out of the compiler (§4.7, P2a) -- yes / no / only if it blocks?
2. Is ~3x slower per uncached eval acceptable in exchange for no perl at run time
   (cached evals unchanged)?  Or keep the perl server as the default where perl exists,
   and use the image only where it does not?
3. When: P0 + P1 in the next rounds beside the parser program, P2.. after C2?
