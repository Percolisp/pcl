---
name: project_cpan_module_survey
description: Testing pure-Perl (XS-free) CPAN modules through PCL — survey results & open bugs
metadata: 
  node_type: memory
  type: project
  originSessionId: ac932eb0-4a77-41cd-aef3-b89c2e716d88
---

Session 233 began testing **XS-free CPAN modules** end-to-end through PCL (user
goal: "hash out problems"; network IS up — see [[project_network_and_cpan_available]]).

**STANDING RULE (s258b, user): ASK before installing ANY CPAN module.** Smoke-test
what's already installed (site_perl / `~/.cpan/build`); ask before fetching a new
dist tarball to run its `t/`. **DIRECTION (s258b): lead with CPAN BREADTH** — widen
PAST the Moo cluster (the same 7 dists we keep re-testing); each new module = harvest
the GENERAL bug it exposes, fix at the right layer, regression-test, and **track
distinct bug-CLASSES to measure convergence** (target = arbitrary pure-Perl; tiers:
(a) primitives=fuzzer, (b) idioms/mechanisms=CPAN [converging], (c) interp internals=out of scope).

**SESSION 258c (2026-06-17) — strategy agreed + `@ISA = qw/.../` non-bracket-delimiter bug FIXED (commit `2c74f3d`). Gate 95/3480.** Started CPAN BREADTH with **YAML::PP** (installed, pure-Perl, non-OO code shape). Found GENERAL bug: `_extract_parent_classes` (`Pl/Parser.pm`) stripped only bracket qw delimiters `([{<`, so `our @ISA = qw/ Parent /` (slash/`!|#`) kept `qw/`+`/` → bogus parents `("qw/","Parent","/")` → broken `(defclass …(qw/::plc-qw/ … /::plc-/))` → READ error "Package QW/ does not exist". `qw(...)` always worked. Fix: strip qw + ANY non-word delim (mirrors `_process_use_base`). Test misc-fixes-02.t (80). **YAML::PP next walls (deeper, NOT fixed):** `Handle single node of unknown type: ref=''` ×7 (PExpr codegen gap); dynamic `Module::Load::load` w/ computed name → `Can't locate .pm`/`NIL not STRING`. **NEXT (tomorrow): continue breadth — Test::Deep, Hook::LexWrap, Sub::Uplevel, PPI (biggest pure-Perl test); ASK before installing.** (Strategy: tier-(a) primitives=fuzzer, (b) idioms=CPAN[converging], (c) interp-internals=out-of-scope; per-stmt handler-case = worthwhile robustness investment.)

**SESSION 258b (2026-06-17) — CPAN re-survey + %INC double-slash bug FIXED (commit `64c1438`). Gate 95/3479.** Re-ran dist suites via `tools/run-dist-t.pl <dist> t/foo.t --summary` after the func-arg fix. **Status now:** Class::Inspector `class_inspector.t` 53/1 (only evil-`->isa` crash test 54), `_functions` 16/3 (7-9 = export-not-found), `01_use` 3/0✓; Safe::Isa `safe_isa.t` 63/5, `safe_does.t` crash-after-9; Try::Tiny `basic.t` 25/0✓, `context.t` 5/8 (VOID-in-catch wantarray), `named.t` 0/3 (Sub::Util::set_subname + caller(0)[3]), `finally.t` 11/4 (DESTROY); Role::Tiny `role-tiny.t` 22/0✓ `proto.t` 5/0✓ `does.t` 13/1; Data::Dump `dump.t` uses ANCIENT `use Test`→`TEST::_` unbound (pre-existing s254); CMM still load-crashes (eval-lexical-capture). **GENERAL BUG FIXED: %INC keys had DOUBLE slash** — `p-module-to-path` `(substitute #\/ #\: name)` turned `Foo::Bar`→`Foo//Bar.pm` (OS tolerates `//` opening the file so loading worked, but `$INC{"Foo/Bar.pm"}` / `loaded_filename` missed). Fixed: collapse `::`→one `/`. Also added the missing `%INC-MARKER%` case to `p-keys`/`p-values` (`keys %INC` returned `()`). Every multi-seg module had a wrong %INC key. Test in misc-fixes-02.t (79). **NEXT fixable: CI test-54 evil-`->isa` crash (aborts file); `Class::Inspector::Functions` export-not-found.**

**SESSION 258 (2026-06-17) — s257 OPEN THREAD CLOSED: unprototyped user-FUNCTION-call args are LIST context. Commit `96a111b`. Gate 95/3478, sweep +40 (18088), 66 fully-passing held.** The twice-flagged thread is DONE. **Root cause of the s257 ~170-test regression:** the TAP assertions (`is`/`ok`/`like`/…) LOOK unprototyped but carry real `($$@)` prototypes (perl-core `t/test.pl`: `sub is ($$@)`) → leading `$` slots force SCALAR, so `is(unpack(...),$exp)` runs unpack scalar (matches perl). perl-tests reach them via `require './test.pl'`, NOT `use Test::More`, so the shim-proto extractor never saw them; blanket LIST list-ified `is`'s 1st arg → pack/aassign broke. **Fix = make the discriminator prototypes, read like a shim (NOT test code in the parser — user corrected me):** (1) new `_extract_file_prototypes` (`Pl/Parser.pm`): literal-path `require` now extracts prototypes from the required file (require-equiv of `_extract_module_prototypes`; nested requires recurse → `test.pl`→`t/test.pl` redirect followed); (2) `perl-tests/t/test.pl` declares the real TAP prototypes as forward decls (`sub is ($$@);`…, no bodies — runtime `pl-is` still supplies TAP; `p-declare-sub` no-clobber); (3) `child_context`: unprototyped non-builtin funcall args→LIST, prototyped subs keep per-slot ctx. **+2 latent bugs exposed & fixed:** bit-shift/bitwise `<< >> & | ^` now force SCALAR on operands (`($x||255)<<8` gave 0 in list ctx); `chop`/`chomp` force LIST on args (`is(chop(@slice))` collapsed the slice) — kept a **context-only special-case branch, NOT a `_builtin_prototypes` (@) entry** because the proto table is ALSO read by codegen → broke `chomp @a` (answer to user's "wouldn't a prototype table be simpler?": no, "prototype" conflates codegen + arg-ctx in PCL). aassign +39, 48 newly-pass, 8 sweep-diff "new" = stale-baseline artifacts (verified file-by-file). **Pre-existing edge NOT fixed:** `chop $f,@duh,$bar` no-parens = named-unary in perl (chops only `$f`), but PCL `[-1,-2]` always grabs the list. **Process: don't `git stash` when a stash exists (collided with pack-P WIP, leaked `UU cl/pcl-pack.lisp` — restored, pack-P safe in `stash@{0}`); use a worktree/copies. Baseline NOT re-blessed (later single-file sweeps clobbered `.faillog`) — needs a fresh clean full sweep next session.**

**SESSION 257 (2026-06-17) — block-scoped package @ISA wall DOWN; Safe::Isa 0→63/68, Class::Inspector RUNS. 3 commits.** Knocked down the s256 NEXT blocker (block-scoped `{ package X; @ISA=… }`). Gate 95/3475, sweep 18048 pass / 66 fully-passing held / 0 real regressions.
- **`24f9da8` block-scoped `{ package X; our @ISA=(…) }` inheritance** (3 sub-bugs, `Pl/Parser.pm`): the block emits the package as ONE `(let …)` top-level form so the inner `(in-package)` is a NO-OP at READ time → (a) parented "Redefine" defclass hoisted to preamble got clobbered by the inline bare defclass [fix: emit parented inline when `_block_depth>0`]; (b) `@ISA` defvar unqualified→`MAIN::@ISA` but push runs in `:Dog`→`Dog::@ISA` empty [fix: `_qualified_isa_symbol`]; (c) bare `(defclass plc-foo)` interned in read-pkg but sibling super-ref `Foo::plc-foo` left it FORWARD-REFERENCED→FINALIZE-INHERITANCE crash [fix: `_qualified_clos_class`].
- **`5e9a527` method call on undef/unblessed-ref DIES** (`p-method-call`): was falling through nil-class→"main"→lived; now dies "on an undefined value"/"on unblessed reference" (Safe::Isa `$_isa/$_can` rely on it under eval).
- **`e005e37` method-call args are LIST context** (`child_context` methodcall case): `File::Spec->catfile(split /::/,$name)` ran split in scalar→count `2`→Class::Inspector `->filename`="2.pm". Fixed for the METHOD path only.
- **✅ CLOSED in s258 (see top):** the SIBLING func-call case (`myfunc(split…)`→`2`). The narrow discriminator = prototypes from `test.pl` via the new require-path extractor.
- Safe::Isa remaining 5 = wantarray/`is_deeply` ctx propagation (deferred). Class::Inspector: `class_inspector.t` 50/54, `_functions` 13/19, `01_use` 3/3; remaining = the func-call LIST fix + `->subclasses`/evil-`->isa` crash (~line 769). **Runner caveat still holds: `rm -f ~/.pcl-cache/*.lisp` before re-testing a dist.**

**SESSION 256 (2026-06-16) — Try::Tiny t/basic.t 1→24/25; THREE general bugs fixed (commits `e073213`, `526d481`).**
Drove Try::Tiny's own test suite (`tools/run-dist-t.pl <dist> t/basic.t --summary`). Three independent, GENERAL bugs (all gate-green 95 files / 3459 tests; tests in `Pl/t/misc-fixes-02.t` 57→63):
1. **`scalar(eval{die})` dropped from lists** (`e073213`). scalar() of undef returned raw `nil`; `p-flatten-args` (used by `return (LIST)` in list context) splices raw nil as an empty list → the ubiquitous `return (scalar(eval{...}), $@)` test-helper idiom dropped the undef + shifted the list. Fix: `p-scalar` returns the `*p-undef*` sentinel (like literal undef) for an undef value, not raw nil.
2. **`(&;@)` block-form over-slurp** (`e073213`). `try {42}, 42, "d"` swallowed the trailing comma args into try's `@_` → croak "unexpected argument". Perl's slurpy `@` consumes only JUXTAPOSED terms; a comma terminates the slurp. Fix in `Pl/PExpr.pm` block-form path: stop the slurp at a leading comma (gated on `$has_block_proto`, so grep/map/sort list-ops unaffected).
3. **Prototype-driven SCALAR arg context — THE general one** (`526d481`). A function argument that lands in a `$` (or ref `\$`/`\@`/`\%`) prototype slot now gets SCALAR context, mirroring the pre-existing slurpy-`@`→LIST rule. Before, `child_context` only forced LIST on the slurpy tail and left `$` slots to **inherit the caller context = VOID at statement level**, so `wantarray()` reported undef inside any sub called as an argument. THAT is why `is(try{42},42)` returned undef: Test::More `is($$;$)` ran try in void → its no-context else-branch → undef. Three-part GENERAL fix (NO Test::More special-case in the parser): (a) `Pl/PExpr.pm child_context` honors `$`/`\X` slots → SCALAR; (b) `Pl/Parser.pm _merge_module_prototypes` now propagates scalar/ref/slurpy prototypes from a shim (was block/ref only); (c) `_extract_module_prototypes` reads the Test::More/Test::Simple shims (still skips the heavy Test2 stack); (d) NEW `lib/Test/More.pm` = prototype-ONLY forward decls (is/ok/like/cmp_ok/isnt/unlike/isa_ok/BAIL_OUT) — runtime still supplies the TAP layer, this file is read by the parse-time extractor only.
**REJECTED broad heuristics** (both regress): `void→scalar` for all args breaks list-returning args (Try-Tiny 23→12, wantarray-01, misc-fixes-02); `void→list` breaks `is(unpack(...))`/pack.t. **Prototypes are the only correct discriminator** — that's why the fix is prototype-gated.
4. **`die` preserves ANY reference in `$@`** (`90a6ad3`) — was blessed-only. `die { prev => $@ }` (unblessed hashref, Try::Tiny basic.t test 24) fell to the string branch → `"HASH(0x..) at line N"`. Broadened `p-die`'s object-exception check to any ref. → **Try::Tiny basic.t now 25/25.**
5. **`my $x = EXPR if COND`** (`bdcdd7e`) = `my $x; $x = EXPR if COND;`. Lexical declared unconditionally (scanner's let, re-bound per call, no stale carryover); only the assignment is conditional. Was: `my $c = shift if @_>1` mis-parsed as `(p-shift (p-if ...))` crash; `my $c = 5 if @_>1` dropped the initializer. Fix in `_process_variable_statement`: in-sub `my` + trailing modifier → strip declarator, route `$x=EXPR if COND` through the expr-statement path. Found in `lib/File/Spec.pm` via Class::Inspector.
6+7. **`require "interp/$var.pm"` + interpolated `@ISA` element** (committed end-of-session IF gate `/tmp/gate6.txt` green; else uncommitted in working tree). require: emitted the quote's raw `->string` (no interpolation) — fixed to route interpolating-quote-with-sigil through the runtime expr path. @ISA: `our @ISA=("File::Spec::$module")` baked the LITERAL into the compile-time CLOS defclass — fixed via `_classify_isa_parents` (literal parents → defclass+MRO; interpolated → runtime `(p-push @ISA (p-string-concat ...))`, resolved by `%pcl-isa-ancestry`). **These two make the REAL (pure-Perl, NOT XS) File::Spec load+work** (`File::Spec->catfile('a','b','c')`→"a/b/c" with the shim removed). `lib/File/Spec.pm` is pure-Perl, shimmed ONLY for those two now-fixed reasons; header documents removal-is-now-possible-but-DEFERRED.
**NEXT WALL (where s256 stopped):** dropping the File::Spec shim + finishing Class::Inspector/Safe::Isa is blocked by the **block-scoped `package Baz; @ISA=...` CLOS crash** ("Not a legal superclass name"/FINALIZE-INHERITANCE on a forward-ref class — inline/`do{}` package + `@ISA` defclass super). Fix that → re-survey → drop the shim. Sub::Quote needs `B::svref_2object` (XS, skip).
**Try-Tiny basic.t test 24 (NOW FIXED — was):** `$caught` held `HASH(0x1) at …line 152` not a clean hashref — die-ref-preservation, fixed by #4 above.
**Known SEPARATE divergence found (NOT fixed):** a parenthesised list literal in a `$` slot — `p1((LIST))` with `sub p1($)` — yields LIST in PCL, SCALAR (comma-op last elem) in perl, because the `progn` (comma operator) node forces LIST on its children regardless of scalar context. Edge case; tangential to prototypes.
**Runner caveat re-confirmed:** `rm -f ~/.pcl-cache/*.lisp` before re-testing a dist — STALE module FASLs gave false failures (a pre-fix Try::Tiny compile lingered and showed undef even after the fix landed; cache key is path-based so `use lib '/tmp/copy'` masked it).

**SESSION 255c (2026-06-16) — ALL 6 sweep CRASHES FIXED.** Each now exits CLEANLY (the files still under-count, but no longer abort). Sweep 18020→**18049 pass**, 66 fully passing held, 0 zero-passing, 0 real regressions (the 7 sweep-diff "new" are the same stale-baseline artifacts; baseline last blessed 449→482). Fixes (gate 3457/3457; tests in `Pl/t/misc-fixes-02.t` 51→57):
- **Bucket A (3 files) — `@{bareword}` autoquote + `$^H`/`%^H` specials.** A lone bareword in a deref block is a SYMBOLIC ref to the package var, never a sub call: `@{foo}`/`"$x->@{foo}"` emitted `(pl-foo)`→UNDEFINED-FUNCTION. Fixed in `Pl/PExpr.pm` (`_block_sole_bareword` + Block-deref autoquote) and `Pl/PExpr/StringInterpolation.pm` (interp autoquote). Fixed **postfixderef.t** (`*@{HASH}`) AND **magic.t** (same line!). **eval.t** was `\%^H` (hints hash) unbound: added inert `$^H`(0)/`%^H`(empty hash) specials — `%SPECIAL_VARS` in `Pl/ExprToCL.pm` + runtime defvars/exports in `cl/pcl-runtime.lisp`.
- **Bucket B (2 files).** **index.t**: embedded `our $var` in a `use constant` value (`\our $referent`) was stripped by extract_declarations and never defvar'd → fixed in `_compile_constant_value` (`Pl/Parser.pm`) via new env `expression_our_vars` (emitted in the forward-decl pass) + `Pl/Environment.pm` field. **scalar.t**: `select STDERR` evaluated the bareword FH as an unbound var → new `select(BAREWORD)` special-case in `Pl/ExprToCL.pm` (mirrors the `readline(BAREWORD)` one), emits `(p-select 'NAME)`.
- **Bucket C (1 file).** **multideref.t** `($r//0)->[i]{k}...=v`: a parenthesised scalar arrow-deref base is a single-child `tree_val`; in LVALUE/LIST_CTX it rendered as `(vector ...)` → autoviv into a bogus 1-elem vector → `p-autoviv-aref-for-hash` TYPE-ERROR. Fixed in `Pl/ExprToCL.pm` gen_array_ref_access/gen_hash_ref_access via `_is_paren_scalar_base` + `_gen_scalar_deref_base` (force SCALAR ctx). RVALUE reads were already correct — bug was lvalue-only.
- Post-fix sweep status (all clean exit now): postfixderef 118/128, magic 181/208, eval 163/169, **index 109+1+10/120 FULLY COMPLETES**, scalar →125+, multideref 52/65. Remaining stops are ordinary under-counts (not crashes).

**Earlier (255b) characterization (superseded by the fixes above):**
Of the 14 "Partial (early stop)" files, 6 were TRUE crashes; the other 8 exit 0 and under-count. **bop.t is NOT a crash** (runs clean to 507/510). Clean under-counts (no fix needed): bop.t 507/510, caller.t 65/112, length.t 47/49, method.t 159/163, ref.t 237/245, state.t 162/166, reset.t & tr.t (reach final test). Char via `cd perl-tests && sbcl … --load /tmp/char_<n>.lisp; echo $?`.

**SESSION 255 (2026-06-16) — string `require` blocker FIXED; dist-test runner now permanent.**
- **String `require "Foo/Bar.pm"` now resolves via @INC/shims** (was: only cwd → `Can't locate`). `p-require-file` (`cl/pcl-runtime.lisp`): a `.pm` path is converted to a `::` module name and delegated to `p-require` (reusing @INC search, lib/ shims, XS-only/Test::More shortcuts, caching, %INC); non-`.pm` paths keep literal cwd load + an @INC fallback. **This unblocks `use if COND, MODULE[, ARGS]`** — the `if` pragma builds `"MODULE.pm"` and string-requires it. Verified: `use if 1,"strict"` (pragma), `use if 1,"Scalar::Util","blessed"` (real module+import), false branch, bareword require, absolute-path require all work.
- **Pragma load no-op generalized**: new `*p-pragma-modules*` (strict/warnings/feature/utf8/open/bytes/locale/integer/re/overloading/warnings::register); `p-use` now skips LOADING them (was only the import *method* stubbed → string-require of a pragma hit `STRICT::$^H unbound`). One list drives both the load-skip and the method-stub loop.
- **Runner now permanent at `tools/run-dist-t.pl`** (was ephemeral `/tmp`): `tools/run-dist-t.pl [--no-dist-lib] [--summary] <dist-dir> <t-file>`. `--no-dist-lib` for XS-stubbed dists (Scalar-List-Utils) to avoid polluting pl2cl's own @INC.
- Tests: `Pl/t/use-require-01.t` 41→44 (string-require via @INC, `use if 1,MOD` true branch+import, pragma no-op). Gate **3451/3451 green**; sweep **18020 pass / 66 fully passing / 0 zero-passing** (identical to s254 — 0 real regressions; the 7 sweep-diff "new" are the same stale-baseline artifacts, baseline last blessed 449→482, several sessions old).
- **STILL OPEN (the other s254 blocker): block-scoped `do{package X; sub f{} ...}`** — `sub f` is hoisted to a top-level def as bare `pl-f` (in `main`), but `\&f` *inside* the block resolves to qualified `|X|::pl-f` → `undefined function`. Sub-qualification/hoisting bug; blocks Class-Method-Modifiers. NEXT target.



**SESSION 254 (2026-06-15) — ran the modules' OWN `t/*.t` suites; problems CONVERGE; shipped collision+casing+blessed fixes (commit `dee80e7`).**
`use Test::More` is now wired internally (loads PCL's TAP layer on demand) → CAN run dist test suites. Runner: `/tmp/run-dist-t.pl <dist-dir> t/foo.t` (transpiles+runs a dist's t-file with its lib/t-lib on @INC). **CAVEAT: don't add `$dist/lib` for XS-stubbed modules (Scalar/List::Util) — it pollutes pl2cl's OWN @INC (pl2cl `use`s Moo→Scalar::Util) → false TRANSPILE-FAIL; PCL uses its lib/ shims anyway.**
Swept 7 worked-with dists (~/.cpan/build: Try-Tiny, Role-Tiny, Safe-Isa, Data-Dump, Class-Inspector, Class-Method-Modifiers, Scalar-List-Utils). **Crashes cluster into ~7 buckets, dominated by 3** → answers user's "infinite problems" fear: NO, finite.
- **CL-name collision (28 files, biggest): FIXED.** Perl pkg upcasing to a locked CL symbol (`If`→CL:IF via `use if`, also `Second`/`Symbol`) crashed `(defclass NAME)`. Fix: CLOS class names now **`plc-`-prefixed** (`_pkg_to_clos_class` + `perl-pkg-to-clos-class`/`clos-class-to-pkg`); dropped ad-hoc escape list. Runtime dispatch is string-based so ref/blessed unaffected. **Naming discipline: builtins `p-`, user subs `pl-`, classes `plc-`.**
- **`package Class`/`Error`/`Method` casing: FIXED.** 3 copies of a class/error/method/function pipe-quote special-case (`_cl_pkg_designator` + Parser.pm ~306 + ~389) disagreed with each other AND the runtime's `perl-pkg-to-cl-pkg-name` (upcase). Unified all through `_cl_pkg_designator` (pipe-quote ONLY multi-seg). Removing it was safe BECAUSE plc- handles the defclass collision. (Incomplete edit briefly desynced → broke substr/pos/vec/hash; lesson: there are 3 designator paths, keep them in lock-step.)
- **`blessed`/`reftype`: FIXED.** Scalar::Util shim now delegates to core builtins (blessed→undef for UNblessed refs, not the reftype); `p-reftype` returns undef (not "") for non-refs; `UNIVERSAL::pl-isa` guarded for non-string reftype.
- **substr.t recovery: FIXED.** lvalue-sub `die` is hard only in eval-string mode; whole-file degrades to per-stmt PARSE ERROR (0→389 ok).
- **STILL OPEN (deeper, NOT fixed — the real CMM/`use if` blockers):** (1) **block-scoped package** `do { package X; ... }` emits `(defvar X::$a)`/qualified calls at READ-time before the inline runtime `(p-defpackage)` runs → "Package X does not exist" read error (this still blocks the whole Class-Method-Modifiers suite — collision+casing were necessary prerequisites, not sufficient). (2) **string `require "Foo/Bar.pm"`** doesn't resolve via @INC/shims the way bareword `use` does (blocks `use if COND,MOD`'s true branch; `use if 0,...` no-op already == perl). (3) pre-existing: `blessed`-was-fine but reftype-on-nonref edge. Gate 3448/3448; sweep 18020 pass / 0 zero-passing (vec.t now fully passes); 7 sweep-diff "regressions" verified file-by-file as stale-baseline artifacts (committed baseline is many sessions old).
- Other buckets: Data::Dump 10 files = `use Test` (OLD pre-Test::More module) → `$_` (`TEST::_`) unbound; Safe::Isa 2 = CLOS finalize-inheritance on forward-ref class; Role::Tiny 6 SIMPLE-ERROR (role composition internals) + role-basic-* need `My::Example` test helper.

**SESSION 240 (2026-06-09) — Moo LOADS its whole stack, accessors generate, rw set/get works; 6 general bugs fixed.**
Chased `use Moo; has x=>(is=>'ro'); Point->new(x=>3)` end-to-end. Gate green 91/3318 throughout; misc-fixes-01.t 126→133. Two commits (first 3 fixes, then 3). Bugs (each a Moo wall):
1. **`package NAME;` inside BEGIN/scheduled block is block-scoped** — leaked past the block, so later subs resolved unqualified calls in the inner pkg (Moo's `Method::Generate::Accessor::_Generated` idiom). `_process_scheduled_block` snapshots pkg stack + bumps _block_depth + reverts. COMMITTED `0aebf0a`.
2. **`$obj->${ EXPR }(args)`** method-name-as-scalar-deref (Moo::Object) — new PExpr arrow case (Case 1E) + gen_methodcall routes computed (internal-node) methods through dynamic p-method-call. COMMITTED `0aebf0a`.
3. **`Carp::short_error_loc`/`long_error_loc`** added to lib/Carp.pm. COMMITTED `0aebf0a`.
4. **`caller()` returned UPCASED single-seg pkg** (`POINT` not `Point`) — THE KEY MOO BUG. Moo keys ALL per-class %MAKERS state on `caller`, but blesses into the correct-case name → mismatch silently broke construction. Root: orig-case registered only by `p-set-current-package`, emitted AFTER the pkg's `use` stmts, so `pcl-pkg-perl-name` fell back to the CL name during import. Fix: new exported `p-register-pkg-name` emitted in the pkg PREAMBLE (before `use`). NOTE: a DIRECT `Class->import` method-call (not via `use`) STILL returns the method's own pkg as caller — separate unfixed issue, only the `use` path is fixed.
5. **`CORE::<builtin>`** now == the builtin: `CORE::shift()`/`CORE::shift` default to @_; `CORE::ref $x` (no parens) is a named unary (was a bareword string). handle_subcalls normalizes `CORE::foo`→`foo` (via set_content, gated on known_no_of_params); add_implicit_default_param strips CORE::. Moo's quote_sub'd code is full of CORE::shift()/CORE::ref/CORE::join.
6. **Nested ternary in TRUE branch w/o parens** `A ? B ? C : D : E` failed to parse ENTIRELY (not just named-unary) — inner `?`'s false-end scan used `prec<15`, swallowed the outer `: E` (the `:` is also prec 15). Fixed scan to stop at a `:`. **perl-tests/cond.t (17 lines) tests &&/||/eq, NOT `?:` at all — real coverage gap; user flagged it.**

**MOO REMAINING WALL (240):** `Point->new` now reaches MGC's generated constructor and arg-copy WORKS (MGC objects store package/attribute_specs). Dies `assert_constructor: "Unknown constructor for Method::Generate::Constructor already exists"` = `$self->{constructor}` falsy while MGC's own `new` exists in glob → `$Moo::MAKERS{MGC}{constructor}` looks unset post-bootstrap (memoization/coderef-identity in Moo's self-referential bootstrap; subconstructor_handler calls `Moo->_constructor_maker_for($class)` when MAKERS{$class} set but {constructor} not). NEXT: trace why that memo is falsy. See `docs/cpan-module-blockers.md` §240.


**SESSION 239 (2026-06-09) — Moo walls A–C DOWN; Moo loads its whole stack into constructor gen.**
Three Moo walls knocked down in sequence + the general bugs each exposed (all gate-green 69/3315,
sweep-diff 0/0; commit pending at session end). (1) **glob-slot `*{$glob}{EXPR}`** parses now —
variable AND general expression (lone-bareword→string autoquote, else→expr; `Pl/PExpr.pm`
`_glob_slot_spec`/`_attach_glob_slot`); wall A, fixed the `_Utils` mid-load PARSE ERROR that orphaned
`_set_loaded`. (2) **nested-import `caller` binding** — `%p-do-import` binds `*pcl-current-package*`
to the importing pkg (`to-pkg`) around method-import dispatch; `use Foo qw()` inside a loading module
now installs into the right pkg (PCL emits `p-set-current-package` AFTER the pkg's `use` stmts, so
caller lagged). Unblocks `_set_loaded`. (3) **symbolic `\%{"Pkg::Name"}`** deref — `p-cast-%` had no
symbolic-ref case (only `"Pkg::"` stash), so `\%{...}` backslashed the STRING → SCALAR not HASH;
new `%p-symref-hash` (mirrors `%p-symref-array`). This is Exporter::Heavy's `%hash` export, so it
unblocks `use Config`/`%Config`. Same class as 238f's `\&{}` fix, for `%`. (4) **`caller(N)` list
context** returned 1 elem (used `values-list`→truncated; now a list-vector) + **`[3]` subname** via
new `*pcl-caller-subname-stack*` (SBCL can't name PCL's anon-lambda subs) — needed by Exporter::Heavy
`as_heavy`. **Errno shim regenerated** from real module (fix #3's correct Exporter validation exposed
missing EBADF → chdir.t regressed 44/44→partial; fixed). Built **`tools/shim-gaps.pl`** (diffs each
lib/ shim vs the real module; 221→47 gaps after Errno regen; remaining 47 = functions, fill-as-needed).
**Moo NEXT walls:** (a) `Carp::short_error_loc` undefined (lib/Carp.pm missing internal loc helpers —
DO NEXT, now that caller[3] works); (b) `$self->${\(EXPR)}` deref-as-method-name (Moo::Object) =
"Cast unknown type" ×8; (c) eval-lexical-capture (Sub::Quote accessor gen). See `docs/cpan-module-blockers.md`.

**SESSION 238f — GENERIC `use Foo LIST` → Foo->import(LIST) DONE** (commit `57fab5f`); the
custom-import-dispatch gap (long the Moo/Test::More blocker) is CLOSED. `use` now parses the
arg LIST and calls the module's import (own OR inherited **real core Exporter::import**, which
transpiles fine now — no PCL shim). Multi-seg `\&{"Foo::Bar::sub"}` fixed (was the only Data::Dump
bug). Gate 91/3310, sweep 0/0. Re-survey under the new path: **Scalar::Util / List::Util(sum,max) /
Try::Tiny / Safe::Isa / JSON::PP / Data::Dump all ✅** (Data::Dump now byte-exact via the real path).
**NEXT WALL = pragma `->import` cascade** (the thing that gating note below predicted): a module
whose import calls `strict->import`/`warnings->import` (Role::Tiny, Moo) → `STRICT::$^H unbound`
(PCL loads core strict.pm). **DONE 238g (commit `d436707`):** runtime no-op import/unimport for
the pragma packages (UNIVERSAL-stub pattern); core .pm never loads; don't model `$^H`.

**SESSION 238g — Moo + Role::Tiny blow through the pragma cascade; next walls mapped:**
- **Moo**: `use Moo` (a silent no-op all session) now LOADS Moo + RUNS its custom import + clears
  strict/warnings, reaching Moo internals → dies `Moo::_set_loaded undefined`. **ROOT (traced, NOT
  import-related): `Moo::_Utils` aborts mid-load on a PARSE ERROR at `*{$old}{$type}` — a dynamic
  typeglob-slot with a VARIABLE slot name** (in its glob-copy `foreach my $type (qw(SCALAR HASH
  ARRAY IO))`). `_block_is_glob_slot` (Sub::Override work) only accepts LITERAL `{CODE}`/`{SCALAR}`
  barewords (guard vs misreading `*{$x}{$y}`); variable slot falls through → broken CL → load aborts
  before `_set_loaded` (line 221). **NEXT MOO WALL = parse `*{$glob}{$var}` variable-slot glob
  access** → emit `(p-glob-slot <glob> <var>)` (runtime already takes a string slot); mind the
  ambiguity guard. THEN the always-known last wall: accessor-gen via Sub::Quote/eval-lexical-capture.
- **Role::Tiny**: past cascade, into role composition, dies TYPE-ERROR `2 is not HASH-TABLE` =
  `$INFO{$target}{non_methods}` with `$INFO{Comp}`=2 not a hashref (Role::Tiny-internal; deeper).
- Separate small gaps: `List::Util first{}` (block-arg), `defined &glob_installed_sub` cosmetic.

**AGREED DIRECTION (end of session 237b): CONTINUE WITH CPAN MODULES next session.**
Rationale: a `t/op` coverage survey (we have ~99/221; comp 0/25, class 1/10,
re/io/mro/uni barely touched) found broad *test* gaps but few *bugs* on
feature-probing — PCL is mostly correct where probed. CPAN modules are the
higher-yield driver: this whole session's bug cluster (Sub::Quote captures →
`%h=(k=>\$x)` scalar-ref, `return \$x`, top-level `my ($x)=`, the deref-slice/
postfix-deref family) came from chasing Sub::Quote/Moo, not from backfilling
op-tests. **Concrete next CPAN step = Moo** (see Moo blocker map below): wire
custom-`import` dispatch into `p-use`, gated behind making `strict`/`warnings`/
`feature` `->import` no-op method calls + binding `$^H`; then re-survey how far
Moo gets (next wall = eval-lexical-capture for accessor gen — but Sub::Quote
itself now WORKS, so that wall is softened). Also queued (separate, b): pull
`perl-tests/postfixderef.t`+`multideref.t` to guard the deref work.

**Survey (already-installed, XS-free, via `./runpcl`):**
- PASS: `Safe::Isa`, `Role::Tiny`, `List::Util` (PCL shim) — 3/5 ran unmodified.
- FAIL: `Try::Tiny`, `Data::Dump`.

**Session 238b — broader smoke survey of installed pure-Perl modules** (user: find
simpler-than-Moo targets; ASK before installing — all probed were already installed).
Installed pure-Perl set lives under `…/site_perl/5.40.3/` (arch-indep dir); XS ones
(skip) = Clone, Params::Util, Class::XSAccessor, B::COW, Test::LeakTrace, PPI::XSAccessor.
Smoke results:
- **`Class::Inspector` — FULLY WORKS** (238b+238d). `loaded`/`installed`/`functions`/`methods`/
  `function_exists` (238b, commit `814724e`) **and `subclasses`** (238d, commit `8cb0e78`) match
  perl 5.40 (multi-seg + @ISA-linked single-seg, transitive, correct case). `subclasses` fix:
  `p-stash` now adds `"<child>::"` keys for packages one segment deeper (new
  `%p-stash-add-child-namespaces`, orig-case from `*pcl-pkg-name-map*`; runs even for intermediate
  namespaces w/ no CL package). 238b fixes: symbolic `defined/exists &{"Pkg::sub"}`,
  `keys %{"Pkg::"}`→p-stash, multi-seg `@{"Foo::Bar::var"}`.
- **`Sub::Override` — NOW WORKS END-TO-END (session 238c, commit `e7e6665`).** replace +
  restore match perl 5.40. Two fixes: (1) **paren-less named unary before a dynamic glob-slot**
  — `defined *{$g}{CODE}` was a PARSE ERROR, `*$g{CODE}` mis-parsed as `*(%g{CODE})`; new
  pre-pass `_precollapse_dyn_glob_slots` (`Pl/PExpr.pm`, before handle_subcalls) collapses
  `*{EXPR}{SLOT}` AND `*$var{SLOT}` into one glob_slot node; `_block_is_glob_slot`/
  `_glob_slot_name_of` accept a quoted slot ({CODE}→{"CODE"} from Subscript autoquote). (2)
  **blessed-hash `:__class__` leaked into keys/values/each/count/flatten** (broke `keys %$self`);
  new `%p-real-hash-key-p`/`%p-hash-user-count` (`cl/pcl-runtime.lisp`) skip it at all 16
  user-visible sites (clones keep it).
- `Test::Deep` → `Scalar::Util::@EXPORT_FAIL` unbound (looks like a cheap Exporter-emulation
  default-to-empty fix; not done). `YAML::PP` → compile-file failure (deeper). `Sub::Uplevel`
  → `uplevel` not imported (import gap). `oo.pm` loads (trivial). `Hook::LexWrap` → `wrap`
  not imported. `Test::Fatal` → error (builds on Try::Tiny).
- **Next CPAN steps:** (a) dynamic-glob CODE-slot read/install → unblocks Sub::Override +
  Class::Inspector `subclasses` shares the global-walk need; (b) `@EXPORT_FAIL` default.

**XS-free check used:** `find $SITE/auto/<Mod>/ -name '*.so'` (presence ⇒ XS).
Test method: tiny `use Mod; ...` script through `./runpcl`, parse verdict.

**TESTING METHODOLOGY (answered 238d):** we currently do **smoke probes** — tiny `use Mod; …`
drivers diffed **byte-for-byte against stock `perl`** — NOT the modules' own `t/*.t` suites. Why:
a dist's `t/` uses `use Test::More`, which PCL resolves to the real site-perl Test2 stack →
`%Config` unbound → crash (tried Try::Tiny `t/basic.t` in s234, blocked). The `perl-tests/` files
dodge this via perl-core `require './test.pl'`. **Gateway to running real CPAN suites = a Test::More
shim** wiring `use Test::More` → PCL's TAP fns in `cl/pcl-test.lisp` (pl-ok/pl-is/pl-like…) =
**pcl-rollout-plan Phase 3-4**. Building it converts every working module into "N/M of author's
tests pass" — likely the highest-leverage next infra step. (Test::Fatal/Warn/Deep overlap it —
they hook Test::Builder.)

**Try::Tiny — core try/catch NOW WORKS (session 234).** Five general bugs fixed
(uncommitted as of session 234; run `prove Pl/t/misc-fixes-01.t` = 29 tests):
1. **`not` as assignment RHS** — FIXED earlier (commit 0708782).
2. **Multi-segment package casing** (`cl/pcl-runtime.lisp`) — glob/bless/typeglob/
   symref code did `(string-upcase pkg-str)` for ALL packages, creating a wrong-
   case empty `TRY::TINY` that shadowed the real case-preserved `Try::Tiny` (where
   the subs live), so imports found nothing. Codegen rule: multi-segment names
   are pipe-quoted → case-preserved (`|Try::Tiny|`); single-segment → bare →
   reader-upcased (`Carp`→`CARP`). New `perl-pkg-to-cl-pkg-name` helper mirrors
   that; applied at p-make-typeglob/p-glob-assign/p-dynamic-typeglob/p-local-glob/
   bless + `%pcl-find-package`/`p-find-module-package`. Single-segment unchanged.
3. **`use Carp` was a no-op pragma** (`Pl/Parser.pm` line ~5510 regex) → `croak`
   never imported; the runtime stub was in wrong-case pkg `|Carp|`=mixed, unreachable.
   Removed `Carp` from the pragma list + added **`lib/Carp.pm`** shim (croak/carp/
   confess/cluck/longmess/shortmess + @EXPORT). lib/ is earlier in @INC so it wins
   over the real (utf8-looping) Carp.pm.
4. **`&`-prototype block args were 0-arg lambdas** (`Pl/PExpr.pm` ~2042) — `try{}`/
   `catch{}` blocks compiled with is_anon_sub=0, but Try::Tiny calls catch with
   `$error` → "invalid number of arguments". Pass `$has_block_proto` as is_anon_sub
   so the block accepts @_. (`do{}` has no block-proto, stays 0-arg.)
5. **`return` inside `eval { }` exited the whole sub**, not the eval (perldoc -f
   return: return exits an eval BLOCK). `p-eval-block` (`cl/pcl-runtime.lisp`) used
   handler-case (catches conditions) but `(p-return)` is a `throw :p-return` that
   sailed to the sub's catch. Fix: wrap body in `(catch :p-return ,@body)`. This
   was the success-value bug (`try{42}` returned Try::Tiny's inner `return 1`).

**Prototype-aware list context (the subtle one):** the general "user-sub args are
LIST context" rule (Perl default) is CORRECT but DANGEROUS — Test::More's
`is($$;$)` makes its args SCALAR, and PCL doesn't know that prototype, so a blanket
rule forced `is(unpack(...), …)` into list context and **regressed pack.t 10→138
not-ok** (and any `is(context_sensitive_call(), …)`). The safe fix in
`child_context` (`Pl/PExpr.pm`): force LIST_CTX ONLY for args landing in the slurpy
`@`/`%` tail of a KNOWN prototype (exactly `try (&;@)`). Unprototyped subs and
`$`-proto positions are left to inherit. So `outer(inner())` for a plain sub still
(wrongly, but safely) gives SCALAR — full correctness needs PCL to know all
prototypes; deferred.

**Try::Tiny `finally` does NOT run** — uses `Try::Tiny::ScopeGuard`→`DESTROY` at
scope exit; that's the documented DESTROY-via-GC limitation (`not-supported.md`),
not a fixable bug here.

**Running a CPAN module's OWN test suite (session 234, started):** fetched
Try-Tiny-0.32 dist (`curl …/E/ET/ETHER/Try-Tiny-0.32.tar.gz`; cpanm download path
is `http://www.cpan.org/authors/id/…`). Ran `t/basic.t` etc. through
`pl2cl | sbcl (+pcl-runtime+pcl-test)`. Two blockers found:
1. **POSIX.pm `LDBL_MAX` crash — FIXED (commit 7ea47c7).** `t/basic.t` pulls in
   POSIX; `lib/POSIX.pm` `use constant LDBL_MAX => 1.18e+4932` emitted a raw float
   literal SBCL can't read → compile-file crash (also the session-232 `parent.t`
   blocker). Root cause: `_compile_constant_value` (`Pl/Parser.pm`) "single literal"
   fast path returned the raw PPI token, bypassing ExprToCL number codegen (also
   broke octal `0777`→777). Dropped the Number fast path → flows through gen_node →
   `(p-double-inf)` (Inf, matches Perl double-NV) / `#o777`. POSIX.pm loads now.
2. **`use Test::More` loads the REAL Test2 stack → `%CONFIG` unbound crash — OPEN.**
   CPAN test files do `use Test::More`, which PCL resolves to site_perl's Test::More
   → Test2::Util → `%Config` (unbound) → crash. perl-tests mostly use the perl-core
   `require './test.pl'` helper instead, so they dodge this; `parent.t` (a real
   `use Test::More` perl-test) hits the SAME wall and is still 0-passing after fix #1.
   **NEXT STEP = ship a Test::More shim** wiring `use Test::More` to PCL's existing
   TAP fns in `cl/pcl-test.lisp` (pl-ok/pl-is/pl-like/…): either a `lib/Test/More.pm`
   that re-exports, or register Test::More as runtime-provided (skip the load, import
   the built-ins). This is **pcl-rollout-plan Phase 3-4** ("run user tests") and is
   the gateway to running ANY CPAN test suite + unblocks parent.t. NOT yet built.

**Session 235 — Data::Dump NOW WORKS** (commit 9c8403d). Four general bugs en
route: (1) **p-true-p ref-awareness** — a BOXED value is a Perl scalar (a held
ref is ALWAYS true even if referent empty: `my $r=[]`/`{}`); a RAW container is a
bare @/%hash in bool ctx (true iff non-empty). Was: empty %hash wrongly true
(broke Data::Dump `if(%refcnt)`), empty arrayref wrongly false. (2) **`&foo` no
parens re-uses caller @_** (was empty list) — `local $_=&quote` in str(); plus
constants now `(p-sub NAME (&rest %_args) (progn %_args VAL))` so `&CONST` doesn't
arity-error. (3) **do{} in elsif cond** emitted its defun BETWEEN p-if branches →
now inline lambda (return_lambda) preserving bare-if tail-return. (4) **do{} loop
transparency** — body wrapped in `(progn)` not `(block nil)` so last/next/redo
escape to the enclosing loop; `return` exits the enclosing sub. Verified vs perl
5.40. 69 fully-passing held; new tests misc-fixes-01.t.

**Session 235 — method dispatch** (commit 8d7b752, `p-method-call`): (a)
**`$obj->$coderef(@args)`** invokes the coderef directly as `$coderef->($obj,…)`
(was stringifying → "CODE(0x..)" method-lookup fail); (b) **method args now
flatten** (`$o->isa(@a)` spreads @a via p-flatten-args; built-ins p-isa/p-can take
fixed args). Both needed by Safe::Isa.

**Session 236c — per-iteration closure capture FIXED, Safe::Isa works.**
`map { my $x=$_; sub {$x} } qw(a b c)` now returns `abc` (was `ccc`). New
`_begin_block_closure_scope`/`_end_block_closure_scope` (Pl/Parser.pm) reproduce
the `$x__lex__N` rename DIRECTLY in the `parse_block_to_cl_string` string path
(the bucket-based `_emit_scoped_block` does NOT compose with string collection —
that's why the earlier attempt failed): mint a fresh never-defvar'd lexical,
populate the 4 maps codegen reads (state_var_renames, _current_scope_new_renames,
_current_scope_old_renames, _let_bound_vars), wrap body in `(let …)`. Block is a
`(lambda ($_) …)` called once/element ⇒ per-iteration box. No-op when nothing
captured. ALSO fixed `_vars_referenced_in_closures`: only scanned
PPI::Token::Symbol, missing vars used only in string interp (`sub {"v=$x"}`); now
scans interpolating quote/heredoc/regex tokens too (`_vars_in_interpolated_text`).
**Surprise:** the `for my $n (…){ sub {$n} }` foreach case ALREADY WORKED (stale
memory said broken) — verified all variants; closure.t 50/50. So per-iteration
capture is COMPLETE. **Safe::Isa now works end-to-end** ($_isa/$_can/
$_call_if_object). Gate 3262, sweep 18045/785/69, diff 0/0. Tests misc-fixes-01.t
73→77.

**Session 236 — Class::Method::Modifiers survey: 3 general bugs fixed** (committed).
CMM (pure-Perl, used by Moo) drove out: (1) **`caller()` always returned pkg
"main"** — broke any module capturing `scalar(caller)` at import to find the
target pkg (Exporter, CMM's before/after/around). Fixed via dynamic
`*pcl-current-package*` (orig-case; PCL upcases single-seg pkg names so case is
otherwise lost) set at each `package` stmt by `p-set-current-package`, +
`*pcl-caller-pkg-stack*` pushed per `p-sub` entry, read by `p-caller`. (2)
**method dispatch on a class-name STRING in a scalar** (`my $c="Foo"; $c->m`/
`can`/`isa`) dispatched against "main" — boxed string invocant fell through
`p-get-class`→nil; only literal `"Foo"->m` worked. New shared helper
`%pcl-invocant-class` (p-method-call/p-can/p-isa). **DO NOT fold into
p-get-class** — that must keep returning nil for a boxed plain string or the
`bless` guard's `p-find-overload` treats the class-name string as itself
overloaded → breaks subclass-inherited `""` overload (regression caught). (3)
**`||= &&= //=` broken for ALL hash/array element places** (not just nested):
macros box-set the READ result, but absent key reads as undef (no stored box) →
nothing stored; nested unviv'd. Fixed by delegating the store to `p-setf`.
Gate 3253, sweep 18044/786/69, sweep-diff 0/0. Tests misc-fixes-01.t 55→64.

**CMM STILL BLOCKED** (documented hard limit): builds wrapped method via
`eval "sub $name { ...\$before...\$wrapped... }"` closing over the installer's
LEXICALS — PCL's subprocess string-eval can't capture outer lexicals
(`FOO::$BEFORE` unbound). Same family as the closure/eval-lexical limitation.

**Follow-on FIXED (same session 236): runtime `@ISA` → can()/isa()**. `p-can`
read `sb-mop:class-precedence-list` directly → `%CLASS-PRECEDENCE-LIST unbound`
crash for a class whose `@ISA` was set at runtime (never finalized); and CPL
wouldn't reflect runtime `@ISA` anyway (PCL emits CLOS classes w/ EMPTY supers;
all inheritance lives in `@ISA`). New shared **`%pcl-isa-ancestry`** (DFS `@ISA`
linearize + cycle/diamond guard + implicit UNIVERSAL = the walk `p-method-call`
already prefers); `p-can` resolves through it (own-package methods only), `p-isa`
checks membership (dropped its handler-case'd CLOS-CPL path). Diamond + negatives
verified vs perl. Gate 3256, sweep 18044/786/69, diff 0/0. Doc:
`docs/caller-implementation.md` (caller pkg) written this session.

**Still NOT fixed (queued):** `caller(0)` in list ctx returns only the package
(wantarray-propagation gap on `(caller(0))[N]`; pre-existing; file/line are
documented non-support).

**Session 236b — JSON::PP now round-trips** (committed). 3 general bugs:
(1) **`overload::import`/`overload::unimport` undefined** (`OVERLOAD::PL-IMPORT/
PL-UNIMPORT`) — JSON::PP::Boolean calls them directly. Implemented in OVERLOAD
pkg; both act on the CALLER pkg (`*pcl-current-package*`) after shifting the
'overload' class arg (like real overload.pm); import→p-register-overloads,
unimport→remhash. (2) **bareword constant as ARRAY subscript** `$self->[P_FOO]`
was autoquoted to "P_FOO" → crash. Perl autoquotes only HASH `{bareword}`;
`[bareword]` is numeric → a KNOWN sub/constant is called, unknown bareword =
string→0. New `_bareword_subscript_autoquotes` (Pl/PExpr.pm) checks env
`has_prototype`/`declared_subs`; two-pass parse_file handles forward-ref +
imported constants. (3) **module load leaked its last `package`** into caller —
`p-load-module-cached` now rebinds `*pcl-current-package*` around the load
(name-map persists). Doc: `docs/caller-implementation.md` updated.
**JSON::PP remaining gaps (NOT fixed):** numbers encode as quoted strings
(JSON::PP `_looks_like_number` reads `B::svref_2object->FLAGS` SVp_IOK/NOK/POK —
the `B` XS module's SV-flags, an SV-flags limitation like dualvar/utf8); hash key
ordering (only matters with `canonical`). Gate 3260, sweep 18045/785/69.

**Session 236d — `builtin::` core namespace implemented** (commit 4f02f9c).
The Perl 5.36+ `builtin` pragma functions are always available without `use`, so
a generated `builtin::NAME(...)` compiles to a direct `BUILTIN::PL-NAME` form that
must resolve at load → they live in `cl/pcl-runtime.lisp` (new `BUILTIN` package,
registered in `*p-declared-subs*` so `defined &builtin::NAME` is true). Implemented
flag-free subset: true/false/is_bool, weaken/unweaken/is_weak, blessed/refaddr/
reftype, ceil/floor, trim, stringify, created_as_number/created_as_string.
**Faithfulness boundary** (same SV-flags limit as JSON::PP): `is_bool` returns
false (safe — a bool is still an ordinary scalar); `created_as_*` proxy via box
value type (number-held⇒numeric, string-held⇒string) — best-effort, left
defined so Sub::Quote etc. don't special-case. blessed/refaddr/reftype reuse
p-ref/p-reftype/object-address, return undef (not "") for non-refs. Gate
misc-fixes-01.t 77→80, sweep 18045/785/69 held, diff 0/0.

**Session 236d — Sub::Quote now works** (commit 3a5a576; installed, pure-Perl,
core **Moo** dep). `quotify` worked immediately (validated the builtin::
created_as_* path). `quote_sub`/`qsub` crashed: PCL's execution of Sub::Quote's
`capture_unroll` (qq with `${1}` and `${\quotify $_}`) produced garbage. Root
cause = string interp only handled `${identifier}`; the **`${ EXPR }`
block-deref forms were ALL broken** (`${$ref}`→SCALAR(0x..), `${\ EXPR}`→
REF(0x..), `${N}`→literal digit). Fixed `parse_braced_expression`
(Pl/PExpr/StringInterpolation.pm): route complex content through the full
`${...}` scalar-deref (p-cast-$) + numbered-capture branch. quote_sub/qsub/
quotify + deferred Sub::Defer path all work. **Exposed latent grep/map/sort
[perl #78194] bug** ($_/$a/$b bound to raw literal-scalar elements → \$_
re-boxed each access) → fixed by boxing raw elements per-iteration in p-grep/
p-map/p-sort. concat2.t RT#132385 newly passes; 69 fully-passing held.
NEXT no-XS targets toward Moo: Sub::Defer (✓ via Sub::Quote), then Moo itself
(installed, pure-Perl) — expect eval-lexical-capture walls (CMM-style).

**Session 237b — Moo survey: 4 general bugs fixed; Moo blocked on eval-capture.**
Resumed toward Moo. Recovered an uncommitted `print STDERR` filehandle fix (the
"lost contact" work), validated + committed. Four general bugs, each w/ a
misc-fixes-01.t regression test (83→89):
1. **`print STDERR`→stdout** (5971cf3) — `p-get-filehandle-stream` looked up FH
   symbols by `eq` only; user-pkg STDERR ≠ `:pcl` STDERR → miss → stdout. Added
   by-name fallback to the canonical `:pcl` symbol.
2. **Multi-colon `our $var` defvar** (ed81095) — `our $x` in a sub in a multi-seg
   pkg emitted `(defvar Foo::Bar::$x …)` → reader "too many colons" crash. Route
   prefix through `_cl_pkg_designator` (→`|Foo::Bar|`). Hit via Moo::sification.
3. **Glob-REF assign/slot** (404e2b5) — Moo `_install_coderef`: `\*{$name}` then
   `*{$glob}=$code`/`if(*{$glob}{CODE})`. `p-glob-assign-dynamic`/
   `p-dynamic-typeglob` stringified the glob ref (→GLOB(0x..)) not unboxing the
   `p-typeglob`; `*{EXPR}{SLOT}` was a parse error. Fixed both (shared
   `%p-glob-assign-slots`; new PExpr detection + `_block_is_glob_slot`).
Gate 91/3278, sweep 18047/784/69 held (+1), sweep-diff 0/0 each.

**Moo BLOCKED — chain mapped (next-session map):**
- After fix #2, `use Moo` loads Moo::sification+Moo.pm. But **has/with/extends
  never install** because **`p-use` does NOT call a module's custom `import`**
  (`cl/pcl-runtime.lisp` ~7657) — only `p-import-exports` (copies `@EXPORT`). PCL
  fakes Exporter via `@EXPORT`; custom-import modules (Moo/Moose/namespace::clean)
  run nothing. **Real general gap.**
- Wiring custom-import cascades: Moo's import calls `strict->import;
  warnings->import;` as real method calls → `STRICT::PL-IMPORT` (real strict.pm)
  → **`STRICT::$^H` unbound**.
- Beyond that: `_install_subs`→`_gen_subs`→`Method::Generate::{Constructor,
  Accessor}` build accessors via **Sub::Quote/eval closing over installer
  lexicals** = the **eval-lexical-capture wall** (CMM family). Full Moo needs that
  solved first.
- Did NOT wire custom-import (would turn `use Moo` from silent no-op into a `$^H`
  crash — worse). **Next:** (A) custom-import in p-use *gated* behind no-op
  `strict/warnings/feature ->import` + bound `$^H`, re-survey; (B) eval-lexical-
  capture (recurring Moo/CMM wall); (C) lighter modules.

**Still open (lower priority):** `defined(&glob_installed_sub)` reports undef
though callable (cosmetic). cpanm works (`--local-lib`); dists w/ tests via curl.
