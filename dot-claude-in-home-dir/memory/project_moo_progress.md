---
name: ""
metadata: 
  node_type: memory
  originSessionId: a2e18eb2-c4ef-4664-b55c-f871695cc0a3
---

# Moo progress — session 253c (2026-06-15) — METHOD MODIFIERS WORK (around too)

**Moo is now broadly working.** before/after/around (single + STACKED) verified
vs perl 5.40. Commit 2de1d33. Four fixes:
1. `$$ref->()` precedence (Pl/PExpr.pm Case-2 arrow): leading scalar Cast binds
   with the ref → `(${$r})->()` not `${$r->()}`. General bug, not eval.
2. `return` in string eval returns from the EVAL (p-eval `catch :p-return`).
3. assignment to a non-lvalue sub call (`&sub=x`/`foo()=x`/`$cref->()=x`) is a
   PCL:-prefixed (propagating) transpile error; substr/pos/vec still allowed. In
   eval-string mode the pl2cl server reports it → eval returns undef. This makes
   Class::Method::Modifiers `_sub_attrs` (`eval 'return 1; &_sub=1'`) feature-probe
   work (it relies on Perl's compile error). KEY LEARNING: the parser swallows
   per-statement codegen `die`s into `;; PARSE ERROR` comments UNLESS the message
   starts with `PCL:` (Parser.pm ~6722 re-throws those). The pl2cl --server wraps
   parse_code in eval{} → returns status=err → p-transpile-string signals →
   p-eval handler-case → undef+$@.
4. `\$ref->{k}`/`\$ref->[i]` are LIVE refs: new `p-gethash-deref-box`/
   `p-aref-deref-box` return the live slot box in l-value context (arrow-deref
   used to snapshot; direct `\$h{k}` was already live). Stacked `around` needs
   `\$cache->{wrapped}` to track reassignment.
Tests: misc-fixes-02.t +3, eval-named-sub-01.t +2.

**VERIFIED WORKING:** new; ro/rw; defaults (scalar/coderef/ref); required; lazy
+builder/_build_*; BUILD; predicate; clearer; builder; trigger; isa
coderef-constraint; extends/multi-level inheritance; isa(); roles
(with/does/requires/multi-method/role-attributes); before/after/around
(single+stacked); handles delegation to an OBJECT.

**REMAINING for Moo (mostly NOT Moo-specific):**
- **Package/sub names colliding with CL builtins** (`package Car`→CL `car`;
  `has log`→`log`; `list`, etc.) → SYMBOL-PACKAGE-LOCKED-ERROR. General mangling
  bug (separate from s252 case-collision work). Likely the highest-impact
  remaining gap for real modules.
- Native array/hash delegation (`handles=>{m=>'push'}` on an arrayref) needs
  Sub::HandlesVia — plain Moo + perl also die on it (not a PCL gap).
- Module compile-load double-exec workaround (`*pcl-cache-fasl* nil`) — perf only,
  not correctness (see [[project_module_compile_load_double_exec]]).
- DESTROY/DEMOLISH via GC — permanent not-supported.
- Type::Tiny / coercions, BUILDARGS edge cases — untested.

# Moo progress — session 253b (2026-06-14) — ROLES WORK (ordering invariant fixed)

**Moo ROLES NOW WORK** (with/does/requires/multi-method composition, verified vs
perl 5.40). Repros /tmp/role2.pl, /tmp/moo_role.pl green. The Bug D sub-hoisting
wall (below) is FIXED — commit 00eb171, gate green 94/3436.

**THE PERMANENT ORDERING FIX (docs/declaration-ordering-fix-plan.md):**
Root cause = top-level sub bodies routed to the `declarations` bucket (assembled
before the `definitions` bucket holding use/BEGIN), so every sub ran before every
use/BEGIN AND the p-declare-sub stub made subs fboundp to introspection.
Modules introspecting the package's subs at use-time (Moo::Role make_role) saw
subs Perl hadn't compiled yet. FIX (3 parts):
1. `Pl/Parser.pm`: top-level sub bodies → `definitions` bucket in SOURCE ORDER
   with use/BEGIN. **Nested named subs → `declarations`** (must be a DIFFERENT
   bucket than top-level subs, else they interleave inside the outer's parens and
   only exist once it runs — this was the transpile-test-05 #41/#43 regression).
2. `cl/pcl-runtime.lisp`: forward stubs invisible to introspection — `p-stash`
   (keys %Pkg::) and `p-can` (->can) skip a `:stub` symbol; real def flips it to
   `:defined`. `p-backslash-sub` returns a late-binding trampoline for a stub-only
   sym (so `\&foo` before `sub foo` reaches the real body).
3. Tests: `decl-ordering-02.t` (10, differential) locks the invariant;
   `decl-ordering-01.t` 5 structural assertions updated to the new (correct)
   source-order policy (defvar now precedes sub; END/sub interleave in source).

**THE INVARIANT:** within a package, reproduce Perl's two timelines — compile-time
stream (use/BEGIN/sub) in source order (each sees only earlier names), then
runtime stream. Only defvar proclamations + introspection-invisible stubs jump
ahead. Don't special-case modules; hold the invariant.

**METHOD MODIFIERS — half-fixed + planned (s253b).** before/after/around
(`/tmp/mod.pl`): `Class::Method::Modifiers::install_modifier` `eval`s a string
that installs a NAMED method closing over lexicals `$before`/`$after`/`$wrapped`.
Two PCL bugs found:
- **Bug B (in-package) FIXED — commit 699baff:** `p-eval` read the whole eval
  text in one `read-from-string` under one `*package*`, so `package X;` inside the
  eval was defeated and the named sub landed in `main`. Now reads/evals
  form-by-form (like `load`). Tests `Pl/t/eval-named-sub-01.t` (3 pass).
- **Bug A (free-var capture) DONE — commit 4870b7f:** AST-level scope-aware
  free-var detection (`_eval_free_vars_from_ppi` in Parser.pm) that DESCENDS into
  named-sub bodies + excludes `PPI::Token::Magic` specials by type (so `$.`/`$!`
  etc. aren't captured, and `local $.` in an outer scope still shows through);
  plus ExprToCL fix so an INTERPOLATED eval string (`eval "package $into;..."`)
  also gets the lexical-capture alist (it was falling through to the generic
  funcall path). **`before`/`after` modifiers WORK** end-to-end vs perl
  (`/tmp/mod.pl`). Tests `Pl/t/eval-named-sub-01.t` (6, no longer TODO).
- **`around` STILL BLOCKED — SEPARATE general parser bug (`$$ref->()`):** deref a
  scalar-ref-to-coderef then call is mis-associated — PCL emits
  `(p-cast-$ (p-funcall-ref $r))` = `${$r->()}` instead of `(${$r})->()`. Root in
  `Pl/PExpr.pm` arrow/cast precedence (~762–916): leading scalar Cast consumes
  `$r->()` as its operand. `before`/`after` limp past only because
  `p-funcall-ref` double-unboxes a ref-to-coderef. NOT eval-related; own focused
  session (PExpr precedence = high regression risk). Repros `/tmp/derefcall.pl`,
  `/tmp/around1.pl` (→50), `/tmp/around.pl` (→60). See docs/method-modifiers-plan.md
  STATUS section.
- **(historical) Bug A plan:** a named sub in the eval that
  refs an enclosing SUB-scoped `my` (a CL `let`, unlike top-level `my`=defvar which
  already works) must close over it. Detection must be SCOPE-AWARE — a regex over
  generated CL was tried & REVERTED (scope-blind, mishandles shadowing; user
  flagged it). **Plan: `docs/eval-free-vars-plan.md`** — route through
  `Pl/BlockAnalyzer.pm` (`analyze`→`outer_refs` = AST-level free vars, walks
  OpcodeTree not text), extend it to descend into NAMED subs (it currently skips
  them by design). Consumer plan: `docs/method-modifiers-plan.md`. Acceptance =
  3 TODO tests in eval-named-sub-01.t flip + Moo before/after/around end-to-end.
  SEPARATE from ordering; own session. Also: attr-named-after-builtin
(`has log=>`) collides — minor. Junk empty files `apply_roles_to_package`/`import`
appear during role runs (a method name leaks to a bareword filehandle somewhere) —
harmless, rm them; minor bug to chase later.

# Moo progress — session 253 (2026-06-14) — ROLES advanced 3 layers; blocked on sub-hoisting

**3 real bugs fixed (commit f4169aa, gate green 93/3410):**
1. **require must NOT import** — `p-require` was an alias for `p-use` so
   `require Foo` re-ran `Foo->import` into the *current* pkg. Moo's `with` does
   `require Moo::Role; Moo::Role->apply_roles_to_package`, so require re-imported
   Moo::Role into the consumer → fatal "Cannot import Moo::Role into a Moo class".
   `p-use` now takes `:do-import`; `p-require` passes nil.
2. **nested autoviv stores a REFERENCE not a bare aggregate (GENERAL bug).**
   `p-autoviv-gethash`/`-for-array`/`-aref-for-hash`/`-for-array` stored the raw
   new hash-table/vector into the parent slot. A later scalar copy
   (`my $x = $h{a}`) then hit box-set's %hash/@arr-in-scalar rule and collapsed
   to the key/elem COUNT. Now `(make-p-box …)`, matching explicit `{}`/`[]`.
   Found via `$Role::Tiny::INFO{Greet}` coming back as `2` (=key count).
   Tests: `Pl/t/autoviv-01.t` +16 differential-vs-perl.
3. **BEGIN sets `*pcl-current-package*`** (Pl/Parser.pm `_process_scheduled_block`).
   BEGIN runs in the definitions bucket, before the runtime `p-set-current-package`,
   so an explicit `Module->import` in a BEGIN (Moo's `require Moo; Moo->import;`
   bootstrap in Method::Generate::Constructor) resolved `caller()` wrong and never
   installed has/new. Emit `p-set-current-package` as first stmt inside each BEGIN.

**ROLES NOW BLOCKED ON SUB-HOISTING (Bug D, the real wall).** `with 'Greet'`
loads + applies the role, but Greet's own `sub hello` is never composed into the
consumer. ROOT CAUSE (white-box confirmed): PCL routes ALL named subs to the
`declarations` bucket (`Pl/Parser.pm:5105`), assembled BEFORE the `definitions`
bucket holding use/BEGIN. `p-declare-sub` (cl/pcl-runtime.lisp:376) installs a
**stub defun** so the sub is `fboundp` before `use Moo::Role` runs. So
`make_role`'s non_methods snapshot (taken at use-time) WRONGLY includes `hello`
→ `_concrete_methods_of` = all_subs − non_methods excludes it → empty method set.
Verified: `$Role::Tiny::INFO{Greet}{non_methods}` contains `hello` (should not).
Perl only lets a BEGIN see subs defined EARLIER in source; PCL's blanket
"all subs before all BEGINs" over-approximates. FIX = declaration-ordering
project (route the real `p-sub` def to definitions bucket in source order AND
make the forward stub not count as `exists &sub` for stash enumeration — the
stub-fbound part is the subtle bit). Own session, careful gate. Repros:
/tmp/moo_role.pl, /tmp/role_compose.pl, /tmp/nm.pl.

# Moo progress — session 251 (2026-06-13) — SUBCLASS FIXED + feature matrix

**SUBCLASS EMPTY-ATTRS FIXED.** Root cause = module compile-file+load
double-execution (NOT coderef identity — s249/s250 theory superseded; see
[[project_module_compile_load_double_exec]]). Workaround: `*pcl-cache-fasl*`
default flipped to **nil** (load modules as source, single-pass). Gate GREEN
93/3392. Proper FASL-preserving fix deferred → `docs/module-double-exec-bug.md`
(DO NEXT SESSION).

**Moo feature matrix (verified vs perl 5.40):**
- WORKS: `new`; `ro`/`rw` accessors (scalar AND ref defaults `sub{[]}`/`sub{{}}`);
  single + **3-deep inheritance**; `isa()`; `required`; `lazy`+`_build_*` (incl.
  **subclass override**); coderef defaults; **`BUILD`**. Repros `/tmp/moo_t1.pl`,
  `/tmp/moo_sub.pl`, `/tmp/moo_probe.pl` all match perl.
- **BROKEN (next Moo targets) — ROOT CAUSES FOUND (s251), both DEEP:**
  1. **Method modifiers** `around`/`before`/`after` → `P::$WRAPPED is unbound`
     (`/tmp/moo_a.pl`). ROOT: Class::Method::Modifiers does
     `eval "package $into; sub $name {... \$\$wrapped->(@_) ...}"` — a RUNTIME-
     built string that defines a **NAMED sub** referencing the eval's captured
     lexical `$wrapped` (= `\$cache->{wrapped}`). PCL installs named subs at
     PACKAGE level (not as closures), so inside the eval'd `sub greet` the
     `$wrapped` ref resolves to unbound package var `P::$WRAPPED` instead of the
     s250-captured lexical. FIX = named-sub-inside-string-eval must close over
     the eval-thunk params (extends s248b named-sub-closure + s250 eval-capture;
     HARD). `before`/`after` likely same path.
  2. **Roles** `with`/`does`: progressed THREE layers (s251, none fully working
     yet) — `/tmp/moo_b.pl`:
     (a) parse hard-crash FIXED (`exit 0`→`die`, commit d957056) → transpiles.
     (b) Role::Tiny `_non_methods` `(P-GETHASH 2 …)` was a SYMPTOM of (c): the
         real cause was `caller` after `goto &Role::Tiny::import` returning
         "Moo::Role" (Role::Tiny set up the WRONG pkg as the role).
     (c) **`goto &sub` caller-frame FIXED (commit ca1cdb3)** — p-goto-sub now
         pops the goto-ing frame + restores `*pcl-current-package*` to its
         caller, AND evaluates the target expr BEFORE the pop (Exporter
         `goto &{as_heavy()}` needs `(caller(1))[3]` off the un-popped stack;
         else nil coderef → regressed `use Config`). Roles now load past
         Role::Tiny + Config.
     (d) NOW blocked at **`Cannot import Moo::Role into a Moo class`** —
         Moo::Role::import guard `$Moo::MAKERS{$target}{is_class}` truthy when it
         shouldn't be. NEXT: trace `$target`/caller + MAKERS state at the
         Moo::Role::import in the `with 'R'` / role-load path. Was about to test
         `package R; use Moo::Role; sub hi{}` ALONE (no consumer) when session
         ended — start there.
  3. **Attr named after a builtin collides** — `has log=>...` → `$o->log` calls
     the `log` builtin; rename fixes. General method/builtin-name collision,
     lower priority.

---

# Moo progress — session 250 (2026-06-13) — subclass empty-attrs LOCALIZED

eval-capture (s250) did NOT fix Moo subclass (it's a separate bug); s249 loop
fix holds — `Dog->new` (extends Animal) now COMPLETES but builds an EMPTY object
(name/breed UNDEF). Localized the root cause precisely with white-box probing:

**Symptom chain:** `Dog->can('new')` returns the INHERITED `Animal::new`
(`defined &Dog::new`=NO) → Dog's own ctor never installed → `Dog->new` runs
`Animal::new`, hits `$class ne "Animal"` + `MAKERS{Dog}{constructor}` truthy →
`return $invoker->SUPER::new(@_)` → `Moo::Object::new` → bare `bless {}` → empty.

**Why Dog::new isn't installed:** `MGC::install_delayed` does
`defer_sub "${package}::new"` but **`$self->{package}` is "" (empty)** for Dog
(instrumented copy of Method/Generate/Constructor.pm: `pkg=[] self-keys=[]`).
So `defer_sub "::new"` installs nothing useful.

**Why package="":** the maker object came back EMPTY — `MGC->new(%construct_opts)`
for Dog stored NONE of its args (`selfkeys={}`), so package is undef→"".

**RULED OUT (advances past s248b guesses):**
- arg-flatten / `{@_}` k/v pairing shift: feeding the REAL construction_string
  (+ blessed non-empty hashref values) through a plain `sub{my%a=@_}` preserves
  all keys. NOT a flatten bug.
- re-entrancy ("call 3 from inside eval'd Animal::new"): the empty install fires
  at SETUP (during Dog's `has breed`), BEFORE any `Dog->new`. Not re-entrant.
- wrong invoker/opts: instrumented Moo.pm `_constructor_maker_for` — at the call
  site target=Dog, con=MGC, **invoker=Method::Generate::Constructor**, optpkg=Dog,
  optkeys include construction_string — ALL correct + identical to perl.
- a clean Moo class with the SAME 4 attrs (package/accessor_generator/
  subconstructor_handler/construction_string) stores them fine in PCL.

**NARROWED TO:** MGC's own BOOTSTRAP-generated `new` (the chicken-egg one with the
`if($class ne "Method::Generate::Constructor"){...subconstructor...}` branch +
inline `{@_}`, see SUB_QUOTE_DEBUG dump lines ~9-52) returns an EMPTY object for
the Dog call (3rd call, the one WITH construction_string) although invoker IS
"Method::Generate::Constructor" (so the `ne` branch should be FALSE and it should
store). Animal's call (no construction_string) stores fine. Strongly suggests the
memory's "2nd bootstrap-body function (0x4)" — dispatch hitting a WRONG/empty
MGC::new body for the 3rd call. **NEXT:** instrument INSIDE MGC::new (override
BUILDARGS won't work — bootstrap new inlines `{@_}`; instead force a single
known MGC::new body / dump which coderef Dog's `ref($con)->new` actually invokes
vs the bootstrap body's address). Tooling: copy real Moo.pm + Method/Generate/
Constructor.pm into lib/ + `chmod u+w` + warns, REMOVE after (they shadow
site_perl). `SUB_QUOTE_DEBUG=1 ./runpcl` dumps generated subs → /tmp/sqdump.txt.
Repro: /tmp/moo_probe.pl (Animal+Dog, prints name/breed). See
[[project_coderef_identity_blocker]].

---

# Moo progress — session 249 (2026-06-13)

**s249 update — subclass loop FIXED, attrs still empty (3 fixes landed):**
1. `lib/Sub/Util.pm` shim (set_subname/set_prototype/subname/prototype) →
   `Sub::Defer::_CAN_SUBNAME=1` → defer_sub uses pure-Perl CLOSURE branch
   (not string-eval, which can't capture lexicals in PCL).
2. Multi-seg `defined &Pkg::sub`: `p-sub-defined`/`-exists`/`p-undef-sub` →
   `%pcl-find-package` (was `find-package (string-upcase)`).
3. **Stable coderef identity** (the big one): `object-address` now uses a
   weak `eq` id table, not the GC-movable raw pointer. Killed the infinite
   `goto` self-loop in Sub::Defer's coderef-keyed %DEFERRED. See
   [[project_coderef_identity_blocker]].
**STILL FAILING:** `Dog->new` (extends Animal) → empty attrs. Construction-time
`MGC->new` for Dog returns empty (madepkg=UNDEF) because `(ref $con)->new`
falls through to `Moo::Object::new` instead of MGC::new (bootstrap-self-delete
+ install_delayed reinstall on multi-seg pkg). NEXT investigation target.

---
# Moo progress — session 248b (2026-06-13)

**WORKS (single class):** new/ro/rw/accessors, scalar defaults, **coderef
defaults** (`default => sub {...}`), lazy + `_build_*`, required.
moo8.pl GREEN.

**s248b fixes (commit pending gate):**
1. Named sub in a block closes over block `my` lexicals — removed the
   named-sub skip in `_vars_referenced_in_closures` so the `__lex__N`
   rename fires (was: let-of-defvar shadowing; the eval'd Sub::Quote shape
   `{ my $default_for_b = ...; sub new { $default_for_b->($new) } }`).
2. `p-super-call` now uses `%pcl-find-package` (3 sites) — raw
   `(string-upcase ...)` lookups missed case-preserved `|Moo::Object|`
   pkgs → "No SUPER::new found from Animal".
Tests misc-fixes-02.t #28/#29.

**NEXT WALL — subclass bootstrap** (`extends 'Animal'; Dog->new` → empty
hash): localized via shadow-instrumented lib/Moo.pm + lib/MGC.pm (removed
after; `chmod u+w` the copies — cp preserves 444). `%construct_opts` correct
at the `MGC->new(%construct_opts)` call (package=Dog), but the maker comes
back with **selfkeys={}** — generated MGC::new stored NOTHING → install_
delayed gets package="" → defer_sub "::new" → `$package ||= caller` installs
the ctor as MGC::new (not Dog::new) → re-dispatch loops to Animal::new →
SUPER::new → bare bless. Calls 1 (MGC self) + 2 (Animal, at `has` time)
store fine; only call 3 (Dog, re-entrant from inside the EVAL'd Animal::new)
fails. **Hypothesis:** the call-3-only `construction_string` opt
(`$con->construction_string`) arrives as a raw vector/hash → p-flatten-args
spreads it mid-arglist → `{@_}` pairing shifts → every `exists $args->{...}`
fails. **NEXT PROBE:** dump ref/value of `$con->construction_string`; or
plain repro `K->new(a=>1, weird=>RAWVEC, b=>2)` through an eval'd ctor.

**Debug tools that worked:** `SUB_QUOTE_DEBUG=1 ./runpcl x.pl` dumps all
generated subs (host pl2cl's own Moo pollutes the dump — grep your pkg);
probe ladder /tmp/moo13–18.pl; `defer_info(Dog->can('new'))` tells which
name the deferred ctor was installed under.

**Known cosmetic gap:** `defined &Pkg::name` is a false NEGATIVE for subs
installed via glob CODE-slot side table (dispatch sees them, `defined &`
doesn't) — inverse of the s247 stash-delete fix. Didn't matter for Moo flow
(it uses ->can), but worth fixing eventually.

**Git:** `stash@{0} = "pack-P WIP (paused)"` still exists — do NOT pop casually.

Related: [[project_cpan_debugging_horizon]], docs/cpan-module-blockers.md (top).
