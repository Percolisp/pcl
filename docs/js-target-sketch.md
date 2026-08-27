# A JavaScript target for the PCL IR — exploration sketch

**Status:** fun/exploratory, NOT scheduled (USER, s448: "something we could
look into for fun").  Nothing here is a commitment; it exists so the thinking
is not re-derived.  The normative input is `docs/ir-spec.md` — this sketch is
an application of it, and where they disagree, ir-spec wins.
**Task:** #622.

## 1. The compiler itself: small

The generated CL is a tree with a **closed vocabulary** — a few dozen `p-*`
special forms and op families with documented family rules (ir-spec §10).  A
JS backend is an S-expression reader (trivial) plus a tree-walker mapping each
form to a JS control structure or a runtime call.  Almost all operators become
runtime calls (`rt.add(a,b)`, `rt.concat(...)`) because Perl coercion cannot
ride on JS's `+` any more than it could on CL's — which is already how the IR
works, so nothing new.  The macro layer (`p-foreach`, `p-while`, `p-if`)
becomes plain JS templates.  The translator is the *small* part of the
project, precisely because the front-end already lowered Perl into this shape.
The Perl front-end (`Pl/**`) is reused as-is; a JS target is a second backend,
not a second compiler.

## 2. Scoping: mostly solved before the backend sees it

All the genuinely hard Perl scoping work — the `$x__lex__N` rename families,
my-shadowing, file-lexical promotion, state cells, capture analysis, the
sub/eval partition — happens in the front-end, and the IR arrives with it
done (ir-spec §2b).  The mapping:

- **Lexicals** → `let x = new Box(...)`.  JS closures capture by reference
  exactly like CL `let`, so closures just work; JS's per-iteration `let`
  binding in loop heads even matches fresh-binding-per-iteration natively.
- **Package variables** → a namespace registry (`pkg("main").scalar("x")`
  holding boxes).  The CL-specific symbol machinery — `:invert` case
  inversion, pipe-quoting, read-time `in-package` — **vanishes entirely**:
  names become plain string keys.  A whole class of CL-target bugs (the
  #418/#498/#600 family) does not exist in JS.
- **The one real loss: dynamic binding.**  CL gives `local` and `*wantarray*`
  free via special variables; JS has nothing.  Both compile away: `local`
  becomes save/restore in `try`/`finally` (correct across `die`, since
  finally runs on unwind), and the context protocol becomes an explicit
  context stack or hidden argument.  The IR already brackets both explicitly
  (`p-list-ctx` etc.; a `local` target is a structured item since s446i), so
  the backend has what it needs.
- **Non-local exits fit better than CL**: `die`/`eval` → native exceptions;
  `last`/`next` with labels → native labeled `break`/`continue`.  Only
  `redo` and `goto` need transforms, and the IR already restricts goto
  shapes.

## 2b. Why boxes survive the port (the "references in Perl" question)

Perl is unusual among high-level scripting languages in having first-class
references **to variables**: `\$x` (and `foreach` topic aliasing, `@_`
aliasing, `local`, tie) makes a scalar an *aliasable cell*, and JS — like CL,
unlike Perl — has no way to take a reference to a variable.  So the cell must
be reified: **the box model ports unchanged** (ir-spec §2.2).  JS closures
capturing by reference cover the closure half natively; the box is for the
half closures cannot do — `\$x` stored in a data structure, aliased across
scopes, or localized.

But the IR already carries the escape analysis that makes this cheap: the
freeze-licensed **raw-slot facts** (`PCL_OPT` raw-slot / raw-numeric, ir-spec
§2.2) prove when no reference or alias can ever exist, and those variables
emit unboxed.  A JS backend inherits the verdicts for free: boxes only where
Perl's semantics genuinely demand a cell, plain `let x` where the compiler
already proved they don't.

## 3. Porting the runtime: the bulk, in three tiers

**Mechanical (weeks):** the core value layer — boxes, undef, the coercion
tables (§3), truthiness, the op families, arrays/hashes (JS arrays and `Map`
fit well; references are object identity — *easier* than CL); the
wantarray/calling convention; the phase model (BEGIN ordering is emit-order
in JS — no `eval-when` gymnastics).  A dividend: the three checked-in
transpiled artifacts (`pack`/`mro`/`warnings`) are themselves IR — they
compile through the new backend for free.  Writing `pack-impl` in Perl pays
off a second time.

**Real work:** IO and processes.  Node has fds, `child_process`; even the
s448 std-descriptor semantics (dup2, close-frees-fd) are expressible;
fork-pipe opens and `%SIG` need care; a browser target stubs nearly all of
it.  Tie/overload/magic port as hook points — the runtime already routes
them through a small number of hooks.  Quiet nuisances: numbers (JS doubles
vs perl's IV/NV duality; BigInt in the corners) and byte-strings vs
char-strings (JS strings are UTF-16 code units).

**The two genuinely hard problems:**
1. **Regex** — the Perl-regex-vs-JS-RegExp semantic gap (deliberately not
   detailed here; it is the big one).
2. **String eval's architecture** — `p-eval` calls back into the compiler at
   runtime.  On Node: spawn the Perl-based `pl2cl`-equivalent as a service,
   exactly as the CL runtime does.  In a browser: the compiler itself would
   need porting or WASM.  A product decision, not a porting task.  (The
   HARD REQUIREMENT stands in any target: `eval $str` must work.)

XS drops from a *pure* JS port — but see §5.

## 4. Sizing verdict (compiler + runtime)

Front-end reused as-is; translator small; runtime port dominated by IO +
regex + eval-architecture; scoping mostly a non-event; the CL-target pain
class (case inversion, read-time in-package, paren discipline) replaced by a
smaller JS-target pain class (no dynamic binding — compiled away; the number
model; byte-vs-char strings).  As a Node-only target with
eval-via-subprocess: a plausible medium project.  The browser is where it
gets philosophical.

## 5. pclxs on JavaScript

pclxs is the **most portable** piece, because its design already solved the
cross-language problem: real compiled XS modules (unmodified C) link against
`libperlshim`, which is host-INDEPENDENT, and everything host-specific goes
through a narrow, versioned C **vtable** in capability groups (magic, io, …),
pinned by ABI.  A JS host implements the same vtable; libperlshim and the
compiled dists do not change.  Crucially, **"a host is done" is already
defined host-independently**: the conformance corpus (`tools/pcl-conform`,
real perl as oracle) transfers verbatim as the JS host's acceptance test.

**Node host:** an N-API trampoline addon — vtable function pointers calling
into JS, SV handles as entries in a handle registry mapping to boxes; a JS
twin of `cl/pcl-xs.lisp`; `pcl-xs-install` retargeted (modest build-system
work).  With the CL host as reference implementation: a focused medium
project, *easier than the CL host was the first time*.

**The three hard parts, ranked:**
1. **Lifetime/GC coordination** — C holds SV references to JS objects;
   perl's refcounts must drive a retain/release handle table so the JS GC
   does not collect what C still holds.  Same class as the CL side's
   `xs-ref-target` referent-identity bug, with less to hold on to.  The
   DESTROY leak (`docs/xs-abi5-and-destroy.md`) gets MORE pressing in a
   long-lived Node process.
2. **Crossing cost** — N-API calls are much dearer than SBCL alien
   callbacks; the vtable design already caches per-class to avoid crossings
   (`pclxs_has_destroy`) and a JS host leans harder on that.
3. **Reentrancy** — `call_sv`: C → shim → vtable → JS running IR code →
   possibly back into C.  Node handles it; it is where the subtle bugs live.

**Browser:** no `dlopen` — compile each dist + libperlshim to WASM
(Emscripten), the vtable becoming WASM imports (cheap crossings).  Pure-C
compute modules (digests, parsers) plausible; anything touching fds, fork,
threads, signals never works there.  The bulk is per-dist build retargeting
to emcc — plumbing, not design.  Hard ceiling on which modules can ever run.

## 6. If a spike ever happens

Smallest honest first step: an S-expression reader + a walker covering the
§10 op families used by `Pl/t/transpile-test-01.t`'s programs, a runtime of
boxes + coercion tables only, Node-only, no IO beyond print, no regex, no
eval.  Acceptance = the same oracle discipline as everything else in this
project: run the program under perl and under node, byte-compare.  The
existing gate files are the ready-made corpus, in difficulty order.
