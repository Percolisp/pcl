# A JavaScript target for the PCL IR — the plan

**Status:** PLAN (s453, Fable, 2026-08-30).  Continues (does not replace)
`docs/js-target-sketch.md` — the sketch is the architecture argument, this is
the working plan.  Task **#622**.  Still **fun/parked** (USER s448): nothing
here is scheduled; do not start without the USER asking.
**Normative input:** `docs/ir-spec.md`.  Where this doc and ir-spec disagree,
ir-spec wins.

**Two audiences, two parts.**  Part I is for a person — a Perl programmer who
knows neither Common Lisp nor compiler internals (drafted by a simpler model
per the USER's instruction, reviewed for factual accuracy).  Part II is for
the AI session that one day implements this: exact IR-to-JS lowerings keyed
to real emitted IR, and the design decisions fixed in advance.  Read the part
written for you; they repeat on purpose.

## Part I — The JavaScript target, in plain terms

### What this is

PCL already turns Perl source into an intermediate representation, or IR: a
tree made of a few dozen building blocks, things like "print this", "loop
over this list", "assign this value to this lexical variable". The IR is a
very literal, unambiguous restatement of the Perl program — all the
questions about what a variable name refers to, whether a sub was declared
with a prototype, what a bareword means in context, are already answered by
the time the IR exists. Producing the IR is the hard 90% of writing a
Perl-to-anything compiler, because Perl's grammar and scoping rules are
famously irregular.

Today PCL does one thing with that IR: it writes out Common Lisp code and
runs it under SBCL (a Lisp implementation). "Transpile" is the word for
this — translating source code in one language into source code in
another, as text, rather than running it directly. `perl foo.pl` runs
foo.pl directly; PCL instead reads foo.pl and writes a `.lisp` file that
does the same thing when run.

This document is about a second thing PCL could do with the same IR: write
out JavaScript instead, run under Node. That is a "second backend" — the
front end (parsing Perl, building the IR) does not change; only the last
step, turning the IR into text in a target language, gets a second
implementation. Because the front end is reused untouched, a JavaScript
backend is mostly a translation exercise plus a support library (a
"runtime") in JavaScript that the generated code calls into — not a new
implementation of Perl.

### What a Perl variable becomes

Perl has a feature that most scripting languages do not: you can take a
reference to a variable itself, not just to its value. `\$x` gives a handle
that, if you change through it, changes `$x`. Several other Perl features
only make sense because a variable can be "pointed at" this way — a
`foreach` loop variable is not a copy of each array element, it *is* that
element, so assigning to it changes the array; `local` temporarily swaps a
variable's value and restores it later even if the code dies; `tie`
attaches custom code to reads and writes of a variable.

Neither JavaScript nor Common Lisp has a way to point at a plain variable —
`let x = 2; let y = x;` in JavaScript copies the value, and there's no
equivalent of `\$x`. PCL's existing Lisp backend works around this by never
storing a Perl scalar as a plain Lisp variable: each one is wrapped in a
small heap object called a "box", holding the current value. A reference to
the variable is then just a reference to its box — ordinary object identity
gives the aliasing Perl needs. This box model carries over to JavaScript
unchanged, because the same gap between the target language and Perl
exists there too:

```perl
my $x = 2;
my $ref = \$x;
$$ref = 5;
print $x;   # 5
```

```js
let $x = new Box(2);
let $ref = $x;       // the box itself, not its value
$ref.set(5);
print($x.get());     // 5
```

(This and the JS snippets below are simplified to show the shape, not
literal generated output.) Boxing every
scalar would be wasteful, and the compiler already avoids it where it can:
PCL runs an analysis proving many variables are never referenced or
aliased, and those compile straight to a plain `let x = 2` with no box, on
the Lisp side today. That analysis lives in the IR, not the Lisp backend,
so a JavaScript backend inherits it for free — it only pays for a box where
Perl's semantics genuinely require one.

Perl variables come in a handful of different classes, and each has its own
translation:

**`my` (lexical variables).** These become a JavaScript `let` holding a box
(or a plain value, per the above). JavaScript closures — a function that
remembers variables from the scope it was defined in — capture by
reference, the same way the IR expects, so nested subs translate naturally.
JavaScript also has a feature that matches Perl surprisingly well: a `let`
declared in a `for` loop header gets a fresh binding on every iteration,
which is exactly what a Perl `foreach my $i` loop variable needs when a
closure captures it:

```perl
my @subs;
foreach my $i (1..3) {
    push @subs, sub { $i };
}
print $subs[0]->();   # 1
```

```js
let subs = [];
for (let i of [1, 2, 3]) {
    subs.push(() => i);
}
print(subs[0]());   // 1
```

Each closure captures its own `i`, not a shared one — this works natively
in JavaScript, without PCL having to do anything special.

**`our` / package variables** (globals like `$main::x`). These live in a
registry: each Perl package is a lookup table from name to box, something
like `pkg("main").scalar("x")`. JavaScript is noticeably simpler than Lisp
here: Common Lisp variable names must become valid Lisp symbols, forcing a
whole apparatus of case-folding and escaping for names that don't look like
normal Lisp identifiers. In JavaScript, package variable names are just
plain strings used as lookup keys — that entire class of naming trouble
does not exist.

**`local`.** Not a new variable — it temporarily replaces the value of an
existing package variable, and restores the old value when the enclosing
block ends, even if the block exits through an exception. JavaScript has no
built-in feature like this (in Lisp terms, "dynamic binding"), so it
compiles to an explicit save-and-restore, using `try`/`finally` so the
restore always happens on the way out:

```perl
our $level = 0;
sub inner { print $level; }
sub outer {
    local $level = $level + 1;   # <-- the hard part in JS: it has no `local`
    inner();                     # prints the raised value...
}
outer();                         # ...and after outer() returns, $level is 0 again
```

```js
// our $level = 0 — a package variable lives in the registry, not in a JS let
pkg("main").scalar("level").set(0);

function inner() {
    print(pkg("main").scalar("level").get());
}

function outer() {
    // Hard part in JS: no dynamic binding — `local` becomes save + try/finally
    let saved = pkg("main").scalar("level").get();
    pkg("main").scalar("level").set(saved + 1);
    try {
        inner();                                   // sees 1
    } finally {
        pkg("main").scalar("level").set(saved);    // restored even if inner() dies
    }
}
```

(Simplified in one important way: what Perl really swaps is the variable's
*box*, not just its value — a reference taken before the `local` keeps
pointing at the old box, and sees the old value until the block ends.
Part II pins that down.)

**`state`.** Initializes once, on the declaration's first run, then keeps
its value between calls — unlike `my`, which gets a fresh variable every
call. This becomes a box living outside the function (at module scope)
plus a flag for "already initialized" — exactly what the IR already
expresses for `state`, so the translation is the same shape in JS.

**`foreach` aliasing.** As shown above, the loop variable (or `$_` when
none is named) is not a copy of the current element — writing to it writes
through to the array. The runtime handles this by iterating over element
boxes directly, rather than copying values out on each pass.

### Arrays, hashes and references

Perl arrays become a small runtime class wrapping a plain JavaScript array,
because Perl arrays support things JS arrays don't do natively: negative
indices count from the end (`$a[-1]`), assigning past the end auto-extends
with undefs, `$#a` gives the last valid index, and `splice` has its own
rules — the wrapper supplies all of that on top of a real JS array.

Perl hashes map naturally onto JavaScript's `Map`, keyed by string, since
Perl hash keys are always strings. One detail worth preserving is the
difference between a key existing with an undef value (`exists` true,
`defined` false) and no key at all (`exists` false) — `Map` handles this
fine, just implemented consistently.

References are where JavaScript is actually easier than Common Lisp. A
Perl reference to an array or hash can just be the array-wrapper or Map
object itself (with a small wrapper recording the reference's kind, so
`ref()` can report "ARRAY" or "HASH"). Taking a reference (`\@a`) costs
nothing — it's the same object — and dereferencing is a method call on it.

Two data-representation details are real work, not exotic edge cases:

- **Numbers.** Perl keeps integers and floats distinct internally, with
  full 64-bit integers. JS numbers are all IEEE doubles, losing precision
  above 2^53, so the corners need a fallback to JS's `BigInt` type.
- **Strings.** Perl strings are, by context, sequences of bytes or of
  characters; JS strings are UTF-16 code units. The two don't line up
  automatically, especially for non-ASCII text or raw bytes, so the runtime
  needs a careful, explicit mapping rather than treating a Perl string as
  "just a JS string".

### Context, calls and exceptions

Perl subs can behave differently depending on how they were called — a
function called where a list is expected can return something different
from the same function called where a single value is expected. This is
the `wantarray` feature. The IR already records, at every call site, which
context applies, so the backend only has to carry that fact through at run
time, as an explicit context stack (or a hidden extra call argument) — the
JS equivalent of the dynamic variable the Lisp runtime uses for it today.

`die` and `eval` — Perl's exception mechanism — map onto JavaScript's
`throw` and `try`/`catch` almost directly, arguably more naturally than
onto Lisp, which has no built-in exception model shaped quite like Perl's.
Loop controls translate cleanly too: `last`/`next`, including labeled forms
(`OUTER: while (...) { last OUTER; }`), become labeled `break`/`continue`.

One thing that does *not* map onto JavaScript's own operators is Perl's
arithmetic and string operators. `$a + $b` in Perl has to coerce oddly on
input like `"5 apples" + 1` (giving `6`, with a warning), which is not what
JavaScript's `+` does. So `+` compiles not to JavaScript's `+` but to a
runtime call implementing Perl's actual coercion, roughly
`rt.add($a, $b)` instead of `$a + $b`. This is not a new problem introduced
by targeting JavaScript — the existing Lisp backend already routes
operators through runtime calls for the same reason, since Lisp's own `+`
doesn't do what Perl's does either. Nothing is lost carrying this over.

### The honest hard parts

**Regular expressions.** Perl's regex engine and JavaScript's built-in
`RegExp` differ in a long list of corner cases — character classes, named
captures, lookaround, Unicode property matching, and more. Reconciling the
two is the single largest item of work in porting the runtime.  The shape
of the answer is now sketched (Part II §II.8 item 3): translate the common
subset to native JS `RegExp` (modern JS regex covers far more than its
reputation, and V8's engine is fast), fall back to PCRE2 — WASM build
first, native binding if measured necessary — for the perl constructs JS
cannot express, and keep the CL target's loud compile-time refusals for
the code-block features neither engine runs.  What remains open is the
translation rule table itself, not the architecture.

**String eval.** Perl's `eval "some code string"` compiles and runs Perl
source at run time, so the compiler itself has to be reachable while the
program is running, not just beforehand. On Node: PCL can spawn its
existing Perl-based transpiler as a subprocess and hand it the string to
compile, exactly as the current Lisp runtime does today. In a browser there
is no subprocess to spawn and no obvious way to invoke a separate compiler
process at all, so a browser target needs its own answer — a product
decision for later, not a technical detail to solve now. (`eval "$string"`
working correctly is a firm project requirement on any target, not an
optional feature.)

**IO and processes.** Reading and writing files, spawning subprocesses,
talking to file descriptors — Node has solid support for all of this. A
browser target would have to stub out almost this entire area, since
browsers don't expose that kind of access to a web page.

### What "done" looks like, and the first step

Running a transpiled program would look like `node program.js`, alongside a
single runtime support file (one `.mjs` module) the generated code imports.
Correctness is checked the way PCL already checks its Lisp output: run the
same Perl program under real perl and under the generated JavaScript, and
compare output byte for byte. PCL already has a large body of test programs
used this way for the Lisp backend; the same corpus would serve as the
acceptance test here.

If a first, small version were ever built, the smallest honest starting
point would be: a reader for the IR's textual form, a translator handling
only the handful of operations used by PCL's very simplest existing test
file, and a runtime implementing only boxes and the coercion tables — no
file IO beyond `print`, no regex, no `eval`. Deliberately narrow: it proves
the basic shape (IR in, working JavaScript out, byte-identical to perl)
before spending effort on the two hard problems above.

## Part II — normative mapping specification (for the implementing session)

**Rewritten s470bt as TABLES over the generated inventory.**  Part II used to
be four worked probe programs with their captured IR beside hand-written JS —
about 250 lines of example, keyed to one commit's emission, going stale on its
own.  Everything it taught is now DERIVABLE, because the IR describes itself:

| the question Part II used to answer by example | where the answer lives now |
|---|---|
| which ops exist, and what does each promise? | `docs/ir-op-inventory.md` — GENERATED from the runtime's export list plus each op's `Contract:` docstring tail; `docs/ir-op-inventory.tsv` is the machine form (ir-spec §10a) |
| which non-`p-*` host forms must I implement? | ir-spec §11b — MEASURED (`tools/ir-host-leak.pl`), 100 names, with a JS and a C column per group |
| what does THIS program need of me? | `pl2cl --manifest` — `USES` / `NEEDS` / `FACTS` per program (ir-spec §10b); `p-sub`'s `:needs` scopes it per sub |
| which variable is a plain `let` and which is a cell? | the IR says so: `p-let`'s CLASS and facts tail, `p-raw-params`' parameter classes (ir-spec §2b.2a, §5.1) |
| how do I parse it without a CL reader? | `pl2cl --emit-sexp` — the DATA form, five reader rules, a working reader in Perl and JS (ir-spec §12b) |
| am I right? | `tools/ir-conform --backend ./my-backend` — 345 cases with perl 5.40.3 as the oracle (`ir-conform/README.md`) |

The four probe walkthroughs are in git history if the narrative helps:
`git show 981480f4~1:docs/js-target-plan.md`.  The JS on the right of every
table below is normative in SHAPE, not in letter: runtime entry-point names
may differ, the division of labour may not.

### II.0 Architecture and inputs

| input | what it is | contract |
|---|---|---|
| the emitted program | `pl2cl --emit-sexp FILE` — one top-level form per line, 7-bit, every symbol pipe-quoted | ir-spec §12b |
| the op vocabulary | 698 `p-*` / `%p-*` names in 54 families | ir-spec §10, §10a; `docs/ir-op-inventory.md` |
| the host vocabulary | 100 CL kernel names (68 constructs + the ignorable/syntax sets) | ir-spec §11b |
| what the program demands | `pl2cl --manifest` JSON: `uses`, `needs`, `facts`, `depends` | ir-spec §10b |
| what the compiler PROVED | `pl2cl --facts` wraps each licensed form in `(p-fact (NAME …) FORM)`; declaration classes are on `p-let` / `p-raw-params` / `p-sub` unconditionally | ir-spec §12c, §2b.2a |
| the oracle | `ir-conform/cases/*.expected` — perl 5.40.3's stdout and exit code | `ir-conform/README.md` |

Three rules the walker lives by:

* **DIE on an unknown head** (CLAUDE.md rule 12).  The vocabulary is closed
  and generated; a head that is not in it is a PCL change the backend has not
  caught up with, never something to skip.
* **Do not re-derive front-end facts.**  The `__lex__N` / `__state__N`
  renames, capture analysis, my-shadowing, the sub/eval partition and the
  `PCL_OPT` verdicts all arrive already applied.  The backend adds NO scoping
  analysis of its own.
* **Refuse loudly, per sub.**  `p-sub`'s `:needs` lists the obligation classes
  a sub actually uses, so a partial backend compiles every sub it can and
  emits the ir-spec §9.3b refusal shape for the rest — the same shape the CL
  target uses.

Backend = an S-expression reader + one tree walker.  Runtime = one ES module
(`pcl-rt.mjs`), Node-first; browser concerns are out of scope until M3+.

### II.1 Value model — and the FACTS that decide it

| Perl / IR concept | JS representation (decided) |
|---|---|
| undef | JS `undefined`.  `exists` vs `defined` is `Map.has` vs the stored value |
| scalar value | raw JS `string` / `number` (BigInt in corners, II.8 item 1); never auto-wrapped |
| scalar CELL (`make-p-box`) | `class Box { v }` — one mutable slot; tie/magic hooks attach here |
| array (`make-array :adjustable :fill-pointer`) | `class PArray` over a JS array.  Elements stored RAW; `cell(i)` promotes element *i* to a Box in place (ir-spec §2.2) and reads unwrap transparently.  Negative indices, auto-extension, `$#a`, splice, holes-as-undef |
| hash (`make-hash-table :test 'equal`) | `class PHash` over a `Map` (string keys); same element-cell promotion for `\$h{k}` |
| reference (`p-backslash`) | `class PRef { target, kind }`; `kind` feeds `ref()`.  Identity IS the reference — `\@a` twice yields two PRefs to one PArray |
| bless | a `blessedInto` (package-name string) on the REFERENT, not on the PRef — perl blesses the referent |
| code ref | the JS closure, with a properties object (name, prototype string) under a well-known `Symbol` |
| glob / filehandle | a `PHandle` wrapping a Node fd; deferred to the IO milestone |

**The facts, and what each buys in JS** (`docs/plan-speed-and-ir-s470.md` §B.3
is the source; ✓ = PCL proves it today, ✗ = it would be new work in PCL).  A
backend that ignores every row still produces a CORRECT program — these decide
only how fast it is.

| fact, and where it is on the node | ✓ | what JS does with it |
|---|---|---|
| scalar class (`p-let` `:class` — `:box :scalar :num :str :str-buffer :array :hash`) | ✓ | a plain `let` number or string instead of a `{v}` cell: V8's fast path, no allocation |
| numeric RANGE (fits int32 / int53 / int64) | ✗ | `x\|0` int32 arithmetic, or doubles with no BigInt guard; without it every `+` must check for the NV escape |
| array facts: escapes / written-in-region | ✓ | a plain `Array` used in place — `push` is `arr.push`, no alias cells, `for…of` over values |
| element HOMOGENEITY (all-fixnum / all-double / all-string) | ✗ | `Int32Array` / `Float64Array`, or a PACKED_SMI-shaped array |
| `foreach-raw` (loop variable read-only) | ✓ | `for (const x of arr)` — no alias object per iteration |
| hash key class (constant string / small int) | ✓ partly | a plain object with a stable hidden class for constant-key hashes; `Map` for dynamic ones |
| sub facts (`p-sub` plist: `:returns`, `:insensitive`, no `goto`/`caller`/string-eval/`local`) | ✓ | a plain function with positional parameters and a plain `return`: no wantarray argument, no frame object |
| parameter class (`p-raw-params`) | ✓ | positional parameters — no `arguments`, no `@_` aliasing |
| capture manifest (`:captured` / `:spanning`, `:why`) | ✓ | closures are native; the manifest says which `let`s must live in the closure scope rather than the loop body |
| call-site facts (callee statically known, static context) | partly | a direct call instead of a dispatch; a consistently shaped blessed hash lets V8's inline caches do the method memoisation PCL does by hand |
| `tail-return` | ✓ | a plain `return` — no exception object for a non-local exit |
| `str-buffer` | ✓ | V8 ropes make `+=` cheap already; the fact confirms no aliasing observer |
| regex TIER per literal + parsed flags | ✓ (`:tier`, ir-spec §10-tier) | native `RegExp` for `:native`, the PCRE2 fallback for `:pcre`, the loud refusal for `:refused` — II.8 item 3 |
| dynamic-scope use per sub (`:needs`) | ✓ | no save/restore stack in a sub that never `local`s; magic globals as module-level `let`s |
| exception use per sub (`:needs`) | ✓ | no `try` frame where nothing can throw |
| phase facts (`BEGIN`/`INIT`/`END` present) | ✓ | run in order; no phase machinery when absent |

### II.2 The op inventory, family by family

54 families, 698 names (`docs/ir-op-inventory.md`; the per-op arity, context
sensitivity, coercions, magic, dies/dynamic/phase columns and the family's
ir-spec §10 rule are all there).  This table says only what the JS side of
each family looks like, and which milestone owes it.

| family (count) | JS |
|---|---|
| numeric (9), numeric-compare (8), math (8) | `rt.*` one-for-one.  Numify per ir-spec §3.1, return a raw number; `/` yields a double when inexact, `%` follows perl's sign rules, shifts truncate and clamp.  Compares return `1`/`""`, not booleans |
| string (27), string-compare (7) | `rt.*`; stringify per §3.2, return a raw string.  `$_`-defaults arrive explicit in the tree |
| bitwise (14), bit-string (2) | ONE mode decision per op (`%p-bitwise-operand-kind`): numeric iff an operand carries a number, else byte-by-byte on the stringified operands.  Overload hook first |
| logical (8) | `&&` / `||` / `??` — they return the OPERAND, exactly as JS does |
| increment (4), compound-assignment (35) | read-modify-write on the box or the slot; the `-raw` twins are a plain `x = NEW` with the identical NEW form.  `&&=`/`||=`/`//=` short-circuit and store the RHS unchanged.  `p-++` on a pure-alpha string is perl's magic string increment |
| assignment (11) | store per §2.2.  A list assignment used as a VALUE is two-faced: scalar/void yields the element COUNT, list context yields the LHS lvalues |
| elements (22), slice-delete (4) | `PArray`/`PHash` accessors.  Reads unbox scalars and keep reference boxes; writes autovivify the intermediate refs; an EMPTY slice answers undef in scalar and the empty list in list context |
| aggregate-builtin (27) | `push`/`pop`/`shift`/`splice`/`keys`/`values`/`each`/`sort`/`grep`/`map` on `PArray`/`PHash`.  `p-sort`'s default is STRING order and its comparator sees the `$a`/`$b` pair (M2); `%p-push1` and `%p-sort-classic` are sugar — expand them back and nothing is lost |
| box (24), reference (21), refaliasing (7) | `Box` / `PRef` construction and deref; refaliasing (`\$x = \$y`) rebinds the CELL, so it must go through the cell, never the value |
| declaration (3) | `p-let` / `p-raw-params` / `p-sub`'s facts — the compiler's VERDICTS.  A backend may DROP all three vocabularies and still be correct; II.1's fact table is what it gives up |
| context-frame (9) | names for `let`/`lambda` shapes: implement the expansion, nothing else.  The wantarray value is the frame's, ir-spec §4 |
| control-flow (26) | see II.3 |
| exception (12) | `PerlDie` objects; `p-eval-block` is `try`/`catch` with `$@` set per §6.3 |
| dynamic-scope (18) | `local`: slot swap + `try`/`finally`.  Only needed in subs whose `:needs` says so |
| regex (5), compiled-regex (2) | II.8 item 3's three tiers.  A `qr` is an OBJECT with its own flags and identity that stringifies as `(?^flags:SOURCE)`; a pattern that is exactly one interpolated qr IS that qr |
| range (8) | `p-foreach-range` is a counting `for`; the list form materialises |
| io (24), file-ops (19), directory-io (4), capture-io (3), filetest (29), socket (15) | `PHandle` over Node's fs/net.  Bareword handles are SYMBOLS (ir-spec §7.5); `p-open` boxes its handle argument; `p-close` on a pipe reaps and sets `$?`.  The filetest family shares ONE operand resolution and the `_` cache (§10c).  M3+ |
| command-capture (1), process (11), env (3), time (6), user-db (11) | Node `child_process`, `process.env`, `Date`, the passwd/group lookups.  Command capture is wantarray-sensitive: scalar = the whole stdout, list = split into `$/` records |
| oo (6), package-tracking (5), typeglob (12), introspection (15) | the package registry, the C3 MRO walk (§7.3), `ref`/`can`/`isa`/`caller`.  `p-caller` reports the package; file/line are stubs today (a divergence the backend inherits, #1240's neighbourhood) |
| module-system (6), phase (8) | `use`/`require`/`%INC` and the section model — II.5 |
| sub-definition (5), signature (4), call (1) | II.4 |
| overload (6) | the operator-overload hook the numeric/string/bitwise families consult FIRST |
| tie (8) | hooks on the cell.  Not in M0–M2 (and `tie %h` is not implemented on the CL target either — #155) |
| magic-global (136) | `$_`, `$0`, `@ARGV`, `%ENV`, `$@`, `$!`, the match state, the caret variables.  ir-spec §8 says which are read-only, which are set by which op, and which are per-package.  Most are module-level `let`s; the match-state ones are written by the regex family |
| pack (2), extension (1), runtime-config (1), ir-literal (2), misc-builtin (3) | `p-esc` and `p-literal-string` are the string-escape decoder II.7 needs anyway; `pack`/`unpack` is a port of the transpiled `cl/pcl-pack` artifact, which compiles through this backend for free |
| tap (27) | Test::More's assertions.  Needed to run the perl test suites, not to run programs |
| UNCLASSIFIED (3) | the inventory says which; a backend treats them like any unknown head — DIE |

### II.3 Statement and control flow — the §11b kernel

The IR's own heads:

| IR head | JS |
|---|---|
| `p-if` / `p-while` / `p-until` | native `if` / `while`, with `rt.truthy` on the condition |
| `p-for` (C-style) | native `for` |
| `p-foreach`, `p-foreach-range`, `p-foreach-raw` | `for…of`; `-raw` needs no per-iteration alias object |
| `last` / `next` + labels | labelled `break` / `continue` where the exit is lexical; a tagged throw where it is not (perl lets a CALLED SUB exit its caller's loop) |
| `redo` | loop-body-in-inner-loop; the only consumer of that transform |
| `p-return` | `return` in tail position (the `tail-return` fact says when), a `ReturnSignal` throw otherwise |
| `p-local-cell` | slot swap + `try`/`finally` |
| `p-try` | native `try`/`catch`/`finally` + `PerlDie` |
| `p-eval-block` | `try`/`catch`, `$@` per §6.3 |
| `p-eval` (string) | the compiler as a subprocess on Node — II.8 item 4 |

And the host forms underneath them (ir-spec §11b, which carries the same table
with a C column):

| kernel group | JS |
|---|---|
| `progn` `prog1` `prog2` `let` `let*` | a block; `prog1` needs a temp; `let*` is sequential `let`s |
| `lambda` `function` `funcall` `apply` | a closure; `apply` is `f(...args)` |
| `multiple-value-bind` `values` `multiple-value-list` `nth-value` | return a small array and destructure |
| `setq` `setf` `psetf` `incf` `decf` `push` `pop` | assignment and its place setters — the emitter uses `setf` only on `aref`, `gethash`, `p-aref`, `p-gethash` and a variable.  CL `push`/`pop` are at the FRONT |
| `if` `when` `unless` `cond` `case` `ecase` `typecase` | `if` chains — every one is an EXPRESSION in CL, so a statement target needs a temp or a ternary; `ecase`'s missing arm must throw |
| `and` `or` `not` `null` | `&&` / `||` / `!` — they return the OPERAND; CL `nil` is **not** perl's false (§2.1) |
| `block` `return-from` | a labelled block with `break LABEL` when lexical, a tagged throw otherwise |
| `catch` `throw` | `throw {tag, value}`, re-thrown when the tag does not match.  The tag is a VALUE compared with `eq`, not a static label |
| `tagbody` `go` | `while (true) switch (pc)` — `go` can jump BACKWARD, which `continue` cannot express |
| `unwind-protect` | `try { … } finally { … }` |
| `list` `cons` `car` `cdr` `append` | cons cells — the runtime's own list arguments (a capture alist, a facts plist), never a perl array |
| `vector` `make-array` `vector-push-extend` `aref` `elt` `length` | `Array`; `:adjustable t :fill-pointer 0` is `[]` and `vector-push-extend` is `push` |
| `make-hash-table` `gethash` | `Map` (the test is `equal` — structural string equality) |
| `declare` `declaim` `locally` and their hints | dropped; ir-spec §11 |

**What is not in the kernel yet.**  §11b's closing table lists the bare CL
functions still reaching the output through a v1 seam — 31 distinct symbols
over the 111-file corpus at s470bo (`-` `+` `*` `1+` `rem` `truncate` `logior`
… `format` `intern`), owned by #1175/#1176.  A backend either implements them
or refuses the files that contain them; `tools/ir-host-leak.pl` says which
files those are.

### II.4 Subs, packages, dispatch

| IR | JS |
|---|---|
| `(p-sub pl-NAME LAMBDA-LIST …)` | `P.sub("NAME", rt.mkSub(...))`.  The `pl-` prefix and ALL CL symbol discipline — pipe-quoting, `:invert`, read-time `in-package` — disappear: the registry key is the plain Perl name, which is why the #418 symbol-mangling bug class does not exist on this target |
| `p-declare-sub` | `P.declareSub("NAME")` — the forward declaration barewords resolve against |
| `p-raw-params` | positional JS parameters, one per class in the list |
| the `p-sub` facts plist | II.1's fact table; `:needs` decides which frames the body gets |
| `p-defpackage` / `in-package` | `const P = rt.pkg("Name")` scoping the section.  The current package is a RUN-TIME value (§7.1): a module-level `currentPackage` with save/restore |
| `p-method-call` | dispatch by string name through the C3 MRO walk (§7.3).  The transpiled `cl/pcl-mro` artifact compiles through this backend for free |
| `p-call-of-undefined-sub` | the AUTOLOAD-then-die decision, at the CALL where perl makes it (ir-spec §5.4) |

### II.5 Phases

`p-run-compile-phase-blocks` and the `p-bucket` section markers arrive in
EMIT order, and that order is the execution order: every section's
compile-phase buckets first, then every section's run bucket (ir-spec §12b,
§9).  In JS, "load the module" IS the compile phase, so BEGIN ordering is
statement order.  The walker keeps the section boundaries as comments.

### II.6 What the runtime module must contain, per milestone

| milestone | the runtime owes |
|---|---|
| M0 | `Box`, `PArray`/`PHash` with element-cell promotion, `PRef`, the ir-spec §3 coercion TABLES (port the table, not an approximation), the scalar/string op families, the context stack + `mkSub`, `die`/`evalBlock`, `print`, the package registry |
| M1 | the aggregate builtins, `sort` with the `$a`/`$b` pair, `local`, references and refaliasing |
| M2 | II.8's numbers and strings notes implemented; regex tier 1 |
| M3 | module loading (`p-use` of the `lib/` shims, pre-transpiled), file IO, `%INC` |
| never | formats (not-supported on the CL target too) |

### II.7 The preamble, and the five reader rules

The CL preamble (`@INC` pushes with build-machine paths, `*pcl-pl2cl-path*`,
`p-defpackage`, the forward `defvar`s) is environment bootstrap, not program
(ir-spec §11).  The walker RECOGNISES it and replaces it with the JS runtime's
own init (argv/env binding, `@INC` from the environment).  Do not translate it
literally — those paths are #217, and a JS target must not inherit them.

A backend that consumes `--emit-sexp` needs none of the CL reader's rules; one
that parses the CL TEXT needs exactly five, and ir-spec §11b states each with
its measurement:

| rule | why it bites |
|---|---|
| `\|…\|` is verbatim | that is how `\|$"\|`, `\|@,\|` and every non-ASCII name is spelled; outside the bars a token is NFKC-normalised and case-INVERTED |
| an EMPTY `\|\|` contributes nothing | `p-\|\|` and `p-` are the SAME symbol, named `P-`, and it is perl's `\|\|` |
| `#\c` is a character | `#\Newline`-style names run to the next delimiter |
| `#x` `#o` `#b` `#NNr` are NUMBERS | 400+ of them in `perl-tests/pack.t` |
| a control character is never raw in a literal | it is `(p-esc "…")` with the §12b escape alphabet — so a line-oriented consumer is correct, and `p-esc` is the same unescape routine the data form's reader needs |

**Acceptance.**  `tools/ir-conform --backend ./my-backend` (345 cases, perl
5.40.3 as the oracle, `ir-conform/README.md`).  Milestone M0's own bar is the
subset of cases whose manifest `NEEDS` is within what M0 implements — the
manifest is there precisely so that subset is computed, not guessed.

### II.8 Open design items (each needs its own note before its milestone)

1. **Numbers** (before M2): JS double vs perl IV/NV.  Direction: raw JS
   number; `rt` ops escalate to BigInt when an integer op leaves the safe
   range; stringification implements perl's %.15g-equivalent.  Acceptance
   corpus: the arith/int rows of perl-tests.
2. **Strings** (before M2): perl strings are codepoint sequences with a
   byte/char duality; JS strings are UTF-16 code units.  Direction:
   JS strings throughout; `rt.length` counts code points; byte semantics
   ride on ≤0xFF-only strings (perl's own downgraded model).  The corners
   (chr(0x110000)+, `use bytes`) are already not-supported on the CL
   target — parity, not perfection.
3. **Regex** (the big one, own doc): a THREE-TIER design (settled in
   outline s460, 2026-09-01; the own doc fills in the translation table).
   - **Tier 1 — translate to native JS `RegExp`** for the measured-common
     subset.  This is the performance-right default: V8's regex engine is
     JITted and fast, and ES2018+ covers more than the folklore says —
     named captures, lookbehind, Unicode property escapes (`\p{…}` under
     `u`), dotall `s`, and sticky `y` for the `\G`/`m//gc` idiom.  PCL
     already ships a pattern-rewriting layer (the cl-ppcre translation in
     the CL runtime), so the approach is proven in-house; this is a second
     rule table over the same scanner, not a new mechanism.
   - **Tier 2 — PCRE2 as the fallback engine** for constructs JS RegExp
     cannot express but perl programs actually use (possessive
     quantifiers, atomic groups, recursion `(?R)`, conditionals
     `(?(1)…)`, POSIX classes, `\K`).  npm landscape as surveyed
     2026-09-01: the bare `pcre` and `pcre2` packages are DEAD (years
     stale) — do not reach for them; the live options are
     `@segevfiner/pcre2` (Node-API native binding, maintained) and the
     current `pcre2-wasm` builds (tracking PCRE2 10.47.x).  The trade-off
     to decide at that milestone: native addon = PCRE2's JIT (fast) but
     node-gyp/prebuilt distribution pain; WASM = zero toolchain and
     browser-portable, but interpreter-only (no JIT inside WASM) plus a
     per-match string-marshalling copy — exactly the m//g hot-loop cost
     the CL backend just spent a round removing.  Start WASM for
     portability, measure, promote to the N-API binding only if a real
     workload demands it.
   - **Tier 3 — the LOUD perl-shaped die** for what neither engine runs
     (`(?{…})`, `(??{…})` — the same compile-time announced refusals the
     CL target has, #874's shape), so the refusal discipline is shared
     verbatim across backends.
   - **Both-backends note**: PCRE2 is ALSO the CL side's named "engine
     gap next" (regexg ~2.1× = cl-ppcre vs perl's C engine, s454ac).  The
     pattern-classification corpus and the translation rule table built
     for tier 1/2 should be written backend-neutral — one "which
     constructs does this pattern use" classifier serving the JS
     translator, a future PCRE2-FFI path on SBCL, and the announce layer.
4. **String eval architecture**: Node = `pl2cl --server` subprocess,
   verbatim from the CL runtime's design (HARD REQUIREMENT preserved);
   browser = out of scope until someone asks.
5. **pclxs on Node**: sketch §5 is the plan of record (N-API trampoline,
   same vtable/ABI, `tools/pcl-conform` as the acceptance bar); nothing to
   add until the pure-JS runtime exists.

### II.9 Milestones and acceptance

Since s470bt every milestone's acceptance is the SAME corpus, sliced by what
the milestone implements: `tools/ir-conform --backend ./my-backend` over the
cases whose `pl2cl --manifest` `NEEDS` fits inside it (345 cases, perl 5.40.3
as the oracle — `ir-conform/README.md`).  The slice is computed from the
manifest, never guessed, and the CL target's own score on it (287 pass, 58
known bugs) is the honest ceiling to measure against.

* **M0 (the sketch §6 spike):** reader + walker + `pcl-rt.mjs` covering
  the heads used by `Pl/t/transpile-test-01.t`'s programs; no IO beyond
  print, no regex, no eval.  Acceptance: the ir-conform cases whose `NEEDS`
  is empty of regex, IO, string-eval and phases.
* **M1:** the remaining `transpile-test-*` corpora and the ir-conform
  `context` / `coercion` / `refs` / `array` / `hash` topics; `use`-free only.
* **M2:** numbers + strings notes implemented; the non-regex, non-IO rows
  of a first perl-tests file (e.g. the aassign/list slices).
* **M3:** module loading (`p-use` of `lib/` shims via pre-transpiled JS),
  minimal file IO; first full perl-tests file green under node.

Each milestone = measured row counts, never impressions (the
`feedback_cause_not_count` rule applies to this backend from day one).
