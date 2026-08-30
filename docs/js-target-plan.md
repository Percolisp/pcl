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
    local $level = $level + 1;
    inner();
}
```

```js
function outer() {
    let saved = pkg("main").scalar("level").get();
    pkg("main").scalar("level").set(saved + 1);
    try {
        inner();
    } finally {
        pkg("main").scalar("level").set(saved);
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
two is the single largest item of work in porting the runtime; this
document does not enumerate the differences, only flags it as the biggest
unsolved problem.

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

Everything in this part is keyed to REAL IR captured from HEAD (`e43ef48`,
gen v2-405) by transpiling four probe programs; re-capture them before
implementing — the IR vocabulary is stable but individual emissions move.
The JS on the right-hand side is normative in SHAPE, not in letter: runtime
entry-point names may differ, the division of labor may not.

### II.0 Architecture and inputs

* Backend = an S-expression reader + one tree walker over the emitted CL,
  exactly as sketch §1.  It consumes `pl2cl`'s OUTPUT; it never sees Perl.
  `Pl/**` is reused byte-for-byte.
* Runtime = one ES module (`pcl-rt.mjs`), Node-first.  Browser concerns are
  out of scope until M3+ (sketch §3–§5).
* The walker must implement the **closed vocabulary** of ir-spec §10 and DIE
  on any head it does not know (CLAUDE.md rule 12 — a missing case never
  falls through).  The emitted preamble (`@INC` pushes,
  `*pcl-pl2cl-path*`) is recognized and largely *discarded* (Node has its
  own resolution story; see II.7).
* **Do not re-derive front-end facts** (sketch §2, §2b): the `__lex__N` /
  `__state__N` renames, capture analysis, my-shadowing, the sub/eval
  partition and the PCL_OPT raw-slot verdicts all arrive already applied.
  A raw (unboxed) variable in the IR compiles to a plain `let`; a celled
  one to a Box.  The backend adds NO scoping analysis of its own.

### II.1 Value model

| Perl / IR concept | JS representation (decided) |
|---|---|
| undef | JS `undefined`.  `exists` vs `defined` is carried by `Map.has` vs the stored value. |
| scalar value | raw JS `string` / `number` (BigInt in corners, II.10); never auto-wrapped. |
| scalar CELL (`make-p-box`) | `class Box { v }` — one mutable slot.  Later magic (tie hooks) attaches here. |
| array (`make-array :adjustable :fill-pointer`) | `class PArray` over a JS array.  Elements stored RAW; `cell(i)` promotes element *i* to a Box in place (the CL runtime's `p-aref-box` policy, ir-spec §2.2) and reads unwrap transparently.  Implements negative indices, auto-extension, `$#a`, splice, holes-as-undef. |
| hash (`make-hash-table :test 'equal`) | `class PHash` over a `Map` (string keys); same element-cell promotion for `\$h{k}`. |
| reference (`p-backslash`) | `class PRef { target, kind }` — `kind` feeds `ref()`.  Identity IS the reference; `\@a` twice yields two PRefs to one PArray (perl's `\@a == \@a` string-equality holds via target identity when stringifying). |
| bless | a `blessedInto` (package-name string) field on the REFERENT (PArray/PHash/Box), not on the PRef — perl blesses the referent. |
| code ref | the JS closure itself, with a properties object (name, prototype string) under a well-known `Symbol`. |
| glob / filehandle | deferred to the IO milestone; the IR's handle objects map onto a `PHandle` wrapping Node fds. |

### II.2 Variable classes — the worked examples

Captured IR is abbreviated (preamble and some nesting elided); the JS shows
the intended lowering shape.

#### (a) `our`, file `my`, `state`, `local` — probe `ir1.pl`

```perl
our $g = 1;
my $x = 2;
sub bump { state $n = 0; $n++; return $n }
sub show { print "g=$g x=$x\n" }
sub localize { local $g = 99; show(); }
```

IR (the load-bearing forms):

```lisp
(p-defcell $g (make-p-box nil))            ; our — package cell
(p-defcell $x (make-p-box nil))            ; file lexical, captured by subs → promoted cell
(p-defcell $n__state__0 (make-p-box nil))  ; state cell, renamed by the front-end
(p-defcell $n__state__0__init nil)
(p-sub pl-bump (&rest %_args)
  (p-args-body (block nil (p-void-ctx
    (unless $n__state__0__init (box-set $n__state__0 0) (setf $n__state__0__init t))
    (p-post++ $n__state__0)
    (p-return $n__state__0)))))
(p-sub pl-localize (&rest %_args)
  (p-args-body (block nil (p-void-ctx
    (p-local-cell $g (p-box-for-local 99)
      (p-caller-ctx (pl-show)))))))
```

JS lowering:

```js
const P = rt.pkg("main");
const $g = P.sv("g");                    // p-defcell $g → the package's named cell (a Box)
const $x = new rt.Box();                 // promoted file lexical: module-level Box, plain identifier
const $n__state__0 = new rt.Box();       // state: module-level cell + init flag, verbatim
let  $n__state__0__init = false;

P.sub("bump", rt.mkSub((args) => {
  if (!$n__state__0__init) { $n__state__0.v = 0; $n__state__0__init = true; }
  rt.postInc($n__state__0);
  return rt.retScalar($n__state__0);
}));

P.sub("localize", rt.mkSub((args) => {
  return rt.localCell(P, "g", rt.boxForLocal(99), () =>   // try/finally inside
    rt.callerCtx(() => P.call("show")));
}));
```

Decisions fixed here:

* **`p-defcell` = define-once** (Direction-D semantics): `P.sv(name)`
  creates on first use, returns the existing Box after — same contract as
  the CL `p-defcell` guard.
* **`local` localizes the CELL IN THE SLOT, not the value**: `p-local-cell`
  installs a FRESH Box (`p-box-for-local`) into the package slot and
  restores the old Box in `finally` — so a reference taken before the
  `local` keeps seeing the old value, exactly perl.  Consequence: compiled
  code may cache `P.sv("g")` **only across a region where no `local` of
  that name can intervene**; the safe default is that *package*-variable
  access in sub bodies goes through the slot lookup (`P.sv("g")` each
  time), and only promoted file lexicals get the cached-const treatment.
  (Measure before optimizing; this is the JS twin of the CL symbol-value
  read.)
* Exceptions unwind through `finally`, so `local` + `die` is correct for
  free — the sketch §2 claim, now concrete.

#### (b) Arrays, hashes, references — probe `ir2.pl`

```perl
my @a = (1,2,3);  my %h = (k => 'v');
my $ar = \@a;     my $hr = { x => 1 };
push @a, 4;
print "$a[0] $h{k} $$ar[1] $hr->{x} ", scalar(@a), "\n";
my $sr = \$a[0];  $$sr = 10;
```

IR:

```lisp
(let ((@a (make-array 0 :adjustable t :fill-pointer 0)))
  (p-array-= @a (vector 1 2 3))
  ... (p-my-= $ar (p-backslash @a))
  ... (p-my-= $hr (make-p-box (p-hash "x" 1)))
  (p-push @a 4)
  (p-print (p-string-concat (p-aref @a 0) " " (p-gethash %h "k") " "
                            (p-aref-deref $ar 1) " " (p-gethash-deref $hr "x") " ")
           (p-list-ctx (p-scalar @a)) "\n")
  ... (p-my-= $sr (p-backslash (p-aref-box @a 0)))
  (p-setf (p-cast-$ $sr) 10))
```

JS:

```js
const a = new rt.PArray();               // block-scoped my → plain let/const of the container
rt.arrayAssign(a, [1, 2, 3]);            // p-array-= : clear + fill, returns the list
const h = new rt.PHash();
rt.hashAssign(h, ["k", "v"]);
const $ar = new rt.Box(rt.ref(a));       // p-backslash @a → PRef{target:a, kind:"ARRAY"}
const $hr = new rt.Box(rt.hashRef(["x", 1]));   // anon {} → new PHash wrapped in a PRef
a.push(4);
rt.print(rt.concat(a.get(0), " ", h.get("k"), " ",
                   rt.arefDeref($ar, 1), " ", rt.hgetDeref($hr, "x"), " "),
         rt.listCtx(() => a.scalar()), "\n");
const $sr = new rt.Box(rt.ref(a.cell(0)));      // p-aref-box: promote element 0 to a Box
rt.derefSet($sr, 10);                            // (p-setf (p-cast-$ ...)) — writes through PRef→Box
```

Decisions fixed here:

* `p-aref-box` is the SINGLE mechanism behind `\$a[0]`, foreach aliasing
  and `local $a[0]`: element promotion, one policy, inherited from ir-spec
  §2.2 — do not invent a second one.
* Dereference ops (`p-aref-deref`, `p-gethash-deref`, `p-cast-$`) are
  runtime calls that unwrap PRef (and vivify where the IR says so); they
  never become raw JS property access, because the coercion/vivification
  rules live in them.

#### (c) foreach, fresh bindings, aliasing, closures — probe `ir3.pl`

```perl
foreach my $i (1..3) { push @subs, sub { return $i * 10 } }
$_ *= 2 for @l;
```

IR:

```lisp
(p-foreach-range ($i 1 3) :my t
  (p-push @subs
    (lambda (&rest %_args)
      (let ((@_ (p-flatten-args %_args))
            (*pcl-current-package* "main")
            (*pcl-caller-wantarray* *wantarray*))
        (catch :p-return (block nil (p-return (p-* $i 10))))))))
(p-foreach ($_ @l) (p-*= $_ 2))
```

JS:

```js
for (let i = 1; i <= 3; i++) {           // p-foreach-range … :my t
  const $i = new rt.Box(i);              // FRESH Box per iteration — JS per-iteration let
  subs.push(rt.mkSub((args) => rt.retScalar(rt.mul($i.v, 10))));
}
for (const $_ of l.aliasCells())         // p-foreach over an array ALIASES elements
  rt.mulAssign($_, 2);                   // writes through the element Box into the array
```

Decisions fixed here:

* `:my t` = fresh cell per iteration; without it the loop reuses one cell
  (perl's non-`my` foreach variable).  The walker reads the flag, nothing
  else.
* Aliasing foreach iterates `aliasCells()` — element-Box promotion again
  (one mechanism, II.2b).
* `mkSub` is the ONE sub prologue: flatten args into `@_`, push the
  package/frame, capture the caller's context (`*pcl-caller-wantarray*` →
  a frame field), install the `:p-return` boundary.  In JS a plain
  `return` covers most `p-return`s; a `ReturnSignal` throw + try/catch in
  `mkSub` covers returns from inside nested constructs.  Emit the cheap
  form when the walker can see the return is tail-positioned.

#### (d) Context, wantarray, eval/die — probe `ir4.pl`

```perl
sub ctx { return wantarray ? "list" : "scalar" }
my @r = ctx(); my $s = ctx();
print "@r $s\n";
my $n = eval { die "oops\n"; 1 };
print "err=$@" if !defined $n;
```

IR:

```lisp
(p-sub pl-ctx (&rest %_args)
  (p-args-body (block nil (p-return (p-if (p-wantarray) "list" "scalar")))))
(p-array-= @r (p-list-ctx (pl-ctx)))
(p-my-= $s (p-scalar-ctx (pl-ctx)))
(p-my-= $n (p-scalar-ctx (p-eval-block (p-die :loc "- line 4" "oops\n") 1)))
(p-if (p-! (p-defined $n)) (p-print (p-string-concat "err=" $@)))
```

JS:

```js
P.sub("ctx", rt.mkSub((args) =>
  rt.retScalar(rt.wantarray() ? "list" : "scalar")));

rt.arrayAssign(r, rt.listCtx(() => P.call("ctx")));
rt.myAssign($s, rt.scalarCtx(() => P.call("ctx")));
rt.myAssign($n, rt.scalarCtx(() =>
  rt.evalBlock(() => { rt.die("oops\n", "- line 4"); return 1; })));
if (rt.truthy(!rt.defined($n.v)))
  rt.print(rt.concat("err=", rt.sv("@").v));   // $@ is just the package cell "@"
```

Decisions fixed here:

* **Context is an explicit stack in `rt`** (`rt.CTX`): `listCtx`/
  `scalarCtx`/`voidCtx` push, run the thunk, pop in `finally`;
  `callerCtx` re-pushes the current frame's captured caller context
  (the `p-caller-ctx` form).  `wantarray()` reads the frame field
  `mkSub` captured.  This is the sketch §2 "context stack or hidden
  argument" question ANSWERED: stack in the runtime, captured into the
  frame at sub entry — the exact JS image of the CL dynamic pair
  `*wantarray*` / `*pcl-caller-wantarray*`.
* `rt.die` throws `class PerlDie { msg, loc }`; `evalBlock` catches
  **everything** (perl's eval traps runtime errors too), stringifies
  non-PerlDie conditions into `$@` perl-shaped, sets `$@`, returns
  `undefined`; clears `$@` on success.  Magic globals (`$@`, `$_`, `$"`,
  `$\`, `$0`, `%ENV`, `@ARGV`) are ordinary cells in the registry — the IR
  already spells them as variables (`$@`, `|$"|`).

### II.3 Statement & control-flow heads

| IR head | JS |
|---|---|
| `p-if` / `p-while` / `p-until` | native `if`/`while` with `rt.truthy` on the condition |
| `p-foreach`, `p-foreach-range` | II.2c |
| `p-for` (C-style) | native `for` |
| `last`/`next` + labels | labeled `break`/`continue` (IR restricts the shapes; sketch §2) |
| `redo` | loop-body-in-inner-loop transform (only consumer of it) |
| `p-return` | `return` (tail) / `ReturnSignal` (nested), II.2c |
| `p-local-cell` | II.2a — slot swap + try/finally |
| `p-try` (perl 5.34 try/catch/finally) | native try/catch/finally + PerlDie |
| `p-eval-block`, `p-eval` (string) | II.2d; string eval = compiler-as-subprocess on Node (sketch §3) |
| op families (`p-+`, `p-string-concat`, `p-post++`, `p-*=`, …) | `rt.*` calls one-for-one; the ir-spec §10 family rules (which operand is a cell, which coerces) port UNCHANGED |

### II.4 Subs, packages, dispatch

* `(p-sub pl-NAME …)` → `P.sub("NAME", rt.mkSub(...))`; `p-declare-sub` →
  `P.declareSub("NAME")` (forward-decl so barewords resolve).  The `pl-`
  prefix and all CL symbol discipline (pipe-quoting, `:invert`, read-time
  `in-package`) **disappear**: the registry key is the plain Perl name —
  the sketch's promise that the #418/#498 bug class does not exist here.
* `p-defpackage`/`in-package` → `const P = rt.pkg("Name")` scoping the
  emitted section; a mid-block `package` switch (which the CL target must
  emit qualified names for) is just a different registry handle.
* Method calls: the IR dispatches by string name through C3 MRO in the
  runtime (`p-method-call`); the JS runtime ports the MRO walk, and the
  transpiled `pcl-mro` artifact compiles through this backend for free
  (sketch §3's dividend).

### II.5 Phases

`p-run-compile-phase-blocks` and the section model arrive in emit order;
in JS, "load the module" IS the compile phase, so BEGIN ordering is just
statement order (sketch §3).  The walker keeps the section boundaries as
comments for debuggability, nothing more.

### II.6 What the runtime module must contain for M0–M1

Boxes, PArray/PHash + element-cell promotion, PRef, the coercion tables
(ir-spec §3: to-number, to-string, truthiness — port the TABLE, not an
approximation), op families for scalars/strings/arrays, context stack +
mkSub, die/evalBlock, print to stdout, the package registry.  Explicitly
NOT in M0–M1: regex, IO beyond print, string eval, tie/overload, sort's
`$a`/`$b` pair (M2), formats (never — not-supported).

### II.7 The preamble

The CL preamble (`@INC` pushes with build-machine paths, `*pcl-pl2cl-path*`)
is recognized by the walker and REPLACED by the JS runtime's own init
(argv/env binding, `@INC` from env).  Do not translate it literally — the
paths are the #217 problem, and a JS target must not inherit it.

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
3. **Regex** (the big one, own doc): start with pattern TRANSLATION to JS
   RegExp for the measured-common subset + a LOUD perl-shaped die on any
   untranslatable construct (the project's refusal discipline), and let
   the corpus decide whether an engine port is ever needed.
4. **String eval architecture**: Node = `pl2cl --server` subprocess,
   verbatim from the CL runtime's design (HARD REQUIREMENT preserved);
   browser = out of scope until someone asks.
5. **pclxs on Node**: sketch §5 is the plan of record (N-API trampoline,
   same vtable/ABI, `tools/pcl-conform` as the acceptance bar); nothing to
   add until the pure-JS runtime exists.

### II.9 Milestones and acceptance

* **M0 (the sketch §6 spike):** reader + walker + `pcl-rt.mjs` covering
  the heads used by `Pl/t/transpile-test-01.t`'s programs; no IO beyond
  print, no regex, no eval.  Acceptance: byte-compare node vs perl on
  those programs — the project's standard oracle discipline.
* **M1:** the four probe programs of II.2 + the remaining
  `transpile-test-*` corpora; `use`-free only.
* **M2:** numbers + strings notes implemented; the non-regex, non-IO rows
  of a first perl-tests file (e.g. the aassign/list slices).
* **M3:** module loading (`p-use` of `lib/` shims via pre-transpiled JS),
  minimal file IO; first full perl-tests file green under node.

Each milestone = measured row counts, never impressions (the
`feedback_cause_not_count` rule applies to this backend from day one).
