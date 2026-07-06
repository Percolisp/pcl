# PCL IR Manual — Semantics of the Generated CL

**Status:** normative. Where this document and the runtime disagree, the
runtime (`cl/pcl-runtime.lisp`) is the reference implementation and this
document has a bug — please fix it.
**Audience:** anyone (person or AI) consuming PCL's generated Common Lisp:
translating it to another target environment, building tooling on it, or
debugging it. You should be able to reimplement the semantics from this
document without reading Lisp.
**Companion:** `docs/generated-cl-ir-review.md` (what the output looks like,
its friction points, and the improvement roadmap). This manual covers what
the constructs *mean*.
**Verified against:** cache generation v2-7, 2026-07-06. Each claim below
was checked against the runtime source; section references name the
defining function so you can re-verify.

---

## 0. The one-paragraph model

PCL compiles Perl to a tree of calls into a closed runtime vocabulary
(`p-*`). Perl scalars become mutable cells ("boxes") or, when the compiler
proves it safe, raw numbers/strings. Perl's implicit coercions live inside
the ops (`p-+` numifies, `p-.` stringifies), not in the code shape. Perl's
scalar/list context becomes a dynamically-scoped flag bound at call sites.
Non-local control (`return`, `last`, `die`) becomes tagged non-local exits.
Packages become namespaces loaded strictly in source order. A translator
reimplements the vocabulary and the five protocols below (§2–§8); the tree
itself carries no host-specific semantics beyond them.

## 1. Reading the output

- **Parse with an S-expression reader.** Symbols contain `$ @ % & : + - * /
  . < > = ~ ! # | ^` — never tokenize by regex. Strings are
  double-quoted with `\"` and `\\` as the only escapes; they may contain
  raw control characters (real newlines/tabs inside the quotes — see the
  review doc §3.2).
- **Comments (`;` to end of line) are non-semantic** — source echoes for
  humans.
- **`(declare …)` forms are host compiler advice** — droppable wholesale.
- **Order is the program.** A file is a sequence of top-level forms
  executed in order by `load`. There is no linker step: a name must be
  defined by an earlier form (or a forward declaration) before a later
  form *executes* it. `(eval-when (:compile-toplevel :load-toplevel
  :execute) …)` marks forms that Perl requires visible to `BEGIN` blocks
  earlier in the same file; a translator with no compile/load phase split
  simply executes them in order — but see §9 for `BEGIN` itself.
- **File layout:** preamble (`in-package`, `@INC` setup) → per-package
  *sections* in source order, each: package preamble → declarations
  (`p-declare-sub`, `defvar`) → definitions (`p-sub`, scheduled blocks) →
  runtime (top-level statements). A `package` statement mid-file starts a
  new section; a reopened package gets only `(in-package …)` +
  `p-set-current-package`.

## 2. The data model

### 2.1 undef

Perl's `undef` is the singleton `:undef` (the keyword — held in
`*p-undef*`), **not** the host's null. The host null (`nil`) *also*
appears, with a different meaning: an array slot that was never assigned
(a hole), or "absent". `p-exists-array` distinguishes a hole (`nil`) from
a stored `undef`. Rules: `undef` numifies to 0, stringifies to `""`, is
false. A translator needs two distinguishable values here.

### 2.2 Scalars: boxes and raw slots

A Perl scalar is by default a **box** (`p-box`, defined at
`defstruct p-box`): a mutable cell with fields

| field | meaning |
|---|---|
| `value` | the authoritative value |
| `nv`/`nv-ok` | cached numeric view + validity flag |
| `sv`/`sv-ok` | cached string view + validity flag |
| `class` | blessed class name (nil unless blessed) |
| `is-ref` | t when the box was created by the reference operator `\` |

The nv/sv caches are **pure optimization** — every write
(`box-set`) invalidates them; a translator may omit caching entirely.
`class` and `is-ref` are semantic.

When the compiler (VarAnnotator) proves a `my` scalar is only ever written
raw-value shapes, it emits a **raw slot** instead: a plain host variable
holding a number or string directly. The shapes tell you which is which:

| kind | declaration | write | read |
|---|---|---|---|
| boxed lexical | `(let (($x (make-p-box nil)))` | `(p-my-= $x V)` | `$x` |
| raw lexical | `(let (($n 10))` | `(setf $n V)` | `$n` |
| package var | `(defvar $g (make-p-box nil))` | `(p-scalar-= $g V)` | `$g` |

**Invariant: a raw slot never holds a box or a reference** — only host
numbers and strings. Ops always accept either form (they unbox
internally), so reads look identical.

`box-set` semantics worth knowing: assigning a whole array to a scalar box
stores its element **count** (Perl array-in-scalar-context); assigning a
box created by `\` (is-ref) stores the reference value, not the wrapper.

### 2.3 Arrays

A Perl array is an **adjustable vector with a fill pointer** (growable,
ordered, integer-indexed). Elements are boxes (or `nil` for holes;
elements of proven-safe intermediate lists may be raw values). Negative
indices count from the end. Reading past the end yields `undef` without
growing; **writing** past the end extends the vector, filling the gap with
holes. `p-aref` (read) **unboxes scalar elements but returns reference
elements still boxed** (`p-aref-unbox-elem`) — so `==` on two references
compares object identity, not content. An array in numeric/scalar position
coerces to its length.

### 2.4 Hashes

A Perl hash is a string-keyed equality hash table. **All keys are
stringified on the way in** (`(to-string key)`). Values are boxes or raw
values; `p-gethash` unboxes scalars and preserves reference boxes, exactly
like `p-aref`. A hash in numeric position coerces to its key count.
`%ENV` is special-cased: the table holds a marker and reads/writes go to
the process environment.

### 2.5 References

`(p-backslash X)` — Perl `\X` — returns a **box with `is-ref` = t** whose
`value` is:
- for a scalar: the scalar's *box* (so writes through `$$r` hit the
  original — a box-in-box);
- for an array/hash/code/glob: the underlying vector/table/function
  itself;
- for a raw value (`\42`): a fresh box wrapping it (so `$$r += 1` works).

Dereference ops unwrap one level. Reference identity = identity of the
referenced structure. Stringification of a reference yields
`"HASH(0x…)"`-style text; numification yields the object address.

### 2.6 Blessed objects, strings, numbers

`bless` stores the class name in the reference box's `class` field
(`(p-bless (make-p-box (p-hash …)) "Class")`); `ref($x)` reads it. The
*identity* of the object is the underlying hash/array — two references to
it share blessing.

Strings are host Unicode strings (character, not byte, semantics — see
`docs/not-supported.md` §Unicode for divergences). Numbers are host
integers (arbitrary precision) and IEEE doubles; `*read-default-float-format*`
is double-float, i.e. every float literal in the output is a double.

## 3. Coercion — the heart of Perl semantics

Every op coerces its operands itself. The three canonical functions:

### 3.1 `to-number` (numification)

| input | result |
|---|---|
| number | itself |
| box | its value, numified (cached) |
| `undef`, `nil` | 0 |
| host boolean true | 1 |
| string | leading-numeric-prefix parse, below |
| array | length |
| hash | key count |
| anything else | 0 |

String parse (`parse-perl-number`): trim leading whitespace; accept
optional sign, then the longest prefix that reads as integer / float /
scientific-notation / `Inf`/`Infinity`/`NaN` (case-insensitive); ignore
everything after it (`"3rd"` → 3, `"3.14foo"` → 3.14); no numeric prefix
→ 0. (Real Perl warns on trailing garbage; PCL does not.)

### 3.2 `to-string` (stringification)

Strings pass through; boxes stringify their value (cached); `undef` → `""`;
integers print exactly; floats print in Perl's `%.15g`-equivalent shortest
form (`0.5` not `0.5d0`; integral floats print without `.0`); references →
`"TYPE(0xADDR)"`; blessed references → `"Class=TYPE(0xADDR)"`.

### 3.3 `p-true-p` (truthiness)

False: the number 0 (but **NaN is true**), the strings `""` and `"0"`,
`undef`, `nil`, empty array, empty hash. Everything else is true —
including `"0.0"`, `"00"`, `" "`, and all references. Boxes test their
value. Blessed objects consult `use overload 'bool'` first.

### 3.4 What ops return

- **Numeric/string operators** (`p-+`, `p-.`, …) return **raw** host
  numbers/strings — never boxes. This is what makes raw slots sound.
- **Comparison operators** return Perl booleans: `1` (number) for true,
  `""` (empty string) for false — *not* host booleans. `p-<=>`/`p-cmp`
  return -1/0/1.
- **Logical operators** `p-&&`, `p-||`, `p-//` return the deciding
  *operand's value* (Perl's `$a || $default` idiom): `p-&&` returns the
  first operand if false, else the second; `p-||` symmetrically; `p-//`
  keys on definedness. They are macros — the second operand is not
  evaluated unless needed.
- **Overload protocol:** every arithmetic/string/compare op first checks
  whether a blessed operand's class declares `use overload` for that
  operator and dispatches to it (`p-find-overload`/`p-call-overload`).
  Translators supporting objects must preserve this hook order:
  left operand, then right (with the swapped-args flag).

## 4. Context (scalar / list / void)

The dynamic variable `*wantarray*` carries the calling context:

| value | meaning |
|---|---|
| `t` | list context |
| `nil` | scalar context |
| `:void` | void context |

**Call sites bind it** where the callee is context-sensitive:
`(let ((*wantarray* t)) (pl-f …))`. Statement position emits no bind
(void by convention); `return EXPR` and sub-tail positions inherit the
frame's context. `p-sub` snapshots the value at entry into
`*pcl-caller-wantarray*` so nested binds inside the body don't lie to
`(p-wantarray)`, which maps t→`1`, nil→`""`, :void→`undef`.

For a translator: this is a hidden argument threaded through every call,
defaulting to "inherit". Context-sensitivity is *observable* — `wantarray`,
list-vs-scalar returns, `=~` in list context returning captures — so it
cannot be erased statically in general.

## 5. Calling convention

### 5.1 Definition

```lisp
(p-sub pl-NAME LAMBDA-LIST body…)
```

registers the function (visible to compile-time `BEGIN` code — the whole
form is inside `eval-when`) and wraps the body so that at **every call**:
1. the caller's package and sub-name are pushed on stacks (for
   `caller()`),
2. `*pcl-current-package*` becomes the sub's home package,
3. `*pcl-caller-wantarray*` snapshots `*wantarray*`,
4. the body runs inside `(catch :p-return …)` — the sub's return frame.

`(p-declare-sub pl-NAME)` is a forward stub so earlier code can reference
the name; calling a stub returns `nil` (it is always overwritten by the
real `p-sub` before a semantically-valid program calls it).

### 5.2 Arguments — two body shapes

**General shape** (body may use `@_`):

```lisp
(p-sub pl-f (&rest %_args)
  (p-args-body            ; binds @_ = (p-flatten-args %_args)
    …body…))
```

**Fast path** (body provably never touches `@_`; emitted for
`my ($a,$b) = @_;` prologues and coalesced leading `my $x = shift;` runs):

```lisp
(p-sub pl-f (&optional ($a (p-undef)) ($b (p-undef)) &rest %_args)
  (declare (ignore %_args) (dynamic-extent %_args))
  …body…)
```

Missing arguments are `undef`; extra arguments are silently ignored (they
sit in the unused rest list). The two shapes are call-compatible — every
call site just applies the function to the flattened values.

**Flattening:** Perl has no argument structure — at every call, array and
hash arguments splice into one flat value list (`p-flatten-args`). A hash
flattens to key, value, key, value…

**No aliasing:** `@_` holds copies; writing `$_[0]` does *not* modify the
caller's variable (deliberate divergence, `docs/not-supported.md`).

### 5.3 Return

`(p-return V…)` throws to the nearest `:p-return` catch — normally the
sub frame, but `eval { }` installs its own (§6.3), matching Perl. On the
way out `p-return-value` adjusts the value:
- scalar box → unboxed value (blessed boxes stay boxed to keep the class);
- array value in **scalar** context → its element count;
- `undef`/empty in **list** context → empty list;
- multiple values `return (5,3,1)` → the list in list context, the *last*
  element in scalar context.

A sub body falling off the end returns its last evaluated form (the
`catch`'s value).

## 6. Control flow

### 6.1 Conditionals

`(p-if COND THEN [ELSE])`, `(p-unless COND THEN [ELSE])` — plain
conditionals over `p-true-p` (§3.3). Ternary `?:` is also `p-if`.

### 6.2 Loops and loop control

`p-while`, `p-until`, `p-for` (C-style: `(p-for (INIT) (COND) (STEP)
body…)`), `p-foreach ((VAR LIST) body…)`. All accept trailing keys
`:label NAME` (must be first key) and `:continue (progn …)`.

The compiled shape per iteration: an outer named block (the loop's exit
target), a `tagbody` with a `:next` label (the continue target). Loop
control:

| Perl | unlabeled | labeled |
|---|---|---|
| `last` | exit the innermost loop block | throw to tag `LAST-<label>` |
| `next` | jump to `:next` (runs `:continue` form) | throw to `NEXT-<label>` |
| `redo` | jump to `:redo` (body restart, no cond/step) | throw to `REDO-<label>` |

Labeled control uses dynamic tags (catch/throw), so `last LABEL` works
across function boundaries (Test::More's `skip` relies on this). A bare
block `{ … }` is a loop-that-runs-once: same shape, so `last` inside it
exits the block.

`p-foreach` evaluates its list in list context, flattens it, and binds VAR
to **each element's box** (`ensure-boxed`) — so mutating the loop variable
writes through to the array, matching Perl's foreach aliasing (raw
elements get a fresh box; files where that aliasing would be observable
gate to the v1 pipeline).

### 6.3 Exceptions: `die` / `eval { }` / `$@`

`die` signals a `p-exception` carrying either a string or an arbitrary
Perl value (object exceptions). **PCL does not append " at FILE line N."**
to string messages (documented divergence).

`eval { body }` compiles to `p-eval-block`:
1. installs its own `:p-return` catch (Perl: `return` inside `eval{}`
   exits the eval, not the enclosing sub);
2. runs the body; on success sets `$@` to `""` and yields the body value;
3. on `p-exception` sets `$@` to the payload (string or object) and
   yields `nil`; any other host error is stringified into `$@`.

`$@` is an ordinary global box. `die` with no arguments re-raises `$@`.
`$SIG{__WARN__}` fires on `warn`; `$SIG{__DIE__}` does **not** fire
(documented divergence).

### 6.4 goto

`goto &sub` (tail call) is supported — it re-dispatches with the current
`@_`. Computed `goto LABEL` is a no-op (documented divergence); intra-sub
`goto LABEL` is partial.

## 7. Packages, variables, and OO

### 7.1 Namespaces and case

Each Perl package is a host namespace. Because CL uppercases unquoted
symbols, PCL uses the reader's `:invert` convention: single-segment
package names are stored upcased, and the original spelling is registered
via `(p-register-pkg-name :Animal "Animal")` — string-level lookups
(method dispatch, `ref`) go through that registry. Multi-segment names
(`File::Basename`) keep their exact spelling as `|File::Basename|`.
A qualified Perl variable `$Foo::x` is the symbol `$x` in namespace `FOO`.

### 7.2 Package variables and `local`

Package vars are globally-registered boxes (`defvar`). `local` saves the
box's current **value** and restores it on scope exit (dynamic, not
lexical) — including `local` on hash/array elements and typeglobs, via
dedicated `p-local-*` macros. Restore also invalidates the box caches.

### 7.3 Method dispatch

- Method names travel as **strings**. `(p-method-call OBJ "name" ARGS…)`:
  flatten args; find the invocant's class (the box `class` field for an
  object, the package-name string for `Class->method`); walk the class's
  **C3 linearization** (backed by CLOS proxy classes `plc-*`, one per
  package, whose superclass edges mirror `@ISA`); the first package
  defining the method wins. Method-name string → function mapping goes
  through the `%pcl-cl-sub-name` registry (case-preserving).
- `SUPER::name` dispatches starting *after* the current sub's home
  package in the linearization. `$obj->$coderef(@a)` calls the code ref
  directly with the invocant prepended.
- `AUTOLOAD` is honored (walks `@ISA`, skips `DESTROY`). `can`/`isa` work.
  `DESTROY` is **never called by GC** (documented divergence).
- PCL always linearizes with C3 (stock Perl defaults to DFS; documented
  divergence — `docs/not-supported.md` §mro).

### 7.4 Scheduled blocks

`BEGIN`/`CHECK`/`INIT`/`END` compile to definitions-bucket forms that run
at their Perl-mandated points relative to the *file's* load: `BEGIN`
bodies execute as soon as their form loads (before later statements),
`END` blocks run at program exit in LIFO order.

## 8. Magic globals

All are dynamically-scoped boxes exported from the runtime namespace:

| var | semantics |
|---|---|
| `$_` | default topic; ops with an omitted operand read/write it |
| `@_` | current sub's args (lexical per `p-args-body`, §5.2) |
| `$@` | last eval error (§6.3) |
| `$1`…`$N`, `%+` | capture groups; set by the most recent successful match (`p-=~` family); dynamically saved/restored around scopes like Perl |
| `$0`, `@ARGV`, `%ENV` | program name, args, environment (`%ENV` writes through to the process) |
| `$!` | last OS error (dualvar: numifies to errno, stringifies to message) |
| `$.` | line number of the last-read filehandle (per-handle) |
| `$a`, `$b` | sort comparator operands (per-package defvars) |

Regex match state is *global-with-dynamic-save*, exactly Perl: a failed
match leaves `$1` from the previous successful match intact.

## 9. The load model and string eval

Generated files are loaded form-by-form; a `use`/`require` triggers
transpilation (or cache lookup) of the target module and loads it inline,
recursively. `eval "string"` calls the transpiler *at runtime* on the
string, with an alist of the call site's lexical boxes passed in so the
eval'd code reads/writes the enclosing scope's `my` variables
(`docs/eval-lexical-capture.md`); calling context does not propagate into
string eval (documented divergence). Translators targeting environments
without a runtime compiler must either bundle one or reject `eval EXPR`
programs — there is no static escape.

## 10. Op inventory — family rules

The full inventory is the export list of the runtime namespace (~500
symbols; `grep '#:p-' cl/pcl-runtime.lisp`). You do not need each one
individually: every op follows its family's rule, and each `p-*`
function's docstring states its Perl contract. The families:

| family | members (representative) | rule |
|---|---|---|
| numeric ops | `p-+ p-- p-* p-/ p-% p-** p-<< p->> p-& p-\| p-^` | numify operands (§3.1), return raw number; overload hook first; `/` yields a double when inexact; `%` follows Perl sign rules; bitwise ops truncate to integer (Inf→0) |
| numeric compare | `p-== p-!= p-< p-> p-<= p->= p-<=>` | numify; return `1`/`""` (`<=>` −1/0/1; NaN comparisons → `""`/undef) |
| string ops | `p-. p-x p-lc p-uc p-lcfirst p-ucfirst p-length p-substr p-index p-reverse p-sprintf p-join` | stringify operands (§3.2), return raw string; `$_`-defaulting members read `$_` when no arg |
| string compare | `p-eq p-ne p-lt p-gt p-le p-ge p-cmp` | stringify; return `1`/`""` |
| logical | `p-&& p-\|\| p-// p-! p-not` | short-circuit macros returning operand values (§3.4) |
| assignment | `p-my-=` (boxed lexical) `p-scalar-=` (package) `setf` (raw slot) `p-array-= p-hash-= p-list-=` | store per §2.2; list-assign in scalar context yields the RHS element count; all return the assigned target/value per Perl |
| increment | `p-++ p---- p-++-post p----post` | numeric ±1 on the box/slot; `p-++` on a pure-alpha string does Perl string increment (`"az"→"ba"`) |
| elements | `p-aref p-gethash` (read) `(setf p-aref/p-gethash)` / `p-setf` (write) `p-exists p-delete p-aslice p-hslice` | reads unbox scalars, keep reference boxes (§2.3–2.4); writes through `p-setf` autovivify intermediate refs; `p-delete` returns the removed value |
| array/hash builtins | `p-push p-pop p-shift p-unshift p-splice p-keys p-values p-each p-sort p-map p-grep p-wantarray p-scalar p-defined` | Perl signatures; `p-sort` default is string order, comparator lambda gets `$a`/`$b`; `p-defined` returns `1`/`""` |
| regex | `p-=~ p-!~` with `(p-regex "/pat/flags") (p-subst …) (p-tr …)` | match/substitute/transliterate against a box (writes back for s///, tr///); sets §8 match state; list context returns captures; `p-split` |
| I/O | `p-print p-say p-printf` (`:fh HANDLE` key) `p-open p-close p-readline p-eof p-binmode …` | Perl builtins; bareword handles are symbols; `p-open` boxes its handle argument |
| introspection | `p-ref p-bless p-caller p-can p-isa` | §7; `p-caller` returns package but file/line are stubs (divergence) |

Anything not covered: read the `p-NAME` docstring in
`cl/pcl-runtime.lisp` — by project rule the runtime implements *real Perl
semantics only*, so the function *is* the spec, and
`docs/not-supported.md` is the closed list of deliberate divergences.

## 11. What a translator may ignore

- Comments; `(declare …)`; `(dynamic-extent …)`.
- The box nv/sv caches (pure optimization).
- The preamble's `@INC`/path setup (environment bootstrap, not program).
- `eval-when` wrappers *if* the target has no phase split — execute in
  order; `BEGIN` ordering is preserved by form order alone.
- The per-section `$a`/`$b` defvars and duplicate `defvar`s (idempotent).
- `p-double-inf` — the single SBCL-specific symbol, meaning IEEE ±Inf.

## 12. Worked example

Perl:

```perl
sub greet { my ($name) = @_; return "hi " . $name; }
my @who = ("ann", "bob");
foreach my $w (@who) { print greet($w), "\n"; }
```

Generated CL (abridged):

```lisp
(p-sub pl-greet (&optional ($name (p-undef)) &rest %_args)
  (declare (ignore %_args) (dynamic-extent %_args))
  (block nil (p-return (p-. "hi " $name))))

(let ((@who (make-array 0 :adjustable t :fill-pointer 0)))
  (p-array-= @who (vector "ann" "bob"))
  (p-foreach ($w @who)
    (p-print (let ((*wantarray* t)) (pl-greet $w)) "
")))
```

Faithful JavaScript-flavored translation, applying this manual:

```js
// §5.2 fast path: params pre-bound, missing → undef; §5.3 catch frame
function pl_greet(ctx, $name = UNDEF) {          // ctx = *wantarray* (§4)
  try { throw new PReturn(p_concat("hi ", $name)); }   // p-. → §3.2 stringify
  catch (e) { if (e instanceof PReturn) return e.value; throw e; }
}
let who = new PArray(["ann", "bob"]);            // §2.3
for (const w of who.elementBoxes()) {            // §6.2 foreach binds boxes
  p_print(pl_greet(LIST_CTX, w.get()), "\n");    // call site binds context
}
```

(A real translator would elide the `PReturn` frame when the body has one
tail return — the same optimization PCL itself applies in reverse.)
