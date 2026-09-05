# PCL IR Manual — semantics of the generated Common Lisp

**What this is.** The normative description of what PCL's output *means*.
Read it to translate that output to another target, to build tooling on it,
or to debug it — you should be able to reimplement the semantics from this
document without reading any Lisp.

| | |
|---|---|
| **Status** | normative.  Where this document and the runtime disagree, [`cl/pcl-runtime.lisp`](../cl/pcl-runtime.lisp) is the reference implementation and *this document has a bug* — please fix it. |
| **Companion** | [`generated-cl-ir-review.md`](generated-cl-ir-review.md) — what the output *looks like*, its friction points and the roadmap.  This manual covers what the constructs *mean*. |
| **Verified against** | every claim verified at cache generation v2-7 (2026-07-06), maintained per-change since; last section-level review at v2-44 (2026-07-20).  Section headings name the defining runtime function, so any claim can be re-checked at the source. |

Sections marked *(normative, sNNN)* were added or revised by a specific
design ruling; `sNNN` names an internal working session.

## Contents

* [0. The one-paragraph model](#0-the-one-paragraph-model) — start here
* [1. Reading the output](#1-reading-the-output) — file shape, naming conventions
* [2. The data model](#2-the-data-model) — [undef](#21-undef) · [scalars and raw slots](#22-scalars-boxes-and-raw-slots) · [tied scalars](#22b-tied-scalars--the-raw-slot-behind-the-magic) · [arrays](#23-arrays) · [hashes](#24-hashes) · [references](#25-references) · [blessed objects](#26-blessed-objects-strings-numbers)
* [2b. Declarations, scoping, and the rename families](#2b-declarations-scoping-and-the-rename-families) — [the tension](#2b1-the-fundamental-tension) · [declaration forms](#2b2-the-declaration-forms) · [rename families](#2b3-the-rename-families) · [guard rails](#2b4-the-guard-rails-when-renaming-refuses)
* [3. Coercion](#3-coercion--the-heart-of-perl-semantics) — [numification](#31-to-number-numification) · [stringification](#32-to-string-stringification) · [interpolation extent](#32b-interpolation-extent--which-text-belongs-to-a--reference-inside-a-dq-string-regex-or-heredoc-normative-s426) · [truthiness](#33-p-true-p-truthiness) · [what ops return](#34-what-ops-return)
* [4. Context (scalar / list / void)](#4-context-scalar--list--void)
* [5. Calling convention](#5-calling-convention) — [definition](#51-definition) · [arguments](#52-arguments--two-body-shapes) · [return](#53-return) · [comparator frames](#54-comparator-frames--p-sort-cmp)
* [6. Control flow](#6-control-flow) — [conditionals](#61-conditionals) · [loops](#62-loops-and-loop-control) · [exceptions](#63-exceptions-die--eval----) · [goto](#64-goto)
* [7. Packages, variables, and OO](#7-packages-variables-and-oo) — [namespaces and case](#71-namespaces-and-case) · [package variables and `local`](#72-package-variables-and-local) · [method dispatch](#73-method-dispatch) · [scheduled blocks](#74-scheduled-blocks) · [bareword filehandles](#75-bareword-filehandle-names-normative-s443f) · [stdio buffering](#76-stdio-buffering-normative-s451)
* [8. Magic globals](#8-magic-globals)
* [9. The load model and string eval](#9-the-load-model-and-string-eval) — [the eval protocol](#91-the-string-eval-protocol-normative-s295) · [the generation stamp](#92-the-generation-stamp-is-a-promise-normative-s402) · [the drop form](#93-the-drop-form-a-statement-the-compiler-could-not-lower-normative-s435)
* [10. Op inventory — family rules](#10-op-inventory--family-rules)
* [11. What a translator may ignore](#11-what-a-translator-may-ignore)
* [12. Worked example](#12-worked-example)

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
  humans — **with one exception, and a reserved second one.**  The exception
  is the first line, the pipeline marker (below, §9.2): a comment with a fixed
  grammar that tooling reads and that the compiler promises.  The reserved one
  is the **statement source map**, `;; @ FILE:LINE` before a statement's forms
  (task #1035 step 4, DECIDED s469bg, not emitted yet).  The grammar is a
  comment rather than a `(p-line N)` marker form for two measured reasons: a
  marker is a real form the host compiler has to see and delete, and in TAIL
  position it would change a block's value, whereas a comment costs the reader
  0.4 ms on the corpus's largest file even at one marker per LINE (measured
  s469bg) and cannot change what is evaluated.  A consumer may drop every
  comment and still read a correct program; what it must not do is invent
  meaning for a free-text echo, which stays free text.
- **The tree is explicit.** Perl's implicit operands are materialized at
  parse time: `$_` defaults (§8), `@_`/`@ARGV` for bare `shift`/`pop`,
  filetest operands. What you see in the tree is the complete argument
  list — there is **no** runtime-side argument default (bare
  `print;`/`say;`/`printf;` arrive as `(p-print $_)` etc., materialized by
  the codegen like every other `$_`-default form).
- **`(declare …)` forms are host compiler advice** — droppable wholesale.
- **No host-implementation symbols appear in the tree** (normative, and
  cheaply checkable: `./pl2cl FILE | grep -ac 'sb-[a-z]*:'` is **0** over
  the whole corpus, measured s382g). The runtime is free to use SBCL
  internals — it does, extensively — but every such mechanism reaches the
  emitted code wrapped in a `p-*` macro, so a translator reimplements the
  vocabulary of §10 and never an implementation detail.
  **This is an invariant to hold, not just a fact about today**: it would
  be easy to lose. Direction D (task #289) declares each ordinary package
  global as a symbol macro over a directly-addressed global cell, which in
  its natural spelling would put `sb-ext:symbol-global-value` at *every
  global declaration and every `local`* — thousands of sites. It does not,
  because both halves are runtime macros (`p-defcell`, `p-local-cell`) and
  the emitted forms stay `(p-defcell $x (make-p-box nil))` /
  `(p-local-cell $x INIT …)`. A port then owes two macro definitions
  instead of a new convention at every declaration. Any future change that
  needs a host primitive in emitted code should reach for the same shape.
- **Order is the program.** A file is a sequence of top-level forms
  executed in order by `load`. There is no linker step: a name must be
  defined by an earlier form (or a forward declaration) before a later
  form *executes* it. `(eval-when (:compile-toplevel :load-toplevel
  :execute) …)` marks forms that Perl requires visible to `BEGIN` blocks
  earlier in the same file; a translator with no compile/load phase split
  simply executes them in order — but see §9 for `BEGIN` itself.
- **The first line is the pipeline marker** — `;;; pcl: pipeline=v2
  gen=v2-NN` — a comment stamping which pipeline and cache generation
  emitted the file. Non-semantic like all comments, but it is the
  version key tooling should read, and it is a **promise**, not an
  incidental: see §9.2.
- **File layout:** preamble (`in-package`, `@INC` setup) → per-package
  *sections* in source order, each: package preamble → declarations
  (`p-declare-sub`, `defvar`) → definitions → runtime (top-level
  statements). Within the definitions stream, `p-sub` forms and
  scheduled blocks (`p-BEGIN`/`p-END`/…) are **interleaved by source
  position** (s300b, #55): a `BEGIN` form loads after exactly the subs
  defined above it in source and before those below — this ordering is a
  semantic guarantee (compile-time sub-existence introspection relies on
  it), not a formatting choice. A `package` statement mid-file starts a
  new section; a reopened package gets only `(in-package …)` +
  `p-set-current-package`.
- **A `package X;` INSIDE A BLOCK is not expressed by the emitted
  `in-package`** (#239, s378). A block lowers to ONE top-level form, and
  the CL reader interns every symbol of a top-level form before it is
  evaluated, so a nested `(in-package :X)` cannot re-home the symbols
  around it. Perl's switch is nonetheless lexical and covers the rest of
  the block — nested blocks and nested sub bodies included. The compiler
  therefore **spells the region's variables package-qualified at the
  source level** before lowering (`Pl/Parser2.pm`
  `_requalify_block_globals_after_pkg_switch`): `$z` → `$X::z`,
  `@a` → `@X::a`, `$a[0]` → `$X::a[0]`, `$#a` → `$#X::a`, and the same in
  interpolating text. A consumer reading the emitted CL therefore sees
  explicit `X::$z` symbols, never a bare name whose package depends on an
  enclosing `in-package`. Lexicals, in-scope `our` aliases (which resolve
  to their DECLARING package), already-qualified names and the always-main
  specials are left where they are; `$a`/`$b` are excluded because the sort
  lowering lexically binds those two symbols. Sub definitions, `*glob`
  installs and bareword calls in such a region never had the problem —
  they resolve through the package stack at lowering time.

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
| package var | `(p-defcell $g (make-p-box nil))` — `defvar` for the exception set (§2b.1) | `(p-scalar-= $g V)` | `$g` |

A **coercing compound assignment** on a raw slot (`$n += V` and the whole
`-= *= /= %= **= x= .= <<= >>= &= |= ^= &.= |.= ^.=` family — every op
whose stored value is an operator result, so a raw number/string by
construction) lowers to the boxed macro's **`-raw` twin**:
`(p-incf-raw $n V)`, `(p-.=-raw $s V)`, … — each expands to
`(setf slot NEW)` with the *same* new-value form its boxed macro
(`p-incf`, `p-.=`, …) computes, so the two store disciplines cannot
diverge semantically (task #62; `docs/raw-numeric-verdict.md`).  The
non-coercing `||= &&= //=` store the RHS unchanged (it may be a
reference), so their targets never become raw slots.  A statement-root
`$x++;`/`$x--;` likewise lowers to `(p-incf-raw $x)`/`(p-decf-raw $x)`
when every other write to `$x` is numeric-valued (the A-num rule —
magical string increment is then unreachable); a tail-position postfix
wraps in `prog1` to return the old value.

**Invariant: a raw slot holds a host number or string** — never a box or a
reference — **with one exception, the freeze-licensed slot's runtime
decline described below.** Ops always accept either form (they unbox
internally), so reads look identical.

A raw slot may also be **freeze-licensed** (the B-regime, s303, task #62):
when every USE of the variable is provably numeric (resp. string/boolean)
but a write's value shape is unproven (`my $n = $h{k}`, `$x = $y`, a bare
sub call), the slot stays raw and every native write routes through
`(%pcl-to-number-strict V "$n")` / `(%pcl-to-string-strict V "$n")` — an
eager coercion that applies box-set's aggregate scalar-context collapse
and then freezes the value to a plain host number/string.  (The second
argument names the slot in the IR; the runtime does not read it.)

**The freeze DECLINES when the arriving value carries per-use behavior**
— its class overloads *any* operator, or it is a genuine dualvar.  Then
the coercer stores `(p-box-init V)` instead: a fresh box under box-set's
own assignment rules, i.e. **exactly what the general-form (boxed)
compiler would have stored for that write**.  That is the one exception to
the raw-slot invariant above, and it is what keeps the optimized emission
running identically to `PCL_OPT=none` — a translator therefore has to make
every raw-slot read box-tolerant, which it already is, since all reads go
through ordinary `p-*` ops.  A decline is silent and value-correct, not an
error: the compile-time licence is a textual scan of *this file* for `use
overload`, so an overloaded object arriving from a module, a container, or
a string `eval` is invisible to it, and only the runtime can see it (task
#890).  Semantics and the full licensing/disqualifier tables:
`docs/raw-numeric-verdict.md`.

A freeze-licensed slot whose only writes are plain roots + `.=` and whose
every use is a *transient* stringify/boolean read may further become a
**str-buffer** (S1): the slot holds an adjustable fill-pointer string,
plain writes replace it via `(%pcl-str-buffer V)`, and `.=` appends in
place via `(%pcl-str-append $s V)` — O(1) amortized instead of a fresh
concatenation per append.  **Consumer contract: the buffer object never
crosses the IR boundary.**  Every escape channel — sub return, call
argument, store into a box/container, package var, hash key (the table
retains the key object) — is an opaque/retaining use that disqualifies
the verdict, so host code calling PCL-generated CL only ever receives
ordinary simple strings; and even internally, every standard string
operation respects the fill pointer.

`box-set` semantics worth knowing: assigning a whole array to a scalar box
stores its element **count** (Perl array-in-scalar-context); assigning a
box created by `\` (is-ref) stores the reference value, not the wrapper.

### 2.2b Tied scalars — the raw slot behind the magic

`tie $x, 'Class'` replaces the box's `value` with a **`p-tie-proxy`**
holding the tie object and the box's previous value.  Every read
chokepoint (`unbox`, `box-nv`, `box-sv`, the invocant resolution in
`p-method-call`) dispatches to `FETCH`; every write chokepoint
(`box-set`, `p-scalar-=`) dispatches to `STORE`.

The proxy's `saved-value` is not merely a restore-on-untie snapshot — it
**is the SV's raw slot**, and the following rules make it behave like
perl's (mg.c).  A port that models tie must reproduce all four:

1. **A write hits the raw slot first, then calls `STORE`** (perl's
   `sv_setsv` + `mg_set` order), so `untie` after `$tied = "A"` leaves
   `"A"` behind.
2. **`FETCH`'s result is written back to the raw slot** (`magic_methpack`
   ends in `sv_setsv(sv, result)`), so `untie` after a read leaves the
   fetched value.
3. **While a handler for a cell is on the stack, that cell's magic is
   OFF** (`save_magic`/`restore_magic`): reads and writes of the tied
   variable *inside its own* `FETCH`/`STORE` hit the raw slot instead of
   re-entering the handler.  This is not an optimization — Math::BigInt's
   `sub STORE { $rnd_mode = (ref $_[0])->round_mode($_[1]) }` assigns to
   the very variable it proxies, and without suppression it recurses
   until the stack dies (task #224).  Suppression is **per cell** (a
   second tied variable still dispatches from inside), ends with the
   handler including on a non-local exit, and does **not** hide the tie
   from `tied()`, which still reports the object.
4. **Re-tying replaces the magic, it does not stack**: a second `tie` on
   an already-tied cell carries the existing raw slot forward rather than
   saving the old proxy as the new "value".

PCL implements 1–4 by swapping the raw value into the box for the
duration of the handler call (`%with-tie-magic-off`), so no read/write
site needs a special case; only `tied`/`untie` consult the suppression
list.

### 2.3 Arrays

A Perl array is an **adjustable vector with a fill pointer** (growable,
ordered, integer-indexed).

**Element storage: RAW by default, promoted to a box at an alias event**
(s457ai, `docs/boxed-aggregates-design-s455.md`).  A slot holds one of three
things:

| slot content | meaning |
|---|---|
| `nil` | a HOLE — deleted, or never assigned.  **`nil` is the existence test**; a box has nothing to do with whether an element exists. |
| a raw value (a number, or a string with no cached numeric half) | a live element nobody has taken an alias to |
| a `p-box` | a live element that carries IDENTITY — someone may write through it or observe writes to it |

The whole model rests on ONE **write rule**, obeyed by every store:

> *a slot that holds a BOX is written THROUGH it (`box-set`), never replaced;
> a slot that holds a raw value or a hole takes the new value raw when the
> value is raw-storable, and otherwise gets a fresh box.*

"Raw-storable" (`%p-storable-raw`) is a plain number, or a plain string with
no cached numeric half.  Everything else carries identity on the CONTAINER
rather than in the value — a bless class, the `is-ref` flag (`\$x`, `\*foo`),
a dualvar's two halves, a magic cell, a tie proxy, a box-in-box scalar ref —
and therefore keeps its box.  This is the same split the reading side already
makes, which is why **no read path changes**: `p-aref` (read) unboxes scalar
elements and returns reference elements still boxed (`p-aref-unbox-elem`), and
a raw slot passes through it unchanged.

**Promotion** (`%p-elem-cell`) turns a raw slot into a box IN PLACE at the
moment something needs the element's identity — an alias event.  The complete
list of alias events: `@_` aliasing of one element (`f($a[0])`) or of every
element (`f(@a)`), a `foreach`/`map`/`grep` loop variable, `\$a[0]`, `local
$a[0]`, `values`, a slice, `%h` or a hash-assignment result in list context.
Promotion is **monotone** — nothing ever demotes a slot — which is what keeps
an alias taken at any time live for as long as the element exists.  Copying a
container (`@b = @a`, `push`, `sort`, list assignment) reads VALUES and never
shares slots, so a copy breaks aliasing, exactly as in perl.

A port may store elements however it likes, but it must be able to answer
"give me a stable cell for element *i*" and it must not treat "has a cell" as
"exists".  `pcl:*p-raw-elems*` (the `PCL_RAW_ELEMS` environment variable) turns
raw storage off and restores an all-boxed world; both settings are correct and
may be mixed over one container, because a boxed slot is always legal.

Negative indices count from the end. Reading past the end yields `undef`
without growing; **writing** past the end extends the vector, filling the gap
with holes.  Growing an array by **assigning to `$#a`** (`p-set-array-length`,
incl. the `$#a++` idiom) likewise fills the new slots with holes (`nil`),
never with fresh boxes — `exists $a[$i]` on the new slots must stay false
(s295; a port that models holes as "slot absent" gets this for free). An array
in numeric/scalar position coerces to its length.

**Read-only arrays (s337, task #159):** the ONE case where the storage is not
adjustable.  `Internals::SvREADONLY(@a, 1)` replaces the variable's storage
with a **simple vector** — same element boxes, no fill pointer, not adjustable
— because perl's read-only AV is precisely a *fixed-size* array whose elements
remain writable.  The predicate is therefore the storage itself
(`%p-array-readonly-p` = a non-string vector with no fill pointer), and every
size-changing entry point (push/unshift/pop/shift/splice/delete, whole-array
assignment, `undef @a`, the out-of-bounds extend inside element writes, and
growth via `$#a`) checks it and raises perl's `Modification of a read-only
value attempted`.  A port that has no equivalent of "fixed-size vector" needs
an explicit per-array flag consulted at those same points; nothing else in this
spec changes.

**Hole aliasing (defelem, s316e):** when a hole slot is *aliased* — by a
foreach/grep/map `$_` binding or by spreading the array into `@_` — the
alias is a **deferred-element box** (`%p-defelem-box`): a box whose value
is a `p-magic-cell` of `:kind :defelem`, getter → `undef`, setter →
de-magic the box, store it into the source slot, re-dispatch through
`box-set`.  Reads never vivify; the first write through the alias does,
exactly perl's defelem magic.  Consequences for a port: array slots
themselves never hold a still-magic defelem (the setter de-magics before
storing), but *flattened views* (`@_`, a foreach binding) can —
`p-exists-array` and `p-aref-unbox-elem` treat that state as a hole.  A
bare lexical `@array` as the sole foreach list is iterated **live** (no
copy): `push` during the loop extends the iteration, like perl.

### 2.4 Hashes

A Perl hash is a string-keyed equality hash table. **All keys are
stringified on the way in** (`(to-string key)`). A hash in numeric position
coerces to its key count.  `%ENV` is special-cased: the table holds a marker
and reads/writes go to the process environment.

**Values follow §2.3's element model exactly** — RAW by default, promoted to a
box in place (`%p-hash-elem-cell`) at an alias event, the same write rule, the
same monotone promotion.  The array's `nil` hole has no hash counterpart: an
absent KEY is the hash's hole, and its lazy alias is `%p-hash-defelem-box`,
whose reads look the key up live (staying non-`exists`) and whose first write
creates it.  `p-gethash` unboxes scalars and preserves reference boxes, exactly
like `p-aref`, so a raw value passes through unchanged.  KEYS are always copies
— perl's hash keys are read-only — so only the VALUE half of `values %h`,
`%h` in list context, `@h{…}`, `%h{…}` and a hash-assignment's list-context
result is an alias.

**BLESSING DOES NOT CHANGE ELEMENT ALIASING** (normative, s458ak, task #841).
A blessed container is an ordinary container with a stash attached, and its
ELEMENTS are ordinary lvalues: `w(@$obj{"k"})` and
`for my $v (@$obj{'a','b'}) { $v .= "!" }` write through, exactly as they do
for `{k=>…}`.  PCL records a hash's class in the reserved key `:__class__`,
which is a CL keyword — no Perl-level key can name it, because every key
arrives through `to-string` — so the class needs no guard at the aliasing
sites and must not get one.  (A blessed ARRAY carries its class on the
enclosing box rather than in the vector, so the array side never had the
question.)  A port that keeps the class inside the container must make the
same argument about its own class slot before aliasing elements.

### 2.5 References

**`\` applied to a LIST is a LIST of references, one per element** (normative,
task #892). The distributing forms are `(p-refgen-list X)` — one ref per
element of the list value `X`, each aliasing the container's slot — and, for a
multi-term `\( … )`, a `(vector …)` of per-term refs with the spreading terms
looped in. What SPREADS is decided by the term, not by whether parens were
written: **a SLICE** (`\@A[0,1]`, `\@h{…}`, and the kv spellings `\%h{…}` /
`\%a[…]`) and **the range operator** (`\(1..3)`). What does NOT spread is an
array or hash VARIABLE: perl's special case spreads `\(@foo)` only when the
parenthesized list is exactly that one aggregate, so `\@A` and `\(@A,$x)` are
one ARRAY ref (plus, there, one scalar ref) — `Pl::ExprToCL::_is_refgen_spread_node`
is the single predicate for the first question and `_is_list_node_for_refgen`
for the second. In explicit scalar/void context a distributing form is the
comma operator: `my $s = \@A[0,1]` is `\$A[1]`.

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

**The printed type and the address are properties of the REFERENT, never of
the wrapper** (normative, task #163). `\` allocates a fresh wrapper on every
evaluation, so any rule that reads the wrapper makes `\$x == \$x` false and
prints two addresses for one variable. `%p-ref-referent` is the one rule that
resolves the referent, and `is-ref` on the wrapper is its only discriminator:

- the value handed to a stringifier is **either the wrapper itself** — `\$x`
  reaching `print`, an array/hash element, or a raw (`p-raw-params`) parameter
  — **or a variable box holding one** (`my $r = \$x`).  `is-ref` says which;
  counting box levels cannot, and a level-counting box-sv is exactly why the
  same reference used to print `SCALAR` through a variable and `REF` straight
  into `print`.
- `p-ref`, `box-sv`, `stringify-value` and `box-nv` all read that one rule, so
  the word and the number agree no matter which path a reference took.
- **`SCALAR` vs `REF` is dynamic**: it is decided by what the referent holds
  *at that moment*, exactly as perl does. `my $r = \1; my $rr = \$r;` prints
  `REF`; after `$r = 5` the same `$rr` prints `SCALAR`. A reference's string is
  therefore **not cached** on the holding box — the referent is a different box
  and writing to it cannot invalidate a cache there. (`box-nv` refuses to cache
  address-based NVs for the same reason.)
- A reference **to a plain scalar is not a container**: `@$sref`, `%$sref`,
  `$sref->{k}`, `$sref->[0]` are perl's fatal `Not a(n) HASH/ARRAY reference`.
  What PCL still tolerates is one *representation* layer (a box left over after
  one unwrap) — see `%p-scalar-referent-p`; that leniency exists only because
  the parser drops the outer level of `$$refref->{k}` (task #211), and goes
  away when that is fixed.
- **The invocant of a postfix `->` is ONE scalar value, whatever the group
  around it looks like** (normative, s443h/#516 + s448p/#527). A
  parenthesised base — `($r//0)->[i]`, `(1,2,$r)->[1]`, `(1,2,$h)->{k}`,
  `(1,2,$cr)->(…)`, `(1,2,$o)->m` — lowers in SCALAR context, so a
  multi-element group is the comma operator: **every element is evaluated,
  the LAST one is the reference**. It emits a `progn`, never a `(vector …)`.
  The look-alike `(LIST)[i]` — and its `qw(a b c)[i]` spelling — is a
  different operator, a LIST SLICE; PExpr marks both `list_ctx_subscript`
  and the array emitter keeps their base a list. **The DEREF family is the
  same base** (normative, s451y/#612): a sigil cast, a `$#`, and a slice
  container take ONE scalar value too, and each has two spellings that mean
  the same op — the postfix `EXPR->@*` / `->%*` / `->$*` / `->&*` / `->**` /
  `->$#*` / `->@[…]` / `->@{…}` / `->%[…]` / `->%{…}` and the prefix
  `@{ EXPR }` / `%{ EXPR }` / `${ EXPR }` / `$#{ EXPR }` / `@{ EXPR }[…]` —
  because `Pl::PExpr` lowers the postfix form ONTO the prefix one. The one
  way it differs from the four `->` members: a deref base is kept in its
  emitter's LVALUE context, because `@{ ($h{k}) } = (7,8)` AUTOVIVIFIES
  `$h{k}`, where an arrow invocant is read as a value.
  **The scalar context reaches through TRANSPARENT paren layers**
  (normative, s451y/#611): `((0,$h))->{k}` is `(0,$h)->{k}`, and any depth
  of nesting is the same value. Contexts are annotated top-down before
  emission (`PExpr::annotate_contexts`), and a comma group's children are
  annotated LIST unconditionally, so an emitted-time scalar context — the
  one the arrow's base gets — has to be pushed down as the group lowers:
  the LAST child only, which is the comma operator's value; the earlier
  ones are still evaluated, for their effects.
- **A TYPEGLOB is the one payload whose ref-ness lives on the box, not on the
  object** (normative, task #423). Perl distinguishes a glob *value*
  (`$g = *foo`, which turns the SV into a GV: `ref($g)` is `""`, `"$g"` is
  `*main::foo`, `0+$g` is 0, and `\$g` is a **GLOB** ref) from a glob
  *reference* (`$g = \*foo`: `ref($g)` is `GLOB`, `"$g"` is `GLOB(0x…)`,
  `0+$g` is the address, and `\$g` is **REF**). Both hold the same
  `p-typeglob` in the box, so **`is-ref` is the whole distinction** — every
  reader asks it (`box-sv`, `box-nv`, `p-ref`, `%p-ref-string`, via
  `%p-glob-value-box-p`), and every path that *copies* a scalar must carry it
  (`box-set`, `%p-flatten-list`, `%p-array-store-scalar`, `p-aref-unbox-elem`,
  `p-flatten-args`, `p-return-value`). A copy that drops the flag silently
  demotes a glob reference to a glob value; a copy that keeps the *source box*
  instead of a flag-carrying snapshot **aliases**, and `($g1,$g2) = ($g2,$g1)`
  collapses to one glob. A **raw** `p-typeglob` outside a box is a glob VALUE
  by the same convention `stringify-value` uses (#316).
- **That aliasing rule is not special to globs — it is the rule for EVERY
  payload that travels as a box** (normative, task #891). Perl evaluates the
  whole right-hand side of a list assignment before any store happens
  (`OPpASSIGN_COMMON`), so an RHS element that is a live box must be
  snapshotted: `%p-assign-snapshot` reduces a plain scalar to its inner VALUE
  (copy semantics), reads a magic cell or tie proxy *now*, and for anything
  whose facts live on the CONTAINER — a reference (inner is a box, a vector or
  a hash table), a blessed box, a typeglob, a dualvar — hands back a **fresh
  box with the same inner and the same container flags**
  (`%p-container-snapshot`: class, `is-ref`, the cached NV/SV). Handing over
  the live box instead makes `($x,$y) = ($y,$x)` read `$x` back *after* store 0
  overwrote it, so both names end up holding `$y`'s referent — silently, and
  only for references, which is why the plain-scalar swap looks correct. The
  REFERENT is untouched by the copy, so `==`, `refaddr` and the `TYPE(0x…)`
  string are unchanged: identity is the referent's, per the rule above. Both
  assignment families read this one rule — `p-list-=` through
  `%p-flatten-list`'s scalar arm, and `p-setf`'s array-slice / hash-slice arms
  through `%p-assign-snapshot-vector` (a slice hands out the container's own
  element boxes, #818).

**Ref identity is a monotonic id, deliberately NOT a machine address**
(`object-address`, `cl/pcl-runtime.lisp`). A translator must reproduce the
*invariants*, not the digits:

- **Stable for the object's lifetime.** SBCL's compacting GC relocates
  objects and PCL re-boxes refs on some paths, so the raw pointer is not an
  identity. The failure this was built for: a coderef threaded through
  Sub::Defer's coderef-keyed `%DEFERRED` presented two different
  `CODE(0x…)` strings for one logical sub, breaking Moo's lazy/subclass
  bootstrap.
- **Never reused.** Ids come from a monotonic counter, so two distinct
  objects can never share one — *stronger* than perl, which recycles freed
  addresses. Code that relies on address reuse is relying on a defect.
- **Non-leaking.** The object→id table is weak-on-key, so dead objects drop
  out.
- **Consistent between the two views.** `refaddr`, `==` on refs, and the
  `0x…` in stringification are the same number, which is what
  `t/op/bless.t`'s `expected()` checks (`hex($addr) == $ref+0`).

The numbers are therefore small and dense (`ARRAY(0x1)`), not
address-shaped, and that is correct rather than approximate. This is the
worked example of the general rule: **match Perl's shape and invariants, not
its bytes — and where Perl's bytes carry a defect, do better.** The
deliberate exception is a value a program can legitimately branch on, e.g.
the seeded `rand` sequence, where PCL reproduces perl's drand48 exactly
(`p-srand`/`p-rand`).

### 2.6 Blessed objects, strings, numbers

`bless` records the class on the thing all aliases share, mirroring
perl's referent-attached stash: for a hash object the class rides in the
hash itself (`:__class__` key); for a **scalar ref** it is written to the
*referent box* (s314, `%p-scalar-ref-referent`), and `ref($x)` /
`p-get-class` consult the referent first (`%p-referent-class`) before any
class cached on a wrapper or variable box. Those cached `class` slots
still exist — `p-bless` writes them and `box-set` copies them — but only
as caches for fast is-object checks; the referent is the truth, which is
what makes a second `\$x` wrapper and a re-bless through one alias
behave, and what XS reads as `SvSTASH(SvRV(rv))`. Array/code/glob refs
keep the class on the wrapper box (their raw referents cannot carry a
slot). `box-set` copies a class only when the assigned *value* is itself
a reference: copying a plain value out of a blessed referent yields an
unblessed scalar, exactly as perl's SV-attached stash does not travel
with the value. The *identity* of an object is the underlying
hash/array/referent — two references to it share blessing.

Strings are host Unicode strings (character, not byte, semantics — see
`docs/not-supported.md` §Unicode for divergences). Numbers are host
integers (arbitrary precision) and IEEE doubles; `*read-default-float-format*`
is double-float, i.e. every float literal in the output is a double.

## 2b. Declarations, scoping, and the rename families

How a Perl declaration becomes CL is the one place where the transpiler
does real *surgery* on the program: some variables are silently renamed to
fresh unique names. A consumer will meet symbols like `$tick__file__0` and
`$x__shadow__1` in the output; this section explains why they exist, what
each family means, and the rules that keep the surgery sound.

### 2b.1 The fundamental tension

Perl file/block lexicals (`my`) and CL top-level forms don't line up:

- In Perl, a **named sub sees file lexicals declared above it**, and
  `BEGIN` blocks can run code that touches variables mid-file.
- In PCL's output, named subs are **hoisted** into the definitions bucket
  (so calls work regardless of definition order), and each top-level form
  is loaded independently — a CL lexical `let` cannot span top-level
  forms, and a hoisted `defun` cannot see a `let` in the runtime bucket.

The **v1 pipeline** resolved this with a hammer: every `my` became a
global `defvar`'d box (dynamically scoped). That makes hoisted subs and
BEGIN blocks work, but it breaks real lexical semantics — `defvar`
*proclaims the symbol special globally*, so **every** later `let` of the
same name in the file silently becomes a dynamic binding: closures
capture one shared cell instead of per-instance cells, and a `my $x`
inside a `map` block writes through to the outer `$x`
(`docs/closure-lexical-scoping.md`). v1 patched the worst of this by
renaming *closure-captured* block lexicals to fresh `$x__lex__N` names
(fresh ⇒ never `defvar`'d ⇒ the `let` stays truly lexical).

The **v2 pipeline** (the default) inverts the model: a `my` is a true CL
lexical — `(let (($x INIT)) …rest-of-block…)` — and the hard cases are
handled by *renaming the variable to a fresh package-level cell* instead
of poisoning the name. The prime invariant, worth memorizing:

> **A let-bound name is never `defvar`'d.** (`_forward_global_decls`
> excludes every name in the cumulative lexical set `_all_lex`.)
> When a lexical must be globally visible after all, it gets a **fresh
> unique name** first, so the `defvar` cannot poison any other `let`.

Since s382h (task #289, "direction D") the *poisoning* half of that
tension is gone at the root, because **an ordinary package global is no
longer a special variable at all**. It is a symbol macro over its own
global value cell:

    (p-defcell $x (make-p-box nil))     ; ≈ (define-symbol-macro $x <cell>)
                                        ;   + initialize the cell once

Two consequences a consumer must know:

- A `let` of such a name is legal CL and installs an ordinary **lexical**
  binding — it does *not* rebind the global. That is precisely what Perl's
  `my $x` shadowing a package `$x` means, so the shadow no longer leaks
  into called subs.
- `local` therefore cannot be a `let`: it lowers to `p-local-cell`, which
  saves the cell, installs the new value, and restores under
  `unwind-protect` (§7.2).

**Exception set** — names that keep `defvar` and the dynamic `let`, decided
by name alone (`Pl::GlobalPartition`, the one authority both emitters ask):
everything not word-shaped (punctuation and caret magic — `$@`, `$1`, `$!`,
`|${^WARNING_BITS}|`, …) plus the runtime-owned word-shaped set
`$_ @_ %_args @ARGV $ARGV @ARGVOUT @INC %INC %ENV %SIG` and the sort pair
`$a`/`$b` in every package. For those a dynamic binding *is* the mechanism
(sort binds the pair; the magic vars are where `local` actually runs hot),
and mixing the two declarers on one symbol is a load-time error in SBCL —
loud, never silent.

**Punctuation names are `main::` everywhere** (perlvar: "forced to be in package
main").  The punctuation SCALARS and the regex arrays/hashes (`$_ $0 $! $, $\
@- @+ %+ %-` …) are runtime symbols exported from `:pcl`, so every user
package (which `(:use :pcl)`) reads the same symbol; since s440 (#498) the
punctuation ARRAYS `@? @! @. @/ @~ @^ @& @% @= @< @>` and the synthesized
`@#` are owned the same way — the compiler emits the bare name and declares
nothing, so `@?` written under `package A` and read under `package B` is one
array.  A per-package `(defvar @? …)` was the old emission and made them two.
Since s446j (#506) that ownership covers **every punctuation container perl
allows, hash and array alike** — measured character by character on 5.40.3,
which takes a punctuation name for every one of these:

```
" $ % & ' ( ) * + , - . / : ; < = > ? @ [ ] ^ _ ` | ~
```

(Only `$^[0]` is a syntax error, because `$^` wants a letter, and `#` starts a
comment.)  The CL-unsafe ones are pipe-quoted on both
sides (``|%;|``, ``|@\||``, ``|%\\|``).  A container the runtime does not own is not
a wrong value but a **dead file**: the read compiles to a bare CL symbol
nobody declared and the load dies before line 1 (a write auto-vivifies through
`p-setf`, so only the read-first spelling shows it).  Two related spellings are
still open: `$$ {EXPR}` — PID magic, a SPACE, then braces — is an element of
`%$` (without the space it is the double deref `${${EXPR}}`, perl's own
adjacency rule), and the WHOLE-container spellings (`keys %$`, `%? = (…)`)
still drop, because PPI lexes `%?` as two operators (task #550).

### 2b.2 The declaration forms

| Perl | v2 emission |
|---|---|
| `my $x = INIT;` (boxed) | `(let (($x (make-p-box nil))) (p-my-= $x INIT) …rest…)` |
| `my $x = INIT;` (raw slot, §2.2) | `(let (($x INIT)) …rest…)` |
| `my $i = $i + 1;` (self-ref init) | `(let (($i (p-box-init $i))) …)` — CL `let` inits evaluate in the *outer* environment, matching Perl's "RHS sees the outer variable" rule |
| `my @a; my %h;` | `let` binding to a fresh vector / hash table |
| `our $g = V;` | `(p-defcell $g (make-p-box nil))` hoisted to the section's declarations + `(p-scalar-= $g V)` in place (`defvar` instead for an exception-set name). `our` shadowing a `my` gates to v1 |
| `state $n = 0;` in a **named sub** | native since s277c: the variable is renamed to a per-sub package cell (§2b.3) — `(p-defcell $n__state__K (make-p-box nil))` + raw once-flag `(p-defcell $n__state__K__init nil)` hoisted to the declarations; the statement lowers to `(unless $n__state__K__init (box-set $n__state__K INIT) (setf $n__state__K__init t))` followed by the bare cell as the statement value. One cell per named sub = exactly Perl's named-sub `state` semantics. `state` *outside* named subs, in anon subs / map-grep-sort blocks (per-closure instances), list/non-scalar `state`, and blocked renames still gate → v1, which uses its own `$state__<sub>__<name>__N` cells |
| undeclared globals | swept up at assembly time (`_forward_global_decls`, a text scan over the finished section): every referenced name gets `(p-defcell NAME <fresh container>)` — `(defvar …)` for the exception set — under "Forward declarations"; package-qualified refs get the declaration in *their* package |
| `state $n` cells, promoted lexicals (`__file__`/`__state__`) | the same `p-defcell`, since the renamed name is word-shaped and therefore ordinary |

**A name may be BOTH declared here and `let`-bound in the same section**, and
that is not a contradiction: a `p-defcell` is a symbol macro, which a `let` of
the same name lexically shadows.  So the scan declares a cell for every name it
sees and never asks which role the name plays — under `defvar` it had to ask
(a special proclamation would have turned the section's own `let`s into dynamic
rebinds), and the three approximate answers it used were the `__shadow__` /
`__cond__` / `__emb__` renames, deleted in #291.  The cost is an inert cell for
a name that is only ever lexical here; the benefit is that "is this a lexical
or a global?" stops being a question the compiler has to guess (see also §6.2's
`:my`, the one place the guess survives and is stated instead).

The rest-of-block nesting means scope is structural (review doc §2.3):
reading a `)` closes a scope. At file level the `let`s nest the remaining
*runtime* statements of the section — the hoisted definitions sit outside,
which is exactly why the rename families below exist.

Bookkeeping a consumer can ignore but a debugger should know: every `my`
registers in three sets (`_reg_lex`) — `_let_bound_vars` (scoped, drives
the seam's my-vs-package decisions and the string-eval capture alist),
`_live_lex` (scoped, drives capture *gates*), `_all_lex` (cumulative,
drives the never-defvar exclusion).

### 2b.2a The declaration form carries its CLASS — `p-let` (normative, s466, task #1035)

Since s466 every `my` binding in the table above is printed as

    (p-let ((NAME CLASS INIT . FACTS) …) BODY…)

and `p-let` expands to exactly the `let` the table shows — the class costs
nothing at run time and changes no behaviour; a consumer that ignores it still
reads a correct program, it just loses the information.  CLASS is the
compiler's verdict about what the binding IS, from a CLOSED set (an unknown
class is an error at macroexpansion, CLAUDE.md rule 12 — never a silently
untyped binding):

| class | the binding is | INIT shape |
|---|---|---|
| `:box` | a p-box cell — the general scalar (§2.2), incl. the self-referential init | `(make-p-box nil)`, `(p-box-init …)` |
| `:scalar` | a raw UNBOXED slot holding the scalar VALUE itself — the plain `unboxable` verdict: never aliased, no fixed value family | the lowered init expression |
| `:num` | a raw numeric slot — the B-regime `coerce => num` verdict | `(%pcl-to-number-strict …)` |
| `:str` | a raw simple-string slot — `coerce => str` | `(%pcl-to-string-strict …)` |
| `:str-buffer` | an adjustable fill-pointer string — the S1 append-only verdict | `(%pcl-str-buffer …)` |
| `:array` | a perl array, fresh or copied | `(make-array 0 :adjustable t :fill-pointer 0)`, `(p-copy-array …)` |
| `:hash` | a perl hash, fresh or copied | `(make-hash-table :test 'equal)`, `(p-copy-hash …)` |

FACTS are optional keyword pairs after INIT, from a CLOSED set (an unknown key
is an error at macroexpansion, exactly like an unknown class).  Since s469bg
three are emitted:

| key | value | meaning |
|---|---|---|
| `:perl` | the source spelling, sigil included (`"$x"`) | this NAME is a rename; the perl program called the variable that |
| `:why` | a family keyword (`:exception-global`, `:seam-shadow`, `:state-cell`, `:spanning`, `:captured`) | *why* the compiler renamed it — the §2b.3 family |
| `:captured` | `t` | a nested anonymous sub's body names this binding: it must outlive the frame (heap, not stack, for a target without native closures) |

`:perl`/`:why` come from the compiler's own RENAME MANIFEST, recorded at the
moment the rename is minted — **never re-derived from the suffix text**, which
is what a consumer would otherwise have to do (and what §2b.3 used to leave it
to).  The manifest is complete for the families that bind a `let`: in the
perl-tests corpus 129 of 2 214 `p-let` entries are renames and every one
carries the pair (119 `:exception-global`, 7 `:seam-shadow`, 3 `:state-cell`).
The other two families promote to a package CELL (`p-defcell`), which is not an
entry and carries no facts — `:captured` reaches emission for those on `p-sub`
instead (§5.1).

`:captured` is the compiler's capture VERDICT and is conservative in the OVER
direction: it is a scan of the closure body's text, so a name a nested sub only
mentions in a string can carry it.  Over-reporting is the safe direction for
every consumer (heap-allocating a binding nothing captures is merely wasteful;
missing one is a dangling reference), and it is why the key is not narrowed to
the veto the compiler acts on.

Reserved and not emitted: the provenance flag (`:proven` / `:declared`, task
#1034).  **An absent key means "not stated", never a default** — the compiler
omits what it did not prove, so a present key is a promise and a missing one is
silence.

No SBCL type declaration is derived from the class: the
runtime runs at `(speed 3)`, where a wrong declaration is undefined behaviour.
The compiler prints every `my` binding through ONE printer (`Pl::Parser2::_decl_entry`
/ `_decl_let`); `PCL_IR_PLAIN=1` prints the pre-s466 `let` spelling, a
verification switch whose only use is proving a change touched nothing but the
syntax.  **Its output is for COMPARISON, not for running**: the `p-sub` facts
plist is a POSITIONAL slot, so the plist-less form the switch prints does not
load on the runtime that expects it.  That is the price of an unambiguous
lambda list, and it costs nothing — the only consumer is a byte-diff against a
tree that predates the step.

**Sub PARAMETERS carry the same class.**  A parameter of the signature fast
path is a declaration too — the sub's own binding of the caller's value — so
since s469bg each `p-raw-params` entry is `(NAME CLASS)` with CLASS from the
same closed set and the same `_slot_class`:

```lisp
(p-raw-params (($a :scalar) ($b :scalar)) BODY…)
```

The class is ignored at run time; the binding is the raw incoming argument
either way.  Every parameter class in the perl-tests corpus is `:scalar` today
and that is a fact about the ANALYSIS, not a placeholder: the B-regime freeze
verdicts (`:num`/`:str`) exclude parameters by construction, because a
parameter's initial value is caller-bound and not a wrappable write.

**A `p-sub` carries a FACTS PLIST** — see §5.1.

### 2b.3 The rename families

All renames happen **at the PPI token level, before lowering**
(`set_content` on the parse tree), so the native path, the v1 fallback
seam, and every analysis pass see the new name with no further plumbing.
Numbering comes from per-file counters iterated in **sorted order** —
hash-order iteration once made the numbering nondeterministic per process,
churning the module cache. Every rename preserves one Perl visibility
rule: in `my $x = $x + 1`, the RHS reads the *outer* `$x`
(`_rename_decl_within` skips tokens inside the declaration statement).

| symbol shape | family | meaning |
|---|---|---|
| `$x__file__N` | v2 file-cell promotion (W5/W10) | a file lexical that must be visible outside the `let`s — because a named sub captures it, because a **BEGIN/END scheduled block references it** (their `p-BEGIN` forms live in the definitions stream, outside the runtime `let` chain — s295c, source-position interleaved since s300b: `my $x; BEGIN { $x = 5 }` / END-cleanup idioms), or because it spans a `package` boundary — promoted to a package-level CELL (the `our` shape: p-defcell + `p-scalar-=`, no `let`). The fresh name is the whole point: declaring `$x__file__0` cannot disturb an unrelated `let $x`. **WHEN THE NAME IS FRESH** (amended #470): the compiler keeps the ORIGINAL name — `p-defcell $x`, the *identity* promotion — exactly when the file both declares that name once (`my`/`state`, any sigil) *and* never spells the package variable of the same canonical name (`$Pkg::x`, `$::x`, `our $x`, a qualified interpolation `"$Pkg::x"`, a `*Pkg::x` glob). Then no `let $x` and no other spelling of the cell exists, and keeping the name is what makes interpolation, `${x}` and string eval resolve to it. Any of those spellings present, the name is mangled — a translator reading `p-defcell $x` may assume the lexical IS that package variable's only user; reading `$x__file__N` it must not |
| `$Pkg::x__file__N` | v2 spanning refs (W10) | uses of the above from *later package segments* — package-qualified so their section's reader (sitting in its own package) reaches the declaring section's cell |
| `$x__shadow__N` | v2 seam-shadow rename (W8.5) | a `my $x` *inside a block that lowers through the v1 seam* (`map { my $x = … }`, `do { my $x … }`) while an outer lexical `$x` is live. Unrenamed, the seam's defvar-based handling would write through the outer variable (the v1 bug); renamed, the inner block gets its own unique cell |
| `$x__cond__N` | v2 poisoned-condition rename (W8.5) | `if (my $x = …)` / `for (my $x…)` where the *same bare name* is also used outside the construct as a package global. The construct's lexical takes the fresh name so the global keeps `$x` and gets its forward defvar |
| `$x__emb__N` | v2 embedded-`my` rename (W8.5, #265) | an *expression-embedded* `my` **inside a named sub** (`++my $x->{k}`, `open my $fh, …`, `… if my $x = …`) whose bare name is also mentioned by another named sub in the segment. Unrenamed, the let-hoist refuses the decl (it cannot tell that other sub apart from one sharing a file-level cell) and the `my` writes the package GLOBAL — persisting across calls. Renamed, the sub gets its per-call `let` AND the global keeps `$x` and its forward defvar |
| `$x__lex__N` | v1 closure-capture rename | v1's fix for defvar-poisoned closures: a block `my` captured by a nested sub becomes a fresh, never-defvar'd name so its `let` stays truly lexical. Appears in v2 output too, inside seam-lowered map/grep bodies |
| `$x__state__N` (+ `…__init`) | v2 state cells (s277c) | a named sub's `state` variable promoted to a per-sub package cell + raw once-flag (see the declarations table above); same blockers as the other renames |
| `$state__<sub>__<name>__N` (+ `…__init`) | v1 state cells | same idea, v1's spelling — seen in v1-dialect files |
| `--anon-block-N--` | both | hoisted anonymous-block functions (block-form prototype args: `first { … }`, `sort` comparators via the seam) |
| `--pcl-if-ret--N`, `%_args`, `$state…` | both | compiler temporaries; never user-visible names |

For a translator the practical takeaway is reassuring: **renames need no
special handling**. By the time the tree reaches you, a renamed variable
is just an ordinary variable with an unusual name — read its kind off the
emitted shape exactly as for any other name (§2b.2): `__file__` cells are
defvar'd package vars; `__cond__`/`__emb__`/`__lex__` are `let`-bound lexicals;
`__shadow__` is whichever the lowering path produced (observed: the v1
seam emits it as a defvar'd cell — sound *because* the name is unique).
The suffixes matter only for mapping output back to source (strip
`__family__N` to recover the Perl name).

### 2b.4 The guard rails (when renaming refuses)

Renaming by token rewrite is only sound when the token walk can *reach
every use*. Each pass checks blockers (`_shadow_rename_blocker`,
`_scan_lex_facts` disqualifiers) and, on any hit, refuses the file with
a hard `Parser2 TODO:` error rather than renaming unsoundly (before the
E4.1 flip these conditions gated the file to the v1 pipeline; v1 is
gone, so a refusal is now terminal):

- **Interpolated uses** — `"$x"`, `/$x/`, heredocs: the name lives inside
  a quote token the Symbol walk can't rewrite.
- **Brace-deref** — `${x}`: same reason — for the passes that still walk
  Symbols only.  The *spanning* rename resolves and rewrites this shape
  since s363 (#264): one helper (`_brace_name_refs`) answers "which
  canonical variables does this node mention that way" for both its
  detector and its rewriter.
- **Shadowing / multiple declarations** of the bare name: a single
  positional rename would merge distinct scopes.
- **Array/hash family sharing the bare name** — `@x`, `%x`, `$#x`,
  `$x[i]`, `$x{k}` are *different variables* whose element-access tokens
  share the `$x…` spelling.
- **String `eval` in scope** — *narrowed (s294/295)*: a renamed cell is
  made reachable by its original source name (the alias rule + span
  pairs, §9.1), so string eval alone no longer refuses a rename. What
  still refuses: a **container** (`@x`/`%x`) promotion with a post-decl
  string eval (only scalar cells are aliased today), and a renamed decl
  **nested inside an outer `my` of the same bare name** (the site
  alist's let-bound pair precedes the global in resolution order, so
  the deeper cell could never win the by-name lookup). The `cond`
  family no longer refuses (s363, #254 B-i): `$x__cond__N` is
  let-bound, and `_eval_lexical_alist` strips `__cond__N` like the
  other let-bound suffixes. `state` keeps the blanket refusal — see the
  three-route rule below.
- **`state`** — its per-instance semantics run through the separate
  `state_var_renames` machinery; token-renaming would bypass it.

**The three-route eval-visibility rule (s363/s364, normative).** Which
mechanism makes a renamed cell reachable from a string eval under its
original source name decides whether a rename may waive the string-eval
refusal (`eval_ok`):

1. A **let-bound** rename (`__lex__`, `__shadow__`, `__cond__`) is
   carried by the eval-site alist; `_eval_lexical_alist` strips the
   suffix to recover the key, and the pair exists only while the
   binding is in scope — so an eval inside the construct sees the
   lexical and one outside sees the global, matching perl.
2. A **defvar'd package cell** (`__file__` span/capture promotions) is
   reached through the alias rule (`p-alias-eval-cell`) plus the
   cross-package span pairs (§9.1).
3. A cell reachable by **neither** mechanism keeps a hard refusal:
   today `state`'s `__state__` cells (separate per-instance machinery)
   and container promotions with a post-decl eval.

A new rename family must decide which of the three it is before it may
pass `eval_ok`.

Where no rename applies and the capture would misbehave, the same
conditions exist as hard *gates* (`_check_sub_captures`,
`_check_my_spanning`, the block-form-arg capture gate): the file is
refused with an error naming the condition. (Pre-E4.1 these routed the
file through v1's defvar model; that fallback no longer exists.)

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

### 3.2b Interpolation extent — which text belongs to a `$`/`@` reference inside a dq string, regex or heredoc (normative, s426)

ONE reader decides it — `Pl/InterpScan.pm` (`scan_one`), consumed by the
regex-pattern interpolator (s382f) and by `Pl/PExpr/StringInterpolation.pm`
(s426); the contract is `docs/interp-scan.md`.  A translator must reproduce
these rules (every one probed on perl 5.40.3):

- `$name` / `@name` continue into a subscript CHAIN — `"$x[1]"`,
  `"$h{k}[0]"`, `"$r->[0]{k}"`, `"@x[0,1]"`, `"@h{'a','b'}"`; an arrow is
  taken only when `[` or `{` follows it (`->@*` only under `postderef_qq`).
- A DEREF base continues the same way: `"$$r[1]"` is `$r->[1]`, `"@$r[0,1]"`
  is a slice, `"$$h{k}"` an element, `"${$r}[1]"` is `$r->[1]`.
- A braced NAME **closes** the reference — `"${x}[0]"` is `$x` followed by a
  literal `[0]`; so do the braced magic spellings `${^NAME}`, `@{^NAME}`,
  `${+}`, `@{+}` (the magic arrays `@-`, `@+`, `@{^CAPTURE}` join with `$"`).
- A braced EXPRESSION does **not** close it: `"${$r}[1]"` is 20,
  `"@{$hr}{'a','b'}"` is a hash slice, `"@{[1,2]}[0]"` is 1, and
  `"${\ $x}[0]"` dies "Not an ARRAY reference" — perl TOOK the group.
- The `$#` family — `$#name`, `$#{name}`, `$#$r`, `$#{EXPR}`, `$#-`, `$#+` —
  never chains (a following `[` is literal text; in code it is a syntax
  error).
- The VALUE of a reference is what the equivalent CODE gives: a deref base,
  a braced expression, a second subscript group or an explicit arrow is
  compiled from the reference's own source text through the ordinary
  expression pipeline, so a string and the code it abbreviates cannot
  disagree (the #443 wrong-kind-deref leniency is therefore shared with
  code, not an interpolation bug).  Residue: the `@{ EXPR }` arm unescapes
  the block text and the `${ EXPR }` arm does not (#444).

### 3.3 `p-true-p` (truthiness)

False: the number 0 (but **NaN is true**), the strings `""` and `"0"`,
`undef`, `nil`, empty array, empty hash. Everything else is true —
including `"0.0"`, `"00"`, `" "`, and all references. Boxes test their
value. Blessed objects consult `use overload 'bool'` first — **or the
handler perl derives it from** (`0+`, then `""`; see §3.4's conversion
table), so an overloading object can be false where a plain reference
never is.

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
- **Overload protocol:** every arithmetic, string, bitwise and compare op
  first checks whether a blessed operand's class declares `use overload`
  for that operator and dispatches to it
  (`p-find-overload`/`p-call-overload`). Translators supporting objects
  must preserve this hook order, which is perl's and is spelled once in
  `%with-binary-overload` / `%with-unary-overload`:

  1. the **left** operand's handler for this operator, then the **right**
     one with the swapped flag;
  2. perl's **autogeneration**, where perl has a derivation — for the
     binary ops that is `.` and `x` from `""`, and the `== != < > <= >=`
     family from `<=>` / the `eq ne lt gt le ge` family from `cmp`;
  3. the class's **`nomethod`** handler, called with a **fourth**
     argument naming the operator (`handler($self,$other,$swapped,$op)`);
  4. the ordinary non-overloaded semantics.

  The **third** argument is three-valued and every state is observable:
  `""` (defined, false) for an ordinary call, `1` when the operands were
  swapped, `undef` when the handler is invoked as a *mutator* (a `+=`-family
  key, which PCL does not produce — it autogenerates every compound form
  from its base operator). Never pass `undef` for "not swapped".

  A **comparison handler's return value is the operator's value** and is
  *not* truthified; only the `<=>`/`cmp` derivation produces a Perl boolean,
  because there the language itself is comparing -1/0/1 against 0.

- **Conversion derivation.** The three conversion operators derive from one
  another when a class does not declare the one being asked for, in a fixed
  preference order (perl's `Perl_amagic_call`):

  | asked for | first fallback | second |
  |---|---|---|
  | `""`   | `0+`  | `bool` |
  | `0+`   | `""`  | `bool` |
  | `bool` | `0+`  | `""`   |

  so a class with only `0+` stringifies through it, and an object whose
  `0+` returns 0 (or whose `""` returns `""`/`"0"`) is **false** — a
  blessed reference is not unconditionally true once its class overloads a
  conversion. `fallback => 0` forbids the derivation outright (perl dies;
  PCL keeps the ordinary address form until the binary refusal lands).
  `nomethod` answers a conversion too, with the conversion's own name as
  the fourth argument. One reading: `%p-conversion-handler`.

## 4. Context (scalar / list / void)

The dynamic variable `*wantarray*` carries the calling context:

| value | meaning |
|---|---|
| `t` | list context |
| `nil` | scalar context |
| `:void` | void context |

**Call sites bind it** where the callee is context-sensitive, and the bind
is **named** (s414, #281 item 1) — these four macros are the emitted
spelling:

| emitted form | expands to | meaning |
|---|---|---|
| `(p-list-ctx BODY…)` | `(let ((*wantarray* t)) BODY…)` | list context |
| `(p-scalar-ctx BODY…)` | `(let ((*wantarray* nil)) BODY…)` | scalar context |
| `(p-void-ctx BODY…)` | `(let ((*wantarray* :void)) BODY…)` | void context |
| `(p-caller-ctx BODY…)` | `(let ((*wantarray* *pcl-caller-wantarray*)) BODY…)` | propagate the caller's context (`goto &sub`, tail call) |

The expansion is exactly the `let` it replaces — same binding, same body,
identical code after macroexpansion, no runtime cost. There is no fifth
context: the compiler-side builder (`Pl::CLForm::ctx_bind`) dies on an
unknown one.

**A consumer must accept both spellings.** The bare
`(let ((*wantarray* V)) …)` still appears wherever the context bind rides
along with a second binding — the `sort $var LIST` comparator binds
`*package*` alongside it — and in statement classes still emitted as v1
text. Anything that pattern-matches through a context wrap must peel
either form; the runtime does it with one `%p-strip-ctx` (used by
`%p-fh-arg`'s bareword-filehandle recovery and `p-list-=`'s
undef-placeholder test — a wrap those cannot see past silently changes
behaviour).

`return EXPR` and sub-tail positions inherit the frame's context. `p-sub`
snapshots the value at entry into `*pcl-caller-wantarray*` so nested binds
inside the body don't lie to `(p-wantarray)`, which maps t→`1`, nil→`""`,
:void→`undef`.

**Statement (void) position — the sub-body regime.** A sub body with more
than one statement (or a single compound) is wrapped ONCE in
`(p-void-ctx …)`; every non-tail statement inside then
trusts that ambient and emits no bind of its own.  The tail (implicit
return) statement restores the caller's context at the innermost
expression-statement level: `(p-caller-ctx TAIL-FORM)` — a compound tail
(if/elsif chain) carries the restore on each
branch's leaf value statement, never around the whole compound (its
non-tail inner statements stay in the :void ambient).  Explicit `return`
needs no restore: the `p-return` macro evaluates its values under
`*pcl-caller-wantarray*` itself.  A body that is a single non-compound
statement skips the regime entirely (no binds at all — the tail already
inherits the caller's dynamic context).  `do{}`/`eval{}` blocks and
map/grep/sort bodies are regime *boundaries*: they run in their own
caller's context, so void statements inside them carry per-statement
`(p-void-ctx …)` wraps.  Toplevel (non-sub) statement
position emits per-statement binds only where the form is
context-sensitive (user funcalls, g-modifier matches).

For a translator: this is a hidden argument threaded through every call,
defaulting to "inherit". Context-sensitivity is *observable* — `wantarray`,
list-vs-scalar returns, `=~` in list context returning captures — so it
cannot be erased statically in general.

## 5. Calling convention

### 5.1 Definition

```lisp
(p-sub pl-NAME LAMBDA-LIST FACTS body…)
```

registers the function (visible to compile-time `BEGIN` code — the whole
form is inside `eval-when`) and wraps the body so that at **every call**:
1. the caller's package and sub-name are pushed on stacks (for
   `caller()`),
2. `*pcl-current-package*` becomes the sub's home package,
3. `*pcl-caller-wantarray*` snapshots `*wantarray*`,
4. the body runs inside `(p-sub-frame …)` — the sub's return frame:
   a `(catch :p-return …)` whose value then goes through perl's LEAVE
   rule, `%p-leavesub` (§5.3).

`(p-declare-sub pl-NAME)` is a forward stub so earlier code can reference
the name; it is normally overwritten by the real `p-sub` before anything
calls it.

**FACTS (normative, s469bg, task #1035 step 3)** is a plist at a FIXED
position after the lambda list — **always present, possibly `()`** — holding
what the compiler PROVED about this sub and used to throw away at emission.  A
consumer reads the slot by POSITION, never by shape, and may drop it entirely:
it changes no behaviour and the macro ignores it.  The key set is CLOSED at
both ends (an unknown key is an error at macroexpansion):

| key | value | meaning |
|---|---|---|
| `:returns` | `:num` / `:str` | every `return` and the implicit tail value proved the SAME raw family (#77), so a call-site write `my $x = f()` needs no coercion wrapper |
| `:wantarray-insensitive` | `t` | the caller's context provably cannot be observed: no `wantarray` in the body and every returned value is scalar-shaped (§4).  **True-only**: the walk answers 0 on any doubt, so an absent key already says "not proven" |
| `:writes-args` | `t` / `nil` | does the sub write through `@_` into its caller's variables (#189, §5.2)?  **Both directions**: the scan answers 1 on any doubt, so `nil` is a real proof that the arguments may be passed by value |
| `:string-eval` | `t` | the body contains a string `eval`.  True-only, and conservative: `->eval`, `eval =>` and a hash key spelled `eval` over-fire harmlessly |
| `:captures` | a list of cell names | the promoted package cells this hoisted sub closes over, recorded by the promotion that PROVED the capture (§2b.3's `:captured` / `:spanning` families) |
| `:prototype` | the text (`"$$"`) | an OLD-STYLE prototype.  A signature is not a prototype and prints nothing |

`()` is common and means exactly "nothing proven": 155 of the perl-tests
corpus's 661 `p-sub` forms print it, most of them `use constant` definitions,
which are lowered by a path that computes none of these facts.

The plist is what the compiler proved **under the configuration that emitted
the file**: `PCL_OPT` switches off Kind-A rules, and a fact whose analysis a
switched-off rule pays for (`:returns`, the #77 family) is then simply absent.
That is the same contract as everywhere else here — absent means not stated —
and it is why a consumer must never read an absent key as a negative.

**A plain call that reaches no body is never a value (normative, s432 +
s441c).** Whether the sub was forward-declared and never defined, or never
mentioned at all, the answer is perl's, in perl's order:

1. if the *sub's own package* defines `AUTOLOAD` — that package's own symbol,
   with a body, **no `@ISA` walk** (inheritance is the METHOD rule, §7) —
   call it, with `$AUTOLOAD` set to the fully-qualified name and the original
   arguments;
2. otherwise die `Undefined subroutine &Pkg::name called` (trappable by
   `eval {}` like any other die; the message carries no location of its own —
   the emitted call has none — so a CAUGHT one reads `… called at (eval 0) line
   0.` through `%p-caught-perl-value`, the placeholder every runtime die gets,
   and an uncaught one prints `… called` — s440).

One runtime entry point implements it (`%p-call-of-undefined-sub`; its two
questions — "does this symbol have a body" and "does its package have an
AUTOLOAD" — are the single readings `%p-sub-has-body-p` and
`%p-autoload-symbol`, which `\&NAME` and `sort NAME` §5.4 ask too) and four
paths reach it: the forward stub's body, the trampoline `p-backslash-sub`
returns for `\&NAME` **and for `\&$name` / `\&{EXPR}`, which are the same
late-bound thing** (s446j, task #517: a code ref taken on a body-less name is
`CODE`, its call reaches that package's `AUTOLOAD` with the FULL name, and a
body defined *after* the ref was taken is the one that runs; a name whose
package does not exist is still `CODE` and dies only when called), a SYMBOLIC
call whose name resolves to no body (`&$name(…)` / `$name->(…)` /
`&{"name"}(…)`, in the package §7.1's rule picks — perl reaches AUTOLOAD
through those too, probed s442d), and — for a name the file
never declared, whose call the codegen emits as a direct `(pl-NAME …)` — an
`undefined-function` handler that resumes through CL's `use-value` restart.
A translator to another host needs the same four, or the equivalent of a
per-package "no such function" hook: the decision belongs at the CALL, which
is where perl makes it, and not at an error boundary, because `eval {}` must
be able to see the AUTOLOAD *value*.

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
(p-sub pl-f (&rest %_args) FACTS
  (p-raw-params (($a :scalar) ($b :scalar))
    …body…))
```

`p-raw-params` binds the parameters raw — unboxed, positionally, missing
arguments `undef` — from the enclosing `p-sub`'s `&rest %_args`, and it does
the callee-side flattening the uniform calling convention requires (a plain
`&optional` lambda list would misbind every `f(@args)` / `f(@_)` delegation).
Extra arguments are silently ignored.  Each entry's CLASS is the declaration
class of §2b.2a, carried for a reader and ignored at run time.  The two body
shapes are call-compatible — every call site just applies the function to the
flattened values.

**Flattening:** Perl has no argument structure — at every call, array and
hash arguments splice into one flat value list (`p-flatten-args`). A hash
flattens to key, value, key, value…

**Aliasing (perl's defelem, task #131):** `@_` slots hold the caller's
LIVE boxes for most argument shapes — globals/`our`/`$_`, array/hash
spreads (holes as lazy `%p-defelem-box` cells), and named-container
elements: `f($h{k})` / `f($a[i])` emit `p-gethash-argbox` /
`p-aref-argbox` in argument position (all user-call shapes: named,
coderef, method), which return the slot box when the element exists and
a lazy defelem magic cell when it does not — reads see undef and never
vivify; the first write through `$_[N]` creates the key / extends the
array.  **Copying OUT of `@_` is by VALUE**: `my ($x) = @_` / `my $x =
shift` snapshot the getter's value (`%p-flatten-list`, `box-set`), never the
magic cell — a later `$x = 0` must not write through the alias (s411; the
raw-params fast path had hidden the general path's aliasing).  A plain `my` lexical scalar is boxed **when the callee is known
to write through `@_`** (task #189): the callee's body is scanned once,
the fact rides `sub_info` as `writes_args`, and the VarAnnotator's
`arg-to-writer` event boxes that call site's arguments — so
`sub setit { $_[0] = "x" } setit($lex)` writes the caller's variable
while a read-only callee's arguments stay raw slots.  What still COPIES:
a lexical handed to a callee the scan cannot identify (coderef call,
method dispatch, cross-file sub — the runtime's "Cannot modify non-boxed
value" warning is the backstop there) and deref-element args
(`f($ref->{k})`).  See `docs/not-supported.md` §`@_` argument aliasing.

**Element targets of `s///` / `tr///`** are the element's BOX:
`$a[0] =~ s/…/…/` emits `(p-=~ (p-aref-box @a 0) (p-subst …))`, not
`p-aref`.  A plain match is a read and keeps `p-aref`.

**An `s///` REPLACEMENT is one of three things**, and which one is a
compile-time decision:

* a plain STRING, when the text is literal or names only numbered backrefs
  (`$1`…`$N`) — the runtime rewrites those for the regex engine, with no
  per-match call;
* a `(lambda () …)`, when the text interpolates ANYTHING ELSE — a variable, a
  punctuation magic (`$&`, `` $` ``, `$'`, `$+`, `$^N`, `@-`, …), or a
  dq case-shift escape (`\U` `\L` `\u` `\l` `\Q` `\E`).  The replacement is
  compiled by the ORDINARY double-quoted-string compiler, so it has exactly dq
  semantics; `/e` produces the same shape from Perl code.  **Inside that lambda
  the full match state is live** — the runtime sets it with the same pair of
  calls a plain `m//` makes, so every §8 magic reads the CURRENT match;
* a `(lambda () "…")` over a CONSTANT, when the replacement is SINGLE-QUOTED
  (`s'A'$1'`).  A single-quoted half interpolates nothing at all — not a
  variable, not a magic, not a case shift, and not a backref — and the constant
  goes through a lambda precisely so the engine's own `$N`/`\N` rewrite cannot
  see it.  Whether a half interpolates is the DELIMITER's answer, taken
  separately for the two halves: `s{A}'[$x]'` has a dq-like pattern and a
  literal replacement.

**`&`-sigil calls without an argument list** re-use the caller's `@_`
(Perl's `&foo;` rule).  Named form: `&foo;` → `(pl-foo @_)`.  Deref forms:
`&$ref;` / `&{expr};` / `&{"name"};` → `(p-funcall-ref EXPR @_)`;
`p-funcall-ref` accepts a code ref OR a symbolic sub-name string
(no-strict-refs, `'` = `::`).  The closed set of parents that want the
coderef *mention* rather than a call — `\`, `defined`, `exists`, `undef`,
`goto` — lower it themselves (`(p-backslash (p-get-coderef EXPR))`,
`(p-coderef-defined-p EXPR)`, …), so `(p-get-coderef EXPR)` in emitted
code always means "the coderef itself", never a suppressed call.

### 5.3 Return

Two rules, in this order: how a `return` **reaches** the frame, and what the
frame does to the value on the way **out**.

**Reaching the frame.**  `(p-return V…)` throws to the nearest `:p-return`
catch — normally the sub frame, but `eval { }` installs its own (§6.3),
matching Perl.  On the way out `p-return-value` adjusts the value:
- scalar box → unboxed value (blessed boxes stay boxed to keep the class),
  **except that a box holding `nil` yields `*p-undef*`, never raw `nil`** — a
  box is a SCALAR and a scalar contributes exactly one element to the returned
  list, while raw `nil` is the runtime's *empty-list* marker that
  `%p-flatten-list` drops by design.  Without the normalisation the scalar
  vanished from the caller's list and every later element shifted left
  (#790: `my $ok = eval { die }; return ($ok, "w")` assigned `"w"` to the
  caller's FIRST target).  The two producers of a `nil`-holding scalar are
  `eval { }` after a die and a bare `return` read in scalar context;
- a box holding a hash-table, a function, or a NON-STRING vector is a
  reference and stays boxed.  A CL string IS a vector, so a string-holding
  box used to stay boxed here too and `return $s` handed the caller its own
  box (task #964).  That accident was load-bearing for one shape: a genuine
  dualvar holds a STRING and carries its numeric half on the BOX, so it now
  has an arm of its own (`%pcl-dualvar-p`) instead of riding on `vectorp`;
- array value in **scalar** context → its element count;
- a **raw** (unboxed) `nil`/empty in **list** context → empty list — this is
  the bare-`return`/`return ()`/empty-array arm, and it is the one place the
  marker still means "no elements";
- multiple values `return (5,3,1)` → the list in list context, the *last*
  element in scalar context.

A sub body falling off the end returns its last evaluated form (the `catch`'s
value), so the tail expression and `return` arrive at the frame the same way.

**A `return` in TAIL POSITION does not throw (normative, s465bb, task #994).**
When a `return EXPR` (no statement modifier, single expression) IS a sub
body's LAST statement, nothing stands between it and the frame's catch, so the
emitter writes the value where the implicit tail would go:

```lisp
(p-tail-value EXPR)                      ; single-statement body
(p-caller-ctx (p-tail-value EXPR))       ; multi-statement body (:void regime)
(p-return-empty)                         ; a bare `return;` in tail position
```

`p-tail-value` is `p-return-value` with its IDENTITY fast path inlined: a
value that is neither raw `nil` nor a raw non-string vector comes back
unchanged, so the ordinary scalar return costs two type tests and no call.
The two arms that do fire are the ones the frame exit does NOT repeat — an
ARRAY in scalar context is its element COUNT, and raw `nil` in list context is
the empty list; every BOX arm of `p-return-value` is applied again by
`%p-leavesub` below, which is why a box may pass through here untouched.  The
`p-caller-ctx` wrap is the same restore an implicit tail statement gets (§4):
a multi-statement body binds `*wantarray*` to `:void` once, and the tail value
must be computed in the CALLER's context — which is what `p-return`'s own
`*pcl-caller-wantarray*` rebind used to supply.  A single-statement body never
rebinds `*wantarray*`, so it needs no wrap.

Everything else still throws: a `return` under a statement modifier, a
multi-element `return (A, B)`, a `return` nested in a compound or a loop, and a
`return` inside `eval { }` or a sort comparator (whose frame is a different
`catch` — §5.4 and §6.3).  The transform is the Kind-A emission `tail-return`,
so `PCL_OPT=none` / `PCL_OPT=-tail-return` restore `(p-return EXPR)` and run
identically.

**Leaving the frame — the copy (normative, s464a, task #964).**  A non-lvalue
Perl sub returns a *mortal copy* of every value it returns (perl's
`pp_leavesub`); only a `:lvalue` sub returns the variable itself.  PCL applies
that rule ONCE, at the frame exit, in `%p-leavesub`, reached through the
`p-sub-frame` macro:

```lisp
(p-sub-frame BODY…)  ≡  (%p-leavesub (catch :p-return BODY…))
```

`p-sub` expands to it, and the emitter writes it for an anonymous sub's
wrapper (`Pl/Parser2.pm` `_lower_embedded_anon`, `Pl/Parser.pm`'s v1 seam).
The rule reads `*wantarray*`, which at that point is still the **caller's**
binding — `p-sub` never rebinds it, it only snapshots it into
`*pcl-caller-wantarray*` — so the copy is context-correct.

| value at the frame exit | list context | scalar (and `:void`) |
|---|---|---|
| genuine dualvar box (`$!`, `Scalar::Util::dualvar`) | a FRESH box keeping BOTH halves (`%p-dualvar-copy`) | same |
| ref / blessed / container box (value is a hash-table, a function, or a non-string vector) | a FRESH box with the same value, is-ref and class (`p-copy-scalar-arg`) | same |
| any other box | its unboxed value; `nil` → `*p-undef*` | same |
| raw non-string vector | a FRESH vector **of the same shape**, the ELEMENT rule per element | unchanged — the consumer counts it |
| raw non-blessed hash-table | a FRESH hash-table, scalar rule per real-key value | unchanged |
| deferred-flatten marker (`(p-flatten @a)`, the emitter's `@array` element of a comma list) | a FRESH marker around the copied aggregate | unchanged |
| anything else raw | unchanged | unchanged |

The **element rule** for a vector's slots is not the scalar rule: a box is one
scalar, and *a raw aggregate is copied same-shape as well*.  Without that,
a nested aggregate rode out of the frame as the callee's own storage —
`my @a=(1,2,3); sub nest_a { (0, @a) }; for (nest_a()) { $_ = 'W' }` wrote into
`@a`, and so did `sub two_arr { (@a, @b) }` — because the consumer's flatten
walk then aliases what it finds.  `return (@a, 9)` was already right: `p-return`
flattens before the frame sees it.

Notes a translator needs:
- **A plain box costs nothing** — the unboxed raw value *is* the copy.  Only a
  reference/blessed/dualvar box allocates, which is exactly `pp_leavesub`'s
  own per-SV `mortalcopy`.
- **A vector's SHAPE is semantic and the copy preserves it.**  An *array* is
  adjustable with a fill pointer and its slots are the values; a *list temp*
  is the simple vector the emitter builds for a comma list, and its aggregate
  elements are still UNFLATTENED — the consumer's `%p-flatten-list` walk
  expands them.  Copying a list temp into an adjustable vector tells every
  consumer it is an array, and a nested aggregate then becomes ONE element:
  `sub steps { qw(x), $_[0]->SUPER::steps }` yields `ARRAY(0x1)`.  A nested
  aggregate is therefore never FLATTENED here — that stays the consumer's job —
  but it *is* COPIED, same-shape, which leaves it one element and changes only
  whose storage it is.  (That the ARGUMENT flattener spreads only one level
  where the list-assignment one recurses is a separate, pre-existing bug —
  #988.)
- **The copy is of the REFERENCE, never the referent.**  `sub f { $obj }`
  returns a fresh box over the same blessed hash-table, so the object still
  `==` and method calls still reach it; only `\f()` and `$_[0] = …` stop
  writing through.
- **Never in place.**  `&f;` and `goto &f` share `@_` with the caller, so a
  vector reaching the frame exit may be the caller's storage.
- **`:void` does NOT skip the copy.**  PCL's void regime binds `*wantarray*`
  to `:void` around a whole discarded STATEMENT, so an inner call whose value
  *is* consumed sees `:void` too — `my $r = \f(); $$r = 10` and
  `sub mod { $_[0] = 12 } mod(f())` both leak if `:void` returns the value
  untouched.  `:void` therefore takes the scalar rule.
- **An EVAL is a frame too** (task #987).  perl's `pp_leaveeval` mortal-copies
  the value an `eval { }` or a string `eval` leaves with, exactly as
  `pp_leavesub` does at a sub's exit, so `for (eval { $x }) { $_ = 5 }` leaves
  `$x` alone and `\eval { $x }` is not `\$x`.  The same rule, applied at
  `p-eval-block`'s and the string-eval's `catch :p-return` — in the `prog1`'s
  *value* position, so `$@` is still set after the body and the error path
  still yields a bare `nil`.
- **`do { }` is NOT a frame**, and that is the boundary: `\do { $x }` *is*
  `\$x` in perl, so a `do` block must not copy.
- **Not frames:** `p-sort-cmp` (§5.4 — the value is numified), `do { }` above,
  and `p-goto-sub` (the target sub runs its own frame, which copies).
- **`:lvalue` (task #930) is the switch on this macro**: an lvalue sub is to be
  emitted with a frame that omits the copy, since the bare `catch` value is
  already the place.  Not implemented.

### 5.4 Comparator frames — `p-sort-cmp`

A `sort` comparator is the one *block* that gets a return frame of its
own, and since s414 (#281 item 6) it is named:

```lisp
(p-sort-cmp (PAIR…) [(declare …)…] BODY…)
  ≡ (lambda (PAIR…) [(declare …)…] (catch :p-return (block nil BODY…)))
```

**Why the frame exists:** `return` inside a `sort` block exits the
*comparator*, not the enclosing sub, so the comparator installs the same
`:p-return` catch a `p-sub` does (§5.1). `block nil` is the ordinary CL
exit target. Leading `(declare …)` forms stay at the lambda head, where CL
requires them: a comparator inside a block-level `package X;` region binds
the region's *qualified* pair (`X::$a` / `X::$b`, §1), whose `defvar`s have
not yet been evaluated when the lambda is compiled, so without
`(declare (special …))` the parameters would bind lexically and a
comparator reading the global would see nothing.

The body is emitted in scalar context, i.e. the full shape is
`(p-sort-cmp ($a $b) … (p-scalar-ctx BODY…))`. All three comparator
spellings share it: a literal block, `sort NAME LIST` — whose body is
**just the call**, wrapped since s446j (#514) in the name-carrying
`(p-sort-named 'pl-NAME (p-sort-cmp ($a $b) (p-scalar-ctx (pl-NAME))))`, with
the pair passed as arguments as well when NAME has a `($$)` prototype — and
`sort $var LIST` (resolved at runtime by `p-sort-get-fn`, and the only one
that still wraps the lambda in a `let` capturing `*package*` at creation
time, so a *string* comparator name resolves in the user's package rather
than in `:pcl`).

**`sort NAME LIST` has no comparator-specific undefined-sub handling, and a
translator must not add any** (s442d, task #501). Until then the named form
wrapped its call in an `undefined-function` handler that dispatched to
`AUTOLOAD` for perl bug #30661; it was a third copy of §5.1's rule, its
no-`AUTOLOAD` arm returned `nil` — so `sort nonexistent LIST` quietly
compared everything equal where perl dies — and it could never fire (it
interned `"PL-AUTOLOAD"`, not the symbol `%pcl-cl-sub-name` produces).
§5.1's rule at the CALL covers every case, and matches perl measurably: a
sort comparator name **does** reach the package's `AUTOLOAD` (probed on
5.40.3), and with no `AUTOLOAD` it dies.

**Entry resolution (s446j, task #514).** perl resolves the sort sub when the
sort *starts*, not at the first comparison: `sort nonexistent (7)` and even
`sort nonexistent ()` die `Undefined sort subroutine "main::nonexistent"
called` although no pair is ever compared — and they die **after** the LIST
has been evaluated (probed with `$|=1`: the list's own output comes first).
That ordering is what puts the check inside `p-sort` rather than in the
comparator form, which is an *argument* and therefore evaluated before the
list: `p-sort-named` builds a `p-named-cmp` struct carrying the symbol, and
`p-sort` resolves it on entry (`%p-sort-resolve-comparator`).  The check asks
**"a body, or the package's own `AUTOLOAD`"** — never `fboundp` alone, because
a forward declaration with no body IS `fboundp` and perl dies for it, while an
`AUTOLOAD`-only name works at every list size.  A translator that keeps the
resolution lazy is wrong only in *when* it dies; one that keys it on `fboundp`
is wrong about *whether*.


**The comparator may be GONE: `%p-sort-classic` (s470a5, task #996 half A5).**
When the comparator is one of the five *classic* shapes — no block at all, or
exactly `{ $a <=> $b }` / `{ $b <=> $a }` / `{ $a cmp $b }` / `{ $b cmp $a }`
on the current package's pair — and the sort's ALIASES are unobservable, the
optimization registry's `classic-sort` pass replaces the whole call with

```lisp
(%p-sort-classic MODE ARGS…)
  MODE ∈ { :default :num-asc :num-desc :str-asc :str-desc }
```

and there is no comparator frame left in the IR.  `:default` is the no-block
form and is NOT the same as `:str-asc`: their fast paths agree, but their
FALLBACKS do not — `p-sort`'s no-comparator arm stringifies (`string<` on
`to-string`) while `{ $a cmp $b }` dispatches a `cmp` overload.  An unknown
MODE is a compiler/runtime disagreement and dies naming the value.

**A translator may treat this form as sugar and expand it back** to the
`(p-sort (p-sort-cmp …) ARGS…)` of this section — the emission under
`PCL_OPT=none` is exactly that, byte for byte, and the runtime itself falls
back to it whenever any element is not a plain scalar.  What the fast form
buys is that a *plain* element (a number, a string or undef — no bless class,
no is-ref flag, no dualvar halves, no tie proxy, no magic) has pure numeric
and string readings, so the key may be taken ONCE and the source array's raw
slots need not be promoted to boxes (§3.2).

**When it may NOT be emitted, and why it is a property of the CONSUMER.**
`sort` yields ALIASES: `$_++ for sort { $a <=> $b } @a` writes back into `@a`,
and `map { \$_ } sort @c` hands out the very scalar `\$c[i]` denotes.  The
pass therefore emits the fast form only where either (A) the value is copied
before anything can write through it — an array/hash/list assignment, an
anon-array constructor, `push`/`unshift`, `join`/`print`/`say`/`printf`,
`return`, or a `p-foreach-raw` list (§6.2's read-only loop verdict) — or (B)
every top-level argument produces temporaries: a literal, `p-keys`, `p-map`,
`p-split`, `p-readdir`, `p-glob`, a range, or a user sub call.  `p-values`,
`p-grep`, `p-reverse` and a nested `p-sort` all hand aliases through and are
NOT sources under (B); `p-reverse` *is* transparent to (A).

`grep`/`map`/`eval` block bodies are plain `(lambda …)` with **neither**
the catch nor the context bind — `return` inside them must propagate to the
enclosing sub's frame.

## 6. Control flow

### 6.1 Conditionals

`(p-if COND THEN [ELSE])`, `(p-unless COND THEN [ELSE])` — plain
conditionals over `p-true-p` (§3.3). Ternary `?:` is also `p-if`.

### 6.2 Loops and loop control

`p-while`, `p-until`, `p-for` (C-style: `(p-for (INIT) (COND) (STEP)
body…)`), `p-foreach ((VAR LIST) body…)` and its read-only twin
`p-foreach-raw`. All accept trailing keys
`:label NAME` (must be first key) and `:continue (progn …)`.  The foreach
family additionally accepts `:my t` (after `:label`, before the body) —
see "Loop variable: lexical or localized" below.

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
to **each element's cell** (`%p-foreach-elt`) — so mutating the loop
variable writes through to the array, matching Perl's foreach aliasing.
Under raw element storage (§2.3) a slot holding a raw value is **promoted
in place** to a box at that moment, which is what makes the alias real; a
hole binds a deferred-element box that vivifies on first write.

**`p-foreach-raw` is the read-only twin** (normative, s459am): identical in
every respect except that VAR is bound to the slot **as it stands**
(`%p-foreach-elt-raw`) — a raw value directly, an existing box unchanged, a
hole as undef — with no promotion and no allocation.  A translator may treat
the two as the same construct; the `-raw` spelling is a *promise by the
compiler*, not a different loop.  The promise is that the loop variable is
read only: VarAnnotator emits it only when the region's every use of the name
is a pure read (no write of any kind — including the statement-root `=`,
coercing-compound and `++` writes that leave no boxing event — no `\$v`, no
`local`, no regex target, no mutating builtin, no `@_`-writing callee, no
capture with an event, no string eval).  It is worth having because promotion
is **monotone**: a read-only walk over an array would otherwise box every slot
permanently, taxing every later read of that array.  Emitted only with `:my`;
the package-cell arm is refused (a raw value in a global a callee can write
through would lose the write).  Switchable as the Kind-A gate `foreach-raw`.

**Loop variable: lexical or localized.**  Perl's `foreach $pkgvar (…)`
implicitly *localizes* the package variable — the loop variable is aliased to
each element for the body's dynamic extent, so a sub called from the body sees
the current element, and the old value is restored on exit (including on
`die`).  `foreach my $x (…)` declares a fresh *lexical* instead, which no
called sub can see.  Since the direction-D flip (§2) an ordinary global and a
lexical are spelled the SAME symbol, so the two cases are told apart like this:

- `:my t` present → **lexical binding**, always.  The compiler emits this key
  for `foreach my $x`, whose declaration the macro cannot otherwise see.
- no `:my` key → the macro asks whether VAR names a global cell (a
  `p-defcell` symbol macro) *in its macroexpansion environment*.  Yes →
  save/set/restore over the cell.  No (a plain lexical is in scope, or the
  name is exception-partition, or there is no such global) → lexical binding.

A consumer that lowers this IR must reproduce the same split; treating `:my`
as decoration silently leaks the loop value into every sub the body calls.

**Declarations in a loop/condition HEAD.**  A `my` anywhere in an `if`/`while`
condition, or in ANY of a C-style `for`'s three head sections, scopes to the
whole construct — head *and* body — and to nothing after it.  The IR states
that structurally: the construct is wrapped in a `let` binding a fresh cell per
declared name, and the section itself lowers to the per-iteration assignment
into it (`(let (($k (make-p-box nil))) (p-for () ((p-< (p-my-= $k $i) $j)) …))`).
The C-for INIT counter is the one exception in *spelling* only — it gets its own
`let` one level in, because it may also take an unboxed raw slot.  There is no
"declare it in the enclosing scope" fallback: a head `my` that reached the
package cell would stay defined after the loop and collide with a same-named
global (#297).  Known divergence: the wrap is ONE binding for the whole loop,
where perl gives the declaration a fresh instance per iteration — observable
only by closing over it in the body (`while (my $x = shift @l) { push @c, sub
{ $x } }` yields perl's 1,2,3 vs PCL's last value); tracked as #300.

**Counting-loop range foreach** (s286b): a foreach whose list is a **sole
range** `A..B` lowers to `(p-foreach-range ((VAR A B)) body…)` or its
`-raw` variant instead of materializing the range vector.  Semantics:

- **Endpoints are evaluated exactly once, before the first iteration** —
  this is Perl's own behavior (perl builds the foreach list first;
  assigning to `$w` inside `for my $i ($q..$w)` never changes the trip
  count), verified against perl.  Numeric ranges allocate no list at all;
  a magical *string* range (classified by `%p-range-classify`, the same
  oracle `p-..` uses) falls back to iterating a materialized vector inside
  the same skeleton.
- Same `:label`/`:continue`/`next`/`redo` protocol as `p-foreach`.
- `p-foreach-range-raw` binds VAR as a **raw host value** (no box).  This
  is sound without any overload/dualvar analysis because range elements
  are **fresh plain scalars by construction** (perl numifies/stringifies
  the endpoints when building the range — even an overloaded endpoint is
  coerced once, at construction), every body write to VAR must be
  arith-shaped (VarAnnotator), and any `eval` word in the region vetoes
  the raw variant entirely (`eval-in-region`).  Contrast with the
  *planned* raw-numeric verdict for arbitrary-source variables, which DOES
  need a no-overload scan plus a strict checked write —
  `docs/raw-numeric-verdict.md` §"Scope boundary".
- Known divergence: perl's range elements are read-only (`$i = 9` in the
  body dies in perl); PCL permits the write (next iteration rebinds) —
  same family as the `!0`/`!1` read-only non-emulation in
  `docs/not-supported.md`.

**Bare-block `continue`** (s287): `{ … } continue { … }` places the
lowered continue block **after the tagbody, inside the loop-once block**
(labeled shape: after the NEXT catch, inside the LAST catch).  Effect:
`last` skips the continue, `next` (and normal completion) reaches it,
`redo` re-runs the body without it — Perl's semantics.  The continue
block is its own lexical scope.

### 6.3 Exceptions: `die` / `eval { }` / `$@`

`die` signals a `p-exception` carrying either a string or an arbitrary
Perl value (object exceptions). **PCL does not append " at FILE line N."**
to string messages (documented divergence).

`eval { body }` compiles to `p-eval-block`:
1. installs its own `:p-return` catch (Perl: `return` inside `eval{}`
   exits the eval, not the enclosing sub);
2. runs the body; on success sets `$@` to `""` and yields the body value
   **after applying the frame leave rule to it** — an eval is a frame
   (`pp_leaveeval`), so the value is a copy, exactly as at a sub's exit;
   see §5.3;
3. on `p-exception` sets `$@` to the payload (string or object) and
   yields `nil`; any other host error is stringified into `$@`.

A string `eval` is a frame on the same terms.  A `do { }` block is **not**:
it has no return frame and no copy, so `\do { $x }` *is* `\$x`.

`$@` is an ordinary global box. `die` with no arguments re-raises `$@`.
`$SIG{__WARN__}` fires on `warn`; `$SIG{__DIE__}` does **not** fire
(documented divergence).

`try BLOCK catch (VAR) BLOCK [finally BLOCK]` (perl 5.34's `use feature
'try'`) compiles to `p-try`, and it is **deliberately not** `p-eval-block`:

```lisp
(p-try (progn TRY…) ($e (progn CATCH…)) (progn FINALLY…))   ; finally optional
```

1. **no `:p-return` catch and no loop-tag catch** — `return`, `last`, `next`,
   `redo` and `goto` inside the try block belong to the ENCLOSING sub or loop,
   which is the sharpest difference from `eval {}`;
2. `$@` is **localized to the construct**: set to `""` around the try body and
   again around the catch body, and restored to its pre-`try` value in an
   `unwind-protect` cleanup — so a `finally` block, and everything after the
   construct, reads the OLD `$@`.  The caught value reaches the program only
   through VAR;
3. the catch body runs whenever an exception was signalled — never on a test of
   `$@`'s truth, so `die 0` and a bool-overloading object are caught;
4. the construct's VALUE is the executed block's last form, evaluated in the
   caller's context (`*wantarray*` is not rebound), and `finally`'s value is
   discarded.  No new frame is pushed, so `caller()` does not see the block;
5. `finally` is an `unwind-protect` cleanup: it runs on every exit path,
   including a `return` out of the try block.

The Perl value of a caught condition (object payload, or message text with
perl's " at … line …" tail) is `%p-caught-perl-value`, shared by `p-eval-block`
and `p-try` — the two places a program can see a caught error.

### 6.4 goto

Four source forms, four fates:

- `goto &sub` (tail call) — supported; re-dispatches with the current `@_`.
- `goto EXPR` **where EXPR evaluates to a code ref** (`goto \&NAME`,
  `goto $coderef`, `goto $h->{cb}`) — Perl treats this as the *same* tail
  call, and so does PCL: it lowers to `(p-goto-computed EXPR)`, a macro over
  `(p-goto-sub (%p-goto-target EXPR))`, so the frame-replacement semantics
  (caller package/`@_` handling) are the ones §6.4's `goto &sub` describes.
  Before s328 this lowered to a runtime no-op that returned undef silently.
- `goto LABEL` to a **standalone label** in the same statement list —
  fully lowered (backward and forward; details below).
- `goto LABEL` to a label PPI glues onto the statement it marks — a **loop**
  (`LBL: while (…) {…}`) or a **bare block** (`LBL: { … }`), the only two
  shapes that swallow their label (s316) — same lowering, with the tagbody
  opening *at* that statement so the jump re-runs it from the top, as in
  Perl.  In value position the run is bracketed in `(setf RET (progn …))`
  and RET read after the tagbody (the §6.2 bare-block tail regime), since a
  `tagbody` yields nil.  The label's own loop-control `block`/`catch` tags
  are a separate namespace — `last LBL` inside is unaffected.
- **Computed** `goto EXPR` where EXPR is a **label name string** — a CL
  tagbody tag is not a first-class value, so this cannot be expressed.
  `%p-goto-target` ANNOUNCES the operand on stderr and execution falls
  through past the goto (announced-not-silent; a die measurably aborts
  whole files — the rule-12 boundary ruling, `fable-answers-s328.md` §1);
  documented divergence in `docs/not-supported.md`.

Context: every goto tail call restores `*wantarray*` to the goto-ing sub's
`*pcl-caller-wantarray*` around the apply (s329) — the target inherits the
ORIGINAL caller's context, exactly as `p-return` restores context for its
argument.  The frame replacement is throw-based, so a goto CHAIN still
nests dynamic bindings: bounded at ~10^5 in practice where perl is
constant-depth (`not-supported.md` §goto depth note).

**Not a gap: `goto` INTO a construct.**  Jumping to a label that sits
*inside* a loop/block body from outside it ("Use of `goto` to jump into a
construct") has been deprecated since perl 5.12 and is a **fatal error as
of perl 5.44**.  PCL does not implement it and must not grow support for
it.  This is a different shape from the labeled-loop case above, where the
label marks the *statement*: perl's own `lib/sigtrap.pm` still uses that
one in blead (`Arg_loop:` + `goto Arg_loop;`), which is what motivated the
s316 lowering.

**The lowering has two regimes, chosen by the label's position relative to
the goto.**  Both start from the same label lowering: a goto-target label
statement (`again:`) turns the *remainder of its enclosing statement list*
into `(tagbody :again <remainder>)`.

**Backward goto — a lexical jump.**  A `goto again` textually *after* the
label sits inside that tagbody and lowers to the lexical `(go :again)`:
the loop-retry idiom is a plain local branch.  A `my` jumped back over
re-binds fresh (its `let` sits inside the tagbody), matching Perl.
*Porter mapping:* any local jump/loop construct — a `while(true)` +
`continue`, a real label if the target has one.

**Forward goto — a dynamic escape (s295, #63).**  A `goto fwd` textually
*before* the label cannot be a lexical jump (the jump target does not
exist yet at that point in the form), and Perl also allows the goto to
fire from *inside a lambda run during the prefix* — e.g. out of a `map`
block — unwinding whatever frames are between.  So the prefix statements
(everything before the label) are wrapped in a **catch** whose tag names
the label, and the goto lowers to a **throw** to that tag:

```lisp
;; goto fwd; print 1; fwd: print 2;
(catch :pcl-goto-fwd
  (throw :pcl-goto-fwd nil)      ; the goto site — may be nested arbitrarily
  (p-print 1))                   ; skipped statements
(tagbody :fwd (p-print 2))       ; the label and everything after it
```

Semantics of the pair: the throw **unwinds all frames up to the enclosing
catch** (including any lambda/sub invocations entered during the prefix)
and control falls out of the catch's end, straight into the tagbody — i.e.
to the label.  The tag is the keyword `:pcl-goto-<label>`, one per label
name.  *Porter mapping:* any exception/non-local-unwind mechanism — wrap
the prefix in `try { … } catch (GotoFwd) {}` and compile the goto as
`raise GotoFwd`; the fall-through after the handler IS the label.  The
throw's value is irrelevant (always `nil`): the wrap is only applied in
statement position, so nothing consumes the catch's value.

Composition rules (what a port must reproduce):

- **Multiple labels**: the wrap applies to the first standalone label of
  the list and the lowering recurses on `[label..end]` — a second forward
  label produces its own catch *inside the first label's tagbody*, so
  sequential forward gotos nest naturally.
- **Backward gotos are unaffected** by the catch machinery: only gotos
  lowered *inside a wrapped prefix* consult the label→tag map and become
  throws; a goto inside the tagbody remainder stays a lexical `(go)`.
- **Scope guard**: the prefix is only catch-wrapped when it contains no
  `my`/`state`/`local` declaration — wrapping would cut the declaration's
  `let`/save scope short (the catch closes before the label).  Such
  goto-before-declaration shapes are not lowered natively (v1 fallback via
  gate), as is a label in **value position** (tagbody yields `nil`).

Perl's own restrictions (may not jump *into* a construct, nor into a
different sub) are assumed, not checked — PCL compiles valid Perl (§design
rule 9).

## 7. Packages, variables, and OO

### 7.1 Namespaces and case

Each Perl package is a host namespace. Because CL uppercases unquoted
symbols, PCL uses the reader's `:invert` convention: single-segment
package names are stored upcased, and the original spelling is registered
via `(p-register-pkg-name :Animal "Animal")` — string-level lookups
(method dispatch, `ref`) go through that registry. Multi-segment names
(`File::Basename`) keep their exact spelling as `|File::Basename|`.
A qualified Perl variable `$Foo::x` is the symbol `$x` in namespace `FOO`.

**Two "current packages", and a translator must not confuse them
(normative, s442d):** the host's own reader package (`*package*` in CL) is
set by the emitted `(in-package …)` forms *as the file is read*, so once
loading is past a file's last one it is `MAIN` for the rest of the run —
it says nothing about which Perl package a running form was written in.
The Perl-level current package is `*pcl-current-package*`: codegen sets it
at every `package` statement, `p-sub` rebinds it per call to the sub's home
package, `p-require`/`p-use`/`p-eval` rebind it around a load, and
`p-eval-thunk` binds it (with `*package*`) to a string eval's `package X;`
region. **Every "unqualified name → current package" rule that runs at
RUN time reads that one** — notably the symbolic sub-name resolver
(`&$name(…)`, `$name->(…)`, `&{"name"}(…)`, `\&$name`, `goto &$name`,
`sort $name LIST`, `defined`/`exists &{…}`, all through
`%p-resolve-sub-symbol`), which is perlmod's "if the string is unqualified
it is looked up in the current package". Reading `*package*` there resolved
`package NA; sub p { my $s = "nafun"; &$s(3) }` in `main` (task #503). The
*compile-time* symbolic paths (a name the emitter itself qualifies) are
unaffected, and a string containing `::` always names its own package.
An **anonymous** sub has no name to read a home package off, so its wrapper
carries the package in force at the `sub {` as a compile-time constant —
`(let ((@_ …) (*pcl-current-package* "X") …) …)`, the same binding `p-sub`
makes and not a second mechanism (task #515). A closure built in `X` and
invoked from `main` therefore resolves in `X`, and one built in `main` and
invoked from inside `X` resolves in `main` — perl's rule is the *defining*
package in both directions, so inheriting the caller's binding was wrong
both ways. A `package Y;` statement inside the body still switches for the
rest of that body (it emits its own binding). A `map`/`grep`/`sort` **block**
is not a sub in perl and gets no such wrapper: it keeps resolving in the
package current where it runs.

**The rule is now without exception (s446l, task #525).** `%p-symref-symbol`
— the name resolver behind the symbolic **variable** refs `${"n"}`, `@{"n"}`,
`%{"n"}` and their assignment forms — used to read `*package*`, so an
unqualified symbolic variable name resolved in `main` rather than the current
package, in a named sub as much as in an anonymous one, while the typeglob
paths (`*{"n"}`, `p-dynamic-typeglob`) and the sub resolver already read
`*pcl-current-package*`: `*{"n"}` and `${"n"}` disagreed about which stash `n`
was in. It reads `*pcl-current-package*` too now, so every run-time
"unqualified name → current package" resolution in the IR asks one question of
one variable. A **leading `::`** on a symbolic name is perl's ROOT stash —
`${"::v"}` IS `$main::v` — and resolves in `main` whatever the current package
is.

**And a FOREIGN-qualified name never reaches the magic (s451w, task #685).**
The host namespaces PCL makes for Perl packages `:use` the runtime package, so
every sigil-named symbol the runtime exports — the punctuation/caret/magic set
(`|$!|`, `|%!|`, `|$<|`, `|@+|`, …) plus the word-named specials `%SIG`,
`%ENV`, `@INC`, `%INC`, `@ARGV`, `$_` — is *inherited* into each of them, and
inheritance is exactly what makes the UNQUALIFIED spelling right (perl forces
an unqualified special into `main`). It is wrong for an explicitly qualified
one: `%{"foo::!"}` is a distinct, empty hash in perl, and PCL answered main's
errno table (`t/op/leaky-magic.t` rows 4, 48, 67–70), while the array/hash
writers *replaced* the inherited binding, so `%{"foo::ENV"}` destroyed the
process environment. So: **a name explicitly qualified into a package other
than `main` treats an inherited answer as NOT FOUND**, and the create path
shadows before interning, so a write lands in the named package. Twelve
specials probed vs perl: every `foo::`-qualified spelling is separate, every
`main::`-qualified one shared. A translator whose host has no
namespace-inheritance has nothing to do here; one that does must reproduce the
asymmetry, not the inheritance.

Residual gaps in the same area, filed and not fixed: `local ${"n"} = …` does
not localise the cell an unqualified symbolic read then sees (task #574); the
LITERAL spelling `%foo::SIG` reaches the inherited symbol through the host
READER rather than through this resolver, so it still leaks (task #700); and
`%{"main::ENV"}` / `%{"ENV"}` overwrite the `%ENV` marker binding with a plain
empty hash (task #701).

### 7.2 Package variables and `local`

Package vars are globally-registered boxes. **How** the box is registered
depends on the partition (§2b.1): an ordinary variable lives in a global
value cell reached through a symbol macro (`p-defcell`); the exception set
keeps `defvar`. Name-based access — `symbol-value`, `boundp`,
`makunbound`, the glob and symbolic-ref helpers — reaches the same storage
either way, so nothing in the runtime has to know which side a name is on.

`local` saves the current **value** and restores it on scope exit
(dynamic, not lexical): `p-local-cell` for a cell, a dynamic `let` for an
exception name, plus the dedicated `p-local-*` macros for hash/array
elements, typeglobs and the magic cells (`$.`, `$|`, `$!`). Every flavor
restores through a non-local exit as well as a normal one. Restore also
invalidates the box caches.

**`local (TARGET, …) = LIST` is two independent halves** (task #509), and a
translator must keep them apart: every target is SAVED and RESTORED by the
mechanism its storage kind dictates (a binding for a variable, the element
macros for a subscripted slot), and then the values are written by an
ORDINARY LIST ASSIGNMENT over the targets' *places* — the same forms
`($p, $h{a}, $!) = …` would produce. The two halves ask different questions
of the same target and get different answers for two kinds of name: a
subscripted target's storage is its CONTAINER plus keys while its place is
the element accessor, and a magic name can have a storage cell that is not
its place (`$!` is stored in `*p-stored-errno*` and written through
`(p-errno-string)`). Building the assignment out of the STORAGE names is
what made `local($p,$h{a}) = (5,6)` write a phantom scalar `$h`. The RHS is
evaluated BEFORE any target is localized, so it reads the old values.

**`local TARGET if COND` gates the SAVE AND RESTORE, never the value** (task
#541). Perl does not execute the statement at all when the condition is
false, so nothing is saved and nothing is restored — and a WRITE to the slot
inside the scope therefore SURVIVES the block. "Localize to the slot's
current value" (what PCL emitted before) is observationally identical only
while nothing writes it: a save and restore of an unchanged slot is
invisible, a save and restore *around a write* is not.

Two conditional forms carry it, and the split between them is **lexical
binding**: `p-local-cell-if` is the ordinary-global twin of `p-local-cell`
and keeps the body IN PLACE, because `p-local-cell` rebinds its name
lexically around the body; `p-local-maybe COND (LOCALIZER…) BODY` carries
every other shape (element, slice, `$.`, `$|`, symbolic deref, and the
exception set's dynamic `let`) by writing BODY once into a local function
called from both arms — a dynamic binding is visible through that call, a
lexical one would not be. The condition is evaluated FIRST and exactly once
(in a temporary when more than one form reads it); the localizer's init form,
and a list assignment's RHS, run only in the true arm — perl evaluates
neither when the condition fails. *Porter mapping:* any host can spell this
as "if COND then (save; install; unwind-protect BODY (restore)) else BODY",
with BODY emitted once.

`foreach $pkgvar (LIST)` is an *implicit* `local` of the loop variable —
the body and everything it calls see the current element, and the old
value is restored on exit, including via `last`/`die`. The loop macros
decide which mechanism to use from the expansion environment (a symbol
macro ⇒ localize the cell; a special ⇒ dynamic `let`; a lexical `my` loop
var ⇒ plain lexical binding, no localization at all).

### 7.3 Method dispatch

- Method names travel as **strings**. `(p-method-call OBJ "name" ARGS…)`:
  flatten args; find the invocant's class (the box `class` field for an
  object, the package-name string for `Class->method`); walk the class's
  **C3 linearization** (backed by CLOS proxy classes `plc-*`, one per
  package, whose superclass edges mirror `@ISA`); the first package
  defining the method wins. Method-name string → function mapping goes
  through the `%pcl-cl-sub-name` registry (case-preserving).
- `SUPER::name` dispatches starting *after* the current sub's home
  package in the linearization, and **finishes the lookup exactly like an
  ordinary method call** (s446m, #533): UNIVERSAL and UNIVERSAL's own
  `@ISA`, then the `isa`/`can`/`DOES` built-ins, then `import`/`unimport`
  as no-ops, then `AUTOLOAD` searched **from the parents** (never the
  current class's own) with `$AUTOLOAD` set to `Current::SUPER::method`,
  and only then a trappable `Can't locate object method … via package
  "CURRENT"` — perl names the current class there, not the parent.
  `$obj->$coderef(@a)` calls the code ref directly with the invocant
  prepended.
- **A no-op `import`/`unimport` returns the EMPTY LIST in list context and
  `undef` otherwise** (s448p, #534) — one helper, `%pcl-no-op-import-result`,
  answers for every arm that reaches the question (the three inside
  `p-method-call` and `%pcl-super-fallback`). The empty list is an empty
  **vector**, the value `sub { return () }` yields; it must NOT be a
  `p-flatten-marker`, which only the argument-spreading walkers recognise —
  a bare one reaching a list-assign, a `foreach` list, a hash init or a
  `print` is stored as ONE element and stringifies as the struct.
  Since s463au that vector comes from **`(%p-empty-list)`**, the ONE producer
  of Perl's empty list as a runtime value (`%pcl-no-op-import-result`,
  `p-return-value`'s list-context `nil` arm and `do-regex-match`'s no-match
  arm all call it). Raw `nil` is the empty list only where `%p-flatten-list`
  reads it; a translator that emits raw `nil` anywhere else gets an array
  hole or a phantom argument instead.
- **Dispatch resolves per call — nothing about the resolution is cached**
  (the USER's cache-free ruling, s444; measured in s446m). A method
  glob-assigned or redefined after an object has already dispatched, and a
  runtime `@ISA` rewrite, must be visible on the very next call. What *is*
  memoized is name→package (the stash table, perl's `gv_stashpv` shape)
  and name→`pl-NAME`, both pure functions of a name. Guard:
  `Pl/t/method-dispatch-01.t`.
- `AUTOLOAD` is honored (walks `@ISA`, skips `DESTROY`). `can`/`isa` work.
  `DESTROY` is **never called by GC** (documented divergence).
- PCL always linearizes with C3 (stock Perl defaults to DFS; documented
  divergence — `docs/not-supported.md` §mro).

### 7.4 Scheduled blocks

`BEGIN`/`CHECK`/`INIT`/`END` compile to definitions-stream forms that run
at their Perl-mandated points relative to the *file's* load: `BEGIN`
bodies execute as soon as their form loads (before later statements),
`END` blocks run at program exit in LIFO order. Since s300b (#55) the
scheduled forms sit **at their source position among the `p-sub` defs**
(see §1 file layout), so a `BEGIN` observes exactly the subs defined
above it — `defined &f`/`->can` snapshots at compile time match Perl.
In a **fork child**, `END` blocks inherited from the parent still run at
the child's exit (as in Perl); test-harness plan-checking is pid-guarded
separately and is not an IR concern.

### 7.5 Bareword filehandle NAMES (normative, s443f)

A bareword handle is a **name**, and the emitted CL carries it as a symbol
(`(p-open FH …)`, `(p-close FH)`, `(p-readline 'FH)`, `(p-print :fh 'FH …)`;
the `%p-fh-arg` family quotes an unquoted bareword itself, the other two
quote it in the emission). Perl names the handle by its *glob*, so one
handle has more than one spelling, and **the emitter canonicalises the name
so that one handle is one symbol** — a translator that keys handles by the
spelling as written will make two handles out of one:

* the eight handles perl forces into `main::` from every package — `STDIN`
  `STDOUT` `STDERR` `ARGV` `ARGVOUT` `ENV` `INC` `_` — resolve in `main`
  wherever they are named **unqualified**, so `main::STDOUT` ≡ `STDOUT`;
* every other bareword handle belongs to the package that names it, so
  `main::FH` ≡ `FH` in `main`, and `package P; open(P::FH,…)` ≡
  `open(FH,…)`;
* **but an explicit qualifier always names that package's own glob.**
  `Foo::STDOUT` is NOT `STDOUT` — inside `package Foo`, `print STDOUT "x"`
  writes and `print Foo::STDOUT "x"` writes nothing and returns undef. The
  collapse in the second bullet therefore does not apply to the eight names
  in the first.

A qualifier PCL cannot collapse stays in the name. Since a host symbol
`Foo::H1` needs the namespace `Foo` to exist at read time, the emitter
registers that namespace (the same `p-defpackage` a qualified call emits)
and quotes the designator so the reader's namespace choice survives
`%p-fh-arg`; a qualified spelling of one of the eight is emitted as a single
`|Foo::STDOUT|` symbol instead, so it cannot be found by the runtime's
resolve-by-short-name fallback. Perl's stash autovivifies, so `Foo::H1` is a
usable handle whether or not the program contains `package Foo`.

**In an argument slot, a bareword handle is its NAME as a plain string.**
A `*`-prototype slot of a *user* sub receives `"FOO"` — and `"G"` even when
`G` is an open handle, never a glob — while a name that is *callable* at
that point is called instead.

**The builtin handle slots are TWO groups, not one, and the discriminator is
the slot's shape** (re-measured s465ba, perl 5.40.3,
`scratch/p17-builtin-slot-rule.pl` + `p18-slot-warning-names.pl`; the earlier
one-group claim here was wrong, and its own example was the case that
disproves it):

* **Glob slots** — `tell` `eof` `fileno` `close` `binmode` `seek` and the
  `open` family: an ordinary declared sub does NOT displace the bareword, so
  `sub SPATH {…}; tell SPATH` reads the *handle*, not the sub's result.
* **EXPR slots** — `stat` `lstat` and the filetests `-X`: the operand is an
  expression, so a declared sub IS called.  `sub SPATH {"/etc/passwd"}` makes
  `-e SPATH` and `stat SPATH` answer for that path, with or without parens.

**A `()`-prototype sub is inlined BEFORE either rule applies**, and that
cuts across the two groups: `use constant CPATH => "YYPATH"; tell CPATH`
warns `tell() on unopened filehandle YYPATH` — the *value* is the handle name
— and `sub FILE1 () {42}; tell FILE1` is `-1` because the handle is named
`42`, not because the bareword survived.  So a translator must resolve an
inlinable constant first, then apply the group rule to what is left.

PCL follows the EXPR rule for the filetests (the emitter produces the call)
and does NOT yet follow it for `stat`'s paren-argument spelling — task
**#1044**, whose colliding shape occurs zero times across all six populations.

**A dup-open's SOURCE is a handle DESIGNATOR, not a string** (task #513).
`open FH, ">&", SRC` accepts every spelling the resolver accepts — a
bareword or string NAME, a typeglob or a ref to one, a lexical handle, a
stream, or a raw fd NUMBER — so a translator must hand the dup path the
*value*, not its stringification (a glob ref stringifies to `GLOB(0x…)`,
which names no handle, and that is the whole bug the task recorded). The two
FAILURE shapes are different, and the FIRST discriminator is the ARGUMENT
FORM (task #621).  In the **three-argument** spelling perl then distinguishes
by what the designator IS, not by whether it resolved: a NAME that names no
open handle is FATAL (`Bad filehandle: NOSUCH`), a literal `undef` is fatal
(`Can't use an undefined value as filehandle reference`), while a CLOSED
lexical handle or a bad fd number is a plain false with `$!` set.  The
**two-argument** spelling `open FH, "<&SRC"` never dies: every source it
cannot find — unknown name, empty name, qualified name, a lexical handle that
stringified to `GLOB(0x…)` — is a plain false with `$!` = EINVAL, and a bad fd
NUMBER is EBADF there too.

**A dup's DIRECTION is the descriptor's, not the mode letter's** (task #590,
probed 5.40.3): the handle READS iff the descriptor is readable — `open $d,
'>&', $readonly` then `<$d>` reads the file — and WRITES iff the mode asks for
a write AND the descriptor allows one.  Before the dup, the source handle is
FLUSHED (task #591): a buffered read handle has pulled the file past the
program's logical position and a buffered write handle has not yet reached the
descriptor, so without the flush the dup starts at EOF, or its writes overtake
the source's.  A flush of a read handle also discards its buffer, which is why
perl's source handle reads undef after the dup consumed the rest.

**A write the OS refuses is a FALSE `print`, never an exception** (task #590),
and a `close` whose final flush failed returns false while still freeing the
descriptor.  Buffering decides only WHEN the failure is noticed, never
whether: a translator on a host that signals (SBCL does) must convert the
signal at both the write and the close.  A handle that cannot be written at
all — a read-only file handle, an in-memory `<` handle, a dup of a read-only
descriptor — answers false with `$!` = EBADF without attempting the write, and
goes on READING normally.

### 7.6 stdio buffering (normative, s451)

Buffering is not an implementation detail once a program can observe it, and
it can: two handles on one descriptor, a child process, or an abort all make
the mode visible.  perl's policy, probed 5.40.3 and implemented by
`%p-output-buffering` / `%p-std-buffering`:

* an **output handle is LINE-buffered iff its descriptor is a terminal**, and
  BLOCK-buffered otherwise.  The rule is per DESCRIPTOR and is not inherited:
  a dup of STDERR onto a pipe is block-buffered, not unbuffered;
* **STDERR is UNBUFFERED**, on a terminal and off it alike — the one
  exception, and it survives a reopen of the handle;
* **`$|` is the HANDLE's flag and overrides the policy**, per handle, and it
  **survives a reopen of that handle** (`$|=1; open(STDOUT,'>&',DUP)` still
  flushes every write);
* a **dup gets its OWN buffer** (its writes and the source's interleave by
  flush, not by program order) but its own descriptor's MODE;
* a **fork-pipe child keeps the PARENT's mode**: `open(FH,'-|')` dup2s at the
  descriptor level under the same handle, so a child of a terminal-attached
  parent line-buffers onto the pipe.

The decision is **per process**, never per build: a host that saves an image
(PCL's cached core) must re-ask `isatty` when the image boots.

Two consequences a translator must implement or lose output:

* **every child path flushes every handle first** — perl's
  `PERL_FLUSHALL_FOR_CHILD`, at `fork`, `system`, `exec`, backticks and a pipe
  open.  Without it `print "a\n"; system("echo MARK")` prints MARK first;
* **every exit path flushes, INCLUDING the die path.**  perl flushes at exit
  even on an uncaught die, so `print "row\n"; die` still shows the row; a host
  whose abort path skips its exit hooks turns a mid-file abort into silent row
  loss, which for a test file is every row it had produced.

## 8. Magic globals

All are dynamically-scoped boxes exported from the runtime namespace:

| var | semantics |
|---|---|
| `$_` | default topic. **The transpiler materializes it explicitly at parse time** (`add_implicit_default_param` / `_default_filetest_operand` in `Pl/PExpr.pm`, and the print family in `ExprToCL` `gen_funcall`): Perl's omitted-operand forms — `uc;`, `chomp;`, `length;`, bare `-e`, a bare `/re/` match, and bare `print;`/`say;`/`printf;` — arrive in the tree as `(p-uc $_)`, `(p-=~ $_ …)`, `(p-print $_)`, etc. **A translator implements no per-op defaulting** — there is no longer any runtime-side `$_` default. |
| `@_` | current sub's args (lexical per `p-args-body`, §5.2) |
| `$@` | last eval error (§6.3) |
| `$1`…`$N`, `%+` | capture groups; set by the most recent successful match (`p-=~` family); dynamically saved/restored around scopes like Perl.  **TWO SPELLINGS, one meaning (task #851):** `$1`…`$20` are emitted as bare symbols (one runtime special per group — that is what makes a capture read a variable read), and `$21` and up as `(p-high-capture N)`, which reads element N−1 of `@{^CAPTURE}`.  The two agree — `@{^CAPTURE}` is filled by the same `set-capture-groups` that writes the specials, and it is complete — so **20 is a SPEED boundary, not a semantic one** and a translator may implement either spelling as the same accessor.  What PCL does NOT model is that a capture variable is READ-ONLY in perl (`$1 = 5`, `chop $1`, and `open $99` — which autovivifies into an undef scalar — are all `Modification of a read-only value attempted`); under PCL such a write is a silent no-op (task #873) |
| `$0`, `@ARGV`, `%ENV` | program name, args, environment (`%ENV` writes through to the process).  **`$0` is an ORDINARY WRITABLE box** (task #512), initialised by the program preamble to the script the compiler was given — not to `argv[0]`, which under a CL host is the lisp binary and is what no Perl program means by `$0`.  A translator must make it assignable: `$0 = "X"` is a plain scalar store whose value every later read sees, `local $0` saves and restores it, and because a SCALAR in the filehandle slot naming a handle IS that handle (§7.5), `$0 = "H"; print $0 LIST` writes through the handle named `H`.  What PCL does not do — and a host without argv-area access cannot — is the OS-level process rename `ps` reports; see `not-supported.md`. |
| `$!` | last OS error (dualvar: numifies to errno, stringifies to message).  **Also a CANONICAL MAGIC BOX** — see the rule below. |
| `%!` | the errno hash: one key per platform errno NAME, each value magic.  `$!{NAME}` is the errno NUMBER when `$!` holds that errno and `0` otherwise — never `1`, and always defined; a STORE is fatal (`ERRNO hash is read only!`), as Errno's tied hash is.  `keys`/`values`/`each`/`exists` are ordinary hash reads of a REAL table. |
| `%SIG` | signal handlers. **Pre-populated at load with every platform signal name, values undef** (`*p-signal-numbers*`, Config's sig_name order; 67 keys on Linux, `ZERO` excluded exactly as perl does), so `exists $SIG{HUP}` is true before any handler is installed — pragmas like `sigtrap` probe it that way. `__WARN__`/`__DIE__` are *not* keys until assigned. The same table resolves `kill`'s name designators. |
| `$.` | line number of the last-read filehandle (per-handle) |
| `$a`, `$b` | sort comparator operands (per-package defvars) |
| `$\`, `$,` | output record / field separator. **Both are UNDEF until the program sets one** (task #465) — the separator defaults are asymmetric and a translator must copy the asymmetry, not normalize it: `$/` is `"\n"`, `$;` is `"\034"`, `$"` is `" "`, `$!` is the errno dualvar, all DEFINED. An empty string here is invisible on the write side and wrong on the read side (`defined($,)`, `$\ // ","`, `length($\)`), which is what made it silent. `print` treats undef as "print nothing between/after": its readers test *non-empty string*, never `defined`. **`say` appends `"\n"` INSTEAD of `$\`, never as well as it** (task #500), while `$,` still separates its arguments; `printf` appends neither. perl does not *localize* `$\` over the call — an overload or tie handler that runs while an argument stringifies still reads the program's value (probed s442d) — so the terminator is passed to the one writer (`%p-write-list`), not bound over it. |

Regex match state is *global-with-dynamic-save*, exactly Perl: a failed
match leaves `$1` from the previous successful match intact.

**`$&`, `` $` `` and `$'` are DERIVED, not stored** (normative, s450u / task
#477).  A successful match records the subject string and two offsets; the
three variables are cut from them the first time a program reads one, and the
cut is memoised until the next successful match.  This is perl's own rule and
it is not an optimisation a translator may skip: building them eagerly makes
every scalar-context `m//g` loop QUADRATIC, because each of the N matches
copies the whole subject twice (measured before the change: `while ($x =~
/./g) {}` over 100 000 chars 3.4 s, 200 000 chars 12.8 s, where perl does
1 000 000 in 0.09 s).  Two consequences for a translator: a deferred cut is
only correct in a runtime whose string writers COPY (PCL's do — lvalue and
4-arg `substr`, `tr///`, `chop`, `vec`, the magic increment all build a new
string, so `$x =~ /cd/; substr($x,0,1) = "Z"` still answers from the subject
as it was at match time); and the three names then hold no value in their
symbol at all, so the symbolic-reference rule below has to reach them through
their getter rather than through storage.

**A SYMBOLIC scalar reference reaches the magic globals** (normative, s446j /
task #505).  `${"1"}` is `$1`, `${"10"}` is `$10`, `${"&"}` is `$&` — perl
names them like any other package variable, and the value is undef only when
the variable is.  This is a rule about *storage*, which is why it was missed:
a runtime that keeps its magic scalars RAW in their symbol (as PCL does — the
capture groups are not boxes) must not answer a symbolic-ref read from the
box table alone.  **Writing one dies** `Modification of a read-only value
attempted` for the regex-result family — a name that is all digits with no
leading zero, plus `&`, `` ` ``, `'`, `+` — *even for a group the last match
never had* (`${"7"} = 1` dies with no match at all, probed); `${"0"}` (the
program name) and `${"007"}` (an ordinary variable — a capture name has no
leading zero) are writable.  A NUMERIC-valued name (`my $n = 5; ${$n}`) is
perl's same rule, but PCL cannot implement it while the box model drops the
reference wrapper on a container read — task #551 has the measurement.

**A COMPUTED MAGIC GETS A CANONICAL BOX** (normative, s450t / task #561).  A
magic scalar whose *emission* is an ACCESSOR CALL rather than a variable
reference — today `$!` and `$^E`, both `(p-errno-string)` — must ALSO exist as
the variable its glob slot names (`|$!|`, `|$^E|`), holding a magic cell whose
getter and setter ARE that accessor and its `setf`.  The reason is structural:
**a glob slot IS a variable, and a scalar slot's value IS the p-box**, so
`*Y = *!` is box aliasing and needs no per-name case in the glob path — but a
slot with no variable behind it copies nothing, and `$Y` then reads `""` where
perl reads the strerror text.  One state, two doors: the box and the accessor
read the same place and cannot disagree.  The same rule gives `${"!"}` and
`\$!`-through-an-alias their values.

*Boundary (accepted, probed):* perl's `*! = *src` REPLACES the glob, so
afterwards `$!` is `src`'s plain scalar and the magic is gone; PCL's plain
`$!` keeps computing, because its emission never reads the slot.

**Element access names the AGGREGATE, so the sigil swap precedes the magic
table.**  `$!{ENOENT}` is an element of `%!`, not a subscript on the errno
accessor; a container whose scalar spelling renders as a compound FORM is
re-rendered from its `%`/`@` spelling instead (`Pl::ExprToCL::_bare_container_sym`).

## 9. The load model and string eval

Generated files are loaded form-by-form; a `use`/`require` triggers
transpilation (or cache lookup) of the target module and loads it inline,
recursively.

**`%INC` records every successful `use`/`require`, including the ones PCL
does not actually load (normative, s443h/task #511).** Perl's key is the
relative path (`strict.pm`, `File/Basename.pm`) and its value is the file
that was opened; programs read both (`require Foo unless $INC{"Foo.pm"}`,
`if.pm`'s string require, `IO/Handle.pm`'s `!$INC{"IO/File.pm"}`). PCL
deliberately loads no `.pm` for three classes — a lexical pragma (there is no
`$^H` bitmask to set), an XS-only module, and one whose interface PCL
supplies itself (`Test::More` → the TAP layer) — and each still gets its
`%INC` entry, whose value is the file that *would* have been loaded,
resolved through `@INC` (falling back to the relative path when `@INC` does
not hold it). Codegen emits `(p-note-inc "strict")` for a pragma `use`/`no`
(compile phase, since perl's is a `BEGIN`) or `require` (in place); `p-use`
records the other two on its own no-load exits. `p-note-inc` **loads
nothing**, so it must never be emitted for a module that might still have to
arrive — an entry satisfies `p-use`'s already-loaded guard, and `no Moose`
(which perl requires) is deliberately not routed through it. Not yet
recorded: `use constant` / `vars` / `lib` / `base` / `parent` / `overload`,
which PCL also handles without a load.

**The COMPILE PHASE of the whole file precedes the RUN PHASE of any of it
(normative, s436).** Perl compiles a file before it runs a line of it: every
named sub is defined and every `BEGIN` has run before the first run-time
statement, no matter where in the file they sit. A generated file reproduces
that with two groups of top-level forms, in this order:

1. **compile phase**, one group per package section in source order — the
   section's package preamble and `(in-package …)`, its declarations, a
   `package NAME VERSION` section's `$VERSION` assignment (perl sets it as
   the `package` statement is COMPILED, so it precedes every `BEGIN`, `use`
   and sub of the section — s437), its
   captured `use`/`require`/`BEGIN` declarations, and its sub definitions and
   scheduled blocks interleaved by SOURCE POSITION (so a `BEGIN` sees exactly
   the subs written above it and none below);
2. `(p-run-compile-phase-blocks)` — UNITCHECK and CHECK in reverse
   registration order, then INIT in source order (§the phase boundary);
3. **run phase**, one group per section in source order — an `(in-package …)`
   to put the READER back where that section's symbols resolve, a
   `(p-set-current-package …)` for `caller()`/`__PACKAGE__`, then the
   section's run-time forms.

A section is a top-level `package` switch, so a single-package file has one of
each group and the split is invisible. The rule is load-bearing exactly when a
file has more than one: emitting a whole section at a time — its run phase
included — puts a LATER section's compile phase after an EARLIER section's
run-time code, which perl never does (tasks #456, #469; the two shapes are a
cross-section forward call and a `BEGIN` that could see a run-time
assignment). A translator that emits sections whole reproduces the bug.

Note what may NOT be done instead: hoisting a definition on its own, above its
own section's declarations, compiles that section's `p-defcell` symbol-macro
as a plain free variable. The unit is the whole compile phase.

**Declarations are define-once.** Both declarers initialize a variable only
when it is not already bound — `defvar` by definition, `p-defcell` by an
explicit `boundp` guard. This is load-bearing, not tidiness: the same name
can be declared by more than one section of a file *and* by more than one
file, and a module can be loaded twice; an unconditional initialization
would wipe a value an earlier declaration's code had already assigned. A
translator that lowers these declarations to plain assignment introduces a
silent wrong. `eval "string"` calls the transpiler *at runtime* on the
string (`docs/eval-lexical-capture.md` is the original design note;
§9.1 below is normative). Calling context does not propagate into
string eval (documented divergence). Translators targeting environments
without a runtime compiler must either bundle one or reject `eval EXPR`
programs — there is no static escape.

### 9.1 The string-eval protocol (normative, s295)

String eval must let the eval'd code *read and write* the enclosing
scope's `my` variables by their **source names**, even though the
compiler may have let-bound, renamed, or package-promoted those
variables. Perl's own mechanism is pad lookup at the eval site; PCL
reproduces it with three cooperating pieces. An implementer in another
language needs exactly these three; each is described by its observable
contract.

**Piece 1 — the eval site: `(p-eval STRING ALIST)`.** At every `eval
EXPR` call site (literal or dark/dynamic string — the two are handled
identically), codegen passes an alist literal of the **let-bound**
lexicals in scope:

```lisp
(p-eval $code
  (list (cons "$x" $x__shadow__2) (cons "$x" $x) (cons "$y" $y__lex__0)))
```

Each key is the variable's *original Perl source name* (the string the
eval'd code will use); each value is the live container — a scalar box,
array, or hash — that the compiled code binds under its (possibly
renamed) symbol. Keys may repeat: resolution is first-match, so the
alist is ordered **innermost binding first**. Concretely,
`_eval_lexical_alist` strips the `__lex__N`/`__shadow__N`/`__file__N`/
`__cond__N` rename suffixes to recover the key and, within one key, orders shadow renames
by descending `N` (deeper shadows have higher counters) with the plain
unrenamed name last. After the let-bound pairs it appends the
**cross-package span pairs** (see piece 3). The alist is rebuilt at each
call — it snapshots which bindings are live at that site, not their
values (values live in the shared containers).

**Piece 2 — the eval body: free variables become parameters.** The
runtime transpiles the string in the caller's Perl package (result
cached under `(string . package)`), then reads and evaluates the
generated forms one at a time (so an `(in-package …)` inside the eval
text takes effect before later forms are read). Variables that are
*free* in the eval'd code — used but not declared by it — are compiled
to parameters of a wrapper lambda:

```lisp
(p-eval-thunk '("$x" "$y") (lambda ($x $y) …body…))
```

`p-eval-thunk` resolves each name via `p-eval-lex-lookup` (piece 3) and
applies the lambda to the resulting containers.

An eval whose whole body is one `package X; …` region takes a third
argument — the region's CL package designator:

```lisp
(p-eval-thunk (list "$Z") (lambda ($Z) …body…) :|Foo::Bar|)
```

With it, `p-eval-thunk` binds `*package*` to X's CL package (find-or-
create, `:use '(:cl :pcl)`) around **both** the free-name resolution and
the body. Perl says the current package inside such a region *is* X, and
every unqualified-name resolution in the runtime asks `*package*` that
question: `p-eval-lex-lookup`'s stop 2/3 below, `%p-symref-box` and its
array/hash siblings, `p-use`'s default import target, `p-bless`'s empty
class, the symbolic funcall/coderef resolvers. Without the argument
(any eval that is not a single region) `*package*` stays the caller's,
which is equally what Perl says. The thunk is emitted whenever a region
package is present, even with an empty free-name list — the binding's
main effect is on the body. The eval TEXT was read in the caller's
package before the thunk runs, so the lambda's own symbols are
unaffected by the binding.

Because the containers
are bound as ordinary lexical parameters, everything inside the body —
including closures and **named subs the eval defines**, which outlive
the eval — captures the *containers themselves*. Writes (`$x = 84`
box-sets the scalar box) are visible to the enclosing compiled code and
vice versa, with no copy-back step.

**The pad-chain continuation (`%p-eval-env%`, s383/#295).** Perl
compiles eval'd text *in the pad chain of the eval site*: an eval site
**inside** the eval'd text sees the text's own lexicals and then the
enclosing scope at the outer site — and a named sub the eval defines
keeps that chain when called after the eval has returned. The site
alist reifies the outer link, but `*p-eval-lex-alist*`'s dynamic
binding dies with the eval, so the compiled body lexicalizes it: when
any eval site occurs in the compiled text, the body (inside the thunk
lambda when one is emitted, as the single body form otherwise) is
wrapped in

```lisp
(let ((%p-eval-env% pcl:*p-eval-lex-alist*)) …body…)
```

and every eval site *emitted in eval mode* appends `%p-eval-env%` to
its site alist — `(p-eval $code %p-eval-env%)`, or
`(p-eval $code (append (list own-pairs…) %p-eval-env%))` when the site
has let-bound pairs of its own (own pairs first: they are inner scope).
Named subs close over `%p-eval-env%` like any lexical, which is what
makes the chain survive the eval's extent; nested evals thread it (each
nesting level's `%p-eval-env%` binds to the alist its own `p-eval`
received, which already carries every outer level). This is **not** a
fourth resolution stop — it rides the site alist, stop 1 below.
Implementers must not substitute an ambient/dynamic hand-off: a sub
merely *called* from eval'd code must not see the eval's lexicals, and
only lexical capture makes that distinction.

**Piece 3 — name resolution: `(p-eval-lex-lookup NAME)`.** Exactly
three stops, in order:

1. **The site alist** (`*p-eval-lex-alist*`, dynamically bound by
   `p-eval` for the extent of the eval): first `string=` match wins.
   This is how let-bound lexicals — including seam shadows — are found,
   and why alist order encodes shadowing depth.
2. **The named global of the current package**: intern NAME (through the
   same case transform the reader applies to generated code) in the
   package current at the eval site; if that symbol is bound, its value
   is the container. This is how file-scope lexicals are found — see
   the alias rule below.
3. **Autovivify**: an unbound name yields a fresh container chosen by
   sigil (`$` → undef box, `@` → empty array, `%` → empty hash),
   matching Perl's global autovivification inside eval. The fresh
   container is **installed** as the symbol's global value (s304), so a
   later eval of the same name finds it at stop 2 — a package global
   used *only* inside eval strings persists across evals, exactly like
   Perl's autovivified global. (Before s304 this accidentally worked
   via a phantom `defvar` the enclosing file emitted from scanning the
   eval string literal's innards; that scan bug is fixed, so the
   install is now load-bearing.)

**The alias rule (v2 renamed cells).** v1 `defvar`s every file-scope
lexical under its *original* name, so stop 2 finds it for free. v2
renames such cells (`$x__file__N`, §2b.3) precisely so they cannot
poison unrelated `let`s — which would make them invisible to stop 2.
The fix is one runtime primitive:

```lisp
(p-alias-eval-cell '$x $x__file__0)   ; (setf (symbol-value '$x) cell)
```

emitted by codegen **at the renamed declaration's run position,
immediately after its initializing assignment** (after, so the decl's
own RHS — including an eval in it — still resolves the name to the
*outer* variable). It stores the cell's container as the value of the
original-name symbol in the declaring section's package (the quoted
symbol is unqualified: the reader interns it under the section's
`in-package`, which is by construction the declaring package). Both
rename families that produce file cells flow through it: span promotion
(W10) and capture promotion (M-C/M-F).

Consequences an implementer must preserve:

- **One storage location per name.** The alias target is the *same*
  global slot a plain un-renamed file lexical or package global uses.
  There is deliberately no side registry: two storage locations with a
  fixed precedence would let a stale entry in one permanently shadow a
  live binding in the other (the s294 registry bug). Any later
  declaration of the same name — renamed (re-alias) or plain (defvar +
  assign on the same symbol) — takes the slot over from the moment it
  *executes*, giving v1's time-ordered last-declaration-wins model.
- **Emission is gated per file** (`_file_has_str_eval`): the alias call
  is emitted only when the file contains at least one non-block `eval`.
  A file without string eval is **byte-identical** to before and pays
  zero runtime cost; the gate may over-fire on non-eval uses of the
  word (`->eval`, `eval =>` …), which adds only an inert one-write
  call. Dark strings cost nothing beyond literal ones — the mechanism
  never inspects the eval'd text at compile time.
- **Nested/late evals work with an empty alist.** A sub defined inside
  an eval string, called later, whose body does `eval '$x'`: the site
  alist is empty by then, and stop 2 finds the aliased cell — the same
  path v1 takes. Nothing from any site alist is ever persisted.
- **Cross-package spans still need site pairs.** Stop 2 interns in the
  *eval site's* package, so a cell aliased in the declaring package is
  invisible to an eval in a later `package` segment. For those, the
  span pass records original-name → package-qualified-cell pairs per
  extent segment, and piece 1 appends them after the let-bound pairs
  (`(cons "$x" MAIN::$x__file__0)`). They are position-static and carry
  no lifetime hazard.

**Deliberate divergences** (all shared with v1, listed in
`docs/not-supported.md` where user-visible): after the alias executes,
an explicit fully-qualified `$Pkg::x` names the lexical's cell rather
than a distinct package global — exactly v1's defvar-under-original-name
behaviour. Calling context (`wantarray`) does not propagate into the
eval. Only scalar cells are aliased today; a file needing
container-capture-by-eval, or a renamed cell nested inside an outer
`my` of the same name (whose alist pair would always win stop 1), gates
to v1 (§2b.4).

### 9.2 The generation stamp is a promise (normative, s402)

**Line 1 of every file PCL emits is `;;; pcl: pipeline=v2 gen=<generation>`,
where `<generation>` is the value of `cl/pcl-runtime.lisp`'s
`*pcl-cache-generation*` at the moment of emission.** It is a comment, so it
carries no semantics for a translator — but it is a *stable interface*, and
tooling is entitled to key on it. Two consequences, both already relied on:

- **Staleness is checkable without re-running the compiler.** The three
  transpiled artifacts checked into the tree (`cl/pcl-pack.lisp`,
  `cl/pcl-mro.lisp`, `cl/pcl-warnings.lisp`) are just emitted files, so their
  line-1 stamp says which compiler built them. `Pl/t/artifact-staleness-01.t`
  compares each stamp against the live generation, which is why an
  emission-changing commit that forgets `tools/rebuild-pack` fails a row the
  same session instead of drifting for forty generations (#331). The
  artifacts are *discovered by the stamp*, not by a list — anything under
  `cl/` whose first line carries it is an artifact and is checked.
- **The module cache keys on it.** A cached module transpile is only reused
  when its generation matches, so bumping the generation string is what makes
  an emission change take effect for already-cached modules.

Two obligations follow, and both are gated: an emission-changing commit
**bumps** the generation (or stale cache entries are silently reused), and it
**regenerates the artifacts** in the same commit. The stamp's *format* is
part of the promise: a single line, first line of the file, matching
`^;;; pcl: pipeline=\S+ gen=(\S+)`. `Pl/t/no-hardcoded-paths-01.t` cites it
too — it is the marker that tells a generated artifact apart from a
hand-written source file, which is the difference between "this path is a
bug" and "this path is a build-machine artifact of the emitter".

### 9.3 The drop form: a statement the compiler could not lower (normative, s435)

Where a statement of the source program could not be lowered, the emitter puts
this in its place:

```lisp
(progn ;; PARSE ERROR: <reason>
  (p-die "PCL: statement not supported at FILE line N: <source text> -- <reason>
"))
```

**What it means at run time.** Reaching the form signals a Perl `die` carrying
that message. Every statement before it has already run; every statement after
it *in the same top-level form* does not. It is trappable exactly like any
other `die` — `eval { … }; $@` sees it — because it is one. FILE and N name the
**Perl** source, not the generated file; no `(:loc …)` marker is emitted, and
the message ends in a newline, so no ` at FILE line N.` suffix is appended
(§the `p-die` contract, and perl's own rule for newline-terminated messages).

**The unit of the decision is the statement; the unit of the LOSS is the
enclosing top-level form.** The compiler decides per statement, but a signalled
condition unwinds to whatever handler the loader established, and that is one
top-level form. A file-level `my` compiles to a `let` around the whole
remainder of the file, so a drop under it costs every row after it in that
form. This is not a regression against Perl: an untrapped run-time `die` at
file scope costs perl the whole remaining program, so a translator that loads
form-by-form with per-form recovery (as PCL's measurement runners do) loses
strictly less. A translator that loads a generated file as ONE unit will lose
the remainder, exactly as perl does.

**What a translator must do: reproduce the die.** Do not lower it to a no-op.
A silently discarded statement is the #138 family — the worst failure mode in
this codebase — and this form exists to end it: the statement takes a test row
or a side effect with it while every count still looks healthy.

**One shape, every drop, every mode.** There is no classifier in the emitter
that makes some drops die and others continue. The distinction between "a gap
PCL intends to close" and "a feature PCL deliberately refuses" lives in
`baselines/parse-error-drop-census-s399.tsv` and `docs/not-supported.md`, never in
the emission — a classifier here would be asymmetric in the dangerous
direction, since one miss would kill a whole file. Module mode is covered by
construction: the die is *in* the emission, so it survives the module cache
(the transpile-time stderr announcement is suppressed there, ruled s403 —
the statement now says so itself, when it is reached).  A module whose drop sits
in a sub BODY loads; its load-time code is a run phase like any other, so a
load-time CALL into that sub dies at `require` time — trappable by the `use`'s
own `eval`, as perl's compile error would be (measured s436: Text::Balanced's
`gen_delimited_pat`, called from the module's top level, took all 958 board
rows until #457 closed the drop; ruled s437).

**The comment is load-bearing to tooling, not to semantics.** PCL's census,
`tools/corpus-diff.pl`'s SILENT-DROP counter and both measurement runners'
`drops` column all find drops by the exact text `;; PARSE ERROR:`. A
translator may discard the comment; PCL's emitter may not reword it.

### 9.3b The refusal form: a statement PCL deliberately declines (normative, s466)

A **ruled refusal** — a construct PCL recognises and will not translate, each
one carrying an entry in `docs/not-supported.md` — takes the same shape, with a
different marker and message:

```lisp
(progn ;; RULED REFUSAL: <reason>
  (p-die "PCL: <reason>, at FILE line N
"))
```

Everything §9.3 says about the drop form's run-time meaning holds verbatim:
reaching it signals a Perl `die`, trappable in `eval { … }`; the unit of the
decision is the statement and the unit of the loss is the enclosing top-level
form; a translator must reproduce the die and never lower it to a no-op.

Two things differ, both deliberate:

* **The marker is `;; RULED REFUSAL:`, never `;; PARSE ERROR:`.** The second
  string is the drop census's key, and the census exists to be *shrunk* — it
  counts compiler gaps PCL intends to close. A refusal is not a gap: no fix to
  the term grammar will ever close it. A translator may discard both comments;
  PCL's emitter may not merge them.
* **The message is the feature's own wording**, `PCL: <reason>, at FILE line N`
  — byte-identical to the text the refusal used to raise at transpile time and
  to the text `docs/not-supported.md` documents — rather than the drop form's
  `statement not supported at F line N: <text> -- <reason>`.

**In `eval STRING` a refusal still arrives at transpile time**, landing in
`$@`, because the emission is discarded on error and there is nothing to carry
a run-time form; that is also what perl does with code that does not compile.
The same is true of a drop (§9.3, task #363).

Before s466 a refusal aborted the whole transpile. That was a bug in the
refusal, not a property of the feature: `perl-tests/state.t` and perl's own
`t/op/state.t` each contain one `given` block in an otherwise supported file
and each lost every one of its ~160 rows to it (USER ruling, s465; task #1037).
The three refusals that are *not* statement-level — the `our`-alias region, the
oversized generated form, and the two string-eval refusals — each say why in
their `not-supported.md` entry.

## 10. Op inventory — family rules

The full inventory is the export list of the runtime namespace (~500
symbols; `grep '#:p-' cl/pcl-runtime.lisp`). You do not need each one
individually: every op follows its family's rule, and each `p-*`
function's docstring states its Perl contract. The families:

| family | members (representative) | rule |
|---|---|---|
| numeric ops | `p-+ p-- p-* p-/ p-% p-** p-<< p->>` | numify operands (§3.1), return raw number; overload hook first; `/` yields a double when inexact; `%` follows Perl sign rules; the shifts truncate to integer (Inf→0) and clamp a shift count ≥ the word size to 0 |
| bitwise (**mode-dispatched**) | `p-& p-\| p-^ p-~` · always-string twins `p-str-bit-and p-str-bit-or p-str-bit-xor p-str-bit-not` (`&. \|. ^. ~.`) | overload hook first; then ONE mode decision (`%p-bitwise-operand-kind`): the op is NUMERIC iff an operand carries a number, else it STRINGIFIES both operands and operates byte by byte (`&` truncates to the shorter, `\|`/`^` pad with NUL). A reference, glob, `qr//`, blessed object or undef carries neither a number nor a string body and therefore goes to the STRING side — `undef \| "abc"` is `"abc"`, `[1] \| ("\0" x 13)` is `"ARRAY(0x…)"`. **Unary `~` differs on one state and only one**: a REFERENCE goes to its numeric side (the complemented address), because perl's `pp_complement` takes the string branch only for an SV that has a PV. A bit-STRING op whose operands hold a code point above 0xFF is FATAL, with perl's wording. PCL's stand-in for perl's per-SV "used as a number" flag is `looks-like-number`, so a numeric-LOOKING string still takes the numeric side (`"12" & "10"`; task #1040) |
| numeric compare | `p-== p-!= p-< p-> p-<= p->= p-<=>` | numify; return `1`/`""` (`<=>` −1/0/1; NaN comparisons → `""`/undef) |
| string ops | `p-. p-x p-lc p-uc p-lcfirst p-ucfirst p-length p-substr p-index p-reverse p-sprintf p-join` | stringify operands (§3.2), return raw string; Perl's `$_`-default forms arrive with `$_` already explicit in the tree (§8) |
| string compare | `p-eq p-ne p-lt p-gt p-le p-ge p-cmp` | stringify; return `1`/`""` |
| logical | `p-&& p-\|\| p-// p-! p-not` | short-circuit macros returning operand values (§3.4) |
| assignment | `p-my-=` (boxed lexical) `p-scalar-=` (package) `setf` (raw slot) `p-array-= p-hash-= p-list-=` | store per §2.2; **a list assignment used as a VALUE is two-faced (task #721): scalar/void yields the number of elements the RHS produced, LIST context yields the LHS *lvalues* after the assignment** — every slot the LHS consumed, including the ones an `undef` placeholder or an element/`x`-repeat target took, and `()` therefore yields nothing while `(undef) = (10,20,30)` yields `10`. A named scalar target contributes its BOX (writable: `$_++ foreach ($x,$y) = (…)`); a slot with no nameable box contributes the value that landed in it. Because the answer is context-dependent, a DECLARATION whose assignment is a block's tail value must be lowered in the caller's context, never frozen to scalar. All forms return the assigned target/value per Perl |
| compound assignment | `p-incf p-decf p-*= p-/= p-%= p-**= p-.= p-str-x= p-bit-and= p-bit-or= p-bit-xor= p-<<= p->>= p-str-bit-and= p-str-bit-or= p-str-bit-xor=` (any place) · `-raw` twins of each (raw slot, §2.2) · `p-and-assign p-or-assign p-//=` (no raw twin) | read-modify-write; boxed macros store back via box-set/setf per place shape, `-raw` twins are `(setf slot NEW)` with the identical NEW form; `&&=`/`||=`/`//=` short-circuit and store the RHS unchanged |
| increment | `p-++ p---- p-++-post p----post` · on raw slots `p-incf-raw`/`p-decf-raw` (statement-root only; tail postfix wraps in `prog1` for the old value) | numeric ±1 on the box/slot; `p-++` on a pure-alpha string does Perl string increment (`"az"→"ba"`) — a raw slot only takes root incdec when every write is numeric-valued (A-num), so the raw twins are never asked to do the magical form |
| elements | `p-aref p-gethash` (read) `(setf p-aref/p-gethash)` / `p-setf` (write) `p-exists p-delete p-aslice p-hslice` | reads unbox scalars, keep reference boxes (§2.3–2.4); writes through `p-setf` autovivify intermediate refs; `p-delete` returns the removed value |
| slice delete | `p-delete-hash-slice p-delete-array-slice p-delete-kv-hash-slice p-delete-kv-array-slice` | every one flattens its key/index arguments alike (`%p-flatten-slice-args`: a range or interpolated list contributes its elements, a STRING is one key — task #394), and every one answers **nil for an EMPTY slice** — undef in scalar context, the empty list in list context, per [perl #29127].  The emptiness test comes BEFORE the read-only check: perl allows `delete @ro[()]` on a read-only array and dies only on a real index (probed, s414) |
| array/hash builtins | `p-push p-pop p-shift p-unshift p-splice p-keys p-values p-each p-sort p-map p-grep p-wantarray p-scalar p-defined` | Perl signatures; `p-sort` default is string order, comparator lambda gets `$a`/`$b`; `p-defined` returns `1`/`""`.  `p-sort` also has a *sugar* form with no comparator, `(%p-sort-classic MODE ARGS…)` — §5.4; expand it back to `p-sort` and nothing is lost |
| regex | `p-=~ p-!~` with `(p-regex "/pat/flags") (p-subst …) (p-tr …)` | match/substitute/transliterate against a box (writes back for s///, tr///); sets §8 match state; list context returns captures; `p-split`.  **A FAILED `m//` answers by context and by NOTHING else (tasks #962/#459):** scalar/void gives perl's defined-false `""` (never `undef`, never `0` — the `$&`-family rule of #416), and LIST context gives **the EMPTY LIST**, whatever the pattern — a capture-less miss is not a one-element false value.  The empty list is spelled `(%p-empty-list)`, a zero-length vector, never raw `nil`: only `%p-flatten-list` reads raw `nil` as "no elements", while `p-array-fill` keeps it as an array HOLE and `p-flatten-args` spreads it as ONE argument, so `f(/nomatch/, "d")` handed the callee two arguments where perl hands one and every later argument shifted |
| compiled regex (qr) | `(pcl::p-qr "qr/pat/flags")` literal · `(pcl::p-regex-from-parts PAT "flags")` interpolated | A **Regexp object**, not a string: it carries its own flags and identity. It stringifies as perl's `(?^flags:SOURCE)` wrapper — from the SOURCE text as written, never from any backend-rewritten form, and `/xx` prints both x's (a one-x wrapper silently demotes an interpolated pattern to `/x`). Two rules a translator must implement, both about the wrapper (s322, task #181): **(1)** a pattern that is exactly ONE interpolated qr *is* that qr — `qr/$re/` and `/$re/` keep `$re`'s own flags and **ignore the outer modifiers** (`qr/$re/i` on `qr/abc/` does not match `"ABC"`), so the check must happen where the operand is still the object; **(2)** a qr used as PART of a larger pattern embeds its wrapper verbatim (`qr/x$re/` → `(?^:x(?^:abcdef))`), which is what keeps the inner flags scoped. Consequently a variable holding a qr must NOT be frozen to its string form by any raw-slot/unboxing optimization (`write-object` in `Pl/VarAnnotator.pm`): the stringification is lossy and is re-parsed by the next regex that interpolates it |
| I/O | `p-print p-say p-printf` (`:fh HANDLE` key) `p-open p-close p-readline p-eof p-binmode …` | Perl builtins; bareword handles are symbols; `p-open` boxes its handle argument. 2-arg `p-open` parses pipe/dup modes (s301, #70): `"|-"`/`"-|"` **fork** (returns child pid to the parent / `0` in the child, whose STDIN/STDOUT is rewired to the pipe; with command text the child execs it — `"| cmd"`/`"cmd |"` are the classic spellings); `p-close` on a pipe handle **reaps the child, sets `$?`**, and is true iff exit 0. Dup modes: `">&FH"`/`"<&FH"` dup the fd (fresh descriptor; onto the well-known fd for STD handles), `">&=FH"`/`">&=N"` are fdopen-style — same fd or stream alias, no dup |
| command capture | `p-backtick` (`` `CMD` ``, every `qx` delimiter, `` <<`TAG` `` and the NAMED `readpipe EXPR` — ONE runtime function, four surface syntaxes) | **wantarray-sensitive, exactly like `p-readline`** (task #731): scalar/void yields the whole captured stdout as one string, LIST context yields it SPLIT INTO `$/` RECORDS, each keeping its separator — so empty output is the empty list in list context and `""` in scalar.  The split uses `%p-read-record`, the same `$/` reader `p-readline` uses, so slurp (`$/ = undef`), paragraph mode (`$/ = ""`) and a custom separator cannot drift apart between the two.  A package that displaced the builtin with `use subs "readpipe"` is called instead, for every one of the four syntaxes (#703/#734) |
| introspection | `p-ref p-bless p-caller p-can p-isa` | §7; `p-caller` returns package but file/line are stubs (divergence) |
| context & frames | `p-list-ctx p-scalar-ctx p-void-ctx p-caller-ctx` (§4) · `p-sort-cmp` (§5.4) | **names, not operations**: each expands to exactly the `let`/`lambda` shape it replaced, so a translator implements the expansion and nothing else. They mark where the IR says "this runs in context C" / "this is a comparator frame" |
| declarations | `p-let` (§2b.2a) · `p-raw-params` (§5.2) · `p-sub` (§5.1) | **names carrying the compiler's own VERDICTS**: a binding's class, a parameter's class, a sub's proven facts.  Every one expands to exactly the form it replaced — the information is free at run time — and every set (classes, `p-let` fact keys, `p-sub` fact keys) is CLOSED: a member outside it is an error at macroexpansion, never a silent pass.  A translator may drop all three vocabularies and still produce a correct program; what it loses is the ability to choose a representation (raw slot vs box, stack vs heap, by-value vs by-reference arguments) without re-deriving the analysis |

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
  (p-foreach ($w @who) :my t
    (p-print (pl-greet $w) "
")))
```

The call carries **no** context bind: `greet`'s body never observes
`wantarray`, so the `insensitive-call` emission rule (`Pl/Passes.pm`) drops
it. The general shape — what you see whenever the callee might look — is
`(p-list-ctx (pl-greet $w))`, §4.

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
