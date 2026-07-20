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
**Verified against:** full per-claim verification at cache generation
v2-7 (2026-07-06); maintained incrementally since (each semantic
emission/runtime change updates its section — standing rule), last
section-level review at **v2-43, 2026-07-20 (s302: §2.2 raw compound-assign
`-raw` twins, task #62 step 1)**. Section references
name the defining function so you can re-verify against the runtime.

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
- **The tree is explicit.** Perl's implicit operands are materialized at
  parse time: `$_` defaults (§8), `@_`/`@ARGV` for bare `shift`/`pop`,
  filetest operands. What you see in the tree is the complete argument
  list — there is **no** runtime-side argument default (bare
  `print;`/`say;`/`printf;` arrive as `(p-print $_)` etc., materialized by
  the codegen like every other `$_`-default form).
- **`(declare …)` forms are host compiler advice** — droppable wholesale.
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
  version key tooling should read.
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

A **coercing compound assignment** on a raw slot (`$n += V` and the whole
`-= *= /= %= **= x= .= <<= >>= &= |= ^= &.= |.= ^.=` family — every op
whose stored value is an operator result, so a raw number/string by
construction) lowers to the boxed macro's **`-raw` twin**:
`(p-incf-raw $n V)`, `(p-.=-raw $s V)`, … — each expands to
`(setf slot NEW)` with the *same* new-value form its boxed macro
(`p-incf`, `p-.=`, …) computes, so the two store disciplines cannot
diverge semantically (task #62; `docs/raw-numeric-verdict.md`).  The
non-coercing `||= &&= //=` store the RHS unchanged (it may be a
reference), so their targets never become raw slots.

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
holes.  Growing an array by **assigning to `$#a`** (`p-set-array-length`,
incl. the `$#a++` idiom) likewise fills the new slots with holes (`nil`),
never with fresh boxes — `exists $a[$i]` on the new slots must stay false
(s295; a port that models holes as "slot absent" gets this for free). `p-aref` (read) **unboxes scalar elements but returns reference
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

### 2b.2 The declaration forms

| Perl | v2 emission |
|---|---|
| `my $x = INIT;` (boxed) | `(let (($x (make-p-box nil))) (p-my-= $x INIT) …rest…)` |
| `my $x = INIT;` (raw slot, §2.2) | `(let (($x INIT)) …rest…)` |
| `my $i = $i + 1;` (self-ref init) | `(let (($i (p-box-init $i))) …)` — CL `let` inits evaluate in the *outer* environment, matching Perl's "RHS sees the outer variable" rule |
| `my @a; my %h;` | `let` binding to a fresh vector / hash table |
| `our $g = V;` | `(defvar $g (make-p-box nil))` hoisted to the section's declarations + `(p-scalar-= $g V)` in place. `our` shadowing a `my` gates to v1 |
| `state $n = 0;` in a **named sub** | native since s277c: the variable is renamed to a per-sub package cell (§2b.3) — `(defvar $n__state__K (make-p-box nil))` + raw once-flag `(defvar $n__state__K__init nil)` hoisted to the declarations; the statement lowers to `(unless $n__state__K__init (box-set $n__state__K INIT) (setf $n__state__K__init t))` followed by the bare cell as the statement value. One cell per named sub = exactly Perl's named-sub `state` semantics. `state` *outside* named subs, in anon subs / map-grep-sort blocks (per-closure instances), list/non-scalar `state`, and blocked renames still gate → v1, which uses its own `$state__<sub>__<name>__N` cells |
| undeclared globals | swept up at assembly time (`_forward_global_decls`, a text scan over the finished section): every referenced-but-never-let-bound name gets `(defvar NAME <fresh container>)` under "Forward declarations"; package-qualified refs get the defvar in *their* package |

The rest-of-block nesting means scope is structural (review doc §2.3):
reading a `)` closes a scope. At file level the `let`s nest the remaining
*runtime* statements of the section — the hoisted definitions sit outside,
which is exactly why the rename families below exist.

Bookkeeping a consumer can ignore but a debugger should know: every `my`
registers in three sets (`_reg_lex`) — `_let_bound_vars` (scoped, drives
the seam's my-vs-package decisions and the string-eval capture alist),
`_live_lex` (scoped, drives capture *gates*), `_all_lex` (cumulative,
drives the never-defvar exclusion).

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
| `$x__file__N` | v2 file-cell promotion (W5/W10) | a file lexical that must be visible outside the `let`s — because a named sub captures it, because a **BEGIN/END scheduled block references it** (their `p-BEGIN` forms live in the definitions stream, outside the runtime `let` chain — s295c, source-position interleaved since s300b: `my $x; BEGIN { $x = 5 }` / END-cleanup idioms), or because it spans a `package` boundary — promoted to a `defvar`'d package-level box (the `our` shape: defvar + `p-scalar-=`, no `let`). The fresh name is the whole point: `defvar $x__file__0` cannot poison an unrelated `let $x` |
| `$Pkg::x__file__N` | v2 spanning refs (W10) | uses of the above from *later package segments* — package-qualified so their section's reader (sitting in its own package) reaches the declaring section's cell |
| `$x__shadow__N` | v2 seam-shadow rename (W8.5) | a `my $x` *inside a block that lowers through the v1 seam* (`map { my $x = … }`, `do { my $x … }`) while an outer lexical `$x` is live. Unrenamed, the seam's defvar-based handling would write through the outer variable (the v1 bug); renamed, the inner block gets its own unique cell |
| `$x__cond__N` | v2 poisoned-condition rename (W8.5) | `if (my $x = …)` / `for (my $x…)` where the *same bare name* is also used outside the construct as a package global. The construct's lexical takes the fresh name so the global keeps `$x` and gets its forward defvar |
| `$x__lex__N` | v1 closure-capture rename | v1's fix for defvar-poisoned closures: a block `my` captured by a nested sub becomes a fresh, never-defvar'd name so its `let` stays truly lexical. Appears in v2 output too, inside seam-lowered map/grep bodies |
| `$x__state__N` (+ `…__init`) | v2 state cells (s277c) | a named sub's `state` variable promoted to a per-sub package cell + raw once-flag (see the declarations table above); same blockers as the other renames |
| `$state__<sub>__<name>__N` (+ `…__init`) | v1 state cells | same idea, v1's spelling — seen in v1-dialect files |
| `--anon-block-N--` | both | hoisted anonymous-block functions (block-form prototype args: `first { … }`, `sort` comparators via the seam) |
| `--pcl-if-ret--N`, `%_args`, `$state…` | both | compiler temporaries; never user-visible names |

For a translator the practical takeaway is reassuring: **renames need no
special handling**. By the time the tree reaches you, a renamed variable
is just an ordinary variable with an unusual name — read its kind off the
emitted shape exactly as for any other name (§2b.2): `__file__` cells are
defvar'd package vars; `__cond__`/`__lex__` are `let`-bound lexicals;
`__shadow__` is whichever the lowering path produced (observed: the v1
seam emits it as a defvar'd cell — sound *because* the name is unique).
The suffixes matter only for mapping output back to source (strip
`__family__N` to recover the Perl name).

### 2b.4 The guard rails (when renaming refuses)

Renaming by token rewrite is only sound when the token walk can *reach
every use*. Each pass checks blockers (`_shadow_rename_blocker`,
`_scan_lex_facts` disqualifiers) and, on any hit, falls back to v1 for
the whole file (the sanctioned gate) rather than renaming unsoundly:

- **Interpolated uses** — `"$x"`, `/$x/`, heredocs: the name lives inside
  a quote token the Symbol walk can't rewrite.
- **Brace-deref** — `${x}`: same reason.
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
  the deeper cell could never win the by-name lookup). The `cond`/
  `state` rename families also keep the blanket string-eval refusal —
  their cells are neither alist-carried nor aliased.
- **`state`** — its per-instance semantics run through the separate
  `state_var_renames` machinery; token-renaming would bypass it.

Where no rename applies and the capture would misbehave, the same
conditions exist as hard *gates* (`_check_sub_captures`,
`_check_my_spanning`, the block-form-arg capture gate): the file lowers
through v1, whose defvar model handles the capture — with v1's known
closure caveats.

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
`(let ((*wantarray* t)) (pl-f …))`. `return EXPR` and sub-tail positions
inherit the frame's context. `p-sub` snapshots the value at entry into
`*pcl-caller-wantarray*` so nested binds inside the body don't lie to
`(p-wantarray)`, which maps t→`1`, nil→`""`, :void→`undef`.

**Statement (void) position — the sub-body regime.** A sub body with more
than one statement (or a single compound) is wrapped ONCE in
`(let ((*wantarray* :void)) …)`; every non-tail statement inside then
trusts that ambient and emits no bind of its own.  The tail (implicit
return) statement restores the caller's context at the innermost
expression-statement level: `(let ((*wantarray* *pcl-caller-wantarray*))
TAIL-FORM)` — a compound tail (if/elsif chain) carries the restore on each
branch's leaf value statement, never around the whole compound (its
non-tail inner statements stay in the :void ambient).  Explicit `return`
needs no restore: the `p-return` macro evaluates its values under
`*pcl-caller-wantarray*` itself.  A body that is a single non-compound
statement skips the regime entirely (no binds at all — the tail already
inherits the caller's dynamic context).  `do{}`/`eval{}` blocks and
map/grep/sort bodies are regime *boundaries*: they run in their own
caller's context, so void statements inside them carry per-statement
`(let ((*wantarray* :void)) …)` wraps.  Toplevel (non-sub) statement
position emits per-statement binds only where the form is
context-sensitive (user funcalls, g-modifier matches).

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
2. runs the body; on success sets `$@` to `""` and yields the body value;
3. on `p-exception` sets `$@` to the payload (string or object) and
   yields `nil`; any other host error is stringified into `$@`.

`$@` is an ordinary global box. `die` with no arguments re-raises `$@`.
`$SIG{__WARN__}` fires on `warn`; `$SIG{__DIE__}` does **not** fire
(documented divergence).

### 6.4 goto

Three source forms, three fates:

- `goto &sub` (tail call) — supported; re-dispatches with the current `@_`.
- `goto LABEL` to a **standalone label** in the same statement list —
  fully lowered (backward and forward; details below).
- **Computed** `goto EXPR` (label name in a variable) — no-op, documented
  divergence (`docs/not-supported.md`).

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

## 8. Magic globals

All are dynamically-scoped boxes exported from the runtime namespace:

| var | semantics |
|---|---|
| `$_` | default topic. **The transpiler materializes it explicitly at parse time** (`add_implicit_default_param` / `_default_filetest_operand` in `Pl/PExpr.pm`, and the print family in `ExprToCL` `gen_funcall`): Perl's omitted-operand forms — `uc;`, `chomp;`, `length;`, bare `-e`, a bare `/re/` match, and bare `print;`/`say;`/`printf;` — arrive in the tree as `(p-uc $_)`, `(p-=~ $_ …)`, `(p-print $_)`, etc. **A translator implements no per-op defaulting** — there is no longer any runtime-side `$_` default. |
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
`_eval_lexical_alist` strips the `__lex__N`/`__shadow__N` rename
suffixes to recover the key and, within one key, orders shadow renames
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
applies the lambda to the resulting containers. Because the containers
are bound as ordinary lexical parameters, everything inside the body —
including closures and **named subs the eval defines**, which outlive
the eval — captures the *containers themselves*. Writes (`$x = 84`
box-sets the scalar box) are visible to the enclosing compiled code and
vice versa, with no copy-back step.

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
   matching Perl's global autovivification inside eval.

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

## 10. Op inventory — family rules

The full inventory is the export list of the runtime namespace (~500
symbols; `grep '#:p-' cl/pcl-runtime.lisp`). You do not need each one
individually: every op follows its family's rule, and each `p-*`
function's docstring states its Perl contract. The families:

| family | members (representative) | rule |
|---|---|---|
| numeric ops | `p-+ p-- p-* p-/ p-% p-** p-<< p->> p-& p-\| p-^` | numify operands (§3.1), return raw number; overload hook first; `/` yields a double when inexact; `%` follows Perl sign rules; bitwise ops truncate to integer (Inf→0) |
| numeric compare | `p-== p-!= p-< p-> p-<= p->= p-<=>` | numify; return `1`/`""` (`<=>` −1/0/1; NaN comparisons → `""`/undef) |
| string ops | `p-. p-x p-lc p-uc p-lcfirst p-ucfirst p-length p-substr p-index p-reverse p-sprintf p-join` | stringify operands (§3.2), return raw string; Perl's `$_`-default forms arrive with `$_` already explicit in the tree (§8) |
| string compare | `p-eq p-ne p-lt p-gt p-le p-ge p-cmp` | stringify; return `1`/`""` |
| logical | `p-&& p-\|\| p-// p-! p-not` | short-circuit macros returning operand values (§3.4) |
| assignment | `p-my-=` (boxed lexical) `p-scalar-=` (package) `setf` (raw slot) `p-array-= p-hash-= p-list-=` | store per §2.2; list-assign in scalar context yields the RHS element count; all return the assigned target/value per Perl |
| compound assignment | `p-incf p-decf p-*= p-/= p-%= p-**= p-.= p-str-x= p-bit-and= p-bit-or= p-bit-xor= p-<<= p->>= p-str-bit-and= p-str-bit-or= p-str-bit-xor=` (any place) · `-raw` twins of each (raw slot, §2.2) · `p-and-assign p-or-assign p-//=` (no raw twin) | read-modify-write; boxed macros store back via box-set/setf per place shape, `-raw` twins are `(setf slot NEW)` with the identical NEW form; `&&=`/`||=`/`//=` short-circuit and store the RHS unchanged |
| increment | `p-++ p---- p-++-post p----post` | numeric ±1 on the box/slot; `p-++` on a pure-alpha string does Perl string increment (`"az"→"ba"`) |
| elements | `p-aref p-gethash` (read) `(setf p-aref/p-gethash)` / `p-setf` (write) `p-exists p-delete p-aslice p-hslice` | reads unbox scalars, keep reference boxes (§2.3–2.4); writes through `p-setf` autovivify intermediate refs; `p-delete` returns the removed value |
| array/hash builtins | `p-push p-pop p-shift p-unshift p-splice p-keys p-values p-each p-sort p-map p-grep p-wantarray p-scalar p-defined` | Perl signatures; `p-sort` default is string order, comparator lambda gets `$a`/`$b`; `p-defined` returns `1`/`""` |
| regex | `p-=~ p-!~` with `(p-regex "/pat/flags") (p-subst …) (p-tr …)` | match/substitute/transliterate against a box (writes back for s///, tr///); sets §8 match state; list context returns captures; `p-split` |
| I/O | `p-print p-say p-printf` (`:fh HANDLE` key) `p-open p-close p-readline p-eof p-binmode …` | Perl builtins; bareword handles are symbols; `p-open` boxes its handle argument. 2-arg `p-open` parses pipe/dup modes (s301, #70): `"|-"`/`"-|"` **fork** (returns child pid to the parent / `0` in the child, whose STDIN/STDOUT is rewired to the pipe; with command text the child execs it — `"| cmd"`/`"cmd |"` are the classic spellings); `p-close` on a pipe handle **reaps the child, sets `$?`**, and is true iff exit 0. Dup modes: `">&FH"`/`"<&FH"` dup the fd (fresh descriptor; onto the well-known fd for STD handles), `">&=FH"`/`">&=N"` are fdopen-style — same fd or stream alias, no dup |
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
