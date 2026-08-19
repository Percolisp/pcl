# The Generated CL as an Intermediate Representation — Review & Roadmap

**Written:** 2026-07-06 (session 277), against the v2 default pipeline
(Parser2/ExprToCL2/VarAnnotator/CLForm, cache generation v2-7).
**Audience:** both people and AI assistants who need to *consume* PCL's
output — to debug it, to build tooling on it, or to translate it to another
target environment.
**Companion docs:** `CODEGEN_DESIGN.md` (early design — **stale**, still shows
the `pl-setf`-era naming; superseded by this doc's §2 until rewritten),
`docs/parser2-prototype.md` (architecture + history),
`docs/v2-completion-plan.md` §5.2 (seams and invariants).

The governing constraint, unchanged from CLAUDE.md §2: **when clarity and
speed conflict, speed wins.** Every recommendation below is emit-time or
macro-expansion-time only; none adds work at runtime. That is the standard a
proposal must meet to be listed here.

---

## 1. What the output is today

One `.lisp` file per Perl compilation unit, loadable by SBCL with
`cl/pcl-runtime.lisp` preloaded. It is *executable program text*, not a data
structure: the only contract a consumer gets is "the CL reader can read it".
Structurally, every file is:

```
preamble        (in-package :pcl), @INC setup, cache flags
sections        one per package segment, in source order:
                  package preamble   (p-defpackage / in-package / defclass plc-*)
                  declarations       (p-declare-sub, defvar boxes)
                  definitions        (p-sub …), BEGIN/END via eval-when
                  runtime            top-level statements, my-vars as nested lets
```

A `package` statement mid-file starts a new section; a *reopened* package
gets a short section (just `(in-package …)` + `p-set-current-package`).
This section model exists because CL's reader resolves symbols at read time
— a form mentioning `Foo::bar` can only be read once package `FOO` exists.

### 1.1 The three vocabularies

Symbol prefixes partition the output cleanly — this is one of the IR's real
strengths and must be preserved:

| shape | meaning | example |
|---|---|---|
| `p-*` | runtime primitive (op, builtin, macro) — defined in `cl/pcl-runtime.lisp`, exported from `:pcl` | `p-+`, `p-gethash`, `p-foreach` |
| `pl-*` | *user* sub, transpiled from Perl `sub foo` | `pl-fib`, `pl-new` |
| `plc-*` | CLOS proxy class backing a Perl package (method dispatch/C3) | `plc-animal` |
| `$x @x %x &x` | Perl variables, sigil kept in the CL symbol name | `$sum`, `@words`, `%count` |
| `+NAME+` | `use constant` | `+PI+` |
| `*name*` | runtime dynamic state | `*wantarray*`, `*pcl-current-package*` |

Operator naming rule: `p-` + the Perl operator *spelled literally*. So `p--`
is subtraction, `p-.` is string concat, `p-<=>` is numeric compare. To a
Lisp reader these are ordinary symbols; to a *regex-based* consumer they are
traps (`p--` contains the separator). Parse the output with a real
S-expression reader, never with line regexes.

### 1.2 The box model, visible in shapes

A Perl scalar is by default a `p-box` (mutable cell with cached numeric/string
views and a bless slot). The VarAnnotator proves some `my` scalars never need
the cell ("raw slots"). **The verdict is not written down anywhere in the
output — it is implicit in the emitted shape**, and a consumer must learn
this table to read the code:

| verdict | declaration | write | read |
|---|---|---|---|
| boxed | `(let (($x (make-p-box nil)))` … `(p-my-= $x V)` | `(p-my-= $x V)` | `$x` (ops unbox internally) |
| raw slot | `(let (($n 10))` | `(setf $n V)` | `$n` |
| package var | `(defvar $g (make-p-box nil))` | `(p-scalar-= $g V)` | `$g` |

The invariant behind it: **a raw slot must never receive a box.** Every op
that feeds a raw slot (`p-+`, `p-.`, …) returns a raw CL number or string.

### 1.3 The context protocol

Perl's scalar/list context becomes a dynamic variable: callers of
context-sensitive code bind `*wantarray*` (`t` = list, `nil` = scalar,
`:void`), callees read it (`p-wantarray`, and context-sensitive builtins
internally). Statement position emits no bind; `return` inherits.

### 1.4 The seams

Anything ExprToCL2 cannot lower natively goes through the embedded v1
generator and lands in the output as **raw text** spliced into the structured
forms. Raw seams are invisible in the final file (it is all text by then)
but they are why the output's *formatting* is inconsistent — see §3.1.

---

## 2. What already works well as an IR — keep these

1. **S-expressions.** The single best property. Any language with an
   S-expression reader (or 40 lines of parser) gets the full tree for free.
   No other IR change matters as much as not losing this.
2. **A closed op vocabulary with Perl semantics attached.** `(p-+ a b)`
   means *Perl* `+`: numify both sides, `"3x" + 1 == 4`. The semantic burden
   sits in ~one runtime function per op, exactly where a translator to
   another environment wants it: reimplement the `p-*` inventory, reuse the
   tree. The trees themselves are host-agnostic except for the constructor
   forms noted in §3.4.
3. **Scope is structural.** Every `my` is a `let` that nests the *rest of
   its block*; closing parens are scope ends. No symbol tables needed to
   read lifetimes. (Cost: deep indentation — cosmetic, worth it.)
4. **Context is explicit at every call site** (§1.3). Translators to
   environments without dynamic binding can thread it as a hidden argument
   precisely because every bind is visible in the tree.
5. **Source echoes.** `;; $a <=> $b` comments above lowered fragments, and
   Perl-shaped names throughout, keep the output navigable for a human with
   the Perl source in the other window.
6. **Speed transforms are visible, not magic.** A raw slot, a coalesced
   `(&optional ($x (p-undef)) …)` lambda list, a native `(p-gethash %h k)` —
   each optimization has a recognizable shape. Debugging performance means
   reading shapes, not profiling guesswork.

---

## 3. Friction points, each with a fix that costs no runtime

Ordered by how much they hurt a consumer, not by effort.

### 3.1 The IR is text with unstructured islands (the seams)

**Symptom:** inside one output file, native forms are printed by CLForm's
printer (2-space, depth-encoding indentation) while seam text carries v1's
own formatting — flush-left restarts mid-form, blank lines inside lambdas:

```lisp
(p-array-= @sorted (let ((*wantarray* t)) (p-sort (lambda ($a $b)
  (catch :p-return
    (block nil
(let ((*wantarray* nil))
  ;; $a <=> $b
  (p-<=> $a $b)
  )))) @nums)))
```

The tree is fine (the reader doesn't care), but: (a) "indentation encodes
depth" (CLAUDE.md §10) does not hold for generated output, so humans lose
their main visual check; (b) no tool can get the *structured* program out of
PCL — CLForm exists in-process only, and the printed file is the only
artifact; (c) any consumer wanting the tree must re-read the text.

**Fix (the single biggest win): make CLForm the total carrier, then make the
printed form canonical.**
- Keep shrinking the seams (this is already the v2 trajectory — every W-item
  moved constructs from raw to native). The end state: `raw()` leaves exist
  only for genuinely opaque payloads (inline CL, if ever), not for ordinary
  Perl constructs.
- Until then, cheap normalization: read the finished text once with the CL
  reader and pretty-print it back out (SBCL's `write` with `*print-pretty*`,
  or CLForm re-printing the re-read tree). One reader pass per output file
  at transpile time, zero runtime cost, and the emitted file becomes
  uniformly formatted regardless of which pipeline produced each fragment.
  Comments are the complication (the CL reader drops them) — either accept
  their loss in a `--canonical` mode, or keep them by making the CLForm
  printer the only emitter (the real fix).
- Once CLForm is total, `pl2cl --emit-sexp` (or JSON) that serializes the
  CLForm tree directly is a ~50-line addition and gives non-Lisp consumers
  the program as *data*. Do not build this before the seams are gone; a
  sexp dump with embedded text islands is worse than no dump.

### 3.2 Control characters are emitted raw inside string literals

**Symptom:** `print "fib: $n\n"` emits a string literal containing an
*actual* newline; `"a\tb"` contains an actual tab:

```lisp
(p-print (p-string-concat "a	b" "
"))
```

Standard CL string syntax has no `\n` escape, so this is the natural
emission — but it breaks every line-oriented tool: greps hit half a string,
diffs split literals across hunks, line counts stop matching form counts,
and AIs reading the file must constantly second-guess whether a line break
is syntax or data.

**Fix:** emit escaped ASCII and decode at macroexpansion time. A tiny reader
convention — `(p-str "fib: ~%")`? No: simplest is a macro
`(p-esc "a\tb")` whose *argument* uses `\n`/`\t`/`\xNN` escapes in an
ordinary CL string (doubling the backslash so the CL reader passes it
through) and which expands to the decoded literal constant:

```lisp
(p-print (p-string-concat (p-esc "a\\tb") (p-esc "\\n")))
```

`p-esc` is a macro → the decode happens once at compile time, the compiled
code holds the same constant string as today. Emit `p-esc` *only when the
literal contains control characters* so the common case stays a plain
string. One-line printer change + a ~15-line macro. (A translator to
another environment implements `p-esc` as its own string-unescape — easier,
not harder, than handling raw control bytes.)

### 3.3 Regex/subst/tr literals are un-parsed Perl source

**Symptom:** `(p-regex "/(\\w+)\\s+(\\w+)/")`, `(p-subst "world" "perl")` —
the regex still carries its Perl delimiters and (when present) trailing
flags inside one string. Every consumer must re-implement Perl's
delimiter/flag scan; the runtime re-parses it too.

**Fix:** structured literals:

```lisp
(p-regex :pat "(\\w+)\\s+(\\w+)" :flags "")        ; m//
(p-subst :pat "world" :rep "perl" :flags "")       ; s///
(p-tr   :from "a-z" :to "A-Z" :flags "")           ; tr///
```

The transpiler already knows the parts (PPI hands them over separately);
joining them back into `/…/` throws structure away. Runtime cost: *negative*
— `p-regex` stops scanning for delimiters at compile/first-use time. This
is an emission-changing commit (bump `*pcl-cache-generation*`) and the
`p-regex`/`p-subst`/`p-tr` entry points need a compatibility arm while both
shapes exist (or a one-shot flag-day with the generation bump — cleaner).

### 3.4 Two abstraction levels for construction

**Symptom:** Perl-level ops sit beside raw host-level constructors in the
same forms:

```lisp
(let ((@words (make-array 0 :adjustable t :fill-pointer 0)))   ; host idiom
  (p-array-= @words (vector "alpha" "beta"))                    ; host idiom inside p- op
(let ((%count (make-hash-table :test 'equal)))                  ; host idiom
(let (($x (make-p-box nil)))                                    ; struct constructor
```

`make-array 0 :adjustable t :fill-pointer 0` is SBCL-speak for "new Perl
array". A translator must special-case each host idiom; an AI must know CL
to recognize them.

**Fix:** close the vocabulary with three inlined constructors and use them
everywhere the emitter currently prints host forms:

```lisp
(p-new-av)          ; ≡ (make-array 0 :adjustable t :fill-pointer 0)
(p-new-hv)          ; ≡ (make-hash-table :test 'equal)
(p-new-sv)          ; ≡ (make-p-box nil)   — and (p-new-sv V) for init
```

Declared `inline` (or defined as macros), SBCL compiles them to *identical*
code — zero cost, measured trivially. Also fold `(vector …)` used as a Perl
list into `(p-vlist …)` (a macro over `vector`) so "list of values crossing
a p- op boundary" is one recognizable head instead of a host idiom.
After this, the ONLY non-`p-` heads left in generated bodies are CL
specials with universal analogues: `let`, `setf`, `progn`, `block`,
`lambda`, `defvar`, `eval-when`, `declare`, `in-package` — a closed set a
translator maps once. That set should be *listed normatively* in the spec
(§4) and the emitter kept to it.

### 3.5 Context binds are correct but deafening

**Symptom:** `(let ((*wantarray* t)) …)` / `(let ((*wantarray* nil)) …)` /
`(let ((*wantarray* *pcl-caller-wantarray*)) …)` wrap nearly every call.
Semantically ideal (§2.4); textually it is the single largest source of
visual noise, and it exposes a host mechanism (dynamic `let`) where a
concept ("evaluate in scalar context") is meant.

**Fix:** three macros, purely cosmetic, same expansion:

```lisp
(p-list-ctx (pl-fib 12))      ; ≡ (let ((*wantarray* t)) (pl-fib 12))
(p-scalar-ctx (p-=~ $s RX))   ; ≡ (let ((*wantarray* nil)) …)
(p-caller-ctx (p-my-= …))     ; ≡ (let ((*wantarray* *pcl-caller-wantarray*)) …)
```

Each line shortens by ~14 characters, the *concept* gets a searchable name,
and a translator keys on three heads instead of pattern-matching a `let` of
a special variable. Macroexpansion-time only.

### 3.6 Redundant and asymmetric emissions

Small individually; together they teach a reader that the output cannot be
trusted to mean what it says:

- **Duplicate defvars** — `our $global` inside the runtime emitted
  `(defvar $global (make-p-box nil))` twice in one section (forward-decl
  pass + our-decl pass), and `p-defpackage :Animal` appears both in the
  file-top pre-declaration block and the section preamble. Harmless to CL
  (defvar/defpackage are idempotent), misleading to everyone else. Dedupe
  at assembly time (a `%seen` per section).
- **The `$a`/`$b` defvar pair** is emitted per section even when no sort
  block exists in the file. Emit on demand (the emitter knows whether any
  sort/block-form construct occurred) or accept and document.
- **Lambda shape asymmetry** — a sort comparator lowers as
  `(lambda ($a $b) (catch :p-return (block nil …)))` while map/grep bodies
  are bare `(lambda ($_) …)`. This one is *semantically motivated*, not a
  bug: `perldoc -f return` says `return` exits a **sort block** (so the
  comparator needs its own catch frame) but inside a **map/grep block** it
  returns from the *enclosing sub* (verified against perl 5.40) — so the
  bare lambda must let `:p-return` fly through. Keep the shapes; write the
  rule into the spec so the asymmetry stops looking like an accident.
- **Dead-weight declarations** — `(declare (ignore %_args)
  (dynamic-extent %_args))` on every no-extra-args sub. Correct and useful
  to SBCL; just document that `declare` forms are host advice a translator
  may drop wholesale.
- **Trailing whitespace / blank-line runs** in section assembly. Cosmetic;
  fold into the §3.1 canonical printer.

### 3.7 The environment is baked into every output file

**Symptom:** the preamble hardcodes the build machine's perlbrew paths into
`@INC` and sets `*pcl-pl2cl-path*` to an absolute path. Every output file
is machine-specific before the first line of user code.

**Fix:** one form, `(p-init-env :inc '("." "lib" …) :pl2cl "…")`, emitted
for *scripts*; module transpiles (cache entries) should not carry it at
all. For translation targets the whole preamble is then one recognizable,
skippable head instead of a dozen raw `vector-push-extend`s. (Runtime cost:
nil — same work, one function call at startup.)

### 3.8 The implicit contracts have no normative home

The box/raw shape table (§1.2), the context protocol (§1.3), the magic
globals (`$1`/`$2`/`$_`/`$@` as dynamic variables set by the match/eval
machinery), `p-return` via catch-tag, the section model, the closed
CL-special set (§3.4) — all of it is real, stable, and **documented only in
scattered design docs and the runtime source**. `CODEGEN_DESIGN.md` — the
file a newcomer or AI will find first — still shows `pl-setf`-era output
that no longer exists.

**Fix:** a normative `docs/ir-spec.md`: the grammar (what heads may appear
where), the op inventory (generated from the `:pcl` export list + one-line
semantics each), the shape tables, the context protocol, the consumer
rules ("parse with an S-expr reader", "declares are droppable advice",
"comments are non-semantic"). Rewrite or delete `CODEGEN_DESIGN.md` so
there is exactly one place the IR is defined. Cheap, high leverage — for
AI consumers it is the difference between inferring the rules from samples
and being told.

> **DONE (s277):** `docs/ir-spec.md` exists — the translator's manual
> covering the data model, coercion, context, calling convention, control
> flow, OO, magic globals, load model, and op family rules, each claim
> verified against the runtime. Read it first; this review remains the
> roadmap for *improving* the output.

---

## 4. Recommended order of work

| # | item | §| effort | payoff |
|---|---|---|---|---|
| 1 | `docs/ir-spec.md` + retire stale CODEGEN_DESIGN.md | 3.8 | docs only | every consumer, immediately |
| 2 | constructor + context macros (`p-new-av/hv/sv`, `p-vlist`, `p-*-ctx`) | 3.4, 3.5 | small; emitter + runtime macros; cache bump | closed vocabulary, −30% visual noise |
| 3 | structured regex literals | 3.3 | small-medium; flag-day + cache bump | removes the last re-parse a consumer must do |
| 4 | `p-esc` for control characters | 3.2 | small; printer + macro; cache bump | line-oriented tools and diffs work |
| 5 | dedupe/asymmetry cleanups | 3.6 | small, several commits | trust in the output |
| 6 | canonical re-print pass (`--canonical`) | 3.1 | small, interim | uniform formatting today |
| 7 | seam retirement to CLForm-total, then `--emit-sexp` | 3.1 | the long road (v2 W-items already walk it) | the IR becomes *data* |

Items 2–5 are emission-changing: each needs the full parity discipline
(`docs/v2-completion-plan.md` §2) and a `*pcl-cache-generation*` bump.
None of them changes what the compiled code *does* — that is the test each
must pass before landing.

## 4b. Addendum (s277b) — findings from the declarations/renaming deep-dive

Writing `ir-spec.md` §2b (declarations and rename families) surfaced three
clarity items the original review missed, plus a reframing of priorities.
All are emit-time only; none costs runtime speed.

### 4b.1 The two-dialect problem — now the #1 clarity cost

The review treated raw seams as the structural problem. The deep-dive
showed it is bigger: **the corpus speaks two dialects.** A file that hits
any whole-file gate is emitted entirely in the v1 dialect —
`defvar`-based `my`, `(p-eval-always …)` wrappers, `(let ((*wantarray*
:void)) …)` statement wraps — while v2-native files use let-based
lexicals and no void wraps. A consumer must implement *both models* until
fallbacks are rare. So the clarity ranking for future work is: (1) retire
the *whole-file* gates, (2) retire the seams. Every gate removed shrinks
the v1-dialect surface a consumer must know.

> **Update (s277c):** `state` in named subs — previously the largest named
> gate — is now v2-native (ir-spec §2b, the `$x__state__N` family). The
> full gate-retirement roadmap now lives in `docs/v2-transfer-plan.md`.

### 4b.2 No pipeline marker in the output

Verified: the v2-native and v1-fallback headers are byte-identical — a
consumer *cannot tell which dialect they are reading* without recognizing
shapes. Fix: one header comment line, e.g.
`;;; pcl: pipeline=v2 cache-gen=v2-7 src=<file>`. Trivial, zero cost,
and tooling can dispatch on it immediately. Do this before anything else
in this addendum.

### 4b.3 Rename manifest

The rename surgery (ir-spec §2b.3) is invisible in the output — a reader
meets `$tick__file__0` and must infer the provenance from the suffix
convention. Emit a manifest as header comments in files where renames
fired: `;;; rename: $tick -> $tick__file__0 (captured by named sub)`.
Makes source-mapping mechanical, documents the surgery in-band,
transpile-time only.

### 4b.4 Seam-emitted declarations execute per call

Observed shape: a renamed shadow cell inside a `map` lambda is emitted by
the seam as `(p-eval-always (defvar $x__shadow__0 …))` *inside the lambda
body* — a boundp check on every call, and a declaration buried
mid-expression. Hoisting it to the declarations bucket is clearer **and**
removes per-call work (speed-positive). Disappears with seam retirement;
cheap to fix earlier if a probe shows it on hot paths.

### 4b.5 Minor: fresh-name collision guard

`$x__file__N` etc. are chosen without checking that user code doesn't
already contain that exact token (legal Perl). Theoretical today; a
one-line scan of the segment text before picking `N` closes it.

## 5. What a translator to another environment must implement

The minimal consumer contract — the checklist below; the full semantics of
each item are specified in **`docs/ir-spec.md`** (the translator's manual):

1. An S-expression reader (symbols may contain `$ @ % + - . < > = ~ ! : #`).
2. The `p-box` cell type and the §1.2 shape table (boxed/raw/package).
3. The `p-*` op inventory with Perl coercion semantics — the one large
   item; the semantics live today in `cl/pcl-runtime.lisp`, one function
   per op, which *is* the reference implementation to port.
4. The context protocol: a hidden scalar/list/void flag threaded through
   calls where the tree shows `*wantarray*` binds (or the §3.5 macros).
5. Dynamic (stack-restored) bindings for the magic globals `$_ $1.. $@ $0`
   and `local`.
6. `p-return` = non-local exit to the nearest enclosing sub frame;
   loop control (`p-last`/`p-next`) = exits to the loop's block/tag.
7. May ignore: comments, `declare` forms, `eval-when` wrappers (if the
   target has no compile-time/load-time split, run them in order),
   `dynamic-extent`, and the whole preamble behind `p-init-env`.

Everything not on that list should be — and after items 2–4 above, would
be — expressible in the `p-*` vocabulary alone.

## 4c. Addendum (s335) — findings from transpiling idiomatic CPAN code

pack-impl.pl is near-C Perl; the s335 review (USER ask) transpiled
Try::Tiny, Data::Dump and File::Which to see what *idiomatic* CPAN Perl
does to the output.  Headline: statement-level bodies read fine (File::Which
is genuinely presentable end to end); the damage clusters in five shapes.
Each is tagged with its owner.

### 4c.1 Expression-position anon subs print as one-liners (owner: #78 tail — VERIFY on completion)

`sub { $_[1] }` inside a ternary/glob-assign emits as a single 326-column
line: the whole `(lambda (&rest %_args) (let ((@_ (p-flatten-args …))
(*pcl-caller-wantarray* …)) (catch :p-return (block nil …))))` chain flat.
Cause: expression-position lambdas still cross the E2 `to_flat` boundary
(exact-flat is the parity contract there).  #78's inline_lambda re-host
should make them structured and therefore pretty-printed — when #78
closes, re-transpile Try::Tiny and CHECK this actually became multi-line.

### 4c.2 The anon-sub calling-convention boilerplate (owner: #75 — add `p-lambda`)

Every anon sub — including a `sort { lc($a) cmp lc($b) }` comparator —
carries `%_args`/`p-flatten-args`/`*pcl-caller-wantarray*`/`catch
:p-return`/`block nil` in full.  ~110 chars of wrapper around a body that
is often one form.  This is the single biggest vocabulary win available:
a `p-lambda (params) body` macro hides the whole convention.  Same for the
sub prologue: `(p-args-body (block nil (let ((*wantarray* :void)) …` plus
`(let ((*wantarray* nil)) (p-list-= (vector $x @rest) @_))` for
`my ($x, @rest) = @_;` → one `p-my-params` form.  (§3.5's context macros
are the same family; this names the two highest-frequency instances.)

### 4c.3 Statement-position comma expressions emit BOTH context versions (NEW — task #219)

`$n++, last if $cond;` lowers to `(if *wantarray* (vector …both ops…)
(progn …both ops…))` — the same two side-effects duplicated under a
RUNTIME context test, in a position whose context is statically void.
Reader cost (every such statement doubles) and a pointless runtime branch.
Fix in the v2 lowering: statement position is void, emit the progn only.

### 4c.4 Empty `my` lists emit a no-op assignment (NEW — task #219)

`my ($catch, @finally);` → the let-binding (correct) PLUS
`(let ((*wantarray* nil)) (p-list-= (vector $catch @finally) (vector)))` —
an assignment of nothing to fresh boxes.  Drop the init when the RHS list
is empty.

### 4c.5 v1-raw regions are double-spaced and double-echoed (owner: E5.2/#78 deletion; do not spot-fix)

BEGIN bodies (still v1-shaped) print one blank line between every line,
echo the same source twice (`;; unless (…) { … }` then `;; unless (…)`),
and leave `nil` else-arms and bare `)` on their own lines.  Embedded raws
also reset indentation to column 0 mid-tree (the pcl-pack.lisp `;; return`
finding, same family).  All of it dies with the raw path — record, don't
polish v1.

### Measured (s335, pl2cl at gen v2-100)

| file | src lines | out lines | indent>40 cols | leading-ws bytes |
|---|---|---|---|---|
| File::Which | 398 | 218 | 0% | 21% |
| Try::Tiny | 820 | 321 | 0% | 19% |
| Data::Dump | 730 | 1132 | 39% | 57% |

Data::Dump's 39%/57% is the #213/#218 nesting pair (long subs, many `my`s,
deep elsif chains); the other two show the baseline is already close once
nesting is fixed.  Also visible everywhere, already catalogued: §3.5
context binds, §3.4/§4b.1 mixed assignment spellings (`p-my-=` /
`p-scalar-=` / `p-list-=` / `setf` / `box-set` on one screen), duplicate
`defvar $a/$b` + qualified/unqualified `$VERSION` pairs (#66 family).

### Measured again (s407, pl2cl at gen v2-151 — #281 step 1, Fable)

Same three-file shape plus corpus/lib samples; counters per emitted file
(`ind>40` = lines indented past column 40, `lead-ws` = leading-whitespace bytes,
`wa/100L` = `(let ((*wantarray* …))` binds per 100 output lines, `echo;;` =
`;; <perl source>` echo comments, `dupdv` = duplicate `(defvar X` lines,
`%_args` = `(declare (ignore %_args) …)` lines, `maxdep` = deepest paren depth):

| file | out lines | ind>40 | lead-ws | wa/100L | echo;; | dupdv | %_args | maxdep |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| Try::Tiny | 341 | 0% | 18% | 8.2 | 24 | 4 | 0 | 21 |
| Data::Dump | 1206 | 35% | 54% | 7.0 | 19 | 2 | 0 | 43 |
| perl-tests/closure.t | 1495 | 9% | 37% | 16.1 | 21 | 2 | 0 | 30 |
| perl-tests/sort.t | 3900 | 9% | 35% | 17.4 | 41 | 19 | 0 | 28 |
| perl-tests/hash.t | 633 | 2% | 24% | 10.9 | 26 | 6 | 0 | 22 |
| lib/List/Util.pm | 702 | 0% | 21% | 10.7 | 7 | 2 | 0 | 13 |

What changed since s335 and what did not — the #281 worklist, ranked by
measured frequency:

1. **§3.5 context binds are still the loudest thing in the file**: 7–17 per
   100 lines, i.e. every 6th–14th line.  Three macros (`p-list-ctx`,
   `p-scalar-ctx`, `p-caller-ctx`), expansion identical → free.  FIRST.
2. **§3.6 duplicate defvars are real and per-section**: sort.t emits
   `(defvar $a …)`/`(defvar $b …)` TEN times each (one pair per section) plus
   `Bar::$a` twice; every file carries 2–19.  Assembly-time `%seen` per
   file.  Free.
3. **§3.1's printing symptom is GONE** — no flush-left restarts mid-form in
   any sample (the E2 layout work + the fold); the remaining `raw` regions
   print through CLForm.  What is left of §3.1 is the *structural* half
   (CLForm not yet the total carrier — the s386 88% seam rate), which is
   #153's, not #281's.
4. **§3.2 control characters inside string literals — unchanged**: every
   `"…\n"` prints an actual newline (Data::Dump has 37 such continuation
   lines).  A printer-side escape needs a reader-side counterpart (CL has no
   `\n`); candidates: emit `(p-str "a" #\Newline "b")`-style concatenation for
   literals with control characters, or a `#.`-free reader macro.  Design
   item, not free — measure the load-time cost before choosing.
5. **#218 nesting is Data::Dump's whole 35% / depth 43** (elsif chains and
   sequential `my` lets); Try::Tiny and List::Util are flat.  `p-cond` for
   elsif chains is a macro that changes shape → bench; `let*` runs (#213 b)
   at E5.
6. **The sort comparator boilerplate** `(lambda ($a $b) (catch :p-return
   (block nil …)))` appears once per `sort BLOCK`; a `p-sort-cmp` macro with
   the same expansion names the semantic rule (§3.6: `return` exits a sort
   block) — free.
7. `%_args` declares are gone (0 everywhere) — the s335 item is closed.
8. The `;; <perl source>` echo comments (7–41 per file) are the seam's
   traceability line; keep, but the spec should say they are comments a
   consumer may drop (§4b.1's two-dialect note stands until #153's FOLD).

Everything under 1, 2 and 6 is emission-changing at zero runtime cost and
can go in ONE Opus commit set: corpus-diff explained per file (every diff
is one of the three shapes), gate, sweep TOTAL/LOST, gen bump, the three
artifacts regenerated, and `docs/ir-spec.md` updated normatively (§6.x for
the context macros, the sort-comparator rule).  Items 4 and 5 are the
design half of #281 and wait for the bench.

**Items 1, 2 and 6 are DONE — shipped s414 (`s414f`), verified and merged
s415.**  `p-list-ctx` / `p-scalar-ctx` / `p-void-ctx` / `p-caller-ctx`
(four, not three: `:void` is 246 of sort.t's 665 binds), `p-sort-cmp`, and
a `_forward_global_decls` dedupe keyed on **(package, name)** — item 2's
premise above is corrected in place: sort.t's ten `(defvar $a …)` lines are
ten different symbols, one per `in-package` section, so the `dupdv` counter
above measured TEXT and a text-level fix would have been a silent wrong;
the real repeats are 16 in sort.t, 4 in hash.t, 2 in closure.t.
`docs/ir-spec.md` §4, §5.4, §10 and §12 carry the normative text.  What is
left of this list for #281: item 4 (control characters in string literals)
and item 5 (`p-cond`), both design + bench, both Fable's.
