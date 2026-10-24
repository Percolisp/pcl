# The code generation, reviewed against perl's op tree and the IR spec (s526c, Fable fork, 2026-10-24)

The USER's third review (2026-10-24, 13:5x): "When the updates to the parser and
the runtime are done, create a subtask to review the code generation."  Same
method as `docs/parser-review-s525.md` (the parser against perl's front end) and
`docs/runtime-review-s525.md` (the runtime against perl's implementation):
measure first, perl 5.40.3 as the oracle (`perl` beside `./runpcl`, its `op.c`
and `B::Concise` as the reference shape), inventories with line refs, options
sized with the number that proves each.  Measured on main `a8f0166d` (code =
MAIN-READY-45 `761d92f4`; the compiler files `Pl/*.pm` last changed at
MAIN-READY-43 apart from s525p's two inert modules).  No code was changed; one
task filed (#3880, a runtime overload finding the probes turned up).

Words a reader needs.  The **IR** is the Common Lisp text PCL emits: every
construct is a prefix form whose head is a runtime macro or function named
`p-…` (`docs/ir-spec.md` is its normative manual).  A **box** (`p-box`) is
PCL's Perl scalar, a mutable cell with cached numeric and string views; a **raw
slot** is a `my` scalar the compiler has proven only ever holds a plain number
or string and emits as a bare host variable.  A **Kind-A emission** is a fast
IR shape the emitter may use only when an analysis FACT licenses it, switchable
by name through `PCL_OPT` (`Pl/Passes.pm`; `PCL_OPT=none` is the general-form
compiler).  **CLForm** (`Pl/CLForm.pm`) is the tree the emitter builds and the
ONE printer that turns it into text.  The **seam** is the escape hatch in that
tree: a `Pl::CLForm::raw` node holding text produced by the old statement
compiler (`Pl/Parser.pm`, "v1") instead of a tree.  The **generation stamp**
(`*pcl-cache-generation*`, today `v2-8284`) is the string every
emission-changing commit bumps so cached module translations are never reused.
**Shape macros** are the twelve runtime macros (`p-setf` first) that look at
the CODE SHAPE of their place argument and pick a store form at
macro-expansion time -- ir-spec §10d calls them the IR's assembler.

## 0. The answer in ten lines

1. **The code generation is sane as an artifact and, like the runtime, mostly
   sane as a structure** -- the opposite of the parser's verdict.  ONE
   expression compiler (`Pl/ExprToCL.pm`, 95 distinct IR heads, since s411),
   ONE printer whose parentheses balance by construction, ONE registry of 24
   named optimizations plus one post-lowering pass, one normative spec the
   output matches where checked, duplication 3.2 % in ExprToCL and eight
   five-to-eleven-line pairs in the statement lowering, 6.0 % of the transpile
   time.  Eight op probes and nine representation probes are identical to perl
   except the two ruled or filed faces.
2. **The compiler already NAMES most stores.**  Over 120 files (100,728 emitted
   lines) 92 % of the ~6,200 store forms are specific -- `p-my-=` 2,498,
   `p-scalar-=` 1,390, `p-array-=` 1,038, `p-list-=` 461, `p-hash-=` 154, raw
   `setf` 93, element `setf` 88; the generic `p-setf` carries 483 (7.8 %) and
   reaches 16 of the runtime's 24 arms.  The runtime review's "second lowering
   layer" is the TAIL -- element, deref, slice and lvalue-builtin stores, plus
   `.=`/`++` on element places (about one in ten) -- not the rule.
3. **The structural debt is in three places, all in Parser2's STATEMENT
   lowering, none in ExprToCL:** (a) there is no statement classifier --
   `_lower_block_1` (1,188 lines: 25 top-level `if`s, 37 `isa` tests, 15
   `content` tests), `_lower_stmt` (303) and `_lower_compound` (630) recognise
   statement shapes by matching raw token arrays at the point of use, the
   exact defect `docs/v2-target-architecture.md` §2.2 named in July; (b) the
   v1 TEXT seam is the DEFAULT of statement lowering -- 1,058 of the corpus's
   statements (5.1 %) still become opaque text (`local` 337, `use` 244,
   `BEGIN` 147, `no` 133, signatured `sub` 89, `return` under a loop modifier
   37, `require` 34, …), 33 v1 statement processors stay live for them, and
   the seam costs 160,000 `is_raw` predicate calls per module; (c) the mutable
   scope accumulator `_let_bound_vars` (51 uses, 12 save/restore sites) that
   the target shape said to replace with a fact.
4. **The representation verdicts are sound where they fire and conservative
   where they do not.**  Of 1,878 scalar lexicals in the corpus 90 % stay
   boxed (perl-tests), Calc.pm 81 %, so the raw slot is the exception, not the
   rule.  A raw slot drops exactly ONE semantic bit perl keeps -- "this scalar
   has been used as a number" (#3780, widened at the s525u merge) -- and
   nothing else probed: `pos`, aliasing and `tie` through `@_`, a `foreach`
   alias, a dualvar, a string `eval`, `local`, an overloaded object arriving in
   a numeric slot all match perl.
5. **Context is dynamic where perl's is static.**  perl marks every op's
   context at compile time (`OPf_WANT`); PCL binds `*wantarray*` at run time:
   12,720 binds in 95,914 lines of perl-tests (13.3 per 100), 8.3 in Calc.pm,
   4.5 in the everyday programs.  `p-void-ctx` is the single most frequent
   head of the whole IR (8,636), and 90 % of its per-statement uses wrap a
   user-sub call -- the one family #2775's list-context work did not reach,
   and one a known-callee fact already covers (`insensitive-call`).
6. **The optimization registry IS the target's contract, and it is honest:**
   24 Kind-A names, one Kind-B pass (`classic-sort`), `PCL_OPT=none` identical
   on three probes, and NO hidden switch -- the 17 other `PCL_*` variables the
   compiler reads are all debug or census instruments.  What the registry does
   not yet give the target is facts keyed on BINDINGS: VarAnnotator and the
   accumulator key on token text, which is why every rename must precede them
   (the parser program's 1c).
7. **IR hygiene is measured and mostly clean, with one unfinished item.**  761
   exported names in 56 families (19 unclassified); 31 host leaks over 111
   files, of which 11 are the `use integer` arithmetic lowered to host `+ - *
   truncate rem logand logior …` on `p-int` operands, 3 are odd ops and 17 are
   quoted data symbols (labels, operator names); five drops in 120 files.  The
   s277 constructor macros (`p-new-sv/av/hv`, `p-vlist`; #75) were never
   built: `make-p-box` 6,079, `make-array 0 :adjustable …` 1,175,
   `make-hash-table` 302 and `vector` 1,304 host idioms remain, 93 duplicate
   `p-defcell`s, and the preamble bakes seven machine paths into every file.
8. **Compile time is not the code generation's problem.**  Of a 23.1 s
   profiled Calc.pm transpile: PPI 60.5 %, PExpr 11.6 %, Parser2's
   resolve/repair half 9.1 %, Moo and friends 8.6 %; the whole code generation
   (CLForm 2.1 %, VarAnnotator 1.1 %, ExprToCL 1.1 %, Parser2 lowering 0.7 %,
   OpcodeTree 0.4 %) is 6.0 %.  The optimizations cost nothing measurable
   (3.65-3.72 s default vs 3.66-4.20 s `PCL_OPT=none`).
9. **Growth is churn and facts, not accretion.**  ExprToCL +4,965 / -4,289
   lines in 162 commits over 90 days is net +676 (August's -4,006 is Phase A
   deleting the second compiler); VarAnnotator +1,804 / -540 is the fact
   families; Parser2's lowering half received 2,557 of that file's 12,012
   added lines (21 %) -- the rest is the resolver and repair side the parser
   review covered.  A 40-commit sample: 17 semantic fixes, 8 new constructs,
   7 facts or optimizations, 5 structural moves, 3 docs and instruments.
10. **Recommendation: no separate code-generation program.**  The three debts
    of point 3 are the lowering HALF of the parser program: name them as its
    step **1d** (`#3694`) -- a statement classifier (identical emission, 2-3
    sessions), then the seam closed one statement kind per session (biggest
    first: `local`, then `use`/`no`/`require`, `BEGIN`, signatured subs, loop
    modifiers; each an emission change with the sweep), with facts moving onto
    binding ids as the 1c ports land.  The runtime's shape macros shrink as a
    consequence (R4's code half).  Two small items stand alone as fillers: the
    void-position bind as a Kind-A gate (measure first) and #75's first slice,
    the constructor macros, bundled with the next generation bump.  One slot
    per round stays the parser's; the code generation's items ride in it.

## 1. What the code generation is today (measured)

### 1a. Size and growth

| file | lines | subs | > 80 | > 200 | largest | 90-day churn |
|---|---|---|---|---|---|---|
| `Pl/Parser2.pm`, lowering half (the 22 `_lower_*` / `*_facts` subs) | ~3,330 of 15,728 | 22 | 9 | 3 | `_lower_block_1` 1,188, `_lower_compound` 630, `_lower_stmt` 303 | +2,557 of the file's +12,012 |
| `Pl/ExprToCL.pm` (the expression compiler) | 6,704 | 176 (43 `gen_*`) | 15 | 2 | `gen_funcall_form` 853, `gen_binary_op_form` 365 | +4,965 / -4,289 in 162 commits |
| `Pl/VarAnnotator.pm` (the representation verdicts) | 2,537 | 55 | 7 | 2 | `_tw_walk` 367, `_analyze_tree` 290 | +1,804 / -540 in 42 |
| `Pl/Environment.pm` (compile-time facts: constants, prototypes, pragmas) | 1,694 | 59 | 2 | 1 | | |
| `Pl/InterpScan.pm` (the one interpolation scanner) | 794 | | | | | |
| `Pl/CLForm.pm` (the tree + the printer) | 505 | 23 | 1 | 0 | `to_string` 105 | +359 / -40 in 16 |
| `Pl/Passes.pm` (the registry) + `Pl/ClassicSort.pm` (the one Kind-B pass) | 261 + 222 | 13 | 0 | 0 | | +280 / -19 in 22 |
| `Pl/Parser.pm` (v1, reached through the seam) | 13,562 | 265 (33 `_process_*`) | 37 | 8 | | +7,367 / -2,106 |

By month, the four emission files together: Jul (from the 26th) +370 / -102,
Aug +3,068 / -4,006, Sep +3,121 / -556, Oct (to the 24th) +849 / -224.

### 1b. The pipeline, as it runs

```
 PPI statement
   │  Parser2::_lower_block_1  (1,188 lines)  — statement KIND by PPI class + token shape;
   │                                              7 shapes go to _fallback_stmt = v1 TEXT
   ├─ _lower_stmt  (303)   — simple statements: root write / compound / incdec / bare call …
   ├─ _lower_compound (630) — if/unless, while/until, for(;;), foreach (4 lowerings by facts), try
   ├─ _lower_sub_inner (121) — p-sub + the argument-body regime (args-copy, sig-classic facts)
   ▼
 expression tokens → PExpr::parse_expr_to_tree → OpcodeTree
   │  ExprToCL::gen_node_form → gen_*  (95 distinct IR heads; the op tables at :240-:4600)
   ▼
 CLForm tree (atoms + [head, @args] + raw text islands)
   │  Passes::run — Kind-B passes (classic-sort), the --manifest observer
   ▼
 CLForm::to_string — THE printer (2-space depth; parens balance by construction)
```

`VarAnnotator::analyze` runs once per sub body before lowering and returns the
per-name verdicts (`unboxable`, the B-regime `numeric`/`string` freeze,
`strbuf`, `numonly`, `foreach_ro`, the array facts) that `_lower_stmt` and
ExprToCL read -- 28 direct `$vi->{…}` reads in Parser2, 2 in ExprToCL, every
fast shape behind a `Pl::Passes::enabled` / `fact` gate (Parser2 16 + 11,
ExprToCL 5 + 8, VarAnnotator 9).

### 1c. Where the compile time goes (`Devel::NYTProf` 6.15 on `./pl2cl < lib/Math/BigInt/Calc.pm`, 2,566 lines, 3.7 s real, 23.1 s profiled)

| where | exclusive | share |
|---|---|---|
| `PPI::*` | 13.97 s | 60.5 % |
| `Pl::PExpr*` (the expression parser) | 2.68 s | 11.6 % |
| `Pl::Parser2`, parse / resolve / repair half | 2.09 s | 9.1 % |
| Moo, `Scalar::Util`, `UNIVERSAL`, `main` | 1.97 s | 8.6 % |
| **`Pl::CLForm`** (`_flat` 136,514 calls 0.33 s; `is_raw` 85,328 + `is_raw_wrap` 75,430 calls) | 0.48 s | 2.1 % |
| `Pl::Parser` (the shared pre-pass and facts layer) | 0.29 s | 1.3 % |
| **`Pl::VarAnnotator`** | 0.26 s | 1.1 % |
| **`Pl::ExprToCL`** | 0.26 s | 1.1 % |
| **`Pl::Parser2`, lowering half** | 0.17 s | 0.7 % |
| `Pl::Environment`, `Pl::OpcodeTree` | 0.19 s | 0.8 % |
| **code generation in all** | **1.39 s** | **6.0 %** |

The parser review's finding stands and is sharpened: the transpile's cost is
the tree being asked scope questions, and the emission half is a rounding
error in it.  One number inside the 6 % is the seam's: 160,000 `is_raw` /
`is_raw_wrap` tests per module exist only because the tree may contain text.

## 2. Is there ONE lowering per construct?

### 2a. The per-op table (six probe programs, `perl -MO=Concise,-exec` beside `./pl2cl`; every program's run output identical to perl's)

| perl op (Concise) | PCL IR form(s) | where the shape is decided | decided twice? |
|---|---|---|---|
| `padsv_store` / `sassign` on a `my` (statement root) | `(p-my-= $x V)` boxed; `(setf $x V)` raw; `(setf $n (%pcl-to-number-strict V "$n"))` freeze-licensed | Parser2 `_lower_stmt` :12422 (+ `_lower_block_1` :11268, :11349) | no |
| `sassign` elsewhere (chained, in a condition, in arguments) | `(p-setf PLACE V)` | ExprToCL `gen_binary_op_form` :1929; `%OP_EXCEPTIONS` `'=' => 'p-setf'` :253 | **yes** -- the runtime macro's 24 arms (ir-spec §10d) |
| `sassign` to a package variable | `(p-scalar-= $g V)` | ExprToCL :1969 / Parser2 :11251 | no |
| `aassign` | `(p-list-= (vector L…) (vector R…))` -- 461 of 461 LHS are `(vector …)` | ExprToCL :1932, Parser2 :10323 (params) | **yes** -- `p-list-=` dispatches on the LHS list (≤ 8 scalars, skip placeholders, greedy aggregate, slices, refaliasing) |
| `aassign` to one aggregate | `(p-array-= @a V)` / `(p-hash-= %h V)` | ExprToCL :1965-:1967 | no |
| `helem`/`aelem` store | `(setf (p-gethash %h K) V)` under the `elem-setf` fact; else `(p-setf (p-gethash …) V)` | ExprToCL `=` + `Pl::Passes::enabled('elem-setf')` | yes for the unlicensed case |
| `multideref` rvalue, autovivification | `(p-gethash (p-viv-container (p-gethash-deref $hr "z")) "w")` -- the vivifying levels are chosen at compile time (ir-spec §3.2c) | ExprToCL `gen_hash_access_form` :4808, `gen_array_access_form` :4226, `_gen_scalar_deref_base_form` | no |
| `add` `multiply` … | `(p-+ a b)` …; under `use integer` host `(+ (p-int a) (p-int b))`, `truncate`, `rem`, `logand`/`logior` + `p-to-s64` | ExprToCL `gen_binary_op_form` :1976-:1988 | no (but see §6: host arithmetic in the IR) |
| `concat` / `multiconcat` | `(p-string-concat …)` for an interpolated string; `(p-. a b)` for `.`; `(p-.= $s V)` for `.=` (316 of 334 on a plain scalar) | ExprToCL `gen_string_concat` :2040 | `.=` on an element place: yes (18 of 334) |
| `preinc` / `postinc` / `predec` / `postdec` | statement root: `(p-incf-raw $x)` / `(p-incf $x)` (Parser2 :12478); expression: `(p-post++ PLACE)` … (ExprToCL `gen_postfix_op_form` :4176) | | expression form on an element / box / cast place: yes (~50 of 560) |
| `entersub` | `(pl-f ARGS)` under a context bind; bare under the `insensitive-call` fact (`(pl-g 1 2)` in probe o3) | ExprToCL `gen_funcall_form` :2483 (853 lines, 37 branches) | no |
| `method_named` | `(p-method-call $o "m" ARGS)` | ExprToCL `gen_methodcall_form` :3736 | no |
| `print` `say` `join` `sprintf` `length` `keys` … | `(p-print …)` `(p-join …)` `(p-sprintf …)` -- one head per builtin, bound only if in `%WANTARRAY_SENSITIVE` (52 names) | ExprToCL `gen_funcall_form` builtin arms | no |
| `cond_expr` `and` `or` `dor` | `(p-if C A B)` as a value, `(p-&& …)` `(p-\|\| …)` `(p-// …)` | ExprToCL `gen_ternary` :3923, the op table | no |
| `enteriter` / `iter` / `leaveloop` | `p-foreach` / `p-foreach-raw` / `p-foreach-range` / `p-foreach-range-raw` (+ `:arrays t`, `:dyn t`) -- FOUR heads chosen by FIVE facts | Parser2 `_lower_compound` :13251-:13254 | the runtime `p-foreach` still dispatches on `:dyn` / `:arrays` -- compile-time facts restated as keyword arms |
| `enterloop` (while/until), C-style `for` | `(p-while C …)`, `(p-for (init) C step …)` | Parser2 `_lower_compound` | no |
| `match` / `subst` / `trans` | `(p-=~ $s (p-regex …))` with the regex TIER fact, `(p-subst …)`, `(p-tr …)` | ExprToCL `gen_substitution_form` :5860, `gen_transliteration_form`, `Pl::RegexTier` | no |
| `sort` | `(p-sort (p-sort-cmp ($a $b) …) LIST)` → rewritten to `(%p-sort-classic MODE …)` by the Kind-B pass | ExprToCL; `Pl/ClassicSort.pm` post-lowering | the one legitimate "twice": the licence is about the CONSUMER, so it lives where all consumers meet |
| `map` / `grep` | `(p-map (lambda ($_) …) LIST)` -- a bare lambda, so `return` flies through (ir-spec rule) | ExprToCL block-arg arm | no |
| `leavesub` / `return` | `(p-sub NAME (p-args-body :copy …))`, `(p-raw-params …)` under `sig-classic`; `(p-return …)`, or the tail expression itself under `tail-return` | Parser2 `_lower_sub_inner` :10194, `_lower_body_regime` | no |
| `wantarray` | `(p-wantarray)` | | no |
| `nextstate` | nothing (the `line-track` Kind-A is default OFF) | | -- |
| `gvsv` + `local` | expression-level `local` → `p-box-for-local`; the STATEMENT `local $x = …;` → **v1 text** (337 statements in the corpus) | `_fallback_stmt` | -- |
| `use` / `no` / `require` / `BEGIN` | **v1 text** (`p-eval-always`, 18 text sites in Parser.pm; 558 statements in the corpus) | `_fallback_stmt` | -- |
| string `eval` / block `eval` | `(p-eval STR alist)` with the capture alist (ir-spec §9.1), `(p-eval-block …)` | ExprToCL `_gen_eval_string_form` :3529 | no |

Reading: of 24 construct families, 19 are decided ONCE, in the compiler, and
named.  Five are decided twice -- assignment through `p-setf` (7.8 % of
stores), list assignment's LHS shape, `.=`/`++` on element places, the
`foreach` keyword arms, and `sort` (by design).  Two families are not lowered
at all but handed to v1 text (§7).

### 2b. The store census (120 files: 111 `perl-tests/*.t`, `lib/Math/BigInt/Calc.pm`, 8 everyday programs; 100,728 emitted lines)

| store spelling | count | who decided |
|---|---|---|
| `(p-my-= $x V)` | 2,498 | compiler (my-store to a box) |
| `(p-scalar-= $g V)` | 1,390 | compiler (package variable) |
| `(p-array-= @a V)` | 1,038 | compiler |
| `(p-list-= (vector …) (vector …))` | 461 | compiler names it; runtime picks the LHS arm |
| `(p-hash-= %h V)` | 154 | compiler |
| `(setf $n V)` raw slot | 93 | compiler (`raw-slot` fact) |
| `(setf (p-gethash\|p-aref …) V)` | 88 | compiler (`elem-setf` fact) |
| **`(p-setf PLACE V)`** | **483 (7.8 %)** | **runtime macro** -- 16 place shapes reached: `p-aref` 92, `p-gethash` 84, pipe-quoted specials (`$^W` 28, `$/` 21, `$\` 12 …) 70, `p-substr` 69, `p-cast-$` 27, `p-aref-deref` 25, `p-errno-string` 20, `p-vec` 18, `p-aslice` 17, `$_` 17, `p-gethash-deref` 12, `p-pos` 11, `p-hslice` 9, `p-stash` 6, `p-backslash` 4, `progn` 2 |
| `(p-.= $s V)` / on an element | 316 / 18 | compiler / runtime |
| `(p-post++ $x)` `(p-pre++ $x)` … / on element, box or cast places | ~510 / ~50 | compiler / runtime |
| `(p-incf $x …)` `(p-incf-raw …)` `(p-decf …)` `(p-*= …)` | 39 / 32 / 30 / 10 | compiler (`numeric-slot`, the `-raw` twins) |
| `(%pcl-to-number-strict …)` / `(%pcl-to-string-strict …)` | 22 / 20 | compiler (the B-regime freeze) |
| `(%pcl-str-buffer …)` / `(%pcl-str-append …)` | 3 / 2 | compiler (`str-buffer`) |

So the runtime review's point 5 ("the compiler emits a generic store and the
runtime macro re-derives what the compiler knew") is true of one store in
thirteen.  The twelve shape macros' arms are REACHED -- 16 of `p-setf`'s 24
in this corpus -- so they are not dead; they are the long tail of places the
compiler has no name for yet.  perl's optree has a name for each (`helem`,
`aelem`, `substr` with `OPf_MOD`, `pos`, `vec`, the `rv2sv` of a special);
the fix is the compiler naming them, and it is ONE emitter site (`=` in
`gen_binary_op_form` :1929) that would do so.

### 2c. Emission sites per head (rule 11)

ExprToCL emits 95 distinct structured heads, Parser2 39; SEVEN are emitted
from both files (`p-setf`, `p-list-=`, `p-scalar-=`, `p-if`, `p-die`,
`p-flatten-args`, `p-bareword-value`) -- a statement-position and an
expression-position site each, legitimate but a watch item.  **29 heads are
emitted both as a tree and as v1 TEXT** (`Pl/Parser.pm` still prints 53
distinct `p-` heads as strings: `p-eval-always` 18 sites, `p-if` 8,
`p-set-current-package` 7, `p-defclass` 6, `p-list-=` 5, `p-copy-array` 4 …) --
the "two dialects" of `docs/generated-cl-ir-review.md` §4b.1, still the one
real duplication of lowering knowledge, and it ends when the seam closes (§7).

## 3. The representation verdicts (box vs raw slot)

### 3a. How often the verdict fires

| population | `:box` | `:scalar` (raw) | `:num` | `:str` | `:str-buffer` | raw share |
|---|---|---|---|---|---|---|
| `perl-tests/*.t` (111 files) | 1,464 | 120 | 3 | 9 | 2 | 8.4 % |
| `lib/Math/BigInt/Calc.pm` | 218 | 41 | 9 | 0 | 0 | 18.7 % |
| 8 everyday programs | 10 | 1 | 0 | 1 | 0 | 17 % |
| `:captured t` lexicals (a CL closure shares the binding; NOT a box) | 143 | | | | | |

The fast slot is the exception.  VarAnnotator's event vocabulary is 18
boxing events (`write-embedded`, `write-cond`, `write-list`, `write-incdec`,
`write-compound`, `write-deref-viv`, `ref-taken`, `regex-target`, `local`,
`pos-arg`, `mutating-builtin-arg`, `handle-viv-arg`, `foreach-alias`,
`deref-of-unwritten`, `eval-in-region`, `fallback-text`, `multi-decl`,
`foreach-alias-rw`) and any doubt boxes -- "wrong = slow, never fast-but-wrong"
is the target's contract and it holds.

### 3b. What a raw slot drops, probed (perl → `./runpcl`; scratchpad `s526c/repr/`)

| SV state perl keeps on every scalar | raw slot | probe | result |
|---|---|---|---|
| `pIOK`/`pNOK` -- "used as a number" | NO place for it; the box face is exact since s525u | `my $w = "ab"; $w += 0; $w \| "  "` | **#3780** (widened at the s525u merge: the ordinary `my $x` IS the raw case) -- the one dropped bit |
| `pos()` magic | the match target is a boxing event (`regex-target`) | r1: three `pos` reads incl. an assignment | identical |
| aliasing through `@_`, `for`, `tie` through `$_[0]` | `ref-taken` / `foreach-alias-rw` / copy-back (#3348) | r2: `inc($x)`, `tieit($y)`, `for my $e ($z)`, `$_++ for $w` | identical |
| dualvar | a dualvar is a representation FACT (ir-spec §3.2a) -- the freeze declines into a box | r3: `dualvar(5,"five")` + 1, truthiness of `dualvar(0,"zero")` | identical |
| a string `eval` reading the name | `eval-in-region` boxes it (block `eval` does not: `raw-block-eval`) | r4: `eval '$x + 1'`, `eval 'uc $s'`; `0.1+0.2`; `"3abc" += 1`; `$i/4`; `"7"+1 . "x"` | identical |
| `local` | not on a `my`; on globals and `$_` | r5: `local $g` in a loop, `local $_`, foreach-alias writes, a swap | identical |
| an overloaded object arriving in a numeric-use slot | the freeze's runtime DECLINE stores `(p-box-init V)` | r7: `my $t = $m + 1` with `+` overloaded → `:scalar` holding 42 | identical for the slot; the probe found **#3880** instead (§13) |
| IV/UV/NV range, `use integer` wrap | unbounded host integers | r9: `use integer; 9223372036854775807 + 1` = perl -9223372036854775808, PCL 9223372036854775808 | **RULED** (`docs/not-supported.md` "Integers are unbounded", #1513) |

Verdict: the raw-slot premise ("an unshared scalar needs no cell") is complete
EXCEPT for perl's numification flag -- and that one is not a verdict error
but a representation gap the runtime review already named.  The invariant "a
raw slot holds a host number or string, never a box" (ir-spec §2.2) is kept by
the decline path, probed.

## 4. Context: perl's static `OPf_WANT` against PCL's dynamic `*wantarray*`

| population | emitted lines | context binds | per 100 lines |
|---|---|---|---|
| `perl-tests/*.t` | 95,914 | 12,720 | 13.3 |
| `lib/Math/BigInt/Calc.pm` | 4,261 | 354 (scalar 201, list 57, void 50, caller 46) | 8.3 |
| 8 everyday programs | 553 | 25 | 4.5 |

By kind over the corpus: `p-void-ctx` 8,636, `p-list-ctx` 2,248,
`p-scalar-ctx` 1,546, `p-caller-ctx` ~300, bare `(let ((*wantarray* …` ~250
(the two-binding sites ir-spec §4 allows).  `p-void-ctx` is the most frequent
head of the whole IR -- about 3,900 are the sub-body regime's single wrap and
about 4,700 are per-statement wraps, **4,212 of which wrap a user-sub call**
(`pl-is` 2,827, `pl-ok` 387, `pl-cmp` 295, `pl-like` 156 … in test code; the
everyday programs show the same shape at 4.5 per 100).

What is static in PCL today: a builtin's context sensitivity
(`%WANTARRAY_SENSITIVE`, 52 names -- the #2775/#2801 work), a KNOWN user sub's
insensitivity (`insensitive-call`, from `_sub_return_facts`: `(pl-g 1 2)` in
probe o3 is bare), the argument context from the callee's signature (ir-spec
§4.1), the sub-body regime.  What is still dynamic: every call in VOID
position to a sub not proven insensitive, the list-assignment binds, the
`g`-modifier matches.  perl's answer is a 2-bit flag on the op; PCL's is a
`let` of a special variable per statement -- cheap, but 8,636 of them in
100,728 lines is the IR's loudest head, and the `insensitive-call` fact already
answers the void case for the same callees (option C4).  The probe r8 (seven
context shapes incl. a tail call and a `return`) is identical to perl: the
protocol is correct; the question is only how much of it the compiler can
decide.

## 5. Facts and passes, against the target's contract

`docs/v2-target-architecture.md` §3 asked for a registry where every
optimization is a facts-licensed emission (Kind A) or a CLForm→CLForm pass
(Kind B), and §2.3 for facts as data.  Measured:

| asked | today |
|---|---|
| Kind-A names | **24** (`raw-slot raw-numeric str-buffer foreach-range insensitive-call elem-setf tail-return raw-block-eval raw-op-family raw-closure-capture raw-topic raw-return-family local-push dyn-loop-exit foreach-arrays numeric-slot symref-const line-track fh-scope-close sig-classic list-decl-split args-copy foreach-raw`), one default-OFF (`line-track`), one exempt from `none` (`fh-scope-close`, a correctness mechanism) |
| Kind-B passes | **1** (`classic-sort`, `Pl/ClassicSort.pm`, 222 lines) -- the pass contract's one worked example, and it is the RIGHT kind: its licence is about the sort's consumer, and the lowered tree is the one place all six consumers meet |
| hidden switches | **none**: the 17 other `PCL_*` variables the compiler reads (`PCL_E2_RAW_CENSUS`, `PCL_SPAN_DEBUG`, `PCL_B_DEBUG`, `PCL_DROP_ANNOUNCE`, `PCL_PROTO_STATS` …) are debug and census instruments; a typo in `PCL_OPT` dies naming the list |
| `PCL_OPT=none` = the general form | identical run output on three probes; `Pl/t/passes-01.t` guards it; `--facts` prints every licence that held |
| facts as data | `_sub_facts`, `_decl_facts`, `_expr_root_facts`, `_sub_return_facts`, `_scan_lex_facts`, VarAnnotator's verdicts: yes.  **Keyed on token TEXT**, not bindings -- so every rename pass must run before VarAnnotator (parser review §3) |
| the lexical scope map | **not built**: `_let_bound_vars` is still the live accumulator -- 51 uses, 12 `local`/reset sites in Parser2, 5 in ExprToCL |
| the context map (one encoding) | **three encodings still**: PExpr's integers (`SCALAR_CTX 0 … INHERIT_CTX 3`, 63 uses in ExprToCL), CLForm's strings (`'t' 'nil' ':void'`), the runtime's values |
| the Stmt classifier (§2.2) | **not built**: statement shapes are recognised by `isa` + `content` tests at the point of use in `_lower_block_1` (37 + 15), `_lower_stmt` (`!$mod && @$expr >= 3 && isa Symbol && content =~ /^\$\w+$/ && isa Operator && content eq '='` :12210-:12242) and `_lower_compound` |

Verdict: the optimization half of the target is DONE and clean; the
data-structure half (Stmt records, facts on bindings, one scope map, one
context encoding) is where the code generation still has the parser's shape.

## 6. IR hygiene

`tools/ir-inventory.pl`: **761 exported names, 56 families, 19 UNCLASSIFIED**
(363 functions, 208 macros, 142 variables, 25 lazy functions, 10 symbol
macros, 6 globals, 4 classes).  `tools/ir-host-leak.pl` over the 111
perl-tests files: **31 leaked symbols, 208,018 tokens**:

| class | symbols | what it is |
|---|---|---|
| `use integer` arithmetic lowered to HOST ops | `- + * truncate rem logand logior logxor lognot 1+ 1-` (11; 69 occurrences) | real: ExprToCL :1976-:1988 emits CL arithmetic on `p-int` operands; a backend must implement CL integer semantics there, and these are also the ruled unbounded faces (r9) -- name them (`p-+-int` like `p-<<-int`) or whitelist them in ir-spec §11b |
| other ops | `format` (yadayada's die text), `intern`, `eq` | 3; each a one-site fix or a whitelist row |
| quoted data | labels (`label1`, `last_113684` …), operator names from cmpchain.t, `undef`, `try`, `y`, `of`, `start` | 17; harmless -- symbols as DATA |

Host idioms the s277 review asked to close (its §3.4, task #75, pending):
`(make-p-box …)` 6,079, `(make-array 0 :adjustable t :fill-pointer 0)` 1,175,
`(make-hash-table :test 'equal)` 302, `(vector …)` 1,304 in the corpus -- a
translator still special-cases SBCL's spelling of "new scalar / array / hash /
list".  The kernel forms (`block` 2,289, `let` 1,836, `progn` 1,146,
`tagbody` 1,080 + `go` 425 for loop control, `lambda` 785, `catch` 636,
`setf` 642) are inside the §11b whitelist.  Of the s277 friction list: §3.2
control characters CLOSED (`p-esc` 1,282 uses), §3.3 regex literals CLOSED,
§3.5 context macros DONE (s414), §3.8 the normative spec DONE; §3.4
constructors NOT done; §3.6 duplicate declarations NOT done (93 duplicate
`p-defcell` names across the 120 files; the `$a`/`$b` pair is now on demand);
§3.7 the environment is still baked in (5 `vector-push-extend` + 7 machine
paths per file; #217); §4b.1 the two dialects remain (29 heads, §2c).  Drops:
5 `;; PARSE ERROR` forms in 120 files = the census.

Speed-over-readability (RULED 2026-07-02) cost the IR exactly what §2b shows:
the `(setf (p-gethash %h "k") $y)` host store beside `(p-setf …)`, the `-raw`
twins, `(%pcl-to-number-strict V "$n")` with its string argument the runtime
never reads, and `(p-foreach-range-raw …)` beside three sibling heads.  Each is
documented in ir-spec §2.2 / §10d; none is undocumented.

## 7. Duplication, the seam, and dead code

`tools/dup-census.pl` (5-line shingles): **ExprToCL 27 runs (1 exact), 212
deletable lines = 3.2 %** (the two biggest: a 5×5 and a 6×4 shape, both in
the slice / element emitters #387 already folded once); **Parser2 34 runs /
226 deletable = 1.4 %, of which 8 small pairs in the lowering half**
(`_lower_embedded_body` vs `_lower_embedded_anon` twice, three inside
`_lower_compound`, two inside `_lower_block_1`); VarAnnotator 1; CLForm 0;
Passes 1.  Rule 11 holds in the emission files; the s386 review's compiler
duplication findings (precedence copies, two generators) are gone.

**The s386 number re-measured.**  s386 found "88 % of expressions still
lowered by the v1 machinery".  The seam census (`PCL_V2_SEAM_CENSUS=1`, built
into Parser2) over the 120 files: **20,825 expressions lowered natively, 0
through v1** -- Phase A (s411) closed the expression seam completely -- and
**1,058 STATEMENTS lowered as v1 text (5.1 % of statements)**:

| statement kind | count | why it is text |
|---|---|---|
| `local …;` | 337 | v1's `_process_local_declaration` (1,068 lines) owns the statement form |
| `use …;` | 244 | the phase model is structured (`p-run-compile-phase-blocks`) but the statement is captured as `p-eval-always` text |
| `BEGIN { }` | 147 | same |
| `no …;` | 133 | same |
| `sub NAME (SIG) { }` with defaults / slurpy placeholders | 89 | v1 binds the parameters; plain signatures are native since `sig-classic` (s501q) |
| `return … for/while/until …;` | 37 (26 in Calc.pm alone) | `_modifier_needs_fallback`: only `if`/`unless` modifiers lower natively |
| `require …;` | 34 | as `use` |
| `delete local …;` | 12 | as `local` |
| `my … = … for/while …;` | 10 | the #3375 desugar covers part of it |
| `__END__` / `__DATA__` / `END { }` / `do` / `push` | 10 | whole-statement fallbacks |

For these, 33 `_process_*` statement processors in `Pl/Parser.pm` stay live
through `fallback_parser`, 53 `p-` heads are printed as strings (29 of them
also emitted as trees), `Pl::CLForm::raw` keeps the tree open, and the printer
asks `is_raw`/`is_raw_wrap` 160,000 times per module.  The parser review's
§1c said four kinds; it is eight kinds and ~1,000 statements per 20,000.

## 8. Compile time

§1c has the profile: the code generation is 6.0 %.  Wall time of `./pl2cl <
lib/Math/BigInt/Calc.pm`: 3.65 / 3.72 s default, 3.66 / 4.20 s under
`PCL_OPT=none` (noise; the facts cost nothing measurable); `perl-tests/pack.t`
3.78 s.  Inside the 6 %: CLForm's `_flat` (the one-line-vs-multi-line
decision, 136,514 calls) is a third of it, the seam predicates a seventh,
VarAnnotator's per-statement re-parse (`_tw_expr_parse`, 1,270 calls) and
`_lower_block_1` a twentieth each.  Nothing here changes the parser review's
bars (Calc.pm 3.7 s → under 1.5 s is the resolver's to deliver).

## 9. Growth: what the 90 days added

212 commits touched the four emission files.  A 40-commit even sample
(`git log --since=90.days`, every fifth) classified by subject:

| kind | commits | examples |
|---|---|---|
| semantic fix (perl says X, PCL said Y) | 17 | `&NAME(...)` ignores the prototype; a bareword invocant with a same-named sub is a call; backslash-newline in a dq string; `eval STRING` bound by its own context; `->(` joins the paren-scalar-base family; a transparent paren layer passes scalar context on |
| new construct or builtin | 8 | `eof()` with empty parens; telldir / the system databases; `fcntl`; the n-at-a-time `foreach`; sub attributes without XS; `%{ELEM} = LIST` vivifies |
| fact / optimization (a Kind-A name) | 7 | `sig-classic` + `args-copy`; `numeric-slot`; `local-push` + the array facts; `foreach-arrays`; the `utf8::` mutators' lvalue |
| structural | 5 | Phase A (one expression compiler, ExprToCL2 deleted); the 24 unreachable text emitters deleted; #387's family folds; the context bind named (#281); the raw census |
| docs / instruments | 3 | ir-spec §2.2 + §3.2f + §6.3c; the F-DEBUG line; the review pass |

ExprToCL's net is +676 lines in 90 days with a -4,006 month -- the file is
being REWRITTEN toward one shape, not accreted.  VarAnnotator's +1,264 net is
the fact families of §5 (each a registry name).  Parser2's lowering half took
21 % of that file's growth; the other 79 % (`parse` +954, the repairs, the
renames, the requalifiers) is the parser review's subject.  The emission side
grows the way the runtime does -- one perl mechanism per batch -- and the
`_lower_block_1` / `_lower_compound` share of it (+1,234 lines in two subs that
were already over 600) is the one place where the parser's pattern recurs: a
new statement shape is a new `if` in a 1,188-line sub, because there is no
classifier to put it in.

## 10. Rules 10, 11, 12 in the compiler

| rule | measured |
|---|---|
| 10: no sub over ~80 lines | ExprToCL **15 over 80, 2 over 200** (`gen_funcall_form` 853 -- 37 branches dispatching on the builtin's NAME; `gen_binary_op_form` 365); VarAnnotator 7 / 2 (`_tw_walk` 367, `_analyze_tree` 290); Parser2's lowering half **9 over 80, 3 over 200** (`_lower_block_1` 1,188, `_lower_compound` 630, `_lower_stmt` 303); CLForm 1 (`to_string` 105) |
| 11: one mechanism | §2c: 7 heads from two emitters, 29 heads in two dialects, 212 + 226 deletable lines; the op TABLES (`%OP_EXCEPTIONS`, `%WANTARRAY_SENSITIVE`, `%COMPOUND_ASSIGN_OP`, `%ELEM_BOX_TWIN`, `%RVALUE_DEREF_FN` … 18 tables in ExprToCL) are the right shape and are consulted, not copied |
| 12: a missing case dies | `_lower_compound`: 7 `die "Parser2 TODO: …"` arms, unknown shapes DIE (51 such sites in Parser2, 128 `die`s); `_lower_stmt` / `_lower_block_1`: the default is **`_fallback_stmt` = v1 text**, which RUNS -- neither a die nor a silent drop, but the one place where "not lowered" is not loud; the #138 drop (`;; PARSE ERROR` + the stderr announcer): 5 sites in Parser2, 13 in Parser.pm, 5 drops in this corpus = the census |

## 11. What is right and must survive

ONE expression compiler with one parse per expression (s411).  CLForm: text
only at the printer, parentheses by construction, 2-space depth.  The registry
with its `none` form, `--facts`, the fact-first gate spelling, and the one
Kind-B pass as the model for licences about consumers.  The verdicts'
conservatism (18 boxing events; probed sound).  The `-raw` twin discipline
(the same new-value form for both stores, so the disciplines cannot diverge).
The context macros and the `%WANTARRAY_SENSITIVE` table.  The compile-time
autovivification rule (§3.2c).  `Pl::InterpScan` as the one scanner.  The
measuring tools -- `corpus-diff`, `emission-ab`, `ir-inventory`,
`ir-host-leak`, `drop-census`, the seam census, `PCL_OPT=none` -- which make
every claim in this document a command.  `docs/ir-spec.md` as the normative
manual, matching the emission where checked (§2.2, §4, §10d).  The rule-12
culture in `_lower_compound`.

## 12. Verdict, options, recommendation

**Verdict.**  Sane as an artifact, and as a structure in its expression half
and its registry; the statement-lowering half has the parser's shape (shape
tests at the point of use, a text default, a mutable scope accumulator) and
grows the parser's way.  No restructuring program of its own: the three debts
are the second half of the parser program already decided (`#3694`, Option 1),
and the runtime's twelve arms shrink as the compiler names the places.

| option | what | size | bar (the number that proves it) |
|---|---|---|---|
| **C1 -- the statement classifier** (target §2.2; parser program step 1d, first) | ONE `Stmt::classify($ppi_stmt, $env)` returning `kind` + pre-split spans; `_lower_block_1` / `_lower_stmt` / `_lower_compound` switch on `kind` and never index token arrays; the expect-state checker's statement tables (s525p) are its data | 2-3 sessions, IDENTICAL emission | `_lower_block_1` 1,188 → under 300 lines; `isa`/`content` shape tests outside the classifier 52 → 0; corpus-diff IDENTICAL; gate + the sweep |
| **C2 -- close the text seam, one kind per session** (parser review §4's "second filler", promoted) | structured lowering for `local` statements (337; `p-local` from v1's 1,068 lines), `use`/`no`/`require` (411; the phase model already has the form), `BEGIN` (147), signatured subs (89; generalise `sig-classic`), loop-modifier statements (47; generalise #3375's desugar), the tail (22); then delete `Pl::CLForm::raw`, `fallback_parser`, the 33 `_process_*`, the 53 text heads | 4-6 sessions, EMISSION change per kind (generation bump, sweep, corpus-diff explained per file) | seam statements 1,058 → 0 on the 120-file corpus; `is_raw` calls 160k → 0 per module; Parser.pm's 13,562 lines → its pre-pass/facts half only (then renamed, parser review §4) |
| **C3 -- facts on binding ids** (= 1c's consumer side) | VarAnnotator and the facts keyed by the binder's binding id; `_let_bound_vars` → the binder's scope query; one context encoding | rides on the 1c ports (`docs/binder-dual-run-s525.md` §6) | `_let_bound_vars` 51 → 0; the three context encodings → 1; corpus-diff IDENTICAL |
| **C4 -- the void-position bind** | widen `insensitive-call` to void position (a callee proven not to observe `*wantarray*` needs no bind there either), and let a shim's sub facts count as KNOWN | half a session, measure first | `p-void-ctx` 8,636 → the count after; bench rows no slower; sweep +0; gate |
| **C5 -- the constructor macros** (#75's first slice; s277 §3.4, §3.6, §3.7) | `p-new-sv/av/hv`, `p-vlist`, one `p-init-env` preamble form, de-duplicated `p-defcell`s | 1 session, EMISSION change -- bundle with the next generation bump | host idioms 8,860 → 0 in the corpus; duplicate defcells 93 → 0; bench unchanged (the macros inline) |
| **C6 -- name the integer-mode arithmetic** (or whitelist it) | `p-+-int` … like `p-<<-int`, or ir-spec §11b rows | half a session | host leaks 31 → the 17 data symbols |
| C7 -- rule-10 splits | `gen_funcall_form` by builtin family; `_lower_block_1` falls to C1 | fillers | subs over 200 in the emission files 7 → 0 |

**Recommendation.**  Put C1 and C2 into `#3694` as step **1d**, run in the
parser's slot: C1 right after 1a (the expect-state repair pass), because a
classifier is what makes 1a's tables consumable and it is identical emission;
then C2 one kind per session in the order of the table, interleaved with the
1c ports, because each kind deletes v1 code the ports would otherwise have to
carry (the s411 rule); C3 is how 1c lands in the emission.  C4 and C5 are
fillers for whichever slot has a bench bar handy; C5 waits for a batch that
bumps the generation anyway.  C6 and C7 are half-session fillers.  The runtime
review's R4 code half (`p-setf` to one arm) is not a step of its own: it
follows from C1 + C2 naming the places, measured by the §2b census shrinking
`p-setf` from 483 toward zero.  Nothing here competes with correctness batches
for the second slot, and nothing changes what the generated code DOES until a
C2 kind lands -- each of those is an emission change with the full bars.

## 13. Filed and cited

- **#3880 FILED (runtime, outside this review's subject):** with `fallback`
  unset, `$obj * 1` on a class that overloads `+`, `""` and `0+` but not `*`
  DIES in perl ("Operation \"*\": no method found" -- `*` has no autogeneration
  rule) and answers 7 in PCL (the `0+` conversion, as if `fallback => 1`).
  Probe `s526c/repr/r7-decline.pl`; related #972 / #1005 (done), #1006
  (pending, the `fallback => 0` face of the same table).
- **Cited, not re-filed:** #3780 (the raw slot has no numification flag --
  the ordinary `my $x`), #1040 (the literal face: `"12" | "3"`), #1513 /
  `docs/not-supported.md` "Integers are unbounded" (`use integer` wrap, probe
  r9), #75 (the constructor macros), #217 (the baked-in preamble), #219 (the
  void-position comma emission), #3694 (the parser program this review's
  options join), #2775 / #2801 (the list-context binds, done).
- Probes and measurements: the fork's scratchpad `s526c/` (`ops/*.pl` with
  `.lisp` + `.concise`, `repr/*.pl`, `corpus/` the 120 emissions, `seam/` the
  census rows, `nytprof.out` + `nyt-agg.pl`, `dup-*.txt`, `ir-host-leak.log`,
  `STATE.md`).

Reading list for whoever executes 1d: `docs/v2-target-architecture.md` §2.2
and §2.3, `docs/ir-spec.md` §2.2, §2b.2a, §4, §10d, §11b,
`docs/binder-dual-run-s525.md` §6, `docs/raw-numeric-verdict.md`,
`docs/boxed-aggregates-design-s455.md`, `docs/generated-cl-ir-review.md` §3.4-
§3.7 and §4b.1, perl's `op.c` (`op_contextualize`, `op_lvalue`, `S_fold_constants`,
`rpeep`) and `B::Concise`'s output for the six probe programs.
