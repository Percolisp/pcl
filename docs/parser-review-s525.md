# The parser, reviewed against perl's own front end (s525, Fable, 2026-10-24)

The USER's question: "There has been addition after addition with special
cases, I wonder if it is sane anymore.  The Perl implementation has lots of
those weird corner cases, coming from the exact implementation.  Maybe we can
get simpler and clearer code from looking at that original implementation?
If that works, we will look at the runtime after."

Everything below is measured on main `c3706d71` (= MAIN-READY-43 + notes) and
on perl 5.40.3's source (`~/perl5/perlbrew/build/perl-5.40.3/`).  No code was
changed.  The two inventories this review rests on (every pass of
`Parser2::parse` in call order; every term/operator predicate of PExpr) were
taken by two read-only agents and are condensed in §1; the profile, the
perl-side measurements and the optree probes are Fable's.

## 0. The answer in ten lines

1. **It is sane as an ARTIFACT and not as a STRUCTURE.**  Every special case
   is documented, probed against perl and loud when it misses (gate 331 /
   11,286, sweep 18,759, everyday 116 of 122).  But the compiler side is 54,155
   lines growing by about +15,000 net lines per quarter, `Parser2::parse` runs
   **69 passes** of which **18 may re-serialize and re-parse the whole
   document** (two of them up to 20 rounds), the variable-scoping question is
   answered by **18 separate scope-walk implementations**, and the
   term-vs-operator question by **34 predicates plus two 1,800-line loops**.
2. **Yes: perl's corner cases come from three small mechanisms, and PCL
   re-derives each one as many unrelated special cases.**  toke.c decides every
   ambiguous character from ONE state variable (`PL_expect`, 11 states); it
   gives every builtin ONE of about ten argument-shape classes, and perly.y has
   ONE grammar rule per class; pad.c answers "which declaration is this" from
   ONE table (name, visible-from, visible-to, captured-from).  PCL has no single
   expect state (PPI's heuristic plus ~35 repair passes), no class table (174
   arity entries in 25 shapes plus the predicates), and no binding table (the
   18 walks).  Perl's code is not simple -- toke.c is 14,025 lines -- but each
   corner case is a `case` in one switch, not a new pass.
3. **Three ways to use the original implementation**, from cheap to radical:
   as a MODEL (copy its three states into the present shape, deleting passes:
   §4 Option 1, ~12-18 sessions, every step byte-diff gated); as a SOURCE to
   port (an own toke.c-shaped lexer replacing PPI: §4 Option 2, 25-40 sessions,
   not recommended first); as a COMPONENT (let perl compile the program and
   translate its optree: §4 Option 3 -- exact by construction, replaces about
   43k of the 54k lines, 40-60 sessions, and raises two USER decisions).
4. **Recommendation (§5): a 2-3 session SPIKE that sizes Option 3 before any
   Option 1 investment**, because Option 1's biggest piece (the binding table)
   is exactly what the optree hands over for free, and a fix in code a phase
   deletes is wasted (the s411 rule).  The runtime is untouched by every
   option, so "the runtime after" can start whenever the USER wants.

## 1. What the parser is today (measured)

### 1a. Size and growth

| file | lines | subs | subs > 80 lines | > 200 | largest |
|---|---|---|---|---|---|
| `Pl/Parser2.pm` (v2 statement lowering) | 15,728 | 355 | 30 | 6 | `parse` 1,329, `_lower_block_1` 1,188, `_lower_compound` 630, `_rename_spanning_lexicals` 491 |
| `Pl/Parser.pm` ("v1", see 1c) | 13,562 | 265 | 37 | 8 | `_process_local_declaration` 1,068, `_process_sub_statement` 438, `_process_include_statement` 404 |
| `Pl/PExpr.pm` (expressions) | 7,790 | 137 | 11 | 4 | `handle_subcalls` 1,815, `parse` 1,459 |
| `Pl/ExprToCL.pm` (expression emission) | 6,704 | 176 | 15 | 2 | `gen_funcall_form` 853 |
| `Pl/VarAnnotator.pm` | 2,537 | 55 | 7 | 2 | `_tw_walk` 367 |
| `Pl/Environment.pm` | 1,694 | 59 | 2 | 1 | |
| the other 14 files | 5,140 | | | | InterpScan 794, StringInterpolation 736, ProtoCache 736, Config 707, Manifest 528, CLForm 505, TokenUtils 454 |
| **total `Pl/`** | **54,155** | | | | |

Churn over the last 90 days (`git log --numstat`):

| file | commits | added | removed |
|---|---|---|---|
| Parser2.pm | 268 | +12,012 | -2,552 |
| Parser.pm | 174 | +7,367 | -2,106 |
| PExpr.pm | 109 | +3,595 | -1,561 |
| ExprToCL.pm | 162 | +4,965 | -4,289 |
| VarAnnotator.pm | 42 | +1,804 | -540 |
| (`cl/pcl-runtime.lisp`, for scale) | 730 | +36,292 | -9,852 |

The compiler side gains about 15,000 net lines a quarter.  The 38 confirmed
PPI bugs in `docs/ppi-upstream-bugs.md` each carry a repair pass; the sub
names alone count 20 `_repair_*`, 8 `_fix_*`, 6 `_merge_*`, 9 `_rename_*`,
7 `_rewrite_*`, 8 `_split_*`, 5 `_normalize_*`, 40 `_is_*` predicates.
The one smell test that matters passed: no CPAN module name and no non-core
function name appears in `Pl/` (the layer rule holds).

### 1b. The pipeline as it actually runs

**`Pl::Parser::_ppi_parse` (18 passes).**  A regex pre-process of the SOURCE
TEXT (hex/binary floats to decimal, `sub a'b`, `for my Type $x`, `CORE::my`,
`format` blocks), the PPI tokenizer with a signature-aware `$)` repair at the
tokenizer level, then 14 whole-document token walks (modulo-vs-`%-`, spaced
sigils, glob slots, prototype attributes, anon-sub signatures desugared to
`my (...) = @_`, `__SUB__`, loop modifiers desugared to blocks, bare `vNN`
words, non-ASCII symbols, punctuation arrays, `*$o` after a block, indented
heredocs, `print({$fh} LIST)`), and **a serialize-and-reparse if any of seven
of them changed anything**.

**`Pl::Parser2::parse` (passes 19-69, 1,329 lines).**  A PPI repair block
(passes 19-39: decorations, `tie my`, null statements, anon-sub attributes,
n-ary and alias `foreach`, the glob-pattern and `WORD /PAT/` cascades looping
to a fixpoint, `)*name`, punctuation globs and hashes, `)-name`, `$x.2`, a sub
named `x`, term-initial `~~`, `<FH>` followed by `/`, orphan `finally`);
semantic normalisation (signatures, literal-list declarations split,
non-initial `local` terms hoisted, `use bigint` literal handlers, lexical subs
renamed); early registration (prototypes twice, strict, glob constants, `use
constant`, `use subs`, three pragma-region tables); `goto` rewrites; two
package-switch requalifiers; segmentation; the eval-mode collapse; nine
identity passes (spanning, state, exception `my`, eval captures, lex facts,
capture promotion); the sub prepass; per-segment lowering; phase-order
assembly.  Of the 51 Parser2 passes, **19 exist to correct a PPI mis-lex,
about 18 to make a perl semantic explicit, about 20 to serve the compiler's own
representation** (the inventory's classification).  **18 of the 51 may
re-serialize the document to text and re-parse it; each re-parse re-runs the
14 `_ppi_parse` walks.**  Three repair comments cite the wrong
`ppi-upstream-bugs.md` section (`_restore_signature_gid` says §24, is §32;
`_brace_glob_slot_symbol` §24 vs §27; `_repair_term_initial_complement` §20 vs
§21) -- a residue to fix.

**`Pl::PExpr::parse` (1,459 lines).**  A 12-pass token pre-scan
(`cleanup_for_parsing`, split qualified words, pid magic cast runs, braced
punctuation magic, `$#` retagging, `print $fh -e` fusion, the filetest `$_`
default, braced deref subscripts, list-slice subscripts, elided call arrows,
dynamic glob slots) and then **`handle_subcalls` (1,811 lines) BEFORE term
reduction**: a `CORE::` strip, two indirect-object pre-passes, a left-to-right
loop over `name(...)` / `name BLOCK LIST` / `sort SUBNAME` / `open` / `split` /
`print FH (...)`, a right-to-left loop over paren-less calls deciding zero-arg
words, named-unary extent (`_term_extent`, with a legacy fallback), the
list-operator argument ceiling, the print filehandle slot, `split`'s first
argument, the bareword verdict.  Then single-term reduction by PPI class (420
lines), comma lists, the #153 term fold, postfix chains (490 lines, five method
call cases, cast runs), slices and glob slots, and the operator-precedence loop
with chained comparisons and the ternary.

**34 term-vs-operator / argument-extent predicates** (PExpr + TokenUtils +
two in Parser2): 11 consult the Environment (prototypes, declared subs,
filehandles), 4 read Config tables only, about 19 read token shape only.
`handle_subcalls` also bypasses the predicates with 14 inline table lookups.
`Config.pm`'s builtin arity table has **174 entries in 25 distinct value
shapes** built from 11 atomic codes; `-11` and `-12` both mean "one argument
before a list" and no consumer tells them apart.  `core_arg_context` already
asks the RUNNING PERL `prototype("CORE::name")` for argument contexts -- PCL
already uses perl as a parse oracle in one place.

**`ExprToCL::gen_funcall_form` (853 lines)** dispatches on the function NAME
STRING with 27 arms for 25 builtins plus three pattern arms.
`_lower_stmt` has 15 arms, `_lower_compound` 6 kinds plus two in-line PPI
fixups.  51 `Parser2 TODO:` refusal sites, 128 `die` sites: the rule-12
discipline is real.

### 1c. "v1 is gone" is true of the file level only

`Pl/Parser.pm` is reached through `become_seam`, `capture_v1` and
`embed_block_v1`.  Statically, 262 of its 265 subs are reachable (11,000 of
13,562 lines; the three dead ones total 75 lines).  Dynamically, transpiling
`lib/Math/BigInt/Calc.pm` (2,566 lines) ran 139 of its 299 subs and sent **39
statements through the v1 TEXT path** (`_fallback_stmt` → `capture_v1` →
`Pl::CLForm::raw`): every `use`/`require`, every statement with a
`for`/`while`/`until` modifier, the BEGIN block, `use constant`.  So the
structured-emission invariant has a text hole exactly where perl's phase model
lives (`use`, BEGIN).  And Parser.pm is not a residue: about 190 `sub` definitions
appeared in its diffs in the last 90 days (additions and moves) -- it is the home of the shared pre-pass and
facts layer (`feature_regions_of`, `strict_refs_regions_of`,
`use_statements_of`, `_extract_module_prototypes`, `fragment_doc`), named as
if it were the old pipeline.  A reader cannot tell from the file name what is
load-bearing.

### 1d. Where the compile time goes

`Devel::NYTProf` on `./pl2cl < lib/Math/BigInt/Calc.pm` (3.74 s real, 17.0 s
under the profiler; perl's own `perl -c` of the same file: 0.01 s; PPI alone:
0.23 s):

| where | share of profiled time |
|---|---|
| `PPI::Node::find` -- 9,779 calls | 46 % |
| `_rename_captured_file_lexicals` → `_promote_captured` (432 calls) → `_block_captures_name` (782 calls) | 25 % |
| the whole PExpr expression parse (2,602 trees) | 10 % |
| VarAnnotator | 9 % |
| PPI's lexing -- **9 `PPI::Document` builds** for one file (the file, its re-parse, the used modules read for prototypes, interpolation fragments) | 7 % |
| `UNIVERSAL::isa` -- 7.73 million calls; `_elem_within` 214,804 calls | 7 % |

The transpile is 16x PPI's own parse and about 370x perl's compile, and the
cost is not parsing: it is **asking the tree the same scope questions over and
over**, one whole-document search per name per question.  This is the
measured form of the s379 finding ("51 % of Parser2 is variable identity").

### 1e. What is right and must survive any change

The verification harness (corpus-diff, the sweep with its five buckets, the
everyday battery, the CPAN board, 230 behaviour-asserting `Pl/t` files out of
331) is parser-independent.  `Pl::InterpScan` is ONE scanner modelled on
toke.c's `intuit_more` -- the one family already done perl's way.  The #153
FOLD (`_term_extent`) unified the postfix-chain walk.  CLForm makes
parentheses balance by construction (except the raw seam of 1c).  The layer
rule (no module names in `Pl/`) holds.  The "a missing case dies" discipline
holds.  The runtime (40,771 lines, 174 exported symbols = the IR vocabulary)
is a separate concern and is not touched by anything below.

## 2. Verdict

Sane to USE: it is tested, probed, documented and loud.  Not sane to EXTEND:
the parser has no single notion of "what comes next" (the expect state), of
"what shape does this call take" (the keyword class), or of "which declaration
is this" (the binding).  Each new corner case therefore becomes a new pass or
a new predicate beside the old ones, every pass re-walks the document, and the
re-parse loop means the token stream is not even stable within one transpile.
The growth rate (§1a) and the pass count (§1b) are the symptoms; the profile
(§1d) is the bill.  Earlier reviews saw the same shape (`v2-code-review.md`
2026-07: "hand-rolled precedence knowledge, N copies"; `var-handling-review-s379`
2026-08: "bind once"; `pexpr-term-parsing-review`: "no single notion of a
term") and the one that was executed -- the FOLD -- worked.  The binder
(Direction A) was designed in s379 and never built; its deletion potential has
grown since.

## 3. What perl's front end does, component by component

perl 5.40.3: `toke.c` 14,025 lines, `perly.y` 1,693, `pad.c` 3,002, `op.c`
15,993, `regen/keywords.pl` 384, 422 ops.  Four mechanisms carry the corner
cases:

| perl mechanism | its single state or table | what PCL has instead |
|---|---|---|
| **`PL_expect`** (toke.c): 11 states (`XOPERATOR XTERM XREF XSTATE XBLOCK XATTRBLOCK XATTRTERM XTERMBLOCK XBLOCKTERM XPOSTDEREF XTERMORDORDOR`), assigned at 80 sites, read by 48 `yyl_*` character handlers.  After a term expect an operator; after an operator expect a term; `/ { < % & * ? << -X` are lexed by that state alone. | one variable | PPI's previous-significant-token heuristic, which is the same idea without the state (hence the 38 logged deviations), plus ~35 PCL repair passes, 18 of which re-parse the document |
| **keyword classes** (toke.c's keyword switch, `regen/keywords.pl`): each builtin is `UNI` (72 uses; named unary), `LOP` (89; list operator), `FUN0` (25), `FUN1`, `UNIDOR`, `UNIBRACK`, `PREBLOCK` (12), ..., and `perly.y` has ONE rule per class: `UNIOP term`, `LSTOP optlistexpr`, `FUNC '(' optexpr ')'`, `LSTOPSUB startanonsub block optlistexpr`.  A user sub's prototype maps it to the same classes (`UNIOPSUB`, `LSTOPSUB`). | about ten classes, 38 `%token` lines | `Config.pm`: 174 entries / 25 value shapes; 34 predicates; `handle_subcalls`' two loops; `is_named_unary` reads the prototype (s438b) -- the perl rule, for one class |
| **precedence** (`perly.y` lines 123-155): 31 `%left/%right/%nonassoc` lines; `UNIOP` sits between relational and shift, `LSTOP` below the comma. | one table | `Config::%precedences` 98 operators / 21 levels -- **matches perl; keep** |
| **the term grammar** (`perly.y`): `termbinop`, `termunop`, `subscripted`, `listop`, `anonymous`: 248 alternatives for the whole language, LALR(1). | one grammar | PExpr's precedence climber -- fine for operators; the OPERAND side was the maze, half-fixed by the FOLD |
| **pads** (`pad.c`): a lexical is `(name, COP_SEQ_RANGE_LOW exclusive, COP_SEQ_RANGE_HIGH inclusive)`; `intro_my` makes a `my` visible from the NEXT statement (so `my $x = $x + 1` reads the outer `$x`); `pad_findmy` walks the current pad then outward through `CvOUTSIDE`, and a hit outside creates an OUTER entry in the inner pad -- that entry IS the closure capture; `pad_leavemy` closes the ranges at block end.  Eight functions. | one table | 18 scope-walk implementations, six rename reasons (`:spanning :captured :state-cell :exception-global :qualified-global :seam-shadow`), 39 references to `_file_lex_renamed`, VarAnnotator keyed on token TEXT (so every rename must precede it) |
| **context and lvalue propagation** (`op.c`): `scalar()`/`list()`/`op_contextualize` set `OPf_WANT` on every op at compile time; `op_lvalue` sets `OPf_MOD`/`OPpLVAL_INTRO`; `OPpTARGET_MY`; constant folding. | flags on the tree | the three-valued wantarray protocol + VarAnnotator's facts + the Kind-A verdicts -- correct, but derived from text |

Two honest caveats.  First, toke.c is itself the source of the "weird corner
cases": `intuit_more`'s scoring of `[` inside a string, `XTERMORDORDOR` (the
file calls it an evil hack), `S_lop`'s "if we have a next token", `checkcomma`,
`yyl_leftcurly`'s block-vs-hash lookahead.  Porting it does not make the cases
fewer; it puts them in one switch.  Second, perl parses by RUNNING (`BEGIN`,
`use`, prototypes installed by code) -- `docs/static-parse-limits-review-s512.md`
has the seven families a static parse cannot know.  Any design that keeps a
static front end keeps those families; only Option 3 below removes them.

The three probes behind §4 are reproducible in seconds:

```
# perl resolves every ambiguity PCL repairs, incl. positional prototypes:
printf 'sub e {7} my $f="x"; my $fh=\*STDOUT; print $fh -e $f; print $fh - e $f;' | perl -MO=Deparse,-p
#   print($fh (-e $f));  print(($fh - e($f)));
# the pad IS the binding table (scratch padprobe.pl, main_cv->PADLIST):
#   main  pad[2] $x  seq 708..732      main  pad[3] $x  seq 710..711
#   inner pad[2] $x  OUTER(captured from pad[2])
# an eval string compiles WITHOUT running, outer lexicals supplied as a pad:
#   pad of the inner sub: $x(outer) @list(outer)
```

## 4. The options

### Option 0 -- status quo plus the standing consolidations

Keep adding one-predicate campaigns (s438's `is_named_unary`, InterpScan).
Each PPI bug costs a pass; the pass count and the 46 % `find` share stay.
Not recommended as a plan, but it is what happens by default.

### Option 1 -- perl as the MODEL: three states copied into the present shape

Three independent, byte-diff-gated moves.  Each deletes code; none changes
the emission, the runtime, PPI or the tests.

**1a. ONE expect-state repair pass.**  Walk PPI's significant tokens once,
left to right, carrying `expect` exactly as toke.c does (XSTATE at a statement
start, XTERM after an operator, XOPERATOR after a term, XBLOCK after `{` in
statement position, XREF after a list-operator keyword that takes a
filehandle).  A token whose PPI class contradicts the state is re-lexed from
its own text (PPI::Tokenizer on the span -- the `fragment_doc` pattern already
in use) and re-tagged; `{` in XSTATE/XBLOCK takes `yyl_leftcurly`'s lookahead
(word or string followed by `,`/`=>` is an anon hash).  Deletes the ~35 repair
passes and **all 18 re-serialize-and-reparse sites**; the 38 canary rows
(`misc-fixes-02.t`, `ppi-bug-report.t`) stay as the bar.  Size: one design
session (the table of "PPI class × expect state → correct class" is read off
`yyl_*`) and 2-3 execution sessions.

**1b. ONE keyword-class table and ONE argument-extent rule per class.**  The
class of a builtin is derivable: `prototype("CORE::name")` (which PCL already
calls) gives `_`/`$` = UNI, a slurpy = LOP, `()` = FUN0; the ~30 syntax keywords
with no prototype (`print sort map grep eval do require defined exists delete
split system exec return goto my ...`) are a hand table, as in toke.c.  A user
sub gets the class of its prototype's shape (the s438b rule, generalised).
Then the extent rule is perly.y's: FUNC takes its parentheses; UNIOP takes one
term up to the comparison level; LSTOP takes a list up to the next same-level
`or`/`and`/`xor`, `)` or statement end; FUN0 takes nothing.  The filehandle
slot (XREF) and the indirect object stay special, as they are in perl.
Deletes most of `handle_subcalls`' right-to-left loop, `_listop_arg_ceiling`,
the 25 value shapes, the inline bypasses.  Bar: `handle_subcalls` 1,811 →
under 600 lines, corpus-diff identical.  2-3 sessions.

**1c. ONE binding table = pad.c in Perl** (s379's Direction A with pad.c's
exact rule).  One early walk assigns every `my`/`our`/`state`/signature
parameter a binding `(canon, scope node, visible-from = the statement AFTER
the declaration, visible-to = scope end)` and resolves every use (code and
interpolated, via InterpScan) to the innermost binding whose range holds its
statement; a use inside a sub body whose binding lies outside is a capture
(OUTER).  The 18 walks become queries; `_block_captures_name`,
`_rename_decl_within` and the rename families become metadata read at
emission; VarAnnotator keys on binding ids; the 46 % goes away.  The s379
estimate was one session for the table in dual-run mode plus 3-5 for the ports
plus the emission step; the machinery has grown since, so 5-7 sessions.  Bar:
scope-walk implementations 18 → 1, Calc.pm transpile 3.7 s → under 1.5 s,
corpus-diff identical at every port.

**Also, independently of the three**: rename `Pl/Parser.pm`'s live halves to
what they are (a `Pl/PrePass.pm` and a `Pl/Facts.pm`), and give the four
statement kinds of §1c a structured lowering so the text seam closes.

Option 1 total: 12-18 sessions.  Bars in numbers: passes 69 → 30 or fewer,
re-parse sites 18 → 0, repair passes ~35 → 1, scope walks 18 → 1, Calc.pm
under 1.5 s.  The three states are also the three places a future corner case
goes, so growth changes kind: a new `case`, not a new pass.

### Option 2 -- perl as the SOURCE: an own toke.c-shaped lexer replacing PPI

A Perl port of toke.c's essentials (expect state, keyword switch, quote and
heredoc scanning, numeric literals) at 6-8k lines plus a Pratt or LALR term
parser from perly.y's 248 alternatives (~2k lines).  It deletes PPI, its 38
bugs, every repair, PExpr's pre-scan and `handle_subcalls`.  But Parser2's
15,728 lines consume a PPI tree; a new tree re-targets the lowering, which is
the rewrite the s316t architecture doc priced at 60+ sessions with a
correctness trough.  Realistic: 25-40 sessions.  Options 1a+1b get most of the
benefit inside the existing shape; Option 3 gets all of it for a similar
price.  Not recommended as the first move.

### Option 3 -- perl as the COMPONENT: translate the optree

Let the oracle perl compile the program (`-c` style: BEGIN and `use` run,
the main program does not) and translate its optree to CLForm through `B`.
This is the literal "exact implementation" route, and the probes show what it
settles by construction:

- term-vs-operator for every character, prototypes including `()`,
  POSITIONAL prototypes (#2871's whole family), the indirect object, stacked
  filetests, block filehandles, `sort`'s comparator kinds -- the §1b repair
  block and `handle_subcalls` have nothing left to decide;
- string interpolation arrives as `concat`/`multiconcat` with the constant
  parts separated -- InterpScan's job is done by the oracle;
- lexical identity arrives as the pad (name, ranges, OUTER captures) -- Option
  1c for free, with perl's own shadowing and closure rules;
- every op carries its context (`v`/`s`/`l`), `LVINTRO` (`my`/`local`),
  `TARGMY`, `OPpDEREF` autovivification flags; every statement carries its
  line, package, and `strict`/`feature`/`warnings` hints (`nextstate`
  `v:*,&,{,x*,x&,x$,$`) -- the facts the wantarray protocol, VarAnnotator and
  #221 derive from text;
- BEGIN and `use` have RUN in the compiling perl, so subs installed by code,
  imported names, `*CORE::GLOBAL::` overrides, source filters, `use bigint`
  constants (families A-F of `static-parse-limits-review-s512.md`) are
  resolved; the #2610 detector becomes a RUN-time question only, as in perl;
  `B::save_BEGINs` keeps the BEGIN bodies for emission (probed: present);
- a string eval compiles without running, its outer lexicals supplied as a
  synthetic pad whose OUTER entries map onto today's capture alist (probed);
  the runtime already talks to a perl process (`pl2cl --server`), so no new
  run-time dependency.

**Size.**  5.40 has 422 ops; `B::Deparse` -- optree to TEXT, the harder
direction -- is 7,282 lines.  An optree-to-CLForm emitter onto the existing
runtime vocabulary is of that order, 8-12k lines including the pad-to-`let`/
cell mapping, the phase assembly (BEGIN/END/`use` into eval-when), the module
boundary, the eval seam, regex pass-through, formats, the wantarray protocol,
and the four Kind-A optimisations restated on op facts.  It replaces
Parser.pm, Parser2, PExpr (+Config, TokenUtils, StringInterpolation),
VarAnnotator, InterpScan, ProtoCache, BlockAnalyzer, GlobalPartition: about
43k of the 54k lines.  ExprToCL's emission vocabulary survives in part; CLForm,
Passes, Manifest, DataForm and the whole runtime survive whole.  Transpile
time becomes linear in ops on top of perl's 10 ms compile.

**Costs and risks, honestly.**

1. **BEGIN and `use` execute at transpile time.**  Today PCL does NOT run user
   code while transpiling; BEGIN is emitted as `eval-when`.  Under Option 3
   the compiling perl runs them (as `perl -c`, `B::Deparse`, `Devel::Cover`
   and every CPAN smoker do) and the generated program runs them again at
   load.  Harmless for state (two processes), visible for side effects (a
   BEGIN that prints or writes).  **A USER decision** -- and a measurable one
   (§5 S3 counts the hazards in the four populations).
2. **The compiled-code CACHE freezes the compile-time environment** (the
   USER's question, s525).  Today a cached unit is valid by CONTENT alone:
   source bytes + generation + the dependency manifest of the modules whose
   prototypes and exports it read (#1261), and BEGIN and `use constant` bodies
   run at every LOAD, so no unit depends on the environment of the run that
   built it.  Under Option 3 the compiling perl evaluates them once and FOLDS
   on the result -- probed with `B::Concise`: `use constant D => $ENV{D};
   print "x" if D;` arrives as an unconditional `print` when D was set and
   with the `print` ABSENT when it was not; `$^P`'s NOOPT flag does not
   prevent it.  A cached translation would therefore bake %ENV, @ARGV, `$^O`,
   files probed by `-e` or `eval { require }`, and the clock into the unit,
   where perl re-evaluates them on every run.  Mitigations, each with a cost:
   record the compile-time INPUTS into the manifest (a tied %ENV, %INC and
   @INC hooks, @ARGV, the stat/open calls) and key on them -- the build-system
   "depfile" model, covering the common idioms; mark a unit whose compile read
   an unrecordable input (time, rand, pid, a socket) UNCACHEABLE and run it
   from text (the #2702 path, slower every run); or accept the perlcc / B::C
   precedent (BEGIN effects frozen in the binary, a documented limitation).
   The idiom is rare in PCL's populations (0 `use constant`-from-environment
   in `lib/` and `perl-tests/`, 7 BEGIN blocks reading %ENV/@ARGV in everyday
   + the board) and classic in the wild (`use constant DEBUG => $ENV{...}`).
   **This is the strongest argument against Option 3**, and the spike's S3 is
   widened to a census of compile-time INPUTS, not only side effects.
3. **Perl version coupling** (the USER's question, s525; measured on the local
   sources).  The op set went 396 (5.26.3) → 400 (5.32.1) → 422 (5.40.3): +4
   (`multiconcat`, `isa`, `cmpchain_and`, `cmpchain_dup`) then +22 (`padsv_store`,
   `aelemfastlex_store`, `helemexistsor`, `emptyavhv`, the try/catch/defer ops,
   the class ops, the `builtin::` ops) and NONE removed -- about two new ops a
   year, each needing one emitter arm.  The optree is an internal format with
   no stability promise, and core `B::Deparse` now supports only ITS OWN perl
   (zero `$]` conditionals in 5.40's 7,282 lines), so the maintenance model is
   a per-version op table where an unknown op DIES by rule 12 -- a perl newer
   than the emitter is refused loudly, never mis-translated; a perl older than
   the oracle (the install matrix runs 22.04 = 5.34, 24.04 = 5.38) needs its
   missing ops expressed in older shapes (`padsv` + `sassign` for
   `padsv_store`), as Devel::Cover and Devel::NYTProf do.  Today's design pays
   a comparable tax in a different place: a new perl syntax waits for a PPI
   release, then a repair, then a lowering.  **A USER decision** under the
   portability rule.
4. **Modules load in the compiling perl.**  `use POSIX` loads the REAL POSIX
   there (only the parse facts matter); the runtime still loads the shim.  A
   shim whose prototypes differ from the real module's would mis-parse call
   sites -- the inverse of #2610, and checkable mechanically (shim vs
   `prototype(\&Real::f)`).
5. **The emitted CL changes shape** (flat, op-shaped, constants folded:
   `pi / 2 + pi / 4` arrives as a number).  Speed wins over readability by
   ruling; Target B (#75, clear IR) would be restated as "clear optree-to-CL".
   The 70 `Pl/t` files that assert emitted SHAPES need re-expression; the 230
   behaviour files, the sweep, everyday and the board do not.
6. **The trough.**  Parity with today's 18,759 sweep rows is reached by
   rebuilding the emission half from zero against the same oracles.  The s316t
   argument against a greenfield v3 (it re-derives parse knowledge) does NOT
   apply to the front half -- perl supplies it -- but applies to the emission
   half.  Estimate 40-60 sessions to parity, ±50 %.
7. **What stays wrong.**  Every RUN-time divergence (numeric semantics such as
   #3692, string ops, the regex engine, I/O) is untouched: the optree fixes
   the parse, not the runtime.

What it ends with: a compiler about a fifth of today's size, zero parse
divergence from perl by construction, the seven static-parse families gone,
transpiles in milliseconds, and a front end that tracks perl releases (about
two new ops a year) instead of PPI's -- at the price of risk 2, the cache.

## 5. Recommendation and the decisions it needs

**RULED (USER, 2026-10-24, the same session): Option 3 is OFF.**  Requiring
an exact perl version at transpile time, or carrying one parser per perl
version, is unrealistic; together with risk 2 (the cache) that closes the
optree route.  The spike is cancelled and #3694 is re-pointed at Option 1.
The paragraphs below this box are kept as the record of what was weighed; the
live recommendation is this box.

**Recommendation after the ruling: start now, in ONE slot per round, checkers
first -- do not push it to "later".**
- *Why not later.*  The code Option 1 replaces is the fastest-growing part of
  the tree (Parser2 +12,012 lines in 90 days, Parser.pm +7,367); every corner
  case landed in the present shape is a line the migration must carry; a PPI
  bug costs a pass, probes and a guard today and one table row after 1a; the
  3.7 s module transpile is paid on every cold board run.  Deferral compounds.
- *Why not a pause of everything else.*  The correctness-first ruling and the
  two-subjob rule stand.  The structural work takes ONE of the two slots per
  round (the shape the perf slot has), 12-18 sessions over as many rounds; the
  board and residue batches keep the other slot.
- *Order, revised from §4's 1c-first.*  **(0) Two INERT checkers, one session
  each, zero emission change**: the expect-state walk as a CHECKER that
  reports every token where its verdict differs from PPI-plus-repairs over the
  four populations (does one state really cover the ~35 repairs?); the binder
  in DUAL-RUN asserting agreement with `_binding_at`, `_ref_shadowed` and
  `_block_captures_name` at every call (s379 step 1).  Every disagreement is a
  live bug or a design error, found before any port.  **(1) 1a** -- the token
  stream must be stable before anything binds on it; the smallest blast radius
  (the pre-pass layer); the 38 canaries + corpus-diff as the bar; it stops the
  PPI-bug bleeding.  *(s527a, 2026-10-25: for the TOKEN rows (1) is now PPI's own fixed
  tokenizer -- PCL's `pcl` branch, vendored as `vendor/PPI` (`docs/ppi-fix-at-source-s527.md`
  §8); the expect-state checker remains as the detector of new PPI bugs.)*  **(2) 1c** ports, newest consumer first (s379's recipe),
  the sweep as the gate.  **(3) 1b.**  The two fillers (rename Parser.pm's
  live halves; close the four-kind text seam) ride where they fit.
- *Standing rule, effective now* (s379 §8 restated): no new scope walk, no
  new rename suffix family; once the expect-state checker exists, no new repair
  pass -- a PPI mis-lex becomes a row in its table, and until then a new bug
  still gets its catalogue section, report row and canary in the same commit.
- *Bars in numbers* (unchanged): passes 69 → 30 or fewer, re-parse sites 18 →
  0, repair passes ~35 → 1, scope-walk implementations 18 → 1,
  `handle_subcalls` 1,811 → under 600 lines, Calc.pm 3.7 s → under 1.5 s,
  corpus-diff IDENTICAL at every step, sweep TOTAL +0.

---

**Do not start Option 1c before Option 3 is decided**: the binding table is
the one piece the optree makes free, and the s411 rule says a fix in code a
phase deletes is wasted.  **Do not decide Option 3 without its measurement**:
the s335 rule says a "suspect X" carries the cheap discriminating measurement.
So:

**Step 1 -- the Option 3 spike, 2-3 Opus sessions, no production code.**
- **S1 op census**: a `B` walker over the four populations (sweep, companion,
  everyday, board) under 5.40.3 -- distinct ops, frequency, cumulative
  coverage ("N ops cover 95 % of occurrences"), the long tail by name.
- **S2 version delta**: the same census under the local 5.32.1 (and 5.26.3)
  perlbrew builds -- which ops differ, how many conditionals an emitter
  carries per supported floor.
- **S3 compile-time INPUT and side-effect census**: BEGIN bodies, `use
  constant` values and import-time code in the four populations that READ the
  environment (%ENV, @ARGV, `$^O`, files, `eval { require }` probes, the clock)
  or do anything but `use`, `@INC`, `$|`, `plan`, `chdir 't'` -- the count and
  the kinds, so decision 1 below and the cache design (§4 Option 3, risk 2)
  are taken on numbers.
- **S4 vertical slice**: emit CLForm for the ~40 commonest ops onto the
  existing runtime and run the everyday programs that fit end to end; this
  proves the op-to-runtime-vocabulary fit and calibrates lines per op.
- **S5 eval seam**: compile one eval string with a synthetic pad and map its
  OUTER entries to the capture alist; one probe file, five shapes.
- Bar: a table the USER can decide from, plus the recalibrated session
  estimate for Option 3.

**Step 2 -- decide.**  If Option 3 is taken: it becomes the next structural
program (the runtime review runs beside it, untouched).  If not: Option 1 in
the order **1c → 1a → 1b** (biggest win first; each byte-diff gated; the
Parser.pm renaming and the §1c seam closure as fillers), 12-18 sessions.

**Decisions this review puts to the USER**
1. Is user code (BEGIN, `use` imports) running at TRANSPILE time acceptable,
   given S3's count -- and with it the cache consequence (§4 Option 3, risk
   2): a cached unit that froze its compile-time environment, unless the
   compile's inputs are recorded into the manifest?  (Option 3's
   precondition.)
2. The perl floor at transpile time: support 5.34+ in the emitter, or require
   the oracle perl?  (Portability rule.)
3. Spike first (recommended) or Option 1 now?
4. The runtime review: independent of this choice; start it when convenient.

## 6. Residues filed from this review (no code changed)

- Three stale `ppi-upstream-bugs.md` section citations in repair comments
  (§1b) -- comment-only edits, one filler.
- `Config.pm`'s `-11`/`-12` codes mean the same thing and no consumer
  distinguishes them -- fold on the next touch of the table (dies under 1b).
- `Pl/Parser.pm` is the shared pre-pass/facts layer under the name of the
  deleted pipeline; 39 statements per module still lower through its text
  seam (`use`/`require`, loop-modifier statements, BEGIN, `use constant`).
- `_ppi_parse` builds nine PPI documents for one 2,566-line module; the
  re-serialize-and-reparse loop (18 sites, two bounded at 20 rounds) is the
  single largest source of compile-time instability.

## 7. The PPI-bug REGISTRY (USER requirement, 2026-10-24: "easy to add to, and remove from if some would get fixed")

Today one PPI bug is written in FOUR hand-synchronised places: its catalogue
section, its repair pass, its canary row, its report row -- and they have
drifted (the report's "Bug N" is not the catalogue's §N; §5 and §32-§38 have no
report row; three repair comments cite the wrong §).  The registry makes it ONE
place, on the pattern this project already trusts for the same problem shape:
`cl/skip-registry.lisp` (`docs/test-skip-registry.md`) -- declarative rows,
keyed, with a STALE DETECTOR that fires when reality changes.

**One row per bug** (Perl data, e.g. `Pl/PPIBugs.pm`):

```perl
{ id    => 28,                       # = the catalogue's §; never renumbered
  title => '`$x.2` -- a `.` before a digit after a term is lexed as a Float',
  kind  => 'token',                  # token | structure | source | lexer-dies
  ppi   => { confirmed => '1.291', fixed_in => undef },
  when  => { expect => 'XOPERATOR', class => 'PPI::Token::Number::Float',
             content => qr/^\.\d/ },
  fix   => 'relex',                  # relex the span | retag => CLASS | handler => \&sub
  repro => 'my $x = 1; print $x.2;', perl => '12',
  tokens => { got => [...], want => [...] },      # the report's and the canary's data
  unblocks => ['perl-tests/op/concat.t'], tasks => ['#480'] }
```

**Four consumers, all mechanical, none hand-written per bug:**
1. **The engine** -- the ONE expect-state pass of §4 Option 1a.  A `token` row
   matches on (expect state, PPI class, content) and applies its `fix`; a
   `structure` row matches a PPI tree shape and re-structures (block-vs-hash,
   `finally`, `sub _`); a `source` row is a pre-tokenizer text fix (hex floats,
   `$)` under `signatures`, the `$/`-dependent `__END__`); a `lexer-dies` row
   is a source rewrite tried only when `PPI::Document->new` fails (the three
   `for`-loop-variable shapes), so it costs nothing on a healthy file.  Rough
   split of the 38 by the catalogue's titles: token ≈ 21, structure ≈ 7,
   source ≈ 6, lexer-dies ≈ 3, fixed 1 -- the checker session verifies it.
2. **The canary test** iterates the rows and asserts PPI's `got` on `repro`.
   The day a PPI upgrade fixes a bug, that row FAILS with "PPI N fixed §id:
   set fixed_in" -- the stale detector.  Nobody has to remember.
3. **The upstream report** iterates the same rows (`want` vs `got`, the
   control from `perl`): one file, always complete, numbered by `id`.
4. **The catalogue** keeps the WHY (dumps, history, the perl probe) and links
   by `id`; its list table can be generated from `title` and `ppi`.

**Add** = one row plus its prose.  **Remove** = set `fixed_in`: the engine
skips the row for PPI ≥ `fixed_in`, the canary expects the fixed behaviour
there, the report marks it fixed; when the installer's PPI floor passes
`fixed_in`, the row is deleted.  **Report upstream** = run the report file.

## 8. The endgame -- what the parser is when Option 1 is done

The `docs/v2-target-architecture.md` §1 pipeline, with the three states named
and each the ONE place its kind of corner case goes:

```
 Perl source text
        │  source-kind registry rows (pre-tokenizer fixes; lexer-dies fallbacks)
        ▼
 PPI tokens + structure        PPI KEPT: quotes, heredocs, regex bodies, numbers, the tree
        │  ONE expect-state pass, driven by the registry      <- a LEXER corner case = a row
        ▼
 a token stream toke.c would agree with
        │  ONE binding pass (the pad model: name, visible-from, visible-to, OUTER)
        ▼                                                     <- a SCOPING corner case = the one rule
 tokens + binding table
        │  classify (Stmt records); expression parse = ONE keyword-class table
        │  + ONE extent rule per class + the precedence table  <- a CALL-SHAPE corner case = a class
        ▼
 expression trees + Facts (keyed on binding ids, not on text)
        │  lower; names chosen at emission from the binding table
        ▼
 CLForm  →  registered passes  →  print
```

**What exists at the end:** three small tables (the registry; the class table,
about ten classes plus some thirty syntax keywords; the precedence table,
unchanged), one binder, one expression parser whose operand side is
`_term_extent` plus the class rule, the lowering, the emitter, the runtime.

**What is gone:** the ~35 repair passes and the 18 re-parse sites; the 18
scope walks, the six rename families and every source-mutating rename
(`_rename_*`, `_rewrite_var_uses`, `_interp_fixer`); `handle_subcalls`' two
loops; Config's 25 arity shapes; the v1 text seam (the four statement kinds
lowered structurally); Parser.pm as a name (its live halves become PrePass and
Facts, its v1 statement processors are deleted).  Estimated deletion 15-20k of
the 54k lines (v1 statement processors ~6k, identity machinery ~4-5k, repairs
~3k, `handle_subcalls` ~1.2k, the rest in shapes and seams) -- an estimate the
two checker sessions calibrate.

**What stays as it is, by design:** PPI as the tokenizer (good at the
quote-like constructs; its bugs isolated in one registry with a stale
detector); the static-parse limits of `static-parse-limits-review-s512.md`
(BEGIN-time facts remain approximations, the #2610 run-time detector remains
the safety net -- the ruling against Option 3 is the ruling to live with this);
the emission vocabulary; the runtime; every test.

**What "done" means day to day:** a new PPI mis-lex is a registry row (report
row and canary by construction, same data); a new perl syntax is a PPI release
or a registry row plus one lowering arm; a scoping question is answered by the
binder's one rule; a call-shape question by a class.  Transpile time ≈ PPI's
0.23 s plus linear passes, sub-second per module.  The parser becomes boring,
which is the goal; the remaining parse divergences are the s512 families, and
they are tracked by the detector, not by the parser.  Bars: passes ≤ 30,
re-parse sites 0, scope walks 1, repairs = 1 engine + N rows, `handle_subcalls`
< 600 lines, Calc.pm < 1.5 s, corpus-diff identical per step, sweep +0.

## 9. Next review: the runtime (the USER's question B, measured in passing)

`cl/pcl-runtime.lisp` is 40,771 lines: 1,618 `defun`, 208 `defmacro`, 322
`defvar`; **41 functions over 80 lines and 7 over 200** against rule 10's
~80-line limit (`p-list-=` 446, `p-method-call` 346, `p-setf` 340,
`sprintf-one` 305, `p-array-fill` 235, `p-sprintf` 227, `%p-clock-resolution`
210); 90-day churn +36,292 / -9,852 in 730 commits -- MORE growth than the
whole compiler side.  A review of the same kind is warranted, with perl's own
implementation as the reference again: `sv.c` (the SV model: IOK/NOK/POK and
the UTF8 flag, upgrade rules, `sv_2nv`/`grok_number` stringify-numify) against
PCL's box / raw-slot / undef / string-number model -- the "is it one mechanism"
question that the pad was for the parser; `pp.c` / `pp_hot.c` (one `pp_`
function per op) against the `p-*` family for per-op divergence (#3692's
int-vs-double compare is the archetype); `mg.c` against tie and the special
variables; the duplication census (`tools/dup-census.pl`, s411 scope) re-run;
the rule-10 / rule-12 audit (#152).  One Fable session, no agent slot.

Reading list for whoever executes: `docs/var-handling-review-s379.md` §3 (the
binder), `docs/pexpr-term-parsing-review.md` (the operand grammar),
`docs/static-parse-limits-review-s512.md` (what only running code knows),
`docs/v2-target-architecture.md` §1 and §5 (the incremental-path argument this
review re-weighs), perl's `perly.y` lines 123-155 and 1095-1400, `toke.c`
`yyl_leftcurly` / `yyl_slash` / `S_lop` / `S_intuit_more`, `pad.c`'s header
comment and `pad_findlex`.
