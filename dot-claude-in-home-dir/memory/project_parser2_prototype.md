---
name: project-parser2-prototype
description: "v2 compiler pipeline prototype (4c8ee41) — Pl::Parser2/ExprToCL2/VarAnnotator/CLForm, PCL_V2=1 switch, parallel to v1"
metadata: 
  node_type: memory
  type: project
  originSessionId: 657169b8-1488-43d3-ad7f-5e5daa0e21a9
  modified: 2026-07-18T23:32:04.480Z
---

**The compiler-rewrite prototype exists and runs** (2026-07-02, `4c8ee41`):
`PCL_V2=1 ./pl2cl` / `PCL_V2=1 ./runpcl` selects it; v1 default untouched.
Doc: `docs/parser2-prototype.md`. Guard: `Pl/t/parser2-01.t` (13 tests, in gate).

Files (parallel to originals, same APIs):
- `Pl/CLForm.pm` — forms `[head,@args]`/`['list',…]`/`raw($cl)` + the ONLY
  printer (parens/indent by construction). `raw` must NOT be re-indented
  (corrupts multiline string literals — was a bug).
- `Pl/Parser2.pm` — statement lowering; `my` nests rest-of-block in a `let`
  FORM (R3); real lambda lists `(&optional ($n (p-undef)) &rest %_args)` for
  `my(LIST)=@_` subs (spec #3); no VOID wantarray wraps (#2); no dead boxes (#1).
- `Pl/ExprToCL2.pm` — STRICT native subset (scalars/decimal nums/binary arith),
  all-or-nothing per expression, else undef.
- `Pl/VarAnnotator.pm` — conservative Gate-1: unboxable ⇒ raw `let` + `setf`;
  requires every write arith-shaped (raw slots never hold a box → no aliasing).

**Fallback seam**: unsupported exprs → embedded real `Pl::Parser`
(`fallback_parser->_parse_expression`) → `raw` leaf. GOTCHAS learned:
(1) old pipeline converts `p-scalar-=`→`p-my-=` via regex INSIDE `_emit`;
fallback bypasses `_emit` → Parser2 applies same rewrite at the raw boundary
+ lowers plain assignments natively as p-my-=/setf. Keep
`fallback_parser->{_let_bound_vars}` updated as scopes open.
(2) foreach list must pass ctx=1 to `_parse_expression` or `2..$n` becomes
scalar flip-flop.
(3) `p-sub` passes lambda-list through verbatim — `&optional`+`&rest %_args`
+ `p-args-body` works (extra args land in @_, unused by gate).

Verified perl-identical: recursive fib, loop fib, intmath. Speed ≈ v1 still —
shapes only; gains arrive with R1 (inline ops + FPU modes at startup, see
[[project-codegen-rewrite-review]] / `where-the-time-goes.md`).

**NATIVE FUNCALLS + R2 CALLER HALF DONE (9703bac)**: Parser2 pre-pass →
`sub_info{name}={cl_name,insensitive}`; ExprToCL2 lowers known-sub calls w/
native args to direct `(pl-f …)`, `*wantarray*` bind only for sensitive
callees. Insensitive = no wantarray + every return scalar-ROOTED (arith/cmp/
`.` root, $scalar, number, string). GOTCHA: `&&`/`||`/`//` are context-
transparent to their RIGHT operand and `x` repeats lists → NOT scalar roots;
bare `return;` = ()-vs-undef → sensitive. fib compiles to
`(p-+ (pl-fib (p-- $n 1)) (pl-fib (p-- $n 2)))`; fib(29) compute 0.72→0.51s
(~30%; perl 0.141). Remaining call cost = p-sub's 5 per-call special binds +
p-flatten-args + generic ops.

**SESSION 268 (2026-07-02) GROWTH DONE** ([[project-r1-runtime-fast-paths-done]],
`303dcde`+follow-up): native strings/`.`/str-cmps (raw-slot rule = "raw CL
VALUE" incl. strings); `my $x = f()+1` unboxes (top-level op coerces call
result; bare `f()` boxed; ops inside call args don't count as top-level —
VarAnnotator `_scan` is index-based for Word+List funcall pairs);
elsif→nested p-if; C-style for (raw counter iff step arithmetic, boxed under
`++`); goto/next/last/redo via fallback (goto keeps p-args-body — forwards
live @_); lean p-sub (constants hoisted; p-args-body skipped when @_ unused).
fib(29) v2 0.078s vs perl 0.138 — CALLS BEAT PERL. Guards parser2-01.t 32.

**SESSION 269b (2026-07-04) COVERAGE+CORRECTNESS DONE:** non-scalar `my`
(containers in let + v1-expr assignment; self-ref init → TODO); statement
fallback seam `_fallback_stmt` (v1 _process_element → scratch section; decls
hoisted via `_captured_decls`, runtime raw in place; handles use/require/
BEGIN/__END__ — **GOTCHA: Statement::Scheduled ISA Statement::Sub, exclude
it**); pl2cl `parse_with_fallback` = whole-file v1 fallback on any TODO die
(makes PCL_V2=1 safe for module subprocesses); gates → v1: string eval
(lexicals invisible to eval'd code), prototyped subs. Pre-pass now registers
subs in shared Environment (add_declared_sub + add_prototype min_params=-1)
else bareword `foo` lowers as STRING. Context-correct native calls: gen_form
ctx = nil/t/:void/'inherit' (arg=t, statement=:void, return+sub-tail=inherit
→ NO bind), $tail_ctx threaded through blocks + tail-if branches.
`_forward_global_decls` = v2 forward-decl pass (defvar referenced-never-
let-bound + cross-pkg like main::$IS_ASCII; NEVER defvar let-bound names —
would poison lexicals). Native: simple-interp strings → p-string-concat
(strict escape set), unary `!`, C-for pure-`++`-step carve-out (re-analyze
region minus step → raw counter + setf). 9 v2-lowered perl-tests at FULL v1
parity; 11/40 files lower via v2, rest fall back (previously died).
Guards parser2-01.t = 43.

**SESSION 270 (2026-07-04) `package` DONE (section splitting):** top-level
statement-form `package Foo;` splits the file into segments → one output
SECTION each, v1 preamble shape (p-defpackage/in-package/defclass plc-*/
p-register-pkg-name/per-pkg $a $b), p-set-current-package in runtime order,
top-of-file `(pcl:p-defpackage …)` predecls (qualified syms must be READABLE
— load evals form-by-form). sub_info now PER-PACKAGE (`_cur_sub_info`);
cl_names stay unqualified (section reader pkg interns them). Forward-decl
pass per-section. Die→v1 gates: block-form/versioned pkg, my-lexical
spanning a pkg boundary (`_check_my_spanning`; our/local exempt), **file
lexical captured by a NAMED sub** (`_check_sub_captures` — named subs hoist
OUTSIDE the nested lets; qq.t's `my $test; sub is {$test++}` compiled a free
symbol; anon subs fine, they lower in place).
**TWO BIG DISCOVERIES:** (1) **PCL_V2=1 never reached v2 in ANY runner** —
runpcl/runt/clt/sweep all pass `--lenient-ppi` which parse_with_fallback
treated as special-mode→v1; earlier "parity" sweeps silently measured v1.
Fixed in pl2cl (flag ignored for the v2 attempt — only matters when PPI
can't parse, and Parser2 dies→v1 then anyway). (2) **Octal leak**: ExprToCL2
number gate accepted `0100` → CL reads 100, perl reads 64; leading-zero ints
now fall back. **OPEN v1 BUG found:** `my $g; package Foo; print $g;` → v1
defvars $g under :main, :Foo section reads Foo::$g → unbound crash (v1 needs
to qualify my-var refs by declaring section's pkg). Verified: 12 perl-tests
lower via v2 at FULL parity 428/428 (chars cond context defined dor if num
qq sleep translate warn while). Guards parser2-01.t = 54 (incl pkg e2e).

**s270b: `our` DONE (f022dd3)** — `_lower_our_decl`: defvar hoisted to
section top via `_captured_decls` (no let), INIT = plain assignment through
ordinary machinery; shadowing a my-lexical dies→v1. Alias visibility across
a later `package` NOT modelled (v1 same). PERL GOTCHA (cost 20 min): a
trailing `|| (…)` after `grep {…} @names` gets slurped into grep's LIST —
parenthesize. parser2-01.t=61.

**SESSION 271 (2026-07-04) A3 DONE (899c3ba):** `local` (ALL shapes +
standalone `delete local`) lowers via `_fallback_stmt_capture` → v1's
`_process_local_declaration`; the open save/restore scope wraps the lowered
block remainder via new CLForm `raw_wrap(open,n_closes,body)` — n from v1's
own `_local_let_depth`, so balance-by-construction holds; `_fallback_stmt`
dies on unexpected opens (safety); s269 notinline OOM guard carries over
(scratch at indent 0). Non-if/unless statement modifiers (+do{}while) →
per-statement fallback at the 3 `_split_modifier` sites (modifier-written
vars stay boxed — VarAnnotator conservative, guard-tested). `for(;;)` empty
sections native (positional collection, Null=empty, cond→t).
**TWO LATENT v2 BUGS FIXED (silent wrong code, not fallback):**
(1) loop conds missed v1's `_auto_defined_cond` — `while (<FH>)` never set
`$_`/tested defined; grent.t was a FALSE-POSITIVE pass (loop processed 0
entries). Fix `_auto_defined_raw` on raw conds in while+C-for (until exempt,
matches perl); native conds can't contain each/readline/glob so raw-only is
complete. (2) `continue` blocks silently DROPPED by while/foreach branches
(infinite loop) → explicit gate → v1. **Census: 14 files fully v2-native**
(+errno_test grent pow), sweep parity EXACTLY v1 (670/671; grent t2
env-dependent both). parser2-01.t=74 (incl. paren_balance guard + local
dynamic-scope e2e). **NEXT: A2 — bare blocks = loop-once + labels (23
files), then A4 pkg block form.**

**SESSION 271b A2 DONE (0a645e0):** bare blocks = loop-once (v1's exact
shapes: `(block nil (tagbody :redo … :next))`, labeled + LAST/NEXT/REDO
catch tags) under `*package*` guard; labels → leading `:label` keys in
p-while/p-for/p-foreach (parse-loop-keys wants them FIRST); nested named
subs are package-global → pre-pass finds subs anywhere, `_lower_block`
hoists; REAL captures (static-var idiom `{my $x; sub f {$x++}}`) die→v1 via
`_live_lex` (scope-restored by `_lower_scope`/`_lower_sub`; sibling scopes
don't over-fire). **3 MORE LATENT v2 BUGS:** (3) CLForm one-line flatten of
raw `;;` comment chunks swallowed sibling forms+parens (unshift.t EOF) —
`_flat` refuses `;`-outside-string chunks; (4) `in_subroutine` never bumped
→ bare `shift` in sub read @ARGV (exp.t silent 0.0); (5) VarAnnotator
missed `($a,$b)=…` list-assign writes → raw slot DROPPED the write
(each_array '7 7') — gates: list-assign LHS, chomp/chop/undef/read args,
non-my foreach var. Plus qualified-call pkgs (PerlIO::) pre-declared via
get_undeclared_packages. **32 files v2-native at EXACT v1 parity
(1175/9/29 both pipelines).** parser2-01.t=90.

**SESSION 272 W1 DONE (package block form + versioned pkgs):** `package Foo
{ … }` and `package Foo 1.5;` now v2-native. Segment-split loop tracks
`$cur_pkg`+`%opened`; block form pushes a Foo segment + short-form RETURN
segment for the enclosing pkg (`reopen`); assembly emits full preamble only
for a pkg's FIRST section, `;;; back to package X`+`(in-package X)` for
reopens (no re-defpackage, no dup `$a`/`$b`), both keep p-set-current-package.
Version validated `/^v?\d+(?:[._]\d+)*$/` (PPI `->version` returns BLOCK text
for unversioned block form); emits eval-when `$VERSION` defvar + source-order
`(p-scalar-= …)`. `{ package Foo; … }` (pkg inside bare block) STAYS gated→v1
(concat2/hash/vec.t; reader can't switch pkg mid-form, DESTROY-GC deps). **34
files v2-native (was 32) at EXACT v1 parity 1227/9/35, 31 fully-passing,
_status.tsv byte-identical.** parser2-01.t=96. **NEXT: W2 — replace the
string-eval text-scan gate with a PPI walk (highest-exposure item).**

**SESSION 272b W2 DONE (string-eval gate → PPI walk):** replaced the text
scan `/\beval\b(?!\s*\{)/` (false-fired on eval in comments/strings/POD/hash-
keys + `eval {` split across lines) with a `$doc->find` PPI walk AFTER
`PPI::Document->new`: Word `eval` gates only for genuine string eval (not
`->eval` method, not `eval =>`/`sub eval`, not `eval { }`). **GOTCHA exposed:**
bodyless forward decls `sub foo;`/`sub u;` (exists_sub/reset.t) reached
`_lower_sub` once eval stopped gating → CRASHED on `$sub->block->schildren`
(undef). Fix: bodyless named subs emit `(p-declare-sub pl-foo)` (v1 shape), no
def; registered in Environment but NO sub_info (calls take fallback path);
prototyped bodyless gate→W4. Fixed in 3 spots (pre-pass, top-level loop,
`_lower_block` nested-sub branch). NB: `$h{eval}` bareword subscript key WOULD
gate (snext_sibling undef in subscript) but NO perl-tests file uses it, so no
subscript exclusion shipped. **40 files v2-native (was 34) at EXACT parity
1454/10/2612, 36 fully-passing, _status.tsv byte-identical.** parser2-01.t=102.
**NEXT: W3 — enable `eval EXPR` via the existing `_eval_lexical_alist` capture
seam (add `_all_lex` cumulative set; scope `_let_bound_vars` in `_lower_scope`/
`_lower_sub`; switch `_forward_global_decls` exclusion to `_all_lex`; delete
the W2 die). 53 files gate on it.**

**SESSION 272c W3 DONE (eval EXPR via capture seam):** string-eval gate
REMOVED; `eval EXPR` lowers through the expr fallback → v1 gen_funcall →
`(p-eval STR (list (cons "$x" $x) …))` alist (s250 mechanism). Made
`_let_bound_vars` SCOPED (snapshot/restore in `_lower_scope`/`_lower_sub`) for
alist scope-accuracy; added file-wide `_all_lex` accumulator for forward-decl
exclusion (kept separate so forward-decls unchanged = parity). **3 BUGS FIXED:**
(1) foreach loop var leaked into sibling eval alists → `$e0 unbound` crash
(cmpchain/infnan); scoped loop var to body — SAME EDIT had a list-vs-scalar slip
(`my $body = _lower_scope(...)` took form COUNT 1/2/6 → must be `my @body`).
(2) ExprToCL2 native attempt's `cleanup_for_parsing` DESTRUCTIVELY rewrites
shared `=>` token → `,` (set_content), defeating the fallback's fat-comma
auto-quote → `%h=(N=>1)` lowered N as `(pl-N)` call → undefined crash (tr.t);
`_lower_expr` now snapshots+restores leaf-token content around the native
attempt. **2 BENIGN DIVERGENCES PROVEN:** sprintf.t v2 MORE correct (559 vs v1
buggy 552 — v1 wrongly skips 7 ASCII-only DATA lines); chop.t t100 `\$a[0]==\$b`
aliasing = documented not-supported, skip-registry catches under v2-native.
**60 files v2-native (was 40) at v1 parity, all deltas explained, no crashes.**
parser2-01.t=110. **NEXT: W4 protos → W5 captured lexicals → W6 small gates →
W7/W8 full-sweep+gate parity → W9 flip default (CACHE KEYING first!).**

**SESSION 272d W4 DONE (prototype/signature subs):** pre-pass registers proto
via `parse_prototype_or_signature`→add_prototype/add_declared_sub (call sites
parse w/ imposed ctx); DEFINITION routes through `_fallback_stmt` (v1 owns
signature binding/arity); NO sub_info (call sites take fallback funcall).
**GOTCHA: `_proto_or_sig_str` must detect BOTH** `$sub->prototype` (old-style
`PPI::Token::Prototype`) AND a `PPI::Structure::Signature` child — a real
signature with `use feature 'signatures'` in the doc has `->prototype`=undef,
so checking only `->prototype` sent it to native `_lower_sub` → body with no
`@_` binding. **arith.t GIANT-FORM CRASH FIXED (not a proto bug):** `my $T=1`
then ~180 `try $T++,…` = ONE huge top-level `let`; R1 inline hot ops
open-coding across it exhausts SBCL compiler heap. Reuse v1
`Pl::Parser::_cap_inlining_if_huge` — wraps oversized (>20k char) top-level run
forms in `(locally (declare (notinline <hot ops>)))`. **Count-gate tried +
REVERTED** (over-fired chop/infnan/tr/sort/split/do/signatures/local, 120-306
stmts but compile fine — trigger is INLINE EXPANSION not count). **61 native at
v1 parity, arith 183/183 both.** parser2-01.t=114.

**SESSION 272e W5 DONE (file lexicals captured by named subs):** a single-scalar
`my $x` captured by a named sub (which hoists OUTSIDE the lexical lets) is
rewritten to a fresh package-level `$x__file__N` cell, lowered as a defvar'd box
(the `our` shape — no let, shared by hoisted sub + in-place code). Same effect v1
gets by defvar'ing file lexicals; the fresh NAME avoids proclaiming a common
symbol special file-wide. `_rename_captured_file_lexicals` runs per segment BEFORE
the pre-pass. **Conservative subset** (else keep gate→v1): exactly one `my $x`
scalar decl, no other my/state decl of the bare name, no array/hash-family use
(`@x`/`%x`/`$#x`/`$x[…]`/`$x{…}` via PPI `->symbol`), no `${x}` deref-block, no
INTERPOLATED use in string/regex/heredoc (`_interp_names` — not Symbol tokens, a
content-rewrite can't reach them). `_lower_block`/`_lower_stmt`/`_forward_global_decls`/
`_check_sub_captures` all consult `_file_lex_renamed`. **2 PRE-EXISTING v2 BUGS
FIXED (surfaced by un-gating):** (1) `_let_bound_vars` LEAKED across package
segments — a `package Foo { my @a }` file lexical (registered durably at
segment-level `_lower_block`, no `_lower_scope` wrapper) leaked into a LATER
segment's string-eval capture alist → `@b`/`$x` UNBOUND at load (grep.t). Reset
`fallback_parser->{_let_bound_vars}={}` per segment (cross-boundary `my` already
gated by `_check_my_spanning`; `_all_lex` stays file-wide). (2) NAMED sub nested
inside a signatured sub (`sub t152 ($a=…,@b){ sub t152x {@b=…} }`, signatures.t):
W4 lowers the signatured def in ISOLATION via `_fallback_stmt` (no file-scope
lexical context) → capture unbound at load; new pre-pass gate→v1. **67 files
native (was 61) at EXACT v1 parity** (_status.tsv identical bar known chop
skip-registry / sprintf-v2-better); qq.t + grep.t now fully passing under v2.
parser2-01.t=121. **NEXT: W6 small gates (continue blocks, `my $aa,$bb,$cc`) →
W7/W8 full-sweep + gate parity → W9 flip default (CACHE KEYING first!).**

**SESSION 272f W6 DONE (small gates):** (1) while/until/foreach `continue` →
native `:continue (progn …)` key via `_continue_keys`, placed AFTER the body
(parse-loop-keys finds :continue by position; v1 emits it there). Foreach
continue lowered while loop var still registered. C-for+continue gated (p-for
ignores :continue, invalid Perl); **bare-block continue `L:{…}continue{…}` STAYS
gated** (v1 runs it after the tagbody — different shape; loopctl.t's sole
remaining blocker, deliberately not chased). (2) `my $scalar <non-'=' trailing>;`
(`my $aa,$bb,$cc;` / `my $a . $foo;`) → boxed `my $scalar` let + discarded void
trailing expr; $scalar forced boxed in remainder (`$vi2`) so a later write can't
hit the setf raw-slot path. **69 native (was 67)** at v1 parity; concat.t+or.t
net-new. **Guard suite SPLIT: new `Pl/t/parser2-02.t`** (W6+ tests, 10) so files
stay manageable; parser2-01.t=122. **NEXT: W7/W8 full-sweep + gate parity → W9
flip default (CACHE KEYING first!) → W10 s270 my-across-pkg bug → C perf.**

**SESSION 272g W7 DONE + W8 IN PROGRESS.** W7 (full-sweep parity): CLEAN — full
108-file v1-vs-v2 sweep, only chop(skip-registry)/sprintf(v2-better) deltas +
int/assignwarn (parallel-load flakiness, identical isolated on both). W8
(`PCL_V2=1 prove -j8 Pl/t/` must match v1's 114f/3858t, all pass v1): **~23 files
FAIL under v2** — v2 native-lowering gaps that the perl-tests sweep MASKS (real
files gate to v1 for other reasons; the smaller Pl/t snippets are v2-native and
hit the gap). **KEY MENTAL MODEL: perl-tests parity ≠ v2 correct** — Pl/t is the
stricter gate. FIXED s272g: (1) BEGIN/END ordering — v1's p-BEGIN lands in the
`definitions` bucket alongside subs; v2 split subs→@defs and BEGIN→_captured_decls
(before defs) so `BEGIN{greet()}` ran before `sub greet` existed. New `_sched_defs`
bucket assembled AFTER defs, BEFORE run (all subs defined before any BEGIN; all
BEGINs before runtime). +gate: BEGIN/END referencing a file-`my` var → v1
(hoisted to compile-time outside the runtime lets). Found v2 MORE correct than v1:
`our $x="d"; BEGIN{$x="b"} print $x` → perl+v2 "d" (runtime our-init overwrites
compile BEGIN), v1 "b" — begin-end-01 pipeline-aware. (2) bare tail if/unless
return value — native `--pcl-if-ret--` transform DRIVEN BY $tail_ctx (each cond
`(setf RET cond)` = test → false chain leaves RET=last cond; taken branch
`(setf RET (progn body))`; form yields RET). perl-correct even on empty-true-body
(RET=nil=undef; v1 wrongly returns cond). Block+postfix+elsif+nested. (3) foreach
over aliasable lvalue element (`for($a[i])`) → gate→v1 via reused
`Pl::Parser::_foreach_alias_rewrite` (needs p-aref-box aliasing; v2 binds value).
**~19 FILES STILL FAIL** (closure/wantarray/state/match-vars/lvalue-ref/use-require/
decl-ordering-01+02/misc-fixes-01+02/transpile-test-01..05/bop-01/socket-01/
pcl-dash-m/fileio-02). Per-file triage: reproduce, cmp v2/perl/v1 → fix native /
gate→v1 / pipeline-aware (NEVER weaken). Then W9 flip default (CACHE KEYING first).

**DETAILED PLAN = `docs/parser2-prototype.md` "What's left" (d92b768)** w/
measured first-gate census (111 files: 65 string-eval, 16 bare-block, 6
labels, 4 pkg-block-form, 3 sub-captured-lexicals, 2 local-decl, 2 proto,
11 fully lower). Tiers: A=coverage (A1 eval: PPI-level gate → scoped
demotion to session-250 capture + `$x__lex__N` rename; A2 bare blocks=
LOOP-ONCE + labels; A3 route local/modifiers/for(;;) through _fallback_stmt
— NOT safe for my/loop-ctl/subs; A4 pkg block form; A5 captured lexicals =
same rename machinery as A1; A6 protos: register + _fallback_stmt the def),
B=parity (full sweep + gate under PCL_V2, flip default, fix the s270 v1
my-across-package bug properly), C=perf (native $h{k}/$a[i], OpcodeTree
VarAnnotator, lean p-sub r2 — measure first). Suggested order: A3→A2→A4→
A1.1→A6→A1.2/3+A5→B→C.

**s273 (2026-07-05, W8 FINISHED — review of Opus s272h–i batch):** decisions
D1–D22 verified sane EXCEPT **D20 (our-init raw p-box-value setf) = WRONG,
reverted (D23)**: driven by a wrong v1-era test (decl-ordering-01 "BEGIN calls
sub" — pinned v1 divergence, real perl lets runtime our-init clobber BEGIN
value); the setf shape "worked" only via a STALE BOX SV-CACHE read (value slot
was right; print read cached string; box-set invalidates, raw setf doesn't).
Test fixed to `our $result;` (no init). **v1 still emits the raw setf =
latent v1 stale-cache bug.** Handoff undercounted: 5 files failed, not 3; all
fixed: D24 bitwise `&=|=^=` VarAnnotator class (bop-01), D25 paren-less
`\substr $t` (misc-fixes-02), D26 open-family FH arg (fileio-02), D27 self-ref
`my $i = $i` → `p-box-init` let-binding init (closure-01; CL let inits eval in
OUTER env; runtime fn added+exported). **D28 = biggest find: seam my-shadow
gate** — `map/do/sub { my $x = … }` falling back over live outer `$x` wrote
through the OUTER lexical (silent corruption, common idiom); gated → v1
(`_gate_seam_my_shadow`: my/state inside a Block within fallback + name in
_live_lex; same-level my = sanctioned seam contract, stays). Reclaim = plan
W8.5 (PPI rename, W5 pattern). Guards parser2-02.t +8. Plan updated: W8 DoD,
W8.5, W9 cache-hazard-now note, W11+W14 = the perf difference-makers, W12
checklist (VarAnnotator header + D2/D11/D12/D15/D24–D26).

**s273b (W8.5 DONE, D29/D30):** First full-green v2 gate (114/3866). Parity
sweep caught defins.t PARTIAL → root cause: D4 cond-my in _all_lex blocks the
forward defvar of a SAME-NAMED package global used elsewhere (unbound crash).
Shared rename machinery `_rename_decl_within`+`_shadow_rename_blocker`
(honours perl same-statement visibility: decl RHS `my $x = $x` reads OUTER;
`->symbol` skips `$x[0]`/`$x{k}` element accesses): (a) seam my-shadow →
`$x__shadow__N`, D28 gate now fallback (blockers: interp/re-shadow/${x}/
string-eval/state) — do.t+vec.t reclaimed, yadayada.t gated; **map probe
perl-CORRECT `2 4 6 outer` under v2-native, BETTER than v1** (documented);
(b) POISONED cond-my → `$name__cond__N` segment pre-pass (only when used
outside construct; self-contained loops zero-churn). OPEN siblings (plan
W8.5): interp-token rename; C-for single-counter carve-out same poison
(rename must carry vi key or intloop unbox lost → W12).

**s273c (W9 DONE — v2 IS THE DEFAULT):** pl2cl defaults Pl::Parser2;
**PCL_V1=1 = escape hatch** (PCL_V2=1 no-op). Cache keyed by
`*pcl-cache-generation*` ("v2-1") + effective pipeline in
p-compute-cache-path (verified: per-pipeline separation + same-pipeline hit);
~/.pcl-cache cleared. **BUMP THE GENERATION on every emission-changing
commit.** begin-end-01 pipeline branch keys `!$ENV{PCL_V1}` now. Bench:
fib(29) v2 ≈0.05s vs v1 ≈0.15s vs perl 0.14s (v2 beats perl). Clean-env +
PCL_V1 gates green.

**s274d (W15.1 + W12 PREP):** W12 prep note in plan §W12 (0bec3d4): event
vocabulary per text-scan regex, analyze()-side parse passes (D7 fat-comma
snapshot + D14 _ppi_parse hazards), PCL_W12_DIFF dual-run bring-up,
swap-at-zero-unexplained. W15 perf menu in plan §W15 (incl. int-annotation
analysis: general form BLOCKED by IV→NV overflow; sound subset = C-for
literal-bounds counter decl; SBCL 2.6.0 inline+ftype ICE caution). W15.1
shipped (6a32848): W11 `=` arm emits bare `(setf (p-gethash/p-aref …))` —
**measured perf-NEUTRAL** (boundp ns-cheap; plan entry corrected), kept
because the skipped p-setf arm PROCLAIMS the lexical container SPECIAL on
first write (defvar-poisoning class; still latent in v1/fallback — logged
§W15.1). Cache gen v2-5. Gates: parity exact (sprintf-only), Pl/t
114/3891 PASS.

**s274c (W14 DONE — shift-coalesce; PERF PAIR COMPLETE):**
`_leading_shift_params` in `_lower_sub_inner`: leading run of exactly
`my $x = shift;` → params of the EXISTING `(&optional ($x (p-undef)) …)`
fast path. Guards: bare shift only (no `shift @a`/`shift()`/`//`/modifier),
distinct names (dup = illegal CL lambda list), remainder never observes @_
(shared `\@_|\$_\[|\bshift\b|\bgoto\b|\bwantarray\b` scan — a LATER shift
kills the whole run; shift MUTATES @_, the list-assign doesn't), no string
eval in remainder. shift-fib(29): v2 0.28→**0.04s** (v1 0.27, perl 0.14) —
beats perl, ~6× over v1. parser2-01.t p-shift shape test re-anchored to a
non-coalescible body (remainder reads $_[0]) — same invariant. Census 66;
parity exact (sprintf only). Cache gen v2-4.

**s274b (W11 DONE — native element access):** ExprToCL2 `_elem_place` + `=`
operator arm: `$h{k}`/`$a[i]` on LET-BOUND containers (new `lexicals` attr =
fallback `_let_bound_vars`) → v1's exact rvalue forms `(p-gethash %h K)`/
`(p-aref @a I)` (both return UNBOXED element value) and write shape
`(p-setf (p-gethash …) RHS)` (macro owns autoviv/tie/auto-declare). Package
containers/chains/derefs/multi-key/compound/++/`\` targets still fall back.
VarAnnotator `_scan`: Symbol+Subscript chain = ONE `others` value (element
may hold a ref BOX → bare `my $x=$h{k}` stays boxed; only operator-coerced
RHS unboxes — class-5). Arrhash bench 2M startup-subtracted: perl 0.17 /
v1 0.39 / v2 0.25 → **0.21s** (accumulator raw slot, loop fully native).
Census 66 unchanged; parity EXACT bar documented sprintf v2-better (+7).
Cache gen v2-3. keys/values/each native NOT done (bench first).

**s274 (W10 DONE — my-across-package):** `_rename_spanning_lexicals` pre-pass
(before `_check_my_spanning`; shares W5 scan via extracted `_scan_lex_facts`):
qualifying spanning `my` → `$x__file__N` defvar cell in declaring segment +
package-qualified `$Pkg::x__file__N` in LATER segments (qualified refs get a
harmless dup forward-defvar). Pre-decl refs (earlier segs/stmts, decl RHS) =
package GLOBAL, untouched — perl's rule, verified (`our $g=99; my $g=5;
package Foo; print $g,$main::g` → `5 99`). Subset = W5 + declaring seg not
package-BLOCK + **no string eval from decl seg on** (s250 alist captures
by-name from _let_bound_vars — renamed cell invisible). s270 repro runs under
v2; v1 still crashes (open v1 bug, v2 IS the fix). **W5 loop now sorts keys**
— unsorted hash walk made `__file__N` numbering nondeterministic per process
(emission churn = cache hazard, pre-existing). Census 66 unchanged (ref.t
shadows, sprintf2/undef/caller interp/eval-disq). Parity = all 66 native
emissions byte-identical vs HEAD. Cache gen v2-1→v2-2. Guards 01.t reshaped
+3, 02.t +1 e2e.

**s277 (block-form capture gate + W12_OLD deleted + IR review):** Fixed the
s276b catalogued bug (Try-Tiny basic.t t24–25): block-form-prototype arg body
(`catch { $caught = $_ }`) hoists as `--anon-block-N--` defun via the
`_lower_expr` bucket drain OUTSIDE the lets → unbound. Fix = gate: drain runs
the `_hoist_nested_sub` capture scan over drained text vs `_live_lex`, dies
"block-form arg body captures live lexical" → v1. Census 66 unchanged (zero
perl-tests hit it); Try-Tiny basic.t 25/25. Guards parser2-02.t +2. Cache gen
v2-7 (previously-cached broken v2 module transpiles). `PCL_W12_OLD` hatch
DELETED (text annotator = no-host/tree-crash fallback only; PCL_W12_DIFF
kept). New doc `docs/generated-cl-ir-review.md` (generated CL as IR: keep
S-exprs/closed p-* vocab/structural scope/explicit ctx; fix seams→CLForm-total,
p-esc control chars, structured p-regex, p-new-av/hv/sv, p-*-ctx macros,
defvar dedupe; consumer contract). Verified: sort-vs-map lambda asymmetry
CORRECT (return exits sort block, returns from enclosing sub in map/grep).

**s277b (IR MANUAL):** `docs/ir-spec.md` = the NORMATIVE translator's manual
for the generated CL (user's actual ask — semantics, not idiom list; the
review doc is the improvement roadmap, the spec is the meaning). Covers data
model (undef=`:undef` singleton ≠ nil=array hole; element reads unbox scalars
but KEEP reference boxes for identity ==), coercion/truthiness tables, op
return conventions (raw values / 1-or-"" compares / operand-value logicals /
overload hook order), *wantarray* protocol, p-sub convention (:p-return
frame; eval{} installs its OWN), loop-tag table, C3 string-name dispatch,
load model, op-FAMILY rules. Every claim runtime-verified. Cross-linked
CLAUDE.md + review R-1 DONE. Keep ir-spec.md updated on any semantic
runtime/emission change.

**s277c (STATE NATIVE + TRANSFER PLAN):** `state` in NAMED subs = v2-native
via new rename family `$x__state__N` (+raw `__init` flag): per-sub defvar'd
cell, v1's exact guarded-init shape, `_rename_state_vars` pre-pass FIRST
among segment renames (facts scans then see state off the bare name);
blockers reuse `_shadow_rename_blocker`; pre-pass AUTHORITATIVE (rename or
die). GOTCHA: flag must be in `_file_lex_renamed` or forward-decls emit a
BOX defvar first → flag truthy → init never runs. Probes byte-match perl.
Cache gen v2-8. Census 66 (perl-tests never used named-sub state; win =
real code). **`docs/v2-transfer-plan.md` = ROADMAP TO ONE PIPELINE** (user
direction): T0 marker+seam census; T-A gates (18× package-in-block = big;
interp-rename clears capture family); T-B eval-mode v2; T-C seams (port
head, re-house tail as CLForm emitter); T-D delete v1 (strengthen difftest
BEFORE oracle loss). START NEXT: T0.1 marker, then T-A1.

## Session-history ledger (moved from MEMORY.md 2026-07-07; detail also in docs/session-log.md)

- **W1–W7 (s270–s272g)**: package section-splitting/block-form/versioned; string-eval PPI-walk gate + `eval EXPR` capture seam (`_all_lex`, scoped `_let_bound_vars`); proto/sig subs; captured file-lexical rename `$x__file__N`; while/foreach `continue`; `my $scalar <trailing>`. W7 sweep parity CLEAN.
- **W8 (s272h–i Opus, s273 Fable review)**: 19→5→0 Pl/t files. Decision log `docs/v2-w8-session-decisions.md` (D1–D30). Key: `p-my-=` returns box; fat-comma token-restore (broke every OO `bless{key=>shift}`); D20 our-init raw setf WRONG→REVERTED D23 (stale SV-cache masked it; v1 still has the our-init stale-cache bug, open); D24 bitwise-assign boxing; D25 paren-less `\substr`; D26 open($h) FH boxing; D27 self-ref `my $i=$i` → `p-box-init` (CL let inits eval in OUTER env); D28 seam my-shadow gate (fallback-block `my $x` corrupted OUTER var).
- **W8.5 (s273b, D29/D30)**: first full-green v2 gate; poisoned cond-my rename `$name__cond__N`; seam my-shadow rename `$x__shadow__N`; map shadow probe perl-CORRECT under v2 (better than v1).
- **W9 (s273c)**: v2 DEFAULT. Gate 114/3873, census 66 native, parity EXACT (sprintf v2-better documented; assignwarn v1-flake isolated). fib v2 0.05s beats perl 0.14.
- **W10 (s274)**: spanning `my` → `$x__file__N` cell, qualified `$Pkg::x__file__N` in later segments. v1 STILL crashes on the s270 repro (`Foo::$g` unbound — open v1 bug). W5 rename loop SORTED (was nondeterministic → cache churn). Gen v2-2.
- **W11 (s274b)**: native `$h{k}`/`$a[i]` on let-bound containers (`_elem_place`; elem read counts as `others` in annotator). Gen v2-3.
- **W14 (s274c)**: `_leading_shift_params` → `(&optional …)` fast path; shift-fib 0.28→0.04s. **W15.1 (s274d)**: bare setf on let-bound element writes (perf-neutral, kept for p-setf special-proclaim hazard removal). Gen v2-5.
- **W12 (s275–276)**: TREE ANNOTATOR DEFAULT. 4 miscompiles fixed (embedded writes = write-embedded; tie needs box); split.t `nought` bug = stale `_bareword_string` on shared PPI tokens → shared `_ppi_state_snapshot/_restore`. Gen v2-6. Gate 114/3898 parity EXACT.
- **s276b**: bench (fasl exec-only) PCL BEATS PERL 7/8 (fib 0.029 vs 0.14; collatz 0.079 vs 0.695; arrhash 0.088 vs 0.152). ONE loss: string append O(n²) → plan §W15.8 (`p-str-append!` fill-pointer). CPAN under v2: Try-Tiny 5/3/3 of 11, S-L-U 7/22/9 of 38, Role-Tiny 4/6/13 of 23; TAP names exported eagerly from :pcl fixed NAME-CONFLICT (9ca0026). Block-form-arg body capturing block-level `my` catalogued (v1 passes; capture family). V1-vs-V2 (s272d): arith 10.9×/6.8×, `my($n)=@_` 5.4×; details `docs/parser2-prototype.md`. TODO: perl t/mro + t/class never surveyed; W13 only if measured.
- **s277**: block-form capture GATE (Try-Tiny catch fix → basic.t 25/25); `PCL_W12_OLD` hatch DELETED; review doc `docs/generated-cl-ir-review.md`. Gen v2-7. V2 correctness-COMPLETE (rest = perf §W15).
- **s277b**: `docs/ir-spec.md` = NORMATIVE semantics manual — UPDATE on semantic emission/runtime changes. **s277c**: state native in named subs (`$x__state__N`; GOTCHA: flag symbol must be `_file_lex_renamed`-marked). Gen v2-8.
- **s278 (Fable→Opus handoff)**: T0 DONE (marker/censuses), T-A1 flattening built behind `PCL_V2_PKGBLOCK=1` (open: join.t section-ordering miscompile), VarAnnotator write-deref-viv fix. **Read `docs/v2-transfer-plan.md` §SESSION 278 STATUS.** Gen v2-9.
- **s279 (Opus 4.8)**: native self-referential container init `my @a=(@a,…)`/`my %h=(%h,…)` — bind to `(p-copy-array/-hash <RHS>)` in the let BINDING position (RHS sees outer var, CL parallel-let). SIMPLE single-container only (Symbol @x/%x, no nested my/our/local/state in RHS); list-form (`my (undef,@a)=@a`) + nested-declarator (`my @a=my @a=…`) still v1. Safe groundwork (dying construct → no regress). array.t still gates on its list-form self-ref (census 75 unchanged). ALSO fixed PRE-EXISTING p-copy-hash bug (both pipelines incl v1 `local %h=%h`): hash-table branch shallow-copied value BOXES → `my %h=%h;$h{k}=…` mutated source; now mints fresh entry per real key via %p-make-hash-entry, copies :__class__ verbatim. Gen **v2-17**. Commit bd56e56. THEN CORE:: declarator prefix (my/our/state/local) stripped at SOURCE level (_preprocess_source, str_re+lookbehind+lookahead guarded) — MUST be pre-PPI (PPI mis-structures `for CORE::my`, loses list/block). De-gates for.t → v2-native, census 75→**76**; fixes v1 too. Commit 23e101a. Gate 114/3935 green; fully-passing 64.
- **s279 cont. (capture consolidation, commit eb09826)**: extracted the ONE sharp rewrite primitive `_rewrite_var_uses` (sigil-aware, keyed on ->symbol: @a/$a[i]/@a{}/$#a follow the one array, sibling $a/%a untouched) — replaces the "rewrite-by-content, safe only via disq" hazard across the 4 promotion passes (spanning/state/W5/cond-my = a house of cards, silent-miscompile on wrong sigil). Added CONTAINER capture: `{ my %cache; sub get{} sub set{} }` idiom → shared defvar container cell (array/hash analogue of W5 scalar box), reusing parked W10-ext-3's container_decl fact + defvar-container lowering. **Block-extent guard (critical)**: refuse if any family-use escapes the decl's enclosing block (a use after the block = package @a, a DIFFERENT var; single-cell promotion would MERGE them). method.t's %methods is captured CROSS-package → needs container SPANNING (cross-segment), NOT this per-segment pass → stays gated (defer w/ #40). Corpus byte-identical (census 76, fully-passing 64). De-gates ZERO torture files (they gate on other shapes) but fixes the common CPAN idiom. **Remaining capture de-gates (task #44)**: multi-scalar my($a,$b) capture (push.t); block-extent-scoped decl_count (do.t `my $called`×3 file-wide false-positive — ALSO helps scalar W5/W10); _hoist_nested_sub over-broad text scan (wantarray.t). NOTE: scalar W5 has the SAME latent block-escape conflation bug (unguarded) — rare pattern, left as-is.
- **s279 cont2 (block-extent capture, commit 229cb94)**: unified scalar+container capture into ONE per-decl EXTENT-scoped promotion `_promote_captured` (extent = nearest enclosing block, else segment; same-name my in a DIFFERENT block = distinct var). Promote iff within its own extent it's the sole decl of the bare name (`_count_name_decls`) AND captured by a named sub in that extent (`_captured_in_subs`); rewrite confined to extent (block-scoped `_rewrite_var_uses($stmts,$canon,$new,$extent)`). Replaced file-wide decl_count==1 (which conflated same-name across blocks) + the container escape-guard. **De-gated do.t (`my $called`×3 separate blocks) + delete.t (X::DESTROY static-var); census 76→78; both exact v1 parity.** GOTCHA FIXED (silent miscompile): `_interp_names` only matched $-sigil, so interpolated ARRAY `"@x"` set no guard → container promotion renamed decl/writes to @x__file__N but the interpolated read stayed bare @x (empty) → split. Now `_interp_names($node,$out,$sigils)`; @-forms → `interp` ONLY (not disq → scalar path byte-identical). Corpus byte-identical except the 2 de-gated files. **REMAINING capture (task #44)**: multi-scalar my($a,$b) (push.t); `_hoist_nested_sub` over-broad text scan (wantarray.t); container SPANNING cross-segment (method.t, w/ #40). NOTE the block-extent promotion is exactly the reusable core for container-spanning later.
- **s278b (Opus 4.8)**: T-A1 DEFAULT-ON (`PCL_V2_PKGBLOCK` flag removed). join.t bug was NOT ordering — `_all_lex` forward-decl exclusion was bare-name file-wide; fix = package-aware (name→pkg→1). Census 66→70; pkg-in-block gate → 5 residue files. Seam census: 88.9%/81.2% expressions fall back → T-C(ii) re-housing CONFIRMED; module corpus ZERO pkg-in-block, 17× W5-capture (A2 > A1 for CPAN). Gen v2-10. Detail: `docs/v2-transfer-plan.md` §278b.
- **s280 (Fable)**: capture family DONE (task #44) — census 80, gen v2-19/20. Shadow-aware `_block_captures_name` replaced both capture-gate text scans (wantarray.t); multi-scalar `my($a,$b)` capture + interp-following scalar rename in `_rewrite_var_uses` (scalar guard = new `family` fact, not `disq`) de-gated push.t + Test::Tester (CPAN). qr//+readline added to `_interp_names`/`_interp_canon`; `_check_my_spanning` skips unmangled canons. NEW GATE `_check_interp_postderef` ("$r->@*" silent miscompile in BOTH pipelines — task #45 cascade). v1 bug logged: nested `sub{ my $x = $x+1 }` reads fresh box. @a/@b removed from `_forward_global_decls` runtime_vars (only $a/$b runtime-owned). Verify method est.: worktree byte-diff vs HEAD (normalize paths, strip `;;; pcl: pipeline=` marker) + `--jobs 1` sweeps of changed files.
- **s281 (Fable)**: E1.5 nested `package` DONE as D1-lite — census 84, gate 114/3948, gen v2-21. See [[project_v2_nested_package_d1lite]] (mechanism, the 3 exposed pre-existing bugs, gotchas).
- **s282b (Fable)**: E1.1 DONE (tasks #39+#40), census 84→86 (method.t + sprintf2.t de-gated), gen v2-24. Container-spanning loop ported into `_rename_spanning_lexicals` (patch was 3/4 already in-tree; file-unique identity-unmangle, interp-refusing, sigil-preserving rewrite). Fixed pre-existing `_interp_canon` $1-clobber (inner `$2=~/\[/` on success resets $1 → interpolated `"$x[i]"` hits dropped). method.t cascade: (1) `sub main::::flomp` PPI name-split → doc-normalization merge pass in Parser2::parse; (2) indirect SUPER block forms `SUPER::m{}@a`/`SUPER::m{@a}"b"` implemented (PExpr Word+Block branch à la `system{PROG}LIST`; ExprToCL emits ALL kids; `%pcl-super-indirect` &rest+flatten; semantics verified vs perl: block+LIST concat, invocant=first). method.t v2 102+29 ⊃ v1 99+32, both stop@157 (shared, next target); sprintf2 1619 exact-parity. Remaining spanning gates = SCALAR spans (caller/eval/ref/scalar/sort) → E1.2.
- **s282c (Fable)**: E1.2 INVESTIGATED, plan in session-log §282c (resume there). `PCL_SPAN_DEBUG=1` = new gate-hit diagnostics. M1 shipped (41881c0, corpus-byte-identical): `_symbol_is_declarator` climbs list-decl `Statement::Expression` wrappers — **exact `ref() eq` REQUIRED, isa() walks out of Statement::Variable (subclass!) and re-gated do/each/vec/sprintf2**. caller.t still gates post-M1, reason TBD (PCL_V2_VERBOSE first). M2=`open my $fh` expr-embedded decls; M3=shadow-aware extent dc+rewrite (ref/sort/scalar); eval.t=defer (eval-by-name needs capture-alist-under-original-name). Parity baselines captured in log.
- **s282 (Fable)**: task #49 DONE, gen v2-23 — `package` inside EXPRESSION blocks now block-scoped at all three levels: (1) `parse_block_as_function` snapshot/restores package_stack (leak hit code BEFORE the stmt — 3× pre-pass parses); (2) `parse_block_to_cl_string` same + `_block_depth` bump — **`eval { package X; … }` had silently emitted `(p-eval-block nil)`** (top-level pkg path opens a section the string collector drops); (3) runtime revert via `(let ((*pcl-current-package* …) [(*package* …)]))` wrapper (tail value passes through) + `_block_depth` bump in parse_block_as_function so nested named subs qualify (`XD::pl-mk` def/call now agree). edge2 battery recreated, all-match-perl except 2 deliberate: do-tail value (`do{42; package XT;}` → perl 42, PCL "XT") and sort `$a/$b` re-homing — both pathological, pending user sign-off for not-supported.md. Corpus diff: only caller.lisp (intended + fixed a real `DB::pl-pass` leak). 5 tests → transpile-test-04b.t.
- **s283 (Fable)**: E1 survey → `docs/e1-remainder.md` (per-file gate triage by mechanism M-A…M-F, exact gate strings + SPANREFUSE traces); bop.t de-gate attempt showed the parity rule's teeth (lost a test → for-scope fix first). Census 89, gen v2-25, commit cd9dd3b.
- **s284 (Opus 4.8)**: M-C + M-D SHIPPED — hashassign/index/undef de-gated, exact sweep parity (25-file corpus diff line-identical). CAPREFUSE diagnostics; canon+sigil-shape-aware capture gates; `_promote_captured` rewritten (shadow-aware `_hard_decl_count`, positional pre-decl/RHS-reads-outer, post-decl-sub capture, string-eval-name guard, extent-scoped family); identity promotion for file-unique names (no mangle → eval/interp/`${x}` safe); M-D decls-inside-named-subs + init'd single containers + mixed list decls (#50); container interp rewrite (`"@a"`/`$a[`/`$#a`/`$h{`/`@h{`); embedded-my let-hoist (weaken box-in-box REF≠HASH fix; veto when another named sub references the name). v2 FIXES two v1 miscompiles natively (shadow-RHS capture; block container capture). Census 92, gen v2-26.
- **s285 (Opus 4.8)**: E1-a element foreach-alias SHIPPED — chop/aassign/sub de-gated (`_alias_box_form` head-swap `p-gethash`→`p-gethash-box`/`p-aref`→`p-aref-box`; container already boxed, no annotator change). substr.t re-gated on `foreach over a magic-lvalue element` + per-statement void-wrap heap exhaustion (CLAUDE.md #8 → E2 hoist). Real bug: bare `return;` was `(p-return (p-undef))` → zero-arg `(p-return)`. Census 95, gen v2-27, gate 114/4003.
- **s286+s286b (Fable)**: exec-speed "regression" RESOLVED (none — shape taxes); counting-loop range foreach SHIPPED (`p-foreach-range`/`-raw`, endpoints once, no vector; `%p-range-classify` shared with p-..; `foreach_range_split` AST oracle — bare Words reject; annotator foreach-alias veto refined → raw my-var). `for my $i (1..5M)` 2.8× FASTER than perl (was 7.8× slower). Gen v2-28, gate 114/4023, commit c7ba84a. Left → #62: `+=` raw verdict, postfix-for range.
- **s287 (Fable)**: E1 M-E singles SHIPPED (8bb3792) — **loopctl.t de-gated 67/67** (bare-block `continue{}`: labeled in-compound extract + PPI orphan-sibling join incl. glommed trailing; fixed silent v2 miscompile — unlabeled continue was DROPPED); **my.t de-gated 49/1 v1-parity** (standalone label → `(tagbody :label <remainder>)`, backward goto = lexical go; value-position/forward gotos gated); list-form self-ref init (`my (undef,@bee)=@bee` per-var copy-binding: p-copy-array/-hash/p-box-init) + chained `my @a = my @a = …`; container capture de-conflation (`_hard_decl_count`/`_count_name_decls` sigil-aware for container canons — `my $x` beside `my %x` no longer blocks %x). array.t re-gated on `forward goto to a standalone label` (goto out of map LAMBDA needs dynamic throw = task #63; **v1 CRASHES on that shape** — crash-fix item, not just de-gate). PERL GOTCHA: mid-position `grep{}` in `||` chain swallows rest as LIST — parenthesize. Census 97/14, gen v2-29, gate 114/4034. Docs refresh 74fd499 (plan guardrails 10–15, ir-spec §6.2/6.4, raw-verdict scope boundary); tools `corpus-diff.pl` + census pointer added.
- **s288 (Fable)**: task #60 void-wrap hoist SHIPPED (ecda6a9) — sub-body `:void` regime in v2 (`_lower_body_regime`: one bind per multi-stmt body, `wa_void_active` suppresses seam `_ctx_wrap` + ExprToCL2 funcall bind + g-match wrap; `_restore_caller_wa` LEAF-level tail restore — never wrap a compound tail; single non-compound-stmt bodies skip = accessor carve-out). Large-sub SBCL heap blowup GONE (300-stmt probe: 301→1 binds); **substr.t's only gate now = magic-lvalue foreach**. Verified: corpus-diff 35/111 all regime-shaped; FULL-sweep per-file parity vs HEAD (18133/64-fully; print.t 0/3 was load flakiness — fresh_perl_is subprocesses; stale "can't run" doc rows corrected); gcdrec bench row added (+0.7% noise). **`cl/pcl-pack.lisp` REGENERATED by v2** (92c0046): pack oracle transpiles v2-native zero-gate, pack.t 5638/87 with the SAME 87 test numbers — first production artifact on v2; body 4726→2079 lines. Found task #64: bare-block sub tail loses value (BOTH pipelines, pre-existing; if/else tails fine). Census 97/14 (hoist de-gates nothing itself), gen v2-30, gate 114/4036.
- **s289 (Fable)**: E1 M-A SHIPPED (task #67) — **pack.t de-gated 5638/87 (SAME 87 as v1)** + **yadayada.t 21/15 parity**; census 99/12, gen v2-31. Three mechanisms: (1) interp rewrite — `_interp_fixer`/`_fix_interp_token` factored from `_rewrite_var_uses`, wired POSITIONALLY into `_rename_decl_within` (pre-decl + decl-RHS interp keeps outer name); `_shadow_rename_blocker` "interpolated use" refusal REMOVED (brace-deref `${x}` still refuses) — lifts cond-my/seam-shadow/state families at once. (2) **Oversized-extent flattening**: top-level `my` nests the whole remainder in ONE let; pack.t = one 162k-char form -> SB-REGALLOC heap OOM at 1GB even notinlined. `_oversized_top_decls` force-promotes top decls with post-decl runtime remainder > $RUN_NEST_MAX (20k src ~ 64k emitted at 2.2-3.2x ratio) via `_promote_captured(force)`; refusal dies -> v1. `_gate_oversized_run_form` ($RUN_FORM_MAX 64k; largest passing corpus form ~55k) = by-construction backstop, OOM class unreachable. Re-emitted split/sprintf/sprintf2 — exact HEAD parity. (3) **`_premerge_include_prototypes`**: v2 lowers subs + VarAnnotator-preparses BEFORE the use/require fallbacks that learn prototypes -> cross-require `sub is ($$@)` never imposed scalar ctx in v2 sub bodies (`is($be, reverse($le))` LIST-reversed — silent wrong-context at HEAD, invisible while pack.t gated). Pre-merge all PPI::Statement::Include (nested/BEGIN incl.) at parse start. GOTCHA: baseline for a v2-NATIVE file is a HEAD-WORKTREE sweep, NOT PCL_V1 (sprintf.t v1 plans 552 vs v2 559 — "different fails" was baseline error). Oracle cross-check (user req): pack.t on system perl with CORE::GLOBAL pack/unpack -> pack-impl.pl: 372 fails = 311 float-stub-only + 61 common (oracle bugs) + 26 CL-only (23 utf8-flag not-supported + 3 byte/charset). CL translation loses only ~3 substantive tests vs the Perl oracle.
- **s290–s292 (Opus 4.8)**: M-B per-declaration spans → sort.t de-gated (s290, census 100/11, gen v2-32); scalar.t de-gated at exact parity + nested-require inline + paren-print filehandle self-heal (s291, 101/10); substr.t de-gated (magic-lvalue foreach residue) + nested-sub bareword registration + magic-lvalue-arg force-box (s292, 102/9, gen v2-34). Full detail: docs/session-log.md §290–§292.
- **s293 (Fable)**: E2.0 SHIPPED (task #57) — emitter-conversion scaffold: `CLForm::to_flat` (exact flat renderer), ExprToCL `form_handlers`/`gen_node_form` (form emitters win, may DECLINE undef→text; decline BEFORE side effects), `corpus-diff.pl --show`. Dual-run = worktree-vs-HEAD via corpus-diff.pl, NOT in-process (emitter side effects). First 3 emitters converted at byte parity BOTH pipelines (gen_ternary, gen_string_concat, gen_array_str_interp). Guard `Pl/t/clform-01.t`. GOTCHA: PCL_V1 full gate has 7 PRE-EXISTING fails (v2-only-feature tests through pl2cl; verified identical at HEAD) - E2 v1-gate criterion = failure set identical to HEAD, not green. Recipe → exec-plan §E2.0. Census 102/9 unchanged (E2 ≠ gates), gen v2-34 (no bump — byte-identical).
- **s293b (Fable)**: E2.1 step 1 — `gen_funcall_form` = the `funcall` form handler covering the GENERIC call path (user subs incl. is/ok frontier head + non-special builtins + prototype p-scalar/p-backslash + print $_ default + die/warn :loc + my/our identity + split/join wraps + ctx binds via `_ctx_wrap_form`/`_wrap_wantarray_ctx_form`) at byte parity BOTH pipelines (corpus 111/111 identical). DECLINES before any side effect: `%FUNCALL_FORM_DECLINES` (require/goto-family/do/eval/grep/map/bless/push/unshift/readline/select/tied/pos/delete/exists/defined/undef/chop/chomp) + -bareword/SUPER::/non-Word heads — the hash IS the remaining E2.1 worklist. Word-head-only guard: gen_node on a Word is pure (gen_leaf), so decline-then-text-re-run repeats no side effect. clform-01.t 15 guards.
- **s293c (Fable, ⚠ UNCOMMITTED at session end)**: E1 M-F task #69 in progress — eval-capture of span-mangled lexicals. In tree: `_eval_lexical_alist` appends `_eval_span_captures` pairs (orig "$x" → `Pkg::$x__file__N`; let-bound shadows win by assoc order; v1 byte-neutral); span pass registers pairs per extent segment (`//=` innermost-first) + section driver publishes per segment; DROPPED span refusals `eval-unsafe (non-unique)` (+W10-ext-4 scan) and `family use` (stale since s289 fixer — skips `$x[`/`$x{`); `_block_captures_name` per-canon string patterns + shadow-checked Quote/HereDoc tokens. Probe green == perl (read/write-back/dynamic eval/shadow: "42 6 84 99"). ref.t transpiles V2-NATIVE (sweep parity unverified). eval.t peeled 3 layers, stopped at `file lexical yyy captured by sub fred4` (genuine eval-string capture, eval.t:326/332). NEXT: (a) `_captured_in_subs` count eval/quote-string mentions (reuse canon+shadow logic) so `_promote_captured` promotes; (b) _promote_captured mangled renames must register eval_span_captures pairs too. NO verification run yet; CACHE-GEN BUMP required before commit. Baselines (v1 fallback): eval.t 121+39/169 PARTIAL@163, ref.t 183+19/245 PARTIAL. Detail: session-log §293c + task #69.
- **s297–s298 (Opus 4.8, on branch `wip/s296-state-family`, NOT main)**: E2.1 internal-node frontier continued (task #68). s297: funcall introspection/lvalue/goto/do/grep-map families + leaf sub-phase (Number/Symbol/Magic/Quote/Word/Operator) + internal nodes arr_init/hash_init/func_ref/a_acc/h_acc/access-slice family/progn/backtick/readline/filehandle/glob_slot. **s298: four form handlers — `methodcall` (a9ba5e3) + `prefix_op` (b6c5a23) + `postfix_op` (3970ccf) + `tree_val` (d8cce38).** methodcall: invocant disambig AST-level; SUPER:: text-check only on a static Word bareword; method child generated ONCE for gensym order; `_ctx_wrap_form` bind. prefix_op = PARTIAL — DECLINES `\`/`++`/`--` (text emitter inspects GENERATED operand text `/^\(p-array-last-index|p-substr|p-pos|p-vec/` for magic lvalues), converts the rest. postfix_op: converts chained-cmp + plain `++`/`--`, DECLINES only `$#array++` arylen setter (AST-detected via new `_operand_is_arylen` = ArrayIndex leaf or `$#` prefix_op). **tree_val: the tricky one** — its `$child =~ /\(p-=~/` list-ctx regex check (regex match returns captures → NOT `(vector …)`, → `(let ((*wantarray* t)) child)`) is reproduced BYTE-EXACTLY via `to_flat($child) =~ /\(p-=~\s/` (E2 invariant: `to_flat(gen_node_form(x))==gen_node(x)`). **A pure AST predicate is UNSOUND** — must fire when `(p-=~` is ANYWHERE in child (nested in a larger expr `(1+($x=~/y/))`, or inside an inline_lambda `body_cl` opaque string an AST walk misses); `!~`→`(p-!~` stays vector; empty `()` declines. Verified additionally by direct HEAD-worktree byte-diff on an adversarial probe. All via `form_handlers` (text handler kept as decline fallback/HEAD oracle; decline BEFORE side effects). Verify each: `tools/corpus-diff.pl` + `PCL_V1=1 tools/corpus-diff.pl` BOTH byte-identical to HEAD; no cache-gen bump (emission unchanged). Also `ref_funcall` (0594551): `$cref->(args)`→`(p-funcall-ref …)` ctx-wrapped, clean. **`gen_binary_op` DONE (0caab12) — the big one, every operator.** converts arith/compare/logical/string/`.`/`x`/`..`flipflop+range/`isa`/use-integer + `=~`/`!~` (4b2cfad: subst/tr-RHS skip-wrap decided AST-level = RHS is Regexp::Substitute/Transliterate node, not text-grep). **Only `=` still DECLINES** (LHS-text sigil/magic-lvalue/typeglob dispatch — before side effects, op-string decision). **DUAL-REP wiring (the caution): binary op reaches codegen as `PPI::Token::Operator`/`Word` WITH children AND as internal-node type → `gen_binary_op_form` wired at THREE points: `gen_internal_node` (`!exists handlers{type}`→to_flat), `gen_node_form` internal-branch (same guard), `gen_node_form` Operator/Word-with-children branch (replaced `raw(gen_node)`).** Each guarded, decline→unchanged text, no double-gen (decline precedes all side effects incl. `$g_flipflop_count++`). Verified: corpus-diff BOTH pipelines identical to HEAD 111 files + HEAD-worktree byte-diff on operator-heavy probes. Also `anon_sub` DONE (9b150d9): `sub{}` via expr path (real site = `s///e` replacement → `(lambda () CODE)`) → `['lambda',['list'],@body]`, empty declines. **Remaining E2: `=` un-decline (LHS-text AST rewrite), `glob` (negated-class text analysis), regex leaves (`qr//`/`m//`/`s///`/`tr//` — non-idempotent regex side effects), `inline_lambda` (E2 final — `body_cl` pre-gen CL strings; also removes VarAnnotator seam special-case), then E2.final delete text printer.** clform-01.t = 144 guards. Gate green bar the 2 parked s296 state fails (state-01.t #3, parser2-02.t #39).
- **s294–s295 (Fable)**: E1 M-F SHIPPED (task #69 done) — **eval.t de-gated 126+34/163 (BEATS v1 121+39, strict-SUBSET fail set: fixes t27/28/81/84/97) + ref.t 183+19/245 IDENTICAL fail set; census 104/7, gen v2-35**. s294 tried a runtime EVAL-CELL REGISTRY (hash + lookup stop + p-eval permanently registering site alists) → structural regression: a side registry with fixed precedence lets a stale sibling-shadow entry permanently poison the live cell. s295 replaced it with the **ALIAS RULE (normative: ir-spec §9.1)**: `(p-alias-eval-cell '$x $x__file__N)` at the renamed decl's RUN position = `(setf (symbol-value sym) cell)` on the ORIGINAL-name global of the declaring package (quoted unqualified symbol → reader interns under the section's in-package) — ONE storage location shared with plain defvar'd lexicals = v1's time-ordered last-decl-wins model; `p-eval-lex-lookup`/`p-eval` byte-reverted to v1. Site alist (innermost-first `__shadow__N` DESC, plain last) covers let-bound; pkg-qualified span pairs cover cross-package sites (stop-2 interns in the SITE's package). `_file_has_str_eval` gates emission → eval-free files byte-identical. ALSO FIXED: `_enclosing_lex_decl` blind to already-renamed enclosing decls (strip `__(file|lex|shadow|cond)__N` before compare — promotion ORDER decided the outer-my refusal; encl probe silent "2 2"). parser2-01 t65 stale guard updated (interp captured lexical now = IDENTITY promotion, defvar under original name, native). Guards: transpile-test-01b +4 (read/write-back/dynamic/shadow, evaldef nested dark eval, encl split). Verified: probe battery == perl (evalspan/recurse/evaldef/dbblock; dofile+encl v1-parity classes; v1 CRASHES on evalspan's cross-pkg spanning print = its known W10 bug, v2 better); corpus-diff v1 BYTE-IDENTICAL 111 files, v2 = 2 de-gates + 12 files (alias adds/new promotions/fwd-decl defvars) ALL sweep-identical to HEAD in a HEAD worktree; v2 gate ALL PASS 115/4064; PCL_V1 gate = known 7 v2-only set. Detail: session-log §295, ir-spec §9.1.
