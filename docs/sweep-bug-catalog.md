# PCL Sweep Bug Catalog

## Real CPAN-module test-suite findings (session 262)

Strategy: run a real pure-Perl module's *own* `.t` suite through PCL (the module
ships in the perl source tree, e.g.
`perl5/perlbrew/build/perl-5.40.3/perl-5.40.3/cpan/<Mod>/t/`), using `is`/`ok`
as the oracle — a far deeper compositional fuzzer than single-expression tests.
First target **Text::ParseWords** surfaced two real bugs:

- **Undef regex captures vanish in a `my (...) = (...)` list — FIXED.** A
  non-participating capture group was raw `nil`, but `%p-flatten-list` (the
  `p-list-=` flattener) drops raw `nil` as an array-hole/empty-list marker, so
  `my ($a,$b)=($3,$4,'Z')` shifted `'Z'` into `$a`. (`p-array-=` kept it, so
  `my @x=(...)` was unaffected — the inconsistency.) Fix: capture-group undef is
  now `*p-undef*` (`clear-capture-groups`, `%set-cap`, the `$1..$9` defvars), per
  the flattener's own convention ("Perl undef comes as `*p-undef*`, not raw
  nil"). Regression tests in `Pl/t/misc-fixes-02.t`.

- **cl-ppcre `/x` + scoped `(?-x:…)` — OPEN (cl-ppcre bug, needs PCL workaround).**
  `:extended-mode` is not restored after an inline `(?-x:…)` group, so trailing
  whitespace/comments become literal and the match fails. Blocks
  `Text::ParseWords::parse_line` (and any `/x` regex using `(?x:)`/`(?-x:)`).
  Full writeup + minimal repro: `docs/clppcre-extended-mode-modifier-bug.md`.
  Workaround = PCL-side `/x` normaliser (strip insignificant ws/comments
  ourselves, honour `[…]`/escapes/mode-scopes, drop `:extended-mode`). Safest to
  engage the normaliser ONLY when the pattern contains a `(?x`/`(?-x` modifier,
  bounding the blast radius to the already-broken case.

## Differential-fuzzer findings (session 241, `tools/difftest-ops.pl`)

The operator/precedence fuzzer (PCL vs real perl 5.40) surfaced 5 bugs that the
whole `perl-tests/` suite missed — all FIXED session 241 (regression tests in
`Pl/t/misc-fixes-02.t`): chained string-cmp crash (`cmp-op-to-fn`→`p-str-*`),
bitwise `& | ^` signed→unsigned-64 (`%pcl-to-u64`), relational-vs-equality
precedence (chained-cmp left-scan now same-prec only), false-comparison returns
`""` not nil (`p-bool` wrapping the cmp macros), and bitwise string-mode needing
BOTH operands strings (`or`→`and`). See `memory/project_difftest_fuzzer.md`.

Two fuzzer mismatches DEFERRED (not fixed, by design / risk):

- **`**` always returns NV in Perl, exact bignum in PCL.** `2 ** 3 ** 4` →
  perl `2.41785163922926e+24` (lossy float), PCL `2417851639229258349412352`
  (exact). Only differs for results > 2^53. **Representation choice** — PCL's
  exact value is arguably better, and matching Perl risks regressing any
  `2**N`-as-integer-size/bitmask code with no CPAN module needing the float
  imprecision. Treated as a documented difference; revisit only if a real module
  depends on the NV behaviour.

- **Named-unary operator precedence (REAL fix-target, deferred).** Perl gives
  named unary ops (`length`, `sqrt`, `int`, …) LOWER precedence than `+ - .`, so
  `length $s + 1` means `length($s + 1)` (= 1); PCL computes `length($s) + 1`
  (= 5). The bug is in `Pl/PExpr.pm` ~line 2784: named unary is parsed by a
  special "consume one term" mechanism (not the precedence table), and the
  comment there wrongly claims they "bind tighter than binary operators".
  Correct perlop order: `* / % x` > `+ - .` > `<< >>` > **named unary** >
  relational. Fixing means giving named unary a real precedence band looser than
  `.`/shift — an involved parser change, so deferred to a focused session.

- **Generated-CL whitespace cosmetic** (`(p-if                       (cond)` big
  gap): condition sub-expressions carry a leading `indent_str x indent_level`
  prefix when inlined into `(p-if …)` (tail-if transform / statement-path
  render in `Pl/ExprToCL.pm`). Cosmetic codegen TODO, no semantic effect.

### Brace block-deref + subscript — FIXED (session 241, deref axis)

Found by the `tools/difftest-ops.pl` deref axis. The **`${ BLOCK }[idx]`** and
**`@{ BLOCK }[slice]`** forms — block-dereference of an array ref followed by an
element/slice subscript — silently produced the wrong value:

```perl
my @a = (10,20,30); my $ar = \@a;
${$ar}[1]      # perl 20      — PCL was undef
@{$ar}[0,2]    # perl (10,30) — PCL was ()
```

**Root cause:** PPI mis-tokenizes the `[...]` after a `${BLOCK}`/`@{BLOCK}` as a
`PPI::Structure::Constructor` (anonymous-array literal) instead of a
`PPI::Structure::Subscript` — purely because it follows a Block `}` rather than a
Symbol.  The hash form `${$hr}{a}` is correctly a Subscript (that is why it always
worked).  The Cast+Block+Constructor triple matched no case in the PExpr main loop
→ fell through to the "Missing case" die → degraded to a silent `(progn ;; …)` =
`undef` (which is why this *common* idiom survived undetected).

**Fix:** new preprocessing pass `_retag_braced_deref_subscript` in `Pl/PExpr.pm`
re-blesses the mis-tagged `Constructor` into a `Subscript` when it follows a
`$`/`@` Cast + Block, so the existing Cast+Block+Subscript machinery (the same path
`${$hr}{a}` uses) handles it.  `%`-cast (KV slice `%{$ref}[i]`) and `*`-cast (glob)
are left as Constructors — they have dedicated handlers.  Verified vs perl 5.40;
regression test in `Pl/t/misc-fixes-02.t`.  Adjacent forms unaffected (anon arrays,
`%{$ref}[i]`, `${$hr}{a}`, `@{$hr}{…}`, nested `${$rr}[1][0]`).

---

Generated 2026-05-07. Baseline: 18209 pass / 10159 fail across 100 files, 40 fully passing.
Updated 2026-05-15 (session 185). Current: ~12506 pass / ~2396 fail, 42 fully passing.
Updated 2026-05-23 (session 200). Current: 27439 pass / 2230 fail, 58 fully passing (107 files + 2 skipped).
Updated 2026-05-23 (session 201). Current: **27443 pass / 2226 fail, 58 fully passing** (107 files + 2 skipped).
Updated 2026-05-23 (session 202). **`%a`/`%A` hex-float sprintf implemented** (cl/pcl-runtime.lisp sprintf-one). `parse-perl-number` overflow bug identified but NOT YET fixed (see infnan.t entry).
Updated 2026-05-24 (session 207). **Fixed: time.t fully passing (72/72); chdir.t fully passing (44/44); qr.t 18→21 passing; args.t 15→18 passing**. Skips sprintf.t (deprioritized) and Unicode/utf8-encode issues (documented not-supported).
Updated 2026-05-26 (session 210). **PPI 1.291 upgrade. 27727 pass / 903 fail, 58 fully passing. Dotted bitwise ops (&./|./^./~.) implemented. newline-in-use-comment bug fixed. bop.t: 434→446 passing (crash vs early-stop). sprintf.t: POSIX::DBL_MAX crash (pre-existing, no stub).**
Updated 2026-05-27 (session 211). **flip.t 12→13 passing (test 13 fixed: flip-flop state sharing across recursive sub calls). `return /3/../5/` now generates `p-flipflop` not `p-..` range. Added `p-flipflop-dyn`/`p-flipflop-dyn-3` macros for string-literal flip-flop operands (compare with `$.`). PCL test suite: 78 files, 3010 tests, all passing.**
Updated 2026-05-28 (session 212). **27787 pass / 841 fail, 60 fully passing. 4 bugs fixed: (1) p-return-value case 3 stripped box from references (return \%x → hash count); (2) sub hoisting inside let-bound blocks (call-before-def only, first-def-only); (3) eval{} boundary in _find_all_declarations (vec.t 25-26); (4) inline package @ISA wrong package (index.t 119: MyTie::@ISA now qualified). Newly fully passing: join.t, range.t, lc.t, vec.t.**
Updated 2026-05-28 (session 212b — no-op). **Added perl-tests: tr.t (94/134 failures, then crashes on Unicode non-chars), eval.t (crashes early, needs investigation), yadayada.t (partial failures). Removed: repeat.t (3 failures, edge cases), numconvert.t (fully skipped on 64-bit). Noted: ExprToCL.pm warnings ($idx undef line 2082, $num undef lines 570/583/589/595/601/607/609) exposed by numconvert.t — TODO to fix. ${^MAX_NESTED_EVAL_BEGIN_BLOCKS} documented as not-supported.**
Updated 2026-05-28 (session 214). **28464 pass / 1036 fail, 60 fully passing** (104 files + 2 skipped). +468 vs session 213, no fully-passing drop. sprintf.t **14→460**, length.t **32→35** (overloaded `length` via `to-string` on boxed value — `p-length` no longer unboxes before stringifying). sprintf fixes:
  - (1) `%p-array-store-scalar` now COPIES the scalar container for refs to raw objects (array/hash/code/glob/qr) — `[$x]`/`@a=($x)` no longer aliases the original box when `$x` is later reassigned (was corrupting sprintf.t's `@tests` across `<DATA>` iterations). Box-in-box (scalar refs `\$x`/`\\1`) keep the box AS-IS, since box-nesting depth encodes SCALAR-vs-REF (adding a layer turned `\$x` into REF, broke ref.t stringify).
  - (2) `do-regex-subst` now passes `:extended-mode t` for `/x` (was silently ignored in s///; match path already had it).
  - (3) sprintf integer formatting rewritten (`sprintf-one`): `%#x`/`%#b` suppress the `0x`/`0b` prefix when the value is 0; `%#o` forces a leading `0`; `%.0d`/`%.0x` of 0 → empty. The `0x`/`0b`/`0` prefix now sits left of zero-padding (`%#08x` 255 → `0x0000ff`).
  - (4) negative `*` precision (`%.*d`, -2) means precision OMITTED, not 0.
  - (5) `%c` now honors the `0` zero-pad flag (`%010c`).
  - (6) **`%v` vector flag implemented**: `%vd`/`%vX`/`%*vX` (separator from arg)/`%v.3X`/`%v02x` — new `sprintf-vector` helper formats each char ordinal joined by sep ("." default). +55 tests.
  Remaining sprintf.t (92): INVALID/REDUNDANT/MISSING warning detection (~warning-tag mismatches), `%N$` positional args, `version->new` objects in `%vd`.
Updated 2026-05-29 (session 215, Part 2). **More list-vs-scalar context fixes** (wantarray work now authorized). do.t 20→10, split.t 9→8, array.t 41→38. Fixes: `split` args scalar; `=~`/`!~` bare-match context wrapper; `return @a`/map=count + `return @slice`/`my $s=@slice`=last (new `p-list-scalar`/`p-slice-result`, `_slice_in_context`, `get_node_context_raw`); `return do {@a}` caller context; string-interp `@a[..]`/`@h{..}` slice+join. New test `Pl/t/list-scalar-context-01.t` (13). **Full sweep NOT re-run after Part 2 — run it next session.**
Updated 2026-05-30 (session 217). **Skip-registry migration: ref.t 35 documented not-supported failures registered** (deref of IO/FORMAT glob slots, \substr/\pos/\vec lvalue refs, ref-to-FORMAT/IO, NUL/UTF-8 symbolic-ref stash names, literal-aliased read-only assignment). ref.t **57→22 fail** (still PARTIAL). Honest sweep total **808 fail** (registry-era counter; −35), **63 fully passing**, no regression. Baseline re-blessed (531 keys). No runtime changes (a `ref(\$ref)`→"REF" fix was attempted but reverted — box-nesting depth doesn't cleanly separate scalar-ref from ref-to-ref through a `my` round-trip).
Updated 2026-05-29 (session 216). **Post-fix sweep: 28604 pass / 896 fail, 60 fully passing** (104 files + 2 skipped; +124 vs the 28480 baseline this session, no fully-passing regression; gate `prove -j8 Pl/t/` green at 81 files / 3040 tests). Three fixes: tr.t /c complement +43, sprintf2.t hex-float-string + positional +69, sprintf.t positional +9, hexfp.t +3. Baseline before fixes was 28480/1020/60 (+13 vs s215p1, confirming the s215 Part-2 net gain). **tr.t /c complement family FIXED: 229→272 passing (+43)** — `do-tr` rewritten to rank each complemented char by its position among all codepoints NOT in the search list (was mapping every complement char to repl[0]); `/cd` delete-past-repl, `/cs` squeeze-translated-only, and `/r` non-destructive return all now correct. New `Pl/t/tr-01.t` (8). Remaining tr.t failures are error/warning-message detection (principle 9) + named sequences/RO strings. **Stale-number corrections this session: infnan.t now only 6 fail (was 396); array.t 38; aassign.t 66; index.t 11; do.t 10; split.t 8; chop.t 5; method.t 39; ref.t 57.**
Updated 2026-05-29 (session 215, Part 1). **28467 pass / 1033 fail, 60 fully passing** (+3 vs s214, no regressions, Pl/t 3013/3013). array.t 115–117 fixed (AASSIGN_COMMON via `our`). Two fixes: (1) `our (...) = (...)` now parses its RHS in LIST context (`Pl/Parser.pm` `_process_our_declaration`), so `our ($x,$y,$z) = (1..3)` generates a range, not a flip-flop (was emitting `p-flipflop-num`, yielding all-empty). (2) short-circuit ops `&&`/`and`/`||`/`//`/`or` now evaluate the LHS in scalar (boolean) context but let the RHS **inherit** the surrounding context (`Pl/PExpr.pm` `child_context`) — `$cond && (list)` and `() || (list)` return the list in list context (matches Perl `@a=(0||@x)` → elements; `@a=(@x||@y)` LHS still scalar count). Regression tests added to `Pl/t/aassign-01.t` (8→11).
Updated 2026-05-30 (session 219b). **`%hash`/`@array` list-flatten fixes + sweep proven on -j8.**
`join(":", %h)`, `for (%h)`, `print @a`, `print %h`, `push @a, %h`, `map/grep … %h` all
stringified the aggregate as `HASH(0x..)`/`ARRAY(0x..)` instead of flattening it in list
context. **`print @a` (a very common op) was fully broken** — print.t never covered bare
`print @array`. Fixes (`cl/pcl-runtime.lisp`): new `%p-hash-keyval-list` helper; `p-join`
(both loops), `%p-flatten-for-list` (foreach), `%p-collect-list` (map/grep), and
`p-push-impl` now flatten raw hashes; `p-print` now flattens its LIST via `p-flatten-args`
(arrays AND hashes; p-box refs stay scalar). state.t 148→151; gate 3092/3092; sweep clean
on **-j8** (16855 pass / 767 fail / 11881 skip, only bop.t+eval.t crash, 0 SIMPLE-FILE-ERROR).
The -j8 "flakiness" was the same relative-faillog bug surfacing under GC pressure, not a
write race (every child writes unique paths). +8 regression tests in `transpile-test-01b.t`.
Updated 2026-06-02 (session 231c). **Two correctness fixes (0 sweep delta, 0 regressions).**
(1) Plain `local $ref->{k} = v` / `$ref->[N] = v` (no delete) mis-bound the scalar `$ref` and
crashed — same arrow-drop family as 231b; the `local`-with-init matcher now accepts the arrow
form (`Pl/Parser.pm`). Not in perl-tests/ but a real crash on valid Perl. (2) `qr//`
stringification dropped modifier flags (`(?^:abc)` for `qr/abc/i`) — the flag check used keys
never present; now reads the upcased flag letters and emits Perl's canonical m,s,i,x order
(`cl/pcl-runtime.lisp`). Gate 91 files / 3189; sweep unchanged 17816/736, 69 fully passing.
Updated 2026-06-02 (session 231b). **`delete local $ref->{k}` arrow-deref + glob-ref numify.**
`delete local $a->{b}` / `$a->[N]` silently dropped the `local` (matcher required Symbol
immediately+Subscript; arrow form is Symbol `->` Subscript) → element never restored
(**local.t 300→302**, 119/120). A glob ref `\*foo` numified to 0 (bare glob and glob ref
both became `box(value=typeglob)`); `p-backslash` now sets `is-ref`, `box-set` preserves it,
`box-nv` returns the address for glob refs (**bless.t 96→99**, 16/52/68). Full sweep
**17816 pass / 736 fail, 69 fully passing** (0 regressions). Gate 91 files / 3182.
Updated 2026-06-02 (session 231). **`\(LIST)` scalar-context refgen (cross-cutting #5) +
`//=`/`||=` signature defaults.** `bless \(map "$_","test"), "C"` blessed an ARRAY ref —
`\(LIST)` in scalar context must yield a SCALAR ref to the LAST element (comma-operator
semantics); fixed in `Pl/ExprToCL.pm` by wrapping `(p-refgen-list …)` in `(p-list-scalar …)`
only when the node's *raw* context is SCALAR/VOID (**bless.t 95→96**). Separately, signature
default operators `$x //= D` / `$x ||= D` (Perl 5.38+) were dropped entirely by
`_parse_signature` (regex only matched `=`) — now captured into `default_op` and emitted with
a definedness (`//=`) / truthiness (`||=`) guard (**signatures.t 780→788**). Full sweep
**17811 pass / 741 fail, 69 fully passing** (sweep-diff 0 new / 8 fixed; baseline 423→415).
Gate 91 files / 3172.
Updated 2026-06-02 (session 230). **`local $#a = N` fixed + TAP `$TODO` harness support.**
`local $#a` (PPI `ArrayIndex`) was silently dropped by `_process_local_declaration`; now
emits the plain length-set (Perl does NOT restore on scope exit — RT #7411 — so neither do
we). **local.t 315/316 pass.** Separately, the harness now honors Test::More `$TODO`
(`cl/pcl-test.lisp` `%current-todo` reads `main::$TODO`; `sweep-perl-tests.pl` counts
`# TODO` as non-fail like prove) — failing-TODO tests across 18 files no longer count as
failures. **Full sweep 17802 pass / 750 fail (was 757), 69 fully passing** (+1 blocks.t),
baseline 423 keys, only bop.t+eval.t crash. **Catalog STILL stale**: chop.t & range.t now
fully pass; split.t 149–151 are chained list-assign-as-lvalue (not package-array); or.t
8–10 are `||`-lvalue/pos-context. Re-triage before trusting per-file notes below.
Updated 2026-06-01 (session 228). **yadayada.t 16→21** (`...` now dies `Unimplemented at $0 line N.` via a runtime-built `:loc`, `Pl/Parser.pm`); **sprintf2.t 1544→1617** (+73; `Integer overflow in format string` guards for `*`-width/`*`-precision/literal-precision, `cl/pcl-runtime.lisp`); **signatures.t 672→780** (+108; three signature-param declaration bugs fixed — params are now mutable boxes, `local $G=RHS` defaults restore on sub exit, and `our $VAR` defaults are declared; see `docs/variable-declarations-spec.md` §4.1/§4.2/§4.3). Full sweep 0 new fails, 66 fully passing. Gate 90 files / 3150. NOTE: this session found the catalog significantly stale — list-of-arrays slice, chop-on-assigned-array, `($a=…) .= 'c'`, and LHS array-slice assignment all already pass.
Updated 2026-05-31 (session 223c). **`crypt()` implemented via FFI; lfs.t permanently skipped.**
`p-crypt` (`cl/pcl-runtime.lisp`) calls the system `crypt(3)` (`libcrypt.so.1`) through
`sb-alien` — byte-identical to Perl (same C function). Latin-1-encoded args, wide-char→die,
NULL→undef. `crypt` added to `RUNTIME_NAMES` (`Pl/ExprToCL.pm`); `d_crypt`→`define`
(`lib/Config.pm`). **crypt.t 0 → fully passing (5/6, test 6 = utf8 flag registered
not-supported).** lfs.t added to sweep `@SKIP` (64-bit lseek/sparse files, XS/platform). **Full
sweep 17341 pass / 1263 fail, 64 fully passing** (+1: crypt.t; 0 regressions, only bop.t+eval.t
crash). Gate 88 files / 3118 tests. New `Pl/t/crypt-01.t` (4). Baseline 489 keys.
Updated 2026-05-31 (session 223b). **Match variables `$&`/`` $` ``/`$'` implemented; signatures.t un-skipped.**
These were fully broken: codegen emitted bare `$'` (CL quote macro) and `` $` `` (quasiquote),
none were defvar'd. Now mapped to pipe-quoted symbols in `%SPECIAL_VARS` (`Pl/ExprToCL.pm`),
defvar'd+exported, and **set on every successful match/subst** via new `set-match-vars`
(`cl/pcl-runtime.lisp`) wired into the single / /g-scalar / /g-list / s/// / s///e paths.
`$+` deliberately NOT in the codegen map (it would hijack `$+{name}` = `%+` named-capture
access). **signatures.t un-skipped** (the "734 eval too slow" note was stale — runs in ~4s via
the persistent pl2cl server, NOT the s219–221 FASL work); two source-tree/not-supported
landmines guarded (missing `regen/keywords.pl`; `:lvalue`-sub + return-in-default RT#132141).
signatures.t now **completes: 418 pass / 559 fail / 1 skip** (was a whole-file skip; the 559 are
arity error-message-text + experimental-warning detection = not-supported/principle-9). **Full
sweep 17337 pass / 1262 fail, 63 fully passing** (held; +418/+559 = signatures now running, 0
non-signatures regressions, only bop.t+eval.t crash). Gate 87 files / 3111 tests. New
`Pl/t/match-vars-01.t` (7). Baseline re-blessed (489 keys).
Updated 2026-05-31 (session 223). **`(LIST) x $n` list-repeat in `return` list fixed.**
do.t 35/36: `return (@a, (@b) x $n)` in list context scalar-repeated `(@b)` (`x` list-vs-
string decision only handled LIST_CTX, but `return` args get INHERIT_CTX). Added an
INHERIT_CTX runtime `*wantarray*` check to the `x` arm of `gen_binary_op`
(`Pl/ExprToCL.pm`), mirroring the `..` path. **Full sweep 16919 pass / 703 fail, 63 fully
passing** (was 16917/705/63; +2/−2, no regression, only bop.t+eval.t crash). do.t 10→8 fail.
Gate 87 files / 3111 tests green. Baseline re-blessed (441 keys). +2 in
`Pl/t/list-scalar-context-01.t`.
Updated 2026-05-31 (session 222). **sprintf "Invalid conversion" cluster fixed.**
Unrecognised conversions (`%C`/`%I`/`%Z`/`%L`/`%h`/`%v`/`%vc`/malformed `%6. 6s`) now
leave the spec verbatim + warn "Invalid conversion" + don't consume an arg + suppress
the trailing "Redundant" (new `sprintf-valid-type-p`, `saw-invalid`/`spec-start-arg` in
`p-sprintf`; `V` added to size-modifiers; root cause was the unconditional type-char
downcase that aliased `%C`→`%c` etc.). **sprintf.t 469→523** (+54, git-stash-verified 0
regressions). Full sweep **16917 pass / 705 fail, 63 fully passing** (unchanged), only
bop.t+eval.t crash. New `Pl/t/sprintf-invalid-01.t` (9).
Updated 2026-05-30 (session 219). **Sweep harness fixed — only 2 genuine crashes left.**
The full sweep had been aborting ~36 files with `SB-INT:SIMPLE-FILE-ERROR` (looked like
mass crashes; pass total slumped to ~7993). **Root cause:** the failure-log writer
(`%test-log-stream`, `cl/pcl-test.lisp`) opened `PCL_TEST_LOG_DIR/<file>.fails.tsv` via a
**relative** path; many perl-tests do `chdir 't'`, so when a *failing* test fired after the
chdir, the relative `.faillog/` dir didn't exist in the new cwd → unhandled file error killed
the whole file. It only surfaced for files whose failing test was GC-nondeterministic
(e.g. array.t 83 "freed array") — hence the "flakiness" memory noted since s216. **Fix:**
(1) `sweep-perl-tests.pl` absolutizes `$log_dir` (relative → `$project_root/$log_dir`);
(2) `%test-log-stream` now `ensure-directories-exist` + `ignore-errors` (a diagnostic
side-channel must never crash a run). **Clean sweep now: 16849 pass / 770 fail / 11881 skip,
63 fully passing** (this is the honest registry-era counter; the old 28604 scored skips as
pass). **Only genuine crashes: bop.t & eval.t** (both not-supported, see below). Baseline
re-blessed (506 keys). +24 fixed vs s217 baseline (array.t holes→skips s218, push.t guards
s218, `\substr`/`map +()` s219), 0 real regressions (lone sprintf `%P` diff is a flaky
pointer address).
Updated 2026-05-28 (session 213). **27996 pass / 964 fail, 60 fully passing. Fixes: (1) v-string without `v` prefix (`256.65.258`): now handled as Version token in ExprToCL; (2) surrogate/non-char Unicode in CL string literals: new `_cl_string_literal()` in ExprToCL uses (concatenate 'string ...) forms; (3) tr/// escape sequences: new `_expand_tr_escapes()` converts Perl escapes before CL embedding — tr.t 40→226 passing; (4) yada yada `...`: detected in `_process_expression_statement`, emits (p-die "Unimplemented"); (5) lib/POSIX.pm stub added: DBL_MAX, math constants, errno, SEEK_* — sprintf.t crash fixed, 1→14 passing. Root cause of remaining 552 sprintf.t failures identified (see session-log.md §213).**

**Session 207 fixes:**
- **time.t** (previous session): `(EXPR)[N]` subscript now forces LIST_CTX on inner expression (`list_ctx_subscript` metadata in PExpr.pm + `gen_array_ref_access` in ExprToCL.pm). `pl-like`/`pl-unlike` now respect regex modifiers (pcl-test.lisp). `times` added to Config.pm `known_no_of_params`. `$ENV{TZ}` limitation documented in not-supported.md.
- **qr.t** (21/37): `box-nv` now returns object-address for p-regex-match structs (not 0). `p-ref` returns "REGEXP" when inner value is p-regex-match (ref to qr// = "REGEXP"). Fixes tests 3, 9, 22, 29.
- **args.t** (18/23): `local(@_)` now correctly handled — `_find_symbols_and_undefs_in_list` now accepts `PPI::Token::Magic` tokens (was Symbol-only; `@_` is Magic). Single array/hash local with init now emits var as default return value in let body.
- **chdir.t** (FULLY PASSING): `p-chdir` now captures `sb-posix:syscall-errno` on failure and stores in `*p-stored-errno*`.
Skips sprintf.t (deprioritized) and Unicode/utf8-encode issues (documented not-supported).

---

## Cross-cutting bugs (high value targets)

### 1. `state ++$var` / `state $var++` — parser drops `state` or the `++` ✅ FIXED (session 172)

**Files:** state.t (tests 77–82) — now passing.

---

### 2. `scalar(%hash)` returns hash-table debug string instead of key count ✅ FIXED (session 175)

**Files:** each.t (tests 47, 53) — fixed. hashassign.t (test 209) — separate issue (group 3).

---

### 3. `%hash = (...)` in list context doesn't return the list ✅ FIXED (sessions 183–185)

**Files:** hashassign.t — 42 → **4 failures** (tests 304, 307–309 = lvalue aliasing, documented not-supported)

---

### 4. `substr` out-of-bounds: no "substr outside of string" warning or error ✅ FIXED (session 174)

**Files:** substr.t — ~38 OOB failures fixed; now 358/397 passing.
Remaining failures (tests 313-397) are lvalue substr and \substr — documented not-supported.

---

### 5. `\(list_expr)` takes ref to ARRAY instead of SCALAR in some contexts ✅ FIXED (session 231)

**Files:** bless.t (test 11 — fixed), ref.t

**Root cause:** `bless \(map "$_", "test"), "C"` — `map` returns a list; `\(LIST)` in
scalar context should take a ref to the last scalar element, giving a SCALAR ref.
PCL emitted `(p-refgen-list …)` (a vector) regardless of context, so a scalar consumer
treated it as an ARRAY ref.

**Fix (session 231):** `gen_prefix_op` in `Pl/ExprToCL.pm` now reads the refgen node's
*raw* context and, only when explicitly SCALAR_CTX/VOID_CTX, wraps the result in
`(p-list-scalar (p-refgen-list …))` — comma-operator semantics (last element). List and
unannotated (list-natural) contexts keep the full vector, so `my @r = \(map…)` still
gets one ref per element. bless.t 95→96.

---

### 6. `p-/` produces CL rational `1/4` instead of float `0.25` ✅ FIXED (session 172)

---

### 7. `infnan.t` — wrong error format for `chr(Inf)` / `chr(NaN)` ✅ FIXED (session 172)

---

### 8. Dynamic loop labels: `last $var` / `next $var` ✅ FIXED indirectly (loopctl.t now fully passing)

---

### 9. `join.t` — `join(undef, list)` ✅ CLOSED (join.t fully passing)

---

### 10. `each.t` ✅ FULLY PASSING (session 200)

---

## Per-file issues (smaller scope, current as of session 201)

Files ordered roughly by remaining failure count.

---

### hash.t (269 failures, 225/494 passing)

Three distinct bug classes:

- **`Hash::Util` bucket statistics** (~225 tests): `bucket_ratio`, `bucket_array`,
  `num_buckets`, `bucket_stats` — all test CL internal hash layout (number of buckets,
  fill ratios). Not implementable: CL hash tables have opaque internals.
  These tests should be treated as documented not-supported.

- **`scalar(%hash)` string format** (~24 tests): In older Perl, `scalar(%hash)` returned
  `"used/total-buckets"` e.g. `"2/8"`. As of 5.26 it returns just the count. Tests that
  compare `scalar(%h) eq scalar(0+keys %h)` still pass, but tests that expect the old
  ratio string fail. PCL returns an integer key count which is correct for 5.26+.

- **Hash copy identity** (~13 tests): `a .. zz (+N copy) has same keys` — copy of hash
  should have identical key set. `@{%copy} == @{%orig}` fails, probably because PCL's
  hash copy iterates differently or misses some keys.

- **DESTROY / weak-ref** (3 tests): GC-based destruction — documented not-supported.

- **`ref hash keys at compile`** (test 3): `ref hash keys are not stringified during
  compilation` — compile-time constant folding check.

- **`magic keys`** (test 493): hash magic keys not implemented.

---

### infnan.t (6 failures, 1106/1112 passing — session 216)

**MOSTLY RESOLVED.** The large clusters below were fixed between sessions 202–215
(pack Inf/NaN error messages, eval-block arithmetic-signal trapping, parse-perl-number
overflow). Only **6 failures remain** as of session 216 — re-triage these individually
(run `./runt infnan` and read the `not ok` lines) before assuming the historical
breakdown still applies. The breakdown below is retained for reference only.

- **`sprintf("%a", Inf)` case** (tests 21, 25, ~2 total): Returns `'inf'` instead of
  `'Inf'`. `%a` format for Inf/NaN should capitalize. Fix: `p-sprintf` special-case
  for Inf/NaN with `%a` format.

- **`pack c/C/s/S/...` with Inf/NaN error messages** (tests 56–167, ~112 failures):
  `pack('c', Inf)` should die `"Cannot pack Inf in pack"`. PCL returns wrong error
  message or undef. Fix area: `_pack_tmpl` in `cl/pcl-pack.lisp` — add Inf/NaN checks
  before integer conversion.

- **`pack W/U` with Inf** (tests 175–207, ~33 failures): `pack('W', Inf)` should die
  `"Cannot pack Inf"`. PCL currently returns empty string. W/U formats don't have
  the Inf guard that integer formats have.

- **`pack p/P` with Inf** (tests 252–255, 4 failures): `pack p Inf` should return
  a pointer to the string "Inf" (not undef). p/P format is disabled (skipped) in PCL.

- **`sprintf` Inf/NaN with various formats** (tests 365–440, ~73 failures): padding,
  width, precision flags with Inf/NaN — edge cases in `p-sprintf`.

- **`parse-perl-number` overflow for `'1e9999'`** (~10 failures): `(ignore-errors
  (read-from-string "1e9999"))` silently catches SBCL's `FLOATING-POINT-OVERFLOW`
  READ-ERROR and returns `nil`, so `parse-perl-number` returns 0 instead of Inf.
  Fix: change to `handler-case` catching `floating-point-overflow` → return
  `sb-ext:double-float-positive-infinity` (or negative Inf if leading minus present).

- **String eval of NaN/Inf** (tests 500+, ~126 failures): `eval { $x + 1 }` with NaN
  raises SBCL `FLOATING-POINT-INVALID-OPERATION` instead of returning NaN. The
  `p-eval-block` trap doesn't catch SBCL arithmetic signals. Needs SBCL condition
  handler inside `p-eval-block`.

---

### sprintf.t (27 failures, 523/552 passing — session 222, was 83)

`p-sprintf` / `sprintf-one` / `sprintf-vector` in `cl/pcl-runtime.lisp`.

- **Invalid conversions** ✅ **FIXED (session 222), +56**: `%C %H %I %J %K %L %M %N %P
  %Q %R %S %T %V %W %Y %Z`, lowercase `%h %j %l %q %t %v %w %y %z`, the malformed
  embedded-space specs `%6. 6s`/`%6 .6s`/`%6.6 s`, and vector-with-non-integer `%vc`/
  `%vf`/`%vs`/`%vp`. Each is now left verbatim, warns "Invalid conversion", doesn't
  consume an arg (arg pointer restored to spec start, so a trailing `%d` re-reads it),
  and suppresses the "Redundant argument" warning. `%Vd` is now `%d` (V = IV/UV size
  flag). See session-log §222.
- **Remaining 27** are hard/niche: `version`-object `%vd`/`%vx` (need real `version`
  objects, 147–152/441/442), `%n` family (271–273/341), `%.0hf` size-modifier
  rejection (227), `%.0g` float edges (231–237), reordered positional+vector
  (482/540/543/546).

- **Reordered positional width/precision** ✅ **FIXED (session 216)**: `%*N$` / `%.*N$`
  drew their value from positional arg N but were emitted literally. New helper
  `%sprintf-star-positional`; both `*` arms in `p-sprintf` now consume a trailing `N$`
  without advancing the sequential arg index. Fixes 332–335 (`%.*2$d`, `%*3$.*2$d`,
  `%3$*2$.*1$d`) and the 674–685 reorder block. +9.

- **INVALID / REDUNDANT / MISSING warning markers** (biggest remaining group, ~40t):
  tests like `>%z< >''< >%z INVALID< >%z REDUNDANT<` expect PCL to (a) warn
  "Invalid conversion", (b) leave the bad spec verbatim in the output, and (c) warn
  "Redundant argument". PCL formats most of these but doesn't reproduce the exact
  marker text the harness compares. Mostly error/warning-detection — borderline
  principle 9; revisit only if the harness's `%REDUNDANT`/`%INVALID` accounting can be
  matched cheaply. NB the harness encodes the *expected warnings* in the data columns,
  so these are not pure "ignore invalid input" cases.

- **`version->new` / `version::qv` objects in `%vd`/`%vx`** (~15t): `sprintf "%vd",
  version->new("1.2")` should print `1.2` using the version object's release components.
  PCL stringifies the object char-by-char (`49.46.50`). Needs real `version` object
  support (the `%v` flag reads each *character ordinal* of the stringification; a version
  object must expose its v-string form). Fix area: `sprintf-vector` + `version` in `lib/`.

- **`%n`** (~3t): stores the running char count back into an argument (and the magic/utf8
  variants). Not implemented; discouraged in modern Perl. Leave unless a CPAN dep needs it.

---

### sprintf2.t (28 failures, 1617/1678 passing — session 228, was 102)

- **Integer overflow in format string** ✅ **FIXED (session 228), +73**: a width/precision
  exceeding a C int (2³¹−1) leaked an SBCL `(UNSIGNED-BYTE 44)` type error instead of Perl's
  `Integer overflow in format string`. The literal-*width* path already guarded this; added
  the three missing sibling guards in `p-sprintf` (`cl/pcl-runtime.lisp`): width-from-`*`,
  precision-from-`*` (via `(abs …)`, so huge-negative IV_MIN errors before the
  negative-precision-omitted rule), and literal `.NNN` precision. One O(1) compare per format
  spec; formatting hot path untouched. Regression test `Pl/t/sprintf-invalid-01.t` (9→13).
- **Remaining 28**: subnormal/denormal `%a` last-hexdigit rounding (`0x0p+0` for tiny
  denormals, ~17t), `%n` (2t), `.=`-on-array-elem (1657), "Numeric format result too large"
  (1673), and a few float-precision edges. Niche float-internals + error detection.

### sprintf2.t (HISTORICAL: 102 failures, 1576/1678 passing — session 216, was 171)

- **String-literal hex-float corruption** ✅ **FIXED (session 216)**: `_preprocess_source`
  in `Pl/Parser.pm` ran the hex/binary/octal-float→decimal regex over the WHOLE source,
  so the string `'0x1p+0'` became `'1'` — corrupting the entire `@hexfloat` data table.
  Fixed by matching a quoted string as the first alternative of each substitution (skip)
  before the float pattern (`($str_re)|0x...p...`). +69 here, +3 hexfp.t. Regression test
  `Pl/t/hexfloat-01.t`.
- **Reordered width/precision positional args** ✅ **FIXED (session 216)**: `%*N$`/`%.*N$`
  (width/precision drawn from positional arg N) were emitted literally. New helper
  `%sprintf-star-positional` in `cl/pcl-runtime.lisp`; both `*` arms in `p-sprintf` now
  detect a trailing `N$`. +9 in sprintf.t (positional reorder block 332–335, 674–685).
- **Remaining 102**: subnormal/denormal `%a` precision (last-hexdigit rounding, `0x0p+0`
  for tiny denormals ~10t), `%.HUGEa` integer-overflow error messages, utf8 invalid-format
  warnings, `%n`. Niche float-internals + error detection.

- **`%a`/`%A` hex float format** ✅ **IMPLEMENTED (session 202)**: `sprintf-one` now has a
  full `(#\a)` arm using `integer-decode-float`. Verified: `printf "%a\n", 3.14` →
  `0x1.91eb851eb851fp+1`. The 171 remaining failures in sprintf2.t are NOT `%a` tests —
  they are: `%NNN$s` positional args, missing/redundant argument warnings, UTF-8 string widths.
  The `%a` test block in sprintf2.t is gated on `$Config{nvsize} == 8` — investigate `$Config`
  support as a next step to enable those gated tests.

- **`%a` precision overflow** (tests 1570–1572, 3 failures): Integer overflow in extreme
  precision values for `%a`.

- **`%n` format** (tests 1577–1578, 2 failures): `sprintf("%n", ...)` — stores character
  count to an argument. Not implemented (and intentionally not in Perl's standard sprintf).

- **`.=` on array element with sprintf** (test 1657, 1 failure): `$ary[3] .= sprintf(...)` —
  probably lvalue context issue on array element .=.

- **Croak for large numeric format** (tests 1673–1678, 6 failures): `sprintf("%7000000000E", 1)`
  should die "Integer overflow in format string". Error message format mismatch.

---

### aassign.t (85 failures, 104/177 failing — PARTIAL, early stop not shown)

- **List-context function return** (test 1): `($a,$b) = f_ret_14()` where `f_ret_14`
  returns `1..4`. Gets `(:)` meaning empty. Root cause: the wantarray/VOID_CTX regression
  from sessions 162–163. **Do NOT fix wantarray issues without explicit user request**.

- **Array alias assignments** (tests 12–16): lvalue aliasing forms — documented not-supported.

- **Tied variable assignments** (tests tied-*): tie interactions with list assignment.

---

### array.t (13 failures, 165/195 passing, 17 skip — confirmed session 230)

Remaining 13 verified session 230: freed-array length (83–88, 100; GC-hard), symbolic-ref
length (109–114), arylen-magic interactions (126, 172). All as the session-218 notes below.


**AASSIGN_COMMON is DONE** — the old catalog claim of "~27 self-assignment failures
needing RHS snapshot in `p-list-=`/`p-array-=`" was STALE. Verified directly (session 218):
`@a=@a`, `(undef,@a)=@a`, `@a=('X',@a,'Y')`, and the `my`/`local`/`our @bee` blocks
(tests 37–62) all PASS (fixed sessions 209 + 215). `p-hash-=` already snapshots before
`clrhash`, so even `my %x = %$x` (the [perl #70171] ref-self-assign) works. **There is no
AASSIGN snapshot bug left.**

**Registered not-supported (session 218, 17 skips, 0 stale)** in `cl/skip-registry.lisp`,
backed by not-supported.md §"Sparse arrays (holes), element aliasing, and SV identity":
- error-detection of non-creatable negative index (`$a[-1]=0`, alias-to-neg-index): 82, 133, 135
- `&PL_sv_undef`/SV identity (127, 128), `@_` alias to nonexistent elem (130, 131)
- sparse-array holes / lazy element creation / map-no-vivify: 174, 176, 179, 181, 184, 189, 191–194

**Remaining 21 = two held-back fix targets (NOT registered):**

- **arylen magic `\$#array` write-through — ✅ DONE (session 218), +6 tests** (92, 95, 98,
  101, 103, 105). Implemented exactly as predicted, via the existing `tie` mechanism. New
  `(defstruct p-magic-cell getter setter)` (sibling of `p-tie-proxy`) in `cl/pcl-runtime.lisp`,
  intercepted at **four** chokepoints (not two — `tie` itself uses all four): `unbox` (call
  getter), `box-set` STORE-arm + the value-copy arm at the `(p-box-p value)` cond (call
  getter so `my $c = $$ref` copies the value, not the cell), `box-sv` and `box-nv` (bypass
  the lazy cache → getter, mirroring tie's FETCH). New `p-arylen-ref` builds
  `(p-backslash (make-p-box (make-p-magic-cell :getter #'(p-array-last-index arr)
  :setter #'(p-set-array-length arr …))))`; codegen: `\$#array` → `(p-arylen-ref X)` (one
  `if` in `Pl/ExprToCL.pm` backslash handling matching `(p-array-last-index X)`). `$$ref`
  read/write and numeric/string contexts all resize the array. **Gate 3052/3052, 0 real
  regressions** (the one flagged sprintf `%P` "regression" is a flaky pointer-address diff).
- **arylen residue still failing (NOT the write-through):**
  - **freed-array (83–88, 100):** the GC-hard corner, as predicted. The magic cell holds a
    *strong* ref to the vector so it never dies → reads stale index, not `undef`. Needs
    `sb-ext:make-weak-pointer` + GC nondeterminism (same family as DESTROY-via-GC). Leave.
  - **symbolic-ref length (109–114):** `$#{@array}`/`$#{$x}`/`$4[8]` (#37350) — a different
    feature (symbolic ref → glob), not arylen write-through.
  - **126** (`arylen_p` magic vs `@ISA` element magic), **172** (arylen aliased in foreach):
    separate magic-interaction sub-problems.
- **BONUS still open:** the generalized `p-magic-cell` (getter/setter closures) now EXISTS and
  is proven — it can back `\substr`/`\pos`/`\vec` and lvalue `substr` (the `:lvalue` skips in
  ref.t 68–73 / substr.t 313–397 / state.t) by emitting analogous magic-cell refs. Next spike.

- **`map +(LIST)` unary-plus parse bug** (tests 118, 121) ✅ **FIXED (session 219)**.
  `map +($_, $h{$_}), LIST` misparsed the leading `+(` (a no-op disambiguator in Perl) as
  unary numeric plus, collapsing the list into `(p-+ (progn …))`. Fix: `gen_prefix_op` in
  `Pl/ExprToCL.pm` now treats unary `+` as a **pure no-op** (perlop: "no effect whatsoever,
  even on strings" — it must NOT numify): it propagates its own context to the operand and
  returns it unchanged, so `+(A,B)` stays a list (→ vector, map flattens) while a single
  `+(EXPR)` is unwrapped from its tree_val (so `print +(2+3)` stays scalar 5, not a 1-vector).
  array.t **163→165** (118,121); substr.t 358→**359**. Regression tests in
  `Pl/t/transpile-test-01b.t` (+5). Gate 3077/3077.

- **List-of-arrays slice** (test 30): `join('', (@foo,@bar)[0..5])` → `'ARRAY(0x..)..'`
  instead of `'acebdf'`. Two arrays in a list + slice: PCL boxes each as an array-ref
  instead of flattening. Fix area: list-context expansion of array vars inside a slice.

---

### ref.t (22 failures, 167/245 passing, 41 skip — PARTIAL, stops at ~230; session 217)

**Session 217: 35 documented not-supported failures REGISTERED** in `cl/skip-registry.lisp`
(stale: 0). Registered clusters — leave these alone (now skips):
- **IO/FORMAT dereference errors** (tests 32–40): `$$`/`@$`/… on `*STDOUT{IO}`/`*STDERR{FORMAT}`
  must die "Not a X reference"; FORMAT unsupported. → :error-msg.
- ~~**ref to substr/pos/vec lvalue** (tests 68–73): "LVALUE"~~ ✅ **IMPLEMENTED (session 219)**
  via `p-substr-ref`/`p-pos-ref`/`p-vec-ref` (`p-magic-cell` kind `:lvalue`). Registry
  entry removed; ref.t 168→174. See not-supported.md "Lvalue subroutines".
- **ref to format / IO refs** (tests 88–90): format/write not implemented. → :error-msg.
- **NUL/UTF-8 symbolic-ref stash names** (tests 134–168): Unicode/NUL stash lookup. → :utf8.
- **literal-aliased read-only assignment / weaken read-only ref** (tests 211, 213–216):
  "Modification of a read-only value". → :read-only.

**Remaining 21 = genuine bugs / gaps (NOT registered):**
- **`ref(\$ref)` ref-to-ref → "REF"** (test 66) ✅ **FIXED (session 217)**. `p-ref` uses the
  `is-ref` flag to find the referent (`inner` when `val` is itself a wrapper, else `inner2`)
  and reports "REF" iff the referent is a wrapper (`\\1`) or *holds* a reference (new
  non-recursive `%scalar-holds-ref-p`, which keeps `*p-undef*`/array-element/`\$qr` cases
  correct and can't loop on `$x=\$x`). Also fixed substr.t 377. Two earlier attempts (naive
  `p-box-p inner2`; recursive-`p-ref` restructure) were reverted after the sweep flagged
  qr.t/index.t/split.t regressions — do not retry those shapes.
- **vstring refs** (tests 64–65): `ref(\v1)` should be "VSTRING".
- **`&{""}` call** (test 21): `ref eval {\&{""}}` should return "CODE". PCL raises error.
- **PVBM ref-type** (tests 178–182), **list-slice deref** (test 177), **sub-ref CL-lambda
  stringification** (tests 171–172), **`-e` vs `-` eval filename** (tests 189–191).
- **Early stop**: investigate tests ~230 onwards (crash to localize).

---

### bop.t (446/510 passing — CRASH at test 496; session 219)

**GENUINE CRASH (1 of the only 2 left after the s219 sweep fix).** bop.t runs to test 495,
then **`pack "P"`** (line 636: `unpack("P2", pack "P", …)`, a memory-layout idiom) dies
`"Invalid type 'P' in pack"` — a **correct** die (P/P pointer pack is not-supported,
not-supported.md §pack pointer types) but thrown in a **bare `for` loop** (no eval), so it
aborts the file and tests 496–510 never run. To recover them, the offending statement must
not abort the process — needs the deferred **per-statement `handler-case` wrapper**
(`docs/test-skip-registry.md` §3.1) turning a top-level die into one `not ok` + continue.
- **Large shift / `use integer` edge cases** (tests 50–125): `4 << 2147483648` should yield 0;
  CL integers are bignums, not 64-bit fixed-width. Documented not-supported.
- **UTF-8 flag operations** (test 158): bitwise ops with UTF-8 flagged strings — `use bytes`.
- **`~.` string complement with number** (tests 172, 186): `~."string"` where the operand
  is a number — PCL returns large integer instead of complement string.
- **Duplicate warning suppression** (tests 202, 215): `no warnings 'uninitialized'` should
  prevent repeated warnings.
- **Glob bitwise ops** (tests 320–328): `*STDOUT | "string"` — bitwise ops between glob and string.

---

### eval.t (15/169 passing — CRASH at test 29; session 219)

**GENUINE CRASH (2 of 2).** Runs to test 28, then top-level **`die if $@`**
(`(P-IF $@ (P-DIE))`) re-throws and aborts — because a preceding **string `eval "…"`**
that succeeds in real Perl FAILS in PCL (the string-eval **lexical-scope** gap: the
`pl2cl` subprocess can't see the caller's lexicals), leaving `$@` set. So the crash is a
*consequence* of the string-eval limitation compounded by an uncaught top-level die. Same
remedy as bop.t (per-statement `handler-case` wrapper would let tests 29–169 run, most
still failing on the lexical-scope gap but no longer aborting). The lexical-scope gap
itself is HARD (would need in-process eval with caller-env capture, not a subprocess).

---

### caller.t (53 failures, 12/112 passing — PARTIAL)

Mostly not-supported — see `docs/not-supported.md`. Caller returns `"(unknown)"` filename and line 0.

---

### method.t (39 failures, 95/163, 26 skip — PARTIAL, runs 160/163; confirmed session 230)

Bulk of the 39 is **error-message-text** (principle 9: 6/8/10/12, 68/69, 90/102, 116–118,
125/128/129, 158) and the **AUTOLOAD-via-@ISA** cluster — both hard. Genuinely discrete,
plausibly-fixable rows worth a look: test 40 (`got '#(C::d NIL)'` exp `'1'` — list leaking
where scalar expected), test 121 (`got 'arg'` exp `'arg b'`), tests 130/131 (typeglob method
call returns a box-wrapped struct instead of the value/REF). Clusters below still hold:
- **`&$one()` where `$one=1`** (tests 5–12): PCL raises "Not a CODE reference" instead of
  "Undefined subroutine &main::1 called". Error message mismatch.
- **AUTOLOAD via @ISA chain** (tests 40–58): method resolution with AUTOLOAD + goto &$AUTOLOAD
  returns wrong result across multi-level inheritance.
- **`SUPER` inside moved package** (tests 77–78): `$obj->Bminor::SUPER::test(...)` fails.
- **DESTROY/AUTOLOAD interaction** (tests 100, 103, 114–115): documented (GC doesn't call DESTROY).
- **`UNIVERSAL::AUTOLOAD`** (tests 97–99): `$AUTOLOAD` scoping across packages.
- **Error message format** (tests 116–118): `"new{}"` in error message.
- **Method call on typeglob** (tests 128–131): `*glob->method`.

---

### do.t — SUPERSEDED, see the "do.t (8 failures … session 223)" entry below

(The old "20 failures, 53/73 passing" analysis here is obsolete — `return do {}` scalar
context and the list-context cases were fixed in sessions 215/223. Current state: 8
failures; see the later do.t entry.)

---

### local.t (16 failures, 300/319 passing, 3 todo-skip — session 230)

- **`local $#a = N`** (old tests 315/316) ✅ **FIXED (session 230)**. PPI tokenizes `$#a`
  as `PPI::Token::ArrayIndex`, unrecognized by `_process_local_declaration` (Symbol/Magic/
  List only) → statement silently dropped. New ArrayIndex branch emits a plain length-set,
  **no save/restore** — Perl does NOT restore `local $#a` on scope exit (RT #7411; the
  "after local … should be restored" rows are `local $::TODO`, now correctly TODO-skipped).
- **`local $a` in `if` condition + TODO** (tests 317–319): RT #7411/#7615 — `local $::TODO`
  rows; now emitted as `# TODO` and counted as expected non-failures (session 230 harness).
- **`delete local $ref->{k}` / `$ref->[N]`** (tests 119–120) ✅ **FIXED (session 231b)**.
  The arrow-deref form (`my $b = delete local $a->{b}`) was not matched by either
  delete-local matcher (both required Symbol immediately followed by Subscript), so the
  `local` was dropped and the element never restored on scope exit.  Added an arrow branch
  in `_process_local_declaration`'s standalone and `my VARS =` paths (container = unboxed
  referent).  (The catalog's old "array size after local($a[5])" description was wrong —
  these are the hashref-element `delete local` rows at source lines 247–266.)
- **`local $_` with filetest/match** (tests 255–264): `local $_` interactions with
  filetest operators and pattern matching on default `$_`.
- **package-name reported unqualified** (tests 237, 240): got `'foo'`, expected
  `'main::foo'` — a qualified-name lookup gap (not yet investigated).
- **`local *{$pkg}{method}`** (tests 271–278): temporarily replacing a method via stash
  slot — not supported.

---

### pos.t (16 failures, 14/30 passing)

- **`pos()` set inside `//g`** (test 4): PCL's `pos()` tracking doesn't update correctly
  inside a `//g` loop when using `$&` or other side effects.
- **Lvalue pos DESTROY** (test 9): documented not-supported (GC).
- **`pos` refuses @array / %hash** (tests 10–11): error detection — principle 9.
- **`pos *glob`** (test 12): `pos` on a typeglob not supported.
- **`pos` through defelem** (tests 13–21): accessing/setting pos through aliased array
  elements (defelem). PCL returns undef. Defelem not fully supported.
- **`use bytes` pos** (tests 26, 29–30): byte-offset vs char-offset — `use bytes` not supported.

---

### state.t (9 failures, 153/166 passing — PARTIAL; session 225)

**Fixed in session 200** (4 more tests):
- Test 36: `p-post++` `*p-undef*` check fixed.
- Tests 38–40: `state %x = qw(...)` init codegen fixed.

**Remaining:**
- **State hash `:shared`** (tests 41–43): `%f :shared` attribute.
- **Computed goto with state label** (tests 70–73): `goto state $label` — not implemented.
- **State in map/grep** (tests 74–75) ✅ **FIXED (session 225)**. `state $x = EXPR` used as a
  block/tail expression returned the init-guard result (`1` first call, `nil` after) instead of
  `$x`'s current value. Both state handlers (`_process_toplevel_state_declaration`,
  `_process_state_declaration` in `Pl/Parser.pm`) now emit the declared variable as the trailing
  form for single-var declarations, so `map { state $x = $_ }` and a sub whose tail is
  `state $y = $v` yield the (persistent) variable value. Regression tests in `Pl/t/state-01.t`
  (26–27).
- **Reference to state variable** (test 76): `\state $x` gives same address each call.
- **Lvalue substr as state** (tests 83–92): documented not-supported.

---

### qr.t (16 failures, 21/37 passing)

- **Regex object identity** ✅ FIXED (session 207): `p-regex-match-p` now handled in `box-nv` — qr// objects return unique addresses for `+0` comparison.

- **`ref(\$qr)` returns "REGEXP"** ✅ FIXED (session 207, tests 22, 29): `p-ref` now returns "REGEXP" when the inner value is a p-regex-match struct.

- **Scalar aliasing after bless** (test 6): `my $b1 = $b; bless $b, 'Pie'` — `$b1` should also be Pie. Scalar assignment copies in PCL, can't share identity.

- **PVLV deref `${qr//}`** (tests 11, 12, 16, 24, 25, 27, 31, 32, 37): `${qr//}` should return a magical PVLV that stringifies to the pattern. Complex. Not supported.

- **Stringification of blessed qr// objects** (tests 14, 18): `"$qr"` should match `Foo=REGEXP(0x...)`. Not supported.

- **DESTROY via GC** (test 36): documented not-supported.

---

### index.t (1 failure, 119/120 passing; 10 utf8::encode tests registered not-supported - session 224)

**Session 224:** tests 49-58 (the `utf8::encode` octet-vs-char-offset cluster) registered
in `cl/skip-registry.lisp` (:utf8) - PCL has no per-scalar UTF-8 flag, CL strings are
always Unicode. NUL-search tests (the old "tests 63-72" bullet) now PASS. Only **test 111**
("index respects changes in ref stringification" - the `""` overload must fire when
`index` stringifies a blessed scalar-ref constant) remains as a real fix target.

- **`utf8::encode` octet-mode index** (tests 49–58, ~10 failures): After `utf8::encode($s)`
  the string is raw bytes. `index($encoded, $encoded_pattern)` should find at byte position,
  not character position. PCL treats everything as characters.

- **NUL character search** (tests 63–72, ~10 failures): `index($str, "\0")` returns -1 when
  it should find the NUL byte. CL strings are NUL-safe but PCL's `p-index` may stop at NUL
  or use a C-string-based search. Fix: use CL's `search` on character sequences.

---

### length.t (12 failures, 35/49 passing — PARTIAL, 2 tests unreached)

- **`use bytes; length(unicode_str)`** (tests 7–23): not supported.
- **Overloaded `length`** (tests 35, 41, 43): ✅ FIXED (session 214). `p-length` now calls
  `to-string` on the original boxed value (not the unboxed inner), so a blessed object's
  `""` overload fires. `length($obj)` where `""` returns undef → 0; returns "hello" → 5.
- **`length(undef)` on a tied scalar** (test 34): tie FETCH returns `''` not `undef`
  (`undef $u` on a `Tie::StdScalar`). Tie semantics, not the plain-undef path (which works).
- **Missing "uninitialized" warnings** (tests 36, 42): when the `""` overload returns undef,
  Perl warns "Use of uninitialized value". PCL under-emits uninit warnings generally (a
  cross-cutting feature); not emitted here. Adding it risks breaking test 202 (warning count).
- **Tests 48–49 not reached**: depend on `charset_tools.pl`.

---

### substr.t (~39 failures, ~358/397 passing)

- **Out-of-bounds warning/error**: ✅ FIXED session 174.
- **Lvalue substr** (tests 313–397): documented not-supported.

---

### range.t ✅ FULLY PASSING (158/162, 4 skip — re-triaged session 230)

**Catalog was stale** — the "18 failures" entry below was long obsolete; range.t now
runs clean (only the bignum-bounds / not-supported rows are skipped). The LHS-array-slice
(test 4), `/e`-range-context (15/17), and scalar-range counting bugs all already pass.
History retained for reference only:

- ~~**LHS array slice in list assignment** (test 4)~~ — already passes.
- ~~**Range in `/e` eval** (tests 15, 17)~~ — already passes.
- ~~**`scalar range`** (tests 15–16)~~ — already passes.
- **Bignum range bounds not rejected** (tests 78–118): `(9223372036854775808..10)`
  should die "Range iterator outside integer range". SBCL bignums don't overflow 64-bit
  bounds — documented not-supported (skipped).

---

### readline.t (5 failures, 31/36 passing — session 220)

- **`pipe` + `alarm`/`$SIG{ALRM}` implemented** ✅ **FIXED (session 220), +5 tests** (16, 19–22).
  `p-pipe` was a no-op stub; now creates a real OS pipe via `sb-posix:pipe`, wrapping both
  ends as unbuffered utf-8 `fd-stream`s bound to the `$in`/`$out` boxes (or bareword FHs).
  Implementing `pipe` exposed a latent hang: test 17 does `alarm 1; readline $in` on an empty
  pipe, and `p-alarm` was a no-op so the blocking read never returned. `p-alarm` now schedules
  a real `SIGALRM` (`sb-posix:alarm`) and a lazily-installed Unix handler dispatches to the
  Perl `$SIG{ALRM}` handler — the handler's `die` unwinds out of the interrupted read.
  `%p-syswrite-impl` is now unbuffered (`finish-output`) + encode-safe (`handler-case`). The
  utf-8 pipe round-trips both ascii (test 19) and Unicode (tests 20–22) content. Regression
  tests in `Pl/t/fileio-02.t` (+3). Neither `pipe` nor `alarm` was a documented not-support.

- **Read-only modification** (test 1): error message format mismatch for
  `readline()` on read-only value.

- **`<>` autovivification** (tests 26–27): `<>` and `readline` should not autovivify
  a scalar when the filehandle doesn't exist.

- **`tell()` after GV unglobbed** (tests 28–30): `tell()` should return -1 after the
  last typeglob pointing to a filehandle is destroyed. PCL returns 0.

- **Error message on unopened FH** (test 32): `readline()` on unopened FH — error
  message format `"readline() on unopened filehandle y"` not matching.

---

### bless.t (~11 failures, 95/106 passing)

- **`\(map ...)`** (test 11) ✅ **FIXED (session 231)**: see cross-cutting bug #5.
- **glob-ref numeric value** (tests 16, 52, 68) ✅ **FIXED (session 231b)**: `\*foo` numified
  to 0 (couldn't distinguish a bare glob from a glob ref — both `box(value=typeglob)`).
  `p-backslash` now sets `is-ref` on a glob-ref box; `box-nv` returns the address when set.
- **C3 rebless-in-place** (tests 61/62, 46–48): `my $c1 = bless $c1, "C3"` doesn't rebless the
  outer `$c1` — the class is stored on the wrapper box, not the referent (the same scalar-
  identity-copy limitation as qr.t 6). Documented/hard.
- **`\substr` lvalue ref** (tests 26–28): documented not-supported.
- **POSIX errno values** (tests 77–78): `POSIX::EINVAL` gives wrong errno string.
- **Bless-into-ref detection** (test 101): `bless $obj, $ref_ref` should die.

---

### grep.t (FULLY PASSING - 77/77, 6 registered not-supported - session 224)

**Session 224:** all 6 failures registered in `cl/skip-registry.lisp`. Tests 69/71/73
("grep void/scalar/list post") and 75/76 ("block map void 2/3") are DESTROY-via-GC
(blessed objects must be DESTROYed when refcount hits 0; PCL never calls DESTROY via GC).
Test 61 is the compile-time "Missing comma after first argument to grep" error-detection
case (principle 9). not-supported.md: "DESTROY called by garbage collector".

- **DESTROY in grep** (tests 69–76): documented (GC doesn't call DESTROY).
- **Invalid grep syntax** (test 61): error detection — principle 9.

---

### split.t (8 failures, 185/193 passing — re-triaged session 230)

- **Unicode whitespace separator** (tests 136–138): `split(' ', $str)` with Unicode
  whitespace (e.g. U+2000) — PCL's awk-split is ASCII-whitespace only. Needs Unicode `\s`
  in CL-PPCRE (a separate, known cl-ppcre gap; `split ' '` itself is Unicode-aware since
  s218 but the regex `\s` path isn't).

- **Chained list-assignment-as-lvalue** (tests 149–151) — **catalog mislabeled these
  "package-qualified array"; they are not.** The construct is `(@a = split //, "abc") = 1..10`
  (also the `local @a` and `@{\@a}` stacked variants). In Perl a list assignment used as an
  lvalue yields N element-slots where **N = the count of the inner RHS** (3 here), so the
  outer `= 1..10` fills only 3 → `@a = (1,2,3)`. PCL treats the inner result as a whole-array
  lvalue and assigns all of `1..10`. Niche double-list-assignment lvalue semantics.

- **`/e` re-eval count** (tests 153, 155): `split(/(?{ $n++ })/, ...)` — regex code blocks
  `(?{...})` are not-supported (CL-PPCRE has no mid-match Perl callback); see not-supported.md.

---

### tr.t (45 failures, 272/317 passing — session 216, was 88 failing)

- **`/c` complement family** (tests 17–95) ✅ **FIXED (session 216)**. `do-tr` in
  `cl/pcl-runtime.lisp` rewritten: complemented chars are ranked by codepoint position
  among all non-search codepoints (`%tr-from-index`), mapped positionally into the
  replacement list (last char repeats past the end); `/cd` deletes past repl end; `/cs`
  squeezes only translated runs (pass-through chars break the run); `/r` returns the
  transliterated copy without mutating the box. Regression test `Pl/t/tr-01.t`.

- **Remaining 45 failures are mostly error/warning detection** (principle 9 — out of scope,
  candidates to comment out after user OK):
  - Tests 5, 6, 154, 161, 167, 250–251: error messages (named sequence in tr, reversed/
    min>max range, bad LHS, zero-length read-only string).
  - Tests 129–131: `/r` error in `!~`, void-context warnings.
  - Tests 258–305 (RT #130198, ~30 tests): `chop(tr/a/a/)` / `chomp(...)` should die
    "Can't modify transliteration (tr///) in chop" — error detection of invalid lvalue.
  - Tests 223–224: non-modifying tr/// on a scalar ref (shouldn't stringify the ref).
  - Test 257: `tr// of \N{name}` for upper-Latin1 — named char escape in tr.

---

### do.t (8 failures, 64/73 passing, 1 skip — session 223 brought 10→8)

See `memory/project_wantarray_followup.md`.
- **`return (do{}, (do{}) x N)` list context** (tests 35/36) ✅ **FIXED (session 223)**. The
  `x` repetition op's LHS-is-parenthesized list-repeat decision (`gen_binary_op` in
  `Pl/ExprToCL.pm`) only fired for `$ctx == LIST_CTX`. In a `return` list each element gets
  **INHERIT_CTX**, so `(@b) x $n` fell through to `p-str-x` (scalar repeat → scalar(@b)
  count). Added an INHERIT_CTX arm mirroring the `..` path: emit a runtime
  `(if (eq *wantarray* t) (p-list-x …LIST-gen…) (p-str-x …SCALAR-gen…))`, generating the
  parenthesized LHS in both list and scalar context. Regression tests in
  `Pl/t/list-scalar-context-01.t` (+2).
- Remaining: 63–68 (`do subname(arg)` vs `do subname("arg")` syntax distinction), 70
  (RT 124248), 73 (EISDIR on `do dir`).
- NOTE: a separate pre-existing parser-precedence bug exists — `return (X) x N` (no
  comma, single paren) parses as `(return X) x N` instead of `return((X) x N)`. The
  comma-list form do.t needs works; this degenerate form is left as-is.

---

### time.t ✅ FULLY PASSING (session 207)

---

### chop.t ✅ FULLY PASSING (96/100, 4 skip — re-triaged session 230)

**Catalog was stale.** `chop(@stuff = @stuff)` (old test 35) now passes; the lvalue-error
and chomp-aliasing rows are skipped (registered not-supported / principle 9). No fix
targets remain here.

---

### args.t (5 failures, 18/23 passing)

- **`splice(@_, 0, 0, 'x')`** (tests 3–4): `splice` on `@_` inside a function — modifies
  `@_` but changes aren't visible through lvalue args. Got `'4'` instead of `'a b c x'`.

- **`goto &sub` with multiple @_ elements** (tests 7–8): `goto &sub` with a modified `@_`
  containing multiple elements — same @_ aliasing issue.

- **`local @_`** ✅ FIXED (session 207, tests 12, 15, 18): `_find_symbols_and_undefs_in_list` now handles `PPI::Token::Magic` (`@_` is Magic not Symbol). Single array local with init emits var as default return value.

- **`delete $_[0]`** (test 23): `delete $_[0]` outside a block should set element to undef.

---

### chdir.t ✅ FULLY PASSING (session 207)

---

### concat2.t (2 failures, 2/4 passing — re-triaged session 230)

Both failures need **`use overload`** support (operator overloading), not plain concat:
- **Test 1** (`'"\xff\x{101}\x{101}"'`): `package o` overloads `""`; the test does
  `$x .= chr 257` twice on a blessed object and checks the result isn't "confused by
  changing utf8ness". Needs `""`-overload-aware `.=`. (got `'ÿ'`, exp `'ÿāā'`.)
- **Test 2** (RT #132385, got `'REF(0x..)REF(0x..)'` exp `'AB'`): `package RT132385`
  overloads `.` to `push @a, \$_[1]; $_[0]`; `$o . "A" . $o . 'B'` must give each const
  TEMP a distinct SV so `\$_[1]` refs deref back to "A"/"B". Needs overloaded `.` + per-call
  distinct argument SVs. Both are overload-machinery gaps, not utf8 (catalog was wrong here).

---

### closure.t ✅ FULLY PASSING (50/50 — session 227)

- **Nested closure with `my $i = $i`** (tests 11, 13) ✅ **FIXED (session 227)**.
  `sub bizz { my $i=7; if(@_){my $i=shift; sub{...}} else {my $i=$i; sub{...}} }`.
  The else branch's `my $i = $i` closure returned `undef` instead of 7. Root cause was
  NOT the RHS-scope theory in the old note — the else branch codegen was already correct
  (`$i__lex__3 = $i__lex__1`). The bug: `Pl::BlockAnalyzer::_collect_declarations` recursed
  into if/else/while/for **bodies** and hoisted their `my $i` to the parent sub level at the
  compound statement's index. The two-phase scoped block then opened a *second*
  `(let (($i__lex__1 (make-p-box nil))))` wrapping the `if` — reusing the outer var's
  closure rename and shadowing the `7` with `nil`. Fix: compound-statement bodies now bubble
  up only `state` vars (like bare blocks); their `my` vars are scoped by the body's own
  `_with_declarations`. Regression test `Pl/t/closure-01.t` test 16; `Pl/t/block-analyzer-01.t`
  test 8 updated to assert the no-hoist behavior.

---

### or.t (2 failures + 1 todo-skip, 11/14 — re-triaged session 230)

- **`$tied || $var`** (the `local $TODO = 'Double FETCH'` row): now emitted as `# TODO`
  and counted as an expected non-failure (session 230 harness). Tied double-FETCH.
- **`||` propagates lvalue/pos context** (tests 9–10): `for (pos $x || pos $y) { $_++ }` —
  `||` should pass lvaluish context to whichever side it returns, so `$_` aliases the live
  `pos()` slot and `$_++` mutates it. PCL doesn't propagate lvalue context through `||`, so
  `pos()` stays undef. Niche (foreach-aliasing + pos-lvalue through `||`).

---

### push.t (1 failure, 31/32 passing — session 218)

- **Push onto invalid target** (tests 4–6) ✅ **FIXED (session 218)**. `p-push-impl`
  (`cl/pcl-runtime.lisp`) now guards its first arg: a non-array literal dies "Type of arg 1
  to push must be array (not constant item)" (matches `qr/must be array/`); a scalar/ref
  dies "Experimental push on scalar is now forbidden". Previously a raw CL type error (a
  Lisp `#S(P-BOX …)` struct dump) leaked into `$@`. Regression-safe: only the already-erroring
  non-vector path changed.
- **Croak on readonly array** (test 32): read-only arrays not marked — documented
  not-supported (read-only scalars / Internals). Same as unshift.t 19.

---

### repeat.t (3 failures, 45/48 passing)

- **Void context list repeat** (test 43): `(...)x... in void context in list (via scalar
  comma)` — context classification edge case.
- **Lvalue aliasing in repetition** (tests 46–47): `\$_[0] == \$_[1]` when @_ aliases
  elems repeated by x — documented not-supported (@_ aliasing).

---

### delete.t (3 failures, 53/56 passing)

- **`\delete $h{key}` address equality** (tests 26, 54): `\(values %a)` == `\$a{bar}` ==
  `\delete $a{bar}` — all three should give the same address. PCL's delete returns a
  copy, not the same slot.
- **DESTROY on deleted element** (test 56): GC-based DESTROY — documented not-supported.

---

### hashassign.t (4 failures, 305/309 passing)

- **Remaining 4 failures** (tests 304, 307–309): lvalue aliasing into RHS list —
  documented not-supported.

---

### flip.t (1 failure, 13/14 passing)

- ✅ **FIXED (session 211)**: `return /3/../5/` recursion flip-flop state sharing (test 13).
  Root cause: `..` with INHERIT_CTX (from `return`) + non-literal operands generated `p-..` (range)
  instead of `p-flipflop`. Fix: INHERIT_CTX + non-literal operands → always use flip-flop.
  New `p-flipflop-dyn`/`p-flipflop-dyn-3` macros added for string-literal operands (compare
  against `$.` numerically). Integer literals → `p-flipflop-num`. String literals → `p-flipflop-dyn`.
  Variables/expressions/regex matches → `p-flipflop` (boolean evaluation, no `$.` comparison).

- **String flip-flop warning count** (test 10): `"foo".."bar"` in scalar context should
  generate 2 "isn't numeric" warnings (one for "foo", one for "bar"). PCL generates 0.
  The `p-flipflop-dyn` macro correctly uses `p-==` which calls `to-number`, but `parse-perl-number`
  silently returns 0 for non-numeric strings without warning. Fix: `parse-perl-number` needs to
  generate "isn't numeric" warnings for strings with no leading numeric content.

---

### concat.t (1 failure, 233/225 passing — note: extra tests exist)

- **`($a = expr) .= 'c'`** (test 220): `($a = 'A'.$b) .= 'c'` — assignment expression
  used as lvalue for `.=`. Expected `"Abc"`, got `"ab"`. PCL doesn't treat the result
  of `=` as an lvalue. Fix: `p-.=` / assignment chain to detect `(var = expr) .= rhs` form.

---

### undef.t (3 failures, 32/35 passing)

- **Modification of read-only value** (tests 16–17): `undef $constant` should die
  "Modification of a read-only value attempted". PCL gives `''` (no error).
- **`undef &tcp_proto`** (test 18): `undef &Socket::tcp_proto` — undefining a function
  slot. PCL generates "function undefined" error instead of silently setting it to undef.
  Fix: `p-undef-func` or similar in runtime.

---

### unshift.t (1 failure, 18/19 passing)

- **Croak on readonly array** (test 19): `unshift @readonly, ...` should die "Modification
  of a read-only value attempted". PCL doesn't mark arrays as readonly.

---

### wantarray.t (1 failure, 27/28 passing)

- **Void context through `||`** (test 11): `sub f { $false || context(shift) }; f('V')` —
  should see void context at `||` RHS. PCL propagates scalar context instead.
  Do NOT fix — wantarray regression area. See `docs/wantarray-context.md`.

---

### vec.t (2 failures, 76/78 passing)

- **`my $foo` hoisted across `eval {}`** (tests 25–26): `eval { my $foo = vec($foo,...) }`
  — PCL hoists `my $foo` to the outer block, shadowing file-level `$foo`. Documented in
  session-log §189. Fix: don't hoist declarations across `eval {}` boundaries.

---

### grent.t (1 failure, 2/3 passing)

- **Parallel test ordering** (test 2): "not necessarily serious: run t/op/grent.t by itself".
  Race condition in parallel sweep only.

---

### Hexfp.t (21 failures, 104/125 passing)

- PPI can't parse `0x1.8p-1` hex float literals — documented not-supported.
  Remaining failures all involve hex float literals in source code.

---

### sort.t ✅ FULLY PASSING
### join.t ✅ FULLY PASSING
### loopctl.t ✅ FULLY PASSING
### for.t ✅ FULLY PASSING
### my.t ✅ FULLY PASSING
### chr.t ✅ FULLY PASSING
### ord.t ✅ FULLY PASSING
### do.t — listed above
### splice.t ✅ FULLY PASSING
### reset.t ✅ FULLY PASSING
### vec.t — 2 failures listed above
### flip.t — 1 failure listed above
### wantarray.t — 1 failure listed above
### auto.t ✅ FULLY PASSING
### pos.t — listed above
### qr.t — listed above
### readline.t — listed above

---

### Zero-passing files (UPDATED session 223b — none remain a black hole)

- **crypt.t** ✅ **FULLY PASSING (5/6, 1 skip) — session 223b**: `crypt()` now implemented via
  FFI to the system `crypt(3)` (`libcrypt.so.1`); byte-identical to Perl. Test 6 (utf8 flag)
  registered not-supported.
- **lfs.t**: large-file support (64-bit `lseek` offsets + sparse files) — XS/platform feature,
  self-skips (`1..0`). **Permanently in the sweep `@SKIP`** (session 223b); no recoverable tests.
- **signatures.t** — **un-skipped, now completes 418 pass / 559 fail / 1 skip (session 223b)**.
  Was a stale whole-file skip ("eval too slow" — false, runs ~4s). The 559 fails are arity
  error-message-text + experimental-warning detection (not-supported / principle-9).
- **test-pack-new.t / test_ref_pass.t**: REMOVED (no longer in `perl-tests/`); stale entries.

---

## Documented not-supported (no fix needed)

- `?pat?` one-match regex (reset.t)
- `@_` aliasing (pos.t defelem, aassign.t alias tests, repeat.t 46–47)
- `lvalue substr` (state.t, substr.t, bless.t)
- DESTROY via GC (ref.t, grep.t, bless.t, delete.t test 56, hash.t)
- `use bytes` (chr.t, pos.t 26/29/30, length.t 7–23)
- Error detection for invalid Perl (for.t 131–138, my.t 53–59) — per principle 9, comment out
- Hash bucket internals (`Hash::Util` bucket_ratio etc.) — not implementable in CL
- Hex float literals `0x1p-2` — PPI misparse

---

## Priority ranking (open items, session 207)

Ordered by estimated fixable test count, excluding pack.t (separate plan).

| # | Bug | Files affected | Est. impact |
|---|-----|----------------|-------------|
| 1 | `%a` hex float format in sprintf | sprintf2.t, infnan.t | ~170 tests |
| 2 | `infnan.t` SBCL arithmetic signal in eval-block | infnan.t | ~126 tests |
| 3 | `Hash::Util` bucket stats — document/skip | hash.t | ~225 tests (all not-supported) |
| 4 | `gmtime` large/negative timestamps | time.t 47–70 | ✅ FIXED (time.t fully passing) |
| 5 | NUL character search in `index`/`rindex` | index.t 63–72 | ✅ FIXED (already passing) |
| 6 | `index` in utf8::encode octet mode | index.t 49–58 | ~10 tests |
| 7 | AASSIGN_COMMON: `@a = @a` snapshot RHS | array.t 33–62 | ~27 tests |
| 8 | `pack c/C/s/...` Inf/NaN error messages | infnan.t 56–167 | ~112 tests |
| 9 | `do {}` scalar context at sub return | do.t 17–38 | ~14 tests (wantarray-adjacent) |
| 10 | method AUTOLOAD chain + goto &$AUTOLOAD | method.t 40–58 | ~15 tests |
| 11 | `ref($qr)` returns "REGEXP" | qr.t | ✅ PARTIALLY FIXED (session 207, +3 tests) |
| 12 | `gmtime(0)` scalar year format | time.t 43,45 | ✅ FIXED (time.t fully passing) |
| 13 | `local @_` in sub not properly scoped | args.t 12/15/18 | ✅ FIXED (session 207) |
| 14 | `~.` string complement on number | bop.t 172, 186 | ~2 tests (PPI parses wrong, hard) |
| 15 | `\(list_expr)` → ARRAY not SCALAR | bless.t, ref.t | ~3 tests |
| 16 | `state $x` in map/grep block | state.t 74–75 | ~2 tests |
| 17 | Glob bitwise ops (`*STDOUT \| "str"`) | bop.t 320–328 | ~9 tests |
| 18 | `chdir('')` not setting `$!` | chdir.t 28, 34 | ✅ FIXED (session 207) |

### All previously listed cross-cutting bugs — FIXED or CLOSED

| Bug | Resolution |
|-----|-----------|
| `state ++$var` parser | ✅ FIXED session 172 |
| `scalar(%hash)` key count | ✅ FIXED session 175 |
| `%hash = (...)` list-context return | ✅ FIXED sessions 183–185 |
| `substr` out-of-bounds warning | ✅ FIXED session 174 |
| `p-/` CL ratio not float | ✅ FIXED session 172 |
| `chr(Inf/NaN)` error message | ✅ FIXED session 172 |
| `join(undef, ...)` test 18 | ✅ FIXED session 175 |
| `p-sort` inplace sort | ✅ FIXED session 184 |
| `while (my ($k,$v) = each %h)` | ✅ CLOSED (each.t now fully passing) |
| Dynamic loop labels `last $var` | ✅ CLOSED (loopctl.t now fully passing) |
| `p-list-=` list-context return | ✅ FIXED sessions 183–185 |
| `tail_position` leaked in `gen_funcall` | ✅ FIXED session 184 |
| Sort comparator wrong context | ✅ FIXED session 184 |
| splice.t flip-flop LIST_CTX | ✅ FIXED sessions 182–183 |
| eval-block list context (pack.t Group A) | ✅ FIXED session 199 |
| time.t — `(EXPR)[N]` subscript LIST_CTX, `times` bareword, regex modifiers in `pl-like` | ✅ FIXED session 207 |
| chdir.t — `$!` not set on failed `chdir('')` | ✅ FIXED session 207 |
| args.t — `local(@_)` not localized (Magic token not found) | ✅ FIXED session 207 |
| qr.t — qr// object numeric address was 0 | ✅ FIXED session 207 |

### Session 200 fix: `(pcl:p-defpackage ...)` pre-declaration

**Bug:** `_assemble_output()` in `Pl/Parser.pm` emitted `(defpackage :PKG (:use :cl :pcl))`
for cross-package symbol pre-declarations, triggering SBCL "MAIN also shadows" warnings.

**Fix:** Changed to `(pcl:p-defpackage $cl_pkg)` (warning-suppressed, idempotent).
Updated `Pl/t/decl-ordering-01.t` test 23 regex accordingly.
