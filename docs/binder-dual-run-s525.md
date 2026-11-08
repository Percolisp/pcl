# The binder in dual-run (s525q, 2026-10-24)

Parser-structure program, Option 1 step 0, member **c2** (`docs/parser-review-s525.md`
§5 box, step 0 second half; design `docs/var-handling-review-s379.md` §3
Direction A, migration step 1).  **Inert by design**: with `PCL_BINDER_CHECK`
unset the binder module is not even loaded, and with it set nothing reads its
answers -- each hook compares, the legacy verdict is what the resolver returns.
**Since s533b (1c port 1, §6 item 1) the binder is built in PRODUCTION and the
block-package requalifier reads it; the `binding_at` hook is gone and
`PCL_BINDER_CHECK` arms the two remaining hooks only.  Since s535a (1c port
3, §6 item 3) NO dual-run hook is left: the span pass and every rename pass's
shadow test read the binder, and `PCL_BINDER_CHECK=log` logs only the
measurement rows (S build / R rebuild / C query counts / E errors).**

| file | what it is |
|---|---|
| `Pl/Binder.pm` | THE BINDING TABLE: one walk over the PPI document in document order, perl's pad model (pad.c).  A binding record per declaration (`id kind canon sigil name perl_package decl_node decl_ord scope_node from to`), a ref table per variable token (`canon via ord pkg cv binding`), globals/specials minted lazily.  Rules in §1.  Rule 12: an unknown variable-token shape DIES naming it. `PCL_BINDER_TRACE=1` prints every ref with its binding. |
| `Pl/Parser2.pm` (hooks, 31 lines added, 0 removed) | the build point (§2) and three wrappers: `_binding_at` / `_ref_shadowed` / `_block_captures_name` each call their unchanged body (`*_0`), then `Pl::Binder::check` when `$self->{_binder}`. |
| `tools/binder-check.pl` | `FILE... \| --population perl-tests\|perl-t\|everyday\|cpan\|all [--jobs N] [--out F] [--summary]`, `--summary-of LOG`, `--dump FILE` (ref -> binding listing under `PCL_BINDER_CHECK=die`).  Log rows: `S` (per build: refs, bindings, kinds, via, captures, fragments, build_ms, tokens), `D` (a disagreement), `R` (a rebuild), `C` (resolver calls per transpile), `E` (a binder error), `X` (a timeout / failed transpile). |
| `tools/lib/PCLPopulations.pm` | the four population lists, shared with `tools/ppi-expect-check.pl` (no third population walker). |
| `Pl/t/binder-01.t` | the gate guard: 17 rows, 1 s, no SBCL -- 15 binding shapes + no-die + the `PCL_BINDER_INJECT` inverse row. |

Modes: `PCL_BINDER_CHECK=die` dies at the first disagreement with both verdicts
(`PCL binder: binding_at disagrees at F line L col C `$c` ($c): legacy=lex
binder=undef [my] my $c decl=1:12`); `=log` appends a TSV row to
`$PCL_BINDER_LOG` (STDERR when unset) and continues; a binder error is an `E`
row in log mode, a die otherwise.  `PCL_BINDER_INJECT=binding_at|ref_shadowed|block_captures`
inverts the binder's answer (the inverse guard).

## 1. The rules (each one sub in `Pl/Binder.pm`, perl 5.40.3 source)

| rule | sub | perl |
|---|---|---|
| a `my`/`our`/`state` is visible from the statement AFTER its own; its initializer sees the OUTER binding (`my $x = $x + 1`) | `_intro_point` | op.c:8653 `newSTATEOP` -> `intro_my` pad.c:1451; COP_SEQ_RANGE_LOW exclusive |
| a binding ends with its scope (block / document / compound) | `_intro_point` | `pad_leavemy` pad.c:1503 |
| `for (my $i = …; …; …)`: from after the init, to the end of the loop | `_intro_point` (Structure::For) | perly.y:758 `mnexpr` |
| `foreach my $x (LIST) BLOCK`: from after LIST, to the end of the loop | `_intro_point` (Compound) | perly.y:519 (LIST is `mexpr`) |
| `if`/`elsif`/`unless`/`while`/`until` HEAD `my`: after the head, to the end of the compound | `_intro_point` (Structure in Compound) | perly.y:464-497, :704 `mexpr`/`texpr`+`mintro` |
| a redeclaration in the same scope opens a NEW binding (innermost / latest `from` wins) | `_resolve_ord` | `pad_findmy_pvn` pad.c:961 -> `pad_findlex` pad.c:1101 (current pad, then `CvOUTSIDE`) |
| `foreach $x` with no `my` is a USE: it aliases the lexical in scope, else the global | `_token` (no decl) | toke.c `yyl_foreach` |
| signature parameters: each from the end of its OWN part (a later default sees it, its own default the outer name, #485) to the end of the sub | `_signature` | perly.y `sigsub` |
| a named-parameter `Token::Prototype` binds only where the signature feature is in force at that sub (#455 / #2872) | `_proto_params` | toke.c / perly.y prototype vs signature |
| `catch ($e) BLOCK`: `$e` in BLOCK only | `_catch_param` | perly.y:294 `catch_paren` |
| `our $x` = a lexical ALIAS of `$PKG::x`, PKG = the package in force at the DECLARATION (#475) | `_declare` | pad.c `pad_add_name_pvn` with `padadd_OUR` + `SvOURSTASH` |
| `my sub f` = kind lexsub; `state` = its own kind | `_declare` | pad.c (`&` pad names) |
| an undeclared name = the global of the POSITIONAL package; `_ INC ENV SIG ARGV STDIN STDOUT STDERR ARGVOUT` and punctuation = `main::` (kind special); a qualified name is always a global (#3080) | `_resolve_ord`, `_sym_canon` | toke.c:9994 `S_pending_ident`; gv.c `S_gv_is_in_main` |
| `local` binds nothing; `strict vars` is not enforced (CLAUDE.md rule 9) | -- | -- |
| `${x}` / `@{x}` / `$#{x}` at code level: the ONE helper `Parser2::_brace_name_refs` | `bind` | -- |
| interpolated uses: `Pl::InterpScan::scan` over dq / heredoc / regex / qx / qr / readline text, all at the token's ordinal; `@{[ … ]}` / `${\ … }` code through `Pl::Parser::fragment_doc` (the s438f pattern) | `_interp_refs`, `_code_refs` | toke.c `S_intuit_more`, `scan_const` |
| a cast-prefixed symbol is its own content (`*$g{IO}` reads `$g`, not `%g`); `\` is NOT a cast for this (`\$b[0]` is `@b`) | `_sym_canon` | -- (PPI `->symbol` quirk) |
| `$#+` / `$#-` (PPI: one `Token::Magic`) = the arrays `@+` / `@-` | `_sym_canon` | perlvar |
| an unnamed signature placeholder `$` / `@` / `%` binds nothing | `_sym_canon` | perly.y `sigscalarelem` |

**Current names.**  The legacy resolvers are asked MID-RENAME (a pass has
already rewritten `my $x` to `my $x__file__0` and asks about the rest by the
names they carry now).  The binder keeps perl's ranges and re-reads each
token's CURRENT text (`_live_canon` / `_cur_refs` / `_resolve_cur`): a
renamed-away declaration no longer binds its old name.  The rename suffixes
it strips are the ten families `__file __state __shadow __excl __lex __evalcap
__lexsub __cond __family __emb`.

## 2. Where it runs

Built ONCE per document in `Parser2::parse`, right after
`_classify_nested_gotos($doc)` -- **document pass 36 of 40** that take `$doc`
in `parse` (counted with the review's method, `refaddr` excluded) -- and
before `_scan_pkg_global_spellings`, the two requalifiers
(`_requalify_block_our_after_pkg_switch`, `_requalify_block_globals_after_pkg_switch`)
and every identity pass (`_rename_spanning_lexicals`, `_promote_captured`,
`_rename_state_vars`, `_rename_exception_mys`, …).  Passes that ran before it
and that the binder therefore sees applied: the PPI repair block, the
`_normalize_*` passes (incl. `_normalize_tie_my`), `_rewrite_state_prepass`,
`_split_literal_list_decls`, `_rename_lexical_subs` (hence the `__lexsub`
suffix in the strip list).  In eval mode the capture alist is the outermost
pad (kind `outer`, ir-spec §2b) and the eval's package the starting package.

**Rebuilds: 0** in all four populations and the gate.  The hook rebuilds the
binder when a resolver is asked about a token the table does not hold (a
re-parsed document or an inserted token) and logs an `R` row; none fired, so
every resolver call of today lands on the document as it stood at pass 36 or
on tokens rewritten in place (which the current-names layer reads).  Whether
the 18 re-serialize sites of review §1b re-parse before or after a resolver
is asked, none of today's calls lands on a re-parsed document -- 1a may
reorder them freely as far as these three resolvers go.

## 3. The measurement (D3)

PPI 1.291, this tree's `./pl2cl` (`--module` for `.pm`) under `PCL_BINDER_CHECK=log`,
`--jobs 2`.  `decl` = declared bindings; refs = every variable reference the
table holds (declarations included).

| population | files | tokens | refs | decl | interp refs | captures | binding_at / ref_shadowed / block_captures calls | D sites | E | build_ms |
|---|---:|---:|---:|---:|---:|---:|---|---:|---:|---:|
| `everyday/*/*.pl` | 122 | 27,912 | 2,430 | 606 | 469 | 44 | 0 / 36 / 101 | 0 | 0 | 308 |
| `perl-tests/*.t` | 111 | 295,389 | 23,708 | 3,221 | 2,401 | 577 | 499 / 1,257 / 2,823 | 7 | 0 | 2,704 |
| `cpan-tests/modules/**` | 378 | 345,981 | 27,959 | 5,214 | 1,935 | 1,223 | 45 / 991 / 3,129 | 2 | 0 | 2,457 |
| perl's own `t/` (528 files; 525 reach the build point) | 525 | 824,124 | 65,764 | 10,250 | 9,548 | 1,609 | 1,142 / 3,250 / 7,903 | 13 | 0 | 5,505 |

Bindings by kind (perl's `t/`): my 9,604, our 496, param 124, lexsub 116,
state 26; global 1,641, special 1,743.  perl-tests overlaps perl's `t/`
(same programs, PCL's copies), so its 7 D sites are 7 of the 13.

The two GATE-SIDE runs, every file transpiled with the binder built and the
hooks asserting: **the full sweep** under `PCL_BINDER_CHECK=log` (2,681 builds
over 399 files -- run-time string evals included, hence 10,695 `outer`
bindings; calls 507 / 6,218 / 10,290; the same 6 perl-tests D sites as above,
0 E, 0 R) and **the gate** (`tools/prove-core`, 7,208 builds over 5,039
transpiled files; refs 139,493; calls 670 / 3,666 / 14,001; 9 D rows in 6
programs, 0 E, 0 R -- verdicts in §4).  The gate's first log-mode run FAILED
`Pl/t/parser-leak-01.t` row 2 (300 transpiles grew 32 MB): the binder held
its parser strongly and the parser held the binder, a cycle; the back
reference is now weak, and the rerun is PASS.

Transpiles that fail with the binder on fail identically without it (11
files: 1 cpan, 10 perl `t/`; the same message, checked on the base): a die's
exit code is errno noise in both paths (`pl2cl` :1346), so `X` rows are
compared by message, not code.

## 4. Every disagreement has a verdict

**(a) LEGACY BUG -- the legacy resolver answers differently from perl (probe:
perl -> base `81ef05e5` -> tree; the tree is inert so it equals the base):**

| task | site(s) | resolver | the shape | perl -> base |
|---|---|---|---|---|
| **#3800** | local.t:615 | binding_at lex->undef | `_head_binding` takes every EARLIER sibling of an anon sub's list as its head, so a `my $x` inside an earlier anon sub makes `$x` "lex" in a later one; in a sub body (not flattened) the requalifier leaves it unqualified -> written to `main::` | A7.pl: `main=u P=5` -> `main=5 P=u` **SILENT WRONG** |
| **#3801** | local.t:652, :660; write.t:1562, :1564 | binding_at undef->lex | chained `my $S = my $F = 0;`: `_declarator_syms` reads only the first declarator, so `$F` is requalified to the package global after a `package` switch | B4.pl: `F=2 S=2 T::F=u` -> `F= S=2 T::F=2` **SILENT WRONG** |
| **#3804** | universal.t:186; taint.t:1763 | binding_at undef->lex (the declarator) | `tie my($x), CLASS` after an in-block `package` switch in a sub: the `my` is lost, `$CLASS::x` is tied and read | F1.pl: `global untied` -> `global tied` **SILENT WRONG** |
| **#3802** | Text-CSV t/66_formula.t:74, :170; gate: first-run-01.t's Side.pm (x2) | block_captures 1->0 | a `my` nested in a call's argument list (`ok(my $csv = …)`, `open(my $l, …)`) is invisible to the capture scan, so the FILE lexical of that name is reported captured and promoted | C1.pl: identical output -- BENIGN (wrong `:captures`, an unneeded cell) |
| **#3803** | write.t:2224 | ref_shadowed 0->1 | `_ref_shadowed`'s sibling walk does not see a compound HEAD `foreach my $e` (no twin of `_head_binding`) | E4.pl: identical output -- BENIGN; in write.t the loop var becomes a `:dyn` cell, unreachable (formline dies first) |
| **#3806** | gate: sig-param-shadow-01.t, row "a DIFFERENT-sigil use of the same bare name is still a capture" | block_captures 1->0 | `sub g ($x) { "g($x)[@x]" }` written BEFORE `my @x`: perl reads the GLOBAL `@x` (intro_my); the signature path counts the later `my @x` and refuses -- and the row ASSERTS that refusal | H1.pl: `g(1)[]` -> `Transpile failed` -- a LOUD refusal of valid perl |

**(b) BINDER ERROR -- fixed in this batch (rule in §1):** `\$b[0]` read as
`$b` (aassign.t:183; the cast rule excludes `\`); a bare `$` signature
placeholder (signatures.t; binds nothing); `$#+` / `$#-` died as an unknown
shape (re/pat.t, re/pat_rt_report.t; they are `Token::Magic`; #3805 asks the
PPI-registry filler whether that is a PPI quirk -- probably not).  Earlier
(before the resume): the current-names layer, for every rename-mid-pass row.

**(c) DESIGN GAP -- recorded for 1c:** **5 sites in the suites + 6 rows in 3 of the
gate's own programs**, one shape: a name that occurs only inside the TEXT of a
string `eval` (`sub { eval '$x' }` -- closure.t:548, lexsub.t:653, eval.t:279
`$zzz`, eval.t:328 `$zzz` / `$yyy`; gate: `eval '$n = $o'`,
`eval 'sub { package Other; … }'`, `return eval '…'`).
The legacy capture scan counts it (conservatively: a string eval compiled at
run time may read any visible lexical, ir-spec §2b); the binder models the
compile-time pad, where a closure does not capture a name only an eval string
mentions.  Information 1c needs: "this CV contains a string eval" as a fact
on the CV, which turns every visible outer lexical into a potential capture
(perl's own rule: an eval sees what its CV can see at run time, closure.t:548's
point is exactly that an anon sub has only what it captured).
**RESOLVED (s533d, 1c port 2).**  The binder records every string eval
(`eval EXPR`, `CORE::eval EXPR`, `s///ee`) with its CV (`has_string_eval($cv)`)
and reads, on demand, the names a literal text mentions (`text_refs`: a quote's
raw body, a heredoc, qw -- through `Pl::Parser::fragment_doc`, no new
scanner).  `captures_name(..., eval => 1)` counts such a text use of a lexical
L exactly when a string eval runs inside L's visible range (`eval_sees`):
that eval can see L and the text can reach it (an argument, a return value, a
variable -- the text in one sub, the eval in another).  Keyed on the CV alone
(the first D0 cut) the text-in-one-sub / eval-in-another shape would have
lost its cell; keyed on L's range it keeps it.  With the rule the 9 sites
AGREE (the D0 re-run, s533d scratch `d0-pop.tsv` / `d0-gate.tsv`).

## 5. The cost

Wall time of `./pl2cl` (min of 3; everyday one run per file) with the
variable unset vs `PCL_BINDER_CHECK=log`, measured in an exclusive bench
slot (load 1.96 -> 1.64).  `build_ms` = the walk + resolve alone (the `S`
row); the rest of the difference is the hooks' queries.

| input | unset | log | wall delta | binder build | build share |
|---|---:|---:|---:|---:|---:|
| `lib/Math/BigInt/Calc.pm` (2,566 lines, `--module`) | 5,769 ms | 6,112 ms | +6.0 % | 152 ms | 2.6 % |
| `perl-tests/pack.t` (the sweep's largest, 72 kB) | 4,880 ms | 4,965 ms | +1.7 % | 114 ms | 2.3 % |
| `everyday/*/*.pl` (122 files, total) | 24,946 ms | 25,589 ms | +2.6 % | 158 ms | 0.6 % |

**The table itself costs 2-3 % of a transpile** -- under the ~5 % line that 1c's
Calc.pm bar (3.7 s -> under 1.5 s) allows.  Calc.pm's +6 % wall is the
dual-run's QUERIES on top (not profiled further), a cost the port removes
with the hooks.  The review's 3.7 s was a different command (`./pl2cl <
Calc.pm`); 5.8 s here is `--module` on this tree, so compare the deltas, not
the absolutes.

## 6. The 1c port plan (D4)

The consumer order is s379 §3's (newest first); each port replaces a private
resolver with a binder query, its gate is corpus-diff byte-identical + the gate
+ the full sweep (the name-resolution row) + the port's own family tests, and
each deletes its resolver and veto twin.  The scope loops are the 19
`for (my $p = $X->parent …)` loops of `Pl/Parser2.pm` (line numbers at this
tree).

1. **Requalifier** (`_requalify_block_our_after_pkg_switch`,
   `_requalify_block_globals_after_pkg_switch` -> `_requalify_switched_regions`
   -> `_requalify_region` :725, the ONE `_binding_at` call).  Query:
   `binding_of($tok)` -> kind (`my|state|param|lexsub` leave it, `our` ->
   `$perl_package::x`, `global` -> the positional package, `special` ->
   `main::`).  Retires `_binding_at_0` (:785 loop), `_stmt_binding`,
   `_head_binding`, `_decl_binding_in`, `_pkg_stmt_in_effect_at` (:937) and, after the signature consumers move,
   `_signature_left_params` (:3582).  Disagreements under it: **#3800, #3801,
   #3804** (all three silent wrongs) -- the port fixes them by construction;
   the binder already answers #475 the perl way (Pl/t/binder-01.t row 11).  Smallest port, biggest value: FIRST.
   **DONE (s533b, `75e513e0`, 2026-11-03).**  The binder is built in
   production (no flag; `PCL_BINDER_CHECK` arms only the two remaining hooks);
   `_requalify_region` asks `Pl::Binder::requalify_home` per token (`decl` and
   my/state/param/lexsub/outer -> leave, our -> declaring package, global ->
   positional package, special -> main; no record -> DIE, rule 12).  The D0
   measurement compared the requalifier's FINAL home, not `_binding_at`'s
   verdict, and found 15 sites (the 10 of #3800/#3801/#3804 below plus the
   uses after a lost declarator -- universal.t:187-188, taint.t:1765 -- that
   the s525q hook could not see, because the current-names layer read the
   already-rewritten declarator -- and NEW #4423, a C-style `for (my $i ...)`'s
   condition and step, core Encode.pm:184/196).  Deleted: `_binding_at`,
   `_binding_at_0`, `_head_binding`, `_decl_binding_in`, `_q_binding_at`.  STAY
   (other readers): `_stmt_binding` (`_ref_shadowed_0`), `_pkg_stmt_in_effect_at`
   (`_pkg_in_effect_at`, `_pkg_region_at`), `_signature_left_params`
   (`_signature_binds`) -- they go with port 3.  Not reached: #3801's
   FILE-level bare-block form (the span pass reads only the first declarator,
   #4425), a `package NAME { }` block inside a sub (no region, #4421).
2. **Capture gate + promoter** (`_check_sub_captures` :3857 from parse:2500,
   `_captured_in_subs` :5631 -> `_sub_captures_name` :3805 ->
   `_block_captures_name`; `_promote_captured` :5551; `_hoist_nested_sub`
   :10621).  Query: `captures($cv_block)` (pad_findlex's OUTER entries) + the
   string-eval CV fact of §4(c).  Retires `_block_captures_name_0`'s two
   scope loops (:4025, :4055) and its `%canon_pat` text sets,
   `_enclosing_lex_decl` (:5654); rename reason `__file__` (:captured).
   Under it: **#3802**, **#3806** and the 9 design-gap sites (which need the
   eval fact first, or the port changes those files' emission).
   **DONE (s533d, `1fed1b04`, 2026-11-05).**  The capture gate, the promoter,
   the nested-sub hoist and the closure tests that went through
   `_block_captures_name` (`_init_reads_scalar`, `_cond_mys_closure_captured`)
   ask `Pl::Binder::captures_name` (pad_findlex's OUTER entries, positional,
   body + signature) with the string-eval rule of §4(c) (`eval_sees`: a
   literal text names L only where a string eval can see L); no range / no
   binder / no record DIES (rule 12).  `_enclosing_lex_decl` asks
   `Binder::enclosing_decl` (a same-name my/state whose SCOPE encloses the
   extent, before or after it).  D0 compared the consumer's FINAL answer: the
   #3802 / #3806 / #4462 sites only, corpus-diff IDENTICAL, the wide A/B's
   diffs exactly those files (+ the #3961 flip).  Deleted:
   `_block_captures_name_0`, `_canon_text_patterns`, `_signature_captures_name`,
   `_sub_signature_canons`, the legacy `_enclosing_lex_decl` walk, the
   `block_captures` hook, Binder's `_q_block_captures` (Parser2 -274 net).
   STAY: `_signature_part_defaults` / `_signature_param_canons` (read by
   `Binder::_proto_params`), `_signature_left_params` / `_signature_binds`
   (`_ref_shadowed`, port 3).  Found: #4462 (fixed by the port), #4461 (filed).
3. **Span** (`_check_my_spanning` :3128 and `_rename_spanning_lexicals` :4505
   / :4671-:4740 -> `_canon_refs_in` -> `_ref_shadowed`; `_lex_referenced_after`
   :3075 from `_pkgblock_shadows_file_lexical` :3044).  Query: "does binding B
   have a ref in segment j" = the ref table grouped by binding, segment = a
   range of ordinals.  Retires `_ref_shadowed_0`, `_flattenable_pkg_block`
   (:3003), `_loop_scope_of` (:7659), the bare-name `%spanning` text
   pre-filter (which is how write.t's `%e` made `$e` a candidate); rename
   reason `__file__` (:spanning).  Under it: **#3803**.
   **DONE (s535a, the commits "s535a D1" + "s535a D2", 2026-11-08; generation
   v2-12000).**  The span pass asks the binder: `Pl::Binder::spans` answers
   which my/state bindings have a use (or, under port 2's string-eval rule,
   a literal text) in a later segment -- every declarator a binding of its
   own, so a chained `my $S = my $F` (#4425) and an expression-embedded
   `open my $fh` span too; a span made only of possible (text) uses promotes
   but never refuses.  The renamer promotes each and rewrites exactly the
   binding's uses (`refers_to_decl`); a use the rewrite missed dies (rule
   12); the checker dies for a spanning binding left unpromoted; the flatten
   refusal asks `binding_after` / `uses_of`.  Every rename pass's shadow test
   (the promoter's, `_rename_decl_within`'s, the state prepass's with a
   binder of its own document) asks `refers_to_decl`, so `_ref_shadowed` --
   the LAST dual-run hook -- is gone, and with it the D rows.  D0 compared
   the consumer's FINAL answer (the emission, switch on vs off over 3,263
   files): 14 predicted rows; the actual vs `06a4b8ba` matched in both
   directions, plus the D2-only `our` diffs (#475) and #3961's base flip.
   The sweep found what the D0 reading missed: two blessed scalar.t rows
   that were the embedded-`my` span bug (mis-caused #1446) and an eval.t row
   that was the legacy over-promotion (#1455's instance).  Deleted:
   `_ref_shadowed`, `_ref_shadowed_0`, `_stmt_binding`, `_stmt_declares_canon`,
   `_signature_binds`, `_signature_left_params`, `_symbol_is_signature_param`,
   `_canon_refs_in`, `_collect_lexical_canon`, `_lex_referenced_after`, the
   `%spanning` text pre-filter, `%pre_my` (Parser2); `check()`,
   `_q_ref_shadowed`, `_pass_name`, `_invert`, `_binding_cur`,
   `PCL_BINDER_INJECT` (Binder).  STAY (other readers, so not retired as
   this item guessed): `_flattenable_pkg_block` (the segment builder's
   flatten decision -- its loop asks about loop-control targets, not
   bindings), `_loop_scope_of` (loop control), `_pkg_in_effect_at` /
   `_pkg_stmt_in_effect_at` / `_pkg_region_at` (PExpr's sort pair, the `our`
   rename).  Beside it (same family, the requalifier): #4421 #4440 #4422
   #475.  Found: #4546 (fixed: the promoter's rewrite renamed a C-style
   `for (my $i ...)` in a sub -- core Math::Complex::root), #4541 #4543
   #4544 #4545 #4547 (filed).  Port 4 asks for a dual-run point at
   `_rename_decl_within` first; the hook machinery left with this port, so
   port 4 adds its own if it wants one (the S/R/C/E log stays).
4. **state / cond / shadow / emb / exception** (`_rename_state_vars` :9357,
   `_state_rewrite_routes` :9210, `_rename_exception_mys` :9471,
   `_gate_seam_my_shadow` :15234, `_cond_mys_closure_captured` :12853,
   `_init_reads_scalar` :15403 -- all through `_rename_decl_within` :6294 or
   `_block_captures_name`).  Query: "the refs of binding B" (a rename becomes
   "print binding B's chosen name"), plus `captures`.  Retires
   `_lexical_decl_scope` (:9585), `_enclosing_sub_body` (:9611),
   `_lexsub_scope` (:6173), `_init_reads_scalar`'s loop (:15398); rename
   reasons `__state__`, `__cond__`, `__shadow__`, `__emb__`, `__excl__`.  No
   disagreement under it today (the rename passes ask `_rename_decl_within`,
   not the three hooked resolvers -- a SECOND dual-run point is needed here
   before porting: hook `_rename_decl_within` the same way).
   **Done ahead of port 4 (s536e, generation v2-13300), the questions its
   silent wrongs needed:** the embedded-`my` VETO asks `uses_of` of the
   declarator's binding (a use inside another named sub, plus the port-2
   eval-text rule) -- the sigil-blind text scan and its two exemption walks
   (`_sub_declares_name`, `_sub_freely_references_name`, `_decl_scope_block`)
   are deleted (#4641); "does the statement mention its embedded `my` again"
   is `Binder::mentions_in` (as spelled now), read by both embedded arms and
   by the rename that makes the let safe (`_rename_rereading_embedded_mys`,
   `__shadow__` / `:reread-shadow`, #4642); the state prepass qualifies a
   cell use whose binder package is not the declaration's (#4541).  The span
   pass's three silent wrongs: an exception-partition name never takes the
   identity cell (#4547), a LIST declaration's container is offered (#4543),
   a one-token signature default is renamed in the token text (#4545).
5. Not identity (stay): `_scan_eval_site_features` (:189),
   `_rewrite_unreachable_gotos` (:261), `_classify_nested_gotos` (:307),
   `_ancestors` (:654), `_premerge_glob_const_prototypes` (:1150),
   `_constant_handlers_at` (:6741), `_arg_owner_word` (:14254) -- 7 of the 19
   loops ask about labels, constants or call owners, not bindings.

Cost bar for the ports: the binder replaces walks, so 1c's Calc.pm target (3.7
s -> under 1.5 s) is met only if the ports DELETE the legacy walks; dual-run
cost (§5) is the upper bound of what the table adds.
