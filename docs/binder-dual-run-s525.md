# The binder in dual-run (s525q, 2026-10-24)

Parser-structure program, Option 1 step 0, member **c2** (`docs/parser-review-s525.md`
§5 box, step 0 second half; design `docs/var-handling-review-s379.md` §3
Direction A, migration step 1).  **Inert by design**: with `PCL_BINDER_CHECK`
unset the binder module is not even loaded, and with it set nothing reads its
answers -- each hook compares, the legacy verdict is what the resolver returns.

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
GATE_LOG_ROW

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
| **#3802** | Text-CSV t/66_formula.t:74, :170 | block_captures 1->0 | a `my` nested in a call's argument list (`ok(my $csv = …)`) is invisible to the capture scan, so the FILE lexical `$csv` is reported captured and promoted | C1.pl: identical output -- BENIGN (wrong `:captures`, an unneeded cell) |
| **#3803** | write.t:2224 | ref_shadowed 0->1 | `_ref_shadowed`'s sibling walk does not see a compound HEAD `foreach my $e` (no twin of `_head_binding`) | E4.pl: identical output -- BENIGN; in write.t the loop var becomes a `:dyn` cell, unreachable (formline dies first) |

**(b) BINDER ERROR -- fixed in this batch (rule in §1):** `\$b[0]` read as
`$b` (aassign.t:183; the cast rule excludes `\`); a bare `$` signature
placeholder (signatures.t; binds nothing); `$#+` / `$#-` died as an unknown
shape (re/pat.t, re/pat_rt_report.t; they are `Token::Magic`; #3805 asks the
PPI-registry filler whether that is a PPI quirk -- probably not).  Earlier
(before the resume): the current-names layer, for every rename-mid-pass row.

**(c) DESIGN GAP -- recorded for 1c:** **5 sites**, one shape: a name that
occurs only inside the TEXT of a string `eval` (`sub { eval '$x' }` --
closure.t:548, lexsub.t:653, eval.t:279 `$zzz`, eval.t:328 `$zzz` / `$yyy`).
The legacy capture scan counts it (conservatively: a string eval compiled at
run time may read any visible lexical, ir-spec §2b); the binder models the
compile-time pad, where a closure does not capture a name only an eval string
mentions.  Information 1c needs: "this CV contains a string eval" as a fact
on the CV, which turns every visible outer lexical into a potential capture
(perl's own rule: an eval sees what its CV can see at run time, closure.t:548's
point is exactly that an anon sub has only what it captured).

## 5. The cost

COST_TABLE

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
2. **Capture gate + promoter** (`_check_sub_captures` :3857 from parse:2500,
   `_captured_in_subs` :5631 -> `_sub_captures_name` :3805 ->
   `_block_captures_name`; `_promote_captured` :5551; `_hoist_nested_sub`
   :10621).  Query: `captures($cv_block)` (pad_findlex's OUTER entries) + the
   string-eval CV fact of §4(c).  Retires `_block_captures_name_0`'s two
   scope loops (:4025, :4055) and its `%canon_pat` text sets,
   `_enclosing_lex_decl` (:5654); rename reason `__file__` (:captured).
   Under it: **#3802** and the 5 design-gap sites (which need the eval fact
   first, or the port changes those five files' emission).
3. **Span** (`_check_my_spanning` :3128 and `_rename_spanning_lexicals` :4505
   / :4671-:4740 -> `_canon_refs_in` -> `_ref_shadowed`; `_lex_referenced_after`
   :3075 from `_pkgblock_shadows_file_lexical` :3044).  Query: "does binding B
   have a ref in segment j" = the ref table grouped by binding, segment = a
   range of ordinals.  Retires `_ref_shadowed_0`, `_flattenable_pkg_block`
   (:3003), `_loop_scope_of` (:7659), the bare-name `%spanning` text
   pre-filter (which is how write.t's `%e` made `$e` a candidate); rename
   reason `__file__` (:spanning).  Under it: **#3803**.
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
5. Not identity (stay): `_scan_eval_site_features` (:189),
   `_rewrite_unreachable_gotos` (:261), `_classify_nested_gotos` (:307),
   `_ancestors` (:654), `_premerge_glob_const_prototypes` (:1150),
   `_constant_handlers_at` (:6741), `_arg_owner_word` (:14254) -- 7 of the 19
   loops ask about labels, constants or call owners, not bindings.

Cost bar for the ports: the binder replaces walks, so 1c's Calc.pm target (3.7
s -> under 1.5 s) is met only if the ports DELETE the legacy walks; dual-run
cost (§5) is the upper bound of what the table adds.
