# The expect-state checker and the PPI-bug registry (s525p, 2026-10-24)

Parser-structure program, Option 1 step 0, member **c1** (`docs/parser-review-s525.md`
§5 box, step 0 first half; the registry is its §7).  **Emission-neutral by
design**: nothing the compiler loads reads either new module.

| file | what it is |
|---|---|
| `Pl/PPIExpect.pm` | the CHECKER: walks a `PPI::Document`'s significant tokens carrying perl's `PL_expect` (perl.h:5912) as toke.c does, and returns one row per token whose PPI class contradicts that state.  The two tables (D2) are in it: `@TRANSITIONS` (class × expect → next) and `@CONTRADICTIONS` (class × expect → kind, catalogue §), every row citing its toke.c handler + line.  Inert; rule 12: an unknown class DIES. `PPI_EXPECT_TRACE=1` prints every token with its state. |
| `tools/ppi-expect-check.pl` | the command: `FILE...` or `--population perl-tests\|perl-t\|everyday\|cpan\|all`, `--jobs N`, `--summary`, `--mode raw\|pcl` → TSV `file line col text class state expect kind sec cascade note`.  `use utf8` sources are decoded as PCL decodes them (`Pl::Parser::_maybe_decode_utf8`).  Never run it with `-Ilib` (PCL's `lib/Cwd.pm` breaks FindBin). |
| `Pl/PPIBugs.pm` | the REGISTRY: 38 rows, one per catalogue §, DATA + one accessor. |
| `Pl/t/ppi-bugs-registry-01.t` | the CANARY (gate): 76 rows, 0.16 s, PPI only. |
| `tools/t/ppi-expect-check.t` | the checker's breaking-case rows (not in the gate): 12 rows. |

## 1. The measurement (D3)

PPI 1.291, raw documents (`--mode raw`: no PCL repair applied).  A row is
**primary** unless an earlier row stands in the same walk statement, on the same
line, after a split continuation, or inside a format's swallowed statement
(`cascade` = the damage of one mis-lex, not a new bug).

| population | files | tokens | rows | primary | cascade |
|---|---:|---:|---:|---:|---:|
| `perl-tests/*.t` | 111 | 181,726 | 333 | 257 | 76 |
| perl's own `t/` (`run-perl-suite.pl --all` dirs) | 528 | 483,314 | 1,180 | 823 | 357 |
| `everyday/*/*.pl` | 122 | 17,480 | 24 | 16 | 8 |
| `cpan-tests/modules/*/{lib,t}/**/*.{pm,t}` | 378 | 208,592 | 89 | 84 | 5 |
| **all four** | 1,139 | **891,112** | 1,626 | **1,180** | 446 |

Wall time: 0.8 s (everyday) to ~40 s (perl's t/) at `--jobs 2`.

### Primary contradictions by kind (all four populations)

| rows | kind | PPI class | state | § |
|---:|---|---|---|---|
| 288 | list-slice-as-constructor | Structure `[` | XOPERATOR | NEW #3740 |
| 109 | brace-class | Structure | XOPERATOR | §17 |
| 91 + 80 + 3 | hexfloat-split | Word `p` / Number::Float / Exp | XOPERATOR | §2 |
| 71 | statement-not-ended | Word | XSTATE | NEW (#3746 defer, #3747 class, #3743) |
| 70 | brace-class (`*NAME{IO}`) | Structure `{` | XOPERATOR | NEW #3741 |
| 70 | cast-then-word | Word | XCAST | §23 |
| 57 | block-owner-split | Structure `{` | XBLOCK | §10 |
| 44 + 32 + 3 + 2 | lt-as-comparison | Operator `<` | XTERM / XTERM! / XSTATE / XREF | §14 |
| 34 | format-body-lexed-as-code | Word `format` | XSTATE | NEW #3745 |
| 20 | bare-sigil-symbol | Symbol `$` | XTERM | §32 |
| 14 | cast-then-punct | Operator | XCAST | §24 |
| 13 | minus-in-number | Number `-N` | XOPERATOR | §15 |
| 13 + 6 | slash-as-division | Operator `/` | XTERM! / XTERM | §11 |
| 12 + 4 | continuation-split | Word `continue` / `finally` | XSTATE | NEW #3743 / §18 |
| 10 + 3 | brace-class (anon hash) | Structure `{` | XREF / XTERM | NEW #3742 |
| 9 + 4 + 3 | label-not-at-statement-start | Label | XTERM / XSTATE / XTERM! | §7 |
| 9 | cast-then-word (`$ name`) | Word | XCAST | NEW #3744 |
| 7 / 7 / 6 / 6 | decl-attribute / star-as-multiply / logical-xor / pid-magic | | | §8 / §26 / §16 / §1 |
| 11 | x-as-repetition | Operator `x` | XSUBNAME / XSTATE / XTERM | §19 |
| 5 / 2 / 2 / 1 / 1 / 1 | smartmatch / dot-in-number / filetest-split / §5 / §9 / §31 | | | §21 / §28 / §22 / §5 / §9 / §31 |
| ≤ 15 each | the residue | | | NEW #3748-#3755 (§3 below) |

Catalogued: **640 primary rows in 23 §§**; uncatalogued: **540 rows in 16
NEW buckets**.  The §§ the four populations never reach: 3 (fixed), 4, 12, 13,
20, 25, 27, 29 (one lexer death counted under §6/29), 30, 33, 34, 35, 36, 37,
38 -- their shapes live in the corpora PCL's repairs were found in (CPAN
modules outside the board, Data::Dump, IO::Uncompress::Base).

## 2. The JOIN against the 38 catalogue sections

Each § was run through the checker with the registry's own repro (a runnable
program, report-derived; `Pl/PPIBugs.pm` `repro`).  The canary file re-asserts
this split on every gate run (its third block).

**PREDICTED -- 34 of 38**: §1 §2 §4 §5 §6 §7 §8 §9 §10 §11 §12 §14 §15 §16
§17 §18 §19 §20 §21 §22 §23 §24 §25 §26 §28 §29 §31 §32 §33 §34 §35 §36 §37
§38 (§6 and §29 as the one `lexer-dies` row, `sec` = `6/29` -- PPI's error is
the same string for both).

**NOT PREDICTED -- 4** (§3 because PPI 1.291 fixed it: the checker is
correctly CLEAN there and its registry row asserts `want`; the other three by
construction):

| § | kind | why the token walk cannot see it |
|---|---|---|
| 3 | token | FIXED: the stream is right |
| 13 | source | the bug is in how PPI reads the TEXT (`$/` undefined adds a newline after a final `__END__`) -- every token class is right |
| 27 | token | the stream is right; `PPI::Token::Symbol->symbol` answers `%x` for the `$x` of `*$x{SLOT}` -- an accessor bug, not a class |
| 30 | token | the HereDoc token is right; its body's INDENTATION is over-counted -- token content, not class |

**The kind split of the 38** (registry `kind`): **token 28** (§1 2 3 4 7 8 9
11 12 14 15 16 19 21 22 23 24 25 26 27 28 30 31 32 33 34 35 38), **structure 7**
(§5 10 17 18 20 36 37), **source 1** (§13), **lexer-dies 2** (§6 29).  §29's
`for my \$x` half (29a) is a structure split, counted under its title's kind.

## 3. FLAGGED-BUT-UNCATALOGUED: every one has a verdict

**NEW PPI bugs (filed, each with a repro probed vs perl 5.40.3 and PCL):**

| task | bucket | shape | rows | PCL today |
|---|---|---|---:|---|
| #3740 | N1 | list slice `(LIST)[i]` / `qw()[i]` → Constructor | 288 | right (consumers know) |
| #3741 | N2 | glob slot `*NAME{IO}` → Block | 70 | parse right; runtime is #1453/#3674 |
| #3742 | N3 | anon hash `{k => …}` after map/grep/die/exists → Block (§5's sibling) | 14 | right (`_block_is_hash_constructor`) |
| #3743 | N4 | bare block + `continue` → split, next statement swallowed | 16 | right |
| #3744 | N5 | spaced sigil `$ name` → Cast + Word | 9 | right (`_fix_spaced_sigils`) |
| #3745 | N6 | `format` body lexed as code | 34 | source-repaired (`_strip_format_blocks`); `write` not supported |
| #3746 | N7 | `defer BLOCK` swallows the next statement | 11 | refused (not-supported, DEFERRED) |
| #3747 | N8 | 5.38 `class`/`method`/`field`/`ADJUST` syntax | 71 | refused (not-supported, DEFERRED) |
| #3748 | N9 | `main::::x` / `foo::::bar` split into two Words | 6 | right |
| #3749 | N10 | here-doc started inside `s///e` / `qr/(?{<<END})/` -- body lexed as code | 4 | **dies** ("Undefined subroutine &main::END") |
| #3750 | N11 | lexical sub named like a keyword (`my sub s`, `my sub if`) | 9 | **silent wrong** (`s(1)` a substitution) |
| #3751 | N12 | `CORE::__END__` / `CORE::__DATA__` not a Separator | 2 | **dies** after the code ends |
| #3752 | N13 | `& {BLOCK}` (space) at statement start → Operator | 2 | **statement dropped** loudly |
| #3753 | N14 | `require *{"..."}` deref block → Constructor | 1 | right |
| #3754 | N15 | old package separator `sub 'Pkg'name` → Quote + Word | 1 | **statement dropped** loudly |
| #3755 | N16 | `ref shift : shift` in a ternary → Label `shift :` | 2 | right |

N11 is NOT flagged on its minimal repro (the stream is a valid substitution;
the fact is the `my sub` declaration -- a static-parse fact of
`docs/static-parse-limits-review-s512.md`); the checker sees only its cascades.

**CHECKER ERRORS (table fixes made during D3 -- each removed rows that were
the checker's misreading, not PPI's):**

| fix | rows it removed (measured on perl-tests/ unless noted) | toke.c / perly.y |
|---|---|---|
| predicates read the CURRENT state (an anon sub's prototype was judged under XSUBNAME) | 1 | -- |
| `0x1p0` (no dot) is §2, not "word after term" | 44 → §2 | scan_num |
| FUN0 takes `()` (`time()`, `wantarray()`) | 11 | perly.y 1481 `FUNC0 '(' ')'` |
| `->(` is a call, so `->()()` chains | 1 | yyl_hyphen 5972 |
| `print($fh LIST)` -- the indirect-object slot after the `(` | 2 | yyl_dollar 5475-5530 |
| indirect object `meth $obj (…)` / `meth Pkg "x"` / `foo WHATEVER "x"` | 4 | intuit_method 4792-4834 |
| declarations are POSITIONAL and package-qualified (a `sub Pkg::m` further down is not yet known) | 3 | just_a_word: the GV lookup at the point of use |
| feature-gated keywords (`class method field ADJUST`, `given when default break`, `try catch finally`, `defer`) are user words unless the feature (or a keyword-plugin module) is on | 1 (perl-tests; t/class/* rows moved to #3747) | keyword(): FEATURE_*_IS_ENABLED |
| an ALL-CAPS word declared nowhere followed by a term is an imported call (`BAIL_OUT 'x'`) | 2 | (valid-Perl policy) |
| `shift->[0]`: an arrow after a named unary that took no term | 2 | yyl_hyphen 5956 |
| a sub NAMED `x` (`sub x {`, `method x {`) | 5 → §19 | yyl_sub 5535 |
| `use utf8` sources decoded before PPI | 55 bogus lexer-dies (perl t/) | (PCL's own decode) |
| cascade: same line; a format's statement; after a split continuation | 11 | -- |
| NEW rows: `word-after-sub-name` (§N9), `data-marker-as-word` (§N12) | +4 | yyl_sub, yyl_data_handle 6996 |

## 4. Findings for the reviewer

1. **The catalogue's §38 prose repro does not reproduce** on PPI 1.291
   (`{ my $c = 1; $c++ } *$o->{A} = 7;` lexes `*` as a Cast); the report file's
   row (`{ $c = $b; }`) does.  The registry uses the reproducing spelling.  The
   catalogue was not edited (out of scope).
2. **Three of the NEW bugs are live PCL failures** (#3749 dies, #3750 silent
   wrong, #3751 dies) and two are loud statement drops (#3752, #3754); the
   other eleven are already repaired downstream or refused as not-supported.
3. **List-slice-as-Constructor is the largest single PPI misclassification**
   (288 primary rows, every population) and needs no repair: it is the first
   `retag` row 1a can take at zero risk.
4. `Pl::Parser::_reclassify_bare_vwords` (PPI's `v65 =>` as Number::Version)
   is a repair with no catalogue § and no checker row (a Number in term
   position is consistent); it is the one repair this measurement cannot see.

## 5. The per-kind handler sketch for 1a (D5)

**token rows (28).**  The engine IS this walk: the two tables become its
dispatch, and a contradiction whose `(state, class, content, pred)` matches a
registry row's `when` fires that row.  `relex` (19 rows): take the text from the
bad token to the end of the mis-lexed span, re-tokenize it with
`PPI::Tokenizer` inside a one-token prefix that forces perl's state (`;` for
XSTATE, `1 ` for XOPERATOR -- the `Pl::Parser::fragment_doc` pattern), splice
the new tokens in.  MERGES (n tokens → 1): §2 §9 §11 §14 §16 §22 §23 §24 §26
§32 §35; SPLITS (1 → n): §1 §4 §12 §15 §21 §25 §28 §34.  `retag` (6 rows: §7
§8 §19 §31 §33 §38): same text, a new class -- re-bless the token.  They replace
`_split_pid_magic_cast_run` §1, `_fix_modulo_magic` §4, `braced_punct_magic_name`
§9, `_repair_word_match` (+ `_repair_glob_pattern_cascade`) §11/§14,
`_repair_glob_multiply` §12, `_repair_readline_cascade` /
`_fix_ppi_glob_after_block` §14, `_fix_ppi_negative_number_bug` §15/§34,
`_fix_ppi_logical_xor_bug` §16, `_repair_term_initial_complement` §21,
`_fuse_print_filehandle_filetest` §22, `_merge_unicode_symbols` §23,
`_merge_punct_array_symbols` + `_repair_punct_hash_name` (+ `_cast`) §24,
`_repair_minus_word` §25, `_repair_punct_glob_name` §26, `_repair_dot_number`
§28, the `$)` arm of `_ppi_new` §32, `prefix_first_code_line` §35;
`_normalize_anon_sub_attrs` + `_repair_swallowing_prototypes` §7,
`_strip_decl_decorations` §8, `_repair_word_x_call` §19,
`_desugar_loop_modifiers` §33, `_repair_statement_start_glob_cast` §38.
§27 / §30 keep handlers (`_brace_glob_slot_symbol`, `_repair_indented_heredocs`):
the class is right, the walk cannot fire them -- they run where the token is read.

**structure rows (7).**  Not re-lexable: the tokens are right.  The walk fires
at the row's `when.check` (brace-class, block-owner-split, continuation-split,
statement-not-ended, filehandle-block-split) and calls the named handler, which
re-parents nodes (§10 moves the head list + block into the Compound, §18/§20
attach the continuation, §5/§17/§36/§37 re-bless Block ↔ Constructor ↔
Subscript): `_bare_block_is_anon_hash` §5, `_repair_nary_foreach` +
`_repair_alias_foreach` §10, `_parse_subscript_ix` §17, `_repair_try_finally`
§18, `_pcl_feature_include_cb` §20 (a PPI hook, stays), `_brace_name_refs` §36,
`_repair_paren_block_filehandle` §37.

**source rows (1).**  Before tokenization; stays a pre-pass (`_trim_invented_tail`
§13).  The registry row exists for the canary only.

**lexer-dies rows (2).**  No document: the engine catches PPI's undef, rewrites
the `for` head to a lexable spelling and restores the dereference when
lowering (§6, §29; none shipped -- #1580, #1649).

**Uncatalogued repairs** the NEW tasks name: `_fix_spaced_sigils` (#3744),
`_strip_format_blocks` (#3745), `_block_is_hash_constructor` (#3742); each
becomes a registry row when its § is written.
