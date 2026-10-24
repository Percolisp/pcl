# Fixing PPI at the source instead of repairing its stream (s527, the USER's question, 2026-10-24 evening)

**The question (USER, s526 close, s527 open):** "examine if we could fix the bugs in PPI instead -- would that be simpler than the repair engine?" and "if we can just pull that and fix it."  Constraint added during the review (USER): **no patches specific to one PPI version** -- the next release must not break them.

**The answer, measured:** yes, and clearly.  Twelve of the catalogued token bugs were fixed in PPI's own tokenizer in **54 inserted / 8 deleted lines across 7 files** (1 to 13 lines each, at the single decision site where PPI already holds the context), PPI's own suite stayed green (69 files, 53,089 rows, 9 s) and one of PPI's own TODO tests started passing; PCL carries **1,313 lines of repair subs** for the 28 token rows and was about to add a 1,000-2,000 line repair engine (s526a).  **DECISION (USER, 2026-10-24 ~19:xx): PCL uses its own patched PPI**, carried as a BRANCH on upstream master (ordinary commits, rebased per release -- never a version-pinned patch set or a monkey-patch layer), vendored into the tree, with every fix sent upstream as a PR in PPI's required shape.  The repair engine is NOT built for token rows; the expect-state checker stays as the detector of new PPI bugs; the structure rows stay PCL handlers until PPI's Lexer is fixed the same way.

## 1. What PCL carries today because of PPI's bugs

| | count | source |
|---|---|---|
| catalogued PPI bugs (`docs/ppi-upstream-bugs.md`, `Pl/PPIBugs.pm`) | 38 (+16 found s523-s525, tasks #3740-#3755, §39-§54 unwritten; +1 design choice #3805) | registry |
| of the 38: token / structure / source / lexer-dies | 28 / 7 / 1 / 2 | `kind` |
| of the 28 token rows: relex / retag / handler | 20 / 6 / 2 | `fix.kind` |
| repair subs the token rows name | **1,313 lines** (all 38 rows: 2,749) | measured s527 over `Pl/Parser.pm` `Pl/Parser2.pm` `Pl/PExpr.pm` |
| re-parse sites (serialize + `PPI::Document->new`) | 13 in `Pl/` (the parser review counted 18 incl. fragment parses) | grep |
| `Pl::Parser::_ppi_parse` on Calc.pm (new + repairs + redo) vs a plain `PPI::Document->new` | **0.95 s vs 0.25 s** (×3 each) | measured s527; the review's whole transpile is 3.7 s, so the repair layer is ~19 % |
| the planned engine (`s526/s526a-prompt.md`) | `Pl/PPIRepair.pm` ~1-2k lines, relex = re-tokenize spans in fragment documents (more PPI builds) | brief |
| the upstream report (`docs/ppi-bug-report.t`) on PPI 1.291 | **14 ok / 77 not ok** of 91 | run s527 |

## 2. Upstream, measured

- Repo `github.com/Perl-Critic/PPI`, master = v1.291 + 6 commits (whitespace; `$VERSION` 1.292 pending).  Maintainers: MITHALDU (Christian Walde) releases, OALDERS co-maintains; `CLAUDE.md` added 2026-04-26 with the PR rules: **two commits per PR -- failing tests marked `$TODO`, then the fix**; perl 5.8 compatible; `prove` runs the suite; tests are `t/data/*/NN.code` + `.dump` pairs (56 regression pairs) or unit rows.
- Release cadence: 1.285-1.291 = **seven releases in three days (2026-04-23..25)**; before that 1.284 (2025-10), 1.283 (2025-05), 1.281 (2024-12, the feature-tracking framework); nothing since April; last push 2026-06-19 (dependabot).
- PRs: **118 open, 101 of them by `toddr-bot`** (an automated campaign, April-May 2026, with its own "merge order" issue #441 and "duplicate PRs" issue #440); 11 human PRs open (two from 2014); **22 merged since 2024-01** (book, mauke, haarg, myrrhlin, happy-barney -- external humans ARE merged, in the bursts).  Issues: 120 open, 3 closed since 2025-01.
- **Our own reports are already there and unanswered: #449 (2026-06-14, `$$$ref` + the hex float) and #451 (2026-06-20, `7%-3`), filed from the Percolisp account with the report file attached.**
- The pending PRs that touch our rows, each run against `docs/ppi-bug-report.t` alone (new ok rows): #428 +1 (§1), #339 +2 (§11 family), #337 +3 (§14), #431 +2 (§15 §25), #387 +2 (§28), #361 +2 (§20), #382 +2 (§36), #366 +1 (§37); #426 #372 #410 #429 #369 #333 #433 #388 #307 #346 #427 #430 #351 #384 #383 +0.  Merged together: #431 and #387 CONFLICT, the other six give **25 ok** -- and PPI's suite then FAILS on a test-plan count.  Verdict: the campaign fixes 11 of our rows in more lines than we need, and does not combine as it stands.  Upstream is where the fixes should ALSO go; it is not a plan for PCL's timeline.

## 3. The prototype (s527, `briefs-and-rules-for-claude-subagents/s527/ppi-proto.patch`, 186 lines incl. context)

Applied to a worktree of upstream master by `scratch/ppi/proto-patch.pl` (anchored edits, each anchor unique):

| § | bug | PPI site | lines | mechanism |
|---|---|---|---|---|
| 2 | `0x1.8p+1` split in 5 | `Token/Number/Hex.pm` on_char | 13 | scan `(\.hex*)?p[+-]?\d+`, class Number::Float |
| 4 | `7%-3` = magic `%-` | `Token/Unknown.pm` `%` arm | 5 | after a term `%` is modulo (new helper `_after_term`, 10 lines, shared) |
| 12 | `)*name` = glob | `Token/Unknown.pm` `*` arm | 1 | the Number-only test becomes `_after_term` |
| 15 25 34 | `)-1`, `)-length`, `1.5-0.5` | `Tokenizer.pm` `_opcontext` | 3 | `)` and `]` end a term; `isa Number` covers Float/Exp/Hex |
| 16 | `^^` | `Singletons.pm` `%OPERATOR` | 1 | add the operator |
| 21 | term-initial `~~` | `Token/Operator.pm` | 4 | two complements unless after a term |
| 23 | `$Ｘ` = Cast + Word | `Token/Unknown.pm` `$` arm | 1 | `/[a-z_]/i` -> `/[^\W\d]/` (as `@ % * &` already do) |
| 24 | `@?` split | `Singletons.pm` `%MAGIC` | 1 | add the name |
| 28 | `$x.2` = Float | `Token/Operator.pm` | 1 | `.digit` is a number only in term position |
| 31 | `sub _` = Magic | `Token/Word.pm` (two paths) | 8 | `_` after `sub` is a Word -> `Statement::Sub` is built |

Results: our report **14 -> 33 ok** (19 rows); PPI's suite **PASS** (one plan bumped 3073 -> 3081 because `t/ppi_token_operator.t` enumerates `%OPERATOR`; **its TODO "(1)-2 not parsed correctly" now passes** -- that is our §15, a bug upstream has recorded and left); PCL's canary `Pl/t/ppi-bugs-registry-01.t -I <proto lib>` reports exactly **"PPI fixed §N: set fixed_in" for §2 §4 §12 §15 §16 §21 §23 §24 §25 §28 §31 §34** and nothing else -- the stale detector works as designed in the direction we now need.  The prototype keeps PPI's round-trip invariant (only token classes and boundaries change, never text).

**End-to-end A/B (PCL emission with the installed PPI 1.291 vs the patched one, `PERL5LIB=<proto lib>`, over ALL 111 `perl-tests/*.t` + 122 `everyday/**/*.pl`; the two preamble lines that embed `@INC` -- `*p-core-inc-dirs*` and the `vector-push-extend … pcl::@INC` of the extra dir -- dropped on both sides): 233 of 233 files BYTE-IDENTICAL, 0 real differences.**  So on the fixed stream PCL's repairs for those rows are inert and the emission does not move -- the deletions in the brief's M5 are expected to be corpus-diff IDENTICAL, and `perl-tests/hexfp.t`'s hex-float literals already reach the emission unchanged (no drop on either side).

## 4. The three routes

| | UPSTREAM ONLY (PRs, wait for a release, bump the floor) | **OUR BRANCH on upstream master, vendored + PRs** | a monkey-patch / subclass layer in `Pl/` |
|---|---|---|---|
| lines for the 28 token rows | ~150-300 in PPI + tests (same patches) | the same ~150-300, carried as commits | ~10-20 per bug as wrappers, BUT the decisions sit mid-sub (`Unknown.pm` on_char is 380 lines), so most wrappers copy the sub |
| when PCL benefits | at the next PPI release that includes them -- months, unknown | at once | at once |
| compile time | the repairs and their re-parses go (≤ 0.7 s of Calc.pm's 3.7 s; the desugaring passes stay) | same | same |
| the next PPI version | nothing to do | `git rebase` the branch; each landed PR shrinks it; the canary + PPI's suite are the bar | **breaks** (copies 1.291 internals) -- the USER's constraint rules it out |
| portability | none | pure Perl; PPI's 4 non-core deps (Clone, Params::Util, Task::Weaken, Safe::Isa) still installed by apt/cpanm | none |
| stale detection | the canary flips when a release fixes a row | the canary runs against BOTH libs: vendored (must pass `want`) and system (tells when upstream caught up) | the canary |
| ownership | none | a 676 KB / 19.6k-line copy, owned as a SHORT patch series | a shadow of PPI's internals |

## 5. The decision and its shape (USER 2026-10-24 evening; design Fable, execution = brief s527a)

1. **A fork `Percolisp/PPI`** (the USER creates it; `gh` is unauthenticated here) with branch `pcl` = upstream master + our fix commits, each fix = PPI's two-commit shape so the same commits are the upstream PRs (#449/#451 linked).  The patch series is the only thing we own.
2. **Vendored snapshot in the tree: `vendor/PPI/`** (lib + the LICENSE/Changes/README of the dist; a `vendor/PPI/UPSTREAM` stamp: upstream sha, branch sha, the list of carried fixes by §).  `pl2cl` (and every script that loads PPI) gains ONE `use lib "<root>/vendor/PPI/lib"` line ahead of its others; `PCL_SYSTEM_PPI=1` skips it (for the canary's second run and for A/Bs).  The installer copies `vendor/`; CI drops `cpanm PPI` and installs only PPI's deps; the 1.291 floor rule in `tools/install-pcl` / `ci.yml` / README / CLAUDE.md becomes "the vendored copy"; `tools/lib/PCLLicense.pm` excludes `vendor/` from tagging (PPI is "same terms as Perl", like PCL; its LICENSE stays beside it).
3. **The registry row for a fixed bug** sets `ppi.fixed_in => 'pcl-branch'` (or the upstream version once released) and gains `fix.commit => <sha>`; the canary asserts `want` on the vendored lib and reports on the system lib; `tools/ppi-expect-check.pl --mode raw` should read fewer contradictions on the vendored lib (the AFTER number).
4. **PCL's repairs for a fixed row are DELETED, one row per commit, `tools/corpus-diff.pl` IDENTICAL after each** -- the s526a bar, unchanged; the gate-SET scan over both populations at the end; the full sweep only if a diff is kept.
5. **The expect-state checker (`Pl/PPIExpect.pm`) stays** as the detector: a NEW contradiction over the four populations is a new PPI bug -> catalogue § + registry row + fix on the branch.
6. **Not changed:** the structure rows (7), lexer-dies (2), source (1) keep their PCL handlers for now -- PPI's Lexer is the same route later (`_curly` 131 lines, `_square` 37, `_statement` 240 -- the brace-class heuristics PRs #433/#382/#366 touch); the word-is-a-term family (§11, §14 partly, §19, the `-name` half of §25) needs PCL's prototype table and stays PCL's at the lowering.

## 6. The remaining token rows, with their PPI site (for the brief)

| § | PPI site | estimate | note |
|---|---|---|---|
| 1 `$$$ref` | `Token/Magic.pm` `$$`+`$` | 8 (PR #428 has it) | |
| 7 `(sub :lvalue` / 8 `my $x : shared` | `Token/Unknown.pm` `__TOKENIZER__is_an_attribute` + `Lexer.pm` `_add_element` label test (PR #430) | 10-15 | |
| 9 `${ + }` / `@{+}` | `Token/Unknown.pm` `$`/`@` `{` arms | 5 | the Magic token must keep the text `@{+}` (round-trip); our `want` says `@+` -- adjust the row |
| 20 `use experimental 'try'` | `Statement/Include.pm` | 2 (PR #361) | |
| 22 / 35 filetest after `$fh` / after `BEGIN { }` | `Token/Unknown.pm` `-` arm; §35 needs the `}`-is-a-block knowledge | 10 / brace stack | §22 needs the print-filehandle rule |
| 26 `*-` glob | `Token/Unknown.pm` `*` arm | 5 | |
| 27 `->symbol` of `*$x{SLOT}` | `Token/Symbol.pm` `symbol` cast set | 1 | |
| 30 indented here-doc | `Token/HereDoc.pm` | 3 | |
| 32 `$)` under signatures | `Token/Unknown.pm` `$` `%noname` test (from PR #318's feature tracking) | 15 | must test "inside a signature structure", a bounded back-scan like `_cast_or_op` |
| 33 / 38 `}*` after a block | `Token/Unknown.pm` `_cast_or_op` | 10 | it already scans back to the matching `{`; find why `$h{$_}; }` misleads it |
| 11 / 14 / 19 | `Token/Whitespace.pm` `/` and `<` arms, `Tokenizer.pm` `_current_x_is_operator` | partial | PPI can know core list operators (PR #337's FUN0 list, #339); a user sub's prototype is PCL's knowledge |
| 13 `$/`-dependent read | `Document.pm` | 3 | source kind |
| new §39-§54 (#3740-#3755) | mostly `Lexer.pm` (list slice, glob slot, anon hash after a list op, `continue`, `defer`, class syntax, `& {`, `require *{}`) ; tokenizer: #3744 spaced sigil (PR #388 / #307), #3748 `::::`, #3749 here-doc in a quote-like, #3750 lexsub keyword, #3751 `CORE::__END__`, #3754 `'` separator, #3755 `WORD :` in a ternary (PR #430) | per row | the registry filler writes the rows; fixes go on the branch |

## 7. What this supersedes

- `s526/s526a-prompt.md` (the expect-state REPAIR ENGINE, not launched): its token-row half is replaced by the branch; its bars (corpus-diff IDENTICAL per row, the three test files, the gate-SET scan, emission-ab, timings not slower) carry over to s527a; its structure-row half (handlers called from the checker walk) is deferred until the Lexer route is sized.
- `docs/parser-review-s525.md` §5 box step (1) "1a ONE expect-state repair pass": the pass is now PPI's own tokenizer, fixed; the checker remains.  `docs/expect-state-checker-s525.md` §5 (the relex/retag sketch) is historical.
- Task #3694's 1a session plan (`=== 1a ===`) is re-pointed to s527a.
