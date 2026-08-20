# The flip's re-census — every drop classified, and the flip verdict (s419, Fable, 2026-08-21)

This is the re-census the queue ordered after Track A + B1 + B2 + #369/#370
(`docs/option-b-phase2-plan.md` §3; `docs/fable-answers-s400.md` §6.4;
ruled obligation `docs/fable-answers-s415.md` §7.2).  It answers ONE
question: **may the `PARSE ERROR` emitters flip from announce-and-drop to
DIE in file mode?**

## 1. The measurement

* `tools/drop-census.pl . census-s419.tsv 8` at `7a03d93`: **42 files /
  135 drops — row-for-row IDENTICAL to the blessed
  `docs/parse-error-drop-census-s399.tsv`** (diff of sorted non-comment
  rows: empty).  The blessed census needs no edit.
* `tools/drop-harvest.pl` over those 42 files: 133 statement texts (2
  same-line duplicates deduplicated by the announcer, as designed).
* Every row below was read; three were probed this session (§4).

**Refused files** (the §7.2 obligation: list them so "378 → 135" is never
read as "the underlying problems fell").  These files die loudly at
transpile under the Track A refusals and therefore carry no countable
drops; the features stay tracked in `docs/not-supported.md` + #399/#400:
`t/op/switch.t`, `t/class/*.t` (7 files), `t/op/smartmatch.t`,
`t/op/defer.t`, the format torture rows of `t/op/write.t`/`t/uni/write.t`
that the source-level stripper does not remove, and the two `state.t`
files (#400).  A refusal is flip-legal — it already dies.

## 2. The classification — all 135, by family

| # | family | drops | rows | owner / verdict |
|---|---|---:|---|---|
| 1 | lvalue-sub assignment (`f() = …`, `&$r(0) = …`) | 39 | sub_lval.t 33, op+perl-tests substr.t 2+2, try.t 1, signatures.t 1 | **registered** (`not-supported.md` §Lvalue subroutines) + **RULED to stay a loud drop** (`fable-answers-s400.md` §6.3) — permanently EXEMPT from the flip |
| 2 | non-ASCII identifiers in stashes/globs/filehandles (`delete $ＦŌŌ௨::{Ƒ}`, `$Ｊ{a}`, `<Ạ>`, `$ᛁ앛…::ISA[1]`, `++${"\xff::foo"}`) | 21 | uni/gv.t 8, uni/stash.t 4, uni/caller.t 2, uni/method.t 2, uni/readline.t 2, mro/basic_utf8.t 1, mro/package_aliases_utf8.t 2 | **NEW task #410** — the largest non-exempt family; E5-adjacent naming |
| 3 | lexsub call-position residue (`my $x = if if if`, statement-initial `x` / `x();`, `my sub φου`, nested `state sub` torture, `eval "\\&x"` inside nested subs) | 12 | op/lexsub.t | **existing #374(b) + #365** — waits for `_reduce_term` (B3) |
| 4 | sigil-block / glob-surgery odd spellings (`$${$_[0]}`, `*{;undef} = 3`, `*X = *-`, `local *a = *1`, `*^R = *…`, `++${"23::foo"}`, `${no strict; \$_}`) | 12 | op/gv.t 3, uni/gv.t 2, reg_namedcapture.t 2, re/subst.t 2, re/pat.t 1, universal.t 1, op/lex.t 1 | **#153 B3 residue** — listed for B3's re-measure acceptance set (pointer appended to #153); by-design decline of the term walker (DECIDED s398), the fallback lacks the case |
| 5 | postfix call-of-call-result chains (`$s2->()()`, `$subsubs[0]()(0)`, `(sub {"bar"})[0]()`) | 8 | op/closure.t 2 + perl-tests/closure.t 1, current_sub.t 3, op/ref.t 1 + perl-tests/ref.t 1 | **NEW task #411** — blocked on #153 (B3), same standing as #147 |
| 6 | deliberate parse-error torture rows (`print 1+`, `"${; =pod =cut }"`, `sub 'Hello'_he_said (_);`, bodiless heredocs, `open(…), || _diag $!`) | 7 | comp/parser.t 5, comp/final_line_num.t 1, io/open.t 1 | **registered** (§Error compatibility for invalid Perl input; principle 9) — test-deliberate, flip-legal by the ruled bar |
| 7 | qualified-name prototype declaration: `sub main::end(&)` inside `package End` is invisible to the block-form prototype mechanism, so `my $c = end { … };` drops | 6 | die_unwind.t 4, die_except.t 2 | **NEW task #413** — a mechanism gap (prototype extraction keyed on unqualified names), not a feature absence |
| 8 | regex code blocks / control verbs — `/(a+b?)(*SKIP)(?{…})(*FAIL)/` (PPI mis-tokenizes the pattern; the feature inside is itself unsupported) | 5 | re/pat.t 1, re/pat_advanced.t 4 | **registered** (§Regex code blocks `(?{code})`) |
| 9 | `$#{^CAPTURE}` (last-index through a caret-name block deref) | 5 | re/pat.t | **NEW task #412** |
| 10 | indirect-object method calls (`method $obj "a"`, `doit $object "FOO"`) | 4 | op+perl-tests method.t 1+1, op+perl-tests ref.t 1+1 | **existing #399** (ruled OUT of Track A: the files carry 288 passing rows) |
| 11 | prototyped parenless list-op family (`1 == a_hash 'a'`, `(unilist3 0 || 5) == 6`) | 3 | comp/proto.t | **existing #259** (blocked on #153 step-3b arity work) |
| 12 | interpolation `Number:?` leaf — `"$_[!$_[2]]"`-style subscript interpolation dies with `no form emitter for expression leaf PPI::Token::Number:?` | 3 | op+perl-tests postfixderef.t 1+1, numconvert.t 1 | **NEW task #414** |
| 13 | odd singles, each one row (list in §3) | 9 | scattered | **NEW task #415** carries the list + probe notes |
| 14 | registered absences, one row each: format torture inside `eval q\|…\|` (write.t 1930), hex-float block (perl-tests/sprintf2.t 739) | 2 | write.t, sprintf2.t | **registered** (§format/write; §Hex floating-point literals) |

Sum: 39+21+12+12+8+7+6+5+5+4+3+3+9+2 = **135**. ✓

## 3. The odd singles (family 13, all in task #415)

| file:line | text | probe note |
|---|---|---|
| re/subst.t:346 | `ok( ! @?, 'parsing of split subst with comment' );` | **probed s419: `@?` is legal perl** (punctuation array); PCL drops — real gap, family of the `%SPECIAL_VARS` machinery (#89) |
| op/utftaint.t:18 | `any_tainted @_;` (inside `sub tainted ($)`) | **probed s419: does NOT reproduce isolated** — the minimal `(@)`-prototype + parenless-call shape transpiles and runs; the in-file trigger is contextual, unlocated |
| op/filetest.t:161 | `sub _ { … } is(-f _, 1, …)` | `_` defined as a sub while used as the stat-buffer bareword; #403-family fidelity edge |
| op/glob.t:110 | `ok <~>, '~ works';` | `<~>` tilde glob |
| op/utf8cache.t:70 | `substr $x, 0, 1, = "\x{100}";` | 4-arg substr spelled with a trailing comma before `=` |
| op/sub_lval.t:970 | `continue { … }` (in an rvalue-return context) | torture row of the lvalue file |
| op/sub_lval.t:1065 | `{; @119797 }` | numeric array name in a bare block |
| perl-tests/closure.t + op/closure.t share their `$s2->()()` rows with family 5 | — | — |
| op/write.t and uni/gv.t remainders are counted in their own families above | — | — |

(The table lists 7 texts; family 13's count of 9 includes the two
`sub_lval.t` fell-through rows, which the census reason column separates
from its 33 lvalue-reason rows.)

## 4. THE VERDICT: the flip is BLOCKED — by the plan's own bar, not by a new judgment

The ruled bar (`option-b-phase2-plan.md` §3): the flip may land when every
remaining drop is a deliberate error test, a registered absence, or an odd
single explained — and §6.4: "once the census is explained and near zero".

The census IS now fully explained (every row above has an owner), but it is
not near zero, and the arithmetic is:

* 39 lvalue rows — **permanently exempt** (ruled §6.3, the drop IS the design);
* 14 rows flip-legal today (7 test-deliberate + 5 §regex-code-blocks +
  §format 1 + §hex-float 1);
* **82 rows are genuine compiler gaps** (families 2, 3, 4, 5, 7, 9, 10,
  11, 12, 13) sitting in files that today contribute large passing-row
  counts: ref.t 191, method.t 97, closure.t 272, op/lexsub.t 52, re/pat.t,
  uni/gv.t, op/gv.t, comp/proto.t.  Flipping now would TRANSPILE-FAIL
  ~30 productive files — the state.t lesson (88 rows) and the exact
  reasoning that ruled indirect object OUT of Track A (#399, 288 rows),
  at ten times the scale.

**Unblock list** (the flip's precondition, in value order): #410 (21),
#374(b)/#365 via B3 (12), #153-B3 residue (12), #411 (8), #413 (6), #412
(5), #399 (4), #259 (3), #414 (3), #415 (9).  When those land, re-run this
census; the remainder should be the exempt lvalue family + the
test-deliberate/registered rows, and the flip proceeds with the s373
three-leg bar (gate-SET scan over both populations, sweep TOTAL/LOST,
corpus-diff).

## 5. The flip increment that is NOT blocked — and why it still waits

`pl2cl --module` (the runtime transpiling a module mid-run) announces
NOTHING (ruled s403) and the runtime discards the server's stderr — so a
drop inside a USER's module is today the last fully SILENT member of the
#138 family (the census covers the SHIPPED lib/, which is at zero, and the
cpan corpus via corpus-diff; it cannot see an arbitrary user module).
Flipping module mode to DIE would be rule-12-correct and touches no test
population.  It waits for ONE measurement: a cpan-board re-run counting
which board modules carry drops (a module-mode die aborts the program at
first cold-cache transpile of a dropping module).  Candidate first
increment of the flip when the board shows zero or the affected modules
are already FAIL.  Recorded here so the next session can take it as a
measured, single-mechanism step.
