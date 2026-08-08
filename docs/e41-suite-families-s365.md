# #254 session 2 (s365, Opus) — the registration, then A-iv → A-i → A-iii → B-ii

Continues `docs/e41-suite-families-measurement-s363.md` (the measurement) and
executes the order Fable ruled in `fable-answers-s363.md` §5–§7.  Everything
below is measured, per file, against the ratified bar — the file's own
`docs/perl-suite-run.tsv` snapshot row (the v1-era C_ok).

## 0. The registration (required before any further #254 fix)

**`docs/perl-suite-timeouts.tsv` is new**, and `tools/run-perl-suite.pl`
honours it: `rel<TAB>seconds<TAB>cause`, effective timeout = `max(seconds,
--timeout)`, and every allowance in effect for a run is printed to the journal
AND to stderr, so it can never be a silent property of a file nobody reads.
One row today:

    re/pat_advanced.t   900   s363/#254 B-i: … needs ~600s wall …

Verified live: with the registry in place the default-timeout runner completes
re/pat_advanced.t at **936 ok / 733 not-ok** — the exact numbers s363 measured
by hand with `--timeout 900`.  Without it the file TIMEOUTs, contributes NO
rows, and its 936 recovered passes evaporate with nothing saying so (the #176
pack.t lesson, which is why Fable required this first).

The other two B-i files re-measured unchanged in the same run: op/my.t 51/8,
re/regexp_unicode_prop.t 778/332.

## 1. A-iv — the sigil-blind family-use refusal is DELETED

`_promote_captured` refused a SCALAR promotion whenever the same bare name was
used in container form (`@x`/`%x`/`$#x`) anywhere in the extent.  That predates
the sigil-exact rewriter and duplicated its knowledge as a veto:

* the Symbol loop tests `$s->symbol eq '$x'`, and PPI answers `@x` for the
  `$x[0]` element spelling — so element uses are already excluded;
* the ArrayIndex loop runs for `@`-canons only;
* the interp fixer's scalar arm carries `(?![\[\{])`, so `"$x[0]"` and
  `"$x{k}"` inside strings are left alone as well.

Nothing in the rewrite relied on the veto, so it only refused CORRECT
promotions — most of them where the container is a different variable declared
inside a sub (`my ($name, $ref, @attrs) = @_;` beside a file `my $attrs`).
The `${x}` text shape keeps its own refusal.  This is the same argument the
CONTAINER path has used since array.t's bug-70171 block, applied to the scalar
side.

**Result: op/attrproto.t de-gates and lands on 17 ok / 28 not-ok — exactly its
snapshot row.**  (re/reg_eval_scope.t, the other A-iv file, moves off the
family-use refusal and onto a THIRD cause — see §5.)

## 2. A-i — not an extent question at all: an embedded `my` the capture test could not see

The measurement read A-i as "the promotion only claims a lexical captured by a
sub defined AFTER the decl", and proposed ordering-independent promotion as a
semantic design question for Fable.  **Probing the two files says otherwise, and
no such design is needed.**  op/getppid.t:

    sub fork_and_retrieve {
        …
        die "Garbled output '$_'"
            unless my ($how, $first, $second) = /^([a-z]+),(\d+),(\d+)\z/;
        cmp_ok ($first, '>=', 1, …);          # ← the sub's OWN $first
    }
    …
    my $first = fork_and_retrieve("first");   # ← the file lexical

The sub does not capture the file lexical at all: it declares its own `$first`.
The gate fired because `_block_captures_name`'s shadow scan only knew two
declaration shapes — a `PPI::Statement::Variable`, and the `for my $x (…)` loop
head M6 added in s353.  A `my` EMBEDDED in another statement is neither, so the
sub's own lexical read as a capture.

The fix adds the general case to that scan, with perl's two scopes:

* statement-MODIFIER form (`… unless my ($a,$b) = …;`) — the names live from
  the end of that STATEMENT to the end of the enclosing block;
* compound HEAD (`if (my $x = f())`, `while (my $l = <$fh>)`) — the names live
  in that compound statement (its blocks), not after it.

Same class of blind spot as M6, and the same rule as A-v/#264: the pass that
DETECTS and the scoping the program actually has must agree.

**Results:**

| file | now | snapshot | verdict |
|---|---|---|---|
| io/through.t | **942 ok / 0** | 942/0 | **OK — fully passing, at snapshot** |
| op/getppid.t | 0/0 (crash) | 0/0 (same crash) | de-gated, at snapshot |

io/through.t is #254's single biggest recovery (942 rows).  op/getppid.t
de-gates but its first statement is `pipe my ($r, $w)`, which fails to compile
(`(p-pipe (vector $r $w))`) — the same crash signature its snapshot row
carries, so it is at its bar with nothing recovered; the pipe-decl shape is
filed as its own residue.

## 3. A-iii — a block-form package's lexicals cannot span, so they are not span candidates

`op/sub_lval.t` declares `my $x = 'nonlv'` inside `package _102486 { … }` at
line 984 and uses an unrelated `$x` at line 1021, outside that block.  The span
detector had no scope for a block-form package segment (it carries the
ENCLOSING bare block's `blk` tag), so it read the two as one spanning variable
and died; the rename pass then refused with `blockform decl segment`.

The fix is one line in each of the two passes, in the same place: a blockform
segment's declarations are not added to the live set.  It has to be a SKIP of
the declarations rather than a blk-style kill on entry — outer lexicals stay
visible THROUGH a block-form package (`{ my $x; package Foo { print $x } }` is
a real span perl resolves to the outer `$x`), and killing them on entry would
turn a die into a silently free read.  Probed both ways.

**Result: sub_lval.t's span gate is gone; the file still gates**, now on
`Parser2 TODO: C-style for with continue block`, which is an unrelated PPI
mis-lex: PPI 1.291 hands `for my $sub (sub :lvalue {$_}, sub :lvalue {…})` a
`PPI::Structure::For` instead of a `Structure::List`, so a plain foreach lands
in the C-style branch, whose defensive `continue` die then fires.  Filed as
**#268** (the discriminator is that a C-style `for` never has a loop VARIABLE
before the parens).  **#268 was then fixed in this same session — see §7; the
file now reaches its snapshot C_ok of 12.**

## 4. B-ii — the cond-my rename is shadow-aware, so a nested re-shadow no longer refuses

    while (my $i = ++$var) { …; my $i = 0; }
    continue { ($got_var, $got_i) = ($var, $i) }

`_shadow_rename_blocker` refused with `multiple declarations` because a
positional rename would have merged the two scopes.  `_rename_decl_within` is
now shadow-aware instead: it skips an inner declaration's own target
(`_is_lexical_decl_name`) and every use the inner declaration shadows
(`_ref_shadowed`, the span pass's resolver — one reducer, both passes).  The
blocker's refusal stays for every other caller and is waived only where the
caller renames shadow-aware.

**Result: op/while.t 20 ok / 6 not-ok, above its snapshot row (11/4, and that
was a TIMEOUT).**

## 5. Where the 13 files stand after s363 + s365

| file | cause(s) | state now |
|---|---|---|
| io/through.t | A-i | **OK 942/0, at snapshot** |
| op/attrproto.t | A-iv (+A-i) | 17/28, **at snapshot** |
| op/exec.t | A-v | 15/0, at snapshot (s363) |
| re/regexp_unicode_prop.t | B-i | 778/332, at snapshot (s363) |
| op/my.t | B-i | 51/8, one row short (#265) |
| re/pat_advanced.t | B-i | 936/733, regex-engine residue |
| op/while.t | B-ii | 20/6, **above snapshot** |
| op/getppid.t | A-i | de-gated, 0/0 = snapshot (`pipe my (…)`) |
| op/sub_lval.t | A-iii + #268 | **de-gated (§7), 12/58 = snapshot C_ok** |
| re/reg_eval_scope.t | A-iv+A-i | still gated — THIRD cause, `lexical 'r' possibly captured by nested sub f2` (nested-sub capture, outside the measured families) → **#269** |
| op/svleak.t, io/shm.t, op/taint.t | A-ii | PARKED behind E5 (ruled s364) |

## 6. Verification

* **Gate SET diffed file-by-file over BOTH populations** (perl-tests/*.t +
  perl's own t/*/*.t = 715 files) at each step, per the standing rule:
  **27 gated at HEAD → 26** (#263 + A-iv, de-gated op/attrproto.t) **→ 24**
  (A-i, de-gated io/through.t + op/getppid.t) **→ 23** (A-iii + B-ii, de-gated
  op/while.t) — **zero NEW gates at any step**.  op/sub_lval.t stays in the
  gate set: A-iii removed its span refusal, #268 (the PPI mis-lex) still
  blocks it.
* `tools/prove-core`: **132 files / 4735 tests, Result: PASS** (4731 + the 4 new
  foreach-aliasing rows).
* `tools/corpus-diff.pl`: **emission identical to HEAD across all 111 corpus
  files** — #263's emission change does not reach the sweep corpus (it reaches
  the perl t/ suite files, which is where it was measured).  The
  `*pcl-cache-generation*` bump to **v2-118** stands anyway: emission DOES
  change for inputs outside the corpus.
* Full **cold-cache sweep** (`rm -rf ~/.pcl-cache/*`, `--jobs 8`):
  **GATE clean — 0 new, 0 fixed, TOTAL passing 18498 = baseline (+0)**, 64
  fully-passing files, with the standing crash-file noise only (6 UNSTABLE / 4
  unverified across method.t, postfixderef.t, ref.t, tr.t — all already
  PARTIAL, same set as s341).  min MemAvailable 3.8 GB.
* Suite `--all` run recorded in `docs/perl-suite-run-s365.tsv` (the v1-era bar
  in `docs/perl-suite-run.tsv` is left untouched — it is what #254 measures
  against).

**Gotcha worth writing down**: do NOT run the gate under `nohup` — nohup
ignores SIGHUP, perl then reports `$SIG{HUP}` as defined, and
transpile-test-06.t's `%SIG` row fails against a PCL that (correctly) knows
nothing of the inherited disposition.  It cost this session one false FAIL.

---

## 7. #268 — the PPI attribute/label mis-lex (the last file in §5's table)

`op/sub_lval.t` was the one file A-iii de-gated *partly*: its span refusal was
gone, but the transpile still died on `Parser2 TODO: C-style for with continue
block`.  Two layers, both fixed here:

1. **The die itself.**  PPI had lexed the loop parens as a
   `PPI::Structure::For` (a C-style `for(;;)` header).  `_lower_compound` now
   re-blesses that to `PPI::Structure::List` when there is a loop VARIABLE or
   no `;` separator — a C-style `for` never has a loop variable before its
   parens.

2. **Why PPI got there** — a genuine 1.291 LEXER BUG, now
   `docs/ppi-upstream-bugs.md` §7.  At the START of an expression an anon sub
   carrying an attribute becomes `Label('sub :') Word('lvalue')`; chained
   attributes chain as more Labels; inside a `for` list each label gets a
   STATEMENT of its own.  Mid-expression the same text tokenizes correctly,
   which is exactly why `my $f = sub :lvalue {…}` worked and
   `(sub :lvalue {…})` did not.  Nothing downstream could see an anon sub, so
   the expression fell through to `Missing case: [` and the whole statement was
   replaced by a PARSE ERROR comment — **a silent code drop**, the same family
   as #138 and #259.

   `Pl::Parser2::_normalize_anon_sub_attrs` (document level, beside the other
   PPI repairs) merges the split statements, drops the attribute run — including
   `:prototype($$)`'s own parens — and re-blesses the `Label` into a plain
   `Word('sub')`.  It only fires when the run ends at the sub's block; anything
   else is left alone rather than guessed at.

   The CORRECTLY-lexed mid-expression spelling had no handler either
   (`my $one = sub :lvalue { 7 }` was its own "Missing case"); PExpr's existing
   "strip the prototype after `sub`" pass in `handle_subcalls` now consumes
   interleaved `(':' Attribute)` pairs as well — reuse, not a second copy.

`:prototype(…)` is the one attribute that normally SURVIVES the drop, as a
runtime `__pcl_set_prototype` wrap emitted by
`Pl::Parser::_extract_prototype_attributes` — but that pass keys on a
`PPI::Token::Attribute`, the very token PPI failed to produce, and it cannot be
re-run after the repair without a reparse that re-creates the mis-lex.  So the
repair drops it, **announces on stderr naming the attribute**, and carries a
`docs/not-supported.md` entry.  Effect-only under the s329 boundary (an anon
sub has no name for the call-site parser to consult; even the correctly-lexed
spelling only records it at runtime), and the shape occurs in neither audit
population — only the mid-expression `my $t118 = sub :prototype($) ($a) {…}`
does, which is untouched and still passes.

**Result**: `op/sub_lval.t` **211 P / 12 C_ok / 58 C_fail = its snapshot C_ok
exactly** (the bar).  Its residue is `undef-fn:main::pl-rlv1t`, a different
axis.  Two more files move on the same fix:

| file | before | after |
|---|---|---|
| op/anonconst.t | 0 ok / 1 fail (crash: `Undefined subroutine &main::`) | **1 ok / 6 fail**, no crash — above its snapshot C_ok of 0 |
| perl-tests/hashassign.t | `$_++ foreach sub :lvalue { … }->()` emitted a PARSE ERROR comment | real anon-sub call; file still **309/309 fully passing** |

(hashassign.t is the one CORPUS file whose emission changes — verified with
`tools/corpus-diff.pl --show`, and re-swept single-file to confirm the newly
live statement costs nothing.  op/attrs.t and uni/attrs.t contain attributes
too but stay gated on an unrelated cause, `my ($cows, @go, %bong) : teapots`.)

Gate SET over both populations **23 → 22, zero new gates**.  Guard rows:
`Pl/t/transpile-test-09.t`, one snippet covering statement position, list
position, chained attributes, the `for`-list-with-`continue` shape, a named
sub's attributes, and the INVERSE guard that a real loop label still lowers as
a label.  `*pcl-cache-generation*` → **v2-119**.
