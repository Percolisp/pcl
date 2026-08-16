# Review requests — session 408 (Opus 5, 2026-08-16)

Session **F** of `docs/plan-post-s400.md` §2d: **#337, the rename half**.

## 1. What landed

| commit | what |
|---|---|
| `7f930fc` s408a | **#337** — `my sub NAME` / `state sub NAME` are LEXICALS: a scope-unique rename + the uses their region owns, including the code inside interpolating text |
| `719ecf0` s408b | **#360** — the core feature pragmas (`use v5.40`, `use experimental`) answered by a table, through PPI's own hook |
| `ed67333` s408c | **#364** — a string eval inherits the perl features in effect at its site; they key the eval cache |
| `850a4bf` s408d | **#363** — an eval-mode DROP dies into `$@`; and **#375**, the `qq {…}` silent-wrong it exposed |

*(§§2–8 below are #337's; #360/#364 are §9, #363/#375 are §10.)*

Docs/tasks in the same commit: `DECIDED.md` s408 section, `session-log.md`
s408, `not-supported.md` (one new section + one paragraph added to the #347
"will not stay shared" family), `Pl/t/lexical-sub-01.t` (18 oracle rows),
task #337 marked completed with its outcome, **task #373 filed** (§4),
`docs/perl-suite-run.tsv` (one row spliced with its cause).

## 2. The bug and the fix

PCL compiles every named sub as a PACKAGE sub, so two `my sub x` in different
scopes clobbered each other and every reference — including one captured in a
closure built *before* the second declaration ran — resolved to whichever was
defined LAST.  Silent: no warning, no die, a wrong value.

    my $g1 = sub { my sub x () { 8 } \&x };
    my $g2 = sub { my sub x () { 3 } \&x };
    print &{$g1->()}, " ", &{$g2->()};        perl: 8 3      PCL: 3 3

`Parser2::_rename_lexical_subs` gives each declaration a scope-unique name
(`NAME__lexsub__N`) and rewrites the uses its region owns — the `_rename_*`
family's answer for `my $x`, applied to a name that happens to be a Word.  The
region runs from the DECLARATION (not the top of the block: a call before it
still reaches the package sub) to the end of the enclosing scope, and stops at
a sibling redeclaration (#296-B2).

**Nesting needed no shadow test.**  A use is claimed by the covering
declaration with the LATEST start, which is by construction the innermost one
in scope there — so shadowing, sibling redeclaration and the
before-the-declaration rule all fall out of one comparison.  That is the part
worth reviewing: `_ref_shadowed`'s variable-side equivalent is a scope walk,
and this pass deliberately has none.

## 3. The hole the design missed, and the probe that caught it

A rename that rewrites only the TOKEN STREAM is incomplete.  Interpolated CODE
— `"@{[ f() ]}"`, the same inside a heredoc or a pattern — is compiled from the
STRING's TEXT, so the embedded call still named the package sub, which after
the rename does not exist:

    my sub f { "L" }  print "got @{[ f() ]}\n";
        before the fix: "the function main::pl-f is undefined" — a CRASH THIS
        PASS INTRODUCED, in code that worked before it

The fix takes the spans from `Pl::InterpScan` (standing rule §8) and classifies
the code inside one by **parsing it as Perl** through the SAME predicate as the
token stream — never by matching the name in text, where it could equally be a
string, a hash key or a method (probed: `"@{[ 'f' ]} @{[ f() ]}"` renames only
the second).  It is wired through the existing `_fix_interp_token`, so heredoc
bodies and patterns come free and `heredoc_is_raw` keeps `<<'E'` out of it.

## 4. Registered, not fixed — three divergences

New `not-supported.md` section "A lexical sub (`my sub NAME`) reached from a
place that is not the token stream":

1. **A STRING eval cannot see a lexical sub** (`my sub f {…}; eval "f()"`) —
   perl finds it in the pad; PCL's capture alist carries VARIABLES only.
   **Task #373, filed.**  Be precise about what this is: before the rename it
   "worked" only because every lexical sub WAS a package sub — the same
   accident that made two of them clobber each other.  It is loud and
   trappable, never a wrong value.  Sized WITH #364 (the same seam: presumed
   state that rides the request and keys the eval cache).
2. **A body's call to its own name is accepted where perl DIES** — principle 9;
   `t/op/lexsub.t` asserts perl's answer, so those rows fail there by design.
3. **Shape 10** (fresh closure per loop iteration) — recorded on the #347
   "will not stay shared" family as ruled, not promised with the rename.

## 5. Measurements

| measurement | value |
|---|---|
| Probes vs perl 5.40.3 | **41 shapes, 38 identical**; the 3 divergences are the registered ones above |
| Gate `tools/prove-core` | **147 files / 5359 rows**, PASS except the 13 pclxs xs rows |
| Gate vs a HEAD worktree, per file | **IDENTICAL** — same row count and same failures in all 147 |
| `tools/corpus-diff.pl` | 3 of 111 differ (eval.t, sort.t, sub.t); every hunk a name substitution; silent drops 12 unchanged |
| `tools/emission-ab.pl` over `lib/` | 20 files, **20 SAME** |
| Full perl-tests sweep | **GATE clean, 0 new / 0 fixed, TOTAL 18517 = baseline, drops 12 = census** |
| the three emission-changed files, per row | at baseline: eval.t PARTIAL 127/33/169, sort.t OK 203/1/205, sub.t OK 62/2/65 |
| Companion `--all --quick --jobs 4` | 509 of 523 measured, **3 rows differ**: `op/const-optree.t` 86/62 → **90/58** (the fix, confirmed solo), + 2 known noise (§7) |
| `op/const-optree.t` | now **REGISTERED** (XDIFF), rows blessed, after a fresh per-row read of all 58 |
| Drop census | `t/op/lexsub.t` **6 → 10**, deliberate — see §7 |
| Gen / artifacts | v2-151 → **v2-152**, all three regenerated (stamp-only diffs); pack.t **5636/89 = blessed** |
| Final gate | **148 files / 5377 rows** (the new guard file included) |
| `Pl/t/lexical-sub-01.t` | 18 oracle rows, 52 s |

**On the gate row count:** the written-down number is 147/**5355** and this
session measured 147/**5359**.  That is not a finding — the per-file worktree
compare above shows the two trees identical file by file.  The pclxs xs files
abort at different points as pclxs is worked on, so their row counts move on
their own; the number to compare against is a *measurement of the same tree*,
not a written-down one.

## 6. `op/const-optree.t` is registered at last — and it is the worked example

The four `retval of my sub …` rows were the file's only non-readout
divergences, and they are why it stayed UNEXPLAINED for nine sessions: the
s397 ruling had authorised registering it, and the per-row read the bar demands
was the thing that found a real fix target inside a file everyone was ready to
bless.  With them fixed, a fresh read of all 58 remaining rows gives 28
inlinable + 25 `:method` readouts (§Readouts of perl's own internals) and 5
`RT 134138` rows (§Error compatibility for invalid Perl input) — all-or-nothing
satisfied, both reasons cited, rows blessed, verified XDIFF.

## 7. The drop census took its first INCREASE — this is the ask

`t/op/lexsub.t` **6 → 10**, and I want this one ruled rather than assumed.

perl lets a lexical sub take a KEYWORD's name and that file asserts it:

    { our   sub if() { 42 }  my $x = if if if;  is $x, 42 }   # line 71
    { state sub if() { 44 }  my $x = if if if;  is $x, 44 }   # line 209
    { my    sub if() { 44 }  my $x = if if if;  is $x, 44 }   # line 576

* **Renamed** (`my`/`state`, 4 statements): the statement becomes three
  juxtaposed zero-arg calls → the term grammar cannot lower it → **announced
  drop**.
* **Not renamed** (`our sub if` — a package sub by design, 2 statements): still
  read by the KEYWORD parser, still emits `(p-if (p-if) (p-my-= $x (p-if)))` —
  **a zero-argument `p-if`, and its macroexpansion error IS this file's crash
  cause** ("Form: (p-if) Compile-time error: during macroexp"), which is why
  the file stops at 6 of 156 rows.

So the edit trades 4 crash-forms for 4 counted drops, and the file's verdict is
unchanged (DIFF 6/8) because the `our sub` pair comes first.  My reading: a
counted, announced drop is strictly better than a form that cannot expand, and
the census exists to make drops countable rather than to freeze them — so the
census row is EDITED UP with the trade argued in its header, and task **#374**
owns the residue.  **Please rule**: is "census up, with the trade argued in the
edit note" the right shape when a change converts crash-forms into drops, or
should such a case be held until the drop itself is fixed?

Two sub-questions inside #374, if the shape is accepted:

1. The zero-argument `p-if` is a rule-12 case in the statement lowerer (a
   statement-modifier `if` with no condition should DIE naming the shape, not
   build a form that cannot expand).  Fixing it is cheap-ish and would let
   `t/op/lexsub.t`'s remaining ~150 rows be measured for the first time — worth
   doing BEFORE the term-grammar half?
2. The term-grammar half (`TERM TERM TERM` of declared empty-prototype subs) is
   Option B phase 2's #372 family; I did not touch the `$end_pars` region.

## 8. Other asks

1. **The gate row count** (§5): I treated a written-down 5355 vs a measured
   5359 as *not a finding* on the strength of a per-file worktree compare.  Is
   that the rule you want stated — "compare a measurement against a measurement
   of the same tree, never against a number in a doc" — given the pclxs files
   move on their own while pclxs is under separate work?
2. **The string-eval hole (#373)**: registered rather than fixed, on the
   grounds that it is loud, never a wrong value, and that its "working"
   behaviour before was the same accident as the bug.  Confirm it belongs with
   #364 (both need state that rides the eval request and keys its cache), and
   that it should not jump the queue.
3. **The interpolation reach** (§3): I extended the rename into embedded code
   via `Pl::InterpScan` + a mini-parse of each span.  That is a second place
   that classifies Perl tokens; it reuses the same predicate rather than
   duplicating it, but it does parse a fragment with a fresh
   `PPI::Document->new`.  Is that acceptable, or should the fragment path be
   folded into something already in the pipeline?

## 9. #360 + #364 — the feature pragmas, and what a string eval inherits

**#360.** `use feature 'try'` was the only one of perl's three spellings PCL
compiled: `use v5.40; try {…} catch ($e) {…}` was a **whole-statement DROP**
(announced, rc 0) and `use experimental 'try'` did not compile at all.  Both
are PPI (`ppi-upstream-bugs.md` §20, Bug 17 — three new failing rows in the
report), and PPI has the hook, so the fix is a table consulted before its
built-in logic.  Two decisions the ruling did not spell out, both measured: a
version bundle REPLACES the scope's feature set (so the "off" answers are
explicit), and an argument outside the table returns an EMPTY answer rather
than falling through — PPI's `experimental` branch answers `signatures => 0`
for *any* list, which would silently disable signatures.

Measured: corpus-diff IDENTICAL, lib 21/21 SAME, **perl's own t/ 560/560 SAME**,
op/try.t unchanged.  Nothing in either population uses `use experimental`
except op/taint.t, which is TRANSPILE-FAIL for an unrelated cause.

**#364.**  The features ride the server request as a fifth protocol line, seed
PPI's lexer through the document's `feature_mods`, and join the eval CACHE key.
corpus-diff moved exactly one file (signatures.t), and the difference is proven
*mechanically* to be only the 530 feature arguments: strip them from the new
emission and the whitespace-normalized files are byte-identical.

Two things cost time and are in DECIDED because they will cost the next session
time too: **`lex_home` IS the Parser2 object** (a per-statement value published
under the same key as a per-document map read as "Not an ARRAY reference"), and
**the per-statement hook is `_lower_block`, not `_lower_stmt`** — the
declaration paths (`my $r = eval "…"`) never reach the latter.

## 10. #363 + #375 — the flip, and the -66 that was not the flip's

**The first measurement was a lie, and reading it is the finding.**  The sweep
after the flip said **-66 rows**, of which **-61 was `index.t` alone**.  That
file does `eval $test_str; die $@ if $@;` — its own logic — and its
`$test_str` was MALFORMED by a *separate* bug: **`qq {…}` with whitespace
before the delimiter kept the delimiters in the value** (#375).  PCL took the
character right after `qq` — the space — as the delimiter.  Only the
interpolating form was wrong, which is why it had survived; three corpus files
were producing wrong strings.  PPI's `->string` has always been right.

With #375 fixed, the flip's real cost is **4 rows net** in perl-tests and
**14 moved rows** in the companion, every one the same family: an eval whose
text PCL cannot lower now reports `$@` instead of silently returning undef.

| | |
|---|---|
| WINS | op/cmpchain.t **+45**, op/signatures.t **+21** (both registered — rows re-blessed, XDIFF again, 66 fewer diverging rows), op/magic.t +1, op/eval.t +1 |
| LOSSES, all false passes | op/smartmatch.t **143 → 44**, dor.t -3, glob.t -3, postfixderef.t -2, exec.t -2, comp/parser.t -2, yadayada.t -1, retainedlines.t -1, packagev.t -1 |

`op/smartmatch.t` is the one to look at: it does `$res = eval $tstr; if ($@ ne
'') { fail }`, so 99 unsupported `~~` expressions were dropped in silence, `$@`
stayed empty, and `undef` happened to satisfy the "does not match" assertion.
Those rows were never measuring anything.

### Asks

4. **Is the -46 net in the companion the trade you want?**  I believe yes — every
   lost row asserted "no error" about code PCL genuinely cannot compile — but it
   is a big number and it is the first time this project has traded ~100 rows for
   honesty in one go.  The alternative I did NOT take, and which the ruling's own
   rationale ("the reader of stderr does not exist here") would allow: carry the
   announcement BACK over the protocol and print it from the runtime, so the
   drop is loud without being fatal.  That costs zero rows and still kills the
   silence.  Worth a ruling before Option B phase 2 makes the same choice for
   file mode.
5. **#375's blast radius**: I fixed the two hand-strip sites I found by grep.  If
   there is a third place that strips quote delimiters by hand, it has the same
   bug — a sweep of that pattern might be worth a filler.
