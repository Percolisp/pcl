# The flip's re-census, second pass — every drop classified and PRICED (s431, Opus 5, 2026-08-22)

*Session P1 of `docs/plan-post-s430.md`.  Supersedes
`docs/drop-census-s419-flip-gate.md` as the current census reading; that file
stays as the s419 measurement and its family numbering, which this one keeps.
No compiler change was made this session — P1 is measurement.*

It answers the same one question as its predecessor — **may the `PARSE ERROR`
emitters flip from announce-and-drop to DIE in file mode?** — and adds the
thing s419 could not: **what each option COSTS in passing rows.**

## 1. The measurement

* `tools/drop-census.pl "$PWD" census-s431.tsv 8` at `54e2d80` on a COLD cache
  (`rm ~/.pcl-cache/*.lisp` first): **27 files / 82 drops, row-for-row
  IDENTICAL** to the blessed `baselines/parse-error-drop-census-s399.tsv` (diff of
  the sorted non-comment rows: empty).  The blessed census needs no edit.
  1m16s at 8 jobs.
* `tools/drop-harvest.pl` over it: **82 statement texts**, one per drop — the
  table below is row-level, not family-level.
* **About 60 probe shapes vs perl 5.40.3** this session (§3, §5), in a dozen
  batches — because six rows' s419 classification did not survive contact with
  `perl` (a mis-classified row is a mis-priced flip), and because eight of the
  module drops minimised to three one-line bugs.
* Per-file row costs: `.faillog/_status.tsv` (the s428 sweep, TOTAL 18367) for
  the four `perl-tests` files and `baselines/perl-suite-run.tsv` for the 23
  companion files.  **`t/op/lex.t` had no snapshot row at all** — see §6.

Since s419 the census has gone **135 → 82** (42 → 27 files).  Closed in
between: #410 (17 of its 21), #411 (8), #413 (6), #412 (5), #259 (3), #414
(3), #415's `@?` and `<~>` (2), plus the s427/s430 movers.

## 2. All 82 rows, by verdict

The four verdicts are the ones the flip's bar is written in
(`docs/option-b-phase2-plan.md` §3: *a deliberate error test, a registered
absence, or an odd single explained*).

| verdict | drops | files |
|---|---:|---|
| **exempt** — lvalue-sub assignment, ruled a permanent loud drop | 39 | 5 |
| **registered** — a `not-supported.md` absence | 7 | 4 |
| **deliberate** — the test IS a syntax error | 1 | 1 |
| **needs-a-ruling** — indirect object, USER "maybe later" | 4 | 4 |
| **gap-with-task** — a compiler gap, every one now owned | 31 | 15 |

### 2.1 exempt — 39 rows (family 1)

`f() = …`, `&$r(0) = …`: `not-supported.md` §Lvalue subroutines, **ruled to
stay a loud drop** (`fable-answers-s400.md` §6.3).

| file | rows | sample |
|---|---:|---|
| t/op/sub_lval.t | 33 | `get_st = 7;`  `alv2(20) = "ok 51\n";`  `&$pie($depth) = $value;` |
| t/op/substr.t + perl-tests/substr.t | 2 + 2 | `bar = "XXX";`  `ta_tindex() = 23;` |
| t/op/signatures.t | 1 | `f() = "X";` |
| t/op/try.t | 1 | `fscalar = 123;` |

### 2.2 registered — 7 rows

| file:line | text | not-supported.md section |
|---|---|---|
| t/re/pat_advanced.t 1235,1255,1267,1285 | `/(a+b?)(*SKIP)(?{…})(*FAIL)/` | Regex code blocks `(?{code})` |
| t/re/pat.t 934 | `1 while /b(?{$foo = $_; $bar = pos})c/g;` | same |
| t/op/write.t 1930 | format torture inside `eval q\|…\|` | format/write |
| perl-tests/sprintf2.t 739 | the `0x0.b17217f7d1cf78p0` block | Hex floating-point literals |

### 2.3 deliberate — 1 row

`t/comp/final_line_num.t:13` — `print 1+`.  The file exists to produce a
syntax error at EOF and assert its line number.  **It is the ONLY row of the
82 that is deliberately invalid Perl** (s419 counted seven; §3 corrects that).

### 2.4 needs-a-ruling — 4 rows (family 10, #399)

`method $obj "a","b","c"` (op+perl-tests/method.t) and `$foo = doit $object
"FOO";` (op+perl-tests/ref.t).  Indirect object is **MAYBE LATER** (USER,
DECIDED s425): not refused, because the four files carry 583 passing rows.
#381 is the crash twin (op/lexsub.t).

### 2.5 gap-with-task — 31 rows

| # | shape | rows | files | owner |
|---|---|---:|---|---|
| a | a lexical sub named after a QUOTE-LIKE operator (`x`, `y`) | 4 | t/op/lexsub.t 448,751,836,851 | **#460 (new)** |
| b | glob surgery / symbolic ref: `$${$_[0]}` ×4, `++${"…"}` ×3, `*X = *-` ×2, `local *a = *1` ×2, `*{;undef} = 3`, `*^R = *g` | 13 | op/gv.t, uni/gv.t, op/universal.t, uni/method.t, re/reg_namedcapture.t, re/subst.t, re/pat.t | **#463 (new)** |
| c | a non-ASCII bareword filehandle in `<…>` | 2 | t/uni/readline.t 24,26 | **#461 (new)** — #410 residue |
| d | `${no strict; \$_}` (a BLOCK deref with statements) | 1 | t/op/lex.t 99 | #415 |
| e | valid-perl torture: pod in `"${; … }"`, `sub 'Hello'_he_said (_);`, two heredocs on one line, a heredoc containing `#line` | 5 | t/comp/parser.t 432,456,733×2,764 | #415 (moved in, §3) |
| f | `ok open(…), 'desc', \|\| _diag $!;` | 1 | t/io/open.t 267 | #415 (moved in, §3) |
| g | `sub _ {…}` while `_` is used as the stat buffer | 1 | t/op/filetest.t 161 | #415 / #403 family |
| h | 4-arg `substr $x, 0, 1, = "…";` | 1 | t/op/utf8cache.t 70 | #415 |
| i | `ok /a*b?c*/, qq [...]` — the §11 repair declines on a quantifier | 1 | t/re/pat.t 106 | **#458 (new)** |
| j | `continue { … }` on a foreach; `{; @119797 }` in an lvalue body | 2 | t/op/sub_lval.t 970,1065 | #415 |

39 + 7 + 1 + 4 + 31 = **82** ✓

## 3. Six rows whose s419 classification (or owner) did not survive a probe

The rule that produced them: **a dropped statement inside a file that `perl`
itself compiles is valid Perl by construction** — the only exception is a file
designed not to compile.  Probed one by one (`perl` vs `./runpcl`, minimal
files):

| row | s419 said | probe | now |
|---|---|---|---|
| t/io/open.t:267 | deliberate torture | `ok $x, 'desc', \|\| _diag "boom";` → perl `ok(1 desc)`, PCL drops | gap (#415) |
| t/op/utf8cache.t:70 | odd single | `substr $x,0,1, = "Z"` → perl `Zbc`, PCL prints `abc` — **silent wrong** | gap (#415) |
| t/comp/parser.t ×5 | deliberate torture | the file compiles under perl; each row is legal | gap (#415) |
| t/op/lex.t:99 | family 4 | `print ${no strict; \$_}` → perl `rhubarb`, PCL drops | gap (#415) |
| t/re/pat.t:106 | unattributed (s419 places 7 of re/pat.t's 8 rows) | a paren-less `ok /a*b?c*/` — PPI §11, the repair declines | gap (**#458**) |
| t/uni/gv.t ×2 | family 4 in s419, "non-ASCII names" in plan-post-s430 §0 | `$${$_[0]}` — PPI reads `$$` as the PID; nothing Unicode about it | gap (**#463**) |

Two further probe findings, filed, that are NOT census rows (nothing drops —
the wrong answer is produced silently):

* **#459** `sh(/a*b/, "d")` passes **2** arguments where perl passes 1: a
  failed match in list context contributes `""` instead of the empty list in
  an ARGUMENT list (the assignment path is right).
* **#460** `{ my sub y { 8 } print y, "\n" }` prints **0**; perl prints 8.

And one more that a drop hides: several drops **swallow the following
statement**, because PPI's mis-lex runs past the `;` — measured for
`ok /a?/, "d";`, `{ my sub x {7} x; print …; }` and `sub _ {…} is(-f _, …)`.
A census row is therefore a LOWER bound on what a drop costs.

## 4. THE PRICE OF THE FLIP — the table P1 owes the design

A file whose transpile DIEs contributes zero rows.  "rows today" is C_ok
(companion) or the sweep's passing count (perl-tests).

| file | drops (exempt/reg/delib/indirect/gap) | rows today |
|---|---|---:|
| perl-tests/sprintf2.t | 0/1/0/0/0 | **1631** |
| t/re/pat_advanced.t | 0/4/0/0/0 | **950** |
| t/op/signatures.t | 1/0/0/0/0 | 920 |
| t/op/substr.t | 2/0/0/0/0 | 377 |
| perl-tests/substr.t | 2/0/0/0/0 | 375 |
| t/op/ref.t | 0/0/0/1/0 | 198 |
| perl-tests/ref.t | 0/0/0/1/0 | 192 |
| t/io/open.t | 0/0/0/0/1 | 102 |
| perl-tests/method.t | 0/0/0/1/0 | 97 |
| t/op/method.t | 0/0/0/1/0 | 96 |
| t/op/universal.t | 0/0/0/0/1 | 75 |
| t/comp/parser.t | 0/0/0/0/5 | 65 |
| t/op/gv.t | 0/0/0/0/3 | 61 |
| t/uni/gv.t | 0/0/0/0/2 | 50 |
| t/op/filetest.t | 0/0/0/0/1 | 25 |
| t/op/try.t | 1/0/0/0/0 | 23 |
| t/uni/method.t | 0/0/0/0/2 | 23 |
| t/op/lex.t | 0/0/0/0/1 | 13 |
| t/op/sub_lval.t | 33/0/0/0/2 | 12 |
| t/op/lexsub.t | 0/0/0/0/4 | 9 |
| t/uni/readline.t | 0/0/0/0/2 | 4 |
| t/op/utf8cache.t | 0/0/0/0/1 | 2 |
| t/comp/final_line_num.t, t/op/write.t, t/re/pat.t, t/re/reg_namedcapture.t, t/re/subst.t | — | 0 each |
| **total** | **39/7/1/4/31** | **5300** |

### The four options, priced

| option | what DIEs | files lost | **rows lost** |
|---|---|---:|---:|
| **A** flip everything | all 27 census files | 27 | **5300** |
| **B** exempt lvalue + indirect object; everything else DIEs | 19 files | 19 | **3022** |
| **C** B, plus exempt the *registered* absences and the deliberate row | 15 | **441** |
| **D** fix the 31 gaps first, then flip with A's rule | 0 | 0 | **0** |

Two numbers decide the shape of this:

* **Option B is not affordable**, and the reason is not the gaps — it is that
  four *registered* absences sit in files with 2581 passing rows between them
  (sprintf2.t 1631 for one hex-float block, pat_advanced.t 950 for four regex
  code blocks).  s419's arithmetic put the registered rows on the "flip-legal
  today" side; that is true per ROW and false per FILE, because the flip's
  unit is the file.
* **Option C costs 441 rows across 15 files** — six of them are nearly free
  (`op/lexsub.t` 9, `op/sub_lval.t` 12, `op/utf8cache.t` 2 and the three 0-row
  files re/pat.t, re/reg_namedcapture.t, re/subst.t), and three files carry 268
  of the 441 (io/open.t 102, op/universal.t 75, comp/parser.t 65 — three gaps
  that are one task between them, #415).

So the ordering the price implies: **the exempt/registered/deliberate families
need a shape that is NOT "the file dies"** (a perl-shaped refusal at the drop
SITE, or keeping them announced) — and once they have one, the residue is 441
rows over 15 files, i.e. the unblock list is ~5 tasks (#458, #460, #461, #463,
#415), not the ~30-file catastrophe s419 measured.

## 5. The module-mode increment (flip-gate §5) — measured, and it is NOT free

s419 parked module-mode DIE as the "first measured single-mechanism increment"
pending one number: how many board modules carry drops.  Measured s431 with
`PCL_DROP_ANNOUNCE=all ./pl2cl --module`:

| population | files scanned | files with drops | drops |
|---|---:|---:|---:|
| the 14-dist board's own `lib/**.pm` | 28 | **3** | **5** |
| `cpan-tests/modules` (`**/*.pm`) | 94 | **9** | **15** |
| shipped `lib/**.pm` | 22 | 0 | 0 |

**Board (3 modules, 5 drops):**

| module | drop | board rows today |
|---|---|---:|
| Text::Balanced 118 | `$escs .= substr($escs,-1) x (length($dels)-length($escs));` | **780 ok** / 300 not-ok |
| Text::Balanced 397 | `$closetagpos = pos($$textref)-length($1);` | (same dist) |
| Sub::Uplevel 49 | `require $m if delete $INC{$m};` | 28 ok / 128 not-ok |
| Mojo::DOM58::_Collection 46,47 | `return List::Util::first { … } @$self if …` | 0 ok |

Both Text::Balanced rows are **one new bug, #457**: `)-WORD` with no space —
`length($dels)-length($escs)` — is lexed by PPI as `Word(-length)`, a negative
bareword, so the statement drops.  It is the unrepaired third sibling of
`docs/ppi-upstream-bugs.md` §12 (`)*name`) and §15 (`)-1`), both of which PCL
already repairs with the same `_ends_term` predicate.  A scan of all four
in-repo populations (1139 files) with that predicate finds **zero** sites — the
shape exists only in real CPAN code, which is why no census ever saw it.

**cpan-tests (9 of 94 modules, 15 drops)** — all in `Test-Simple-1.302199`'s Test2
stack: `try { require $file }` ×4, `require … unless $INC{…}` ×4,
`local($\, $,) = (undef, '') if $\ || $,;` ×2, `print $_ @_ for @$self;` /
`printf` ×2, one `HAVE_PERLIO ? grep {…} PerlIO::get_layers($fh) : …`, one
that reports `Can't call method "content" on an undefined`.

**Eight of those 20 module drops minimise to three one-line bugs** (probed this
session; each is a statement perl runs and PCL silently does not):

| minimal reproducer | perl | PCL | task |
|---|---|---|---|
| `my $m = "strict.pm"; require $m if 1;` | ok | DROP | **#464** |
| `local($\, $,) = (undef, "") if 1;` | ok | DROP | **#464** |
| `local $_ = \*STDOUT; print $_ "x\n";` | `x` | DROP | **#466** |

The negatives are what make them small: `require $m;` alone, `if (1) { require
$m }`, `require strict if 1;`, `local($\, $,) = (undef, "");`, `local (@a) =
(1,2) if 1;`, `my $fh = \*STDOUT; print $fh "x\n" for (1);` are all right
today.  So #464 is a MODIFIER interacting with two statement classes (suspect
one cause), and #466 is `$_` in the filehandle slot (PPI hands it over as
`Token::Magic`, not `Symbol`).

The Test2 line that carries two of them, `local($\, $,) = (undef, '') if $\ ||
$,;`, also carries a third bug found while minimising: **`$\` and `$,` are
DEFINED in PCL and undef in perl** (`$/`, `$;`, `$!` agree), so the guard takes
the branch perl skips — **#465**, runtime-only and therefore sweep-owing.

`try { require $file }` (×4) and the two remaining singles did NOT reproduce
isolated; their triggers are contextual and unlocated.

**Verdict: the module-mode flip is the OPPOSITE of free.**  A module-mode DIE
aborts any program that loads Text::Balanced (780 board rows) or the Test2
stack (which is most of the cpan-tests population).  It stays the right
end-state — these are the last *fully* silent members of the #138 family — but
it is now a 20-drop unblock list of its own, not an increment to take first.

The measurement also exposes a hole in the instrument: **no census covers the
module populations at all** (`drop-census.pl` = perl-tests + perl t/ + lib/;
`corpus-diff.pl`'s SILENT-DROP counter = perl-tests only).  Task **#462**.

## 6. Side finding: five companion files had no snapshot row

`baselines/perl-suite-run.tsv` carried 523 rows for 528 files.  Absent — not
quarantined, not registered, simply missing, so a regression in them could
never read as a mover (the #176 family): `comp/line_debug.t`, `op/goto.t`,
`op/lex.t`, `op/require_errors.t`, `run/dtrace.t`.  Measured s431 and spliced
in with their first measurement (so none is a mover):

    comp/line_debug.t     P 25/0    C  1/24   DIFF
    op/goto.t             P 132/2   C  0/0    TRANSPILE-FAIL  (#314 refusal:
                                              "my-lexical 'count' spans a package boundary")
    op/lex.t              P 53/0    C 13/39   DIFF  unbound:%0   (1 drop = the census row)
    op/require_errors.t   P 73/0    C  3/70   DIFF
    run/dtrace.t          P 0/0     C  0/0    NOTAP  (perl itself emits no TAP)

## 7. What P1 hands to the Fable design (§2.1 of the plan)

1. **The exempt families cannot be "kept as drops" without keeping the drop
   MECHANISM** — and the same is true of the registered ones, which are worth
   2581 rows.  The question is therefore not "what do we do with the 39 + 4",
   it is: **is there one shape that serves exempt + registered + deliberate?**
   The obvious candidate is a perl-shaped, trappable REFUSAL emitted at the
   drop site that dies *when the statement runs* rather than at transpile —
   which keeps every row before it, kills the silence, and makes a `$@`-shaped
   test row possible.  §4's option C is the same 441-row bill under either
   spelling; what differs is whether sprintf2.t's 1631 rows survive.
2. **The order the price implies**: give the exempt/registered families their
   shape first (it is worth 4859 of the 5300 rows), then close five tasks
   (#458, #460, #461, #463, #415) for the remaining 441, then flip.
3. **Module mode is not the free first increment** (§5) — it is a 20-drop
   list, and #457 is its first and most valuable row (a top-20 CPAN dist,
   silently wrong today, one repair in an existing family).
4. The census instrument needs the module populations before the flip's bar
   can be evaluated honestly at all (#462).
