# Review requests — session 423 (Opus 5, 2026-08-22)

One task: **#418 (widened s421)**, item O1.2 of `docs/plan-post-s420.md` —
*pipe-quote ANY emitted symbol whose name carries a non-ASCII character*.
One commit.

This session **continues** a previous s423 agent that died leaving an
uncommitted diff (10 files, no commit).  §1 is the review of that inherited
work — what it got right and the three seams it missed, each found by
probing rather than by reading.  §2 is the rule as it now stands.  §3 the
probe table.  §4 the measurements, including a new population-wide instrument
that counts this bug directly.  §5 what was found and FILED, not fixed.
§6 the asks.

## §1  The inherited diff, reviewed

The previous agent's diff was sound and is kept essentially unchanged.  Its
central judgement — the one this review most wanted to check — is right:

* **The rule lives in ONE helper** (`Pl::CLForm::cl_sym` / `cl_pkg`), and the
  four `$pkg =~ /::/ ? "|$pkg|" : $pkg` copies are folded into it (rule 11).
  A grep for that shape over `Pl/**` now returns nothing.
* **cl_sym is the IDENTITY on ASCII.**  That is not a convenience, it is the
  correctness condition: under `:invert` a bare `$x` reads as the symbol `$X`
  while `|$x|` reads as `$x`, so quoting an ASCII name would silently RENAME
  it.  The inverse guard is therefore byte-identical emission over every ASCII
  file, and that is what §4.2 measures.
* **The runtime half is one guard** — `%pcl-invert-case` returns a name
  carrying a non-ASCII character unchanged, which is what makes "pipe-quoted"
  mean "verbatim" on both sides of the seam.  Because essentially every
  runtime intern site already goes through that one function (symbolic refs,
  `->can`, globs, stash keys, `caller`, bareword filehandles, loop tags, sub
  names), the emitter and the runtime agree everywhere at once.  The two
  `(intern (string-upcase clos-class-name) …)` method dispatchers were moved
  onto it in the same diff — the same transform for the all-lowercase ASCII
  name `perl-pkg-to-clos-class` builds, and the only matching one for a
  non-ASCII package.
* `p-stash` keys stay raw strings, as the task ruled: they are perl names, not
  CL tokens.

### The three seams it missed

Each was found by probing a seam vs perl, not by reading the diff, and each
is the same failure: **one side of a seam spelled the name and the other did
not, so the two named different symbols and the program ran on the wrong one.**

**(a) `pl2cl`'s `build_eval_preamble` held its own copy of the designator
rule.**  It was the fifth copy — outside `Pl/`, which is why the grep the
task prescribed did not reach it.  A string eval compiled inside
`package ＦＯＯ` therefore opened `(in-package :ＦＯＯ)` (the reader's
NFKC-folded, `:invert`-ed `foo`) while the FILE that called it used
`:|ＦＯＯ|`, so the eval's free `$z` resolved in a *different package* and
read back undef:

```perl
package ＦＯＯ; our $z = 1;
sub f { my $v = 7; return eval 'q(x) . $v . $z' }
# perl x71   inherited diff: x7   fixed: x71
```

This is the one place where the inherited diff was a REGRESSION against
`a2ac578`: before it, both halves were wrong the same way and cancelled.
`build_eval_preamble` now calls `Pl::CLForm::cl_pkg` and says in its comment
that it is the eval-mode twin of `Parser::_cl_pkg_designator`.

**(b) `open ＦＨ` / `close ＦＨ` emitted the bareword BARE** while
`print ＦＨ …` and `<ＦＨ>` (which go through `gen_filehandle_form` /
`gen_readline_form`, both fixed by the inherited diff) quoted it.  The program
wrote to one handle, read from another, and printed **nothing** — a silent
wrong, not a crash.

The fix is at the leaf emitter, keyed on the `is_filehandle` registry: that
registry is the mechanism which already tells the parser which barewords are
handles, so keying on it covers every builtin that takes one — `open`,
`close`, `binmode`, `eof`, … — instead of a branch per builtin (rule 11).

It is deliberately **not** a blanket `cl_sym` on every `PPI::Token::Word`.
That was the first cut, and the breaking-case probe killed it: the same leaf
also carries names that are fed BACK to `cl_name` as perl names (a qualified
call's Word), and `|ＦＯＯ::f|` there splits on `::` into the package
`|ＦＯＯ` — an unbalanced token that made the whole file fail to READ.  The
episode also produced one hardening: `cl_sym`/`cl_pkg` now treat *any* `|` in
the input as "already a CL spelling" (no perl identifier can contain one), so
a double pass is a no-op instead of a corruption.

**(c) `_swap_elem_sigil` did not know the QUALIFIED quoted spelling.**
`$Pkg::A[0]` names the array `@A`, and the swap that turns `$` into `@` reads
the EMITTED token.  The helper knew `Foo::$x` and the whole-token `|$Ｘ|`, but
not `Foo::|$Ｘ|`, so `"$main::Ｌ[0]"` handed `p-aref` the SCALAR and died in
`gethash`.  One pattern with an optional `|` now covers all three spellings —
the two-alternative form was the seam.

## §2  The rule, as it now stands

> A perl NAME that becomes a CL TOKEN is spelled by `Pl::CLForm::cl_sym`
> (`cl_pkg` for the package half).  It is the identity on ASCII and
> pipe-quotes a name carrying a non-ASCII character.  A perl name that stays a
> STRING (a `p-stash` key, a method name, a hash key, `p-register-pkg-name`'s
> second argument) is untouched.  The runtime's `%pcl-invert-case` is the
> identity on exactly the same names, which is what makes the two sides agree.

Callers now routed through it: package designators (`p-defpackage`,
`in-package`, the eval preamble, `@ISA` parents, `use base`), variables of
every sigil (bare, `our`-qualified, cross-package, state cells, `use vars`),
sub names (`p-sub`, `p-declare-sub`, qualified calls), CLOS class names, loop
labels and `goto` tags, bareword filehandles, `select`/`readline` handles, and
the string-eval capture alist's VALUES (its KEYS stay perl names).

Two consumers read the spelling back rather than writing it:
`Pl::GlobalPartition::_split_name` (so a quoted global is still "word-shaped"
and gets a `p-defcell`, not the exception `defvar` — the #313 collision) and
`Parser2::_fresh_container` (the SIGIL is behind the quote).  Both use
`cl_unquote`, and `Parser::_wrap_runtime_labels` matches both label spellings.

## §3  The probe table — perl 5.40.3 vs `a2ac578` (before) vs this branch

28 probe programs.  `SAME` = byte-identical to perl.

| probe | shape | before | after |
|---|---|---|---|
| p01 | the task's own reproducer: stash exists/delete + `%Ｘ`/`%X` + `$Ｘ`/`$X` | DIFF (`0 n 2266`) | **SAME** (`1 y 1256`) |
| p02 | `${"ＦＯＯ::z"}`, `@{"ＦＯＯ::w"}`, `${"ＦＯＯ::h"}{k}`, symbolic WRITE | DIFF | **SAME** |
| p03 | `*{"ＦＯＯ::bar"}{CODE}`, `defined &{…}`, `&{…}()` | DIFF (crash) | **SAME** |
| p04 | `"ＦＯＯ"->can`, `ＦＯＯ->bar`, `ＦＯＯ->$m`, `ref($o)`, `$cls->bar` | DIFF (crash) | **SAME** |
| p05 | `local $Ｘ` beside `local`-free `$X` | DIFF (`99 22`) | **SAME** (`92 12`) |
| p06 | `my`/`our` of all three sigils, fullwidth vs ASCII twins | DIFF (`226688`) | **SAME** (`125678`) |
| p07 | `sub Ｆ` beside `sub F`; `&{"Ｆ"}`, `defined &{…}`, `\&Ｆ` | DIFF | **SAME** |
| p08 | MIXED names `$aＸ` / `$aX` / `$ax` | DIFF (`445 77`) | **SAME** (`345 67`) |
| p09 | fullwidth LABEL beside ASCII `LOOP`; `next`, `last` | DIFF | **SAME** |
| p10 | **all-ASCII inverse** (every construct, no wide chars) | DIFF\* | DIFF\* |
| p11 | `keys %ＦＯＯ::` | DIFF | DIFF — **#430** |
| p12 | `*{"main::ｑｕｕｘ"} = \&ＦＯＯ::bar`, `\*ＦＯＯ::bar` | DIFF (crash) | **SAME** |
| p13 | `@ISA` + `SUPER::` across fullwidth packages | DIFF (crash) | **SAME** |
| p14 | string eval reading `our $z` inside a fullwidth package | SAME | **SAME** |
| p15 | fullwidth array/hash slices, kv-slice, `$$ｒ[1]` | SAME | SAME |
| q01 | `use constant Ｐ` beside `use constant P` | DIFF (`44`) | **SAME** (`34`) |
| q02 | interpolation `"$Ｘ$X @ａ $ｈ{k} $ａ[1]"` | DIFF | **SAME** |
| q03 | `__PACKAGE__` and `caller(0)` inside a fullwidth package | SAME | SAME |
| q04 | bareword filehandle `ＦＨ`: open/print/readline/close | DIFF (crash) | **SAME** |
| q04a | the ASCII twin of q04 | SAME | SAME |
| q05 | fullwidth `sort`/`map`/`grep` block vars and an anon sub | SAME | SAME |
| q06 | `*{"ＦＯＯ::two"} = sub`, `->can`, `our @Ｌ`/`%Ｍ`, qualified interpolation | DIFF (crash) | **SAME** |
| q06a | the ASCII twin of q06 | SAME | SAME |
| q07 | AUTOLOAD: method form and `ＦＯＯ::other()` | DIFF | DIFF — **#431** |
| q07a | the ASCII twin of q07 | DIFF | DIFF — **#431**, identical text |
| q08 | `bless`/`ref`/`isa`/`UNIVERSAL::isa`/error text, fullwidth class | DIFF (crash) | **SAME** |
| q09 | fullwidth HASH KEYS (`$h{ｋ}` beside `$h{k}`) | SAME | SAME |
| q10 | `"$main::Ｌ[0]|$main::Ｍ{a}|$Ｌ[1]"` | SAME\*\* | **SAME** |
| x1/x2 | ASCII vs fullwidth package + string eval, side by side | SAME/SAME | SAME/SAME |

\* p10 is the **runpcl blank-line artifact**, identical before and after and
ASCII-only — filed as **#432**, and it is why a byte-compare through
`./runpcl` needs care.
\*\* q10 was SAME on `a2ac578` and BROKE under the inherited diff; the §1(c)
fix restores it.

The three residual DIFFs are p10 (#432), p11 (#430) and q07 (#431).  **All
three are PRE-EXISTING and reproduce identically with ASCII names** (p10 and
q07a are ASCII programs; p11's ASCII twin is #430's own reproducer).  That is
the evidence that #418 is closed rather than merely narrowed: after this
change a non-ASCII name behaves *exactly* like its ASCII twin, wherever that
twin is right and wherever it is wrong.

## §4  Measurements

### §4.1  A new instrument: "a non-ASCII character in a BARE token"

`#418` has a direct, countable signature in the emitted CL — a non-ASCII
character appearing in a token that is not inside `|…|`, a string literal, a
`;`-comment or a `#\c` char literal.  Every such occurrence is a symbol the
reader will NFKC-fold and case-invert, i.e. a symbol no runtime string can
spell.  A ~30-line scanner counts them (kept in the session scratchpad; it is
a measurement, not a gate — the gate is `Pl/t/utf8-source-01.t` plus the
emission A/B below).

| tree | population | files with a bare non-ASCII token | occurrences |
|---|---|---|---|
| `a2ac578` | perl `t/uni/*.t` + `t/mro/*.t` | **49** | **1962** |
| this branch | the same, plus `t/op/*.t` + `t/re/*.t` (404 emitted files) | **0** | **0** |

### §4.2  The inverse guard: `tools/emission-ab.pl --ref a2ac578`

Population: `perl-tests/*.t` + `lib/**/*.pm` + every file of perl's own `t/`
— **738 files**.

```
emission-ab: files=738 SAME=686 DIFF=52 RCDIFF=0 (both-empty pairs: 31)
```

Every ASCII file is byte-identical, and the 52 differing files are **exactly**
the 52 whose BASE emission carried a bare non-ASCII token (checked
mechanically: 52 of 52 with, 0 without; and side B has 0 such tokens in all
738).  49 are `t/uni/` + `t/mro/`; the other three are `t/op/lexsub.t`
(`|pl-φου__lexsub__38|`), `t/op/substr.t` (`:|ザ|`) and `t/op/utf8cache.t`
(`:|_abcdefgα|`, `:|αabcdefg_|`).  `RCDIFF=0`: no file's exit status moved.

### §4.3  `tools/corpus-diff.pl`

```
silent drops: 7, unchanged
emission identical to HEAD across 111 files
```

### §4.4  The gate

`PCLXS_DIR=$HOME/pclxs tools/prove-core`, cold (generation `v2-165` is new,
so no module cache existed):

```
Files=156, Tests=5653, 359 wallclock secs
```

Failures are **exactly the 13 pclxs xs rows** (`xs-01.t` 5 of 6, `xs-02.t` 4,
`xs-03.t` 4) — pclxs is under separate work, ignored per USER s394/s395.
Arithmetic against main: main measures 156/**5639** with the same 14 xs rows
produced; `Pl/t/utf8-source-01.t` goes 7 → 21 rows on this branch (+9 from the
inherited diff, +5 here), and 5639 + 14 = **5653**.

`Pl/t/utf8-source-01.t` is 21 rows / 24 s wall, five of them added here: the
eval-preamble package, the bareword filehandle, the qualified element sigil
swap, and two ASCII **inverse** rows (a bareword filehandle stays bare at
`open` and is never pipe-quoted).

### §4.5  Full perl-tests sweep

Mandatory: `cl/pcl-runtime.lisp` changed.  `perl sweep-perl-tests.pl --jobs 3`.

```
TOTAL dropped statements: census 7, current 7 (+0)  [1 file(s) not measured]
TOTAL passing: baseline 18365, current 18365 (+0)
summary: 0 new, 0 fixed, 7 unstable (crash-file noise), 10 unverified (did not run)
         (baseline 696 fails, current 693 fails)
GATE: clean
```

**+0 in every bucket**, and no baseline row was edited — `docs/fail-baseline.tsv`
and `docs/pass-baseline.tsv` are untouched by this session.  The 7 UNSTABLE and
10 unverified rows are the standing crash-file noise (new/absent failures ABOVE
the abort point of files already PARTIAL: method.t, postfixderef.t, ref.t,
yadayada.t, eval.t, magic.t, tr.t).  Spot checks against the blessed numbers:
`ref.t` 191, `pack.t` 5636/89, `substr.t` 375, `closure.t` and `tr.t` 239 — all
at their baseline values.  min MemAvailable 4.6 GB.

This is the expected result: the sweep's population is ASCII, and §4.2 proves
the emission for it is byte-identical.  The sweep is run because
`cl/pcl-runtime.lisp` changed — `%pcl-invert-case` is on nearly every intern
path in the runtime, so a mistake in its new guard would land here and nowhere
else.


### §4.6  Companion legs `uni/` + `mro/`

The s421 ruling makes an `op/`-style companion leg mandatory for a `cl/`
change; this task's population is `uni/` + `mro/` (103 files).  Run on this
tree at `--jobs 2`, then **every mover re-run ALONE (#366) on this tree AND on
a `463a8f8` base worktree** — the two runs agree row for row, and
`docs/perl-suite-run.tsv` carries the whole table with its causes in a header
block.

**16 movers.  Fifteen are gains; one is a loss of three ACCIDENTAL passes.**

| file | base 463a8f8 | branch | what |
|---|---|---|---|
| `uni/method.t` | 0/0 | **23/9** | died at the first fullwidth method call; 32 rows now run |
| `uni/caller.t` | 1/3 | **7/12** | same; 4 rows → 19 |
| `uni/package.t` | 1/0 | **8/10** | same; 1 row → 18 |
| `uni/select.t` | 0/0 | **1/4** | same; 0 rows → 5 |
| `uni/readline.t` | 3/1 | **4/1** | the bareword-filehandle fix |
| `mro/overload_c3_utf8.t` | 0/0 | **7/0** | **now fully passing** |
| `mro/recursion_c3_utf8.t` | 1/7 | **8/0** | **now fully passing** |
| `mro/recursion_dfs_utf8.t` | 1/7 | **8/0** | **now fully passing** |
| `mro/method_caching_utf8.t` | 10/0 | **21/7** | 11 registered rows stopped diverging |
| `mro/basic_utf8.t` | 10/10 | **13/12** | 3 stopped diverging |
| `mro/isa_c3_utf8.t` | 19/15 | **21/13** | 2 stopped diverging |
| `mro/isa_dfs_utf8.t` | 19/15 | **21/13** | 2 stopped diverging |
| `mro/next_ineval_utf8.t` | 0/0 | **0/1** | the file produces a row at all |
| `mro/package_aliases_utf8.t` | 72/34 | **73/40** | +1 ok, +6 rows run |
| `uni/gv.t` | 53/28 | **50/31** | **the one loss — see below** |
| `uni/parser.t` | 26/32 | 26/32 | **PRE-EXISTING**: the base measures the same; the SNAPSHOT (23/35) was stale |

Registry hygiene, in the same change: the three now-fully-passing files were
REMOVED from `docs/perl-suite-expected.tsv` (their reason was literally
"utf8 package-name variant — non-utf8 twin passes", i.e. the divergence WAS
the name spelling), and `docs/perl-suite-expected-rows.tsv` was re-blessed for
the four still-XDIFF files: **18 rows removed, 0 added** — a pure narrowing of
what each registered reason excuses.  All seven re-verified afterwards: 3 OK,
4 XDIFF, 0 STALE.

**`uni/gv.t` 53/28 → 50/31 — three ACCIDENTAL passes, not a regression.**  Row
count is unchanged (81); PCL rows 61-63 flip `ok` → `not ok` with
`got: undef, expected: '1'`.  They are perl's
`local *Ｊ = *Ｊ; *Ｊ = sub{}` idiom (t/uni/gv.t 224-228), and PCL's
`local *NAME` loses the glob's scalar/array/hash slots.  That is
**PRE-EXISTING and identical for ASCII names** — probed on the base worktree:

```perl
our $J = 1; local *J = *J; print defined $J ? $J : "undef";
# perl 1     base 463a8f8 undef     this branch undef
```

Before #418 the fullwidth glob `*Ｊ` and the fullwidth cells `$Ｊ`/`@Ｊ`/`%Ｊ`
were DIFFERENT CL symbols, so the broken glob-local could not reach them and
the rows passed by accident; now that both sides spell the name the same way,
the file behaves exactly like its ASCII twin.  Same family as the s418
bless.t / split.t un-drops.  Filed as **#433** with both reproducers.


### §4.7  Artifacts

All three checked-in transpiled artifacts were **regenerated on the rebased
tree** (`tools/rebuild-pack`; `./pl2cl --extension lib/mro.pm` and
`… lib/warnings.pm` + `tools/tag-license` + `check-parens`) and each came back
**byte-identical** to the file already in the tree at `gen=v2-165` — which is
the expected result, since their sources are ASCII and ASCII emission is
byte-identical.  `Pl/t/artifact-staleness-01.t` passes.

## §5  Found and FILED, not fixed

All three reproduce identically with ASCII names, so none is caused by #418.

* **#430** (filed by the previous agent, kept) — `keys %Pkg::` lists only
  SUBS; a package's scalar/array/hash globals never appear as stash keys, and
  `exists`/`delete $Pkg::{name}` answer on that same snapshot.  The fullwidth
  twin behaves identically, which is the point.
* **#431** (new) — AUTOLOAD is not consulted for a plain qualified sub call.
  `Foo->anything` works (the method path falls back to AUTOLOAD);
  `Foo::other()` is emitted as a direct CL call and dies undefined-function.
  Carries the design question (resolver-at-call vs AUTOLOAD trampoline) and
  says to measure the corpus spelling first.
* **#432** (new) — `./runpcl` can emit a spurious blank line, falsifying a
  byte-compare against perl.  The generated CL is correct; the cause is
  `2>&1` interleaving of the runtime's one compile-time style-warning block
  with a leading-only blank-line strip.  A **measurement trap**: it made one
  probe in this very table read DIFF.  Fix candidate (a): keep stderr separate.
* **#433** (new) — `local *NAME = *NAME` loses the glob's scalar/array/hash
  slots.  perl keeps them (that idiom is how you localize a glob without losing
  what is in it); PCL reads them back undef.  ASCII-identical, probed on the
  base worktree; it is the cause of the ONE companion loss (§4.6).

Task **#418 is closed** with the measurement in its notes.

## §6  Asks

**Ask 1 — the generation number.**  This branch resolves the rebase conflict
to **`v2-165`**, as the session brief instructed, which is *lower* than main's
current `v2-166`.  v2-165 has never been used by any other tree, so it is a
unique cache key and nothing can collide; but if Fable renumbers once at merge
(the s422–s424 plan says `v2-167`), the three artifact stamps must move with
it — they are byte-identical to a fresh regeneration, so a stamp-only edit is
exactly right, and `Pl/t/artifact-staleness-01.t` is the arbiter.

**Ask 2 — is the `is_filehandle` key the right gate for the bareword leaf?**
`gen_leaf`'s Word branch is the one place a bareword becomes a CL token, but
it is a MIXED site: it also carries text that has already been rewritten to a
qualified CL symbol and is fed back to `cl_name` as a perl name.  Gating on
`environment->is_filehandle($content)` is precise and ASCII-neutral, and it
reuses the mechanism that already answers "is this bareword a handle".  The
alternative reading is that the mixed site is itself the bug (a PPI token
whose content has been overwritten with CL text) and that the real fix is to
stop overwriting it — which is E5/`#243`-shaped work, not this session's.
Confirm the narrow gate stands, or schedule the wider clean-up.

**Ask 3 — nothing else.**  The three residual probe DIFFs are filed, and no
divergence remains that needs a `docs/not-supported.md` entry: after this
change every non-ASCII name behaves exactly as its ASCII twin does, so
`not-supported.md` gains nothing (the *absences* #430/#431 name are ASCII
bugs with their own tasks).

## §7  What was NOT done

* The wider clean-up behind Ask 2 (the PPI-Word-content overwrite).
* #430 / #431 / #432 / #433 — filed with reproducers, not fixed (the standing rule:
  a found silent-wrong is FILED and jumps the queue only if it regresses a
  baseline or blocks a phase; none of the three does).
* No `docs/not-supported.md` entry, for the reason in Ask 3.
