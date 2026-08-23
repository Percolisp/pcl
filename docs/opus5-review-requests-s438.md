# Opus 5 review requests — s438 (2026-08-23): the two census instruments

The session the s437 queue opens with (`docs/fable-answers-s437.md` §4):
**#473** (the cpan `.t` census population), **#472** (the `PCL_DROP_LOG` side
channel) and **s434 ask 1** (the five never-refreshed snapshot rows).  No
product change — three instruments and what they found.

Record: DECIDED §s438, `docs/session-log.md` Session 438.

## §1  What shipped

1. **`tools/drop-census.pl` — a sixth population, `cpan-t`** (task #473):
   every `.t` at any depth under a `t/` directory below `cpan-tests/modules`
   (289 files), transpiled as a PROGRAM.  **42 files / 83 drops**, blessed into
   `docs/parse-error-drop-census-s399.tsv` with causes (39/102 → **81 files /
   185 drops**).  The tool also strips the repo root out of message TEXT.
2. **`Pl::Parser::_announce_dropped_statement` — the `PCL_DROP_LOG` arm**
   (task #472), plus `sweep-perl-tests.pl`'s `child-drops` column and its
   two-number report.  No emission change; no generation bump.
3. **`tools/run-perl-suite.pl` — need-harness files join the scan**
   (s434 ask 1): 523 → **528 files**, with `%NEED_HARNESS_NOT_RUN` (empty by
   measurement) feeding the existing NOT-RUN path for a future one.

## §2  The reconciliation this session owes: 43/92 vs 42/83

#473's acceptance said "reproduces 43 files / 92 drops".  It does not, and the
difference is population, not progress:

| | files | drops |
|---|---|---|
| the s436 A/B (every `.t` under `cpan-tests/modules`) | 43 | 92 |
| the `cpan-t` population (`.t` under a `t/` dir) | **42** | **83** |
| `examples/tools.t` (an EXAMPLE, not the suite) | 1 | 9 |

Measured on an `f702da3` worktree: **the same 42 files / 83 drops there**, so
nothing moved since s436.  `xt/` and `examples/` are named in the census
header with that count, beside `t/japh/` (1 file / 2 drops, ruled permanently
outside).

Two decisions inside the tool that were MEASURED rather than assumed:

* the dist `.t` files are transpiled **without** the dist's own `lib/` and
  `t/lib` on @INC (which is what `tools/run-dist-t.pl` adds) — rows are
  identical either way, so the census stays uniform with its other
  program-mode populations;
* one compiler message quotes the file it was raised in, so a row depended on
  how ROOT was spelled on the command line (`.` vs an absolute path).  The tool
  now normalises it out.

Two of the 289 files REFUSE to transpile (`Parser2 TODO:` refusals) and so
contribute no rows; both are named in the census header.

## §3  What the instruments found — six filings, four of them silent-wrongs

**#478 — 79 of the 83 new census drops are ONE mechanism, and it is a module
NAME LIST inside the compiler.**  `_extract_module_prototypes` skips every
`Test2::`/`Test::` module by name, so Test2's `(&)`-prototyped
`intercept`/`exception`/`capture`/`warnings`/`context_do`/`no_context` (and
Text::CSV's `try`/`async`) are not block-form calls.  The discriminating probe
is two modules identical but for the package name:

```
sub blk (&) { $_[0]->() }   in Test2::Fake  ->  blk { 42; }  DROPS
sub blk (&) { $_[0]->() }   in My::Blk2     ->  blk { 42; }  runs
```

and the spelling that does NOT drop is worse: `blk { 42 }` (no semicolon)
emits `(pl-blk 42)` — the block's VALUE where perl passes a code ref.  The
task records why the skip exists (compile time, and Test::More's shim
prototypes) and that the fix must key on a mechanism, not more names
(CLAUDE.md 9a).

**#479 — the first thing `PCL_DROP_LOG` printed was a drop in OUR OWN
HARNESS**, `perl-tests/t/test.pl:179-180`, reached by **98 of 98** sweep files:

```perl
my $so = do { local $/; open(my $f, '<', $out) ? <$f> // '' : '' };
```

Minimised, the compiler gap is `COND ? <$fh> // "" : ""` (a readline in a
ternary branch followed by `//`; `? <$f> : ""`, `readline($f) // ""` and
`<$f> // ""` all lower).  **And the harness line is wrong in real perl too**:
`my $f` declared in a ternary's condition is not in scope in its branches, so
`runperl_and_capture` returns `('','')` for a file that plainly has content —
probed against perl 5.40.  Repairing it is a HARNESS change (both populations,
baselines row by row), so it is filed, not done here.

**#483 — a real gap in a real module, in the one population nothing could
count**: core `Devel/Peek.pm:59`, `$num |= (1<<index($D_flags,$_)) for split
//, $on;`.  PPI lexes `<<index` as a **HEREDOC**, so every unspaced left-shift
by a call drops (`1 << index(...)` with spaces lowers).  Same `_ends_term`
discriminator #457 already built for `)-name`.

**#480 / #481 / #482** are the four-drop residue of the new population:
`$_.2` (PPI lexes `.2` as a float — no concat operator in the stream at all),
`is $csv->module => 'M'` (the fat comma autoquotes a METHOD NAME), and
`$obj->state` (a keyword-named method dies inside the compiler).  #481 and
#482 are the same fact — "a Word after `->` is a method name" — at two more
sites; the s407 review fix was the third.  The tasks say to look for one
upstream place before patching either.

## §4  The seventh population, measured (task #472)

Full sweep, 108 files: **241 drops in 98 files — TEN distinct sites.**

| site | files | what |
|---|---|---|
| `perl-tests/t/test.pl:179`, `:180` | 98 each | #479, our harness |
| six child programs | 1 each | bop.t + split.t (the two known), blocks.t ×2 `BEGIN(){…}`, lex.t `sub ub(){0} ub ub` + `flock _$`, magic.t bare `]` — four of the six are programs perl itself rejects |
| `Devel/Peek.pm:59` | 1 | #483, a run-time module load |

The report prints **both** numbers on purpose: the per-file count says what
that file's run lost and double-counts the harness (closure.t alone: 38 of the
241, all `test.pl`, because it loads it nineteen times under
`*pcl-skip-cache*`); the site list is the census view.  Ten of the 108 files
report 0.  No verdict moved.

## §5  The one snapshot row this session edited, and its attribution

`op/require_errors.t  C_notok 70 → 68` (C_ok unchanged at 3).  **PRE-EXISTING
and bisected, not inferred**: 3/70 at `4356e77` and at `9138404` (s434), 3/68
at `f702da3` (s435) and at `d0b52e9` (HEAD) — **the flip is the cause**.  The
two rows are TAP 55 "should fail to load" and 56 "check the second attempt also
fails": both were `not ok` before and are `(missing)` now, because a drop in a
module the file requires dies when reached and takes the rest of its top-level
form with it.  Nothing that PASSED was lost.  This row is the first place the
flip's price in that file could be seen — which is the argument for closing the
never-refreshed hole.

`io/pvbm.t` read 20/8 in the parallel AND the #366 serial pass and **23/5 alone
— the FIFTH time**.  Not edited.

## §6  The bar, run

| leg | result |
|---|---|
| gate (`tools/prove-core`) | **163 files / 5741 rows**; only failures are the 13 pclxs xs rows (xs-01 ×5, xs-02 ×4, xs-03 ×4 — `~/pclxs` is abi 8, the pin says 6; user-deferred).  The +2 over s437's `5739` are **s437's own review-fix rows** (`decl-ordering-02.t` 19 → 21) — its number was taken before its own commit |
| `tools/corpus-diff.pl` | **emission identical to HEAD across 111 files**, silent drops 5 unchanged → no generation bump |
| full sweep (`--jobs 8`) | **TOTAL 18312, current 18312 (+0)**; 0 new / 0 fixed; drops census 5 = current 5; **GATE clean** (7 unstable / 10 unverified = the usual PARTIAL-file noise, identical to s437) |
| companion `--all --quick --jobs 4` | **528 files** (was 523); `SNAPSHOT: 0 … have NO row` and `SNAPSHOT: 0 row(s) for files this --all scan does not run` — the hole is closed from both sides; DROPS vs census: no `+`/`-` line; four #366 movers, all resolved above |
| census tool vs blessed file | 78 files / 180 drops without `--board`, + the 3 board files / 5 drops it did not re-measure = **81 / 185**, the blessed TOTAL line |
| the 36 non-board pre-existing census rows | **byte-identical** |

## §7  Asks

**Ask 1 — the `child-drops` column is in `_status.tsv` but NOT in the pass
baseline, and not gated.**  `sweep-diff.pl` reads the first six columns and
ignores it; `save-status` writes five, so `docs/pass-baseline.tsv` is
unaffected.  The ruling was "measure first, gate after one blessed run".  The
measurement is §4.  Is the blessed form (a) the census header paragraph it is
in now, (b) its own section of rows in the census file, or (c) a separate
`docs/child-drop-census-sNNN.tsv` with the runner comparing against it?  Ten
sites is small enough that (a) is honest today, and (c) is a third baseline to
keep in step.

**Ask 2 — `tools/run-perl-suite.pl` does NOT set `PCL_DROP_LOG`.**  The ruling
named the sweep, and the companion population drives fresh_perl children
through the same shadow `test.pl`, so it has the same seventh population and
almost certainly the same two harness sites ×N files.  Worth doing in the same
shape (measure first, no gate), or is one population's census enough for now?

**Ask 3 — #478's fix touches a skip that exists for compile time.**  Removing
`Test2::`/`Test::` from `_extract_module_prototypes` would let 79 census drops
lower, but the comment calls the Test2 stack "heavy" and the skip predates the
memoised walk.  Should the next session MEASURE that cost (corpus-diff prints
the time) and let the measurement decide, or is the name list defensible and
the census rows simply registered against it?

**Ask 4 — #479's harness half.**  `runperl_and_capture` is dead-wrong in perl
as well as dropped in PCL, and it is OUR line, not perl's.  Fixing it is a
harness change with the full both-population bar.  Ride it with the compiler
fix (one commit, one bar), or fix the harness alone first so the compiler fix
can be measured against a harness that works?

---

# Part 2 — Q4 (s438b + s438c): #453 and #365

The queue item after the instruments.  Both changes are in PExpr's
operand/term region, and both turned out to be guarded by ROWS rather than by a
corpus: the shapes occur in no population we measure.

## §8  #453 — the two operand sites become one

`Pl::PExpr::handle_subcalls` had two operand sites and only the named-unary one
(Config's BUILTIN table) ran `_extend_high_prec`.  A user `($)`/`(;$)`/`(*)`/`(_)`
sub took the other, which stops at the first term:

| | was | now / perl |
|---|---|---|
| `sub f ($)` … `f "a" . "b"` | `f(a)b` | `f(ab)` |
| `sub f ($)` … `f $x + 1` | `1` | `f(6)` |
| `sub g (*)` … `print "R=", g + 1, "\n"` | `g(1,\n)` | `g(1)` |

`is_named_unary` now answers for a declared sub whose `_proto_parse_spec` is 1
or [0,1] — exactly perl's named-unary set.  ONE predicate fixes both halves,
because it is also the strictly-single site's own guard.  `known_no_of_params`
is deliberately NOT a second source: its 1 covers `shift`/`close`/`fileno`,
which are not named unaries and whose bareword-filehandle branch is why that
site still exists.

## §9  #365 — the task pointed at the wrong place, and that is the finding

#365 said the prototypes are known to the environment and the operator-loop
term reading does not ask.  **Measured: it IS asked** — `_bareword_callable_here`
runs 23 times for `pi` in the reported program — **and answers `no`, because
`has_prototype('pi')` is FALSE.**  The prototype never crossed the `use`.

`_merge_module_prototypes` imported a prototype only for a block arg, a
parameter SLOT (`$`/`\X`/`@`/`%`), or a name `export_names` listed.  A `()`
prototype has no slots, so it rode entirely on that scan — and the scan reads
literal `qw()` out of `@EXPORT`/`@EXPORT_OK`, which real modules build from
variables:

```perl
my @trig = qw( pi tan … );                    # Math::Complex, where pi lives
our @EXPORT = (qw( i Re Im … atan2 ), @trig); # Math::Trig re-exports from here
```

Two fixtures isolated it: a local module with a literal export list is right
today, and so is a two-level re-export chain.  The fix keys on the PROTOTYPE —
an empty one is a parse fact, so it crosses a `use` like a block prototype —
rather than on interpreting the module's own code.

Two things worth keeping: **`is_proto` does not identify the `()` shape**
(`sub pi ()` arrives as is_proto 0; a first attempt keyed on it and changed
nothing), and the record test lived in `PExpr::_is_zero_arg_func` and *nowhere*
in the merge, which is how they disagreed — it is now one predicate,
`Pl::Environment::proto_is_zero_arg`, asked by both (rule 11).

## §10  The bar for Q4

| leg | #453 | #365 |
|---|---|---|
| `corpus-diff` (111) | identical, silent drops 5 unchanged | identical, same |
| emission A/B vs the previous commit, lib + cpan-tests + perl t/ | **951 SAME / 0 DIFF / 0 RCDIFF** | **951 SAME / 0 DIFF / 0 RCDIFF** |
| probes vs live perl | the s429 matrix, 3 files, all identical | the #365 table, 7 rows, all identical |
| guard rows | `Pl/t/user-unary-01.t` 12 (5 negatives) | `Pl/t/imported-term-01.t` 7 (3 inverses/negative) |
| inverse guard on a `fe46c7b` worktree | rows 1-7 FAIL there | rows 1-4 FAIL there |
| gate | 164/5753 | **165/5760** |

Batch legs: full sweep **TOTAL 18312 (+0), 0 new / 0 fixed, drops 5 = census,
GATE clean**; companion `--all --quick --jobs 4` **528 files, ZERO real
movers** (io/pvbm.t 20/8 in parallel AND serial, 23/5 alone — the SIXTH time;
op/utf8cache.t contention; uni/variables.t the known flood), both SNAPSHOT
holes zero, DROPS with no `+`/`-` line.  RCDIFF 0 over 951 files twice is also
the die-scan.

## §11  Asks (Q4)

**Ask 5 — the over-import trade in #365, stated so it can be rejected.**  A
`()` prototype now crosses a `use` whether or not the using file imported that
name, so a bareword sharing the name of a module's non-exported `()` sub
becomes a call where perl keeps the string.  That is the same over-import the
parameter-slot rule has always done, and it moves nothing in 951 measured
files — but it IS a widening of what the compiler believes.  Accept as is, or
should the merge cross-check the export scan when it HAS one (import only when
`export_names` is empty or lists the name)?

**Ask 6 — #484's ordering question.**  The #351 `WORD /` repair asks
`_word_is_term`, which reads only this document's terms, and
`_premerge_include_prototypes` runs AFTER the repair block.  Moving the
pre-merge earlier is a seam move (it would walk an unrepaired document, and a
repair can `_reparse_doc`); extraction is memoized by module name, so a second
post-repair merge should be free.  Is "pre-merge, repairs, pre-merge again" the
shape to measure, or should `_word_is_term` get a lazy per-name path instead?

**Ask 7 — two probe-found pre-existing shapes, filed nowhere yet.**  A BAREWORD
operand to a user prototyped sub emits as an unbound CL symbol for `(*)`
(`fh FOO` → `(pl-fh FOO)`, an unbound-variable crash) and as a call to an
undeclared sub for `($)` (`one BAR` → `(pl-one (pl-BAR))`), where perl gives
the string "FOO"/"BAR"; and `close G ? "a" : "b"` is read as
`close(G ? …)` because the strictly-single bareword branch only narrows before
a comma or end-of-run.  Both are unchanged by Q4 (verified on a worktree).
Worth their own tasks now, or do they belong to the #266 classifier campaign
that #481/#482 already point at?
