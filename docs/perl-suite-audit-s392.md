# Companion-suite snapshot audit (task #304, s392)

`baselines/perl-suite-run.tsv` was stamped `1e7c4d7 (s323e)` — **191 commits
stale**, and taken BEFORE E4.1 (#242, s356) turned v1 gates into hard errors
and retired `--lenient-ppi`.  So it measured a compiler with a silent v1
fallback; today's has none.  This file is the per-file audit that had to
happen **before** re-blessing, the companion suite's analogue of what #223 did
for the sweep baselines: bless first and 44 decreases become the new normal,
hiding the next real one.

**Run:** `tools/run-perl-suite.pl --all --jobs 6` at `582667e`, 523 files —
89 OK / 30 NOTAP / 101 XDIFF / 1 FIXTURE / 302 UNEXPLAINED.  The 8 TIMEOUT
files were then re-measured **in the same tree at `--timeout 300`** and
spliced in (§1).

## 1. Seven of the 44 "decreases" were my own flag — not lost coverage

A TIMEOUT row's `C_ok` is not a property of the compiler; it is *how far the
file got before the cutoff*.  The stale snapshot's rows for these files were
measured at `--timeout 300` (its own header says so: s325, task #195), and my
full run used the 90 s default.  Re-measured at 300 s:

| file | snapshot | s392 @90s | s392 @300s |
|---|---|---|---|
| re/regexp.t            | 793 | 505 | **793** |
| re/regexp_noamp.t      | 794 | 509 | **794** |
| re/regexp_notrie.t     | 793 | 527 | **793** |
| re/regexp_qr.t         | 793 | 519 | **793** |
| re/regexp_qr_embed.t   | 793 | 497 | **793** |
| re/regexp_trielist.t   | 793 | 525 | **793** |
| comp/require.t         | 543 | 410 | **909** (+366) |

Identical to the digit for the six `re/regexp*` files, and comp/require.t is
a large *gain* the stale snapshot was hiding.  **Rule this establishes: never
compare TIMEOUT rows measured at different `--timeout` values.**  The blessed
snapshot carries the 300 s rows for all 8 (re/overload.t included), and the
splice is recorded in its header.

That leaves **37 real decreases**, below.  (36 increases too — §4.)

## 2. The TRANSPILE cluster: 21 files, 7 families, all E4.1 hard refusals

14 of them are in the decrease list; the other 7 already read 0.  Every one
dies at transpile with a `Parser2 TODO:`-class refusal.  **None has a
`docs/not-supported.md` owner** — a `Parser2 TODO:` is a compiler gap, not a
blessed non-support, so by #304's rule these are category (b), *unexamined
losses*, and are filed as task **#314**.

| family | refusal text | files |
|---|---|---|
| F-A1 comma-list `my` | `unsupported declaration: my @raw, @upgraded, @utf8;` | opbasic/cmp.t **(12078 rows)** |
| F-A2 attributed `my` | `unsupported declaration: my ($cows, @go, %bong) : teapots = …` | op/attrs.t, uni/attrs.t |
| F-B `our` non-assign | `unsupported our declaration: our $count++;` / `our \$TODO = \$::TODO;` | op/inccode.t, op/repeat.t, re/opt.t |
| F-C foreach head | `foreach without list` | op/const-optree.t, op/for-many.t |
| F-D my spans package | `my-lexical 'X' spans a package boundary` | io/shm.t, op/svleak.t, op/taint.t |
| F-E our shadows my | `our '$y' shadows a my-lexical` | comp/our.t |
| F-F state | `state $classcount in named sub (string eval)`; `non-scalar state declaration outside a named-sub statement: @a` | op/coresubs.t, op/lvref.t |
| singles | `Failed to compile s///e expression '"${\<<END}"` | base/lex.t |
| | `compound 'try'` | op/try.t |
| | `self-referential init: my %base_opts = ( … )` | run/runenv.t |
| | `lexical 'x__file__2' possibly captured by nested sub` | op/lexsub.t |
| | `Handle single node of unknown type: ref='PPI::Token::Operator'` | re/pat_rt_report.t |

Two have owners already and are **not** part of #314:
* **op/for.t** — upstream PPI 1.291 lexer bug, `docs/ppi-upstream-bugs.md` §6
  (task #253).  Its s323e rows were `--lenient-ppi` truncation artifacts.
* **re/reg_eval_scope.t** — task #269, parked behind #196; all 47 assertions
  are `(?{ })`, so de-gating buys ~0 rows.

Proved NOT #291 at s388h and not re-derived here: the same 21 files produce
byte-identical first-error lines at `66bdb93` (main immediately before #291).

## 3. The rest of the 37, each with its verdict

**LIVE CRASH REGRESSION — task #313** (found by this audit; the whole reason
the audit had to precede the bless):
* `mro/next_edgecases_utf8.t` 9 → **0** (XDIFF → DIFF, no TAP at all)
* `mro/basic_utf8.t` 10 → 9 (XDIFF → DIFF)
Both die with `Cannot proclaim a macro variable special: <pkg>::@ISA`.
Cause: `Pl::GlobalPartition::_split_name` matches a package segment's first
character ASCII-only (`[A-Za-z_]`), so a package whose name *starts* with a
non-ASCII letter falls to the EXCEPTION partition and is declared `defvar`,
while `p-defpackage` has already made that package's `@ISA` a symbol-macro
cell.  12-line reproducer and the fix bar are on #313.  **These two rows are
owed back**; the snapshot records them at their crashed value.

**Ruled refusals, owner exists — no action:**
* `op/tr.t` 270 → 92: the F6 oversized-run-form refusal
  (`docs/fable-answers-s346.md` §2.3), the same event the sweep already
  carries for tr.t.
* `op/decl-refs.t` 187 → 42: `Parser2 TODO: state outside a block-level
  declaration` inside an eval — family F-F above, folded into #314.
* `op/not.t`, `op/ver.t`, `mro/package_aliases_utf8.t`: XDIFF, registered;
  package_aliases_utf8.t is the `*rows-unstable*` file whose row count is
  nondeterministic by registration (s336).

**Measurement, not regression:**
* `re/pat_advanced.t` 1073 → 937: the snapshot value is pre-E4.1; the
  timeouts registry already records this file as ~936 rows measured at s363
  (#254 B-i).  Today's 937 IS the registered value.

**Registration drift (XDIFF → DIFF, needs a rows re-bless, not a fix):**
* `comp/hints.t` 17 → 15 — 2 unregistered diverging rows ("tied hint hash is
  copied to inner scope").

**Unexamined losses — filed as task #315** (first diverging row captured for
each; none diagnosed further this session):
| file | Δ | first diverging row / signature |
|---|---|---|
| re/script_run.t | −70 | `Cyrillic 'paypal' with a Latin 'l' is not a script run` |
| re/regex_sets.t | −20 | `undef-fn:main::pl-capture_warnings` (harness stub) |
| op/packagev.t | −17 | `undef-fn:version::pl-is_strict` |
| op/warn.t | −7 | `[at warn.t line 30]` |
| op/readline.t | −4 | `undef-fn:PerlIO::pl-get_layers` (known, #137/#139) |
| comp/package_block.t | −3 | first row, no description |
| uni/parser.t | −3 | `sanity check.` |
| op/select.t | −2 | `plan(): unrecognized plan form (#(N))` — no TAP |
| io/dup.t | −1 | TAP renumbering (3 rows) |
| op/array.t | −1 | `$a[-1] = 0` |
| op/lex_assign.t | −1 | `object destruction via reassignment to variable` |
| op/pos.t | −1 | TAP renumbering (10 rows) |
| op/tr_latin1.t | −1 | `gh#17227` — **file left OK** |
| re/bigfuzzy_not_utf8.t | −1 | `[perl #134329]` — **file left OK** |

## 4. What the stale snapshot was also hiding: 36 increases

Biggest: comp/require.t +366 (§1), comp/parser.t 0 → 65 (TRANSPILE → DIFF),
op/universal.t +60, io/open.t +58, op/eval.t +17, op/closure.t +13,
op/tiehandle.t 0 → 12, op/while.t +9 (TIMEOUT → DIFF), and four files that
reached **OK**: op/push.t, op/splice.t, op/unshift.t,
mro/inconsistent_c3_utf8.t.

Net over all 523 files: **TOTAL C_ok 72159 → 59640**, i.e. −12519, of which
−12078 is opbasic/cmp.t alone (one `my @a, @b, @c` declaration).

## 5. Conclusions the next session should not re-derive

1. A TIMEOUT row is only comparable against a row measured at the SAME
   `--timeout`.  Re-measure before diffing, or the flag reads as a regression.
2. The companion suite has no LOST bucket, so a C_ok decrease is invisible
   until someone diffs by hand — #304 step 3 (give it the sweep's #204
   treatment) stays open, now with 191 commits of evidence for why.
3. The E4.1 flip's coverage cost on the companion suite is concentrated: ONE
   declaration shape (`my @a, @b, @c`) is worth 12078 rows of opbasic/cmp.t.
4. **The audit found a live crash the gate and the sweep both miss** (#313) —
   utf8 package names occur in `t/mro/*_utf8.t` and nowhere in perl-tests/ or
   Pl/t.  That is the argument for keeping this snapshot fresh.
