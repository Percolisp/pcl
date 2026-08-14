# The 14 undiagnosed companion-suite decreases, diagnosed (task #315, s393)

The s392 audit (`docs/perl-suite-audit-s392.md` §3) listed 14 files whose
`C_ok` had fallen against the s323e snapshot with **no cause captured** — only
the first diverging row.  This file gives each one a cause.

**All 14 re-measured first**, in the s393 tree (`--jobs 6 --timeout 300`), and
every one reproduces its snapshot value **to the digit**.  So these are stable
facts, not load artifacts, and none of them moved with s393's fixes.

Nothing here is a fix.  Each row names the cause and the owner; the ones that
are new findings are called out as such.

---

## The one that explains the most: #202 made `unlike` able to fail

**re/script_run.t, 70 → 0 (185 rows, all failing).**

The file is 185 `like`/`unlike` assertions against `qr/(*script_run: … )/` in
its four spellings.  cl-ppcre has no `(*script_run:…)` construct and refuses
the pattern:

    Regex syntax error: Quantifier '*' not allowed. at position 4

Before s330, `unlike`'s scanner call ended in `(error () t)` — *any* pattern
cl-ppcre refused to compile became a **PASS** (`docs/tap-assertion-audit.md`
F1).  Roughly half of this file's assertions are `unlike`, and those were the
70 "passing" rows.  #202 replaced both directions with one matcher whose
scanner error is a `not ok` naming the error, which is why the file now reads
0.

**This is not a regression — it is the same rows, honestly reported.**  The
underlying gap (cl-ppcre lacks script-run assertions) is a regex-engine parity
item, in the same family as #196 and #71.

*Verification note for whoever re-checks this: `./runpcl` merges stderr into
stdout (`2>&1` in the script), so the cl-ppcre warning LOOKS like it is
polluting the TAP stream.  It is not — run sbcl directly with the streams
separated and the warning is on stderr, where the announce policy puts it.
The suite runner's own `> out 2>&1` merge is what puts it in the captured
file; the rows are still parsed, and they are honest `not ok`s.*

---

## Known owners — no new work

| file | Δ | cause | owner |
|---|---|---|---|
| op/readline.t | −4 | `undef-fn:PerlIO::pl-get_layers` | #137 / #139 (PerlIO layer model) |
| op/lex_assign.t | −1 | row "object destruction via reassignment to variable" — DESTROY does not fire | `not-supported.md` §DESTROY, #198 |
| io/dup.t | −1 | PCL's TAP numbering offset from perl's for 3 rows; the runner pairs by description and reports the join, not a lost assertion | measurement artifact (s321/#177) |
| op/pos.t | −1 | same, 10 rows | measurement artifact |

## New causes found by this pass

**op/packagev.t, −17** — `undef-fn:version::pl-is_strict`.  PCL's `version`
shim has no `is_strict`, so every row that validates a version string with it
dies.  One shim function; the layer is `lib/version.pm` (CLAUDE.md 9a), not
the runtime.

**re/regex_sets.t, −20** — two causes stacked: the harness stub
`capture_warnings` is undefined (`undef-fn:main::pl-capture_warnings`), and
the file's subject is `(?[ … ])` extended character-class set operations,
which cl-ppcre does not have.  Same parity family as script_run.

**op/warn.t, −7** — the failing rows assert warn's DEFAULT message text
including the source location: `"foo at warn.t line 29.\n"`.  PCL cannot
produce `warn.t line 29` — the caller-fidelity family (#233: `.lisp`
filename, `#line` ignored).  Note these rows genuinely branch on the text, so
the "error text is not a goal" ruling does not cover them; they are #233's
rows.

**comp/package_block.t, −3 (7 rows, all failing)** — every assertion is a
nested `package Foo { … package Bar::Baz { … } }` *inside a string eval*,
checking `__PACKAGE__` and a further `eval("__PACKAGE__")` at each depth.
This is the eval-region package family (#226 / #240) at its hardest: nested
package BLOCKS inside an eval, two levels deep, with the package visible to an
inner eval.

**uni/parser.t, −3 — NEW BUG, and it is not Unicode-specific.**  The first
row is `is *tèst, "*main::tèst"`.  Probed:

    use utf8;  print *tèst, "\n";   # perl: *main::tèst   PCL: *MAIN::TÈST
               print *plain, "\n";  # perl: *main::plain  PCL: *MAIN::PLAIN

A glob stringifies through the CL symbol's name, which is **upcased**, instead
of the original Perl spelling.  The ASCII case is wrong too — the Unicode row
just happens to be where the suite noticed.  PCL already carries original-case
package names out of band for `caller()`
(`docs/caller-implementation.md`); the glob printer does not consult it.
**Filed as #316.**

**op/select.t, −2 (no TAP at all)** — the file's plan line is

    plan reverse 9;

and PCL's TAP layer dies with `plan(): unrecognized plan form (#(N))`.  Probed:
argument flattening is NOT the bug — `f(reverse 9)` and the paren-less
`f reverse 9` both pass one flattened arg to a perl sub, matching perl.  The
difference is that `plan` is a RUNTIME function (`cl/pcl-test.lisp`), so it
receives `(p-reverse 9)`'s value directly — a one-element VECTOR — and its
form dispatch has no case for it.  Per #202 `plan` is the one assertion that
may die, so dying is right; recognising a legal perl spelling first is the
fix.  **Filed as #317.**

**op/array.t, −1** — row `$a[-1] = 0`, in the block that tests writes to a
freed/deleted array.  Not diagnosed further; single row.

**op/tr_latin1.t, −1** (row `gh#17227`) and **re/bigfuzzy_not_utf8.t, −1**
(row `[perl #134329]`) — single rows in files that are otherwise **OK**.  Both
are Latin-1/UTF-8 boundary cases in `tr` and in the regex engine.  Left as
single rows; they cost one assertion each and neither file's status changes.

---

## What to do with this

1. The re/script_run.t entry should be **registered**, not fixed: it is a
   cl-ppcre parity gap with a blessed reason, and the row count is now
   honest.  Same for re/regex_sets.t's `(?[ … ])` half.
2. #316 (glob case) and #317 (plan form) are new, small, and independent.
3. op/packagev.t is one shim function.
4. comp/package_block.t belongs to the eval-region package work, not here.
5. op/warn.t belongs to #233.
6. The two TAP-renumbering rows are not losses at all and should stop being
   counted as such — that is #257's per-row cause column, not a fix.
