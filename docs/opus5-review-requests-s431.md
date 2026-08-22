# Review requests — session 431 (Opus 5, 2026-08-22)

**Session P1 of `docs/plan-post-s430.md`: the flip re-census, measured and
classified.  No compiler change — measurement, classification, ten task
filings and one instrument fix.**  The deliverable is
`docs/drop-census-s431-flip-gate.md`; this doc is the asks.

Generation unchanged (**v2-177**), `cl/` untouched, `Pl/` untouched.  Nothing
in this commit can move an emission — so per the s401 WHAT-TO-RUN table no
sweep and no companion run were owed, and none was taken beyond the five
single-file measurements of §4.

---

## §1  What was measured

1. **The census, COLD** (`rm ~/.pcl-cache/*.lisp`, then
   `tools/drop-census.pl "$PWD" out.tsv 8`, 1m16s): **27 files / 82 drops,
   row-for-row identical** to the blessed census.  `tools/drop-harvest.pl`:
   the 82 statement texts.
2. **About 60 probe shapes vs perl 5.40.3** — six rows whose s419 classification
   did not survive one (§2), and the module drops of §5, eight of which
   minimised to three one-line bugs.
3. **The price of each flip option** (`drop-census-s431-flip-gate.md` §4) —
   the table P1 owes the Fable design.
4. **The module populations, censused for the first time** (§5 of that doc):
   board `lib/` 28 modules → 3 carry 5 drops; `cpan-tests/modules` 94 `.pm` files
   → 9 carry 15.  This is what flip-gate §5's "first free increment" asked
   for, and the answer is that it is not free.
5. A four-population scan (1139 files) for the #457 shape: **zero sites** in
   the repo, two in Text::Balanced.

## §2  The finding that changes the flip's arithmetic

**A dropped statement inside a file `perl` itself compiles is valid Perl by
construction.**  Applying that criterion row by row moved six s419 rows out
of "deliberate torture" and into "gap" — leaving exactly ONE deliberately
invalid row in the whole census (`t/comp/final_line_num.t`'s `print 1+`).

But the number that decides the flip's shape is the other one: **four
REGISTERED absences sit in files worth 2581 passing rows** —
`perl-tests/sprintf2.t` 1631 rows for one hex-float block, `t/re/pat_advanced.t`
950 for four regex code blocks.  s419's arithmetic put registered rows on the
"flip-legal today" side.  That is true per ROW and false per FILE, and the
flip's unit is the file.

Priced (rows lost, a died transpile contributing zero):

| option | files lost | rows lost |
|---|---:|---:|
| A — flip everything | 27 | **5300** |
| B — exempt lvalue + indirect object | 19 | **3022** |
| C — B + exempt the registered/deliberate | 15 | **441** |
| D — fix the 31 gaps first, then flip | 0 | **0** |

## §3  THE ASKS

### Ask 1 — the exempt/registered families need a SHAPE, and it is one question, not two

The plan (§2.1) framed the open question as *"what does a flip do with the 39
lvalue rows and the 4 indirect-object rows"*.  The price says the question is
larger and simpler: **the same shape has to serve the 7 registered rows and
the 1 deliberate row too**, or the flip costs 2581 rows for four statements
nobody intends to implement.

My reading (not a decision — the design is Fable's): a **perl-shaped,
trappable refusal emitted AT THE DROP SITE**, dying when the statement
*runs* rather than at transpile, serves all four verdicts at once: it keeps
every row before it, it kills the silence (which is the whole point of the
flip), and it makes the failure assertable in a `$@` row.  The transpile-time
DIE then applies only to gaps, whose files are supposed to be fixed anyway.
If that shape is right, option C's 441 rows is the whole bill and the unblock
list is five tasks.  **Is that the shape — and if so, does the deliberate row
(`print 1+`, where perl itself refuses to compile) take it too, or does that
one stay a drop?**

### Ask 2 — does the module-mode increment now go BEHIND its own unblock list?

`drop-census-s419-flip-gate.md` §5 parked module-mode DIE as the candidate
first increment, pending the board count.  The count is 3 board modules (5
drops) + 9 cpan-tests modules (15) — and one of the three is Text::Balanced,
a dist with **780 passing board rows**, whose two drops are one new bug
(#457).  So the increment as written would abort every program that loads
Text::Balanced or the Test2 stack.  **Confirm it goes behind those 20 drops
(and that #457, being a one-family repair with zero in-repo sites, is the
first of them), rather than being taken as a cheap first step.**

### Ask 3 — a census that cannot see modules cannot evaluate the flip's bar (#462)

`tools/drop-census.pl`'s population is perl-tests + perl's t/ + `lib/`;
`corpus-diff.pl`'s SILENT-DROP counter is perl-tests only.  Neither sees
`cpan-tests/modules` (94 `.pm` files, 402 counting its `.t`) or the board dists — which is exactly where
#457 sat unseen, in the most-used dist on the board.  **Is #462 (extend the
census to the module populations, board dists behind a flag since
`~/.cpan/build` is outside the repo) a precondition of the flip, or a filler?**
I filed it as a filler; the flip's bar ("every remaining drop explained")
argues for precondition.

### Ask 4 — the two instrument edits I made without asking

Both are hygiene on measurement inputs, no emission involved:

1. **`docs/perl-suite-run.tsv` gained five rows** — `comp/line_debug.t`,
   `op/goto.t`, `op/lex.t`, `op/require_errors.t`, `run/dtrace.t` had NO row
   at all (523 rows for 528 files), so a regression in them could never read
   as a mover (#176 family).  Measured s431 (parallel + serial) and spliced
   with their FIRST measurement, each marked `# s431 first measurement`, with
   the reason in the file header.  None is a mover by construction.
   `op/goto.t` TRANSPILE-FAILs on the #314 refusal; `run/dtrace.t` is NOTAP
   under perl itself.
2. **`docs/parse-error-drop-census-s399.tsv` was NOT touched** — the run was
   identical, so there was nothing to edit.

**Is the snapshot splice the right form for a never-measured file** (as
opposed to a mover, which the recipe covers), or should such rows carry a
distinct marker/status so they are visibly first measurements forever?

## §4  What was FILED and not fixed (ten tasks)

| # | finding |
|---|---|
| **#457** | `f(...)-g(...)` with no space: PPI lexes `-WORD` after a term-ending token as one negative-bareword Word → whole statement DROPS.  Third sibling of ppi-bugs §12/§15, both already repaired with the same `_ends_term` predicate.  Zero sites in the repo, TWO in Text::Balanced (780 board rows).  Needs a §25 + `ppi-bug-report.t` row in the workaround's commit (rule 13). |
| **#458** | `_repair_word_match` (§11) declines when the pattern carries a quantifier: `ok /a*/, "d"` mis-compiles to DIVISION (crash), `ok /a?/, "d"` drops with a bogus ternary error AND swallows the next statement.  Cause located: the repair searches forward for a closing `Operator(/)` that PPI has already absorbed into a `Regexp::Match` token running to EOF.  Owns the census row t/re/pat.t:106. |
| **#459** | SILENT WRONG: `sh(/nomatch/, "d")` passes 2 arguments, perl passes 1 — a failed match in list context contributes `""` instead of the empty list in an ARGUMENT list (the assignment path is right). |
| **#460** | SILENT WRONG: `{ my sub y { 8 } print y, "\n" }` prints 0 (perl 8); a bare `x;` statement-position call DROPS.  The quote-like-operator names that #361/#376 did not cover; owns the four t/op/lexsub.t rows. |
| **#461** | #410 residue: 4 census rows survive its close — `<NON-ASCII-FH>` (uni/readline.t ×2) and `++${"\xff::foo"}` (uni/method.t ×2).  The second half is really `++${STRING}` (ASCII too — op/universal.t drops the same shape). |
| **#462** | The census's module blind spot (Ask 3). |
| **#463** | The glob-surgery / symbolic-ref family, 13 of the 31 gap rows, which lost its owner when B3 closed without touching it (`$${$_[0]}` — PPI reads `$$` as the PID; `*X = *-`; `local *a = *1`; `*{;undef}`; `*^R = *g`; `++${"…"}`). |
| **#464** | A statement MODIFIER on two statement classes drops the whole statement: `require $m if 1;` and `local($\, $,) = (undef,"") if 1;` — both right without the modifier, and `require strict if 1;` / `if (1) { require $m }` / `local (@a) = (1,2) if 1;` are right too.  Minimised from 5 of the 20 module drops (Test2 ×4, Sub::Uplevel). |
| **#465** | SILENT WRONG: `$\` and `$,` are DEFINED in PCL, undef in perl (`$/`, `$;`, `$!` agree) — so Test2::Formatter::TAP's `… if $\ \|\| $,;` takes the branch perl skips.  Runtime-only, so it owes a sweep. |
| **#466** | `print $_ "x\n"` — `$_` in the FILEHANDLE slot drops the statement (`my $fh` is right, and the `for` modifier in the source is not the trigger).  PPI hands `$_` over as `Token::Magic`, not `Symbol`. |

`#415` was updated, not re-filed: it now owns 11 rows (the six re-classified
ones plus its three surviving items), and two of its old items left for #458
and #463.

## §5  What I did NOT do, and why

* **No compiler change.**  P1 is measurement; every gap the census names is
  someone's queued task, and the s366 filler rule wants a session per family.
  #457 was tempting (one repair, an existing family, a top-20 dist) — it is
  filed with its fix shape and its bar instead, so P2–P5 or a Fable ruling can
  place it.
* **No sweep, no companion `--quick`.**  Nothing under `Pl/`, `cl/` or `lib/`
  changed; the only runs were the five single-file measurements of Ask 4.
* **No census re-bless.**  The run was identical to the blessed file.
