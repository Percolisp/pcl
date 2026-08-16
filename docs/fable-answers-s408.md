# Fable answers — the s408 batch (s409, 2026-08-16)

Answers to `docs/opus5-review-requests-s408.md` (seven code commits,
`7f930fc` … `9120a9f`).  Verdict first, then the independent verification,
then what the review's probes found, then the asks.  The queue from here is
`docs/plan-post-s408.md`.

## 0. Verdict

**s408 APPROVED as shipped — all seven code commits.**

| commit | what | verdict |
|---|---|---|
| `7f930fc` | **#337** `my sub` / `state sub` are LEXICALS — scope-unique rename + the uses its region owns, incl. interpolated code | approved; the "latest start wins" design is right (§3.7); three uncovered spellings filed as **#376** (§2.1), none a regression |
| `719ecf0` | **#360** the core feature pragmas via PPI's `custom_feature_include_cb` table + `lib/experimental.pm` | approved; both un-ruled decisions (bundle REPLACES; unknown name → EMPTY answer) confirmed by probe (§1) |
| `ed67333` | **#364** a string eval inherits its site's features; fifth protocol line; joins the eval cache key | approved |
| `850a4bf` | **#363** an eval-mode drop DIES into `$@`; **#375** `qq {…}` delimiters | approved; the −46 companion trade RATIFIED (§3.4); #375's blast radius closed by measurement (§3.5) |
| `5fef203` | **#367** per-file SESSION isolation + reap; **#366** serial re-run of movers | approved; **one real hole fixed here** — a `--quick` NOT-RUN row was treated as a mover and re-run ALONE at its full allowance (all 11 of them, +23 min, and the report was un-quicked) — plus the three-way label (§4) |
| `49b8a0c` | **#368** anon `__SUB__` dies | approved as a rule-12 die — but its companion COST was not measured: `op/sub.t` 51/14 → 25/6 (the file aborts at its `sub {…; CORE::__SUB__->()}->()`).  The feature is modern Perl's recursive closure, so **#378 IMPLEMENTS it** (a self-reference rewrite) in the next session (§2.3) |
| `9120a9f` | **#374 half (a)** a statement keyword is never a function; `statement_modifiers`/`statement_keywords` in Config | approved; half (b)'s description CORRECTED in the task (§3.6) |

No regression was found.  Two pre-existing bugs were found by the review's
probes and filed (**#376**, **#377**, §2.1–2.2) — both are in the family s408
opened — and the companion run found one UNMEASURED cost of #368 (**#378**,
§2.3).  All three head the next Opus session.

## 1. Independent verification

| measurement | this session |
|---|---|
| Gate `tools/prove-core`, **cold** (`~/.pcl-cache` cleared), at `74c1b93` | **149 files / 5442 rows**, the only failures the 13 pclxs xs rows (xs-01 5 of 6, xs-02 4, xs-03 4) — s408 wrote 149/5439; the difference is the xs files' own row count (§3.1) |
| Full perl-tests sweep, `--jobs 8` | **GATE clean: 0 new / 0 fixed, TOTAL 18513 = baseline, drops 12 = census**; 6 UNSTABLE + 10 unverified, all in postfixderef.t / ref.t / yadayada.t / eval.t / magic.t / tr.t — the PARTIAL files, crash-file noise as before |
| Companion `--all --quick --jobs 4` | **523 files: 87 OK, 30 NOTAP, 110 XDIFF, 1 FIXTURE, 295 UNEXPLAINED — 3 measured movers vs the snapshot, each re-run ALONE**: `op/signatures.t` 912/334 → **920/326** (a WIN: #374(a) makes the eight statement-keyword `sub ($x = if, $y) {}` evals an error, as perl; STALE → rows re-blessed, XDIFF again); `op/sub.t` 51/14 → **25/6 — #368's die aborts the file at line 214, a COST s408 did not measure** (§2.3, task #378); `mro/package_aliases_utf8.t` = its registered rows-unstable.  Plus **11 NOT-RUN files wrongly re-run serially** — a #366 × #345 runner bug, fixed here (§4); it turned a ~20-min `--quick` into 43 min |
| Probes vs perl 5.40.3, 27 shapes (lexical subs 17, `qq`/regex whitespace-delimiter family 1, feature pragmas 3, eval-drop 2, `__SUB__` 1, keyword-as-function 1, nested-sub capture 2) | **19 identical**; 4 differ exactly as REGISTERED (string-eval hole #373 — loud; loop-body `my sub` — the capture refusal, loud; anon `__SUB__` — dies; the keyword-named sub — a counted drop); **4 differ UNREGISTERED → #376 (three spellings) and #377** |
| #360's two decisions | `use v5.36; eval q{try…}` → `$@` set (bundle does not enable try; PPI's own answer would have); `use v5.40; try` runs; `use experimental qw(signatures try)` runs both; `no feature 'try'` in an inner block turns it off for an eval there — all as perl |
| #364 | `use feature 'try'; eval q{ … try {…} catch ($e) {…} }` runs under PCL exactly as perl; the two orders (feature-on first / feature-off first) both correct — the cache key holds |
| #363 | `eval q{ 1 + ; }` → undef with `$@` SET; a good eval leaves `$@` empty — as perl |
| #375 | `qq {…}`, `qq (…)`, `qq\n[…]` (newline before the delimiter), `m {…}`, `s {…} {…}`, `qr {…}`, `tr {…} {…}` — **all identical to perl** (the regex family was the ask's "third place"; there is none, §3.5) |
| #368 | named `__SUB__` 120 = perl; anon dies inside `eval {}` |
| #374(a) | `eval q{ my $y = if if if; 1 }` → eval error, program continues, a following `if (1) {…}` statement runs |

The seventeen lexical-sub probes that MATCH perl (all inside the token stream
and interpolating text): nested shadowing with a use before the inner
declaration; a package sub of the same name called before the `my sub` in the
same block, then `main::f()` and `->can('f')` after it; `"@{[ f() ]}"`,
`${\ f() }`, a heredoc body, a pattern, with a fat-comma key, a bare hash key
and a method name of the same spelling all left alone; `\&f` captured before a
sibling redeclaration then `defined &f`, `&f()`, `goto &f`; a prototype and a
signature on a lexical sub in a `package Foo {}` block with `Foo->can` NOCAN;
two lexical subs calling each other; two anon subs each capturing its own
block's `x`; `sort bynum LIST` with a lexical comparator; `__SUB__` inside a
`state sub`; a lexical sub named after a builtin (`max` with List::Util's in the
outer scope, `uc`); `my sub inner { 7 }` inside a sub that reads its own
`shift`ed param outside the nested sub.

## 2. Findings of the review — filed (2.1 and 2.2 PRE-EXISTING, verified in a worktree at `b7ce704`; 2.3 is s408's own, unmeasured)

### 2.1 #376 — the lexical-sub rename's three uncovered spellings

```perl
{ my sub c; sub c { "c1" } print c(), " " }        # (a) forward-declaration idiom
{ my sub c; sub c { "c2" } print c(), "\n" }       #     perl: c1 c2     PCL: c2 c2  ← SILENT WRONG
my sub f { "L" }  { package O; sub f { "M" } }      # (b) a plain `sub f` in the region DEFINES THE LEXICAL in perl
print f(), " ", (O->can('f') ? "O::f exists" : "no O::f");   # perl: M no O::f    PCL: L O::f exists
my sub helper { "H" }  package Other;  sub go { helper() }    # (c) use from another package's code
package main;  print Other::go();                            # perl: H    PCL: undefined-function (in Other)
```

(a) is the exact bug #337 fixed, in the spelling #337 chose to skip (the pass
has `next if !$st->block`, with a comment that the fill-in "is a package-sub
token shape").  perlsub documents this idiom for mutually recursive lexical
subs.  (b) is what makes (a) work in perl: inside the region, `sub NAME {…}`
IS the lexical's definition; `_lexsub_renamable` returns 0 for it.  (c) is a
loud crash — and it crashed BEFORE #337 too (`Other::helper`), so it is not a
regression; the renamer can now fix it because it already rewrites the token.
Fix shape (three edits, one pass) is in the task; the sweep is the gate.

### 2.2 #377 — `my $x = shift` promoted for a nested sub gets NO cell

```perl
sub outer { my $x = shift; my sub inner { $x * 2 } return inner(); }
print outer(3), " ", outer(4);      # perl: 6 8     PCL: unbound-variable $x__file__0 — CRASH
```

The plain `sub inner` twin crashes identically (perl there: `6 6`).  The
emission is the discriminating measurement: `(p-raw-params ($x__file__0) …)`
binds the promoted name LEXICALLY inside `pl-outer` and no `(p-defcell
$x__file__0 …)` is emitted, so the nested sub reads a free special that was
never declared.  `my $x = 70;` in the same position emits the cell and runs;
`my ($x) = @_;` is the capture REFUSAL — three spellings of "take a param",
three different answers.  Not #337's — but #337 makes the shape the FIRST
thing a `my sub` user writes, which is why it heads session H.

### 2.3 #378 — anon `__SUB__` must be IMPLEMENTED, not only refused

`#368`'s die is right by the s329 boundary (a value the program consumes), and
the request's claim "`op/current_sub.t` is unchanged — it never reached the
stub" is true.  But `op/sub.t` DOES reach it — line 214, the `[perl #122845]`
closure-recursion test, `sub { local $depth = $depth + 1; …; CORE::__SUB__->()
}->()` — and since that is not inside an eval the die aborts the file: **51/14
→ 25/6, 26 passing rows lost**, both runs agree.  s408 re-ran only
`op/current_sub.t` after #368; the WHAT-TO-RUN row for a `cl/**` change says
"the dirs the change touches", and the dir was `op/`.

The shape matters beyond the count: `__SUB__` inside an anon sub is THE
modern-Perl spelling of a recursive closure (a named sub can call itself by
name; the anon case is the one real code writes).  So the answer is not to
soften the die but to implement the feature — **#378**: at the one PPI entry
that already rewrites a NAMED sub's `__SUB__` to `\&name`, an anon sub whose
body mentions `__SUB__` becomes `do { my $__SUB__N; $__SUB__N = sub { … $__SUB__N
… }; $__SUB__N }` — a shape PCL already compiles (`my $walk; $walk = sub {
$walk->() }`), zero new mechanism, zero cost where the token is absent, and
`__SUB__ == $f` holds.  Snapshot row edited by hand with the cause; the
task carries the five probe shapes and the bar (op/sub.t back to ≥ 51/14).

### 2.4 Not filed, noted

- **#374 half (b) was mis-described** and the task is corrected: `my $x = if
  if if` deparses to `(my $x = if()) if if()` — the middle `if` is the
  statement-modifier KEYWORD; only term-position occurrences are calls.  The
  renamer rewrites all three, which is what makes the statement unparseable.
  The fix is position-aware renaming (or excluding statement keywords from
  the rename), in the renamer's classification — not term grammar.  Exotic;
  stays where it is.
- The `#366` re-run phase treated a `--quick` NOT-RUN row as a mover — 11
  files re-run ALONE at their full allowances (the #326 hang set, +23 min)
  and their NOT-RUN rows overwritten with serial verdicts, un-quicking the
  report; and its label asserted "serial matches the snapshot" for every
  serial≠parallel row without checking (mro/package_aliases_utf8.t was a
  three-way disagreement).  Both fixed here (§4).

## 3. The asks, ruled

### 3.1 §8.1 — the gate row count

**Yes, and it is now a standing rule (DECIDED s409): a gate row count is
compared against a measurement of the SAME tree.**  The three pclxs files
`plan` 6+4+4 = 14 rows but PRODUCE between 0 and 14 depending on where
pclxs's current state aborts them, so a written-down total is stale the
moment pclxs moves.  When only a written number is available, subtract the xs
rows produced in each run before comparing (this session: 5442 − 14; s408's
5439 − 11 — the same 5428).  The per-file worktree compare stays the
authoritative check.

### 3.2 §8.2 — #373 (a string eval cannot see a lexical sub)

**Confirmed: registered, not fixed, and it does not jump the queue.**  With
#364 shipped, its seam exists — a sixth protocol line (the sub-capture alist:
name → renamed symbol for every lexical sub whose region covers the site),
keyed into the eval cache exactly as the features are.  Population is ~0 (the
corpus's only `my sub b; sub b` pairs sit INSIDE eval strings, where they are
whole and untouched).  It becomes worth doing only if #341's per-row read of
`t/op/lexsub.t` shows rows behind it (plan §2 item 4).

### 3.3 §8.3 — the interpolation reach (fragment mini-parse)

**Acceptable as shipped.**  A `PPI::Document->new` on an interpolated span is
not a second pattern — it is the codebase's established one: 28 fragment sites
in `StringInterpolation.pm`, `ExprToCL.pm` and `Parser.pm` already do exactly
this.  The "ONE construction site" claim of #360 is about FULL documents
(`_ppi_new`), and it still holds.  What matters is that the classification is
the same predicate the token stream uses (`_lexsub_use_name` +
`_lexsub_renamable`), and it is.  Folding the fragment path into something
else would be the InterpScan-side work of standing rule §8, which is not
scheduled and would not change the answer.

### 3.4 §10.4 — the −46 companion trade

**Yes — and the alternative is REJECTED, not deferred.**  perl's contract for
`eval STRING` is "what does not compile sets `$@` and the eval returns undef".
"Announce over the protocol and continue" would keep the wrong VALUE (undef
returned, `$@` empty) that the program then consumes — rule 12's
value-flows-onward case, the s329 boundary, and exactly what op/smartmatch.t
shows: 99 rows satisfied "does not match" with an undef that came from a
statement that never ran.  A lost row whose assertion is "no error" about a
construct PCL cannot compile is not a cost.  Standing rule (DECIDED s409).

Two consequences to carry: (i) op/smartmatch.t's 99 rows are Track A #371's
smartmatch-infix arm — same verdict later, but the `$@` text becomes the ruled
`PCL: unsupported …` refusal naming the feature; (ii) FILE mode's flip is
still Option B phase 2's LAST step (plan-post-s400 §3 item 3), because file
mode HAS an announcer and the census must fall first — the reasoning is the
same, the sequencing is not.

### 3.5 §10.5 — #375's blast radius

**Closed by measurement.**  Grepped every hand-strip of a quote-like's
opening: `ExprToCL::convert_perl_string_form` already tolerates `\s*` between
`qq`/`q` and the delimiter (all four bracket pairs and the generic form); the
five `qw` sites in Parser.pm/Parser2.pm all strip `qw\s*`; the two fixed sites
were the only ones taking the character right after the operator.  Probed the
REGEX family, which is the plausible third place: `m {…}`, `s {…} {…}`, `qr
{…}`, `tr {…} {…}` — all identical to perl.  No filler needed.

### 3.6 §7 + §14 ask 6 — the census increase, and half (b)

**"Census up, with the trade argued in the edit note" is the right shape, and
it is now a standing rule (DECIDED s409): a census INCREASE is legal when it
converts a WORSE failure into a counted drop** — a crash-form the census
cannot see (a form that fails at macroexpansion and takes the file with it) →
an announced, counted drop.  Conditions: the edit note names the form it
replaced and the task that owns the residue; the file's verdict does not
regress; sweep TOTAL/LOST unchanged.  All three held (6/8 → 7/10 after half
(a); TOTAL 18513).  Holding such a change until the drop is fixed would freeze
the census as a ratchet on a metric that does not count crash-forms — the
census exists to make drops countable, not to freeze them.

Sub-question 1 (do half (a) first) — you did, correctly, and the cause was
better than the task's guess: `if` in `%RUNTIME_NAMES`.  Sub-question 2 / ask
6 — **half (b) stays behind Option B phase 2**, but with its description
corrected (§2.3): the shape is position-aware renaming of a keyword-named
lexical sub, not `TERM TERM TERM`; whichever session touches it fixes the
RENAMER's classification, not the term grammar.  Exotic; three statements in
one file.

### 3.7 The design point the request asked to have reviewed (§2, "nesting needs no shadow test")

**Confirmed.**  "A use is claimed by the covering declaration with the LATEST
start" is correct because (i) a declaration's region is [its own first token,
last token of its scope] clipped at a same-scope sibling redeclaration, and
(ii) `_has_ancestor` restricts candidates to declarations whose scope encloses
the token — so of two candidates, the later-starting one is nested inside the
earlier one's scope or is its sibling successor, and in both cases perl's
answer is the later one.  The one thing the variable-side scope walk does that
this cannot is the position-aware keyword case above (§2.3), which is not a
scope question.  The probes (nested-shadow with a use before the inner
declaration; sibling redeclaration with a captured `\&f`) exercise both legs.

## 4. Housekeeping done this session

- `tools/run-perl-suite.pl` (#366): (i) a NOT-RUN or KILLED parallel row is
  never a mover — only a measured verdict can move (the 11 wrongly re-run
  files this session); (ii) the re-run report line says which of the three
  values agree (parallel / serial / snapshot) instead of asserting "serial
  matches the snapshot" unconditionally.  Runner row honoured: the full
  `--all --quick --jobs 4` was RE-RUN with the fixed runner — **18 min (was
  43)**, the report keeps its 13 NOT-RUN rows, and the 510 measured files
  compare file-by-file against the first run **508 identical + 2 explained**
  (op/signatures.t STALE → XDIFF, the re-bless; op/utf8cache.t DIFF → TIMEOUT
  at the same 2/0 — the load-sensitive flip its snapshot note already
  records).  Its two movers were labelled correctly: mro/package_aliases_utf8.t
  THREE-WAY (registered rows-unstable), op/utf8cache.t REAL MOVE at equal
  counts.
- `docs/perl-suite-expected-rows.tsv`: `op/signatures.t` re-blessed with
  `--bless-rows` — exactly the eight statement-keyword rows left it (read
  first, then blessed).  `docs/perl-suite-run.tsv`: two rows edited by hand
  with their causes (op/signatures.t 920/326; op/sub.t 25/6 → #378).
- Tasks: **#376**, **#377**, **#378** filed with reproducers and fix shapes;
  **#374** half (b) corrected; **#368** carries its measured cost.
- `docs/plan-post-s408.md` written (the queue from here); `plan-post-s400.md`
  §2d points at it; DECIDED s409; CLAUDE.md pointer 2e updated; session log;
  memory STATE line.
