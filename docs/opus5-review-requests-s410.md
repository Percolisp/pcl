# Review requests — session 410 (Opus 5, 2026-08-17)

Session H of `docs/plan-post-s408.md` §2 is COMPLETE (#378 → #377 → #376 →
#341 measured); session I item 5 (#342 piece 2) is done; item 6 (#281 items
1+2+6) was begun and REVERTED unfinished — §6 has what it established.

Five commits: `03cc639` (the `our` tail), `abdeedd` (#378), `d0f6236` (#377),
`cb0f871` (#376), `1484246` (#342 piece 2).  Each section below is one commit
unless it says otherwise; the asks are §7.

## §0  Status

| measurement | value |
|---|---|
| Gate `tools/prove-core` | **149 files / 5459 rows**, PASS except the 13 known pclxs xs rows |
| Full perl-tests sweep | **GATE clean, 0 new / 0 fixed, TOTAL 18513 = baseline, drops 12 = census** — run after each of the three name-resolution commits, identical every time; the 6 UNSTABLE / 10 unverified are the same three PARTIAL files as s409 |
| corpus-diff | **IDENTICAL across 111 files** after every commit; silent drops 12, unchanged |
| emission-ab | all 22 lib shims SAME after every commit |
| Companion `--all --quick --jobs 4` | run TWICE (after #378; after #377+#376): 523 files, **87 OK / 30 NOTAP / 111 XDIFF / 1 FIXTURE / 294 UNEXPLAINED** both times — every bucket identical, no verdict moved.  ONE real mover: **op/sub.t 25/6 → 52/13** |
| Cache generation | v2-154 → **v2-155**; the three artifacts regenerated, bodies byte-identical, only the stamp moves |
| Drop census | **380 → 378**: t/op/current_sub.t 4 → 3 (#378), t/op/lexsub.t 14 → 12 (#376) |
| Probes vs perl 5.40.3 | **46 shapes, every one identical** (§8) |
| Tasks | #378 #377 #376 #341 #342 closed; **#380, #381, #382 filed** |

The two companion files that differ from the snapshot are the two registered
ones: `mro/package_aliases_utf8.t` (rows-unstable) and `op/utf8cache.t` (its
known contention TIMEOUT — DIFF 2/0 when re-run alone, which is the snapshot).

# Review requests — session 410 (Opus 5, 2026-08-17)

Session H of `docs/plan-post-s408.md` §2.  Each section is one commit unless
it says otherwise; the asks that need a ruling are collected in §N at the end.

## §1  The `our` statement's tail was being lost — in BOTH pipelines

Not on the queue.  Found in the first ten minutes of #378, because #378's bar
depends on it: op/sub.t:214 is

```perl
sub { local $depth = $depth + 1;
      our $ok++, return if $depth == 2;
      ()= $parent; our $whatever;
      CORE::__SUB__->(); }->();
```

and with `__SUB__` implemented that recursion is REAL.  It did not terminate.

**v2** (`Parser2::_lower_our_decl`) lowers `our` as DECLARE-then-lower-the-tail,
and handed the tail to `_lower_expr` with the statement modifier still in it.
PExpr answered `Bug. Fell through. Missing case: []`, which is a `;; PARSE
ERROR` drop — the whole statement replaced by nil:

| shape | perl | PCL before |
|---|---|---|
| `our $z = 5 if 1;` | 5 | undef (statement dropped, announced) |
| `our ($p,$r) = (1,2) if 1;` | 1 2 | undef undef |
| `our @a = (1,2,3) if 1;` | 1 2 3 | empty |
| `our $z if $c;` | declares, runs `$c` | **hard die**, "unsupported our declaration" |

**v1** (`Parser::_process_our_declaration`) has the same job and a different
bug: it scans for an `=`, and when there is none it emits the declaration and
DISCARDS everything after the names.  That path is reached whenever a block
routes to v1 — an anon sub containing a `local` is the cheapest trigger — and
there five shapes were wrong at once, in silence:

```perl
my $v1 = sub { local $gl = 1;
  our $c = 0;  our $c++;         # perl 1   PCL 0
  our $V ||= 7;                  # perl 7   PCL undef      <- Exporter's idiom
  our $d = 3;  our $d += 4;      # perl 7   PCL 3
  our $e = 1 if 1;               # perl 1   PCL undef
  our @a = (1,2) if 1;           # perl 1 2 PCL empty       (PARSE ERROR in the init)
};
```

**The fix is the rule v2's own comment already states** — perl declares the
package cell unconditionally (a compile-time act) and runs `NAMES <tail>` as an
ordinary statement:

* v2 splits the modifier with `_split_modifier` and re-applies it with
  `_apply_modifier` — the trio the `my` path uses.  The split is accepted only
  PAST the declared names, so a word that merely looks like a modifier in
  declarator position (`our sub if() { 42 }`, op/lexsub.t) still reaches the
  shape check unchanged, and `our $c++ if $x` does not read `$x` as a second
  declared name.  A no-tail declaration with a modifier still evaluates the
  CONDITION in void, which is what the `my` path's `@declmod_eval` does.
* v1 routes the whole `NAMES <tail> MOD COND` run through
  `_process_expression_statement`, which owns all six modifiers — the same
  move the in-sub `my` branch of `_process_variable_statement` makes two
  hundred lines above.  This covers the non-`=` tails too, which is v2's
  stated rule ("the tail may be ANY operator").
* the `our` declaration emitter had been written out three times in that one
  function; it is now `_emit_our_declarations` (CLAUDE.md 11).

Nine v2 shapes and six v1 shapes probed against perl 5.40.3, all identical.
Guard rows in `Pl/t/our-local-01.t` exercise both pipelines deliberately (a
named sub for v2, an anon sub with a `local` for v1) plus the op/sub.t comma
shape.  **Task #380** holds the remainder: the four LOOP modifiers still drop
(announced), because `our $x = E while C` needs the loop lowering that v2 keeps
behind `_fallback_stmt` — and the corpus population of that shape, measured
over perl-tests + perl's own t/ + lib, is ZERO.

Emission: `corpus-diff` IDENTICAL across 111 files, silent drops 12 unchanged;
`emission-ab` over all 22 lib shims SAME.

## §2  #378 — anon `__SUB__` implemented

Shape as ruled (`fable-answers-s408.md` §2.3): at the one PPI entry that
already rewrites a NAMED sub's `__SUB__` to `(\&name)`,

```
sub { … __SUB__ … }  →  do { my $__SUB__N; $__SUB__N = sub { … $__SUB__N … }; $__SUB__N }
```

innermost enclosing sub wins, one fresh N per anon sub, per DOCUMENT so
emission stays deterministic across parses.  `CORE::__SUB__` is one Word token
and was never matched by the old predicate; both spellings are now.  No
parentheses around the `do`: `print sub {…}->()` would become `print (…)->()`,
which perl reads as a call.

**Two things worth recording, because both are traps this codebase can hit
again:**

1. **PPI overloads stringification to an element's CONTENT.**  The natural
   `$words_of{$block}` is therefore keyed by TEXT — it collides between two
   textually identical anon subs, and it goes STALE the moment a nested block
   is rewritten.  The observable symptom was the OUTER sub's own `__SUB__`
   silently keeping the runtime stub while the inner one was rewritten.  Keyed
   by `refaddr` now, with the reason in the comment.
2. **The pass had to move to LAST** in `_ppi_parse`'s repair chain.  An ANON
   sub's signature is a single `Token::Prototype` until
   `_desugar_anon_signatures` turns it into statements, so a `__SUB__` in a
   parameter default is not a Word before that pass.  A NAMED sub's signature
   is already a Structure, which is why the order never mattered.

17 shapes probed vs perl 5.40.3, all identical (§9 has the list).  The runtime
stub stays, and its message is now narrowed to what actually reaches it:
`__SUB__` in no sub at all (perl: undef) and `__SUB__` inside a string eval
(perl: the sub containing the eval — the #373 capture seam).  Both DIE rather
than guess, per rule 12's s329 boundary; `docs/not-supported.md` rewritten to
say exactly that, and the `#368` guard rows in `Pl/t/misc-fixes-02.t` flipped
from "dies" to perl's values (one `run_cl`, four claims — the file's WALL TIME
is the gate's constraint).

`perl-tests/sub.t` carried `ok(1, 'SKIP: CORE::__SUB__ … not implemented')`
where the `[perl #122845]` test belongs.  Restored; it passes.  sub.t stays
62/2 (the SKIP row was a pass too), so no baseline row moves.
## §3  #377 — a lexical sub reading its enclosing sub's PARAMETER

Two causes, and the second one was not in the task.

**(1) The crash the task diagnosed.**  The promotion pass renames the captured
`my $x` to `$x__file__0` in both subs, but the declaration was lowered by the
raw-params optimisation — `(p-raw-params ($x__file__0) …)` binds the promoted
name LEXICALLY inside `pl-outer` and emits no cell, so the hoisted nested sub
read a free special that was never declared.  Fix shape (i) as ruled: a
promoted name may take NEITHER param binding in `_lower_sub_inner` (the other
one binds a `let`), so the declaration goes back into the body, where
`_lower_block`'s `_file_lex_renamed` branch already emits the cell and the
plain assignment.  The discriminating measurement was in the tree already —
the same sub with one more use of `@_`, which defeats the shift coalescing on
its own, ran and matched perl.

**(2) `my ($x) = @_` — the task said "stays the refusal or joins (i), probe
first".  It joins, and its cause is one line.**  The probe: perl gives 6 8;
PCL killed the whole FILE with `Parser2 TODO: lexical 'x' possibly captured by
nested sub`.  `_scan_lex_facts` only recorded a LIST declaration as a
promotion candidate when it declared TWO OR MORE names — so a one-element
`my ($x) = @_` was invisible to the promoter: not `_single_scalar_decl`'s
shape (it is a list assignment), not the container branch's (that wants a
`[@%]` sigil), and one short of the list branch's bound.  `my ($x, $y) = @_`
two lines away promoted fine.  The N=1 case is the N=k case (#267's rule); the
bound is gone and the single-CONTAINER spelling `my (@a) = …` stays where it
was.

Seven shapes probed vs perl, all identical.  The PLAIN named-sub twin now RUNS
(6 8 where perl says 6 6): the per-call member of the registered "will not
stay shared" family, added to that entry with the note that it used to be a
CRASH and that the `my sub` spelling — how post-5.26 code writes a private
helper — agrees with perl exactly.

## §4  #376 — the lexical-sub rename's three uncovered spellings

Three edits, each one the shape the task ruled, all three probed against perl
5.40.3 before and after:

1. a bodiless `my sub NAME;` OPENS a region (the `!$st->block` skip is gone).
   perlsub's own idiom for mutually recursive lexical subs; leaving it alone
   left both halves package subs, so two scopes clobbered each other — #337's
   core bug in the spelling it skipped.
2. `_lexsub_renamable`: a Word after `sub` heading a PACKAGE sub statement
   inside a covering region is RENAMED — it is the lexical's definition, as
   perl reads it (`my sub f {"L"} { package O; sub f {"M"} } f()` → `M`, and
   `O->can("f")` is FALSE).  `package NAME` / `require NAME` still return 0,
   and a `my`/`state sub` statement is covered by its own declaration.
3. a use (or a definition) under a different `package NAME;` spells the
   QUALIFIED renamed name.  A lexical sub is scoped to the file, not the
   package, but PCL resolves a bare name in the package in effect at the
   token — hence `the function Other2::pl-helper__lexsub__5 is undefined`.
   One helper, `_lexsub_spelling`, on the resolver the variable-rename family
   already uses (`_pkg_in_effect_at`) — no second package walk (rule 11).  It
   is applied at BOTH rename sites, the token stream and the interpolated-code
   map, so `"@{[ helper() ]}"` under another package is right too.

Edit 3 turned out to be needed for edit 2 as well: with 1+2 alone, the
`{ package O; sub f {"M"} }` definition landed in O as `O::f__lexsub__N` while
the call in main looked for `main::f__lexsub__N` — the same bug wearing the
other hat.

Must-NOT-fire list, all probed identical to perl: a package sub of the same
name outside every region; a bodiless PACKAGE `sub k;` inside a region
(perl keeps the lexical body — so does PCL); `package NameLike;`; a method
call spelled like the lexical; a hash key and a fat-comma key spelled like it;
an interpolated call; `\&NAME` and `&NAME`; a nested same-name `my sub`
(inner wins in its own region, outer after it).

## §5  #341 measured — and what it found (task #381 filed)

`t/op/lexsub.t` alone, after #376: perl 156/0, PCL **7/10 and then the file
DIES at row 18**.  Drops 14 → 12 (#376 recovered two).  So the ~150 rows
#341 was sized against are behind ONE statement, and it is not a lexsub bug:

    op/lexsub.t:91   is((h F), 4242, 'our sub symbol translation does not
                                      affect meth names')

`h F` is the INDIRECT-OBJECT method call — perl reads it as `F->h`.  PCL
compiles it to `(pl-h (pl-F))`, i.e. a call to a function `F` that does not
exist, and the file aborts.  **perl's own rule is not a guess, and PCL already
has the discriminator**: a bareword after a callable word is an indirect
object only when it is not itself a declared sub.  Probed both ways —

    sub F { 7 } sub F::h {4242} sub h {4343}; print h F;
      perl 4343   PCL 4343     <- AGREE (a declared sub wins, in both)
    (no `sub F`)  sub F::h {4242} our sub h {4343}; print h F;
      perl 4242   PCL crash    <- the divergence

— which is exactly #266's callable classifier.  `docs/option-b-phase2-plan.md`
lists "indirect object" under Track A (#371) as a feature absence to turn into
a ruled refusal; **that is not enough for this file**, because a refusal dies
too and the file still aborts at row 18.  Recovering the 139 rows needs either
the LOWERING (`WORD BAREWORD ARGS?` → `BAREWORD->WORD(ARGS)`) or, as an
interim, a counted DROP — which the s409 census ruling explicitly allows,
since it converts a crash-form into a counted drop.  Task **#381** carries the
measurement, both probes, the must-not-fire list (`new Foo(1,2)`,
`print STDERR "x"`), and the sequencing note (with #372/#343, not before).

#373 stays a filler: the plan said "only if #341's read shows rows behind it",
and the read says the rows are behind #381, not behind the string-eval seam.
## §6  #342 piece 2, and where #281 stopped

### 6.1  #342 piece 2 — the heredoc inside `${\ … }` inside `s///e`

The replacement text PCL is handed already CONTAINS the body, because it sits
inside the `s|…|…|e` delimiters.  PPI lexes `"${\<<END}"` as ONE Quote::Double
token, never sees the opener, and leaves the body and terminator as loose code
the expression parser reads as `ok $test - heredoc END` and refuses.

Fixed by a heredoc pre-pass on the replacement text, run ONLY on a replacement
that already failed to compile — so every shape that compiles today keeps its
emission byte for byte, and the retry cannot make a working program worse.
The body is HOISTED into a `my` variable and the opener replaced by that
variable, NOT spliced in as a literal where it stood: the opener is inside the
`"…"`, so a literal there ends the string at its own first quote (measured —
the emission came out `"${\"ok …"}"` and the CL reader choked).

**A regression this almost shipped, and corpus-diff is the only thing that
caught it.**  Splitting `_compile_subst_e_expr` into a try-half plus a retry, I
dropped the `my $result = 'nil'` initialisation — and `nil` is the ANSWER, not
a failure, when a replacement parses to no statements at all.  `s/o//eg` in
perl-tests/closure.t reaches that code, so closure.t stopped transpiling
entirely (1494 lines → 0).  The gate would not have caught it; corpus-diff
reported `1 of 111 files differ` and `SILENT DROPS 12 -> 11`, and the file was
closure.t.  Restored, with the reason in the comment.

The other four spellings in t/base/lex.t put the body AFTER the statement, so
it lives in the enclosing DOCUMENT — **task #382**, a document-level pre-pass.
All four are inside `eval '…'` and fail into `$@` (loud, trappable).  base/lex.t
still does not transpile, on `s/${s|||;\""}not //;` two lines later, which is
why #342's own note said not to size its 120 rows against piece 2 alone.

### 6.2  Where #281 items 1+2+6 stopped, and one correction to item 2

Begun and REVERTED unfinished rather than left half-applied.  What it
established, so the next session does not re-derive it:

* **The macros** (item 1): four, not three — `p-list-ctx` / `p-scalar-ctx` /
  `p-void-ctx` / `p-caller-ctx`, each expanding to exactly the `let` it
  replaces, exported from `:pcl`.  `:void` is as common as the other three in
  the emitters and the review's list omitted it.
* **The sites**: **31 single-binding string sites** (Parser.pm 20,
  ExprToCL.pm 10, Environment.pm 1 — that last one is POD) are covered by a
  mechanical PREFIX rewrite, because the binding list's parens are
  self-contained: replacing `(let ((*wantarray* t)) ` with `(p-list-ctx `
  leaves the trailing paren count unchanged.  That includes the two
  OPEN-then-close-later pairs in Parser.pm (5400/5457, 7580/7621), which keep
  working for the same reason.  **Two multi-binding lets must stay plain
  lets** — `(*package* |sort--pkg|)` in the sort seam and
  `(*p-in-list-assign-rhs* t)` in the list-assign RHS; the prefix pattern does
  not match them, since it requires `))` right after the value.
* **Six form-shaped sites** in Parser2.pm / ExprToCL2.pm need a small
  `_ctx_macro($bind)` dispatch instead (ExprToCL2.pm:134 computes the bind at
  run time).
* **The verification is the open question** — see ask 7.7.

**A CORRECTION to item 2.**  `docs/generated-cl-ir-review.md` reads "sort.t
emits `(defvar $a …)`/`(defvar $b …)` TEN times each".  It does — but those ten
are in **ten different `in-package` sections**, so they are ten DIFFERENT
symbols (`main::$a`, `Foo::$a`, `Bar::$a`, `Oscalar::$a`, …).  Deduping on the
line text would delete nine real declarations.

Measured properly (same symbol, resolved through the section's in-package),
sort.t has **11 true duplicates out of 35 defvars**, and they are a different
shape: a BARE defvar in a section plus the QUALIFIED spelling of the same
symbol, e.g. `(defvar $a …)` inside `(in-package :Foo)` at line 1207 and
`(defvar Foo::$a …)` at line 1213.  closure.t, hash.t and List::Util have ZERO.
So item 2 is still worth doing and still free, but the dedupe key is
`(section package, symbol)`, not the line — and the payoff is 11 lines in one
file, not 20.

## §7  The asks

**7.1 — Fixing v1's copy of the `our` mechanism.**  §1's second half is a fix
inside `Pl/Parser.pm`, i.e. inside the compiler #379's fold is meant to
delete.  I did it because that copy is the ACTIVE path for every block that
routes to v1 (an anon sub containing a `local` is enough), and what it was
doing was silently dropping `our $V ||= 7`.  Leaving it until the fold would
have left a silent-wrong in place for an unknown number of sessions.  Is that
the right call, or is there a standing rule that v1 gets bug fixes only when
the shape is unreachable through v2?  (If the answer is "fix it", the same
logic will keep applying — v1 owns embedded blocks until E5.4.)

**7.2 — `__SUB__` outside any sub: die, or answer `undef`?**  perl gives
`undef`; PCL dies.  I could rewrite a no-owner `__SUB__` to `undef` at parse
time and match perl exactly — but the SAME "no owner" state is what an
`__SUB__` inside a string EVAL looks like, where the right answer is the sub
containing the eval and `undef` would be a silent wrong value.  So I kept the
die for both and narrowed its message to name them.  Distinguishing them costs
one flag (`$self->eval_mode` into the pass).  Worth it, or is the die right for
a shape real code does not write?

**7.3 — #376 edit 2's blast radius.**  A file-level `my sub NAME` now makes
EVERY later `sub NAME` in the file the lexical, and `main::NAME` never comes
into existence.  That is what perl does (probed, both same-package and
cross-package), but it is a big rule: a module that declared `my sub helper`
and later defined `sub helper` intending an exported package sub would lose
the export — silently, and identically to perl.  Confirming there is no guard
to add: PCL should follow perl here even where perl surprises.

**7.4 — #377's plain-sub twin: crash → registered divergence.**
`sub outer { my $x = shift; sub inner { $x*2 } … }` used to CRASH and now runs,
giving `6 8` where perl gives `6 6` (perl's nested named sub keeps the first
instance).  It joins the "will not stay shared" entry as its per-CALL member.
That follows s405's ruling ("registered rather than refused: the refusal took
the WHOLE FILE with it") — flagging it because a divergence is a divergence,
and s405's decision was about a different shape.

**7.5 — #381's sequencing.**  The indirect-object call holds 139 rows of
op/lexsub.t.  The plan puts "indirect object" in Track A (#371) as a refusal,
which does not recover them.  Should #381's LOWERING move up (it is small once
the callable classifier can say "this bareword is not a sub"), or does it wait
for B2/#343 as the task currently says?  It is also the `new Foo(…)` grammar,
so it is not a one-file item.

**7.6 — a commit-split question of practice.**  §1 and §2 are two commits, but
the generation bump and the three artifacts ride in the SECOND one; §1 alone is
emission-changing without a bump.  The bodies are byte-identical either way
(only the stamp moves), and both commits land together, so nothing stale can
be served.  Is deferring the bump to the session's last emission-changing
commit acceptable, or must every such commit carry its own bump + three
regenerations?

**7.7 — how should #281 item 1 be VERIFIED?**  It changes emission in every
file (a context bind every 6th–14th line), so "corpus-diff explained per file"
becomes "every hunk is one of the four shapes", which is not something to
eyeball across 111 files.  Two options, and I would like the ruling before
spending the session:

  (a) a NORMALIZER: a scanner that rewrites `(p-list-ctx X)` back to
      `(let ((*wantarray* t)) X)` (paren-matching, so it can re-insert the
      level) and then compares against the pre-change emission byte for byte.
      Rigorous and reusable for the rest of the macro vocabulary; ~40 lines.
  (b) the behavioural gate alone — the macro expands to exactly the old form,
      so gate + full sweep + companion is a very strong check, and corpus-diff
      is used only to confirm that the CHANGED files are all of them.

I lean (a), because #281 will introduce more macros and the normalizer pays
for itself; but it is a tool in `tools/` that nothing else needs, so it is
your call whether that is worth the surface.

**7.8 — item 2's dedupe, now that its premise is corrected** (§6.2): 11 lines
in one corpus file, keyed on `(section package, symbol)`.  Still worth a
commit, or fold it into whichever session does item 1?

## §8  Every probe taken this session, against perl 5.40.3

All run as the same source through `perl` and through `./runpcl`, output
compared.  "=" means identical.

**`our` tails, v2 route (a named sub / file level)** — 9 shapes, all `=`:
`our $m++, return "R" if $x == 2` (and its false branch); `our $z = 5 if 1`;
`our $y = 5 if 0`; `our $q = 7 unless 0`; `our $w if bump()` (the condition's
side effect runs, the declaration happens, no value); `our ($p,$r) = (1,2) if
1`; `our @arr = (1,2,3) if 1`; `our $v ||= 9`; `our $cnt2 = ++$cnt if 1`.

**`our` tails, v1 route (an anon sub containing a `local`)** — 6 shapes, all
`=`: `our $c++`; `our $V ||= 7`; `our $d += 4`; `our $e = 1 if 1`;
`our @a = (1,2) if 1`; and the whole `[perl #122845]` recursion, which needs
`our $ok++, return if $depth == 2` to terminate.

**anon `__SUB__`** — 17 shapes, all `=`: factorial via `__SUB__->()`;
`__SUB__ == $f`; fib through two levels of anon nesting; `sub {…}->(3)`;
`CORE::__SUB__`; the statement-start `sub {…}->()` with `local` and `our` in
it; nested anon subs each with their own; a hash value; a `return`ed anon sub;
a `map` block; a `grep` block; two TEXTUALLY IDENTICAL anon subs (the refaddr
regression); `print sub {…}->()` (the paren trap); an anon sub inside a
`my sub`; an anon SIGNATURE default; a named sub's `__SUB__` (unchanged); a
named sub reached through `\&name` after redefinition (unchanged).

**#377** — 7 shapes, all `=`: `my $x = shift` + `my sub`; `my ($x) = @_` +
`my sub`; `my ($x,$y) = @_` + `my sub`; `my $x = 70` + `my sub`; a `my sub`
that does NOT read the param; a file-level `my ($fx) = (5)` captured by a
named sub; `my (@a)` / `my (%h)` / `my ($p, @q)` captured by named subs.  Plus
an EMISSION assertion: an uncaptured shift-param still takes the
`p-raw-params` fast path and is not promoted.

**#376** — 5 fixed shapes and 9 must-not-fire shapes, all `=`: the
forward-declaration idiom in two scopes; perlsub's mutual recursion through
two forward declarations; a package `sub NAME` in the region (both same- and
cross-package); a cross-package use; multi-segment packages
(`package Foo::Bar; my sub deep …` used from `Baz::Qux`); an interpolated call
across packages; `\&NAME` across packages; a lexical declared inside a package
region; a package sub of the same name outside every region; a bodiless
package `sub k;` inside a region; `package NameLike;`; a method call spelled
like the lexical; a hash key and a fat-comma key spelled like it; nested
same-name `my sub`s (inner wins in its region, outer after it).

**#381** — 2 shapes: `h F` with and without a `sub F` in scope.  PCL AGREES
with perl in the `sub F` direction (both call `h(F())`), which is what makes
perl's rule implementable here.
## §9  What is left of the plan's session H and I

Session H is complete.  Session I: item 5 (#342 piece 2) done; **item 6 (#281
items 1+2+6) is where the next session starts** — §6.2 has the inventory, and
asks 7.7/7.8 are the two things that would change how it is done.

Filed this session and NOT scheduled: **#380** (`our $x = E while C` — the four
loop modifiers, corpus population zero), **#381** (the indirect-object call,
139 rows of op/lexsub.t, sequencing question in ask 7.5), **#382** (the
document-level heredoc pre-pass, t/base/lex.t only).
