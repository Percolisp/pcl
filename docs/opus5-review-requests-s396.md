# Review request — s396 (Opus 5), for Fable

Five commits.  The queue said "#325 refaliasing, then #314's residue", and both
halves are done — but the session's second product is a set of MEASUREMENTS
that retire most of what was left of #314.

**Companion-suite rows recovered: +149** (op/const-optree.t 0 → 86,
op/for-many.t 0 → 63) plus **+4 in `perl-tests/aassign.t`**.  Gate **141 files /
5200 tests** (failures exactly the pclxs xs rows the user has said to ignore).
Full sweep **GATE clean**, TOTAL passing 18539 (baselines re-blessed, +0
against each other).  corpus-diff vs the session start: **one file, one line**.
Cache generation v2-145 → v2-147.

| file | before | after |
|---|---|---|
| op/const-optree.t | TRANSPILE-FAIL (0 of 148) | 86 ok / 62 |
| op/for-many.t | TRANSPILE-FAIL (0 of 71) | 63 ok / 8 |
| op/aassign.t | 169 ok / 20 | 173 ok / 16 |
| op/substr.t | 376 ok / 23, CRASHED before its last test | 376 ok / 24, all 400 judged |
| re/opt.t | TRANSPILE-FAIL | XDIFF (needs `re::optimization`) |

---

## 1. `s396a` — a `\`-cast LVALUE is an ALIAS (#325)

Six spellings probed against perl 5.40.3 **before** any code.  **Four were
SILENT WRONG** — `\$x = \$y`, `\my $x`, `\my @b`, `\my %g` all wrote into a
throwaway ref box and changed nothing — and two were hard refusals.

The fix is ONE arm in `p-setf`'s PLACE dispatch, because every spelling the
emitter produces converges there already: `\$x = …` and `\my @b = …` as a plain
place, `(\$x) = @_` through `p-list-=`'s default arm (which routes an
unrecognised element form to `p-setf`), `\$h{k} = …` as a `(p-gethash-box …)`
place, `\&c = \&d` as `(p-backslash-sub …)`.  The arm REBINDS THE NAME'S
STORAGE — `(setq $x <$y's box>)`, the vector/hash-table object for `@`/`%`, the
referent box written into the slot for an element — so `\$x == \$y` follows for
free, a ref's identity being its referent's.

Two things the probes found that the design did not predict:

- **`\$x = \$y` and `\$x = $r` differ by ONE BOX LAYER, and `is-ref` is what
  tells them apart.**  `p-backslash` sets the flag on the wrapper it makes; a
  variable HOLDING that wrapper does not have it.  Reading the flag keeps a
  reference-to-a-reference right, where "peel twice" takes one level too many.
- **`\(EXPR)` collapses to `\EXPR` as an rvalue but NOT as an lvalue.**  One ref
  either way for a value, but the parens are what make `\($x) = @_` a LIST
  assignment (perl aliases $x to $_[0]), while `\$x = @_` is scalar context and
  dies.  Only the one-element case needed saying.

## 2. `s396b` — `for \my %e (@list)` (#327)

PPI cannot lex a `for` whose loop variable is a `\`-cast or a non-scalar: the
compound statement keeps only the word `for`, and the rest of the construct and
every following statement land in one flat sibling.  No tree edit can repair
that, so this is the #270 pattern — repair the RAW TOKEN STREAM, then reparse —
and the rewrite is a pure re-spelling into the mechanism §1 just shipped:

    for \my %e (@list) { BODY }
      ⇒  for my $__PCL_RA0 (@list) { \my %e = $__PCL_RA0; BODY }

No new foreach macro, no VarAnnotator work, and `%e` is scoped to the body and
fresh per iteration — perl's scoping exactly.  **The one semantic question was
probed rather than assumed: perl does NOT restore an aliased PACKAGE foreach
variable** (`our $s = "orig"; for \$::s (\"a",\"b") {} print $s` prints **b**),
so no save/restore is needed and the naive rewrite IS perl.

## 3. `s396c` — `for my ($q,$r) (LIST)` (#329), the n-at-a-time foreach

Same mis-lex, same cure.  Re-spelled as a `while` over `map \$_, LIST`:

    for my ($q, $r) (@a) { BODY } continue { CONT }
      ⇒  my @L = map \$_, (@a);
         my @PD; push @L, \$PD[scalar @PD] while @L % 2;
         my $I = 0;
         while ($I < @L) { \my $q = $L[$I]; \my $r = $L[$I+1]; BODY }
         continue { CONT; $I += 2 }

Three decisions, each settled by a probe:

- **`map \$_, LIST`, not `\(LIST)`.**  map ALIASES `$_` to each element, so
  `\$_` is a write-through ref to the original — probed for arrays, several
  arrays, a hash, literals and `reverse`, all six identical to perl.  `\(LIST)`
  distributes over the list's **TERMS**, so **`\(@Q, @A)` is two ARRAY refs**
  (perl's own answer); the first version of this rewrite used it and silently
  ran two iterations instead of six.
- **`while` + `continue`, not a C-style `for`** — perl allows a continue block
  on this loop (op/for-many.t uses one with redo and next) and a C-style for
  cannot carry one.  With the STEP in the continue block the three non-local
  exits land where perl puts them: `next` runs the continue block and the step,
  `redo` runs neither, `last` leaves.
- **the PAD array gives the short final chunk its own writable slot per missing
  variable**; one shared pad would make two loop variables the same variable.

Of op/for-many.t's remaining 8: two are "invalid perl must be rejected" rows
(principle 9) and six are ONE missing fact — PCL has read-only ARRAYS but no
read-only SCALARS (**task #330**, with the `box-set` hot-path cost stated).

## 4. What the measurements retired

**Half of #314's residue is not reachable, measured not guessed:**

| file | rows | what it actually needs |
|---|---|---|
| op/coresubs.t | 1109 | `use B` + `B::walkoptree`.  Waiving its state blocker experimentally makes it transpile and produce **ZERO** rows |
| re/opt.t | 639 | `re::optimization` — perl's readout of its OWN regex optimizer |
| op/svleak.t | 156 | `XS::APItest::sv_count` (PL_sv_count) |
| io/shm.t | 21 | IPC::SysV |
| op/const-optree.t | 62 of the remainder | `B::` optree inspection |

**A t/ file that measures perl's INTERNALS is not a PCL row count** — check
what a file's assertions read before sizing a family from its plan.

**F-E (comp/our.t, 7 rows) was ATTEMPTED AND REVERTED → task #328.**  Narrowing
the `our shadows a my-lexical` refusal to the exception partition makes the file
7/7 — and is a SILENT WRONG: a use after `our $y` keeps reading the `my $y` it
shadows (eight shapes probed, five diverge).  The obvious fix does not work
either: **`main::$z` and `$z` are THE SAME CL SYMBOL**, so qualifying cannot
escape a lexical binding; it needs a global-read FORM, or the my-shadow rename.
The refusal stands.

## 5. Two bugs both gates shipped green

Recorded because the *method* is the finding, not just the fixes.

- **`s396d`** — the sweep reported 4 FIXED rows in aassign.t while the file
  emitted 3 FEWER.  Two of the "fixed" ones had **stopped existing**:
  `\($a[0], $a[1]) = \(…)` reaches p-setf as the PLAIN accessor `(p-aref …)`
  where the single-element spelling arrives as `(p-aref-box …)`, so §1's arm hit
  rule 12's die at MACROEXPANSION and took the whole enclosing form — and its
  two `is` calls — out of the load.  **`sweep-diff`'s FIXED bucket counts a row
  that VANISHED**, and LOST cannot catch it (LOST reads the PASS baseline; the
  row was never passing).
- **`s396e`** — the `\(%h)` fix went into `_child_is_list_expr`, which
  `gen_tree_val_form` also consults, so `(%h)` stopped emitting `(vector %h)`
  and `(%h) = LIST` lowered as `p-hash-=`.  op/inc.t's chained
  `my (%orig) = my (%inc) = …` lost 8 rows.  Found by diffing the COMPANION
  SUITE against its snapshot — neither gate saw it.

## 6. Housekeeping

- `docs/fail-baseline.tsv` 684 → 680 (the four aassign.t rows out by EDIT);
  `docs/pass-baseline.tsv` re-blessed gate-green after a per-file audit
  (`# taken-at: ebabc69`); the two now read **+0**.
- Five rows spliced into `docs/perl-suite-run.tsv`, with the reason for each and
  an explicit note on what was NOT spliced (the TIMEOUT-shaped files).
- **`comp/require.t` gets a 450 s row in `docs/perl-suite-timeouts.tsv`** —
  #326's "do this regardless" half.  With it the file returns its snapshot
  909/835 exactly; at the 90 s default it reads 294–351 and its 909 passing rows
  evaporate invisibly.
- **The orphaned `pl2cl --server` reaper** (see ASK 2).

---

## ASK 1 — is `re/opt.t`-shaped XDIFF the right verdict, or should these files leave the population?

Five files (~2000 rows) turned out to measure **perl's own internals**:
`B::walkoptree` optrees, `PL_sv_count`, `re::optimization`'s minlen/anchored/
floating/stclass.  These are not regex or op SEMANTICS — they are a readout of
one particular engine's optimizer state, and PCL's optimizer is a different
program.

I registered re/opt.t XDIFF with a not-supported entry, which keeps it running
and row-checked.  But the suite's headline "298 UNEXPLAINED" now contains
several files whose *entire* content is unreachable-by-construction, and they
also drag the C_ok TOTAL down as if rows were owed.

Question: should there be a distinct verdict for "this file measures the
implementation, not the language" — so the population is honest about what is
actually owed?  It is the same question `docs/perl-suite-expected.tsv` answers
for a feature gap, but the cause is categorically different: a feature gap can
be closed, this cannot.

## ASK 2 — the orphan reaper's placement

The user caught a **5 GB `pl2cl --server` that had been running 30 minutes**
(a second one at 4.6 GB alongside it).  Cause: an SBCL child that used string
eval spawns the server; when `timeout -k` SIGKILLs that SBCL the server is
reparented to init, and because its loop only checks stdin BETWEEN requests, one
caught mid-transpile (op/cond.t's 20k-nested ternary — the documented quadratic
pathological-nesting case) never notices EOF.

I put a reaper in BOTH runners, called per file, keyed on `PPID == 1` so a
concurrent run in another shell is never touched.  Two things I did not do, and
would like a ruling on:

1. **The real fix is arguably in the server**: make `pl2cl --server` check its
   parent (or install a SIGPIPE/idle timeout) so it dies on its own rather than
   relying on every caller to reap it.  Cheap, but it puts a liveness policy in
   the transpiler.
2. **Or in the runtime**: have the SBCL side kill its transpiler process from
   the exit hook.  That fixes clean exits but NOT the SIGKILL case, which is the
   one that actually happened — so it would be belt without braces.

Filed against **#273** (this is its MEMORY half; #273 itself is about the same
orphan swallowing a sweep's VERDICT).  Worth doing properly, or is the reaper
enough?

## FYI — a pre-existing bug the probes turned up, recorded not fixed

`for my $r ({5,6},{7,8})` reads the FIRST brace group as a LIST, not a hash
constructor: perl prints `HASH HASH`, PCL prints two empties and one `HASH`.
Same cause as **#286** (perl's `intuit_curly`), different position — recorded
there as a second population, and it now has a cause line (op/lvref.t line 359
needs it, once #314 family F-F unblocks that file).
