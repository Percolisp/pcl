# The TAP layer audited for unfalsifiable assertions (task #202, s330)

Ruled in `docs/fable-answers-s328.md` §3 and run before #204/#189.

**The question:** which assertion in `cl/pcl-test.lisp` has *no reachable
failure path* — i.e. reports `ok` for a claim it never checked?  This is
#152's bug ("a closed set of outcomes where one outcome is unreachable")
located in the instrument instead of the runtime, which is worse: a wrong
runtime answer shows up as a failing row, while a wrong HARNESS answer shows
up as a green board.

**Method:** one inverse probe per TAP-emitting function — drive it into the
state where it must say `not ok`, run the same file under real perl, and
compare row for row.  Probes live in the session scratchpad; the ones worth
keeping are `Pl/t/tap-assert-01.t` (16 rows).

**Accept criterion (Fable's):** every TAP-emitting function either (a) has a
reachable failure path proven by an inverse probe, or (b) says so loudly when
its claim cannot be checked.

---

## The policy question this raised, and the answer taken

Rule 12 (as sharpened in `fable-answers-s328.md` §1) says a missing case that
should have produced a VALUE the program consumes must DIE.  An assertion's
"value" is its verdict, which the sweep then publishes — so the literal
reading says *die inside the harness*.

**Decision taken here: an assertion whose claim cannot be evaluated emits
`not ok` with a diagnostic naming the reason, not a die.**  Reasons:

1. It cannot be mistaken for a pass — the disease #202 exists to cure.
2. It names itself in the row's own diagnostic, so it is not silent; the
   sin §1 identifies is the silence, not the fall-through.
3. A die in the harness converts ONE unverifiable row into an aborted file,
   losing every row after it — the measured 88-row lesson from s328's
   `goto` experiment, applied to the instrument that does the measuring.
4. It is what Test::More itself does: an operator its string eval cannot
   compile fails the test and diags the error.

The one exception is `plan()`, which does die: the plan is the count the
WHOLE file is judged against, so an unrecognized plan form leaves nothing
checkable and there is no row to attach a diagnostic to.

---

## Inventory: every TAP-emitting entry point

| function | failure path | verdict |
|---|---|---|
| `ok` / `pass` / `fail` | direct | reachable (pass/fail are *defined* as one-sided) |
| `is` / `isnt` | value compare incl. undef pairs | reachable |
| `like` / `unlike` | match / no-match | **was broken** — see F1 |
| `cmp_ok` | operator dispatch | **was broken** — see F3 |
| `is_deeply` | structural compare | reachable (diag improved, F8) |
| `use_ok` / `require_ok` | load error | reachable since s328; description fixed (F4) |
| `isa_ok` | `p-isa` | reachable; descriptions were one key for four claims (F5) |
| `can_ok` | `p-can` per method | reachable; naming + degenerate calls fixed (F6) |
| `skip` / `skip_all` | one-sided by definition | correct |
| `skip_without_dynamic_extension` | **never checked its claim** (F7) | fixed |
| `plan` / `done_testing` | count claim | unrecognized form was silent (F9) |
| `eq_array` (feeds `ok`) | length + element compare | reachable |
| `eq_hash` (feeds `ok`) | **type-errored on every call** (F2) | fixed |

## Findings

**F1 — `unlike` could not fail.**  Its scanner call ended in `(error () t)`:
any pattern cl-ppcre refused to compile became a **PASS**.  `unlike($s, "(")`
was an assertion with no reachable failure path in the most literal sense.
`like`'s twin arm was `(error () nil)` — safe by luck, not by design.
Fixed: `like` and `unlike` are now one function (`%test-like`) over one
matcher (`%test-regex-match-p`), and a scanner error is a `not ok` naming the
error in both directions.

**F2 — `eq_hash` had never worked.**  `(p-box-value (unbox ref))` unwraps
twice, so every real hashref type-errored (`The value #<hash-table …> is not
of type P-BOX`) and killed the file.  The inverse probe was the first code
ever to call it.  Fixed to the same single unwrap `eq_array` uses.

**F3 — `cmp_ok` manufactured verdicts for four ordinary operators.**  The
dispatch knew twelve operators and answered everything else with a comment
plus a **FAIL**, so `cmp_ok(1,'<=>',2)` (true in perl) and
`cmp_ok($s,'=~',qr/x/)` were published as failures.  `<=>`, `cmp`, `=~`, `!~`
implemented; anything still unhandled reports `not ok` naming the operator.
(Corpus check first: perl-tests uses only `== != < > <= >= eq`, and the ten
CPAN board dists only `< > eq gt` — so no live row moved.)

**F4 — `use_ok`'s description carried the import list.**  Test::More prints
`use Foo;` for every form; PCL printed `use List::Util first;`.  Descriptions
are join keys (skip-registry, `tools/sweep-diff.pl`), so a prettier text is a
different row.  Ruled in `fable-answers-s328.md` §3.

**F5 — `isa_ok` gave four different claims the same description.**  Object,
plain reference, class-like string and undef all printed `The object isa X`.
Now Test::More's wording, which names what the thing is (`An object of class
'Foo' isa 'Bar'` / `A reference of type 'ARRAY' …` / `The class (or
class-like) 'X' …` / `undef isa 'Bar'`), with matching diagnostics.  No
baseline row keys on the old text (checked: zero hits in
`docs/fail-baseline.tsv`).

**F6 — `can_ok`'s degenerate calls said nothing useful.**  `can_ok('Foo')`
printed `->can(...)` (no class — `p-ref` of a string is `""`) and the diag
`method(s) not found:` with an empty list.  Now `ref $proto || $proto` for the
class, `Foo->can('m')` when there is exactly one method, and Test::More's two
diagnostics for the empty-invocant and no-methods calls.

**F7 — `skip_without_dynamic_extension` skipped unconditionally.**  Its skip
reason claimed the extension "is not available" without ever asking.  Perl's
t/test.pl asks `%Config`; PCL now asks the loader, which is our `%Config`.
Live effect: `Devel::Peek` (undef.t) really is missing and still skips; `IO`
(readline.t) resolves to `lib/IO.pm`, so those four rows now RUN — see the
sweep note below.

**F8 — TAP diagnostics leaked SBCL internals.**  `is_deeply(sub{}, sub{})`
printed `#<function (lambda (&rest %_args) :in "/tmp/…") {B800D1356B}>`.
`test-display-value`'s catch-all now stringifies the perl way (`CODE(0x…)`).

**F9 — `plan()` fell through unrecognized forms in silence**, printing no
`1..N` line at all, so the file's count claim could not be checked by anyone.
Now dies naming the form.  (Corpus check: every call in `perl-tests/` is
`plan N`, `plan tests => N`, `plan 'no_plan'` or `plan skip_all => …`.)

**F10 (runtime, found by the audit's own probes) — `scalar()` dereferenced.**
`p-scalar` unboxed first, so a box holding a vector answered with the ELEMENT
COUNT (`ref(scalar($aref))` was `""`, `scalar([1,2])` was `2`) and a
ref-wrapper answered with its referent (`ref(scalar(\5))` was `""`).  An array
VARIABLE is a raw adjustable vector and is never boxed, so the distinction was
always available — the hash branch below it already carried exactly the
`(not (p-box-p val))` guard the array branch lacked, the "second copy of a
mechanism, one of them incomplete" shape again.  This is not a harness bug at
all: `scalar($ref)` is wrong everywhere in the language.  It surfaced here
because `isa_ok([1,2], 'Bar')` lowers its argument through `p-scalar`, so the
harness was reporting on a number that used to be a reference.

## Not changed, deliberately

- **A plain STRING as a `like`/`unlike` pattern is treated as a pattern.**
  Test::More FAILS it ("doesn't look much like a regex to me"); perl's
  t/test.pl interpolates it (`$got =~ /$expected/`).  One entry point serves
  both callers, and test.pl's reading is the one `perl-tests/` needs.
  Divergence recorded here rather than fixed.
- **`pl-which_perl` returns a hard-coded perlbrew path and `pl-run_perl`
  always returns undef.**  Both are stubs whose claims are unverified; neither
  is an assertion, and making them real means running a child interpreter.
  Filed as task #207.
- **`UNIVERSAL::isa` does not honour the reftype rule.**  Perl's
  `isa_ok([], 'ARRAY')` and `isa_ok(bless([],'Foo'), 'ARRAY')` both pass —
  `isa` is true when the class name equals the referent's reftype.  PCL's
  `p-isa` only walks `@ISA`, so both fail.  A runtime semantic, in #163's
  family; filed as task #206.

## Measured

- **Pl/t gate**: PASS **128 files / 4532** (was 127 / 4516; +1 file, +16 rows).
- **Full sweep**: TOTAL **18469 passing / 918 failing** across 108 files (was
  18462 / 925) — **+7 passing**, all from F10; fully-passing files **66**,
  unchanged.  `sweep-diff` **0 new / 0 fixed** after the baseline edit below
  (the two UNSTABLE rows and the one unverified row are the crash-file noise
  `docs/fail-baseline.tsv` already carries for postfixderef.t and ref.t).
- **Baseline edits** (by hand, never by re-blessing a run): the 7 rows F10
  fixed were removed; ONE row added — `array.t` t128 "undef preserves
  identity in array [perl #109726]".  That row used to PASS *because* of the
  F10 bug: both `\$_[0]` and `\undef` were flattened to undef by `scalar()`,
  so `is` compared undef with undef.  With refs surviving, PCL answers
  `REF(0x1)` vs `REF(0x2)` where perl answers the same address twice —
  perl shares one immortal `PL_sv_undef` and PCL does not (and prints `REF`
  where perl prints `SCALAR`, the #163 half).  An honest failure replacing a
  fake pass, which is what this task is for.  689 → 683 blessed fails.
- **CPAN**: the four-dist board and File-Which are **byte-identical between a
  HEAD worktree and this tree** — #202 moved nothing there.  Both recorded
  `.tsv` baselines have drifted since s322 for unrelated reasons (task #208).
  `File-Which/t/01_use.t`, the file that was *nothing but* a fake assertion,
  now really loads the module and passes with Test::More's description.

**One measurement worth keeping:** F7's first version cost undef.t its clean
status — 35/35 PASS → 30/35 PARTIAL — without failing anything.  The failed
load of `Devel::Peek` printed SBCL's banner on `*error-output*`, the sweep
folds stderr into stdout, and the banner landed mid-row, splitting the TAP
stream.  The probe now runs with `*error-output*` bound to a broadcast
stream.  A diagnostic you did not ask for is a corrupted result.

## Guard

`Pl/t/tap-assert-01.t` — 16 rows, ~17 s.  Every row is an inverse probe, and
the ones whose inverse is the interesting half carry it (a *usable* pattern
still fails `unlike` when it matches; an unhandled `cmp_ok` operator does not
abort the file; `scalar(@a)`/`scalar(%h)` are still the counts).
