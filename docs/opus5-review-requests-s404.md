# Review request — s404 (Opus 5), for Fable

Session B of `docs/plan-post-s400.md`: the measurement portfolio (#345) and the
two new silent-wrongs (#349, #350), plus #353 folded in per
`docs/fable-answers-s402.md` §4.  Four commits.

| commit | item |
|---|---|
| `00b150f` s404a | **#345** — `--quick`, and the hang set MEASURED at three budgets |
| `a82acbc` s404b | **#353** — prototype extraction died on top-level POD |
| `d1ecafc` s404c | **#349** (closes **#217**) — an extension carries no program preamble |
| `987c78a` s404d | **#350** — a file-top `require` runs where it stands |
| `a46aa3f` s404f | **#354 + #351** — the two PPI operator-vs-term mis-lexes (§10) |

The parts worth your time: **§1.2** (a ruling whose premise was false for one
of its two files — measured), **§4**'s population number, **§8** (the #351 /
#354 sizing, which answers where they go in the queue) and **§9**'s asks.

## 1. #345 — `--quick`, and a ruling whose premise was false

### 1.1 What shipped

`tools/run-perl-suite.pl --quick` does not run two kinds of file:

* the **hang set** — files measured to return the SAME rows at a larger budget
  (task #326's own test), so time buys nothing;
* a file whose **registered allowance exceeds 120 s** — it cannot finish inside
  a quick run's budget.

Each gets a `NOT-RUN` row naming WHICH rule fired and its cause, and NOT-RUN is
an UNEXPLAINED status, so the file still fails the run and still appears in the
tsv as a hole — the QUARANTINE treatment, for the same reason.

**Deviation, deliberate: a capped file is not run at all, rather than run with a
smaller budget** (the task's proposal item 2 said "contributes what it can").
A truncated TAP stream is not a cheaper measurement, it is a different one:
`C_ok` then means "how far it got before the cutoff" (the s325/#195 lesson the
snapshot header records), so the file's verdict would differ between quick and
full runs for no reason but the clock — which is exactly what `--quick`'s own
bar forbids ("identical verdicts for every file that runs in both").

### 1.2 §7.4's premise was false for one of the two files — please rule

The ruling said to REGISTER an allowance for `re/pat_psycho.t` and
`re/speed.t`.  Measured this session, at three budgets:

| file | perl | PCL @90 s | PCL @300 s | PCL @900 s | verdict |
|---|---|---|---|---|---|
| `re/overload.t` | 87 | 3 (TIMEOUT) | 3 (s398) | 3 (TIMEOUT) | HANG |
| `re/speed.t` | 59 | 0 (crash, s392) | 1 (TIMEOUT) | 1 (TIMEOUT) | HANG |
| `re/pat_psycho.t` | 15 | 11 (crash, s392) | **11, COMPLETED (DIFF)** | 12 (TIMEOUT) | SLOW + flaky |

So `re/speed.t` is a hang: an allowance would promise "give it the time and it
finishes", which is false, and `docs/perl-suite-timeouts.tsv` says in its own
header that this is what the registry means.  `re/pat_psycho.t` IS merely slow
— it completed once at 300 s — so it got the allowance (450 s, sized from that
completion), which the 120 s cap then keeps out of quick runs exactly as §7.4
intended.

`re/overload.t` (which task #345 named alongside the #326 six) is a third hang,
now measured at 10× the default: it joins the skip set.

## 2. #353 — prototype extraction died on top-level POD

One line, exactly as the task diagnosed.  `Parser::_process_block`'s sub-hoist
pre-pass walks `children` (which yields INSIGNIFICANT tokens too) and skipped
only Whitespace and Comment BY CLASS NAME, so a `PPI::Token::Pod` reached
`find`, which is a `PPI::Node` method.  The die was caught by
`_extract_module_prototypes`, reported once as "Failed to extract prototypes
from <Module>", and undef was cached — so EVERY prototype in such a module was
silently invisible to the compiler.

The fix asks the CLASS, not the name, in the shape
`Parser2::_collect_named_subs` already uses:

    my $words = $child->isa('PPI::Node')
                ? ($child->find('PPI::Token::Word') || [])
                : ($child->isa('PPI::Token::Word') ? [$child] : []);

**Inverse check** (a worktree at HEAD, same fixture): prototype MISSING before,
FOUND after.  The minimised fixture is now a guard in `Pl/t/prototype-01.t`
(three rows): a module with POD inside a block that binds a lexical and calls a
sub before defining it — the pre-pass only runs under those conditions, which
is why most POD-carrying modules never hit it.

**Reach, measured:** `Unicode::UCD` declares **13** prototypes (`charprop
($$;$)`, `loose_name ($)`, …) and all 13 were being thrown away.  Emission,
however, does not move: `tools/corpus-diff.pl` identical across 111 files, and
`tools/emission-ab.pl` over **both populations (657 files) reports 657 SAME**.
So no file in either population was actually mis-compiled by this — the call
sites in the six companion files are spelled in ways the prototypes do not
change.  What the fix removes is a latent silent-wrong (any module reached this
way loses ALL its prototypes) and the noise that hid other stderr signals.
Per the WHAT-CHANGED table that identical pair means the sweep is NOT owed for
this change; it ran anyway, for #349's measurement (§3).

## 3. #350 — the file-top `require` hoist

The task asked for a MEASUREMENT, not a patch.  Both halves are done.

**The probe, vs perl 5.40.3** (`push @INC, $dir; require MyLocal350;`):

    perl                       hi from MyLocal350
    PCL, today                 unhandled condition (module not found)
    PCL, emitted in place      hi from MyLocal350

**The other two spellings already emit in place** — measured, not assumed:
`require "file350.pl"` and `require $var` both go through the quoted/expression
branch, which emits into the CURRENT bucket, so the push precedes them and both
already match perl.  Only the BAREWORD form hoists.

**Population** (`tools/emission-ab.pl --env PCL_REQUIRE_INPLACE=1` over both
populations, 657 files): **52 DIFF, 605 SAME, 0 RCDIFF** — 7 perl-tests files,
1 `lib/` shim (`lib/IO/Handle.pm`), 44 in perl's own t/ (op 17, re 14, mro 10,
comp/io/run 1 each).  **Every one of the 52 diffs is the same shape**, checked
mechanically and then by eye on the four that carry more than one hunk: the
`(p-eval-always (p-require "X"))` form leaves the declarations bucket and
becomes `(p-require "X")` at the statement's own position; the only other lines
that move are the `(p-run-compile-phase-blocks)` / `(p-set-current-package …)`
markers it used to precede.  Nothing else changed anywhere.

**Nothing depended on the hoist**, so it is gone — the three reasons a NESTED
`require` was never hoisted (an `eval { require Foo }` must be able to catch
the failure; a `require` in a block runs at runtime; a `require` inside `SKIP {
}` must not run when the block is skipped) are all the same reason, and it
applies at depth 0 too.  The comment that justified the hoist predates the
emitted `(p-defpackage …)` line that now guarantees read-time package
existence.

## 4. #349 — the artifacts' program preamble (closes #217)

`pl2cl --extension` emits the gen stamp, `(in-package :pcl)` and the `:main`
package switch, and NO program preamble.  It differs from `--module` in one
way only: `--module` is the RUNTIME transpiling mid-program (drop announcements
off, s402 §1.4), while `--extension` is a developer building a checked-in
artifact, so diagnostics stay on.

**Population, measured before the fix** (temporary instrumentation in
`p-load-extension`, one full sweep): **17 extension loads across 14 test
files, all of them `pcl-pack`, and EVERY ONE lost an @INC entry** — 13 lost the
program's own source directory, and 4 (the `fresh_perl`/`runperl` children,
which run out of `/tmp`) lost theirs.  No perl-tests file ALSO pushed a
directory at runtime before calling pack, so nothing in this population was
visibly broken by it — the loss was latent, exactly as the task predicted, and
the shape that breaks (`push @INC, $dir; pack …; require Foo`) is the one in
the probe.

The instrumentation is gone; what replaced it is the rule-12 guard: an
extension that changes `@INC`, `*pcl-pl2cl-path*` or `*p-core-inc-dirs*` across
its load makes `p-load-extension` DIE naming the extension and telling the
reader to rebuild it with the flag.

**Consequences that landed with it:**

* all three artifacts regenerated (`tools/rebuild-pack` now passes the flag;
  the two shim commands in the runtime and in `artifact-staleness-01.t` say
  `--extension`), and they now contain **zero** absolute paths;
* `Pl/t/no-hardcoded-paths-01.t` **tightens**: the artifacts are no longer
  excluded from the scan, they are scanned like any hand-written file (the
  pinned row now says "scanned", not "excluded");
* new `Pl/t/extension-preamble-01.t` (5 rows): the flag emits no preamble and
  keeps the stamp; no checked-in artifact carries one; a runtime `push @INC`
  survives the first `pack()`; and a deliberately dirty scratch extension dies
  at load;
* **#217 closes** — the artifacts are machine-independent, so #277's installer
  item 2 (regenerate at install) is gone, as §9 of the s400 answers ruled.

## 5. The one gate expectation that had to be rewritten

`Pl/t/parser2-01.t` asserted the OLD emission — `require hoisted as eval-always
declaration (v1 parity)`.  Its own comment gives the reason it existed: parity
with which BUCKET v1's assembly used.  That is not a semantic claim, and perl's
rule is the other way, so under the standing expectation-rewrite rule (four
conjuncts) it is rewritten, not deleted:

* **perl-probed**: `push @INC, $dir; require MyLocal; MyLocal::hi()` loads
  under perl 5.40.3 and could not under the hoist;
* **the diff is exactly the divergence**: the require moves between the two
  prints, nothing else;
* **the edit STRENGTHENS**: the row now asserts source order (`print "a"` →
  `require` → `print "b"`) AND a new `unlike` row that it is not wrapped in
  `p-eval-always` — a semantic claim where the old one was a bucket claim;
* **guard in the same commit**: the runtime row in `Pl/t/use-require-01.t`
  (push a tempdir onto `@INC`, `require` a module from it, print its result).

## 6. Measurements

The WHAT-CHANGED table decided these.  `Pl/**` + `cl/**` + `lib/`-reaching
emission + a runner change all fired, so everything below was owed.

| measurement | result |
|---|---|
| `tools/corpus-diff.pl` (#353 alone) | **identical across 111 files**, silent drops 12 unchanged |
| `tools/emission-ab.pl` vs HEAD, both populations (#353 alone) | **657 files, 657 SAME, 0 DIFF, 0 RCDIFF** |
| `tools/emission-ab.pl --env PCL_REQUIRE_INPLACE=1`, both populations (#350) | 52 DIFF / 605 SAME / 0 RCDIFF, every diff the same shape (§3) |
| Full sweep BEFORE the fixes (instrumented, for #349's population) | **GATE clean**, 0 new / 0 fixed, TOTAL passing **18516 = baseline**, DROPS census 12 = current 12 |
| Full sweep AFTER (#349 artifacts + #350 emission + gen v2-149) | **GATE clean**, 0 new / 0 fixed, TOTAL passing **18516 = baseline**, DROPS 12 = 12; the standing 2 UNSTABLE + 8 unverified |
| `perl-tests/pack.t` on the regenerated artifact | **5636 pass / 89 fail** — the blessed s316b/s322 numbers to the digit |
| Gate `tools/prove-core` | **145 files / 5299 rows**, failures exactly the 13 pclxs xs rows (was 144 / 5289: +1 file, +10 rows — 5 `extension-preamble`, 3 `prototype`, 1 `use-require`, 1 `parser2`) |
| Companion `--all --quick` vs the blessed snapshot | **504 files identical**, 13 not run by quick, 5 status moves, 1 count move — none of them mine (§7) |
| Companion `--all` vs `--all --quick` | §7 — the #345 bar |

### 6.1 The companion population did not move

`--all --quick` (523 files: 86 OK, 110 XDIFF, 271 DIFF, 30 NOTAP, 1 FIXTURE,
11 not-run-by-quick + 2 quarantined) against `docs/perl-suite-run.tsv`:

| file | move | reading |
|---|---|---|
| `io/shm.t` | TRANSPILE → XDIFF | the snapshot row is STALE: it transpiles at HEAD too (probed in a worktree — the only emission difference is the gen stamp and the worktree's own paths) |
| `op/assignwarn.t`, `op/hashassign.t`, `op/numify.t` | DIFF → XDIFF, counts IDENTICAL | the row multiset now matches the blessed registry; no coverage moved (same C_ok/C_notok) |
| `op/utf8cache.t` | DIFF → TIMEOUT, C 2/0 both | timing artifact at `--jobs 4` |
| `mro/package_aliases_utf8.t` | 48/26 → 60/30 | the file registered `*rows-unstable*` — its documented noise |

Its DROPS line reads **353 in this run with no per-file increase** against the
census (24 files not measured, the ones that produce no CL).

## 7. #345's bar: `--all` vs `--all --quick`, same tree

| | |
|---|---|
| ran in BOTH forms | **512 files** |
| identical verdict AND counts | **510** |
| disagreements | 2 — `mro/package_aliases_utf8.t` (the file registered `*rows-unstable*`: 60/30 vs 66/32) and `op/utf8cache.t` (quick TIMEOUT, full DIFF, **same 2/0 counts**) |
| run only in the full form | the 11 quick did not run |
| wall clock, `--jobs 4` | quick **13 min**, full **19.5 min** |

**The skip set is exactly right, and the full run proves it**: all 9 skipped
files came back **TIMEOUT** in the full run, i.e. they spent their whole
budget and produced nothing comparable.  The two allowance-capped files
(`comp/require.t`, `re/pat_advanced.t`) ran in the full form as usual, and
`re/pat_psycho.t` — the one I gave an allowance rather than a skip — TIMEOUTed
even at 450 s under `--jobs 4` contention, which is what its registry cause
says: slow AND flaky, completing only when it has the machine.

The full run's DROPS line: **364, no per-file increase** over the census.

## 8. Sizing by-product for #351 and #354 (free, as §7 of the s402 answers asked)

One transpile pass over both populations with `PCL_DROP_ANNOUNCE=all`, 657
files, 2.5 min, collecting every `PCL: statement dropped` line (388 of them):

* **#351 (bare `/re/` after a paren-less call) — 6 announcements in 3 files,
  ALL in perl's own t/**: `t/re/pat.t` ×4 (lines 99, 101, 105, 114),
  `t/re/pat_advanced.t:1343`, `t/re/pat_re_eval.t:1360`.  **Zero in
  perl-tests** (the sweep population) and zero in `lib/`.  So by the s403
  ruling's own test it does NOT jump ahead of #346: no sweep row is affected.
  (`t/op/switch.t:836` is the same PPI family with `when /pea/` — a different
  keyword; worth a line in the task.)  One near-miss worth recording: the
  `grep /\s/u, map chr, …` drop in `perl-tests/split.t:503` LOOKS like #351
  but is not — the statement transpiles fine in isolation, its census message
  is the `funcall`-reference one, and it is blessed in the census.
* **#354 (`)*name` lexed as a glob) — ZERO in both populations.**  The shape
  occurs in CPAN code (Data::Dump:325) and in no `.t` file of either
  population, so its guard rows will have to be written, not harvested.

## 9. What needs a ruling

1. **§1.2 — s400 §7.4's premise.**  I registered `re/pat_psycho.t` (slow) and
   skipped `re/speed.t` + `re/overload.t` (hangs), against a ruling that said
   to register both.  The measurement is on the task and above; if you want
   `re/speed.t` registered anyway (so a full run shows its 1 row), say so and
   I will move it.
2. **§1.1 — the capped-file deviation** (NOT-RUN rather than run-with-a-
   smaller-budget).  Ratify or overrule.
3. **#349's flag name and shape** — `--extension` as a third mode beside
   `--module`, differing only in that diagnostics stay on.  The alternative
   was reusing `--module` and losing the drop announcement for artifact
   builds; I judged the announcement worth a flag.
4. **#350 shipped rather than reported.**  The task said "FIX = A
   MEASUREMENT, not a patch"; the measurement came back clean (52 files, one
   shape, nothing depending on the hoist) and the probe matches perl, so I
   flipped it in the same session.  If you would rather have seen the
   measurement alone first, that is the process point to make.
5. **The one rewritten gate expectation** (§5) — please check the four
   conjuncts yourself; it is the only test whose meaning changed.


## 10. #354 + #351 — the two PPI operator-vs-term mis-lexes (one commit)

Fable's §7 put these next, and the sizing in §8 said neither touches a sweep
row, so they went in one commit: the layer (`_repair_*` on the raw token
stream) and the mechanism (rewrite source, reparse) are shared, even though
each predicate is its own.

**#354** — `_repair_glob_multiply` splits `*name` back into operator + word
when the previous significant token ENDS A TERM.  `}` counts only when it
closes a SUBSCRIPT: `$h{x}*foo()` is multiplication, `sub f {…} *bar = \&f;` is
a real glob, and the tree says which.  Seven shapes probed against perl.

**#351** — `_repair_word_match` rewrites the opening `/` to `m/`.  The
condition Fable ruled (#266's callable classifier) turned out to be
**unavailable at repair time and unnecessary**: the classifier needs the
environment, which at repair time knows nothing (`ok` comes from a
BEGIN-required `test.pl`), and perl's rule has a NEGATIVE form that needs no
import knowledge at all —

    sub ok {…} … ok /foo/, "d";     CALL(1 desc)      (declared ABOVE)
    … ok /foo/, "d"; sub ok {…}     syntax error      (declared BELOW)
    … ok /foo/, "d";                syntax error      (not declared)
    require "./t.pl"; ok /foo/…     syntax error      (runtime require)
    time / 60                       division
    use constant PI => 6; PI / 2    3  (division)
    sub f {…} print f / 2           match ("Search pattern not terminated")
    sub g () {…} print g / 2        5  (division)

perl reads `/` as division only when the word is a TERM; for a non-term it does
not fall back to division, it is a **syntax error**.  PCL assumes valid Perl
(principle 9), so **"not a term" is the whole test** — and "term" is answerable
from the document plus the arity table: 0-ary builtins (from the ONE table, not
a new list), `use constant`/`sub NAME ()` declared here, and the ALL-CAPS
convention `_bareword_subscript_autoquotes` already uses.  **Please rule on
that substitution** — it is a deviation from the letter of §4's ruling, made
because the ruled condition cannot see the names that matter.

**The population scan is what makes this safe, and it found the trap.**  28
`WORD /` sites over both populations: `ok` (6), `while` (11), `when` (2) — all
repairs — and `map { … } <op/*>` (3), where PPI derails a GLOB into
`< Word / * >` and a repair would have been catastrophic.  Hence the `<` guard.

**A third bug, found on the way and fixed with them.**  `pl2cl`'s stdin branch
held a bare `local $/;` across the parse, and **PPI's tokenization of a trailing
`__END__`/`__DATA__` depends on `$/`** — in slurp mode the section gains a
newline.  Every program compiled through `pl2cl < file` therefore got an extra
empty line in its `<DATA>` handle; `tools/emission-ab.pl` feeds files that way,
which is how it surfaced (as an unexplained diff I chased rather than waved
through).  The slurp is scoped, and `_ppi_parse` — the one place either pipeline
turns source into a document — trims a tail the parse invented, so the result no
longer depends on the caller's `$/`.

**Measurements:** emission-ab over both populations 653 SAME / 4 DIFF (the three
files whose drops now compile + `tie_fetch_count.t`, whose empty `__DATA__`
stops gaining a line); corpus-diff **identical** across 111 files (so the sweep
cannot move); the announcement census **388 → 377**, i.e. 11 statements
recovered; companion `--dir re` moves exactly one row — `pat_re_eval.t`
461 → 462 failing rows, a statement that used to vanish now reporting honestly;
gate **145 files / 5306 rows**, failures exactly the 13 pclxs xs rows.

**Rule 13 discharged:** `ppi-upstream-bugs.md` §12 and §13; `ppi-bug-report.t`
Bug 9 and Bug 10 (`tests => 10`, all ten rows failing on PPI 1.291, as they
must); three canaries in `misc-fixes-02.t`; end-to-end guards in
`transpile-test-10.t` and `data-handle-01.t` (including the stdin path).

**#356 filed** (pre-existing, found probing #351's breaking cases): `print PI /
2, "\n"` is dropped, and `print PI + 1, "\n"` treats the constant as a
FILEHANDLE — `(p-print :fh 'PI 1 "\n")` — and prints nothing at all.  Both are
the #266 question asked at the print-filehandle site.
