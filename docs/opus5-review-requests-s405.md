# Review request — s405 (Opus 5), for Fable

Session C finished, then session D of `docs/plan-post-s400.md`.  The two things
s404 recorded as OWED were paid first, and the first of them turned out to be a
runtime bug rather than a harness one.

| commit | item |
|---|---|
| `8609e38` s405a | **#358, closes #346** — `open FH, "<&=N"` on a CLOSED fd SUCCEEDED and the first read spun forever; that WAS the cloexec hang |
| `97c938f` s405b | **#340** — perl 5.34's `try` / `catch` / `finally` (op/try.t 0 → 23/28), with the PPI repair its `finally` needs |
| `a90b481` s405c | **#277** — the installer (release phase 1) |
| `a7e8ed9` s405d | docs: session log, DECIDED, ir-spec §6.3, the snapshot/baseline edits |
| `b0fb372` s405e | **#347** — the closure gap: a PROMOTED lexical is legitimately captured (one missing `next`); **#348 now has no blocker left** |
| s405f | docs: this file's §7, the plan's progress section, the snapshot row |

The parts worth your time: **§1.3** (the discriminating measurement, and why
`/proc/<pid>/fd` alone would have missed it), **§3.2** (the value/context and
`$@` model I chose for `p-try`, probed but not ruled), **§5** (two findings
filed, one of which makes the feature I just shipped unreachable in the spelling
most real code uses), **§6** (the installer's shape), **§7** (#347, including a
judgment call in §7.3 you may want to overrule) and **§8**'s asks.

## 1. #358 — the cloexec hang was `open`, not the harness

### 1.1 The measurement s404 left owed

`PERL=$PWD/tools/pclperl-for-tests tools/run-perl-suite.pl run/cloexec.t
--jobs 1 --timeout 300` reproduces #348's configuration for one file without
touching `pl-which_perl`.  Result BEFORE any change this session:

```
run/cloexec.t   P: 22/0   C: 2/0   TIMEOUT
```

### 1.2 The stalled child, caught live

```
$ ls -l /proc/10552/fd
0 -> /dev/null   1 -> pipe:[57351]   2 -> /tmp/…/err          <- s404l's fd leak IS fixed
$ grep ^State /proc/10552/status
State:  R (running)                                            <- SPINNING, not blocked
```

The child's entire program is `open INHERIT, "<&=$fd"; my $line = <INHERIT>`,
and cloexec.t's whole point is that the fd it was handed is NOT inherited.  So
the open should have failed.

### 1.3 The bug, and why the task's own measurement was not enough

perl fails all three spellings on a closed descriptor (probed):

```perl
open(my $t, "<&=37") or print "$!";   # Bad file descriptor
open(my $u, ">&=37") or print "$!";   # Bad file descriptor
open(my $v, "<&37")  or print "$!";   # Bad file descriptor
```

PCL did not: `sb-sys:make-fd-stream` does not check the descriptor, so
`%p-open-dup` handed back a stream on a dead fd, and the first read retried
EBADF **forever**.  Reproduced standalone in three lines (`open(FH,"<&=5"); <FH>`
— perl dies, PCL never returns).  The fix is one `fcntl(F_GETFD)` before the
stream is built, with the EBADF errno perl sets.

**The task named `/proc/<pid>/fd` as the discriminating measurement.  It was
half of one.**  The fd list said only that s404l's leak was gone; it was
`/proc/<pid>/status` — State **R**, not **S** — that said "retry loop, not a
blocked read" and pointed at the open rather than at the descriptor plumbing.
Worth carrying into the hang protocol: for a hang, always take the STATE with
the fds.

### 1.4 What it moves

| | before | after |
|---|---|---|
| `run/cloexec.t` under a PCL child | TIMEOUT, 2 rows | **DIFF 16/6** |

16/6 is *exactly* the row that file already had with real-perl children, so
**#348's cloexec half is unblocked** — and §7 closes its other half (#347) in
the same session, so the switch has no blocker left.  The six rows that still
differ are the `$^F` / fd-inheritance feature, filed as #359 (§5.1).

### 1.5 A spin OUTLIVES the run that started it

An hour after the first (pre-fix) cloexec measurement I found its child still
alive at **99.9% CPU, 3516 s**: `timeout` had killed the SBCL it started, and
the spinning grandchild — spawned by `pclperl-for-tests` inside it — was
orphaned, not reaped.  It had been burning one of sixteen cores through every
measurement in this session.  Same family as #273's orphaned `pl2cl --server`:
**a hung PCL child is not just a lost row, it is a resource leak that outlives
the harness**, and nothing in the suite runner notices.  Not fixed here (the
cause was #358 and it is gone); recorded because the next hang will do it
again, and because "wall time was odd today" is exactly how such a thing hides.

## 2. The runners row of the WHAT-CHANGED table, paid

`tools/pclperl-for-tests` changed in s404l, so the table's runners row wants
both populations re-run and compared file-by-file.  Done, and **exactly one row
moved, in the same direction in both**:

| population | before | after |
|---|---|---|
| `perl-tests/ref.t` (sweep) | 190 passing | **191** |
| `t/op/ref.t` (companion) | 196/45 | **197/44** |

Attributed by bisection in a worktree rather than by reasoning: e588c56 → 190,
df10b95 → 191.  It is the blank-line half of s404l, in a file that spawns 23
`fresh_perl`/`runperl` children and compares their output byte for byte.  Both
baselines edited row by row with that cause (`docs/pass-baseline.tsv` header,
`docs/perl-suite-run.tsv` header).

## 3. #340 — try / catch / finally

### 3.1 The PPI half (rule 13 discharged)

With `use feature 'try'` in scope PPI 1.291 knows the construct **half way**:
`try {…} catch (VAR) {…}` is a `PPI::Statement::Compound`, and then `finally
{…}` is left out of it — and the orphan statement it starts is unterminated, so
it swallows everything up to the next `;`:

```
PPI::Statement::Compound    try { foo(); } catch ($e) { bar($e); }
PPI::Statement              finally { baz(); } is($x, 1, 'desc');   <- two statements
```

In op/try.t that is an assertion per site.  `ppi-upstream-bugs.md` §18,
`ppi-bug-report.t` Bug 15 (`tests => 15`, all fifteen failing as they must),
canary in `misc-fixes-02.t`, end-to-end guard in `try-catch-01.t`.

The repair terminates the orphan where perl does (a `;` on the finally block's
closing brace, then reparse) and `_lower_block` joins the `finally {…};` back
onto its `try` — the same route the unlabeled `continue` block already takes,
so there is no new mechanism.

### 3.2 The lowering — and the model I chose

One `_lower_compound` arm + the `p-try` macro.  Every rule below was probed
against perl 5.40.3 first, and each is a place where copying `p-eval-block`
would have been wrong:

| | `eval {}` | `try {}` |
|---|---|---|
| `return` inside | exits the eval | **returns from the enclosing sub** |
| `last` / `next` / `redo` | (same) | **control the enclosing loop** |
| `$@` after | the error | **its pre-`try` value** (localized) |
| `$@` inside the handler | the error | **`""`** — the error only reaches VAR |
| a FALSE exception (`die 0`) | `$@` false, code usually misreads it | **catch runs** |
| value | body value or `nil` | **the executed block's last value, caller's context** |

So `p-try` catches no `:p-return` and no loop tag, binds nothing around
`*wantarray*`, pushes no frame (`caller()` sees through it), saves/restores `$@`
in an `unwind-protect` — which is also what makes `finally` run on a `return`
out of the try block — and decides on "an exception was signalled", never on a
value's truth.

`%p-caught-perl-value` is now the ONE condition→Perl-value converter, shared
with `p-eval-block` (rule 11); `p-eval-string` keeps its own tail text because
its `(eval N)` counter must not increment on the newline path.

### 3.3 What it measures

* `Pl/t/try-catch-01.t` — **25 rows, all green**.  Every row runs the same
  source through perl and through PCL and compares, so the expectations cannot
  drift from the oracle.
* **op/try.t: TRANSPILE-FAIL → 23/28.**  The five that remain are four other
  registered families, each checked rather than assumed:
  * 2 rows — `sub fscalar :lvalue { try { return $scalar } … }` + `fscalar = 123`:
    the file-level lvalue-sub refusal, which is the file's ONE drop and drops
    loudly by ruling (`fable-answers-s400` §6.3);
  * 1 row — `caller()` reporting the generated `.lisp` file and line instead of
    the Perl ones.  **Probed: identical inside and outside a try block**, i.e.
    not a try defect (`not-supported.md` "caller() filename and line number");
  * 1 row — the experimental-feature compile-time warning (#221);
  * 1 row — asserts perl's parse-error TEXT for `catch` without `(VAR)`, which
    principle 9 puts out of scope.

  Left **DIFF, not registered XDIFF**, so a real try regression still shows.

## 4. Measurements

| measurement | result |
|---|---|
| Gate `tools/prove-core` (final, after §7) | **146 files / 5345 rows**, failures exactly the 13 pclxs xs rows |
| Gate after §1–§6 (before §7) | 146 files / 5341 rows, same 13 |
| `tools/corpus-diff.pl` | **emission identical across 111 files**; silent drops 12, unchanged |
| `tools/emission-ab.pl --ref HEAD` over `lib/**` | **19/19 SAME, 0 DIFF, 0 RCDIFF** |
| Full sweep, twice (the runtime changed, and §7 is a name-resolution change) | **GATE clean** both times, 0 new / 0 fixed, **TOTAL 18517 = baseline**, DROPS census 12 = 12 |
| Companion `--all --quick` | *see §4.2* (and §4.1 for the run that was thrown away) |
| Drop census | `t/op/try.t` 0 → 1 (new row, lvalue-sub), `t/re/pat.t` 11 → 8 and `t/re/pat_re_eval.t` 1 → 0 (s404's repairs) — all EDITED with cause |

### 4.1 A companion run thrown away, and the rule it taught

The first `--all --quick` run of the session overlapped two `Pl/Parser2.pm`
edits and came back with 145 TRANSPILE files and three lost ref.t rows.  All of
it was the window in which `parse()` called `_repair_try_finally` before the sub
existed — including the ref.t rows, whose `fresh_perl` CHILDREN are transpiled
at run time.  The run was discarded and re-run after the code settled.  **Never
edit the compiler while a measurement is running** — the failure mode is not a
crash, it is a plausible-looking table.
### 4.2 The companion, file by file: 18 of 523 rows differ, all explained

`--all --quick` against the s400 snapshot (which was taken with a full
`--all`).  Every difference falls into four groups, and only two of them are
row movements at all:

| group | files | what it is |
|---|---|---|
| **NOT-RUN** | 11 | `--quick` skipping the hang set and the three >120 s allowances — by design (#345), and each one is listed in the run's own output |
| **label only, counts identical** | 3 | `op/assignwarn.t`, `op/hashassign.t`, `op/numify.t` DIFF → XDIFF: registered in `perl-suite-expected.tsv` after the snapshot was taken |
| **registered rows-unstable** | 1 | `mro/package_aliases_utf8.t` 48/26 → 63/31, the file the snapshot header already names as unstable |
| **real movements, both already accounted for** | 3 | `op/ref.t` 196/45 → **197/44** (§2, bisected to s404l); `re/pat_re_eval.t` 461 → **462** failing (s404's #351/#354 — a statement that used to vanish now reports honestly); `io/shm.t` TRANSPILE → XDIFF, 0 rows either way (the file now transpiles and reaches its registered IPC::SysV crash instead of failing to compile — also s404's repairs) |

DROPS printed no comparison lines, i.e. the census matches after this session's
three edits.

**A third `--all --quick` ran after §7** (a name-resolution change: the table
asks for the companion once).  Against the snapshot as this session leaves it,
**13 of 523 differ: the same 11 `--quick` NOT-RUN files, the registered
rows-unstable one, and `op/utf8cache.t` DIFF → TIMEOUT with its row counts
unchanged (2/0 both sides)** — a timing artifact, not a movement: emission-ab
proves the only file #347 touches is `op/lexsub.t`, and the snapshot header
already records that TIMEOUT-shaped files are not comparable across runs.  Its
DROPS is 358 vs the previous run's 350 — exactly `op/lexsub.t`'s eight, now
countable — and again no census-diff lines.


## 5. Filed this session

### 5.1 #359 — `$^F` / fd inheritance across exec (the 6 remaining cloexec rows)

perl passes descriptors ≤ `$^F` to exec'd children and marks the rest
close-on-exec AT OPEN TIME.  PCL has the `$^F` box and ignores it: SBCL's
`run-program` closes every descriptor above 2 in the child, so nothing is ever
inherited.  That is accidentally right for the default (`$^F` == 2) and wrong
for any program that raises it — which is what cloexec.t's six remaining rows
test.

**Sized already**: `sb-ext:run-program :preserve-fds` works — measured, for fd
**≥ 4**; fd 3 is used by SBCL's own spawn plumbing and cannot be handed through
(`head -1 <&3` in the child: "Bad file descriptor"; `<&4`, `<&5` read fine).
So the shape is: mark a descriptor inheritable at open time when `fd <= $^F`,
and pass the still-open marked ones as `:preserve-fds` at the ~8 `run-program`
sites.  With the default `$^F` the list is always empty, so nothing else can
move.  The fd-3 hole needs a ruling (announce? refuse?) — hence a task, not a
commit.

### 5.2 #360 — `use experimental 'try'` cannot be used, for two independent reasons

The spelling most real code uses to enable this feature fails twice over:

1. **PPI does not switch its `try` support on for `use experimental 'try'`** —
   only for `use feature`.  So the construct lexes like the no-pragma case (one
   swallowing statement) and the file does not compile.
2. **`experimental.pm` itself dies at load under PCL**: it does
   `$_ = version->new($_) for values %min_version;` and **`for values %h` does
   not alias in PCL** — the write lands on a copy, the hash keeps its strings,
   and the next line calls `->stringify` on `"5.34.0"`.  Probed:
   `$_ = $_ * 10 for values %h` leaves `%h` untouched (perl: multiplied);
   the array form aliases correctly.

(2) is the `values` half of the residue `fable-answers-s370` §5 records, which
is RULED onto the E5 boxed-aggregates axis — DO NOT START — so I did not touch
it.  A `lib/experimental.pm` workaround shim (the `File::Basename` /
`IO::Handle` pattern, with a delete-when trigger) would fix (2) for every
`use experimental` program, but not (1) for `try`.  **Ask 3 below.**

### 5.3 The compile-time cost of one more repair walk

`_repair_try_finally` adds one `$doc->find('PPI::Token::Word')` walk per file,
alongside the five that were already there.  I did not add an early-out,
because every cheap pre-check I could find (`serialize`, `find_first`) walks
the same tree — and the per-file transpile time is dominated by PExpr, not by
the repair chain.  Flagged here rather than measured to a number: if you want
a bound, the honest measurement is `pl2cl` wall time on the ten largest corpus
files before/after, which I can run in the next session.

## 6. #277 — the installer (release phase 1)

Session E of the plan, started because it is emission-neutral and therefore
cheap to verify next to everything above.  `tools/install-pcl`:

* **checks every dependency before copying anything** and reports them all at
  once (perl ≥ 5.20, PPI, Moo, Test::More, File::Temp, sbcl ≥ 2.5.2 — the floor
  CLAUDE.md names, i.e. what the suite is validated against, not a guess);
* **copies the runtime tree in its repo-RELATIVE shape** (`pl2cl`, `runpcl`,
  `Pl/`, `lib/`, `cl/`, `tools/lib/`) — the arrangement is not cosmetic, it IS
  the lookup mechanism (`dirname(abs_path($0))` in pl2cl, `FindBin::RealBin` in
  runpcl).  `Pl/t` is excluded: the gate is not part of an installation;
* **compiles the runtime into `<root>/pcl.core` at install time** — the USER
  ruling — writing to a temp name and renaming, so a half-written core can
  never be loaded;
* **writes wrappers, not symlinks**, in `$PREFIX/bin`: runpcl takes its root
  from `dirname($0)`, which a symlink would answer with the bin directory;
* **refuses to finish unless the INSTALLED tools work** — the smoke test
  transpiles AND runs a two-line program and compares the output exactly;
* **replaces an existing tree rather than merging into it** (`--force`), because
  a stale `lib/Foo.pm` shim from an older install is not inert — it is an @INC
  entry that shadows the core module PCL now uses.  It refuses to remove a
  directory that does not look like a PCL install.
* **warns** when the checked-in artifacts' `gen=` stamps are older than the
  checkout's `*pcl-cache-generation*`.  A warning, not a refusal: an installer
  is not the place to overrule what the developer chose to ship (and #349 took
  the *regeneration* item out of this task — the artifacts are machine-independent
  now).

**How the installed core is found.**  One clause in `PCLSbcl.pm`, the single
command-line builder: when a runner asks for source mode and `<root>/pcl.core`
sits beside `<root>/cl/<runtime>`, that core is used — with the same freshness
test `PCL_TEST_CORE` gets, so a core older than its runtime is ignored rather
than trusted.  The `/cl/` in the pattern is deliberate (matching "two
directories up" would couple the answer to whatever sits beside an arbitrary
caller's path).  **A checkout has no `pcl.core`, so no development runner's
command line changes** — verified with `PCL_SHOW_SBCL=1` before and after, and
four new rows in `tools/t/sbcl-prefix.t` pin it (absent → source mode; present
→ used; stale → ignored; not-a-`cl/`-layout → ignored).

End-to-end test: `tools/t/install-pcl.t` (11 rows, run directly like the other
two `tools/t` files) installs into a temp prefix with `--no-core` and checks the
tree shape, the wrappers, the absence of `Pl/t`, and that the installed pl2cl +
runpcl reproduce perl's output for a real program.

**Measured on a real install** (temp prefix, with the core): the whole install
is **2.9 s** and the core is **43 MB**; the installed `runpcl` spawns
`sbcl --core …/pcl.core …` (checked with `PCL_SHOW_SBCL=1`) and a hello-world
runs in **0.16 s with the core vs 1.58 s without** — which is the whole point of
compiling at install.  The emitted preamble points at the INSTALLED tree
(`*pcl-pl2cl-path*` and every `@INC` push), i.e. #349's relocatability holds end
to end on a tree that is not the checkout.

**The runners row, for this change.**  `PCLSbcl.pm` is one of the five runners'
shared parts, so the table asks for a re-run.  I ran the GATE (the runner whose
contract the file states) and the `PCL_SHOW_SBCL=1` before/after, and did NOT
re-run the sweep or the companion, because the change cannot reach them: it
fires only when `<root>/pcl.core` exists next to `<root>/cl/`, a checkout has
none, and the sweep/companion pass `PCL_TEST_CORE` which short-circuits the
lookup before it is consulted.  Both facts are pinned by unit rows rather than
left as reasoning.  Say if you want the two long runs anyway.

**Not in this commit** (both are their own tasks): #278's path sweep, and the
7.1 quoting unification — the latter changes two runners' command-line BYTES,
so it wants its own before/after comparison rather than a ride on this one.

## 7. #347 — the closure gap: one missing `next`, and #348 is now unblocked

### 7.1 What it was

A named sub nested in a block died with `Parser2 TODO: lexical 'X' possibly
captured by nested sub Y`, and since the v1 fallback that die was written for is
GONE (E4.1), it cost the whole file.  `op/closure.t` builds programs of exactly
that shape and runs them in a CHILD, which is why its 24 rows were invisible to
every transpile-time scan (`emission-ab`, `gate-set-scan`): you only see them by
pointing the child at PCL.

### 7.2 The fix is the sibling's own comment

`_check_sub_captures` — the scan for the same question one scope out —
has carried this since W5:

```perl
# W5: a name already rewritten to a package-level cell ($x__file__N) is
# legitimately captured — the hoisted sub and in-place code share the
# one defvar'd box, so it must NOT gate.
next if grep { $self->{_file_lex_renamed}{"$_$bare"} } '$', '@', '%';
```

`_hoist_nested_sub` was missing that one line.  So the promotion machinery
promoted the lexical to a cell — making the capture work — and the gate fired
anyway.  The fix is that `next`, with a comment naming the sibling it comes
from.

### 7.3 Probed, both directions — and the judgment call

Six shapes against perl 5.40.3:

| shape | perl | PCL after | |
|---|---|---|---|
| `{ my $x = 5; sub h { $x++ } }` (static-variable idiom) | 5 6 | 5 6 | ✓ |
| `sub outer { my $s = 70; sub h { $s } }` | undef / 70 | undef / 70 | ✓ |
| `foreach $fv (7,8) { sub h { $fv } }` — the loop VARIABLE | 900 | 900 | ✓ (**used to DIE**) |
| two same-named lexicals, one promoted | 1 2 | 1 2 | ✓ |
| `for my $i (1,2) { my $z = 10*$i; sub h { $z } }` | 10 | 20 | ✗ pre-existing |
| `while (…) { my $w = …; sub h { $w } }` | 101 | 102 | ✗ pre-existing |

The two misses are one family — perl's "will not stay shared" — and they
**already diverged before this change**: they never died.  A third member
(`foreach my $w` shadowing a promoted `$w`: perl undef, PCL 10) *did* die and
now diverges instead, so the family is registered in `not-supported.md` with
all three shapes, the four that match, and what would lift it (the hoisted sub
needs its OWN never-written cell when the enclosing lexical's storage is
per-call — a promotion-decision change, not a hoist change).

**The judgment call, stated so you can overrule it:** I traded a LOUD die for a
divergence in a family perl itself warns about.  What decided it: the die cost
every row of the file; the family already diverged in the shapes that did not
die, so the die was not protecting a coherent invariant; and a wrong answer here
is not silent in any measurement PCL runs — it is a failing TAP row.

### 7.4 What it moves

| | before | after |
|---|---|---|
| `op/closure.t` under PCL children | 235/27 (s400) | **267/3** — its real-perl-child row, all 24 rows back |
| `t/op/lexsub.t` | TRANSPILE-FAIL | **DIFF 6/8** (rc 2 → 0) — the ONLY file in either population whose emission the fix moves |
| `emission-ab` over both populations | — | 638 files, **637 SAME / 1 DIFF / 1 RCDIFF** |
| `corpus-diff` | — | **identical across 111 files** |

`t/op/lexsub.t` is a `my sub` file — #337's own territory — and its 8 drops are
countable for the first time (census row added, with that cause).

**#348 IS NOW UNBLOCKED.**  Its two blockers were #346 (this session, via #358)
and #347 (this section), and both files now read the SAME under a PCL child as
under a perl child: `run/cloexec.t` 16/6 and `op/closure.t` 267/3.  I did not
land it: the switch wants its own measurement pass (19 companion callers + the
sweep, rows spliced with causes), and mixing it into this session's runs would
muddle attribution.

## 8. Asks

1. **§3.2's model** — `$@` restored BEFORE `finally` runs, and the catch body
   seeing `""` rather than the error, are both probed against perl, but they are
   also the kind of thing that gets re-derived later.  ir-spec §6.3 now states
   them normatively; please confirm that is where they belong.
2. **op/try.t stays DIFF** (§3.3) rather than being registered XDIFF with its
   four causes.  My reasoning: three of the four causes are live tasks (#221,
   lvalue subs, caller file/line), so an XDIFF row would go stale silently the
   moment one lands, and the file is the only try coverage in the companion.
   Say if you want it registered instead.
3. **#360 (§5.2) — layer question.**  Making `use experimental 'try'` parse
   needs the compiler to know that this pragma enables features, i.e. a literal
   module name inside `Pl/` — which is exactly what CLAUDE.md 9a's smell test
   forbids.  The alternatives I see: (a) leave it unsupported and say so in
   `not-supported.md`; (b) ship the shim for (2) only, so every other
   `use experimental` program works and `try` alone stays unsupported; (c) rule
   that a *feature-enabling pragma* is a language mechanism, not module
   behaviour, and let a repair rewrite `use experimental LIST` to
   `use feature LIST` (both are no-ops in PCL, verified).
4. **#359 (§5.1)** — worth doing, or does it wait behind the release?  It is the
   only thing between cloexec.t and OK, and #348's other blocker (#347) is
   session G anyway.
5. **The installer's two shape decisions** (§6), both routine judgment calls I
   made rather than asked about, flagged because they are hard to change later:
   the default prefix is `$HOME/.local` (overridable with `--prefix` or
   `$PREFIX`), and the tree goes to `$PREFIX/lib/pcl` with wrappers in
   `$PREFIX/bin`.  A single-directory install (`/opt/pcl` + PATH) would also
   work and is what some Lisp implementations do; say if you prefer it before
   #282 writes a README around this one.
6. **Where the installed core lives.**  `<root>/pcl.core` is beside `cl/`, and
   `PCLSbcl` finds it by pattern.  The alternative was an explicit env var
   (`PCL_CORE`) written into the wrappers, which is more explicit but makes the
   wrappers load-bearing — a user calling `$PREFIX/lib/pcl/runpcl` directly
   would then silently lose the core.  I chose the pattern for that reason.
