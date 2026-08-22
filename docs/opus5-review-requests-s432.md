# Review requests — session 432 (Opus 5, 2026-08-22)

**Session P2 of `docs/plan-post-s430.md`: task #456 half (a) — the
forward-declaration stub that answered `nil`.**  Half (b) (the hoist order) is
measured and written up, not fixed; the plan authorised that split ("Do (a)
first — it turns the silent wrong into a loud one even if (b) slips").

Runtime-only: `cl/pcl-runtime.lisp` (plus guard rows, one restored perl-tests
assertion and baseline/snapshot edits).  `Pl/` untouched, so the emission
cannot move and the generation stays **v2-177**; the full sweep AND the
companion leg are owed anyway — the WHAT-TO-RUN `cl/**` row, because a runtime
change is invisible to corpus-diff.  Both were run.

---

## §1  What the fix is

`p-declare-sub` installs a forward stub so a call compiled before the
definition is readable.  Its body was `(defun name (&rest args) nil)` — so
CALLING it produced perl's `undef` and no diagnostic.  Two ways that happens,
and perl gives neither of them a value:

* the sub is **never defined** — perl dies `Undefined subroutine &main::foo
  called at F line N.`;
* PCL ran the call **before** the definition — #456's shape, a file-level
  `sub nm {…}` that lands after a block carrying a `package Q;` switch, because
  that block becomes its own emission section.

Now the stub calls `%p-call-of-undefined-sub`, which follows **perl's own
order**: run the package's `AUTOLOAD` if it has one (with `$AUTOLOAD` set to
the qualified name and the original arguments — and NO `@ISA` walk, that is the
method rule), else `p-die` with perl's message.

| probe | perl | PCL before | PCL after |
|---|---|---|---|
| `{ package Q; print main::nm(), "\|\n"; } sub nm {"PKG"}` | `PKG\|` | `\|` (silent) | dies `Undefined subroutine &main::nm called.` |
| the same inside `eval { }` | `$@` empty | `$@` empty | `$@` = the message (trappable) |
| `sub nope; nope();` | dies | undef, rc 0 | perl's own message |
| `sub foo; sub AUTOLOAD {…} foo()` | `AUTO(main::foo)` | undef | `AUTO(main::foo)` |
| `AUTOLOAD { $b <=> $a } sub stubbedsub; sort stubbedsub split//,'04381091'` | `98431100` | `04381091` | `98431100` |
| `{ print main::nm(), "\|\n"; } sub nm {"PKG"}` (no package switch) | `PKG\|` | `PKG\|` | `PKG\|` |
| `sub nm {"PKG"} { package Q; print main::nm(); }` (sub first) | `PKG\|` | `PKG\|` | `PKG\|` |

`\&foo` is untouched: `p-backslash-sub` hands back a trampoline that re-reads
`symbol-function` at call time, so a coderef taken on a stub still reaches a
body defined later.  `p-can`, `p-stash` and `p-coderef-defined-p` already
skipped `:stub`; only the stub's own body answered.

**A test came back.**  `perl-tests/sort.t` carried
`ok(1, "SKIP: stubborn AUTOLOAD — forward-declared sort sub blocks AUTOLOAD
dispatch in PCL")` in place of perl's `is join("", sort stubbedsub split//,
'04381091'), '98431100'`.  That limitation is exactly what the AUTOLOAD
fall-through removes, so the assertion is **restored** and passes.

## §2  Half (b), measured and NOT fixed

The mechanism is Parser2's "Cross-section forward sub calls" block, whose own
comment names the case: an earlier section's load-time code may call a sub a
LATER section defines, so it emits `(pcl:p-declare-sub Pkg::pl-name)` in the
prologue.  It hoists the DECLARATION, which makes the form readable; the
DEFINITION stays in its section, which is why the call reaches a stub.

**Population (measured s432, 722 files — perl-tests + perl's t/ + lib/ +
cpan-tests/modules): 75 files emit at least one such stub.**  Almost all are
tie/DESTROY/AUTOLOAD callbacks invoked after the file has finished loading, so
the stub is never entered — in the whole sweep exactly ONE program entered one.

Fix shape (in #456, with its bar): emit the sub's DEFINITION in the prologue
wrapped in its own `(in-package :X)` … `(in-package :pcl)` pair, since
`in-package` is read-time per top-level form and that is the only way to keep
the body's free symbols resolving in their own package.  It moves the emission
of 75 files, so it owes the four-population A/B with every diff explained.

## §3  Measurements

* **Gate: 160 files / 5705 rows**, only the 13 pclxs xs rows failing (5700 at
  s431 + 5 new guard rows in `Pl/t/decl-ordering-02.t`: two that assert the
  #456 shape now fails LOUDLY, one inverse `both_agree`, and two `both_agree`
  rows for the AUTOLOAD fall-through).
* **Full sweep, re-run on the final tree after the baseline edit: GATE clean,
  TOTAL passing 18366 = baseline (+0), 0 new / 0 fixed, drops 5 = census.**
  The one row that moved (before the edit) was **`sort.t` 203 → 202** (status OK →
  PARTIAL), and it was an **accidental pass**: bug 36430's
  `sort { A::min(@$a) <=> A::min(@$b) }` runs before the `package A; sub min`
  in its own block, both calls reached the stub, `undef <=> undef` is 0, the
  list came back unsorted, `$answer` was never touched and the assertion only
  checked that flag.  The statement now dies honestly and the sweep's
  `p-load-with-recovery` drops that one top-level form.  Edited into
  `docs/pass-baseline.tsv` BY HAND with that cause; `docs/fail-baseline.tsv`
  unchanged (0 new, 0 fixed).
* Drops unchanged (census 5 = current 5).
* **Companion suite (`--all --quick --jobs 4`, the `cl/**` row's leg): 522 of
  523 files measured** (the run was interrupted in its solo phase, which is
  op/cond.t — the file CLAUDE.md says to ignore).  **18 rows differ from
  `docs/perl-suite-run.tsv`; an A/B against a `bc02000` worktree attributes
  exactly FOUR to this change**, and all four are serial-confirmed REAL MOVEs:

  | file | at bc02000 | now | |
  |---|---|---|---|
  | op/method.t | 96/30 | **44/7** | −52 rows |
  | op/sort.t | 181/24 | **142/9** | −39 rows |
  | op/lexsub.t | 9/8 | **6/6** | −3 rows |
  | op/gmagic.t | 0/0 | **1/0** | +1 row |

  The other 14 are byte-identical on both trees — **stale snapshot rows**, not
  movers (the Track A refusal registrations moved `op/state.t`,
  `op/smartmatch.t`, `op/switch.t`, `op/coreamp.t`, `op/defer.t`,
  `op/tie_fetch_count.t` and the four `class/` files to XDIFF/TRANSPILE-FAIL in
  s415, and nobody has re-run `op/index.t`, `op/avhv.t`,
  `mro/package_aliases*.t` since).  Every one of the 18 is spliced with its
  attribution.

  **Why the same change costs 1 row in one population and 94 in the other:
  the two runners disagree on recovery.**  The sweep loads the emitted CL with
  `p-load-with-recovery` (one top-level form at a time, recovering from a die);
  `tools/run-perl-suite.pl` uses a plain `--load`, so the first die ends the
  file.  All 94 lost rows are AFTER the dying form, in three files that already
  crash (they were at 96/163, 181/205 and 9/156 of perl's rows).  Filed as
  **#467** — the two runners should agree, and the fix owes a snapshot re-bless.

## §4  THE ASKS

### Ask 1 — is AUTOLOAD-before-die right, given what it re-silences?

perl's rule for a body-less sub is AUTOLOAD, and following it fixed two real
rows (the restored sort.t assertion, and `sub foo; sub AUTOLOAD {…} foo()`).
But in a package that HAS an AUTOLOAD, it also re-silences #456's shape:

    sub AUTOLOAD { "AUTO" }
    { package Q; print main::nm(), "|\n"; }
    sub nm { "PKG" }          # perl PKG | PCL AUTO (before s432: empty)

PCL cannot tell "never defined" from "not yet defined" at the call, so it has
to pick one, and perl's rule is the right default for the case that is actually
perl's.  **Confirm** — or rule that a `:stub` symbol must DIE even when the
package has an AUTOLOAD (which would cost the two rows above).  Half (b)
removes the dilemma entirely.

### Ask 2 — the accidental pass, and the shape of the guard

The sort.t row is the third of this kind (s418's bless.t and split.t were the
first two): a silent wrong that made an assertion pass while testing nothing.
The row is edited out of `pass-baseline.tsv` with its cause, and the guard in
`Pl/t/decl-ordering-02.t` asserts the LOUD failure with an explicit note to
replace it with a `both_agree` row when (b) lands.  **Is asserting a
known-wrong-but-loud outcome the right guard shape here**, or should the row
instead be a `TODO`-style marker so it fails when (b) fixes it?  (The s416
stale-guard rule says the danger is a guard that silently keeps passing after
the behaviour it locks is gone; this one would keep passing.)

### Ask 3 — the companion cost: is rule 12 still right at −94 rows?

The sweep says the change costs ONE row.  The companion suite says 94, all of
them after the dying form in three files that already crash — and #467 shows
most of that gap is the runner, not PCL.  The rule-12 reading is unambiguous
(the stub produces a VALUE the program consumes, so it must die), and the
s329 "announce and continue" exception is for EFFECT-only cases.  But the
state.t precedent that produced that exception was exactly this shape: a die
that cost 88 verified rows while sweep-diff reported "0 new".

My reading: keep the die.  The state PCL is in when a stub is entered is a
COMPILER bug (half (b)), not a program property, and a compiler bug that
answers a value is the thing rule 12 exists to stop — the loud failure is what
will get (b) prioritised.  **Confirm, or rule that this one announces on stderr
and returns undef until (b) lands.**

### Ask 4 — half (b)'s priority

It is worth one honest sweep row today, and it is the last thing between #456's
shape and correctness.  The plan queues P3–P5 fillers ahead of it.  **Leave it
in #456 for a later session, or promote it** — it is the only item measured to
move 75 files' emission, so it wants its own session either way.
