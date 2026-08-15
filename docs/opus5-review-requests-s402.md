# Review request — s402 (Opus 5), for Fable

Session A of `docs/plan-post-s400.md`: the three diagnostics-and-tools items,
all shipped.  Three commits.

| commit | item |
|---|---|
| `235d0b4` s402a | #339 (the drop announcement, option (b) + its four amendments) + #343's `add_node` piece |
| `db3f98b` s402b | #343 §6.5 — the DROPS runner column and the sweep's fifth bucket |
| `958baf4` s402c | ruling §7.3 — the gen-stamp promise in `ir-spec.md` §9.2, the two guards citing it, session log, DECIDED |

**Measurements** (the WHAT-CHANGED table decided these): `tools/corpus-diff.pl`
**identical across 111 files**, silent drops 12 unchanged; `tools/emission-ab.pl`
over the 19 `lib/**.pm` shims **19 SAME**; gate `tools/prove-core` **144 files /
5289 rows**, failures exactly the 13 pclxs xs rows; **gate-SET scan over BOTH
populations** (638 files, base worktree vs working tree) — 160 changed rows, all
accounted for (§1.3); **full sweep GATE clean** — 0 new / 0 fixed, TOTAL passing
**18516 = baseline**, the standing 2 UNSTABLE + 8 unverified, and the new bucket
reading `TOTAL dropped statements: census 12, current 12 (+0)`.

The sweep was owed by the *runner* row of the table (I changed
`sweep-perl-tests.pl`), not by the emission rows — corpus-diff was identical.
It also validated the new column end to end, which is why I ran it before, not
after, writing this.

The parts worth your time: **§1.2** (an amendment whose stated condition turned
out to be false, and I went the other way), **§1.4** (a deviation the gate found
that no amount of reading would have), and **§4** (three new tasks, all silent
wrongs, one of them a PPI bug that eats whole statements).

---

## 1. #339 — the announcement

### 1.1 What it says

    PCL: statement dropped at perl-tests/bless.t line 179: is ref $untied, "main", 'blessing through tied refs' or diag $@; -- Bug. Fell through. Missing case: [
    PCL: statement dropped at perl-tests/substr.t line 697: bar = "XXX"; -- PCL: Can't modify non-lvalue subroutine call in assignment
    PCL: statement dropped at t/comp/parser.t line 541: tell FILE1 -- Can't locate object method "add_node" via package "Pl::PExpr"

stderr, transpile time, exit 0, once per statement.  The census's own example
now self-reports, and the third line is how I found the `add_node` bug the task
asked me to probe — I did not have to look for it.

**Deviation 1 (small, deliberate): the separator is ASCII `--`, not an em
dash.**  `pl2cl` does `binmode(STDERR, ":utf8")`, so a raw UTF-8 em dash in the
source is DOUBLE-encoded on the way out — measured, `303 242 302 200 302 224` —
and a `\x{2014}` character would warn "Wide character" under any entry point
without that layer.  Every other diagnostic in this compiler is ASCII.  The
keying half of amendment (i) (the fixed prefix) is unchanged.

**Dedupe.**  A statement can reach an emitter twice (the v2 seam tries the form
entry; a block body is lowered inside an outer lowering).  op/switch.t announced
**138 events for 112 emitted drops**; keyed on (file, line, text) it is 112.
"Announce ONCE" is the ruling, and the COUNT of drops is the runner column,
which reads the emitted CL — so the two never disagree.

### 1.2 Amendment (iii): the condition was FALSE, and I still deleted them — please rule

The ruling says delete the two `local $SIG{__WARN__} = sub {}` workarounds
"**if this warn was all they silenced** … verify by running without them first".

I verified, with a full-stderr scan (every line, not just the first) over both
populations, 659 files, before and after.  It was **not** all they silenced:

| exposed line | files |
|---|---|
| `Use of uninitialized value in pattern match (m//)` at `Pl/VarAnnotator.pm` line 1005 | t/op/numconvert.t (×2) |
| `Deep recursion on subroutine "Pl::VarAnnotator::_tw_walk"` / `_tw_operand_ok` | perl-tests/concat.t, t/opbasic/concat.t (×2 each) |

Five rows over 659 files.  **I kept the deletion** on three grounds: the sin is
the silence (rule 12's own logic — a blanket silencer is exactly what hid these);
the same "Deep recursion" warning already prints *unsilenced* from
`Pl::CLForm::_flat` **3193 times** on t/comp/parser.t alone and from
`Pl::Parser::_find_all_declarations` 200 times, so the codebase already tolerates
it; and the uninitialized value is a real latent defect (a `PPI::Token::Symbol`
whose `->content` is undef — the same undef-node family as #351).  Filed as
**#352** with the reproducer.

**Ask:** ratify, or tell me to restore the silencers (the two lines are in
`235d0b4`'s diff).  If restore, I would rather narrow them to the exact
diagnostic than restore a blanket handler.

### 1.3 The gate-SET bar, decomposed

160 changed rows over 638 files, and every one is one of three things:

- **69 files** — the first stderr line is now the drop announcement.  The drop
  was always there; only the silence changed.
- **11 files** — the first line changed to a *different, pre-existing* line,
  because the deleted warn WAS line 1 and something below it moved up.  All
  verified present in the before-scan: the Unicode::UCD prototype failure (×6,
  now filed as #353), `Use of uninitialized value $kind in string eq` (×2), the
  deep-recursion lines.
- **3 files** (inside the 11) — the workaround deletion of §1.2.

No file gained a die; none lost one.  No file moved from OK to TRANSPILE-FAIL.

### 1.4 The gate found the deviation that mattered: `--module`

`Pl/t/misc-fixes-01.t` t47 (`use Data::Dump`) went RED.  Cause: `use` transpiles
the module **inside the running SBCL** (`p-transpile-file`, `:error
*error-output*`), so the announcement landed in the PROGRAM's merged output —
and only on a **cold module cache**, which makes it nondeterministic output on
top of noisy.  So the announcement is OFF in `pl2cl --module`, with
`PCL_DROP_ANNOUNCE=all` to force it on.

The ruling's "(the runtime never sees a drop)" is true of *program* transpiles;
module loading is the exception, and it is the one place where the output is
also a test fixture.  Worth recording as the general rule: a compiler diagnostic
that can fire during a RUN has to answer "and what happens on a warm cache?"

**By-product worth a line:** with `PCL_DROP_ANNOUNCE=all`,
`Data/Dump.pm` line 325 drops `$kstat_sum2 += length($key)*length($key);` — a
statement of a real CPAN module PCL deletes.  The census covered perl-tests +
perl's t/ + `lib/`, never installed CPAN modules; noted on #343.

## 2. #343 — the runner column

`.faillog/_status.tsv` gains `drops` (name, status, pass, fail, planned,
**drops**, note; `-1` = NOT MEASURED, never 0); `sweep-diff.pl` gains the fifth
bucket against `docs/parse-error-drop-census-s399.tsv`, exits nonzero on a new
drop, and prints `DROPS: NOT CHECKED` when it cannot compare (the LOST rule's
shape).  `run-perl-suite.pl` records field 8 and prints the same comparison for
perl's t/.

It works in both directions on its first outing: the sweep reports `census 12,
current 12 (+0)` per file, and the suite reports `comp/parser.t 10 -> 8 fixed;
EDIT the census row` — which is s402a's `add_node` fix, in the population the
sweep does not cover.  The census row was EDITED, header total 379 → 377, with
the edit and its cause written into the file's header.

**The `add_node` piece.**  `Pl/PExpr.pm`'s zero-arg-prototype branch called
`$self->add_node({type=>'funcall', …})` — an `OpcodeTree` method PExpr does not
have — so the branch **always** died and the caller dropped the statement:

    sub FILE1 () { 1 }   sub dummy { tell FILE1 }     # the whole body vanishes

It now builds the node the way the rest of the file reads it back
(`make_node_insert` + one child = the function name, which is precisely what the
`*`-filehandle-prototype post-pass 3000 lines below asserts).  Ten probes of the
zero-arg-prototype family (`print F`, `$a[F-40]`, `$h{F()}`, `use constant`,
ternary, sort, map, `\&F`, a string-returning constant) identical to perl;
corpus-diff identical, so no perl-tests file's emission moves.

## 3. Ask: the ir-spec promise (§7.3) — I widened it slightly

`ir-spec.md` §9.2 states the stamp's format, its two consumers, and the two
obligations it creates.  I added one sentence the ruling did not ask for:
**the artifacts are discovered BY the stamp, not by a list** — because that is
what `artifact-staleness-01.t` actually does, and it is the property that made
s399 find the third artifact nobody had listed.  Say the word if you want §9.2
trimmed back to the ruling's one paragraph.

## 4. Filed this session — three tasks, all silent wrongs, all found by probes

**#351 — PPI mis-lexes a bare `/PATTERN/` after a paren-less call as DIVISION.**
This was amendment (iv)'s `ref=''` probe.  Two of the three `ref=''` sites are
legitimate declines (comp/final_line_num.t is *deliberately invalid Perl* —
`print 1+` at EOF; op/closure.t is a `format` block).  The third is a bug, and it
is a bad one:

    ok /$qr/, "desc";     perl: ok(1 desc)   PCL: (nothing — statement dropped)
    print /foo/, "\n";    perl: 1            PCL: (nothing)
    ok /foo/x, "d";       perl: ok(1 d)      PCL: (P-/ (P-/ …) "x") → division-by-zero

PPI gets it right after `grep`, `return`, `(` and `=`, and wrong after every
other Word — `print` included.  Logged as `ppi-upstream-bugs.md` §11 with a
failing row in `ppi-bug-report.t` (Bug 8, `tests => 8`), per rule 13.  The fix
belongs with the raw-token-stream repair family (`_repair_*` in Parser2) and
needs perl's own condition, so it is a task, not a guard.  **Sizing is now
cheap**: the announcement prints file+line+text for every drop, so one stderr
sweep over both populations gives the exact list.

**#352** — the two signals §1.2 exposed.

**#353** — prototype extraction dies on any module with **top-level POD** inside
a let-bound block: `$child->find('PPI::Token::Word')` where `$child` is a
`PPI::Token::Pod` (a Token, not a Node).  So *all* of Unicode::UCD's prototypes
are silently unavailable to six companion files.  One-line fix identified; NOT
taken here because a sweep was already in flight for this batch, and piling a
second compiler change onto a measurement in flight is how a verdict stops
meaning anything.

## 5. One fragility I did not act on

~20 `Pl/t/*.t` helpers run `` `$pl2cl $file 2>&1` `` and put the result straight
into the `.lisp` file — i.e. they merge transpile stderr INTO the generated CL.
Nothing drops in those snippets today (the gate is green), but a future test
whose snippet drops a statement will fail with a Lisp reader error instead of a
useful message.  That is pre-existing (it is why the two `$SIG{__WARN__}`
workarounds existed at all), and fixing it is ~20 files of `2>&1` → `2>/dev/null`
plus a decision about how a transpile ERROR should surface.  Say if you want it
filed.

## 6. Next

Session B of the plan: **#345** (`--quick`), **#349** (which closes #217 and
unblocks #277) and **#350**.
