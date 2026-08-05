# The 65 PARTIAL files of the 14-dist board — where the 635 failing rows are (s344, measured)

Companion to `docs/cpan-board14-survey-s343.md`, which classified the 53 FAILs
(41 real, in ~4 causes). This is the other half — task **#231**. Data:
`docs/cpan-board14-partial-causes-s344.tsv` (one line per PARTIAL file: family,
perl-oracle verdict, ok/not-ok counts, the FIRST not-ok row and its diagnostic).

Measured at `191f35d` / gen v2-105, same board as s343.

## Where the rows are

The board's 674 not-ok assertions split 635 in PARTIAL files, 39 in FAIL files.
The 635 are **not** spread evenly — three dists hold 85% of them:

| dist | files | not-ok | family |
|---|---|---|---|
| Text-Balanced | 7 | **300** | B (three distinct sub-causes) |
| Sub-Uplevel | 7 | **127** | A — `caller()` fidelity, one root |
| Scalar-List-Utils | 21 | **110** | C — List::Util/Scalar::Util shim parity |
| Sort-Versions | 1 | 31 | H — one unexplained cause (see below) |
| Role-Tiny | 7 | 22 | D |
| Capture-Tiny | 4 | 16 | E (#201) |
| Data-Dump | 6 | 7 | F |
| Class-Method-Modifiers | 4 | 7 | D (+1 row perl itself TODOs) |
| File-Which | 1 | 6 | H |
| Try-Tiny | 4 | 4 | G |
| Algorithm-Diff | 1 | 3 | H |
| Class-Inspector | 2 | 2 | H |

## Oracle correction — read this before repeating the method

s343's rule was right (always run real perl beside the board), but the **command
matters**: running `perl -I<dist>/lib` against an **XS dist whose `.so` is not
built** makes perl die at `use`, produce no TAP, and look like `PERL-NOTAP`.
That is exactly what happened on the first pass here: all 21 Scalar-List-Utils
files reported PERL-NOTAP and read as "board artifact". Re-run **without** the
dist lib — perl then uses the *installed* List::Util and every one of the 21
runs clean. So those 110 rows are **real PCL failures, not artifacts**.

Caveat recorded with them (`PERL-PASS*` in the TSV): that oracle is the
installed List::Util, older than the 1.70 dist, so two rows (`mesh.t`, `zip.t`)
fail under it for version reasons.

## The families

### A — `caller()` fidelity (7 files, 127 rows; Sub-Uplevel + one CMM row)

One root: PCL's `caller` does not report what perl's does. **Probed directly**
(`sub f { my @c = caller }`):

| | perl | PCL |
|---|---|---|
| `caller` in list context, no EXPR | 3 elements | **4** (`main /tmp/runpcl_*.lisp 20 main::f`) |
| filename | the `.pl`/`.t` file | the generated `.lisp` (or the `~/.pcl-cache` copy) |
| `$0` | the script path | **`sbcl`** |
| `#line NN` directive | honoured | ignored |

On top of that, `*CORE::GLOBAL::caller` overrides are not honoured
(04/05_honor_*_override), internal frames are visible to `caller` walks
(03_nested_uplevels: `main, Call, …, Wrap, Sub::Uplevel` where perl shows
`main, Call, …`), and `caller(N)` past the top of the stack returns a count
instead of an empty list (09_emptylist). 03_nested_uplevels alone is 103 rows —
one bug, 103 assertions. Filed as **#233**.

### B — Text::Balanced (7 files, 300 rows) — three sub-causes

**B1 `goto LABEL` emits a `go` outside its tagbody (07_exttag 86, 09_gentag 69).**
The runtime error is `Execution of a form compiled with errors … (go :failed) …
attempt to GO to nonexistent tag: :failed`. **Reduced to a 1-line repro**:

```perl
sub f { my $n=shift;
        if($n==0){goto failed}                  # site 1: outside a loop
        while(1){ if($n==2){goto failed} else {last} }   # site 2: inside a loop
        short: return "s";                      # an intervening label
        failed: return "F"; }
print f(0), "\n";        # perl: F     PCL: CRASH(go :failed)
```

All three ingredients are needed — drop the loop-internal `goto`, or the
intervening `short:` label, and it compiles fine (probed: 12 neighbouring
shapes pass). Worse, SBCL only *signals* when that branch executes, so the
breakage is invisible until the failing path is taken — `print f(1)` on the same
file is silent and correct. Filed as **#232**.

**B2 an in-scope lexical is unbound at run time (03_extcbk, part of 06_extqlk).**
`The variable |Text::Balanced|::$mods is unbound at (eval 1) line 1` — the
string-eval/closure scope family.

**B3 the extract_* return shape (02_extbrk, 05_extmul, 06_extqlk, 08_extvar).**
The prefix/remainder offsets come back wrong: `extract_quotelike` on
`<<EOHERE;\nEOHERE\n;` returns `'<'` where perl returns `';'`, `08_extvar` on
`========$a;` returns `'='` for `';'`. One `pos()`/`\G` scanning question, not
four. Filed as **#237**.

### C — List::Util / Scalar::Util shim parity (21 files, 110 rows)

Not one bug — a list of individually named gaps, each visible in the TSV's
first-not-ok column: `dualvar`, `weaken`, `readonly`, `openhandle` (returns a
glob string, not a GLOB ref), `prototype()` introspection (returns undef —
two files), `reftype` REF-vs-SCALAR (**this is task #163's referent-kind tag**),
`pairgrep` in scalar context, `product()` with no args (should be 1),
`head`/`tail`, `looks_like_number(Inf)`, `uniqnum` float precision, taint, and
warning-on-undef rows (the #221 warnings model). Filed as **#238** as a
checklist, not a single fix.

### D — role / method-modifier machinery (11 PARTIAL files, 29 rows)

Same cluster the FAIL half found (#135): `overload` through a role does not
apply (13 of the 22 Role-Tiny rows: `'MyClass=HASH(0x1)'` where `'welp'` was
expected), `before`/`after` modifiers not run, `can()` on a composed class,
lvalue attributes, and around-modifier list context. Together with the 11 FAIL
files this is the largest genuine compiler/runtime cluster on the board.

### E — Capture-Tiny (4 files, 16 rows) — #201

`$?` not preserved across a capture, captured content empty, PerlIO layer
introspection. Same territory as the 16 FAIL files; #201 stays the
highest-value single fix on the board.

### F — Data::Dump (6 files, 7 rows) — mostly *its* output revealing *our* bugs

Two are worth naming on their own:

- **`( -f => 4, … )` is silently miscompiled.** A filetest LETTER before `=>`
  is parsed as the filetest operator instead of being autoquoted, and it eats
  the following element: probed, `my %h = (-f => 4, abc => 3)` gives perl
  `{-f=>4, abc=>3}` and PCL `{3=>undef, 4=>'abc'}` — **silent wrong values**,
  the worst class. A plain `-bareword` (e.g. `-bare`) is fine; only the filetest
  letters break. Filed as **#234**.
- `tied.t` dumps `{}` for a tied hash — #155 (tie on HASH), already known.

The rest are formatting/unicode-repeat divergences inside Data::Dump.

### G — Try::Tiny (4 files, 4 rows)

One row each: previous `$@` visible in `try`, `finally`, given/when, and one
error-text row (#149 territory).

### H — singles (5 files, 44 rows)

- **Sort-Versions versions.t — 31 rows, one cause, NOT yet explained.**
  `versions()` returns the wrong answer when called from a foreign package.
  Two hypotheses were probed and **ruled out**: `(caller)[0]` reporting the
  wrong package inside `eval { package Foo; … }`, and the symbolic-ref read
  `${"Foo::a"}` — both match perl exactly in isolation. Do not re-try those.
  Filed as **#239** with the probes recorded.
- Class-Inspector `class_inspector.t` t54: `->subclasses` with a hostile `isa`
  crashes the image — `Not a class or a legal superclass name: 'baz::plc-bar`.
- File-Which 6 rows, Algorithm-Diff 3 rows, Class-Inspector inc_to_local 1 row.

## Two findings that fall outside the board's own rows

- **`use lib "$ENV{HOME}/x"` does not interpolate.** Probed: `@INC[0]` is the
  literal `$ENV{HOME}/x` under PCL, `/home/bernt/x` under perl, while the same
  string in a plain assignment interpolates correctly. A `use` argument list is
  being taken as raw text. Filed as **#235**.
- **`explain()` does not dump.** PCL's TAP `explain` stringifies
  (`ARRAY(0x53)`), where Test::More returns a Data::Dumper rendering. Every
  `is_deeply` failure on the board therefore reports
  `got 'ARRAY(0x53)' / expected 'ARRAY(0x54)'` and says nothing — that is why
  05_extmul, shuffle.t, zip.t and the around-modifier rows above have no usable
  cause line. Fixing it is cheap and buys diagnosis everywhere. Filed as **#236**.

## Honest summary

The 635 PARTIAL rows collapse to roughly **eight families**, and three of them
carry 85% of the rows. Two are single bugs with big multipliers (#233 caller,
#232 goto), one is a scanning question (#237), one is a shim checklist (#238),
and the rest were already on the board's books (#201, #135, #155, #163).

Nothing in the PARTIAL half is a board artifact: every one of the 65 files
runs clean under real perl (with the oracle correction above), except the two
Scalar-List-Utils version rows and one Class-Method-Modifiers row perl itself
marks TODO.

## Reproduce

```bash
# per-file PCL output (the classification input)
tools/run-dist-t.pl [--no-dist-lib] ~/.cpan/build/<dist> ~/.cpan/build/<dist>/t/<file>
# the oracle — NOTE: no -I<dist>/lib for an XS dist with no built .so
cd ~/.cpan/build/<dist> && perl -It/lib t/<file>
```
