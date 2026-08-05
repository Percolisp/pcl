# The 14-dist CPAN board — where it stands (s343, measured)

First survey of the **whole** widened board in one run, on `9615196`
(gen v2-105). Data: `docs/cpan-board14-s343.tsv` (per t-file status) and
`docs/cpan-board14-fail-causes-s343.tsv` (per-FAIL cause **plus what real perl
does with the same file**).

Unfinished: the 65 PARTIAL files were not cause-classified — that is the next
step, and it is where the remaining ~674 failing assertions live.

## The numbers

| | files | |
|---|---|---|
| PASS | 65 | |
| PARTIAL | 65 | at least one ok, plus not-ok rows or a late crash |
| FAIL | 53 | zero ok |
| **total** | **183** | across 14 dists |

Assertions: **1794 ok / 674 not-ok**.

## The discriminator that matters

The board measures PCL only, and "zero ok" is its FAIL rule — so a file that
legitimately has no assertions counts as a failure. Running every FAIL file
under **real perl** separates them:

| perl's verdict | files | reading |
|---|---|---|
| PERL-PASS | **41** | real failures — perl runs them, PCL produces no TAP |
| PERL-SKIPALL | 7 | author-only tests (`1..0 # SKIP Author testing only`) — **board artifact, not a PCL issue** |
| PERL-NOTAP | 5 | perl produces no TAP either (missing author prereqs) — same |

So the FAIL column is **41 real, 12 artifact**. Text-Balanced's five `9x_*.t`
(changes/critic/pmv/pod/pod_coverage) are the clearest case: perl skips all
five outright.

## The 41 real FAILs, by dist

| dist | real FAILs |
|---|---|
| Capture-Tiny | 16 |
| Class-Method-Modifiers | 6 |
| Role-Tiny | 5 |
| Mojo-DOM58 | 5 |
| Data-Dump | 5 |
| Sub-Uplevel | 3 |
| Try-Tiny | 1 |

**They are not 41 independent bugs.** Grouped by cause:

- **Capture-Tiny = essentially ONE bug.** 10 of its 16 die with the same SBCL
  `invalid number of arguments: 1`, and 2 more with
  `#<synonym-stream …> is closed`. This is task **#201**'s territory, and it is
  by far the highest-value single fix on the board.
- **XS / absent loadable object — 6 files, and this is the XS boundary, not a
  compiler bug.** `Storable` ×2 and `Encode` ×1 (Mojo-DOM58), plus 3 files
  where the message reads *"Can't locate loadable object for module **this
  module**"*. That literal is `XSLoader::load()` called with **no arguments**
  (perl 5.10+ infers the caller's package; PCL's shim does not) — the module
  there is `namespace::clean`. PCL dying exactly as perl-without-the-`.so`
  would is **deliberate and load-bearing** (it is what makes every dual-life
  module fall back to pure Perl); these dists simply have no artifact built via
  `tools/pcl-xs-install`. *There is no module called `XSLoader` to support —
  it is the XS boundary showing through.* The no-arg form losing the module
  name is a small honest defect worth fixing for diagnosis.
- **Role / method-modifier machinery — 11 files** (Class-Method-Modifiers 6,
  Role-Tiny 5), the biggest genuine *compiler/runtime* cluster:
  `FINALIZE-INHERITANCE was called on a forward referenced class` ×2,
  `The method 'orig' is not found in the inheritance hierarchy`,
  `MyRole1::pl-before_constant_deflate is undefined`,
  `Can't locate object method "apply_roles_to_package"`, and **`read error
  during load:` ×2** — that last one means the emitted CL does not even READ,
  which is task **#135**'s "proto (unreadable emission)".
- **Singles worth naming**: `Can't take sqrt of -171.125` (Data-Dump dd.t),
  `IO::Socket::UNIX::AF_UNIX is undefined` (glob.t),
  `main::pl-subtest is undefined` — Test::More's **`subtest` is not
  implemented** in PCL's TAP layer (Mojo-DOM58 collection.t).

## Honest summary

Of 183 files, **65 pass outright and 65 run partially**. The FAIL column is 41
real, and those 41 collapse to roughly **four causes plus a handful of
singles** — one Capture-Tiny bug, the role/method-modifier cluster, the XS
boundary, and `subtest`.

## Next step — DONE in s344

The 65 PARTIAL files are classified in **`docs/cpan-board14-partials-s344.md`**
(data: `docs/cpan-board14-partial-causes-s344.tsv`): 635 of the 674 rows, in
eight families, three of which carry 85% of them — Text-Balanced 300,
Sub-Uplevel/`caller` 127, Scalar-List-Utils 110. New tasks #232–#239.

**Correction to the method below, learned there:** the oracle command must NOT
pass `-I<dist>/lib` for an XS dist whose `.so` is unbuilt — perl then dies at
`use` and produces no TAP, and 21 real-failure files read as artifacts.

Reproduce:

```bash
perl tools/cpan-scoreboard.pl --jobs 6 --timeout 120 --tsv <out.tsv> \
  ~/.cpan/build/{Try-Tiny-0.32-0,Role-Tiny-2.002004-0,Sub-Uplevel-0.2800-0,\
Algorithm-Diff-1.201-0,Capture-Tiny-0.50-0,Class-Inspector-1.36-0,\
Class-Method-Modifiers-2.15-0,Data-Dump-1.25-0,File-Which-1.27-0,\
Mojo-DOM58-3.002-0,Safe-Isa-1.000010-0,Sort-Versions-1.62-0,Text-Balanced-2.07-0} \
  --no-dist-lib ~/.cpan/build/Scalar-List-Utils-1.70-0
```

**Always run the perl oracle beside it** — without it, 12 of 53 FAILs read as
PCL problems when perl does not run them either.
