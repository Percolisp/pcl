# Low-hanging speed fruit in the rest of PCL (s540b, 2026-11-13)

The USER's ask (s540, task #4788): review the rest of PCL -- the emitted code, the
runtime's hot helpers, the shims, the compile pipeline -- for low-hanging speed
fruit.  Its sibling #4787 (s540a, `docs/cpan-speed-comparison-s540.md`) ranks by
what the CPAN test suites spend; this one is a READING review backed by small
measurements.  **Nothing under `Pl/`, `cl/` or `lib/` changed**: every suspicion
was measured on a scratch copy (a redefinition loaded on top of the runtime, a
hand-edited emitted file, or a copy of a shim's body), and every lever that
passed the filter below is filed as a task.

Measured on main's CODE sha **`05806e16`** (MAIN-READY-91, generation
**v2-13900**).  Scratch: `scratch/s540b/` in the s540b worktree (not on main; the
tables below carry the numbers).

## The method in ten lines

1. Profiled nine programs with `sb-sprof` (`:cpu`): the bench rows `json-rt`, `moo-objs`,
   `textproc`, and six ordinary programs -- Data::Dumper of 300 records (`dumper`),
   Pod::Text over perlfunc.pod (`podtext`), Text::Balanced (`tbal`), a word count
   (`wordfreq`), a PPI parse (`ppi`), and four shim loops.
2. Transpiled the everyday corpus (103 programs) and seven modules of different character:
   Text::Balanced (string-heavy), Pod::Simple + BlackBox + Pod::Text (OO, inheritance),
   JSON::PP (data structures), PPI::Tokenizer (a parser), Math::BigInt::Calc.
3. Counted emitted shapes over that text with a perl scan; measured each shape on ONE
   hand-edited emitted file (`ab.pl --emit`: interleaved best of 7, a base-vs-base column).
4. Ran every registry name `PCL_OPT=-NAME` over the 75 bench rows: first which rows' emission
   changes at all (`optdiff.pl`, pure transpiles), then the timing of those rows (`optab.pl`).
5. Runtime suspicions: a redefinition loaded as a fasl in front of the unchanged program
   (`ab.pl --lever`), interleaved best of 5 with the base-vs-base band C/A beside it.
6. Shims: `shimbench.pl` -- per-call time of 35 exports on ordinary inputs under perl (XS),
   under perl running PCL's own shim text where it loads, and under PCL.
7. Compile pipeline: NYTProf (subs only) of a Calc.pm transpile, then the SBCL side per
   unit: `compile-file` time, fasl bytes, fasl load in a fresh image, with and without the
   #2610 detector table, at the default policy and a cheap one.
8. Start-up: the one number, re-measured; not designed (parked, #2422 / #1862 / #2421).
9. Timings ran as `heavy.sh s540b bench` legs (exclusive, load < 2 at start, each < 15 min):
   `legs/leg1.log` .. `legs/leg4.log`.  A gain inside 2x its row's C/A band is "not measurable".
10. "Low-hanging" = one session at most, no representation change, no ruling, correctness
   unchanged by construction or held by an existing guard, a measured gain on ordinary code.

## Part 1 -- the emitted code

Population: the everyday corpus (103 programs transpiled; 10,606 emitted lines, 0.44 MB) and seven
modules (24,363 lines, 1.2 MB).  Counts are a perl scan over the emitted text; each measured shape is ONE
hand-edited copy of one emitted file, run against the unedited file (`ab.pl --emit`, best of 7,
`legs/leg1.log`, load 1.1-1.4, base-vs-base C/A within 1.6 %).

| # | shape | count (what the scan matched) | measurement | verdict |
|---|---|---|---|---|
| P1 | a literal `use constant` used as a CALL: `(p-scalar-ctx (pl-DEBUG))` | 33 literal constant subs, 45 call sites in the sample (42 in JSON::PP); 1,613 `use constant` declarations in 321 of 3,797 .pm files of perl's lib + the board dists | the call replaced by its literal, 3 M iterations of `DEBUG > 2 and print`: 0.116 -> 0.035 s, **-70.2 %** (perl 0.04 s) | **LEVER** (THE LIST #4, task #4982) |
| P2 | a constant sub installed by a glob in BEGIN (`*DEBUG = sub () {0} unless defined &DEBUG`, Pod::Simple's idiom) | 199 `(p-scalar-ctx (...::pl-DEBUG))` in Pod::Simple + BlackBox | the same loop with a `use constant` sub instead: **-26.9 %** -- the anonymous sub's frame (`p-flatten-args` + three special bindings + `p-sub-frame`) | not foldable at transpile time (a BEGIN effect); the cost is the FRAME -> s539d's subject, not filed here |
| P3 | the context bind around a call the compiler cannot prove insensitive (codegen review C4) | `p-scalar-ctx` 296 / 1,117, `p-list-ctx` 206 / 179, `p-void-ctx` 98 / 258, `p-caller-ctx` 28 / 230 (everyday / modules); 296 of the modules' 1,117 scalar binds wrap a method call | the void bind removed around `f($i)` (f's tail is a call), 3 M calls: **-2.2 %**, ~4 ns a bind | **REFUTED as a speed lever** -- below the bar; C4 stays an IR-clarity item at most |
| P4 | a scalar lexical kept in a box | 120 of 370 everyday names raw (32 %), 417 of 1,308 module names (32 %); sole reasons of the boxed: `write-list` 57 / 104 (`my ($a, $b) = ...`), `write-shape` 46 / 256 (the value can be a reference or a box), `handle-viv-arg` 32 everyday, `eval-in-region` 50 modules; 67 / 343 have several reasons (PCL_B_DEBUG over both populations) | not measured as one edit -- a raw slot cannot hold a REFERENCE in today's representation, and that is the largest class (`$self`, `$r`, `$ar`, `$o`) | **not low-hanging**: a representation decision (ir-spec §2.2's "a raw slot holds a host number or string, never a box"); `%make-p-box` + `box-set` are 3-17 % of every profile in part 2 |
| P5 | a method call resolved by NAME per call | `p-method-call` with a literal name: 775 in the modules, 145 everyday | s534a measured it (inherited calls 3.9-7.9x perl, own-class 1.2x) | already designed: #4523-#4525, awaiting the USER |
| P6 | `p-funcall-ref` where the callee could be known | 27 sites in the whole sample | -- | too rare to matter |
| P7 | `p-setf` (the generic store) | 348 in the modules | the codegen review: one store in thirteen, the tail of element / deref / slice stores | rides on #3694's step 1d (C1 + C2), not a lever of its own |

**What each EXISTING transform holds** (`optdiff.pl`: which of the 75 bench rows' emission changes
under `PCL_OPT=-NAME`; then `optab.pl`: those rows timed default vs `-NAME`, startup subtracted, best of 3):
