# `caller()` — the call-frame stack

How PCL answers `caller`, what each field holds, what the frame costs, and
what is left out and why.  Rewritten s539d (task #233 tier 1 + #1361; the
USER's ruling of s539: make `caller` better where that is cheap in execution
time and code size).  The normative statement is `docs/ir-spec.md` §7.3c; the
measurements are in the s539d section of `docs/session-log.md`.

---

## What perl does

perl keeps one context-stack entry per sub call: the CV, the context the
call was made in (`gimme`), and `blk_oldcop` — the statement that was
running when the call was made.  `caller(N)` reads the Nth entry down:

| field | perl |
|---|---|
| 0 package | the package the CALL was compiled in |
| 1 file, 2 line | the call site (`blk_oldcop`), after any `# line` directive |
| 3 subname | the called sub's name; `PKG::__ANON__` for an anonymous sub; `(eval)` for an eval frame |
| 4 hasargs | 1 for a call with an argument list, '' for `&name;` |
| 5 wantarray | 1 list, '' scalar, undef void |
| 6 evaltext, 7 is_require | an eval / require frame's |
| 8-10 hints, bitmask, hinthash | the call site's pragmas |

`caller` with no argument is the first three fields of `caller(0)`; past
the outermost frame it is the empty list.

## What PCL keeps: ONE frame vector

`cl/pcl-runtime.lisp`, section "THE CALL-FRAME STACK":

```lisp
(sb-ext:defglobal *p-frames* (make-array 320 :initial-element 0))
;; frame D (1 = the outermost sub call) is slots 5D .. 5D+4:
;;   +0 the CALLER's package    *pcl-current-package* at the call
;;   +1 the frame's sub         a named sub's "Pkg::name", or an anonymous sub's FUNCTION
;;   +2 the call site's file id } the location registers (ir-spec §6.3b)
;;   +3 the call site's line    } as they stood at the call
;;   +4 the frame's context     *wantarray* at the call
```

Every frame binds `*pcl-sub-call-depth*` to one more than its caller's and
fills its five slots (`p-frame-bind` → `%p-frame-enter`, inline).  Nothing is
pushed or popped: the depth is a dynamic binding, so a non-local exit
unwinds it and the slots above it are simply dead.  `caller(N)` reads frame
`depth - N`.

This REPLACES the two consed stacks (`*pcl-caller-pkg-stack*`,
`*pcl-caller-subname-stack*`: two conses and two dynamic bindings per call)
and the `sb-debug` backtrace walk `caller` used for its file and line (which
answered the generated `.lisp` file and a Lisp form offset).

* **Why a vector, one per process.**  A slot store costs no allocation and
  no binding-stack traffic.  Perl code runs only on the main thread; the
  signal relay thread INTERRUPTS main to run a handler, which pushes its own
  frame one above whatever main had bound.  A frame binds its depth before it
  stores, so an interrupt between the two writes one frame higher.
* **Why five slots, the location unpacked.**  The two location slots are
  fixnums, so their stores take no GC card mark.  A packed `(file << 32) |
  line` fixnum went through the generic (bignum) arithmetic path.
* **Why `%p-frame-enter` is inline at safety 0.**  It is expanded into every
  sub, so its size is paid per sub in every fasl.  At the default safety it
  was +232 bytes per sub (Math::BigInt::Calc's fasl +1.2 %); as a full call
  it was 1-5 ns slower per call.  The bounds are the explicit length test,
  which also grows the vector.

### Who pushes a frame

* every NAMED sub (`p-sub`);
* an ANONYMOUS sub whose body reads a frame — it calls the `caller` builtin
  or compiles perl at run time (a string eval and its kin) — marked
  `(p-anon-sub "PKG" :frame (lambda …))` by the compiler
  (`Pl::Parser2::anon_reads_frames`, the one predicate both emitters ask).
  Its frame names the function itself, so `(caller(0))[3]` is the name it
  carries NOW: a `Sub::Util::set_subname` name (`%pcl-sub-names`), else
  `PKG::__ANON__`.  The function reaches itself through a `labels` name —
  free per call, and the identity rules of #1083 are unchanged.
* NOT `eval {}`, `eval STRING`, `require`, `sort` blocks, `map`/`grep`
  blocks — perl has frames for the first three, PCL does not (see below).

`goto &sub` replaces the frame: the depth goes one down, and
`*pcl-current-package*` and the location registers go back to what the
replaced frame recorded of its call, so the target's frame records the
ORIGINAL call.

hasargs is not a slot: `&name;` (`p-amp-call`, which shares the caller's
`@_`) pushes the depth of the frame it makes onto `*p-amp-frames*`, and
`caller` answers `''` for exactly those depths -- the rare spelling pays a
cons and a binding, an ordinary call nothing.

## The location: recorded in every build (s543a)

A frame stores the location REGISTERS as they stood at the call.  The
registers are written by the `line-track` emission (`(p-line N)` per
statement, `(p-file …)` per file run bucket and sub body; ir-spec §6.3b),
which is **ON BY DEFAULT since s543a** (the USER's ruling of s543; until
then it was off, s473c-s542) and kept by `PCL_OPT=none`.  So `caller`'s file
and line are perl's — the call site, through `goto &sub`, a string eval
(`(eval N)`), an anonymous sub, a `# line` directive — and so are Carp's
`at FILE line N.` and a runtime-raised die's location.

* `PCL_OPT=-line-track` is the A/B switch: no register is written, a frame
  answers file `-` line 0 (or `(eval N)` line 0 from a string eval's code,
  whose file id `p-eval` sets in every build), Carp says `at - line 0.` and
  a runtime die `at (eval 0) line 0.`.  Line 0 is never a real line.
* File id 0 is reserved for `-`: a register nobody wrote names no file.
* Not yet perl's (not-supported `ns-caller-fidelity`): a call from a BEGIN /
  END / CHECK / INIT body or an `import` reached by `use` (those bodies are
  v1 text and write no location, #5101), from a sub defined by a string
  eval (#1360), an `elsif` condition (#5102).

### What it costs (measured s543a, base c5f618f2 vs the s543a tree)

**Time.**  The full bench (`tools/bench-exec.pl`, 77 rows, best of 5,
interleaved; each side transpiles with its own `pl2cl` and runs its own
runtime, `BENCH_EMIT_B=1`), base against the tree, after the same bench base
against base for the noise band; each run one exclusive leg on a quiet box
(2026-11-21, load 0.7-1.8; a desktop browser at ~13 % CPU in both runs).
Logs: `scratch/s543a/bench-noise.log`, `bench-tree.log` in the s543a
worktree; `scratch/s543a/bench-cmp.pl` reads them.

* The noise band (base against base): rows under 0.1 s -6.8 .. +9.6 %, rows
  of 0.1 s and more -3.8 .. +4.9 %.  All 77 rows: median +0.8 %, geomean
  +1.1 % (the noise run's geomean +0.6 %).
* The counting loops are slower in every row, but inside the short rows'
  band: intloop+= +6.4 %, intloop= +4.9 %, cfor +4.1 %, arith +3.1 % (s539d
  expected +4..6 %).
* Every row OUTSIDE the band:

| row | base s | tree s | cost | what it is |
|---|---|---|---|---|
| feread | 0.1047 | 0.1226 | +17.1 % | `for my $x (@a) { $s += $x }`: one `(p-line N)` per iteration of a ~3.5 ns body, ~0.6 ns a store (#5103's same-line case) |
| feread2 | 0.1099 | 0.1272 | +15.7 % | the same over `(@a, @b)` |
| feread3 | 0.1139 | 0.1319 | +15.8 % | the same over `(@a, @b, @c)` |
| emptysub | 0.3687 | 0.3984 | +8.1 % | two calls an iteration: a `(p-file …)` in each callee, the caller's stores; ~15 ns an iteration, more than the stores account for: not attributed |
| collatz | 0.3199 | 0.3428 | +7.2 % | two stores per iteration of the inner `while` |
| methinh | 0.3288 | 0.3458 | +5.2 % | a method call an iteration: `(p-file …)` + `(p-line N)` in the callee; not attributed beyond that |
| accumread | 0.0100 | 0.0086 | -14.0 % | FASTER, a 10 ms row: noise |
| symref | 0.0159 | 0.0148 | -6.9 % | FASTER, a 16 ms row: noise |
| blessnew | 0.4348 | 0.4104 | -5.6 % | FASTER with stores added: noise |
| splitstrre | 0.1967 | 0.1876 | -4.6 % | FASTER: noise |
| blessdes | 0.5445 | 0.5217 | -4.2 % | FASTER: noise |

  The five faster rows have no emission cause (the flip only adds stores):
  they say the long rows' real run-to-run spread is about ±6 %, wider than
  the one-sample band.  So emptysub and methinh lie near the spread's edge;
  feread* (+16..17 %) and collatz lie well outside it.

**Size** (the s543a tree; `-line-track` gives the base's text exactly):

| what | base | tree | |
|---|---|---|---|
| `./pl2cl --module lib/Math/BigInt/Calc.pm`, bytes | 217,379 | 256,971 | +18.2 % |
| the same module's fasl, bytes | 1,222,379 | 1,271,879 | +4.05 % |
| the saved runtime core, bytes | 63,642,632 | 63,675,416 | +0.05 % |
| emitted text over the 2,053-file wide list | | | +11.1 % (corpus +14.1, everyday +7.5, lib +11.4, the board's 402 files +10.0, core lib +10.0, perl's t/ +12.6) |

**First run** (transpile + compile + run, COLD: the cache's `modules/`,
`scripts/`, `evals/`, `proto/` emptied before each run; the runtime core
and the compiled extensions kept; base and tree interleaved, best of 3;
`scratch/s543a/legs/firstrun.sh`, `firstrun.log`):

| program | lines | base s | tree s | |
|---|---|---|---|---|
| `everyday/programs/real-log-parser.pl` | 40 | 0.462 | 0.471 | +1.9 % |
| `perl-tests/hashassign.t` | 533 | 6.648 | 6.669 | +0.3 % |
| `perl-tests/signatures.t` | 1,742 | 14.190 | 14.181 | -0.1 % (each side's runs spread 14.2-16.3) |

The two test files exit 1 and 255 on both sides: their blessed failing
rows, not a crash.  At these sizes the transpile and the compile dominate,
and the extra text (+7.5 % on the everyday programs, +14.1 % on the
corpus) does not show beyond the runs' spread.

**One `cl/` shape match the flip defeated, FIXED in s543a (#5107)**: `p-sub`
recognised an empty `DESTROY` / `AUTOLOAD` by its exact body
`(p-args-body (block nil (p-return-empty)))`; the leading `(p-file …)` hid
it, so `sub DESTROY {}` opened the global-destruction gate (+28 ns every
bless, the empty sub called once per live object at exit; output
unchanged) until `%p-empty-sub-body-p` read the body through its leading
location stores -- measured with a `*macroexpand-hook*`, the two other
body-shape matches (`%p-let-presize`, `p-void-ctx`'s sole `eval {}`) fire as
on the base.  The strip comparison below proves the emitted TEXT; a macro
that matches its argument's shape runs after it.  Six of the bench rows
re-run after the fix (`bench-fix.log`; none holds an empty `DESTROY`, so the
fix cannot move them): emptysub +3.6 %, methinh +1.9 % -- inside the band,
so their first numbers were the runs' spread -- and blessnew -5.2 %,
blessdes -4.6 %, faster again (reproducible; cause not found).

In the emitted TEXT the flip defeated one optimisation, found by stripping the location forms
from the default emission and comparing it with `PCL_OPT=-line-track` over
2,053 files (35 differed): the classic-sort pass's comparator match
(`(p-scalar-ctx (p-line N) (p-<=> $a $b))` is not the classic shape, so 26
files fell back to the general `p-sort` with a store per comparison) --
fixed in `Pl/ClassicSort.pm`.  The other 11 are the 20,000-character
inlining cap (`(locally (declare (notinline …)))`) that the extra text pushes
a few huge top-level forms over: cold code that runs once, left as it is.
No file moved across the 64,000-character refusal gate.

What was measured before the flip (s473c, s539d): one store ~0.25 ns, one
cycle per statement; +4..6 % on the counting-loop micro rows, nothing on the
macro rows; a narrower emission (a store only before a statement that can
make a call) still writes one form per call site (~280 in Calc.pm, >= +1.5 %
text).  The narrowing that is left is the PERF task #5103: a store before a
statement made only of raw forms that cannot raise is dead, and so is a
store of the line the register already holds.

## `# line N "FILE"`

PPI applies perlsyn's directive rule while it indexes locations
(`PPI::Document::_logical_line_and_file`); `Pl::Environment::src_location`
is the ONE reading of a node's location after any directive, used for
`__LINE__`, `__FILE__` and an explicit `die`/`warn`'s `:loc` (every build),
and for the `line-track` constants (which also write the file register when
a directive changes the file).  No run-time cost: the constants change.

## Carp (#1361)

`lib/Carp.pm` is perl's Carp 1.54 algorithm over this `caller`:
`short_error_loc` (the first call whose caller does not trust the called
package: `%CarpInternal`, `%Internal`, `@CARP_NOT` / `@ISA`, `$CarpLevel`),
`long_error_loc`, `ret_summary`, `ret_backtrace`.  So croak / carp report
their caller's location as perl does, in the default build since s543a
(`at - line 0.` under the A/B switch `PCL_OPT=-line-track`: perl's message
SHAPE, which a test matching `\s+at` or normalising ` at \S+ line \d+`
reads).  A backtrace line
names the sub without its arguments (`@DB::args` is not kept), and there is
no `, <FH> line N` tail.

## What is not kept, and why

| item | why | cost if added |
|---|---|---|
| a frame for every anonymous sub | +3.5 ns on a 28 ns anonymous call (+12 %), the anoncall row +4 % | measured, left out |
| `eval {}` / `eval STRING` / `require` frames (`(eval)`, evaltext, is_require) | each would be the same prologue as an anonymous frame, on every eval execution | ~+3.5 ns per eval (the anonymous frame's number) |
| hints / bitmask / hinthash (fields 8-10) | PCL keeps no per-frame pragma state | — |
| `@DB::args`, `CORE::GLOBAL::caller`, frames `Sub::Uplevel` hides | tier 2, a design (docs/not-supported.md ns-caller-fidelity) | — |

## What pclxs inherits

XS asks `caller` too (Params::Validate's `get_caller` reads the package and
sub name for an error message).  When pclxs forwards that question it gets
this file's answers: the package and the sub name are right; the file and
line are perl's too (s543a).
