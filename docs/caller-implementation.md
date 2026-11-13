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

## The location: only where it is recorded

A frame stores the location REGISTERS as they stood at the call.  The
registers are written by the `line-track` emission (`(p-line N)` per
statement, `(p-file …)` per file run bucket and sub body; ir-spec §6.3b),
which is **DEFAULT OFF**.  So:

* compiled with `PCL_OPT=line-track`: `caller`'s file and line are perl's —
  the call site, through `goto &sub`, a string eval (`(eval N)`), an
  anonymous sub, a `# line` directive;
* in the default build no register is written: a frame answers file `-`,
  line 0 (or `(eval N)` line 0 for a call made from a string eval's code,
  whose file id `p-eval` sets in every build).  Line 0 is never a real
  line; it says "not recorded".

Recording the location by default is NOT cheap, and is left out (measured
s539d): `line-track` on is +17.9 % emitted text on Math::BigInt::Calc
(221,629 → 261,201 bytes) and +4..6 % on the counting-loop bench rows
(s473c).  A narrower emission — a line store only before a statement that
can make a call — still writes one form per call site: Calc.pm has ~280
of them, ≥ +1.5 % text against a +0.5 % bar.

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
their caller's location as perl does — exactly in a `line-track` build,
`at - line 0.` in the default build (perl's message SHAPE, which a test
matching `\s+at` or normalising ` at \S+ line \d+` reads).  A backtrace line
names the sub without its arguments (`@DB::args` is not kept), and there is
no `, <FH> line N` tail.

## What is not kept, and why

| item | why | cost if added |
|---|---|---|
| file / line in the default build | needs a per-statement or per-call-site register write | text +17.9 % (all statements) / ≥ +1.5 % (call sites) |
| a frame for every anonymous sub | +3.5 ns on a 28 ns anonymous call (+12 %), the anoncall row +4 % | measured, left out |
| `eval {}` / `eval STRING` / `require` frames (`(eval)`, evaltext, is_require) | each would be the same prologue as an anonymous frame, on every eval execution | ~+3.5 ns per eval (the anonymous frame's number) |
| hasargs '' for `&name;` | the frame does not record how it was called | one more slot store |
| hints / bitmask / hinthash (fields 8-10) | PCL keeps no per-frame pragma state | — |
| `@DB::args`, `CORE::GLOBAL::caller`, frames `Sub::Uplevel` hides | tier 2, a design (docs/not-supported.md ns-caller-fidelity) | — |

## What pclxs inherits

XS asks `caller` too (Params::Validate's `get_caller` reads the package and
sub name for an error message).  When pclxs forwards that question it gets
this file's answers: the package and the sub name are right; the file and
line are perl's only in a `line-track` build.
