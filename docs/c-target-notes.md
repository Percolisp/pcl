# A C backend for PCL's IR — the notes

**Status: notes, not a plan.**  Nobody is building this.  It exists because
writing it down is how we found out whether the IR is really target-neutral —
the JavaScript notes (`docs/js-target-plan.md` Part II) and these two are the
same tables with a different right-hand column, and every row that could not be
filled in for C was a place the IR was still describing SBCL rather than perl.
Written s470bt (Part B item B7 of `docs/plan-speed-and-ir-s470.md`); everything
asserted about perl's semantics comes from `docs/ir-spec.md` or a probe.
**§8 (s481, 2026-09-10) answers the USER question "C or C++ as a target, then
perl's own regex library etc for compatibility — hard or simple?": the target is
HARD (§8.4 adds the cost §1–§6 had not priced, string eval), the borrowing is
SIMPLE to do and the WRONG dependency (§8.2), probed and measured on this box.

**The acceptance bar is the same for every backend**: `tools/ir-conform
--backend ./my-backend` — 345 small programs with perl 5.40.3's stdout and exit
code recorded beside each (`ir-conform/README.md`).  A C backend is done when
it answers every case the way real perl does.

## 0. What you consume, and what it already tells you

| input | what it is | contract |
|---|---|---|
| the program | `pl2cl --emit-sexp FILE` — one top-level form per line, 7-bit ASCII, every symbol pipe-quoted | ir-spec §12b |
| the op vocabulary | 698 `p-*` / `%p-*` names in 54 families, with per-op arity, context sensitivity, coercions, magic, dies/dynamic/phase | ir-spec §10, §10a; `docs/ir-op-inventory.md` (+ `.tsv`) |
| the host vocabulary | 100 CL kernel names | ir-spec §11b |
| what THIS program demands | `pl2cl --manifest` JSON: `uses`, `needs`, `facts`, `depends` | ir-spec §10b |
| what the compiler PROVED | `pl2cl --facts` wraps a licensed form in `(p-fact (NAME …) FORM)`; declaration classes ride on `p-let` / `p-raw-params` / `p-sub` unconditionally | ir-spec §12c, §2b.2a |

The data form matters more here than anywhere else: a C consumer that had to
implement a Common Lisp reader (NFKC normalisation, case inversion, `#x`
radix literals, `#\Newline`) before it could see a single op would spend its
first week on the wrong problem.  `--emit-sexp` is five rules and a ~50-line
recursive-descent reader.

Three rules, the same three as every other backend:

* **DIE on an unknown head** (CLAUDE.md rule 12).  The vocabulary is closed
  and generated.
* **Do not re-derive front-end facts.**  Renames, capture analysis,
  my-shadowing, the sub/eval partition and the raw/boxed verdicts arrive
  applied.
* **Refuse per sub, loudly.**  `p-sub`'s `:needs` lists the obligations a sub
  uses, so a partial backend compiles what it implements and emits the
  ir-spec §9.3b refusal for the rest.

## 1. The value model, and the one decision C cannot borrow

| Perl / IR concept | C |
|---|---|
| undef | a tag in the value union — **not** a null pointer, and not CL `nil` (ir-spec §2.1: `nil` is not perl's false) |
| scalar value | a tagged union: `int64_t` IV, `double` NV, `char*`+len PV, exactly perl's own three, with perl's IV→NV promotion rules (§3.1) |
| scalar CELL | one heap `SV`-like struct; the tie/magic hook hangs here |
| array | a growable `SV**` — or a raw `int64_t[]`/`double[]` where the homogeneity fact says so (§2 below).  Negative indices, auto-extension, `$#a`, splice, holes-as-undef (§2.2) |
| hash | an open-addressing table with interned string keys; a constant-key hash can be a STRUCT (§2) |
| reference | a pointer plus a `kind` tag; identity IS the reference, so `\@a` twice gives two refs to one array |
| bless | a package-name field on the REFERENT, not on the reference — perl blesses the referent |
| code ref | a function pointer plus its captured environment (§3) |
| string | UTF-8 with a length cache, or UTF-32; perl strings are CODEPOINT sequences with a byte/char duality (§3.2), and the ≤0xFF-only "downgraded" case is worth a representation of its own |
| glob / filehandle | a struct over a `FILE*` or fd; bareword handles are NAMES (§7.5) |

**GC is the one decision the IR cannot make for you**, and it is the first one:

* **Perl-style refcounting (the recommendation).**  It keeps `DESTROY` timing
  identical to perl, which is observable in real programs (a file handle
  closed at scope exit, a lock released), and it is what pclxs's ABI expects
  if this backend should ever host XS.  Its cost is refcount traffic on every
  assignment — and that is exactly where the IR's class facts pay: most values
  never enter a refcounted cell at all (§2).
* **A tracing collector (Boehm).**  Simpler, faster for allocation-heavy code,
  and it changes destructor timing — which is a semantic change, not a
  tuning knob.  Take it only with eyes open.

The cycle problem is perl's too, and perl's answer (weak references, and
leaking otherwise) is the compatible one.

## 2. The facts, and what each buys in C

`docs/plan-speed-and-ir-s470.md` §B.3 is the source.  ✓ = PCL proves it today;
✗ = it would be new work in PCL (and would pay on the CL target too).  A
backend that ignores every row is still CORRECT — these decide only speed.
For C they decide more than for JavaScript, because a raw class is a C TYPE:

| fact, and where it is on the node | ✓ | what C does with it |
|---|---|---|
| scalar class (`p-let` `:class` — `:box :scalar :num :str :str-buffer :array :hash`) | ✓ | an `int64_t` / `double` / `char*` LOCAL instead of an SV: no allocation, no refcount traffic, and the C compiler can register-allocate it |
| numeric RANGE (fits int32 / int53 / int64) | ✗ | `int64_t` arithmetic with a single overflow check instead of perl's IV→NV promotion test on every operation |
| array facts: escapes / written-in-region | ✓ | stack or arena allocation for the array itself; `realloc` growth; no per-element SV |
| element HOMOGENEITY (all-fixnum / all-double / all-string) | ✗ | `int64_t[]` / `double[]` instead of `SV*[]` — the single biggest memory and cache win available, and it is not computed yet |
| `foreach-raw` (loop variable read-only) | ✓ | iterate by value; no alias pointer per iteration |
| hash key class (constant string / small int) | ✓ partly | a STRUCT with named fields for a constant-key "record" hash; interned keys and open addressing for the rest |
| sub facts (`p-sub` plist: `:returns`, `:insensitive`, no `goto`/`caller`/string-eval/`local`) | ✓ | a C function with a FIXED prototype: no `@_` array, no frame push, no wantarray parameter |
| parameter class (`p-raw-params`) | ✓ | positional C parameters of the class's C type |
| capture manifest (`:captured` / `:spanning`, `:why`) | ✓ | **closure conversion**: heap-allocate ONLY the captured cells and pass them in an environment struct; everything else stays on the stack.  This is the fact C needs most, and PCL already proves it |
| call-site facts (callee statically known, static context) | partly | a direct C call; a per-site inline cache for method calls |
| `tail-return` | ✓ | a plain `return` — no `longjmp` |
| `str-buffer` | ✓ | a growable byte buffer with amortised append instead of copy-on-concat |
| byte-only string content | ✗ | Latin-1 storage, and no UTF-8 decode on `length`/`substr` |
| regex TIER per literal + parsed flags | ✓ (`:tier`, ir-spec §10-tier) | PCRE2 with JIT for `:native` and `:pcre` alike, compiled ONCE at load from the structured literal; the loud refusal for `:refused` (§5) |
| dynamic-scope use per sub (`:needs`) | ✓ | no save/restore stack in a sub that never `local`s; magic globals as plain C globals |
| exception use per sub (`:needs`) | ✓ | **no `setjmp` frame** where nothing can throw — the biggest C-side cost avoided, because `setjmp` defeats register allocation across the call |
| phase facts (`BEGIN`/`INIT`/`END` present) | ✓ | run in order; no phase machinery when absent |

Three of these are ✗ and are Part A candidates for PCL itself: the numeric
RANGE proof, element HOMOGENEITY, and byte-only strings.  A C backend without
them is a working C backend; with them it is a fast one.

## 3. Control flow: `setjmp` only where `:needs` says so

| IR head | C |
|---|---|
| `p-if` / `p-while` / `p-until` / `p-for` | native, with the §3.4 truthiness test on the condition |
| `p-foreach`, `p-foreach-range`, `p-foreach-raw` | a counting or pointer loop; `-raw` needs no alias pointer |
| `last` / `next` + labels | `goto` where the exit is lexical.  It is NOT always lexical: perl lets a CALLED SUB exit its caller's loop, which needs the unwind path |
| `redo` | an inner loop around the body |
| `p-return` | a plain `return` when `tail-return` holds; the unwind path otherwise |
| `p-local-cell` | a save/restore stack, unwound on every exit from the scope |
| `p-try`, `p-eval-block` | the unwind path plus `$@` per §6.3 |
| `p-eval` (string) | the compiler as a subprocess — the same architecture the CL target uses (§9.1) |

And the host forms underneath (ir-spec §11b carries this table with both
columns):

| kernel group | C |
|---|---|
| `progn` `prog1` `let` `let*` | blocks and locals; `prog1` needs a temp |
| `lambda` `funcall` `apply` | a function pointer plus its captured environment; `apply` unpacks an argv |
| `multiple-value-bind` `values` | an out-parameter struct, or a small struct returned by value |
| `setf` and its places | the emitter uses `setf` only on `aref`, `gethash`, `p-aref`, `p-gethash` and a variable — five setters |
| `if` `cond` `case` `ecase` | `if` chains; `ecase`'s missing arm is an `abort()` naming the value (rule 12) |
| `and` `or` `not` | a temp plus a branch — they return the OPERAND, not an `int` |
| `block` `return-from` | a label and `goto` when lexical, `longjmp` when not |
| `catch` `throw` | `setjmp`/`longjmp` with the tag in the jump buffer; the tag is a VALUE compared by identity, not a static label |
| `tagbody` `go` | labels and `goto`, directly — the one place C is a BETTER target than JavaScript, which has to build a `switch (pc)` state machine because `go` jumps backward |
| `unwind-protect` | a cleanup label the unwinder runs |
| `defvar` `defparameter` `defconstant` | globals; `defvar`'s once-only rule needs an initialised flag |
| `define-symbol-macro` | an accessor macro |
| `defclass` `defmethod` `find-class` | a vtable per perl package, for the `@ISA`/C3 machinery (§7.3) |
| `in-package` / `*package*` | the current package is a RUN-TIME value (§7.1): a global with a save/restore stack |
| `make-hash-table` `gethash` | a string-keyed hash table; the `equal` test means structural string equality |

The unwind path is one mechanism, chosen once: `setjmp`/`longjmp` with a tag,
or an explicit unwind stack the caller checks.  Whichever it is, the `:needs`
fact keeps it out of the subs that cannot need it, which is most of them.

## 4. The op inventory, family by family

54 families, 698 names.  `docs/ir-op-inventory.md` is the port list and carries
each family's ir-spec §10 rule; this table says only what C owes.

| family (count) | C |
|---|---|
| numeric (9), numeric-compare (8), math (8) | libm plus perl's rules: numify per §3.1, `/` yields a double when inexact, `%` follows perl's sign rules, shifts truncate (Inf→0) and clamp a count ≥ the word size.  Compares return perl's `1`/`""`, not `int` |
| string (27), string-compare (7) | stringify per §3.2 and return a string.  The `%.15g`-equivalent number→string rule is perl's, not `printf("%g")`'s |
| bitwise (14), bit-string (2) | ONE mode decision per op: numeric iff an operand carries a number, else byte-by-byte over the stringified operands.  Overload hook first |
| logical (8) | short-circuit, returning the OPERAND |
| increment (4), compound-assignment (35) | read-modify-write on the cell or the raw local; the `-raw` twins are a plain assignment with the identical RHS.  `p-++` on a pure-alpha string is perl's magic string increment |
| assignment (11) | store per §2.2; a list assignment used as a VALUE yields the element COUNT in scalar/void and the LHS lvalues in list context |
| elements (22), slice-delete (4) | array/hash accessors; reads unbox scalars and keep reference cells, writes autovivify intermediates, an EMPTY slice answers undef / the empty list by context |
| aggregate-builtin (27) | the array/hash builtins.  `p-sort`'s default is STRING order and its comparator sees the `$a`/`$b` pair |
| box (24), reference (21), refaliasing (7) | cell construction and deref; refaliasing rebinds the CELL, so it goes through the cell and never the value |
| declaration (3) | the compiler's verdicts — droppable, at the cost of §2's whole table |
| context-frame (9) | the wantarray frame (§4): three-valued, and a call site binds it |
| control-flow (26), exception (12), dynamic-scope (18) | §3 |
| regex (5), compiled-regex (2) | §5.  A `qr` is an OBJECT with its own flags and identity that stringifies as `(?^flags:SOURCE)` |
| range (8) | a counting loop; the list form materialises |
| io (24), file-ops (19), directory-io (4), capture-io (3), filetest (29), socket (15) | libc and the POSIX calls, which is the one area where C is the EASIEST target: perl's I/O semantics are libc's, and the filetest family's `_` cache (§10c) is a `struct stat` plus a tag saying which call filled it |
| command-capture (1), process (11), env (3), time (6), user-db (11) | `fork`/`exec`/`waitpid`, `environ`, `time`, `getpwnam`.  Command capture is wantarray-sensitive: scalar = the whole stdout, list = split into `$/` records |
| oo (6), package-tracking (5), typeglob (12), introspection (15) | the package registry, the C3 MRO walk, `ref`/`can`/`isa`/`caller` |
| module-system (6), phase (8) | `%INC` and the section model; the phase order is the EMIT order (§12b) |
| sub-definition (5), signature (4), call (1) | §2's sub facts decide each sub's prototype |
| overload (6) | the hook the numeric/string/bitwise families consult FIRST |
| tie (8) | hooks on the cell; `tie %h` is not implemented on the CL target either (#155) |
| magic-global (136) | `$_`, `$0`, `@ARGV`, `%ENV`, `$@`, `$!`, the match state, the caret variables.  §8 says which are read-only, which op sets which, and which are per-package |
| pack (2) | `cl/pcl-pack` is `cl/pack-impl.pl` TRANSPILED — it compiles through this backend for free, as does `cl/pcl-mro` |
| tap (27) | Test::More's assertions; needed to run the test suites, not to run programs |
| UNCLASSIFIED (3) | the inventory names them; treat them like any unknown head — die |

**What is not in the kernel yet.**  ir-spec §11b's closing table lists the bare
CL functions still reaching the emission through a v1 seam — 31 distinct
symbols over the 111-file corpus at s470bo (`-` `+` `*` `1+` `rem` `truncate`
`logior` `logand` … `format` `intern`), owned by #1175/#1176.  All of them have
obvious C, so a C backend can simply implement them; `tools/ir-host-leak.pl`
prints which files contain which.

## 5. Regex: PCRE2, and where the tier verdict comes from

PCRE2 with the JIT is the engine, and every pattern literal carries its own
verdict: `pl2cl --emit-sexp` prints `(p-regex :pat "…" :flags "…" :tier …)`,
with `:tier` one of `:native` / `:pcre` / `:refused` (ir-spec §10-tier).  For C
the first two are the same engine, so the tier is not a routing decision here —
it is the REFUSAL boundary: `:refused` is `(?{…})` / `(??{…})`, perl code
inside a pattern, and the answer is the same loud perl-shaped die the CL target
gives (§9.3b), never a silent mismatch.

Compile each literal ONCE at load, from the structured form — that is what the
parsed `:pat`/`:flags` are for, and it is why the literal is structured at all.

PCRE2 is also the CL side's named "engine gap next" (cl-ppcre is ~2.1× perl's C
engine, measured s454ac), so the pattern-classification work is shared: one
"which constructs does this pattern use" classifier serves the tier verdict,
a PCRE2 path on either target, and the announce layer.

## 6. What C gets for free, and what it does not

**For free**, relative to the JavaScript target:

* `tagbody`/`go` is `goto`.
* I/O, processes, signals, `%ENV`, the filetest family: perl's semantics ARE
  libc's, and `sb-posix` in the CL runtime is a thin wrapper over the same
  calls.
* the integer model: `int64_t` is perl's IV exactly, so the IV/NV split needs
  no BigInt escape hatch and no range proof to be CORRECT (the range proof is
  a speed lever here, not a correctness one).
* pclxs: the XS bridge's whole point is a C ABI, so an XS module is closer on
  this target than on any other.

**Not for free:**

* GC (§1) — the only decision with no default.
* closures: the capture manifest tells you WHICH cells escape, but the
  conversion itself is yours (JavaScript gets closures from the host).
* strings: perl's codepoint/byte duality has to be built; the CL target gets
  codepoint strings from the host.
* the unwind path (§3), which the host gives both other targets.

## 7. Acceptance

```sh
tools/ir-conform --backend ./my-backend        # 345 cases, perl as the oracle
tools/ir-conform --backend ./my-backend 1xx    # one slice (substring filter)
```

The command is run as `CMD <case>.ir` in a fresh empty working directory, and
must print the program's stdout on its stdout and exit with the program's exit
code.  stderr is not compared — the interleaving of the two streams is
buffering-dependent, so comparing merged streams would bless a flake.

Slice the corpus by `pl2cl --manifest`'s `NEEDS` to get the subset a partial
backend owes: that is what the manifest is for.  PCL's own CL target scores
287 of 345 with 58 known bugs (`ir-conform/known-fail.tsv`, each with its task)
— the honest ceiling to measure against, not 345.

## 8. The libperl question (s481, 2026-09-10): borrowing perl's own libraries

USER asked: use C (or C++) as the target, then link perl's own regex library
"etc" for compatibility — would it be hard or simple?  Two questions, two
answers, both measured here rather than argued.

**A C backend is the hardest of the three targets** — everything §6 lists as
"not for free" plus the one obligation this note had not priced: string eval
has no host answer in C (§8.4).  **Calling perl's own libraries from C is
simple** — 60 lines, §8.1 — **and it is the wrong dependency**: it binds the
product to one perl BUILD's ABI and semantics, and every library past the
regex engine takes SVs, which are exactly what PCL's speed comes from not
having (§8.2).  The recommendation is §8.5.

### 8.1 The probe: what "simple" means

A C program that constructs an interpreter, compiles a pattern with perl's
own `pregcomp`, matches with `regexec_flags`, reads the captures out of the
`regexp` struct, then numifies with `grok_number_flags`/`my_atof` and
stringifies an NV the way perl does — against the DISTRO perl (5.40.1,
threaded, shared), compiled with the flags `ExtUtils::Embed` prints:

```c
#define PERL_NO_GET_CONTEXT
#include <EXTERN.h>
#include <perl.h>
static PerlInterpreter *my_perl;
/* PERL_SYS_INIT3; my_perl = perl_alloc(); perl_construct(my_perl);
   perl_parse(my_perl, NULL, 3, {"", "-e", "0"}, NULL); */
REGEXP *rx = pregcomp(newSVpvs("(\\w+)@(\\w+)\\.com"), 0);
SV *sv = newSVpvn(subj, len);                       /* pos()/$& magic wants an SV */
I32 r = regexec_flags(rx, subj, subj + len, subj, 0, sv, NULL, 0);
regexp *rxp = ReANY(rx);                            /* rxp->offs[i].start/.end, RX_NPARENS(rx) */
int f = grok_number_flags("  12.5e3xyz", 11, &uv, PERL_SCAN_TRAILING);   /* 0x44, trailing */
NV v = my_atof("  12.5e3xyz");                      /* 12500 */
SvPV_nolen(newSVnv(0.1 + 0.2));                     /* "0.3" — perl's %.15g rule, not printf's */
```

```
gcc -O2 embed-re.c $(/usr/bin/perl -MExtUtils::Embed -e ccopts) \
    $(/usr/bin/perl -MExtUtils::Embed -e ldopts | sed 's|-lperl|/usr/lib/x86_64-linux-gnu/libperl.so.5.40|')
```

Everything answered the way perl does (captures `alice@example.com` /
`alice` / `example`; the numifier reports the trailing garbage; `0.1+0.2`
prints `0.3`).  The link line needed the versioned soname because
`libperl-dev` — the unversioned `libperl.so` — is not installed here; the
headers are (the `perl` package ships `CORE/` for XS builds).

| same loop, 3 000 000 matches of `(\w+)@(\w+)\.com` over a 40-byte subject | time | per match |
|---|---|---|
| C calling perl's engine through libperl (this probe) | 0.422 s | **141 ns** |
| perl 5.40.1 (distro) `for (1..$n) { $c++ if $s =~ /…/ }` | 0.54 s | 180 ns incl. the loop |
| perl 5.40.3 (perlbrew, the dev perl) | 0.51 s | 170 ns |
| PCL (cl-ppcre), differenced between n = 3 M and 6 M so transpile + startup drop out | 2.78 s per 3 M | **927 ns** = 5.2× perl on this shape |

An embedded interpreter costs nothing visible at start (0.00 s wall for the
n = 1 run) and 5.7 MB RSS; the binary is 16.8 kB.  So: the engine is
reachable, its answer is perl's by construction, and the boundary is cheaper
than perl's own op loop.  That is the whole of the "simple" half.

### 8.2 Why it is the wrong dependency — four facts from this box

1. **The ABI is per perl BUILD, not per perl version.**  This machine has two
   perl 5.40s.  The distro one is threaded and shared: every API call carries
   the interpreter pointer (`aTHX_`), and `libperl.so.5.40` exists.  The
   perlbrew one that PCL develops against is non-threaded and **static**
   (`useshrplib='false'`, `libperl.a` only): there is nothing to link a
   runtime against at all, and a struct layout compiled for one does not
   match the other.  XS modules are rebuilt per perl for exactly this
   reason; a PCL runtime linked to libperl inherits the rebuild-per-perl
   rule for the RUNTIME ITSELF.  This is strictly worse than the PCRE2
   version spread (10.39 → 10.46) that parked #71 (DECIDED §s476), and the
   s474 portability rule (install matrix + a macOS leg green, and macOS's
   perl is another build again) applies to it in full.
2. **Semantics follow the INSTALLED perl.**  The engine's answers are those
   of whatever perl the machine has — 5.38 and 5.40 disagree on `re_tests`
   rows — which is "behaviour that varies by OS", the s473q objection to a
   PCRE2 floor, now for every regex in every program.
3. **The SV data model is the price of the "etc".**  The regex engine is the
   ONE library whose subject is a plain buffer (and even it wants an SV for
   the pattern and one for `pos()`/`$&` magic, inside a constructed
   interpreter).  Everything else the ask reaches for takes `SV*`: sprintf
   (`sv_vcatpvfn_flags`), pack/unpack (`packlist`/`unpackstring`), `sort`'s
   comparators (`sv_cmp`), `do_sprintf`, the numeric-string cache.  Building
   on them means PCL's values BECOME SVs — and that is B::CC, the perl 5
   perl-to-C compiler that emitted C against the perl runtime: 1–2× at best,
   dead since the 5.2x series.  The README rows where PCL is 3–5× FASTER
   than perl (`collatz` 0.18×, `cfor` 0.24×, `arith` 0.25×, `feread` 0.29×)
   are precisely the rows with no SV anywhere: raw locals, raw arrays, the
   §2 facts.  Adopting SVs for the libraries forfeits the speed target to
   buy fidelity that perl-as-oracle already gives for free.
4. **`(?{ … })` stays refused either way.**  An embedded perl would compile a
   code block, but against ITS pad, not PCL's lexicals — the same refusal
   boundary PCRE2 callouts have (s473q), with less control over it.

### 8.3 The same engine from the CL target — and why that is not #71's answer

The shape exists and is proven: the PCRE2 spike's `%pcl-build-scanner` seam
(one function returning a closure with cl-ppcre's `(scanner function)`
protocol, `sb-alien`, no CFFI).  Perl's engine could sit behind the same
seam — and would pay what PCRE2-32 did not: perl's engine wants **UTF-8
bytes** with the SV's UTF8 flag, an SBCL string is 32-bit code points, so a
non-ASCII subject is encoded per match and every capture offset comes back
as a BYTE offset to map to a character index — the "budgeted hard part" that
the `libpcre2-32` design (DECIDED §s474) deleted, back again.  Plus a live
interpreter inside the SBCL image (`PERL_SYS_INIT3` wants argv/env, perl's
signal and locale initialisation beside SBCL's, 5.7 MB), fact 1's ABI
problem (the dev perl on this very box could not host it), the one-engine
rule, and a sweep whose verdicts would depend on which perl is installed.
Parity would be 100 % of the installed perl by construction; portability is
below PCRE2's, which was the parking reason.  Verdict: a possible future with
#71's three re-open triggers (`docs/faster-codegen-suggestions.md` §8) and a
fourth that does not exist today — a shared, build-stable libperl on every
supported OS.

### 8.4 The string-eval crux: the cost §1–§6 had not priced

`eval $string` compiles Perl at run time, and it is a HARD REQUIREMENT
(USER 2026-07-07; ir-spec §9.1 is the protocol).  The CL target answers it
with SBCL's in-image `compile`; the JavaScript target with `new Function`
behind a subprocess compiler (js-target-plan II.8 item 4).  **C has no host
compiler**, so a C backend must choose one of:

| answer | what it costs |
|---|---|
| (i) run `cc` at run time and `dlopen` the result | a C toolchain on every machine that RUNS a PCL program — perl users do not have one; two-step failure modes at run time |
| (ii) embed `libtcc` | a foreign library = the s474 platform rule (matrix + macOS); tcc's code is several times slower than gcc's, so eval'd code runs in a different speed class; two code generators to keep in agreement |
| (iii) an IR interpreter inside the C runtime | the only portable answer, and a SECOND execution engine — two semantics for one program, the s474 one-engine argument generalised; a project the size of the backend's own op inventory |

None is free; (iii) is the one that ships, and it is what makes C the
largest of the three targets rather than merely the least hosted.

How much code needs it — the manifest census, this session, `pl2cl
--manifest --module` over the 112 shipped `lib/` shims and CPAN board
modules: **10 files carry a string eval (9 %)** — and they are
`Test::More`, `Test::Builder`, `Test2::API`, `Test2::API::Instance`,
`Text::CSV`, `Text::CSV_PP` and YAML::Tiny's test libraries.  A C target
without string eval runs no CPAN test suite at all.  (Same census, the other
obligations: `BEGIN` 51 files, `local` 21, `goto` 8, `tie` 6, `overload` 3,
formats 1; regex tiers native 289 / dynamic 36 / pcre 0 / refused 0.)

### 8.5 Sizing and verdict

* **What a C backend re-owes.**  The host-supplied parts of the 28 303-line
  runtime — GC, closures, dynamic binding, the unwind path, bignum and float
  printing, CLOS dispatch, code-point strings, the 534 `sb-*` references
  that are libc/POSIX in disguise — plus §8.4's engine, plus the 698-op
  vocabulary (§4), on the same M0–M3 ladder as the JavaScript target
  (js-target-plan II.6) with C's §6 extras.  Larger than the JS target,
  which is itself deferred until the IR is quiet (USER s448/s460f).  Not
  sized in sessions: nothing on the queue depends on it.
* **The slow rows are not the target language.**  SBCL emits native code and
  the loop rows already beat perl 3–5×.  Where PCL is slower it is the
  regex engine (`regexg` 2.07×; 5.2× on §8.1's capture shape) and OO
  dispatch/overload/Moo (`methret` 1.06× … `moo-objs` 32.3×) — runtime
  DESIGN, which a C rewrite would carry over unchanged.  The engine has its
  own parked lever (#71); the OO rows have theirs (#582's inherited half,
  #1546, parked by the USER 2026-09-09).
* **C vs C++.**  If it were built: C++ — RAII for the refcounts §1
  recommends, table-driven zero-cost exceptions for §3's unwind path instead
  of `setjmp`, lambdas for the closure conversion.  It changes nothing in
  §8.2 or §8.4; pclxs's ABI is C either way.
* **Recommendation.**  No C target now, and no libperl dependency in the
  runtime at any point: the portability rule forbids it and the SV model
  forfeits Target A.  "Use perl itself for compatibility" already lives where
  it costs nothing — perl is the ORACLE of every measurement (the sweep,
  `ir-conform`, `re_tests`, the board), and that is the right seat for it.
  Re-open a C backend on the JS target's trigger (a quiet IR, or an explicit
  ask); re-open an engine swap on #71's triggers, with PCRE2 ahead of libperl
  on every axis but parity.
