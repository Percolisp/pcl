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
   `legs/leg1.log` .. `legs/leg5.log` (leg6: the `:alloc` profiles and the #4987 manifest diff, a bounded leg).  A gain inside 2x its row's C/A band is "not measurable".
10. "Low-hanging" = one session at most, no representation change, no ruling, correctness
   unchanged by construction or held by an existing guard, a measured gain on ordinary code.

## Part 1 -- the emitted code

Population: the everyday corpus (103 programs transpiled; 10,606 emitted lines, 0.44 MB) and seven
modules (24,363 lines, 1.2 MB).  Counts are a perl scan over the emitted text; each measured shape is ONE
hand-edited copy of one emitted file, run against the unedited file (`ab.pl --emit`, best of 7,
`legs/leg1.log`, load 1.1-1.4, base-vs-base C/A within 1.6 %).

| # | shape | count (what the scan matched) | measurement | verdict |
|---|---|---|---|---|
| P1 | a literal `use constant` used as a CALL: `(p-scalar-ctx (pl-DEBUG))` | 33 literal constant subs, 45 call sites in the sample (42 in JSON::PP); 1,613 `use constant` declarations in 321 of 3,797 .pm files of perl's lib + the board dists | the call replaced by its literal, 3 M iterations of `DEBUG > 2 and print`: 0.116 -> 0.035 s, **-70.2 %** (perl 0.04 s) | **LEVER** (THE LIST #4, #4982) |
| P2 | a constant sub installed by a glob in BEGIN (`*DEBUG = sub () {0} unless defined &DEBUG`, Pod::Simple's idiom) | 199 `(p-scalar-ctx (...::pl-DEBUG))` in Pod::Simple + BlackBox | the same loop with a `use constant` sub instead: **-26.9 %** -- the anonymous sub's frame (`p-flatten-args` + three special bindings + `p-sub-frame`) | not foldable at transpile time (a BEGIN effect); the cost is the FRAME -> s539d's subject, not filed here |
| P3 | the context bind around a call the compiler cannot prove insensitive (codegen review C4) | `p-scalar-ctx` 296 / 1,117, `p-list-ctx` 206 / 179, `p-void-ctx` 98 / 258, `p-caller-ctx` 28 / 230 (everyday / modules); 296 of the modules' 1,117 scalar binds wrap a method call | the void bind removed around `f($i)` (f's tail is a call), 3 M calls: **-2.2 %**, ~4 ns a bind | **REFUTED as a speed lever** -- below the bar; C4 stays an IR-clarity item at most |
| P4 | a scalar lexical kept in a box | 120 of 370 everyday names raw (32 %), 417 of 1,308 module names (32 %); sole reasons of the boxed: `write-list` 57 / 104 (`my ($a, $b) = ...`), `write-shape` 46 / 256 (the value can be a reference or a box), `handle-viv-arg` 32 everyday, `eval-in-region` 50 modules; 67 / 343 have several reasons (PCL_B_DEBUG over both populations) | not measured as one edit -- a raw slot cannot hold a REFERENCE in today's representation, and that is the largest class (`$self`, `$r`, `$ar`, `$o`) | **not low-hanging**: a representation decision (ir-spec §2.2's "a raw slot holds a host number or string, never a box"); `%make-p-box` + `box-set` are 3-17 % of every profile in part 2 |
| P5 | a method call resolved by NAME per call | `p-method-call` with a literal name: 775 in the modules, 145 everyday | s534a measured it (inherited calls 3.9-7.9x perl, own-class 1.2x) | already designed: #4523-#4525, awaiting the USER |
| P6 | `p-funcall-ref` where the callee could be known | 27 sites in the whole sample | -- | too rare to matter |
| P7 | `p-setf` (the generic store) | 348 in the modules | the codegen review: one store in thirteen, the tail of element / deref / slice stores | rides on #3694's step 1d (C1 + C2), not a lever of its own |

**What each EXISTING transform holds** (`optdiff.pl`: which of the 75 bench rows' emission changes
under `PCL_OPT=-NAME`; then `optab.pl`: those rows timed default vs `-NAME`, startup subtracted, best of 3):

| registry name | rows whose emission changes | B/A with the name OFF (k=3; legs/leg3.log, leg5.log, leg4.log) | holds |
|---|---:|---|---|
| `raw-slot` | 74 of 75 | (the base of everything; see `none`) | everything |
| `foreach-range` | 72 | (counting loops; see `none`) | everything |
| `str-buffer` | 6 | `strcat` (20 M `.=`) did not finish in 4 min with it off (killed) -- O(n) -> O(n^2); `accum` (30 k `.=` of a line) 0.0058 -> 1.350 s, 230x | everything on its rows |
| `classic-sort` | 2 | sortnum +341 %, sortstr +373 % | a lot |
| `sig-classic` | 1 | sigarith +425 % | a lot |
| `foreach-arrays` | 5 | feread +95 %, feread2 +315 %, feread3 +337 %; subste -3.1, lcbytes +1.0 | a lot |
| `symref-const` | 1 | symref +136 % | a lot |
| `list-decl-split` | 2 | listdeclcat +99 %; textproc +2.1 | a lot (on its shape) |
| `local-push` | 2 | pushloc +80 %, arrhash-k +9.7 | a lot |
| `numeric-slot` | 4 | sigarith +60 %, cfor +18 %; collatz +2.7, regexg +3.2 | yes |
| `foreach-raw` | 7 | powdigit +40 %; textproc +0.7 | yes |
| `tail-return` | 9 | subret +23 %, methret +11 %, gcdret +10 %; fibret -2.4, feargs -1.4 | yes |
| `args-copy` | 1 | passarr +9.9 % | yes |
| `raw-op-family`, `raw-numeric`, `raw-topic` | 12 / 10 / 19 | not timed one by one (their rows are inside `none`) | -- |
| `insensitive-call` | 3 | subret +4.0, sigarith +3.7, localvar +1.3 | **not measurable** at k=3 (~4 ns a bind -- P3) |
| `elem-setf` | 2 | arrhash +3.3, tiehash -4.0 | **not measurable** |
| `raw-return-family` | 7 | subret -1.2, methret -4.9, methinh +0.2, methpoly -1.3, moo-objs -2.0, localvar +0.9 | **not measurable** |
| `raw-block-eval`, `raw-closure-capture` | **0** | no bench row reaches them | **unknown on the bench** |
| `dyn-loop-exit`, `fh-scope-close`, `line-track` | 0 / 1 / 0 | semantics switches, not speed (`line-track` default off) | -- |
| **`none`** (everything off) | 75 | intloop+= +2,439 %, fib(27)x +280 %, methret +213 %, arrhash +191 %, fhprint +120 %, textproc +11 %, json-rt +0.3 % (its time is inside JSON::PP, loaded from the module cache) (`legs/leg4.log`) | -- |

A transform that "holds nothing measurable" here is not dead code: `raw-return-family`,
`elem-setf` and `insensitive-call` LICENSE other verdicts (a raw slot downstream) and remove binds
whose cost is a few ns, and the bench rows were not written for them.  #4992 measures the five
(those three + the two no-row coverage narrowings) on the everyday corpus and the four modules with
`PCL_OPT=-NAME` corpus-diff counts first -- deleting one is the decision that measurement supports or
not.  The registry otherwise holds what it claims: nine names are worth 10-425 % on their rows.

## Part 2 -- the runtime's hot helpers

Profiles (`sb-sprof :cpu`, 0.5 ms) of nine programs on `05806e16` (`scratch/s540b/prof/*.cpu.prof`;
whole-run wall times from the same harness, warm module cache).  Self-time leaders per program:

| program | perl | PCL | where PCL's time is (self % unless "total") |
|---|---:|---:|---|
| dumper (Data::Dumper, 300 records x 10) | 0.03 s (XS) / 0.12 s (`Useperl`) | 7.8 s | **`do-tr` 77 % total**: `vector-remove-duplicates*` 65 % total, `%find-position` 23 %, `eql` 12 % |
| podtext (Pod::Text, perlfunc.pod) | 0.16 s | 0.87 s | `p-method-call` 91 % total (dispatch); cl-ppcre scanners ~20 %; **`do-tr` 11 % total**; `grow-hash-table` 5 % total |
| json-rt (bench) | 0.99 s | 1.74 s | cl-ppcre 12 %; `box-set` 3.4 + `%make-p-box` 2.9; `%p-str-eq-slow` 7.4 % total; `%p-true-p-slow` 2.4 |
| textproc (bench) | 0.50 s | 1.02 s | cl-ppcre 27 % total; `do-regex-match` 6.6; `ub32-bash-copy` 5.2; `%p-split-literal` 3.5 |
| moo-objs (bench) | 0.06 s | 0.61 s | EQUAL hash tables (`gethash/equal` 4.6 + `%sxhash-string` 3.5); `p-method-call` 85 % total; fasl loading 12.7 % total |
| wordfreq (split + hash count + sort) | 0.04 s | 0.15 s | `cl-ppcre:split` 35 % total; `gethash/equal` 14 % total; `%p-read-record-1` 12.5 % total |
| tbal (Text::Balanced) | 0.04 s | 0.12 s | spread thin, nothing over 6 % |
| m-reduce (`List::Util::reduce`, 30 k) | 0.014 s | 0.46 s | **`map-backtrace` 49 % total** (`caller` in the shim walks SBCL's backtrace), `%p-symref-box` + `gethash/weak` 12 % |
| m-strftime (`POSIX::strftime`, 30 k) | 0.07 s | 0.64 s | **`fstatat` 24 %** (two `tzset` per call: `mktime` + `localtime`), `%make-p-box` 10.5 |

The suspicions read in `cl/pcl-runtime.lisp`, each with its measurement (`ab.pl --lever`: a
redefinition loaded as a fasl in front of the unchanged program, best of 5, C/A = base vs base):

| # | where | what is avoidable | measurement | verdict |
|---|---|---|---|---|
| R1 | `do-tr` (~42116) | every call re-runs `expand-tr-chars` on both lists, builds a fresh EQL table, and for `/c` sorts a `remove-duplicates` (O(n^2)) of the search list; a count-only tr builds the whole result string to count | `lev/tr-cache.lisp` (expansion cached per op, sorted list from the table keys, count-only path): **dumper 7.120 -> 0.591 s (-91.7 %)**, podtext -16.2 %, tbal -3.1 % (C/A <= 0.7 %; `legs/leg1.log`) | **LEVER** #4981 |
| R2 | `%p-maybe-autoflush` (~4084) | once any handle has `$\|` set, every print to any handle probes the WEAK table `*p-autoflush-handles*` (global lock) -- a #4061 sibling | 1 M `print $fh` with vs without `$\| = 1` on STDOUT: **0.074 -> 0.134 s (+81 %)**, ~60 ns a print (`legs/leg2.log`) | **LEVER** #4983 |
| R3 | `p-hash` (~16477) | conses a full copy of the pairs (`append`), then grows an unsized EQUAL table (SBCL's default size is 7) | `lev/hash-presize.lisp`: 10-key constructor loop **-15.4 %**; moo-objs +1.8 % (C/A 0.8: not measurable); podtext 0 (`legs/leg2.log`) | **LEVER** (small) #4984 |
| R4 | `%def-overloaded-str-cmp` slow path (`eq ne lt gt le ge`) | no plain-scalar arm (the numeric comparisons got one in #2513): a boxed string `eq` pays four guarded lookups plus two UNguarded `nomethod` lookups | `lev/str-cmp.lisp` (plain arm first): json-rt -3.6 %, **textproc +10.7 %**, podtext -0.2 % (`legs/leg1.log`) | **REFUTED as written** -- mixed and inside 2x the band on two rows; not filed.  A one-family probe of runtime review R3 that did not pay |
| R5 | `object-address` (refaddr, `==` on refs, `HASH(0x...)`) | every call probes `*p-object-id-table*`, weak AND `:synchronized` | shimbench: `refaddr` 131 ns vs perl 18 ns | not low-hanging: a stable id needs a slot ON the referent (a hash table / vector has none) -- a representation decision |
| R6 | `p-method-call`'s `&rest` + `apply`, `p-sub`'s `&rest` + `p-flatten-args` + five special bindings | a method call conses its argument list twice and builds `@_` | s534a (lookup) and s539d (the frame) own these measurements | pointers only: #4523-#4525, s539d |
| R7 | `caller` in shims (List::Util `reduce` / `first`'s `$a`/`$b` via `caller`) | `caller` walks the SBCL backtrace (49 % of a reduce loop); reduce is 45x perl, perl running the SAME shim text is 7x | m-reduce profile; shimbench | s539d's cheap `caller` tier is the lever; nothing filed here |
| R8 | `%p-symref-box` / `%p-stash-live-sym` | guarded by a count -- but once ANY stash delete ran (namespace::clean, Moo), every symbolic ref probes a weak table | -- | #4525 / #4501's family, already filed |

`:alloc` profiles (`legs/leg6.log`: podtext, json-rt, moo-objs) say the same in allocation terms:
`listify_rest_arg` -- the `&rest` lists of `p-sub` / `p-method-call` -- is 2.6-5.7 % of the
allocation samples (the frame: s539d), ~92 % of everything allocated is under `p-method-call`, and
31 % of moo-objs' allocation is SBCL compiling string-eval'd code (#2421, parked).

Also read and NOT a finding: `p-true-p`'s slow path already has the plain-box arms (#2513, #2772);
`%p-sv-stash`, `%p-stash-deleted-p`, `%p-live-sub-p` and `*p-tie-table*` are count-guarded; `p-print`
/ `p-say` lists are stack-allocated (#2771); the regex engine is parked (ONE engine, USER s476).

## Part 3 -- the shims against perl's XS

`shimbench.pl`: per-call time (ns; the timed closure's call overhead, ~40 ns in perl, is in every
column) on ordinary inputs, best of 3 inside one process, three columns: perl with its XS modules,
perl running PCL's OWN shim text (`-I` a copy of `lib/` where the shim loads under perl), and PCL.
`legs/leg4.log` (load 1.1-1.4).  Call sites counted by a perl scan over perl's lib + `~/.cpan/build`
(the board's dists) + `everyday/` -- the pattern is the function's name followed by `(` or `{` (so
`min`/`max`/`first` overcount a little).

| export | perl XS | perl, PCL's shim | PCL | PCL / XS | sites (files) | cheap route | verdict |
|---|---:|---:|---:|---:|---|---|---|
| `md5_hex` (1 KB) | 1,162 | 264,922 | **356,030** | 306x | 93 (30) | sb-md5 is already loaded: **6,650 ns** a call, digest identical | **#4988** |
| `sha1_hex` / `sha256_hex` (1 KB) | 1,231 / 2,400 | 500,151 / 1,296,160 | **705,627 / 1,462,831** | 573x / 609x | 92 (16) | none in SBCL | not low-hanging (a Lisp SHA is new code) |
| `encode_base64` (1 KB) | 529 | 129,053 | **125,956** | 238x | 56 (23) | a `builtin::` (a few dozen lines) | **#4990** |
| `decode_base64` (16 chars) | 45 | 2,394 | **18,556** | 412x | (same) | #4981's `tr` cache; then the primitive | #4981 + #4990 |
| `dclone` (small nested hash) | 1,707 | -- (the shim needs PCL builtins) | **420,045** | 246x | 48 (18) | the Clone shim's copy: 3,000 copies 1.390 s -> 0.084 s whole run (perl 0.013) | **#4989** |
| `freeze` / `thaw` | 1,296 / 1,000 | -- | 174,108 / 233,932 | 134x / 234x | 45 (6) | -- (the format IS the job) | not filed |
| `Dumper` (small hash) | 7,098 | -- | **1,117,145** | 157x | 159 (91) | #4981 (`tr/\0-\377//c` per value) | #4981 |
| `strftime` | 1,948 | -- | **15,543** | 8.0x | 80 (21) | one tzset; or libc strftime | **#4991** |
| `catfile` / `catdir` | 454 / 228 | -- | **4,509 / 2,385** | 9.9x / 10.5x | 1,565 (259) | canonpath guard (-69 %, `legs/leg2.log`) | **#4985** |
| `reduce` | 240 | 2,097 | **11,167** | 47x | 70 (14) | `caller` -> s539d | pointer |
| `shuffle` / `uniq` / `pairs` | 216 / 408 / 360 | 2,145 / 885 / 504 | 2,884 / 1,589 / 1,756 | 13x / 3.9x / 4.9x | 9 / 39 / 76 | -- | PCL ~= perl on the same text; the shim algorithm is the cost; not common |
| `sum` / `max` / `min` / `first` / `any` | 44 / 39 / 2 / 101 / 170 | 226 / 279 / 277 / 378 / 804 | 260 / 396 / 390 / 553 / 858 | -- | 36 / 75 / 67 / 83 | -- | sub-microsecond; not a lever |
| `blessed` / `reftype` / `refaddr` | 58 / 19 / 16 | -- | 85 / 96 / 145 | 1.5x / 5x / 9x | 643 / 285 / 562 | `refaddr` = part 2 R5 | not low-hanging (an id slot) |
| `looks_like_number` / `weaken` | 24 / 44 | -- | 418 / 240 | 17x / 5.5x | 125 / 247 | -- | sub-microsecond |
| `floor` / `ceil` | 18 / 17 | -- | 101 / 92 | 5.5x | 49 | -- | not a lever |
| `getcwd` | 701 | -- | 1,152 | 1.6x | 205 | -- | fine |
| `encode_utf8` / `decode_utf8` / `encode('UTF-8')` | 50 / 375 / 981 | -- | 240 / 5,120 / 8,329 | 5x / 14x / 8.5x | 46 / 223 | -- | measured, not profiled: a candidate for the next review |
| JSON::PP `encode` / `decode` | 9,792 / 20,179 (perl runs JSON::PP too) | -- | 17,960 / 25,916 | 1.8x / 1.3x | 71 (25) | -- | fine |

The reading: where PCL is far behind, it is because the SHIM's algorithm is pure Perl (perl
running the same text is within 1.5x of PCL on MD5, SHA, base64 and the List::Util bodies, `reduce`
(`caller`) aside) -- the
lever is a primitive, not PCL's execution.  The two exceptions are `decode_base64` (PCL 7.8x perl on the same text) and Data::Dumper (the
pure-Perl path: `dumper.pl` 7.12 s under PCL, 0.12 s under perl with `$Data::Dumper::Useperl = 1`, 60x):
both are #4981.

## Part 4 -- the compile pipeline

**The perl side** (`pl2cl`, a fresh perl each, no server; best of 3, `legs/leg2.log`, load 1.3):

| file | source lines | `pl2cl` | per 1,000 lines |
|---|---:|---:|---:|
| everyday `moo-class.pl` | 16 | 0.258 s | (the ~0.17 s fixed cost + Moo's prototype record from the proto cache) |
| JSON/PP.pm | 3,142 | 3.219 s | 1.02 s |
| Text/Balanced.pm | 2,434 | 1.866 s | 0.77 s |
| Math/BigInt/Calc.pm | 2,566 | 4.336 s | 1.69 s |
| Pod/Simple/BlackBox.pm | 2,437 | 4.143 s | 1.70 s |
| PPI/Tokenizer.pm (vendored) | 1,584 | 3.860 s | 2.44 s |

Where it goes (NYTProf 6.15, subs only, `calls=0`, of the Calc.pm transpile, exclusive time grouped
by package -- `comp/phases.pl`; the profiler inflates small subs, so read the SHARES): PPI's own parse
and tree 27 %, `PPI::Node::find` 26 %, `UNIVERSAL::isa` 11 % (11.1 M calls -- 4,300 per source line,
nearly all from `find`'s wanted-closures), Parser2's other subs 14 % (`_interp_token_candidate`,
`_interp_token_text`, `_elem_within`: 0.7 s each under the profiler), PExpr 7 %, the rename passes
4 %, Binder 2.4 %, CLForm printing 1.5 %, VarAnnotator 1.1 %, ExprToCL 0.7 %.  Everything above
the PExpr line is #3694's (PPI::Node::find, the re-parsing passes, the scope walks) -- nothing new
for this review except that the `isa` count belongs to `find` and dies with it.

**What #3694 does NOT own**, checked one by one:

| item | finding |
|---|---|
| per-module work repeated per PROGRAM | the prototype/export scan of every `use`d module is cached (`~/.pcl-cache/proto/`, content-keyed); `moo-class.pl` (16 lines, `use Moo`) transpiles in 0.258 s against ~0.17 s for a one-liner.  Nothing to take. |
| a module transpile spawned as a new perl | the runtime spawns `pl2cl --module` per module cache MISS; it is served by `pl2cl --xserver` only when `PCL_XSERVER` is set (the gate sets it, `pcl` does not).  Misses are first-run cost -- PARKED by the USER's ruling (#2420 / #2423). |
| a cache that misses when it could hit -- LOCAL modules | **a module under `use lib` / -I / `.` is never fasl-compiled by default (the s471 trust class): its cached TEXT is compiled by SBCL on EVERY run.**  One 2,434-line local module: 0.530 s per run vs 0.055 s with `PCL_COMPILE_DIRS='*'` (perl 0.009; `legs/leg1.log`) -- ~0.2 s per 1,000 lines of a program's own modules, every invocation.  A ruling, so not on THE LIST: #4986. |
| a cache that misses when it could hit -- the vendored PPI as a library | a program `use`-ing `vendor/PPI/lib` re-transpiles PPI's modules on EVERY run (cache entries rewritten each run; 98-116 s a run, s534a's "3 s per parse" excluded load); with `PCL_COMPILE_DIRS='*'` `PPI.pm` is a `fasl-build` on every run and its 100 dependencies load from TEXT under the nested-build rule.  The rewritten manifest is byte-identical to the one it replaces (`legs/leg6.log`), so the invalidation is an ORDERING, not a content, change -- suspected: the modules' `use` cycles.  #4987 |
| emitted size as the driver of SBCL compile time | by top-level form over the sample (1.64 MB): `p-sub` 52.6 %, `p-eval-always` 20.3 % -- of which the **#2610 detector tables are 254,512 of the modules' 1,199,790 bytes (21 %)**, `p-let` 8 %, `p-defcell` 3.7 %, comments 2.4 %.  SBCL side below. |
| `compile-file` policy | generated code compiles at `(speed 2) (safety 1) (debug 0)` (the runtime's last declaim, saved in the core); a cheaper policy is measured below -- it matters for FIRST runs and string evals only (both parked) |
| the fasl cache's own load time | below |

**The SBCL side** (`comp/comp-leg.pl`: per unit, `compile-file` best of 3 in a fresh image, fasl
load best of 5 in a fresh image, both timed inside SBCL; `-nodet` = the same unit with its
detector table emptied; `fast` = `(speed 1) (compilation-speed 2) (space 1)`; `legs/leg3.log`):

| unit | emitted lines | Lisp bytes | `compile-file` | fasl bytes | fasl load | compile, `fast` policy |
|---|---:|---:|---:|---:|---:|---:|
| everyday `moo-class` (a program) | 336 | 10,197 | 1.275 s | 75,310 | 0.564 s | 0.719 s |
| same, detector table emptied | 286 | 8,566 | 1.078 s | 74,524 | 0.566 s | |
| JSON::PP | 3,961 | 178,575 | 1.160 s | 1,069,684 | 0.177 s | **0.548 s** |
| same, emptied | 3,044 | 139,262 | 1.116 s | 1,051,739 | 0.176 s | |
| Text::Balanced | 3,047 | 155,339 | 0.806 s | 617,396 | 0.017 s | **0.374 s** |
| same, emptied | 2,424 | 126,998 | 0.790 s | 605,453 | 0.016 s | |
| Math::BigInt::Calc | 4,345 | 217,379 | 3.074 s | 1,222,874 | 0.015 s | **1.054 s** |
| same, emptied | 3,495 | 175,692 | 3.183 s | 1,205,908 | 0.014 s | |
| Pod::Simple::BlackBox | 5,354 | 302,306 | 4.027 s | 948,427 | 0.207 s | **0.719 s** |
| same, emptied | 4,148 | 239,183 | 3.950 s | 925,582 | 0.205 s | |

(PPI::Tokenizer did not compile outside a program that puts `vendor/PPI/lib` on `@INC`; left out.
A fasl LOAD here includes the unit's own `use`s loading from the module cache, which is why
JSON::PP and BlackBox read ~0.2 s and Text::Balanced 0.017 s; a one-module fasl itself loads in ~15-28 ms per MB of fasl.)

Readings: **the #2610 detector table is cheap** -- 21 % of the bytes, 1-4 % of the compile time
(once, at the fasl build), ~1-2 % of the fasl, nothing at load: it is quoted data, compiled as one
constant, and the run-time side is one `push` per unit (the tables are built lazily, only when the
compile phase installs a sub).  **Not a lever.**  The emitted BYTES are not what drives SBCL's compile
time; the POLICY is: the `fast` policy cuts `compile-file` by 53-82 %.  That is the cost every
FIRST run (a module's fasl build), every string eval (#2421) and every run of a program with LOCAL
modules (#4986) pays -- all three parked or a ruling -- and its RUN-time price was not measured
here, so it is in the "not low-hanging" table, not on THE LIST.

## Part 5 -- start-up (parked; the one number re-measured)

`startup.pl`, best of 15 interleaved, `legs/leg1.log` (load 1.3): `perl hi.pl` **2.0 ms**; `pcl hi.pl`
warm **36.3 ms**, of which compiling the Perl launcher (`perl -c pcl`) is **17.8 ms** and SBCL booting
the 53 MB core and exiting **4.4 ms**; `pcl -e 1` (never cached, #1862) **166.0 ms**.  s509 read
53 / 27 / 6.5 / 243 ms at load 5 and s499 43-48 ms quiet: unchanged in kind -- the launcher is still
about half of a warm one-liner.  Parked by the USER (2026-09-27, 2026-10-06): #2422, #1862, #2421.

## THE LIST -- the low-hanging levers of all five parts, ranked

Ranked by the time a program that USES the construct gets back (the USER's measure: absolute
time on ordinary programs), with how widely the construct is used beside it.  Every row's
discriminating measurement was taken; every row is filed.

| rank | lever | where | measured (population) | reach | size | risk | task |
|---:|---|---|---|---|---|---|---|
| 1 | cache `tr///`'s expansion per op; no O(n^2) complement; count-only without building a string | `cl/` `do-tr` | Data::Dumper program **7.120 -> 0.591 s (-91.7 %)**; Pod::Text -16.2 %; Text::Balanced -3.1 % (leg1) | 746 `tr` sites in 383 files; every Data::Dumper call | S, runtime only | low (op immutable per site) | #4981 |
| 2 | Digest::MD5 over the sb-md5 the runtime already loads | `lib/Digest/MD5.pm` + a `builtin::` | **356 us per KB -> 6.65 us** with sb-md5 (perl XS 1.16 us) (leg4) | 93 sites / 30 files | S-M | low (digest identical; test vectors) | #4988 |
| 3 | dclone by direct copy, freeze+thaw only for hooks / CODE / tied | `lib/Storable.pm` | **3,000 copies 1.390 s -> 0.084 s** whole run through the Clone path (perl 0.013 s) (leg4) | 48 sites / 18 files | S-M | medium (Storable's edge cases) | #4989 |
| 4 | fold a LITERAL `use constant` at its uses after the declaration | `Pl/ExprToCL.pm` (a Kind-A name) | constant-check loop **0.116 -> 0.035 s (-70.2 %)**, ~30 ns a use (leg1) | 1,613 declarations in 321 of 3,797 .pm | S-M, emission | low-medium (position-aware; perl itself inlines) | #4982 |
| 5 | `$\|`'s autoflush mark without a weak-table probe on every print | `cl/` `%p-maybe-autoflush` | 1 M prints after `$\| = 1`: **0.134 -> 0.074 s (+81 % today)**, ~60 ns a print (leg2) | every print in every script that sets `$\|` | S | low | #4983 |
| 6 | base64 encode/decode as a `builtin::` primitive | `lib/MIME/Base64.pm` | encode 1 KB **126 us** (perl XS 0.53 us; perl running the same shim text 129 us) (leg4); decode is #4981 | 56 sites / 23 files | S | low | #4990 |
| 7 | canonpath returns an already-clean path unchanged | `lib/File/Spec.pm` | **1.53 -> 0.47 us a call (-69 %)**, perl 0.27 -> 0.24 (leg2) | 1,565 catfile / catdir sites / 259 files | S | low (15 probe paths) | #4985 |
| 8 | strftime: one tzset, fields normalised without a zone (or libc strftime) | `lib/POSIX.pm` (+ a `builtin::`) | **15.5 us** a call, perl 1.9 us (leg4); fstatat 24 % of the loop | 80 sites / 21 files | S-M | low | #4991 |
| 9 | the anonymous-hash constructor presizes, no flattening copy | `cl/` `p-hash` | 10-key `{...}` loop **-15.4 %**; moo-objs / podtext not measurable (leg2) | every `{...}` with 8+ keys | S | low | #4984 |

Not on the list because they did not pay (measured, recorded so nobody retries them blind):
the string comparisons' plain-scalar arm (R4: -3.6 / +10.7 / -0.2 %), the void-position context
bind (P3 / C4: -2.2 % on 3 M calls), emptying the #2610 detector table (1-4 % of a fasl build,
nothing at run time).

## Not low-hanging -- listed with what each waits for

| item | measured | waits for |
|---|---|---|
| a program's OWN modules are compiled from text on every run | 2,434-line local module: 0.530 s per run vs 0.055 s fasl'd (perl 0.009) | a ruling on the s471 default (#4986) |
| a cheaper `compile-file` policy for generated code | `compile-file` -53..-82 % (JSON::PP 1.16 -> 0.55 s, Pod::Simple::BlackBox 4.03 -> 0.72) | its RUN-time price measured; and it pays only on first runs / string evals / local modules -- parked (#2420, #2421) or #4986 |
| the vendored PPI used as a library rebuilds every run | 98-116 s a run (PCL_COMPILE_DIRS='*' too) | the cause (#4987; leg6's manifest diff) |
| inherited method lookup by name per call | s534a: 3.9-7.9x perl, the fix 1.3-1.7x | the USER (#4523-#4525) |
| the sub frame (five special bindings, `&rest`, `@_` built per call); a glob-installed `sub () {0}` is 27 % slower than a `use constant` sub | leg1 (P2) | s539d's frame measurement, running |
| `caller` in shims (List::Util `reduce` / `first` / `pairmap` find the caller's `$a`/`$b` through `caller`) | backtrace walk 49 % of a reduce loop; reduce 45x perl | s539d's cheap `caller` tier |
| boxes for lexicals that hold a reference | 32 % of scalar names raw in both populations; `%make-p-box` + `box-set` 3-17 % of every profile | a representation decision (a raw slot holding a reference) |
| ref identity (`refaddr`, `HASH(0x...)`, `==` on refs) through a weak + synchronized table | refaddr 145 ns vs perl 16 ns (leg4) | a representation decision (an id slot on the referent) |
| Digest::SHA in pure Perl | sha1 706 us, sha256 1.46 ms per KB (perl 1.2 / 2.4 us) (leg4) | a Lisp SHA-1/SHA-2 implementation -- new code, no primitive to route to (or pclxs) |
| a string eval re-run in a loop recompiles | #2191: 0.57 ms per re-execution | the parked string-eval work (#2191, #1410, #2421) |
| the regex engine (cl-ppcre scanners 10-27 % of json-rt / textproc / podtext / wordfreq) | profiles, part 2 | ONE regex engine (USER s476, #71 parked) |
| start-up: the launcher is half of a warm one-liner | 36.3 ms warm, 17.8 ms of it `perl -c pcl` | the USER's start-up ruling (#2422, #1862, #2421) |

## The older worklists, item by item

Every OPEN item of `docs/faster-codegen-suggestions.md` §12 (and its §0.5 catalogue),
`docs/where-the-time-goes.md` §5, `docs/speed-review-s509.md`, and the speed items of the
two s525 / s526 reviews -- one line each, as of `05806e16`.

| item | source | status | the line |
|---|---|---|---|
| S1 string-append buffer | fcs §12.1 | DONE | #62 `str-buffer`; registry name, 6 bench rows change under `-str-buffer` |
| M1 method cache | fcs §12.2, wttg §5.8 | STILL TRUE (inherited half) | own-class cache shipped (s473s); the inherited half is #4523-#4525, the s534a recommendation awaiting the USER; the per-call-site cache stays REJECTED (USER s444) |
| N1 raw-numeric verdict | fcs §12.3, wttg §5.6 | DONE | `raw-numeric` / `raw-slot`; but the verdict fires for 32 % of scalar names only (part 1, P4) |
| P1 pack / sprintf template memo | fcs §12.4 (#74) | PARKED | pack is parked until the XS decision (USER 2026-09-05); sprintf's constant-format half partly done (#3220) |
| A1/A3 aggregates | fcs §12.5, wttg §5.7 | DONE | boxed-aggregates flip (raw elements), `local-push` (#1140), `foreach-arrays` |
| F1 `dynamic-extent @_` | fcs §12.6, wttg §5.3 | PARTLY DONE | `p-print`/`p-say` lists are stack-allocated (#2771); `p-sub`'s own `&rest` and `p-method-call`'s are heap -- that is s539d's frame measurement, not repeated here |
| N2 / #811 in-place box write | fcs §12.7 | DONE | #811 completed |
| T1 return-family transfer | fcs §12.7b (#77) | DONE | `raw-return-family` |
| X1 block-compile the runtime | fcs §12.8, wttg §5.10 | PARTLY DONE | `(declaim (optimize (speed 3)))` over the runtime (#950); no block compilation; no lever measured here |
| R1 constant regex once | fcs §12.9 | DONE | #1250 / #770 (`%p-op-once`) -- but `tr///` re-expands its tables per call: THE LIST #1 (#4981) |
| O2 constant-fold operands | fcs §12.9 | STILL TRUE | a literal `use constant` is a full call at every use: THE LIST #4 (#4982, -70 % on a constant-check loop) |
| O1 fixnum behind a range proof | fcs §12.10 | STILL TRUE, low value | `numeric-slot` drops the overload guard; no fixnum declarations; not measured here |
| 5.1 FPU modes, 5.2 R1 inline ops | wttg | DONE | 2026-07-02 |
| 5.4 R2 calling convention | wttg | PARTLY DONE | `insensitive-call`, `sig-classic`, `args-copy`; the void-position half (codegen review C4) measured below: ~4 ns a call, REFUTED as a speed lever |
| 5.5 classic sort comparators | wttg | DONE | `classic-sort` (Kind B) |
| 5.9 inferred flags | wttg | PARTLY DONE | "no overload anywhere" is a run-time fact (`%p-no-overload-possible-p`); closed world not done |
| 5.11 map/grep deforestation | wttg | STILL TRUE | not measured here; `p-map-flat` (#3122) removed one intermediate |
| 5.12 profile-guided specialisation | wttg | NOT STARTED | long game, no lever |
| 5.13 regex engine | wttg | PARKED | ONE regex engine (USER s476, #71); cl-ppcre scanners are 10-27 % of json-rt / textproc / podtext |
| 5.14 GC / allocation hygiene | wttg | STILL TRUE | `%make-p-box` + `box-set` are 3-17 % of every profile here; a representation question (part 1 P4) |
| #2770-#2773 levers A-D | s509 | DONE | rounds 40-47 |
| #3301 lcbytes chain, #3302 join one-vector | s509 §2 | STILL TRUE, under the 15 % bar | unchanged |
| #2422 / #1862 / #2421 / #2420 / #2423 start-up + first run | s509 §1, fcs §0.2r | PARKED (USER 2026-09-27, 2026-10-06) | start-up re-measured in part 5 |
| C4 void-position bind | codegen review s526 | REFUTED as speed | -2.2 % on 3 M void calls of a two-call body (~4 ns a bind, `legs/leg1.log`); keep it, if at all, as IR clarity |
| C5 constructor macros | codegen review s526 | NOT A SPEED ITEM | macros expand to the same code |
| R1 numification flag | runtime review s525 | DONE (box face) | s525u |
| R3 one typed entry per op family | runtime review s525 | STILL TRUE | the structural candidate; the string-compare arm (part 2 R4) is a one-family probe of it and did NOT pay |
| R5 rule-10 splits, R6 default arms | runtime review s525 | R6 DONE, R5 partial | not speed |

## Neighbours and files

- **s540a** (#4787, `docs/cpan-speed-comparison-s540.md`) was not on main while this was written and
  its ranked list was not yet drafted; a lever both reviews name is to be listed once, with both
  numbers, by whichever merges second.
- **s539d** (the cheap `caller` tier and a cheaper sub frame) owns the frame and `caller` costs this
  review only points at (part 2 R6 / R7, part 1 P2).
- Scratch, in the s540b worktree's `scratch/s540b/` (not on main): `prof.pl` (sb-sprof harness),
  `ab.pl` (runtime / emission A/B with a base-vs-base column), `optdiff.pl` + `optab.pl` (what a
  registry name changes and holds), `shim/shimbench.pl`, `comp/` (phases, SBCL compile/load,
  detector stripping, bytes by form), `lev/` (the scratch redefinitions: `tr-cache.lisp`,
  `str-cmp.lisp`, `hash-presize.lisp`, `md5-prim.lisp`), `emitlev/` (the hand-edited emissions),
  `legs/leg1..6.log` (every timing quoted above).
