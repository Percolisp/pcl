# The PCRE2 engine spike (#71) — measured, s473q (Opus agent, 2026-09-07)

*Main at launch `782bc37e`, generation v2-1020.  **No product code changed.**  The
spike lives in the agent's `scratch/s473q/` (untracked); this file and task #71 are
the deliverable.  Every timing prints `uptime` beside it and is an interleaved
best-of-5 with a byte-identical control pair in the same window.*

**One-line verdict.**  The FFI is easy and the runtime seam is ONE function, but on
the configuration the brief specified (no JIT) **PCRE2 LOSES the risk case `subste`
1.188× against a control pair whose spread is 0.6 %** — the stop rule's first branch
fires, cl-ppcre stays, and #1187 was done in the same round.  With
`pcre2_jit_compile_32` added PCRE2 *wins* `subste` (0.972×), `textproc` (0.880×) and
`json-rt` (0.915×) and still *loses* `regexg` (1.134×), so the second branch's
precondition is not met either.  **Both readings agree: do NOT begin the
replacement.**  What the spike found instead is that **the case for PCRE2 is PARITY,
not speed: +311 / −7 rows of perl's own `re_tests`, and it answers the 18 rows where
cl-ppcre HANGS — which is task #196 exactly.**

---

## 1. The binding: it works, and it is small

`libpcre2-32.so.0` through `sb-alien` (`load-shared-object` +
`define-alien-routine`), the `crypt()`→libcrypt pattern, no CFFI, no headers.
Version found by `pcre2_config_32(PCRE2_CONFIG_VERSION)`: **10.46 2025-08-27**;
`PCRE2_CONFIG_JIT` = 1.

The 32-bit width pays off as the brief predicted, with one exception (§2b):

* an SBCL `(simple-array character (*))` is 32-bit code points in memory, so
  `sb-sys:with-pinned-objects` + `sb-sys:vector-sap` hands the subject over with
  **zero copy**;
* every ovector offset comes back as a **character index** — probed with
  `\p{L}+` over `"αβγ δεζ"`: `ms=0 me=3`, the three-character answer, not a
  byte count.  The byte-offset→char-index mapping and the cached UTF-8 encoding
  task #71 budgeted as the hard part **do not exist on this path**.

**The runtime seam is one function.**  `%pcl-build-scanner` returns a closure with
cl-ppcre's `(scanner function)` protocol — `(funcall scanner string start end)` →
`(values ms me reg-starts reg-ends)` — and `END` maps onto PCRE2's subject LENGTH,
which is exactly ppcre's `[START, END)`.  Replacing that one function therefore
carries m//, s///, `cl-ppcre:do-scans`, `regex-replace`/`regex-replace-all`,
`%p-ppcre-scan` and `%pcl-scan-anchored-list` at once.  `scratch/s473q/mkrt.pl`
builds the modified runtime from `cl/pcl-runtime.lisp` by replacing exactly that
function, so an `ab-rt.pl` A/B differs by exactly the engine.

The perl pattern text goes in **verbatim** — `perl-regex-to-ppcre`'s six passes are
not run, which is half the point.  Over the five bench programs: **8 distinct
patterns compiled, 0 fallbacks**, every program's output identical to perl's.

Compiled codes are cached in the op's `%compiled` slot exactly as today and freed
by `sb-ext:finalize` (one finalizer per `pcre2_code`; SBCL runs finalizers off the
GC, so the cost is one weak-pointer entry per pattern, not per match).

### 1a. What "JIT" means here, and what it costs a port (Fable, s476 — the USER asked)

Nothing to do with Java, and nothing to do with Common Lisp.  **JIT is a PCRE2
feature.**  PCRE2 is a C library; `pcre2_compile` turns a pattern into a compact
BYTECODE, and the ordinary `pcre2_match` is an INTERPRETER of that bytecode — a C
loop with a large `switch`, one dispatch per matching step, with its own
backtracking stack.  `pcre2_jit_compile` (optional; built on the small embedded
code generator *sljit*) translates that bytecode into native machine code for the
specific pattern, once, and later `pcre2_match` calls run the machine code
directly.  On the many-tiny-matches shape the per-step dispatch IS the cost, which
is why JIT was worth ~1.6× at engine level (§3, "The engine on its own") and why
it flipped `subste` from a loss to a small win.  It costs ~4.8 µs per pattern at
compile time (§3 (d)) — about what cl-ppcre's own compile costs.

Where SBCL sits: SBCL is an ahead-of-time NATIVE compiler, so everything cl-ppcre
builds is machine code already — but cl-ppcre compiles a regex not to bytecode but
to a tree of small CLOSURES, one per pattern node, each calling the next.  That is
native code paying an indirect call per node per step, which places it between
PCRE2's interpreter and PCRE2's JIT on the tiny-match shape — and ahead of both on
the long `/./g` scan, where its dot closure is trivial to re-enter while every
PCRE2 `/g` step is a fresh `pcre2_match` paying full setup (27 vs 57 ns per step,
§3).

**Why it matters to a port**: the brief's binding list did not include
`pcre2_jit_compile_32`; the spike bound it as ONE extra to learn whether the
verdict depended on it, and it did — *the speed case exists only with JIT*.  JIT is
also one more PORTABILITY variable on top of §5: a distribution can build PCRE2
without it (`pcre2_config` `PCRE2_CONFIG_JIT` answers 0 and `pcre2_jit_compile`
returns an error — a port must check at install time, like the version), sljit
supports the common CPUs but not every one, and executable memory is a permission
topic on macOS (the hardened runtime / `MAP_JIT` rules).  An engine port that
needs JIT for its speed case therefore needs the JIT leg in the install matrix and
the macOS leg, not only the library.

## 2. The two measurement bugs the spike paid for — both are portability facts

### 2a. `PCRE2_NO_UTF_CHECK` is a MATCH option, not a compile option

Passed at compile time it only skips the *pattern* check; the **subject is then
re-validated on every `pcre2_match` call**, which is O(subject) per call and makes
a `/g` loop QUADRATIC.  Measured, `/./g` over 200 000 chars ×5:

| variant | seconds |
|---|---:|
| cl-ppcre | 0.0270 |
| PCRE2 UTF, subject checked per call | **41.9014** |
| PCRE2 UTF + `NO_UTF_CHECK` at match | 0.0570 |

A port must pass it at match time.  The cost of doing so: SBCL strings may hold
lone surrogates, which PCRE2 rejects under UTF checking and whose behaviour under
`NO_UTF_CHECK` is *undefined*.  perl does not re-validate either (a PV either is
or is not `SvUTF8`), so passing the flag is what perl does — but perl's engine
cannot be handed a lone surrogate the way an SBCL string can.  **A port owes a
validity decision at the boundary, not at every match.**

### 2b. The zero-copy promise holds only for `(simple-array character (*))`

A `simple-base-string` subject (8-bit) must be widened, and in the naive spike that
copy happened **per match call**: `textproc` went from cl-ppcre's 3.5 s to **over
six minutes**.  The census says why — `textproc` handed over **42 840** base-string
subjects, because PCL's `split` scans one 200 kB base-string blob thousands of
times.  Memoising the widened copy one-deep on the subject's identity fixed it
(42 840 widenings → **6 006 widenings + 36 834 cache hits**) and is what every
number below runs on.

This is the *shape* of the cost task #71 budgeted (a cached encoding), in a
different currency: not UTF-8 bytes, but base→character widening.  **A real port
must answer it, and answer it soundly** — a base-string PCL mutates in place (a
str-buffer) would make an identity-keyed cache stale.

## 3. (a)–(e): the row-level table

`scratch/s473q/ab-rt.pl`, one transpile, two runtimes (two cores), interleaved,
startup subtracted, best-of-5.  Programs taken verbatim from `tools/bench-exec.pl`'s
ROWS table.  Ratios are **pcre2 / cl-ppcre — below 1.0 means PCRE2 is faster**.

### Without JIT (the configuration the brief's binding list specifies)

| row | what it is | cl-ppcre (s) | PCRE2 (s) | ratio |
|---|---|---:|---:|---:|
| control (N=2000) | byte-identical, no regex | 0.0129 | 0.0140 | 1.093 |
| **`subste` (a)** | MANY tiny s/// — THE RISK CASE | 0.2631 | 0.3127 | **1.188** |
| **`regexg` (b)** | one `/./g` over 200 000 chars | 0.7297 | 0.8989 | **1.232** |
| `textproc` (c) | regex-heavy line processing | 2.0962 | 2.0832 | 0.994 |
| `json-rt` (c) | JSON::PP encode+decode | 1.6245 | 1.4907 | **0.918** |
| `pos100000` (e) | `while (m//g)` + pos, 100 k | 0.0963 | 0.1237 | 1.284 |
| `pos200000` (e) | the same, 200 k | 0.1190 | 0.1470 | 1.236 |
| control (N=2000) | the same pair, end of window | 0.0155 | 0.0151 | 0.978 |

An independent earlier run of the same three rows read `subste` 1.185, `regexg`
1.267 — the direction is stable.

### With JIT (`pcre2_jit_compile_32`, one binding beyond the brief's list)

Control at a comparable absolute time, so the spread is meaningful:

| row | cl-ppcre (s) | PCRE2+JIT (s) | ratio |
|---|---:|---:|---:|
| control (N=40000) | 0.3054 | 0.3074 | 1.006 |
| **`subste` (a)** | 0.2680 | 0.2604 | **0.972** |
| **`regexg` (b)** | 0.7975 | 0.9042 | **1.134** |
| `textproc` (c) | 2.1262 | 1.8716 | **0.880** |
| `json-rt` (c) | 1.6804 | 1.5372 | **0.915** |
| `pos100000` (e) | 0.1043 | 0.0961 | 0.922 |
| `pos200000` (e) | 0.1009 | 0.0961 | 0.953 |
| control (N=40000) | 0.3087 | 0.3098 | 1.003 |

**The control pair's spread is 0.3–0.6 %**, so every entry above is real.

### (d) per-pattern COMPILE cost — the cold-start shape

200 distinct literal patterns, each compiled once, ×40, engine level:

| | per pattern | vs cl-ppcre |
|---|---:|---:|
| `cl-ppcre:create-scanner` | 4.1 µs | 1.00× |
| `pcre2_compile` | **0.9 µs** | **4.7× faster** |
| `pcre2_compile` + JIT | 4.8 µs | 0.87× |

So a plain PCRE2 compile is nearly five times cheaper than cl-ppcre's, and
**turning JIT on brings the compile back to roughly cl-ppcre's price** — which is
what makes JIT affordable at module load rather than a cost that has to be
amortised.

### The engine on its own (no PCL plumbing)

`scratch/s473q/engine-ab.lisp`, same loop shapes driven directly:

| shape | cl-ppcre | PCRE2 UTF | +`NO_UTF_CHECK` | no UTF | +JIT |
|---|---:|---:|---:|---:|---:|
| A: 20 short subjects ×4000, 2 patterns | 1.00× | 1.148× | 1.170× | 1.240× | **1.590×** |
| B: `/./g` over 200 000 ×5 | 1.00× | (41.9 s) | 0.474× | 0.466× | 0.540× |

**Read this together with the row table.**  The engine WINS the many-tiny-matches
shape and LOSES the long-scan shape, because a `/g` step is a fresh
`pcre2_match` paying full match setup while cl-ppcre's `.` scanner closure is
trivial to re-enter (27 ns vs 57 ns per step).  `regexg` is the row that scales the
SUBJECT, and §A.4.3's own measurement lesson already says such a row measures the
engine and not PCL — here it measures precisely the one shape PCRE2 is worst at.

## 4. Parity: the argument that is actually strong

perl's own `t/re/re_tests` (2169 lines) through `scratch/s473q/parity-extract.pl`,
applying `t/re/regexp.t`'s unescaping and carrying the `/pat/flags` spelling's
modifiers.  **1188 rows** survive with a verdict; **perl 5.40.3 itself is the
oracle** (each row's verdict is taken by running the match in perl, not read from
the file).  Each row is then replayed through PCL's cl-ppcre path
(`perl-regex-to-ppcre` + `%pcl-build-scanner`, exactly what PCL does today) and
through PCRE2 **verbatim**, in one image, with a 5-second cap per row.

| | rows | % |
|---|---:|---:|
| both agree with perl | 819 | 68.9 |
| **PCRE2 right, cl-ppcre wrong** | **311** | **26.2** |
| **cl-ppcre right, PCRE2 wrong** | **7** | **0.6** |
| both wrong | 51 | 4.3 |
| cl-ppcre could not answer (ERR 197 + HANG 18) | 215 | 18.1 |
| PCRE2 could not answer (ERR 15, HANG 0) | 15 | 1.3 |

**+311 / −7.**  Where cl-ppcre disagrees with perl, by construct:

| shape | rows | example |
|---|---:|---|
| plain (no special construct) | 114 | `a\Nc` |
| possessive / atomic `(?>` | 86 | `a++a`, `a*+a`, `a{1,5}+a` |
| recursion `(?R)` `(?1)` `(?&)` | 45 | `^(<(?:[^<>]+\|(?1))*>)$` |
| named capture, `\g` `\k` | 22 | `(?P<n>foo)`, `(?'n'foo)\k<n>` |
| control verbs `(*VERB)` | 18 | `a*(*FAIL)`, `(A(A\|B(*ACCEPT)\|C)D)(E)` |
| `{n,m}` quantifier | 18 | `\N {1}` |
| POSIX class `[:…:]` | 11 | `[[:lower:]]` |
| lookbehind | 11 | `(?<=af?)b` |
| `\N{…}` | 10 | |
| `\z \Z \A \G \b \B` | 8 | `\B{gcb}` |
| backreference | 7 | `(.*)\d+\1` |
| `\p{…}` | 6 | `^\p{L}` |
| `\K` | 3 | |
| `(?{…})` | 2 | |
| conditional `(?(…)` | 1 | |

Where **PCRE2** disagrees, the list is short and enumerable (15 refusals; the 7
rows cl-ppcre gets right and PCRE2 does not are drawn from it):

* **perl's charset modifiers** `(?u: (?l: (?d:` — PCRE2 has none of them (5 rows);
  and `/aa` semantics (ASCII-only, no ASCII/non-ASCII folding under `/i`) produce
  the two wrong *answers* — `s` under `/aia` and `[A-Z]` under `/iaa`.  `(?a:…)`
  itself compiles.
* forward conditional `(?(1)a|b)` with no group 1 — perl is lenient, PCRE2 refuses (2);
* `{37,17}` with n > m — perl warns and allows, PCRE2 refuses (1);
* `\o{1_0000}` (underscores in an octal escape) — perl-only (1);
* `\p{nv=-0}`, `\p{L_}`-family property spellings — perl-only (2);
* one row is an artifact of the extractor (an `m?…?` spelling it did not strip).

### The 18 HANGs are task #196

The rows cl-ppcre could not answer inside 5 s are exactly
`.X(.+)+X`, `.X(.+)+XX`, `.XX(.+)+X`, `.X(.+)+[X]` and friends — which is
**#196 verbatim** ("`re/regexp_{noamp,notrie,qr}.t` hang at the re_tests
catastrophic-backtracking block (~line 906, `.X(.+)+X`) — cl-ppcre exponential
where perl's optimizer is linear").  PCRE2 returns from all of them.
**#71 is #196's fix**, and that is a fact neither task recorded.

**A rule-12 note the spike must hand on.**  `pcre2_match` returns a negative for
`PCRE2_ERROR_NOMATCH` (−1) **and** for the resource limits
(`MATCHLIMIT` −47, `DEPTHLIMIT` −53, `JIT_STACKLIMIT` −46).  The spike treats every
negative as "no match", which is why those rows read `pcre2=n` where perl says `y`.
A port MUST separate them and die or announce naming the code — silently answering
"did not match" when the engine gave up is the silent-wrong family.

### Constructs `not-supported.md` names #71 as the lift for — compile probe at 10.46

**PCRE2 HAS:** every control verb (`(*FAIL) (*ACCEPT) (*SKIP) (*PRUNE) (*COMMIT)`),
`(*script_run:` / `(*sr:` / `(*asr:`, perl's `/n` (both `PCRE2_NO_AUTO_CAPTURE` and
inline `(?n)`), `\p{L}` `\p{Alphabetic}` `\p{Script=Greek}` `\p{L_}`, possessive and
atomic groups, `(?R)` and `(?1)` subroutine calls, named captures with `\k`, `\K`,
conditionals, variable-length lookbehind, POSIX classes, `\N{U+0041}`, and both
callout spellings `(?C1)` and `(?C"str")`.

**PCRE2 LACKS:** `(?{ CODE })` and `(??{ CODE })`, `\N{NAME}`, `\p{Word}`,
`\p{IsGreek}`, `\p{nv=…}`, the `(?u: (?l: (?d:` charset modifiers, lenient `{n,m}`,
`\o{1_0000}`.

**The callout question, answered as asked.**  PCRE2 refuses perl's `(?{ CODE })`
outright but accepts numeric and string callouts, and `pcre2_set_callout` on a match
context is reachable by the same technique (an `sb-alien` callback).  So carrying
`(?{…})` is *possible* but not free: the pattern must be **rewritten** to a numbered
callout with the perl code held on the PCL side and dispatched from the callback.
That is a design item, not a binding.

**`(?[ … ])`**: PCRE2 10.46 *compiles* it (10.45 added an extended-class syntax).
Its semantics were **not** verified against perl's operator language — do not count
it in either column.

## 5. Portability — what an engine change would have to test

USER, 2026-09-06: *"we are not writing software for our laptop."*  Measured with
`podman` inside each of the install matrix's four images
(`tools/install-matrix/`, `.github/workflows/install-matrix.yml`):

| image | package | version | soname installed | in the BARE image? | after `deps.sh`'s packages? |
|---|---|---|---|---|---|
| ubuntu:22.04 | `libpcre2-32-0` | 10.39-3ubuntu0.1 | `libpcre2-32.so.0.10.4` | NO | **NO** |
| ubuntu:24.04 | `libpcre2-32-0` | 10.42-4ubuntu2.1 | `libpcre2-32.so.0.11.2` | NO | **NO** |
| debian:12 | `libpcre2-32-0` | 10.42-1+deb12u1 | `libpcre2-32.so.0.11.2` | NO | **NO** |
| debian:13 | `libpcre2-32-0` | 10.46-1~deb13u1 | `libpcre2-32.so.0.14.0` | NO | **NO** |
| this dev box | `libpcre2-32-0` | 10.46 | `libpcre2-32.so.0.14.0` | yes | — |

Two facts fall out, and the second is the hard one:

1. **`libpcre2-8` IS present in all four bare images** (grep and git pull it in) —
   `libpcre2-32` is NOT, and installing the recipe's own dependency list does not
   add it.  `tools/install-matrix/deps.sh` would need `libpcre2-32-0`.  Cheap.
2. **The version spread is 10.39 → 10.46 across the four supported images**, and
   the parity gains measured here are on 10.46.  Variable-length lookbehind is
   10.43+; the extended-class syntax 10.45+; several `(?aX)` modifiers 10.43+.  So
   **three of the four images ship a PCRE2 too old for the behaviour measured
   above.**  A floor (the PPI-1.291 pattern) would exclude ubuntu:22.04,
   ubuntu:24.04 and debian:12 from their own repositories; the alternatives are to
   lower the floor and let *behaviour vary by operating system* — strictly worse
   than cl-ppcre's uniform answer — or to build/vendor PCRE2 at install time, which
   the recipe does for nothing today.

**What `tools/install-pcl` would have to do:** `load-shared-object` the library and
run one match at INSTALL time, refusing LOUDLY like the PPI floor.  **Never a silent
fall-back to cl-ppcre** — that is "both engines" through the back door, and the
ruling is ONE engine.  The dlopen name list to try, in order: `libpcre2-32.so.0`
(Linux soname), `libpcre2-32.so` (a `-dev` symlink), `libpcre2-32.0.dylib` then
`libpcre2-32.dylib` (macOS / Homebrew).

**macOS:** Homebrew's `pcre2` formula ships all three widths, so the library is
available — **but there is no macOS leg in CI at all today** (`grep -rn macos
.github/workflows/*.yml` finds nothing).  An engine change would have to add one
before it could ship, and that is a prerequisite, not a follow-up.  **Musl (Alpine)
and the BSDs: REPORTED, not gated**, per the brief.

## 6. The stop rule, applied

> PCRE2 LOSES (a) by more than the control pair's spread → cl-ppcre stays, and
> #1187 is member 1b of THIS round.
> PCRE2 wins or ties (a) and wins (b)/(c) → REPORT; do NOT begin the replacement.

* In the configuration the brief's binding list specifies (**no** `pcre2_jit_compile_32`):
  PCRE2 loses (a) by **18.8 %** against a control spread of **0.6 %**.
  **Branch 1 fires.  cl-ppcre stays; #1187 was done in this round (§7).**
* With JIT: PCRE2 wins (a) by 2.9 %, wins both (c) rows, wins (e) — and still loses
  (b) `regexg` by 13.4 %.  Branch 2's precondition ("wins or ties (a) **and** wins
  (b)/(c)") is therefore also not met.

**Both readings agree on the operative half: do NOT begin the replacement.**  The
replacement remains a PHASE for Fable to design if it is ever taken, and the honest
summary for that decision is: *the speed case is a wash and depends entirely on JIT;
the parity case is strong and unambiguous (+311/−7 and the #196 hang); the
portability case is the blocker, because three of the four supported images cannot
supply a new-enough library from their own repositories.*

## 7. #1187, answered — and a different lever in the same profile entry

**There is no non-simple operand to coerce.**  The subject's representation is worth
0.8 %–1.3 % (inside the control's noise) on both the `/g` and the literal-prefix
shape, measured with the same content as a `simple-base-string` and as a
`(simple-array character (*))`.  The cause is static and is in cl-ppcre, not PCL:

```lisp
;; cl-ppcre/specials.lisp:71
(declaim (simple-string *string*))
```

and SBCL's `simple-string` is the **union** `(or (simple-array character (*))
(simple-array base-char (*)) (simple-array nil (*)))`, so every element access
inside cl-ppcre's compiled matchers is a runtime type dispatch **no matter what PCL
hands it**.  #1187's own second branch fires: the generic call is cl-ppcre's own
type-agnostic code, so the owner is A.2 row 11 (#71) and the profile number is its
price tag.  **The "S if it is one coercion" half of #1187 is closed by measurement.**

But reading the site the profile actually names found something else:

```lisp
;; cl-ppcre/scanner.lisp:72 -- taken ONLY when *use-bmh-matchers* is NIL
(search pattern *string* :start2 start-pos :end2 *end-pos* :test test)
```

and `cl-ppcre:*use-bmh-matchers*` **defaults to NIL** (`specials.lisp:126`) — PCL
has never bound it.  Binding it to `T` switches literal-prefix scanning to a
Boyer–Moore–Horspool matcher:

| measurement | cl-ppcre default | BMH |
|---|---:|---:|
| literal-prefix scan, isolated | 0.3090 s | **0.0470 s (6.6× faster)** |
| `create-scanner` ×20 literal patterns | ~0 | 0.019 s (≈1 ms each) |
| heap per literal pattern | 0 | **8.5 MB** (skip table = `*regex-char-code-limit*` = 1 114 112 fixnums, dense) |

Row level, same `ab-rt.pl` method, a runtime copy whose only difference is that
binding (`scratch/s473q/mkrt-bmh.pl`):

| row | default (s) | BMH (s) | ratio | peak RSS default → BMH |
|---|---:|---:|---:|---|
| control (N=40000) | 0.2912 | 0.2927 | 1.005 | — |
| **`textproc`** | 1.9944 | **1.4813** | **0.743 (+34.6 %)** | 91.6 → **100.4 MB** |
| `json-rt` | 2.6412 | 2.5662 | 0.972 | 94.9 → 94.8 MB |
| `subste` | 0.2551 | 0.2539 | 0.995 | 75.2 → 75.3 MB |
| `regexg` | 0.7530 | 0.7457 | 0.990 | — |
| `pos100000` / `pos200000` | 0.1632 / 0.1564 | 0.1570 / 0.1560 | 0.961 / 0.998 | — |

So it is a **`textproc`-shaped lever: +34.6 % on the one row whose patterns have
literal prefixes, ~0 elsewhere, for one extra skip table (+8.8 MB)** — well past the
20 %-of-its-row bar.  It is **not shipped here**, because the memory is per literal
pattern and unbounded in principle (a module with 100 such patterns would pay
850 MB), and `*regex-char-code-limit*` cannot be lowered to shrink the table without
breaking Unicode character classes — cl-ppcre's own docstring says so.  Filed as
**#1461** with these numbers and the budget question; it needs a decision, not a
measurement.

## 8. Where the spike lives

Everything is in the agent worktree's untracked `scratch/s473q/`:
`pcre2.lisp` (the standalone binding), `engine-ab.lisp` (engine-only A/B),
`mkrt.pl` → `pcl-runtime-pcre2.lisp` (the one-function runtime swap),
`mkrt-bmh.pl` → `pcl-runtime-bmh.lisp` (#1187's lever),
`ab-rt.pl` + `mkprogs.pl` + `rows.pl` + `progs/` (the row harness),
`parity-extract.pl` + `parity.lisp` + `parity-report.pl` + `parity-detail.pl`
(the parity leg), `features.lisp` (the construct probe), `portability.sh`
(the four images), `p1187.lisp` + `p1187b.lisp` + `rss.pl` (#1187), and the run
logs.  Nothing under `Pl/`, `cl/` or `lib/` was touched.
