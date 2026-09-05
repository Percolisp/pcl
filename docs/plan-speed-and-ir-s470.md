# Plan: total speed (the low-hanging apples) and the IR as a contract for other targets (s470, Fable, 2026-09-05)

*USER's framing: "what to do about speed — total speed, get the low hanging apples; there
are enough features faster than Perl that we don't need to stare at those larger than 1×"; then,
sharper: "forget beating Perl in individual items, we do enough for that — just try to get PCL
as fast as possible"; and then: "forget `pack()` until we know if XS will work; prioritize at least half
the weight by how low hanging the fruit is, how easy to implement" —
and "how we should extend the IR to inform the users about what compilation from IR to
JavaScript or C would need".  Two plans, one file.  Every number is from the quiet-box board
(`docs/faster-codegen-suggestions.md` §0.2i, s467) plus today's moves (A5 `sortnum` 7.4× → 3.2×,
`sortstr` 3.5× → 1.9×; BJ `pushloc` 0.48× → 0.28×, `arrhash-k` 1.15× → 1.03×).*

---

## Part A — Speed

### A.0 The metric (USER, 2026-09-05): PCL as fast as possible, in ABSOLUTE time; rank by EASE first

"Forget beating Perl in individual items, we do enough for that."  So the board's pcl/perl
ratios stop being the steering instrument.  The metric is **PCL's own seconds on
representative programs**; a lever is ranked with **at least half its weight on how
low-hanging it is** (how little compiler/runtime work, how little risk, how contained the
change) and the rest on the seconds it removes (profile share × how many real programs hit
it).  A hot path already "faster than perl" is still a target if it is where the cycles go
and the fix is cheap; a big win that needs a design waits behind the cheap ones.  perl's
column stays on the board as a sanity reference and the ten winning rows stay as the CONTROL
rows every perf agent prints beside its numbers, nothing more.  **`pack`/`unpack` are PARKED
(USER): the transpiled oracle and its 3 s extension load are not touched until the XS
question is answered — if pclxs carries `pack`, that whole cost class goes away without
compiler work.**  (`docs/where-the-time-goes.md` is the earlier profile-driven record; this
plan re-instates that method with macro programs as the subject.)

Method, unchanged: every transform is a NAMED Kind-A/Kind-B emission in the registry
(`PCL_OPT=none` runs identically — `Pl/t/passes-01.t`); a change is SIZED first by the
hand-replaced A/B (s469bh's `ab-lisp.pl` method: the same transpiled program with one form
replaced, one core, interleaved series, best-of-K, a byte-identical control pair in the same
window); one perf agent per round; the merged tree's sweep is the correctness bar; a row that
gets slower anywhere is a stop.

### A.1 The cheap round first, the yardstick beside it

Because ease carries half the weight, the FIRST perf round is the three small levers whose
cause is already known and whose change is a few dozen lines each (A.2 rows 1–3); the
yardstick (macro rows, constants, profile) is built in the SAME round by the same agent
after they ship, so round 28 has a ranked table to read.  The yardstick:

| macro row | what it exercises | why this one |
|---|---|---|
| `json-rt` | `JSON::PP` encode+decode of a 50 kB nested structure, ×N | the commonest pure-Perl CPAN workload: hashes, arrays, string building, `sprintf`, regex, method calls (its `pack`/`unpack` of unicode is measured but NOT a target — parked) |
| `moo-objs` | a `Moo` class with three attributes, ×N constructions + accessor calls | the OO mix: `bless`, method dispatch, closures, `local`, argument passing |
| `textproc` | `Text::Balanced`/`Text::Wrap`-style line processing of a 1 MB string, ×N | regex-heavy string processing with `pos`, `substr`, `.=`, `split`, `join` |

Each measured at N and 2N (intercept = constant term, slope = per-iteration cost) and
PROFILED with `sb-sprof` on the quiet box.  Four constant terms measured on their own, each a
one-line program timed end to end: startup (`pcl -E 'print 1'`), module load warm and cold
(`pcl -E 'use JSON::PP; print 1'`, second run / after `--clear-cache`), string eval (`pcl -E
'eval "1"; print 1'`, the `pl2cl --server` spawn).  The extension load is NOT in the list
(parked with `pack`).  **Round output: a RANKED TABLE** — every runtime function or emitted
shape with ≥ 2 % of the cycles in any macro row, the constants as rows of their own, each with
its candidate lever from A.2 or "no lever known", each with an EASE score — and the rounds
after take it from the top of the ease-weighted order.

### A.2 Candidate levers, ranked (ease ≥ 50 % of the weight; the profile re-ranks the impact half)

Ease: **S** = a few dozen lines in one place, one Kind-A name, no new fact; **M** = a new
emission shape or runtime helper with a licence from EXISTING facts; **L** = a new fact
family, a new invalidation rule, or a foreign library.  Impact: a guess until the profile
(A.1) replaces it — "wide" = most CPAN programs hit it, "narrow" = a shape.

| # | lever | ease | impact (guess) | what it is | cause known? |
|---|---|---|---|---|---|
| 1 | `symref-const` | **S** | narrow (symbolic access in older CPAN code, `Exporter`-style loops) | a CONSTANT string operand of `${"…"}`/`@{"…"}`/`&{"…"}` resolves once per site (a load-time cell); non-constant keeps the lookup | yes: `symref` resolves the string per access |
| 2 | sort-result ADOPTION | **S** | wide (every `my @x = sort …`, `= map`, `= grep`, `= reverse`, `= keys`) | `p-array-=` whose RHS is a FRESH vector (the producer says so) adopts it instead of copying — A5's consumer analysis already proves freshness; the same helps every list-producing builtin | yes: the copy is visible in the A5 profile |
| 3 | foreach-raw over a LIST of arrays | **S–M** | medium (`for my $x (@a, @b)`, `for (@$r, @$s)`) | iterate each array in turn, no flattened temporary; #1140's facts already say which arrays are safe | yes: `feread2` flattens (0.47× → 1.32× for one extra array) |
| 4 | const-key slice assignment | **M** | medium (`@h{qw(a b)} = …` is idiomatic) | a list-assign whose LHS slice has constant keys/indices becomes N direct stores | yes (§0.2h): the generic list-assignment path |
| 5 | `p-array-=` from a RANGE | **M** | narrow–medium | `@a = (1..$n)` and `(1..20, $_)` fill in one loop, no materialised range | yes: `arrfill` |
| 6 | raw-element rvalue slices | **M** | medium (`@a[…]`, `@h{@k}`) | fill a fresh simple-vector directly when the container is raw-element (the #1140 facts + the hash's raw flag) | yes (§5): keys and lookups, not boxes |
| 7 | sub-call frame trimming | **M** | **wide** (CPAN code is call-dense) | a sub proven not to `goto`/`wantarray`/`caller`/string-eval gets a leaner `p-sub-frame`; `raw-params` coverage measured (which subs still take boxed `@_`) | partly: #964 protocol; needs the `moo-objs` profile |
| 8 | typed key sort | **M** | narrow | after adoption (#2): a merge sort specialised for all-fixnum / all-string key vectors | yes: the generic `stable-sort` |
| 9 | `strcat` residue | measure | narrow | profile first (`sb-sprof` over the row); no design until the profile names it | no |
| 10 | per-CLASS method + overload-handler cache (#582) | **L** | wide for OO code | blocked on the `@ISA`-write invalidation rule — a Fable design | yes: #73 measured the lookup share |
| 11 | PCRE2 regex backend (#71, + #196 hangs, #477 quadratic `pos`) | **L** | wide for text code, and a PARITY lever | sb-alien binding; the JS target's tier design reuses the classification | yes: the engine itself |
| — | `pack`/`unpack` (#74, the extension load) | — | — | **PARKED (USER) until the XS decision** | yes, but not ours to fix yet |

### A.3 The rounds (each round = ONE perf agent + the correctness agents)

1. **Round 27 perf = levers 1–3 + the yardstick.**  Three sizings by the hand-replaced A/B
   (ship what clears 20 % of its row), then the macro rows, the four constants and the
   profiles → the ranked table.
2. **Round 28 perf = levers 4–6** (the aggregate family, one mechanism: raw-element containers
   under the #1140 facts) — sized together, ordered by the table.
3. **Round 29 perf = lever 7** if `moo-objs`' profile puts the frame protocol high; else the
   next table entry.
4. **Fable designs in between**, unblocking the L rows for later rounds: the `@ISA`-write
   invalidation for #582; the PCRE2 backend (also Part B's regex question); the string-eval
   daemon only if the constant says so.  **The XS decision (USER) unparks `pack` or closes it.**

What "done" looks like for Part A: every S and M lever either shipped or closed by its sizing;
the macro rows' profiles flat (no function above ~10 %) or their top entries owned by a filed
lever; the general-form compiler (`PCL_OPT=none`) still running everything identically.

### A.4 The ranked table (round 27 measurement, s470bn)

*Measured on `f728637` + the four round-27 levers, generation v2-750.  **THE BOX
WAS NOT QUIET** (a browser with several content processes): `perl`'s own column
moved 40 % between consecutive runs of the same tree, so `tools/bench-exec.pl`
could not resolve a 10 % change and every LEVER number below came from
`scratch/s470bn/ab-lisp.pl` — two `.lisp` files on ONE core, interleaved,
best-of-K, with a byte-identical control pair timed in the same window and
printed beside the result.  The RATIOS are stable across four independent runs
because both engines see the same load; the intercept/slope decomposition is
NOT (see A.4.2).  Raw profiles: `scratch/s470bn/profiles/`.*

#### A.4.1 THE FOUR CONSTANTS — and the answer they gave

Each a one-line program timed END TO END, best-of-5, nothing subtracted: the
constant IS what a user waits for before the program's own work starts.

| constant | perl (s) | PCL (s) | PCL/perl |
|---|---:|---:|---:|
| startup (`print 1`) | 0.0016 | **0.169** | 105× |
| `use Moo; print 1` — module cache WARM | 0.0081 | **3.50** | 431× |
| `use JSON::PP; print 1` — module cache WARM | 0.0098 | **6.44** | 658× |
| `use JSON::PP` — module cache COLD | — | **13.42** | (the warm run right after it: 6.63) |
| `eval "1"; print 1` — the `pl2cl --server` spawn | 0.0020 | **0.292** | 142× |

**THIS IS THE BIGGEST NUMBER IN THE WHOLE YARDSTICK, and it is not a codegen
question at all.**  A program that says `use JSON::PP` waits **6.4 seconds**
before its first statement runs — WARM, i.e. with the module's transpiled
`.lisp` already in `~/.pcl-cache/`.  The cause is one `defparameter`:

```lisp
(defparameter *pcl-cache-fasl* nil
  "When true, cache compiled FASL; when nil, cache .lisp and load as SOURCE.
   NOTE (session 251): defaults to NIL as a correctness workaround for the
   module compile-file+load DOUBLE-EXECUTION bug. … Loading as source is
   single-pass and correct, at the cost of slower module loads.  The proper fix
   … is option C/D in docs/module-double-exec-bug.md — DO NEXT SESSION.")
```

So the module cache stores TEXT and SBCL recompiles it on every run.  That
"next session" is long past, and until now the cost had never been priced: it
is 3.5–6.4 s per run per module set, against a codegen worklist whose whole
round-27 harvest is measured in tens of milliseconds.  **Everything else in
this table is smaller than this one row.**  It is task **#1188** (ease L,
because a correctness bug has to be eliminated first — the double execution
runs a module's guarded BEGIN-time sub redefinitions twice and clobbers them,
which is what broke Moo subclasses in s251) and it is the FIRST thing the next
perf round should cost out, not the last.

The startup and string-eval constants are small beside it and both are already
understood (the saved core; the `pl2cl --server` spawn).

#### A.4.2 THE THREE MACRO ROWS

`tools/bench-exec.pl` gained `json-rt`, `moo-objs`, `textproc` (+ `feread3` as
L3's scaling control).  Best-of-3, startup subtracted, at N and 2N:

| row | N | perl(s) | PCL(s) | PCL/perl | 2N | perl(s) | PCL(s) | PCL/perl |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| `json-rt` | 100 | 0.967 | 3.280 | **3.39×** | 200 | 3.159 | 11.413 | 3.61× |
| `moo-objs` | 20 000 | 0.045 | 2.439 | **54.5×** | 40 000 | 0.142 | 6.303 | 44.4× |
| `textproc` | 400 | 0.688 | 3.479 | **5.06×** | 800 | 1.638 | 7.371 | 4.50× |

An earlier pair at a quarter of these N read json-rt 3.46×/3.55× and textproc
5.60×/4.99× — so **the RATIO is the stable quantity** (json-rt 3.4–3.6×,
textproc 4.5–5.6×, moo-objs 36–55×).

**The intercept/slope decomposition FAILED on this box and must be re-taken on
a quiet one.**  Both engines' times grew SUPERLINEARLY between the two runs
(perl's json-rt went 0.967 → 3.159 for a 2× work increase), which is a property
of the machine, not of the programs; the resulting "intercept" comes out
negative for json-rt and 2.3 s for moo-objs, and neither is meaningful.  What
the numbers DO say is that all three rows' constant terms are already
subtracted by the bench's own `t(N) − t(0)` and that the module load — the
A.4.1 row — is therefore NOT in them.  It is in the user's wall clock.

#### A.4.3 THE RANKED TABLE — every function ≥ 2 % of a row's RUN samples

`sb-sprof`, `:mode :cpu`, 1 ms interval.  Sample counts: json-rt 26 973
(N=400), moo-objs 22 655 (N=60 000), textproc 12 202 (N=1 200) — thousands of
samples per row, so a 2 % entry is ~250 samples and stable.  The programs are
LOADED AS SOURCE (a program containing `use M` cannot be `compile-file`d first:
the `p-use` sits in an `eval-when` and runs before the plain load-time
`(setf @INC …)`), so SBCL's own compiler frames are in each profile; they are
reported as one compile share and the rest renormalised over the run.
Instrument: `scratch/s470bn/prof.pl` + `rank.pl`.

Compile share: json-rt 0.0 %, textproc 0.0 %, **moo-objs 16 %**.

| # | function (self, % of the row's RUN samples) | json-rt | moo-objs | textproc | candidate lever (A.2 row) | ease | Kind-A? |
|---|---|---:|---:|---:|---|---|---|
| 1 | `list-all-packages` under a system mutex | – | 17.9 | – | **#1189** — moo-objs' profile is LOAD-and-COMPILE dominated (`p-use` was 59.7 % of its samples before Moo was pre-loaded out of the run); this is the A.4.1 module-load cost seen from inside | L | no |
| 2 | `search` | 14.5 | – | 11.5 | **#1187** — it is cl-ppcre's OWN scanner calling it (graph-confirmed: `create-scanner-aux`'s lambda, 23.7 % TOTAL in json-rt), not `p-index`.  A.2 row 11 (PCRE2, #71) — or find the non-simple operand first: `do-regex-match` already coerces the SUBJECT (#680), so something else in the pair is generic | L (PCRE2) / S (if it is one coercion) | no |
| 3 | `sb-kernel:vector-hairy-data-vector-ref/check-bounds` | 11.6 | – | 12.5 | same as #2 — a GENERIC aref on a non-simple vector, inside that scan | S–M | no |
| 4 | `sb-kernel:%member-eq` | – | 12.6 | – | #1189 (package machinery) | L | no |
| 5 | cl-ppcre `create-scanner-aux`'s scan closure | 8.7 | – | 8.2 | A.2 row 11 (#71 PCRE2) | L | no |
| 6 | `(sb-vm::optimized-data-vector-ref character)` | 7.2 | – | 3.1 | as #3, but the FAST arm — the residue after #2/#3 | — | no |
| 7 | a cached-module lambda (`~/.pcl-cache/021484…`) | 6.9 | – | – | JSON::PP's own transpiled body — a PCL-emitted sub; the shape to profile next | M | — |
| 8 | cl-ppcre `create-char-set-matcher` closure | – | – | 6.3 | A.2 row 11 | L | no |
| 9 | `char=` | 5.2 | – | 4.5 | as #2/#3 | — | no |
| 10 | `sb-pcl` BRAID / `shared-initialize` / `update-ctors` | – | 5.1 / 4.0 / 2.6 | – | #1189 | L | no |
| 11 | `sb-kernel:%sxhash-string` | – | 4.7 | – | hash-key stringification (§5's "keys and lookups, not boxes") | M | no |
| 12 | `pcl::do-regex-match` | – | – | 4.5 | PCL's own regex entry — the per-match protocol around the scan | M | no |
| 13 | `package-implements-list` | – | 4.3 | – | #1189 | L | no |
| 14 | `pcl::set-match-vars` | – | – | 3.6 | the per-match `$1`/`$&` bookkeeping; a lever only if a match's captures are provably unread | M | Kind-A candidate |
| 15 | `sb-kernel:ub32-bash-copy` | 2.8 | – | – | string copying — `p-string-concat` / `substr` | M | no |
| 16 | `sb-kernel:string=*`, `length`, `two-arg-and` | 2.2 | 2.6 | 2.4 | residue | — | no |

**How to read it.**  For the two text-heavy rows — which are the shape most
CPAN code has — **more than a third of the run is inside cl-ppcre** (#2, #3,
#5, #6, #8, #9 all belong to one scan), and A.2's row 11 (the PCRE2 backend,
#71) is the lever that owns it.  That makes it the biggest CODEGEN-adjacent
prize the profile found, and it is an L: an sb-alien binding plus the tier
classification the JS plan already settled.  Before committing to it, the cheap
half is worth one measurement (#1187): a generic `search` and a hairy
`data-vector-ref` inside a scan whose subject `do-regex-match` already coerced
means some OTHER operand is non-simple, and that could be one `coerce`.

For `moo-objs`, the table is NOT a table of object work: 16 % is SBCL
compiling the program and, before Moo was pre-loaded out of the profile, 59.7 %
of the samples sat under `p-use`.  What it measures is A.4.1's module load.  The
per-object ranking needs an instrument that separates them (#1189).

**AND ONE LEVER THE SCALING CONTROL HANDED OVER FOR FREE.**  With
`foreach-arrays` in, `feread2` reads **0.28x** of perl and `feread3` 0.30x
(they were 1.32x and unmeasured), so the run's cost does not grow with the
array COUNT — which is what `feread3` exists to say.  But the SINGLE-array
`feread` still reads **0.47x**: a two-array loop is now FASTER than a one-array
loop over the same 1000 elements, because the run indexes each source's
element-storage simple-vector with `svref` while `p-foreach-raw` over one live
array goes through `%p-foreach-elt-raw` on the array OBJECT.  So the
single-array path can take the run's own indexing — ease **S** (the expander
already has both shapes), impact narrow but free, and the number to beat is
0.47x -> ~0.28x.  It is the cheapest lever this round found and it was not
taken: it needs the live-array semantics checked (a `push` during a
single-array loop DOES extend the iteration today, which the run deliberately
does not).

**None of the four round-27 levers appears in this table**, which is the
expected outcome and worth stating: `symref-const`, the bulk fill,
`numeric-slot` and `foreach-arrays` each removed the whole of a microbench
row's cost, and none of those shapes is where a real program spends its time.
The rounds after this one should be ordered by A.4.1 and A.4.3, not by A.2's
guesses — and A.2's own ease-weighted order stands only for the rows the
profile does not contradict.

---

## Part B — The IR as a contract: what a JavaScript or C backend would need to know

### B.0 What exists, and the gap

`docs/ir-spec.md` is the translator's manual (data model, coercion, context protocol, calling
convention, control flow, OO, magic, load model, the op-inventory FAMILY rules, §11 "what a
translator may ignore").  `docs/generated-cl-ir-review.md` §5 is the minimal consumer
checklist (reader, box type, op inventory, context, dynamic binding, non-local exits) and §3
the friction list (seams, raw control characters, un-parsed regex literals, two construction
levels, deafening context binds, the baked-in environment).  #1035 put the compiler's own
FACTS on the declaration forms (`p-let` class + `:perl`/`:why` manifest, `p-raw-params`,
`p-sub` facts plist).  `docs/js-target-plan.md` Part II is the JS mapping specification with
its open items (numbers, strings, the three-tier regex design — settled s460f — string eval,
pclxs on Node).

**The gap is not semantics — it is INVENTORY and MEASURE.**  A backend author today must read
a 2,600-line spec and a 500-symbol export list to answer three questions the IR could answer
on its own: *which ops does THIS program use*, *which of them carry which obligations*
(dynamic scope, non-local exit, phase, magic, string eval, regex tier), and *how do I know my
backend is right*.  Part B makes the IR answer those, in the order that each answer costs
the least and informs the most.

### B.1 Deliverables, in order

**B1. The op inventory as GENERATED data, with a gate row.**  A tool (`tools/ir-inventory.pl`)
walks the runtime's export list and each `p-*`/`%p-*` docstring and produces
`docs/ir-op-inventory.md` (+ a machine form, `docs/ir-op-inventory.tsv`): one row per op —
family (§10's table), arity/`&rest`, context-sensitive?, coercions applied (§3), magic globals
read/written (§8), can die, needs dynamic binding, needs the phase model, macro-or-function,
SBCL-specific?  The docstring is the source (the runtime "is the spec" by project rule), so
the tool reads a small structured tail we ADD to each docstring over time (`Contract:`
lines) and marks the rest `UNCLASSIFIED`; the gate row fails when an exported op is missing
from the table or the table is stale (the #1072 body-comparison pattern).  This is the port
list a C or JS runtime author works from.  *Cost: small tool + a slow docstring campaign that
rides on other work.*

**B2. A per-program MANIFEST in the emitted IR.**  `pl2cl` already prints one header; add a
comment block (and `pl2cl --manifest` to print it alone, as JSON):
`;; @ USES: p-+ 412, p-gethash 88, p-sort 3, %p-sort-classic 2 …` (every runtime op with its
count), `;; @ NEEDS: dynamic-scope(local:7) non-local-exit(last-dyn:1, goto:0) string-eval(2)
regex-tier(native:14, pcre:2, refused:0) phase(BEGIN:3, END:1) tie(0) overload(1) formats(0)
xs(0)`, and `;; @ FACTS: raw-numeric 41/60 scalars, raw-elems 12/12 arrays, foreach-raw 7/9
loops`.  A backend author reads it and knows what the program demands of a target
("string-eval(2) → not the browser without a compiler service"; "regex-tier pcre:2 → the
WASM fallback is needed").  The counts come from the ONE lowered CLForm tree the Kind-B
passes already walk — one more walk at print time, zero runtime cost.  *Cost: small; the
`NEEDS` classes are the ones ir-spec already names.*

**B3. Per-sub facts widened to OBLIGATIONS.**  `p-sub`'s facts plist (#1035, now printed
correctly after #1118) gains `:needs (…)` with the same classes as B2 scoped to the sub —
so a backend can compile a program partially (every sub whose `:needs` it implements) and
refuse the rest with the same loud die shape the CL target uses (ir-spec §9.3b).  *Cost: the
B2 walk, keyed by sub.*

**B4. The host-leak census as a gate.**  The IR must contain only three vocabularies: `p-*`
/`%p-*` runtime names, a WHITELISTED kernel of CL special forms/macros (`let let* lambda progn
if when unless block return-from tagbody go catch throw unwind-protect setf incf setq
multiple-value-bind the declare eval-when defvar defparameter function funcall apply quote`
— ir-spec §11 names what may be ignored; this names what must be implemented), and literals.
`tools/ir-host-leak.pl` scans the corpus (the four populations) and fails on any other symbol
(`sb-*`, `p-double-inf`'s successors, a bare CL function leaking through a seam).  The
whitelist IS the "CL kernel subset" a C or JS backend implements — write it down in ir-spec
as §11b with the JS and C rendering of each (see B.2 below).  *Cost: small tool; the
whitelist is the finding.*

**B5. The IR as DATA, not text.**  Friction §3.1 item 7: `pl2cl --emit-sexp` (or JSON) prints
the CLForm tree with every symbol pipe-quoted and every string escaped, so a consumer parses
it with a 50-line reader in any language and never needs a CL reader's rules for `$@%<>`
symbols.  The CLForm tree exists; this is a second printer.  Structured regex literals
(friction §3.3: `(p-regex "/pat/flags")` → `(p-regex :pat "…" :flags "…" :tier :native)`)
belong here, because the JS regex tiers need the parsed flags and the tier VERDICT without
re-parsing Perl source; control characters escaped (§3.2) too.  *Cost: medium (a printer +
the regex literal flag-day with a generation bump).*

**B6. The IR conformance corpus.**  The pattern pclxs uses for hosts (`tools/pcl-conform`,
"a host is done when it answers every case the way real perl does"): `tools/ir-conform`
holds ~300 small programs (the existing probe files — every `scratch/*/p*.pl` a review left
behind — plus `Pl/t`'s runtime rows) with perl's output recorded as the oracle; it emits the
IR for each (B5's data form) into `ir-conform/cases/`.  A backend implementer runs THEIR
backend over those IR files and compares to the oracle; PCL's own CL target runs it in the
gate as the proof the corpus is sound.  *Cost: medium; mostly harvesting what exists.*

**B7. Then the target notes.**  With B1–B6 in place, `docs/js-target-plan.md` Part II and a
new `docs/c-target-notes.md` become SHORT: each is the B4 kernel table + the B1 inventory's
per-family mapping + the open items.  The JS prototype itself waits for the quiet IR, as the
USER ruled (2026-09-01); the C notes are written when someone needs them.

### B.2 What each target must supply (the minimum) — the table B4 fills in

| obligation (IR concept) | in the CL runtime | JavaScript | C |
|---|---|---|---|
| boxes and raw slots (§2.2) | `p-box` struct, raw fixnum/double/string in a `let` | object `{v}` vs local; numbers are doubles (IV/NV split — js-plan II.8 item 1) | struct + tagged union; IV/NV/PV exactly as perl |
| strings as codepoint sequences (§3.2) | CL strings | UTF-16 strings need a codepoint layer (js-plan II.8 item 2) | UTF-8 with a length cache, or UTF-32 |
| bignum/overflow rules (§3.1) | CL integers → IV/NV coercion in `p-+` | BigInt or double-with-flag | 64-bit + double, perl's rules |
| dynamic scope: `local`, magic globals (§7.2, §8) | special variables + `let` | an explicit save/restore stack | a save/restore stack |
| non-local exit: `p-return`, `last`/`next`, `die` (§5.3, §6) | `block`/`return-from`, `catch`/`throw`, conditions | exceptions with a tag object; labeled `break` where lexical | `setjmp`/`longjmp` or an unwind stack |
| the phase model: `BEGIN`/`eval-when` (§9) | `eval-when` | run in form order (§11) | run in form order |
| string eval (§9.1) | `pl2cl --server` subprocess | Node: the same subprocess; browser: a compiler service (product decision) | subprocess |
| regex (§10 regex family) | cl-ppcre | three tiers: native `RegExp` / PCRE2-WASM / refused | PCRE2 |
| OO dispatch, C3 (§7.3) | CLOS-free string dispatch with memos | the same tables | the same tables |
| GC | SBCL | the host's | refcounting or Boehm — the one decision C cannot borrow |
| `tie`, `use overload` (§2.2b, §10) | hooks in the accessors | the same hooks | the same hooks |
| I/O, processes, `%ENV`, signals | sb-posix | Node yes / browser stubbed | libc |

B1 tells a backend WHICH rows of this table a given program needs (through B2/B3); B4 tells
it which host constructs it must implement; B6 tells it when it is right.

### B.3 What a FAST backend needs from the IR — facts, not PCL's rewrites

The B.1–B.3 items make a backend POSSIBLE.  A fast one needs something else: **the facts PCL
proves, delivered as facts**, because a foreign target cannot use PCL's own fast-path
rewrites (`%p-push1`, `%p-sort-classic`, `p-incf-raw` are shaped for SBCL) but can use the
PROOF behind each of them to pick its own.  Today the facts reach the IR in two forms: on
declarations (#1035: `p-let`'s class + `:perl`/`:why`, `p-raw-params`, `p-sub`'s plist) and
implicitly, as the Kind-A rewrite that consumed them.  The rule for Part B is therefore:

> **Every Kind-A/Kind-B licence is printed as a FACT on the general form**, so `PCL_OPT=none
> --facts` yields the plain IR with every proof attached — the portable speed — and the
> SBCL-shaped rewrites stay what they are: PCL's own consumption of those facts.

What each fact buys, per target (the ones PCL already proves are marked ✓; the rest are the
facts a fast backend would want and PCL does not yet compute):

| fact (on the IR node) | PCL proves it | JavaScript uses it for | C uses it for |
|---|---|---|---|
| scalar class: raw numeric / raw string / boxed (`p-let :class`) | ✓ | a plain `let` number or string instead of a `{v}` cell — V8's fast path; no box allocation | `int64_t`/`double`/`char*` locals instead of an SV; no refcount traffic |
| numeric RANGE proof (fits int32 / fits int53 / fits int64) | ✗ (the s3xx experiments: the fixnum win is gated on it) | `x \| 0` int32 arithmetic, or safe double arithmetic without a BigInt guard; without it every `+` must check overflow to NV | `int64_t` with overflow check vs perl's IV→NV promotion |
| array facts: escapes / written-in-region (#1140) | ✓ | a plain JS `Array` used in place (push = `arr.push`), no alias cells; a raw `for…of` | stack or arena allocation of the array; `realloc` growth; no per-element SV |
| element HOMOGENEITY (all-fixnum / all-double / all-string array) | ✗ | `Int32Array`/`Float64Array` or a V8 PACKED_SMI_ELEMENTS-shaped array | `int64_t[]`/`double[]` instead of `SV*[]` |
| foreach loop var read-only (`foreach-raw`) | ✓ | a `for (const x of arr)` — no alias object per iteration | iterate by value, no alias pointer |
| hash key class (constant string / small-int) and raw-element hash | ✓ partly (raw elements; #982 small-fixnum keys) | a JS object with a stable shape (hidden class) for constant-key hashes, `Map` for dynamic ones | open-addressing table with interned keys; a struct for constant-key "record" hashes |
| sub facts: `:returns` family, `:insensitive` (context-insensitive), no `goto`/`caller`/string-eval/`local` inside (#1035, #1045) | ✓ | a plain JS function with positional parameters and a plain `return`; no wantarray argument; no frame object | a C function with a fixed prototype; no `@_` array; no frame push |
| parameter class (`p-raw-params`) | ✓ | positional parameters, no `arguments` array, no `@_` aliasing | positional C parameters |
| capture manifest (`:captured`/`:spanning`, the `:why` on cells) | ✓ | closures are native — the manifest says which `let`s must live in the closure scope vs the loop body | closure conversion: heap-allocate ONLY the captured cells, everything else on the stack |
| call-site facts: callee statically known, monomorphic method site, static context | partly (insensitive-call; static ctx where a `p-*-ctx` bind is absent) | a direct call instead of dispatch; V8 inline caches reward a stable receiver shape, so blessed hashes should be shaped consistently | a direct C call; a per-site cache for method calls |
| `tail-return` (a `return` in tail position) | ✓ | a plain `return` — no exception for non-local exit | a plain `return` — no `longjmp` |
| string facts: buffer-only use (`str-buffer`), byte-only content | ✓ (buffer), ✗ (byte-only) | V8 ropes make `+=` cheap already; byte-only strings can be `Uint8Array`/Latin-1 one-byte strings | a growable byte buffer; UTF-8 only where the content is non-ASCII |
| regex TIER per literal (native / PCRE / refused) + the parsed flags (B5) | ✗ (the classifier is designed, js-plan §II.8 item 3) | native `RegExp` (V8's engine is fast) for the common subset, PCRE2-WASM only where needed | PCRE2 with JIT; literal patterns compiled once at load |
| dynamic-scope use (`local`, magic globals written) per sub | partly (the #1035 plist could carry it — B3) | skip the save/restore stack for subs that never `local`; magic globals as module-level `let`s | skip the dynamic stack; magic globals as plain globals |
| exception use (`die`/`eval BLOCK` reachable) per sub | ✗ (B3's `:needs`) | no `try` frame where nothing can throw (V8's `try` is cheap but not free in tight loops) | no `setjmp` frame — the biggest C-side cost avoided |
| phase facts (`BEGIN`/`INIT`/`END` present) | ✓ (form order) | run in order; no phase machinery when absent | same |

Three facts on this list are new work for PCL itself and would pay on the CL target too,
so they are Part A candidates as well as Part B ones: the **numeric range proof** (s3xx measured
~10× for native fixnum add once it exists — it was rejected only because it is unsound
without the proof), **element homogeneity** (typed arrays on every target), and **per-sub
exception/dynamic-scope use** (B3's `:needs`, which on the CL target would let `p-sub-frame`
drop its `catch` for subs that cannot `return` non-locally — lever 7 of A.2).

For the two targets specifically, the shape of a FAST runtime follows from the table:

* **JavaScript (Node first, V8):** values are doubles and strings; the IR's class facts decide
  which variables are bare JS values and which are `{v}` cells; blessed hashes are objects
  with a constructor-per-class so V8 gives them a hidden class and inline caches do the
  method-dispatch caching PCL does by hand; arrays with the #1140 facts are plain `Array`s
  (or typed arrays with homogeneity); closures and exceptions are native and cheap when not
  thrown; `local` is a save/restore stack used only in subs whose `:needs` says so; strings
  are UTF-16 with a codepoint layer only where the byte-only fact is absent; regex tier
  native → `RegExp`.  The one thing V8 cannot do fast is perl's IV/NV integer semantics —
  the range proof is the lever, BigInt the fallback.
* **C:** the IR's class facts map to C types directly (that is the whole point of carrying
  them); the capture manifest is closure conversion; `tail-return` and the per-sub
  `:needs` remove `setjmp`/`longjmp` from most subs; GC is the one decision the IR cannot
  make for you — perl-style refcounting keeps `DESTROY` timing identical to perl (and pclxs's
  ABI expects it), a tracing collector (Boehm) is simpler and faster for allocation-heavy
  code but changes destructor timing; the plan's recommendation is refcounting with the
  class facts keeping most values OUT of refcounted cells, which is exactly where perl's own
  cost is.  Regex is PCRE2 with JIT, patterns compiled at load from B5's structured literals.

So the Part B order becomes: B1–B4 (inventory, manifest, `:needs`, host-leak gate) → **B4½:
facts printed on the general form (`--facts`, section B.3)** → B5 (data form + regex literals) → B6
(conformance corpus) → the three new facts (range proof, homogeneity, per-sub `:needs`
consumed on the CL target) as Part A levers.

### B.4 Order and cost, summarised

| step | what | cost | informs |
|---|---|---|---|
| B1 | inventory tool + gate row + `Contract:` docstring tails | small + campaign | the port list |
| B2 | per-program `USES`/`NEEDS`/`FACTS` manifest, `--manifest` | small | feasibility per program |
| B3 | `:needs` on `p-sub` | small | partial compilation |
| B4 | host-leak census gate + the kernel whitelist in ir-spec §11b | small | the target's minimum |
| B5 | `--emit-sexp`/JSON + structured regex literals + escaped control chars | medium, gen bump | parsing without a CL reader; the regex tiers |
| B6 | `tools/ir-conform` + the corpus | medium | correctness of a backend |
| B7 | JS Part II and C notes rewritten as tables over B1/B4 | docs | the reader |

B1–B4 are one Opus agent each (or two per round as the correctness slot); B5 and B6 are
each a round's structural item; none of them changes generated-code speed (B5 is a second
printer, B2/B3 are comments and a plist), so they interleave with Part A under the
one-perf-plus-correctness round shape without competing for it.
