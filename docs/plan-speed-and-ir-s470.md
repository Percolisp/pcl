# Plan: total speed (the low-hanging apples) and the IR as a contract for other targets (s470, Fable, 2026-09-05)

*USER's framing: "what to do about speed — total speed, get the low hanging apples; there
are enough features faster than Perl that we don't need to stare at those larger than 1×"; then,
sharper: "forget beating Perl in individual items, we do enough for that — just try to get PCL
as fast as possible" —
and "how we should extend the IR to inform the users about what compilation from IR to
JavaScript or C would need".  Two plans, one file.  Every number is from the quiet-box board
(`docs/faster-codegen-suggestions.md` §0.2i, s467) plus today's moves (A5 `sortnum` 7.4× → 3.2×,
`sortstr` 3.5× → 1.9×; BJ `pushloc` 0.48× → 0.28×, `arrhash-k` 1.15× → 1.03×).*

---

## Part A — Speed

### A.0 The metric (USER, 2026-09-05): PCL as fast as possible, in ABSOLUTE time on real programs

"Forget beating Perl in individual items, we do enough for that."  So the board's
pcl/perl ratios stop being the steering instrument.  The metric is **PCL's own seconds on
representative programs**, and a lever is ranked by **how much of those seconds it removes**
(profile share × how many real programs hit it) divided by its cost — regardless of whether
the row involved is at 0.3× or 3× of perl.  A hot path that is already "faster than perl"
is still a target if it is where the cycles go; a 2× row is not a target if real programs
spend no time there.  perl's column stays on the board as a sanity reference and the ten
winning rows stay as the CONTROL rows every perf agent prints beside its numbers, nothing
more.  (`docs/where-the-time-goes.md` is the earlier profile-driven record; this plan
re-instates that method with macro programs as the subject.)

Method, unchanged: every transform is a NAMED Kind-A/Kind-B emission in the registry
(`PCL_OPT=none` runs identically — `Pl/t/passes-01.t`); a change is SIZED first by the
hand-replaced A/B (s469bh's `ab-lisp.pl` method: the same transpiled program with one form
replaced, one core, interleaved series, best-of-K, a byte-identical control pair in the same
window); one perf agent per round; the merged tree's sweep is the correctness bar; a row that
gets slower anywhere is a stop.

### A.1 The yardstick first: macro programs, their constant terms, and a PROFILE of each

The board has nineteen micro-rows and no macro-row; a user's program is a mix plus constants.
Round 27's perf agent adds THREE macro rows to `tools/bench-exec.pl`, measures each at N and
2N (the intercept is the constant term, the slope the per-iteration cost), and PROFILES each
with `sb-sprof` on the quiet box — the profile, not the ratio, is the deliverable:

| macro row | what it exercises | why this one |
|---|---|---|
| `json-rt` | `JSON::PP` encode+decode of a 50 kB nested structure, ×N | the commonest pure-Perl CPAN workload: hashes, arrays, string building, `sprintf`, regex, `pack`/`unpack` of unicode, method calls |
| `moo-objs` | a `Moo` class with three attributes, ×N constructions + accessor calls | the OO mix: `bless`, method dispatch, closures, `local`, argument passing |
| `textproc` | `Text::Balanced`/`Text::Wrap`-style line processing of a 1 MB string, ×N | regex-heavy string processing with `pos`, `substr`, `.=`, `split`, `join` |

And FIVE constant terms measured on their own, each a one-line program timed end to end:

| constant | how | today's guess (MEASURE) |
|---|---|---|
| startup | `pcl -E 'print 1'` | the saved core makes this ~0.1–0.2 s; confirm |
| module load, warm cache | `pcl -E 'use JSON::PP; print 1'` twice, second run | the cached transpile's LOAD time (fasl or source?) |
| module load, cold cache | same after `pcl --clear-cache` | the transpile itself — what the FIRST run of any program pays |
| **extension load** | `pcl -E 'print length pack("N",1)'` | **the `pack` rows' 1000× IS this**: `cl/pcl-pack.lisp` is loaded lazily from SOURCE and compiled at load — ~3 s in every program that touches `pack`/`unpack`, and CPAN uses them everywhere (Digest, Storable, Encode, JSON::PP's unicode path) |
| string eval | `pcl -E 'eval "1"; print 1'` | the `pl2cl --server` spawn: a perl process + PPI load, ~0.3–0.5 s, once per program that evals |

**The output of round 27 is a RANKED TABLE**: every runtime function or emitted shape with
≥ 2 % of the cycles in any macro row, with the constant terms as rows of their own, each
with its candidate lever from A.2 (or "no lever known — needs a design").  Rounds 28+ take
that table from the top.  This is the same discipline that found the 2400× string buffer
and the 13× raw slot in s3xx (`faster-codegen-suggestions.md` §0.5): profile, then emit.

**Decisions the constants will force, in the order the numbers are likely to rank them:**

1. **The extension load is very likely the biggest single apple.**  If ~3 s is confirmed,
   the fix is one of: (a) compile the three checked-in extensions into the saved core (it is
   content-keyed, so a change makes a new core — no staleness); or (b) cache each extension's
   FASL under `~/.pcl-cache/` keyed like the core.  (a) is simpler and makes `pack` cost what
   a builtin costs; the per-call cost that remains is #74's subject (constant-template
   memoization, ~5× measured s3xx).  Acceptance: the `pack` row and `perl-tests/pack.t`'s 54 s run.
2. **Module load** — if the cached transpile is loaded from source each run, cache the FASL
   too (the same mechanism as 1b).  If it is already a fasl, nothing to do.
3. **String eval** — if the server spawn dominates a program with one `eval "…"`, the lever
   is a persistent `pl2cl --server` kept warm across runs (an idle-timeout daemon) — a Fable
   design (a listening compiler's security, per-user socket), NOT before the number says so.
4. **Startup** — only if > 0.3 s.

### A.2 Candidate levers, by absolute cost class (the profile ranks them; this lists what is known)

The micro-row work left a catalogue of KNOWN causes.  They are listed here by the kind of
cost they remove, so the profile can pick them up by name; the ratio column is gone on
purpose.

| cost class | where the cycles go | lever (known or to size) | size |
|---|---|---|---|
| **constant terms** | extension load (~3 s), module load, compiler spawn for string eval, startup | A.1 items 1–4 | small–medium |
| **calls** | every user-sub call pays the `p-sub-frame`/`%p-leavesub` protocol (#964), argument flattening into `@_`, the context bind; CPAN code is call-dense even where the bench loops are not | measure the per-call floor on `moo-objs`; levers: `raw-params` coverage (which subs still take the boxed `@_` path), the `tail-return` family, a leaner frame for subs proven not to `goto`/`wantarray`/`caller` | measure first |
| **method dispatch** | the string-keyed dispatch with memos (#73 cache-free); every `use overload` operation looks the handler up per op | per-CLASS handler + method cache (#582), blocked on `@ISA`-write invalidation — a Fable design | medium |
| **aggregates** | keys and lookups, not value boxes (§5 of the suggestions): hash key stringification, `p-gethash` on non-constant keys, slices materialising temporaries (`slices`, `sliceasgn`), list assignment from ranges (`arrfill`), multi-array foreach flattening (`feread2`) | raw-element slice fast paths on the #1140 facts; `p-array-=` filling from a range; foreach-raw over a list of arrays; const-key slice assignment as N stores | medium |
| **sort** | after A5 the collector copy, the decorate, the generic `stable-sort`, and the result COPIED into `my @x` | `p-array-=` ADOPTS a fresh vector (licensed by A5's own consumer analysis); a typed merge sort for all-fixnum / all-string keys | small / medium |
| **symbolic access** | `${"name"}` resolves the string per access | `symref-const`: a constant string resolves once per site | small |
| **strings** | `.=` is buffered (#62/#881); what remains in `strcat`, and `sprintf`/`pack` templates re-parsed per call (#74) | profile `strcat`; #74 memoization | measure / medium |
| **regex** | the cl-ppcre engine: `/./g` scanning, exponential backtracking (#196), `pos` quadratic on long strings (#477) | #71 PCRE2 via sb-alien — the one lever that is also a PARITY lever; a Fable design, its own round | large |
| **the general form** | `PCL_OPT=none` is the correctness twin; anything the profile finds in a Kind-A path must be re-measured with the name off, so the optimisation and not the general form gets the credit | rule, not lever | — |

### A.3 The rounds (proposed order; each round = ONE perf agent + the correctness agents)

1. **Round 27 perf = A.1**: the three macro rows, the five constants, the `sb-sprof` profile of
   each macro row → the RANKED TABLE; the extension load moved into the core (or fasl-cached)
   in the same round if it is the ~3 s the pack rows say; #74 sized.
2. **Round 28+ perf = the table from the top**, one cost class per round where the levers
   share a mechanism (the aggregate family together; the three small items — symref-const,
   sort-result adoption, multi-array foreach-raw — together).  Each lever is SIZED by the
   hand-replaced A/B before compiler work; a sizing under 20 % of the row closes the lever by
   measurement.
3. **Fable designs in between**: the per-class cache's `@ISA`-write invalidation (#582); the
   PCRE2 backend (#71 + #196 + #477), which is also Part B's regex question; the string-eval
   daemon if A.1 item 3 says so.

What "done" looks like for Part A: the three macro rows' constant terms named and cached;
each macro row's profile flat (no function above ~10 %) or its top entries owned by a filed
lever with a sizing; the general-form compiler (`PCL_OPT=none`) still running everything
identically.

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

### B.2 What each target must supply — the table B4 fills in

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

### B.3 Order and cost, summarised

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
