# Direction-D implementation plan (task #289): symbol-macro globals

**Status: PLAN, s382d (2026-08-11, Fable).  The audit is
`docs/defglobal-audit-s382.md` (GO, no third class); this file adds the
USER-requested performance verification and the ordered implementation
steps.  Executable by Opus; every design decision below is probed, none
are open.**

## 1. The mechanism (final, after three probe rounds)

    (define-symbol-macro $x (sb-ext:symbol-global-value '$x))
    (setf (sb-ext:symbol-global-value '$x) (make-p-box nil))   ; init

One symbol per global.  `sb-ext:symbol-global-value` reads/writes the
global cell directly, skipping the thread-local binding lookup — valid
here because partition symbols are NEVER dynamically bound (no special
proclamation, no progv, locals go through the same accessor), an
invariant that holds by construction.  Name-based access from the
runtime (`symbol-value`, `boundp`, `makunbound`, glob/symbolic-ref
helpers) interoperates with the same cell unchanged — probed, including
the `(setf symbol-value)` writer.  The **exception set stays `defvar`**:
`$a`/`$b` in every package (sort pair) + the runtime-magic list
(`@_ $_ $@ $1..$N %ENV %SIG @INC $?` + punctuation) — name-decidable,
image-global, and SBCL turns any partition mistake into a loud load
error (special and symbol-macro exclude each other both ways).

Rejected by probe (rationale on record):
- `sb-ext:defglobal` — SBCL forbids `let` of a global name entirely; the
  review's premise died on the first probe.
- plain `(symbol-value '$x)` access — reads +9%, and `local` via the
  TLS-checking setter is 8.4× (404 ms/10M).
- `progv`-based local — 4.8× (217 ms/10M).
- two-symbol (macro → separate `defglobal` cell) — matches sm1g's speed
  but every name-based runtime helper (glob save/restore, symbolic
  refs, eval free names, stash) would need a name→cell mapping; a missed
  site is a wrong-cell read.  Recorded as the upgrade path ONLY if a
  profile ever shows `local`-on-ordinary-globals hot.

## 2. Performance evidence (SBCL 2.6.0, best-of-3, realistic boxed shapes)

Emitted code keeps a p-box in the cell and mutates the BOX on
assignment (`p-scalar-=`), so the cell is read-mostly; these shapes
mirror that.

| path (per-op cost) | today (defvar) | direction D (sm1g) | delta |
|---|---|---|---|
| READ cell→box→value, 100M | 79 ms (0.79 ns) | 63 ms (0.63 ns) | **−20%** |
| CALL: sub reads global, 10M | 19 ms | 17–19 ms | parity |
| MY-shadow bind, 10M | 46 ms (4.6 ns, dynamic) | 29 ms (2.9 ns, lexical) | **−36%** |
| LOCAL ordinary global, 10M | 46 ms (4.6 ns) | 410 ms (41 ns) | **+9× micro** |
| LOCAL magic var ($_, $/, $@…) | 46 ms | unchanged (stays defvar) | 0 |
| WRITE through the box | 65 ms | 50–74 ms by variant; box path unchanged | ~0 |

**The flagged regression (s379b conjunct 3 requires flagging): `local`
of an ORDINARY user global costs ~41 ns instead of ~4.6 ns** — the
unproclaimed-symbol setter is a full call, and the only way to open-code
it (a global proclamation) forbids `let`.  Why this is acceptable: the
construct is rare (54 scalar + ~20 container sites in the whole corpus
emission, none in hot loops), the absolute cost is two hash-lookups'
worth, and the `local`s that DO run hot in real code target magic vars,
which keep today's fast dynamic bind.  The end-to-end gate (§4 step 2)
verifies this stays invisible at program granularity.

Benchmark validity: loops sink into a summed fixnum so nothing folds;
disassembly confirms per-iteration cell reads (special 25 instrs, sm1g
≈20); `my`/`local` loops allocate a fresh box per iteration on BOTH
sides so allocation cancels.

## 3. Semantic changes (all toward perl; each gets a probe + guard row)

1. A called sub no longer sees a caller's `my $x` that shadows a global
   `$x` (perl: correct; today: leaks).
2. `$$name` symbolic deref under a `my $x` shadow reads the PACKAGE
   variable, as perl does (today it wrongly reads the `my`).
3. `local` still installs a NEW box and restores on die/exit —
   `unwind-protect` probed, incl. through a signalled error.
4. Reads of a never-assigned global still yield the initialized
   undef-box (0 bare defvars measured; the init pass is unchanged).

## 4. Steps (each its own commit, each gated)

**Step 1 — runtime: `p-local-cell`.**  A macro doing
save/install/restore via `symbol-global-value` + `unwind-protect`,
mirroring `p-local-glob`'s idiom; scalar flavor installs
`(p-box-for-local v)`, container flavors install the copied
array/hash box exactly as today's `local @a`/`%h` inits do.  Export it;
paren check; note the `sb-ext` dependency beside the existing
SBCL-internals uses.  Gate: `tools/prove-core`.

**Step 2 — the flip (one commit).**  In the forward-decl emission
(`_forward_global_decls` / `_insert_variable_forward_declarations` and
the region-qualified defvar sites):
- ordinary global → `define-symbol-macro` + cell init (the partition
  predicate lives in ONE shared function — rule 11 — consulted by decl
  emission AND `local` lowering);
- exception-set name → today's `defvar`, byte-identical emission;
- `local` lowering: ordinary → `p-local-cell`; exception set → today's
  dynamic `let` (incl. the s380 sort-pair `declare special` path,
  untouched);
- eval-mode inherits automatically (same emitter);
- `docs/ir-spec.md` data-model + load-model sections updated IN THIS
  COMMIT (pre-v0.1 IR rule, USER s379c);
- `*pcl-cache-generation*` bump (a stale cached `defvar` against a new
  symbol-macro dies loudly at load — correct, but must not be reachable
  warm);
- probe + guard rows for §3.1–3.4 in `Pl/t/transpile-test-10.t` (new
  file — the -09 wall-time anchor rule).
Gates: Pl/t cold; **full sweep with TOTAL/LOST** (corpus-diff will be
total by design — every global-bearing file changes — so the sweep and
board carry the verdict; expect and individually explain rows where an
accidental dynamic previously masked or produced a result);
**CPAN board vs baseline**; **`tools/bench-exec.pl` before/after**
(existing benches must be within noise) plus one `local`-heavy
microbench recorded in the commit message; regenerate
`cl/pcl-pack.lisp`/`cl/pcl-mro.lisp` (checked-in artifacts rule).

**Step 3 — delete the poisoned-my machinery, per family.**
`__shadow__` (611 corpus bindings), `__cond__` (194), `__emb__` (4):
each family's rename pass + veto predicate + counter + eval-alist strip
entry goes in its own commit with Pl/t + targeted family tests + sweep
on the 3rd (cadence rule).  #205 closes with the last one.  The
`__file__`/`__state__` families STAY (span/capture/state — different
causes).

**Step 4 — the extra net.**  `tools/difftest-ops.pl` fuzz run (the
rename/capture/interp axes exercise exactly this machinery), plus the
full perl-suite companion sweep.

## 5. Risks and rollback

- Every risk class dies loudly (SBCL partition errors, unbound cells),
  never silently — rule-12-shaped by construction.
- Step 2 is a single commit: revert + gen bump is a complete rollback.
- Threads: `symbol-global-value` is not thread-local; PCL is
  single-threaded — recorded constraint, unchanged from the audit.
- Sequencing: after the #237 Opus wiring half (both bump the cache
  generation; wiring is queued first), before the v0.1 mechanical
  track.  Step 3 may interleave with other IR-batch items.
