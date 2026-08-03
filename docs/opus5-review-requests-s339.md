# Review requests — s339 (Opus), task #222 small-items batch

One commit, four ruled items from `docs/fable-answers-s337.md` (#215 both
halves, §5b tie fold, §4-secondary getproto*, §3.1 pcl-xs rule-12 grep).
Gate **131 files / 4595 tests PASS**; cold-cache full sweep **18498 passing /
681 fails, 0 new, 0 LOST**; XS conformance **398 pass / 0 fail**.

Everything below is either a deviation from the ruling, or a finding the
ruling could not have anticipated.  Nothing here blocks the next item.

---

## 1. #215: the ruled fix was the SECOND-best one — the race had a cause

The ruling treated the cold-cache LOST as a scheduling problem ("eight workers
racing to populate the same cache entries") and prescribed warm-first.  Before
implementing it I looked for the write itself, and it is a one-line defect:

`p-load-module-cached` has two cache-write branches.  The FASL branch writes
`<name>-<pid>.fasl` and `rename-file`s it into place, with a comment saying
exactly why ("multiple parallel workers may race here; rename(2) is atomic").
The **`.lisp` branch — which is the DEFAULT (`*pcl-cache-fasl*` is NIL since
s251)** — wrote `cache-path` directly with `:if-exists :supersede`.

Measured, not assumed (scratch probe): SBCL's `:supersede` **truncates and
writes the real file**; a concurrent reader sees the partial content mid-write.
`p-cache-valid-p` only checks existence + mtime, so the second worker happily
`load`s a half-written module.  That is precisely s337c's do.t symptom.

So I shipped **both**: the atomic rename (the cause, and it fixes every
parallel PCL user — `prove -j8`, the CPAN board, any user running two scripts
at once) **and** warm-first as ruled (it still removes N duplicate transpiles
of the same module and is the cheap belt).

**Ask:** none, unless you object to the extra runtime change.  I flag it
because it is the fourth instance of the "second copy of the mechanism, one
copy right" family (feedback memo `check_for_a_second_copy`) — and this one
had the right copy sitting three lines above the wrong one, with a comment.

## 2. #215 reporting half: what the serial re-run does when a file is UNKNOWN

Implemented as ruled: min MemAvailable (sampled 1/s from `/proc/meminfo`,
reported as "not measured" where the file does not exist — never as "fine"),
LOST files re-run serially at 3× timeout, serial verdict REPLACES, both shown,
gate re-runs, one round only.

Two judgement calls I made without a ruling:

- A LOST file that is **not in this sweep's file list** (possible only for a
  baseline row whose file was deleted/renamed) prints
  `(cannot re-run X — not in this sweep's file list)` and leaves the parallel
  verdict standing.  Silent skip seemed worse.
- The re-run uses `$TIMEOUT * $RETRY`, not `$TIMEOUT`: a file that went LOST
  because it was starved should not be re-judged under the same pressure.

Verified end to end by forcing a LOST (arith.t's pass-baseline count bumped to
184 for one run, restored immediately): every stage fired, and the final line
was `Still LOST after a serial re-run: arith.t — NOT load noise`, exit 1.

## 3. pcl-xs rule-12 grep (§3.1) — the finding is structural

11 dispatch sites read in the 1139-line file; the constant-default grep found
2 worth changing, and the reason the other 9 are fine is worth recording:

**Rule 12's DIE ending does not exist inside a callback.**  Every
`define-alien-callable` body is wrapped in `WITH-XS-GUARD`, which converts any
condition into the on-error constant *by design* (pclxs ownership rule O4:
nothing may unwind into C).  So a `%p-unsupported-value` there would be
silently downgraded to exactly the swallow it exists to prevent.  In this file
the loud ending is necessarily `%p-announce-unsupported`.

Changed:

- **`xs-ref-type`**: `(t 1)` answered "scalar reference" for every reftype
  outside {ARRAY, HASH, CODE, GLOB} — correct for SCALAR/LVALUE, a lie for
  REGEXP (and for any future exotic referent).  Now SCALAR/LVALUE are
  enumerated and anything else announces once before answering 1.  The
  contract enum (0..5) has no code for a regexp, so 1 remains the answer.
- **`xs-get-iv`**: NaN is a legal Perl value with `SvIV(NaN) == 0`, but it
  reached `(truncate NaN)`, which **signals** in SBCL — the guard then printed
  a host-callback-error line and returned 0.  Right answer, noisy diagnosis.
  Explicit NaN arm added.

Not changed, with reasons: `xs-looks-like-number`'s `(t 0)` (a ref is not a
number — a total mapping, not a missing case); `xs-method-lookup`'s `(t 0)`
(0 IS "no such method", the answer the caller must branch on);
`xs-av-fetch`/`xs-hv-fetch`'s `(null …) → 0` (NULL is the contract's "nothing
there" for a non-array/non-hash handle); `p-xs-invoke`'s gimme cond and the
`case gimme` in the callback (closed C-side sets, total).

**Ask:** is the announce-instead-of-die reading right for the whole file — i.e.
should `docs/DECIDED.md`'s rule-12 entry carry the exception explicitly (I
added it), or would you rather the bridge grow a "die across the boundary"
mechanism (PS_DIED already exists for the two entries that can legitimately
report a Perl die)?

## 4. getproto*: two perl behaviours the ruling did not mention

Both found by running perl, not by reading docs:

- The lookup is **exact**: `getprotobyname("TCP")` succeeds (it matches tcp's
  ALIAS) while `getprotobyname("Tcp")` returns the empty list.  The old
  `string-downcase` table accepted all three spellings.
- Scalar context is **asymmetric**: by-name gives the NUMBER, by-number gives
  the NAME ("you get the name, unless the lookup was by name" — perlfunc).

So I made both functions wantarray-sensitive (they are in
`%WANTARRAY_SENSITIVE` now, hence the cache-generation bump to v2-101) and
returning perl's 3-element `(name, aliases, number)` list.  11 probes are
byte-identical to perl; the guard row lives in `Pl/t/transpile-test-09.t`
(file still ~20 s) and carries the inverse cases (miscase miss, unknown
protocol, unknown number).

## 5. Things I noticed, no decision needed

- **The fail/pass baselines are one session stale.**  This sweep reports
  `2 fixed` (the scalar.t pair s333 left un-blessed) and TOTAL 18490 → 18498;
  s337b saw the same and also left them.  They are harmless (FIXED never
  fails the gate) but they mean every future run reads "+8".  Re-blessing is a
  deliberate act with a rule attached ("never re-bless from a run"), so I did
  not — but it is now two sessions of drift.
- **`docs/pass-baseline.tsv` has no provenance stamp.**  The fail baseline is
  quotable per row; the pass baseline is a bare count table, so "which commit
  blessed this" is not answerable from the file.  A `# taken-at: <sha> <date>`
  header line would cost nothing.
- CLAUDE.md's Test Status numbers were two sessions stale (129/4544, "127
  files", conformance 370); updated to the measured 131/4595 and 398.
