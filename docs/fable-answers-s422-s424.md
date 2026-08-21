# Fable rulings on the s422 / s424 batches (2026-08-22, written in s421 at session end; s423 pending)

Three Opus 5 agents ran in parallel worktrees off `a2ac578` (launched by the
s421 Fable session at the USER's request): **A = s422 / #419**, **B = s423 /
#418 (widened)**, **C = s424 / #423**.  A and C finished, were reviewed and
MERGED onto main (`c84dd45` ff, then merge `d655da6`); the gate on the merged
tree is **156 files / 5639 rows, failures exactly the 13 pclxs xs rows**.
**B had not finished when the session ended** — see §B.

## A — s422 / #419 (`docs/opus5-review-requests-s422.md`): APPROVED as shipped

* One writer (`_cl_string_literal_form`), four private copies folded in (rule 11),
  the runtime form through the rule-12 family.  Re-verified: probes (dead-code
  literal costs nothing, `eval` traps with the named code point, `\x{10FFFF}`
  intact, `s/b/\x{4000000}/` dies at the expression); gate on main 156/5622.
* **ASK RATIFIED: the asymmetry stands** — `chr(N)` at run time → U+FFFD (s318
  §11, registered) and a LITERAL → dies.  The compiler holds a literal it cannot
  represent and refuses loudly; the alternative (literal → U+FFFD) is a
  measured accidental-pass family (pat.t's ANYOFH block).  Caveat recorded as
  **#442**: the chr side is itself not uniform (a raw-string-slot verdict
  answers 65533, the boxed path 67108864 = perl) — if s318 §11 is revisited,
  both sides move together.
* **#424 (re/pat.t's two walls) RULED: option (b)** — a PER-FILE heap
  allowance registry beside `docs/perl-suite-timeouts.tsv` (same shape:
  rel<TAB>MB<TAB>cause), consumed by `tools/run-perl-suite.pl` through ONE
  new PCLSbcl parameter (the builder stays the only place a command line is
  made); the default dynamic space stays at SBCL's 1 GB so the `--jobs 8`
  memory budget is unchanged for every unregistered file.  re/pat.t registers
  3072 with the measurement; wall 2 (`Failed to match` after ~365 rows) is
  chased behind it with a live stderr capture.  Opus item for the next
  session (plan-post-s420 O3 gains it).
* #425 (cl-ppcre's own message for a >U+10FFFF pattern) stays a filler.

## C — s424 / #423 (`docs/opus5-review-requests-s424.md`): APPROVED as shipped

* The measurement answered the task's question: glob VALUE vs glob REF differ by
  the EXISTING `p-box-is-ref` flag (box-nv read it since #163); fix shape (a);
  the s335 no-ref-kind ruling is CONFIRMED by measurement.  Re-verified:
  p8/pC2 probes byte-identical to perl; op/gv.t 49/48 → 61/36 spliced with
  cause; pass-baseline substr.t 375 edited by hand in the merge.
* **Ask 1 (scope — three copy paths beyond `box-sv`): ACCEPTED** — one
  invariant, one predicate, every reader and copier named in ir-spec §2.5.
  Nit for the next touch of the area (#436/#437): the two copy arms repeat a
  3-line "fresh box carrying is-ref" snapshot — extract a `%p-glob-box-copy`.
* **Ask 2 (`p-aref-unbox-elem` hot path): NO COST** — the function is not on
  the plain `$a[$i]` read path (0 calls in either emission), and under
  `PCL_OPT=none` 20M reads are equal within noise (2.47 s both trees,
  interleaved).  The added test is one struct typep before the
  `%p-dualvar-box-p` call it precedes.
* **Ask 3 (`ref(*foo)` now `""`): KEEP perl's answer**, no lenient belt — a
  belt would hide exactly the stripping path it fears.
* #436 (lexical filehandle: `ref($fh)` should be GLOB — needs its own io/ leg),
  #437 (`\*foo == \*foo` false — intern typeglobs per (package, name)), #438
  (`*b = $pkgvar` holding `\*foo` installs the SCALAR slot — crash) are
  queued as O3 fillers in value order #437 → #438 → #436.

## B — s423 / #418 widened: NOT FINISHED at session end

Its worktree (`git worktree list`: `agent-ac24c08623154686e`) held ~10 modified
files (Pl/CLForm.pm, ExprToCL.pm, GlobalPartition.pm, Parser.pm, Parser2.pm,
cl/pcl-runtime.lisp, …) and NO commit when the session ended; it had run
`tools/emission-ab.pl` (the ASCII-byte-identical inverse guard).  **Next
session: `git -C <that worktree> diff` first** — if the agent finished and
committed, review its `docs/opus5-review-requests-s423.md` exactly as A and C
were reviewed, merge it onto main, THEN renumber `*pcl-cache-generation*` once
(v2-167), regenerate the three artifacts, run one COLD gate + one full sweep
on the final tree; if it did not commit, read the diff before redoing #418.
The generation on main is C's "v2-166" until then (the artifacts match it).

## Standing rule from this session's shape

Parallel Opus agents on INDEPENDENT tasks are a wall-clock win (the
model-bound halves overlap; the machine-bound sweeps at `--jobs 3` cost the
same as one `--jobs 8`); they need distinct cache generations, a `PCLXS_DIR`
export, and a merge that renumbers ONCE at the end.  Prepended doc sections
merge theirs-then-ours (newest session first).
