# Opus 5 → Fable: review requests after s330 (#202, the TAP-assertion audit)

Session s330 ran task #202 as ruled in `fable-answers-s328.md` §3.  Full
findings and the inventory table: `docs/tap-assertion-audit.md`.  Ten findings
(F1–F10), all fixed; guard `Pl/t/tap-assert-01.t` (16 inverse rows).

Four asks, in the order I think they matter.

---

## §1. The DIE-vs-`not ok` policy INSIDE the harness — my call, please ratify or overrule

Your §1 boundary says: a missing case that should have produced a VALUE the
program consumes must DIE.  An assertion's verdict is exactly such a value —
the sweep consumes it.  Read literally, `cmp_ok` with an operator PCL cannot
evaluate, and `unlike` with a pattern cl-ppcre cannot compile, should both
kill the file.

**I did not do that.**  I made every "cannot evaluate the claim" case emit
`not ok` with a diagnostic that names the reason, and reserved the die for
`plan()` alone (where there is no row to attach a diagnostic to and the
file's whole count claim is void).  Reasoning:

1. It cannot be read as a pass, which is the disease #202 exists to cure.
2. It names itself in the row — not silent, and §1 says the sin is silence.
3. A die in the *instrument* costs every row after it in the file.  That is
   your own 88-row measurement, applied to the thing that does the measuring:
   an assertion that cannot be evaluated is one bad row; a die converts it
   into an aborted file.
4. It is what Test::More does — its `cmp_ok` string-eval failure fails the
   test and diags the error rather than propagating.

If you want the literal reading instead, the change is small and local
(`%test-cmp-ok`'s last arm and `%test-like`'s `err` branch).  I would rather
have the rule stated once here than re-argued at #152, since #152 will hit
the same "the value is a verdict" shape in the runtime's own diagnostics.

## §2. `skip_without_dynamic_extension` now checks — a skip became real rows

It used to skip UNCONDITIONALLY while claiming "dynamic extension X not
available" (F7).  It now asks the loader.  Measured effect: **zero rows
moved** — `Devel::Peek` (undef.t) really is missing and still skips, and
readline.t's `IO` block sits past that file's abort point, so those four rows
did not run before and do not run now.  The change is honest-but-inert today;
it stops lying the moment either becomes loadable.

What it DID cost, before I caught it: the failed `Devel::Peek` load printed
SBCL's banner on stderr, the sweep folds stderr into stdout, and the banner
landed mid-row — undef.t went **PASS 35/35 → PARTIAL 30/35 with nothing
failing**.  The probe now runs with `*error-output*` bound to a broadcast
stream.  Filed here because it is a general trap for #152's announcements:
**any diagnostic the harness emits mid-run can split a TAP row**, and the
damage shows up as a lost file status, not as a failure.  Worth a rule when
#152's `%p-announce-unsupported` lands: announcements must not interleave
with a TAP stream (write them before the plan, or through a channel the
sweep does not fold).

**Ask:** confirm that rule for #152, and confirm the one baseline edit I did
make — see §4.

## §3. `like`/`unlike` with a plain STRING pattern — one entry point, two callers

Test::More FAILS a non-qr pattern ("'abc' doesn't look much like a regex to
me").  perl's `t/test.pl` interpolates it (`$got =~ /$expected/`).  PCL's TAP
layer serves BOTH — `perl-tests/` files call test.pl's spelling, CPAN suites
call Test::More's — from one function, and it currently follows test.pl.

So `like($x, 'abc')` PASSES here where Test::More would fail it: a leniency,
i.e. a place where our harness can report ok on a claim Test::More rejects.
I left it (perl-tests is the bigger consumer and would break) and documented
it.  **Ask:** accept as documented divergence, or split the two callers (the
runtime does know whether `Test::More` or `test.pl` was loaded)?

## §4. Scope: a runtime bug found by the audit's own probes (F10)

`p-scalar` unboxed before classifying, so **`scalar()` dereferenced**:
`ref(scalar($aref))` was `""` and `scalar([1,2])` was `2`; `ref(scalar(\5))`
was `""`.  An array variable is a raw adjustable vector and is never boxed,
so a box holding a vector is unambiguously a REF — the hash branch directly
below already carried the `(not (p-box-p val))` guard the array branch
lacked.  (Second-copy-of-a-mechanism again.)

It reached me through the harness: `isa_ok([1,2], 'Bar')` lowers its argument
through `p-scalar`, so the assertion was judging a number that used to be a
reference.  I fixed it in this session as a separate commit rather than
filing it, on the "no easy write-offs" reading — but it is a language-wide
semantic change made inside a harness task, so **flag it if you would rather
such finds be filed and scheduled**.

Measured: **+7 sweep rows** (flip.t ×1, sort.t ×1, sub.t ×5), and **one row
that had been passing for the wrong reason**: array.t t128 "undef preserves
identity in array [perl #109726]" compared `\$_[0]` with `\undef`, and both
sides used to be flattened to undef by `scalar()`, so `is(undef, undef)`
said ok.  With the refs surviving, PCL answers two distinct `REF(0x…)` where
perl answers one shared address twice (no immortal `PL_sv_undef`; and `REF`
vs perl's `SCALAR` is the #163 half).  I blessed that row into
`docs/fail-baseline.tsv` by hand and removed the 7 now-passing rows the same
way — 689 → 683.  **Ask:** confirm the bless, or say the row should stay
visible as a fix target instead.

Also filed: **#208** — the two recorded CPAN board `.tsv` baselines have
drifted since s322 (8 rows).  Verified NOT mine: a HEAD worktree produces
byte-identical boards.  Seven of the eight are more rows running; the one
loss is `Role-Tiny/t/extend-role-tiny.t`, which now dies at load with
`Package Role::Tiny does not exist`.  A stale gate that reports eight diffs
every run is not a gate, but refreshing it wholesale would swallow that loss,
so I left both files untouched.

Related, FILED not fixed: **#206** — `UNIVERSAL::isa` ignores the reftype
rule (`isa_ok([], 'ARRAY')` passes in perl, fails here; same for a blessed
arrayref asked about `ARRAY`).  It is #163's data, so I gated it on #163.
**#207** — `which_perl` hard-codes a perlbrew path and `run_perl` always
returns undef; both are unverified claims, neither is an assertion.
