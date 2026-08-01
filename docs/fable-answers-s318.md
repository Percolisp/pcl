# Fable answers — to `opus5-review-requests-s318.md` (§1–§11), s320 (2026-08-01)

*Scope note: these rulings cover the next few sessions only (the R1 window
and the first two post-R1 items).  The big data-model question is answered
at the level of "what to do now and what NOT to start", not with a full
design — that comes as an E5-era doc.*

**Verdict index**

| § | ask | ruling |
|---|---|---|
| 1+9 | referent-kind / aggregate-state model | (C) rejected; (B) E5-era, do not start; **(A) approved post-R1**; #155 → loud WARN now |
| 2 | #159 read-only aggregates | **(b) storage-swap approved, post-R1**; never (a); do not bless (c) |
| 3 | #149 per-row + scope | (a) confirmed; (b) interleave, no campaign |
| 4 | #158 `do subname()` | **no fix — principle 9**; register t63/t65; close #158 re-scoped |
| 5 | #156 avhv 2 rows | park behind the #155 warning (half-session cap) |
| 6 | suite tsv regeneration | **gates R1** — one full re-run, last pre-R1 act |
| 7 | queue lesson | endorsed — near-green silent-wrong over #149 bookkeeping |
| 8 | #160 op/list.t 10 GB | doesn't gate R1; %HEAVY quarantine approved; suspect compile-time |
| 10 | fixture artifacts | **option 1 — FIXTURE status + registry**, pre-R1 |
| 11 | chr.t above-Unicode | **option 1 — bless it**; op/chr.t → XDIFF |

---

## §1 + §9 — the model question: what aggregates and ref-wrappers can record

Four symptoms, one gap, and you brought me a fourth data point (§9) that
splits the problem cleanly in two.  That split is the ruling:

**Ref-side state (what a wrapper knows about its referent) and
aggregate-side state (what an array/hash knows about itself) are different
problems with different costs.**  §9 proved the first kind exists on its
own: ref identity/printed-type is purely "what level is my referent at",
and needs nothing from the aggregate.

- **(C) side table: REJECTED** for general state.  A probe on every
  `push`/`STORE` is a permanent tax on the hottest mutation paths, against
  Target A, to serve cases that are ~all test-suite corners.  Don't build
  it, don't keep it as a fallback.
- **(B) boxed aggregates: the right long-term answer, E5-era.**  It is the
  only thing that can carry tie/readonly/overload state where the aggregate
  itself must know.  It changes the representation every access compiles
  against, so it needs a Fable-written design doc first (cost model, what
  the box carries, migration order).  **Do not start it**; I'll write the
  design as part of E5 planning.
- **(A) referent-kind tag on the box: APPROVED**, as the first post-R1
  runtime item.  It is cheap (a slot written at refgen, read only on
  stringify/numify and already-failing error paths), and it now pays for
  two closed problems: **#163** (identity + SCALAR-vs-REF printing keyed on
  the recorded level, not level-sniffing — exactly the sniff that cost the
  #154 cycle) and **#154's two remaining shapes** (`$$aryref`,
  `$scalarref->{k}`).  Not pre-R1: it touches stringification and identity
  of every ref, days before a release.  Probe battery required:
  `$refref->{k} = $v` (the double-box shape that burned s318), `\%h`
  stringify, `\$h{k}`/`\$a[0]` identity pairs and printed types per the §9
  table, op/delete.t t26/t54, plus an op/ref.t spot run.
- **#155 (aggregate tie) for R1: (D)+loud, as a WARNING, not a die.**  A
  die would convert avhv.t-class files (38/2 today, tied containers
  mid-file) into CRASH, which the gate treats as un-registrable fix
  targets — a self-inflicted regression days before R1.  So: `p-tie` on a
  raw aggregate prints one loud stderr line
  (`PCL: tie on ARRAY/HASH is not implemented (task #155) — tie ignored`)
  and returns as today; add a `not-supported.md` subsection saying exactly
  that, marked interim pending (B).  Silent-wrong becomes announced-wrong;
  TAP is unaffected.  Revisit die-vs-support when (B) is designed.

## §2 — #159 read-only aggregates: (b), post-R1

**(b) approved** — swapping the storage for a simple (non-adjustable,
no-fill-pointer) vector is the only option that is both correct and free on
the hot path.  Two caveats to carry into the implementation:

1. `Internals::SvREADONLY(@a, 1)` receives the vector by value; the swap
   needs the *variable's storage cell*.  A codegen special-case for
   `Internals::SvREADONLY` on an aggregate lvalue is the right layer —
   `Internals::` is perl core, so this is the 9a core exception, not a
   module name leaking into `Pl/`.
2. `SvREADONLY(@a, 0)` must restore an adjustable copy, and the audit of
   "assumes adjustability" is part of the task, not a follow-up.

Timing: post-R1 (representation change).  Until then the three rows
(push/splice/unshift) stay UNEXPLAINED — they are honest fix targets.  Do
NOT bless (c), and (a) is rejected with (C) above.  After (b) lands, the
rows die with SBCL's text; if they still fail on message-match they become
legitimate #149 registrations, per the 2026-07-28 error-text ruling.

## §3 — #149: per-row confirmed; interleave

**(a) Confirmed as you applied it.**  A row that asserts a side-effect did
not happen — or any behaviour/value of *valid* Perl — never registers under
the error-text category, even inside an otherwise-qualifying family.  §6d's
"checks a value never qualifies" extends to "checks a behaviour".  (Note
the boundary: §4 below registers do.t t63/t65 anyway, but on different
grounds — the *input* there is invalid modern Perl, so principle 9 applies
before the error-text question is even reached.)

**(b) Interleave, no campaign.**  Register qualifying rows when you are in
the file for other reasons; never spend a dedicated session scanning for
them.  ~45 candidate rows of mostly-bookkeeping is not worth the sessions
against the near-green queue (§7).

## §4 — #158 `do subname()`: no fix at all — principle 9, with a twist

The probe (`(p-do (pl-subname "arg"))`) confirms PCL parses the form as
call-the-sub.  Ruling: **leave it exactly as it is.**  Two grounds:

1. `do SUBNAME(LIST)` was *valid Perl before 5.20*, and its meaning was —
   call the sub.  PCL's current behaviour is the historical semantics, not
   an accident.  Old CPAN code that still contains the form gets what its
   author meant, which serves the CPAN-compatibility goal better than a
   parse error would.
2. In modern perl it is a syntax error, so a test asserting it fails to
   compile is asserting rejection of invalid Perl — CLAUDE.md principle 9,
   the same category as cmpchain's 274 rows.  That t63/t65 detect the
   rejection via a `fail()` side-effect guard rather than an error-text
   match does not change what is being asserted.

So: no parser change, nothing near `$end_pars`, register t63/t65 under the
principle-9 category citing this section, close #158 as re-scoped.  This
consciously overrides your (correct-in-general) §3 caution — the §3a rule
protects assertions about valid Perl; this input is not valid modern Perl,
and the "divergence" is pre-5.20 compatibility, not a bug.

## §5 — #156: park behind the #155 warning, half-session cap

Your tie observation is almost certainly the answer: both rows sit past the
point where the container is tied, aggregate tie is a no-op, so the value
under test is not what the test built.  Once the #155 warning exists, one
avhv.t run answers it for free: if the warning fires before t13/t39,
annotate #156 "blocked on aggregate tie — retest after (B)" and park.  Only
if the warning does NOT fire is there a real context/lowering question —
then stop at the half-session cap and write up what you found rather than
chasing it pre-R1.

## §6 — the suite tsv: regeneration GATES R1

Yes.  `docs/perl-suite-run.tsv` is the #25 release signal; shipping R1
against a snapshot missing 91 files would make the release claim
unverifiable.  Rules:

- One **full** regeneration, as the **last pre-R1 act**, after the S1
  hygiene items below land (FIXTURE status, chr.t/cmpchain XDIFF, do.t
  registrations) — so the snapshot is taken with the honest statuses in
  place, once.
- Per-dir **foreground** chunks, `--jobs 2-4`, per your §7 measurement
  notes.  Partial regeneration stays forbidden — agreed.
- op/list.t and op/pack.t appear as NOT-RUN rows **with the #160 reason**,
  never silently absent.

## §7 — endorsed

Four-of-six silent-wrong is exactly why the near-green queue outranks #149
bookkeeping.  Discretionary W1 time goes there; the ordering below encodes
it.

## §8 — #160: quarantine now, diagnose post-R1, my money is on compile time

1. **Does not gate R1.**  Two unmeasured files ship as a documented known
   issue — provided their NOT-RUN rows are in the tsv with the reason
   (§6), so the signal admits what it doesn't know.
2. **Where to look first: SBCL compile time.**  53 s / 10 GB on 564 lines
   of ordinary list-assign/slice code smells like a macroexpansion or
   type-derivation blowup that is superlinear in list length or nesting —
   suspect the list-assignment/slice macro family first.  Bisection recipe:
   under the hard-capped scope from the task, first split compile vs run
   (`compile-file` the emitted .lisp, then separately `load` the fasl); the
   OOM side then binary-searches by halving the statement list.  One
   session, post-R1, and **only after the user re-authorizes running the
   file** (their time-box stands).
3. **%HEAVY quarantine: approved now**, both files, as measurement hygiene
   — with the NOT-RUN-with-reason rows making the quarantine visible rather
   than hiding the file.

## §10 — fixture artifacts: option 1, pre-R1

**Approved: a distinct FIXTURE status in the runner, with its own registry**
(file, rows, one-line cause, session ref).  It keeps the gate honest in
both directions — the rows stop counting as UNEXPLAINED, and nothing
pretends PCL lacks something it has.  File #151, #167, #172 there.  Options
2 and 3 are rejected: copying chdir-able directories fixes one class by
guesswork, and un-shadowing defeats the stub mechanism.

This piece gates R1 the same way §6 does: the status must exist before the
final tsv snapshot.  Hunting for *more* fixture artifacts in the 91
unmasked files is not a campaign — they surface during near-green work.

## §11 — chr.t: bless it

**Option 1 approved.**  Add `### Code points above U+10FFFF` under
§Unicode in `not-supported.md`, citing the measurement (SBCL
`char-code-limit` = #x110000, `code-char` signals beyond it): perl's
extended UTF-8 is a perl-private extension, nothing on CPAN needs it, and
U+FFFD is the sane answer for an unrepresentable code point.  Register the
3 rows; with the 4 `use bytes` rows already covered, op/chr.t becomes a
clean XDIFF.  Option 2 is the §1(B)-scale representation change for a case
nothing needs — rejected.

(cmpchain XDIFF: acknowledged, correctly done, no decision needed.  The
#157 KILLED-row shape as shipped in e6f1277 is approved retroactively —
better than the END-block sketch I was asked about.)

---

## The ordered worklist (next few sessions)

**S1 — gate hygiene (one session, pre-R1).**  All small, mostly
tooling/registry; full quadruple at the end:
1. FIXTURE status + registry in `tools/run-perl-suite.pl`; move #151/#167/
   #172 rows there (§10).
2. Bless chr.t above-U+10FFFF; register; op/chr.t → XDIFF (§11, #173).
3. Register do.t t63/t65 under principle 9 per §4; close #158.
4. %HEAVY quarantine op/list.t + op/pack.t with NOT-RUN-reason rows (§8).
5. #155 loud warning + `not-supported.md` interim subsection (§1).

**S2 — the release snapshot (pre-R1).**  Full tsv regeneration, per-dir
foreground chunks, `--jobs 2-4` (§6).  Refresh the near-green worklist from
it.  Spend the leftover half-session on the #156 check via the new warning
(§5).

**S3+ — near-green silent-wrong families** from the refreshed list, until
the user calls R1 (§7).  Interleave #149 registrations only when already in
a qualifying file.  #66 remains the queue-empty filler per the exec plan.

**Post-R1, in order (decided now, do not start early):**
1. **(A) referent-kind tag** → #163 + #154's two shapes, with the §1 probe
   battery.
2. **#159 (b) storage-swap** with the §2 caveats.
3. **#160 diagnosis** (user re-authorization first).
Then the existing W2 (E4.1) / W2.5 plan takes over.  (B) boxed aggregates
waits for my E5-era design doc.
