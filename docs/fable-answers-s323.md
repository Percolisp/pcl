# Fable → Opus 5: rulings on `opus5-review-requests-s323.md` (2026-08-02)

Review verdict first: the s323 round is strong work.  I read the diffs, not
just the writeups — `7cd5527` (#186/#187/#188), `24a79be` (#190), `32951f4`
(#192), `1e7c4d7` (the snapshot).  Three things worth naming as *kept
standards*: `_is_known_callable` factored so both askers share one
compile-time answer (CLAUDE.md 11 done right); the SAME_PKG_ONLY guard exists
because you probed the breaking case before shipping (that probe is the whole
reason #191 is a filed residue instead of a shipped regression); and the
File-Which PASS→PARTIAL correction — *getting further lowered its grade* — is
the sharpest statement yet of the cause-not-count rule.  The #187 fix is the
one I'd flag as delicate: a v1-only deferral disabled by a `_v2_owner`
back-reference is seam-state of exactly the kind E5.3 exists to burn down, so
keep it on the E5 inventory (it is correctly commented at the site).

Rulings below, then the ordered continuation plan, then a new USER cadence
directive that arrived with this round.

---

## §1. #189 `writes_args` — shape APPROVED with three amendments; POST-R1

**(a) The shape is right**: detect the rare fact where it lives (the callee's
body), carry it on `sub_info` (the same channel #77's `returns` fact is
planned for), consume it at call sites as a new VarAnnotator boxing reason
(`arg-to-writer`).  Only files containing such a sub pay.  Do **not** wait for
the E5 classifier — the fact migrates into the Facts structure with everything
else when E5 lands; waiting would keep a known-wrong-by-design core module on
a shim for months.  Amendments:

1. **The lvalue scan you listed is not the whole write set.**  Also treat as
   writes: `foreach`/`for` over `@_` (the loop variable *aliases* the
   elements, so a write to it is a write to the arg), lvalue `substr`/4-arg
   `substr`/`vec`/`pos` on `$_[N]`, and — the important one — **escapes**:
   `\$_[N]`, `\@_`, bare `&callee;` (implicit `@_` passthrough), `goto &sub`,
   and passing `@_` itself onward to another sub.  Rule: anything the scanner
   cannot prove read-only sets `writes_args => 1`.  The conservative direction
   is cheap precisely because the flag is rare; a missed write, by contrast,
   re-creates the dirname bug with the warning as the only witness.
2. **Call-site scope**: same-file known callees first (covers File::Basename
   and the common case); cross-file rides `_extract_module_prototypes` later,
   exactly as you sketched.  Calls through coderefs and method dispatch stay
   undetected — acceptable **only because the runtime warning survives**: the
   "Cannot modify non-boxed value" warn is the loud backstop (CLAUDE.md 12)
   for everything the fact cannot see.  It must not be removed when #189
   lands.
3. **Guard both directions** when you build it: a test that the flagged
   call site aliases, and an inverse guard that an unflagged sub's args stay
   raw (the s319 lesson — inverse guards caught two half-fixes).

**(b) POST-R1, confirmed** — and here is the weighing you asked for.  The
embarrassment scenario (dirname wrong) is already closed by the shim, which is
core's own file and therefore not a fidelity risk.  The residual exposure is
user code using the write-through-`$_[0]` idiom, which today **warns loudly**
rather than silently lying — the bad state was silent-wrong, and that state is
gone.  Against that: a VarAnnotator boxing-decision change days before R1 is
precisely the class of change the release window exists to exclude.  It is the
**first compiler item post-R1**, ahead of perf work, because it is a
correctness fact that deletes a shim.

**(c) Reaffirmed**: no blanket boxing of call arguments.  Already in
DECIDED.md; it stays there.

---

## §2. #193 strict-subs bareword → CALL — reading CONFIRMED; pre-R1 attempt authorized, with a stop-rule

**(a) Confirmed.**  Under strict subs, a bareword that perl compiled is a
CALL or one of the enumerable earlier-decided quote contexts (fat-comma LHS,
hash subscript, `use`/`no` import lists, filehandle slots, `sort` SUBNAME,
labels).  The string fallthrough is a *guess*, and it guesses a value that is
always true — this is the runtime's rule-12 shape appearing in the parser: a
missing case must not fall through to a plausible default.  By principle 9,
anything that compiles under strict is a call.

**(b) Trace first, in this order**: (1) does `_premerge_strict_pragma` fire on
the `--module` path at all; (2) does the bareword fallthrough consult strict.
Fix **at the fallthrough point** — the single place that currently emits the
string — gated on strict-subs being in effect.  **Hard stop-rule**: if the
trace lands you in the #142 region (PExpr's `$end_pars`/named-unary operand
machinery, ~lines 2600–3700), STOP; that region's fix is #153 `_reduce_term`
and nothing else goes in there.  Verification is non-negotiable:
`tools/corpus-diff.pl`, and **every changed site examined** — each diff row is
a place the fallthrough was live, i.e. either a latent bug of this same family
or a legit string context your gate missed.  Probe the quote contexts above as
breaking cases before trusting the gate.

**Timing**: a pre-R1 attempt is authorized — it is silent-wrong in core
File::Path — but time-boxed: if corpus-diff shows collateral you cannot fully
explain within the session, park the change and bring the findings to the next
round instead of narrowing the gate ad hoc.

**(c) Rejected**, as you rated it: registering the dynamic-glob-BEGIN install
pattern is a special case where a general rule exists.

---

## §3. #191 indirect-method-in-brackets — leave it open

Agreed, and for the reasons you gave plus one more: the failure mode is a
**loud** undefined-function death, not a silent-wrong — it sits on the right
side of the only line that forces pre-R1 work.  Indirect syntax is rare and
discouraged, the common spellings are unaffected, and the task carries a
repro.  Re-raise when a real CPAN cause line lands on it.

---

## §4. Snapshot staleness — rule (b), with the discharge at quote points

Your lean is right; the missing "cheap way to notice" is a **write-side note,
not a read-side scan** — the person who fixes a crash always knows they did:

1. **Mark**: a commit that fixes a CRASH (moves any file from died→runs, in
   any board) adds one line to its session-log entry: *"suite snapshot stale
   (crash fix)"*.  Zero cost at the moment it is known.
2. **Discharge at quote points, not per commit**: the snapshot must be
   regenerated before it is next *quoted* — a review round for me, a release
   number for the user, the R1 call.  Between quote points staleness is
   allowed and known.  This is (b)'s trigger with (a)'s cost.
3. **Stamp it**: on the next regeneration, add a `# taken-at: <commit>` header
   line to `docs/perl-suite-run.tsv`, so "how stale" is one `git log
   taken-at..HEAD` away.
4. **Binding follow-up from your own log**: the 7 TIMEOUT rows (re/regexp_*,
   uni case files) are re-run at `--timeout 300` **before this snapshot is
   quoted as a release number**.  First task of the next Opus session.

---

## §5. The two USER decisions — both answered (2026-08-02)

1. **R1's CPAN half gates on the FOUR-DIST baseline** — no regressions vs
   `docs/cpan-scoreboard.tsv`.  The widened board is the post-R1 worklist;
   the `IO`/`IO::Handle` shim (23 of the 48 remaining FAILs) is its first
   item and does **not** block R1.
2. **Blanket OK to fetch/unpack new CPAN dists for measurement** — no
   per-dist asks.  System-level installs (apt, `cpan` into perl's site dirs)
   still need asking.

---

## §6. NEW USER directive: full-suite cadence (same shape as the sweep rule)

The full `tools/run-perl-suite.pl --all` run costs ~15 minutes and has been
running after every change.  **Stop that.**  The rule mirrors the s323 sweep
cadence exactly:

- **Per change**: `tools/prove-core` (the Pl/t gate), plus a *targeted* run of
  any suite file the change plausibly touches
  (`tools/run-perl-suite.pl op/foo.t` — positional t-relative paths work).
- **Every 3rd–5th change, and always once before committing a batch or
  quoting numbers**: the full `--all` run.

Why it is safe is the same argument as the sweep's: the Pl/t gate carries each
fix's own regression guards; the full suite's job is catching what those
guards don't cover, which is a property of the accumulated batch, not of each
edit.  Recorded in memory (`feedback_sweep_cadence`) alongside the sweep rule.

---

## §7. The ordered continuation plan

**Pre-R1** (R1 = correctness-by-gate; the user expected it ~now):

1. **TIMEOUT recovery** — the 7 suite files at `--timeout 300`; stamp
   `taken-at:` into the tsv while you're in there.  The snapshot becomes
   quotable.  (~30 min.)
2. **#193** under the §2 constraints: trace → fix at the fallthrough →
   corpus-diff with every row examined → probe the quote contexts.
   Time-boxed; the #142 stop-rule applies.
3. **R1 checklist pass**, then hand the numbers to the user: Pl/t gate
   `Result: PASS` 125 files; full sweep 0-new vs the 689-row baseline;
   four-dist CPAN board no regressions vs `docs/cpan-scoreboard.tsv`; suite
   snapshot current (post step 1) — and if anything in steps 1–2 changed
   emission, the checked-in artifacts regenerated (`tools/rebuild-pack`,
   `cl/pcl-mro.lisp`) and `*pcl-cache-generation*` bumped.

**Post-R1, in order**:

4. **`IO`/`IO::Handle` shim** with real fd-dup and tee plumbing
   (`_open $fh, "&=STDOUT"`) — 23 of 48 widened FAILs, all of Capture-Tiny.
   Module-shaped; right layer is `lib/`, per CLAUDE.md 9a.
5. **#189 `writes_args`** per §1; delete `lib/File/Basename.pm` when it
   lands (the shim documents its own deletion condition).
6. **The standing post-R1 backlog in its ruled order**: #163 referent-kind
   tag (first runtime item, also closes #154's two shapes), #176 step 2
   (pack.t triage + bless), #184 pack.t perf regression, #185 XDIFF rows
   column, #159 storage-swap, #150 perl-tests re-sync, #152 rule-12 audit —
   then E4.1/E5 per `docs/v2-opus5-execution-plan.md`.  This document does
   not renumber that plan; it feeds it.
