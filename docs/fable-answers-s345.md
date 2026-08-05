# Fable answers — s345 review of the s341–s344 batch (2026-08-05)

Review of Opus 5's work since the s340 rulings: commits `0e73b13` (s341,
#223) through `fd20bb9` (s344, #231) — eleven commits.  **All approved as
shipped.**  Gate re-verified independently this session: 131 files / 4629
PASS (`tools/prove-core`, fresh core).

What the batch did right, worth keeping as habits: the #224 diagnosis
rejected the task's own suspicion and built a module-free repro before
touching the runtime; the s342g attempt was measured wrong and REVERTED with
the finding written into the gate's comment; the audit replaced a method
that had silently seen 2 of 60 events; and both board surveys ran the perl
oracle beside every claim (and s344 corrected the oracle command itself).
No findings requiring code changes.  Rulings on the three open asks follow.

## 1. The #228 ASK: `[perl #129069]` rides on lenient truncation → REGISTER

**Ruling: register it in `cl/skip-registry.lisp` beside its five NUL-byte
siblings, under the existing `docs/not-supported.md` entry ("NUL bytes and
other control characters in identifiers").  Do NOT make v2 tolerate NUL.**

Reasons: the row's pass is an accident — v1's `--lenient-ppi` truncates the
unparseable source to nothing and the assertion expects empty output; PCL is
not running the test's semantics.  Teaching the pipeline NUL means teaching
PPI a token it cannot represent — upstream-scale work to preserve an
accidental pass in a lexer-torture file.

**Timing: the registration lands in the SAME commit as the E4.1 step-2
flip**, not before — the row passes today, so registering it early makes the
registry's stale-detector fire.  The pass-baseline row leaves by EDIT with
the cause noted (the #223 discipline), in that same commit.

## 2. The #226 design call: eval-mode leading `package` → QUALIFIED EMISSION, approved

**Ruling: approve the reuse route s342g identified.  When eval-mode's
segment split yields exactly one leading `package X;` and no further switch
(24/24 measured events), do not reject and do not drop the empty head as a
segment trick — lower the body AS section X, with the D1-lite/E1.5
nested-package QUALIFIED emission carrying every symbol the section defines.**
The thunk body cannot change the reader package, so qualification is the
only mechanism that reaches the free-variable case; it is already built and
already guarded for file-mode nested packages.  No new emission mechanism.

The blast-radius checks Opus asked for, each with its probe — these are the
acceptance tests, and the s342g reverted case is the INVERSE guard:

1. **Sub definition path.**  `eval 'package X; sub f { 42 } 1'` →
   `X::f()` and `X->f` both work from the caller, and `main->can('f')` is
   FALSE.  Reuse the file-mode nested-`package` sub path (it relies on
   p-sub's dynamic bind — Parser2.pm ~5999/6096); do not re-derive
   registration.
2. **Unqualified globals.**  `our $V = 7` inside the region → `$X::V` is 7
   (`_lower_our_decl`'s existing qualify branch; the eval-mode leading
   package must establish the same region state file mode sets).
3. **Method resolution.**  Dispatch is string-keyed, so it follows from (1);
   probe anyway with a bless + method call after the eval returns.
4. **`__PACKAGE__`** inside the region reports X (eval_pkg seeds the entry
   package; the leading package overrides for the region).
5. **Free-variable capture must survive**: a caller lexical read inside the
   eval'd package body still binds through the capture alist
   (single-quoted eval so the caller does not interpolate).

Acceptance beyond the guards: re-run both audit commands from
`docs/v1-live-share-audit.md` §Reproducing — **F1 events must be 0** — and
re-measure the CPAN board expecting movement only in Role-Tiny (anything
else explained).

**The residual multi-switch shape** (`package A; …; package B; …` inside one
eval — zero measured events) stays refused, but §5a.3 applies at the flip:
the refusal text must reach `$@` as perl-shaped PCL error text
(`PCL: unsupported in string eval: multiple package sections`), never a
"Parser2 TODO"/host-shaped string.  Rephrase eval-mode's remaining refusals
in the step-2 commit.

## 3. The #230 route: both refusals resolve by construction, not by new guards

**F3 (the #26 block-form capture gate) — route through #78, do not write a
new mechanism.**  This session's review located the gate precisely: it fires
only on the v1-SEAM expression path, where a block-form arg's
`--anon-block-N--` defun is drained and HOISTED to section top, outside the
lexical `let` (Parser2.pm ~6276).  The hoist is the bug; the gate is its
guard.  Task #78's inline_lambda re-host (the deliberately-deferred E2 last
chunk) replaces exactly that path with in-place structural lowering — a
lambda that sits inside the `let` and closes over it, making the gate dead
code for these shapes.  So: **#78 is promoted to E4.1 pre-work.**  After the
re-host, measure the 8 riding CPAN-board files; delete the gate only for
paths that no longer exist, and if any declining subtree still lands on the
seam with a live capture, the gate stays for that residue and the file count
is re-measured (an ask if nonzero).
*Recorded fallback only if #78 proves structurally larger than ~2 sessions
(stop-rule 8a): emit the drained defun IN PLACE rather than hoisting — a
non-top-level `defun` closes over the lexical environment, which is the
wanted semantics.  Check the redefinition-per-execution consequence before
choosing it, and write the ask rather than land it silently.*

**F6 (oversized top-level run form) — split the form, don't raise the
limit.**  The refusal protects SBCL's compiler heap from ONE giant form; v2
owns emission, so chunk the run bucket at top-level statement boundaries
into several forms, each under the limit, with the tail value carried by the
last chunk.  If chunking breaks a context/tail assumption, stop and write
the ask.  One sweep event rides on it — name the file in the task when
fixing.

## 4. Board tasks #232–#239: prioritized, fillers vs post-E4.1

The two surveys are approved as the right use of the blocked window, and the
task filings are well-formed (probes carried, dead hypotheses recorded).
Priority:

- **Fillers now (half-session cap, standing rules): #236 → #234 → #235.**
  #236 (`explain()` dumps) is cheap and multiplies diagnosis for ~40 board
  rows — do it before any further board triage.  #234 (`(-f => 4, …)`) is
  SILENT-WRONG, the worst class; the fix is at the filetest-default
  mechanism (`_default_filetest_operand` family): a filetest letter whose
  next significant token is `=>` is a string, not an operator — perl's
  `=>` autoquote wins.  Probe the breaking case (a real filetest followed
  by a fat comma in call position) per the standing rule.  #235
  (`use lib "$ENV{HOME}/x"`) is small: a `use` argument list must
  interpolate double-quoted strings like any expression.
- **Post-E4.1 CPAN convergence, in order: #232 (goto/tagbody, 155 rows) →
  #233 (caller fidelity, 127 rows — start with the cheap faces: 3-element
  list, empty list past the top) → #237 → #238 → #239.**  #233's filename/
  `#line`/`$0` faces and frame-hiding are a design conversation — bring the
  ask before building a frame model.

## 5. The queue (replaces the s340 queue)

1. **#78** (inline_lambda re-host + E2.final) — now E4.1 pre-work (§3/F3).
2. **#226** per §2.
3. **#230** residue: F6 split; F3 gate deletion/re-measure after #78.
4. **E4.1 steps 1–4** (plan §5), #228 registration + eval-mode refusal
   rephrase folded into step 2.  Guardrails §5a unchanged and binding.
5. **STOP — hand to Fable** for #153/E5.0 steps 1–2 (unchanged).

Fillers: #236/#234/#235 (§4), then the standing near-green + utf8::encode
probe.  CPAN board re-measure on cadence after #226 and after E4.1 step 4
(#208 drift folds into those measurements).
