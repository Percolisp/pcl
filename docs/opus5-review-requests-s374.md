# Opus 5 review requests — s374 (2026-08-09)

The whole s373 queue shipped: **#266, #236, #234, #235**, plus a cache-generation
bump.  Four functional commits.  Gate ends at **133 files / 4773 PASS**.

| commit | task |
|---|---|
| `151fdcb` | #266 — a bare NAME is a call only where it is callable |
| `f9021f1` | cache generation v2-124 + restamp of the two transpiled artifacts |
| `3d1b917` | #236 — `explain()` dumps a ref instead of stringifying it |
| `5b0e112` | #234 — `-BAREWORD` autoquotes before `=>` and in a hash subscript |
| `cbf6d71` | #235 — `use` arguments are compiled, not wrapped as raw text (gen v2-125) |

## Verification actually taken

* **#266**: 12-case grid (qualified/plain × declared-above/below/never ×
  mid-list/tail) + 11 breaking-case probes, all matching live perl;
  `corpus-diff` 2 of 111 files, **both fixes**, each explained; FULL SWEEP with
  gate — `0 new / 0 fixed`, TOTAL 18499 passing, no LOST, 6 UNSTABLE +
  4 unverified all in files already PARTIAL in the baseline.
* **#236**: three guard rows compare against the LIVE Data::Dumper text
  (Indent 1 / Terse 1 / Sortkeys 1), including a cycle and a shared ref.
* **#234**: `corpus-diff` **identical over all 111 files** — the sweep's `.t`
  half provably cannot move; four probe files (incl. the two inverse shapes)
  match perl.
* **#235**: `corpus-diff` identical; the five constant-using `lib/` shims
  (Socket, POSIX, Errno, Fcntl, Math::BigInt::Calc) byte-identical to HEAD
  below the embedded-root lines.
* A second full sweep over the #234+#235 pair was run at session end; its
  verdict is appended to the session log.

## Ask 1 — #266's THREE-valued predicate: is the asymmetry the right shape?

`_bareword_callable_here` returns `yes` / `not-yet` / `no`, and the two
negatives are treated differently:

* `not-yet` (this FILE declares the name BELOW the call site) → the string,
  wherever it sits.  That is positive knowledge: perl compiling top-down does
  not know it either.
* `no` (nothing this compiler can see) → still a CALL unless the word sits in
  operator context, because PCL's compile-time name knowledge is **incomplete**.

The asymmetry was measured, not assumed.  Treating `no` as a string too made
the whole 12-case grid match perl, but the corpus said what it cost: `next`,
`goto again` and File::Spec's `curdir` all became STRINGS (20 of 111 files
moved; loopctl.t's `push @a, last` and array.t's `goto again` are the
worked examples).  The narrowed rule moves 2 files, both fixes.

**Ask:** is "an ABSENCE of knowledge keeps answering CALL" the ruling, or
should the incompleteness be attacked instead — i.e. complete the callable set
(the builtins outside `known_no_of_params`, the `:DEFAULT`-tag imports) so `no`
becomes trustworthy and the last divergences close?  The known residue if we
stay: mutual recursion by bareword (`sub a { b } sub b { 1 }` — perl reads the
string "b", PCL calls), unchanged from HEAD and now probe-documented.

## Ask 2 — the unqualified path stays PACKAGE-BLIND

The new predicate resolves a QUALIFIED name against `declared_subs` by
(name, package).  An UNQUALIFIED name stays package-blind, exactly as the
prototype table it replaces there always was — so a sub declared in package A
still answers "callable" to a bare call in package B.  Narrowing that needs the
import list as a per-package fact (the same limitation `_is_known_callable`
already documents).  Confirm the narrowing is accepted, or schedule the
per-package fact.

## Ask 3 — #234 now has THREE autoquote sites; one cannot share the helper

`=>`-autoquote and subscript-autoquote are two different decisions, and the
subscript one had drifted into **two copies** (`_subscript_to_cl_str` and
`_parse_subscript_ix`) — which is exactly why a fix in one would have left the
other broken.  They now share `_subscript_autoquote_text`.  The third site,
string interpolation, works on raw TEXT rather than PPI tokens and so cannot
call a token-side helper; it got the same rule as a widened regex plus a
comment naming its twin.  Is the comment-linked pair acceptable, or should the
autoquote answer be lifted to a text-level predicate all three call?

## Ask 4 — two deliberate Dumper deviations in #236

1. No trailing newline (Dumper's `Dump` ends with one).  `pl-diag` splits on
   newline, so keeping it would print a bare `# ` line after every dump.
2. A number prints bare when the CL value is an INTEGER and quoted otherwise —
   `'1.5'`, and `'10'` for the string "10".  Dumper's XS reads the SV's flags;
   PCL's CL integer/float/string types carry the same distinction, so this
   matches perl on every probe, but it is a type-based proxy for a flag-based
   rule.

Both are commented at the code.  Confirm, or ask for byte-fidelity.

## Ask 5 — two NEW bugs the probes found (filed, not fixed)

1. **(filed #275)** **`use Test::More tests => N` emits NO `1..N` plan line** (the
   `plan tests => N` spelling does).  A TAP file with no plan is one a harness
   cannot judge — the #202 family ("a claim that cannot be evaluated").  The
   sweep counts rows itself, which is why it has never shown.  Fix now or
   queue?
2. **(filed #276)** **An empty `{}` as a list-operator argument parses as an empty ARRAY**:
   `explain {}` dumps `[]` where perl reads a hashref and prints `{}`.
   `explain {a=>1}` and `f({})` are both already right, so this is only the
   empty-brace disambiguation.

## Ask 6 — cadence

Two full sweeps this session (one for #266, one for the #234/#235 pair) plus
three `corpus-diff` runs and three gates.  Given corpus-diff proved emission
identical for #234 and #235, was the second sweep the right spend, or is
"corpus-diff identical ⇒ no sweep" the ruling for a parse-rule change whose
module transpiles were also checked byte-identical?
