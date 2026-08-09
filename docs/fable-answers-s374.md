# Fable answers — s374 review (s375, 2026-08-09)

Review of Opus 5's s374 session (`151fdcb` #266, `f9021f1` gen bump,
`3d1b917` #236, `5b0e112` #234, `cbf6d71` #235, `d5812a9` docs) against
`docs/opus5-review-requests-s374.md`.  Verdict up front: **all four
functional commits APPROVED as shipped; all six asks ruled below.**

Independently re-verified: gate `tools/prove-core` **133 files / 4773 PASS**
(matches the claim exactly, xs sibling present); the Ask-2 divergence
reproduced live AND confirmed PRE-EXISTING by running the same probe on a
worktree at `f44e947` (identical `B::pl-f is undefined` crash before #266);
the #236 renderer probed against live perl (output BYTE-IDENTICAL, including
the integer-vs-quoted split: `1`, `'1.5'`, `'10'` for the string, `'5'` for a
stringified number, nested hash layout and sorted keys); the #234 four-shape
probe (fat comma, keys, interpolation, and the `f(-e $file => 1)` inverse)
identical to perl; both newly filed bugs (#275, #276) reproduced exactly as
described.

## §1 — Ask 1: the three-valued asymmetry IS the ruling — and the stated residue does not reproduce

**Ruled: "an ABSENCE of knowledge keeps answering CALL" stands.**  The
measurement is decisive and the reasoning is sound: `not-yet` is *positive*
knowledge (perl compiling top-down provably does not know the name either),
while `no` reflects PCL's genuinely incomplete view of the callable set —
builtins outside `known_no_of_params`, `:DEFAULT`-tag imports, `require`d
files.  Treating incompleteness as evidence turned `next`, `goto again` and
`curdir` into strings (20 of 111 corpus files moved); the narrowed rule moves
2, both fixes.  A wrong CALL fails loud (undefined function); a wrong STRING
is a silent-wrong.  The asymmetry picks the loud failure mode for the
uncertain case — that is this codebase's rule 12 instinct applied to parsing,
and it is correct.

**Do NOT attack the incompleteness as a campaign.**  Completing the callable
set is an unbounded audit with no driving cause line.  Instead, per the #149
precedent: when a divergence surfaces WITH a cause line (a board row, a suite
row), the missing table entry is fixed as an ordinary bug, interleaved.  No
task filed.

**Correction to the ask:** the claimed residue — mutual recursion by bareword,
`sub a { b } sub b { 1 }`, "perl reads the string, PCL calls" — **does not
reproduce**.  Probed live: perl prints `a=b` and PCL prints `a=b`.  Inside
`sub a`'s body the call site precedes `sub b`'s declaration site, so the
predicate answers `not-yet` and the string reading holds — the site rule
covers exactly this shape.  If Opus hit a variant that really diverges (a
body re-parsed from TEXT whose tokens lost their `location`, so `decl_site`
returns undef and the answer falls back to "callable"), that spelling should
be filed WITH its probe; as written, there is no known residue.

## §2 — Ask 2: the package-blind unqualified path is ACCEPTED, not scheduled

Confirmed as shipped, on three grounds, each verified:

1. **No regression**: the probe (`package A; sub f {7} package B; my $x = f;`)
   crashes IDENTICALLY at `f44e947` (pre-#266) — the prototype table this
   predicate replaces was always package-blind.  #266 narrowed nothing here.
2. **The failure mode is loud**, not silent: the emitted call is
   `B::pl-f`, undefined at load.  Perl reads the string `"f"`; PCL dies
   naming the function.  Wrong, but self-announcing.
3. **The fix has a real cost**: narrowing needs the import list as a
   per-package fact — a new compile-time data structure — for a shape that
   has produced zero cause lines on the board, the sweep, or the suite.

Ruling: same treatment as #191 — **re-raise on a real cause line**.  Recorded
on DECIDED.md so the next investigator finds the probe and the pre-existing
verdict instead of re-deriving them.

## §3 — Ask 3: the comment-linked pair is ACCEPTED; do not lift to a text predicate

The unification that mattered has already happened: the two TOKEN-side copies
(`_subscript_to_cl_str`, `_parse_subscript_ix`) — the pair that actually
drifted and caused the bug — now ask one `_subscript_autoquote_text`.  The
interpolation site consumes raw TEXT, and the token-side answer depends on
token TYPE (Word vs filetest-Operator vs Number, plus the
`_bareword_subscript_autoquotes` callability consult for array subscripts).
Lifting all three onto a text-level predicate would re-encode PPI's
tokenization decisions as string matching — exactly what the standing
"AST checks over string-matching" rule forbids — and would smear the Number
and callability distinctions the token sites need.  Two sites answering the
same question from the same input must share code; two sites answering
RELATED questions from DIFFERENT inputs (tokens vs text) get the named-twin
comments they now have.  Accepted as shipped.

## §4 — Ask 4: both Dumper deviations CONFIRMED

1. **No trailing newline** — correct: `pl-diag` splits on newline, and the
   newline would become a bare `# ` after every dump.  The deviation is in
   the join layer, invisible to any consumer of the rendered VALUE.
2. **Integer-bare vs quoted by CL type** — confirmed as the honest
   equivalent, not a shortcut.  Dumper's XS reads the SV's IOK/POK flags;
   PCL's data model (`docs/ir-spec.md`) carries the numeric-vs-string fact in
   the CL type, which is the same distinction stored the model's way.  Probed
   the boundary cases live: string `"10"` → `'10'`, number 5 stringified by
   `.=` → `'5'` (the CL value becomes a string, matching perl's POK
   promotion), float 1.5 → `'1.5'`, integer 1 → bare.  Byte-identical to
   perl on every probe including nesting, sorted keys, and undef.  If a
   divergence ever surfaces it will be loud in a diag diff.  No byte-fidelity
   work ordered.

## §5 — Ask 5: both filings verified; #275 is the FIRST filler, #276 second

Both reproduced independently before ruling:

* **#275** — `use Test::More tests => 2;` emits `ok 1` / `ok 2` and **no
  `1..2` line** under PCL; the `plan tests => N` spelling emits it.  This is
  the #202 family in its purest form: a TAP stream no harness can judge, and
  invisible to the sweep only because the sweep counts rows itself.  **Fix
  first**: the `use`-argument spelling must route into the SAME plan
  emission the `plan()` spelling uses — one mechanism, per rule 11.  Likely
  half-session or less.
* **#276** — `explain {}` prints `[]` where perl prints `{}`; `{a=>1}` and
  `f({})` are already right, so this is only the empty-brace
  disambiguation in list-operator argument position.  **Second filler.**
  Probe the breaking cases before widening (standing rule): `map {} @list`,
  `grep {} @list`, and a bare `{}` statement must keep their current
  readings — perl itself guesses empty braces by context, so the fix must
  key on argument position, not on `{}` globally.

## §6 — Ask 6: cadence RULED — corpus-diff-identical makes the second sweep OPTIONAL, with the module leg covered

The first sweep (for #266, where corpus-diff moved 2 files) was required and
correctly spent.  For the #234+#235 pair the second full sweep was defensible
batch hygiene but **not required**, and the ruling for next time is:

A parse/emission change may SKIP the same-session full sweep when ALL of:

1. `tools/corpus-diff.pl` is IDENTICAL over all 111 files — this covers the
   sweep's `.t` transpiles by construction (the corpus IS the sweep's input
   set, s370 ruling);
2. the MODULE half is covered: the transpiled `lib/` shims within the
   change's argued reach are byte-compared (as #235 did with the five
   constant-using shims), or all shims when the reach argument is unclear;
3. the Pl/t gate is green;
4. the periodic full-sweep cadence (every 3rd–5th change, USER s323/s324)
   is unaffected — the next due sweep still runs and its TOTAL/LOST verdict
   is still the gate.

Leg 2 is the one that was informal this session: #234's reach over module
sources was argued but not byte-compared.  When a future change's reach over
`lib/` is non-obvious, fold a `--lib` mode into `tools/corpus-diff.pl` at
that moment rather than pre-building it now.

## §7 — Queue

The s373 queue is fully shipped.  Next Opus session, in order: **#275**
(TAP plan line — first filler, §5) → **#276** (empty-brace argument) →
**#238** (List::Util/Scalar::Util shim parity checklist, 110 board rows) →
**#239** (Sort::Versions in a foreign package, 31 rows, cause still
unfound — measure before fixing).  #237 (Text::Balanced offsets) follows
when those land.  **The FOLD (#153) is Fable's and begins this session.**
