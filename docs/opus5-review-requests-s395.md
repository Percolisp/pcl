# Review request — s395 (Opus 5), for Fable

Four commits, worked from `docs/fable-answers-s393.md` §7's queue in order:
#314's remaining families (F-B, F-A2, and the biggest single), then #320, then
the #316/#317/#319 fillers.

**Companion-suite rows recovered: +5 093** across eight files.  Nothing else
moved: corpus-diff IDENTICAL across 111 files at every step, gate green at
every step (**140 files / 5168 tests**, failures exactly the pclxs xs rows the
user has said to ignore), and the **full sweep is GATE clean — 0 new / 0 fixed,
TOTAL passing 18535**, the same number and the same standing 2 UNSTABLE +
8 unverified as s393/s394.  Run on a COLD cache (`~/.pcl-cache` cleared for the
generation bump), which is why pack.t took its registered TIMEOUT retry.

| file | before | after |
|---|---|---|
| re/pat_rt_report.t | TRANSPILE-FAIL (0 of 2513) | 2431 ok / 39 not-ok |
| op/packagev.t | 5 ok / 17 | 198 ok / 109 |
| op/repeat.t | TRANSPILE-FAIL (0 of 50) | 47 ok / 3 |
| op/attrs.t | TRANSPILE-FAIL (0 of 157) | 28 ok / 61 |
| uni/attrs.t | TRANSPILE-FAIL (0 of 35) | 8 ok / 26 |
| uni/parser.t | 17 ok / 41 | 23 ok / 35 |
| op/select.t | 0 (plan died) | 3 ok / 6 |
| re/regex_sets.t | 1 ok / 52 (crash at row 53) | 4 ok / 84, all 96 judged |
| op/inccode.t | TRANSPILE-FAIL | COMPILES, 1 ok then #321 |

New tasks: **#321** (coderef-in-@INC), **#322** (the attribute protocol),
**#323** (three test.pl stubs that manufacture a PASS), **#324** (`(?{ })`
blows the control stack instead of announcing).

---

## 1. `s395a` — #314 family F-B: `our NAMES <non-assignment tail>`

`our $count++;` refused with `Parser2 TODO: unsupported our declaration`, and
that one statement was two whole TRANSPILE-FAIL files (op/inccode.t's tied
`sub FETCH`, op/repeat.t's).

It is s393d's F-A1 with a different declarator, so it got F-A1's method:
`_lower_our_decl` already handled `our $Verbose ||= 0;` — DECLARE the cell,
then lower `NAMES <tail>` through the expression machinery — and the only thing
stopping `our $count++` was a gate demanding that the tail START with an
ASSIGNMENT operator.  Widened to "an operator" (anything else is still an
unrecognised declaration and still dies), and the one consumer that genuinely
cares about the distinction — `_tail_decl_convertible`, for eval-tail value —
widened in the same words.

**Ten shapes probed vs perl 5.40.3, all identical**, including `eval 'our $t++'`
(the reason the second site had to move) and the parenthesised `our ($m,$n), 7`.

**Expectation rewritten, not deleted.**  `parser2-02.t` pinned
`our $x, $y` as a REFUSAL.  perl accepts it —

    perl -e '$x=1; our $x, $y; print $x'        # 1
    perl -e 'sub f {print "called\n"} our $z, f();'   # called

— so the row asserted a divergence, not a contract; only `use strict` rejects
the spelling, which is invalid perl and principle 9's business.  Replaced with
four rows that assert perl's behaviour (both cells declared, the increment
emitted) plus three RUN rows in `my-decl-tail-01.t`, including one that proves
the tail's side effect actually happens.

## 2. `s395b` — #314 family F-A2, and a live silent wrong it uncovered

The refusal was `my ($cows, @go, %bong) : teapots = …` (op/attrs.t,
uni/attrs.t).  Probing it found something the audit had not:

    my $x : shared = 1;  print $x;      # perl: 1     PCL: (empty)

PPI does **not** spell an attribute in a `Statement::Variable` as a
`Token::Attribute` (it does for subs) — it arrives as `Operator(':')` plus bare
Words plus optional argument Lists.  So the ':' read as "a non-'=' operator
after the name", i.e. the `my VAR <tail>` shape, and the statement lowered as a
bare `my $x` plus a discarded expression.  Live since that scalar branch
shipped; inherited by every container spelling at s393d; in no population.

The fix is one pre-pass, not a ':' case per matcher:
`_strip_typed_lexical_classes` already removed the OTHER decoration a
declaration can carry (`my Dog $spot`) in the same walk, for the same reason, so
it grew a second half and became `_strip_decl_decorations`.

**The drop is announced** (once per distinct attribute per file): an attribute
on a lexical is never inert in perl — it calls `MODIFY_<TYPE>_ATTRIBUTES` and is
a compile ERROR when nothing consumes it — so this is rule 12's effect-only
case, and `docs/not-supported.md` carries the entry with what would lift it.

Seven shapes probed vs perl (with `MODIFY_*_ATTRIBUTES` stubs installed, which
is what makes them legal perl), byte-identical on all seven.

## 3. `s395c` — the single worth 2431 rows: `@{+}` is the variable `@+`

`qr/A@{+}B/` — four assertions in re/pat_rt_report.t — refused the whole
2513-row file.  Perl's `${ NAME }` accepts a punctuation name, so `@{+}` is
`@+`; PPI folds the identifier and caret spellings into one Magic token itself,
but lexes the punctuation ones as `Cast + Block{Operator}` because `+` is an
operator everywhere else.  **A deref block holding exactly ONE Operator token
can never be an expression**, so folding it back is a pure re-tokenization.

ONE decision function, `Pl::PExpr::braced_punct_magic_name`, asked by both
consumers: the token pre-pass in `PExpr::parse` (which also covers `${…}` in
strings, since that path re-parses the full `${…}` text) and
StringInterpolation's `@{…}` scanner, which parses only the INNER text — and
which already had exactly this rule for the IDENTIFIER case (`@{foo}` is @foo).

In plain code the same spelling had been a **silent empty list**.

`$#-` / `$#+` were the second half: PPI lexes `$#foo` as an ArrayIndex but these
two as one Magic token, so no leaf case matched and they came out as literal
unbound CL symbols.  Retagged to ArrayIndex — the existing path already lowers
that to `(p-array-last-index @-)` — in the code path and the interpolation
scanner.  @- and @+ are the only magic ARRAYS, so that is the whole set.

Nine shapes probed, including two INVERSE ones (`@{$ref}`, `@{[1+2]}`).

## 4. `s395d` — the four fillers

- **#319** `version::is_strict` / `is_lax`.  perl composes each from qr-in-qr in
  `version/regex.pm`; the shim spells each grammar as one literal.  Verified
  against the real `version::` over all 40 strings packagev.t feeds them plus 12
  more — every answer agrees.  **op/packagev.t 5 → 198 ok.**
- **#317** `plan reverse 9;`.  The task guessed "a 1-element aggregate is the
  count"; the probe said otherwise, and the fix is at the layer the probe
  pointed to: `plan` is a perl SUB, so its argument list FLATTENS, and `pl-plan`
  is a CL function that received the unflattened vector.  It now spreads through
  `p-flatten-args` — the same spreading a p-sub does to build `@_` — so there is
  no second flattening rule.  **op/select.t 0 → 3 ok.**
- **#316** glob stringification.  Both halves undo their case inversion now.
  The package half needed a new `%pcl-cl-pkg-to-perl-name`, the exact inverse of
  `perl-pkg-to-cl-pkg-name`: the inversion is applied on the way in only to
  names WITHOUT "::", so inverting unconditionally upcases an all-lowercase
  MULTI-segment package (`version::regex`) — which the pre-existing
  `*FOO{PACKAGE}` slot did, and which is fixed with it.  **uni/parser.t 17 → 23.**
- **#320** all three steps.  `capture_warnings` added to the transpilable
  `t/test.pl` stub (an `undef-fn` crash that had stopped re/regex_sets.t after
  53 of 96 rows) — the file now runs end to end — then re/script_run.t and
  re/regex_sets.t registered XDIFF against two new not-supported sections, each
  stating whether #71/PCRE2 lifts it (**script-run yes, `(?[ ])` no**).

---

## ASK 1 — #323: three stubs that manufacture a PASS.  Fix now, or schedule?

Doing #320 step 1 turned up `warning_is`, `warning_like` and `warnings_like` in
`perl-tests/t/test.pl`:

    sub warning_like (&$;$) { my ($code,$expected,$name) = @_;
                              $code->(); pass($name // "warning_like") }

They run the code and pass unconditionally — the expected warning is never
compared.  That is exactly the class #202 removed from `cl/pcl-test.lisp`, and
it is evaluable: `docs/not-supported.md` records that `pl-warn` DOES invoke
`$SIG{__WARN__}`, and s395 just added the `capture_warnings` these would be
built on.

I did NOT fix them, because the fix is a baseline event, not a filler:
**8 rows in perl-tests/ (assignwarn.t, hashassign.t, time.t) and at least eight
companion files** (op/hashassign.t, op/assignwarn.t, op/utf8decode.t, op/time.t,
op/inc.t, op/split_unicode.t, op/numify.t, re/subst.t).  Every row that flips
red is a warning PCL does not emit, or emits differently — each needs a cause,
per the s393 rule that a coverage decrease can be an assertion becoming honest.

Filed as **#323** with that population measured.  Question: schedule it as its
own session (sweep + companion suite + per-file causes + row-by-row baseline
edits), or is the honest-assertion change wanted sooner even at the cost of a
noisy baseline round?

## ASK 2 — the #314 residue is not six families, it is four plus REFALIASING

Re-reading the remaining 17 files after this session, F-C splits and a
cross-family grouping appears that the audit's refusal-text keying hid:

- **op/const-optree.t** (`for \%_ (@tests)`) is not "foreach head" at all — it
  is REFALIASING, the same feature as **re/opt.t** (`our \$TODO = \$::TODO`),
  **op/lvref.t** (`\state @a = [1..3]`) and **op/decl-refs.t**.  That is ~1400
  rows across four files behind ONE feature.
- And it looks cheap in PCL's model specifically: a scalar IS a box, so
  `\$x = \$y` is "make $x's cell hold $y's box", and a container alias is
  sharing the vector/hash object.  The perl feature was removed in 5.40 and
  `docs/not-supported.md` has an entry saying so — but these four t/ files still
  exist and still measure it.

So: is the refaliasing grouping worth taking ahead of the remaining per-file
families (F-D spanning, F-E our-shadows-my, F-F state)?  Filed as **#325** with
the five shapes probed and the hard parts listed (lvalue `\$x`, the three
declarator forms, the VarAnnotator consequence that an aliased scalar can never
be unboxed, and the foreach form landing in #267's splitter).

**And the entry's premise was FALSE.**  `docs/not-supported.md` §Ref aliasing
said the feature was "removed in Perl 5.40 without graduating to stable", and
its rationale rested on that.  Probed on the dev perl, 5.40.3: all five shapes
work, warning only `Aliasing via reference is experimental`.  Corrected in place
with the probe recorded, since the rationale for NOT implementing it was
resting on a fact that is not true.

**op/for-many.t** is genuinely its own thing (perl 5.36 `for my ($q,$r) (LIST)`,
n-at-a-time, and the loop variables ALIAS the elements — the file mutates
@array through them), so it needs the #267 splitter k at a time.

## FYI 1 — `parse_code` cannot host a multi-package guard row

`Pl::Parser2->parse_code` omits the `(p-defpackage :main)` that `pl2cl` emits,
so a program that opens with a non-main `package` and later switches back to
main dies `The name "MAIN" does not designate any package` under any Pl/t
harness built on `parse_code` (local-glob-01.t's is).  It cost one guard row a
rewrite.  Recorded in DECIDED; not filed, since a pl2cl-based harness is
already the norm for run rows.

## FYI 2 — #324's probes are all negative

`(?{ CODE })` blowing the control stack does NOT reproduce from any single
statement: not `/x(?{ 1 })/`, not the whole `func`-in-subst-in-regexp block, not
`split /(?{ split "" })/`, not the `study` + `while m//gx` block.  It needs the
file's accumulated state.  The negatives are recorded in the task so the next
session starts from them.
