# Review requests for Fable — from Opus 5, s316v (2026-08-01)

*Written at gen v2-90, gate 123/4439, sweep 0-new vs the 702 baseline,
census 111/111 v2-native, tree clean at `e6c858b`.  Four items, each with
the evidence that produced it.  Items 1 and 2 are asks for a design
decision; items 3 and 4 are asks for a priority call.  Nothing here is
blocking R1 — the two shipped commits are verified and the tree is
shippable.*

---

## 1. Bareword class names — I failed three times; the shape of the failure
##    points at `pexpr-term-parsing-review.md` Option B (task #142)

**Symptom.** `t/op/avhv.t:114` is `tie %fake, Tie::StdHash;` — a BAREWORD
class name.  PCL compiles it to a call `(Tie::pl-StdHash)` and dies
(suite row `undef-fn:Tie::pl-StdHash`).  The quoted form works, and
Tie::StdHash itself loads fine from the system `@INC`.

**It blocks two suite files, and both are otherwise close.**  `op/warn.t`
(`P:32/1`, row `undef-fn:Tie::pl-StdScalar`) uses BOTH forms: line 156
`tie $@, "Tie::StdScalar";` — quoted, works — and line 182
`tie $t, Tie::StdScalar;` — bareword, dies.  So the two files differ only
by quoting, which is a tidy confirmation that the class-name argument
position is the whole problem and nothing else about `tie` is missing.

**Three attempts, all reverted; main never carried any of them.**

| # | change | killed by |
|---|---|---|
| 1 | qualified bareword + `!has_prototype` → string | `package Foo; sub init` + `my $a = Foo::init` → PCL gave the STRING, perl calls.  `has_prototype` keys on the LITERAL declared name, so a `Foo::init` query answers "unknown". |
| 2 | + `strict_subs` exclusion, + `declared_subs` `{name,package}` as the qualified oracle | **All probes green.**  `corpus-diff` caught method.t: the invocant `Count::DATA->getline` lost its `(p-resolve-invocant …)` wrapper — the class got hard-wired and perl's runtime class-vs-filehandle resolution vanished.  Those are method.t's *"file handles take priority"* rows. |
| 3 | + guard "next token is `->`" | **No effect at all.**  method.t still stringified, so the invocant does not reach that decision with `->` at `$e->[$i+1]`. |

**Two facts I did not know going in, both of which turned out to be
already tracked** (the user pointed at both):

- `strict_subs` is decisive, not incidental.  Under `use strict` an
  undeclared bareword here is a *compile error* (`Bareword "Tie::StdHash"
  not allowed while "strict subs" in use`), so by principle 9 anything
  that compiles in strict code is a declared call and the rule must never
  fire there.  Attempt 1's core defect was OR-ing the clause OUTSIDE the
  existing `$strict_subs` branch.
- `declared_subs` (`Environment`, `{name, package}`) is the
  package-qualified oracle `has_prototype` cannot be — and `Parser2:734`
  already documents it as *"the fallback expression machinery decides
  bareword-vs-string from declared_subs"*.  I used the wrong accessor.

**What I want reviewed.**  Attempt 3's failure is the interesting one: the
guard was a no-op, which means a method invocant arrives at `PExpr.pm:3695`
by a route I could not trace.  That decision sits under
`if ($end_pars < $i + 1)` — i.e. inside exactly the operand-boundary
machinery your `pexpr-term-parsing-review.md` calls "the maze", and whose
Option B you already specced as the target architecture.  My reading is
that **this bug is not independently fixable in that region** — every guard
is another hand-derived boundary condition of the kind Option B exists to
delete.

**My recommendation, which needs no PExpr change at all:** `bless` already
has a class-name-bareword branch (`ExprToCL.pm` ~2007, `$is_bareword`).
`tie`/`tied`/`untie` take a class name in exactly that argument position.
Routing them through the same mechanism is CLAUDE.md 11 reuse, and as a
per-builtin *argument-position* rule it cannot reach `Foo::init` or an
invocant by construction.  **Is that the layer you want, or do you want
this held until Option B lands?**

---

## 2. Scalar-context flip-flop: constant operands compare against `$.` (task #141)

perlop: *"if either operand of scalar `..` is a constant expression, that
operand is considered true if it is `==` the current input line number"*.
PCL applies this only when BOTH operands are numeric (`p-flipflop-num`);
a mixed case takes the boolean `p-flipflop` and diverges:

```perl
my $a = ("x" .. 3);   # perl: 1     PCL: 1E0
```

PCL treats both operands as booleans, so the flip-flop turns on and off in
one step and appends the final-value `E0` marker.  (The marker itself is
correct perl — the classification is what is wrong.)

Found downstream of the #140 probes: `for ($x |= "a" .. 3)` gives 0 vs
perl's 1, and the `|.=` variant leaks a stringified ARRAY ref.

**Ask:** where should the per-operand constant classifier live — the
`Config.pm` param-spec family, PExpr's operand analysis, or a codegen-side
decision in the `p-flipflop*` selection?  I did not attempt it: it is a
classifier change with real regression risk and the R1 window is the wrong
place to learn it by iteration.

---

## 3. Priority call: the "one place" theme is paying out, per instance

Two consolidations this session, and **each one surfaced live bugs that
nobody was looking for**:

- **#138** (below-assignment precedence, `6c95f99`): 3 hand-rolled copies +
  5 sites missing it entirely.  Fixed a **silently deleted statement**
  (`my ($a) = @_, g();` — the caller deletes the matched statement and the
  match was `>= 4` instead of exact arity), a loop that ran **zero
  iterations** (`for (my $i=0, $j=9; …)`), and four value folds.
- **#140** (compound-assignment operator set, `e6c858b`): 4 copies, 2
  missing `&.= |.= ^.=`.  Both gaps live.  **Corpus emission came back
  identical across all 111 files** — the signature of a correct
  de-duplication.

That is 2 for 2, with the worst failure mode in the codebase (silent
statement deletion) found by the first one.  The review's §2 already names
the endpoint — a read-only PExpr classifier, blocked on PExpr's destructive
parse — and §8 puts shared predicates at E5.5, last.

**Ask:** does this evidence move E5.5 (or at least the non-mutating parse
mode it depends on) earlier in the E5 order?  I have no view on the
sequencing cost; I only have the hit rate.

---

## 4. #138 residual: the ambiguous comma in a `state` init

`lowprec_split_safe` declines to split when a bare Word could be a
parenless list operator eating the comma — that is what keeps
`state $c = \substr $s, $i, 1` (state.t) whole and v2-native.  The cost is
that `state $s = f 1, $t = 2` still folds.

Resolving it needs PExpr's arity knowledge at the split point, i.e. the
same read-only classifier as item 3.  **Ask: leave as recorded, or is
there a cheaper oracle I am not seeing?**  Every other #138 consumer
avoids the question entirely by handing the whole `$x = …` run to PExpr;
only the state once-guard must interpose code between head and tail, which
is why it is the one site that has to split at all.

---

---

## 5. Reading `docs/perl-suite-run.tsv` — I got this wrong twice; here it is
##    from the source

The row (`run-perl-suite.pl:352`) is

    rel  P_ok  P_notok  C_ok  C_notok  status  sig

and the authoritative meaning is the file's own header (~line 69), confirmed
by the assignments at 268 and 296:

    # Output columns: P:perl_ok/notok  C:pcl_ok/notok  STATUS  [crash-signature]
    # NOTAP (perl itself produced no TAP — not comparable, doesn't fail the
    #   run; PCL result shown).

**P is PERL. C is PCL** (C for CL).  Two consequences that are easy to get
backwards, and I did — first treating `C_ok` as PCL's *failures*, then
treating `P` as PCL:

- **`NOTAP` is a statement about PERL, not PCL.**  It means perl produced no
  TAP under the runner, so the row cannot be compared — the PCL numbers are
  still shown and may be perfectly healthy.  `op/bless.t` reports
  `P:0/0 C:111/5`: perl gave nothing, PCL passed 111.  Several files I first
  read as "PCL produces no TAP" are the opposite.
- **`op/fork.t` is NOT clean** (I said it was): the row is `P:28/0 C:1/0` —
  perl 28, **PCL 1**.

The correct near-green filter is `status eq 'DIFF'` (both sides produced
TAP) with **`C_notok`** small and `C_ok` large:

| file | perl | PCL | blocker |
|---|---|---|---|
| io/defout.t | 22/0 | 21/**1** | — |
| io/print.t | 24/0 | 23/**1** | — |
| op/localref.t | 64/0 | 63/**1** | — |
| uni/bless.t | 84/0 | 83/**1** | — |
| op/quotemeta.t | 60/0 | 58/2 | — |
| op/not.t | 24/0 | 22/2 | — |
| op/64bitint.t | 425/0 | 423/2 | — |
| op/lex_assign.t | 353/0 | 351/2 | s316t: t3 DESTROY-on-reassign, t283 schop |
| op/rand.t | 263/0 | 260/3 | — |
| op/dor.t | 34/0 | 31/3 | — |
| op/warn.t | 32/1 | 21/5 | **#142** bareword `tie` |
| op/chop.t | 148/0 | 96/4 | — |

The four one-failure rows at the top are the cheapest genuine W1 targets in
the snapshot.

**A second trap in the same tooling:** `.suitelog/<file>.fails.tsv` joins the
two TAP streams on test number, and the *description* column comes from
**perl's** line.  For io/print.t it read `printf with %n (got a5c)` — that
`a5c` is PERL's value interpolated into perl's own test name, not PCL's.
PCL's value was `abc`.  Read the description as perl's text, always.

### What the four resolved to (triaged s316v)

- **io/print.t — one real bug, now task #143.**  `printf "ok 22%n…", substr $n,1,1`:
  perl writes the emitted-character count (5) through the lvalue `substr`, so
  `$n` becomes `a5c`.  PCL has no `%n` at all — it emits the literal `%n`,
  warns "Redundant argument in printf", and leaves `$n` as `abc`.  Test 22
  passes only by luck (the runner's `/^ok /mg` matches the `ok 22` prefix of
  the malformed line).  Ruled out first: the `x` operator with a false/empty
  count is correct in all six forms probed.  Implementing needs an **lvalue
  argument convention for one conversion**, routed through the existing
  box-magic hook because the argument is an lvalue `substr` — so it is a
  decision, not a drive-by; #143 states the alternative (bless as
  not-supported, with the note that `%n` is the classic format-string-attack
  primitive).
- **io/defout.t, op/localref.t, uni/bless.t — not comparable right now.**
  All three come back `NOTAP`, i.e. **perl** produced no TAP under the
  runner, so the snapshot's PCL failure counts (21/1, 63/1, 83/1) cannot be
  checked against anything today.  Whether that is runner drift since the
  v2-85 snapshot or genuinely unrunnable files is unresolved and worth a
  look before anyone treats these as fix targets.

## Verification standard used for the two shipped commits

corpus-diff (every changed file explained) → `tools/prove-core` full gate →
full sweep `--jobs 8 --timeout 380` + `sweep-diff` vs `docs/fail-baseline.tsv`
→ `v2-census`.  Artifacts regenerated and confirmed header-only at each gen
bump (v2-89, v2-90).  Regression batteries in `Pl/t/transpile-test-07.t`
(now 31 tests).
