# Review requests for Fable — from Opus 5, s316v (2026-08-01)

*Written at gen v2-90, gate 123/4439, sweep 0-new vs the 702 baseline,
census 111/111 v2-native.  Each item carries the evidence that produced it.
Nothing here blocks R1 — every shipped commit is verified and the tree is
shippable.*

**Index — what each section wants from you**

| § | topic | the ask |
|---|---|---|
| 1 | bareword class names (#142) | design: take the `bless` route now, or hold for Option B? |
| 2 | flip-flop constant operands (#141) | design: which layer owns the classifier? |
| 3 | "one place" hit rate | priority: pull E5.5 earlier? |
| 4 | #138 state-init residual | is there a cheaper oracle? |
| 5 | suite-tsv column traps | none — record, so it stops recurring |
| 6a | `printf %n` (#143) | **policy: implement, or bless?** |
| 6b | quotemeta / no per-scalar UTF8 flag (#146) | **policy: which global rule?** + doc is wrong |
| 6c | deterministic `DESTROY` | **policy: is the blanket non-support still right for R2?** |
| 6d | error-text / invalid-input rows | **policy: blanket skip category?** |
| 6e | `$`-prototype arg parse (#147) | design: fix in place, or wait for Option B? |
| 6f | XDIFF registration bar | policy: confirm the rule I have been applying |
| 6g | silent-wrong vs loud-fail | **policy: should a missing case DIE rather than return a default?** |
| 6h | perl-parity vs "reasonable" for host-visible values | **policy: how far to chase perl's exact bytes?** |
| 6i | suite corpus overlap (perl-tests/ vs t/) | policy: is the copied-file split still earning its keep? |
| 6j | pack's character-vs-byte mode model (#148) | **scope: full mid-template U0/C0, or the common case?** |
| 7 | discoverability of decided questions | **process: a `DECIDED.md` index + a triage order in CLAUDE.md?** |

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

---

## 6. Decisions I am NOT taking — for the user + Fable to settle

These came out of W1 triage.  Each is a *policy* call, not a missing
implementation detail, so I have left them and recorded the evidence.

### 6a. `printf`/`sprintf` `%n` — implement, or bless as not-supported? (#143)

Sole blocker for t/io/print.t (perl 24/0, PCL 23/1).  `%n` writes the
emitted-character count *through an lvalue argument*; the case in the wild
is an lvalue `substr`, so it needs the box-magic hook and an **lvalue-argument
convention for one conversion** — sprintf currently receives evaluated
copies.  Against implementing: `%n` is the classic format-string-attack
primitive and real code essentially never uses it.  For: it is a documented
core feature, and CLAUDE.md 4 says a non-support must be an explicit,
argued decision with a `docs/not-supported.md` entry — not an accident,
which is what it is today (no entry, no handling, emits the literal `%n`).

### 6b. `quotemeta` and the missing per-string UTF8 flag (#146)

`p-quotemeta` escapes non-ASCII "unless Unicode alphanumeric", i.e. it
applies the `unicode_strings` rule UNCONDITIONALLY; perl only does that
under the pragma or for a UTF8-flagged string, so
`quotemeta chr(0xe9)` is 2 chars in perl and 1 in PCL.  The real question is
bigger than quotemeta: **PCL has no per-scalar UTF8 flag**, so any
`\w`-sensitive builtin has to pick one rule globally.  Options: (i) make the
default ASCII-only and Unicode-aware only under `use utf8`/`unicode_strings`
(fixes the common case, risks the opposite divergence for utf8 code);
(ii) bless the whole class in the Unicode section.  **Either way
`docs/not-supported.md` needs correcting** — its Unicode section currently
claims "Without the pragma, high bytes stay Latin-1 (byte semantics),
matching Perl", and quotemeta is a counter-example.

### 6c. Deterministic `DESTROY` — is the blessed non-support still right?

`not-supported.md` §DESTROY is explicit and I have used it to classify rows
(op/localref.t is now XDIFF on it).  Recording it here only because the
probe was starker than the doc implies: **PCL fires `DESTROY` in NONE of the
four common shapes** — lexical scope exit, `undef $x`, reassignment, sub
return — where perl fires in all four.  It also blocks op/bless.t t117/t118
and, per `project_cpan_pureperl_findings`, Try::Tiny.  If CPAN reach is the
R2 goal, this may deserve re-opening as a scoped feature (refcount on
blessed refs only?) rather than staying a blanket non-support.  Not a
suggestion to change it now — a flag that its cost is larger than the
section suggests.

### 6d. Error-message text and invalid-input detection

op/dor.t t26/t28 want `like($@, qr/^Search pattern not terminated/)` for
*invalid* Perl.  That is doubly covered — §"Error message text and format"
plus CLAUDE.md 9 ("PCL is a transpiler for functioning Perl code, not a
validator") — and the user has already ruled exact fatal-error text out of
scope.  Flagging only because such rows are scattered across many files and
will keep surfacing in triage: **is a blanket skip-registry category
warranted** so they stop consuming triage time one at a time?

### 6e. `$`-prototype argument parse — fix in place, or wait for Option B? (#147)

`like([] // 0, qr/^ARRAY/, '…')` (t/op/dor.t t4) passes **0** instead of the
array ref.  Isolated with a hand-written prototype:

| call | perl | PCL |
|---|---|---|
| `proto3([] // 0, …)` — `sub proto3 ($$@)` | ARRAY | **0** |
| `plain3([] // 0, …)` — no prototype | ARRAY | ARRAY ✓ |
| `proto3($n // "dflt", …)` | dflt | dflt ✓ |
| `proto3(1 + 2, …)` | 3 | 3 ✓ |

So a `$` slot handles expressions and `//` fine in general; the trigger is an
**anonymous constructor followed by `//`** in a prototyped slot.  A `$`
prototype imposes scalar *context* on a full expression — it does not end the
argument at the `]`.  The unprototyped path gets this right, so this is a
second parse route not sharing the `//`-vs-empty-pattern decision: the
CLAUDE.md-11 "count the parse paths" smell, and the same region as §1.
**Ask: is this fixable in place, or does it want the same non-mutating parse
that §1 and Option B need?**  I did not attempt it — §1 is exactly where
three attempts died.

Impact is wider than one row: every `$`-prototyped sub, which includes
test.pl's `is`/`like`/`isnt`/`cmp_ok`.  With #147 fixed, op/dor.t is
otherwise fully explained (t26/t28 are error-text/invalid-input, §6d) and
would register or pass.

### 6f. Confirm the XDIFF registration bar I have been applying

I registered four files in `docs/perl-suite-expected.tsv` this session
(op/localref.t §DESTROY, io/defout.t §format/write, op/not.t §`!0`/`!1`, and
earlier rows).  The rule I used: **register only when EVERY failing test in
the file is explained by an already-blessed `not-supported.md` section** —
so op/bless.t stayed unregistered because only 2 of its 6 failures are
DESTROY, and op/dor.t stays unregistered because t4 is a real bug (#147).
That follows the file's own header ("the reason MUST cite the
docs/not-supported.md section"; "Do NOT add rows for unexplained crashes")
and mirrors the skip-registry philosophy, but the all-or-nothing part is my
inference, not something the header states.  **Ask: confirm, or should
partially-explained files get rows too** (with the unexplained tests tracked
separately)?  The trade-off is that a per-file row makes a file stop
reporting UNEXPLAINED even while it still contains real bugs.

### 6g. Should a missing case DIE rather than return a default?

The `vec` bug this session is the archetype.  Both `p-vec` and `p-vec-set`
had per-width branches for 1/2/4/8/16/32 and **no 64** — despite both
docstrings listing 64 as legal.  The reader fell through to a `(t 0)`
default; the writer's `cond` simply had no matching clause, so
`vec($x,1,64) = $q` extended the string to the right length and wrote
**nothing**.  Silent, plausible-looking, all-zero output.

That is the same failure mode as #138's silently deleted statement and the
`\$!` snapshot: a **default arm that swallows an unimplemented case**.  The
alternative is a `t` arm that dies "Parser2 TODO"-style with the unhandled
value in the message.  The two shipped consolidations (#138, #140) both
argue the same way — a missing entry in a hand-rolled table produced a wrong
value rather than a complaint.

**Ask:** is a general rule wanted — *runtime `cond`s over a closed set of
legal values end in an explicit error, not a default* — and is R1 the moment
to sweep for such arms, or is that E5 hygiene?  My instinct is that the
sweep should be a separate audit (it will find many), but that new code
should adopt the rule immediately.

### 6h. How far to chase perl's exact bytes for host-visible values?

I implemented perl's **drand48** so `srand(1); int rand(1000)` is 41, matching
perl exactly, rather than merely making PCL self-consistent (which would have
satisfied "same seed replays" but failed t/op/rand.t).  I think that was
right — seeded `rand` is how CPAN modules and test suites get determinism, so
it is observable behaviour, not an implementation detail.

**Ref addresses are NOT an open case — they are already decided, and they
are the best worked example of the rule** (user correction; I had wrongly
listed them here as an approximation).  `object-address`
(cl/pcl-runtime.lisp:1477) deliberately does **not** use SBCL's raw pointer:
the compacting GC relocates objects and PCL re-boxes refs on some paths, so
the pointer is not a stable identity.  The concrete failure it was built for
is recorded in the comment — a coderef threaded through Sub::Defer's
coderef-keyed `%DEFERRED` presented two different `CODE(0x..)` strings for one
logical sub, breaking Moo's lazy/subclass bootstrap.  Instead each object
gets a monotonic id on first identity request, held in a weak-on-key table so
dead objects do not leak, and **ids are never reused, so distinct objects can
never collide** — explicitly "an improvement over reusable raw addresses",
since perl recycles freed addresses.

So the settled answer there was: **match perl's SHAPE (`ARRAY(0x…)`,
`refaddr`, `==` on refs) and its invariants, not its bytes — and where
perl's bytes carry a defect, do better.**  That is a stronger rule than the
one I was about to propose, and it already covers the remaining examples:
`$$` at image boot, hash ordering.

**Ask:** confirm that as *the* rule, so it can go in `docs/ir-spec.md`
alongside the data model rather than living in a code comment.  The drand48
case then reads as the exception it should be — bytes matter there only
because a program can branch on the sequence, which is exactly the test
`t/op/rand.t` performs.

### 6i. Is the perl-tests/ vs t/ corpus split still earning its keep?

`tools/run-perl-suite.pl` skips any t/ file whose basename exists in
`perl-tests/` ("the sweep owns those") unless `--include-copied`.  But the
copies have drifted: `perl-tests/chop.t`, `dor.t`, `not.t`, `quotemeta.t` are
all **fully passing** in the sweep, while the *real* t/ files behind the same
names have 2-5 failures each — the ones I have been fixing this session
(op/not.t, op/quotemeta.t, op/dor.t) were invisible to the default runner
view precisely because a passing copy shadows them.

**Ask:** should the copied-file skip be dropped (or inverted, so the t/
original is authoritative and `perl-tests/` is only the fast sweep), and
should the drifted copies be re-synced?  This affects what #25's scoreboard
actually measures — a green sweep row can coexist with a failing real file
of the same name, which is a misleading signal for a release gate.

### 6j. `pack`'s character-vs-byte mode: how much of the model to build? (#148)

`unpack("U*","\x{1234}")` gives 4 in PCL and 4660 in perl; `ord(pack("U",0x1234))`
gives 225 (0xE1, the first UTF-8 byte) instead of 4660.  Diagnosed, **not
attempted** — it is a mode model, not a coding slip, and pack is the highest-
blast-radius area in the tree.

Perl has two modes: **character** (the default — `U` packs the codepoint
directly) and **byte** (entered by `U0`, left by `C0` — `U` encodes UTF-8),
and the switches are **mid-template**.  `cl/pack-impl.pl` inverts this: the
`U` letter *always* routes through `_pack_utf8_char`/`_unpack_utf8_char`
(i.e. always byte behaviour), while `U0` is handled only as a template
*prefix* whose effect is to pre-encode the whole input — so `U0` currently
means "transform twice" rather than "switch mode".

**Ask:** implement the real mode state threaded through
`_pack_tmpl`/`_unpack_tmpl` with mid-template `U0`/`C0` toggles, or only fix
the default-character-mode case (which is what the failing tests and almost
all real code use) and leave mid-template switching unimplemented with a
`docs/not-supported.md` note?  The full model is more code in the file whose
regression suite is 5635 assertions; the partial fix is a smaller diff that
still leaves a known hole.  Related: the same byte-vs-character question
probably applies to `W` vs `C`.

Constraint either way: `cl/pcl-pack.lisp` is a checked-in transpiled
artifact, so `tools/rebuild-pack` must land in the same commit, verified with
pack.t (blessed 5635/90) before the full gate+sweep.

---

## 7. Discoverability of decided questions — a process proposal

*Raised because this session wasted real time re-deriving things the project
had already settled, and once mis-stated a good decision as an open one.
The content is fine; the entry points are not.*

Settled facts currently live in six places with no single index: `CLAUDE.md`,
`docs/not-supported.md`, `docs/ir-spec.md`, the `*-review.md` design docs,
task descriptions, and **code comments**.  All four failure modes below are
from this session:

| what happened | where the answer already was | why it was missed |
|---|---|---|
| Probed four `DESTROY` shapes before checking | `not-supported.md` §DESTROY | no rule saying "grep it FIRST" |
| Wrote up ref addresses as an open question | a comment in `cl/pcl-runtime.lisp` | **not in `ir-spec.md` at all** |
| Misread the suite scoreboard twice (P vs C) | `run-perl-suite.pl` header ~line 69 | nothing at the point of use said so |
| Three failed attempts in PExpr's operand region | `pexpr-term-parsing-review.md` | not in CLAUDE.md's "Key Files to Read" |

**Proposals, highest leverage first.**

1. **`docs/DECIDED.md` — a one-line index of settled questions**, each with a
   pointer, not prose: "DESTROY → not-supported §DESTROY.  Ref identity →
   ir-spec §2.5.  Error-message text → not-supported §Error message text.
   `!0`/`!1` → §Interned boolean constants."  The value is that it is ONE
   grep before touching anything; today it takes knowing four files.
2. **Put the triage order in `CLAUDE.md` itself**, as a numbered procedure
   near the top: read the failing test → grep `DECIDED.md` → grep
   `not-supported.md` → *only then* probe.  `test-debugging-runbook.md` has
   the FIX-vs-REGISTER tree, but the ordering rule is not where someone
   trips over it before starting.
3. **Rule: a load-bearing design decision belongs in `ir-spec.md`, not only
   in a comment.**  Ref identity is the proof — a genuinely good decision
   (monotonic ids, never reused, stable across GC relocation; strictly better
   than perl's reusable addresses) that was invisible to anyone reading the
   normative manual.  Moved into §2.5 this session; the comment should point
   at the spec rather than be the only copy.
4. **A format legend where data is CONSUMED, not only where it is produced.**
   `docs/perl-suite-run.tsv` has no header line, and the only authoritative
   statement that `P`=perl / `C`=PCL is in the tool source.  One comment line
   in the tsv would have prevented two wrong readings — and the second wrong
   reading produced a "near-green" worklist that was entirely fictitious.
5. **Record failed attempts in the task, not just the goal.**  #142 now
   carries all three things tried and why each died; #148 records that the
   root is a mode model rather than an encoding slip.  Tasks that say what
   NOT to retry are worth more than tasks that only say what to do.

**What already works and should not change:** `not-supported.md`'s entry
style (Perl behaviour / PCL behaviour / rationale) is what made confident
XDIFF classification possible once found, and the skip-registry +
expected-tsv mechanism with stale-detection is the right design.  The gap is
discovery, not content.

**Ask:** are 1 and 2 worth doing (the rest are cheap follow-ons), and if so
does `DECIDED.md` want to be hand-curated or generated from the section
headings of `not-supported.md` + `ir-spec.md`?

## Verification standard used for the two shipped commits

corpus-diff (every changed file explained) → `tools/prove-core` full gate →
full sweep `--jobs 8 --timeout 380` + `sweep-diff` vs `docs/fail-baseline.tsv`
→ `v2-census`.  Artifacts regenerated and confirmed header-only at each gen
bump (v2-89, v2-90).  Regression batteries in `Pl/t/transpile-test-07.t`
(now 31 tests).
