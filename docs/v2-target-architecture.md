# PCL v2-final — target architecture

*The north star the E5 steps converge on.  Written for a contributor who
has never seen this codebase: this is the machine we are building, why
each piece exists, and where YOUR optimization pass plugs in.  The
migration path and calibrated costs live in `docs/v2-endgame-plan.md`;
the gap analysis that produced this document is `docs/v2-code-review.md`.
Semantics of the GENERATED code are specified separately and normatively
in `docs/ir-spec.md` — this document is about the compiler's own shape.*

Status: design target (s316t).  Sections marked **[today]** describe
what already exists and is kept; **[E5.x]** marks the step that builds
the rest.

*Progress note (2026-08-25):* since this was written, the target's two
biggest structural planks have landed — there is **one pipeline** (the
whole-file v1 fallback was deleted, s356) and **one expression compiler**
(`ExprToCL2` and its discarded second parse deleted; the two emission
rules it carried became named passes — s411, Phase A of
`docs/plan-one-compiler-s411.md`), and the **pass registry this document
specifies exists** as `Pl/Passes.pm` (`PCL_OPT`; Kind-A facts-licensed
emissions live, Kind-B CLForm passes still empty).  The E5 step numbering
below is superseded by `docs/plan-one-compiler-s411.md`; the live work
queue is `docs/plan-post-s433.md`.

---

## 1. The pipeline as data transformations

One direction, six arrows, a named data structure on every edge.  No
stage re-parses the output of a later stage; no stage mutates the input
of an earlier one.

```
 Perl source text
        │  normalize            (regex-level source fixes + reparse)
        ▼
 PPI document                   (foreign AST: tokens, statements, blocks)
        │  classify             (ONE statement-shape reader)
        ▼
 Stmt records                   (typed: decl / write / compound / call / …)
        │  analyze              (fact passes, side-effect-free)
        ▼
 Stmt records + Facts           (per-name verdicts, per-stmt context)
        │  lower                (ONE lowering, consults Facts)
        ▼
 CLForm tree                    (structured CL: nested arrays + atoms)
        │  optimize             (registered CLForm→CLForm passes)
        ▼
 CLForm tree (final)
        │  print                (THE printer — the only text producer)
        ▼
 Common Lisp text
```

The two properties every arrow preserves:

- **Parens balance by construction** — text exists only after `print`.
  **[today]** (`Pl/CLForm.pm`; invariant 1 of the endgame plan).
- **Verifiable in isolation** — every arrow can be diffed corpus-wide
  against the previous commit (`tools/corpus-diff.pl`), and the whole
  pipe against real perl (the sweep).  A change to one stage that
  byte-changes nothing is provably a refactor.  **[today]** — this
  verification harness is the project's crown jewel; nothing below
  weakens it.

## 2. The data structures

### 2.1 PPI document — the foreign AST **[today]**

We do not own the Perl parser; PPI is imperfect but battle-tested.  All
PPI bug workarounds live in ONE place: `normalize` (today
`_preprocess_source` + the pre-pass rewrites).  Rule: a workaround
either edits SOURCE TEXT and reparses, or it doesn't exist — no pass
downstream of `classify` may patch tokens.

### 2.2 Stmt records — the typed statement layer **[E5.5, seeded by task #138]**

The single biggest clarity defect of the current code is that statement
shapes are recognized *at the point of use* by pattern-matching raw
token arrays — ~30 sites, each re-deriving a sliver of Perl's grammar
("everything after `=` is the RHS"), several provably wrong on edge
shapes (review §2).  v2-final replaces the culture with one reader:

```perl
# The ONLY place statement-level grammar knowledge lives.
my $s = Stmt::classify($ppi_stmt, $env);
# $s->{kind}    : 'decl' | 'write' | 'compound-write' | 'incdec'
#               | 'expr' | 'return' | 'loopctl' | 'compound' | 'package'
#               | 'sub' | 'use' | 'local' | 'label' | ...
# $s->{mod}     : postfix modifier (kind + cond tokens), or undef
# $s->{lhs}/{rhs}/{init}/{names}/{tail} : token spans, SPLIT CORRECTLY —
#   classify owns the one low-precedence table (depth-0 , => or and xor),
#   so a below-assignment tail is returned as ->{tail}, never folded in.
```

Consumers switch on `kind` and receive pre-split spans; they never index
into token arrays.  The classifier is small (the knowledge already
exists, scattered), pure, and testable in isolation — and it is the one
place a grammar bug can live, which is the point.

### 2.3 Facts — analysis results as data **[today, generalized in E5.5]**

`Pl/VarAnnotator.pm` is already the model: a side-effect-free pass over
the statements that returns immutable per-name verdicts
(`unboxable`, `strbuf`, capture/span/shadow facts), consumed by lowering.
v2-final keeps its shape and adds the two facts currently smuggled
through shared mutable state:

- **lexical scope map** — which names are let-bound where (replaces the
  live `_let_bound_vars` accumulator with its 8 save/restore pairs);
- **context map** — the statement/expression `*wantarray*` context,
  computed once (replaces the three parallel encodings — v2 strings,
  PExpr integers, runtime values — with one constant set).

The contract that makes optimization safe: **facts license emissions**.
An emitter may use a fast shape only if a fact grants it; a pass that
cannot prove a fact emits the general (boxed) form.  Wrong = slow-but-
correct, never fast-but-wrong.  **[today]** — this is the raw-verdict
design (`docs/raw-numeric-verdict.md`); v2-final makes the
license-check a shared predicate (`native_root_write`) instead of
comment-coordinated parallel logic.

### 2.4 Expression trees **[today: PExpr OpcodeTree; E5-hardened]**

PExpr's precedence parser and its OpcodeTree stay — they are correct and
carry ten months of Perl edge-case knowledge.  Two hardenings:

- **non-destructive parse** (the E5-quality endpoint of review §2): the
  fat-comma rewrite and parse-state keys move from `set_content`
  mutation to annotations, deleting the snapshot/restore machinery
  around every speculative parse — and with it the *reason* the token
  fast-path culture existed;
- prune v1-only parameters and the block-to-string escape hatches once
  E5.2/E5.3 close them.

### 2.5 CLForm — the output IR **[today]**

Unchanged: atoms + `[head, @args]` lists + the one printer.  Two
additions land with the already-planned Target B flag-day (task #75):

- **macro vocabulary**: every CL-specific mechanism behind an obvious
  `p-*` macro, so the tree reads as "Perl semantics in prefix notation"
  — which is also what makes CLForm→CLForm passes tractable to write;
- `raw`/`raw_wrap` deleted (E5.4) — after which a CLForm tree is CLOSED:
  a pass can walk it exhaustively with a 10-line visitor, no opaque
  text islands.

## 3. Plugging in an optimization

This is the payoff the whole shape buys.  An optimization is one of two
kinds, and both have a fixed contract:

**Kind A — a facts-licensed emission** (compile-time specialization).
Add a fact pass (or extend a verdict), then teach the emitter the fast
shape it licenses.  Existing examples **[today]**: raw-numeric slots
(`setf` + machine arithmetic instead of box traffic), the string-append
buffer, counting-loop `p-for`.  The contract: the fact must be
justified by a conservative static argument; the general form remains
the fallback; corpus-diff explains every changed file; the sweep stays
at baseline.

**Kind B — a CLForm→CLForm pass** (post-lowering rewrite).  A pure
function over the closed tree, registered in an ordered list:

```perl
# Pl/Passes.pm (E5.4+)
register_pass('inline-method-cache', \&pass_imc);   # task #73
sub pass_imc {
  my ($form, $facts) = @_;
  # walk: rewrite (p-method-call $obj 'name ...) whose receiver class
  # is monomorphic-in-practice into
  # (p-method-call-cached #<site-id> $obj 'name ...) — the runtime
  # macro holds the per-site cache; semantics unchanged on miss.
  ...
}
```

Worked examples queued today that become Kind-B passes instead of
hand-edits: the method-dispatch inline cache (#73, ~15× measured on
dispatch-bound code), pack/sprintf constant-template memoization (#74),
return-family transfer (#77 — reads `sub_info` facts, rewrites
call-site writes to the raw family).  Each lands as: one pass file, one
registration line, corpus-diff + bench + sweep.  Nothing else moves.

**Why this is safe to open to others:** the verification loop is
already mechanical (corpus byte-diff, 4k-test gate, 15k-assertion sweep
vs real perl, CPAN suite scoreboard), and the facts contract means a
wrong pass produces a *visible* diff or a *slower* program, not a
silent miscompile.  A contributor can write a pass knowing the harness
will catch them.

## 4. Module map (v2-final)

| module | role | provenance |
|---|---|---|
| `Pl/Normalize.pm` | source fixes + PPI workarounds, reparse-based | split from Parser.pm/Parser2.pm pre-passes |
| `Pl/Stmt.pm` | THE statement classifier (§2.2) | new, small; absorbs ~30 scattered matchers |
| `Pl/PExpr.pm` (+ PExpr/*) | expression parsing → OpcodeTree | today's, non-destructive |
| `Pl/Facts.pm` | fact passes: box verdicts, scopes, context, captures | VarAnnotator generalized |
| `Pl/Lower.pm` | statements × expressions → CLForm | Parser2's lowering + EmitCL (ExprToCL folded, one brain) |
| `Pl/Passes.pm` | registered CLForm→CLForm optimizations | new (§3) |
| `Pl/CLForm.pm` | the form model + THE printer | today's |
| `Pl/Environment.pm` | packages/prototypes/constants | today's |

Gone: `Pl/Parser.pm` (earned via E5.3), `Pl/ExprToCL2.pm` (folded),
the seam machinery (nothing to fall back to), `raw`/`raw_wrap`.

## 5. How hard is "really good"?  (the honest estimate)

Measured against the project's calibrated velocity (endgame plan §4):

| increment | content | sessions |
|---|---|---|
| R1 residue | #138 splitter seed (pre-release must-fix) | done pre-R1 |
| E4.1 | dual-pipeline property removed | 1–2 |
| E5.1–E5.5 | seam object → embed totality → fallback burn-down → one brain → shared predicates/split | 9–17 |
| §2.2 classifier | Stmt::classify absorbing the matcher culture | 2–3 (inside/after E5.5) |
| §2.4 non-destructive PExpr | delete snapshot machinery | 2–4 |
| §3 pass framework | Passes.pm + port #73/#74/#77 as passes | 2–3 (overlaps Target A tier-1 work already planned) |
| **total to v2-final** | | **~16–29 sessions post-R1** |

Two things make this number credible rather than optimistic.  First,
**no step is a rewrite**: every increment ships alone behind the
byte-diff + sweep harness, exactly how E1–E3 landed (111/111 files
native was reached at ~2.8 files/session with zero big-bang risk).
Second, **the "historic garbage" is localized**: it is (a) the v1
statement layer — deleted by the E5.3 burn-down, (b) the seam machinery
— deleted when (a) completes, and (c) the destructive-parse
workarounds — deleted by §2.4.  The parts worth keeping (PExpr's
precedence knowledge, the runtime's semantics, CLForm, the verdict
design, the test oracles) are exactly the parts that took ten months to
learn and would have to be re-learned by any clean-room rewrite.

For contrast: a true greenfield v3 ("nice data structures from day 1")
would re-derive that knowledge against the same oracles — on this
project's own calibration data that is a 60+ session program with a
long correctness trough in the middle, i.e. 2–3× the incremental path
to reach the *same* end state.  The incremental path also keeps every
intermediate week shippable.  **Recommendation: v2-final by burn-down,
no v3.**

## 6. Invariants (restated for the final shape)

1. No text-form CL between parse and print; CLForm trees are closed.
2. One printer.
3. One statement classifier; one low-precedence table; one fact-scan +
   rename engine.
4. Anything unlowerable dies loudly, backed by a blessed
   `docs/not-supported.md` entry.
5. Oracles are external (real perl via sweep/suite/fuzzer), never the
   compiler's own previous output.
6. Facts license emissions: unproven = general form.  Wrong analysis
   degrades speed, never correctness.
7. Optimizations are registered passes with the §3 contract — never
   inline special cases in the lowering.
