# V2 Completion Plan — from 32-file prototype to default pipeline

**Written:** 2026-07-04 (session 271b), for implementation by Claude Opus 4.8.
**Prereq reading, in order:** `docs/parser2-prototype.md` (architecture +
session history), `CLAUDE.md` (§10 paren discipline, §11 reuse-don't-duplicate),
`docs/test-debugging-runbook.md`, `docs/eval-lexical-capture.md`.
**Baseline commits:** `899c3ba` (A3), `0a645e0` (A2), `0a88cff` (this plan).

This document is prescriptive. Where it says "die → v1", that is a
`die "Parser2 TODO: …\n"` which pl2cl's `parse_with_fallback` catches and
retries through v1 — the *sanctioned* way to not support something. Where it
says "verify parity", that is the full Working Loop in §2, not a spot check.

---

## 0. How to work (read this first)

### 0.1 Orientation exercise (do before any edit)

Run these and read the output until each line makes sense:

```bash
cd /home/bernt/pcl
echo 'my $x = 1 + 2; print "$x\n";' | PCL_V2=1 ./pl2cl --no-cache   # v2 output
echo 'my $x = 1 + 2; print "$x\n";' | ./pl2cl --no-cache            # v1 output
prove Pl/t/parser2-01.t                                             # 90 tests, all pass
```

Then skim `Pl/Parser2.pm` top-to-bottom once (≈1200 lines). The function
map in §5.1 tells you what each sub is for.

### 0.2 The micro-loop (after every single edit)

1. `perl -I. -c Pl/Parser2.pm` (or whichever module you touched).
2. If you touched a `.lisp` file: run the CLAUDE.md §10 paren checker on it
   and fix any nonzero depth **before doing anything else**.
3. `prove Pl/t/parser2-01.t` — must stay 100%.
4. A 5-line targeted transpile check of the thing you changed
   (`echo '…' | PCL_V2=1 perl -I. pl2cl --no-cache`), eyeballed against
   v1's output for the same input.

### 0.3 The macro-loop (after every work item) — see §2

Census + parity sweep, both pipelines, exact match. Then commit, then
update docs (§6). Never batch two work items into one commit.

### 0.4 Rules that override your instincts

- **When a construct is hard, gate it** (`die "Parser2 TODO: …\n"`). A gate
  is always correct; a half-implementation is how silent wrong answers
  happen. Coverage lost to a gate shows up in the census and can be
  reclaimed later; a wrong answer can ship unnoticed.
- **Never silently skip anything.** If you cannot lower a child node,
  either fallback the statement or die. Do not emit a comment and move on.
- **Copy v1's emitted shapes exactly** when routing new constructs. Do not
  "improve" a form's shape while porting it — byte-similar output makes
  diff-triage possible. Improvements come later, measured.
- **Do not edit `perl-tests/*.t`, ever.** Not to debug, not to simplify.
  Make scratch copies under the scratchpad dir if you need to bisect a file
  (and note `require "./test.pl"` means bisected copies usually break —
  prefer instrumenting via minimal standalone repros).
- **Do not trust line numbers in this document** — they drift. Every
  anchor is given as a function name or a unique grep string.
- **`--no-cache` on every experimental pl2cl invocation.** Cached module
  transpiles will otherwise mix pipelines mid-experiment.
- **grep runpl output with `grep -a`** (it can contain NUL bytes).
- Commands run from the repo root (`/home/bernt/pcl`). The Bash tool resets
  CWD between calls — do not rely on a `cd` from a previous call.

### 0.5 Know your risk profile — and when to escalate

The work items are not uniformly hard. **W1, W4, W6, W9 are mechanical**:
the steps below plus the working loop should carry you through. **W2's
aftermath, W7, and W12 are not** — they are open-ended debugging/triage
over dozens of files, where the plan can scaffold judgment but not replace
it:

- **W2 aftermath**: many files become v2-native at once and hit v2 paths
  nothing has exercised before. Expect novel failures of the §3 classes.
- **W7**: full-sweep parity triage — every delta needs a root cause.
- **W12**: annotator equivalence proofs.

**Escalation rule:** if you have spent more than ~3 focused
attempt-analyze-retry cycles on a single file's divergence without a root
cause, or you notice you are *rationalizing* a delta ("probably flaky",
"close enough", "v2 is just better here") instead of proving it — STOP.
Do not paper over it, do not weaken a test, do not accept the delta.
Commit the good work you have, write the open question into
`docs/parser2-prototype.md` (file, symptom, what you ruled out, generated
CL diff location), and tell the user that this item needs a stronger
model or their input. An honest handoff of a hard bug is a good outcome;
a plausible-looking wrong explanation is the worst possible one.

### 0.6 Glossary

- **v1** — `Pl::Parser` + `Pl::ExprToCL`, the original text-stream pipeline.
- **v2** — `Pl::Parser2` + `Pl::ExprToCL2` + `Pl::VarAnnotator` +
  `Pl::CLForm`, the structured pipeline, selected by `PCL_V2=1`.
- **native** — lowered by v2's own code into CLForm forms.
- **raw / seam** — CL text produced by the embedded v1 machinery, embedded
  opaquely as a `Pl::CLForm::Raw` leaf.
- **gate** — a `die "Parser2 TODO: …"` that sends the whole file to v1.
- **segment** — a top-level run of statements belonging to one package
  (created by splitting at `package` statements in `parse()`).
- **section** — a segment's output record `{pkg, decls, defs, run,
  captured}`; sections are concatenated in `parse()`'s assembly loop.
- **bucket** — v1's name for its output streams (preamble / declarations /
  definitions / runtime); the statement seam reads v1's buckets.
- **box** — a `p-box` mutable cell; the default representation of a Perl
  scalar. **raw slot** — a let binding holding a bare CL number/string
  instead of a box (VarAnnotator-approved only). The invariant: a raw slot
  must never receive a box.

---

## 1. Where things stand

> **s273 update:** W1–W8 are DONE (W8 pending its final re-sweep, see the W8
> box). 69 files were v2-native after W6; re-census after the D28 gate. The
> remaining road to "working v2 compiler" is, in order: **W8 re-sweep → W9
> (flip the default; cache keying FIRST) → W8.5/W10 as cleanup → W11 + W14
> (the two big PERF items) → W12 (the annotator rewrite that retires the
> text-scan disqualifiers) → W13 (only if measurements justify)**. The list
> below this note describes the pre-W1 state and is kept for history.

- `PCL_V2=1` selects `Pl::Parser2` in `pl2cl` (see `parse_with_fallback`
  there; eval-mode and any special opts always route to v1; `--lenient-ppi`
  is deliberately ignored for the v2 attempt — it only matters when PPI
  can't parse at all, and then Parser2 dies to v1 anyway).
- **32 of 111 `perl-tests/*.t` lower fully through v2** at exact v1 sweep
  parity: 1175 pass / 9 fail / 29 fully-passing — identical on both
  pipelines. The 9 fails are pre-existing v1 bugs (flip.t 3, delete.t 3,
  grent.t 1 env-dependent, …), NOT v2 work.
- Guard file: `Pl/t/parser2-01.t` (90 tests). Every work item below must
  extend it.
- Remaining first-gate census (each file shows only its FIRST gate):

  | files | gate | work item |
  |---:|---|---|
  | 65 | string eval (`eval EXPR` text scan) | W2 + W3 |
  | 4 | package block form | W1 |
  | 2 | `package` inside a block | W1 (assess) |
  | 2 | sub with prototype/signature | W4 |
  | 4 | lexical captured by a named sub | W5 |
  | 1 | loop with continue block | W6 (optional) |
  | 1 | `my $aa, $bb, $cc;` | W6 (optional) |

---

## 2. The Working Loop (non-negotiable, after EVERY work item)

```bash
# 0. Syntax + paren discipline (after every edit)
perl -I. -c Pl/Parser2.pm          # and any other touched module

# 1. Guards
prove Pl/t/parser2-01.t            # must be 100%

# 2. First-gate census (which files lower through v2 now?)
for f in perl-tests/*.t; do
  r=$(PCL_V2=1 PCL_V2_VERBOSE=1 perl -I. pl2cl --no-cache --lenient-ppi $f \
      2>&1 >/dev/null | grep -m1 'fell back');
  echo "$f|${r:-V2-NATIVE}";
done > /tmp/census.txt
grep -c V2-NATIVE /tmp/census.txt
# summarize gates:
perl -F'\|' -lane '$F[1] =~ s/.*Parser2 TODO: //; $F[1] =~ s/ at \S+ line \d+\.?$//;
  $c{$F[1]}++; END { printf "%3d  %s\n", $c{$_}, $_
  for sort { $c{$b} <=> $c{$a} } keys %c }' /tmp/census.txt

# 3. Parity sweep over ALL v2-native files, BOTH pipelines
FILES=$(grep V2-NATIVE /tmp/census.txt | cut -d'|' -f1 | tr '\n' ' ')
PCL_V2=1 perl sweep-perl-tests.pl --jobs 8 $FILES   # v2
cp -r .faillog /tmp/faillog-v2
perl sweep-perl-tests.pl --jobs 8 $FILES            # v1 baseline
cp -r .faillog /tmp/faillog-v1
```

**Parity means the two sweeps match EXACTLY** — same totals, same per-file
pass/fail counts, same fully-passing list. Two hard rules learned in
session 271:

1. **v2 scoring HIGHER than v1 is a bug until proven otherwise.** grent.t
   "passed" 3/3 under v2 because a broken `while (<GR>)` silently processed
   zero entries and the test's degenerate path passed. When v2 beats v1 on
   any file, transpile both, diff the generated CL, and explain the delta
   before accepting it.
2. **A file that regresses is debugged with the cookbook in §5.4.** Never
   shrug off a one-test delta as noise without re-running it twice
   (sweep flakiness exists but is rare; see memory
   project_sweep_flakiness_investigation).

Sweep notes: a full 111-file sweep takes ~15–20 min at `--jobs 8`; the
subset sweeps above are 2–5 min. `heredoc.t`, `list.t`, `lfs.t` are
skipped by the harness (known hangs) — ignore them. The sweep prints
`Crashed (SBCL)` and `Partial (early stop)` categories — under parity
those must match v1's categories too, not just the totals.

---

## 3. The five silent-failure classes (check before writing code)

Session 271 found five latent v2 bugs that produced **silently wrong code**
— no fallback, no crash at transpile time. Every new statement/expression
form you lower natively must be checked against this list:

1. **v1 does more than the obvious emission.** Before lowering construct X
   natively, read v1's handler for X end-to-end and list every *extra*
   rewrite it applies. Example: v1's while-condition path applies
   `_auto_defined_cond` (each/readline/readdir/glob → implicit `$_` assign +
   `p-defined` wrap). v2 missed it; loops mis-terminated. The fix pattern:
   apply v1's rewrite function at the raw seam (see `_auto_defined_raw`) —
   native forms can't contain those calls, so raw-only is complete.
2. **Never silently drop a child node you don't recognize.** v2's while
   branch took "the first Block" and dropped `continue` blocks → infinite
   loop. If a construct can carry extra children (continue, labels, attrs),
   either handle them or `die "Parser2 TODO: …"` when they're present.
   Concrete technique: after destructuring `@k = $stmt->schildren`, account
   for every element; `die` if anything significant is left over that you
   did not consume.
3. **The printer's one-line flattening vs raw text.** `Pl::CLForm::_flat`
   must return undef for any chunk containing a newline OR a `;` outside a
   string literal (a flattened `;; comment` raw swallows every sibling after
   it on that line). This is fixed (`_no_flat`); don't weaken it. Any new
   CLForm node type must define its `_flat` behaviour explicitly.
4. **Environment state the fallback reads must be mirrored.** The embedded
   v1 machinery consults shared state; v2 must maintain ALL of it:
   - `environment->in_subroutine` — bumped in `_lower_sub`; drives bare
     shift/pop → `@_` vs `@ARGV`, my-qualification, top-level checks.
   - `environment->package_stack` — set via `_set_cur_package` per segment.
   - `fallback_parser->{_let_bound_vars}` — my-vs-package decisions, the
     `p-scalar-=`→`p-my-=` raw rewrite, and (W3) `_eval_lexical_alist`.
   - `environment->state_var_renames` — reset in `parse()`.
   When you route a new construct through the seam, grep its v1 handler for
   `$self->environment->` and `$self->{_` reads and ask what v2 needs to
   have set. This class of bug produces the sneakiest failures (exp.t's
   `shift` read `@ARGV` and returned 0.0 from every trig wrapper).
5. **VarAnnotator must see every WRITE shape.** A missed write → raw slot →
   the write vanishes (`($a,$b) = each @a` wrote into `(vector 7 …)`).
   Current write detectors (all in `Pl/VarAnnotator.pm` step 3/4):
   `$x = RHS` statements, `$x++/--`, compound assigns, `=~`, `local`,
   `pos`, foreach vars (my and plain), list-assign LHS `($x,…)=`,
   chomp/chop/undef/read/sysread/recv args. If you make a new construct
   native and it can WRITE a scalar, add a gate + a guard test.
   Over-firing (keeping a box) is always safe; a missed write never is.

Also inherited PPI gotchas: `find` returns `0` not undef → always `|| []`;
`PPI::Statement::Scheduled` ISA `Statement::Sub` (exclude it from sub
handling); `for(;;)` sections are positional with `Statement::Null`
placeholders and absent trailing sections; a `Statement`'s content includes
its trailing `;`; `$_->schildren` skips whitespace/comments, `->children`
does not.

---

## 4. Work items, in order

Each item ends with **Acceptance** (what must be true before you commit)
and **Guards** (tests to add to `Pl/t/parser2-01.t`). Test style: shape
tests via `like($cl, qr/…/)` on `Pl::Parser2->parse_code(...)` output;
runtime tests inside the existing `SKIP:` block at the bottom via the
`$run->($cl_text)` helper (bump the skip count when adding). The
`paren_balance()` helper there guards raw seams — use it whenever your
feature involves raw text.

### ~~W1. Package block form `package Foo { … }` (4 files) + versioned packages~~ — DONE (s272, this commit)

**Files:** `Pl/Parser2.pm` only.
**Anchors:** in `parse()`, the segment-split loop (search
`"PACKAGE SEGMENTS"`); the two dies `"package block form"` and
`"package with version"`; the assembly loop (search
`"---- Assemble the sections"`).
**v1 model to read first:** `Pl/Parser.pm sub _process_package_statement`
(the `if ($block)` top-level branch) and `sub _emit_package_version`.

What v1 does for a top-level block form, in order: full package preamble
for Foo → process the block's children as Foo → then **open a NEW section
for the enclosing package whose preamble is ONLY `(in-package <prev>)`**,
plus a runtime `(p-set-current-package <prev> "<prev>")`. That short-form
return section is the key fact — do not re-emit
`p-defpackage`/`defclass plc-*`/`p-register-pkg-name` for a package that
already has a section.

**Step-by-step:**

1. Extend the segment record with two fields. Current shape is
   `{ pkg => 'main', stmts => [] }`; make it
   `{ pkg => …, stmts => [], reopen => 0 }` (`reopen` = this section's
   package was already opened earlier in the file, emit short form).
2. Rewrite the split loop with an explicit `$cur_pkg` and an `%opened` set:

   ```perl
   my @segments = ({ pkg => 'main', stmts => [], reopen => 0 });
   my $cur_pkg = 'main';
   my %opened  = (main => 1);
   for my $child ($doc->schildren) {
     if ($child->isa('PPI::Statement::Package')) {
       my ($block) = grep { $_->isa('PPI::Structure::Block') } $child->schildren;
       my $pkg = $child->namespace // 'main';
       my $version = eval { $child->version };
       # PPI quirk (see v1 _emit_package_version): ->version returns the
       # BLOCK text for an unversioned block form — only accept real
       # version literals.
       undef $version unless defined $version && $version =~ /^v?\d+(?:[._]\d+)*$/;
       if ($block) {
         push @segments, { pkg => $pkg, stmts => [$block->schildren],
                           reopen => $opened{$pkg}++ ? 1 : 0,
                           version => $version };
         # return to the ENCLOSING package; its section already exists
         push @segments, { pkg => $cur_pkg, stmts => [], reopen => 1 };
         next;                          # $cur_pkg unchanged
       }
       push @segments, { pkg => $pkg, stmts => [],
                         reopen => $opened{$pkg}++ ? 1 : 0,
                         version => $version };
       $cur_pkg = $pkg;
       next;
     }
     push @{ $segments[-1]{stmts} }, $child;
   }
   ```

   Notes: the old dies for block form and version disappear here.
   A block-form package NESTED inside another block still dies via
   `_lower_stmt`'s `Statement::Package` branch — that gate stays (see the
   "package inside a block" note below). `$block->schildren` as the
   segment's stmts is exactly what the per-segment loop expects (it
   already greps out Statement::Null).
3. In the assembly loop, honor `reopen`. Today the `if ($i > 0)` branch
   emits the full preamble; change to:

   ```perl
   if ($i > 0 && !$sec->{reopen}) {   # first section for this package
     …existing full preamble…
   } elsif ($i > 0) {                 # reopened (return) section: short form
     push @body, ";;; back to package $pkg",
                 "(in-package $cl_pkg)", '';
   }
   ```

   and keep the existing `(p-set-current-package …)` emission for every
   `$i > 0` section (both forms need it — that is v1's behaviour too).
   The per-section `$a`/`$b` defvars: emit them only for NON-reopen
   sections (they'd be duplicate defvars otherwise — harmless in CL but
   noisy; v1 emits them once per package).
4. Versioned packages: after the section's decls, when
   `$sec->{version}` is set, emit v1's exact two forms (copy the text
   shapes from `_emit_package_version`):

   ```
   (eval-when (:compile-toplevel :load-toplevel :execute)
     (defvar <Pfx>::$VERSION (make-p-box nil)))
   (p-scalar-= <Pfx>::$VERSION <ver>)
   ```

   where `<Pfx>` is `_cl_pkg_designator($pkg)` minus its leading `:`, and
   `<ver>` is numeric bare / otherwise quoted (v1's `$ver_cl` logic —
   just copy it). Put the eval-when+defvar into the section's `decls`
   list and the assignment at the FRONT of its `run` list. Known,
   accepted divergence (v1 has it too, documented in
   `_emit_package_version`'s comment): `$VERSION` is set in source order,
   not at BEGIN time.
5. `_check_my_spanning` / `_check_sub_captures` run per segment already
   and need NO changes: a package-block's `my`s are block-scoped in Perl
   so they can't legally span; leaving them in `%live` only over-fires
   the gate (→ v1), which is safe.

**`package` inside a block (2 census files):** run
`grep -n -B3 -A3 'package ' <file>` on both census hits first. v2 cannot
switch the CL reader's package mid-form (the whole reason for the section
model), and v1 itself uses environment-only tracking for package-in-sub.
Decision rule: if both files use `{ package Foo; … }` at top level inside
a bare block, keep the gate and record the decision; only if one of them
is trivial (e.g. package statement as the last thing in a block) consider
more. Do not sink more than an hour here — 2 files.

**Verification programs** (run each under `PCL_V2=1 ./runpl` and `perl`,
diff output):

```perl
# a) block form + return + method dispatch
package Animal { sub speak { return "generic" } }
package Dog { our @ISA = ('Animal'); sub name { return "rex" } }
print Dog->speak(), " ", Dog->name(), "\n";
print __PACKAGE__, "\n";              # must print main (return section!)

# b) versioned
package Counter 1.5;
print $Counter::VERSION, "\n";        # 1.5
package main;
print "back\n";

# c) reopen: code after the block runs in main again, main's vars intact
our $tag = "M";
package Box { sub hi { return "boxed" } }
sub my_check { print "main-sub $tag\n"; }
my_check();                           # call AFTER definition, same segment
print Box::hi(), "\n";
```

(Do NOT test a sub call that crosses a segment boundary forward —
`f(); package Foo { } sub f {…}` — sections load one after another, so
segment 1's runtime runs before segment 3's definitions exist. v1 has the
same per-section limitation; it is not a v2 regression. If a real
perl-tests file trips on it, that is a shared pre-existing gap — record
it, don't chase it in W1.)

**Acceptance:** the 4 block-form files lower natively; census re-run;
parity sweep exact on all natives; the three programs above byte-match
perl.
**Guards:** transpile-shape test asserting the full preamble appears ONCE
for Foo and the return section uses only `(in-package`; a versioned-pkg
shape test for the `$VERSION` defvar+assign pair; program (a) as an
end-to-end run.

### ~~W2. String eval, stage 1 — replace the text-scan gate with a PPI test~~ — DONE (s272b, this commit)

**Files:** `Pl/Parser2.pm`.
**Anchor:** in `parse()`, `die … if $src =~ /\beval\b(?!\s*\{)/` (near the
top, BEFORE `PPI::Document->new`).

The text scan fires on comments, strings, POD, `eval {` split across
lines, and hash keys. Replace with a walk over the parsed document — which
means **moving the gate to after `PPI::Document->new`**:

```perl
my $evals = $doc->find(sub {
  my $t = $_[1];
  return 0 unless $t->isa('PPI::Token::Word') && $t->content eq 'eval';
  my $prev = $t->sprevious_sibling;
  return 0 if $prev && $prev->isa('PPI::Token::Operator')
           && $prev->content =~ /^(?:->|=>)$/;      # ->eval method / key => eval
  return 0 if $prev && $prev->isa('PPI::Token::Word')
           && $prev->content eq 'sub';               # sub eval (pathological)
  my $next = $t->snext_sibling;
  return 0 if $next && $next->isa('PPI::Structure::Block');   # eval { }
  return 0 if $next && $next->isa('PPI::Token::Operator')
           && $next->content eq '=>';                # eval => value (hash key)
  return 1;                                          # eval EXPR (string eval)
}) || [];
die "Parser2 TODO: string eval\n" if @$evals;
```

Comments/strings/POD never appear as Word tokens, so those false positives
vanish for free. Note `eval;` and `eval $str` correctly still gate (they
ARE string eval). Hash-subscript `$h{eval}` — the Word's siblings inside
the subscript are the braces' ends; `snext_sibling` is undef → it would
gate. Check whether any census file actually does this before adding a
subscript-parent exclusion; don't handle cases that don't occur.

**Measurement:** re-run the census. Record in the commit message how many
of the 65 recover (expect a large fraction — most test files only use
`eval { }`). Then the parity sweep over all new natives. **This is the
highest-exposure step in the plan** — dozens of files newly exercise v2
paths. Budget real time for triage and expect to find more class-1/class-5
bugs (§3). Work through failures one file at a time with the §5.4
cookbook; do not move on with unexplained deltas — and apply the §0.5
escalation rule when a file resists root-causing.

**Acceptance:** census jump; exact parity on every newly-native file.
**Guards:** three parse_code shape tests — a file with only `eval { }`
lowers natively (no die), `eval "str"` still dies with `string eval`, and
`my %h = (eval => 1); $h{eval};`-style mentions don't gate (adjust to
whatever exclusions you shipped).

### ~~W3. String eval, stage 2 — enable `eval EXPR` via the existing capture seam~~ — DONE (s272c, this commit)

**Read first:** `docs/eval-lexical-capture.md`, then
`Pl/ExprToCL.pm sub _eval_lexical_alist` (short — read the whole sub), and
its call sites (grep `p-eval ` in ExprToCL.pm, two spots in gen_funcall).

**Key insight (supersedes older "demotion/rename" sketches in
parser2-prototype.md):** v1's capture mechanism needs NO defvars. At each
`eval EXPR` call site the expression generator emits
`(p-eval STR (list (cons "$x" $x) …))` — an alist of *in-scope lexical
names → their live boxes*, built by reading `$parser->{_let_bound_vars}`.
The eval'd string is transpiled by a subprocess into a `p-eval-thunk`
whose lambda binds those boxes as parameters; reads and writes go through
the shared boxes. v2's true-lexical `let`s holding boxes work as capture
sources as-is. The eval-side transpile always runs v1: pl2cl's
`parse_with_fallback` routes any call with opts (eval_mode) to v1 —
verify that stays true and leave it that way.

So `eval EXPR` can flow through the ordinary expression fallback seam,
PROVIDED two invariants hold at the call site:

1. **Every captured var is boxed.** Already guaranteed: VarAnnotator's
   region-wide `$has_eval` (`$text =~ /\beval\b/` in `analyze()`)
   disqualifies unboxing in any region whose text mentions eval. **Do NOT
   narrow that scan as part of this work item** — it is the safety net
   that makes the rest of this item small.
2. **`_let_bound_vars` is scope-accurate at the call site.** Today it
   never shrinks. A name from an already-closed sibling let would land in
   the alist as a free CL symbol → unbound-variable error at load time.

**Step-by-step:**

1. Add the cumulative set. In `_reg_lex`, alongside the two existing
   writes, add `$self->{_all_lex}{$n} = 1;`. Initialize
   `$self->{_all_lex} = {}` next to `_live_lex`'s init in the per-segment
   loop in `parse()`.
2. Make the fallback set scoped. In `_lower_scope`, snapshot and restore
   `fallback_parser->{_let_bound_vars}` exactly like `_live_lex`:

   ```perl
   my %saved_lb  = %{ $self->fallback_parser->{_let_bound_vars} };
   my %saved_lex = %{ $self->{_live_lex} // {} };
   my @forms = $self->_lower_block($stmts, $vi, $tail_ctx);
   $self->fallback_parser->{_let_bound_vars} = \%saved_lb;
   $self->{_live_lex} = \%saved_lex;
   ```

   Do the same in `_lower_sub` (which already snapshots `_live_lex`).
3. Switch `_forward_global_decls`' exclusion from
   `$self->fallback_parser->{_let_bound_vars}` to `$self->{_all_lex}`.
   Why the asymmetry: forward decls are computed over the WHOLE section's
   text at the end; a name that is let-bound anywhere must never be
   defvar'd, because `defvar` proclaims the symbol special and poisons
   every let of that name (closures would capture the symbol, raw slots
   would break). The eval alist, by contrast, must reflect the *call
   site's* scope — hence scoped. The `p-scalar-=`→`p-my-=` rewrite in
   `_lower_expr` keeps using the (now scoped) `_let_bound_vars`, which
   makes it MORE accurate than before, not less.
4. Only now delete the W2 die. `eval EXPR` statements then lower through
   `_lower_stmt` → `_lower_expr` → fallback → v1's gen_funcall →
   `_eval_lexical_alist` with an accurate set.
5. Check the assembly interaction: `_forward_global_decls`' `$lb` variable
   is read once per section — make sure you switched the right lookup and
   didn't leave a second read.

**Order of testing for this item:**

```bash
PCL_V2=1 prove Pl/t/eval-capture-01.t    # 30 tests; must equal v1's result
prove Pl/t/eval-capture-01.t             # (v1 for comparison)
```

Then the verification programs:

```perl
# a) read + write-back through eval
my $x = 1; my @a = (1,2,3);
eval '$x = $x + 10; push @a, 9;';
print "$x @a\n";                          # 11 1 2 3 9

# b) closed sibling scope must NOT be captured (would be unbound)
{ my $dead = 5; print "$dead\n"; }
my $y = 2;
print eval '$y + 1', "\n";                # 3 — and the generated (p-eval …
                                          # alist must NOT mention $dead

# c) eval inside a sub captures params and body lexicals
sub f { my ($p) = @_; my $q = 10; return eval '$p + $q'; }
print f(5), "\n";                         # 15
```

For (b), also transpile with `PCL_V2=1 perl -I. pl2cl --no-cache` and
grep the output: the `(p-eval … (list …))` alist must contain `$y` and
not `$dead`.

**Known divergences to preserve** (do not try to fix): context/wantarray
propagation into string eval stays deferred (`docs/not-supported.md`
"Context propagation into string eval"); the three documented divergences
at the bottom of `docs/eval-lexical-capture.md`.

**Expected aftermath:** the census will jump again, and eval-heavy files
(eval.t, caller.t, …) will lower but keep failing tests they already fail
under v1 (string-eval scope corners, caller() gaps). Parity with v1 is
the bar — absolute pass counts are NOT this work item's problem.

**Acceptance:** eval-capture-01.t identical under both pipelines; programs
a–c byte-match perl; census jump; exact parity on newly-native files.
**Guards:** shape test for (b) (alist excludes closed-scope names);
end-to-end run of (a); a test that `eval EXPR` no longer dies.

### ~~W4. Prototype/signature subs (2 files)~~ — DONE (s272d, this commit)

**Files:** `Pl/Parser2.pm`.
**Anchors:** the pre-pass die (`"sub with prototype/signature"`); the
top-level sub branch in `parse()`'s per-segment loop (search
`p-declare-sub`); `_hoist_nested_sub`.
**v1 model:** `Pl/Parser.pm sub _process_sub_statement` — specifically the
`parse_prototype_or_signature($prototype, $stmt)` call and the
`add_prototype($name, $sig_info)` + `add_declared_sub` pair right after
(grep `parse_prototype_or_signature` for both definition and call).

**Step-by-step:**

1. In the pre-pass loop in `parse()`, replace the die with registration:

   ```perl
   if (my $proto = $sub->prototype) {
     my $sig_info = $self->fallback_parser->parse_prototype_or_signature($proto, $sub);
     $self->environment->add_prototype($sub->name, $sig_info);
     $self->environment->add_declared_sub($sub->name, $seg->{pkg});
     next;   # NOT in sub_info: call sites must go through the fallback
   }
   ```

   The `next` matters: ExprToCL2's native direct-call path (driven by
   `sub_info`) knows nothing about imposed contexts (`($)` imposes scalar
   context on the arg — memory: commit 74de22e) or block-form parsing
   (`(&@)`); excluding prototyped subs keeps their call sites on the
   fallback path where all of that already works.
2. Route the DEFINITION through the statement seam. In the per-segment
   top-level loop:

   ```perl
   if ($child->isa('PPI::Statement::Sub') && $child->name
       && !$child->isa('PPI::Statement::Scheduled')) {
     if ($child->prototype) {
       $self->_fallback_stmt($child);   # v1 emits p-sub into defs bucket →
       next;                            # hoisted via _captured_decls
     }
     …existing _lower_sub path…
   }
   ```

   and the same two lines at the top of `_hoist_nested_sub` (before the
   capture scan — v1's machinery handles its own captures via defvars? No:
   a PROTOTYPED nested sub that captures a live lexical still breaks the
   same way, so run the `_live_lex` capture scan FIRST, then the
   prototype fallback).
3. `_fallback_stmt` returns a raw for runtime lines — for a sub definition
   v1 puts everything in the definitions bucket, so expect `()`. If a
   runtime raw does come back (some signature shapes emit runtime
   arity-check helpers?), keep it in place — i.e. call it where the
   statement sits rather than discarding.

**Verification programs:**

```perl
# a) old-style prototype imposing scalar context
sub takes_scalar ($) { my ($n) = @_; return $n; }
my @list = (7, 8, 9);
print takes_scalar(@list), "\n";     # 3 (count, not 7) — scalar ctx imposed

# b) signature with default
use feature 'signatures'; no warnings 'experimental::signatures';
sub greet ($name, $greeting = "hi") { return "$greeting $name"; }
print greet("bob"), "\n";            # hi bob
print greet("ann", "yo"), "\n";      # yo ann
```

**Acceptance:** the 2 gated files lower or reveal their next gate; both
programs byte-match perl; parity holds.
**Guards:** shape test — `sub f ($) {…} f(@a);` call site wraps the arg in
scalar-context shape identical to v1's output for the same input (generate
v1's text in the test and compare the relevant fragment, or pin the
`(p-scalar` wrap); a runtime test of program (a).

### ~~W5. File lexicals captured by named subs (4 files, incl. qq.t)~~ — DONE (s272e)

**Shipped s272e.** `_rename_captured_file_lexicals` (per-segment, before the
pre-pass) rewrites a single-scalar captured file lexical to a fresh
package-level `$x__file__N` cell, lowered as a defvar'd box (the `our` shape).
Subset: exactly one `my $x` scalar decl, no other decl of the bare name, no
array/hash-family use (`@x`/`%x`/`$#x`/`$x[…]`/`$x{…}` via PPI `->symbol`), no
`${x}` deref-block, no INTERPOLATED use (string/regex/heredoc — `_interp_names`);
anything else keeps the gate. Fixed two pre-existing v2 bugs surfaced by
un-gating: `_let_bound_vars` leaking across package segments (reset per segment),
and a named sub nested in a signatured sub (gated → v1). 67 files native (was 61)
at exact v1 parity; guards `Pl/t/parser2-01.t` = 121. Original plan below.


The `{ my $x = 0; sub f { $x++ } }` static-variable idiom and qq.t's
`my $test = 1; sub is { $test++ }`. Named subs hoist to the definitions
bucket OUTSIDE the lets, so the capture needs a shared cell both can see.

**Files:** `Pl/Parser2.pm`.
**Anchors:** `_check_sub_captures` (top-level gate) and
`_hoist_nested_sub` (nested gate).

**Design (single-declaration case only — gate the rest):** rename the
captured lexical to a fresh package-level name, defvar it, and rewrite
every reference in the segment at the PPI-token level so both native and
fallback paths see the new name with zero further plumbing.

**Step-by-step:**

1. Write a detector `_captured_file_lexicals($seg_stmts)` returning
   `{ bare_name => sigil_variants_seen }` for names that (a) are declared
   by a `my` in the segment and (b) appear in some named sub's block
   content (reuse the scan shapes in `_check_sub_captures`). Run it at the
   TOP of the per-segment loop in `parse()`, BEFORE the pre-pass reads any
   sub content (`_sub_ctx_insensitive` caches text impressions — rewrite
   first, then analyze).
2. Precondition per name: exactly ONE `my` declaration of that name in the
   whole segment (count with a PPI walk over `Statement::Variable`s like
   VarAnnotator step 1 does — a text-count is too sloppy here). If a name
   fails the precondition, keep today's die → v1. Shadowing needs v1's
   `__lex__N` machinery; out of scope.
3. For each qualifying bare name `test`: allocate `test__file__N`
   (`$self->{_file_lex_counter}++`), then walk every token in the segment
   (`$stmt->find('PPI::Token::Symbol') || []` per statement) and for each
   Symbol whose content is `$test`/`@test`/`%test` (sigil-preserving),
   `$tok->set_content($sigil . 'test__file__' . $N)`. Also match
   `PPI::Token::ArrayIndex` (`$#test`) if — and only if — the detector saw
   that form. Cast/deref forms (`${test}`) are rare; if the detector text
   sees `{test}` usages, keep the die instead of chasing them.
4. Rewrite the DECLARATION statement: the `my $test__file__N = INIT;`
   token stream still starts with `my` — intercept it in the
   `_lower_block` Variable branch: maintain
   `$self->{_file_lex_renamed}{'$test__file__N'} = 1`, and when
   `_single_scalar_decl`/`_multi_decl` yields a renamed name, emit —
   instead of a `let` — a defvar in `_captured_decls` plus a plain
   assignment, exactly the `_lower_our_decl` shape (read that sub; it is
   the model: defvar hoisted, `p-scalar-=`/container assignment inline,
   NO let, NOT registered in `_let_bound_vars`).
5. VarAnnotator: nothing to do — after the rename there is still exactly
   one `my`, but step 4 removed it from the let path entirely; the name is
   a package var, boxed by construction. Verify no `(let (($test__file__`
   appears in output.
6. Delete/relax the corresponding dies: in `_check_sub_captures` and
   `_hoist_nested_sub`, a name that was renamed this way is no longer in
   `_live_lex`/the top-level my set, so the gates simply stop firing for
   it. Names failing the precondition still die. Make sure the ERROR
   MESSAGE for the still-gated path stays distinct (it feeds the census).

**Verification programs:**

```perl
# a) qq.t's shape
my $test = 1;
sub is_ok { print "ok $test\n"; $test = $test + 1; }
is_ok(); is_ok();                    # ok 1 / ok 2

# b) static-variable idiom in a block
{ my $count = 0; sub bump { $count = $count + 1; return $count; } }
print bump(), bump(), bump(), "\n";  # 123

# c) privacy: the renamed var is not the same as a same-named later my
#    (must still DIE → v1 because of the two-declaration precondition)
```

**Acceptance:** qq.t + the other capture-gated files lower (check census
messages disappear); parity exact; programs a/b byte-match perl.
**Guards:** end-to-end run of (b); shape test that `$test` got renamed to
`$test__file__\d+` with a defvar and no let; a two-`my` shadowing case
still dies to v1.

### ~~W6. Small gates (optional, do only if cheap)~~ — DONE (s272f)

**Shipped s272f.** while/until/foreach `continue` → native `:continue (progn …)`
key (`_continue_keys`, after the body); C-for+continue and bare-block continue
stay gated (the latter is loopctl.t's sole blocker, deliberately not chased).
`my $scalar <non-'=' trailing>;` (`my $aa,$bb,$cc;` / `my $a . $foo;`) → boxed
`my $scalar` let + discarded void trailing expr. concat.t + or.t net-new native;
69 files native at v1 parity. Guards → new `Pl/t/parser2-02.t` (10). Original
plan below.


- **continue blocks (1 file):** native lowering is genuinely small: the
  loop macros accept `:continue FORM` (see `parse-loop-keys` in
  `cl/pcl-runtime.lisp` — `:label` must come FIRST in body-and-keys;
  `:continue` is found by position anywhere after). First check what v1
  emits for `while … continue` and `foreach … continue` (`p-while` takes
  `:continue (progn …)`; `p-for` IGNORES continue — so gate
  `for(;;) … continue`, which is invalid Perl anyway). Then in v2's
  while/foreach branches, instead of the gate: detect the `continue` Word
  + following Block in `@k`, and append
  `':continue', ['progn', $self->_lower_scope([$cont_block->schildren], $vi)]`
  after the label keys. Bare-block continue keeps its gate (v1 runs it
  after the tagbody — different shape, 1 census file doesn't justify it).
  Mind PPI's sibling-split quirk: for BARE blocks `{ } continue { }`
  arrives as TWO sibling statements — that path must keep dying (it
  currently dies via the `continue` compound).
- **`my $aa, $bb, $cc;` (1 file):** Perl declares only `$aa`; the rest are
  comma-op reads of package vars (it warns "Parenthesize"). Read v1's
  `_process_variable_statement` for this shape first and copy its
  behaviour. If v1 effectively ignores the trailing names at declaration
  time, the v2 route is: lower as `my $aa;` and let later uses of
  `$bb`/`$cc` become forward-declared globals. Write the 3-line probe
  under real perl first (`perl -Mstrict -e 'my $aa, $bb;'` errors under
  strict! — check what the census file actually runs with) before
  choosing.
- **Standalone labels / computed goto:** keep gated. Intra-sub goto is
  v1-partial (memory: project_intra_sub_goto); do not open it here.

### ~~W7. Tier B1 — full-sweep parity~~ — DONE (s272g): CLEAN

Full 108-file v1-vs-v2 sweep matched exactly bar the two documented deltas (chop
skip-registry; sprintf v2-better) and int/assignwarn parallel-load flakiness
(identical isolated on both pipelines). No v2-native regressions. Original steps
below.

### W7. Tier B1 — full-sweep parity

```bash
perl sweep-perl-tests.pl --jobs 8            # fresh v1 baseline (~20 min)
cp -r .faillog /tmp/faillog-v1-full
PCL_V2=1 perl sweep-perl-tests.pl --jobs 8   # v2 (~20 min)
cp -r .faillog /tmp/faillog-v2-full
tools/sweep-diff.pl                          # per-test diffs (read its --help)
```

Procedure:

1. Re-measure the v1 baseline the same day. Memory records 18089 pass /
   62 fully passing WITH known open regressions (see MEMORY.md "fully-
   passing sweep regression hunt" — parked, not yours to fix here). Do
   not compare against stale numbers, and do not chase pre-existing v1
   failures.
2. Build a per-file comparison table: for each file, (v1 pass, v1 fail) vs
   (v2 pass, v2 fail). A one-line Perl join over the two `_status.tsv`
   snapshots does it (`.faillog/_status.tsv`; col6 localizes crash points
   — memory: project_partial_stop_analysis).
3. Every per-file delta gets exactly one of:
   - **(a) v2 bug** → minimal repro (§5.4) → fix → re-sweep that file.
   - **(b) v2 legitimately better** → PROVE it: diff generated CL, run the
     divergent test's construct against real perl, write the explanation
     into `docs/parser2-prototype.md`. (Expect nearly zero of these.)
   - **(c) flaky** → re-run the file twice under both pipelines; if it
     moves, log it against project_sweep_flakiness_investigation and
     exclude from the comparison.
4. Iterate to zero unexplained deltas. Budget: this is the longest item;
   several find-fix-resweep cycles at ~20 min a sweep. Run file-subset
   sweeps while iterating; full sweeps only to confirm. This item is
   squarely in §0.5 territory — escalate stubborn files rather than
   classifying them as (b) or (c) without proof.

### ~~W8. Tier B2 — the Pl/t gate under v2~~ — DONE code-wise (s272g–s273); re-sweep = first task below

**~23 of 114 Pl/t files failed under `PCL_V2=1`** at the start.  Critical
realization: full perl-tests parity does NOT mean v2 is correct — those files
gate to v1 for other reasons, MASKING native-lowering gaps that the smaller
Pl/t snippets (v2-native) expose.  Pl/t under v2 is the stricter gate.

- s272g: BEGIN/END ordering (`_sched_defs` bucket) + BEGIN-refs-file-my gate;
  native bare-tail-if return; foreach-aliasable-lvalue gate.
- s272h–i (Opus): the D1–D22 batch — see `docs/v2-w8-session-decisions.md`.
- s273 (review pass): D20 REVERTED (it was wrong vs real perl — read D23, it
  is a case study in "the test can be the bug"), and the last 5 failing files
  fixed: bop-01 (D24 bitwise `&=` boxing), begin-end-01 (D23 revert),
  misc-fixes-02 (D25 paren-less `\substr`), fileio-02 (D26 `open($h,…)`
  boxing), closure-01 (D27 `my $i = $i` self-ref init via `p-box-init`).
  Plus D28: the seam my-shadow GATE (`map { my $x … }`/`do { my $x … }` over a
  live outer `$x` silently corrupted the outer lexical — gate → v1; reclaim in
  W8.5). Guards added to parser2-02.t (7 shape + 1 runtime).

**Definition of done for W8 (verify before starting W9):**
1. `PCL_V2=1 prove -j8 Pl/t/` — zero failing files.
2. `prove -j8 Pl/t/` (v1) — still 100%.
3. Census + v2-native parity sweep vs same-day v1 baseline — EXACT (the D28
   gate may have knocked some files off native; that is a coverage loss, not a
   parity loss — record the new census number).
   s273 left this running; if the numbers in `docs/parser2-prototype.md` are
   already updated with a s273 sweep block, trust them; otherwise re-run (§2).

### W8. Tier B2 — the Pl/t gate under v2

```bash
PCL_V2=1 prove -j8 Pl/t/     # ~6 min; must match v1's file/test counts
```

- Some `Pl/t/*.t` assert on v1's exact output text. Classify each failure:
  - Missing v2 behaviour → fix v2.
  - Shape-only assertion (the CL text differs but is semantically the
    v2-native equivalent) → make the assertion pipeline-aware:

    ```perl
    if ($ENV{PCL_V2}) { like($cl, qr/<v2 shape>/, $desc) }
    else              { like($cl, qr/<v1 shape>/, $desc) }
    ```

    NEVER weaken what the test verifies (CLAUDE.md §5) — the v2 branch
    must pin the v2 shape just as tightly.
  - Tests that spawn pl2cl themselves inherit `PCL_V2` from the prove
    environment — that is intended; don't strip it.
- `Pl/t/parser2-01.t` calls `Pl::Parser2` directly and is env-independent.

### ~~W8.5 — reclaim the D28-gated files~~ — DONE (s273, decisions D29+D30)

**Shipped s273.** Option 1 below was implemented as `_rename_decl_within` +
`_shadow_rename_blocker` (shared machinery), used by BOTH:
- `_gate_seam_my_shadow` — renames the fallback-block shadow to
  `$x__shadow__N`; blockers (interpolation, re-shadow, `${x}`, string eval,
  state, non-scalar) still die → v1. do.t/vec.t reclaimed; yadayada.t stays
  gated (interpolated `"($err)"`). The map probe is perl-correct under
  v2-native (`2 4 6 outer`) — deliberately BETTER than v1's defvar-shadow
  `2 4 6 6`; proof recorded in D29.
- `_rename_poisoned_cond_mys` — segment pre-pass renaming condition-my names
  to `$name__cond__N` ONLY when the name is also used outside the construct
  (the defins.t unbound-global crash, D30). Self-contained loops keep their
  names (zero churn, interpolation untouched).

**Still open in this family (for a later session):**
1. yadayada.t-class reclaim needs renaming INSIDE interpolating tokens
   (guarded `s/\$\Qname\E\b/…/` on Quote::Double etc.; gate on any
   backslash-adjacent or `${name}` occurrence). Only do it if the census
   shows real files blocked on the interpolation blocker.
2. The single-counter C-for carve-out has the same poison conflict
   (`for (my $i…)` + global `$i` elsewhere → unbound). Renaming there must
   carry the VarAnnotator vi verdict across the rename or the intloop
   unboxing optimization is lost. Cleanest under W12's structural events.
3. v1-side bugs found and deliberately NOT fixed (parity behaviour): the raw
   `(setf (p-box-value …))` our-init stale-sv-cache read after a BEGIN write,
   and the defvar-shadow map corruption (`docs/closure-lexical-scoping.md`).
   Both are reasons v2 exists.

Original design notes below (kept for reference):

The D28 gate sends a whole file to v1 whenever a fallback block
re-declares a live lexical (`my $x = …; my @r = map { my $x = … } …`). Two
ways to reclaim, in order of preference:

1. **Rename at the PPI level, before lowering (recommended — reuses W5's
   machinery).** In `_gate_seam_my_shadow`, instead of dying: rewrite the
   inner declaration and every reference to it WITHIN that block to a fresh
   name (`$x__shadow__N`) via `set_content`, exactly the W5 rename pattern
   (sigil-preserving, skip if the block also contains `${x}`-style
   brace-derefs or string interpolation of the name — then keep the die; read
   `_rename_captured_file_lexicals` first and factor out its token-walk
   helper rather than copying it — CLAUDE.md §11). After the rename there is
   no collision: the seam sees a NEW name, registers it let-bound inside its
   own scope handling, and the outer lexical is untouched. Subtlety: the
   rename must NOT leak outside the block (a shadow is block-scoped by
   definition, so rewriting only tokens inside the Block subtree is exactly
   right). Interpolation: `"$x"` inside the block refers to the shadow —
   `_interp_names` decides whether to rename-in-string or keep the gate; keep
   the gate for interpolated cases in round 1.
2. **Lower map/grep/sort blocks natively** (bigger; overlaps W11's "consider
   keys/values/each native" note). Not worth it for this alone.

**Also fold in here (same family, found s273):** the do-block/anon-sub cases
(t9/t11 in the s273 scratch probes). And note two v1-side bugs found during
the D20 investigation that this workstream must NOT try to fix (they are v1
parity behaviour, record only): (a) v1's raw `(setf (p-box-value …))` our-init
reads back stale sv caches after a BEGIN wrote the box; (b) v1's
defvar-based lexicals make a map-block shadow write through the file-level
`my` (`docs/closure-lexical-scoping.md`). Both are reasons v2 exists; both
are pinned pipeline-aware where tests cover them.

**Acceptance:** the census count recovers to ≥ the pre-D28 number with the
gate replaced by renames; a guard in parser2-02.t runs the map probe
end-to-end and byte-matches **perl** (`2 4 6 outer`) — note: matching perl
here means v2-native output BEATS v1; per §2 rule 1 that requires the written
proof, which is this section.

### W9. Tier B3 — flip the default

1. **Cache keying FIRST** (hard prerequisite). The module cache
   (`~/.pcl-cache`) is keyed by `sxhash` of the source file's absolute
   path only — see `cl/pcl-runtime.lisp sub p-compute-cache-path`. Flip
   the default without fixing this and every previously-cached v1 module
   transpile gets reused as if it were v2 output. Fix:

   ```lisp
   (defparameter *pcl-cache-generation* "v2-1"
     "Mixed into cache paths; bump on any codegen change that invalidates
      cached module transpiles (pipeline flips, major emission changes).")
   ;; in p-compute-cache-path:
   (let* ((abs-path (namestring (truename source-path)))
          (hash (sxhash (concatenate 'string abs-path "|" *pcl-cache-generation*)))
          …)
   ```

   Also mix in the pipeline OVERRIDE when the escape hatch is set
   (`(sb-ext:posix-getenv "PCL_V1")` → append `"|v1"`), so a user flipping
   back and forth doesn't cross-contaminate. NOTE this hazard already exists
   TODAY in the opposite direction: a `PCL_V2=1` run with caching enabled
   shares `~/.pcl-cache` with v1 runs (only `--no-cache` invocations are
   safe). So implement this step as the very FIRST thing in W9 — mix in the
   *effective pipeline* (v1/v2), not just the escape hatch — and `rm -rf
   ~/.pcl-cache/*` once after deploying it. Paren-check the file after
   editing (§0.2 step 2). Verify: transpile a module-using file, flip the
   env var, re-run — it must RE-transpile (watch timing or add
   `PCL_V2_VERBOSE`).
2. In `pl2cl`: default `$PARSER_CLASS` to `Pl::Parser2`; `PCL_V1=1`
   selects v1 (the escape hatch); keep accepting `PCL_V2` as a no-op so
   existing scripts don't break. Update `parse_with_fallback`'s guard
   condition accordingly (it keys off `$PARSER_CLASS`, so it mostly just
   works — re-read it).
3. Re-run W7 + W8 once with a CLEAN environment (no PCL_V2 anywhere) and
   once with `PCL_V1=1` (must reproduce the v1 baseline).
4. Perf check — v2 exists for speed. Recreate the benches from
   `docs/parser2-prototype.md` (fib(29) recursive, fib loop, intmath 2M
   iterations) as small .pl files in the scratchpad; time
   `./runpl bench.pl` under both pipelines and `perl bench.pl`; subtract a
   null-program baseline run. Expected ballpark from s268: v2 fib(29)
   ≈0.08 s vs perl 0.138 s. Record the fresh numbers in
   `docs/parser2-prototype.md`. A regression here blocks the flip.
5. Update: CLAUDE.md (Quick Reference + status line), `docs/
   parser2-prototype.md` header ("opt-in via PCL_V2" → default), memory
   (project_parser2_prototype + MEMORY.md), and any runner comments
   mentioning PCL_V2 (grep the repo).

### W10. Tier B4 — fix the v1 my-across-package bug properly

The open v1 bug (found s270): `my $g; package Foo; print $g;` — v1 defvars
`$g` under `:main` but the Foo section reads it as `Foo::$g` → unbound
crash. v2 currently gates this (`_check_my_spanning`).

Reuse W5's machinery with one addition: when the (single-declaration)
lexical spans a package boundary, rewrite references in LATER segments to
the *package-qualified Perl form* `$main::g__file__N` (the fallback
machinery already compiles qualified vars; `main` handling — check
`_forward_global_decls`' `%skip_pkg` and the `$pkg eq 'main'` special case
so the qualified defvar isn't skipped, and make sure `main` lands in the
pre-declared package set if the reader needs it). The defvar stays in the
DECLARING segment's section. Same preconditions as W5 (single
declaration; no `{name}` brace-deref usages); shadowing spans stay gated.

**Acceptance:** the s270 repro
(`my $g = 5; package Foo; print $g, "\n";` → `5`) works under v2 and is
pinned by a guard test; `_check_my_spanning`'s die only fires for the
still-gated shapes; parity holds.

### W11. Tier C1 — native hash/array element access (measure first)

**Do not start until W1–W6 are done and W7 parity holds.**

> **Perf priority note (s273):** W11 and W14 are the two items that make v2
> visibly faster on real code, per the s272d bench table in
> `docs/parser2-prototype.md`: arith already dominates (6.8–10.9×), but
> arrhash is only 1.7× (→ W11) and `my $x = shift`-style subs only 1.04×
> (→ W14). Real CPAN/OO code is exactly hash-element + shift-heavy, so these
> two unlock the headline win. W12 is a *correctness/maintainability*
> rewrite (it retires the text-scan disqualifiers) — do it after the perf
> pair unless disqualifier over-fire is measurably costing coverage.

1. Write the bench first:

   ```perl
   my %h; my @a; my $s = 0;
   for (my $i = 0; $i < 500000; $i = $i + 1) {
     $h{x} = $i; $a[3] = $i + 1;
     $s = $s + $h{x} + $a[3];
   }
   print "$s\n";
   ```

   Time perl vs v1 vs v2-today (`./runpl` both pipelines). Only proceed
   if the v2 gap is real (expect yes: every element access round-trips
   the expression fallback and its `(let ((*wantarray* nil)) …)` wraps).
2. ExprToCL2: add native READ lowering for `$h{k}` / `$a[$i]` where the
   base is a known let-bound container (check
   `fallback_parser->{_let_bound_vars}`) and the subscript is native →
   `(p-gethash %h KEY)` / `(p-aref @a IDX)`. These return boxes; that is
   fine as OPERANDS of the R1 ops (they coerce via `unbox` fast paths),
   but a bare `my $x = $h{k}` must stay boxed — class-5 rule: never store
   a box in a raw slot. The existing `_arith_rhs` "operator coerces"
   model already handles this: element reads count as `others` (like sub
   calls), so `my $x = $h{k} + 1` may unbox, `my $x = $h{k}` may not.
3. WRITE position (`$h{k} = EXPR`, `$a[$i] = EXPR`): copy the exact
   p-setf shapes v1 emits (transpile the bench under v1 and read them) —
   tied hashes and autovivification live behind those entry points; do
   not invent new forms.
4. VarAnnotator: no new scalar gates for reads; container names are never
   unboxed anyway. But re-check class-5 for any new WRITE form you make
   native.
5. Re-measure the bench; record before/after in
   `docs/parser2-prototype.md`. Then consider `keys`/`values`/`each`
   native iteration only if a bench says it matters.

### W12. Tier C2 — OpcodeTree-walk VarAnnotator

Replace the text-scan gates with facts from the PExpr OpcodeTree that
ExprToCL2 already builds per expression (design context:
`docs/type-flow-and-codegen-plan.md` §(s)).

**The shape checklist is maintained in two places — W12 must reproduce every
entry structurally:** the disqualifier list in `Pl/VarAnnotator.pm`'s header
(s272h/s273 entries) and the boxing decisions in
`docs/v2-w8-session-decisions.md` (D2, D11, D12, D15, D24, D25, D26 — plus
the pre-existing step-3 regexes). Each regex = one "write/alias event" the
tree walk must emit: lvalue-assign, list-assign target, conditional init,
magic-ref target (`\substr`/`\vec`/`\pos`, paren-less included), bitwise and
string-bitwise compound assigns, handle-vivifying builtin args, mutating
builtin args, `local`, `pos`, foreach alias, `=~`, `++/--`, `\$x`. The
per-name gates D27/D28 (self-ref init, seam shadow) are SCOPING facts, not
boxing facts — the tree walk should also expose "declaration whose init
references the declared name" and "my nested inside a block that lowers via
the seam" so those two stop being text scans as well.

Sketch: per statement, after
`parse_expr_to_tree`, walk the tree collecting per-name (read, write,
ref-taken, magic-target) events; aggregate per block; keep the same
conservative rules keyed on AST facts instead of regexes. The win is
un-gating names the text scan falsely disqualifies (`"$x"` inside string
literals, shadowing, `++` outside the C-for carve-out, `eval` in a
comment). Bring-up protocol: run BOTH annotators over the full census
corpus and diff their verdicts; any name AST-unboxable but text-boxed
needs a written justification before switching; any name text-unboxable
but AST-boxed is fine (more conservative). Only then swap the default and
delete the text scan.

### W13. Tier C3 — lean p-sub round 2

Only with fresh measurements (`docs/parser2-prototype.md` "Lean p-sub"
table): remaining per-call residue was ~18 ns catch/throw for `return` +
~15 ns for the five special binds. The catch elision needs a "no closure
in the body can re-throw :p-return after the frame exits" analysis; the
bind elision needs a whole-program "nobody calls caller()" bit (note
caller() reads the whole chain — it is NOT a per-sub decision). If the
numbers haven't changed, expected win is small; prefer W11/W12.

### W14. Tier C4 — `my $x = shift` → `my ($x,…) = @_` normalization (deferred)

**Do this only after the coverage tiers (A/B) are done and the pipeline is the
default.** Measured motivation (session 272d benchmarks, both pipelines,
startup subtracted): a sub written `my $n = shift; …` gets only **~1.0×** over
v1 because bare `shift` forces the `p-args-body`/`@_`-flatten prologue, while
the identical sub written `my ($n) = @_; …` gets **~5.4×** via the
`(&optional ($n (p-undef)) …)` lambda list. Same computation, 5× apart. The
`my $x = shift` / `my $self = shift` idiom is ubiquitous (esp. OO code), so
this unlocks the fast call path for a large fraction of call-heavy programs.

**Approach — a parse-time normalization into the EXISTING fast path** (CLAUDE.md
§11), NOT a new codegen path: in `_lower_sub_inner`, detect a *contiguous
leading run* of `my $SCALAR = shift;` statements and coalesce the WHOLE run into
one `my ($a,$b,…) = @_;`, after which the current `_extract_params` lambda-list
optimization takes over unchanged. The run length is arbitrary — a single
`my $n = shift;` and a multi-arg `my $self = shift; my $x = shift; my $y = shift;`
are the same transform, just runs of length 1 and 3:

```perl
sub f { my $x = shift; my $z = shift; return $x + $z; }   # →
sub f { my ($x, $z) = @_;              return $x + $z; }   # (&optional ($x …) ($z …) …)
```

The order is preserved (the Nth bare `shift` binds `@_[N-1]`, exactly the Nth
slot of the list assignment), so the coalesce is value-identical whenever the
guard below holds. It is safe precisely because the guard forbids the remainder
from observing `@_` — the one thing the run of `shift`s mutated that the single
`my (…) = @_` does not.

**Guard (all must hold — conservative):**
1. Each rewritten statement is exactly `my $scalar = shift;` with a **bare**
   `shift` — never `shift @arr`, `shift(@x)`, or `my $x = shift // $default`
   (those are not a plain first-element bind).
2. Only a contiguous **leading** run — collect `my $x = shift;` statements from
   the top of the body and STOP at the first statement that is not one (any
   number qualify, so `my $x=shift; my $z=shift;` → `my ($x,$z)=@_;`). An
   *interleaved* non-shift statement (`my $x=shift; my $t=1; my $z=shift;`)
   ends the run at `$t` — the trailing `my $z=shift;` is NOT folded in for now
   (it could be, since `$t` doesn't consume `@_`, but that needs care and is a
   later refinement; keeping only the leading run is the conservative version).
3. The **remainder must not use `@_`** — reuse the existing `$body_uses_args`
   scan (`/\@_|\$_\[|\bshift\b|\bgoto\b|\bwantarray\b/`) on the post-run body.
4. **The remainder must not contain string `eval`.** This is the subtle,
   shift-specific condition: bare `shift` MUTATES `@_` (drops the first
   element), whereas `my ($x)=@_` leaves `@_` intact, so the rewrite changes
   what `@_` holds for the rest of the body. The text scan (3) catches a
   literal `@_` in the remainder but cannot see inside `eval $code`, so any
   string eval in the remainder must disqualify the rewrite. (The existing
   `my ($n)=@_` optimization needs NO eval condition — it never mutates `@_`,
   so an eval reading `@_` sees the same value either way. The eval disqualifier
   is unique to the shift rewrite, precisely because of the mutation.)

**Guards to add:** `Pl/t/parser2-01.t` — single leading `my $x = shift` (clean
body) lowers to `(&optional ($x (p-undef)) …)`; a MULTI run `my $x=shift;
my $z=shift;` lowers to `(&optional ($x (p-undef)) ($z (p-undef)) …)` and runs
end-to-end matching perl for `f(1,2)`; a body that still reads `@_`/`$_[N]`
stays on `(&rest %_args) (p-args-body …)`; a body with `eval '…'` stays on the
`p-args-body` path; an interleaved `my $x=shift; my $t=1; my $z=shift;` folds
only `$x` (or, in the conservative version, keeps v1). Then the full parity
sweep before committing. Re-measure the
`shift`-fib bench — it should move from ~1.0× to ~5×.

---

## 5. Reference

### 5.1 Function map — `Pl/Parser2.pm` (as of 0a645e0)

| sub | role |
|---|---|
| `parse` | segment split → pre-pass (sub registration) → per-segment lowering → section assembly. THE place file-level order lives. |
| `_set_cur_package` | resets Environment package stack to a segment's pkg |
| `_check_my_spanning` | gate: my-lexical crossing a package boundary → v1 |
| `_collect_lexical_names` | helper for the spanning check |
| `_check_sub_captures` | gate: top-level my captured by top-level named sub |
| `_forward_global_decls` | v2's forward-declaration pass (defvar referenced-never-let-bound names + cross-pkg refs; NEVER let-bound names) |
| `_lower_sub` / `_lower_sub_inner` | p-sub emission; in_subroutine bump; `_live_lex` snapshot; lambda-list optimization for `my (LIST) = @_` |
| `_reg_lex` | register a `my` name (fallback set + live set) |
| `_lower_scope` | block-scope wrapper: `_live_lex` save/restore (W3 adds `_let_bound_vars`) |
| `_hoist_nested_sub` | named sub in a block → defs bucket; capture gate via `_live_lex` |
| `_extract_params` | `my ($a,$b) = @_;` recognition for real lambda lists |
| `_lower_block` | THE statement dispatcher; `my` nests rest-of-block in a `let`; local/include/sub branches |
| `_lower_stmt` | non-declaration statements: compounds, break, modifiers, native assignments |
| `_lower_compound` | if/unless/elsif, while/until, C-for, foreach, bare blocks, labels |
| `_label_keys` | `:label NAME` pair for loop macros (must be FIRST in body-and-keys) |
| `_lower_bare_block` | loop-once tagbody shapes (unlabeled + labeled) |
| `_lower_expr` | native-or-fallback expression seam; `p-scalar-=`→`p-my-=` raw rewrite |
| `_auto_defined_raw` | v1's `_auto_defined_cond` applied to raw loop conds |
| `_sub_ctx_insensitive` / `_expr_scalar_rooted` | context-sensitivity analysis for the R2 caller optimization |
| `_split_modifier` / `_modifier_needs_fallback` / `_apply_modifier` | statement-modifier handling |
| `_lower_our_decl` | `our` → hoisted defvar + plain assignment (MODEL for W5's renamed lexicals) |
| `_multi_decl` / `_single_scalar_decl` | declaration shape recognizers |
| `_fresh_container` | sigil → empty box/vector/hash-table form |
| `_fallback_stmt` / `_fallback_stmt_capture` | statement seam (scratch v1 section; decls → `_captured_decls`; `_local_let_depth` surplus reporting) |
| `_is_local_stmt` / `_lower_local` | `local`/`delete local` → raw_wrap around block remainder |
| `_cond_parts`, `_strip_semi`, `_pure_incr_step` | small PPI helpers |

### 5.2 Seams and invariants

- **Three fallback levels:** expression (`_lower_expr` → raw leaf),
  statement (`_fallback_stmt` → scratch-section run of v1's
  `_process_element`; decl buckets hoist to `_captured_decls`; a
  `_local_let_depth` surplus is only legal via `_lower_local`'s raw_wrap),
  whole file (`die "Parser2 TODO: …"` → pl2cl retries v1).
- **CLForm is the only printer.** Forms: string atom, `[head, @args]`,
  `['list', @elems]`, `raw($text)` (balanced), `raw_wrap($open, $n, @body)`
  (open text with exactly $n unclosed parens, from v1's counter). Never
  string-rewrite generated CL outside the two sanctioned seam rewrites
  (`p-scalar-=`→`p-my-=` for let-bound names; `_auto_defined_cond` on raw
  loop conditions).
- **Section assembly order** (per section, in `parse()`): package preamble
  (non-first, non-reopen) → decls → per-pkg `$a`/`$b` defvars → forward
  global decls → captured decls → defs → `p-set-current-package`
  (non-first) → runtime. All later-section packages and
  Environment-registered undeclared packages are `p-defpackage`'d at the
  file top (load reads one form at a time; qualified symbols must be
  internable when READ).
- **Context rules:** funcall args bind `*wantarray*` t; statement position
  is void (v2 emits NO void wraps); `return`/sub-tail = `'inherit'` = no
  bind; binds are emitted only for context-SENSITIVE callees.
- **Numbers:** leading-zero integer literals are octal — never native
  (`#o100` via fallback). `*read-default-float-format*` is double-float.

### 5.3 Test-writing conventions for parser2-01.t

- Shape tests: `my $cl = Pl::Parser2->parse_code(q{...}); like($cl, qr/…/,
  'desc');` — calls Parser2 directly, so gates DIE (catch with `eval {}` +
  `like($@, qr/…/)` when testing a gate).
- Runtime tests: inside the `SKIP:` block at the bottom; `$run->($cl)`
  loads the runtime + the transpiled text in a fresh SBCL; REMEMBER to
  bump the skip count in the `skip '…', N` line.
- Use `paren_balance($cl)` (helper at the bottom of the file) for anything
  involving raw seams.
- One test per behaviour; descriptions state the invariant, not the
  implementation ("list-assigned scalar stays boxed").

### 5.4 Debugging cookbook (symptom → procedure)

| Symptom | Procedure |
|---|---|
| SBCL `read error … end of file` while loading | Unbalanced output. Run the §0.2 paren checker on the generated `.lisp`; find the first form that never closes (`sed -n 'N,Mp'` around the reported line). Usual causes: a raw with a comment flattened (fixed — but check `_no_flat` first), a raw_wrap with a wrong count, a fallback that left `_local_let_depth` opens (the `_fallback_stmt` die should have caught it). |
| `The function main::pl-foo is undefined` at runtime | The sub definition never landed: nested sub not hoisted, prototype path not registered, or defs bucket ordering. Grep the generated .lisp for `p-sub pl-foo`. |
| `Package FOO does not exist` at LOAD time | A qualified symbol was READ before its package existed. The pre-declaration set in `parse()` missed it — check `_referenced_pkgs`, `get_undeclared_packages`, and whether the emission happens before the `(pcl:p-defpackage :FOO)` line. |
| `unbound variable $x` at load/run | Either a raw references a closed-scope lexical (W3 scoping), or `_forward_global_decls` skipped a name it shouldn't have (check its exclusion sets), or a defvar-poisoning case (a let-bound name got defvar'd — grep for `(defvar $x`). |
| Wrong VALUE, no error | The worst class. Transpile the file under BOTH pipelines (`--no-cache --lenient-ppi`), `diff` the outputs, and read the first semantic divergence. Then shrink: copy the construct into a ≤10-line repro, run `PCL_V2=1 ./runpl repro.pl` vs `perl repro.pl` vs `./runpl repro.pl`. If v1 also differs from perl, it is a pre-existing v1 bug — record it, do NOT fix it in this workstream. |
| Test file hangs under v2 | `timeout 120` everything. A dropped continue/step → infinite loop; diff the loop forms between pipelines. See also `docs/debugging-hangs-crashes.md`. |
| Value differs only in FULL file, not in your repro | Runtime state built up by earlier statements (last-accessed filehandle, `$.`; flip-flop state; `%ENV`). Instrument by inserting prints into a COPY of the generated .lisp (not the .t) and loading it via `sbcl --load cl/pcl-runtime.lisp --load copy.lisp`. |
| Census says a previously-native file fell back | Your new gate over-fires. The census line shows the die message; loosen the precondition or accept the loss consciously (write it down). |
| `./runt <name>` can't find helpers / `pl-plan undefined` | `runt` loads `cl/pcl-test.lisp` and runs from the right CWD; `runpl` does neither. Anything calling plan()/ok() needs `runt` and the file under `perl-tests/`. |

### 5.5 Runner cheat-sheet

```bash
echo '…' | PCL_V2=1 perl -I. pl2cl --no-cache      # transpile stdin, v2
PCL_V2=1 ./pl2cl --no-cache --lenient-ppi F.t      # transpile a test file
PCL_V2=1 ./runpl file.pl                           # transpile+run (CWD=repo root, no test.pl)
PCL_V2=1 ./runt <name>                             # run perl-tests/<name>.t with TAP harness
PCL_V2=1 perl sweep-perl-tests.pl --jobs 8 [files] # sweep (writes .faillog/)
PCL_V2_VERBOSE=1                                   # print fallback reasons to stderr
```

All runners pass `--lenient-ppi`; v2 ignores it (see pl2cl). `PCL_V2` is
inherited by p-use/require subprocesses — that is intended and safe
(whole-file fallback covers modules v2 can't lower).

---

## 6. Bookkeeping (every work item)

- Extend `Pl/t/parser2-01.t` for every behaviour added or bug fixed.
- Update `docs/parser2-prototype.md`: add a dated session section (follow
  the existing ones' style: what shipped, gotchas found, census/parity
  numbers) and refresh the census table. Strike through this plan's
  completed items with the commit hash.
- Append to `docs/session-log.md` (newest first, compact).
- Update memory: `project_parser2_prototype.md` gets a short dated block;
  MEMORY.md's ACTIVE section gets the one-line status + NEXT pointer.
- One commit per work item on `main`, message style `feat(v2): …` /
  `fix(v2): …` / `docs(v2): …`, ending with the Co-Authored-By trailer per
  CLAUDE.md. Include the census/parity numbers in the commit body.
- Never edit `perl-tests/*.t`; not-supported cases go through
  `cl/skip-registry.lisp` with a `docs/not-supported.md` entry — but for
  v2 work the normal outcome is a gate (die → v1), not a skip.
