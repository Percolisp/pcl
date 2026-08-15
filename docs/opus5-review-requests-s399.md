# Review request — s399 (Opus 5), for Fable

Five commits, the s397 queue worked in order (items 1–6).  Everything is
measured; the parts worth your time are the three places where **the premise a
queue item was filed under turned out to be wrong**, and one thing nobody had
ever counted.

Gate **142 files / 5275 tests** (failures exactly the 13 pclxs xs rows the user
said to ignore).  Full sweep **GATE clean**, TOTAL passing **18516** — down 24
by design, see §4.  `tools/corpus-diff.pl` identical at every step.  Cache
generation v2-147 → **v2-148**.

| commit | item |
|---|---|
| `7af2a97` s399a | #331 artifacts regenerated + a staleness GATE |
| `bf3fe69` s399b | #332 `\(@a) = LIST` silent wrong |
| `f8ffd56` s399c | ASK-1 internals section — and op/const-optree.t NOT registered |
| `6f04839` s399d | #323 the three warning helpers made honest |
| `c754abc` s399e | #314 F-D scalar half |

Sized, not started: **#340** (op/try.t), **#341** (op/lexsub.t), **#342**
(base/lex.t).  Filed: **#337** (`my sub` scoping), **#338** (F-D container
residue), **#339** (a warn that is not noise), **#343** (the drop census).

---

## 1. #331 — there are THREE checked-in artifacts, and the drift is now a gate

The task named two; the test I wrote to stop the drift recurring found
`cl/pcl-warnings.lisp` (from `lib/warnings.pm`), also eleven generations stale.
It found it because the file **discovers** artifacts by their line-1 `gen=`
stamp instead of listing them, and carries a COUNT row so a header-format
change cannot make it pass vacuously.  That count row is the whole reason the
third one is not still stale.

Consequence, deliberate and worth your ruling: **an emission-changing commit
that bumps `*pcl-cache-generation*` must now regenerate the artifacts in the
same commit**, or `Pl/t/artifact-staleness-01.t` goes red.  That is what
CLAUDE.md already asked for and nothing enforced.  The cost is one
`tools/rebuild-pack` (~40 s) plus, when pack's emission really moves, its
pack.t verification.  I think that is the right trade; say if you disagree.

## 2. #332 — the fix is the sibling, and the RHS context was the hidden half

`\(@a) = LIST`, `\my(@x) = …`, `\(my @x) = …` all reached `p-setf` as the
RVALUE form `(p-list-scalar (p-refgen-list @a))` — a list of fresh refs, not a
`\`-cast place, so the alias arm never saw it and the array stayed EMPTY at
exit 0.  Recognised on the FORM (the one shape the three spellings share) and
lowered to `(p-backslash-list @a)`, so it flows through the SAME dispatch as
`\$x = REF`; the runtime arm resolves referents through the same
`p-alias-scalar-target` the scalar arm uses.

The half the task did not mention: **the right-hand side had been generated in
SCALAR context** (`(progn (p-backslash $x) (p-backslash $y))`, the comma
operator), so even with the place fixed a call on the right would have been
wrong.  It is now LIST context — probed with `\(@i) = refs();`.

Fifteen shapes byte-identical to perl.  Rule 12: any other `\(…)` target dies
at transpile, matching perl, which rejects `\(%h) =` and `\(@$ref) =` at
compile time.

## 3. ASK-1 — the section is in, but **op/const-optree.t must NOT register**

The class section exists as ruled ("Readouts of perl's own internals: `B::`
optree inspection, `re::optimization`, `XS::APItest`", `grep -c` over
`perl-suite-expected.tsv` IS the population, "what would lift it: nothing").

The ruling also authorised registering op/const-optree.t on the premise that
**every** one of its 62 diverging rows is such a readout.  The per-row read the
bar demands says:

| rows | what they are | class |
|---|---|---|
| 53 | `… is/is not inlinable` (28), `… has (no) :method attribute` (25) | this section |
| 5 | `… now throws exception (RT 134138)` — perl REJECTS `sub () { $x }` when `$x` is modified elsewhere; PCL compiles it | §Error compatibility for invalid Perl input |
| **4** | `retval of my sub …` | **a real fix target** |

The four are task #337: PCL compiles `my sub x () { 8 }` as a PACKAGE sub, so
two same-named lexical subs in different scopes clobber each other and every
`\&x` resolves to the last one —

    my $g1 = sub { my sub x () { 8 } \&x };
    my $g2 = sub { my sub x () { 3 } \&x };
        perl: 8 3        PCL: 3 3        silent

In ISOLATION PCL is right, which is why it took a file with two of them to
show.  All-or-nothing keeps the file UNEXPLAINED; the section records the split
so re-registration after #337 needs no re-derivation.  I ran `--bless-rows` and
reverted it with the row.

**The general point for future rulings:** a registration authorised in the
abstract still has to survive its per-row read, and this one is the second time
that read has changed the answer (s393's `unlike` was the first).

## 4. #323 — 24 rows went red on purpose, and **#221's trigger has fired**

The three helpers ran the code and `pass()`ed unconditionally.  Replaced by the
real t/test.pl bodies on top of `capture_warnings`.  Every row that flipped has
ONE cause: PCL emits no warnings-gated diagnostic.

    perl-tests/assignwarn.t  116/0 → 96/20    Use of uninitialized value
    perl-tests/hashassign.t  309/0 → 305/4    odd elements / even-sized list
    perl-tests/time.t        72/0 unchanged   (its rows expect NO warning —
                                               they pass HONESTLY now)

Task #221 (a minimal `use warnings` model) was UNSCHEDULED pending "the first
test family or CPAN module whose failure is *warning not emitted*".  **This is
that family**, and with the companion suite it is 121 rows (assignwarn 20,
hashassign 4, numify 11, utf8decode 86).  I did not schedule anything — the
standing rule (default-off diagnostics stay ABSENT, never unconditional) is
untouched, and there is now a not-supported section with #221 as owner.  Your
call whether the trigger firing changes #221's priority.

Two measurement notes:

- **The companion population is CLOSED**: eight files call these helpers
  anywhere under perl's `t/`, and all eight were run.  No `--all` needed.
- **op/assignwarn.t's row set is nondeterministic** and is registered
  `*rows-unstable*` on measurement, not assumption: the file iterates
  `keys %should_warn`, so both sides emit rows in per-process random order and
  the description-multiset pairing gave 81 then 73 diverging rows in two
  consecutive runs.  The COUNTS are stable, so the file still registers XDIFF.
- op/utf8decode.t (644/42 → 620/90) stays UNEXPLAINED: 86 are this entry, 4 are
  a pre-existing divergence of another kind (verified in a worktree at
  `f8ffd56`).  Its row count now MATCHES perl's 710, which retires the
  snapshot's "numbering is offset for 592 rows" note — the old stub emitted one
  row where perl emits two.

Baselines edited ROW BY ROW with a header note; `sweep-diff` clean against the
edited pair.

## 5. #314 F-D — the scalar half shipped; the container half was diagnosed wrong

`my ($fetch, $store) = (0, 0);` spanning a package boundary refused with
`sdecls=0 dc=1` — the declaration COUNTED but no statement was found to rename.
perl declares each name of a list form exactly as the single form does, and the
pass renames ONE symbol inside the declaring statement either way, so the two
facts are merged for that pass in one accessor (`scalar_decl` + the `$`-sigil
entries of `mlist_decl`, which already recorded exactly this).  The promotion
pass keeps reading them separately — merging there processes a list decl twice.

Measured at the s372 bar with a **GATE-SET scan of 638 files** (111 perl-tests
+ 527 of perl's t/, first stderr line normalized, ~2.5 min per population):
FOUR rows differ and only four.

**The s395 diagnosis of the container half was imprecise** — it is not "the
facts scan does not record container declarations" (`container_decl` and its
own span loop have existed since s305).  It is two things (#338): op/svleak.t
declares `my @a` three times, so the container loop's file-uniqueness rule
refuses; and a container name inside a list decl (`my ($x, @rest)`) needs the
DECL LOWERING, which knows only the no-init single-container shape.  The first
may be lifted by direction D (a `let` of a symbol-macro name is legal and
shadows it — the comment that justifies the rule predates that), but that is
your call, and it is worth ZERO rows.

Small thing in the same commit: the checker's die now names the CANONICAL
variable (`my-lexical 'a' (canon @a) …`).  The bare name hid the one fact that
says which loop must handle it.

## 6. The thing nobody had counted: **72 files carry a silently dropped statement**

Chasing a stderr line the gate-set scan kept printing led to
`(progn ;; PARSE ERROR: … nil)` — the #138 family.  Census in
`docs/parse-error-drop-census-s399.tsv` (task #343), taken at `c754abc`:

    72 files, 379 drops    9 perl-tests (12 drops) · 63 companion (367) · 0 lib
    56 files  "Bug. Fell through. Missing case: ["
    21        "Handle single node of unknown type"
     5        a ruled "PCL:" refusal swallowed into a drop (deliberate, §below)
     1        Can't locate object method "add_node" — an INTERNAL error, hidden

**A drop is not cosmetic.**  perl-tests/bless.t's single drop is

    is ref $untied, "main", 'blessing through tied refs' or diag $@;

— an assertion that never runs and appears in no count, in a file the sweep
reports as passing.  The dominant cause minimises to

    f ref $u, "m", "d" or g "fb";    perl: f( m d) g(fb)    PCL: (nothing)
    f ref $u or g "fb";              perl: f() g(fb)        PCL: (nothing)
  INVERSE, both fine today:
    f $u, "m", "d" or g "fb";        f(ref $u, "m") or g "fb";

i.e. **parenless call × named-unary first argument × a following low-precedence
`or`/`and`** — which is `Test::More`'s `is … or diag $@`.

I did not touch it: that is the `$end_pars` region
`pexpr-term-parsing-review.md` says not to patch in place.  **My ask: put this
reproducer, with #259 (a `(;$;)`-prototype call drops) and #335 (a list slice
heading a postfix-if drops), into Option B phase 2's acceptance set, and make
the census phase 2's metric** — re-run `tools/drop-census.pl`, the number falls, every
remainder explained.  Two cheaper pieces that need no parser work are on the
task: the `add_node` internal error, and a GATE (a per-file drop count vs a
blessed census) that would have caught the s398 `(LIST)[i]{k}` drop
automatically.

**One design question for you.**  The 5 "PCL:" refusals that appear as drops
are DELIBERATE: `_shape_expr_error` re-raises every `PCL:` error except
"Can't modify non-lvalue subroutine call in assignment" outside eval mode (the
eval half must propagate so the eval returns undef, like perl).  perl refuses
to compile the whole FILE for the non-eval case; PCL drops the one statement
and runs on.  33 of the 379 drops are that.  Keep, or make it die?

## 7. Also filed

- **#339** — PExpr's `Handle single node of unknown type` warn prints for a
  routine decline on 25 companion files.  I tried deleting it and **reverted**:
  in op/glob.t that decline is a real dropped statement (`ok <~>, '~ works';`).
  The distinction that matters is handled-decline vs dropped-statement, and it
  belongs at the caller that emits the drop.
- **#341** corrects #314's note on op/lexsub.t: it does NOT die on "Negative
  repeat count does nothing" — that is a warning printed ten times, from a
  NEGATIVE `indent_level` in `Pl/Parser.pm:8908` (its own small bug).  The
  killer is the capture refusal.
- **#342** records that base/lex.t's blocking shape is a rule-12 violation in
  isolation: pl2cl exits 0 and emits `(p-subst … (lambda () nil) :e)`, so the
  s///e replacement silently becomes nil.

## 8. Queue as I leave it

Remaining from the s397 list: the **v0.1 track** (#277–#280, #282–#283) — I did
not start it, because it is release engineering with user-facing decisions
(installer shape, repo hygiene, CI) rather than mechanical work.  Fillers
unstarted: #330, #321, #322, #324, #326, #328, plus this session's #337–#343.
