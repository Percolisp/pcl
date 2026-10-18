# Ultrareview of s519a (review/s519a -> main), probed -- 2026-10-18

Written by the review session in the ultra clone
`~/pcl-agent-scratch/s513/review/ultra-s519a` (branch `review/s519a`, tip
`b695c5f2`) for the main session in `~/pcl`.

## What this is, and the one fact to read first

The cloud review (ultrareview) ran over the 27-file diff of `review/s519a`
against the clone's `main`.  **The clone's `main` is stale at `0d03285f`.**
In `~/pcl`, s519a is ALREADY MERGED as MAIN-READY-29 under rebased shas
(`7ee40337` m0 .. `4980ca01` everyday record; main `18d7f3a2`, sweep on the
merged main clean, TOTAL 18,756 +0).  `~/pcl` main `15cd2224` has
`Pl/Parser.pm` and `Pl/Parser2.pm` BYTE-IDENTICAL to the branch tip, so every
line number below holds on main.

Consequence: the confirmed items are silent wrongs that are ON MAIN now, and
the gate, the sweep and everyday did not see them (none of the shapes occurs
in those populations).  They are #3361 data points: regressions that a round
lets sit on main.

## How each finding was verified

Every finding was probed three ways: perl 5.40.3, the branch tip (`./runpcl`
in the clone), and the TRUE pre-batch base `0d03285f` (a throwaway worktree,
since removed).  A first attempt to use `~/pcl` as "the base" gave output
identical to the branch -- that is how the merge was noticed.  The probe
programs are in `~/pcl-agent-scratch/s513/review/ultrareview-s519a-probes/`
(f1 f1b f2 f3b f4 f5 f7 f8 f9 .pl); each is 2-4 lines and runs as
`perl FILE` vs `./runpcl FILE`.

## Verdicts

Severity labels are the review's.  "Base" = `0d03285f`.

### CONFIRMED REGRESSIONS (perl and base agree, branch differs)

**R1 (review: normal) -- `_split_void_local_list` splits a comma list it does
not own.**  `Pl/Parser2.pm:6995-7013` (the split; the statement-shape test at
6986).  It splits at every top-level comma without checking that the comma is
the statement's loosest operator.  A paren-less list operator or call
(`print`/`return`/`push`/`die`/`foo LIST`) or a looser `and`/`or`/`not` at the
same level owns the list.

| probe | perl | base | branch |
|---|---|---|---|
| f1: `our $x; print "a", local $x = 5; print "\n";` | `a5` | `a5` | `a` |
| f1b: `my $ok = 1; $ok or h(), local $x = 9; print "x=$x\n";` | `x=0` | `x=0` | `x=9` |

f1b: the `local` now runs UNCONDITIONALLY (`$ok or h(); local $x = 9;`).
`_logical_local_list` does not catch it because its first loop wants `local`
right after the `or`.  The review notes `_local_value_consumed`'s void test
has the same gap.  Fix shape: before splitting, reject (or route to the
refusal) a statement whose first term is a list-operator / call word taking
unparenthesised arguments, or whose top level holds an operator looser than
comma.

**R4 (review: normal) -- the local-term rewrites lose source NEWLINES, so
every later line number in the file drifts.**  `Pl/Parser2.pm:7009-7015`
(`_split_void_local_list` joins terms with `' '`) and `:7137`
(`_hoist_local_term` stops at `$pre->[-1]` and appends `$guard;`, dropping
the whitespace before `if`/`unless`).  The `"\n" x ($line-1)` padding in
`_replace_statement_by` does not help: nothing indexes the fragment's tokens,
so they arrive in `$doc` without `_location` and PPI re-indexes from there to
EOF against the shortened text.

| probe | perl | base | branch |
|---|---|---|---|
| f4: `foo(local $h{k} = 19)` NL `  if $c;` then `warn "here";` on line 4 | `line 4` | `line 4` | `line 3` |

Affects die/warn `at FILE line N`, `caller`, `__LINE__`, Test::More failure
diagnostics and the refusal/announce messages, by a running total over all
such rewrites in a file.  Capture::Tiny's `$localize{stdout}++,
local(*STDOUT)` NL `if ...;` (the pass's own cited shape, comment at
Parser2.pm:344) is a real instance.  Fix shape: keep the replaced statement's
newline count -- put the original inter-term whitespace (or at least its
newlines) between the emitted statements and before the guard.

**R8 (review: NIT -- but it is a new silent wrong of R1's weight) -- the
hoist invariant lets a CALL HEAD run after the localization.**
`Pl/Parser2.pm:7100-7110`: the safety scan before the `local` word skips
every Word, every `(`/`)` structure and the operators `=` `->` `\`, so
`foo()->m(local $x = 1)` / `$o->get->set(local $x = 1)` hoist although
`foo`/`get` run before the term in perl.

| probe | perl | base | branch |
|---|---|---|---|
| f8: `cfg()->apply(local $X = 'new')`, `cfg` prints `$X` | `cfg sees outer` | `cfg sees outer` | `cfg sees undef` |

Fix shape: reject the hoist when a Word followed by a call (an argument
list, or a method name after `->`) precedes the `local`, except the call that
directly owns the local's argument list.

### CONFIRMED, where the base was a DIFFERENT wrong

**R3 (review: normal) -- `local($$r) = LIST` / `local(${"name"}) = LIST`
(parenthesised, UNSUBSCRIPTED deref) now takes the deref branch with SCALAR
rhs context.**  `Pl/Parser.pm:4669-4671`: the pre-unwrap's new
`|| ref($flat[0]) eq 'PPI::Token::Cast'` strips the parens, so `$paren_lhs`
is never set; the cast-element branch at 4831-4835 needs a Subscript in
`$non_ws[2]` and does not fire; the deref branch (rhs_ctx 0 at ~5079) gets
it.

| probe (symbolic `$r = 'g'`, no strict) | perl | base | branch |
|---|---|---|---|
| f3b: `local($$r) = (7, 8); print $g` | `7` | statement DROPPED (announced) + die | `8` |
| f3b: `sub t { local(${"main::g"}) = @_; print $g } t(3,4)` | `3` | dropped + die | `2` (the count) |

**A LOUD refusal became a SILENT wrong -- the worst direction under rule 12.**
Fix shape: only unwrap a Cast-led list when a Subscript follows the cast
operand (the slice/element shapes the change targets), or carry the paren
fact into the deref branch's RHS context.

**R2 (review: normal) -- a one-key-group slice `local` with an initializer
and a statement modifier evaluates the condition THREE times.**
`Pl/Parser.pm:4910-4923`: the `pcl-local-cond-N` temporary is bound only
`if (@key_cls > 1)` (4912-4920); the new `!$ld_slice` test at 4923 sends the
one-group slice to the let/init path, where `$etest` is pasted inline into
`(if $etest init nil)`, `(p-local-maybe $etest ...)` and `(when $etest
assign)`.

| probe | perl | base | branch |
|---|---|---|---|
| f2: `sub f { $n++; 1 }` `{ local @a[0..1] = (1,2) if f(); print "n=$n a=@a" }` | `n=1 a=1 2 0` | `n=1 a=2 0 0` | `n=3 a=1 2 0` |

Base ran the condition once but assigned to the WRONG elements (`a=2 0 0`);
branch assigns right and runs it three times.  Fix shape: bind the temporary
whenever the let/init path is taken (`@key_cls > 1 || $ld_slice`).

**R5 (review: nit) -- the cast-spelling slice base (`local @{EXPR}{...}`)
is evaluated once per key group plus once for the initializer.**
`Pl/Parser.pm:4840-4845`: `$ld_cl_var` is the flattened text of
`(p-cast-% EXPR)`, spliced into every per-group open and into the
`(p-setf (p-hslice ...))` assign; no temporary (the Parser2 hoist binds an
expression base once to `$__pcl_lbase_N`; this branch does not).

| probe | perl | base | branch |
|---|---|---|---|
| f5: `sub f { $n++; \%h }` `{ local @{ f() }{qw(a b)} = (10,20) }` | `n=1`, values right | dies "Can't localize through a reference" | `n=3`, values right |

Fix shape: evaluate a non-Symbol base once into a let-bound temporary before
the opens and use that name in every macro and in the assignment.

### CONFIRMED BY CODE READ (not probed)

**R6 (review: nit) -- `_lower_local_terms` makes at most 8 rewrites per
document.**  `Pl/Parser2.pm:6718` `for my $round (1 .. 8)`; each round
`last`s after ONE successful split/hoist (6726 "the tree moved: find
again").  A document's ninth qualifying `local` term is neither split,
hoisted, refused nor announced -- it keeps the old silent plain read, which
is exactly what #3370 set out to remove.  Fix shape: loop until a round
makes no change (each rewrite leaves a leading `local`, so it terminates),
or keep scanning within a round.

**R10 (review: nit) -- comment misplacement.**  `Pl/Parser2.pm:~15168-15182`:
the new `_cond_mys_closure_captured` comment runs straight on after
`_cond_my_names`'s doc comment ("Returns a deduped list of $names ... Dies
(-> v1) on an array/hash my") with no blank line, above the wrong sub.

### PARTLY RIGHT -- an improvement that is still wrong, NOT a regression

**R7 (review: nit) -- `while (local $x = ...)` hoists the local out of the
loop condition to the enclosing block.**  `Pl/Parser2.pm:~7089` (the
ancestor walk's `next if ... Condition ... /^(?:if|unless|while|until)$/`,
justified by "perl's local there lasts to the block end" -- true for `if`
(RT #7615, perl-tests/local.t 862 TODO), NOT for loops: perl restores a
loop-condition local per iteration and on leaveloop).

| probe | perl | base | branch |
|---|---|---|---|
| f7: `{ while (local $x = shift @a) {} print $x }` then `print $x` after the block | `orig` / `orig` | `undef` / `` (never restored) | `undef` / `orig` |

Inside the block both base and branch read undef where perl reads the
original; the branch at least restores at block end.  Fix shape: only the
`if`/`unless` condition qualifies for the hoist; `while`/`until` refuse or
announce.

### NOT REPRODUCED

**R9 (review: pre-existing) -- `_cond_mys_closure_captured` (#3374) skips an
anon sub whose next token after `sub` is not the Block (signature /
prototype / attribute).**  `Pl/Parser2.pm:15189-15191`.  The claim reads
right in the code, but the probe is perl-identical on the branch:

| probe | perl | base | branch |
|---|---|---|---|
| f9: `use v5.36; while (my $x = shift @q) { push @cbs, sub ($arg) { "$x:$arg" } }` | `1:z,2:z,3:z` | `:z,:z,:z` | `1:z,2:z,3:z` |

The prototype (`sub (&) {...}`) and attribute (`sub :lvalue {...}`)
spellings were NOT probed.  Low priority; if the detector really skips the
Signature token, something else is making f9 right and that is worth one
look before closing it.

## Suggested handling

R1, R3, R4 and R8 are silent, on main, and each is a 2-4 line perl program
away from a guard row.  One task each (or one task for the split guard R1 +
hoist invariant R8, since both are "what may precede the `local`"), each
carrying its probe as the discriminating measurement.  R2 + R5 are one task
(the slice path's temporaries).  R6 is a one-line fix with a 9-term guard.
R7 and R10 are fillers.  R9 closes after one probe of the two unprobed
spellings.

The review session did NOT change any code or file any task; this note and
the probe directory are its whole output.
