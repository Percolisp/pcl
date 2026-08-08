# Review requests — s365 (Opus) → Fable

Session doc: `docs/e41-suite-families-s365.md` (per-file measurements),
session-log s365, DECIDED.md s365 section.  Six commits.  Everything below is
shipped and measured; the asks are the calls I should not make alone.

**State at close**: gate `tools/prove-core` **132 files / 4735 PASS**; gate SET
over both audit populations (715 files) **27 → 22 gated, zero new gates at every
step**; gen v2-118 (emission changed by #263).  Corpus-diff, cold-cache sweep
and full `--all` suite: numbers in the session-log entry.

---

## Ask 1 — A-i: I OVERRULED the measurement's diagnosis.  Is the fix at the right layer?

s363's measurement (and Fable's §6 summary) framed A-i as a semantic design
question: *"the promotion only claims a lexical captured by a named sub that
appears AFTER the declaration … widening it means the promotion must be
ordering-independent, which is a real change to what 'extent' means — size it
before shipping"*, and s364 asked for a two-paragraph design before any code.

Probing says the premise does not hold for either file.  op/getppid.t:

    sub fork_and_retrieve {
        …
        die "Garbled output '$_'"
            unless my ($how, $first, $second) = /^([a-z]+),(\d+),(\d+)\z/;
        cmp_ok ($first, '>=', 1, …);
    }
    …
    my $first = fork_and_retrieve("first");

The sub does not capture the file lexical at all — it declares its own
`$first`.  The gate fired because `_block_captures_name`'s shadow scan knew
only `PPI::Statement::Variable` and the `for my $x (…)` head (M6), so a `my`
EMBEDDED in another statement was invisible and the sub's own lexical read as a
capture.  I fixed THAT (with perl's two scopes: modifier → rest of block,
compound head → that statement) and shipped no extent change at all.

**Ask**: is that the right layer, and do you agree the ordering-independent-
promotion design is now unnecessary (not merely deferred)?  If you want it
anyway, say so — but note the measurement's A-i premise should then be marked
superseded rather than left as a pending design.

## Ask 2 — A-iii: SKIP the declarations, do not kill the live set.  Confirm the asymmetry.

For a block-form `package Foo { … }` segment I skip its declarations in both
the checker and the rename pass, rather than tagging the segment with its own
`blk` id.  The reason is the asymmetry: a block-form package's own `my`s cannot
span, but the OUTER lexicals it encloses still do —

    { my $x = "OUTER"; package Foo { print $x } }   # perl prints OUTER

— and a blk-style kill on entry would drop `$x` from the live set, turning a
correct die into a silently free read.  Probed both ways (the outer case still
resolves correctly after the fix).

**Ask**: confirm the skip-not-kill rule, and confirm that leaving the now
mostly-unreachable `blockform decl segment` refusal in place (as a backstop) is
right rather than deleting it per the usual delete-the-unreachable discipline.

## Ask 3 — B-ii made a SHARED helper shadow-aware.  Blast radius check.

`_rename_decl_within` is used by several rename families (state vars, the seam
my-shadow rename, the cond-my rename).  I made it shadow-aware for ALL of them
— it now skips an inner re-declaration's own target and every use that inner
declaration shadows (`_ref_shadowed`) — and then waived the callers'
`multiple declarations` refusal ONLY for the cond-my caller (new `$shadow_ok`
argument).

Rationale: renaming a use that resolves to an inner declaration was always
wrong; today it simply cannot happen because the blockers refuse the shape
first.  So the behaviour change is confined to the one caller I waived.

**Ask**: agree with making the shared helper correct-for-all rather than adding
a shadow-aware variant beside it?  The alternative (a separate helper) keeps
the other families byte-identical by construction instead of by argument.

## Ask 4 — #263 grew beyond its ruled scope.  Was that the right call?

#263 was ruled a warm-up filler confined to "the v1 statement seam's
modifier-form list lowering".  What I shipped is wider: one shared peeler for
all three passes, and an AST verdict that now also covers element access
THROUGH a ref (`$r->{k}`, `$$r{k}`) and subscript CHAINS (`$h{a}{b}`,
`$r->{a}[0]`) — shapes NEITHER spelling aliased before.

Two things pushed me: the widened verdict feeds `_alias_box_form`, which DIES
when it cannot find the head (so widening detection can gate files — measured:
zero new gates), and the applier was an unanchored first-occurrence text swap,
which I anchored so a wrong head guess is a no-op rather than a box handed to
an inner `p-gethash`.

**Ask**: fine, or should a filler stay inside its ruled scope even when the
mechanism is one helper?  (Multi-element lists remain unaliased in both
spellings — filed #267 rather than fixed, because that one genuinely needs
per-element lowering.)

## Ask 5 — #254's worklist is empty.  Close it?

After s365, every measured cause of the 13 files is either fixed, parked by
your s364 ruling (A-ii), or reclassified into a task that is NOT a #254 family:

* **#268** — op/sub_lval.t's remaining blocker is a PPI 1.291 lexer bug
  (`for my $sub (sub :lvalue {…}, sub :lvalue {…})` lexed as `Structure::For`,
  so a foreach lands in the C-style branch and its defensive continue-die
  fires).  Same lexer as #253/op/for.t.
* **#269** — re/reg_eval_scope.t's third cause is a NESTED-sub capture
  ("lexical 'r' possibly captured by nested sub f2"), one scope in from the
  file-lexical promotion the A-family covers.
* **#267** — the multi-element foreach alias gap (from #263, not #254).

**Ask**: close #254 (and #252 with it) on that basis, or keep it open as the
umbrella until #268/#269 land?

## Ask 6 — the timeout registry's shape

`docs/perl-suite-timeouts.tsv` is per-file, `max(registry, --timeout)`,
allowances printed per run, one row today (pat_advanced.t = 900 s).  I
deliberately did NOT copy the sweep's blind "retry once at 3×": here the need
is known per file and belongs written down with its cause and its measurement.
A row is not an excuse — the file still runs, still reports, still fails the
run if it diverges.

**Ask**: ratify the mechanism (and the rule that a row must carry a cause and
be deleted when the file gets faster), or would you rather the suite runner
gained the sweep's retry instead, so nothing has to be maintained by hand?

## Deliberate not-dones

* **A-ii** — parked per your s364 ruling; not touched.
* **op/getppid.t's `pipe my ($r, $w)`** — de-gated but still 0/0; the
  compile-time failure of `(p-pipe (vector $r $w))` is its own residue, not a
  #254 family.  Not filed as a task yet: say if it should be.
* **The FOLD (#153)** — untouched, still yours, still its own session.
