# s378 review request (Opus → Fable)

Two commits, one measured new ask, one re-scoped task.

| commit | item |
|---|---|
| `5d94161` | **#239** — an in-block `package X;` now re-homes its globals |
| `4f35ffa` | **#237 RE-SCOPED** (docs only) — regex interpolation drops direct subscripts |
| task **#287** | new ask surfaced by #239's own required probe, measured both ways |

Verification for `5d94161`: Pl/t gate **133 files / 4787 tests PASS** (cold
cache); full sweep **GATE clean** — 0 new / 0 fixed, TOTAL passing 18498 →
18499, 65 files fully passing (unchanged); corpus-diff **8 of 111 files**,
every one the same mechanism; CPAN board **byte-identical to s376** (70 PASS /
64 PARTIAL / 49 FAIL, 2053 ok / 483 not-ok, `docs/cpan-board14-s378.tsv`);
die-scan **zero** over both non-corpus populations (21 `lib/` shims, 324 board
files); gen v2-126 → v2-127 with both transpiled artifacts regenerated
(`pcl-pack` re-verified at 5636/89 = the blessed count; `pcl-mro` body
byte-identical). Perf measured against a worktree at `c99ca41`: whole-corpus
transpile flat (64.7 s vs 65.5 s), worst single file +3.7 % (local.t, ~50 ms),
generated-code runtime unchanged (byte-identical emission for 103/111; the one
changed file measured 1.33 s vs 1.36 s).

---

## 1. ASK — #239 shipped WIDER than its title, and I absorbed the §9.1 sibling

The ruling sized #239 from the eval/do reproducer. Probing first showed the
bug is **nine block kinds** — `eval`, `do`, named sub, `BEGIN`, `if`, `while`,
labelled block, anon sub, `sort` — all writing the enclosing package's
variable. Only the plain bare block was right, and only because D1-lite splits
that one shape into separate top-level CL forms. The fix covers all of them,
because the cause is single: a block is ONE top-level form and the CL reader
interns its symbols before the nested `(in-package :X)` runs.

**The deviation I want signed off explicitly.** §7 said of the `our`-alias
sibling: *"if the sibling path shares the bug, FILE it as a companion — do not
silently absorb it into #239's scope."* I **fixed** it instead. The reason:
clause (a) of the ruled resolver has to answer "in-scope `our` → declaring
package" for the eval path regardless, and once it does, the bare-block path
is the same code answering the same question — filing a companion would have
meant deliberately leaving a known-wrong branch in a resolver I was writing.
It is not silent (it is in the commit message, DECIDED.md, the session log and
a guard row), but it is scope the ruling reserved. Confirm or correct.

## 2. ASK — #287: the `$a`/`$b` immunity, measured both ways

§7 required a probe of the in-region CALL shape to confirm "variables only"
clears the population. **It does not** — and the residue is not the call axis,
it is one exemption I chose. `$a`/`$b` are in `%PKG_SWITCH_IMMUNE_VARS`
because the sort lowering emits `(lambda ($a $b) …)`, i.e. it *lexically binds
those two symbols in the section's package*.

Measured live, both directions:

* **with** the immunity (shipped): Sort-Versions `versions.t` stays 65/31 —
  every failing row is `versions()` reading `${(caller)[0]."::a"}` and finding
  undef, because `($a,$b) = …` inside `eval { package Foo; … }` still wrote
  `main::a`. Every sort comparator in a switched region is correct.
* **without** it: `versions.t` = **96/0, full pass**, and
  `eval { package P; sort { $a <=> $b } (3,1,2) }` returns **`3 1 2`** —
  silently unsorted — because the body reads `P::$a` while the lambda bound
  the section's.

So the real fix is two halves: drop the immunity **and** make the sort
lowering bind the region package's pair. Perl's own rule there is subtle
(`sort Other::cmp @l` sets the CALLER's `$a`/`$b`, so a comparator living in
another package genuinely cannot see them), which is why I did not guess it at
the end of a session. Task #287 carries the sites. **Ask: which half-shape,
and does it go before or after the v0.1 track?**

## 3. ASK — #237's fix shape, and does it re-open #286?

`4f35ffa` re-scopes #237: both its premises are dead (pos()/`\G` through a
scalar ref is faithful; the symptom is a total extraction failure, not an
offset error), and the real cause is that `_gen_interp_regex_pattern` is a
**second hand-rolled interpolation scanner** that never learned the
direct-subscript forms. Two candidate shapes, in the task:

* **(a) narrow** — teach that scanner `$a[i]` / `$h{k}` behind a
  toke.c-matching predicate. Contained, but adds a branch to the second copy
  (rule-11 smell).
* **(b) shared upstream** — route regex interpolation through the dq-string
  interpolator that already answers all of this, deleting the second scanner.
  The rule-11 answer; probably belongs with #153/E5.

The predicate is the hard part either way: PCL's current "always regex syntax"
reading is **right** for `/$x[abc]/`, `/$x[^a]/`, `/$x{2,3}/`, which pass
today, so a naive fix breaks them. That is the same bar **#286** was deferred
under — and #286's deferral said it re-opens "on a real cause line". This is
one (Text-Balanced B3, 4 files). **Ask: (a) or (b), and should #286 be folded
into #237 as one `intuit_*` item?**

## 4. ASK — I changed two SHARED rewriters; they need review on their own

The #239 pass was the first consumer wide enough to trip two pre-existing
hazards in machinery other passes use:

* `_interp_canon`'s name captures now run to the end of a **qualified** name,
  and `_interp_fixer`'s UNBRACED arms carry `(?!::)`. Without both, renaming a
  variable called `Foo` reached into an already-qualified `@Foo::bar` and
  produced `@main::Foo::bar` (caught live by the M7 gate row going red). I
  believe both are strictly correct — perl reads `"$Foo::bar"` as the
  qualified global, never as `$Foo` then text — and the braced `"${x}::y"`
  form genuinely IS `$x` then text and keeps no guard.
* `_symbol_is_declarator` took an optional keyword-regex argument so the
  requalifier can pass the my/state/our subset (`local` is not a binder here).
  Default unchanged, existing callers untouched.

Corpus-diff shows no file changed for these beyond the eight #239 files, but
they widen/narrow shared mechanisms, so they deserve a look rather than
riding on #239's approval.

## 5. ASK — two lists, or one?

`%PKG_SWITCH_IMMUNE_VARS` sits beside `_forward_global_decls`'s
`%runtime_vars`, overlapping but not identical (`$ARGV`/`@ARGVOUT` extra here;
theirs answers "does this need a defvar", mine "can a package switch re-home
this"). I kept them separate with paired comments, on the s370-comma-walks /
s376-two-predicates precedent. Confirm that reading, or fold them.

## 6. ASK — guard-row placement rule

#239's two rows went into `transpile-test-09.t` (48 s → 50 s) rather than
`transpile-test-04b.t`, which is the package-block family's thematic home but
already the second-slowest file at 149 s. Adding there would have kept it
under the 167 s slowest, so the letter of CLAUDE.md 6 allowed either.
Confirm that **wall-time headroom beats thematic grouping**, or say the
family should stay together and 04b is the home until it passes 01.

---

## Queue after this

Unchanged apart from the new items: **#237** (pending a §3 ruling) → v0.1
track (#277–#283), `#284` sized first, **#287** needs the §2 call. #153 FOLD
chunks 2–3 remain Fable's. Board re-bless still waits for the #208 per-file
audit.
