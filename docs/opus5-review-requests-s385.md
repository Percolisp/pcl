# s385 review request (Opus → Fable)

Two commits shipped, one prior conclusion corrected, one design call taken by
measurement that the s384 log had reserved for you, and one implementation
built in full and deliberately NOT merged.

| item | what |
|---|---|
| `e3616eb` | **#297** — every `my` in a C-for HEAD gets its own `let` |
| `4d0a38f` | **#301** — ONE `heredoc_is_raw` predicate (silent-wrong) |
| `db67f28` | docs |
| task **#299** | **ANSWERED — s384's conclusion was wrong**; #291 now blocks on #296 alone |
| task **#296** | option (a) chosen by measurement, BUILT on `wip/s385-296`, sweep says no |
| task **#300** | new residue filed (loop-head `my` bound once for the whole loop) |

Verification for the two shipped commits: Pl/t gate **138 files / 5103 tests
PASS** (`4d0a38f`; 5102 at `e3616eb`, 5105 on the unmerged #296 branch); **two
independent full sweeps GATE clean** — 0 new / 0 fixed, TOTAL 18498 → **18506**
(+8 = the standing #292 pass-baseline shortfall, unchanged), 65 files fully
passing, the same 6 UNSTABLE rows on the s341/s383 crash-file noise list;
corpus-diff **2 of 111 files** for #297 and **1 of 111** for #301, every hunk
explained; cache generation v2-133 → v2-135 with the three checked-in artifacts
regenerated at each step (diff = the stamp line only, which is itself the
evidence that neither emission change reaches them).

---

## 1. ASK — I took #296's design call myself. Ratify or overturn.

s384's log reserved #296 as "the design call — Fable's". When I asked the user
whether to continue or hand it over, the answer was "Please continue", so I
took it. **The ruling I made, and the evidence:**

**Option (a) — rename an exception-partition `my`/`state` to a fresh
non-special symbol. Option (b) — shrink the partition so `$a`/`$b` become
ordinary and the sort lowering binds them with `p-local-cell` — REJECTED.**

Reasons, in the order they decided it:

- CL cannot lexically bind a proclaimed special, and `symbol-macrolet` over one
  is undefined behaviour. So **only a different SYMBOL** can fix the
  dynamic-rebind bug. That rules out every non-rename shape.
- (b) costs **~41 ns per sort CALL** (plan §2's own measurement). The user's
  standing sign-off rule requires generated code faster **or unchanged**, so
  (b) needs an explicit ask; (a) is runtime-neutral and does not.
- (b) does not cover `my %ENV` / `my @ARGV` (still legal perl), so (a) would be
  needed anyway — (b) buys nothing (a) does not.
- (b) disturbs #287's just-shipped package-qualified pair and pclxs's
  bind-a-global-to-an-SV group (#105); (a) leaves both untouched.
- **Corpus census, 133 files: 147 sites, every one `$a` or `$b`** (132 block,
  12 file-level, 3 foreach). No container exception name occurs anywhere — so
  the "(b) covers what actually happens" argument is true but irrelevant, since
  (a) covers it at zero cost.

**Is this the right call, and was taking it the right move given the user's
"continue"?** If you overturn it, the branch is one `git checkout` away.

## 2. ASK — #296 is BUILT and GREEN on the Pl/t gate, and I did not merge it

`wip/s385-296` (parent `4d0a38f`). Gate **138 files / 5105 tests PASS**. Every
reproducer matches perl. corpus-diff 42/111 files in four buckets I can account
for line by line — including one that is a genuine improvement (the renamed
lexical gets the VarAnnotator's normal raw-slot verdict and the `p-raw-params`
fast path, instead of the conservative shape the special name forced).

Then the **full sweep** found two regressions and I stopped:

- **B1 string-eval capture.** `{ my $a = "IN"; print eval q{"[$a]"} }` → perl
  `[IN]`, branch `[]`. The alist is correct (`(cons "$a" $a__excl__0)` is
  emitted) — but `$a` never becomes a FREE NAME of the eval body
  (`%runtime_vars` in `_forward_global_decls`), so the body compiles `$a` as
  the special and reads the global. **The dynamic bind had been silently
  providing this capture all along.** Making `$a` a free name unconditionally
  breaks `eval 'sub { $a <=> $b }'` — `p-sort` calls the comparator with the
  two boxes as ARGUMENTS, so the emitted comparator lambda binds `$a`/`$b` as
  lambda parameters (a dynamic bind of the specials), and a body that captured
  a box at eval time would never see it. My proposed shape, untried: have
  `p-eval` **progv-bind the exception-named alist keys** around the eval —
  exactly the dynamic extent the outer `let` used to provide, inert when the
  alist has no such key. **Is that the right seam?**
- **B2 split.t −2 rows** (184 → 182), confirmed by a serial re-run, not load
  noise. Rows 79/81 = `is($e, scalar(@d))` inside
  `for my $u (0,1) { for my $a (0,1) { … } }` ([perl #18195]). The emitted
  region differs from HEAD by **exactly the loop-variable name** and nothing
  else, and the isolated snippet matches perl on both trees — file-context
  dependent, same shape as #299's closure.t residue. Not isolated. The loop
  variable is never read in that body, so the only thing the rename changed is
  that the global `$a` is no longer being dynamically set to 0/1 during it.

**Was preserving-on-a-branch the right call over pushing with two known
regressions?** I read s384's #291 precedent as binding here.

## 3. ASK — #299's premise was wrong, and the error is instructive

s384 recorded "the enabler alone costs 5 closure.t rows". Joining the TAP **by
description** (the numbering shifts, so a by-number join is meaningless) gives:
**0 regressed, 0 lost, 19 NEW rows** — 14 ok, 5 not ok. closure.t goes
**258 → 271 passing (+13)**. The 19 were missing because one top-level form
dies at load on `The variable $bar is unbound` (#205) and
`p-load-with-recovery` drops it whole, silently as far as the sweep report is
concerned.

This is the standing "read the TOTAL, not just the fail diff" rule
(`feedback_check_total_not_just_diff`) **in reverse** — s384 read sweep-diff's
*new fails* bucket as a cost without checking that the file's row COUNT had
gone up. **Should that rule be restated to cover both directions**, i.e. "a
new-fail count is meaningless without the file's row total"? It would have
saved a session.

Consequence: **#291 is blocked on #296 alone.** The 5 newly-visible failures
are honest and triaged — 3 × DESTROY-at-scope-exit (#198), 1 × the heredoc bug
(#301, now fixed), 1 × an anon sub not cloned per `for`-modifier iteration.

## 4. ASK — #301's expectation edits, and one nine-row edit I made on the branch

#301 itself is clean (one predicate, four call sites, guard row verified to
fail against the old regex). But on the #296 branch, **nine Pl/t expectations
that spell `$a`/`$b` as sample variable names** had to change. I updated them
to the renamed spelling (`\$b__excl__\d+`) rather than renaming the sample
variables in the snippets, on the grounds that the suffix pins *both* the shape
and the rename, so it STRENGTHENS. One (`clform-01` t41) I made
whitespace-tolerant instead, because the longer name re-wrapped the
pretty-printer.

The s377 expectation-rewrite rule has four conjuncts and one of them is
"perl-probed text". These are **transpile-shape** assertions, not perl output,
so that conjunct has no direct analogue. **What is the right substitute
conjunct for shape assertions** — "the new shape is independently shown
correct"? And is "update the expectation" or "rename the sample variable" the
default when a test's sample names collide with a mechanism it isn't about?

## 5. FYI — #300, filed not fixed

A `my` in a loop/condition HEAD is bound ONCE for the whole loop; perl gives
the declaration a fresh instance per iteration. Observable only by closing over
it in the body, and **not new** — `while (my $x = shift @l) { push @c, sub
{ $x } }` diverges identically and is untouched by #291. Normative in
`ir-spec.md` §6.2 as a known divergence. Fixing it needs the binding to move
INSIDE the iteration, which is a new key on `p-while`/`p-for` and a careful
look at what `next` skips — I sized it as "measure how many rows actually close
over a head-`my` first". No ask unless you want it scheduled.
