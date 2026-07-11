# E1 Remainder — the last 22 whole-file gates (survey, s283)

> **UPDATE (s284, 2026-07-12): M-C + M-D SHIPPED — census 92 native / 19
> gated, gen v2-26.**  De-gated: **hashassign.t, index.t, undef.t** (exact
> sweep parity).  The M-C hypotheses below were partly wrong (the CAPREFUSE
> diagnostics proved it): my.t was a canon-conflation false positive (now
> fixed, file re-gates on `standalone label` — a new M-E single); chdir.t's
> capture cleared, re-gates on `BEGIN sub-existence introspection`; aassign.t
> + sub.t captures cleared, both re-gate on the M-E `foreach over an
> aliasable lvalue element`; closure.t's real blocker is the **string-eval
> guard** (dynamic `eval $code` after the decl — joins the eval.t family,
> M-F).  Remaining 19 gates: M-A (pack, yadayada), M-B (ref, scalar, sort),
> M-E (chop, substr, loopctl, array, aassign, sub, my[label],
> chdir[BEGIN-introspection], postfixderef #45), M-F (state, signatures,
> eval+closure, lfs).  s284 detail in `docs/session-log.md` §284.

**State:** census **89 v2-native / 22 gated**, gen v2-25, gate 114/3979 PASS
(after session 283, commit cd9dd3b).  This doc is the per-file triage of every
remaining gate, grouped by the *mechanism* that clears it, so the next E1
session can start implementing instead of re-surveying.  Exact gate strings
were captured with `PCL_V2_VERBOSE=1` on 2026-07-11; refusal detail for the
span family with `PCL_SPAN_DEBUG=1` (`SPANHIT` + `SPANREFUSE` lines).

Estimates use the endgame plan's unit (a focused session).  E1's budget had
10–15 sessions; ~6 are spent (through s283).  The remainder below is
**4–7 sessions** — consistent with the plan, but these are the *hard residue*:
each family below needs a real mechanism, not per-file patching.

---

## The 22 files, by clearing mechanism

### M-A — Interpolated-text rewrite for renames (~1–2 sessions, clears 3 files, unblocks 2 more)

The single most repeated refusal: a rename pass (span rename, cond-my rename,
shadow rename, container promotion) refuses because the name appears inside
*interpolated text* (`"…$x…"`, regex, heredoc, readline `<$fh>`) which the
token-level rewrite cannot reach.  The mechanism already exists for one
caller: `_rewrite_var_uses`'s interpolation rewriter (capture promotion,
scalars).  Generalize it — **with shadow awareness** (never rewrite interp
text inside a scope that re-declares the name) — and expose it to the other
rename passes.

| file | exact gate | note |
|---|---|---|
| pack.t | `poisoned condition-my $base (interpolated use)` | `foreach my $base (split '', …)` at 224; the cond/loop-head rename refuses because `$base` is interpolated somewhere in scope |
| yadayada.t | `my-shadow of live lexical $err inside fallback block (interpolated use)` | shadow-rename refusal, same root |
| sub.t | `file lexical 'scratch' captured by sub a` | `my @scratch` + `"@scratch"` at line 94 — **container** interp is an element-join; either implement the join-aware rewrite or use the identity-name trick: a *file-unique* captured container could be promoted under its own name (like the span loop's unique path) so interp text keeps resolving — cheaper and proven |
| (ref.t) | see M-B | its `$test` is interpolated at 437 *inside the shadow scope* — M-A is a prerequisite for the mangled-path rename |
| (scalar.t) | see M-B | `$fh` appears in readline `<$fh>` text — same prerequisite, plus the rewriter must handle `QuoteLike::Readline` tokens |

### M-B — Per-declaration span tracking (~1–2 sessions, clears 3 files)

ref.t / scalar.t / sort.t all have the same deep shape (found s283): the
shadow re-decl sits at the **top of a flattened blk segment** (its block
contains a `package` statement), so the shadow *itself* spans segments within
its blk run.  The current span model is keyed by bare name
(`%live/%decl_seg/%spanning`), which conflates the outer variable with the
shadow; `_ref_shadowed`'s `at_seg` rule is deliberately blind to cross-segment
shadows (method.t's flattening rule).  Fix = track span candidates
**per declaration instance** (decl statement → its own extent, uses attributed
to the innermost declaring instance), then rename each instance independently.
M3's `_hard_decl_count` + shadow-skipping rewrite (shipped s283) are the
building blocks; the bare-name loop needs re-keying.

| file | exact gate | extra blockers |
|---|---|---|
| ref.t | `my-lexical 'test' spans a package boundary` | interp `"…$test"` at 437 (inside shadow scope) → also needs M-A |
| scalar.t | `my-lexical 'fh' spans a package boundary` | `<$fh>` readline text → also needs M-A; many `open my $fh` shadows (M2 already recognizes them) |
| sort.t | `my-lexical 'answer' spans a package boundary` | no interp — **cleanest first target for M-B alone** (decl 846, blk-segment shadow 866) |

### M-C — Shadow-aware decl counting in *capture* promotion (~1 session, likely clears 3–4 files; VERIFY FIRST)

The capture-promotion path (`_promote_captured` → `_count_name_decls`) still
counts *every* same-name decl in the extent — including decls inside named
subs and nested blocks, which are distinct shadowing variables.  M3 fixed
exactly this for the *span* path; apply the same model here.  **First step:
add `SPANREFUSE`-style diagnostics to `_promote_captured`** (it currently
refuses silently, so the per-file blockers below are hypotheses from source
inspection, not verified refusal traces).

| file | exact gate | hypothesis |
|---|---|---|
| closure.t | `file lexical 'i' captured by sub foo` | `my $i` at 17 + re-decls inside subs (39, 52, …) inflate the count; NOTE closure.t also contains the known `for my $n (0..4) { sub { $n } }` per-iteration-binding limitation (CLAUDE.md TODO) — de-gating may still leave those tests failing in both pipelines |
| my.t | `file lexical 'x' captured by sub foo` | file `my $x` + `++my $x->{foo}` inside sub foo3 (line 131) inflates count |
| chdir.t | `file lexical 'Saved_Env' captured by sub clean_env` | `%Saved_Env` container + hash-slice uses `@Saved_Env{@magic_envs}`; sigil-aware rewrite exists, count likely inflated or slice-form refusal |
| hashassign.t | `file lexical 'names' captured by sub in` | `my (%names, %names_copy)` — a **multi-container list decl**: the multi-decl promotion only handles all-scalar lists; extend to all-container lists |

### M-D — Block-scoped nested-sub capture = task #50 (~1 session, clears 3 files, fixes real v1 bugs)

“lexical '…' possibly captured by nested sub X” — a named sub *nested inside a
block* captures a block lexical.  Promotion is currently extent-scoped but the
hoist-scan gate (#44) still refuses these.  **Important: this family is not
just a de-gate — v1's fallback MISCOMPILES it** (s282b edge probes: block-
scoped `my @dup` captured by named sub → empty; block-scoped container capture
returns the global).  Fixing it in v2 removes silent wrong answers.

| file | exact gate |
|---|---|
| index.t | `lexical 'store' possibly captured by nested sub STORE` (tie handler in a block, line 277) |
| undef.t | `lexical 'hash' possibly captured by nested sub X::DESTROY` |
| aassign.t | `lexical 'a' possibly captured by nested sub f1` |

### M-E — True singles (~1 session for the first three; rest are design items)

| file | exact gate | mechanism |
|---|---|---|
| chop.t, substr.t | `foreach over an aliasable lvalue element` | native foreach must bind the loop var as an ALIAS to a live container slot (`for ($h{k})`, `for (substr(…))`); v1 uses `p-aref-box`/`p-gethash-box`/lvalue cells — port that binding into the native `p-foreach` lowering |
| loopctl.t | `loop with continue block` | `while … { } continue { }` (line 59/84, with `last`/`redo` from inside the continue): foreach already lowers `:continue` (`_continue_keys`); extend to the while/until branch and make loop-control exits from the continue block correct |
| array.t | `self-referential init: my (undef,@bee) = @bee` | s282b shipped self-ref init for the single-container form; extend to LIST-form decls (v1's "init in let binding" dance for lists) |
| postfixderef.t | `interpolated postfix deref (postderef_qq)` | task #45 / plan E1.4: implement `postderef_qq` in `StringInterpolation.pm` — fixes BOTH pipelines (v1 has the same gap) |

### M-F — Design items / blessed-residue candidates (decide, don't grind)

| file | exact gate | assessment |
|---|---|---|
| state.t | `state in anon-sub / map-grep-sort block (per-closure)` | real design work: each closure instance needs its own state cell — per-closure cell allocation in the lowering.  Worth doing (state is common in modern Perl), but it is its own mini-project |
| signatures.t | `state outside a block-level declaration` | `sub t126 ($c = (state $s = $z++)) {…}` — `state` inside a **signature default**.  Exotic; propose blessing as residue unless it falls out of the state.t work |
| eval.t | `my-lexical 'x' spans a package boundary` | genuine span + eval-by-name with 12 non-unique `'x'` decls; needs the s250 capture alist to carry **original-name → renamed-cell** pairs.  Already deferred to its own task (s282c decision).  Interacts with the dynamic-eval HARD REQUIREMENT (memory): the eval itself must keep working via v1 fallback until then |
| lfs.t | `file lexical 'big0' referenced in a END block` | `$big0` interpolated inside generated-code heredocs AND referenced in END; file is in the sweep's known-hang skip list, so a de-gate is unverifiable end-to-end today.  **Propose blessing as residue** (or fix only after the hang is solved) |

---

## Recommended order for the next E1 sessions

1. **Diagnostics first** (hours): `SPANREFUSE`-style warns in
   `_promote_captured` — converts M-C from hypotheses to a worklist.
2. **M-C + M-D** (1–2 sessions): both are "apply the M3 shadow model to the
   promotion path"; M-D also kills known v1 miscompiles (#50).  Expected
   +5–7 files.
3. **M-E singles** (1 session): foreach-alias, while-continue, list self-ref
   init.  Expected +4 files (chop, substr, loopctl, array).
4. **M-A then M-B** (2 sessions): interp rewrite, then per-decl span tracking;
   sort.t first (no interp), then ref.t/scalar.t.  Expected +3–4 files
   (+ pack.t, yadayada.t, sub.t from M-A alone).
5. **Decide the M-F residue** with the user: state.t (implement),
   signatures.t-state / lfs.t (bless), eval.t (scheduled own task).

End state: ~106–108 native + a small blessed residue — matching the plan's
"111 minus blessed-permanent" acceptance.

## Standing rules (unchanged)

Every de-gate: corpus byte-diff vs HEAD (worktree, normalize paths, strip
marker with perl) + `--jobs 1` sweep parity of changed files + full
`tools/prove-core` gate + cache-generation bump on emission change.  A
de-gate that loses even one test vs the v1 fallback is a regression, not a
win (bop.t in s283 was exactly this and needed the for-scope fix first).
