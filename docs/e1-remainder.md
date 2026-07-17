# E1 Remainder — the last 22 whole-file gates (survey, s283)

> **UPDATE (s295, 2026-07-18, Fable 5): M-F eval family SHIPPED — census
> 104 native / 7 gated, gen v2-35.**  De-gated **eval.t** (126+34/163 —
> BEATS v1's 121+39 with a strict-SUBSET fail set, fixes t27/28/81/84/97)
> and **ref.t** (183+19/245, IDENTICAL fail set = exact parity) via the
> **alias rule** (normative: **ir-spec §9.1**): renamed file cells become
> string-eval-visible under their ORIGINAL names through
> `(p-alias-eval-cell '$x $x__file__N)` at the decl's run position —
> writing the cell into the original-name global, v1's one-storage-location
> lookup model — plus site-alist pairs for let-bound shadows
> (innermost-first) and package-qualified span pairs for cross-package
> sites.  Only emitted when the file has a string eval
> (`_file_has_str_eval`): eval-free files byte-identical.  Dynamic
> `eval $code` (the HARD REQUIREMENT) is native — nothing inspects the
> eval text at compile time.  The s294 registry interim design (and its
> structural stale-shadow regression) was replaced before commit; also
> fixed `_enclosing_lex_decl` blindness to already-renamed enclosing decls
> (promotion order no longer decides the outer-my refusal — encl probe).
> Gates: v2 ALL PASS, PCL_V1 = known 7 v2-only set, v1 corpus byte-identical.
> **Remaining 7 gates: chdir (M-E #55), lfs (bless as residue, #56), array
> (#63), postfixderef (#45), state + signatures (#56), closure
> (per-iteration closure binding + own remaining decl gate).**
> s295 detail in `docs/session-log.md`.

> **UPDATE (s292, 2026-07-16, Opus 4.8): M-E substr.t SHIPPED — census
> 102 native / 9 gated, gen v2-34.**  De-gated **substr.t** (OK 374/8/397,
> IDENTICAL fail set to v1 = exact parity) — the `for (substr($x,…))`
> magic-lvalue foreach gate.  `_alias_box_form` already head-swapped to
> `p-substr-lvalue-cell` (the gate was residue; the void-wrap heap blocker it
> cited was fixed s288).  Two GENERAL fixes it needed: (a) **nested-sub
> bareword registration** — the sub pre-pass stopped at a top-level `sub
> run_tests` and never registered `sub bar` nested inside, so `is(bar,…)`
> resolved to the string "bar" not `(pl-bar)`; now recurses
> (`($child,@{find})`).  (b) **magic-lvalue-arg force-box** — VarAnnotator
> `_ev`s the scalar arg of `for(substr/pos/vec($x,…))` so `$x` stays boxed and
> the write-through cell works (top-level `$x` was raw-slotted).  corpus-diff:
> only substr.t.  s292 detail in `docs/session-log.md`.

> **UPDATE (s291, 2026-07-15, Opus 4.8): M-B session 3 SHIPPED — census
> 101 native / 10 gated, gen v2-33.**  De-gated **scalar.t** (OK 81/35/12
> of 128, IDENTICAL fail set to the v1 fallback = exact parity) by dropping
> the STAGED "interpolated use" refusal in `_rename_spanning_lexicals` (the
> M-A interp fixer rewrites cross-package interpolated uses to the QUALIFIED
> cell `main::$x`, runtime-verified) + fixing two GENERAL v2 bugs it exposed:
> (a) `require Module` nested in a block/sub now stays INLINE (v1's
> `_block_depth>0` branch — `_fallback_stmt_capture` reflects the statement's
> real PPI block-nesting, so `SKIP:`-guarded `require B`/`require threads`
> don't load unconditionally at file top and crash on XS); (b) paren-form
> `print($fh LIST)` keeps its filehandle across v2's analysis+emission
> double-parse (`_extract_paren_filehandle`'s destructive `$first->remove`
> now self-heals via a returned `[$first,$anchor]` token the caller
> re-inserts).  parser2-01 t54 (asserted the dropped refusal) updated; +3
> regression tests.  s291 detail in `docs/session-log.md`.

> **UPDATE (s290, 2026-07-14, Fable 5): M-B session 1 SHIPPED — census
> 100 native / 11 gated, gen v2-32.**  De-gated **sort.t** (202/2/1 of
> 205, SAME failing tests 170/177 as the v1 fallback — exact parity) via
> per-declaration span instances (scalar loop innermost-first with
> re-scanned facts; any-instance-spans → rename-all), canon-exact
> container uniqueness/span-test/rewrite (`canon_decl_count`), the
> `_rewrite_unreachable_gotos` pre-pass (goto-in-sub/sort-block with no
> in-barrier label → perl's runtime `die "Can't find label …"`), the
> per-section forward-decl exclusion (`_seg_lex` — sort.t's global `@a`
> was left unbound by the file-wide `_all_lex` rule), and the on-demand
> file-top `p-declare-sub` stub sweep (cross-section forward sub calls —
> v1 stubs everything at top).  **STAGED for M-B session 3: the interp
> fixer is wired into the span rename loops but the "interpolated use"
> refusal is restored** — dropping it de-gates scalar.t at 78+36/128
> PARTIAL (early stop after t126, new fail t64) vs v1's 81/35/12: debug
> that divergence first, then remove the one `next if` (marked STAGED in
> `_rename_spanning_lexicals`).  **ref.t re-triaged → M-F eval family**
> (its `$test` clears; it re-gates on `$x` named in a string eval —
> eval-unsafe is a correct refusal until the s250 capture alist carries
> renamed cells).  s290 detail in `docs/session-log.md`.

> **UPDATE (s289, 2026-07-14, Fable 5): M-A SHIPPED — census 99 native /
> 12 gated, gen v2-31.**  De-gated **pack.t** (5638/87, SAME 87 test
> numbers as the v1 fallback) and **yadayada.t** (21/15 parity).  Three
> mechanisms: (1) the M-A interp rewrite — `_interp_fixer` /
> `_fix_interp_token` factored out of `_rewrite_var_uses` and wired into
> `_rename_decl_within`, so ALL `_shadow_rename_blocker` callers
> (poisoned cond-my, seam my-shadow, state-in-named-sub) now rename
> interpolated uses instead of refusing (`${x}` brace-deref still
> refuses); (2) oversized-extent flattening — top-level `my` decls whose
> post-decl source remainder > `$RUN_NEST_MAX` (20k) are force-promoted
> to defvar cells (v1's model) via `_promote_captured`, because pack.t's
> nested-let emission was ONE 162k-char form that OOMs SBCL's register
> allocator; `_gate_oversized_run_form` ($RUN_FORM_MAX = 64k) makes the
> OOM class unreachable by construction (flattening also re-emitted
> split/sprintf/sprintf2 — exact HEAD parity verified); (3)
> `_premerge_include_prototypes` — cross-require prototypes (test.pl's
> `sub is ($$@)`) are merged BEFORE sub lowering / VarAnnotator
> pre-parse, fixing a silent v2 wrong-context class (`is($be,
> reverse($le))` list-reversed in v2-lowered sub bodies at HEAD).
> **sub.t's M-A table row below was stale — it de-gated in s285.**
> M-A's job is done; ref.t/scalar.t still need M-B (their interp is
> inside shadow scopes — the span path's per-declaration re-keying).
> s289 detail in `docs/session-log.md`.

> **UPDATE (s288, 2026-07-13, Fable 5): E2-prereq void-wrap hoist SHIPPED
> (task #60), gen v2-30 — census unchanged (97 native / 14 gated; the hoist
> de-gates nothing by itself).**  v2 sub bodies now carry v1's :void regime
> (one `(let ((*wantarray* :void))` around a multi-statement body,
> `wa_void_active` suppresses every per-statement bind, tail restores
> `*pcl-caller-wantarray*` at the leaf; single non-compound-statement
> bodies skip the regime).  **This removes the large-sub SBCL heap blowup
> — substr.t's E2 blocker is gone; its only remaining gate is the
> magic-lvalue foreach.**  Also: v2 regenerates `cl/pcl-pack.lisp`
> byte-functionally identically (pack.t 5638/87, same 87 test numbers);
> new task #64 (bare-block sub tail loses its value, BOTH pipelines,
> pre-existing).  Full sweep parity vs HEAD (only flaky print.t differed
> under load; passes standalone).  s288 detail in `docs/session-log.md`.

> **UPDATE (s287, 2026-07-12, Fable 5): M-E singles SHIPPED — census 97
> native / 14 gated, gen v2-29.**  De-gated **loopctl.t** (67/67, fully
> passing) and **my.t** (49/1, exact v1 parity on the pre-existing test-46
> failure).  Shipped: bare-block `continue { }` (labeled in-compound +
> PPI's orphan-sibling unlabeled form with glommed-trailing join — the
> unlabeled form was a SILENT v2 miscompile before: the continue block was
> dropped with only a PARSE-ERROR comment); **standalone label** lowered to
> `(tagbody :label <block-remainder>)` (backward `goto LABEL` = lexical
> `(go :label)`; value-position labels and FORWARD gotos stay gated — this
> replaces v1's text-level `_wrap_runtime_labels` for the v2 path); list-form
> self-ref init `my (undef,@bee) = @bee` (per-variable copy-binding dance);
> chained declarators `my @a = my @a = …`; container-capture de-conflation
> (`_hard_decl_count`/`_count_name_decls` sigil-aware for container canons —
> `my $x` beside `my %x` no longer blocks promoting `%x`).
> **array.t re-gates on `forward goto to a standalone label`**: line 663
> `map { …; goto aftermap; } @a; aftermap:` — goto out of a LAMBDA needs a
> dynamic unwind (throw), not lexical `(go)`.  NOTE: **v1 crashes on that
> shape today** (compile error on the `(go :aftermap)` inside the lambda),
> and array.t already stops at test 114 at HEAD — so the v1 fallback is
> byte-identical and nothing regressed; clearing array.t needs the dynamic
> goto mechanism (new design item, both pipelines), not an M-E single.
> Remaining M-E singles: chdir.t BEGIN-introspection, postderef #45.

> **UPDATE (s285, 2026-07-12, Opus 4.8): M-E element foreach-alias SHIPPED —
> census 95 native / 16 gated, gen v2-27.**  De-gated **chop.t, aassign.t,
> sub.t** (element `for ($h{k})`/`for ($a[i])` → `p-gethash-box`/`p-aref-box`
> head-swap; container already boxed → no annotator change).  Also fixed a real
> bug: bare `return;` was `(p-return (p-undef))` (1-elem list in list ctx) → now
> zero-arg `(p-return)`.  **substr.t re-gated on the narrower `foreach over a
> magic-lvalue element (substr/pos/vec)`** — it needs the scalar force-boxed AND
> its huge run_tests exhausts the sweep's 1GB heap via the per-statement
> `(let ((*wantarray* :void)))` wrap (425 vs v1's 30 — the CLAUDE.md #8 VOID_CTX
> over-broad wrap; fix = hoist the regime once + `wa_void_active`, an E2 task
> that unblocks every large file).  substr.t also loses 1 test on a user
> `:lvalue` sub (not-supported).  **Remaining M-E**: loopctl.t (`while…continue`),
> array.t (list-form self-ref init), postfixderef #45.  s285 detail in
> `docs/session-log.md` §285.
>
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

### M-A — Interpolated-text rewrite for renames — **DONE (s289, task #67)**

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
| ~~pack.t~~ | **DONE (s289)** | de-gated 5638/87, same 87 test numbers as the v1 fallback (also needed oversized-extent flattening + `_premerge_include_prototypes`, see s289 header note) |
| ~~yadayada.t~~ | **DONE (s289)** | de-gated 21/15, exact v1 parity |
| ~~sub.t~~ | **DONE (s285)** | cleared earlier via container capture work; this row had been stale |
| (ref.t) | see M-B | its `$test` is interpolated at 437 *inside the shadow scope* — M-A (now done) was the prerequisite for the mangled-path rename |
| (scalar.t) | see M-B | `$fh` appears in readline `<$fh>` text — same prerequisite; the rewriter's `_fix_interp_token` handles readline tokens (s289) |

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
| ~~my.t~~ **DONE s287** | was `standalone label` after s284 | de-gated 49/1 (v1-parity, pre-existing t46 fail) via the standalone-label tagbody lowering |
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
| ~~chop.t~~ **DONE s285** | element alias | de-gated: `p-gethash-box`/`p-aref-box` head-swap |
| substr.t | `foreach over a magic-lvalue element (substr/pos/vec)` | narrowed gate (s285): needs scalar force-boxed + 1 user-`:lvalue` test loss.  **The per-statement void-wrap heap blocker is FIXED (s288, task #60 regime hoist)** — only the magic-lvalue gate remains |
| ~~loopctl.t~~ **DONE s287** | bare-block continue | de-gated 67/67: the gate was the BARE-block `LABEL: { } continue { }` (while/until continue already worked); labeled in-compound + orphan-sibling join |
| array.t | ~~self-ref init~~ → `forward goto to a standalone label` (s287) | list self-ref + chained-my + capture de-conflation all shipped s287; residual = `goto` out of a map LAMBDA to a later label — needs a DYNAMIC goto (throw/catch), and v1 CRASHES on it today (compile error); file also stops at t114 at HEAD.  A design item, not a single |
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
