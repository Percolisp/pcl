# Variable handling in v2 — design review (s379, Fable)

**The ask (user, s379):** a big review of the variable-handling code —
hoisting, declarations, renaming — with simplification ideas.  "It seems
really complex now?"

**Verdict up front:** yes, and measurably.  ≈4,450 lines — **51 % of
`Pl/Parser2.pm`** — is variable identity, renaming and requalification.
The complexity is *essential in its causes* (Perl's scoping vs CL's
reader/packages genuinely collide in four distinct ways) but *accidental in
its form*: the same three questions — "what binds this name here?", "where
does this name appear inside this string?", "may this `my` stay a plain
`let`?" — are answered by **at least five scope resolvers, 26 hand-rolled
interpolation-scanning sites, and six rename families with three
near-duplicate veto predicates**.  Every recent bug in this area (#239's
`$Foo::bar` over-rename, #237's dropped subscripts, the s379 signature
miss, #265/#272's scope-blind vetoes) is a *disagreement between two copies
of one of those answers*.  The fix direction is not "fewer features" but
"one copy of each answer".

Companion data: the full function-by-function inventory (names, line
ranges, callers) was compiled this session and is summarized in §1; the
numbers below come from it.

---

## 1. The map — what exists today

Eight subsystems touch variable identity, in pass order inside `parse()`:

1. **Requalification** (two passes, ~465 lines): in-block `package X;`
   re-homes `our`-aliases (pass 1) and bare globals (pass 2, #239) by
   rewriting SOURCE spellings to qualified names, because CL's reader
   interns a top-level form's symbols before a nested `(in-package)` runs.
   Owns the newest and most correct scope resolver (`_binding_at` +
   `_stmt_binding`/`_head_binding`/`_decl_binding_in`/`_pkg_in_effect_at`).
2. **Span machinery** (W10, ~700 lines): a file lexical used across
   package-segment boundaries can't live in one `let` (segments are
   separate top-level CL forms), so it is renamed to `$x__file__N` and
   defvar'd — `_rename_spanning_lexicals` alone is **458 lines**, the
   largest function in the compiler.  Gate: `_check_my_spanning`.
3. **Capture machinery** (W5, ~900 lines): named subs are HOISTED to
   top-level `defun`s outside the `let`s (for BEGIN/compile-time
   visibility), so a captured file lexical must be promoted to a defvar'd
   cell (`_promote_captured`, `_block_captures_name` = 181 lines of
   per-canon regex sets).  Gate: `_check_sub_captures`.
4. **The rename suffix zoo**: `__file__N` (span/capture), `__state__N`,
   `__cond__N` (my in a condition colliding with a global name),
   `__shadow__N`, `__emb__N` (embedded my vs same-named global), plus v1's
   `__lex__N` still minted through the seam.  Six families, five counters,
   **three near-duplicate veto predicates** (`_shadow_rename_blocker`,
   `_state_container_blocker`, `_state_rw_blocker`), two decl counters
   (`_hard_decl_count`, `_count_name_decls`), two lexical-name collectors
   (sigil-aware and not).  The eval-capture alist strips five suffix
   families to restore user-visible names for string eval.
5. **Interpolation scanning**: **26 sites** read or rewrite variable
   references inside quote/heredoc/regex text.  ONE is a real parser
   (`Pl/PExpr/StringInterpolation.pm`, 1,178 lines, correct); the other 25
   are ad-hoc regex approximations of it — `_interp_canon`,
   `_interp_names`, `_interp_fixer` (10 substitution regexes with
   backslash-parity and `(?!::)` guards), `_block_captures_name`'s
   ~12-alternation `%canon_pat`, `ExprToCL::_gen_interp_regex_pattern`
   (the #237 culprit), `ExprToCL2::_string_literal_form`, three scanners
   inside VarAnnotator, and ~15 single-purpose `\Q$bare\E` text scans.
   The non-interpolating-heredoc test is duplicated verbatim in 3 places.
6. **Forward global declarations** (~126 lines): `_forward_global_decls`
   discovers which globals need `(defvar …)` by **regex-scanning the
   emitted CL text** of a section, after `_blank_string_innards` blanks
   string literals character by character.  A scanner over generated code —
   the compiler asking its own output what it just did.  (Known false
   positives: task #66.)
7. **Lowering-time identity**: `_reg_lex`/`_live_lex`/`_let_bound_vars`
   (v1-mirroring, one never shrinks), `_file_lex_renamed` branches inside
   `_lower_block`, hoist gating, `our`-shadow dies.
8. **VarAnnotator** (1,372 lines): boxing analysis (may this my-scalar be a
   raw CL slot?).  Not identity itself — but it is **keyed on token text**,
   which is why every rename pass must run before it, which is why the pass
   order in `parse()` is a 12-step tower with comments explaining why each
   step precedes the next.

## 2. Root causes — why it grew this shape

**R1 — there is no symbol table.**  The compiler never computes "this token
is a use of binding B" once.  Each pass re-derives binding facts from the
raw PPI tree with its own walk, its own shadow test, its own notion of
scope.  So every new semantic corner (state, embedded my, poisoned
condition my, package switch, signatures…) adds a *pass* with a *private
resolver*, and the resolvers drift.  The s379 signature bug is the
canonical failure: the newest resolver didn't know what the seam's binder
knew.  #265/#272 (scope-blind veto) were the same story a month earlier.
The standing rule "detector and rewriter share ONE resolver" keeps being
re-earned pairwise; a symbol table earns it globally.

**R2 — interpolated text is opaque.**  A Perl string/regex is one PPI token,
so renaming `$x` means regex-editing token text everywhere it might appear.
Since a correct rewrite needs a correct parse of interpolation syntax,
every scanning site re-approximates StringInterpolation.pm, and each
approximation has its own bug tail (`$Foo::bar` was fixed in TWO scanners
in s378; the 24 others each still have their own subset of the rules).

**R3 — renaming instead of scoping.**  Distinct Perl bindings with the same
name land in one flat CL namespace whose global cells are `defvar`'d
special variables.  Three consequences drive most of the machinery:
  - a hoisted `defun` can only see a file lexical through a global cell →
    capture promotion + `__file__N`;
  - segments are separate top-level forms → span promotion + `__file__N`;
  - `defvar` proclaims the SYMBOL special, so a *plain* `let` of `$x`
    anywhere in the file silently becomes a dynamic rebinding of the global
    → the whole "poisoned my" family (`__cond__N`, `__shadow__N`,
    `__emb__N`, task #205) exists to keep user `my`s away from defvar'd
    names.
  The first two are forced by CL's model.  **The third is not** — see §6.

**R4 — the v1 seam.**  `__lex__N` and the `_let_bound_vars` mirroring exist
because anon subs/signatured subs still lower through v1's expression seam.
Every identity decision must round-trip through text the seam re-parses.
This is known (#153/E5 territory) and shrinking.

## 3. Direction A — bind once: a symbol table pass (the structural fix)

Build ONE early pass — after the normalization prepasses, before
everything else — that walks the PPI tree once and records, for every
variable reference (Symbol, ArrayIndex, code-level `${x}`, and
interpolated occurrence via §4's scanner):

```
ref → { binding_id, kind: my|state|our|global|special|param,
        decl_node, scope_node, perl_package, sigil, canon }
```

Then each consumer becomes a query instead of a walk:
- **span** = a binding whose refs cross a segment boundary;
- **capture** = a ref inside a sub body whose binding's scope is outside it;
- **requalify** = a spelling decision from `binding.perl_package` (the
  four-way resolver is just `kind`);
- **poisoned/embedded/cond** = a `my` binding whose canon collides with a
  `global` binding in the same file;
- **forward decls** = the set of `global` bindings, computed from the
  table — the emitted-CL text scanner (§1.6) deletes outright;
- **VarAnnotator** keys on `binding_id` instead of text, and the entire
  "every rename must precede the annotator" ordering tower dissolves.

Renaming stops mutating source: the EMITTER prints each binding's chosen
name (`$x`, `$X::x`, `$x__file__3`) at output time.  One uniquifier, one
counter, reasons as metadata.  The 10-substitution `_interp_fixer` is
replaced by span-splicing (§4).

This is not a new idea for this codebase — it is the `Facts` structure of
`docs/v2-target-architecture.md` finally applied to names, and it is the
same shape as the W12 VarAnnotator rewrite (text scan → tree walk) that
already succeeded once.  **Migration is the W12 recipe, not a flag day:**

1. Build the binder + table.  Run DUAL: assert its verdict against
   `_binding_at`, `_ref_shadowed`, and `_block_captures_name` at every
   call site over the corpus + Pl/t + board (disagreement = die with both
   verdicts).  Ship inert.
2. Port consumers one at a time, newest first (requalifier → capture
   gate+promoter → span → state/cond/shadow/emb).  Each port deletes its
   private resolver and its veto twin.  Gate per port: corpus-diff
   byte-identical + gate + the port's own family tests.
3. Move name-choice to emission; delete the source-mutation machinery.

Step 3 is the big win (it deletes most of §1.2–1.4's rewriting) but steps
1–2 pay for themselves alone: every resolver disagreement found by the dual
run is a live bug of the s379-signature class.

**Cost estimate:** step 1 is one focused session (the resolver logic
already exists in `_binding_at` — it becomes the reference
implementation); step 2 is 3–5 sessions of ports; step 3 rides on #153/E5's
emitter work.  Deletion potential: ~1,500–2,000 lines of Parser2 plus the
ordering-comment tower.

## 4. Direction B — one interpolation scanner (immediate; #237 is the entry)

Extract from StringInterpolation.pm an **event scanner**: given
interpolating text, yield `(sigil, name, subscript-chain, span)` for every
variable reference, with the qualified-name rule, escape parity, braces,
`$#`, and postderef handled ONCE.  Consumers:

- dq-strings/heredocs (current parser keeps its role — the scanner is
  extracted from it, not written beside it);
- **regexes**: same events, plus the `intuit_more`-style bracket classifier
  ruled for #237 (`docs/fable-answers-s378.md` §3) deciding
  subscript-vs-charclass per event.  `_gen_interp_regex_pattern`'s
  hand-rolled walk deletes;
- the rename/requalify machinery: `_interp_canon`, `_interp_names`,
  `%canon_pat` become filters over events; `_interp_fixer`'s regex
  substitutions become span splices — the backslash-parity/`(?!::)`/`\b`
  guard class of bugs (three shipped fixes and counting) becomes
  unrepresentable;
- `ExprToCL2::_string_literal_form` and VarAnnotator's three quote
  scanners consume events.

This is independent of Direction A and worth doing first — **#237's ruled
fix IS its first consumer**, so the scanner should be built as #237's
mechanism rather than as a separate project.  Retires ~15 of the 26 sites;
the ~10 single-purpose `\Q$bare\E` existence checks can migrate lazily.

## 5. Direction C — one promotion engine (near-term, mechanical)

Even before A: `__file__`, `__cond__`, `__shadow__`, `__emb__` all mean
"this `my` cannot remain a plain `let`; give it a unique global cell and
rewrite its extent".  Today that is four passes, three blocker twins, four
counters, four eval-alist registrations.  Fold into one
`promote(decl, reason)` engine — one blocker predicate with capability
flags, one counter, one alist hook; reasons become metadata on the renamed
cell (and the suffix can become uniform, e.g. `__pcl__N`, once nothing
keys on the family name — the eval-alist strip list is the only consumer
and it already strips all of them identically).  `state` keeps its distinct
cell semantics but shares the rewriter it already shares
(`_rename_decl_within`).

## 6. Direction D — dissolve the poisoned-my family (measured proposal)

R3's third consequence is the one that can be REMOVED rather than
reorganized.  Today `(defvar $x …)` for a global proclaims `$x` special
file-wide, so a user's innocent `my $x` in the same file lowers to a `let`
that DYNAMICALLY rebinds the global — perl's lexical shadow silently
becomes a dynamic one, visible to every sub called in the extent.  The
`__cond__`/`__shadow__`/`__emb__` renames and task #205 exist solely to
keep `my`s away from that trap.

SBCL has the exact tool: **`sb-ext:defglobal`** — a global value cell with
NO special proclamation.  Under defglobal, a `let` of `$x` is a plain
lexical binding that shadows the global for code in its scope — which is
perl's semantics, for free.  The poisoned-my family (and its vetoes, and
its share of the suffix zoo) dissolves.

What stands in the way, measured this session:

- **`local` today NEEDS special.**  `local $v = 2` lowers to
  `(let (($v (p-box-for-local 2))) …)` — a dynamic rebinding.  Under
  defglobal this must become explicit save/install/restore:
  `(let ((old $v)) (setf $v (p-box-for-local 2)) (unwind-protect …
  (setf $v old)))`.  **Probed s379: the new-box shape is the faithful
  one** — perl's `local` installs a NEW container (a ref captured before
  the `local` keeps reading the OLD value during the extent; PCL already
  matches this today).  Save/restore of the *slot* preserves exactly that.
  `unwind-protect` restores on die/goto as perl does.
- **Dynamic localization** (`local ${$name}`, `p-local-glob`) must go
  through `(setf symbol-value)` save/restore — works on defglobal cells.
- **Threads**: defglobal cells are not per-thread; PCL is single-threaded
  today.  Record as a constraint, not a blocker.
- **Migration hazard**: every `let` of a global name in EMITTED code that
  currently *relies* on dynamic visibility (if any exists beyond `local`)
  breaks silently.  The discriminating measurement before any work: audit
  emitted `let`s of defvar'd names across the corpus (mechanical grep of
  generated CL) and classify each as local-lowering vs poisoned-my vs
  deliberate.  If a third class exists, this direction needs a redesign of
  that class first.

Payoff if it survives measurement: `__cond__`/`__shadow__`/`__emb__`
passes delete (~300 lines + their vetoes), #205 closes by construction,
and the "my near a global name" bug class — which has produced at least
four tasks (#205, #254's second family, #265, #272) — becomes
unrepresentable.  This is a Fable-design item that changes the emitted
contract (`docs/ir-spec.md` §load model must be updated in the same
commit).  **Per the s379b USER sign-off rule (DECIDED.md), no user ask is
needed if the measurements show it simpler + clearer + generated code
faster-or-unchanged + compile time < 50 % worse** — defglobal reads are if
anything cheaper than special-variable reads, so the runtime leg is
expected to pass; the audit above is the go/no-go.

## 7. Hoisting — shrink the reason, not just the machinery

Named subs are hoisted to top-level `defun`s inside `eval-when` so BEGIN
blocks and `use` can call them (CLAUDE.md §3).  Hoisting is what turns
"sub captures file lexical" from a closure (free in CL) into a promotion
problem (§1.3).  But the *reason* applies to few subs: only those actually
called at compile time.

Proposal to size: hoist ONLY subs that (a) some BEGIN/`use`-position code
in the file references, or (b) are exported/named in package installs the
compile phase needs; define the rest **in place** as
`(setf (fdefinition 'pl-f) (lambda …))` inside the section's `let`s, where
captures are ordinary closures and no promotion is needed.  The
discriminating measurement (cheap, one corpus pass): count subs per file
that are compile-time-referenced.  If the typical file needs 0–2 hoists,
most of the capture machinery stops firing for most files — the code
stays (spanning still needs it) but the hot path narrows and whole
bug-classes (eval-visibility of promoted cells, promotion order, forced
`__file__N` naming) stop applying to ordinary code.  Interaction to probe:
`p-declare-sub` forward stubs, recursion between in-place subs across
sections, and SBCL's compile-quality difference between top-level `defun`
and installed lambda (Target A says measure, not assume).

## 8. What NOT to do

- **No more per-scanner fixes.**  The s378 `$Foo::bar` fix was applied to
  two scanners; 24 others still lack it.  The next interpolation bug gets
  fixed in the §4 scanner or not at all — patching a copy is rule-11 debt
  with interest.  (Ask-4 of the s378 review is APPROVED as shipped —
  correct and necessary — but it is the last of its kind I want to
  approve.)
- **No new suffix family.**  Any new "this my can't stay a let" reason
  goes through §5's engine.
- **No new scope walk.**  A pass that needs binding facts uses
  `_binding_at` (today) or the §3 table (once it exists).  A diff adding a
  fresh `for (my $p = $node->parent; …)` scope loop is the smell.
- **Don't start §6/§7 without their measurements** — both are
  probe-first items with explicit discriminating measurements named above.
  (Per the s379b USER sign-off rule they need no ask once the
  measurements show simpler + clearer + generated code
  faster-or-unchanged + compile time < 50 % worse; a result that SLOWS
  generated code goes back to the user.)

## 9. Sequencing (USER-ruled s379c: IR changes go BEFORE v0.1)

The organizing rule (USER, s379c): **the emitted CL must be stable at the
first release** — anything that changes the intermediate code's shape lands
before the v0.1 tag; IR-neutral internal work may land any time.

1. **#237 via the shared scanner** (§4 = `fable-answers-s378.md` §3's b′),
   **SPLIT (USER s379c)**: Fable builds the scanner core + the intuit_more
   bracket classifier + its probe table; Opus wires the consumers
   (regex, dq, rename machinery) and the guard rows.  1–2 sessions.
2. **#287** (sort pair, ruled `fable-answers-s378.md` §2) — 1 session.
3. **The pre-v0.1 IR batch** — every emitted-shape change, together:
   - §6 defglobal measurement → implement if the s379b conjuncts hold;
   - §7 selective-hoisting measurement → same bar;
   - the #281/#75-scoped macro-vocabulary pass over the generated code.
   Only a generated-code slowdown or a semantic surprise goes back to the
   user (s379b rule; compile budget per change, drift watched against the
   s379 ~65 s whole-corpus baseline).
4. **v0.1 mechanical track** (#277–#280, #282, #283) → tag.  The IR is
   frozen at whatever step 3 produced.
5. **Post-v0.1, IR-neutral**: §3 binder + dual-run and its ports (byte-
   identical by construction, corpus-diff-gated), §5 promotion-engine fold,
   #153 FOLD chunks 2–3.  §3 step 3 (emission-time naming) changes the IR
   and is v0.2 material unless it is pulled into step 3's batch.
