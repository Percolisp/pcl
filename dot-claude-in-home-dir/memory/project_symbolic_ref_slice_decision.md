---
name: project_symbolic_ref_slice_decision
description: "RESOLVED (s246): @{EXPR}[slice] fixed at the PARSER layer (option 3) — slice-vs-deref disambiguated by subscript position at parse time; codegen string-rewrite deleted"
metadata: 
  node_type: memory
  type: project
  originSessionId: 61f1f17b-6e7e-4006-a2c0-97c32e5d76c5
---

# Symbolic-ref slice codegen: RESOLVED session 246 (parser-level fix, option 3)

**The session-245 open decision is closed.** User confirmed the breakage to fix
was the `scalar @{$a[0]}`→1 regression and asked whether parse-time
disambiguation was possible — it is, completely: the subscript's **position**
(inside vs after the braces) is the discriminator, and PPI preserves it.
`@{$a[0]}` = Cast+Block (no third token); `@{EXPR}[1,3]` = Cast+Block+trailing
subscript. The ambiguity only ever existed in generated CL strings. Design doc:
`docs/symbolic-ref-slice-parse-fix.md`.

## What was done (session 246)
- **`Pl/PExpr.pm`**: in the `is_arr_or_hash_braces` dispatcher, a raw-token
  branch — `Cast('@'|'$')` + `{BLOCK}` + trailing subscript → slice/element
  node (`@…[`→slice_a_acc, `@…{`→slice_h_acc, `$…[`→a_ref_acc,
  `$…{`→h_ref_acc), base = `parse(BLOCK)`, whatever the block contains.
  Mirrors the pre-existing `%`-sigil kv patterns. The `is_var` heuristic
  remains only for brace-less `@$s[..]`/`$$s[0]` forms.
- **`Pl/ExprToCL.pm`**: the gen_prefix_op string-rewrite + `_is_symbolic_name`
  + `_slice_indices` + `_split_first_sexp` DELETED (incl. the committed
  `2bc25da` branch that carried the live regression). No base-sniffing exists.
- **Runtime**: unchanged — `p-aref` (s245, kept) and `p-gethash` (pre-existing)
  both resolve string operands symbolically; one path, ref-vs-string at runtime.

## Verification
Gate 92/3344 all pass; fuzzer 959/965 (only documented divergences);
misc-fixes-02.t 20/20 (test 20 = deref-guard pinning the regression);
ref.t fail-set identical modulo addresses + test 19 newly fixed (21→20 fails).

## Still open (separate small target)
`$ar->$#*` postfix last-index deref → pcl=undef (perl=2). Fix in the
postfix-deref arm of `Pl/PExpr.pm` (~line 1007 area).

Related: [[project_difftest_fuzzer]], [[feedback_fix_at_right_layer]],
[[feedback_dont_string_rewrite_codegen]].
