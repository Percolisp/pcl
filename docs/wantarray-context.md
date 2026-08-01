# Wantarray / Context System

## User Preference
**Work authorized 2026-05-29 (session 215).** The user previously asked to skip
wantarray/context work across many sessions; that prohibition is now lifted and
active work on this area is expected. (History kept for context.)

## What It Is
Perl has a concept of "context" — functions can detect whether they're called in scalar, list, or void context via `wantarray()`:
- `wantarray()` returns true in list context
- `wantarray()` returns false (but defined) in scalar context
- `wantarray()` returns undef in void context

## Current State
- `*wantarray*` special variable exists in pcl-runtime.lisp
- Some call sites set `(let ((*wantarray* t)) ...)` for list context
- Many call sites don't set context correctly
- This affects: `@arr = func()` vs `$x = func()`, return values, etc.

## Known Issues (now in scope for fixing — authorized 2026-05-29)
- Functions returning arrays in scalar context should return count
- Assignment context detection incomplete
- Many Perl test failures are caused by incorrect wantarray propagation

---

## The rule PCL is missing: **a phase block is not a runtime statement**

*(found s319 via `t/op/context.t` t8; task #164.  Recorded here because the
lookup order lands here, and because the A/B probe below is the whole proof.)*

`BEGIN`/`END`/`CHECK`/`INIT`/`UNITCHECK` blocks run at their phase and
contribute **no runtime value**.  Perl therefore does not count them when
deciding which statement is a block's **tail** — the statement whose value
becomes the block's value and which inherits the caller's context.

PCL does count them, so a phase block written *after* the real last statement
silently demotes that statement to **void**:

```perl
sub context { $cx = qw[void scalar list][wantarray + defined wantarray] }

$_ = sub { context(); BEGIN { } }->();   # perl: scalar   PCL: void    <-- BUG
$_ = sub { context();           }->();   # perl: scalar   PCL: scalar  <-- correct
```

The A/B pair matters: it proves the tail/void classifier is **right in
general** and wrong only in the presence of a trailing phase block.  So the fix
is not "rework context propagation" — it is one rule, applied in the one place
that picks a block's tail statement:

> When selecting a block's tail statement, skip trailing phase blocks
> (`BEGIN`, `END`, `CHECK`, `INIT`, `UNITCHECK`).

Keyed on the *mechanism* (compile-time-only block), never on a name list at a
call site — CLAUDE.md 11.

**Why it is worth more than one test row.**  The failure is **silent** and
value-changing: any sub whose body happens to end with a phase block returns
the wrong thing and reports the wrong `wantarray`, with no diagnostic.  That is
the same family as the #138 silently-deleted statement.  `BEGIN` at the end of
a sub is rare in hand-written code but ordinary in generated code and in
modules that install things at compile time.

**Sequencing (do not start blind).**  This lives in the same machinery as:
- the **CLAUDE.md §8 regression** (the VOID_CTX sub-body wrap in
  `_process_expression_statement` is too broad) — CLAUDE.md says fix that
  *before* further wantarray work; and
- **task #161** (a void-context `eval STRING` reports scalar, because nothing
  binds `*wantarray*` to `:void` at a void call site).

All three are "who decides void-ness, and where".  Fix §8 first, then re-probe
#164 and #161 — they may share a classifier and one fix may move all three.
Verify with the full gate **and** sweep, never the probe alone: a tail-position
rule changes every sub's return value.
