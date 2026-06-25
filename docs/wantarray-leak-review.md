# Review: the recurring `*wantarray*` leak

**Status:** mechanism consolidated 2026-06-25; a deeper position-based fix is
proposed below but NOT yet implemented (needs sign-off — it is a calling-convention change).

## Symptom (the bug that keeps coming back)

A *wantarray-sensitive* built-in returns the wrong shape when it sits in a
statically-known scalar position **inside a sub that was called in list context**:

```perl
sub f {
    my %h = (a => 1);
    my $e = each %h;          # WANT scalar each → the key "a"
    my @a = (10,20,30,40);
    my $s = splice(@a, 1, 2); # WANT scalar splice → last removed elem 30
    my $line = <$fh>;         # WANT one line, not all of them
    return "$e $s $line";
}
my @r = ( f() );              # f() called in LIST context
```

Before the fix, `each`/`splice`/`<$fh>` all saw **list** context and returned the
list form; `$e`/`$s`/`$line` then collapsed to a count / arrayref / first-of-many.

## Root cause

PCL models the caller's context with a **dynamic variable** `*wantarray*`.  A
sub entry binds it to the sub's *own* calling context (so `wantarray()` works).
The wantarray-sensitive built-ins read that same dynamic var at runtime:

```lisp
(defun p-each (collection)
  (if (eq *wantarray* t) <list: (k v)> <scalar: k>))
```

The problem: a built-in call buried in `my $x = …` is **statically** in scalar
context, but nothing re-binds `*wantarray*` to scalar there, so it reads the
*enclosing sub's* `*wantarray*` (which is `t` when the sub was called in list
context).  The dynamic var **leaks across the static context boundary.**

## The fix in place (operation-keyed wrapping)

The codegen knows each node's static context (`annotate_contexts` →
`get_node_context`).  For every wantarray-sensitive form it emits an explicit
rebinding so the leak can't happen:

```lisp
(p-my-= $e (let ((*wantarray* nil)) (p-each %h)))   ; scalar position → nil
(p-array-= @x (let ((*wantarray* t)) (p-each %h)))  ; list position   → t
```

Three syntactic forms produce these built-ins, so three codegen paths apply the
wrapper through one shared helper, `_wrap_wantarray_ctx($call, $ctx)`:

| form | gen function | which built-ins |
|------|--------------|-----------------|
| funcall `reverse(...)`, `each %h`, … | `gen_funcall` (via `%WANTARRAY_SENSITIVE`) | `reverse localtime gmtime caller unpack each splice` |
| `<FH>` readline | `gen_readline` | `p-readline` |
| `<pat>` file glob | `gen_glob` | `p-glob` |

`_wrap_wantarray_ctx` leaves `INHERIT_CTX` and **tail position** unwrapped, so a
built-in that *is* the tail expression of a sub still lets the real caller's
context flow through (and the `p-readline` macro's `*p-in-list-assign-rhs*` guard
for `while (($x)=<FH>)` still wins).

### The invariant (this is what was being violated each time)

> **Every runtime builtin that branches on `*wantarray*` must be listed in
> `%WANTARRAY_SENSITIVE` (or handled by its own node's gen function).**

The recurring bug was that this list was an ad-hoc regex
(`reverse|localtime|gmtime|caller|unpack`) that nobody updated when a new
wantarray-sensitive builtin (`each`, `splice`) or a new syntactic form
(`<FH>` in a scalar `my`) appeared.  The complete runtime consumer list today
(grep `*wantarray*` in `cl/pcl-runtime.lisp`, look for `(if (eq *wantarray* t)`):

`p-reverse · p-each · p-splice-impl · p-localtime · p-gmtime · p-caller ·
p-unpack · p-readline · p-glob · p-slice-result`

(`p-slice-result` and array/hash-in-scalar go through the slice/`p-my-=` paths,
which already annotate; `p-return-value` and `p-wantarray` intentionally read the
sub's own context and must NOT be wrapped.)

## Why this is still fragile — the deeper fix (TWO contexts)

Operation-keyed wrapping is **opt-in**: a missed builtin silently leaks, and the
only signal is a wrong answer in a list-context caller (rarely unit-tested).

The root confusion is that **one dynamic variable carries two different facts**:

- **(A) evaluation context** — the scalar/list/void context that *this
  expression position* imposes.  Changes constantly as you descend: the RHS of
  `my $x = …` is scalar even when the enclosing sub was called in list context.
  Read by the wantarray-sensitive **operators** (`each`, `<FH>`, slices, `@a` in
  scalar).
- **(B) return context** — the calling context of the *current sub*, fixed for
  the whole body.  Read by `wantarray()` and by the sub's implicit/`return`
  value.

These are genuinely independent, so they want **two variables** — this is the
clean fix (raised in review):

| concept | proposed var | set where | read by |
|---------|--------------|-----------|---------|
| (A) evaluation context | `*eval-context*` (today: `*wantarray*`) | rebound at every static-context boundary; **reset at sub entry** to the body default | `each`/`splice`/`<FH>`/`<glob>`/slices/array-in-scalar |
| (B) return context | `*return-context*` (today: `*pcl-caller-wantarray*`) | once, at sub entry = caller's eval context | `wantarray()`, `p-return-value`, the tail expression |

**PCL already has a *partial* split**, which is exactly why the bug is subtle:
`*pcl-caller-wantarray*` IS (B) — `p-wantarray` reads it so `wantarray()` is
correct.  But `p-sub` (cl/pcl-runtime.lisp ~L418) binds
`(*pcl-caller-wantarray* *wantarray*)` and then **leaves `*wantarray*` itself
untouched for the whole body** — so inside the body the evaluation-context var
(A) still holds the *caller's* context (B).  That single missing reset is the
leak: every scalar-position operator in the body sees the caller's list context
unless a wrapper rebinds it.

**The structural fix:** at sub entry, after saving (B), **reset (A) to the body
default** (void/statement context) instead of inheriting the caller's.  Then:

- a bare `each %h` / `<FH>` statement in the body no longer sees the caller's
  list context — (A) is determined purely by position;
- `p-return-value` and the tail expression must switch to reading **(B)**
  (`*return-context*`), since (A) is now reset;
- `%WANTARRAY_SENSITIVE` / `_wrap_wantarray_ctx` still set (A) for *non-default*
  positions (list-assignment RHS, function args), but a *missed* operator can no
  longer leak (B) into a scalar body position — it just falls back to the body
  default, which is the safe answer.

This is a small but cross-cutting calling-convention change (touches `p-sub`,
`p-return-value`, and the wantarray-sensitive operators), so it is left for a
dedicated change with explicit sign-off rather than folded into a bug-fix.  It is
the permanent cure; the `%WANTARRAY_SENSITIVE` allowlist below is the interim
containment.

Until then: **when you add a builtin that reads the evaluation context, add it to
`%WANTARRAY_SENSITIVE`** (or wrap its node's gen function with
`_wrap_wantarray_ctx`).

## Guards

`Pl/t/list-scalar-context-01.t` — `scalar each/splice/readline/reverse inside a
list-context sub` rows.  `Pl/t/fileio-02.t` test 22 also exercises the readline
case through glob filehandle aliasing.
