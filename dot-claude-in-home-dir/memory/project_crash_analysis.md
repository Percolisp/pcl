---
name: Sweep Crash Analysis (session 126)
description: Root causes for each crashing/partial test file in the sweep, with fix complexity
type: project
originSessionId: b92fb35e-e1da-49ca-a2c5-2ce3e85327c6
---
Last updated: session 126 (2026-04-10). Sweep state: 7843 passing, 35 fully-passing.

**Full analysis in `docs/test-failures-categorized.md`** — this is a pointer/summary only.

## Quick-Win Stubs

- **`alarm(N)` no-op** → fixes readline.t crash (unlocks 18 tests). 1-liner in pcl-runtime.lisp.
- **`loop {}` keyword** → fixes my.t crash. Treat `loop` as `while(1)` in parser.
- **`evalbytes` stub** → fixes lex.t crash (4 hidden tests).
- **`my sub` lexical subs** → fixes sub.t crash at test 17 (`PL-NOT_CONSTANTM`).

## Crash Clusters

- **Auto-viv write-back (Hard)**: ref.t, array.t, grent.t — `push @{$arr[N]}` doesn't write back
- **Tie/DESTROY (Not worth pursuing)**: state.t, or.t, hash.t
- **Dynamic label scoping (Hard)**: loopctl.t — `last LABEL` from inside called sub
- **Missing builtins (Medium)**: closure.t (PL-READ), defins.t (defined(BAREWORD_FH))
- **Prototype arg-limiting + shift hang**: bop.t — also documented hang
- **Pre-existing baseline crash**: sprintf2.t (1420+9/crash at baseline bbbbfc0)

## Partial Clusters

- **fresh_perl_is (subprocess)**: blocks.t, die_exit.t, print.t — all use subprocess, silently return
- **Lvalue/wantarray**: kvhslice.t, time.t, split.t — deferred
- **@ISA/indirect-object**: bless.t, method.t — medium fix
- **Regex object identity**: qr.t — CL objects don't have Perl SV address identity

## Files Not Worth Pursuing

args.t (@_ aliasing), caller.t (stash+eval), chdir.t (XS), each.t (Hash::Util),
hash.t (DESTROY+tie), join.t (overload), lc.t (locale Unicode), length.t (use bytes),
pack.t (many formats), undef.t (read-only+stash), wantarray.t (wantarray deferred)

## Next Quick Wins (in order of impact)

1. Stub `alarm(N)` → readline.t (18 hidden tests)
2. Implement `loop {}` → my.t (unknown hidden tests)
3. Fix `my sub` → sub.t tests 17-18
4. Fix `do.t` tests 9-10 (bare-if return) — see docs/v1-implementation-plan.md B1
5. Fix `@A::ISA = scalar` coercion → bless.t (88 hidden tests)
