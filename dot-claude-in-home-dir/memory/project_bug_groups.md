---
name: PCL remaining bug groups (session 174)
description: Categorized list of remaining fixable bugs by subsystem, excluding wantarray (deferred), UTF-8/encode, pack/unpack, and sprintf flags
type: project
originSessionId: dc459e74-ee42-417b-a41a-003df03f113c
---
Baseline: 18196 passing, 40 fully passing (session 172). Groups ordered by estimated value.
Excluded: wantarray (deferred by policy), UTF-8/encode, pack/unpack, (s)printf flags.

**Why:** Produced session 173 as a working list of what's left to fix.
**How to apply:** Pick a group to tackle; each is self-contained. Start from Group 1 or 2 for highest ROI.

---

## Group 1: Hash semantics
- `scalar(%hash)` returns CL debug string instead of key count (each.t, hashassign.t)
- `%hash = (...)` in list context doesn't return the flattened list (hashassign.t ~13 failures)
- `each`/`keys` use different CL iteration orders (each.t test 3)
- `keys %h = N` bucket pre-allocation not supported (each.t tests 5,8,14–20)
- `each`/`keys`/`values` with 0 args gives wrong error message (each.t tests 40–42)
**Fix area:** `p-scalar`, `p-hash-=`, `p-keys`, `p-each` in pcl-runtime.lisp

## Group 2: String/substr bounds — ✅ MOSTLY FIXED (session 174)
- ~~`substr` OOB read/write warnings and errors~~ — FIXED: `p-substr` now warns/dies, end-pos fixed, 2-arg lvalue fixed, undef-len warns, ref-lvalue warns, 4-arg-as-lvalue gives "Can't modify substr". substr.t: 356/397 (+8).
- `substr` remaining failures: lvalue for-loop aliasing (tests 313-387, not-supported), tied scalar 4-arg write-back (test 142), large offsets SKIP block (tests 391-397)
- `chr(-N)` → U+FFFD: already works (returns replacement char)
- `vec` lvalue form (vec.t): not attempted
**Fix area:** `p-substr`, `p-chr`, `p-vec` in pcl-runtime.lisp

## Group 3: Sort subsystem
- Inplace `@a = sort @a`: `$a`/`$b` hold boxes instead of values (sort.t ~5 failures, tests 66–70)
- `wantarray` inside sort comparator returns list ctx instead of false (sort.t ~7, tests 56–62)
- Overloaded `<=>` / `cmp` not used by sort (sort.t ~3, tests ~88–90)
**Fix area:** `p-sort` in pcl-runtime.lisp

## Group 4: OOP / method dispatch
- `local @ISA` does not invalidate method cache (method.t tests 49–57, ~9 failures)
- `UNIVERSAL::AUTOLOAD` sets `$AUTOLOAD` in wrong package (method.t tests 97–99)
- `&{1}()` numeric symbolic ref dispatch (method.t tests 5–9)
- POSIX errno stub values wrong (`EINVAL` → "Operation not permitted") (bless.t)
- `bless $obj, $ref_ref` should die "Attempt to bless into a reference" (bless.t)
**Fix area:** `p-method-call`, `%pcl-dispatch-autoload` in pcl-runtime.lisp; POSIX stubs in lib/

## Group 5: Reference and regex objects
- `\(list_expr)` produces ARRAY ref instead of SCALAR ref to last element (bless.t, ref.t ~3)
- `ref(\$qr_obj)` returns "SCALAR" instead of "REGEXP" (qr.t ~5 failures)
- `qr//` numeric coercion — two qr objects get same address number (qr.t)
- NUL byte in symbolic refs (ref.t tests 87–113 — low priority / Perl internals)
**Fix area:** ExprToCL.pm for `\(list)` codegen; `p-ref` in pcl-runtime.lisp for qr// class

## Group 6: Parser / codegen bugs
- `grep { $hash_constructor }->{deref}` — block misread as hash constructor (grep.t tests 29,35,37)
- `for my Dog $spot (...)` type-annotated loop variable crashes (for.t tests 127, 129)
- `pos $_[N]` — subscript bleeds extra arg into `pos` call (pos.t tests 14–20)
- `j(1..12)` in function arg position: `..` evaluates as flip-flop instead of range (splice.t tests 2,4,6,8,10,12)
**Fix area:** PExpr.pm / Parser.pm

## Group 7: Loop control & control flow
- Dynamic loop labels `last $var` / `next $var` at runtime (loopctl.t tests 62–64)
- `$_` set in `continue` block not visible in test after loop (loopctl.t tests 49–53)
- `state $x` in map/grep block loses value between iterations (state.t tests 74–75)
- `goto state $label` computed goto (state.t tests 70–73)
- Flip-flop `..` in scalar context not implemented (flip.t, splice.t)
**Fix area:** ExprToCL.pm for dynamic labels; `p-foreach`/`p-map` for state in blocks

## Group 8: Local / dynamic scoping
- `local($a[5])` restore incorrectly trims array (local.t tests 119–120)
- `local *{$pkg}{method}` stash-slot syntax not supported (local.t tests 271–278)
- `local $_` interactions with filetest / default `$_` matching (local.t tests 255–264)
**Fix area:** `p-local-array-elem` restore logic in pcl-runtime.lisp; Parser.pm for stash-slot syntax

## Group 9: Numeric / arithmetic edge cases — ✅ FIXED (session 173)
- ~~Glob arithmetic: `$x = *foo; $x--` produces huge address instead of `-1` (auto.t tests 45,47)~~
  Fixed: `box-nv` returned `object-address(typeglob)` for `--` path (inconsistent with `to-number` raw path returning 0). Changed to always return 0 for typeglobs. Also removed p-typeglob from no-cache list.
- ~~`ord` of codepoints > 0x10FFFF returns 65533 instead of the value (ord.t tests 33–35)~~
  Fixed: added `p-superchar` struct; `p-chr` returns `(make-p-superchar :code N)` for N > 0x10FFFF; `p-ord` checks `p-superchar-p` first; `stringify-value` falls back to U+FFFD for p-superchar.

## Group 10: I/O / readline
- `$a .= <FH>` rcatline append (readline.t tests 4–7)
- SIGALRM interrupting `readline` (readline.t tests 16, 18)
**Fix area:** `p-readline` in pcl-runtime.lisp

## Group 11: Error/warning compatibility (low priority)
- Read-only array: push/unshift/delete should die with Perl's message (push.t, unshift.t, delete.t)
- `join(undef, ...)` should warn "Use of uninitialized value in join" (join.t test 18)
- Error-detection for invalid Perl: for.t tests 131–138 and my.t tests 53–59 — per principle 9, comment out (needs user approval)
