# The ignored-tests audit, round 22 — the rows nobody read (s464ay)

Executes `docs/plan-test-audit-s464.md` phase 4a (task #965, the inline
SKIPs), phase 3 (the 695 blessed sweep rows read for cause) and a first pass
over the companion suite's count-blessed rows.  Measured on `80b715c`.

**The question this session was asked**: #964 — an ordinary sub returning the
caller's box instead of a copy — hid for months although perl's own test for
it existed TWICE.  Both copies had been neutralised: one replaced by an inline
`ok(1, 'SKIP: @_ aliasing not supported …')` with a WRONG diagnosis, one
blessed inside a per-file COUNT.  So: how many more rows are in that state, and
what is behind them?

**The short answer**: the census that was supposed to bound the problem was
itself wrong by 51 %, five files the sweep calls *fully passing* were passing
on manufactured rows, and reading the restored rows produced **nine new
filed bugs**, five of them silent-wrong semantics of the #964 kind.

---

## 0. Three findings about the instruments, before any test row

**0a. The inline-SKIP census undercounts by 51 %.**  Task #965 says "132 rows
in 11 files", from `grep -c "ok(1, 'SKIP"`.  That pattern only matches the
SINGLE-quoted spelling.  With both quote styles:

| | files | rows |
|---|---:|---:|
| census as filed (`ok(1, 'SKIP`) | 11 | 132 |
| **actual (`ok(1, ['\"]SKIP`)** | **13** | **199** |

Three files were entirely invisible — **`sort.t` (32 rows)**, `kvhslice.t`
(25), `splice.t` (2) — and `reset.t` was counted as 4 when it has 22.  sort.t
is one of the semantically central files.  The census grep in #965 and in
`plan-test-audit-s464.md` §2b must be
`grep -c -E "ok\(1, *['\"]SKIP"`.

**0b. There is a THIRD spelling the census never looked for.**  Nine
hand-added `skip "… not supported in PCL", N` calls hide at least 21 further
rows — `each.t` 3 calls (4 + 8 + 1 rows), `pack.t` 4, `range.t` 1 (4 rows),
`chr.t` 1 (4 rows).  A `skip` inside a `SKIP:` block is Test::More's own
mechanism, so it produces real `# skip` TAP and looks legitimate — but used
this way it is the same rule-5 sin as `ok(1,'SKIP')`: the assertion never
runs.  The skip-registry exists precisely so that it does.  Not fixed here;
listed for the next phase.

**0c. "Fully passing" is not a fact about PCL.**  Five files the sweep
reported as PASS were passing on manufactured rows:

| file | reported before | actually, restored |
|---|---|---|
| `sort.t` | **PASS 204/205** | 182 pass / 22 fail |
| `reset.t` | PASS 41/45 | 22 pass / 19 fail |
| `loopctl.t` | PASS 67/67 | 63 pass / 4 fail |
| `join.t` | PASS 43/43 | 41 pass / 2 fail |
| `kvhslice.t` | PASS 39/39 | 19 pass / 19 fail (+1 row never produced) |

`sort.t` — 32 manufactured rows — was in the "Fully passing" list.

**The sweep's "Fully passing" count goes 62 → 58**: `sort.t`, `loopctl.t`,
`kvhslice.t` and `each.t` leave it.  `join.t`, `reset.t`, `splice.t` and
`time.t` stay, because their restored rows either pass or are registry-skipped
with a cited reason — which is what an honest "fully passing" looks like.

---

## 1. Pass 1 — the inline SKIPs restored (task #965)

Method, per row: recover the upstream assertion (perl 5.40.3 `t/`, which the
`## PCL SKIP:` comments quote verbatim), restore it, run the file, and
classify.  A row that PASSES was hiding a fix nobody counted.  A row that
FAILS gets its WRITTEN REASON verified by probing the primitive the reason
names — and the reason is wrong often enough that this is the whole method.

### 1a. Verdicts

**11 of 13 files done; 115 of the 199 rows.**

| file | rows | STALE (pass now) | correct reason → registry | WRONG reason → registry | new BUG → fail-baseline | crash/incomplete |
|---|---:|---:|---:|---:|---:|---:|
| `sub.t` | 11 | **2** | 5 | 3 | 1 (#1020) | — |
| `sort.t` | 32 | **10** | 13 | 3 | 6 (#1021 ×3, #684 ×2, 1 unattributed) | — |
| `reset.t` | 22 | **3** | 19 | — | — | — |
| `kvhslice.t` | 25 | **6** | 14 | — | 4 (#1023, #1024) | 1 row never produced |
| `loopctl.t` | 8 | **4** | 3 | — | 1 (#1022) | — |
| `join.t` | 5 | **3** | 2 | — | — | — |
| `local.t` | 3 | — | — | 3 | — | — |
| `splice.t` | 2 | **2** | — | — | — | — |
| `each.t` | 2 of 11 | — | — | 2 | 2 (#1027) | — |
| `time.t` | 1 | **1** | — | — | — | — |
| `concat.t` | 1 (probed) | — | 1 | — | — | — |
| **total** | **115** | **31** | **57** | **11** | **14** | 1 |

**31 of 115 restored rows PASS today.**  Every one of them was a fix that
shipped and was never counted, sitting behind a comment that said the feature
did not work.

Not done: `state.t` (46 rows) and `lex.t` (38).  Reasons in §1d.

**Nine of the eleven files are now BYTE-IDENTICAL to perl 5.40.3's `t/op/*.t`**
(`sub.t`, `sort.t`, `join.t`, `reset.t`, `loopctl.t`, `local.t`, `time.t`,
`kvhslice.t`, `splice.t`) -- they had differed from upstream ONLY by the skip
edits, so the restoration was a copy.  That is the state `cl/skip-registry.lisp`'s
own header asks for: "the perl-tests/*.t files stay byte-identical to upstream
Perl (diffable)".  It also means those nine drop out of the "43 of 105
extractions differ from 5.40.3" line in `plan-test-audit-s464.md` section 2b.

### 1b. The written reasons that were FALSE

These are the #964 pattern exactly — a plausible sentence, in the tree, wrong:

| file:row | what the comment said | what is true |
|---|---|---|
| `sub.t:42` | "@_ aliasing not supported — shifted @_ element is the same box as the caller's variable" | **@_ aliasing works** (probed: write-through, array spread, `\$_[0]` identity all match perl).  The row was #964, the return-copy protocol, and it PASSES since #964 shipped. |
| `sub.t:146` | "@_ aliasing / XS in-place modification not supported" | Same false premise.  The real cause is the ruled `utf8::encode` no-op; the hole-write itself agrees with perl (probed). |
| `sub.t:246` | "string eval runs in a subprocess and cannot capture outer lexical variables" | String eval is native and captures `state` since #401.  **PASSES.** |
| `sub.t:290-291` | "PCL's local-hash-key restore does not invoke tie DELETE callbacks" | `tie %hash` is not implemented AT ALL (#155, announced loudly).  A narrow-sounding gap standing in for a wholesale one. |
| `sort.t` ×7 | "wantarray regression in PCL sub body" | **All seven PASS.**  `sub sortr { reverse sort @_ }` in list context matches perl in every spelling probed. |
| `sort.t:958-960` | "sort doesn't use overloaded 'cmp'" / "sort ignores overloaded '\"\"'" | First half right for the DEFAULT comparator, second half backwards — sort uses `""` and nothing else (#1021). |
| `sort.t:821` | "crashes SBCL" | It does not; PCL survives with a different message. |
| `loopctl.t:990` | "foreach loop var aliasing not supported in PCL … PCL copies the value instead" | **Flatly false.**  bug 37725 passes. |
| `each.t:374-375` | "`use feature 'refaliasing'` is not supported … removed in Perl 5.40" | Refaliasing is IMPLEMENTED (#325, s396) and is not removed.  The rows still fail, for a reason nobody had looked for (#1027). |
| `kvhslice.t` ×6 | "wantarray regression" / "context propagation through string eval" | Both false for these rows: they reproduce with no eval at all (#1023). |
| `splice.t` ×2 | "wantarray regression gives :void ctx to splice" | **Both PASS.** |
| `time.t:64` | "PCL localtime does not call tzset() on $ENV{TZ} change" | **PASSES.**  See §5. |
| `local.t:77-85` | "eval string not supported" | String eval is a hard requirement and works; the row is about perl's rejection message for `local $$ref`. |

### 1c. Rows held back as fix targets (NOT registered as skips)

| file:row | cause |
|---|---|
| `sub.t` 24 | **#1020** `undef *GLOB` is a silent no-op |
| `sort.t` 162-164 | **#1021** sort's default comparator ignores an overloaded `cmp` (+ #155 for the tie half) |
| `sort.t` 174-175 | **#684** match variables are block-scoped in perl.  NB #684's own text claims sort comparators are already handled; these rows show they are not. |
| `sort.t` 26 | unattributed — does NOT reproduce in isolation (two shapes probed); an in-file interaction |
| `loopctl.t` 41 | **#1022** unlabelled `last` inside a called sub |
| `kvhslice.t` 9, 11 | **#1023** kv-slice in scalar context yields the count |
| `kvhslice.t` 28, 29 | **#1024** bareword key in a kv-slice is not autoquoted (crashes) |
| `each.t` 64, 65 | **#1027** refaliasing + `each` in list assignment, in-file only |

### 1d. What is left, and why

- **`state.t`, 46 rows.**  The file is TRANSPILE-FAIL whole (one `given` block,
  the ruled given/when refusal, since s415) so it produces ZERO rows today —
  restoring its assertions changes nothing measurable.  Its skips are almost
  all principle-9 ("PCL does not reject invalid state list assignment").  This
  is phase-1 work (`plan-test-audit-s464.md` §4), not phase 4a.
- **`lex.t`, 38 rows.**  `perl-tests/lex.t` is 251 lines against upstream
  `t/base/lex.t`'s 588 — a different extraction, so there is no upstream text
  to restore from row by row.  That is phase-4b (the refresh decision, §5 of
  the plan, a USER call).
- **`each.t` 9 of 11, `range.t` 7, `concat.t` 1.**  These files carry
  legitimate local edits (real `skip` blocks, `$Config` guards, a changed plan
  count), so a wholesale restore is unsafe and each row needs hand work.  Their
  claims were all probed and are recorded above and in §5.

---

## 2. Pass 2 — the skip-registry's reasons

The registry's own stale-detector already reports a registered row that starts
PASSING, so that half is instrumented.  What is not instrumented is a **wrong
reason on a still-failing row**, and there are three:

Three registrations cite `not-supported.md: '@_ argument aliasing'` with the
wording "@_ elements are copies in PCL" / "@_ aliasing not emulated".  **@_
aliasing works** (probed).  Corrected in place:

| registration | verdict |
|---|---|
| `array.t` "@_ alias to nonexistent" | **WRONG REASON.**  The named primitive works — `$#a=3; sub setit { $_[0]="v" } setit($a[1])` vivifies in PCL exactly as in perl.  The row still fails, so it fails for an unidentified reason; the registration is now marked provisional. |
| `substr.t` "scalar does not affect lvalueness of substr" | right in substance, false in wording — the gap is that the CALLEE lowers substr's target as a value (#209).  Probed: still diverges. |
| `chop.t` "chomp @a when … eq 0" | re-pointed at 'Sparse arrays … and SV identity'; it is result-SV identity, not argument aliasing. |

`docs/not-supported.md`'s own index line for `@_` argument aliasing still read
"arguments are copies, so `$_[0] = 42` does not write back" — false since #131
/ #189.  Left for the doc owner; the section body (§632) is accurate.

---

## 3. Pass 3 — the 695 blessed sweep rows, clustered and attributed

`baselines/fail-baseline.tsv` has no cause column, so the rows were clustered
by file × mechanism ("rows that would plausibly flip together") and each
cluster looked up in DECIDED → not-supported.md → the task store.

| status | clusters | rows |
|---|---:|---:|
| ATTRIBUTED | 81 | 298 |
| UNEXPLAINED | 47 | 397 |
| **total** | **128** | **695** |

Full per-cluster table: `scratch/pass3-clusters.md` in the s464ay worktree
(kept out of `docs/` — it is working material, not a decision record).

### 3a. The single biggest finding: 229 rows, one cause, no owner

**`bop.t`, 229 of the 695 blessed rows (33 %), had no recorded cause.**  It is
one mechanism, now filed as **#1028**: `&` `|` `^` choose the NUMERIC op
whenever an operand is a ref / glob / qr// / object / undef, where perl
stringifies it and does the BIT-STRING op.

    my $u;  print +($u | "abc");     # perl: abc      PCL: 0
    my $r = [1]; print +($r | ("\x00" x length "$r"));
                                     # perl: ARRAY(0x…)   PCL: 1

Plain-string and plain-numeric operands are correct in every spelling probed,
so the bit-string machinery exists — only the mode decision is wrong.
`undef | "abc"` → 0 is an ordinary shape with a plausible answer, which is why
it never surfaced outside bop.t.

### 3b. What the causes are, by weight

- **#221 (warnings-gated diagnostics absent) is the largest attributed cause,
  ~62 rows**: assignwarn.t 20, signatures.t 19, caller.t 5, magic.t 4,
  hashassign.t 4, bop.t/pack.t/length.t/flip/blocks/qr the rest.
- **#233 (caller fidelity)** owns 29 of caller.t's 44.
- **Principle 9 + error-message text** owns most of postfixderef.t, method.t,
  eval.t and signatures.t's remainder.
- `sort.t` and the `tie*.t` files contribute **zero** rows to this baseline —
  which is exactly why sort.t's 32 manufactured rows were invisible from here.

### 3c. Unexplained clusters worth a probe next (largest first)

pack.t utf8-flag family (23 + 11 + 5 + 4 + 2), scalar.t in-memory-handle
code-points-over-0xFF (12), magic.t %ENV store encoding (10), caller.t
`@DB::args` (5), multideref.t (5), magic.t `%SIG` key qualification (4) and
`${^LAST_FH}` (3), postfixderef.t residue (5), **scalar.t's scalar-tie
FETCH/STORE call sequence (4)** — that last one is the interesting shape,
because scalar `tie` DOES work in PCL, so it is a real ordering question and
not a missing feature.

**One correction to the clustering, made by reading the source**: aassign.t's
6-row "tied … FETCH/STORE order is wrong" cluster is `tie @proxy, "ArrayProxy"`
— a tied ARRAY, so it is **#155** (`not-supported.md` 'tie on an ARRAY or
HASH'), not unexplained.  That moves 6 rows from UNEXPLAINED to ATTRIBUTED and
is a reminder that a cluster's mechanism name must come from the test SOURCE,
not from its description.

---

## 4. Pass 4 — the companion suite's count-blessed rows

(filled in below)

---

## 5. For the USER — not decided here

1. **`docs/not-supported.md` 'Runtime `$ENV{TZ}` changes not reflected in
   `localtime`' looks stale.**  `time.t` is 72/72 with its TZ skip restored,
   i.e. the assertion `$hour != $hour2` after two `$ENV{TZ}` assignments now
   passes.  Re-probe the entry and retire it if it holds.
2. **`docs/not-supported.md` 'Context propagation into string eval' names
   `kvhslice.t` 9-12 and 25-28 as affected.**  Four of those rows diverge with
   no eval involved (#1023); the Affected-tests line needs re-pointing.
3. **The 9 hand-added `skip "… not supported in PCL"` calls** (§0b) are the
   same rule-5 problem in a spelling the census does not see.  Migrating them
   to the registry is a phase-4a remainder.
4. **`kvhslice.t` goes OK → PARTIAL** (39 planned, 38 produced).  The
   manufactured rows had been filling a planned-minus-produced hole — exactly
   what instrument I2 in the plan is for.
5. **`sort.t` row 26** and **`each.t` 64/65** (#1027) fail in-file and pass in
   every isolated shape probed.  Both need a bisection of the file, which is a
   session of its own.
