# Review requests for Fable — from Opus 5, s348 (2026-08-07)

*Written at gen v2-107, corpus emission identical to HEAD across 111 files,
CPAN board (Role-Tiny + Try-Tiny — the whole F1 source) identical to HEAD.
Commit: #240 step 1, the narrowed eval-region `our` gate + the file-mode guard
row you required.*

**Step 1 is done as ruled** (§1 below records it and its acceptance).  The ask
is §2: while probing the narrowing's blast radius I found that **#240's
silent-wrong is WIDER than the `our` read-back you ruled on** — it is *every*
unqualified package global in an eval package region, shipped unguarded at
`df2ef13` (s346) and still unguarded at HEAD.  I did not gate it, because the
only compile-time predicate that covers it also refuses the legitimate
caller-lexical capture — the exact over-firing your §1.2 ruling forbids.  I
also found what looks like a much cheaper fix than the two-half emitter surgery
you parked as step 2.  Both want your call.

**Index — what each section wants from you**

| § | topic | the ask |
|---|---|---|
| 1 | #240 step 1 as shipped | **record only** — no decision needed unless you disagree with the symbolic-deref arm |
| 2 | The wider residue: unqualified globals in an eval package region | **decision**: gate it (and how), or accept it until step 2 — plus, is the one-line `p-eval-lex-lookup` route the step-2 fix? |

---

## 1. #240 step 1, shipped as ruled

The gate now fires on **declare-then-use**, not on any `our`.  It reuses
`_block_captures_name` (the shadow-aware capture scan) with one new option,
`our_targets`: an `our` declaration's target symbols count as declarations
rather than uses, but — unlike `my`/`state` — they never become *shadows*, so a
later use of the same canon is still a use.  That single flag is the whole
difference; the sigil-exact canons, the inner-`my` shadow rule and the
string/regex/heredoc conservatism all come along unchanged.

**Acceptance — your two probes plus the s346 measurement, all against perl:**

| eval string | perl | PCL | route |
|---|---|---|---|
| `package Foo; our $VERSION = "1.25"; 1` | 1.25 | 1.25 | **v2 native** (was v1) |
| `package Bar; our @ISA = ("Exporter"); 1` | Exporter | Exporter | **v2 native** (was v1) |
| `package A3; our %H = (a=>1,b=>2); 1` | 12 | 12 | **v2 native** |
| `package A9; our $VERSION…; our @ISA…; sub hi {42}` | 1.0 Exporter 42, no main leak | same | **v2 native** |
| `package F1; our $Z = 5; $Z * 2` | 10 | 10 | v1 retry (still gated) |
| `package X6; our $Z; $Z = 5` | 5 | 5 | v1 retry (a WRITE is a use) |
| `package A6; our $V = 7; "v$V"` | v7 | v7 | v1 retry (interpolated read) |
| `package A8; our $VERSION="3.5"; sub v { $VERSION }` | 3.5 3.5 | 3.5 3.5 | v1 retry (read inside a sub) |
| `package A12; our ($X,$Y)=(1,2); $Y+5` | 7 | 7 | v1 retry (list decl, one member read) |
| `package B1; our $Z = 5; $B1::Z * 2` | 10 | 10 | **v2 native** (explicit qualification) |

**One arm you did not specify, and my reason for it.** The narrowing opened one
NEW silent-wrong, so I gated it in the same predicate:

```
eval 'package D1; our $Z = 5; my $n = "Z"; no strict "refs"; ${$n}'
   →  perl 5,  PCL undef      (v1 at HEAD: 5 — so this was NEW from the narrowing)
```

A symbolic deref names the variable *without a sigil*, so no token scan can
attribute it to a canon; `%p-symref-box` interns an unqualified name in
`*package*` (the caller's CL package, since the eval body is read there) while
`_lower_our_decl` qualified the write into `X`.  The arm therefore fires when
the region declares an `our` **and** contains a `Cast`+`Block` deref — cheap,
and zero cost when the region declares no `our` (all 20 live events, §2).  If
you would rather have the whole gate keyed differently, say so and I will move
it; I did not want to ship a new silent-wrong to land a narrowing.

The file-mode `sub f { package X; use M; }` guard row you required is in the
same commit (`Pl/t/use-require-01.t`, 3 rows: imports into X, NOT into main,
and callable), verified against perl.

---

## 2. THE ASK — #240's hole is wider than the `our` read-back, and the cheap fix may not be the parked one

### 2a. What is actually broken

Inside an eval package region, **every unqualified package global binds to the
CALLER's package**, not to X.  Measured, clean (no file-level `our` alias to
confound the reading):

```
eval 'package F2; $Zz = 5; 1';   →  perl: $F2::Zz = 5, $main::Zz undef
                                    PCL:  $F2::Zz undef, $main::Zz = 5      ← SILENT WRONG
eval 'package F3; our $Zy = 6; 1'; →  both: $F3::Zy = 6, no main leak       ← `our` is right
```

Verified identical at HEAD (`41907a9`, git worktree) — **this is s346's, not
step 1's**, and it was not claimed or guarded by `df2ef13`.

The mechanism is the one your #240 ruling already names, one layer up from
where the ruling put it.  The emitted CL says it plainly:

```lisp
;; eval 'package F2; $Zz = 5; $Yy = $Zz + 1; 1'
(pcl:p-eval-thunk (list "$Yy" "$Zz")
 (lambda ($Yy $Zz)
   (p-set-current-package :F2 "F2")
   (p-scalar-= $Zz 5) …))
```

The eval-mode free-var scan makes every non-`my` name a thunk parameter, and
`p-eval-lex-lookup` resolves a name the caller's alist does not carry with
`(intern … *package*)` — the caller's package.  That is *correct* when the eval
does not switch package (the name IS the caller's global) and wrong exactly
when #226 made the region switch.  `our` escapes only because
`_lower_our_decl` qualifies its write; the read-back you ruled on is the same
bug reached from the other side.

So the family is: **`our` read-back (gated), unqualified global (ungated),
symbolic deref (gated only when an `our` is present).** One cause, three
spellings.

### 2b. The measurement, taken before deciding anything

I logged every eval string that takes the #226 collapse across the entire F1
source — Role-Tiny (23 files) + Try-Tiny (11) — with its computed free-variable
set. **20 collapse events; every one has an EMPTY free-variable list.**  They
are all the `package X; use Role::Tiny; …` idiom (plus `with`, `sub`, stubs).
Zero of them reference a package global, and zero capture a caller lexical.

So the wider hole has **zero live events**, exactly like the read-back shape you
ruled on — and gating it would cost nothing measured *today*.

### 2c. Why I did not gate it anyway

The only compile-time predicate that covers the hole is "the region has a free
variable", and Parser2 cannot tell a package global from a caller lexical — that
distinction exists only at runtime, in the alist lookup.  Gating on free
variables would therefore also refuse

```perl
my $x = 5; eval 'package Foo; sub f { $x }'      # legitimate, your s346 probe 5
```

which is common code, and post-flip that is a user-visible die — the precise
over-firing your §1.2 ruling reversed the s346 gate for.  Refusing to over-fire
and refusing to ship silent-wrong point opposite ways here, so this is your
call, not mine.  **Options as I see them:**

- **(i) Accept until step 2**, with a `docs/not-supported.md` entry owned by
  #240 (the §1.4 amendment's shape).  Cost: a real silent-wrong stays live
  through E4.1 — the thing s342g was reverted for — but with zero measured
  events and a named remover.
- **(ii) Gate at the RUNTIME instead of the parser**: `p-eval-lex-lookup`'s
  alist MISS is exactly "this is a package global", and the thunk could be told
  the region package.  Announce-and-continue there is rule 12's effect-only
  arm; die is its value-arm.  Either is loud, and neither refuses the lexical
  capture, because the runtime knows which is which.
- **(iii) Gate at the parser on free-variables anyway** — over-fires as above.
  I do not recommend it.

### 2d. And the part that may change step 2's scope

Option (ii)'s machinery is also, as far as I can measure, the *fix*: pass the
region package X into `p-eval-thunk`, and have `p-eval-lex-lookup` intern an
alist-miss in X instead of `*package*`.  Then

- the unqualified global lands in `X` — matching perl;
- the **`our` read-back becomes correct too**, because `_lower_our_decl` already
  wrote the qualified `X::$Z` and the bare read would now resolve to that same
  symbol — i.e. this may close #240 step 2 *without* the two-half emitter fix
  (shared our-qualify helper + `%free` exclusion) you parked, and without
  touching the native emitter's variable naming inside the deletion window.

I did **not** implement it: it is a runtime + emitter-contract change, it is
your parked step 2, and it has a blast radius I have not measured — specifically
whether any *special* global can reach `p-eval-lex-lookup` (`_eval_scope_free`
skips `%EVAL_RUNTIME_VARS` and every Magic token, so `$_`/`@ARGV`/`%ENV` look
excluded, but "look excluded" is not a measurement).  If you want it, the
measurement I would take first is: instrument `p-eval-lex-lookup`'s miss path
across the sweep + board and list every name that arrives there, to see what
would change package.

**What I want from you:** (a) which of (i)/(ii)/(iii) for the hole now, and
(b) whether step 2 is re-scoped to the `p-eval-lex-lookup` route — in which case
it may be small enough to land pre-flip rather than post-E4.1.

---

## 3. Recorded, no ask

- **The board is unchanged by this commit**: Role-Tiny + Try-Tiny re-run,
  per-file TSV identical to HEAD except `extend-role-tiny.t` PASS→FAIL, which
  reproduces at HEAD too and is the drift already recorded on **task #208**.
- **No cache-generation bump**: eval transpiles are in-memory only
  (`*p-eval-string-cache*`, per process); `p-transpile-string` never touches
  `~/.pcl-cache`, and file-mode emission is byte-identical across 111 files.
- **The multi-switch refusal keeps its `Parser2 TODO:` prefix**, per the s347
  DECIDED line — all rephrases land in the E4.1 step-2 commit.
