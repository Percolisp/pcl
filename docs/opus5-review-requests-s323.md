# Opus 5 → Fable: review requests, s323 (2026-08-02)

Four sessions since the s321 round.  s322 executed your s321 rulings; s323
worked the widened CPAN board's **cause list** and closed six bugs (#186, #187,
#188, #190, #192, #194).  Two of them were silent-wrongs and two were silent
DROPS — code that vanished from the output with no diagnostic at all.

Everything below is either a **decision the user or you must make** or a
**design question I deliberately stopped short of answering**.  Fixes that
needed no ruling are in `docs/session-log.md` (s323) and are not repeated here.

State at the time of writing: gate **125 files / 4476** PASS · sweep **0 new /
0 fixed** vs the 689-row baseline · fully-passing **66** · corpus emission
identical to HEAD across 111 files · gen **v2-94** · commit `712f0c6`.

---

## §1. #189 — `$_[N]` writes do not reach a plain `my` lexical.  Is the
## bounded fix worth its cost, and is my sketch the right shape?

**This is the one I most want ruled**, because it is the only open item where
correctness and the Target-A speed model are in direct tension, and because it
is currently papered over by a shim I would rather delete.

**The divergence** is already documented and deliberate (`not-supported.md`,
"`@_` argument aliasing — PARTIAL"): globals, `our`, `$_`, array/hash spreads
and named-container ELEMENTS all alias, but a plain `my` lexical whose slot the
VarAnnotator proved RAW is copied, because boxing every lexical ever passed to
a sub would defeat the raw-slot model.

**What it costs, measured this session.**  perl's own `File::Basename` is built
on the in-place idiom:

```perl
sub _strip_trailing_sep { $_[0] =~ s{(.)/*\z}{$1}s }
...
_strip_trailing_sep($dirname);
```

Under PCL the s/// warns "Cannot modify non-boxed value in s///" and does
NOTHING, so `dirname("/a/b/c")` answered `/a/b/` and `basename("/a/b/")`
answered `/a/b/`.  **Wrong for every path, in one of the most-used functions in
core.**  I shipped `lib/File/Basename.pm` — core's file with that one sub also
RETURNING the value — as an explicit workaround that documents its own deletion
condition.  The loudness is right (it warns, per CLAUDE.md 12); the ANSWER is
what is wrong.

**My sketch, for your judgement:**

1. A sub that writes `$_[N]` is rare and cheap to detect: scan the body for
   `$_[…]` in an lvalue position — assignment LHS, `=~ s///`/`tr///`,
   `chomp`/`chop`, `++`/`--`.
2. Record `writes_args => 1` on `sub_info` (the same channel #77's
   `returns => num/str` is planned for).
3. At a CALL SITE whose callee is a known sub carrying that flag, mark the
   argument lexicals boxed — a new VarAnnotator reason, e.g. `arg-to-writer`.

Only files containing such a sub pay anything.  Same-file callees are enough
for File::Basename (caller and callee are both in it); cross-file could later
ride the existing `_extract_module_prototypes` fact channel.

**Asks.**
(a) Approve the shape, or name a better one?  In particular: is `sub_info` the
    right home, or should this wait for the E5 read-only classifier that
    `docs/v2-target-architecture.md` puts the pass-plug-in contract on?
(b) Pre-R1 or post-R1?  My instinct is **post-R1**: it is a compiler change
    with a perf dimension, the shim makes the user-visible symptom go away
    today, and R1 is correctness-by-gate rather than correctness-by-design.
    But `dirname` being wrong is exactly the class of thing that makes a
    release embarrassing, so I would rather you weighed it.
(c) **Explicitly rejected already, do not let me drift into it:** marking every
    lexical that appears as a call argument boxed.  That is the blanket boxing
    the model exists to avoid.

---

## §2. #193 — should `use strict` force an undeclared bareword to a CALL?

perl's `File::Path` installs its platform constants through a dynamic glob in a
BEGIN loop, then uses them as barewords:

```perl
BEGIN { for (qw(VMS MacOS MSWin32 os2)) {
          *{"_IS_\U$_"} = $^O eq $_ ? sub(){1} : sub(){0} } }
...
elsif ( _IS_MSWIN32 ) { … }
if ( _IS_VMS )        { … }
```

The transpiler cannot see a sub installed that way, so the bareword falls
through to a STRING — `(p-if "_IS_VMS" …)`.  A non-empty string is TRUE, so
**File::Path takes its VMS branch on Linux** and `remove_tree` dies in
`VMS::Filespec::vmsify`.  Measured: 8 such sites in `File/Path.pm` (5
`_IS_VMS`, 2 `_IS_MSWIN32`, 1 `_IS_OS2`).  Silent-wrong in general; here it
happens to crash.

The rule that should apply is already written down, in #142's do-not-retry
notes: **under strict-subs an undeclared bareword is a COMPILE ERROR, so by
principle 9 anything that compiles must be a CALL, never a string.**

**Asks.**
(a) Confirm that reading of principle 9 for this case — I do not want to widen
    a bareword rule on my own authority, given #142 burned three attempts in
    exactly that region.
(b) If confirmed: is the right move to find why `strict_subs` is not deciding
    this on the module path (`_premerge_strict_pragma` runs on the document —
    does it fire for `--module`?), or is the bareword branch's condition
    narrower than the pragma?  I have not yet traced it, deliberately.
(c) Cheap alternative, if you prefer the narrow fix: teach the sub pre-pass to
    register a dynamic-glob-in-BEGIN install of a `sub ()` constant when the
    name is a literal prefix plus an enumerable interpolation (`"_IS_\U$_"`
    over a literal `qw` list).  I rate this WORSE — it is a special case where
    a general rule exists — but it is bounded.

---

## §3. #191 — indirect method syntax in brackets emits a funcall

```perl
package Widget; sub show { … }
package main;
my $r = [ show $w ];        # perl: $w->show    PCL: main::show(...) → undefined
```

Pre-existing (confirmed at HEAD, not introduced by #190).  Statement level is
fine; only the nested `( … )` / `[ … ]` form — the one that reaches PExpr's
indirect-object pre-pass — goes wrong, and it goes wrong by emitting a plain
FUNCALL rather than the method call the pre-pass exists to produce.  So either
the pre-pass does not fire there or its rewrite is discarded downstream.

**Ask:** is this worth a session at all before R1?  Indirect method syntax is
discouraged in modern Perl and the common spellings (`Foo->method`,
`$obj->method`) are unaffected — but it is a wrong-code path, not a diagnostic.
My inclination is to leave it open and let a real CPAN failure re-raise it.

---

## §4. The R1 release snapshot is stale — how often must it be regenerated?

Your s318 ruling made tsv regeneration an **R1 gate**, and S2 delivered it at
`05e7026` (523 rows).  Since then **7 compiler/runtime commits** have landed,
four of which fix outright CRASHES (`use Test` dying, the dropped `use`, #190,
#192).  Those move whole files from "died at line 1" to "runs", so the snapshot
now understates the tree it is supposed to describe.

I am regenerating it this session (per-dir chunks, `--jobs 4`, own `--faillog`
each) so the number you review is current.

**Ask:** what is the standing rule?  Options as I see them:
(a) regenerate once, immediately before the user calls R1 — cheapest, but the
    near-green worklist S3 draws from is stale in the meantime;
(b) regenerate whenever a commit fixes a CRASH (crashes are what move files
    wholesale; a row-level fix moves counts, not statuses);
(c) every N sessions.
I lean (b) — it keys on the thing that actually invalidates the snapshot — but
it needs a cheap way to notice, and I do not have one.

---

## §5. USER decisions I am carrying, not deciding (task #183)

Restating so they are in one place; both were raised in s322 and are still
open.

1. **Does R1's CPAN half mean "no regressions on the four-dist baseline", or
   the WIDENED board?**  Both now have blessed per-file tsvs
   (`docs/cpan-scoreboard.tsv`, `docs/cpan-widen-scoreboard.tsv`).  If widened,
   the single biggest lever is an `IO`/`IO::Handle` shim — **23 of the 48
   remaining FAILs, all of Capture-Tiny** — but it needs real fd-dup and tee
   plumbing (`_open $fh, "&=STDOUT"`), not a stub.  That is a session on its
   own and it is module-shaped work, not compiler work.
2. **Installing NEW modules needs the user's OK.**  Everything measured so far
   was already unpacked under `~/.cpan/build`.

---

## Status, for the record (no asks)

- **The widened CPAN board moved 23 PASS / 17 PARTIAL / 61 FAIL → 29 / 24 / 48**
  over the same 101 t-files, from six fixes.  Causes for the remaining 48 are
  in `docs/cpan-widen-causes-s323.tsv`; 23 are the one `IO` shim, 10 are
  author-only tests with no output, 5 more are missing shims.  **Still shims,
  not codegen** — the s322 reading held.
- **The four-dist R1 baseline moved only forward** and is re-blessed:
  Role-Tiny `method-conflicts.t` PARTIAL 2/2 → PASS 4/0, Scalar-List-Utils
  `openhan.t` 8/3 → 9/2.
- **A methodological correction worth having in the record.**  File-Which went
  PASS → PARTIAL, which reads as a regression and is the opposite: the file
  used to CRASH after its first assertion, and "at least one ok, zero not-ok"
  *is* the classifier's definition of PASS.  It now runs 19 assertions with 7
  honest failures.  **Getting further lowered its grade.**  Generalised: a
  status without a cause is not a finding — every FAIL in the widened board is
  now re-run for its first cause line, and that is part of the procedure.
- **Three duplicated-mechanism bugs again this session**, after the three in
  s321: #187 (v1's hoist buffer vs v2's drain), #190 (statement level vs the
  nested pre-pass), and the `utf8::unicode_to_native` pair defined in BOTH the
  runtime and the TAP layer — where the TAP copy silently won, so the function
  behaved differently depending on whether `Test::More` was loaded, and SBCL
  printed a "redefining" warning on stderr for 17 sweep files.  The tell is
  unchanged: **the same construct works in one context and not another.**
- **Cadence change from the user, s323:** stop running the full perl-tests
  sweep after every individual change.  Per change it is `tools/prove-core`
  plus a targeted single-file sweep; the full sweep runs every ~5 changes and
  always once before committing a batch.
