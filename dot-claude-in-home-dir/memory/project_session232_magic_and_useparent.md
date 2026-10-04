---
name: project_session232_magic_and_useparent
description: Session 232 (2026-06-04) UNCOMMITTED work — magic.t fixes + use parent require; blocked on POSIX LDBL_MAX float overflow
metadata: 
  node_type: memory
  type: project
  originSessionId: 2fffdfb7-db16-4154-9877-61749f8eca2e
---

Session 232 work is **UNCOMMITTED** in the working tree (10 modified, 2 untracked). Full detail
in `docs/session-log.md` §232. Summary + the live open threads:

**Done (verified, in tree):** unknown `${^NAME}` caret vars degrade to globals (was `die`);
`@-`/`@+` match-offset arrays; `$$` assignable (boxed); `unlink_all` harness helper; `$?` runtime
test. **The big one:** `$\` was mapped in `%SPECIAL_VARS` to text `|$\|` — the `\` escapes the
closing pipe → unreadable CL symbol → reader swallows rest of file ("unmatched close paren") →
silent truncation. Fixed to `|$\\|`. **magic.t 0 → 129/208.** Also `use parent`/`use base` now do
the implicit `require` (new non-fatal `p-require-parent`) + fixed `-norequire` detection (PPI gives
`-norequire` as ONE Word, not `-`+word).

**Full sweep: 18041 pass / 789 fail / 69 fully passing; 0 regressions outside magic.t** (the +23 are
newly-visible magic.t rows). Baseline NOT re-blessed yet.

**BLOCKED / OPEN (resume here):**
1. `perl-tests/parent.t` (Perl's authoritative parent test) = NO OUTPUT, blocked by a **pre-existing**
   bug: `lib/POSIX.pm:15` `use constant LDBL_MAX => 1.1897314953572317e+4932` (80-bit long-double max)
   overflows SBCL `double-float` (max ~1.8e308) at READ time → whole POSIX module fails to compile.
   parent.t hits it via real `use Test::More` (chain pulls POSIX); most perl-tests use `test.pl` instead.
   FIX (do both): (a) `lib/POSIX.pm` LDBL_MAX → `most-positive-double-float`/+Inf; (b) GENERAL — ExprToCL
   float-literal emission should clamp out-of-double-range values to a safe form so no float literal ever
   yields unreadable Lisp (same class as the `$\` bug).
2. **User decision pending:** keep or revert the `use parent` require change — correct + zero-regression
   but sweep showed **0 fixed** (no current file exercises `use parent 'RealModule'` failing→passing).
3. **Design question pending:** parent.t tests 7-8 expect `use parent 'Missing'` to DIE ("Can't locate
   ... in @INC"); my `p-require-parent` is deliberately NON-FATAL. Perl = fatal. Decide.
4. Then: clean full gate + sweep, re-bless `baselines/fail-baseline.tsv`, commit (split: `$\`+magic feature
   as one unit; use-parent as another if kept).

Both `$\` and the POSIX `LDBL_MAX` overflow are the same bug class: **codegen emitting Lisp that can't
be READ** (worse than a runtime crash — the recovery loader can't skip an unreadable form). Watch for
more of this class. See [[feedback_no_simplify_tests]] (the magic-vars-01 test 18 fix was correcting a
test that had PINNED the buggy output, not weakening it).
