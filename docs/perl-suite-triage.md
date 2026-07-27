# perl-suite crash-family triage (s309, 2026-07-23; re-run s316b, 2026-07-27)

**s316b re-run (433 files): 43 OK, 31 NOTAP, 91 XDIFF, 268 UNEXPLAINED**
— snapshot in `docs/perl-suite-run.tsv`.  Per-file against the s309 row
set: 372 unchanged, 39 DIFF→XDIFF (expected-tsv rows added since), 4
DIFF→OK (the s310 fixes: mro/recursion_c3+dfs, op/groups, op/mkdir), 7
OK→DIFF.  Of those 7, **five already failed at s315d** (A/B'd in a
worktree at 810df31) and are task **#127**: op/stash_parse_gv.t,
op/tr_latin1.t, re/reg_eval.t, run/fresh_perl.t, run/switchF2.t.  The
other two were s316's own, and both were *false passes* being corrected:

- **op/sigsystem.t** skips 4 tests `if not exists $SIG{CHLD}` — with the
  old empty `%SIG` it emitted 4 skip-oks against perl's 4 oks and scored
  OK.  With `%SIG` populated it runs for real and blocks on Time::HiRes
  (XS) → expected-tsv row.
- **io/paragraph_mode.t** was a HARNESS bug, not PCL: `tempfile()` in
  `perl-tests/t/test.pl` returned `/tmp/pcl-test-$$-N` without checking
  the name was free (perl's own does `!$tmpfiles{$try} && !-e $try`).
  PIDs recycle and op/mkdir.t leaves *directories* with those names, so a
  later run opened a directory and died "Is a directory".  Fixed by
  probing for a free name — which also recovered **op/read.t,
  op/sysio.t and run/script.t**, three silent victims of the same
  collision (43 OK vs 39 in the run before the fix).

**s316e: run/fresh_perl.t 53→55 — the hole-aliasing gap is CLOSED**
(task #127): `for (@a)` / grep/map `$_` / `@_` hole slots now alias via
lazy *defelem-lite* boxes (`%p-defelem-box` in the runtime — reads undef
and stay non-`exists`, first write vivifies the source slot; bare-array
foreach also iterates the live array, so a mid-loop `push` extends the
iteration).  Cases 29 (`for (@a) { $_ = 2 }` over holes) and 30 (defelem
on `@_` + `local $_`) now match.  Remaining 36 fails = the fatal-error
message-fidelity family, invalid-perl detection (out of scope, CLAUDE.md
§9), and one `(?{})` case.  Guards: 7 new tests in
`Pl/t/transpile-test-06.t`; array.t skip-registry rows 174/179 dropped
(stale — they pass now).  not-supported.md §sparse-arrays rewritten.
**File CLOSED as XDIFF** (expected-tsv row): the user confirmed
2026-07-28 that matching perl's exact fatal-error TEXT is not a goal
(not-supported.md §Error message text and format) — task #127 done.

**s316d: task #127 resolved for 4 of the 5 drift files** (per-file
verdicts, not a group bless):

- **op/stash_parse_gv.t 0/5 → OK.**  Three general bugs, none about long
  names per se: (1) `&{"name"}` / `&$ref` / `&{$ref}` with *no argument
  list* lowered to a bare `(p-get-coderef …)` — a fetch, never a call —
  on BOTH pipelines; the `&` prefix now lowers to
  `(p-funcall-ref … @_)` like the leaf `&foo;` form, and the mention
  parents (`\`, `defined`, `exists`, `undef`, `goto`) reach past it via
  `_amp_cast_operand_id`.  (2) The Perl-4 `'` package separator: PPI
  cannot tokenize it at all, so `sub x'y {…}` is normalised to
  `sub x::y` in `_preprocess_source` (string-guarded, sub-decl shape
  only) and symbolic name strings normalise at runtime
  (`%p-tick-package-seps`).  (3) `p-funcall-ref` resolved the package
  with `%pcl-invert-case` — wrong for multi-segment packages
  (`|aa::bb|` keeps case) — and now routes through the shared
  `%p-resolve-sub-symbol`.  Guards: 4 perl-oracle cases in
  `Pl/t/transpile-test-06.t`.
- **op/tr_latin1.t → OK.**  Count-only `tr` (empty replacement, no /d)
  on a read-only value warned "Cannot modify non-boxed value" — perl
  accepts it since nothing changes; the warn is now gated on an actual
  result difference.
- **run/switchF2.t 1/4 → OK.**  `tools/pclperl-for-tests` ignored `-F`
  entirely: it now parses `-F pattern` (strips a //, "" or '' wrap) and
  applies the perl-5.20 implication chain `-F` → `-a` → `-n`
  (perl #116190, which is exactly what the file tests).
- **re/reg_eval.t → expected-tsv row.**  All 8 cases drive
  `(?{…})`/`(??{…})` regex code blocks — already-documented
  not-supported.md §regex-code-blocks, same family as the existing
  pat_re_eval.t row.
- **run/fresh_perl.t stays DIFF: 53/91 match, 38 honest per-case
  failures** (shadow re-run after the fixes above).  Categories: perl
  *fatal-error fidelity* is the big one (expected output is perl's exact
  death message — `Can't call method "go" on an undefined value at -
  line 1.` — where PCL emits an unhandled-SBCL-condition dump); plus a
  real aliasing gap (`for (@a)` does not alias hole slots: case 28
  prints "  2" for perl's "2 2 2"); invalid-perl-detection cases
  (`local $lexical` must die at compile time — out of scope per
  CLAUDE.md §9); and one regex-code-block case (45).  Each still needs
  its own verdict; the shadow-repro recipe lives in task #127.

Caveat on the s316b row for **op/cond.t**: it reads TIMEOUT rather than
its expected-tsv XDIFF because the solo-phase memory cap killed it (see
the deep-nesting section below) — that is the guard working, not a status
change.

Original s309 run for reference (433 files): 45 OK, 85 XDIFF (expected),
31 NOTAP, 7 TIMEOUT, 265 DIFF.  The 167 crash-signature files were re-run with the
message-level signatures (runner now appends the normalized condition
message to `crash:*` sigs), giving the families below.  Raw data for this
run: session scratchpad `suite-all.tsv` / `crash-final.tsv`; regenerate any
time with `tools/run-perl-suite.pl --all --tsv FILE`.

## Fix families, largest first

| n | signature | files (sample) | verdict / next action |
|---|-----------|----------------|----------------------|
| 16 | `This Perl not built to support threads` | class/threads, op/*_thr, op/threads* | Oracle perl is unthreaded too (NOTAP both sides). Not a PCL gap; leave NOTAP. |
| 10 | `type-error: The value N is not of type hash-table` | comp/hints, op/coreamp, re/reg_nc_tie, re/regex_sets | One runtime family: magic hashes (`%^H`, `%+`/`%-` named-capture ties) resolve to a number where a hash is expected. Investigate `%^H` first (comp/hints). |
| 8 | `sb-c::input-error-in-load: read error during load` | comp/require, op/groups, uni/attrs, uni/caller, uni/gv, uni/stash | **ELIMINATED s309–s310d** (never "in-harness only" — all reproduced standalone): unicode pkg pre-declaration + `for $main::x` (s309); trailing-:: package calls `Bear::::baz` via `_merge_split_qualified_words` (s310c); credential specials `$< $> $( $)` as pipe-quoted vars + gid strings via alien getgroups (s310d); `cl_name` reader-safety net — residual colons pipe-quote, bad shapes fail as ONE undef-fn, never a file-killer (s310d). Left: uni/gv = binding-stack-exhausted (separate); unicode stash access `$온ꪵ::{…}` parses wrong (PPI one-Symbol gap, §symbol-table-hashes deferred). |
| 6 | `invalid number of arguments: N` | io/fs, op/current_sub, op/evalbytes, op/mkdir, op/sselect, op/sysio | **FIXED s310** (bare mkdir/rmdir `$_`, sysread/syswrite offsets+errors, chown(), 4-arg select, evalbytes named-unary). current_sub.t = deferred `__SUB__` → expected-tsv. Survivors show distinct next blockers (sysio unbound:I; sselect in-harness read-error; fs tempfile-vs-dir stub collision). |
| 6 | `Can't locate loadable object for module this module` | op/coresubs, op/hash-*, op/svflags, run/locale | Tests loading XS at runtime (XS::APItest-style). XS gap → pclxs (docs/xs-shim-design.md). Expected-tsv rows once confirmed. |
| 6 | `loadable object for module Unicode::Normalize` | re/anyof, re/reg_fold, uni/fold ... | Needs a Unicode::Normalize shim via sb-unicode (group with the \p{} plan, docs/unicode-property-regex-plan.md). |
| 4 | `Can't locate Config.pm` (note: `@INC contains: #S(p-box ...)`) | run/runenv*, run/switchM | **FIXED s310**: user code replacing @INC (`@INC='../lib'`) lost PCL's core shims — preamble now records `*p-core-inc-dirs*` (perl's compiled-in-defaults role), `p-find-module-in-inc` falls back to it; lib/Config.pm + `non_bincompat_options`. Survivors block on the runperl real-perl-spawn gap (not-supported §fork). |
| 4 | `type-error: The value nil is not of type real` | op/inc, op/stash, re/pat_re_eval, run/switchd | **FIXED s310**: whitespace between regex op and delimiter (`qr //`) broke all three raw-text regex parsers (`_parse_regex_content`, p-qr, p-regex). Also: `${qr//}` deref = string form; plain-string `=~` patterns now compile (pre-existing p-=~ gap). Survivors show distinct blockers (inc.t warning-capture; stash.t read-error; pat_re_eval unbound `$^N`). |
| 3 | `compiled-program-error ... (go :Arg_loop)` | io/msg, io/sem, io/shm | **FIXED s316** — two general bugs, both in `lib/sigtrap.pm`, neither about SysV IPC: (1) `goto LABEL` into a label PPI glues onto a LOOP or BARE BLOCK had no tagbody at all (crash on both pipelines) — `_lower_block` now opens `(tagbody :LBL …)` at the labeled statement, reusing the standalone-label lowering (value position uses the task-#64 setf-RET regime); (2) `%SIG` was EMPTY, so `exists $SIG{HUP}` was false and sigtrap's retry loop spun forever — one `*p-signal-numbers*` table now seeds %SIG's 67 keys and also feeds `%p-resolve-signal`, which used to answer 15 (TERM) for every name outside its 13-name `cond`. All three files now reach the XS blocker (`Can't locate loadable object for module IPC::SysV`) → the XS family below. |
| 3 | `control-stack-exhausted` | op/cond, op/utf8cache, re/speed | op/cond = deep-nesting, blessed XDIFF + deferred post-R1 (see below). utf8cache + re/speed share only the *signature* — triage separately as real bugs (utf8cache dies after 2 tests on ordinary code; re/speed likely cl-ppcre recursion on long strings). |
| 2 | `loadable object for module Storable` | uni/greek, uni/latin2 | Storable XS gap → pclxs or pure-perl shim. |
| 2 | `Can't locate Foo.pm` | op/overload_integer, op/override | Test-fixture .pm files not found — shadow-t @INC/cwd issue in the harness fixture, not PCL semantics. |

Remaining singletons: see `crash-final.tsv`.  Non-crash TAP-diff files
(~100) are listed in the s309 triage report; biggest: comp/utf.t (500 rows,
UTF-16 sources), io/through.t + io/crlf_through.t (~520 rows, PerlIO layer
semantics), op/cmpchain.t (274 rows, chained comparisons), op/const-optree.t
(146 rows), comp/retainedlines.t (90 rows, `@{"_<..."}` debugger lines).

## The op/cond.t deep-nesting bug (root cause of the s308/s309 desktop OOM kills)

**DECISION (user, s309): deferred until after Release 1** — blessed as
expected-divergence; full verdict + the post-R1 revisit checklist (flat-width
quadratic check, index-range fix) in `not-supported.md` §pathological-nesting.

`t/op/cond.t` builds a 20,000-deep right-nested ternary string (220 KB) and
`eval`s it.  Measured (2026-07-23, tern-N files = the exact cond.t shape):

| depth | source | pl2cl peak RSS | PPI-only RSS | generated CL |
|-------|--------|----------------|--------------|--------------|
| 2,500 | 27 KB | 335 MB | – | 31 KB, loads+runs OK (134 MB total) |
| 5,000 | 55 KB | 785 MB | – | |
| 10,000 | 110 KB | 2.1 GB | 117 MB | |
| 20,000 | 220 KB | 6.75 GB | 213 MB | control-stack-exhausted |

- **Transpiler memory is quadratic** (each doubling ×2.3→×3.1): PExpr's
  recursive descent copies each parenthesized subexpression into fresh
  arrays per nesting level (`@$e[...]` slices, e.g. the ternary arm's
  `@condition/@true_expr/@false_expr`), and every live recursion frame holds
  its copy → sum over levels ≈ n²/2 SV slots ≈ the observed ~6.7 GB.
  PPI is linear and innocent (~10 KB/level).  Fix direction: parse index
  ranges over a shared element array, or reduce right-nested chains
  iteratively.  Also fixes the quadratic wall time (36 s at 20k).
- **SBCL side is lean** (CL output ~12 bytes/level) but compiling a
  20k-deep nested form exhausts the control stack at load.  Fix direction:
  larger `--control-stack-size` for generated-code compiles, or flatten
  deep right-nested ternary/if chains in codegen.
- Until fixed, `op/cond.t` is listed in the runner's `%HEAVY` set and runs
  in a solo phase (no parallel jobs beside it); the sweep itself is wrapped
  in a `systemd-run --user` scope with `MemoryMax=10G` so a balloon can
  never OOM the desktop session again.
