# perl-suite crash-family triage (s309, 2026-07-23)

Full `--all` sweep (433 files): 45 OK, 85 XDIFF (expected), 31 NOTAP,
7 TIMEOUT, 265 DIFF.  The 167 crash-signature files were re-run with the
message-level signatures (runner now appends the normalized condition
message to `crash:*` sigs), giving the families below.  Raw data for this
run: session scratchpad `suite-all.tsv` / `crash-final.tsv`; regenerate any
time with `tools/run-perl-suite.pl --all --tsv FILE`.

## Fix families, largest first

| n | signature | files (sample) | verdict / next action |
|---|-----------|----------------|----------------------|
| 16 | `This Perl not built to support threads` | class/threads, op/*_thr, op/threads* | Oracle perl is unthreaded too (NOTAP both sides). Not a PCL gap; leave NOTAP. |
| 10 | `type-error: The value N is not of type hash-table` | comp/hints, op/coreamp, re/reg_nc_tie, re/regex_sets | One runtime family: magic hashes (`%^H`, `%+`/`%-` named-capture ties) resolve to a number where a hash is expected. Investigate `%^H` first (comp/hints). |
| 8 | `sb-c::input-error-in-load: read error during load` | comp/require, op/groups, uni/attrs, uni/caller, uni/gv, uni/stash | Generated .lisp file SBCL cannot read — encoding (utf8/latin1 bytes in emitted source?). Inspect one generated file byte-level. |
| 6 | `invalid number of arguments: N` | io/fs, op/current_sub, op/evalbytes, op/mkdir, op/sselect, op/sysio | Builtin arity family (pl-* called with unsupported arg count, e.g. optional-arg forms). Cheap fixes; do per-file. |
| 6 | `Can't locate loadable object for module this module` | op/coresubs, op/hash-*, op/svflags, run/locale | Tests loading XS at runtime (XS::APItest-style). XS gap → pclxs (docs/xs-shim-design.md). Expected-tsv rows once confirmed. |
| 6 | `loadable object for module Unicode::Normalize` | re/anyof, re/reg_fold, uni/fold ... | Needs a Unicode::Normalize shim via sb-unicode (group with the \p{} plan, docs/unicode-property-regex-plan.md). |
| 4 | `Can't locate Config.pm` (note: `@INC contains: #S(p-box ...)`) | run/runenv*, run/switchM | Two bugs: missing lib/Config.pm shim AND a p-box leaking un-stringified into @INC (the `-I` path push). Fix the box leak first — it is generic. |
| 4 | `type-error: The value nil is not of type real` | op/inc, op/stash, re/pat_re_eval, run/switchd | undef reaches arithmetic in the runtime — nil box where number expected. Reproduce with op/inc.t first divergence. |
| 3 | `compiled-program-error ... (go :Arg_loop)` | io/msg, io/sem, io/shm | Codegen emits `(go :Arg_loop)` outside its tagbody (SysV IPC files). Generic control-flow bug worth a look despite niche files. |
| 3 | `control-stack-exhausted` | op/cond, op/utf8cache, re/speed | Deep-nesting family — see below. |
| 2 | `loadable object for module Storable` | uni/greek, uni/latin2 | Storable XS gap → pclxs or pure-perl shim. |
| 2 | `Can't locate Foo.pm` | op/overload_integer, op/override | Test-fixture .pm files not found — shadow-t @INC/cwd issue in the harness fixture, not PCL semantics. |

Remaining singletons: see `crash-final.tsv`.  Non-crash TAP-diff files
(~100) are listed in the s309 triage report; biggest: comp/utf.t (500 rows,
UTF-16 sources), io/through.t + io/crlf_through.t (~520 rows, PerlIO layer
semantics), op/cmpchain.t (274 rows, chained comparisons), op/const-optree.t
(146 rows), comp/retainedlines.t (90 rows, `@{"_<..."}` debugger lines).

## The op/cond.t deep-nesting bug (root cause of the s308/s309 desktop OOM kills)

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
