---
name: project_suite_runner_oom_and_cond_t
description: "Desktop OOM kills root-caused to op/cond.t 20k-ternary eval — pl2cl PExpr is O(n²) memory on deep nesting; suite runner now hardened (group-reap, systemd scope, %HEAVY solo phase, message-level crash sigs)"
metadata: 
  node_type: memory
  type: project
  originSessionId: 4eee65cb-90ae-4b38-a602-7e0b8094732a
  modified: 2026-07-22T22:21:46.447Z
---

**Desktop OOM kills (2026-07-22 and -23, "Ubuntu says you filled memory"):** the
kernel OOM killer shot a ~6 GB perl inside the terminal cgroup, killing the
GNOME-reported scope + the Claude session. The process was the `pl2cl --server`
eval-transpiler (spawned by the SBCL runtime, `cl/pcl-runtime.lisp` ~:9902)
ballooning on **`t/op/cond.t`'s 20,000-deep nested-ternary `eval`**.

**Root cause (measured):** PExpr recursive descent copies each paren
subexpression into fresh arrays per nesting level (`@$e[..]` slices; ternary
arm `@condition/@true_expr/@false_expr`), every live frame holds its copy →
**O(n²) live memory**: 335 MB / 785 MB / 2.1 GB / 6.75 GB at depths
2.5k/5k/10k/20k (and quadratic wall time, 36 s at 20k). PPI is linear/innocent
(~10 KB/level). Generated CL is ~12 B/level; SBCL fails only by
control-stack-exhaustion at 20k (separate fix: bigger --control-stack-size or
codegen flattening). Fix direction for PExpr: index ranges over a shared
element array, or iterative reduction of right-nested chains. Filed in
`docs/perl-suite-triage.md`. NOT yet fixed — op/cond.t still needs 6.7 GB
per sweep (contained, solo phase).

**Runner hardening shipped (852d260, 41ebd45, 0605cf6, 8881a15):** workers are
process-group leaders and self-KILL their group after writing results (reaps
orphans from fork-heavy tests — timeout(1) only kills its direct child);
straggler kill targets `-$pid`; pl2cl step has `timeout`; `ulimit -v 4G` on
perl steps; SIGINT/TERM forwarded to groups; runner re-execs itself under
`systemd-run --user --scope -p MemoryMax=10G` (PCL_SUITE_SCOPED guard) so a
balloon can only OOM the sweep, never the desktop; `%HEAVY` set (op/cond.t)
runs in a solo phase after the parallel bulk; crash sigs append the normalized
condition message → sweeps self-triage (see [[project_perl_suite_survey]]).

**Gotcha (harness):** a `setsid nohup ... &` process launched from a Bash tool
call does NOT survive the call's sandbox cleanup — long sweeps must run as
foreground chunked calls or via run_in_background (10-min cap).

s309 triage output: `docs/perl-suite-triage.md` = the #25 fix-family table
(top: %^H/%+ magic-hash type-error ×10, generated-lisp read-error-in-load ×8,
builtin arity ×6, p-box leaks into @INC + Config.pm shim ×4, nil-not-real ×4,
`(go :Arg_loop)` outside tagbody ×3; threads ×16 = oracle-also-NOTAP, not a
PCL gap).
