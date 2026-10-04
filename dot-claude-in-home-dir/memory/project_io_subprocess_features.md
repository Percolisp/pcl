---
name: project_io_subprocess_features
description: TODO — socket/<>-diamond/runperl/fork are feasible (not out-of-scope); per-item blockers and effort
metadata: 
  node_type: memory
  type: project
  originSessionId: 3bdb78de-1964-43b1-8c62-ddd8f283d386
---

TODO (user-requested 2026-06-26): the t/io files I'd called "out of scope"
(argv, socket, pipe, fork) are mostly **not-yet-built, not impossible**. Pick one
up next time — `socket` or the `<>`/`@ARGV` diamond are the cleanest, highest-value.

Per item:

- **pipe** — ALREADY WORKS. `p-pipe`/`%p-pipe-impl` in `cl/pcl-runtime.lisp` use
  `sb-posix:pipe` + fd-streams (in-process). Not a blocker.

- **socket / socketpair** — NOT implemented yet, but **PLANNED in detail:
  `docs/socket-impl-plan.md`** (written 2026-06-27). sb-bsd-sockets single-
  process TCP loopback **VERIFIED working, no fork needed** (spike in the doc).
  Layering: `lib/Socket.pm` shim = constants + `sockaddr_in`/`inet_aton` via
  pack (right layer); runtime = core builtins `socket`/`bind`/`connect`/`listen`/
  `accept`/`send`/`recv`/… on socket objects + packed sockaddr bytes; 4-edit
  pattern each. Key non-obvious bit: store the socket OBJECT in the fh, extend
  `p-get-stream` to lazily `socket-make-stream`+cache (`*p-socket-streams*`) so
  print/`<$sock>`/close just work. Scope order: AF_INET TCP first → AF_UNIX →
  UDP → socketpair. START HERE next session.

- **`<>` diamond over `@ARGV` + in-place `$^I`** — ✅ DONE 2026-06-27 (commit
  8a305a3). `<>`/`<ARGV>` iterate @ARGV (open each, STDIN fallback, set `$ARGV`,
  cumulative `$.` seeded across files), scalar+list ctx; in-place `$^I` (backup
  ext, `''` no-backup, `*`-subst, skip-on-rename-fail). Fixed latent bug: `$^I`
  was missing from `%SPECIAL_VARS` (mapped to wrong symbol under :invert →
  runtime `|$^I|` unreachable). Runtime `%p-readline-argv`/`%p-inplace-*`. Test
  `Pl/t/diamond-01.t`. NB in-place redirects `*standard-output*` via setf only
  while a real <> file is open.

- **runperl-based tests** (argv.t, dup.t, fflush.t, bom.t, many others) — `$^X`
  in PCL points at the **real system `perl`** (`command -v perl`, see
  `|$^X|` defvar in pcl-runtime.lisp ~line 597). So `runperl` spawns REAL perl;
  the only blocker is runperl's plumbing (arg quoting, feeding stdin, capturing
  output via pipe-`open`/backticks). Implementable. **Catch:** passing those tests
  then validates real perl in the child, NOT PCL — to exercise PCL end-to-end,
  re-point `$^X` at the `pcl` runner (rollout plan P1) so children run transpiled
  code.

- **fork** — the ONE genuinely hard case (`Config` `d_fork=>''`). SBCL is a
  multithreaded image; POSIX `fork()` keeps only the calling thread in the child,
  GC/finalizer threads vanish, held locks freeze → a child that does real Lisp
  work can deadlock/crash. `sb-posix:fork` exists and **fork+exec is safe**, but
  **fork-then-run-Perl-code** (what most fork tests do) is fragile. Approach only
  with care (single-threaded-child contract, or restrict to the exec path).

See [[project_pcl_rollout_plan]] / `docs/pcl-rollout-plan.md` for the runner, and
`docs/perl-test-suite-survey.md` t/io table for which files each unlocks.
