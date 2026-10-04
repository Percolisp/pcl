---
name: debugging_hangs_crashes
description: How to debug SBCL hangs and crashes in PCL test runs
metadata: 
  node_type: memory
  type: feedback
  originSessionId: feb4ddb9-b888-46cd-bf26-2e8b023e6a18
  modified: 2026-08-16T08:12:16.256Z
---

When a PCL Perl test hangs or crashes, follow docs/debugging-hangs-crashes.md.

Key rules:
1. **Distinguish hang vs crash first**: `timeout 10 perl run-perl-test.pl foo.t` — returns fast = crash, times out = hang
2. **Run SBCL directly** (not via run-perl-test.pl backtick) to see error output: `cd perl-tests && sbcl --load ../cl/pcl-runtime.lisp --load ../cl/pcl-test.lisp --load /tmp/foo.lisp 2>&1`
3. **NEVER use `~S` to print p-box values** — the struct printer recurses and hangs
4. **Always add `force-output`** after debug `format t` calls — output is buffered
5. **Check generated CL before running** — many bugs visible from `./pl2cl` output
6. **Test regressions immediately**: after any fix, run `prove -j8 Pl/t/` and 2-3 related Perl tests BEFORE running the full sweep
7. **Take the STATE, not just the fds** (s405): catch the stalled process and read
   `/proc/<pid>/status`. **State R = a SPIN** (a retry loop — look at what the
   program last asked the OS for), **State S = a BLOCK** (a read/wait — look at
   `/proc/<pid>/fd` and wchan). #346's hang read as a descriptor problem from
   the fd list alone; State R said "retry loop" and the cause was `open` on a
   closed fd returning a stream instead of EBADF (#358).
8. **A spin OUTLIVES the run** (s405): `timeout` kills the SBCL it started, not
   the grandchild `pclperl-for-tests` spawned inside it. One orphan burned a
   full core for an hour across later measurements. After any hang, check
   `ps -eo pid,etimes,pcpu,args | grep sbcl` and kill what is left. Same family
   as #273's orphaned `pl2cl --server`.
9. **A COMPILER runaway takes the MACHINE, not the run (s435)**: a
   self-recursive helper in `Pl/Parser.pm` grew one perl to 7.7 GB, ate the
   whole 4 GB swap and hung the box until the kernel's global oom-killer took
   it. The compiler side has NO cap (only `tools/run-perl-suite.pl` wraps
   itself in a `systemd-run` MemoryMax scope; task **#471** is the gap).
   So: **run any compiler-side probe under a cap** —
   `( ulimit -v 4000000; ./pl2cl < file )` — and it dies in ~5 s with perl's
   own "Out of memory" plus the preceding **"Deep recursion on subroutine
   <name> at <file> line N"**, which names the bug. Unbounded, that same
   warning scrolls past and changes nothing: a warning does not stop the
   allocation. Check `dmesg -T | grep -i "oom\|Killed process"` after any
   whole-machine hang — the kernel records the pid, the RSS and the cmd.


**Why:** In session 78, 4+ hours spent on a hang in test-to-scalar because:
- Didn't use `timeout` to confirm it was a hang (not crash)
- Used `~S` format which itself caused the hang
- Didn't check generated CL before running
- Ran full sweep before smoke-testing related files (missed sprintf.t regression)
