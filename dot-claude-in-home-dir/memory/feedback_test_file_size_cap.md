---
name: feedback-test-file-size-cap
description: "Don't grow the biggest Pl/t test file — the largest file bounds the parallel suite's wall time; start a new transpile-test-NN.t instead"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: b6e5cae0-312b-4832-988c-ef41649df64e
  modified: 2026-08-01T20:21:54.275Z
---

The user (2026-07-28): "Don't make too big test files. The biggest one is the limit for running tests."

**Why:** `prove -j8` parallelizes across files, so total wall time ≈ the slowest single file. Each `test_transpile` spawns SBCL (+ a perl oracle run), so per-file test count directly sets that bound. Adding tests to an already-large file makes the whole gate slower even though the suite is parallel.

**How to apply:** Before adding a Pl/t test, check per-file test counts; never add to the current largest file. When the smallest transpile-test file approaches ~50 tests, start a new `transpile-test-NN.t` (copy the header/helpers; see transpile-test-07.t created s316g). This refines the older "add to the smallest transpile-test file" rule in CLAUDE.md §6 — small files are fine to grow, the cap is on the big ones. Related: [[project-test-core-fast-path]].

**RESTATED and REFINED by the user 2026-08-01 (s321):** first "Don't make test files so big that they take minutes to run. Please don't add more to `transpile-test-07.t`, create a new file next time", then the clarification that matters — **"if the tests are few (or fast), it is not a problem. The target is to not let the maximum running time get too big."**

So the metric is **wall time per file, not row count**. A file with many cheap rows is fine; a file with a few `test_transpile` rows may not be (each runs a perl oracle AND an SBCL transpile+run). Measure with `prove --timer Pl/t/<file>` before adding, and compare against the current slowest file — that file is what the whole `-j8` gate waits for.

-07 reached 45 rows in s321 and the user closed it to new tests. Files as of s321: 01, 01b, 02, 03, 04, 04b, 05, 06, 07, 08 — **next new file is `transpile-test-09.t`**.
