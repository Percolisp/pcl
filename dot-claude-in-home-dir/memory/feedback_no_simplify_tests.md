---
name: Never simplify tests to make them pass
description: When a Pl/t/ test fails, fix the code — do not weaken the test to avoid the failure
type: feedback
---

Do NOT change a test's Perl code to a weaker/simpler form just to make it green. If a test fails, either fix the underlying bug or understand why the test is legitimately wrong.

**Why:** Weakening a test hides real bugs. The test suite exists to detect regressions and verify behavior. A green test that doesn't test what it claims is worse than a red test.

**Example of what NOT to do:**
```perl
# Original test (fails because Test::More loading crashes in PCL):
test_cl('is(reverse("abc"), "cba") as function arg',
    'use Test::More tests => 1; is(reverse("abc"), "cba", "simple reverse");',
    "1..1\nok 1 - simple reverse\n");

# WRONG — do NOT change to this simpler form:
test_cl('reverse("abc") via assignment gives "cba"',
    'my $x = reverse("abc"); print $x eq "cba" ? "ok" : "fail", "\n";',
    "ok\n");
```

The correct fix was to write the test so it doesn't depend on Test::More loading, but still exercises the same behavior (reverse as function argument):
```perl
test_cl('reverse("abc") as function argument gives "cba"',
    'sub check_eq { print $_[0] eq $_[1] ? "ok" : "fail: got $_[0]", "\n" } check_eq(reverse("abc"), "cba");',
    "ok\n");
```

**How to apply:** When writing Pl/t/ regression tests, if the "obvious" test form fails, diagnose WHY it fails before changing the test. If it fails due to a separate unrelated bug (e.g., Test::More loading), work around THAT specific issue while keeping the test semantically equivalent. Never reduce what the test is actually testing.

This principle is also in CLAUDE.md: "Never Simplify Tests: When a test fails, fix the code, not the test."
