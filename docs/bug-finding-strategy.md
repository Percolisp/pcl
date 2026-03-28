# PCL: Bug-Finding and Triage Strategy

**Written:** 2026-03-28

The sweep tells you *how many* tests fail per file.  It doesn't tell you *why*, which
root causes are shared across files, or which files are one fix away from fully passing.
This document describes how to work smarter.

---

## The Core Problem

```
sweep → file counts → pick a file → run it → read failures → fix → re-sweep
```

This loop is O(files).  Many failures across many files share the same root cause.
One fix to one runtime function can unblock dozens of tests across many files.
The key is to identify those shared root causes *before* diving into any individual file.

---

## Tool 1 — The Error Aggregator Script

Run all partially-failing tests, capture their first error, group by error type.

```perl
#!/usr/bin/env perl
# Usage: perl docs/scripts/aggregate-errors.pl
# Runs each partial-passer, extracts the failure reason, groups by type.
use strict; use warnings;
use File::Glob qw(bless);

my @files = glob("perl-tests/*.t");
my %groups;   # error-type => [ [file, test#, message], ... ]

for my $file (sort @files) {
    next unless -f $file;
    my $out = `perl run-perl-test.pl $file 2>&1`;
    # Extract "not ok N - description" lines
    my @failures = ($out =~ /^not ok \d+ - (.+)/mg);
    # Extract first SBCL crash message
    my ($crash) = ($out =~ /Unhandled (\S+) in thread/);
    # Extract first UNDEFINED-FUNCTION name
    my ($undef_fn) = ($out =~ /The function (\S+) is undefined/);

    my $key = $undef_fn   ? "UNDEF-FN:$undef_fn"
            : $crash      ? "CRASH:$crash"
            : @failures   ? "FAIL:" . substr($failures[0], 0, 60)
            : next;

    push @{$groups{$key}}, { file => $file, count => scalar @failures };
}

# Print sorted by total tests affected (descending)
for my $key (sort { sum_count($groups{$b}) <=> sum_count($groups{$a}) } keys %groups) {
    my $total = sum_count($groups{$key});
    my @files = map { $_->{file} } @{$groups{$key}};
    printf "%5d tests  %d files  %s\n    %s\n\n",
           $total, scalar @files, $key, join(", ", @files);
}

sub sum_count { my $aref = shift; my $n=0; $n += $_->{count} for @$aref; $n }
```

**What this shows you:**
```
  983 tests  1 file   CRASH:SB-C::INPUT-ERROR-IN-LOAD
             sprintf2.t

   47 tests  8 files  UNDEF-FN:PL-OVERLOAD-DISPATCH
             sort.t, hashassign.t, aassign.t, warn.t ...

   21 tests  3 files  UNDEF-FN:P-PACK
             infnan.t, pack.t, vec.t
```

Now you know: implement `p-pack` once → fixes 3 files.  Much better than investigating
each file in isolation.

---

## Tool 2 — Near-Miss Ordering

Files sorted by failure count ascending (from current sweep output).  These are the
fastest wins — one or two fixes away from a fully-passing file:

```
perl sweep-perl-tests.pl --jobs 8 2>&1 | \
    grep -E '^\S+\.t\s' | \
    awk '$3 > 0' | \
    sort -k3 -n | head -20
```

Expected output (approximate, based on current sweep):
```
delete.t         38     1
local.t         127     2
auto.t           45     2
do.t             46     6
each_array.t     52     5
repeat.t         43     5
concat.t        232     2
split.t         202    12
index.t         108    12
```

**Working through near-misses is fast:** each one is a focused investigation into 1-5
failures, usually a single root cause.

---

## Tool 3 — The Focused Investigation Protocol

For any individual file, follow these steps in order.  Do not skip ahead.

### Step 1 — Check the doc first
```
grep -A 10 "^### filename.t" docs/test-failures-categorized.md
```
If the file is characterized, the root cause is already known.  Skip to step 4.

### Step 2 — Get the failing test numbers
```
perl run-perl-test.pl perl-tests/file.t 2>&1 | grep "^not ok"
```
Note the test numbers.  Look at the test source to understand what they test.

### Step 3 — Get the first error
```
perl run-perl-test.pl perl-tests/file.t 2>&1 | head -40
```
Look for:
- `Unhandled UNDEFINED-FUNCTION` → what function?  Add it to the runtime.
- `Unhandled SIMPLE-ERROR: ...` → what did it say? Usually a runtime semantic bug.
- `Unhandled SB-C::INPUT-ERROR-IN-LOAD` → the generated CL doesn't parse.  Check
  codegen with `./pl2cl file.t | grep -A5 "failing-section"`.
- `not ok N - description` with no crash → runtime gave wrong value.

### Step 4 — Transpile and inspect
```
./pl2cl perl-tests/file.t > /tmp/file.lisp 2>/dev/null
# Find the region around the failing test:
grep -n "test.*N\|ok.*N" /tmp/file.lisp | head -5
```
Read the generated CL around the failing test.  Is the pattern obviously wrong?
Compare to what a correct transpilation should look like.

### Step 5 — Write a focused `Pl/t/` test BEFORE fixing

```perl
# Pl/t/file-01.t — isolated reproduction of the failure
{
    my $out = run_pl(q{ ... minimal reproduction ... });
    like($out, qr/expected/, 'what this tests');
}
```

This gives you:
- Fast iteration (no SBCL startup for each fix attempt — just `prove -v Pl/t/file-01.t`)
- A regression test that prevents the bug from coming back
- Documentation of what was broken

### Step 6 — Fix, verify, sweep

```bash
prove -v Pl/t/file-01.t            # must pass
prove -j8 Pl/t/                    # full suite, must still be all green
perl sweep-perl-tests.pl --jobs 8 perl-tests/file.t   # check improvement
```

---

## Tool 4 — Root-Cause Clustering

Some root causes appear with the same SBCL error across many files.  Current known
clusters (as of 2026-03-28, 1957 failing):

| Root Cause | Files affected | Approx tests |
|-----------|---------------|-------------|
| `use overload` (stringify, numify, ops) | sprintf2.t, sort.t, concat2.t, hashassign.t | ~1050 |
| `pack`/`unpack` missing formats | infnan.t, pack.t, length.t | ~200 |
| wantarray (deferred by policy) | time.t, do.t, splice.t, context.t, list.t | ~80 |
| `$SIG{__DIE__}` not called | die.t, warn.t | ~50 |
| named inner sub closures | closure.t | ~13 |
| flip-flop `..` in scalar context | flip.t | 3 |

Working top-to-bottom on this table is far more efficient than working file-by-file.

---

## Tool 5 — Regression-First Development

When fixing a bug:

1. **Reproduce in `Pl/t/` first.** Write the smallest possible test that fails.
2. **Fix the code** until the `Pl/t/` test passes.
3. **Check the sweep** to confirm the perl-tests improvement matches expectation.
4. **Update `docs/test-failures-categorized.md`** with the root cause and fix summary.

This builds a growing library of regression tests that catch reintroduced bugs immediately,
and keeps `docs/test-failures-categorized.md` as an accurate map of remaining work.

---

## Tool 6 — Updating `test-failures-categorized.md`

After every session, update the doc:
- Move fixed items from "Partially Passing" to the "Fully Passing" list
- Add new characterizations for files investigated but not yet fixed
- Update pass/fail counts (they drift as fixes land)
- Add new root-cause rows to the cluster table above

The doc is only useful if it reflects the current state.  Stale docs are worse than
no docs because they send you down already-investigated paths.

---

## Suggested Session Workflow

```
1. Read docs/test-failures-categorized.md — what's already known?

2. Run the error aggregator (Tool 1) or look at near-misses (Tool 2)
   to pick the highest-ROI target.

3. For each target:
   a. Step through the focused investigation protocol (Tool 3)
   b. Write Pl/t/ test first (Tool 5 step 1)
   c. Fix
   d. Verify with prove -j8 Pl/t/
   e. Check sweep for improvement

4. At end of session:
   a. Update docs/test-failures-categorized.md
   b. Update docs/session-log.md
   c. Update MEMORY.md current-status entry
```

---

## Anti-Patterns to Avoid

| Anti-pattern | Why bad | Better approach |
|-------------|---------|-----------------|
| Picking a file with 100+ failures as first target | One fix rarely helps all 100; you spend a session on one file | Start with near-misses (≤5 failures) |
| Fixing the sweep output before understanding root cause | You might fix a symptom, not the cause; same error recurs in a different form | Always read the actual SBCL error first |
| Commenting out failing tests | Hides bugs, masks regressions | Only comment out when root cause is a documented not-supported feature, after discussion |
| Running full sweep after every one-line fix | Sweep takes 3+ min; wastes time | Use `perl sweep-perl-tests.pl --jobs 1 perl-tests/target.t` for spot checks; full sweep at session end |
| Investigating a file that's already characterized | Wastes time re-discovering known causes | Always check `test-failures-categorized.md` first |
| Writing Pl/t/ test after the fix | Doesn't catch regressions in that session; future fix may reintroduce the bug | Write the test first, before touching any code |
