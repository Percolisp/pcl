# Plan: Running Perl's Own Test Suite with PCL

## Overview

Perl's source distribution contains extensive tests in `t/` that verify Perl semantics.
These tests would provide authoritative verification that PCL matches Perl behavior.

Location: `/home/bernt/perl5/perlbrew/build/perl-5.40.3/perl-5.40.3/t/`

## Test Infrastructure

### Perl's test.pl

The tests use a custom minimal test framework in `t/test.pl` that provides:
- `plan(N)` / `done_testing()` - declare test count
- `ok($test, $name)` - basic assertion
- `is($got, $expected, $name)` - equality test
- `isnt($got, $expected, $name)` - inequality test
- `like($got, $regex, $name)` - regex match
- `unlike($got, $regex, $name)` - regex non-match
- `cmp_ok($a, $op, $b, $name)` - comparison with operator
- `pass($name)` / `fail($name)` - explicit pass/fail
- `skip($reason, $count)` - skip tests
- `skip_all($reason)` - skip entire file
- `diag($msg)` / `note($msg)` - diagnostic output

### Strategy

**Option A: Port test.pl to PCL**
- Transpile test.pl itself
- Advantages: Uses same infrastructure as Perl
- Challenges: test.pl uses some magic (`$|`, `$\`, etc.)

**Option B: Create PCL-native test.pl**
- Write a CL version that provides the same interface
- Advantages: Can be optimized for CL, avoid porting issues
- Implementation: Simpler, just need TAP output

**Recommendation: Option B** - Create a minimal CL test library that outputs TAP format.

## Phase 1: Test Infrastructure

### 1.1 Create `pcl-test.lisp`

```lisp
;; Minimal TAP test framework for PCL
;; Provides: plan, ok, is, isnt, like, unlike, pass, fail, skip, done_testing

(defvar *test-count* 0)
(defvar *planned-tests* nil)

(defun plan (n)
  (setf *planned-tests* n)
  (format t "1..~A~%" n))

(defun ok (test &optional name)
  (incf *test-count*)
  (format t "~A ~A~@[ - ~A~]~%"
          (if test "ok" "not ok") *test-count* name)
  test)

(defun is (got expected &optional name)
  (ok (equal got expected) name))
;; etc.
```

### 1.2 Create `test.pm` wrapper

A Perl module that PCL can `use` which provides the test.pl interface
but generates code that uses pcl-test.lisp.

## Phase 2: Identify Target Tests

### 2.1 Simple Tests (No Dependencies)

Tests in `t/op/` that don't use:
- `use Config;` (XS module)
- `tie` / `overload` (not implemented)
- `format` (not implemented)
- `eval "string"` (deferred)
- Threading features

**Candidate simple tests** (~30 lines or less):
- `cond.t` - ternary operator (31 lines, but uses eval string)
- `bool.t` - boolean values (37 lines)
- `defined.t` - defined() function (20 lines)
- `sleep.t` - sleep function (22 lines)
- `print.t` - print statement (43 lines)

### 2.2 Test Complexity Analysis

```bash
# Find tests without complex dependencies
for f in t/op/*.t; do
  if ! grep -qE "use Config|tie |format |eval \"|require.*threads" "$f"; then
    echo "$f"
  fi
done
```

## Phase 3: Test Runner

### 3.1 Create `run-perl-test.pl`

```perl
#!/usr/bin/env perl
# Transpile and run a Perl test file with PCL

my $test_file = shift or die "Usage: $0 <test.t>\n";

# Transpile
my $cl_code = `./pl2cl $test_file`;

# Write to temp file
my $cl_file = "/tmp/pcl-test-$$.lisp";
open my $fh, '>', $cl_file;
print $fh $cl_code;
close $fh;

# Run with SBCL
my $output = `sbcl --noinform --non-interactive \\
  --load cl/pcl-runtime.lisp \\
  --load cl/pcl-test.lisp \\
  --load $cl_file 2>&1`;

print $output;
unlink $cl_file;
```

### 3.2 Batch Runner

```perl
#!/usr/bin/env perl
# Run multiple Perl tests and report results

use TAP::Harness;
# ... collect results, report pass/fail
```

## Phase 4: Iterative Testing

### 4.1 Start with Simplest Tests

1. `t/op/defined.t` - tests defined() function
2. `t/op/bool.t` - boolean semantics
3. `t/op/cond.t` - ternary operator (skip eval test)
4. `t/op/print.t` - print statement

### 4.2 Track Issues

For each test failure:
1. Identify missing feature or bug
2. Create PCL issue/TODO
3. Fix or mark as known limitation
4. Re-run test

### 4.3 Expand Coverage

Progressively add more complex tests:
- `t/op/array.t` - array operations (195 tests)
- `t/op/hash.t` - hash operations
- `t/op/substr.t` - string operations
- `t/op/arith.t` - arithmetic
- etc.

## Implementation Order

1. **Week 1**: Create pcl-test.lisp with basic TAP output
2. **Week 1**: Create test runner script
3. **Week 1**: Run first 5 simple tests, fix issues
4. **Week 2**: Expand to 20 tests
5. **Ongoing**: Add tests as features are implemented

## Success Metrics

- **Phase 1**: 5 tests passing
- **Phase 2**: 20 tests passing
- **Phase 3**: 50 tests passing
- **Long-term**: 100+ t/op/ tests passing

## Notes

- Some tests explicitly test Perl internals (refcounts, etc.) - skip these
- Some tests are platform-specific - may need to skip on CL
- Focus on behavioral tests, not implementation tests
