# PCL Runtime Test Plan

This document tracks end-to-end tests that verify Perl code runs identically when transpiled to Common Lisp.

## Test Order

Tests are ordered by dependency - each test builds on features from previous tests.

| # | Test File | Features Tested | Status |
|---|-----------|-----------------|--------|
| 1 | `test-01-basic.pl` | Arithmetic, strings, comparisons, loops | ✅ Pass |
| 2 | `test-02-control.pl` | if/elsif/else, unless, while, until, for, nested | ✅ Pass |
| 3 | `test-03-subs.pl` | Sub definitions, signatures, defaults, recursion | 🔨 Created |
| 4 | `test-04-arrays.pl` | Array access, push/pop/shift, iteration | 🔨 Created |
| 5 | `test-05-hashes.pl` | Hash access, exists/delete, keys/values | 🔨 Created |
| 6 | `test-06-refs.pl` | Scalar/array/hash refs, anon refs, nested | 🔨 Created |
| 7 | `test-07-objects.pl` | bless, method calls, OO patterns | 🔨 Created |
| 8 | `test-08-advanced.pl` | Nested structures, complex expressions | TODO |

## Running Tests

```bash
# Run Perl version
perl examples/test-01-basic.pl

# Translate to CL
./pl2cl examples/test-01-basic.pl > examples/test-01-basic.lisp

# Run CL version (after manual wrapping)
sbcl --load cl/pcl-runtime.lisp --load examples/test-01-basic-run.lisp

# Compare outputs
diff <(perl examples/test-01-basic.pl) \
     <(sbcl --noinform --non-interactive --load cl/pcl-runtime.lisp \
            --load examples/test-01-basic-run.lisp 2>&1 | grep -v "^PCL Runtime")
```

## Test Descriptions

### Test 1: Basic Operations (test-01-basic.pl)
- Arithmetic: +, -, *, /, %, **
- Increment/decrement: ++, --
- Compound assignment: +=, -=, *=, .=
- String operations: concatenation, length, x (repeat)
- Comparisons: numeric (==, <, >) and string (eq, lt, gt)

### Test 2: Control Flow (test-02-control.pl)
- Conditionals: if, elsif, else, unless
- Ternary operator: ? :
- Loops: while, until, for (C-style), foreach
- Loop control: last, next (when implemented)

### Test 3: Subroutines (test-03-subs.pl)
- Sub definition with `sub name { }`
- Parameters via @_ and shift
- Named parameters with signatures (5.32+)
- Default parameter values
- Return values (explicit and implicit)
- Calling subs with various argument counts
- Recursive subs (factorial, fibonacci)

### Test 4: Arrays (test-04-arrays.pl)
- Array creation: my @arr = (1, 2, 3)
- Element access: $arr[0], $arr[-1]
- Assignment: $arr[1] = 10
- push, pop, shift, unshift
- Array length: scalar(@arr), $#arr
- Array slices: @arr[1, 3, 5]
- Iteration: foreach

### Test 5: Hashes (test-05-hashes.pl)
- Hash creation: my %h = (key => value)
- Element access: $h{key}
- Assignment: $h{newkey} = value
- keys(), values()
- exists(), delete()
- Hash slices: @h{qw(a b c)}
- Iteration: foreach keys

### Test 6: References (test-06-refs.pl)
- Scalar references: \$x, $$ref
- Array references: \@arr, @$ref, $ref->[0]
- Hash references: \%hash, %$ref, $ref->{key}
- Anonymous arrays: [1, 2, 3]
- Anonymous hashes: {a => 1}
- Nested structures: $ref->[0]{key}
- Modify via reference

### Test 7: Objects (test-07-objects.pl)
- bless with hash ref
- Method calls: $obj->method()
- Constructor pattern: Class->new()
- ref() to check type
- Simple inheritance (if supported)

### Test 8: Advanced (test-08-advanced.pl)
- Nested data structures
- Complex expressions
- Subroutines with refs
- Closures (if supported)
- Real-world patterns

## Known Limitations

These features are not yet working:

1. **String interpolation**: `"Hello $name"` - use concatenation instead
2. **Array/hash literals in assignment**: `my @a = (1,2,3)` - broken
3. **Regex**: `=~`, `s///`, `m//` - stubs only
4. **Anonymous subs**: `map { } @list` - parser limitation
5. **Filehandles**: open/close/read - not implemented

## Adding New Tests

1. Create Perl test file in `examples/`
2. Use explicit concatenation (avoid string interpolation)
3. Run in Perl to verify expected output
4. Translate with `./pl2cl`
5. Create `-run.lisp` wrapper with `pl-let` for variables
6. Compare outputs
7. Update this document with status
