# Perl Expression Parser - Development Session Summary

**Project:** Pl::PExpr - Perl to Common Lisp Expression Parser
**Last Updated:** December 28, 2024
**Status:** V2 - Constants and OO Support Added

---

## Latest Session (December 28, 2024) - V2 Features

### Constants Support - COMPLETE

Implemented `use constant` parsing and usage in expressions.

**Examples working:**
```perl
use constant PI => 3.14159;              # → (defconstant +PI+ 3.14159)
use constant { A => 1, B => 2 };         # → (defconstant +A+ 1) (defconstant +B+ 2)
my $x = PI;                              # → (pl-setf $x +PI+)
my $area = PI * $r * $r;                 # → (pl-setf $area (pl-* (pl-* +PI+ $r) $r))
```

**Changes made:**
- `Pl/Parser.pm`: Added `_process_use_constant()`, `_process_constant_hash()`, `_process_single_constant()`, `_emit_constant()`, `_compile_constant_value()`
- `Pl/PExpr.pm`: Added constant check in bareword handling - creates `constant` node type
- `Pl/ExprToCL.pm`: Added `gen_constant()` to output `+NAME+` (CL naming convention)

**Tests:** 19 tests in `constants-01.t`

### Bless/OO Support - COMPLETE

Added `bless`, `ref`, and `package` handling for object-oriented Perl.

**Examples working:**
```perl
bless {}, "MyClass";                     # → (pl-bless (pl-hash) "MyClass")
my $obj = bless $ref, $class;            # → (pl-setf $obj (pl-bless $ref $class))
ref($obj);                               # → (pl-ref $obj)
package MyClass { sub new { } }          # Block-scoped package with pop
```

**Changes made:**
- `Pl/PExpr/Config.pm`: Added `bless`, `ref`, `tied`, `tie`, `untie` to known functions
- `Pl/Environment.pm`: Added `package_stack` attribute with `current_package()`, `push_package()`, `pop_package()` methods
- `Pl/Parser.pm`: Updated `_process_package_statement()` to handle block form with package stack

**Design decision:** Hash-based objects (matches Perl semantics) rather than CLOS

**Tests:** 21 tests in `bless-01.t`

### Test Status

- **22 test files, 1326 tests**
- **21/22 passing** - only `anon-sub-01.t` fails (known: `map { block } @list` out of scope)

---

## Previous Session (December 28, 2024)

### Subroutine Signature/Prototype Parsing - COMPLETE

Implemented full parsing of subroutine signatures and prototypes, with default value compilation and environment tracking.

**Examples working:**
```perl
sub foo($x, $y) { }           # → (defun foo ($x $y) ...)
sub bar($x, $y = 10) { }      # → (defun bar ($x &optional ($y 10)) ...)
sub baz($a, $b = $a * 2) { }  # → (defun baz ($a &optional ($b (pl-* $a 2))) ...)
sub qux($x, @rest) { }        # → (defun qux ($x &rest @rest) ...)
sub old_style($$;$) { }       # Old prototype: 2 required, 1 optional
```

**Key features:**
- Parses both new-style signatures (`$x, $y = 10`) and old-style prototypes (`$$;$`)
- Compiles default expressions to CL at definition time (lexical scope preserved)
- Stores signature info in Environment for use when parsing function calls
- PExpr checks environment for declared subs before built-in function table

**Changes made:**
- `Pl/Parser.pm`: Added `parse_prototype_or_signature()`, `_parse_signature()`, `_parse_old_prototype()`, `_compile_default_expr()`. Updated `_process_sub_statement()` to use these and store in environment.
- `Pl/Environment.pm`: Updated to store signature info structure `{ params => [...], min_params => N, is_proto => 0/1 }`. Added `get_min_params()` helper.
- `Pl/PExpr.pm`: Added `environment` attribute. Updated `no_params_of_sub()` to check environment first.

**Signature info structure:**
```perl
{
  params => [
    { name => '$x', default_cl => undef },
    { name => '$y', default_cl => '10' },
  ],
  min_params => 1,   # Minimum required args
  is_proto   => 0,   # 1 for old-style ($$), 0 for new-style ($x, $y)
}
```

**Tests:** 34 new tests in `signatures-01.t`

### wantarray Parameter for Context - FIXED

All generated `defun`s now include `&key wantarray` so callers can pass context:

```lisp
;; sub foo($x) { @array }
(defun foo ($x &key wantarray)
  @array)

;; my @result = foo(1);   -- list context
(pl-setf @result (pl-foo 1 :wantarray t))

;; my $scalar = foo(2);   -- scalar context (default)
(pl-setf $scalar (pl-foo 2))
```

**Changes:**
- `Pl/Parser.pm`: Added `&key wantarray` to all generated defuns
- `Pl/ExprToCL.pm`: Changed `:ctx :list` to `:wantarray t`

---

### s/// Substitution and tr/// Transliteration - COMPLETE

Implemented support for regex substitution and transliteration operators.

**Examples working:**
```perl
s/foo/bar/           # → (pl-subst "foo" "bar")
s/hello/world/gi     # → (pl-subst "hello" "world" :g :i)
$str =~ s/old/new/   # → (pl-=~ $str (pl-subst "old" "new"))
tr/a-z/A-Z/          # → (pl-tr "a-z" "A-Z")
tr/aeiou//d          # → (pl-tr "aeiou" "" :d)
y/abc/xyz/           # → (pl-tr "abc" "xyz")  (y is synonym)
```

**Changes made:**
- `Pl/PExpr/TokenUtils.pm`: Added `PPI::Token::Regexp::Substitute` and `PPI::Token::Regexp::Transliterate` to `is_atomic()`
- `Pl/ExprToCL.pm`: Added `gen_substitution` and `gen_transliteration` handlers using PPI's `get_match_string`, `get_substitute_string`, and `get_modifiers` methods

**Tests:** 22 new tests in `regexp-subst-01.t`

**Modifiers supported:**
- s///: g, i, m, s, x, e
- tr///: c, d, s

---

### Context Passing in Code Generation

Added `:ctx :list` parameter to function calls when in list context. Scalar context is default (no parameter).

**Examples:**
```perl
my $x = foo(1);   # → (pl-foo 1)
my @x = foo(1);   # → (pl-foo 1 :ctx :list)
my %h = foo(1);   # → (pl-foo 1 :ctx :list)
foo(bar(1));      # → (pl-foo (pl-bar 1))
```

**Changes:**
- `Pl/Parser.pm`: Added `annotate_contexts()` call after parsing
- `Pl/ExprToCL.pm`: Added `get_context_keyword()` helper, updated `gen_funcall` to emit `:ctx :list` when needed

**Design decisions:**
- Scalar context is default (most common case)
- Void context treated as scalar (result just discarded)
- Only list context is explicitly passed

### List Declarations Implemented

Added support for `my ($x, $y) = (1, 2)` style declarations.

**Changes to `Pl/PExpr.pm`:**
- Enhanced `extract_declarations` to handle `PPI::Structure::List`
- Added case for pending declarator + list structure
- Extracts all Symbol tokens from list, records each as declaration

**Examples working:**
```perl
my ($x, $y) = (1, 2)     # → declarations: $x, $y
my ($a, @rest) = @array  # → declarations: $a, @rest
my (%hash) = @pairs      # → declarations: %hash
our ($x, $y)             # → declarations: $x, $y (type: our)
```

**Tests:** 19 new tests in `declarators-01.t` (53 total)

### Filehandle Support - VERIFIED WORKING

Previously noted as buggy, but now works correctly:

```bash
echo 'print STDERR "hello";' | ./pl2cl
# Output: (pl-print :fh STDERR "hello")
```

All cases working:
- `print STDERR "msg"` → `(pl-print :fh STDERR "msg")`
- `print $fh "msg"` → `(pl-print :fh $fh "msg")`
- `print "msg"` → `(pl-print "msg")` (no filehandle)
- `say STDOUT "msg"` → `(pl-say :fh STDOUT "msg")`

### Zero-Parameter Function Fix

Fixed `time()` and other zero-param functions not creating proper `funcall` nodes.

**Problem:** Lines 1115-1122 in PExpr.pm created a wrapper but never inserted it into OpcodeTree.

**Fix:** Use `make_node_insert('funcall')` like the normal function path:
```perl
if (defined $no_pars && $no_pars == 0) {
  my($top_node, $top_id) = $self->make_node_insert('funcall');
  my $node_id = $self->make_node($now);
  $self->add_child_to_node($top_id, $node_id);
  $e->[$i] = $self->make_subtree_item($top_id, 'funcall');
  next;
}
```

**Result:** All 48 tests in `default-param-01.t` now pass.

### Reference Semantics Design (CODEGEN_DESIGN.md)

Documented how Perl references compile to Common Lisp with pass-by-reference support.

**Key Design Decisions:**

1. **All scalars are boxed** (V1 simplicity - optimize later if needed)
2. **Boxing hidden in macros** - code generator outputs clean code
3. **Pass-by-reference works** - functions can modify caller's variables

**How Macros Hide Boxing:**

| Perl | Generated CL | Macro Behavior |
|------|--------------|----------------|
| `my $x = 10` | `(pl-let (($x 10)) ...)` | Creates box internally |
| `$x = 20` | `(pl-setf $x 20)` | Sets box value |
| `$x + 1` | `(pl-+ $x 1)` | Auto-unboxes arguments |
| `\$x` | `(pl-ref $x)` | Returns box as-is |
| `$$ref` | `(pl-$ $ref)` | Returns/sets box value |

**Pass-by-Reference Example:**
```perl
sub modify { my $ref = shift; $$ref = 20; }
my $x = 10;
modify(\$x);  # $x is now 20
```

```lisp
(defun modify ($ref)
  (pl-setf (pl-$ $ref) 20))
(pl-let (($x 10))
  (modify (pl-ref $x)))  ; $x now contains 20
```

**Why box everything (V1):**
- String `eval` can reference any variable: `eval '\$x'`
- Avoids complex two-pass analysis
- Optimize later if profiling shows need

---

## Previous Session (December 27, 2024 - Continued)

### Statement Parser Prototype Created

Created `Pl/Parser.pm` - a working statement-level parser that:
- Iterates over PPI statements
- Delegates expressions to PExpr
- Generates Common Lisp via ExprToCL
- Outputs Perl code as Lisp comments, then CL code

**Files created:**
- `Pl/Parser.pm` - Statement-level parser (600+ lines)
- `pl2cl` - Command-line tool
- `Pl/t/parser-01.t` - 5 tests

### Statement Types Implemented

| Statement | Perl Example | Generated CL |
|-----------|-------------|--------------|
| Expression | `$x = 1;` | `(pl-setf $x 1)` |
| Variable declaration | `my $x = 1;` | `(pl-setf $x 1)` |
| if/elsif/else/unless | `if ($x) { ... }` | `(pl-if ... (progn ...) ...)` |
| while/until | `while ($x) { ... }` | `(pl-while ...)` |
| for/foreach | `for $x (@a) { ... }` | `(pl-foreach ($x @a) ...)` |
| Bare blocks | `{ ... }` | `(progn ...)` |
| Subroutines | `sub foo ($x) { ... }` | `(defun foo ($x) ...)` |

### Features Added
- Recursive block processing (nested control structures work)
- Condition parsing through PExpr
- Multi-line Perl comments with `;;` prefix on each line
- Per-clause comments for if/elsif/else
- Subroutine signature parsing (`sub foo ($x, $y) { ... }`)

### Print/Say Filehandle Support - ✅ COMPLETE

Handles `print STDERR "hello"` and `print $fh "hello"` correctly.

**Key insight:**
`is_word()` returns `1` (true) or `undef`, NOT the word content!
Must use `$token->content` to get actual word.

### Code Changes Made

**Pl/PExpr.pm:**
- Line ~1091: Fixed `$sub_name` to use `$now->content` instead of `is_word()` result
- Line ~1095-1111: Added filehandle skip logic for print/say
- Line ~1138-1176: Added filehandle detection when processing print/say
- Added debug statements (can be removed after fix)

**Pl/ExprToCL.pm:**
- Added `filehandle` to handler dispatch table
- Added `gen_filehandle` function to generate `:fh STDERR` output

---

## To Resume Tomorrow

1. **Debug the filehandle node creation:**
   Add logging after line 1167 to verify `$filehandle_id` is set:
   ```perl
   say STDERR "DEBUG FH: Created filehandle node id=$fh_id" if 8 & DEBUG;
   ```

2. **Check if filehandle is added to funcall:**
   Verify line 1185-1187 is reached:
   ```perl
   if ($filehandle_id) {
     $self->add_child_to_node($top_id, $filehandle_id);
   }
   ```

3. **Expected final output:**
   ```lisp
   ;; print STDERR "hello"
   (pl-print :fh STDERR "hello")
   ```

4. **Test commands:**
   ```bash
   echo 'print STDERR "hello";' | ./pl2cl
   echo 'print $fh "hello";' | ./pl2cl
   echo 'print "hello";' | ./pl2cl  # No filehandle
   ```

5. **To enable debug:**
   ```perl
   perl -e '
   use lib ".";
   use Pl::PExpr;
   Pl::PExpr::SET_DEBUG(8);
   use Pl::Parser;
   my $result = Pl::Parser->parse_code(q{print STDERR "hello";});
   print $result;
   ' 2>&1 | grep DEBUG
   ```

---

## Previous Session (December 27, 2024)

### Ternary Operator Fix
**Problem:** `$x = $a ? $b : $c` was parsing as `($x = $a) ? $b : $c`

**Solution:** Integrated ternary into precedence system:
- Added `?` to Config.pm with `prec => 15`, `no => 3`
- Added `:` as marker-only (`no => 0`)
- Ternary now handled in operator loop, not special-cased
- Right-associativity works: `$a ? $b : $c ? $d : $e`

### Code Generator Complete
`Pl/ExprToCL.pm` generates Common Lisp from expression AST:
- All operators mapped to `pl-*` functions
- Supports indentation levels
- 94 tests passing in `Pl/t/codegen-01.t`

### Test Status (December 28, 2024)

**Run from project root:** `prove Pl/t/` (NOT `cd Pl && prove t/`)

- **Total:** 1201 tests across 18 files
- **Passing:** 16/18 test files

**Known failures:**
1. `anon-sub-01.t` - `map { block } @list` not implemented (out-of-scope for V1)
2. `default-param-01.t` - 3 failures with `time()` (zero-param function handling)

---

## Project Status

**Expression Parser:** ✅ Complete
**Code Generator:** Working (102 + 22 + 34 = 158 targeted tests)
**Statement Parser:** Working (if/while/for/subs with signatures)
**Filehandle support:** ✅ Complete
**s/// and tr///:** ✅ Complete
**Signature parsing:** ✅ Complete
**Environment tracking:** ✅ Complete

### What's Working
- Expression parser with full operator precedence
- Ternary operator with correct precedence
- Code generator producing Common Lisp output
- Statement parser with control flow (if/while/for/foreach)
- Subroutine parsing with full signature support
- Default parameter compilation (expressions compiled at definition time)
- Environment tracks declared subs for proper argument parsing
- s/// substitution and tr/// transliteration
- Filehandle support (print STDERR, print $fh, etc.)
- List declarations (my ($x, $y) = ...)
- Context passing (:ctx :list)
- 1286 tests across 20 files, 19/20 passing

### Known Test Failures
- `anon-sub-01.t`: `map { block } @list` - out of scope for V1

### Remaining Work (V1)
- None - all major features complete!

**Overall Status:** ✅ V1 FEATURE COMPLETE

---

**To resume:** Read this file to get up to speed on project status.
