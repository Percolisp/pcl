# PCL Code Generation Design

**Project:** Perl to Common Lisp Transpiler
**Status:** Working implementation - 1603 tests passing

---

## Overview

PCL transpiles Perl to executable Common Lisp code. The pipeline:

```
Perl Source → PPI → PExpr (AST) → ExprToCL → Common Lisp
                      ↓
                 Environment (constants, packages, prototypes)
```

---

## Naming Conventions

### Variables: Keep Perl Sigils

Perl sigils are legal in CL symbols:

```perl
$customer_name = "John";
@items = (1, 2, 3);
%config = (debug => 1);
```

```lisp
(pcl:pl-setf $customer_name "John")
(pcl:pl-setf @items (vector 1 2 3))
(pcl:pl-setf %config (pl-hash "debug" 1))
```

### Operators: `pl-` Prefix

All Perl operators use `pl-` prefix to distinguish from CL:

```lisp
(pl-+ $a $b)              ; Numeric addition
(pl-. "Hello" " World")   ; String concatenation
(pl-== $x $y)             ; Numeric equality
(pl-str-eq $a $b)         ; String equality (eq)
```

---

## Value Boxing

All Perl scalars are boxed for reference support:

```lisp
(defstruct pl-box
  value     ; The actual value
  nv nv-ok  ; Cached numeric value
  sv sv-ok  ; Cached string value
  class)    ; Blessed class name
```

Boxing is transparent - macros auto-unbox for operations:

```perl
my $x = 10;
$$ref = 20;   # Modifies original
```

```lisp
(pcl:pl-setf $x 10)              ; Creates box
(pcl:pl-setf (pl-$ $ref) 20)     ; Modifies box value
```

---

## Context Propagation

Perl's scalar/list context is tracked during code generation:

```perl
my $count = @array;    # Scalar context → length
my @copy = @array;     # List context → elements
```

```lisp
(pcl:pl-setf $count (length @array))
(pcl:pl-setf @copy (copy-seq @array))
```

Context constants: `SCALAR_CTX (0)`, `LIST_CTX (1)`, `VOID_CTX (2)`

---

## Generated Code Patterns

### Assignment

```perl
my $x = 1;           → (pcl:pl-setf $x 1)
$x++;                → (pl-post++ $x)
$x += 5;             → (pcl:pl-+= $x 5)
```

### Control Flow

```perl
if ($x) { ... }      → (pl-if $x (progn ...))
while ($x) { ... }   → (pl-while $x ...)
for (...) { }        → (pl-for (init) (cond) (incr) ...)
foreach $x (@a) { }  → (pl-foreach ($x @a) ...)
```

### Functions

```perl
length($s)           → (pl-length $s)
print "hi"           → (pl-print "hi")
print STDERR "err"   → (pl-print :fh 'STDERR "err")
```

### Regex

```perl
$s =~ /pattern/i     → (pcl:pl-=~ $s (pl-regex "/pattern/i"))
$s =~ s/foo/bar/g    → (pcl:pl-=~ $s (pl-subst "foo" "bar" :g))
$s =~ tr/a-z/A-Z/    → (pcl:pl-=~ $s (pl-tr "a-z" "A-Z"))
```

### OO

```perl
bless {}, "Class"    → (pl-bless (pl-hash) "Class")
$obj->method()       → (pl-method-call $obj 'method)
Counter->new()       → (pl-method-call "Counter" 'new)
```

---

## Runtime Library (pcl-runtime.lisp)

The runtime provides all `pl-*` functions/macros:

### Core Operations
- Arithmetic: `pl-+`, `pl--`, `pl-*`, `pl-/`, `pl-%`, `pl-**`
- String: `pl-.`, `pl-length`, `pl-substr`, `pl-index`, `pl-lc`, `pl-uc`
- Comparison: `pl-==`, `pl-!=`, `pl-<`, `pl->`, `pl-str-eq`, `pl-str-lt`
- Logical: `pl-&&`, `pl-||`, `pl-!`, `pl-//`

### Data Structures
- Arrays: `pl-push`, `pl-pop`, `pl-shift`, `pl-unshift`, `pl-splice`
- Hashes: `pl-keys`, `pl-values`, `pl-exists`, `pl-delete`
- List ops: `pl-grep`, `pl-map`, `pl-sort`, `pl-join`, `pl-split`, `pl-reverse`

### I/O
- Output: `pl-print`, `pl-say`, `pl-printf`, `pl-warn`, `pl-die`
- Files: `pl-open`, `pl-close`, `pl-readline`, `pl-eof`, `pl-unlink`
- Dirs: `pl-opendir`, `pl-readdir`, `pl-closedir`

### Regex (CL-PPCRE)
- Match: `pl-regex`, `pl-=~`, `pl-!~`
- Subst: `pl-subst` with `:g`, `:i`, `:s`, `:m`, `:x` modifiers
- Tr: `pl-tr` with `:c`, `:d`, `:s` modifiers

---

## Package System

Perl packages map to CL packages:

```perl
package Foo::Bar;
sub greet { ... }
```

```lisp
(defpackage :Foo::Bar (:use :cl :pcl))
(in-package :Foo::Bar)
(defun pl-greet (...) ...)
```

---

## Key Implementation Files

| File | Purpose |
|------|---------|
| `Pl/Parser.pm` | Statement-level parser |
| `Pl/PExpr.pm` | Expression parser (operator precedence) |
| `Pl/ExprToCL.pm` | AST → Common Lisp code generator |
| `Pl/Environment.pm` | Constants, prototypes, package tracking |
| `Pl/OpcodeTree.pm` | AST node storage |
| `cl/pcl-runtime.lisp` | CL runtime library |

---

## Test Coverage

- **28 test files**, **1603 tests**
- Covers: operators, control flow, functions, OO, file I/O, regex
- All tests compare Perl output with transpiled CL output
