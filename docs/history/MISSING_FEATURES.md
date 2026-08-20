# PCL Missing Features

This document tracks Perl features not yet implemented in PCL (Perl to Common Lisp transpiler).

## Status Summary

- **Test Coverage**: 1376 tests passing (22/23 test files)
- **Core Features**: Expressions, statements, OO, control flow working
- **Known Gap**: Multi-statement blocks need Parser.pm callback

---

## V1 TODO (Must Implement)

### Labels and Loop Control
| Feature | Status | Notes |
|---------|--------|-------|
| `LABEL:` loop labels | Not implemented | Parser needs to recognize labels |
| `last LABEL` | Not implemented | Need label tracking in loops |
| `next LABEL` | Not implemented | Need label tracking in loops |
| `redo LABEL` | Not implemented | Need label tracking in loops |

### Variable Scoping
| Feature | Status | Notes |
|---------|--------|-------|
| `local $var` | Parsed, semantics missing | Dynamic scoping in CL runtime |
| `state $var` | Parsed, semantics missing | Persistent lexical in CL runtime |
| `our $var` | Parsed, semantics missing | Package variable linking |

### Block Constructs
| Feature | Status | Notes |
|---------|--------|-------|
| `BEGIN { }` | Not implemented | Compile-time execution |
| `END { }` | Not implemented | Program end hooks |
| `do { }` | Not implemented | Block as expression |
| `do FILENAME` | Not implemented | Execute file |

### Module System
| Feature | Status | Notes |
|---------|--------|-------|
| `use Module` | Partial (`use constant` works) | Need general require |
| `require` | Not implemented | Load module at runtime |
| `no Module` | Not implemented | Unimport |

---

## Partially Implemented

### Anonymous Subs and Blocks
| Feature | Status | Notes |
|---------|--------|-------|
| `sub { expr }` | Works for single expr | Multi-statement needs Parser callback |
| `grep { block }` | Works for single expr | Multi-statement needs Parser callback |
| `map { block }` | Works for single expr | Multi-statement needs Parser callback |
| `sort { $a <=> $b }` | Works | Comparison blocks work |

### Exception Handling
| Feature | Status | Notes |
|---------|--------|-------|
| `eval { }` | Deferred | P5: After self-hosting (transpiler in Lisp) |
| `eval "string"` | Deferred | P5: After self-hosting (transpiler in Lisp) |
| `die` | Works | Generates `pl-die` |
| `warn` | Works | Generates `pl-warn` |
| `$@` | Deferred | P5: After eval is implemented |

---

## Deferred to V2

### Rarely Used Features
| Feature | Reason |
|---------|--------|
| `tie`, `untie`, `tied` | Complex, rarely needed |
| Formats (`format`, `write`) | Legacy feature |
| `goto LABEL` | Dangerous, rarely used |
| AUTOLOAD | Complex dispatch |
| Indirect object (`new Class`) | Deprecated style |
| Lvalue subs (`:lvalue`) | Rare use case |

### Experimental/Deprecated
| Feature | Reason |
|---------|--------|
| Smart match `~~` | Deprecated in Perl 5.38+ |
| `given`/`when` | Experimental, rarely used |

---

## Implemented Features

### Expressions
- All arithmetic operators: `+`, `-`, `*`, `/`, `%`, `**`
- String operators: `.` (concat), `x` (repeat)
- Bitwise: `&`, `|`, `^`, `~`, `<<`, `>>`
- Logical: `&&`, `||`, `!`, `and`, `or`, `not`, `xor`
- Comparison: `<`, `>`, `<=`, `>=`, `==`, `!=`, `<=>`, `cmp`
- String comparison: `eq`, `ne`, `lt`, `gt`, `le`, `ge`
- Ternary: `? :`
- Defined-or: `//`
- Range: `..`, `...`
- Increment/decrement: `++`, `--` (prefix and postfix)
- All compound assignments: `+=`, `-=`, `.=`, etc.

### References and Dereferencing
- Reference: `\$x`, `\@arr`, `\%hash`, `\&sub`
- Dereference: `$$ref`, `@$ref`, `%$ref`, `&$ref`
- Arrow dereference: `$ref->[0]`, `$ref->{key}`
- Block dereference: `${$ref}`, `@{$ref}`

### Data Structures
- Array access: `$arr[0]`, `$arr[$i]`
- Hash access: `$hash{key}`, `$hash{$var}`
- Array slices: `@arr[0,2,4]`
- Hash slices: `@hash{@keys}`
- Anonymous arrays: `[1, 2, 3]`
- Anonymous hashes: `{a => 1, b => 2}`
- `$#arr` (array last index)

### Control Flow
- `if`/`elsif`/`else`/`unless`
- `while`/`until`
- `for` (C-style)
- `foreach` (list iteration)
- `last`, `next`, `redo` (without labels)
- `return`
- Statement modifiers: `expr if cond`, `expr unless cond`, etc.

### Subroutines
- Named subs with signatures
- Default parameter values
- Prototypes (partial)
- `wantarray` context detection

### OO Support
- `bless`
- `ref`
- Method calls: `$obj->method()`, `$obj->method`
- Class methods: `Class->method()`
- Package declarations with block scoping

### String Interpolation
- Variables in double-quoted strings
- Array interpolation
- Escape sequences

### Regex
- Match binding: `=~`, `!~`
- Substitution: `s///` with modifiers
- Transliteration: `tr///`, `y///`
- Standalone match wraps with `$_ =~`

### Built-in Functions
- Array: `push`, `pop`, `shift`, `unshift`, `splice`, `reverse`
- String: `length`, `substr`, `index`, `rindex`, `lc`, `uc`, `chomp`, `chop`
- List: `grep`, `map`, `sort`, `keys`, `values`, `each`
- Hash: `delete`, `exists`
- Type: `defined`, `ref`, `scalar`, `bless`
- I/O: `print`, `say`, `open`, `close` (with filehandle support)
- Misc: `caller`, `wantarray`, `die`, `warn`

---

## Architecture Notes

### Parser Pipeline
```
Perl Source -> PPI -> PExpr (AST) -> ExprToCL -> Common Lisp
                         |
                    Environment (constants, packages)
```

### Key Files
- `Pl/Parser.pm` - Statement-level parsing
- `Pl/PExpr.pm` - Expression parsing
- `Pl/ExprToCL.pm` - Code generation
- `Pl/PExpr/Config.pm` - Operator precedence, function specs

### Known Limitations
1. Multi-statement blocks in grep/map need Parser.pm callback
2. Anonymous subs with multiple statements need same callback
3. C-style for loop bodies with complex statements
4. `eval` deferred to P5 (self-hosting) - will use transpiler at runtime
