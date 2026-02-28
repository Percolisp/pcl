# Statement Parser Plan

## Current State (December 2024)

### Expression Parser (Complete)
`Pl::PExpr.pm` handles ~95% of Perl expressions:
- All operators with precedence/associativity
- Function calls, method calls, code ref calls
- Array/hash access, slices, dereferencing
- String interpolation, regex matching
- Ternary operator (recently fixed for proper precedence)
- Assignment operators
- Variable declarations extracted as metadata

### Code Generator (Complete)
`Pl::ExprToCL.pm` generates Common Lisp from expression AST:
- All operators mapped to `pl-*` functions
- Supports indentation levels
- 94 tests passing

---

## Statement Parser (Future Work)

### What PPI Provides
PPI already identifies statement types:
- `PPI::Statement::Compound` - if/elsif/else, for/foreach, while/until
- `PPI::Statement::Sub` - subroutine declarations
- `PPI::Statement::Variable` - my/our/state/local
- `PPI::Statement::Include` - use/require
- `PPI::Statement::Package` - package declarations

### Proposed Architecture

```
Pl::Parser (NEW)
    │
    ├── Iterates PPI::Statement objects
    ├── Dispatches to statement handlers
    └── Manages scope/environment
            │
    ┌───────┴───────┐
    │               │
 PExpr          Statement Handlers
 (exists)       (NEW: handle_if, handle_loop, etc.)
```

### Effort Estimate: ~70 hours (~2 weeks)

| Phase | Hours | Description |
|-------|-------|-------------|
| Framework | 12h | PStatement.pm, scope tracking |
| Control Flow | 17h | if/else, while, for/foreach |
| Subroutines | 11h | sub declarations, signatures |
| Other | 9h | use/require, package, BEGIN/END |
| Integration | 20h | Codegen, tests, edge cases |

### Quick Win Option
Postfix modifiers only (`$x = 5 if $cond`) - ~4-6 hours

### Recommended Approach
Incremental: Start with if/else, get end-to-end working, add one statement type at a time.

---

## Recent Changes (This Session)

1. **Fixed ternary precedence bug**
   - `$x = $a ? $b : $c` now correctly parses as `$x = ($a ? $b : $c)`
   - Integrated ternary into precedence system (Config.pm: `?` at prec 15)
   - Right-associativity works: `$a ? $b : $c ? $d : $e`

2. **Removed unused code**
   - Deleted old `handle_ternary` subroutine (~85 lines)

3. **Test updates**
   - expr-02.t: 279 → 360 tests
   - All 518 main tests passing
