# CPAN Module Testing Issues

## Status: All Core Issues RESOLVED

### Fixed Issues:

1. **Dynamic method calls** (`$obj->$method_name`) - FIXED
   - Added Case 1D in PExpr.pm for `X->$foo` pattern
   - Updated gen_methodcall to detect dynamic method names

2. **Function references** (`\&func`) - FIXED
   - Modified gen_prefix_op to detect `\&func_name` pattern
   - Generates `#'pl-func_name` instead of `(pl-backslash &func_name)`

3. **Typeglob function aliases** (`*freeze = \&Dump`) - FIXED
   - Added special case in gen_binary_op
   - Generates `(setf (symbol-function 'pl-freeze) #'pl-Dump)`

---

## YAML::Tiny

YAML::Tiny now transpiles with these aliases working:
```lisp
(setf (symbol-function 'pl-freeze) #'pl-Dump)
(setf (symbol-function 'pl-thaw) #'pl-Load)
```

### Remaining warnings:
- `Use of uninitialized value $idx` - Minor issue with some regex patterns
- Some `PARSE ERROR` messages for complex regex constructs

---

## Test::Simple

### Issue 1: Module dependency chain
Test::Simple requires Test::Builder::Module which triggers parser errors.

**Problem:** The `use Test::Builder::Module;` causes issues in module loading.

---

## Text::CSV_PP

(Testing in progress)

---

## Common Issues (Mostly Resolved)

1. ~~**Typeglob manipulation** (`*name = \&func`)~~ - FIXED
2. **Deep module dependencies** - Each `use Module` needs that module to be transpiled first
3. ~~**`\&func` references**~~ - FIXED
4. ~~**Dynamic method calls** (`$obj->$method_name`)~~ - FIXED

## Next Steps

- Investigate remaining parse errors in YAML::Tiny complex regexes
- Create stubs for common modules (Carp, warnings, strict)
- Test transpiled YAML::Tiny in SBCL
