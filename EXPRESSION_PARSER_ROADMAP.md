# Expression Parser Roadmap

**Project:** Pl::PExpr - Perl Expression Parser
**Target:** ~95% expression parsing (excluding code blocks)
**Current Status:** ✅ V1 COMPLETE

**Recent Fixes (Dec 2025):**
- s/// substitution and tr/// transliteration
- List declarations: `my ($x, $y) = ...`
- Context passing: `:ctx :list`
- Derefs: `$$ref`, `@$ref`, `%$ref`, `&$ref`, `*$ref` + braced forms `@{$expr}`
- Package names: `Foo::bar()`, `$Foo::x` (verified - already worked)
- Unary minus/plus: `-$x`, `+$x`
- Bitwise operators: `&`, `|`, `^`
- Logical xor: `xor`

---

## Phase 1: Verification (Quick Wins)

### 1.1 Derefs ✅ IMPLEMENTED
```perl
# Simple derefs
$$ref       # Scalar deref
@$ref       # Array deref
%$ref       # Hash deref
&$ref       # Code deref
*$ref       # Glob deref

# Braced/complex derefs
@{$ref}             # Braced array deref
${$arr[0]}          # Deref of array element
${$foo{bar}[4]}     # Nested hash/array access
@{$arr_ref->[0]}    # Deref of arrow access
```
**Status:** Fully implemented including complex braced forms.
**Tests:** `Pl/t/derefs-01.t` (66 tests)
**Changes:**
- `TokenUtils.pm`: `is_token_operator()` now recognizes Cast tokens
- `PExpr.pm`: `op_info()` returns unary prefix config for Cast tokens
- `PExpr.pm`: Added `PPI::Structure::Block` and `PPI::Statement` handling

### 1.2 Package-Qualified Names ✅ VERIFIED (Already Works)
```perl
Foo::bar()        # Package function
$Foo::x           # Package variable
Foo::Bar::baz()   # Nested packages
```
**Status:** Already works. PPI tokenizes these as single Word/Symbol tokens.
**Tests:** `Pl/t/package-names-01.t` (33 tests)
**No code changes required.**

---

## Phase 2: Substitution (s///) ✅ IMPLEMENTED

### 2.1 Basic s///
```perl
$str =~ s/old/new/;
$str =~ s/foo/bar/g;
```

### 2.2 With Modifiers
```perl
s/pattern/replacement/gimsx;
```

### 2.3 Non-destructive (Perl 5.14+)
```perl
my $new = $str =~ s/x/y/r;
```

**PPI Class:** `PPI::Token::Regexp::Substitute`
**Status:** Implemented. Added to `is_atomic()` in TokenUtils.pm, generator in ExprToCL.pm.
**Tests:** `Pl/t/regexp-subst-01.t` (22 tests total for s/// and tr///)

---

## Phase 3: Transliteration (tr/// / y///) ✅ IMPLEMENTED

### 3.1 Basic tr///
```perl
$str =~ tr/a-z/A-Z/;
tr/aeiou//d;        # Delete vowels
$count = tr/x//;    # Count x's
```

### 3.2 Modifiers
- `c` - complement
- `d` - delete
- `s` - squash
- `r` - return (non-destructive)

**PPI Class:** `PPI::Token::Regexp::Transliterate`
**Status:** Implemented. Added to `is_atomic()` in TokenUtils.pm, generator in ExprToCL.pm.
**Tests:** `Pl/t/regexp-subst-01.t`

---

## Phase 4: List Declarations ✅ IMPLEMENTED

### 4.1 Simple List
```perl
my ($x, $y) = (1, 2);
my ($a, $b, @rest) = @array;
```

### 4.2 With Default Values (Perl 5.36+)
```perl
my ($x = 1, $y = 2) = @args;  # Future consideration
```

**Status:** Implemented. `extract_declarations` now handles `PPI::Structure::List`.
**Tests:** Added to `Pl/t/declarators-01.t` (19 new tests, 53 total)

---

## Out of Scope for V1

| Feature | Reason |
|---------|--------|
| `map { } @list` | Requires statement parser for block contents |
| `grep { } @list` | Same as map |
| Postfix deref `$ref->@*` | Perl 5.20+, rarely used |
| Smart match `~~` | Deprecated in 5.42 |
| Attributes `:lvalue` | Edge case |

---

## Implementation Priority

| Priority | Feature | Status | Effort |
|----------|---------|--------|--------|
| 1 | Verify derefs | ✅ Done | - |
| 2 | Verify package names | ✅ Done | - |
| 3 | s/// substitution | ✅ Done | - |
| 4 | tr/// transliteration | ✅ Done | - |
| 5 | List declarations | ✅ Done | - |

**Total estimated effort:** ✅ All complete!

---

## Test Strategy

For each feature:
1. Check PPI structure with `PPI::Dumper`
2. Add handling in `parse()` or helper method
3. Write tests covering basic + edge cases
4. Verify context propagation works

---

## Success Criteria

- [x] All verification tests pass for derefs and package names
- [x] s/// works with common modifiers (g, i, m, s, x, e)
- [x] tr/// works with c, d, s modifiers
- [x] List declarations extract all variables (my, our, local, state)
- [x] No regressions in existing tests

**Final Status:** 1252 tests, 18/19 test files passing (only `anon-sub-01.t` fails - out of scope)
