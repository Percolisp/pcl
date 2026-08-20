# PCL Final Stretch - Path to CPAN Compatibility

**Goal:** Run unmodified pure-Perl CPAN modules (no XS, minimal magic)

**Current Status:** 43 test files, 2085 tests, all passing

---

## What's Done

The transpiler handles the vast majority of Perl syntax:

- **Expressions**: All operators, correct precedence (92 levels), ternary, range
- **Control flow**: if/elsif/else/unless, while/until, for/foreach, loop labels, last/next/redo
- **Data structures**: Scalars, arrays, hashes, references, slices, anonymous refs
- **Functions**: Signatures with defaults, prototypes, &block params, 50+ built-ins
- **File I/O**: open/close/print/say, readline, eof/tell/seek, directory ops, heredocs
- **Regex**: Match, substitution, transliteration, captures, all modifiers
- **Special vars**: `$_`, `@_`, `@ARGV`, `%ENV`, `$$`, `$!`, `$0`, `$^O`, `$^V`, `__FILE__`, `__LINE__`
- **OO**: bless, ref, @ISA, SUPER::, method calls, C3 MRO
- **Modules**: use/require, use lib, @INC, module caching (memoized, skips core modules)
- **Scoping**: our, local, state, package variables
- **BEGIN/END**: Compile-time execution, subs/variables visible to BEGIN blocks
- **Forward declarations**: Subs auto-declared so top-level code can call them before definition
- **Constants**: use constant
- **String interpolation**: Variables, array elements (`$arr[0]`), hash elements (`$hash{key}`)

---

## Critical Gaps for CPAN

### Tier 1: Exception Handling ✓ DONE

| Feature | Status | Notes |
|---------|--------|-------|
| `eval { block }` | ✓ Done | Wraps in `handler-case`, sets `$@` |
| `$@` capture | ✓ Done | Set on error, cleared on success |
| `die` with object | ✓ Done | Exception objects preserved in `$@` |
| Proper stack traces | Medium | `caller()` works, needs integration |

### Tier 2: Runtime Introspection ✓ PARTIAL

| Feature | Status | Notes |
|---------|--------|-------|
| `AUTOLOAD` | Not done | Catch undefined function calls |
| `DESTROY` | Not done | Garbage collection hook |
| `can($method)` | ✓ Done | Returns code ref or undef |
| `isa($class)` | ✓ Done | Checks inheritance via MRO |
| `UNIVERSAL::*` | ✓ Done | can/isa available on all objects |

**AUTOLOAD sketch:**
```lisp
;; When pl-foo not found, call pl-AUTOLOAD with $AUTOLOAD set
(defun handle-autoload (pkg name args)
  (let (($AUTOLOAD (format nil "~A::~A" pkg name)))
    (apply (find-autoload pkg) args)))
```

### Tier 3: Filehandle Improvements (Common)

| Feature | Difficulty | Notes |
|---------|------------|-------|
| `$.` line number | Easy | Track per-filehandle |
| `$/` input separator | Easy | Use in readline |
| Three-arg open | Medium | `open($fh, '<', $file)` |
| In-memory handles | Medium | `open($fh, '<', \$string)` |

### Tier 4: Missing Built-ins (As Needed)

| Function | Difficulty | Notes |
|----------|------------|-------|
| `pack`/`unpack` | Hard | Binary data manipulation |
| `vec` | Medium | Bit vectors |
| `tie`/`tied` | Hard | Won't implement (use alternatives) |
| `select` (4-arg) | Medium | Multiplexing I/O |
| `socket`/`connect` | Medium | Networking (use sb-bsd-sockets) |

---

## Not Needed for Pure Perl

These are **not blockers** for pure-Perl modules:

| Feature | Why Not Needed |
|---------|----------------|
| XS/C extensions | Pure-Perl modules don't use them |
| `eval "string"` | Most modules use block form |
| Symbol table hacking | Use pattern recognition for Moo/Moose |
| Formats | Legacy, rarely used |
| Smart match | Deprecated |
| Indirect object syntax | Deprecated |

---

## Recommended Implementation Order

### Phase 1: Exception Handling ✓ DONE
1. ✓ `eval { block }` with handler-case
2. ✓ `$@` capture
3. ✓ Update `die` to create proper exception objects
4. ✓ Tests for exception flows

### Phase 2: UNIVERSAL Methods ✓ DONE
1. ✓ `can()` - check method existence
2. ✓ `isa()` - check inheritance
3. `VERSION()` - version check (not yet)

### Phase 3: AUTOLOAD/DESTROY
1. `AUTOLOAD` mechanism
2. `DESTROY` via finalizers (complex in CL)

### Phase 4: Filehandle Polish
1. `$.` per-handle line tracking
2. `$/` input record separator
3. Three-arg open with modes

---

## Target Modules

Start with these well-behaved pure-Perl modules:

### Easy Targets (Tested - All Pass!)

| Module | Errors | Status |
|--------|--------|--------|
| **File::Basename** | 0 | ✓ Transpiles cleanly |
| **File::Spec** | 0 | ✓ Transpiles cleanly |
| **Carp** | 0 | ✓ Transpiles cleanly |
| **Exporter** | 0 | ✓ Transpiles cleanly |

All previously reported errors were resolved by adding `__END__` / `__DATA__` handling.

### Medium Targets
- **JSON::PP** - pure-Perl JSON
- **YAML::Tiny** - minimal YAML (transpiles with minor issues)
- **Text::CSV** - CSV parser (transpiles with minor issues)
- **HTTP::Tiny** - minimal HTTP client
- **Text::ParseWords** - word splitting

### Tested (Transpilation Works)
- **Test::More** - transpiles in <30 seconds (memoization optimization)
- **YAML::Tiny** - transpiles, 2 minor parse errors (indirect filehandle, `__END__`)
- **Text::CSV** - transpiles, 2 minor parse errors (symbolic subref, `__END__`)

### Stretch Goals
- **Moo** (via pattern recognition, see MOO_MOOSE_DESIGN.md)

---

## Testing Strategy

1. **Unit tests**: Cover each new feature
2. **Module tests**: Pick a target module, try to load it
3. **Error catalog**: Track each failure, fix systematically
4. **Regression**: Don't break existing 2085 tests

---

## Risk Assessment

| Risk | Likelihood | Mitigation |
|------|------------|------------|
| DESTROY complexity | High | Use weak refs, accept some leaks |
| eval edge cases | Medium | Start with common patterns |
| Module dependencies | Medium | Start with leaf modules |
| Hidden XS usage | Low | Check CPAN deps carefully |

---

## Success Criteria

**Phase 1 Complete When:** ✓ DONE
- ✓ `eval { die "x" }; print $@` works
- ✓ Can catch and recover from errors
- ✓ Test::More transpiles quickly (<30 seconds)

**Phase 2 In Progress:**
- ✓ YAML::Tiny transpiles (2 minor issues remaining)
- ✓ Text::CSV transpiles (2 minor issues remaining)
- Fix remaining parse issues (indirect filehandle, symbolic subrefs)

**Full Success When:**
- JSON::PP loads and parses JSON
- HTTP::Tiny makes a request
- 5+ pure-Perl CPAN modules work unmodified
