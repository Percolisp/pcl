# Plan: Implementing Perl's Exporter Functionality in PCL

## Problem Statement

Currently, `use Module` in PCL loads and executes a module but doesn't import symbols:

```perl
use Config;
print $Config{ivsize};  # FAILS - %Config not imported
print $Config::Config{ivsize};  # Works - fully qualified
```

Perl's Exporter mechanism allows modules to declare `@EXPORT` and `@EXPORT_OK`, and `use` automatically imports those symbols into the caller's namespace.

## How Perl's Exporter Works

1. Module declares exportable symbols:
   ```perl
   package Config;
   use Exporter 'import';
   our @EXPORT = qw(%Config);        # Exported by default
   our @EXPORT_OK = qw(config_vars); # Exported on request
   ```

2. When user writes `use Config`:
   - Perl loads Config.pm
   - Perl calls `Config->import()` (inherited from Exporter)
   - `import()` copies symbols from `@EXPORT` into caller's namespace

3. The "copy" creates an alias - `main::%Config` points to `Config::%Config`

## Implementation Options

### Option A: Use CL's Package System (Recommended)

Leverage Common Lisp's built-in package functionality:

```lisp
;; When pl-use loads Config, also do:
(use-package :Config :pcl)  ; Makes Config's exported symbols visible in pcl
```

**Pros:**
- Uses CL's native mechanism
- Efficient - no runtime lookup overhead
- Symbols work naturally

**Cons:**
- Need to mark symbols as exported in CL sense
- Package must exist before use-package

**Implementation:**
1. When transpiling `our @EXPORT = qw(...)`, generate CL exports
2. When `pl-use` runs, call `use-package` after loading module

### Option B: Symbol Aliasing at Load Time

Create explicit symbol bindings in the caller's package:

```lisp
;; After loading Config, for each symbol in @EXPORT:
(setf (symbol-value (intern "%CONFIG" *package*))
      (symbol-value (find-symbol "%CONFIG" :Config)))
```

**Pros:**
- More control over what gets imported
- Can handle selective imports (`use Module qw(sym1 sym2)`)

**Cons:**
- More complex implementation
- Need to handle variables, functions, and constants differently

### Option C: Runtime Symbol Resolution

Modify variable/function lookup to check imported packages:

**Pros:**
- Most flexible

**Cons:**
- Performance overhead on every access
- Complex implementation

## Recommended Implementation: Hybrid A + B

### Phase 1: Basic @EXPORT Support

1. **Track exports during transpilation** (Pl/Parser.pm)

   When we see `our @EXPORT = qw(...)`:
   - Store the export list in Environment
   - Generate CL `export` calls for those symbols

2. **Modify pl-use to call use-package** (pcl-runtime.lisp)

   After loading a module:
   ```lisp
   (defun pl-use (module-name &key imports)
     ;; ... load module ...
     ;; Then make its exports visible:
     (let ((pkg (find-package (string-upcase module-name))))
       (when pkg
         (use-package pkg *package*))))
   ```

3. **Generate proper CL exports** (Pl/ExprToCL.pm)

   When transpiling a module with @EXPORT:
   ```lisp
   ;; At end of module:
   (export '(%Config config_vars) :Config)
   ```

### Phase 2: Selective Imports

Handle `use Module qw(sym1 sym2)`:

```lisp
(defun pl-use (module-name &key imports)
  ;; ... load module ...
  (let ((pkg (find-package ...)))
    (if imports
        ;; Import only specified symbols
        (dolist (sym imports)
          (import (find-symbol sym pkg) *package*))
        ;; Import all from @EXPORT
        (use-package pkg *package*))))
```

### Phase 3: @EXPORT_OK Support

Only export symbols in @EXPORT_OK when explicitly requested.

## Detailed Implementation Steps

### Step 1: Modify Module Transpilation

In `Pl/Parser.pm`, when processing a module:

1. Detect `our @EXPORT = ...` declarations
2. Store export list in Environment
3. At end of module, emit:
   ```lisp
   (eval-when (:compile-toplevel :load-toplevel :execute)
     (export '(symbol1 symbol2 ...) :package-name))
   ```

### Step 2: Modify pl-use in Runtime

```lisp
(defun pl-use (module-name &key imports)
  "Perl use - load module at compile time and import symbols."
  (let ((rel-path (module-to-path module-name)))
    ;; Check if already loaded
    (when (gethash rel-path *pl-inc-table*)
      (return-from pl-use t))

    ;; Load the module
    (let ((abs-path (pl-find-module-in-inc rel-path)))
      (unless abs-path
        (error "Can't locate ~A in @INC" rel-path))
      (pl-load-module-cached abs-path))

    ;; Import symbols from the module's package
    (let ((pkg (find-package (string-upcase module-name))))
      (when pkg
        (if imports
            ;; Selective import
            (dolist (sym-name imports)
              (let ((sym (find-symbol (string-upcase sym-name) pkg)))
                (when sym (import sym *package*))))
            ;; Default: use-package to get all exports
            (use-package pkg *package*))))

    ;; Mark as loaded
    (setf (gethash rel-path *pl-inc-table*) abs-path)
    t))
```

### Step 3: Handle Variable Sigils

Perl's `%Config` becomes `%CONFIG` in CL. Need to ensure:
- Export uses the sigil: `(export '(%Config) :Config)`
- Symbol lookup handles case properly

### Step 4: Test Cases

```perl
# Test 1: Basic export
use Config;
print $Config{osname};  # Should work without Config::

# Test 2: Selective import
use Config qw(config_vars);
config_vars('osname');  # Should work
print $Config{osname};  # Should fail (not imported)

# Test 3: Module with functions
use List::Util qw(sum);
print sum(1, 2, 3);  # Should work
```

## Files to Modify

| File | Changes |
|------|---------|
| `Pl/Parser.pm` | Detect @EXPORT, emit CL export calls |
| `Pl/Environment.pm` | Track module exports |
| `Pl/ExprToCL.pm` | Generate export statements |
| `cl/pcl-runtime.lisp` | Update pl-use to call use-package |
| `lib/Config.pm` | Add explicit export statement |

## Migration Path

1. First implement for Config.pm as test case
2. Verify `use Config; $Config{key}` works
3. Extend to other modules
4. Add @EXPORT_OK support

## Potential Issues

1. **Case sensitivity**: Perl is case-sensitive, CL uppercases by default
   - Solution: Use `|%Config|` or intern with exact case

2. **Symbol conflicts**: What if caller already has a `%Config`?
   - Perl: Later import shadows earlier
   - CL: use-package errors on conflict
   - Solution: Use `shadowing-import` or handle conflicts

3. **Circular dependencies**: Module A uses B, B uses A
   - Already handled by *pl-loading-modules* tracking

4. **Runtime vs compile-time**: Exports must be available when use runs
   - Use eval-when to ensure exports happen early

## Success Criteria

After implementation:
```perl
use Config;
print $Config{ivsize};  # Works!
print $Config::Config{ivsize};  # Also works
```

Tests like `range.t` should work without manually changing `$Config{...}` to `$Config::Config{...}`.
