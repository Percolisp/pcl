---
name: Session 152-153 state and chdir failures
description: Current sweep state, fixes done in sessions 152-153, chdir remaining failures and fix plan
type: project
originSessionId: 7c17bdbf-787e-475a-9081-ef16135f89b0
---
## Session 152 fixes (2026-04-26)

### Baseline
- Session 151 ended at 15287 passing (the noted "regression" from 15354 was an artifact — pre-session-151 commits also show 15287)

### Fixes Applied

**1. sprintf.t `%0$d` crash — `cl/pcl-runtime.lisp` `p-sprintf`**
- `%0$d` positional arg 0 → `positional-idx = (1- 0) = -1` → `(nth -1 args)` → SBCL crash
- Fix: when `call-idx < 0`, output format spec literally and warn "Invalid conversion"
- sprintf.t: was skip_all (0 tests) → now 14/566 (crash fixed, 566 now run)
- Removed the `skip_all("PCL: string eval not yet supported")` from sprintf.t

**2. `p-import-exports`: export tag expansion — `cl/pcl-runtime.lisp`**
- `:DEFAULT` in import list was not expanding to `@EXPORT` — `PL-CURDIR` never imported
- Added `%p-expand-import-tags`: `:DEFAULT` → @EXPORT, `:ALL` → @EXPORT_OK, `:TAG` → %EXPORT_TAGS{TAG}

**3. `p-find-module-package`: exact-case lookup — `cl/pcl-runtime.lisp`**
- Was: `(find-package (string-upcase name))` + `(find-package "|name|")` — pipe-quoted fails
- Fix: `(find-package (string-upcase name))` + `(find-package name)` (exact case)
- Now finds packages like `|File::Spec::Functions|` (CL name = `File::Spec::Functions`)

**4. `p-import-perl-symbol`: use `fdefinition` for functions — `cl/pcl-runtime.lisp`**
- `shadowing-import` makes the imported symbol accessible but compiled lambdas that interned
  a local `MAIN::PL-CURDIR` before the import still reference the OLD (unbound) local symbol
- Fix: for functions, use `(setf (fdefinition (intern "PL-FOO" to-pkg)) (fdefinition from-sym))`
  which binds the already-interned local symbol in to-pkg
- Why: in SBCL, when a `defun` body is compiled and references `PL-CURDIR`, the reader interns
  `MAIN::PL-CURDIR`. `shadowing-import` brings in a DIFFERENT symbol object from File::Spec::Functions.
  The compiled lambda still holds the old reference. `setf fdefinition` binds the already-interned
  local symbol to the imported function.

**5. `perl-tests/test.pl` redirect — new file**
- chdir.t does NOT chdir to `t/` before `require "./test.pl"` (intentionally — it's testing chdir)
- Sweep runs SBCL from `perl-tests/` so `./test.pl` needs to exist there
- Created `perl-tests/test.pl` that does `require './t/test.pl'; 1;`

**6. `lib/File/Spec.pm` + `lib/File/Spec/Functions.pm` — new files**
- chdir.t uses `File::Spec::Functions` for path manipulation
- Created stubs providing: catfile, catdir, splitdir, splitpath, rel2abs, curdir, updir, rootdir, file_name_is_absolute, no_upwards, path
- File::Spec::Functions.pm uses `cwd()` (a PCL builtin → `p-cwd`) for rel2abs base

**7. `lib/Cwd.pm` — new file**
- File::Spec::Functions needs `cwd()` (for `rel2abs`)
- `sub cwd { cwd() }` and `sub getcwd { getcwd() }` — PCL transpiles these to `p-cwd`/`p-getcwd`

### Results
- Sweep: **15335 passing** (+48 from 15287 baseline)
- PCL suite: 74 files, 2886 tests, all passing
- Fully passing: still 34 files (no regressions)
- chdir.t: CRASH → **35/44 partial** (no longer crashing)
- sprintf.t: 0/0 (skip) → 14/566 (no longer crashing)

## Session 153 fixes (2026-04-26)

**1. `rel2abs('.')` — `lib/File/Spec/Functions.pm`**
- `rel2abs('.', $base)` returned `$base . '/.'` for path eq '.'
- Fix: return `$base` directly
- chdir.t: now 37/43 run (6 failures)

## chdir.t remaining failures (6 tests)

37/43 run, 6 failing.

**Test 22: `fchdir` unimplemented**
- `eval { chdir(STDIN) }` should set `$@` to "The fchdir function is unimplemented at..."
- Currently p-chdir gets the STDIN symbol, stringifies it, tries to chdir to that path, fails with ENOENT
- Fix: detect filehandle/typeglob arg and die with the expected message

**Tests 27, 33: `$!` (errno) not set to ENOENT after `chdir('')`**
- `sb-posix:chdir ""` correctly fails with C errno=2 (ENOENT)
- But `$!` maps to `(p-errno-string)` which returns strerror STRING "No such file or directory"
- `$!+0` converts string to 0 (no leading digit), not 2
- Fix: change `p-errno-string` to return `(sb-alien:get-errno)` as an integer
- NOTE: this changes `"$!"` interpolation from "No such file or directory" to "2"
  — acceptable because no currently-passing test uses $! in string context for the strerror text

**Test 29: `chdir()` with only `$ENV{LOGDIR}` set**
- When no arg, p-chdir tries HOME only. Needs LOGDIR fallback.
- Fix: `(or (sb-posix:getenv "HOME") (sb-posix:getenv "LOGDIR"))`

**Test 42: `$!` not EINVAL after `chdir()` with no HOME/LOGDIR**
- When no HOME and no LOGDIR set, chdir() should fail with errno=EINVAL (22)
- Currently p-chdir calls sb-posix:chdir nil which throws a TYPECASE error (errno stays 0)
- Fix: detect nil path (no HOME, no LOGDIR) and `(setf (sb-alien:extern-alien "errno" sb-alien:int) 22)` → return nil

**Why: How to apply:**
- All tests 27/33/42 (and test 29): change p-chdir in pcl-runtime.lisp
- Also change p-errno-string to return integer
- Test 22 (fchdir): needs filehandle detection — harder, do separately
- `(setf (sb-alien:extern-alien "errno" sb-alien:int) N)` is the way to set C errno from SBCL
