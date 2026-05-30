# Wantarray / Context System

## User Preference
**Work authorized 2026-05-29 (session 215).** The user previously asked to skip
wantarray/context work across many sessions; that prohibition is now lifted and
active work on this area is expected. (History kept for context.)

## What It Is
Perl has a concept of "context" — functions can detect whether they're called in scalar, list, or void context via `wantarray()`:
- `wantarray()` returns true in list context
- `wantarray()` returns false (but defined) in scalar context
- `wantarray()` returns undef in void context

## Current State
- `*wantarray*` special variable exists in pcl-runtime.lisp
- Some call sites set `(let ((*wantarray* t)) ...)` for list context
- Many call sites don't set context correctly
- This affects: `@arr = func()` vs `$x = func()`, return values, etc.

## Known Issues (now in scope for fixing — authorized 2026-05-29)
- Functions returning arrays in scalar context should return count
- Assignment context detection incomplete
- Many Perl test failures are caused by incorrect wantarray propagation
