# Wantarray / Context System

## User Preference
**Do NOT implement wantarray/context fixes.** The user has asked multiple times across sessions to skip this work. Do not include it in plans or start working on it without explicit user request.

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

## Known Issues (not to be fixed without explicit request)
- Functions returning arrays in scalar context should return count
- Assignment context detection incomplete
- Many Perl test failures are caused by incorrect wantarray propagation
