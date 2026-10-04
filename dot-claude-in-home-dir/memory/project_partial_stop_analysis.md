---
name: Partial-stop test file analysis
description: Root-cause analysis of 13 "partial (early stop)" perl-tests/ files from session 171 sweep
type: project
originSessionId: 7c279685-d61f-49b9-8932-f73db6b2694a
---
Analysis from session 171 (2026-05-06). Most "early stops" are NOT process crashes —
they're plan mismatches or PARSE ERROR drops. Full session log details in session 171.

**Why:** User asked to investigate files dying near last test. Analysis saved to avoid
re-investigating same root causes in future sessions.

**How to apply:** Before investigating a partial-stop file, check this table first.

| File | Ran/Plan | Root Cause | Fixable? |
|------|----------|------------|----------|
| `time.t` | 72/72 | FIXED (session 170/171 context) | Done |
| `lex.t` | 52/53 | Test 2 heredoc fixed; test 35 = `${BLOCK}` PARSE ERROR | Feature gap |
| `kvhslice.t` | 37/39 | Test 16 `%{$h}{'keys'}` PARSE ERROR; plan=39 but source has 38 | Feature gap + source mismatch |
| `length.t` | 47/49 | Tests 48-49: `pass()` inside `$SIG{__WARN__}` handler for `length(undef)` warning. PCL doesn't emit warnings from length() on undef | Needs warning system |
| `substr.t` | 398/400 | Plan mismatch: plan=400, source=398 (plan count is wrong) | Source bug |
| `sub.t` | 64/65 | Plan mismatch: plan=65, source=64 (plan count is wrong) | Source bug |
| `state.t` | 162/166 | Tests 163-166: given/when block (removed in Perl ≥5.38, not-supported) | Comment out (needs approval) |
| `method.t` | 160/163 | 3 missing: indirect method call syntax + null-byte in method name | Feature gap |
| `caller.t` | 65/112 | 47 missing: `${^WARNING_BITS}`, `DB::args`, `%^H`, `$^P`, tied arrays, complex caller() features | Deep feature gaps |
| `bop.t` | 496/510 | Plan mismatch: plan=510, source=496 (14 extra in plan) | Source bug |
| `each.t` | 63/65 | NOT actually an early stop — 2 tests are legitimate skips | No issue |
| `ref.t` | 179/245 | Gap=66: direct `print "ok N\n"` tests at lines 63-79 (documented in memory) | Needs raw-TAP support |
| `pack.t` | 13922/14722 | Large gap — investigate separately | Unknown |

**Key insight:** The "grep({BLOCK} LIST) paren form" splice bug (session 171 context) was the
real crash bug — it caused PARSE ERROR for expressions like `ok(grep({...} LIST), "test")`.
That's fixed. The remaining "partial" files are mostly plan mismatches in old test files.

**Heredoc interpolation fix (session 171):** `PExpr.pm` now routes `<<"..."` and `<<BARE`
through `str_interpol->parse_interpolated_string()` when content has `$`/`@`. Location:
the `ref($e1) eq 'PPI::Token::HereDoc'` handler, ~line 675. Fixes lex.t test 2 (`print <<""; $yow`).
