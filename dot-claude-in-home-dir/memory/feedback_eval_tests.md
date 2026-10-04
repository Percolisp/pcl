---
name: String eval tests — do not comment out
description: String eval is implemented; do not comment out eval "..." tests. Only use bytes tests may be commented out.
type: feedback
---

Do NOT comment out tests that use `eval "string"`. String eval (`p-eval`) is fully implemented since session 104. If a test using `eval "..."` fails, fix the transpiler or runtime — do not hide the failure by commenting.

**Why:** User explicitly corrected this in session 117. The old guidance ("comment out eval string tests") was based on eval not being implemented. It is now implemented, so failing eval tests represent real bugs to fix.

**How to apply:** Only `use bytes` tests may be commented out (bytes pragma not supported). All other tests, including those using string eval, must remain active. Ask the user before commenting out any other test.
