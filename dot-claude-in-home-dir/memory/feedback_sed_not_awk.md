---
name: Use sed for line selection, not awk
description: User prefers sed for selecting line ranges; awk calls are rejected
type: feedback
originSessionId: 63dde3fb-14f4-484f-950e-38733a18d0c2
---
Use `sed -n 'START,ENDp'` to select a range of lines from a file. Do NOT use `awk` for line selection.

**Why:** User interrupted an awk call and said: "Use `sed` for selecting a set of lines, not `awk`."

**How to apply:** When you need to inspect lines N through M of a file, use `sed -n 'N,Mp' filename`. Never use `awk 'NR>=N && NR<=M'` for this purpose.
