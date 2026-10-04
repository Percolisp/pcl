---
name: reference_encode_check_modifies_source
description: Encode::decode/encode with a TRUE CHECK argument EMPTIES its source string unless LEAVE_SRC is or-ed in
metadata: 
  node_type: memory
  type: reference
  originSessionId: 32e33a59-b1b3-48bb-a224-f1ed5dfc2aa1
  modified: 2026-08-16T10:30:48.068Z
---

`Encode::decode($enc, $str, $CHECK)` and `encode` **modify `$str` in place —
removing the converted part — whenever `$CHECK` is true**, unless you or in
`Encode::LEAVE_SRC`.  So a validity check like

```perl
my $bytes = encode("ISO-8859-1", $chars, Encode::FB_CROAK);
eval { decode("UTF-8", $bytes, Encode::FB_CROAK) };   # <-- EMPTIES $bytes
$line = $bytes;                                       # writes ""
```

silently writes an empty string.  Measured s406: this deleted five lines of
`MEMORY.md` while repairing double-encoded text (they vanished rather than
being corrupted, which is why a line COUNT before/after is the check).  The
fix is `my $CHK = Encode::FB_CROAK | Encode::LEAVE_SRC;` everywhere.

**How to apply:** whenever a script rewrites a file line by line, assert the
invariants before writing — line count unchanged, each rewritten line still
ends in `\n`, length above a sane floor — and keep the pre-edit copy until the
new file has been read back.  Same family as
[[feedback_perl_i_slurp_truncates]]: the destructive form looks like the safe
one.
