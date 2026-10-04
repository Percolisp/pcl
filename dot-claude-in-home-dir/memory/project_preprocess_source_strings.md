---
name: project_preprocess_source_strings
description: _preprocess_source regexes run over raw source incl. string literals — must skip quoted strings
metadata: 
  node_type: memory
  type: project
  originSessionId: e2adf2c5-33a8-4836-8723-64c700e3592f
---

`Pl::Parser::_preprocess_source` rewrites the **raw source text** with plain regexes
*before* PPI tokenizes it (hex/binary/octal float literals → decimal, `for my Class $x`
type-strip, etc.). These substitutions have NO knowledge of Perl lexical structure, so a
naive pattern also fires inside **string literals**, comments, heredocs, qw//, regexes.

**Bug fixed session 216:** the hex-float rule `0x...p...` rewrote the *string* `'0x1p+0'`
to `'1'`, silently corrupting sprintf2.t's entire `@hexfloat` data table (+72 tests once
fixed). Fix: each substitution now matches a quoted string as its FIRST alternative and
passes it through unchanged — `s{($str_re)|0x...p...}{ defined $1 ? $1 : convert }gex`
where `$str_re = qr{'(?:\\.|[^'\\])*'|"(?:\\.|[^"\\])*"}` ("match-what-you-skip OR
match-what-you-change"). Comments need NOT be skipped (PPI discards them, so converting a
lookalike there is harmless); `$#array` must NOT be treated as a comment if you ever add
comment-skipping.

**Why:** preprocessing trades lexical correctness for getting past PPI's parser limits.
**How to apply:** any NEW `_preprocess_source` rule that could match inside a string must
guard with the same `($str_re)|...` skip-first alternation. When a string value mysteriously
turns into a number/other text, suspect this function. Related: [[project_wantarray_followup]].
