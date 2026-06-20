# cl-ppcre bug: `:extended-mode` not restored after an inline `(?-x:…)` group

**cl-ppcre (bundled with PCL), SBCL 2.6.0**

In a pattern compiled with `:extended-mode t`, an inline mode-modifier group
`(?-x:…)` turns extended mode **off for the rest of the pattern** instead of
just for the group. Everything after the group then treats whitespace and `#`
as literal, so patterns that mix `/x` comments/whitespace with a `(?-x:…)` span
fail to match.

## Minimal repro

```lisp
(cl-ppcre:scan (cl-ppcre:create-scanner "a (?-x:b) c" :extended-mode t) "abc")
;; => NIL NIL          (BUG: should match 0..3)

(cl-ppcre:scan (cl-ppcre:create-scanner "a (?:b) c"  :extended-mode t) "abc")
;; => 0 3              (a plain (?:…) group is fine — it doesn't change mode)
```

After `(?-x:b)`, the ` c` is matched as a literal space + `c`, so "abc" (no
space) does not match. With `(?:b)` the trailing whitespace is still stripped,
so it matches.

## Why it matters for PCL

Perl's `/x` extended mode with scoped `(?x:…)` / `(?-x:…)` modifiers is real and
used in widely-deployed modules. **Text::ParseWords** (a common dependency) is a
prime example — its `parse_line` regex is `/…/x` with inline comments and a
`(?-x:$delimiter)` span, so every `quotewords`/`shellwords`/`parse_line` call
mis-parses.

## Workaround (PCL side, not yet implemented)

Do PCL's own `/x` normalisation before handing the pattern to cl-ppcre: strip
insignificant whitespace and `#`…EOL comments **ourselves**, honouring
`(?x:…)`/`(?-x:…)`/`(?x)`/`(?-x)` scope toggles, character classes `[…]` (where
whitespace is literal), and backslash escapes — then pass the cleaned pattern
**without** `:extended-mode`. With no insignificant whitespace left, cl-ppcre's
broken mode-restoration is never exercised.

## Upstream

cl-ppcre: <https://github.com/edicl/cl-ppcre>. Re-run the minimal repro against
the latest release before filing.
