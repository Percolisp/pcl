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

## Workaround (PCL side — IMPLEMENTED, session 262)

`%pcl-create-scanner` in `cl/pcl-runtime.lisp`. When (and only when) a pattern
contains an `x` mode-modifier (`%pcl-has-x-modifier`), PCL does its own `/x`
normalisation (`%pcl-normalize-extended`): it strips insignificant whitespace and
`#`…EOL comments itself, honouring `(?x:…)`/`(?-x:…)`/`(?x)`/`(?-x)` scope
toggles, character classes `[…]` (whitespace literal), `(?#…)` comment groups,
and backslash escapes — then hands cl-ppcre the cleaned pattern **without**
`:extended-mode`, so the broken mode-restoration is never exercised. Plain `/x`
patterns (no `x` modifier) keep using cl-ppcre's native extended mode untouched,
so this can never regress them. Regression tests: `Pl/t/regex-extended-mode-01.t`.

Scanner builds are memoized (`*pcl-scanner-cache*`, keyed on pattern+options), so
the normaliser runs once per distinct pattern and repeated matches don't
recompile — a general speedup independent of this workaround.

**Out of scope (pre-existing cl-ppcre limitation, NOT introduced here):**
whitespace *inside* `\x{ … }` / `\N{ … }` under `/x` (e.g. `/\x{ 263a }/x`) is not
stripped by cl-ppcre and fails to match even without any modifier group. Real
code virtually never writes spaces inside a hex/name escape.

## Upstream

cl-ppcre: <https://github.com/edicl/cl-ppcre>. Re-run the minimal repro against
the latest release before filing.
