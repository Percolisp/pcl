# Plan: `\p{...}` / `\P{...}` Unicode-property regex support

**Status:** planned, not implemented (2026-06-23).
**Why it matters:** convergent root cause behind multiple module failures —
`Text::Tabs` (`expand`/`unexpand` compute column width with `( () = /\PM/g )`)
and `Text::Wrap` (wrap regex `\PM\pM*`) both die/misbehave because cl-ppcre
currently treats `\p` / `\P` as the literal characters `p` / `P`. Also closes
the long-standing `\p{IsWord}` gap noted in `docs/not-supported.md` (Unicode
section).

## Current behaviour (the bug)

cl-ppcre's lexer only consults a property resolver when
`cl-ppcre:*property-resolver*` is non-NIL; PCL never sets it, so:

```perl
"a" =~ /\pL/      # perl: match;   PCL: no match (treated as literal "pL"… )
"a" =~ /\p{L}/    # perl: match;   PCL: no match
"x" =~ /\PM/      # perl: match;   PCL: no match
```

Observed downstream:
- `Text::Tabs::expand("a\tb")` → PCL pads every tab to a *full* `$tabstop`
  (8 spaces) instead of "to the next stop", because `( () = /\PM/g )` returns 0
  (no `\PM` support) so the running column offset is always 0.
- `Text::Wrap::wrap(...)` → falls through its `\PM\pM*` alternation to the
  final `else { die "This shouldn't happen" }`.

## cl-ppcre integration contract (verified by reading the source)

`/usr/share/common-lisp/source/cl-ppcre/`:

- `specials.lisp:138` — `*property-resolver*`: "NIL or a designator for a
  function which accepts strings and returns unary character test functions or
  NIL."
- `convert.lisp:72` — `resolve-property`: a **string** property name is resolved
  via `(funcall *property-resolver* property-name)`; a **function** is used
  as-is. So our resolver returns a `(function (character) generalized-boolean)`.
- `lexer.lisp:280` — `read-char-property` **requires a `{`** after `\p`/`\P`
  (`"Expected left brace after \p"`). It reads up to `}` and emits
  `(:property NAME)` for `\p{` or `(:inverted-property NAME)` for `\P{`.
  ⇒ **cl-ppcre handles the `\P` negation itself**; our resolver only ever
  returns the *positive* predicate.
- `lexer.lisp:619` — outside a char class, `\p`/`\P` only enter the property
  path when `*property-resolver*` is set; otherwise the char is left literal.

### Consequence: two pieces of work

1. **Brace normalization** (in `perl-regex-to-ppcre`, `cl/pcl-runtime.lisp`).
   cl-ppcre's resolver path fires *only* for the braced form `\p{X}`. Perl also
   allows the single-letter shorthand `\pL` / `\PM`. Convert shorthand →
   braced *before* handing the pattern to cl-ppcre:
   - regex-replace `\\([pP])([A-Za-z])` → `\<p|P>{<letter>}` (a non-`{`
     character after `\p` ⇒ shorthand; the braced form is untouched because `{`
     is not in `[A-Za-z]`).
   - Do this with the existing `regex-replace-all ... :simple-calls t` lambda
     style already used in `perl-regex-to-ppcre`.
   - **Edge cases to guard:** a literal `\\pL` (escaped backslash then `pL`)
     and `\p` inside `\Q…\E` (already meta-quoted earlier in the same function).
     Both are rare; document if not handled.

2. **The resolver** (`%pcl-unicode-property-test`, new defun in
   `cl/pcl-runtime.lisp`, installed once at load with
   `(setf cl-ppcre:*property-resolver* #'%pcl-unicode-property-test)`).

## Resolver design

Back it with **`sb-unicode:general-category`** (returns a keyword such as
`:LU`, `:LL`, `:MN`, `:ND`, …). Two clean rules cover the General_Category space:

- **Group** (one letter, e.g. `\p{L}`, `\p{M}`, `\p{N}`): first character of the
  category keyword's name equals the group letter.
  `(char= (char (symbol-name (sb-unicode:general-category c)) 0) #\L)`
- **Value** (two letters, e.g. `\p{Lu}`, `\p{Mn}`): full category name (string,
  case-insensitive) equals the requested value.

Name normalization before dispatch:
- trim spaces; strip a leading `Is` / `In` prefix (`\p{IsAlpha}`); fold case for
  alias comparison (keep the General_Category compare case-insensitive too).

Aliases to map (Perl/POSIX names commonly seen in CPAN):

| property | predicate |
|----------|-----------|
| `L` / `Letter` / `Alpha` / `Alphabetic` | GC group `L` |
| `Lu`,`Ll`,`Lt`,`Lm`,`Lo` and other 2-letter GCs | exact GC value |
| `M` / `Mark` | GC group `M`  ← unblocks Text::Tabs/Text::Wrap |
| `N` / `Number` / `Digit` (`Nd` for strict digit) | GC group `N` / `Nd` |
| `P` / `Punct` / `Punctuation` | GC group `P` |
| `S` / `Symbol` | GC group `S` |
| `Z` / `Space` / `White_Space` / `SpacePerl` | reuse existing `%perl-space-char-p` |
| `C` / `Control`/`Cntrl` | GC group `C` |
| `Word` / `IsWord` | `L`∪`M`∪`N` ∪ `Pc` ∪ `_` (Perl `\w`) |
| `Upper`/`Uppercase` → `Lu`; `Lower`/`Lowercase` → `Ll` | exact |
| `Alnum` | `L` ∪ `N` |
| `ASCII` | `char-code < 128` |

**Unknown property name:** decide policy explicitly (test empirically what
cl-ppcre does when the resolver returns `NIL` — likely a `nil` "test function"
that errors at scan time). Safest: return a predicate that signals a clear
`p-die`-style "Unknown Unicode property '<name>'" *at compile time* (resolver
call time), so a typo is visible rather than silently matching nothing. Revisit
if some in-scope module legitimately uses a property we don't model — then add
it to the table (the table is the single extension point).

## Risk / blast radius

- Setting `*property-resolver*` is **global** and changes how *every* regex with
  `\p`/`\P` is lexed. Today those are mis-lexed as literals, so any regex that
  *intended* a literal `p`/`P` immediately after a backslash (`/\p/` meaning
  literal `p`) would change meaning. In practice `\p`-as-literal is essentially
  never written (you'd write `p`); low risk, but call it out in the commit.
- Performance: the resolver returns a closure invoked per character during
  scanning. `sb-unicode:general-category` is a table lookup — fine. Char classes
  with `*optimize-char-classes*` may precompute; leave that default.

## Test plan

- New `Pl/t/regex-unicode-prop-01.t`:
  - transpile/runtime parity vs perl for `\pL \p{L} \PM \p{Mn} \p{Nd} \pP \w`
    over a mixed ASCII + combining-mark + digit string.
  - `( () = $s =~ /\PM/g )` count equals perl's (the Text::Tabs idiom).
- Module-level acceptance (add to `docs/cpan-module-log.md`):
  - `Text::Tabs` `expand`/`unexpand` byte-identical to perl.
  - `Text::Wrap` `wrap`/`fill` no longer dies; output matches perl for the
    basic column-wrap cases.
- Run the full `Pl/t/` gate (≥6 min) + a perl-tests sweep `tools/sweep-diff.pl`
  to confirm no regex regressions (watch lc.t / tr.t / split-unicode-ws-01.t).

## Files touched (when implemented)

- `cl/pcl-runtime.lisp` — `%pcl-unicode-property-test`, the
  `(setf cl-ppcre:*property-resolver* …)` install, and the shorthand→braced
  normalization inside `perl-regex-to-ppcre`. Export not needed (internal).
- `Pl/t/regex-unicode-prop-01.t` — new regression file.
- `docs/not-supported.md` — remove/soften the `\p{IsWord}` bullet once it works.
- `docs/cpan-module-log.md` — move Text::Tabs/Text::Wrap to ✅.
