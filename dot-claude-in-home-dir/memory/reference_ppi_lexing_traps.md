---
name: reference_ppi_lexing_traps
description: "Where PPI mis-lexes perl, and the repair pattern PCL uses for each"
metadata: 
  node_type: memory
  type: reference
  originSessionId: 50bf0bf5-b10e-4a5a-b5d9-4cf32802f839
  modified: 2026-08-15T07:50:39.628Z
---

PPI is the parser PCL feeds. Its gaps are load-bearing; each of these was a
silent-wrong or a whole-file refusal. Details in `docs/DECIDED.md` (s390, s395,
s396) and `docs/ppi-upstream-bugs.md`.

**RULE (user, s396 — CLAUDE.md 13): every PPI bug PCL works around gets a section in
`docs/ppi-upstream-bugs.md` and a runnable case in `docs/ppi-bug-report.t` IN THE
SAME COMMIT**, plus a CANARY row in `Pl/t/misc-fixes-02.t` asserting the broken
behaviour (the repair is keyed on it — a failing canary means DELETE the
workaround).  We owe these upstream; the workaround is the moment it feels
finished and the logging feels skippable.

**The repair pattern**: when the mis-lex breaks the STATEMENT tree (not just a
token), no tree edit can fix it — the enclosing structure is left unfinished
and following statements are swallowed. Repair the RAW TOKEN STREAM
(`set_content`), then `_reparse_doc`. `_repair_swallowing_prototypes` is the
archetype; `_repair_alias_foreach` and `_repair_nary_foreach` follow it.

- **A declaration ATTRIBUTE is not a `Token::Attribute`** inside a
  `Statement::Variable` — it is `Operator(':')` + bare Words. So
  `my $x : shared = 1` matched the `my VAR <tail>` shape and printed EMPTY.
  Strip decorations in ONE document pre-pass, never teach each matcher about ':'.
- **`@{+}` / `${!}` / `%{+}` are VARIABLES**, not derefs: perl's `${ NAME }`
  takes a punctuation name. PPI folds the identifier and caret spellings into
  one Magic token but lexes punctuation ones as Cast + Block holding a lone
  Operator — and a deref block with exactly ONE Operator can never be an
  expression. `$#-` / `$#+` are likewise one Magic token, not an ArrayIndex.
- **`$$` lexes as the PID magic var unless an identifier follows DIRECTLY**
  (repaired by `_split_pid_magic_cast_run`). Cast-run rule: the OUTERMOST cast
  picks the ACCESS KIND, inner casts deref the BASE, an arrow makes them all
  derefs — always probe the MIXED-sigil spellings.
- **`for` only lexes as `foreach [my] $scalar (LIST) BLOCK`.** A `\`-cast loop
  variable (refaliasing) or a non-scalar one, and the perl 5.36 n-at-a-time
  `for my ($q,$r) (LIST)`, all leave the Compound holding just the keyword and
  swallow the rest of the file into one flat sibling. Both repaired s396 by
  re-spelling into constructs PCL already lowers.
- **`Pl::Parser2->parse_code` omits the `(p-defpackage :main)` that `pl2cl`
  emits** — a program that opens with a non-main `package` and later switches
  back dies under any `parse_code`-based Pl/t harness. Use a pl2cl-based one.
