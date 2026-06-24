# Perl core test-suite survey (`t/base`, `t/cmd`, `t/comp`)

**Purpose.** PCL's `perl-tests/` corpus (111 files) is almost entirely `t/op/`.
Perl's own distribution ships many *other* test directories that exercise areas
`t/op/` never touches — and running them through PCL is one of the best bug
finders we have (the framing: *"this is how security people find holes."*).
This file records a survey of the **self-contained** files (no
`require './test.pl'`, no `chdir`) in `t/base`, `t/cmd`, `t/comp`, so we **don't
re-investigate the same files repeatedly**. Update it when a row changes.

> Source tree: `/home/bernt/perl5/perlbrew/build/perl-5.40.3/perl-5.40.3/t`.
> Re-run a row with: `tools/run-perl-suite.pl <rel>` (see "How to re-run").

**Legend:** ✅ PASS (PCL matches perl ok/notok counts) · 🐞 real fixable bug ·
🚧 known-limitation (documented in `not-supported.md`) · 🟡 partial feature ·
P=perl `ok/notok`, C=PCL `ok/notok` at survey time (2026-06-23).

---

## t/base — basic language (mostly self-contained raw-TAP)

| file | P | C | status | notes |
|------|---|---|--------|-------|
| `cond.t`      | 4/0   | 4/0   | ✅ | conditional operators |
| `if.t`        | 2/0   | 2/0   | ✅ | |
| `num.t`       | 56/0  | 56/0  | ✅ | numeric stringification |
| `pat.t`       | 2/0   | 2/0   | ✅ | |
| `while.t`     | 4/0   | 4/0   | ✅ | |
| `translate.t` | 257/0 | 257/0 | ✅ | `tr///` |
| `term.t`      | 7/0   | 6/0   | ✅* | NOT a PCL bug: test 7 opens a relative `harness` file that only exists in perl's `t/` CWD; the runner runs sbcl from the PCL root, so `open` fails and `die`s. Fixture dependency. |
| `lex.t`       | 120/0 | 1/0   | 🐞 | **aborts at test 2**: `$x = $#[0]` mis-parses to an unbound `@#` array (obscure `$#`-lexing). The whole file dies on the unbound-var crash. Niche syntax, but the crash-abort kills 119 good tests. |
| `rs.t`        | 41/0  | 6/35  | 🟡 | `$/` record separator (incl. paragraph mode `$/=""`, fixed-record `$/=\N`, and `*FH` glob passed to a sub). Real I/O feature gap. |

## t/cmd — control flow

| file | P | C | status | notes |
|------|---|---|--------|-------|
| `elsif.t`  | 4/0  | 4/0  | ✅ | |
| `switch.t` | 18/0 | 18/0 | ✅ | (despite the name, no `given/when`) |
| `for.t`    | 16/0 | 11/0 | 🚧 | aborts on `Internals::stack_refcounted` — `Internals::*` is not-supported (`not-supported.md`). |
| `mod.t`    | 15/0 | 14/1 | ✅* | **FIXED 2026-06-24**: `do BLOCK while/until COND` is now a POST-test loop (body runs at least once) via new `p-do-while`/`p-do-until` macros. Remaining 1 fail = test 8 opens a relative `TEST` fixture file that only exists in perl's `t/` CWD (not a PCL bug). |
| `subval.t` | 36/0 | 12/0 | 🟡 | aborts `unbound:$level`. `$level` is a file-lexical `my` mutated by recursive subs; the abort is a closure/recursion capture interaction (cf. fold.t). |

## t/comp — compilation / parsing

| file | P | C | status | notes |
|------|---|---|--------|-------|
| `cmdopt.t`       | 44/0   | 44/0   | ✅ | constant-folding of comparisons |
| `multiline.t`    | 6/0    | 6/0    | ✅ | |
| `term.t`         | 23/0   | 23/0   | ✅ | **FIXED 2026-06-24**: `eval "{ LITERAL , ... }"` anon-hash (string/number key + comma) — PPI mis-tokenized as a bare block (only `=>` triggered its Constructor). New `_bare_block_is_anon_hash` reroute. PPI bug #5 logged. |
| `colon.t`        | 25/0   | 14/11  | 🚧 | almost all fails are `eval "<invalid perl>"` returning undef = **error-detection of invalid Perl** (principle 9 / `not-supported.md`). PCL doesn't reject invalid Perl. |
| `bproto.t`       | 16/0   | 10/6   | 🚧 | `prototype()` introspection — returns undef in PCL (`not-supported.md`). |
| `uproto.t`       | 32/0   | 13/21  | 🚧 | same: prototype introspection + invalid-Perl rejection. |
| `redef.t`        | 20/0   | 1/19   | 🚧 | matches the `"Subroutine X redefined"` / prototype-mismatch **warning text** — error-message wording is not-supported (`not-supported.md`). |
| `our.t`          | 7/0    | 0/7    | 🚧 | built on a `tie`+`AUTOLOAD` `TieAll` harness — tied ARRAY/HASH unwired (`not-supported.md`, tie roadmap). Not actually an `our` bug. |
| `decl.t`         | 9/0    | 3/0    | 🚧 | aborts on `write` (format/`write` not-supported). |
| `form_scope.t`   | 14/0   | 0/0    | 🚧 | `format`/`write` — not-supported. |
| `opsubs.t`       | 36/0   | 0/0    | 🟡 | aborts `unbound:$TODO` — `$::TODO` read inside a sub generated bare `$TODO` with no defvar. Narrow codegen gap (NOT the general undeclared-global case, which returns undef correctly). |
| `fold.t`         | 35/0   | 11/9   | 🟡 | aborts `unbound:$test` — a file-lexical `my $test` interacts with the surrounding `eval q{...}` string-eval blocks (string-eval lexical capture is only partial — `eval-lexical-capture.md`). |
| `package_block.t`| 7/0    | 2/5    | 🐞 | `package NAME { BLOCK }` block-form scoping — 5 fails, investigate. |
| `utf.t`          | 4216/0 | 12/4204| 🟡 | Unicode source / wide-char identifiers; massive divergence, likely one early cascade. Unicode-semantics bucket (`not-supported.md`). |

---

## What the survey teaches (recurring buckets)

Most non-✅ rows fall into already-documented buckets — **do not re-triage these**:

- **Error/warning *text* detection** (`redef.t`, parts of `colon.t`) — PCL
  doesn't reproduce Perl's exact diagnostic wording. `not-supported.md`.
- **Invalid-Perl rejection** (`colon.t`, `uproto.t`) — `eval "<bad perl>"` is
  expected to return undef; PCL accepts valid Perl only (principle 9).
- **`prototype()` introspection** (`bproto.t`, `uproto.t`) — returns undef.
- **`format`/`write`** (`decl.t`, `form_scope.t`) — not-supported.
- **`Internals::*`** (`cmd/for.t`) — not-supported.
- **`tie` ARRAY/HASH** (`our.t`) — unwired; tie roadmap.
- **Unicode semantics** (`utf.t`) — not-supported bucket.

### Genuinely real / fixable (fix targets — NOT yet done)

1. **Crash-aborts that kill a whole file.** When PCL hits an unbound var
   (`@#`, `$TODO`) or an undefined sub (`Internals::*`) at top level, the SBCL
   process aborts and every remaining test in the file is lost. Perl would
   either warn-and-continue (non-strict undef) or `die` into an `eval`. Making
   these *catchable Perl-level errors* (or undef for non-strict undeclared
   globals) would recover dozens of otherwise-passing tests per file.
   *Note:* whether an undeclared var is an error at all **depends on `strict`** —
   under `use strict 'vars'` it is a compile error; without it, it is a package
   global defaulting to undef. PCL targets valid Perl, so the non-strict path
   (undef, no crash) is the one to get right.
2. **`$/` record separator** (`rs.t`) — paragraph mode, fixed-record mode.
3. **`$#[0]` / `$#`-lexing edge** (`lex.t`) — niche, but the crash-abort is the
   real damage; treating an unknown `@#` as empty avoids it.
4. **`package NAME { }` block scoping** (`package_block.t`) — 5 fails.
5. **`comp/term.t` (3) / `cmd/mod.t` (2) / `base/term.t` (1)** — small crash/fail
   counts, individually investigable.

### Not yet surveyed (next sessions)

`t/re/` (80), `t/io/` (44 — partly tracked in `project_io_tests_and_open_errors`),
`t/uni/` (30), `t/mro/` (73), `t/class/` (10), and the rest of `t/op/` not in
`perl-tests/`. Import the highest-signal ones the same way.

## How to re-run

```bash
T=/home/bernt/perl5/perlbrew/build/perl-5.40.3/perl-5.40.3/t
perl tools/run-perl-suite.pl base/rs.t            # one file: perl-vs-PCL TAP + crash sig
perl tools/run-perl-suite.pl --dir comp           # all self-contained files in a dir
```
