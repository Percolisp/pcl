# Opus review requests — s422 (2026-08-22)

Session s422 executed ONE item: **task #419** (`docs/plan-post-s420.md` §1,
Opus session O1 item 1) — a code point above U+10FFFF in a string literal
made the WHOLE emitted file unreadable to SBCL.

There is **one ask** (§4).  Everything else below is the measurement record.

---

## 1. What was done

`Pl/ExprToCL.pm` — one emission rule, in the one place a CL string literal is
written:

* `_cl_string_literal_form` already split the code points that cannot go into
  a UTF-8 source file (surrogates, U+FFFE/U+FFFF) out of a literal as
  `(string (code-char N))` parts of a `(concatenate 'string …)`.  Its
  bad-character class now also covers **everything above U+10FFFF**, and such
  a part becomes `(p-unrepresentable-char N)`: a form that READS, and dies
  when evaluated.  The two classes (`$BAD_CHAR_RE`, `$SAFE_CHARS`) are exact
  complements over the whole code-point range, verified point by point.
* **Rule 11 — four private copies folded in, not one more added.**  The
  `'…'`, `q{}`, `q//` and `s///`-replacement paths each did their own
  `s/\\/\\\\/g; s/"/\\"/g` and returned `qq{"$content"}`.  Each was a
  string-literal writer the new rule would have had to be copied into; all
  four now call `_cl_string_literal_form`.  Byte-identical for clean content,
  and it closes the `s///`-replacement hole for free — `s/b/\x{4000000}/`
  broke the file too, through its own path, and no probe in the task text
  covered it.

`cl/pcl-runtime.lisp` — the runtime half, routed through the existing rule-12
family rather than beside it:

* `%p-unsupported-value` gained the `&optional detail` its announce twin
  `%p-announce-unsupported` already had (it had **no other caller**, so this
  cannot move any existing behaviour);
* `p-unrepresentable-char` (exported) calls it:

  ```
  PCL: string literal: code point 0x4000000 is not implemented — above SBCL's
  char-code-limit (U+10FFFF); see docs/not-supported.md "Code points above
  U+10FFFF (perl's extended UTF-8)"
  ```

`docs/not-supported.md` §"Code points above U+10FFFF" rewritten to state both
answers (see §4), the measured population cost, and the `ord`/`chr`
round-trip precisely (the old text said "`ord` still round-trips the number";
measured s422, that is true for `ord(chr(N))` and false once the value has
been assigned — `my $s = chr(0x4000000); ord($s)` is 65533).

Guard: `Pl/t/wide-codepoint-01.t`, 8 rows, 10.3 s.

## 2. Probes vs perl 5.40.3 — 26 shapes, before and after

Fourteen spellings used to make the file unreadable; every one of them now
leaves it readable and dies only where the value is used:

| shape | before | after |
|---|---|---|
| `"\x{4000000}"` | file unreadable | dies at the expression |
| `"\x{110000}"` (just above the limit) | file unreadable | dies |
| `qq{x\x{4000000}y}` | file unreadable | dies |
| `"\N{U+4000000}"` | file unreadable | dies |
| heredoc `<<"EOT"` | file unreadable | dies |
| interpolated `"a\x{4000000}$v"` | file unreadable | dies |
| `tr/\x{4000000}/Z/` | file unreadable | dies |
| `s/b/\x{4000000}/` | file unreadable | dies |
| `s/b/"\x{4000000}"/e` | file unreadable | dies |
| hash key, `use constant`, sub body, list element | file unreadable | dies |
| `sprintf`/`printf` format string | file unreadable | dies |
| `"\U\x{4000000}\E"` | file unreadable | dies |
| **in DEAD code (`if (0) {…}`)** | **file unreadable** | **costs nothing — A/B both print** |
| `eval { "\x{4000000}" }` | file unreadable | `$@` set, program continues |

Unchanged, perl-oracle verified: `"\x{10FFFF}"` (1 char, ord 1114111),
`"\x{10FFFE}"`, `"\x{FFFE}"`, `"\x{D800}"` (still `(string (code-char
55296))`), `q{\x{4000000}}` (no decoding in perl either, 11 chars both).

Regex shapes (probed, **no divergence in value**): `/\x{4000000}/` → 0 in
both; `qr/[\x{4000001}\x{4000003}\x{4000005}]+/` against `"x"` → 0 in both;
`/a[\x{110000}]?bc/` against `"abc"` → 1 in both.  PCL prints cl-ppcre's own
`Regex syntax error: Unknown token question-mark` on stderr for the first —
filed as **#425** (message quality; it cannot become a wrong value, because
after this fix no PCL string can hold such a character to match against).

`chr(0x4000000)` at run time is **unchanged** and still answers U+FFFD — see
the ask.

## 3. Measurements

| leg | result |
|---|---|
| gate (`tools/prove-core`, `PCLXS_DIR=~/pclxs`) | **156 files / 5622 rows**; failures = exactly the 13 pclxs xs rows (xs-01 5, xs-02 4, xs-03 4).  155/5614 + the new guard's 8 |
| `tools/corpus-diff.pl` (perl-tests, 111 files) | **emission identical to HEAD**; silent drops 7, unchanged |
| `tools/emission-ab.pl --ref HEAD` over perl's `t/` (605) + `lib/**` (22) + cpan-tests/modules (402) | files=1029 **SAME=1027 DIFF=2** — exactly `t/re/pat.t` and `t/uni/variables.t`, the only two files in any population that carry such a literal |
| full sweep `--jobs 3` | **GATE clean, TOTAL passing 18364 (+0)**, drops 7 = census (+0), summary 0 new / 0 fixed; the 6 UNSTABLE + 10 DID-NOT-RUN rows are the usual PARTIAL-file noise (postfixderef, ref, yadayada, eval, magic, tr) |
| companion `re/pat.t` + `uni/variables.t`, `--jobs 1` | see §3.1 |
| artifacts | all three regenerated at gen **v2-164**; each diff is the one-line `gen=` stamp, so the change is emission-neutral for `pack-impl.pl`, `mro.pm` and `warnings.pm` |
| paren checker | balanced |

### 3.1 The population scan, and what #419 actually costs

Scanned all four populations (1205 files) for `\x{N}`, `\o{N}`, `\N{U+N}`
with N > 0x10FFFF and for raw perl-extended-UTF-8 byte sequences (leads
`F5`-`FD`, and `F4` followed by `90`-`BF`):

| population | files | with a spelling | that actually DECODE one |
|---|---|---|---|
| `perl-tests/` | 174 | 2 (index.t, pack.t) | **0** |
| perl's `t/` | 645 | 8 | **2** — `re/pat.t` (8 characters), `uni/variables.t` (1) |
| `lib/**` | 22 | 0 | 0 |
| cpan-tests/modules | 406 | 3 (Text-CSV) | **0** |

The other eleven candidates carry the spelling inside a single-quoted
`fresh_perl_is` string (a child perl's source), inside regex *text* (which
reaches cl-ppcre unprocessed), or as raw bytes in `q{}` — none of them builds
the character in the parent.  **That is why the sweep cannot see this at
all**, and why the emission A/B over the other three populations is the
measurement that matters.

### 3.2 `re/pat.t`: the wall moved, the rows did not — task #424

| tree | verdict |
|---|---|
| `a2ac578` worktree (base) | `P:1263/4  C:0/0  DIFF  crash:sb-c::input-error-in-load: read error during load:` |
| this tree, runners as shipped | `P:1263/4  C:0/0  DIFF` **(empty signature)** — the file LOADS, then **exhausts SBCL's default 1 GB dynamic space** (`Heap exhausted during garbage collection … dynamic_space_size = 1073741824`); SBCL dies hard so no TAP is flushed and the runner logs "PCL produced no TAP output" |
| this tree, `--dynamic-space-size 3072` (measurement-only PCLSbcl edit, **reverted**) | `P:1263/4  C:225/140  DIFF  crash:simple-error: Failed to match at …/re/pat.t line N` |

So **365 rows sit behind the heap**, and a second wall behind those.  None of
the five runners passes `--dynamic-space-size`; `tools/lib/PCLSbcl.pm` sets
only `--control-stack-size 512`.  Raising it is a RUNNER decision with a
shared memory budget (`--all --jobs 8` already OOMs the 10 G scope, s399), so
it is **filed as #424 with three options and their bar**, not taken here.

`uni/variables.t` does **not** move: 1248/319 on the base worktree and
1248/319 after.  Its emission does change (it is one of the two DIFF files),
but the file dies on `unbound:$}` before `load-as-source` ever reads the form
holding the wide literal — verified by running it on an `a2ac578` worktree
(the s421 "PRE-EXISTING is WHEN, not WHY" rule).

`docs/perl-suite-run.tsv` edited ROW BY ROW: `re/pat.t`'s **signature only**,
with the cause and the #424 pointer in the header note.  No other row touched,
nothing re-blessed.

## 4. THE ASK — two answers for one gap: is the asymmetry right?

PCL now answers the *same* unrepresentable value two different ways:

* `chr(0x4000000)` at RUN time → `\x{FFFD}`, program continues.  This is the
  **blessed** ruling (`docs/fable-answers-s318.md` §11, shipped s320,
  `op/chr.t` 40-42 registered XDIFF).
* `"\x{4000000}"` as a LITERAL → `(p-unrepresentable-char N)`, which **dies**.
  This is what task #419 and `plan-post-s420.md` §1.1 specify, and what
  rule 12's boundary says (the missing case would have produced a VALUE the
  program consumes).

I implemented the ask as written, and it is defensible — the compiler knows
the literal and can refuse it, where the runtime given a computed argument
cannot.  But it is still two answers for one gap, which is the shape this
project usually rules against, and I want it ratified rather than inherited.

**The discriminating measurement, since a task must carry one.**  The
alternative (a literal also yields U+FFFD, loudly announced once) was cheap to
size and I sized it: in `re/pat.t` the eight characters sit at line 1357
(`my $str = "\x{110000}"`, feeding four `\p{ASCII_Hex_Digit}` rows) and at
lines 2326-2333 (the ANYOFH block, seven rows).  Under U+FFFD:

* the four `\p{AHEX}` rows would answer on **U+FFFD's** properties, not on
  U+110000's — FFFD is not a hex digit, so all four verdicts come out the way
  perl's do, for the wrong reason;
* in the ANYOFH block **all seven subjects and all three class members
  collapse to the same FFFD**, so every subject matches: the three `like` rows
  pass accidentally and the four `unlike` rows fail — 3 accidental passes,
  4 honest fails, i.e. exactly the accidental-pass family s418 had to unwind
  in `split.t`.

(Neither block is reachable yet: at 3 GB `re/pat.t` dies on an unrelated
`Failed to match` at row 365, so today the die's effect on that file is 0
rows either way.  The sizing above is what the two designs would do once #424
is past.)

That is the argument for dying, and it is why I did not hedge.  What I want
ruled is only whether the *asymmetry with `chr`* is accepted as-is (my
recommendation: yes, and `not-supported.md` + the guard file now say so in
both directions), or whether `chr(N)` should be revisited to match — which
would be its own task, with `op/chr.t`'s registration to redo.

## 5. What was NOT done, and why

* **The heap was not raised.**  It is the thing standing between this fix and
  365+ rows, and it was tempting.  It is a change to the ONE command-line
  builder every runner shares, on a machine whose memory budget is already
  the binding constraint for `--all --jobs 8` — the exact class of change
  s399/#324 says must be decided once, in one place, with its own
  measurement.  Filed as **#424** with the measurement, three options and the
  bar for each.
* **The `Failed to match` die after row 365 was not chased.**  Its text is in
  neither `re/pat.t`, perl's `t/`/`lib/`, the emitted CL, PCL's `.lisp`
  sources, nor `/usr/share/common-lisp` — it is built at run time, and
  locating it needs a live capture behind the raised heap.  Recorded in #424
  as wall 2 with the next step named.
* **No companion `--quick` run.**  The change touches neither name resolution
  nor the harness, the emission A/B over the whole companion population
  isolates the two files it can affect, and both were run individually
  against a base worktree.
* **`"\U\x{4000000}\E"` still makes `pl2cl` print perl's own
  `Operation "uc" returns its argument for non-Unicode code point` warning on
  stderr at transpile time** (from `_apply_case_escapes`).  Cosmetic, one
  corpus occurrence (none), and suppressing it means a `no warnings` in the
  compiler, which s403's ruling reserves for a fixed cause at the narrowest
  scope — left alone deliberately, mentioned here so it is not read as an
  oversight.
