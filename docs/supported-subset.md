# The supported subset (DRAFT, s543, 2026-11-16)

**Status: DRAFT.** The freeze itself is decided (the USER, 2026-11-16: do it
until it is seen to work; DECIDED `## s543`), and so are two of its
boundaries: the UTF-8 flag is the LAST item (6a) and line tracking goes on by
default (6b). Section 6 lists what is still open (6c-6e). Nothing here is
quoted in the README or `docs/STATUS.md` until that is settled.

## 1. Why this file exists

The bug queue is a frontier: every new test population finds new divergences
as fast as old ones close. Between 2026-10-10 and 2026-11-16 the CPAN board
went from 14 to 30 distributions and the count of open tasks marked "silent
wrong" stayed at 105. A number that never falls cannot be steered by.

So the target is fixed. This file says which programs PCL makes a promise
about; the queue is triaged against it; and the work until the exit criteria
in section 5 are met is the IN part of the queue and nothing else.

## 2. The claim

For a program inside the subset, PCL does one of two things:

1. it gives perl's STDOUT and exit status, byte for byte; or
2. it fails LOUDLY: a message at compile time (`PCL: statement dropped ...`,
   `PCL: unsupported ...`) or a perl-shaped `die` at the place perl would
   have continued.

A wrong answer at exit 0 with nothing on stderr -- a SILENT WRONG -- inside
the subset is a bug of the first priority. That is the whole claim. It does
not cover the text of error messages, warnings, run time or memory.

## 3. What is IN

**The language**: perl 5.40 as PCL implements it, minus section 4. `eval
STRING`, closures, `local`, `tie` on scalars / arrays / hashes / handles,
`sprintf`, regexes within cl-ppcre's reach, `fork`, pipes, sockets, file I/O.

**Modules PCL ships** (`lib/`): Carp, constant, Cwd, Digest::MD5,
Digest::SHA, Encode (the codecs listed in `not-supported.md`), English,
Errno, Fcntl, File::Glob, File::Spec, Hash::Util, IO, IO::Handle, List::Util,
Scalar::Util, Sub::Util, MIME::Base64, POSIX, Socket, Storable, Time::HiRes,
Try::Tiny, autodie, bigint / bignum / bigrat, mro, version, Clone,
Compress::Raw::Zlib (over the system zlib), Test::More.

**Pure-Perl core modules the everyday battery exercises**
(`everyday/modules/`): Data::Dumper, Getopt::Long, Getopt::Std, JSON::PP,
File::Basename, File::Compare, File::Copy, File::Path, File::Find,
File::Temp, FindBin, IO::File, IO::Select, IO::Socket, IPC::Open2,
IPC::Open3, Math::BigInt, Math::BigFloat, Term::ANSIColor, Text::Wrap,
Text::ParseWords, Text::Abbrev, Tie::Hash / Tie::Array / Tie::Scalar,
Time::Local, Benchmark, Memoize, Sys::Hostname, Env, overload, parent, base,
fields.

**CPAN distributions on the board** -- IN when the dist's remaining failing
rows are all attributed and none is an unowned silent wrong. Failing rows in
the blessed baseline today (`baselines/cpan-board14-fails.tsv`), by cause:

| dist | bug | UTF-8 flag (#1389) | not-supported | XS | proposal |
|---|---:|---:|---:|---:|---|
| Class-Inspector, Text-Balanced, Class-Method-Modifiers, File-Which, Safe-Isa, Sort-Versions, Algorithm-Diff, Getopt-Long, TimeDate, HTTP-Date | 0 | 0 | 6 | 0 | IN |
| Capture-Tiny, Role-Tiny, JSON-PP, YAML-Tiny, HTTP-Tiny, Mojo-DOM58, Data-Dump, Try-Tiny | 1-4 each | 0-1 | 0-7 | 0 | IN; the rows are the work |
| HTTP-Message | 5 | 1 | 2 | 2 | IN |
| Template-Toolkit | 7 | 20 | 10 | 0 | IN |
| URI | 12 | 31 | 1 | 0 | IN |
| YAML-PP | 14 | 1 | 2 | 0 | IN |
| Data-Dumper | 15 | 0 | 31 | 4 | IN |
| Path-Tiny | 16 | 0 | 26 | 0 | IN |
| Text-CSV | 36 | 189 | 5 | 0 | IN; its 189 UTF-8-flag rows stay OUT until the flag is built (6a, last) |
| Pod-Simple | 38 | 16 | 11 | 0 | IN |
| IO-Compress | 77 | 0 | 16 | 19 | IN for gzip / deflate / zip; the XS files OUT |
| Log-Log4perl | 1 | 0 | 63 | 4 | IN, with `caller` locations as section 4 says |
| Moo | not on the board as a dist | | | | IN (it loads and the everyday rows pass); its own suite is owed a census |
| Scalar-List-Utils | 23 (+388 in one file, #3670) | 1 | 25 | 0 | the SHIMS are IN; the dist's own XS-facing tests are a measure of the shims, not a target |
| Sub-Uplevel | 0 | 0 | 118 | 0 | OUT: it is `caller` fidelity itself |

## 4. What is OUT

The authoritative list is `docs/not-supported.md`; this is the part a
production program is most likely to meet. Each is OUT until its decision
changes, and each fails or differs as that file describes.

| OUT | what the program sees |
|---|---|
| XS modules without a shim (DBI, JSON::XS, Time::Piece, Unicode::Normalize, ...) | `Can't locate ...` naming the XS bridge |
| `DESTROY` at scope exit or at the last reference | runs at program exit; a guard, lock or temp file is released late |
| a filehandle closed by going out of scope | written out at a trigger or at exit, not at the drop |
| `caller` file / line, the location in a `croak` message | perl's in the default build since s543a (6b), except from a BEGIN / END body or an `import` (#5101); frames a program hides (`Sub::Uplevel`), `@DB::args` and eval frames stay OUT |
| `utf8::is_utf8` | always 1 until a real flag exists (6a: the last item of the freeze) |
| `use warnings` diagnostics | none are emitted |
| `format` / `write`, source filters, `given` / `when`, 5.38 `class` | refused with a message |
| regex code blocks, recursion, control verbs | removed or refused with a message |
| Unicode case folding beyond the simple mappings | wrong for `uc("straße")`-class input |
| `@_` aliasing through a reference or a second pass of one variable | a copy |
| taint mode, threads, the debugger | not supported |
| platforms other than Linux x86-64 with SBCL 2.5.2+ | not tested |

## 5. The exit criteria ("it works")

The freeze ends, and the production wording may be written, when ALL hold:

1. **Zero open tasks marked IN and silent-wrong.** Counted by the triage
   field (section 7), printed at every merge.
2. **A held-out sample finds no new silent wrong.** The Rosetta corpus
   (#2104: 655 programs by other authors, 478 identical when last run) is
   re-run; its silent rows inside the subset are fixed; then a FRESH sample
   of programs never used for tuning is run once. A silent wrong in it
   reopens criterion 1, not the subset.
3. **The soak test passes** (#2945): a server-shaped program for an hour;
   resident memory, descriptors and pauses recorded.
4. **No open heap-exhaustion task inside the subset** (#5083, #4904, #4643
   today).
5. **The everyday battery's remaining rows are all OUT by section 4** (it is
   115 of 122; six are OUT today, one is #1391).

Criteria 3 and 4 are for the batch-program claim. The claim for long-running
services adds: scope-exit `DESTROY` decided (6c) and a macOS leg in CI.

## 6. What the USER still has to settle

* **6a. The UTF-8 flag -- SETTLED (the USER, s543): it is done LAST.** The
  content rule (answer 1 only when the string holds a character above 0xFF)
  is REJECTED as wrong, not merely incomplete: perl marks a string as UTF-8
  whatever its characters are, so a decoded string of plain ASCII is flagged
  and a rule that reads the content answers 0 for it. What is built, when
  its turn comes, is a real flag, and it needs its own design (the cost of
  carrying it through string operations has never been measured). Until
  then the 262 board rows keyed on #1389 are OUT.
* **6b. Line tracking by default -- SETTLED (the USER, s543): ON, DONE in
  batch s543a.** `caller`, `croak` and a runtime die name perl's file and
  line in the default build; the cost, measured on the full bench and the
  emitted text, is in `docs/caller-implementation.md` ("What it costs"):
  the median bench row is +0.8 % slower (geomean +1.1 %, base against base +0.6 %), a loop body of a few nanoseconds up to +17 % (feread; the narrowing is sized in #5103), the emitted text +11 % and a fasl +4 %, a cold first run inside its spread -- and an empty `DESTROY` now opens the global-destruction gate (#5107).
* **6c. Scope-exit `DESTROY`.** Parked twice. Not needed for the batch claim;
  needed before the service claim.
* **6d. Moo.** IN or OUT changes the weight of the string-eval compile cost
  (29 % of the board's run phase, #2421) from a speed item to a subset item.
* **6e. The module lists in section 3**: anything to add or strike.

## 7. How the queue is triaged against this file

One pass over the open tasks (917 on 2026-11-16), two review sessions:

* every open task gets one of `in`, `out`, `tooling`, `decision`, with a
  one-line reason; duplicates are merged;
* `out` tasks stay filed and are not scheduled;
* a brief written during the freeze names IN tasks only, silent wrongs
  first, then dropped statements, then loud failures (the s538 order);
* a finding outside the subset is filed and marked `out` -- it does not
  start a batch.

**The first pass is done (s543, 2026-11-16)**: every open task carries a
`triage` field (`in` / `out` / `tooling` / `decision`) and a one-line
`triage_reason`; `tools/task-triage.pl` prints the counts and the number
criterion 1 asks for, and `--list CLASS` prints one class. On 2026-11-16:
927 open -- 634 in, 120 out, 115 tooling, 36 decision, 22 not yet written
(the tasks the two running batches own); **117 IN and marked silent wrong**.
What the pass is and is not:

* it was made from each task`s SUBJECT, not by running its reproducer: 28 of
  the silent wrongs were probed with one shape each and 18 of those did not
  reproduce, so the 117 is an upper bound, and a VERIFICATION batch (each
  task`s own reproducer, perl vs main; close with rows or narrow) gives the
  true count;
* `out` covers both "outside the subset" (section 4, `not-supported.md`, XS,
  parked work, the board`s widening) and "outside the claim" (speed, message
  text, warnings);
* the 36 `decision` tasks are the USER`s: 21 are constructs PROPOSED for
  section 4 (glob and stash surgery, arylen magic, refaliasing and declared
  references, a few exotic spellings), the rest are standing questions
  (scope-exit DESTROY 6c, perl`s false as a dualvar, bytes 128-255 without
  `use utf8`, the layer model, the numeric model, `:lvalue` subs, the macOS
  leg);
* a silent wrong is counted only when its task SAYS so (subject or
  description): the label is part of filing one.

What pauses: widening the board, new steering populations, perl's own suite
rows that no IN program reaches. What does not pause: the round instruments,
the PPI branch when the expect-state checker reports a new mis-lex in an IN
program, the gate.
