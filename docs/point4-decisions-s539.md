# Point 4 of the compatibility plan: three decisions for the USER (s539, 2026-11-12)

The compatibility plan (DECIDED `## s538`) says three questions block application-shaped
code and are the USER's to decide, not the reviewing session's: scope-exit `DESTROY`
(#2370), `caller` fidelity (#233 with #1361), and the UTF-8 flag / malformed-UTF-8 string
representation (#1389).  The board hunts have reached all three.  This note gives each
one's measured size, what was found while sizing it, the options, and a recommendation.
Nothing here is decided or built.

**RULED BY THE USER the same day (s539, 2026-11-12; DECIDED `## s539`):**

1. **The UTF-8 flag: left alone for now.**  The always-1 ruling stands; the measurement below stays as the record for when the question is re-opened.
2. **`caller`: made better where that is cheap in execution time and code size.**  Tier 1 below becomes a batch in which each item ships only if it is measured free (brief `briefs-and-rules-for-claude-subagents/s539/s539d-prompt.md`); tier 2 is not scheduled.
3. **Scope-exit `DESTROY`: stays parked**, as recommended; the returned-handle case is a small member of a later batch.

The CPAN board's blessed baseline holds 1,909 failing rows (`baselines/cpan-board14-fails.tsv`,
main `d1e98177`).  The three questions hold 740 of them:

| question | board rows | whole files stopped | what a user sees |
|---|---|---|---|
| `caller` fidelity (#233, #1361) | 458 | 0 | wrong file / line / sub in logs and in every `croak` message |
| UTF-8 flag (#1389) | 261 | 0 | text that is never decoded, or decoded twice: wrong output, exit 0 |
| scope-exit destruction (#2370, #2006 b) | 21 | 7 | a file not flushed until exit; a guard that fires late |

## 1. The UTF-8 flag (#1389) -- RECOMMENDED: change the ruling, small batch

**What it is.**  `utf8::is_utf8($s)` / `Encode::is_utf8($s)` answer 1 for every string in
PCL (ruled s473h: PCL has one string form).  Modules ask that question to decide "is this
bytes or characters", and on the answer 1 they skip a decode or accept a binary character.
The result is wrong text at exit 0, the plan's first-priority class.

**Measured now (real perl, no PCL work; `~/pcl-agent-scratch/s539/p4/emurun.pl` + `emu/IsUtf8Emu.pm`).**
The seven board dists that carry #1389 rows were run under real perl with the two functions
replaced by a candidate answer:

| dist | rows | PCL's answer today (always 1) fails | content rule fails |
|---|---|---|---|
| Text-CSV 2.04 | 52,558 | 197 | 10 (`50_utf8.t`; 2 of them fail today too) |
| Pod-Simple 3.47 | 1,469 | 24 | 0 |
| URI 5.34 | 998 | 31 | 7 (`iri.t`; 4 fail today too) |
| Template-Toolkit 3.106 | 2,850 | 20 | 0 |
| Data-Dump 1.25 | 149 | 1 | 3 (`quote-unicode.t`, new) |
| HTTP-Message, HTTP-Tiny | 1,314 | 0 | 0 |
| **total** | | **273** | **20** (14 of them new) |

The "content rule" is: 1 if the string holds a character above 0xFF, else 0.  It needs no
flag and no propagation; it is a scan of the string at the call.  The always-1 column
reproduces the board's 261 rows, which says the emulation measures the right thing.

**Why it was rejected before, and what changed.**  s473h rejected the content rule because
it gets `utf8::upgrade($s); is_utf8($s)` wrong (the ir-conform row 243) and answers 0 for a
decoded string that happens to hold only narrow characters.  Both are true.  What that
ruling did not have is the count: on the board the always-1 answer costs 273 rows and the
content rule 20.

**Options.**
* (a) Keep the ruling.  261 board rows stay not-supported.
* (b) The content rule.  One small runtime batch; expected about 250 board rows fixed and
  about 14 new ones, each of which is then a known residue with a name.
* (c) The content rule plus a mark on the string object for the narrow-but-decoded case
  (the result of `decode` / `utf8::upgrade` / `utf8::decode` is recorded by identity, with
  no propagation through string operations).  It fixes the ir-conform row and some of the
  20; it costs a weak table probe inside `is_utf8` only.  To be measured in the same batch
  and kept only if it pays.
* Not proposed: a real per-string flag with propagation through every string operation.
  That is the data-model change s473h named, and the measurement does not ask for it.

**Recommendation: (b), with (c) measured inside the batch.**  The malformed-UTF-8 question
(#4624: what a string read through a lax `:utf8` layer holds for a bad byte) is separate
and smaller; it stays as it is until a board row needs it.

## 2. `caller` fidelity (#233, #1361) -- RECOMMENDED: un-defer the cheap tier, measured

**What it is** (probe `~/pcl-agent-scratch/s539/p4/c1.pl`, main `d1e98177`):

| asked | perl | PCL today |
|---|---|---|
| `caller` (no argument) | 3 fields, `c1.pl`, line 2 | 4 fields, the generated `.lisp` file, a Lisp line |
| `caller(0)` in an anonymous sub | `main c1.pl 6 main::__ANON__` | empty |
| `caller(1)` file and line | the `.pl` file and line | the `.lisp` file and line |
| `(caller(0))[5]` (wantarray) | true in list context | false |
| after `# line 100 "foo.pl"` | `foo.pl`, 100 | ignored |
| `croak "bad arg"` from a module | `bad arg at foo.pl line 102.` | `bad arg` (no location) |
| `die "x"` | `x at FILE line N.` | right (the location registers of #1240) |

So `die` already knows the Perl file and line; `caller` does not read them.

**Size.**  458 board rows, the largest single cause (24 % of the baseline): Scalar-List-Utils
`exotic_names.t` 260 (an anonymous sub has no frame), Sub-Uplevel 123 (103 need frame
hiding and the `CORE::GLOBAL::caller` override), Log-Log4perl 64 over 14 files (the `%F %L
%M` layout fields and `logcarp`), Template-Toolkit 8 (`#line`), Try-Tiny 3.  Beside the
board: 36 rows of `perl-tests/caller.t`, 43 companion rows, and #1361: every module
diagnostic raised through Carp loses its location, in every program.

**Why it was deferred.**  A priority call for speed (USER, s488): no per-call bookkeeping
without a measured reason.  It was never a feasibility question; no debugger state is
involved.

**Options.**
* (a) Keep the deferral.
* (b) Tier 1, one batch with a speed bar: `caller` reads the file and line the frame
  already saves for `die`; three fields for the no-argument form; the empty list past the
  top of the stack; the wantarray field; `$0`; a frame for an anonymous sub; `#line`
  (a compile-time change of the constants the location register is given); `croak` /
  `carp` built on the now-correct `caller`.  Bar: the sub-call and method-call bench rows
  inside the noise band, anything above 1 % flagged before the merge.  Expected: the
  Log-Log4perl, Template-Toolkit and Try-Tiny rows, `exotic_names.t`, part of Sub-Uplevel,
  most of `caller.t`, and Carp locations everywhere.
* (c) Tier 2, a design first: frames a program can hide (`Sub::Uplevel`, which
  `Test::Exception` is built on) and the `CORE::GLOBAL::caller` override.  About 110 board
  rows.  It matters for point 5 (widening the board), because many CPAN test suites use
  `Test::Exception`.

**Recommendation: (b) now, as one measured batch; (c) designed when the widened board
shows how many suites stop on `Test::Exception`.**

## 3. Scope-exit `DESTROY` (#2370) -- RECOMMENDED: stays parked; the board does not ask for the design that exists

**What exists.**  Stage 1 (a filehandle that does not leave its block is closed at the
block's exit) is on main.  Stage 0 (`DESTROY` for every surviving object at program exit,
#2371) is being built now (batch s538b).  Stage 2 (#2370) is the parked design in
`docs/scope-exit-destroy-design.md`: a lexical scalar that provably owns its object
releases it at scope exit; four to six agent-days plus a 60-shape retention battery; the
risk is a `DESTROY` on a live object if one retention route is missed.

**Found while sizing it: Stage 2 as designed reaches none of the board's whole-file losses.**
Read from the sources, not measured:

| board file(s) | rows in perl | how the object is held | Stage 2 as designed |
|---|---|---|---|
| Try-Tiny `context.t`, `finally.t` | 25, 30 | guards in an array under a `local` hash element | no (a container holds it; `local` is on the deny side) |
| IO-Compress `104destroy-*.t` (4) | 15+ each | a glob tied to itself, blessed from `Symbol::gensym()` | no (tie is on the deny side; the blessed value is not fresh) |
| Template-Toolkit `leak.t` (2 rows) | | objects in the template stash | no (a container holds it) |
| Pod-Simple `htmlbat.t` (8 rows) | | a handle in an object field, dropped by `undef` | no (a container holds it) |
| Path-Tiny `sig_die.t` | 4 | `my $fh = $file->openw` in a bare block | yes (a handle returned by a helper; the design's widening of Stage 1) |
| YAML-PP `49.include.t` (1 row) | | not read | unknown |

Two more facts.  A program that uses `Try::Tiny` gets PCL's own shim, where `finally`
works; the board rows are the dist testing its own guard-based copy.  And after Stage 0 an
unflushed compressor is complete once the program ends; what stays wrong is reading the
file back inside the same program.

**Options.**
* (a) Keep #2370 parked; re-read these rows after Stage 0 merges.
* (b) A narrow extension instead of Stage 2: the handle returned by a helper
  (`sig_die.t`; the largest residue of the s494 handle census, 4 of 7 escaping sites).
  Small, and it is the same licence Stage 1 already has.
* (c) Stage 2 in full.  The board gives no reason for it today.

**Recommendation: (a) and (b).**  What would re-open (c): a dist on the widened board
(point 5) whose rows are lexical-scalar owners -- a lock guard, a database handle, a
`File::Temp` object in a block.

## What each recommendation would start

| decision | if yes | size |
|---|---|---|
| #1389 content rule | one runtime batch; not-supported entry and ir-spec rewritten; ir-conform row 243 re-keyed | small |
| #233 tier 1 | one batch with a bench bar; #1361 closes with it | medium |
| #2006 (b) returned handle | a member of a later batch | small |
