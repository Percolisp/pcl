# IO-Compress: the #3494 umbrella sorted by cause (s521 census, 2026-10-18)

Read-only census (Opus agent, s521, 2026-10-18 evening).  It answers the debt
#3494 states: s521a (MAIN-READY-33) un-stuck 33 IO-Compress test files, and the
failing rows that had been hidden behind the old stops went into
`baselines/cpan-board14-fails.tsv` under one umbrella cause, "not yet sorted".
This page sorts every one of them by its real cause.  It changes nothing in the
tree.  The proposed re-caused baseline is a separate file:
`~/pcl-agent-scratch/s521/ioc-census/cpan-board14-fails.proposed.tsv` (identical
to the blessed file except column 7 of the #3494 rows; header lines untouched).

**Inputs.** The blessed rows, 2026-10-18 (`grep -a` -- 710 data rows cite #3494;
the s521a header note says 713, a 3-row difference no data row explains -- the
header count is probably off, or three rows were edited out later; 710 is what
is there).  The current measurement is the round-2 board on MAIN-READY-34
(`~/pcl-agent-scratch/s521/board-r2.rows.tsv`, tree 70a73d5d).  Probes were
run through `~/pcl-agent-scratch/s513/probe.sh` against perl 5.40.3 and the main
checkout; scratch is `~/pcl-agent-scratch/s521/ioc-census/` (`p/` = probes,
`classify.pl` / `propose.pl` = the sort, `classified.tsv` = every row with its
class).

## 1. Still failing?  Joined against round 2

The join is on (t-file, description, got, expected), which is stricter than the
board's own key:

* **703 of 710 rows still fail in round 2 with the same got / expected.**
* The 7 that do not match are not fixes.  Five are `*PLAN*` rows whose text
  moved: 107multi-* now say `rc 9` instead of `rc 2`, because die's exit status
  is `$!` (EBADF, 9) since the closed-handle work.  cz-14gzopen says `rc 0`
  instead of `rc 2`.  The other two are the 105oneshot-zip-only rows
  `unzip tstNNNNNNX000NN.tmp ok`, whose description contains a pid-named temp
  file, so the key changes on every run (see §5).
* **FIXED candidates: none on round 2.** The rows that will leave soon are the
  five #3530 rows, once s521c (member 2) merges (§3, cause 8).

## 2. Shapes: rows by file and got / expected

Most rows come from the shared drivers `t/compress/*.pl`, so one shape repeats
across the four compressors (deflate / gzip / rawdeflate / zip).

| # | shape (first row's got / expected) | rows | files | cause | SILENT |
|---|---|---|---|---|---|
| 1 | `$. is 4`: got `undef`, expected `'4'` (and `'0'` `'1'` `'2'`) | 480 | 107multi-{deflate,gzip,rawdeflate,zip} (120 each) | **NEW-A** | **yes** |
| 2 | `exit status is 0`: `'512'` / `'0'`; `no stderr`: `'Compress::Raw::Zlib version 2.214 required--this is only version 2.212 at …blib/lib/IO/Compress/Adapter/Deflate.pm line 8.'`; content `''` / `'hello…'` | 84 | 011-streamzip (28 + 28 + 28) | **NEW-C** (board environment) | no |
| 3 | `'IO::Compress::Deflate::seek: cannot seek backwards` / `to match: /^…cannot seek backwards\s+at /`; also `unknown value, 10, for whence parameter`, `SEEK_END not allowed`, `write: not a scalar reference` (pattern `/not a scalar reference /`), `read: length parameter is negative`, `deflate: unknown key value(s) Fred`, `input filename is undef or null string`, `gzseek: …` | 76 | 100generic-* (11 each), 103newtied-* (5 each), 102tied-* (1 each), 105oneshot-deflate (3), cz-14gzopen (5) | `NS:Error messages` = **#1361** | no (the text is right; the location is missing) |
| 4 | rows 190-199 of 100generic: `'0'` / `'3'`, `''` / `'Thi'`, `'  '` / `'Ths i'`, `'ab  '` / `'ab  s a'`, `'0'` / `'47'`, … | 40 | 100generic-* (10 each) | **#330** (knock-on from the row above them) | yes (knock-on) |
| 5 | `unzip ok`: (empty) / (empty); `content ok`: `''` / `'hello '`, `''` / `'goodbye '` | 11 | 105oneshot-zip-only | **NEW-D** | **yes** |
| 6 | `$gz`: (empty) / (empty) (isa_ok on undef) + `*PLAN*` `ran 1227 of 1829` | 8 | 107multi-* (2 each) | **NEW-B** | **yes** |
| 7 | `''` / `to match: /buffer parameter is read-only\s+at /` (100generic row 187); `Output is read-only`: `''` / `/…output buffer is read-only/` (105oneshot-deflate row 5) | 5 | 100generic-* (1 each), 105oneshot-deflate (1) | **#330** | no |
| 8 | cz-14gzopen 150 / 161: `'ok 142 -   redirect STDOUT\n<gzip bytes>'` / `'hello'`; 146 / 157 `length 52, expected 5`; `*PLAN*` `ran 263 of 265` | 5 | cz-14gzopen | **#3530** | no |
| 9 | `gzopen with missing mode fails`: `'gzopen: file parameter is not a filehandle or filename` / `/Not enough arguments .*? Compress::Zlib::gzopen\s+at /` | 1 | cz-14gzopen | `NS:Error compatibility for invalid Perl input` | no |
| | **total** | **710** | | | |

## 3. Causes, with verdicts

### Cause NEW-A: `$.` assigned while no handle has been read is lost (480 rows, SILENT)

* **Mechanism.** `cl/pcl-runtime.lisp` `%p-dot-set` stores the value only on
  `*p-last-read-handle*`, and `%p-dot-get` returns undef when there is no such
  handle.  In perl, `$.` is a magic SV.  With no last-read handle, the set goes
  nowhere but the SV keeps the assigned value, and get leaves it alone.
  `IO::Uncompress::Base::getline` does `$. = ++ *$self->{LineNo}`;
  `nextStream` / `input_line_number` / `reset` also assign `$.`.  The module
  reads its input with `read()`, which does not make a handle current in perl
  either.  So after every `getline` loop, `is $., N` reads undef.  This happens
  in all three modes: file, filehandle and buffer.
* **Discriminating probe** (`p/dot.pl`): a package whose method does
  `$. = ++$n` three times, then `print $.`, then `$. = 7; print $.`.
  perl prints `3`, `0`, `7`.  PCL prints `undef` all three times.  **Confirmed**,
  on main.
* **Verdict.** Layer: cl (one magic cell).  Size: small.  The magic cell needs
  a fallback value slot.  The setter stores into the slot when no handle is
  current; the getter returns the slot when no handle is current.  `local $.`
  (`p-local-dot`) must also save and restore that slot: Base.pm has
  `local $.` at lines 202 / 1377 / 1394.  Blocked by: nothing.  Rows: 480, all
  of the `$. is N` rows of the four 107multi files.  This is the biggest single
  cause in the umbrella.  It is a rule-12 shape: a value is silently wrong.

### Cause NEW-C: 011-streamzip's child is the oracle perl with an older Compress::Raw::Zlib (84 rows, board environment)

* **Mechanism.** The test runs `"$^X" "-MExtUtils::testlib" -w bin/streamzip …`.
  Under PCL, `$^X` is the box perl 5.40.3 (#2689, by design), so the CHILD is
  real perl.  ExtUtils::testlib puts the dist's `blib/` first.
  `blib/lib/IO/Compress/Adapter/Deflate.pm` line 8 asks for
  `Compress::Raw::Zlib 2.214`, and that perl's core copy is 2.212, so every
  child dies at load: exit 512, a stderr line, empty output.  These are 28
  invocations × 3 rows each.
* **Discriminating probe.** `perl -MExtUtils::testlib -w bin/streamzip < in`
  from the dist root gives the same message and rc 2.  The same command with
  `PERL5LIB=~/.cpan/build/Compress-Raw-Zlib-2.214-0/blib/{lib,arch}` gives
  rc 0 and a 124-byte zip.  **Confirmed.**  PCL's parent never fails here:
  PCL's own `lib/Compress/Raw/Zlib.pm` says 2.214.
* **Verdict.** Layer: the BOARD (tools), not PCL.  Size: small.  Two options:
  (a) when the board runs a dist, put that dist's built prerequisites' `blib`
  on `PERL5LIB`, as `make test` under the CPAN shell would (here
  Compress-Raw-Zlib-2.214-0 is already built under `~/.cpan/build`); or
  (b) bless these rows as a box-environment class.  These 84 rows say nothing
  about PCL.  With (a) the file measures streamzip's parent side for real.
  Recommend (a).  Rows: 84.

### Cause #1361 / `NS:Error messages`: croak location (76 rows)

* Every row: the got TEXT contains the pattern's literal part, and the pattern
  then needs `\s+at ` (or a trailing space) that only perl's
  ` at FILE line N.` supplies.  This was checked mechanically for all 76
  (`classify.pl`: the literal is a substring of got, and got has no
  ` at … line N`).
* This is the same family as the 112 rows already blessed that way.  The review
  (`docs/cpan-board-failure-review-s521.md` §3) confirmed the mechanism
  (lib/Carp.pm ends the message with `"\n"`) and that it is held behind #233's
  deferral.  The proposed file uses the existing cause text verbatim, so one
  grep counts the whole family: 112 + 76 = **188 rows**.
* Verdict: no new work.  The rows are #1361's.  It is the largest blocked
  family on IO-Compress.

### Cause #330: read-only scalars, plus the rows that fail because of it (45 rows)

* 5 direct rows: 100generic row 187 (`eval { $io->read(1) }` must croak
  "buffer parameter is read-only") and 105oneshot-deflate row 5 ("output buffer
  is read-only").
* 40 knock-on rows (100generic rows 190-199 × 4).  Because `Scalar::Util::readonly($_[0])`
  is false for the literal `1`, the read goes on.  It reads the WHOLE stream
  (no length), so every following `read` / `sysread` / `eof` row sees an empty
  stream.  **Probed** (`p/ro.pl`): perl prints `err=[read-only] n=3 buf=[Thi]`;
  PCL prints `err=[] n=0 buf=[]`.  These are **silent knock-on failures** of a
  value-level gap.
* Verdict: #330's.  Fixing #330 should clear the 40 knock-on rows with it.

### Cause NEW-D: a second package's constant loses its empty prototype under an import list (11 rows, SILENT)

* **Mechanism.** `IO::Compress::Base::Common` defines `package U64` with
  `use constant MAX32 / HI_1 / LOW / HIGH`.  `IO::Uncompress::Unzip` loads it
  as `use IO::Compress::Base::Common 2.214 qw(:Status)`, and the compiler then
  has NO prototype for `U64::MAX32`.  The detector table in the emitted
  Unzip.pm lists `("U64" "MAX32" 421 "unknown")`.  So
  `$sizeHi * U64::MAX32 + $sizeLo` compiles as
  `(U64::pl-MAX32 $sizeLo)`, a CALL that takes `+$sizeLo` as its argument.
  The size is then 0, so `skipCentralDirectory64Rec` skips 0 bytes, reads
  garbage as the next signature, and `unzip` returns undef with an EMPTY
  `$UnzipError`.  Only Zip64 archives reach that path: 4 of 8 combinations in
  the first loop, and the second file of each Zip64 archive in the second
  loop.
* **Discriminating probes.**
  * `p/zo.pl` (8 combinations): perl gives all `st='1'`.  PCL gives `undef`
    exactly for `Zip64 => 1`.
  * The written zip64 bytes are IDENTICAL between perl and PCL (`p/zw.pl`), so
    the fault is on the reading side.
  * An instrumented scratch copy of Unzip.pm prints `FF64 buf=2c00000000000000
    lo=44 hi=0 max=4294967295` followed by `size=0`.
  * Minimal (`p/u64b.pl`, core module, no dist needed):
    `use IO::Compress::Base::Common qw(isaFilename); my ($lo,$hi)=(44,0); print $hi * U64::MAX32 + $lo`.
    perl prints **44**; PCL prints **0**.  With `()` or with no import list,
    PCL emits `(p-+ (p-* …(U64::pl-MAX32)) …)` and gets it right.  So the
    import-list path (`Pl/Parser.pm` `_merge_module_prototypes` returns after
    importing the listed names) is the one that loses it, while the no-list
    path finds it through the bare-name fallback.
  * A hand-written module with the same layout (a constant in a second
    package, an Exporter list, an import list) is parsed correctly, so the
    exact trigger inside Common.pm is **not isolated**.  The core-module
    reproducer above is still exact and cheap.
* **Verdict.** Layer: Pl (module-prototype merge; this is the #421 / #3143
  family of bare-name versus qualified keying).  Size: small to medium.
  Blocked by: nothing.  Rows: 11 here.  Any Zip64 unzip under PCL is silently
  wrong, so the real reach is beyond these rows.  **Rule-12 shape.**

### Cause NEW-B: a scalar that held a glob ref keeps the glob when given a scalar ref (8 rows + 2,408 never-run, SILENT)

* **Mechanism.** multi.pl's outer `my $cc` holds an `IO::File` in the
  filehandle pass.  The buffer pass then does `$cc = $output`, where
  `$output = \$compressed`.  On PCL, `$cc` still answers the old glob:
  `ref($cc)` is `IO::File`, `$cc == \$compressed` is false.  So Gunzip reads
  the closed handle and fails with "Header Error: Minimum header size is 10
  bytes".  `isa_ok $gz` fails (row 1226), and the next `$gz->read` dies
  "Can't call method "read" on an undefined value".  That is the `*PLAN*` stop
  at 1227 of 1829.
* **Discriminating probes.**
  * `p/mb2.pl` (filehandle then buffer, outer `$cc`) fails exactly like the
    test.  `p/mb3.pl` (the same with an inner `my $cc`) and `p/mb4.pl`
    (buffer twice) pass.
  * Minimal (`p/gr2.pl`):
    `my $c = bless Symbol::gensym(), 'Foo'; $c = \$s; print ref $c`.
    perl prints `SCALAR`; PCL prints `Foo`.  Unblessed `gensym` gives `REF`
    on PCL; hash or scalar referents are fine.
  * Identical on base-5e6e241d, base-ed8a4504 and base-0b836815, so it is
    **pre-existing**: not s521a's #3433, not #3516.
* **Verdict.** Layer: cl (the box assignment path for a box whose value is a
  typeglob; the #423 "glob value ≡ glob ref" box model).  Size: medium.
  Blocked by: nothing, but it touches the same representation as #3516 /
  #3433, so it should be designed with #3516.  Rows: 8 blessed, plus
  **602 × 4 = 2,408 rows that never run** behind the stop.  **Rule-12 shape.**

### Cause #3530: TAP written into a redirected STDOUT (5 rows)

* cz-14gzopen does `open(STDOUT, ">$name")` and checks the file.  PCL's TAP
  line `ok 142 -   redirect STDOUT` (and `ok 153`) is written INTO that file,
  so the file reads 52 bytes, not 5 (rows 146 / 157), and its content is TAP
  plus gzip data (150 / 161).  The `*PLAN*` row is 2 short (263 of 265)
  because those two TAP lines never reach the harness.
* **Probed** with a one-file run on main (`p/gzo.out`): the file runs to 265,
  and the diags show the TAP line inside the data.
* Verdict: #3530's mechanism ("Test::More writes through its own dup of
  fd 1").  s521c member 2 implements it and is not yet merged.  These 5 rows
  are FIXED candidates at s521c's acceptance run.

### `NS:Error compatibility for invalid Perl input` (1 row)

* cz-14gzopen row 163: `eval ' $fil = gzopen() '`.  In perl this is a
  prototype COMPILE error ("Not enough arguments for Compress::Zlib::gzopen").
  Principle 9 covers it: PCL does not reject invalid Perl.

## 4. Counts per proposed cause

| proposed cause | rows | class |
|---|---|---|
| NEW-A `$.` with no handle | 480 | bug (to file) |
| NEW-C board env (streamzip child perl) | 84 | to file (board) |
| `NS:Error messages` (#1361) | 76 | not-supported |
| #330 (5 direct + 40 knock-on) | 45 | bug |
| NEW-D `U64::MAX32` prototype lost | 11 | bug (to file) |
| NEW-B glob-ref box keeps the glob | 8 | bug (to file) |
| #3530 | 5 | bug (fixed in s521c, unmerged) |
| `NS:Error compatibility for invalid Perl input` | 1 | not-supported |
| **total** | **710** | |

`#NEW-A` … `#NEW-D` are placeholders in the proposed file.  Fable files the four
tasks below and replaces each `#NEW-X` with its number:
`perl -pe 's/#NEW-A\b/#NNNN/'` etc. on the proposed file before applying it row
by row.  No other column of any row changed.

### Task texts, ready to file

**NEW-A.**
> SILENT WRONG: `$. = N` while no handle has been read is LOST.
> `%p-dot-set` stores only on `*p-last-read-handle*`, and `%p-dot-get` answers
> undef without one.  perl keeps the assigned value in the `$.` SV.
> `package F; sub g { $. = ++$main::n } package main; F->g for 1..3; print $.; $. = 7; print $.`:
> perl prints `3` and `7`; PCL prints undef twice.
> IO::Uncompress::Base::getline / nextStream / input_line_number assign `$.`
> after reading with `read()`, so every `is $., N` in IO-Compress 107multi-*
> reads undef: **480 board rows**.
> Fix shape: a fallback value slot in the `$.` magic cell (the setter writes it
> when no handle is current, the getter reads it); `p-local-dot` saves and
> restores it too.  Layer cl, small.  Guard: the probe plus `local $.` around
> an assignment.

**NEW-B.**
> SILENT WRONG (pre-existing, 5e6e241d): a scalar that holds a GLOB REF keeps
> it when assigned a scalar ref.
> `use Symbol; my $s = 1; my $c = bless Symbol::gensym(), 'Foo'; $c = \$s; print ref $c`:
> perl prints `SCALAR`, PCL prints `Foo`.  An unblessed gensym gives `REF`, an
> IO::File gives `IO::File`, and `$c == \$s` is false.
> IO-Compress multi.pl reuses an outer `$cc` that held an IO::File, so
> Gunzip->new reads the dead handle ("Minimum header size is 10 bytes").
> 107multi-* stop at 1227 of 1829: 8 blessed rows plus **2,408 rows never
> run**.  Hash and scalar referents are fine.
> Layer: cl, the box store for a box whose current value is a typeglob (the
> #423 box model).  Design it with #3516.

**NEW-C.**
> BOARD: a dist test that runs `$^X` (the oracle perl, #2689) gets that perl's
> core modules, which can be older than the dist's own prerequisites.
> IO-Compress 011-streamzip runs bin/streamzip under perl 5.40.3, whose
> Compress::Raw::Zlib 2.212 fails the blib IO::Compress 2.214's
> `use Compress::Raw::Zlib 2.214`, so all 28 children die at load: **84 rows**.
> With `PERL5LIB` holding `~/.cpan/build/Compress-Raw-Zlib-2.214-0/blib/{lib,arch}`
> the child exits 0.
> Fix: `tools/run-dist-t.pl` / the board puts the dist's built prerequisites'
> blib on `PERL5LIB` for the run (the `make test` under the CPAN shell
> environment).  Or, if that is ruled out, these rows get a box-environment
> cause, not a PCL one.

**NEW-D.**
> SILENT WRONG: an imported module's SECOND-package constant loses its empty
> prototype when the module is used WITH an import list.
> `use IO::Compress::Base::Common qw(isaFilename); my ($lo,$hi)=(44,0); print $hi * U64::MAX32 + $lo`:
> perl prints 44, PCL prints 0 (`(U64::pl-MAX32 $lo)`, a call).  With `()` or
> with no list it is right.  The detector table lists `("U64" "MAX32" … "unknown")`.
> `_merge_module_prototypes`' import-list branch returns before anything else
> registers U64's records (the no-list path finds them only through the
> bare-name fallback, #421 / #3143 family).  A hand-made module with the same
> layout did NOT reproduce, so the trigger inside Common.pm is still to be
> isolated (start from the core module).
> IO::Uncompress::Unzip computes a size of 0 for every Zip64 record: every
> Zip64 unzip under PCL returns undef with an empty `$UnzipError`
> (105oneshot-zip-only: 11 rows).  Layer Pl.

## 5. What the board does not see for IO-Compress

* **Files still stopping short** (round 2):
  * 107multi-{deflate,gzip,rawdeflate,zip}: ran 1227 of 1829, **NEW-B**
    (2,408 rows never run).
  * 105oneshot-{deflate,gzip,rawdeflate,zip}: ran 8 of 1008 or no TAP, #3493
    (about 4,000 rows never run).
  * 004gziphdr: 450 of 919, #3432 eq_set (fixed in s521c, unmerged); rc is now
    255.
  * 104destroy-*: 5 of 16, #2370 DESTROY (USER-parked).
  * 109merge-{deflate,gzip,rawdeflate}: 10 of 166, InflateScan (#3161 residue).
  * 010examples-zlib: 2 of 20, #3493.
  * 112utf8-zip: 20 of 29, #3495 `Test::Builder->details`.
  * cz-06gzsetp: 6 of 52, croak location.
  * cz-03zlib-v1: TRANSPILE-FAIL #3347.
  * cz-14gzopen: 263 of 265, #3530 (above).
* **XS bzip2:** 11 files with no TAP plus 001bzip2 (ran 1 of 842):
  Compress::Raw::Bzip2 has no PCL build (`NS:DynaLoader`, pclxs parked).
  105oneshot-zip-bzip2-only skips itself for the same reason.
* **The 105oneshot flap** (blessed header, s519b).  In round 2,
  105oneshot-rawdeflate ran 8 rows (its 4 croak / read-only rows and a
  `*PLAN*` that are not in the blessed file), and 105oneshot-zip produced no
  TAP (blessed: `*PLAN*` ran 8).  The two have swapped again.  These rows are
  outside #3494 and were left alone.  If the flap is ever chased, the
  0-row-versus-8-row difference is the place to look.
* **Perl skips** (unchanged, `PERL-SKIP`): 020isize, 101truncate-*,
  105oneshot-zip-store-only, 106prime-* (deflate/gzip/rawdeflate/zip),
  109merge-zip, 999meta-json, 999meta-yml, 999pod.
* **Unstable join keys.** 105oneshot-zip-only's `unzip tstNNNNNNX000NN.tmp ok`
  rows name a pid-based temp file, so every board run reads them as one LOST
  plus one NEW row.  A normalisation of `tst\d+X\d+\.tmp` in the runner's
  description (or in the join) would stop that.
* **Litter.** The dist directory holds unreadable `tst*X00015.tmp` files left
  by earlier runs (`cp -a` of the dist fails on three of them).  A LexFile that
  is never DESTROYed (#2370) plus a file chmod'ed to 0 by a test.  This is
  harmless to the board, but a copy of the dist needs `--no-preserve` or a
  skip.

## 6. Probes run

All are under `~/pcl-agent-scratch/s521/ioc-census/p/` and were run through
`probe.sh` (perl vs `runpcl` on main):

* `dot.pl`: NEW-A.
* `mb.pl`, `mb2.pl`, `mb3.pl`, `mb4.pl`, `gr.pl`, `gr2.pl`, `gr3.pl`: NEW-B.
  Also run on three base extractions.
* `zo.pl`, `zw.pl`, `zr.pl`, `u64.pl`, `u64b.pl` and the `qc*` modules:
  NEW-D, plus an instrumented Unzip.pm in `../distcopy`.
* streamzip by hand, with and without the Zlib 2.214 blib: NEW-C.
* `ro.pl`: #330 knock-on rows.
* Four one-file dist runs: 107multi-gzip twice on a trimmed copy,
  105oneshot-zip-only, cz-14gzopen.

No board, gate or sweep leg was run, and no tree file other than this page was
written.
