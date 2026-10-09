# The detector's first measurement (s513d, 2026-10-09): where a parse fact only running code produces would have changed a value

**Question (the USER, s510):** "can we recognize when we will fail because of that problem?" -- the class of Perl programs
where a sub, or a prototype, exists only after BEGIN-time code ran (brian d foy's `*{$name} = COND ? sub ($) {…} : sub ($$) {…}`,
merlyn's `BEGIN { eval "sub zany ();" }`, `set_prototype` in BEGIN, `BEGIN { Module->import }` with a computed list, a displaced
built-in), which a static transpile parses before anything has run.  Task #2610; the design sketch is in that task; the review
that ranked it is `docs/static-parse-limits-review-s512.md`.

**Answer (this measurement):** yes.  The compiler can record how it parsed every bareword call, the runtime knows every place a
sub gets a name and whether the unit is still in its compile phase, and the two disagree in a countable set of places.  Run over
PCL's four test populations the disagreement is **374 distinct call sites from 69 names**, and reading them by shape says that the
commonest member is harmless, two members are real, and one is instrument noise that points at a registry gap.

**Ruling (the USER, 2026-10-09 09:1x):** the detector's reaction is a FLAG -- an error message on stderr with the run continuing,
or a die.  Not scheduled yet.  The log-only instrument below stays as built.

## 1. The instrument (s513d, log-only, off by default)

- `PCL_DETECT_TABLE=1` at transpile time: every program or module unit (never a string eval's) carries, right after its
  `(in-package :pcl)`, one form `(p-eval-always (pcl::p-detect-table UNIT CALLS DEFS))`.  CALLS is every answer the prototype
  lookup gave at a call site: `(PKG NAME STATEMENT-LINE KIND)` with KIND in the closed set `builtin` / `proto:<text>` /
  `list-call` / `unknown` / `method`; DEFS is the unit's own definitions with their prototypes.  Without the switch the emission
  is byte-identical (an emission row in `Pl/t/detect-01.t` says so).
- `PCL_DETECT_LOG=FILE` at run time: `%p-note-sub-install`, the one hook every install site already calls (`p-sub`, the
  glob-assign CODE arms, the glob-copy CODE slot, `set_prototype`), compares -- while a table is active and the unit's compile
  phase is not over -- the installed sub's prototype with the KIND assumed at every call site of that name whose statement line
  is after the install, and appends one line per disagreement:
  `unit \t name \t install-line \t install-kind \t call-line \t assumed-kind \t installed-prototype`.
- Cost: nothing on any hot path; the comparison runs only when a sub is installed during a compile phase and a table exists.
- Guard `Pl/t/detect-01.t` (11 rows): foy's example, merlyn's example and `set_prototype` in BEGIN log exactly one line each; a
  plain program logs none; the program's output is identical with and without the switches.

## 2. Method

Each population was transpiled and run with both switches set and a FRESH module cache (`PCL_CACHE_DIR` pointing at an empty
directory per population), so every module's table was emitted and registered:

| population | what it is | log lines | distinct (unit, name, call line) | distinct names | distinct units |
|---|---|---|---|---|---|
| gate | PCL's own regression suite, `Pl/t/` (291 files) | 185 | **53** | 18 | 8 |
| sweep | perl's own tests extracted, `perl-tests/` (108 files) | 107 | 107 | 2 | 1 |
| everyday | the 122 ordinary programs, `everyday/` | 205 | 99 | 24 | 10 |
| CPAN board | the board's distributions' `t/` files | 1,308 | 115 | 25 | 18 |

Lines exceed distinct sites because a module's FASL build registers its table at compile time and again at load.  Under the
instrument the sweep reads TOTAL 18,740 and EVERYDAY 114 of 122, both equal to main: measuring changes nothing.

Each site was then classified by the agent's `classify.pl` on the token after the name at the call line, by rule 12's VALUE
boundary: **SAFE** (the next token cannot start an argument and the installed prototype takes none the old parse would have
given differently), **RISK** (an argument follows, or a built-in was displaced), **PAREN** (`NAME(...)`: the argument list is the
same, only a slot's context can differ), **METHOD** (`->NAME`: no parse effect), **UNREAD** (the unit is gone -- the gate's test
programs are temp files -- or the line does not name it).

| population | SAFE | RISK | PAREN | METHOD | UNREAD |
|---|---|---|---|---|---|
| gate | 20 | 1 | 0 | 0 | 32 |
| sweep | 0 | 0 | 26 | 28 | 53 |
| everyday | 27 | 22 | 16 | 0 | 34 |
| board | 16 | 50 | 10 | 0 | 39 |

## 3. The five shapes, with the verdict per shape

1. **Computed-name `()` constants glob-installed in BEGIN** -- the bulk of the gate, everyday and the board: core File::Path's
   `_IS_VMS` / `_IS_MSWIN32` / `_IS_OS2` / `_IS_MACOS` (installed at its line 42 by a loop over a list of names), Sub::Quote's
   `_CAN_TRACK_BOOLEANS` / `_CAN_TRACK_NUMBERS` / `_HAVE_HEX_FLOAT` / `_HAVE_IS_UTF8`, `Moo::sification::_in_global_destruction`,
   IO::Socket::UNIX's `AF_UNIX` / `SOCK_STREAM` / `SOCK_DGRAM`, Text::Wrap's `REGEXPS_USE_BYTES`, and the gate's own fixtures
   (`_T_AAA`, `_T_BBB`, `amb`, the `LOCK_*` constants of a fixture).  The compiler assumed `unknown` (a bareword string in operator
   position) where perl has a nullary constant.  **Harmless at every readable site**: the sites are `if (_IS_VMS)`,
   `_IS_MSWIN32 ? … : …`, `return _IS_VMS;` -- a bareword string "File::Path::_IS_VMS" and the constant's value are both true or
   both false there.  A program that did `_IS_VMS + 1` would be wrong.
2. **A displaced built-in** -- REAL: core File::Copy's `BEGIN { eval q{ use Time::HiRes qw( stat utime ) } }` (everyday: 14
   `stat` + 3 `utime` sites, assumed `builtin`, installed `(;$)` / `(@)`; `copy`/`move` keep integer timestamps under PCL where
   perl keeps fractions); Sub::Uplevel's `*CORE::GLOBAL::caller = sub (;$) {…}` in BEGIN (board: 18 sites in its own t/ files);
   a gate fixture's BEGIN-imported `unlink`.
3. **A prototyped sub from a computed export list** -- REAL: Capture::Tiny's `t/lib/Cases.pm` does `use Capture::Tiny ':all'`
   where `@EXPORT_OK = keys %api` and the subs are glob-installed (`capture {…}` / `tee {…}` with `(&;@)`; board: 60 sites, 36 with a
   block argument -- the five board test files that fail today, #1509); Getopt::Long's `GetOptionsFromArray` imported into main
   (everyday, PAREN).
4. **Fcntl tag constants** in one everyday program (`use Fcntl qw(:DEFAULT :flock :seek :mode)`; `O_WRONLY | O_CREAT | O_EXCL`,
   `LOCK_EX | LOCK_NB`): the classifier says RISK because it does not list `|` as a safe follower; read by hand they are SAFE and
   the program is identical to perl.
5. **Instrument noise -- and a registry gap behind it.**  92 of the sweep's 107 sites are `Math::BigInt::modify`, recorded as
   `assumed proto:() installed none`, plus `Math::BigInt::blessed` / `Math::BigFloat::blessed` (`use Scalar::Util qw< blessed
   refaddr >`, every site `blessed(...)` = PAREN, harmless) and, on the board, Text::Balanced's `extract_*` recorded as
   `assumed proto:;$$ installed none`.  "Installed none" against a source that plainly says `sub modify () { 0; }` and
   `sub extract_delimited (;$$$$)` means the RUNTIME's prototype registry (`%pcl-sub-prototypes`, what `prototype()` reads) has
   no entry for a definition the compiler read correctly -- the same gap #2961 found for Getopt::Long.  Here the compiler is
   right and the registry is incomplete: the detector reported a disagreement, which is its job, but the fix is in the
   registration path, not in the parse.

**What this says for the die-vs-announce question:** working programs DO trip the detector -- every population loads File::Path
or Sub::Quote through Moo -- and the commonest member is harmless at every readable site, while the two real members (a displaced
built-in, a computed export list) run correctly in their own suites today except for Capture::Tiny's block-form calls.  A DIE on
every event would therefore kill File::Path, Sub::Quote and Moo users in every population; an ANNOUNCE costs nothing and names the
site.  Hence the flag.

**s513g: what moved.**  Shapes 1 and 3 got FACTS OVERLAYS (#2878, `lib/PCL/Facts/`, `shipped-modules.md` "Facts
overlays"): File::Path, Sub::Quote, Moo::_Utils (the module that installs `_in_global_destruction`), IO::Socket::UNIX,
Text::Wrap, Capture::Tiny.  Re-measured the same way (both switches, a fresh cache per population, on gen v2-5284), distinct
sites BEFORE -> AFTER: **gate 53 -> 7, sweep 107 -> 107, everyday 99 -> 46, board 115 -> 31** -- every overlaid name is GONE
from every log (none reappears as "installed differs").  Shape 4 also left (gate `LOCK_*`, everyday `O_*`/`LOCK_*`/`SEEK_SET`)
and the gate's BEGIN-imported `unlink` with it: an import list's `:tag` now expands through the module's `%EXPORT_TAGS`
(generic, not an overlay).  The residue is exactly the rest of the table: the gate's own detector fixtures (`_T_AAA`,
`_T_BBB`, `amb`), shape 2 (File::Copy's `stat`/`utime`, `CORE::GLOBAL::caller`), shape 5 (Math::BigInt/BigFloat
`blessed`/`modify`, Text::Balanced `extract_*`) and Getopt::Long's `GetOptionsFromArray` (PAREN).  Capture::Tiny's five
#1509 files now compile their `capture { ... }` calls and stop on the next cause, #3023 (SEEK_END on a File::Temp handle).

## 4. Limits, said plainly

- The call line is the STATEMENT's line as `Pl::Environment::parse_site` publishes it; inside a fragment re-parse it is relative
  to the fragment, so some events are misplaced (above or below the install) and about 35 % of sites are UNREAD by the
  classifier (the gate's programs are also temp files, gone by the time the classifier runs).
- Method-name questions the parser asked are recorded too (class 5's `->modify` sites, METHOD, no parse effect).
- `CORE::GLOBAL` overrides of names the compiler never asked about are not seen (the table holds call sites, not names).
- The sweep population is perl's test files, whose only two names are both Math::BigInt's: perl's own tests rarely install subs
  at BEGIN time by computation.

## 5. Where the data is

The raw logs (`detect-{gate,sweep,everyday,board}.log`, the first-run `detect1-*.log`, `classify.pl`, `m2610.txt`) are in the
s513d agent's scratch (`.claude/worktrees/agent-a32d9284777ba37b9/scratch/s513d/`, not under version control); the deduplicated
per-site tables and the classifier's output are in `~/pcl-agent-scratch/s513/review/detector-report/`.  The appendix below is
the durable copy: the gate's 53 sites in full, and the per-name tables of the other three populations.

## Appendix A -- the gate's 53 distinct call sites

`unit  name  install:line/kind  call:line  assumed  installed`  (CORE: = perl's own lib; /tmp/*.pl = a gate test's program)

```
~/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/Moo/sification.pm	Moo::sification::_in_global_destruction	install:8/glob	call:13	assumed:unknown	installed:()
~/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/Moo/sification.pm	Moo::sification::_in_global_destruction	install:8/glob	call:17	assumed:unknown	installed:()
~/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/Sub/Quote.pm	Sub::Quote::_CAN_TRACK_BOOLEANS	install:85/glob	call:156	assumed:unknown	installed:()
~/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/Sub/Quote.pm	Sub::Quote::_CAN_TRACK_BOOLEANS	install:85/glob	call:95	assumed:unknown	installed:()
~/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/Sub/Quote.pm	Sub::Quote::_CAN_TRACK_BOOLEANS	install:85/glob	call:99	assumed:unknown	installed:()
~/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/Sub/Quote.pm	Sub::Quote::_CAN_TRACK_NUMBERS	install:85/glob	call:156	assumed:unknown	installed:()
~/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/Sub/Quote.pm	Sub::Quote::_CAN_TRACK_NUMBERS	install:85/glob	call:95	assumed:unknown	installed:()
~/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/Sub/Quote.pm	Sub::Quote::_CAN_TRACK_NUMBERS	install:85/glob	call:99	assumed:unknown	installed:()
~/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/Sub/Quote.pm	Sub::Quote::_HAVE_HEX_FLOAT	install:85/glob	call:151	assumed:unknown	installed:()
~/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/Sub/Quote.pm	Sub::Quote::_HAVE_HEX_FLOAT	install:85/glob	call:99	assumed:unknown	installed:()
~/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/Sub/Quote.pm	Sub::Quote::_HAVE_IS_UTF8	install:85/glob	call:156	assumed:unknown	installed:()
~/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/Sub/Quote.pm	Sub::Quote::_HAVE_IS_UTF8	install:85/glob	call:99	assumed:unknown	installed:()
CORE:File/Path.pm	File::Path::_IS_MACOS	install:42/glob	call:267	assumed:unknown	installed:()
CORE:File/Path.pm	File::Path::_IS_MACOS	install:42/glob	call:353	assumed:unknown	installed:()
CORE:File/Path.pm	File::Path::_IS_MSWIN32	install:42/glob	call:127	assumed:unknown	installed:()
CORE:File/Path.pm	File::Path::_IS_MSWIN32	install:42/glob	call:267	assumed:unknown	installed:()
CORE:File/Path.pm	File::Path::_IS_MSWIN32	install:42/glob	call:337	assumed:unknown	installed:()
CORE:File/Path.pm	File::Path::_IS_MSWIN32	install:42/glob	call:338	assumed:unknown	installed:()
CORE:File/Path.pm	File::Path::_IS_MSWIN32	install:42/glob	call:356	assumed:unknown	installed:()
CORE:File/Path.pm	File::Path::_IS_MSWIN32	install:42/glob	call:82	assumed:unknown	installed:()
CORE:File/Path.pm	File::Path::_IS_OS2	install:42/glob	call:173	assumed:unknown	installed:()
CORE:File/Path.pm	File::Path::_IS_OS2	install:42/glob	call:183	assumed:unknown	installed:()
CORE:File/Path.pm	File::Path::_IS_VMS	install:42/glob	call:173	assumed:unknown	installed:()
CORE:File/Path.pm	File::Path::_IS_VMS	install:42/glob	call:186	assumed:unknown	installed:()
CORE:File/Path.pm	File::Path::_IS_VMS	install:42/glob	call:267	assumed:unknown	installed:()
CORE:File/Path.pm	File::Path::_IS_VMS	install:42/glob	call:341	assumed:unknown	installed:()
CORE:File/Path.pm	File::Path::_IS_VMS	install:42/glob	call:366	assumed:unknown	installed:()
CORE:File/Path.pm	File::Path::_IS_VMS	install:42/glob	call:396	assumed:unknown	installed:()
CORE:File/Path.pm	File::Path::_IS_VMS	install:42/glob	call:484	assumed:unknown	installed:()
CORE:File/Path.pm	File::Path::_IS_VMS	install:42/glob	call:532	assumed:unknown	installed:()
CORE:File/Path.pm	File::Path::_IS_VMS	install:42/glob	call:550	assumed:unknown	installed:()
CORE:File/Path.pm	File::Path::_IS_VMS	install:42/glob	call:569	assumed:unknown	installed:()
CORE:File/Path.pm	File::Path::_IS_VMS	install:42/glob	call:574	assumed:unknown	installed:()
CORE:File/Path.pm	File::Path::_IS_VMS	install:42/glob	call:610	assumed:unknown	installed:()
CORE:Text/Wrap.pm	Text::Wrap::REGEXPS_USE_BYTES	install:15/sub	call:27	assumed:unknown	installed:()
CORE:Text/Wrap.pm	Text::Wrap::REGEXPS_USE_BYTES	install:15/sub	call:97	assumed:unknown	installed:()
/tmp/5hlKtIPOor.pl	main::unlink	install:8/glob	call:9	assumed:builtin	installed:(@)
/tmp/CLERn8dwMf.pl	main::LOCK_EX	install:2/glob	call:13	assumed:unknown	installed:()
/tmp/CLERn8dwMf.pl	main::LOCK_EX	install:2/glob	call:20	assumed:unknown	installed:()
/tmp/CLERn8dwMf.pl	main::LOCK_EX	install:2/glob	call:6	assumed:unknown	installed:()
/tmp/CLERn8dwMf.pl	main::LOCK_NB	install:2/glob	call:13	assumed:unknown	installed:()
/tmp/CLERn8dwMf.pl	main::LOCK_NB	install:2/glob	call:6	assumed:unknown	installed:()
/tmp/CLERn8dwMf.pl	main::LOCK_SH	install:2/glob	call:22	assumed:unknown	installed:()
/tmp/CLERn8dwMf.pl	main::LOCK_SH	install:2/glob	call:6	assumed:unknown	installed:()
/tmp/CLERn8dwMf.pl	main::LOCK_UN	install:2/glob	call:18	assumed:unknown	installed:()
/tmp/CLERn8dwMf.pl	main::LOCK_UN	install:2/glob	call:6	assumed:unknown	installed:()
/tmp/KHavK8RgtN.pl	main::amb	install:1/glob	call:2	assumed:unknown	installed:()
/tmp/McczvrVK52.pl	main::_T_AAA	install:4/glob	call:5	assumed:unknown	installed:()
/tmp/McczvrVK52.pl	main::_T_AAA	install:4/glob	call:6	assumed:unknown	installed:()
/tmp/McczvrVK52.pl	main::_T_AAA	install:4/glob	call:7	assumed:unknown	installed:()
/tmp/McczvrVK52.pl	main::_T_BBB	install:4/glob	call:10	assumed:unknown	installed:()
/tmp/McczvrVK52.pl	main::_T_BBB	install:4/glob	call:5	assumed:unknown	installed:()
/tmp/McczvrVK52.pl	main::_T_BBB	install:4/glob	call:7	assumed:unknown	installed:()
```

## Appendix B -- the gate by name

```
 12  File::Path::_IS_VMS                        install:42/glob assumed:unknown installed:() CORE:File/Path.pm
  6  File::Path::_IS_MSWIN32                    install:42/glob assumed:unknown installed:() CORE:File/Path.pm
  3  Sub::Quote::_CAN_TRACK_BOOLEANS            install:85/glob assumed:unknown installed:() ~/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/Sub/Quote.pm
  3  Sub::Quote::_CAN_TRACK_NUMBERS             install:85/glob assumed:unknown installed:() ~/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/Sub/Quote.pm
  3  main::LOCK_EX                              install:2/glob assumed:unknown installed:() /tmp/CLERn8dwMf.pl
  3  main::_T_AAA                               install:4/glob assumed:unknown installed:() /tmp/McczvrVK52.pl
  3  main::_T_BBB                               install:4/glob assumed:unknown installed:() /tmp/McczvrVK52.pl
  2  File::Path::_IS_MACOS                      install:42/glob assumed:unknown installed:() CORE:File/Path.pm
  2  File::Path::_IS_OS2                        install:42/glob assumed:unknown installed:() CORE:File/Path.pm
  2  Moo::sification::_in_global_destruction    install:8/glob assumed:unknown installed:() ~/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/Moo/sification.pm
  2  Sub::Quote::_HAVE_HEX_FLOAT                install:85/glob assumed:unknown installed:() ~/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/Sub/Quote.pm
  2  Sub::Quote::_HAVE_IS_UTF8                  install:85/glob assumed:unknown installed:() ~/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/Sub/Quote.pm
  2  Text::Wrap::REGEXPS_USE_BYTES              install:15/sub assumed:unknown installed:() CORE:Text/Wrap.pm
  2  main::LOCK_NB                              install:2/glob assumed:unknown installed:() /tmp/CLERn8dwMf.pl
  2  main::LOCK_SH                              install:2/glob assumed:unknown installed:() /tmp/CLERn8dwMf.pl
  2  main::LOCK_UN                              install:2/glob assumed:unknown installed:() /tmp/CLERn8dwMf.pl
  1  main::amb                                  install:1/glob assumed:unknown installed:() /tmp/KHavK8RgtN.pl
  1  main::unlink                               install:8/glob assumed:builtin installed:(@) /tmp/5hlKtIPOor.pl
```

## Appendix C -- the sweep, everyday and the CPAN board, by name (sites, install, assumed, installed, units)

```
=== sweep: per name (sites, install, assumed, installed, units)
 92  Math::BigInt::modify                       install:270/sub assumed:proto: installed:none core Math/BigInt.pm
 15  Math::BigInt::blessed                      install:25/glob assumed:unknown installed:($) core Math/BigInt.pm
=== everyday: per name (sites, install, assumed, installed, units)
 15  Math::BigInt::blessed                      install:25/glob assumed:unknown installed:($) core Math/BigInt.pm
 14  File::Copy::stat                           install:19/glob assumed:builtin installed:(;$) core File/Copy.pm
 13  Math::BigFloat::blessed                    install:21/glob assumed:unknown installed:($) core Math/BigFloat.pm
 12  File::Path::_IS_VMS                        install:42/glob assumed:unknown installed:() core File/Path.pm
  6  File::Path::_IS_MSWIN32                    install:42/glob assumed:unknown installed:() core File/Path.pm
  4  IO::Socket::UNIX::AF_UNIX                  install:10/glob assumed:unknown installed:() core x86_64-linux/IO/Socket/UNIX.pm
  3  File::Copy::utime                          install:19/glob assumed:builtin installed:(@) core File/Copy.pm
  3  Sub::Quote::_CAN_TRACK_BOOLEANS            install:85/glob assumed:unknown installed:() ~/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/Sub/Quote.pm
  3  Sub::Quote::_CAN_TRACK_NUMBERS             install:85/glob assumed:unknown installed:() ~/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/Sub/Quote.pm
  2  File::Path::_IS_MACOS                      install:42/glob assumed:unknown installed:() core File/Path.pm
  2  File::Path::_IS_OS2                        install:42/glob assumed:unknown installed:() core File/Path.pm
  2  IO::Socket::UNIX::SOCK_DGRAM               install:10/glob assumed:unknown installed:() core x86_64-linux/IO/Socket/UNIX.pm
  2  IO::Socket::UNIX::SOCK_STREAM              install:10/glob assumed:unknown installed:() core x86_64-linux/IO/Socket/UNIX.pm
  2  Moo::sification::_in_global_destruction    install:8/glob assumed:unknown installed:() ~/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/Moo/sification.pm
  2  Sub::Quote::_HAVE_HEX_FLOAT                install:85/glob assumed:unknown installed:() ~/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/Sub/Quote.pm
  2  Sub::Quote::_HAVE_IS_UTF8                  install:85/glob assumed:unknown installed:() ~/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/Sub/Quote.pm
  2  Text::Wrap::REGEXPS_USE_BYTES              install:15/sub assumed:unknown installed:() core Text/Wrap.pm
  2  main::O_CREAT                              install:7/glob assumed:unknown installed:() everyday/modules/Fcntl-Errno.pl
  2  main::O_EXCL                               install:7/glob assumed:unknown installed:() everyday/modules/Fcntl-Errno.pl
  2  main::O_WRONLY                             install:7/glob assumed:unknown installed:() everyday/modules/Fcntl-Errno.pl
  1  main::GetOptionsFromArray                  install:7/glob assumed:proto:@ installed:none everyday/modules/Getopt-Long.pl
  1  main::LOCK_EX                              install:7/glob assumed:unknown installed:() everyday/modules/Fcntl-Errno.pl
  1  main::LOCK_NB                              install:7/glob assumed:unknown installed:() everyday/modules/Fcntl-Errno.pl
  1  main::SEEK_SET                             install:7/glob assumed:unknown installed:() everyday/modules/Fcntl-Errno.pl
=== board: per name (sites, install, assumed, installed, units)
 18  CORE::GLOBAL::caller                       install:11/glob assumed:builtin installed:(;$) Sub-Uplevel-0.2800-0/t/02_uplevel.t Sub-Uplevel-0.2800-0/t/04_honor_later_override.t Sub-Uplevel-0.2800-0/t/05_honor_prior_override.t Sub-Uplevel-0.2800-0/
 18  Cases::capture                             install:5/glob assumed:unknown installed:(&;@) Capture-Tiny-0.50-0/t/lib/Cases.pm
 13  main::capture                              install:14/glob assumed:unknown installed:(&;@) Capture-Tiny-0.50-0/t/09-preserve-exit-code.t Capture-Tiny-0.50-0/t/16-catch-errors.t Capture-Tiny-0.50-0/t/17-pass-results.t Capture-Tiny-0.50-0/t/18-cus
 12  File::Path::_IS_VMS                        install:42/glob assumed:unknown installed:() core File/Path.pm
  6  File::Path::_IS_MSWIN32                    install:42/glob assumed:unknown installed:() core File/Path.pm
  5  Cases::capture_stderr                      install:5/glob assumed:unknown installed:(&;@) Capture-Tiny-0.50-0/t/lib/Cases.pm
  4  Cases::tee                                 install:5/glob assumed:unknown installed:(&;@) Capture-Tiny-0.50-0/t/lib/Cases.pm
  4  IO::Socket::UNIX::AF_UNIX                  install:10/glob assumed:unknown installed:() core x86_64-linux/IO/Socket/UNIX.pm
  4  main::extract_delimited                    install:6/glob assumed:proto:;$$$$ installed:none Text-Balanced-2.07-0/t/04_extdel.t
  3  main::extract_quotelike                    install:6/glob assumed:proto:;$$ installed:none Text-Balanced-2.07-0/t/06_extqlk.t
  2  Cases::capture_merged                      install:5/glob assumed:unknown installed:(&;@) Capture-Tiny-0.50-0/t/lib/Cases.pm
  2  Cases::capture_stdout                      install:5/glob assumed:unknown installed:(&;@) Capture-Tiny-0.50-0/t/lib/Cases.pm
  2  Cases::tee_merged                          install:5/glob assumed:unknown installed:(&;@) Capture-Tiny-0.50-0/t/lib/Cases.pm
  2  Cases::tee_stderr                          install:5/glob assumed:unknown installed:(&;@) Capture-Tiny-0.50-0/t/lib/Cases.pm
  2  Cases::tee_stdout                          install:5/glob assumed:unknown installed:(&;@) Capture-Tiny-0.50-0/t/lib/Cases.pm
  2  File::Path::_IS_MACOS                      install:42/glob assumed:unknown installed:() core File/Path.pm
  2  File::Path::_IS_OS2                        install:42/glob assumed:unknown installed:() core File/Path.pm
  2  IO::Socket::UNIX::SOCK_DGRAM               install:10/glob assumed:unknown installed:() core x86_64-linux/IO/Socket/UNIX.pm
  2  IO::Socket::UNIX::SOCK_STREAM              install:10/glob assumed:unknown installed:() core x86_64-linux/IO/Socket/UNIX.pm
  2  main::extract_codeblock                    install:6/glob assumed:proto:;$$$$$ installed:none Text-Balanced-2.07-0/t/03_extcbk.t
  2  main::extract_multiple                     install:6/glob assumed:proto:;$$$$ installed:none Text-Balanced-2.07-0/t/04_extdel.t
  2  main::extract_variable                     install:6/glob assumed:proto:;$$ installed:none Text-Balanced-2.07-0/t/08_extvar.t
  2  main::tee                                  install:12/glob assumed:unknown installed:(&;@) Capture-Tiny-0.50-0/t/16-catch-errors.t
  1  main::capture_merged                       install:13/glob assumed:unknown installed:(&;@) Capture-Tiny-0.50-0/t/17-pass-results.t
  1  main::capture_stdout                       install:13/glob assumed:unknown installed:(&;@) Capture-Tiny-0.50-0/t/17-pass-results.t
```

## Appendix D -- the classifier's verdict per shape and population (classify.pl)

```
== board: 115 events; PAREN 10, RISK 50, SAFE 16, UNREAD 39
  36	proto-glob	RISK	Cases::capture Cases::capture_merged Cases::capture_stderr Cases::capture_stdout Cases::tee Cases::tee_merged Cases::tee_stderr Cases::tee_stdout (+4)
  24	proto-glob	UNREAD	Cases::capture Cases::capture_merged Cases::capture_stderr Cases::capture_stdout Cases::tee Cases::tee_merged Cases::tee_stderr Cases::tee_stdout (+6)
  15	const-glob	UNREAD	File::Path::_IS_MACOS File::Path::_IS_MSWIN32 File::Path::_IS_OS2 File::Path::_IS_VMS IO::Socket::UNIX::AF_UNIX IO::Socket::UNIX::SOCK_DGRAM IO::Socket::UNIX::SOCK_STREAM
  15	const-glob	SAFE	File::Path::_IS_MSWIN32 File::Path::_IS_OS2 File::Path::_IS_VMS IO::Socket::UNIX::AF_UNIX IO::Socket::UNIX::SOCK_STREAM
  14	displaced-builtin	RISK	CORE::GLOBAL::caller
  10	proto-glob	PAREN	CORE::GLOBAL::caller main::extract_codeblock main::extract_delimited main::extract_quotelike main::extract_variable
  1	proto-glob	SAFE	CORE::GLOBAL::caller
== everyday: 99 events; PAREN 16, RISK 22, SAFE 27, UNREAD 34
  26	const-glob	SAFE	File::Path::_IS_MSWIN32 File::Path::_IS_OS2 File::Path::_IS_VMS IO::Socket::UNIX::AF_UNIX IO::Socket::UNIX::SOCK_STREAM Moo::sification::_in_global_destruction Sub::Quote::_CAN_TRACK_BOOLEANS Sub::Quote::_CAN_TRACK_NUMBERS (+5)
  20	const-glob	UNREAD	File::Path::_IS_MACOS File::Path::_IS_MSWIN32 File::Path::_IS_OS2 File::Path::_IS_VMS IO::Socket::UNIX::AF_UNIX IO::Socket::UNIX::SOCK_DGRAM IO::Socket::UNIX::SOCK_STREAM Moo::sification::_in_global_destruction (+4)
  17	displaced-builtin	RISK	File::Copy::stat File::Copy::utime
  16	proto-glob	PAREN	Math::BigFloat::blessed Math::BigInt::blessed main::GetOptionsFromArray
  13	proto-glob	UNREAD	Math::BigFloat::blessed Math::BigInt::blessed
  5	const-glob	RISK	main::LOCK_EX main::O_CREAT main::O_WRONLY
  1	const-sub	UNREAD	Text::Wrap::REGEXPS_USE_BYTES
  1	const-sub	SAFE	Text::Wrap::REGEXPS_USE_BYTES
== gate: 53 events; RISK 1, SAFE 20, UNREAD 32
  31	const-glob	UNREAD	File::Path::_IS_MACOS File::Path::_IS_MSWIN32 File::Path::_IS_OS2 File::Path::_IS_VMS Moo::sification::_in_global_destruction Sub::Quote::_CAN_TRACK_BOOLEANS Sub::Quote::_CAN_TRACK_NUMBERS Sub::Quote::_HAVE_HEX_FLOAT (+8)
  19	const-glob	SAFE	File::Path::_IS_MSWIN32 File::Path::_IS_OS2 File::Path::_IS_VMS Moo::sification::_in_global_destruction Sub::Quote::_CAN_TRACK_BOOLEANS Sub::Quote::_CAN_TRACK_NUMBERS Sub::Quote::_HAVE_HEX_FLOAT Sub::Quote::_HAVE_IS_UTF8
  1	const-sub	SAFE	Text::Wrap::REGEXPS_USE_BYTES
  1	const-sub	UNREAD	Text::Wrap::REGEXPS_USE_BYTES
  1	displaced-builtin	RISK	main::unlink
== sweep: 107 events; METHOD 28, PAREN 26, UNREAD 53
  46	proto-sub	UNREAD	Math::BigInt::modify
  28	proto-sub	METHOD	Math::BigInt::modify
  18	proto-sub	PAREN	Math::BigInt::modify
  8	proto-glob	PAREN	Math::BigInt::blessed
  7	proto-glob	UNREAD	Math::BigInt::blessed
```
