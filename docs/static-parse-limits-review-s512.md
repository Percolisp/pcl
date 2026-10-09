# What a static parse of Perl cannot know — the failures PCL will cause that we have not seen yet (review, s512, 2026-10-07)

**The USER's question (s512):** "What failures will our static parsing cause that we haven't seen yet?
If prototypes/signatures are set dynamically it fails.  What else?  Is there known discussion about this?"

**Short answer.**  Perl's parser is not a function of the text: as `perl` reads a file it *runs* every
`BEGIN` block and every `use` (a `BEGIN { require; import }`) before it parses the next line, and what
that code did — defined a sub, gave it a prototype, imported a name, switched a feature on, set a lexical
hint, installed a source filter or a keyword plugin, rewrote what a numeric literal means, changed `@INC` —
governs how the rest of the text is read.  PCL parses the whole file first (PPI + the facts its `lib/`
shims and a whole-file pre-scan supply) and runs it afterwards, so **everything the parse should learn
from running code is either supplied statically (a shim, a pre-scan), or guessed, or missed**.  The misses
fall into seven families (§2).  Two of them were measured as *new* in this review — a prototype applies to
calls above its definition (#2871), and a prototype-shaped head under the signatures feature (#2872) — and
two everyday pragmas turned out to be silent no-ops (`use autodie` #2873, `use bigint` #2874).  The
underlying fact is a theorem (Kegler 2008, §3): no static parser can be right on every program, and the
only faithful route is to run the compile-time code while parsing (Perlito's route) or to detect at run
time that the parse was wrong (#2610, PCL's planned route).

Probes: `~/pcl-agent-scratch/s512/review/static/` (`run.sh`, `run-one.sh`; one directory per probe,
`o.perl` / `o.pcl` with the exit status, perl 5.40.3 vs `./pcl` on main `3ea47a41`).  **18 valid probes:
10 identical to perl, 8 different.**

## 1. The model: what perl's parser knows that a text does not

| perl, while parsing line N | what it changes for line N+1 | how PCL learns it |
|---|---|---|
| runs `BEGIN {…}` and `use M LIST` (= `BEGIN { require M; M->import(LIST) }`) | subs that now exist, their prototypes, imported names, `*CORE::GLOBAL::` overrides, `@INC`, `%^H`, `$^H` | a whole-file pre-scan of *static* declarations (`sub f (…)`, `use constant`, `:prototype`), the `use` statement's own import LIST, the module's source read at transpile time (`_extract_module_prototypes`), the `lib/` shims' declarations |
| a sub becomes known → a bare word becomes a CALL; a prototype makes `f $x, $y` unary, `f {…} @l` a block call, `f(@a)` a scalar-context or reference-taking call; `()` makes `PI * 2` a multiplication | the parse of every later call to that name | the same pre-scan — **whole-file, not positional** (family B) |
| `use feature` / `use v5.NN` / a module's `import` calling `feature->import` | `sub f ($x)` is a signature; `$r->@*` interpolates; `try`; `class` | the pragma text (`_signatures_enabled_at`, bundles); an import is invisible unless it is spelled in the text |
| `%^H` hints (`autodie`, `bigint`, `integer`, `strict`, custom pragmas) | lexically scoped replacements of built-ins, literal rewriting, run-time checks | file-global at best (not-supported: "hints are file-global"); some pragmas have no shim at all |
| `overload::constant` | what a numeric / string literal MEANS | nothing (family C, #2874) |
| a source filter (`Filter::Util::Call`) or a keyword plugin (XS `PL_keyword_plugin`) | the TEXT itself, or new syntax | nothing; documented not supported (family D) |
| `@INC` changed by code, an `@INC` hook (coderef / object) | whether `use M` finds M, and WHICH source | `use lib` statically; a computed `@INC` is hoisted (#350); hooks are absent (#1815) |
| `eval STRING` / `do FILE` / `require` at run time | the sub table *as it is at that moment* governs the eval's own parse | the eval request carries no sub table at all (#2870), neither static nor run-time |

Everything in the right-hand column is a *fact supplied in advance*.  A fact that only running code can
produce is where the parse goes wrong, and the wrong parse is **silent** in most spellings (a different
argument count, a different context, a string where a call was meant), which is the failure mode this
project ranks worst.

## 2. The seven families, with what has been seen and what is new

Status key: **SEEN** = a task exists / a DECIDED ruling; **DOC** = `docs/not-supported.md` entry;
**NEW** = filed by this review; **OK** = probed identical to perl.

### A. Definitions that reach perl's parser only by RUNNING code
| shape | perl | PCL | status |
|---|---|---|---|
| a prototype set by code in BEGIN: `eval "sub f (\$) {…}"`, `*f = sub (\$) {…}`, `Sub::Util::set_prototype` | applies to later calls | not seen: list-op call (`($)` / `()` silent wrong, `(&@)` a loud drop) | SEEN #2610 (the run-time DETECTOR sketch), s511 probes |
| a replaced built-in, any spelling | the replacement's own arity and context | the BUILT-IN's argument count (`rand(undef, 5)` → `rand(5)`) | SEEN #2779 |
| `BEGIN { require M; M->import }` (no names in the text) for a BUILT-IN override | overrides | invisible: the built-in stays | SEEN #2053; for a PLAIN sub the paren-less call parses right (`p03-beginimport` OK) |
| `@EXPORT` built by code (`map {"fn$_"} 1..2`) | the imports exist | `p12-compexport` OK (PCL reads the module's definitions, not only its export list) | OK |
| `use constant { map {…} }` (computed NAMES) | constants | names unknown: `print X + 1` prints to filehandle X | SEEN s511 (DECIDED `## s511`) |
| a string eval's parse uses the RUN-TIME sub table: `*g = sub (\$) {7}; eval 'my @r = (g 1, 2); scalar @r'` | 2 | 1 — the eval request carries no sub table | SEEN #2870 (static half); the run-time half (`p07-evalproto`) belongs to #2610 — **note added to #2870** |
| AUTOLOAD / `can` / run-time glob assignment | no parse effect in perl either | — | OK by construction |

### B. Declaration POSITION — **NEW, #2871**
perl applies a prototype, a `()` constant or a `use subs` predeclaration only to calls parsed AFTER it
("called too early to check prototype" is the warning for the other case).  PCL's `collect_prototypes`
walks the whole document before parsing, so every call sees every prototype.

| probe | perl | PCL |
|---|---|---|
| `print f(@a); sub f ($) { $_[0] }` (sub below the call) | 10 (list context, first element) | 3 (count) |
| `print cnt(@a); sub cnt (\@) {…}` | `val:1` (the list) | `ref:3` (a reference) |
| `use subs qw(sh); my @r = (sh 1, 2); sub sh ($) {9}` | 1 element | 2 |
| `print PI * 2; use constant PI => 3;` | prints nothing (filehandle PI) | 6 |

This is the common script shape (main code at the top, prototyped helpers at the bottom) and it is
silent.  The paren-less too-early call (`f 1, 2` with no declaration above) is a perl syntax error, so
it is not a divergence.  Fix site in the task: a position on each prototype entry, consulted against
the parser's current position.

### C. Pragmas, features and hints that change the parse
| shape | perl | PCL | status |
|---|---|---|---|
| `use feature 'signatures'` / `use v5.36` / a bundle, named parameters | signature | signature | SEEN #455 (handled), `p04-sigimport` OK even when a module's import enables it |
| the same feature, a PROTOTYPE-SHAPED head `sub f ($) {…}` | a signature (one unnamed parameter; `f(@two)` dies) | a prototype (scalar context, 42) | **NEW #2872** (direct and through an import) |
| `use autodie` | built-ins replaced by dying wrappers, lexically | silent no-op (`open` of a missing file continues) | **NEW #2873** — an everyday idiom |
| `use bigint` / `bignum` / `bigrat` (`overload::constant`) | `2**100` exact | a double `1.26765060022823e+30` | **NEW #2874** |
| `use integer` | 7/2 = 3 | 3 | OK |
| `use feature 'postderef_qq'` (`"$r->@*"`) | interpolates | interpolates | OK |
| `use Time::HiRes qw(time sleep)` (a built-in displaced by an import LIST) | HiRes | HiRes | OK (#1870/#1992) |
| `BEGIN { *CORE::GLOBAL::sleep = sub {…} }` | override | override | OK (arity: #2779) |
| `%^H` custom lexical pragmas (`(caller)[10]`), `no autodie` in a block | lexically scoped | file-global | DOC (not-supported "hints are file-global") |
| `use re '/x'` (default regex flags), `use feature 'indirect'` / `no feature 'bareword_filehandles'`, `use utf8` after text has been read, `use charnames` | parse-affecting | untested in this review | open |

### D. The text itself is rewritten before perl parses it
Source filters (`Filter::Util::Call`, `Filter::Simple`, `Switch`, `Smart::Comments`, obfuscators) — DOC,
with a feasible transpile-time route on file (run the real filter under the system perl over the rest of
the file, splice, parse).  Keyword plugins and `Devel::Declare` (Object::Pad, Function::Parameters,
Syntax::Keyword::Try/Dynamically/Defer, Future::AsyncAwait, Keyword::Declare, MooseX::Declare) — DOC in
one line; PPI cannot tokenize the new syntax, so each such module is a parser-level shim (the way `try`
was added for the core feature, s405) or nothing.  `Inline::C` and friends define subs at run time
(family A, calls with parentheses are fine).

### E. Module resolution at transpile time
| shape | status |
|---|---|
| `@INC` hooks (a coderef / object in `@INC`: App::FatPacker — `cpanm` itself is fatpacked —, PAR, `Module::Runtime`-style loaders) | `p08-inchook` DIFF, a raw host type error; SEEN #1815 |
| `@INC` computed by code before a `use` (`BEGIN { unshift @INC, compute() }`) | the `require` is hoisted above it — SEEN #350; and the module's prototypes are unknown to the transpile |
| a module only present at RUN time (installed later, generated, `require`d from a sub) | its exports are unknown to the parse — paren-less calls to them are perl syntax errors anyway; with parentheses fine |

### F. Perl's own guessing, re-implemented by PPI and PCL
These are not "dynamic" — perl guesses from the text too — but two guessers disagree at the edges.
`/` regex vs divide after a word (#351, #484, #2774 — merlyn's `sin / 25 ; # / ; die`), `{` block vs
anonymous hash (#286), `print $fh -1` (#405), `-bareword` (#476), `[`/`{` in a regex (`intuit_more`,
#237), `<` glob vs less-than (fixed in PPI 1.291), the indirect object (`new Foo` works; the
scalar-invocant spelling is MAYBE LATER, DECIDED s425), `y`/`s`/`q` as hash keys and the other PPI
mis-lexes (`docs/ppi-upstream-bugs.md`).  Probed OK here: `sort bynum @l` with `bynum` defined below;
`my $x = foo;` with `sub foo` below (both give the string `foo`).

### G. What no static parser can fix
A BEGIN block whose outcome depends on run-time data (`BEGIN { *f = rand() < .5 ? sub ($) {…} : sub {…} }`)
makes the parse of the next line undecidable from the text — Kegler's theorem (§3).  Two honest routes
exist: run the compile-time code *while* parsing (what perl does; Perlito5 does it with a compile-time
interpreter and its `$Perlito5::PROTO` table), or parse statically and **detect** at run time that a
compile-phase definition contradicts what the parser assumed, then die loudly or re-transpile (#2610's
sketch).  PCL's ruling (s511, DECIDED): the detector is the plan; "ask real perl at transpile time" is
recorded in #2610 with its costs.

## 3. Known discussion of the problem
- **Jeffrey Kegler, "Perl Cannot Be Parsed: A Formal Proof"** (PerlMonks node 663393, 2008; expanded as a
  three-part series in *The Perl Review*; revisited in his "Perl and Parsing 11: Are all Perl programs
  parseable?", 2011).  Formalizes Adam Kennedy's (PPI's author's) conjecture: parsing Perl 5 reduces to the
  Halting Problem because a `BEGIN` block can define, from any computation, the sub whose prototype
  decides the parse of the next line.  Kegler's own later caveat: the framing is about *static* parsing;
  perl itself "parses" by running.  https://blogs.perl.org/users/jeffrey_kegler/2011/10/perl-and-parsing-11-are-all-perl-programs-parseable.html
- **PPI's documentation** (`PPI`, `PPI::Tokenizer`): "the purpose of PPI is not to parse Perl Code, but to
  parse Perl Documents"; perl "incrementally tokenizes, lexes AND EXECUTES at the same time"; code under
  source filters "should not be assumed to be parsable"; the `&dothis` example — whether a line is one
  call or two "might not even be in the same file … [or need] the prior execution of a BEGIN block".
  PPIx::Regexp's docs on `intuit_more`: perl's regex heuristics "are documented as being undocumented".
  https://metacpan.org/pod/PPI
- **perlsub "Prototypes"** ("called too early to check prototype" — family B), **perlop "Gory details of
  parsing quoted constructs"** and toke.c's `intuit_more` / `intuit_method` (family F), **perlfilter** and
  **perlapi `PL_keyword_plugin`** (family D), **overload "Overloaded constants"** (family C).
- **Guacamole / "Standard Perl"** (Sawyer X, 2020): a parser that accepts only a dialect in which
  prototypes cannot change the parse (`first {…} @l` is rejected in favour of `first(sub {…}, @l)`),
  plain calls need parentheses, a bareword before `->` is always a class, no string eval — the opposite
  answer to PCL's (restrict the language instead of recovering the facts).  https://metacpan.org/pod/Guacamole
- **PPR** (Damian Conway): a regex-grammar *recognizer* of Perl syntax that builds no tree and, like PPI,
  cannot know prototypes or filters.  https://metacpan.org/pod/PPR
- **Perlito5** (Flavio Glock): a Perl-to-JS/Java compiler that *executes* `use` and `BEGIN` at compile time
  in its own interpreter ("'Use' is no longer an AST node, because all 'use' statements are executed at
  compile-time"; a compile-time scratchpad; `$^H`/`%^H` support) — the faithful route, at the price of
  an interpreter inside the compiler.  https://metacpan.org/pod/Perlito5
- Randal Schwartz's "only perl can parse Perl" and brian d foy's r/perl questions (s510–s511: "how do you
  know rand takes one argument? I can redefine it"; "are you sure you can detect it? `eval
  $dont_know_what_this_is`") are the same point from the user's side; the answers are in DECIDED
  `## s510` / `## s511` and #2610.

## 4. What to do, ranked by (silent × common)
1. **#2871 — position-aware prototype lookup.**  Silent, common script shape, a contained fix (a position
   on each entry, the parser's current position at the lookup).  The sweep's `sub.t` / `proto.t` rows and
   a guard file are the bar.  Pairs naturally with #2870 (the eval's table must carry the same entries).
   **DONE s513b** (records carry the introducing statement's site; ir-spec §5.2; guard
   `Pl/t/proto-position-01.t`).  Remains: the eval's table (#2870); the `WORD /` repair still asks whole-file.
2. **#2873 — `use autodie`.**  An everyday idiom that is silently a no-op; the shim route rides the
   builtin-override registry PCL already has.  Until built, an announcement (rule 12's effect-only boundary).
   **DONE s513b** (`lib/autodie.pm` + the registry reading `@EXPORT` / `:tag` / `no M`; ir-spec §7.1a;
   guard `Pl/t/autodie-01.t`).  Remains: lexical (block) scope, the message location (#233), the socket /
   IPC / fcntl / ioctl wrappers (announced) -- not-supported "autodie".
3. **#2870 + the run-time half of family A (#2610)** — the eval's sub table, static first, then the
   detector; brian d foy's question is answered honestly only when both are there.
   **DONE s513d** -- #2870: the eval request carries the `NAME=PROTO` pairs of
   the prototyped subs visible at the eval site (own package; another package
   by the qualified name the text spells) and they join the eval cache key
   (ir-spec §9.1 piece 4; 16 of the 23 s511 probes now = perl, was 7).  #2610:
   the DETECTOR exists as a LOG-only instrument (`PCL_DETECT_TABLE` +
   `PCL_DETECT_LOG`, not-supported "A sub or prototype that only BEGIN-time
   code installs"); its first measurement is in #2610 and DECIDED `## s513d`.
   Remains: die vs announce (the USER's call, from that measurement); the
   file-level parse of a BEGIN-time install (by construction); a built-in's
   name in the eval's table (#2779); the detector's call line is the
   STATEMENT's and a method name it was asked about is recorded too.
4. **#2872, #2874** — low frequency, both silent; each is a one-session item with its guard rows.
   **DONE s513f**: #2872 (a prototype-shaped head is a signature wherever the feature is on, the pragma's own line and a module's `feature->import` included) and #2874 (`use bigint` / `use bignum` through a module's statically-read constant handlers; `use bigrat` announced).  What remains: `overload::constant` with an ANONYMOUS handler (code run while parsing, #2610), a literal in a string eval, bigrat (#3000).
5. **Keyword plugins** — per module, only when a target needs one (Object::Pad is the likely first).
   **Source filters** — the filed transpile-time route, only when a target needs it.
6. The family-C "open" row (`use re`, `indirect`, `bareword_filehandles`) — probe when touched.
