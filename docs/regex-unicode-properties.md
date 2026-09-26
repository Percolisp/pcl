# Unicode properties in patterns — `\p{…}`, `\pX`, `\P{…}` (s496a, task #2060)

## The problem

cl-ppcre reads `\p{NAME}` only when its special `*property-resolver*` holds a
function from a NAME to a character test.  PCL never set it, so every property
in every pattern reached the engine as the letter `p` and **silently never
matched** — `"\x{301}" =~ /\pM/` was false, `"abc" =~ /\p{L}/` was false.
The most visible casualty was core `Text::Wrap` 5.40.3, whose main loop is
`/\G((?>(?!\n)\PM\pM*|(?<![^\n])\pM+){0,$ll})($break|\n+|\z)/xmgc`: every
`wrap()` / `fill()` died "This shouldn't happen".  (An older Text::Wrap used
`\X`; that is why #2050 looked at `\X` first.)

## The design as shipped

**The data is perl's.**  `tools/rebuild-uniprops` runs under the oracle perl
and asks `Unicode::UCD` for everything: `prop_invlist(SPELLING)` gives the
exact inversion list (the sorted code points at which membership flips) for
any spelling perl accepts.  It writes `cl/pcl-uniprops.lisp`, a checked-in
artifact:

* line 1: `;;; pcl-uniprops unicode=15.0.0 perl=5.40.3 tool=tools/rebuild-uniprops`
  — **no `gen=`**: it is perl's data, not compiler output, so
  `Pl/t/artifact-staleness-01.t` (which discovers artifacts by
  `^;;;\s*pcl:\s*pipeline=… gen=`) does not adopt it;
* one function `%pcl-uniprop-data` answering four values — UNICODE, KEYS,
  LISTS, CASELESS (the runtime's `%pcl-uniprops-install` builds the hashes): KEYS maps a
  normalized spelling to `2*LIST-INDEX + NEGATED` (a list and its complement
  are stored once), LISTS are the inversion lists, CASELESS is the /i map
  (below).  9,000 keys, 717 lists, 82,950 integers, ~706 KB of text.

The generated set: every value of General_Category, Script,
Script_Extensions, Block, Age and Present_In under every alias perl lists for
the property and the value; every bare name in perl's own regex keyword table
(`unicore/uni_keywords.pl`: the binary properties, the POSIX / XPosix / Perl
classes, bare scripts and blocks) plus their `=Y/=N` forms; the `Is` prefix of
all of these; and every census spelling perl accepts.  **Nothing is derived
by hand** (DO-NOT-RETRY: mapping Word/Alpha/Space/Punct/… onto `sb-unicode`
predicates — each is a perl-specific formula).  perl's internal `_Perl_*`
properties have no prop_invlist answer; their lists are scanned from perl's own
regex engine at every code point (t/uni/variables.t asks `\p{_Perl_IDStart}`
of every code point: TIMEOUT 1,300 ok → 20,750 ok).  Every stored spelling is one
perl accepts in a match (`"a" =~ /\p{S}/` — a match, not a `qr`, because an
unknown `Is…` name is only looked up then), and each distinct list is
spot-checked against perl's regex engine.  The tool is deterministic:
`Pl/t/uniprops-01.t` regenerates into a temp file and compares bytes, and
cross-checks the 38 General_Category lists against SBCL's own
`sb-unicode:general-category` at all 1,114,112 code points (zero
disagreements — perl 5.40.3 and SBCL 2.6.0 both carry Unicode 15.0).

**Loaded lazily.**  `%pcl-uniprop-data` is a self-loading stub in the runtime
(`%pcl-def-ext-stub`, the one mechanism every extension entry uses), called
at the first property a program compiles (FASL-cached like `pack`, #1202:
0.004 s on a hit), so a program without `\p` never pays for it.

**The resolver** (`%pcl-property-resolver` in `cl/pcl-runtime.lisp`) is
installed once, globally: `(setf cl-ppcre:*property-resolver*
'%pcl-property-resolver)`.  In perl `\p` is never the letter p, and the
runtime's own internal cl-ppcre patterns never spell it.  It runs when a
pattern COMPILES — once per distinct pattern, scanners being memoized — never
per match.  For NAME it: complements on a leading `^`; asks for a
user-defined property first (below); normalizes NAME and looks it up; under
/i swaps in the caseless equivalent; returns a closure doing a binary search
over the list (`%pcl-invlist-member-p`, ~11 steps).  **It never returns
NIL**: cl-ppcre would store it and fail at MATCH time with "The function
COMMON-LISP:NIL is undefined".

**The pre-rewrite** lives in the ONE forward escape scan,
`%pcl-expand-hv-escapes` (its pre-test `%pcl-has-hv-escape` gained `p P`):

| pattern | outside a class | inside a class |
|---|---|---|
| `\pX` | `[\p{X}]` | `\p{X}` |
| `\p{X}` | `[\p{X}]` | `\p{X}` |
| `\PX` / `\P{X}` | `[^\p{X}]` | `\P{X}` |
| `\p{^X}` / `\P{^X}` | as `\P{X}` / `\p{X}` | as `\P{X}` / `\p{X}` |

cl-ppcre demands the braces (`\pM` bare is its syntax error once a resolver
is set).  A property OUTSIDE a class becomes a class of its own because
cl-ppcre case-folds only inside a class, and because perl under /i folds
FIRST and complements AFTER: `"a" =~ /\P{Lu}/i` is false in perl, and
cl-ppcre's own `:inverted-property` would complement first and answer true.
A `\Q…\E`-quoted `\p` was already turned into `\\p` by the `\Q` pass, so the
scan copies it as an escaped backslash plus the letter.

## Loose matching (`norm` in the tool = `%pcl-uniprop-normalize` in the runtime)

Lowercase; drop whitespace, `-` and `_`; the first `:` means `=`; a run of
`&`/`_` after a lone `L` (with or without `Is`) is `l_` — `L&` and `L_` are
Cased_Letter, and plain stripping would make them `L`; a numeric value is
canonical (leading zeros and a `.0` fraction dropped: `Age=011` = `Age=11.0`
= `Age=11`; `Age=V1_1` squeezes to `v11`, which perl also reads as 1.1).  The
two normalizers are compared over the census by `Pl/t/uniprops-01.t`, and the
tool dies if two spellings normalize to one key with different meanings.

Probed against perl (all identical, `Pl/t/uniprops-01.t` rows 6–7):
`\pM` ≡ `\p{M}` ≡ `\p{Mark}` ≡ `\p{ mark }` ≡ `\p{Is_Mark}` ≡ `\p{gc:Mn}` ≡
`\p{General_Category=Nonspacing_Mark}` on U+0301; `\p{^M}` ≡ `\P{M}`,
`\P{^M}` double negation; `\p{Alpha}` ⊋ `\p{L}` (U+2160); `\p{Digit}` = Nd,
`\p{PosixDigit}` ASCII; `\p{Space}` has U+00A0; `\p{Punct}` on `$` 0 but
`\p{XPosixPunct}` 1; `\p{Print}` excludes TAB; `\p{L&}` `\p{L_}` `\p{LC}`;
`\p{Latin}` bare is **Script_Extensions** (94 boundaries) while
`\p{sc=Latn}` is Script (78); `\p{InBasicLatin}`, `\p{Block=Basic_Latin}`,
`\p{Latin1}`; `\p{Age=1.1}` `\p{In=1.1}` `\p{IsAge=1.1}` `\p{Present_In=…}`;
`\p{Alpha=N}`; breaking cases `\Q\pM\E` literal, `[\p{L}-]`, `\p{L}{2}`,
`(?i)`, `(?-i:…)` inside /i, `/x` with spaces in the braces, a `qr//`
interpolated into a bigger pattern, `split /\p{Z}+/`, `s/\pM//g`, `tr/\\p//`
(not a property — tr has no such escape), `\h` + `\p` + a POSIX class in one
pattern.

Known deviation (principle 9, valid input only): perl REJECTS a lowercase
`is` before a `name=value` form (`\p{isgc=punct}`; `\p{Isgc=Punct}` and
`\p{isword}` are fine); PCL lowercases first and accepts it.

## /i: caseless equivalents

Under /i perl answers a few properties not by folding their own set but from
a caseless equivalent (`%Unicode::UCD::caseless_equivalent`): `\p{Lu}`/i is
`\p{LC}`, so `"a" =~ /[\P{Lu}]/i` is FALSE (the class is `\P{LC}`), which
folding `\P{Lu}` would answer true.  Which of OUR keys that applies to is
MEASURED by the tool, not assumed: every key whose set is one of those
properties' sets is matched /i by perl over ~1,300 sample code points, and
the sample picks the one target (`gc=LC`, `Cased`, `Cased=N`, `PosixAlpha`)
that explains every answer — or leaves the key unmapped when plain folding
already does.  The measurement found what the hash does not say: `\p{Lt}`/i
behaves as `\p{Cased}` (it shares its set with `Title`), not as `\p{LC}`.

## What dies, and with which text

All at the pattern's compile, through the ONE compile-error path
(`%pcl-regex-compile-die`, task #2372), trappable by `eval`:

| spelling | text (perl's words; PCL appends ` in regex; marked by <-- HERE in m/… <-- HERE /`) |
|---|---|
| `\p{NoSuchProp}`, `\p{ea=W}` (not generated) | `Can't find Unicode property definition "NoSuchProp"` |
| `\p{}` | `Empty \p{}` |
| `\p{IsNoSuch}` (user-shaped, no sub, no property) | `Unknown user-defined property name \p{main::IsNoSuch}` |

The ungenerated enumerated properties (`ea= lb= nv= bc= ccc= dt= gcb= wb= sb=
hst= jt= jg= InSC= InPC= nt= vo= bpt= NF*_QC …`) are listed in
`docs/not-supported.md`.  Adding one is a one-line change to `@ENUM_PROPS`.

## `\X`

`+p-grapheme-text+` is now perl's legacy approximation
`(?>\r\n|\P{M}\p{M}*|\p{M}+)` (spelled with the classes already made, because
the scan inserts it and does not re-scan it): a base character with its
combining marks.  Still not UAX #29 — a regional-indicator pair, a Hangul
L+V(+T) sequence and an emoji ZWJ sequence stay several clusters (#2381).

## User-defined properties

`\p{IsFoo}` / `\p{InFoo}` / `\p{Pkg::IsFoo}`: a name whose last component
starts with `In` or `Is` is looked up as a sub (`%p-resolve-sub-symbol`, the
current package unless qualified) BEFORE the tables — a user sub wins over a
perl property of the same spelling (`sub IsAlpha { "0030\t0039\n" }` makes
`\p{IsAlpha}` digits).  The sub is called once per pattern compile with one
argument, true under /i (read from cl-ppcre's conversion-time FLAGS), and its
lines are: `hhhh`, `hhhh<ws>hhhh` (a range), `+NAME` include, `!NAME` include
the complement, `-NAME` exclude, `&NAME` intersect (`utf8::NAME` is a perl
property, anything else another user property), `#` comments, blank lines.
The set is (ranges or includes) and no exclude and every intersect.  All
probed identical to perl (`Pl/t/uniprops-01.t` row 9).  A line that is none
of these dies naming it.

## Census

`docs/uniprops-census-s496.tsv` (`tools/rebuild-uniprops --census`): 146
distinct `\p` spellings in perl's `t/`, the core library and `perl-tests/`.
perl-accepts / answered 91 (a census spelling of an otherwise ungenerated
property -- `lb=cr`, `bc=AL` -- is stored as that one spelling, and perl's
INTERNAL `_Perl_IDStart` `_Perl_IDCont` `_Perl_Charname_Begin`
`_Perl_Charname_Continue`, which prop_invlist cannot answer, are MEASURED from
perl's regex engine at every code point); perl-accepts / dies 6 (the `name=`
wildcards and `Blk=...`); perl-rejects /
dies 48 (user-defined names the test files define themselves — answered at run
time by the user-property path, the census asks perl without the sub — and
deliberately bad spellings); perl-rejects / answered 1 (`isgc=punct`, above).

## What the compile-error DIE surfaced (#2372)

A pattern cl-ppcre could not compile used to be a stderr warning and a silent
no-match; since this batch it is a trappable die at the op.  That turned every
translation GAP into a visible failure — perl-valid patterns included — and
the companion run over the 31 property files found six.  Four were fixed in
the same mechanism that owned them (rule 11):

| gap | where it bit | fix |
|---|---|---|
| the USELESS inline flags `(?c)` `(?g)` `(?o)` | t/re/pat_advanced.t (all 1,220 rows past it) | dropped with the charset letters in `%pcl-strip-charset-flags` |
| perl 5.38's OPTIMISTIC `(*{…})` code block | t/re/pat_rt_report.t | stripped (and announced) like `(?{…})` |
| a /x `qr` whose text ends inside a `#` comment | t/re/pat_advanced.t:753 (`/($R)/`) | stringifies with perl's trailing newline |
| perl's internal `\p{_Perl_IDStart}` & co. | t/uni/variables.t (every code point) | generated by scanning perl's regex engine |

Two stay dies, because PCL cannot do what perl does and a no-match is a wrong
value: regex RECURSION `(?1)` (#2382; pat_advanced.t line 1122, C_ok
1262 → 698) and a code block used as a CONDITION `(?(?{…})…)` (#2383; pat.t,
245 → 206).  The control verbs `(*SKIP)` `(*FAIL)` `(*:NAME)` were already
ruled "left in, rejected" and now die (pat_rt_report.t line 872, 2459 → 2431);
possessive quantifiers `a++` are #2380.
