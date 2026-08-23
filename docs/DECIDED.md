# DECIDED.md — one-grep index of settled questions

*Grep THIS FILE before probing, designing, or writing anything up as an open
question.  One line per decision: keywords, the ruling, the pointer to the
authoritative text.  This file is an INDEX — never put the full argument
here.  When a session settles a question (user ruling, Fable review, design
doc), add a line in the same commit.  When a line turns out wrong, fix the
authoritative doc first, then the line.*

*Origin: s316v wasted real time re-deriving four already-settled facts
(review doc §7).  The rule now: read failing test → grep DECIDED.md → grep
not-supported.md → only then probe.*

*The per-session review records this index points at -- `docs/fable-answers-sNNN.md`
(the rulings) and `docs/opus5-review-requests-sNNN.md` (the asks) -- were REMOVED
from the tree in s440 on the USER's decision ("it doesn't seem necessary").  A
line below that cites one still names the authoritative text; read it from
history: `git show 959bf43:docs/fable-answers-s439.md` (959bf43 = the last
commit holding all 85, and the six session HANDOFF docs `docs/*handoff-s*.md`
removed with them).  The settled content lives HERE and in
`docs/session-log.md`; since s440 a review session writes its rulings into
those two files and the live plan doc directly -- no new review-doc families.*

## s443h (2026-08-24, Opus agent H) — an ANON sub's home package; `%INC` for a module PCL does not load

- **`(EXPR)->(...)`** — a `->` postfix on a PARENTHESISED expression
  dereferences ONE scalar value, in every context.  `->(` is the fourth
  member of ExprToCL's paren-scalar-base family (`->m`, `->[i]`, `->{k}` were
  already there); before #516 the group lowered to a `(vector …)` in LIST
  context and the file died at load with "Not a CODE reference".  A
  MULTI-element paren base (`(1,2,$r)->[1]`) is still wrong for all four —
  `_is_paren_scalar_base` requires exactly one child where perl's rule is the
  comma operator's LAST element (task #516's commit names it).
- **An ANONYMOUS sub's home package is the package it was COMPILED in**
  (#515, `docs/ir-spec.md` §7.1).  `p-sub` rebinds `*pcl-current-package*`
  per call for a NAMED sub; the anon lambda's wrapper now carries the same
  binding as a compile-time constant, so a closure built in `X` and called
  from `main` resolves symbolic names in `X` — and one built in `main` and
  called from inside `X` resolves in `main`.  A `map`/`grep`/`sort` BLOCK is
  NOT a sub and gets no wrapper (it keeps resolving where it runs); a
  `package Y;` inside the body still switches for the rest of the body.
- **`%INC` records every successful `use`/`require`, including the three
  classes PCL never loads** (#511, `docs/ir-spec.md` §9 head): a lexical
  pragma, an XS-only module, and one PCL supplies itself.  Codegen emits
  `(p-note-inc "strict")`; `p-use` records the other two on its no-load
  exits; the VALUE is the file that would have been loaded, resolved through
  `@INC`.  **`p-note-inc` must never be emitted for a module that might still
  have to LOAD** — the entry satisfies `p-use`'s already-loaded guard, so
  `no Moose` is deliberately not routed through it.
- **The unqualified symbolic VARIABLE resolver is the one exception to
  "reads `*pcl-current-package*`"** — `%p-symref-symbol` reads `*package*`,
  so `${"n"}` resolves in `main` where perl uses the current package, in a
  named sub as much as an anon one.  The typeglob paths already read
  `*pcl-current-package*`, so `*{"n"}` and `${"n"}` disagree today.  Found by
  the #515 probes, NOT fixed here (task filed in the s443h report).
## s443g (2026-08-24, Opus agent G) — a TOKEN REPAIR asks the same term question the compiler does, so the prototype pre-merge runs BEFORE the repairs (#484); which s/// REPLACEMENTS interpolate is the SCANNER's answer, not a private ASCII class (#492)

- **#484 CLOSED, shape (a) as ruled (task #493).**  `_premerge_include_
  prototypes` now runs TWICE — once before the `_repair_*` block, once where
  it always was.  The repairs ask "is this bareword a TERM" and an imported
  `()`-prototype sub is one (#365), but the table that carries it across a
  `use` was filled AFTER them, so `use Math::Trig; my $q = pi / 2 + pi / 4;`
  was repaired into `pi m/ 2 + pi /` and DROPPED.  **Both calls are needed**:
  the early one walks an UNREPAIRED document (a `use` swallowed by a PPI
  mis-lex is invisible to it), the late one sees the reparsed one.  Running it
  twice is free — `_extract_module_prototypes` memoizes by module name in a
  `state` cache and `add_prototype` is idempotent; the `use lib` seeding now
  skips a path already on the list, which is what makes the sub re-entrant.
  `_word_is_declared_term` reads the Environment through the ONE record test
  `Pl::Environment::proto_is_zero_arg` (rule 11 — the same one
  `PExpr::_is_zero_arg_func` asks).  Guard `Pl/t/imported-term-01.t` 7 → 11.
- **A `die`'s EXIT STATUS is `$!`** — so a pass that probes for a missing file
  changes the exit code of a refusal that happens later.  Measured: moving the
  pre-merge turned `t/op/lvref.t`'s `Parser2 TODO:` from rc 255 into rc 2
  (ENOENT) with byte-identical output and an identical message.  Contained
  with `local $!` in the pre-merge, where the expected misses are made.  The
  general fragility stands (any filesystem probe before any `die`).
- **#492 CLOSED.**  `ExprToCL::_replacement_interpolates` asked a private
  `(?<!\\)[\$\@][a-zA-Z_{]` — ASCII where the rest of the pipeline is
  Unicode-aware — so `use utf8; s/Ｘ/$ｉ/` emitted the LITERAL `$ｉ` while the
  braced `${ｉ}`, the dq string `"$ｉ"` and the PATTERN side were all right.
  **The text arrives DECODED (measured: utf8 flag on, ord 65353), so the class
  was the whole bug** — the "undecoded bytes" hypothesis in the task is KILLED.
  The gate now asks `Pl::InterpScan::scan` (standing rule §8) and keeps its
  deliberate narrowness explicitly: a BRACED spelling always takes the lambda
  path (what the `{` meant, and what keeps `${^NAME}` working), a bare MAGIC
  name never does.  Three ASCII spellings the private class could not see are
  fixed with it, each probed vs perl 5.40.3: `$::qq`, `$#arr`, and a `\\`-
  escaped sigil.  Guards `Pl/t/utf8-source-01.t` 28 → 30 + one row in
  `Pl/t/regexp-subst-01.t`.
- **Still open, all PRE-EXISTING and verified on the pre-fix tree**: the
  punctuation magics in an s/// replacement (`$&` `` $` `` `$'` `$+` `$!` `$$`)
  emit LITERALLY, and `` $` ``/`$'` are ALSO empty on the lambda path — so that
  hole needs both halves fixed at once, which is why the gate was not widened
  to them here.  `s/A/${\ "L"}/` emits an unreadable form
  (`(p-backslash (p-backslash "L\"}))`) and kills the whole file at LOAD; the
  `@{[ "L" ]}` twin is fine, so it is the `${\ …}` re-parse and the `"`.
## s443e (2026-08-24, Opus 5) — the IDENTITY promotion of a file lexical requires that the file never SPELLS the package variable of that name (#470)

- **A file lexical promoted under its OWN name is the package variable of
  that name** — that was the whole bug.  Both promotion passes
  (`_rename_spanning_lexicals`, `_promote_captured`) kept the plain name
  (`p-defcell $y`) when the bare name had exactly ONE `my`/`state` binding
  file-wide; the condition counted DECLARATIONS only, so `$main::y` / `$::y` /
  `our $y` / `"$main::y"` / `*main::y` / `$#main::a` in the same file landed
  on the same cell.  Twelve shapes measured silent-wrong against perl 5.40.3
  (a qualified READ saw the lexical, a qualified WRITE clobbered it); all
  twelve agree now.  The identity condition gained one conjunct — the file
  must not spell the package variable — computed ONCE by
  `Pl::Parser2::_scan_pkg_global_spellings` BEFORE the first rename (the span
  rename itself writes qualified `$Pkg::name` tokens, which are the
  compiler's spelling of the promoted LEXICAL, not a source-named global).
  Name-based and sigil-exact, a glob marking all three sigils; over-refusal
  costs only the `$name__file__N` mangle, which is the general path.
- **`our $x` MASKS an outer lexical, so `_ref_shadowed` asks `_stmt_binding`**
  (the resolver that already answers `'lex'` for my/state and the PACKAGE for
  an `our` alias) instead of the my/state-only `_stmt_declares_canon`.
  Without it the mangled rename rewrote the `our` scope's uses too — the same
  one-cell-for-two-variables confusion, spelled with a declaration.
- **Three spellings stay invisible and are ACCEPTED**: a symbolic reference
  (`${"main::y"}`), the qualified brace form `${main::y}` in code, and a
  qualified name inside a NON-interpolating string that is then `eval`ed.
  `docs/not-supported.md` "A SYMBOLIC spelling of a package variable does not
  demote an identity-promoted lexical (#470)" has all three with probes; the
  normative rule (when the promoted name is fresh, and what a translator may
  assume from `p-defcell $x` vs `$x__file__N`) is `docs/ir-spec.md` §2b.3's
  `$x__file__N` row.  Guards: `Pl/t/parser2-01.t` (+12 rows, inverse-guarded)
  and `Pl/t/closure-01.t` (+4, one of them the inverse).

## s440 (2026-08-23, Fable) — CI RED = a NON-CORE perl import (`Data::Dump`), not SBCL; PCL's dependencies are EXACTLY PPI ≥ 1.291 + Moo; the review-doc families removed from git (USER)

- **The first CI run's failure** (run 32648385694, `tools/install-pcl` exit 2
  in 3 s) was `Can't locate Data/Dump.pm` in the INSTALLED `pl2cl`:
  `Pl/Parser.pm`, `Pl/PExpr.pm`, `Pl/OpcodeTree.pm` imported a non-core module
  for debug dumps.  Reproduced on a BARE perl 5.38.2 + `cpanm PPI Moo`; the
  s439 suspect (sbcl.org tarball / Quicklisp visibility) was wrong — that
  setup passes.  **A sanitized HOME with the dev perl is not a stock machine.**
  Fixed: core `Data::Dumper` helpers (`Pl::PExpr::_dd`, `Pl/t/PCLDump.pm`);
  corpus-diff IDENTICAL.  Session log s440.
- **Dependency rule (standing): PCL depends on PPI ≥ 1.291 and Moo and on
  NOTHING else that is not core perl.**  Guard `Pl/t/core-deps-01.t` (loads
  compiler + runners, asserts repo/core/PPI+Moo closure; a test's `use X` of an
  installed non-core X needs the fixture guard `eval { require X; 1 }`).  A
  CPAN module used as a test FIXTURE (transpile subject / perl-oracle module:
  Data::Dump, Try::Tiny) makes its rows SKIP, naming it, where absent; CI
  installs the fixtures so the rows run.  A perl-oracle row whose program needs
  a newer perl than the host compares against a PROBED literal
  (`feature-pragma-01.t` `test_src`), never skips.
- **PPI floor = 1.291, ENFORCED by the installer** (`check_deps`; remedy
  `cpanm PPI`, which today IS 1.291).  Ubuntu 24.04's `libppi-perl` is 1.277;
  CI installs PPI with `cpanm` and asserts the version.
  `tools/t/install-pcl.t` 15 rows (a fake PPI 1.277 is refused, nothing copied).
- **CI GREEN: run 32650698636 on `7e6d1eb`** (all steps, full gate, stock runner) —
  #282 + #283 DONE; the v0.1 tag waits only on #494 (doc refresh at tag time).
- **CI diagnosability**: the job log is admin-only (API 403); `tools/ci-step`
  re-emits a failing step's tail as a `::error::` ANNOTATION (public API:
  `/check-runs/<id>/annotations`).  Every failing-capable step runs through it.
- **#496 the SHAPES corpus SHIPPED (s440b)**: `Pl/t/shapes/*.pl` (six families, one
  block-scoped package per lifted snippet, valid perl); `tools/corpus-diff.pl` prints
  the shapes verdict on ITS OWN line (fails like a corpus diff; skipped for a file
  subset); `tools/emission-ab.pl --shapes`.  Its first run found **#497** (same
  signature-sub NAME in two packages: the second's parameter becomes a package
  global — #421's family), **#498** (a punctuation ARRAY is package-qualified;
  perl forces it into main::) and **#499 FIXED** (LIST-context glob was stateful
  per pattern: full, EMPTY, full … — `p-glob--list-context` keeps no state now;
  guard glob-01.t).  **#498 FIXED s440c**: the twelve punctuation arrays are
  runtime-owned (defvar + export from :pcl, the `@-`/`@+` way) and the compiler
  declares them no more (gen v2-182; ir-spec §2b); guards punct-array-glob-01.t.  **#497 FIXED
  s440d**: a signature PARAMETER token is a declaration to BOTH resolvers
  (`_symbol_is_signature_param` asked first by `_binding_at` and `_ref_shadowed`);
  `_dd` renders tokens compactly on one line; gen v2-183; guards sig-param-shadow-01.t.
- **s441c (agent C) MERGED: #465** (`$\` and `$,` undef until set — ir-spec §8) and
  **#468** (a never-declared call reaches AUTOLOAD / perl's die via the ONE mechanism,
  re-entered at the CALL by encapsulating `sb-kernel::restart-undefined` — both other
  routes measured and rejected, DO NOT RETRY; ir-spec §5.1).  Filed #500–#504 from its
  report; agent D (s442d) on #500 + #501 + #503.
- **s441a+b (agent A) MERGED: #463 items 2 + 1** — a Cast IS a prefix operator
  (`_is_prefix_op_token`; `++`/`--` at 92 outranked the cast at 90), `$${EXPR}`'s
  braces re-blessed Subscript → Block; `${"name"} += 2` was a SILENT NO-OP
  (`%p-accessor-place-p` omitted `p-cast-$`), `p-pre--` numifies; op/universal.t
  61 → 79; filed #505–#507.  Items 3–5 = a `*`-sigil token repair (Fable measured).
- **s441b (agent B) MERGED: #466** (a Magic scalar IS a scalar in the filehandle
  slot — `_is_scalar_fh_token`, three sites) and **#464** (the statement-modifier
  split is ONE shared helper pair, four callers; `local … for` keeps dropping
  loudly by design); census recounted 73/167 at the merge; filed #508–#513;
  gate-SET scan owed (detectors widened).
- **s442d (agent D) MERGED: #500** (say appends "\n" INSTEAD of `$\` — one writer, one
  `:fh` splitter), **#501** (the dead `sort NAME` AUTOLOAD wrapper deleted; perl DOES
  reach AUTOLOAD for a comparator, via the one mechanism), **#503** (a symbolic sub
  name resolves in `*pcl-current-package*`, never the CL reader's `*package*`; one
  resolver, a third copy removed); filed #514–#517.  **Generation v2-201 on the merged
  tree.**
- **v0.1.0 TAGGED** (`29c2cf3`, 2026-08-23): CI green on the tip, gate 171/5846, sweep
  18313 (+0) clean, companion blessed (io/open.t = the ALONE value, load-sensitive,
  not a batch mover), #494 done.  **Round 4 agents**: E #470 (v2-205), F #491+#495
  (v2-210), G #485+#484+#492 (v2-215); the next rounds H–K are listed in the session
  log s440g.
- **The review-doc families are GONE from the tree** (USER s439: "remove the
  fable answers and opus reviews from git.  It doesn't seem necessary?"):
  `docs/fable-answers-*.md` (39) + `docs/opus5-review-requests-*.md` (46)
  `git rm`'d, and the six session HANDOFF docs (`docs/*handoff-s*.md`, USER
  s440) with them; citations stay as references; `git show 959bf43:docs/<file>`
  reads one.  **From s440 a review session writes its rulings into THIS
  index + the session log + the live plan doc; no new review-doc family.**

## s439c (2026-08-23, Fable, USER) — ONE checkout: `~/pcl` is the repository of record; `~/testgit/pcl` (old public history + nine dirty files = `74cf2b1`, verified byte-identical) holds nothing unique and can go; `~/pcl`'s origin is now the SSH URL that authenticates; the push recipe is `docs/plan-post-s433.md` §4a

## s439b (2026-08-23, Fable, USER) — THE RUNTIME IS KEPT COMPILED AND CACHED BY DEFAULT: every runner starts SBCL from a content-keyed saved core built on first use

- **USER ruling: "by default the CL runtime is kept compiled and cached."**
  Implemented in the ONE SBCL command builder, `tools/lib/PCLSbcl.pm`: when
  no explicit core / `PCL_TEST_CORE` / installed `<root>/pcl.core` applies, a
  runner gets `~/.pcl-cache/core/pcl-<path8>-<content12>.core`, built on
  first use (`flock`ed, tmp+rename) from `cl/pcl-runtime.lisp` and NAMED by a
  hash of the runtime's absolute path + its source + `sbcl --version` +
  `~/.sbclrc` size/mtime + a format version.  **No mtime comparison anywhere:
  an edit, an SBCL upgrade or a second checkout produces a different NAME**,
  so the stale-core failure `tools/prove-core`'s per-run rebuild was designed
  against cannot occur; older cores for the same path are pruned.  The
  extensions (pack/mro/warnings/xs) load LAZILY from `*pcl-runtime-directory*`
  and are NOT in the core — which is why the key hashes one file and why the
  PATH is in it (a core captures that directory at build time).
- **Knobs**: `PCL_NO_CORE=1` = never build/use the cached core (an installed
  core still counts — it is an install's product, not a cache); `PCL_SHOW_SBCL=1`
  prints which core; `pcl --make-core` forces the build early; `pcl
  --clear-cache` removes cores and cached modules; a FAILED build announces
  itself, falls back to source and leaves `<core>.failed` for an hour so
  parallel runners do not each pay a failing build.  The first-run progress
  line prints only when stderr is a terminal (or `PCL_SHOW_SBCL`) — a caller
  capturing a runner's stderr (the installer's smoke test was the catch) must
  not see it.  **`./pcl` now goes through PCLSbcl too** (it was a sixth runner
  hand-writing its command line); `tools/install-pcl --no-core` installs now
  fall back to the per-user cache rather than source.
- **Measured**: plain `prove -j8 Pl/t/` **224 s wall, 166/5780** (was ~5+
  min; `tools/prove-core` 233–236 s today — it stays as the fresh-image
  belt); `./pcl -E 'say 6*7'` 0.145 s end to end; full sweep on the new
  runner IDENTICAL (TOTAL 18312 +0, GATE clean) in **2:10 wall instead of
  ~10 min** — every fresh_perl child recompiled the runtime before.
  `tools/t/sbcl-prefix.t` 17 → 27 rows (the
  cached-core block builds a real core from a tiny runtime in a temp cache
  dir; the content-change row resolves in a CHILD process because a process
  memoises its answer per runtime); `tools/t/install-pcl.t` 11/11.
- Notes written: README quick start, CLAUDE.md quick reference,
  `docs/test-infrastructure.md` (the "not yet implemented" section is now the
  implemented one), `Pl/t/PCLCore.pm` + `tools/prove-core` headers.

## s439 (2026-08-23, Fable) — s438 → s438i REVIEWED + APPROVED (instruments, Q4, Q5, Q6); one review fix (#455's `qw()` spelling); #491–#496 filed; THE DISTANCE TO v0.1 IS FIVE STEPS (`docs/fable-answers-s439.md` §4)

- **All nine commits APPROVED as shipped** (`docs/fable-answers-s439.md`).
  Independently re-measured on a COLD cache: gate **166/5779** (only the 13
  pclxs xs rows); sweep **TOTAL 18312 (+0), 0 new / 0 fixed, drops 5 =
  census, child drops 241/98, GATE clean**; companion `--all --quick --jobs
  4` **528 files, ZERO real movers — the four known noise files only (io/open.t + op/utf8cache.t contention, serial matches the snapshot; uni/variables.t the known unstable TIMEOUT; io/pvbm.t 20/8 in parallel AND serial vs 23/5 alone — the EIGHTH time, not edited); both SNAPSHOT holes zero; DROPS with no +/- line**; 61 probes vs perl 5.40.3 (the Q4 operand
  matrix, the imported term in seven positions, the sub-head shapes, the
  per-character punctuation-array set, 12 glob shapes, the handle spellings).
- **REVIEW FIX — `_signatures_enabled_at` recognises every enabling
  spelling.**  It matched only a QUOTED `'signatures'`, so `use feature
  qw(signatures say); sub f ($x, @r) {…}` on ONE line still took the
  old-prototype lowering (PCL `-u|2 3-2`, perl `0-u|2-2`).  Now: the word
  `signatures` in ANY quoting after `use feature`/`use experimental`, a
  `:5.NN` bundle with NN ≥ 36, `use v5.36`+; a `no feature …` statement is
  never an enabling site.  Guard `Pl/t/sig-param-shadow-01.t` 8 → 9; corpus
  identical over 111 (no population has the same-line spelling — the row is
  the bar).
- **Filed from the probes, all PRE-EXISTING on `d0b52e9`**: **#491** the
  qualified handle NAME is never canonicalised (`print main::STDOUT` is a
  CALL; `open(Foo::H1)` with no `package Foo` is a CL READ error, whole
  file; `open(main::FH)`/`print FH` do not meet) — one fix at the
  Environment seam; **#492** the s/// REPLACEMENT side leaves a non-ASCII
  identifier literal (`s/Ｘ/$ｉ/`; `${ｉ}` and dq strings are right);
  **#493** pointer record (`-pi` → #476 widened; `(_)` default → #260; #484
  → shape (a)); **#495** ask 7's bareword-operand shapes as ONE task in
  #266's family; **#496** the SHAPES corpus (ask 10); **#494** the v0.1
  doc refresh AT TAG TIME.  **#277** corrected to completed (shipped s405c).
- **Rulings on the asks** (answers §2): child-drop sites → OWN file
  `baselines/child-drop-sites-sNNN.tsv`, gate = SITE SET only, files-reached
  reported not gated, bless AFTER #479's harness half; **`run-perl-suite.pl`
  gets `PCL_DROP_LOG`** in the same session (the companion is where
  `runperl_and_capture` has callers: `t/run/runenv*.t`); **#478 MEASURE,
  default = the `Test::`/`Test2::` name list GOES** (CLAUDE.md 9a hard stop;
  79 of 83 drops in a shipped-for population) — if the cost is real the
  mechanism is a BUDGET on the recursive walk, never a name; **#479 harness
  FIRST and ALONE** (wrong in perl too, probed; no perl-tests callers;
  removes 196 of 241 child drops), compiler gap a filler with a `Pl/t` row;
  **#365's over-import ACCEPTED AS IS** — the export-scan cross-check would
  re-break the measured Math::Complex case (standing trade: a `()`
  prototype crosses a `use` on its shape); **#484 = measure shape (a)** (a
  second, memoised post-repair pre-merge; NOT a lazy path in
  `_word_is_term`, rule 11); **#486** out of scope as a compiler item,
  cheap filler now that `_signatures_enabled_at` exists; **#485 PROMOTE**
  the outer lexical a default reads, never refuse; **#489 = accepted
  divergence for v0.1**, `not-supported.md` entry, `:site` counter fix
  queued behind the release; **runbook §4b** (a fix that makes a value
  REAL: grep for rows comparing TWO results of the changed builtin before
  the run).
- **THE DISTANCE TO v0.1 (answers §4)**: (1) USER force-pushes `main`
  (origin = the OLD history) → (2) first CI run (#283 authored; fix image
  breakage) → (3) #282 = that green run → (4) **#494** refresh README /
  STATUS / CHANGELOG on the TAGGED tree — numbers AND the drop paragraph
  ("loud but not fatal" is false since the s435 flip) → (5) tag v0.1.0.
  Nothing in Q7 is a precondition.  **#281 recommendation: items 1+2+6 +
  ir-spec (merged s415) ARE the v0.1 IR pass; items 4/5 post-v0.1.**
- **Queue**: Opus = Q7 in the ruled order (#463 item 2 first, #464 → #466 →
  #465, #468, #470), then the s439 fillers (#479+ask 2, #478 measured,
  #491, #485, #492, #484(a), #495, #496), #494 at tag time.  Fable: rule the
  asks; post-v0.1 boxed aggregates.  USER: the push.

## s438g + s438h + s438i (2026-08-23, Opus 5) — Q6: three of P5's four (#452, #451, #450); #449 stays with #418 by its own ruling

- **#452 DONE — a PACKAGE-QUALIFIED bareword filehandle is a NAME.**
  `_bareword_fh_p` tested a plain identifier, so `main::FH2` failed it and the
  token was passed through UNQUOTED: `(p-readline main::FH2)`, a bare CL
  symbol, died at LOAD ("The variable FH2 is unbound") and took the whole file.
  ONE predicate, TWO sites, both broken the same way — `<main::FH2>` and
  `print main::FH5 "x"` — while `readline(main::FH3)` was right because the
  BUILTIN path quotes the name itself.  corpus-diff: 1 of 111, `method.t:672`
  (`while (<Colour::H1>)`), exactly the shape; A/B 951 files, 1 DIFF, its
  companion twin.
- **#451 DONE — a punctuation ARRAY element interpolates**, in
  `Pl/InterpScan.pm` as standing rule §8 requires.  The `$` arm continued into
  a subscript chain for `+` and `-` only, so `"$?[1]"` printed `0[1]` — the
  scalar followed by literal text, silently.
- **THE INTERP SUBSCRIPT SET IS NOT `Pl::Parser`'s `%PUNCT_ARRAY_CHARS`, and
  the difference is the point**: `^` is in that set (it asks which characters
  can be EMITTED bare as a CL symbol, and `@^` is a legal array) but `"$^[1]"`
  does NOT subscript in perl — `$^` is the format-top-of-page name.  Probed one
  character at a time; two sets, two questions, documented at both ends.
- **After #451 NINE of the ten agree with perl in BOTH paths.  The tenth is
  `!`, wrong in the CODE path too** (`$![1]` emits
  `(p-aref (p-errno-string) 1)` — the SCALAR `$!`, indexed): pre-existing,
  filed **#487**.  Before the fix the two paths disagreed, so only the string
  half looked broken; now they agree, and agree wrongly.
- **#450 DONE — a metacharacter-free glob pattern is ITSELF**, and with it
  **perl's WORD model**: a glob pattern is a whitespace-separated LIST of
  patterns, one bsd_glob call per word.  The word model is not optional — the
  literal rule is a per-word rule, and without it `glob("a b")` would answer
  the single string "a b".  `{` counts as a metacharacter although PCL has no
  brace expansion (**#488**), because answering `{a,b}` literally is further
  from perl's `a`, `b` than the empty result.
- **A FIX THAT MAKES VALUES REAL EXPOSES WHAT WAS PASSING ON NOTHING** (the
  s435-flip pattern, now in a second place): t/op/glob.t row 18 compared
  `$output1 eq $output2` from two `eval q{ glob(q(./"TEST")) }` call sites,
  both undef, both times.  With real values it fails honestly on **#489** (the
  scalar-context glob iterator is keyed by PATTERN, not by CALL SITE, so the
  second call answers undef) and **#490** (glob does not strip csh quotes).
  Snapshot row edited by hand 14/4 → 13/5 with that cause.
- **A Pl/t EXPECTATION MAY ENCODE THE OLD BUG**, and `glob-01.t` did: "glob
  with nonexistent literal file returns empty" asserted `count:0`, the one
  answer perl never gives.  Rewritten under the s377 four-conjunct rule — the
  probe is in the comment, and the row now asserts the count AND the value,
  strictly more than before.
- **#449 NOT DONE, by its own ruling**: the CL-unsafe punctuation arrays
  (`@,` `@;` `@|` `@'` `@"` …) need a pipe-quoted emission that does not exist
  yet, and the task says to take it WITH #418's work rather than beside it.
  They keep dropping LOUDLY, which is the right failure while the emission
  rule is missing.
- Gate **166 / 5779**; sweep **TOTAL 18312 (+0), GATE clean** after each;
  companion io/ + op/ for the `cl/` change.  Guards: `Pl/t/punct-array-glob-01.t`
  13 → 21 (four for #452, two for #451, two for #450, half of them INVERSES),
  inverse-guarded on a cf0076c worktree.

## s438d + s438e + s438f (2026-08-23, Opus 5) — Q5: signature parameters are declarations (#454), the feature region includes the pragma's own line (#455), and every fragment re-parse runs the token repairs (#435)

- **#454 DONE — a SIGNATURE PARAMETER shadows a same-named file lexical.**
  `sub f ($x) {…}` desugars to `my $x = $_[0]` inside the sub, so PCL's
  "Parser2 TODO: file lexical 'x' captured by sub f" refusal was wrong for
  `($x)`, `($x = 1)` and `($x, @r)` alike.  Both scope questions now ask
  `_signature_param_canons`: the DETECTOR (`_check_sub_captures`, which scans
  the sub's BLOCK — a signature is not in it) and the REWRITER
  (`_ref_shadowed` → `_stmt_declares_canon`, which walks the Statement::Sub's
  preceding siblings — the signature is one).  **Fixing only the detector
  would have turned the refusal into a SILENT WRONG** (the body's `$x` renamed
  to the file lexical's promoted cell), which is why the pair moves together.
- **ONLY THE PARAMETER NAMES**: a default is an expression evaluated in the
  sub, so `sub f ($x = $y)` really does read the outer `$y`.  The helper splits
  on TOP-LEVEL commas and takes each part's leading symbol; the detector
  discounts only the canons the signature declares, so a file `my @x` with a
  `($x)` parameter still gates on the sub's `@x`.
- **PPI PRODUCES BOTH SHAPES FOR THE SAME SOURCE, and a reader that knows one
  is half a reader.**  With the feature in force `($x)` is a
  `PPI::Structure::Signature`; on the pragma's own line, and in a file that
  never enables it, the identical text is a `PPI::Token::Prototype`.  This cost
  #454 its first attempt and IS #455.
- **#455 DONE — the repair is the BOUNDARY, not the TEXT, and that was
  MEASURED.**  A first attempt made the flag textual (named params ⇒ signature)
  and corpus-diff caught it: `perl-tests/signatures.t:17` has
  `sub t000 ($a) { $a || "z" }` with the feature not on until line 32, and line
  20 asserts `&t000(456) == 123` — perl reads `($a)` as an old-style prototype
  there.  So a Token::Prototype is signature syntax only with named params AND
  `_signatures_enabled_at($stmt)` (an enabling pragma at an earlier line, or
  the same line at an earlier column).  Deliberately NOT a lexical scope walk:
  it recovers PPI's one missing line, and PPI owns every other position.
- **RULE 11, twice in one session.**  The textual "named params" test was in
  FOUR copies (one of them an absence) → `Pl::Parser::proto_text_has_named_params`.
  Same shape as #365's `proto_is_zero_arg` earlier the same day: **when two
  predicates answer the same question about the same record and disagree, that
  IS the bug.**
- **#435 DONE — ONE fragment document.**  `Pl::Parser::fragment_doc` builds the
  document and runs the three passes `_ppi_parse` applies after any
  serialize+reparse (`_reclassify_bare_vwords`, `_merge_unicode_symbols`,
  `_merge_punct_array_symbols`) — exactly the ones that swap token CLASSES in
  place and leave text untouched, which is what makes them safe on a fragment.
  Nine sites routed; `_normalize_signature_text` deliberately not (it
  re-serialises token TEXT, which these passes cannot change).
- **THE POPULATIONS AND THE GATE ANSWER DIFFERENT QUESTIONS** (#435's near
  miss, worth keeping): the first version called the helper fully-qualified
  from three other files, and two `Pl/t` files died at "Undefined subroutine"
  because they load `Pl::PExpr::StringInterpolation` WITHOUT `Pl::Parser`.
  Under pl2cl the call always resolves, so corpus-diff, the 951-file A/B and
  the full sweep were ALL clean while the gate lost 97 rows.  A compile-time
  `use` would be circular, so each cross-file site does a runtime `require`.
- **All three changes are emission-IDENTICAL over the four populations** —
  corpus-diff 111 files and emission-ab 951 files SAME / 0 DIFF / 0 RCDIFF,
  once per change — so the s371 rule applies again and the guards are the bar:
  `Pl/t/sig-param-shadow-01.t` (8 rows) and three rows added to
  `Pl/t/utf8-source-01.t` (25 → 28), all inverse-guarded on a worktree.
  #454 also ran the **gate-SET scan over BOTH populations (638 files each
  side): IDENTICAL** — mandatory there, because a refusal that stops firing is
  how a silent wrong gets in.  Gate **166 / 5771**; sweep **TOTAL 18312 (+0),
  GATE clean** after each.
- **Three pre-existing findings filed, each verified on a worktree**: **#485**
  (a signature DEFAULT that reads an outer lexical gets undef — the gate scans
  the block, and a default is not in the block), **#486** (an old-style
  prototype with NAMES binds them as parameters on BOTH lowering paths, so
  `sub t000 ($a)` outside a signature region answers 456 where perl answers
  123 — already blessed in `baselines/fail-baseline.tsv`), and #484 from Q4.

## s438b + s438c (2026-08-23, Opus 5) — Q4: the two operand sites become ONE (#453), and an imported `()`-prototype sub is a TERM (#365)

- **#453 DONE — `is_named_unary` answers for a DECLARED sub too.**  perl decides
  from the prototype alone: after its leading `;`s, a prototype that is exactly
  one of `$` `_` `*` `+` or one `\X`/`\[…]` group is a NAMED UNARY OPERATOR.
  `Pl::PExpr::_declared_named_unary` reads `_proto_parse_spec` (1 or [0,1] is
  exactly that set), so a user `($)`/`(;$)`/`(*)`/`(_)` sub now takes the
  named-unary operand site — which runs `_extend_high_prec` — instead of the
  strictly-single one, which stops at the first term.  ONE predicate fixes both
  halves, because it is also the strictly-single site's own guard:
  `f "a" . "b"` was f("a")."b" and is f("ab"); `f $x + 1` was 1 and is f(6);
  `g + 1, "\n"` for `(*)` was g(1,"\n") and is g(1).
- **`known_no_of_params` is deliberately NOT the second source** — its 1 covers
  Config builtins (shift, close, fileno, eof) that are NOT named unaries and
  must keep the strictly-single site, whose bareword-filehandle branch is why
  it still exists.  Config's table is asked FIRST, so a builtin's name keeps
  meaning the builtin.
- **#365 DONE, and NOT where the task pointed — the finding is the difference.**
  The task said the prototypes are known and the operator-loop term reading
  does not ask.  Measured: `_bareword_callable_here` IS asked (23 times for
  `pi`) and answers `no`, because `has_prototype('pi')` is FALSE — **the
  prototype never crossed the `use`**.  `_merge_module_prototypes` imported a
  prototype only for a block arg, a parameter SLOT (`$`/`\X`/`@`/`%`), or a
  name the export scan listed; a `()` prototype has no slots, so it rode
  entirely on that scan, which reads literal `qw()` — and Math::Complex (where
  `pi` is defined; Math::Trig re-exports it) builds `@EXPORT` from a variable
  (`my @trig = qw(pi …); our @EXPORT = (qw(…), @trig)`).  An empty prototype is
  a PARSE fact, so it now crosses a `use` on the same footing as
  `has_block_arg`, independent of the export scan.
- **`is_proto` does NOT identify the `()` shape** — `sub pi ()` arrives as
  is_proto 0 (parse_prototype_or_signature's empty case and `use constant`
  register it that way).  A first attempt keyed on `is_proto` and changed
  nothing; the record shape is min_params 0 with no params.
- **ONE predicate, `Pl::Environment::proto_is_zero_arg`** (rule 11): the record
  test lived in `PExpr::_is_zero_arg_func` and nowhere in the merge, which is
  exactly how the two disagreed.  Both ask it now.
- **A shape that occurs in NO corpus is guarded by rows, not by a sweep**
  (s371, minus-word-01.t's precedent, applied twice here): both changes are
  emission-IDENTICAL over all four populations — corpus-diff 111 files, and
  `tools/emission-ab.pl` over lib/**.pm + cpan-tests/** + perl's own t/ = **951
  files SAME / 0 DIFF / 0 RCDIFF**, twice.  RCDIFF 0 is also the die-scan.
  Guards `Pl/t/user-unary-01.t` (12 rows) and `Pl/t/imported-term-01.t` (7
  rows), both `both_agree` against live perl, both inverse-guarded on a
  `fe46c7b` worktree (7 of 12 and 4 of 7 fail there).
- **A guard fixture should reproduce the MECHANISM, not the module.**
  imported-term-01.t builds its own module whose `@EXPORT` comes from a
  variable, the way Math::Complex does, so the row cannot pass by accident if
  one CPAN module's spelling changes.
- Gate **165 files / 5760 rows** (only the 13 pclxs xs rows); sweep **TOTAL
  18312 (+0), 0 new / 0 fixed, drops 5 = census, GATE clean**; child drops 241
  in 98 files, unchanged.
- **#484 filed, pre-existing, and it corrects #365's own guess**: `pi / 2 + pi
  / 4` is still repaired to a match and dropped.  The classifier CAN be reached
  from the #351 repair — `_repair_word_match` already asks `_word_is_term` —
  but that predicate reads only THIS document's terms, and
  `_premerge_include_prototypes` runs AFTER the repair block.

## s438 (2026-08-23, Opus 5) — the two census instruments: the census gains a SIXTH population (#473) and a SEVENTH is measured for the first time (#472); the companion scan stops silently skipping need-harness files (s434 ask 1)

- **#473 DONE — `cpan-tests/modules/**/t/**/*.t` is a census population,
  PROGRAM mode** (289 files; `tools/drop-census.pl`).  **42 files / 83 drops**,
  blessed into `baselines/parse-error-drop-census-s399.tsv` (39/102 → **81 files /
  185 drops**); the 36 non-board pre-existing rows come back BYTE-IDENTICAL.
  The s436 A/B's "43 files / 92 drops" reconciles exactly: it scanned every
  `.t` under `cpan-tests/modules`, which is these 42 plus
  `examples/tools.t` (9 drops) — author/example tests, deliberately outside,
  stated with their count in the census header beside `t/japh/` (1 file / 2
  drops, ruled permanently outside s437).  **Nothing moved since s436**: an
  `f702da3` worktree gives the same 42 files / 83 drops.
- **The dist `.t` files are transpiled WITHOUT the dist's own `lib/`+`t/lib`
  on @INC, and that is MEASURED, not assumed** — rows are identical either way
  (`tools/run-dist-t.pl` adds them; the census stays uniform with its other
  program-mode populations).  The one thing that differed was an absolute path
  quoted INSIDE a compiler message, so the census tool now strips the repo root
  from message text: a row must not depend on how ROOT was spelled.
- **79 of the 83 new drops are ONE mechanism (task #478): the prototype
  pre-scan skips every `Test2::`/`Test::` module BY NAME**
  (`_extract_module_prototypes`), so a block-form call to a dist-declared
  `(&)` sub is not a block-form call.  Discriminating probe: two modules
  identical but for the package name — `Test2::Fake` drops, `My::Blk2` runs.
  **The no-semicolon spelling of the same call is SILENTLY mis-parsed
  instead** (`blk { 42 }` → `(pl-blk 42)`, the block's value where perl passes
  a code ref).  Residue filed: #480 (`$_.2` — PPI lexes `.2` as a float, no
  concat operator in the stream), #481 (a fat comma autoquotes a METHOD NAME:
  `is $csv->module => 'M'`), #482 (`$obj->state` — a keyword-named method dies
  inside the compiler).
- **#472 DONE — `PCL_DROP_LOG` is a side channel in the ONE announcer**
  (`Pl::Parser::_announce_dropped_statement`): `FILE<TAB>LINE<TAB>TEXT<TAB>
  REASON`, appended for every drop, **ungated by `PCL_DROP_ANNOUNCE`**, never
  routed to stderr — the child's stderr IS the row's observed output.  It
  records what the census records: deduplicated per statement, no ruled
  refusal (the file is refused, loudly), no eval-string drop (it DIES and
  emits nothing).  `sweep-perl-tests.pl` sets it around the RUN only (never
  around the file's own transpile), carries a `child-drops` column in
  `_status.tsv` and prints BOTH a per-file count and the DISTINCT sites.
  REPORTED, NOT GATED until rows are blessed by hand.  No emission change, so
  **no generation bump** (corpus-diff identical over 111 files).
- **FIRST MEASUREMENT of the seventh population: 241 drops in 98 files —
  and only TEN distinct sites.**  98 of 98 files reach the SAME two, in **our
  own harness**: `perl-tests/t/test.pl:179-180`, `open(my $f,'<',$out) ? <$f>
  // '' : ''` (**task #479** — and that line is ALSO wrong in real perl: `my
  $f` in a ternary's condition is not in scope in its branches, so
  `runperl_and_capture` returns ('','') for a file that plainly has content).
  Six sites are one-off fresh_perl/runperl child programs (four of them
  programs perl itself rejects).  **One is a real gap in a real module**: core
  `Devel/Peek.pm:59` `$num |= (1<<index($D_flags,$_)) for split //, $on;` —
  **PPI lexes `<<index` as a HEREDOC**, so every unspaced left-shift by a call
  drops (**task #483**; `1 << index(...)` with spaces lowers).
- **Two numbers, deliberately** (the report prints both): the per-file COUNT
  says what that file's run lost and double-counts the harness, which every
  file re-transpiles under `*pcl-skip-cache*` (closure.t alone: 38 of the 241,
  all `test.pl`); the SITE list is the census view.
- **s434 ask 1 DONE — the companion scan runs need-harness files**
  (`tools/run-perl-suite.pl`).  A `BEGIN`-`@INC` file used to be dropped from
  the dir scan SILENTLY, which is how five files got snapshot rows nothing
  could ever refresh.  All five were measured by naming them and all five
  produce a verdict, so they join `--all`: **the file count is 528, not 523**.
  The rule survives as DATA — `%NEED_HARNESS_NOT_RUN`, empty by measurement —
  feeding the live `%not_run` NOT-RUN path (#345's shape), so a future
  unmeasurable file is COUNTED on every run instead of inferred from a total.
- **A gate row count is only comparable to a measurement of the SAME tree**
  (the s409 rule, worth restating): s437's `163/5739` was taken BEFORE its own
  review fix added two `decl-ordering-02.t` rows.  HEAD and this session both
  measure **163 files / 5741 rows**, only the 13 pclxs xs rows failing.

## s437 (2026-08-23, Fable) — s434 + s435 + s436 REVIEWED + APPROVED; Q1–Q3 DONE; `package NAME VERSION` sets `$VERSION` at the HEAD of its section's compile phase (review fix); #475–#477 filed

- **All three sessions APPROVED as shipped** (`docs/fable-answers-s437.md`).
  Independently re-measured on a COLD cache: gate **163/5739** (only the 13
  pclxs xs rows); sweep **TOTAL 18312 (+0), 0 new / 0 fixed, drops 5 = census,
  GATE clean**; companion `--all --quick --jobs 4` **523 files, zero real
  movers** (io/open.t contention, uni/variables.t the known unstable TIMEOUT,
  io/pvbm.t + op/utf8cache.t re-run alone — below); lib A/B vs `f332682`
  re-walked: 21 pure permutation + 1 `(in-package …)`; `ppi-bug-report.t`
  row 30 fails / 31 passes on 1.291; 42 probes vs perl 5.40.3.
- **REVIEW FIX — `package NAME VERSION;` sets `$VERSION` as the package
  statement is COMPILED, before every BEGIN/`use`/sub of the section**
  (`Pl::Parser2::parse`, the `@ver_run` list is gone; the assignment follows
  its own defvar at the head of the section's compile phase).  s436's
  end-of-compile-phase placement and the pre-phase-model front-of-run
  placement BOTH read undef from `package Foo 1.5; BEGIN { print
  $Foo::VERSION }` (perl 1.5).  Guard `Pl/t/decl-ordering-02.t` (two
  `both_agree` rows, statement + block form; both FAIL on the pre-fix tree).
  Corpus identical over 111; lib 22/22; the one population file with the
  spelling (`t/comp/package_block.t`) SAME.  Generation **v2-181**, artifacts
  stamp-only.  ir-spec §9 load model says where the assignment sits.
- **`io/pvbm.t` fooled the #366 serial re-run for the FOURTH time** (20/8 in
  the parallel pass AND serial, 23/5 alone — s435, s436 ×2, s437).  Standing:
  a fresh_perl-driving file's serial verdict is a signal, not a proof; re-run
  it alone before editing its row.
- **op/utf8cache.t DIFF 2/0 → TIMEOUT 2/0 is the RECOVERY load (#467)
  reaching the file's "quadratic pos" loop, and PCL runs it QUADRATICALLY —
  #477 (Target A)**: `while ($x =~ /./g) {}` over 100k chars 4.1 s net, 200k
  15.7 s, perl 1M in 0.09 s; the cost is the MATCH (pos() and wide chars ruled
  out by probe).  Snapshot row left as the measured TIMEOUT with this cause.
- **Filed from the review probes, both PRE-EXISTING on the `4356e77` base**:
  **#475** a FILE-level `our` alias is not requalified across a TOP-level
  `package` statement (`our $t = "m"; package A; $t .= "A"` → PCL m, perl mA;
  the in-block spelling works, #239/#251 — scoping ⇒ sweep is the gate);
  **#476** `-NAME` with a DECLARED sub is a negated CALL in perl (`sub foo {8}
  print -foo` → -8), the string `-foo` in PCL.
- **Rulings on the asks** (`fable-answers-s437.md` §2): prove-core under a
  `systemd-run` MemoryMax scope is IN (tools filler; measure the gate's peak
  first, announce when systemd-run is absent); **#473 YES** — `cpan-tests/
  modules/**/t/*.t` joins the census in PROGRAM mode, `t/japh` = a header
  sentence; **#472** = a `PCL_DROP_LOG` append-file side channel in the ONE
  announcer + the sweep's `child-drops` line, MEASURE first, gate after a
  blessed run; ir-spec §9.3 carries the load-time-call sentence; **a promoted
  owner's fix may ride in the promoting session under the s366 filler rule**
  (same mechanism + own bar met + new axes filed; a NEW mechanism waits);
  **#474 FOLD** into the next scheduled-block-emission change (five exposure
  probes, none diverges today); the on-demand stub block STAYS; s434's
  six-file `--bless-rows` RATIFIED after READING the rows (**standing: a bless
  after an instrument change states per file that the new rows fall under
  the registered reason**); the five never-refreshed snapshot rows → NOT-RUN
  rows or joined scan, BY MEASUREMENT (op/lex.t runs when named).
- **Queue**: next Opus session OPENS with the census instruments (#473 +
  #472 + the never-refreshed rows), then **Q4 = #453 + #365**, Q5, Q6; Q7
  re-ordered with the PROMOTED #463 item 2 (`++${"23::foo"}`, 18
  op/universal.t rows behind one drop) first, then #464 → #466 → #465, #468,
  #470, the prove-core scope, #474 when folded, #475, #476, #477.  Fable:
  review at Q4.

## s436b (2026-08-23, Opus 5) — Q3: #456 half (b) / #469, the PHASE MODEL across sections.  perl compiles the whole file before it runs a line of it, and now so does PCL

- **The assembly runs in TWO PASSES over the same sections** (`Pl::Parser2::parse`):
  every section's COMPILE phase (package preamble, decls, captured
  use/require/BEGIN, defs and scheduled blocks interleaved by SOURCE POSITION —
  order kept within and across sections), then `(p-run-compile-phase-blocks)`
  once, then every section's RUN phase.  Nothing is compiled twice; the forms
  MOVE.  A run group is preceded by an `(in-package …)` because `in-package` is
  READ-time and pass 1 leaves the reader in the LAST section's package.
- **The diff class was verified MECHANICALLY, not read**: a walker reduces each
  A/B pair to a LINE MULTISET.  Over all four populations — perl-tests 36/111,
  lib 22/22, cpan-tests 127/402, perl's t/ 159/604 — **344 changed files, ZERO
  removed lines, and every added line is an `(in-package …)` or a
  `(p-set-current-package …)`.**  A one-section file is byte-identical.
- **`p-BEGIN` SETS the runtime current package on entry and NEVER RESTORES it
  (#474).**  Invisible before, because a section's BEGINs ran immediately
  before that same section's run forms.  Under the phase model every BEGIN runs
  first, so the package left current at the start of the run phase is the LAST
  BEGIN's — measured: perl-tests/caller.t 15 → 13, because
  `{ package RT129239; BEGIN {…} }` at line 369 made the `eval 'pb()'` at line
  129 transpile in RT129239.  **Fixed where the assumption lived: every run
  group states its own package**, section 0 included, whenever the file has
  more than one section.  A one-section file reorders nothing and keeps its
  byte-identical emission.  The p-BEGIN asymmetry itself is #474.
- **Sweep TOTAL 18311 → 18312 (+1), GATE clean**, 0 new / 0 fixed, drops
  census 5 = current 5.  The +1 is the row the s432 `pass-baseline.tsv` note
  promised back: sort.t's `cmp_ok($answer,'eq','good','bug 36430')` passes for
  the reason the test intends, because `package A; sub min` is now defined
  before the earlier block's comparator runs.  sort.t 202 → 203, PARTIAL → OK,
  edited BY HAND.
- **A STALE GUARD WAS ENCODING THE BUG** (the s416 rule, paid in the same
  commit): `Pl/t/begin-end-01.t`'s "BEGIN can access package variable from
  other package" assigned `our $value = 100;` as a RUN-TIME statement in an
  earlier section and asserted a later BEGIN saw 100.  **Perl prints the empty
  string** — the row was asserting PCL's bug.  Split in two, both perl's
  answer: a COMPILE-time assignment IS visible across packages, a run-time one
  is not.
- Ten inverse probes vs perl (same-section forward call, sub-first, a BEGIN
  that DOES see an earlier section's sub, a BEGIN that must NOT see a sub below
  it, `__PACKAGE__` across sections, END order, cross-section `our`, a package
  block with a closed-over lexical, CHECK/INIT, a `use` in a later section) all
  passed BEFORE the change and still do; only the two bug reproducers moved.
- Guards: `Pl/t/decl-ordering-02.t`'s two "dies loudly" rows become six
  `both_agree` rows.  `docs/ir-spec.md` §9 gains the normative load-model
  ordering, including why "hoist the definition alone" is unsafe.  Gate cold
  **163/5739**; generation **v2-180**; all three artifacts regenerated
  (`pcl-mro.lisp`/`pcl-warnings.lisp` carry the reordering).  **#456 and #469
  close together.**
- **Companion suite (#467 runner): NET +36 C_ok over five files, nothing lost** —
  op/gmagic.t **4 → 30**, op/gv.t 130 → 134, uni/gv.t 51 → 55, comp/hints.t
  15 → 16, op/sort.t 181 → 182 (op/cond.t C_notok 0 → 1, C_ok unchanged); every
  gainer re-measured ALONE on a `f332682` worktree, which reproduces the blessed
  value.  Six `perl-suite-run.tsv` rows edited by hand.  **io/pvbm.t read low in
  the parallel pass AND in #366's serial re-run and is still CONTENTION** —
  three later alone-runs on both trees give its blessed 23/5.  A #366 serial
  verdict is a strong signal, not a proof, for a fresh_perl-driving file.
- Compile time within noise (five multi-section files, three runs each: head
  7.41/7.60/7.64 s vs base 7.44/6.87/7.38 s) — the forms MOVE.

## s436 (2026-08-23, Opus 5) — Q2 CLOSED (its four unrun legs are run and clean); #471 the compiler-side memory cap; #457 PROMOTED and fixed — 958 rows back

- **Q2's four-population emission A/B is CLEAN and mechanically shape-checked.**
  `emission-ab --ref 9138404`: lib 0/22, cpan-tests 52/402 (107 sites), perl's
  t/ 24/604 (79 sites), plus corpus-diff's 4 perl-tests files.  A walker over
  every DIFF pair requires the two sides to agree line for line except where A
  has ` nil<closers><suffix>` and B the `p-die` ending `")<closers><suffix>` —
  **186 sites, 76 files, 0 unexplained lines**.  Trap for the next such
  checker: the `nil` can carry a TAIL (`1 while …` emits ` nil) 1)`).
- **#473 NEW — the A/B and the census disagree, and the A/B is right.**  The 9
  cpan `.pm` DIFFs and the 24 suite DIFFs reproduce the census row for row,
  count for count; **43 cpan-tests `.t` files / 92 sites** and
  **t/japh/abigail.t / 2** are in no census population (`drop-census.pl`'s
  cpan population is `**/*.pm`; the suite population is the runner's
  `@DEFAULT_DIRS`, where japh is excluded).  Not a regression — they were
  dropping silently — but 94 sites that now DIE are uncounted.
- **Confirming sweep: TOTAL 18311 (+0), 0 new / 0 fixed, drops 5 = census,
  GATE clean.**  s435's hand-edited baselines reproduce exactly.
- **`baselines/perl-suite-run.tsv`: 13 rows edited BY HAND**, each measured three
  times.  **CORRECTION to s435: the companion price is −114 C_ok over 9 files,
  not −117 over 11** — `io/pvbm.t` does not move (three serial runs give its
  blessed 23/5; the `--all --quick` −3 was CONTENTION).  op/signatures.t and
  re/reg_eval.t go XDIFF → DIFF and are deliberately NOT re-blessed into
  `perl-suite-expected-rows.tsv`: those rows are lost to an ABORTED FORM, not
  to the registered not-supported reason, and blessing them would hide the
  flip's price behind an "expected divergence" label.  (Contrast s434, where
  six files went XDIFF → DIFF because they REACHED more rows — there
  re-blessing was right.)
- **A signature is useless when a path eats its budget.**  Since the flip the
  commonest aborted-form condition starts with an absolute file name and every
  one came back as `…/perl-N.N.N/t/o`.  `run-perl-suite.pl` now collapses a
  DEEP absolute path to its basename before truncating.  **Verdict-neutral by
  construction**: `read_snapshot` compares status + C_ok + C_notok and never
  `$sig`.  Rule in `docs/test-infrastructure.md`.
- **THE FLIP'S REAL WORST CASE, and the second half of "a module with a drop
  still loads" (s433 §A.1).**  The load IS fine — but a load-time CALL into a
  sub whose body carries a drop takes the module with it.  Board A/B (base
  worktree vs HEAD, because the blessed board file is far older than the tree):
  Mojo-DOM58 and Sub-Uplevel byte-identical; **Text-Balanced every test file
  PASS/PARTIAL → FAIL, 958 passing rows → 0**, because `Text/Balanced.pm:118`
  sits in `gen_delimited_pat` and the module's own top level calls it at line
  308.  The largest single price the flip has anywhere, from ONE PPI token.
- **#457 PROMOTED out of Q7 by the standing rule (a loss out of proportion
  promotes its drop's owner) and FIXED**: `Pl::Parser2::_repair_minus_word`,
  beside `_repair_glob_multiply`, same `_ends_term` predicate — after a token
  that ends a term, `Word('-name')` is split back into `- name` and the
  document reparsed.  The condition is a NEGATIVE, which is what makes it safe
  (`(-foo => 1)`, `foo(-bar)`, `$h{-x}`, `1, -bar`, `"x" . -foo` all follow a
  token that does not end a term; every one probed vs perl).  PPI §25 +
  `ppi-bug-report.t` rows in the same commit (rule 13); guard
  `Pl/t/minus-word-01.t`.  A/B: **0 diffs in all four repo populations** — the
  shape occurs nowhere in them, which is why the guard file IS the guard —
  and Text-Balanced back to 958.
- **#471 DONE: the compiler side runs under a memory cap.**  `pl2cl` re-execs
  ONCE through `sh -c 'ulimit -v N; exec …'` before PPI loads, replaying
  `/proc/self/cmdline` so perl's own switches survive (`$^X, $0, @ARGV`
  silently drops the `-I<root>` the suite runner spawns it with).  Default 4096
  MB, MEASURED: the heaviest legitimate transpiles peak at ~140 MB of address
  space, so ~30x headroom; `PCL_MEM_CAP_MB` / `PCL_NO_MEM_CAP` /
  `PCL_SHOW_MEM_CAP`.  Acceptance run for real: a scratch tree with a
  self-recursive helper dies in **3.9 s** naming the sub and the line, swap
  untouched.  Cost unmeasurable (20 transpiles: 2.276 s vs 2.287 s).  Exempt:
  `--bundle`/`--executable` (they spawn SBCL, which RESERVES address space).
  Guard `Pl/t/mem-cap-01.t`.  **RESIDUE: the ~40 `Pl/t` files that `use
  Pl::Parser` in process are still uncapped** — ask 1 of
  `docs/opus5-review-requests-s436.md`.
- **NEVER EDIT THE COMPILER WHILE A MEASUREMENT RUNS.**  A `--jobs 1` companion
  pass overlapped the in-flight #471 edit and reported TRANSPILE-FAIL for all
  13 files; the run was worthless and had to be repeated.  Same family as "an
  interrupted tool call may have RUN".
- Gate cold **163 files / 5734 tests** (only the 13 pclxs xs rows); pack.t
  5636/89 = blessed; generation **v2-179**, all three artifacts regenerated
  (stamp only).

## s435 (2026-08-23, Opus 5) — Q2: THE FLIP is in the tree (a dropped statement DIES when reached, trappable); the loss unit is the TOP-LEVEL FORM, not the statement; Q2 NOT closed

- **The flip shipped in the ruled shape** (`fable-answers-s433.md` §A.1): both
  `PARSE ERROR` emitters replace the statement with a `p-die` naming file, line,
  source text and reason.  The `;; PARSE ERROR:` comment is byte-identical — all
  four counters key on that text, never on the `nil`.  One shape, every mode, no
  classifier.  Guard `Pl/t/drop-die-01.t`; generation **v2-178**; gate 161/5715.
- **`p-die`'s no-location branch was `(error msg)` — the message was a CL FORMAT
  CONTROL string.**  Found in review.  This flip is the first emitter to feed
  arbitrary user SOURCE TEXT through it, so any drop containing `~` (every `=~`)
  raised an UNTRAPPABLE `sb-format:format-error` that killed the file.  Now
  `(error "~A" msg)`.  **A perl die message is DATA; it never reaches a format
  control position.**  All 12 internal callers with a `~` pre-resolve through an
  inner `(format nil …)`, so none relied on directives — and several build their
  message from user data, so the fix closes a latent hole for them too.
- **THE COST MODEL (record this): the unit of the DECISION is the STATEMENT; the
  unit of the LOSS is the enclosing TOP-LEVEL FORM.**  A file-level `my` compiles
  to one `let` around the whole remainder of the file, so ONE drop can cost every
  row after it (op/method.t: 1 drop, 39 unrun rows).  Not a regression against
  perl: perl's loss unit for an untrapped die is the whole remaining PROGRAM, and
  per-form recovery (#467) already narrows that to the form — PCL loses LESS.
  **RULED s435: accept, no narrowing** (every narrowing re-creates the sin the
  flip exists to kill).  **A file whose loss is out of proportion PROMOTES its
  drop's owner task; it does not bend the flip.**
- **Measured price.** perl-tests TOTAL 18366 → **18311 (-55)**, four files, both
  baselines edited ROW BY ROW with causes naming each drop's `file:line` and
  owner.  Companion `--all --quick`: **-117 C_ok over 11 files**, and every one of
  the 13 non-TIMEOUT movers reproduces its SNAPSHOT value on a HEAD worktree, so
  all are this change ("pre-existing is WHEN, not WHY").
- **A SIXTH, UNCOUNTED drop population (#472)**: every instrument reads emitted
  `.lisp` FILES, but a `fresh_perl`/`runperl` CHILD is transpiled from a string at
  run time.  Two known (perl-tests/split.t:682, bop.t:701) — **both were rows
  passing on nothing for years** (they assert the child prints nothing; PCL
  dropped the child's only statement, so it did).  The flip exposed them.
- **The compiler side has NO memory cap (#471)** — filed after a self-recursive
  helper stub took 7.76 GB, exhausted swap and hung the machine into a global
  OOM kill.  Under a cap the same bug dies in ~5 s naming itself; unbounded, the
  depth-100 deep-recursion warning fired and changed nothing.  **A warning does
  not stop an allocation.**  RULED: fix at the `pl2cl` seam (every compiler-side
  tool spawns it), own filler commit.
- `drop-census-s419-flip-gate.md`'s "lvalue family is permanently EXEMPT from the
  flip" is **STRUCK** — §A.1 killed emitter-side classifiers; it stays a
  REGISTERED absence, which is a census/doc fact, not an emission one.
- **Q2 IS NOT CLOSED**: the four-population A/B, the three-dist board re-run, a
  confirming sweep, and the `perl-suite-run.tsv` row edits are unrun (for time,
  not blocked).  `docs/opus5-review-requests-s435.md` §6 is the list.

## s434 (2026-08-22, Opus 5) — Q1: the two instruments.  BOTH measurement runners load with RECOVERY; the drop census has FIVE populations; a snapshot hole is PRINTED, from both sides

- **#467 DONE: `tools/run-perl-suite.pl` loads the emitted CL with
  `pcl::p-load-with-recovery`**, like `sweep-perl-tests.pl`.  The rule is in
  `docs/test-infrastructure.md`: **both MEASUREMENT runners load with recovery;
  a program (`./runpcl`) uses a plain load** — recovery is measurement policy,
  it buys rows after a failure, which a harness wants and a program must not.
  A recovered form is COUNTED and PRINTED (`aborted-forms:N: <condition>` in the
  signature), placed BEFORE the status decision so such a file can never read as
  OK — the opposite placement from the #367 orphan note, which is an observation
  about the RUN.  A reader stop gets its own `unreadable-form:` signature.
- **`baselines/perl-suite-run.tsv` re-blessed in the same commit** (one `--all`
  pass + the #366 serial discipline; the 22 movers over the runner's own cap
  re-run in a second `--jobs 1` pass): **52 files gained rows, 0 lost, TOTAL
  C_ok 74803 → 77128 (+2325)**, 116 signature-only changes, 2 status movers
  that a `4356e77` worktree proves PRE-EXISTING.  **The six #326 hang-set rows
  are STOPWATCH READINGS** (the file never finishes; C_ok is how far it got in
  90 s) — under load they drop 300–400 rows and re-measure at their blessed
  value alone; never read a regression into them from a count.
- **`baselines/perl-suite-expected-rows.tsv` re-blessed for SIX files only** — the
  registered XDIFF files that now REACH rows they used to die before (io/shm.t,
  mro/basic.t, mro/basic_utf8.t, mro/next_skip.t, mro/next_skip_utf8.t,
  op/current_sub.t).  The five registered files already DIFF before the session
  were deliberately left alone.
- **#462 DONE: `tools/drop-census.pl` has FIVE populations** — perl-tests, perl
  t/, `lib/**.pm`, `cpan-tests/modules/**.pm` and (behind `--board`) the 14-dist
  board's `lib/`.  **Every `.pm` is transpiled with `--module`**, the emission
  the runtime caches; the board's dists are read from
  `baselines/cpan-board14-s343.tsv` and its build root from `$PCL_CPAN_BUILD` /
  `$HOME/.cpan/build`, never written down.  The blessed census gained the s431
  hand measurement, number for number: **+12 files / +20 drops → 39 files / 102
  drops**, the 27 pre-existing rows byte-identical.  `lib/` also moved to a
  RECURSIVE glob (two shims had never been in any census).
- **A snapshot hole is now PRINTED, from both sides** (s433 §A.4): the files a
  run measured that have NO row, and — on `--all` only — the rows for files the
  scan never runs.  The second half found that all five rows s431 spliced in are
  need-harness files, so `--all` cannot refresh them and they still cannot read
  as movers; naming them is the fix for now (Ask 1 of
  `docs/opus5-review-requests-s434.md`).
- Queue unchanged: **Q2 = the flip** (`docs/plan-post-s433.md`), then Q3.

## s433 (2026-08-22, Fable) — s431 + s432 REVIEWED + APPROVED; THE FLIP HAS A SHAPE (a run-time die at the drop site, one shape, every mode); #456 half (b) = the PHASE model across sections; THE LIVE QUEUE IS `docs/plan-post-s433.md`

- **Both Opus batches APPROVED as shipped** (`docs/fable-answers-s433.md`).
  Independently re-verified: cold gate **160 / 5705** (only the 13 pclxs xs
  rows), full sweep **GATE clean TOTAL 18366 (+0)**, drops 5 = census; every
  s432 hunk read; thirteen probe files vs perl 5.40.3.
- **THE FLIP'S SHAPE (s431 Ask 1), RULED: the `PARSE ERROR` emitters keep
  their comment and replace `nil` with a perl-shaped, trappable RUN-TIME die**
  `(pcl:p-die "PCL: statement not supported at F line N: <text> -- <reason>\n")`
  — ONE shape for exempt / registered / deliberate / gap alike, in FILE and
  MODULE mode alike; NO classifier in the emitter (the distinction is the
  census's, and a classifier would be asymmetric in the dangerous direction —
  a miss on a registered row dies a 1631-row file at transpile).  The
  transpile-time stderr announcement, #363's eval-string die, `--module`'s
  silence and the Track A transpile refusals all STAY.  The flip's unit
  becomes the STATEMENT: every row before it survives, a program that never
  reaches it is unaffected, one that does gets perl's own `die` in `$@`.
  Nothing in s400 §6.4 required transpile time; P1's price table (2581 rows
  for FOUR registered statements) is what decides the shape.  The s400 §6.3
  lvalue sentence becomes "dies when the statement runs".  **The module-mode
  increment is DISSOLVED** (a module with a drop still loads); #457/#464/#466/
  #465 are ordinary fillers, #457 first.  **#462 is part of the flip's BAR**
  (a price nobody can count is not a price) and lands before it with #467.
- **#467 RULED: recovery in BOTH runners** (`p-load-with-recovery` in
  `tools/run-perl-suite.pl` too; the `perl-suite-run.tsv` re-bless in the
  SAME commit, one measured pass, every moved file explained); **until then a
  companion row count is not comparable to a sweep row count for a change
  that makes something die.**  The runner must also PRINT suite files with
  no snapshot row (the five s431 rows were found by counting).
- **s432 asks: AUTOLOAD-before-die CONFIRMED** (perl's own-package rule for a
  body-less sub; the re-silencing is half (b)'s artefact); **the guard shape
  is RIGHT** — the criterion is "one row turns RED the day the behaviour is
  fixed" (row 1 does), never a TODO marker; **rule 12 at −94 rows CONFIRMED,
  keep the die** (the 94 are the runner's, #467).
- **#456 half (b) PROMOTED and RULED as the PHASE model** (#469 is the
  general form, found by probe: `our $x = 5; { package Q; sub q1 {1} } BEGIN
  { print "B=[$main::x]" }` — perl `B=[]`, PCL `B=[5]`; the single-section
  inverse is right): emit every section's compile-phase forms (decls,
  captured, defs, sched) before any section's run-phase forms, each group
  under its own `(in-package …)` switches, MOVE never copy.  **"Hoist the
  definition alone" is UNSAFE**: a body above its section's decls compiles a
  `p-defcell` SYMBOL-MACRO (`main::$x`) as a plain free variable (probed on
  the #456 shape with a captured lexical).
- **Filed from the review probes (all PRE-EXISTING): #468** a plain call to a
  NEVER-declared sub reaches no AUTOLOAD and dies with SBCL's raw
  `undefined-function` (`%p-call-of-undefined-sub` is the ONE mechanism;
  handler-vs-stub-emission by measurement); **#469** (above); **#470** a file
  lexical promoted under its OWN name (the identity branch) aliases the
  same-named package variable — `my $y = 7; sub nm { $y } print "[$main::y]"`
  → perl `[]`, PCL `[7]`; the identity condition must also require no
  package spelling of the name.
- **Queue (`docs/plan-post-s433.md`): Q1 = #467 + #462 + the missing-row
  report → Q2 = the flip (recipe in the plan) → Q3 = #456(b)/#469 → Q4 = #453
  + #365 → Q5 = #454/#435/#455 → Q6 = #451/#452/#449/#450 → Q7 = #457 → #464
  → #466 → #465 → #468 → #470 → the former flip-gap list.  The release gate
  (push 2026-08-24 USER → CI → v0.1 tag) runs independently of Q1–Q3.**

## s432 (2026-08-22, Opus 5) — #456 half (a): a forward-declaration STUB that is CALLED runs AUTOLOAD, else DIES — it never answers a value again

- **`p-declare-sub`'s stub body was `nil`** — so a call that reached it produced
  perl's `undef` with no diagnostic.  It now calls `%p-call-of-undefined-sub`
  (`cl/pcl-runtime.lisp`), which follows **perl's own order for a body-less sub**:
  run the package's `AUTOLOAD` if it has one (`$AUTOLOAD` set to the qualified
  name, original arguments, **no `@ISA` walk** — that is the METHOD rule), else
  `p-die` with perl's message.  Rule 12: the stub's value flowed onward.
- **Two ways a stub is reached, and perl gives neither a value**: the sub is
  never defined (perl dies), or PCL ran the call BEFORE the definition — #456's
  shape, a file-level `sub nm {…}` that lands after a block carrying a
  `package Q;` switch, because that block becomes its own emission section.
- **`\&foo` is unaffected**: `p-backslash-sub` returns a TRAMPOLINE that
  re-reads `symbol-function` at call time, so a coderef taken on a stub still
  reaches a body defined later.  `p-can` / `p-stash` / `p-coderef-defined-p`
  already skipped `:stub`; only the stub's own body answered.
- **A simplified test came back**: `perl-tests/sort.t`'s
  `ok(1, "SKIP: stubborn AUTOLOAD …")` was RESTORED to perl's own assertion
  (`sort stubbedsub split//, '04381091'` → `98431100`), which passes now that
  the stub falls through to AUTOLOAD.
- **Cost: exactly one sweep row, and it was an ACCIDENTAL pass** — `sort.t`
  203 → 202 (OK → PARTIAL).  Bug 36430's `sort { A::min(@$a) <=> A::min(@$b) }`
  runs before the `package A; sub min` in its own block; both calls reached the
  stub, `undef <=> undef` is 0, the list came back unsorted, `$answer` was never
  touched and the assertion only checked that flag.  Edited into
  `baselines/pass-baseline.tsv` by hand with its cause (the third of this kind after
  s418's bless.t and split.t).
- **KNOWN INTERACTION, recorded in #456**: in a package that HAS an AUTOLOAD,
  #456's shape is now silently answered by AUTOLOAD instead of dying
  (`sub AUTOLOAD {"AUTO"} { package Q; print main::nm(); } sub nm {"PKG"}` →
  perl `PKG`, PCL `AUTO`).  PCL cannot tell "never defined" from "not yet
  defined" at the call; perl's rule is right for the case that is actually
  perl's.  Half (b) removes the dilemma.
- **THE TWO RUNNERS DISAGREE ON RECOVERY, and it is worth 93 rows** (#467): the
  perl-tests sweep loads the emitted CL with `p-load-with-recovery` (one
  top-level form at a time, recovering from a die), `tools/run-perl-suite.pl`
  with a plain `--load`.  The SAME change cost ONE row in the sweep and 94 in
  the companion suite (op/method.t 96 → 44, op/sort.t 181 → 142, op/lexsub.t
  9 → 6, op/gmagic.t 0 → 1; all four A/B-attributed against a `bc02000`
  worktree and serial-confirmed), every lost row AFTER the dying form in a file
  that already crashes.  **A companion row count is not comparable to a sweep
  row count for any change that makes something die.**
- **A companion `--quick` run's snapshot diff is mostly STALE ROWS, not movers**:
  of 18 differing rows, 14 were byte-identical on both trees (the s415 Track A
  refusal registrations moved six files to XDIFF/TRANSPILE-FAIL and nobody had
  re-run four more).  All 18 were spliced with their attribution.
- **Half (b) (the hoist order) is MEASURED, not fixed** — Parser2's
  "Cross-section forward sub calls" block hoists the DECLARATION so the form is
  readable and leaves the DEFINITION in its section.  **75 of 722 files emit
  such a stub** (perl-tests + perl's t/ + lib/ + cpan-tests/modules), nearly all
  tie/DESTROY/AUTOLOAD callbacks invoked after the file has loaded — exactly ONE
  program in the whole sweep entered one.  The fix shape (emit the DEF in the
  prologue inside its own `(in-package :X)` pair — `in-package` is read-time per
  top-level form) and its bar are in #456.

## s431 (2026-08-22, Opus 5) — the flip re-census is PRICED: what the announce→DIE flip costs in passing rows, per option

- **Census re-verified on a COLD cache at `54e2d80`: 27 files / 82 drops,
  row-for-row IDENTICAL to `baselines/parse-error-drop-census-s399.tsv`.**  No
  compiler change this session (P1 is measurement).  Full row-level table +
  the price: **`docs/drop-census-s431-flip-gate.md`** (supersedes the s419
  flip-gate doc as the current reading; s419 keeps the family numbering).
- **The 82 by verdict: exempt 39 (lvalue), registered 7, deliberate 1,
  needs-a-ruling 4 (indirect object #399), gap-with-task 31.**  Every gap now
  has an owner: #458, #460, #461, #463 (all new), #415 (11 rows).
- **A dropped statement in a file `perl` itself compiles is VALID PERL by
  construction** — the criterion that re-classified six s419 rows.  Only
  `t/comp/final_line_num.t`'s `print 1+` is a deliberate syntax error (s419
  counted seven); `t/io/open.t:267`, `t/op/utf8cache.t:70` and
  `t/comp/parser.t`'s five are gaps, probed against perl.
- **THE PRICE (rows lost, since a file whose transpile DIEs contributes
  zero): flip everything = 5300 rows / 27 files; exempt lvalue+indirect
  object = 3022 / 19; also exempt the registered absences = 441 / 15; fix
  the 31 gaps first = 0.**  The decisive number is not the gaps: **four
  REGISTERED absences sit in files worth 2581 rows** (perl-tests/sprintf2.t
  1631 for one hex-float block, t/re/pat_advanced.t 950 for four regex code
  blocks).  s419 called those rows "flip-legal today" — true per ROW, false
  per FILE, and the flip's unit is the file.
- **A census row is a LOWER bound on a drop's cost**: several drops swallow
  the FOLLOWING statement too, because PPI's mis-lex runs past the `;`
  (measured for `ok /a?/, "d";`, `{ my sub x {7} x; print …; }`,
  `sub _ {…} is(-f _, …)`).
- **Module-mode DIE is NOT the free first increment** (`drop-census-s419-flip-gate.md`
  §5 hoped it was): measured s431, **3 of the 28 board modules carry 5 drops**
  (Text::Balanced ×2 — a dist with 780 passing board rows —, Sub::Uplevel,
  Mojo::DOM58::_Collection) and **9 of the cpan-tests modules carry 15**, all
  in Test-Simple's Test2 stack.  A module-mode DIE aborts every program that
  loads them.
- **Eight of the 20 module drops MINIMISE to three one-line bugs**, all
  probed s431: **#464** a statement MODIFIER on two statement classes drops it
  (`require $m if 1;`, `local($\, $,) = (undef,"") if 1;` — right without the
  modifier, and `require strict if 1;` / `if (1) { require $m }` are right);
  **#466** `print $_ "x\n"` — `$_` in the FILEHANDLE slot (PPI gives it as
  `Token::Magic`, not `Symbol`); and on the same Test2 line, **#465** `$\` and
  `$,` are DEFINED in PCL and undef in perl (`$/`, `$;`, `$!` agree), so
  `… if $\ || $,;` takes the branch perl skips.
- **The census instrument has a MODULE BLIND SPOT** (task #462):
  `tools/drop-census.pl` = perl-tests + perl t/ + lib/; `corpus-diff.pl`'s
  SILENT-DROP counter = perl-tests only.  Neither sees `cpan-tests/modules`
  or the board dists — which is where #457 lived unseen.
- **#457 (new, from that measurement): `f(...)-g(...)` with no space —
  PPI lexes `-WORD` after a term-ending token as ONE Word (a negative
  bareword), so the whole statement DROPS.**  Third sibling of
  `ppi-upstream-bugs.md` §12 (`)*name`) and §15 (`)-1`), both already
  repaired with the same `_ends_term` predicate.  ZERO sites in all four
  in-repo populations (1139 files scanned), TWO in Text::Balanced.
- **Five companion files had NO row in `baselines/perl-suite-run.tsv`** (523 rows
  for 528 files): `comp/line_debug.t`, `op/goto.t`, `op/lex.t`,
  `op/require_errors.t`, `run/dtrace.t` — absent, not quarantined, so a
  regression in them could never read as a mover (#176 family).  Measured
  s431 and spliced with their FIRST measurement; `op/goto.t` TRANSPILE-FAILs
  on the #314 refusal.

## s430 (2026-08-22, Fable) — B3.3 / #374(b): a keyword-named lexical sub is renamed POSITION-AWARE — the sub where a term is expected, the keyword where an operator is

- **The live queue moved to `docs/plan-post-s430.md`** (P1 the flip re-census → P2 #456 → P3 #453+#365 → P4 #454/#435/#455 → P5 #451/#452/#449/#450 → push week → CI → v0.1 tag; Fable: the flip design from P1's table).
- **`my`/`state`/`our sub if () { 44 }` + `my $x = if if if` (t/op/lexsub.t
  lines 71/209/576): the renamed spellings DROPPED as three juxtaposed calls,
  the `our` one as the keyword parse (#374(b), B3.3 of
  `docs/b3-operand-collapse-s428.md` — the LAST B3 widening).**  perl's rule
  (toke.c): a bareword is looked up as a lexical sub only when `PL_expect !=
  XOPERATOR`, so the statement is `(my $x = if()) if if()` — the first and
  third `if` are the sub, the middle one the modifier (deparsed, 5.40.3).
  Fix in the RENAMER's classification, as s409 ruled — NOT the term grammar:
  `Pl::Parser2::_rename_lexical_subs` asks `_word_in_operator_position` for
  the six statement-modifier names (`Pl::PExpr::Config::is_statement_modifier`):
  operator position = the previous significant token ENDS A TERM (`_ends_term`,
  the repairs' oracle), a bareword TERM (`_word_is_term`: constant / `()` sub
  / 0-ary builtin), or a `()` call this pass itself just produced; a previous
  Word that IS a modifier keyword is an operator; the PPI statement class is
  NOT consulted (PPI makes a Compound of every statement-initial keyword —
  `(unless, 2)` inside a list, a bare `if;`, both the sub in perl; a real `if
  (…) {…}` in the sub's scope is a perl compile error, so valid input never
  has one).  **`our sub if` is the same rule with the qualified spelling**:
  perl aliases the lexical `if` to `&main::if`, PCL's bare `if` is the keyword
  at every site, so its term-position uses spell `main::if` (probed: 42 in perl
  AND PCL; the declaration stays `our sub if` — it IS the package sub; the
  only `our` declaration the pass collects).  The keyword-shaped structure PPI
  built is RE-CLASSED IN PLACE (`_reclass_keyword_call_site`, the label-merge
  precedent, no reparse): a statement-initial Compound (`if;`, `map { if } …`,
  `(unless, 2)`) → the class the parent's other children have; the orphan
  sibling PPI split off (`(for, for)` → Compound(for) + Expression(`, for`))
  joined back unless a `;` really ended it; the Condition after `if()` → List
  (`my $w = if();` dropped "Missing case: [" on BOTH trees).  ONE classifier
  for the token stream and the interpolated-code walk (`_rewrite_lexsub_use`).
- **`_ends_term` gained the POSTFIX `++`/`--` arm it was missing**: `$i++
  while …` put `while` in operator position only once the classifier asked;
  which `++` it is is the same question one token back (postfix follows a
  term, prefix does not).  The #354 glob-multiply repair shares the predicate:
  `$i++*foo` was lexed as a glob and DROPPED — now multiplication (guard row).
- **Verified over ALL FOUR populations vs `0b56ff9`**: corpus-diff IDENTICAL
  (111 files, drops 5); lib A/B SAME=22; perl-t A/B 603/604 (t/op/lexsub.t the
  ONLY diff); cpan A/B SAME=402; gate-SET scan 638×2: exactly ONE row —
  lexsub.t's first stderr line moves from the `if if if` drop to its next
  (`x`); B3 §1a reachability re-run (`PCL_TERM_DECL`, both populations): corpus
  9 declines all Word-led (unchanged), perl-t 72 declines (47 single + 25 unary), all Word-led — identical to s428.  Companion
  op/lexsub.t 7/10 → 9/8 REAL MOVE (serial-confirmed; the two `our sub if`
  rows PASS; the state/my twins un-drop behind the file's unchanged
  `undef-fn:main::pl-F` abort).  Cold gate 160/5700, failures = the 13 pclxs xs rows.  Census 27/90 → 27/82
  (lexsub.t 12 → 4; the 4 left are the `x`-named lexical sub — PPI lexes a sub
  named `x` as the repetition operator, #361/#376 family, not B3).  gen v2-177
  (artifacts stamp-only).  **Sweep NOT run, by the s401 row** (corpus-diff
  identical + lib byte-identical).  Guards: `Pl/t/lexical-sub-02.t` (6
  perl-oracle rows, 40+ shapes: my/state/our × same/other package, `if()`,
  `&if`, `\&if`, `defined &if`, `if;`, `map { if }`, `(unless, 2)`, `3 +
  unless`, `unless unless 0`, `print … if unless`, `"@{[ unless ]}"`, `$t +=
  for for 1..2`, `(for, for)`, `$i++ while`, the non-keyword inverse, the
  postfix-`++` arm); lexical-sub-01.t's #374 rows REWRITTEN from "the
  statement is a counted DROP" to "it lowers" + a runtime row (the s416
  stale-guard rule — caught by the targeted-guard batch, which is why that
  batch runs).  **#374 CLOSED (both halves).  B3 — #153's last track — is
  COMPLETE: B3.1 (s428), B3.2 (s429), B3.3 (s430).**  Next: the flip
  re-census (`docs/drop-census-s419-flip-gate.md` §4; #410 and #399 are the
  holdouts).
- **Filed #456 (PRE-EXISTING, found by the guard probe)**: `{ package Q; print
  main::nm(), "|\n"; } sub nm {"PKG"}` prints EMPTY (perl `PKG|`) — the block
  with the package switch is emitted as its own section, the hoisted `(p-sub
  pl-nm …)` lands AFTER it, and the forward `(p-declare-sub main::pl-nm)` stub
  ANSWERS undef instead of dying (rule 12: the stub should name the sub).

## s429 (2026-08-22, Fable) — B3.2 / #259: a declared sub's call parses by its PROTOTYPE'S SHAPE, never by its minimum arity

- **`1 == a_hash 'a'` (`(%)`), `(unilist3 0 || 5)` (`(;$;)`), `any_tainted @_`
  (`(@)` inside `($)`), `f 5` for `sub f($x = 1)` — every `min 0` prototype and
  every all-defaulted signature was called with ZERO arguments, its real
  arguments left dangling, and the statement DROPPED (#259, B3.2 of
  `docs/b3-operand-collapse-s428.md`).**  Cause: `Pl::PExpr::no_params_of_sub`
  returned a declared sub's `min_params` — an ARITY fact — and its callers read 0
  as "takes no arguments" (Config's convention).  Fix: ONE reading of the
  prototype's SHAPE, `Pl::PExpr::_proto_parse_spec`, implementing perl's toke.c
  `just_a_word` classes in Config's convention — `()` → 0 (a term); after the
  leading `;`s exactly one of `$ _ * +` or one `\X` / `\[…]` → 1 (named unary),
  `[0, 1]` when `;`-led; everything else, including `($;)` / `(;$;)` (a TRAILING
  `;` keeps a list operator), → -1; a signature NEVER affects parsing (-1);
  builtin records (no `min_params`) keep Config's table.  The s361 second look
  at the prototype at the strictly-single site is DELETED as redundant (the spec
  is 1 / [0, 1] exactly for the unary class).  Collateral, both perl-faithful:
  `(*)` alone is a NAMED UNARY (was read as a list op), and a 1-param / slurpy
  signature is a LIST OPERATOR (was read as a named unary — `f 'a', 'b'` for
  `sub f($x, @r)` gave `f(a;) b`).
- **The discriminating measurement was a 16×10 matrix (prototype/signature
  shape × call context) vs perl**: SAME 61 → 103 of 160, every changed row
  toward perl.  The remaining valid-program DIFFs are SEPARATE pre-existing
  findings, FILED: **#453** (a user named unary's operand does not extend
  through `.` / `+`: `f "a" . "b"` → `f(a)b` for `($)` / `(;$)` / `(*)` / `(_)`,
  and `(*)`'s `f + 1, "\n"` swallows the comma — only the BUILTIN named-unary
  site applies `_extend_high_prec`; the two operand sites should be one),
  **#454** (a signature PARAMETER `$x` plus a later file-level `my $x` →
  "file lexical 'x' captured by sub f" REFUSAL), **#455** (`use feature
  "signatures"; sub f ($x, @r) {…}` on ONE LINE: `"@r"` warns "uninitialized
  value in join" — the feature region looks line-based); **#260** gains the
  `(_)` row (`f || 5` → perl `f($_)`, PCL `f()`).
- **Verified over ALL FOUR populations vs `374fcf7`**: corpus-diff IDENTICAL
  (111 files, drops 5 unchanged — the corpus has no file with the shape); lib
  A/B SAME=22; perl-t A/B 602/604 SAME (t/comp/proto.t + t/op/utftaint.t, each
  ONLY the un-drops); cpan A/B SAME=402 (94 .pm + 308 .t); gate-SET scan:
  exactly proto.t + utftaint.t drop→OK, plus t/op/taint.t whose FIRST stderr
  line was the same-family `taint_these @list[1,3,5,7,9]` (`:prototype(@)`)
  drop and is now its pre-existing `s///e` TRANSPILE-FAIL (no CL either way).
  Companion rows UNCHANGED (proto.t 47/14 DIFF — the three rows were ACCIDENTAL
  passes, the dropped `print "not " unless …` left `ok N` printing; per-row A/B
  vs the base tree: no row moved; utftaint.t 72/17 NOTAP).  Cold gate
  159/5693, failures = the 13 xs rows.  **Sweep NOT run, by the s401
  WHAT-TO-RUN row** (corpus-diff identical + lib byte-identical ⇒ it cannot
  move).  Census 29/94 → 27/90; gen v2-176 (artifacts stamp-only); guard
  `Pl/t/proto-parse-class-01.t`.  #259 CLOSED; B3.3 (#374(b)) remains.
- **Rule (the s361 lesson, generalized): ARITY and PARSE CLASS are different
  facts.**  `min_params` / `_proto_max_args` answer "how many arguments may the
  call-time emitter see"; only `_proto_parse_spec` answers "how does a call to
  this name PARSE" — never read one as the other.  Also: `lib/warnings.pm`'s
  regen comment said plain `./pl2cl`; the artifacts are `--extension` builds
  (#349) — comment fixed.

## s428 (cont., 2026-08-22, Fable) — B3.1 / #411: an elided-arrow call of a postfix result

- **`$a[0]()`, `$s2->()()`, `(sub{})[0]()`, `$h{k}()`, `$r->{m}()` — a `(args)`
  list directly after a completed postfix element is an ELIDED-ARROW CALL of that
  result (#411, B3.1 of `docs/b3-operand-collapse-s428.md`).**  Fixed by ONE
  normalizing pre-pass `Pl::PExpr::_insert_elided_call_arrows`: it makes the
  elided `->` EXPLICIT so the existing `-> ( args )` path (walker W2 + reduction
  Case 2) handles every shape — no `_term_extent` change, no new reduction logic.
  The discriminator is the token BEFORE the list (a Subscript, a `->()` call
  result, or a list-slice Constructor — never a bare Symbol/Word primary, so
  `$foo(1)`/`func(1)`/`$cr->(9)` are untouched).  Reuse, don't duplicate: it
  normalizes PPI's elided arrow exactly as `_retag_list_slice_subscripts`
  normalizes its predecessor-classified braces.
- **The measurement corrected the plan, twice.**  (i) The fix is REDUCTION-side,
  not `_term_extent`: for `$s2->()()` / `$r->{m}()` the walker already claimed the
  full extent (ext=3) yet the statement dropped, so the sketch's "widen the
  walker" was aimed at the wrong layer.  (ii) `f()()` is a perl SYNTAX ERROR (a
  paren-less call's result cannot be called without `->`), not a #411 shape.
- **Un-dropping into a crash is not a regression when the file already crashed
  there.**  op/current_sub.t is 0/0 on BOTH b70ccaf and this tree (it crashes on
  `__SUB__`, #378); #411 removed its 3 drops (0 PARSE ERRORs emitted now) but the
  file's row count is unchanged — the drop-census removal is honest and there is
  no lost row.  Always A/B the file on the base tree before calling a companion
  move yours (the s420 rule) — op/ref.t's 197 → 198 checked out because base
  measured 197/44, matching the snapshot.
- **Verified over ALL FOUR populations vs `b70ccaf`** — corpus-diff (only
  closure.t + ref.t), perl-t A/B (only the four op/ files, zero new drops), lib
  A/B SAME=20, cpan A/B SAME=94 — plus cold gate clean (158/5688, only the 13 xs
  rows) and sweep GATE clean (TOTAL 18365 → 18367 +2, drops 7 → 5).  Baselines
  edited row by row; census 32/104 → 29/94; gen v2-175; guard
  `Pl/t/elided-call-01.t`.  #411 CLOSED; B3.2 (#259) and B3.3 (#374(b)) remain.


## s428 (2026-08-22, Fable) — s427 reviewed + APPROVED + merged (ff, `821f0bb`); generation v2-174 (`53dcd2e`); round 2 of the Opus fan-out complete

- **s427 (O3: #442, #422.2, #421, #415 items 1+4) APPROVED as shipped**, merged
  fast-forward at `821f0bb`; `53dcd2e` renumbered `*pcl-cache-generation*`
  v2-173 → **v2-174** on the merged tree and regenerated the three artifacts
  (bodies byte-identical, stamp only).  Rulings:
  `docs/fable-answers-s423-s426-s427.md` §s427.
- **The #442 expectation rewrite is CONFIRMED (ask 1)**: "the blessed answer is
  U+FFFD" (s318 §11) is about the CHARACTER — what a CL string can hold — so a
  box keeping its NUMBER through assignment is not a re-litigation of it, and
  `Pl/t/wide-codepoint-01.t`'s rewrite meets the four s376 conjuncts.
- **#421's narrow form ACCEPTED as shipped (ask 2)**: the per-package prototype
  table is consulted only when a bare name has more than one declaring package;
  the strict form is one line in `_proto_entry` if the queue ever wants it, and
  no working program can tell the difference (perl dies "Undefined subroutine"
  in the only distinguishing case).
- **STANDING RULE (ask 3): a change that RECLASSIFIES A TOKEN FAMILY runs the
  companion-suite leg** — `tools/run-perl-suite.pl` over the touched dirs, A/B
  against a base worktree — not only a `cl/` coercion change (the s421 rule).
  `<<>>` was found by that leg and by NOTHING else: the probe table,
  corpus-diff, the gate-SET scan and the sweep were all silent; only
  `io/argv.t`'s row counts moved.
- **A generation-string COLLISION is re-measured, not redone** (ratified from
  s427): when two agents take the same `*pcl-cache-generation*`, the branch
  moves to a FRESH string and RE-RUNS the affected measurements under it —
  `~/.pcl-cache` is keyed by that string, so the risk is a cached module
  transpile from the wrong compiler, not a wrong fix.  Launcher side: reserve
  per-agent strings with a GAP above main.
- **Task-store JSON is written with `JSON::PP->new->utf8` to a `:raw` handle** —
  #415/#421/#422/#442/#449/#450 had arrived DOUBLE-ENCODED (mojibake `â` runs,
  `ï¼¸` for `Ｘ`) and were repaired in s428 (`reference_task_store_json`).
- **Filed, pre-existing, not fixed: #451** — `"$?[1]"` inside a dq string prints
  `0[1]` (the scalar `$?` interpolates and `[1]` stays literal; perl prints
  element 1 of `@?`; the `"$-[0]"`/`"$+[0]"` twins work — the fix belongs in
  `Pl/InterpScan.pm`); **#452** — `<main::FH2>`, a package-QUALIFIED bareword
  handle in angle brackets, emits a BARE unquoted symbol and dies "The variable
  FH2 is unbound" at load, where `readline(main::FH3)` emits `'main::FH3` and
  works (A/B'd byte-identical on `ff0cd86`).
- **B3 (`_reduce_term`, #153) SIZED from measurement — `docs/b3-operand-collapse-s428.md`.**
  Its step-1 reachability inventory (`PCL_TERM_DECL`, BOTH populations: corpus 9,
  perl-t 72) is **entirely by-design — every operand-site decline leads with a
  Word, Operator or Cast** — so B3's DELETION half (steps 3–5's "delete the
  unreachable branches") is already complete and the two rule-12 `die` guards
  are proven safe over 715 files.  B3's remaining work is the WIDENING half:
  `_term_extent` claiming #411 (a `(args)` List after a completed postfix step,
  guarded by `$next > $i` = past the bare primary; 8 drops, first), #259
  (parenless proto arity in operator position; 3, second), #374(b) (`WORD WORD
  WORD` empty-proto lexsubs; 4, last).  ~15 of the census's 104 drops, not ~40 —
  the rest are exempt/registered/#410/#399, NOT B3's to widen.  Each widening
  lands alone behind the s373 three-leg bar, with §1a re-run as its regression
  check (a new Symbol/Structure decline = a walker inconsistency).
- **Measured on the merged tree:** cold gate **157 files / 5684 rows**, failures
  = exactly the 13 pclxs xs rows, 218 s; full sweep `--jobs 8`: **GATE clean**,
  TOTAL passing baseline 18365 / current **18365 (+0)**, drops census 7 /
  current **7 (+0)**, 0 new / 0 fixed (7 unstable = crash-file noise, 10
  unverified inside the same aborted files).  Neither baseline touched.


## s427 (2026-08-22, Opus) — O3 fillers: #442, #421, #422.2, #415

- **`chr(N)` above U+10FFFF keeps its NUMBER in every optimizer regime
  (#442)**.  The raw-string slot's eager freeze `%pcl-to-string-strict` called
  `to-string` on the `p-superchar` payload, so `my $c = chr(N); ord($c)`
  answered 65533 under the default emission and N (perl's answer) under
  `PCL_OPT=none`: **two answers for one value, chosen by a speed transform**.
  The freeze now passes a superchar through (`%pcl-superchar-payload`, which
  looks through a plain box but deliberately NOT through tie/magic — running
  FETCH there would run it twice for one write).  The U+FFFD collapse still
  happens, at `to-string`, which is where the boxed path makes it: **an
  optimization may not pre-empt a decision the general form makes later** —
  the optimization registry's own contract (`PCL_OPT=none` must RUN
  identically).  `not-supported.md` "Code points above U+10FFFF" corrected
  (s422's "once assigned it is 65533" was the bug, not the rule); guard
  `Pl/t/wide-codepoint-01.t` rows 8-11.
- **The non-ASCII subscript repair steps over WHITESPACE, because PPI does
  (#422 item 2)**.  `_reclass_subscripts_after` (Pl/Parser.pm, the #410
  repair) walked with `next_sibling`, so a space between the repaired symbol
  and its subscript stopped it and the LEXER's bareword-derived guess stood:
  `print $Ｘ {a}` was a block-form FILEHANDLE spec and `print $Ｖ [1]` an
  anonymous array (both SILENT WRONG, not drops).  `snext_sibling` is the
  exact mirror — the ASCII `$h {a}` / `@h {qw(a b)}` dump as Subscripts under
  PPI 1.291, so the repair now decides what PPI would have decided had it
  seen a whole symbol.  Guard `Pl/t/utf8-source-01.t` row 8.  (Item 3 —
  PPI's LEXER failing a whole file on `for my $Ｉ (…)` — is upstream and
  already logged, `ppi-upstream-bugs.md` §23 addendum; it is unreachable from
  a token repair and stays PPI's.)
- **A prototype belongs to the package that DECLARES the sub (#421)**.  The
  `prototypes` table was one flat bare-name hash — the #413 convention — so
  two packages declaring the same bare name with different prototypes
  collided and the LAST registration won for every call site: a SILENT WRONG
  in one declaration order (`package A; print f 1, 2` gave `A:1`, perl
  `A:12`) and a DROP in the other (`print C::h { "y" } 7, 8` → "Missing
  case: [").  A second table `pkg_prototypes` ({bare => {pkg => info}}) is
  filled at the same seam and **consulted only when a bare name has more than
  one declaring package** — with 0 or 1 the flat table IS that entry, so every
  non-colliding program takes exactly the path it took before (corpus-diff
  IDENTICAL, lib A/B SAME=22).  An unqualified call resolves in
  `current_package`, a qualified one in its own qualifier, and **the flat
  table stays the fallback** because that is how an IMPORTED prototype
  reaches a call site.  Guard `Pl/t/prototype-01.t` (9 rows, beside #413's).
- **A PUNCTUATION-named array is a variable, and `@#` was one member of a
  family (#415 item 1)**.  perl lets any punctuation character name a global
  (`@? = (1,2)` works for every character but `@+`/`@-`, which are read-only);
  PPI has Magic tokens only for the ones perl documents, so `@?` lexed as
  Cast + Operator and the statement DROPPED (`ppi-upstream-bugs.md` §24 —
  a `@` cast can never abut an operator in valid perl, so the merge is
  unambiguous).  The second half needed no PPI bug at all: `$?[1]` is element
  1 of `@?` and already lowered to `(p-aref @? 1)` through the machinery that
  serves `$#[0]`, but ONLY `@#` was forward-declared, so a file containing
  `$?[…]` died at LOAD with an unbound variable.  Both halves are general now;
  the boundary is the **CL symbol spelling**, not perl — the repair covers
  `? ! . / ~ ^ & % = < >` (CL constituents, so the symbol reads bare like
  `@#`), and `@,` `@;` `@|` `@'` `@"` `@(` need a pipe-quoted spelling and
  keep dropping loudly (**#449**).
- **`<X>` is a READLINE only for a filehandle — perlop states it as a
  whitelist and PCL had the inverse (#415 item 4)**.  The old test looked for
  glob metacharacters, so `<~>` was a readline on a filehandle named `~` and
  emitted the unbound CL symbol `~` (a dead FILE), and so did `<foo.txt>` and
  every other metacharacter-free pattern.  The rule is now: empty, a bareword
  handle, or a simple scalar (**possibly package-qualified** — Parser2's
  rename passes rewrite the symbol INSIDE the token, so `<$fh>` arrives as
  `<$main::fh__file__0>`; perl reads `<$main::fh>` as a readline too, probed)
  → readline; everything else globs.  `~` joined
  `_fix_ppi_glob_after_block`'s glob-metacharacter class (it IS one, and it is
  less ambiguous than the `*` already there), and `p-glob` expands a leading
  tilde the way bsd_glob does.  **`<<>>` (the double diamond) is the case the
  whitelist would have got SILENTLY wrong** — PPI hands it over as a Readline
  token whose inner text is `<>`, so the old blacklist rule crashed on an
  unbound `<>` symbol (io/argv.t's own failure note) and a naive whitelist
  would have globbed the string "<>" and returned nothing; it is named
  explicitly and lowers like `<>` (io/argv.t 23/30 → 27/26).  Found by the
  COMPANION A/B, not by the probe table.  Census: t/op/glob.t 1 → 0 (row removed),
  t/re/subst.t 3 → 2; gate-SET scan over both populations = exactly those two
  verdicts.  **A handle NAME is a perl IDENTIFIER, so the whitelist is the
  UNICODE word class** (`[^\W\d]\w*`, not `[A-Za-z_]\w*`): the ASCII-only
  first cut read `<ＦＨ>` as a filename pattern and globbed it silently,
  which #418's own bareword-filehandle guard row caught when the branches met
  — the probe-the-case-it-would-BREAK rule, paid by a sibling's test.
  Guard `Pl/t/punct-array-glob-01.t` (13 rows, 13 s).
- **#415's other five singles stay open, each measured**: op/utftaint.t does
  not reproduce isolated (contextual trigger, unlocated); op/filetest.t:161 is
  #403-family; `substr $x, 0, 1, = "Z"` and `{; @119797 }` are term-grammar
  drops that lead into `_reduce_term` (B3) and are NOT filler-sized —
  `{; @119797 }` also swallows the following statement into its drop;
  op/sub_lval.t:970 is the lvalue file's torture row.
- **`glob(PATTERN)` with no wildcard returns EMPTY where perl returns the
  pattern** (`glob("/nope-xyz")`, `glob("/home/")`) — PRE-EXISTING, verified
  identical on a 753ecab worktree, filed as **#450**.
- **A SHARED cache keyed by the generation string makes two agents with the
  same string one compiler** (s427's own accident).  This session was assigned
  v2-171 and agent B (#418) set the same string in its tree; because
  `~/.pcl-cache` is keyed by it, a cached module transpile could have come
  from either compiler.  The fix is the cheap one — move to a FRESH string and
  re-run the measurements under it (this branch is **v2-173**; the gate and the
  full sweep were re-run and agreed with the v2-171 runs row for row, and the
  three artifact BODIES are byte-identical between the two builds).  The
  standing rule from the s422–s424 round already says parallel agents "need
  distinct cache generations"; what this adds is the recovery: **a collision is
  not a re-do of the work, it is a re-run of the measurements under a new
  string**, plus a doc note saying which run happened under which.

## s426 (2026-08-22, Opus) — #388 consumer 3: StringInterpolation is a `scan_one` consumer; #420 + #422.1 CLOSED

- **Interpolation scanning now happens in `Pl::InterpScan` for the dq/heredoc
  parser too** (`docs/interp-scan.md` step 3, standing rule
  `var-handling-review-s379.md` §8).  `parse_interpolated_variable` is a
  `scan_one` dispatcher; `parse_braced_expression`,
  `parse_array_braced_interpolation`, `parse_array_subscript`,
  `parse_hash_subscript`, `_parse_subscript_chain` and their NINE brace-depth
  walks are **deleted** (1216 → 664 lines).  Divergences 3–7 of the
  interp-scan table are closed with them.  **Consumer 2 (the Parser2 rename
  machinery, also the 1.65 s compile-time hot spot) is the last one open.**
- **A braced NAME closes an interpolated reference; a braced EXPRESSION does
  not.**  Probed on 5.40.3: `"${x}[0]"` is `$x` then literal, but `"${$r}[1]"`
  is 20, `"@{$r}[1]"` is 20, `"@{$hr}{'a','b'}"` is a hash slice,
  `"${ $r }[1]"` is 20 with blanks, `"${$rr}->[1]"` takes the explicit arrow,
  and `"${\ $x}[0]"` dies *because it took the group*.  The scanner's flat
  "braces CLOSE the reference" comment had been probed on the NAME form only.
- **`@{^NAME}` is the magic ARRAY of that name**, the twin of `${^NAME}` —
  without that arm the reference fell to form `expr` and `"@{^CAPTURE}"` was a
  whole-statement DROP (#422 item 1).  Like every braced name it closes:
  `"@{^CAPTURE}[0]"` is `a b[0]`.  `$#-` / `$#+` gained their scanner arm too
  (#417 had shipped them in the consumer only).
- **A braced PUNCTUATION name is the SAME rule** — `@{+}` is `@+`
  (`Pl::PExpr::braced_punct_magic_name`, which the token pre-pass
  `_fold_braced_punct_magic` already applies to the same spelling in code).
  Caret and punctuation are one predicate (`InterpScan::_braced_magic_name`),
  asked by both sigils; braces close for it too (`"@{+}[1]"` is `4 3 4[1]`,
  `"${+}[1]"` is `b[1]`, probed).  **The Pl/t gate is what found this**: the
  port had no branch for it, the drop-announcement helper (#355) failed
  `Pl/t/match-vars-01.t`, and it is the exact shape #314 cost
  `re/pat_rt_report.t` its whole file.
- **A SLICE whose container is a SCALAR is a slice through a REFERENCE, and
  all four slice emitters skipped the deref** (#420's emitter half, found when
  the port finally delivered the subscript).  Not a string bug — `my @s =
  @$r[0,1]` was `""` in ordinary code where perl says `10 20`, every hash
  spelling DIED, and `@$r[0,1] = (1,2)` was a no-op.  It *looked* like it
  worked because a single-boxed anon ref (`my $ao=[7,8,9]; @$ao[1]`) is right
  by luck and `\@named` is not.  ONE helper, `ExprToCL::_slice_container_form`
  (`p-cast-@` / `p-cast-%` unless the child is a bare Symbol/Magic naming the
  aggregate), used by all four emitters **and** `_elem_container_key`'s slice
  arm; it replaced `gen_kv_array_slice_form`'s `(unbox $r)`, the same rule
  written shape-blind.  The ELEMENT accessors have always dereferenced
  (`p-aref-deref`); only the slices did not.
- **A leaf-token dispatch must reproduce WHICH PPI token the old branch made,
  not just the right value.**  `Pl/t/string-interp-01.t` is a WHITE-BOX test of
  the interpolation node shapes: wrapping a bare `"@arr"` in
  `array_str_interp` (correct at run time — `gen_string_concat` and
  `gen_array_str_interp` both join) failed it, and would have put a second
  `(p-cast-@ …)` in front of every array interpolation in the corpus.  A
  digit-named array (`"@119797"`, t/op/sub_lval.t) comes back as form `magic`
  and needs the same bare-Symbol path — there is no magic `@1`.
- **Two PRE-EXISTING bugs filed, not fixed: #443** (PCL is lenient where perl
  is FATAL on a wrong-kind deref — `${$aref}` returns the array,
  `$$scalarref_to_aref[1]` returns undef; identical in code and in strings,
  which is why `"$$r->[1]"` now prints where perl dies) and **#444**
  (`"${ \"a\" }"` emits UNREADABLE CL and loses the whole file: the `@{ EXPR }`
  arm unescapes the block text, the `${ EXPR }` arm never has — and a blanket
  unescape breaks `"${\ (2*3) }"`, whose backslash is the ref operator).

## s425 (2026-08-22, Fable) — the three open USER decisions CLOSED; second round of parallel Opus agents (finish #418, O2, O3)

- **Indirect object syntax (#399/#381) = MAYBE LATER (USER)** — registered in
  `docs/not-supported.md` ("Indirect object syntax with a SCALAR invocant"),
  a row in `docs/STATUS.md`, a bullet in the README roadmap.  MEASURED first:
  the CLASS-NAME spellings (`new Foo`, `new Foo LIST`, `new Foo(LIST)`,
  `ref(new Foo)`) already WORK through the PExpr indirect-object pre-pass,
  and every corpus occurrence of the constructor shape (cpan-tests 3 lines,
  perl t/ 3, perl-tests 0, lib 0) is one of those; the missing half is the
  SCALAR-invocant spelling (`doit $obj "FOO"` — a loud drop, exactly 2 census
  rows: ref.t:334, method.t:72 + twins) and #381's `h F` mis-read (dies
  undefined function).  NOT refused (ref.t + method.t = 288 passing rows), not
  scheduled; cheapest after B3.  #399 CLOSED, #381 stays open.
- **The v0.1 tag DECOUPLES from the announce→DIE flip — YES** (Fable, the
  USER delegated it): tag after the first green CI run + O1; the flip is
  v0.2's headline.  `docs/plan-post-s420.md` §4.1.
- **Next Fable session = B3 (`_reduce_term`, #153) — YES** (delegated);
  plan §3.1 / §4.2.
- **Parallel Opus agents, round two (USER: "try sub processes")** — the s421
  pattern (`docs/fable-answers-s422-s424.md` standing rule): a B-finisher
  continues s423/#418 IN ITS ORIGINAL WORKTREE (the previous agent died with
  the session leaving an uncommitted 10-file diff — probed first: the #418
  reproducer already answers like perl there; a copy of the diff was saved
  before the worktree was unlocked); E = s426 = O2 (#388 consumer 3 + #420 +
  #422.1, gen v2-170, ids 443–448); F = s427 = O3 fillers (#415, #421,
  #422.2, #442; gen v2-171, ids 449–454).  Each agent rebases onto main
  before its final gate so the merge is a fast-forward; Fable renumbers the
  generation ONCE per merge (v2-167 …) and regenerates the artifacts.
  **A dead agent's worktree is the first thing the next session reads**
  (memory `project_s421_opus_agents_inflight`).
- **Round-2 outcome (s425 end): E + B MERGED (`c1983e1`, `f02fe2a` + review fixes
  `dbef93c`), F STILL RUNNING at session end — the next session reads its
  worktree first.**  Rulings `docs/fable-answers-s423-s426-s427.md`: the
  in-scope second-mechanism rule (measured to its own bar + removes a copy);
  consumer 2 = O4; the narrow `is_filehandle` gate stands (#434); #435 fragment
  re-parses skip the #410 repair; **launcher rule: per-agent generation strings
  get a GAP above main** (B's rebase renumber landed on F's key).


## s424 (2026-08-22, Opus) — #423 CLOSED: a glob VALUE and a glob REFERENCE differ by the EXISTING `is-ref` flag; no new slot

- **MEASURED FIRST (the task's discriminating question): they DIFFER
  structurally today** — `p-backslash` sets `is-ref` on a typeglob wrapper and
  `box-set` propagates it, and **`box-nv` has always read it** (a bare glob
  numifies to 0, a glob ref to its address).  Only the READERS on the string /
  `ref()` side ignored it.  So fix shape (a); the s335 "no `ref-kind` slot"
  ruling is **untouched** — nothing was added to the box.
- **The typeglob is the ONE payload whose ref-ness lives on the BOX**, not on
  the object (`\@a` is a ref because a vector cannot be a scalar value; a
  typeglob CAN).  Normative in `docs/ir-spec.md` §2.5; the predicate is
  `%p-glob-value-box-p` and every reader asks it (box-sv, p-ref,
  `%p-ref-string`).  A **raw** typeglob outside a box is a glob VALUE — the
  convention `stringify-value` already fixed for the string half (#316).
- **Therefore every path that COPIES a scalar must carry the flag.**  Three
  did not, and each silently demoted a glob REF to a glob VALUE:
  `%p-flatten-list` (its preserve-list is a copy of box-set's and omitted
  typeglobs — this is what broke `my $x = shift`), `%p-array-store-scalar`
  (fresh container without the flag) and `p-aref-unbox-elem` (unboxed to the
  raw glob).  **A copy path that drops a discriminator is the same bug class
  as a reader that ignores it** — and the harder half to see, because the
  readers are wrong on probe line 1 while the copiers only show up once the
  readers are honest.
- **Carrying a flag through a copy is NOT the same as keeping the source box.**
  The first cut preserved the box in `%p-flatten-list`; a list assignment
  snapshots its RHS, so `($g1,$g2) = ($g2,$g1)` collapsed to one glob (perl
  swaps).  The snapshot must be a FRESH box carrying the flag.  Caught by the
  probe-the-breaking-case rule, guarded in `Pl/t/ref-identity-01.t` t37–t39.
- **`%p-ref-string` had no typeglob-referent arm**, so `our $r = \*STDOUT;
  print "$r"` printed `SCALAR(0x…)` while the identical lexical printed
  `GLOB(0x…)` — a missing case in a closed set that fell through to the
  caller's fallback (rule-12 family, found by the probe table).
- **s419d's op/gv.t regression is repaired at the cause**: `s///` and `tr///`
  stay on the box path (the `""` overload + tie FETCH fix stands) and the box
  path now gives a glob value its VALUE spelling.  op/gv.t 49/48 → 61/36 (the final s424 run; an intermediate run read 52/45).
- Filed pre-existing, all found by the probe table, none fixed here:
  **#436** (a lexical filehandle is a raw STREAM: `ref($fh)` is `""` where perl
  says GLOB — same family, but widening it changes every IO path, so it needs
  its own io/ leg), **#437** (`\*foo == \*foo` is FALSE: `p-make-typeglob`
  mints a fresh struct per mention — #163's defect one level down), **#438**
  (`*b = $pkgvar` holding `\*foo` installs the SCALAR slot — `p-scalar-=`'s
  box-in-box shape is invisible to `%p-glob-assign-slots`).
## s423 (2026-08-22, Opus) — #418: a non-ASCII name is spelled `|…|` EVERYWHERE, or it is a different symbol

- **THE NAME-SPELLING RULE lives in `Pl::CLForm::cl_sym` / `cl_pkg`** — a perl
  NAME that becomes a CL TOKEN is spelled there (package designators, variables
  of every sigil, sub names, CLOS class names, labels, goto tags, state cells,
  bareword filehandles, the eval capture alist's VALUES); a perl name that stays
  a STRING (`p-stash` keys, method names, hash keys, `p-register-pkg-name`'s
  second argument) is untouched.  Rationale + the reader mechanism: the comment
  block at the top of `Pl/CLForm.pm`; review record
  `docs/opus5-review-requests-s423.md`.
- **`cl_sym` is the IDENTITY on ASCII, and that is a CORRECTNESS condition, not
  a convenience**: under `:invert` a bare `$x` reads as the symbol `$X` while
  `|$x|` reads as `$x`, so pipe-quoting an ASCII name silently RENAMES it.  The
  acceptance bar for any change to this rule is therefore byte-identical
  emission over every ASCII file (`tools/emission-ab.pl`).
- **The runtime half is ONE guard**: `%pcl-invert-case` returns a name carrying
  a non-ASCII character unchanged.  Because essentially every intern site in
  `cl/pcl-runtime.lisp` already goes through that function, the emitter and the
  runtime agree everywhere at once.  Never add a second case-transform beside it.
- **The instrument for this bug class: "a non-ASCII character in a BARE token"**
  in the emitted CL (not inside `|…|`, a string literal, a `;`-comment or a
  `#\c` char literal).  Every occurrence is a symbol the reader NFKC-folds and
  case-inverts, i.e. one no runtime string can spell.  1962 occurrences / 49
  files at `a2ac578`; 0 across 404 emitted files after.
- **DO NOT put the rule on every `PPI::Token::Word` in `gen_leaf`.**  That leaf
  is a MIXED site: it also carries text already rewritten to a qualified CL
  symbol and fed BACK to `cl_name` as a perl name, where `|ＦＯＯ::f|` splits on
  `::` into the package `|ＦＯＯ` and the file fails to READ.  The bareword
  filehandle is spelled there keyed on the `is_filehandle` registry — the
  mechanism that already answers "is this bareword a handle", so every builtin
  taking one is covered at once.
- **A `|` in a name means it is ALREADY a CL spelling** (no perl identifier,
  package name or bareword can contain one), so `cl_sym`/`cl_pkg` return it
  unchanged and a double pass is a no-op instead of a corruption.
- **The sigil swap and the slice DEREFERENCE are two different questions and
  both apply** (s423 + s426, resolved at the rebase): `_swap_elem_sigil` names
  the aggregate (`$Pkg::A[0]` -> `@A`, in either spelling), and
  `_slice_container_form` then dereferences a container that is a scalar.  In
  `gen_hash_slice_form` and `gen_kv_array_slice_form` they run in THAT order.
- **`pl2cl`'s `build_eval_preamble` is the EVAL-MODE TWIN of
  `Parser::_cl_pkg_designator`** and must answer identically, or a string eval
  resolves its globals in a different CL package than the file that called it.
  It lives outside `Pl/`, which is why a `Pl/`-only grep misses it.

## s422 (2026-08-22, Opus) — #419 CLOSED: a >U+10FFFF literal costs the EXPRESSION, not the FILE

- **A code point above U+10FFFF in a string LITERAL emits
  `(p-unrepresentable-char N)`** — a form that READS and dies on evaluation,
  naming the code point and citing `not-supported.md`.  It is trappable by
  `eval { }` like any perl death, and a literal in code that never runs costs
  nothing.  Writing the character raw is what cost the whole FILE (#419):
  perl's extended UTF-8 is not valid UTF-8, so SBCL's reader rejected the
  source and `t/re/pat.t`'s 1263 rows measured as 0.
- **The two spellings of the same gap deliberately answer differently**, and
  the asymmetry is the schedule, not an oversight: `chr(N)` at RUN time keeps
  the older blessed `\x{FFFD}` (`fable-answers-s318.md` §11) because the
  compiler has no literal to refuse there; a LITERAL is a compile-time fact
  and dies.  Guarded together in `Pl/t/wide-codepoint-01.t` so neither can
  drift silently.  (Raised as the one ask of `opus5-review-requests-s422.md`.)
- **`Pl/ExprToCL.pm`'s `_cl_string_literal_form` is THE writer of every CL
  string literal** — the `'…'`, `q{}`, `q//` and `s///`-replacement paths were
  four private copies of its escaping and now route through it (byte-identical
  for clean content; corpus-diff IDENTICAL over 111 files).  Add a new quote
  form there, never beside it.
- **The population cost of the gap is TWO files** of 1205 (perl-tests/, perl's
  `t/`, `lib/**`, cpan-tests/modules): `t/re/pat.t` (8 characters) and
  `t/uni/variables.t` (1).  `perl-tests/`, `lib/` and the CPAN dists have
  none, which is why the sweep cannot see this at all.
- **#419 did NOT deliver pat.t's rows, and the next wall is measured**:
  SBCL's default 1 GB dynamic space is exhausted mid-run (0 TAP, the runner
  records an EMPTY signature), and at `--dynamic-space-size 3072` the file
  gives **225/140** and then a `Failed to match` die.  Raising the heap is a
  RUNNER decision (`tools/lib/PCLSbcl.pm` builds all five command lines and
  the memory budget is shared) → task **#424**.

## s421 (2026-08-22, Fable) — s420 REVIEWED and APPROVED; five findings filed; the live queue is `docs/plan-post-s420.md`

- **s420 APPROVED as shipped** (`docs/fable-answers-s420.md`): gate COLD
  155/5614 (only the 13 pclxs xs rows), sweep TOTAL 18364 (+0) GATE clean,
  drop census 33/106 with an EMPTY sorted-row diff against the blessed TSV,
  eight probe files — every shape the commit names identical to perl 5.40.3.
  **#390 CLOSED** (the same bug as #414, probe-verified).
- **"PRE-EXISTING" is a verdict about WHEN, not WHY.**  A companion mover
  verified on a base worktree is spliced into `baselines/perl-suite-run.tsv` only
  WITH its cause (a task or a commit) or the named next measurement.  s420's
  op/gv.t 50/47 → 49/48 had neither; one three-way probe (HEAD / `47e0750` /
  `98159c7`) attributed it to **s419d** (task #423).
- **A `cl/` change to a COERCION or STRINGIFICATION path runs the op/
  companion leg before it ships** — perl-tests has no gv.t, so the sweep
  cannot see a glob row; s419d ran re/ only.
- **A box holding a typeglob prints the REF spelling** (`GLOB(0x…)`) on the
  box path (`box-sv`) and the VALUE spelling (`*main::foo`) on the raw path
  (`stringify-value`): the box model does not distinguish `$a = *FOO` from
  `$a = \*FOO` (`ref(\$a)` answers REF where perl says GLOB).  s419d moved
  s///+tr/// from the raw to the box path — consistent with print, but op/gv.t
  lost a row.  Measure the representation before choosing (#423); the s335
  no-ref-kind ruling is re-raised WITH the measurement, never bypassed.
- **A DEREF spelling with a trailing subscript inside a dq string is SILENT
  WRONG** — `"$$r[1]"`, `"$$h{k}"`, `"${$r}[i]"`, `"@$r[…]"`, `"@{$r}[i]"`
  leave the subscript as literal text (emission `(p-string-concat (p-cast-$
  $r) "[1]")`); pre-existing, corpora near zero, but `"$$self{name}"` is a
  common old CPAN idiom (#420).  Routed through #388 consumer 3 — s379's "no
  new scanner fixes" stands.
- **The prototype table's bare-name key collides ACROSS PACKAGES** (last
  registration wins: `sub A::f($)` + `sub B::f(&@)` → `A::f 1, 2` parses with
  `(&@)`; the other order DROPS the block-form call).  Pre-existing and rare
  (#421); fix shape = (package, bare) with the bare table as fallback.
- **The reader's NFKC + `:invert` folding collides VARIABLES too**:
  `our %Ｘ=(a=>1); our %X=(a=>2)` are ONE symbol (`|%x|`) — #418 WIDENED: the
  rule is "pipe-quote ANY emitted symbol carrying a non-ASCII character".
- **PPI's LEXER fails a whole document on a non-ASCII foreach loop variable**
  (`for my $Ｉ (…) {` → "Illegal state in 'foreach' compound statement";
  `ppi-upstream-bugs.md` §23 addendum + `ppi-bug-report.t` row; no token-level
  workaround exists — #422.3).  Also in #422: `"@{^CAPTURE}"` interpolation
  drops (the `$#` and `${…}[i]` spellings work); `$Ｘ {a}` (whitespace before
  the subscript after a repaired symbol) is read as scalar + block.
- **The queue is `docs/plan-post-s420.md`** (replaces plan-post-s408 §2): O1 =
  #419 + #418 (two emission rules, the biggest prizes: re/pat.t's 1263 perl
  rows behind one literal; the uni/ files behind one quoting rule); O2 = #423
  + #388 consumer 3 with #420/#422.1/#390's shape as the acceptance set; O3 =
  fillers (#415, #421, #422.2; #399 shape count for the USER).  **Next Fable
  session = B3 (`_reduce_term`, #153)** — the flip's long pole (~40 of the ~53
  remaining compiler-gap drops).  USER decisions: the v0.1 tag DECOUPLES from
  the flip (recommended YES — tag after the first green CI run + O1); B3 next
  (yes); indirect object #399/#381 (count first).

## s420 (2026-08-22, Opus) — four of the flip's unblock-list tasks SHIPPED: census 135 → 106

- **The drop census is 33 files / 106 drops** (was 42/135 at s419), measured
  with `tools/drop-census.pl` and edited into
  `baselines/parse-error-drop-census-s399.tsv` row by row with causes.  Closed:
  **#414** (3), **#413** (6), **#412** (5), **#410** (15 of 21).  The flip is
  still BLOCKED — the remaining families are unchanged.
- **A CLONED PPI element is NOT enough to keep its tokens alive** (#414).
  PPI's DESTROY walks the tree and empties every descendant hash, so a clone
  that is a NODE takes its own tokens down when the caller returns: the
  OpcodeTree then holds HOLLOW tokens whose `content` is undef, and the leaf
  emitter dies with `…:?` — that `?` IS the undef.  Anchor anything handed to
  the parser (`StringInterpolation::_anchor`); the document-parsing sites in
  that file already did.
- **The prototype table and `declared_subs` are keyed by the BARE sub name**
  — that was always the documented convention (`_bareword_callable_here`
  splits a qualified CALL and matches the basename), but a qualified
  DECLARATION broke it.  `Pl::Environment` now normalizes at the seam, in
  BOTH directions.  **perl applies a prototype on a QUALIFIED CALL too**
  (probed: `Foo::g { … }` takes the block form) — the opposite of what task
  #413 guessed.
- **`@{^CAPTURE}` / `%{^CAPTURE}` / `%{^CAPTURE_ALL}` are implemented**
  (#412): the array lives beside `@-`/`@+` in the runtime; the two hashes are
  perl SYNONYMS for `%+`/`%-` and map straight onto them.  A caret variable of
  ANY sigil must be pipe-quoted in the emission — a bare all-caps token reads
  DOWN-cased under `:invert` and aborts the load unbound.
- **`@-` and `@+` are sized differently, and perl means it** (#417): `$#+` is
  the pattern's GROUP COUNT (undef for a non-participant), `$#-` stops after
  the last group that participated.  PCL truncated both.
- **`s///` with no match returns `""`, not `0`** (#416) — perl's PL_sv_no.
  `tr///` genuinely returns a count of `0` there (probed), and `m//` already
  answered `""`.
- **PPI splits `$Ｘ` into Cast + Word** — `Unknown.pm`'s `$` branch tests
  `/[a-z_]/i` where its `*`/`%`/`&`/`@` siblings test `/[\w:]/`
  (`docs/ppi-upstream-bugs.md` §23).  Merging the tokens is NOT enough: the
  lexer had already built the following `{…}` as a BLOCK and `[…]` as a
  CONSTRUCTOR, so `_merge_unicode_symbols` re-classes that postfix chain now.
- **A non-ASCII PACKAGE name still mismatches at the CL reader** (#418, filed
  with the measurement): SBCL NFKC-normalizes symbol names and `:invert`
  down-cases them, so `:ＦＯＯ` reads as `foo` while the runtime string stays
  `ＦＯＯ`.  Pipe-quoting defeats both.
- **One `>0x10FFFF` codepoint in a literal costs the WHOLE file** (#419):
  `t/re/pat.t` emits `"\x{4000000}"` as the six-byte pre-2003 extended form,
  which is not valid UTF-8, so SBCL's reader rejects the file and perl's 1263
  rows there measure as 0.  SBCL cannot hold the character (char-code-limit),
  so the fix is to keep the FILE readable and make only that expression die.

## s419 (2026-08-21, Fable) — the flip re-census: 135 drops ALL CLASSIFIED; the announce→DIE flip is BLOCKED

- **The flip re-census is DONE and the verdict is BLOCKED — by the plan's own
  bar, not a new judgment** (`docs/drop-census-s419-flip-gate.md`).
  `tools/drop-census.pl` at `7a03d93` reproduces the blessed census ROW FOR
  ROW (42 files / 135 drops); the harvest was read whole and every drop now
  has an owner (14 families).  Arithmetic: 39 lvalue rows permanently exempt
  (§6.3), 14 flip-legal (test-deliberate + registered), **82 genuine compiler
  gaps in productive files** (ref.t 191 passing rows, closure.t 272,
  method.t 97, …) — flipping would TRANSPILE-FAIL ~30 files, the state.t/#399
  lesson at 10× scale.  Unblock list = tasks #410–#415 (new), #399, #374(b)/
  #365, #259, #153-B3.  Re-run the census after they land; flip with the
  s373 three-leg bar.
- **The one UNBLOCKED flip increment: `pl2cl --module` mode** — a drop in a
  USER's module at runtime is the last fully SILENT #138 member (announce
  OFF by ruling s403, server stderr discarded).  Flipping module mode to
  DIE touches no test population but WAITS for one measurement: a
  cpan-board re-run counting board modules that carry drops (flip-gate doc
  §5).
- **`@?` is legal perl** (probed s419) — re/subst.t:346's drop is a real
  punctuation-ARRAY gap (#415 item 1), not a torture row.
- **`sub main::end(&)` — a package-qualified prototype declaration — is
  invisible to the block-form prototype mechanism** (#413): die_unwind.t /
  die_except.t's six `my $c = end { … }` drops are THIS, not a feature
  absence.
- **#119 + #402 SHIPPED (the ruled phase-4 pair, s416 §7.4) — the two
  stringify-instead-of-overload shortcuts are closed.**  #119: `do-regex-subst`
  and `do-tr` read the match source as `(to-string (unbox box))`, skipping
  box-sv's `""` dispatch — now `(to-string box)`, matching `do-regex-match`'s
  existing fix.  #402: `p-string-concat` now folds left through the
  overload-aware `p-.` when any piece carries a `.` handler — RUNTIME-ONLY
  (the emission already passes pieces raw), so no gen bump; probed: perl's
  multiconcat calls the handler once per object piece, left-to-right with the
  reversed flag, a plain-string handler result makes the rest plain, and a
  SINGLE `"$o"` piece is stringification (never `.`) — the fold with a
  one-piece guard reproduces all of it byte-identically.  Guards in
  `Pl/t/overload-01.t` (23 rows).  Filed **#416**: `s///` no-match returns
  `0` where perl returns `""` (PL_sv_no) — pre-existing, found by the probe.

## s418 (2026-08-20, Fable) — s417 RULED; the B2 fix DESIGNED **and SHIPPED**; runpcl surfaces drop announcements

- **B2 (#343) SHIPPED in the same session** (`docs/b2-ceiling-fix-s418.md` §5's
  bar, all legs): emission A/B over the FOUR populations — diffs ONLY
  bless.t/split.t (both populations) + **Text::CSV_PP.pm:1566, an ELEVENTH
  uncounted silent-wrong in the cpan population the s417 scan never covered**
  (grep swallowed `and $hdrs ||= "auto"`; probed both branches vs perl);
  reg_fold.t byte-identical as predicted.  Gate-SET scan 638×2: exactly the 4
  drop→OK moves.  Gate **154/5594**; census 46/139 → **42/135**; generation
  **v2-161**; guard `Pl/t/listop-ceiling-01.t`.  Sweep TOTAL 18369 → **18363**
  and the -6 is the fix being HONEST: bless.t's un-dropped assertion runs and
  fails (bless misses get-magic on a tied operand, filed **#408**); split.t's
  un-dropped `or skip` fires the file's own guard (PCL `\s` under /u misses
  NBSP, filed **#407**), skipping 3 baseline fails and 6 ACCIDENTAL passes
  that ran on the dropped statement's undef `$sp`.  Baselines edited row by
  row.  #259/#335 probed post-fix: no change, as the design predicted.

- **s417 APPROVED as shipped** (`docs/fable-answers-s417.md`): gate re-run
  COLD 153/5590 (only the 13 pclxs xs rows), sweep RE-RUN clean TOTAL 18369
  (+0) drops 10 = census, 14 probes vs perl 5.40.3 all as documented,
  reduce-term-01.t byte-identical to the s416 tree.
- **B2 (#343) fix RULED: RECOMPUTE the ceiling at use; `$last_low_prio_op`
  is DELETED** (`docs/b2-ceiling-fix-s418.md` is the design).  One rightward
  scan of the CURRENT `@$e` from `$i+1` to the first same-level
  `and`/`or`/`xor` — the probe's own `$actual`, validated over 658 files.
  Adjust-on-splice REJECTED (bookkeeping at every reduction site = the bug's
  shape, multiplied).  The `pexpr-term-parsing-review` region prohibition
  does NOT apply — no new rule, the boundary's meaning is unchanged.  The
  `PCL_B2_TRACE` probe is deleted with the fix and does NOT become a gate.
- **`runpcl` forwarded NOTHING from a successful transpile's stderr** — the
  #339 drop announcement was discarded in exactly the run context it exists
  for.  Fixed (s418 review commit): `^PCL: ` lines are forwarded to stderr on
  success; clean programs byte-identical; a difftest case that drops now
  MISMATCHES instead of accidentally matching.

## s418c (2026-08-21, Fable) — #401 eval half SHIPPED: a named sub's `state` cell reaches string eval

- **A `state` cell joins eval-site capture alists via `_eval_state_captures`**
  (#401, s418c): original→cell registered when the DECL STATEMENT lowers (so
  visibility starts at perl's masking point — an eval before the decl sees
  the outer name), scoped by `_lower_sub`'s save/restore WITHOUT the
  `_let_bound_vars` wipe (an enclosing sub's state stays visible to a nested
  sub's eval — op/sub.t's `predeclared`/`inside_predeclared`; the cells are
  defvars, so the wipe's unbound-at-call-time hazard cannot apply).  Alist
  order: let-bound pairs (a `my` shadow wins) → state pairs → span pairs.
  Waived ONLY for a SCALAR decl at the top level of the sub's own block;
  inner-block decls and containers keep the refusal.
- **The p-eval cache needed NO new code for the capture-dependent emission —
  #296-B1's key already carries the capture NAMES** and the alist VALUE
  resolves at runtime; the mandatory leg was the discriminating probe (two
  subs, same eval text, different cells → `ABA`, identical to perl).
- t/op/sub.t TRANSPILE-FAIL → **DIFF 53/12**, one row better than its s410
  snapshot; emission A/B: op/sub.t is the ONLY mover across 1139 files.
  Guard `Pl/t/state-eval-01.t`.  #401 CLOSED (both halves).

## s417 (2026-08-20) — Track B1 SHIPPED: stacked filetests parse, and lower to the `_`-chain

- **`-f -d $x` IS ONE TERM and lowers to perl's `_`-chain, never a nest**
  (#372, `docs/b1-operand-grammar-s416.md` is the design, shipped unchanged).
  `-f -w -x $file` == `-x $file && -w _ && -f _`: RIGHTMOST test on the real
  operand, each earlier one on the stat buffer, `&&`-short-circuited.  Nesting
  is SILENT WRONG in the opposite direction — `-e -f "/etc/passwd"` is 1 in
  perl and undef nested.  Guard `Pl/t/filetest-stack-01.t`.
- **A `-X` Operator STARTS A TERM — one line in the SHARED oracle**
  (`_is_print_term_start`), not scoped to the caller that had the bug.  All
  three call sites want perl's answer; `print STDERR -e $f` reads STDERR as the
  handle BECAUSE `-e` starts a term.  The `$_`-default pre-pass was splicing
  `$_` into the MIDDLE of a stacked run, which is the whole story of the 27
  census drops — **the operator loop's prefix-run walk already reduced the run
  rightmost-first**, so B1 needed NO `_term_extent`/`_reduce_term`/`$end_pars`
  change and no new grammar production.
- **PPI SPLITS A FILETEST AFTER A SCALAR/BLOCK FILEHANDLE**
  (`ppi-upstream-bugs.md` §22): `print $fh -e $f` lexes as Operator('-') +
  Word('e') while `print STDERR -e $f` lexes correctly.  ADJACENCY is the
  discriminator and perl honours it too (`print $fh - e $f` really is
  `-(e($f))`, by deparse), so the repair keys on `next_sibling`.  There is no
  competing reading to protect: **`$n -e $b` is a perl SYNTAX ERROR**, so `-e`
  is never a binary operator.
- **A FILETEST'S FALSE CARRIES INFORMATION** — perl gives a DEFINED `""` when
  the stat succeeded and `undef` only when it FAILED (`-s` gives 0, `-e` is
  1-or-undef).  PCL answers undef for both, for a plain `-f "/tmp"` too.
  Filed **#403**; do not assert definedness on a filetest until it closes.
- **perl STACKS THROUGH PARENTHESES**: `-e (-f $x)` deparses to `-e -f $x`,
  but `my $t = -f $x; -e $t` does NOT stack — the rule is syntactic, not
  "a filetest applied to a filetest's value".  PCL nests the paren form
  (pre-existing, byte-identical emission).  Filed **#404**.
- **`print $fh -3` writes to `$fh`** (an adjacent negative literal is a term)
  while `print $fh - 3` is a subtraction on the glob.  PCL reads both as the
  subtraction.  Filed **#405** — the same adjacency question §22 answered for
  letters, unanswered for numbers.
- **`class NAME ;` REFUSES only on EXPLICIT evidence** (#399 half b, RULED
  s416 §7.5, shipped s417).  The statement form parses as the indirect-object
  call `NAME->class` — in PPI, in PCL, and in PERL when the feature is off
  (probed: perl dies "Can't locate object method \"class\"") — so the default
  reading is RIGHT and must not change.  The refusal fires only for
  `use feature 'class'`, `use experimental 'class'`, or a `class NAME { … }`
  BLOCK elsewhere in the file.
- **A VERSION BUNDLE IS NEVER EVIDENCE FOR AN EXPERIMENTAL FEATURE on code
  that COMPILES** — `use v5.38; class Foo;` is a perl SYNTAX ERROR (probed),
  so the bundle cannot mean the feature is on.  It stays acceptable at DROP
  sites, where the statement is already lost.  Two keys, ONE scanner with a
  `$strict` flag (`Pl::Parser::_class_feature_in_scope`), so they cannot drift.
  Guard `Pl/t/class-refusal-01.t`.
- **B2 (#343) IS A STALE INDEX, not a grammar gap** — MEASURED, not yet fixed
  (`docs/b2-stale-operand-ceiling-s417.md`; Track B is Fable-designed).
  `handle_subcalls` scans `@$e` RIGHT TO LEFT, saves `$last_low_prio_op = $i`
  at an `and`/`or`/`xor`, then REDUCES words to its left — splicing `@$e` and
  shifting that operator left without adjusting the saved index.  The
  paren-less list operator's `$end_pars = $last_low_prio_op - 1` is therefore
  computed from a stale position and its argument list swallows the operator.
  The error is (elements the intervening reduction consumed − 1), so it is
  **not always off-by-one**.
- **THIS FAMILY HAS AN UNCOUNTED HALF**: with a shift ≥ 2 the statement does
  not DROP, it RUNS wrong (`f ref $h{k} or g "fb"` — perl `f() g(fb)`, PCL
  `g(fb) f(1)`).  The drop census can only ever see part of #343.
- **Population scan (`PCL_B2_TRACE=1`, 658 files): 10 events, 3 sources** —
  bless.t:179 (the census DROP), split.t:503 (a SILENT WRONG in no count),
  reg_fold.t:165 (**stale but BENIGN**, probed identical to perl).  So a
  stale-index disagreement is **necessary but not sufficient** for a
  divergence — do not build a gate that assumes otherwise.  `map` and `eval`
  fire too, so #343's "parenless USER-sub call" framing is too narrow.
- Census after B1: **46 files / 139 drops** (was 49/165); gate **152 files /
  5579 rows**; cache generation **v2-160**, all three artifacts regenerated.

## s416 (2026-08-20, Fable) — s414+s415 REVIEWED and APPROVED (one stale guard row fixed); the §7 asks ruled; B1 re-designed from measurement

- **s414 + s415 APPROVED as shipped** (`docs/fable-answers-s415.md`): cold
  gate, full sweep (TOTAL 18369 = baseline, GATE clean, drops = census) and
  a `--quick` companion independently re-run; 14 probes vs perl 5.40.3 all
  identical; the ten code commits read whole.  ONE review fix:
  `Pl/t/parser2-02.t` t60 asserted the refusal s415e removed and had failed
  the gate deterministically since `829bcf5` — the shape itself is CORRECT
  (probed: `22`, = perl); the guard was stale.  Gate row count is now 5566.
- **REMOVING/REWORDING A REFUSAL: grep Pl/t FOR ITS MESSAGE TEXT in the same
  commit** — guard rows are twins and live in more than one file (t60 was
  state-01.t's twin).  `fable-answers-s415.md` §2.
- **Track A's two deviations RATIFIED** (indirect object OUT, format
  INVERTED); **standing procedure: a refusal-family conversion runs
  drop-harvest FIRST and re-decides per family on file productivity**
  (§3/7.1).  **"Refused is explained" IS the census intent** — the census
  counts SILENT drops, not remaining features; the flip's re-census lists
  refused files as their own rows (§3/7.2).
- **#401 string-eval half SCHEDULED session L** (cache-key leg mandatory,
  sweep IS the gate); **#402+#119 = two tasks, one session, release phase 4**;
  **`class NAME ;` refusal AUTHORIZED with the STRICT key** (explicit
  feature/experimental 'class' or a class BLOCK in the file — never the
  v5.38 bundle heuristic on compiling code) + full s372 bar (§3/7.3–7.5).
- **B1 (#372) RE-DESIGNED FROM MEASUREMENT — `docs/b1-operand-grammar-s416.md`
  supersedes the plan §2 sketch.**  The drop is in
  `_default_filetest_operand`/`_is_print_term_start` (a `-X` Operator "is
  not a term start" ⇒ `$_` spliced mid-run), NOT at the operand-taker; the
  prefix-run walk already reduces rightmost-first.  B1 = one predicate line
  + the print-argument leg it exposes + the `_`-chain DESUGARING (naive
  nesting `(p--f (p--d $x))` is SILENT WRONG: perl defines `-f -d $x` as
  `-d $x && -f _`, probed).  NO `_term_extent` change, NO `$end_pars` edits.

- **THE THREE RELEASE DECISIONS ARE SETTLED (USER, s416)** — plan-post-s408
  §5 is the authoritative text: name = **PCL under the GitHub org
  Percolisp**; **pclxs WAITS** (not bundled with v0.1 — one real module
  validated end-to-end; 50 % of random XS-with-deps is months away; the XS
  target is "top-20 hub modules", measured by coverage curve); hosting =
  **github.com/Percolisp/pcl** (exists, user's curated 70-commit public
  history, disjoint from the local 1,197; local `origin` wired, NO push
  yet).    PUBLISH MODEL SETTLED: FULL HISTORY, executed week of 2026-08-24 after #279 (plan-post-s408 §5 has the recipe).  #279 gains: the tracked `.claude/settings.json` hook
  path is absolute — make relative before the next public push.

## s415 (2026-08-19/20, Opus) — #281 items 1+2+6 merged; Track A (#371) refuses five families; then #370 (a PPI lexer bug), #369 (qx) and half of #401 put statements BACK; census 378 -> 165

- **#281 items 1+2+6 are IN `main`** (`s414f`, verified s415).  The four bar
  items, all measured on the branch tip: full sweep `--jobs 8` **GATE clean,
  TOTAL 18513 (+0), 0 new / 0 fixed, LOST 0, drops 13 = census** (the 6
  UNSTABLE + 10 unverified rows are the same three crash-files — postfixderef.t,
  ref.t, yadayada.t — as every recent run); pack.t alone at `--timeout 380`
  **5636 / 89 = the blessed numbers, 0 new** against the regenerated
  `cl/pcl-pack.lisp`; bench A/B main-vs-branch over all 15 rows **no move**;
  and `docs/ir-spec.md` §4 / §5.4 / §10 / §12 rewritten normatively.
- **THE CONTEXT BIND HAS TWO LEGAL SPELLINGS, and the manual says so.**
  `(p-list-ctx …)` / `(p-scalar-ctx …)` / `(p-void-ctx …)` / `(p-caller-ctx …)`
  are the named form, but the bare `(let ((*wantarray* V)) …)` REMAINS
  wherever the context bind rides with a second binding (the `sort $var LIST`
  comparator binds `*package*` alongside) and in v1-emitted statement text.
  Anything matching through a context wrap must peel both — `%p-strip-ctx` in
  the runtime, and see the s414 line above for the three matchers that broke.
  `docs/ir-spec.md` §4 is the normative statement.
- **A `sort` comparator is the one BLOCK with a return frame of its own**
  (`p-sort-cmp`, ir-spec §5.4): `return` inside a sort block exits the
  COMPARATOR, so it carries `(catch :p-return (block nil …))`, its body is
  emitted in scalar context, and leading `(declare (special …))` stays at the
  lambda head for a region's package-qualified `$a`/`$b`.  `grep`/`map`/`eval`
  bodies get NEITHER — `return` there propagates to the enclosing sub.
- **A call to a context-INSENSITIVE sub emits no bind at all**, which the
  ir-spec worked example (§12) had been getting wrong since before the
  `insensitive-call` Kind-A rule existed: `print greet($w)` emits
  `(p-print (pl-greet $w) …)`, not a wrapped call.  The example now says so
  and names the general shape beside it.
- **A BENCH MOVE THAT REVERSES ON RE-MEASUREMENT IS NOISE — and the perl
  column is the noise meter.**  The K=5 A/B showed `cfor` +5.4 % and `gcdrec`
  +3.2 %; at K=7 `cfor` reversed and `gcdrec`'s two interleaved repeats gave
  branch 0.0921 / main 0.0970 / branch 0.0990 / main 0.0911 — a within-tree
  spread wider than the between-tree delta.  `perl(s)` moved ±3 % across the
  same runs with an identical binary, which is what sets the band.  The
  structural argument comes first, though: the two trees' emission for the
  bench differs in exactly the `let`→`p-…-ctx` renames (diffed), and the
  macros' expansion is asserted byte-equal by `Pl/t/transpile-test-10.t`.
- **A PARKED BRANCH NEEDS A REBASE BEFORE `--ff-only` if main took any commit
  meanwhile** — twice in two sessions now (s414's `s413-lisp-dedup`, s415's
  `s414-ir-macros`), both times because the parking session's own docs commit
  landed on main after the branch.  Rebase (the branch is code, main's extra
  commit is docs — conflict-free), re-verify on the rebased tip, then
  fast-forward.  Writing the docs commit BEFORE the branch commit would avoid
  it entirely.

- **FIVE FAMILIES NOW REFUSE INSTEAD OF DROPPING** (#371, Track A of Option B
  phase 2): `given`/`when`/`default` (+`CORE::given`), the 5.38
  `class`/`field`/`method`/`ADJUST` syntax, an uncaught `format NAME =`,
  `defer {`, and **infix** `~~`.  ONE classifier
  (`Pl::Parser::_ruled_refusal_for_drop`) asked by the drop announcer that both
  PARSE-ERROR emitters already call, so it runs only where a statement was
  ALREADY lost and cannot break compiling code.  Guard file
  `Pl/t/ruled-refusal-01.t` (27 rows, 1 s) pairs every refusing shape with the
  sibling that must keep dropping.
- **A REFUSAL CLASSIFIER IS ASYMMETRIC — a miss costs nothing, a false
  positive costs a whole file.**  Hence: `~~` only with a TERM before it
  (`is(~~$y, 3)` in bop.t, 507 rows, is a double bitwise complement and PPI
  lexes both as ONE `~~` token); `field`/`method`/`ADJUST` only when the FILE
  says the feature is on (`use feature 'class'` / `use experimental 'class'` /
  `use v5.38`+ / a `class NAME` statement), with NO shape guard beside it,
  because PPI reads `method m { 1 }` as a `m{…}` MATCH — correct for a file
  without the feature.
- **MEASURE WHERE THE DROPS ARE BEFORE CONVERTING THEM.**  Two of Track A's
  seven table rows did not survive contact with `tools/drop-harvest.pl`:
  **indirect object** was REMOVED from the list (its only two drops are in
  perl-tests/ref.t and method.t, which contribute 191 and 97 passing rows —
  ~288 rows to convert 2 drops, and perl still parses the syntax; task #399),
  and **format was INVERTED** — the drops were in productive files
  (t/op/closure.t has 267 passing rows), so the mechanism got fixed instead.
- **THE `($str_re)|` PASS-THROUGH IDIOM IN `_preprocess_source` IS WRONG, not
  merely imperfect.**  Any quote imbalance opens a "string" that swallows what
  follows: a `"` inside a regex (`qr/Undefined format "…"/`), an apostrophe
  pair across two comments (`# doesn't` … `# that's`), a quote in a format
  PICTURE line.  Measured consequences: **t/op/write.t stripped 39 of its 104
  formats** and each of the 65 survivors ate the statement after it; the
  HEX-FLOAT pass rewrote hex-float text INSIDE string literals it failed to
  skip, so `is(sprintf("%a",-0.0), "-0x0p+0", …)` had its EXPECTATION turned
  into `"-0"`; `CORE::state $x = 42` stayed unnormalised.  The skip pattern now
  also consumes comments, and **the format strip is line-anchored**
  (`_strip_format_blocks`): header line ending at `=`, body, a line holding
  just `.`, a 500-line safety valve, removed lines replaced by EMPTY lines so
  later line numbers (and every `p-die :loc`) match perl's.  The name may be
  anything up to the `=`.  Result: 12 perl-tests rows moved fail→pass, and the
  drop census fell **73 files / 378 drops → 56 / 177**.
- **A REFUSED FILE'S OTHER DROPS LEAVE THE CENSUS WITH IT** — t/op/coreamp.t's
  4 lvalue-sub drops, t/op/tie_fetch_count.t's 3.  The census now falls faster
  than the underlying problem; that is by design (the file is refused, not
  fixed) and is written into the census header so the phase-2 exit criterion
  is read with it in mind.
- **THE COST, RECORDED, NOT HIDDEN** (task #400): a transpile-time refusal
  yields no TAP, so `perl-tests/state.t` (158 rows) and `t/op/state.t` (126)
  contribute nothing for one `given` block each.  Both left the baselines BY
  HAND with the cause.  Not avoidable by ordering — at the end of phase 2
  every remaining drop dies and given/when will still not be implemented — and
  every other file the five families touch was already producing 0–27 rows.

- **`CORE::state` NORMALISING FOR THE FIRST TIME COSTS TWO COMPANION FILES —
  and that is the right answer** (task #401): `t/op/sub.t` (52 rows) and
  `t/opbasic/concat.t` (248) now die on two v2 state gates instead of
  transpiling.  Before, `CORE::state $x = 42` lowered as a plain PACKAGE
  ASSIGNMENT — a state declaration silently compiled into something else — so
  those rows passed on a program PCL had mis-parsed.  A loud
  `Parser2 TODO:` beats a silent wrong; #401 carries the two shapes with the
  300 rows as its acceptance measure.

- **A TERM-INITIAL `~~` IS TWO COMPLEMENTS, and PPI says smart match** (#370,
  `docs/ppi-upstream-bugs.md` §21).  Sixth member of the token-repair family:
  `Parser2::_repair_term_initial_complement` splits a `~~` whose previous
  significant token fails `_ends_term`; an infix one is left for Track A to
  refuse.  bop.t's two rows now RUN (sweep 18367 → 18369).  **And the refusal
  classifier stopped re-asking the same question** — its hand-rolled whitelist
  missed `$o->method ~~ 1`, which `_ends_term` gets right; with the repair
  upstream, any surviving `~~` at a drop site IS infix.
- **EVERY `qx` SPELLING IS THE BACKTICK TERM** (#369): PPI hands `qx{}`/`qx()`/
  `qx[]`/`qx//`/`qx''` a `QuoteLike::Command` and only backticks their own
  class, so `my $c = qx{echo hi}` had no primary and the statement was dropped
  — `$c` silently undef.  ONE primary arm takes both, with the body and the
  interpolate-or-not answer read from **PPI's own section record** (a `''`
  section is perl's literal form, so `qx'…'` must travel as a SINGLE-quoted
  token — a double-quoted one would re-interpolate the `$`).  VarAnnotator's
  two interpolating-leaf predicates learned the class too: a variable used only
  inside `qx{…}` was invisible to them.
- **TWO `state` RENAME REFUSALS WERE OBSOLETE** (#401 half): both predate
  `_rename_decl_within` becoming shadow-aware (#254 B-ii) and region-limited
  (#296-B2), and one of them was simply a caller not passing the `shadow_ok`
  waiver its own blocker already took.  Probed in all three orderings — inner
  `my` in a nested block, a later `my` at the same level, a `my` BEFORE the
  state decl (which it shadows from its own point on) — all identical to perl.
  **t/opbasic/concat.t recovered its 248 rows.**  What stays is the STRING-EVAL
  refusal (t/op/sub.t, 52 rows): a state rename makes a defvar'd cell that
  never enters `_let_bound_vars`, so an eval capturing by name would miss it —
  real, not stale, and its fix must answer the p-eval source+package cache key
  first.
- **A REFUSAL THAT PREDATES A FIX IS A TAX NOBODY IS COLLECTING.**  Three of
  the four refusals cleared this session were conservative guards written
  against a renamer/predicate that has since been made correct, and the code
  comments beside them SAID so ("which is why the callers' blocker refused the
  shape outright").  When a blocker's stated reason names a mechanism, re-read
  that mechanism before believing the blocker.
- **#402 filed**: string interpolation STRINGIFIES instead of concatenating, so
  an overloaded `.` never runs — `ref("a $obj b")` is empty where perl says the
  class ([perl #124160], the thing t/opbasic/concat.t:203 asserts).  Same
  family as #119.

## s414 (2026-08-19, Opus) — the s413 runtime branch merged (#395); #387 families 2+10 extracted, and the three differences between the copies were three bugs

- **s414 (Opus): the runtime branch VERIFIED and fast-forwarded into `main`**
  (task #395, tip `71d2506`).  Three legs, all measured on the branch tip:
  gate 150 files / **5517 rows** (5516 + the #394 guard row), only the 13
  pclxs xs rows red; bench A/B main-vs-branch over the five new runtime rows
  + arrhash/collatz/strcat — every `pcl(s)` within noise (the largest
  apparent move, `strcat`, was 1 ms at the resolution floor and REVERSED
  direction at best-of-7: noise, not a regression); full sweep `--jobs 8`
  **GATE clean, TOTAL 18513 (+0), 0 new / 0 fixed, LOST 0, drops 13 =
  census**, with the 6 UNSTABLE rows in the same three crash-files
  (postfixderef.t, ref.t, yadayada.t) the batch's own main-side sweep
  reported.  The #394 fix added no perl-tests row — nothing in that
  population takes a string index or `delete @a[RANGE]`; its guard is the
  Pl/t row.  The branch was REBASED onto main's docs commit first, so the
  merge was a true fast-forward and the six shas are main's: `4c88bd9`
  (23), `8a7ffe8` (13), `c1f7142` (21 + #394), `a6bad40` (37), `637712b`
  (42), `71d2506` (46–48).
- **`ExprToCL::_elem_container_key` is THE decomposition of an element or
  slice access** (`s414b`, #387 families 2+10): given a builtin's argument it
  answers `($kind, $container, @keys)` or `()`, and the `tied` / `pos` /
  `delete` / `exists` arms became a head table each — 201 lines → 62, corpus
  emission IDENTICAL over 111 files, all 23 lib modules + `cl/pack-impl.pl`
  byte-identical (no gen bump, no artifact regeneration).
- **The three ways the copies disagreed were three bugs**, each probed
  against perl 5.40.3 before unifying (the s413 census-as-bug-finder rule):
  1. **#397** — `tied`/`pos` swapped the container's sigil UNCONDITIONALLY,
     `exists`/`delete` only for a bare Symbol/Magic container.  The guard is
     load-bearing: `_swap_elem_sigil`'s regex is unanchored, so on a nested
     access's text it also rewrote the package-qualified INDEX —
     `pos($a[$i]{$k})` with `our $i` emitted `Pkg::%i`, a reference to a
     DIFFERENT variable (ac6fdc1 fixed the two in 2026-06 and never reached
     the twins).  Masked today because `pos` on an element always reads undef
     (#396) and `tie $a[0],…` is unimplemented (#155).
  2. **The empty slice, [perl #29127]** — `slice_h_acc` and `kv_slice_a_acc`
     accepted a one-child (no-subscript) node, `slice_a_acc` and
     `kv_slice_h_acc` demanded a subscript and therefore fell through to the
     SCALAR `delete`, which **crashed on arity**: `delete @a[()]` gave
     "invalid number of arguments: 1" where perl gives undef.  One rule now:
     a slice needs only its container.
  3. **The runtime half of the same rule** — only `p-delete-hash-slice` had
     the empty-slice → nil answer; the other three returned an empty vector,
     i.e. **0** in scalar context.  All four now answer nil, and in the two
     array functions the emptiness test runs BEFORE the read-only check,
     because perl allows `delete @ro[()]` and dies only on `delete @ro[0]`
     (probed).  `docs/ir-spec.md` gained the slice-delete row.
- **A compiler warning is a TEST FAILURE, on a cold cache**: the first cut of
  the arms compared an undef `$kind`, and `Pl/t/misc-fixes-01.t` t47 went red
  — `use Data::Dump` transpiles the module INSIDE the running SBCL, so a
  perl warning from ExprToCL lands in the PROGRAM's stdout (the same
  mechanism as the s402 drop announcement).  It reproduces only cold, so
  **clear `~/.pcl-cache` before believing a green gate on a compiler
  change**, and scan the corpus transpile's stderr for new warnings.
- **New tasks filed by these probes: #396** (`pos()` on an array/hash element
  is ALWAYS undef — `=~` binds the element's VALUE while `pos`/`tied` bind
  its BOX, so `/g` position is recorded under the string object and read
  under the box; diagnosed at the emission and the runtime, fix not
  attempted) and **#397** (above, fixed here).
- **The EXTRACT worklist's remaining in-scope families are DONE** (`s414c`,
  `s414d`, `s414e`): Parser.pm's six `Pl::PExpr->new … Pl::ExprToCL->new`
  pairs → `_expr_parser`/`_expr_generator` (14+26); InterpScan's FOUR (not
  two) "read the name after the sigil" copies → `_name_event` (38); and the
  `&`-mention family of `exists`/`defined`/`undef` → `_amp_sub_name` plus the
  existing `_amp_cast_operand_id` that only `undef` had been using.  Each
  corpus-diff IDENTICAL, emission-ab 23/23 SAME, gate green.
- **`p-defined-fh` resolved filehandles ITS OWN WAY** — a raw `(gethash sym
  *p-filehandles*)` — and so hit the `eq` miss that `p-get-stream`'s by-name
  fallback exists for: the standard handles are keyed by the `:pcl` symbols
  while generated code in a user package passes that package's own same-named
  symbol.  **`defined STDIN` read FALSE.**  Fixed by going through
  `p-get-stream` (`s414e`), the defun moved below it so no forward reference
  is added (load-warning set unchanged at 48).  Residue **#398**: perl's
  `defined BAREWORD` is not an openness test at all — without strict it is
  `defined "NAME"` = true, under strict the bareword is a COMPILE ERROR, and
  with a sub of that name it is a CALL — so a closed or never-opened handle
  still diverges.  That one is a decision, not a patch.
- **THE TAIL RULE IS SATISFIED — stop extracting.**  Re-running
  `tools/dup-census.pl --min-lines 8` over `Pl/**` + `cl/pcl-runtime.lisp`
  leaves SIX EXACT runs and every one is in code with a standing verdict:
  four in `Pl/Parser.pm`'s `_process_block` / `_process_block_in_tail_context`
  / `_process_scheduled_block` / `_process_local_declaration` /
  `_process_tail_stmt` (LEAVE — DELETED-BY E5.3) and two in
  `StringInterpolation.pm`'s subscript walkers (SUPERSEDED-BY the InterpScan
  consumer-3 port, #388).  Everything else is SHAPE-only.  The queue moves to
  `docs/plan-post-s408.md` §2 (I = #281 items 1+2+6 → J–L Option B phase 2 →
  M–N release).
- **#281 items 1+2+6 are on branch `s414-ir-macros` (`7e56c4f`), NOT merged** —
  complete and gate-green (150 files / 5522 rows, only the 13 pclxs xs rows),
  with `tools/emission-normalize.pl --corpus HEAD` reporting **identical after
  normalization across 111 files**, but the FULL SWEEP, the pack-artifact
  re-check, the bench and the `docs/ir-spec.md` update are still to run.  The
  commit message carries that bar.  Next session: run the four, then
  `git merge --ff-only`.
- **A RENAME OF AN EMITTED SHAPE IS NOT COSMETIC — grep for the consumers that
  pattern-match it.**  Renaming `(let ((*wantarray* …)) …)` to the `p-…-ctx`
  macros broke THREE matchers: `Parser2::_auto_defined_call` +
  `Parser::_auto_defined_cond` (perl's implicit `defined()` around
  `while (my $l = <FH>)` — unseen, the loop stops on a "0" line), the runtime's
  `%p-fh-arg` (unseen, `binmode(NOPE)` CALLS `pl-NOPE`, undefined function) and
  the runtime's `p-list-=` undef-placeholder test.  The first was caught by the
  corpus A/B and the second by the gate, neither by reading the diff.  Both
  runtime matchers now share ONE `%p-strip-ctx`.
- **The duplicate-declaration key is (PACKAGE, NAME), never the text**
  (#281 item 2): `in-package` is READ-time per top-level form, so sort.t's ten
  `(defvar $a …)` lines are ten DIFFERENT symbols and no duplicates at all.
  The s407 `dupdv` counter measured text; a text-level fix would have been a
  silent wrong.  The real repeats — one name, one package, several sections —
  are 16 in sort.t, 4 in hash.t, 2 in closure.t.
- **Regenerating an artifact needs `tools/tag-license` after it**: `pl2cl
  --extension` does not emit the license header `Pl/t/license-tag-01.t`
  requires, so a regeneration without it turns one red row into two.  Written
  into `Pl/t/artifact-staleness-01.t`'s recipe.

## s413 (2026-08-18, Fable) — the first EXTRACT batch of the duplicate worklist (#387); scope = compiler + runtime (USER); three silent-wrongs found by the family reviews; handoff to Opus

- **USER RULING (s413): duplicated-code extraction matters ONLY for the
  compiler and the runtime** — `Pl/**` + `cl/pcl-runtime.lisp` are the
  product; `tools/**` and the two runner scripts are scaffolding that may be
  replaced and are OUT OF SCOPE; test files are never an optimization target
  (guard rows are only ADDED).  `docs/dup-census-worklist-s411.md` §1 rule 7;
  memory `feedback_dup_census_compiler_only`.  Family 6 (`s413a`,
  `tools/lib/PCLProc.pm`) landed before the ruling and stays.
- **Families 5, 34, 3, 4, 8, 17+20, 44 landed on `main` (`s413b`–`s413i`)**,
  each `tools/corpus-diff.pl` IDENTICAL over 111 files (drops 13 = census)
  and gate-green (150 files, only the 13 pclxs xs rows): ONE my/state
  declaration walk in Parser2 (`_decl_syms_under`, eight copies → one, the
  walk COUNT went down), the five brace predicates one-liners AND called as
  functions from PExpr (the Moo accessor + dispatch was the only call layer
  on the hottest predicate; compile A/B 57.2/57.9 s vs 57.4/57.9 s =
  neutral), the postfix-`->` loop's mechanics (`add_child_flattening`,
  `_kv_slice_node`, `_reduce_pre`, `_prefix_op_node`, `_take_rest_as_args` —
  node-id allocation order preserved at every site, which is what makes the
  emission identical rather than merely equivalent), ExprToCL's
  `_slice_index_forms` / `_hash_key_form` / `_decode_escape`.
- **Families 23, 13, 21, 37, 42, 46–48 (the runtime) are on branch
  `s413-lisp-dedup` (`s413j`–`s413o`)**, verified by Fable (parens; the tip
  loads with HEAD's exact warning set; macroexpansions and the two `declaim
  inline`s inspected in the disassembly) — **gate + bench A/B + full sweep on
  the branch tip and the fast-forward merge are Opus's task #395**
  (`docs/opus5-handoff-s413.md` §2).  Runtime rows added to
  `tools/bench-exec.pl` (arrfill, slices, sliceasgn, ovlsub, symref).
- **The census is a BUG FINDER: when copies of one rule differ, probe the
  difference against perl before unifying — it is a reason for two copies or
  a bug, never noise.**  Three found today, each "the sibling that spelled
  it differently": **#393** `"\b"` in dq context was `b` (perl: backspace;
  tr had the arm) — FIXED `s413h`, guard row transpile-test-10.t, whole
  escape table (17 rows) probed identical to perl before and after; **#394**
  `@a["12"]` exploded a STRING index into characters and `delete @a[1..2]` /
  `delete @a[@i]` / `delete %a[1..2]` deleted element 0 (the two array
  deletes did not flatten at all) — FIXED on the branch (`s413l`, ONE
  `%p-flatten-slice-args`, 8 users), guard row on the branch.
- **One full sweep covers a batch when every other commit is corpus-diff
  IDENTICAL** (a mover bisects to the one non-identical commit); it ran at
  the batch's end over `main` (verdict in the session log).  **An
  interrupted tool call may have RUN to completion** — check `git log
  --stat` before re-running a commit command (`s413g` was doubled once and
  soft-reset).
- **Handoff:** `docs/opus5-handoff-s413.md` — §2 the branch verification
  recipe, §3 the queue (remaining in-scope families 2+10, 14/26, 38 →
  `docs/plan-post-s408.md` §2: #281 items 1+2+6 → Option B phase 2 with
  Fable's operand grammar first → release), §4 the rules above.

## s412 (2026-08-18, Fable) — Phase B DONE: ONE seam function; the hook always answers; analysis parses compile nothing

- **Phase B1 DONE (`s412a`): PExpr `analysis_only => 1`** — an ANALYSIS parse
  (VarAnnotator's tree walk, Parser2's `_expr_scalar_rooted`) builds its
  embedded-block nodes with NO body: no structural lowering, no v1 text
  compile — the ~900 discarded block compiles per corpus are gone, and with
  them the third copy of the bucket dance (VarAnnotator's save/redirect/
  restore).  A body-less lambda/func_ref node reaching an emitter DIES.
  Corpus-diff: ONE comment line (hash.t — the discarded compile used to
  REWRITE the shared token `$h{k}`→`$h{"k"}` and leak it into a source
  echo).  Measured cut ≈ 1 % of compile time (the discarded compiles were
  small) — NOT the plan's expected larger cut; see the next line.
- **The plan's `_process_element 11.2 s` IS the module PROTOTYPE PRE-SCAN
  (task #391)**: `Pl::Parser2::_premerge_include_prototypes` →
  `_extract_module_prototypes` / `_extract_file_prototypes` runs v1's
  WHOLE-FILE `parse()` (`collect_prototypes_only`) over every use'd module
  and `require`d file, transitively, per pl2cl process — 13.1 s of a 50 s
  12-file sample (26 %); the seam (`_fallback_stmt_capture`) is 0.9 s.  Its
  consumed output is `prototypes` + `export_names` only.  After B1 every
  remaining `decl:no-hook` census event (162/corpus) is under it.  Fix
  shape = a facts-only PPI walk; it is also the LAST live entry into v1's
  file-level `parse()`.  Filed, not done (structural first).
- **Phase B2 DONE (`s412b`): `Pl::Parser::capture_v1($code, %opt)` is THE
  seam function** — saves the six emission-state fields, fresh scratch
  section, bucket / block_depth / hook installed, runs, drains by bucket
  name, restores, returns `{result, runtime, decls, defs, opens}`;
  `become_seam($owner)` sets the seam's standing state.  Parser2 reads NONE
  of `_sections/_cur_bucket/indent_level/_local_let_depth/_block_depth/
  _open_section` any more (grep = 0).  IDENTICAL.
- **Phase B3 DONE (`s412b`): the embedded-block hook ALWAYS ANSWERS.**
  `Pl::Parser::embed_block($block, $kind)` = the one thing PExpr's five block
  sites ask (hook when installed, else v1's own text — `embed_block_v1`);
  `Parser2::lower_embedded_block` = dispatcher → structural (`_lower_embedded_
  body` / `_lower_embedded_anon`, undef = decline) → `_embed_via_v1` (v1's
  text under its own `capture_v1`, drained hoists → `_captured_decls`, ONE
  place).  Kinds map/grep/sort/eval answer BODY forms, do/sub the whole
  LAMBDA form.  `body_cl`/`raw_lambda` are dead as body carriers; ExprToCL's
  text-body branches deleted.  `_lower_expr` has NO capture, and
  `Parser2::parse` ends with `assert_seam_clean` (rule 12: the seam's
  standing section must be empty — emission nobody drained is lost output).
  IDENTICAL; gate-SET scan over both populations: zero verdicts moved.
- **The assertion found the ONE out-of-capture emission on its first run**:
  Parser2's sub PRE-REGISTRATION calls v1's `parse_prototype_or_signature`
  for facts, and `_parse_signature` EMITS (an `our` in a signature default
  declares its cell) — silently lost, harmless only because the v1-routed
  sub statement emits it again.  Now inside `capture_v1` with the drain
  deliberately DISCARDED + a comment: the one documented discard in Parser2,
  gone when E5.3 ports sub-with-signature.
- **Hash-constructor route = map/grep ONLY (E2 silent-wrong fixed in place,
  rule 7.1)**: `{ WORD => …` after eval/do/sub is a BLOCK whose statement is
  a LIST (perl: `sub { a => 1 }` returns (a, 1); `do { b => 2 }` is 2;
  `eval { k => 1 }` is (k, 1)); PCL emitted a garbage lambda (crash) / a
  HASH ref since the E2 hook applied `_block_is_hash_constructor` to every
  kind.  Guard row transpile-test-10.t.  perl reads the braces as an anon
  hash only after map/grep (the `map({k => $_}, LIST)` paren form).
- **Phase C DONE (`s412d`): the embedded-block decline shapes 17 → 3 per
  corpus.**  (1) `Pl::CLForm::to_flat` renders a `raw_wrap` (open text +
  body forms + its closers, comment-safe) and `embed_unsafe` accepts one
  whose body is safe — a v1 `local` inside an eval/do/sub body no longer
  sends the whole body to v1's text route (10 of the 17).  (2) A tail
  `PPI::Statement::Include` converts: `_lower_block` routes it through
  `_fallback_stmt`, whose runtime raw IS the tail value (`eval { require X }`
  = require's value / undef + `$@`; a `use`/`no` tail emits no runtime form
  → the previous statement's value, as perl in scalar context; #392 filed
  for perl's EMPTY list-context value there).  Left declining (kept in
  `_embed_via_v1`): a `package` statement inside the block (2, caller.t), a
  non-convertible tail declaration (1, local.t).
- **v1's top-level-`local` inlining cap keyed on `indent_level == 0` alone
  MISFIRED under the seam** (every v1-routed statement lowers at indent 0):
  a `local` in a file-level LOOP body got `(locally (declare (notinline …)))`
  around the rest of the body — the fast-path inlining suppressed in exactly
  the hot code the discriminator meant to exclude (found when Phase C routed
  `eval { local … }` bodies there).  `_block_depth == 0` (the seam's
  real-nesting fact) joined the conjunction: a true file-level `local` keeps
  the cap, a nested one does not.  This is why ~40 corpus files changed;
  `tools/emission-normalize.pl --rule notinline-locally` (new) proves the
  rest identical except the four files whose bodies now go structural
  (their diffs are the structural route's known shapes: `foreach-range-raw`,
  the void regime, `(vector …)`-less single-element foreach lists, an
  unqualified cell spelling inside its own section).  Guard rows
  parser2-02.t (shape) + transpile-test-10.t (semantics, perl-probed).
- **#391 DONE (`s412f`): the module prototype pre-scan is a FACTS WALK** —
  `Pl::Parser::collect_prototypes($doc)` visits the document in order and
  reads only sub HEADS (`_sub_head` / `_sub_sig_info` /
  `_register_sub_prototype`, the same three helpers `_process_sub_statement`
  now uses — one copy), `PPI::Statement::Include` (v1's own include
  handler: `use constant`, `use lib`, `use Module LIST` extract+merge,
  `require "file"`), and recurses into every other node (BEGIN blocks,
  compounds, anon-sub bodies).  Both extractors call it; ONE PPI parse per
  module (`ppi_doc`, the repaired document, also feeds the @EXPORT scan).
  **v1's file-level `parse()` is DELETED** (+ `_assemble_output`,
  `_insert_variable_forward_declarations`, the `output` attribute — 450
  lines) — nothing constructs a whole-file v1 emission any more.  Proof of
  identity: `PCL_PROTO_ORACLE=DIR` (kept: per-module JSON of `prototypes` +
  `export_names`) diffed old walk vs new over the 24 modules/files the
  corpus loads AND 95 modules (every lib/ shim, a 15-dist CPAN sample, core
  modules) — identical (only the oracle's own path field differs).  Bars:
  corpus-diff IDENTICAL, lib emission-ab SAME, gate, full sweep +0.
  **Compile time, measured the way the sweep runs it (`pl2cl FILE`, so
  `require './test.pl'` resolves): 81.0 s → 64.0 s (−21 %)** — the
  `pl2cl < file` loop the plan quoted was blind to the pre-scan (no filename
  → test.pl never found), which is why B1's cut looked like 1 %.
- **Two measurement traps recorded** (memory `project_pcl_measurement_traps`):
  corpus-diff transpiles via STDIN — module/file prototype facts from a
  relative `require` do not apply there (the sweep passes the filename); and
  `tail -N` on a gate output can hide the FIRST failing file — the Phase C
  gate (`s412d`) went in with clform-01.t row 7 red (a shape row pinning the
  deleted `to_flat` die), corrected in `s412f`; read gate summaries with
  `grep -a Wstat`.
- **Bash trap of the session**: a commit message written inline with
  backticks in a double-quoted string EXECUTES them (`local $p->{…}` ran as a
  shell command and left a file named `{_v2_embed}` in the tree).  Write
  commit messages to a file with a quoted heredoc and `git commit -F`.


## s411 (2026-08-18, Fable) — #379: the one-compiler plan; the duplicate-code census; structural first (USER)

- **STRUCTURAL FIRST, NOT AT ANY COST (USER 2026-08-18)** — the queue is
  `docs/plan-one-compiler-s411.md` §6 (Phase R registry → A one expression
  compiler → B one seam function → C the 12 decline shapes → the old queue
  resumes).  A newly found silent-wrong is FILED and jumps the queue only if
  it regresses a baseline or blocks a phase; a fix landing in code a phase
  deletes is wasted — check the deletion list first.  Fable sessions rule
  the asks and do structure; no cold re-verification of a gate Opus ran.
- **E5.1–E5.5 (9–17 sessions) REPLACED as the worklist by `plan-one-compiler-s411.md`
  (3–5 sessions to the release shape)** — MEASURED: the two generators'
  entire emission difference is TWO rules (`elem-setf`, `insensitive-call`;
  643 diff lines / 24 files with the native attempt forced off); the native
  attempt costs 9 % of compile time; `lower_embedded_block` declines 12 of
  1 064 embedded blocks; ~1 500 embedded blocks per corpus are compiled by
  v1 TEXT and DISCARDED (native attempt + analysis parses).  "One compiler"
  for v0.1 = one GENERATOR + one statement entry; zero v1 lines = E5.3 post-
  release, `local` (310/corpus) first.  → plan §1 (numbers), §2 (phases),
  §5 (what it deliberately does not do).
- **The optimization registry (`Pl/Passes.pm`, `PCL_OPT`) is Phase R, FIRST**
  — the USER's "flag to turn optimizations on/off so they can be done after
  the compiler is done"; Kind-A gates for the five existing transforms
  (`raw-numeric`, `str-buffer`, `foreach-range`, `insensitive-call`,
  `elem-setf`), empty Kind-B hooks; `PCL_NO_RAW_VERDICT` = alias of
  `-raw-numeric`.  → plan §2 Phase R; design `v2-target-architecture.md` §3.
- **Duplicate-code census = `tools/dup-census.pl` (families, EXACT/SHAPE,
  `--calls` hot tags) + `tools/sub-call-census.pl` (Devel::NYTProf per-sub
  calls/time)**; verdicts and order in `docs/dup-census-worklist-s411.md`
  (rules: DELETED-BY a phase → do not touch; SUPERSEDED-BY a port → do the
  port; hot code → collapse in place, never a new call layer; the Parser2
  `my`-scan family → the sweep is the gate).  462 clusters / ~4 300
  deletable lines at s411; v1 `_process_*` families are LEAVE.
- **StringInterpolation's subscript scanners (the #1 duplicate family, ~350
  lines) are InterpScan consumer 3** — schedule the port, not a helper.
  `Parser2::_interp_names` is 1.65 s EXCLUSIVE in a 50 s sample = consumer 2
  is also the compile-time fix.  → worklist rows 1, 27.
- **Phase R DONE s411 (Fable, same session): `Pl/Passes.pm` + `PCL_OPT`** — Kind-A
  `enabled(name)` gates `raw-slot` (the whole unboxable verdict, incl.
  `p-raw-params`), `raw-numeric` (`PCL_NO_RAW_VERDICT` = alias),
  `str-buffer`, `foreach-range`; Kind-B `register_pass`/`run` at the four
  `to_string` handoffs (identity until a pass exists); an unknown name in
  PCL_OPT DIES with the known list (`Parser2::parse` → `check_env`).
  `PCL_OPT=none` is the general-form compiler: `Pl/t/passes-01.t` (22 rows)
  pins that every name gates its shape and that the SAME program prints the
  SAME output under every setting.  **The bar's reading: under `none` every
  RUN row passes; transpile-SHAPE rows that assert a fast shape differ by
  definition** (list in the plan §2 Phase R).  → plan §2 Phase R; #383.
- **Phase A1–A3 DONE s411 (Fable, same session): ONE expression compiler.**
  `Pl/ExprToCL2.pm` DELETED; its two rules are Kind-A gates in ExprToCL
  (`insensitive-call` at the user-sub bind, `elem-setf` at the `=` element
  store — with a PURE-key predicate: the native rule let a call key through
  and CL setf evaluates the key before the value, perl the value first);
  `_lower_expr` parses ONCE (v1's `_parse_expression_form` carries
  `sub_info`/`lexicals` to ExprToCL); the discarded parse, snapshot/restore,
  string ctx encoding, `_expr_via_fallback` and the blame census are gone.
  **Verified with `tools/emission-normalize.pl` (NEW; the s410 7.7 (a)
  normalizer: reader → rewrite the expected shapes → flat print; `--corpus
  REF`)**: identical to HEAD after normalization except FOUR explained files
  — method.t (the native generator resolved `our $AUTOLOAD` inside a package
  BLOCK in MAIN: a package-resolution BUG the fold FIXES), ref.t/sort.t
  (calls spelled package-QUALIFIED now, v1's robust spelling), sub.t (three
  declarations no longer emitted twice by the discarded parse).  Sweep
  TOTAL 18513 (+0) clean; gen v2-156; corpus compile 68.4 → 55.1 s.  **A
  shape row that pinned a native SPELLING is updated with its reason, not
  the emission** (parser-01 7, parser2-02 46).  A4 (one DIALECT: v1's TEXT
  `->generate(` sites → forms; delete the text twins) is the next item.
- **Phase A4 DONE s411 (`s411d`): ONE DIALECT** — `generate`/`gen_node` = the
  flat print of `gen_node_form` (corpus-diff IDENTICAL: E2's dual-run promise
  held); the text emitters (`gen_binary_op` 295 lines, `gen_internal_node*`,
  `gen_inline_lambda`, `handlers`) DELETED, ExprToCL −430 lines; the two
  decline sites DIE (rule 12).  It exposed one broken emission: a NESTED
  subscript in a dq string (`"$_[$_[2]]"`, postfixderef.t) printed as
  `(p-aref @_ )` (run-time crash) — now an announced drop, census 12 → 13
  under the s409 rule, **#390** filed (InterpScan consumer 3).  Gate-SET scan
  over both populations: zero verdicts moved; sweep 18513 (+0).
- **`PCL_OPT=none` found a default-configuration silent-wrong on its first
  run (as predicted): `%p-flatten-list` snapshotted a MAGIC CELL / TIE PROXY
  instead of its value**, so `my ($x) = @_; $x = 0` (when the raw-params
  fast path did not apply — a closure captured `$x`) wrote THROUGH the
  defelem alias and VIVIFIED the caller's `$h{k}`, and a tied source made
  the target tied.  Fixed at the runtime (snapshot the getter's / FETCH's
  value, as `box-set` already did); guard row transpile-test-10.t; ir-spec
  §@_ aliasing "copying OUT of @_ is by VALUE".  Sweep clean.  Filed while
  probing: **#389** `++$$r` / `--$$r` / `++${$r}` is a whole-statement DROP
  in every position (announced; Option B phase 2 operand grammar).
- **s410 asks ruled short** (`docs/fable-answers-s410.md`): v1 fixes stay
  legal when reachable + small AND filed on the E5.3 class; `__SUB__` outside
  a sub keeps the die; #376 blast radius = follow perl; #377 twin registered;
  #381 behind Option B phase 2 (itself behind the plan); the LAST-commit gen
  bump is legal only when both commits land in one push; #281 item 1 = the
  normalizer (a), now also Phase A's bar; item 2 folds into item 1.

## s409 (2026-08-16, Fable) — the s408 review: approved as shipped; the census-increase rule; two lexsub-family tasks

- **s408 APPROVED as shipped (all seven code commits)** — gate re-verified COLD
  149/5442 (only the 13 pclxs xs rows), sweep RE-RUN clean TOTAL 18513, 27
  probes vs perl 5.40.3.  → `docs/fable-answers-s408.md`; queue →
  `docs/plan-post-s408.md`.
- **A census INCREASE is legal when it converts a WORSE failure into a counted
  drop** (a crash-form the census cannot see → an announced drop): the edit
  note names the form replaced + the owning task; the file's verdict must not
  regress; sweep TOTAL/LOST unchanged.  Never hold such a change until the drop
  is fixed — the census counts drops, it does not freeze them.  → answers §3.6.
- **A gate row count is compared against a measurement of the SAME tree**,
  never a number in a doc: `Pl/t/xs-01/02/03.t` PRODUCE 0–14 rows depending on
  where pclxs's current state aborts them.  Subtract the xs rows produced in
  each run when only a written number is at hand.  → answers §3.1.
- **An eval-mode drop DIES; "announce over the protocol and continue" is
  REJECTED** — it keeps the wrong VALUE (undef, `$@` empty) the program
  consumes (rule 12 value case).  A lost companion row asserting "no error"
  about code PCL cannot compile is not a cost (op/smartmatch.t 143 → 44: 99
  `~~` evals that never ran; they become #371's ruled refusal).  FILE mode's
  flip stays Option B phase 2's LAST step.  → answers §3.4.
- **A fragment mini-parse (`PPI::Document->new` on an interpolated span) is the
  established pattern** (28 sites); `_ppi_new` is the one construction site
  for FULL documents only.  → answers §3.3.
- **"One compiler" — the state, measured s409, and the USER's instruction.**
  One PIPELINE since s356; but 88.2 % of expressions (16,898 / 19,166,
  `tools/v2-census.pl`) still lower through v1's ExprToCL and 1,050
  statements/corpus through v1's statement layer — unchanged since s316t.
  Strings→forms is DONE (ExprToCL 41 `*_form`, 0 string); the complexity is
  ENTANGLEMENT (bucket/`_emit` drain around every fallback, embedded blocks →
  v1 statement layer, 12 statement classes, destructive parse = 88 % parsed
  TWICE).  **USER: "seems much too complex — make a plan to reorganize it,
  can it be done in a simpler way?" → NEXT Fable session, before executing
  anything.**  → task #379 (all numbers + the E5.4-first hypothesis),
  plan-post-s408 §3.0.  Not on the v0.1 critical path as planned.
- **USER (end of s409): a COMPREHENSIVE SEARCH FOR DOUBLED CODE** — "lots of
  small bits can be extracted to subs" — is part of the same next-Fable plan:
  a duplicate-code census (5–20-line copies, ranked by size × count) → a
  mechanical extraction worklist for Opus, corpus-diff IDENTICAL per step.
  CLAUDE.md 11 is the rule; this is its census.  → task #379.
- **#368's die is right, but a die that ABORTS a companion file is a COST that
  must be measured in the dir it touches** — op/sub.t 51/14 → 25/6 at its
  `sub {…; CORE::__SUB__->()}->()` (line 214), unmeasured in s408 (only
  op/current_sub.t was re-run).  Anon `__SUB__` is modern Perl's recursive
  closure, so it is IMPLEMENTED, not softened: **#378** = a self-reference
  rewrite at the PPI entry that already rewrites a named sub's `__SUB__`
  (`sub {…}` → `do { my $__SUB__N; $__SUB__N = sub {…$__SUB__N…}; $__SUB__N }`).
  → answers §2.3, snapshot row edited by hand.
- **A `--quick` NOT-RUN (or KILLED) row is never a MOVER** — #366's re-run
  phase re-ran all 11 NOT-RUN files ALONE at their full allowances (+23 min)
  and overwrote the NOT-RUN rows with serial verdicts, un-quicking the report;
  fixed s409, and the re-run label now says which of the three values agree.
  → `tools/run-perl-suite.pl` `rerun_movers_serially`.
- **#337's "latest start wins" needs no shadow walk** — confirmed (region =
  [decl, end of scope] clipped at a same-scope sibling; candidates restricted
  to enclosing scopes).  Its residue is #376 (fwd-decl `my sub c; sub c {…}`
  SILENT WRONG across two scopes; `sub NAME` inside the region defines the
  LEXICAL in perl; cross-package use crashes) and #377 (`my $x = shift` +
  nested sub reading it → unbound `$x__file__0`: p-raw-params binds the
  promoted name lexically and no cell is emitted) — both PRE-EXISTING at
  b7ce704, both head the next Opus session.  → answers §2.
- **#374 half (b) is POSITION-AWARE renaming, not `TERM TERM TERM`**: `my $x =
  if if if` deparses to `(my $x = if()) if if()` — the middle `if` is the
  modifier keyword.  Fix is in the renamer's classification; exotic; behind
  Option B phase 2.  → task #374, answers §2.3.


## s408 (2026-08-16, Opus 5) — an eval-mode DROP dies (#363); `qq {…}` (#375)

- **In EVAL-STRING mode a #138-family drop DIES into `$@`** (#363, as ruled):
  perl's contract for `eval STRING` is "what does not compile sets `$@`", and
  nothing else can say so there — the runtime starts `pl2cl --server` with
  `:error nil`, so the announcement goes NOWHERE and the statement simply
  disappears from the program.  FILE mode is untouched (announce, rc 0) until
  Option B phase 2.  → `Pl::Parser::_announce_dropped_statement`,
  `Pl/t/eval-01.t` (3 rows incl. the file-mode inverse).
- **Measured cost: 4 rows net.**  −3 dor.t, −2 postfixderef.t, −1 yadayada.t —
  every one a `'… compiles'` assertion (`$@ eq ''`) about code PCL genuinely
  cannot lower, i.e. **rows that were passing only because the failure was
  silent**; +1 eval.t (an invalid construct in an eval now sets `$@` as perl
  does) and +1 magic.t (from #375).  Baselines edited row by row.
- **A loud failure in one place exposes a silent wrong in another.**  The first
  flip measurement said −66, of which −61 was `index.t` alone: its own `die $@
  if $@` aborted the file because an eval'd assertion was malformed — by #375,
  a bug that had been quietly producing wrong STRINGS.  The −61 was never the
  flip's cost.  **Do not accept a big regression number at face value; find the
  file's own mechanism first.**
- **#375: `qq {…}` with WHITESPACE before the delimiter kept the delimiters in
  the VALUE** (`qq {interp $x}` → `"{interp V}"`).  PCL took the character right
  after `qq` — the space — as the delimiter; **PPI's `->string` has always been
  right, and asking it is the fix** (CLAUDE.md 11).  Only the interpolating form
  was wrong, which is why it survived.  Three corpus files carried wrong
  strings.  → `Pl/t/string-interp-01.t`.
- **The population instrument earns its keep, and so does reading the shapes.**
  `PCL_EVAL_DROP_LOG` counted 48 events / 34 shapes (perl-tests) and 548 / 446
  (perl's t/) before the flip; the shapes said "mostly deliberately-invalid
  Perl", which predicted small honest movement — and the two files that moved
  most were the two the shapes did NOT explain.

## s408 (2026-08-16, Opus 5) — lexical subs are LEXICALS (#337); the feature pragmas (#360)

- **The core feature-enabling pragmas are answered by a TABLE in PCL, through
  PPI's own hook** (`custom_feature_include_cb`, consulted before PPI's built-in
  logic): `Pl::Parser::_pcl_feature_include_cb` handles `use feature`, `use
  experimental` and the `use vN` / `use N.NNN` BUNDLES, plus every `no` form.
  PPI knew only `use feature`, so `use v5.40; try {…}` was a whole-statement
  DROP and `use experimental 'try'` did not compile.  A feature-enabling core
  pragma is LANGUAGE, not module behaviour (9a) — and the mechanism is a table,
  never a source rewrite.  → task #360, `Pl/t/feature-pragma-01.t`,
  `ppi-upstream-bugs.md` §20 + Bug 17 in the report.
- **The bundle thresholds are STATIC but re-derived from the running perl by a
  guard row** — PCL must compile `use v5.40; try` the same way whatever perl it
  runs under, so the table cannot be read from `%feature::feature_bundle` at
  runtime; a test that re-derives it turns a drift into a red row instead.
  (`try` is in bundles 5.39/5.40, `signatures` from 5.35 — perl's own answer.)
- **A version bundle REPLACES the scope's feature set**, so the table's "off"
  answers are explicit: `use feature 'try'; use v5.36;` leaves try OFF, as perl
  does.  And an argument the table does not model returns an EMPTY answer
  rather than falling through — PPI's `experimental` branch answers
  `signatures => 0` for any list, which would silently disable signatures.
- **`lib/experimental.pm` is a PCL shim** with a delete-when trigger: the real
  module dies at load because `for values %h` does not alias (E5 axis).  The
  trigger is a guard row that FAILS when aliasing lands.
- **A string eval INHERITS the feature pragmas in effect at its site** (#364),
  and the site is the only thing that knows: PCL compiles the eval text in the
  `pl2cl --server` subprocess on the bare string.  The features ride the server
  request next to `eval_captures` (a fifth protocol line), seed PPI's lexer via
  the document's `feature_mods`, and **join the eval cache key** — same text
  under two feature scopes must not share an entry (guard rows, both orders).
  The site's own answer comes from PPI's `->presumed_features`, which is exact
  and lexical (a `no feature 'try'` in an inner block turns it off there and
  back on after) now that the #360 table teaches it every spelling.
- **The per-statement publication hook is `_lower_block`, not `_lower_stmt`** —
  the declaration paths (`my $r = eval "…"`, the commonest eval statement there
  is) never reach `_lower_stmt`.  Found by tracing, after the first attempt
  silently published nothing.
- **`lex_home` IS the Parser2 object**, so a key published there shares a
  namespace with Parser2's own fields — a same-named per-document map and
  per-statement value collided and read as "Not an ARRAY reference".

## s408 (2026-08-16, Opus 5) — lexical subs are LEXICALS (#337, session F)

- **`my sub NAME` / `state sub NAME` get a scope-unique name
  (`NAME__lexsub__N`) and the uses their region owns are rewritten** —
  `Pl::Parser2::_rename_lexical_subs`, run in `parse()` before every name-keyed
  pass.  The region runs from the DECLARATION (a call before it still reaches
  the package sub) to the end of the enclosing block, and stops at a sibling
  redeclaration (#296-B2).  Nesting needs no shadow test: the covering
  declaration with the LATEST start wins, which is the innermost one in scope.
  → task #337, `Pl/t/lexical-sub-01.t` (18 oracle rows).
- **A rename that only rewrites the TOKEN STREAM is incomplete: interpolated
  code (`"@{[ f() ]}"`, heredocs, patterns) is compiled from the string's
  TEXT.**  Found by a probe — the pass itself caused "the function main::pl-f
  is undefined".  The spans come from `Pl::InterpScan` (standing rule §8) and
  the code inside one is classified by parsing it as Perl, through the SAME
  predicate as the token stream, never by matching the name in text.
  → `_fix_lexsub_interp`, guard rows 16/17 of the new file.
- **Three divergences REGISTERED, not fixed** (`docs/not-supported.md`): a
  string eval cannot see a lexical sub (loud, and it "worked" before only
  because every lexical sub WAS a package sub — the same accident as the bug);
  a body's call to its own name is accepted where perl rejects it (principle
  9); a lexical sub in a loop body cannot be a fresh closure per iteration
  (#337 shape 10 = #347's "will not stay shared" family).
- **A gate ROW-COUNT difference against a written-down number is not a finding
  until it is attributed** — the docs said 147/5355, this session measured
  147/5359, and a per-file worktree compare at HEAD showed the two trees
  IDENTICAL file by file.  The pclxs xs files abort at different points as
  pclxs is worked on, so their row counts move on their own.  Compare a
  measurement against a measurement OF THE SAME TREE, never against a number
  in a doc.  *(Shape asked for confirmation, `opus5-review-requests-s408.md` §8.)*
- **op/const-optree.t is REGISTERED at last** (XDIFF, rows blessed): #337 fixed
  the 4 `retval of my sub …` rows, and a fresh per-row read of the 58 that
  remain (28 inlinable + 25 `:method` readouts, 5 RT 134138) met the
  all-or-nothing bar with two cited reasons.  It is the worked example of why
  that bar is per-row: the s397 ruling had authorised the registration, and the
  per-row read is what found a real fix target inside it.
- **The drop census took its first INCREASE, deliberately** — `t/op/lexsub.t`
  6 → 10.  A lexical sub named after a KEYWORD (`state sub if() {44}; my $x =
  if if if`) is renamed, so the statement stops being a keyword parse and
  becomes a term-grammar drop; what it emitted before is a **zero-argument
  `(p-if)` whose macroexpansion error is that file's crash cause**.  Four
  crash-forms → four counted drops, argued in the census header, residue =
  task #374.  *(Ruling asked: §7 of the review request — is "census up with the
  trade argued" the right shape when a change converts crash-forms to drops?)*

## s407 (2026-08-16, Fable) — the s404 + s405 + s406 batch review: `docs/fable-answers-s406.md`

- **All three sessions APPROVED as shipped**; three requests were pending (s404's
  was never answered either).  → `fable-answers-s406.md` §0.
- **A Word after `->` is a METHOD NAME and ENDS A TERM — every token-stream
  repair must know it.**  `$o->name x 3` (repaired to `+x(3)`, crash: #361
  regression), `$o->w / $o->h / 2` (dropped: #351 regression) and `$o->w*w()`
  (dropped: #354's hole) were one family; `Parser2::_is_method_name_word` is
  the one predicate, consulted by `_ends_term` and both Word repairs.  The `x`
  repair ALSO fires only when the document DECLARES a `sub x`
  (`_document_declares_sub`, from the same token walk as the term words).
  → answers §2, `Pl/t/bareword-call-01.t`.
- **A token-stream repair needs BOTH lists in its commit: where it fires, and
  where it must NOT (over the term forms: method call, subscript, `)`, quote,
  number, declared term).**  The 17-probe / 28-site scans of #351/#361 were all
  the first list.  → answers §4.6.
- **#362 CLOSED — cause corrected: NOT `\&NAME` identity.**  `p-backslash-sub`
  returns the same function object every time; the compared-only side was
  type-flow-frozen to a RAW numeric slot and `%to-number-raw` had no `functionp`
  arm (a rule-12 `(t 0)` swallow, the second-copy shape).  A code ref is the ONE
  reference that is a raw function, not a wrapper box.  **A "SILENT WRONG" task
  must carry the EMISSION of its reproducer** — the `%pcl-to-number-strict`
  line was the whole diagnosis.  → answers §3, `Pl/t/ref-identity-01.t` t21/22.
- **The #351 "not a term" substitution is RATIFIED** (perl's negative rule +
  principle 9), with the method-name amendment above; the imported lowercase
  `()`-sub residue (`use Math::Trig; pi / 2 + pi / 4`) is accepted and is
  #365's (the #266 classifier at the operator-loop term site).  → §4.6, §5.4.
- **The try/catch `$@`/finally model is NORMATIVE in ir-spec §6.3** (confirmed;
  six extra shapes probed identical).  op/try.t stays DIFF.  #347's die→
  registered-divergence trade UPHELD.  → §4.7.
- **Feature-enabling CORE pragmas are LANGUAGE, and PPI has the hook**:
  `_ppi_parse` passes `custom_feature_include_cb` answering `use feature` /
  `use experimental` / the `use vN` bundles (**`use v5.40` enables `try` —
  today a whole-statement DROP**; `use v5.36` does not).  `experimental.pm`'s
  own death (`for values %h` does not alias) gets a thin `lib/experimental.pm`
  shim.  → #360 (widened), answers §4.7 ask 3, §5.1.
- **A DROP inside a string eval DIES (perl-shaped, into `$@`)** — the eval
  server's stderr is `:error nil`, so announcing is impossible there; perl's
  contract for `eval STRING` is "what does not compile sets `$@`"; it is Option
  B phase 2's last step taken early for the one path that cannot announce.
  Population first (s373 three legs).  → #363, answers §5.2.
- **String-eval FEATURE inheritance**: the eval site's `presumed_features`
  rides the server request beside `eval_captures`, keys the eval cache (s387),
  reaches `PPI::Document->new(…, feature_mods => …)`.  → #364.
- **anon-sub `__SUB__` returning a no-op lambda is a value-producing missing
  case → DIE** (s329 boundary), measure op/current_sub.t first.  → #368.
- **The companion RUNNER re-runs a moved row alone** (the #215 shape; capped,
  both values reported, serial = verdict).  → #366.  **Both runners kill the
  process GROUP on TIMEOUT** (a spinning grandchild burned a core for an
  hour, s405 §1.5).  → #367.
- **`re/speed.t` NOT registered (a hang, not slow); NOT-RUN rather than a
  smaller budget; `--extension` its own flag; #350 flip-in-session with the
  measurement in the commit; the `parser2-01.t` rewrite meets the four
  conjuncts.**  → §4.1.
- **#337 split CONFIRMED**: rename half (shapes 1/2/3/12) = session F, sweep
  as gate; shape 10 joins #347's registered "will not stay shared" family
  (per-call cell for a hoisted sub), sized separately.  → §4.8 ask 4.
- **#359 behind the release; the fd-3 hole ANNOUNCES**.  Installer shape
  stands as shipped.  crlf_through OK = (a).  Scratch `local::lib` of dev
  modules is inside the standing permission (recipe: runbook "Leak hunting").
- **`Pl/t/parser-leak-01.t` scans the COMPILER only** — lib/ shims run under
  SBCL (cycles collected) and `__SUB__` is a no-op stub there.
- **Option B phase 2 is SIZED from the census TEXT, and it is mostly NOT
  parser work** (`docs/option-b-phase2-plan.md`): of 373 drops (73 files,
  `tools/drop-census.pl` at e79f0a6 = the edited baseline exactly), ~300 are
  FEATURE ABSENCES / deliberate error tests (given/when ~117, lvalue-sub ~41
  (ruled), class ~25, hexfloat ~20, unicode stash names ~16, defer 13,
  format ~9, `~~` infix 5, indirect object 4, `__SUB__` 4), ~40 are the term
  grammar (stacked filetests ~27, the #343 shape ~4, singles), ~15 lexer
  bugs / gaps (`qx{}` delimiters DROPPED #369, term-initial `~~` #370).
  Tracks: **A #371** refusals at the drop site (no parser risk) → **B1 #372**
  (a named unary's operand may BEGIN with a named unary; A/B by the fold
  recipe) → **B2 #343** → fillers → re-census → the announce→DIE flip at
  ≤ ~30 all-explained.  **Do NOT rewrite `parse()`'s main loop for this** —
  the maze may stay a corridor; B3 deletes only what is then unreachable.
  Recipe: `tools/drop-census.pl` (counts) + **`tools/drop-harvest.pl`** (the
  TEXT — new s407).


## s406 (2026-08-16, Opus 5) — #348 lands, one gate transpile helper (#355), the compiler's own leak (#128)

- **A self-referential closure (`my $w; $w = sub { … $w->(…) … }`) is BANNED in
  the compiler — use `__SUB__`.**  It is a reference cycle: the CV holds the
  variable that holds the CV, so perl frees neither it nor its pad.  One such
  walker in `Pl/Parser2.pm::_seam_lex_assign_fix` leaked ~8.5 kB per transpile
  of a 50-character snippet (~150 kB for a 1.4 kB source, linear, no plateau) —
  which IS task #128's "6 GB after ~1400 eval requests" in a long-lived
  `pl2cl --server`.  Guard: `Pl/t/parser-leak-01.t` (the shape across `Pl/` and
  `pl2cl` — NOT `lib/`, s407 — plus a 300-transpile RSS bound).  → `session-log.md` s406.
- **A gate file NEVER folds transpile stderr into the `.lisp` it loads** — it
  calls `PCLCore::transpile($cmd)`, which captures stderr separately and JUDGES
  it (a `PCL: statement dropped` line or a nonzero exit FAILS the row; other
  stderr is a diag).  A row that ASSERTS on the transpiler's diagnostics uses
  `PCLCore::transpile_raw` instead.  → task #355, `Pl/t/PCLCore.pm`.
- **A companion-suite row that moved is NOT a finding until it has been re-run
  ALONE.**  Measured s406: an `--all --quick --jobs 4` run differed from the
  snapshot in 36 rows, and 22 of them were contention — each reproduced the
  snapshot exactly when run by itself.  Files that spawn fresh_perl/runperl
  children lose rows under load, and #348 makes the run busier.
  → `perl-suite-run.tsv` s406 note.
- **`which_perl`'s children run PCL (#348 LANDED)** — see the entry under
  "Test / triage infrastructure"; zero rows moved in either population, and the
  one companion row that did (io/crlf_through.t → OK) moved because both ends
  now share PCL's `:crlf` gap, not because anything was fixed (#139).
- **A bareword before an operator is a TERM only if it is DECLARED one** —
  `use constant FOO => …` / `sub FOO () {…}` make FOO an operand
  (`print FOO x 3`, `print FOO . "b"`); an undeclared ALL-CAPS word after
  `print` is a FILEHANDLE, and a handle is not an operand
  (`print STDOUT x(), …` calls `x`).  Two predicates, one copy each:
  `PExpr::_is_zero_arg_func` (the print-filehandle decision, shared with the
  bareword branch of `parse()`) and `Parser2::_word_is_declared_term`
  (`_word_is_term` minus the ALL-CAPS guess).  → task #361,
  `ppi-upstream-bugs.md` §19.

## s405 (2026-08-16, Opus 5) — the cloexec hang was an open() bug (#358 closes #346), and try/catch/finally (#340)

- **`open FH, "<&=N"` on a CLOSED descriptor must FAIL EBADF, and PCL's SPUN.**
  SBCL's `make-fd-stream` does not check the fd, so the stream came back fine
  and its first read retried EBADF forever — a hang, not an error.  One
  `fcntl(F_GETFD)` before the stream is built.  **This was #346's hang**:
  `t/run/cloexec.t` exists to have a child open an fd it was NOT given, so
  under a PCL child it spun (measured: TIMEOUT at 300 s with 2 rows → DIFF
  16/6, which is exactly its real-perl-child row).  → task #358, `Pl/t/fileio-02.t`.
- **A HANG is a diagnosis, not a verdict — take `/proc/<pid>/status` too.**
  The stalled child was state **R (running)**, not blocked, which is what said
  "retry loop" rather than "waiting on a descriptor" and pointed straight at
  the open.  `ls -l /proc/<pid>/fd` alone would have shown only that the s404l
  fd leak was gone.
- **perl 5.34's `try`/`catch`/`finally` is IMPLEMENTED** (`use feature 'try'`):
  one arm in `_lower_compound` + the `p-try` macro.  It is NOT `eval {}` with a
  different name — `return`/`last`/`next` belong to the enclosing sub/loop
  (nothing catches `:p-return`), `$@` is localized (empty inside try AND inside
  catch, restored BEFORE finally runs), the construct has a value in the
  caller's context, and catch fires on a FALSE exception.  op/try.t 0 → 23/28;
  the five that remain are four OTHER registered families.  → task #340,
  `Pl/t/try-catch-01.t`.
- **PPI leaves `finally {…}` out of the try statement, and the orphan swallows
  the next one** (ppi-upstream-bugs §18).  Repaired by terminating the orphan
  where perl does (a `;` on the finally block's brace) and joining it back in
  `_lower_block` — the route the unlabeled `continue` block already takes.
  **`use experimental 'try'` does not switch PPI's try support on at all**, so
  that spelling still does not parse (task #360, with the `for values %h`
  aliasing gap that also makes the module die at load).
- **Never edit the compiler while a measurement is running.**  A companion
  `--all --quick` run overlapped two Parser2 edits and reported 145 TRANSPILE
  files and three ref.t rows lost — all of it an artifact of the window in
  which `_repair_try_finally` was called but not yet defined.  The run was
  discarded and re-run.
- **#347 CLOSED — a PROMOTED lexical is legitimately captured by a hoisted
  named sub, and the hoist gate was the only scan that did not know it.**  The
  sibling scan (`_gate_file_lexical_captures`) has carried that exemption since
  W5 ("the hoisted sub and the in-place code share the one defvar'd box");
  `_hoist_nested_sub` lacked the identical `next`, so the promotion happened and
  the gate fired anyway — costing the WHOLE FILE, because the v1 fallback the
  die was written for is gone.  **op/closure.t under PCL children 235/27 →
  267/3 (its real-perl-child row), so with #358 both of #348's blockers are
  closed.**  Exactly one file in either population changes emission
  (`t/op/lexsub.t`, rc 2 → 0).
- **perl's "will not stay shared" family is a REGISTERED divergence, not a
  refusal** (`not-supported.md`): when the captured lexical is re-created per
  call or per iteration, perl's named sub keeps the FIRST instance and PCL's
  reads the shared cell (three measured shapes).  Five of six probed shapes
  match perl exactly, the two that do not never died in the first place, and a
  wrong answer here surfaces as a failing TAP row — while the refusal it
  replaced took every row of the file with it.
- **#277 SHIPPED — `tools/install-pcl`, and the installed layout is a
  CONTRACT.**  `$PREFIX/lib/pcl/` holds the runtime tree in its
  repo-RELATIVE shape (`pl2cl`, `runpcl`, `Pl/`, `lib/`, `cl/`, `tools/lib/`)
  because that arrangement IS the lookup mechanism (`dirname(abs_path($0))`,
  `FindBin::RealBin`); `Pl/t` is not part of an installation; `$PREFIX/bin`
  gets WRAPPERS, never symlinks (runpcl derives its root from `dirname($0)`).
  The core is compiled at install from the INSTALLED runtime, written to a temp
  name and renamed.  An existing tree is REPLACED, not merged (`--force`): a
  stale `lib/Foo.pm` shim is an @INC entry that shadows a core module.
  → `docs/release-plan-v0.1.md` phase 1, `tools/t/install-pcl.t`.
- **A saved core beside the tree (`<root>/pcl.core`, with `<root>/cl/<runtime>`)
  is used automatically** by `PCLSbcl`, under the same freshness test
  `PCL_TEST_CORE` gets.  The `/cl/` in the pattern is load-bearing — "two
  directories up" would couple the answer to whatever sits beside an arbitrary
  caller's path.  A CHECKOUT has no `pcl.core`, so no development runner's
  command line changes (verified with `PCL_SHOW_SBCL=1`, pinned by four rows in
  `tools/t/sbcl-prefix.t`).

## s404 (2026-08-15, Opus 5) — session B: the portfolio, the two @INC silent-wrongs, POD prototypes

- **`--quick` is the companion suite's default form** (`tools/run-perl-suite.pl
  --all --quick --jobs 4`): it does not run the measured HANG set or a file
  whose registered allowance exceeds 120 s, and each gets a NOT-RUN row naming
  the rule and cause (UNEXPLAINED, like QUARANTINE).  A capped file is not run
  at all rather than run on a smaller budget — a truncated TAP stream is a
  DIFFERENT measurement, not a cheaper one.  → task #345, CLAUDE.md.
- **Hang vs slow is a MEASUREMENT, and the two go to different registries.**
  Run the file at a LARGER budget (task #326's test): same rows ⇒ HANG ⇒
  `%QUICK_SKIP` in the runner; more rows ⇒ SLOW ⇒ `baselines/perl-suite-timeouts.tsv`,
  whose promise is "give it the time and it finishes".  Measured s404:
  re/overload.t and re/speed.t are hangs, re/pat_psycho.t is slow (completes at
  300 s) — so s400 §7.4 held for one of its two files.
- **An EXTENSION carries no program preamble** — `pl2cl --extension` (like
  `--module`, but diagnostics stay on because a developer runs it).  The three
  checked-in artifacts load INTO a running program, so a preamble RESET its
  @INC; `p-load-extension` now DIES (rule 12) on an extension that changes
  @INC / `*pcl-pl2cl-path*` / `*p-core-inc-dirs*`.  **#217 closes** and #277
  loses its regenerate-at-install item.  → task #349, `Pl/t/extension-preamble-01.t`.
- **`require` is a RUNTIME statement AT EVERY DEPTH** — only `use` is
  compile-time.  A file-top bareword `require` no longer hoists into the
  declarations bucket above a runtime `push @INC` (52 of 657 files' emission
  moves, one shape).  → task #350.
- **Prototype extraction: ask the CLASS, not the class NAME** — a
  `PPI::Token::Pod` in a block walked with `children` reached `find` (a Node
  method) and killed EVERY prototype of the module (Unicode::UCD's 13).
  → task #353.
- **Sizing (free, from the drop announcements over both populations): #351 = 6
  drops in 3 files, all in perl's t/, none in the sweep population; #354 = 0 in
  both populations** (CPAN-only).  So #351 stays the first filler after B
  rather than jumping ahead of #346.  → `docs/opus5-review-requests-s404.md` §8.
- **A `/` after a bareword is DIVISION only when the word is a TERM** — for
  anything else perl does not fall back to division, it is a SYNTAX ERROR, so
  "not a term" is a SAFE repair condition under principle 9.  Term = 0-ary
  builtin (the one arity table), `use constant`/`sub NAME ()` in the document,
  or the ALL-CAPS convention.  → task #351, `_repair_word_match`.
- **A `*` where a TERM has just ended is multiplication, never a glob** — `)`,
  `]`, a SUBSCRIPT-closing `}`, a Symbol or a Quote; a BLOCK-closing `}` is the
  real-glob case (`sub f {…} *bar = \&f`).  → task #354, `_repair_glob_multiply`.
- **PPI's tokenization depends on `$/`**: with the slurp separator in force a
  trailing `__END__`/`__DATA__` section gains a newline.  Scope every slurp AND
  do not trust the parse — `_ppi_parse` trims a tail the parse invented.
  → `docs/ppi-upstream-bugs.md` §13.

## s403 (2026-08-15, Fable) — the s402 review: approved, two rules, #354/#355 filed

- **s402 APPROVED as shipped (quick ruling by USER instruction — NOT
  independently re-verified this time)** — ASCII `--` separator, per-(file,
  line, text) dedupe, `--module` OFF, the runner column, ir-spec §9.2 with the
  discovered-by-stamp sentence KEPT.  → `docs/fable-answers-s402.md`.
- **A blanket `$SIG{__WARN__}` that swallows is NEVER a fix** — the s402
  deletion stands although its stated condition was false; the only forms are
  (1) fix the cause or (2) `no warnings 'category'` at the narrowest lexical
  scope with a comment saying why the category is expected there (the deep
  tree walkers get `no warnings 'recursion'`).  → §2, task #352.
- **A compiler diagnostic that can fire during a RUN must answer "and on a
  warm cache?"** — if the answer differs it is not a program-output diagnostic
  (side channel or off).  Modules join the drop census through their cached
  emission (`;; PARSE ERROR:`), not through stderr.  → §3, note on #343.
- **#351's repair keys on the EXISTING callable classifier (#266), never a
  word list; layer = Parser2 `_repair_*`, never `$end_pars`.**  → §4.
- **PPI 1.291 lexes `)*name` as a GLOB** — `length($k)*length($k)` is dropped
  whole (Data::Dump:325 is this, in a plain program too); same family + repair
  layer as #351; rule 13 obligations ride the repair commit.  → task #354.
- **Pl/t transpile helpers: ONE stderr-aware helper in PCLCore.pm, a drop
  announcement FAILS the row** (the gate as its own drop detector) — filler.
  → task #355.
- **#353 folds into session B** (one-line compiler change; B owes a sweep
  anyway; acceptance = the six companion files + one guard row).  → §4.


## s402 (2026-08-15, Opus 5) — the drop family gets a voice, a runner column and a spec promise

- **A dropped statement ANNOUNCES itself, once, at the DROP site**:
  `PCL: statement dropped at FILE line N: <text> -- <reason>` on stderr from
  `Pl/Parser.pm`'s two `PARSE ERROR` emitters, transpile-time, exit status
  unchanged.  The prefix is FIXED (tools key on it); the separator is ASCII
  `--` because `pl2cl` sets `binmode(STDERR, ":utf8")` and a raw em dash would
  be double-encoded.  Deduped per (file, line, text) — a statement can reach an
  emitter twice (op/switch.t: 138 events, 112 emitted drops).  PExpr's
  "Handle single node of unknown type" WARN is gone; the die is untouched.
  → task #339, `session-log.md` s402.
- **The announcement is OFF in `pl2cl --module`** — that mode is the RUNTIME
  transpiling a module mid-run (`p-transpile-file`), so the line would land in
  the PROGRAM's output, and only on a cold module cache (nondeterministic).
  `PCL_DROP_ANNOUNCE=all` forces it on; that is how you see a drop inside a
  CPAN module (Data::Dump.pm:325 has one).  Found by a RED gate row, not by
  reasoning.  → task #339.
- **A blanket `local $SIG{__WARN__} = sub {}` hides more than its stated
  cause** — both analysis-parse silencers claimed to silence only PExpr's
  decline warn; measured over both populations they also hid an uninitialized
  value in `VarAnnotator` and two deep-recursion warnings.  Deleted anyway (the
  sin is the silence), exposed signals filed.  → task #352.
- **DROPS is the sweep's FIFTH bucket and a runner COLUMN** —
  `.faillog/_status.tsv` gains `drops` (col 6; note moves to 7; `-1` = NOT
  MEASURED, never 0), `sweep-diff.pl` compares it against
  `baselines/parse-error-drop-census-s399.tsv` (the census IS the baseline; a drop
  leaves by EDIT) and FAILS the run on a new drop;
  `tools/run-perl-suite.pl` records the same column (field 8) and prints the
  same comparison for perl's t/.  → task #343, CLAUDE.md Test Status.
- **Line 1's `gen=` stamp is normative** — `docs/ir-spec.md` §9.2 fixes its
  format and names its two consumers (`artifact-staleness-01.t`,
  `no-hardcoded-paths-01.t`), both of which now cite it.  → ruling §7.3.
- **PPI mis-lexes a bare `/PATTERN/` after a paren-less call as DIVISION** —
  `ok /$qr/, "d"` is dropped whole, `ok /foo/x, "d"` compiles to real division
  and dies; right after `grep`/`return`/`(`/`=`, wrong after every other Word
  (`print` included).  `ppi-upstream-bugs.md` §11 + `ppi-bug-report.t` Bug 8.
  → task #351.
- **`$e1` = undef at PExpr's term dispatch (the `ref=''` decline) has three
  causes, two of them legitimate**: deliberately invalid Perl
  (comp/final_line_num.t's `print 1+` at EOF), a `format` block (blessed
  non-support), and #351.  → task #339 amendment (iv).

## s401 (2026-08-15, Fable) — the s399+s400 review, the WHAT-CHANGED cadence table, two new silent-wrongs

- **What runs when is keyed on WHAT CHANGED, not on a change count** — the
  "every 3rd–5th change" rule is RETIRED; the decision table is in
  CLAUDE.md (Quick Reference → "WHAT TO RUN WHEN") and its rationale in
  `fable-answers-s400.md` §8.  Its NEGATIVE is the point: corpus-diff
  IDENTICAL + lib byte-identical + not a name-resolution change ⇒ the sweep
  CANNOT move — do not run it "to be safe".  Companion `--quick` is the
  default form; the full `--all` at most once per session, and only when a
  row says so.  → task #345, plan `docs/plan-post-s400.md`.
- **A registration authorised in the abstract still has to survive its
  per-row read** (op/const-optree.t, s399; `unlike`, s393) — a standing
  rule now.  → `fable-answers-s400.md` §3.
- **The drop family: announce at the DROP site, not the decline site;
  the census is Option B phase 2's metric; a drop DIES only as phase 2's
  last step** — `PCL: statement dropped at F line L: <text> — <reason>` at
  Parser.pm's `PARSE ERROR` emitters, PExpr's warn deleted, the two
  `$SIG{__WARN__}` workarounds with it; the drop GATE is a RUNNER COLUMN
  (sweep `_status.tsv` + suite snapshot, sweep-diff DROPS bucket vs the
  census tsv), not a Pl/t row.  → §6, tasks #339 #343.
- **The 33 file-level "Can't modify non-lvalue subroutine call" drops KEEP
  dropping, loudly** — user `:lvalue` subs are a blessed non-support and a
  transpile-time die takes every other row of the file (s329 boundary).
  → §6.3.
- **#221 (warnings model) is SCHEDULED post-v0.1**, first item of the
  post-release correctness backlog; design constraint = zero cost on the
  non-undef path.  → §4.
- **#348 lands only after #346 (first) and #347** — a deliberate HANG in
  the companion run is the tail #345 removes.  → §7.2.
- **`tools/lib/PCLSbcl.pm` unifies to QUOTED paths** (filler in #277) — a
  checkout under a path with a space is a fresh-machine reality.  → §7.1.
- **Line 1 of every emitted file, `;;; pcl: pipeline=v2 gen=<gen>`, is a
  PROMISE** tools may key on — goes into `ir-spec.md`; the two guards cite
  it.  → §7.3.
- **The checked-in artifacts RESET `@INC` (and `*pcl-pl2cl-path*`,
  `*p-core-inc-dirs*`) when they load at the first `pack`/`mro`/
  `warnings::enabled` call** — a runtime `push @INC` is silently lost
  (probed).  They carry a PROGRAM preamble an extension never needed, which
  is also the whole of #217; fix = `pl2cl --extension` (no preamble) +
  a rule-12 guard in `p-load-extension`; #217 closes with it and the
  installer stops regenerating.  → task #349, §9.
- **A file-top `require Bareword;` is HOISTED above every runtime
  statement** (`p-eval-always` in the declarations bucket), so a preceding
  runtime `push @INC` has not run — loud, one companion file; fix is a
  MEASUREMENT (emit in place, corpus-diff both populations).  → task #350.
- **Fable's order: #281 (IR pass) design → Option B phase 2 sizing +
  execution (before release phase 4) → boxed aggregates.**  #281 is no longer
  blocked by #153 (the FOLD is done).  → plan §3.
- **The process docs are PUBLISHED AS-IS (USER, s401): "this is open
  source"** — session-log, DECIDED.md, the fable-answers/review-request
  series stay under `docs/`; no `docs/history/`, no pruning before the tag.
  #279 is pure mechanics.  → `release-plan-v0.1.md` decision 2.
- **LICENSE = same as Perl (USER, s401), and EVERY PCL code file carries the
  tag** — the text, the comment style, the insertion point (after a shebang /
  emacs mode line / artifact gen stamp) and the definition of "code file"
  live in ONE place, `tools/lib/PCLLicense.pm`; `tools/tag-license` applies
  it idempotently (also from `tools/rebuild-pack`); `Pl/t/license-tag-01.t`
  is the gate row (one row per root, exclusions must exist and be met).
  **Files from the Perl distribution or CPAN are NOT tagged** (USER: "Don't
  tag code files straight from the Perl distro!") — perl-tests/, cpan-tests/,
  `lib/IO/Handle.pm`, `lib/Math/BigInt/Calc.pm`, each excluded by name with
  its reason.  Regenerating an artifact re-applies the tag on line 2 (line 1
  stays the gen stamp).  → `release-plan-v0.1.md` decision 3, LICENSE.

## s400 (2026-08-15, Opus 5) — #344/#324/#207/#278

- **A plain `grep` SILENTLY SKIPS a file it thinks is binary, and that has
  now falsified three measurements** — s399's #323 population census (8
  files, really 11: perl's regex `.t` files are full of control bytes), the
  #278 hard-coded-path survey (22 hits, really 31 — `cl/pcl-pack.lisp`), and
  the earlier `.tsv`/emitted-CL cases.  **Any census that decides scope uses
  `grep -a` or perl**, and a guard that greps reads BYTES (see
  `Pl/t/no-hardcoded-paths-01.t`).  → `session-log.md` s400.
- **The SBCL command line is built in ONE place**:
  `tools/lib/PCLSbcl.pm` (`sbcl_prefix`/`sbcl_prefix_str`, `$STACK_MB=512`).
  Five runners spawn SBCL; a runner may choose WHAT to load, never the stack
  size / banner flags / `--core` placement.  `PCL_SHOW_SBCL=1` prints the
  command a runner spawns — that is the drift check.  → task #344.
- **Paths outside the checkout are DERIVED, never written down** —
  `tools/lib/PCLPaths.pm` (`perl_suite_t`: `$PCL_PERL_SUITE_T`, else
  `$PERLBREW_ROOT`, else `%Config{prefix}`; dies naming the override).
  Enforced by `Pl/t/no-hardcoded-paths-01.t`, which excludes the three
  transpiled artifacts by their gen stamp and counts them (#217 owns those).
  → task #278.
- **`which_perl`'s children run PCL (`$PCLPERL`), LANDED s406** — the #90
  policy now covers `which_perl` too, so a test that spawns a child through it
  measures PCL and not perl-to-perl.  Held since s400 for two measurement
  holes; both were fixed first (#358 the cloexec hang, #347 the closure gap),
  and with them fixed the switch moved **ZERO rows in either population**:
  the 19 companion callers byte-identical in their failure logs, the sweep
  GATE clean at TOTAL 18517, closure.t OK 272/4 and pack.t OK 5636/89
  unchanged.  What changed is what the rows MEAN, and the wall time (a PCL
  child costs ~4× — op/closure.t + run/cloexec.t 7.7 s → 28 s, the
  discriminating measurement that the children really switched).
  `PCL_FRESH_PERL=real` still forces real perl.  → task #348,
  `cl/pcl-test.lisp` `pl-which_perl`.
- **A TIMEOUT-shaped row is not comparable across runs — but the file that
  moved is** — the s400 completion of #324's verification: of 135 files, 125
  identical, 9 TIMEOUT-shaped noise, 1 real mover, and the mover was pinned
  with two cheap measurements (re-run at the OLD stack size; diff the emitted
  CL) before any bisect.  → `session-log.md` s400.

## Semantics / language policy

- **DESTROY**: not fired in ANY shape (scope exit, undef, reassign, sub
  return) → `not-supported.md` §DESTROY.  Re-open as scoped R2 feature =
  OPEN user decision (`fable-answers-s316v.md` §6c).
- **Error-message text / invalid-input detection**: out of scope — PCL
  transpiles valid Perl, exact fatal text not a goal (user 2026-07-28) →
  `not-supported.md` §Error message text + CLAUDE.md 9.  Blanket
  skip-registry category approved (`fable-answers-s316v.md` §6d).
- **Ref identity / refaddr / `CODE(0x…)` / `==` on refs**: monotonic
  never-reused ids, stable across GC — deliberately NOT raw pointers →
  `ir-spec.md` §2.5.
- **Host-visible values, how far to chase perl's bytes**: match perl's
  SHAPE and invariants, not its bytes; where perl's bytes carry a defect,
  do better; exact bytes only where a program can branch on them (e.g.
  seeded rand → drand48) → `ir-spec.md` (rule), `fable-answers-s316v.md` §6h.
- **`eval $str`**: HARD REQUIREMENT, must always work, never gated (user
  2026-07-07).
- **String-eval scope capture has NO third route** (s383, #295): the eval'd
  text's enclosing scope reaches eval sites INSIDE the text — and named subs
  the eval defines, called later — via `%p-eval-env%`, a lexical bound at
  body entry to the entry value of `*p-eval-lex-alist*` and APPENDED to each
  eval-mode site alist (route 1, the pad chain lexicalized).  Never an
  ambient/dynamic hand-off: that leaks the eval's lexicals into subs it
  merely calls (measured: the s382h "publish around the body" fix moved
  zero rows and was reverted) → `ir-spec.md` §9.1 "pad-chain continuation";
  guard `Pl/t/transpile-test-10.t`.
- **Speed vs readable generated CL**: performance wins (user 2026-07-02) →
  CLAUDE.md §2.
- **Bareword class names** (`tie $x, Tie::StdHash`): per-builtin
  class-name-ARGUMENT-position rule reusing the `bless` bareword mechanism
  (ExprToCL `$is_bareword`), NEVER a global bareword→string rule — three
  failed global attempts recorded in task #142 → `fable-answers-s316v.md` §1.
- **PExpr operand-boundary region** (`$end_pars` maze, PExpr.pm ~2600-3700):
  do NOT add guards there; the fix is Option B's `_reduce_term` →
  `pexpr-term-parsing-review.md`.  #147 and the #138 state-init residual
  WAIT for it (`fable-answers-s316v.md` §6e, §4).
- **Flip-flop constant operands** (`"x" .. 3` vs `$.`): classify per-operand
  at the existing codegen `p-flipflop*` selection point → task #141,
  `fable-answers-s316v.md` §2.
- **quotemeta / `\w`-sensitive builtins / no per-scalar UTF8 flag**: default
  is perl's BYTE semantics; Unicode rules only under `use utf8` /
  `unicode_strings`, selected at transpile time per scope → task #146,
  `fable-answers-s316v.md` §6b.
- **pack/unpack `U` character-vs-byte modes**: implement the real mode model
  (default character mode; `U0`/`C0` switches); anything not implemented
  DIES loudly, never a silent wrong value → task #148,
  `fable-answers-s316v.md` §6j.
- **`printf %n`**: OPEN — user policy call (recommendation: bless as
  not-supported) → task #143, `fable-answers-s316v.md` §6a.
- **Aggregate state model** (tie/readonly/referent-kind — #154-residue,
  #155, #159, #163): side table REJECTED (mutation-path tax); boxed
  aggregates = E5-era Fable design, do NOT start; referent-kind tag on the
  box APPROVED post-R1 (fixes #163 ref identity/printed type + #154's two
  shapes) → `fable-answers-s318.md` §1.  **The tag half is SUPERSEDED
  (s333 measurement, CONFIRMED s335)**: no slot was added — `is-ref` on the
  wrapper already discriminates, and a sticky tag would be *wrong* where it
  differs from the flag (perl re-decides SCALAR↔REF from the referent's
  current content) → `fable-answers-s334.md` §s333-1, DECIDED §s333 below.
- **tie on ARRAY/HASH**: interim = loud stderr WARNING + not-supported
  subsection (NOT a die — would CRASH avhv.t-class files); real support
  waits for boxed aggregates → `fable-answers-s318.md` §1, task #155.
  SHIPPED s320: `%p-warn-aggregate-tie` in `pcl-runtime.lisp`, one line per
  (kind, class) per process so a tie in a loop stays one line.  **s339: folded
  into the shared `%p-announce-unsupported`** (ruled `fable-answers-s337.md`
  §5b) — the class rides in the OPERAND, which is what keeps the per-class
  dedup: `PCL: tie: a HASH (class Foo) is not implemented — …`.
- **Read-only aggregates** (`Internals::SvREADONLY(@a,1)`): storage-swap to
  a simple vector, post-R1; never a weak-hash probe on the push path; do
  not bless as not-supported → `fable-answers-s318.md` §2, task #159.
  **SHIPPED s337**: `Internals::pl-SvREADONLY` is now a MACRO (the call site
  is the only place that has the variable's storage cell — no `Pl/` change),
  the flag IS the storage (simple vector = fixed size = perl's read-only AV,
  whose ELEMENTS stay writable), and the runtime checks only supply perl's
  message.  Rows: push.t/unshift.t/splice.t fully pass, sort.t's in-place
  row passes, both skip-registry entries deleted, the splice.t + sort.t
  hand-edited `ok(1,"SKIP")` stubs restored from the t/op originals.  Guard
  `Pl/t/readonly-array-01.t`.  Three announced divergences (shrinking
  `$#ro=N`, a ref taken BEFORE the swap, `SvREADONLY(@$ref,1)`) and the
  still-unimplemented SCALAR/HASH forms are in `not-supported.md`
  §`Internals::*`.
- **`do SUBNAME(LIST)`**: NO fix — pre-5.20 perl called the sub and PCL
  keeps that semantics; modern perl's rejection is principle-9 material
  (register do.t t63/t65, nothing near `$end_pars`) →
  `fable-answers-s318.md` §4, task #158.  Measured s320: the family is
  **four** rows, t63-t66 (one `foreach my $mode` over `subname("arg")`,
  `subname()`, `$subref("arg")`, `$subref()`, each asserting
  `$@ =~ /\Asyntax error/`), and do.t's **2 PCL-only extra TAP numbers are
  those same rows' `fail()` guard firing** — not a separate defect.  op/do.t
  still reads DIFF (65/8) and gets NO expected.tsv row: t67 (delete-helem
  result copied), t68 (RT 124248 DESTROY-by-GC) and t70 (`$@` false on
  `do DIR`) are unexplained, and the registration bar is all-or-nothing.
- **chr() above U+10FFFF**: blessed — SBCL `char-code-limit` = #x110000,
  U+FFFD is the answer; op/chr.t → XDIFF → `fable-answers-s318.md` §11,
  `not-supported.md` §Unicode, task #173.  SHIPPED s320 (verified XDIFF:
  t10-13 `use bytes` + t40-42 above-Unicode = all 7 rows).  `ord` still
  round-trips the number — only the character/encoded form is lost.
- **#149 application**: per-ROW; side-effect/value/behaviour assertions on
  VALID Perl never register; interleave, never a campaign →
  `fable-answers-s318.md` §3.
- **:crlf layer model** (#139) and **source-echo comments** (#132):
  user-held design decisions — do not start.
- **`**` float divergence**: parked → memory `project_power_op_float_divergence`.
- **mro**: C3-only, no `mro::` API / `next::method` yet →
  `not-supported.md` §mro, provider planned in `mro-plan.md`.
- **@_ aliasing, Unicode limits, and every other blessed gap**: grep
  `not-supported.md` FIRST — the answer is often already there.

## Coding rules (the ones people trip over)

- **Missing case DIES, never defaults**: a runtime `cond`/dispatch over a
  closed set of legal values ends in an explicit error naming the value,
  not a `(t …)` default — silent-wrong is the worst failure mode in the
  tree (vec-64, #138 silent drop) → CLAUDE.md 12, audit task #152.
- **Fix at the right LAYER**: module behavior → `lib/` shim; mechanisms →
  parser; core semantics → runtime → CLAUDE.md 9a.
- **Reuse, don't duplicate**: find the sibling mechanism; same logic in two
  places = hard stop → CLAUDE.md 11.
- **A tree-walking predicate is asked about CANDIDATES, never about every
  node**: `_ref_shadowed`-class predicates (climb parents + scan siblings)
  cost O(file) per call, so a per-token call is quadratic — filter first
  (`eq $canon`, "the fixer matched the name"), then ask.  s316b's one
  `$skip->($t)` per `PPI::Token` cost pack.t 5.8 s → 74 s of transpile for
  BYTE-IDENTICAL output → task #184, session-log s334, guard
  `Pl/t/parser2-02.t` (counts the calls, not the seconds).  **Corollary
  (s335): when the fix is "normalise into the sibling's discipline", grep
  for the OTHER siblings before closing** — the same predicate had two more
  per-token callers in the W10 spanning-rename loops (86.6 s → 0.98 s on a
  two-package 200-statement file) → `fable-answers-s334.md` §5.
- **#213's TIME was the printer, not the recursion**: `_close` asked
  `_ends_in_comment` (pure-Perl char scan) about the WHOLE accumulated
  subtree text at every nesting level — 93% of a nested file's transpile.
  Fixed byte-identically s335 (a terminal comment needs a `;` after the
  last newline — `rindex`/`index` guard).  What remains of #213 is bytes +
  recursion depth: clamp-indent (a) approved cosmetic, `let*` runs (b) is
  the real fix at E5, depth-keyed defvar flattening (c) REJECTED →
  `fable-answers-s334.md` §1.
- **A "suspect X" task carries the cheap discriminating measurement** (or
  names it and why it was skipped) — #184's suspicion was carried 13
  sessions when "time the two phases" was 3 minutes → CLAUDE.md lookup-order
  block, `fable-answers-s334.md` §2.
- **Never simplify a failing test**; skip only via the registry →
  CLAUDE.md 5, `test-skip-registry.md`.
- **Checked-in transpiled artifacts** (`cl/pcl-pack.lisp`, `cl/pcl-mro.lisp`):
  regenerate after emission changes (`tools/rebuild-pack`; pl2cl lib/mro.pm)
  → CLAUDE.md §Quick Reference.
- **Emission change ⇒ bump `*pcl-cache-generation*`** or stale caches run
  old codegen → CLAUDE.md §Pipelines.

## Test / triage infrastructure

- **`baselines/perl-suite-run.tsv` columns**: `P`=PERL, `C`=PCL (C for CL);
  `NOTAP` means PERL produced no TAP (row not comparable — says nothing
  bad about PCL) → legend at top of the tsv; source
  `tools/run-perl-suite.pl`.
- **`.suitelog/*.fails.tsv` description column is PERL's test line**, values
  interpolated by perl — never read it as PCL's output.
- **A run that comes back all/mostly NOTAP means the FIXTURE is broken, not
  the files**: the perl side runs with CWD = the real `t/`, so `require
  './test.pl'` must find PERL's 2000-line harness; with PCL's ~400-line stub
  there instead, real perl cannot even compile `plan tests => N` and emits
  zero TAP.  This is what made io/defout.t / op/localref.t / uni/bless.t look
  NOTAP in s316v (a `cp` had followed a symlink onto the real file).  The
  runner now dies on that instead of producing the misleading run
  → `tools/run-perl-suite.pl` fixture-sanity block, task #151.
- **XDIFF registration bar**: a file gets an `perl-suite-expected.tsv` row
  ONLY when every failing test in it is explained by a blessed
  `not-supported.md` section — partially-explained files stay UNEXPLAINED
  → `perl-suite-expected.tsv` header, `fable-answers-s316v.md` §6f.
  **And the claim is MACHINE-CHECKED per row since s336 (task #185)**: the
  reason excuses only the rows blessed in `baselines/perl-suite-expected-rows.tsv`
  (generated by `run-perl-suite.pl --bless-rows`, keyed by PERL's test
  DESCRIPTION per #177).  An unregistered diverging row keeps the file DIFF
  and is named; a row that stops diverging makes it STALE.  Escape hatch
  `*rows-unstable*` (one entry, hand-placed, needs a measurement that the row
  set is nondeterministic) — used by exactly one file,
  `mro/package_aliases_utf8.t`.
- **Copied-file corpus split**: t/ originals are authoritative; the runner
  no longer hides a t/ file behind a `perl-tests/` copy of the same name;
  drifted copies re-sync post-R1 → task #150, `fable-answers-s316v.md` §6i.
  **PART 2 DONE s337**: `perl-tests/{chop,dor,not,quotemeta}.t` restored
  byte-identical to `t/op/` (they had hand-lowered plans, commented-out
  assertions, and a hand-written `dor.t` substitute).  Each newly-visible
  failure got a CAUSE: not.t 21–24 → registry `:read-only` (interned
  booleans), dor.t 26/28 → registry `:principle9` (assert rejection of
  invalid Perl), quotemeta.t 30/31 → honest baseline fails owned by #146,
  chop.t → PARTIAL 96/148 because **`utf8::encode` is a no-op**, so the
  file's own `next` guard skips 48 rows (not skip-registrable — they never
  run).  A hand-lowered plan had been masking exactly that shortfall.
- **v1 is the emission ORACLE until E4.1** (`PCL_V1=1 ./pl2cl`) — copy its
  shape when correct, don't invent one → `v2-opus5-execution-plan.md` §2.
- **New Pl/t tests** → smallest `transpile-test-NN.t`, cap ~50/file, never
  -01 (user 2026-07-28).
- **Failed attempts are recorded IN the task** (what was tried, what killed
  it) — a task that says what NOT to retry outranks one that only states
  the goal.
- **The TAP layer must not build its own regex scanner.**  `pl-like`/
  `pl-unlike` in `cl/pcl-test.lisp` called `ppcre:create-scanner` directly,
  so `like`/`unlike` judged patterns by DIFFERENT rules than `=~` did —
  missing the cl-ppcre extended-mode workaround, and (s321) choking on the
  `/xx` option, with the `handler-case` turning the error into a quiet test
  FAILURE.  Route through `%pcl-create-scanner` → task #179, CLAUDE.md 11.
- **The emitted CL contains NO host-implementation symbols — a host
  primitive reaches generated code only wrapped in a `p-*` macro.**
  Checkable: `./pl2cl FILE | grep -ac 'sb-[a-z]*:'` is 0 over the corpus
  (measured s382g).  The runtime uses SBCL internals freely; the IR does
  not, so a translator reimplements the `p-*` vocabulary and never an
  implementation detail.  Direction D is the worked example of holding the
  line: a symbol-macro-over-global-cell would naturally have put
  `sb-ext:symbol-global-value` at every global declaration and every
  `local`, and instead lives inside `p-defcell`/`p-local-cell` → two macro
  definitions for a port instead of thousands of sites.  Any future change
  needing a host primitive in emitted code takes the same shape →
  `ir-spec.md` §1, tasks #289/#290, s382g.
- **A regex PATTERN interpolates through the one scanner, and each reference
  lowers by being COMPILED AS CODE.**  `Pl::InterpScan` (perl's own
  `S_intuit_more`/`Perl_regcurly`) decides where a reference starts and
  whether `[…]`/`{…}` after it is a subscript or regex syntax; the
  ExprToCL consumer copies the text between events verbatim (regex escapes
  must reach cl-ppcre unprocessed) and lowers each event by re-parsing its
  own source text through the ordinary expression pipeline — never a second
  lowering table.  The interpolation GATE is the same scan, because a
  narrower gate silently un-does the consumer (`/$1/`, `/$#a/`, `/\Q$^O\E/`
  stayed literal).  The s/// REPLACEMENT keeps the legacy predicate on
  purpose (dq text, and `$1$2` is better served by the runtime's backref
  substitution than a per-match lambda) → `interp-scan.md`, task #237,
  guard `Pl/t/regex-interp-01.t`, s382f.
- **A qr is an OBJECT, and its `(?^flags:…)` wrapper is load-bearing.**  A
  pattern that is exactly one interpolated qr IS that qr (outer modifiers
  ignored); as PART of a bigger pattern the wrapper embeds verbatim; `/xx`
  prints two x's; and the stringification comes from the SOURCE text, not
  from any cl-ppcre rewrite.  Therefore a variable holding a qr is never
  frozen to text by the raw-slot verdict (`write-object`, `VarAnnotator`) →
  `ir-spec.md` §10 regex rows, task #181, s322.
- **NEVER join two TAP streams by test NUMBER** — pair them by DESCRIPTION
  (`tools/lib/PclTapAlign.pm`, unit-tested in `tools/t/tap-align.t`).  A
  file where PCL emits extra or missing rows mid-run then mis-attributes
  every LATER row, in both directions: op/do.t's number join accused two
  rows that PASS and credited two that fail.  Re-sync only on evidence (an
  exact desc match ahead, confirmed by the next row); value-interpolated
  and unnamed descriptions must fall back to positional pairing →
  task #177, s321.
- **A dead run must not look like a run nobody asked for**: every requested
  file gets a row (KILLED / NOT-RUN when the run died), the exit code is
  nonzero, and a journal records rows as they arrive → task #157,
  `tools/run-perl-suite.pl` header.
- **Fixture artifacts get a FIXTURE status + registry in the runner** (not
  `perl-suite-expected.tsv` — a harness artifact is not a language gap):
  #151 stub-cp, #167 splitpath-skip, #172 shadow-symlink getcwd →
  `fable-answers-s318.md` §10.  SHIPPED s320: `baselines/perl-suite-fixture.tsv`
  (`file<TAB>rows<TAB>cause`), matched per TEST NUMBER and all-or-nothing —
  one unregistered failing row keeps the file DIFF and names it, a
  fully-passing registered file is STALE.  #151/#167 were FIXED, not
  registered; the file's RESOLVED list says why (its bar rule 3).
- **The suite tsv snapshot GATES R1**: one full regeneration (per-dir
  FOREGROUND chunks, `--jobs 2-4`) as the LAST pre-R1 act, after the
  FIXTURE/XDIFF hygiene lands; partial regeneration forbidden →
  `fable-answers-s318.md` §6.
- **pack.t sweep hole (#176)**: DONE s322 — the sweep **retries a TIMEOUT
  once at 3× the timeout** (`sweep-perl-tests.pl`, `--no-retry` disables).
  Ruling was `fable-answers-s321.md` §1; its step 2 (bless ~89 rows post-R1)
  turned out moot — **pack.t's 58 baseline rows were there all along** and the
  run matches them exactly.  The "0 baseline rows" claim came from grepping a
  NUL-containing tsv (grep goes binary-silent — use `grep -a`/perl).  The
  90→166 s slowdown is a separate Target-A task (#184).
- **A TIMEOUT is a measurement failure, not a result**: a timing-out file's
  baseline rows come back "unverified", so a regression inside it is invisible
  while the headline still reads "0 new" → task #176, s322.
- **Mis-attributed-evidence registrations (#177 aftermath)**: a tainted
  registration must be RE-VERIFIED before R1; reason-text corrections gate
  R1, status changes only if re-verification fails.  In `renumbered` files,
  per-row claims must quote the DESCRIPTION, not bare tNN →
  `fable-answers-s321.md` §2.  Measured radius s321: backlog empty.
- **XDIFF rows column (machine-checked like FIXTURE)**: APPROVED, POST-R1
  S3 item — backfill from the S2 snapshot, key by description, enforce
  all-or-nothing → `fable-answers-s321.md` §3.
- **op/list.t + op/pack.t are QUARANTINED** (op/list.t OOMs a 10 GB
  cgroup in 53 s, transpiler innocent); they appear as NOT-RUN rows WITH
  the #160 reason, never silently absent; diagnosis post-R1, user
  re-authorization first, suspect SBCL compile time →
  `fable-answers-s318.md` §8, task #160.  SHIPPED s320 as `%QUARANTINE` in
  `run-perl-suite.pl` — a SEPARATE mechanism from `%HEAVY` (which only runs
  a file solo).  Quarantined ≠ skipped: NOT-RUN is an UNEXPLAINED status,
  so these still fail the run and still show as a hole in the tsv.
- **Forked workers must never run the parent's END blocks or signal
  handlers** ($$ == $MAIN_PID guard): one signalled worker otherwise
  `rm -rf`s the SHARED tmpdir and kills its siblings → task #157, guard in
  `run-perl-suite.pl` + `sweep-perl-tests.pl`.
- **`File::Spec::Functions` DELEGATES to `File::Spec`** — it must never carry a
  second implementation.  The two copies drifted apart in BOTH directions and
  the `splitpath($p,1)` divergence silently changed test control flow
  (op/chdir.t's `skip("Already in t/")` never fired, so PCL RAN two tests perl
  skips) → task #167, `lib/File/Spec/Functions.pm` header.
  Corollary when probing a shim: **probe the CALL FORM the caller uses** — the
  method form was correct while the imported function form was broken.
- **A phase block (`BEGIN`/`END`/`CHECK`/`INIT`/`UNITCHECK`) is NOT a runtime
  statement**: it must be skipped when picking a block's TAIL statement, or a
  trailing `BEGIN {}` silently demotes the real last statement to void context
  → `docs/wantarray-context.md` §"a phase block is not a runtime statement",
  task #164 (open; sequence after the CLAUDE.md §8 VOID_CTX regression).
- **`system()`/backticks in an END block EAT the exit status** — `$?` at the
  end of the last END block IS the process exit code, so cleanup ENDs need
  `local $?`.  This silently zeroed run-perl-suite's exit code for every run
  it ever made → task #157.
- **A `use` inside an expression block (`do{}`/`eval{}`/anon-sub) HOISTS, and
  under v2 the hoist must not take v1's DEFERRAL path** — `_pending_hoisted_defs`
  is flushed by `_process_children`, which v2 never calls, so the `use` was
  silently dropped and only surfaced as an undefined function at run time →
  task #187, `Pl/Parser.pm` `parse_block_as_function` teardown
  (`_v2_owner`), guard `Pl/t/transpile-test-09.t`.
- **A `package` inside a do/eval block is a RUNTIME switch only** — it opens no
  CL section, so a hoisted `use` would import into the enclosing package.
  `p-use` therefore takes `:into "Pkg"`, emitted whenever the block's package
  differs from the section's → task #187.
- **An unmatched capture group is perl UNDEF, never raw CL nil** — raw nil means
  "the empty list" to `%p-flatten-list`, so a list ASSIGNMENT (`p-list-=`)
  silently shifted every later capture up a slot, while the ARRAY target
  (`p-array-=`) was correct.  `my ($d,$f) = $p =~ m{^(.*/)?(.*)}` put the
  filename in `$d` → task #188, both capture-collection sites in
  `cl/pcl-runtime.lisp`.
- **`_` is perl's stat-cache filehandle, and it is a BOUND VARIABLE in :pcl** —
  `-e $f and -f _` lowers the bareword to the bare symbol `_`, so `:pcl`
  exports a defvar holding a marker; every filetest resolves its operand
  through ONE funnel (`%p--path` → `%p-stat-arg`) that maintains the cache.
  PCL caches the PATH and re-stats where perl caches the stat BUFFER — a
  deliberate divergence, same answer outside a race → task #186.
- **`lib/File/Basename.pm` is a SHIM ONLY because `$_[N]` writes do not reach a
  plain `my` lexical** (`_strip_trailing_sep($dirname)` did nothing, so
  `dirname("/a/b/c")` answered "/a/b/").  Delete the shim when task #189 lands;
  do NOT "fix" it by blanket-boxing every lexical passed to a sub.
- **A sub Perl already knows about beats INDIRECT-OBJECT syntax** — `divide
  $text => 4` is `divide($text, 4)`, never `$text->divide(4)`.  PExpr's
  indirect pre-pass asks `_is_known_callable($name, 1)`, the same compile-time
  question `_bareword_subscript_autoquotes` asks for bareword array subscripts.
  The question must be PACKAGE-QUALIFIED there (`Widget::show` is not visible
  as a bare `show` from main — probed, the unqualified version broke it), and
  the prototype table cannot answer it because it is keyed by bare name →
  task #190; residue #191.
- **`utf8::unicode_to_native` / `native_to_unicode` are defined ONCE**, in
  `cl/pcl-runtime.lisp`.  `cl/pcl-test.lisp` used to define the same two names
  again for charset_tools.pl; since the TAP layer loads AFTER the runtime its
  copy silently won, so the function behaved differently depending on whether
  Test::More was loaded — and SBCL printed "redefining …" on stderr for 17
  files of the perl-tests sweep.  pl2cl/runtime stderr must stay clean: test
  harnesses merge it into the generated CL.
- **An UNQUALIFIED dynamic glob name resolves in the package IN EFFECT, not
  main** — `*{"_IS_\U$_"} = …` inside `package File::Path` installs
  `File::Path::_IS_MSWIN32`.  Both `p-glob-assign-dynamic` and
  `p-dynamic-typeglob` hardcoded "main" → task #192.  Follow-on #193: those
  same constants are then USED as barewords, and under `use strict` an
  undeclared bareword is a compile error, so by principle 9 anything that
  compiles is a CALL — PCL still emits some of them as strings (always true).

## s323 Fable rulings (2026-08-02, `fable-answers-s323.md`)

- **#189 `writes_args`: shape APPROVED, POST-R1** — detect in the callee body
  (lvalue set PLUS `foreach` over `@_`, lvalue substr/vec/pos, and ESCAPES:
  `\$_[N]`, `\@_`, `&callee;`, `goto &sub`, `@_` passed on — anything not
  provably read-only sets the flag), carry on `sub_info`, consume as a
  VarAnnotator `arg-to-writer` boxing reason at same-file call sites first.
  The "Cannot modify non-boxed value" warning STAYS as the backstop for
  undetected writers.  NO blanket boxing of call args, ever.  Delete
  `lib/File/Basename.pm` when it lands → `fable-answers-s323.md` §1.
- **#193 strict-subs bareword = a CALL, CONFIRMED (principle 9)** — fix at
  the string-FALLTHROUGH point, gated on strict-subs; pre-R1 attempt
  authorized, time-boxed, corpus-diff with every changed row examined;
  **stop-rule: if the trace lands in the #142 `$end_pars` region, STOP**
  (that fix is #153 `_reduce_term`) → `fable-answers-s323.md` §2.
- **#191 indirect-method-in-brackets: DEFERRED** — loud failure, rare
  syntax; re-raise on a real CPAN cause line → `fable-answers-s323.md` §3.
- **Suite-snapshot staleness rule**: a crash-fixing commit MARKS the snapshot
  stale (one session-log line, write-side); regeneration is required at
  QUOTE points (review round, release number, the R1 call), not per commit;
  next regeneration adds a `# taken-at: <commit>` header
  → `fable-answers-s323.md` §4.
- **USER (2026-08-02): R1's CPAN half gates on the FOUR-DIST baseline only**
  (`baselines/cpan-scoreboard.tsv`, no regressions); the widened board is the
  post-R1 worklist, `IO`/`IO::Handle` shim first (23 of 48 FAILs)
  → `fable-answers-s323.md` §5.
- **USER (2026-08-02): fetching/unpacking CPAN dists for measurement is
  blanket-OK'd**; system-level installs still ask → `fable-answers-s323.md` §5.
- **USER (2026-08-02): the full `tools/run-perl-suite.pl --all` run (~15 min)
  runs every 3rd–5th change, NOT per change** — per change it is
  `tools/prove-core` + a targeted single-file suite run (positional
  t-relative paths); full run always once before committing a batch or
  quoting numbers.  Same shape as the s323 sweep cadence
  → `fable-answers-s323.md` §6, memory `feedback_sweep_cadence`.

## s324 (Fable, 2026-08-02): #193 + the two bugs under it

- **#193 DONE: under strict-subs an undeclared bareword before a BINARY
  operator is a CALL** — the strict gate the end-of-expression branch already
  had, added to its second copy (the followed-by-binary-operator branch in
  `handle_subcalls`; the #142 `$end_pars` branch untouched).  `=>` and `->`
  keep the string reading even under strict (fat-comma autoquote; class-name
  invocant — probed).  No-strict behavior unchanged.  Guard:
  `transpile-test-09.t` (#193 row + inverse row).
- **readdir/opendir semantics**: opendir treats its path as a DIRECTORY even
  without a trailing slash (merge-pathnames used to list the PARENT); entries
  are the child's OWN last component (a subdir used to come back as `""`);
  `.`/`..` are included; symlinks unresolved; **readdir is
  wantarray-sensitive** (`%WANTARRAY_SENSITIVE`, list context drains to a
  vector) → `cl/pcl-runtime.lisp` `%p-opendir-impl`/`%p-readdir-impl`,
  session-log s324.
- **A sole-ternary parenthesized lvalue is a SCALAR assignment** —
  `($c ? $a : $b) = V` gives V scalar context and the expression's value is
  the assigned value (never the list-assignment COUNT), so while-loop
  implicit defined() applies; emitted as `(box-set (p-if …) V)` via
  `_sole_ternary_lvalue_id` in BOTH '=' emitters, with box-set arms in both
  auto-defined matchers → defins.t t10, session-log s324.

## R1 (2026-08-02)

- **R1 IS CALLED (user, 2026-08-02, session s325) at `dc6ce64`, tag `R1`.**
  R1 = correctness-by-gate.  The checklist that grounds it: Pl/t gate
  `Result: PASS` 125 files / 4479 tests; full sweep 18461/926 with
  sweep-diff 0 new / 0 fixed vs the 689-row baseline; four-dist CPAN board
  zero regressions (one improvement, Try-Tiny `00-report-prereqs.t`
  FAIL→PASS via #193, baseline advanced); suite snapshot current and
  stamped (`# taken-at:` in `baselines/perl-suite-run.tsv`; the 7 TIMEOUT rows
  re-verified at `--timeout 300`, task #195); artifacts regenerated at gen
  v2-95 (bodies byte-identical) → numbers in `docs/session-log.md` s325.
  **The post-R1 backlog is now ACTIVE in its ruled order**
  (`fable-answers-s323.md` §7): IO/`IO::Handle` shim → #189 `writes_args`
  (+ delete `lib/File/Basename.pm`) → #163 → #176 step 2 → #184 → #185 →
  #159 → #150 → #152 → E4.1/E5 per `docs/v2-opus5-execution-plan.md`.

## s326 (Opus, 2026-08-02): the IO/IO::Handle shim (#197) and the seven bugs under it

- **`use IO::Handle` works: `lib/IO.pm` supplies the XS half in plain Perl**
  (constants, `flush` via the `$|` idiom, `getline`/`getlines`/`gets`,
  `setbuf`/`setvbuf` mapped onto autoflush, `error`/`clearerr`/`untaint`
  answering 0 because PCL latches no error flag and has no taint), and core's
  own pure-Perl `IO/Handle.pm` runs on top of it.  `sync`/`blocking`/`ungetc`
  CROAK by name — no fsync, no fcntl, no pushback — rather than returning a
  plausible value (rule 12).
- **`lib/IO/Handle.pm` is a WORKAROUND, not a shim by choice** — core's file
  with exactly `autoflush` and `printflush` rewritten to save/restore the
  selection explicitly instead of via `SelectSaver`, whose contract is
  restore-on-DESTROY and PCL never calls DESTROY.  **Delete it when
  DESTROY-at-scope-exit lands (task #198).**  Same pattern as
  `lib/File/Basename.pm`.
- **`use Foo ()` and `use Foo qw()` must NOT call import** (verified against
  perl both ways); a bare `use Foo;` still imports with no args.  Decided
  BEFORE the import-args branch, because an empty list still parses to
  `(vector)` → `Pl/Parser.pm` `_use_has_empty_import_list`.
- **A statement modifier on `require` gates it** — `require Foo if COND;` is a
  runtime conditional, not an unconditional hoisted require.  PCL dropped the
  modifier, so `require VMS::Stdio if $^O eq 'VMS'` (File::Temp) died on Linux
  → `_include_statement_modifier`; if/unless only.
- **Perl flushes EVERY handle at exit and before fork, not just the standard
  three** — an unclosed `open my $fh,'>',$f; print {$fh} …` silently lost its
  buffer.  Registry `*p-open-output-streams*` (weak), flushed from the exit
  hook AFTER the END blocks and from `p-fork`.
- **`select(FH)` is real and `$|` is per-handle** — one-arg select sets the
  default output handle and returns the previous designator (so
  `select((select($fh), $|=1)[0])` works); setting `$|` true flushes that
  handle immediately and makes later writes flush.  Both were stubs: select
  returned a constant string and `$|` was one global flag nothing consumed.
- **open() on a GLOB handle must not overwrite the scalar** — `IO::Handle->new`
  / `\*FH` / `Symbol::gensym` produce a (usually blessed) globref; the stream
  belongs in the glob's IO slot, keyed by the glob's name.  PCL box-set the
  stream over it, so `ref($io)` stopped being `IO::Handle` and later method
  calls went elsewhere.  `%p-close-impl` now resolves through `%p-resolve-fh`
  (one resolver, every handle shape) with `%p-forget-fh` mirroring
  `%p-install-fh`.
- **A symbolic container designator resolves in `exists`/`delete` too** —
  `$$p{k}` and `$$p[i]` (and `"Pkg::"` stashes) were resolved on the
  read/write paths only, so `exists $$p{k}` silently answered NO and
  `delete $$p{k}` crashed SBCL's GETHASH.  One resolver:
  `%p-designator-hash`/`%p-designator-array`, and `p-ensure-hashref`/
  `p-ensure-arrayref` delegate their string branch to `p-cast-%`/
  `%p-symref-array` instead of keeping a second copy.
- **`delete $ref->[i]` lowers to `p-delete-array`** — `exists` had both ref
  arms all along, `delete` only the hash one, so the array form fell through
  to a one-argument `p-delete` and crashed on arity (both pipelines).
- **Capture-Tiny is NOT cleared by the shim**: its 23 FAILs move off the IO
  cause onto two string-eval bugs (task #199) — a prototyped sub defined by
  `eval "sub NAME(&;@){…}"` is never installed, and a p-eval-thunk reaches CL's
  `push` with too many arguments.  The "23 of 48" estimate in #197 assumed one
  blocker; there were three.

## s327 (Fable, 2026-08-02): s326 review + the post-#197 order

- **s326 APPROVED with two review fixes (`87f23f6`)** — both found by probing
  the seams the diff touched: close() on a name-string handle cleared the
  scalar (%p-forget-fh now mirrors %p-install-fh's name-string case), and the
  `use Foo VERSION LIST` version-skip was dead code (PPI's `$stmt->version` is
  empty for module versions) so the import args emitted a PARSE ERROR progn —
  version now recognized positionally, with the no-operator-follows guard
  (`use Foo 1.5, "x"` keeps the number a list element; probed).
- **ORDER RULING: #199 runs BEFORE #189** — the two Capture-Tiny string-eval
  bugs are the COMPLETION of post-R1 item 1, whose justification was those 23
  board FAILs.  Fix the eval-string prototyped-sub install first (the
  API-existence bug, a silent-nothing), then the p-eval-thunk→push arity;
  both are #79's area; the #142/#153 stop-rule applies if a trace lands in
  `$end_pars`.  Exit criterion: Capture-Tiny board re-run with per-file causes.
- **#198 (DESTROY at scope exit) is PARKED, design-first** — it interacts with
  #163's referent-kind tag and E5 boxed aggregates; Fable designs it after
  #163 lands (or a CPAN cause line forces it earlier).  Payoff when done:
  delete `lib/IO/Handle.pm`, unblock Try::Tiny/Scope::Guard/File::Temp
  /SelectSaver.

## s328 (Opus, 2026-08-02): #199 — four Capture-Tiny blockers, all silent

- **`goto EXPR` where EXPR is a CODE REF is a TAIL CALL, not a computed
  label** — `goto \&NAME`, `goto $coderef`, `goto $h->{cb}` all route through
  `p-goto-sub` via `%p-goto-target`.  `p-goto-computed` used to be a `defun`
  documented as "not implementable in CL; silently ignore": the sub returned
  undef with no error and no output.  A NON-coderef operand (a real computed
  LABEL) now **names itself on stderr and falls through** — announced-not-
  silent, the #155 tie shape, deliberately NOT the rule-12 die: measured, a
  die costs `perl-tests/state.t` 88 verified passing rows (157/166 → 69/166),
  because its one computed goto sits two thirds up an otherwise-passing file,
  and any CPAN module using one would abort entirely.  The old behaviour's sin
  was the silence, not the fall-through.  Guard `Pl/t/goto-sub-phase-01.t`
  rows 1-6.
- **A `local(...)` inside a BEGIN/END/CHECK/INIT/UNITCHECK block must close
  its `let` before the block does** — `_process_special_block`'s `$process`
  closure calls `_process_children`, which (unlike `_process_block` and the
  sub-body path) does NOT close local-lets.  v1 therefore emitted ONE PAREN
  TOO FEW and the block's `(push (lambda …) *end-blocks*)` swallowed every
  later top-level form; File::Temp's `END { local($.,$@,$!,$^E,$?); … }` made
  the whole module fail to load ("too many elements in (push …)"), taking
  Capture::Tiny with it.  v2 refuses this shape loudly ("statement fallback
  left 1 open scope(s)") and falls back to v1 — that gate is unchanged.
- **`require_ok`/`use_ok` must actually LOAD the module.**  The old
  `cl/pcl-test.lisp` comment claimed the transpiler had already resolved the
  `use` at compile time; nothing under `Pl/` mentions either name, so they
  reported ok and loaded nothing — every row after them failed for no visible
  cause.  Both now load (`%test-load-module`, path-ish name → `p-require-file`)
  and report the real result; `use_ok` imports `:into *pcl-current-package*`,
  with Test::More's "lone numeric arg is a VERSION" rule.
- **`Test::More->builder` exists and answers ONLY the output handles**
  (`output`/`failure_output`/`todo_output` → PCL handle-name designators
  "STDOUT"/"STDERR"/"STDOUT", which `binmode`/`print {…}`/`fileno` accept).
  Every other Test::Builder method is deliberately absent so dispatch dies
  naming it — a stub would corrupt a file's counts.  22 of Capture-Tiny's 24
  t-files died on the missing `builder` alone.
- **The embedded-`my` veto does not fire for a sibling sub that DECLARES the
  same name** (`Pl/Parser2.pm` `_sub_declares_name`).  The veto falls back to
  "the old forward-defvar'd global", which is never emitted when every other
  mention is itself a declaration — so `sub s1 { my $fh; … }` next to
  `sub nf { open my $fh, … }` left BOTH unbound (`Utils::$fh is unbound`).
  A sub that only REFERENCES the name still vetoes (probed; inverse guard).
- **Board result**: Capture-Tiny 1 PASS / 0 PARTIAL / 23 FAIL → **4 PASS /
  4 PARTIAL / 16 FAIL of 24**; the 16 residual causes are measured per file
  in task #201 (File::Temp template check, tie-on-glob arity, closed-stream
  writes, `local $ENV{...}`).

## s329 (Fable, 2026-08-02): s328 review + rulings on its asks

- **Rule-12 boundary RULED: DIE vs ANNOUNCE is decided by whether a VALUE
  flows onward.**  A missing case that should have produced/written a value
  the program consumes → DIE naming it.  An EFFECT-ONLY missing case (jump,
  tie, attribute) in code that otherwise runs correctly → ANNOUNCE on stderr
  + `not-supported.md` entry; unclassifiable-in-a-minute counts as
  value-producing.  De-dup announcements once per (site, operand) per
  process via ONE shared helper, introduced at the start of #152 — not
  per-site hashes.  → `fable-answers-s328.md` §1; gates how #152 runs.
- **Sweep TOTAL-passing is a GATE, machine-checked**: the total must not
  fall vs baseline; any fall explained per file.  `sweep-diff` grows a
  fourth bucket (LOST = baseline-passing rows the run did not produce),
  non-empty LOST = not clean.  Must land BEFORE #152.  →
  `fable-answers-s328.md` §4, task #204.
- **#202 runs FIRST (before #189)** as harness-trust calibration; after it,
  re-verify the four-dist baseline PASSes that contained fake assertions
  (File-Which 01_use.t is nothing but one).  use_ok's TAP description must
  match Test::More's (`use Foo;` — no import list).  →
  `fable-answers-s328.md` §3.
- **#201 File::Temp layer: probe the failing PREDICATE, not the module** —
  run the exact template check under perl and PCL; PCL-only divergence →
  fix the core mechanism (no File::Temp name under `Pl/`/`cl/`), both-fail →
  fix the shim.  Same procedure for the rest of #201.  →
  `fable-answers-s328.md` §2.
- **`goto` restores the ORIGINAL caller's `*wantarray*`** (s329 review fix;
  pre-existing hole in `p-goto-sub`, both spellings ran the target in the
  goto statement's own context).  Guard `goto-sub-phase-01.t` (17 rows).
- **Deep `goto &sub` chains are BOUNDED under PCL** (binding-stack, ~10^5),
  where perl is constant-depth → `not-supported.md` §goto-trampolines.
- **Embedded-`my` veto exemption requires NO FREE REFERENCE** — declaring an
  inner shadow is not enough (`_sub_freely_references_name`: doc-order +
  block-containment; compound-header `my` scopes to the body; `<$fh>` and
  interpolations count as uses — Symbol-only scans cannot see them).
  Residual poisoned-name defvar hole (crashes loudly in every era) → task
  #205.

## s330 (Opus, 2026-08-02): #202 — the TAP layer audited for unfalsifiable assertions

- **An assertion that cannot EVALUATE its claim reports `not ok` naming the
  reason — it does not die.**  Rule 12's "a verdict is a value" reading would
  abort the file and cost every row after it (the s328 88-row lesson applied
  to the instrument).  The one exception is `plan()`, which dies: there is no
  row to attach a diagnostic to and the file's whole count claim is void.
  Full argument + inventory → `docs/tap-assertion-audit.md`; ratification
  asked in `opus5-review-requests-s330.md` §1.
- **`unlike` could not fail**: a scanner error was swallowed into a PASS
  (`(error () t)`).  `like`/`unlike` are now ONE function over one matcher,
  and an unusable pattern is `not ok` in both directions.
- **`eq_hash` had never worked** (double unwrap → type-error on every
  hashref, killing the file).  **`cmp_ok` manufactured verdicts** for `<=>`,
  `cmp`, `=~`, `!~` (all now implemented).
- **TAP descriptions are join keys, so they must be Test::More's**: `use
  Foo;` without the import list; isa_ok's four kinds get four wordings;
  can_ok names the class and the single method.  Zero baseline rows keyed on
  the old texts (checked).
- **`skip_without_dynamic_extension` asks the loader** instead of skipping
  unconditionally.  **A diagnostic the harness prints mid-run can split a TAP
  row**: its first version cost undef.t 35/35 PASS → 30/35 PARTIAL with
  nothing failing, because a failed load's stderr banner landed inside a row
  (the sweep folds stderr into stdout).  Probe now runs with `*error-output*`
  bound to a broadcast stream — a trap #152's announcements must avoid.
- **`scalar()` never dereferences** (runtime, found by the audit's probes):
  `p-scalar` unboxed first, so `scalar($aref)` was the element COUNT and
  `scalar(\5)` was the referent.  An array variable is a raw adjustable
  vector and is never boxed, so a box holding a vector is unambiguously a
  ref; the hash branch already carried that guard.  +7 sweep rows; array.t
  t128 was passing only because both sides flattened to undef (blessed, with
  the cause, into `baselines/fail-baseline.tsv`; 689 → 683).
- **FILED**: #206 `UNIVERSAL::isa` ignores the reftype rule (gate on #163);
  #207 `which_perl`/`run_perl` are unverified stubs; #208 the CPAN board
  `.tsv` baselines drifted since s322 (verified NOT from this session — a
  HEAD worktree gives identical boards).

## s330b (Opus): #204 — the sweep's TOTAL-passing gate is machine-checked

- **`tools/sweep-diff.pl` has a fourth bucket, LOST**: per file, baseline
  PASSING rows the current run did not produce, read from a blessed
  `baselines/pass-baseline.tsv` (`save-status` writes it).  Non-empty LOST = the
  run is NOT clean, same exit code as NEW.  Every run prints
  `TOTAL passing: baseline N, current M`.
- **A FULL sweep runs the gate itself and exits with its verdict**
  (`sweep-perl-tests.pl` with no file arguments; `--no-gate` opts out).  A
  sweep of named files stays informational — a partial run is not comparable
  to a whole-corpus baseline.
- **Two anti-silence rules in the implementation**: when no pass baseline can
  be found the tool prints `LOST: NOT CHECKED — …` rather than nothing, and
  an explicit `--pass-baseline` that does not exist is FATAL, never a silent
  fall-back to the default file.  A check that goes quiet when it cannot run
  is indistinguishable from one that passed.

## s330c (Opus): #189 writes_args — the `@_` aliasing divergence closed for known callees

- **A sub that writes through `@_` is detected in its BODY** (`Parser2::
  _sub_writes_args`), the fact rides `sub_info` as `writes_args`, and
  VarAnnotator turns it into an `arg-to-writer` boxing event at that sub's
  call sites — the same marking `chomp $x` already uses.  Only files with
  such a sub pay.  Conservative by construction: an `@_`/`$_[N]` occurrence
  the scan cannot prove is a read (including `\$_[N]`, `\@_`, `&callee;`,
  `goto &sub`, handing `@_` to an unknown callee) sets the flag.  The runtime
  "Cannot modify non-boxed value" warning STAYS as the backstop for coderef
  calls, method dispatch and cross-file callees.
- **`lib/File/Basename.pm` DELETED** — core's copy now answers `dirname`
  correctly under PCL.  Guard `Pl/t/writes-args-01.t`.
- **`s///` / `tr///` bound to an ARRAY or HASH ELEMENT write the element**
  (`p-aref-box`, not `p-aref`).  Before: a silent no-op on an array element,
  a "Cannot modify non-boxed value" warning on a hash element — for one of
  perl's most common idioms.
- **Element VIVIFICATION follows perl exactly**: taking the lvalue creates
  the element, so only the ops that perl treats as lvalues take it — `s///`
  and a MODIFYING `tr/x/y/` vivify; a count-only `tr/N/N/` (identical lists,
  no d/s/c; an empty replacement replicates the search list), any `/r` form
  and a plain match do not.  Getting this wrong cost `perl-tests/tr.t` two
  passing rows — **caught by #204's LOST bucket on its first live run**,
  which is what that gate is for.

## s332 (Fable, 2026-08-03): the s330/s331 asks ruled + the writes_args scan's blind spot closed

- **Harness verdict rule RATIFIED**: inside the TAP layer, an assertion that
  cannot evaluate its claim reports `not ok` naming the reason; only
  `plan()` dies.  Not an exception to the s328 §1 boundary — an adverse
  verdict IS the produced value; a die in the instrument costs every row
  after it.  #152 inherits this split: harness → adverse verdict, runtime
  → s328 §1.  → `fable-answers-s331.md` §1.
- **Announcements must never interleave with a TAP stream** (a mid-run
  stderr banner SPLIT a row: undef.t 35/35 → 30/35 with nothing failing) —
  hard rule for #152's `%p-announce-unsupported`: emit before the plan or
  outside the folded stream.  → `fable-answers-s331.md` §2.
- **`like`/`unlike` with a plain STRING pattern follows test.pl (interpolate),
  both callers** — accepted divergence from Test::More; a dist that passed
  upstream CI cannot contain the failing spelling.  Revisit on a real CPAN
  cause line.  → `fable-answers-s331.md` §3.
- **UNSTABLE + LOST stay independent buckets; no promotion rule** — UNSTABLE
  is descriptive, not exculpatory; the TOTAL/LOST line decides, and it
  already fails the gate.  → `fable-answers-s331.md` §5.
- **No whitelisting non-core callees in the writes_args scan (9a stands);
  the `writes_args => 0` provably-safe-callee refinement is approved in
  principle, NOT scheduled** — revisit on measured boxing cost (R2).  →
  `fable-answers-s331.md` §6.
- **The element-vivification rule is probe-CONFIRMED** on all seven corners
  incl. `/d`+empty replacement (DELETE → vivifies) and `/s`,`/c` on equal
  lists (vivify); `_rhs_writes_match_target` is the normative statement.
  → `fable-answers-s331.md` §7.
- **writes_args scan: four probe-found FALSE NEGATIVES fixed (gen v2-100)**
  — implicit-`$_` writers have NO Symbol token (`s/b/X/ for @_`,
  `for (@_) { s/// }`), plain-token roots were skipped entirely (even
  `$_ = uc $_ for @_`), and map/grep alias `$_` like foreach but sat in the
  value-consumer list.  All were silent-wrong with no backstop warning.
  Rule for any Symbol-keyed scan: **probe the spellings that produce NO
  Symbol token — implicit `$_` is the standing example.**  Guards:
  `Pl/t/writes-args-01.t` rows 14–15.  → `fable-answers-s331.md` §9.

## s333 (Opus, 2026-08-03): #163 — a reference's type and address are the REFERENT's

- **`%p-ref-referent` is the ONE rule** for "what does this reference point
  at", and `is-ref` on the p-backslash wrapper is its only discriminator.
  **No new box slot was needed**: the ruled referent-KIND tag would have
  answered the same question the flag already answers, at one word per box
  on every scalar in every program.  A value reaching a stringifier is
  either the wrapper itself (`\$x` into `print`, an element, a raw param) or
  a variable box holding one; box-sv counted levels instead, which is why the
  same reference printed `SCALAR` through a variable and `REF` straight into
  `print`.  → `docs/ir-spec.md` §2.5 (normative), guard
  `Pl/t/ref-identity-01.t`.
- **The address is the referent's, never the wrapper's** — a wrapper is fresh
  per `\`, so `\$x == \$x` was false and one variable printed two addresses.
  `p-ref`, `box-sv`, `stringify-value` and `box-nv` now read the one rule, so
  the word and the number agree on every path.
- **`ref(\$aref)` is `REF`, not `ARRAY`**: p-ref's aggregate arms read the
  referent's *value* when handed a wrapper directly.  Fixing that widened
  `\$aref`/`\$href`/`\$qr`/`\$cref` to REF; the INVERSE (`\@a`, `\%h`,
  `$list[0]` holding an aggregate wrapper) is guarded row by row.
- **A reference's string is NOT cached** on the holding box: `SCALAR` vs
  `REF` depends on what the referent holds *now* (`my $r=\1; my $rr=\$r;` →
  REF; `$r=5` → SCALAR), and the referent is a different box whose writes
  cannot invalidate a cache here.  Same reason box-nv never caches an
  address-based NV.
- **#154's two shapes are closed** (`@$sref`, `$sref->{k}`, `%$sref`,
  `$sref->[0]` → perl's fatal): `%p-scalar-referent-p` separates a ref to a
  plain scalar from the representation layer #154 documented as
  indistinguishable.  The ref-to-REF half stays lenient **because the parser
  drops the outer level of `$$refref->{k}` (#211, filed s333)** — the runtime
  leniency is a workaround for that parse bug, not a semantic choice.

## s335 (Fable, 2026-08-03): s333+s334 asks ruled; two byte-identical compile-time fixes shipped

- **s333 §1 CONFIRMED**: no `ref-kind` slot — the s318/s320 tag ruling is
  superseded by the better measurement (see the annotated bullet under
  "Aggregate state model" above).  §2 ACCEPTED: ref strings stay uncached.
- **#211 is parked behind #153** (term-machinery region, no guard-patches);
  its leniency covers exactly the two measured spellings — a NEW shape
  wanting leniency needs its own probe and task, #211 is not a blanket
  citation → `fable-answers-s334.md` §s333-3.
- **Sweep gate vs load noise (#204 follow-up)**: record min MemAvailable
  beside any LOST report, AND re-run a LOST file once at `--jobs 1`; the
  serial numbers REPLACE that file's rows for the verdict, the report prints
  both.  Whitelisting noisy files REJECTED; "operator re-runs" REJECTED →
  `fable-answers-s334.md` §s333-4, task #215.
- **#213 re-scoped**: the TIME was `_ends_in_comment` (fixed s335,
  byte-identical); remaining = cosmetic bytes (clamp approved, bump
  generation) + recursion depth (real fix = `let*` runs at E5; depth-keyed
  defvar flattening REJECTED) → `fable-answers-s334.md` §1, and the Coding
  rules bullet above.
- **W10 spanning-rename loops converted to the #184 rule** (the two
  remaining per-token `_ref_shadowed` callers): 86.6 s → 0.98 s on a
  two-package 200-statement file, emission byte-identical, guard rows in
  `Pl/t/parser2-02.t` → `fable-answers-s334.md` §5.
- **#152 audit scope is CLOSED at the runtime** (s338): pcl-test done under
  #202; skip-registry is data; pack/mro artifacts are audited via their Perl
  sources' oracles; ONE bounded extension = constant-default grep of
  `cl/pcl-xs.lisp` (task #222).  Widening to non-constant default arms
  REJECTED — rule 12 is prospective; fuzzer + sweep TOTAL are the net →
  `fable-answers-s337.md` §3.
- **Default-off perl diagnostics are ABSENT, never unconditional** — PCL has
  no `use warnings` model (grepped s337c); minimal design filed UNSCHEDULED
  as task #221, triggered by the first concrete consumer →
  `fable-answers-s337.md` §5a.  The print-on-unopened measurement lives in
  `%p-out-fh-or-fail`'s docstring; do not retry it.
- **Closed-handle value APPROVED** (defined not-writable value left by
  `close`), queued behind #153/E4.1 as task #220; same magic-cell family as
  #144, shrinks #198 → `fable-answers-s337.md` §4.
- **#215 gains a warm-first half** (transpile one file before fanning out;
  per-worker cache dirs REJECTED); serial re-run stays as backstop →
  `fable-answers-s337.md` §5c, task #222.
  **SHIPPED s339, and the race had a CAUSE**: `p-load-module-cached`'s
  **`.lisp` branch (the DEFAULT) wrote the cache file IN PLACE** while the
  FASL branch beside it already used pid-temp + atomic `rename-file`.
  Measured: SBCL's `:supersede` truncates and writes the real file, so a
  worker whose `p-cache-valid-p` saw the fresh mtime `load`ed a half-written
  module.  Both branches are atomic now; the sweep also warms one file first
  and reports min MemAvailable + a serial re-run of any LOST file (the serial
  verdict replaces, both shown).  Cold-cache `--jobs 8` repro now gives
  do.t 65/3/5 and a clean gate.
- **XS callbacks cannot DIE — the announce IS rule 12's loud ending there**
  (s339 audit of `cl/pcl-xs.lisp`, task #222): `with-xs-guard` converts every
  condition into the on-error constant BY DESIGN (pclxs rule O4, nothing
  unwinds into C), so `%p-unsupported-value` in a callback would be silently
  downgraded.  `xs-ref-type` now enumerates SCALAR/LVALUE and announces any
  other reftype (REGEXP today) as "answered as a scalar reference"; the
  contract enum has no code for it.
- **getprotobyname/getprotobynumber read `/etc/protocols`** (lazy, four-entry
  table only when unreadable), wantarray-sensitive, EXACT name-or-alias match
  (`Tcp` misses), scalar context = number by name / NAME by number →
  `fable-answers-s337.md` §4-secondary, s339.  **Fallback-host divergence
  ACCEPTED** (no `/etc/protocols` → the 4-entry table may differ from perl's
  NSS answer; not a validated host class) → `fable-answers-s339.md` §4.
- **XS announce-not-die RATIFIED for the whole of `cl/pcl-xs.lisp`; NO
  die-across-the-boundary mechanism** (PS_DIED stays the only die channel —
  it is for contracts that carry a Perl-die outcome, not for answerable
  cases; revisit only if the pclxs contract grows an UNKNOWN code) →
  `fable-answers-s339.md` §3, s340.
- **Baseline hygiene (task #223): fail-baseline rows leave by EDIT with the
  cause named, NEVER by regeneration; pass-baseline (a generated file)
  re-blesses ONLY from a gate-green run after a per-file audit** — every
  delta attributed, no file down, an unexplained delta is a finding.
  `save-status` stamps `# taken-at: <sha> <date>`; readers skip `#` lines →
  `fable-answers-s339.md` §5, s340.
- **The `+8` was a STALE BLESS, not drift (#223 executed s341).**  Measured,
  not inferred: at `73d43ac` — the commit that installed
  `baselines/pass-baseline.tsv` — the six drifting files already produced the
  CURRENT numbers, so the blessed `_status.tsv` came from a run older than its
  own commit.  Attribution: push.t/unshift.t +1 each = **#159** read-only
  arrays (measured across `1b0a7e4`, where the row goes SKIP→PASS); scalar.t
  +2 = the **s333** `scalar()` fix (the two rows s333 left un-edited);
  ref.t +2 / state.t +1 / tr.t +1 = **no code cause** — all three are PARTIAL
  (abort) files that gave the higher count at every commit tested, i.e. the
  baseline row was taken from a memory-pressured run (the s337 ref.t
  184-vs-186 oom measurement).  **Rule that follows: a baseline blessed
  without a `taken-at` stamp cannot be attributed later, only re-measured.**
- **`our $x OP= …` is a legal `our` declaration** — the initialiser may use any
  assignment operator (perl's Exporter.pm opens `our $Verbose ||= 0;`); the
  gate asks `TokenUtils::is_assign_op` (the #140 one-true set), s341b.
- **The rule-2 live-v1 audit has two traps** (s341b): a `git worktree` at an
  older commit SHARES `~/.pcl-cache`, so its entries pollute a `pipeline=v1`
  grep — filter on the current `gen=`; and `PCL_V2_VERBOSE=1` goes to stderr,
  which a sweep folds into TAP, so the eval-mode half needs a file
  side-channel, not a global env var → `session-log.md` s341b, task #225.
- **The v1 fallback is MASKING a v2 defect in Math::BigInt** (#224, s341b): the
  v2-compiled module recurses (tie `STORE` → `round_mode` → symbolic-ref write
  → `STORE`) until the binding stack dies, costing pack.t everything (~70 s →
  not finishing at 600 s).  The cond-my poison narrowing that would move it to
  v2 is correct, probe-verified, and parked on `wip/s341-condmy-narrowing`; it
  lands WITH #224, never before.  **Both landed s342** — the narrowing is on
  main, BigInt is on v2, pack.t is 66 s / 5636 passing.
- **#224 FIXED s342 — a tie handler runs with ITS OWN cell's magic off.**  Perl
  (mg.c `save_magic`/`restore_magic`) turns an SV's magic off for the duration
  of any of its magic callbacks; PCL now does the same by swapping the proxy's
  `saved-value` (which IS the raw slot) into the box for the call.  Three
  companion rules probed the same session: a write hits the raw slot BEFORE
  `STORE` runs, a `FETCH` result is written BACK to the raw slot, and re-tying
  REPLACES the magic instead of nesting proxies.  `tied()` still reports the
  object while magic is off.  Normative: `docs/ir-spec.md` §2.2b; guard
  `Pl/t/tie-01.t`.
- **The live v1 share is SIX families, not zero — E4.1 §5a.2 is unsatisfied**
  (#225 DONE s342c, `docs/v1-live-share-audit.md`; tasks #226–#230).  Measured
  60 v1 routes on a cold cache (24 sweep + 36 CPAN board).  **F5 cleared s342d
  (#229) → five families left**, board 36 → 31 events.
- **The live-v1 audit's populations are sweep + board + the Pl/t gate ITSELF,
  and the E4.1 flip needs zero TODO events across all THREE** (RULED s353,
  `fable-answers-s352.md` §1.1).  s342c never measured `Pl/t/`, which hid 27
  gate routes — 15 of them Moo/Sub::Quote — behind a "ZERO events" ruling.
  Gate families M1–M6 = tasks #247/#248, all pre-flip, order M3→M6→M2→M1→M4→M5.
  **All six SHIPPED (M3/M6/M2/M1 s353, M5 s353c, M4 s354): the Pl/t gate is at
  ZERO unaccounted TODO events** — the 2 that remain are the DELIBERATE
  inverse-guard rows in `transpile-test-09.t` (M1-predicate refusals, on
  #242's step-2 rephrase list), and 5 DIE-class events are v2 correctly
  reporting a perl error (exempt, they self-resolve at the flip).
- **E4.1 STEP 2 IS DONE — THERE IS ONE PIPELINE** (#242, s356).  `PCL_V1`,
  `PCL_V1_FILES`, `parse_with_fallback` and the `PCL_V2_AUDIT_LOG` side-channel
  are gone; `pl2cl` has a single `parse_source` entry and an unknown transpile
  mode is a loud error rather than a silently-dropped option.  `Pl::Parser` is
  still LOADED (v1's expression seam runs inside v2) — the unreachable
  file-level chunks are #243.  What the flip required and got:
  **(a)** the four ruled refusals rephrased perl-shaped and trappable
  (`PCL: unsupported in string eval: …` reaches `$@` with perl's own
  `at (eval N) line M.` suffix), each with a `docs/not-supported.md` entry —
  multi-switch, trailing-declaration, F6 oversized run form, and the M7
  our-alias residue; **(b)** `--lenient-ppi` accepted-and-inert with the PPI
  failure now **naming the file** (§5a.4); **(c)** the `#228` `[perl #129069]`
  registration; **(d)** the cache key's pipeline component frozen to `"v2"` so
  this generation's paths keep hashing where they did.  A `pl2cl server:`
  prefix that was leaking into `$@` was removed — the transpiler's message IS
  the error.  Verified: corpus emission **identical across 111 files**, gate
  131/4658 PASS, sweep GATE clean at 18499 (+0 vs the edited baseline).
- **THREE-POPULATION ZERO IS MET (s354 measured, s355 closed the board).**
  Pl/t clean (above); the sweep clean at 17 events, all named, and **one
  BETTER than the blessed 18** — M1's widened collapse absorbed the sweep's
  lone multi-switch event; the CPAN board clean after #251/M7, its only
  remaining TODO being the ruled true-multi-switch.  §5a.2's precondition
  for the E4.1 flip is satisfied.  Also recorded: **the board has no blessed
  Moo baseline** — `baselines/cpan-board14-s343.tsv` covers Role-Tiny but not Moo.
- **An `our` alias runs to the end of its block OR to the next declaration
  of the same name — so a re-declaration ENDS the requalification, it does
  not defeat it** (#251/M7, s355, `_requalify_block_our_after_pkg_switch`).
  The pass refused whenever the switched region re-declared the name, which
  gated ordinary Perl: one bare block declaring `our @ISA` in each of four
  successive packages (Role-Tiny `subclass.t`; perl runs it 6/6).  Two
  fixes, both the M4 lesson again: **truncate** the region at a block-level
  `PPI::Statement::Variable` (its binding runs to the block's end, so it
  partitions cleanly — the tail gets its own turn with its own decl_pkg),
  and make the re-declaration test **sigil-exact** (`foreach my $d` binds
  the SCALAR and must not end an `@d` alias).  Measured: board FAILs
  `Moo/accessor-default.t` 0 → **PASS 40 ok** and `Role-Tiny/subclass.t`
  0 → 4 ok, no row anywhere else changed, corpus emission identical across
  111 files.  **The one surviving refusal is a re-declaration NESTED in an
  inner block or sub** — there the alias RESUMES after the inner scope,
  which a truncation cannot express; it is the shape step 2 must rephrase,
  and its v1 fallback is silently wrong today (probed), so the flip
  improves it.  Guards: `Pl/t/transpile-test-04b.t`.
- **The spanning-lexical RENAME resolves variables canonically, exactly like
  the CHECKER** (M4, s354, `Parser2.pm` `_rename_spanning_lexicals`).  Two
  sites had stayed bare-name: the declaration count (so `my %mix` beside
  `my $mix` read as a re-declaration of `$mix` and refused → the checker then
  died → whole-file v1), and the span test itself (so a sibling `my @x` used
  in a later segment promoted `$x` gratuitously).  Both now go through
  `->symbol` / `_canon_refs_in` — the invariant being kept is *the rename
  never refuses a name the checker will die on*, and the pass must not rename
  one it won't.  Family USES were already blessed as safe here by M-F; the
  matching family DECL simply had not followed.
- **Sub::Quote's `my …; my …; package X;` eval shape collapses via a WHITELIST
  leading-`my` predicate on #226's collapse — never general multi-section
  assembly** (RULED s353, `fable-answers-s352.md` §1.2): accept only
  initializers built from lexicals/magic vars/literals; anything unrecognized
  refuses.  True multi-switch (`package A; …; package B;`) stays a ruled
  refusal; s345 §2's "ZERO measured events" premise is SUPERSEDED for the
  leading-statements shape.
- **An eval's trailing `our`/`my` declaration has a VALUE, and it is whatever
  perl gives** (#227, s342f): `our NAMES [OP= RHS]` → the assignment expression
  (or, with no init, the READ — v1 answered with the variable NAME, a
  silent-wrong); `my ()` → the EMPTY LIST, emitted as the `(progn)` a bare `()`
  lowers to (`(p-undef)` is a 1-element list, `(vector)` an ARRAY ref — both
  measured wrong); a bare multi `my ($c,$d)` → the LIST of its names, lowered
  through the expression machinery with `'inherit'` so both context rules come
  for free.  `eval "our $VERSION = '…'"` is a routine module idiom, and it was
  the entire CPAN half of audit family F2.
- **A BINDING of a name is not a use of it** (s342d, #229) — the same mistake
  made twice, in two different checks, each costing a perl CORE module a
  whole-file gate.  `$attrs{$_}` is a slot of `%attrs`, so a TEXT scan for
  `$attrs` in a `my $attrs = …` initialiser is wrong: ask PPI's `->symbol`,
  which canonicalises `$a[0]`→`@a` and `$a{k}`→`%a`.  And `my ($vobj, $err);`
  DECLARES `$err`, so it is not evidence that a package global `$err` is live
  (`our` still is).  Climb to `PPI::Statement::Variable`, not merely the
  nearest `PPI::Statement` — for `my (…)` that is the Expression in the parens.
- **A `pipeline=v1` cache grep UNDER-COUNTS the live v1 share** (s342c): it is
  blind to every route whose output never becomes a cache entry — eval-strings,
  `fresh_perl` children, temp `.t` transpiles.  The s341 grep saw 2 of 60.  The
  measurement tool is `PCL_V2_AUDIT_LOG=<path>` (a file side-channel in
  `pl2cl`), which also splits **TODO** (real v2 gap) from **DIE** (v2 correctly
  raising a Perl-level error that the fallback needlessly retries on v1 — no
  work, self-resolves at the flip).  `PCL_V2_VERBOSE` cannot do it: stderr is
  folded into TAP by the sweep.
- **Tie machinery must stay OUT OF LINE** (s342): `unbox` is `declaim`ed inline
  and `p-scalar-=` expands into generated code, so an `unwind-protect` written
  at either site multiplies across the whole image — the first attempt made
  SBCL exhaust a 1 GB heap compiling `Pl/t/socket-01.t`.  The macro has exactly
  three callers, all `defun`s.
- **The CPAN board's FAIL rule is "zero ok", so it counts files perl itself
  SKIPS as failures** (s343, `docs/cpan-board14-survey-s343.md`): 12 of 53
  FAILs on the 14-dist board are author-only tests (`1..0 # SKIP`) or files
  perl cannot run either.  **Always run the perl oracle beside the board** —
  real failures were 41, not 53.
- **The perl oracle must NOT get `-I<dist>/lib` for an XS dist with no built
  `.so`** (s344, `docs/cpan-board14-partials-s344.md`): perl then dies at `use`
  and produces no TAP, so real failures read as "board artifact" — that is how
  21 Scalar-List-Utils files (110 rows) were first misread. Run the oracle
  against the *installed* module, and record that it may be an older version.
- **The 635 PARTIAL rows of the 14-dist board are 8 families, 3 of them 85%**
  (s344, #231, `docs/cpan-board14-partials-s344.md` + `-partial-causes-s344.tsv`):
  Text-Balanced 300 (goto/tagbody #232, scope, extract offsets #237),
  `caller()` fidelity 127 (#233 — `caller` returns 4 elements not 3, filename is
  the generated `.lisp`, `$0` is `sbcl`, `#line` ignored), List::Util shim 110
  (#238). Nothing there is a board artifact.
- **There is no module called `XSLoader` to support — it is the XS boundary**
  (s343).  `Can't locate loadable object for module …` is PCL dying *exactly*
  as perl would without the `.so`, which is deliberate and load-bearing (it is
  what makes dual-life modules fall back to pure Perl).  The literal
  "module **this module**" is `XSLoader::load()` called with NO arguments —
  perl 5.10+ infers the caller's package, PCL's shim does not; worth fixing for
  diagnosis, not a blocker.

## s345 (Fable, 2026-08-06): s341–s344 batch reviewed + the E4.1 pre-work ruled

Full rulings: `docs/fable-answers-s345.md`.  All eleven commits (`0e73b13`…
`fd20bb9`) approved as shipped; gate independently re-verified 131/4629 PASS.

- **#228 ASK → REGISTER, never tolerate NUL** (§1): `[perl #129069]` joins its
  five NUL-byte siblings in the skip-registry, IN the E4.1 step-2 flip commit
  (earlier and the stale-detector fires — the row still passes via lenient
  truncation).  Pass-baseline row leaves by EDIT.
- **#226 → qualified emission APPROVED** (§2): a leading-`package X;` eval
  lowers AS section X through the D1-lite nested-package QUALIFIED emission —
  no new mechanism.  Five blast-radius probes + the s342g INVERSE guard are
  the acceptance tests; audit F1 events must reach 0.  Residual multi-switch
  stays refused but must reach `$@` perl-shaped
  (`PCL: unsupported in string eval: …`), rephrased at the flip.
- **#230/F3 → route through #78** (§3): the #26 gate guards the v1-seam HOIST
  of `--anon-block-N--` defuns out of their lexical `let`; the inline_lambda
  re-host removes that path, so **#78 is E4.1 pre-work**.  Never a new
  mechanism; in-place-defun fallback only if #78 exceeds ~2 sessions, as an
  ask.  **#230/F6 → SPLIT the oversized run form** at statement boundaries;
  never raise the limit.
- **Board tasks prioritized** (§4): fillers #236 (explain — diagnosis
  multiplier, do first) → #234 (silent-wrong filetest-before-`=>`; fix at the
  filetest-default mechanism, probe the breaking case) → #235; post-E4.1
  convergence #232 → #233 (cheap faces first; filename/#line/frame-hiding is
  an ASK before building) → #237/#238/#239.
- **Queue**: #78 → #226 → #230 → E4.1 steps 1–4 → STOP, Fable takes #153/E5.0
  steps 1–2.

## s346 (Opus, 2026-08-06) — #78 DONE: block-form arg bodies host IN PLACE

- **A `&`-prototype block arg lowers as an inline lambda at the call site**, not
  as a hoisted `--anon-block-N--` defun: `Pl/PExpr.pm`'s block-proto branch asks
  `_v2_embedded_body($block,'sub')` (the anon-`sub {}` sibling's own route), and
  when that DECLINES asks v1 for a LAMBDA (`$return_lambda=1`), never a defun.
  The second half matters as much as the first: the seam localizes the embed
  hook OFF during its discarded native attempt, so the old code left a DEAD
  defun in the bucket whose text still tripped the gate.
- **F3 is CLEARED**: live-v1 board events 8 → 0 (`docs/v1-live-share-audit.md`).
  Corpus emission identical; gate 131/4633; sweep 0 new/0 fixed/0 LOST.  Board
  statuses match the s343 snapshot except S-L-U `reduce.t` 21 ok/11 → 23 ok/9
  (two v1 defects, both now agreeing with perl).
- **The #26 gate STAYS as an unreached backstop** — the three decline shapes
  (`package` stmt, named sub, `use`) all lower in place and match perl, so it
  has no producer; deleting it is E4.1 step 3's reachability job, with its
  three proofs, not a side effect of this change.
- **Queue now**: #226 → #230 (F6 split; F3 half done) → E4.1 steps 1–4 → STOP.

## s346b (Opus, 2026-08-06) — #226 DONE: a leading-`package X;` eval lowers AS X

- **The fix is SUBTRACTION**: the leading `package X;` is no longer CONSUMED at
  segment level, so it reaches `_lower_block`'s D1-lite nested-package path,
  which pushes X onto the Environment while the SECTION package stays the
  eval's root — the `current ne cur_pkg` condition the QUALIFIED emission
  already keys on.  Three supporting facts, each fed to an existing mechanism:
  (1) skip the segment-level sub extraction for that segment (it runs before
  lowering and would name subs unqualified — the s342g silent-wrong);
  (2) the package ENTER forms lead the eval BODY, ahead of the defs/sched
  interleave (a `use` is in sched, and its import records the package in
  effect — Role-Tiny create-hook.t); (3) that `use` gets `:into "X"`, supplied
  by setting the two facts v1's EXISTING `:into` branch keys on
  (`_seam_outer_pkg`, `_block_depth`), never a second predicate.
- **F1 board events 18 → 0.**  All five ruling probes + the s342g inverse guard
  pass with zero v1 routes.  Gate 131/4640; corpus emission identical; sweep
  0 new / 0 fixed / 0 LOST.
- **`our` declared in the region and read back UNQUALIFIED stays REFUSED** (v2's
  native emitter lacks ExprToCL.pm ~900's our-qualify branch; probed 0 vs
  perl's 10) — task **#240**.  A silent-wrong is never shipped to close a
  family; the narrow v1 retry is.
- **A refusal's TEXT is load-bearing before the flip**: `parse_with_fallback`
  keys the v1 retry on `/^Parser2\b/`, so the perl-shaped rephrase of
  eval-mode's residual refusals belongs to the E4.1 step-2 commit, not earlier.
- **Board labels are not the measure**: `role-basic-composition.t` went
  PASS(8 ok) → PARTIAL(10 ok / 3 not-ok) because it now runs 13 rows where it
  ran 8 — more coverage AND more passes.  Read rows, not the PASS/PARTIAL bit.
- **Queue now**: #230 (F6 split) → E4.1 steps 1–4 → STOP, Fable takes #153/E5.0.

## s347 (Fable, 2026-08-06) — review of s346–s346d: all four commits APPROVED; the two asks ruled (`docs/fable-answers-s346.md`)

- **Standing rule (from ask 1a, CONFIRMED)**: when a ruling's acceptance is met
  but a hole is found beside it — gate the hole LOUDLY, clear the family, file
  the task with the measurement.  Never ship a silent-wrong to close a family.
- **#240 split (ask 1b)**: step 1 is PRE-FLIP — narrow the eval-region `our`
  gate to DECLARE-THEN-USE only, because the gate as written refuses the
  routine write-only idiom (`eval 'package Foo; our $VERSION = …; 1'`, probed
  s347) and would break it user-visibly at the flip.  Step 2 (the two-half
  emitter fix) parks but is SCHEDULED: first post-E4.1 compiler item, or with
  E5.4.  §5a.2 binds MEASURED events, not synthesized probe shapes.
- **§5a.3 AMENDED**: "zero eval-mode fallbacks" → "zero UNEXPLAINED eval-mode
  fallbacks"; ruled refusals (multi-switch, #240 read-back, F6 if it lands
  there) are excepted, each REQUIRED to have perl-shaped `$@` text and a
  not-supported entry by the step-2 commit.  A not-supported entry that names
  its own remover (owner task) is not a write-off.
- **F6 re-scope ACCEPTED (ask 2)**: the run bucket is already per-statement, so
  the s345 "chunk the bucket" wording described an existing split.  Locate
  first (audited sweep); top-level-`my` shape → extend `_oversized_top_decls`
  (reuse); generic let-body chopping REJECTED as a first move; eval/fresh_perl
  source → NO pre-flip fix, ruled refusal + baseline row edit instead.
- **Review find, verified live**: #226's `_fallback_stmt_capture` edit also
  fixed a pre-existing file-mode silent-wrong — `sub f { package X; use M; }`
  imported into main before, X after.  Unclaimed, therefore unguarded:
  guard row REQUIRED in #240 step 1's commit.
- **All eval-mode refusal rephrases land in the E4.1 step-2 commit, none
  earlier** — `parse_with_fallback` keys the v1 retry on `/^Parser2\b/`, so an
  early rephrase converts a silent retry into a user-visible die.
- **Queue**: #240 step 1 → #230/F6 locate+fix-or-rule → E4.1 steps 1–4 →
  STOP (Fable #153/E5.0); post-E4.1 compiler queue gains #240 step 2 at its
  head.

## s348 (Opus, 2026-08-07) — #240 step 1: the eval-region `our` gate narrowed to DECLARE-THEN-USE

- **The gate now refuses only a `our` name that is USED AGAIN in the region**
  (read, later write, interpolated read, read inside a nested sub, one member
  of a list decl).  A **WRITE-ONLY `our` collapses natively**:
  `eval 'package Foo; our $VERSION = "1.25"; 1'` and
  `our @ISA = ("Exporter")` are v2-native and correct — they were v1 retries
  at s346 and would have died user-visibly at the E4.1 flip.
- **The mechanism is `_block_captures_name` + one new option, `our_targets`**:
  an `our` declaration's targets count as declarations rather than uses, but —
  unlike `my`/`state` — never as SHADOWS, so a later use of the same canon is
  still a use.  Sigil-exact canons, the inner-`my` shadow rule and the
  string/regex/heredoc conservatism come along unchanged (reuse, rule 11).
- **One arm the ruling did not specify**: the gate also fires when the region
  declares an `our` AND contains a `Cast`+`Block` (symbolic) deref.  A symbolic
  ref names the variable WITHOUT a sigil, so no token scan can attribute it;
  `%p-symref-box` interns an unqualified name in `*package*` (the CALLER's CL
  package) while `_lower_our_decl` qualified the write into X.  Probed:
  `eval 'package D1; our $Z = 5; my $n = "Z"; ${$n}'` → undef, perl 5 — NEW
  from the narrowing, so gated in the same commit rather than shipped.
- **#240's silent-wrong is WIDER than the `our` read-back, and it is s346's,
  not step 1's** (verified identical at `41907a9` in a worktree): EVERY
  unqualified package global in an eval package region binds to the CALLER's
  package.  `eval 'package F2; $Zz = 5; 1'` → perl `$F2::Zz`, PCL `$main::Zz`.
  Cause, one layer up from the ruling's: the eval-mode free-var scan makes the
  name a `p-eval-thunk` parameter and `p-eval-lex-lookup` resolves an
  alist-miss with `(intern … *package*)`.  `our` escapes only because its write
  is qualified; the read-back is the same bug from the other side.
- **MEASURED before deciding: the wider hole has ZERO live events.**  All 20
  eval-region collapses across the entire F1 source (Role-Tiny 23 files +
  Try-Tiny 11) have an EMPTY free-variable set — every one is the
  `package X; use Role::Tiny; …` idiom.
- **NOT gated, and why**: the only compile-time predicate covering it ("the
  region has a free variable") cannot tell a package global from a caller
  lexical — that distinction exists only at the runtime alist lookup — so it
  would also refuse the legitimate `my $x; eval 'package Foo; sub f { $x }'`,
  the exact over-firing the s347 §1.2 ruling reversed the s346 gate for.
  Escalated as the s348 ask (`docs/opus5-review-requests-s348.md` §2).
- **Possible re-scope of #240 step 2**: passing the region package X into
  `p-eval-thunk`, so `p-eval-lex-lookup` interns an alist-miss in X, appears to
  fix the wider hole AND the ruled read-back (the qualified `X::$Z` write and
  the bare read would resolve to the same symbol) — without the two-half
  emitter fix and without touching native variable naming in the deletion
  window.  NOT implemented: it is the parked step 2, and its blast radius
  (which specials can reach that miss path) is unmeasured.
- **The file-mode `sub f { package X; use M; }` guard row is in**
  (`Pl/t/use-require-01.t`, 3 rows) — imports into X, NOT main, callable.
- Gate 131 files / **4648** PASS; corpus emission identical across 111 files;
  CPAN board (Role-Tiny + Try-Tiny) identical to HEAD but for
  `extend-role-tiny.t`, which reproduces at HEAD (drift on task #208).
- **No cache-generation bump**: eval transpiles are in-memory only
  (`*p-eval-string-cache*`); `p-transpile-string` never touches `~/.pcl-cache`.

## s349 (Fable, 2026-08-07) — review of s348: #240 step 1 approved; step 2 RE-SCOPED to the runtime route and PROMOTED pre-flip

Full rulings: `docs/fable-answers-s348.md`.  Gate independently re-verified
(131 / 4648 PASS); every s348 acceptance row and both measurements re-probed
live and reproduced; routing verified via `PCL_V2_AUDIT_LOG`.

- **s348 approved as shipped, symbolic-deref arm included** — the arm is the
  standing gate-the-hole rule applied correctly (the narrowing would have
  opened a NEW silent-wrong; over-fire keeps the correct v1 retry).
- **The wider hole gets NO interim gate** — (ii)-as-gate's plumbing (region
  package threaded into `p-eval-thunk`) is the whole cost of the FIX, so the
  interim artifact is waste.  (iii) parser gating on free variables stays
  REJECTED (over-fires on the legitimate caller-lexical capture).
- **#240 step 2 RE-SCOPED and PROMOTED pre-flip**: the s347 parking reason
  (native-emitter surgery in the deletion window) does not apply to the
  runtime route.  **Mechanism ruled: bind `*package*` to X's CL package around
  the thunk's free-name resolution AND body** — NOT a patch to
  `p-eval-lex-lookup` alone.  Two review-added probes force this: a
  lookup-only fix REGRESSES `eval 'package D2; $W = 7; my $n = "W"; ${$n}'`
  (accidentally right today: write and symref read both mis-land in caller,
  value 7 correct while both package slots are wrong), and the hole has a
  READ spelling too (`$main::G9 = 9; eval 'package X9; $G9'` → PCL 9, perl
  undef — a caller global wrongly satisfies a region read).  The binding
  covers all three spellings; the step-1 gate + Cast+Block arm are then
  DELETED in the same commit (rows flip to native-and-correct with inverse
  guards).
- **Measurement first, stop-rule attached (one-session cap)**: instrument
  `p-eval-lex-lookup`'s miss path across sweep + board — piggybacked on
  #230/F6's audited locating sweep, one run serves both — plus a per-site
  survey of the runtime's ~10 `*package*` fall-throughs.  If a magic/special
  name reaches the miss path, or the `our`-write container does not converge
  with the thunk parameter, STOP: ship (ii)-as-gate (die at the miss — value
  arm of rule 12; inside a string eval that is `$@` + undef, loud and
  perl-shaped) and step 2 returns post-E4.1.
- **Queue**: #230/F6 (carrying the miss-path instrumentation) → #240 step 2 →
  E4.1 steps 1–4 (refusal-rephrase list shrinks to multi-switch +
  F6-if-applicable) → STOP (Fable #153/E5.0).  Post-E4.1 queue head reverts
  to the board items (#232 → #233 → …).

## s350 (2026-08-07, Opus) — #230/F6 measured and CLOSED; #240 step-2 instrumentation run, stop-rule did not fire

Detail: `docs/eval-region-measurements-s350.md`.  No behaviour change; gate
131/4648 PASS, sweep gate clean (0 new / 0 fixed, TOTAL 18498 = baseline).

- **F6 = `perl-tests/tr.t:474-498` (RT #132608), a STRING EVAL** — a runtime
  `tr///` whose replacement list is 40960 chars, lowering to a 73769-char run
  form.  `parse_code`, so the s346 §2.3 branch applies: **no pre-flip fix,
  ruled refusal.**  `$RUN_FORM_MAX` unchanged.  **Flip cost MEASURED: 2 tr.t
  rows** (241→239 passing; the file already aborts at exactly those rows) —
  edit them into `baselines/fail-baseline.tsv` with their cause, plus the
  not-supported entry, IN THE E4.1 STEP-2 COMMIT (never earlier: rephrasing a
  `Parser2 TODO:` before then turns a silent retry into a user-visible die).
- **#230 is closed** — F3 was already routed through #78 (s345) and the #26
  gate stays an unreached backstop for step 3 to retire.
- **The sweep's whole v1-fallback inventory is 18 events and all are named**
  (F4/#228 ×6, F2 residual ×5, multi-switch ×1, F6 ×1, 5 `DIE` = v2 correctly
  reporting a perl error).  §5a.3's "zero UNEXPLAINED" is satisfied for the
  sweep half.  **The F2 residual costs nothing and likely GAINS 4**: its four
  `my $$x` strings are `eval.t:239-246`, blessed FAILURES today because PCL
  accepts what perl rejects (principle 9); the refusal makes `$@` non-empty
  and they pass.
- **#240 step-2 stop-rule did NOT fire.**  Miss-path listing over the whole
  sweep: 2720 events, 29 distinct names, **zero punctuation/magic variables**.
  The `*package*` consumer survey is 13 real runtime sites, every one
  implementing "unqualified → current package" — the binding moves all
  thirteen toward perl, none away (`p-resolve-invocant` is the only one that
  *removes* a resolution, and removing it is the correction).
- **Two sizing facts step 2 must now assume, neither available at s349**:
  (a) the s348 "20 collapse events, all EMPTY free sets" was measured on the
  module SOURCES — under the running board it is **108 region events, 86 with
  free names** (Class::Method::Modifiers' installer); the alist wins for every
  real capture and the only redirected names are 50 phantom `$method`s (a
  `_eval_scope_free` false positive: the eval string declares it with `my`);
  (b) **a region with no free names emits no `p-eval-thunk` at all**, so the
  binding cannot live only on the thunk's parameter path — the body is where
  all 108 events feel it.
- **Instruments to DELETE with the step-2 commit**: `PCL_EVAL_LEX_MISS_LOG`
  (`%p-eval-lex-miss-audit`, `cl/pcl-runtime.lisp`) and `PCL_EVAL_REGION_LOG`
  (`_assemble_eval_mode`, `Pl/Parser2.pm`).
- **Queue**: #240 step 2 → E4.1 steps 1–4 → STOP (Fable #153/E5.0).

### s350 addendum — the step-2 wrap fork PROBED away (one mechanism, not two)

A region with no free names emits no `p-eval-thunk`, so step 2 must wrap its
body — and wrapping is what `_cap_inlining_if_huge` refuses to do to
`eval-when`/`p-sub`/`defvar`/`p-defpackage`.  **Probed with
`PCL_EVAL_REGION_WRAP=1` (forces the wrap on every #226 region): emission
really changes, and every row of Role-Tiny + Class-Method-Modifiers + Try-Tiny
(48 files, cold cache) is IDENTICAL**; sub installation, `use` import target
and `our` read-back all still match perl.  **So step 2 uses ONE mechanism** —
emit the thunk whenever a region package is present and bind `*package*` on
`p-eval-thunk`, per s349 §2c.  WHY the wrap is safe (do not re-derive): eval
mode has no compile-file phase to lose — `p-eval` reads/evals form by form and
the region's defs are already hoisted inside the one eval'd unit; the
`_cap_inlining_if_huge` prohibition is about FILE-mode top-level forms.
Third instrument to delete with step 2: `PCL_EVAL_REGION_WRAP`.

## s351 (2026-08-07, Opus) — #240 step 2 SHIPPED: `p-eval-thunk` binds `*package*` to the region package; #240 CLOSED

Mechanism ruled s349 §2c, measured s350, implemented here.  The whole of
E4.1's pre-work is now done.

- **`p-eval-thunk (free-names fn &optional region-pkg)`.**  When REGION-PKG is
  given it binds `*package*` to that CL package (find-or-create with
  `:use '(:cl :pcl)` — the `(setf %p-symref-box)` convention) around BOTH the
  free-name resolution and the body.  `p-eval-lex-lookup` is UNCHANGED: its
  miss path already interns in `*package*`.  Normative text:
  `docs/ir-spec.md` §string eval piece 2, `docs/eval-lexical-capture.md` §3.
- **The emitter passes it only from the #226 collapse** (`_assemble_eval_mode`),
  as `_cl_pkg_designator`'s spelling, recorded where the enter forms are built
  (`_lower_block`'s Statement::Package branch) so the perl-name → CL-name rule
  is never derived twice.  **The thunk is emitted whenever a region package is
  present, even with an empty free-name list** — the s350 §6 forced-wrap probe
  measured that shape as row-identical over 48 region-heavy files first.
- **All three spellings of #240 are closed** and agree with perl: a bare write
  (`eval 'package F2; $Zz = 5; 1'` → `$F2::Zz`, not `$main::Zz`), a bare read
  that must NOT see the caller's global (`$main::G9 = 9; eval 'package X9;
  $G9'` → undef), and `our` declared-then-read-back (`package F1; our $Z = 5;
  $Z * 2` → 10).  The D2 regression the s349 review predicted for a
  lookup-only fix does not occur: value AND package slot are both right.
- **The step-1 gate is DELETED** — `_eval_region_our_readback`, its
  `Cast`+`Block` arm, and `_block_captures_name`'s `our_targets` option (the
  gate was its only consumer) are gone; a single-region eval now refuses
  NOTHING.  Only the multi-switch shape (two `package` statements in one eval)
  is still refused, and it keeps its `Parser2 TODO:` prefix until the E4.1
  step-2 commit.
- **All three s350 instruments are deleted**: `PCL_EVAL_LEX_MISS_LOG`,
  `PCL_EVAL_REGION_LOG`, `PCL_EVAL_REGION_WRAP`.
- Guards: `Pl/t/transpile-test-09.t` "eval package-region: unqualified names
  resolve in X" (10 assertions against the perl oracle) for the VALUES;
  `Pl/t/parser2-02.t` for the shape, including the INVERSE guard that a
  region-less eval gets NO region argument.
- **No cache-generation bump**: file-mode emission is byte-identical across
  all 111 corpus files (`tools/corpus-diff.pl`), and eval transpiles are
  in-memory only (`*p-eval-string-cache*`).

## s358 (2026-08-08, Fable) — s357 approved; #252/#243 USER-decided; Text::Balanced restored

- **s357 (#244 step-4 verification) APPROVED as measured** — gate, the
  15,129-row arithmetic, the pre-flip `pipeline=v1` attribution, the board
  totals and the 27-file #243 blocker all independently reproduced.  One
  nit: the Text-Balanced dist carried 780 ok rows (766 is the NET board
  delta after +14 expected gains) → `docs/fable-answers-s357.md` §1.
- **The audited populations are FOUR: sweep + CPAN board + Pl/t gate +
  the perl suite** — a closed list; any "zero events" precondition must
  name all four (§5a.2 re-amended; same miss happened twice) →
  `docs/fable-answers-s357.md` §2.
- **A live-v1-style audit runs on a COLD cache** — transpile-time
  instruments report zero for exactly the already-cached modules (how the
  audited board missed Text::Balanced) → `docs/fable-answers-s357.md` §2.
- **#252 = SPLIT (USER, 2026-08-08): phase 1 (Text::Balanced) executed
  s358; phase 2 (capture/spanning + poisoned-`my` families) is a PLAN
  awaiting scope approval** → `docs/e41-suite-families-plan.md`, task #254.
- **#243 = PORT the 27 Pl/t files off v1's file entry (USER, 2026-08-08)**
  — then complete the step-3 deletion; assertions stay semantically
  equivalent, v1-implementation-detail rows are dropped with a note, never
  weakened → task #255, `docs/fable-answers-s357.md` §3.
- **Forward `goto` to standalone labels is GENERAL for the all-forward
  case** — nested catches, one per label, each enclosing everything before
  its label; leading decls handled by ordinary my-branch nesting (NOT
  hoisted).  Backward/mixed keep the tagbody machinery + gate →
  `docs/fable-answers-s357.md` §4.1, guard rows in
  `Pl/t/transpile-test-09.t`.
- **A textual "reads $name" scan must not fire on `$name[`/`$name{`**
  (elements of @name/%name, not the scalar) — `_reads_name_rx`, four scans
  routed through it; the list-decl self-ref path emitted an unbound
  package-var read on the false positive →
  `docs/fable-answers-s357.md` §4.2.
- **Interpolation of a subscripted variable continues its chain**
  (`"$_[0]->{k}"`, `"$a[0]{k}"`) — chains parse via PPI when a second
  group follows; a lone subscript keeps the legacy path byte-for-byte →
  `docs/fable-answers-s357.md` §4.3.
- **op/for.t (#253) is an upstream PPI 1.291 lexer bug, not a PCL
  regression** — `for ${*$f} (LIST) { }` kills the PPI lexer whole-file;
  its s323e snapshot rows were `--lenient-ppi` truncation (the #228
  accident class), retired by ruling s356 → `docs/ppi-upstream-bugs.md`
  §6; registration with cause via #254 §4.

## s358i (2026-08-08, Fable) — E4.1 COMPLETE: v1's file-level transpile surface deleted (#243/#255)

- **All 27 Pl/t files ported to Pl::Parser2** (#255, `5ba6155`); porting
  rule applied throughout: assertions stay semantically equivalent,
  v1-implementation-detail rows dropped with notes (#132 echoes, call
  qualification), one perl-vs-v2 divergence TODO-marked (#256:
  retroactive prototype application to pre-declaration calls).
- **Pl::Parser::parse survives ONLY as the prototype-collection walker**
  (`2d9aa8b`) — the extractors are its sole callers
  (collect_prototypes_only => 1, _emit no-op); a guard dies on any other
  use.  parse_file/parse_code and lenient_ppi/_ppi_with_fallback are
  DELETED.  Statement/expression v1 code remains live through Parser2's
  seam, which never calls parse().  E5 burns that seam down.
- **Two production silent-wrongs found by the port, both fixed
  (`5ba6155`)**: (1) the v2 prototype premerge now seeds transpile-time
  inc_paths from the document's literal `use lib` paths (v1 got the
  ordering free; the pre-pass didn't, so a file's own lib dir lost its
  modules' block-form prototypes); (2) `_merge_module_prototypes` tags
  merged entries from_module and never overwrites a LOCAL declaration
  (v2's sub hoisting put the seam re-merge AFTER the local override,
  which it clobbered — wrong auto-boxing on every later call).
  Local-before-use divergence (perl's import clobbers with a warning; PCL
  keeps the local) accepted, noted in #256.

## s359 (2026-08-08, Fable) — #153/E5.0 steps 1–2: the term-grammar walker, decline-not-guess

- **`_term_extent` is THE term-grammar walker** (`term := cast* primary
  postfix*`, Pl/PExpr.pm) — and **undef is a first-class answer**: bare
  words, prefix operators, `->method(args)`, cast-block slice groups
  (`@{$r}[0]` — PPI spells the group as a Constructor) DECLINE so the
  call site keeps its legacy derivation.  The walker never guesses and
  never stops inside a term.  Unit tests: Pl/t/reduce-term-01.t.
- **Site flips are measured, then flipped**: `PCL_TERM_DIFF=1` dual-run
  probes at both operand sites; the s359 inventory over all 111 corpus
  files found ZERO real disagreements, and `defined` was flipped onto the
  walker (corpus byte-identical, gate 132/4717, cold-cache sweep GATE
  clean, TOTAL 18499 = baseline).  Steps 3–5 (per-site migration,
  reduction fold-in, `$end_pars`/`$deref_skip` deletion) = Opus, recipe
  in task #153.  Design: docs/pexpr-term-parsing-review.md (Option B).
- **PPI gotcha (test infra)**: a token array goes HOLLOW when its
  PPI::Document is garbage-collected (recursive DESTROY); keep the
  document alive when holding tokens.  Noted in reduce-term-01.t.
- **Bug-hunt sequencing recommendation** (RULED s360, see below):
  docs/bug-review-s359.md — no new campaign now; #254 first if approved;
  big hunt (widened board + full suite + fuzzer with two new axes) is the
  E5 exit gate, pre-R2.

## s360 (2026-08-08) — USER rulings: #254 approved, big bug hunt deferred

- **#254 APPROVED** ("so we can get a functioning compiler"): fix scope of
  `docs/e41-suite-families-plan.md` §§1–3 authorized — capture/spanning
  (9 suite files) + poisoned condition-`my` (4 files), ~12k rows; Opus
  executes, session 1 = measurement, recipe in task #254.  **§4's residue
  registration (the other 15 dark files) is NOT yet signed off** — comes
  back with real shapes after measurement.
- **Big bug hunt DEFERRED** ("in the future when things are stable"):
  the docs/bug-review-s359.md §4 recommendation accepted — E5 exit gate,
  pre-R2; the four standing audit populations remain the hunt until then
  (ruling recorded in that doc's §6).

## s373 (2026-08-09, Fable) — s372 review: both commits APPROVED; five asks ruled

- **s372 APPROVED as shipped** (`docs/fable-answers-s372.md`): gate
  independently re-verified 132/4747; #269's numbers reproduced from the
  suite file; #271's one-file population reproduced by grep; four live
  block-boundary probes vs perl all match.
- **The DIE-SCAN is a ruled NARROWING of the gate-SET bar** for one edit
  class: when the sole non-comment edit is *decline → die on a
  value-returning helper*, emission is identical-or-die by construction, so
  the bar is met by a both-populations die-scan (exit codes checked, every
  pre-existing non-zero triaged) + corpus-diff + the sweep TOTAL/LOST
  verdict.  All three legs required; missing any leg → full file-by-file
  gate-SET diff as ruled (`fable-answers-s372.md` §2).
- **#272's un-widened boundary is probed COSTLESS**: embedded `my` in
  `map`/`sort`/`grep` BLOCKs and a bare file-level block all match perl
  today without the rename — boundary stands, NO residue task.  If a block
  divergence ever surfaces, widening must first answer per-iteration
  freshness (§3).
- **#271 goes BEHIND #153's FOLD** (one-file population; the fix lives in
  the `$end_pars` maze `pexpr-term-parsing-review.md` forbids patching);
  the `pipe my ($r, $w)` shape joins the FOLD's acceptance probes.
- **#269's park behind #196 CONFIRMED** — re-opens when #196 moves, not
  before.

## s372 (2026-08-09, Opus) — #274 shipped; #269 measured and PARKED; #272

- **A FAILED ALIAS ANCHOR DIES AT EVERY k** (#274, ruled s371 §2, shipped
  `033f088`): the v1 seam's sole-element `_apply_foreach_alias_rewrite` no
  longer swallows a non-outermost head — same die as the k>1 site.  Measured
  before flipping: 751 sources across BOTH populations (528 perl-suite + 223
  CPAN-board), **0 new dies**; corpus-diff identical; sweep GATE clean, TOTAL
  18499 (baseline 18498).
- **The two foreach comma walks now carry PAIRED invariant comments** naming
  each other (`_foreach_scalar_elements` ↔ `_ev_foreach_alias_list`): on a
  QUALIFYING list they provably agree; the veto is deliberately a SUPERSET
  elsewhere.  A third walk in the family reopens the shared-primitive question.
- **#269 is PARKED BEHIND #196, measured not guessed**: `re/reg_eval_scope.t`'s
  snapshot `C_ok` is **5** of perl's 48, and all 47 assertions exercise
  `(?{ … })` blocks, which PCL does not run at all — so de-gating buys ~0
  verified rows.  The gate itself names a REAL capture; do not delete it.
- **A gate-SET compare must normalize the COMPILER'S OWN ROOT, not just line
  numbers** — the emitted preamble embeds it (`*pcl-pl2cl-path*`, the @INC
  pushes, `*p-core-inc-dirs*`: task #217), so a worktree-vs-tree diff reports
  EVERY file as changed until both roots fold to one token.
- **The embedded-`my` veto's condition is "inside ANY sub body", not "inside a
  NAMED sub"** (#272) — whether the body has a name says nothing about who can
  see its lexicals; the anon spelling fell through to the package global and
  crashed.  `_enclosing_named_sub` → `_enclosing_sub_body`.

## s371 (2026-08-09, Fable) — s370 review: #267 APPROVED; asks 1–4 RULED

- **s370 APPROVED as shipped** (gate independently re-verified 132/4744;
  ten live probes vs perl identical, incl. same-scalar-twice, k=2
  vivification, named loop var) → `fable-answers-s370.md`.
- **Probes + guard rows ARE the verification bar when a shape occurs in no
  corpus** — do NOT widen the population (new axis; file-don't-grow).
  Corpus-diff-first is the standing cheap-first order → §1.
- **The k=1 foreach-alias anchor-miss must DIE like k>1** — a failed anchor
  is always a compiler self-inconsistency, never a benign decline.  Own
  filler commit, two-population gate SET + sweep TOTAL/LOST first → §2.
- **The two foreach comma walks (qualifier vs VarAnnotator veto) stay TWO**
  — on every qualifying list they provably agree; the veto is deliberately
  a superset on non-qualifying lists.  Paired invariant comments required;
  a third walk in the family reopens the question → §3.
- **Mixed-list residue CORRECTED**: `for ($x, @a)` DOES alias `@a`'s
  elements (boxes survive `p-flatten-args`); what misses is an
  ELEMENT-shaped slot (`$h{a}`) and `values %h` in a non-qualifying list.
  E5 axis, DO-NOT-START — but probe the right boundary → §5.

## s370 (2026-08-09, Opus) — #267 SHIPPED (both ruled commits); #273 filed

- **An all-single-scalar foreach list is `(vector E1 … Ek)` at EVERY k**, and
  each aliasable element carries its BOX head — the N=1 rule IS the N=k rule
  (#267, ruled s369 §2, shipped `f2c7c25` + `0e5b088`).  Resolver:
  `Pl::Parser::_foreach_scalar_elements` (depth-0 split via the SHARED #138
  splitter; all-or-nothing per list).  Boxes must NEVER pass through
  `p-flatten-args`.  **Mixed lists (`for ($x, @a)`) stay flattened and boxless**
  — aliasing an aggregate's elements is the boxed-aggregates axis (E5,
  DO-NOT-START).
- **A LITERAL foreach element is writable, not read-only** — `for ($x, 3)
  { $_++ }` where perl dies.  Probed, **pre-existing at N=1**; accepted
  divergence, `docs/not-supported.md`.
- **A gate-SET stderr diff must normalize compiler LINE NUMBERS**
  (`ROOT/(Pl|tools)/\S+ line \d+`) — a `.pm` that gains lines otherwise reports
  phantom drift on every file that warns.  18 such hits in s370, 0 real.
- **Run `corpus-diff.pl` BEFORE spending a full sweep**: identical emission over
  `perl-tests/*.t` proves the sweep's `.t` half cannot move, for minutes instead
  of an hour.
- **A sweep can FINISH and have its verdict swallowed** (#273): an orphaned
  `sbcl` + `pl2cl --server` deadlock inherits the run's stdout, so the pipeline
  never sees EOF, and **`timeout N` cannot kill it** (SBCL catches SIGTERM; the
  handler cannot run while blocked on the pipe).  Check `.faillog/_status.tsv`'s
  row count + mtime before believing a sweep is still running; reproduce the
  verdict from disk with `tools/sweep-diff.pl diff baselines/fail-baseline.tsv
  .faillog`.

## s369 (2026-08-09, Fable) — s368 asks RULED; #267 sizing decided

- **s367 + s368 APPROVED as shipped** (gate independently re-verified
  132/4739 cold; all probes reproduced) → `fable-answers-s368.md`.
- **#265 rename-not-narrow CONFIRMED; third-sibling pre-pass shape
  CONFIRMED** (populations disjoint via the complementary `$in_sub`
  guards) → `fable-answers-s368.md` §1.
- **#267: the N=1 rule IS the N=k rule** — an all-single-scalar
  multi-element foreach list emits `(vector E1 … Ek)` (per-element
  `_foreach_single_scalar_p`; depth-0 split = the shared #138
  `lowprec_idx` machinery, never a new comma scan); boxes must NEVER pass
  through `p-flatten-args`.  Mixed lists stay flattened, no boxes (E5
  axis).  **TWO commits**: wrapper switch first with a ZERO-change full
  sweep (TOTAL/LOST, cold cache) as its own discriminating measurement,
  then the per-element box verdict → `fable-answers-s368.md` §2.
- **#269 stays (capture is REAL, refusal conservative not blind)**; measure
  `reg_eval_scope.t`'s reachable rows before any session — `(?{…})` blocks
  not running is the #196 regex-engine axis → `fable-answers-s368.md` §3.
- **#271 layer call endorsed**: fix at argument-run lowering (`my (LIST)`
  contributes N args), never per-builtin at the runtime →
  `fable-answers-s368.md` §4.
- **NEW residue #272 (review probe, PRE-EXISTING at 88258a8)**: embedded
  `my` inside an ANON sub body still veto-refused → global read →
  type-error crash; the pre-pass keys on `_enclosing_named_sub`, correct
  condition is "inside ANY sub body".  Measure the route first.

## s368 (2026-08-09, Opus) — #265 closed: embedded `my` in a sub vs a same-named global

- **The embedded-`my` let-hoist's veto is RIGHT at file level and SCOPE-BLIND
  inside a named sub.** A sub mentioning the name can share a FILE-level
  `open my $fh` cell (Capture-Tiny's Utils.pm, #199) — it cannot possibly see
  a lexical declared inside ANOTHER sub's body. `++my $x->{k}` in `sub foo3`
  therefore wrote the package global and persisted across calls (op/my.t t47).
- **Narrowing the veto would have been WRONG — the fix is a rename.** Letting
  the hoist fire registers `$x` in `_seg_lex`, which suppresses the GLOBAL's
  forward defvar and strands the other sub. `$x__emb__N` is a name nobody
  else mentions: the veto stops firing AND `$x` keeps its defvar. (Probed
  both ways.) Rename root = the enclosing BLOCK, which is exactly perl's
  scope for an embedded `my`. → `_rename_vetoed_embedded_mys`, ir-spec §2b.3.
- **ONE veto predicate** (`_embedded_my_veto_names`), read by the refusal and
  by the pre-pass that removes the need for it — the s363 detector/rewriter
  rule, third instance after #264 and #265's promoter.
- **`__emb__N` strips in `_eval_lexical_alist`** like `__cond__`/`__shadow__`:
  the renamed decl is LET-BOUND, so a string eval naming the original `$x`
  still finds it (ir-spec §2b.4's first route). Probed live.
- Measured: gate 132/4739 PASS; `corpus-diff` **1 of 111 files** (my.t, the
  fix itself); CPAN board **0 of 223 sources changed**; perl-suite gate SET
  **2 of 523, 0 new dies** (my.t + pat_advanced.t, which has the same shape
  and gets the same fix); op/my.t 51/8 → **52/7 = its `perl-suite-run.tsv`
  snapshot**.
- **A HEAD-compare is INVALID if the tree changes mid-run** — bumping
  `*pcl-cache-generation*` during the first perl-suite gate-SET pass made
  194 of 523 files "differ" by their `;;; pcl: … gen=…` header line alone.
  Normalize the header, or finish the compare before touching the tree.
- **`grep` goes binary-silent on emitted CL too, not just `.tsv`** —
  pat_advanced.t builds `chr 0..255`, so its output holds NUL bytes and a
  plain `grep 'x__emb__'` printed nothing where `grep -a` finds 9 hits.

## s367 (2026-08-09, Opus) — #270 fixed: the `:prototype($)` silent statement drop

- **A prototype whose text ends in `$` mis-lexes TWICE** — PPI §7 turns
  `sub :ATTR` at expression start into a Label, and inside that run the
  closing paren of `prototype($)` is tokenized as the magic variable `$)`,
  so the attribute's paren group swallows the sub's block.  Affects `($)`,
  `(;$)`, `($;$)`, … ; `($$)`, `(\@)`, `($_)` lex correctly (probed).
  → `ppi-upstream-bugs.md` §7b.
- **The stolen `)` belongs to the ENCLOSING structure, so tree surgery
  CANNOT fix it** — the enclosing List is left unfinished and, inside a
  `for` list, the damage spreads across sibling statements (both measured
  s367 by building the hoist first and watching it destroy the text).  The
  repair is at SOURCE level on the RAW TOKEN STREAM, before any §7 tree
  surgery, then one `_reparse_doc` — the mechanism the state prepass already
  uses (`_state_reparse` renamed and shared).  → `Pl/Parser2.pm`
  `_repair_swallowing_prototypes`.
- **`Statement::Null` is INSIGNIFICANT**, so an `schildren` walk silently
  skips the `;` in `(;$)` — the tell was a stray `;` surviving into the
  rewritten source.  Walk `children` when you are reconstructing TEXT.
- **A `sub :` Label is only ever produced by this mis-lex**, so a run that
  does not end at a Block is known-mangled input: the §7 repair now DIES
  naming the shape instead of `next`ing into the silent statement drop
  (rule 12, value side).  Gate SET measured over both populations: 15
  sources carry a `sub :ATTR` spelling, stderr identical to HEAD on every
  one, `corpus-diff` identical across 111 files.
- **`run_cl` in `Pl/t/transpile-test-09.t` no longer folds transpile stderr
  into the `.lisp`** — a `PCL: …` announce at the top of the file is a CL
  read error, and an announce has no perl counterpart.  Stderr is still
  appended when pl2cl FAILS.  (Each `transpile-test-NN.t` carries its own
  copy of the helper; only -09 is fixed.)
- **§8 guard shipped**: PExpr's attribute strip announces a LIVE
  `prototype(...)` Attribute before dropping it — the residual silent path
  left by `_extract_prototype_attributes`' bail-outs.

## s366 (2026-08-09, Fable) — review of s365: all nine asks ruled, one new bug

- **All eight s365 commits APPROVED as shipped** — gate independently
  re-verified (132/4737), every semantic ask probed live against real perl.
  → `docs/fable-answers-s365.md`.
- **#254 and #252 are CLOSED** — worklist empty; A-ii parked behind E5
  (s364); residue lives in #265/#267/#269/#271 with their own snapshot bars.
  → fable-answers-s365.md §5.
- **A-i's ordering-independent-promotion design is STRUCK, not parked** —
  the s363 premise is marked SUPERSEDED in the measurement doc; nobody
  re-derives it.  → fable-answers-s365.md §1.
- **"One capture test, shared by promoter and gate" is a STANDING rule** —
  the promoter is the third instance of detector-and-rewriter-share-one-
  resolver.  → fable-answers-s365.md §9.
- **Filler-scope rule**: a filler may grow while the growth is the SAME
  mechanism, each widening step is measured against the gate SET over both
  populations, and new design axes are FILED, not fixed.
  → fable-answers-s365.md §4.
- **Timeout registry RATIFIED; the suite runner gets NO blind retry** — an
  unregistered slow file must surface as TIMEOUT so it gets a row with a
  cause; a row is deleted when the file gets faster.
  → fable-answers-s365.md §6.
- **NEW BUG (#270), found by review probes**: `sub :prototype($) {…}` at
  expression start is a SILENT STATEMENT DROP — any prototype text ending in
  `$` triggers a second layer of PPI §7 (`$)` lexes as the magic var, the
  block is swallowed into the attribute's parens) and the #268 repair's
  block-check declines silently.  Ruled: the decline DIES, the repair
  extends to the `$)` swallow, PExpr's strip gains a `^prototype\(` announce
  guard.  → fable-answers-s365.md §10.
- **#265 rename-half shape approved** (renamed lexical via shadow-aware
  `_rename_decl_within`; eval route = ir-spec §2b.4 let-bound → site alist),
  ordered BEFORE #267; #267 gets a SIZING step against the CLForm
  per-element sketch; #269 gets a PROBE step (the nested-sub capture may be
  real).  → fable-answers-s365.md §9, §11.

## s365 (2026-08-08, Opus) — #254's registration + A-iv/A-i/A-iii/B-ii, and #263

- **A per-file TIMEOUT ALLOWANCE registry exists**: `baselines/perl-suite-timeouts.tsv`
  (`rel<TAB>seconds<TAB>cause`), honoured by `tools/run-perl-suite.pl` as
  `max(seconds, --timeout)` and printed per run.  A file that needs longer than
  the default is registered WITH ITS CAUSE, never left to TIMEOUT into "no rows"
  (#176's lesson generalised).  First row: re/pat_advanced.t = 900 s.
  → `docs/e41-suite-families-s365.md` §0.
- **A capture/span refusal that is SIGIL-blind or SCOPE-blind is a bug, not
  conservatism** — three of #254's four remaining causes were exactly that, and
  all three are now deleted rather than narrowed: the scalar promotion's
  `family use (@x/%x/$#x)` veto (A-iv), `_block_captures_name`'s blindness to a
  `my` EMBEDDED in another statement (A-i), and the span detector's lack of a
  scope for a block-form `package Foo { … }` segment (A-iii).  Each one is the
  same standing rule one layer over: the pass that DETECTS and the scoping the
  program actually has must agree.  → `docs/e41-suite-families-s365.md` §1–3.
- **A-i needs NO extent design** (the s363 measurement's reading is superseded):
  op/getppid.t's sub never captured the file lexical at all — it declares its
  own `$first` in a statement-modifier `my`.  Ordering-independent promotion is
  NOT required and was not built.  → `docs/e41-suite-families-s365.md` §2.
- **A block-form package's lexicals never span**, but the outer lexicals it
  encloses still do: the fix SKIPS that segment's declarations, and must not
  kill the live set on entry (probed both ways).  → §3 of the same doc.
- **The cond-my rename is shadow-aware** (B-ii): `_rename_decl_within` skips an
  inner re-declaration's target and everything it shadows (`_ref_shadowed` —
  the span pass's resolver), so `multiple declarations` is waived for callers
  that rename that way.  → §4.
- **foreach aliasing is decided by ONE peeler** (`_foreach_list_unwrap`) shared
  by the rewrite, the single-scalar wrap and the annotator's veto — the two
  spellings (block form / statement modifier) wrap the list differently and the
  passes disagreed, which WAS #263.  The rewrite is anchored at the outermost
  call, so a wrong head guess is a no-op instead of an inner box.
- **Never run the Pl/t gate under `nohup`**: nohup ignores SIGHUP, perl then
  reports `$SIG{HUP}` as defined, and transpile-test-06.t's %SIG row fails
  against a PCL that correctly knows nothing of the inherited disposition.
- **The capture PROMOTER and the capture GATE must use the SAME test.**
  `_captured_in_subs` ran its own shadow-blind Symbol/ArrayIndex loops before
  calling the shadow-aware `_block_captures_name`, so a sub's OWN `my $x` uses
  promoted a same-named file lexical.  Harmless for a statement-level `my` (its
  let shadows the cell), SILENT WRONG for an EMBEDDED one (the embedded-`my` let
  is skipped for promoted names, so the sub wrote the shared cell).  Deleted
  (#265).  **Open sub-case**: when the name is a package GLOBAL used by another
  sub, the embedded-`my` veto is RIGHT to refuse a plain `let` — the symbol is
  defvar'd, so `let` is a dynamic rebinding; that shape needs a renamed lexical
  (`_rename_decl_within`).  → `docs/e41-suite-families-s365.md` §8.
- **An anon sub's ATTRIBUTE at the START of an expression is lexed by PPI as a
  LABEL** (`(sub :lvalue {…})` → `Label('sub :') Word('lvalue')`; inside a
  `for` list each label even gets its own STATEMENT) while the SAME text
  mid-expression lexes correctly.  Upstream bug, registered
  `docs/ppi-upstream-bugs.md` §7; repaired at the document level by
  `Pl::Parser2::_normalize_anon_sub_attrs`, with the `PPI::Structure::For`
  re-bless (a C-style `for` never has a loop VARIABLE before its parens) as the
  second half.  Before it, the statement became a PARSE ERROR comment — a
  SILENT CODE DROP, same family as #138/#259.  → `docs/e41-suite-families-s365.md` §7.

## s364 (2026-08-08, Fable) — review of s363: all seven asks ruled

- **All seven s363 commits APPROVED as shipped** — gate independently
  re-verified (132/4731), step-5 die guards adversarially probed (regex-token
  operands, empty operand: no death), all six diffs read.
  → `docs/fable-answers-s363.md`.
- **Step 5's `die` stands — die-side of the s329 boundary**: a wrong-sized
  operand is a VALUE the program consumes; announce-and-continue would commit
  the silent wrong it announces.  → fable-answers-s363.md §1.
- **"Detector and rewriter share one resolver" is a STANDING rule** (third
  confirmation: M2 s353, A-v, #264), and the gate-SET file-by-file diff over
  both populations is the standing verification for detection-widening
  changes.  → fable-answers-s363.md §3.
- **The THREE-ROUTE eval-visibility rule is normative** (ir-spec.md §2b.4):
  let-bound rename → site alist + suffix strip; defvar'd package cell →
  alias rule + span pairs; neither → hard refusal.  A new rename family picks
  its route before it may pass `eval_ok`.  ir-spec's stale cond/state and
  "falls back to v1" text fixed s364.  → fable-answers-s363.md §4.
- **A-ii PARKED behind E5**: stop-rule fired + all three files have snapshot
  C_ok = 0 (svleak 0/0; shm XS-blocked on #117; taint never transpiled, no
  taint model exists) — zero v1-era recovery by the ratified bar.  Design the
  declaration-shape enumeration into E5's promotion layer.
  → fable-answers-s363.md §6.
- **The s317 bareword probe claim was STALE**: the declared-before spelling
  CALLS correctly today; only the undeclared spelling diverges (perl
  stringifies, PCL prints empty) → **#266**.  → fable-answers-s363.md §2.
- **Before more #254 fixes**: register pat_advanced.t's `--timeout 900` and
  re-snapshot the three B-i files, else the +1765-row recovery evaporates
  (the #176 lesson).  → fable-answers-s363.md §5.

## s363 (2026-08-08, Opus) — #153 steps 4–5, plus #254's first three fixes

- **`${x}` at CODE level IS a use of `$x`** (`36b4d7f`, #264): PPI spells it
  Cast + Block-holding-one-Word, so there is no Symbol token and every
  Symbol-driven pass was blind — the span checker never fired (silent wrong,
  not even a gate) and the refusal written for that shape sat downstream of the
  span test, unreachable.  ONE helper, `_brace_name_refs`, now serves BOTH the
  detector and the renamer; `_has_code_brace_deref` is a filter over it.
  **A pass that DETECTS and a pass that REWRITES must share the resolver** —
  the same rule that made A-v (op/exec.t) a bug.
- **Adding DETECTION can turn silently-wrong files into DYING ones, so diff the
  GATE SET, not just the gate**: transpile every file of both populations
  before and after and compare per file (scratchpad `xstat.pl`, s363).  #264
  measured 30 → 30 (zero new gates); B-i measured 30 → 27 (exactly its three
  files).  Adopt this whenever a fix widens what a checker sees.
- **B-i needed no new mechanism** (`04316ab`): a poisoned condition-`my` rename
  mints a LET-BOUND `$x__cond__N`, so teaching `_eval_lexical_alist`'s key
  function that fourth suffix (beside `__lex__`/`__shadow__`/`__file__`) and
  passing `eval_ok` at the cond site is the whole fix.  `state` keeps the
  refusal — `__state__` is a defvar'd cell, never a let, so it never enters
  `_let_bound_vars`.  1257 rows.
- **De-gated ≠ done**: the ratified bar is the file's `perl-suite-run.tsv`
  snapshot C_ok.  Of B-i's three, only re/regexp_unicode_prop.t lands on it;
  op/my.t is one row short (**#265**) and re/pat_advanced.t is 137 short with a
  regex-engine residue.  Report both numbers.

## s363 (2026-08-08, Opus) — #153 steps 4–5: walker widened, unreachable operand branches deleted

- **The term walker claims method-call ARGS and cast-deref SLICE GROUPS**
  (`f322b19`, #153 step 4): `-> method ( args )` consumes the List and
  continues the chain; a Block/Constructor group after a cast-deref
  (`@{$r}[0]`, `\$V{V}`) is one slice postfix and ENDS the term (a slice
  yields a list; nothing postfixes a list).  PPI spells that group as a
  Constructor only because a `}` precedes it.
- **Bare WORDS and PREFIX operators stay DECLINED — by design, not by
  omission.**  Whether a bareword is a call, a filehandle, a class name or a
  constant is not the term grammar's question (it is decided in the main
  loop), and a prefix operator is phase-2 operator binding.  The plan text
  listing "bare words" among step 4's widenings is superseded by the walker's
  own rule.  → `_term_extent`'s header, session-log s363.
- **The operand CEILING cannot fall inside a postfix chain**, so a walker
  decline on a Symbol/Magic/Structure/already-parsed operand is IMPOSSIBLE —
  the ceiling only ever falls at a top-level low-precedence operator or a
  ternary `:`.  That argument (plus a 110-decline inventory over both
  populations in which every decline led with a Word, an Operator or a Cast)
  licensed deleting those branches at BOTH operand sites (`57086d8`, step 5);
  what is left handles only the two by-design declines, and anything else
  DIES naming the shape (rule 12) rather than leaving `$end_pars` at the
  ceiling — a wrong-sized operand is a value-producing silent wrong.
- **`tools/corpus-diff.pl`'s corpus IS `perl-tests/*.t` — the sweep's own
  input set.**  So "emission identical across 111 files" already proves the
  sweep's `.t` transpiles are identical; the only thing a post-parser-change
  sweep adds is MODULE transpiles, which corpus-diff does not diff.  (Reason
  to still run it cold, not a reason to skip it.)
- **A foreach loop-var write must reach EVERY bare scalar operand, in BOTH
  spellings** (`70e6e5c`, #262): one helper `_ev_foreach_alias_list` owns the
  `foreach-alias-list` veto; the block form calls it from `_tw_stmt`, the
  statement-modifier form from `_tw_stmt_expr`, and it vetoes every top-level
  comma slot that is a lone bare `$name`.  `$_ = "w" for ($s)` and
  `for ($p,$q){$_="w"}` were both silent-wrong.  Elements/derefs need no veto
  (they arrive as live boxes) — EXCEPT in the modifier spelling, where the v1
  seam lowers to `p-gethash` instead of `p-gethash-box` (#263, open).

## s361 (2026-08-08, Opus) — #153 step 3: both operand sites on the walker; the measurement population rule

- **MEASURE ON PERL'S OWN t/ TOO, not just the 111-file corpus.** s359's
  `PCL_TERM_DIFF` inventory over the census corpus said ZERO disagreements;
  the same probes over all 604 files of `t/*/*.t` produced THREE real shapes,
  two of them live silent-wrongs.  Any "measured then flipped" step in the
  #153 family runs over BOTH populations before the flip (helper:
  `tools/term-diff-sweep.pl`).  → session-log s361, task #153.
- **#153 step 3 DONE**: named-unary site (`3509115`) and strictly-1-arg site
  (`ece9d35`) both take their operand extent from `_term_extent`; a decline
  still falls to the legacy branches.  Both `PCL_TERM_DIFF` probes DELETED —
  once a site's answer IS the walker's, the probe can only report equality.
  Steps 4–5 (widen the walker to the declined shapes, fold reduction in,
  delete the dead `$end_pars` chains + `$deref_skip`) remain.
- **Paren-less call arity is decided by the PROTOTYPE, not by `min_params`**
  (`1279be6`): `min_params` is a MINIMUM, and reading it as "exactly one"
  made `sub f ($;$) {…}; f $a, $b;` silently drop every argument after the
  first.  The site now uses max-args AND perl's **trailing-`;` = LIST-operator
  precedence** rule (`sub unilist ($;)` ⇒ `unilist 0 || 5` is
  `unilist(0||5)`, while a plain `($)` is a named unary ⇒ `unilist(0) || 5`).
  Builtins are untouched (no `min_params` in their records).
- **A foreach LIST that is a single SCALAR is ONE element — decided in the
  EMITTER** (`d2bb91c`): the runtime cannot tell a box-wrapping-a-vector
  (`\@a`, `[1,2]`) from an `@array` box, and the ref-kind slot was rejected
  by measurement (s335), so `%p-flatten-for-list` was spreading the referent.
  The sigil is compile-time knowledge: `Pl::Parser::_foreach_single_scalar_p`
  is the ONE predicate, consumed by BOTH lowering sites (Parser2 block form
  and the v1 statement-seam modifier form), and the single scalar is routed
  through the SAME `(vector …)` shape a multi-element list already used.
  A scalar used as a foreach LIST is also force-boxed (VarAnnotator event
  `foreach-alias-list`) so `for ($x) { $_ = … }` writes back.
- **A sweep row that starts failing after a PARSE fix may have been passing
  under the wrong parse** — check the emission diff before calling it a
  regression.  pos.t t21 is the worked example: it scored "pass" while the
  statement parsed as `(ok($_[3])) =~ /\Ge/`; with the correct parse it fails
  for a real reason (#261, the `=~` target is a copy, not the arg box).
  Row added to `fail-baseline.tsv` by EDIT with that cause; hash.t's
  concurrent churn was measured PRE-EXISTING at the session-start commit
  (an empty-description baseline row — descriptions are join keys).
- **A bare NAME is a CALL only if it is callable WHERE the call site sits**
  (s374, task #266): perl decides top-down at compile time, so the answer is
  both QUALIFICATION-aware and POSITION-aware, and `Pl::PExpr::_bareword_callable_here`
  is the ONE place that knows it — consulted by both handle_subcalls branches
  that face a no-argument bareword (the binary-operator branch and the
  end-of-expression branch), which each used to carry their own copy of a name
  test that could answer neither half.  It is THREE-valued, and the two
  negatives are not interchangeable: `not-yet` (this file declares the name
  BELOW — positive knowledge that perl does not know it here either) reads as
  the string wherever it sits, while `no` (nothing this compiler can see) keeps
  answering CALL unless the word sits in operator context, because PCL's
  compile-time name knowledge is INCOMPLETE — measured: treating `no` as a
  string turned `next`, `goto again` and File::Spec's `curdir` into strings.
  Declaration SITES ride on `declared_subs` (`Pl::PExpr::TokenUtils::decl_site`
  / `site_precedes`); positions from two documents are incomparable and answer
  "callable", the old whole-file answer.  Control-flow words
  (`last`/`next`/`redo`/`goto`/`return`) are callable everywhere via
  `Config::control_flow_ops` — they are absent from `known_no_of_params`
  because their operand is a LABEL.  Guard `Pl/t/bareword-call-01.t`.
- **`explain()` DUMPS a ref** (s374, #236): Test::More renders every ref with
  Data::Dumper under Indent(1)/Terse(1)/Sortkeys(1) and passes a non-ref
  through; PCL stringified, so an is_deeply failure that printed its operands
  read `got 'ARRAY(0x53)'`.  The renderer in `cl/pcl-test.lisp` reuses
  `test-deeply-equal`'s shape test, so anything is_deeply can WALK, explain can
  PRINT, and it prints Dumper's `$VAR1` back-reference for a cycle or a shared
  ref.  Two deliberate differences, both commented at the code: no trailing
  newline (pl-diag would emit a bare `# ` line) and integer-vs-quoted decided
  by the CL type.
- **`-BAREWORD` autoquotes before `=>` and inside a HASH subscript** (s374,
  #234): perl reads `(-f => 4)` as the key "-f" and `$h{-f}` as that key, and
  the STRING reading beats the operator one.  A SINGLE-letter `-f` arrives from
  PPI as the FILETEST operator token, so both autoquote sites missed it (`-foo`
  and `-1` tokenize differently and were always right), the `$_` default took
  over, and the filetest's result ATE the next list element.  Settled at the
  three sites that own an autoquote decision — the fat-comma rewrite in
  `cleanup_for_parsing`, `_subscript_autoquote_text` (NEW: the ONE answer both
  subscript paths now ask, they had drifted into two copies), and the
  interpolation key regex.  NOT in the `$_`-default pre-pass: by then the
  element is already split and there is no `=>` left to key on.
- **`use` ARGUMENTS are compiled, never wrapped as raw text** (s374, #235):
  `use lib "$ENV{HOME}/x"` and `use constant X => "…"` are compile-time perl
  expressions.  Two hand-rolled sites wrapped `$tok->string` in CL quotes,
  which dropped interpolation AND mangled escaping four ways — `"a\nb"` → CL
  "anb", `'a\b'` → "ab", `'a"b'` → unreadable CL that killed the file at load.
  Both now use the ordinary expression path; the single-quote-token
  short-circuit in `_compile_constant_value` is DELETED (its own comment
  already said why NUMBERS must not be short-circuited — strings were the same
  trap one type over).  A general `use Module LIST` already compiled its import
  args, so the scope really is those two sites.

## s375 (2026-08-09, Fable) — s374 review: all four commits APPROVED; six asks ruled

- **s374 APPROVED as shipped** (`docs/fable-answers-s374.md`): gate
  independently re-verified **133/4773**; the #236 renderer probed
  byte-identical to perl (incl. integer-vs-quoted on stringified numbers);
  the #234 four-shape probe identical; both new filings (#275, #276)
  reproduced.
- **#266's three-valued asymmetry IS the ruling**: `not-yet` (declared BELOW
  in this file) is positive knowledge → the string anywhere; `no` (nothing
  visible) keeps answering CALL, because PCL's callable set is INCOMPLETE and
  a wrong CALL fails loud where a wrong STRING is silent.  Do NOT complete
  the callable set as a campaign — missing entries are fixed as ordinary
  bugs when a cause line names them (#149 precedent).  **Correction: the
  ask's mutual-recursion residue (`sub a { b } sub b { 1 }`) does NOT
  reproduce** — the site rule covers it, probed `a=b` both sides
  (`fable-answers-s374.md` §1).
- **The package-blind unqualified bareword path is ACCEPTED, unscheduled**
  (§2): probed PRE-EXISTING at `f44e947` (identical `B::pl-f undefined`
  crash), loud not silent, zero cause lines on any population.  Re-raise on
  a real cause line, like #191.
- **The #234 comment-linked interpolation twin STANDS** (§3): token sites
  share `_subscript_autoquote_text`; the text site cannot consume tokens,
  and lifting to a text predicate would re-encode tokenization as string
  matching (forbidden).  Two sites, different INPUTS → named-twin comments,
  not shared code.
- **#236's two Dumper deviations CONFIRMED** (§4): no trailing newline
  (pl-diag splits on newline); integer-bare-vs-quoted decided by CL type —
  the ir-spec data model's honest carrier of the SV IOK/POK distinction.
- **Cadence RULED** (§6): a parse/emission change may skip the same-session
  full sweep when corpus-diff is IDENTICAL over all 111 **and** the lib/
  shim transpiles in the change's argued reach are byte-compared **and** the
  Pl/t gate is green; the every-3rd–5th-change sweep cadence still applies
  and its TOTAL/LOST verdict is still the gate.  Fold a `--lib` mode into
  corpus-diff.pl the first time a change's lib/ reach is unclear, not
  before.
- **Queue**: Opus next = #275 (TAP plan line, FIRST — one mechanism with
  `plan()`) → #276 (empty-brace list-op argument; probe `map {}`/`grep {}`
  before widening) → #238 → #239; then #237.  The FOLD (#153) = Fable,
  begun s375.

## s375b (2026-08-09, Fable) — #153 FOLD chunk 1: phase-1 term reduction in the main loop

- **`_fold_terms` is the FOLD's first chunk**: parse() reduces every embedded
  postfix-bearing term to ONE node via `_reduce_term` before the arrow/
  subscript machinery runs.  NOT folded, each guard unit-tested in
  reduce-term-01.t: Word-led terms, Block-led terms (chunk 2, `$deref_skip`),
  terms followed by a raw Block/Constructor, the whole array (recursion
  guard), positions preceded by a Cast or `->` (mid-term), and Constructor
  starts not preceded by an operator — **PPI classifies `[...]` by its
  PREDECESSOR**, so after `)` (list slice) it is a SUBSCRIPT, and folding it
  as an anon-array orphans the term it subscripts (found live in ref.t/
  grep.t; same family as `_retag_braced_deref_subscript`).
- **The fold is emission-preserving BY MEASUREMENT over all four audit
  populations**: corpus-diff IDENTICAL 111/111 vs HEAD; A/B (same compiler,
  `PCL_NO_FOLD=1` toggle — no worktree, no normalization needed) SAME
  604/604 suite + 183/183 board + 21/21 lib shims, zero exit diffs; gate
  133/4784 PASS; perf neutral.  Hence NO sweep and NO cache-generation bump
  (s375 cadence ruling, all three legs).  `PCL_NO_FOLD` is the standing A/B
  instrument for chunks 2–3, deleted with the legacy branches.
- **Chunk-2 boundary probed vs perl**: `grep {a=>1}->{a}, LIST` = expr-form
  (deparse), `eval {…}->[0]` derefs eval's RESULT — both already match PCL,
  so Block-led folding must key on hash-constructor SHAPE, never on a block
  after a block-taking word.  #271/#211 unchanged from HEAD, still the
  FOLD's later acceptance targets.

## s375c (2026-08-09) — USER: the v0.1 public-release track is opened

- **Release plan = `docs/release-plan-v0.1.md`** (tasks #277–#283 + #217/#128
  promoted).  Two USER rulings: (1) **the runtime + saved core are COMPILED
  AT INSTALL** on the target — the XS bridge's install-time model, never
  first-use (#277); (2) **a pre-release IR pass**: re-measure the generated
  code, introduce the macro vocabulary where it costs no speed, and update
  `docs/ir-spec.md` normatively (#281 — the scoped pull-forward of Target B;
  #75 stays the full flag-day; emission-changing, full verification).
- Track order: FOLD chunks 2–3 → #217→#277/#278/#128 → #281 → #279/#280 →
  big bug hunt (the s360 E5 exit gate — this release is what it was deferred
  FOR) → #282 fresh-machine gate → #283 CI → tag v0.1.
- Open USER decisions listed in the plan doc: public name (PCL collides in
  the CL world; percolisp org exists), publish process docs or not, LICENSE
  body check, pclxs bundling, hosting/remote.

## s376 (2026-08-09, Opus) — #275 + #276 + the #238 shim batch

- **`use Test::More tests => N` is a PLAN, and it now reaches the TAP layer
  (#275).**  Test::Builder::Module::import strips an `import => [...]`
  export list and hands EVERYTHING ELSE to `plan()`, so `tests => N`,
  `'no_plan'` and `skip_all => REASON` all mean the matching `plan()` call.
  `p-use` returned as soon as it recognised a PCL-provided module, dropping
  the whole list — so such a file published TAP with **no plan line and no
  `done_testing`**, one no harness can judge (the #202 family).  Fix:
  `p-use` forwards the import list to `%test-import` in `cl/pcl-test.lisp`
  (that is where Test::More's semantics belong); it consumes `import`,
  hands the rest to the existing `pl-plan`, and lets `pl-plan` reject an
  unrecognised form as it already did.  A bare VERSION (`use Test::More
  0.88`) is not an import arg and stays plan-less, as in perl.  Guards:
  `Pl/t/tap-assert-01.t` (every expected block now opens with its `1..N`,
  plus one row for `no_plan` / `skip_all` / VERSION) and
  `Pl/t/goto-sub-phase-01.t`.  58 board files gain a plan line; no board or
  sweep row moves (the counters read `ok`/`not ok`, never the plan).
- **An empty `{}` in TERM position is an anonymous HASH, not a block
  (#276).**  perl's toke.c decides on the character after the brace, with
  no ambiguity to resolve.  PPI already labels `{}` a Constructor
  everywhere it can; the one place it still says Block is after a bareword
  in paren-less list-operator position — `f {}`, `explain {}` — where PCL
  fell through to the block-body parse and produced an empty ARRAY (`ref`
  said `""` and the call lost an argument).  Fixed in `Pl/PExpr.pm`'s
  single term-position Block arm via `_block_is_empty`, deliberately NOT
  folded into `_block_is_hash_constructor`: that predicate answers the
  map/grep/`(&@)` BODY question, where a bare `{}` is not valid perl at all
  (`map {} (1,2)` is a syntax error), so widening it would be a claim about
  input principle 9 says we need not read.  Emission moves in ONE corpus
  file (method.t's three `new{}` rows, already-failing "pl-new is
  undefined" either way — verified 93+38/163, identical to baseline).
  Gen bumped to **v2-126**, both transpiled artifacts restamped
  (byte-identical below the header).
- **#238 List::Util/Scalar::Util shim parity, first pass: 319 ok/120 not-ok
  → 398 ok/75 not-ok on the dist's own 38 files** (12→17 PASS, 5→2 FAIL).
  Implemented: `maxstr`/`minstr`/`reductions`/`sample`/`zip_shortest`/
  `mesh_longest`/`mesh_shortest` (were dying stubs); `zip` returns
  ARRAYREFS and `mesh` flattens (they were both the flat list);
  `product()` of nothing is 1; `head`/`tail` take `($@)`, clamp both ends
  and die "Not enough arguments"; the four `uniq*` separated by what they
  compare and what they RETURN (`uniqnum`'s key is `pack "d"` — the raw
  double, because stringifying collapses 1.4142135623730951 and
  ...54 into one key); `$List::Util::RAND` honoured by `shuffle`/`sample`;
  `pairgrep`/`pairfirst` scalar-context answers; one shared `_need_code`
  gives all ten block-taking functions perl's two error texts
  (t/undefined-block.t 0/18 → 18/18); `looks_like_number` accepts
  Inf/Infinity/NaN and `0 but true`, uses `[0-9]` not `\d` (MONGOLIAN
  DIGIT FIVE), and answers on an overloaded object's stringification;
  `isdual`/`isvstring` route through new `builtin::is_dual` /
  `builtin::is_vstring` (box-representation facts no plain Perl can ask).
  **Remaining 75 rows are all parked families**, listed on task #238:
  tie (#155), REF-vs-SCALAR (#163), prototype introspection, `weaken`,
  read-only SCALAR storage, taint, get-magic, `subname`, `@_` aliasing,
  error text (#149).

## s376b (2026-08-09, Opus) — #239 DIAGNOSED: `package X;` in a BLOCK does not re-home unqualified globals

- **An in-block `package X;` (inside `eval { … }` / `do { … }`) leaves every
  unqualified global resolving in the ENCLOSING package.**  Not
  $a/$b-specific, not eval-specific — reproduced with a plain `$z` in both
  `eval {}` and `do {}`.  perl puts the write in `Foo::z`; PCL puts it in
  `main::z`.
- **Mechanism**: the block lowers to ONE top-level CL form containing both
  `(in-package :Foo)` and the statements.  CL's reader interns every symbol
  in a top-level form BEFORE evaluation, so an `in-package` nested inside a
  form cannot change how the symbols around it were read.  A FILE-level
  `package X;` works precisely because D1-lite splits it into separate
  top-level forms.  The emitter already spells that region's PRE-DECLARED
  globals qualified (`(defvar Foo::$a …)`); only the use sites stay bare.
- **Fix shape**: #226's QUALIFIED-emission rule, applied to code-level
  blocks — a sibling trigger for
  `_requalify_block_our_after_pkg_switch` (Pl/Parser2.pm:157), which already
  performs exactly this rewrite for `our`-DECLARED names in the same region.
  The new part is deciding which bare names are globals; route it through
  the SAME resolver the detector/promoter use, never a sigil- or scope-blind
  scan.
- **Population**: 31 occurrences in 14 files across the audit populations,
  including NINE Class-Method-Modifiers t-files and Role-Tiny's
  method-conflicts.t (both dists carry open board failures, #135) — so this
  is a family, not the Sort::Versions one-off it was filed as.
- Two s344 hypotheses stay RULED OUT and must not be re-tried: `(caller)[0]`
  reporting the wrong package (it reports `Foo` correctly — instrumented in
  the real dist), and the symbolic read `${"Foo::a"}` (the READ is right;
  the WRITE went to the wrong package).  Full detail on task #239.

## s376c (2026-08-09) — USER: the three s376 questions

- **#239 goes NEXT, before #237.**  The in-block `package X;` bug is a
  measured family (31 occurrences / 14 files, nine of them
  Class-Method-Modifiers, plus Role-Tiny method-conflicts.t), its cause is
  pinned, and the fix reuses a rewriter that already exists.
- **#238's `subname` family is SIZED FIRST, not implemented** (task #284).
  exotic_names.t (1554 rows) + subname.t (21) both hang on PCL not being able
  to name a compiled CL closure.  Measure two things before any code: whether
  a function-object → name registry answers `subname()` at no hot-path cost,
  and how many of the 1554 rows need only a NAME versus the exotic
  NUL/newline/latin-1 stash names.  Bring the answer back before writing.
- **The CPAN board gets a fresh baseline, but only after a PER-FILE audit**
  (#208) — the s376 whole-board run is saved as `baselines/cpan-board14-s376.tsv`
  (2053 ok / 483 not-ok at beb4187).  Same discipline as #223's sweep
  pass-baseline: a blanket refresh absorbs a loss silently.  Read ROWS, not
  the PASS/PARTIAL label; the one known LOSS (Role-Tiny extend-role-tiny.t,
  `Package Role::Tiny does not exist` at load) is diagnosed on #208 and must
  be explained or hand-edited in with its cause.  Best taken AFTER #239
  lands, since #239 moves board files.

## s377 (2026-08-09, Fable) — review of the s376 batch: all four commits APPROVED

Full rulings in `docs/fable-answers-s376.md`.  Gate independently re-verified
133/4785 PASS; #276/#275/#239 probed live vs perl.

- **Expectation-rewrite rule (Ask 1)**: a gate expectation may be rewritten
  only when (a) the new text is real perl's output probed live, (b) the diff
  is exactly the corrected divergence, (c) the edit STRENGTHENS the assertion
  (never deletes/loosens — that stays "never simplify"), and (d) the same
  commit carries an explicit guard row for the fixed behaviour.
- **`builtin::` is PCL's shim-dispatch seam for box-representation
  primitives (Ask 5)**: perl-shared names must match perl exactly; PCL-only
  names allowed only for box facts plain Perl cannot express, consumed by
  `lib/` shims; perl claiming a name later wins.  `readonly` → same seam
  when the storage fact exists; `prototype` is a CORE builtin → runtime,
  not the seam.
- **A change under `lib/` (or `cl/`) is invisible to corpus-diff (Ask 8)**:
  the s374/s375 second-sweep optionality NEVER applies to it — the shipping
  session runs the full sweep + the owning dist's files.
- **#239 fix shape ruled (Ask 7)**: sibling trigger on `_rewrite_var_uses`;
  four-way resolver (lexical incl. in-scope `our` → declaring package /
  magic-special / qualified / else → X) from the SAME scope walk the rename
  machinery uses; unclassifiable name DIES (s372 gate-SET bar applies);
  variables only — probe the in-region CALL shape during verification.
  NEW probe finding: the bare-block `our`-alias case diverges TODAY in the
  opposite direction (`our $x; { package Bar; $x = 1 }` writes Bar::x, perl
  writes main::x) — mandatory guard probe on #239.
- **uniqnum key (Ask 6) = HYBRID, measured**: integer-valued args within
  |v| < 2^64 key as exact integer text, else `pack "d"` (integral NVs beyond
  2^64 MUST stay pack "d" — 15-digit stringification collapses distinct
  doubles); ship undef→0 output coercion with it; buys 4 of the 6 rows.
- **Two predicates stand (Ask 3)**, s370-comma-walk style: paired comments,
  no refactor; a THIRD consumer of the strip/unwrap shape forces extraction.
- **Filed**: #285 (perl's no-plan ending diagnostic: stderr + exit 254 —
  the last #202 spelling; detection-widening bar applies); #286 (ambiguous
  `f {$k,$v}` / `f {%h}` intuit_curly shapes — deferred, #191 pattern,
  re-raise on a real cause line).

## s378 (2026-08-09, Opus) — #239 DONE: an in-block `package X;` re-homes globals

- **A `package X;` inside a BLOCK re-homes every unqualified package VARIABLE
  for the rest of that block — and v2 could not express that with the nested
  `(in-package :X)` it was emitting.**  CL's reader interns every symbol of a
  top-level form before evaluation, so a package switch nested inside one form
  changes nothing about the symbols around it.  Measured s378 across NINE block
  kinds: `eval`/`do`/named-sub/`BEGIN`/`if`/`while`/labelled/anon-sub/`sort`
  all wrote the ENCLOSING package's variable; only the plain bare block (the
  one shape D1-lite splits into separate top-level forms) was right.  Fix =
  `_requalify_block_globals_after_pkg_switch` in `Pl/Parser2.pm`, a sibling
  trigger beside the `our` one, rewriting the region's bare names to `X::name`
  through the same `_rewrite_var_uses` family rewriter → task #239,
  guard rows `Pl/t/transpile-test-09.t`.
- **The four-way resolver, and where "left alone" is not enough**: an `our`
  alias in scope is requalified to ITS DECLARING package, not left bare —
  leaving it bare let the section package answer, which is the s377 §9.1
  divergence (`our $x; { package Bar; $x = "X" }` wrote Bar::x where perl
  writes main::x).  That bare-block sibling is FIXED by the same pass, not
  filed as a companion.
- **`$a`/`$b` are immune to the switch, deliberately**: the sort lowering
  emits `(lambda ($a $b) …)`, i.e. it LEXICALLY binds those two symbols in the
  section's package, so requalifying them would leave a `sort { $a <=> $b }`
  inside a switched region reading `X::$a` while the lambda bound the
  section's — a working sort turned silently wrong.  Listed with the
  always-main specials in `%PKG_SWITCH_IMMUNE_VARS`; `@a`/`@b` are ordinary
  globals and are NOT immune.
- **Variables only — sub definitions, `*glob` installs and bareword CALLS
  inside such a region were already correct** (probed s378): they resolve
  through the package stack at lowering time, never through the CL reader.
- **"$Foo::bar" is ONE qualified name to every interpolation scanner**:
  `_interp_canon`'s captures now run to the end of a qualified name and
  `_interp_fixer`'s unbraced arms carry `(?!::)`.  Without that, renaming a
  variable called `Foo` reached into an already-qualified `@Foo::bar` and
  produced `@main::Foo::bar` (caught live by the M7 gate row).  The BRACED
  form `"${x}::y"` genuinely IS `$x` then text and keeps no guard.
- **`local` is not a binder for this pass**: `_symbol_is_declarator` grew an
  optional keyword-regex argument so the requalifier can pass the
  my/state/our subset — `local $v` in the region localizes the REQUALIFIED
  global and its symbol must be rewritten with the rest.
- **NEW ASK, measured (task #287)**: the `$a`/`$b` immunity is exactly what
  stops #239 clearing its board population.  Dropping it takes Sort-Versions
  `versions.t` from 65/31 to **96/0**, and simultaneously makes every
  `sort { $a <=> $b }` inside an eval/do/named-sub package-switched region
  return the list UNSORTED.  Both halves measured live s378.  The real fix is
  two-part — drop the immunity AND make the sort lowering bind the region
  package's pair — and it needs a design call, so it did NOT ride #239.

## s378b (2026-08-09, Opus) — #237 RE-SCOPED: regex interpolation drops direct subscripts

- **`$a[i]` / `$h{k}` interpolated inside a REGEX are silently dropped** — the
  subscript is left as literal pattern text, so `/^$a[1]$/` compiles as `$a`
  followed by the character class `[1]`.  Every regex consumer is affected
  (`m//`, `s///`, `split`, `qr//`, the braced `${a[1]}` form); DOUBLE-QUOTED
  strings are all CORRECT.  That asymmetry is the tell: `ExprToCL.pm`'s
  `_gen_interp_regex_pattern` is a SECOND hand-rolled interpolation scanner
  beside `PExpr/StringInterpolation.pm`, and it only ever learned `${name}`,
  `$name` and the `$name->[i]` / `$name->{k}` ARROW chains → task #237.
- **#237's two original premises are DEAD, do not retry them**: `pos()`/`\G`
  through a scalar ref is faithful (six probes, all identical to perl), and
  the Text::Balanced symptom is not an offset error but a total extraction
  failure — `qr/\G$_[1]/` compiles to `(?^:\G[1])`.
- **The fix needs perl's `intuit_more` heuristic, not a guess** (same bar
  #286 was deferred under, and this is the cause line that re-opens that
  family).  PCL's current "always regex syntax" rule is RIGHT for the shapes
  perl reads that way and they pass today — `/$x[abc]/`, `/$x[^a]/`,
  `/$x{2,3}/` — so a naive "always a subscript" fix would break exactly those.
  `/$x[0]/` and `/$x{k}/` are the failing side; `$x[0]` / `$x{2}` are
  genuinely ambiguous shapes where perl picks the subscript.

## s379 (2026-08-09, Fable) — s378 review: both commits APPROVED; six asks ruled; two resolver fixes shipped (`docs/fable-answers-s378.md`)

- **Absorb-vs-file-a-companion, refined**: a sibling bug may be ABSORBED into
  a ruled fix's scope only when (i) zero new mechanism — the ruled mechanism
  inherently answers it, (ii) loudly recorded, (iii) guarded in the same
  commit.  Absent any one, file the companion → `fable-answers-s378.md` §1.
- **#287 fix shape RULED**: two halves in ONE commit — drop `$a`/`$b` from
  `%PKG_SWITCH_IMMUNE_VARS` AND make the sort lowering bind the pair the
  comparator block actually reads (reuse `_pkg_in_effect_at`, rule 11); the
  perl gotcha list to probe (`sort Other::cmp` = caller's pair, etc.) is in
  §2.  Slot: after #237.
- **#237 fix shape RULED = (b′)**: ONE shared variable-reference EVENT
  scanner extracted from StringInterpolation.pm; dq-strings take every
  chain, the REGEX consumer applies the intuit_more bracket classifier per
  event; `_gen_interp_regex_pattern`'s private walk is DELETED as the
  acceptance criterion.  Do NOT route regexes through the dq interpolator
  wholesale (escape semantics differ).  **#286 is NOT folded in** — intuit_curly
  is a different heuristic at a different site; it stays deferred on its own
  cause line → §3.
- **Guard-row placement**: wall-time headroom beats thematic grouping; a
  family's rows may scatter — the task/DECIDED entry is the anchor → §6.
- **Signature params ARE binders to the package-switch resolver** (s379 fix
  `2af263f`): the head scan reads Prototype/Signature through the seam's own
  `_signature_param_specs`; a Symbol in a DEFAULT expr is a use; a pure
  prototype binds nothing.  And `my`/`our`/`state` in NON-declarator
  positions (fat-comma key, hash-subscript key, method name, `my sub`)
  contribute no binding and never die → §7.
- **#288 filed (pre-existing)**: a bareword CALL in a package-switched
  region resolves to the ENCLOSING package's sub where perl dies
  "Undefined subroutine &P::hello" — wrong-callee silent-wrong, task carries
  the layer-discriminating measurement.
- **Raw `.fails.tsv` row counts are NOT comparable to baseline row counts**
  (signatures.t: 90 raw vs 34 keys, 0 new) — compare through `sweep-diff`
  or by its join key → §7.
- **Variable-handling design review** (USER ask) →
  `docs/var-handling-review-s379.md`: 51 % of Parser2.pm is variable
  identity; 26 interpolation-scanner sites; directions A (bind-once symbol
  table, W12-style dual-run), B (one interp scanner = #237's mechanism),
  C (one promotion engine), D (defglobal + save/restore `local` dissolves
  poisoned-my — MEASURE FIRST, changes the IR contract), §7 (hoist only
  compile-time-referenced subs — measure first).  Standing: no new scanner
  fixes, no new suffix family, no new scope walk (§8).

## s379b (2026-08-09) — USER: the sign-off rule

- **A design change needs NO user sign-off when all four hold: (1) simpler,
  (2) clearer, (3) generated code FASTER (or unchanged — a change that
  SLOWS generated code is still flagged), (4) total compile/transpile time
  < 50 % worse.**  Take the measurements first; if they hold, proceed.
  This waives the DESIGN ask only — correctness gates (Pl/t gate,
  corpus-diff, sweep TOTAL/LOST, probes vs perl) apply in full, semantic
  behavior changes still follow the probe-and-rule process, and queue ORDER
  remains the user's.  Immediate effect: `var-handling-review-s379.md`'s
  directions D (defglobal + save/restore `local`) and §7 (selective
  hoisting) are no longer "user decisions" — they are measurement-gated
  work items under this rule.

## s379c (2026-08-09) — USER: three planning answers

- **#237 is SPLIT**: Fable designs/builds the shared variable-reference
  event scanner + the intuit_more bracket classifier + its probe table;
  Opus wires in the consumers (regex/dq/rename) and the guard rows after.
- **IR STABILITY GATES v0.1**: the intermediate code (emitted CL) must be
  STABLE at the first release — any change to the emitted shape happens
  BEFORE v0.1, not after.  Derived ordering: after #237 → #287, run the
  IR-affecting batch first — the §6 defglobal measurement (+ implementation
  if the s379b conjuncts hold), the §7 selective-hoisting measurement
  (same), and the #281/#75-scoped macro-vocabulary pass — THEN the
  mechanical v0.1 track (#277–#280, #282, #283) and the tag.  IR-NEUTRAL
  work (direction A binder + ports, #153 FOLD, direction C's internal
  fold) may land any time; direction A step 3 (emission-time naming) is
  v0.2 material unless it makes the pre-v0.1 batch.
- **The s379b < 50 % compile-time budget is PER CHANGE, drift watched**:
  each change is judged against the tree it lands on, but cumulative
  whole-corpus transpile time is tracked and the user is flagged if the
  total creeps past ~50 % over the s379 baseline (~65 s whole-corpus,
  measured s378).

## s380 (2026-08-09/10, Opus) — #287 DONE: `sort` binds the pair of the package it was COMPILED in

- **perl's rule, now PCL's: `sort` sets the `$a`/`$b` of the package the SORT
  was compiled in, never the comparator's.**  So a comparator compiled in
  `main` and called from inside a `package P;` region reads `$main::a` and
  sees nothing — `sort bylen`/`sort Other::cmp` from a region legitimately
  return the list UNCHANGED, in perl and in PCL.  Matching perl here REMOVED
  three PCL divergences; it added none.
- **`$a`/`$b` are no longer immune to the package switch** (the s378 entry
  above is SUPERSEDED on this point): they are ordinary globals, `#239`
  requalifies them like any other, and the sort lowering binds *the pair the
  comparator body actually reads* — `Pl::PExpr::_sort_pair`, feeding all four
  sort-lambda sites (paren block, block form, `sort NAME LIST`,
  `sort $scalar LIST`) and `ExprToCL`'s `$node->{params}`.
- **The discriminator is "was this node REQUALIFIED", not "what package is in
  effect"** — a FILE-level `package X;` is split by D1-lite into its own
  top-level form, so a bare `$a` there is already read as X's and rewriting
  it would only churn bytes.  `Pl::Parser2::_pkg_region_at` answers exactly
  that question (BLOCK-level `package X;` statement, word-shaped namespace —
  deliberately the same condition `_requalify_block_globals_after_pkg_switch`
  uses), which is why corpus-diff moved **1 of 111 files** (`sort.t`, only
  inside its `{ package Foo; … }` block).  It shares `_pkg_in_effect_at`'s
  ONE walk via the extracted `_pkg_stmt_in_effect_at` (rule 11 — no third
  package-in-effect resolver).
- **A qualified pair MUST be `(declare (special X::$a X::$b))` in the lambda.**
  The region's `defvar` is emitted INSIDE the enclosing top-level form, so it
  has not proclaimed the symbol special when the lambda is compiled — probed
  in bare SBCL, interpreted and compile-file both: without the declaration the
  parameter is a plain LEXICAL and `(symbol-value 'X::$a)` still reads the
  defvar's value.  A block comparator would survive that; a NAMED comparator
  and the `${(caller)[0]."::a"}` symbolic read would not.  The bare pair needs
  nothing (its defvars are top level), so every unswitched sort is emitted
  byte-for-byte unchanged.
- **A guard row must not make a claim the SORT ALGORITHM answers.**  Caught
  live in this session: a comparator with a constant NON-ZERO verdict prints
  perl's mergesort merge order (`2 3 1`) vs the runtime's `stable-sort`
  (`3 1 2`) — nothing semantic.  Zero-verdict rows are fine (both are stable,
  deliberately: `p-sort` uses `stable-sort` because perl's sort is stable);
  for a non-zero one, print what the comparator OBSERVED instead of the sorted
  list.  Guard rows: `Pl/t/transpile-test-09.t` ("package-switched sort binds
  the region's $a/$b pair (#287)").
- **Sort-Versions `versions.t`: 65 pass / 31 fail → 96 / 0** — the dist that
  filed the ask, now clean.

## s382 (2026-08-10, Fable) — #237 Fable half SHIPPED: Pl::InterpScan (event scanner + intuit_more port + probe table)

- **`Pl/InterpScan.pm` is THE variable-reference scanner for interpolating
  text** (ruled b′, `fable-answers-s378.md` §3; split USER s379c): `scan` /
  `scan_one` emit events (sigil/form/name/canon/span/name_span/chain/
  slice/postderef); `intuit_more` + `regcurly` are line-faithful ports of
  perl 5.40.3's toke.c/regcomp.c, read from the running perl's own source.
  NO consumer is wired yet — emission unchanged; Opus wires consumers +
  guard rows per `docs/interp-scan.md` (the contract + divergence table).
  Verified by `Pl/t/interp-scan-01.t`: a live-perl probe table (re-derived
  every run), classifier-vs-probe, and event-shape pins.  Per
  `var-handling-review-s379.md` §8, new interpolation behavior goes in this
  module or not at all.
- **CORRECTION to the s378b entry above**: probed, `/$x{2}/` is a
  QUANTIFIER (regcurly: `{2}` `{2,}` `{2,3}` `{,3}` `{ 2, 3 }` all are),
  NOT a subscript as the parenthetical guessed.  Subscript side: `{k}`
  `{'k'}` `{$i}` `{-3}` `{}` `{2x}`.
- **Only the FIRST bracket group is classified** — continuations always
  bind (`/$m[0][abc]/` dies on the bareword, `/$h2{k}{2,3}/` dies "Not a
  HASH reference"), and `->[`/`->{` bind unconditionally everywhere.
- **Braces CLOSE a reference in interpolation, both modes**: `"${x}[0]"`
  is $x then literal `[0]` (`"${m}[0]"` under strict dies on the SCALAR
  $m); `@{x}[0]`, `${ar}->@*` likewise.  Consequence: `_interp_fixer`'s
  `${x}[`-is-@-family arm has the sigil family WRONG (pre-existing; fix
  lands when the fixer becomes an event consumer — divergence table §2).
- **The weigher's symbol-table hook is real and ported** (`known_name`):
  `/$x[\n@foo]/` flips subscript↔charclass with @foo's existence; only
  1–2 digit guts are subscripts (`[100]`/`[123]` are charclasses).

## s382c (2026-08-11, Fable) — Direction-D audit DONE: GO, mechanism CORRECTED (docs/defglobal-audit-s382.md)

- **`sb-ext:defglobal` is DEAD for direction D** — probed: SBCL refuses
  `let` of a global name outright ("cannot be used in LET"); the
  var-handling-review §6 premise of free lexical shadowing was FALSE.
- **The working mechanism is the SYMBOL-MACRO GLOBAL**:
  `(define-symbol-macro $x (symbol-value '$x))` over an initialized,
  unproclaimed value cell.  Probed: `let` = plain lexical shadow (perl's
  `my`), closures capture it, save/restore-under-unwind-protect = `local`
  (die path included), read cost at parity with specials (111 vs 106 ms /
  100M).  Special and symbol-macro are mutually exclusive BOTH directions
  — a partition bug dies at load, never silently.
- **Corpus classification: NO third class** (675 defvar'd names, 113
  files): 416 my/param collisions (accidental dynamics today — D makes
  them correct lexical shadows), 348 runtime-magic rebinds (stay defvar),
  54+ `local` lowerings (become save/restore; `p-local-glob` already
  ships the idiom, runtime has NO progv), 32 declare-special = sort pairs
  only.  0 bare defvars, so the cell-init requirement has no gap.
- **Exception set (stays defvar), name-based and image-global**: `$a`/`$b`
  in every package + the runtime-magic list.
- **Payoff measured**: 809 poisoned-my renames in corpus emission
  (`__shadow__` 611 / `__cond__` 194 / `__emb__` 4) + 3 veto predicates +
  ~300 Parser2 lines delete; #205 closes by construction.
- **s379b conjuncts all hold → implementation proceeds without an ask**
  (pre-v0.1 IR batch; ir-spec.md + gen bump in the same commit).

## s382d (2026-08-11, Fable) — #289 mechanism FINALIZED after USER perf challenge (docs/direction-d-plan.md)

- **Access macro is `(sb-ext:symbol-global-value '$x)`, not `symbol-value`**
  — direct cell access, valid because partition symbols are never
  dynamically bound (holds by construction).  Measured: READS 20% FASTER
  than today's specials (63 vs 79 ms/100M, boxed shapes), my-shadow binds
  36% faster (29 vs 46 ms/10M), call-level parity, name-based
  symbol-value/boundp/makunbound interop probed intact.
- **FLAGGED regression (s379b conjunct-3 duty): `local` of an ORDINARY
  user global costs ~41 ns vs ~4.6 ns** (the unproclaimed-symbol setter
  is a full call; open-coding requires a proclamation that forbids `let`).
  Accepted because: rare (54+~20 corpus sites, none hot), absolute cost
  ≈ two hash lookups, and hot locals target MAGIC vars which stay defvar
  at today's speed.  bench-exec.pl before/after is a step-2 gate.
- **Rejected by probe**: plain symbol-value access (local 8.4×), progv
  local (4.8×), two-symbol defglobal cell (same speed as chosen, but a
  runtime-wide name→cell mapping audit; recorded as the upgrade path if
  a profile ever shows ordinary-global local hot).
- Plan of record for #289: `docs/direction-d-plan.md` (steps, gates,
  guard rows, artifact regeneration, rollback).

## s384 (2026-08-12, Opus) — #294: `foreach my $x` states its declaration in the IR (`:my t`)

- **Since the flip a lexical and an ordinary global are the SAME symbol, so
  "is this loop variable a package cell?" is not answerable from the emitted
  form alone.**  `%p-cell-loop-var-p` reads the macroexpansion environment,
  which is right for `foreach $x` (an enclosing `my $x` shadows the symbol
  macro; no global → plain `let`) and WRONG for `foreach MY $x` — the loop's
  own declaration is not in the environment yet, so a same-named package
  variable made the loop LOCALIZE the cell.  Measured: a sub called from the
  body saw the loop value (#294), and a closure made in the body saw the
  post-loop restore.
- **The compiler states it: `:my t`, after `:label`, before the body**, from
  BOTH foreach emitters (Parser2's compound branch and the v1 seam's
  `_process_foreach_statement`, still live for block-form args/anon subs —
  measured reachable in 20+ corpus files).  The runtime consults the key
  FIRST; `%p-cell-loop-var-p` is now only ever asked about an UNDECLARED loop
  variable.  Normative: `ir-spec.md` §6.2 "Loop variable: lexical or
  localized"; guard rows (incl. the must-not-break `foreach $global` case) in
  `Pl/t/transpile-test-10.t`.
- **This is a PRECONDITION for #291**: the poisoned-`my` deletion makes more
  names carry cells, which is exactly what turns this latent guess into
  live miscompiles.

## s384b (2026-08-12, Opus) — #291 MEASURED NOT SHIPPABLE AS PLANNED; four blockers filed

- **The poisoned-`my` renames have TWO causes, and `direction-d-plan.md` §4
  step 3 names only one.**  Besides "a `defvar` poisons the section's own
  `let`", `_seg_lex` makes `_forward_global_decls` SKIP any name the section
  let-binds, so without a rename the GLOBAL loses its declaration and is
  unbound at load (probed: deleting the block-my pass alone drops
  `(p-defcell @a …)`).  The enabler is to drop that exclusion in FILE mode
  only — **EVAL mode keeps it**, because there the same list is the
  p-eval-thunk's capture PARAMETERS from the CALLER, a question the flip did
  not touch.
- **The renames were IMPLEMENTING three things, not just dodging one.**  Each
  is now a blocker task with a live reproducer: **#296** `my $a`/`my $b` is a
  DYNAMIC bind because the exception partition keeps them `defvar` (closures
  lose the value; PRE-EXISTING, probed on plain main); **#297** a `my` in a
  C-for CONDITION has no `let` at all and writes the package cell; **#298**
  `my $x = <self-ref with a list-operator comma>` becomes a hard refusal,
  because `_rename_decl_within` skips the decl's own RHS and so was producing
  perl's "RHS reads the OUTER variable" for free.
- **#299: a "dead" cell is NOT emission-neutral.**  The enabler alone (declare
  every referenced name) costs 5 closure.t rows; corpus-diff shows the ONLY
  change to that file is 47 added `p-defcell` lines.  Same risk class as
  #294's `%p-cell-loop-var-p` — **before widening what gets a cell, grep for
  every place that reads "this name has a cell" as a proxy for "this name is
  a global"**.
- **Guard rows outlive a reverted mechanism**: all 20 rows of
  `Pl/t/transpile-test-10.t` pass both with and without the renames, because
  they assert perl's semantics via `test_transpile` rather than the emitted
  spelling.  They are the acceptance bar for the retry.

## s384c (2026-08-12, Opus) — #298: when a syntactic pre-check and PExpr disagree about a comma, ASK PExpr

- **`my $c = bless $c, "C3"` refused the whole file.**  `_lower_block` treats a
  depth-0 low-prec token in a `my`-init as a statement tail and hands the run to
  PExpr INSIDE the fresh `let`, where a self-referential init would read the new
  binding — hence the refusal.  But that comma is a LIST-OPERATOR argument
  separator, and only PExpr can tell (`my $c = h 1, 2` really does pass both).
- **The rule: lower the run in the OUTER scope and read the RESULT.**  Exactly
  one assignment to the declared name ⇒ there was no tail ⇒ its RHS goes into
  the `p-box-init` let binding.  Anything else keeps the refusal.  Lowered
  exactly once — PExpr's cleanup mutates the shared tokens, so a speculative
  re-lowering is not available.
- **Reachable on plain main**: `our $c1` stops the block-shadow rename from
  covering the name.  op/bless.t survives only because ITS `$c1` is poisoned.
- Guard rows in `Pl/t/transpile-test-10.t`, incl. the inverse (`my $x = $x, 1`
  still refuses).  corpus-diff identical across 111 files — a die became code.

## s385 (2026-08-12, Opus) — #297: EVERY `my` in a loop HEAD gets its own `let`

- **The head of a C-style `for` is one lexical scope, and every `my` in it
  scopes to the loop.**  The init counter always had a `let`; a `my` in the
  CONDITION or the STEP, and an init `_single_scalar_decl` declines
  (`my ($x) = …`, `my @a = …`), had none — the declaration lowered to a bare
  write into the package cell, so the name stayed defined after the loop and
  shared storage with a same-named global.  `while (my $x = …)` / `if (my $x
  = …)` have had the wrap since forever; this makes C-for consistent with
  them, through the SAME `_cond_my_names` + `_wrap_cond_mys` pair (rule 11).
- **Why it had to land before #291**: the `__cond__` rename was the only thing
  scoping those declarations.  Verified live by disabling the rename and the
  `_seg_lex` forward-decl exclusion (#291's enabler) together: all five shapes
  then run identical to perl.
- **The registry save/restore now covers the WHOLE head** (it used to start
  after the multi-counter branch), so the multi-`my` init no longer leaks its
  counters into a later sibling's string-eval capture alist either.
- corpus-diff: exactly 2 of 111 files (`my.t`, `loopctl.t`), every hunk the
  let-wrap plus the now-unneeded top-level `p-defcell` for the renamed name.
  Both files re-measured against HEAD in the sweep: identical (67/0, 52/0).
- **Known divergence, filed as #300 and normative in `ir-spec.md` §6.2**: the
  wrap is ONE binding for the whole loop, where perl gives the declaration a
  fresh instance per iteration.  Observable only by closing over it in the body
  — and it is NOT new: `while (my $x = shift @l) { push @c, sub { $x } }`
  diverges the same way and is untouched by #291.

## s385b (2026-08-12, Opus) — #301: ONE predicate answers "does this heredoc interpolate?"

- **A heredoc is RAW exactly when its terminator is SINGLE-quoted**, and perl
  allows both `~` and whitespace between `<<` and a QUOTED terminator:
  `<<'E'`, `<< 'E'`, `<<~'E'`, `<<~ 'E'` are all raw.  PCL asked this with FOUR
  hand-written regexes and every one was narrower than perl — PExpr's `/^<<'/`
  missed both `~` and the space, the three Parser2 copies missed the space.
- **A miss is SILENT-WRONG**: the text goes through string interpolation, so
  `$x`/`@y` vanish and `\n` collapses to a real newline, with no diagnostic.
  Same class as the #138 silently deleted statement.
- **`Pl::PExpr::TokenUtils::heredoc_is_raw` is now the only spelling** — it
  reads `{_heredoc_content} // content` (the rename passes rewrite the marker
  there) against `/^<<~?\s*'/`.  All four sites call it.
- Found by #299's triage: the one perl-tests assertion that catches it
  (closure.t RT #23265) lives inside a top-level form that dies at load today
  (#205), so the row never ran.  corpus-diff: 1 of 111 files, the one hunk
  being that heredoc going from a `p-string-concat` of interpolated pieces to
  a plain string literal.  Guard row covers all seven spellings and was
  verified to FAIL against the old predicate.

## s385c (2026-08-12, Opus) — #296: an EXCEPTION-partition `my` is RENAMED, not re-partitioned

- **The bug (pre-existing since the #290 flip, not caused by #291).**  An
  ORDINARY global is a symbol macro over a cell, so `my $x` beside one is a
  plain lexical `let`.  An EXCEPTION name ($a/$b and the runtime-owned set) is
  still a `defvar` — PROCLAIMED SPECIAL — so `(let (($a …)) …)` is a DYNAMIC
  rebinding: a closure made inside loses the value at scope exit.
  `sub mk { my $a = shift; sub { $a } }` printed nothing where perl prints the
  value.  Same for `my $b`, `my %ENV`, and `for my $a`.
- **CL cannot lexically bind a proclaimed special**, so no emission fixes this
  while the name stays `$a`: the declaration must get a different SYMBOL.
  `symbol-macrolet` over a special is undefined behaviour, so that is not an
  escape either.
- **RULED: rename the declaration (option (a)), do NOT shrink the partition.**
  Shrinking it — making $a/$b ordinary and having the sort lowering bind them
  with `p-local-cell` — would cost every sort CALL ~41 ns (plan §2), still
  leave `my %ENV` broken, and disturb #287's just-shipped package-qualified
  pair.  The rename costs nothing at runtime and is name-decidable: ONE
  `Pl::GlobalPartition::is_exception_global` call.
- **This is NOT the poisoned-my family.**  No poison test, no analysis of
  WHETHER — only the declaration's own scope, which perl states syntactically.
  It OUTLIVES #291, which deletes the other three passes.
- **Ordering is load-bearing**: it runs before `_rename_captured_file_lexicals`
  and before the three poisoned-my passes, so every later pass sees the final
  name.  Probed the other way round: a file-level `my $a` captured by a named
  sub then died "Parser2 TODO: file lexical 'a__excl__0' captured by sub foo".
- **Perl's own rule is preserved, and it is the surprising one**: a `my $a` in
  scope makes a sort comparator read the LEXICAL, not the sort-bound pair.
  Probed — perl `a=LEX`, PCL before `a=9` (the package pair, WRONG), after
  `a=LEX`.  A guard row asserts what the comparator OBSERVED, never the
  resulting ORDER: an inconsistent comparator's order is the sort ALGORITHM's
  answer (perl mergesort vs SBCL stable-sort), not a claim PCL can make.
- **corpus-diff: 42 of 111 files, four explained buckets** — (1) the rename
  plus pretty-printer re-wrap; (2) `__shadow__`/`__cond__` counter
  RENUMBERING, because the earlier pass now renames fewer names; (3) a
  promoted lexical moving from the exception partition to ORDINARY
  (`defvar $a` → `p-defcell $a__excl__N`), which is correct — it IS ordinary
  now; (4) the VarAnnotator and the params fast path giving the renamed
  lexical their NORMAL verdict (raw slot, `p-raw-params`) instead of the
  conservative one the special name forced — correct and faster.
- Nine Pl/t expectations that spell `$a`/`$b` as sample variables were updated
  to the renamed spelling (STRENGTHENS: the suffix pins that the rename fired
  AND the shape held); one was made whitespace-tolerant for the re-wrap.
- **B1 (string-eval capture) SHIPPED s387 (`a062914`)**: in EVAL-MODE
  compilation the CAPTURE ALIST beats the special table — a free $a/$b whose
  spelling the caller's alist carries compiles as that captured lexical (a
  fresh non-special `$a__evalcap__N` bound as a thunk param whose LOOKUP name
  stays `"$a"`); no key → today's special path, so an eval'd comparator with
  no `my $a` in scope still reads sort's dynamic binding.  The five-row
  acceptance table + both reproducers are identical to perl.
- **A capture-dependent emission makes the capture names part of the eval CACHE
  KEY** (s387): eval-mode compilation runs in the `pl2cl --server` SUBPROCESS
  (its request was `pkg\nlen\ncode`), so the names now travel in a `<captures>`
  header line and `*p-eval-string-cache*` is keyed on
  (source, pkg, capture-names).  Without that key, one eval string used from
  two scopes would reuse whichever emission compiled first — silent-wrong.
- **Block-scoped and FILE-level lexicals reach an eval by DIFFERENT mechanisms**
  (s387): a block `my` reaches it through the site capture alist; a file-level
  one reaches an eval inside a NAMED SUB only through promotion to a package
  cell (the sub is hoisted out of the file-level `let`), and that pass finds
  the declaration by its PERL name in the eval text.  So the #296 rename stands
  aside for exactly that case — file-level decl + a string eval inside a named
  sub — at no cost, because a promoted cell is not a `let`.  Keyed on "file has
  ANY string eval" instead, it reverted four corpus files' renames;
  corpus-diff caught it.
- **B2 (later declaration wins) SHIPPED s388; #296 CLOSED, branch merged.**
  `_rename_decl_within` walked from its declarator to the end of the scope, so
  an earlier `my $a`'s rename claimed uses belonging to a LATER `my $a`; the
  later declaration's own pass then found nothing left to rename and its binding
  sat unread.  The walk now ends its claim at a later declaration of the same
  canonical name, in the two shapes `_lexical_decl_scope` distinguishes:
  **same scope → STOP at the LAST TOKEN OF THE STATEMENT** (perl does not
  introduce the new name until the statement finishes, so the redeclaration's
  own initializer still reads the EARLIER variable — probed,
  `my $a = "X"; my $a = "[$a]"` prints `[X]`); **construct scope → SKIP the
  construct**, resuming after it, except the region `_lexical_decl_scope` names
  as evaluated outside it (`for my $x (LIST)`, `while (my $x = …)`,
  `for (my $x = 0; …)`).  A NESTED BLOCK redeclaration is unchanged — still
  `_ref_shadowed`'s call, which is positionally exact there.
- **`_ref_shadowed` cannot see EITHER kind of later declaration** (s388): it
  inspects Block/Sub parents, and neither a same-scope sibling statement nor a
  construct HEAD is a sibling of one.  That is why both spellings were live at
  once, and why the fix belongs in the rename walk, not in the shadow reducer.
  The construct-scoped twin was found by PROBING the family, not by the failing
  rows — `{ my $a = "O"; while (my $a = …) { print $a } }` printed the outer
  value and no test named it.
- **The Pl/t gate was green with BOTH regressions live** — for a change this
  wide the full sweep is the gate, not prove.

## s386 (2026-08-12, Fable) — s385 review: both commits APPROVED; #296's two fix shapes ruled

- **s385 APPROVED as shipped** (#297 + #301): gate independently re-verified
  cold 138/5103; eight probes vs perl identical (five C-for head shapes +
  shadow inverse + body read/write; seven heredoc spellings).  The one
  divergence found is exactly the filed #300, behaving as documented →
  `fable-answers-s385.md`.
- **#296 design call RATIFIED (option (a) rename), and taking it was right**
  — the sign-off rule selected the option needing no ask →
  `fable-answers-s385.md` §1.  Do not re-litigate.
- **#296-B1 RULED: NOT the progv seam** — a progv rebuilds the old
  dynamic-extent approximation one level down (fails the escaping-closure and
  comparator-under-`my` probes, 3/5 vs perl).  THE FIX: in eval-mode name
  resolution the CAPTURE ALIST beats the special table — an exception name
  that is an alist key compiles as the renamed captured lexical (the ordinary
  `__shadow__` path: read/write/#295 pad chain inherited); no key → special,
  unchanged.  Alist membership IS perl's "was a `my $a` in scope here".
  Five-row acceptance table in `fable-answers-s385.md` §2a + task #296.
- **#296-B2 DIAGNOSED: sibling same-scope redeclaration** — an earlier
  exception-`my`'s rewrite region must STOP at a sibling redeclaration of the
  same name (B-ii only covered NESTED redecls); two-line reproducer in task
  #296.  The "not isolated, file-context dependent" conclusion was a
  by-NUMBER TAP join across shifted numbering pointing at a region that in
  fact passes → `fable-answers-s385.md` §2b.
- **STANDING RULE (ask 3, adopted): a sweep-diff bucket count is meaningless
  without the file's row TOTAL, in BOTH directions — and a row NUMBER is only
  meaningful within the run that produced it.**  Join TAP by description;
  for unnamed rows re-derive number→source from the CURRENT tree's own TAP,
  never from the other tree's numbering → `fable-answers-s385.md` §3.
- **Shape-assertion expectation edits (ask 4)**: substitute conjunct = the new
  text is copied from the emission of a build whose RUNTIME behavior for that
  snippet is perl-verified; default on sample-name collision = RENAME the
  sample + one dedicated interaction pin row; the branch's nine edits stand →
  `fable-answers-s385.md` §4.
- **#300 stays filed, unscheduled** (probe-confirmed to behave exactly as
  documented; interleave on a real cause line, no campaign) →
  `fable-answers-s385.md` §5.
- **runpcl/runt no longer delete the program's blank lines** — the blanket
  `s/^\s*\n//gm` noise filter falsified byte-compares vs perl for any output
  containing `\n\n` (leading blanks only now; fixed s386, verified
  byte-identical on three probes).  `tools/run-dist-t.pl` never had it.

## s386b (2026-08-12, Fable) — USER-asked duplication review: the numbers behind the queue

- **v1 is still the PRIMARY expression compiler — 88% of seam expressions
  fall back to `_parse_expression_form` (16,897/19,165); ExprToCL2 native
  ~12%, unchanged since s316t.**  Measured by a full-corpus dynamic call
  trace.  #153's FOLD is the ruled answer; fallback rate = its progress
  metric; chunk 0 = move `_let_bound_vars`/`_catch_labels`/
  `_eval_span_captures` out of the fallback_parser object →
  `compiler-duplication-review-s386.md` §1, task #153.
- **~3.5k lines of the compiler are confirmed dead on the corpus** — the
  pre-E2 TEXT emitters beside their `*_form` twins (ExprToCL 2,238 lines),
  BlockAnalyzer (whole module), 437 lines of superseded v1 handlers,
  VarAnnotator's W12 text-scan remnant.  Deletion batch = task #303,
  AFTER #291; bar = corpus-diff byte-identical.  Eval-mode/bundle/
  instrumentation paths excluded from dead claims (trace = file
  transpiles only) → review doc §2.
- **`PExpr::DEBUG` is a real sub called 4.3M times per corpus transpile**
  (`sub DEBUG { $DEBUG_VAL }` never inlines; SET_DEBUG has zero callers).
  Fix = `use constant DEBUG => $ENV{PCL_PEXPR_DEBUG} // 0` — #303 step 0.
  Hot-spot numbers (accessors, `CLForm::_flat` ×1.24M) recorded in #213.

## s388b (2026-08-12, Opus) — #291's enabler: the forward-decl pass stops excluding let-bound names (#205 closes)

- **The poisoned-`my` renames had TWO causes, and the plan named only one.**
  The flip removed "a `defvar` poisons the section's own `let`" — but the
  renames also existed because `_seg_lex` made `_forward_global_decls` SKIP any
  name the section let-binds anywhere, so without a rename the GLOBAL lost its
  declaration and was unbound at load (probed: deleting
  `_rename_poisoned_block_mys` alone drops `(p-defcell @a …)`).  Both causes are
  the same `defvar` fact, so both die with it.
- **FILE mode: the exclusion is gone** — every referenced name gets its cell,
  and a `let` of the same name shadows the symbol macro.  **EVAL mode keeps
  it**: there the list is not declarations but the p-eval-thunk's capture
  PARAMETERS, bound from the CALLER's lexicals, and a name the eval region
  declares itself is not a caller capture.  That question is untouched by the
  flip.  The switch is the presence of `$free_out`, the parameter that routes
  the list to the thunk.
- **Cost, measured s384: +1.5%–7% emitted lines** (inert `p-defcell`s for names
  that are only ever lexical here: my.t +20, sort.t +58, pack.t +150).
  Accepted — the alternative is guessing a name's role from an assembled-text
  scan, which is what the renames were, and they were scope-blind three times
  (#205, #265, #272).  A future exact answer is the bind-once symbol table
  (`var-handling-review-s379.md` direction B/C), not a fourth approximation.
- **#205 CLOSES here** (the s329 probe — a hidden `<$fh>` readline of a
  file-level `open my $fh`, inside a sub that also block-shadows the name — runs
  identical to perl; it crashed "unbound" before).  Guard row in
  `Pl/t/transpile-test-10.t`.
- **PRECONDITIONS, all shipped first**: #294 (`:my t`), #297, #298, #296.
- **A green gate is NOT the check when a merge touched a TEST file — the COUNT
  is** (s388, caught live): a botched conflict resolution deleted 12 rows from
  `transpile-test-10.t` and the gate still said PASS, at 5096 where 5109 was
  due.  Verify a rebuilt test file against its pre-merge version by diff before
  trusting the run.

## s388c (2026-08-12, Opus) — #291 COMPLETE: the three poisoned-`my` renames are gone

- **All three passes deleted, one per commit, each gated**: `__shadow__`
  (nested bare block, s388c), `__cond__` (if/while/C-for head, s388d),
  `__emb__` (expression-embedded `my` in a sub body, s388e).  Corpus rename
  bindings 716/128/5 → 36/0/0; the remaining 36 `__shadow__` are
  `_gate_seam_my_shadow`, a v1-seam mechanism with a live cause that stays
  until the seam does (#153).
- **Family 3 is not a deletion, it is the FIX the rename was standing in for.**
  `_lower_block`'s let-hoist VETO presumes another named sub shares the
  forward-declared global as this declaration's cell — true at FILE level
  (Capture-Tiny's Utils.pm, #199), false inside a sub body, where no other sub
  can see this body's lexical (#265, #272).  s368 could not simply narrow it,
  because the `let` registered the name in `_seg_lex` and suppressed the
  GLOBAL's declaration.  s388b removed that, so the narrowing IS the fix: the
  veto is not asked when the statement sits inside a sub body.  Same outcome,
  one mechanism fewer.
- **`__excl__` STAYS in the eval-capture-alist strip list.**  Twice during this
  replay the s384 hunk wanted to shorten that list to its s384 shape and would
  have dropped #296's suffix with the deleted ones.  It is a let-bound lexical
  whose suffix the alist must strip so a string eval in its scope finds it
  under the original name — the #296-B1 path.  A suffix leaves that list only
  when its MINTER leaves the compiler.
- **A four-commit replay of reverted work is not a cherry-pick.**  Every
  Parser2.pm hunk conflicted, because the regions the s384 commits delete now
  hold code written after them (#296's two passes sit inside family 1's and 2's
  spans).  Resolved by hand, deletions-only verified per commit
  (`diff | grep -c '^>'` = 0), gate re-run after each.

## s388d (2026-08-12, Opus) — #291's sweep: 4 new closure.t rows, each a NAMED cause

- **GATE clean after the baseline edit**: 0 new / 0 fixed, 2 unstable
  (postfixderef.t, ref.t — known PARTIAL crash files).  **TOTAL passing
  18498 → 18532 (+34)**, accounted for per file: closure.t +14, eval.t +13
  (9 of them #296-B1's, already on main), method.t +4, for.t +2, my.t +1.
- **The 4 new closure.t failures are newly-REACHABLE, not new breakage.**
  Before the enabler the file died at load on `The variable $bar is unbound`
  and `p-load-with-recovery` dropped a whole top-level form; those assertions
  never ran.  Causes, one per row (read individually, not inferred from the
  count):
  - t264 `RT #1028`, t265 `RT #10085`, t270 (unnamed, `is($flag, 1)`,
    closure.t:653) — **DESTROY at scope exit (#198)**.  Each watches a
    `DESTROY` that appends to a scalar; PCL never fires it, so got='1' where
    perl has '12'.
  - t272 `cloneable with //ee` — **an anon sub is not cloned per
    statement-modifier `for` iteration** (`push @s, sub {…} for 1,2`), so
    `$s[0] == $s[1]`.  Its sibling `cloneable with eval` passes.
- **Rows entered the baseline BY EDIT** (s330), verified as a 4-line insertion
  against the previous file before installing; 680 → 684 rows.
- **eval.t's 4 baseline fail rows are absent and reported UNVERIFIED** (the
  file is PARTIAL).  Left in the baseline — removing them would be blessing an
  unverified pass, which is the error the s330 rule exists to prevent.
- **corpus-diff carries no signal for a change like this** and was not run:
  #291 alters emission for nearly every file by construction (cells added,
  renames removed).  The sweep is the measurement.

## s388e (2026-08-13, Opus) — #292's net: fuzzer clean, companion suite clean OF #291 — and the suite SNAPSHOT is 191 commits stale

- **Fuzzer (`tools/difftest-ops.pl`) reproduces the standing clean result
  EXACTLY**: 1060 valid, 1056 match, 4 mismatches in the same 2 blessed
  clusters as s336 (`**` exact-bignum ×3, `ctx-count split` ×1), each with its
  `not-supported.md` section.  No new divergence from the direction-D flip or
  #291.
- **Companion suite vs `baselines/perl-suite-run.tsv`: 44 C_ok decreases and 36
  status changes — NONE of them #291.**  21 files flipped to TRANSPILE-fail
  (`opbasic/cmp.t` alone 12078 → 0).  Discriminating measurement: transpile
  each of the 21 at `66bdb93` (main immediately BEFORE #291) and compare the
  first error line, NORMALIZED for compiler line numbers and worktree paths —
  **19 identical, and the 2 apparent differences are both artefacts**:
  `op/lexsub.t` (my normalizer covered `Parser2.pm` but not `Parser.pm` — the
  message is the same) and `comp/our.t`, where
  `our '$y__shadow__0' shadows a my-lexical` became `our '$y' …` — the same
  refusal now naming the REAL variable because #291 deleted the rename.
  C_ok 0 → 0 on both sides; the message got strictly better.
- **THE SNAPSHOT IS THE PROBLEM, not the compiler.**  `perl-suite-run.tsv` is
  stamped `1e7c4d7` (s323e) — **191 commits back**, before E4.1 (#242, s356)
  flipped v1 gates to HARD ERRORS and retired `--lenient-ppi`.  The TRANSPILE
  cluster is precisely that flip: files that used to yield partial rows through
  a silent v1 re-transpile (or through lenient truncation) now refuse loudly.
  `op/for.t` is the already-documented instance of exactly this (#253).  A
  companion-suite snapshot older than a semantics flip measures the flip, not
  the session.
- **Two STALE registrations cleared BY HAND, not by `--bless-rows`**:
  `mro/inconsistent_c3_utf8.t` (whole-file expected-divergence that now passes;
  removed from `perl-suite-expected.tsv`) and `re/reg_eval.t`'s rowkey
  `regex distillation 4` (removed from `perl-suite-expected-rows.tsv`).
  `--bless-rows` would have rewritten all 1740 rowkeys from a drifted run and
  silently blessed the whole TRANSPILE cluster as expected — the same error the
  fail-baseline's "rows leave by EDIT" rule exists to prevent.  Verified: the
  two files now report OK and XDIFF respectively.
- **NOT DONE, filed as #304**: re-blessing the suite snapshot.  It is a real
  decision, not hygiene — blessing 44 C_ok decreases makes them the new normal,
  and some are deliberate E4.1 refusals while others may be unexamined losses.
  It needs a per-file audit the way #223 gave the sweep baselines.

## s389 (2026-08-13, Fable) — #153 chunk 0 (lex_home) + FOLD chunk 2 (intuit_curly boundary)

- **The lexical registries have ONE owner: Parser2** (`_let_bound_vars`,
  `_catch_labels`, `_eval_span_captures` live on the Parser2 object; the v1
  seam and ExprToCL reach them through `Pl::Parser::lex_home`, which follows
  the `_v2_owner` back-ref, or answers `$self` for a standalone
  prototype-collection parser).  A seam parser whose weakened owner is GONE
  dies in the accessor — never a silent empty registry (`_v2_owned` non-weak
  twin).  Pure ownership move: corpus byte-identical 111/111, sweep clean.
- **The brace group after grep/map/sort followed by `->` Subscript is perl's
  intuit_curly EXPR-form boundary** (probed vs perl 5.40, 13 shapes):
  hash-ctor-shaped OR EMPTY `{…}` → anon-hash term (for sort the deref'd
  value is a plain LIST ELEMENT — sort has no expr-comparator form);
  block-shaped → perl COMPILE-TIME syntax error near `}->`.  PCL: one
  shared `_ctor_deref_verdict` at both entry spellings re-blesses
  Block→Constructor and falls through to the generic parse; block-shaped
  dies perl-shaped.  `eval {…}->…` / `do {…}->…` deref the block's RESULT —
  the chain stays IN the token stream and binds on the funcall node.
- **`$deref_skip` is DELETED — and it was hiding three silent-wrongs**: the
  eval spelling double-consumed the chain (wrapped into body text AND left
  in the stream → `eval {[41]}->[0]` printed empty), sort swallowed the
  deref'd element in BOTH the block and paren spellings, and the
  `inline_lambda` `{deref_skip}` node field had no reader anywhere.  The
  paren path was a full TWIN of the block path (second-copy rule) — both
  copies gone, plus the #78 `has_deref` v1-forcing gate.
- **FOLD chunk 3 design** (recorded on task #153): instrument the legacy
  opportunistic arrow/subscript branches for fired-on-claimed vs
  fired-on-declined over corpus + suite + board, then widen or delete;
  `PCL_NO_FOLD` dies with the deletions.  The legacy reduction is NOT
  wholesale-deletable — it IS the reducer `_reduce_term`'s recursive parse
  invokes for the whole-array case.

## s390 (2026-08-13, Opus) — #303 dead-code batch chunks 1-2, and how a dead-code census lies

- **A dynamic call trace taken by a LOAD-TIME wrap-all-subs tracer cannot
  see a lazily-`require`d module.**  `docs/compiler-duplication-review-s386.md`
  §2 called `Pl::BlockAnalyzer` "whole module, 0 of 11 subs called"; it fires
  **1244x per corpus transpile** (Parser.pm `_with_declarations` requires it
  at runtime).  §2 now carries a CORRECTION block.  Audited: BlockAnalyzer is
  the only lazily-required `Pl::` module.  **Do not delete it.**
- **Moo's `is => 'lazy'` names its builder IMPLICITLY** — `_build_X` can have
  ZERO textual references and run on every object (`_build_fallback_parser`,
  `_build_ppi_doc`).  A grep census must special-case it.
- **`^sub (\w+)` over a `.pm` also matches POD** — `Environment::body`, the
  s386 review's largest single Environment claim (157 lines), is a POD line
  reading "sub body, or direct value of a return statement)".  Confirm every
  candidate with `grep -rn "^sub NAME\b"` before counting it.
- **The bar for deleting a sub is BOTH legs**: static (`grep -rn NAME`, no
  `| head`, whole output) AND dynamic (source-level counter in every
  column-0 named sub, run over corpus AND the Pl/t gate — the gate covers
  eval mode and module transpiles).  Tooling: scratchpad `instrument.pl`
  (`--undo` restores from git) + `census.pl`; recipe on task #303.
  LIMIT: a ONE-LINE `sub f { ... }` records as called at load (its counter
  lands at file scope) — biases toward LIVE, never invents a dead sub;
  settle one-liners by grep.
- **A text emitter whose CLForm twin can decline is not statically dead** —
  measure the declines.  Only `inline_lambda` declines (77 corpus events /
  33 files, 51 gate events); all 24 other named types: zero in both
  populations, so their pre-E2 text emitters are gone (`7285ccc`).
- **`%NAMED_TYPE` exists because `!exists $self->handlers->{$type}` was the
  "this type is a BINARY OPERATOR" test** at both ExprToCL dispatch sites —
  dropping a named type from the table would hand it to `gen_binary_op`
  under its own name.  The set stays; the text table holds one entry; a
  named type reaching the text dispatch DIES (rule 12).
- **The VarAnnotator text annotator is reachable only as a SILENT FALLBACK
  when `_analyze_tree` dies** (plus an unreachable `!$host` guard and
  `PCL_W12_DIFF`).  Recommendation recorded on #303: delete it, make both
  paths die — an annotator decides BOXING, so a second one silently
  substituting is a silent-wrong generator (the E4.1 fallback lesson).
  Behaviour change: needs the s373 gate-SET bar.

## s390d/e (2026-08-13, Opus) — #305 the cast RUN, and the `$$` mis-lex repair

- **PPI lexes `$$` as the PID magic variable unless an identifier follows it
  DIRECTLY** (`docs/ppi-upstream-bugs.md` §1, on file since before s390 and
  now WORKED AROUND).  The mis-lex is not uniform: `$$rr` is correct
  (Cast+Symbol), `$$$rr` is Magic+Symbol, `$$$$rr` is Magic+Cast+Symbol.
  Repair = `Pl::PExpr::_split_pid_magic_cast_run`, ONE token pre-pass beside
  `_default_filetest_operand`; source ADJACENCY comes from PPI sibling links,
  not the whitespace-filtered token list, so `$$ $x` is left alone.  The
  `docs/not-supported.md` "Triple dereference without braces" limitation is
  RETIRED.
- **Perl's rule for a RUN of leading deref casts before a subscript**: the
  OUTERMOST cast decides the ACCESS KIND (`@` slice / `%` kv-slice / `$`
  element), every INNER cast is a deref on the BASE, and a real `->` supplies
  the kind so ALL the casts become derefs.  `$$$rrr->{k}` == `${${$rrr}}->{k}`.
  Both cast-consuming sites read the run through `_cast_run_start` /
  `_all_scalar_casts`; with 0 or 1 casts the behaviour is bit-for-bit the
  pre-#305 one.
- **Folding casts into the base changes what `$pre_n` IS** — it becomes a cast
  node, not a Symbol, so an `is_var($pre_n)` guard on a type decision silently
  falls through.  That turned `@$$arr[0,1]` from a crash into a ONE-element
  silent-wrong for one probe run; `$base_casts` now qualifies for the mapping.
  Probe the mixed-sigil spellings whenever this region is touched.
- **A previously-DROPPED statement that is a TAP assertion renumbers the file**:
  recovering ref.t's three dropped statements shifted every later assertion by
  +3, so baseline rows with EMPTY descriptions (joined by number, not
  description) appear to move.  `sweep-diff` bucketed them as UNSTABLE /
  DID-NOT-RUN; **TOTAL passing is the measure** (18532 → 18535).  Do not read
  those buckets as regressions, and do not re-bless a PARTIAL file's numbers
  casually (#223/#257).
- **BlockAnalyzer's `$pexpr_factory` 4th argument was never passed by anyone**
  — `$usages` was always `{}`.  That path is deleted (s390e); the module's
  other 7 subs are LIVE.

## s391 (2026-08-14, Fable) — s390 review: batch approved, #303's judgment items ruled

Rulings + independent verification in `docs/fable-answers-s390.md`; the same
verdicts are on task #303.

- **All five s390 commits APPROVED as shipped** — cold gate 138/5128 (only
  the 8 user-ignored xs rows), fresh #305 probes byte-identical to perl,
  sweep verdict recomputed from artifacts (TOTAL 18532 → 18535), deleted
  names re-grepped clean.
- **A silently-substituting FALLBACK annotator/emitter is ruled the same as
  a missing case**: VarAnnotator's W12 text fallback and ExprToCL's
  `_gen_interp_replacement_simple` both DELETE, with the miss becoming a
  rule-12 DIE — measurement-first (instrument the fallback paths over both
  populations + sweep; zero events ⇒ delete under the s373 gate-SET bar;
  non-zero ⇒ per-event verdicts first).  `_text_gate_tags` stays (shared
  with the tree annotator's parse-failure fallback).
- **DEBUG→constant is GO** — the "21 live SET_DEBUG calls" blocker
  dissolves: the one non-zero call sits in `if (0) {}`, no live caller
  passes non-zero.  `use constant DEBUG => $ENV{PCL_PEXPR_DEBUG} // 0`;
  bar = corpus-diff + gate; pre-flight the guarded sites for side effects.
- **`_tok_run_desc` KEEP (handoff corrected)** — it is `_term_probe`'s own
  helper; deleting it breaks the retained #153 instrument.  A delete list
  must be closed under "who calls whom" against its own KEEP list.
- **Deleting an unreachable form handler keeps its `%NAMED_TYPE` row** — the
  type must die via the rule-12 arm if it ever arrives, never lower as a
  binary operator (`gen_anon_sub_form` ruling).
- Queue: Opus = #304 (per-file audit, never `--bless-rows`) → #303
  cheapest-first (DEBUG → anon_sub/generate/extras singles → W12 →
  interp fallback).  v1 state handlers stay with #153's FOLD (Fable).

## s392 (2026-08-14, Opus) — #304: the companion-suite snapshot audited, then re-blessed

Full audit + per-file verdicts: `docs/perl-suite-audit-s392.md`; the 37
decreases are owned by tasks #313 (crash), #314 (E4.1 refusals), #315
(undiagnosed rows).

- **A TIMEOUT row's `C_ok` is not comparable across different `--timeout`
  values** — it records how far the file got before the cutoff, not a
  compiler property.  Seven of the 44 apparent decreases were exactly this:
  re-measured at `--timeout 300` the six `re/regexp*.t` files return the
  stale snapshot's numbers TO THE DIGIT (793/794), and comp/require.t comes
  back +366.  Re-measure before diffing; the snapshot records which rows
  were spliced from a 300 s run.
- **The E4.1 coverage cost on the companion suite is concentrated, not
  diffuse**: 21 TRANSPILE files collapse to 7 `Parser2 TODO:` families, and
  ONE of them — `my @a, @b, @c` (a comma-list `my`) — is opbasic/cmp.t's
  12078 rows, 96% of the total loss (task #314).
- **A `Parser2 TODO:` refusal is a compiler GAP, never a blessed
  non-support** — it gets a task, not a `docs/not-supported.md` entry, and a
  snapshot must not be re-blessed until each one has a verdict (the #223 rule
  for the sweep baselines, now applied to the suite snapshot).
- **The audit found a live crash both the gate and the sweep miss (#313)**:
  `Cannot proclaim a macro variable special: <pkg>::@ISA` for any package
  whose name STARTS with a non-ASCII letter — `GlobalPartition::_split_name`
  matches the segment head ASCII-only, so the name falls to the EXCEPTION
  partition and is `defvar`'d over the cell `p-defpackage` already made.
  utf8 package names occur in `t/mro/*_utf8.t` and nowhere in perl-tests/ or
  Pl/t — which is the argument for keeping this snapshot fresh.

## s393 (2026-08-14, Opus) — #303 CLOSED, #313 fixed: two dead fallbacks die, one partition regex widens

- **A three-population ZERO is what buys a deletion, and the probe must be
  positive-controlled BEFORE any zero is believed** (#303 items 5/6).  The
  counters: `VarAnnotator::analyze` entered 1943 / 6242 / 11238 times over
  corpus / Pl/t gate / full sweep, `_gen_interp_replacement` 15 / 26 / 15 —
  and the five FALLBACK arms inside them fired **0** in all three.  Both
  fallbacks are now deleted and their paths DIE: the annotator decides
  whether a name may leave its box and the interpolator produces the
  replacement STRING, so in both cases a fallback verdict is a value the
  program consumes — rule 12's die case, not its announce case.
- **An empty s/// replacement never reaches `_gen_interp_replacement`** —
  `gen_subst_form` only calls it when `_replacement_interpolates` says so,
  and `s/x//` does not (it emits `(p-subst "x" "")`).  So the
  "form is defined but empty" tail was a parse miss like the other two, and
  dies with them.  (This was ruling 6's pre-check; probed s393, don't redo.)
- **`PCL_W12_DIFF` is gone**; the two `reasons` stores it gated now key on
  `PCL_B_DEBUG`, which was already the only reader — a `B-DEBUG` line used to
  print `reasons=[]` unless the *diff* switch was also set.
- **The identifier head in `Pl::GlobalPartition` is Unicode, not ASCII**
  (#313, a LIVE load-time crash): one shared `$ID = qr/[^\W\d]\w*/` for both
  the package segments and the variable name.  The old head class
  `[A-Za-z_]` next to a Unicode-matching `\w` meant `Baɾ::` split and `બʑ::`
  did not; a failed split reads as "not word-shaped" = EXCEPTION = `defvar`,
  which then collided with the symbol-macro cell `p-defpackage` had already
  made for that package's `@ISA`.  **The shape needs the declaration to be
  QUALIFIED** — `package બʑ { our @ISA = … }` at file top level was always
  fine, the nested-block spelling is what crashes.  Guards:
  `Pl/t/global-partition-01.t` (9 rows) + `Pl/t/utf8-source-01.t` (the
  12-line reproducer, run under SBCL).  It repays exactly the two rows the
  #304 audit called owed: `mro/next_edgecases_utf8.t` 0 → 9,
  `mro/basic_utf8.t` 9 → 10, both DIFF → XDIFF again.
- **`my VAR <non-'=' trailing>;` declares ONLY VAR — and ONE predicate says so
  for both consumers** (#314 family F-A1).  `my @raw, @upgraded, @utf8;` is
  perl's "declare the first, evaluate the rest in void context, the other
  names are PACKAGE variables" shape (it warns "Parenthesize"; opbasic/*.t
  runs without strict, so real code writes it).  The lowering had the branch
  for the SCALAR spelling and tested `/^\$\w+$/` — widening it to all three
  sigils, binding through the existing `_fresh_container`, turned
  opbasic/cmp.t from TRANSPILE-FAIL into **12078 ok / 0 not-ok**, its
  pre-E4.1-flip value to the digit and 96% of the flip's companion-suite cost.
  The second half of the same question: `_collect_lexical_names` answered
  "every symbol in the statement", so a named sub reading a TAIL name refused
  with "file lexical captured by sub" for a name that is a package global —
  wrong for the SCALAR spelling too, since it shipped.  Both sites now call
  `_lead_decl_with_expr_tail`.  Guard `Pl/t/my-decl-tail-01.t` (10 rows, every
  expectation probed against live perl).  corpus-diff IDENTICAL: this spelling
  occurs nowhere in perl-tests/, only in the companion suite.
- **A coverage DECREASE can be an assertion becoming HONEST** (#315, s393).
  re/script_run.t "lost" 70 rows to #202/F1: `unlike`'s scanner call used to
  end in `(error () t)`, so any pattern cl-ppcre refused to compile was an
  unconditional PASS — and that file is 185 like/unlike assertions against
  `qr/(*script_run: …)/`, which cl-ppcre rejects outright.  Roughly half are
  `unlike`; those were the 70.  Before reading a decrease as a regression,
  check whether an assertion in that file GAINED a failure path.  Per-file
  causes for all 14: `docs/suite-decreases-s393.md`.
- **`./runpcl` merges stderr into stdout** (`2>&1` in the script), so a runtime
  warning read through it LOOKS like it is polluting the TAP stream.  Separate
  the streams under sbcl directly before concluding anything about which
  stream a diagnostic uses.  (Cost s393 one wrong diagnosis, caught by
  re-measuring.)

## s394 (2026-08-15, Fable) — s393 review: batch approved, both asks ruled

Rulings + independent verification in `docs/fable-answers-s393.md`.

- **All six s393 commits APPROVED as shipped** — cold gate independently
  re-verified 139/5148 (only the 8 user-ignored xs rows), sweep verdict
  recomputed from artifacts (TOTAL 18535, 0 new / 0 fixed), fresh probes vs
  live perl for F-A1 / #316 / #317 all reproduce.
- **Ask 3 (F-A1's absorbed second half): IN SCOPE.**  The boundary rule: a
  filler may absorb a second consumer when it answers the SAME question about
  the SAME syntactic shape and the fix routes both through ONE predicate;
  a second consumer answering a DIFFERENT question is its own commit.
- **Ask 4: script_run / regex_sets are blessed ENGINE non-support** (cl-ppcre
  parity, owner family #196/#71), unlike a `Parser2 TODO:` which is always a
  compiler gap.  Register as XDIFF + not-supported entries — but fix the
  `capture_warnings` test.pl-stub gap FIRST (a harness gap stacked under
  regex_sets), and each entry must say what would LIFT it (#71/PCRE2 lifts
  script_run; `(?[ … ])` is perl-only and survives #71).  Mechanics: #320.
- **NEW residue #318 (review probe): in `my VAR, <tail>;` the tail reads the
  FRESH binding where perl reads the OLD one** (`$x = 9; my $x, $b1 = $x + 1;`
  → perl 10, PCL 1; container spelling likewise).  Pre-existing for scalars,
  inherited by containers; occurs in no population; unscheduled.
- **Skipping the cache-generation bump is fine ONLY with the justification
  stated**: corpus-diff IDENTICAL ⇒ no cacheable input's emission moved (a
  refusal never cached; no cached module has the newly-fixed shape).  Say so
  in the commit/review when skipping; bump in every other case.

## s395 (2026-08-15, Opus) — #314 F-B/F-A2 + `@{+}`, and the four fillers

Session log entry has the per-file numbers; tasks #321–#324 filed.

- **A declaration statement may CONTINUE as an expression, for `our` too**:
  `our $count++;` is the same shape as `my VAR <tail>` — declare the cell,
  lower `NAMES <tail>` through the ordinary machinery.  The
  operator-vs-ASSIGNMENT distinction is only `_tail_decl_convertible`'s
  business (eval-tail value), and it accepts both.  (#314 family F-B.)
- **PPI does NOT spell a variable-declaration attribute as Token::Attribute**
  (it does for subs): inside a Statement::Variable it is Operator(':') +
  bare Words + optional argument Lists, terminated by `=`, `;` or anything
  else.  So `my $x : shared = 1` matched the `my VAR <non-'=' tail>` shape and
  printed EMPTY — **strip the decorations in ONE document pre-pass before any
  decl matcher runs**, never by teaching each matcher about ':'.  Attributes
  on a declaration are dropped with an ANNOUNCE (rule 12 effect-only) and
  a not-supported entry; the protocol itself is task #322.
- **`@{+}` / `${!}` / `%{+}` are VARIABLES, not derefs** — perl's `${ NAME }`
  takes a punctuation name.  PPI folds the identifier and caret spellings into
  one Magic token but lexes the punctuation ones as Cast + Block holding a lone
  Operator; a deref block holding exactly ONE Operator can never be an
  expression, so folding it back is a pure re-tokenization.  In plain code it
  had been a SILENT EMPTY list, in a pattern a die — 2513 rows of
  re/pat_rt_report.t on four assertions.  **`$#-` / `$#+` are one Magic token
  too** (not an ArrayIndex like `$#foo`) — retag, do not add an emission case.
- **`plan` is a perl SUB, so its argument list FLATTENS** — a CL-implemented
  TAP entry point must spread through `p-flatten-args` (the same spreading a
  p-sub does for @_) or `plan reverse 9` arrives as an unflattened vector and
  the file publishes no TAP at all (#317).
- **A glob's stringification must UNDO both case inversions**, and the package
  half needs the exact inverse of `perl-pkg-to-cl-pkg-name`
  (`%pcl-cl-pkg-to-perl-name`): the inversion is applied on the way in only to
  names WITHOUT "::", so inverting unconditionally upcases an all-lowercase
  multi-segment package (`version::regex`).  (#316.)
- **`Pl::Parser2->parse_code` omits the `(p-defpackage :main)` that pl2cl
  emits**, so a program that opens with a non-main `package` and later switches
  back to main dies `package MAIN does not exist` under any Pl/t harness built
  on parse_code.  Use a pl2cl-based harness for multi-package guard rows.
- **perl-tests/t/test.pl's `warning_is` / `warning_like` / `warnings_like`
  MANUFACTURE a pass** — they run the code and never compare the warning.
  That is the #202 class and it is evaluable (pl-warn does invoke
  `$SIG{__WARN__}`); measured population and the baseline work in task #323.
- **`refaliasing` was NOT removed in perl 5.40** — `docs/not-supported.md` said
  so and rested its "do not implement" rationale on it.  Probed on 5.40.3: all
  five shapes (`\my $x = \$y`, `\my @b`, `\my %g`, `our \$T = \$::TODO`,
  `for \my %e (@list)`) work, warning only `Aliasing via reference is
  experimental`.  Entry corrected in place; task **#325** sizes the feature —
  it is ONE cause behind ~1400 rows of #314's residue across four t/ files, and
  in PCL's model an alias is "two names hold the SAME box/vector/hash object".
- **re/regexp.t and its five siblings HANG at TAP row 906 — they are not slow**
  (s395, task #326).  `--timeout 150` and `--timeout 400` give byte-identical
  counts (793 ok / 112 not-ok); a slow file grows with time, and the contended
  90 s run giving FEWER (344) shows what slowness looks like in the same data.
  Six drivers share one data file (`t/re/re_tests`, 2169 independent one-line
  regex tests), so ~7500 rows are unmeasured.  **The last TAP row names the
  offending pattern** — never bisect by adding exits to perl's own `.t` files.
  comp/require.t is the genuinely SLOW one (909 at `--timeout 300`, 350 at 90 s)
  and wants a `baselines/perl-suite-timeouts.tsv` row.

## s396 (2026-08-15, Opus) — #325 refaliasing: the assignment forms

- **A `\`-cast in LVALUE position is an ALIAS, and PCL's box model makes it a
  one-line move**: the assignment REBINDS THE NAME'S STORAGE to the right-hand
  referent — `(setq $x <$y's box>)` for a scalar, the vector/hash-table object
  for `@`/`%`, the referent box written into the SLOT for `\$h{k} = \$v`.
  Both names then denote one object, which is perl's aliasing exactly, and
  `\$x == \$y` follows for free (a ref's identity is its referent's).
- **The whole surface converges on ONE p-setf arm.**  Every spelling the
  emitter produces — `\$x = …`, `\my @b = …`, `our \$T = …`, `(\$x) = @_`
  (through `p-list-=`'s default arm, which routes an unrecognised element form
  to `p-setf`), `\$h{k} = …` (`p-gethash-box` place), `\&c = \&d`
  (`p-backslash-sub` place) — arrives as a `p-setf` PLACE that is a `\`-cast.
  So the fix is one arm in the place dispatch, not a branch per parse path.
  Before it, four of six spellings were **silent wrong**: the place was a
  FRESH ref box, so `(box-set (p-backslash $x) …)` wrote into a temporary.
- **`\$x = \$y` vs `\$x = $r` differ by ONE BOX LAYER, and `is-ref` is what
  tells them apart** — not a layer count.  `p-backslash` sets `is-ref` on the
  wrapper it makes; a VARIABLE holding that wrapper does not have it.  Reading
  the flag keeps a reference-to-a-reference right, where "peel twice" would
  take one level too many.  (`p-alias-scalar-target`.)
- **`\(EXPR)` collapses to `\EXPR` as an rvalue but NOT as an lvalue.**  One
  ref either way for a value, but the parens are what make `\($x) = @_` a LIST
  assignment (perl aliases $x to $_[0]); `\$x = @_` is scalar context and dies.
  The one-element case is the only one that needed saying — the multi-element
  spelling already emits a `(vector …)`.  (`_is_backslash_paren_lvalue`.)
- **`our \$T = \$::TODO` is the DECLARE-then-lower-the-tail shape again**
  (s395's family F-B): read the names PAST the cast, leave the cast in the
  expression tail, and the alias falls out of the ordinary expression path.
- **#325's row estimate was 46% wrong, and only running the file could say
  so.**  `t/re/opt.t` (639 of the ~1400) had exactly one refusal, so the task
  sized it as reachable; with the refusal gone it transpiles, runs, and dies on
  **`re::optimization`** — perl's XS readout of its own regex optimizer
  (minlen/anchored/floating/stclass).  That is not regex SEMANTICS, it is one
  engine's internal state, and cl-ppcre is a different program: registered
  XDIFF with a not-supported entry.  **A "one refusal blocks N rows" estimate
  is an upper bound until the file has actually run.**
- **PPI cannot lex a `for` whose loop variable is a `\`-cast or a non-scalar**
  — the compound statement keeps only the word `for` and swallows the rest of
  the file into one flat sibling, so no tree edit can repair it.  Fixed (#327)
  by token-stream repair + reparse (the #270 pattern), spelled as a rewrite
  into `for my $tmp (LIST) { \my %e = $tmp; BODY }` so the alias mechanism
  above does the work — no new foreach macro, no VarAnnotator work, and the
  loop variable is scoped to the body and fresh per iteration, which is perl's
  scoping.  `t/op/const-optree.t` 0 → 86.
- **perl does NOT restore an aliased PACKAGE foreach variable** (probed 5.40.3:
  `our $s = "orig"; for \$::s (\"a", \"b") {} print $s` prints **b**).  An
  ordinary `for $pkgvar (…)` localizes and restores; the aliasing form leaves
  the last alias standing.  So the foreach rewrite needs no save/restore.
- **A `\`-cast lvalue must be matched BEFORE the sigil regexes** in the
  assignment dispatch: a package target emits `(p-backslash main::%a)`, whose
  TEXT matches the `::%` hash-assignment test and lowers as an ordinary hash
  assignment (`(boundp '(p-backslash %a))` at runtime).  The lexical spelling
  has no `::` and fell through to `p-setf` by luck — a reminder that the
  dispatch chain is ordered text matching, so a new place form must be placed
  above every pattern its text could satisfy.

## s396b–c — the n-at-a-time foreach, and what #314's residue actually costs

- **perl 5.36's `for my ($q, $r) (LIST)` mis-lexes exactly like the refaliasing
  foreach**, and takes the same token-stream repair (#329).  The re-spelling is
  a `while` over `map \$_, LIST` with `\my $q = $L[$I]` per variable.
- **`map \$_, LIST` is the per-ELEMENT refgen; `\(LIST)` is not.**  map aliases
  `$_` to each element, so `\$_` is a write-through ref to the original —
  probed identical to perl for arrays, several arrays, a hash, literals and
  `reverse`.  `\(LIST)` distributes over the list's TERMS, so **`\(@Q, @A)` is
  two ARRAY refs** (perl's own answer): right for `\(…)`, silently wrong as a
  general element-ref idiom.
- **`while` + `continue`, never a C-style `for`, when a perl loop may carry a
  continue block** — and putting the STEP in the continue block is what puts
  the three non-local exits where perl puts them: `next` runs the continue
  block and the step, `redo` runs neither, `last` leaves.
- **`\(%h)` is 2N scalar refs, not one hash ref** (fixed with #329, it was a
  pre-existing silent-wrong): a hash flattens to a key/value list in list
  context exactly as an array flattens.  `p-refgen-list` now delegates the
  hash case to `%p-flatten-list`, the same flattener list assignment uses, so
  the ORDER matches `%h` everywhere else and the VALUE boxes arrive unwrapped
  (which is what makes the value refs write back).
- **PCL has read-only ARRAYS but no read-only SCALARS**, so
  `Modification of a read-only value attempted` never fires for a scalar and a
  literal in a foreach list is writable.  Six rows of op/for-many.t; task #330
  weighs it against `box-set` being the hot path.
- **Half of #314's remaining residue is not reachable, measured not guessed.**
  op/coresubs.t (1109) needs `use B` + `B::walkoptree` — waiving its state
  blocker experimentally makes it transpile and produce ZERO rows.
  op/svleak.t (156) needs `XS::APItest::sv_count` (PL_sv_count), io/shm.t (21)
  needs IPC::SysV, re/opt.t (639) needs `re::optimization`, and 62 of
  op/const-optree.t's remainder are `B::` optree inspection.  **A t/ file that
  measures perl's INTERNALS is not a PCL row count** — check what a file's
  assertions actually read before sizing a family from its plan.

## s396d — sweep-diff's FIXED bucket also counts rows that VANISHED

- **A row that stops being EMITTED reads as FIXED.**  `sweep-diff.pl` joins
  failing rows by description; a baseline failure whose assertion no longer
  runs at all is simply "not failing", so it lands in FIXED next to the genuine
  ones — and LOST cannot catch it either, because LOST reads the PASS baseline
  and this row was never passing.  **When a file's FIXED count and its emitted
  ROW COUNT move together, audit the descriptions.**  s396 did: two of
  aassign.t's four "fixed" rows had disappeared, because
  `\($a[0], $a[1]) = \($y,$x)` hit rule 12's die at MACROEXPANSION and took the
  whole enclosing form (and its two `is` calls) out of the load.
- **An element lvalue has FOUR spellings and a place-dispatch arm must take all
  of them**: the `-box` twins the emitter uses in lvalue position
  (`p-aref-box`/`p-gethash-box`) and the PLAIN accessors, which is what a LIST
  assignment's element forms are — each also in its deref flavour.  The first
  refaliasing arm took only the `-box` pair, so the single-element spelling
  worked and the list spelling died.  The container argument differs in shape
  between them, so the helpers resolve it through `p-cast-@` / `p-cast-%`
  rather than assuming one.
- **The full sweep is what found it, and the Pl/t gate could not have.**  Both
  gates were green with the bug present; only the per-file row audit against
  `baselines/pass-baseline.tsv` showed the two assertions had ceased to exist.

## s396e — "already a vector" and "spreads under `\(…)`" are two questions

- **They differ on exactly ONE shape, a HASH**, and conflating them broke
  op/inc.t.  `_child_is_list_expr` answers *"does this already EVALUATE to a
  vector, so a paren around it needs no `(vector …)` wrapper?"* — a hash
  evaluates to a hash-table, which is NOT a vector.  `_is_list_node_for_refgen`
  in `'spread'` mode answers *"does `\(X)` distribute over more than one
  element?"* — a hash spreads into its 2N key/value scalars.
- **Widening the wrong one changes LVALUE lowering**, because
  `gen_tree_val_form` consults the same predicate: with `%h` called a list,
  `(%h)` stopped emitting `(vector %h)`, so `(%h) = LIST` lowered as
  `p-hash-=` instead of `p-list-=` and the chained
  `my (%orig) = my (%inc) = my (%dec) = …` in op/inc.t lost 8 rows.  One
  function, one MODE argument, so the shared part cannot drift.
- **An orphaned `pl2cl --server` can hold GIGABYTES for as long as it lives.**
  An SBCL child that used string eval spawns one; when `timeout -k` SIGKILLs
  that SBCL the server is reparented to init, and because its loop only checks
  stdin BETWEEN requests, one caught mid-transpile (op/cond.t's 20k-nested
  ternary — the quadratic pathological-nesting case) never notices EOF.  Two of
  them held 4.8 GB and 4.6 GB for half an hour in s396.  Both runners now reap
  `pl2cl --server` processes whose PPID is 1 between files — PPID 1 is the
  whole test, so a concurrent run in another shell is untouched.  This is the
  MEMORY half of #273, and it feeds #215: MemAvailable is what decides whether
  a parallel sweep stays stable.

## s396h — PPI bugs get LOGGED, not just worked around (CLAUDE.md rule 13)

- **USER RULE (s396): every PPI mis-tokenization or mis-lex PCL works around
  gets a section in `docs/ppi-upstream-bugs.md` IN THE SAME COMMIT**, plus a
  runnable case in `docs/ppi-bug-report.t` (the self-contained Test::More file
  that IS the upstream report — every row FAILS on the current PPI, because a
  failing row is the bug).  Reason: PPI is upstream software we depend on and
  keep finding real bugs in; the workaround is exactly the moment the bug feels
  finished and the logging feels skippable.  An unlogged PPI bug is a fix we
  owe upstream and never send, and a rediscovery a future session pays for.
- **Backfilled s396**: FOUR bugs had been worked around without an entry.  Now
  §8 (a variable declaration's attribute is `Operator ':'` + Words, not a
  `Token::Attribute` — PPI *does* produce one for subs), §9 (`${ PUNCTUATION }`
  is a variable: PPI folds `@{foo}` and the caret spelling but leaves `@{+}` as
  Cast + Block, and `$#-`/`$#+` come back as one Magic token where `$#foo` is an
  ArrayIndex), §10 (`for` accepts only `[my] $scalar`, and unlike §6 fails
  SILENTLY — the Compound keeps just the keyword and swallows the rest of the
  construct plus every following statement to the next `;`).
- **A canary row asserts the CURRENT broken behaviour**, because the repair is
  keyed on it: when PPI is fixed the repair stops matching and the shape breaks
  again silently.  A FAILING canary is the signal to DELETE the workaround, not
  to fix the row.  Three added to `Pl/t/misc-fixes-02.t` beside the §6 one.
- **Scope, unchanged**: only what PPI itself tokenizes or structures wrongly.  A
  correct token stream that PExpr then reads wrongly is PCL's bug and belongs in
  a task.

## s397 (2026-08-15, Fable) — review of s395+s396; the orphaned-server fix belongs IN the server

- **s395 + s396 batches APPROVED as shipped** (`docs/fable-answers-s396.md`):
  cold gate independently re-verified 141/5203 (failures exactly the 13 pclxs
  xs rows), full sweep re-run GATE clean TOTAL 18539 = baseline, ten fresh
  refaliasing / n-at-a-time probes vs perl 5.40.3 all identical, compile time
  on pack.t 2.76 → 2.76/2.87 s (noise).
- **A process that can outlive its parent is the ONE place that knows it did
  — liveness policy lives IN `pl2cl --server`, not in every caller.**  The
  server now ticks once a second (`$SIG{ALRM}` + `getppid()`); a changed parent
  means the client SBCL is gone and the reply would go nowhere, so it
  `POSIX::_exit`s.  Measured: a server orphaned 0.5 s into a 2.8 s transpile
  was gone 0.5 s after its parent died; two requests answered across five idle
  ticks (PerlIO retries the EINTR'd read after the handler).  This closes the
  MEMORY half of #273 for every adoption target, and it is the fix the s396
  ASK 2 option (1) asked about — option (2) (an SBCL exit hook) was rejected:
  it cannot see the SIGKILL case, which is the one that happened.
- **`PPID == 1` is NOT what "orphaned" looks like on a systemd desktop.**  Every
  orphan under a `systemd --user` session is adopted by THAT process (a
  subreaper, PID 4471 here), so the s396 reaper's PPID==1 key never fired on
  the machine it was written on — measured with a real orphaned server (ppid
  4471, comm `systemd`).  The reapers in both runners stay as the belt (a
  server stuck in ONE long op cannot run the tick) but now key on "parent is
  a reaper" = PPID 1 or parent comm `systemd`/`init`, still never touching a
  server whose parent is an sbcl or anything else.  Rule earned: **a
  process-tree assumption is measured on the machine, not inferred from the
  textbook** — `sh -c 'cmd &'` and `ps -o ppid` is a ten-second probe.
- **No new suite verdict for "measures perl's internals" (s396 ASK 1).**  XDIFF
  + a not-supported section already say "explained divergence, per row, STALE
  if it ever passes"; whether the cause is CLOSABLE is a property of the
  ENTRY (its "what would lift it" line), not of the verdict — and half the
  named files are not internals at all (IPC::SysV is a module PCL could shim;
  op/coresubs.t and op/svleak.t are still behind REAL compiler declines that
  stand on their own).  What the ask is right about is countability: the
  class gets ONE not-supported section ("Readouts of perl's own internals:
  `B::` optrees, `re::optimization`, `XS::APItest`") that every such
  registration cites, so `grep -c` of that section name in
  `perl-suite-expected.tsv` IS the population.  op/const-optree.t (86/62,
  every diverging row a `B::` inlinability/`:method` readout) may register
  XDIFF under it now — the all-or-nothing bar is met.
- **#323 (the three test.pl stubs that manufacture a PASS) is scheduled as its
  own session, not a filler** (s395 ASK 1): it is a baseline event by
  construction (8 sweep rows + ≥8 companion files flip red for CAUSES, not for
  bugs), so it needs the sweep + companion run + per-row cause edits in one
  sitting; the false passes are known and bounded, nothing is hidden by
  waiting.  After #331 and the F-D fix, before the v0.1 track.
- **The parenthesized-ARRAY refaliasing spellings are SILENT WRONG — task
  #332.**  `\(@a) = (\$x, \$y)`, `\my(@x) = \(@y)`, `\(my @c) = LIST` (perlref
  §"Assigning to References": "@x now contains $x, $y, and $z") reach p-setf as
  the place `(p-list-scalar (p-refgen-list @a))`, which is not a `\`-cast place
  and so falls through to a value write into a throwaway — perl prints `10 2`,
  PCL prints an empty line, exit 0.  The slice spellings (`\@a[5..7] = …`,
  `\@h{'foo','bar'} = …`) correctly DIE naming the target.  Rule 12: the die is
  the minimum; the fix is per-element (replace @a's contents with the referent
  boxes).  Found by review probes; occurs in no population.
- **`cl/pcl-pack.lisp` / `cl/pcl-mro.lisp` are ELEVEN generations stale
  (v2-136 vs v2-147) — #331 is Opus's next-session OPENER**, with the
  staleness CHECK (artifact line-1 generation vs `*pcl-cache-generation*`) as a
  Pl/t row so it cannot recur silently.
- **#153 FOLD chunk 3 MEASURED (s397, Fable), work PARKED on branch
  `wip/s397-fold-chunk3` — do not merge without the verification steps on
  task #153.**  The chunk-1 fold never claims an embedded METHOD CALL WITH
  ARGS: `handle_subcalls` has already reduced `name(args)` to a `<funcall>`
  node by fold time and `-> <funcall>` is not a step of the chain walker
  (`_extend_postfix_chain`), so `$o->m(args)` embedded stays with the legacy
  loop — 14,663 firings on the 14-dist board alone.  Same for `-> (args)`
  coderef calls, word-led `Class->m(...)`, list slices `(...)[i]`, `->$#*`,
  `*x{IO}`.  perl REJECTS `f()[0]` and `$o->m()[0]` (probed) — only a List or
  qw() primary takes a Constructor as a list-slice group.  Instrument:
  `PCL_FOLD_PROBE=1` + `tools/term-diff-sweep.pl`; verdicts WHOLE / DECLINE /
  GUARD / WALKER-STOPS / CONT / GAP.  A ()-replacement artefact (method args
  become `<tree_val>` before the legacy loop) read as a false GAP until the
  probe mirrored the walker's `-> name (args)` step over the node — a probe
  that judges at a LATER pass than the thing it measures must replay the
  passes in between.

## s399 (2026-08-15, Opus 5) — the s397 queue items 1–6, and the first #138 drop census

- **The checked-in transpiled artifacts are THREE, not two** — `cl/pcl-pack.lisp`,
  `cl/pcl-mro.lisp` and `cl/pcl-warnings.lisp` (from `lib/warnings.pm`) — and
  their staleness is now a GATE: `Pl/t/artifact-staleness-01.t` compares each
  file's line-1 `gen=` stamp against `*pcl-cache-generation*`.  Artifacts are
  DISCOVERED by the stamp, not listed, and a count row keeps a header-format
  change from making the file pass vacuously (that row is how the third
  artifact was found).  Consequence, deliberate: **an emission-changing commit
  that bumps the generation must regenerate the artifacts in the same commit**,
  which is what CLAUDE.md already asked for and nothing enforced.  → s399a
  `7af2a97`, task #331.
- **`\(@a) = LIST` / `\my(@x)` / `\(my @x)` alias the ELEMENT SLOTS** and
  resize the array to the right-hand length (perl REPLACES, it does not merge).
  The lvalue reaches `p-setf` as the `\`-cast place `(p-backslash-list @a)`;
  the runtime arm is `p-alias-array-elements`, resolving referents through the
  same `p-alias-scalar-target` as the scalar arm.  Any other `\(…)` target
  DIES at transpile (rule 12) — `\(%h) =` and `\(@$ref) =` are compile errors
  in perl too.  → s399b `bf3fe69`, task #332, guard `Pl/t/refaliasing-01.t`.
- **"Readouts of perl's own internals" is ONE not-supported section** (`B::`
  optree inspection, `re::optimization`, `XS::APItest`), and citing its name in
  `baselines/perl-suite-expected.tsv` IS the population (`grep -c`).  Its
  "what would lift it" line is *nothing*.  → s399c `f8ffd56`, ruling
  `fable-answers-s396.md` §4.
- **op/const-optree.t does NOT register, and the ruling's premise was wrong** —
  the per-row read the bar demands splits its 62 diverging rows 53 internals /
  5 §Error-compatibility (`now throws exception (RT 134138)`) / **4 a real fix
  target**.  All-or-nothing keeps the file UNEXPLAINED.  A registration
  authorised in the abstract still has to survive its per-row read.
- **`my sub NAME` compiles to a PACKAGE sub** — two same-named lexical subs in
  different scopes clobber each other and every `\&x` resolves to the last one
  (perl `8 3`, PCL `3 3`, silent).  In isolation PCL is right, so it needs two
  of them to show.  → task #337; op/lexsub.t is the same feature's other half.
- **A claim that cannot be evaluated must not manufacture a pass — now also in
  `perl-tests/t/test.pl`**: `warning_is`/`warning_like`/`warnings_like` carry
  the real t/test.pl bodies.  24 rows went honest and RED with ONE cause.
  → s399d `6f04839`, task #323.
- **PCL emits no warnings-gated diagnostic, and that now has a section and an
  owner** (`not-supported.md` "Warnings-gated diagnostics are absent"; task
  #221).  **#221's trigger — "the first test family whose failure is *warning
  not emitted*" — HAS FIRED**: assignwarn.t (20), hashassign.t (4),
  op/numify.t (11), op/utf8decode.t (86).  The standing rule is unchanged
  (default-off diagnostics stay ABSENT, never unconditional); what changed is
  that the debt is now countable.
- **A file whose TAP order depends on hash iteration gets `*rows-unstable*`** —
  measured, not assumed: op/assignwarn.t iterates `keys %should_warn`, so both
  sides emit rows in per-process random order and the description-multiset
  pairing reports a different missing/extra split every run (81 then 73).  The
  COUNTS are stable, so the file still registers XDIFF; only the ROW check opts
  out.  → s399d.
- **A LIST-form `my` may span a package boundary** (#314 F-D scalar half): the
  span pass's candidate declarations are `scalar_decl` + the `$`-sigil entries
  of `mlist_decl`, merged in one accessor because perl declares each name of a
  list form exactly as the single form does.  The promotion pass keeps them
  separate — merging there processes a list decl twice.  io/shm.t transpiles.
  → s399e `c754abc`.
- **The span checker's die names the CANONICAL variable** (`my-lexical 'a'
  (canon @a) …`).  The bare name hid the one fact that says which loop must
  handle it, and cost a probe.
- **#314 F-D's container half is TWO different things, and the s395 diagnosis
  of it was wrong** — not "container decls are not recorded" (`container_decl`
  and its own span loop have existed since s305) but (b1) op/svleak.t declares
  `my @a` three times, so the loop's file-uniqueness rule refuses, and (b2) a
  container name inside a list decl needs the DECL LOWERING, which knows only
  the no-init single-container shape.  → task #338.
- **The #138 silent-drop family is 72 files / 379 drops, measured for the first
  time** (`baselines/parse-error-drop-census-s399.tsv`, task #343): 9 perl-tests
  files, 63 companion, ZERO in lib/; 56 of them one message ("Bug. Fell
  through. Missing case: [").  A drop is NOT cosmetic — bless.t's is the test
  row `is ref $untied, "main", '…' or diag $@;`, which never runs and appears
  in no count in a file the sweep reports as passing.
- **The census's dominant cause minimises to `f ref $u, "m" or g "fb"`** — a
  parenless call × a named-unary first argument × a following low-precedence
  `or`/`and`.  Parenthesising the arguments, or dropping the named unary, works.
  It is the `$end_pars` region (`pexpr-term-parsing-review.md`: do not patch in
  place), so it belongs — with #259 and #335, the same fall-through — in Option
  B **phase 2**'s acceptance set, and the census is phase 2's metric.
- **PExpr's `Handle single node of unknown type` warn is NOT pure noise** —
  deleting it was tried and REVERTED: in op/glob.t that decline becomes a
  `;; PARSE ERROR` drop of `ok <~>, '~ works';`.  The distinction that matters
  is handled-decline vs dropped-statement, and it belongs at the caller that
  emits the drop, not at the die site.  → task #339.
- **The `PCL:` refusals that appear as drops are DELIBERATE**, not an
  oversight: `_shape_expr_error` re-raises every `PCL:` error except
  "Can't modify non-lvalue subroutine call in assignment" outside eval mode
  (the eval half must propagate so the eval returns undef, like perl).  Whether
  the FILE-level case should die like perl is an open design call — 33 of the
  379 drops.

- **`tools/run-perl-suite.pl` was the ONLY runner without
  `--control-stack-size 512`** (the gate, the sweep and `./runpcl` all pass
  it), so the companion suite measured PCL on SBCL's 2 MB default and four
  files died `control-stack-exhausted` THERE AND NOWHERE ELSE.  #324's
  `(?{ CODE })` attribution was wrong, and its probes "did not reproduce"
  because they ran through `./runpcl`, which has the flag.  **When one runner
  disagrees with the others about a crash, diff the RUNNERS before the code**
  — second occurrence, so the four hand-written sbcl command lines are now
  task #344.  Fixed s399j: +37 C_ok (re/pat_rt_report.t 2431→2454,
  op/utf8cache.t 0→2, re/pat_psycho.t 0→11, re/speed.t 0→1).  The flag costs
  memory — `--all --jobs 8` reserves 8 × 512 MB and got SIGTERMed at 41 of 521
  files; `--jobs 4` is comfortable, `PCL_SUITE_STACK_MB` overrides.
- **`tools/corpus-diff.pl` prints the SILENT-DROP count on both sides** — a
  `;; PARSE ERROR` progn is invisible at run time, so nothing else notices one
  being added; the population-wide version is `tools/drop-census.pl` (#343).

## s398 (2026-08-15, Fable) — #153 FOLD chunk 3 verified and flipped; Option B phase 1 complete

- **#153 FOLD chunk 3 SHIPPED (s398a–c: `f501ada`, `c6a211e`, `5323d9e`);
  Option B PHASE 1 (reducer + fold) is COMPLETE.**  Nine walker widenings
  W1–W9 (`-> <funcall>`, `-> (args)`, `-> $#*`, list-slice group after a
  List/Condition/qw() primary, word-directly-followed-by-arrow primary +
  Quote::/qw()/word-arrow start set, `*name{SLOT}` glob slot, `-> ${EXPR}`
  computed method + args, PPI `Structure::Condition` as a paren primary,
  further `[j]` after a slice); emission byte-identical over ALL FOUR
  populations in BOTH compares (main-vs-branch AND fold-on-vs-off), so no
  cache-generation bump (s375 rule).  `PCL_FOLD_PROBE` + `PCL_NO_FOLD`
  deleted at the flip; the in-loop DynGlob handler is a rule-12 die
  (unreachable by argument — the pre-pass consumes the triple first — and by
  measurement: zero over the four populations + all 135 `*{` files of 108
  dists).  → session-log s398, task #153 `chunk3_s398`.
- **The fold's residue is BY DESIGN, not a gap** (final re-probe): board14
  and lib ZERO embedded firings; corpus 0 true misses; suite 7 = whole arrays
  with an arrow-less trailing CALL `(…)[0]()` (the `$h{k}(1)` inverse — a
  List after a subscript is not the term grammar's), `$${$_[0]}` (PPI
  `$$`-mislex spelling; the walker declines, Xsub reduces), and `return
  (…)[3]` (a `return(...)` funcall node + slice).  Do not widen for these
  without a population that needs it.
- **Chunk 3 does NOT move the s386 seam-fallback metric, by construction**
  — the fold lives inside v1's PExpr; the 88% falls only when v2 consumes
  the reducer's output.  Remaining Option B scope = PHASE 2 (operator
  binding: the `$end_pars` machinery, "take the next node") — on #153.
- **`(LIST)[i]{k}` — arrow-less hash subscript after a list slice — was a
  SILENT PARSE-ERROR drop; fixed at ONE pre-pass** (s398b,
  `_retag_list_slice_subscripts`, sibling of `_retag_braced_deref_subscript`):
  PPI labels the `{k}` after a slice's `]` a Block by predecessor; re-bless
  to Subscript so the `$h{a}{b}` chain path and the walker both see a plain
  chain.  The `[j]` twin (`([qw/a b/])[0][1]`) is left on CtorSub — it
  works and its emission is unchanged.  Guard rows transpile-test-10.t.
- **A forked child's `END` block runs too** — an A/B driver whose END
  removed the ref worktree deleted it under its siblings (every ref
  transpile rc 2, empty stderr); guard with `$$ == $parent`.  The driver is
  now `tools/emission-ab.pl` (parallel two-ref or env-flag emission A/B over
  ANY file list — the four-population bar's tool; corpus-diff.pl stays the
  one-command corpus check).  Same family
  as the s397 "probe judges at a later pass" note: measure the measurer.
- **Filed, all PRE-EXISTING on main (fillers)**: #333 `Foo->x => 1`
  autoquotes the method name (silent nil); #334 `->can('Pkg::name')`;
  #335 `print 1 if (f())[1]` silently false (Condition + Constructor →
  Missing case — `is_list` should accept the Condition mislabel, one
  predicate); #336 `(qw(a b), "c")[2]` — a qw inside a SLICED list is not
  flattened.
