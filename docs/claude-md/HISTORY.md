# CLAUDE.md HISTORY -- what was moved out of `CLAUDE.md` and is NOT needed to do a task

Moved VERBATIM out of `CLAUDE.md` at the s541 split (2026-11-13; the pre-split
file is `git show 8afc7dda:CLAUDE.md`).  Three parts:

1. **Test Status -- the numbers history**: the gate / sweep / board / everyday
   numbers and a summary of every merge from MAIN-READY-92 (s540) back to
   s315, newest first.  At each merge the reviewing session moves the
   paragraph it replaces in `CLAUDE.md` "Test Status -- CURRENT NUMBERS" to
   the top of this part.
2. **Key Files to Read -- the review record**: the compressed record of the
   review rounds s321-s439 (items "2", "2a".."2z", "2b0..."): what each
   session shipped and what was ruled.  Every ruling in it is indexed in
   `docs/DECIDED.md`; its operational rules are in `CLAUDE.md` "Standing rules
   from the review rounds".
3. **V1-era notes**: the old Architecture tables, "What is Implemented", Code
   Generation Patterns (they show the OLD `pl-` prefix; the runtime uses `p-`),
   Config.pm parameter specs, Common Pitfalls, Common Tasks, TODOs.  The
   current structure is `docs/v2-target-architecture.md` and `docs/ir-spec.md`.

The working reference (full principles, commands, WHAT TO RUN WHEN, deep
dives, dependencies) is NOT here: it is `docs/claude-md/REFERENCE.md`.

---

# Part 1 -- Test Status: the numbers history (newest first)

## Test Status

- **CURRENT NUMBERS (s540, 2026-11-13 18:0x, main = MAIN-READY-92 `212d4782` = s539d merged -- `caller` FIDELITY, THE CHEAP TIER (#233 tier 1 + #1361; the USER's s539 ruling: only where cheap in time and code size; every item shipped with its cost A/B, two were MEASURED AND LEFT OUT): the sub-call FRAME is one preallocated vector indexed by `*pcl-sub-call-depth*` (five slots: the caller's package, the sub, file id, line, context; `p-frame-bind`, no allocation) replacing the two consed stacks and the `sb-debug` walk -- a named call 35 -> 31 ns, call rows FASTER (fib -20 %, fibret -42 %, subret -10 %, methret -16 %, emptysub -12 %); `caller` with no argument answers perl's three fields, `caller(N)` its sub name / hasargs / wantarray; an anonymous sub gets a frame ONLY where its body calls `caller` or compiles perl at run time (a frame for every anonymous sub cost +12 % a call: left out); `# line N "file"` is honoured (constants only); `croak` / `carp` / `confess` / `cluck` report the first caller outside the package by perl's rule through the `lib/Carp.pm` shim.  **FILE AND LINE of a call are recorded only under `PCL_OPT=line-track`** (recording them by default costs +4-6 % on counting loops and +17.9 % emitted text: left out) -- in the default build `caller` answers file `-` and line 0 and `croak` says `msg at - line 0.`; under line-track the tree = perl on every probe row but the eval-frame rows.  Generation **v2-14400**; gate **371 / 12,131** PASS; sweep **18,803** (+5 caller.t, by edit) GATE clean; corpus-diff 5 / 111; wide 39 DIFF all explained (35 `:frame`, 4 `# line`), RCDIFF 0; ir-conform 328 / 0; ir-host-leak 31; **EVERYDAY 115 of 122** (`goto-sub-caller` tests `(caller)[2] > 0`, which passed on a positive Lisp form offset by accident; an explained row).  BOARD: 545 rows out (Scalar-List-Utils exotic_names.t 648 -> 388 failing, IO-Compress 275 rows over 17 files, Log4perl 3, Sub-Uplevel 5, Try-Tiny 2), 19 re-keyed.  Residue in `ns-caller-fidelity`: file / line in the default build (#4790: the FILE can be had with no per-call cost), a frame for the other anonymous subs, eval / require frames, tier 2 (Sub::Uplevel, CORE::GLOBAL::caller, @DB::args).  Filed #4941 #4942.

  ALSO MERGED today, docs only: **s540b** `docs/speed-low-hanging-review-s540.md` (nine levers #4981-#4985 #4988-#4991) and **s540a** `docs/cpan-speed-comparison-s540.md` + `tools/board-timing.pl` (the board under perl and PCL: perl 64.9 s CPU, PCL warm 2,154 s = 33x, and 82 % of it is LOAD -- 1,412 s compiling each distribution's own modules from text on every run; RULED by the USER the same day, DECIDED ## s540: local modules are compiled once, #4986, built by s540d).

  Before it: (s540, 2026-11-13 14:3x, main = MAIN-READY-91 `3246e41a` = s538a merged -- THE CPAN BOARD'S WHOLE-FILE LOSSES, part 1 of the compatibility plan's point 1: slurp mode at end of file answers "" once per handle and then undef, on files, pipes and in-memory handles (#660, perl's IOf_NOLINE kept in the `$.` table), the `_` stat buffer re-reads with the flavour (stat / lstat) that FILLED it (#3554: File::Path `remove_tree` no longer walks into a symlinked directory), `use integer` is a REGION read by position, as strict refs is -- a file-level pragma reaches the sub bodies, a pragma inside one sub no longer leaks into every later sub, and a compound assignment takes the integer op (#3781; `Pl::Parser::integer_regions_of`; main had both silent wrongs: URI's punycode, Math::BigInt::Calc / Time::Piece), `>>=` / `<<=` read a UV as perl does, a hash slice of a Magic-token container (`@_{...}` `@+{...}`) is the HASH's slice (#3615), an opener's handle variable holding a non-handle reference dies "Not a GLOB reference" (#3618), an array index of 2^61 or more dies "Out of memory during array extend" trappably (bop.t's `$#a>>=1` child ran out of heap), `pack 'w'` above UV_MAX is perl's digit-string division, ONE encoder `_pack_ber` for the five sites; generation **v2-13900**; gate **370 / 12,121** PASS; sweep **18,798** +0 GATE clean; corpus-diff vs d1678177 1 / 111 (bop.t); wide 18 DIFF, all among the 34 predicted files, RCDIFF 0; ir-conform 328 / 0; ir-host-leak 31; EVERYDAY 116 of 122; bench fhread / useint / boxarith in the noise; Calc.pm compile unchanged.  BOARD one-file runs: Path-Tiny filesystem.t 90/3 no plan -> 105/3 of 108, rel-abs.t 33/0 whole, URI punycode.t 2/2 -> 15/0, iri.t 13/3 -> 24/6 of 30, Text-CSV 91_csv_cb.t 19 -> 77/2 of 79, 90_csv.t 116/1 -> 128/0; 9 rows out, 5 in, 3 re-keyed.  **FLAGGED, a board cost: the three Path-Tiny `input_output*.t` files no longer die early -- they HANG to the 120 s timeout** (`*FILE*` rc=124, `NS:ns-escaping-filehandle`): Path::Tiny's `slurp` takes a shared lock on a handle a helper sub RETURNED and lets it go; perl closes it there, PCL keeps it open (#2006 part 2), so the next locked `append` waits on the program's own lock -- main hangs on the same plain-Perl reduction, the files only never reached it.  Review probes perl -> main -> tree (5 files): tree = perl on p1 p3 p4, p2 differs in the #4786 message only, p5 in one line (`**` under `use integer` is integer power in perl: #4789 filed, pre-existing); main wrong on all five and out of heap on one.  Filed by the batch #4841-#4846.

  Before it: (s540, 2026-11-13 13:5x, main = MAIN-READY-90 `938cc88d` = s537b merged -- STATE CONTAINERS AND THE CONTAINER SPAN MANGLE: a `state` array / hash / list and a refaliased `\state @a` outside a named-sub statement compile (#314 F-F, #1579 the state half: file level, loop bodies, closures each with their own state, a `do` initialiser run once; `t/op/lvref.t` now stops at the next refusal, slice refaliasing #4823), a file-level `my` of an exception-partition CONTAINER (`my %ENV` / `my @ARGV` / `my %INC`) and a container in a chained / embedded declaration or with a canon declared twice keep their cell across a later `package` section (#4661 #4662 = #338 half b1), a BLOCK-extent promoted scalar with a later string eval takes the per-site capture pair and no global alias, in the single-name AND the multi-name declaration branch -- ONE helper `Parser2::_eval_reach_promoted` (#4461: `{ my ($r, $c); sub ... } print $r` printed the block's value; found by the batch's own predicted-vs-actual scan, 13 of 14 predicted files moved, the 14th = #4824); #4663 measured, open; generation **v2-13800**; gate **370 / 12,084** PASS; sweep **18,798** +0 GATE clean; corpus-diff vs 53dd73da 3 / 111 (closure eval postfixderef = #4461); wide 16 DIFF + 2 RCDIFF all explained (`op/svleak.t` rc 2 -> 0 = #4662: compiles, skips itself without XS::APItest; `perf/optree.t` rc 255 -> 0 = #314 F-F: compiles, aborts at its `use B` import -- both `NS:ns-perl-internals-readouts`, companion rows by edit); gate-SET one line (`op/lvref.t` refusal -> refusal); ir-conform 328 / 0; ir-host-leak 31; drop census 15 / 55; EVERYDAY 116 of 122; Calc.pm compile 4.29 -> 4.11 s.  Review probes perl -> main -> tree (4 files): tree = perl on 3 and on all but the #4663 line of the fourth; main refuses three of them.  NOTE: a file-level `state %k` read inside `"@{[ %k ]}"` moves from a refusal to #4822's SILENT empty (the binder and the interp fixer do not see a bare `%name` in an interpolated code block; the named-sub spelling was silent on main already) -- #4822 goes to the parser slot.  Filed by the batch #4821-#4824.

  Before it: (s540, 2026-11-13 13:1x, main = MAIN-READY-89 `3615c4b5` = s538b merged -- `DESTROY` AT PROGRAM EXIT (#2371, the USER's s538 schedule; `cl/` only): once a program has a non-empty DESTROY or AUTOLOAD (the one-way gate `*p-destroy-live*`, opened where subs are installed), every `bless` notes its referent in ONE weak vector, and the exit hook -- after the END blocks, before the flushes -- calls DESTROY (or the class's AUTOLOAD) on every registered object still alive, newest first, each once, class and method read AT EXIT, `${^GLOBAL_PHASE}` DESTRUCT, `exit` in a destructor ends the phase, a `$?` set there is the exit status; NEVER at scope exit (#2370 stays parked), and an object the collector reclaimed earlier gets none (measured: 9,727 of 100,000 dropped objects; a forced full GC leaves 3 of 11) -- README / STATUS / `ns-destroy-gc` / ir-spec §7.4a say so; perl destroys file lexicals BEFORE the END blocks, PCL everything after; generation **v2-13750** unchanged (code sha `381d9063`); gate **370 / 12,077** PASS; sweep **18,798** (+2 ref.t, by edit) GATE clean; corpus-diff vs 53dd73da IDENTICAL; ir-conform 328 / 0; EVERYDAY 116 of 122; companion op/ref.t 210/32 -> 215/30, op/bless.t 111/6 -> 112/6; Try-Tiny context.t 13 -> 25 ok (still rc 255: Test::More's END counts before the finally blocks run), finally.t 11/4 -> 23/7.  FLAGGED bench (kept, the USER's s536 rule; input to #4621): a program WITH a destructor pays ~22-35 ns per `bless` (`blessnew-d` +7.4-8.5 %, `blessdes` +5.5 %); one with none pays one global read (`blessnew` in the noise); 100,000 live objects are destroyed in 0.052 s (perl 0.017 s).  Review probes perl -> main -> tree (4 files): the tree makes every DESTROY call perl makes, each once, timing only differs; main makes none.  #4864 (keep a guard object alive to its scope's end) RULED unscheduled under #2370.  Filed by the batch #4861-#4866 (#4866 = a pre-existing SILENT WRONG: in a module, `local $h{K} = V` on a file-level `my %h` is not seen by the sub's own reads -- every Try::Tiny `finally` block gets an empty `@_`).

  Before it: (s540, 2026-11-13 12:1x, main = MAIN-READY-88 `a435a391` = s537a merged -- CALL AND SUB-BODY SHAPES: a signature default is the right side of a scalar assignment (#4782: `sub f ($n = @d)` binds the count), a `while` / `until` loop that is the last statement of a sub, a file or a block answers its condition's last value (#4281, `p-until` its own head; the condition of such a loop is a VALUE read for the slot verdicts, `VarAnnotator::_loop_cond_uctx`; residue #4801 a loop left by `last`), an unknown package-qualified bareword with nothing after it is the string, in a builtin handle slot the handle, and a leading `::` names main for every symbolic name (#4762, one splitter `%p-split-symbolic-name`), a feature-gated keyword that is the USER's sub here parses as that sub, `isa` included (#4703 #4802, one answer `_gated_sub_here`), `push` reads a tied argument once (#4664), a whole-statement `eval { }` copies nothing out of its frame (#4325 eval half; the sub-call half #4804, the class operand #4785), `s///` on a read-only target dies before the match and an identical `tr` never writes -- a count-only `tr` on a reference leaves the reference (#4264 #1493); generation **v2-13750**; gate **369 / 12,047** PASS; sweep **18,796** (+1 tr.t, by edit) GATE clean; corpus-diff vs 23f0f917 3 / 111; wide 29 DIFF all explained (24 `p-until`, 3 signature default, 2 `::pass`, loopctl.t), RCDIFF 0; gate-SET 638 identical; ir-conform 328 / 0; ir-host-leak 31; EVERYDAY 116 of 122; Calc.pm compile 5.52 -> 5.51 s.  Review probes perl -> main -> tree (4 files): tree = perl on 3 and on all but the #4785 line of the fourth, main wrong on all.  THE ROUND BOARD of s539 on MAIN-READY-85: **641 PASS / 161 PARTIAL / 27 FAIL / 67 SKIP of 896 files, 109,797 ok / 1,740 not ok**, 0 NEW (twelve load timeouts counted by their one-file re-runs).  Filed by the batch #4801 #4803-#4807.

  Before it: (s538, 2026-11-11 02:2x, main = MAIN-READY-87 `5ca8e976` = s536i merged -- THE ALIASING RESIDUE: a list slice in an aliasing consumer is the selected elements' cells and an index past the list is perl's read-only undef (#4682: `w((@b)[9])` grew @b), a magic-lvalue call (`substr` / `vec` / `pos`) in a flattened foreach / map / grep list is its write-through cell (#4684), `scalar(SCALAR)` in such a list and under every `\` spelling but one is the scalar itself (#4685; residue #4783 the parenthesised `\(scalar($x))`), a dereferenced BLESSED hash spreads its pairs in every list builder and `unshift` spreads a hash at all (#4683), `++` / `--` on an assignment evaluates it once (#4681), a container in a lazy argument slot is an EAGER place -- no raw SBCL gethash death, YAML-PP 32.cyclic-refs.t 8/8 (#4283), an element's SUBSCRIPT is a plain read -- `\$r->[$j{zz}]` / `$n{$m{y}}++` no longer create the key (#4741); generation **v2-13700**; gate **369 / 12,018** PASS; sweep **18,795** +0 GATE clean; corpus-diff vs 3d524ad7 10 / 111; wide 112 DIFF all explained (49 #4283, 42 #4682, 32 #4741, 3 the map / grep scalar box), RCDIFF 0; ir-conform 328 / 0; ir-host-leak 31; EVERYDAY 116 of 122; Calc.pm compile 5.54 -> 5.52 s.  FLAGGED bench: `mapmulti` +2.7 % (reproduced twice: the `p-map-flat` argument rule), `pushloc` +5.1 % (unclear: -1..+3 % on the first run).  Review probes perl -> main -> tree (7 files): tree = perl on 6, main wrong on all.

  Before it: (s538, 2026-11-11 00:5x, main = MAIN-READY-86 `d2e4ba52` = s536j merged -- PCL's PPI batch 8: `pcl` = `c2f21706` (groups AV §62 #4701: `-Foo::bar` / `-CORE::log(...)` is ONE Word, split by PExpr's `_split_negated_call_words` with a qualified name -- core Math::Complex lines 750 + 1081 no longer die; a DECLARED qualified name before `=>` or in a hash subscript is not autoquoted, as perl; AW §63 #4542: `x3` / `x64` after a `}` that ends a statement is a Word, the 19b arm of `Parser2::_repair_word_x_call` + `_starts_statement` DELETED; AX §64 #4482: `__DATA__ =>` / `__END__ =>` are Words, not a data section); #4302 §65 (PPI rewrites CR LF / CR to LF) MEASURED, not fixed = #4761, RULED by the USER s538 (code may be normalised, a string keeps what perl keeps); report 411 rows / 401 ok; expect raw 214 -> 209, pcl 160 -> 155; generation **v2-13500**; gate **369 / 12,009** PASS (the agent's, in the worktree checkout -- a gate on an extraction outside git cannot read PASS until #4763 is fixed, measured in both placements); sweep **18,795** +0 GATE clean; corpus-diff vs e7205dde IDENTICAL; wide 1 DIFF (Math/Complex.pm = #4701), RCDIFF 0; gate-SET 638 identical; ir-conform 328 / 0; ir-host-leak 31; install-pcl 61; EVERYDAY 116 of 122.  Review probes perl -> main -> tree: tree = perl on #4701 #4482 and the #4542 statement-start shapes, no regression; filed #4784 (`x<N>` in a list-operator argument slot: `print x64()` drops, `print STDOUT x3()` prints NOTHING -- PPI, pre-existing) and #4783 (from the s536i pre-review).  The fork `Percolisp/PPI` `pcl` IS pushed again (USER s538: at the reviewing session's discretion).

  Before it: (s537, 2026-11-10 00:5x, main = MAIN-READY-85 `bdd36d06` = s536h merged -- I/O STATUS FACTS + FORCED-MAIN NAMES, `cl/` + one non-emitting accessor: a 2-arg `-` open is a new handle on the standard stream (its close leaves STDOUT open; the 3-arg `-` is a file) and an empty file name fails ENOENT in every mode (#4263 #4262), `wait` / `waitpid` store `$? = -1` when nothing was reaped -- a WNOHANG poll stored garbage (#4224), a read that reaches end of file clears `$!` once per EOF, `read` / `sysread` / `syswrite` / `truncate` clear it first, `getc` at EOF is EBADF, `do FILE` that finds its file clears it (#4284; `*p-eof-seen*` a plain table dropped at close), a symbolic glob of a forced-main name is main's from every package -- ONE runtime list `+p-forced-main-names+` asserted equal to the one in Pl::Environment (#4310); #4286 MEASURED, open; generation **v2-13300** unchanged; gate **369 / 11,998** PASS; sweep **18,795** +0 GATE clean; corpus-diff vs e7205dde IDENTICAL; wide 2,053 SAME; ir-conform 328 / 0; fhread bench in the noise; EVERYDAY 116 of 122.  Review probes perl -> main -> tree (5 files): tree = perl on all, main wrong on all.  Filed by the batch #4721 #4722.

  Before it: (s537, 2026-11-10 00:3x, main = MAIN-READY-84 `9c425a19` = s536e merged -- FILE LEXICALS ACROSS PACKAGE SECTIONS: a file-level `my $a` / `my $b` (an exception-partition name) keeps its cell across a later `package` section (#4547 = #2401 #1027 #1964; each.t rows 64 65), a file-level `state` cell is a global of the DECLARING package (#4541), a container inside a LIST `my` spans a section (#4543; #338 half b1 stays open = #4662), a default inside a one-token signature reads the file lexical (#4545), the file-level embedded-`my` veto asks the BINDER -- another sigil or a sub defined before the declaration no longer vetoes (#4641), an embedded `my` its own statement mentions again reads the OUTER variable (#4642); the rename rewrites walk each statement ONCE; generation **v2-13300**; gate **369 / 11,990** PASS; sweep **18,795** (+2 each.t, by edit) GATE clean; corpus-diff vs 3139fcb0 3 / 111 (each.t magic.t my.t); wide 11 DIFF predicted, RCDIFF 0; ir-conform 328 / 0; ir-host-leak 31; EVERYDAY 116 of 122; compile time Calc.pm -1.4 %.  Review probes perl -> main -> tree (10 files): tree = perl on 8, main wrong on 5 of them; the two misses pre-existing: #314 F-F (file-level `state @arr` refuses the file) and #4782 FILED (`sub f ($n = @d)` binds the array, not its count).  Filed by the batch #4661-#4664.

  Before it: (s536, 2026-11-08 20:2x, main = MAIN-READY-83 `f5ecb8fc` = s536g merged -- CALL-SITE HEADS: `-NAME` of a CALLABLE name (declared above, imported, builtin, or `(` follows) is `-` applied to a call of NAME -- PExpr splits PPI's one Word into perl's two tokens (`_split_negated_call_words`; `_negated_call_form` DELETED; position-aware: `-K` above `sub K` stays the string; t/comp/parser.t row 45 + a silent `-time` in t/re/speed.t; #4222 #4602), Cwd's `cwd` / `getcwd` leave the core arity table and `Cwd` leaves the module-walk skip list (#4601, rule 9a), a FEATURE-GATED keyword (`say fc isa evalbytes`, measured from feature.pm) is one only where its feature is in force -- ONE answer `Pl::Parser::gated_keyword_at`, three-valued: feature on = builtin, `no feature` = the sub, unsaid = builtin unless a sub of the name is known (Search::Dict's BEGIN-time `feature->import`; #4603, op/lc.t row 7), a postfix `->` after a dynamic-name method call with arguments continues the chain (#4481, was a DROP), `*x{SCALAR}` always answers a SCALAR reference (#4604; YAML-PP 54.glob.t 3/3); generation **v2-13100**; gate **369 / 11,975** PASS; sweep **18,793** +0 GATE clean; corpus-diff vs 0ac58419 2 / 111 (array.t negate.t); wide 55 DIFF (47 detector rows + 8 explained), RCDIFF 0; ir-conform 328 / 0; ir-host-leak 31; EVERYDAY 116 of 122.  Review probes perl -> main -> tree: tree = perl on 37 lines.  ROUND ATTRIBUTION (reviewing session): comp/parser.t rows 89-92 (stub decls "ignored after compilation error") fail since s536b #4561 (MAIN-READY-79) -- invalid-Perl rejection, keyed #1695, snapshot 47/51.  Filed #4701-#4703.

  Before it: (s536, 2026-11-08 19:1x, main = MAIN-READY-82 `b28b8799` = s536f merged -- ALIASING AND LIST SHAPES: a list slice indexes the FLAT list (`((1,2),3)[1]` is 2, `(%h)[0]` a key; #4341), a foreach list of several `substr` / `vec` / `pos` calls writes each window (#4581), the value of an assignment to a BOX place is the box -- `f($t{k} = V)` aliases a tied element, `($t{k} = V) =~ s///` no longer dies read-only, and reading a tied scalar after `|=` FETCHes again as perl does (#4644; bop.t +9), `scalar(EXPR)` of a scalar / element / `$$r` in a user-sub, method or coderef argument (and a `$`-prototype slot) passes the operand itself (#4323; p-scalar sites 52,979 -> 52,658 over the A/B population), `(*x, *y) = LIST` assigns each glob through `p-glob-assign` and any other glob reaching the box store DIES (#4442, rule 12), `&CORE::push(\@a, ...)` and the other `\@` / `\[%@]` builtins take a REFERENCE, else perl's "Type of arg 1" die (#4383, `p-core-amp-aggr`); generation **v2-13000**; gate **369 / 11,963** PASS; sweep **18,793** (+9 bop.t, by edit) GATE clean; corpus-diff vs ba7863a3 1 / 111 (pack.t); wide 76 DIFF = #4323 unwraps, RCDIFF 0; ir-conform 328 / 0; ir-host-leak 31; EVERYDAY 116 of 122; compile time Calc.pm -2.6 %; bench 15 rows unchanged.  Review probes perl -> main -> tree: tree = perl on 35 lines, main wrong on 11 and died twice.  Filed #4681-#4685.

  Before it: (s536, 2026-11-08 17:4x, main = MAIN-READY-81 `ce06e1f6` = s532c-4623 merged -- the round board's REGRESSION #4623 FIXED: #1423's own-package-first glob read no longer applies to main's standard handles (STDIN STDOUT STDERR ARGV ARGVOUT, ONE predicate `%p-main-std-handle-p` shared with `%p-local-glob-std-p`) -- a glob assignment `*STDIN = *STDIN` / Capture::Tiny's `local *STDIN = *CT_ORIG_STDIN` left a copy under main's symbol that `local *STDIN` neither saves nor restores, so `fileno(\*STDIN)` answered 0 inside a later `local *STDIN`; Capture-Tiny 15 / 22 / 23 / 24 run whole again (873/0, 583/0, 875/2 #3434, 587/0 = the 1191fa1a counts); `cl/` only, generation **v2-12700** unchanged (code sha `ba7863a3`); gate **369 / 11,957** PASS; sweep **18,784** +0 GATE clean; corpus-diff vs c8886b61 IDENTICAL; ir-conform 328 / 0; EVERYDAY 116 of 122.  Review probes perl -> main -> tree: tree = perl on 10 lines, main wrong on 3.  Filed #4310 (`*{"STDIN"}` inside a package builds that package's glob).

  Before it: (s536, 2026-11-08 17:0x, main = MAIN-READY-80 `89365fbd` = s536d merged -- a `my` or an assignment in an EXPRESSION position: the chained `my $c = my $d = EXPR` let-binds `$d` fresh per execution (#4544; recursion / closures shared one cell), an embedded `my $x` named like an enclosing lexical DECLARES a new variable (#3493 / #4625: IO-Compress `LexFile->new(my $in)` croaked "NO!!!!"; residue #4642), an element assignment passed as an argument hands @_ the element itself (#4622; tied residue #4644), and no reader loses data on a malformed UTF-8 byte (#4624 no-loss half: `:encoding` gives perl's \xHH text; lax `:utf8` keeps each bad byte as a character -- the malformed-string representation stays #1389's); generation **v2-12700**; gate **369 / 11,956** PASS; sweep **18,784** +0 GATE clean; corpus-diff vs 581e5eab 4 / 111; wide 48 DIFF explained, RCDIFF 0; ir-conform 328 / 0; ir-host-leak 31; EVERYDAY 116 of 122.  IO-Compress 105oneshot-* now flip on a HEAP exhaustion compiling oneshot.pl's huge `sub run` (#4643, main too); 132 newly reached rows blessed (NS:ns-error-location 124 + #330 8).  Review probes perl -> main -> tree: tree = perl on 8 lines.  Filed #4641-#4644.

  Before it: (s536, 2026-11-08 16:3x, main = MAIN-READY-79 `352e26e0` = s536b merged -- the dereference overloads `@{}` / `%{}` apply on every array / hash dereference of an object through ONE resolver `%p-deref-overload` (#4305: Mojo-DOM58 dom.t 8 rows -> 126/126; FLAG: once ANY class registers a deref overload, every unblessed deref pays the lookup -- refderefov +93 %, input to #4621), `use constant` after `package R;` in a block defines R's constant + per-statement parse positions inside a BEGIN body (#4562; a BEGIN-declared prototype was a silent wrong), a bodyless `sub NAME;` is a stash entry visible from its own position only (#4561 + #4583; the string-eval half of #4583 stays OPEN -- Role-Tiny stub.t 4), `for (substr($h{k}, ...))` writes the element (#4309), `use parent` in a do / eval BLOCK (#4521), a piped open whose program cannot be executed fails in the parent with ENOENT (#4306; magic.t row 67); generation **v2-12600**; gate **369 / 11,950** PASS; sweep **18,784** (+1, by edit) GATE clean; corpus-diff vs 84a2e6ec 101 / 111 (774 wide files = detector line numbers from the BEGIN parse positions); wide 838 DIFF all explained, RCDIFF 0; ir-conform 328 / 0; ir-host-leak 31; EVERYDAY 116 of 122.  Board: dom.t, Role-Tiny concrete-methods 7/7, role-tiny-with 6/6 out; stub.t 3/4.  Review probes perl -> main -> tree: tree = perl on 8 lines.  Filed #4581 #4582 #4583.

  Before it: (s536, 2026-11-08 16:0x, main = MAIN-READY-78 `02995071` = s536c merged -- #1611 FIXED: a call the PROGRAM wrote reaches `p-NAME` only if NAME is in the runtime table AND a perl keyword (ONE predicate `Pl::ExprToCL::source_call_is_builtin`, read by `cl_name` and Parser2 `_rewrite_bareword_stmt`; `_name_is_declared_sub` + `is_runtime_name` DELETED); a sub named `hash` / `exception` / `box` / `cwd` / `reftype` ... -- declared, imported or installed at run time through a computed glob (Test::Deep) -- is called; the compiler's own heads (`__pcl_set_prototype` / `__pcl_set_attributes`) carry the `_core_qualified` marker; PCL's own Perl spells `CORE::NAME` (the `:prototype` desugar, lib/Scalar/Util.pm lib/Sub/Util.pm lib/attributes.pm lib/File/Spec.pm); measured first (1,369 events over four populations: only two compiler-built heads); predicted.tsv (485 head sites / 451 files) == the wide A/B (1,270 DIFF, RCDIFF 0) token by token; generation **v2-12300**; gate **369 / 11,942** PASS; sweep **18,783** +0 GATE clean; ir-conform 328 / 0; ir-host-leak 31; EVERYDAY 116 of 122; YAML-PP 37.schema-perl.t 3 -> 14 rows, 54.glob.t subtest `valid` whole.  Review probes perl -> main -> tree: tree = perl on 8 lines; main died on a run-time-installed `box`.  Filed #4601-#4604.

  Before it: (s536, 2026-11-08 15:0x, main = MAIN-READY-77 `fcfce8a5` = s535a merged -- the PARSER PROGRAM's step 1c, PORT 3: the SPAN PASS asks the BINDER (`_check_my_spanning` / `_rename_spanning_lexicals` on `Binder::spans` / `refers_to_decl`); the last dual-run hook `_ref_shadowed` and its private readers DELETED (`_ref_shadowed_0`, `_canon_refs_in`, `_lex_referenced_after`, `_stmt_declares_canon` (rule 23: s532c's #4304 guards pass), `_signature_*`, `_stmt_binding`, `%spanning`; Binder `check` / `PCL_BINDER_INJECT`; Parser2 15,779 -> 15,313 lines); fixed by construction #3803 #4425 #4546 (core Math::Complex `root()` clobbered `i` -- silent wrong), the package-region residues #4421 #4440 #4422 #475 in the requalifier, and the sweep's own find: an `open my $handel` inside an expression is no longer cut off at a `package NAME { }` split (scalar.t 76-78, eval.t 40); generation **v2-12000**; gate **369 / 11,932** PASS (span-binder-01.t +22); sweep **18,783** (+4, by edit) GATE clean; corpus-diff vs c950ad43 4 / 111 (eval local scalar substr); D0 predicted == actual both ways (wide 9 DIFF + board/lib 8); gate-SET 638 identical; ir-conform 328 / 0; ir-host-leak 31; compile time -1..-5 %; EVERYDAY 116 of 122.  Review probes perl -> main -> tree: tree = perl on 8 lines, main wrong on `for our` after an in-sub package (#4440), a file `our` across a top-level package (#475), the handle across a section split.  Filed #4541-#4547.

  Before it: (s536, 2026-11-08 13:3x, main = MAIN-READY-76 `4651f35a` = s536a merged -- the handed-back board hunts, `cl/pcl-runtime.lisp` only (no generation change, v2-11900): `use overload` writes the `(`-stash entries perl keeps and a glob copy of one registers the handler (#135: Role-Tiny overload.t 2/13 -> 14/1), `*y = $r` through a variable ALIASES (a silent wrong found on the way), `warn` returns true (#4181 row 229), a tied scalar copied into an array holds its value fetched once and `unshift` / `splice` fetch once (#1814 copy half, #2601; Text-CSV 76_magic.t 12), the rest re-keyed (#4104 -> #1611, #4182 -> #1494, 70_rt.t 464 -> #1389, Role-Tiny stubs -> #4561, concrete-methods.t -> #4562); gate **368 / 11,910** PASS; sweep **18,779** +0 GATE clean; corpus-diff vs c950ad43 IDENTICAL; lib emission-ab 57 SAME; ir-conform 328 / 0; EVERYDAY 116 of 122 (load 2.0).  Review probes perl -> main -> tree: tree = perl on 5 lines, main wrong on all 5.  Board 14 rows out / 11 re-keyed.  Filed #4561 #4562 #4563.

  Before it: (s536, 2026-11-08 12:0x, main = MAIN-READY-75 `33334fb1` = s532d merged -- a FIRST bless of an ARRAY / CODE / qr referent is recorded on the referent, so a copy taken before it sees the class (#4061; bless.t 105 + qr.t "object is blessed" pass; FLAGGED for the USER: `arrobj-new` 1.9x and `ref []` after any array bless 2.3x slower -- SBCL weak tables are synchronized, ~220 ns per write), the module cache compares the source SIZE beside the mtime (#4165) + `utime(@t, $f)` spreads its times from an array (#4321), `defined` / the REF test / `scalar()` no longer materialise a string being built by appends (#3830: strbuf rows 0.62 s -> 0.004 s; the 16 MB zipbomb probe 39 s heap death -> 1.7 s), a cached prototype record reads back as the walked one and `use M ()` imports nothing (#3961: CPAN/FirstTime.pm reproducible, 163 files x 8 transpiles identical), a served pl2cl request carries the caller's -I (#4041, closes #3180); generation **v2-11900**; gate **368 / 11,903** PASS; sweep **18,779** (+1 net, by edit) GATE clean; corpus-diff vs 0affbe86 1 / 111 (hash.t = the base's own cold/warm flip); wide A/B 37 DIFF = #3961, RCDIFF 0; ir-conform 328 / 0; EVERYDAY 116 of 122.  Review probes perl -> main -> tree: tree = perl; main wrong on #4061 and #4321.  Filed #4322-#4325.

  Before it: (s536, 2026-11-08 10:3x, main = MAIN-READY-74 `fcfcb5d3` = s532c merged -- the CPAN board hunts: `defined *{EXPR}` asks whether the glob EXISTS and creates nothing (#4301, `p-glob-defined`; it answered true for every name -- Template-Toolkit proc.t), an element assignment is an lvalue for `=~ s///` and `OP=` (#4303: `($h{k} = $2) =~ s///` died "Modification of a read-only value" -- YAML-Tiny 30/31 run), a `my` in a `return` / `next` / `last` modifier condition is a block lexical (#4304, Mojo-DOM58 dom.t), `$AUTOLOAD` is set in the AUTOLOAD sub's defining package (#1486), a glob designator resolves in its own package first (#1423 read half), a 2-arg pipe open with no command fails with EPIPE before any fork (#4502: the child ran the rest of the program -- io/pipe.t's 10 `*extra*` rows), and #4308 found by the batch's own sweep: a PAREN-LESS `substr` / `vec` / `pos` as the sole foreach element is ALIASED (`for (substr $v, 0, 1) { $_ = ... }` wrote a copy; magic.t's 22 caret-glob rows had passed only because `defined *$v` lied); generation **v2-11800**; gate **368 / 11,888** PASS; sweep **18,778** (+4: readline.t 2, method.t 1, pos.t 1, by edit) GATE clean; corpus-diff vs 06a4b8ba 3 / 111 (magic.t readline.t pos.t); wide A/B 2,053 files 34 DIFF all explained, RCDIFF 0; ir-conform 328 / 0; ir-host-leak 31 = 31; EVERYDAY 116 of 122.  Review probes perl -> main -> tree: tree = perl on all; main wrong on `defined *{"nosuch"}`, dies on #4303, forks on #4502.  Filed #4305 (the `@{}` / `%{}` deref overloads -- Mojo-DOM58 dom.t's *PLAN*), #4306, #4307, #4309.

  Before it: MAIN-READY-73 `480d41de` = s534a merged (docs + four bench rows: the method-lookup evaluation, `docs/method-lookup-eval-s534.md`).

  Before it: (s535, 2026-11-08 02:0x, main = MAIN-READY-72 `1191fa1a` = s533c merged -- the PARSER PROGRAM's step 1d / C2 kind 1: the `local` statement OFF THE v1 TEXT SEAM -- every family (`local $x` / LIST / elements / slices / `*NAME` / `*{EXPR}` / derefs / `delete local` / `local $#a`) lowered as structured trees by `Pl::Stmt::local_shape` sub-kind; v1's local machinery DELETED (Parser.pm 13,107 -> 11,751; a 15-line bridge for blocks v1 still compiles, BEGIN/END bodies); seam statements 1,338 -> 956 on the 234-file corpus (`local` 370 -> 0; `my (...) = delete local` stays on the seam BY NAME); #4185 (glob targets in a local list), #4441 (an array element init through box-set), #4443 (a block ending in a `local` takes its value) fixed, #4442 #4444 filed; generation **v2-11700**; gate **364 / 11,878** PASS; sweep 18,774 +0; corpus-diff vs 95a1de03 59 / 111 (58 layout + local.t #4443); wide A/B 2,053 files, 12 non-layout all explained, RCDIFF 0; ir-host-leak 31 = 31; compile time local.t -17.9 %, everyday -9.4 %, Calc.pm unchanged; EVERYDAY 116 of 122.  Review probes perl -> main -> tree (22 local shapes): tree = perl, main wrong on the tail value and `local (*A, *B)`.

  Before it: (s535, 2026-11-08 01:3x) main = MAIN-READY-71 `4748a7f5` = s532b merged -- the REQUIRE family + TAP: a required file that DIES leaves `$INC{NAME}` undef and a second require dies "Attempt to reload" (#4162), a found file clears `$!` (#4166), module_true read at a top-level `return` (#4168), `(require NAME)` inside a list is the require term (#4364, the statement was DROPPED), a sub named like a forced-main identifier (`INC` `ENV` `SIG` `STDIN` ...) is main::'s from every package (#4163, `Pl::Environment::forced_main_name`), `POSIX->import("locale_h")` (#4169), `Test::Builder->exported_to / ->maybe_regex / ->cmp_ok` (#4203 #4186: Test::Pod loads), a later `use Test::More` re-wins names a user import shadowed (#4363); the board `--diff` gains UNVERIFIED + MISMATCH `*PLAN*` buckets (#4382) and `--serial=REGEX` (#4381: TT `^compile` 0 NEW vs the parallel order race 7 NEW); generation **v2-11400**; gate **363 / 11,860** PASS (the agent's, on the identical code tree -- the rebases were records-only); sweep 18,774 +0; corpus-diff vs ce665621 IDENTICAL; lib emission-ab 57 SAME; module-mode A/B 389 SAME + Getopt/Long.pm (#3961); ir-conform 328 / 0; companion comp/require.t 1730/17 -> 1741/6, op/require_errors.t 11/62 -> 12/61; EVERYDAY 116 of 122.  Review probes perl -> main -> tree: the reload / %INC / $! / list-require / forced-main / locale_h shapes tree = perl, main wrong on all.  FOUND by the round bisect (s535): **#4502** (a pipe open with an EMPTY command forks and the child keeps running the program -- io/pipe.t's 10 NEW `*extra*` rows on main; pre-existing at -65, widened by s532a #4164; fix handed to s532c).

  Before it: (s534, 2026-11-06 01:0x) main = MAIN-READY-70 `458c4785` = s533d merged -- the PARSER PROGRAM's step 1c, port 2: the CAPTURE GATE, the PROMOTER and the nested-sub hoist ask the BINDER (`Pl::Binder::captures_name`; rule 12: no range / no binder / no record DIES), with the string-eval rule: a literal text naming lexical L counts as a use of L iff a string eval lies in L's VISIBLE RANGE (keyed on the binding, not the CV -- "text in one sub, eval in another" keeps its cell); `_enclosing_lex_decl` asks `Binder::enclosing_decl`; the legacy chain DELETED (`_block_captures_name_0` 157 lines + four helpers; Parser2 -274, Binder +181); #3802 #3806 #4462 fixed by construction (#4462 was a SILENT WRONG: `${x}[...]` / `$d[\/]` in a sub's regex read as an element of @x -- core Pod::Simple::RTF interpolated an EMPTY pattern); #4461 filed (pre-existing silent wrong); generation **v2-11200**; gate **362 / 11,847** PASS (the agent's, on the combined tree = MAIN-READY-69 + this batch); corpus-diff vs 59317afd IDENTICAL (111 + 6 shapes, drops 5); wide emission-ab 3 + 6 DIFF = the predicted set, RCDIFF 0; gate-SET 638 identical; sweep 18,774 +0 (on 0f2db817 + the batch; s532e changed no name resolution); ir-conform 328 / 0 / 17 / 0; EVERYDAY 116 of 122; compile cost Calc.pm -4.6 %, everyday -0.8 %.  Review probes perl -> main -> tree: #4462 (`s/(${id_re}[\$\@:]\S+)/` and `m{^$d[\/]}` in a sub) tree = perl, main wrong; controls (a signature param vs a later `my @x`, eval text in one sub run by another, closures in a loop, `ok(my $c = ...)` in a sub) identical on both.

  Before it: MAIN-READY-69 `b3d5df1f` = s532e merged -- PCL's PPI batch 7: `pcl` = `86623347` (groups AR §48 #3740: the `[` after the `)` of a list / condition / `qw()` is a Subscript, read by ONE PExpr marker pass `_mark_list_slice_runs`, the five Constructor readers DELETED, #335 fixed; AS §60 #3983 an attribute after `my sub NAME`; AT §61 #4102 `${m{q}}` / `${y[1]}`; AU §40 #3745 `PPI::Token::Format` holds a format body, `_strip_format_blocks` DELETED -- text that looks like a format inside a here-doc / string / regex code block is no longer blanked); report 372 rows / 364 ok; expect raw 680 -> 214, pcl 626 -> 160; generation **v2-11000**; gate **361 / 11,832** PASS (the agent's, on the identical code tree -- the resume's rebase was records-only); sweep 18,774 +0; corpus-diff 110 same + ref.t (a detector row); wide 7 explained / RCDIFF 0; gate-SET identical; EVERYDAY 116 of 122; install-pcl 61, container 17/17 (reviewing session).  Review probes perl -> main -> tree: list slices (`(sort ...)[0,1]`, `(split ...)[1]`, `EXPR if (1,0)[1] == 0`, `qw()[1,3]`, `($f->())[2]`), `my sub y :prototype()`, `${m{q}} ${y[1]}`, #335, a here-doc holding `format Y =` -- tree = perl, main wrong on all; `write` still prints nothing = not-supported "format / write".  The fork `Percolisp/PPI` `pcl` is NOT pushed (PR #459's head -- the USER's word).

  Before it: MAIN-READY-68 `3730f293` = s533a merged -- the PARSER PROGRAM's step 1d / C1: THE STATEMENT CLASSIFIER `Pl/Stmt.pm` (`classify` -> kind expr / decl / compound / sub / use / return / local / loopctl / package / scheduled / label / end / refused, + `decl_shape` / `compound_shape` / `break_shape` / `expr_shape`; PURE; an unknown statement class DIES); `_lower_block_1` 1,139 -> 49 lines dispatching on `%BLOCK_ARM`, `_lower_stmt` 303 -> 37, `_lower_compound` 647 -> 70, shape tests in the three 0 (from 28/28/27, 17/19/25, 26/12/7); the dual-run `PCL_STMT_CHECK` ran 2,047 files / 521,444 checks / 0 disagreements and was DELETED; emission IDENTICAL vs 75e513e0 (corpus-diff re-run by the reviewing session: 111 + 6 shapes identical, drops 5; wide emission-ab 2,047 SAME / RCDIFF 0; stderr identical); no generation change (v2-10900); gate **361 / 11,819** PASS (guard `Pl/t/stmt-classify-01.t` 74 pure rows); EVERYDAY 116 of 122; compile time unchanged (Calc.pm 4.82 -> 4.69 s).  Left: `_lower_foreach_stmt` 249 + `_lower_decl_list` 240 lines (C7).  Next in the parser slot: C2 (close the v1 text seam one statement kind per session, `local` first) interleaved with the 1c ports.  RUNNING: s532e (PPI batch 7).

  Before it: MAIN-READY-67 `7a4cbf67` = s533b merged -- the PARSER PROGRAM's step 1c, port 1: the block-package REQUALIFIER asks the BINDER (`Pl::Binder::requalify_home`; the binder is built in PRODUCTION for every document, `PCL_BINDER_CHECK` now arms only the two remaining dual-run hooks); the private resolver `_binding_at` + `_binding_at_0` + `_head_binding` + `_decl_binding_in` + `_q_binding_at` DELETED (Parser2 -120 lines); four SILENT WRONGS fixed by construction: #3800 (a `my` in an EARLIER sibling anon sub), #3801 in a sub body (`my $S = my $F = 0`; the file-level bare-block half = #4425, port 3), #3804 (`tie my($x), CLASS` after an in-sub `package`), #4423 (a C-style `for (my $i ...)`'s condition and step homed to the package global -- core Encode.pm:184/196) + #4424; predicted sites (the requalifier's FINAL home compared, not the s525q hook's answer) = actual diffs in both directions (corpus-diff = local.t; wide = local.t universal.t write.t Encode.pm; coreamp.t + Getopt/Long.pm = #3961's base nondeterminism, not this batch); generation **v2-10900**; gate **360 / 11,745** PASS (the agent's, on the tree rebased onto 1aa5d0e5 = main's code); sweep 18,774 +0 GATE clean; companion one-file local.t write.t universal.t taint.t identical base = tree; EVERYDAY 116 of 122; compile cost Calc.pm +2.7 %, pack.t +2.3 %, everyday -1.1 % (the binder's own cost).  Review probes perl -> main -> tree: A7 B4 F1 CF tree = perl, main wrong; controls N1 N4 N6 + two review files identical; filed **#4440** (pre-existing: `for our $o (...)` inside a sub after an in-sub `package` reads an EMPTY loop variable).  Filed #4421 #4422 #4425 (s533b).  RUNNING: s532e (PPI batch 7), s533a (C1 the statement classifier).

  Before it: MAIN-READY-66 `70ff3112` = s532a merged -- the SILENT WRONGS filed 10-29..11-01 with the most board rows: #4184 (indirect object `new Foo, LIST` -- a comma, `=>` or `->` right after `new CLASS` ends it), #4086 + #3862 (an ELEMENT in an aliasing slot -- `for ($_[3])`, `\($a[0])`, a mixed foreach list -- is a one-slot slice through the ONE aliasing rewrite; 11 Template-Toolkit rows out; Pod-Simple tiedfh.t's 6 rows re-caused to the documented copy-back residue), #4084 (a map block's reference value gets a fresh container; TT chomp.t 18 + block.t 2 rows out), #4085 (a bareword handle after a run-time `package X;` is spelled for the reader's package -- `Environment::reader_package`; TT evalperl.t 23/0), #4164 (2-arg open trims the white space), #4202 (`&unless(5)` calls the sub), #3982 (`"$#{+}"` interpolates), #4187 (half 2: runpcl-streams.t row 14 compares against perl's own answer), **#4362 FIXED** (the s532 review's regression: `&CORE::NAME(LIST)` is the builtin call again, never `p-amp-spread`; `amp_ignores_prototype` decides; coreamp.t back to d47caabe's shape 14/53 -- the file stops at row 67 on `&CORE::exit((1)x2)`, #1577's arity family; its companion rows re-blessed for that file after reading, causes by hand); generation **v2-10300**; gate **359 / 11,733** PASS (the agent's, on the tree rebased over MAIN-READY-65 = the combined tree); sweep 18,774 +0; corpus-diff vs d3231423 = aassign.t local.t tr.t (#4086, explained); lib emission-ab 57 SAME; EVERYDAY 116 of 122 (the battery caught a list-slice regression the gate and sweep missed -- fixed with a control row); 40 board rows out, 8 re-caused.  Filed #4261-#4265 (s532a), #4383 (`&CORE::push(\@a, …)` takes a REFERENCE, pre-existing).

  Before it: MAIN-READY-65 `157f7255` = s531d merged -- the CPAN board's whole-file failures part 2 (the one-file causes): #4205 (an anon array constructor flattens nested lists fully -- `[ $p ? ((7) x $n) : () ]`), #3556 (`system` / `exec` / pipe-open LIST run an object argument's `""` overload), #3822 (the runtime spawns ITS perl, the first on the PATH at process start, never the program's `$ENV{PATH}`), #3553 (HARNESS: `is_deeply` unoverloads `""` objects as Test::More does), #3551 + #3827 shape 1 (`${ REF-VALUE }` of a scalar holding a ref answers the ref box); **#4361 FIXED** (the s532 review's regression: a `local *FH` scope-exit close is IMPLICIT -- the child is reaped, `$?` and `$!` untouched; io/pipe.t 22/11 -> 23/10); THE ROUND COMPANION ATTRIBUTED (every NEW / FIXED row of the MAIN-READY-63 `--all --quick` run keyed to its merge by one-file bisects: comp/parser.t "-F calls the F function" = #4222 exposed at -50, coreamp.t = #4362 at -57 (4 real regressions among the 16 NEW; 12 were never produced before), filter_exception.t = #1815's exposure (NS, #1687 folded), the FIXED rows = s527b / s527c / s527e / s530c / s531a; `perl-suite-fails.tsv` 41 out / 19 in / 3 re-keyed, `perl-suite-run.tsv` 13 rows, `row-shortfall.tsv` 4, the drop census 1 -- all BY EDIT, a 14-file acceptance run 0 NEW / 0 FIXED / 0 LOST); runtime + harness only, NO generation change (v2-10200); gate **357 / 11,722** PASS (the agent's, on the combined tree); sweep 18,774 +0; corpus-diff IDENTICAL; EVERYDAY 116 of 122; Path-Tiny locking.t + mutable_tree_while_iterating.t, URI cwd.t, TT table.t now PASS.  Filed #4222 #4224.

  Before it: MAIN-READY-64 `c88e5b1e` = s531e merged -- PCL's PPI batch 6: `pcl` = `a376a6d8` (AO §58 #4201: the parens after a method named `if` / `while` / ... are its argument list -- 153 Template-Toolkit rows left the baseline; AP §59 #3962: a `{` FIRST inside a map / grep / sort or deref block is an anonymous hash, a silent wrong in six core-lib files too, `_deref_inner_brace` deleted; AQ §42 #3749: a here-doc inside an `s///e` replacement or a `(?{ })` block takes the lines after it, both PPI halves + a 6-line `Pl/ExprToCL.pm` change); report **311 ok of 323**; `tools/t/ppi-expect-check.t` registry-driven, 108 rows (#3980 = #4187 half 1); the checker reads Attribute tokens and lexical subs (#3946 #4141: raw 715 -> 680, pcl 661 -> 626, no kind rose); generation **v2-10200**; gate **354 / 11,711** PASS (the agent's, on the identical code tree -- two records-only rebases); sweep 18,774 +0; corpus-diff vs 10cdc985 IDENTICAL, wide emission-ab 8 differ / RCDIFF 0 (§59's six core-lib copies + §42's parser.t); gate-SET 638 identical; EVERYDAY 116 of 122; install-pcl 61, container 17/17 (Fable).  THE ROUND BOARD on MAIN-READY-63 (s532, Fable): **596 PASS / 193 PARTIAL / 40 FAIL / 67 SKIP of 896 files, 105,501 ok / 1,867 not ok** (was 558 / 205 / 66 / 67); `--diff` 11 NEW / 18 FIXED, of which only Role-Tiny load-module.t 5-6 (#1688) and the zipbomb *FILE* -> *PLAN* were real: 7 NEW = the scoreboard's file-ORDER race (TT compile2/5 read compile1/4's cache; **#4381**), 3 NEW = load TIMEOUTs (now 300 s allowances), 16 FIXED = rows the files never REACHED (**#4382**: the board diff has no UNVERIFIED bucket).  THE FABLE MERGE REVIEW of d47caabe..b31345ca (the USER's ask; `briefs-and-rules-for-claude-subagents/s532/merge-review-s532.md`): 13 batches OK, two REGRESSIONS -- **#4361** (s529a: an implicit `local *FH` scope-exit close of a pipe writes `$?`; io/pipe.t's NEW row; fix in s531d) and **#4362** (s530c: `&CORE::NAME(LIST)` spreads a MACRO head -> raw SBCL error; coreamp.t's 16 NEW rows; fix in s532a); #4363 #4364 LOW; #1687 folds into #1815.

  Before it: MAIN-READY-63 `cc5f6310` = s531a merged -- the CPAN board's whole-file failures part 1: #3650 (a pack/unpack template beginning with `U` is character mode -- all 16 JSON-PP 099_binary files 1536/1536, behind #3282), #1815 (`@INC` hooks: code ref / array ref / object), #3831 + #3696 (a stash delete MARKS a sub: compiled calls keep it, a literal `defined &NAME` loaded after the delete answers false; residue #4170), #1688 (`require` answers the file's value; a false one dies "did not return a true value"), #1705 (`%Config` lists the shipped extensions), #3821 (all-caps class names inherit AUTOLOAD and `""`); generation **v2-10100**; gate **354 / 11,704** PASS (the agent's, on the identical code tree); sweep TOTAL **18,774** (+15 pack.t, -2 length.t by hand); comp/require.t 1730 / 17, op/inccode.t 79 / 10; corpus-diff IDENTICAL; EVERYDAY 116 of 122.  OWED: the `--all --quick` companion for its `cl/skip-registry.lisp` edit (length.t rows) -- it ran the one file only.

  Before it: MAIN-READY-62 `25813c08` = s531b merged -- THE CAUSE COLUMN HARDENED (the Fable 5.1 review of the register plan ruled REWORK SMALLER): `PCLCauses::parse_cause` leading-marker grammar, 128 stable `ns-` ids in `docs/not-supported.md` (`tools/ns-ids.pl`), the validator `Pl/t/verdict-register-01.t` (GATE on the board, REPORT on the companion / sweep / shortfall), a cached perl ORACLE for the board (`run-dist-t.pl --oracle`), the SKIP file verdict, `tools/migrate-causes.pl`; board file verdicts are PASS / PARTIAL / FAIL / SKIP (SKIP = perl skips the file too) and not-ok rows split ours / ruled / perl's / harness: on code ade297e7 PASS 558 / PARTIAL 205 / FAIL 133 -> 66 / SKIP 67, not-ok 1,249 ours / 793 ruled / 6 perl's, 55,881 assertions never reached behind 104 whole-file rows; gate **348 / 11,692** PASS; no generation change.

  Before it: MAIN-READY-61 `6baaa3df` = s531c merged: #4083 (a method named like a statement modifier -- `$f->foreach(...)` -- no longer splits as a modifier; 157 Template-Toolkit rows pass, 153 re-caused to #4201 = PPI `_round` builds `->if(` / `->while(`'s parens as a Condition), #3851 (a glob alias `*a = *b;` carries b's prototype), #4140 (`"@{name[0,2]}"` is the slice), #4101 (a string eval inherits `use re` flags), #4020 (`*main::{HASH}` reads the stash); #4103 folded into #932; generation **v2-9900**; gate **347 / 11,685** PASS (the agent's run on the identical code tree; the rebase was records-only); sweep 18,761 +0; corpus-diff vs ade297e7 IDENTICAL; EVERYDAY 116 of 122.

  Before it: MAIN-READY-60 `ade297e7` = s527f merged -- PCL's PPI `pcl` = `9286f7c0` (§44 the Lexer statement class for `CORE::__END__`, AM §52 `defer`, AN §53 `class` statement structure -- both refused per statement, perl-shaped; `_core_separator_statements` deleted); PPI bug report **291 ok of 304** (stock PPI 1.291: 14 of 91); generation **v2-9700**; gate **346 / 11,660** PASS, install-pcl 61, container 17/17 (Fable); corpus-diff vs a76aaf01 IDENTICAL; sweep 18,761 +0; EVERYDAY 116 of 122.  Twelve batches merged in s530 (MAIN-READY-49..60).

  Before it: MAIN-READY-59 `b04ff624` = s527g merged -- PCL's PPI `pcl` = `c4d0da86` (group AH 2nd pair §14: a closed `<...>` where perl expects a term is one readline token -- #4100 `sort </ABS/x-1/*>` fixed; AL §43: a lexical sub named like a keyword (`my sub s` / `y` / `q` / `tr` / `x`) is called by its bare name, scoped to its block); report **283 ok of 298**; op/glob.t 14/4 -> 17/1, op/lexsub.t 81/61 -> 83/60 (DROP -> OK); emission IDENTICAL, generation unchanged; gate **346 / 11,654** PASS, install-pcl 61, container 17/17 (Fable).  §42 handed to the Lexer side, §40 `format` not started.

  Before it: MAIN-READY-58 `c6b6f325` = s530b merged -- Template-Toolkit JOINS the CPAN board (121 files: 57 PASS / 45 PARTIAL / 19 FAIL, 2,085 ok / 408 not ok, 0 TIMEOUTs; was 3 / 10 / 108 with 31 timeouts; 428 rows blessed by cause, #4083 holds 314); the #3869 hangs fixed at both halves: #4080 (an element store through a package-qualified scalar was `p-scalar-=`), #4081 (an imported sub named like a runtime function -- `ok` -- overwrote `pcl::pl-ok`), #4082 (a slice assignment `@h{@k} = (@v, 3)` stored @v's COUNT -- silent wrong); board **30 dists / 896 files, 550 / 208 / 138, 80,702 ok** (before s530c's gains); generation **v2-9600**; gate **346 / 11,649** PASS (Fable); corpus-diff vs d8e387f2 IDENTICAL; speed vs the session start: NO CHANGE (66 rows, runtime A/B confirmed).

  Before it: MAIN-READY-57 `d8e387f2` = s530c merged -- #4042 (`use re '/FLAGS'` default flags reach m// qr// s/// split and a run-time pattern; one scope walk `re_pragma_at`), #3859 (`&NAME(LIST)` bypasses the prototype: `p-amp-spread`), #3864 (`${name{KEY}}` / `${name[i]}` / `@{name[...]}` in CODE are the element / slice; the interpolated residue = #4140), #4062, #4060 (a fresh qr object per evaluation, `p-qr-from-parts`, the compiled-regex cache shared), #3854; Pod-Simple 8 files + Log4perl 025CustLevels.t now pass; generation **v2-9300**; gate **346 / 11,644** PASS (Fable, combined tree); corpus-diff vs 2857cd7f = 16 files (`p-amp-spread` + `p-qr-from-parts`).

  Before it: MAIN-READY-56 `2857cd7f` = s527e merged -- PCL's PPI `pcl` = `dd90e749` (89 commits over upstream; Lexer groups AI §49/§54/§57 a `{` after a glob is a subscript, AJ §51 a bare block's `continue`, AK §50 an anonymous hash where perl expects a term), the orphan-`continue` repairs DELETED (146 lines), report twins **295 rows, 273 ok / 22 not ok**; generation **v2-9100**; gate **343 / 11,595** PASS (Fable, rebased over s530a, artifacts regenerated stamp-only); corpus-diff vs ff265320 = 7 files, the #2610 detector table only.  NOT YET PUSHED: the container run.

  Before it: MAIN-READY-55 `ff265320` = s530a merged on MAIN-READY-54 = s527d.  s527d: PCL's PPI `pcl` = `40fed036` (89 commits over upstream; groups Z..AH: `$ name`, `CORE::__END__`, `& {BLOCK}`, `@{+}`, `foo::::bar`, `use experimental 'try'`, `<...>` after a block, `defer`/`class` as features), three repairs deleted, report **258 ok / 17 not ok** (275 rows); emission IDENTICAL.  s530a: #4000 rebless half (a re-blessed array/code referent records its new class on the value; #4061 the first-bless half, a representation decision), #4001, #3963, #3865 (`$h{@a}` keys on the count), #3945 (`do sub {...}` is undef, never called), #4040 (`use base` skips an already-isa parent); generation **v2-9000**; gate **342 / 11,580** PASS (Fable, combined tree); corpus-diff vs 38616933 = method.t; sweep 18,761 +0; EVERYDAY 116 of 122.

  Before it: MAIN-READY-53 `76a048ec` = s529a merged -- three runtime correctness fixes: #3824 (a reference copied into an element gets a FRESH container with the same referent and class -- the element no longer aliases the variable; URI / Pod-Simple / Path-Tiny board rows left the baseline; side effect #4000 ACCEPTED: an element copy of an already-blessed array/code object misses a later rebless, as the scalar-copy spelling already did), #3868 (`do { local *FH; *FH }` hands out a fresh glob per scope; the filehandle slot is localized), #3524 (a required file's top level runs in scalar context; bind in `p-load-module-cached`), #3923 (the companion's `*desc-unstable*` marker); runtime-only, generation v2-8900; gate **341 / 11,543** PASS (Fable); corpus-diff + lib emission-ab IDENTICAL vs 3973327c; companion comp/require.t 915/832; EVERYDAY 116 of 122.

  Before it: MAIN-READY-52 `3973327c` = s529b merged -- three compiler correctness fixes: #3870 (`use constant` registers in its own package; a later `require` of a same-named constant no longer turns the earlier reads into strings -- every Log4perl program printed its debug lines), #3860 (`use base` / `use parent` push  inside `p-BEGIN` at their own position), #3856 (`#` comments under /x and `(?#...)` are not interpolated -- `Template/Parser.pm` transpiles with 0 drops); generation **v2-8900**; gate **340 / 11,534** PASS (Fable); corpus-diff vs df8e1ec9 = magic.t method.t parent.t; sweep 18,761 +0; EVERYDAY 116 of 122.

  Before it: MAIN-READY-51 `df8e1ec9` = s527c merged -- PCL's PPI `pcl` = `a2fa26a0` (70 commits over upstream; Lexer groups T-Y: §18 finally, §6/§10/§29 foreach loop variables, §17/§36 subscripts after a deref, §5 anon hash, §13 `$/`, §37 `print({ STDOUT } ...)` with PPI's fixture updated per the s529 ruling), five more repairs DELETED (192 lines), report twins reconciled **253 rows, 235 ok / 18 not ok**, t/op/for.t RUNS (136/148, was a whole-file loss, #1580), a silent wrong fixed (`sub j { { Foo, 1 } }` returned a flat list); generation **v2-8885**; gate **337 / 11,488** PASS, install-pcl.t 61, container 17/17 (both Fable); corpus-diff vs f6ceec03 IDENTICAL; EVERYDAY 116 of 122.

  Before it: MAIN-READY-50 `f6ceec03` = s527b merged -- PCL's PPI `pcl` = `ded727c3` (34 commits, 57 over upstream 455bfa73; groups J-S + D2/E3 + YAML), twelve PPI repairs DELETED from `Pl/` (+114 / -1,081 lines), report **166 ok / 31 not ok** (197 rows), #1817 fixed (`-f 'TEST'` after a block), YAML::PP no longer a dependency; generation **v2-8785**; gate **337 / 11,493** PASS (Fable re-run), install-pcl.t 61 PASS, container 17/17 (agent, final code); sweep 18,761 +0; EVERYDAY 116 of 122.

  Before it: MAIN-READY-49 `77c7d9d7` = s526w2 merged -- the CPAN board widens by three dists (YAML-PP, Pod-Simple, Log-Log4perl; Template-Toolkit censused, not joined, behind #3856 + #3869): **29 dists / 775 files, 480 PASS / 171 PARTIAL / 124 FAIL, 78,256 ok**, `--diff` 0 NEW; nine fillers (#3850 #3853 #3855 #3858 #3861 #3862 #3863 #3866 #3867), generation **v2-8688**; gate **337 / 11,480** PASS (Fable re-run); sweep 18,761 +0 GATE clean; EVERYDAY 116 of 122; corpus-diff vs ff5f72b5 = 11 files (the `:strict-lv` marker + hash.t #3863).  Below = the s528 numbers, HISTORY.)  (s528, 2026-10-25 21:4x, main = MAIN-READY-48 `d47caabe` = s527a merged + three CI-found fix-forwards -- YAML::PP is the vendored PPI's fifth dependency `13720073` (loaded on demand only since s527b), the installer test's remedy row `30b4d0c5`, the compiler stamp hashes the VENDORED PPI #3921 `ff5f72b5` (`%p-vendored-ppi`; cache-surface-01.t 34-35, module-fasl-cache-01.t's #1843 block under PCL_SYSTEM_PPI=1); gate after them 337 / 11,469, everyday 116 = 116, the container test 17/17 (PPI VENDORED: `vendor/PPI` = branch `pcl` 8955ba30 over upstream 455bfa73, the 12 token bugs fixed AT THE SOURCE in PPI's two-commit PR shape, PPI's own suite green at every commit, the report 14 -> 33 ok, 2 of 10 repairs deleted (§16 §23; the 8 left wait for s527b's term-end rule), emission byte-identical so generation v2-8585 is UNCHANGED, last CODE commit `48fd2e21`; the round-5 FULL companion read and every mover attributed, `c47f0f28`: op/range.t 158/4 = #1025, op/utfhash.t 97/2 = #3832 + #1389, comp/require.t's 3 "NEW" = a count difference on one repeated description, not a mover, rows 42-44 blessed under #3524); -47 = `584c1ca6` = s526w1 merged (the CPAN board widens by six dists -- TimeDate, HTTP-Date, URI, HTTP-Message, YAML-Tiny, Data-Dumper; fillers #1025 #1026 the s///e replacement compile, #3825 the context bind of a sub named like a runtime operator, #3826 the pure-Perl Clone shim, #3832 a bare `local $/;` is slurp mode; last CODE commit `31cd8c02`, generation v2-8585; Fable restored the three s214 `ok(1, SKIP)` rows of perl-tests/range.t to their original assertions, 158/0); -46 = `e2ce5b2e` = s525q merged (parser step 0 c2: the BINDER in dual-run, INERT -- `Pl/Binder.pm` + 31 hook lines, emission identical); -45 = `761d92f4` s525u merged (the runtime review's named repairs: #3760 shift UV, #1040 box face PARTIAL, the flip-flop fold, the 66 default arms audited; `cl/`-only, emission identical); -44 = s525p merged (the expect-state checker + the PPI-bug registry, INERT); -43 = s523d; last CODE commit `cd76a6f1` (s525u's shift fast path), newest generation stamp v2-8585 (s526w1; before it v2-8284 from s523d, unchanged by a `cl/`-only batch; the artifacts carry it -- generations are per batch, never renumbered) -- measured on that sha; everything further down this section is HISTORY and its counts are stale):** gate `Result: PASS`, **337 files / 11,469 rows** (11,466 at the -48 merge; 336 / 11,462 at -47); **the CPAN board after s526w1 (26 dists / 582 files, measured on its tree): 362 PASS / 136 PARTIAL / 84 FAIL, TOTAL passing 69,476** -- the 20 older dists read round 4 exactly (`docs/STATUS.md` has the row); full `perl-tests/` sweep GATE clean, **TOTAL passing 18761**, 626 failing (s525u's two bop.t negative-shift rows left `fail-baseline` by edit), 60 of 108 files fully passing, 98 run to the end and 10 stop part-way, drops 5 = census; the CPAN board ROUND 3 (20 dists, 449 files; `baselines/cpan-board-s523r3.tsv`, measured on MAIN-READY-38) **254 PASS / 125 PARTIAL / 70 FAIL, TOTAL passing 65,073** (round 2: 28,489 -- Text-CSV's 20 files run for the first time after #3061; the 908 new not-ok rows of SLU exotic_names.t = #3529) against 2,065 blessed rows (1,512 blessed rows left after the s524 merges; the ROUND-4 board on MAIN-READY-43 (re-run s525 11:21 -- the s524 run was cut by the 02:43 shutdown; `review/board-main43.tsv` + `.rows.tsv`): **277 PASS / 107 PARTIAL / 65 FAIL, TOTAL passing 66,729**, `cpan-scoreboard.pl --diff` 4 NEW / 16 FIXED / 1 LOST against the blessed rows -- attribution in progress, `s525/PAUSE-s525.md` 11:3x + 12:0x); `EVERYDAY: 116 of 122 identical to perl (95.1 %)`; **CI on main GREEN (4f11ffdc, 2026-10-25 21:5x, after the three MAIN-READY-48 fix-forwards; the install matrix GREEN on 6ddab77d and the container test 17/17 on the later runtime change) -- "CI green" means BOTH workflows; the matrix is trigger-gated (Mondays 05:20 UTC, or a change under tools/install-matrix/ or its yml), so a `cl/`-only change proves it locally with `prove tools/t/install-container.t`.**  The user-facing copy of these numbers is `docs/STATUS.md` -- update both together.
- **The three xs files are PARKED (USER decision, s485, 2026-09-14): `Pl/t/xs-01/02/03.t`
  `plan skip_all` at the top until the pclxs project (the XS bridge, a separate
  repo, sibling checkout `~/pclxs`) is working again — their 13 failing rows were
  pclxs's, not PCL's (USER s394/s395: "ignore XS rows").  So the gate now reads
  **`Result: PASS`** with those three files reported as skipped, and "xs-only"
  is no longer a verdict: a `Result: FAIL` is a real failure.  The tests are kept
  whole; `PCL_XS_TESTS=1 prove Pl/t/xs-0*.t` runs them; re-enabling = deleting
  the block at the top of each file.  The paragraphs below describe the
  pre-s485 state (the 14 conditional rows) and stay as history.
- **160 test files, 5700 rows** (s430) with a built pclxs sibling (s409, measured
  COLD; the 13 pclxs xs rows currently FAIL there — pclxs is under separate
  work, user s394/s395: ignore XS rows); **5600 without** (arithmetic: minus
  the 14 xs rows).  **RULED s409: compare a gate count against a measurement
  of the SAME tree** — the xs files PRODUCE 0–14 rows depending on where
  pclxs\x27s current state aborts them, so a written-down total drifts on its
  own; when only a number is at hand, subtract the xs rows produced in each
  run (s408 wrote 5439 with 11 xs rows = the same 5428).  The gate count is deterministic *per environment*, but it
  is conditional: `Pl/t/xs-01/02/03.t` (6+4+4 = **exactly 14** rows) resolve
  pclxs as `$FindBin::Bin/../../../pclxs` — **a sibling of the CHECKOUT** — and
  `plan skip_all` (contributing 0) when it is missing or `libpclxs.so` is not
  built.
  **GOTCHA that cost s319 an investigation: comparing against a `git worktree`
  silently subtracts those 14**, because the worktree lives elsewhere and its
  sibling path does not exist.  A worktree is still the right way to compare
  against HEAD (never a stash-copy) — just set `PCLXS_DIR=~/pclxs`, or expect
  and subtract the 14.  What must hold either way is `Result: PASS` and the
  file count.
- **XS conformance: 398 pass, 0 fail — fully green** against pclxs's corpus
  with real perl as oracle (`tools/pcl-conform`, re-measured s339 after the
  rule-12 pass over `cl/pcl-xs.lisp`; XS OO/magic
  works — Digest::MD5's own md5-aaa.t is 256/256 under PCL).
  Not in the Pl/t gate — minutes, not seconds.
- **All passing**
- **Runtime: ~2:30 with `tools/prove-core`** (~5+ min with plain `prove -j8`;
  each test file spawns a new SBCL process)
- **The full sweep RUNS ITS OWN GATE (s330, #204)**: `perl tools/sweep-perl-tests.pl
  --jobs 8` with no file arguments ends by running `tools/sweep-diff.pl diff
  baselines/fail-baseline.tsv .faillog` and **exits with that verdict** (`--no-gate`
  opts out; a sweep of named files stays informational).  The diff now has a
  **fourth bucket, LOST** — baseline PASSING rows the run did not produce,
  compared against `baselines/pass-baseline.tsv` (`sweep-diff.pl save-status .faillog
  baselines/pass-baseline.tsv` re-blesses it).  The first three buckets read failing
  rows only, so a change that makes a file abort EARLIER used to report
  `0 new / 0 fixed` while coverage evaporated (s328: state.t 157 → 69).  Every
  run prints `TOTAL passing: baseline N, current M`, and when no pass baseline
  is found it prints `LOST: NOT CHECKED` rather than nothing.
  **FIFTH bucket, DROPS (s402, #343)** — statements the compiler could not
  lower and replaced with `nil` (#138 family).  They are invisible to every
  other bucket: the row simply is not there (bless.t's drop IS a test row, in
  a file the sweep calls passing).  The sweep records a per-file `drops` count
  in `.faillog/_status.tsv` (columns: name, status, pass, fail, planned,
  **drops**, note) and `sweep-diff.pl` compares it against
  `baselines/parse-error-drop-census-s399.tsv` — **the census IS the baseline**, a
  drop leaves it by EDIT; more drops than the census fails the run like a NEW
  failure.  `tools/run-perl-suite.pl` prints the same comparison for perl's
  own t/, and every transpile now announces a drop as it happens:
  `PCL: statement dropped at F line N: <text> -- <reason>` (task #339; OFF in
  `pl2cl --module`, the runtime's module load — `PCL_DROP_ANNOUNCE=all`
  forces it back on).
- Full `perl-tests/` sweep, CURRENT (s406, re-run after #348 switched
  `which_perl`'s children to PCL): **TOTAL passing 18517** across 108
  files, GATE clean, drops 12 = census — identical to s405 in every bucket
  (closure.t stays OK 272/4, pack.t OK 5636/89).  The one row above the s399
  number is `ref.t` 190 → 191,
  edited into `baselines/pass-baseline.tsv` by hand with its cause (s404l's
  blank-line fix in `tools/pclperl-for-tests`, attributed by bisection in a
  worktree).  The s399 measurement it sits on: **704 blessed fails**,
  **64 files fully passing**, TOTAL passing 18516 across 108 files.  The
  s399 move is the only one that is not a regression to chase: task #323 made
  `warning_is`/`warning_like`/`warnings_like` in `perl-tests/t/test.pl` stop
  manufacturing a pass, so 24 rows (assignwarn.t 20, hashassign.t 4) became
  honest failures with ONE cause — PCL emits no warnings-gated diagnostic
  (`not-supported.md` "Warnings-gated diagnostics are absent", owner #221).
  Both baselines were edited ROW BY ROW, never re-blessed, and
  `baselines/pass-baseline.tsv` carries a header note saying so.
  Historical baseline text follows.
- Full `perl-tests/` sweep: **679 blessed fails** in `baselines/fail-baseline.tsv`,
  **65 files fully passing**, 18499 passing / 910 failing across 108 files
  (re-measured s356 at the E4.1 flip; GATE clean, +0.  The flip moved three
  files, all pre-authorized and edited into the baselines with their causes:
  **eval.t 110 → 114** — the ruled eval refusal makes the four `my $$x`-family
  rows fail as perl does, so they pass (and leave `fail-baseline` by EDIT);
  **tr.t 241 → 239** — `tr.t:494` builds a 73,769-char `eval` string, the one
  F6 oversized-run-form event, a ruled refusal per `fable-answers-s346.md`
  §2.3; **lex.t 46 → 45** — the #228 `[perl #129069]` registration, whose pass
  was an accident of `--lenient-ppi` truncating NUL source to nothing.
  (re-measured s341 on a COLD cache; `sweep-diff.pl diff
  baselines/fail-baseline.tsv .faillog` = **0 new / 0 fixed / 0 LOST**, plus 2 rows
  the tool
  itself flags UNSTABLE — new fails ABOVE the abort point of postfixderef.t and
  ref.t, both already PARTIAL, so they are crash-file noise, not regressions.
  **Both baselines were re-hygiened in s341 (#223)**: the two now-passing
  scalar.t rows edited out of the fail baseline, and `baselines/pass-baseline.tsv`
  re-blessed from that run after a per-file audit — it had been blessed at
  s337b from a run OLDER than its own commit, which is why every later session
  read a phantom `+8`.  `save-status` now stamps `# taken-at: <sha> <date>` so
  a stale bless is visible instead of inferred.
  689 → 683 in s330: the `scalar()`-never-dereferences fix made 7 rows pass
  (removed by EDITING them out, never by re-blessing from a run, which would
  silently absorb anything else that moved) and exposed 1 row that had been
  passing only because `scalar()` flattened both sides to undef — added by
  hand with its cause.  s323 had removed the `do.t` "$! is EISDIR on do dir"
  row the same way.)  Baseline from s315d —
  class-model target-first reads, $TODO honored again under :invert,
  no-match s/// write gate; fresh_perl/runperl children run under PCL via
  `tools/pclperl-for-tests`; `PCL_FRESH_PERL=real` restores the old compare
  mode.
  **pack.t and the TIMEOUT retry (#176, s322).**  A file that TIMEOUTs
  contributes NO rows, so `sweep-diff.pl` can only report its baseline
  failures as *unverified* — a regression inside a timing-out file is
  invisible, and the headline "0 new" says nothing about it.  pack.t was
  merely SLOWER than the timeout (~166 s at s322), not hung, so the sweep now
  **retries a TIMEOUT once at 3× the timeout, at the end of the queue**
  (`--no-retry` disables); no operator has to remember `--timeout 400`.
  Measured s322: pack.t completes at 5636 pass / 89 fail and its failures are
  **identical to the blessed baseline** — 0 new, 0 fixed.
  **s334 (#184): pack.t is now ~69 s — under the 90 s default.**  Its minute
  was COMPILE time, not run time (78.5 s transpile → 7.0 s; the run is 54.7 s
  either way), and the cause was a per-token tree-walking predicate in
  `_rewrite_var_uses`, not the regenerated artifact the task suspected.  The
  retry stays as the backstop for contention.
  **CORRECTION (s322): the earlier claim here that "pack.t has NO rows in the
  blessed baseline" was FALSE** — `baselines/fail-baseline.tsv` has always carried
  its 58 pack.t rows.  The claim came from `grep`ping that file, which
  contains NUL bytes: grep then treats it as binary and prints nothing.  Use
  `grep -a`, or perl, on any `.tsv` under `.faillog/` or `baselines/` (the
  blessed baselines moved from `docs/` to `baselines/` in s440 — USER: docs/
  must stay browsable).
  See `docs/sweep-bug-catalog.md`
- v2 pipeline census: 111 files v2-native / 0 gated to v1 — E1 complete
  (`perl tools/v2-census.pl` for the live numbers)


---

# Part 2 -- Key Files to Read: the review record (s321 .. s439)

## Key Files to Read

When resuming work:
0. `docs/DECIDED.md` - **One-grep index of settled questions** (grep it before probing or designing anything — see the lookup order at the top of this file)
0a. `dot-claude-in-home-dir/README.md` - **the TASK STORE and Claude's MEMORY NOTES live IN THE REPO** (s507, USER 2026-10-04, task #1833): `~/.claude/tasks/pcl` and `~/.claude/projects/-home-USER-pcl/memory` are SYMLINKS into `dot-claude-in-home-dir/{tasks/pcl,memory}`, so a clone is the whole project and every `#NNNN` in the docs is a file there.  It is PUBLISHED with the repo: a task or a note holds technical content and work decisions only, nothing personal.  In an agent WORKTREE the directory is a stale checkout — tasks are always read and written through `~/.claude/tasks/pcl` (the main checkout's copy) and committed on main by the reviewing session.
0b. `briefs-and-rules-for-claude-subagents/README.md` - **the agent RULEBOOK, the session PROTOCOL and the BRIEFS live in the repo too** (s507): `s473/COMMON.md` (the rules every background agent reads first), `sNNN/SHARED-BOX.md` (which batches run, the merge order, how heavy runs take turns on one machine), `sNNN/<label>-prompt.md` / `resume-<label>.md` (the briefs), `sNNN/PAUSE-sNNN.md` (the reviewing session's running notes — the resume recipe after a cut).  The old `~/pcl-agent-scratch/...` paths are FILE-level symlinks into it; `~/pcl-agent-scratch/` otherwise holds only measurement output and review probes.  WRITE THE REPO PATH (the Edit/Write tools refuse a symlinked file; `perl -pi` / `sed -i` on the old path replaces the link with a copy).  A new session writes its protocol, briefs and notes here under `sNNN/`.
1. `docs/session-log.md` - Session history (compact, newest first)
2. **The per-session review records -- `docs/fable-answers-sNNN.md` (rulings) and
   `docs/opus5-review-requests-sNNN.md` (asks) -- were REMOVED from the tree in s440
   (USER: "it doesn't seem necessary").**  Their rulings are indexed in `docs/DECIDED.md`,
   summarised in the items below (2a–2z, kept as the compressed record) and narrated in
   `docs/session-log.md`; a cited file is readable from history:
   `git show 959bf43:docs/fable-answers-s439.md` (959bf43 = the last commit holding all
   85, and the six session HANDOFF docs `docs/*handoff-s*.md` removed with them).  **Since s440 a review session writes its rulings into DECIDED.md + the session log
   + the live plan doc directly; do not start a new review-doc family.**
2b000. **s417 RULED + Track B2 (#343) SHIPPED (s418, Fable, 2026-08-20;
`docs/fable-answers-s417.md` + `docs/b2-ceiling-fix-s418.md`).**  s417 APPROVED
(gate COLD 153/5590; sweep re-run clean; 14 probes).  **B2 = the stale ceiling,
FIXED by recomputing at use**: `$last_low_prio_op` DELETED; the paren-less
list-operator argument ceiling is derived from the CURRENT `@$e` by one
rightward scan to the first same-level `and`/`or`/`xor` (the probe's own
`$actual`, validated over 658 files; the region prohibition does not apply —
no new rule, the boundary's meaning is unchanged; the `PCL_B2_TRACE` probe is
deleted, not a gate).  Emission A/B over the four populations: diffs ONLY
bless.t + split.t (both populations) + **Text::CSV_PP.pm:1566 — an ELEVENTH
uncounted silent-wrong in the cpan population** (`… && grep m/\D/ => keys %{…}
and $hdrs ||= "auto"`, grep swallowed the `and`; probed both branches vs
perl); reg_fold.t byte-identical as predicted.  Gate-SET scan 638×2: exactly
the 4 drop→OK moves.  Sweep TOTAL 18369 → **18363** — bless.t's un-dropped
assertion runs and FAILS HONESTLY (`bless` misses get-magic on a tied operand,
**#408**), split.t's un-dropped `or skip` takes the file's own skip branch
(PCL's `\s` under /u misses NBSP, **#407**): 3 baseline fails fixed, 6
ACCIDENTAL passes (rows running on the dropped statement's undef `$sp`) now
skip; both baselines edited ROW BY ROW.  Census 46/139 → **42/135**; gate
**154/5594**; generation **v2-161**.  Guard `Pl/t/listop-ceiling-01.t`.
**runpcl now forwards `^PCL:` lines on a successful transpile** — it used to
discard the #339 drop announcement.  #259/#335 probed post-fix: NO change, as
designed.
2b00000000000000000. **s439 (Fable, 2026-08-23): s438 → s438i REVIEWED + APPROVED — the census instruments, Q4, Q5, Q6 all re-measured COLD and clean (gate 166/5779 → 5780 with the review row; sweep TOTAL 18312 (+0) GATE clean; companion 528 files, zero real movers); 61 probes vs perl.**  ONE review fix: #455's `_signatures_enabled_at` matched a QUOTED `'signatures'` only — `use feature qw(signatures say); sub f ($x, @r) {…}` on one line still took the old-prototype lowering; now any quoting, `:5.NN` bundles ≥ 36, `use v5.36`+, never `no feature`.  Filed **#491** (qualified handle NAME canonicalisation — `print main::STDOUT` is a CALL, `open(Foo::H1)` with no `package Foo` is a READ error, `open(main::FH)`/`print FH` do not meet), **#492** (s/// REPLACEMENT side leaves `$ｉ` literal), **#493** (pointer: `-pi` → #476, `(_)` → #260, #484 → shape (a)), **#494** (v0.1 doc refresh AT TAG TIME — README/STATUS say 155/5600 and "a drop is loud but not fatal", both false), **#495** (ask 7, #266's family), **#496** (the SHAPES corpus).  Twelve asks ruled in `docs/fable-answers-s439.md` §2.  **THE DISTANCE TO v0.1 (§4) IS FIVE STEPS: USER push → first CI run → #282 green → #494 → tag; #281 items 4/5 post-v0.1.  NEXT (Opus) = Q7 (#463 item 2 first), then the s439 fillers #479+ask 2 → #478 measured → #491 → #485 → #492 → #484(a) → #495 → #496.**

2b0000000000000000. **s438g + s438h + s438i (Opus, 2026-08-23): Q6 — three of P5.**  **#452**: `_bareword_fh_p` tested a PLAIN identifier, so `main::FH2` failed it and `<main::FH2>` emitted a BARE CL symbol — dead at LOAD ("The variable FH2 is unbound"), whole file lost; the SAME predicate serves the print `:fh` slot, so `print main::FH5 "x"` died too, while `readline(main::FH3)` was right because the BUILTIN path quotes the name.  One predicate widened, two bugs.  **#451**: `Pl::InterpScan`'s `$` arm continued into a subscript chain for `+`/`-` ONLY, so `"$?[1]"` printed `0[1]` silently.  **The interp set is NOT `Pl::Parser`'s `%PUNCT_ARRAY_CHARS`** — `^` is in that one (it asks which characters EMIT bare; `@^` is a legal array) and not in this one (perl does not subscript `$^`); probed character by character.  Nine of ten now agree in BOTH paths; the tenth, `!`, is wrong in the CODE path too (`$![1]` = the errno string indexed) — **#487**.  **#450**: a glob pattern with no metacharacter is ITSELF (`glob("/nope-xyz")`), and that is a per-WORD rule, so PCL also gained perl's WORD model (a pattern is a whitespace-separated LIST); `{` counts as a metacharacter although brace expansion is missing (**#488**), because answering `{a,b}` literally is further from perl than empty.  **A FIX THAT MAKES VALUES REAL EXPOSES WHAT WAS PASSING ON NOTHING**: t/op/glob.t row 18 compared two `eval q{ glob(...) }` call sites, both undef; now it fails honestly on **#489** (the scalar-context iterator is keyed by PATTERN, perl keys by CALL SITE) and **#490** (no csh quote stripping) — row edited by hand 14/4 → 13/5.  **A `Pl/t` EXPECTATION CAN ENCODE THE OLD BUG**: `glob-01.t` asserted `count:0` for a nonexistent literal file, the one answer perl never gives — rewritten under the s377 four-conjunct rule.  **#449 stays with #418** by its own ruling (the CL-unsafe punctuation arrays need a pipe-quoted emission; they keep dropping LOUDLY).  Gate **166/5779**; sweep TOTAL **18312 (+0)** GATE clean; companion io/+op/ for the `cl/` change.  Guard `Pl/t/punct-array-glob-01.t` 13 → 21.  **NEXT = Q7 fillers, #463 item 2 first.**

2b000000000000000. **s438d + s438e + s438f (Opus, 2026-08-23): Q5 — a signature PARAMETER is a DECLARATION (#454), the feature region includes the pragma's OWN LINE (#455), and every FRAGMENT re-parse runs the token repairs (#435).**  **#454**: `sub f ($x) {…}` + a later file `my $x` refused the whole file ("Parser2 TODO: file lexical 'x' captured by sub f").  BOTH scope questions now ask `_signature_param_canons` — the DETECTOR (`_check_sub_captures` scans the BLOCK; a signature is not in it) and the REWRITER (`_ref_shadowed` → `_stmt_declares_canon` walks the Statement::Sub's siblings; the signature is one).  **Fixing only the detector would have turned the refusal into a SILENT WRONG** (the body's `$x` renamed to the file lexical's cell).  Only the parameter NAMES count: a DEFAULT is an expression evaluated in the sub, so the split is top-level commas and each part's LEADING symbol.  **#455**: PPI hands the SAME source over as a `Structure::Signature` or a `Token::Prototype` depending on whether the feature is in force, and its tracking starts the line AFTER the pragma — so a sub on the pragma's own line took the OLD-prototype lowering (raw CL lambda list; an empty slurpy interpolated as undef).  **The repair is the BOUNDARY, not the TEXT, and corpus-diff proved it**: a textual rule broke `perl-tests/signatures.t:17`, where `sub t000 ($a)` BEFORE the pragma is an old-style prototype and `$a` is the package variable.  The textual test itself was in FOUR copies (one an absence — that IS how `_sub_head` disagreed) → `Pl::Parser::proto_text_has_named_params`.  **#435**: `Pl::Parser::fragment_doc` builds a fragment document AND runs the three in-place token passes `_ppi_parse` applies (`_reclassify_bare_vwords`, `_merge_unicode_symbols`, `_merge_punct_array_symbols`); NINE sites routed, so `"$Ｘ[$ｉ]"` stops reading element 0 and `"$Ｘ[$ｉ+1]"` stops dying.  **A GATE CATCH THE POPULATIONS COULD NOT SEE**: the first version called the helper fully-qualified from three other files and two `Pl/t` files died at "Undefined subroutine" (they load StringInterpolation WITHOUT Pl::Parser) — corpus-diff, the 951-file A/B and the sweep were ALL clean while the gate lost 97 rows; each cross-file site now does a runtime `require` (a compile-time `use` is circular).  All three changes emission-IDENTICAL over the four populations; **#454 also ran the gate-SET scan over BOTH populations (638×2) IDENTICAL** — mandatory when a refusal stops firing.  Gate **166/5771**; sweep TOTAL **18312 (+0)** GATE clean.  Guards `Pl/t/sig-param-shadow-01.t` (8) + `Pl/t/utf8-source-01.t` (25→28), inverse-guarded.  Filed **#485** (a signature DEFAULT reading an outer lexical gets undef) and **#486** (an old-style prototype with NAMES binds them as parameters on BOTH paths — a blessed baseline fail).  **NEXT = Q6 (P5: #451 / #452 / #449 / #450).**

2b00000000000000. **s438b + s438c (Opus, 2026-08-23): Q4 — the two named-unary OPERAND SITES become ONE (#453), and an imported `()`-prototype sub is a TERM (#365).**  **#453**: perl decides a named unary from the PROTOTYPE alone, and `_proto_parse_spec` was already the one reading of that shape, so `is_named_unary` now answers for a DECLARED sub whose spec is 1 / [0,1] (`(1000 4 24 27 30 46 100 115 1000` `(;1000 4 24 27 30 46 100 115 1000` `(*)` `(_)` `(\@)`) — which routes it to the site that runs `_extend_high_prec` and, because the same predicate is the strictly-single site's guard, out of the site that stops at the first term: `f "a" . "b"` was f(a)b, `f $x + 1` was 1, `(*)`'s `g + 1, "\n"` was g(1,"\n").  `known_no_of_params` is NOT a second source (its 1 covers shift/close/fileno, which must keep the strictly-single site and its bareword-filehandle branch).  **#365 — NOT where the task pointed, and that is the finding**: the classifier IS asked (23 times for `pi`) and answers `no` because **the prototype never crossed the `use`** — `_merge_module_prototypes` imported only block args, parameter SLOTS and names the export scan listed, and that scan reads literal `qw()` while Math::Complex builds `` from a variable (`my  = qw(pi …); our  = (qw(…), )`).  An empty prototype is a PARSE fact and now crosses a `use` on its own shape; `is_proto` does NOT identify it (`sub pi ()` arrives as is_proto 0 — a first attempt keyed on it changed nothing).  **ONE predicate `Pl::Environment::proto_is_zero_arg`** — the record test was in `PExpr::_is_zero_arg_func` and nowhere in the merge, which is how they disagreed (rule 11).  **Both changes are emission-IDENTICAL over the four populations** (corpus-diff 111 + emission-ab 951 files SAME / 0 DIFF / 0 RCDIFF, twice — RCDIFF 0 is also the die-scan), so the s371 rule applies and the GUARDS are the bar: `Pl/t/user-unary-01.t` (12 rows, 5 negatives) + `Pl/t/imported-term-01.t` (7 rows; its fixture builds `@EXPORT` from a VARIABLE so it tests the mechanism, not one module's spelling), both inverse-guarded on a `fe46c7b` worktree.  Gate **165/5760**; sweep TOTAL **18312 (+0)** GATE clean; companion **528 files, ZERO real movers** (io/pvbm.t alone 23/5 — the SIXTH time).  **#484 filed**: the #351 `WORD /` repair CAN reach a classifier (`_word_is_term`) but that reads only THIS document's terms and the prototype pre-merge runs AFTER the repairs, so `pi / 2 + pi / 4` is still repaired to a match and dropped.  **NEXT = Q5 (P4: #454, #435, #455).**

2b0000000000000. **s438 (Opus, 2026-08-23): the two CENSUS INSTRUMENTS — the drop census gains a SIXTH population (#473) and a SEVENTH is measured for the first time (#472); the companion scan stops filtering silently (s434 ask 1).  NO product change; six findings filed.**  **#473**: `cpan-tests/modules/**/t/**/*.t` (289 files, PROGRAM mode) is a census population — **42 files / 83 drops**, blessed with causes (`baselines/parse-error-drop-census-s399.tsv` 39/102 -> **81 files / 185 drops**; the 36 non-board pre-existing rows byte-identical).  The s436 A/B's "43/92" reconciles exactly: these 42 + `examples/tools.t` (9 drops), an EXAMPLE, named in the header with its count beside `t/japh/`; an `f702da3` worktree gives the same 42/83, so nothing moved.  Measured, not assumed: the dist `.t` files transpile WITHOUT the dist's own `lib/`+`t/lib` (identical rows), and the tool now strips the repo root out of message TEXT (one message quotes its own file, so a row depended on how ROOT was spelled).  **79 of the 83 are ONE mechanism — #478: `_extract_module_prototypes` skips every `Test2::`/`Test::` module BY NAME, so a `(&)` block-form call is not parsed** (probe: two modules identical but for the package name); the no-semicolon spelling is SILENTLY mis-parsed instead (`blk { 42 }` -> `(pl-blk 42)`, the block's value where perl passes a code ref).  Residue #480 (`$_.2` — PPI lexes `.2` as a float), #481 (a fat comma autoquotes a METHOD NAME), #482 (`$obj->state` dies in the compiler).  **#472**: `PCL_DROP_LOG` is one arm in the ONE announcer (`FILE/LINE/TEXT/REASON`, ungated by `PCL_DROP_ANNOUNCE`, never on stderr — the child's stderr IS the row's observed output); the sweep sets it around the RUN only, carries a `child-drops` column in `_status.tsv` and prints a per-file count AND the distinct SITES; REPORTED, NOT GATED.  **First measurement: 241 drops in 98 files, TEN sites** — two of them OURS, `perl-tests/t/test.pl:179-180` reached by 98 of 98 files (**#479**, and that line is wrong in REAL PERL too: `my $f` in a ternary's condition is not in scope in its branches), six one-off child programs, and **one real gap in a real module: core `Devel/Peek.pm:59`, where PPI lexes `<<index` as a HEREDOC (#483)**.  **s434 ask 1**: the companion `--all` scan is **528 files, not 523** — a `BEGIN`-`@INC` file was dropped silently, which is how five rows came to be unrefreshable; all five measured, all five produce a verdict, and `%NEED_HARNESS_NOT_RUN` (empty by measurement) feeds the existing NOT-RUN path.  ONE snapshot row edited: `op/require_errors.t` C_notok 70 -> 68, bisected to **the flip** (3/70 at 4356e77 and 9138404, 3/68 at f702da3 and HEAD), two rows that were failing and are now missing with their form.  Bar: gate **163/5741** (the +2 over s437's 5739 are s437's OWN review-fix rows), corpus-diff IDENTICAL over 111 (no generation bump), sweep TOTAL **18312 (+0)** GATE clean, companion 528 files with both SNAPSHOT holes at zero.  Asks: `docs/opus5-review-requests-s438.md`.

2b000000000000. **s437 (Fable, 2026-08-23): s434 + s435 + s436 REVIEWED + APPROVED — Q1 (instruments) + Q2 (THE FLIP) + Q3 (THE PHASE MODEL) are DONE; NEXT = the two census instruments (#473 + #472 + s434 ask 1), then Q4 (#453 + #365)** (rulings `docs/fable-answers-s437.md`).  Independently re-measured COLD: gate 163/5739 (only the 13 pclxs xs rows), sweep TOTAL 18312 (+0) GATE clean drops 5 = census, companion `--all --quick` 523 files with ZERO real movers (io/pvbm.t read 20/8 in the parallel AND the #366 serial pass and 23/5 alone — the FOURTH time; op/utf8cache.t DIFF→TIMEOUT at C_ok 2 is the recovery load reaching its "quadratic pos" loop, which PCL runs QUADRATICALLY — **#477**, a Target-A runtime bug: 100k chars 4 s, 200k 15.7 s, perl 1M in 0.09 s), lib A/B 21 permutation + 1 in-package, 42 probes vs perl.  **ONE REVIEW FIX (ask 6 was a real bug): `package NAME VERSION` now sets `$VERSION` at the HEAD of its section's compile phase** — a BEGIN in the same section read undef (perl 1.5) on HEAD and on the base alike; one line moves, corpus identical over 111, two `decl-ordering-02.t` rows, gen **v2-181**.  Filed from probes, both PRE-EXISTING: **#475** (a FILE-level `our` alias is not requalified across a TOP-level `package` statement — `our $t="m"; package A; $t.="A"` prints m; the in-block spelling works), **#476** (`-NAME` with a DECLARED sub is a negated CALL in perl, the string in PCL).  Rulings: prove-core MemoryMax scope IN (tools filler, measure the gate's peak first); #473 YES (cpan `.t` population, program mode; japh = header sentence); #472 = `PCL_DROP_LOG` side channel in the ONE announcer + the sweep's `child-drops` line, measure first; a promoted fix may ride in the promoting session under the s366 filler rule; #474 FOLD; the stub block stays; s434's six-file `--bless-rows` RATIFIED after READING the rows (standing: a bless after an instrument change says per file that the new rows fall under the registered reason); never-refreshed rows → NOT-RUN rows / joined scan by measurement.  Q7 re-ordered: the PROMOTED #463 item 2 (`++${"23::foo"}`, 18 rows behind one drop) first.
2b00000000000. **s433 (Fable, 2026-08-22): s431 + s432 REVIEWED + APPROVED — THE LIVE QUEUE IS `docs/plan-post-s433.md`** (rulings `docs/fable-answers-s433.md`).  Re-verified cold gate 160/5705 + sweep TOTAL 18366 GATE clean.  **The announce→DIE flip has a SHAPE: a perl-shaped, trappable RUN-TIME die at the drop site — ONE shape for every drop in every mode, no classifier** (the `;; PARSE ERROR` comment stays for the census; the stderr announcement, #363, `--module` silence and Track A stay); its unit is the statement, so the s431 file prices do not apply and the module-mode increment is DISSOLVED; it waits only for the two instruments (#467 recovery in both runners + re-bless; #462 module census).  **#456 half (b) PROMOTED and RULED as the PHASE model across sections** (#469: a later section's BEGIN runs after an earlier section's run-time code — probed; "hoist the def alone" is UNSAFE, symbol-macro cells).  Filed #468 (never-declared plain call: no AUTOLOAD, raw CL error), #469, #470 (identity-promoted file lexical ≡ `$main::y`).  Queue: Q1 #467+#462 → Q2 the flip → Q3 #456(b) → Q4 #453+#365 → Q5 → Q6 → Q7 module fillers (#457 first) + #468/#470; release gate independent.

2b0000000000. **s430 (Fable, 2026-08-22): B3 COMPLETE (#153 CLOSED; B3.3 = #374(b), the keyword-named lexical sub renamed POSITION-AWARE) — THE LIVE QUEUE IS `docs/plan-post-s430.md`** (P1 flip re-census → P2 #456 → P3 #453+#365 → P4 #454/#435/#455 → P5 #451/#452/#449/#450 → push week → CI → v0.1 tag; recipes in its §3; Fable: the flip design from P1's table).  Record: DECIDED §s430, `docs/b3-operand-collapse-s428.md` §B3.3.
2b000000000. **s425 (Fable, 2026-08-22): the three USER decisions CLOSED and round 2
of parallel Opus agents — E + B MERGED, F IN FLIGHT at session end.**  Decisions
(`docs/plan-post-s420.md` §4, DECIDED s425): **indirect object = MAYBE LATER**
(USER; `not-supported.md` entry after measuring that `new Foo`/`new Foo(LIST)`
already WORK and the scalar-invocant spelling is 2 loud census drops — not
refused, 288 rows); **the v0.1 tag DECOUPLES from the flip** (tag after the
first green CI run + O1); **next Fable = B3**.  **E = s426 (#388 consumer 3 +
#420 + #422.1) MERGED `c1983e1`**: StringInterpolation is an InterpScan
`scan_one` consumer (1216 → 664 lines) and a slice through a reference
(`@$r[0,1]`, silent-wrong in CODE) is fixed by one helper; ir-spec §3.2b.
**B = s423 (#418 widened) MERGED `f02fe2a`**: one helper `Pl::CLForm::cl_sym`/
`cl_pkg` (identity on ASCII) + the runtime `%pcl-invert-case` guard; uni/+mro/
legs 15 gains; **Fable fixes on top `dbef93c`** (`$#NAME` ArrayIndex spelling —
`$#Foo::Bar::x` was an ASCII READ ERROR; interpolated hash-key autoquote
class).  Gate 156/5655, sweep TOTAL 18365 +0 (B's tree; the two fixes verified
by guard rows + probes only).  Generation **v2-171** (B's; to be renumbered
FRESH after F).  Filed: #434 (mixed Word leaf), #435 (fragment re-parses skip
the #410 repair — `"$Ｘ[$ｉ]"` silently reads element 0), #443/#444 (E),
#431–#433 (B).  Rulings: `docs/fable-answers-s423-s426-s427.md`; consumer 2
scheduled as O4.  **F = s427 (O3 fillers) was STILL RUNNING — its worktree
`agent-a1ef8cda8527aca2b` is the first thing the next session reads** (memory
`project_s421_opus_agents_inflight`).  Launcher rule: per-agent generation
strings get a GAP above main (a rebase renumber landed on a sibling's key).
2b00000000. **s421 (Fable, 2026-08-22): s420 REVIEWED + APPROVED; THE LIVE
QUEUE IS `docs/plan-post-s420.md`** (review record `docs/fable-answers-s420.md`).
Independently re-measured: gate COLD 155/5614 (the 13 xs rows), sweep TOTAL
18364 (+0) GATE clean, census 33/106 with an EMPTY row diff, eight probe
files vs perl — every shape s420 names identical; #390 CLOSED (= #414).  Five
PRE-EXISTING findings filed from the probes: **#420** (`"$$r[1]"` /
`"$$h{k}"` / `"${$r}[i]"` / `"@$r[…]"` / `"@{$r}[i]"` inside a dq string leave
the subscript LITERAL — silent wrong; routed through #388 consumer 3),
**#421** (prototype table keyed by BARE name collides across packages),
**#418 WIDENED** (NFKC + `:invert` makes `%Ｘ` ≡ `%X` — pipe-quote ANY
non-ASCII symbol), **#422** (`"@{^CAPTURE}"` interpolation drops; `$Ｘ {a}`;
PPI's LEXER fails a file on `for my $Ｉ (…)` — §23 addendum), and **#423**: the
op/gv.t 50/47 → 49/48 mover s420 spliced as "pre-existing" is ATTRIBUTED to
**s419d** by a three-way probe (the box path prints every typeglob-holding box
as `GLOB(0x…)` — glob VALUE ≡ glob REF in the box model; s419d moved s///+tr
onto that path).  Rulings: **"pre-existing" is WHEN, not WHY** — splice a
companion mover only with its cause; **a `cl/` coercion/stringification change
runs the op/ companion leg**.  Queue: O1 = #419 + #418 (two emission rules,
the biggest prizes) → O2 = #423 (measure the glob representation first) +
#388-consumer-3 with #420/#422.1 as acceptance → O3 fillers (#415, #421,
#422.2, #399 count) → push week (USER, 2026-08-24) → first CI run → tag.
**Next Fable session = B3 (`_reduce_term`, #153)** — the flip's long pole.  **Then three Opus agents ran in parallel (s422/#419 + s424/#423 MERGED, gate 156/5639; s423/#418 IN FLIGHT at session end — `docs/fable-answers-s422-s424.md` §B).**
USER decisions open: tag DECOUPLES from the flip (recommended yes), B3 next
(yes), indirect object #399/#381.
2b0000000. **s420 (Opus, 2026-08-22): four of the flip's unblock-list tasks
SHIPPED — the drop census is 33 files / 106 drops** (was 42/135; edited row by
row with causes, `baselines/parse-error-drop-census-s399.tsv`).  **#414**: a CLONED
PPI element does not keep its tokens alive — PPI's DESTROY empties every
descendant, so a clone that is a NODE (the inner `[0]` of `"$x[$i[0]]"`) went
HOLLOW between parse and emit and the leaf emitter died on `content` = undef
(that is what the `…:?` meant).  One `_anchor` helper at all six
StringInterpolation fragment sites; every nested interpolated subscript works
now, and perl-tests/postfixderef.t's `.` overload handler stopped being
dropped (**sweep TOTAL 18363 → 18364**, test 107 left `fail-baseline` by
EDIT).  **#413**: the prototype table and `declared_subs` are keyed by the
BARE name — the convention `_bareword_callable_here` already relied on — so a
package-QUALIFIED declaration (`sub main::end(&)` from inside `package End`)
was invisible; normalized at the `Pl::Environment` seam in BOTH directions
(**perl applies a prototype on a QUALIFIED CALL too** — probed, the opposite
of what the task guessed).  **#412**: `@{^CAPTURE}` implemented beside
`@-`/`@+`, `%{^CAPTURE}`/`%{^CAPTURE_ALL}` mapped onto `%+`/`%-` (perl
SYNONYMS), `$#{^CAPTURE}` collapsed by `_block_caret_name` (which also fixed
the spaced `$ {^XY}`); a caret variable of ANY sigil must be pipe-quoted —
bare all-caps reads DOWN-cased under `:invert`.  **#410**: PPI splits `$Ｘ`
into Cast + Word (`ppi-upstream-bugs.md` §23, `Unknown.pm`'s `$` branch tests
`/[a-z_]/i` where its siblings test `/[\w:]/`), and merging the tokens is not
enough — the LEXER had already built the following `{…}` as a BLOCK, so
`_merge_unicode_symbols` re-classes the postfix chain (uni/gv.t 37/38 →
41/40).  Also **#417** (`$#+` is the pattern's group count, `$#-` stops at the
last participant — PCL truncated both) and **#416** (`s///` no-match returns
`""`, not `0`).  Gate **155/5614**; generation **v2-163**; gate-SET scan over
both populations = exactly 8 files, all drop→OK; every companion mover
verified PRE-EXISTING on a `47e0750` worktree and spliced into
`baselines/perl-suite-run.tsv`.  Filed: **#418** (a non-ASCII PACKAGE name
mismatches at the CL reader — SBCL NFKC-normalizes and `:invert` down-cases;
pipe-quoting defeats both) and **#419** (one `>0x10FFFF` literal makes the
WHOLE emitted file unreadable — `t/re/pat.t` is 1263 perl rows behind it).
2b00000. **s419 (Fable, 2026-08-21): the flip re-census DONE — the announce→DIE
flip is BLOCKED by its own bar; release phases 3+5 executed.**
`docs/drop-census-s419-flip-gate.md`: census re-verified ROW FOR ROW (42/135),
every drop classified into 14 families with owners; 82 are compiler gaps in
productive files (ref.t 191 rows, closure.t 272, …), so the flip waits on the
unblock list (#410–#415 new, #399, #374(b)/#365, #259, #153-B3) — re-census
after they land, flip with the s373 three-leg bar.  The one unblocked
increment (module-mode DIE, the last fully-silent #138 member) waits for a
cpan-board drop count (flip-gate §5).  **#279 DONE** (root junk deleted,
24 planning .mds → `docs/history/`, CODEGEN/MOO_MOOSE designs → `docs/`,
tracked `.suitelog-*` gone, hook path relative); **#280 DONE** (README
refreshed with measured numbers, `docs/STATUS.md` = the user-facing
compatibility page, `CHANGELOG.md`; NO v0.1 tag — precondition is #282 green
+ phase 4); **#283 authored** (`.github/workflows/ci.yml`, installer-based —
activates at the deferred push, week of 2026-08-24); **#282**: no container
runtime on this machine — the sanitized-fresh-HOME rehearsal ran instead, and
the first green CI run is the container half.  **Phase 4 OPENED in the same
session: #119 + #402 SHIPPED** (the ruled pair, s416 §7.4 — the two
stringify-instead-of-overload shortcuts: s///-and-tr/// match sources now run
the `""` overload + tie FETCH; `p-string-concat` folds through the
overload-aware `p-.` when a piece carries a `.` handler, RUNTIME-ONLY, perl's
multiconcat probed byte-identical incl. the single-piece-stringifies rule).
Gate **155/5600**; sweep GATE clean TOTAL 18363 (+0); opbasic/concat.t
248/6 → 249/5 = the ONLY change-attributed mover ([perl #124160]).  Guard
`Pl/t/overload-01.t`; filed #416 (s/// no-match `0` vs perl `""`).
**Queue: the M–N remainder (push + #282-in-CI, user-scheduled) → phase 4
continues (the bug hunt) → tag decision (see the schedule finding) → the
flip unblock list.**
2b0000. **#401 eval half SHIPPED (s418c, Fable, 2026-08-21) — `state` in a
named sub reaches STRING EVAL; #401 CLOSED (both halves).**  The cell
`$x__state__N` is a defvar, never let-bound, so the eval alist could not
carry it and the route refused any sub containing a string eval.  Fix (the
task's sketch, on the M5 block-cell precedent): original→cell registered when
the DECL STATEMENT lowers (`_eval_state_captures` — visibility starts at
perl's masking point), scoped by `_lower_sub`'s save/restore WITHOUT the
`_let_bound_vars` wipe (nested subs inherit — op/sub.t's very shape; defvars
cannot be the wipe's unbound-at-call-time hazard); alist order = let-bound
(my-shadow wins) → state → span.  Waiver ONLY for a scalar decl at the top
level of the sub's own block; inner-block decls + containers keep the
refusal.  **The mandatory cache leg needed NO code** — #296-B1's key already
carries capture NAMES, the alist VALUE resolves at runtime; probed: two subs,
same eval text, `ABA`.  t/op/sub.t TRANSPILE-FAIL → **DIFF 53/12** (one row
better than s410's 52/13; spliced).  Eight probes identical to perl; emission
A/B: op/sub.t the ONLY mover in 1139 files; gate-SET scan: only its verdict.
Gate **155/5598** (guard `Pl/t/state-eval-01.t`); generation **v2-162**.
**Two Pl/t guards were STALE against this session's own fixes and repaired
in the same commit** (the s416 rule, missed once and now paid twice over):
eval-01.t's #363 drop-in-eval rows used the very statement the B2 fix
un-dropped (reproducer swapped to #335's shape, with a swap-again note),
and parser2-02.t t62 asserted the refusal #401 removed (now asserts the
inner-block residue still gates).  **Queue now: re-census → announce→DIE
flip → M–N release (#279 → #280 → #282 → #283).**
2b00. **Track B1 (#372) SHIPPED s417 — stacked filetests.**  `-f -d $x` is ONE
term and lowers to perl's `_`-chain (`-x $f && -w _ && -f _`), never a nest
(nesting is SILENT WRONG the other way: `-e -f "/etc/passwd"` is 1 in perl,
undef nested).  It needed NO `_term_extent`/`_reduce_term`/`$end_pars` change —
the operator loop already reduced prefix runs rightmost-first; the bug was one
line in the SHARED oracle `_is_print_term_start`, which answered 0 for a `-X`
Operator, so the `$_`-default pre-pass spliced `$_` into the MIDDLE of a run.
Exposed a PPI bug (§22): after a SCALAR/BLOCK filehandle PPI splits `-e` into
`-` + Word, so `print $fh -e $f` was a subtraction of a call to sub `e` —
repaired on ADJACENCY (`next_sibling`), which is perl's own discriminator
(`print $fh - e $f` IS `-(e($f))`).  Census 27 drops → 1; gate **153/5590**;
generation **v2-160**.  Guard `Pl/t/filetest-stack-01.t`.  Residue filed:
**#403** (a filetest's FALSE is a DEFINED `""` when the stat succeeded — the
whole `p--*` family conflates it with undef; do NOT assert definedness on a
filetest until it closes), **#404** (perl stacks through PARENS), **#405**
(`print $fh -3` writes to `$fh`).  **Queue now: #343 (B2 — mechanism measured s417, `docs/b2-stale-operand-ceiling-s417.md`, FIX NEEDS A FABLE DESIGN) → #401-eval filler → re-census → announce→DIE flip → M–N release.**  (`class NAME ;` DONE s417.)
2b0. **`docs/fable-answers-s415.md` + `docs/b1-operand-grammar-s416.md` — the s414+s415 batches RULED (s416, Fable, 2026-08-20) and THE B1 DESIGN.**  Both sessions APPROVED (gate/sweep/companion independently re-run; TOTAL 18369; 14 probes identical to perl); ONE review fix — parser2-02.t t60 was a stale twin guard of the refusal s415e removed, failing the gate since `829bcf5` (standing rule: grep Pl/t for a refusal's message text in the commit that removes it; gate rows now 5566).  Rulings: Track A deviations RATIFIED + drop-harvest-first standing; "refused is explained" IS the census intent; #401-eval → session L (cache-key leg mandatory); #402+#119 two tasks / one session / release phase 4; `class NAME ;` refusal AUTHORIZED, strict key only.  **B1 (#372) re-designed from measurement and UNBLOCKED — the b1 doc supersedes option-b-phase2-plan §2's sketch**: one predicate (`_is_print_term_start`: a `-X` Operator starts a term) + the print-argument leg + the `_`-chain desugaring (naive nesting is SILENT WRONG, probed); NO `_term_extent`/`$end_pars` changes.  **Queue: #372 → #343 → `class NAME ;` + #401-eval fillers → re-census → announce→DIE flip → M–N release.**
2b. **(`docs/opus5-handoff-s413.md` — the s413 handoff, REMOVED s440 with the other handoffs; `git show 959bf43:docs/opus5-handoff-s413.md`; its queue is long since executed — kept here as the compressed record.)**  §1 where the project is; §2 Opus's first job = task #395: verify (gate + bench A/B + full sweep on the tip) and `--ff-only` merge branch `s413-lisp-dedup` (the six runtime dedup families + fix #394, Fable-verified); §3 the queue (remaining in-scope dedup families 2+10, 14/26, 38 → plan-post-s408 §2); §4 rules: **dup-census scope = COMPILER + RUNTIME ONLY (USER s413; tools may be replaced; tests never optimized)**, the census is a bug finder (probe the DIFFERENCE between copies vs perl before unifying — #393, #394), one sweep covers an IDENTICAL batch, an interrupted tool call may have RUN.

2c. **`docs/plan-one-compiler-s411.md` + `docs/dup-census-worklist-s411.md` — THE LIVE QUEUE since s411 (Fable, 2026-08-18; USER: "structural first, but not at any cost")**: the answer to #379, sized from measurements — the two generators differ in exactly TWO emission rules, the native attempt costs 9 % of compile time, `lower_embedded_block` declines 12 of 1 064 blocks, ~1 500 embedded blocks per corpus are compiled by v1 text and DISCARDED.  Phases **R** (`Pl/Passes.pm` + `PCL_OPT`, the optimization registry, #383) → **A** (one expression compiler: port the two rules, ONE parse, delete ExprToCL2, one dialect, #384) → **B** (one seam function `capture_v1`, PExpr `analysis_only`, hook-only blocks, #385) → **C** (the 12 decline shapes, #386) = 3–5 sessions to the release shape, replacing E5.1–E5.5's 9–17; E5.3 (`local` first) post-release; the extraction worklist #387 (`tools/dup-census.pl` + `tools/sub-call-census.pl`; verdicts EXTRACT / LEAVE / DELETED-BY / SUPERSEDED-BY) and the InterpScan consumers 2+3 port #388 interleave.  Standing rules: a found silent-wrong is FILED and jumps the queue only if it regresses a baseline or blocks a phase; a fix in code a phase deletes is wasted; Fable sessions rule asks + do structure.  `docs/fable-answers-s410.md` = the s410 asks, ruled short.  The plan-post-s408 queue resumes after Phase C (§6).

2d. **`docs/fable-answers-s408.md` + `docs/plan-post-s408.md` — the s408 batch RULED (s409, Fable, 2026-08-16) and THE LIVE QUEUE**: all seven s408 commits APPROVED as shipped (gate re-verified COLD 149/5442, only the 13 pclxs xs rows; sweep RE-RUN clean TOTAL 18513; 27 probes vs perl 5.40.3, no regression).  Standing rules added: **a census INCREASE is legal when it converts a worse failure (a crash-form) into a counted drop**; **a gate row count is compared against a measurement of the SAME tree** (the xs files produce 0–14 rows on their own); **an eval-mode drop DIES — "announce and continue" REJECTED** (it keeps the wrong value); a fragment mini-parse is the established pattern; **a `--quick` NOT-RUN row is never a mover** (#366 runner bug fixed: 11 files were re-run ALONE, +23 min).  **Three tasks head the next Opus session — #378** (anon `__SUB__` IMPLEMENTED as a self-reference rewrite: #368's die aborts op/sub.t 51/14 → 25/6, a cost s408 did not measure; the shape is modern Perl's recursive closure), **#377** (`sub outer { my $x = shift; my sub inner { $x } }` CRASHES unbound `$x__file__0` — p-raw-params binds the promoted name, no cell) and **#376** (the lexsub rename's three uncovered spellings: `my sub c; sub c {…}` fwd-decl SILENT WRONG across two scopes; `sub NAME` in the region defines the LEXICAL in perl; cross-package use crashes).  #374 half (b) corrected (position-aware renaming, not TERM TERM TERM).  **Queue (plan-post-s408 §2): H = #378 → #377 → #376 → #341 measured (→ #373 only if rows sit behind it) → I = #342-2 + #281 items 1+2+6 → J–L = Option B phase 2 (#371 → #372 → #343 → #369/#370 → the flip) → M–N = #279 → #280 → #282 → #283.  Fable: #281 design half, B1 operand grammar, boxed aggregates post-v0.1, #221 post-release.**

2e. **`docs/fable-answers-s406.md` — the s404 + s405 + s406 asks ALL RULED as one batch (s407, Fable, 2026-08-16): all three sessions APPROVED as shipped (gate independently re-verified 147/5355; sweep RE-RUN clean TOTAL 18517; try/catch, #347, #358, #348 and #362 probed live).  Two REVIEW FIXES landed (`e79f0a6`): a Word after `->` is a METHOD NAME and ends a term — `$o->name x 3` (#361 regression), `$o->w / $o->h / 2` (#351 regression), `$o->w*w()` (#354 hole) were one family; and **#362 CLOSED at its real cause** (`%to-number-raw` had no `functionp` arm — the compared-only side was frozen to a raw numeric slot; NOT `\&NAME` identity).  Rulings: #360 = PPI `custom_feature_include_cb` (core feature pragmas are LANGUAGE; **`use v5.40; try` is a whole-statement DROP today**) + `lib/experimental.pm` shim; **a DROP inside a string eval DIES** (#363, the server discards stderr); string-eval feature inheritance #364; #337 split confirmed; #359 behind the release, fd-3 announces; runner serial re-run #366 + process-group kill #367; anon `__SUB__` dies #368; imported `()`-sub bareword #365.  **Queue: `docs/plan-post-s400.md` §2d** — F=#337 → #360+#364 → #363 → #366+#367 → #342-2 → release leftovers.**
2f. `docs/fable-answers-s402.md` - **s402 review RULED (s403, 2026-08-15, quick — NOT re-verified, by USER instruction)**: s402 APPROVED as shipped.  Blanket `$SIG{__WARN__}` deletion STANDS though its condition was false — the only forms are fix-the-cause or `no warnings 'category'` at the narrowest scope (tree walkers get `'recursion'`; #352).  **General rule: a diagnostic that can fire during a RUN must answer "and on a warm cache?"** (`--module` OFF ratified; modules join the drop census via cached emission).  ir-spec §9.2 keeps discovered-by-stamp.  #351's repair keys on #266's callable classifier, layer `_repair_*`.  **#354 NEW: PPI 1.291 lexes `)*name` as a GLOB — `length($k)*length($k)` dropped whole (Data::Dump:325).**  #355 = one stderr-aware Pl/t helper (drop announcement FAILS the row).  #353 folds into session B.  Queue: B (#345 #349 #350 +#353) → #354+#351 → #355 → plan-post-s400 C–G.

2g. `docs/fable-answers-s400.md` + **`docs/plan-post-s400.md`** - **s399 + s400 asks ALL RULED (s401, 2026-08-15) and THE PLAN FOR THE COMING SESSIONS**: both batches APPROVED as shipped (gate independently re-verified 143/5278; sweep RE-RUN clean TOTAL 18516).  **WHAT-TO-RUN-WHEN table replaces the count rule** (Quick Reference above; #345 implements `--quick`).  Drop family: announce at the DROP site (#339 (b)), census = Option B phase 2's metric, DIE only as phase 2's last step, DROPS as a runner column (#343); file-level lvalue-sub drops keep dropping loudly.  #221 SCHEDULED post-v0.1.  #348 after #346 (first) → #347.  **TWO NEW SILENT-WRONGS from review probes: #349 (the checked-in artifacts RESET `@INC` when they load at first `pack` — closes #217 via `pl2cl --extension`) and #350 (file-top `require` hoisted above a runtime `push @INC`).**  Opus sessions A–G: #339+#343 pieces → #345+#349+#350 → #346+#342 piece 1 → #340 try → #277 installer → #337→#341 → #347→#348 → v0.1 rest.  Fable: #281 design → Option B phase 2 → boxed aggregates.  Five USER decisions still open (plan §4).
2h. `docs/fable-answers-s396.md` - **s395 + s396 asks ALL RULED (s397, 2026-08-15)**: both batches APPROVED as shipped (gate independently re-verified 141/5203 cold; sweep RE-RUN clean TOTAL 18539; ten refaliasing / n-at-a-time probes vs perl identical).  **Orphaned `pl2cl --server` ends ITSELF** (getppid tick, s397a) — the s396 PPID==1 reaper was inert under `systemd --user` (a subreaper adopts orphans); reapers stay as belt keyed on parent-is-a-reaper.  **NO new suite verdict for "measures perl internals"** — one citable not-supported class section instead; op/const-optree.t may register XDIFF.  #323 = own session; refaliasing-first ratified.  **#332 filed** (parenthesised-array refaliasing spellings silent wrong).  Queue: #331 artifacts (opener + staleness row) → #332 → internals registration → #323 → F-D → size try/lexsub/lex.t singles → v0.1; Fable: FOLD chunk 3 — **DONE s398 (Option B phase 1 complete, `docs/DECIDED.md` §s398); next Fable = #281 macro vocabulary → boxed aggregates.**  Fillers filed s398: #333–#336 (all pre-existing silent-wrongs found by the chunk-3 review probes).
2i. `docs/fable-answers-s385.md` - **s385 asks ALL RULED (s386, 2026-08-12)**: both s385 commits APPROVED as shipped (gate independently re-verified 138/5103 cold; eight probes vs perl). **#296 design call RATIFIED** (option (a) rename; do not re-litigate). **#296-B1 RULED: NOT progv** — eval-mode name resolution consults the CAPTURE ALIST before the special table (alist key ⇒ ordinary renamed-lexical path; five-row acceptance table vs perl in task #296). **#296-B2 DIAGNOSED**: sibling same-scope redeclaration — the earlier decl's rewrite region must STOP at a sibling redecl (two-line reproducer; the "not isolated" claim was a by-number TAP join onto the wrong region). **Standing rule (ask 3)**: bucket counts need the row TOTAL in both directions AND a row number is only meaningful within its own run. Shape-assertion expectation-edit conjuncts + sample-rename default (ask 4). #300 unscheduled (ask 5). runpcl/runt blank-line strip fixed (byte-compares were falsified). Queue: #296 finish → #291 → #292 → v0.1 track.
2j. `docs/fable-answers-s378.md` - **s378 asks ALL RULED (s379, 2026-08-09)**: both s378 commits APPROVED as shipped (gate independently re-verified 133/4787 cold; nine resolver probes vs perl). **Absorb-vs-companion rule refined** (zero-new-mechanism + loud + guarded = may absorb). **#287 ruled**: two halves ONE commit (drop $a/$b immunity + sort lowering binds the pair the block reads), after #237. **#237 ruled (b′)**: ONE shared variable-reference event scanner (from StringInterpolation.pm) + intuit_more classifier in the regex consumer only; deleting `_gen_interp_regex_pattern`'s walk is the acceptance bar; #286 NOT folded. Two review fixes shipped (`2af263f`): signature params now bind in the package-switch resolver; keyword-shaped hash keys no longer die. #288 filed (bareword call in switched region falls back to enclosing sub where perl dies). **USER-asked design review: `docs/var-handling-review-s379.md`** — 51% of Parser2.pm is variable identity, 26 interp-scanner sites; directions A (bind-once symbol table) / B (one scanner = #237) / C (one promotion engine) / D (defglobal, measure first); standing rules: no new scanner fixes, no new suffix family, no new scope walk. Queue: #237 → #287 → v0.1 track.
2k. `docs/fable-answers-s376.md` - **s376 asks ALL RULED (s377, 2026-08-09)**: all four s376 commits APPROVED as shipped (gate independently re-verified 133/4785; #276 six-shape probe + #275 import-list-plan spelling byte-identical to perl; #239 reproducer reproduced). **Expectation-rewrite rule is standing** (four conjuncts: perl-probed text, diff = exactly the divergence, edit STRENGTHENS, guard row in the same commit). **`builtin::` BLESSED as the shim-dispatch seam** for box-representation primitives (perl-shared names match perl; `prototype` stays CORE/runtime). **A `lib/` (or `cl/`) change makes the sweep NON-OPTIONAL** — invisible to corpus-diff. **#239 fix shape ruled**: sibling trigger on `_rewrite_var_uses`, four-way resolver from the existing scope walk (lexical incl. in-scope `our` → declaring pkg / magic / qualified / else → X), unclassifiable DIES (s372 gate-SET bar), variables only + probe the in-region CALL shape; NEW probe finding: the bare-block `our`-alias case diverges TODAY in the opposite direction (mandatory guard on #239). **uniqnum = hybrid key, measured 4-of-6** (integer text for |v| < 2^64, else pack "d"; undef→0 output). Two predicates stand (s370 style); filed #285 (no-plan diagnostic, stderr + exit 254) + #286 (ambiguous braces, deferred). Queue: #239 → #237 → v0.1 track.
2l. `docs/fable-answers-s374.md` - **s374 asks ALL RULED (s375, 2026-08-09)**: all four s374 commits APPROVED as shipped (gate independently re-verified 133/4773; #236 renderer byte-identical to perl, #234 four-shape probe identical, Ask-2 divergence probed PRE-EXISTING at f44e947). **#266's three-valued asymmetry IS the ruling** (`no` keeps answering CALL; no completion campaign — interleave on cause lines); the ask's mutual-recursion residue does NOT reproduce (probed). Package-blind unqualified path accepted unscheduled (re-raise like #191). Comment-linked interpolation twin stands. **Cadence ruled**: corpus-diff-identical + lib-reach byte-compare + green gate ⇒ same-session second sweep OPTIONAL. Queue: #275 → #276 → #238 → #239, then #237; FOLD (#153) begun s375 (Fable).
2m. `docs/fable-answers-s372.md` - **s372 asks ALL RULED (s373, 2026-08-09)**: #274 + #272 APPROVED as shipped (gate independently re-verified 132/4747; four block-boundary probes vs perl all match). **Ask 2 blessed as a standing narrowing**: a decline→die-only edit may meet the gate-SET bar with a both-populations die-scan + corpus-diff + sweep TOTAL/LOST (all three legs required). #269 park behind #196 confirmed; #271 ruled BEHIND #153's FOLD (its `pipe my ($r,$w)` shape joins the FOLD's acceptance probes); #272's map/sort/grep-block boundary probed costless, no residue. Queue: #266, then #236→#234→#235; FOLD (#153) stays Fable's.
2n. `docs/fable-answers-s370.md` - **s370 asks ALL RULED (s371, 2026-08-09)**: #267 APPROVED as shipped (gate independently re-verified 132/4744; ten probes vs perl identical incl. same-scalar-twice, k=2 vivification, named loop var). **Ask 1**: probes + guard rows ARE the bar when a shape occurs in no corpus — do not widen; corpus-diff-first ratified. **Ask 2**: the k=1 anchor-miss must DIE like k>1 (always a compiler self-inconsistency) — own filler commit, two-population gate SET + sweep TOTAL/LOST first (task #274). **Ask 3**: two comma walks CONFIRMED (provable agreement on qualifying lists; the veto is a superset); paired invariant comments required, no refactor. **§5**: mixed-list residue corrected — `@a`'s elements DO alias through the flattener; the miss is element-shaped slots + `values`. Queue: #269 → #274 → #272, #271 sizing, #266, #236→#234→#235; FOLD (#153) stays Fable's.
2o. `docs/fable-answers-s368.md` - **s368 asks ALL RULED (s369, 2026-08-09)**: s367 (#270) + s368 (#265 rename half) APPROVED as shipped (gate independently re-verified 132/4739 cold, all probes match perl). **#267 sizing decided**: the N=1 rule IS the N=k rule — all-single-scalar multi-element foreach lists emit `(vector E1 … Ek)` (shared #138 splitter, boxes never through `p-flatten-args`, mixed lists stay flattened boxless); TWO commits — wrapper switch first with its own ZERO-change full sweep, then the per-element box verdict; sweep TOTAL/LOST required. #269 do-not-delete confirmed, measure reachable rows first. #271 argument-run-lowering layer endorsed. **NEW residue #272** (review probe, pre-existing): embedded `my` inside an ANON sub body still veto-refused → global read → crash; condition should be "inside ANY sub body". Queue: #267 → #269 → fillers (#272, #271 sizing, #266, #236→#234→#235); FOLD (#153) stays Fable's.
2p. `docs/fable-answers-s365.md` - **s365 asks ALL RULED (s366, 2026-08-09)**: all eight s365 commits approved (gate independently re-verified 132/4737, every semantic ask probed live vs perl). **#254 + #252 CLOSED**; A-i's extent design STRUCK (s363 premise superseded); timeout registry ratified, NO blind suite retry; "one capture test, promoter = gate" standing; filler-scope rule (same mechanism + gate-SET measured + new axes filed). **NEW BUG #270 found by review probes**: `sub :prototype($) {…}` at expr start = SILENT DROP (`$)` lexes as magic var, block swallowed, repair declines silently) — decline must DIE + repair extends. Opus queue: #270 → #265 rename half (shape approved) → #267 (size first) → #269 (probe first) → fillers. #153 FOLD stays Fable's.
2q. `docs/fable-answers-s357.md` - **s357 APPROVED + the two E4.1-aftermath asks USER-decided (s358, 2026-08-08): #252 = SPLIT** — phase 1 (Text::Balanced) shipped s358: general forward-goto (nested catches, defer-decls-to-my-nesting), the `_reads_name_rx` false-self-ref fix, interp subscript-chain continuation; board 933 ok vs v1-era 780.  Phase 2 = `docs/e41-suite-families-plan.md` (#254, awaiting scope approval).  **#243 = PORT the 27 Pl/t callers (#255), then delete.**  Ratified: the audit populations are FOUR (sweep + board + Pl/t + perl suite) and live-v1-style audits run on a COLD cache.
2r. `docs/fable-answers-s352.md` - **s352 ask RULED (s353, 2026-08-07): the E4.1 flip stays HELD; the gate's 27 v1 routes are ALL pre-work, option (b+).**  Measurement independently reproduced event-for-event (gate green 131/4652).  Fix order M3→M6→M2→M1→M4→M5 (tasks #247/#248), re-running the instrumented gate after each family; step-2 precondition = zero TODO events across sweep + board + Pl/t gate (§5a.2 amended — s342c never measured Pl/t).  M1 = whitelist leading-`my` predicate on #226's collapse (Sub::Quote, 15/15 events verified zero-free-name); M2 = braced-interpolation `"${name}"` fix + PPI-narrowed blocker (the two Moo hits are string interpolation, probed); M5 (static-variable idiom) = one-session cap + stop-rule, only a fired stop-rule reopens #153-first.  s345 §2's "ZERO events" premise SUPERSEDED for the leading-statements shape; steps 1 + F8 approved as shipped.
2s. `docs/eval-region-measurements-s350.md` - **#240 step 2 SHIPPED s351, #240 CLOSED; ALL E4.1 pre-work is done.**  `p-eval-thunk` takes an optional region-package designator (emitted only by the #226 eval collapse) and binds `*package*` to it around the free-name resolution AND the body — so every "unqualified → current package" resolution in the runtime answers X, as perl says inside a `package X;` eval region.  All three spellings of the silent-wrong closed; the step-1 gate + Cast+Block arm and all three s350 instruments DELETED; the thunk is now emitted whenever a region is present even with no free names.  **E4.1 step 2's refusal-rephrase list is down to multi-switch + F6.**  This file holds the measurements the fix was sized against (§2b board numbers, §3 the 13-site `*package*` survey, §6 the forced-wrap probe); the ruling is `docs/fable-answers-s348.md` §2c.
2t. `docs/fable-answers-s348.md` - **s348 ask RULED (s349, 2026-08-07)**: s348 approved as shipped (gate independently re-verified 131/4648, all probes + routing reproduced). Wider #240 hole gets NO interim gate — **step 2 RE-SCOPED to the runtime route and PROMOTED pre-flip** (the s347 parking reason doesn't apply): bind `*package*` to X's CL package around `p-eval-thunk`'s free-name resolution AND body (NOT a `p-eval-lex-lookup`-only patch — that regresses the symref-without-`our` case, probed). Measurement first (miss-path instrumentation piggybacked on #230's audited sweep + the ~10-site `*package*` survey), stop-rule = (ii)-as-gate die-at-the-miss, one-session cap. Step-1 gate + arm deleted in the step-2 commit. Queue: #230/F6 (+instrumentation) → #240 step 2 → E4.1 steps 1–4 → STOP → Fable #153/E5.0.
2u. `docs/fable-answers-s346.md` - **s346 asks ALL RULED (s347, 2026-08-06)**: all four s346 commits approved (gate independently re-verified 131/4640). Gate-the-hole policy CONFIRMED as a standing rule; **#240 SPLIT** — step 1 PRE-FLIP (narrow the eval-region `our` gate to declare-then-use; the write-only `$VERSION`/`@ISA` idiom must pass, probed s347) + the file-mode `sub { package X; use M; }` guard row (a real fix #226 shipped unclaimed, verified live) in the same commit; step 2 (two-half emitter fix) = first post-E4.1 compiler item or with E5.4. **§5a.3 AMENDED**: zero UNEXPLAINED eval-mode fallbacks — ruled refusals excepted, perl-shaped + not-supported entries naming their owner in the step-2 commit. **F6 re-scope ACCEPTED**: locate first; top-level-`my` shape → extend `_oversized_top_decls`; eval/fresh_perl source → no pre-flip fix, ruled refusal. Queue: #240 step 1 → #230/F6 → E4.1 steps 1–4 → STOP → Fable #153/E5.0.
2v. `docs/fable-answers-s339.md` - **s339 asks ALL RULED (s340, 2026-08-03)**: both s339 commits approved as shipped (atomic cache write = the PRIMARY fix, warm-first the belt); XS announce-not-die ratified whole-file, NO die-across-the-boundary mechanism; getproto* approved with the fallback-host divergence accepted; stale baselines → task #223 (fail-baseline rows leave by EDIT, pass-baseline re-blesses only gate-green after a per-file audit, `# taken-at:` stamp). Queue: #223 → E4.1 (Opus, 1–2 sessions) → STOP, hand to Fable for #153/E5.0 steps 1–2.
2w. `docs/fable-answers-s337.md` - **s337 asks ALL RULED (s338, 2026-08-03)**: s337's three commits approved as shipped; #152 closes at the runtime (one bounded xs-grep extension, wider net REJECTED); closed-handle value → #220 (behind #153/E4.1); getprotobyname via /etc/protocols; warnings model → #221 UNSCHEDULED (default-off diagnostics stay absent, never unconditional); tie warning folds into the shared announce helper; #215 gains warm-first; near-green queue = standing filler + utf8::encode probe. Next-session batch = task #222, then E4.1, then #153/E5.0 (Fable steps 1–2).
2x. `docs/fable-answers-s334.md` - **s333+s334 asks ALL RULED (s335, 2026-08-03)**: no `ref-kind` slot CONFIRMED (s318 tag ruling superseded); ref strings uncached ACCEPTED; #211 parked behind #153 (leniency covers only the two measured spellings); sweep gate vs load noise = MemAvailable note + serial re-run of a LOST file, serial verdict replaces, report shows both (#215); #213 re-scoped — its TIME was `_ends_in_comment` (fixed s335 byte-identically, with the two W10 per-token `_ref_shadowed` loops), remaining = clamp-indent (a) cosmetic + `let*` runs (b) at E5, (c) REJECTED; "suspect X" tasks carry the discriminating measurement
2y. `docs/fable-answers-s331.md` - **s330+s331 asks ALL RULED (s332, 2026-08-03)**: harness `not ok`-not-die RATIFIED (#152 inherits the harness/runtime split); TAP-interleave hard rule; like-string-pattern = accepted divergence; UNSTABLE/LOST independent, no promotion; writes_args no-whitelist + refinement not scheduled; vivification rule probe-confirmed; #163 order = third path → tag → probes; §9 = the four probe-found writes_args false negatives (implicit-`$_` spellings) fixed s332
2z. `docs/fable-answers-s328.md` - **s328 asks ALL RULED (s329, 2026-08-02)**: rule-12 DIE-vs-ANNOUNCE = value-flows-onward test (gates #152); #201 File::Temp = probe the predicate, layer follows from who diverges; #202 first, then #204 (sweep TOTAL gate / LOST bucket), then #189; s329 review fixes: goto restores caller *wantarray*, veto exemption requires no free reference (residual → #205)
2a. `docs/fable-answers-s323.md` - **s323 asks ALL RULED (2026-08-02), incl. the ordered pre-/post-R1 plan (§7)**: #189 approved-with-amendments, POST-R1, warning stays as backstop; #193 principle-9 reading CONFIRMED, pre-R1 attempt authorized with the #142 stop-rule; #191 deferred (loud, rare); snapshot rule = stale-marker at crash-fix time + regenerate at quote points; USER: R1 CPAN half = four-dist baseline only, dist fetches blanket-OK'd, **full suite run every 3rd–5th change, not per change**.
2b. `docs/fable-answers-s321.md` - s321 asks ALL RULED (2026-08-02): #176 → (c) measurement fix now / bless post-R1; #177 → tainted registrations re-verify before R1 (backlog measured empty), per-row claims quote descriptions; XDIFF rows column approved post-R1. Previous rounds: `fable-answers-s318.md`, `fable-answers-s316v.md`
3. `docs/CODEGEN_DESIGN.md` - Code generation design notes

Not relevant now:
1. `docs/history/XS_BRIDGE_DESIGN.md` - superseded XS sketch; the active design is `docs/xs-shim-design.md` (libperlshim + host vtable, ready for implementation)
2. `docs/MOO_MOOSE_DESIGN.md` - Future Moo/Moose OO framework support plan
2. `docs/history/SESSION_SUMMARY.md` - Older detailed session history, use `docs/session-log.md` now



---

# Part 3 -- V1-era notes

## Architecture

### Core Pipeline

```
Perl Source → PPI → PExpr (AST) → ExprToCL → Common Lisp
                      ↓
                 Environment (constants, packages, prototypes)
```

### Key Modules

| Module | Purpose |
|--------|---------|
| `Pl/Parser.pm` | Statement-level parser. Entry: `parse()` |
| `Pl/PExpr.pm` | Expression parser. Entry: `parse_expr_to_tree()` |
| `Pl/ExprToCL.pm` | Code generator. Entry: `generate()` |
| `Pl/Environment.pm` | Tracks constants, prototypes, package stack |
| `Pl/OpcodeTree.pm` | AST node storage (ID-based) |

### Parser Components (Pl/PExpr/)

| File | Purpose |
|------|---------|
| `Config.pm` | Operator precedence (1-92), function param specs |
| `StringInterpolation.pm` | `"$var"` string parsing |
| `TokenUtils.pm` | Token classification helpers |

## What's Implemented

### Expressions
- All operators with correct precedence
- Ternary `? :`
- Method calls `$obj->method()`
- Array/hash access `$a[0]`, `$h{key}`
- References `\$x`, `$$ref`
- Regex `s///`, `tr///`

### Statements
- `if`/`elsif`/`else`/`unless`
- `while`/`until`
- `for`/`foreach` (both C-style and list)
- `sub` with signatures and defaults
- `package` with block scoping
- `use constant`
- `try`/`catch`/`finally` (perl 5.34 `use feature 'try'`; task #340, s405) —
  `p-try`, semantics in `docs/ir-spec.md` §6.3.  `use experimental 'try'`
  also works (task #360, completed; the "does NOT work" note that stood here
  was stale — probed working s453).

### OO Support
- `bless`, `ref`
- Method calls: `$obj->method()`, `Class->new()`
- Inheritance via `@ISA` with C3 MRO (CLOS-based)
- `SUPER::method()` calls
- Package stack (3+ levels)
- Multiple inheritance / diamond inheritance

## Code Generation Patterns

```perl
# Assignment
my $x = 1;           → (pl-setf $x 1)

# Operators
$a + $b              → (pl-+ $a $b)
$x++                 → (pl-++-post $x)

# Function calls
length($s)           → (pl-length $s)
print "hi"           → (pl-print "hi")
print STDERR "err"   → (pl-print :fh STDERR "err")

# Control flow
if ($x) { }          → (pl-if $x (progn ...))
while ($x) { }       → (pl-while $x ...)
for (...) { }        → (pl-for (init) (cond) (incr) ...)
foreach $x (@a) { }  → (pl-foreach ($x @a) ...)

# Subroutines (wrapped in eval-when via pl-sub for BEGIN visibility)
sub foo($x) { }      → (pl-sub pl-foo ($x) ...)

# Constants
use constant PI => 3.14  → (defconstant +PI+ 3.14)
PI                       → +PI+

# OO
bless {}, "Class"    → (pl-bless (pl-hash) "Class")
$obj->method()       → (pl-method-call $obj 'method)
```

## Config.pm Parameter Specs

```perl
# In known_no_of_params hash:
func => 2           # Exactly 2 params
func => [1, 2]      # 1 or 2 params
func => -1          # List (variable)
func => -2          # Defaults to $_ if no args
func => -3          # Defaults to @_ if no args
func => -12         # 1 param before list
```

## Common Pitfalls

### Runtime Symbols Must Be Exported

When adding new special variables or functions to `pcl-runtime.lisp`, they must be exported from the `pcl` package. Generated code runs in user packages (e.g., `|File::Basename|`) that `(:use :pcl)`, so unexported symbols won't be visible.

Example: `*wantarray*` must be in the `:export` list, otherwise `(let ((*wantarray* t)) ...)` in a user package creates a different variable.

```lisp
;; In defpackage :pcl
(:export
 ...
 #:*wantarray*   ; Context variable - MUST be exported
 ...)
```

## Common Tasks

### Adding a new operator
1. Add to `Pl/PExpr/Config.pm` `%precedences`
2. If special handling needed, update `Pl/ExprToCL.pm`

### Adding a built-in function
1. Add to `Pl/PExpr/Config.pm` `known_no_of_params`
2. Code gen usually automatic via `pl-funcname`

### Adding a statement type
1. Add case in `Pl/Parser.pm` `_process_element()`
2. Create `_process_X_statement()` method

(v2, which is the only pipeline: a COMPOUND statement gets an arm in
`Pl/Parser2.pm`'s `_lower_compound`, and its runtime shape a macro in
`cl/pcl-runtime.lisp` — exported, or generated code cannot see it.  Worked
example, s405: `try`/`catch`/`finally` = one arm + `p-try` + a PPI repair,
because PPI leaves `finally` out of the statement it builds.)

## TODOs

### `&$foo(args)` / `&{expr}(args)` — Code Ref Call Syntax ✅ DONE (session 62)
`&$scalar(args)` and `&{expr}(args)` now generate `(pl-funcall-ref ...)` correctly.
`grep.t` fully passing (7/7). `closure.t` tests 1-7 pass; tests 8+ need Phase 2 closures.

### Phase 2 Closures — `defvar` + `let` = dynamic binding problem ✅ DONE (session 63)

**Status (session 63):** `_vars_referenced_in_closures` added to Parser.pm. `_with_declarations`
now renames captured `my` vars to `$i__lex__N` when `in_subroutine > 0`. `_process_variable_statement`
splits RHS parsing for renamed vars (handles `my $i = $i + 1` shadowing). `closure.t` 38→42/50.

**Remaining 8 failures** = `for my $n (0..4) { sub { $n } }` (foreach loop variable capture).
This requires `pl-foreach` macro changes to create a new binding per iteration — out of scope for now.

**KEY BUG to remember:** PPI's `find` returns `0` (not `undef`) when nothing found.
Always use `|| []` not `// []` when dereferencing results of `$elem->find(...)`.

**New test:** `Pl/t/closure-01.t` (8 tests, all passing).

### `map({key=>$_}, LIST)` — Hash Constructor Block in Paren-Form Map ✅ DONE (session 62)
`_block_is_hash_constructor()` added to PExpr.pm; `parse_hash_block_to_cl_string()` added to Parser.pm.
Both paren-form and block-form map/grep/sort now generate correct `(make-pl-box (pl-hash ...))`.
`grep.t` fully passing (7/7).

### Chained Method Calls
`$obj->method1()->method2()` fails — the parser emits a PARSE ERROR for the second `->` when the left-hand side is a method call result (not a simple variable). Example: `B->new()->name()`. Workaround: assign to a temp variable first. Needs investigation in `Pl/PExpr.pm` where postfix `->` is handled after a complete expression.

### Perl's Own Test Suite
Extract tests from Perl's source distribution (`t/` directory) to verify PCL.
Perl uses these to verify new Perl builds work correctly - they cover edge cases
and expected behavior comprehensively. Start with:
- `t/op/` - operator tests
- `t/base/` - basic functionality
- `t/uni/` - unicode (later)

These would provide authoritative verification of Perl semantics.

### Suggested Workflow: `perl-tests/` Failures → `Pl/t/` Tests

When investigating a failing `perl-tests/foo.t` file, consider first creating a
focused `Pl/t/foo-01.t` file that reproduces the specific failure modes as small,
targeted tests. Benefits:

- Faster iteration — no need to re-run the full 200-test file on every attempt
- Self-documenting — the test file records *what* fails and *why* at the unit level
- Regression protection — once fixed, the `Pl/t/` test prevents that bug returning
- Easier diagnosis — smaller test cases isolate whether the issue is codegen or runtime

**Pattern:**
1. Run `perl tools/sweep-perl-tests.pl --jobs 1 perl-tests/foo.t` to see the failure count
2. Inspect the generated CL (`./pl2cl < perl-tests/foo.t > /tmp/foo.lisp`) for wrong output
3. Write `Pl/t/foo-01.t` with:
   - Transpilation tests (`like($cl, qr/expected-pattern/, 'desc')`) for codegen bugs
   - Runtime tests using the `run_cl()`/`test_cl()` pattern for semantic bugs
4. Fix the code against the `Pl/t/` tests, then verify the sweep count improves

See `Pl/t/sort-01.t` for an example (created session 93, documents sort.t failures).
