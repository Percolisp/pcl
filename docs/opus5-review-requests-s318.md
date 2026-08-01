# Review requests for Fable — from Opus 5, s318 (2026-08-01)

*Written at gen v2-91, gate 123/4452, sweep 0-new vs the re-blessed 698
baseline, census 111/111 v2-native.  Seven commits landed, each verified by
the full quadruple; the tree is shippable.  Nothing here blocks R1.*

*Session context: I worked the W1 queue from `v2-opus5-execution-plan.md` §4
in order — #142, then #154 (which #142 exposed), #151, #150 part 1, #149,
and the first near-green family.  The asks below are what I hit that I
should not decide alone.*

**Index — what each section wants from you**

| § | topic | the ask |
|---|---|---|
| 1 | the box has no referent-kind word (#154 residue, #155, #159) | **design: one model, or three point fixes?** |
| 2 | read-only aggregates (#159) | **design: which of three, given the `push` hot path?** |
| 3 | #149 in practice — it is per-ROW, not per-file | policy: confirm the strict reading + is the rest worth the sessions? |
| 4 | `do subname()` executes (#158) | design: confirm the fix layer (parser, not runtime) |
| 5 | avhv.t t13/t39 pass in isolation, fail in file (#156) | priority: worth chasing, or park? |
| 6 | the suite tsv is now incomplete by construction (#150) | process: when to regenerate, and does it gate R1? |
| 7 | what this queue taught about the queue itself | priority: the near-green list is mostly silent-wrong |

---

## 1. Three of this session's findings are the same missing thing: the box
##    has nowhere to record what KIND of referent it holds

This is the one I most want your read, because I hit it three times from
three directions and each time the local fix was worse than the gap.

**The evidence, in the order it appeared.**

1. **#154.** `$aryref->{k}` must die `Not a HASH reference`.  I got 12 of 14
   shapes matching perl by guarding the deref funnels.  Two shapes I could
   NOT do: `$$aryref` (must be `Not a SCALAR reference`) and
   `$scalarref->{k}` (must be `Not a HASH reference`).  Both fail for the
   same reason — the guard has to ask "is this remaining `p-box` a scalar
   ref, or a representation layer?" and **there is no way to tell**.  PCL's
   `\%h` is a DOUBLE box, so `unbox` peeling one layer legitimately leaves a
   box; `p-ensure-hashref` returns it and `(setf p-gethash)` unboxes it
   again.  I learned this the hard way: counting a box as a mismatch broke
   `$refref->{k} = $v` and cost me a gate + sweep cycle.
2. **#155.** `tie %h, 'Tie::StdHash'` is a SILENT no-op.  `p-tie` starts
   `(unless (p-box-p box) (return *p-undef*))`, and an aggregate arrives raw
   (a hash-table / vector), so the tie is dropped and the program runs on the
   untied aggregate.  Scalar tie is correct end to end.  The runtime says so
   itself: *"Phase 1: scalars only.  Arrays/hashes require boxing those types
   first."*
3. **#159.** `Internals::SvREADONLY(@a, 1)` reports success and does nothing,
   because a raw CL vector has nowhere to hang the flag.

Three symptoms, one cause: **scalars carry a box (with `sv-ok`, `nv-ok`,
`class`, `is-ref` …) and aggregates carry nothing.**  Every per-site
workaround I considered was either a hot-path cost or a lie.

**The ask.**  Is there a model you want here, or should these stay three
separate blessed gaps?  What I can see:

- **(A) A referent-kind tag on the box.**  Cheap for #154's two shapes (one
  more slot, written at refgen, read only on the already-failed path).  Does
  nothing for #155/#159, which need the AGGREGATE to carry state.
- **(B) Boxed aggregates** — the thing the `p-tie` comment anticipates.
  Solves all three, and is the biggest change in the runtime's data model; I
  would not start it without your design.  It also lands squarely on Target A
  (every array/hash access grows an indirection).
- **(C) A side table** (weak EQ hash) keyed by aggregate, holding tie-proxy +
  read-only + whatever else.  No representation change, no cost on READS,
  but a probe on every mutation (`push`/`STORE`/…) — see §2 for why that
  worries me.
- **(D) Leave all three as documented gaps.**  Honest, but #155 and #159 are
  currently SILENT, which is the failure mode CLAUDE.md rule 12 exists to
  stop; at minimum they should announce themselves.

My instinct is (D)+loud for R1, and (B) as a real E5-era design item — but
the data-model question is yours, and I have not written any of it.

---

## 2. #159 read-only aggregates: the three fixes cost different things, and
##    the cheap one changes array representation

Concretely, from three near-green files that share ONE mechanism:

```
op/push.t t32   "croak when pushing onto readonly array"
op/splice.t t33 "croak when splicing into readonly array"
op/unshift.t t19 "croak when unshifting onto readonly array"

my @a = (10,11); Internals::SvREADONLY(@a, 1); eval { push @a, 9 };
perl: $@ = "Modification of a read-only value attempted", @a unchanged
PCL : SvREADONLY succeeds, push SUCCEEDS, @a = 10 11 9      (probe s317/ro.pl)
```

- **(a) weak-hash probe in `p-push`/`p-unshift`/`p-splice`.**  A `gethash`
  per push.  push is ~50-100ns, a weak-hash probe ~20-30ns — order 30% on a
  very common op.  Against Target A; I would not ship this without you
  saying the correctness is worth it.
- **(b) `SvREADONLY` swaps the storage for a SIMPLE vector** (no fill
  pointer, not adjustable), so `vector-push-extend` fails BY CONSTRUCTION.
  **Zero hot-path cost**, and it converts a silent-wrong into a real die.
  Costs: the message is SBCL's, not perl's (which would then make these three
  rows legitimate #149 material); anything assuming adjustability must be
  audited; `SvREADONLY(@a, 0)` has to restore.
- **(c) bless as not-supported** — extend `not-supported.md`'s "Read-only
  constants" section, which today covers only `\undef` stash tricks on
  SCALARS, and register the three rows.  Needs sign-off (CLAUDE.md 4), and it
  is NOT #149 material as it stands: the assertion is "must die at all", not
  "must say X".

**I recommend (b)** — it is the only one that keeps `push` fast while making
the behaviour right.  But it changes how a read-only array is represented, so
I want your call before implementing.

---

## 3. #149 in practice: the category is approved, the APPLICATION is per-ROW,
##    and the first file proved why

You approved the blanket category in §6d and I have landed the first
instalment (a83a99a).  Two things worth your eyes.

**do.t looked like 4 clean rows and was 6.**  t64/66/67/68 are pure
rejection assertions (`like $@, qr/\Asyntax error/`, do.t:280) — registered.
But the same family has t63/t65, "do subname(arg) called" / "do subname()
called": the file's `fail()` guards, and they fire because **PCL actually
EXECUTES** the form perl removed in 5.20 (§4).  Registering those under an
approved category would have buried a real behavioural divergence.  I left
them failing.

**Ask (a):** confirm the reading I applied — *a row that asserts a
side-effect did not happen is never error-text material, even when it sits
inside an otherwise-qualifying family.*  I think that is what §6d's "a test
that also checks a value never qualifies" means, extended to "checks a
behaviour".

**Ask (b):** is the rest of #149 worth the sessions?  A scan of the 698-row
baseline for `/error|die|Can't|syntax|invalid|Missing/` returns ~45 rows, but
most check a VALUE or behaviour, not text — eval.t's "No segfault inside
sort …" rows are behaviour; "eval syntax error in list context" checks the
CONTEXT of the return.  Each needs the test source read.  My estimate is
several sessions for pure bookkeeping.  I would rather interleave it than
run it as a campaign; say if you disagree.

---

## 4. #158: PCL executes `do subname()` — fix in the parser, not the runtime?

```
sub subname { print "CALLED\n" }
eval 'do subname("arg")';
perl: $@ = 'syntax error … near "do subname("'   (removed in 5.20)
PCL : prints CALLED, $@ empty                    (probe s317/do1.pl)
```

Perl's own message is not required (your 2026-07-28 ruling); **not executing
the sub is the point**.  So the fix is to stop accepting the form where it is
parsed, and the layer question is whether that is the `do` statement handler
in `Pl/Parser.pm` or the `do` term in PExpr.

**Ask:** confirm parser-layer, and — more importantly — confirm this is NOT
in the `$end_pars` region you fenced off in `pexpr-term-parsing-review.md`.
I have not touched it pending your answer.  If it IS in that region, this
becomes another E5.0 consumer rather than a W1 item.

---

## 5. #156: two avhv.t rows behave differently in-file than in isolation

After #154, `t/op/avhv.t` is **38 pass / 2 fail** (was 0/40).  The two
survivors:

```
t13 (avhv.t:130)  eval { my $slice = join('', 'x', @$a{'abc','def'}, 'x') };
t39 (avhv.t:275)  eval { (%$avhv, @extra) = (foo => 42, …) };
```

Both must set `$@` to `/^Not a HASH reference /`, and **both do exactly that
when I probe the expressions standalone** (scratchpad s317/av3.pl matches
perl).  Inside the file they do not.  Candidates I did not chase: the
container is a TIED array at that point (and tie-on-aggregate is #155's
no-op, so it may not be what the test thinks it is); t39's LHS is a
list-assignment to `%$avhv` which may never reach `p-cast-%`; t13 wraps the
slice in `join` inside `eval`, so context/flatten may differ.

**Ask:** worth a session, or park?  It is 2 rows, but "same expression,
different answer depending on surrounding code" is the kind of thing that
usually means a context/lowering bug with a wider blast radius than the rows
suggest.  That is why I am asking rather than parking it myself.

---

## 6. #150 part 1 landed — the checked-in suite tsv is now incomplete by
##    construction, and I did not regenerate it

Dropping the copied-file skip made **91 files** visible that the runner had
been skipping because `perl-tests/` had a copy (op 82, base 5, opbasic 3,
io 1).  It was hiding real failures — against copies the sweep calls clean:

```
op/tr.t    P:317/0  C:270/46      op/state.t  P:166/0  C:125/37
io/scalar.t P:128/0 C: 94/34      op/chop.t   P:148/0  C: 96/4
```
(re-verified at `--jobs 1` on a quiet machine: identical, so not artefacts of
the loaded run — see §7.)

88 of the 90 runnable ones: **29 OK, 59 DIFF**, with **16 near-green files
≤4 rows off perl**, listed in #150 as the worklist.

**`docs/perl-suite-run.tsv` predates this and has no rows for those 91** —
incomplete, not wrong.  A full re-run grows it by ~91 rows.

**Ask:** does that regeneration gate R1, or is it E4.1-time work?  It wants
per-dir FOREGROUND chunks (the 10-min background cap kills `--all` before it
writes `--tsv`), and on this machine it wants `--jobs 2-4` (§7).  I did not
start it because a partial regeneration is worse than none.

---

## 7. What the queue taught about the queue

Not an ask, a data point for how you order the remaining W1/W2.5 work.

**Four of the six items I worked were SILENT-wrong, not the visible failure
the task described.**  Deref answering `undef` where perl dies; `tie` on an
aggregate doing nothing; `SvREADONLY` reporting success and doing nothing;
`localtime` ignoring `$ENV{TZ}` *while a code comment claimed it handled TZ
env vars*.  In each case the suite row was the symptom and the real gap was
"programs get wrong answers with no error".

That suggests the near-green list is a better bug-finder than its row counts
imply, and it is why I would rather spend the remaining W1 window there than
on #149 bookkeeping.

**Two mechanical notes for the record:**

- The laptop was short on memory during the big runs (user).  An OOM-killed
  SBCL emits PARTIAL TAP, which the runner scores as a normal DIFF with
  depressed counts — so any numbers taken from a loaded run need re-checking
  before they are believed.  I re-ran the four headline files at `--jobs 1`;
  they reproduce exactly.  Separately the runner ends such a run with **no
  row, no summary, exit 0** (#157) — indistinguishable from a run never
  requested.  I plan to fix that with an END block that prints a KILLED row
  for everything still queued and exits nonzero; say if you want a different
  shape.
- I put an untested runtime into a commit by `cp`-ing a pre-fix snapshot back
  over the live file to undo a HEAD comparison, then quoting the fixed
  version's numbers in the message.  Caught it re-running avhv.t; amended
  after full re-verification (1c9148e → 2d70df3).  Rule now in memory and the
  session log: compare with `git show HEAD:file` or a worktree, never
  overwrite the working file.

---

## 8. (added s319) op/list.t eats 10 GB in SBCL — an ordinary 277-line file,
##    and the transpiler is innocent.  Two t/op files are unmeasured.

I fixed #157 first (commit e6f1277) — the reporting shape you were asked
about above, plus two defects I did not expect: a forked WORKER inherited the
parent's `$SIG{TERM}` handler and END blocks, so one signalled worker
`rm -rf`'d the SHARED tmpdir and unlinked the shared core, killing the run;
and `system()` in an END block overwrites `$?`, which after the last END block
IS the exit status — so the tool's documented "Exit: nonzero iff any DIFF/…"
contract **had never held for any run**, crash or not.  Both proven by probe
and by a HEAD counterfactual; details in the session log.

Then I used the fixed runner on the two files that have never produced a row,
and it reported honestly instead of vanishing.  systemd's own log for the run:

```
run-p12204-i1223.scope: The kernel OOM killer killed some processes in this unit
run-p12204-i1223.scope: Failed with result 'oom-kill'
Consumed 53.318s CPU over 53.463s wall, 10G memory peak, 1G swap peak
```

`--jobs 1`, 9 GB free, idle machine.  So s318's reading ("the laptop was
short on memory") was **wrong**: one file eats a 10 GB cgroup in 53 seconds.

**The transpiler is not the problem** — `pl2cl --no-cache --lenient-ppi
op/list.t` is 1.23 s and **64 MB** max RSS, emitting 564 lines of CL.  The
blowup is entirely SBCL-side (compile or run — not yet split; the user
time-boxed this and asked that these two files not be run again for now).
And t/op/list.t is unremarkable: 277 lines, longest line 85 chars,
`plan(tests => 73)`, plain list-assign/slice/split rows — nothing like
op/cond.t's 20k-nested ternary, which is the one file already on the %HEAVY
solo list.  So this looks like a scaling pathology reachable from **ordinary
Perl**, which is what makes me want your eyes on it.

**The asks.**
1. Priority: does this gate R1?  It is two unmeasured files in the t/op
   release signal (op/pack.t is NOT-RUN purely because it was queued behind
   list.t), against a compiler that is otherwise 111/111 and 0-new.
2. Where would you look first — compile-time (a macro or type-inference
   blowup on some form in the 564 lines) or run-time (an allocation
   pathology)?  I can bisect either way, but a guess from you about which
   family of forms to suspect would save a session.
3. Should `op/list.t` join `%HEAVY` in the runner meanwhile, so one
   pathological file cannot OOM a whole run?  That is a measurement-hygiene
   patch, not a fix, and I would rather you bless it than have me quietly
   hide the file.

Tracked as task #160 with the reproduction commands and a hard-capped probe
recipe (`systemd-run --user --scope -p MemoryMax=3G -p MemorySwapMax=0`) so
no future probe can take the desktop down.

---

## 9. (added s319) §1 has a third symptom, and it is the cleanest one yet:
##    a reference's identity is its WRAPPER's, not its referent's

While clearing the near-green queue I hit your §1 question from a fourth
direction, and this one needs no interpretation at all.  Perl's rule is that a
ref's address IS the referent's address, so two refs to the same element are
`==` and stringify identically:

```
                          perl                      PCL
\$h{k} twice              SCALAR(0x5f4d81a1a3b8)    SCALAR(0x1) / SCALAR(0x2)
                          (equal)                   (NOT equal)   <-- op/delete.t t26
\$a[0] twice              SCALAR(0x…) equal         REF(0x3) / REF(0x3)
                                                    equal, but WRONG TYPE
\(values %h) vs \$h{k}    equal                     not equal
```

**The cause is read from the source, not inferred.**  `object-address`
(runtime:1521) is a deliberate EQ-serial table, stable across GC — correct.
`p-aref-box`/`p-gethash-box` both store a box into the container and return
that same box every call — correct and symmetric.  The break is that
`p-backslash` (runtime:11627) allocates a **fresh wrapper box per `\`**, and
the stringify/numify site (runtime:1799) takes `object-address` of that
**wrapper**.  The array case only looks right because it lands in the deeper
branch at ~1886 that unwraps one more level — and that same level-counting is
why `\$a[0]` prints `REF` where perl prints `SCALAR`.

**Why this is your call and not mine.**  The obvious patch — "if the wrapper is
`is-ref` and holds a `p-box`, use the inner box's address" — is precisely the
move that cost s318 a gate+sweep cycle in #154: a remaining `p-box` does *not*
distinguish a scalar-ref from a representation layer, because `\%h` is a double
box.  And identity and printed TYPE are decided by the same level count, so
they cannot be fixed independently without regressing the type.  This is §1's
model question with a sharper edge: **the wrapper needs to know what level its
referent is at** — which is the referent-kind word §1 is about.

**The ask.**  Does this change your preference among §1's (A)–(D)?  It is the
first of the four symptoms where a referent-kind tag on the box (option A)
would be sufficient on its own — #154's two shapes, #155 and #159 all wanted
something else, but ref identity is purely a "which level is the referent"
question.  If (A) is worth doing for R1, this row pair is its cheapest proof.

Tracked as task #163.  Not registered as an expected divergence: op/delete.t's
third failing row (t56) is a genuinely different, already-blessed gap
(DESTROY-via-GC), so the file misses the all-or-nothing bar and correctly stays
UNEXPLAINED with t26/t54 as the fix target.

---

## 10. (added s319) THREE fixture artifacts now found in the #25 signal — the
##     gate keeps containing failures that are not PCL's

This is a process ask, not a code one, and it is the one I most want closed
before R1 is called.

Three separate times this session a "PCL failure" in the release signal turned
out to be the harness:

| # | file | looked like | actually was |
|---|---|---|---|
| #151 | io/defout, op/localref, uni/bless | PCL drift (NOTAP) | a `cp` had overwritten perl's real `t/test.pl` with PCL's stub |
| #167 | op/chdir.t t1/t2 | PCL failing 2 tests | `splitpath` ignored `no_file`, so the test's own `skip` never fired and PCL RAN tests perl SKIPS |
| #172 | op/chdir.t t25/t31 | PCL's chdir/abs_path wrong | the shadow t/ symlinks `op`, and `getcwd(3)` returns the PHYSICAL path — so `"$Cwd/op"` never equals the post-chdir cwd |

For #172 I verified PCL and perl are **identical** on every primitive involved
(bare `chdir()` honouring HOME *and* LOGDIR, `delete $ENV{}`, `cwd()` tracking
a chdir, `rel2abs(curdir)`, and chdir-into-a-symlink resolving to the physical
path).  Nothing in PCL is wrong; the two sides simply run in different trees.

**The ask.**  Where should a fixture artifact be recorded?  It is not
`perl-suite-expected.tsv` material — that file's bar is "explained by a blessed
`not-supported.md` section", and a harness artifact is not a language gap, so
filing it there would mislabel it and let a future reader think PCL lacks
something it has.  Candidates:

1. a distinct **FIXTURE** status in the runner (like NOTAP/XDIFF), with its own
   registry, so these rows stop counting as UNEXPLAINED without pretending to
   be non-support;
2. make the shadow **copy** rather than symlink any directory a test may
   `chdir` into (costs disk and time per run, and only fixes this class);
3. run PCL in the real `t/` and inject the stub another way — but the shadow
   exists precisely so `require './test.pl'` resolves to PCL's stub.

I lean (1): it is cheap, it keeps the signal honest, and it makes the count of
"artifacts" visible rather than buried.  But it adds a status to a tool that is
the R1 gate, so I would rather you pick.

**Does this gate R1?**  Today op/chdir.t reads 42/2 UNEXPLAINED and two of those
rows are not real.  That is honest but misleading, and there may be more of the
same in the 91 files #150 part 1 unmasked.

---

## 11. (added s319) A blessed-gap I will not declare on my own: perl's
##     above-Unicode code points exceed SBCL's character ceiling

Small, but CLAUDE.md 4 says not to mark something a limitation without asking,
so here it is rather than in `not-supported.md`.

`t/op/chr.t` t40-t42 encode code points ABOVE Unicode's maximum using perl's
own extended UTF-8:

```
chr(0x110000)  perl: f4 90 80 80        PCL: U+FFFD
chr(0x1FFFFF)  perl: f7 bf bf bf        PCL: U+FFFD
chr(0x200000)  perl: f8 88 80 80 80     PCL: U+FFFD
```

**Measured, not assumed:** SBCL's `char-code-limit` is 1114112 (`#x110000`),
so the largest representable character is `#x10FFFF` and `(code-char #x110000)`
signals a `SIMPLE-TYPE-ERROR`.  A CL string physically cannot hold these.

So the options are:

1. **Bless it** (my recommendation) — a `### Code points above U+10FFFF`
   subsection under §Unicode.  Perl's extended UTF-8 is a non-standard perl
   extension; no real CPAN module emits code points past U+10FFFF, and U+FFFD
   is already the sane answer for an unrepresentable one.
2. **Represent Perl strings as something other than CL strings** — this is the
   only way to actually support it, and it is a representation change on the
   scale of the boxed-aggregates question in §1, for a case nothing needs.
3. Leave it UNEXPLAINED, which keeps 3 rows in the gate looking like defects.

If you take (1), op/chr.t's other four rows are already covered by the existing
`### use bytes` subsection, so the whole file becomes a clean XDIFF and three
more phantom rows leave the R1 signal.

**Separately and already done** (no decision needed, flagging so the number is
not a surprise): `op/cmpchain.t` is registered XDIFF.  All **274** of its
failing rows are the identical assertion
`is eval("sub { \$a <=> \$b <=> \$c }"), undef, "… non-associative"` — invalid
Perl that must fail to compile.  That is CLAUDE.md §9's *verbatim* example and
task #149's approved category.  I counted every row (274 of 274 say
"non-associative"; none asserts a value) and verified the legitimately
CHAINABLE form `$a == $b == $c` works correctly in PCL, so this is exclusively
the rejection category and not a comparison-operator bug.

---

## Verification standard used for the seven commits

corpus-diff (emission-changing commits only — four of the seven are
runtime-or-tooling-only and touch no `Pl/` file), `tools/prove-core` full
gate, full sweep + `sweep-diff` vs baseline, census.  Every commit: gate
123/4452, sweep 0 new.  The baseline moved 702 → 698 exactly once, for the
four #149 rows, by EDITING those rows out rather than re-blessing from a run
— a wholesale re-bless would have written 695 and silently dropped the 3
eval.t/postfixderef.t rows that merely did not run.
