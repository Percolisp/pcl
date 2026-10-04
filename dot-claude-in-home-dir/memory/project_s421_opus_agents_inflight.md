---
name: project_s421_opus_agents_inflight
description: "Parallel Opus 5 subagents in git worktrees (round 1 s421 A/B/C; round 2 s425 B-finisher + E + F) â ALL MERGED by s428; the pattern, generation-string rules and merge checklist for the NEXT fan-out"
metadata: 
  node_type: memory
  type: project
  originSessionId: fed7fc1f-e343-42f1-8d7d-d9de0ee91f13
  modified: 2026-09-01T05:03:49.251Z
---

Pattern (USER-requested s421, repeated s425 "try sub processes for the Opus 5
jobs"): each Opus 5 agent works in its own `git worktree`/branch, commits
there, never pushes, never edits CLAUDE.md/memory, never merges; Fable
reviews each `docs/opus5-review-requests-sNNN.md` + commit diff, re-runs its
probes vs perl, merges, renumbers `*pcl-cache-generation*` ONCE per merge and
regenerates the three artifacts, then one COLD gate + one full sweep on the
final tree.  Distinct generation strings per agent (shared `~/.pcl-cache`).
Round-2 addition: each agent REBASES onto main before its final gate so the
merge is a fast-forward.

| round | agent | task | session | gen | task ids | worktree |
|---|---|---|---|---|---|---|
| 1 | A | #419 | s422 | v2-164 | 424â429 | MERGED (c84dd45) |
| 1 | C | #423 | s424 | v2-166 | 436â441 | MERGED (d655da6) |
| 1â2 | B (finisher) | #418 widened | s423 | v2-171 (rebased; was v2-165) | 430â433 used | MERGED s425 (f02fe2a, ff; worktree removed) |
| 2 | E | #388 consumer 3 + #420 + #422.1 | s426 | v2-170 | 443â448 | MERGED s425 (c1983e1, ff; rulings docs/fable-answers-s423-s426-s427.md; worktree removed) |
| 2 | F | #415 items 1+4, #421, #422.2, #442 | s427 | v2-173 (v2-171 collided with B) | 449â450 used | MERGED s428 (ff onto ff0cd86 â 821f0bb; renumber 53dcd2e = v2-174; worktree removed) |

**ROUND 3 MERGED s440 (A/B/C/D; main gen v2-201, v0.1.0 tagged 29c2cf3).  ROUND 4 MERGED s444 (2026-08-24): the four STOPPED agents (USER break s440) were each RESTARTED as a fresh Opus agent in its KEPT worktree, told what its uncommitted diff was â every diff finished, none redone; all four reviewed with independent probes vs perl and ff-merged (E #470 37bd6f2; G #485+#484a+#492 9c5e983+a31aad4; H #516+#515+#511 8d468b7+8cdaeef; F #491+#495ac 4c32128+c42cc8a); worktrees + branches removed.  Final tree gen v2-221; legs all clean (session-log Â§444).  LESSONS ADDED s444: (1) assign agent generation strings ABOVE the highest PLANNED main â F's assigned v2-210 fell below main's v2-220 after its rebase and it had to renumber itself to keep the shared cache honest; (2) a stop-record's description of a dirty diff can be wrong (H's "v1 text twin" was actually the START of #515) â the restarted agent must read the diff FIRST and finish what is actually there; (3) resuming a stopped agent as a fresh agent in the SAME worktree path works exactly as designed.**

**STATE (s428, 2026-08-22): ROUND 2 COMPLETE â ALL MERGED.**  F (s427) finished and committed before the s425 session died; s428 (Fable) reviewed it (review doc `docs/opus5-review-requests-s427.md`, six own probe files vs perl, one pre-existing finding each way: #451 `"$?[1]"` interpolation, #452 `<PKG::NAME>` readline), APPROVED, fast-forwarded, renumbered the generation v2-173 â **v2-174** on the merged tree, regenerated the three artifacts (stamp-only diffs), ran the COLD gate + the full sweep on the final tree, and pruned ALL worktrees (F's, the merged A/C ones, the s201-era `agent-a228b3f7e9b55b5fd` whose `%p-snapshot-array-rhs` diff is on main at cl/pcl-runtime.lisp:4323, `s381-review` whose log entry is on main, F's `s427-base`).  Nothing is in flight.  This file is HISTORY now â the pattern + the lessons (distinct generation strings with a GAP above main; a collision = re-run the measurements under a fresh string, not a redo; rebase before the final gate so the merge is ff; task JSON written with a utf8 JSON::PP to a :raw handle â F's updates to #415/#421/#422/#442/#449/#450 arrived double-encoded and were repaired s428).

**ROUND 5 LAUNCHED s445 (2026-08-25), base `ad6553d`, five Opus agents in
`~/pcl/.claude/worktrees/` â IN FLIGHT until the s445+ merge review records
otherwise.**  **M FINISHED FIRST and is MERGED (`d0d32b6`: #73 remainder
cache-free â monomorphic 2.62Ã/inherited 4.74Ã of perl, steps 2/4 closed by
measurement, ratified; #533; #73 completed, #534 filed by review â the
import/unimport empty-list family, pre-existing; #580â#582 filed by M;
#582 scheduled post-v0.1; #518 CI half MET, four consecutive green runs).**
The other four hit the session token limit mid-run (2026-08-25), were
RESUMED via SendMessage with context intact, and **L, J, I are now MERGED
too**: L `82dc86a` (#502 lib/English.pm shim; #504 runpcl stream
separation; #525; filed #570â#576), J `c3b3eaf` (#506 all-29-chars punct
containers; #507; #514 sort-NAME entry resolve + the perl-4 tick fix; #517
late-bound \&$name; #505 part â magic-scalar symbolic reads; gen v2-235;
edited 2 fail-baseline + 2 pass-baseline rows for #517, verify in the
batch sweep; filed #550/#551), I `8600556` (#508/#509/#510 local family +
P-HSLICE arm; #512 $0 writable + preamble line â corpus-diff = exactly
that one line; #513 dup-open; filed #540â#543).  I's rebase conflicted
(gen line + artifacts): resolved by Fable to FRESH **v2-255** + artifacts
regenerated; gate on the combined tree 174/5969 PASS.  #572 closed as dup
of #512.  **K MERGED TOO â ALL FIVE IN, main `f95fa97`, gen v2-290 = K's
post-rebase string, adopted as the batch string (round-4 precedent;
artifacts regenerated by K on exactly this tree).**  K: #479 CLOSED both
halves (readline cascade, PPI Â§14b), #478 name list GONE with the ruled
budget measured and REJECTED (4.3Ã intrinsic; #560 = disk-cached
prototype facts is the real mitigation), #463 items 3+4 + item-5 part;
census 73/167 â 34/89 BY EDIT; filed #560â#565.  Renumber convention
CONFIRMED (K ask 2): assign above main AT REBASE TIME.  BATCH LEGS
RUNNING (s445): full sweep first (magic.t rows 104/105/110 out of
fail-baseline, 204 in, pass-baseline 150/39â152/37, per I; verify J's
four baseline rows; drops 5 = census; watch #514's entry die + K's
%p-check-symbol-reference die), then gate-SET scan vs ad6553d both
populations, companion op/+io/+re/+mro/+class/+method (expect
reg_namedcapture abortâ0/2 honest #561), cold gate on main (xs rows RUN
here â 13 fail expected), then session log + push.  Each was told: no push/merge, no sweeps (Fable runs one over
the merged batch), no review-request docs (removed s440 â asks go in task
JSONs + final report), rebase onto main before the final gate, session ids
s446iâs446m.  Reserved NEW-task ID ranges per agent (task store is shared!):

| agent | tasks | gen string | new-task IDs | worktree |
|---|---|---|---|---|
| I | #508 #509 #510 #512 #513 (local-LIST family, $0, fh dup) | v2-230 | 540â549 | agent-a77686fbaadaadd9f |
| J | #505 #506 #507 #514 #517 (symbolic-ref/PID-cast family, sort NAME, \&$s) | v2-235 | 550â559 | agent-a8e7e8606824b855b |
| K | #463 items 3â5 + #479 compiler half + #478 (glob-value parse family; drop census falls) | v2-240 | 560â569 | agent-a075f2f1198b3b987 |
| L | #502 lib/English.pm shim + #504 runpcl stderr (+#525 if room) | v2-245 | 570â579 | agent-ab9a0bdc1a07ccde2 |
| M | #73 remainder (stash-in-box â fast path â pre-built pl-NAME; cache-free, USER s444) + #533 | v2-250 | 580â589 | agent-aa919a8bab2e1b0c1 |

Merge-review owes (agents told to enumerate too): ONE full sweep over the
merged batch + companion legs (op/+io/ for I, mro/+class/+method for M,
gate-SET if any refusal stopped firing â K's census work likely qualifies);
L's lib/ change makes the sweep NON-OPTIONAL; M's stash-in-box is a
box-representation change (sweep IS the gate; ref()/glob stringification
probes, #423 family).  Generation renumber ONCE on the merged tree (above
v2-250) + artifact regeneration, per the standing pattern.

**ROUND 5 ALL MERGED s445 (`f95fa97`, gen v2-290); worktrees pruned.**

**ROUND 6 LAUNCHED s448 (Fable, 2026-08-27), base `922675a` (main after the
s447 docs-only README pass) â IN FLIGHT until a merge review records
otherwise.**  Three Opus agents in Agent-tool worktrees, same standing rules
(no push/merge, no sweeps â Fable runs ONE sweep + companion legs on the
merged batch, no review-request docs, rebase onto main before final gate,
sweep baselines untouched by agents; agent MAY edit the drop census row-by-row
with cause):

| agent | session | tasks | gen string | new-task IDs |
|---|---|---|---|---|
| N | s448n | #543 #535 #529 (io/dup family, cl/ only) | none needed (cl/-only) | 590â599 â **MERGED third (main `866830c`)**: dup modes both arg forms (2-arg `+>&SRC` used to CREATE a file `&SRC`); std handles = descriptors 0/1/2 (dup2 + rebuild, close frees the fd); fileno undef; #535's filed diagnosis was WRONG (racing child, root cause #593); filed #590â#594 |

**ROUND 6 COMPLETE (s448, 2026-08-28): ALL THREE MERGED, batch legs clean
(sweep 18321 +2; gate-SET 638Ã2 zero; companion 9 movers attributed row-level,
8 snapshot rows spliced; cold gate 179/6054 xs-only), record commit `c8a8311`,
worktrees pruned, git gc'd (52M â 13M).  NO AGENTS IN FLIGHT.  Next round =
plan-post-s433 Â§s448 (design rulings first, then round 7: io residue /
#593-scoping / parser fillers).**
| O | s448o | #565 #562 #571 #573 (caret/punct magic family) | v2-321 (renumbered at rebase; artifacts regenerated on the merged tree) | 600â609 â **MERGED second (s448, main `7a2bc37`, ff; worktree pruned)**: all four done ($^R was a raw defvar that box-set silently ignored; $^Eâp-errno-string; $: one-char; *^R via chr(18) + %p-slot-name meeting point; *] via UnmatchedBrace); census t/re/pat.t 3â2 edited with cause; filed #600â#602; Fable probes all identical to perl; the concatenation-operand position (`"" . *-`/`*^R`) drops loudly on main too â the s446k whitelist's deliberate boundary, not filed |
| P | s448p | #570 #527 #534 | v2-320 | 610â619 â **MERGED first (s448, main `c9dd01d`, ff; worktree pruned)**: all three done + the qw-slice `list_ctx_subscript` marker fix #527 exposed; filed #610â#612; Fable filed #620 (`my @a = LIST if COND` drops, pre-existing, own-probe find); Fable probes all identical to perl; guards aassign-01/anon-sub-01/method-dispatch-01 pass |

Deliberately EXCLUDED from this round (need Fable design/ruling): #551, #541,
#561 (box-model question for computed magics + %!), #560 (disk-cached
prototype facts, Fable-sized), #542 (global stdio buffering policy â its own
sweep price).  Merge-review owes: ONE full sweep; io/ leg (N), re/+op/ legs +
gate-SET scan for un-dropped shapes (O), op/ leg (P); generation renumber
ONCE above v2-320 on the merged tree + artifact regen; the sweep-row
movements each agent's final report enumerates (scalar.t `+<&`, io/open.t
restore region, t/re/pat.t, t/op/tie_fetch_count.t).

**ROUND 7 LAUNCHED s449 (Fable, 2026-08-29), base `c8a8311` â IN FLIGHT until
a merge review records otherwise.**  Three Opus agents in Agent-tool worktrees
(`~/pcl/.claude/worktrees/agent-*`), standing rules as rounds 5â6 (no
push/merge, no sweeps â Fable runs ONE sweep + companion legs on the merged
batch, no review-request docs, rebase onto main before final gate, sweep
baselines untouched; drop census editable row-by-row with cause):

| agent | session | tasks | gen string | new-task IDs |
|---|---|---|---|---|
| Q | s449q | #591 #621 #590 #592 (io/dup residue + FD_CLOEXEC/fcntl; shape settled s449) | v2-340 (used; fcntl emission) | 630â639 â **MERGED first (main `cf49f95`, ff; worktree pruned)**: all four done (#591 dup flushes source BOTH directions via one %p-sync-fd-position; #621 failure shape = the ARGUMENT FORM, 2-arg EINVAL-false vs 3-arg fatal; #590 refused write = false print + dup direction from the DESCRIPTOR + in-memory `<` read-only via new p-string-input-stream + close false-not-abort; #592 fcntl builtin + FD_CLOEXEC above $^F on %p-install-fh â the inheritance premise did NOT hold, run-program closes >2, filed #633).  Filed #630â#634; Fable review probes ALL identical to perl (5 probe files + clean re-probes); Fable filed #660 (slurp-at-EOF first read is DEFINED "" in perl) + #661 (fcntl on undef lexical DIES in perl) from probe misfires, both pre-existing on main.  Asks ruled: EBADF-vs-untouched-$! accepted (errno fidelity stance); >&-of-O_RDWR bidirectional = watch io/ leg at batch sweep; in-memory read-only approved (no blessed stance existed). |
| R | s449r | #593 #594 #530 #532 (my-before-in-block-package scoping = io/open.t root cause; bareword-handle-in-expression family) | v2-350 (renumbered at rebase above Q's v2-340; artifacts regenerated, content-identical) | 640â649 â **MERGED second (main `f0dda81`, ff; worktrees pruned incl. its scratch base)**: #593 = TWO PREDICATES disagreeing (`_symbol_is_declarator` called every paren-list open() argument a declaration â span detector saw no USE â SPANREFUSE; fix = ONE resolver `_declarator_syms` read by both â the #454 shape again); #594 dup SOURCE slot = third site of the bareword-handle family, MODE (`&`) is the discriminator, argument override through `_fh_sym` (early-return version dropped a p-list-ctx wrap â caught by A/B); #532 = the registry exit now asks `_bareword_callable_here`; **#530 ATTEMPTED, MEASURED, BACKED OUT** (marker fix broke six handle slots â record in #641, do not retry as written).  A/B 1147 SAME 1145/DIFF 2/RCDIFF 0 (both diffs = the intended one-token quote).  Expected companion mover: io/open.t 136/25 â 141/24 (#593 +1 row, #594 +4; abort reason now the pre-existing line-267 drop); io/dup.t unchanged.  Filed #640 (declared sub beats handle in print-FH/<FH> slots too â three-valued rule) + #641.  Fable probes (12 rows, 4 files) all identical to perl; guard files 72 rows pass; the #542 interleaving noise seen once, known.  GATE-SET SCAN OWED at batch end (two triggers: #593 widened detector, #532 narrowed exit). |
| S | s449s | #563 #564 #550 (+#620 bonus) (diamond-glob derail, local *{EXPR}, %-container repair) | v2-355 (renumbered twice at rebases; adopted as the batch string) | 650â659 â **MERGED third (main `46325f9`, ff; worktree pruned)**: #563 glob-pattern cascade repair (term-position `<` IS a diamond â perl probed; two `_ends_term` gaps closed: ArrayIndex + deref-block `}`); #564 `local *{EXPR}` = p-local-glob-dynamic through ONE dynamic-glob resolver `%p-glob-dynamic-target`, the silent empty-target return now a loud drop, `%p-glob-save/restore` carry declared-subs status (the "static twin" fix â it made the op/gv+uni/gv constant-sub rows pass); #550+#449 punct containers (position rule via `_ends_term`+declared-term; `%{?}` block-spelling TEXT rewrite â a Parser2-chain repair must change the TEXT, `_reparse_doc` re-splits spliced nodes; `needs_pipes`/`_already_cl` refined â WHERE the pipe is decides); #620 diagnosed to one line.  Filed #650â#653; PPI Â§14c/Â§24b/Â§26b + ppi-bug-report.t 38â42.  Fable review fix on top (`2d0da65`): the cascade repair runs to FIXPOINT (one derail hides the next); Fable filed #662 (`f % 3` = perl's hash %3, outside the punct set, pre-existing). |

**ROUND 7 COMPLETE (s449, 2026-08-29): ALL THREE MERGED, batch legs clean**
(sweep Ã2 GATE clean TOTAL 18325 (+4 = method.t #564, serially verified both
trees); gate-SET 638Ã2 byte-identical; companion io/+op/+uni/ six real movers
all attributed by base-worktree A/B and spliced â io/open 141/24 (R),
io/pipe 18/14 (Q #590 un-aborts Broken pipe), op/method 103/22 + op/gv
131/60 + uni/gv 56/32 (S #564); pvbm 12th sighting + uni/variables TIMEOUT
left).  **#663 = uni/parser.t's STALE row hid a ROUND-5 regression** (s446k
#463 range, bisected agent-by-agent: `${*$a{SCALAR}}` dies; 5-line repro in
the task) â LESSON: companion legs skipping a dir leave rows unrefreshable;
glob/name rounds include uni/.  Records: DECIDED Â§s449, session-log Â§449.
Round 8 = T (#561+#602) / U (#560+#477 perf); V (#541+#520â#522) â round 9
with solo-#542 (USER: TWO agents this round).

**ROUND 8 LAUNCHED s449 (Fable, 2026-08-29), base `e2a7567` (pushed; CI
in_progress on it) â IN FLIGHT until a merge review records otherwise.**
Two Opus agents in Agent-tool worktrees, standing rules as rounds 5â7
(no push/merge/sweeps, no review docs, rebase before final gate, baselines
untouched except the drop census row-by-row):

| agent | session | tasks | gen string | new-task IDs |
|---|---|---|---|---|
| T | s450t | #561 #602 (computed-magic design implemented â docs/computed-magic-design-s449.md; Errno shim FILED not built) | v2-365 (used â ONE justified emission deviation) | 670â679 â **MERGED first (main `5e2f498`, ff; worktree pruned)**: #561 both halves as designed (canonical |$!|/|$^E| magic boxes; %! = real 134-key table with magic-cell values, numbers from sb-posix + an 11-name glibc fallback Ã  la *p-signal-numbers* â **ask RATIFIED s449**: platform parity was the design's intent, sb-posix values win where present); the deviation = `$!{ENOENT}` had NEVER read %! (sigil-swap on a rendered COMPOUND form is unreachable â `_bare_container_sym` re-renders from the aggregate spelling for ref-valued %SPECIAL_VARS names ONLY; 634-file A/B: 1 real diff, t/op/require_errors.t, verdict unmoved); probed answers that beat the guess: `$!{NAME}` is the errno NUMBER or 0 (never 1, always defined), STORE dies "ERRNO hash is read only!".  #602 clear-then-copy via ONE `%p-glob-empty-slot` shared with `undef *foo` (rule 11).  Filed #670â#674 (#673 = the Errno shim; #674 = English.pm may drop the $!/$^E ties).  Expected mover: re/reg_namedcapture.t 0/2 â 1/1 (row 2 held by #671+#672).  Fable probes: 3 files, all byte-identical to perl (134 keys, aliased-number pairs, RO store, clear-then-copy, import inverse, pat.t:1715 shape); guards 47 rows pass; the `exists $main::{c3}` probe residue = the blessed live-stash DEFERRED absence, no filing. |
| U | s450u | #560 (proto-facts disk cache, RULED design; corpus-diff IDENTITY = first bar leg; 289-file timing A/B) #477 (quadratic pos â algorithmic, curve measured before/after) | none expected (cost-only changes) | 680â689 |

Merge-review owes: ONE full sweep; op/+re/+uni/ companion legs (T = errno/
glob â uni/ per the #663 lesson); corpus-diff-IDENTITY asserted for U;
expected movers = re/reg_namedcapture.t (T) and op/utf8cache.t's TIMEOUT
verdict (U); snapshot splices at merge only.

**U MERGED second (main `3c7c6ec`; worktree pruned; Fable rebased U's branch
over T â doc-section conflicts only, both kept)**: #560 ProtoCache.pm
(pop. 226.6s â 62â78s, warm hit 99.0%; identity 4 ways incl. server-mode
byte-equality; THREE ruled-key strengthenings: compiler stamp, dependency
re-stat, taint-no-store for cwd-dependent walks â Encode pair = the warm
pass's only 6 misses) + #477 (root cause = set-match-vars copying the
subject TWICE per match building $`/$Â´ eagerly; now offsets + lazy cut via
SYMBOL MACROS â a magic-cell box would alias `push @a, $&`, the live $. bug
it found = #683; 200k 12.8sâ0.72s, 1M 2.83s linear, utf8cache.t
TIMEOUTâDIFF 14/0; residual Target-A gap = #680).  Gen NOT bumped by U
(v2-365 = T's stands).  Filed #680â#683; Fable filed **#684** (match vars
are BLOCK-scoped in perl â leaving the block RESTORES $&/$1; pre-existing,
probed on pre-#477 main).  Fable probes: partition/failed-match/keep-across
byte-identical; 200k timing re-measured 0.5s; guards 43 rows; combined-tree
gate 182/6124 PASS + corpus identical before merge.

**ROUND 9 AGENT V MERGED (s450b, main `9d5c468`; worktree pruned)**: #541
(the ruled conditional save/restore â p-local-cell-if + p-local-maybe;
progv deviation RATIFIED: the conditional `let` in the localizer slot IS
the conditional dynamic bind; unconditional spellings byte-identical,
20 A/B diffs all conditional-local hunks) + #520/#522/#521 (ONE
replacement path: cl-ppcre non-simple calls + the shared match-state
setters; case shifts routed to the dq compiler; `_undelimit` undoes ONLY
the escaped delimiter).  Filed #690â#693 by V; Fable filed **#694**
(op/exec.t's one flipped `$!` row â bisected to the s/// commit, mechanism
unisolated, measurements in the task).  **MERGE-PROCESS LESSON: commit
Fable's own doc rulings BEFORE `git merge --ff-only` â a dirty tree aborts
the merge AFTER `git branch -D` already succeeded; recover the ref from
the reflog (`git branch v-recover <sha>`), patch-save the dirty edit,
merge, re-apply.**  Legs: sweep +0; op/lc 2685/31 + re/subst 200/72
spliced.  NO AGENTS IN FLIGHT.  Next round: #685 + #663 + #694 bundle.  **ROUND-8 LEGS DONE (s450): sweep clean
18325 +0; companion movers spliced (utf8cache 14/0, reg_namedcapture 1/1);
ONE regression understood+filed = #685 (leaky-magic row 48, the
qualified-foreign symref inheriting exported magic symbols â pre-existing
mechanism, #561 added a member; fix shape ruled in the task, heads the
next filler slot with #663).**

Merge-review owes: ONE full sweep; io/ companion leg (Q); gate-SET scan both
populations where R/S stop a refusal/drop from firing (S un-drops census
rows; R's #593 is scoping â sweep IS the gate, run at merge); generation
renumber ONCE above v2-340 on the merged tree + artifact regen.  The five
design rulings shipped the same session (DECIDED Â§s449;
docs/computed-magic-design-s449.md): round 8 = #561+#602 session, #541
session, #542 alone, #560 build.

**ROUND 10 LAUNCHED s451 (Fable, 2026-08-29), base `718db6b` (main gen
v2-375; round-9 push done, CI green on 718db6b) â IN FLIGHT until a merge
review records otherwise.**  Three Opus agents in Agent-tool worktrees
(`~/pcl/.claude/worktrees/agent-*`), standing rules as rounds 5â9 (no
push/merge, no sweeps â Fable runs ONE sweep + companion legs on the merged
batch, no review-request docs, rebase onto main before final gate, baselines
untouched except the drop census row-by-row with cause):

| agent | session | tasks | gen string | new-task IDs | worktree |
|---|---|---|---|---|---|
| W | s451w | #685 #663 #694 â **MERGED second (main `2264a3c`, Fable-rebased over X, one DECIDED.md conflict kept-both)**: #685 one resolver (%p-symref-find/-intern, :INHERITED = not-found for foreign-qualified; leaky-magic 65/6â70/1); #663 root cause = PPI `->symbol` cast set misses `*` (Â§27 logged; `_brace_glob_slot_symbol` repair; uni/parser 18/5+abortâ28/30 no abort; Carp.pm silent-wrong fixed); #694 root cause = `_undelimit` ran on RAW-text synthetic wrappers (heredoc/qq{}/backtick/glob) â `_delim_escapes_dquote($origin_tok)`; op/exec 22/3â23/2.  Filed #700â#703; asks ratified (PCL_SUITE_KEEP stays; #700 bundles later; #701 ruled die-shaped); Fable probes 4 files SAME + 2 inverse-DIFF on main | v2-380 (used; artifacts regenerated stamp-only) | 700â709 | pruned |
| X | s451x | #542 SOLO â **MERGED first (main `80a11a7`, ff)**: %p-output-buffering/%p-std-buffering policy fns (STDOUT :line iff isatty else :full; STDERR :none), %p-apply-std-buffering at load + *init-hooks*, %p-flush-all-output at every PERL_FLUSHALL_FOR_CHILD site + exit hook (measured: SBCL 2.6.0 runs exit-hooks on unhandled error AND SIGTERM; only :abort t skips â both such sites flush by hand); $| carries across reopen (%p-carry-autoflush, ride-along ratified).  Guard stdio-buffering-01.t 9 rows.  Fable probes 8/8 pipe + direct-SBCL pty SAME (runpcl tty divergence = its backtick capture, correct).  Owes at batch legs: io/fflush.t snapshot 3/1â4/0 splice.  Filed #710 #711 | none (cl/-only) | 710â719 | pruned |
| Y | s451y | #610 #620 #611 #612 â **MERGED third (main `4b7540b`, ff)**: #611 `_scalar_ctx_pushdown` licensed by EXPLICIT annotation only (`get_node_context_raw` â the default-keyed version moved 3 unrelated files, recorded do-not-retry), last child only, both emitters; #612 `_paren_deref_base_form` at six sites + `->&*`/`->**` parsed â postfixderef.t 96/25â100/21 expected, rows 72/73 now fail HONESTLY (accidental passes on a dropped stmt; fail/pass-baseline edits owed at batch legs); #610 `_is_no_name_my_decl` arm (RHS still evaluated once, list ctx); #620 modifier split ONCE before `_multi_decl`, let unconditional + guarded assignment (probed), self-ref-under-modifier residue drops LOUDLY = #722 (shape RULED s451: cond to temp once, tested twice; unscheduled filler).  Filed #720â#722; Fable probes 3 files SAME + inverse DIFF on main.  Asks owed to Fable at batch legs: Mojo census row 2â4 (pre-existing, verify), census MESSAGE column stale tree-wide since the s440 _dd swap, postfixderef baseline row edits | v2-390 (used; artifacts stamp-only) | 720â729 | pruned |
| Z | s451z | #702+#703 + #700 + #701 + #711 + #710 â **MERGED fourth (main `c880cd9`, ff; one accidental USER stop mid-#700, resumed via SendMessage with context intact)**: #702/#703 ONE command-capture lowering (`heredoc_is_command` sibling + `_make_command_node`; `use subs` recorded by a Parser2 PRE-PASS with source position â statement-time was too late, sub bodies lower first); #700 as ruled (`_foreign_runtime_global_form` â the #685 resolver form, the FOURTH name table pinned by global-partition-01.t; `local` followed via deref target kind); #701 ONE `%p-hash-marker-p` (third forgotten site) + rule-12 die; #711 was the COMPILER not the shim (file's own `use constant` invisible to sub bodies â name swallowed its args); #710 bounded dup-and-close (finalizer hazard MEASURED absent).  Backed out + recorded: unconditional bare-quote import arm (#733, now RULED: export-scan decides).  Filed #730â#735; Fable filed #736 (marker-flatten family, PRE-EXISTING â probed both trees).  Fable probes: 5 files SAME (incl. pty a-S-b), z3 residue = #736.  Z skipped its session-log record â folded into Fable's batch record | v2-395 (used; artifacts regenerated on the FINAL tree â string adopted as batch gen, round-5/7 precedent) | 730â739 | pruned |

**ROUND 10 COMPLETE (s451, 2026-08-29): ALL FOUR MERGED, batch legs ALL
CLEAN, record commit `0dd7434` PUSHED** (final code tree `c880cd9`, gen
**v2-395** = Z's, artifacts regenerated on exactly the final tree).  Legs:
sweep TOTAL **18327 (+2)** GATE clean after row-by-row edits (postfixderef
fail rows 16â21; reset.t+tr.t â1 each = **#723 TAP-glue**, PRE-EXISTING,
manual-child-A/B-probed on BOTH trees â the unterminated stderr diagnostic
glues ONE stdout row; #542 moved the landing from skip rows to pass rows;
Â±1 churn in PARTIAL files until #723 lands); gate-SET 638Ã2 ZERO diff;
companion 528 files with **18 movers ALL attributed** (5 predicted;
sub_lval = Y; pvbm/uni-variables noise; comp/parser + 2 `--bless-rows`
stale registrations pre-existing on 718db6b; **7 bisected to X's 80a11a7
merge point** â the #542 child-flush/capture family, +50 honest coverage
rows (attrs +41), one #723 glue instance (charset), ONE 2-row loss =
**#738** rt119311 exit-in-format); cold gate 184/6230 xs-only.  17
snapshot rows spliced with causes; census Mojo row 2â4 (verified
pre-existing); pass-baseline note block updated.  Fable-filed this round:
#723 (glue fix shape, harness-only), #736 (%ENV marker flatten/referent
arms, pre-existing), #737 (census MESSAGE column refresh), #738.  Rulings
recorded: #701 die-shaped, #722 ratified, #733 export-scan decides.
**LESSONS: (1) a buffering/exit-path runtime change's companion leg is
op/+re/+uni/, NOT io/ alone â X's io/ spot checks missed all seven of its
movers; (2) the merge-point BISECT over agent tips is the cheap companion
attributor (2 worktree runs split 4 agents); (3) an accidentally-stopped
agent resumes via SendMessage with worktree intact; (4) PCL_SUITE_KEEP (W)
is the row-level attribution instrument â copy the current runner into a
base worktree to use it there (verdict-inert, measured).**  NO AGENTS IN
FLIGHT.  Next: plan-post-s433 Â§s451 (filler pool #684/#736/#723/#721/#720
etc., then the census push / Q7 re-check).

**ROUND 11 LAUNCHED s452 (Fable, 2026-08-29), base `0dd7434` (main gen
v2-395; CI in_progress on it at launch) â IN FLIGHT until a merge review
records otherwise.**  TWO Opus agents (USER: "two opus jobs"), standing
rules as rounds 5â10 (no push/merge/sweeps, no review docs, rebase before
final gate, baselines untouched except the drop census row-by-row; each
told to write its OWN session-log section â Z forgot in round 10):

| agent | session | tasks | gen string | new-task IDs | worktree |
|---|---|---|---|---|---|
| AA | s452aa | #736+#723+#730+#738 â **MERGED first (main `008b364`, ff)**: #736 ONE `%p-marker-pairs` walker + referent identity (`\%ENV == \%ENV`, write-through; `%$envref = (â¦)` was a silent NO-OP, fixed); #723 writer = SBCL's handler-bind context note in load-as-source (reached via cached-module `load`), fix = line-atomic Gray *error-output* for the whole load, glued rows 0 both A/B files; #730 sysopen (O_ACCMODE 3 dies rule-12; #542 policy asked; tempfile() default works; bareword-handle registration SHARED with open); #738 mechanism = nested sb-ext:exit in an exit hook ABANDONS the rest of the hook â `exit` in END killed remaining ENDs + flush; now perl's semantics (per-block catch, $? carries, status last act) â rt119311 expected 11/2, BETTER than pre-#542 (ratified: perl's own rows are the oracle).  Filed #740 (-s on empty file), #741 (runpcl always exits 0 â never read status through runpcl).  Fable probes 3 files + rc SAME (one own-harness pipeline-rc misfire, redone).  **CORRECTION owed to the s451 record at batch legs: re/charset.t's â1 was NOT the #723 glue** (AA: zero glued rows on fixed tree, count unchanged) â still X's #542 family, mechanism unnamed | v2-400 (used â #730's name entry; artifacts regenerated) | 740â749 | pruned |
| AB | s452ab | #721 FIRST+ALONE (list-assign value in LIST ctx, family-wide; `my $n = () = f()` count idiom must stay right) + #720 (lvalue slice via autovivified elem) + #731 (+#734) (command capture list ctx + readpipe name â rule 11 on Z's `_make_command_node`; %WANTARRAY_SENSITIVE trap) + #733 if budget (ruled: export scan decides) | v2-405 | 750â759 | agent-ab9cb7abc2d5febf1 |

Merge-review owes: ONE full sweep; **#723 is a HARNESS change â companion
`--all --quick` BOTH populations** (what-to-run-when harness row); op/+re/
+uni/ legs regardless (round-10 lesson); pass-baseline rows reset.t 40â41 /
tr.t 238â239 edited BACK with #723 as cause + charset snapshot 2775â2776 +
rt119311 re-splice if #738 fixes; corpus-diff on AB's every emission change
re-read; generation renumber/adopt above the agent strings + artifact
regen on the final tree; gate-SET scan only if a refusal/decline widens
(#733 narrows the import reading â check AB's report).

**ROUND 11 COMPLETE (s452, 2026-08-30): AA + AB MERGED with one review fix (b3d1ef3, the cons-splice arm â the SWEEP caught it), legs all clean (sweep TOTAL 18337 GATE clean; companion 528 files, 10 movers attributed â the #723 un-glue reaches the COMPANION capture too; cold gate 184/6260 xs-only), record e43ef48 PUSHED, worktrees pruned.  NO AGENTS IN FLIGHT.  ROUND 12 = the interleaved plan (plan-post-s433 §s452): one PERF agent (re-measure #73 bench → #680 + #582 measurement) + correctness agents (census families + #755 + #732).**

**ROUND 12 LAUNCHED s454 (Fable, 2026-08-30), base `154f4a9` (main gen
v2-405) — then USER-STOPPED ~30 min in (token budget); all three agents sent
a graceful stop order (commit worktree state + stop-record commit + 1-para
report).  NOTHING MERGED.  Worktrees KEPT — resume via the round-4 pattern
(fresh agent in the kept worktree, told to READ THE DIFF FIRST; a
stop-record's description can be wrong).**  THE FIRST
INTERLEAVED ROUND (plan-post-s433 §s452): one PERF agent + two correctness
agents, standing rules as rounds 5–11 (no push/merge/sweeps, no review docs,
rebase before final gate, baselines untouched except the drop census
row-by-row with cause; each writes its own session-log section):

| agent | session | tasks | gen string | new-task IDs |
|---|---|---|---|---|
| AC (perf) | s454ac | re-measure #73 bench FIRST → #680 (profile-then-fix m//g per-match) → #582 discriminating measurement (implement ONLY if it lags AND marker+audit bar fits; else record) — #758–#761 explicitly excluded (round 13) — **FINISHED ALL THREE before the stop landed; sha `dc13541` in kept worktree, ff-able onto `154f4a9`, NO gen bump (cl/-only), NOT MERGED.**  Bench table in faster-codegen-suggestions §0.2 (+regexg row; ovlsub/symref load-suspect, re-check round 13).  #680 CLOSED: 66% was p-regex RE-PARSING the pattern per iteration (not the task's suspects); memoized p-regex + struct-cached scanner + high-water capture clear (perl DOES clear $1 on 0-group success — group-count gate would have been silent-wrong) + in-place @-/@+ box reuse + direct-funcall scan; 1M 2.83→0.40 s (~2.4x perl, engine gap next = PCRE2 FFI); 29 probes OK; match-vars-01.t 34 rows; filed #770 (s/// twin re-parses per iter).  #582 measured only: inherited 0.984 / mono 0.470 / plain 0.188 s @2M — cache WOULD fire; marker+audit still the blocker.  Merge owes: full gate on final tree (agent's own was core-invalidated; print-fh-magic-01.t passes standalone), sweep + re/ op/ uni/ legs; expect re/pat_psycho.t 15/0 REAL move (verify), utf8cache DIFF 14/0 snapshot, regexp.t re-run quiet. | none used (cl/-only) | 770–779 (#770 filed) |
| AD | s454ad | #755 + #732 — **BOTH ESSENTIALLY DONE before the stop; sha `aa70f9a` (2 commits) in kept worktree, ff-able onto `154f4a9`, NO gen bump (every population byte-identical), NOT MERGED.**  #755 DONE (`5a5bd86`): ONE seam `%p-literal-path` (parse-native-namestring) through all 11 pathname consumers (p-glob excluded by design) + p-unlink→unlink(2) (dangling symlink, EISDIR not crash) + p-rename→rename(2); probes byte-identical; guard wild-filename-01.t; gate PASS 185/6257 on its tree.  #732 CODE-COMPLETE (final prove-core + rebase skipped on stop; targeted proves green): readpipe rows were already green via #734 — real asymmetry was `use subs qw(length…)` not displacing; fix = Environment::builtin_is_overridable (keywords.pl '-' half; prototype("CORE::") is NOT the rule — `system` proves it) + `_core_qualified` marker at the CORE::-strip + override lookup in gen_funcall_form; guard use-subs-override-01.t 13 rows; blast radius MEASURED ZERO (corpus-diff IDENTICAL, emission-ab 1023 SAME, gate-SET 638×both IDENTICAL).  Filed #780 (unlink/chmod/utime/kill @array unflattened), #781 (paren-less overridden named-unary extent), #782 (`-f 'TEST';` mis-parse = filetest.t's remaining abort).  Merge owes: splice op/filetest.t snapshot 181/250→**181/253** (rows 34–36 now fail honestly, #221 family; old 183 was skip-as-pass), run AD's skipped final prove-core after rebase, flip task JSONs 755/732 to completed. | none used | 780–789 (#780–#782 filed) |
| AE | s454ae | census push + #737 — **STOPPED MID-RUN with 3 fixes committed, `dc06a28` in kept worktree, NOT MERGED, and — UNLIKE AC/AD — ITS BAR IS UNRUN: no prove-core/gate, no emission-ab, no corpus-diff after change 3 (was IDENTICAL after 1–2), no Pl/t guards, gen NOT bumped (v2-420 still owed if emission changed — change 2/3 are Pl/ emission-adjacent!).**  Census 34/90 → 27/77 (7 rows out row-by-row with causes; Mojo 4→2; stale header total corrected).  Fixes: (1) glob-assigned code declares the name with the code's prototype (`*try = \&_manual_try`, Test2::Util shapes; BEGIN-only in same file, load-time in module facts walk — negative probed); (2) `_take_rest_as_args` consumed to END-OF-STREAM → now shares `_listop_arg_ceiling` with the operator loop — also fixed a probe-caught pre-existing SILENT WRONG (`grep {…} LIST or EXPR` swallowed the `or`); (3) `_merge_module_prototypes` registers facts per-PACKAGE (`add_pkg_prototype`; qualified spellings answered there first; first attempt via declared_subs failed on ProtoCache L2 hits — shipped shape reads serialized tables).  Filed #790 (died-eval scalar vanishes from returned list, pre-existing).  NOT started: #458, #480–#482, WTF pair, readline, perl-t singles, full #737 refresh.  Merge owes for AE: THE FULL BAR (prove-core, corpus-diff, emission-ab, guards, gen bump + artifacts if emission moved, sweep) — expect movers in Test2-heavy cpan files + Mojo board rows. | v2-420 OWED, not set | 790–799 (#790 filed) |

**ROUND 12 COMPLETE (s455, Fable, 2026-08-30): ALL THREE MERGED** (AC
`dc13541` ff; AD rebased → `4c7dde0`; Fable review fix `ea58eb4` = readdir
literal names, glob twin filed #800; AE rebased ×2 → `b2ca837`; gen
**v2-420** + artifacts + AE's owed guards `d9748cb` — listop-ceiling row 5
+ glob-sub-alias-01.t, both inverse-verified on a 154f4a9 worktree).  AE's
unrun bar run by Fable, all green: prove-core PASS 186/6273, corpus-diff
IDENTICAL, lib A/B 22 SAME, Test2/Mojo A/B 14 DIFFs all attributed (7
census un-drops + 7 qualified-invocant silent-wrong FIXES), census
re-measured EXACTLY 27/77.  Legs: sweep GATE clean TOTAL 18337 (+0) drops
5 = census; serial re-run of the six PARTIALs reproduces baseline pass
counts; gate-SET 638×2 ZERO.  Records: DECIDED §s455, session-log §455.
Round 13 = plan-post-s433 §s452 item 2 (perf #758–#761; correctness #790
first + census remainder).  Worktrees pruned.

**ROUND 13 LAUNCHED s455 (Fable, 2026-08-30), base `83b335f` (pushed; main
gen v2-420) — IN FLIGHT until a merge review records otherwise.**  TWO Opus
agents (USER: "start two more Opus processes"; models PINNED opus),
standing rules as rounds 5–12 (no push/merge/sweeps, no review docs,
rebase before final gate, baselines untouched except the drop census
row-by-row with cause, own session-log section):

| agent | session | tasks | gen string | new-task IDs | worktree |
|---|---|---|---|---|---|
| AF (perf) | s456af | re-measure ovlsub/symref FIRST (quiet-box check) → #758 → #759 → #760 → #761 (the s453 verdict-coverage walk; Kind-A/B gates in Pl/Passes.pm, PCL_OPT=none equivalence, bench before/after per row) + strike tier-2 N2 if the acc variants confirm | v2-430 | 810–819 | agent-a60be029f1e14df02 |
| AG | s456ag | #790 FIRST (died-eval scalar vanishes from returned list) → census remainder (27/77: #480–#482, the Event.pm content-on-undef pair, 637.t WTF, Mojo local-glob-list 2, perl-t families by harvest) → Q7 re-check #457/#464–#466/#468/#470 (close overtaken with probe output) | v2-440 | 820–829 | agent-aab151e79d2ac08b7 |

**ROUND 13 MERGED SAME DAY (s455b, 2026-08-30): AF `030a089` ff; AG rebased
→ `b42ef7a` + Fable pack-regen `1bc9de5` + nit `49e55ad` = main tip.**
Combined gate PASS 189/6342; sweep GATE clean TOTAL 18339 (+2, split.t +
hexfp.t verified AG-only on a cbfd7c0 worktree; fail/pass baselines edited
row-by-row incl. the aassign round-11 lag + magic wobble + reset PARTIAL→OK
+ state planned −1); census 21/65 EXACT with both cpan populations at ZERO;
bench on merged tree intloop+= 0.27x / intloop= 0.29x / cfor 0.24x.
Rulings in DECIDED §s455b (print/say = #813; #810 rides boxed aggregates;
range-topic $_ is READ-ONLY in perl — the residue line's sharpening).
AF/AG worktrees pruned after merge.  Companion op/+uni/+re/ legs ran same
session: SEVEN movers, six spliced with tip-bisect attribution (lexsub/
readline/split/hexfp/packagev = AG; charset +1 honest fail = AF;
uni/variables TIMEOUT churn untouched).  **AH MERGED TOO (`5850328`, pushed
a4ce009..5850328): #800 (glob leaf renderer = %p-dirent-name, dup DELETED;
the escaped leaf also broke glob's own MATCHING — glob("gx?q.dat") over a
literal gx?q.dat matched nothing), #740 (-s is a VALUE: empty file = 0
defined, undef only on stat failure), #737 (census message text refreshed,
counts asserted byte-identical 21/65); filetest.t snapshot 184/250 spliced
by AH with A/B cause; filed #830 (glob pattern has no backslash escape).
NO AGENTS IN FLIGHT.  Round 14 = plan-post-s433 item 3.**

**AGENT AI LAUNCHED s455e (Fable, 2026-08-31), base `0237940` — IN FLIGHT
until a merge review records otherwise.  SCOPE EXTENDED (USER, mid-run):
PHASES 0–3 + the bench measurement — the flip included.**  Order: #817+#818
first (real fixes, own bars — expect honest values/slice suite movers),
elem-alias-01.t battery + gate-shape measurement (doc §7.2), inert phases
1–2 with ZERO-CHANGE sweep bars, then PHASE 3 (raw-elems ON; battery vs
perl; full sweep + op/uni/re/io legs + census; gate-off = all-boxed world
bit-identical; ir-spec §2.3/§2.4 rewrite; not-supported sort-comparator
entry per §7.1; gen v2-450 + artifacts) and bench §0.2c before/after
(targets arrhash ≤1.0×, arrfill ~1×; won rows must not regress).  Told to
STOP AT A BLOCKER rather than force a red bar.  Hit the Opus session limit
once mid-phase-2 (3 commits landed: 9c0cf75 #817/#818, baf8f29 claw-back,
2dbb42e phases 0–1); RESUMED via SendMessage 01:31 with read-the-diff-first.
Plan = docs/boxed-aggregates-design-s455.md (task #816).  Pinned opus;
session s457ai; task IDs 840–849; worktree agent-abd59a3685135e911.
Merge owes: review ALL phase commits + the flip, #817/#818 + phase-3 mover
splices, verify zero-change bars ran, gate-SET verification, battery
re-run, my own bench re-measure on the merged tree, ff-merge + records +
push.

**BOTH s457 AGENTS MERGED (s455e, 2026-08-31, main `8e38d79` PUSHED, gen
v2-470).  NO AGENTS IN FLIGHT.**  AJ (`21e0b70`): #820 Unicode ALL-CAPS
with the measured predicate split + #850 trailing-comma ceiling (census
21/65 → 19/63; io/open.t completes for the first time) + #813 measured &
declined — its measurement DISPROVED the s455b range-`$_`-read-only
sharpening (corrected in place, `07f2df0`).  AI (`405ebb3`..`5f7a23e`):
boxed-aggregates PHASES 0–3, elements RAW by default; #817/#818; four
flip-time gaps all caught by existing guards; gate PASS 190/6373 both
PCL_RAW_ELEMS settings; sweep clean 18340 both ways; census re-measured
19/63 = blessed; Fable k1 battery byte-identical to perl both settings;
bench verified arrhash 1.23× / arrfill 3.00×, won rows held.  Rulings:
two knobs stand; #841 filed.  LESSON: the report's census "18/61" was a
misstatement — MEASURED before merging, per the trust-nothing rule.
Round 15 = phase 4 (accessor dispatch + proven arms + E2c′) beside
#815/#811.

**ROUND 15 LAUNCHED s455e (Fable, 2026-08-31), base `8e38d79` (pushed) —
IN FLIGHT until a merge review records otherwise.**  TWO Opus agents
(USER: "start a couple of Opus sub jobs"), standing rules as always
(no push/merge, own session-log section, rebase before final gate,
baselines row-by-row; siblings warned disjoint — AK owns runtime
accessor/box/overload/element regions + elem-alias-01.t, AL owns
lib/feature|warnings|experimental.pm + parser census):

| agent | session | tasks | gen | IDs | worktree |
|---|---|---|---|---|---|
| AK (perf) | s458ak | #816 PHASE 4: accessor-dispatch fast paths MEASURED-FIRST (sprof arrhash/arrfill; target arrhash ≤1.0×) → #815 overload negative path → #811 box-set fast path → §4.4 proven arms (foreach-LIST raw binding + slice copy positions; slices 5.16× is the scoreboard; EMISSION work) → #841 probe-first (in its files); battery green BOTH gate settings per step | v2-480 if emission | 860–869 | agent-aa5937714ad7107be |
| AL | s458al | #840 feature/warnings tables at the SHIM layer (lib/warnings.pm = the artifact source — regen owed; full sweep NON-OPTIONAL for lib/) → census remainder families (19/63; #564 excluded) → #851 → #852 if budget (gate-SET if classifier widens) | v2-490 | 870–879 | agent-ab6c440455333edf2 |

**ROUND 15 MERGED (s455f, 2026-08-31, main `731d4be` PUSHED, gen v2-490 =
AL's; AK runtime-only took no bump).  NO AGENTS IN FLIGHT.**
**arrhash 0.67× — BEATS perl** (2.17× slower 3 days ago); arrfill 1.51×,
slices 3.28×, ovlsub 3.43×; won rows held — Fable re-measured on merged
main.  AL: #840 (real experimental.pm loads, shim DELETED; tables =
language data per s408; #875 filed = dynamic warnings::register), #872,
#851a; census 19/63 → **18/58 verified**; the announce lesson (run-time
diagnostic = sweep question, never gate question) in DECIDED.  AK: phase-4
runtime half all sprof-attributed (%p-vec-data 44% of arrfill;
%p-fixnum-string; #815 inlined ×7; #811); **#841 resolved — perl aliases
blessed-hash elements, refusal deleted (E13)**; #862 = the deferred
emission arms WITH numbers (foreach-LIST proven arm ~40%, own session,
sweep-is-gate).  Filed #860/#861 (backslash-loses on \\\$\$h{k} —
silent-wrong)/#862/#870 (vcmp, promote)/#871/#873/#874/#875.
Round 16 = #862 + #870 + #861 + fillers (#873/#874/#875/#852) + #812.

**ROUND 16 LAUNCHED s459 (Fable, 2026-08-31), base `e9296cb` (main gen
v2-490; the #876 install-matrix commit, pushed) — IN FLIGHT until a merge
review records otherwise.**  TWO Opus agents (USER: "start 2 Opus subjobs
with the things in the queue"), models PINNED opus, standing rules as
rounds 5–15 (no push/merge, own session-log section, rebase before final
gate, baselines row-by-row with cause only; siblings warned disjoint — AM
owns VarAnnotator/Passes/foreach lowering + aggregate/box runtime regions,
AN owns lib/version.pm + PExpr backslash-term + capture/regex-announce/
warnings runtime regions):

| agent | session | tasks | gen | IDs |
|---|---|---|---|---|
| AM (perf) | s459am | #862 ARM A first (foreach-LIST read-only raw binding; #810 rides; full sweep IS the gate + gate-SET both populations + Kind-A registry + elem-alias battery both PCL_RAW_ELEMS + bench before/after) → re-measure slices before any ARM B → #814 (regexg bench row honest) → #812 measurement only | v2-500 | 880–889 |
| AN | s459an | #870 (vcmp componentwise + numify; lib/ ⇒ full sweep) → #861 (backslash-loses; read pexpr-term-parsing-review first) → fillers #875 → #874 (announce = sweep question) → #873 (design-or-ship, no scattered guards) → #852 if budget (gate-SET if classifier widens) | v2-510 if emission | 890–899 |

**ROUND 16 COMPLETE (s459f, 2026-08-31): BOTH MERGED + the Sonnet README
job (fd80583), legs all clean, record pushed.  NO AGENTS IN FLIGHT.**
AM `4335e2e` ff (#862 ARM A `foreach-raw`, feread 0.71×→0.46×; #814 =
bench-exec was a SEVENTH runner off PCLSbcl.pm timing a stack crash —
regexg honest 2.18×; #880; ARM B closed by measurement, #882 ratified);
AN rebased → `f4af1fc` (#870 version tuple compare, 3864-check battery;
#861 `\` stops the cast run — one line in `_cast_run_start`; #874
compile-time regex announce; #875 warnings allocation; #873 SIZED to three
spellings, not shipped; #852 skipped).  Fable legs: 22 own probes vs perl
(both worktrees), pat_advanced splice 951/729 verified by own run, cold
gate 191/6441 xs-only (proven by PCLXS_DIR=/nonexistent PASS 191/6427),
sweeps GATE clean TOTAL 18340 (+0), bench re-measured all won rows held.
Gen **v2-510** = AN's.  Filed this round: #881 #883 (AM), #890 #891 #892
(AN).  Next-round fillers: #891 + #890 head the pool; #873
one-predicate-sized; #862 E2c′ remains.

**ROUND 17 LAUNCHED s460 (Fable, 2026-09-01), base `6fa3757` (gen v2-510,
CI green) — IN FLIGHT until a merge review records otherwise.**  TWO Opus
agents (USER: "a couple more Opus subjobs"), models PINNED opus, standing
rules as rounds 5–16 (no push/merge, own session-log section, rebase before
final gate, baselines row-by-row with cause only; siblings warned
disjoint — AO owns VarAnnotator/Passes/module-facts + raw-verdict/coerce
runtime regions, AP owns list-assign (`p-list-=`/snapshot) + capture-var
runtime regions, PExpr backslash/slice, bareword-FH classifier):

| agent | session | tasks | gen | IDs |
|---|---|---|---|---|
| AO | s460ao | #890 FIRST (raw-numeric freeze on module-provided overloads — fatal; bar = reproducer prints like perl AND won bench rows hold; if the sound fix costs the wins, STOP and write the measured trade-off as a Fable ask) → #862 E2c′ (writes_args-gated raw @_ pass, both PCL_RAW_ELEMS settings) → #883 measure-first → #812 measurement only | v2-520 | 900–909 |
| AP | s460ap | #891 FIRST (ref swap lost in `p-list-=` — verify AN's snapshot diagnosis before fixing; full family probes; cl/ ⇒ full sweep + op/ leg) → #892 (`\` distributes over slices; read pexpr-term-parsing-review first) → #873 (the SIZED three-spelling read-only-capture fix; open-on-defined is legal like perl) → #852 if budget (gate-SET if classifier widens) | v2-530 | 910–919 |

**ROUND 17 COMPLETE (s460f, 2026-09-01, main `19d4283` code + `b8bbcfc`
record, PUSHED, gen v2-530 = AP's).  Both merged with Fable probes (AO 5
SAME; AP 9 SAME), gate PASS 191/6443 xs-masked, sweep 18342 (+2), bench:
collatz 0.26×, won rows held.  E2c′ ruled NOT shipped (#862 closed).  NO
round-17 agents in flight.**

**ROUND 18 LAUNCHED s460/s461 (Fable, 2026-09-01), base `33a71c9` — IN
FLIGHT until a merge review records otherwise.**  USER priority ruling
(DECIDED s460f): speed + correctness first; JS prototype waits on a quiet
IR.  TWO Opus agents, models PINNED opus, standing rules as rounds 5–17
(disjoint: AQ owns VarAnnotator/sub_info/Passes + symref resolver +
list-assign macro; AR owns census statement lowering + capture-write
runtime sites + overload increment):

| agent | session | tasks | gen | IDs |
|---|---|---|---|---|
| AQ (perf) | s461aq | #77 return-family transfer (verdict work — sweep IS the gate + gate-SET + Kind-A + bench; route new proven writes through %pcl-raw-freeze-unsafe-p, never a hard freeze) → #812 name→symbol memo (bench symref) → #910 common-assignment snapshot (swap battery must hold; conservative side = snapshot) → #881 filler | v2-540 | 920–929 |
| AR | s461ar | census push: sub_lval.t 33-drop lvalue-sub family DIAGNOSE-AND-SIZE first (timeboxed; ship if bounded, else write sizing; drops stay LOUD) → singles harvest (17 files, 1–2 drops each; census edited row-by-row, re-measured) → #900 overload ++ autogeneration (reuse #890's MRO predicate) → #911 if budget (runtime write sites; compile-time die is provably wrong) | v2-550 | 930–939 |

Merge-review owes: both diffs + own probes vs perl; census re-measured on
the merged tree; sweeps per change class; bench re-measure (the #77/#812
rows + won rows held); gen renumber/adopt + artifacts on final tree; push
+ records.

**ROUND 18 USER-STOPPED same evening (2026-09-01, end of day), shortly
after launch.  Both agents sent the graceful stop order (commit worktree
state + `STOP-RECORD` commit + 1-para report — the round-12 pattern);
NOTHING MERGED; worktrees KEPT.  NEXT SESSION, FIRST ACTION: `git worktree
list`, then `git -C <wt> log`/`status`/`diff` for BOTH round-18 worktrees —
READ THE ACTUAL DIFF FIRST (a stop-record's description can be wrong,
round-4 lesson), then resume each as a fresh Opus agent in its KEPT
worktree with read-the-diff-first instructions, or merge if one actually
finished.  Main is clean and pushed at `d4099fc` (gen v2-530), CI green.**

AQ's stop record (received before shutdown): commit `00d1ef0`, ONE file
(cl/pcl-runtime.lisp), un-rebased.  **#812 DONE + MEASURED — symref
0.2215s/9.56× → 0.0315s/1.44×** (%p-symref-symbol = equal-table memo on
the name string → #(sigil pkg symbol); misses never stored; `shadow` in
%p-symref-intern clears the table; 2 probe files byte-identical to perl,
#525+#685 families).  **#910 CODE-COMPLETE, NEVER RUN ONCE** — resumer
must run the 9-probe swap battery + aassign-01.t rows 37–40 + the
micro-bench before trusting; the three #910 hunks are droppable without
touching #812.  #77/#881 NOT STARTED (the commit message carries the #77
machinery map: hook = VarAnnotator.pm:1263 funcall-root `return 0`;
sub_info reaches both consumers; clone `_sub_ctx_insensitive`; do not
re-freeze past the #890 decline).  UNRUN BARS: prove-core, full sweep
(mandatory, cl/ change), companions, PCL_OPT=none, rebase.

AR's stop record (received at shutdown): commit `59eb514`, clean,
un-rebased; the diff to read first is ~40 lines in Pl/Parser2.pm.
**The lvalue family SIZED not shipped, filed #930** — measured vs perl:
of EIGHT lvalue-sub return shapes only `sub f :lvalue { $x }` works today
(the box model returns the box; `$_[0]`, `substr`, `vec`, `$h{k}`,
`$a[0]`, `${\shift}`, `return $x` all lose identity — the aggregate
shapes because s455 raw elements), so a half-fix = 33 loud drops → 33
SILENT WRONGS; it is lvalue-CONTEXT propagation;
`Pl::Environment.lvalue_subs/is_lvalue_sub/add_lvalue_sub` exist as DEAD
API.  Census classified: 39/58 lvalue, 4 indirect-object (ruled MAYBE
LATER), 2 Mojo/#564, 13 singles (6 deliberate parser-torture).  **#931
FIXED IN CODE, one bar run**: `_repair_word_match` must run to FIXPOINT
(same bug s449 fixed in `_repair_glob_pattern_cascade`) + a repair-ORDER
silent-wrong exposed (`_repair_glob_multiply` was rewriting pattern text
tokenized as code — a guard must assert BOTH rows answer 1, not merely
drop-gone).  corpus-diff CLEAN over 111 (the only bar run — and it cannot
see t/re/pat.t, a perl-t population).  UNRUN: prove-core, sweep,
emission-ab, drop-census re-measure (census UNVERIFIED at 18/58), the
guard row, the rule-13 PPI addendum (§11 + ppi-bug-report.t row), gen
bump if needed.  #900/#911 NOT STARTED.

**ROUND 18 RESUMED (s461, Fable, 2026-09-01): both worktrees' diffs READ FIRST
and verified to MATCH their stop records (AQ `00d1ef0` = #812 done+measured +
#910 code-complete-unrun, cl/pcl-runtime.lisp only; AR `59eb514` = #931
fixpoint+run-order fix, ~40 lines Pl/Parser2.pm, only corpus-diff run).  Each
resumed as a FRESH Opus agent (model pinned opus) in its KEPT worktree with
read-the-diff-first instructions + the remaining task list (AQ: #910 bars
first — 9-probe swap battery/aassign rows/micro-bench, drop the 3 hunks if
unhappy — then standard bars incl. MANDATORY full sweep, then #77 with the
stop-record machinery map + #881; AR: #931 bars first — both-rows-answer-1
guard, PPI §11 addendum + ppi-bug-report.t row, prove-core, emission-ab,
census re-measure, companion re/ leg — then singles harvest, #900, #911).
IN FLIGHT until a merge review records otherwise.  The four stale
fully-merged worktrees from rounds 16–17 (AO/AN/AM/AP) were pruned the same
session.**

**AQ MERGED (s461, Fable, 2026-09-01, main `72930b7` ff, gen v2-540 =
AQ's, artifacts regenerated by AQ; worktree pruned).**  #812 shipped
(symref 9.01×→1.42×); #910 shipped in a BETTER shape than the draft (the
commonality test rides %p-flatten-list's own walk against a DYNAMIC-EXTENT
target list — the drafted per-target %p-protect-target pass measured +0.0%
because a generic AREF on the adjustable src-vec costs what a box costs,
filed #923; OO entry −5.0%, consing −118 B/call); #77 half (a) = Kind-A
`raw-return-family` (ONE oracle `Pl::VarAnnotator::value_family`, ONE walk
`_sub_return_facts`, ONE predicate `_tail_below_assign_prec` for both
native-root models) + TWO pre-existing silent-wrongs fixed (unary `+` is a
NO-OP and is now value-transparent; the parenless-list-op tail `$c = two
1, 2` stored nothing once provable); #881 strcat N→20M (honest 2.20×).
AQ bars all green (gate 191/6462; sweep +0 clean drops 5 = census;
gate-SET 638×2 zero; companion op/ zero movers; PCL_OPT legs identical).
Fable review: both diffs read, probes SAME vs perl (#910 9-shape battery
incl. runtime-common `f($q,$p)`; #77 shapes; gate-off identical); the one
probe diff = #920, CONFIRMED pre-existing on main.  Filed by AQ:
#920–#924.  Asks for Fable: (1) #77's population effect is 4 sites/405
files — the silent-wrong fixes may be most of its value; (2) #922 design
question: should the raw-slot verdict consult use classes and decline for
stringy uses (box caches its string form, raw slot 9.9% slower on hash-key
use)?

**ROUND 18 COMPLETE (s461f, 2026-09-01): AR MERGED TOO (`b583f77` ff —
it had already rebased itself onto 72930b7) + one Fable review fix
`5fc8cfd` (docstring #933→#934); record `4c354bc` PUSHED.  NO AGENTS IN
FLIGHT; all worktrees pruned.**  AR: #931 closed with every bar (guard
asserts both ANSWERS; ppi-upstream-bugs §11b + ppi-bug-report.t Bug 8b;
census 18/58 → 17/57 by edit), #900 (++/--/+=/-= autogenerate from +/-
and keep the OBJECT both paths; postfix = copy of the REFERENCE; the
measured `*p-any-overload-registered*` guard), #911 (s///+tr/// runtime
write sites die perl's read-only death; the one perl-accepts arrival →
#939), #930 sized-not-shipped (39 lvalue drops stay loud).  Asks ruled
(DECIDED §s461f): #911 flip stands; re/pat.t 231/138 splice stays (#935
fragile-row note); #934 deferral ratified.  Fable probes all SAME vs perl
(#900 10-shape, #911 12-shape, #931 pairs); **#940 filed from a Fable
probe** (`ok /q*/, LIST` → q* quote-operator lex, RAW PERL TEXT in the
emitted CL, pre-existing).  Final legs: cold gate 191/6480 xs-only (13);
sweep 18342 (+0) clean; census 16/55 in-repo = blessing; bench won rows
held, symref 1.37×.  Gen **v2-540** (AR no emission move; artifacts =
AQ's).  Filed this round: #920–#924 (AQ), #932–#939 (AR), #940 (Fable).
Round-19 filler pool: #922 design question (stringy raw slots), #924
(#77 half b — population effect was small, may decline), #935, #936,
#938 singles, #939, #940, #934, #852.

**ROUND 19 LAUNCHED s461/s462 (Fable, 2026-09-01), base `4c354bc` (main gen
v2-540, pushed, CI in_progress at launch) — IN FLIGHT until a merge review
records otherwise.**  TWO Opus agents (USER: "two more Opus-jobs"), models
PINNED opus, Agent-tool worktrees, standing rules as rounds 5–18 (no
push/merge, own session-log section, rebase onto main before final gate,
baselines/census row-by-row with cause only; disjoint: AS owns VarAnnotator
verdict + sub_info facts + Passes + runtime list-assign/flatten + hash-read;
AT owns regex-subst/tr write sites + read-only mark + overload dispatch/
fallback + ExprToCL `=~` target lowering + Parser2 repair block):

| agent | session | tasks | gen | IDs |
|---|---|---|---|---|
| AS (perf) | s462as | **MERGED first (main `12eacb5`, ff; worktree pruned; runtime-only, NO gen bump)**: #923 = sized flattener (separate pre-scan %p-flatten-sized-p — element-by-element bail would run a tied FETCH twice, measured; ftype declaim carries simple-vector to the generated program's call sites; OO entry −15.4%, swap −30%); #922 direction (a) = %p-fixnum-string's most-negative-fixnum arm (the `(- n)` type derivation made every TRUNCATE generic; numstr −8.2%, raw-vs-boxed hash-key gap 10.6%→7.4%), direction (b) DECLINED on a population count (ZERO hash-key raw slots in 1159); #924 SIZED to 3/535 files, RECOMMEND DECLINE, left pending with re-open condition.  Filed #950 (runtime compiles at default optimize policy — re-measure quiet).  Bars: gate 191/6483 cold, sweep 18342 (+0) clean, companion op/+io/ zero real movers, PCL_OPT=none + PCL_RAW_ELEMS=0 green.  Fable probes SAME (tie-FETCH-once counts, expansion shapes, fixnum boundaries incl. most-negative-fixnum, swap battery re-run). | none used | 950 filed |
| AT | s462at | #939 substr-lvalue `=~` target (bind p-substr-lvalue-cell, rule 11; emission → corpus/emission-ab; op/substr.t splice back) → #936 read-only mark at 4 production sites (reuse #873's mark) → #934 overload fallback death (evidence bar per DECIDED §s461f: cpan/Test2 A/B + op/ leg first) → #940 if budget — **USER-STOPPED at end (2026-09-01 break) and stopped GRACEFULLY with the work essentially DONE: worktree `agent-a5c84d24df2d2db9a` KEPT, HEAD `c57c0cb` (stop record) on `ba7a61b` (the work), clean, UN-REBASED on 4c354bc.  Report claims: #939 shipped (ONE %MAGIC_LVALUE_BASE table + `_write_through_form`; p-arylen-lvalue-cell; two rule-12 setter guards; substr/vec/pos `=~` write-through), #934 shipped (fallback read at last, BOTH paths — also closed a #900 hole: the raw `$x++` path never asked for a `++` handler), #940 shipped BOTH halves (raw text → `(p-unparsable-quote …)`; quote-token derail repaired NEXT-TOKEN-strict — the whole-statement scan broke t/re/pat.t:113, caught by the 1029-file A/B), #936 SIZED AND DECLINED with measurement (task carries it).  Bars it ran: gate 191/6495 xs-only; corpus-diff 1/111 + emission A/B 1029 = 2 DIFF both #939's probed; gate-SET 638×2 identical; sweep 18343 drops 5 = census; ~130 probes; gen v2-560 + artifacts; pass-baseline substr.t 347/3→348/2 + perl-suite-run op/substr.t spliced.  THE ONE UNVERIFIED BAR: op/+re/ companion cut at 231/301 — the ~70-file re/ TAIL must be re-run at merge.  Filed #960–#962.  MERGE REVIEW OWES: read the diff first (round-4 — verify the claims), Fable probes, rebase over 12eacb5 (watch misc-fixes-02.t tail + DECIDED/session-log tops), re/ tail, batch legs incl. bench, gen adopt (v2-560 > AS's none), push + records.** | v2-560 (used, artifacts regenerated) | 960–969 (960–962 filed) |

Merge-review owes: both diffs + own probes vs perl; census re-measured;
sweeps per change class (both bundles carry cl/ changes → full sweep each
or once over the merged batch); bench re-measure (won rows + #923/#922
rows); companion op/+re/ legs; gate-SET where a checker/decline moves;
gen renumber/adopt above v2-540 + artifacts on final tree; push + records.

**How to apply:** if this memory is recalled and the merge is not recorded in
DECIDED/session-log, run `git worktree list` and `git -C <wt> status`/`log`
for each agent worktree FIRST â a dead agent's uncommitted diff is the
deliverable; read it before redoing its task.  Delete or mark this file DONE
when all merges are recorded.

**ROUND 19 COMPLETE (s463f, Fable, 2026-09-01): AT MERGED as main `6e6f191`.**
Review method (round-4 rule): read the full code diff first (ExprToCL
%MAGIC_LVALUE_BASE + _write_through_form; Parser2 _manufactured_quote_close;
runtime %p-require-writable-target / %p-write-match-target / %p-overload-
fallback-of / %p-incdec-autogen / p-unparsable-quote), then rebased the kept
worktree over 12eacb5 (conflicts ONLY the DECIDED/session-log tops — both
sections kept, AT's above AS's), paren check, three probe files vs perl
5.40.3 (#939 19 rows SAME; #934 25 rows SAME except the #961 shape; #940 11
rows SAME after dropping my two INVALID-PERL rows: `substr("lit",..) =~ s///`
is a compile error in perl, and `declared_sub / 3, "\n"` is "Search pattern
not terminated" in perl too).  Bench (quiet, K=3): strcat 2.21x, ovlsub
3.36x (abs 0.129 s < record), regexg 2.21x — held.  LESSONS: (1) `pkill -f`
/ `pgrep -f` with a pattern whose literal text ALSO appears elsewhere in the
same command line (a heredoc or a setsid string) kills the calling shell —
exit 144, twice; write long chains to a SCRIPT FILE and launch `setsid
script &` from a command that does not mention the pattern; (2) the Agent
tool's worktree isolation branched from the SESSION-START commit (12eacb5),
not from main's HEAD at launch (6e6f191) — tell every agent its real base
and to rebase first; (3) a Fable probe can contain invalid Perl — run the
perl side alone first and read its stderr before calling a DIFF a finding.

| agent | session | tasks | gen | IDs | worktree |
|---|---|---|---|---|---|
| AU (correctness) | s463au | #962(=#459) failed capture-less m// in LIST ctx → #960 overload BINARY refusal + substr target as place → #920 `return EXPR if FALSE` → #938(3) `is y, 43` lexsub repair if budget | v2-580 if emission moves | 970–979 | agent-a10f0eaeb611f3da9 |
| AV (perf) | s463av | #950 speed-3 policy measured at K=5 + note harvest + core compile-time → §0.2f full bench re-tabulation → worst lagging non-pack/ovlsub/regexg row: file (980+) with sb-sprof profile, then fix (runtime, or a registered Kind-A emission) | v2-590 if emission moves | 980–989 | agent-a9bfc5808a4d4ac80 |

Ownership: AU = do-regex-match/subst, overload dispatch family, substr/vec/
pos setters + 4-arg, return-family lowering, Parser2 repair block, ExprToCL
`=~`-target/substr-assign; AV = VarAnnotator, Passes, runtime list-assign/
flatten/array-fill/slice/hash-read/element paths, coercion helpers,
bench-exec.pl, faster-codegen-suggestions.md.  Merge checklist as before:
read diff → probes → rebase → ff → legs on the final tree (gate, sweep,
companion dirs touched, bench) → gen adopt + artifacts stamp check → records
(DECIDED + session-log + this file) → push → CI check.

**ROUND 20 COMPLETE (s463f, Fable, 2026-09-02): AU MERGED (`aafb02c`, ff, gen v2-581) and AV MERGED (`fc96b08`, ff, runtime-only); records + splices pushed as main `a8b4043`.**  AU: #962=#459 (`%p-empty-list`, the ONE producer of the empty-list value), #960(b) (`_lvalue_target_form`, ONE rewriter for the three write spellings of a magic-lvalue window; #960(a) DECLINED with evidence → #972), #920 (`_modifier_ret_form`); 3 sweeps, 1145-file A/B, four guard files; filed #970–#972.  AV: #950 `(speed 3)` at the top of the runtime (six interleaved A/B runs, sign-of-six verdict; +1.1 s core build), #980 (pack/unpack stubs through `%pcl-def-ext-stub`/SYMBOL-FUNCTION — the `(> speed debug)` local-self-call hazard; SOURCE-invariant guard), #981 (array-fill capacity; `listcopy` row), §0.2f board; `BENCH_RT_B` runtime A/B mode + interleaved series = standing form; filed #980–#986.  Both worktrees had started at 12eacb5 (session-start commit) and rebased on instruction; both final rebases were clean ff.  Batch legs on fc96b08: gate 191/6521 xs-only, sweep 18346 (+0), companion --all --quick 528 files with two attributed movers spliced (#962 opsubs.t, #939 runenv_hashseed.t).  Rulings in DECIDED §s463f parts 1–3.  Fable-filed this session: #963 (tie STORE of a ref constructor), #964 (return protocol), #965 (132 inline ok(1,SKIP) rows), #966 (pos after /g on an element), #967 (value of substr()=V).  NO AGENTS IN FLIGHT.  NEXT ROUND (21, USER-ordered): the return protocol SOLO (#964 + #930 scalar half), one owner of the return family; see MEMORY.md state line for the queue after it.

**ROUND 21 IN FLIGHT (s464, Fable, 2026-09-02): ONE agent, solo, on the RETURN PROTOCOL.**  USER asked how common `:lvalue` subs are on CPAN (measured: 2 real definitions in ~1000 core+site .pm — JSON::PP::incr_text, Thread::Queue::limit; 0 in cpan-tests) and DEFERRED #930 ("worth doing, but push it forward"); ordered #964 alone: "fix so the local subs aren't :lvalue by default".  Fable measured the leak on a8b4043 (probe scratchpad p964/leak.pl, 38 rows: 24 leak, identical under PCL_OPT=none; `return $str` aliases because p-return-value's container arm tests `(vectorp v)` and a CL string is a vector) and wrote the DESIGN into task #964: ONE runtime function `%p-leavesub` (perl's pp_leavesub rule: :void unchanged; scalar plain box → unbox, ref/blessed/container box → fresh box via p-copy-scalar-arg's body; list → fresh vector, per-element rule, NEVER in place because `&f;`/`goto &f` share @_) + ONE macro `p-sub-frame` = `(%p-leavesub (catch :p-return …))` used by p-sub and EMITTED at the anon-wrapper's 3 sites (Parser2 ×2, Parser.pm ×1).  NOT sites: p-sort-cmp, eval frames (perl copies at pp_leaveeval too — a SIBLING rule the agent files), p-goto-sub.  Bench bar: all bench-exec rows main vs branch, raw-return rows within noise; escape = Kind-A copy-elision keyed on _sub_return_facts only if a row moves.

| agent | session | tasks | gen | worktree |
|---|---|---|---|---|
| s464a (correctness, solo) | s464a | #964 return copy at the frame exit + p-return-value string arm + guards (return-copy-01.t, foreach-aliasing-01.t:103 de-vacuated) + files the eval-frame sibling | v2-590 (emission moves at the anon wrapper) | /home/bernt/pcl/.claude/worktrees/agent-a9203dffcad74e505 |

Expected companion mover: op/sub.t 53/12 → 54/11.  Merge checklist as before (read diff → probes → rebase → ff → legs → gen/artifacts → records → push).  After merge: record in DECIDED (s464) the CPAN measurement + the #930 deferral + the protocol pointer; then the round-21 pool #972 → #966/#967/#971 → #965 chunks → #963; perf pool #982/#985/#986/#983.

**ROUND 21 COMPLETE (s464, Fable, 2026-09-02): s464a + s464b MERGED ff as main `4fd661b`; records pushed as `80b715c` (CI to check next session).**  #964 the return protocol: `%p-leavesub` + `p-sub-frame` (one rule at the frame exit; the anon wrapper EMITS the macro at 3 sites); gen v2-590; artifacts regenerated.  Review method (standing): read the full diff → the 38-row leak.pl AND a second 28-row edge.pl battery vs perl on BOTH PCL_OPT paths (the edge battery found the s464b residue: a nested aggregate in a returned list temp is a deferred `p-flatten-marker` around the LIVE array) → rebase → ff → legs on the final tree (gate 192/6527 xs-only; sweep 18346 +0; bench K=3 spot-check inside the ±6 % band) → records → push.  LESSONS: (1) the bench tool's per-row noise is ±5 % (symref ±14 %) — measured by a byte-identical-runtime CONTROL A/B; read nothing smaller; (2) `git worktree remove` of the directory the shell is cd'd into makes the command "fail" with a getcwd error after the work is done — remove from the repo root; (3) a fix that makes values REAL exposes accidental passes: 25 companion rows through `: lvalue` subs now fail honestly, spliced with cause #930 (USER deferred #930 this session).  The agent worktree `agent-a9203dffcad74e505` is merged and can be pruned.  **NEXT SESSION (s465): PRESENT `docs/plan-test-audit-s464.md` (#993) — re-measure its §2 on `4fd661b` first; then the round-21 pool #972 → #966/#967/#971 → #965 → #963; perf pool #982/#985/#986/#983; #987 (eval frames) joins the correctness pool.**

**ROUND 22 IN FLIGHT (s464, Fable, 2026-09-02, USER: "do a run of a couple of normal sub-jobs with Opus"): TWO agents, one correctness + one perf (the s452 shape), launched from main `80b715c` — the Agent tool branched them from the session-start commit a8b4043, both told to `git rebase 80b715c` FIRST.**

| agent | session | tasks | IDs | gen if emission moves | worktree |
|---|---|---|---|---|---|
| AW (correctness) | s464aw | #972 (bitwise overload dispatch → nomethod w/ 4th arg → `""` derived from `0+`; #960(a) refusal only if it falls out for free) → #987 (eval frames copy at exit, reuses `%p-leavesub`) | 1000–1009 | v2-600 | .claude/worktrees/agent-aca4f512c037ba6b1 |
| AX (perf) | s464ax | #985 (`%p-flatten-slice-args` single-vector fast path) → #982 (numeric-key string cache; "not worth it" is a valid measured outcome) | 1010–1019 | v2-610 | .claude/worktrees/agent-a40c254eb3909ed81 |

Ownership: AW = overload dispatch family (`%with-binary-overload`, bitwise ops, `%def-overloaded-cmp`, stringify's OBJECT arm), eval-frame catches; AX = slices, `%p-fixnum-string`/number→string arms, hash-key path, bench-exec.pl, faster-codegen-suggestions.md.  Known overlap risk: `stringify-value`/`to-string` — different arms; reconcile at merge.  Merge checklist as before (read diff → probes vs perl on both PCL_OPT paths → rebase → ff → legs on the final tree (gate, sweep, companion dirs touched, bench ALONE on the box) → gen/artifacts → records → push → CI).  Round-21 worktree `agent-a9203dffcad74e505` is merged; prune it.
| AY (review, added on USER instruction "review the failing tests … to see if we have missed bugs like that sub calling followed :lvalue") | s464ay | #965 (132 inline SKIPs: restore-if-passes / verify the written reason / file) → the 58 skip-registry reasons → the 695 blessed sweep rows clustered + attributed (`docs/blessed-fails-review-s464.md`) → companion op/ per-row clustering (first pass) | 1020–1039 | none (tests/baselines/docs/tasks only; NO runtime/compiler edits) | .claude/worktrees/agent-afc40626938966acc |

**ROUND 22 PROGRESS (s464): AW MERGED ff as main `3d666ea`** (fe93a89 #972: ten bitwise keys dispatch through `%with-binary-overload`/new `%with-unary-overload`; `nomethod` with the 4th arg at every no-handler point incl. #934's ++ refusal; the conversion chain `%p-conversion-handler` (own → derived `""`↔`0+`↔`bool` → nomethod), a blessed ref is no longer unconditionally TRUE; comparison handlers return their value verbatim; `<<=`/`>>=` delegate (had no clamp).  3d666ea #987: `%p-leavesub` at p-eval-block + the string-eval catch).  Fable review: diff read in full; 37-row ovl.pl probe (scratchpad p964/) — 33/37 identical to perl on both PCL_OPT paths, the 4 diffs are all "perl DIES, PCL runs on" = the declined #960(a) refusal (fallback 0 stringify/bool, `+` on a `""`-only class, `++` on a nomethod class without a copy constructor).  Filed by AW: #1000–#1008 (#1002 `join` flattens a ref arg; #1003 `"$a$a"`; #1004 ++ handler return; #1007 `$aryref x N`; #1008 `do { $lexical }` no alias — pre-existing).  Coverage hole for #993: perl's `t/lib/overload_*.t` are NOT in the companion scan set (overload_nomethod.t was 0/3 on main).  Legs on main after the merge: gate running; sweep next; ovlsub bench ALONE after AX/AY finish.  AX, AY still in flight.

**ROUND 22 COMPLETE (s464, Fable, 2026-09-02): AW MERGED `3d666ea`, AX MERGED `300d47f`, AY MERGED `ea34c0f` (all ff after rebase); records `be5c611` + fix-up `1370533` PUSHED (CI to check).**  Legs on the final tree: gate 193/6557 xs-only; sweep TOTAL 18266 (+0; 18346 → 18265 by AY's honest restorations +1 AW), GATE clean, drops 5; bench alone: slices 3.01× → 2.54×, others inside the band.  MERGE LESSONS: (1) `baselines/fail-baseline.tsv` is BINARY to git (NUL bytes) — a rebase conflict has NO markers; merge it as a three-way ROW SET from the index stages (:1 base, :2 ours, :3 theirs): result = theirs − (base − ours) + (ours − base) — scratchpad `p964/merge-tsv.pl`; (2) doc-top conflicts (DECIDED/session-log) = keep both sides, ours first; (3) a records paste via `perl -0pi` with an env var must SET the var in the SAME invocation — an unset $ENV{X} substitutes EMPTY and the anchor regex silently no-ops; verify with grep -c before committing (be5c611 shipped an empty legs line + no session-log entry; fixed in 1370533); (4) an agent's `scratch/` probe files are cited by the tasks it files — copy them out before pruning (AY's 128 files → scratchpad `p964/probes-agent-afc40626938966acc`; the AY worktree is KEPT; AW/AX pruned; the round-21 worktree is harness-locked).  Filed this session: #987 #988 #993–#996 #1000–#1008 #1010–#1012 #1020–#1033.  **NEXT SESSION = (1) the found-bugs report (DECIDED §s464 part 2 + docs/blessed-fails-review-s464.md) → (2) re-measure + PRESENT docs/plan-test-audit-s464.md (#993) → USER decisions → then the pool: #1028 (229 sweep rows, one cause) is the biggest single correctness prize; #994/#995/#74 the perf ones.**

**ROUND 23 IN FLIGHT (s465, Fable, 2026-09-02; USER: "start working on the stack of tasks"): THREE Opus agents (models pinned opus), worktrees branched from the session-start commit 1370533, each told to `git rebase main` (1fed80b) FIRST.**  The USER took all four audit-plan §5 decisions as recommended the same day (DECIDED §s465): interleaved standing slot; Unicode class out (#1036); wholesale refresh (#1038); statement-level refusal rule (#1037).

| agent | letter | job | gen string | notes |
|---|---|---|---|---|
| AZ (audit slot) | s465az | phase 0 instruments I1–I4 of `docs/plan-test-audit-s464.md` §3 — runner/baseline work only (row-level companion bless `baselines/perl-suite-fails.tsv`; planned−produced column blessed with cause; `cause` column in fail-baseline.tsv from the review's §3; NOT-RUN stamps + the I4 cadence line for CLAUDE.md, which Fable edits) | none | needs a full `--all --jobs 4` companion run (30–60 min) on a loaded box — load movers expected (io/pvbm.t) |
| BA (correctness) | s465ba | #1028 (bitwise ops numify non-plain operands; ONE dispatch, rule-12 read; 229 sweep + 259 companion rows) → #1032 (bareword handle in stat/filetest slot; #452's predicate family; ~810 companion rows) → #1037 if budget | v2-600 if emission moves | cl/ change ⇒ full sweep mandatory; baselines edited row by row |
| BB (perf) | s465bb | #994 `tail-return` Kind-A emission (tail `return EXPR` = the tail expression; %p-leavesub still wraps) → #995 measured | v2-610 (artifacts regenerate) | bench ALONE on the box with a control pair; corpus-diff explained by shape class |

Merge checklist as before: read the full diff → Fable probes vs perl on both PCL_OPT paths → rebase → ff → legs on the final tree (gate, sweep, companion dirs touched, bench alone) → gen renumber FRESH above the highest agent string → artifacts → records (doc-top conflicts: keep both, ours first; fail-baseline.tsv is BINARY: three-way row-set merge, scratchpad p964/merge-tsv.pl) → push → CI.  Fable's own item after the merge: #1035 (export the compiler's facts into the IR) — emission-only, waits for the round because of the generation collision.  AY's worktree `agent-afc40626938966acc` and round-21's `agent-a9203dffcad74e505` are still listed; AY's is KEPT for its scratch/ probes, the round-21 one is prunable.

**BB MERGED (s465, 2026-09-03): ff as main `73edcff` (gen v2-610, artifacts regenerated by the agent).**  Fable review: diff read (40 lines Parser2 + 1 registry line + 2 runtime macros `p-tail-value` / `p-return-empty`); own 40-row battery vs perl on default / none / -tail-return — identical except two PRE-EXISTING rows (#1039 array-in-string-context, filed by BB; **#1045 `goto &NAME` under `insensitive-call`**, filed by Fable: a sub ending in `goto &ra` is classified context-insensitive so the list-context call gets the COUNT — right under PCL_OPT=none / -insensitive-call); gate on BB's tree 194/6567 xs-only.  **LESSON: BB COMMITTED its 51 `scratch/` files** (scratch/ is untracked on main, not gitignored) — dropped from the index with `git rm -r --cached scratch` + `--amend` BEFORE the ff; copy in scratchpad `p994/scratch-bb`; the worktree keeps them on disk.  Tell future agents explicitly: scratch/ is never committed.  Bench-alone leg deferred to round end (AZ + BA still loading the box).  Sibling agents filed #1040–#1044 meanwhile (BA: #1040 numeric-looking strings in bitwise ops, #1042 `$^T` from the saved core, #1043 scalar-context stat, #1044 bareword handle family; AZ: #1041 sweep-diff keys unnamed rows on file+description).

**BA MERGED (s465, 2026-09-03): ff as main `0e7c5e3` (four commits rewritten WITHOUT scratch/ by `git filter-branch --index-filter 'git rm -r --cached scratch' main..HEAD` — BA had committed 55 scratch files across its commits; the rewrite removes the previously-tracked files from the worktree's DISK too, so copy scratch/ out FIRST — scratchpad `p1028/scratch-ba`).**  Runtime-only: #1028 (`%p-bitwise-operand-kind`, one classifier, rule-12 die; bop.t 253/256 → 480/29, 224 baseline rows left by edit, non-bop rows verified byte-identical) + #1032 (`%define-fh-slot-op` makes stat/lstat/26 filetests/write macros over `%…-impl` bodies through the existing `%p-fh-arg` contract with the call-form arm OFF; `_` excluded).  Fable legs: gate 196/6696 xs-only; probe batteries bit.pl 26 rows (4 diffs, all pre-existing: #1040 numeric-looking string, ref-address length, `use feature 'bitwise'` not making `|` numeric = #1040's scope, **#1050** false-is-a-dualvar) + fh.pl 20 rows (4 diffs, all pre-existing: **#1048** glob value / glob ref / dirhandle / std streams in a filetest slot, **#1049** a STRING naming an open handle resolves to the handle); main had 16 + 20 diffs on the same files.  Sweep on the merged main: see DECIDED §s465.  BA did NOT start #1037 (measured: two files, perl-tests/state.t 157 + t/op/state.t 166 = ~323 rows).

**ROUND 23, SECOND WAVE (s465, 2026-09-03, launched from main `c9398d9`, both told to rebase first, both told NEVER to commit scratch/):**
| agent | letter | job | gen string |
|---|---|---|---|
| BC (correctness) | s465bc | #1037 — given/when refusal → STATEMENT-level die (perl-tests/state.t 157 rows + t/op/state.t 166 = ~323), then #965's 46 state.t inline SKIPs restored, then the classification TABLE of every other file-level refusal (a/b/c) with the statement-shaped ones converted; bars = Pl/ change + checker change ⇒ gate-SET scan both populations | v2-620 |
| BD (perf) | s465bd | #995 — the write-incdec mark lands on the lvalue ROOT, not the subscript's key variables (BB's measurement: `$h{$k}++` boxes `$k`, 15 %+8 % of arrhash); a boxing-verdict widening ⇒ sweep-as-gate + gate-SET scan; then #1046 explicit-return bench rows | v2-630 |
AZ (phase-0 instruments, first wave) still in flight.  Bench-alone leg on the final tree = round end.

**SESSION END s465 (2026-09-03): AZ / BC / BD STOPPED by TaskStop (USER out of tokens), worktrees KEPT.**  AZ: 4 commits on c9398d9, clean, HEAD 542e878 — instruments DONE, merge review owed (its report was never produced; read its session-log section in the worktree for the numbers + the proposed I4 CLAUDE.md line).  BC: no commits, `Pl/Parser.pm` modified (the given/when die form, minutes in).  BD: no commits, `Pl/VarAnnotator.pm` modified (the lvalue-root walker beside `_tw_mark`, minutes in).  Resume rule (round-19 precedent): a FRESH Opus agent in the kept worktree, told to `git diff` first and to rebase onto main.

**AZ MERGED (s466, Fable, 2026-09-03): ff as main `57848f3` after a clean rebase onto c562d21 (five commits: four s465az + one s466 review-fix).**  Review: full diff read (sweep-diff.pl / sweep-perl-tests.pl / run-perl-suite.pl / PCLShortfall.pm / audit-instruments.t / docs); fail-baseline.tsv reconciled row-for-row against main (484 = 484, col 6 stripped); row-shortfall.tsv reconciled against its own totals — the agent's report said 86,126 UNEXPLAINED / 185 files, the blessed file holds 82,666 / 183 (376,788 caused + 82,666 = its 459,454 total; the report number did not add up) — RECORD CORRECTED, not the file; gate on the AZ tree 196/6696 xs-only.  Review fixes: the shortfall definition comment said `planned - (pass+fail+skip)` in FOUR places while the code, the unit test and the blessed 12,257 all COUNT skips (`planned - (pass+fail)`); `eq UNEXPLAINED` vs `/^UNEXPLAINED/` on the two sides.  I4 cadence line written into CLAUDE.md (full `--all --jobs 4` at least once per ROUND with `--bless-stamps`).  Worktree pruned; scratch copied to scratchpad `scratch-az` (11 files: the i1–i4 blessing scripts + merge-tsv.pl + pass3 clusters).  LESSON: a record's COUNT must add up to the record's TOTAL — recompute every quoted number from the blessed FILE, not from the run report that preceded the last hand-edit.

**ROUND 23 CONTINUES (s466, Fable, 2026-09-03/04): three Opus agents IN FLIGHT at write time** — BC (#1037 statement-level refusal, kept worktree `agent-a9ca34a71273d52ef`, gen v2-630), BD (#995 lvalue-root walker, kept worktree `agent-a474d73ddc7aa408f`, gen v2-640), BE (companion row-diff attribution + per-file blesses + the volatile-description report bucket, fresh worktree, no emission change).  Main: `57848f3` AZ merged → `f330e5f` #1035 steps 0+1 (Fable, gen v2-611 — execution Fable should have delegated, USER s466) → `e660d12` four companion-instrument fixes → records.  Review rule for BE: read its attribution table row by row; a "pre-existing" row must carry WHY; no wholesale bless.  Queue after these three: #1035 steps 2–4 (an Opus agent: FACTS keys :perl/:why, p-raw-params classes, p-sub facts plist), the pool #1022/#1020/#1045, perf #1046/#996; the owed bench-alone leg.

**s466 DIED / s467 RECONSTRUCTED + ENDED EARLY (Fable, 2026-09-04).**  s466's context was lost at ~00:25 with three agents in flight; nothing on main was lost (`bc9aa4a`, clean).  What survived and where: BD's work is COMMITTED and already rebased by Fable (`65e519c`, v2-640) — its only blocker is the stale `(let ((` guard spelling (`Pl/t/lvalue-root-01.t` 21/23) against main's #1035 `p-let`; BC's work is COMMITTED on 57848f3 (`e60ce12`, v2-630), unrebased, and its companion run finished after its agent died (UNREAD; copied into its `scratch/s466bc-legs/`); BE (attribution) had no commits — its three bisection run logs and the validate2 companion log are copied into its `scratch/s466be/`.  **LESSONS**: (1) an agent's FINAL REPORT dies with the parent session — the record that survives is what the agent COMMITTED (session-log section + DECIDED) plus its `scratch/`; tell agents to write the numbers into their session-log section BEFORE reporting.  (2) A session scratchpad under `/tmp/claude-1000/…` is not durable across sessions (the s464 `p964/` copies are gone) — leg logs an agent must read next session go into the WORKTREE's `scratch/` (untracked, never committed), and a pruned worktree's scratch that tasks cite must be copied somewhere durable BEFORE pruning (AY's worktree is now the only copy for #1020–#1033: KEEP it).  (3) A Fable-launched leg (`tools/prove-core` on BD) with no `Files=` line was killed with the session — re-run, never trust a partial log's silence.  s467 launched BD-finish + BC-finish (Opus, pinned) and stopped them within a minute on the USER's "end for today" — both worktrees clean at their HEADs; the prompts are in the s467 transcript and must be rewritten next session (BD → merge → BC second rebase at v2-650 → BE).

**ROUND 23 COMPLETE (s468, Fable, 2026-09-04): BD (#995) MERGED ff `0e0b0b9` (gen v2-640) → BC (#1037) MERGED ff `b128345` (gen v2-650) → BE (companion ROW DIFF attribution) MERGED ff `a715608`; legs on the final tree + records + push in s468.**  THE DURABILITY PATTERN (new, standing): every agent prompt is WRITTEN TO ITS WORKTREE before launch (`scratch/s468xx/prompt.md`, untracked) and each agent commits its numbers into its record section BEFORE reporting — s466 died with three agents in flight and s467 spent a morning reconstructing what each had been told.  A relaunch after a dead session = a fresh Opus agent on the saved prompt file, told to `git status` / `git log main..HEAD` first.  Leg logs go in the worktree's `scratch/<session>/`; before pruning a merged worktree its scratch is ARCHIVED to `~/pcl-agent-scratch/<session>/<letter>-<worktree>/` (s468: bd 6275 files, bc 71, be 313 — the probes cited by #1072–#1074 and #1082–#1084 live there).  Merge lessons this round: (1) a STALE GUARD can be the whole blocker of a finished change — when main changes an emission SPELLING (#1035's `p-let`), every guard that greps the old spelling fails for the wrong reason; respell as a STRENGTHENING (assert the class) and re-base the inverse guard on a main that has the spelling; (2) two agents may edit the SAME baseline tsv files if their ROW SETS are disjoint by file — the rebase conflict (no markers: NUL bytes make git call the file binary) is resolved from the index stages as a three-way ROW SET, and BOTH sides prove afterwards that unowned rows are byte-identical to main in order (Fable re-ran that proof on every tsv at each merge); (3) a second rebase after a sibling merge is cheap when told in the prompt (gen string chosen ABOVE the sibling's, artifacts regenerated on the final tree, bodies byte-identical to main); (4) sequencing = bench first on the quiet box, siblings READ for twenty minutes.  AY worktree `agent-afc40626938966acc` still KEPT (only copy of #1020–#1033 probes).  NEXT ROUND: #1035 steps 2–4 (Opus), pool #1022 / #1020 / #1045 / #1084 / #1083, perf #1046 / #996 / #1056, #1072 tools filler.
