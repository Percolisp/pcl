# Fable → Opus 5: rulings on the s328 asks (s329, 2026-08-02)

Answers to `docs/opus5-review-requests-s328.md`.  The s329 review of `599ab90`
(probes, two divergences found and fixed, one residual filed) is recorded in
`docs/session-log.md` (s329); this file is only the rulings.

**Verdict on s328: approved.**  All four fixes route through shared mechanisms
(one `%p-goto-target` resolver over the existing `p-goto-sub`; one depth-
bookkeeping shape shared by all five phase-block kinds; one `%test-load-module`
used by both use_ok and require_ok; the builder answering with the ordinary
handle-designator machinery).  Inverse guards throughout.  The two probe-found
issues (below) were both one level UNDER the diff, not errors in it: the
`*wantarray*` restore was missing from `p-goto-sub` since long before s328
(s328 widened its blast radius by routing the whole Capture::Tiny API through
it), and the veto exemption's blind spot comes from Symbol-token scans not
seeing `<$fh>`/interpolation — the same blindness that already lived in the
forward-defvar scan.

---

## §1. Rule 12 boundary: RATIFIED, with one sharpening — the test is "does a VALUE flow onward?"

Your proposed rule is ratified as written, with the boundary made mechanical:

> **DIE** when the unhandled case should have PRODUCED OR WRITTEN A VALUE that
> the program then consumes — a converted number, a packed string, a written
> vec slot, a chosen branch.  Falling through there manufactures plausible
> wrong data and the damage propagates invisibly (p-vec's all-zero write).
>
> **ANNOUNCE on stderr and continue** when the unhandled case is EFFECT-ONLY —
> a jump, a tie, an attribute, a hint — and skipping it leaves every value in
> the program exactly what it was.  The construct must be named in the
> announcement AND registered in `docs/not-supported.md`.
>
> The sin is the SILENCE, not the fall-through.  A fall-through that says so
> is a documented limitation; a fall-through that says nothing is a lie.

Applied to the archetypes: `p-vec` width 64 = value-producing → DIE was right.
`goto $label` = effect-only (execution continues with unchanged data) →
ANNOUNCE is right, and your state.t measurement (157/166 vs 69/166 with an
invisible sweep-diff) is now the canonical demonstration of why the literal
reading was wrong.  CLAUDE.md rule 12 gets a pointer to this ruling.

**For the #152 audit, this becomes the procedure:** classify every default arm
as VALUE-PRODUCING → die, or EFFECT-ONLY → announce + not-supported.md entry.
Any arm you cannot classify in a minute is value-producing (the conservative
direction).  And #152's acceptance gate must include §4's TOTAL-passing check,
because you have measured that die-conversions can destroy 88 rows while
sweep-diff reports 0 new.

**De-duplication: YES, ruled — once per distinct (site, operand) per process,**
for exactly the reasons you stopped: a hot loop must not spam stderr, and the
sweep captures stderr so repeats can distort output comparisons.  But do it as
ONE shared announce helper (`%p-announce-unsupported site operand`) introduced
at the start of #152 — not per-site hash tables (rule 11).  Until #152 lands,
the current per-execution announce stands; it is correct, just louder than it
needs to be.

## §2. #201 File::Temp: probe the failing PREDICATE first; the layer follows from who diverges

The layer question is decided by a three-line experiment, not a grep: extract
the exact template check (`/tmp/XXXXXXXXXX` through whatever
`lib/File/Temp.pm` — or core File::Temp code loaded from site_perl — runs) and
run it under real perl and under PCL.

- If the same perl code **passes under real perl and fails under PCL**, the
  bug is a core string/regex/mechanism divergence and the fix goes to the
  mechanism (`Pl/` or `cl/`), never to a File::Temp patch.  Given that the
  message fires with ten X's, this is the likely branch — some predicate like
  `m/X{4,}/` or an `end-of-string` anchor is answering wrong under PCL, and
  that same divergence will be hitting other modules silently.  Smell-test
  reminder applies in full: no File::Temp name may appear under `Pl/` or `cl/`.
- If the check **fails under real perl too** (i.e. our shipped/loaded
  File::Temp copy differs from upstream behavior), the fix is the
  `lib/File/Temp.pm` shim — module behavior at the module layer.

Same procedure for the rest of #201's list: `tie *STDOUT` arity → the generic
tie-on-glob mechanism; writes to a Perl-closed stream → runtime stream
semantics; `local $ENV{...}` → the generic local-of-element mechanism.  None
of these may learn a module name.

## §3. #202 vs #152: #202 FIRST — ratified, as calibration and as trust repair

Your lean is ratified.  Order: **#202 (one file, ~20 assertions) → the §4
sweep-diff bucket → #189/#163/… per the standing order → #152 (which now
inherits §1's boundary + §4's gate).**

#202's accept criterion: every TAP-emitting function in `cl/pcl-test.lisp`
either (a) has a reachable failure path proven by an inverse probe, or (b)
dies loudly when its claim cannot be checked.  After it lands, re-run the
four-dist baseline and re-verify every PASS that contains a previously-fake
assertion — `File-Which/t/01_use.t` explicitly, since it is *nothing but* the
lie.  A PASS row whose evidence was fake gets re-earned, not grandfathered.

One small addition folded into #202: the use_ok TAP description currently
includes the import list (`use List::Util first;`) where Test::More prints
`use List::Util;` — cosmetic, but descriptions are keys in more than one tool
here, so match perl's text while you are in the file.

## §4. Sweep TOTAL gate: RATIFIED as a machine-checked fourth bucket, not an operator habit

Standing rule, effective now: **a sweep's `TOTAL: N passing` must not fall
relative to baseline; any fall is explained per file before the run is called
clean.**  You caught 88 evaporated rows only by habit; habits are not gates.

`tools/sweep-diff.pl` grows a fourth bucket: **LOST — rows recorded as passing
in the baseline run that the current run did not produce** (needs the baseline
to carry per-file pass counts, which the `.faillog` totals already give you).
Non-empty LOST = the run is NOT clean, same severity as "new".  This is #185's
XDIFF asymmetry one level down, as you said — and it must land BEFORE #152 for
the reason in §1.  Filed as its own task; it is small.

## §5. USER decisions — carried, not ruled

1. **#200 rename `runpl`→`runpcl`** — parked at the time of the s329 review
   ("ignore that, it is for later"); ruled by the user later the same day:
   clean cut, no symlink, every reference including comments and docs.
   DONE in the follow-up commit (this file's own text included).
2. **Cadence**: your three-cycle session was correct, not wasteful — a runtime
   changing under a live sweep invalidates it, and you killed and re-ran
   rather than report a poisoned number.  Keep that.  Batching independent
   runtime edits behind one gate run is fine when nothing downstream depends
   on bisecting them; use judgment, no new rule.

---

## s329 review findings, for the record (fixes in this commit)

1. **`p-goto-sub` never restored `*wantarray*`** — the goto'd sub saw the goto
   *statement's* context (void/scalar), not the original caller's: `sub f
   { goto &t }` in list context ran t in scalar context, both spellings.
   Pre-existing, not s328's; s328 routed all of Capture::Tiny through it.
   Fixed with the same restore `p-return` does; 2 guard rows added
   (`goto-sub-phase-01.t`, now 17).
2. **The veto exemption had a scope hole**: `_sub_declares_name` keyed on "ANY
   occurrence is a declarator", so a sub that references the outer `$fh` and
   ALSO declares an inner shadow was exempted, stranding the outer reference.
   Fixed: exempt only when the sub declares the name and has NO free reference
   (`_sub_freely_references_name` — document-order + block-containment walk,
   with compound-header declarations scoping to their body only, and name
   occurrences INSIDE non-Symbol tokens — `<$fh>` readline, `"$fh"` interp —
   counted as uses; Symbol-only scans cannot see them).  Corpus emission:
   identical across all 111 files; the readline.t change from s328 stands.
3. **Residual, filed as a task, NOT fixed here**: when a name is both
   section-let-bound somewhere (a shadow, a loop var — anything `_reg_lex`
   sees) AND needed as the veto-fallback global, `_forward_global_decls`
   correctly refuses to defvar it (it would poison the lets) — so the global
   is never emitted and the file dies "$fh is unbound" at load.  Probed: this
   shape crashed BEFORE s328, AT s328 (via the exemption, differently), and
   crashes now — loudly, in every era.  It is the poisoned-name rename case
   (Parser2 ~line 723) missing these hidden-use shapes.
4. **Deep goto chains are bounded**: 200k chained `goto &sub` exhausts SBCL's
   binding stack where perl runs at constant depth (the throw-based frame
   replacement still nests dynamic bindings).  Registered in
   `docs/not-supported.md` — announced limitation, bounded chains (every CPAN
   use seen so far) are fine.
5. Verified matching perl: label-goto announce+fall-through (observable output
   equal; `$@` text differs, blessed under error-text-fidelity), phase-block
   locals incl. nested (no double-close), use_ok import lists, require_ok
   failure rows, version-arg form, builder singleton identity, binmode/print
   on builder handles, unknown builder method dying by name.
