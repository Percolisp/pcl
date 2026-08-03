# Review requests — s337 (Opus) → Fable

Three commits this session: `1b0a7e4` (#159 read-only arrays), `73d43ac`
(#150 part 2, four drifted sweep copies restored), and the #152 rule-12 audit
below.  Gate 131/4594 PASS and sweep GATE clean at each.  The asks are §3–§4.

---

## 1. #159 read-only arrays — what shipped, and the one deviation

Option (b) as ruled (`fable-answers-s318.md` §2).  Nothing to decide; recorded
for the review trail.

**The flag IS the storage.**  `Internals::SvREADONLY(@a,1)` swaps the array's
storage for a simple vector (no fill pointer, not adjustable) over the same
element boxes — which is exactly perl's read-only AV: fixed SIZE, elements
still writable.  Size changes then fail by construction; the runtime checks
only supply perl's message.

**Caveat (i) — reaching the storage cell — needed no `Pl/` change.**
`Internals::pl-SvREADONLY` became a MACRO: the codegen already emits the array
VARIABLE (`(Internals::pl-SvREADONLY @a 1)`), so the call site is the only
place holding the cell.  CLAUDE.md 11 rather than a codegen special case.

**Caveat (ii) — the adjustability audit — is done**, and is the guard list in
the task.  Everything else that touches `fill-pointer` operates on
freshly-made local vectors.

**Deviation to note:** the ruling said the three rows "die with SBCL's text; if
they still fail on message-match they become #149 registrations".  They do not:
the guards raise perl's text directly, so push.t / unshift.t / splice.t are
fully passing and sort.t's in-place row passes.  No #149 registration needed.

---

## 2. #150 part 2 — four sweep files that were lying

`perl-tests/{chop,dor,not,quotemeta}.t` had been **edited in place** (plans
hand-lowered, assertions commented out, `dor.t` replaced by a hand-written
substitute).  Restored byte-identical to `t/op/`; each new failure triaged to a
cause (registry / baseline / documented).  Fully-passing 66 → 64 is the point.

The finding worth carrying forward: **`utf8::encode` is a no-op in PCL**, and
chop.t *guards a block with it* (`next if $end_utf8 eq $end`), so 48 of its 148
rows were never emitted — while the hand-lowered plan of exactly 100 made that
read as a clean pass.  A blessed limitation can make a test file skip its own
rows; nothing can be skip-registered, because the rows never run.

---

## 3. ASK — #152: the audit says the runtime is already clean.  Is that the
##       finding, or should the net be wider?

Step 1 of the ruling is done: ONE shared pair of helpers, one dedup table —
`%p-announce-unsupported (site operand)` for EFFECT-ONLY and
`%p-unsupported-value (site operand)` for VALUE-PRODUCING — and the per-site
hash s337 had just added for SvREADONLY was folded into it.

Then I inventoried every `(t …)` / `(otherwise …)` arm in
`cl/pcl-runtime.lisp` whose body is a bare `nil` / `0` / `""` / `*p-undef*` /
`t`: **38 sites**.  Classified:

- **~33 are Perl's own TOTAL rules, not swallowed cases** — truthiness
  ("anything not explicitly false is true"), numification ("anything else is
  0"), `ref()` of a non-reference is `""`, `wantarray`'s third arm, "read past
  the end of a string is 0", sprintf's "none of these flags apply → no prefix",
  `-T`/`-B` over a closed scan result.  Several already carry an explicit
  "this IS the Perl semantics, not a swallowed case" comment (`p-glob-slot`),
  which suggests someone walked part of this before.
- **The rule-12 ARCHETYPE is already fixed.**  `p-vec`'s width dispatch now
  handles 16/32/64 in one loop and *dies* on an illegal width; its remaining
  `(t 0)` is only "sub-byte access past the end", which is correct perl.
- **One family had a real user-visible smell** — §4 below.

So the honest headline is **"the retroactive audit found no value-producing
swallow left in `cl/pcl-runtime.lisp`"**.  Two things I did not do, and want a
call on:

1. **Scope.** I audited `cl/pcl-runtime.lisp` only.  `cl/pcl-test.lisp` was
   #202's subject; `cl/pcl-xs.lisp` and `cl/skip-registry.lisp` are unaudited;
   `cl/pcl-pack.lisp` / `cl/pcl-mro.lisp` are generated artifacts (auditing
   them means auditing `cl/pack-impl.pl` / `lib/mro.pm`).  Worth a pass, or is
   the runtime the whole of the task?
2. **The net.** My grep catches only *constant* default bodies.  A default arm
   that calls something (and thereby returns a plausible value) would be
   missed.  Widening it means reading every `cond` tail in a 16k-line file —
   is that worth a session, given result (1) above?

## 4. ASK — the one real finding is BLOCKED on a representation change

`print` / `printf` / `say` each carried **their own copy** of the same arm: an
unresolvable filehandle sets EBADF, writes nothing, returns false, and says
nothing.  Three copies (CLAUDE.md 11) — now ONE, `%p-out-fh-or-fail`.  That
part shipped.

But both obvious rule-12 conversions are WRONG, measured, and the measurements
are recorded in the function so they are not retried:

- **Warn "print() on unopened filehandle"?**  That warning is `use warnings`-
  GATED in perl, not default-on (measured: plain `perl` prints nothing, exactly
  like PCL).  PCL's silence IS perl's silence.  Emitting it unconditionally
  broke `fileio-02.t` and `transpile-test-09.t`, which assert the default
  quiet.  **PCL tracks no warnings state at all** — no `use warnings` model
  anywhere in `Pl/` or `cl/`.
- **Die "Can't use an undefined value as a symbol reference"?**  Perl does
  exactly that for a handle that was *never opened* — but returns undef for one
  that was *closed*.  **PCL cannot tell them apart**: `%p-forget-fh` leaves the
  variable UNDEFINED after `close`, so `print $closed_fh …` and
  `my $u; print {$u} …` arrive identical.  Dying broke transpile-test-09.t's
  closed-handle rows (#186), which are right.

So the ask: is a **closed-handle value** (something `close` leaves behind that
is defined-but-not-writable) worth filing as its own task?  It would buy the
undef-designator die, and it is the same shape as the `$!`-aliasing and
arylen-magic cells the runtime already has.  My inclination is yes but small
— it only shows up in diagnostics — so it should queue behind #153/E4.1
rather than in front.

Secondary, much smaller: `p-getprotobyname` answers from a **four-entry static
table** (tcp/udp/icmp/ip) and returns undef for anything else, where perl reads
`/etc/protocols`.  Unknown-protocol → undef is indistinguishable from a real
miss.  Announce, implement properly, or leave?

---

## 5. Other things that surfaced — no decision needed unless you want one

Not asks; things I noticed while in these files and would rather you saw than
found later.

**(a) PCL has NO `use warnings` model at all.**  Grepped `Pl/` and `cl/`:
nothing tracks warnings state, lexically or globally.  §4 hit this from one
direction (a default-off warning cannot be emitted correctly), but it is
broader — every perl diagnostic PCL might want to add has to be either
unconditional or absent, and the unconditional choice is wrong for anything in
a default-off category.  Whether that deserves a design item of its own, or
stays a documented non-goal, is a call I did not want to make inside a
dispatch-audit task.

**(b) `%p-warn-aggregate-tie` (#155) was left OUTSIDE the new shared helper.**
The #152 ruling said one announce helper, and I folded the SvREADONLY one in —
but the tie warning has its own dedup table and its own message shape
("…tie ignored (class X)").  It is a genuine EFFECT-ONLY announce and would fit
`%p-announce-unsupported "tie" "an ARRAY/HASH"`.  I did not touch it because
its text is quoted in `not-supported.md` and the #155 task, and rewording a
message during an audit felt like the wrong moment.  Fold it in, or leave the
two?

**(c) The sweep's LOST detector caught a real thing, but the cheap fix is
upstream of it** (#215, updated with the measurement): warming
`~/.pcl-cache` with one file before fanning out — or a per-worker cache dir —
removes the race at source, rather than detecting it after the fact and
re-running serially.  Worth pulling in front of the reporting half?

**(d) `docs/perl-suite-run.tsv` is still pre-#150-part-1** — it has no rows for
the 91 files that became visible when the copied-file skip was dropped, so the
#25 release signal is incomplete-not-wrong.  A full re-run grows it by ~91
rows.  Not started; it wants a foreground per-dir run (the background cap kills
`--all` before the summary).

**(e) #150's near-green list is still unmined.**  From the s318 data, and
minus the three that #159 just cleared: `op/args.t`, `op/context.t`,
`op/crypt.t`, `op/exists_sub.t`, `op/print.t`, `op/wantarray.t`, `op/time.t`
are each **1 row** off perl, and `op/delete.t` / `op/flip.t` / `op/or.t` are 3
(flip.t is probably #141, or.t is #165).  Each is likely one small family.
Good "between big items" material if you want a low-risk queue filler.
