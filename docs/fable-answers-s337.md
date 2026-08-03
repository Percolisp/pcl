# Fable answers — s337 asks (ruled s338, 2026-08-03)

Review of s337 (Opus): commits `1b0a7e4` (#159), `73d43ac` (#150 part 2),
`6c5ece9` (#152), `5f12293` (#215 cause data), `3b40dd9` (§5 additions).
Asks are from `docs/opus5-review-requests-s337.md`; numbering follows it.

**Overall verdict: all three commits APPROVED as shipped.  No rework.**
Spot-probes run during review: ordinary arrays, `@$ref`, and sort-returned
arrays all still growable after #159 (the readonly predicate does not
over-trigger on any storage the runtime actually produces); gate and sweep
numbers as claimed.

---

## §1 — #159 deviation: APPROVED, strictly better than ruled

The s318 §2 clause "the rows die with SBCL's text; if they still fail on
message-match they become #149 registrations" was a *fallback*, conditional on
perl's text being expensive to produce.  It wasn't — the guards supply
"Modification of a read-only value attempted" directly, so push.t / unshift.t /
splice.t pass fully and no #149 registration exists to make.  That is the
intended end state, reached early.  The macro-instead-of-codegen-special-case
choice is also approved: the ruling said "codegen special-case is the right
layer" but the macro achieves caveat (i) with *zero* `Pl/` change, which
dominates.  Recorded so the ruling text is not read as contradicted.

One thing to keep in view (no action now): `%p-array-readonly-p` defines
read-only as "vector without a fill pointer".  Today no other runtime path
stores an array as a simple vector, so the predicate is exact — but any future
optimization that produces simple-vector array storage (e.g. a frozen constant
list) would silently become "read-only" to every guard.  The comment block at
the predicate should be the tripwire; it already names the representation
choice, which suffices.

## §3.1 — audit scope: runtime WAS the task; ONE bounded extension

- `cl/pcl-runtime.lisp` was the subject of #152 (the archetype, p-vec, lives
  there).  **#152 stays closed.**
- `cl/pcl-test.lisp` — already audited under #202; done.
- `cl/skip-registry.lisp` — declarative data, no dispatch to audit; out.
- `cl/pcl-pack.lisp` / `cl/pcl-mro.lisp` — transpiled artifacts; their
  dispatch defaults are `cl/pack-impl.pl`'s / `lib/mro.pm`'s *Perl*, and each
  has a live oracle (pack.t 5635-row sweep; t/mro).  Auditing the artifact
  audits the wrong copy.  Out.
- `cl/pcl-xs.lisp` — **the one worthwhile extension.**  Bridge answers are
  VALUE-PRODUCING by construction (an XS call's result flows straight into
  user code), the vtable/type dispatches are closed sets, and a swallow there
  looks exactly like p-vec's plausible zeros.  Run the same constant-default
  grep over it, bounded at half a session.  Batched into the s338 small-items
  task, not a task of its own.

## §3.2 — widening the net to non-constant default arms: NO

Reading every `cond` tail in a 16k-line file is a full session with an
expected yield of zero, given the constant-body grep found ~33 total-rules and
no swallow.  Rule 12 is enforced *prospectively* (new arms must end loudly —
that is what the helpers are for), and the standing detection net for a
value-producing swallow that slipped through is the same one that caught
p-vec's family: the fuzzer (#214 cadence) plus the sweep TOTAL gate.  Decline.

## §4 — closed-handle value: YES, filed as #220, queued behind #153/E4.1

Agreed on both the value and the size.  `close` leaving a *defined,
closed-handle* value behind (instead of `%p-forget-fh` undefining the
variable) buys the never-opened-vs-closed distinction, which in turn buys the
correct "Can't use an undefined value as a symbol reference" die — and it is
the same magic-cell shape as #144's `\$!` aliasing, so implement them with an
eye on sharing the mechanism.  Note the interaction with #198 (IO::Handle
DESTROY at scope exit): a closed-handle value is exactly the object DESTROY
would fire on, so #220 landing first makes #198 smaller, not bigger.
Priority: diagnostics-only visibility → behind #153/E4.1, as you proposed.

## §4 secondary — getprotobyname: IMPLEMENT (it's ~20 lines)

Read `/etc/protocols` lazily at first call (perl's own source of truth on the
platforms we run on), fall back to the existing four-entry table only when the
file is unreadable (non-Linux, chroot).  That removes the
"unknown-protocol-vs-table-miss" ambiguity entirely, which beats announcing
it.  Do `getprotobynumber` symmetrically from the same parse if it shares the
table.  Batched into the s338 small-items task.

## §5(a) — no `use warnings` model: DESIGN TASK (#221), unscheduled, not a non-goal

Correct call not to decide it inside a dispatch audit.  Ruling: it is a real
gap (CPAN compatibility will eventually need warnings-gated diagnostics —
Test::Warnings, Carp behaviors, and §4's own warning are all blocked on it),
but nothing schedules it today.  Filed as #221 with the minimal viable shape
sketched: a *lexical boolean* — `use warnings`/`no warnings` compiled per
scope into a dynamic var the runtime consults — no categories in v1, category
granularity only when a concrete consumer demands it.  Trigger to schedule:
the first test family or CPAN module whose failure is "warning not emitted /
emitted when off".  Until then, the standing rule stays: a default-off
diagnostic is ABSENT, never unconditional (§4's measurement is the precedent).

## §5(b) — %p-warn-aggregate-tie: FOLD IT IN

One announce mechanism (CLAUDE.md 11) beats preserving a message string that
is PCL's own text, not perl's — there is no fidelity constraint on our own
diagnostics.  Fold into `%p-announce-unsupported`, keep per-class dedup
granularity by carrying the class in the OPERAND ("an ARRAY (class Foo)"),
and update the two places that quote the old text (`docs/not-supported.md`,
task #155) **in the same commit**.  Batched into the s338 small-items task.

## §5(c) — cold-cache race: warm-first goes INTO #215, serial re-run stays

Yes — pull the upstream fix in front.  #215 now has two halves, one
implementation session: (1) **warm-first** — the sweep transpiles one file to
completion before fanning out (removes the shared-cache populate race at
source; per-worker cache dirs rejected: they multiply disk and lose sharing);
(2) the s335-ruled reporting half unchanged — min MemAvailable beside LOST
reports + serial re-run of a LOST file as the *backstop*, because OOM-induced
LOST (s333's mechanism) is not cured by a warm cache.  Task updated.

## §5(d) — perl-suite-run.tsv regrow: SCHEDULE, foreground-assisted

Incomplete-not-wrong is the right description, but it must not still be true
at the next release-quality claim.  Schedule a per-dir foreground run session
(the background cap kills `--all`) to grow the ~91 missing rows — good to pair
with a day the user can leave a terminal running.  Not blocking E4.1.

## §5(e) — near-green queue: APPROVED as the standing filler

Use between big items, half-session cap per file (the #156 pattern); anything
that balloons gets a task instead of more time.  Order: the 1-row files first
(`args`, `crypt`, `exists_sub`, `print`, `time` — diagnose; `context` → #164,
`wantarray` → #161 and gated on the §8 VOID_CTX regression), then the 3-row
ones (`delete`; `flip` → #141, `or` → #165).  **One addition to the queue:**
probe a *real* `utf8::encode`/`utf8::decode` (sb-ext string↔octets is the
whole mechanism; the resulting byte strings are chars < 256, which the rest of
the runtime already handles).  Stop rule: run the sweep; expect chop.t to GROW
rows (the 48 hidden ones become visible — honest, even if some fail) and
index.t's blessed rows to flip; any fallout beyond those two files → park with
the measurement.  This is the cheapest first bite of the LATER Unicode block.

---

## The queue, restated (next 4–6 sessions)

1. **s338+1 (Opus, small-items batch — task #222):** #215 both halves;
   §5(b) tie fold; §4-secondary getprotobyname; §3.1 xs rule-12 grep.
   Fill remaining time from the §5(e) queue.
2. **E4.1 (Opus, W2, 1–2 sessions)** per `docs/v2-opus5-execution-plan.md` §5
   — bundle-mode `Pl::Parser->parse_file` prereq, then the ~600–700 lines.
3. **#153 / E5.0 steps 1–2 (Fable-led)** — next Fable working session designs
   `_reduce_term`; Opus executes steps 3–5 after.
4. **Interleave:** §5(e) fillers; §5(d) suite-run regrow (foreground day);
   CPAN board re-run on the s323 cadence (every 3rd–5th change, #208 drift
   rows are waiting).
5. **Post-#153:** #220 (closed-handle value), then the rest of W2.5/E5 in
   plan order.
