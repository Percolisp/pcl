# Computed magics in the box model — the #561 design (+ the #602 fold) (s449, Fable, 2026-08-29)

*The ruling for task #561 (where `$!`/`%!` live so glob aliases reach them)
and task #602 (`*A = *B` does not clear the slots B lacks), sized for one
Opus implementation session.  Every line here is grounded in a code read of
`cl/pcl-runtime.lisp` at `c8a8311`; the line numbers cite that tree.*

## 1. The question

`$!` works, but `*Y = *!` installs nothing and `$Y` reads `""`; `%!` is not
implemented at all (unbound CL variable — the whole file dies at load).
t/re/reg_namedcapture.t is exactly this shape (`*Y = *!; 0 < keys(%Y)`).
The task's constraint: no per-name special case in the glob path.

## 2. What the code already says (the measurement)

* **A glob slot IS a CL variable**: the `$`/`@`/`%` slots of glob `NAME` are
  the symbols `|$NAME|` / `|@NAME|` / `|%NAME|` in the package
  (`%p-slot-name`, 14686), and `p-glob-copy` (14807) *shares the
  slot value* — for a scalar slot that value is the p-box, so a glob alias
  IS box aliasing.  `$!` fails only because **no `|$!|` variable exists**:
  emission goes through the accessor (`%SPECIAL_VARS`: `'$!' =>
  ['p-errno-string']`, Pl/ExprToCL.pm:252), so `boundp` fails and the copy
  skips the slot.
* **The magic-box mechanism already exists and has a live precedent**:
  `p-magic-cell` (1459) is a getter/setter pair living in a p-box's value
  slot, dispatched at the two scalar chokepoints (`unbox` 1475, `box-set`
  1943) and at `box-nv`/`box-sv` (2384/2538).  **`|$.|` (1723) is already
  exactly this shape** — a defvar'd box holding a magic cell.
* `p-errno-string` (1164) returns the dualvar box (numifies to errno,
  stringifies to strerror, `""` when errno 0) and `(setf p-errno-string)`
  (1178) writes the C errno.  These are the getter and setter, verbatim.
* The array element-read helper `p-aref-unbox-elem` already dispatches a
  magic cell found in an element (6522, the `@_`-hole alias); the hash
  helper `%p-hash-unbox-elem` (6537) does **not** — that is the one missing
  arm.

## 3. RULING — the scalar half: computed magics get a CANONICAL MAGIC BOX

Every magic scalar whose emission is an ACCESSOR call gets a defvar'd
canonical box beside `|$.|`:

```lisp
(defvar |$!|  (make-p-box (make-p-magic-cell :getter #'p-errno-string
                                             :setter #'(setf p-errno-string))))
(defvar |$^E| ...same two functions...)   ; POSIX: $^E IS $! (s448o, #571)
```

* **The accessor IS the getter** (rule 11: one mechanism).  Emission stays
  `(p-errno-string)` for plain `$!` — the box and the accessor read the same
  `*p-stored-errno*` state, so they cannot disagree; the box exists for the
  paths emission does not cover: glob aliasing (`*Y = *!` now copies the
  box; `$Y` reads through the magic getter), symbolic access `${"!"}`, and
  `\$!`-through-alias.
* **ZERO glob-path changes.**  The fix is that the slot now exists — which
  is the "right layer" answer #561 asked for.
* Family audit at build time: walk `%SPECIAL_VARS` for accessor-emitted
  entries.  Today that is `$!` and `$^E` (both errno).  The `['p-undef']`
  stubs (`${^WARNING_BITS}`, `${^LAST_FH}`) are inert stubs, not computed
  state — leave them.
* Boundary (accepted, note in the doc/tests, do not build): because plain
  `$!` emission reads the accessor directly, REPLACING the `!` glob
  (`*! = *src`) does not change what `$!` returns.  Probe what perl does
  and record the divergence if any test ever sits on it.

## 4. RULING — the `%!` half: a REAL hash whose VALUES are magic

`%!` is an ordinary hash-table bound to `|%!|` in main; the magic is per
VALUE, not per hash:

* **Keys** = the platform's errno names, built once at runtime boot from a
  fixed POSIX name list resolved through sb-posix constants
  (`find-symbol`-and-`boundp` per name; a name the platform lacks is simply
  absent — perl's `%!` is per-platform too).  The name→number table is a
  runtime global (`*p-errno-table*`), because errno names are C-library
  facts, not module behavior.
* **Each value** = a box holding a `p-magic-cell` whose getter answers
  truth exactly when that errno == `*p-stored-errno*`.  Probe perl for the
  exact false/true values (`$!{ENOENT}` under and not under ENOENT) before
  writing the getter — do not guess the false spelling.
* **`%p-hash-unbox-elem` gains the magic-cell arm `p-aref-unbox-elem`
  already has** (one arm, the sibling copy the chokepoint design expects).
  Verify `p-values` / `p-each` funnel through it; if one reads elements
  around the helper, route it through (rule 11) rather than adding a second
  dispatch.
* `keys %!` / `exists` / `scalar(%!)` need nothing: the table is real.
* Element WRITES: probe perl first (`$!{ENOENT} = 1`); make the setter do
  what perl does (die or ignore), never silently install an ordinary value.
* **`*Y = *!` then `keys %Y` works with zero glob changes** — the existing
  HASH-slot copy shares the table.
* **NOT shape (B)** (an `%ENV-MARKER%`-style special value): that costs an
  arm at every hash chokepoint (gethash, setf, keys, values, each, exists,
  delete, scalar).  The marker pattern predates value-level magic and
  should not grow new members.
* Follow-on, separate task, module layer (rule 9a): `lib/Errno.pm` shim
  exporting the constants (`ENOENT`, …) by reading the same runtime table
  through one builtin.  `%!` itself is core (perl auto-loads Errno for it;
  PCL just defines it) and lives in the runtime.

## 5. The #602 fold — `*A = *B` becomes CLEAR-THEN-COPY (shape (a))

Approved: `p-glob-copy` makes every slot the source LACKS empty, using
`p-glob-undef-name`'s empties (14854): scalar → fresh undef box, array →
empty adjustable vector, hash → empty table, code → `fmakunbound` + the
`*p-declared-subs*` entry removed, IO → the `*p-filehandles*` registration
removed.  Notes that are part of the ruling:

* It fires ONLY on the glob-to-glob arm.  The import path
  (`*foo = \&sub`, `*{"${caller}::$name"} = \&sub`) goes through the
  `functionp` arm of `%p-glob-assign-slots` and is untouched — so the
  clear cannot empty a real slot on the import shape.  Probe it anyway
  (the #602 bar names the import-shaped cases).
* Clear only slots that are BOUND on the destination; leave an unbound
  destination slot unbound (emptying it would *create* package variables
  `*A = *B` never creates in perl — probe `defined $A` after).
* Shape (b) — both names one GV — stays with the boxed-aggregate/glob-value
  family (post-v0.1 design), exactly as the task says.
* Interaction with §3: `*^R = *src-without-scalar` now clears `|$^R|` to a
  fresh undef box — that is t/re/pat.t:1715's expected behaviour (#602's
  motivating row).  For the COMPUTED magics (`*! = *src`) the clear
  replaces the canonical magic box in the slot; plain `$!` keeps computing
  (§3 boundary) — probe perl, record.

## 6. Bar for the implementing session (one Opus session, cl/-only, no gen bump)

* The #561 three-line repro (`*Y = *{"!"}` and `*Z = *!`) + `%!` truthiness
  + t/re/reg_namedcapture.t 0 ok / 2 notok → passing.
* The #602 repro + the B-has-some-slots case + the import-shaped cases +
  pat.t:1715.
* ~10 probes vs perl 5.40.3 incl.: dualvar through the alias (`$Y+0` /
  `"$Y"`), `$Y = 5` writes errno through the alias, `local $!` still
  restores, `values %!` count and truth, element write.
* cl/ change ⇒ full sweep (Fable runs it if this ships in an agent round)
  + op/ + re/ companion legs; guard rows in `Pl/t/punct-glob-name-01.t`
  plus a new errno guard set.
* `docs/ir-spec.md` §8 gains the canonical-box rule and `%!` in the table
  (in the implementing commit).
