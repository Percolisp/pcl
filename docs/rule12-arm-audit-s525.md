# The 66 default arms, audited (s525u, 2026-10-24)

`docs/runtime-review-s525.md` §8 counted **66 `(t|otherwise nil|0|"")` arms**
in `cl/pcl-runtime.lisp` and recommended (R6) one verdict per arm.  This is
that audit, against CLAUDE.md rule 12 and its s329 boundary: a dispatch over a
CLOSED set that silently answers a default for a missing case DIES naming the
value when a VALUE flows onward; an effect-only missing case ANNOUNCES; an arm
that is the legitimate answer over an OPEN set, or a documented NIL protocol
("not this shape", "none"), is KEPT.

**The count.**  The review's grep `^\s*\((t|otherwise)\s+(nil|0|"")\)` finds
**60** arms at the start of a line on main `ef5df65a`; **6** more sit inline
at the end of a `cond` (5448, 12794, 12803, 13168, 13175, 19830) -- 66 in all,
which is the review's number.  (A seventh inline match, 25482, is a comment.)
Line numbers below are main `ef5df65a`'s.

**Result: 66 of 66 with a verdict -- 62 KEEP, 4 DIE (converted), 0 ANNOUNCE.**
None of the four converted arms is reachable from a Perl program: each closes
a dispatch whose legal values all have arms.  They are converted for the
invariant, as `p-vec`'s width dispatch was the archetype of (64 was listed as
legal, had no arm, and read 0).  Guard: `Pl/t/rule12-arms-01.t` (the legal
neighbours of all four as perl-probed rows; the two arms a CL caller CAN reach
with an illegal value assert their die text; the other two are unreachable
even from CL and have no row).

Columns: **set** = what the dispatch ranges over; **O/C** = open or closed;
**value?** = does the arm's answer flow on as a value the program consumes.

| # | line | function | set | O/C | value? | verdict |
|---|---|---|---|---|---|---|
| 1 | 1064 | `%p-tie-shell-shape-p` | any container | O | predicate | KEEP: false is the answer |
| 2 | 4417 | `%pcl-nan-canonical-p` | NaN spellings | O | predicate | KEEP |
| 3 | 5105 | `%p-ref-address` | box payload states | O | yes | KEEP: "a non-reference numifies to 0" is the documented answer (a raw glob value is `*pkg::x` = 0) |
| 4 | 5454 | `stringify-value` (qr flags /x) | modifier flags | O | yes | KEEP: no flag = no letter |
| 5 | 5829 | `%p-plain-num` | any value | O | NIL protocol | KEEP: NIL = take the general path |
| 6 | 6177 | `%p-pow-int` | operand ranges | O | NIL protocol | KEEP: NIL = "float it" |
| 7 | 6231 | `%p-pow-small` | operand ranges | O | NIL protocol | KEEP: NIL = outside the cheap bounds |
| 8 | 6502 | `%to-number-raw` | raw value states | O | yes | KEEP: perl numifies every scalar; the arms list the non-zero answers (a glob value, a list are 0) |
| 9 | 6550 | `%p-array-index` (length) | container | O | yes | KEEP: a non-array has no elements; the wrong-referent die is the caller's (probed `$h->[0]` dies as perl) |
| 10 | 6992 | `%p-str-data` | string shapes | O | NIL protocol | KEEP |
| 11 | 7434 | `p-chomp-one` | place shapes | O | yes | KEEP: a non-modifiable value removes 0 |
| 12 | 7495 | `p-chop-one` | place shapes | O | yes | KEEP: same, "" |
| 13 | 8010 | `sprintf-inf-nan-p` | doubles | O | predicate | KEEP |
| 14 | 8031 | `sprintf-format-int` (alt prefix) | base 16 8 2 10 | **C** | yes | **DIE**: `(10 "")` + die naming the base; unreachable (the one caller passes ALT-FORM-P NIL) |
| 15 | 8264 | `sprintf-one` (alt prefix) | base 16 2 8 10 | **C** | yes | **DIE**: `((= base 10) "")` + die naming the conversion and base; unreachable (BASE comes from the conversion letter) |
| 16 | 8270 | `sprintf-one` (int sign) | flag chain | O | yes | KEEP: no flag = no sign |
| 17 | 8295 | `sprintf-one` (Inf/NaN sign) | flag chain | O | yes | KEEP |
| 18 | 8303 | `sprintf-one` (float sign) | flag chain | O | yes | KEEP |
| 19 | 8352 | `sprintf-one` (%a sign) | flag chain | O | yes | KEEP |
| 20 | 9058 | `%p-vec-data` | vector shapes | O | NIL protocol | KEEP |
| 21 | 9926 | `%p-fill-segments` (in `p-array-fill`'s eval-when) | RHS forms | O | NIL protocol | KEEP: NIL = not this shape (macro time) |
| 22 | 10055 | `%p-map-list-tail` (same block) | block forms | O | NIL protocol | KEEP |
| 23 | 12475 | `%p-elem-append` | element contents | O | NIL protocol | KEEP: NIL = the ordinary path |
| 24 | 13739 | `p-aref` (length) | container | O | yes | KEEP: as #9 |
| 25 | 13798 | `%p-peek-form` (in `%p-peek-aelem`'s eval-when) | read forms | O | NIL protocol | KEEP |
| 26 | 15245 | `%p-symref-name` | scalar values | O | NIL protocol | KEEP: NIL = designates no variable |
| 27 | 16057 | `%p-exists-p` | container | O | yes | KEEP: perl's false for an undef intermediate; a wrong referent dies one arm above |
| 28 | 16385 | `%p-stash-add-child-namespaces` | package names | O | filter | KEEP |
| 29 | 16417 | `%p-stash-variable-name` | symbols | O | NIL protocol | KEEP |
| 30 | 18134 | `%p-die-hook` | `$SIG{__DIE__}` value | O | NIL protocol | KEEP: no handler |
| 31 | 18922 | `%p-as-stream` | handle payloads | O | NIL protocol | KEEP: NIL = not a handle, the caller reports it |
| 32 | 18943 | `%p-resolve-fh` (box arm) | box payloads | O | NIL protocol | KEEP |
| 33 | 18991 | `%p-resolve-fh` | designators | O | NIL protocol | KEEP |
| 34 | 19243 | `%p-cell-live-length` | magic-cell kinds | C | NIL protocol | KEEP: NIL "for any other cell" is the contract -- only the two private-buffer kinds have a live length |
| 35 | 19528 | `%p-std-name` | designators | O | NIL protocol | KEEP |
| 36 | 19654 | `%p-encoding-ef-lookup` | encoding names | O | NIL protocol | KEEP: NIL = no codec, the callers die / answer perl's `Unknown encoding` |
| 37 | 20646 | `%p-dup-src-name` | designators | O | NIL protocol | KEEP |
| 38 | 20875 | `%p-open-anon-temp-direction` | mode strings | O | NIL protocol | KEEP: NIL = the mode has no temp-file form |
| 39 | 21404 | `%p-scalar-deref-place` | values | O | NIL protocol | KEEP |
| 40 | 22103 | `%p-truncate-impl` | handle or path | O | NIL protocol | KEEP: NIL = not a handle, the path arm follows |
| 41 | 24505 | `%p-glob-one` | match results | O | yes | KEEP: no match and no NOCHECK = the empty list |
| 42 | 25132 | `*p-sig-mask-how*` | platforms | O | data | KEEP: NIL = unknown platform, tested by its one consumer (:25263) |
| 43 | 25498 | `p-vec` (read) | widths 1 2 4 8 16 32 64 x alignment | **C** | yes | **DIE**: names bits and offset; unreachable (an illegal width dies at entry, every legal one has an arm) |
| 44 | 27447 | `%p-parse-manifest-line` | manifest keys | O | NIL protocol | KEEP: NIL = line not understood, the manifest is rejected |
| 45 | 30180 | `%p-split-regex` (limit) | argument chain | O | yes | KEEP: no limit = 0 |
| 46 | 31381 | `%p-symref-cell` | symbol values | O | NIL protocol | KEEP (documented) |
| 47 | 31695 | `%p-ref-string` | referents | O | NIL protocol | KEEP: "NIL when VAL holds no recognizable reference, so each caller keeps its own fallback" |
| 48 | 31830 | `%p-target-class` | box payloads | O | NIL protocol | KEEP: no class |
| 49 | 31954 | `p-ref` | values | O | yes | KEEP: not a reference = "" (perl) |
| 50 | 32032 | `p-openhandle` | values | O | NIL protocol | KEEP: undef for a non-handle (perl) |
| 51 | 33823 | `p-coderef-exists-p` | values | O | yes | KEEP: false |
| 52 | 34046 | `%p-ta-index` | indices | O | NIL protocol | KEEP: NIL = read undef, no FETCH (probed) |
| 53 | 34330 | `%p-tie-container-kind` | containers | O | predicate | KEEP |
| 54 | 34650 | `p-wantarray` | context T / NIL / :VOID | **C** | yes | **DIE**: `((null …) "")` + die naming the context; unreachable from Perl (every binder passes one of the three) |
| 55 | 35191 | `p-get-class` | values | O | NIL protocol | KEEP: not blessed |
| 56 | 36842 | `%pcl-property-escape-end` | pattern text | O | NIL protocol | KEEP: left for cl-ppcre to refuse |
| 57 | 37044 | `%pcl-rx-quantifier-end` | pattern chars | O | NIL protocol | KEEP |
| 58 | 37165 | `%pcl-rx-ascii-escape-text` | escape letters | O | NIL protocol | KEEP: NIL = not an /a class |
| 59 | 37527 | `%p-literal-op-text` | forms | O | NIL protocol | KEEP: NIL = a run-time value |
| 60 | 38112 | `%pcl-regex-tail-allowance` | regex trees | O | NIL protocol | KEEP: NIL = no anchor bound (the conservative answer) |
| 61 | 5448 | `stringify-value` (qr charset) | modifier flags | O | yes | KEEP: as #4 |
| 62 | 12794 | `%p-<=>-slow` | trichotomy | C | yes | KEEP: equal is the third case, NaN answered before |
| 63 | 12803 | `p-<=>` | trichotomy | C | yes | KEEP: same |
| 64 | 13168 | `%p-str-cmp-slow` | trichotomy | C | yes | KEEP: same (strings are totally ordered) |
| 65 | 13175 | `p-str-cmp` | trichotomy | C | yes | KEEP: same |
| 66 | 19830 | `%p-layer-ef` (spelling) | argument chain | O | NIL protocol | KEEP: no current layer = no spelling |

What the audit says about rule 12 in this file: the default arms that remain
are, with four exceptions, NIL PROTOCOLS ("not this shape", "none", "no
handler") or the empty answer of an open set -- the s337c audit's "no
value-producing swallow" holds.  The pattern to watch for in review is the
CLOSED dispatch with an arm that happens to be one legal value's answer (#14,
#15, #54): the default then serves two purposes, the legal value and every
unforeseen one, and the fix is always the same -- name the legal value, die on
the rest.
