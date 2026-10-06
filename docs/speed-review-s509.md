# Speed review: what is left, and is any of it cheap? (s509, 2026-10-06)

The USER's question at the end of s508: *"a review of the remaining speed work? Are there still low
hanging fruit?"*  This is the answer, from measurements taken on main `b43188b5` (generation v2-4280)
on 2026-10-06.  The box was shared with two running batches (load 5–6), so every time below is the best
of 3–5 runs and the PROFILES, not the ratios, carry the argument.  Files: `~/pcl-agent-scratch/s509/review/speed/`
(`prof.pl ROW…` writes each bench row as a standalone program, times perl and PCL and takes an `sb-sprof`
flat profile into `ROW.prof`; `leverA.lisp` is the hand-replaced A/B below).

**Short answer: yes, at two levels.**

1. **For ordinary programs the cheapest large win is still the start-up, which is parked by a USER ruling**
   (2026-09-27: documented, not optimized).  Nothing has changed there since it was measured.
2. **For run time there is one lever that is five lines and halves two bench rows, and three more that
   are each a small change inside one runtime function.**  They were not on the list before; they are
   filed as #2770–#2773.  After those, what remains on the bench table is mid-size or large.

## 1. Ordinary programs: start-up, re-measured

| | today (load 5) | s499 (quiet) |
|---|---:|---:|
| `perl hi.pl` | 2.5 ms | 1.4 ms |
| `pcl hi.pl`, warm | 53 ms | 43–48 ms |
| of that: compiling the `pcl` launcher (`perl -c pcl`) | 27 ms | — |
| of that: SBCL booting the 53 MB core and exiting | 6.5 ms | 3–7 ms |
| `pcl -e 1` (never cached) | 243 ms | 169 ms |

So a one-line program is ~20× perl, almost all of it the Perl launcher, exactly as §0.2r of
`docs/faster-codegen-suggestions.md` found (56 % of the everyday corpus's warm time is start-up; only
~5 % of ordinary programs are bound by their run phase).  The parked items, in order of gain per effort:

| task | what | size | measured gain |
|---|---|---|---|
| #2422 | the launcher: remember the core name, no `sbcl --version` spawn, lazy module loads | S | ~30 of ~50 ms on EVERY run |
| #1862 | `pcl -e CODE` / `-M` runs are not cached | S–M | 243 ms → ~50 ms for a one-liner |
| #2421 | a cheap compile policy around string-eval'd code | S | Moo class set-up 2.65 → 0.54 s (the `moo-objs` row is 26× perl) |
| #2420 | a first run loads existing module fasls instead of their text | S | first runs −56 % |
| #2423 | `pl2cl`'s 0.17 s + 3 ms per line | L | first runs only |

These are the low-hanging fruit by any measure that counts ordinary programs.  They stay parked until
the USER reverses the ruling; **the recommendation is to reverse it for #2422, #1862 and #2421** (three
small changes, each in one place, each with a measured gain larger than any run-time lever below).
#2420 touches the first-run build path that #2702 (the exactly-once first run) will redesign — do them
together, not now.

**USER RULING (2026-10-06, after reading this): the start-up items STAY PARKED "for a bit longer. They feel
risky, we are aiming for sleek functionality and good documentation right now."**  So #2422, #1862, #2421,
#2420 and #2423 are not scheduled; section 2's run-time levers are unaffected.

## 2. Run time: the bench table's slow rows, profiled

56 rows; 23 are slower than perl by more than 1.4×.  `pack` / `packunpk` (146–163×; `pack` is written in
Perl and "will be redone") and `moo-objs` (26×; #2421) are their own subjects.  The rest is an I/O and
string cluster at 2–4×.  Thirteen rows profiled:

| row | pcl / perl | where the time is (self %, unless "total") | class |
|---|---:|---|---|
| catmod (`$s .= 'xy'`) | 2.7× | generic `replace` 39 + `ub32-bash-copy` 13.5 | **lever A** |
| catself (`$s = $s . 'xy'`) | 2.3× | `replace` 36 + bash-copy 16 | **lever A** |
| listdeclcat | 1.8× | `replace` 23 + bash-copy 9; `box-set` 14 + increment 13 (a list-declared `$i` stays boxed) | **lever A**, then verdict coverage |
| joinarr (`join`, `"@a[0..3]"`) | 3.1× | `replace` 27 + bash-copy 9; slice aliasing 15 total | **lever A** sibling; the rvalue slice builds alias boxes |
| lcbytes (`lc . uc . ucfirst`) | 2.9× | `%concatenate-to-string` 29 total, bash-copy 15, case map 16 | **lever A** sibling |
| grepcnt (`my $k = grep $_, @p`) | 2.1× | `%p-collect-list` 26 total (a list built only to be counted), hairy vector refs 15, slow truthiness 14 | **lever C** |
| unshiftq | 2.0× | `%make-p-box` 14 (a box per unshifted number), `replace` 18 | **lever D** |
| fhprint (and every `print`) | 3.3× | `p-flatten-args` 13, `%make-p-box` 8, `vector-to-list` 3, `%p-resolve-fh` 14, the write 9 | **lever B** |
| fhread | 3.5× | `%p-read-record` 49 total, signal mask 7, handle lookup 6 | round 39 takes ~20 %; the rest is the per-line machinery |
| subste (`s///ge`, `s///g`) | 3.1× | cl-ppcre scanner closures ~35, string-stream result building ~20, `build-replacement` 22 total | the regex engine (parked) + mid |
| ovlsub | 2.6× | spread: per-object EQUAL hash table 15–20, flatten 12, boxes 10 | #1890, large (object representation) |
| feargs (`f(@a)`, 1000 elements) | 2.4× | `%p-flatten-run` 61 | mid: `@_` is built element by element |
| textproc / fprint / tiehash / … | 1.6–2.3× | not profiled this session | — |

### Lever A — a TYPED copy where the runtime calls generic `replace` / `concatenate` on strings (#2770)

`%pcl-str-blit` (the copy inside the string buffer's append) has an inline arm for ONE character and
calls `replace` for everything else, with the source declared only `string`.  SBCL then enters the
generic sequence function, which is where 36–39 % of `catmod` / `catself` is spent — to move two
characters.  A hand-replaced copy of the two functions with one more arm (source is a simple character
string: an inline loop up to 16 characters, a both-sides-typed `replace` above that), loaded in front of
the unchanged programs:

| row | perl | main | typed copy | |
|---|---:|---:|---:|---:|
| catmod | 0.206 | 0.603 | 0.290 | −52 % |
| catself | 0.248 | 0.582 | 0.270 | −54 % |
| listdeclcat | 0.169 | 0.352 | 0.238 | −32 % |
| strcat (one character: already inline) | 0.439 | 0.424 | 0.439 | noise |

That is the whole change for the first three rows.  The same call shape is in `%p-join-strings`
(`joinarr`: 36 % in `replace` + bash-copy), in the three-way concatenation behind `lc($s) . uc($s) .
ucfirst($s)` and, after round 39, in the `:strbuf` cell's append: one helper, used at each site (rule
11).  Expected on those rows: −20 to −30 % each; to be measured by the round that takes it.

### Lever B — `print` pays the generic list path for one string (#2771)

Six spellings (`print {$fh} "x\n"`, `print $fh "x\n"`, `print $fh $x`, two arguments, STDOUT, an
interpolated string), 2 million calls each: every one is **3.1–3.9× perl** (145 ns against 45 ns per
call).  The profile is the same for all: the arguments are flattened into a fresh vector, a box is made,
the vector becomes a list, the handle is resolved, and only then are 2 bytes written.  When every
argument form is a scalar at compile time (a literal, an interpolated string, a scalar variable, a
concatenation) none of that is needed.  This is the most common statement in ordinary programs, so it is
worth a compile-time arm — but it is NOT five lines: `$,`, `$\`, tied and in-memory handles and the
selected handle must keep their one path.  Medium; expected −40 to −50 % on `fhprint` and on any
output-heavy loop.

### Lever C — `grep` in scalar context builds the list it only counts (#2772)

`my $k = grep $_, @p`: a quarter of the row is collecting matches, another 15 % is reading the array
through the adjustable-vector accessor instead of its data vector, and 14 % is a slow truthiness test on
small integers.  Three small changes inside `p-grep`.  Expected −35 to −45 % on `grepcnt`; the counting
form (`if (grep { … } @list)`, `my $n = grep …`) is everywhere in ordinary code.

### Lever D — `unshift` boxes each number (#2773)

`unshift @a, $_` allocates a box per element (`push` stores it raw) and shifts through generic
`replace`.  Small; expected −25 % on `unshiftq`.

### Found on the way (not a lever, a bug with a cost)

A regex anchored at the END of a string (`/9\n\z/`) scans from the start: appending a line and testing
the tail, 30 000 times, takes 11 s on main and on the round-39 tree, 0.09 s in perl (#2723, filed by
s507p).  Programs that accumulate a buffer and test its end do exactly this.

## 3. What is NOT cheap

- **fhread** beyond round 39: a line read is a handle lookup, a signal-mask pair, a record scan and a copy.
  Reading in blocks and handing out lines is a redesign of the reader, not a clause.
- **s///ge and the regex rows**: the engine (cl-ppcre closures) and the replacement path through string
  streams.  The engine is parked by decision (ONE regex engine); the stream part is mid-size.
- **Objects** (`ovlsub`, `moo-objs` at run time): the per-object EQUAL hash table (#1890).
- **`f(@big)`**: `@_` aliasing built element by element.
- **`pack`**: a rewrite.
- **`use constant` with a non-literal value** was re-evaluated at every use until s508a's #2681
  (2 M reads of a hash-ref constant 0.46 → 0.16–0.34 s); that batch is not merged yet.

## 4. Recommendation

One perf round (round 40, one Opus agent, the standard bars — hand-replaced A/B first, control rows,
gate + sweep, everyday `--record`), in this order: **#2770 (lever A and its sibling sites) → #2772 →
#2773 → #2771 (with a short design note first)**.  Expected: five rows move by 25–55 %, and `print`
by 40–50 % if B lands.  The decision put to the USER — whether #2422 / #1862 / #2421 leave the parked
list — was answered the same day: they stay parked (section 1).
