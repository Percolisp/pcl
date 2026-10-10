# `tie` on an ARRAY and on a HASH (task #155, s501t)

Perl lets a program replace an array's or a hash's storage with a class: `tie
@a, 'Class'` calls `Class->TIEARRAY`, and from then on every read, write, size
question and iteration of `@a` is a METHOD CALL (`FETCH`, `STORE`, `FETCHSIZE`,
`FIRSTKEY` / `NEXTKEY`, ...).  PCL had the scalar half since phase 1; this
page is the aggregate half.  The design is Fable's (task #155, "empty shell +
side table"); this page is what was built, the census that places every test,
and the method-call table that says where PCL still differs from perl.

## 1. The representation (normative copy in `docs/ir-spec.md` §2)

* **A tied container stays THE SAME LISP OBJECT, EMPTIED.**  `tie` moves the
  contents aside (perl hides them while tied and `untie` gives them back) and
  leaves the vector / hash-table empty.  Identity is what makes the tie visible
  through every alias perl has -- `\@a` taken before the tie, `tie @$ref`,
  `@_`, a symbolic name (`tie %{"main::G"}`), a glob.  A blessed hash keeps its
  internal `:__class__` entry in the shell; no element path can see it.
* **The tie lives in a SIDE TABLE**: `**p-tie-table**`, a weak `EQ`
  hash-table container -> `p-tie-rec` {object, kind, saved contents, each()
  iterator}.  A global fixnum `**p-tie-count**` (`sb-ext:defglobal`: no TLS
  lookup) guards every lookup -- `%p-tied` is
  `(and (plusp count) (gethash c table))`.  A program that never ties pays the
  count test at the census sites and nothing else.
* **EMPTY is the trick**: on an empty shell every element fast path MISSES,
  so the tie test rides in the miss branch that already existed.  An in-range
  element read of an untied container costs exactly what it cost before.
* **An element handed out as an lvalue** -- `for (@tied)`, `\$tied{k}`,
  `f($tied[0])`, `$tied{k}++`, `values %tied` -- is a fresh box holding a
  `p-magic-cell` of kind `:tielem` whose getter is `FETCH(key)` and whose
  setter is `STORE(key, v)`: perl's PVLV, served by the scalar tie's own two
  chokepoints (`unbox`, `box-set`) with no new case.  The cell keeps its first
  FETCH until a write (one operator reading its operand twice -- `$h{k}++`,
  `$_ // 'u'` -- FETCHes once, as perl's `mg_get` does), and a write forgets it.
* **A list-context use** of a tied container reads its VIEW (`%p-tie-view`): a
  fresh vector of element proxies, an array's `0 .. FETCHSIZE-1`, a hash's
  keys first (FIRSTKEY / NEXTKEY) then its value proxies -- perl's order.  A
  copying consumer unboxes them (FETCH in order), an aliasing one writes
  through them.
* **What `tie` / `tied` / `untie` do**: `tie` returns the object, a re-tie
  REPLACES it (the scalar rule) and keeps the hidden contents; `tied` answers
  the object; `untie` calls `UNTIE` when the class has one and restores the
  hidden contents -- less whatever a whole-container assignment wiped (perl's
  `hv_clear` / `av_clear` clear the hidden storage too: probed, `%h = ()` then
  `untie` leaves `%h` empty).  **DESTROY is never called** (USER ruling,
  permanent).  A negative subscript is rebased on FETCHSIZE unless the class
  sets `$NEGATIVE_INDICES` (#1429); still negative, a read is undef with no
  FETCH and an lvalue use is perl's "Modification of non-creatable array value
  attempted"; a negative FETCHSIZE is perl's croak.

## 2. The cost (Step 0, measured before anything was built)

A 30-line replica added ONLY the count test to the hash store path and the
entry of push / shift / keys / flatten, and was benched against base
`a0899ba6` with a byte-identical control core in the same window (base
extraction's `tools/bench-exec.pl`, `BENCH_RT_B`, interleaved):

| row | replica B/A | control B/A |
|---|---:|---:|
| intloop+= | -3.8 % | +8.6 % |
| intloop= | -12.3 % | +0.1 % |
| arrhash | +4.7 %, +1.5 %, +0.5 % | +6.1 %, -0.3 %, -4.7 % |
| arrhash-k | -1.0 % | +4.2 % |
| strcat | -1.7 % | -0.3 % |
| slices | -1.5 % | -0.3 % |
| sortnum | -11.3 % | -0.4 % |
| textproc | +1.9 % | +1.8 % |
| mapmulti | +1.5 % | +1.2 % |
| hashcopy | -1.0 %, -1.2 %, -1.8 % | +1.9 %, -1.3 %, -1.6 % |

Load 1.1-1.7, no other heavy leg.  The control band is -4.7 .. +8.6 %; no
replica row reads above it, so the design's placement stands (no ruling was
needed).  The whole-tree re-take is in the s501t session-log section.

## 3. The census -- where the test goes

Every runtime function and macro expansion that reads or writes a user
container was listed (a scan of `cl/pcl-runtime.lisp` for the container
primitives: 264 forms), classified, and either given its tied form or shown
not to receive a user container.  "Reads the empty shell" is not an allowed
entry (rule 12): a container operation with no tied form DIES naming itself
(`%p-tie-refuse`).

| class | sites | what a tied container gets |
|---|---|---|
| (a) element READ, in the miss branch (free) | `p-gethash` (fast arm + general arm), `p-aref` (fast arm miss; one count test on the general path, which also carries the negative subscript) | FETCH (`%p-hash-miss`, `%p-aref-miss`) |
| (b) array element WRITE, in the extension path | `(setf p-aref)` fast arm (only when `idx >= length`, i.e. every store to a shell) and general path | STORE; a raw aggregate value is its count (`%pcl-scalar-collapse`) |
| (c) hash element WRITE -- the ONE count test on a store path | `%p-gethash-store` (every `(setf p-gethash)` arm, `elem-setf`, slice stores) | STORE, raw aggregate collapsed to its count |
| (d) whole-container entry test | `p-keys` `p-values` `p-each` `%p-exists-p` `p-delete` `p-exists-array` `p-delete-array` `p-delete-hash-slice` `p-delete-kv-hash-slice` `p-delete-array-slice` `p-delete-kv-array-slice` `p-push-impl` `%p-push1` `p-pop` `p-shift` `p-unshift` `p-splice-impl` `p-array-fill` `p-hash-fill` `p-array-deref-=` `p-hash-deref-=` `p-undef` `p-set-array-length` `p-array-last-index` `p-scalar` `%p-hash-user-count` `%p-true-p-slow` `%to-number-raw` `%pcl-scalar-collapse` `box-set` (scalar-assign count) `%p-str-x-plain` `flatten-list-elements` `p-chomp-one` `p-chop-one` `p-return-value` `%p-leavesub-aggregate` `p-copy-array` `p-copy-hash` `p-array-init` `p-hash` `p-refgen-list` `p-sprintf` `p-join` `p-reverse` `%p-collect-list` (map / grep / reverse) `%p-sort-values` (sort: the FETCHed values) `%p-flatten-list` + `%p-flatten-list-general` `%p-flatten-for-list` (foreach) `p-flatten-args` (@_, method args, print) `%p-hash-keyval-list` `p-kv-aslice` | the method(s) perl calls -- see §4; list-context consumers read the view |
| (d) element-alias entry | `p-gethash-box` `p-gethash-argbox` `%p-alias-helem` (hash slices, kv slices, `values`) `p-aref-box` `p-aref-argbox` (`%p-alias-aelem`, array slices) `p-autoviv-gethash` `p-autoviv-gethash-for-array` `p-autoviv-aref-for-hash` `p-autoviv-aref-for-array` `p-cast-@` / `p-cast-%` vivify arms (`%p-viv-result`) | an element proxy box; autovivification = FETCH, STORE a new container, FETCH it back (perl's order) |
| (d) `local` | `%p-lhe-save` / `%p-lhe-init` / `%p-lhe-restore` (`local $h{k}`), `p-local-array-elem` / `-init` (macros; body expanded once through an FLET), `%p-local-array-slice-nested` | EXISTS, FETCH if it exists, STORE (undef or the initializer); on exit STORE the old value or DELETE |
| (e) emitted RAW shapes | `foreach-arrays` (`:arrays t` -> `%p-make-array-run`: one count test per array, the tied one runs over its view); `local-push` (`%p-push1`, above); `elem-setf` (CL `setf` of `p-gethash` / `p-aref` = classes b/c); `p-hash-=`'s list-context value (macro: the view when tied); `p-setf` / `p-list-=` slice and element targets (expand to `(setf p-aref)` / `(setf p-gethash)`) | the runtime entry's answer; no Kind-A shape reads container storage without passing one of the entries above |
| DIES (rule 12: no tied form) | `p-alias-array-elements` (`\(@a) = LIST`), `p-alias-hash-slot` (`\$h{k} = REF`), `p-alias-array-slot` (`\$a[i] = REF`) | `PCL: refaliasing ... on a tied ARRAY/HASH is not supported` (perl-shaped, trappable) |
| not a user-container site | the array WINDOW internals (`%p-array-shift-front` / `-drop-front` / `-unshift-*`, reached only after the entry test), bulk-fill / store helpers behind `p-array-fill` / `p-push-impl` (`%p-array-store-scalar`, `%p-array-bulk-*`, `%p-array-fill-*`, `%p-snapshot-array-rhs`, `%p-flatten-grow`, `%p-flatten-vector-*`, `%p-vpush`, `%p-extend-to`, `%p-aref-store`), readonly-array flags, overload / class tables, MRO and method caches, stream and record readers, glob expansion, regex engine, import / export lists, signal boot, capture buffers, `%p-sort-collect-plain` (after `%p-sort-values`' views), `%p-join-args` (after `p-join`'s views), `%p-listslice-array` / `p-list-scalar` (list temporaries), `p-cast-@` / `p-cast-%` / `p-backslash` / `p-bless` / `p-ref` (identity -- the shell IS the container) | -- |

**Container shapes needed no VarAnnotator gate**: `tie @a` was
already an array ESCAPE event (#1140), which denies `local-push` and
`foreach-arrays` for an array the same file ties by name, and every other
container shape goes through a runtime entry that now takes the test -- so a
container tied ELSEWHERE is right too (probe `shapes2.pl`: a package array
tied through a symbolic name in a sub, then iterated).  No emission changed
except `tie ${"name"}` (§5) -- and ONE type gate: a file that ties a HASH
(`_tie_hash_in_file`, set by Parser2) does not freeze a `strkey` variable to a
string, because a tied hash's methods see the key AS GIVEN (Tie::RefHash, an
undef key).

Sites added after the census (sweep, companion and rebase findings):
`p-array-=` (segment fill, `@t = reverse @t` in place through the methods),
the `p-list-=` collect forms (`%p-collect-tied-lhs`: the list-context value),
`p-arg-supplied-p` and `p-sig-rest-array` / `-hash` (signature defaults and
slurpies over a tied @_ -- they moved out of the "not a site" row),
`%p-flatten-arg-values` (s501q's args-copy / `p-raw-params` path: the same
view arms as `p-flatten-args`), `%p-defelem-box` (a deferred element of an
array tied later FETCHes), the `p-cast-@` / `p-cast-%` vivify arms, and the
`local` of a dereferenced tied element (refused, rule 12).
An entry that only ONE container kind has (an element store, `exists`, `delete`,
an element box or autovivification through `$r->{k}`, and their `$r->[i]`
twins) asks `%p-when-tied-kind`: the tie of the OTHER kind is never consulted,
so `$tied_array_ref->{k} = 1` stays perl's "Not a HASH reference" (t/op/avhv.t).
The kind is read from the record after the count test -- no cost untied.
Review findings (s505, F1-F3): **a tied element proxy whose FETCH answers a
reference is read AS that reference** wherever a reference is consumed --
a nested subscript in argument / lvalue position goes on with the referenced
container (`%p-tielem-container`, in `p-gethash-box` / `p-aref-box` and the
slow arms of `p-gethash-argbox` / `p-aref-argbox`), `ref()` (`p-ref`) and the
method invocant's class (`%pcl-invocant-class`) read it, and the proxy carries
the wrapper's class (`%p-tie-elem-value`, cleared on STORE); **boolean context
of a tied hash is the truth of SCALAR** (`%p-true-p-slow`'s hash arm -- the
`scalar(%h)` rule of §1, truth-tested); and `lib/Config.pm` gained `path_sep`
(core Env.pm splits `@PATH` on it).

## 4. The method-call table (perl's answers first)

`Pl/t/tie-aggregate-01.t` runs a logging `Tie::StdHash` / `Tie::StdArray`
subclass through 126 operations (56 hash, 67 array, and 3 scalar-dereference
rows in the array program) and compares every line -- the methods called,
their order and count, and the result -- with perl 5.40.3; the file is 128
rows (one line-count row per program).  **116 are identical.**  Ten differ in the LOG only (the results are
identical; `a:untie-restores` is the void-push row again, `a:list-assign-list`
the assign-count row):

| operation | perl | PCL | why |
|---|---|---|---|
| `++$h{a}` | FETCH STORE FETCH | FETCH STORE | PCL returns the stored value; perl re-reads it |
| `sort values %h` | FETCH in key order | FETCH in comparison order | sort reads the lazy value proxies |
| `delete local $h{k}` | EXISTS FETCH DELETE | EXISTS FETCH STORE(undef) DELETE | PCL localizes, then deletes |
| `for (@a) { ... }` (alias and read) | FETCHSIZE before every iteration | FETCHSIZE once | the loop takes the size once |
| `scalar(@a = LIST)` | no FETCHSIZE | one FETCHSIZE | PCL counts the array, perl the RHS |
| `push @$r, 1` in void context | PUSH | PUSH FETCHSIZE | `*wantarray*` at a call ARGUMENT is the enclosing statement's (`is(push(@t, 4), 3)` runs under void), so PCL cannot skip the length |
| `my ($x, $y) = @_` over a tied array passed whole | FETCH of EVERY element (1..N-1, then 0) | FETCH of the bound parameters only | the copying callee (s501q's args-copy lever) binds from the argument view and copies only what it binds |

A consequence of the second-to-last class: a loop that grows or shrinks the
tied array it iterates sees the size it started with.

## 5. `tie ${"name"}` -- Env.pm (phase 3)

Core `Env.pm` ties `${"${callpack}::$name"}` and `@{"${callpack}::$name"}`.
The array half worked once phases 1-2 did; the scalar half did not, because
`p-cast-$` answers a symbolic name with its VALUE.  `tie` / `untie` / `tied`
on a scalar dereference now emit `p-cast-$-box` (the same resolver answering
the vivified package scalar's BOX; a hard reference gives its referent box),
so `use Env qw(HOME @PATH)` works.

## 6. What is left

* **TIEHANDLE** (`tie *FH`, `tie *$fh`): IMPLEMENTED s514d (task #2590), §7.
* **DESTROY** is never called (USER ruling).  t/op/tiearray.t's three
  "freed" rows fail for that reason.
* `arysize()--` with `sub arysize :lvalue { $#ary }` -- `:lvalue` subs are
  USER-deferred (#930).
* The named log differences in §4.

## 7. Tied handles (task #2590, s514d)

`tie *FH, 'Class', ARGS` / `tie *STDOUT, ...` / `tie *$fh, ...` call
`Class->TIEHANDLE(ARGS)` and record the tie in a SECOND side table,
`**p-handle-tie-table**` (weak `EQ`), keyed by the STREAM the glob's IO slot
holds -- so every spelling that reaches the same IO (`\*FH`, `{*FH}`, `*$fh`,
`*ALIAS = *FH`, `select(FH)`, a handle passed to a sub) reaches the same
record.  A glob that was never opened gets a `p-tied-handle-stream`
placeholder (a Gray stream that dies naming the operation if any builtin
reaches it without dispatching -- rule 12) registered as its IO, and `untie`
removes it again.  perl ties only when TIEHANDLE returns an OBJECT; a re-tie
replaces the object.

**The cost (Step 0, measured before the design was chosen).**  A handle has no
free miss branch, so every I/O builtin asks at its ENTRY.  Two shapes were
replicated and benched interleaved against a byte-identical control core
(`scratch/s514d/step0-{A,B}.log`):

| shape | fhprint | fhread | band (rows with no hook) |
|---|---|---|---|
| A: a fixnum count `**p-handle-tie-count**` (its own, so a tied ARRAY costs print nothing), then the table | +1.3 % in A's favour | +1.1 % in A's favour | ±2.8 % |
| B: `typep` on a sentinel stream class | 11.3 % slower | 4.7 % slower | ±0.4 % |

A is free and was built.  The entries: `%p-write-list` (print, say and the
fixed-arity print arms), `p-printf`, `%p-readline-impl` / `%p-readline-all`
(and the `<>` pair for a tied ARGV), `%p-eof-impl` / `p-eof-argv`,
`%p-getc-impl`, `%p-read-impl` (read and sysread), `%p-syswrite-impl`,
`%p-close-impl`, `%p-binmode-impl`, `%p-fileno-impl`, `%p-seek-impl`,
`%p-tell-impl`, `%p-open-impl`, and `p-warn`'s default arm (a tied STDERR).

**The method table (perl's answers, probed 5.40.3; guard `Pl/t/tiehandle-01.t`).**

| builtin | method call |
|---|---|
| `print FH LIST` | `PRINT(LIST)` -- the list, not a joined string; `$,` and `$\` are the method's business |
| `say FH LIST` | `PRINT(LIST)` with `$\` localized to `"\n"` |
| `printf FH FMT, LIST` | `PRINTF(FMT, LIST)` |
| `<FH>` / `readline` | `READLINE` in the caller's context; undef ends a `while (<FH>)` |
| `read` / `sysread FH, BUF, LEN [, OFF]` | `READ(BUF, LEN [, OFF])` -- `$_[1]` ALIASES the caller's buffer |
| `syswrite FH, BUF [, LEN [, OFF]]` | `WRITE(BUF, LEN [, OFF])`, LEN = `length(BUF)` when absent |
| `getc FH` | `GETC` |
| `eof(FH)` / bare `eof` / `eof()` | `EOF(1)` / `EOF(0)` (the last-read handle) / `EOF(2)` (ARGV) |
| `close` `binmode [LAYER]` `fileno` `seek POS, WHENCE` `tell` | `CLOSE` `BINMODE([LAYER])` `FILENO` `SEEK(POS, WHENCE)` `TELL` |
| `open FH, MODE, LIST` / `open FH, EXPR` | `OPEN(MODE, LIST)` / `OPEN(EXPR)` |
| `tied *FH` / `untie *FH` | the object / `UNTIE` (when defined), then untied |
| `warn` with a tied STDERR | `PRINT(MESSAGE)` |

`local *FH` undoes a tie made inside its scope (perl change 11639);
otherwise `local *FH` still does not localize a glob's IO slot (pre-existing).
A missing method dies perl's way (`Can't locate object method "PRINTF" via
package "T"`), trappably.  DESTROY is never called (USER ruling).

t/op/tiehandle.t alone (s514d): base 11 pass / 32 fail / 24 never produced -> 52 pass /
2 fail / 13 never produced, the rest behind #3140 (a `package X;` switch inside a block with a `local` re-defines
the class with no parents: the `Bar::Say` block aborts) and #221 (no
warnings-gated diagnostic: `print undef` under `use warnings`).
