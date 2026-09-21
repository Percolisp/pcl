# Scope exit: closing filehandles and calling DESTROY without reference counts

*Design examination, s495 (Fable, 2026-09-22), on the USER's question: "how to handle
closing files (and even DESTROY) at scope exit — are guards possible with a reasonable
amount of work that won't slow down execution much?"*  Status: **DESIGN, nothing built.**
The filehandle half (Stage 1) is already briefed (`#2006(b)`, label s494h); Stages 0 and 2
are new here (task **#2370**).

## 1. The problem

Perl frees a value when its LAST REFERENCE goes away, and two everyday behaviours hang on
that moment: a lexical filehandle is flushed and closed, and a blessed object's `DESTROY`
runs.  PCL runs on SBCL's tracing collector — there is no reference count, so "the last
reference went away" is not an event the runtime ever sees.

What PCL does today, measured:

* **Filehandles** are closed only by an explicit `close`, by a re-open (#2006(a), s492b)
  or at program exit.  `for my $f (@files) { open(my $fh, "<", $f) or die; ... }` leaks
  one fd per iteration and dies at i = 1021 under the stock `ulimit -n 1024` (s494).
* **DESTROY is never called — not even at program exit.**  Probe
  `~/pcl-agent-scratch/s495/destroy/d1.pl` (ten shapes): perl makes 11 DESTROY calls, PCL 0.

Exact perl semantics would need a count on EVERY container and scalar (an object stored in
an array dies when the ARRAY dies, which dies when its holder dies, …).  That is the whole
memory model and is not on the table.  The question is how much of the *everyday*
behaviour can be had without it.

## 2. What the everyday behaviour actually is

**DESTROY is rare, and what it does is narrow.**  Census
(`~/pcl-agent-scratch/s495/destroy/destroy-census.pl`): all of core perl 5.40.3 has 46
`sub DESTROY`, 16 of them EMPTY (the AUTOLOAD-guard idiom); the 30 that do something
restore state (`SelectSaver`: `select $$self`; `autodie::Scope::Guard`; `Test2::Mock`,
`Test2::Util::Guard`), close or flush a handle (`DirHandle`, `IO::Compress::Base` — an
unflushed compressor is DATA LOSS — `IO::Zlib`, `Tie::File`, `Net::FTP/SMTP/POP3`), remove
a temp file (`File::Temp`), or delete a registry entry.  On CPAN the same three families
dominate: scope guards (`Scope::Guard`, `Guard`), resource handles (DBI's `disconnect` /
`finish`, lock files), and Moo's `DEMOLISH`.

**The objects that matter are held by ONE lexical.**  `my $saver = SelectSaver->new($fh);`
`my $guard = guard { ... };` `my $tmp = File::Temp->new;` `my $z = IO::Compress::Gzip->new(...)`
— a sentinel never mentioned again, or a resource used through method calls and dropped at
the end of its block.  That is a fact about a VARIABLE, which the compiler can see.  For
filehandles the same census was taken in s494: 80 % of everyday `open(my $fh, …)` sites
never let the handle escape.

perl's ten shapes, and what the design below gives (never EARLY, never MISSING):

| # | shape | perl | Stage 0+2 |
|---|---|---|---|
| 1 | `{ my $g = G->new; }` sentinel | at block exit | **same** |
| 2 | `{ my $g = G->new; $g->m; }` used as invocant | at block exit | **same** |
| 3 | `eval { my $g = G->new; die }` | during the unwind, before the code after the eval | **same** |
| 4 | `sub make { my $g = G->new; return $g }  { my $h = make(); }` | at the CALLER's block exit | **same** (return = transfer) |
| 5 | `$o = G->new("second")` overwriting the only holder | at the assignment | **same** when `$o` is a licensed owner |
| 6 | `undef $o` | at the `undef` | **same** when licensed |
| 7 | `push @keep, $g` then `@keep = ()` | when the array is cleared | LATE: at program exit |
| 8 | `G->new("temp");` never bound | at statement end | LATE: at program exit |
| 9 | loop body `my $g`, with `next` | per iteration | **same** |
| 10 | alive at the end of the program | global destruction | **same** (Stage 0) |

## 3. Measured: what a guard costs in SBCL

`~/pcl-agent-scratch/s495/destroy/guard-bench.lisp` (SBCL 2.6.0, quiet box, best of 5,
30 M iterations; the sub shape models PCL's emission: `catch :p-return` + special bindings —
PCL's `return` is ALREADY a `throw`, `next` is a lexical `go`):

| shape | ns / iteration |
|---|---|
| sub call, plain | 9.7 |
| + a fixnum/flag test at exit, NO `unwind-protect` | 9.6 (free) |
| + `unwind-protect` whose cleanup tests a flag | 14.0 (**+4.3**) |
| + `unwind-protect` whose cleanup calls out | 15.0 (+5.3) |
| `return` (throw), plain → crossing an `unwind-protect` | 14.3 → 17.5 (+3.2) |
| loop body with `go :next` on half the iterations, plain → guarded | 5.3 → 7.4 (+2.1) |

A real PCL sub call is 50–100 ns, a file `open` is microseconds.  So: **an
`unwind-protect` per guarded scope is affordable where the scope does I/O (Stage 1) and is
NOT free enough to put on every scope that might hold an object** — and it does not have
to be, because a test at the exits that already exist is free (§5).

## 4. Stage 0 — DESTROY at program exit (small, sound by construction)

perl destroys every surviving object at exit; PCL can do exactly that without knowing
anything about lifetimes.

* `bless` into a class for which `can("DESTROY")` finds a NON-EMPTY method pushes a weak
  pointer to the referent on a registry.  The per-class answer is cached beside the method
  cache and invalidated with it (a `sub DESTROY` defined later, an `@ISA` change).  Classes
  without DESTROY pay one cached flag test per `bless`.
* At exit — after the END blocks, which is perl's order — every registered object that is
  still alive and not yet destroyed gets its DESTROY, in reverse creation order (perl's
  order there is unspecified), each inside its own handler: a die in DESTROY is
  `(in cleanup)`-warned, never fatal.  A per-object `destroyed` mark makes a second call
  impossible (an explicit `$obj->DESTROY` does not set it — perl calls it again too).
* Buys today: temp files and directories removed, buffered compressors and `Tie::File`
  flushed, sockets closed politely, the "was my context released" checks.  One agent-day.
* A GC finalizer cannot do more: SBCL runs a finalizer AFTER the object is gone and it may
  not reference it, so `DESTROY($self)` has no `$self`.  Splitting every object into
  header + storage to allow resurrection would tax every object access — rejected.

## 5. Stage 2 — scope-owned objects: a static licence, an owner stack, escape marks

Three parts.  None of them is a reference count.

**(a) The licence (compile time)** — the sibling of Stage 1's `fh-scope-close` verdict, in
the same `Pl/VarAnnotator.pm` use-classification walk (rule 11: extend its vocabulary, no
second walker).  A lexical scalar is an OWNER CANDIDATE when every use of it in its scope is
NON-RETAINING: method invocant (`$g->m(...)`), dereference (`$g->{k}`, `@$g`), boolean /
`defined` / `ref` test, stringification, the handle slot of a core builtin, an ARGUMENT of
a call (the callee BORROWS it for the call's dynamic extent), and `return $g` (a TRANSFER:
the scope gives the value up without destroying it).  RETAINING uses void it: assignment
FROM it, `\$g`, an element of a list / constructor being stored, capture by a nested sub, a
string eval in scope.  DEFAULT DENY: an occurrence the classifier does not recognise is
retaining.  Parameters (`my $self = shift`, `my ($self, %a) = @_`) are never owners: their
value is held by the caller for the whole call — so **a method body gets no code at all**.

**(b) The owner stack (run time) — instead of `unwind-protect`.**  When an owner candidate
is bound or assigned a value that IS an eligible object (test gated by one global flag,
"does any class with a non-empty DESTROY exist" — false in almost every program), the
object's owner count goes up and the variable's box is pushed on a per-thread owner stack.
Release points are the exits that ALREADY exist, each reduced to a fixnum comparison of the
stack depth with the depth at entry (measured: free):

* after a sub's `catch :p-return` returns — normally or by `return`'s throw;
* at the top of each iteration and after the loop, for a loop whose BODY declares a
  candidate (so `next` / `last`, which are lexical `go`s, need no unwinding);
* at the end of a bare block / branch that declares one;
* in `eval`'s handler, down to the eval's mark — a `die` unwinding through three frames
  destroys their objects before the code after the eval runs (row 3);
* at `undef $v` and at an assignment that overwrites a candidate (rows 5, 6).

Releasing a box: owner count down; at zero, if the object is neither `escaped` nor
`destroyed` — call DESTROY (with `$@ $! $?` saved, perl's `local` set).  The return
expression is evaluated before the release by construction (the handle table's row 3).

**(c) Escape marks — what keeps it sound.**  An eligible object that becomes reachable
from the HEAP must never be scope-destroyed.  Every way a value gets there is a retaining
STORE, and the store primitives already split on the value's kind:
`%p-array-store-scalar`, `%p-aref-store`, `%p-gethash-store` and the scalar assignment all
have a fast arm for numbers and strings and a slow arm for references.  The mark goes in
the SLOW arm only, behind the global flag: "is this a reference to an eligible object →
set `escaped`".  Numbers and strings never see it.  The remaining retention routes are
static and get an explicit mark from the compiler: assignment to an UNLICENSED lexical, a
global, a closure-captured variable; a sub whose `@_` / `$_[N]` appears in a retaining
position marks all its arguments at entry; a string eval marks everything in scope.
ELIGIBILITY is granted at `bless` only when the operand is fresh (an anonymous
constructor, or a lexical of the same sub with no retaining use before the `bless`), and
only while tracking is on — an object blessed before its class had a DESTROY is Stage 0's.

Where the per-object state lives: for a blessed HASH (the common case) the class is already
a reserved key of the referent (`:__class__`), so `escaped` / `destroyed` / owner count are
one more reserved key, present only on objects of DESTROY classes; blessed scalars, arrays,
globs and code use a weak side table keyed by referent.

**Cost.**  A program with no non-empty DESTROY anywhere: one flag test at each candidate
binding and one fixnum comparison at each exit of a scope that declares a candidate;
nothing in method bodies; nothing on any numeric or string path.  A program WITH such a
class additionally pays, on each REFERENCE store, a class lookup on the referent — tens of
nanoseconds, only on reference stores, only in those programs.  No `unwind-protect`
anywhere in Stage 2.

**What stays unsupported** (each falls to Stage 0 — late, never early): an object whose
last holder is a container or another object (row 7), a temporary never bound to a
variable (row 8), anything the classifier could not prove (it is marked escaped).  Weak
references, `local`, tie, `@_`-alias tricks and string eval are all on the deny side.

**The risk, stated plainly:** a retention route that nobody marked would call DESTROY while
the object is still in use — a silent wrong of the worst kind.  The defence is the
project's usual one: a RETENTION BATTERY written first — every way Perl code can keep a
reference (some 60 shapes: each store form, slices, `map`/`grep` results, closures, `@_`
aliases, `local`, string eval, `wantarray`-dependent returns, sort blocks, overloads, tie),
each asserting with perl as the oracle that DESTROY has NOT run while the keeper is alive.
Stage 2 ships when that battery is green, and `PCL_OPT=-scope-destroy` turns it off.

**Work.**  Stage 0 ≈ one agent-day.  Stage 1 ≈ one agent-day (briefed).  Stage 2 ≈ four to
six agent-days: the verdict (shared walk), the release points in the sub / loop / block /
eval lowerings, the owner stack and marks in the runtime, `bless` eligibility, the battery,
`docs/ir-spec.md`.  Stage 2 also lets Stage 1 widen: a handle passed to a helper sub is
BORROWED, not escaped — the largest residue in the s494 census (4 of 7 escaping sites).

## 6. Order

Stage 1 (s494h, ready) and Stage 0 first: between them they end the fd leak, the unflushed
compressor and the temp files left behind, and neither can be wrong.  Stage 2 after the
retention battery exists.  `docs/not-supported.md` § "DESTROY called by garbage collector"
is rewritten stage by stage; its residue after Stage 2 is one sentence: *an object whose
last holder is a container, or that was never bound to a variable, is destroyed at program
exit rather than at its last reference.*
