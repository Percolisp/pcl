# pclxs ABI 5 and DESTROY — what PCL has to do, and what it costs

*Written from the pclxs side, for whoever next works in PCL. Nothing here
is done yet in this repo: this is the work list and the reasoning behind
it, so the decisions can be argued with rather than reverse-engineered.*

**Status in this checkout:** `xs-pin` has been moved to **abi 5**, and
**not one line of `cl/pcl-xs.lisp` changed**. `Pl/t/xs-01.t` (6/6) and
`Pl/t/xs-02.t` (4/4) are green. That is the whole PCL-side change so far,
and the fact that it *is* the whole change is the point of §1.

---

## 1. Nothing is broken. That is the design.

ABI 5 adds eight vtable entries for filehandles. In every previous bump
(2→3, 3→4) a new entry was **mandatory**: `pclxs_init` refuses a table
with a hole, so PCL had to implement the new callback in the same session
or `use Digest::MD5` stopped working at boot.

ABI 5 is the first bump where that is not true. The eight `io_*` entries
are an **optional capability group**:

- a host that implements **none** of them is accepted, and
  `pclxs_capabilities()` reports 0;
- a host that implements **all** of them gets `PCLXS_CAP_IO`;
- a host that implements **some** of them is refused by name at
  `pclxs_init`, exactly like a missing core entry.

So PCL's current adapter — which sets no `io_*` entry — still boots, still
runs Digest::MD5, still passes everything it passed yesterday. The only
change is that XS which touches a filehandle now croaks with

    pclxs: this host does not implement filehandles
    (vtable capability group 'io' is absent)

instead of the older *"PerlIO support is Tier 2"*. Both are catchable Perl
errors; only the wording moved.

**Rationale is in pclxs `docs/decisions.md` D4.** Short version: 49 of 79
surveyed distributions never touch a filehandle, and the ones that do
usually touch it in one function you have to ask for (Digest::MD5 hashes
strings all day and only reaches `addfile` if you call it). Gating all of
XS on a runtime's IO story would be the wrong trade.

### Moving the pin is still required, and it is the *only* required step

"Nothing is broken" means the *adapter* needs no change. The **pin guard**
is a different thing and it did fire, correctly:

    pcl-xs-install: xs-pin says abi 4 but /home/bernt/pclxs is abi 5.
      Run tools/build-pclxs --pin-here after checking the adapter.

`Pl/t/xs-01.t` passed throughout (it builds a module against the current
pclxs and runs it), while `Pl/t/xs-02.t` failed on all four cases — the
cache path refusing to install a mismatched artifact. That split is worth
remembering when this happens again at ABI 6: **an ABI bump can leave the
adapter working and the install tooling refusing**, and the refusal is a
feature. The artifact cache is keyed on ABI in the path
(`~/.pcl-cache/xs/abi-N/`), so a stale `.so` built against older headers
can never be loaded against newer ones.

    tools/build-pclxs --pin-here

Done. The `io` group stays empty and `PCLXS_CAP_IO` stays off. The old
`~/.pcl-cache/xs/abi-4/` tree is harmless — nothing looks in it any more.

---

## 2. If and when PCL implements the `io` group

Eight callbacks. All-or-nothing — set one and `pclxs_init` will demand
the rest, by name, at boot.

| entry | signature | what PCL must answer |
|---|---|---|
| `io_from_sv` | `(sv, want_write) -> handle` | given a p-box that is a filehandle (a glob, a ref to one, a lexical handle), the host stream. **0 is a legal answer** meaning "not a filehandle" — it is how `defined fileno($x)` gets decided, not an error. |
| `io_std` | `(which) -> handle` | `PS_IO_STDIN` / `STDOUT` / `STDERR`. 0 if the stream does not exist. |
| `io_read` | `(io, buf, n, *got) -> status` | read up to `n` bytes. **A short read is not an error.** `PS_DIED` only if a *Perl-level* die happened (a tied handle); an ordinary IO failure is `PS_OK` with the error bit set below. |
| `io_write` | `(io, buf, n, *put) -> status` | same contract, writing. |
| `io_state` | `(io) -> uint32` | `PS_IO_EOF \| PS_IO_ERROR \| PS_IO_OPEN \| PS_IO_UTF8`. |
| `io_flush` | `(io) -> status` | |
| `io_clearerr` | `(io) -> void` | |
| `io_fileno` | `(io) -> int` | the OS descriptor, or **-1** when there is none. -1 is a fully supported answer — it is what perl says for an in-memory handle, so callers already cope. |

Two contract points that are easy to get wrong, both because they are
places where "sensible" and "what perl does" differ:

**Bytes, not characters.** `io_read` fills a byte buffer. If PCL's stream
is character-based, the adapter has to read through a byte view
(`flexi-streams`, or an element-type `(unsigned-byte 8)` stream). A host
that decodes on the way through fails `t/94-io.t`'s binary case
immediately — that case reads all 256 byte values and compares hex.

**A short read is not an error, and EOF is not latched until a read has
actually hit it.** `t/94-io.t` has one case that pins all three answers in
order (`short_read`) precisely because getting this wrong truncates every
file the module ever reads, silently.

### The `want_write` argument, and why it exists

perl's IO object holds *two* streams and the standard typemap uses both:
`InputStream` is `IoIFP(sv_2io($arg))`, `OutputStream` is `IoOFP(...)`.
refhost ignores `want_write` because a `FILE *` opened `r+` does both.
**PCL's streams are directional**, so PCL must not: answering the read
stream for a write request would send output to the input end and lose it
without an error.

---

## 3. DESTROY — this one PCL *should* do, and it needs no ABI bump

12 of 79 surveyed dists define a `DESTROY` XSUB (Compress-Raw-*,
Digest-*, DB_File, IO-Compress-Brotli, Unicode-LineBreak). They are all
the same shape: a Perl object holds a pointer to a `malloc`'d C struct,
and `DESTROY` is the only thing that frees it.

Today PCL never calls it. In a script that means a bounded leak nobody
notices. **In a long-lived PCL image it means every Digest::SHA object,
every zlib stream, every DB_File handle leaks its C side forever.** That
is a much more serious gap than PerlIO, and it is why this is the item
worth doing first.

### The design point

The shim does *not* decide when an object died — PCL does, because PCL
owns the objects and the GC. So `DESTROY` is reached through an **exported
entry point PCL calls**, not through the vtable:

```c
int          pclxs_has_destroy(pclxs_ctx *ctx, const char *cls, size_t len);
pclxs_status pclxs_run_destroy(pclxs_ctx *ctx, pclxs_handle obj,
                               const char *cls, size_t len);
```

Because it is host → shim rather than shim → host, **it costs no ABI
bump**. It works against ABI 4 as well as 5, so PCL can implement this
without touching the pin at all.

`pclxs_run_destroy` never fails and never lets a die escape: a die inside
a destructor becomes a warning (`(in cleanup) ...`), exactly as in perl,
because unwinding out of a collector would tear down a frame the host is
in the middle of. That is the same rule as `with-xs-guard`, applied one
level further out.

### What PCL has to supply

A finalizer on the p-box that wraps a **blessed reference built by the
bridge**, which calls `pclxs_run_destroy` with a fresh reference to the
object and its class name. SBCL's `sb-ext:finalize` is the obvious
mechanism.

Three properties the finalizer must have — the first two were each a bug
in refhost before the conformance test caught them:

1. **The destructor is passed a REFERENCE, not the referent.** The class
   lives on the referent (that is where `SvSTASH` lives, and where
   `p-bless` records it), but `DESTROY` receives the reference — every
   T_PTROBJ destructor starts with `SvRV(self)`. Passing the referent
   makes that return undef and the module dereferences a null pointer.
   Passing the *reference* while reading the class off the *reference*
   finds no class and silently does nothing at all. It has to be: class
   from the referent, argument a reference to it.

2. **Exactly once.** A destructor that runs twice frees twice.

3. **Not from inside another destructor.** Running a destructor calls back
   into an XSUB, which will release handles of its own; finalizing from
   inside a finalizer is how a host ends up running a destructor on a
   half-freed object. Queue and drain, do not recurse.

### Interaction with PCL bug #99

pclxs `docs/xs-blessed-ref-referent-bug.md` (in this repo) records that
PCL puts the class on the *wrapper* rather than the referent for
scalar refs. **That bug and this feature are the same question**, so fix
them together: once the class is on the referent, the finalizer knows what
class to look up, and `t/70-aggregate.t`'s one remaining conformance
failure goes away at the same time.

---

## 4. Performance — read this part

The user asked specifically. Three items, in descending order of how much
they will matter to PCL.

### 4.1 `pclxs_has_destroy` must be cached per class, by PCL

This is the one that can actually hurt. `pclxs_run_destroy` does a
**method lookup per dying object** — it has to, because `DESTROY` is
inherited and only the host knows the current `@ISA`. If PCL calls it from
a finalizer on every blessed p-box, every GC'd object costs a method
resolution through the bridge.

The entry point `pclxs_has_destroy(ctx, cls, len)` exists for exactly this:
ask **once per class**, cache the boolean in a hash keyed by class name,
and only install a finalizer at all for classes where the answer is yes.
Invalidate the cache when a sub is installed (PCL already tracks that for
`%pcl-cl-sub-name`).

Done that way the cost is: one method lookup per *class* ever seen, and
zero per object that has no destructor — which is almost all of them.
Done naively it is one bridge crossing per object finalized, forever.

### 4.2 `PerlIO_getc` / `PerlIO_putc` are one host crossing per byte

The shim builds them on `io_read`/`io_write` with a one-byte buffer. perl
implements them as buffered macros that touch memory. So byte-at-a-time IO
through the bridge is **hundreds of times slower than perl's**, not a few
percent.

Nothing in the currently-building set does this. Storable does (2 `getc`,
1 `putc`) and Storable is still `api-gap` for other reasons, so this is a
cost that has not been paid yet. If Storable or another byte-at-a-time
consumer starts building, the fix is a small read buffer inside the shim's
`struct pclxs_io` — a shim-side change, nothing for PCL to do. Block
readers (`addfile`, which reads 4–8 KB at a time) are unaffected: one
crossing per block, amortised to nothing.

### 4.3 What was already fixed, so it does not become PCL's problem

Two costs were designed out during the ABI 5 work rather than left for a
profiler to find later — both would have hit PCL specifically, and neither
would have failed a test:

- **Wrapper accumulation.** A `PerlIO *` is a shim-side wrapper
  deduplicated by *host handle*. refhost's handles are stable, so one
  wrapper per stream. **PCL's are not** — a host with a moving GC cannot
  hand out addresses (that is why handles are table indices at all), so
  PCL mints a fresh handle on every `io_from_sv`, which would have meant a
  fresh wrapper per call on a list scanned linearly at each resolution:
  quadratic time and unbounded memory in a long-lived image. Wrappers are
  now released when the XSUB frame that resolved them pops.

- **`pclxs_capabilities()`** walked the 60-entry vtable field table on
  every call, and it is called on every `sv_2io`. Computed once at init
  now.

The general lesson, worth carrying into the PCL adapter: *the reference
host's handles are stable and PCL's are not*, so any shim-side cache keyed
on a host handle is O(1) in refhost and unbounded in PCL. That asymmetry
will not show up in the conformance suite, because the suite checks
answers, not costs.

---

## 5. Order of work, suggested

1. **Fix #99** (class on the referent, not the wrapper) — one bug, and it
   clears the last conformance failure.
2. **DESTROY finalizer**, with the per-class cache from §4.1. No ABI bump,
   biggest real win for a long-lived image.
3. **Move the pin to 5** whenever convenient. Nothing depends on it.
4. **The `io` group**, only if a filehandle-using module is actually
   wanted — `Digest::MD5::addfile`, `Digest::SHA`, `Sys::Hostname`. The
   conformance cases are `t/94-io.t` in pclxs and they run against PCL
   automatically through `$PCLXS_HOST_DEFS`.

---

*Cross-references: pclxs `docs/decisions.md` D4 (capability groups) and D5
(DESTROY as an entry point); pclxs `docs/porting-a-host.md`; pclxs
`t/94-io.t` and `t/93-destroy.t` for the exact behaviour required;
`census/IO.tsv` in pclxs for the measurement all of it rests on.*
