# XS OO blocker: ext-magic does not survive the trip through the host

**Status:** open, root cause found (session 314). Blocks the OO half of
XS modules that keep their C state in MAGIC — Digest::MD5 included.
The fix needs a pclxs vtable addition first (their side), then two small
callbacks here (our side).

**History.** This file used to claim "the referent of a bridge-built
blessed scalar ref is unreadable (`$$obj` is undef)". That diagnosis was
WRONG on both counts, and the reproducer's expected output was wrong too:

```perl
use Digest::MD5;
my $o = Digest::MD5->new;
print ref($o), "\n";                          # Digest::MD5   ✔
print defined($$o) ? "ok" : "undef", "\n";    # undef — and real perl
                                              # says undef TOO
```

`$$o` is undef under real perl, because Digest::MD5 2.59 does not store
its C pointer as an IV in the referent at all. `new_md5_ctx` (MD5.xs):

```c
SV *sv  = newSV(0);                 /* referent: UNDEF, and stays undef */
SV *obj = newRV_noinc(sv);
sv_bless(obj, gv_stashpv(klass, 0));
sv_magicext(sv, NULL, PERL_MAGIC_ext, &vtbl_md5, (char *)context, 0);
```

and `get_md5_ctx` reads it back by walking `SvMAGIC(SvRV(sv))` for the
entry whose `mg_virtual == &vtbl_md5`. The pointer travels as **ext-magic
on the referent**, not as a value. (The old text quoted an
`SvIOK`/`SvIV` version of `get_md5_ctx` — that is not what 2.59 ships.)

So PCL's reference representation is fine: the wrapper box crosses the
boundary blessed, is-ref, with its inner box intact (verified by
instrumenting `xs-collect-result`), and the pure-Perl analogue
(`bless \$x` then `$$r`) always worked.

## The actual bug

pclxs magic (`src/magic.c`) hangs the chain on the **shim SV struct**
(`sv->magic`). Two lifetimes conspire against it:

1. The referent shim SV is mortal: `newSV(0)` is owned by the RV, the RV
   is `sv_2mortal`'d, and FREETMPS at the end of the `new` XSUB frees
   both structs. Only the referent's **host identity** (the box exported
   before `newRV`) survives, held alive by the wrapper the caller got.
2. When `->add` later derefs the RV, `pclxs_sv_rv` wraps the host
   referent in a **fresh** proxy (`mortal_proxy` → `pclxs_sv_import` —
   a new struct every crossing). Its magic chain is empty, so
   `get_md5_ctx` croaks **"Failed to get MD5_CTX pointer"**.

This is host-independent: refhost cannot preserve it either. pclxs's own
suite passes because no case attaches magic in one XSUB call and reads it
back in a later one — the mechanism is only ever tested within a frame
(an R28-shaped gap on their side).

## The fix, split by repo

The only identity that survives is the host object (here: the p-box), so
the magic chain must be keyed on it.

- **pclxs:** a vtable pair in the spirit of the existing design ("ask the
  host"), e.g. `magic_set(h, void*)` / `magic_get(h) → void*` on
  scalars: the shim stores its own `struct pclxs_magic*` chain head
  there, and `pclxs_sv_import`/`sv_magicext`/`mg_find` consult it. An
  optional capability group like ABI 5's `io_*` would let other hosts
  opt out. **Interlocks with two of their open items:** the MGVTBL
  `free` callback (HTML::Parser) and DESTROY — when the host box dies,
  magic free hooks must run, which is the same finalizer question as
  DESTROY (`docs/xs-abi5-and-destroy.md`).
- **PCL (this repo):** implement the two callbacks — a slot or weak
  `eq`-hash keyed on the referent box, storing an opaque integer the
  shim owns. The referent box's lifetime is the object's lifetime, which
  is exactly perl's contract for magic.

## The neighbour — FIXED (s314, task #99)

PCL used to record a scalar ref's blessing on the wrapper box only.
Perl blesses the referent, and the difference was observable in pure
Perl, not just XS: a second `\$x` wrapper never saw the bless, a
re-bless through one alias was invisible through another, and XS asking
`SvSTASH(SvRV(rv))` found nothing (the `bless_and_class` conformance
failure).

Now the **referent box is the source of truth**: `p-bless` writes the
class there (`%p-scalar-ref-referent` navigates wrapper-or-variable to
the referent), `p-ref`/`p-get-class` consult it first
(`%p-referent-class` — it declines unless the referent holds a plain
scalar, so the REF/ARRAY/REGEXP arms keep winning), and the
wrapper/variable slots remain as caches for the fast "is this an
object" checks. The flip side went in with it: `box-set` copies a class
only when the assigned value is itself a reference — copying a plain
value out of a blessed referent yields an unblessed scalar, because
perl's stash is attached to the SV, not the value.

Guard: `Pl/t/bless-referent-01.t` (six behaviors, perl-verified).
Conformance: **366/366** — the corpus is fully green.

This mirrors how hash objects always worked (class in the hash itself,
`:__class__`), which is why they never had these bugs.
