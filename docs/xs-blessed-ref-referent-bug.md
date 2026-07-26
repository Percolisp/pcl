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

---

## UNBLOCKED: the pclxs side shipped (pclxs session 10, ABI 6)

*2026-07-26, written from the pclxs side. Nothing here needs doing
urgently — PCL keeps working exactly as it does today until you want the
OO half of magic-carrying XS modules.*

**Nothing is broken by the bump.** `PCLXS_ABI_VERSION` is 6, and that is a
non-event for this adapter: `cl/pcl-xs.lisp` builds the table by NAME
(`pclxs_vtable_new` + `pclxs_vtable_set`), and `pclxs_vtable_new` stamps
the version from the library itself. An optional group nobody sets is
simply off (`pclxs_vtable_check` only enforces a group the host has
started). So: rebuild libpclxs, relink, carry on. Module artifacts are
ABI-keyed by name, so a stale ABI-5 `.so` cannot be found by an ABI-6
library — you may need to rebuild built modules, which the loader already
does on a miss.

**What was wrong, in one line each.** Two bugs wearing one symptom, both
now fixed in pclxs:

1. The magic chain hung off the *shim SV struct*, and no deref preserves
   that struct (`SvRV` mints a fresh proxy per call). It now lives with
   the host object, via a new optional capability group.
2. `newRV_noinc` DROPPED the caller's shim-side reference instead of
   taking it over, so `SV *sv = newSV(0); newRV_noinc(sv); sv_magicext(sv,
   ...)` — Digest::MD5's constructor, verbatim — wrote the module's C
   pointer into a freed cell. Now mortalized.

**What PCL can do, when it wants Digest::MD5's OO path to work** — two
callbacks, added to `*xs-callbacks*` like any other:

```
    ("magic_set"         . xs-magic-set)     ; (handle, void*) -> void
    ("magic_get"         . xs-magic-get)     ; (handle) -> void*
```

Store **one word per scalar object**, opaque to PCL — a slot on the box,
or a weak `eq` table keyed on it. The rules, and they matter:

- Hand back exactly what you were given; `magic_get` on an object nobody
  set is NULL (0).
- Never free it, never follow it. The pointer is pclxs's chain head.
- **Never copy it when a value is assigned somewhere else.** Perl attaches
  magic to the SV, not to the value; copying would give two objects one C
  struct and the module would corrupt its own state. This is the mirror
  image of the `box-set` rule you already worked out for blessing (a class
  is copied only when the assigned value is itself a reference) — here the
  answer is simpler: never.
- The word must live exactly as long as the object does. The referent box
  is the right home for a blessed scalar ref, which is what task #99 made
  the source of truth anyway.

Both entries or neither: it is an all-or-nothing group, and `pclxs_init`
refuses half of one by name.

**What it buys.** Every XS module that keeps C state as magic — Digest::MD5
(`MD5_CTX`), Digest::SHA, HTML::Parser, and the usual alternative to
`T_PTROBJ`. Without the group, those modules' objects lose their state the
first time they are used as objects; with it, `Digest::MD5->new->add(...)
->hexdigest` is reachable, which is also what unblocks running that dist's
OWN test suite under PCL — the strongest oracle either project has.

**What it does NOT fix, on any host.** The module's magic `free` hook still
never runs: pclxs cannot run module code from a collector without breaking
rule O4. C state attached this way is reclaimed when the process is. That
is the same standing question as DESTROY (`docs/xs-abi5-and-destroy.md`),
and the two will be answered together or not at all.

**Reference, if you want to read rather than take this on trust**: pclxs
`docs/porting-a-host.md` §"The `magic` group" (host-neutral instructions),
`docs/decisions.md` D10 (why, and the alternatives rejected), and
`t/85-magic.t` (the case, which is Digest::MD5's exact shape and is green
against real perl).
