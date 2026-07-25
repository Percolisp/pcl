# The XS artifact cache

How a shim-built XS module gets from an unpacked distribution to a running
`XSLoader::load` — and, because this is new ground, what we chose, what we
rejected, and **what would make us change it**.

Related: `pclxs/docs/decisions.md` D1 (the shim's side of the same
decision), `cl/pcl-xs.lisp` (the vtable adapter), `tools/pcl-xs-install`,
`Pl/t/xs-02.t`.

## The shape

```
unpacked dist ──tools/pcl-xs-install──▶  ~/.pcl-cache/xs/abi-3/auto/Digest/MD5/MD5.so
                (runs pclxs's xs-build)                    │
                                                           │
     use Digest::MD5;  ──▶  XSLoader::load('Digest::MD5') ─┘
                              └─ %p-xs-artifact-path derives that path,
                                 loads cl/pcl-xs.lisp on demand, boots it
```

Three pieces, each in one place:

| piece | where | job |
|---|---|---|
| `tools/pcl-xs-install` | Perl | build a dist into the cache at the ABI-keyed path |
| `%p-xs-artifact-path` etc. | `cl/pcl-runtime.lisp` | derive the *same* path from a module name |
| `XSLoader::pl-load` | `cl/pcl-runtime.lisp` | boot it, or fail exactly as perl does |

## Decision 1 — compile at install time, not at first use

**Chosen:** the compile happens when you install a distribution.

*Why.* It is what perl does, so CPAN's own expectations hold; latency is
predictable and lands where a user expects to wait; and the unpacked
distribution is still on disk, which it is **not** at runtime — perl
installs the `.so` and the `.pm` and throws the build directory away.

*Rejected: compile lazily on first `use`.* Attractive for a transpiler
(the workflow is already "run it and see"), but it pays cc latency inside
someone's program, needs a build-to-temp-then-`rename()` dance so two
processes racing on the same module cannot load a half-written object, and
requires keeping build trees around forever.

*What would change it.* If `pcl-xs-install` turns out to be a step people
routinely forget — which we will see as "why does this module say
Can't locate loadable object" — a lazy fallback becomes worth its
complexity. It slots in at `%p-xs-try-load` without disturbing anything
else: that function returns NIL for "nothing to load", and a lazy build
would simply try harder before giving up.

## Decision 2 — the cache key is the pclxs ABI, encoded in the path

**Chosen:** `<cache>/abi-<N>/auto/Foo/Bar/Bar.so`, N from `xs-pin`.

*Why the ABI.* An artifact built against ABI 2 is not merely old — it was
compiled against a **different vtable**. Loading it under ABI 3 is
undefined behaviour that would surface as a crash inside the module, miles
from the cause.

*Why in the path rather than as a check.* A check is something that has to
be remembered, written, and kept correct. A path that does not exist
cannot be loaded by anybody, ever, including code we have not written yet.
After an ABI bump the old artifacts are simply invisible; nothing has to
notice they are stale. (`tools/pcl-xs-install --clean` deletes them, but
only to reclaim disk — correctness does not depend on it.)

*Why `xs-pin` and not the loaded library.* The path must be computed
**before** deciding whether to load `libpclxs` at all — a PCL program that
touches no XS must not need the shim built. `tools/build-pclxs` already
refuses to build when pin and checkout disagree, and `pclxs_init` rejects a
vtable whose `abi_version` does not match, so a lying pin is caught twice
downstream. `pcl-xs-install` additionally cross-checks the pin against
`xs-build --print-abi` before writing anything.

*What would change it.* If we ever load an artifact we did not build — a
shared or distributed binary cache — the ABI would have to be discoverable
*from the artifact*, and path-encoding alone stops being enough.

## Decision 3 — architecture is handled by location, not by a path segment

**Chosen:** no archname in the key. The cache lives under `$HOME`, which
is per-machine in the ordinary case; `$PCL_XS_CACHE` overrides it.

*Why.* Two derivations of "the architecture" — one in Perl
(`$Config{archname}`) and one in Lisp (`machine-type`/`software-type`) —
would have to agree exactly, forever, or the installer writes where the
loader does not look. That is a subtle, silent, cross-language coupling to
buy protection against one specific situation: a home directory shared
between machines of different architectures.

*Rejected: `abi-3/x86_64-linux/…`.* Correct, and what pclxs's design doc
originally implied — but it needs the two derivations above to be identical
strings. If we take it up later, the way to do it without the coupling is
for **one** side to compute the key and the other to be told (e.g. the
installer writes the segment name into a file the loader reads).

*What would change it.* A shared `$HOME` across architectures being a real
setup for a real user, rather than a hypothetical. Until then the override
is a one-line answer and the coupling is not worth its cost.

## Decision 4 — a missing artifact must fail exactly like perl

**Chosen:** `Can't locate loadable object for module Foo in @INC`, verbatim.

*Why.* Every dual-life module on CPAN is written as

```perl
eval { require XSLoader; XSLoader::load(__PACKAGE__, $VERSION); 1 }
    or $Useperl = 1;
```

and falls back to a pure-Perl implementation on exactly that failure. A
more helpful message ("no artifact; run pcl-xs-install") would be *worse*:
it would still be caught by the `eval`, so nobody would read it, and any
change to the shape of the failure risks a module taking the XS branch and
then calling a sub that does not exist. `Pl/t/xs-02.t` pins the message.

*What would change it.* Nothing, short of perl changing it.

## What is deliberately not here yet

- **`.pm` installation.** A distribution's Perl side is ordinary Perl and
  PCL transpiles it like any other module, so it belongs wherever `@INC`
  points. `pcl-xs-install` lists what it saw and leaves the placement to
  you; automating it means deciding PCL's module-install story, which is a
  bigger question than the XS bridge.
- **Dependency tracking.** Rebuild after editing a dist's `.xs` is
  currently "run the installer again". A source digest beside the artifact
  is the obvious next step, and only matters once lazy building exists.
- **`DESTROY`.** Refcount→0 in the shim drops the handle without calling a
  destructor, so C objects behind a T_PTROBJ blessed ref leak. That is a
  pclxs-side contract question, not a cache question, but it is the thing
  to fix before anyone runs an XS module in a long-lived image.
