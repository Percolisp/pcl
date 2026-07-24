# XS Shim Design — running CPAN XS/C extensions on PCL (and other hosts)

**Status:** Design complete, ready for implementation.
**Project structure / GitHub:** the shim is built as a **standalone public
repo (`pclxs`)** with PCL keeping only the host adapter — repo layout,
bootstrap steps, CI, and the per-phase two-repo split are in
`docs/xs-shim-repo-plan.md` (2026-07-12). Read that before Phase 0.
**Implementer:** Opus 4.8 — this document is written to be executed phase by
phase; every phase has an acceptance test. Read §2 (how XS really works) and
§4 (the architecture decision) before writing any code.
**Supersedes:** the sketch in `XS_BRIDGE_DESIGN.md` (kept for history; its
"wrap CL objects in C structs" approach is rejected here — see §4.2).
**Superseded ON STRUCTURE by `docs/xs-shim-repo-plan.md` (2026-07-12) and,
since 2026-07-24, by the pclxs repo itself** (sibling checkout `../pclxs`,
`docs/design.md` there). Three places in this document still read as if
everything lived under `pcl/`; the reasoning is unaffected, the layout is
not:
- **Paths** (§6, §9, §10.1, §13): `xs/include/pclxs/` → `include/pclxs/`,
  `xs/src/` → `src/`, `xs/t/` → `t/`, `xs/census/` → `census/`, all at the
  pclxs repo root.
- **The build tool** (§9) is two tools: host-neutral `tools/xs-build` in
  pclxs, and a thin `tools/pcl-xs-build` wrapper here that adds step 4
  (transpiling the dist's `.pm` files).
- **Artifact naming** (§9 step 3): `blib-pcl/…/Bar.pcl.so` is a *pcl*
  choice, passed to the neutral tool as `--suffix`/`--out`; it is never a
  default inside pclxs.
Also folded in: `pclxs/xs-shim-prereview.md` (advisory review, 2026-07-24)
and its findings — the boot handshake is a deliberate no-op, `ppport.h`
needs six documented concessions from our `perl.h`, and perl's internal
types must be named but never defined.
**Related:** `docs/ir-spec.md` (the semantic contract every host callback must
honor), `docs/extensions.md` (how the CL side gets loaded),
`docs/shipped-modules.md` (module resolution), `docs/not-supported.md`
(§DynaLoader — the entry this project deletes).

---

## 0. Goal and non-goals

**Goal:** a CPAN distribution containing `.xs`/C code can be built once
against PCL's shim and its compiled `.so` loaded into the running SBCL image,
so that transpiled Perl code calls the XS subs exactly as it would under perl.

**Portability goal (explicit user requirement):** the C side must be
**host-language-neutral**. The shim talks to its host exclusively through a
versioned vtable of ~60 C function pointers (`pclxs_host.h`). PCL/SBCL is
the first host; a Python, Ruby, or JS host could implement the same vtable and
run the same recompiled XS modules. No CL type, symbol, or assumption may leak
below the vtable.

**Non-goals (v1):**
- Binary compatibility with `.so` files compiled against real perl (§4.1 —
  impossible without replicating SV struct layout; we recompile from source).
- Perl threads / `MULTIPLICITY` (one interpreter context; the context pointer
  is threaded through anyway so this is not painted into a corner).
- Modules that poke perl internals past the documented API (`Devel::*`,
  `threads`, `re`, `B`) — classified Tier X in §10.
- `@_` element aliasing through XS (PCL already doesn't alias `@_`;
  `docs/not-supported.md`).

### 0.1 Where the bridge sits among module providers

The shim does **not** replace the existing provider kinds
(`docs/shipped-modules.md`); it adds one. Per module, the decision order is:

1. **Native CL / pure-Perl `lib/` shim exists and is good** → keep it
   (List::Util, POSIX stub, Scalar::Util…). The bridge is not a reason to
   delete working shims; some XS (List::Util's `MULTICALL`) is Tier X
   anyway, and a native implementation is usually faster than crossing the
   vtable.
2. **Dual-life module with a pure-Perl fallback** (Data::Dumper, JSON::PP
   path…) → nothing to do; the `XSLoader::load` die keeps triggering the
   fallback until someone builds the XS side, after which the XS side wins —
   both routes stay correct.
3. **XS-only module, uses the documented API** → shim-XS build (this
   design).
4. **XS-only, pokes internals (Tier X)** → hand shim in `lib/` or declare
   not-supported, exactly as today.

So yes: `lib/` shims remain a permanent, first-class provider; the bridge
only shrinks the set of modules that *require* hand-shimming.

**Prior art to keep in mind** (this is a well-trodden problem class):
- **HPy** (Python): handle-based C API designed so PyPy/GraalPython can run C
  extensions. Key lessons we adopt: *handles, not pointers* (GC can move
  objects); *explicit context argument* (perl already has one: `pTHX`);
  *recompile from source against new headers* rather than emulate the old ABI.
- **PyPy cpyext** (the cautionary tale): binary-emulating CPython's
  pointer-based API cost years and is still slow. We do not attempt the
  analogous emulation of perl's SV memory layout.
- **JNI**: the `JNIEnv*` first-argument-is-a-vtable pattern is exactly the
  shape of our `pclxs_ctx`.

---

## 1. What already exists in PCL (integration points)

- **Single-function FFI precedent:** `crypt()` is bridged with
  `sb-alien:define-alien-routine` straight to `libcrypt.so.1`
  (`cl/pcl-runtime.lisp` ~line 2482). That pattern stays the right answer for
  *one stable libc symbol*; the XS shim is for *whole CPAN modules*.
- **XSLoader/DynaLoader stubs** (`cl/pcl-runtime.lisp` ~line 13186):
  `XSLoader::load` currently dies with "Can't locate loadable object…" so
  dual-life modules fall back to pure Perl. The shim's loader (§7.4) slots in
  *in front of* this die: if a shim-compiled `.so` exists for the module, load
  it; otherwise keep the die (the fallback behaviour is load-bearing —
  Data::Dumper etc. depend on it).
- **Extension loading:** the CL host side (`cl/pcl-xs.lisp`) loads via
  `p-load-extension` exactly like `pcl-pack` (`docs/extensions.md`), with a
  self-loading stub triggered by the first `XSLoader::load` that finds a
  compiled module.
- **The data model the host maps onto** (`docs/ir-spec.md` §2): scalars are
  `p-box` cells (value + cached nv/sv + class + is-ref), undef is `:undef`
  (nil = array hole), arrays are adjustable vectors of boxes, hashes are
  `equal` hash tables with stringified keys, references are `is-ref` boxes,
  code refs are functions. Every vtable callback in §5 is defined in terms of
  this model.

---

## 2. How XS actually works (read this before designing anything)

An implementer who has not internalized this section will re-invent perl
badly. This is the ground truth the shim must reproduce.

### 2.1 The pipeline

```
Foo.xs ──xsubpp──▶ Foo.c ──cc -I$PERL/CORE──▶ Foo.so ──DynaLoader──▶ dlopen
                                                        + dlsym("boot_Foo")
                                                        + call boot_Foo()
```

- **`.xs` files** are C with a declarative sub-definition syntax on top
  (`MODULE = Foo  PACKAGE = Foo`, then per-sub: signature, `PREINIT:`,
  `CODE:`/`PPCODE:`, `OUTPUT:` sections).
- **`xsubpp`** (module `ExtUtils::ParseXS`, **pure Perl** — this matters: we
  can run it under the system perl, no C needed) translates each XS sub into a
  C function with a fixed signature and generates argument/return marshalling
  from **typemaps**.
- The generated `.c` `#include`s three headers — `EXTERN.h`, `perl.h`,
  `XSUB.h` — and uses only *macros and functions from those headers* for all
  perl interaction. **This is the entire attack surface we must reimplement.**
- **`boot_Foo__Bar`** (`::` → `__`) is the only exported entry point. It calls
  `newXS("Foo::Bar::subname", XS_Foo__Bar_subname, file)` once per XS sub,
  registering C function pointers under Perl sub names.
- **`XSLoader::load('Foo::Bar')`** at runtime finds
  `auto/Foo/Bar/Bar.so` in `@INC`, dlopens it, dlsyms the boot function, and
  calls it as an XSUB.

### 2.2 Anatomy of a generated XSUB

```c
XS_EUPXS(XS_Foo_add)          /* == void XS_Foo_add(pTHX_ CV* cv) */
{
    dVAR; dXSARGS;             /* SP, MARK, ax, items — the arg stack frame */
    if (items != 2)
        croak_xs_usage(cv, "a, b");
    {
        IV a = (IV)SvIV(ST(0));    /* typemap T_IV INPUT */
        IV b = (IV)SvIV(ST(1));
        IV RETVAL;
        dXSTARG;
        RETVAL = add(a, b);        /* the author's C code */
        XSprePUSH; PUSHi((IV)RETVAL);  /* typemap T_IV OUTPUT */
    }
    XSRETURN(1);
}
```

The macro layer the shim's headers must provide, with real-perl semantics:

| macro / symbol | expands to (real perl) | shim obligation |
|---|---|---|
| `dXSARGS` | `dSP; dAXMARK; dITEMS` — local `SP`, `ax` = popped mark, `items = SP - MARK` | same, over the **shim's own stack** (§6.3) |
| `ST(i)` | `PL_stack_base[ax + i]` | same |
| `XSRETURN(n)` / `XSRETURN_IV/NV/PV/UNDEF/YES/NO/EMPTY` | set `PL_stack_sp = PL_stack_base + ax + n - 1; return` | same |
| `EXTEND(SP,n)`, `PUSHs`, `XPUSHs`, `mPUSHi`, `PUSHi/n/p/u`, `dXSTARG` | stack ops (m* = mortal, TARG = per-sub scratch SV) | same; TARG can be a fresh mortal each call (perf note §11) |
| `items`, `ax` | arg count / base index | same |
| `GIMME_V` | `G_VOID / G_SCALAR / G_LIST` from caller context | wired from host's `*wantarray*` (§6.4) |
| `croak(fmt, ...)`, `croak_xs_usage` | printf + longjmp to enclosing `JMPENV` | printf + longjmp to the shim's per-XSUB-call `setjmp` (§6.6) |
| `ENTER/LEAVE`, `SAVETMPS/FREETMPS` | scope + mortal-stack bookkeeping | shim-side scope stack |
| `sv_2mortal(sv)`, `sv_newmortal()` | push on tmps stack, freed at `FREETMPS` | same, over shim tmps stack |

### 2.3 The SV API the generated code and hand-written XS use

Grouped by how often they appear (a census procedure to make this empirical
is in §10.1):

**Creation:** `newSV(len)`, `newSViv`, `newSVuv`, `newSVnv`, `newSVpv`,
`newSVpvn`, `newSVpvf`, `newSVsv`, `newSV_type`, `newRV_inc`, `newRV_noinc`,
`newAV`, `newHV`, `newSVrv`.

**Reading:** `SvIV`, `SvUV`, `SvNV`, `SvPV(sv,len)`, `SvPV_nolen`,
`SvPVbyte`, `SvPVutf8`, `SvTRUE`, `SvOK`, `SvIOK/SvNOK/SvPOK` (type-cache
flags), `SvROK`, `SvRV`, `SvTYPE`, `SvCUR`, `SvLEN`, `SvUTF8`,
`looks_like_number`, `sv_isa`, `sv_derived_from`, `sv_isobject`.

**Writing:** `sv_setiv/uv/nv/pv/pvn/pvf/sv`, `sv_catpv/pvn/pvf/sv`,
`sv_setsv_flags`, `SvSetSV`, `sv_setref_pv/iv/nv` (the T_PTROBJ pattern §8.3),
`sv_bless`, `SvUPGRADE`, `SvGROW`, `SvCUR_set`, `SvPVX` (⚠ direct buffer
pointer — §6.2 makes this work), `SvUTF8_on/off`.

**Refcount / lifetime:** `SvREFCNT_inc(_simple[_void])`, `SvREFCNT_dec`,
`sv_2mortal`, `sv_free`.

**Arrays:** `av_len`/`av_top_index`, `av_fetch(av,i,lval)` (returns `SV**`!),
`av_store`, `av_push`, `av_pop`, `av_shift`, `av_unshift`, `av_clear`,
`av_extend`, `av_make`.

**Hashes:** `hv_fetch(hv,key,klen,lval)` (`SV**`; **negative klen = UTF-8
key**), `hv_store`, `hv_exists`, `hv_delete`, `hv_clear`, `hv_iterinit`,
`hv_iternext`, `hv_iterkey`, `hv_iterval`, `hv_iternextsv`, `hv_fetch_ent`
and friends (HE-based variants — map to the same callbacks).

**Globals / symbol table:** `get_sv("Foo::bar", GV_ADD)`, `get_av`, `get_hv`,
`get_cv`, `gv_stashpv` + `newSVpv`+`sv_bless` (the standard bless idiom),
`GvSV` — Tier 2 for full GV support; the four `get_*` cover most modules.

**Constants:** `PL_sv_undef`, `PL_sv_yes`, `PL_sv_no`, `PL_na` (a scratch
`STRLEN` for `SvPV(sv, PL_na)`).

**Calling back into Perl:**
```c
dSP; ENTER; SAVETMPS;
PUSHMARK(SP);
XPUSHs(sv_2mortal(newSViv(42)));
PUTBACK;
int count = call_pv("Foo::callback", G_SCALAR);   /* or call_sv, call_method */
SPAGAIN;
SV* result = POPs;
PUTBACK; FREETMPS; LEAVE;
```
Flags: `G_VOID/G_SCALAR/G_LIST` (context), `G_DISCARD`, `G_NOARGS`,
`G_EVAL` (trap die; caller then checks `SvTRUE(ERRSV)` — `ERRSV` is `$@`),
`G_KEEPERR`. Also `eval_pv(code, croak_on_error)` / `eval_sv`.

**Errors:** `croak`, `warn`, `croak_sv`, `die_sv`, `vcroak`.

**Magic** (Tier 2): `SvGETMAGIC`/`SvSETMAGIC` (no-op for untied values),
`sv_magic`, `mg_find`, `sv_magicext`. Needed for modules that tie or attach
private magic to objects (many do, to store C state — §8.3 covers the common
pattern without magic).

**The context macros:** every real API function takes the interpreter as a
hidden first argument when perl is built threaded: `pTHX_` (in prototypes),
`aTHX_` (at call sites), `dTHX` (fetch it from TLS). XS source is written
with these macros everywhere, so **our headers control what they mean**. We
define `pTHX` to pass our `pclxs_ctx*`. This is the JNI/HPy context
pattern falling out of perl's own API for free — do not throw it away by
making everything a global, even though v1 has exactly one context.

### 2.4 Typemaps

`xsubpp` maps C types in XS signatures to marshalling snippets via typemap
entries. The standard typemap (shipped with ExtUtils::ParseXS) covers
`T_IV, T_UV, T_NV, T_PV, T_BOOL, T_ENUM, T_U_INT, T_SV, T_SVREF, T_AVREF,
T_HVREF, T_CVREF, T_OPAQUEPTR, T_PTR, T_PTROBJ, T_REF_IV_PTR, T_ARRAY, …`.
Crucially the *expansions are in terms of the §2.3 API* (`SvIV(ST(0))`,
`sv_setref_pv(ST(0), "Foo::Ptr", (void*)RETVAL)`), so typemaps need **no shim
work at all** beyond the API itself. Custom typemaps in CPAN dists likewise
compile against our headers unchanged.

### 2.5 What this all means

Everything an XS module does funnels through (a) macros we get to redefine in
replacement headers, and (b) ~150–250 `Perl_*` functions with documented
semantics. There is no hidden channel *unless the module reaches past the API
into struct internals* — those modules are out of scope (Tier X) and are rare
outside perl's own dual-life core.

---

## 3. Requirements

R1. Recompiled XS modules load into a running SBCL and their subs are
    callable as ordinary PCL subs (dispatch, `wantarray`, `die`/`eval`
    all correct per `docs/ir-spec.md`).
R2. XS→Perl callbacks (`call_pv`/`call_sv`/`call_method`/`eval_pv`) work,
    including `G_EVAL` error trapping, with PCL subs on the other end.
R3. `croak` inside an XSUB surfaces as a PCL `die` (catchable by `eval {}`),
    without corrupting either the C or the Lisp stack.
R4. No raw pointers to host-GC-managed memory ever cross into C. (SBCL's GC
    moves objects — memory note: *"GC moves objects — never cache
    address-based NV"*. Handles only.)
R5. The C↔host boundary is a versioned, host-neutral vtable; the shim
    compiles with no reference to SBCL or CL.
R6. Byte-accurate string semantics at the boundary: `SvPV` yields *bytes*
    (with a UTF8 flag), even though host strings are Unicode (§6.2, §8.1).
R7. A build tool turns an unpacked CPAN dist into an installable shim `.so`
    with one command, using the system perl only for `xsubpp` and
    `Makefile.PL` metadata.

---

## 4. Architecture

### 4.1 Decision D1 — source recompilation, not binary ABI emulation

A `.so` compiled against real perl is full of *inlined macro expansions* that
read SV struct fields directly (`SvIVX(sv)` compiles to
`((XPVIV*)(sv)->sv_any)->xiv_iv`; `SvCUR`, `SvFLAGS`, stack macros likewise).
Supporting existing binaries would force us to replicate perl's exact SV/AV/HV
memory layout *and* refcount lifecycle — the cpyext trap. **Rejected.**

Instead we recompile from source. CPAN ships source by definition; the build
tool (§9) makes recompilation one command. Our replacement `perl.h` defines
every macro as a call into `libpclxs`, and `libpclxs` delegates to the
host vtable. XS authors' C code compiles unmodified because it was written
against the *documented macro API*, not the struct layout.

### 4.2 Decision D2 — SVs are shim-owned structs holding host *handles*, not host pointers

The old sketch (`XS_BRIDGE_DESIGN.md`) stored a `cl_object` pointer inside
each SV. That breaks under a moving GC and welds the shim to one host.
Instead:

```c
typedef int64_t pclxs_handle;   /* index into a host-side object table */

struct sv {                        /* the real definition of SV* in shim */
    uint32_t refcnt;
    uint16_t kind;                 /* SVK_NATIVE_* | SVK_PROXY | SVK_AV | SVK_HV | SVK_CV | SVK_RV */
    uint16_t flags;                /* SVf_IOK|NOK|POK|ROK|UTF8|READONLY|BLESSED... */
    pclxs_handle h;             /* 0 unless proxy/aggregate/code/rv */
    int64_t  iv;                   /* native/cached integer view */
    double   nv;                   /* native/cached float view */
    char    *pv; size_t cur, len;  /* native/cached byte buffer (SvPVX/SvCUR/SvLEN) */
};
```

The host never sees `struct sv`; the shim never sees a host object. They meet
only at `pclxs_handle`.

### 4.3 Decision D3 — two kinds of scalar: **native** and **proxy** (the perf-critical idea)

Most SVs inside an XSUB are *temporaries the host never needs to know about*
(`newSViv(0)`, loop counters, string builders). Crossing the vtable for every
`SvIV` on those would be ruinous.

- **Native SV**: created C-side (`newSViv`, `newSVpvn`, `sv_newmortal`, …).
  Lives entirely in the shim; `SvIV`/`SvPV`/`sv_cat*` are plain C on the
  struct fields, exactly as fast as real perl. `h == 0`. It acquires a host
  identity **only if it crosses the boundary** (returned on the stack, stored
  into a host array/hash, passed to `call_pv`) — at that moment the shim
  exports it via the appropriate `host->new_*` callback.
- **Proxy SV**: represents a host value (an incoming argument, a `get_sv`
  global, an `av_fetch` element from a host array). `h != 0`. Reads
  (`SvIV`, `SvPV`) call the host getter **once** and cache into the struct
  fields with the corresponding `IOK/NOK/POK` flag (this mirrors perl's own
  SvIOK/SvPOK cache design — and PCL's own `p-box` nv/sv caches). Writes
  (`sv_setiv` etc.) call the host setter and invalidate the cache.
- **Aggregates (AV/HV) are always proxies** in v1. A `newAV()` made C-side is
  immediately materialized in the host (one `host->new_av` call). This keeps
  the aggregate story to one implementation and matches how XS code actually
  uses AV/HV (rarely hot; hot data stays in C arrays).
- **RV (references)**: an SV with `SVK_RV`; its `h` is a host handle to a PCL
  reference box (`is-ref` p-box). `SvRV` asks the host for the target and
  wraps it in a proxy SV. `newRV_noinc(av)` asks host to build a ref box.

### 4.4 The three layers

```
┌───────────────────────────────────────────────────────────────┐
│  Recompiled XS module (Foo.so)                                │
│  — author's C + xsubpp output, compiled against layer below   │
├───────────────────────────────────────────────────────────────┤
│  xs/include/pclxs/{EXTERN.h, perl.h, XSUB.h}               │
│  — macro layer: stack macros, ST(), croak, pTHX, typedefs     │
├───────────────────────────────────────────────────────────────┤
│  libpclxs.so  (xs/src/*.c — pure C, no host references)    │
│  — SV structs, arg/mortal/scope stacks, setjmp frames,        │
│    refcounts, ~200 Perl_* functions                           │
├───────────────  pclxs_host.h vtable (§5)  ─────────────────┤
│  Host adapter: cl/pcl-xs.lisp (SBCL alien-callables)          │
│  — handle table, p-box/vector/hash mapping, wantarray, die    │
├───────────────────────────────────────────────────────────────┤
│  PCL runtime (cl/pcl-runtime.lisp) — unchanged semantics      │
└───────────────────────────────────────────────────────────────┘
```

---

## 5. The host vtable (`xs/include/pclxs/pclxs_host.h`)

This header is the **portability contract**. It must compile standalone with
no perl or CL includes. All strings crossing it are `(const char*, size_t,
int utf8)` byte buffers. All object references are `pclxs_handle`
(`int64_t`, 0 = null/none). All fallible entries return `pclxs_status`.

```c
#define PCLXS_ABI_VERSION 1

typedef int64_t pclxs_handle;
typedef enum { PS_OK = 0, PS_DIED = 1 } pclxs_status;
typedef enum { PS_VOID, PS_SCALAR, PS_LIST } pclxs_gimme;
typedef enum { PS_REF_NONE, PS_REF_SCALAR, PS_REF_ARRAY, PS_REF_HASH,
               PS_REF_CODE, PS_REF_GLOB } pclxs_reftype;

/* Host allocates the buffer for outgoing strings via this callback so the
   shim never guesses lifetimes: shim passes a writer, host calls it once. */
typedef void (*pclxs_str_sink)(void *ud, const char *bytes, size_t len, int utf8);

typedef struct pclxs_host_vtable {
    uint32_t abi_version;               /* must equal PCLXS_ABI_VERSION */

    /* -- handle lifecycle ------------------------------------------------ */
    void (*release)(pclxs_handle h);        /* drop host strong ref      */
    pclxs_handle (*dup)(pclxs_handle h); /* second strong ref, same obj */

    /* -- scalar construction (host returns new strong handle) ------------ */
    pclxs_handle (*new_undef)(void);
    pclxs_handle (*new_iv)(int64_t v);
    pclxs_handle (*new_nv)(double v);
    pclxs_handle (*new_pvn)(const char *bytes, size_t len, int utf8);

    /* -- scalar reads (host applies ir-spec §3 coercions) ---------------- */
    int64_t (*get_iv)(pclxs_handle h);
    double  (*get_nv)(pclxs_handle h);
    void    (*get_pvn)(pclxs_handle h, pclxs_str_sink sink, void *ud);
    int     (*get_bool)(pclxs_handle h);    /* p-true-p                  */
    int     (*is_defined)(pclxs_handle h);
    int     (*looks_like_number)(pclxs_handle h);

    /* -- scalar writes ---------------------------------------------------- */
    void (*set_iv)(pclxs_handle h, int64_t v);
    void (*set_nv)(pclxs_handle h, double v);
    void (*set_pvn)(pclxs_handle h, const char *bytes, size_t len, int utf8);
    void (*set_undef)(pclxs_handle h);
    void (*set_sv)(pclxs_handle dst, pclxs_handle src);  /* sv_setsv  */

    /* -- references / blessing -------------------------------------------- */
    pclxs_handle (*new_ref)(pclxs_handle target);       /* \X         */
    pclxs_reftype (*ref_type)(pclxs_handle h);          /* NONE if not a ref */
    pclxs_handle (*ref_target)(pclxs_handle h);         /* deref one level  */
    void (*bless)(pclxs_handle ref, const char *cls, size_t len);
    int  (*blessed_class)(pclxs_handle h, pclxs_str_sink sink, void *ud);
    int  (*isa)(pclxs_handle h, const char *cls, size_t len); /* sv_derived_from */

    /* -- arrays ------------------------------------------------------------ */
    pclxs_handle (*new_av)(void);
    int64_t (*av_len)(pclxs_handle av);                    /* count, not top index */
    pclxs_handle (*av_fetch)(pclxs_handle av, int64_t i, int lval);
    void (*av_store)(pclxs_handle av, int64_t i, pclxs_handle v); /* consumes v? NO — host dups (§5.1) */
    void (*av_push)(pclxs_handle av, pclxs_handle v);
    pclxs_handle (*av_pop)(pclxs_handle av);
    pclxs_handle (*av_shift)(pclxs_handle av);
    void (*av_unshift_n)(pclxs_handle av, int64_t n);      /* prepend n undefs */
    void (*av_clear)(pclxs_handle av);

    /* -- hashes ------------------------------------------------------------ */
    pclxs_handle (*new_hv)(void);
    pclxs_handle (*hv_fetch)(pclxs_handle hv, const char *k, size_t kl,
                                int utf8, int lval);
    void (*hv_store)(pclxs_handle hv, const char *k, size_t kl, int utf8,
                     pclxs_handle v);
    int  (*hv_exists)(pclxs_handle hv, const char *k, size_t kl, int utf8);
    pclxs_handle (*hv_delete)(pclxs_handle hv, const char *k, size_t kl, int utf8);
    void (*hv_clear)(pclxs_handle hv);
    int64_t (*hv_count)(pclxs_handle hv);
    pclxs_handle (*hv_iter_new)(pclxs_handle hv);       /* snapshot iterator */
    int (*hv_iter_next)(pclxs_handle it, pclxs_str_sink keysink, void *kud,
                        pclxs_handle *val_out);            /* 0 = exhausted */

    /* -- symbol table / globals -------------------------------------------- */
    /* sigil: '$' '@' '%' '&'; create != 0 => autovivify (GV_ADD).           */
    pclxs_handle (*get_global)(char sigil, const char *name, size_t len, int create);

    /* -- sub registration & calls ------------------------------------------ */
    /* Host defines a Perl-visible sub NAME that, when called, re-enters the
       shim via pclxs_invoke_xsub (§6.5) with this fnptr.                 */
    void (*define_xsub)(const char *name, size_t len, void *xsub_fnptr,
                        const char *filename);
    /* call_pv/call_sv/call_method + eval_pv unified.  code: a CODE-ref
       handle, or 0 with name set (call_pv / method name for call_method).
       Results are written through push_result into the shim's stack.
       If trap_errors (G_EVAL): host catches its die, stores $@, returns
       PS_DIED without unwinding the shim.  If !trap_errors and the sub dies,
       host must ALSO return PS_DIED (never unwind through C frames);
       the shim then croak-longjmps from its side (§6.6).                    */
    pclxs_status (*call)(pclxs_handle code,
                            const char *name, size_t namelen,
                            int is_method,
                            const pclxs_handle *args, size_t nargs,
                            pclxs_gimme gimme, int trap_errors,
                            void (*push_result)(void *ud, pclxs_handle h),
                            void *ud);
    pclxs_status (*eval_string)(const char *code, size_t len,
                            pclxs_gimme gimme,
                            void (*push_result)(void *ud, pclxs_handle h),
                            void *ud);

    /* -- errors / warnings --------------------------------------------------- */
    void (*set_errsv)(const char *bytes, size_t len, int utf8); /* $@ = msg  */
    void (*set_errsv_h)(pclxs_handle h);                     /* $@ = obj  */
    pclxs_handle (*get_errsv)(void);
    void (*warn)(const char *bytes, size_t len, int utf8);      /* honors $SIG{__WARN__} */
} pclxs_host_vtable;

/* The context every pTHX_ threads around.  v1: one global instance. */
typedef struct pclxs_ctx pclxs_ctx;   /* opaque; holds vtable + stacks */

/* Host entry points exported by libpclxs.so: */
pclxs_ctx *pclxs_init(const pclxs_host_vtable *vt);   /* checks ABI */
pclxs_status pclxs_boot(pclxs_ctx *, const char *so_path,
                              const char *boot_symbol);         /* dlopen+boot */
pclxs_status pclxs_invoke_xsub(pclxs_ctx *, void *xsub_fnptr,
                              const pclxs_handle *args, size_t nargs,
                              pclxs_gimme gimme,
                              void (*push_result)(void *ud, pclxs_handle h),
                              void *ud);                        /* §6.5 */
```

### 5.1 Ownership rules (write these into the header as comments — they ARE the contract)

1. Every handle returned by a host callback is a **strong reference** owned by
   the shim; the shim must `release` it exactly once (normally when the owning
   SV's refcount hits 0, or immediately for transient fetches).
2. Handles passed *into* host callbacks remain owned by the shim; if the host
   stores one (av_store, hv_store, call results kept in Perl data), the host
   duplicates its own internal reference — the C-side handle's lifetime is
   never extended by the host.
   *(For PCL this is free: the host resolves the handle to the underlying
   p-box/vector/table and stores that object; the handle table entry is
   untouched.)*
3. `av_fetch`/`hv_fetch` with `lval` must autovivify (create the element,
   return a handle **through which writes reach the aggregate** — for PCL
   return a handle to the element's p-box itself, which gives write-through
   for free).
4. The host **never unwinds** (throws/longjmps) through a vtable call. Every
   callback that can observe a Perl-level `die` returns `PS_DIED` instead
   (only `call` / `eval_string` can). Symmetrically the shim never longjmps
   through a host frame — `croak` unwinds only to the shim's own `setjmp`
   at the innermost `pclxs_invoke_xsub` / `pclxs_boot` (§6.6).
5. All byte buffers passed into callbacks are valid only for the duration of
   the call (host copies); buffers passed out use `pclxs_str_sink` so the
   host controls allocation on its side and the shim copies into SV buffers
   it owns.

### 5.2 Why the coercions live host-side

`get_iv` on the string `"3 apples"` must yield 3; `get_bool "0"` is false but
`"0.0"` is true; array-in-scalar is its length. That knowledge already exists
exactly once, in the PCL runtime (`to-number`, `to-string`, `p-true-p` —
ir-spec §3), and every host will have its own equivalents. Duplicating perl
coercion in C would create a second drift-prone implementation. The shim's
*native* SVs do need C-side coercions (they never touch the host) — implement
those in `xs/src/coerce.c` as a direct transcription of ir-spec §3.1–3.3 and
**test file `xs/t/coerce.t` must diff them against the runtime's answers**
over a table of nasty inputs ("0 but true", "1e3", " 12", "0x10", "inf",
"nan", overflow to double, trailing garbage).

---

## 6. libpclxs internals

Source layout: `xs/src/sv.c` (SV lifecycle, coercions bridge),
`xs/src/stack.c` (arg/mark/tmps/save stacks), `xs/src/av_hv.c`,
`xs/src/call.c` (invoke_xsub, call_pv family, croak/setjmp),
`xs/src/global.c` (ctx, PL_* variables), `xs/src/coerce.c`.

### 6.1 SV lifecycle

- Allocation: slab-allocate `struct sv` (arena of 4096, freelist) — XSUBs
  churn temporaries.
- `SvREFCNT_dec` → 0: if `h != 0`, `host->release(h)`; free pv buffer; return
  to freelist. There is **no cycle problem**: shim-side refcounts only keep
  host objects alive via the handle table; host-side cycles are the host GC's
  business, and a native SV cannot participate in a host cycle.
- Mortals: `sv_2mortal` pushes onto the ctx's tmps stack; `FREETMPS` decs
  everything above the floor saved by the matching `SAVETMPS`. `ENTER/LEAVE`
  maintain the save-stack floor exactly like perl.

**The refcount↔GC contract in one paragraph:** there are two ownership
domains that never manage each other's memory. Shim SVs are refcounted in C,
exactly like perl. Host objects are owned solely by the host GC. They meet at
the handle table (§7.1), which is a **non-weak** root: while C holds a
handle, the object is reachable and cannot be collected — a live handle *is*
the C side's strong reference. `SvREFCNT_dec` reaching zero translates to
"drop the GC root" (`host->release`); the actual freeing is always the GC's,
at a time of its choosing. Cycles cannot span the boundary because the host
never holds a pointer to a `struct sv` — only C points at the host, never
the reverse.

**Deliberate divergences from perl's refcount semantics** (document these in
`docs/not-supported.md` when the bridge ships; none of them block the module
ladder):

1. **`SvREFCNT(sv)` values are shim-local.** Host-side references to the
   same underlying value are invisible to C, so observed counts are lower
   than real perl's global truth. Only introspection/test code
   (`Internals::SvREFCNT`, `Devel::*`) reads counts as *values* — already
   not-supported territory.
2. **Refcount→0 does not fire `DESTROY`.** In perl, decrementing a blessed
   object's last reference calls `DESTROY` synchronously. Here it merely
   drops the root; PCL does not call `DESTROY` from GC at all
   (`docs/not-supported.md` §DESTROY). XS modules that free C resources in
   `DESTROY` leak them on drop — same status as pure-Perl DESTROY users,
   not a new gap.
3. **`sv_rvweaken` (the engine under `Scalar::Util::weaken`) is Tier 2.**
   When implemented, it becomes a vtable callback and can only be as weak as
   the host's own weaken support (PCL's is limited — hash.t weak-ref tests
   are not-supported today). A weakened proxy's handle-table slot would need
   to become a weak slot — one flag per slot, cheap, but defer until a
   ladder module demands it.

### 6.2 Strings: the byte boundary (R6)

Perl XS is byte-oriented: `SvPV` returns `char*` + length, `SvUTF8` says how
to interpret it. PCL strings are Unicode (`ir-spec` §2.6). The bridge rule:

- **Host → shim** (`get_pvn`): host encodes. If the string is pure
  Latin-1‑range and contains no char > 255, send raw bytes with `utf8=0`;
  otherwise UTF-8 bytes with `utf8=1`. (This matches perl's internal
  upgraded/downgraded distinction closely enough for API purposes.)
- **Shim → host** (`new_pvn`, `set_pvn`, key buffers): shim passes bytes +
  its `SVf_UTF8` flag; host decodes UTF-8 when the flag is set, else treats
  bytes as Latin-1 code points. This exactly mirrors what perl's `SvPVutf8`/
  `SvPVbyte` contract gives XS authors.
- `SvPVX/SvGROW/SvCUR_set` (in-place buffer building, JSON::XS style) work on
  the native pv buffer for native SVs. On a **proxy** SV, the first
  string-mutating macro **converts the SV to native** (fetch-then-own,
  copy-on-write) and it stops tracking the host value — semantically fine
  everywhere it matters, because PCL has no `@_` aliasing anyway (writes back
  to arguments never propagated in PCL even in pure Perl). `sv_set*` on a
  proxy is the exception: it writes through (that's how `OUTPUT:` on
  `get_sv` globals and hash elements works) — the distinction is
  *replace-value APIs write through; buffer-poking APIs localize*.

### 6.3 The argument stack

One `SV**` stack per ctx (`PL_stack_base/sp/max`), one mark stack — the real
perl design, sizes grown geometrically. All the §2.2 macros compile against
these ctx fields. This is deliberately boring: xsubpp output has intimate
expectations (ax arithmetic, `SPAGAIN` after callbacks) and the cheapest way
to satisfy all of them forever is to *be* the same design.

### 6.4 Context (`GIMME_V`)

`pclxs_invoke_xsub` receives `gimme` from the host (PCL: from
`*wantarray*` at the call site — t/nil/:void → `PS_LIST/PS_SCALAR/PS_VOID`)
and stores it in the ctx frame; `GIMME_V` reads it. `call/eval_string` pass
the XS caller's requested gimme back up to the host, which binds
`*wantarray*` accordingly around the funcall.

### 6.5 Invoking an XSUB (host → C), end to end

```
host (pl-sub trampoline for Foo::bar):
  1. flatten PCL args (ir-spec §5.2), one handle per scalar
     (aggregates were already flattened by Perl call semantics)
  2. gimme := *wantarray* mapping
  3. rc := pclxs_invoke_xsub(ctx, fnptr, argv, n, gimme, collect, ud)

shim pclxs_invoke_xsub:
  4. ENTER; SAVETMPS
  5. PUSHMARK; for each handle: push proxy-SV(dup(h)) as mortal
  6. frame.jmp := setjmp(...)          ── croak target
  7. call fnptr(aTHX_ cv)              ── the XSUB runs
  8. results are ST(0..n-1): for each, ensure host identity
     (native → export via new_iv/new_nv/new_pvn/new_ref…), call
     push_result(ud, handle)
  9. FREETMPS; LEAVE; return PS_OK
 10. on longjmp from croak: error SV already stored in ctx;
     set_errsv(...); FREETMPS; LEAVE (unwind save stacks to frame floor);
     return PS_DIED

host:
 11. PS_OK  → convert collected handles to return values per gimme
              (list → vector of boxes; scalar → single value)
 12. PS_DIED → (p-die (get $@))  — ordinary PCL die, caught by eval {}
```

Step 5's *dup* means the XSUB may `SvREFCNT_dec(ST(0))` (rare but legal for
mortalized args) without touching the host's own reference.

### 6.6 croak / die discipline (R3) — the one place people get this wrong

- `croak` **inside XS C code**: format message into an error SV in ctx,
  `longjmp` to the innermost shim frame (step 6). C-only unwind. This is
  bit-for-bit what perl does (its `JMPENV_JUMP`), including the consequence
  that raw `malloc`s in the XSUB leak unless the author used `SAVEFREEPV` —
  implement the `SAVE*`/save-stack family so those authors' cleanups run.
- Perl sub called *from* XS dies (`call` with `!trap_errors`): host returns
  `PS_DIED` (rule 5.1.4). Shim then croaks — i.e. longjmps its own frame —
  so the two unwind mechanisms compose without either crossing the other's
  frames. With `trap_errors` (`G_EVAL`): host stores `$@`, returns `PS_DIED`,
  shim's `call_sv` returns 0 results and the XS code checks `ERRSV` — perl
  semantics exactly.
- **Never** let a CL condition unwind through `alien-funcall` frames and
  never `longjmp` over Lisp frames. The status-code protocol above is the
  entire mechanism; any shortcut discovered during implementation ("SBCL
  seems to survive throwing through the callback") is forbidden — it skips C
  cleanups and is platform-dependent.

### 6.7 PL_* variables

Provide as ctx fields exposed through macros: `PL_sv_undef/yes/no` (three
static native SVs, `SVf_READONLY`), `PL_na`, `PL_stack_*`, `PL_markstack_*`,
`PL_tmps_*`, `PL_savestack_*`, `ERRSV` (macro → `get_errsv` proxy). Anything
else referenced by a target module shows up in the §10.1 census and gets a
case-by-case decision.

---

## 7. The SBCL host adapter (`cl/pcl-xs.lisp`)

New extension, loaded per `docs/extensions.md` (eager line at the bottom of
`pcl-runtime.lisp` **only when** `libpclxs.so` is present; plus a
self-loading stub in `XSLoader::load`).

### 7.1 Handle table

```lisp
(defvar *xs-objects* (make-array 1024 :adjustable t))  ; id → object
(defvar *xs-free-ids* '())                              ; freelist
```
`id → object` array + freelist; ids are fixnums (R4 satisfied — the GC can
move the objects, the array indirection absorbs it). `release` pushes the id
back on the freelist. **Not weak**: a live id *is* the C side's strong
reference. `dup` allocates a new id pointing to the same object (so releases
stay balanced 1:1 with handles, no per-id refcount needed).

What lives in the table: `p-box`es (scalars — for lval write-through, always
the box, never the unboxed value), adjustable vectors (arrays), hash tables,
functions (code), iterator closures.

### 7.2 Callbacks

One `sb-alien:define-alien-callable` per vtable entry (~55). Build the vtable
struct with `sb-alien` struct support, populate with
`alien-callable-function`, call `pclxs_init`. Each callable body is a thin
adapter onto existing runtime functions — **no new semantics in this file**:
`get_iv` = `(truncate (to-number (unbox obj)))` (with the IV-range clamp perl
applies), `get_bool` = `p-true-p`, `bless` = set `p-box-class`, `isa` =
the existing C3/`isa` walk, `warn` = `p-warn` (which already honors
`$SIG{__WARN__}`), `get_global` = intern in the package per ir-spec §7.
`call` wraps the funcall in `(catch :p-die-to-status ...)` — concretely: run
inside `handler-case`/`catch` matching however `p-eval-block` catches die
today, then either store `$@` + return `PS_DIED` (trap_errors) or return
`PS_DIED` with `$@` set for the shim to re-croak.

Every callable body must also guard against *CL-level* errors escaping (rule
5.1.4 applies to unexpected conditions too): wrap in `handler-case`, convert
to `PS_DIED`/`$@` where the signature allows, else log-and-return-default —
an SBCL condition flying into C is a crash.

### 7.3 The XSUB trampoline (`define_xsub`)

When the shim registers `Foo::bar` with fnptr P, the host:

```lisp
(setf (fdefinition (intern "pl-bar" (p-find-or-make-package "Foo")))
      (lambda (&rest %_args)
        (xs-invoke P (p-flatten-args %_args))))  ; plus p-sub bookkeeping
```

Wrap so `caller()`, `*pcl-current-package*` and the `:p-return` frame behave
like any `p-sub` (reuse the `p-sub` machinery — likely by generating a
`p-sub` whose body calls `xs-invoke`; do NOT hand-roll a second calling
convention). Also mark it `:defined` in `*p-declared-subs*`.

`xs-invoke`: allocate ids for each flattened arg (scalars boxed if raw),
stack-allocate the `int64` argv with `sb-alien`, map `*wantarray*` → gimme,
call `pclxs_invoke_xsub`, collect result handles via the `push_result`
callable into a list, convert per gimme (list → fresh adjustable vector of
boxes; scalar → last/only value's box contents), release the transient arg
ids, and `p-die` on `PS_DIED`.

### 7.4 Loader integration

Replace the body of `XSLoader::pl-load` (keep the die as fallback):

1. Compute the module's shim object path:
   `<dist-install-root>/auto/<Module/Path>/<Last>.pcl.so` searched along
   PCL's module path (same roots as `lib/` resolution in
   `docs/shipped-modules.md`), **plus** a version/ABI tag in the filename or
   a sidecar file — refuse to load on ABI mismatch.
2. Found → ensure `pcl-xs` extension + `libpclxs.so` are initialized,
   `pclxs_boot(ctx, path, "boot_Foo__Bar")`, return 1.
3. Not found → existing die (pure-Perl fallback keeps working).
4. Remove the module from `*p-xs-only-modules*` handling only when a shim
   build exists — the pure-perl skip list logic stays for everything else.

`dlopen` note: load `libpclxs.so` with `sb-alien:load-shared-object`
first; the module `.so` is dlopened *by the shim* (`pclxs_boot`) with
`RTLD_NOW | RTLD_LOCAL` — module symbols resolve against libpclxs because
the module links `-lpclxs` at build time with a DT_NEEDED entry, not via
global namespace pollution. Symbols in our headers are prefixed
(`pclxs_sv_iv` etc.) with `#define SvIV(sv) pclxs_sv_iv(aTHX_ sv)` so a
process that somehow also embeds real perl cannot collide.

---

## 8. Semantic hot spots (decide these consciously, don't discover them)

### 8.1 UTF-8 round-trips
Covered by §6.2. Add `xs/t/utf8.t`: pass "café"/"日本語"/high-byte Latin-1
strings through an echo XSUB, through `sv_catpvn`, through hash keys both
directions; compare against real perl running the same `.xs`.

### 8.2 Numbers
IV is `int64_t`. PCL integers are bignums; `get_iv` must clamp/truncate the
way perl does when an NV exceeds IV range (IV_MAX saturation), and `get_nv`
must go through `to-number`. `SvUV` maps onto IV with the usual perl
wrap-negative convention. Floats: doubles both sides — no issue. Add these
cases to `xs/t/coerce.t`.

### 8.3 The T_PTROBJ / "C object" pattern
The dominant XS-OO idiom: `sv_setref_pv(sv, "Foo::Handle", ptr)` blesses a
scalar-ref whose scalar holds the pointer as an IV; methods recover it with
`SvIV(SvRV(ST(0)))` (via typemap `T_PTROBJ`, which also `sv_derived_from`
checks). This works with **zero special support** — the pointer is just an
integer flowing through IV plumbing, and the host stores it in a box like any
number. It's opaque to the host; only the same module's C code dereferences
it. Digest::MD5's context object is exactly this — hence its place in the
module ladder. (Without GC-driven `DESTROY` — `docs/not-supported.md` — such
C objects leak on drop; same status as every other DESTROY-dependent
resource. Document, don't fix here.)

### 8.4 Magic / ties
`SvGETMAGIC`/`SvSETMAGIC` are no-ops in v1 (host `get_*` on a tied box
already FETCHes — PCL's unbox does that — so tied *values* arriving as
arguments behave). `sv_magic` with private (`~`/ext) magic: Tier 2; store a
`(mg_virtual, mg_ptr)` pair in a shim-side weak map keyed by the SV. Punt
until the census (§10.1) shows a target module needing it.

### 8.5 PerlIO
Tier 2. v1 provides `PerlIO_stdout/stderr/printf/write` as thin wrappers over
C `stdio` (correct enough for diagnostics). Modules doing real Perl-filehandle
I/O through XS (`PerlIO_findFILE` on user handles) are deferred; the census
decides if the vtable grows a stream group.

### 8.6 What genuinely cannot work
Direct struct access (`sv->sv_flags` without macro), custom ops, source
filters implemented in XS, `PL_check`/parser hooks, `MULTICALL` (List::Util's
optimized callback loop — but PCL already ships a pure-Perl List::Util shim,
and real List::Util has pure-perl fallbacks). Detect at *build* time: these
appear as unresolved symbols / compile errors against our headers, so the
build tool reports "Tier X: uses internals" with the symbol list, instead of
producing a broken `.so`.

---

## 9. Build toolchain (`tools/pcl-xs-build`) — Perl, per project rules

```
pcl-xs-build [--perl /usr/bin/perl] path/to/Unpacked-Dist-1.23/
```

**Local build only — no prebuilt binaries (decision, 2026-07-07).** PCL
must run across Linux and BSD variants (and eventually macOS); shipping
per-platform `.so`s is a distribution and trust burden we refuse. Both
`libpclxs.so` and every module `.so` are compiled **on the target
machine**. Portability rules:

- **Steal the toolchain knowledge from the system perl's `%Config`**: use
  `$Config{cc}`, `$Config{cccdlflags}` (the platform's `-fPIC` equivalent),
  and `$Config{lddlflags}` (the platform's `-shared` equivalent), plus
  `$Config{dlext}`. The system perl already encodes how to build a loadable
  object on this exact OS — that is literally how it builds its own XS — so
  we inherit Linux/FreeBSD/OpenBSD/NetBSD support without maintaining a
  per-OS flag table. Only the *include path* differs from a normal XS build
  (ours, never perl's CORE).
- **Only POSIX `dlopen`/`dlsym` in the shim's loader** (all ELF platforms;
  macOS works too, revisit `.dylib`/`dlext` naming when it matters).
- **Platform-key the artifacts**: the ABI tag (§7.4) includes
  `PCLXS_ABI_VERSION` **and** `$Config{archname}`-style OS/arch, so a
  home directory or repo shared across machines (NFS, dotfile sync) never
  loads a foreign-platform or stale-ABI object; on mismatch the loader
  falls through to the pure-Perl path and reports why.
- `libpclxs` itself is a bootstrap `make`-style step of the PCL install
  (a `tools/build-pclxs` wrapper using the same `%Config`-derived
  flags); `pcl-xs-build` refuses to run until it exists.

Steps per dist:

1. Run `perl Makefile.PL` in a scratch dir only to *harvest metadata* (or
   parse `META.json` + `MANIFEST`): XS file list, extra C files/libs
   (`MYEXTLIB`, `LIBS`, `INC`), custom typemaps. Do not trust its generated
   Makefile for compilation flags — we supply our own (`%Config`-derived,
   above).
2. For each `.xs`: `perl -MExtUtils::ParseXS -e '...'` (i.e. run xsubpp under
   the **system perl**) with the standard typemap + dist typemaps → `.c`.
3. Compile all `.c` with `$Config{cc} $Config{cccdlflags} -I
   xs/include/pclxs`, **no perl CORE includes anywhere**, link with
   `$Config{lddlflags} -lpclxs` →
   `blib-pcl/auto/Foo/Bar/Bar.pcl.so` (plus the platform+ABI tag from §7.4).
4. Transpile the dist's `.pm` files with `pl2cl` as usual (they're ordinary
   Perl; their `XSLoader::load` now finds the `.so`).
5. Emit a report: unresolved perlapi symbols (census input, §10.1), Tier X
   verdicts, warnings for `Makefile.PL` features we ignored.

Cache/invalidations: a shim `.so` is keyed by `PCLXS_ABI_VERSION`; the
transpiled `.pm` cache keys by `*pcl-cache-generation*` as today (memory
note: bump it when the loader changes emission — this feature shouldn't, but
`XSLoader::load`'s new body lives in the runtime, so a runtime edit → the
usual stale-cache care applies).

---

## 10. API surface: measure, don't guess

### 10.1 The census procedure (do this in Phase 0, before writing sv.c)

For each module on the ladder (§12), build it against **real perl** once,
then:

```bash
nm -u --defined-only=false auto/Foo/Foo.so | perl -ne 'print "$1\n" if /\b(Perl_\w+|PL_\w+)\b/' | sort -u
```

Union across the ladder = the *demanded* API, typically 80–150 symbols —
implement exactly these plus the macro layer, stub the rest of perl.h behind
`#error "pclxs: unimplemented API Perl_xxx — see docs/xs-shim-design.md §10"`
so gaps fail at *compile* time with a searchable message, never at runtime.
Keep the census script as `tools/xs-api-census.pl` and its output per module
in `xs/census/` — it is the living prioritization list.

### 10.2 Tiers

- **Tier 1 (Phase 1–3):** everything in §2.3 except magic/PerlIO/GV
  internals. Sufficient for the ladder through Digest::MD5.
- **Tier 2:** magic (`sv_magic`/`mg_find`), `PerlIO_*` group, `gv_*`/stash
  walking, `hv_*_ent` HE variants, `sv_2mortal`-free `TARG` optimization,
  `newXS_flags`, `CvXSUBANY` aliases.
- **Tier X (documented refusals, add to `docs/not-supported.md` §DynaLoader
  successor):** §8.6 list.

---

## 11. Performance notes (design now, optimize later)

- The proxy-cache design (§4.3) means a typical XSUB doing
  `SvIV(ST(0))` + arithmetic + `XSRETURN_IV` costs ~2 vtable crossings
  (arg export at push, result import) + 1 cached getter — not one crossing
  per macro. Target: an add(a,b) XSUB round-trip within ~3–5× of a native
  `p-sub` call; measure in Phase 1 with `bench/xs-call.pl`.
- `define-alien-callable` crossings on SBCL are ~sub-microsecond; the handle
  table adds an array ref. Fine.
- If a hot module hammers `av_fetch` in a loop (numeric vectors), the
  eventual answer is a bulk callback (`av_read_range` into a C buffer) added
  as vtable v2 — leave a comment, don't build it yet.
- SBCL gotcha from R1 work (memory: *inline+narrow-ftype ICE on 2.6.0*):
  keep the callable bodies plain funcallable definitions, no aggressive
  ftype declamations on them.

---

## 12. Phase plan (each phase ends green before the next starts)

### 12.0 The ladder (the census input — §10.1 depends on this list)

Build order, and what each module is meant to prove. `census/` in the pclxs
repo is generated from exactly this list.

| module | tier | proves |
|---|---|---|
| MIME::Base64 | 1 | smallest real dist: byte strings in and out, no objects |
| Digest::MD5 | 1 | T_PTROBJ C-object pattern, byte strings, OO methods (Phase 3 target) |
| Time::HiRes | 1 | syscall-shaped XS, almost no SV traffic (Phase 4 target) |
| Cwd | 1 | alternate syscall-ish target if Time::HiRes goes native (§14.2) |
| Params::Util | 1 | plain scalar/aggregate API, no magic — breadth check |
| Storable | 2 | heavy aggregate + magic user; measures the Tier 2 boundary |
| JSON::XS | 2 | stress test: buffer building, callbacks, blessed booleans (§14.3) |
| List::Util | X | `MULTICALL` — the refusal reference case: must be *detected*, not built |

First census (2026-07-24, perl 5.40.3): 187 distinct perlapi symbols across
the installed seven; 26–40 per Tier 1 module, 110 for Storable, 119 for
List::Util.

**Phase 0 — census & skeleton.** Write `tools/xs-api-census.pl`; run it over
the ladder modules built against real perl; check results into `xs/census/`.
Create `xs/include/pclxs/*.h` with types, vtable, macro layer stubbed.
*Acceptance:* census files exist; a trivial `.c` including our `perl.h`
compiles.

**Phase 1 — hand-written XSUB end to end.** Implement native SVs, the
stacks, `pclxs_init/invoke_xsub`, croak/setjmp, and the ~20 scalar vtable
entries; `cl/pcl-xs.lisp` with handle table + callables; hand-write
`xs/t/Arith/Arith.c` (no xsubpp yet: add(IV,IV), concat(PV,PV),
list-return under G_LIST, a croak path, a call_pv callback path).
*Acceptance:* new `Pl/t/xs-01.t` — transpiled Perl loads Arith, calls all
five, `eval { Arith::boom() }` catches the croak, callback into a Perl sub
returns through XS. Paren discipline + `prove` gate green.

**Phase 2 — xsubpp + build tool.** `tools/pcl-xs-build`; rebuild Arith from
a real `Arith.xs` via system-perl xsubpp; typemap T_IV/T_NV/T_PV/T_SV pass.
*Acceptance:* `pcl-xs-build` one-command build; `xs/t/coerce.t` diffing
C-side coercions against runtime answers.

**Phase 3 — aggregates + globals + Digest::MD5.** AV/HV vtable group,
refs/bless/isa, `get_global`, `sv_setref_pv`; loader integration (§7.4).
*Acceptance:* unmodified **Digest::MD5** dist builds and its own `t/*.t`
files run under the PCL sweep (OO handle pattern, byte strings, `md5_hex` of
UTF-8 input matching perl).

**Phase 4 — callbacks at scale + a string-heavy module.** `call` flags
complete (G_EVAL/G_DISCARD/G_KEEPERR/method calls), `eval_string`,
SvGROW/SvPVX buffer building hardened. Target module: **Time::HiRes**
(mostly syscalls — easy win, high value) then stretch **JSON::XS** (buffer
building + callbacks + blessed booleans; expect Tier-2 discoveries — file
them, don't rabbit-hole).
*Acceptance:* Time::HiRes dist tests pass rate reported; JSON::XS
`t/01_utf8.t`-class basics pass or failures are census-classified.

**Phase 5 — docs + registry.** Update `docs/not-supported.md` (DynaLoader
section → supported-with-build-step + Tier X list), `docs/shipped-modules.md`
(new provider kind: shim-XS), `docs/extensions.md` (pcl-xs), CLAUDE.md key
files. Add `docs/xs-host-porting.md`: the vtable contract from the *host
author's* perspective (ownership rules 5.1 verbatim, the coercion table a
non-Perl host must implement, the two-status error protocol) — this is the
deliverable that makes the "other scripting languages" goal real.

---

## 13. Testing strategy

- Unit: `xs/t/*.t` run C-side behaviors through tiny dedicated XSUBs (the
  Arith pattern) — every vtable entry gets exercised by at least one.
- Differential: where possible each `xs/t` case also runs under real perl
  (the `.xs` compiles against both); assert identical output. This is the
  same oracle philosophy as `cl/pack-impl.pl` and `tools/difftest-ops.pl`.
- Regression home: `Pl/t/xs-01.t` (new file — allowed, this is a new
  subsystem; keep test count per file modest per CLAUDE.md §6).
- Crash discipline: any segfault reproducer gets minimized into `xs/t/`
  before fixing (follow `docs/debugging-hangs-crashes.md` protocol).

## 14. Open questions (decide with the user before the relevant phase)

1. ~~Ship prebuilt `.so`s or build locally?~~ **RESOLVED (user,
   2026-07-07): local build only.** XS objects cannot be meaningfully
   precompiled across the Linux/BSD spread PCL targets; §9 now specifies
   the `%Config`-derived portable toolchain and platform-keyed artifacts.
2. **Time::HiRes**: PCL may grow a native implementation first — if so, swap
   another syscall-ish module into Phase 4 (e.g. `Cwd`'s XS path or
   `Digest::SHA`).
3. Is **JSON::XS** worth Tier-2 magic work, given `JSON::PP` already
   partially runs? (Recommendation: treat it as a stress test, not a
   commitment.)
4. Vtable v2 candidates discovered during implementation (bulk array ops,
   stream group) — collect, review at Phase 5.
