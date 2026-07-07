# XS/C Extension Bridge Design

**Goal:** Enable any CPAN module with XS/C code to run on Common Lisp.

**Status:** SUPERSEDED — see `docs/xs-shim-design.md` for the full,
implementation-ready design (source-recompile against replacement headers +
`libperlshim` + host-neutral vtable). This file is kept for history; its
"store a CL object pointer inside each SV" approach is rejected there (§4.2:
moving GC, host lock-in).

---

## Strategy Overview

### Two-Track Approach

1. **Native CL Replacement** - For core infrastructure modules, use CL equivalents:

   | Perl Module | CL Replacement | Rationale |
   |-------------|----------------|-----------|
   | DBI, DBD::* | CL-DBI, Postmodern, cl-mysql | Database libs are mature in CL |
   | Moose, Moo | CLOS | CL's object system is more powerful (see `MOO_MOOSE_DESIGN.md`) |
   | JSON::XS | cl-json, jonathan, jzon | Pure CL is fast enough |
   | Try::Tiny | CL's condition system | Native exception handling |
   | List::Util | CL built-ins | Already in the language |

2. **XS Bridge (libperlcl)** - For modules with no CL equivalent:
   - Crypto libraries (Crypt::*, Digest::*)
   - Binary format parsers
   - Scientific/numerical code
   - Bindings to C libraries (libxml2, etc.)

---

## The XS Bridge Architecture

```
┌────────────────────────────────────────────┐
│           Any XS Module (.xs → .so)        │
│  (compiled normally, links against shim)   │
└────────────────────┬───────────────────────┘
                     │
┌────────────────────▼───────────────────────┐
│         libperlcl.so (The Bridge)          │
│  • Implements perlapi.h (~200 functions)   │
│  • SV/AV/HV backed by CL objects           │
│  • Refcount → CL GC integration            │
│  • XS stack ↔ CL function calls            │
└────────────────────┬───────────────────────┘
                     │ CFFI
┌────────────────────▼───────────────────────┐
│         Common Lisp Runtime                │
│  (SBCL, CCL, ECL, etc.)                    │
└────────────────────────────────────────────┘
```

---

## Core Implementation

### Phase 1: Core Types (1-2 months)

```c
// SV (Scalar Value) backed by CL object
typedef struct {
    cl_object cl_ref;      // CFFI-accessible CL object
    uint32_t flags;        // SvFLAGS compatible
    uint32_t refcnt;       // Compatibility layer, CL GC is real owner
} SV_shim;

// Core accessors - delegate to CL
char* SvPV(SV* sv, STRLEN* len) {
    return cffi_call_cl("sv-to-string", sv->cl_ref, len);
}

NV SvNV(SV* sv) {
    return cffi_call_cl("sv-to-number", sv->cl_ref);
}

IV SvIV(SV* sv) {
    return cffi_call_cl("sv-to-integer", sv->cl_ref);
}

SV* newSVpv(const char* str, STRLEN len) {
    cl_object cl_str = cffi_call_cl("make-pl-string", str, len);
    return wrap_cl_object(cl_str);
}
```

### Phase 2: Collections (1 month)

```c
// AV (Array) → CL adjustable vector
AV* newAV() {
    cl_object vec = cffi_call_cl("make-pl-array");
    return wrap_as_av(vec);
}

void av_push(AV* av, SV* val) {
    cffi_call_cl("pl-array-push", av->cl_ref, val->cl_ref);
}

SV* av_fetch(AV* av, I32 idx, I32 lval) {
    cl_object result = cffi_call_cl("pl-array-ref", av->cl_ref, idx);
    return wrap_cl_object(result);
}

// HV (Hash) → CL hash-table
HV* newHV() {
    cl_object ht = cffi_call_cl("make-pl-hash");
    return wrap_as_hv(ht);
}

SV** hv_store(HV* hv, const char* key, I32 klen, SV* val, U32 hash) {
    cffi_call_cl("pl-hash-set", hv->cl_ref, key, klen, val->cl_ref);
    return &val;  // Simplified
}
```

### Phase 3: XS Calling Convention (2 months)

The hardest part - Perl's XS uses a custom stack:

```c
// XS function signature
XS(XS_MyModule_add) {
    dXSARGS;                    // Declare stack access
    if (items != 2)
        croak("Usage: add(a, b)");

    IV a = SvIV(ST(0));         // Get arg 0
    IV b = SvIV(ST(1));         // Get arg 1

    XSRETURN_IV(a + b);         // Return integer
}
```

Must implement:
- `dXSARGS` - Stack frame setup
- `ST(n)` - Access argument n
- `items` - Argument count
- `XSRETURN_*` - Return macros
- `PUSHMARK`/`PUTBACK` - Stack manipulation

### Phase 4: Memory Bridge (2 months)

Key challenge: Prevent CL GC from collecting objects C still references.

```c
// Root table: objects C code is using
static cl_object gc_root_table;  // CL hash-table, prevents GC

SV* wrap_cl_object(cl_object obj) {
    SV_shim* sv = malloc(sizeof(SV_shim));
    sv->cl_ref = obj;
    sv->refcnt = 1;

    // Add to root table so CL doesn't GC it
    cffi_call_cl("gc-root-add", obj);

    return (SV*)sv;
}

void SvREFCNT_dec(SV* sv) {
    SV_shim* shim = (SV_shim*)sv;
    if (--shim->refcnt == 0) {
        // Remove from root table, allowing CL to GC
        cffi_call_cl("gc-root-remove", shim->cl_ref);
        free(shim);
    }
}
```

### Phase 5: Callbacks to CL (1 month)

XS code often calls back into Perl:

```c
// call_pv("some_sub", G_SCALAR) → call CL function
I32 call_pv(const char* name, I32 flags) {
    cl_object result = cffi_call_cl("pl-call-sub", name, flags);
    // Push result onto XS stack
    push_to_xs_stack(result);
    return 1;
}
```

---

## Timeline

| Milestone | Time | Result |
|-----------|------|--------|
| Proof of concept | 2 months | Simple XS module works |
| Core functionality | 4 months | 50% of XS patterns work |
| Production ready | 8-12 months | Most CPAN XS modules work |

---

## Implementation Notes

### Consider ECL (Embeddable Common Lisp)

ECL can embed C code directly, which may simplify the bridge:

```lisp
(ffi:clines "#include <openssl/sha.h>")

(ffi:def-function ("SHA256" c-sha256)
    ((data :pointer-void) (len :size-t) (out :pointer-void))
  :returning :pointer-void)
```

ECL might be the ideal CL implementation for this project.

### Minimal API Subset

Not all 500+ perlapi functions are commonly used. Priority:

**Must Have (Phase 1-2):**
- SV creation/access: `newSV*`, `SvPV`, `SvIV`, `SvNV`, `SvROK`
- AV operations: `newAV`, `av_push`, `av_fetch`, `av_len`
- HV operations: `newHV`, `hv_store`, `hv_fetch`, `hv_exists`
- Memory: `SvREFCNT_inc`, `SvREFCNT_dec`, `sv_2mortal`

**Should Have (Phase 3):**
- XS stack: `dXSARGS`, `ST()`, `XSRETURN_*`
- Callbacks: `call_sv`, `call_pv`
- Errors: `croak`, `warn`

**Nice to Have (Phase 4+):**
- Regex: `pregcomp`, `pregexec`
- I/O: `PerlIO_*`
- Globs: `gv_*`

---

## Modules to Test With

Start simple, work up:

1. **Trivial:** Custom XS module with just arithmetic
2. **Simple:** Digest::MD5 (well-defined C API)
3. **Medium:** JSON::XS (string handling)
4. **Complex:** DBI (but might prefer native CL-DBI)

---

## Open Questions

1. Which CL implementation to target first? (SBCL for speed, ECL for C integration?)
2. How to handle XS modules that use Perl's regex engine?
3. Thread safety considerations?
4. How to distribute the bridge library?

---

## References

- [perlapi documentation](https://perldoc.perl.org/perlapi)
- [perlguts - Perl internals](https://perldoc.perl.org/perlguts)
- [CFFI manual](https://common-lisp.net/project/cffi/manual/)
- [ECL manual](https://common-lisp.net/project/ecl/static/manual/)
