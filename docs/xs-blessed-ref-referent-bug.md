# The referent of a bridge-built blessed scalar ref is unreadable

**Status:** open. Blocks the OO half of every XS module that uses the
T_PTROBJ idiom — which is most of them, Digest::MD5 included.

**Not the XS adapter.** `cl/pcl-xs.lisp` had two bugs of this flavour and
both are fixed (session 312, commit 1d776d8); the conformance case that
catches them passes. What is left is PCL's own representation of a
reference, so this is a runtime question, not a bridge question.

## Reproducer

Needs the artifact in a cache (`tools/pcl-xs-install
~/.cpan/build/Digest-MD5-*`), then:

```perl
use Digest::MD5;
my $o = Digest::MD5->new;
print "ref=",   ref($o),                        "\n";  # Digest::MD5   ✔
print "isa=",   ($o->isa('Digest::MD5') ? 1:0), "\n";  # 1             ✔
print "deref=", (defined($$o) ? "ok" : "undef"), "\n"; # undef         ✘
```

`${ Digest::MD5->new }` is undef too, so an intervening variable is not
involved. Consequently `$o->add("x")` croaks **"Failed to get MD5_CTX
pointer"**, which is Digest::MD5's own check:

```c
static MD5_CTX* get_md5_ctx(pTHX_ SV* sv) {
    if (SvROK(sv)) {
        SV* svp = SvRV(sv);
        if (SvIOK(svp) && SvIV(svp))     /* the C pointer, as an IV */
            return INT2PTR(MD5_CTX*, SvIV(svp));
    }
    croak("Failed to get MD5_CTX pointer");
}
```

So: the reference survives, the class survives, the **referent's value**
does not.

## What is already ruled out

| checked | result |
|---|---|
| the shim (pclxs) | `refhost` passes every reference case, including the new `ptrobj_via_host` (blessed ref → host array → back) |
| the XS adapter's store path | `%xs-own-copy` preserves value *and* class; `av_store`/`av_push`/`hv_store` no longer unbox |
| the XS adapter's result path | `xs-collect-result` returns the cell for a reference instead of unboxing it |
| PCL end to end | pclxs's corpus through `tools/pcl-conform`: **246 pass, 1 fail** — and the one failure is the *blessing* divergence below, not this |

The object is built by `sv_setref_pv`, which on our side becomes:
`new_iv(ptr)` → a `p-box` holding an integer, then `new_ref` → PCL's
`p-backslash` of that box, then `bless`. Each step looks right in
isolation; what comes out cannot be dereferenced from Perl code.

## Where to start

1. **Try the pure-Perl analogue first.** Build the same shape without the
   bridge — a box created by the runtime, `\$x`, blessed, then `$$ref` —
   and see whether it derefs. If it does, the difference is in what
   `new_iv`/`p-backslash` produce versus what the ordinary path produces,
   and that difference is the bug. If it does *not*, the bug is older and
   larger than the XS work.
2. Compare `p-box-value` of the wrapper against what `$$` lowers to.
   `p-backslash` over a box whose value is an integer is the exact case.

## The neighbour

Related but distinct, and also open: **PCL records a scalar ref's
blessing on the wrapper box, not on the referent** (`p-bless`,
`cl/pcl-runtime.lisp` ~12426, deliberate and commented). Perl blesses the
referent, which is why `SvSTASH(SvRV(rv))` reads a class. XS code asking
the referent therefore finds nothing — that is the single remaining
conformance failure (`bless_and_class`).

The two are likely to be fixed by the same piece of thinking about what a
scalar reference *is* here, which is why they are documented together.
Neither is urgent for hash-based objects: those blessing on the inner hash
matches perl, and the referent question does not arise.
