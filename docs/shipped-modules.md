# Shipped modules: how `use Module` is resolved

When a program says `use Foo` or `require Foo`, PCL usually does what perl
does: it finds `Foo.pm` on `@INC` and compiles it like any other Perl
source. A few modules cannot be loaded that way, mostly because their real
implementation is C (XS). For those, PCL ships a replacement written in
plain Perl, in [`lib/`](../lib). This page says how a module is resolved,
which replacements exist and why, and where a new one belongs.

## How `use Foo` is resolved

Checked in this order (`p-use` in `cl/pcl-runtime.lisp`):

1. **Lexical pragmas** (`strict`, `warnings`, `feature`, `utf8`, `integer`
   and a few more): nothing is loaded. PCL handles them in the compiler.
2. **`XSLoader`, `DynaLoader` and `Carp::Heavy`**: recorded in `%INC` as
   loaded, and nothing is run (`*p-xs-only-modules*`).
3. **`Test::More`, `Test::Simple` and `Test2::Bundle::More`**: PCL supplies
   these itself. The real Test::More is built on Test2, which needs XS
   internals, so `use Test::More` loads PCL's own TAP implementation,
   `cl/pcl-test.lisp`, the first time a program asks for it
   (`*p-pcl-provided-modules*`).
4. **A replacement marked `# pcl-shim: must-win`** in its header is used
   without searching `@INC` at all. The marker is on the replacements whose
   real module cannot run under PCL: every XS module in the first table
   below, `Test::More`, and `Carp` and `Math::BigInt::Calc` (perl's own
   copies were measured failing under PCL: `croak` undefined, and a hang).
   So a `PERL5LIB` holding perl's real `List/Util.pm`, which a local::lib
   often does, cannot break a program.
5. **Everything else** is looked up on `@INC`, which has perl's order: the
   `-I` directories, then `PERL5LIB`, then PCL's `lib/`, then perl's own
   library directories. A replacement in `lib/` therefore wins over the
   installed module of the same name, and a user's own `-I` copy wins over
   the replacement, as it would under perl. The file found is compiled and
   cached like the program itself ([`caching.md`](caching.md) §2).
6. **An XS module with no replacement** (its `.pm` is found, but it needs a
   compiled `.so`) fails the way a missing module fails in perl: with
   "Can't locate ...", which is what optional-XS wrappers on CPAN expect. The
   experimental XS bridge can build some real XS distributions; see
   [`STATUS.md`](STATUS.md#xs).

`pack`, `mro::` and `warnings::` functions are not loaded through `use` at
all: they are runtime extensions that load on first call
([`extensions.md`](extensions.md)).

## What is in `lib/`

As of 2026-10-10, `lib/` holds 31 modules. Each file's header comment says
in detail why it exists.

**The real module is XS; this one is plain Perl:**

| module | notes |
|---|---|
| `List::Util`, `Scalar::Util`, `Sub::Util` | the functions and prototypes, in Perl |
| `POSIX` | the parts PCL can provide; the classes and system calls that need a real C library are listed in [`not-supported.md`](not-supported.md) |
| `Fcntl`, `Socket` | constants and functions |
| `Cwd` | `cwd` and `getcwd` use PCL's built-ins; `abs_path` and `realpath` resolve symlinks in Perl |
| `Time::HiRes` | plain Perl over four small runtime primitives (a clock, a sleep, and their resolution) |
| `MIME::Base64` | the whole encoding in Perl |
| `Digest::MD5`, `Digest::SHA` | every digest, HMAC and the OO interface, byte-identical to the XS modules (#2947, s514a); written fresh, every 32-bit step masked, SHA-512 kept in 32-bit halves.  About 400x (MD5), 640x (SHA-256) and 2000x (SHA-512) slower than XS on 1 MB (0.47 s / 1.9 s / 4.4 s against 0.001-0.003 s, s514a); `context`, `getstate`/`putstate` and partial-byte `add_bits` die by name (see [`not-supported.md`](not-supported.md)) |
| `Storable` | perl's own binary format in both directions (#2948, s514a): `freeze`/`nfreeze`/`thaw`/`dclone`/`store`/`nstore`/`retrieve`/`*_fd`/`lock_*`; sharing, cycles, blessed, regexps, tied containers; byte-identical to perl for canonical structures except booleans and upgraded latin-1 strings.  Hooks (`STORABLE_freeze`), CODE and GLOB items die by name (see [`not-supported.md`](not-supported.md)) |
| `IO` | the XS half of `IO::Handle`; `sync`, `blocking` and `ungetc` die by name, since they need system features PCL does not have |
| `mro` | C3 method resolution only, which is what PCL's object system always uses (see "`mro` pragma" in [`not-supported.md`](not-supported.md)) |
| `version` | version parsing in Perl |

**The real module is Perl, but PCL cannot run it unchanged:**

| module | why |
|---|---|
| `Carp` | the real one needs call-stack introspection PCL does not fully model. `croak` and `carp` keep the message but do not append " at FILE line N" |
| `Config` | a fixed set of values for 64-bit Linux with SBCL |
| `Errno` | the real one installs its constants through a symbol-table loop; these are the same constants, generated from perl |
| `English` | the real one aliases names with glob assignments PCL cannot compile |
| `File::Spec`, `File::Spec::Functions` | the real `File::Spec` only dispatches on the operating system; this one inherits the Unix methods directly |
| `IO::Handle` | the real file with `autoflush`, `printflush` and `binmode` changed: the real `autoflush` restores the selected handle from a `DESTROY` method, which PCL never calls (see below) |
| `Try::Tiny` | the real one runs `finally` blocks from a `DESTROY` method; this one calls them directly at the same points |
| `Math::BigInt::Calc` | see below |
| `PerlIO::Layer` | a minimal `find`, since PCL has no PerlIO layer objects |
| `warnings` | the query and emit functions (`warnings::enabled` and the rest), with every category reported as enabled; `use warnings` itself is a pragma and never loads it |
| `Test::More` | prototypes only: it is never run (the runtime supplies Test::More, step 3 above), but the compiler reads the assertion functions' prototypes from it |

## Where a new replacement belongs

Prefer a `.pm` in `lib/` whenever the module can be written in Perl: it is
compiled like user code, there is no Lisp to maintain, and it follows the
project's layer rule (module behaviour lives in `lib/`, never in the
compiler or the runtime). When a module needs something no Perl can say (a
clock, a system call), split it: add the small primitive to the runtime,
and write the rest of the module in Perl on top of it. `Time::HiRes` is the
pattern to copy: four primitives in the runtime, everything else in
`lib/Time/HiRes.pm`.

Only a module that cannot be written in Perl at all belongs in Lisp, as an
extension loaded with `p-load-extension` ([`extensions.md`](extensions.md)).

A `lib/` replacement is an ordinary module as far as caching goes: a
script's dependency manifest covers it, so editing it re-transpiles the
programs that use it ([`caching.md`](caching.md) §2).

## Replacements that work around a gap in PCL

Most of `lib/` stands in for XS. Some files are instead near-verbatim copies
of the real pure-Perl module, patched only to avoid a place where PCL
behaves differently from perl. Each should be deleted once PCL closes the
gap:

| file | the gap | the patch | delete when |
|---|---|---|---|
| `lib/IO/Handle.pm` | PCL never calls `DESTROY`, so `SelectSaver` never restores the selected handle | `autoflush` and `printflush` save and restore the selection explicitly | `DESTROY` runs at scope exit |
| `lib/Try/Tiny.pm` | the same: `finally` blocks run from `DESTROY` | `finally` blocks are called directly | `DESTROY` runs at scope exit |
| `lib/Math/BigInt/Calc.pm` | the real module detects the platform's integer size with loops that run until a product loses precision. PCL's integers never lose precision (see "Integers are unbounded" in [`not-supported.md`](not-supported.md)), so the loops never end and loading hangs | the two loops are replaced by the values perl computes on this platform (`BASE_LEN = 9`, `USE_INT = 1`), which are correct at any size because PCL's arithmetic is exact; the file's header has the details | PCL gains a native-precision mode, or Math::BigInt stops probing |

**General rule:** any CPAN module that probes the platform's numeric
precision with a loop (multiply until inexact, shift until zero) will hang
under PCL's unlimited-precision integers, and a patched copy in `lib/` is
the per-module way out.

## Facts overlays: `lib/PCL/Facts/` (task #2878)

**What it is.** Some modules install a sub, or give it a prototype, only by
RUNNING code at load time: a glob-assign loop over a list of names
(File::Path's `_IS_VMS`), a string `eval` that builds a sub (Capture::Tiny's
`capture`, Text::Wrap's `REGEXPS_USE_BYTES`), a computed export list
(`@EXPORT_OK = keys %api`).  A static parse cannot see any of it, so a later
bareword call is read wrong: `_IS_VMS + 1` as a list-operator call,
`capture { ... }` not as a block-form call.  A facts overlay supplies those
facts in advance, in perl's own syntax for them: FORWARD DECLARATIONS.  It
replaces nothing -- the real module still runs and installs the real sub, so
every VALUE stays the module's -- and it defines nothing at run time.

**Where it lives.** The overlay of module `Foo::Bar` is the module
`PCL::Facts::Foo::Bar`, i.e. the file `PCL/Facts/Foo/Bar.pm` under any root
of the search list (the program's `-I`, `PERL5LIB`, `use lib` dirs, PCL's own
`lib/`, perl's directories), found by the same resolver as every module,
first hit wins.  PCL's shipped overlays are in `lib/PCL/Facts/` (the
installer copies `lib/`); a test fixture keeps its own under its `-I` root
(`Pl/t/lib/PCL/Facts/`).  For a module's OWN unit the root the module was
found in is tried first.  A module without an overlay costs one failed
lookup.

**What it may contain** (anything else DIES naming the file and line -- an
overlay is declarations, never code):

    package Foo::Bar;                    # exactly one, the module's own name
    sub NAME (PROTO);   sub NAME;        # forward declarations, no body
    our @EXPORT = qw(...);  our @EXPORT_OK = qw(...);
    our %EXPORT_TAGS = (tag => [qw(...)], ...);
    1;                                   # plus comments and POD

**The readability rule.** Every declaration group carries a comment saying
what the real module does and why a static parse cannot see it:

    package File::Path;
    # File::Path installs these four at BEGIN time in a loop over a name list
    # (`*{"_IS_\U$_"} = $^O eq $_ ? sub () { 1 } : sub () { 0 }`), which a
    # static parse cannot see.  The declaration supplies the prototype; the
    # module supplies the value.
    sub _IS_VMS ();  sub _IS_MACOS ();  sub _IS_MSWIN32 ();  sub _IS_OS2 ();
    1;

**The merge rule.** `Pl::Parser::_extract_module_prototypes` reads the
overlay through the SAME walk that reads the module's source and merges the
two: an overlay prototype fills an ABSENCE; the export lists and tags are
UNIONED (an import list's `:tag` / `:DEFAULT` expands through them).  A
module on the walk's cost skip list (`File::*`, `IO::*`, ...) gets the
overlay's facts alone.  The facts then reach call sites through the two
existing paths: a `use` registers them at its own position (#2871, so a call
ABOVE the `use` does not see them), and the module's own unit registers
them at its `package` statement.  The overlay's bytes (or "none") are part of
the prototype cache key, and a FOUND overlay is a dependency of every cached
walk, script manifest and module sidecar, so editing or deleting one
re-transpiles what it affects.  An absent overlay is deliberately not
recorded (it would put a `missing:` entry for every module into every
manifest, re-probed at every cached start-up), so a NEW overlay for a module
already in the cache takes effect at the next generation bump -- a shipped
overlay arrives with one -- or after the cache is cleared.

**The conflict rule.** If the module's source declares the same name with a
DIFFERENT prototype, the transpile dies naming both files -- on the `use`
path and when the module file itself is transpiled (its own unit; #2954
made that path fire, Pl/t/facts-overlay-01.t row 7): the overlay is
wrong (or the module changed under it) and must be fixed, never preferred.

**How to add one.** (1) Find the name: the #2610 detector
(`PCL_DETECT_TABLE=1 PCL_DETECT_LOG=FILE`, `detector-measurement-s513d.md`)
logs every call site whose parse a compile-time install contradicted, with
the installed prototype.  (2) Read the real module to see what installs it
and with which prototype.  (3) Write `lib/PCL/Facts/<Module/Path>.pm` with
the declaration and its comment; `tools/tag-license` it.  (4) Re-run the
detector: the name must be GONE from the log -- if it is still there as a
disagreement, the overlay's prototype is wrong.  Shipped today: File::Path,
Sub::Quote, Moo::_Utils, IO::Socket::UNIX, Text::Wrap, Capture::Tiny.

## Proposed, not built

An earlier version of this page proposed a single provider table
(`*pcl-module-providers*`) and a `cl/modules/` directory for modules
written in Lisp. Neither was built: the one module it was meant for,
`Time::HiRes`, turned out to need only four primitives and is now plain
Perl, and the Test::More case is handled by step 3 above. The proposal is
kept in [`history/shipped-modules-proposal.md`](history/shipped-modules-proposal.md).

## See also

- [`extensions.md`](extensions.md): the runtime extensions (`pack`, `mro`,
  `warnings`, XS) and `p-load-extension`.
- [`caching.md`](caching.md): how modules, including `lib/` replacements,
  are cached.
- `cl/pcl-runtime.lisp`: `p-use`, `*p-xs-only-modules*`,
  `*p-pcl-provided-modules*`, `p-load-extension`.
