# Socket support — implementation plan (socket-only)

**Status:** planned, not started. `<>`-diamond + `$^I` shipped 2026-06-27
(commit 8a305a3); sockets are the next clean I/O item per
`project_io_subprocess_features`.

**De-risk (verified 2026-06-27):** SBCL's `sb-bsd-sockets` does a full
single-process TCP loopback (server bind/listen + client connect + accept +
bidirectional read/write) with **no fork** — see the spike below. So the whole
feature is testable in one SBCL process; fork is *not* a prerequisite.

```lisp
(require :sb-bsd-sockets)
(let ((srv (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
  (setf (sb-bsd-sockets:sockopt-reuse-address srv) t)
  (sb-bsd-sockets:socket-bind srv #(127 0 0 1) 0)       ; port 0 = ephemeral
  (sb-bsd-sockets:socket-listen srv 5)
  (multiple-value-bind (host port) (sb-bsd-sockets:socket-name srv)
    (let ((cli (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
      (sb-bsd-sockets:socket-connect cli #(127 0 0 1) port)
      (let ((acc (sb-bsd-sockets:socket-accept srv)))
        (let ((cs (sb-bsd-sockets:socket-make-stream cli :input t :output t :buffering :none))
              (as (sb-bsd-sockets:socket-make-stream acc :input t :output t :buffering :none)))
          (write-line "hi" cs) (finish-output cs)
          (read-line as))))))   ; => "hi"
```

---

## Architecture decision: split across the three layers (CLAUDE.md §9a)

- **`lib/Socket.pm` shim** (module behaviour — *addressing*): the constants
  (`AF_INET`/`PF_INET`/`SOCK_STREAM`/`SOCK_DGRAM`/`SOL_SOCKET`/`SO_REUSEADDR`/
  `INADDR_ANY`/`INADDR_LOOPBACK`/`SOMAXCONN`/…), and the pack/unpack address
  helpers `inet_aton`/`inet_ntoa`/`sockaddr_in`/`pack_sockaddr_in`/
  `unpack_sockaddr_in`/`sockaddr_un`/`pack_sockaddr_un`. These are *pure Perl*
  built on `pack`/`unpack` — PCL transpiles them like user code. This is the
  right layer: address packing is module data, not language.
- **Runtime core builtins** (genuine Perl core): `socket`, `bind`, `connect`,
  `listen`, `accept`, `send`, `recv`, `shutdown`, `getsockname`, `getpeername`,
  `getprotobyname`, `socketpair`. These take fds / socket objects and *already-
  packed* sockaddr byte-strings — exactly Perl's core calling convention — and
  drive `sb-bsd-sockets`. `close` already works.
- **Parser/codegen**: nothing socket-specific. Just the generic 4-edit builtin
  registration (Config param spec + `%RUNTIME_NAMES` + runtime impl + `:export`).
  NB `socket`/`bind`/`connect`/… are already listed as recognised builtin names
  in `Pl/PExpr/Config.pm` (~line 432) — confirm their param specs.

**Sanity (smell test §9a):** no module name leaks into `Pl/`/`cl/`. `Socket` the
module lives in `lib/Socket.pm`; the runtime only knows the *core* builtins.

---

## Scope, in order

1. **AF_INET + SOCK_STREAM (TCP)** — client *and* server. Covers the bulk of
   real code and `t/io/socket.t`. **Do this first; ship it.**
2. **AF_UNIX (local) stream sockets** — `sockaddr_un` + `local-socket`. Small
   add once TCP works.
3. **SOCK_DGRAM (UDP)** — `send`/`recv` with explicit peer addr;
   `socket-receive` returns the sender address for `recv`.
4. **socketpair** — `sb-bsd-sockets` has no direct socketpair; use
   `sb-posix:socketpair` (raw fds) + `sb-sys:make-fd-stream`, or a
   bound-loopback emulation. Defer until 1–3 done.

---

## Filehandle integration (the one non-obvious part)

A Perl socket *is* a filehandle: after `connect`/`accept` you `print $sock` /
`<$sock>` / `close $sock`. But `bind`/`connect`/`listen`/`accept` operate on the
**socket object**, while `print`/readline need a **stream**. Plan:

- `socket($fh,…)` stores the `sb-bsd-sockets` **socket object** as the
  filehandle value (`%p-install-fh fh obj` — works for both a lexical `$fh` box
  and a bareword symbol, same as `open`).
- New `%p-get-socket fh` → resolves `$fh` to the stored socket object (mirror of
  `p-get-stream`'s designator handling) for the bind/connect/listen/accept/…
  builtins.
- **Extend `p-get-stream`**: when the resolved filehandle value is a
  `sb-bsd-sockets:socket`, lazily `socket-make-stream` it (`:input t :output t`)
  and **cache** it in a side table `*p-socket-streams*` (eq: socket-obj →
  stream), returning the cached stream thereafter. Caching is essential —
  re-making the stream would lose buffered data.
- This makes `print $sock`, `printf $sock`, `<$sock>`, `read`, `eof`, `close`
  all "just work" via the existing stream paths (incl. the new per-handle `$.`).
- `close $sock`: close the cached stream if present (which closes the fd);
  else `socket-close` the object. Drop it from `*p-socket-streams*`.
- **Buffering / `$|`**: start with `:buffering :none` to avoid flush bugs in
  line protocols (every write hits the wire immediately). Revisit to honour
  `$|`/`autoflush` + `finish-output` if throughput matters.

---

## Packed-sockaddr parsing in the runtime

`Socket.pm` packs a standard `struct sockaddr_in` (16 bytes):
`pack('S n a4 x8', AF_INET, $port, $inaddr)` =
family(native short) · port(**network-order** short, `n`) · addr(4 bytes) · 8 pad.

Runtime `bind`/`connect` receive that byte-string and parse:
- `port  = (logior (ash (char-code b2) 8) (char-code b3))`   ; network order
- `addr  = #((char-code b4) (char-code b5) (char-code b6) (char-code b7))`
- family: bytes 0–1 (native order — LE on x86-64; only needed to distinguish
  AF_INET vs AF_UNIX). For AF_UNIX (`sockaddr_un`): family then a NUL-terminated
  path → `sb-bsd-sockets:local-socket` + path string.

`getsockname`/`getpeername`/`accept` go the other way: take `(host-vector port)`
from `socket-name`/`socket-peername`/`socket-accept` and **re-pack** a
sockaddr_in byte-string to hand back to Perl (so `unpack_sockaddr_in` works).
Implement the re-pack in Lisp (or, cleaner, return the pieces and let Socket.pm
pack — but the core builtin must return the packed form, so pack in Lisp).

Watch: PCL's `pack`/`unpack` for `S`/`n`/`a4`/`x8` must round-trip. `n` is
network order (unambiguous); `S` is native — confirm `cl/pack-impl.pl` packs `S`
in the platform's native order so the runtime's family read agrees. (Family is
barely used; port+addr are the load-bearing fields.)

---

## The 4-edit pattern, per builtin

For each of `socket bind connect listen accept send recv shutdown getsockname
getpeername getprotobyname socketpair`:

1. **`Pl/PExpr/Config.pm`** — param spec in `known_no_of_params`
   (e.g. `socket => 4`, `bind => 2`, `connect => 2`, `listen => 2`,
   `accept => 2`, `send => [3,4]`, `recv => 4`, `shutdown => 2`,
   `getsockname => 1`, `getpeername => 1`, `getprotobyname => 1`). Several are
   already in the recognised-builtin comment list (~Config.pm:432) — wire specs.
   **Note `accept(NEW, SERVER)` and `recv(SOCK, $buf, LEN, FLAGS)` write
   through their scalar arg** — they need the lvalue/quote-the-FH macro
   treatment like `read`/`open` (see `%p-fh-arg` / how `read` fills its buffer
   in place via `box-set`).
2. **`Pl/ExprToCL.pm` `%RUNTIME_NAMES`** (~line 98) — add the name so it codegens
   `(p-<name> …)`.
3. **`cl/pcl-runtime.lisp`** — the impl (object/stream plumbing above), plus
   `(require :sb-bsd-sockets)` near the top.
4. **`:export`** in `defpackage :pcl` — export each `p-<name>` (and `$ARGV`-style
   any new specials). Generated user-package code `(:use :pcl)` needs them.

`socket`/`bind`/`connect` that take a bareword FH (`socket(SOCK, …)`) need the
bareword auto-quoted exactly like `open`/`read` — route the FH arg through the
`%p-fh-arg` macro so `SOCK` becomes the right symbol.

---

## getprotobyname

`getprotobyname('tcp')` → 6, `'udp'` → 17. A tiny static table in the runtime
(or `/etc/protocols` via `sb-posix`) is enough; real code only ever asks for
tcp/udp. `sb-bsd-sockets` takes `:protocol :tcp/:udp` keywords, so map the
number back to a keyword when constructing the socket.

---

## Test plan

- New `Pl/t/socket-01.t`: single-process TCP loopback (server on `127.0.0.1`
  port 0 / ephemeral, client connect, accept, `print`/`<$sock>` both ways,
  `close`), comparing PCL output to real perl running the same script. The
  ephemeral-port + same-process pattern keeps it deterministic and fork-free.
- Then point at `t/io/socket.t` (and later `socketpair.t`) via the
  CWD=perl-`t/` harness. Note some of `t/io/socket.t` uses `fork` for the
  server/client split — the **single-process ephemeral pattern above sidesteps
  fork** for our own regression test; the perl file's fork parts stay gated on
  the fork work.

## Open questions / risks

- **Blocking accept in one thread:** fine for the loopback pattern (connect
  queues via the listen backlog before we call accept), but a server that
  `accept`s before any client exists will block. Acceptable — that's Perl
  semantics too. Don't add threads.
- **Autoflush:** default `:buffering :none` sidesteps it; wire `$|`/`finish-
  output` only if a real test needs buffered socket writes.
- **Errno on failure:** `bind`/`connect` failures should set `$!` (EADDRINUSE,
  ECONNREFUSED) and return false — wrap `sb-bsd-sockets` condition → errno like
  the file-ops do (`%pcl-save-errno`).
- **IPv6 (`AF_INET6`, `sockaddr_in6`):** out of scope for v1; add later via
  `inet6-socket` if needed.
