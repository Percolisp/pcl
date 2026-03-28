# PCL: Persistent Transpiler Subprocess

**Written:** 2026-03-28

---

## Problem

Every `eval "string"` call currently spawns a fresh `perl pl2cl` process:

```
SBCL → fork/exec perl → load Perl + PPI + Pl::Parser → transpile → exit
                         ~500ms startup overhead
```

For test files with hundreds of eval calls this is fatal:
- `cmpchain.t`: 656 eval calls → ~5.5 min → TIMEOUT (skipped)
- `signatures.t`: 734 eval calls → ~6 min → TIMEOUT (skipped)
- `list.t`: 1 pathological eval (100k-nested expr) → still needs special handling

Same problem affects `require` (module loads), but those are cacheable so less visible.

## Solution

Keep one `perl pl2cl --server` process alive for the lifetime of the SBCL image.
Send requests over stdin/stdout pipes using a simple length-prefixed protocol.

```
SBCL → write request to pipe → perl transpiles → read response from pipe
       no Perl startup; ~5–10ms per call (just parse+transpile)
```

---

## Protocol

All communication is UTF-8 encoded bytes. The Perl server runs with
`binmode(STDIN, ':raw')` and `binmode(STDOUT, ':raw')` to avoid newline
translation on Windows (and for future portability).

### Request (SBCL → perl stdin)

```
<pkg-name>\n
<byte-count>\n
<perl-code: exactly byte-count UTF-8 bytes, no terminator>
```

- `pkg-name`: current CL package name (e.g. `MAIN`, `Foo::Bar`). ASCII-safe
  (package names can't contain non-ASCII in standard Perl).
- `byte-count`: decimal integer, length of the UTF-8 encoded perl-code in bytes.
- `perl-code`: the Perl source string to transpile, exactly byte-count bytes.

### Response (perl stdout → SBCL)

```
ok\n                         (on success)
<byte-count>\n
<cl-code: exactly byte-count UTF-8 bytes>

  — OR —

err\n                        (on transpile error)
<byte-count>\n
<error-message: exactly byte-count UTF-8 bytes>
```

The server **never exits** on error — it sends an `err` response and waits for
the next request. The SBCL side raises a CL `error` condition on `err`, which
`p-eval`'s `handler-case` catches and stores in `$@`.

---

## Changes to `pl2cl`

### New option variable (after `my $eval_pkg`)

```perl
my $server_mode = 0;   # Persistent transpiler server mode
```

### Register in GetOptions

```perl
'server' => \$server_mode,
```

### New server entry point (after GetOptions block, before file/stdin logic)

```perl
if ($server_mode) {
    _run_server();
    exit 0;
}
```

### New `_run_server` sub (add near end of file, before final `print "\n"`)

```perl
sub _run_server {
    # Persistent transpiler server: read requests from stdin, write CL to stdout.
    # Request:  "<pkg>\n<byte-count>\n<perl-code bytes>"
    # Response: "ok\n<byte-count>\n<cl-code bytes>"
    #        or "err\n<byte-count>\n<error-message bytes>"
    use Encode qw(encode decode);
    binmode(STDIN,  ':raw');
    binmode(STDOUT, ':raw');

    while (1) {
        # Read package name
        my $pkg = <STDIN>;
        last unless defined $pkg;
        chomp $pkg;

        # Read byte count
        my $len = <STDIN>;
        last unless defined $len;
        chomp $len;

        # Read exact bytes of Perl code
        my $code_bytes = '';
        my $got = read(STDIN, $code_bytes, $len);
        last unless defined $got && $got == $len;

        my $code = decode('UTF-8', $code_bytes, Encode::FB_CROAK);

        # Transpile
        my $cl = eval {
            my $output = Pl::Parser->parse_code($code);
            # Apply eval-pkg preamble (replaces (in-package :pcl))
            my $preamble = build_eval_preamble($pkg);
            $output =~ s/\(in-package :pcl\)/$preamble/;
            $output
        };

        my $response;
        if ($@) {
            my $err_bytes = encode('UTF-8', "$@");
            $response = "err\n" . length($err_bytes) . "\n" . $err_bytes;
        } else {
            my $cl_bytes = encode('UTF-8', $cl);
            $response = "ok\n" . length($cl_bytes) . "\n" . $cl_bytes;
        }

        print STDOUT $response;
        STDOUT->flush;
    }
}
```

**Notes:**
- `Pl::Parser->parse_code` is already stateless — safe to call repeatedly.
- `eval { }` around transpilation catches PPI parse errors and code-gen exceptions.
- Each response is flushed immediately after writing.
- Loop exits cleanly if stdin closes (SBCL image exits → pipe EOF).

---

## Changes to `cl/pcl-runtime.lisp`

### 1. New defvars (near `*p-eval-string-cache*`, around line 261)

```lisp
(defvar *p-transpiler-process* nil
  "Persistent pl2cl server process, or nil if not yet started.
   Started lazily on first eval-string call. Restarted automatically if it dies.")
```

Does NOT need to be exported — it's an internal implementation detail.

### 2. New `p-ensure-transpiler` function (add before `p-transpile-string`)

```lisp
(defun p-ensure-transpiler ()
  "Return the live transpiler process, starting or restarting it if needed."
  (unless *pcl-pl2cl-path*
    (error "pl2cl path not set — cannot start transpiler server"))
  (when (or (null *p-transpiler-process*)
            (not (sb-ext:process-alive-p *p-transpiler-process*)))
    (when *p-transpiler-process*
      ;; Clean up dead process handle
      (ignore-errors (sb-ext:process-close *p-transpiler-process*)))
    (setf *p-transpiler-process*
          (sb-ext:run-program
           "perl"
           (list (namestring *pcl-pl2cl-path*) "--server")
           :input  :stream
           :output :stream
           :error  nil        ; discard stderr (transpile errors go via protocol)
           :wait   nil        ; don't block — server runs concurrently
           :search t
           :external-format :utf-8)))
  *p-transpiler-process*)
```

**Why `:error nil`:** Transpile errors are reported via the `err\n` protocol response,
not stderr. Discarding stderr keeps the SBCL output clean. If debugging is needed,
change to `:error :output` or `:error *error-output*`.

### 3. Replace `p-transpile-string` (current body uses `run-program` per call)

```lisp
(defun p-transpile-string (perl-code pkg-name)
  "Transpile a Perl string to CL code via the persistent pl2cl server.
   Returns the CL text string, or signals an error on failure."
  (let* ((proc (p-ensure-transpiler))
         (in   (sb-ext:process-input  proc))
         (out  (sb-ext:process-output proc))
         ;; Encode code as UTF-8 bytes for length prefix
         (code-bytes (sb-ext:string-to-octets perl-code :external-format :utf-8)))
    ;; Send request
    (write-string pkg-name in) (write-char #\Newline in)
    (write-string (princ-to-string (length code-bytes)) in) (write-char #\Newline in)
    (write-sequence code-bytes in)
    (force-output in)
    ;; Read response
    (let* ((status    (read-line out))
           (resp-len  (parse-integer (read-line out)))
           (resp-buf  (make-array resp-len :element-type '(unsigned-byte 8))))
      (read-sequence resp-buf out)
      (let ((resp-str (sb-ext:octets-to-string resp-buf :external-format :utf-8)))
        (if (string= status "ok")
            resp-str
            (error "pl2cl server: ~A" resp-str))))))
```

**Key differences from current implementation:**
- No `run-program` per call — reuses `*p-transpiler-process*`
- `force-output` ensures the request is flushed to the pipe
- `read-line` reads `ok`/`err` + byte count; `read-sequence` reads exact bytes
- Byte-count protocol handles multiline Perl code correctly
- On `err`, raises a CL `error` condition (caught by `p-eval`'s `handler-case`)

**Encoding note:** `sb-ext:string-to-octets` / `sb-ext:octets-to-string` handle
Unicode correctly. The server and client must agree on UTF-8 throughout.

### 4. Stream modes

`sb-ext:run-program` with `:external-format :utf-8` makes SBCL treat the
process streams as UTF-8 character streams. This means `write-string`,
`read-line`, and `read-sequence` on char arrays work directly — no manual
encoding/decoding needed on the SBCL side.

**BUT:** `read-sequence` on a `(unsigned-byte 8)` array and on a `string` behave
differently. The protocol uses byte counts but character streams.

**Simpler approach:** just use `:external-format :utf-8` and work entirely in
characters. The `length` in the protocol is then character count, not byte count.
This is fine as long as both sides agree (pure character protocol).

Revised protocol to avoid byte/char confusion:

```
Request:  "<pkg>\n<char-count>\n<perl-code: exactly char-count chars>"
Response: "ok\n<char-count>\n<cl-code: exactly char-count chars>"
       or "err\n<char-count>\n<error: exactly char-count chars>"
```

Revised `_run_server` Perl side uses `length($code)` (character count) after
decoding from UTF-8 bytes, and sends `length($cl)` (character count).

Wait — if SBCL uses character counts and Perl sends byte counts, they'll
disagree on strings with non-ASCII characters. This is the core tension.

**Resolution:** Use **byte counts** throughout, with UTF-8 encoding on both sides.
SBCL side: `sb-ext:string-to-octets`/`sb-ext:octets-to-string` explicitly.
Perl side: `use Encode; encode/decode UTF-8`. Both use raw binary streams.
`read-sequence` on `(unsigned-byte 8)` array, then convert.

This is already reflected in the code above. Keep it bytes throughout.

---

## Error Handling Summary

| Scenario | Behaviour |
|----------|-----------|
| Transpile error (parse fail) | Server sends `err\n...\n<message>`, continues running |
| Server process dies unexpectedly | `p-ensure-transpiler` restarts it on next call |
| `*pcl-pl2cl-path*` not set | Error raised immediately in `p-ensure-transpiler` |
| Partial read from dead server | `read-sequence` returns short; process restart handles it |
| list.t 100k-nested expression | Server will take very long / OOM; list.t stays in SKIP |

---

## Lifecycle

- **Start:** Lazy — first call to `p-transpile-string` starts the server.
- **Restart:** Transparent — `p-ensure-transpiler` restarts if `process-alive-p` is false.
- **Shutdown:** Not needed — OS kills the child when the SBCL image exits.
  Alternatively, close the pipe to send EOF, which exits the server loop cleanly.

No explicit shutdown is required. The server exits when SBCL exits (pipe EOF).

---

## Test Updates

### Files to remove from sweep SKIP list

- `cmpchain.t`: 656 eval calls × ~10ms = ~6.5s → well within 90s timeout
- `signatures.t`: 734 eval calls × ~10ms = ~7.3s → within 90s timeout (keep skip_all
  for signatures enforcement; remove skip_all once signatures feature is implemented)

### `list.t` stays in SKIP

Line 275: `my $e = "1"; $e = "(1,$e)" for 1..100_000; $e = "() = $e"; eval $e;`
This builds a string ~1MB of deeply nested `(1,(1,(1,...)))`. Transpiling would
take seconds even with the persistent server, and might OOM PPI. Keep skipped.

### New tests in `Pl/t/eval-01.t`

Add a test that verifies the persistent process is reused (not respawned):

```perl
# Test: persistent process — multiple evals don't each take 500ms
{
    my $t0 = time();
    for (1..20) {
        run_pl("my \$r = eval \"$_ + 1\"; print \$r, \"\\n\";");
    }
    my $elapsed = time() - $t0;
    ok($elapsed < 30, 'persistent subprocess: 20 evals complete in <30s');
}
```

(This is a timing test — fragile, but gives a sanity check. Alternatively,
just verify the eval results are correct and trust the implementation.)

---

## Files to Change

| File | Change |
|------|--------|
| `pl2cl` | Add `--server` flag + `_run_server` sub |
| `cl/pcl-runtime.lisp` | Add `*p-transpiler-process*`, `p-ensure-transpiler`; replace `p-transpile-string` |
| `sweep-perl-tests.pl` | Remove `cmpchain.t` from `@SKIP` |
| `perl-tests/signatures.t` | Remove skip_all once signatures enforcement is implemented (separate task) |

No changes to `Pl/Parser.pm`, `Pl/PExpr.pm`, `Pl/ExprToCL.pm`, or `Pl/t/eval-01.t`
(existing eval-01.t tests cover correctness; the server is a transparent optimization).

---

## Implementation Order

1. **`pl2cl --server`**: Implement and test manually:
   ```bash
   echo -e "MAIN\n12\nprint 42, \"\\n\";" | perl pl2cl --server
   # Should print: ok\n<len>\n(in-package...)(p-print 42 "\\n")
   ```

2. **`p-ensure-transpiler` + new `p-transpile-string`**: Replace the
   current implementation in pcl-runtime.lisp.

3. **Smoke test**:
   ```bash
   prove -v Pl/t/eval-01.t
   perl sweep-perl-tests.pl --jobs 1 perl-tests/cmpchain.t
   ```

4. **Remove `cmpchain.t` from SKIP**, run full sweep.

5. **Verify no regression**: Full `prove -j8 Pl/t/` must stay green.

---

## Performance Expectation

| Scenario | Before | After |
|----------|--------|-------|
| First eval call (server start) | ~500ms | ~200ms (Perl startup once) |
| Subsequent eval calls | ~500ms each | ~5–15ms each |
| cmpchain.t (656 calls) | ~7.5 min → TIMEOUT | ~10s → passes |
| signatures.t (734 calls) | ~8 min → TIMEOUT | ~11s → passes |
| list.t line 275 | hangs → SKIP | still SKIP (pathological size) |

The ~200ms first-call overhead is the one-time Perl+PPI startup. All subsequent
calls in the same SBCL session are just IPC round-trips.
