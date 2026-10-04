---
name: runt and runpcl helper scripts
description: tools/runt (moved from ./runt in s471b, #1301) for perl-tests/*.t files; ./runpcl for arbitrary Perl files or stdin — both transpile+run through PCL with filtered output
type: reference
originSessionId: 2068cc37-5024-4b5f-b13e-b25182cffed4
modified: 2026-08-12T06:26:56.455Z
---
`tools/runt <name>` (or `tools/runt <name>.t`) from the project root.  **Since s471b (2026-09-06, #1301) the three helpers live in `tools/`: `tools/runt`, `tools/clt`, `perl tools/sweep-perl-tests.pl`; `./runt`, `./clt`, `perl sweep-perl-tests.pl` no longer exist** (`run-perl-test.pl` deleted).  `./runpcl`, `./pl2cl`, `./pcl` stay at the root.

**What it does:**
1. Transpiles `perl-tests/<name>.t` → `/tmp/<name>.lisp` (using `--no-cache --lenient-ppi`, run from `perl-tests/` dir so `chdir "t"` in tests works)
2. Runs SBCL with runtime + testlib + generated lisp
3. Saves raw output to `/tmp/<name>.out`
4. Prints filtered output (strips `;` lines, STYLE-WARNING, LEADING blanks).
   **Since s386 program blank lines are PRESERVED** — the old blanket
   blank-line strip falsified byte-compares vs perl for any output with
   `\n\n`.  Byte-diffs through runt/runpcl are now valid.

**Usage examples:**
```
tools/runt loopctl       # run loopctl.t
tools/runt loopctl.t     # same
```

**Intermediate files:**
- `/tmp/<name>.lisp` — generated CL code (inspect with Read or grep)
- `/tmp/<name>.out`  — raw SBCL output (unfiltered)
- `/tmp/<name>.pl2cl.err` — transpiler stderr (if transpile fails)

Use instead of the long `cd perl-tests && perl -I... pl2cl ... | sbcl ...` one-liner.

## runpcl — run arbitrary Perl files through PCL

`./runpcl file.pl` or `echo 'code' | ./runpcl`

**What it does:**
1. Accepts a Perl file argument OR reads from stdin (writes to temp file)
2. Transpiles and runs through PCL with the same SBCL flags as `tools/runt`
3. Filters output the same way as `tools/runt`
4. Cleans up temp files automatically

**Usage examples:**
```
./runpcl /tmp/test.pl        # run a file
echo 'print 1+1' | ./runpcl  # run from stdin
cat > /tmp/t.pl << 'EOF'    # write multi-line snippet
...
EOF
./runpcl /tmp/t.pl
```

Use `runpcl` for quick Perl snippet testing instead of the brittle `./pl2cl | sbcl --load /dev/stdin` pattern.
