# Security review of the user-facing commands (s486, 2026-09-16, Fable)

USER ask: "review the used commands from a security perspective — are they easy
to fool?  With commands I mean things like the `pcl` command."  This is the
answer, from reading `pcl`, `pl2cl`, `runpcl`, `tools/runt`, `tools/clt`,
`tools/install-pcl`, `tools/lib/PCLSbcl.pm`, `tools/lib/PCLPaths.pm`,
`Pl/t/PCLCore.pm`, the cache code and the drop emitter, plus live probes
against perl 5.40.3.  Numbered findings carry a task; the rest are facts to
keep true.

## 0. The threat model, stated once

A Perl program is code.  Running it under `pcl` can do everything running it
under `perl` can, so "the script did something bad" is never a PCL finding.
What CAN be a PCL finding:

1. **cross-user**: another local user reads or changes what a PCL command
   writes (temp files, sockets, caches) and thereby reads your program's
   output or runs code as you;
2. **weakened program defences**: a Perl program that correctly avoids the
   shell (`system LIST`, `open '-|', LIST`) is exposed to injection because
   PCL implements the form differently;
3. **compile-only is not run-only**: `pcl -c` / `pl2cl` executing something
   from an untrusted file when the user only asked for a translation;
4. **footguns**: a command that destroys something outside its own scope.

## 1. What holds (verified)

- **List forms never reach a shell.**  Probed under perl and `pcl` with the
  same file: `system('echo', 'L1;echo INJECTED')`, `open(my $p, '-|', 'echo',
  'L3;echo INJECTED')`, `open(my $w, '|-', 'cat', '-n')` print the literal
  text under both; string forms (`system "…"`, backticks, 2-arg
  `open F, "cmd|"`) go through `/bin/sh` under both, as perl documents.  A
  3-arg `open '<', '|cmd|'` and `open '<', '>/dev/null'` are refused as file
  names under both.  One divergence, not a security one: the 2-arg pipe-open
  whose reader never reads makes the child report `echo: I/O error` on stderr
  under PCL (perl is silent) — task #1801.
- **Generated Lisp is reader-safe for strings.**  A double-quoted string, a
  heredoc, a hash key and a single-quoted string carrying `"`, `\`, `#.(…)`,
  `|x|`, `;;`, `#| |#` and `)(` all round-trip byte-identically
  (`p-esc` keeps escapes as data; `cl_string_literal` escapes the rest).
- **`pcl` builds its SBCL command as a LIST and `exec`s it** (no shell); its
  temp files come from `File::Temp` (`pcl_<pid>_XXXX`, mode 0600, unlinked in
  `END`); `-e` code is written to such a file; `-I` directories are passed to
  `pl2cl` as separate `-I` arguments.  `pcl --version` shells out with
  `\Q…\E`-quoted paths.
- **`pcl -c` executes nothing from the file.**  It transpiles, prints
  `syntax OK` and exits without loading the Lisp — unlike `perl -c`, which runs
  `BEGIN` blocks and `use`d modules.  `pl2cl FILE` is likewise a pure
  translation: the compiler reads modules in `@INC` for prototypes and
  exports but evaluates nothing from the source except numeric-literal text
  (`eval($num)` in `Pl/ExprToCL.pm`, on tokens PPI has already classified as
  numbers).  Worth stating in the README: **transpiling untrusted Perl runs
  none of it** — the exceptions are `pl2cl --bundle`/`--executable`, which
  compile and therefore run `use`d modules at build time, as documented.
- **The cache root is a real gate.**  `~/.pcl-cache` (or `$PCL_CACHE_DIR`) is
  created 0700; an existing root not owned by the caller or with any
  group/other write bit is REFUSED with the reason (`PCLPaths::
  cache_dir_problem`, mirrored by `%p-cache-dir-problem` in the runtime).
  Compiled modules, eval transpiles, prototype facts, saved cores and XS
  artifacts all live under it, so a poisoned cache needs the user's own
  write access.  The parent directories are not checked (ir-spec §9.2b says
  so): a root placed inside a world-writable non-sticky directory is the
  user's own choice.
- **The string-eval transpiler is a pipe, not a socket.**  `pl2cl --server`
  talks over the child's stdin/stdout, exits when its parent dies (`getppid`
  tick, s397).  Nothing on disk, nothing another user can connect to.
- **The test-harness transpile server (`pl2cl --xserver`, #1545) is
  reachable only by its owner.**  `Pl/t/PCLCore.pm` puts the socket in a
  `File::Temp::tempdir` (0700, cleaned up), its `.warned` marker is created
  `O_EXCL` 0600, and the server exits when the owner pid dies.  The server
  itself does NOT check who connected (no `SO_PEERCRED`); a request carries
  cwd, ARGV and a full environment, which the forked child adopts — so a
  socket placed in a shared directory would let any local user run `pl2cl`
  as the victim with the attacker's `PERL5LIB`.  Today the directory is
  private, so this is a rule to keep, not a hole: **`PCL_XSERVER` must name a
  path inside a directory only the caller can enter.**
- **`tools/install-pcl --uninstall` removes only what it recognises**: it
  refuses when `<prefix>/lib/pcl/VERSION` is absent and deletes only wrappers
  it wrote (one spelling, `write_wrapper`).
- **Environment variables are trusted exactly as perl trusts `PERL5LIB`.**
  `PCL_ROOT` (the runtime tree), `PCL_CORE`/`PCL_TEST_CORE` (a saved core to
  boot from), `PCL_CACHE_DIR`, `PCL_XSERVER`, `PERL5LIB` each redirect what
  gets loaded.  Perl drops `PERL5LIB` under taint/setuid; PCL has no such
  mode.  Consequence to write down: **never run `pcl` with elevated privilege
  and a preserved environment** (`sudo -E pcl …` boots whatever core
  `$PCL_CORE` names).  `$^X` is found through `/bin/sh -c 'command -v perl'`,
  i.e. `PATH`, at run time — the same class.

## 2. Findings with a task

1. **`runpcl`, `tools/runt`, `tools/clt` use predictable `/tmp` names and
   shell strings** (task #1798).  `runpcl` writes `/tmp/runpcl_$$.lisp`,
   `/tmp/runpcl_$$.err`, `/tmp/runpcl_$$.runerr`; `runt`/`clt` write
   `/tmp/<name>.lisp`, `/tmp/<name>.out`, `/tmp/<name>.pl2cl.err` — no pid at
   all.  They are created by shell redirection with the caller's umask
   (0002 here → world-readable), so for the run's duration the transpiled
   program AND its output are readable by every local user; a pre-created
   file or symlink at the predictable name is the classic `/tmp` race
   (arbitrary file clobber; with the `.lisp` swapped between transpile and
   load, code execution as the victim).  The input file name and every path
   are interpolated into a `system("…")` string, so a file called
   `x;rm -rf ~.pl` runs the command.  Modern Linux blunts the race
   (`fs.protected_regular=2`, `fs.protected_symlinks=1` on this box) and the
   injection needs the user to name the file — but `runpcl` is in the README
   as a user command and the fix is mechanical: one `File::Temp::tempdir`
   per run and list-form `system`/`open`, keeping #504's two-streams
   contract.  The sweep's `/tmp/pcl-sweep-$$.out` and its `quote => 0`
   command string are the same shape in a dev-only tool.
2. **`pcl --clear-cache` trusts `$PCL_CACHE_DIR` blindly** (task #1799).  It
   unlinks `<root>/modules/*`, `<root>/*.fasl` and `<root>/*.lisp` (the
   pre-#1261 flat layout) plus the cores and prototype facts.  A user who
   points `PCL_CACHE_DIR` at a directory that also holds their own `.lisp`
   files loses them.  Fix: a marker file written when PCL creates the root,
   required by `--clear-cache`; delete only inside PCL's own subdirectories;
   retire the flat glob.
3. **The Lisp reader is trusted with generated text** (task #1800).  User
   code is loaded with `*read-eval*` = T (SBCL's default): a `#.(…)` that
   reached the reader outside a string or a one-line comment would run at
   read time.  Today nothing reaches it — strings are escaped (probed) and
   the `;; PARSE ERROR: <error>` comment was observed single-line for every
   census shape, including the `Dump:` one — but the invariant is not
   enforced anywhere.  Two cheap lines: bind `*read-eval*` to NIL around
   every load/compile of generated code (`pcl`'s `(load …)`,
   `p-load-with-recovery`, the module `compile-file` path, the eval server's
   output — the runtime's own three `#.` are compiled into the core already),
   and `s/\n/ /g` on the drop comment's text.  Defence in depth, not a hole:
   the only party who can put `#.` there is the author of the program being
   run.
4. **Privacy of the eval cache** (no task; a sentence for `docs/caching.md`).
   `~/.pcl-cache/evals/` keeps the transpiled Lisp of every `eval STRING`
   (0700, pruned after 30 days unused).  Perl keeps nothing.  A program that
   builds code from a secret leaves that secret on disk until the prune or
   `pcl --clear-cache`.
5. **Saved executables trust a baked-in path** (already task #217).
   `pl2cl --executable` records the build tree's absolute paths; the binary
   reads extensions and a cache-missing `require` from there at run time, so
   on a machine where that path is writable by someone else the binary loads
   their code.  Relocatable preamble = the fix already filed.

## 3. Runner drift found on the way (not security) — task #1802

`tools/runt lex.t` (and a hand run with the SAME transpile, the SAME core and
the sweep's own `p-load-with-recovery`) passes lex.t's four `<null> ident`
`fresh_perl_is` rows (53/0, eight `REGISTRY-STALE` lines), while the sweep
fails them and the registry relabels them (45/0, eight `[registry]` skips);
join.t 43 vs 41, sub.t 58/0 vs 54/2.  Ruled out by measurement: the transpile
(`cmp` identical), the loader, `PCL_DROP_LOG`, stdin.  Not yet measured: the
sweep's `run_isolated` session + `timeout` wrapper and its `%ENV` as seen by
the `fresh_perl` CHILD (`tools/pclperl-for-tests`).  Until it is located, a
hand run of a `fresh_perl` file is not the sweep's verdict — which is exactly
how this session's own registry-stale count read 19 where the sweep says 7.

## 4. Recommendations, in one list

- Fix #1798 (`File::Temp` + list forms in `runpcl`/`runt`/`clt`) — the one
  real cross-user exposure, small.
- Add to the README/`docs/pcl-commands.md`: `pcl -c` and `pl2cl` run nothing
  from the file; never `sudo -E pcl`; `PCL_XSERVER` must be a private path.
- #1799 marker for `--clear-cache`; #1800 `*read-eval*` NIL + one-line drop
  comment; the eval-cache sentence in `docs/caching.md`.
- Optional hardening for the harness server: `SO_PEERCRED` uid check on
  accept, so the private-directory rule is enforced rather than relied on.
