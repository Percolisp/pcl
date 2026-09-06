# Plan: the cache surface, the root scripts, the install layout (s471, Fable, 2026-09-06)

The USER's three questions of 2026-09-06, answered from probes on main `9ee95f2`
(gen v2-830) while s470bv-resume and s470bw (#1261) ran:

1. What should the user information and commands for the compiled-module cache
   (#1188 fasls, #1261 dependency manifest) look like — flags to clear it, where a
   user lists directories to compile or not compile (`*` = all), documented in the
   README and the commands' help texts; and what else?
2. Should `runt`, `clt`, `sweep-perl-tests.pl` move into `tools/`?
3. How should PCL install — a `bin/` directory on `$PATH` like perl — when every
   script finds its runtime in a directory beside itself?

## 0. Findings (probed 2026-09-06)

- **F1 — `PCL_CACHE_DIR` reaches three of the four caches, not the module cache.**
  `*pcl-cache-dir*` is `(merge-pathnames ".pcl-cache/" (user-homedir-pathname))`,
  a `defparameter` initform, so it is evaluated when the runtime LOADS — i.e. when a
  core is built.  Probe: `PCL_CACHE_DIR=/tmp/x PCL_NO_CORE=1 ./pcl -e 'use
  Text::ParseWords …'` wrote `proto/` under `/tmp/x` and the module `.lisp` under
  `~/.pcl-cache` (117 → 118 files).  The core cache (PCLSbcl), the proto cache and
  `pcl --clear-cache`'s glob all honour the variable; the README says it "moves"
  the cache.  Worse for the install model: a saved core bakes the BUILDER's home
  in — a system-wide `pcl.core` built by root would send every user's module cache
  to `/root/.pcl-cache`.  Fix: compute the directory at process START (an init
  hook), reading `PCL_CACHE_DIR` there.  Task **#1303**.
- **F2 — the 7-day age rule re-transpiles every module weekly.**  `p-cache-valid-p`
  rejects an entry older than `*pcl-cache-max-age*` (7 days) and
  `p-cleanup-old-cache`, run on every miss, deletes every cache file older than
  that — so a valid, untouched entry is rebuilt once a week whatever its content.
  With #1261's content manifest, validity is content; the prune should be by last
  USE (touch on hit; 30 days).  #682 (`proto/` is never pruned) is the same family.
- **F3 — `tools/install-pcl` does not install `pcl`.**  Its tree list is `pl2cl
  runpcl Pl lib cl tools/lib` and it writes wrappers for `pl2cl` and `runpcl`
  only, while the README says "`pcl` is the everyday command" and "to put the
  commands on your PATH, install a copy".
- **F4 — `pl2cl --help` answers "Unknown option: help"; `pcl --version` does not exist.**
- **F5 — root resolution has five spellings** that agree only because every script
  sits at the checkout root: `pcl` `dirname(abs_path($0))`; `pl2cl` `$script_dir`;
  `runpcl`, the sweep, `tools/pclperl-for-tests` `$FindBin::RealBin`; `runt`/`clt`
  `abs_path(dirname($0))`; `PCLSbcl::_installed_core` a regex on the runtime path
  (`(.*)/cl/[^/]+` → `<root>/pcl.core`).  Q2's move and Q3's install both lean on
  the agreement.
- **F6 — `run-perl-test.pl` at the root is dead**: zero code references; one stale
  doc (`docs/debugging-hangs-crashes.md`) still names it.
- **F7 — the module cache is FLAT in the cache root** (`<HASH>.lisp`,
  `<HASH>-<RTID>.fasl`, `.failed`, and s470bw's `.deps`), beside `core/`, `proto/`,
  `xs/`.
- **F8 — the cache directory is created with the umask default**
  (`ensure-directories-exist`).  A fasl is code.
- Concurrency is fine: both the `.lisp` and the fasl are written temp-file + rename.
  Two PCL VERSIONS sharing one HOME is #1119 (the docs must say so, #1262 §7).

## 1. Q1 — the user surface (RULED s471)

### 1.1 Environment variables

Read by `pcl`/`pl2cl` AND the runtime, at process start — never at core-build time.

| variable | meaning | default |
|---|---|---|
| `PCL_CACHE_DIR` | the root of every per-user cache: modules, `proto/`, `core/`, `xs/` | `~/.pcl-cache` |
| `PCL_COMPILE_DIRS` | colon-separated directories (PERL5LIB syntax).  A module whose SOURCE file lies under one of them is compiled to native code (a fasl) and cached.  `*` = every directory. | perl's installed library dirs + PCL's own `lib/` — the list `*p-core-inc-dirs*` already carries (the "installed" class) |
| `PCL_NO_COMPILE_DIRS` | same syntax.  A module under one is NEVER compiled to native code; it still gets the transpile cache and the manifest check, and loads from the cached text.  Wins over `PCL_COMPILE_DIRS` on any match.  `*` = compile nothing. | empty |
| `PCL_NO_FASL_CACHE=1` | alias of `PCL_NO_COMPILE_DIRS='*'` (shipped with #1188; kept, documented in one line as the alias) | |
| `PCL_NO_CORE=1`, `PCL_CORE=path` | unchanged | |
| `PCL_FASL_DEBUG=1` | per-module HIT / build / TEXT trace (exists since #1188) | |

Rules: entries are realpath'd at start (`~` expanded, relative entries against
the cwd); a match is a path-component prefix on the module file's realpath; a
listed directory that does not exist is not an error (perl's `@INC` tolerates
it) — `pcl --cache-info` shows what resolved.  The s470bw brief's
`PCL_FASL_CACHE=installed|all|none` is NOT shipped: the two lists express all
three (`unset` / `*` / NO=`*`) and more, and a class word needs a classifier the
user can neither see nor adjust, while a directory list is what perl users
already write.  The default list IS the "installed" class.  (Sent to s470bw
07:55; it implements the lists.)

### 1.2 Commands

- `pcl --clear-cache` — everything PCL made under `PCL_CACHE_DIR`: modules
  (`.lisp` / `.fasl` / `.deps` / `.failed`), prototype facts, cores.  NOT the XS
  artifacts (installed by `tools/pcl-xs-install`, cleaned by its `--clean`) — the
  help says so.  ONE flag, no sub-selection: a core rebuilds in ~20 s, and a knob
  is not worth its documentation.
- `pcl --cache-info` (NEW) — where the cache is, size and count per kind, which
  core this run would use (installed / cached / source) and why, and the compile
  policy IN EFFECT: the resolved COMPILE dirs, the NO dirs, and the source of each
  (default / env).  The one diagnostic for "PCL did not notice my change".
- `pcl --no-cache` (NEW) — this run reads and writes no module cache
  (`*pcl-skip-cache*`, which `runt` already sets): "is it the cache?" in one flag.
- `pcl --version` (NEW) — PCL version (the tag / CHANGELOG), cache generation,
  SBCL version, PPI version.  `pl2cl --help` (NEW, F4).
- `pcl --make-core`, `pcl -v` unchanged.
- Help texts: `pcl --help`'s "Startup cache" section becomes "Caches" — the three
  variables of §1.1 and the four flags; `tools/install-pcl --help` gains one
  paragraph (the core is compiled at install into `<prefix>/lib/pcl/pcl.core`;
  modules are compiled per user under `PCL_CACHE_DIR` on first use).

### 1.3 Docs

README "Caches and switches" → three short paragraphs (what is cached / when it
is stale / the knobs) + a link to `docs/caching.md` (#1262, whose §"knobs" is the
§1.1 table).  README's "`PCL_CACHE_DIR` moves it" becomes true only with #1303.

### 1.4 Anything else — the findings as work

1. F1 / **#1303** lands with or right after #1261: an installed core with a baked
   home is a release bug.
2. F2: validity = the manifest only (s470bw drops the age clause — told); the
   prune becomes last-use-based with touch-on-hit, 30 days, `proto/` included
   (#682 folds in) → **#1300**.
3. F8: create the cache dir `0700`; refuse LOUDLY a cache dir not owned by the
   user or group/world-writable → **#1300**.
4. F7: module entries under `<cache>/modules/` — free NOW because #1261 invalidates
   every entry anyway (s470bw does it if ≤ 3 lines, else #1300 at the next
   generation bump).
5. Later, unscheduled: install-time precompile of the 22 shipped `lib/` shims into
   the install tree (the core's model applied to `lib/`), so a fresh user's first
   `use List::Util` pays no compile.

## 2. Q2 — the root scripts (RULED: yes, move them) — #1301

Move `runt`, `clt`, `sweep-perl-tests.pl` → `tools/`; DELETE `run-perl-test.pl`
(F6).  The root keeps `pcl`, `pl2cl`, `runpcl` (products) and `xs-pin` (a pin
file read by `tools/build-pclxs` and pclxs).  Mechanics: `git mv`; the three
derive their root one level up — through Q3's one resolver if it has landed,
else `dirname(dirname(abs_path($0)))`; the sweep must spawn byte-identically
(`PCL_SHOW_SBCL=1` before/after — the runner row of the WHAT-TO-RUN table, one
full sweep, verdicts compared file by file).  References to update: CLAUDE.md
(5), README (1), `.claude/settings.json` + `.local` permission strings,
`tools/dup-census.pl:99`, `Pl/t/no-hardcoded-paths-01.t:53`, `tools/rebuild-pack`
(2 messages), `Pl/t/artifact-staleness-01.t`, `cl/pack-impl.pl`, comments in
`tools/run-perl-suite.pl`, `tools/lib/*`, `tools/sweep-diff.pl`,
`cl/pcl-test.lisp`, the live docs (`test-debugging-runbook`,
`test-skip-registry`, `sweep-bug-catalog`, `perl-test-suite-coverage`,
`debugging-hangs-crashes`), memory `reference_runt_script`.  `docs/history/`
untouched.  Half a session.  Timing: after s470bv and s470bw merge (neither
touches these files, but "live first").

## 3. Q3 — the install layout (RULED: keep the shape, close the gaps) — #1302

The install ALREADY has perl's shape: `<prefix>/bin/<cmd>` are sh wrappers that
`exec <prefix>/lib/pcl/<cmd>`; the tree under `lib/pcl` keeps its repo-relative
layout (`Pl/ lib/ cl/ tools/lib/`) with the core at `<prefix>/lib/pcl/pcl.core`;
every script finds its tree beside its REAL path (abs_path / RealBin follow
symlinks); PCLSbcl derives `<root>/pcl.core` from the runtime path.  "The code
expects the runtime under itself" is not an obstacle — that is perl's own model
(the binary knows its privlib); the wrapper is the bridge.  What changes:

- (a) install `pcl` (F3); the smoke test runs the INSTALLED `pcl` too.
- (b) ONE root resolver (F5, rule 11): `PCLPaths::root()` — `$PCL_ROOT` when set
  (explicit wins, PCLPaths' own rule), else derived from the calling script's
  real path, VERIFIED by `cl/pcl-runtime.lisp` existing there (die naming both
  candidates otherwise, rule 12).  `pcl`, `pl2cl`, `runpcl`, the sweep,
  `pclperl-for-tests`, `runt`/`clt` and `PCLSbcl::_installed_core` all read it.
  The wrappers stay (a symlinked bin dir keeps working).
- (c) PATH: the installer already prints "add `$bindir` to PATH"; make it print
  the exact line (`export PATH="$HOME/.local/bin:$PATH"`) and only when `$bindir`
  is not on PATH; never edit rc files.  The default prefix `~/.local` is already
  on PATH on most distributions.
- (d) `tools/install-pcl --uninstall`: removes `<prefix>/lib/pcl` and the
  wrappers; says the per-user cache is untouched (`pcl --clear-cache` for that).
- (e) installed-mode caches: the core per install (compiled at install), modules
  per user under `PCL_CACHE_DIR` — needs #1303 first.
- (f) what is installed: `pcl`, `pl2cl`, `runpcl`, `tools/lib`; later
  `pcl-xs-install` (needs pclxs).  Never the dev runners.
- (g) `pcl --version` (§1.2).
- Not now: man pages, a distribution package.

## 4. Sequencing

- NOW: s470bw told the §1.1 surface (07:55).
- **BY = #1300** (Opus, after s470bw merges — same files): §1.2 commands, #1303,
  F2, F8, `modules/` if bw skipped it, README + help texts.  Then **#1262**
  (Sonnet) writes `docs/caching.md` from bw's and BY's records.
- **#1301** (Q2; Opus or Sonnet, half a session): after bv + bw merge; independent
  of BY.
- **BZ = #1302** (Opus): §3 (a)–(g); after BY (needs #1303 and `--version`).
- BX (the BS residue) keeps its reserved place; the order of BY / #1301 / BZ
  against BX is the USER's call.  Recommendation: BY → #1301 → BX → BZ — the
  cache shipped this week and its knobs are what a first user meets; BZ after BY
  because it needs #1303.

## 5. Installer TESTS (USER ask, 2026-09-06: "can you add tests for the installer? should I install Docker?")

### 5.1 What exists
- `tools/t/install-pcl.t` (15 rows, NOT in the gate; CI runs it after a real
  install): dry run changes nothing; the PPI floor refuses loudly and installs
  nothing; a `--no-core` install; the tree carries no `Pl/t`; the installed
  `pl2cl` and `runpcl` run a two-line fixture.  It runs under the DEV home, so
  the per-user cache it exercises is this box's.
- `.github/workflows/install-matrix.yml` + `tools/install-matrix/install-and-verify.sh`:
  four images (ubuntu 22.04 / 24.04, debian 12 / 13) × pinned SBCL, the whole
  recipe from apt to `prove tools/t/install-pcl.t` — **as ROOT, on push only**.
- `.github/workflows/ci.yml`: the real install (with the core) on the runner
  user, then the installer test.

Holes: the installed CORE is built only in CI; `pcl` is not installed at all
(F3); nothing runs from another cwd, through a symlinked bin dir, under a fresh
HOME, or as a DIFFERENT user than the one who built the core (the #1303 shape);
`--force`, the PATH hint and (once they exist) `--uninstall` / `PCL_ROOT` are
untested.

### 5.2 Layer A — extend `tools/t/install-pcl.t` (no container; part of #1302)
ONE real install WITH the core, reused by every row (a core build is ~20 s; the
file is not in the gate, so ~1 min is acceptable), the `--no-core` install kept
for the fast rows.  Rows:
1. `<prefix>/lib/pcl/pcl.core` exists and `PCL_SHOW_SBCL=1 <prefix>/bin/runpcl`
   names it (PCLSbcl resolution step 3, the installed core).
2. the installed `pcl -e` runs (after #1302 (a)).
3. the same program from a different cwd, and through a SYMLINKED bin dir.
4. fresh HOME: `HOME=<tmp>` → the installed tools create `<tmp>/.pcl-cache` and
   touch nothing under the dev home; `PCL_CACHE_DIR=<tmp2>` → the module `.lisp`
   and `.fasl` land under `<tmp2>` — **fails today (#1303)**: the installed core
   was built under the dev HOME, so this row IS the "built by A, run under B"
   case without a second user.
5. `--force` replaces an existing tree and a shim that no longer exists does not
   survive (the flag's documented reason).
6. `--uninstall`: wrappers and `lib/pcl` gone, the cache untouched (after #1302 (d)).
7. the PATH hint: printed when `$bindir` is not on PATH, absent when it is.
8. `PCL_ROOT` pointing at a directory without `cl/pcl-runtime.lisp` dies naming
   both candidates; pointing at the install root works (after #1302 (b)).
Inverse-verify rows 4 and 8 on a `git archive` extraction of main.

### 5.3 Layer B — `tools/t/install-container.t` (#1304; skips without a runtime)
A local container test, `podman` preferred, `docker` accepted (the script probes
`podman` then `docker`; `plan skip_all` when neither is present, so it is safe to
write before the runtime is installed).  Split the CI recipe into a
dependency half (`tools/install-matrix/deps.sh`: apt, cpanm PPI, the pinned
SBCL, Quicklisp + cl-ppcre) and a verify half (`verify.sh`, from `tools/install-pcl`
on) so CI and the local test share ONE recipe (rule 11); build the base image
once, tagged by a hash of `deps.sh` + the SBCL version (iterations then cost
~30 s, not the 2–3 min download).  Legs:
- (a) the verify half as root = what the matrix does — one local run before a
  push instead of a push per attempt;
- (b) a NON-ROOT user (`useradd`), install to `$HOME/.local`, the PATH line;
- (c) the SHARED install: root installs to `/opt/pcl`, the user runs
  `/opt/pcl/bin/pcl -e 'use List::Util qw(sum); print sum(1..3)'` with an
  EMPTY home — the module cache must appear in the USER's home (#1303: fails
  today) and the run must print 6;
- (d) `pcl --cache-info` under (c) reports the user's cache dir and the default
  compile list (after #1300).
Minutes, never in the gate; run by the installer tasks and before a tag.

### 5.4 Docker or podman? (RULED: podman, rootless; Docker acceptable)
Yes, install one: the matrix runs only on push, only as root, and a push per
attempt is the wrong loop for installer work; leg (c) is the shape #1303 breaks
and no non-container rehearsal reproduces "another user's home".  **podman**
over Docker: rootless and daemonless (no `docker` group = root-equivalent
membership on a dev box), same CLI (`alias docker=podman` works), in Ubuntu
26.04's apt (`sudo apt install podman`, 5.7).  The test uses whichever is
present.  Cost: minutes to install, a few hundred MB per base image.
