# Compiling a Perl program to ONE standalone binary — the plan

**Status:** PLAN (s453, Fable, 2026-08-30).  Planning only — nothing here is
implemented by this doc; the owner task is **#756**.
**Supersedes** the `--exe` sketches in `docs/pcl-command-plan.md` §pclbuild
and `docs/pcl-rollout-plan.md` Phase 8 where they disagree; the bundling
analysis in `docs/fasl-caching-design.md` §8B still applies.
**Related:** `docs/extensions.md` §Distribution, task #217 (relocatable
preamble), #349 (extension preamble), the s435 flip (drop → trappable die).

## 1. Goal

```
pclbuild --exe prog.pl -o prog     # or: pl2cl --executable, rebuilt on this plan
./prog args...                     # runs on a machine with NO perl, NO SBCL, NO PCL
```

One file, produced by `sb-ext:save-lisp-and-die :executable t`, containing
the runtime, the program, and its module closure.  Perl-shaped behavior end
to end: `@ARGV`, `%ENV`, exit codes, `die` printing its message (not an SBCL
backtrace) and exiting 255, END blocks running.

## 2. What exists today, and why it is not that

| piece | state |
|---|---|
| `pl2cl --bundle` / `--executable` | EXIST but primitive.  `--bundle` concatenates the runtime **source** + the main file's transpile — it does **not** bundle the `use`-closure (the comment in `do_bundle` says so; modules still resolve at run time through `@INC` + a perl subprocess).  `--executable`'s saved `:toplevel` only exits: the program's top-level statements run during the build's `load`, so **the binary re-runs nothing** — it is a demo, not a product. |
| `tools/install-pcl` | Installs the whole tree + saved core.  Right model (compile at install), wrong artifact (a tree, not one file). |
| Saved cores (`~/.pcl-cache/core/`, s439) | The mechanics of `save-lisp-and-die` are proven and content-keyed.  Reusable as-is. |
| `pcl` driver | Phase 1 (transpile-and-run) only; caching tiers are Phase 6, unbuilt. |
| Preamble | Bakes the BUILD machine's absolute paths (`@INC` push list, `*pcl-pl2cl-path*`, perlbrew dirs) — task #217.  Fatal for a shipped binary. |
| Extensions (`pack`/`mro`/`warnings`/`xs`) | Lazy self-loading stubs that read `.lisp` files from the tree at first call — absent on the target machine. |
| String eval | `p-eval` spawns `pl2cl --server` (perl + PPI) at run time. |

## 3. Design

### 3.1 Run-time vs build-time: the `--build-mode` wrap

Top-level CL forms execute at `load`, i.e. at BUILD time.  The transpiler
gains a mode that splits each **section** (the phase model, DECIDED §s433
`#456(b)`/§s437) in two:

* compile-phase material stays top-level — `p-sub`, `p-defcell`, `defvar`,
  `eval-when`, BEGIN bodies, `p-run-compile-phase-blocks`.  These run at
  build, which IS perl's compile phase.  (Consequence to document: a BEGIN
  that reads the environment freezes the *build* machine's answer.)
* run-phase statements move, in section order, into one generated
  `(p-sub pl-__pcl_main__ …)`.  The saved `:toplevel` binds `@ARGV`/`%ENV`
  from the real process, calls it under a handler that prints an uncaught
  `p-die`'s message perl-shaped and exits 255, runs END blocks via the
  existing `p-exit` phase machinery, and exits 0 otherwise.

The unit of the split is the statement's phase, not its syntax: a top-level
`my $x = f()` is `defvar` (declarative, stays) + `p-my-=`/`p-setf` (moves) —
exactly the open question 4 of the old command plan, answered by the phase
model that has since been built.

### 3.2 Module closure: `use` preloads, `require` embeds

Perl's own phase semantics decide the two treatments:

* **The `use`-closure** (transitively: every literal `use Module`, plus
  `use parent`/`use base` requires) is transpiled and **loaded at build
  time** — that is when perl runs it too (compile phase).  Its `%INC`
  entries and a preloaded-registry mark make the runtime `p-use`
  short-circuit (the `*pcl-preloaded-eval-libs*` mechanism from the old
  command plan, generalized).  Discovery reuses the walk that already opens
  every one of these files per transpile (`_extract_module_prototypes`;
  Phase 7's planned `--emit-deps` is the same list surfaced).
* **The `require`-closure** (literal `require Foo` / `require "file.pl"`,
  found by the same static scan) is transpiled and compiled at build but
  **embedded as FASL bytes, not loaded** — a table `module-name → blob` in
  the image.  At run time `p-use`/`p-require-file` consult that table
  before `@INC` (write blob to a temp file, `load`, unlink — or a stream
  load if SBCL cooperates).  This preserves require's RUN-time load timing
  and side-effect order; a conditional `require` that never fires costs
  only image size.
* **Modules reached only dynamically** — `require $var`, `eval "use Foo"` —
  are invisible to any static scan.  These are **parameters**:
  `--with-module Foo::Bar` (repeatable) and `--with-modules-from FILE`
  add to the embedded table; the transitive closure of a `--with-module`
  is embedded too.

A run-time miss (module neither preloaded nor embedded nor findable on the
target's `@INC`) stays perl-shaped: `Can't locate Foo/Bar.pm in @INC …` with
one added sentence — `(binary built without it; rebuild with --with-module
Foo::Bar)`.

### 3.3 Extensions and artifacts

The three transpiled artifacts + `pcl-xs` are `.lisp` files loaded lazily
from the tree — absent beside a binary.  Policy: **embed all non-XS
extension sources in the image as text blobs** and teach `p-load-extension`
to check the blob table first (same mechanism as §3.2's require table; they
are tiny relative to a ~50 MB SBCL image).  XS is out of scope for binary
v1: if the closure pulls in an XS dist, the build **refuses loudly**, naming
the dist (a `.so` cannot live inside the image; shipping it beside the
binary is a later `--with-xs-dir` option, if ever).

### 3.4 The preamble must become relocatable (task #217, now load-bearing)

Binary mode emits NO baked paths: `@INC` = `.` + `$PERL5LIB` +
`$PCL_LIB` at process start; `*pcl-pl2cl-path*` set only if a transpiler is
actually found on the target (§3.5).  This is the existing #217 work item
promoted from "future installation nicety" to a step of this plan.

### 3.5 String eval — the honest story

`eval $str` is a HARD REQUIREMENT in general, but a self-contained binary
cannot contain the Perl-based compiler.  Ruled shape (proposed):

* At run time, `p-eval` looks for `pl2cl` (target machine has PCL/perl
  installed → everything works, exactly as today).
* Otherwise it **dies perl-shaped and trappably** at the eval site:
  `PCL: string eval requires the PCL transpiler, not found on this machine
  (binary built by pclbuild; install PCL or avoid string eval)` — the
  s406 "a drop inside a string eval DIES" precedent, applied one level up.
* `--eval-lib`/`--with-module` cover the *module* half of dynamic eval
  (the common `eval "use Foo; 1"` guard idiom): the module is embedded, and
  an eval whose text is exactly a `use`/`require` of an embedded module can
  short-circuit without the compiler.  (Cheap, covers the idiom that
  gates optional dependencies across CPAN.)

### 3.6 Build-time diagnostics (motivated by the §5 survey)

The build is the last moment a human sees output before the program runs on
some other machine, so `--exe` ends with a **portability report**:

* every drop announcement (already loud since #339) repeated in summary;
* every *silently-divergent* construct the scan can see statically —
  `sub DESTROY` definitions, `format` blocks, `(?{`/`(??{` in regex
  literals, `tie` of an array/hash — one WARN line each, citing
  `docs/not-supported.md`;
* the eval/require sites that could not be resolved statically, so the
  user knows what `--with-module` might be needed for.

## 4. Steps, in order (each independently shippable)

1. **#217 relocatable preamble** — prerequisite, also fixes the installed
   artifacts' baked paths.  Acceptance: a transpile on machine A runs from
   a moved tree on machine B.
2. **`--build-mode` split + perl-shaped toplevel** — binary of a
   no-modules script runs its program at run time, `die` prints message
   only, exit 255; END blocks run.  Acceptance probes: exit codes, @ARGV,
   %ENV, die/eval, END — byte-compared vs perl.
3. **`use`-closure preload** (static scan + build-time load + registry).
   Acceptance: a script using `List::Util` + a local `lib/` module runs
   with `@INC` emptied.
4. **`require` embedding + `--with-module`** (the blob table).
   Acceptance: literal `require`, conditional `require`, `eval "use Foo"`
   with `--with-module Foo` — all on a PATH without perl.
5. **Extension embedding + XS refusal** (§3.3).  Acceptance: a script
   calling `pack` runs standalone; a Digest::MD5 script refuses at build
   naming the dist.
6. **Eval fallback + portability report** (§3.5, §3.6).
7. **End-to-end guard** `tools/t/pclbuild-exe.t` (not in the gate — it
   spawns SBCL builds; the #282 sanitized-HOME/PATH rehearsal pattern is
   the harness model).

Sizing: steps 2–4 are the substance; each is roughly a session.  Step 1 is
independent and already owned by #217.

## 5. Survey — what the compiler says TODAY about unsupported features (measured s453)

Probed on HEAD (`e43ef48`), nine features, transpile stderr + run captured.
Five diagnostic classes exist; **three are loud, two are silent**:

| class | examples probed | behavior |
|---|---|---|
| Transpile-time hard refusal | smart match `~~`; `use feature 'class'` (5.38 class/field/method) | exit 255, perl-shaped message with file+line.  Good. |
| Drop → trappable run-time die (the s435 flip) | lvalue-sub assignment `lv() = 7` | announced at transpile AND dies at the site when reached.  Good.  (Presentation gap: uncaught, it surfaces as an SBCL `Unhandled simple-error` + backtrace, not perl's message-only exit — §3.1's toplevel handler fixes this for binaries; `runpcl` has the same cosmetic issue.) |
| Run-time announce-and-continue (rule-12 effect-only boundary, s329) | `tie @a` (task #155 cited in the message); computed `goto $label` | one clear stderr line naming the task/doc, execution continues.  Good. |
| **Silent stub** | `format STDOUT = …` block: stripped at source level with NO trace (no `;; PARSE ERROR` comment, no announcement); `write` → `(p-write)` which returns 1 silently | **Nothing is ever said.**  Deliberate per `docs/not-supported.md`, but the s329 ruling's own words are "the sin is the silence" — an effect-only missing case should ANNOUNCE. |
| **Silent absence** | `sub DESTROY` (defined, never called — program output simply lacks the destructor's effects); regex code block `/(?{ $seen = 1 })/` (the runtime STRIPS `(?{…})`/`(??{…})` from the pattern before CL-PPCRE sees it — `pcl-runtime.lisp` ~17715, documented, cl-ppcre would hang — so the match proceeds without the block, the code never runs, `$seen` stays undef, no message; note the block's text is also variable-INTERPOLATED before the strip) | **Nothing at transpile, nothing at run.**  The worst class: both are *documented* non-support, but only a reader of `not-supported.md` knows. |

Also found while probing: **CLAUDE.md's "`use experimental 'try'` does NOT
work (task #360)" is STALE** — the probe works end-to-end and #360 is marked
completed.  Corrected in this commit.

**Recommendation (filed as task #757, not scheduled):** close the silent
classes at the announce level, never by implementing —
(a) `format` block stripping announces once per file at transpile;
(b) `p-write` announces once per run (the tie-array pattern);
(c) a `sub DESTROY` definition announces at transpile ("defined but never
called by GC, docs/not-supported.md");
(d) `(?{…})`/`(??{…})` in a pattern announces at transpile.
All four are one-line WARNs at existing sites, and §3.6's portability
report consumes the same scan.

## 6. Open questions for the USER

1. Is the §3.5 eval story acceptable for v1 (works iff PCL is installed on
   the target; trappable perl-shaped die otherwise)?  The alternative —
   porting the compiler off perl — is a different project.
2. Image size: plain SBCL executables are ~40–70 MB.  `save-lisp-and-die
   :compression` (zstd) roughly halves that if the local SBCL is built with
   it.  Care, or ship uncompressed?
3. Does binary work rank ahead of, behind, or interleaved with the v0.2
   census/perf queue (`docs/plan-post-s433.md` §s452)?  Nothing above
   touches the compiler's semantic core except step 2's emission mode.
