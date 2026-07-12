# pclxs Repo Plan — project structure and GitHub bootstrap for the XS shim

**Written:** 2026-07-12 (session 285, Fable), for **Opus 4.8 to execute**.
**Companion to:** `docs/xs-shim-design.md` — that doc remains the normative
*technical* spec (architecture, vtable, semantics, phase acceptance).  This
doc decides *where the code lives*, how it goes public on GitHub, how PCL
consumes it, and how the design's phases split across two repos.

---

## 1. Decision: a parallel public repo, not a `pcl/` subdirectory

The shim becomes its own repository, **`pclxs`** (name verified free on
GitHub, 2026-07-12 — the only search hits are unrelated substring matches
`pclxsk`/`pclxtool`).  **Naming decided by the user (2026-07-12):** the name
must contain *pcl* and *xs* — an earlier candidate, `perlshim`, was rejected
as too generic ("perlshim can be anything").  The C-level naming follows the
repo name so nothing diverges: library `libpclxs.so`, headers
`include/pclxs/`, symbol prefix `pclxs_*`, `PCLXS_ABI_VERSION`.  PCL keeps
only its host adapter.  Four reasons for the split, in order of force:

1. **The design already mandates the boundary.**  R5: the shim compiles
   with *no reference to SBCL or CL*; the vtable
   (`pclxs_host.h`) is the portability contract, written so a Python,
   Ruby, or JS host can implement it.  A separate repo makes that boundary
   physical: a PR that `#include`s anything host-specific is visibly wrong.
   Keeping it inside `pcl/` invites exactly the leakage the design forbids.
2. **`pcl` is not on GitHub.**  The pcl repo has no remote (verified);
   making the shim contributable would otherwise mean open-sourcing the
   whole transpiler or maintaining a subtree-split mirror — both bigger
   decisions with ongoing friction.  A fresh repo decouples them.
3. **The contributor audiences are different.**  pclxs's natural
   contributors are perl/XS/C people (module authors who want their dist to
   build, host authors for other languages).  They need `cc`, `perl`, and
   `prove` — not a 22k-line transpiler and SBCL.  The repo must be testable
   standalone (§3 below makes it so).
4. **CI is only possible standalone.**  GitHub Actions can build the shim
   and run its test suite with stock perl + gcc on every PR.  It could
   never run the PCL side (private).  So the public repo's acceptance
   gates must not depend on PCL — which the design's differential-vs-real-
   perl oracle (§13) already almost gives us.

**Alternative rejected:** `pcl/xs/` subdirectory published via
`git subtree split`.  Workable only if pcl itself goes public someday;
until then it's a one-way mirror where outside PRs land in a repo the
maintainer can't run CI in.  Revisit only if the user decides to publish
all of PCL.

**Cost of the split (accept and manage):** vtable/ABI changes need
coordinated commits in two repos.  Managed by the ABI version the design
already requires (`PCLXS_ABI_VERSION`, checked at `pclxs_init` and
baked into artifact names §7.4/§9) plus the pin file (§4).  The
vtable is frozen within each phase; changing it is a deliberate,
version-bumping event in both repos.

## 2. What lives where

| artifact (design §) | repo | note |
|---|---|---|
| `include/pclxs/{EXTERN.h, perl.h, XSUB.h}` (§2.2, §4.4) | **pclxs** | the macro layer |
| `include/pclxs/pclxs_host.h` (§5) | **pclxs** | THE contract; ownership rules 5.1 as comments |
| `src/{sv,stack,av_hv,call,global,coerce}.c` (§6) | **pclxs** | pure C, no host refs |
| reference host `refhost/` (NEW, §3 below) | **pclxs** | plain-C vtable impl for standalone tests |
| `tools/xs-build` (§9, host-neutral core) | **pclxs** | metadata harvest + xsubpp + compile/link via `%Config` |
| `tools/xs-api-census.pl` + `census/` (§10.1) | **pclxs** | the living prioritization list |
| `t/` unit XSUBs + differential-vs-perl runner (§13) | **pclxs** | runnable with system perl only |
| host-porting guide (§12 Phase 5) | **pclxs** | `docs/porting-a-host.md`; refhost is its worked example |
| host-neutral design text (§2, §4–6, §8–11 of the design doc) | **pclxs** | adapted copy as `docs/design.md`; scrub PCL-internal references |
| `cl/pcl-xs.lisp` (§7): handle table, callables, trampoline | **pcl** | |
| `XSLoader::load` integration (§7.4), module path search | **pcl** | |
| `tools/pcl-xs-build` | **pcl** | thin wrapper: calls pclxs's `xs-build`, then transpiles the dist's `.pm` files |
| `tools/build-pclxs` (§9 bootstrap) + path resolver + `xs-pin` | **pcl** | builds the sibling checkout locally; enforces the pin (§4) |
| `Pl/t/xs-01.t`, `bench/xs-call.pl` | **pcl** | end-to-end acceptance on the real host |
| `docs/xs-shim-design.md` | **pcl** | stays normative; gets a header pointer to the repo |

Rule of thumb: if a non-PCL host would need it, it's pclxs's; if it
mentions a p-box, it's pcl's.

## 3. The reference host (`refhost/`) — the one addition to the design

The design's tests assume PCL as the host.  A public repo needs its own
host, so contributors and CI can exercise every vtable entry without PCL:

- **`refhost/refhost.c`** implements the full `pclxs_host_vtable` in
  plain C: a growable object table (tagged union: undef/iv/nv/byte-string/
  array/hash/code/ref + class slot), perl coercions per the design's
  ir-spec transcription (it can share `src/coerce.c`), a trivial
  `call` that dispatches to C test callbacks, no GC (arena freed at exit —
  it's a test rig, not a runtime).
- **`refhost/main.c`**: a driver that loads a shim-built module `.so`,
  boots it, and invokes named XSUBs with arguments from the command line /
  a tiny script format, printing results — enough for `t/*.t` (written in
  Perl, run with `prove`) to drive black-box comparisons.
- **Dual purpose:** it is also the *worked example* for
  `docs/porting-a-host.md` — a host author ports by reading refhost next
  to the ownership rules.  Keep it boring and heavily commented.
- **Scope guard:** refhost is NOT a perl. It implements exactly the vtable
  + coercion table, nothing else. Feature requests against it that exceed
  the vtable are misfiled shim requests.

The differential oracle (§13) stays: every `t/` case that can also compile
against real perl (`perl.h` from `libperl-dev`) runs both ways and diffs
output.  CI runs three legs per case where applicable: shim+refhost,
real-perl, and (locally only, not CI) shim+PCL.

## 4. How PCL consumes pclxs (sibling checkout + configured path + pin file)

**Decided with the user (2026-07-12): a separate sibling checkout located
via a configured path** — not a git submodule.  During Phases 1–4 both
repos change together nearly every session; a submodule would demand a
pin-bump commit in pcl for every pclxs commit and punish every machine that
forgets `git submodule update`.  A plain checkout plus an explicit pin
check gives the same reproducibility with none of that friction:

- **Location:** default `../pclxs` (sibling of the pcl checkout),
  overridable with the `PCLXS_DIR` environment variable.  One resolver
  owns this rule — a small `Pl/` helper (or `tools/` library) that
  `tools/build-pclxs`, `tools/pcl-xs-build`, and the runtime loader all
  call; no second copy of the path logic anywhere.
- **Pin file:** `pcl/xs-pin` (checked in), two lines: the expected pclxs
  **commit hash** and the expected **`PCLXS_ABI_VERSION`**.
  `tools/build-pclxs` compares the checkout's `git rev-parse HEAD` and
  header ABI against the pin — mismatch is a hard error naming both values
  (with a `--pin-here` flag to update the file deliberately).  This is the
  reproducibility a submodule would have provided, made explicit.
- **Runtime is belt-and-braces:** independent of the pin, the loader
  refuses ABI-mismatched artifacts (design §7.4 platform+ABI tag) and
  falls through to the pure-Perl path, reporting why.  A stale or missing
  pclxs checkout can therefore never load a wrong `.so` — worst case is
  the status quo (XS module unavailable, fallback fires).
- **Build flow:** `tools/build-pclxs` compiles `libpclxs.so` inside the
  pclxs checkout on the target machine with `%Config`-derived flags
  ("local build only" decision unchanged; nothing prebuilt is shipped).
- **Cross-repo change protocol:** land the pclxs side first (its CI
  green), then one pcl commit with the adapter change + `xs-pin` bump.
  Never point `xs-pin` at an unmerged pclxs branch.  Vtable/ABI changes
  bump `PCLXS_ABI_VERSION` in pclxs and `xs-pin` in pcl in that pair of
  commits.

## 5. GitHub bootstrap (session 0 for Opus 4.8 — half a day)

Environment facts (verified 2026-07-12): pcl has **no git remote**; **`gh`
is not installed**.  The user has approved installing from apt/CPAN.

1. **Installs** (ask-then-run per project convention):
   `sudo apt install gh libperl-dev` (gh for repo admin; libperl-dev gives
   real perl headers for the differential leg).  `ExtUtils::ParseXS`
   (xsubpp) is core perl — verify with
   `perl -MExtUtils::ParseXS -e 'print $ExtUtils::ParseXS::VERSION'`.
2. **Auth (user action):** `gh auth login` is interactive — the user runs
   it themselves (in a Claude Code session: type `! gh auth login`).
3. **Create the repo:** `gh repo create <owner>/pclxs --public
   --description "Run CPAN XS extensions on non-perl hosts: a
   source-recompile shim with a host-neutral C vtable"` — owner + license
   are user decisions (§8).  Protect `main` (PRs + CI required) once CI
   exists.
4. **Initial commit** (skeleton, no functionality yet):
   - `README.md`: the §0/§4 pitch condensed — what it is, the
     handles-not-pointers idea, HPy/JNI prior art, status table of the
     phase plan, "PCL is the first host" with a one-line description of
     PCL (no link yet — pcl is private).
   - `docs/design.md`: host-neutral adaptation of `xs-shim-design.md`
     (§§2, 4–6, 8–11, 13–14; leave out PCL integration §1, §7, and
     PCL-internal memory notes).  State clearly that hosts own coercion
     semantics and the coercion table in the doc is normative for them.
   - `docs/porting-a-host.md`: stub pointing at refhost (filled in
     Phase 5).
   - `include/pclxs/pclxs_host.h`: the §5 vtable **verbatim**, with
     ownership rules 5.1 as comments.  This file IS the product; commit it
     first and let CI compile it standalone (`-Wall -Wextra -Werror
     -std=c99 -pedantic`) from day one.
   - `include/pclxs/{EXTERN.h, perl.h, XSUB.h}` stubs: types +
     `#error "pclxs: unimplemented API %s — see docs/design.md §10"`
     pattern for everything not yet provided (design §10.1).
   - `LICENSE`, `CONTRIBUTING.md` (§7 below), `.github/workflows/ci.yml`
     (§6 below), `Makefile` (or `build.pl` — Perl, per project rules)
     using `%Config{cc,cccdlflags,lddlflags,dlext}`.
5. **Wire pcl to it:** clone pclxs as a sibling (`../pclxs`); add the
   path resolver + `pcl/xs-pin` + `tools/build-pclxs` (§4); update the
   header pointer line in `docs/xs-shim-design.md` with the repo URL.
6. **Memory/docs:** update `project_xs_shim_design` memory topic + the
   MEMORY.md XS line with the repo URL and the two-repo change protocol.

## 6. CI (GitHub Actions, in pclxs from day one)

- **Matrix:** `ubuntu-latest` + `macos-latest` (design §9 targets
  Linux/BSD/macOS; macOS on Actions is the cheap second data point —
  it exercises `%Config` flag portability and `.dylib`/dlext naming).
  A FreeBSD leg via a VM action is a later nice-to-have.
- **Steps:** build `libpclxs` (`-Werror`); compile a trivial `.c`
  including `pclxs/perl.h` (Phase-0 acceptance, kept forever);
  build refhost; build test XSUBs; `prove t/`; where `libperl-dev`
  exists, build the same `.xs` against real perl and run the differential
  leg.
- **Census check:** a CI step that runs `tools/xs-api-census.pl` against
  the checked-in `census/` files and fails if a header claims to provide a
  symbol the census marks Tier X (keeps the refusal list honest).
- No PCL leg in CI (private).  The pcl-side gate (`Pl/t/xs-01.t` etc.)
  runs locally per the standing pcl session rules.

## 7. Contribution surface (make outside fixes easy and safe)

- **CONTRIBUTING.md** states: the vtable is versioned and frozen between
  ABI bumps (PRs changing `pclxs_host.h` need a version bump + host
  sign-off); every vtable entry must keep at least one `t/` exercise;
  differential tests against real perl are the correctness oracle; crash
  reproducers get minimized into `t/` before fixing.
- **Census-driven issues:** each unimplemented-but-demanded API symbol
  (from `census/`) becomes a labeled issue (`api-gap`), so "other people
  can fix it" has a concrete shape: pick an `api-gap` issue, implement the
  function against the documented perlapi semantics, add the `t/` case,
  green CI.  The `#error` stubs in `perl.h` make the gap searchable from a
  failed module build straight to the issue.
- **Module reports:** an issue template for "dist X fails to build/run"
  requiring the `xs-build` report output (design §9 step 5), which
  already classifies Tier X symbol usage.

## 8. User decisions needed before session 0

1. **GitHub owner:** personal account or a new org?  (Repo name:
   **decided, `pclxs`** — verified free.)
2. **License:** recommendation **MIT** — maximally simple for a C library
   meant to be embedded by many hosts.  Alternative: perl's own
   "GPL-1.0-or-later OR Artistic-1.0-Perl" dual license for perl-community
   familiarity.  (No perl code is copied — the headers reimplement a
   documented API; xsubpp runs as an external tool from the user's system
   perl and its output is compiled, not vendored.)
3. **Public from day one, or after Phase 1?**  Recommendation: public
   from day one — the vtable header + design doc + refhost are exactly
   what attracts early host-author feedback, and there is nothing secret.
4. **PCL attribution in the README** — how much to say about the
   private first host (name-only vs a paragraph).

## 9. Phase re-cut across the two repos

Same phases and acceptance as design §12, with each split into a
**pclxs deliverable** (CI-gated) and a **pcl deliverable**
(local-gate); a phase is done only when both are green.

| phase | pclxs repo (CI acceptance) | pcl repo (local acceptance) |
|---|---|---|
| **0 census & skeleton** | repo bootstrap §5; headers compile standalone; `xs-api-census.pl` + `census/` for the ladder (built against real perl — needs `libperl-dev`) | sibling checkout + resolver + `xs-pin` wired; `tools/build-pclxs` builds it |
| **1 hand-written XSUB e2e** | native SVs, stacks, `pclxs_init/invoke_xsub`, croak/setjmp, ~20 scalar vtable entries; **refhost**; `t/arith.t` green via refhost AND differential-vs-perl | `cl/pcl-xs.lisp` (handle table + callables + trampoline); `Pl/t/xs-01.t` green incl. croak-catch + callback |
| **2 xsubpp + build tool** | `tools/xs-build` one-command dist build; `t/coerce.t` (C-side coercions vs real perl over the nasty-input table) | `tools/pcl-xs-build` wrapper (+ `.pm` transpile); coerce answers also diffed vs PCL runtime |
| **3 aggregates + Digest::MD5** | AV/HV group, refs/bless/isa, `get_global`, `sv_setref_pv`; refhost grows aggregates; Digest::MD5 *builds* in CI | loader integration (§7.4); Digest::MD5's own `t/*.t` run under the PCL sweep |
| **4 callbacks + string-heavy** | `call` flags complete, `eval_string`, SvGROW/SvPVX hardening; Time::HiRes (or swap per design §14.2) builds | module dist tests pass-rate reported; JSON::XS stretch, failures census-classified |
| **5 docs + registry** | `docs/porting-a-host.md` complete (refhost as the worked example); README status table final | `docs/not-supported.md` §DynaLoader rewrite, `shipped-modules.md` provider kind, `extensions.md`, CLAUDE.md |

Standing pcl rules apply unchanged on the pcl side (paren discipline for
`pcl-xs.lisp`, prove-core gate, one commit per item, session log).  On the
pclxs side the equivalents are: `-Werror` clean, CI green, one PR/commit
per item, and the design doc's §6.6 discipline (no unwinding across the
boundary — treat any "it seems to survive" shortcut as a bug).

## 10. Session budget

| item | sessions |
|---|---|
| session 0 bootstrap (§5) + Phase 0 census | 1 |
| Phase 1 (both repos; the design's biggest single phase) | 3–4 |
| Phase 2 | 1–2 |
| Phase 3 | 2–3 |
| Phase 4 | 2–3 |
| Phase 5 | 1 |
| **total** | **10–14** |

(Design doc gave no totals; these are sized against the v2 endgame's
calibration unit.  Phase 1 dominates: three interacting mechanisms —
stacks, croak/setjmp, handle lifecycle — must land together before
anything runs.)
