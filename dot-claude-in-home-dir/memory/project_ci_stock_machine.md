---
name: project_ci_stock_machine
description: CI is a STOCK Ubuntu runner (perl 5.38, no dev modules); how to rehearse it locally, what broke the first run, and the deps/fixture rule
metadata:
  type: project
---

**CI (`.github/workflows/ci.yml`, Percolisp/pcl on GitHub) is a stock Ubuntu
runner**: perl 5.38 (Ubuntu 24.04), `cpanm PPI` (≥ 1.291 — apt's is 1.277),
apt Moo, sbcl.org 2.6.0 tarball + fresh Quicklisp, then `tools/install-pcl`,
`tools/t/install-pcl.t`, `tools/prove-core`.  First run (2026-08-23, s439)
was RED: `Pl/Parser.pm` & co. imported non-core `Data::Dump` (present on the
dev machine via a distro package, so a sanitized-HOME rehearsal with the dev
perl could NOT see it) — the installed `pl2cl` died at compile, exit 2.  Fixed
s440 (2026-08-23).

**Why:** the dev perl hides missing dependencies; only a BARE perl shows them.
The job LOG is admin-only (API 403) — `tools/ci-step` re-emits a failing
step's tail as a `::error::` annotation, which IS public
(`GET /repos/Percolisp/pcl/check-runs/<id>/annotations`; jobs via
`/actions/runs/<run>/jobs`).

**How to apply:**
- Deps are EXACTLY PPI (≥ 1.291) + Moo; `Pl/t/core-deps-01.t` guards it
  (compiler + runners load nothing else; a test's `use X` of an installed
  non-core X must carry the fixture guard `eval { require X; 1 }` in a SKIP).
  CPAN fixtures CI installs: Data::Dump, Try::Tiny (apt).
- Local stock rehearsal (~10 min, all in the scratchpad): build perl 5.38.2
  (`./Configure -des -Dprefix=$S/perl538 -Dusethreads && make -j8 && make
  install`), `cpanm --notest PPI Moo` into it, sbcl.org binary
  (`INSTALL_ROOT=$H/sbcl sh install.sh`, `SBCL_HOME=$H/sbcl/lib/sbcl`),
  Quicklisp + cl-ppcre into a sanitized `HOME=$H`; then `PATH=$S/perl538/bin:
  $H/sbcl/bin:/usr/bin:/bin`, unset PERL5LIB/PERLBREW_*, run the installer and
  `tools/prove-core`.  A perl-ORACLE row whose program needs a newer perl
  than the host carries a probed literal (`feature-pragma-01.t` `test_src`).
- Check the run: `curl -s https://api.github.com/repos/Percolisp/pcl/actions/runs?per_page=3`.
