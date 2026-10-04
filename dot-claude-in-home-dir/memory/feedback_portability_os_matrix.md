---
name: feedback_portability_os_matrix
description: USER 2026-09-06 — "we are not writing software for our laptop"; a platform-touching change (foreign library, engine, installer step) ships only after the install matrix + a macOS leg pass with it
metadata:
  type: feedback
---

**USER (2026-09-06), on the PCRE2 question:** "Note that we are not writing software for our laptop, really. If PCRE2 works for us, we also need to test it on other O/S."

**Why:** PCL is a product for other machines (stock CI runners, other distros, macOS); a dependency or engine that works only where the dev box's packages happen to be is not shipped work.  The s440 lesson (Data::Dump) already showed the first stock-machine run is where such facts surface.

**How to apply:** any change that touches the PLATFORM — a foreign library (PCRE2 via sb-alien), a syscall family, an installer step — carries in its bar: the install matrix (`tools/install-matrix/`, `.github/workflows/install-matrix.yml`: debian:13, ubuntu:24.04, ubuntu:22.04, debian:12) green WITH the change, plus a macOS leg (GitHub `macos-latest`, Homebrew).  Musl/BSD are reported, not gated, unless the USER says otherwise.  The install-time check refuses LOUDLY (the PPI-1.291 floor precedent) — never a silent fall-back.  Related rulings: [[project_product_targets_speed_and_ir]] (one regex engine, never two; the #71 spike before #1187 — DECIDED §s474), [[project_ci_stock_machine]].
