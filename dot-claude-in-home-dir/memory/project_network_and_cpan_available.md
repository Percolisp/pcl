---
name: project_network_and_cpan_available
description: "Network/CPAN IS reachable in this environment — don't assume the sandbox blocks it"
metadata: 
  node_type: memory
  type: project
  originSessionId: ac932eb0-4a77-41cd-aef3-b89c2e716d88
  modified: 2026-08-02T12:22:56.898Z
---

The dev environment **has working network access and CPAN reachability**. Do NOT
assume the sandbox is offline.

**Why this is here:** in session 233 I wrongly concluded "CPAN is down" from a
single `HTTP::Tiny` GET to `http://cpan.org/` returning 599 — that failed only
because it was plain *http* to that host. The real checks all passed: DNS
resolves (cpan.org, metacpan.org), `curl https://fastapi.metacpan.org/...` → 200,
and `cpanm --info Try::Tiny` → `ETHER/Try-Tiny-0.32.tar.gz`. The user caught it.

**How to apply:** to test connectivity use **https** (curl/HTTP::Tiny to
`https://fastapi.metacpan.org`) or `cpanm --info <Mod>`, not an http GET. `cpanm`
works for installing real CPAN modules (use `--local-lib` / `-l` to sandbox into
e.g. `/tmp/pcl-cpan-test` and keep deps inspectable). perlbrew perl-5.40.3.

**USER ruling (2026-08-02, s324 review round): fetching/unpacking CPAN dists
for measurement is BLANKET-OK'd — no per-dist asks.** Only system-level
installs (apt, `cpan` into perl's site dirs) still need asking first.
Recorded in `docs/fable-answers-s323.md` §5 + DECIDED.md.

Related: [[project_cpan_module_survey]] (testing XS-free CPAN modules through PCL).
