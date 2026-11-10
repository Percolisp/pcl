# PAUSE-s538.md -- the reviewing session's running notes (s538, started 2026-11-10 23:4x)

RESUME RECIPE after a cut: read this file bottom-up, then `s538/resume-s538.md`, `git worktree list`, each batch's `scratch/<label>/STOP.md` first line and its newest logs' END lines.

## 23:4x -- START.  The USER: continue.
* The box was rebooted (up 23:22); `/tmp` empty; no heavy leg, no SBCL, no `HEAVY.holder*`; 7 GB available.  heavy.sh = three slots.  `git worktree prune` removed the dead `/tmp` ref-tree.
* Main `9cd2f311` = MAIN-READY-85 `bdd36d06` + records (code `4a39a1c7`, v2-13300).  origin/main = `9cd2f311`; CI GREEN on it (public API, 23:4x); install-matrix GREEN on `4b1f7493` (its last trigger).
* Written: `s538/resume-s538.md` (the delta since `s537/resume-after-stop.md`).
* RELAUNCHED, each in its existing worktree (no isolation), model opus / effort xhigh (the USER's s537 word, standing): **s536i**, **s536j**, **s537a**.
* ROUND BOARD resumed at chunk b (`~/pcl-agent-scratch/s537/round/round-s538.sh`, label `s538-round`, chunks b-f on the `7284c977` extraction; the killed `board-b.log` renamed `board-b.void-0117.log`).
* NEXT: merge in MERGE-READY order with the usual review; s537b into the first freed slot; the companion `--all --quick` on merged main when no agent has a leg queued.

## 23:5x -- s536i PRE-REVIEWED while it takes its legs (tree `88108e97`, code final unless a leg fails).
* `Pl/t` diff read: only five plan counts removed; rows added to autoviv-02, blessed-copy-01, elem-assign-lvalue-01, list-slice-01, lvalue-ref-01, proto-alias-01.  Nothing weakened.
* Probes perl -> main -> tree (`~/pcl-agent-scratch/s538/review/i/p1..p6.pl`, `p3b.pl`): tree = perl on p1 (#4682 list slices), p2 (#4684 magic lvalues), p4 (#4683 blessed hash spread), p5 (#4681 `++` on an assignment), p6 (#4283 + #4741, incl. the breaking shapes `$a[$k++] = 1`, `$g{$key .= "x"}++`, `\$b[$#b + 1]`, nested subscripts); main wrong on all six files.  ONE miss in p3 (#4685): `\(scalar($x))` / `\(scalar($a[0]))` -- the parenthesised one-element form -- still references a copy; every other spelling is right.  Pre-existing on main: **#4783 FILED**, not held against the batch.
* Left for its merge: the bars in its report, ff, CLAUDE.md + STATUS numbers, its tasks COMPLETED by me.

## 23:5x (2) -- s536j PRE-REVIEWED while it establishes its logs (tree `094108ec`).
* Probes perl -> main -> tree (`~/pcl-agent-scratch/s538/review/j/p1..p4.pl`, `q1..q13.pl`): tree = perl on p1 (#4701 `-Foo::bar`, `-CORE::log(1)`, hash keys, `5 -Foo::bar`) and p3 (#4482 `__DATA__ =>` / `__END__ =>` at a line start, the real `__DATA__` section still read); main wrong on both.  #4542: the statement-start shapes `x3();` and `x3;` are fixed (main drops `x3;`), eight control shapes identical on both.  p4 (#4302 CR in literals) tree = main, as its STOP.md says (NOT fixed -> #4761).  No regression found.
* Residue, pre-existing on both: **#4784 FILED** -- `x<N>` in a list-operator argument slot (`print x64()`, `print x3() x2` drop loudly; `print STDOUT x3()` prints NOTHING, a silent wrong); PPI lexes Operator `x` + Number there.  For the next PPI batch.
* Note for the #4302 / #4761 owner: perl itself reads a CR LF pair in a source FILE as LF inside `q{}` and a here-doc (probed: `q{a<CR><LF>b}` is length 3) and keeps a lone CR, a CR in an eval STRING and a CR in `__DATA__`; the fix is not "keep every CR".
