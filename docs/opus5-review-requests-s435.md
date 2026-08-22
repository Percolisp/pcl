# s435 (Opus 5, 2026-08-23) — Q2: THE FLIP, shipped but NOT CLOSED

The announce→DIE flip (Option B phase 2's last step, ruled
`docs/fable-answers-s433.md` §A.1) is IN THE TREE and measured over two of the
four required populations.  **Q2 is not closed**: three bar legs are unrun,
listed in §6.  Read that section before doing anything else.

## 0. The session started with a machine hang, and it was our own bug

An in-flight edit left `Pl::Parser::_drop_site` calling ITSELF.  Perl has no
tail-call elimination, so the process climbed to **7.76 GB anon RSS / 9.7 GB
VM**, exhausted the 4 GB swap (`Free swap = 200kB`) and hung the machine until
the kernel's global OOM killer took it (`dmesg`: task=perl, pid 422916,
2026-08-23 00:11:27).

Repaired: `_drop_site` holds the real site computation and
`_announce_dropped_statement` calls it — the "ONE builder" its own comment
already described.  Fable verified the extracted body byte-equivalent to the
pre-flip inline copy.

**The general lesson is `#471`**: the compiler side has NO memory cap, so any
runaway in `Pl/**` costs the MACHINE rather than the run.  Only
`tools/run-perl-suite.pl` has a guard (a `systemd-run` MemoryMax scope + a
per-child `ulimit -v`).  Under a cap the same bug dies in ~5 s naming itself
("Deep recursion on subroutine `Pl::Parser::_drop_site`" + "Out of memory in
perl").  Unbounded, that warning fired at depth 100, scrolled past and changed
nothing — **a warning does not stop an allocation**.  Fable RULED the fix
shape: option (a), at the `pl2cl` seam (every compiler-side tool spawns
`./pl2cl`, so that IS the perl-side analogue of `PCLSbcl.pm`), as its own
filler commit — not inside Q2, not behind the release.

## 1. What shipped

* **The flip.** Both `PARSE ERROR` emitters replace the statement with
  `(progn ;; PARSE ERROR: <reason> (pcl:p-die "PCL: statement not supported at
  F line N: <text> -- <reason>\n"))`.  The comment is byte-identical — all
  four counters (`drop-census`, `corpus-diff`'s SILENT-DROP, both runners'
  `drops` column) key on that text alone, never on the `nil`.  One shape,
  every mode, no classifier.
* **`p-die`'s no-location branch was `(error msg)` — A REAL BUG Fable's review
  found.** The message became a CL FORMAT CONTROL string.  This flip is the
  first emitter to feed arbitrary user SOURCE TEXT through it, so a dropped
  statement containing `~` (i.e. every `=~`, common in the census families)
  raised an **untrappable `sb-format:format-error` that killed the whole
  file** — defeating exactly the trappability §A.1 is built on.  Now
  `(error "~A" msg)`.  Audited first: all 12 internal `p-die` callers with a
  `~` pre-resolve it through an inner `(format nil …)`, so none relied on
  directive expansion; several build their message from user data (a method
  name, a module name), so the fix closes a latent hole for them too.
* `Pl::ExprToCL::_cl_string_literal` → public **`cl_string_literal`** (it has a
  cross-module caller now; renamed, not aliased — an alias is the duplication
  rule 11 forbids).
* Docs: `not-supported.md` §Lvalue subroutines + §Indirect object,
  `ir-spec.md` **§9.3 (new, normative)**, the census header.
  `docs/drop-census-s419-flip-gate.md`'s family-1 line said the lvalue family
  was "permanently EXEMPT from the flip" — **struck, with a pointer**: §A.1
  killed emitter-side classifiers, and the Q2 plan directs exactly this edit.
  Left live, a future session would have re-derived the exemption.
* Guard `Pl/t/drop-die-01.t` (10 rows), generation **v2-178**, all three
  artifacts regenerated (only the stamp moved — lib/ has zero drops).

## 2. Gate + corpus-diff

Cold gate **161 files / 5715 tests**, only the 13 known pclxs xs rows failing
(+1 file, +10 rows = the guard).  `corpus-diff`: exactly the 4 perl-tests
census files, one-line shape each, SILENT-DROP count unchanged at 5.

## 3. THE PRICE, and the cost model the ruling did not have

**perl-tests sweep: TOTAL passing 18366 → 18311 (−55).**

| file | Δ | cause |
|---|---:|---|
| substr.t | −28, OK→PARTIAL (353/397) | 2 lvalue drops (`:697`, `:900`), census family 1, a REGISTERED absence |
| method.t | −25 (124/163) | 1 indirect-object drop (`:72`), family 10, owner **#399** |
| split.t | −1 | `fresh_perl_is(…,'')` child — accidental pass, now honest |
| bop.t | −1 | same, child `formline …` (`:701`, test 509) |

**The multiplier**: the unit of the DECISION is the statement, but the unit of
the LOSS is the enclosing TOP-LEVEL FORM — and a file-level `my` compiles to
one `let` around the whole remainder of the file.  method.t is the clean
demonstration: **1 drop, 39 unrun rows**.

Fable RULED **accept, no narrowing** — every narrowing re-creates the sin
(deferred/statement-level recovery = running on past absent code = the
announce arm §A.1 rejected; an emitter classifier stays forbidden;
restructuring file-level `my` is a large change aimed at a temporary debit).
The anchor: **perl's own loss unit for an untrapped die is the whole remaining
program**, so PCL post-flip with per-form recovery (#467) loses strictly
LESS than perl would given the same program.  Standing rule: a file whose loss
is out of proportion **PROMOTES its drop's owner task; it does not bend the
flip**.

Both baselines edited ROW BY ROW with causes naming each drop's `file:line`
and owner; the two honest new failures are in `fail-baseline.tsv`.

## 4. The companion population: −117 C_ok rows, ALL attributed

`--all --quick --jobs 4`: 15 movers vs the snapshot, each re-run alone by the
runner (#366) and agreeing.  **Every one of the 13 non-TIMEOUT movers
reproduces its SNAPSHOT value on a HEAD worktree** — so all are this change,
with no pre-existing mover spliced in ("pre-existing is WHEN, not WHY").

| file | snap C_ok | now | Δ | drops |
|---|---:|---:|---:|---:|
| op/substr.t | 377 | 347 | −30 | 2 |
| op/method.t | 124 | 99 | −25 | 1 |
| op/universal.t | 79 | 61 | −18 | 1 |
| op/sub_lval.t | 67 | 50 | −17 | 35 |
| comp/parser.t | 65 | 49 | −16 | 5 |
| op/signatures.t | 920 | 915 | −5 | 1 |
| io/pvbm.t | 23 | 20 | −3 | 0 |
| op/split.t | 213 | 212 | −1 | 0 |
| op/stash.t | 48 | 47 | −1 | 0 |
| re/reg_eval.t | 2 | 1 | −1 | 0 |
| op/lexsub.t / op/try.t / run/runenv_randseed.t | — | — | C_ok flat, 1 / 2 / 5 rows VANISHED (fail count fell) | 4 / 1 / 0 |

`op/utf8cache.t` and `uni/variables.t` are TIMEOUT both sides, C_ok unchanged.

**Out of proportion, so their owners are promoted per the standing rule**:
op/universal.t (−18 for ONE drop) and comp/parser.t (−16 for five, in a file
whose drops are the family-6 deliberate parse-error torture rows).
op/substr.t/op/method.t are the twins of the perl-tests files above.
The four zero-drop losers (io/pvbm, op/split, op/stash, re/reg_eval) are the
**child-program population** — `re/reg_eval.t`'s own snapshot signature says
all 8 of its cases drive `fresh_perl_is`.

## 5. `#472` — the census has a SIXTH, uncounted population

Every drop instrument reads emitted `.lisp` FILES.  A `fresh_perl`/`runperl`
child is transpiled from a STRING at run time, so its drops are in no
population.  Two are known — perl-tests/split.t:682 and bop.t:701 — and
**both were rows passing on nothing for years**: they assert the child prints
nothing, PCL dropped the child's only statement, so it printed nothing and the
row passed.  The flip is what exposed them.  Size unknown; #472 is the
instrument (side channel, NOT the child's stderr — that is the child's output
and would change verdicts).  The census header says so until it lands.

## 6. WHAT IS NOT DONE — Q2 stays open

1. **Four-population emission A/B** (`tools/emission-ab.pl --ref HEAD`).
   Expect diffs in exactly the 39 census files, one-line shape each.
2. **The three-dist board re-run** (Text::Balanced's movers are #457's price —
   explain per row, §A.1).
3. **A confirming full perl-tests sweep** against the edited baselines
   (expect GATE clean; the numbers in §3 are what it must reproduce).
4. **`docs/perl-suite-run.tsv` is NOT edited.** The 13 companion movers in §4
   are attributed but their snapshot rows still hold the old values, so the
   next companion run will report them again.  Deliberate: it keeps the debt
   visible.  Edit them ROW BY ROW with the causes above — never re-bless.

Nothing here is blocked; it is unrun for time.
