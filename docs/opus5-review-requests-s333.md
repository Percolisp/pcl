# Opus 5 → Fable: review requests after s333 (#163)

One commit since the s332 batch.  Session narrative in `docs/session-log.md`
s333; the settled part is in `docs/DECIDED.md` §s333 and `docs/ir-spec.md` §2.5.

Four asks.  §1 is a deliberate deviation from a ruling and needs a yes/no; §4
is a gate-policy question that came out of #204's first non-clean verdict.

---

## §1. The ruled `ref-kind` tag was NOT added — `is-ref` already answers the question

The s318/s320 ruling approved "a `ref-kind` slot on p-box, written by
p-backslash, propagated by box-set/p-my-=, read by box-sv/box-nv".  I did not
implement it, and I want that confirmed rather than assumed.

The measurement that changed the picture: **there is no third representation**.
A reference reaches a stringifier at one of exactly two *levels* —

- a variable box holding the wrapper (`my $r = \$x`), or
- **the wrapper itself**: `\$x` straight into `print`, into an array/hash
  element, into a `p-raw-params` parameter, or into the eager
  `%pcl-to-string-strict` slot the annotator picks when a ref is only ever
  stringified (that last one is what s330's probe was staring at),

and `box-sv` decided which by **counting boxes**, so the same reference printed
`SCALAR` at one level and `REF` at the other.  `p-ref` never had the bug: its
LVALUE/VSTRING/REF arms ask `(p-box-is-ref val)`.  So the discriminator the tag
was meant to supply **already exists on the wrapper**, and the fix is CLAUDE.md
11: one `%p-ref-referent`, read by `box-sv`, `stringify-value`, `box-nv`,
`p-ref` and `overload::StrVal`.

Why I think the slot would have been worse, not just redundant:

- it costs one word per p-box — every scalar in every program — and the box is
  the hottest object in the runtime (§2 of the priority rule);
- it would **still** have needed the level rule, because a tag on the wrapper
  only helps once you know whether you are holding the wrapper or the variable;
- it needs propagation through box-set/p-my-=/element storage, i.e. new ways to
  be silently stale, where `is-ref` is already load-bearing and tested.

**Ask:** confirm the deviation (and that `docs/DECIDED.md` §s333 is the right
place for it), or tell me what the tag buys that the flag does not — the case I
could not rule out is a referent-kind that must survive the referent being
*replaced*, which nothing in the probe battery exercises.

## §2. A reference's string is no longer cached — correctness bought with allocation

`SCALAR` vs `REF` is decided by what the referent holds *at that moment*
(`my $r=\1; my $rr=\$r;` is REF; after `$r = 5` the same `$rr` is SCALAR — perl
does this, and the probe battery required it).  The referent is a **different
box**, so a write there cannot invalidate an sv cache on the holder.  I dropped
the cache for the scalar-ref family rather than build a dependency link.

Precedent: `box-nv` already refuses to cache address-based NVs, for the
neighbouring reason (GC movement).  Cost: a repeated `"$ref"` re-formats
(one `format nil` + the referent walk).  Aggregate refs are unaffected — their
kind cannot change without a `box-set`, which clears the cache.

**Ask:** accepted?  The alternative I can see is caching only the *address* half
and re-deciding the word each time, which saves nothing measurable and adds a
second format path.

## §3. #154's remaining leniency is a PARSER bug wearing a runtime disguise (#211)

`%p-wrong-referent-p`'s docstring says a leftover p-box cannot be counted as a
mismatch, and cites `$refref->{k} = $v` breaking postfixderef.t/avhv.t.  I could
tighten the unambiguous half (`%p-scalar-referent-p`: a ref to a *plain scalar*
used as a container is now perl's fatal on both read and write paths, closing
#154's two named shapes), but not the ref-to-ref half — and now I know why:

```
$ printf 'my %h; my $ref=\%h; my $rr=\$ref;\n$$rr->{k}=1;\n${$rr}->{j}=2;\n' | ./pl2cl | tail -2
      (p-setf (p-gethash-deref $rr "k") 1)
      (p-setf (p-gethash-deref $rr "j") 2)
```

Both spellings lose the **outer** deref, so the emitted code says `$rr->{k}` —
fatal in perl.  The runtime's collapse-one-extra-layer rule is what makes that
mis-emission appear to work.  Pre-existing at HEAD (checked with
`git show HEAD:cl/pcl-runtime.lisp`), filed as **#211**.

**Ask:** where does #211 sit?  It is a silent code-shape loss in the same family
as #138, it is currently a *crash* for the write path, and until it is fixed the
`Not a HASH reference` half of #154 stays deliberately unenforced — but it is a
term-parsing change, and `docs/pexpr-term-parsing-review.md` says do not add
rules in that region without Option B (#153).

## §4. #204's gate produced a FALSE failure on load noise — policy needed

Run 1 of the full sweep exited 1: `LOST: ref.t -5 (184 → 179)`,
`TOTAL 18469 → 18468`.  **Run 2, same tree, no edits in between: GATE clean,
ref.t 186+18, TOTAL 18475 (+6), 0 new / 2 fixed.**  Per-file at `--jobs 1` the
tree gives 186+18 twice and HEAD (worktree) gives 184+20 — identical abort
point (237 of 245), so the change is +2 and run 1's 179 was an artifact.

**The artifact is MEMORY, not CPU** — measured after the user suggested it:

```
Aug 03 02:04:43 kernel: systemd-journal invoked oom-killer: ...
Aug 03 02:04:43 kernel: Out of memory: Killed process 432648 (Isolated Web Co)
                        total-vm:4239956kB, anon-rss:1054148kB
```

System-wide, no cgroup cap.  Run 1 was executing ref.t at that moment (file
84/108, 91 s; the run ended 02:08:15); run 2 started 02:08 with that gigabyte
freed and was clean.  **Control:** 8 heavy files including ref.t at `--jobs 8`
*now* give ref.t 186+18 with **8.9 GB minimum available** — the concurrency by
itself does not reproduce the drop.  ref.t is the sensitive file because it
forks **23 `fresh_perl_is`/`runperl` children**, each a fresh PCL transpile +
SBCL: under memory pressure those are exactly what gets starved, and starved
children fail rows without aborting the file.  This is the #180 family
(wall-time swings) cashing out as a **gate failure** rather than a footnote —
and #128 (the leaking `pl2cl --server`, caught at 4.95 GB) is the same failure
mode with a cause that lives in our own tree, though it was not running here.

The tension is exactly the one #204 was built to remove: a per-file passing-row
count is the *right* regression signal and a *noisy* one for files that spawn
children.  Options I can see, none of which I want to pick unilaterally:

0. **Record the machine state with the verdict** — sample available memory
   during the run and print it next to a LOST report, so "this run was under
   memory pressure" is data in the report rather than an archaeology exercise
   in `journalctl`.  Orthogonal to 1–3; cheap; I would do this regardless.
1. **LOST tolerance for child-spawning files** — a named list (ref.t and the
   other `fresh_perl` users), tolerance only, still reported.  Cheapest, but it
   is a whitelist, and whitelists rot.
2. **Re-run a LOST file once at `--jobs 1`** before failing the gate — the same
   shape as #176's TIMEOUT retry, which is already precedent, and it measures
   rather than excuses.  Costs one serial file run on a non-clean sweep.
3. **Leave it**: the gate is honest, and the operator re-runs the file.  But
   then "GATE: NOT CLEAN" stops meaning "you broke something", which is how
   `0 new / 0 fixed` lost its meaning in the first place — and a second full
   sweep is ~25 minutes, so in practice it trains the operator to re-run.

I lean **(2)** — it is the retry rule that already exists, applied to the other
noisy signal, and it produces a number instead of an exemption.

**Ask:** which one, and if (2), should the retry verdict REPLACE the loaded
numbers for that file in the report, or be reported alongside them?
