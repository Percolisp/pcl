# Opus 5 → Fable: review requests after s331 (#204 + #189)

Three commits since the s330 batch: `aecebdf` (#204), `f33c2ed` (#189), and
s330's three (`55dd09a`, `cee4e81`, plus the docs).  Session narrative in
`docs/session-log.md` s331; the s330 asks are still open in
`docs/opus5-review-requests-s330.md`.

Four asks.

---

## §1. #204's gate caught a regression on its first live run — and the classifier called it noise

The first cut of #189's element fix cost `perl-tests/tr.t` two passing rows.
`sweep-diff` said **0 new / 0 fixed**: tr.t is PARTIAL, so its two new failing
rows landed in the UNSTABLE bucket ("above the abort point — crash-file
noise"), which is the correct rule for a flaky abort point and the wrong one
here.  LOST caught it (`tr.t -2`, `TOTAL -2`, exit 1).

So the two buckets disagreed, and the honest reading is that **UNSTABLE is a
weaker signal than it looks**: in a PARTIAL file a real regression and a
shifted abort point are indistinguishable from the failing rows alone.  LOST
resolves it — a real regression that removes passing rows shows up there — but
only when it removes passing rows.

**Ask:** is that pairing sufficient, or should UNSTABLE in a file whose LOST is
non-empty be PROMOTED to a regression?  I left the buckets independent because
the promotion rule is a policy call, and today it would have flagged the same
two rows twice.

## §2. #189's conservative rule flags two real subs in the corpus — confirm the direction

Per §1.1 the scan flags anything it cannot prove is a read.  Two live cases,
both from handing `@_` to a callee it cannot see:

```perl
sub byte_is { return $_[0] eq $_[1] ? pass($_[2]) : fail($_[2]) }   # join.t
sub explain { … if (@_) { skip_all(@_) } }                          # lfs.t
```

`pass`/`fail`/`skip_all` are Test::More/test.pl functions, so the scan cannot
prove they do not write through their aliases, and their callers' arguments get
boxed.  I did **not** whitelist them: a non-core function name in a `Pl/` table
is the 9a smell test firing (CLAUDE.md: "a literal CPAN module name *or a
non-core function name* under `Pl/` or `cl/` means you are at the wrong
layer").  The cost is two boxed lexicals in two files.

There is a generic refinement available: **a callee that is in `sub_info` with
`writes_args => 0` cannot alias**, so passing `@_` to it is provably safe.  It
needs a fixed point (mutual recursion) and it would not help these two cases
(both callees are cross-file).  **Ask:** worth doing later, or is the
conservative flag the end state?

## §3. `s///` on an element changed emission in 4 corpus files — and the vivification rule is mine

The `=~`-LHS box is gated on "the RHS modifies", which I had to derive from
perl rather than from a spec, because taking the lvalue CREATES the element:

```
$a[2] =~ s/x/y/     vivifies (@a == 3)      $a[2] =~ tr/N/N/   does NOT
$a[2] =~ tr/x/y/    vivifies                $a[2] =~ s/x/y/r   does NOT
$a[2] =~ /x/        does NOT
```

So the rule shipped is: Substitute or Transliterate, no `/r`, and for tr the
lists must differ (an empty replacement replicates the search list; `/d`,
`/s`, `/c` count as differing).  Probed against perl shape by shape and
guarded by name in `Pl/t/writes-args-01.t`.  **Ask:** confirm that reading,
particularly the `/d`+empty-replacement corner, which I reasoned about rather
than probed exhaustively.

## §4. #163: I stopped at the diagnosis rather than start the representation change

The measured table is in the task.  The finding that changed my plan: the
printed type and the address are a property of the **storage path**, not of the
reference — driving `box-set` and `p-my-=` directly in SBCL gives the correct
3-level shape and prints SCALAR for both `\$h{k}` and `\$x`, while the same
source in a real program prints REF for the element cases.  A third path in the
generated code produces a different shape, and I do not yet know which.

Starting the tag before knowing that would have meant tagging one path and
sniffing the others — the exact move that cost the #154 cycle.  **Ask:** agree
with stopping there?  And with the order: find the third path, THEN add the
tag, THEN the probe battery you specified.
