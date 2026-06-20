# `goto LABEL`, declarations, and the codegen-shape problem — description + rewrite plan

**Written:** 2026-06-21 (session 263). **Audience:** a human deciding how much of
this to do for "release 1". **Companion:** `docs/intra-sub-goto-plan.md` (the
terse status), `docs/codegen-rewrite-spec.md` / `docs/type-flow-and-codegen-plan.md`
(the eventual full IR rewrite).

This document has three parts:
1. **The problem**, with worked code examples (what we want, what works, what breaks).
2. **Root cause** — why it breaks, in three layers.
3. **The plan**, tiered, with an explicit "release 1" line.

---

## 1. The problem

### 1.1 What we're trying to compile

Perl's `goto LABEL` jumps to a label inside the current sub. The idiom that
matters for real CPAN code is the **error/exit label** (e.g. every `extract_*`
in `Text::Balanced` is built this way):

```perl
sub f {
    my $x = shift;
    if ($x < 0) { goto FAIL; }   # bail out
    return "ok";
  FAIL:                          # a bare statement label
    return "fail";
}
```

Common Lisp's equivalent is `tagbody`/`go`: a `(go :FAIL)` is only legal inside a
lexically-enclosing `(tagbody … :FAIL …)`. So the body must become:

```lisp
(block nil
  (let (($x (make-p-box nil)))
    (p-my-= $x (p-shift @_))
    (tagbody                       ; <-- inserted around the label + its goto
      (p-if (p-< $x 0) (progn (go :FAIL)))
      (p-return "ok")
      :FAIL)
    (p-return "fail")))            ; post-label code stays OUTSIDE the tagbody
```

Note two non-obvious requirements:
- `:FAIL` must be a **direct element** of the `tagbody` (a `go` cannot jump to a
  tag that is nested inside a `(let …)` or another form within the tagbody).
- The `go` may be **deeper** than the tag (inside the `if`/`progn`) — that's
  fine, `go` reaches outward. But the tag must not be **deeper** than the `go`.

### 1.2 What works today (session 263, shipped)

The simple case compiles correctly. Generated CL for `f` above:

```lisp
(p-sub pl-f (&rest %_args)
  (p-args-body
    (block nil
      (let (($x (make-p-box nil)))
        (p-my-= $x (p-shift @_))
        (tagbody                              ; <-- correctly inserted
          (p-if (p-< $x 0)
            (progn (let ((*wantarray* :void)) (go :FAIL))))
          (let ((*wantarray* :void)) (p-return "ok"))
          :FAIL  ;; pcl-label
)                                             ; tagbody closes here
        (let ((*wantarray* :void)) (p-return "fail")))))) ; post-label outside
```

This handles forward error-gotos, backward retry loops, multiple labels, goto out
of a `while`, and top-level goto-in-`if`. Tests: `Pl/t/goto-label-01.t`.

How it's done (all in `Pl/Parser.pm`): after a block's statements are emitted,
`_wrap_runtime_labels` **scans the generated CL text**, groups it into balanced
forms, finds label sentinels and their reachable gotos, and splices a `(tagbody …)`
around the minimal form-range. `_scan_lisp_lines` does paren/string/comment-aware
depth tracking to know which lines are at "statement-sibling" depth and which are
inside a nested lambda.

### 1.3 What breaks — `do {}` (and `map`/`grep`/`sort`) + a goto

This is the real `Text::Balanced::_match_tagged` shape, minimised:

```perl
sub mt {
    my ($a, $b, $rdel) = @_;
    my $spec;
    if (!defined $rdel) {
        $spec = "x";
    } else {
        $spec = do { my $d = $rdel; "<$d>"; };   # <-- do-block with its own `my`
    }
    while ($b) {
        if ($a) { goto MATCHED; }                # <-- goto out of the while
        $b--;
    }
    return "none";
  MATCHED:
    return "m:$spec";
}
print mt(1,1,"Z"), "\n";   # Perl: "m:<Z>"
```

PCL aborts at load: **`attempt to GO to nonexistent tag: :MATCHED`** — no
`tagbody` is emitted at all (`grep -c tagbody` → 0).

The label and its goto are at compatible depths *in the final text*
(`:MATCHED` at the body level, the `go` deeper inside the `while`), so in
principle it is wrappable. But the wrapper never gets a chance to see it that way.

---

## 2. Root cause — three layers

The wrapper inspects **generated CL text** and counts parens to recover scope.
That only works if the text is (a) paren-balanced at the point of inspection and
(b) the same regardless of how we got there. Neither holds here.

### Layer 1 — declarations have no scope tree; they are an emit-time `let` stream

PCL does not have a data structure that says "this block declares `$x`,`$y` and
contains statements S1…Sn." Instead, as it walks statements it **emits `(let …`
opens inline** and defers the matching closes to block-end via side bookkeeping
(`_pending_let_closes`, `_local_let_depth`). There are even two competing
strategies — a "two-phase" per-statement-let path and a "flat-let" path — chosen
by heuristics (`_with_declarations`, `_emit_scoped_block`, and now
`_block_has_standalone_label`).

Consequence: to insert a `tagbody` at statement-sibling level, the code has to
*reverse-engineer* the nesting by counting parens in text. There is no "ask the
block for its statement list."

### Layer 2 — the text isn't balanced where the wrapper looks (lambda lowering)

A `do { … }` (and `map`/`grep`/`sort {}` and anon `sub {}`) lowers to a CL
**lambda**, and PCL builds that lambda by **swapping the output buffer to a temp
section** (`parse_block_as_function` with `return_lambda`), collecting the body as
a string, and splicing it back inline. The result is that, in the region the
wrapper captures mid-emit, the paren depth does **not** return to the body level
after the `do`-statement. Here is the actual region the wrapper sees for `mt`
(depth `d`, in-lambda flag `il`):

```
 7 d= 0 il=0|  (p-if (p-! (p-defined $rdel))      ; body level, good
16 d= 2 il=0|    (p-my-= $spec (let ... (funcall (lambda ()   ; do-block opens…
22 d= 7 il=1|  (p-while $b                        ; …never came back: depth 7, il=1
42 d= 8 il=1|  :MATCHED  ;; pcl-label             ; label looks 8-deep & "in a lambda"
```

So `:MATCHED` *appears* to be 8 levels deep and inside a lambda, and the `go`
likewise — the wrapper concludes the goto can't reach the label and emits nothing.
The depth is an artifact of the lambda-via-temp-section lowering, not the real
logical scope (in the *final* assembled text the same label sits at the body
level).

A related instance of the same layer: until this session, `my` vars **inside**
`do`/`eval`/`map`/`grep`/`sort` blocks were hoisted out to the enclosing sub's
`let` by `_find_all_declarations` (it recursed through these scope boundaries).
That both double-bound the var and left the hoisted `let` open around the rest of
the body, pushing the labels even deeper. Fixed for `do`/`eval` (commit
`c580dc3`); **still open for `map`/`grep`/`sort`** — e.g. `grep { my $x = $_; … }`
still emits `(let (($x …)))` wrapping the whole sub body.

### Layer 3 — codegen is not deterministic across passes

`pl2cl` runs two passes (a prototype-collecting pre-pass, then the real pass);
`Pl::Parser->parse()` runs one. The declaration-path heuristic depends on info
gathered in the pre-pass, so **the same source produces different CL** under the
two entry points. In session 263 the same sub yielded a 46-line (unwrappable)
region under `parse()` and a 57-line (wrappable) region under `parse_file()`.
Whatever fix we make must be pass-independent, and this nondeterminism is a hazard
well beyond goto.

### Why "just wrap harder" doesn't work

Each layer defeats the text-scanning approach independently: Layer 1 means we must
scan text at all; Layer 2 means the text we scan is mis-shaped; Layer 3 means it's
not even the same text. Patching the scanner can't fix mis-shaped or
non-deterministic input. The fix has to move *up*, toward representing scope and
control flow before emitting text.

---

## 3. The plan

Three tiers. The **release-1 line is drawn after Tier 0.** Tiers 1–2 are the
direction of travel, listed so the Tier-0 work is done in a way that doesn't have
to be thrown away.

### Tier 0 — tactical, release-1 (est. 2–4 days, low risk)

Goal: make intra-sub `goto LABEL` correct for the real shapes (incl. `_match_tagged`)
**without** introducing an IR. Each item is independently testable and reversible.

**0a. Emit the label `tagbody` structurally, not by text post-processing.**
PCL already emits a `tagbody` structurally for loops (`(tagbody :redo … :next)`).
Do the same for a sub body that contains standalone labels: when `_process_block`
sees the block has standalone labels (it already detects this via
`_block_has_standalone_label`), open `(tagbody` right after the declaration
`let`(s) and close it right before them, emitting each `LABEL:` as a bare `:LABEL`
tag. The gotos already emit `(go :LABEL)`. This **deletes the dependency on
`_scan_lisp_lines`/`_wrap_runtime_labels` for sub bodies** and is immune to
Layers 2 and 3 (we never inspect text). Keep the text-wrapper only for the
top-level case (or convert that too).
  - Tradeoff: a whole-body `tagbody` returns `nil`, so a sub that *implicitly*
    returns its last expression *and* uses labels would lose that value. Real
    label-using subs use explicit `return`. Either accept this (document it) or
    capture the tail into a `--ret--` var (one extra `let`); recommend accept +
    document for release 1.
  - Requires flat-let for label-containing bodies (already forced) so the labels
    are direct `tagbody` elements.

**0b. Make the declaration-path choice pass-independent (Layer 3).**
Compute the flat-vs-two-phase decision (and the `_find_all_declarations` /
BlockAnalyzer inputs it depends on) identically in the prototype pre-pass and the
real pass. Add a regression that asserts `parse()` and `parse_file()` produce
byte-identical CL for a label+`do` sub. This is a correctness fix on its own.

**0c. Finish the scope-boundary exclusion (Layer 2, hoisting half).**
Extend the `do`/`eval` hoisting exclusion in `_find_all_declarations` to
`map`/`grep`/`sort` block bodies (same code shape). Add a `my`-in-grep-block test.

**0d. Re-enable real `Text::Balanced` and add a `Pl/t` regression** for
`extract_tagged` once 0a–0c land. If anything is still red, fall back to a
**documented deferral** in `docs/not-supported.md` (framed as "deferred to the
declaration IR," not a permanent gap) — this is the worst-case release-1 exit.

Exit criteria for release 1: `goto LABEL` works for all `Pl/t/goto-label-01.t`
shapes **plus** the `do`-block/`while` shape; `extract_tagged` works or is
explicitly deferred; gate stays green.

### Tier 1 — bounded refactor (est. 1–2 weeks, medium risk) — NOT release 1

Introduce a real **per-block scope record** (mostly the data `BlockAnalyzer`
already computes): for each block, its declared vars (with `my`/`our`/`local`/
`state` kind and rename), its ordered child statements, and its scope-boundary
children (`do`/`eval`/`map`/`grep`/`sort`/anon-sub). Emit the declaration `let`(s)
**and** any label `tagbody` **structurally from this record**, deleting the
deferred-close bookkeeping (`_pending_let_closes`, `_local_let_depth`) for sub
bodies. This is the "scope node" idea, scoped to declarations + labels only — not
the type-flow rewrite. Tier 0a is a down-payment on this (structural tagbody is a
subset).

### Tier 2 — the full IR rewrite (out of scope) — see the existing specs

`docs/codegen-rewrite-spec.md` + `docs/type-flow-and-codegen-plan.md`: declarations
as IR scope nodes, control flow (labels, `goto`, `next`/`last`/`redo`, loops) as
first-class IR nodes lowered structurally, representation (box/unbox, `repr`
lattice) decided per variable. The goto cases here become acceptance tests.

### Recommendation

Do **Tier 0** for release 1 — it's a few days, unblocks real CPAN code
(Text::Balanced and anything else using error-label gotos), and 0a/0b/0c are each
correctness wins regardless of goto. Defer Tiers 1–2. Make the Tier-0 tagbody
work *structural* (0a) rather than more text-scanning, so it composes with Tier 1
instead of being thrown away.

---

## Appendix — files and symbols touched this session

- `Pl/Parser.pm`:
  - `_scan_lisp_lines` (new) — paren/lambda-aware per-line depth scan.
  - `_wrap_runtime_labels` (rewritten) — complete-form-granular tagbody wrap.
  - `_process_block` — captures its statement region and runs the wrapper.
  - `_block_has_standalone_label` (new) — forces flat-let for label bodies.
  - `_find_all_declarations` — now excludes `do`-blocks from hoisting (was only
    `sub`/`eval`); **TODO: add `map`/`grep`/`sort`.**
- `Pl/t/goto-label-01.t` (new) — 7 regression tests (the working shapes).
- Commits: `a0fa56a` (goto wrap), `c580dc3` (do-block scoping fix).
- Still broken: `Text::Balanced::extract_tagged` → `_match_tagged` (the §1.3 shape).
