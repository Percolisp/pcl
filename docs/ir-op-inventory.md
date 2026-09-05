# The PCL IR op inventory

**GENERATED — do not edit.**  Regenerate with `tools/ir-inventory.pl` (writes
this file and the machine form `docs/ir-op-inventory.tsv`).  The gate row
`Pl/t/ir-inventory-01.t` fails when the checked-in files are not what the tool
produces today.

This is the PORT LIST: every name a PCL-emitted file can contain, taken from
the LOADED runtime's `:pcl` external symbols — which is the evaluated
`(:export …)` list, and therefore also sees the ops a definer macro generates
(`p-+`, `p-*`, the numeric and string compares and the compound-assignment
family have no textual `defun` anywhere).  The semantics of each op are its
docstring in `cl/pcl-runtime.lisp`; the family RULES are `docs/ir-spec.md` §10
and are quoted below per family.

* names exported: **684**
* families: **19** with an ir-spec §10 rule, **34** without one
* with a machine-readable `Contract:` tail: **57** of 684
* UNCLASSIFIED (no family rule matches): **0**

The contract columns come from a final `Contract:` paragraph of the op's own
docstring — the runtime is the spec, so the machine-readable form lives where
the prose does.  The grammar (keys, closed value sets, and what each means) is
normative in `docs/ir-spec.md` §10.  `UNCLASSIFIED` in a contract column means
the op has no tail yet, never a default.

**§10 was RECONCILED against this file in s470bm.**  Its rows had named
Perl-facing spellings for four families — `p-&`-style bitwise, `p-x`,
`p-eq`…`p-cmp`, `p-++`-style increment — none of which is a symbol the runtime
exports; each row now names the real ops (`p-bit-and`, `p-str-x`, `p-str-eq`,
`p-pre++`, …), verified EMITTED before the edit.  The section below is the
standing check that they have not drifted apart again.

## Citations in ir-spec §10 that are not IR names

Names the §10 table prints as family members that the runtime does NOT export.  A backend author works from that table, so each one is a name they would look for and not find.

**Exists, but INTERNAL to `:pcl`** — never emitted bare, so it is not part of the IR's vocabulary: either a runtime-only helper, or an op the emitter writes package-QUALIFIED (1):

* `pcl::p-qr`

# Families with an ir-spec §10 rule

## aggregate-builtin (27)

ir-spec §10 row **array/hash builtins** — Perl signatures; `p-sort` default is string order, comparator lambda gets `$a`/`$b`; `p-defined` returns `1`/`""`.  `%p-sort-classic` and `%p-push1` are sugar — rewrite them back and nothing is lost

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `%p-push1` | function | `(arr item)` | — | — | — | — | — | — | — |
| `%p-sort-classic` | function | `(mode &rest items)` | — | — | — | — | — | — | — |
| `p-array-init` | function | `(&rest elements)` | — | — | — | — | — | — | — |
| `p-array-last-index` | function | `(arr)` | — | — | — | — | — | — | — |
| `p-copy-array` | function | `(src)` | — | — | — | — | — | — | — |
| `p-copy-hash` | function | `(h)` | — | — | — | — | — | — | — |
| `p-each` | function | `(collection)` | — | — | — | — | — | — | — |
| `p-flatten` | function | `(arr)` | — | — | — | — | — | — | — |
| `p-flatten-args` | function | `(args)` | — | — | — | — | — | — | — |
| `p-grep` | function | `(fn &rest items)` | — | — | — | — | — | — | — |
| `p-hash` | function | `(&rest pairs)` | — | — | — | — | — | — | — |
| `p-keys` | function | `(collection)` | — | — | — | — | — | — | — |
| `p-list-x` | function | `(list-val count)` | — | — | — | — | — | — | — |
| `p-map` | function | `(fn &rest items)` | — | — | — | — | — | — | — |
| `p-pop` | function | `(arr)` | — | — | — | — | — | — | — |
| `p-push` | macro | `(arr &rest items)` | — | — | — | — | — | — | — |
| `p-scalar` | function | `(&rest args)` | — | — | — | — | — | — | — |
| `p-set-array-length` | function | `(arr new-last-index)` | — | — | — | — | — | — | — |
| `p-shift` | function | `(arr)` | — | — | — | — | — | — | — |
| `p-sort` | function | `(&rest args)` | — | — | — | — | — | — | — |
| `p-sort-get-fn` | function | `(val)` | — | — | — | — | — | — | — |
| `p-sort-named` | function | `(sym fn)` | — | — | — | — | — | — | — |
| `p-splice` | macro | `(arr &rest args)` | — | — | — | — | — | — | — |
| `p-split` | function | `(pattern str &optional limit)` | — | — | — | — | — | — | — |
| `p-unshift` | function | `(arr &rest items)` | — | — | — | — | — | — | — |
| `p-values` | function | `(collection)` | — | — | — | — | — | — | — |
| `p-wantarray` | function | `nil` | — | — | — | — | — | — | — |

## assignment (11)

ir-spec §10 row **assignment** — store per §2.2; a list assignment used as a VALUE is two-faced (task #721): scalar/void yields the number of elements the RHS produced, LIST context yields the LHS *lvalues* after the assignment

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-array-=` | macro | `(place value)` | — | — | — | — | — | — | — |
| `p-array-deref-=` | function | `(array-ref value)` | — | — | — | — | — | — | — |
| `p-array-fill` | function | `(place value)` | — | — | — | — | — | — | — |
| `p-box-init` | function | `(value)` | — | — | — | — | — | — | — |
| `p-hash-=` | macro | `(place value)` | — | — | — | — | — | — | — |
| `p-hash-deref-=` | function | `(hash-ref value)` | — | — | — | — | — | — | — |
| `p-hash-fill` | function | `(place value)` | — | — | — | — | — | — | — |
| `p-list-=` | macro | `(place value)` | — | — | — | — | — | — | — |
| `p-my-=` | macro | `(place value)` | — | — | — | — | — | — | — |
| `p-scalar-=` | macro | `(place value)` | — | — | — | — | — | — | — |
| `p-setf` | macro | `(place value)` | — | — | — | — | — | — | — |

## bitwise (13)

ir-spec §10 row **bitwise (mode-dispatched)** — overload hook first; then ONE mode decision (`%p-bitwise-operand-kind`): the op is NUMERIC iff an operand carries a number, else it STRINGIFIES both operands and operates byte by byte

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-<<` | function | `(a b)` | — | — | — | — | — | — | — |
| `p-<<-int` | function | `(a b)` | — | — | — | — | — | — | — |
| `p->>` | function | `(a b)` | — | — | — | — | — | — | — |
| `p->>-int` | function | `(a b)` | — | — | — | — | — | — | — |
| `p-bit-and` | function | `(a b)` | — | — | — | — | — | — | — |
| `p-bit-not` | function | `(a)` | — | — | — | — | — | — | — |
| `p-bit-or` | function | `(a b)` | — | — | — | — | — | — | — |
| `p-bit-xor` | function | `(a b)` | — | — | — | — | — | — | — |
| `p-str-bit-and` | function | `(a b)` | — | — | — | — | — | — | — |
| `p-str-bit-not` | function | `(a)` | — | — | — | — | — | — | — |
| `p-str-bit-or` | function | `(a b)` | — | — | — | — | — | — | — |
| `p-str-bit-xor` | function | `(a b)` | — | — | — | — | — | — | — |
| `p-to-s64` | function | `(n)` | — | — | — | — | — | — | — |

## command-capture (1)

ir-spec §10 row **command capture** — wantarray-sensitive, exactly like `p-readline`: scalar/void yields the whole captured stdout as one string, LIST context yields it SPLIT INTO `$/` RECORDS

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-backtick` | function | `(cmd)` | — | — | — | — | — | — | — |

## compiled-regex (1)

ir-spec §10 row **compiled regex (qr)** — a Regexp OBJECT, not a string: it carries its own flags and identity, and stringifies as perl's `(?^flags:SOURCE)` wrapper.  A pattern that is exactly ONE interpolated qr *is* that qr (the outer modifiers are ignored); a qr used as PART of a larger pattern embeds its wrapper verbatim

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-regex-from-parts` | function | `(pattern modifiers)` | — | — | — | — | — | — | — |

## compound-assignment (35)

ir-spec §10 row **compound assignment** — read-modify-write; boxed macros store back via box-set/setf per place shape, `-raw` twins are `(setf slot NEW)` with the identical NEW form; `&&=`/`||=`/`//=` short-circuit and store the RHS unchanged

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-%=` | macro | `(place value)` | — | — | — | — | — | — | — |
| `p-%=-raw` | macro | `(var value)` | — | — | — | — | — | — | — |
| `p-**=` | macro | `(place value)` | — | — | — | — | — | — | — |
| `p-**=-raw` | macro | `(var value)` | — | — | — | — | — | — | — |
| `p-*=` | macro | `(place value)` | — | — | — | — | — | — | — |
| `p-*=-raw` | macro | `(var value)` | — | — | — | — | — | — | — |
| `p-.=` | macro | `(place value)` | — | — | — | — | — | — | — |
| `p-.=-raw` | macro | `(var value)` | — | — | — | — | — | — | — |
| `p-//=` | macro | `(place value)` | — | — | — | — | — | — | — |
| `p-/=` | macro | `(place value)` | — | — | — | — | — | — | — |
| `p-/=-raw` | macro | `(var value)` | — | — | — | — | — | — | — |
| `p-<<=` | macro | `(place value)` | — | — | — | — | — | — | — |
| `p-<<=-raw` | macro | `(var value)` | — | — | — | — | — | — | — |
| `p->>=` | macro | `(place value)` | — | — | — | — | — | — | — |
| `p->>=-raw` | macro | `(var value)` | — | — | — | — | — | — | — |
| `p-and-assign` | macro | `(place value)` | — | — | — | — | — | — | — |
| `p-bit-and=` | macro | `(place value)` | — | — | — | — | — | — | — |
| `p-bit-and=-raw` | macro | `(var value)` | — | — | — | — | — | — | — |
| `p-bit-or=` | macro | `(place value)` | — | — | — | — | — | — | — |
| `p-bit-or=-raw` | macro | `(var value)` | — | — | — | — | — | — | — |
| `p-bit-xor=` | macro | `(place value)` | — | — | — | — | — | — | — |
| `p-bit-xor=-raw` | macro | `(var value)` | — | — | — | — | — | — | — |
| `p-decf` | macro | `(place &optional (delta 1))` | — | — | — | — | — | — | — |
| `p-decf-raw` | macro | `(var &optional (delta 1))` | — | — | — | — | — | — | — |
| `p-incf` | macro | `(place &optional (delta 1))` | — | — | — | — | — | — | — |
| `p-incf-raw` | macro | `(var &optional (delta 1))` | — | — | — | — | — | — | — |
| `p-or-assign` | macro | `(place value)` | — | — | — | — | — | — | — |
| `p-str-bit-and=` | macro | `(place value)` | — | — | — | — | — | — | — |
| `p-str-bit-and=-raw` | macro | `(var value)` | — | — | — | — | — | — | — |
| `p-str-bit-or=` | macro | `(place value)` | — | — | — | — | — | — | — |
| `p-str-bit-or=-raw` | macro | `(var value)` | — | — | — | — | — | — | — |
| `p-str-bit-xor=` | macro | `(place value)` | — | — | — | — | — | — | — |
| `p-str-bit-xor=-raw` | macro | `(var value)` | — | — | — | — | — | — | — |
| `p-str-x=` | macro | `(place value)` | — | — | — | — | — | — | — |
| `p-str-x=-raw` | macro | `(var value)` | — | — | — | — | — | — | — |

## context-frame (9)

ir-spec §10 row **context & frames** — names, not operations: each expands to exactly the `let`/`lambda` shape it replaced, so a translator implements the expansion and nothing else

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `*p-in-list-assign-rhs*` | variable | `` | — | — | — | — | — | — | — |
| `*pcl-caller-wantarray*` | variable | `` | — | — | — | — | — | — | — |
| `*pcl-sub-call-depth*` | variable | `` | — | — | — | — | — | — | — |
| `*wantarray*` | variable | `` | — | — | — | — | — | — | — |
| `p-caller-ctx` | macro | `(&body body)` | — | — | — | — | — | — | — |
| `p-list-ctx` | macro | `(&body body)` | — | — | — | — | — | — | — |
| `p-scalar-ctx` | macro | `(&body body)` | — | — | — | — | — | — | — |
| `p-sort-cmp` | macro | `(params &body body)` | — | — | — | — | — | — | — |
| `p-void-ctx` | macro | `(&body body)` | — | — | — | — | — | — | — |

## declaration (3)

ir-spec §10 row **declarations** — names carrying the compiler's own VERDICTS — a binding's class, a parameter's class, a sub's proven facts.  Every one expands to exactly the form it replaced, and every set is CLOSED.  A translator may drop all three vocabularies and still produce a correct program

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-let` | macro | `(bindings &body body)` | — | — | — | — | — | — | — |
| `p-raw-params` | macro | `((&rest params) &body body)` | — | — | — | — | — | — | — |
| `p-sub` | macro | `(name params facts &body body)` | — | — | — | — | — | — | — |

## elements (22)

ir-spec §10 row **elements** — reads unbox scalars, keep reference boxes (§2.3–2.4); writes through `p-setf` autovivify intermediate refs; `p-delete` returns the removed value

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-aref` | function | `(arr idx)` | insensitive | num | none | yes | no | no | none |
| `p-aref-argbox` | function | `(arr idx)` | — | — | — | — | — | — | — |
| `p-aref-box` | function | `(arr idx)` | insensitive | num | none | yes | no | no | none |
| `p-aref-deref` | function | `(ref idx)` | — | — | — | — | — | — | — |
| `p-aref-deref-box` | function | `(ref idx)` | — | — | — | — | — | — | — |
| `p-aslice` | function | `(arr &rest indices)` | insensitive | num | none | yes | no | no | none |
| `p-delete` | function | `(hash key)` | insensitive | str | none | yes | no | no | none |
| `p-delete-array` | function | `(arr idx)` | insensitive | num | none | yes | no | no | none |
| `p-ensure-arrayref` | function | `(ref)` | — | — | — | — | — | — | — |
| `p-ensure-hashref` | function | `(ref)` | — | — | — | — | — | — | — |
| `p-exists` | function | `(hash key)` | insensitive | str | none | yes | no | no | none |
| `p-exists-array` | function | `(arr idx)` | insensitive | num | none | yes | no | no | none |
| `p-gethash` | function | `(hash key)` | insensitive | str | none | yes | no | no | none |
| `p-gethash-argbox` | function | `(hash key)` | — | — | — | — | — | — | — |
| `p-gethash-box` | function | `(hash key)` | insensitive | str | none | yes | no | no | none |
| `p-gethash-deref` | function | `(ref key)` | — | — | — | — | — | — | — |
| `p-gethash-deref-box` | function | `(ref key)` | — | — | — | — | — | — | — |
| `p-hslice` | function | `(hash &rest keys)` | insensitive | str | none | yes | no | no | none |
| `p-kv-aslice` | function | `(arr &rest indices)` | insensitive | num | none | yes | no | no | none |
| `p-kv-hslice` | function | `(hash &rest keys)` | insensitive | str | none | yes | no | no | none |
| `p-list-scalar` | function | `(val)` | — | — | — | — | — | — | — |
| `p-slice-result` | function | `(val)` | — | — | — | — | — | — | — |

## increment (4)

ir-spec §10 row **increment** — numeric ±1 on the box/slot; `p-++` on a pure-alpha string does Perl string increment (`"az"→"ba"`)

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-post++` | macro | `(place)` | — | — | — | — | — | — | — |
| `p-post--` | macro | `(place)` | — | — | — | — | — | — | — |
| `p-pre++` | macro | `(place)` | — | — | — | — | — | — | — |
| `p-pre--` | macro | `(place)` | — | — | — | — | — | — | — |

## introspection (15)

ir-spec §10 row **introspection** — §7; `p-caller` returns package but file/line are stubs (divergence)

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-__pcl_set_prototype` | function | `(code proto)` | — | — | — | — | — | — | — |
| `p-bless` | function | `(ref class)` | — | — | — | — | — | — | — |
| `p-caller` | function | `(&optional (level 0))` | — | — | — | — | — | — | — |
| `p-can` | function | `(invocant method-name)` | — | — | — | — | — | — | — |
| `p-coderef-defined-p` | function | `(coderef)` | — | — | — | — | — | — | — |
| `p-coderef-exists-p` | function | `(coderef)` | — | — | — | — | — | — | — |
| `p-isa` | function | `(invocant class-name)` | — | — | — | — | — | — | — |
| `p-prototype` | function | `(&optional ref)` | — | — | — | — | — | — | — |
| `p-ref` | function | `(val)` | — | — | — | — | — | — | — |
| `p-reftype` | function | `(val)` | — | — | — | — | — | — | — |
| `p-stash` | function | `(pkg-name)` | — | — | — | — | — | — | — |
| `p-sub-defined` | function | `(pkg-str name-str)` | — | — | — | — | — | — | — |
| `p-sub-exists` | function | `(pkg-str name-str)` | — | — | — | — | — | — | — |
| `p-undef-sub` | function | `(pkg-str name-str)` | — | — | — | — | — | — | — |
| `pl-__SUB__` | function | `nil` | — | — | — | — | — | — | — |

## io (23)

ir-spec §10 row **I/O** — Perl builtins; bareword handles are symbols; `p-open` boxes its handle argument.  2-arg `p-open` parses pipe/dup modes; `p-close` on a pipe handle reaps the child and sets `$?`

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `*p-filehandles*` | variable | `` | — | — | — | — | — | — | — |
| `p-binmode` | macro | `(fh &rest args)` | — | — | — | — | — | — | — |
| `p-close` | macro | `(&optional fh)` | — | — | — | — | — | — | — |
| `p-eof` | macro | `(&rest args)` | — | — | — | — | — | — | — |
| `p-fcntl` | macro | `(fh func arg)` | — | — | — | — | — | — | — |
| `p-fileno` | macro | `(fh)` | — | — | — | — | — | — | — |
| `p-getc` | macro | `(&rest args)` | — | — | — | — | — | — | — |
| `p-lock` | function | `(x)` | — | — | — | — | — | — | — |
| `p-open` | macro | `(fh mode &optional filename)` | — | — | — | — | — | — | — |
| `p-pipe` | macro | `(read-fh write-fh)` | — | — | — | — | — | — | — |
| `p-print` | function | `(&rest args)` | — | — | — | — | — | — | — |
| `p-printf` | function | `(&rest args)` | — | — | — | — | — | — | — |
| `p-read` | macro | `(fh &rest args)` | — | — | — | — | — | — | — |
| `p-readline` | macro | `(&rest args)` | — | — | — | — | — | — | — |
| `p-say` | function | `(&rest args)` | — | — | — | — | — | — | — |
| `p-seek` | macro | `(fh &rest args)` | — | — | — | — | — | — | — |
| `p-select` | function | `(&optional (fh nil) (wbits nil) (ebits nil) (timeout nil timeout-p))` | — | — | — | — | — | — | — |
| `p-sysopen` | macro | `(fh path flags &optional perms)` | — | — | — | — | — | — | — |
| `p-sysread` | macro | `(fh &rest args)` | — | — | — | — | — | — | — |
| `p-sysseek` | macro | `(fh &rest args)` | — | — | — | — | — | — | — |
| `p-syswrite` | macro | `(fh &rest args)` | — | — | — | — | — | — | — |
| `p-tell` | macro | `(&rest args)` | — | — | — | — | — | — | — |
| `p-write` | macro | `(&optional fh)` | — | — | — | — | — | — | — |

## logical (8)

ir-spec §10 row **logical** — short-circuit macros returning operand values (§3.4)

The row named `p-` IS perl's `||`: the runtime writes it `p-||`, and the CL reader takes the `||` as an EMPTY multiple-escape section, so the symbol's name is `P-` (probed s470bm).  A text-parsing backend must fold `p-||` and `p-` — ir-spec §11b.

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-` | macro | `(a b)` | — | — | — | — | — | — | — |
| `p-!` | function | `(a)` | — | — | — | — | — | — | — |
| `p-&&` | macro | `(a b)` | — | — | — | — | — | — | — |
| `p-//` | macro | `(a b)` | — | — | — | — | — | — | — |
| `p-and` | macro | `(a b)` | — | — | — | — | — | — | — |
| `p-not` | function | `(a)` | — | — | — | — | — | — | — |
| `p-or` | macro | `(a b)` | — | — | — | — | — | — | — |
| `p-xor` | function | `(a b)` | — | — | — | — | — | — | — |

## numeric (9)

ir-spec §10 row **numeric ops** — numify operands (§3.1), return raw number; overload hook first; `/` yields a double when inexact; `%` follows Perl sign rules; the shifts truncate to integer (Inf→0) and clamp a shift count ≥ the word size to 0

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-%` | function | `(a b)` | insensitive | num | none | yes | no | no | none |
| `p-*` | function | `(a &optional (b nil b-supplied-p))` | insensitive | num | none | no | no | no | none |
| `p-**` | function | `(a b)` | insensitive | num | none | no | no | no | none |
| `p-+` | function | `(a &optional (b nil b-supplied-p))` | insensitive | num | none | no | no | no | none |
| `p--` | function | `(a &optional b)` | insensitive | num,str | none | no | no | no | none |
| `p-/` | function | `(a b)` | insensitive | num | none | yes | no | no | none |
| `p-abs` | function | `(val)` | insensitive | num | none | no | no | no | none |
| `p-double-inf` | macro | `(&optional negative)` | insensitive | none | none | no | no | no | sbcl |
| `p-int` | function | `(val)` | insensitive | num | none | no | no | no | none |

## numeric-compare (8)

ir-spec §10 row **numeric compare** — numify; return `1`/`""` (`<=>` −1/0/1; NaN comparisons → `""`/undef)

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-!=` | function | `(a b)` | insensitive | num | none | no | no | no | none |
| `p-<` | function | `(a b)` | insensitive | num | none | no | no | no | none |
| `p-<=` | function | `(a b)` | insensitive | num | none | no | no | no | none |
| `p-<=>` | function | `(a b)` | insensitive | num | none | no | no | no | none |
| `p-==` | function | `(a b)` | insensitive | num | none | no | no | no | none |
| `p->` | function | `(a b)` | insensitive | num | none | no | no | no | none |
| `p->=` | function | `(a b)` | insensitive | num | none | no | no | no | none |
| `p-chain-cmp` | macro | `(first-term &rest ops-and-terms)` | insensitive | none | none | no | no | no | none |

## regex (5)

ir-spec §10 row **regex** — match/substitute/transliterate against a box (writes back for s///, tr///); sets §8 match state; list context returns captures.  A FAILED `m//` answers by context and by NOTHING else: scalar/void gives `""`, LIST context gives the EMPTY LIST, spelled `(%p-empty-list)`

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-!~` | function | `(string operation)` | — | — | — | — | — | — | — |
| `p-=~` | function | `(string operation)` | — | — | — | — | — | — | — |
| `p-regex` | function | `(pattern-string)` | — | — | — | — | — | — | — |
| `p-subst` | function | `(pattern replacement &rest modifiers)` | — | — | — | — | — | — | — |
| `p-tr` | function | `(from to &rest modifiers)` | — | — | — | — | — | — | — |

## slice-delete (4)

ir-spec §10 row **slice delete** — every one flattens its key/index arguments alike (`%p-flatten-slice-args`), and every one answers nil for an EMPTY slice — undef in scalar context, the empty list in list context, per [perl #29127]

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-delete-array-slice` | function | `(arr &rest indices)` | — | — | — | — | — | — | — |
| `p-delete-hash-slice` | function | `(hash &rest keys)` | — | — | — | — | — | — | — |
| `p-delete-kv-array-slice` | function | `(arr &rest indices)` | — | — | — | — | — | — | — |
| `p-delete-kv-hash-slice` | function | `(hash &rest keys)` | — | — | — | — | — | — | — |

## string (26)

ir-spec §10 row **string ops** — stringify operands (§3.2), return raw string; Perl's `$_`-default forms arrive with `$_` already explicit in the tree (§8)

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-.` | function | `(a b)` | insensitive | str | none | no | no | no | none |
| `p-chomp` | function | `(&rest vars)` | — | — | — | — | — | — | — |
| `p-chop` | function | `(&rest vars)` | — | — | — | — | — | — | — |
| `p-chr` | function | `(n)` | insensitive | num | none | no | no | no | none |
| `p-crypt` | function | `(plaintext salt)` | insensitive | str | none | yes | no | no | posix |
| `p-fc` | function | `(str)` | insensitive | str | none | no | no | no | none |
| `p-hex` | function | `(str)` | — | — | — | — | — | — | — |
| `p-index` | function | `(str substr &optional start)` | insensitive | str,num | none | no | no | no | none |
| `p-join` | function | `(sep &rest items)` | insensitive | str | none | no | no | no | none |
| `p-lc` | function | `(str)` | insensitive | str | none | no | no | no | none |
| `p-lcfirst` | function | `(str)` | insensitive | str | none | no | no | no | none |
| `p-length` | function | `(val)` | insensitive | str | none | no | no | no | none |
| `p-oct` | function | `(str)` | — | — | — | — | — | — | — |
| `p-ord` | function | `(str)` | insensitive | str | none | no | no | no | none |
| `p-quotemeta` | function | `(str)` | insensitive | str | none | no | no | no | none |
| `p-reverse` | function | `(&rest items)` | sensitive | str | none | no | no | no | none |
| `p-rindex` | function | `(str substr &optional start)` | insensitive | str,num | none | no | no | no | none |
| `p-sprintf` | function | `(fmt &rest args)` | insensitive | str,num | none | no | no | no | none |
| `p-str-x` | function | `(str count)` | insensitive | str,num | none | no | no | no | none |
| `p-string-concat` | function | `(&rest args)` | insensitive | str | none | no | no | no | none |
| `p-substr` | function | `(str start &optional len replacement)` | insensitive | str,num | none | yes | no | no | none |
| `p-uc` | function | `(str)` | insensitive | str | none | no | no | no | none |
| `p-ucfirst` | function | `(str)` | insensitive | str | none | no | no | no | none |
| `p-unparsable-quote` | function | `(text)` | — | — | — | — | — | — | — |
| `p-unrepresentable-char` | function | `(code)` | — | — | — | — | — | — | — |
| `p-version-string` | function | `(&rest code-points)` | — | — | — | — | — | — | — |

## string-compare (7)

ir-spec §10 row **string compare** — stringify; return `1`/`""`

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-str-cmp` | function | `(a b)` | insensitive | str | none | no | no | no | none |
| `p-str-eq` | function | `(a b)` | insensitive | str | none | no | no | no | none |
| `p-str-ge` | function | `(a b)` | insensitive | str | none | no | no | no | none |
| `p-str-gt` | function | `(a b)` | insensitive | str | none | no | no | no | none |
| `p-str-le` | function | `(a b)` | insensitive | str | none | no | no | no | none |
| `p-str-lt` | function | `(a b)` | insensitive | str | none | no | no | no | none |
| `p-str-ne` | function | `(a b)` | insensitive | str | none | no | no | no | none |

# Families ir-spec §10 has no row for

## bit-string (2)

*No ir-spec §10 row.*  vec() — a string used as a bit vector

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-vec` | function | `(str offset bits)` | — | — | — | — | — | — | — |
| `p-vec-set` | function | `(str-box offset bits value)` | — | — | — | — | — | — | — |

## box (24)

*No ir-spec §10 row.*  the box/undef/coercion primitives — ir-spec §2.1, §2.2, §3

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `%pcl-definedp` | function | `(val)` | — | — | — | — | — | — | — |
| `%pcl-dualvar-p` | function | `(v)` | — | — | — | — | — | — | — |
| `%pcl-str-append` | function | `(buf v)` | — | — | — | — | — | — | — |
| `%pcl-str-buffer` | function | `(v)` | — | — | — | — | — | — | — |
| `%pcl-to-number-strict` | function | `(v name)` | — | — | — | — | — | — | — |
| `%pcl-to-string-strict` | function | `(v name)` | — | — | — | — | — | — | — |
| `box-nv` | function | `(box)` | — | — | — | — | — | — | — |
| `box-set` | function | `(box value)` | — | — | — | — | — | — | — |
| `box-sv` | function | `(box)` | — | — | — | — | — | — | — |
| `ensure-boxed` | function | `(val)` | — | — | — | — | — | — | — |
| `make-p-box` | function | `(value &optional class)` | — | — | — | — | — | — | — |
| `p-$` | function | `(box)` | — | — | — | — | — | — | — |
| `p-box` | class | `` | — | — | — | — | — | — | — |
| `p-box-p` | function | `(sb-kernel::object)` | — | — | — | — | — | — | — |
| `p-box-value` | function | `(sb-kernel:instance)` | — | — | — | — | — | — | — |
| `p-copy-scalar-arg` | function | `(val)` | — | — | — | — | — | — | — |
| `p-defined` | function | `(val)` | — | — | — | — | — | — | — |
| `p-defined-fh` | function | `(fh-sym)` | — | — | — | — | — | — | — |
| `p-dualvar` | function | `(num str)` | — | — | — | — | — | — | — |
| `p-true-p` | function | `(val)` | — | — | — | — | — | — | — |
| `p-undef` | function | `(&optional val)` | — | — | — | — | — | — | — |
| `to-number` | function | `(val)` | — | — | — | — | — | — | — |
| `to-string` | function | `(val)` | — | — | — | — | — | — | — |
| `unbox` | function | `(val)` | — | — | — | — | — | — | — |

## call (1)

*No ir-spec §10 row.*  the code-ref call form

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-funcall-ref` | function | `(ref &rest args)` | — | — | — | — | — | — | — |

## capture-io (3)

*No ir-spec §10 row.*  the harness output-capture hooks

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-capture-fh` | function | `(value)` | — | — | — | — | — | — | — |
| `p-capture-write` | function | `(value)` | — | — | — | — | — | — | — |
| `p-high-capture` | function | `(n)` | — | — | — | — | — | — | — |

## control-flow (25)

*No ir-spec §10 row.*  conditionals, loops, loop control, sub return — ir-spec §6

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `%p-dyn-loop-exit` | function | `(kind)` | insensitive | none | none | yes | yes | no | none |
| `p-break` | function | `nil` | — | — | — | — | — | — | — |
| `p-continue` | function | `nil` | — | — | — | — | — | — | — |
| `p-do` | function | `(filename-val)` | — | — | — | — | — | — | — |
| `p-do-until` | macro | `(condition &body body)` | — | — | — | — | — | — | — |
| `p-do-while` | macro | `(condition &body body)` | — | — | — | — | — | — | — |
| `p-dyn-once` | macro | `(form)` | insensitive | none | none | no | yes | no | none |
| `p-for` | macro | `((&optional init) (test) (&optional step) &rest body-and-keys)` | — | — | — | — | — | — | — |
| `p-foreach` | macro | `((var list) &rest body-and-keys)` | — | — | — | — | — | — | — |
| `p-foreach-range` | macro | `((var from to) &rest body-and-keys)` | — | — | — | — | — | — | — |
| `p-foreach-range-raw` | macro | `((var from to) &rest body-and-keys)` | — | — | — | — | — | — | — |
| `p-foreach-raw` | macro | `((var list) &rest body-and-keys)` | — | — | — | — | — | — | — |
| `p-goto-computed` | macro | `(expr)` | — | — | — | — | — | — | — |
| `p-goto-sub` | macro | `(fn)` | — | — | — | — | — | — | — |
| `p-if` | macro | `(condition then-form &optional else-form)` | — | — | — | — | — | — | — |
| `p-last` | macro | `(&optional label)` | — | — | — | — | — | — | — |
| `p-last-dynamic` | function | `(label-name)` | — | — | — | — | — | — | — |
| `p-next` | macro | `(&optional label)` | — | — | — | — | — | — | — |
| `p-redo` | macro | `(&optional label)` | — | — | — | — | — | — | — |
| `p-return` | macro | `(&rest values)` | — | — | — | — | — | — | — |
| `p-return-empty` | macro | `(&optional (ctx (quote *wantarray*)))` | — | — | — | — | — | — | — |
| `p-tail-value` | macro | `(form)` | — | — | — | — | — | — | — |
| `p-unless` | macro | `(condition then-form &optional else-form)` | — | — | — | — | — | — | — |
| `p-until` | macro | `(condition &body body)` | — | — | — | — | — | — | — |
| `p-while` | macro | `(condition &rest body-and-keys)` | — | — | — | — | — | — | — |

## directory-io (4)

*No ir-spec §10 row.*  opendir/readdir/closedir/rewinddir

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-closedir` | macro | `(dh)` | — | — | — | — | — | — | — |
| `p-opendir` | macro | `(dh &rest args)` | — | — | — | — | — | — | — |
| `p-readdir` | macro | `(dh)` | — | — | — | — | — | — | — |
| `p-rewinddir` | macro | `(dh)` | — | — | — | — | — | — | — |

## dynamic-scope (17)

*No ir-spec §10 row.*  `local` in each of its place shapes — ir-spec §7.2

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-defcell` | macro | `(sym init &rest facts)` | — | — | — | — | — | — | — |
| `p-local-array-elem` | macro | `(arr-var idx-form &body body)` | — | — | — | — | — | — | — |
| `p-local-array-elem-init` | macro | `(arr-var idx-form init-form &body body)` | — | — | — | — | — | — | — |
| `p-local-array-slice` | macro | `(arr-var idx-form &body body)` | — | — | — | — | — | — | — |
| `p-local-cell` | macro | `(sym init &body body)` | — | — | — | — | — | — | — |
| `p-local-cell-if` | macro | `(cond-form sym init &body body)` | — | — | — | — | — | — | — |
| `p-local-deref-array` | macro | `(ref-form &body body)` | — | — | — | — | — | — | — |
| `p-local-deref-hash` | macro | `(ref-form &body body)` | — | — | — | — | — | — | — |
| `p-local-deref-scalar` | macro | `(ref-form &body body)` | — | — | — | — | — | — | — |
| `p-local-dot` | macro | `(&body body)` | — | — | — | — | — | — | — |
| `p-local-glob` | macro | `(pkg-str name-str &body body)` | — | — | — | — | — | — | — |
| `p-local-glob-dynamic` | macro | `(name-form cond-form rhs-form &body body)` | — | — | — | — | — | — | — |
| `p-local-glob-if` | macro | `(cond-form pkg-str name-str rhs-form &body body)` | — | — | — | — | — | — | — |
| `p-local-hash-elem` | macro | `(hash-var key-form &body body)` | — | — | — | — | — | — | — |
| `p-local-hash-elem-init` | macro | `(hash-var key-form init-form &body body)` | — | — | — | — | — | — | — |
| `p-local-maybe` | macro | `(cond-form localizer &body body)` | — | — | — | — | — | — | — |
| `p-local-pipe` | macro | `(&body body)` | — | — | — | — | — | — | — |

## env (3)

*No ir-spec §10 row.*  %ENV and its accessors

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `%ENV` | variable | `` | — | — | — | — | — | — | — |
| `p-env-get` | function | `(key)` | — | — | — | — | — | — | — |
| `p-env-set` | function | `(key value)` | — | — | — | — | — | — | — |

## exception (12)

*No ir-spec §10 row.*  die/warn, eval BLOCK, string eval, try — ir-spec §6.3, §9.1

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `*p-eval-lex-alist*` | variable | `` | — | — | — | — | — | — | — |
| `p-alias-eval-cell` | function | `(sym cell)` | — | — | — | — | — | — | — |
| `p-die` | function | `(&rest raw-args)` | — | — | — | — | — | — | — |
| `p-eval` | function | `(string &optional lex-alist features)` | — | — | — | — | — | — | — |
| `p-eval-block` | macro | `(&body body)` | — | — | — | — | — | — | — |
| `p-eval-lex-lookup` | function | `(name)` | — | — | — | — | — | — | — |
| `p-eval-thunk` | function | `(free-names fn &optional region-pkg)` | — | — | — | — | — | — | — |
| `p-evalbytes` | function | `(s)` | — | — | — | — | — | — | — |
| `p-exception` | class | `` | — | — | — | — | — | — | — |
| `p-exception-object` | generic-function | `(condition)` | — | — | — | — | — | — | — |
| `p-try` | macro | `(try-form catch-clause &optional finally-form)` | — | — | — | — | — | — | — |
| `p-warn` | function | `(&rest raw-args)` | — | — | — | — | — | — | — |

## extension (1)

*No ir-spec §10 row.*  the lazy-extension loader — docs/extensions.md

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-load-extension` | function | `(name)` | — | — | — | — | — | — | — |

## file-ops (19)

*No ir-spec §10 row.*  filesystem builtins that are not filehandle operations

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-chdir` | function | `(&optional dir)` | — | — | — | — | — | — | — |
| `p-chmod` | function | `(mode &rest files)` | — | — | — | — | — | — | — |
| `p-chown` | function | `(&optional (uid nil uid-p) (gid nil gid-p) &rest files)` | — | — | — | — | — | — | — |
| `p-cwd` | function | `nil` | — | — | — | — | — | — | — |
| `p-getcwd` | function | `nil` | — | — | — | — | — | — | — |
| `p-glob` | function | `(&optional pattern)` | — | — | — | — | — | — | — |
| `p-link` | function | `(old new)` | — | — | — | — | — | — | — |
| `p-lstat` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p-mkdir` | function | `(dir &optional mode)` | — | — | — | — | — | — | — |
| `p-readlink` | function | `(path)` | — | — | — | — | — | — | — |
| `p-rename` | function | `(old new)` | — | — | — | — | — | — | — |
| `p-rmdir` | function | `(dir)` | — | — | — | — | — | — | — |
| `p-set_up_inc` | function | `(&rest dirs)` | — | — | — | — | — | — | — |
| `p-stat` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p-symlink` | function | `(old new)` | — | — | — | — | — | — | — |
| `p-truncate` | macro | `(fh-or-file size)` | — | — | — | — | — | — | — |
| `p-umask` | function | `(&optional mode)` | — | — | — | — | — | — | — |
| `p-unlink` | function | `(&rest files)` | — | — | — | — | — | — | — |
| `p-utime` | function | `(&optional atime mtime &rest files)` | — | — | — | — | — | — | — |

## filetest (29)

*No ir-spec §10 row.*  the -X operators and the `_` stat cache — ir-spec §10 I/O adjacent

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `*pcl-stat-cache-path*` | variable | `` | — | — | — | — | — | — | — |
| `_` | variable | `` | — | — | — | — | — | — | — |
| `p--A` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--B` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--C` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--M` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--O` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--R` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--S` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--T` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--W` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--X` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--b` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--c` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--d` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--e` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--f` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--g` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--k` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--l` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--o` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--p` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--r` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--s` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--t` | macro | `(&optional (fh (quote (quote STDIN))))` | — | — | — | — | — | — | — |
| `p--u` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--w` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--x` | macro | `(arg)` | — | — | — | — | — | — | — |
| `p--z` | macro | `(arg)` | — | — | — | — | — | — | — |

## magic-global (136)

*No ir-spec §10 row.*  the magic globals themselves — ir-spec §8

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `$!` | variable | `` | — | — | — | — | — | — | — |
| `$$` | variable | `` | — | — | — | — | — | — | — |
| `$%` | variable | `` | — | — | — | — | — | — | — |
| `$&` | symbol-macro | `` | — | — | — | — | — | — | — |
| `$+` | variable | `` | — | — | — | — | — | — | — |
| `$-` | variable | `` | — | — | — | — | — | — | — |
| `$.` | variable | `` | — | — | — | — | — | — | — |
| `$/` | variable | `` | — | — | — | — | — | — | — |
| `$0` | variable | `` | — | — | — | — | — | — | — |
| `$1` | variable | `` | — | — | — | — | — | — | — |
| `$10` | variable | `` | — | — | — | — | — | — | — |
| `$11` | variable | `` | — | — | — | — | — | — | — |
| `$12` | variable | `` | — | — | — | — | — | — | — |
| `$13` | variable | `` | — | — | — | — | — | — | — |
| `$14` | variable | `` | — | — | — | — | — | — | — |
| `$15` | variable | `` | — | — | — | — | — | — | — |
| `$16` | variable | `` | — | — | — | — | — | — | — |
| `$17` | variable | `` | — | — | — | — | — | — | — |
| `$18` | variable | `` | — | — | — | — | — | — | — |
| `$19` | variable | `` | — | — | — | — | — | — | — |
| `$2` | variable | `` | — | — | — | — | — | — | — |
| `$20` | variable | `` | — | — | — | — | — | — | — |
| `$3` | variable | `` | — | — | — | — | — | — | — |
| `$4` | variable | `` | — | — | — | — | — | — | — |
| `$5` | variable | `` | — | — | — | — | — | — | — |
| `$6` | variable | `` | — | — | — | — | — | — | — |
| `$7` | variable | `` | — | — | — | — | — | — | — |
| `$8` | variable | `` | — | — | — | — | — | — | — |
| `$9` | variable | `` | — | — | — | — | — | — | — |
| `$<` | variable | `` | — | — | — | — | — | — | — |
| `$=` | variable | `` | — | — | — | — | — | — | — |
| `$>` | variable | `` | — | — | — | — | — | — | — |
| `$?` | variable | `` | — | — | — | — | — | — | — |
| `$@` | variable | `` | — | — | — | — | — | — | — |
| `$ARGV` | variable | `` | — | — | — | — | — | — | — |
| `$[` | variable | `` | — | — | — | — | — | — | — |
| `$]` | variable | `` | — | — | — | — | — | — | — |
| `$^` | variable | `` | — | — | — | — | — | — | — |
| `$^a` | variable | `` | — | — | — | — | — | — | — |
| `$^c` | variable | `` | — | — | — | — | — | — | — |
| `$^d` | variable | `` | — | — | — | — | — | — | — |
| `$^e` | variable | `` | — | — | — | — | — | — | — |
| `$^f` | variable | `` | — | — | — | — | — | — | — |
| `$^h` | variable | `` | — | — | — | — | — | — | — |
| `$^i` | variable | `` | — | — | — | — | — | — | — |
| `$^l` | variable | `` | — | — | — | — | — | — | — |
| `$^m` | variable | `` | — | — | — | — | — | — | — |
| `$^n` | variable | `` | — | — | — | — | — | — | — |
| `$^o` | variable | `` | — | — | — | — | — | — | — |
| `$^p` | variable | `` | — | — | — | — | — | — | — |
| `$^r` | variable | `` | — | — | — | — | — | — | — |
| `$^s` | variable | `` | — | — | — | — | — | — | — |
| `$^t` | variable | `` | — | — | — | — | — | — | — |
| `$^v` | variable | `` | — | — | — | — | — | — | — |
| `$^w` | variable | `` | — | — | — | — | — | — | — |
| `$^x` | variable | `` | — | — | — | — | — | — | — |
| `$_` | variable | `` | — | — | — | — | — | — | — |
| `${^taint}` | variable | `` | — | — | — | — | — | — | — |
| `$~` | variable | `` | — | — | — | — | — | — | — |
| `%!` | variable | `` | — | — | — | — | — | — | — |
| `%$` | variable | `` | — | — | — | — | — | — | — |
| `%%` | variable | `` | — | — | — | — | — | — | — |
| `%&` | variable | `` | — | — | — | — | — | — | — |
| `%*` | variable | `` | — | — | — | — | — | — | — |
| `%+` | variable | `` | — | — | — | — | — | — | — |
| `%-` | variable | `` | — | — | — | — | — | — | — |
| `%.` | variable | `` | — | — | — | — | — | — | — |
| `%/` | variable | `` | — | — | — | — | — | — | — |
| `%<` | variable | `` | — | — | — | — | — | — | — |
| `%=` | variable | `` | — | — | — | — | — | — | — |
| `%>` | variable | `` | — | — | — | — | — | — | — |
| `%?` | variable | `` | — | — | — | — | — | — | — |
| `%@` | variable | `` | — | — | — | — | — | — | — |
| `%INC` | variable | `` | — | — | — | — | — | — | — |
| `%SIG` | variable | `` | — | — | — | — | — | — | — |
| `%[` | variable | `` | — | — | — | — | — | — | — |
| `%]` | variable | `` | — | — | — | — | — | — | — |
| `%^` | variable | `` | — | — | — | — | — | — | — |
| `%^h` | variable | `` | — | — | — | — | — | — | — |
| `%_args` | name-only | `` | — | — | — | — | — | — | — |
| `%~` | variable | `` | — | — | — | — | — | — | — |
| `*p-errno-table*` | variable | `` | — | — | — | — | — | — | — |
| `@!` | variable | `` | — | — | — | — | — | — | — |
| `@$` | variable | `` | — | — | — | — | — | — | — |
| `@%` | variable | `` | — | — | — | — | — | — | — |
| `@&` | variable | `` | — | — | — | — | — | — | — |
| `@*` | variable | `` | — | — | — | — | — | — | — |
| `@+` | variable | `` | — | — | — | — | — | — | — |
| `@-` | variable | `` | — | — | — | — | — | — | — |
| `@.` | variable | `` | — | — | — | — | — | — | — |
| `@/` | variable | `` | — | — | — | — | — | — | — |
| `@<` | variable | `` | — | — | — | — | — | — | — |
| `@=` | variable | `` | — | — | — | — | — | — | — |
| `@>` | variable | `` | — | — | — | — | — | — | — |
| `@?` | variable | `` | — | — | — | — | — | — | — |
| `@@` | variable | `` | — | — | — | — | — | — | — |
| `@ARGV` | variable | `` | — | — | — | — | — | — | — |
| `@INC` | variable | `` | — | — | — | — | — | — | — |
| `@[` | variable | `` | — | — | — | — | — | — | — |
| `@]` | variable | `` | — | — | — | — | — | — | — |
| `@^` | variable | `` | — | — | — | — | — | — | — |
| `@_` | variable | `` | — | — | — | — | — | — | — |
| `@{^capture}` | variable | `` | — | — | — | — | — | — | — |
| `@~` | variable | `` | — | — | — | — | — | — | — |
| `p-errno-string` | function | `nil` | — | — | — | — | — | — | — |
| `|$"|` | variable | `` | — | — | — | — | — | — | — |
| `|$'|` | symbol-macro | `` | — | — | — | — | — | — | — |
| `|$(|` | variable | `` | — | — | — | — | — | — | — |
| `|$)|` | variable | `` | — | — | — | — | — | — | — |
| `|$,|` | variable | `` | — | — | — | — | — | — | — |
| `|$:|` | variable | `` | — | — | — | — | — | — | — |
| `|$;|` | variable | `` | — | — | — | — | — | — | — |
| `|$\\|` | variable | `` | — | — | — | — | — | — | — |
| `|$\||` | variable | `` | — | — | — | — | — | — | — |
| `|$`|` | symbol-macro | `` | — | — | — | — | — | — | — |
| `|%"|` | variable | `` | — | — | — | — | — | — | — |
| `|%'|` | variable | `` | — | — | — | — | — | — | — |
| `|%(|` | variable | `` | — | — | — | — | — | — | — |
| `|%)|` | variable | `` | — | — | — | — | — | — | — |
| `|%,|` | variable | `` | — | — | — | — | — | — | — |
| `|%:|` | variable | `` | — | — | — | — | — | — | — |
| `|%;|` | variable | `` | — | — | — | — | — | — | — |
| `|%\\|` | variable | `` | — | — | — | — | — | — | — |
| `|%\||` | variable | `` | — | — | — | — | — | — | — |
| `|%`|` | variable | `` | — | — | — | — | — | — | — |
| `|@"|` | variable | `` | — | — | — | — | — | — | — |
| `|@#|` | variable | `` | — | — | — | — | — | — | — |
| `|@'|` | variable | `` | — | — | — | — | — | — | — |
| `|@(|` | variable | `` | — | — | — | — | — | — | — |
| `|@)|` | variable | `` | — | — | — | — | — | — | — |
| `|@,|` | variable | `` | — | — | — | — | — | — | — |
| `|@:|` | variable | `` | — | — | — | — | — | — | — |
| `|@;|` | variable | `` | — | — | — | — | — | — | — |
| `|@\\|` | variable | `` | — | — | — | — | — | — | — |
| `|@\||` | variable | `` | — | — | — | — | — | — | — |
| `|@`|` | variable | `` | — | — | — | — | — | — | — |

## math (8)

*No ir-spec §10 row.*  the transcendental builtins — no §10 row; each dispatches `use overload` since #1005

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-atan2` | function | `(y x)` | — | — | — | — | — | — | — |
| `p-cos` | function | `(val)` | — | — | — | — | — | — | — |
| `p-exp` | function | `(val)` | — | — | — | — | — | — | — |
| `p-log` | function | `(val)` | — | — | — | — | — | — | — |
| `p-rand` | function | `(&optional max)` | — | — | — | — | — | — | — |
| `p-sin` | function | `(val)` | — | — | — | — | — | — | — |
| `p-sqrt` | function | `(val)` | — | — | — | — | — | — | — |
| `p-srand` | function | `(&optional seed)` | — | — | — | — | — | — | — |

## misc-builtin (3)

*No ir-spec §10 row.*  builtins with no family of their own

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-pos` | function | `(var &optional new-pos)` | — | — | — | — | — | — | — |
| `p-reset` | function | `(&optional pattern)` | — | — | — | — | — | — | — |
| `p-study` | function | `(&optional str)` | — | — | — | — | — | — | — |

## module-system (6)

*No ir-spec §10 row.*  use/require and the @INC bookkeeping — ir-spec §9

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-note-inc` | function | `(module-name)` | — | — | — | — | — | — | — |
| `p-require` | function | `(module-name)` | — | — | — | — | — | — | — |
| `p-require-file` | function | `(path)` | — | — | — | — | — | — | — |
| `p-require-parent` | function | `(module-name)` | — | — | — | — | — | — | — |
| `p-require-version` | function | `(ver)` | — | — | — | — | — | — | — |
| `p-use` | function | `(module-name &key (import-args :default) (do-import t) into)` | — | — | — | — | — | — | — |

## oo (5)

*No ir-spec §10 row.*  method dispatch and C3 — ir-spec §7.3

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-get-class` | function | `(obj)` | — | — | — | — | — | — | — |
| `p-method-call` | function | `(obj method &rest args)` | — | — | — | — | — | — | — |
| `p-resolve-invocant` | function | `(name)` | — | — | — | — | — | — | — |
| `p-super-call` | function | `(obj method current-class &rest args)` | — | — | — | — | — | — | — |
| `perl-pkg-to-clos-class` | function | `(name)` | — | — | — | — | — | — | — |

## overload (6)

*No ir-spec §10 row.*  `use overload` registry and dispatch

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `*p-overload-table*` | variable | `` | — | — | — | — | — | — | — |
| `p-call-overload` | function | `(handler self other reversedp &optional op-str)` | — | — | — | — | — | — | — |
| `p-find-overload` | function | `(val op-str)` | — | — | — | — | — | — | — |
| `p-overload-strval` | function | `(obj)` | — | — | — | — | — | — | — |
| `p-overloaded` | function | `(obj)` | — | — | — | — | — | — | — |
| `p-register-overloads` | function | `(pkg pairs-vec)` | — | — | — | — | — | — | — |

## pack (2)

*No ir-spec §10 row.*  pack/unpack (a transpiled artifact, cl/pcl-pack.lisp)

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-pack` | function | `(&rest args)` | — | — | — | — | — | — | — |
| `p-unpack` | function | `(&rest args)` | — | — | — | — | — | — | — |

## package-tracking (5)

*No ir-spec §10 row.*  the caller()/__PACKAGE__ bookkeeping — docs/caller-implementation.md

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `*pcl-caller-pkg-stack*` | variable | `` | — | — | — | — | — | — | — |
| `*pcl-caller-subname-stack*` | variable | `` | — | — | — | — | — | — | — |
| `*pcl-current-package*` | variable | `` | — | — | — | — | — | — | — |
| `p-register-pkg-name` | function | `(pkg perl-name)` | — | — | — | — | — | — | — |
| `p-set-current-package` | function | `(pkg perl-name)` | — | — | — | — | — | — | — |

## phase (8)

*No ir-spec §10 row.*  BEGIN/CHECK/INIT/END and the eval-when wrappers — ir-spec §9

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `*check-blocks*` | variable | `` | — | — | — | — | — | — | — |
| `*end-blocks*` | variable | `` | — | — | — | — | — | — | — |
| `*init-blocks*` | variable | `` | — | — | — | — | — | — | — |
| `*unitcheck-blocks*` | variable | `` | — | — | — | — | — | — | — |
| `p-BEGIN` | macro | `(&body body)` | — | — | — | — | — | — | — |
| `p-CHECK` | macro | `(&body body)` | — | — | — | — | — | — | — |
| `p-eval-always` | macro | `(&body body)` | — | — | — | — | — | — | — |
| `p-run-compile-phase-blocks` | function | `nil` | — | — | — | — | — | — | — |

## process (11)

*No ir-spec §10 row.*  fork/exec/wait/kill/exit and the process ids

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-exec` | function | `(&rest args)` | — | — | — | — | — | — | — |
| `p-exit` | function | `(&optional code)` | — | — | — | — | — | — | — |
| `p-fork` | function | `nil` | — | — | — | — | — | — | — |
| `p-getpgrp` | function | `(&optional pid)` | — | — | — | — | — | — | — |
| `p-getppid` | function | `nil` | — | — | — | — | — | — | — |
| `p-getpriority` | function | `(which who)` | — | — | — | — | — | — | — |
| `p-kill` | function | `(signal &rest pids)` | — | — | — | — | — | — | — |
| `p-setpgrp` | function | `(&optional pid pgrp)` | — | — | — | — | — | — | — |
| `p-system` | function | `(&rest args)` | — | — | — | — | — | — | — |
| `p-wait` | function | `nil` | — | — | — | — | — | — | — |
| `p-waitpid` | function | `(pid &optional (flags 0))` | — | — | — | — | — | — | — |

## range (8)

*No ir-spec §10 row.*  the range and flip-flop operators — ir-spec §3.4

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-..` | function | `(start end)` | — | — | — | — | — | — | — |
| `p-...` | function | `(start end)` | — | — | — | — | — | — | — |
| `p-flipflop` | macro | `(id left-form right-form)` | — | — | — | — | — | — | — |
| `p-flipflop-3` | macro | `(id left-form right-form)` | — | — | — | — | — | — | — |
| `p-flipflop-dyn` | macro | `(id left-form right-form)` | — | — | — | — | — | — | — |
| `p-flipflop-dyn-3` | macro | `(id left-form right-form)` | — | — | — | — | — | — | — |
| `p-flipflop-num` | macro | `(id left-num right-num)` | — | — | — | — | — | — | — |
| `p-flipflop-num-3` | macro | `(id left-num right-num)` | — | — | — | — | — | — | — |

## refaliasing (7)

*No ir-spec §10 row.*  use feature 'refaliasing' assignment targets

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-alias-array-elements` | function | `(arr refs)` | — | — | — | — | — | — | — |
| `p-alias-array-slot` | function | `(arr idx ref)` | — | — | — | — | — | — | — |
| `p-alias-array-target` | function | `(ref)` | — | — | — | — | — | — | — |
| `p-alias-code-target` | function | `(ref)` | — | — | — | — | — | — | — |
| `p-alias-hash-slot` | function | `(hash key ref)` | — | — | — | — | — | — | — |
| `p-alias-hash-target` | function | `(ref)` | — | — | — | — | — | — | — |
| `p-alias-scalar-target` | function | `(ref)` | — | — | — | — | — | — | — |

## reference (20)

*No ir-spec §10 row.*  reference construction, deref casts, lvalue cells — ir-spec §2.5

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-arylen-lvalue-cell` | function | `(arr)` | — | — | — | — | — | — | — |
| `p-arylen-ref` | function | `(arr)` | — | — | — | — | — | — | — |
| `p-backslash` | function | `(val)` | — | — | — | — | — | — | — |
| `p-backslash-list` | name-only | `` | — | — | — | — | — | — | — |
| `p-backslash-sub` | function | `(sym)` | — | — | — | — | — | — | — |
| `p-backslash-sub-ref` | function | `(val)` | — | — | — | — | — | — | — |
| `p-box-for-local` | function | `(value)` | — | — | — | — | — | — | — |
| `p-cast-$` | function | `(val)` | — | — | — | — | — | — | — |
| `p-cast-%` | function | `(val)` | — | — | — | — | — | — | — |
| `p-cast-@` | function | `(val)` | — | — | — | — | — | — | — |
| `p-get-coderef` | function | `(name-val)` | — | — | — | — | — | — | — |
| `p-isweak` | function | `(ref)` | — | — | — | — | — | — | — |
| `p-pos-lvalue-cell` | function | `(var)` | — | — | — | — | — | — | — |
| `p-pos-ref` | function | `(var)` | — | — | — | — | — | — | — |
| `p-refgen-list` | function | `(val)` | — | — | — | — | — | — | — |
| `p-substr-lvalue-cell` | function | `(str start &optional len)` | — | — | — | — | — | — | — |
| `p-substr-ref` | function | `(str start &optional len)` | — | — | — | — | — | — | — |
| `p-vec-lvalue-cell` | function | `(str offset bits)` | — | — | — | — | — | — | — |
| `p-vec-ref` | function | `(str offset bits)` | — | — | — | — | — | — | — |
| `p-weaken` | function | `(ref)` | — | — | — | — | — | — | — |

## runtime-config (1)

*No ir-spec §10 row.*  runtime switches a test or REPL may flip

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `*p-raw-elems*` | global | `` | — | — | — | — | — | — | — |

## signature (3)

*No ir-spec §10 row.*  signature arity and slurpy binding — ir-spec §5.2

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-check-arity` | function | `(funcname got min max flexible &optional hash-start)` | — | — | — | — | — | — | — |
| `p-sig-rest-array` | function | `(args start)` | — | — | — | — | — | — | — |
| `p-sig-rest-hash` | function | `(args start)` | — | — | — | — | — | — | — |

## socket (15)

*No ir-spec §10 row.*  the socket builtins

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-accept` | macro | `(newfh serverfh)` | — | — | — | — | — | — | — |
| `p-bind` | macro | `(fh name)` | — | — | — | — | — | — | — |
| `p-connect` | macro | `(fh name)` | — | — | — | — | — | — | — |
| `p-getpeername` | macro | `(fh)` | — | — | — | — | — | — | — |
| `p-getprotobyname` | function | `(name)` | — | — | — | — | — | — | — |
| `p-getprotobynumber` | function | `(number)` | — | — | — | — | — | — | — |
| `p-getsockname` | macro | `(fh)` | — | — | — | — | — | — | — |
| `p-getsockopt` | macro | `(fh level optname)` | — | — | — | — | — | — | — |
| `p-listen` | macro | `(fh queue)` | — | — | — | — | — | — | — |
| `p-recv` | macro | `(fh buf len flags)` | — | — | — | — | — | — | — |
| `p-send` | macro | `(fh msg flags &optional to)` | — | — | — | — | — | — | — |
| `p-setsockopt` | macro | `(fh level optname optval)` | — | — | — | — | — | — | — |
| `p-shutdown` | macro | `(fh how)` | — | — | — | — | — | — | — |
| `p-socket` | macro | `(fh domain type protocol)` | — | — | — | — | — | — | — |
| `p-socketpair` | macro | `(fh1 fh2 domain type protocol)` | — | — | — | — | — | — | — |

## sub-definition (5)

*No ir-spec §10 row.*  the sub-definition and frame forms — ir-spec §5.1

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-args-body` | macro | `(&body body)` | — | — | — | — | — | — | — |
| `p-cloned-sub` | function | `(f)` | — | — | — | — | — | — | — |
| `p-declare-sub` | macro | `(name)` | — | — | — | — | — | — | — |
| `p-defpackage` | macro | `(name &rest options)` | — | — | — | — | — | — | — |
| `p-sub-frame` | macro | `(&body body)` | — | — | — | — | — | — | — |

## tap (27)

*No ir-spec §10 row.*  the Test::More layer (cl/pcl-test.lisp) — not part of the language

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `pl-BAIL_OUT` | function(lazy) | `` | — | — | — | — | — | — | — |
| `pl-_diag` | function(lazy) | `` | — | — | — | — | — | — | — |
| `pl-can_ok` | function(lazy) | `` | — | — | — | — | — | — | — |
| `pl-cmp_ok` | function(lazy) | `` | — | — | — | — | — | — | — |
| `pl-curr_test` | function(lazy) | `` | — | — | — | — | — | — | — |
| `pl-diag` | function(lazy) | `` | — | — | — | — | — | — | — |
| `pl-done_testing` | function(lazy) | `` | — | — | — | — | — | — | — |
| `pl-eq_array` | function(lazy) | `` | — | — | — | — | — | — | — |
| `pl-explain` | function(lazy) | `` | — | — | — | — | — | — | — |
| `pl-fail` | function(lazy) | `` | — | — | — | — | — | — | — |
| `pl-is` | function(lazy) | `` | — | — | — | — | — | — | — |
| `pl-is_deeply` | function(lazy) | `` | — | — | — | — | — | — | — |
| `pl-isa_ok` | function(lazy) | `` | — | — | — | — | — | — | — |
| `pl-isnt` | function(lazy) | `` | — | — | — | — | — | — | — |
| `pl-like` | function(lazy) | `` | — | — | — | — | — | — | — |
| `pl-locales_enabled` | function(lazy) | `` | — | — | — | — | — | — | — |
| `pl-native_to_unicode` | function | `(&optional cp)` | — | — | — | — | — | — | — |
| `pl-note` | function(lazy) | `` | — | — | — | — | — | — | — |
| `pl-ok` | function(lazy) | `` | — | — | — | — | — | — | — |
| `pl-pass` | function(lazy) | `` | — | — | — | — | — | — | — |
| `pl-plan` | function(lazy) | `` | — | — | — | — | — | — | — |
| `pl-require_ok` | function(lazy) | `` | — | — | — | — | — | — | — |
| `pl-skip` | function(lazy) | `` | — | — | — | — | — | — | — |
| `pl-skip_all` | function(lazy) | `` | — | — | — | — | — | — | — |
| `pl-unicode_to_native` | function | `(&optional cp)` | — | — | — | — | — | — | — |
| `pl-unlike` | function(lazy) | `` | — | — | — | — | — | — | — |
| `pl-use_ok` | function(lazy) | `` | — | — | — | — | — | — | — |

## tie (8)

*No ir-spec §10 row.*  `tie`/`untie`/`tied` and the proxy — ir-spec §2.2b

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `make-p-tie-proxy` | function | `(&key ((:tie-obj #:tie-obj) nil) ((:saved-value #:saved-value) nil) ((:untied #:untied) nil))` | — | — | — | — | — | — | — |
| `p-tie` | function | `(box classname &rest args)` | — | — | — | — | — | — | — |
| `p-tie-proxy` | class | `` | — | — | — | — | — | — | — |
| `p-tie-proxy-p` | function | `(sb-kernel::object)` | — | — | — | — | — | — | — |
| `p-tie-proxy-saved-value` | function | `(sb-kernel:instance)` | — | — | — | — | — | — | — |
| `p-tie-proxy-tie-obj` | function | `(sb-kernel:instance)` | — | — | — | — | — | — | — |
| `p-tied` | function | `(box)` | — | — | — | — | — | — | — |
| `p-untie` | function | `(box)` | — | — | — | — | — | — | — |

## time (6)

*No ir-spec §10 row.*  time/sleep/alarm/localtime/gmtime

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-alarm` | function | `(&optional secs)` | — | — | — | — | — | — | — |
| `p-gmtime` | function | `(&optional time)` | — | — | — | — | — | — | — |
| `p-localtime` | function | `(&optional time)` | — | — | — | — | — | — | — |
| `p-sleep` | function | `(secs)` | — | — | — | — | — | — | — |
| `p-time` | function | `nil` | — | — | — | — | — | — | — |
| `p-times` | function | `(&key wantarray)` | — | — | — | — | — | — | — |

## typeglob (12)

*No ir-spec §10 row.*  typeglobs and glob assignment — ir-spec §7.1

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `make-p-typeglob` | function | `(package name)` | — | — | — | — | — | — | — |
| `p-dynamic-typeglob` | function | `(name-box)` | — | — | — | — | — | — | — |
| `p-glob-assign` | function | `(pkg-str name-str rhs)` | — | — | — | — | — | — | — |
| `p-glob-assign-dynamic` | function | `(name-box rhs)` | — | — | — | — | — | — | — |
| `p-glob-copy` | function | `(dst-pkg dst-uname src-glob)` | — | — | — | — | — | — | — |
| `p-glob-slot` | function | `(glob slot)` | — | — | — | — | — | — | — |
| `p-glob-undef` | function | `(pkg uname)` | — | — | — | — | — | — | — |
| `p-make-typeglob` | function | `(pkg-str name-str)` | — | — | — | — | — | — | — |
| `p-typeglob` | class | `` | — | — | — | — | — | — | — |
| `p-typeglob-name` | function | `(sb-kernel:instance)` | — | — | — | — | — | — | — |
| `p-typeglob-p` | function | `(sb-kernel::object)` | — | — | — | — | — | — | — |
| `p-typeglob-package` | function | `(sb-kernel:instance)` | — | — | — | — | — | — | — |

## user-db (11)

*No ir-spec §10 row.*  the passwd/group database builtins

| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |
|---|---|---|---|---|---|---|---|---|---|
| `p-endgrent` | function | `(&key (wantarray (eq *wantarray* t)))` | — | — | — | — | — | — | — |
| `p-endpwent` | function | `(&key (wantarray (eq *wantarray* t)))` | — | — | — | — | — | — | — |
| `p-getgrent` | function | `(&key (wantarray (eq *wantarray* t)))` | — | — | — | — | — | — | — |
| `p-getgrgid` | function | `(gid &key (wantarray (eq *wantarray* t)))` | — | — | — | — | — | — | — |
| `p-getgrnam` | function | `(name &key (wantarray (eq *wantarray* t)))` | — | — | — | — | — | — | — |
| `p-getlogin` | function | `nil` | — | — | — | — | — | — | — |
| `p-getpwent` | function | `(&key (wantarray (eq *wantarray* t)))` | — | — | — | — | — | — | — |
| `p-getpwnam` | function | `(name &key (wantarray (eq *wantarray* t)))` | — | — | — | — | — | — | — |
| `p-getpwuid` | function | `(uid &key (wantarray (eq *wantarray* t)))` | — | — | — | — | — | — | — |
| `p-setgrent` | function | `(&key (wantarray (eq *wantarray* t)))` | — | — | — | — | — | — | — |
| `p-setpwent` | function | `(&key (wantarray (eq *wantarray* t)))` | — | — | — | — | — | — | — |
