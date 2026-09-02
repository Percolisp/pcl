;;;; Copyright (c) 2025-2026 the PCL authors
;;;; This is free software; you can redistribute it and/or modify it under the
;;;; same terms as the Perl 5 programming language system itself.
;;;; SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

;;;; skip-registry.lisp — declarative registry of documented not-supported tests.
;;;;
;;;; Instead of hand-editing perl-tests/*.t to disable tests PCL deliberately does
;;;; not support, list them here keyed by file basename + a regex on the test
;;;; DESCRIPTION.  cl/pcl-test.lisp's `test-ok` converts a *failing* matched test
;;;; into a real TAP `# skip`; a matched test that PASSES is flagged stale.  The
;;;; assertion still runs, so nothing is weakened (CLAUDE.md principle 5), and the
;;;; perl-tests/*.t files stay byte-identical to upstream Perl (diffable).
;;;;
;;;; Each entry: (DESCRIPTION-REGEX  :CATEGORY  "reason — cite docs/not-supported.md").
;;;; Categories: :principle9 (error detection of invalid Perl), :error-msg,
;;;; :warning-emit, :read-only, :utf8, :destroy-gc, :lvalue, :alias, :tie,
;;;; :xs (XS / C-level: pointer pack types, DynaLoader, etc.),
;;;; :feature (a deliberately-unimplemented language feature, e.g. computed goto).

(in-package :pcl)

;; index.t 49-58 — utf8::encode byte/char-flag distinction.  After utf8::encode a
;; string is treated as raw octets in Perl (each multi-byte char expands to its
;; UTF-8 bytes), so index/rindex use BYTE offsets and an octet-string never matches
;; a character-string.  PCL has no per-scalar UTF-8 flag (CL strings are always
;; Unicode), so utf8::encode is a no-op and index/rindex match on characters.
;; 49/50 are named; 51-58 are UNNAMED -> keyed by test number.
(register-skips "index.t"
                ("^index octets, octets"
                 :utf8
                 "index on a utf8::encode'd byte-string must use byte offsets (expect 7) -- PCL has no per-scalar UTF-8 flag (CL strings are always Unicode). not-supported.md: 'Unicode semantics differences' (utf8::encode/decode).")
                ("rindex octets, octets"
                 :utf8
                 "rindex on a utf8::encode'd byte-string must use byte offsets (expect 21) -- PCL has no per-scalar UTF-8 flag. not-supported.md: 'Unicode semantics differences' (utf8::encode/decode).")
                (51 :utf8 "index octet-string vs utf8-string must not match (expect -1) -- no UTF-8 flag. not-supported.md: 'Unicode semantics differences'.")
                (52 :utf8 "rindex octet-string vs utf8-string must not match (expect -1) -- no UTF-8 flag. not-supported.md: 'Unicode semantics differences'.")
                (53 :utf8 "index utf8-string vs octet-string must not match (expect -1) -- no UTF-8 flag. not-supported.md: 'Unicode semantics differences'.")
                (54 :utf8 "rindex utf8-string vs octet-string must not match (expect -1) -- no UTF-8 flag. not-supported.md: 'Unicode semantics differences'.")
                (55 :utf8 "index octet-string vs char-string must not match (expect -1) -- no UTF-8 flag. not-supported.md: 'Unicode semantics differences'.")
                (56 :utf8 "rindex octet-string vs char-string must not match (expect -1) -- no UTF-8 flag. not-supported.md: 'Unicode semantics differences'.")
                (57 :utf8 "index char-string vs octet-string must not match (expect -1) -- no UTF-8 flag. not-supported.md: 'Unicode semantics differences'.")
                (58 :utf8 "rindex char-string vs octet-string must not match (expect -1) -- no UTF-8 flag. not-supported.md: 'Unicode semantics differences'."))

;; grep.t — DESTROY-via-GC + one error-detection failure.  Tests 69/71/73 ("grep
;; void/scalar/list post") and 75/76 ("block map void 2/3") need a blessed object's
;; DESTROY to fire when its refcount hits zero (after @a=(), or the immediate
;; void-context release of the map block's returned PADTMP); PCL never calls DESTROY
;; via GC.  Test 61 wants the compile-time "Missing comma after first argument to
;; grep" error.  not-supported.md: 'DESTROY called by garbage collector' (grep.t 69-76).
(register-skips "grep.t"
                ("proper error on variable as block"
                 :principle9
                 "grep with a variable (not a block/expr) as first arg must die 'Missing comma after first argument to grep function' -- error detection of invalid Perl. not-supported.md: 'Error compatibility for invalid Perl input'.")
                ("grep void post"
                 :destroy-gc
                 "after @a=() the 3 blessed grep args must be DESTROYed (count==3) -- PCL never calls DESTROY via GC. not-supported.md: 'DESTROY called by garbage collector'.")
                ("grep scalar post"
                 :destroy-gc
                 "after @a=() the 3 blessed grep args must be DESTROYed (count==3) -- PCL never calls DESTROY via GC. not-supported.md: 'DESTROY called by garbage collector'.")
                ("grep list post"
                 :destroy-gc
                 "after @a=() the 3 blessed grep args must be DESTROYed (count==3) -- PCL never calls DESTROY via GC. not-supported.md: 'DESTROY called by garbage collector'.")
                ("block map void 2"
                 :destroy-gc
                 "map in void context must DESTROY each block's returned PADTMP immediately so count resets to 1 -- PCL never calls DESTROY via GC. not-supported.md: 'DESTROY called by garbage collector'.")
                ("block map void 3"
                 :destroy-gc
                 "map in void context must DESTROY each block's returned PADTMP immediately so count resets to 1 -- PCL never calls DESTROY via GC. not-supported.md: 'DESTROY called by garbage collector'."))

(register-skips "tr.t"
                ("RT #130198 eval:"
                 :principle9
                 "chop/chomp of a tr/// result must die 'Can't modify transliteration in chop' — \
error detection of an invalid lvalue. not-supported.md: 'Error compatibility for invalid Perl input'.")
                ("RT #130198 warn: cho(p|mp)\\(@a"
                 :principle9
                 "chop/chomp(@a =~ tr///) must warn — error detection of an invalid lvalue (array as tr target). \
not-supported.md: 'Error compatibility for invalid Perl input'. (Scalar warn: variants legitimately pass.)")
                ("Illegal to tr.* named sequence"
                 :error-msg
                 "tr/// with a \\N{NAMED SEQUENCE} must die with a specific message. not-supported.md: 'Error message text and format'.")
                ("UTF-8 range with min > max"
                 :error-msg
                 "reversed \\x{}-\\x{} range must die 'Invalid range ... in transliteration operator'. not-supported.md: 'Error message text and format'.")
                ("y///r error message"
                 :error-msg
                 "!~ with y///r must die with a specific message. not-supported.md: 'Error message text and format'.")
                ("/r warns "
                 :warning-emit
                 "tr///r in void context warns once ('Useless use of ...'). PCL under-emits diagnostic warnings. not-supported.md: warning emission.")
                ("tr/a-z-9"
                 :error-msg
                 "ambiguous range 'a-z-9' must die 'Ambiguous range in transliteration operator'. not-supported.md: 'Error message text and format'.")
                ("reversed range check"
                 :error-msg
                 "reversed range in tr/// must die. not-supported.md: 'Error message text and format'.")
                ("LHS bad on updating tr"
                 :principle9
                 "tr/// updating a bad lvalue must die — error detection of invalid Perl. not-supported.md: 'Error compatibility for invalid Perl input'.")
                ("fails on zero-length ro string"
                 :read-only
                 "tr/// on a zero-length read-only string must die 'Modification of a read-only value'. not-supported.md: read-only scalars / Internals.")
                )

;; undef.t 16 and 18 are UNNAMED (no description) -> keyed by test number.
;; not-supported.md: 'Read-only constants via \undef stash tricks'.
;;
;; TEST 17 WAS HERE AND IS GONE (s460ap): it is `eval { $1 = undef }`, and #873
;; made a WRITE to a capture variable perl's trappable read-only death, so the
;; row PASSES ON ITS OWN MERIT.  The stale-detector said so in the same run
;; ("REGISTRY-STALE: undef.t test 17 now passes"), which is what this entry's
;; removal answers.  Its two neighbours stay: 16 is `eval { undef $1 }` — the
;; `undef` BUILTIN applied to a capture, a write slot #873 does NOT name (the
;; corpus has zero occurrences outside this row) — and 18 is
;; `undef &constant_sub`, an unrelated constant slot.
(register-skips "undef.t"
                (16 :read-only
                    "undef of a read-only value must die 'Modification of a read-only value' — read-only scalars not emulated. not-supported.md: 'Read-only constants via \\undef stash tricks' (undef.t 16, 18).")
                (18 :read-only
                    "undef &constant_sub must die 'Can't modify constant item' — constant/read-only slots not emulated. not-supported.md: 'Read-only constants via \\undef stash tricks' (undef.t 16, 18)."))

;; not.t 21-24 — perl's `!0`/`!1` are ONE globally-shared read-only scalar, so
;; writing through `for (!0) { $_ = 43 }` dies and `\!0` is the same address every
;; time.  PCL's `pl-!`/`pl-not` return a fresh 1/"" per call: mutable, distinct
;; addresses.  (Task #159 gave ARRAYS a read-only representation; a read-only
;; SCALAR still has nowhere to carry the flag, and interning would need a global
;; constant table besides.)  These four were COMMENTED OUT of perl-tests/not.t
;; until s337 (#150 part 2) — the file is now byte-identical to t/op/not.t and the
;; assertions run.  not-supported.md: 'Interned boolean constants (!0 / !1 identity)'.
(register-skips "not.t"
                ("^not 0 is read-only"
                 :read-only
                 "for (!0) { $_ = 43 } must die 'Modification of a read-only value attempted' -- PCL's ! returns a fresh mutable value, not the shared read-only scalar. not-supported.md: 'Interned boolean constants (!0 / !1 identity)'.")
                ("^not 1 is read-only"
                 :read-only
                 "for (!1) { $_ = 43 } must die 'Modification of a read-only value attempted' -- PCL's ! returns a fresh mutable value. not-supported.md: 'Interned boolean constants (!0 / !1 identity)'.")
                ("^!0 returns the same value each time"
                 :read-only
                 "\\!0 must be the same address every call [perl #114838] -- PCL allocates a fresh value per negation. not-supported.md: 'Interned boolean constants (!0 / !1 identity)'.")
                ("^!1 returns the same value each time"
                 :read-only
                 "\\!1 must be the same address every call [perl #114838] -- PCL allocates a fresh value per negation. not-supported.md: 'Interned boolean constants (!0 / !1 identity)'."))

;; dor.t 26/28 — `f $x /2` and `print $fh /2` must be COMPILE ERRORS in perl
;; ("Search pattern not terminated": the `/` starts a match that never closes).
;; PCL parses them as division and raises nothing, so `$@` is empty.  Both rows
;; assert that INVALID Perl is rejected — CLAUDE.md principle 9, the same category
;; as cmpchain's 274 rows and do.t t63/t65 (ruled fable-answers-s318.md §4).  The
;; sibling rows either side (the same shapes with `/ 2`, which ARE valid) pass and
;; are deliberately not matched by these regexes.
(register-skips "dor.t"
                ("^Caught unterminated search pattern error message: empty subroutine"
                 :principle9
                 "`sub f ($) { } f $x /2` must die 'Search pattern not terminated' -- error detection of invalid Perl. not-supported.md: 'Error compatibility for invalid Perl input'.")
                ("^Caught unterminated search pattern error message: sub with built-in function"
                 :principle9
                 "`sub { print $fh /2 }` must die 'Search pattern not terminated' -- error detection of invalid Perl. not-supported.md: 'Error compatibility for invalid Perl input'."))

;; unshift.t's "croak when unshifting onto readonly array" was registered here
;; until task #159 (s337) made Internals::SvREADONLY(@a,1) swap the array's
;; storage for a fixed-size one.  The row PASSES now — do not re-register it.

;; chop.t 48-51 are UNNAMED `ok($@ =~ /Can't modify.../)` -> keyed by number.
(register-skips "chop.t"
                (48 :principle9
                    "chop($x)=1 must die 'Can't modify chop in assignment' — error detection of invalid Perl. not-supported.md: 'Error compatibility for invalid Perl input'.")
                (49 :principle9
                    "chomp($x)=1 must die 'Can't modify chomp in assignment' — error detection of invalid Perl. not-supported.md: 'Error compatibility for invalid Perl input'.")
                (50 :principle9
                    "chop($x,$y)=(1,2) must die 'Can't modify chop in assignment' — error detection of invalid Perl. not-supported.md: 'Error compatibility for invalid Perl input'.")
                (51 :principle9
                    "chomp($x,$y)=(1,2) must die 'Can't modify chomp in assignment' — error detection of invalid Perl. not-supported.md: 'Error compatibility for invalid Perl input'.")
                ("chomp @a when.*eq 0 and"
                 :alias
                 "result aliasing (\\$a[0] == \\$b after `chomp @a`, the $/ eq 0 case) -- the RESULT of chomp on an array element must stay the element's own SV. NB @_ argument aliasing itself WORKS in PCL (probed s464ay); the gap here is result-SV identity. not-supported.md: 'Sparse arrays (holes), element aliasing, and SV identity'. (The eq 7 sibling legitimately passes.)"))

(register-skips "crypt.t"
                ("crypt turns off utf8 on its target"
                 :utf8
                 "checks !utf8::is_utf8(result); PCL does not track the per-scalar UTF-8 flag \
(utf8::is_utf8 is a stub). not-supported.md: 'Unicode semantics differences' (utf8 flag)."))

(register-skips "readline.t"
                ("perl #19566"
                 :read-only
                 "readline into a read-only target must die 'Modification of a read-only value' — read-only scalars not emulated. not-supported.md: read-only scalars / Internals.")
                ("perl #123790"
                 :error-msg
                 "exact 'readline() on unopened filehandle' error wording. not-supported.md: 'Error message text and format'."))

(register-skips "ref.t"
                ;; Dereferencing an IO/FORMAT typeglob slot (*STDOUT{IO},
                ;; *STDERR{FORMAT}) as $$/@$/%$/&$/*$ must die "Not a SCALAR
                ;; reference" etc. PCL does not produce these errors, and FORMAT
                ;; is not implemented. (The passing "Glob dereference of PVIO is
                ;; acceptable" sibling is excluded by the $-anchor.)
                ("^(Scalar|Array|Hash|Code|Glob) dereference$"
                 :error-msg
                 "deref of an IO/FORMAT glob slot must die 'Not a X reference' — error detection PCL does not perform; FORMAT unsupported. not-supported.md: 'Error message text and format' + 'format / write report formatting'.")
                ;; (\substr / \pos / \vec lvalue refs are now IMPLEMENTED via
                ;;  p-magic-cell — session 219 — so they are no longer skipped.)
                ;; ref()/stringify of a ref to a FORMAT, and IO-handle stringify.
                ("ref to format|stringify for IO refs"
                 :error-msg
                 "ref to a FORMAT or IO handle ('FORMAT'/'IO::File=IO(...)') — format/write not implemented. not-supported.md: 'format / write report formatting'.")
                ;; Symbolic references to package variables whose names contain a
                ;; NUL or are UTF-8 encoded — Unicode/NUL stash names not supported.
                ("UTF8 representation is 3 chars|via the UTF8 byte sequence|via the correct name works"
                 :utf8
                 "symbolic ref to a stash entry with a NUL/UTF-8 name — Unicode/NUL stash lookup not supported. not-supported.md: 'Unicode semantics differences'.")
                ;; Assigning through a value aliased to a read-only literal, or
                ;; weakening a read-only ref, must die "Modification of a read-only".
                ("read-only ref|aliased to literal"
                 :read-only
                 "assignment to a literal-aliased value / weaken of a read-only ref must die 'Modification of a read-only value' — read-only scalars not emulated. not-supported.md: 'Read-only constants via \\undef stash tricks' / 'Internals::* C-level introspection'."))

;; substr.t — documented not-supported failures.  The for(substr())/\substr
;; lvalue-aliasing rows are now IMPLEMENTED (session: p-substr-lvalue-cell with
;; edit-tracking) and NOT skipped.  HELD BACK as fix targets, deliberately NOT
;; registered:
;;   - [perl #62646] huge 32-bit-overflow offsets (substr($a,0xffffffff,1) → undef
;;     + "substr outside of string" warning): out-of-range-offset semantics are
;;     potentially fixable (PCL returns '' not undef), entangled with warning
;;     emission — discuss before writing off (CLAUDE.md principle 4).
(register-skips "substr.t"
                ;; perl #24346: `sub { $_[0]=... }->(scalar substr ...)` writes the
                ;; substr lvalue through @_ — needs @_ pass-by-alias.
                ("scalar does not affect lvalueness of substr"
                 :alias
                 "modifying $_[0] (a scalar-substr lvalue) inside a sub must write back through @_ -- @_ argument aliasing WORKS in PCL (probed s464ay); the remaining gap named in that entry is that the CALLEE lowers substr's target as a value, so a substr lvalue passed through @_ is not written back (task #209). not-supported.md: '@_ argument aliasing'.")
                ;; perl #24200 / #128260: user `: lvalue` subs returning substr, and
                ;; substr/\substr on $#array (arylen) or an lvalue sub.
                (346 :lvalue
                     "user `sub bar : lvalue { substr ... }` then `bar = \"XXX\"` — user-defined lvalue subs are not implemented. not-supported.md: 'Lvalue subroutines'.")
                (391 :lvalue "substr($#ta,0,2)=23 — substr as an lvalue on $#array (arylen) magic. not-supported.md: 'Lvalue subroutines' / arylen magic.")
                (392 :lvalue "substr($#ta,0,2)=~s/// — substr-lvalue on arylen via s///. not-supported.md: 'Lvalue subroutines'.")
                (393 :lvalue "substr($#ta,0,2,23) — 4-arg substr on arylen. not-supported.md: 'Lvalue subroutines'.")
                (394 :lvalue "ta_tindex() = 23 — user `: lvalue` sub returning $#ta. not-supported.md: 'Lvalue subroutines'.")
                (395 :lvalue "substr(ta_tindex(),0,2)=23 — substr-lvalue on an lvalue sub. not-supported.md: 'Lvalue subroutines'.")
                (396 :lvalue "substr(ta_tindex(),0,2)=~s/// — substr-lvalue on an lvalue sub via s///. not-supported.md: 'Lvalue subroutines'.")
                (397 :lvalue "substr(ta_tindex(),0,2,23) — 4-arg substr on an lvalue sub. not-supported.md: 'Lvalue subroutines'.")
                ;; perl #128260: \substr of a whole hash/array (stringified aggregate).
                ("\\\\substr %h"
                 :lvalue
                 "${\\substr %h, 0} — \\substr of a stringified hash (perl #128260 assertion). not-supported.md: 'Lvalue subroutines'.")
                ("\\\\substr @a"
                 :lvalue
                 "${\\substr @a, 0} — \\substr of a stringified array (perl #128260 assertion). not-supported.md: 'Lvalue subroutines'.")
                ;; \substr does not coerce a glob/ref arg — Perl itself defers this.
                ("does not coerce its glob arg just yet"
                 :lvalue
                 "\\substr *glob must not coerce the glob (ref \\$x stays GLOB) — glob/substr coercion not modelled. not-supported.md: 'Lvalue subroutines'.")
                ;; DESTROY must fire when an lvalue-substr target is replaced.
                ("Timely scalar destruction with lvalue substr"
                 :destroy-gc
                 "DESTROY must fire when the object held by a substr-lvalue target is overwritten — PCL never calls DESTROY via GC. not-supported.md: 'DESTROY called by garbage collector'.")
                ;; (Test 142 — `substr($data{a},0,5,"")` on a TIED hash element —
                ;; was registered here as "4-arg substr does not write through tie
                ;; magic" and is DROPPED as of task #960: it passes.  The cause was
                ;; not tie at all.  The target is a hash ELEMENT, so under the s455
                ;; raw-element model the 4-arg call received the element's VALUE and
                ;; p-substr's write is silent on a non-box; #960 gives substr's
                ;; target argument perl's loose lvalue context, so the call now gets
                ;; the element's BOX — which is the box holding the tie proxy, and
                ;; box-set has always run STORE through one.  The registry's own
                ;; stale-detector is what found it: `# REGISTRY-STALE: substr.t
                ;; test 142 now passes`.)
                ;; UTF8-flag toggling: $refee blessed into a Unicode-named class,
                ;; stringified so its SvUTF8 flag flips, then 4-arg substr.  PCL has
                ;; no per-scalar UTF-8 flag (CL strings are always Unicode), so the
                ;; "flag turning off" comparison cannot reproduce.  (The companion
                ;; "turning on" row passes; only this one depends on the flag clearing.)
                ("4-arg substr with target UTF8ness turning off when stringified"
                 :utf8
                 "4-arg substr after stringifying a blessed ref whose class name clears the SvUTF8 flag — PCL has no per-scalar UTF-8 flag. not-supported.md: 'Unicode semantics differences'."))

;; length.t — UTF-8 / `use bytes` / `pack "U"` byte-vs-character tests.  PCL has
;; no per-scalar UTF-8 flag (CL strings are always Unicode), `use bytes` is not
;; implemented, and `pack "U", N` yields the UTF-8 bytes as characters rather than
;; a flagged 1-char string.  So `length` under `use bytes` (byte count) and the
;; byte_utf8a_to_utf8n string comparisons cannot match Perl.  All unnamed -> keyed
;; by test number.  not-supported.md: 'Unicode semantics differences'
;; (utf8 flag / `use bytes` / `pack 'U'`).  NOT registered (other reasons / fix
;; targets): 34 (length of tied undef into a reused TARG), 36/42 (uninit-stringify
;; warning emission).
(register-skips "length.t"
                (7  :utf8 "length(pack 'U',0xFF) must be 1 char -- PCL's pack 'U' yields UTF-8 bytes as chars (got 2). not-supported.md: 'Unicode semantics differences' (pack 'U').")
                (10 :utf8 "length(pack 'U',0xB6) must be 1 char -- pack 'U' yields UTF-8 bytes as chars. not-supported.md: 'Unicode semantics differences' (pack 'U').")
                (14 :utf8 "use bytes: $a eq byte_utf8a_to_utf8n('\\xc4\\x80') -- no per-scalar UTF-8 flag. not-supported.md: 'Unicode semantics differences' (use bytes).")
                (15 :utf8 "use bytes: length(\\x{100}) must be 2 bytes -- use bytes not implemented (got 1 char). not-supported.md: 'Unicode semantics differences' (use bytes).")
                (17 :utf8 "use bytes: $a eq byte_utf8a_to_utf8n('\\xc4\\x80\\xc2\\xb6') -- no UTF-8 flag. not-supported.md: 'Unicode semantics differences' (use bytes).")
                (18 :utf8 "use bytes: length(\\x{100}\\x{B6}) must be 4 bytes (got 2 chars) -- use bytes not implemented. not-supported.md: 'Unicode semantics differences' (use bytes).")
                (20 :utf8 "use bytes: $a eq byte_utf8a_to_utf8n('\\xc2\\xb6\\xc4\\x80') -- no UTF-8 flag. not-supported.md: 'Unicode semantics differences' (use bytes).")
                (21 :utf8 "use bytes: length(\\x{B6}\\x{100}) must be 4 bytes (got 2 chars) -- use bytes not implemented. not-supported.md: 'Unicode semantics differences' (use bytes).")
                (23 :utf8 "use bytes: length(tied \\x{263A}) must be 3 bytes (got 1 char) -- use bytes not implemented. not-supported.md: 'Unicode semantics differences' (use bytes)."))

;; infnan.t — pack/unpack pointer types `p` and `P`.  `pack 'p'/'P'` packs a raw
;; memory address; under CL's moving GC there are no stable addresses, so PCL
;; throws "Invalid type 'p'/'P'" and the eval-wrapped roundtrip yields undef
;; (expected "Inf"/"-Inf"/"NaN").  not-supported.md: 'pack/unpack — pointer types
;; (p/P)'.  These six are the file's only failures.
(register-skips "infnan.t"
                ("^pack [pP] "
                 :xs
                 "unpack(...,pack 'p'/'P',$inf_or_nan) roundtrips a raw pointer -- no stable addresses under a moving GC; PCL throws 'Invalid type'. not-supported.md: 'pack/unpack — pointer types (p/P)'."))

;; push.t's "croak when pushing onto readonly array" was registered here until
;; task #159 (s337): Internals::SvREADONLY(@a,1) now swaps the array's storage
;; for a fixed-size one, so the push dies with perl's message and the row
;; PASSES.  Its sibling "can push empty list onto readonly array" passes too —
;; a push that would store nothing is legal even on a read-only array.  Do not
;; re-register either.

;; array.t — documented not-supported failures (sparse arrays / @_ aliasing / SV
;; identity / error-detection). HELD BACK as fix targets, deliberately NOT registered:
;;   - arylen magic (\$#array, freed-array length, arylen_p): tests 83-88, 92-114,
;;     126, 172 — needs a write-through magical-lvalue + Perl refcount/freed-state.
;;   - the `map +(LIST)` unary-plus parse bug (tests 118, 121) — a real fixable PExpr
;;     bug (the `+(` map disambiguator collapses the list), not not-supported.
(register-skips "array.t"
                ;; $a[-1]=0 on an empty array must die "Modification of non-creatable
                ;; array value attempted" — error detection of an invalid index.
                ("\\$a\\[-1\\] = 0"
                 :principle9
                 "$a[-1]=0 on an empty array must die 'Modification of non-creatable array value attempted, subscript -1' — error detection of invalid Perl. not-supported.md: 'Error compatibility for invalid Perl input'.")
                ;; Writing through an @_ alias to a non-creatable negative index must
                ;; die. (The 'reading alias ...' siblings legitimately pass.)
                ("error when setting alias to (negative index past beginning|-1 elem of empty array)"
                 :principle9
                 "assigning through an @_ alias to a non-creatable negative index must die 'Modification of non-creatable array value attempted' — error detection. not-supported.md: 'Error compatibility for invalid Perl input'.")
                ;; ("exists returns true for &PL_sv_undef elem" …) DROPPED s457ai:
                ;; passes since p-exists-array stopped reading "is a box" as
                ;; "exists" — the hole marker is NIL, and `$a[0] = undef` leaves
                ;; a live slot either way (stale-detector).
                ;; ("undef preserves identity in array" …) DROPPED s295b:
                ;; passes since $#a++ extends with real holes (stale-detector).
                ;; @_ aliasing to nonexistent (sparse) elements.
                ("\\@_ alias to nonexistent"
                 :alias
                 "writing through an @_ alias to a nonexistent array element must autovivify the caller's element -- WRONG REASON, corrected s464ay: the named primitive WORKS (`$#a=3; sub setit { $_[0]="v" } setit($a[1])` vivifies in PCL exactly as in perl, probed).  This row therefore fails for an as-yet-unidentified reason and its registration is provisional -- re-probe it against the file before trusting this line. not-supported.md: '@_ argument aliasing'.")
                ;; Sparse-array holes preserved through subs / reads.
                ("holes passed to sub do not lose their position"
                 :alias
                 "array holes (nonexistent elements) passed to a sub must keep their position — PCL has no defelem/hole. not-supported.md: 'Sparse arrays (holes), element aliasing, and SV identity'.")
                ("non-elems read from (magical )?\\@a do not lose their position"
                 :alias
                 "reading a hole must not vivify or shift later elements — sparse arrays not emulated. not-supported.md: 'Sparse arrays (holes), element aliasing, and SV identity'. (s295b: the 'magical @a' sibling is INCLUDED again — it only ever passed via the pre-hole-fix box-filled $#a++ representation.)")
                ;; Lazy element creation / map-no-vivify — these share descriptions
                ;; with PASSING siblings ('copying an array via =', extra refgen/map
                ;; assertions), so keyed by current TAP number, not regex.
                ;; (174 …) DROPPED s316e: passes since %p-defelem-box gives
                ;; @_ holes lazy defelem aliasing (stale-detector).
                ;; (176 …) DROPPED s295b: passes since $#a++ extends with
                ;; real holes (stale-detector).
                ;; (179 …) DROPPED s316e: passes since the foreach alias is a
                ;; write-through defelem box (stale-detector).
                ;; (181 …) DROPPED s295b: passes since $#a++ extends with
                ;; real holes (stale-detector).
                ;; (184 …) DROPPED s295c: passes under the default (v2)
                ;; pipeline — the our-alias requalify pre-pass homes the
                ;; magical @a to tmp::a, and the hole-preserving $#a++ makes
                ;; map{} see the real holes (stale-detector).  (It used to be
                ;; noted here that PCL_V1=1 still failed this row — v1 had no
                ;; requalify pass.  The v1 entry is gone: E4.1 step 2, #242.)
                )

;; NB qr.t test 6 ("my $b1=$b; bless $b" — $b1 should also be blessed) is the same
;; scalar-identity limitation, but its description "object is blessed" is shared with
;; the PASSING test 1, so it can't be uniquely description-keyed; left as a baselined
;; failure rather than adding a fragile number-key for one cosmetic row.

;; state.t — \state identity (same address each call) + computed goto into a label
;; held in a state variable.
(register-skips "state.t"
                ("^Reference to state variable$"
                 :alias
                 "\\state $x must yield the same address on every call -- PCL re-boxes on scalar copy. not-supported.md: 'Scalar copy does not preserve reference/SV identity'.")
                ("computed goto"
                 :feature
                 "goto EXPR to a runtime-computed label is not implementable in CL (tags are lexical, not first-class). not-supported.md: 'Computed goto (goto EXPR)'."))

;; lex.t — perl-lexer torture rows.  The five "<sigil> <null> ident" tests use
;; a NUL byte as the first identifier character ($\0eq is a variable literally
;; named "\0eq"); PPI cannot tokenize that and no real code writes it —
;; decision 2026-07-24.  The two [perl #NNNNN] rows expect perl's exact
;; compile-error text ("Bareword found ...", "Not enough arguments for flock")
;; for invalid input.
(register-skips "lex.t"
                ("^\\$ <null> ident$"
                 :feature
                 "NUL byte as first identifier char ($\\0eq) -- PPI cannot tokenize it; no real code. not-supported.md: 'NUL bytes (and other control characters) in identifiers'.")
                ("^@ <null> ident$"
                 :feature
                 "NUL byte as first identifier char (@\\0eq). not-supported.md: 'NUL bytes (and other control characters) in identifiers'.")
                ("^% <null> ident$"
                 :feature
                 "NUL byte as first identifier char (%\\0eq). not-supported.md: 'NUL bytes (and other control characters) in identifiers'.")
                ("^& <null> ident$"
                 :feature
                 "NUL byte as first identifier char (&\\0eq). not-supported.md: 'NUL bytes (and other control characters) in identifiers'.")
                ("^\\* <null> ident$"
                 :feature
                 "NUL byte as first identifier char (*\\0eq). not-supported.md: 'NUL bytes (and other control characters) in identifiers'.")
                ;; The SIXTH NUL row (#228, registered in the E4.1 step-2 flip
                ;; commit and not before).  `do\0000000` is a fresh_perl child
                ;; whose source carries a NUL byte, so PPI cannot tokenize it.
                ;; It PASSED until the flip only by accident: the v1 fallback's
                ;; --lenient-ppi truncated the unparseable source to nothing and
                ;; the row asserts empty output, so PCL scored a pass without
                ;; running the test's semantics.  With one pipeline the parse
                ;; failure is honest and loud.  RULED s345 §1: register, never
                ;; teach the pipeline NUL (that is teaching PPI a token it
                ;; cannot represent).
                ("^\\[perl #129069\\] - no output and valgrind clean$"
                 :feature
                 "NUL byte in the source of a fresh_perl child (do\\0000000) -- PPI cannot tokenize it. not-supported.md: 'NUL bytes (and other control characters) in identifiers'.")
                ("Assert failure when mentioning a constant twice"
                 :error-msg
                 "expects perl's exact 'Bareword found where operator expected' compile error for invalid input. not-supported.md: 'Error message text and format' / 'Error compatibility for invalid Perl input'.")
                ("intuit_method\\(\\) invalidates PL_bufptr"
                 :error-msg
                 "expects perl's exact 'Not enough arguments for flock' compile error for invalid input. not-supported.md: 'Error message text and format'."))

;; do.t 64/66/67/68 — `do SUBROUTINE(LIST)` was REMOVED from perl in 5.20, so
;; perl now rejects it at parse time and the test asserts only that rejection
;; ("like $@, qr/\Asyntax error/", do.t:280).  Rejecting invalid Perl is out of
;; scope (CLAUDE.md 9) and the exact wording is not a goal (user 2026-07-28), so
;; the four rejection assertions are the #149 blanket category.
;;
;; NOT registered, deliberately: t63 and t65 ("do subname(arg) called", "do
;; subname() called").  Those are the file's fail() guards, and they fire
;; because PCL actually EXECUTES the removed pre-5.20 form — a behavioural
;; divergence with a side effect, not a message mismatch, so it stays a visible
;; failure and a fix target (task #158).
(register-skips "do.t"
                ("^do subname\\(\"arg\"\\) is syntax error$"
                 :principle9
                 "`do subname(\"arg\")` was removed in perl 5.20 and must now be a parse-time 'syntax error' -- rejection of invalid Perl. not-supported.md: 'Error compatibility for invalid Perl input'.")
                ("^do subname\\(\\) is syntax error$"
                 :principle9
                 "`do subname()` was removed in perl 5.20 and must now be a parse-time 'syntax error' -- rejection of invalid Perl. not-supported.md: 'Error compatibility for invalid Perl input'.")
                ("^do \\$subref\\(\"arg\"\\) is syntax error$"
                 :principle9
                 "`do $subref(\"arg\")` was removed in perl 5.20 and must now be a parse-time 'syntax error' -- rejection of invalid Perl. not-supported.md: 'Error compatibility for invalid Perl input'.")
                ("^do \\$subref\\(\\) is syntax error$"
                 :principle9
                 "`do $subref()` was removed in perl 5.20 and must now be a parse-time 'syntax error' -- rejection of invalid Perl. not-supported.md: 'Error compatibility for invalid Perl input'."))

;; sub.t — the 11 inline `ok(1, 'SKIP: …')` rows were RESTORED to upstream
;; (task #965, s464ay).  Two of them PASS now and were hiding a fix nobody
;; counted: the `[perl #91844]` "result of shift is copied when returned" row
;; (fixed by #964's return-copy protocol — the inline skip blamed "@_ aliasing
;; not supported", which is FALSE: @_ aliasing works, probed) and "eval q/$var/
;; in named sub in predeclared sub" (fixed by #401 — the inline skip blamed a
;; "string eval subprocess", which stopped existing long ago).  Of the nine
;; that fail honestly, eight are registered below and one is a real bug held
;; back as a fix target:
;;   - "goto &xsub when @_ does not exist" — NOT registered: `undef *GLOB` is a
;;     SILENT NO-OP in PCL for all four slots (task #1020), so *_{ARRAY} stays
;;     defined.  A silent-wrong, hence a fix target, not a skip.
;; Three of the eight had a WRONG written reason and are registered against the
;; cause that actually fires (measured, s464ay):
;;   - "content of nonexistent $_[0] …" blamed @_ aliasing; the cause is that
;;     `utf8::encode` is a no-op (perl's stringifies the hole to "").  The
;;     hole-write itself works: `sub w { $_[0]="" } local @_=(); $#_++; &w`
;;     agrees with perl.
;;   - "RT124156 plain"/"depth" blamed the local-unwind DELETE callback; the
;;     cause is that `tie %hash` is not implemented AT ALL (announced, #155).
(register-skips "sub.t"
                ("content of nonexistent"
                 :utf8
                 "&utf8::encode on a nonexistent @_ element must leave it defined as \"\" -- PCL's utf8::encode is a no-op (no per-scalar UTF-8 flag), so the hole stays undef. not-supported.md: 'Unicode semantics differences' (utf8::encode/decode).")
                ("^two warnings from the above$"
                 :warning-emit
                 "re-declaring a constant with `sub constant1;` must warn 'Constant subroutine redefined' twice -- PCL emits no warnings-gated diagnostic. not-supported.md: 'Warnings-gated diagnostics are absent'.")
                ("^RT124156 plain$"
                 :tie
                 "a tied-hash DELETE that dies while `local $a{foo}` unwinds must replace the inner die -- `tie %hash` is not implemented (announced, task #155), so nothing ties and the inner die stands. not-supported.md: 'tie on an ARRAY or HASH'.")
                ("^RT124156 depth$"
                 :tie
                 "same as 'RT124156 plain' one closure deeper -- `tie %hash` is not implemented (announced, task #155). not-supported.md: 'tie on an ARRAY or HASH'.")
                ("^RT124156 freed cv$"
                 :destroy-gc
                 "DESTROY must fire when the blessed coderef is freed during the unwind -- PCL never calls DESTROY via GC. not-supported.md: 'DESTROY called by garbage collector'.")
                ("^RT 124248$"
                 :destroy-gc
                 "a sub must FREETMPS on exit so `bless []` is DESTROYed before the caller's next statement -- PCL never calls DESTROY via GC. not-supported.md: 'DESTROY called by garbage collector'.")
                ("GH Issue #16944"
                 :principle9
                 "`<s,,$0[sub{m]]]],}>0,shift` must produce perl's 'syntax error' -- error detection of invalid Perl. not-supported.md: 'Error compatibility for invalid Perl input'.")
                ("6998 freeing array used as args to sub"
                 :xs
                 "the expected string is chosen by `Internals::stack_refcounted()`, and the assertion is about perl's refcount-based freed-value diagnostic -- C-level introspection PCL does not have. not-supported.md: 'Internals::* C-level introspection'."))

;; sort.t — the 32 inline `ok(1, "SKIP: …")` rows were RESTORED to upstream
;; (task #965, s464ay).  The file had been reported FULLY PASSING (204/205) on
;; the strength of those 32 manufactured rows; it is 182 pass / 22 fail now.
;; TEN of the 32 PASS and were hiding fixes nobody counted:
;;   - seven "wantarray regression in PCL sub body" rows (`sub sortr { reverse
;;     sort @_ }` in list context).  Probed: PCL and perl agree on all four
;;     spellings.  The regression named in CLAUDE.md design principle 8 does
;;     not reach these rows any more.
;;   - "sort subr from other package -- $a/$b package scoping not implemented".
;;   - two of the three Tie::Array EXTEND rows.
;; The claim in the file that `sort self_immolate` "crashes SBCL" is also
;; stale: PCL survives it and answers with a different message (row 149).
;; NOT registered, held back as fix targets:
;;   - 162/163/164 (RT34604 overloaded `cmp`) — sort's DEFAULT comparator
;;     ignores an overloaded `cmp` and compares stringifications instead
;;     (task #1021, a silent wrong); the rows additionally need `tie %h` (#155).
;;   - 174/175 (match vars leaking between comparator calls) — task #684,
;;     match variables are BLOCK-scoped in perl.  NB #684's own text says
;;     "specific frames (sort comparators)" are handled; these rows show they
;;     are not.
;;   - 26 ('redefinition should not take effect during the sort') — does NOT
;;     reproduce in isolation (probed both the bare and the full two-step
;;     redefinition sequence); an in-file interaction, unattributed.
(register-skips "sort.t"
                ("No elements were wrongly converted to utf8 in sorting"
                 :utf8
                 "sort must not upgrade its elements' UTF-8 flag -- PCL has no per-scalar UTF-8 flag (CL strings are always Unicode). not-supported.md: 'Unicode semantics differences'.")
                (22 :error-msg
                    "must die 'Undefined sort subroutine \"CORE::revers\" called' -- PCL dies with 'Undefined subroutine &main::revers' (the CORE:: qualification and the sort-specific wording are lost; the unqualified spelling is correct, probed). not-supported.md: 'Error message text and format'.")
                ("EXTEND was called with an argument of 3"
                 :tie
                 "pp_sort must call the tied array's EXTEND with the element count -- `tie @array` is not implemented (announced, task #155). not-supported.md: 'tie on an ARRAY or HASH'.")
                ("^goto subr outside subr$"
                 :principle9
                 "`goto &sub` outside a sub must die 'Can't goto subroutine outside a subroutine' -- PCL does not raise it. not-supported.md: 'Error compatibility for invalid Perl input'.")
                ("^goto subr from a sort sub$"
                 :principle9
                 "`goto &sub` from a sort comparator must die 'Can't goto subroutine from a sort sub' -- PCL does not raise it. not-supported.md: 'Error compatibility for invalid Perl input'.")
                ("^goto out of a pseudo block 1$"
                 :error-msg
                 "must die 'Can't \"goto\" out of a pseudo block' -- PCL dies with 'Can't find label label'. not-supported.md: 'Error message text and format'.")
                ("^goto out of a pseudo block 2$"
                 :error-msg
                 "must die 'Can't \"goto\" out of a pseudo block' -- PCL dies with 'Can't find label label'. not-supported.md: 'Error message text and format'.")
                ("^undef active subr$"
                 :principle9
                 "`undef &self` from inside a running sort comparator must die 'Can't undef active subroutine' -- PCL runs it and reports 'Undefined subroutine' instead (it no longer crashes SBCL, contrary to the note this replaced). not-supported.md: 'Error compatibility for invalid Perl input'.")
                ("^all gone$"
                 :destroy-gc
                 "every sorted object must be DESTROYed once the list is released -- PCL never calls DESTROY via GC. not-supported.md: 'DESTROY called by garbage collector'.")
                ("^sort block modifying \\$a and \\$b$"
                 :alias
                 "a comparator that ASSIGNS to $a/$b must write through to the element ($#a becomes 10) -- ruled: the comparator binds the element's box only when it has one, so a raw-stored element takes the write in a temporary. perl calls mutation during a sort undefined. not-supported.md: 'Writing to $a/$b inside a sort comparator'.")
                (197 :destroy-gc
                     "rebinding *a's scalar slot inside a comparator must DESTROY the displaced object at a defined moment -- PCL never calls DESTROY via GC. not-supported.md: 'DESTROY called by garbage collector'.")
                (198 :destroy-gc
                     "same for *b. not-supported.md: 'DESTROY called by garbage collector'.")
                ("^empty sort not allowed$"
                 :principle9
                 "bare `sort` with no arguments must be a compile-time 'Not enough arguments for sort' -- error detection of invalid Perl. not-supported.md: 'Error compatibility for invalid Perl input'.")
                ("^empty \\{sort\\} not allowed$"
                 :principle9
                 "`{sort}` must be 'Not enough arguments for sort' -- error detection of invalid Perl. not-supported.md: 'Error compatibility for invalid Perl input'.")
                ("^empty sort; not allowed$"
                 :principle9
                 "`sort;` must be 'Not enough arguments for sort' -- error detection of invalid Perl. not-supported.md: 'Error compatibility for invalid Perl input'.")
                ("^empty \\(sort\\); not allowed$"
                 :principle9
                 "`(sort);` must be 'Not enough arguments for sort' -- error detection of invalid Perl. not-supported.md: 'Error compatibility for invalid Perl input'."))

;; join.t / reset.t / loopctl.t / local.t / time.t — inline `ok(1,'SKIP…')`
;; rows RESTORED to upstream (task #965, s464ay).  All five files were
;; reported PASS or fully passing on manufactured rows; NINE of their skips
;; were STALE and now pass:
;;   time.t 1/1  — 'changes to $ENV{TZ} respected'.  The file's note said
;;     "PCL localtime does not call tzset()"; it does now, and time.t is
;;     still 72/72 with nothing manufactured.  (docs/not-supported.md
;;     'Runtime $ENV{TZ} changes not reflected in localtime' is therefore
;;     suspect — see the s464ay report.)
;;   join.t 3/5 — the two `$SIG{__WARN__}` lazy-argument rows and the
;;     'join(const,const) returns a new scalar each time' ref-identity row.
;;   loopctl.t 4/8 — 'bug 37725: reverse with empty slots' (the note claimed
;;     "foreach loop var aliasing not supported in PCL … PCL copies the value
;;     instead", which is FLATLY FALSE) and the three '[perl #21469] correct
;;     at tail of loop' typeglob-rebinding rows.
;;   reset.t 3/22.
;; NOT registered, held back as a fix target:
;;   loopctl.t 41 'dynamically scoped' — an UNLABELLED `last` inside a called
;;     sub does not reach the caller's loop (task #1022); the labelled twin
;;     row 40 passes.  In a `for` loop the exit is silently ignored, in a
;;     `while` loop it dies "attempt to GO to nonexistent tag: :next".
(register-skips "join.t"
                ("modifications delim from magic should be ignored"
                 :utf8
                 "compares fresh_perl output byte-for-byte after utf8::encode -- PCL's utf8::encode is a no-op (no per-scalar UTF-8 flag). not-supported.md: 'Unicode semantics differences'.")
                ("modifications to delim PVX shouldn't crash"
                 :utf8
                 "same byte-level comparison after utf8::encode. not-supported.md: 'Unicode semantics differences'."))

(register-skips "reset.t"
                ("match doesn't match second time"
                 :feature
                 "a one-match `m?pat?` must not match again until reset -- `?pattern?` is parsed as `/pattern/` and `reset` is not implemented. not-supported.md: 'reset() for one-match ?pattern? and named captures'.")
                ("match doesn't match third time"
                 :feature
                 "same one-match `m?pat?` semantics. not-supported.md: 'reset() for one-match ?pattern? and named captures'.")
                ("leaves patterns alone"
                 :feature
                 "`reset \"\"` must leave one-match patterns alone [perl #97958] -- reset is not implemented. not-supported.md: 'reset() for one-match ?pattern? and named captures'.")
                ("^reset \""
                 :feature
                 "`reset` with a character / character-range argument must undef the matching package variables -- not implemented. not-supported.md: 'reset() for one-match ?pattern? and named captures'.")
                ("cow, qr, vstring, glob, ro test"
                 :feature
                 "`reset` must undef COW/qr/vstring/glob-valued package scalars -- not implemented. not-supported.md: 'reset() for one-match ?pattern? and named captures'.")
                ("^resetting "
                 :feature
                 "`reset` must empty package arrays and hashes -- not implemented. not-supported.md: 'reset() for one-match ?pattern? and named captures'.")
                ("reset leaves real-globs-as-scalars as GLOBs"
                 :feature
                 "`reset` must leave a glob-valued slot a GLOB -- not implemented. not-supported.md: 'reset() for one-match ?pattern? and named captures'.")
                ("And the glob still has the right value"
                 :feature
                 "companion of the previous row. not-supported.md: 'reset() for one-match ?pattern? and named captures'.")
                ("magic tries to SvIV"
                 :feature
                 "`reset \"\\cW\"` must warn 'uninitialized' via $^W's set magic -- reset is not implemented. not-supported.md: 'reset() for one-match ?pattern? and named captures'.")
                ("check magic applied"
                 :feature
                 "`reset '|'` must zero $| through its set magic -- reset is not implemented. not-supported.md: 'reset() for one-match ?pattern? and named captures'.")
                ("isn't marked read-only, but throws on set magic"
                 :feature
                 "`reset \"1\"` must die 'Modification of a read-only value attempted' via $1's set magic -- reset is not implemented. not-supported.md: 'reset() for one-match ?pattern? and named captures'."))

(register-skips "loopctl.t"
                ("with non-constant label"
                 :principle9
                 "`last $label` / `next $label` / `redo $label` with a RUNTIME label must be rejected by perl (the row's fail() is only reached if it is not) -- error detection of invalid Perl. not-supported.md: 'Error compatibility for invalid Perl input'."))

(register-skips "local.t"
                (21 :principle9
                    "`eval 'local($$e)'` must set $@ to 'Can't localize through a reference' -- localizing through a reference is invalid Perl; PCL refuses the eval with its own message. not-supported.md: 'Error compatibility for invalid Perl input'.")
                (22 :principle9
                    "`eval '$e = []; local(@$e)'` -- as above. not-supported.md: 'Error compatibility for invalid Perl input'.")
                (23 :principle9
                    "`eval '$e = {}; local(%$e)'` -- as above. not-supported.md: 'Error compatibility for invalid Perl input'."))

;; kvhslice.t / splice.t — inline SKIP rows RESTORED (task #965, s464ay).
;; splice.t's two rows blamed a "wantarray regression gives :void ctx to
;; splice"; BOTH pass now and the file is 34/34 with nothing manufactured.
;; kvhslice.t's 25 rows blamed the same wantarray regression and "context
;; propagation through string eval"; SIX pass now (incl. both rows the
;; wantarray note named -- probed directly, no eval involved).  The file goes
;; OK -> PARTIAL: it declares 39 tests and PCL produces 38, a planned-minus-
;; produced hole that the manufactured rows had been filling.
;; NOT registered, held back as fix targets:
;;   9 / 11 — `%h{...}` in SCALAR context yields the element COUNT, not the
;;     last VALUE (task #1023).  The eval-context attribution in
;;     not-supported.md 'Context propagation into string eval' is WRONG for
;;     these rows: the divergence reproduces with no eval at all.
;;   28 / 29 — a BAREWORD key inside `%h{...}` is not autoquoted and is called
;;     as a sub (task #1024); `$h{i}`, `@h{i}` and `%h{'i'}` are all correct.
(register-skips "kvhslice.t"
                (10 :warning-emit
                    "`%h{...}` in scalar context must warn '%h{...} in scalar context better written as $h{...}' -- PCL emits no warnings-gated diagnostic. not-supported.md: 'Warnings-gated diagnostics are absent'.")
                (12 :warning-emit
                    "same warning for the single-key spelling. not-supported.md: 'Warnings-gated diagnostics are absent'.")
                (21 :lvalue
                    "`sub foo:lvalue { %h{qw(a b)} }; $_++ foreach foo()` must write through to %h -- user-defined lvalue subs are not implemented. not-supported.md: 'Lvalue subroutines'.")
                ("^local dies$"
                 :principle9
                 "`local %h{...}` must die 'Can't modify key/value hash slice in local' -- error detection of invalid Perl. not-supported.md: 'Error compatibility for invalid Perl input'.")
                ("^assign dies$"
                 :principle9
                 "assigning to a kv-slice must die 'Can't modify key/value hash slice in list assignment' -- error detection of invalid Perl. not-supported.md: 'Error compatibility for invalid Perl input'.")
                ("not allowed as result of lvalue sub"
                 :lvalue
                 "a kv-slice returned from an lvalue sub must be rejected -- user-defined lvalue subs are not implemented. not-supported.md: 'Lvalue subroutines'.")
                ("^warning in scalar context$"
                 :warning-emit
                 "must warn once in scalar context -- PCL emits no warnings-gated diagnostic. not-supported.md: 'Warnings-gated diagnostics are absent'.")
                ("^correct warning text$"
                 :warning-emit
                 "the scalar-context warning's text -- PCL emits no warnings-gated diagnostic. not-supported.md: 'Warnings-gated diagnostics are absent'.")
                ("hash reference is error"
                 :principle9
                 "a kv-slice used as a hash ref must die 'Can't use a hash as a reference' -- error detection of invalid Perl. not-supported.md: 'Error compatibility for invalid Perl input'.")
                ("hash slice reference is error"
                 :principle9
                 "as above for the slice spelling. not-supported.md: 'Error compatibility for invalid Perl input'.")
                ("keys %hash\\{key\\} forbidden"
                 :principle9
                 "`keys %h{k}` must die 'Experimental keys on scalar is now forbidden' -- error detection of invalid Perl. not-supported.md: 'Error compatibility for invalid Perl input'.")
                ("values %hash\\{key\\} forbidden"
                 :principle9
                 "`values %h{k}` must die 'Experimental values on scalar is now forbidden'. not-supported.md: 'Error compatibility for invalid Perl input'.")
                ("each %hash\\{key\\} forbidden"
                 :principle9
                 "`each %h{k}` must die 'Experimental each on scalar is now forbidden'. not-supported.md: 'Error compatibility for invalid Perl input'.")
                ("prototype"
                 :principle9
                 "a kv-slice passed to a hash-ref prototype must die 'Type of arg 1 ... must be hash (not key/value hash slice)' -- error detection of invalid Perl. not-supported.md: 'Error compatibility for invalid Perl input'."))
