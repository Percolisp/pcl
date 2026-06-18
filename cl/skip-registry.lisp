;;;; Copyright (c) 2025-2026
;;;; This is free software; you can redistribute it and/or modify it
;;;; under the same terms as the Perl 5 programming language system itself.

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

;; undef.t 16-18 are UNNAMED (no description) -> keyed by test number.
;; not-supported.md: 'Read-only constants via \undef stash tricks' (tests 16-18).
(register-skips "undef.t"
                (16 :read-only
                    "undef of a read-only value must die 'Modification of a read-only value' — read-only scalars not emulated. not-supported.md: 'Read-only constants via \\undef stash tricks' (undef.t 16-18).")
                (17 :read-only
                    "undef of a read-only value must die 'Modification of a read-only value' — read-only scalars not emulated. not-supported.md: 'Read-only constants via \\undef stash tricks' (undef.t 16-18).")
                (18 :read-only
                    "undef &constant_sub must die 'Can't modify constant item' — constant/read-only slots not emulated. not-supported.md: 'Read-only constants via \\undef stash tricks' (undef.t 16-18)."))

(register-skips "unshift.t"
                ("croak when unshifting onto readonly array"
                 :read-only
                 "unshift onto an Internals::SvREADONLY array must die 'Modification of a read-only value' — read-only arrays not marked. not-supported.md: 'Internals::* C-level introspection'."))

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
                 "result aliasing (\\$a[0] == \\$b after `chomp @a`, the $/ eq 0 case) — element/@_ aliasing not emulated. not-supported.md: '@_ argument aliasing'. (The eq 7 sibling legitimately passes.)"))

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
                 "modifying $_[0] (a scalar-substr lvalue) inside a sub must write back through @_ — @_ elements are copies in PCL. not-supported.md: '@_ argument aliasing'.")
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
                ;; 4-arg substr replacement on a tied (Tie::StdScalar) magical value.
                (142 :tie
                     "substr($tied,0,5,'') must STORE back through the tie so the tied value becomes 'last' — 4-arg substr does not write through tie magic. not-supported.md: 'DESTROY/tie magic' (tie write-through).")
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

;; push.t — push onto an Internals::SvREADONLY array must croak "Modification of
;; a read-only value".  PCL emulates neither Internals::SvREADONLY nor a per-array
;; read-only flag, so the push succeeds and $@ stays empty.  This is the file's
;; only failure.  not-supported.md: 'Internals::* C-level introspection' /
;; 'Read-only constants'.  (The sibling "can push empty list onto readonly array"
;; legitimately passes — a no-op push raises nothing — and is excluded by the regex.)
(register-skips "push.t"
                ("croak when pushing onto readonly array"
                 :read-only
                 "push onto an Internals::SvREADONLY array must die 'Modification of a read-only value' -- read-only arrays / Internals::* not emulated. not-supported.md: 'Internals::* C-level introspection'."))

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
                ;; &PL_sv_undef / per-element SV identity — CL box model has neither.
                ("exists returns true for &PL_sv_undef elem"
                 :alias
                 "exists of the shared &PL_sv_undef SV in an array — CL box model has no shared-undef SV. not-supported.md: 'Sparse arrays (holes), element aliasing, and SV identity'.")
                ("undef preserves identity in array"
                 :alias
                 "\\$_[0] aliased to an undef element must keep SV identity (\\$_[0] == \\undef) — box model copies. not-supported.md: 'Sparse arrays (holes), element aliasing, and SV identity'.")
                ;; @_ aliasing to nonexistent (sparse) elements.
                ("\\@_ alias to nonexistent"
                 :alias
                 "writing through an @_ alias to a nonexistent array element must autovivify the caller's element — @_ aliasing not emulated. not-supported.md: '@_ argument aliasing'.")
                ;; Sparse-array holes preserved through subs / reads.
                ("holes passed to sub do not lose their position"
                 :alias
                 "array holes (nonexistent elements) passed to a sub must keep their position — PCL has no defelem/hole. not-supported.md: 'Sparse arrays (holes), element aliasing, and SV identity'.")
                ("non-elems read from \\@a do not lose their position"
                 :alias
                 "reading a hole must not vivify or shift later elements — sparse arrays not emulated. not-supported.md: 'Sparse arrays (holes), element aliasing, and SV identity'. (The 'magical @a' sibling is excluded.)")
                ;; Lazy element creation / map-no-vivify — these share descriptions
                ;; with PASSING siblings ('copying an array via =', extra refgen/map
                ;; assertions), so keyed by current TAP number, not regex.
                (174 :alias
                     "lazy element creation via sub call must autovivify an array hole — sparse arrays/defelem not emulated. not-supported.md: 'Sparse arrays (holes), element aliasing, and SV identity'.")
                (176 :alias
                     "lazy element creation via refgen (\\$q[$_] address identity) — box model. not-supported.md: 'Sparse arrays (holes), element aliasing, and SV identity'.")
                (179 :alias
                     "lazy element creation via foreach alias — sparse arrays/defelem. not-supported.md: 'Sparse arrays (holes), element aliasing, and SV identity'.")
                (181 :alias
                     "map {} over @a must not vivify holes — sparse arrays not emulated. not-supported.md: 'Sparse arrays (holes), element aliasing, and SV identity'.")
                (184 :alias
                     "map {} over a magical @a must not vivify holes — sparse arrays not emulated. not-supported.md: 'Sparse arrays (holes), element aliasing, and SV identity'."))

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
