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
;;;; :warning-emit, :read-only, :utf8, :destroy-gc, :lvalue, :alias, :tie.

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
