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
                ;; ref()/stringify of a ref to a substr/pos/vec lvalue → "LVALUE".
                ("ref to (substr|pos|vec) lvalue"
                 :lvalue
                 "ref() of a \\substr/\\pos/\\vec lvalue must be 'LVALUE' — lvalue refs not supported. not-supported.md: 'Lvalue subroutines'.")
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
