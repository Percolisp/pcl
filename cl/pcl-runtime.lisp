;;;; Copyright (c) 2025-2026 the PCL authors
;;;; This is free software; you can redistribute it and/or modify it under the
;;;; same terms as the Perl 5 programming language system itself.
;;;; SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

;;;; PCL Runtime - Common Lisp runtime for Perl to CL transpiler
;;;; Requires SBCL (Steel Bank Common Lisp)
;;;;
;;; PCL requires SBCL >= 2.5.2: the test suite is validated on 2.5.2/2.6.0
;;; and the runtime uses SBCL-internal symbols (sb-unicode, sb-kernel float
;;; bits, …).  This check is deliberately the FIRST code to execute, so on
;;; an old host the load fails right under a message explaining why, not
;;; with a cryptic missing-symbol/missing-component error later
;;; (docs/error-pcre.txt: Debian's SBCL 2.1.11).  A WARNING, not an error
;;; (and written directly to *error-output* — the pcl runner load-wraps
;;; with muffle-warning, which must not eat it).
(let* ((min-version '(2 5 2))
       (v (lisp-implementation-version))
       (nums (loop with start = 0
                   while (< start (length v))
                   for dot = (position #\. v :start start)
                   for num = (parse-integer v :start start :end dot
                                            :junk-allowed t)
                   while num
                   collect num
                   do (setf start (if dot (1+ dot) (length v))))))
  (when (loop for a in nums
              for b in min-version
              do (cond ((< a b) (return t))
                       ((> a b) (return nil)))
              finally (return (< (length nums) (length min-version))))
    (format *error-output*
            "~&WARNING: PCL requires SBCL >= 2.5.2; this is SBCL ~a.~%~
             WARNING: Loading will very likely fail below this message.~%~
             WARNING: Debian 13+ / Ubuntu 25.10+ package a new enough SBCL;~%~
             WARNING: otherwise install the binary from https://www.sbcl.org/~%"
            v)
    (force-output *error-output*)))

;;; Load CL-PPCRE for regex support
(require :asdf)
(handler-case (asdf:load-system :cl-ppcre :silent t)
  (error (e)
    (error "PCL: ASDF could not load cl-ppcre (~a).~%~
            If cl-ppcre was installed via Quicklisp, note that `sbcl --script` ~
            skips ~~/.sbclrc, so Quicklisp never registers with ASDF in that ~
            mode.  Run via `sbcl --noinform --non-interactive --load ~
            cl/pcl-runtime.lisp --load FILE` (what ./runpcl does) instead of ~
            --script, or make cl-ppcre visible to plain ASDF." e)))

;;; Load sb-posix for process ID
(require :sb-posix)

;;; Load sb-bsd-sockets for socket builtins (socket/bind/connect/accept/…)
(require :sb-bsd-sockets)

;;; --- :invert spike (case-sensitivity experiment) -----------------------
;;; All third-party libs (cl-ppcre, asdf, sb-posix) are loaded ABOVE under the
;;; standard :upcase readtable.  From here on, read PCL's own runtime AND all
;;; subsequently-loaded generated code under :invert so that Perl identifiers
;;; differing only in case map to distinct CL symbols.  Lower-case tokens
;;; (defun, let, p-box) invert to UPPER (standard CL); all-UPPER Perl names
;;; (STDERR, BASE_LEN) invert to lower; mixed-case (pl-FOO, Foo::Bar) preserved.
(setf (readtable-case *readtable*) :invert)
;;; ----------------------------------------------------------------------

;;; --- Floating-point model: set ONCE at startup ------------------------
;;; Perl's float semantics: overflow -> Inf, invalid -> NaN, silently.
;;; Mask those traps once here instead of per operation (the old
;;; %pcl-ieee-arith wrapped every arithmetic op in a heap-allocated closure
;;; plus with-float-traps-masked -- measured 7.4x slower on arithmetic).
;;; :divide-by-zero stays trapping: Perl dies on 1/0 and 1/0.0 alike.
(sb-int:set-floating-point-modes :traps '(:divide-by-zero))
;;; A core saved with sb-ext:save-lisp-and-die (standalone executables)
;;; resets FP modes at startup -- re-apply them when such a core boots.
(push (lambda () (sb-int:set-floating-point-modes :traps '(:divide-by-zero)))
      sb-ext:*init-hooks*)

;;; --- Inline policy: stored now, enabled at end of file ------------------
;;; Hot operators/accessors below carry (declaim (inline f)) BEFORE their
;;; defun (SBCL stores the inline expansion only when the proclamation is in
;;; effect at definition time) and (declaim (notinline f)) right AFTER it.
;;; The runtime's own thousands of call sites therefore compile as plain
;;; calls (keeps this file's load/compile time low — every test/process
;;; spawn recompiles it), while the end of this file re-proclaims them
;;; INLINE so all GENERATED USER CODE compiled afterwards gets the numberp/
;;; stringp fast paths open-coded at its call sites.
;;; The global optimize policy for generated code lives at the end of the
;;; file for the same reason.
;;; ----------------------------------------------------------------------

(defpackage :pcl
  (:use :cl)
  (:export
   ;; TAP layer (cl/pcl-test.lisp, loaded ON DEMAND by p-ensure-test-lib).
   ;; The NAMES are exported eagerly so user packages inherit pcl::pl-ok etc.
   ;; from the start: v2 hoists definition-bucket forms (anon-block defuns)
   ;; ABOVE the runtime `use Test::More` form, so a pl-diag reference can be
   ;; READ before the test lib loads — without the eager export that interns
   ;; a distinct main::pl-diag and the test lib's own (export …) then dies
   ;; with SB-EXT:NAME-CONFLICT (and the compiled call would name the wrong
   ;; symbol anyway).  Definitions still load lazily; fboundp is untouched.
   #:pl-plan #:pl-done_testing #:pl-ok #:pl-is #:pl-isnt
   #:pl-like #:pl-unlike #:pl-cmp_ok #:pl-pass #:pl-fail
   #:pl-skip #:pl-skip_all #:pl-diag #:pl-note #:pl-BAIL_OUT
   #:pl-eq_array #:pl-curr_test
   #:pl-is_deeply #:pl-use_ok #:pl-require_ok #:pl-isa_ok #:pl-can_ok
   #:pl-explain #:pl-locales_enabled #:pl-_diag
   ;; Value boxing
   #:p-box #:make-p-box #:p-box-p #:p-box-value
   #:unbox #:ensure-boxed #:p-copy-scalar-arg
   #:box-set #:box-nv #:box-sv  ; lazy caching accessors
   #:to-string #:to-number
   #:%pcl-to-number-strict #:%pcl-to-string-strict #:%pcl-dualvar-p
   #:%pcl-str-buffer #:%pcl-str-append
   #:p-undef #:p-defined #:p-defined-fh #:%pcl-definedp #:p-true-p
   #:p-let #:p-$
   ;; Arithmetic
   #:p-+ #:p-- #:p-* #:p-/ #:p-% #:p-** #:p-int #:p-abs #:p-double-inf
   ;; Math
   #:p-sin #:p-cos #:p-atan2 #:p-exp #:p-log #:p-sqrt #:p-rand #:p-srand
   ;; String
   #:p-. #:p-str-x #:p-list-x #:p-length #:p-substr #:p-lc #:p-uc #:p-fc #:p-quotemeta
   #:p-chomp #:p-chop #:p-index #:p-rindex #:p-string-concat
   #:p-chr #:p-ord #:p-hex #:p-oct #:p-lcfirst #:p-ucfirst #:p-sprintf #:p-printf #:p-crypt
   #:p-version-string
   #:p-pos
   #:p-unrepresentable-char
   ;; Assignment
   #:p-setf #:p-incf #:p-decf
   #:p-pre++ #:p-post++ #:p-pre-- #:p-post--
   ;; Compound assignment
   #:p-*= #:p-/= #:p-%= #:p-**=
   #:p-.= #:p-str-x=
   #:p-bit-and= #:p-bit-or= #:p-bit-xor= #:p-<<= #:p->>=
   #:p-str-bit-and= #:p-str-bit-or= #:p-str-bit-xor=
   #:p-and-assign #:p-or-assign #:p-//=
   ;; Raw twins: compound assignment on a raw let-bound lexical slot
   #:p-incf-raw #:p-decf-raw
   #:p-*=-raw #:p-/=-raw #:p-%=-raw #:p-**=-raw
   #:p-.=-raw #:p-str-x=-raw
   #:p-bit-and=-raw #:p-bit-or=-raw #:p-bit-xor=-raw #:p-<<=-raw #:p->>=-raw
   #:p-str-bit-and=-raw #:p-str-bit-or=-raw #:p-str-bit-xor=-raw
   ;; Comparison (numeric)
   #:p-== #:p-!= #:p-< #:p-> #:p-<= #:p->= #:p-<=>
   ;; Comparison (string)
   #:p-str-eq #:p-str-ne #:p-str-lt #:p-str-gt #:p-str-le #:p-str-ge #:p-str-cmp
   ;; Chained comparison
   #:p-chain-cmp
   ;; Range operator
   #:p-.. #:p-...
   ;; Flip-flop operator (scalar context .. and ...)
   #:p-flipflop #:p-flipflop-3 #:p-flipflop-num #:p-flipflop-num-3
   #:p-flipflop-dyn #:p-flipflop-dyn-3
   ;; Dualvar
   #:p-dualvar
   ;; Logical
   #:p-&& #:p-|| #:p-! #:p-not #:p-and #:p-or #:p-xor #:p-//
   ;; Bitwise
   #:p-bit-and #:p-bit-or #:p-bit-xor #:p-bit-not #:p-<< #:p->>
   #:p-str-bit-and #:p-str-bit-or #:p-str-bit-xor #:p-str-bit-not
   #:p-to-s64 #:p-<<-int #:p->>-int
   ;; Data structures
   #:p-aref #:p-aref-box #:p-aref-argbox #:p-gethash-argbox #:p-aref-deref #:p-aref-deref-box #:p-gethash #:p-gethash-box #:p-gethash-deref #:p-gethash-deref-box
   #:p-ensure-hashref #:p-ensure-arrayref
   #:p-aslice #:p-hslice #:p-kv-hslice #:p-kv-aslice #:p-list-scalar #:p-slice-result
   #:p-hash #:p-array-init #:p-array-last-index #:p-set-array-length
   #:p-push #:p-pop #:p-shift #:p-unshift #:p-splice #:p-flatten #:p-flatten-args
   #:p-check-arity #:p-sig-rest-array #:p-sig-rest-hash
   #:p-keys #:p-values #:p-each #:p-exists #:p-exists-array #:p-delete #:p-delete-array
   #:p-delete-hash-slice #:p-delete-kv-hash-slice #:p-delete-array-slice #:p-delete-kv-array-slice
   ;; Control flow
   #:p-if #:p-unless #:p-while #:p-until #:p-do-while #:p-do-until #:p-for #:p-foreach #:p-foreach-range #:p-foreach-range-raw
   #:p-return #:p-goto-sub #:p-goto-computed #:p-last #:p-last-dynamic #:p-next #:p-redo
   #:p-continue #:p-break
   ;; I/O
   #:p-print #:p-say #:p-warn #:p-die
   ;; do BLOCK
   #:p-do
   ;; Exception handling
   #:p-eval #:p-eval-block #:p-eval-thunk #:p-eval-lex-lookup #:p-try
   #:p-alias-eval-cell
   #:*p-eval-lex-alist*
   #:p-exception #:p-exception-object
   ;; File I/O
   #:p-open #:p-close #:p-eof #:p-tell #:p-seek #:p-sysseek #:p-pipe #:p-select #:p-write
   #:p-binmode #:p-read #:p-sysread #:p-syswrite
   ;; Socket builtins
   #:p-socket #:p-socketpair #:p-bind #:p-connect #:p-listen #:p-accept
   #:p-send #:p-recv #:p-shutdown #:p-getsockname #:p-getpeername
   #:p-getprotobyname #:p-getprotobynumber #:p-setsockopt #:p-getsockopt
   #:p-truncate #:p-stat #:p-lstat
   ;; File test operators
   #:p--e #:p--d #:p--f #:p--r #:p--w #:p--x #:p--s #:p--z
   #:p--l #:p--p #:p--S #:p--b #:p--c #:p--u #:p--g #:p--k
   #:p--o #:p--O #:p--R #:p--W #:p--X #:p--M #:p--A #:p--C
   #:p--T #:p--B #:p--t
   ;; `_` — perl's stat-cache filehandle (`-e $f and -f _`).  A bare CL symbol,
   ;; deliberately: that is exactly what the emitter produces for the bareword.
   #:_ #:*pcl-stat-cache-path*
   #:p-unlink #:p-lock #:p-fileno #:p-getc #:p-readline #:*p-filehandles*
   ;; Directory I/O
   #:p-opendir #:p-readdir #:p-closedir #:p-rewinddir
   ;; File glob
   #:p-glob
   ;; File/Directory operations
   #:p-chdir #:p-set_up_inc #:p-mkdir #:p-rmdir #:p-getcwd #:p-cwd #:p-rename #:p-chmod
   #:p-umask #:p-link #:p-symlink #:p-readlink #:p-chown #:p-utime
   ;; Time functions
   #:p-time #:p-times #:p-sleep #:p-alarm #:p-evalbytes #:p-study #:p-reset #:p-vec #:p-vec-set #:p-localtime #:p-gmtime
   ;; Process control
   #:p-exit #:p-system #:p-fork #:p-waitpid #:p-wait #:p-getppid #:p-kill #:p-exec
   #:p-getpgrp #:p-setpgrp #:p-getpriority
   #:p-backtick #:p-errno-string #:p-stash
   ;; Group/passwd database
   #:p-getgrent #:p-setgrent #:p-endgrent #:p-getgrgid #:p-getgrnam
   #:p-getpwent #:p-setpwent #:p-endpwent #:p-getpwuid #:p-getpwnam #:p-getlogin
   ;; Environment
   #:%ENV #:p-env-get #:p-env-set
   ;; Module system
   #:@INC #:%INC #:%SIG #:@ARGV #:$ARGV #:@_ #:%_args #:p-use #:p-require #:p-require-parent #:p-require-file #:p-require-version
   ;; Functions
   ;; Reference aliasing (use feature 'refaliasing'): p-setf's \-cast place
   #:p-alias-scalar-target #:p-alias-array-target #:p-alias-hash-target
   #:p-alias-code-target #:p-alias-hash-slot #:p-alias-array-slot
   #:p-alias-array-elements
   #:p-backslash #:p-backslash-sub #:p-backslash-list #:p-arylen-ref #:p-substr-ref #:p-pos-ref #:p-vec-ref #:p-substr-lvalue-cell #:p-pos-lvalue-cell #:p-vec-lvalue-cell #:p-refgen-list #:p-box-for-local #:p-get-coderef #:p-ref #:p-reftype #:p-scalar #:p-wantarray #:p-caller #:p-prototype #:p-__pcl_set_prototype
   ;; Typeglob support
   #:p-typeglob #:p-typeglob-p #:make-p-typeglob
   #:p-typeglob-package #:p-typeglob-name
   #:p-make-typeglob #:p-glob-assign #:p-glob-assign-dynamic
   #:p-dynamic-typeglob #:p-glob-copy
   #:p-glob-slot #:p-glob-undef-name #:p-local-glob #:p-local-glob-if #:p-local-dot
   #:p-defcell #:p-local-cell
   #:p-local-pipe
   #:p-local-hash-elem #:p-local-array-elem
   #:p-local-hash-elem-init #:p-local-array-elem-init
   #:p-local-array-slice
   #:p-local-deref-scalar #:p-local-deref-array #:p-local-deref-hash
   #:p-copy-array #:p-copy-hash
   #:p-pack #:p-unpack #:p-load-extension
   #:p-grep #:p-map #:p-sort #:p-sort-get-fn #:p-reverse
   #:p-join #:p-split #:p-funcall-ref
   ;; Dereferencing (sigil cast operations)
   #:p-cast-@ #:p-cast-% #:p-cast-$
   #:p-hash-deref-= #:p-array-deref-=
   ;; OO
   #:p-bless #:p-get-class #:p-method-call #:p-resolve-invocant
   #:p-super-call #:perl-pkg-to-clos-class
   #:p-can #:p-isa
   ;; use overload — operator overloading registry
   #:*p-overload-table* #:p-register-overloads
   #:p-find-overload #:p-call-overload
   #:p-overload-strval #:p-overloaded
   ;; Regex
   #:p-=~ #:p-!~ #:p-subst #:p-tr #:p-regex #:p-regex-from-parts
   ;; Capture groups
   #:$_ #:$1 #:$2 #:$3 #:$4 #:$5 #:$6 #:$7 #:$8 #:$9 #:%+
   #:$10 #:$11 #:$12 #:$13 #:$14 #:$15 #:$16 #:$17 #:$18 #:$19 #:$20
   #:|$&| #:|$`| #:|$'| #:|$+| #:|$^N| #:|@-| #:|@+| #:|%-| #:|$^H| #:|%^H|
   #:|@{^CAPTURE}|
   ;; Special variables
   #:$$ #:$? #:|$.| #:$0 #:$@ #:|$^O| #:|$^V| #:|$^X| #:|$^T| #:|$^H| #:|%^H| #:|${^TAINT}| #:|$/| #:|$\\| #:|$"| #:|$\|| #:|$;| #:|$,| #:|$]| #:|$<| #:|$>| #:|$(| #:|$)|
   #:|$~| #:|$=| #:|$-| #:|$%| #:|$:| #:|$^L| #:|$^A| #:|$^| #:|$^R| #:|$^S| #:|$^P| #:|$^D| #:|$^F| #:|$^I| #:|$^M| #:|$^W| #:|$[|
   ;; Context — the variable and the four macros that name its bindings (#281)
   #:*wantarray*
   #:p-list-ctx #:p-scalar-ctx #:p-void-ctx #:p-caller-ctx
   #:p-sort-cmp
   #:*pcl-caller-wantarray*
   #:*p-in-list-assign-rhs*
   ;; Call depth tracking (for p-caller at top level)
   #:*pcl-sub-call-depth*
   ;; Current/caller package tracking (for caller() package, __PACKAGE__-at-runtime)
   #:*pcl-current-package* #:*pcl-caller-pkg-stack* #:*pcl-caller-subname-stack*
   #:p-set-current-package #:p-register-pkg-name
   ;; END blocks
   #:*end-blocks* #:*unitcheck-blocks* #:*check-blocks* #:*init-blocks*
   #:p-run-compile-phase-blocks
   ;; Subroutine reflection (exists &sub, defined &sub, undef &sub)
   #:p-sub-exists #:p-sub-defined #:p-undef-sub
   #:p-coderef-exists-p #:p-coderef-defined-p
   ;; Tie/untie/tied
   #:p-tie-proxy #:make-p-tie-proxy #:p-tie-proxy-p
   #:p-tie-proxy-tie-obj #:p-tie-proxy-saved-value
   #:p-tie #:p-untie #:p-tied
   #:p-weaken #:p-isweak
   #:pl-__SUB__                         ; CORE::__SUB__ stub (returns no-op lambda)
   ;; Compile-time definition macros (for BEGIN block support)
   #:p-defpackage #:p-sub #:p-args-body #:p-raw-params #:p-declare-sub
   ;; eval-when wrappers (named for readability in generated CL)
   #:p-eval-always #:p-BEGIN #:p-CHECK
   ;; Assignment forms (distinct from p-setf for clarity)
   #:p-scalar-= #:p-array-= #:p-hash-= #:p-list-= #:p-array-fill #:p-hash-fill
   ;; Lexical 'my' variable assignment (no auto-declare side-effect)
   #:p-my-= #:p-box-init))

(in-package :pcl)

;;; Forward declarations to suppress compile-time "undefined function" style-warnings.
;;; These functions are defined later in this file; the declarations tell the compiler
;;; they exist so that earlier functions that call them compile without noise.
(declaim (ftype (function * *) %make-p-box p-box-p))
(declaim (ftype (function * *) (setf p-box-nv) (setf p-box-nv-ok)
                (setf p-box-sv) (setf p-box-sv-ok)))
(declaim (ftype (function * *) p-superchar-p))
(declaim (ftype (function * *)
                %pcl-nan-p
                p-ensure-hashref p-ensure-arrayref
                p-warn p-die
                p-glob--list-context p-glob--scalar-context
                %p-symref-array
                p-scalar
                %pcl-find-package %pcl-dispatch-autoload
                %pcl-set-autoload-var
                p-super-call
                p-load-extension))
(declaim (special *p-filehandles* *p-dirhandles*))

;;; Capture the runtime's directory at load time so extensions can be found.
;;; Must be near the top — *load-truename* changes as nested loads execute.
(defvar *pcl-runtime-directory*
  (when *load-truename*
    (make-pathname :name nil :type nil :defaults *load-truename*)))

;;; ============================================================
;;; Compile-Time Definition Macros
;;; ============================================================
;;; These macros wrap definitions in eval-when to make them available
;;; at compile time. This matches Perl's semantics where subs and
;;; package variables are defined as they are parsed, allowing BEGIN
;;; blocks to call subs defined before them in source order.

;;; Tracks how many PCL user subs deep we are (0 = top level).
;;; Used by p-caller to distinguish "called from a sub" vs "top level".
(defvar *pcl-sub-call-depth* 0)

;;; Original-case Perl package name of the lexically-current code.  PCL upcases
;;; single-segment package names into CL packages (Foo -> CL package "FOO"), so
;;; the CL package object cannot recover the Perl case.  This dynamic variable
;;; carries the original case: codegen sets it at each `package` statement, and
;;; p-sub rebinds it per call to the sub's own package.  Read by p-caller.
(defvar *pcl-current-package* "main")

;;; Stack of caller packages.  At each p-sub entry the caller's
;;; *pcl-current-package* is pushed; p-caller(N) reads (nth N ...) for the
;;; package from which the Nth frame's sub was called.
(defvar *pcl-caller-pkg-stack* nil)

;;; Parallel stack of the entered sub's fully-qualified Perl name ("Pkg::name").
;;; p-caller(N) reads (nth N ...) for (caller(N))[3].  SBCL can't name our subs —
;;; they are anonymous lambdas installed via (setf (symbol-function ...)) — so the
;;; name is recorded here at p-sub entry instead of recovered from the backtrace.
(defvar *pcl-caller-subname-stack* nil)

;;; Maps a CL package-name string (e.g. "FOO") to the original-case Perl name
;;; (e.g. "Foo").  Populated by p-set-current-package as `package` statements run.
(defvar *pcl-pkg-name-map* (make-hash-table :test 'equal))

(defun p-register-pkg-name (pkg perl-name)
  "Record the original-case PERL-NAME for CL package PKG in *pcl-pkg-name-map*
   WITHOUT changing the lexically-current package.  Emitted in each package's
   preamble (before its `use` statements) so that caller()/__PACKAGE__ inside an
   imported module's import() resolve the use-site package to its original case
   — p-set-current-package runs only in execution order, which is AFTER the use
   statements, too late for the name-map lookup during import."
  (let ((p (ignore-errors (find-package pkg))))
    (when p
      (setf (gethash (package-name p) *pcl-pkg-name-map*) perl-name)))
  perl-name)

(defun p-set-current-package (pkg perl-name)
  "Record the original-case PERL-NAME for CL package PKG (a package designator
   as emitted by codegen) and make it the lexically-current package.  Called by
   generated code at each `package` statement."
  (let ((p (ignore-errors (find-package pkg))))
    (when p
      (setf (gethash (package-name p) *pcl-pkg-name-map*) perl-name)))
  (setf *pcl-current-package* perl-name))

(defun pcl-pkg-perl-name (cl-pkg)
  "Best-effort original-case Perl name for a CL package object.  Uses the
   *pcl-pkg-name-map* registry; falls back to the CL name (with MAIN -> main)."
  (let ((n (and cl-pkg (package-name cl-pkg))))
    (or (and n (gethash n *pcl-pkg-name-map*))
        (cond ((null n) "main")
              ((string= n "MAIN") "main")
              (t n)))))

(defun %p-sub-perl-name (name)
  "Fully-qualified Perl name 'Pkg::subname' for a PCL sub symbol (Pkg::PL-SUBNAME).
   Strips the runtime's PL- prefix and uses the package's original-case Perl name.
   Recorded on *pcl-caller-subname-stack* at p-sub entry for (caller(N))[3]."
  (if (and name (symbolp name))
      (let* ((sname (symbol-name name))
             ;; Reverse the :invert reader transform to recover the emitted
             ;; `pl-<perlname>` token (invert is its own inverse), then strip the
             ;; now-lowercase prefix.  Recovers original sub-name case, e.g.
             ;; PL-BAR -> bar, pl-Foo -> Foo, pl-FOO -> FOO.
             (inv   (%pcl-invert-case sname))
             (bare  (if (and (>= (length inv) 3)
                             (string= (subseq inv 0 3) "pl-"))
                        (subseq inv 3)
                        sname)))
        (concatenate 'string (pcl-pkg-perl-name (symbol-package name)) "::" bare))
      (and name (format nil "~A" name))))

;;; ── Global storage: cells vs specials (direction D, task #289) ───────────
;;; An ORDINARY package global is a symbol macro over its own global value
;;; cell (`p-defcell`); the exception set ($_, $a/$b, punctuation magic, …)
;;; is a `defvar` special.  Everywhere the runtime VIVIFIES a variable it
;;; reached by NAME — symbolic refs, glob assignment, a write to a package
;;; var nothing declared — it used to proclaim the symbol special first.
;;; That proclamation is an ERROR on a symbol macro, and it is unnecessary:
;;; symbol-value/boundp read and write the same global cell either way.  So
;;; every such site asks this one function instead of proclaiming directly.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %p-ensure-storage (sym)
    "Give SYM global storage the COMPILER will agree with, and return SYM.

     A name the compiler has not declared can still be created at run time —
     by a symbolic ref (`@{\"${class}::ISA\"}`, the Exporter/base idiom), by a
     glob assignment, by a write to a package variable no declaration
     mentioned.  Whatever this makes it, a later `p-defcell` for the SAME name
     — from another file, or this file's own declarations — has to agree, or
     SBCL refuses the second one.  So vivify it as the ORDINARY shape (a
     symbol macro over its own value cell), exactly what p-defcell emits.

     Already-special names take the other branch: those are the runtime's own
     magic variables, which are `defvar`'d before any of this runs, and the
     partition's exception set is precisely them.  (`Foo::$a` in a package
     whose module has not loaded yet is the one name that could be vivified
     as a cell here and then meet a `defvar` — it dies at load, loudly, and
     has never been observed: reaching `$a` by symbolic ref is not an idiom.)

     The eval is unavoidable — define-symbol-macro is a macro over a LITERAL
     symbol, and these symbols are computed — but it runs at most once per
     name, on paths that already intern and look up packages."
    (case (sb-int:info :variable :kind sym)
      ((:macro :special :constant :global) sym)
      (t (eval `(define-symbol-macro ,sym (sb-ext:symbol-global-value ',sym)))
         sym))))

;;; p-defpackage: Create/update a Perl package namespace.
;;; Wraps defpackage in eval-when so it runs at compile time (needed so that
;;; subsequent in-package forms can find the package during compile-file), and
;;; in handler-bind to suppress SBCL's "package at variance" warnings that fire
;;; when p-sub's compile-time shadow calls have already added symbols to the
;;; shadow list before defpackage re-evaluates at load time.
(defmacro p-defpackage (name &rest options)
  "Create/update a Perl package. Defaults to (:use :cl :pcl) when no options given.
   Also ensures @ISA is declared in the package (all Perl packages have @ISA)."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (handler-bind ((warning #'muffle-warning))
       (defpackage ,name ,@(or options '((:use :cl :pcl)))))
     (let* ((pkg (find-package ,(string name)))
            (isa-sym (when pkg (intern "@isa" pkg))))
       (when (and isa-sym (not (boundp isa-sym)))
         (%p-ensure-storage isa-sym)
         (setf (symbol-value isa-sym)
               (make-array 0 :adjustable t :fill-pointer 0))))))

;;; perl-pkg-to-cl-pkg-name: map a Perl package name to the CL package-name
;;; string PCL's codegen uses.  Codegen pipe-quotes multi-segment names
;;; (|Try::Tiny|, case-preserved) but emits single-segment names as bare
;;; tokens the reader upcases (Carp -> CARP, main -> MAIN).  Runtime package
;;; lookups must follow the SAME rule, or e.g. a glob/symbolic-ref op on a
;;; multi-segment package would create/find a wrong-case empty "TRY::TINY"
;;; that shadows the real "Try::Tiny".
;;; %pcl-invert-case: mirror the CL :invert readtable-case transform on a
;;; string.  Generated code is read under (readtable-case :invert): an
;;; all-lowercase token is upcased, an all-uppercase token is downcased, a
;;; mixed-case token is preserved.  Every runtime site that builds a CL symbol
;;; (or package name) from a Perl identifier *string* must apply this SAME
;;; transform so it lands on the symbol the reader produced.  (Replaces the old
;;; blanket string-upcase, which only agreed for all-lowercase names.)
;;; A name carrying a NON-ASCII character is emitted PIPE-QUOTED (task #418,
;;; Pl::CLForm::cl_sym), and inside |…| the reader applies neither its NFKC
;;; normalisation nor :invert — the symbol's name is the perl name, character
;;; for character.  So the runtime's transform must be the IDENTITY on exactly
;;; those names, or the two sides of the seam disagree again in the other
;;; direction: `(p-stash "ＦＯＯ")` would look for the down-cased fullwidth
;;; "ｆｏｏ" while codegen wrote `:|ＦＯＯ|`.  This one guard is what makes
;;; every %pcl-invert-case caller (symbolic refs, ->can, globs, stash keys,
;;; caller, bareword filehandles, loop tags, sub names) agree at once.
(defun %pcl-non-ascii-name-p (s)
  (some (lambda (c) (> (char-code c) 127)) (string s)))

(defun %pcl-invert-case (s)
  (let ((s (string s)) (has-upper nil) (has-lower nil))
    (when (%pcl-non-ascii-name-p s) (return-from %pcl-invert-case s))
    (loop for c across s do
          (cond ((upper-case-p c) (setf has-upper t))
                ((lower-case-p c) (setf has-lower t))))
    (cond ((and has-upper (not has-lower)) (string-downcase s))
          ((and has-lower (not has-upper)) (string-upcase s))
          (t s))))

(defun perl-pkg-to-cl-pkg-name (pkg-str)
  (if (search "::" pkg-str)
      (string pkg-str)
      (%pcl-invert-case pkg-str)))

;;; The INVERSE of PERL-PKG-TO-CL-PKG-NAME: the Perl spelling of a CL package
;;; object, for every site that hands a package name back to the program
;;; (glob stringification, *FOO{PACKAGE}).  %pcl-invert-case is its own
;;; inverse, but it is applied only to SINGLE-segment names on the way in — a
;;; name containing "::" is used verbatim — so undoing it has to make the same
;;; distinction, or an all-lowercase multi-segment package (version::regex)
;;; comes back upcased.
(defun %pcl-cl-pkg-to-perl-name (pkg)
  (let ((n (package-name pkg)))
    (if (search "::" n) n (%pcl-invert-case n))))

;;; %pcl-cl-sub-name: CL symbol-NAME for a Perl sub/method NAME, matching the
;;; token `pl-<name>` the codegen emits and the :invert reader interns.  The
;;; "pl-" prefix participates in the case-uniformity test, so it must be
;;; lower-case and inverted together with NAME (NOT "PL-" + upcase NAME, which
;;; mis-resolves any non-lowercase method such as DESTROY / AUTOLOAD / Foo).
(defun %pcl-cl-sub-name (name)
  (%pcl-invert-case (concatenate 'string "pl-" (string name))))

;;; %pcl-loop-tag: catch/throw tag for a labeled loop or block.  PREFIX is the
;;; literal "LAST"/"NEXT"/"REDO" (a string, so readtable-case never touches it);
;;; LABEL is the loop label as a SYMBOL (compile-time, already :invert-read) or a
;;; STRING (runtime dynamic — inverted to match the symbol form).  Both the
;;; codegen-emitted catch and every runtime throw build the tag through here, so
;;; they agree for ALL label cases (uniform-case like SKIP and mixed like MyLoop).
(defun %pcl-loop-tag (prefix label)
  (intern (concatenate 'string (string prefix) "-"
                       (if (symbolp label)
                           (symbol-name label)
                           (%pcl-invert-case (string label))))
          :pcl))

;;; %pcl-uname-to-sub: CODE-slot sub symbol-NAME for a typeglob whose stored
;;; (already :invert-cased) variable name is UNAME.  Glob var slots are
;;; sigil+UNAME (correct, sigils carry no case), but the CODE slot is the
;;; `pl-<name>` sub, whose prefix participates in the case fold — so recover the
;;; original name (invert is its own inverse) and rebuild via %pcl-cl-sub-name.
(defun %pcl-uname-to-sub (uname)
  (%pcl-cl-sub-name (%pcl-invert-case uname)))

;;; A blessed HASH ref stores its class in the hash under the keyword key
;;; :__class__ (so it survives unboxing).  Real Perl hash keys are always
;;; strings, so this internal key must be hidden from keys/values/each and the
;;; scalar key-count — otherwise a blessed object's class leaks (e.g. broke
;;; Sub::Override's `keys %$self`).  Centralised here so every hash-iteration
;;; site can filter it out the same way.
(declaim (inline %p-real-hash-key-p))
(defun %p-real-hash-key-p (k)
  "T for a user-visible Perl hash key, NIL for the internal :__class__ blessing key."
  (not (eq k :__class__)))

(defun %p-hash-user-count (h)
  "hash-table-count of H minus the internal :__class__ blessing key, if present.
   The user-visible Perl key count (`scalar %h` / `scalar keys %h`)."
  (- (hash-table-count h)
     (if (nth-value 1 (gethash :__class__ h)) 1 0)))

;;; p-sub: Define a Perl subroutine.
;;; Uses eval-when so the function exists at compile time, allowing
;;; BEGIN blocks to call subs defined before them in source order.
;;; This matches Perl's semantics where subs are compiled immediately.
;;; Marks the symbol as :defined in *p-declared-subs* for defined &sub support.
;;;
;;; IMPORTANT: We shadow the name before defining to create a package-local
;;; symbol.  Without this, user-defined methods whose names match PCL built-ins
;;; (e.g. sub PUSH / sub SHIFT) would redefine the global pcl:p-push etc.
;;; because packages (:use :pcl) inherit those symbols.  By shadowing first we
;;; create a fresh local symbol; the body's built-in calls (p-shift @_) were
;;; already resolved at READ time to pcl::PL-SHIFT and are unaffected.
(defmacro p-sub (name params &body body)
  ;; Leading (declare ...) forms are lifted to the lambda head (before the
  ;; bookkeeping let*), e.g. the v2 pipeline's (declare (ignore %_args)
  ;; (dynamic-extent %_args)) for subs that never touch @_.
  (let ((decls (loop while (and (consp (first body))
                                (eq (first (first body)) 'declare))
                     collect (pop body))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ;; Use the symbol's own package (e.g. P1 for P1::p-tmc) so that
       ;; package-qualified subs are defined in the right package regardless of
       ;; the current *package*.  Fall back to *package* for unqualified names.
       (let* ((target-pkg (or (symbol-package ',name) *package*))
              (sym-name   (symbol-name ',name)))
         ;; Shadow to prevent user methods from clobbering pcl:: built-ins with
         ;; the same name (e.g. PUSH/SHIFT in Tie::Array).  The handler-bind
         ;; muffles SBCL's "package at variance" warning that fires when defpackage
         ;; is later re-evaluated and sees the extra shadow.
         (handler-bind ((warning #'muffle-warning))
           (shadow sym-name target-pkg))
         (let ((local-sym (intern sym-name target-pkg))
               ;; Per-sub constants, computed ONCE at definition: recomputing
               ;; them per call (case-inversion + concatenate / gethash) was
               ;; measured at ~150ns per call — the single largest sub-call cost.
               (%%perl-name (%p-sub-perl-name ',name))
               (%%home-pkg  (pcl-pkg-perl-name (symbol-package ',name))))
           (setf (gethash local-sym *p-declared-subs*) :defined)
           (setf (symbol-function local-sym)
                 (lambda ,params
                   ,@decls
                   (let* ((*pcl-caller-pkg-stack* (cons *pcl-current-package*
                                                        *pcl-caller-pkg-stack*))
                          (*pcl-caller-subname-stack* (cons %%perl-name
                                                            *pcl-caller-subname-stack*))
                          (*pcl-current-package* %%home-pkg)
                          (*pcl-sub-call-depth* (1+ *pcl-sub-call-depth*))
                          (*pcl-caller-wantarray* *wantarray*))
                     (catch :p-return
                       ,@body)))))))))

(defmacro p-args-body (&body body)
  "Standard named-sub prologue emitted by the code generator: bind Perl's @_
   from the &rest %_args captured by the enclosing p-sub lambda list, then run
   BODY.  Both @_ and %_args are symbols exported from :pcl and inherited into
   every generated user package, so `(p-sub NAME (&rest %_args) (p-args-body …))`
   binds and reads the SAME symbols here regardless of *package* — no
   expansion-time package resolution needed."
  `(let ((@_ (p-flatten-args %_args)))
     ,@body))

(defun %p-args-need-flatten (args)
  "True when the raw &rest ARGS list holds an aggregate that Perl's argument
   flattening must spread: a raw non-string vector (an array) or a non-blessed
   hash-table (a hash).  Boxed values — including array/hash REFS, which are
   boxed — never spread.  Mirrors p-flatten-args' dispatch."
  (loop for a in args
        thereis (or (and (vectorp a) (not (stringp a)))
                    (and (hash-table-p a) (not (gethash :__class__ a))))))

(defmacro p-raw-params ((&rest params) &body body)
  "Signature fast path for `sub f { my ($a,$b) = @_; … }` whose body provably
   never observes @_ again: bind PARAMS raw (unboxed, no p-list-= / no boxes)
   positionally from the enclosing p-sub's &rest %_args, missing args -> undef.
   Unlike a plain &optional lambda list this HONOURS the uniform calling
   convention — callers pass containers raw and the CALLEE spreads aggregates
   (f(@args) / f(@_) delegation; task #80 broke Moo through exactly that) —
   while the common all-scalar call takes a no-allocation type-scan fast path."
  `(let ((%_args (if (%p-args-need-flatten %_args)
                     (coerce (p-flatten-args %_args) 'list)
                     %_args)))
     (let* ,(loop for p in params
                  collect `(,p (if %_args (pop %_args) (p-undef))))
       ,@body)))

;;; p-declare-sub: Forward-declare a Perl sub as a no-op stub.
;;; Perl subs can be called before definition; CL resolves names at load time.
;;; Only creates the stub if the function isn't already defined.
;;; Marks the symbol as :stub in *p-declared-subs* for exists &sub support.
;;; CALLING a stub is perl's fatal, never a value (CLAUDE.md rule 12: a missing
;;; case that would produce a VALUE the program consumes must DIE).  It happens
;;; two ways, and perl dies for both: the sub is never defined at all, or PCL
;;; ran the call before the definition (task #456 — a file-level `sub nm {…}`
;;; hoisted AFTER a preceding block that switches package).  Answering nil made
;;; the second one silent: `{ package Q; print main::nm(); } sub nm {"PKG"}`
;;; printed the empty string.  \&foo taken on a stub is unaffected — it is a
;;; TRAMPOLINE that re-reads symbol-function at call time (p-backslash-sub), so
;;; it still reaches a body defined later.
(defun %p-call-of-undefined-sub (sym args)
  ;; perl's own order: a plain sub call to a name with no body runs the
  ;; package's AUTOLOAD (with $AUTOLOAD set to the qualified name and the
  ;; original arguments) — and there is NO @ISA walk for a plain call, that is
  ;; the method rule.  Only when the package has no AUTOLOAD is it fatal.
  ;; Probed s432: `sub foo; sub AUTOLOAD {...} print foo()` prints AUTO(main::foo)
  ;; under perl; PCL answered the stub's undef before this.
  (let* ((pkg (symbol-package sym))
         (al  (and pkg (find-symbol (%pcl-cl-sub-name "AUTOLOAD") pkg))))
    (if (and al (eq (symbol-package al) pkg) (fboundp al)
             (not (eq (gethash al *p-declared-subs*) :stub)))
        (progn
          (%pcl-set-autoload-var (pcl-pkg-perl-name pkg) (%p-sub-perl-name sym))
          (apply (symbol-function al) args))
        (p-die (format nil "Undefined subroutine &~A called.~%"
                       (%p-sub-perl-name sym))))))

(defmacro p-declare-sub (name)
  `(progn
     (unless (gethash ',name *p-declared-subs*)
       (setf (gethash ',name *p-declared-subs*) :stub))
     (unless (fboundp ',name)
       (defun ,name (&rest args)
         (%p-call-of-undefined-sub ',name args)))))

;;; p-eval-always: Wrap a form so it runs at compile time, load time, and
;;; execute time.  This is the CL idiom known as "eval-always".  In the
;;; generated intermediate code it marks every Perl declaration (my $x,
;;; our @a, sub foo, use Some::Module, require ...) that must be visible to
;;; BEGIN blocks which may call or inspect them before the file finishes
;;; loading — mirroring Perl's rule that declarations take effect as the
;;; parser sees them.
(defmacro p-eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body))

;;; p-BEGIN: Wrap a Perl BEGIN { } block.  Runs at compile time and when
;;; executing directly, but NOT when loading a pre-compiled FASL (so the
;;; block fires exactly once, as Perl guarantees).
(defmacro p-BEGIN (&body body)
  `(eval-when (:compile-toplevel :execute) ,@body))

;;; p-CHECK: Wrap a Perl CHECK { } or UNITCHECK { } block.  Runs after
;;; compilation, just before execution starts (CL :load-toplevel phase).
(defmacro p-CHECK (&body body)
  `(eval-when (:load-toplevel) ,@body))

;;; Forward declarations to avoid style warnings.
;;; NOTE: do NOT tighten the return ftype of to-number/to-string — they are
;;; declaimed INLINE below, and SBCL 2.6.0 ICEs (type-error in sb-c during
;;; LENGTH derivation) on an inline function with a declaimed narrower return
;;; type.  The inline numberp/stringp fast paths already give callers
;;; branch-local type information, which is what the re-check elision needs.
(declaim (ftype (function (t) t) to-number to-string unbox p-get-stream))
;;; Forward-declare functions defined later in the file to suppress SBCL
;;; STYLE-WARNING: "undefined function" during compilation.
(declaim (ftype (function (t) t)
                object-address looks-like-number
                p-typeglob-p p-typeglob-name p-typeglob-package
                p-regex-match-p p-regex-match-pattern p-regex-match-modifiers
                perl-pkg-to-clos-class
                p-get-coderef))
(declaim (ftype (function (t t) t) p-can p-isa p-glob-slot))
(declaim (ftype (function (&rest t) t)
                p-method-call p-glob-undef-name p-glob-copy parse-number
                build-ppcre-options))
(defvar *p-undef* :undef "Perl's undef value")

;;; Forward declaration for %INC table (full definition in Module System section)
(defvar *p-inc-table* (make-hash-table :test 'equal)
  "Perl %INC - tracks loaded modules (forward declaration)")

;;; Cache for p-eval string transpilation results
(defvar *p-eval-string-cache* (make-hash-table :test 'equal)
  "Cache for p-eval: maps (cons perl-code pkg-name) -> cl-text.
   Avoids re-spawning pl2cl for repeated identical eval calls.")

;;; Counter for the "(eval N)" tag Perl puts in error messages from string eval.
(defvar *p-eval-counter* 0
  "Incremented per string-eval that throws, so $@'s ' at (eval N) line 1.'
   suffix carries a distinct N like Perl's eval-sequence number.")

;;; Lexical-capture bridge for string eval.  Perl's `eval "code"` can see the
;;; enclosing sub's `my` lexicals; CL's `eval` runs in the null lexical
;;; environment and cannot.  PCL bridges the gap by:
;;;   1. codegen at the eval site passing an alist of (name . box/array/hash)
;;;      for the in-scope lexicals (bound to *p-eval-lex-alist* by p-eval),
;;;   2. the transpiler wrapping the eval body in
;;;      (p-eval-thunk '(free-names) (lambda (free-syms) body)),
;;; so every variable the eval references that is NOT declared inside the eval
;;; becomes a lambda parameter.  Because the lambda creates a *lexical* binding,
;;; closures built inside the eval body capture it correctly (the whole point —
;;; e.g. Sub::Defer's `eval 'sub { $captured }'`).  See
;;; docs/eval-lexical-capture.md.
(defvar *p-eval-lex-alist* nil
  "Alist (var-name-string . box/array/hash) of the caller's in-scope lexicals,
   bound by p-eval and consumed by p-eval-thunk.")

;;; M-F (v2, s295 — the ALIAS rule, ir-spec §9.1): a file lexical the v2
;;; pipeline RENAMES to a package cell ($x -> $x__file__N) is invisible to
;;; eval'd code that names the original $x — including code the eval
;;; TRANSPILE emitted itself (a sub defined inside an eval string whose
;;; nested eval mentions $x), where no codegen-site alist can ever know the
;;; cell.  v1 never has this problem because it defvars file lexicals under
;;; their ORIGINAL names, so p-eval-lex-lookup's global fall-through finds
;;; them.  The alias restores exactly that visibility: codegen emits
;;; (p-alias-eval-cell '$x $x__file__N) at the declaration's RUN position —
;;; the quoted symbol is read under the declaring section's in-package, so
;;; it IS the original-name global of the declaring package.  ONE storage
;;; location per name, deliberately: a side registry (tried s294) lets a
;;; stale entry permanently shadow a live global; writing the one symbol
;;; both passes and plain defvar'd lexicals use gives v1's time-ordered
;;; last-declaration-wins model with no lifetime bookkeeping.
(defun p-alias-eval-cell (sym cell)
  "Store CELL (a renamed file-lexical's container) as the value of SYM, the
   variable's ORIGINAL-name symbol in its declaring package, so string eval
   resolves the name through the global fall-through exactly as under v1.
   Called where the renamed declaration executes; last execution wins."
  (setf (symbol-value sym) cell)
  cell)

;;; Persistent transpiler subprocess for p-eval
(defvar *p-transpiler-process* nil
  "Persistent pl2cl --server process, or nil if not yet started.
   Started lazily on first p-transpile-string call. Restarted if it dies.")

;;; Sub declaration/definition tracking for exists &sub / defined &sub
;;; Maps CL function symbol → :stub (declared only), :defined (has body),
;;; or :was-defined (was defined, now undef'd).
(defvar *p-declared-subs* (make-hash-table :test 'eq)
  "Perl sub existence tracking for exists &sub and defined &sub")

(defvar *p-lazy-coderef-target* (make-hash-table :test 'eq :weakness :key)
  "Maps a LAZY code ref (p-backslash-sub's stub trampoline or AUTOLOAD
   fallback lambda) to the sub symbol it stands for.  defined/exists on a
   coderef consult this so \\&foo taken before `sub foo {...}` answers by
   the symbol's CURRENT status, exactly like perl's late-bound glob slot.
   Weak keys: an entry lives only as long as the coderef itself.")

;;; use overload — Operator Overloading Registry
;;; Maps (cons pkg-name op-string) -> handler (CL function or method-name string).
;;; Populated at class-definition time by p-register-overloads.
(defvar *p-overload-table* (make-hash-table :test 'equal)
  "use overload registry: (cons pkg-name op-string) -> handler")

;;; use overload — fallback flags per package.
;;; :undef = default (try stringify/numify), t = autogenerate, :no = die on undef op.
(defvar *p-overload-fallback* (make-hash-table :test 'equal)
  "use overload fallback per package: :undef (default), t, or :no")

;;; @_ - Perl's subroutine arguments array. Must be declared special so that
;;; let-bindings in p-sub are dynamic (not lexical), allowing callbacks and
;;; closures like $SIG{__WARN__} handlers to access the caller's @_ via dynamic scope.
(defvar @_ (make-array 0 :adjustable t :fill-pointer 0)
  "Perl @_ - current subroutine arguments")

;;; Regex capture group variables ($1, $2, ... $9) and named captures (%+).
;;; A non-participating group is Perl undef, represented as *p-undef* (NOT raw
;;; nil): %p-flatten-list drops raw nil as an array-hole/empty-list marker, so a
;;; nil here would make an undef capture VANISH from a list (e.g.
;;; `my ($a,$b)=($3,$4,...)`).  *p-undef* survives flattening as one undef slot.
(defvar $1 *p-undef* "Regex capture group 1")
(defvar $2 *p-undef* "Regex capture group 2")
(defvar $3 *p-undef* "Regex capture group 3")
(defvar $4 *p-undef* "Regex capture group 4")
(defvar $5 *p-undef* "Regex capture group 5")
(defvar $6 *p-undef* "Regex capture group 6")
(defvar $7 *p-undef* "Regex capture group 7")
(defvar $8 *p-undef* "Regex capture group 8")
(defvar $9 *p-undef* "Regex capture group 9")
;; High capture groups.  Codegen emits $10.. as bare symbols (digits are
;; caseless under :invert), so exporting these makes them resolve here.
;; Patterns with more than 20 groups leave $21+ unbound (unseen in practice).
(defvar $10 *p-undef* "Regex capture group 10")
(defvar $11 *p-undef* "Regex capture group 11")
(defvar $12 *p-undef* "Regex capture group 12")
(defvar $13 *p-undef* "Regex capture group 13")
(defvar $14 *p-undef* "Regex capture group 14")
(defvar $15 *p-undef* "Regex capture group 15")
(defvar $16 *p-undef* "Regex capture group 16")
(defvar $17 *p-undef* "Regex capture group 17")
(defvar $18 *p-undef* "Regex capture group 18")
(defvar $19 *p-undef* "Regex capture group 19")
(defvar $20 *p-undef* "Regex capture group 20")
(defvar |$&| nil "Regex MATCH - the whole matched string")
(defvar |$`| nil "Regex PREMATCH - everything before the match")
(defvar |$'| nil "Regex POSTMATCH - everything after the match")
(defvar |$+| nil "Regex - last (highest-numbered) capture group that matched")
(defvar |$^N| nil
  "Perl $^N - the participating capture group whose closing parenthesis is
   rightmost in the pattern (perlvar).  Set by set-match-vars from the
   closer-position vector cached with the scanner.")
(defvar %+ (make-hash-table :test 'equal) "Perl %+ - named regex captures")
(defvar |%-| (make-hash-table :test 'equal)
  "Perl %- - named regex captures, each value an ARRAY of all buffers with
   that name (undef elements for non-participating buffers).")
;; Compile-time hints: $^H (bitmask) and %^H (hints hash).  PCL keeps them as
;; ordinary globals — the perl lexical save/restore-at-scope-exit semantics
;; (and (caller)[10] exposure) are NOT implemented; see docs/perl-suite-triage.md.
(defvar |$^H| 0 "Perl $^H - compile-time hint bits (no lexical scoping in PCL)")
(defvar |%^H| (make-hash-table :test 'equal)
  "Perl %^H - compile-time hints hash (no lexical scoping in PCL)")
;; @- (@LAST_MATCH_START) and @+ (@LAST_MATCH_END): offset arrays from the last
;; successful match.  Element 0 is the whole-match start/end; element N is the
;; start/end of capture group N.  Non-participating groups hold undef (nil).
(defvar |@-| (make-array 0 :adjustable t :fill-pointer 0) "Regex @LAST_MATCH_START - match/group start offsets")
(defvar |@+| (make-array 0 :adjustable t :fill-pointer 0) "Regex @LAST_MATCH_END - match/group end offsets")
;; @{^CAPTURE} (5.26+): the capture GROUP VALUES of the last successful match,
;; 0-based -- element 0 is $1.  Truncated after the last participating group,
;; exactly like @- / @+ (perl: "$#{^CAPTURE} is one less than $#-"), with undef
;; for a non-participating group inside that range (t/re/pat.t asserts both).
;; %{^CAPTURE} and %{^CAPTURE_ALL} are perl SYNONYMS for %+ and %-, so they get
;; no state of their own -- the emitter maps them onto those two.
(defvar |@{^CAPTURE}| (make-array 0 :adjustable t :fill-pointer 0)
  "Regex @{^CAPTURE} - capture group values of the last successful match")

;;; Default variable ($_) - defined later after make-p-box (see Boxed special variables section)
;;; Process ID ($$) is likewise boxed and defined in that later section so
;;; Perl-side `$$ = N` works (assignable since 5.16).

;;; Child exit status ($?)
(defvar $? 0 "Child process exit status from last system/backtick")

;;; Input line number ($.) - defined later after make-p-box (see Boxed special variables section)

;;; Program name ($0)
(defvar $0 (or (car sb-ext:*posix-argv*) "perl") "Program name")

;;; Eval error ($@) - defined later after make-p-box (see Boxed special variables section)

;;; OS name ($^O)
(defvar |$^O|
  #+linux "linux"
  #+darwin "darwin"
  #+windows "MSWin32"
  #-(or linux darwin windows) "unknown"
  "Operating system name")

;;; Lexical hints — $^H (hint bits) and %^H (the hints hash). PCL does not model
;;; compile-time hints; expose them as inert always-bound empties so `$^H & MASK`,
;;; `\%^H` and `keys %^H` never hit an unbound variable (eval.t RT 63110 / the
;;; "use feature" hint-transmission tests). Nothing ever writes meaningful data.
(defvar |$^H| 0 "Perl $^H - lexical hint bits (inert 0 in PCL)")
(defvar |%^H| (make-hash-table :test 'equal) "Perl %^H - hints hash (inert empty in PCL)")
(defvar |$^W| 0
  "Perl $^W - global warnings flag.  Inert 0: PCL does not model runtime
   warning-level switching; reads/writes must simply not crash (run/switcht.t,
   uni/variables.t).")
(defvar |$[| 0
  "Perl $[ - array base.  Always 0 since perl 5.30 removed assigning to it;
   inert here so reads don't crash (uni/variables.t).")

;;; A scalar holding a v-string literal (v1.2.3) remembers its v-string-ness:
;;; ref \$v is "VSTRING" until the scalar is overwritten (s///, y/// and plain
;;; assignment all flatten it, because the replacement value they store is a
;;; plain string — no explicit clearing needed).  Copying propagates it, as in
;;; perl.  S is the character payload: what the value stringifies as and what
;;; sprintf %vd walks.  DISPLAY overrides stringification only — $^V is a
;;; version object in perl, printing "v5.30.0" while %vd walks the payload.
(defstruct p-vstring
  (s "" :type string)
  (display nil))

;;; Perl version ($^V) - we report as PCL
(defvar |$^V|
  (make-p-vstring :s (coerce (list (code-char 5) (code-char 30) (code-char 0))
                             'string)
                  :display "v5.30.0")
  "Perl version (compatibility).  Payload 5.30.0 matches $] = 5.030000.")

;;; Perl executable path ($^X) - point to perl so spawned subprocesses run Perl
(defvar |$^X|
  (or (sb-ext:posix-getenv "PERL")
      (ignore-errors
        (let ((out (with-output-to-string (s)
                     (sb-ext:run-program "/bin/sh" (list "-c" "command -v perl 2>/dev/null")
                                         :output s :error nil))))
          (let ((trimmed (string-right-trim '(#\Newline #\Return #\Space) out)))
            (when (> (length trimmed) 0) trimmed))))
      "perl")
  "Perl executable path")

;;; Taint mode flag (${^TAINT}) - always off in transpiled code
(defvar |${^TAINT}| nil "Taint mode is not enabled")

;;; Regex code-block result ($^R) - result of last successful (?{...}) eval
(defvar |$^R| nil "Result of last successful (?{...}) regex code block")

;;; Interpreter state ($^S): 0 at runtime, 1 while executing an eval body
;;; (p-eval-block / p-eval rebind it), undef during BEGIN — PCL runs BEGIN
;;; bodies through the same eval machinery, so that case reads 1, not undef.
(defvar |$^S| 0 "Perl $^S - eval state: 0 runtime, 1 inside eval")

;;; System error ($!) - dualvar: numeric = errno integer, string = strerror
;;; We cache the errno in *p-stored-errno* so that SBCL's internal C calls
;;; (malloc, GC, etc.) do not corrupt $! between "$! = N" and the next read.
(defvar *p-stored-errno* 0)

(defun p-errno-string ()
  "Return $! as dualvar: (to-number ...) = errno, (to-string ...) = strerror.
   When errno=0, returns \"\" (falsy) to preserve Perl's !$! truthiness semantics."
  (let ((errno *p-stored-errno*))
    (if (zerop errno)
        ""   ; errno=0: falsy like Perl's $! when no error
        (let ((msg (or (sb-int:strerror errno)
                       (format nil "Unknown error ~D" errno))))
          ;; Build a dualvar box so $!+0 gives the integer and $! in string context gives the message
          (let ((box (%make-p-box :value msg)))
            (setf (p-box-sv box) msg (p-box-sv-ok box) t)
            (setf (p-box-nv box) (float errno) (p-box-nv-ok box) t)
            box)))))

(defun (setf p-errno-string) (val)
  "Perl $! = N — set errno to integer N (clears it when N=0)"
  (let ((n (truncate (to-number (if (p-box-p val) (unbox val) val)))))
    (setf *p-stored-errno* n)
    (setf (sb-alien:extern-alien "errno" sb-alien:int) n))
  val)

(defun %pcl-save-errno ()
  "Capture the C errno immediately after a system call into *p-stored-errno*.
   Call this right after any OS call that may set errno on failure."
  (setf *p-stored-errno* (sb-alien:get-errno)))

(defun %pcl-local-errno-init (n)
  "Helper for 'local $! = N': coerce n to int, set C errno, return int for let binding."
  (let ((i (truncate (to-number n))))
    (setf (sb-alien:extern-alien "errno" sb-alien:int) i)
    i))

;;; Wantarray context variable
(defvar *wantarray* nil "Context for the current call: t=list, nil=scalar, :void=void.")
(defvar *p-in-list-assign-rhs* nil
  "True while evaluating the RHS of a p-list-= assignment. Tells p-readline to use scalar
   mode even when *wantarray* is t (avoids reading the entire file in while-loop idioms).")
(defvar *pcl-caller-wantarray* :void
  "Saved *wantarray* from sub entry. p-wantarray reads this so wantarray() always
   reflects the context of the CURRENT sub's caller, even when *wantarray* has been
   overridden by gen_funcall for a nested call.")

;;; THE CONTEXT PROTOCOL, NAMED (task #281 item 1, s414).  Perl's calling
;;; context is a dynamic binding of *wantarray* (docs/ir-spec.md §5), and that
;;; binding was the loudest single shape in the emitted file — 7 to 17 of them
;;; per 100 lines, i.e. every 6th to 14th line, spelled as a bare `let` a
;;; reader has to decode.  These four macros expand to EXACTLY that let, so
;;; the generated code is renamed, never changed: same bindings, same body,
;;; same code after macroexpansion, no runtime cost by construction.
;;; A translator reading PCL's output should treat them as the context marks
;;; they are; there is no fifth context.
(defmacro p-list-ctx (&body body)
  "Evaluate BODY in Perl LIST context."
  `(let ((*wantarray* t)) ,@body))

(defmacro p-scalar-ctx (&body body)
  "Evaluate BODY in Perl SCALAR context."
  `(let ((*wantarray* nil)) ,@body))

(defmacro p-void-ctx (&body body)
  "Evaluate BODY in Perl VOID context."
  `(let ((*wantarray* :void)) ,@body))

(defmacro p-caller-ctx (&body body)
  "Evaluate BODY in the context this sub was CALLED in — the propagating case
   (goto &sub, a tail call): *wantarray* becomes the saved caller context."
  `(let ((*wantarray* *pcl-caller-wantarray*)) ,@body))

;;; THE SORT COMPARATOR, NAMED (task #281 item 6, s414).  A `sort BLOCK` /
;;; `sort NAME` comparator is a lambda over the comparison pair whose body
;;; runs inside (catch :p-return (block nil …)) — because perl's `return`
;;; inside a sort block exits the COMPARATOR, not the enclosing sub.  Spelling
;;; that out at each of the three emission sites hid the rule in boilerplate;
;;; this macro expands to exactly the same three forms.
;;; Leading (declare …) forms stay at the lambda head, where CL requires them:
;;; a region's package-qualified pair must be declared special there or the
;;; parameter binding is lexical and a comparator reading the global sees
;;; nothing (see Pl/ExprToCL.pm gen_inline_lambda_form).
(defmacro p-sort-cmp (params &body body)
  "A Perl sort comparator over PARAMS: BODY with `return` bound to it."
  (let ((decls '()))
    (loop while (and body (consp (car body)) (eq (car (car body)) 'declare))
          do (push (pop body) decls))
    `(lambda ,params ,@(nreverse decls)
       (catch :p-return (block nil ,@body)))))

;;; PEEL A CONTEXT WRAP OFF AN EMITTED FORM, at macroexpansion time.  A macro
;;; that pattern-matches the code its caller was handed — %p-fh-arg recovering
;;; a bareword filehandle from (pl-NAME), p-list-='s undef-placeholder test —
;;; must see through the context bind, or the wrap silently defeats the match
;;; (a bareword FH then CALLS pl-NAME, an undefined function).  There are TWO
;;; spellings and both must be peeled: the (p-…-ctx BODY) macros above, and the
;;; bare (let ((*wantarray* V)) BODY) they replaced — still emitted where a
;;; second variable is bound alongside, and still present in captured v1 text.
;;; Comparison is by symbol NAME, like the "PL-" tests elsewhere: an emitted
;;; form arrives with whatever package's symbols the generated file interned.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %p-ctx-wrap-p (form)
    "True when FORM is a one-body context wrap; NIL otherwise."
    (and (consp form) (symbolp (car form))
         (or (and (= (length form) 2)
                  (member (symbol-name (car form))
                          '("P-LIST-CTX" "P-SCALAR-CTX" "P-VOID-CTX" "P-CALLER-CTX")
                          :test #'string=))
             (and (= (length form) 3)
                  (string= (symbol-name (car form)) "LET")
                  (consp (second form)) (= (length (second form)) 1)
                  (consp (first (second form)))
                  (symbolp (first (first (second form))))
                  (string= (symbol-name (first (first (second form))))
                           "*WANTARRAY*")))))

  (defun %p-strip-ctx (form)
    "FORM with every context wrap peeled off; FORM itself when there is none."
    (if (%p-ctx-wrap-p form)
        (%p-strip-ctx (car (last form)))
        form)))

;;; END blocks - executed in reverse order at program exit
(defvar *end-blocks* nil "List of END block thunks to execute at exit")
(defvar *unitcheck-blocks* nil "UNITCHECK thunks (push = LIFO = perl's reverse order)")
(defvar *check-blocks* nil "CHECK thunks (push = LIFO = perl's reverse order)")
(defvar *init-blocks* nil "INIT thunks (pushed; run in reverse-of-LIFO = source order)")

(defvar *p-compile-phase-done* nil
  "True once the main program's compile->run boundary has been crossed.
   p-exit consults it: exit during the compile phase must still drain
   pending UNITCHECK/CHECK blocks (perl runs them before the ENDs), while
   blocks registered after the boundary are 'too late' and never run.")

(defun %p-drain-compile-blocks ()
  "Run pending UNITCHECK then CHECK thunks, pop-as-we-go so an exit (or
   error) inside one still lets the REMAINING thunks run on the way out."
  (loop while *unitcheck-blocks* do (funcall (pop *unitcheck-blocks*)))
  (loop while *check-blocks* do (funcall (pop *check-blocks*))))

(defun p-run-compile-phase-blocks ()
  "The compile->run boundary of the main program: run UNITCHECK blocks
   (reverse order), then CHECK blocks (reverse order), then INIT blocks
   (source order).  Emitted once, before the first runtime section.
   Blocks registered later (a runtime require or eval) are perl's 'too
   late to run' case — they never fire."
  (%p-drain-compile-blocks)
  (setf *p-compile-phase-done* t)
  (setf *init-blocks* (reverse *init-blocks*))
  (loop while *init-blocks* do (funcall (pop *init-blocks*))))

;; Register exit hook to run END blocks, then flush every open Perl output
;; handle — perl closes (hence flushes) all handles at exit, so a program that
;; prints to a handle and never closes it must still see its output.  The flush
;; runs AFTER the END blocks because an END block may still print.
(pushnew (lambda ()
           (dolist (fn *end-blocks*)
             (handler-case (funcall fn)
               (error (e)
                 (format *error-output* "Error in END block: ~A~%" e))))
           (%p-flush-open-streams))
         sb-ext:*exit-hooks*)

;;; ============================================================
;;; Value Boxing - All Perl scalars are boxed for reference support
;;; ============================================================

;; A box is a mutable cell that holds a scalar value.
;; This enables Perl references: \$x returns the box, $$ref accesses its value.
;; The class slot is set by bless for blessed scalar references.
(defstruct (p-box (:constructor %make-p-box))
  "Perl scalar value with lazy caching (like Perl's SV).
   - value: the authoritative value
   - nv/nv-ok: cached numeric value and validity flag
   - sv/sv-ok: cached string value and validity flag
   - class: blessed class name
   - is-ref: t when this box was created by p-backslash (a reference wrapper, not a
     variable box). Used by box-set to avoid double-boxing when a reference variable
     is passed through a function call: box-set($param, $ref_var) must store the
     reference value (BREF inside $ref_var), not $ref_var itself."
  value
  (nv nil)
  (nv-ok nil)
  (sv nil)
  (sv-ok nil)
  (class nil)
  (is-ref nil))

(defun make-p-box (value &optional class)
  "Create a p-box, pre-caching if value is already typed"
  (let ((box (%make-p-box :value value :class class)))
    (typecase value
      (number (setf (p-box-nv box) value (p-box-nv-ok box) t))
      (string (setf (p-box-sv box) value (p-box-sv-ok box) t)))
    box))

;;; ============================================================
;;; Tie proxy — stored inside a p-box when the variable is tied
(defun p-dualvar (num str)
  "Perl Scalar::Util::dualvar — create a scalar with separate numeric and string values."
  (let ((n (to-number (unbox num)))
        (s (to-string (unbox str))))
    (let ((box (%make-p-box :value s)))
      (setf (p-box-sv box) s (p-box-sv-ok box) t)
      (setf (p-box-nv box) n (p-box-nv-ok box) t)
      box)))

;;; ============================================================
;;; When tie() is called on a scalar, the box's value slot is replaced
;;; with a p-tie-proxy.  unbox() calls FETCH; box-set() calls STORE.
;;; The saved-value is restored when untie() is called.

(defstruct p-tie-proxy
  "Holds the tie object and the RAW slot value of a tied scalar."
  tie-obj            ; object returned by TIESCALAR/TIEARRAY/TIEHASH
  saved-value        ; the RAW slot (pre-tie value, then magic-off writes)
  (untied nil))      ; untie() ran from inside this box's own tie handler

;; A tie object can hold a (blessed) ref back to the very box it proxies — e.g.
;; `sub TIESCALAR { bless \my $x }` — making the structure self-referential.
;; The default structure printer would recurse forever and exhaust the control
;; stack, so print opaquely instead of descending into the slots.
(defmethod print-object ((p p-tie-proxy) stream)
  (print-unreadable-object (p stream :type t :identity t)))

;;; Perl's save_magic/restore_magic (mg.c): while a tie handler for an SV is on
;;; the stack, THAT SV's magic is off, so a read or write of the tied variable
;;; from inside its own FETCH/STORE hits the raw slot instead of re-entering the
;;; handler.  Math::BigInt depends on it — `sub STORE { $rnd_mode = ... }`
;;; assigns to the very variable it proxies, and its round_mode() setter writes
;;; the same cell through a symbolic ref; without suppression that recurses
;;; until the binding stack dies (task #224).
;;;
;;; The raw slot is the proxy's saved-value: swap it into the box for the
;;; duration so every read/write site sees a plain box with no special case,
;;; then swap the proxy back — keeping whatever the handler wrote — on the way
;;; out, including a non-local exit.
(defvar *p-tie-magic-off* nil
  "Alist of (box . proxy) for tie handlers currently on the stack.  Only
   tied()/untie() consult it; every other site sees an ordinary box.")

(defun %p-suppressed-proxy (box)
  "The tie proxy of BOX when BOX's magic is temporarily off because one of its
   own tie handlers is running.  NIL otherwise."
  (cdr (assoc box *p-tie-magic-off* :test #'eq)))

;;; The three dispatch helpers below are the ONLY users of the macro.  They are
;;; deliberately out-of-line: `unbox` is declaimed inline and `p-scalar-=`
;;; expands into generated code, so an inline unwind-protect at either site
;;; multiplies across the whole image (measured s342: SBCL exhausted a 1 GB heap
;;; compiling socket-01.t).
(defmacro %with-tie-magic-off ((box proxy) &body body)
  "Run BODY (a FETCH/STORE dispatch) with BOX's tie magic suppressed."
  (let ((b (gensym "BOX"))
        (p (gensym "PROXY")))
    `(let* ((,b ,box)
            (,p ,proxy)
            (*p-tie-magic-off* (cons (cons ,b ,p) *p-tie-magic-off*)))
       (setf (p-box-value ,b) (p-tie-proxy-saved-value ,p)
             (p-box-nv-ok ,b) nil
             (p-box-sv-ok ,b) nil)
       (unwind-protect (progn ,@body)
         (setf (p-tie-proxy-saved-value ,p) (p-box-value ,b))
         (unless (p-tie-proxy-untied ,p)
           (setf (p-box-value ,b) ,p))
         (setf (p-box-nv-ok ,b) nil
               (p-box-sv-ok ,b) nil)))))

(defun %p-tie-fetch (box proxy)
  "Perl's mg_get: call FETCH with BOX's magic off.  Returns the method's raw
   result — callers apply their own unbox/to-number/to-string."
  (%with-tie-magic-off (box proxy)
                       (let ((result (p-method-call (p-tie-proxy-tie-obj proxy) "FETCH")))
                         ;; mg.c's magic_methpack ends in sv_setsv(sv, result) —
                         ;; the raw slot holds the last FETCH, which is what an
                         ;; untie() leaves behind.
                         (box-set box result)
                         result)))

(defun %p-tie-store (box proxy value plain)
  "Perl's sv_setsv + mg_set: write the raw slot, then call STORE with BOX's
   magic off.  VALUE goes through box-set (full copy semantics); PLAIN is the
   already-unboxed value handed to STORE."
  (%with-tie-magic-off (box proxy)
                       (box-set box value)
                       (p-method-call (p-tie-proxy-tie-obj proxy) "STORE" plain)))

(defun %p-tie-store-ref (box proxy ref)
  "Like %p-tie-store for the reference path of p-scalar-=, where the raw write
   stores the box itself rather than copying through box-set."
  (%with-tie-magic-off (box proxy)
                       (setf (p-box-value box) ref)
                       (p-method-call (p-tie-proxy-tie-obj proxy) "STORE" ref)))

;;; A magical scalar lvalue.  Like p-tie-proxy it lives in a p-box's value slot
;;; and is intercepted at the unbox/box-set chokepoints, but it dispatches to two
;;; CL closures rather than a Perl tie object: reading calls GETTER, writing calls
;;; SETTER.  Used for \$#array (arylen) and reusable for other magic lvalue refs
;;; (\substr / \pos / \vec).  See docs/sweep-bug-catalog.md (array.t arylen).
(defstruct p-magic-cell
  getter        ; (function () -> value)        — invoked by unbox
  setter        ; (function (new-value) -> value) — invoked by box-set
  (kind nil))   ; nil → ref()="SCALAR" (arylen); :lvalue → ref()="LVALUE"
                                        ;   (\substr / \pos / \vec), matching Perl's reftype.

(declaim (inline unbox))
(defun unbox (val)
  "Extract value from a box, or return val if not boxed.
   If the box contains a p-tie-proxy, dispatches to FETCH.
   If the box contains a p-magic-cell, dispatches to its getter."
  (if (p-box-p val)
      (let ((v (p-box-value val)))
        (cond
          ((p-tie-proxy-p v)
           (unbox (%p-tie-fetch val v)))
          ((p-magic-cell-p v)
           (funcall (p-magic-cell-getter v)))
          (t v)))
      val))
(declaim (notinline unbox))

(defun ensure-boxed (val)
  "Ensure a value is boxed"
  (if (p-box-p val)
      val
      (make-p-box val)))

(defun %p-wrong-referent-p (kind v)
  "True when V is DEFINITELY a referent of the wrong kind for a KIND deref —
   an array/code/scalar ref where a hash was asked for, or a hash/code ref
   where an array was.  Deliberately narrow: undef, nil, strings (symbolic
   refs) and typeglobs are NOT mismatches — each has its own established
   behaviour at these sites (autovivify, symbolic lookup, glob slot), and
   widening this predicate would change those instead of only replacing a
   host-level failure with perl's.  A p-box here is a SCALAR ref: `unbox` has
   already peeled the outer container, so a remaining box is \\$x.
   Callers put this AFTER their fast path — it is a diagnosis of a failure
   already reached, never a test a correct access has to pay for.

   A REMAINING P-BOX IS NOT A MISMATCH, and that is the subtle one (s317).
   PCL's `\\%h` is a DOUBLE box — box(box(hash-table)) — so `unbox` peeling one
   layer legitimately leaves a box that the caller unboxes again:
   p-ensure-hashref returns it and (setf p-gethash) unboxes it.  Counting it as
   a scalar-ref mismatch broke `$refref->{k} = $v` and `$a->[0] = $x` through a
   `\\@fake` (perl-tests/postfixderef.t stopped 4 tests early, t/op/avhv.t
   crashed at test 7).  A scalar-ref-to-coderef collapses the same way — see
   p-cast-$.  Telling \\$x from a representation layer needs a referent-kind tag
   on the box, which PCL does not have; until it does, only the three
   UNAMBIGUOUS container types below count as mismatches."
  (let ((hash (hash-table-p v))
        (ary  (and (vectorp v) (not (stringp v))))
        (code (functionp v)))
    (cond ((string= kind "HASH")  (or ary  code))
          ((string= kind "ARRAY") (or hash code))
          ((string= kind "CODE")  (or hash ary))
          ;; SCALAR is deliberately absent — see the comment in p-cast-$.
          (t (error "%p-wrong-referent-p: unknown ref kind ~S" kind)))))

(defun %p-not-a-ref (kind)
  "Perl's fatal wrong-kind dereference: `Not a HASH reference` and friends.
   Raised where a deref site is handed a referent of the wrong kind — an array
   ref used as a hash, a hash ref used as an array.  KIND is the kind the
   SOURCE asked for (\"HASH\", \"ARRAY\", \"CODE\", \"SCALAR\"); the message is
   perl's byte for byte (perl names only the wanted kind), so `$@ =~ /^Not a
   HASH reference/` — which real code greps and t/op/avhv.t asserts 40 times —
   matches.  Two things this replaces, both bad: an SBCL type error naming a
   P-BOX (not catchable in Perl terms), and a SILENT wrong answer where the
   site defaulted (`keys %$aryref` returned the empty list).  CLAUDE.md rule
   12: a dispatch that cannot handle a value says so loudly."
  (error "Not ~A ~A reference" (if (char= (char kind 0) #\A) "an" "a") kind))

;;; ---------------------------------------------------------------------------
;;; Read-only arrays — Internals::SvREADONLY(@a, 1)   (task #159)
;;; ---------------------------------------------------------------------------
;;; A read-only array is stored as a SIMPLE vector: the same element boxes as
;;; before (so in-bounds element writes — `$a[0] = 9`, a foreach alias write —
;;; still land, exactly as in perl, where a read-only AV is FIXED SIZE but its
;;; elements are not read-only), but with no fill pointer and not adjustable.
;;; Every size-changing operation therefore fails BY CONSTRUCTION; the checks
;;; below exist only to replace SBCL's message with perl's, and to catch the
;;; few cases perl kills that a simple vector would otherwise tolerate (unshift
;;; with an empty list, a no-op splice).  Reading costs nothing — LENGTH and
;;; AREF work on either storage — which is why this representation was chosen
;;; over a per-array flag consulted on the push hot path (fable-answers-s318
;;; §2 option (b)).
(declaim (inline %p-array-readonly-p))
(defun %p-array-readonly-p (a)
  "True when A is an array whose STORAGE is read-only, i.e. fixed size.
   Strings are excluded: they are vectors too, and a literal string has no fill
   pointer either, but string writes are not this mechanism's business."
  (and (vectorp a) (not (stringp a)) (not (array-has-fill-pointer-p a))))

(defun %p-readonly-modification ()
  "Perl's fatal for a write to read-only storage, byte for byte — push.t,
   unshift.t, splice.t and sort.t all match /^Modification of a read-only
   value/."
  (error "Modification of a read-only value attempted"))

(declaim (inline %p-check-array-writable))
(defun %p-check-array-writable (a)
  "Die perl's death if A is a read-only array.  Call at the head of any
   operation that changes an array's SIZE."
  (when (%p-array-readonly-p a) (%p-readonly-modification)))

;;; ---------------------------------------------------------------------------
;;; The two rule-12 endings for a case PCL does not implement   (task #152)
;;; ---------------------------------------------------------------------------
;;; CLAUDE.md rule 12: a dispatch over a closed set never falls through to a
;;; default that swallows the value.  Which ending applies was ruled in
;;; docs/fable-answers-s328.md §1 by ONE test — does the missing case produce or
;;; write a VALUE the program then consumes?
;;;
;;;   VALUE-PRODUCING → %p-unsupported-value: DIE, naming the operand.  Silent
;;;     wrong values are the worst failure mode in this codebase (p-vec's
;;;     missing 64-bit width wrote nothing and returned plausible zeros).
;;;   EFFECT-ONLY (a jump, a tie, a flag — the data is unchanged and the program
;;;     otherwise runs) → %p-announce-unsupported: one stderr line, then carry
;;;     on.  Measured s328: making computed-label `goto` die cost state.t 88
;;;     verified rows while sweep-diff still reported "0 new".  The sin is the
;;;     silence, not the fall-through.
;;;
;;; Unclassifiable in a minute counts as value-producing.  Both helpers take the
;;; same (SITE, OPERAND) pair so a grep for either finds the whole family, and
;;; the announce side dedups on that pair — ONE table, not a hash per site.
(defvar *p-unsupported-announced* (make-hash-table :test 'equal)
  "Dedup set for %p-announce-unsupported: one line per (SITE, OPERAND) per
   process, so an unimplemented case inside a loop stays one line.")

(defun %p-announce-unsupported (site operand &optional detail)
  "Say on stderr that SITE cannot honour OPERAND, once per pair per process.
   For EFFECT-ONLY cases only — the caller carries on afterwards.  Returns NIL
   so a dispatch arm can simply end with this call.  DETAIL, when given,
   replaces the default \"ignored\" tail."
  (let ((key (format nil "~A/~A" site operand)))
    (unless (gethash key *p-unsupported-announced*)
      (setf (gethash key *p-unsupported-announced*) t)
      (format *error-output* "PCL: ~A: ~A is not implemented — ~A~%"
              site operand (or detail "ignored"))
      (force-output *error-output*)))
  nil)

(defun %p-unsupported-value (site operand &optional detail)
  "Perl-visible fatal for a VALUE-PRODUCING case SITE does not implement.
   Never returns: the point is that no plausible-looking value escapes.
   DETAIL, when given, is appended the way %p-announce-unsupported appends it,
   so the two halves of the family print the same shape."
  (if detail
      (error "PCL: ~A: ~A is not implemented — ~A" site operand detail)
      (error "PCL: ~A: ~A is not implemented" site operand)))

(defun p-unrepresentable-char (code)
  "The value of a string literal that held CODE, a code point above U+10FFFF.
   SBCL's CHAR-CODE-LIMIT is #x110000, so no CL character holds CODE and the
   string simply cannot exist; perl's extended UTF-8 does hold it.  The
   compiler emits this call IN PLACE of the character (Pl/ExprToCL.pm's
   _cl_string_literal_form) so the emitted file still READS — writing the
   character raw produced bytes SBCL's UTF-8 reader rejects, and that cost the
   whole file, not the one expression (#419).  Never returns."
  (%p-unsupported-value
   "string literal"
   (format nil "code point 0x~X" code)
   "above SBCL's char-code-limit (U+10FFFF); see docs/not-supported.md \"Code points above U+10FFFF (perl's extended UTF-8)\""))


(defun %p-array-set-readonly (a flag)
  "Return the storage @A must have for perl's SvREADONLY flag FLAG.
   Read-only: a simple vector over the SAME element boxes.  Writable: a fresh
   adjustable vector with a fill pointer.  A is returned unchanged when it
   already has the requested storage, and announced-and-returned when it is not
   an array at all.  The caller (the Internals::SvREADONLY macro) stores the
   result back into the variable's cell — which is why the macro needs the
   variable, not its value."
  (cond
    ((not (and (vectorp a) (not (stringp a))))
     (%p-announce-unsupported "Internals::SvREADONLY"
                              (if (hash-table-p a) "a HASH (perl's restricted hash is a different feature)"
                                  "a non-array value"))
     a)
    ((p-true-p (unbox flag))
     (if (%p-array-readonly-p a)
         a
         (let ((ro (make-array (length a))))
           (replace ro a)
           ro)))
    (t
     (if (%p-array-readonly-p a)
         (let ((rw (make-array (max 8 (length a))
                               :adjustable t :fill-pointer (length a))))
           (replace rw a)
           rw)
         a))))

(defun %p-svreadonly-other (thing &optional (flag nil flag-p))
  "Internals::SvREADONLY on anything that is not a named array variable: a
   scalar, a hash (perl's restricted hashes — a different feature), or an
   aggregate reached through a reference (whose storage cell the call site does
   not have).  As a GETTER it answers honestly for an array and \"\" otherwise;
   as a SETTER it announces and does nothing.  Perl returns 1/\"\", not 1/0."
  (declare (ignore flag))
  (let ((v (unbox thing)))
    (when flag-p
      (%p-announce-unsupported "Internals::SvREADONLY"
                               (cond ((hash-table-p v) "a HASH (perl's restricted hash is a different feature)")
                                     ((and (vectorp v) (not (stringp v)))
                                      "an array reached through a reference")
                                     (t "a SCALAR"))))
    (make-p-box (if (%p-array-readonly-p v) 1 ""))))

(defun p-copy-scalar-arg (val)
  "Copy a scalar argument/default into a FRESH p-box for a signature parameter.
   Perl signature params are copies of @_ (like `my ($x) = @_`), so a param must
   be its own mutable box — mutating it ($x = ...) must not write through to the
   caller's variable (p-flatten-args keeps the caller's boxes as-is in @_).
   Reads through tie/magic are FETCHed via unbox; the reference flag and blessed
   class are preserved so a ref/blessed arg copies its container, not its referent."
  (if (p-box-p val)
      (let ((b (make-p-box (unbox val))))
        (setf (p-box-is-ref b) (p-box-is-ref val)
              (p-box-class b)  (p-box-class val))
        b)
      (make-p-box val)))

;;; Boxed special variables (must be after make-p-box definition)
;;; Default variable ($_) - p-box so p-scalar-= / box-set work correctly
(defvar $_ (make-p-box nil) "Perl's $_ - default variable")
;;; Process ID ($$) - p-box so Perl-side `$$ = N` works (assignable since 5.16).
(defvar $$ (make-p-box (sb-posix:getpid)) "Process ID")
;;; A saved core (tools/prove-core, standalone executables) keeps the PID of
;;; the process that BUILT it in $$ — refresh at image boot, like the FP-mode
;;; hook above.  (fork re-sets $$ in the child separately.)
(push (lambda () (box-set $$ (sb-posix:getpid)))
      sb-ext:*init-hooks*)
;;; Input line number ($.) — Perl's $. is not a plain scalar: it reflects the
;;; line counter (IoLINES) of the *last-accessed* filehandle.  Reading $.
;;; returns that handle's counter; writing $. sets it; `tell`/`seek`/`eof`/a
;;; read all make their handle the current one.  We model this with a per-handle
;;; counter table keyed on the CL stream, plus a magic p-box whose getter/setter
;;; dispatch to *p-last-read-handle* (the handle $. refers to).
(declaim (special *p-last-read-handle*))
(defvar *p-fh-lines* (make-hash-table :test 'eq)
  "Per-filehandle input line counter (Perl IoLINES): stream -> line count.")
(defun %p-dot-get ()
  "Getter for $.: the last-accessed handle's line counter, or undef if none."
  (if *p-last-read-handle*
      (gethash *p-last-read-handle* *p-fh-lines* 0)
      nil))
(defun %p-dot-set (new-val)
  "Setter for $.: store NEW-VAL as the last-accessed handle's line counter."
  (let ((n (truncate (to-number new-val))))
    (when *p-last-read-handle*
      (setf (gethash *p-last-read-handle* *p-fh-lines*) n))
    n))
(defvar |$.| (make-p-box (make-p-magic-cell :getter #'%p-dot-get
                                            :setter #'%p-dot-set))
  "Input line number of last filehandle accessed (magic — see above).")
(defmacro p-local-dot (&body body)
  "Localize $. (Perl `local $.`).  Perl localizes only the *current filehandle*
   that $. refers to (PL_last_in_gv), not the per-handle line counter: on scope
   exit the previously-current handle is restored, but any IoLINES changes made
   to a handle inside BODY persist.  Modelled as a dynamic rebinding of
   *p-last-read-handle* (reads/writes inside reach the current handle's counter;
   the pointer reverts on exit)."
  `(let ((*p-last-read-handle* *p-last-read-handle*))
     ,@body))
;;; Eval error ($@) - p-box so it can hold references (e.g. $@ = [])
(defvar $@ (make-p-box "") "Error from last eval")
;;; Input record separator ($/)
(defvar |$/| (make-p-box (string #\Newline)) "Input record separator")
;;; Output record separator ($\)
(defvar |$\\| (make-p-box "") "Output record separator")
;;; List separator ($")
(defvar |$"| (make-p-box " ") "List separator for array interpolation")
;;; Output autoflush ($|) — magic: every write clamps to 1/0 by truthiness
;;; (assigning 5 stores 1; --$| toggles: 1-1=0 stores 0, 0-1=-1 is true so it
;;; stores 1 — op/ver.t 48).  The value lives in the dynamic *p-autoflush* so
;;; `local $|` (p-local-pipe) is a plain rebinding that keeps the magic box in
;;; place, mirroring $.'s *p-last-read-handle* model.
(defvar *p-selected-out* nil
  "The handle select() made the default for print/printf/say, kept as the
   DESIGNATOR the program passed (box, glob, symbol or name); NIL means STDOUT.
   $| is per-handle and applies to whatever is selected — which is why the
   classic autoflush idiom `select((select($fh), $|=1)[0])` needs BOTH select
   and $| to be real, not stubs.")

(defun %p-default-out ()
  "The stream print/printf/say write to when no filehandle is given: the
   select()ed one, else STDOUT."
  (or (and *p-selected-out* (p-get-stream *p-selected-out*))
      *standard-output*))

(defvar *p-autoflush-handles* (make-hash-table :test 'eq :weakness :key)
  "Streams whose $| is 1.  ONLY autoflushing handles get an entry, so
   `(zerop (hash-table-count …))` is a one-fixnum fast path on every print —
   $| is set in a vanishing fraction of programs and must cost nothing in the
   rest.  Weak keys: an abandoned handle must not be pinned here.")

(defun %p-maybe-autoflush (stream)
  "Flush STREAM if its $| is on."
  (when (and (plusp (hash-table-count *p-autoflush-handles*))
             (gethash stream *p-autoflush-handles*))
    (ignore-errors (finish-output stream))))

(defvar |$\|| (make-p-box
               (make-p-magic-cell
                :getter (lambda ()
                          (if (gethash (%p-default-out) *p-autoflush-handles*) 1 0))
                :setter (lambda (new)
                          ;; Perl: $| is the SELECTED handle's flag, writes clamp
                          ;; to 0/1 by truthiness (assigning 5 stores 1; --$|
                          ;; toggles — op/ver.t 48), and turning it ON flushes
                          ;; that handle right away.  The flush is what makes
                          ;; IO::Handle::flush and printflush work in plain Perl.
                          (let ((s (%p-default-out)))
                            (cond ((p-true-p new)
                                   (setf (gethash s *p-autoflush-handles*) t)
                                   (ignore-errors (finish-output s)))
                                  (t (remhash s *p-autoflush-handles*)))))))
  "Output autoflush flag (magic — per selected handle, writes clamp to 0/1)")

(defmacro p-local-pipe (&body body)
  "Localize $| (Perl `local $|`) for the handle selected at entry: local's undef
   clamps to 0 inside, and the previous flag comes back on exit — including on a
   non-local exit (die/last), which the old dynamic rebinding also gave us."
  (let ((s (gensym "STREAM")) (old (gensym "OLD")))
    `(let* ((,s (%p-default-out))
            (,old (gethash ,s *p-autoflush-handles*)))
       (remhash ,s *p-autoflush-handles*)
       (unwind-protect (progn ,@body)
         (if ,old
             (setf (gethash ,s *p-autoflush-handles*) t)
             (remhash ,s *p-autoflush-handles*))))))
;;; Subscript separator ($;)
(defvar |$;| (make-p-box (string (code-char #x1C))) "Subscript separator (default SUBSEP)")
;;; Output field separator ($,)
(defvar |$,| (make-p-box "") "Output field separator for print")
;;; Perl version number ($])
(defvar |$]| (make-p-box "5.030000") "Perl version number")
;;; Format/write special variables (rarely used in modern code)
(defvar |$~| (make-p-box "STDOUT") "FORMAT_NAME - name of current report format for write (defaults to the selected handle's name, like Perl)")
(defvar |$=| (make-p-box 60) "FORMAT_LINES_PER_PAGE - page length for write")
(defvar |$-| (make-p-box 0) "FORMAT_LINES_LEFT - lines left on page for write")
(defvar |$%| (make-p-box 0) "FORMAT_PAGE_NUMBER - current page number for write")
(defvar |$:| (make-p-box " \n-") "FORMAT_LINE_BREAK_CHARACTERS - word-break chars for write")
(defvar |$^L| (make-p-box (string #\Page)) "FORMAT_FORMFEED - formfeed char for write")
(defvar |$^A| (make-p-box "") "ACCUMULATOR - for formline/write output")
;;; Process credentials ($< $> $( $)).  The GID forms are perl's
;;; "gid sup1 sup2 ..." space-joined string.  Snapshots taken at load;
;;; assignment writes the box but does not change process credentials.
(defun %pcl-getgroups-string (lead-gid)
  "LEAD-GID followed by the supplementary group ids, space-joined."
  (let* ((n (sb-alien:alien-funcall
             (sb-alien:extern-alien
              "getgroups" (function sb-alien:int sb-alien:int (* (sb-alien:unsigned 32))))
             0 (sb-alien:sap-alien (sb-sys:int-sap 0)
                                   (* (sb-alien:unsigned 32)))))
         (ids (when (> n 0)
                (sb-alien:with-alien ((buf (sb-alien:array (sb-alien:unsigned 32) 256)))
                  (let ((got (sb-alien:alien-funcall
                              (sb-alien:extern-alien
                               "getgroups" (function sb-alien:int sb-alien:int (* (sb-alien:unsigned 32))))
                              (min n 256) (sb-alien:cast buf (* (sb-alien:unsigned 32))))))
                    (loop for i from 0 below (max got 0)
                          collect (sb-alien:deref buf i)))))))
    (format nil "~D~{ ~D~}" lead-gid ids)))
(defvar |$<| (make-p-box (sb-posix:getuid))  "Real user id")
(defvar |$>| (make-p-box (sb-posix:geteuid)) "Effective user id")
(defvar |$(| (make-p-box (%pcl-getgroups-string (sb-posix:getgid)))
  "Real gid + supplementary groups, space-joined")
(defvar |$)| (make-p-box (%pcl-getgroups-string (sb-posix:getegid)))
  "Effective gid + supplementary groups, space-joined")
(defvar |$^P| (make-p-box 0)  "PERLDB - internal debugger flag (0 = not debugging)")
(defvar |$^D| (make-p-box 0)  "DEBUGGING - debugging flags")
(defvar |$^F| (make-p-box 2)  "SYSTEM_FD_MAX - max file descriptor for subprocesses")
(defvar |$^I| (make-p-box *p-undef*) "INPLACE_EDIT - in-place edit extension")
(defvar |$^M| (make-p-box *p-undef*) "emergency memory pool")
(defvar |$^| (make-p-box "STDOUT_TOP") "FORMAT_TOP_NAME - top-of-page format name (defaults to <handle>_TOP, like Perl)")
;; Signal names and numbers, in Perl's own order (Config's sig_name/sig_num on
;; Linux/glibc).  ONE table serves both consumers: the pre-populated %SIG keys
;; below and kill()'s name designators (%p-resolve-signal).
(defparameter *p-signal-numbers*
  '(("ZERO" . 0)   ("HUP" . 1)    ("INT" . 2)    ("QUIT" . 3)   ("ILL" . 4)
    ("TRAP" . 5)   ("ABRT" . 6)   ("BUS" . 7)    ("FPE" . 8)    ("KILL" . 9)
    ("USR1" . 10)  ("SEGV" . 11)  ("USR2" . 12)  ("PIPE" . 13)  ("ALRM" . 14)
    ("TERM" . 15)  ("STKFLT" . 16)("CHLD" . 17)  ("CONT" . 18)  ("STOP" . 19)
    ("TSTP" . 20)  ("TTIN" . 21)  ("TTOU" . 22)  ("URG" . 23)   ("XCPU" . 24)
    ("XFSZ" . 25)  ("VTALRM" . 26)("PROF" . 27)  ("WINCH" . 28) ("IO" . 29)
    ("PWR" . 30)   ("SYS" . 31)   ("NUM32" . 32) ("NUM33" . 33) ("RTMIN" . 34)
    ("NUM35" . 35) ("NUM36" . 36) ("NUM37" . 37) ("NUM38" . 38) ("NUM39" . 39)
    ("NUM40" . 40) ("NUM41" . 41) ("NUM42" . 42) ("NUM43" . 43) ("NUM44" . 44)
    ("NUM45" . 45) ("NUM46" . 46) ("NUM47" . 47) ("NUM48" . 48) ("NUM49" . 49)
    ("NUM50" . 50) ("NUM51" . 51) ("NUM52" . 52) ("NUM53" . 53) ("NUM54" . 54)
    ("NUM55" . 55) ("NUM56" . 56) ("NUM57" . 57) ("NUM58" . 58) ("NUM59" . 59)
    ("NUM60" . 60) ("NUM61" . 61) ("NUM62" . 62) ("NUM63" . 63) ("RTMAX" . 64)
    ("IOT" . 6)    ("CLD" . 17)   ("POLL" . 29))
  "Perl signal name -> signal number (Config sig_name/sig_num, Linux/glibc).")

;; %SIG: signal/exception handler hash
;; __WARN__ and __DIE__ keys hold Perl callbacks invoked by warn/die.
;; Perl pre-populates %SIG with EVERY signal name the platform knows (values
;; undef), so `exists $SIG{HUP}` is true before any handler is installed —
;; sigtrap.pm's `grep(exists $SIG{$_}, qw(HUP INT PIPE TERM))` finds nothing
;; without it and its import loop then spins forever.  ZERO is a valid kill()
;; designator but is not a %SIG key, exactly as in perl.
(defvar %SIG
  (let ((h (make-hash-table :test 'equal)))
    (loop for entry in *p-signal-numbers*
          for name = (car entry)
          unless (string= name "ZERO")
          do (setf (gethash name h) (make-p-box *p-undef*)))
    h)
  "Perl %SIG - signal handlers")

(defun get-input-record-separator ()
  "Get the current value of $/ (unboxed).
   Returns nil for undef (slurp mode), a positive INTEGER record length for
   $/ = \\N (fixed-length record mode), or the separator STRING otherwise.
   A non-positive record length behaves like slurp (nil), matching Perl."
  (let ((val (unbox |$/|)))
    (cond
      ((eq val *p-undef*) nil)
      ;; $/ = \N (reference to a number) → fixed-length record mode.
      ((p-box-p val)
       (let ((n (truncate (to-number (unbox val)))))
         (if (> n 0) n nil)))
      (t (to-string val)))))

;;; Match position tracking for pos() — must precede box-set which uses it
(defvar *p-match-pos* (make-hash-table :test 'eq)
  "Hash table mapping boxed strings to their /g match positions")

;;; ------------------------------------------------------------
;;; Box accessors with lazy caching
;;; ------------------------------------------------------------

(defun box-set (box value)
  "Set box value, invalidating caches. Pre-caches if already typed.
   If value is a box containing a primitive, unbox it (Perl copy semantics).
   If value is a box containing another box (reference), preserve it.
   If value is a blessed box, copy the class to target box.
   If box is not a PL-BOX (e.g. *p-undef*), silently ignore (Perl: undef = val is no-op).
   If box is tied (contains a p-tie-proxy), routes through STORE."
  (unless (p-box-p box)
    (return-from box-set value))
  ;; Tied variable: delegate to STORE.  Magic lvalue: delegate to its setter.
  (let ((current (p-box-value box)))
    (when (p-tie-proxy-p current)
      ;; Perl's sv_setsv writes the raw slot FIRST, then calls mg_set with the
      ;; SV's magic off — so untie() after `$tied = "A"` leaves "A" behind, and
      ;; a read of the tied variable inside STORE sees "A" (probed s342).
      (let ((plain (if (p-box-p value) (unbox value) value)))
        (return-from box-set (%p-tie-store box current value plain))))
    (when (p-magic-cell-p current)
      (return-from box-set
        (funcall (p-magic-cell-setter current)
                 ;; keep a BLESSED box intact so the setter's stringification
                 ;; sees its "" overload (substr.t: assigning an overloaded
                 ;; object to a substr lvalue must call the overload)
                 (if (and (p-box-p value) (not (p-box-class value)))
                     (unbox value)
                     value)))))
  (let ((old-val (p-box-value box))  ; pre-assignment value, for the class-clear rule below
        (v (if (p-box-p value)
               (let ((inner (p-box-value value)))
                 (cond
                   ;; Tied source variable: call FETCH to get the actual value.
                   ;; Without this, assigning $c = $tied_var copies the proxy
                   ;; into $c, making $c appear tied too.
                   ((p-tie-proxy-p inner)
                    (unbox (%p-tie-fetch value inner)))
                   ;; Magic source ($c = $$arylen_ref): copy the getter's VALUE,
                   ;; not the magic cell itself (else $c would alias the magic).
                   ((p-magic-cell-p inner)
                    (funcall (p-magic-cell-getter inner)))
                   ;; If inner is a box, this is a reference.
                   ;; If value itself is a ref-wrapper (from p-backslash), preserve it as-is.
                   ;; If value is a variable box containing a reference, use inner directly
                   ;; to avoid the double-boxing that breaks recursive reference passing.
                   ((p-box-p inner) (if (p-box-is-ref value) value inner))
                   (t inner)))
               value)))
    ;; Perl: @arr in scalar context gives element count.
    ;; A raw adjustable vector (bare @arr, not wrapped in make-p-box) in a scalar assignment
    ;; becomes the count. But (make-p-box arr) = array ref must stay as-is.
    (when (and (not (p-box-p value))   ; unwrapped raw vector only
               (vectorp v)
               (not (stringp v))
               (adjustable-array-p v))
      (setf v (length v)))
    ;; Perl 5.26+: %hash in scalar context gives key count.
    ;; A raw hash-table (bare %hash) in a scalar assignment becomes the count.
    ;; But (make-p-box ht) = hash ref must stay as-is.
    (when (and (not (p-box-p value))   ; unwrapped raw hash-table only
               (hash-table-p v))
      (setf v (%p-hash-user-count v)))
    (setf (p-box-value box) v
          (p-box-nv-ok box) nil
          (p-box-sv-ok box) nil)
    ;; Perl: assigning to a scalar resets pos()
    (remhash box *p-match-pos*)
    ;; Preserve class from blessed boxes — but ONLY when the assigned value
    ;; is itself a reference: perl's stash is attached to the source SV, so
    ;; copying a plain VALUE out of a blessed scalar referent (`my $y = $$r`
    ;; where \$r's referent is blessed) yields an ordinary unblessed scalar.
    ;; Otherwise, CLEAR a stale class — but only when the box's OLD value was
    ;; itself the reference (vector/hash/function/box): overwriting a blessed
    ;; REFERENCE-holder with a plain value unblesses the variable (substr.t:
    ;; a substr-lvalue write through an overloaded object leaves a plain
    ;; string, else the stale class keeps firing overloaded "" on it).  A
    ;; blessed scalar REFERENT (old value a plain scalar) keeps its class on
    ;; assignment, like Perl's SV stash: qr.t `$$e = 'Fake!'` leaves $e
    ;; blessed into Stew.
    (if (and (p-box-p value) (p-box-class value)
             (or (p-box-p v)
                 (and (vectorp v) (not (stringp v)))
                 (hash-table-p v)
                 (functionp v)
                 (p-typeglob-p v)
                 (p-regex-match-p v)))
        (setf (p-box-class box) (p-box-class value))
        (when (and (p-box-class box)
                   (or (and (vectorp old-val) (not (stringp old-val)))
                       (hash-table-p old-val)
                       (functionp old-val)
                       (p-box-p old-val)))
          (setf (p-box-class box) nil)))
    ;; Glob ref vs bare glob: a typeglob arriving through a ref-wrapper (\\*foo,
    ;; is-ref t) keeps is-ref so it numifies to its address (GLOB(0x..)); a bare
    ;; glob (my $g = *foo) clears it so it numifies to 0.  The typeglob is stored
    ;; raw as the value either way, so the flag is the only discriminator.
    (when (p-typeglob-p v)
      (setf (p-box-is-ref box) (and (p-box-p value) (p-box-is-ref value))))
    (typecase v
      (number (setf (p-box-nv box) v (p-box-nv-ok box) t))
      (string (setf (p-box-sv box) v (p-box-sv-ok box) t)))
    ;; Dualvar preservation: if source box has a pre-cached NV alongside a string
    ;; value (like Perl's $! errno dualvar), copy that NV to the destination.
    ;; Without this, $saved = $! would lose the numeric errno value.
    (when (and (p-box-p value)
               (p-box-nv-ok value)
               (stringp v))
      (setf (p-box-nv box) (p-box-nv value)
            (p-box-nv-ok box) t))
    box))

(defun %pcl-nan-canonical-p (s)
  "True if S (lowercased, sign/whitespace-stripped) is a canonical NaN form that
   Perl converts without an 'isn't numeric' warning."
  (or (member s '("nan" "nanq" "nans" "qnan" "snan"
                  "1.#nanq" "1.#qnan" "1.#ind" "1.#ind00"
                  "1#nan" "1#snan" "1#ind")
              :test #'string=)
      ;; nan/nanq/nans with decimal or hex payload in properly-closed parens
      (and (>= (length s) 3) (string= (subseq s 0 3) "nan")
           (let* ((base-end
                   (cond
                     ((and (>= (length s) 4) (char= (char s 3) #\()) 3)
                     ((and (>= (length s) 5)
                           (member (char s 3) '(#\q #\s))
                           (char= (char s 4) #\()) 4)
                     (t nil))))
             (when base-end
               (let* ((payload-start (1+ base-end))
                      (close (position #\) s :start payload-start)))
                 (and close
                      (= close (1- (length s)))
                      (let ((content (subseq s payload-start close)))
                        (or (and (> (length content) 0)
                                 (every #'digit-char-p content))
                            (and (>= (length content) 3)
                                 (string= (subseq content 0 2) "0x")
                                 (every (lambda (c) (digit-char-p c 16))
                                        (subseq content 2))))))))))))

(defun parse-perl-number (str)
  "Parse a string to number using Perl semantics.
   Extracts leading numeric portion: '3rd' -> 3, '3.14foo' -> 3.14.
   Handles integers, floats, scientific notation, Inf/NaN.
   Returns 0 for non-numeric strings."
  (when (stringp str)
    (let ((trimmed (string-left-trim '(#\Space #\Tab #\Newline) str)))
      (when (> (length trimmed) 0)
        ;; Check for Inf/Infinity/NaN (case-insensitive)
        (let ((sign 1)
              (check trimmed))
          (when (and (> (length check) 0)
                     (member (char check 0) '(#\+ #\-)))
            (when (char= (char check 0) #\-)
              (setf sign -1))
            (setf check (subseq check 1)))
          (let* ((lower (string-downcase check))
                 (lower-stripped (string-right-trim '(#\Space #\Tab #\Newline) lower)))
            ;; Inf: "inf", "infinity", and MSVC "1.#inf*" / "1#inf*"
            (when (or (and (>= (length lower) 3)
                           (string= (subseq lower 0 3) "inf"))
                      (and (>= (length lower) 5)
                           (string= (subseq lower 0 5) "1#inf"))
                      (and (>= (length lower) 6)
                           (string= (subseq lower 0 6) "1.#inf")))
              ;; Warn when the form has garbage after the canonical Inf prefix
              (unless (member lower-stripped
                              '("inf" "infinity" "1.#inf" "1.#inf00" "1#inf" "1#inf00")
                              :test #'string=)
                (p-warn (format nil "Argument ~S isn't numeric~%" str)))
              (return-from parse-perl-number
                (if (minusp sign)
                    sb-ext:double-float-negative-infinity
                    sb-ext:double-float-positive-infinity)))
            ;; NaN: "nan*", "qnan", "snan", "nanq",
            ;; "1.#NAN", "1.#QNAN", "1.#NANQ", "1.#IND*",
            ;; "1#NAN", "1#SNAN", "1#IND" (MSVC-style without dot)
            (when (or (and (>= (length lower) 3)
                           (string= (subseq lower 0 3) "nan"))
                      (member lower-stripped '("qnan" "snan" "nanq") :test #'string=)
                      (and (>= (length lower) 6)
                           (string= (subseq lower 0 6) "1.#nan"))
                      (and (>= (length lower) 7)
                           (string= (subseq lower 0 7) "1.#qnan"))
                      (and (>= (length lower) 7)
                           (string= (subseq lower 0 7) "1.#nanq"))
                      (and (>= (length lower) 6)
                           (string= (subseq lower 0 6) "1.#ind"))
                      (and (>= (length lower) 5)
                           (string= (subseq lower 0 5) "1#nan"))
                      (and (>= (length lower) 6)
                           (string= (subseq lower 0 6) "1#snan"))
                      (and (>= (length lower) 5)
                           (string= (subseq lower 0 5) "1#ind")))
              ;; Warn when the form has garbage after the canonical NaN pattern
              (unless (%pcl-nan-canonical-p lower-stripped)
                (p-warn (format nil "Argument ~S isn't numeric~%" str)))
              (return-from parse-perl-number
                #+sbcl (sb-kernel:make-double-float #x7FF80000 0)
                #-sbcl (/ 0d0 0d0)))))
        ;; Extract leading numeric portion manually
        (let ((end 0)
              (len (length trimmed))
              (has-digit nil))
          ;; Optional sign
          (when (and (< end len)
                     (member (char trimmed end) '(#\+ #\-)))
            (incf end))
          ;; Integer part
          (loop while (and (< end len)
                           (digit-char-p (char trimmed end)))
                do (setf has-digit t) (incf end))
          ;; Optional decimal part
          (when (and (< end len) (char= (char trimmed end) #\.))
            (incf end)
            (loop while (and (< end len)
                             (digit-char-p (char trimmed end)))
                  do (setf has-digit t) (incf end)))
          ;; Optional exponent
          (when (and (< end len)
                     (member (char trimmed end) '(#\e #\E))
                     has-digit)
            (let ((exp-start end))
              (incf end)
              (when (and (< end len)
                         (member (char trimmed end) '(#\+ #\-)))
                (incf end))
              (if (and (< end len) (digit-char-p (char trimmed end)))
                  (loop while (and (< end len)
                                   (digit-char-p (char trimmed end)))
                        do (incf end))
                  ;; No valid exponent, backtrack
                  (setf end exp-start))))
          ;; Parse the extracted portion
          (when (and has-digit (> end 0))
            (let* ((num-str (subseq trimmed 0 end))
                   ;; Pre-check exponent so that "1e9999" doesn't silently return 0.
                   ;; read-from-string wraps fp-overflow in reader-impossible-number-error,
                   ;; so we detect extreme exponents before calling it.
                   (e-pos (or (position #\e num-str) (position #\E num-str)))
                   (exp-val (when e-pos
                              (ignore-errors
                                (parse-integer (subseq num-str (1+ e-pos))
                                               :junk-allowed t)))))
              (when (and exp-val (> exp-val 400))
                (return-from parse-perl-number
                  (if (char= (char num-str 0) #\-)
                      sb-ext:double-float-negative-infinity
                      sb-ext:double-float-positive-infinity)))
              (when (and exp-val (< exp-val -400))
                (return-from parse-perl-number 0.0d0))
              (multiple-value-bind (n pos)
                  (let ((*read-eval* nil))
                    (ignore-errors (read-from-string num-str)))
                (declare (ignore pos))
                (when (numberp n)
                  (return-from parse-perl-number n)))))))))
  0)

;;; Stable object identity.  SBCL's get-lisp-obj-address returns the raw
;;; pointer, which is NOT a usable Perl ref identity: the compacting GC
;;; relocates objects (changing the pointer), and PCL re-boxes refs on some
;;; paths.  A coderef threaded through Sub::Defer's coderef-keyed %DEFERRED
;;; would then present two different CODE(0x..) strings for one logical sub,
;;; breaking the hash lookup (Moo's lazy/subclass constructor bootstrap).
;;; Instead we assign each object a monotonic id the first time its identity
;;; is requested and reuse it for the object's lifetime.  The table is
;;; weak-on-key so dead objects don't leak; ids are never reused, so distinct
;;; objects can never collide (an improvement over reusable raw addresses).
#+sbcl
(defvar *p-object-id-table*
  (make-hash-table :test 'eq :weakness :key :synchronized t)
  "Live object -> stable identity integer (see object-address).")
(defvar *p-object-id-counter* 0
  "Monotonic source of ids for *p-object-id-table*.")

(defun object-address (obj)
  "Stable unique numeric identity for OBJ — the basis for Perl ref identity
   (refaddr, == on refs) and ref stringification (CODE/HASH/ARRAY(0x..)).
   Stable across GC relocation and re-boxing, unlike the raw pointer."
  #+sbcl
  (or (gethash obj *p-object-id-table*)
      (setf (gethash obj *p-object-id-table*)
            (incf *p-object-id-counter*)))
  #-sbcl (sxhash obj))  ; Fallback: use hash as pseudo-address

;;; Forward declarations for use overload helpers (p-get-class and p-method-call
;;; are defined later in the OO section; forward refs suppress SBCL style-warnings).
(declaim (ftype function p-get-class p-method-call))

;;; ============================================================
;;; use overload — Operator Overloading Helpers
;;; ============================================================
;;; These three functions implement the core of `use overload` dispatch.
;;; p-find-overload is called from every overloadable operator — it must be fast.
;;; The common case (non-blessed value) short-circuits at p-box-p with no allocation.

(defun %p-find-overload-mro (cls op-str visited)
  "Walk @ISA hierarchy to find an inherited use overload handler for OP-STR.
   Two-pass BFS: check direct parents first, then recurse into grandparents."
  ;; use overload: cycle guard
  (when (member cls visited :test #'equal)
    (return-from %p-find-overload-mro nil))
  (let* ((pkg      (find-package (%pcl-invert-case cls)))
         (isa-sym  (when pkg (find-symbol "@isa" pkg)))
         (isa-val  (when (and isa-sym (boundp isa-sym)) (symbol-value isa-sym))))
    (when (and isa-val (vectorp isa-val))
      (let ((new-visited (cons cls visited)))
        ;; First pass: direct parent table entries
        (loop for parent across isa-val
              for parent-str = (to-string parent)
              do (let ((h (gethash (cons parent-str op-str) *p-overload-table*)))
                   (when h (return-from %p-find-overload-mro h))))
        ;; Second pass: recurse into grandparents
        (loop for parent across isa-val
              for parent-str = (to-string parent)
              do (let ((h (%p-find-overload-mro parent-str op-str new-visited)))
                   (when h (return-from %p-find-overload-mro h)))))))
  nil)

(defun p-find-overload (val op-str)
  "Return the use overload handler for VAL's class and OP-STR, or NIL.
   Checks the class directly, then walks @ISA for inherited overloads."
  ;; use overload: fast path — non-boxes never have overloads
  (when (p-box-p val)
    (let ((cls (p-get-class val)))
      (when cls
        ;; Direct hit (common case)
        (or (gethash (cons cls op-str) *p-overload-table*)
            ;; Walk @ISA for inherited overloads (subclass of overloaded parent)
            (%p-find-overload-mro cls op-str nil))))))

(defun p-call-overload (handler self other reversedp)
  "Call a use overload handler with Perl's three-argument convention:
   handler(self, other, reversed).  REVERSEDP is true when the blessed
   object was the right operand and Perl swapped the args.
   Handler may be a CL function, a boxed code ref, or a string (method name)."
  ;; use overload: build the three args Perl overload handlers expect
  (let ((other-val   (or other *p-undef*))
        (reversed-val (if reversedp (make-p-box 1) *p-undef*)))
    (cond
      ((functionp handler)
       ;; Direct CL function: lambda or #'pl-name from \&sub
       (funcall handler self other-val reversed-val))
      ((p-box-p handler)
       ;; Boxed code ref (e.g. stored in a variable before use overload)
       (let ((inner (unbox handler)))
         (if (functionp inner)
             (funcall inner self other-val reversed-val)
             (error "use overload: boxed handler is not a function: ~S" inner))))
      ((stringp handler)
       ;; Method-name form: '+' => 'add' — call $self->add($other, $reversed)
       (p-method-call self handler other-val reversed-val))
      (t (error "use overload: invalid handler ~S for ~S" handler self)))))

(defun p-register-overloads (pkg pairs-vec)
  "Register use overload handlers for package PKG from PAIRS-VEC.
   PAIRS-VEC is the CL vector generated by transpiling:
     use overload '+' => \\&add, '\"\"' => \\&str, fallback => 1, ...
   Elements alternate: op-string handler op-string handler ..."
  ;; use overload: iterate pairs (op . handler) from the generated vector
  (let ((pairs (coerce pairs-vec 'list)))
    (loop for remaining on pairs by #'cddr
          for op-raw = (car remaining)
          for fn    = (cadr remaining)
          when (and op-raw (cdr remaining))
          do (let ((op-str (if (stringp op-raw)
                               op-raw
                               (to-string (if (p-box-p op-raw) (unbox op-raw) op-raw)))))
               (if (string= op-str "fallback")
                   ;; use overload fallback setting
                   (setf (gethash pkg *p-overload-fallback*)
                         (cond
                           ((null fn) :undef)
                           ((eq fn *p-undef*) :undef)
                           ((and (numberp fn) (zerop fn)) :no)
                           ((p-box-p fn)
                            (let ((v (unbox fn)))
                              (cond ((null v) :undef)
                                    ((eq v *p-undef*) :undef)
                                    ((and (numberp v) (zerop v)) :no)
                                    (t t))))
                           (t t)))
                   ;; use overload operator handler registration
                   (setf (gethash (cons pkg op-str) *p-overload-table*) fn))))))

;;; overload::import / overload::unimport — the runtime entry points behind
;;; `overload->import(...)` and modules that call them directly (e.g.
;;; JSON::PP::Boolean does `overload::unimport('overload', qw(0+ ++ -- fallback));
;;; overload::import('overload', '0+' => ..., 'bool' => ..., '""' => ...)`).
;;; Both act on the CALLER's package — which PCL tracks as *pcl-current-package*
;;; — after shifting off the leading 'overload' class argument, exactly like
;;; real overload.pm's `my $package = caller(); shift; ...`.
;;; Defined in the OVERLOAD package so the generated OVERLOAD::PL-IMPORT /
;;; OVERLOAD::PL-UNIMPORT calls resolve.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "OVERLOAD")
    (make-package "OVERLOAD" :use '(:cl :pcl))))

(defun %p-overload-import (&rest args)
  "overload::import(class, op1 => h1, op2 => h2, ...): register handlers for the
   caller's package.  ARGS[0] is the 'overload' class (dropped); the rest are the
   op/handler pairs, registered via p-register-overloads."
  (let ((pkg *pcl-current-package*))
    (when (cdr args)
      (p-register-overloads pkg (coerce (cdr args) 'vector))))
  nil)

(defun %p-overload-unimport (&rest args)
  "overload::unimport(class, op1, op2, ...): remove the named overload handlers
   from the caller's package.  ARGS[0] is the 'overload' class (dropped)."
  (let ((pkg *pcl-current-package*))
    (dolist (op (cdr args))
      (let ((op-str (to-string op)))
        (if (string= op-str "fallback")
            (remhash pkg *p-overload-fallback*)
            (remhash (cons pkg op-str) *p-overload-table*)))))
  nil)

(eval-when (:load-toplevel :execute)
  (setf (symbol-function (intern "PL-IMPORT" "OVERLOAD"))   #'%p-overload-import)
  (setf (symbol-function (intern "PL-UNIMPORT" "OVERLOAD")) #'%p-overload-unimport))

(defun p-overload-strval (obj)
  "Return the non-overloaded string value of OBJ (the raw address form).
   Implements overload::StrVal($obj) — bypasses any '\"\"' overload."
  ;; use overload: get raw address representation ignoring any "" handler
  (if (p-box-p obj)
      (let* ((cls   (p-get-class obj))
             ;; Same words and the same REFERENT address the plain stringifier
             ;; would print (#163) — StrVal only bypasses the '""' handler, it
             ;; does not get its own address rule.  This was a third copy that
             ;; printed the VARIABLE box's address for a scalar ref.
             (raw   (or (%p-ref-string obj)
                        (format nil "SCALAR(0x~(~X~))" (object-address obj)))))
        (make-p-box (if cls (format nil "~A=~A" cls raw) raw)))
      (make-p-box (to-string obj))))

(defun p-overloaded (obj)
  "Return true (1) if OBJ has any use overload handlers registered, else undef.
   Implements overload::Overloaded($obj)."
  ;; use overload: scan table for any entry whose package matches obj's class
  (when (p-box-p obj)
    (let ((cls (p-get-class obj)))
      (when cls
        (maphash (lambda (k v)
                   (declare (ignore v))
                   (when (and (consp k) (equal (car k) cls))
                     (return-from p-overloaded (make-p-box 1))))
                 *p-overload-table*))))
  *p-undef*)

(defun box-nv (box)
  "Get numeric value from box with lazy caching.
   Tied variables: bypass cache and call FETCH."
  (let ((inner (p-box-value box)))
    (when (p-tie-proxy-p inner)
      (return-from box-nv
        (to-number (%p-tie-fetch box inner))))
    (when (p-magic-cell-p inner)
      (return-from box-nv (to-number (funcall (p-magic-cell-getter inner))))))
  ;; use overload "0+" (numify): call handler if registered for this class
  (let ((handler (p-find-overload box "0+")))
    (when handler
      (return-from box-nv
        (to-number (p-call-overload handler box nil nil)))))
  (if (p-box-nv-ok box)
      (p-box-nv box)
      (let ((v (p-box-value box)))
        (let ((n (cond
                   ((numberp v) v)
                   ((eq v *p-undef*) 0)
                   ((null v) 0)
                   ((eq v t) 1)  ; CL's T from comparisons - Perl true is 1
                   ((stringp v) (parse-perl-number v))
                   ((p-vstring-p v) (parse-perl-number (p-vstring-s v)))
                   ;; Reference: perl's numeric value is the REFERENT's address,
                   ;; so `\$x == \$x` is true across two separate `\` wrappers
                   ;; (#163).  Same rule as the stringifier, one helper.
                   ((p-box-p v) (object-address (%p-ref-referent box)))
                   ((hash-table-p v) (object-address v))  ; blessed hash: numeric = address
                   ((and (vectorp v) (not (stringp v))) (object-address v))  ; blessed array: address
                   ((functionp v) (object-address v))  ; code ref: address
                   ((p-regex-match-p v) (object-address v))  ; compiled regex: address
                   ;; Glob REF numifies to its address (matches GLOB(0x..) stringify);
                   ;; a bare glob (is-ref nil) numifies to 0 ("*pkg::name" parses as 0).
                   ((p-typeglob-p v) (if (p-box-is-ref box) (object-address v) 0))
                   (t 0))))
          ;; Don't cache address-based NV: SBCL's GC can move objects,
          ;; making the cached address stale while a freshly-computed address
          ;; gives a different value for the same logical object.
          (unless (or (p-box-p v)
                      (hash-table-p v)
                      (and (vectorp v) (not (stringp v)))
                      (functionp v)
                      (p-regex-match-p v)
                      (and (p-typeglob-p v) (p-box-is-ref box)))  ; glob-ref address
            (setf (p-box-nv box) n
                  (p-box-nv-ok box) t))
          n))))

(defun stringify-value (v)
  "Convert a raw value to string"
  (cond
    ((stringp v) v)
    ((eq v *p-undef*) "")
    ((null v) "")
    ((p-vstring-p v) (or (p-vstring-display v) (p-vstring-s v)))
    ((integerp v) (write-to-string v))
    ((floatp v)
     ;; Format floats like Perl's %.15g (Gconvert)
     (cond
       ;; Special float values
       #+sbcl ((sb-ext:float-infinity-p v)
               (if (plusp v) "Inf" "-Inf"))
       #+sbcl ((sb-ext:float-nan-p v) "NaN")
       ((zerop v) "0")
       (t
        ;; Perl's %.15g: use fixed notation when -4 <= exp < 15, else exponential
        (let* ((abs-v (abs v))
               (exp10 (floor (log abs-v (coerce 10 (type-of v))))))
          (if (and (>= exp10 -4) (< exp10 15))
              ;; Fixed notation, %.15g: 15 significant digits total, so the
              ;; number of fraction digits is (15 - 1 - exp10).  Without an
              ;; explicit precision, ~F prints the full round-trip form
              ;; (0.1+0.2 -> 0.30000000000000004 instead of Perl's 0.3).
              (let* ((digits (max 0 (- 14 exp10)))
                     (s (format nil "~,VF" digits v))
                     (c (string-right-trim "." (string-right-trim "0" s))))
                (if (or (string= c "") (string= c "-")) "0" c))
              ;; Exponential notation, %.15g: mantissa to 15 significant
              ;; digits (14 after the point).  ~,14E rounds correctly and
              ;; bumps the exponent when the mantissa rounds up to 10
              ;; (9.999999999999999e15 -> 1e+16), matching Perl.
              (let* ((s (format nil "~,14E" v))
                     ;; Clean up CL exponent notation to Perl format
                     ;; SBCL outputs "1.5d-8" for double, "1.5e-8" for single
                     (s (substitute #\e #\d s :count 1))
                     ;; Split at 'e' and clean mantissa
                     (e-pos (position #\e s)))
                (if e-pos
                    (let* ((mantissa (subseq s 0 e-pos))
                           (exponent-str (subseq s (1+ e-pos)))
                           (exp-val (parse-integer exponent-str))
                           (clean-m (string-right-trim "." (string-right-trim "0" mantissa)))
                           ;; Perl format: e+NN or e-NN (always sign, at least 2 digits)
                           (exp-sign (if (minusp exp-val) "-" "+"))
                           (exp-abs (abs exp-val))
                           (exp-str (if (< exp-abs 10)
                                        (format nil "0~D" exp-abs)
                                        (write-to-string exp-abs))))
                      (format nil "~Ae~A~A" clean-m exp-sign exp-str))
                    ;; Fallback: just strip trailing zeros and dot
                    (string-right-trim "." (string-right-trim "0" s)))))))))
    ((numberp v) (write-to-string v))
    ;; A box in a RAW slot: the same reference box-sv would print, so it must
    ;; print the same word and the same (referent) address — #163.
    ((p-box-p v) (or (%p-ref-string v)
                     (format nil "SCALAR(0x~(~X~))" (object-address v))))
    ((hash-table-p v) (format nil "HASH(0x~(~X~))" (object-address v)))
    ((vectorp v) (format nil "ARRAY(0x~(~X~))" (object-address v)))
    ;; A glob stringifies to its PERL spelling, so both halves must undo the
    ;; case inversion they are stored under — `*plain` printed `*MAIN::PLAIN`
    ;; (#316).  Same restoration `*STDOUT{NAME}` / `{PACKAGE}` already do.
    ((p-typeglob-p v) (format nil "*~A::~A"
                              (%pcl-cl-pkg-to-perl-name (p-typeglob-package v))
                              (%pcl-invert-case (p-typeglob-name v))))
    ;; A raw I/O stream is a lexical filehandle (`open my $fh, …`), which in
    ;; Perl is a GLOB ref — stringify as GLOB(0xADDR), not SBCL's #<fd-stream …>.
    ;; (Named handles reach here as typeglobs, handled above.)
    ((streamp v) (format nil "GLOB(0x~(~X~))" (object-address v)))
    ;; Code reference - stringify as CODE(0xADDR) like Perl
    ((functionp v) (format nil "CODE(0x~(~X~))" (object-address v)))
    ;; Compiled regex (qr//) — stringify as (?^modifiers:pattern) like Perl 5.14+
    ((p-regex-match-p v)
     ;; modifiers is the plist from parse-regex-modifiers, keyed by the upcased
     ;; flag letter (:M :S :I :X ...).  Perl stringifies qr// flags in the fixed
     ;; order m,s,i,x (e.g. qr/x/imsx -> "(?^msix:x)").  (The old code checked
     ;; :case-insensitive etc. — keys that are never present — so flags were
     ;; always dropped.)
     ;; /xx is its own modifier and prints as two x's (task #181): perl gives
     ;; (?^xx:…), and that wrapper is how the xx survives interpolation into a
     ;; bigger pattern — printing one x silently demotes it to /x.
     ;; The body is the SOURCE text, not the cl-ppcre rewrite: perl stringifies
     ;; a qr from what was written.
     (let* ((mods (p-regex-match-modifiers v))
            (mod-str (concatenate 'string
                                  (if (getf mods :m) "m" "")
                                  (if (getf mods :s) "s" "")
                                  (if (getf mods :i) "i" "")
                                  (cond ((getf mods :xx) "xx")
                                        ((getf mods :x)  "x")
                                        (t "")))))
       (format nil "(?^~A:~A)" mod-str (or (p-regex-match-source v)
                                           (p-regex-match-pattern v)))))
    ;; Lists (from return lists, etc.) - join with spaces like Perl's @array interpolation
    ((listp v) (format nil "~{~A~^ ~}" (mapcar #'to-string v)))
    ;; CL's T from comparison operators - Perl true stringifies to "1"
    ((eq v t) "1")
    ;; Super-Unicode character (code > U+10FFFF) — no CL char representation; use U+FFFD
    ((p-superchar-p v) (string #\REPLACEMENT_CHARACTER))
    ;; V-string: the character payload, unless a version object ($^V) carries
    ;; a display string ("v5.30.0").
    ((p-vstring-p v) (or (p-vstring-display v) (p-vstring-s v)))
    (t (format nil "~A" v))))

(defun box-sv (box)
  "Get string value from box with lazy caching.
   Tied variables: bypass cache and call FETCH."
  (let ((inner (p-box-value box)))
    (when (p-tie-proxy-p inner)
      (return-from box-sv
        (to-string (%p-tie-fetch box inner))))
    (when (p-magic-cell-p inner)
      (return-from box-sv (to-string (funcall (p-magic-cell-getter inner))))))
  ;; use overload '""' (stringify): call handler if registered for this class.
  ;; Checked before cache because the handler result IS the string value.
  (let ((handler (p-find-overload box "\"\"")))
    (when handler
      (return-from box-sv
        (to-string (p-call-overload handler box nil nil)))))
  (if (p-box-sv-ok box)
      (p-box-sv box)
      (let* ((inner (p-box-value box))
             ;; Target class first, same rule as p-ref/p-get-class: "$a1"
             ;; after `bless \$a1, "F"` must still stringify with the class
             ;; of what $a1 HOLDS (A=HASH...), not the scalar's own stash
             ;; (bless.t 30); and a plain-scalar payload never shows a
             ;; class at all (join.t 36/40).
             (class (or (%p-target-class box)
                        (and (p-box-class box)
                             (%p-ref-shaped-p inner)
                             (p-box-class box))))
             (raw (cond
                    ;; A typeglob payload is a glob REFERENCE (\*foo) only when the
                    ;; box says so.  `is-ref` is that one discriminator — p-backslash
                    ;; sets it, box-set propagates it, and box-nv has always read it
                    ;; (a glob ref numifies to its address, a bare glob to 0).  Not
                    ;; reading it here made the word and the number disagree (#163's
                    ;; rule): `$g = *foo` printed GLOB(0x1) while numifying to 0, and
                    ;; the raw path already prints the VALUE spelling for the same
                    ;; typeglob (stringify-value, #316).  Blessed glob refs keep
                    ;; C=GLOB(0x…) — a bless leaves is-ref alone.
                    ((p-typeglob-p inner)
                     (if (%p-glob-value-box-p box)
                         (stringify-value inner)
                         (format nil "GLOB(0x~(~X~))" (object-address inner))))
                    ;; A box holding a box is a reference.  Which box is the
                    ;; referent — INNER, or what INNER points at — is decided by
                    ;; %p-ref-referent (is-ref), never by counting levels: BOX is
                    ;; the wrapper itself when `\$x` reaches print/an element/a
                    ;; raw param, and the variable holding one after `my $r = \$x`.
                    ;; Both must print the same word and the same address (#163).
                    ((p-box-p inner)
                     (or (%p-ref-string box)
                         ;; No wrapper under BOX (a single-boxed legacy ref):
                         ;; INNER is the referent.
                         (format nil "SCALAR(0x~(~X~))" (object-address inner))))
                    (t (stringify-value inner))))
             (s (if class
                    (format nil "~A=~A" class raw)
                    raw)))
        ;; Don't cache a SCALAR-reference's string.  Its word is a property of
        ;; the REFERENT's current content — `my $r = \1; my $rr = \$r;` prints
        ;; REF, and `$r = 5` makes the same $rr print SCALAR — and the referent
        ;; is a different box, so writing to it never invalidates this cache.
        ;; box-nv refuses to cache address-based NVs for the same reason.
        (unless (p-box-p inner)
          (setf (p-box-sv box) s
                (p-box-sv-ok box) t))
        s)))

(defmacro p-let (bindings &body body)
  "Perl my declarations - creates boxed variables.
   Usage: (p-let (($x 10) ($y 20)) ...body...)
   Each variable becomes a box that can be referenced with \\$x"
  (let ((box-bindings
         (mapcar (lambda (binding)
                   (if (listp binding)
                       (list (first binding)
                             `(make-p-box ,(second binding)))
                       (list binding '(make-p-box *p-undef*))))
                 bindings)))
    `(let ,box-bindings
       ,@body)))

(defun p-$ (box)
  "Perl scalar dereference $$ref - get value from the referenced box.
   Structure: $ref box -> p-backslash box -> target $x box
   We need to go TWO levels to get the actual value."
  (let ((ref (unbox box)))  ; Get the p-backslash box
    (if (p-box-p ref)
        (let ((target (p-box-value ref)))  ; Get the target $x box
          (if (p-box-p target)
              (p-box-value target)  ; Get the actual value
              target))
        ref)))

(defun (setf p-$) (new-value box)
  "Perl scalar dereference assignment $$ref = val - set value in referenced box.
   Structure: $ref box -> p-backslash box -> target $x box
   We need to go TWO levels to set the actual value."
  (let ((ref (unbox box)))  ; Get the p-backslash box
    (if (p-box-p ref)
        (let ((target (p-box-value ref)))  ; Get the target $x box
          (if (p-box-p target)
              (box-set target new-value)  ; Set the target's value
              (error "Cannot dereference non-reference (target not a box): ~A" target)))
        (error "Cannot dereference non-reference: ~A" ref))))

;;; ============================================================
;;; Value System - Perl's dynamic typing
;;; ============================================================

(defun p-undef (&optional val)
  "Return Perl's undef value, or undefine a variable.
   (p-undef) → undef
   (p-undef @arr) → clear array, return undef
   (p-undef %hash) → clear hash, return undef
   (p-undef $scalar) → set scalar to undef, return undef"
  (when val
    (cond
      ((and (vectorp val) (not (stringp val)))
       ;; `undef @ro` dies in perl — emptying is a size change (task #159)
       (%p-check-array-writable val)
       (setf (fill-pointer val) 0))
      ((hash-table-p val)
       (clrhash val))
      ((p-box-p val)
       (box-set val *p-undef*))
      ;; undef *foo — clear all typeglob slots
      ((p-typeglob-p val)
       (p-glob-undef-name (package-name (p-typeglob-package val))
                          (p-typeglob-name val)))))
  *p-undef*)

;;; Internal predicate — returns CL nil/t (for use in CL if/unless/when/and/or).
;;; Use p-defined for the Perl-value result of defined() expressions.
(defun %pcl-definedp (val)
  (let ((v (unbox val)))
    (and (not (null v)) (not (eq v *p-undef*)))))

(defun p-defined (val)
  "Perl defined() function — returns 1 or \"\" per Perl semantics.
   For CL boolean contexts use %pcl-definedp instead."
  (if (%pcl-definedp val) 1 ""))

(defun %p-true-p-slow (val)
  "Perl truthiness slow path: boxes, undef, aggregates, overloads.

   A BOXED value is a Perl scalar: if it holds a reference (arrayref/hashref/
   coderef/scalarref/typeglob — represented as a raw container or inner box) it
   is ALWAYS true, even when the referent is empty (`my $r=[]; if($r)` is true,
   `if({}` too).  Otherwise normal scalar truthiness applies (0/\"\"/\"0\"/undef false).

   A RAW (non-box) container is a bare @array/%hash used in boolean context, so
   it is true iff non-empty (`if(%h)`/`if(@a)` test element count)."
  ;; use overload "bool": check before unboxing so we have the class info
  (when (p-box-p val)
    (let ((handler (p-find-overload val "bool")))
      (when handler
        (return-from %p-true-p-slow
          (p-true-p (p-call-overload handler val nil nil))))))
  (if (p-box-p val)
      ;; Boxed = Perl scalar.  A held reference is always true.
      (let ((v (unbox val)))
        (cond
          ((eq v *p-undef*) nil)
          ((null v) nil)
          ((or (and (vectorp v) (not (stringp v)))   ; arrayref
               (hash-table-p v)                       ; hashref
               (functionp v)                          ; coderef
               (p-box-p v)                            ; scalarref / ref-to-ref
               (p-typeglob-p v)) t)                   ; globref
          ((and (numberp v) (not (%pcl-nan-p v)) (zerop v)) nil)
          ((and (stringp v) (string= v "")) nil)
          ((and (stringp v) (string= v "0")) nil)
          ((p-vstring-p v) (p-true-p (p-vstring-s v)))  ; v48 = "0" is false
          (t t)))
      ;; Raw value: bare aggregate → count; scalar → normal truthiness.
      (cond
        ((eq val *p-undef*) nil)
        ((null val) nil)
        ((and (numberp val) (not (%pcl-nan-p val)) (zerop val)) nil)
        ((and (stringp val) (string= val "")) nil)
        ((and (stringp val) (string= val "0")) nil)
        ((p-vstring-p val) (p-true-p (p-vstring-s val)))
        ;; bare @array / %hash in boolean context: true iff non-empty
        ((and (vectorp val) (not (stringp val))) (> (length val) 0))
        ((hash-table-p val) (> (%p-hash-user-count val) 0))
        (t t))))

(declaim (inline p-true-p))
(defun p-true-p (val)
  "Perl truthiness with inline fast paths for raw numbers and strings.
   Number: false iff zero (NaN is true).  String: false iff \"\" or \"0\".
   Everything else (boxes, undef, aggregates, overloads) → slow path."
  (cond
    ((numberp val) (not (and (not (%pcl-nan-p val)) (zerop val))))
    ((stringp val) (not (or (string= val "") (string= val "0"))))
    (t (%p-true-p-slow val))))
(declaim (notinline p-true-p))

;;; ============================================================
;;; Arithmetic Operators
;;; ============================================================

(defmacro p-double-inf (&optional negative)
  "Portable +Inf / -Inf double-float literal.  This is the ONLY SBCL-specific
   symbol the transpiler emits into generated code (for overflowing float
   literals such as 1e9999; see Pl/ExprToCL.pm).  Keeping it behind a macro
   gives a future port (or a different CL implementation) a single place to
   change, and expands at compile time so there is no runtime cost."
  (if negative
      'sb-ext:double-float-negative-infinity
      'sb-ext:double-float-positive-infinity))

(defun looks-like-number (str)
  "Check if the ENTIRE string is a valid number (Perl's looks_like_number).
   Returns T only if the whole string (minus whitespace) is numeric."
  (and (stringp str)
       (> (length str) 0)
       (let* ((s (string-trim '(#\Space #\Tab #\Newline #\Return) str))
              (len (length s))
              (pos 0)
              (has-digit nil))
         (when (= len 0) (return-from looks-like-number nil))
         ;; Optional sign
         (when (and (< pos len) (member (char s pos) '(#\+ #\-)))
           (incf pos))
         ;; Digits before dot
         (loop while (and (< pos len) (digit-char-p (char s pos)))
               do (setf has-digit t) (incf pos))
         ;; Optional dot + digits
         (when (and (< pos len) (char= (char s pos) #\.))
           (incf pos)
           (loop while (and (< pos len) (digit-char-p (char s pos)))
                 do (setf has-digit t) (incf pos)))
         ;; Optional exponent
         (when (and (< pos len) has-digit
                    (member (char s pos) '(#\e #\E)))
           (incf pos)
           (when (and (< pos len) (member (char s pos) '(#\+ #\-)))
             (incf pos))
           (loop while (and (< pos len) (digit-char-p (char s pos)))
                 do (incf pos)))
         ;; Must have consumed entire string AND have at least one digit
         (and has-digit (= pos len)))))

;;; use overload — helper macro for binary arithmetic operators.
;;; Each op compiles to an INLINE wrapper whose fast path handles two raw CL
;;; numbers with the native CL op (IEEE Inf/NaN come free — FP traps are
;;; masked once at startup, see file top).  Everything else (boxes, strings,
;;; undef, overloaded objects) takes the out-of-line slow path, which checks
;;; the left operand's overload, then the right (reversed), then coerces.
;;; THE binary-operator overload dispatch (#387 family 23, s413): try the LEFT
;;; operand's `use overload` handler for OP-STR, then the RIGHT one (swapped
;;; = t), else BODY — perl's rule for every binary op.  A macro, so the six
;;; hand-written slow paths (- / % . <=> cmp), p-** and %def-overloaded-arith
;;; expand to exactly the code they used to spell.  A and B are evaluated more
;;; than once — pass variables (every use does).
(defmacro %with-binary-overload ((op-str a b) &body body)
  `(let ((ha (p-find-overload ,a ,op-str)))
     (if ha (p-call-overload ha ,a ,b nil)
         (let ((hb (p-find-overload ,b ,op-str)))
           (if hb (p-call-overload hb ,b ,a t)
               (progn ,@body))))))

(defmacro %def-overloaded-arith (name op-str cl-op)
  (let ((slow (intern (concatenate 'string "%" (symbol-name name) "-SLOW") :pcl)))
    `(progn
       (defun ,slow (a b)
         ,(format nil "Perl ~A slow path: use overload dispatch, then numeric coercion" op-str)
         (%with-binary-overload (,op-str a b)
                                (,cl-op (to-number a) (to-number b))))
       (declaim (inline ,name))
       (defun ,name (a &optional (b nil b-supplied-p))
         ,(format nil "Perl ~A with numberp fast path + use overload dispatch" op-str)
         (if (not b-supplied-p)
             ;; Unary form: e.g. +(expr) — return as-is (no overload for unary +)
             a
             (if (and (numberp a) (numberp b))
                 (,cl-op a b)
                 (,slow a b))))
       ;; expansion stored; plain calls inside the runtime (see file top/end)
       (declaim (notinline ,name)))))

(%def-overloaded-arith p-+ "+" +)
(%def-overloaded-arith p-* "*" *)

(defun %p-neg (a)
  "Perl unary minus slow path.
   Checks 'neg' overload, then applies Perl string-negation rules."
  ;; use overload "neg": unary minus overload
  (let ((h-neg (p-find-overload a "neg")))
    (when h-neg (return-from %p-neg (p-call-overload h-neg a nil nil))))
  ;; No overload: apply Perl string-negation rules
  (let ((val (unbox a)))
    (if (and (stringp val) (> (length val) 0) (not (looks-like-number val)))
        ;; Not a pure number — string operations
        (let ((ch (char val 0)))
          (cond
            ((char= ch #\-) (concatenate 'string "+" (subseq val 1)))
            ((char= ch #\+) (concatenate 'string "-" (subseq val 1)))
            ;; ASCII alpha/underscore: prepend '-'
            ((or (and (alpha-char-p ch) (< (char-code ch) 128)) (char= ch #\_))
             (concatenate 'string "-" val))
            ;; Starts with digit but not pure number (e.g. "12foo"): numeric
            (t (- (to-number a)))))
        ;; Numeric negation
        (- (to-number a)))))

(defun %p---slow (a b)
  "Perl binary subtraction slow path: use overload dispatch, then coercion."
  (%with-binary-overload ("-" a b)
                         (- (to-number a) (to-number b))))

(declaim (inline p--))
(defun p-- (a &optional b)
  "Perl subtraction / unary minus with numberp fast path + overload dispatch."
  (cond
    ((null b) (if (numberp a) (- a) (%p-neg a)))
    ((and (numberp a) (numberp b)) (- a b))
    (t (%p---slow a b))))
(declaim (notinline p--))

(defun %p-/-slow (a b)
  "Perl division slow path: use overload dispatch, then coercion."
  (%with-binary-overload ("/" a b)
                         ;; CL integer/integer -> ratio; Perl gives float for non-integer results.
                         ;; Use (typep r 'ratio) not rationalp: rationalp is true for integers too,
                         ;; so (/ bignum 2) would crash trying to coerce a huge exact-integer to float.
                         (let ((r (/ (to-number a) (to-number b))))
                           (if (typep r 'ratio) (coerce r 'double-float) r))))

(declaim (inline p-/))
(defun p-/ (a b)
  "Perl division with numberp fast path + use overload dispatch"
  (if (and (numberp a) (numberp b))
      (let ((r (/ a b)))
        (if (typep r 'ratio) (coerce r 'double-float) r))
      (%p-/-slow a b)))
(declaim (notinline p-/))

(defun %p-%-slow (a b)
  "Perl modulo slow path with use overload '%' dispatch"
  (%with-binary-overload ("%" a b)
                         (let ((na (to-number a)) (nb (to-number b)))
                           (if (or (%pcl-nan-p na) (%pcl-nan-p nb)
                                   (and (floatp na) (sb-ext:float-infinity-p na))
                                   (and (floatp nb) (sb-ext:float-infinity-p nb))
                                   (zerop nb))
                               (sb-kernel:make-double-float #x7FF80000 0)
                               (mod (truncate na) (truncate nb))))))

(declaim (inline p-%))
(defun p-% (a b)
  "Perl modulo with integer fast path + use overload dispatch"
  (if (and (integerp a) (integerp b) (not (eql b 0)))
      (mod a b)
      (%p-%-slow a b)))
(declaim (notinline p-%))

(defun p-** (a b)
  "Perl exponentiation with use overload '**' dispatch"
  (%with-binary-overload ("**" a b)
                         ;; No overload: numeric path with Inf-on-overflow
                         (let ((na (to-number a))
                               (nb (to-number b)))
                           ;; Return exact bignum when both args are non-negative integers AND the
                           ;; result fits in ~1000 bits.  This matters for pack/unpack: 2**64 as
                           ;; double loses precision.  Guard prevents 9**(9**9) from hanging SBCL.
                           (when (and (integerp na) (integerp nb) (>= nb 0)
                                      (<= (* nb (max 1 (integer-length na))) 1000))
                             (return-from p-** (expt na nb)))
                           (handler-case
                               (expt (coerce na 'double-float) (coerce nb 'double-float))
                             (floating-point-overflow ()
                               (if (and (realp na) (minusp na) (integerp nb) (oddp (truncate nb)))
                                   sb-ext:double-float-negative-infinity
                                   sb-ext:double-float-positive-infinity))))))

(defun p-int (val)
  "Perl int - truncate toward zero. NaN and Inf return unchanged (Perl 5.36+)."
  (let ((n (to-number val)))
    (if (floatp n)
        (if (or (%pcl-nan-p n) (sb-ext:float-infinity-p n))
            n
            (truncate n))
        (truncate n))))

(defun p-abs (val)
  "Perl abs - absolute value"
  (abs (to-number val)))

(defun p-sin (val)
  "Perl sin - sine"
  (let ((n (coerce (to-number val) 'double-float)))
    (when (or (%pcl-nan-p n) (sb-ext:float-infinity-p n))
      (return-from p-sin (sb-kernel:make-double-float #x7FF80000 0)))
    (sin n)))

(defun p-cos (val)
  "Perl cos - cosine"
  (let ((n (coerce (to-number val) 'double-float)))
    (when (or (%pcl-nan-p n) (sb-ext:float-infinity-p n))
      (return-from p-cos (sb-kernel:make-double-float #x7FF80000 0)))
    (cos n)))

(defun p-atan2 (y x)
  "Perl atan2 - arctangent of y/x"
  (atan (coerce (to-number y) 'double-float)
        (coerce (to-number x) 'double-float)))

(defun p-exp (val)
  "Perl exp - e^x"
  (exp (coerce (to-number val) 'double-float)))

(defun p-log (val)
  "Perl log - natural logarithm"
  (let ((n (to-number val)))
    (when (%pcl-nan-p n) (return-from p-log n))
    (when (zerop n)
      (error "Can't take log of 0"))
    (log (coerce n 'double-float))))

(defun p-sqrt (val)
  "Perl sqrt - square root"
  (let ((n (to-number val)))
    (when (%pcl-nan-p n) (return-from p-sqrt n))
    (when (minusp n)
      (error "Can't take sqrt of ~A" n))
    (sqrt (coerce n 'double-float))))

;;; Perl's OWN drand48 (perl 5.20+, [perl #115928]): since that change perl
;;; ships its implementation rather than the platform's, so a given seed
;;; replays the same sequence everywhere — and t/op/rand.t asserts the exact
;;; values (`srand(1); int rand(1000)` == 41).  Reproducing the algorithm is
;;; the only way to match, and it is worth matching: seeded rand is how test
;;; suites and CPAN modules get deterministic runs.
;;;   X(n+1) = (0x5DEECE66D * X(n) + 0xB) mod 2^48,  seed init X = (s<<16)|0x330E
(defconstant +p-drand48-mult+ #x5deece66d)
(defconstant +p-drand48-add+  #xb)
(defconstant +p-drand48-mask+ (ash 1 48))
(defvar *p-drand48-state* (logior (ash 0 16) #x330e)
  "48-bit drand48 state; seeded by p-srand, advanced by p-rand.")

(defun %p-drand48 ()
  "One drand48 draw in [0,1)."
  (setf *p-drand48-state*
        (mod (+ (* +p-drand48-mult+ *p-drand48-state*) +p-drand48-add+)
             +p-drand48-mask+))
  (/ (float *p-drand48-state* 1.0d0) +p-drand48-mask+))

(defun p-rand (&optional max)
  "Perl rand - random number in [0, EXPR).
   perlfunc: \"If EXPR is omitted or zero, uses 1\" — a supplied 0 is NOT a
   zero range (op/rand.t t260 'rand() without args is rand(1)').  Negative
   EXPR is legal and yields a negative result, so the multiply is done on a
   [0,1) draw rather than calling (random EXPR), which needs a positive bound."
  (let ((m (if max (to-number max) 1)))
    (when (zerop m) (setf m 1))
    (* (%p-drand48) m)))

(defun p-srand (&optional seed)
  "Perl srand - seed the RNG, returning the seed.
   Previously a no-op that DISCARDED the seed, so `srand($n); rand` was not
   reproducible at all (op/rand.t t262/t263)."
  (let ((s (if seed (%pcl-to-integer (to-number seed)) (get-universal-time))))
    (setf *p-drand48-state* (logior (ash (logand s #xffffffff) 16) #x330e))
    s))

(defun %to-number-raw (val)
  "Convert a raw non-number, non-box value to number (Perl semantics)."
  (cond
    ((eq val *p-undef*) 0)
    ((null val) 0)
    ;; CL's T from comparison operators - Perl true numifies to 1
    ((eq val t) 1)
    ((stringp val) (parse-perl-number val))
    ((p-vstring-p val) (parse-perl-number (p-vstring-s val)))
    ;; Adjustable vector = Perl @array in scalar context → array length
    ((and (vectorp val) (adjustable-array-p val)) (length val))
    ;; Perl 5.26+: plain %hash in numeric context → key count
    ((hash-table-p val) (%p-hash-user-count val))
    ;; Compiled regex in numeric context → object address (like a reference)
    ((p-regex-match-p val) (object-address val))
    ;; A code ref is a RAW function (no wrapper box, unlike \$x \@a \%h), so
    ;; it reaches this path whenever it sits in a raw slot: `my $r2 = \&f`
    ;; frozen to a raw-numeric slot by type flow (its only use was `==`).
    ;; Its numeric value is its address, exactly as box-nv answers for the
    ;; boxed copy — the missing arm made `\&f == \&f` FALSE (task #362: the
    ;; boxed side numified to the address, this side to 0).
    ((functionp val) (object-address val))
    (t 0)))

(declaim (inline to-number))
(defun to-number (val)
  "Convert value to number (Perl semantics).
   Inline numberp fast path; lazy caching for boxed values."
  (cond
    ((numberp val) val)
    ((p-box-p val) (box-nv val))
    (t (%to-number-raw val))))
(declaim (notinline to-number))

;;; ============================================================
;;; String Operators
;;; ============================================================

(defun %p-.-slow (a b)
  "Perl string concatenation slow path with use overload '.' dispatch."
  (%with-binary-overload ("." a b)
                         (concatenate 'string (to-string a) (to-string b))))

(declaim (inline p-.))
(defun p-. (a b)
  "Perl string concatenation operator (.) with stringp fast path."
  (if (and (stringp a) (stringp b))
      (concatenate 'string a b)
      (%p-.-slow a b)))
(declaim (notinline p-.))

(defun %pcl-dot-overloaded-p (v)
  "True when V is a box whose class registers a '.' overload handler."
  (and (p-box-p v) (p-find-overload v ".") t))

(defun p-string-concat (&rest args)
  "Perl string concatenation for string interpolation (\"$a $b\").
   perl spells \"a $o b\" as 'a ' . $o . ' b', so a '.' overload
   participates and the result need not be a string ([perl #124160],
   task #402; probed: perl's multiconcat calls the handler once per
   object piece, left to right, with the reversed flag, and a handler's
   plain-string result makes the remaining concats plain).  When a piece
   carries a '.' handler, fold left through p-. -- exactly the spelled-out
   concat.  A SINGLE piece is a stringification, never a concat (\"$o\"
   uses '\"\"' alone), and pieces without a '.' handler keep the
   all-at-once fast path, whose to-string runs the '\"\"' overload via
   box-sv -- which is perl's fallback for '.' as well."
  (if (and (cdr args) (some #'%pcl-dot-overloaded-p args))
      (reduce #'p-. args)
      (apply #'concatenate 'string (mapcar #'to-string args))))

(defun p-str-x (str count)
  "Perl string repetition operator (x).
   If str is an array (adjustable vector), uses its length (scalar context)."
  (let* ((v (unbox str))
         ;; If it's an adjustable array (Perl @array), use its length
         ;; Regular strings are also vectors in CL, so check adjustable-array-p
         (s (if (and (vectorp v) (not (stringp v)) (adjustable-array-p v))
                (write-to-string (length v))
                (to-string str)))
         (nc (to-number count))
         (n (if (and (floatp nc)
                     (or (sb-ext:float-infinity-p nc) (sb-ext:float-nan-p nc)))
                0
                (truncate nc))))
    (if (<= n 0)
        ""
        (apply #'concatenate 'string (make-list n :initial-element s)))))

(defun flatten-list-elements (val)
  "Flatten a value into a list of elements for list repeat.
   Nested arrays/vectors are flattened one level deep."
  (cond
    ;; Adjustable vector (Perl @array) - flatten its contents
    ((and (vectorp val) (not (stringp val)) (adjustable-array-p val))
     (loop for elem across val
           append (if (and (vectorp elem) (not (stringp elem)))
                      (coerce elem 'list)
                      (list elem))))
    ;; Regular vector (created by (vector ...)) - flatten its contents
    ((and (vectorp val) (not (stringp val)))
     (loop for elem across val
           append (if (and (vectorp elem) (not (stringp elem)))
                      (coerce elem 'list)
                      (list elem))))
    ;; List - flatten nested vectors
    ((listp val)
     (loop for elem in val
           append (if (and (vectorp elem) (not (stringp elem)))
                      (coerce elem 'list)
                      (list elem))))
    ;; Single value - wrap in list
    (t (list val))))

(defun p-list-x (list-val count)
  "Perl list repetition operator (x) for parenthesized expressions.
   Repeats the list N times: (1,2,3) x 2 -> (1,2,3,1,2,3).
   Flattens nested arrays: (@x) x 2 where @x=(1,2,3) -> (1,2,3,1,2,3).
   Returns an adjustable vector (Perl array)."
  (let* ((n (truncate (to-number count)))
         ;; Normalize input to a flat list of elements, flattening nested arrays
         (elements (flatten-list-elements list-val)))
    (if (<= n 0)
        ;; Return empty array
        (make-array 0 :adjustable t :fill-pointer 0)
        ;; Repeat the list n times and return as adjustable vector
        (let ((result-list (loop repeat n append elements)))
          (make-array (length result-list)
                      :adjustable t
                      :fill-pointer t
                      :initial-contents result-list)))))

(declaim (inline to-string))
(defun to-string (val)
  "Convert value to string (Perl semantics).
   Inline stringp fast path; lazy caching for boxed values."
  (cond
    ((stringp val) val)
    ((p-box-p val) (box-sv val))
    ;; Raw value - convert directly
    (t (stringify-value val))))
(declaim (notinline to-string))

;;; Strict eager coercion for raw-numeric / raw-string slot writes
;;; (docs/raw-numeric-verdict.md §Checked coercion, task #62 step 2).
;;; Writes are rare, uses are hot: the check runs once per write so every
;;; use of the slot stays an unconditional raw read.

(defun %pcl-dualvar-p (v)
  "True when V is a box carrying a GENUINE dualvar: both caches valid and the
   numeric side is NOT what numifying the string side would give ($!-family).
   Ordinary cache-warm boxes have consistent caches and return NIL.  Shared
   definition for Scalar::Util::isdual and the strict raw-slot coercers."
  (and (p-box-p v)
       (p-box-sv-ok v)
       (p-box-nv-ok v)
       (let ((n (ignore-errors (to-number (p-box-sv v)))))
         (and n (/= (p-box-nv v) n)))))

(defun %pcl-raw-coerce-check (v name kind)
  "Die loudly when eagerly coercing V for raw slot NAME would lose behavior:
   (1) an overload-capable blessed ref (a \"\" or 0+ handler) — per-use code
   the frozen value cannot run (catches use-overload loaded by a string eval
   AFTER the transpile-time corpus scan); (2) a genuine dualvar — coercion
   irreversibly drops the other side, so a use-classifier bug would corrupt
   silently.  Never weaken this check; a firing die means fix the classifier
   or re-box the variable (raw-numeric-verdict.md)."
  (when (or (p-find-overload v "\"\"") (p-find-overload v "0+"))
    (error "PCL raw-~a slot ~a: value has use-overload conversion handlers; re-box the variable"
           kind name))
  (when (%pcl-dualvar-p v)
    (error "PCL raw-~a slot ~a: genuine dualvar reached an eager coercion"
           kind name))
  v)

(defun %pcl-scalar-collapse (v)
  "Scalar-assignment context for a RAW aggregate value, mirroring box-set's
   rules: an unwrapped adjustable vector (an @array rvalue / aassign result,
   e.g. `my $n = @a = split ...`) becomes its element count; an unwrapped
   hash-table its user key count.  Boxes (array/hash REFS) and every other
   value pass through untouched."
  (cond
    ((and (vectorp v) (not (stringp v)) (adjustable-array-p v)) (length v))
    ((hash-table-p v) (%p-hash-user-count v))
    (t v)))

(defun %pcl-to-number-strict (v name)
  "Eager numeric freeze for a raw-numeric slot write (the compile-time
   equivalent of the user writing `+ 0`); strict per %pcl-raw-coerce-check.
   Applies box-set's scalar-assignment aggregate collapse first, so the
   wrapper stays equivalent to the boxed write it replaces."
  (to-number (%pcl-raw-coerce-check (%pcl-scalar-collapse v) name "numeric")))

(defun %pcl-superchar-payload (v)
  "V's payload when V IS a super-Unicode character — `chr(N)` for
   N > U+10FFFF, which has no CL character and is carried as a p-superchar
   struct — either raw or inside a plain box; NIL otherwise.  Looks through
   the box WITHOUT unbox's tie/magic dispatch on purpose: a tie proxy or a
   magic cell is never a superchar, and running FETCH here would run it
   twice for one write."
  (let ((u (if (p-box-p v) (p-box-value v) v)))
    (and (p-superchar-p u) u)))

(defun %pcl-to-string-strict (v name)
  "Eager string freeze for a raw-string slot write (`. \"\"`); strict per
   %pcl-raw-coerce-check.  Aggregate collapse as in %pcl-to-number-strict.

   A super-Unicode character (`chr(N)`, N > U+10FFFF) passes through
   UNFROZEN (task #442).  It is the one payload whose CL string form is
   LOSSY — to-string collapses it to U+FFFD, so freezing it here made the
   raw-string slot answer `ord` 65533 where the general-form compiler
   (PCL_OPT=none) answers N, which is perl's answer: two answers for one
   value, chosen by an optimizer verdict.  Every consumer of a raw-string
   slot goes through to-string, so the U+FFFD collapse still happens — at
   the same point the boxed path makes it, and nowhere earlier.  The rule
   is the registry's own contract: the optimized emission must RUN
   identically to the general form."
  (let ((c (%pcl-raw-coerce-check (%pcl-scalar-collapse v) name "string")))
    (or (%pcl-superchar-payload c) (to-string c))))

;;; S1 str-buffer raw slots (task #62, docs/raw-numeric-verdict.md §S1):
;;; an accumulator whose only writes are plain roots + `.=` and whose every
;;; use is a transient stringify/boolean read holds an adjustable
;;; fill-pointer string, so `.=` appends in place (O(1) amortized) instead
;;; of allocating a fresh concatenation (O(n) per append — the strcat bench
;;; tax).  The verdict guarantees the buffer object never escapes into a
;;; retaining site (no hash-key use, no opaque flow), so in-place mutation
;;; is unobservable.  All standard string ops respect the fill-pointer.

(defun %pcl-str-buffer (v)
  "Fresh adjustable fill-pointer buffer holding V's string value — the
   plain-write store discipline for a str-buffer slot (each `$s = V;`
   REPLACES the buffer, so stale aliases cannot exist)."
  (let* ((s (to-string v))
         (n (length s))
         (buf (make-array (max n 16) :element-type 'character
                          :adjustable t :fill-pointer n)))
    (replace buf s)
    buf))

(defun %pcl-str-append (buf v)
  "In-place `$s .= V` on a str-buffer slot: extend and copy V's string
   value after the fill pointer.  Returns the buffer (the compound assign's
   value, like the boxed macro returns the variable's new value).
   Self-append (`$s .= $s`) is safe: the source length is captured first
   and the copied-from region [0,n) never overlaps the destination [n,2n)."
  (let* ((s (to-string v))
         (n (length s))
         (start (fill-pointer buf)))
    (when (> (+ start n) (array-total-size buf))
      (adjust-array buf (max (+ start n) (* 2 (array-total-size buf)))))
    (setf (fill-pointer buf) (+ start n))
    (replace buf s :start1 start)
    buf))

(defun p-length (val)
  "Perl length function - returns undef for undef input.
   Stringifies via to-string on the original (boxed) value so that a blessed
   object's overloaded '' handler fires (e.g. length($obj) on an object that
   overloads stringification), rather than measuring the raw ref text."
  (let ((v (unbox val)))
    (if (or (eq v *p-undef*) (null v))
        *p-undef*
        (length (to-string val)))))

(defun p-substr (str start &optional len replacement)
  "Perl substr function.
   2-3 args: extract substring.
   4 args: replace in place (if str is a box), return replaced portion.
   Negative start: count from end. Negative length: stop that many chars before end."
  (let* ((s (to-string str))
         (slen (length s))
         (raw-st (truncate (to-number start)))
         ;; Adjusted start (without clamping) for bounds checking
         (adj-st (if (< raw-st 0) (+ slen raw-st) raw-st))
         ;; Detect explicitly undefined len: warn, but treat as 0 (Perl behaviour)
         (undef-len-p (and len (not (%pcl-definedp len))))
         (ln-raw (if len (truncate (to-number len)) nil))
         ;; Bounds check: warn for read, die for write.
         ;; Rule: OOB when start is past end of string, OR (with len given) when
         ;; the entire requested region falls before the start of the string.
         (oob (or (> adj-st slen)
                  (and ln-raw
                       (if (< ln-raw 0)
                           ;; Negative len: region is [adj-st .. slen+ln]. OOB when
                           ;; both endpoints are before string start.
                           (and (< adj-st 0) (< (+ slen ln-raw) 0))
                           ;; Positive/zero len: region is [adj-st .. adj-st+ln]. OOB
                           ;; when endpoint is before string start.
                           (< (+ adj-st ln-raw) 0)))))
         ;; Clamp start to valid range for actual extraction
         (st (max 0 (min adj-st slen)))
         ;; Calculate end position, handling negative length.
         ;; Use adj-st (unclamped) so that e.g. substr('54321',-7,4)
         ;; correctly gives end = max(0,-2+4) = 2, not min(0+4,5) = 4.
         (end-pos (cond ((null ln-raw) slen)
                        ((< ln-raw 0) (max st (+ slen ln-raw)))
                        (t (max 0 (min (+ adj-st ln-raw) slen))))))
    (when undef-len-p
      (p-warn (format nil "Use of uninitialized value in substr~%")))
    (when oob
      (if replacement
          (error "substr outside of string")
          (p-warn (format nil "substr outside of string~%"))))
    (if replacement
        ;; 4-arg form (or lvalue): replace and return the replaced portion
        (let* (;; Warn when target is a reference being coerced to string —
               ;; but not when its class has a "" overload (perl uses the
               ;; overloaded stringification silently; substr.t UTF8ness test)
               (_ (when (p-box-p str)
                    (let ((v (p-box-value str)))
                      (when (and (or (and (vectorp v) (not (stringp v)))
                                     (hash-table-p v)
                                     (functionp v))
                                 (not (p-find-overload str "\"\"")))
                        (p-warn (format nil "Attempt to use reference as lvalue in substr~%"))))))
               (replaced-part (subseq s (min st slen) end-pos))
               (new-str (concatenate 'string
                                     (subseq s 0 (min st slen))
                                     (to-string replacement)
                                     (subseq s end-pos))))
          (declare (ignore _))
          ;; Modify in place if str is a box
          (when (p-box-p str)
            (box-set str new-str))
          replaced-part)
        ;; 2 or 3 arg form: extract
        (subseq s (min st slen) end-pos))))

(defun p-lc (str)
  "Perl lc - lowercase"
  (string-downcase (to-string str)))

(defun p-uc (str)
  "Perl uc - uppercase"
  (string-upcase (to-string str)))

(defun p-fc (str)
  "Perl fc - fold case for case-insensitive comparison.
   Uses string-downcase as approximation (full Unicode folding would need ICU)."
  (string-downcase (to-string str)))

(defun p-chomp-single (s)
  "Chomp a single string, returns (new-string . removed-count).
   Removes trailing $/ (input record separator)."
  (let* ((sep (get-input-record-separator))
         (len (length s)))
    (cond
      ;; $/ = undef (slurp mode): chomp does nothing
      ((null sep) (cons s 0))
      ;; $/ = \N (record mode): $/ is a ref, so chomp removes nothing
      ((integerp sep) (cons s 0))
      ;; $/ = "" (paragraph mode): remove all trailing newlines
      ((string= sep "")
       (let ((end len))
         (loop while (and (> end 0) (char= (char s (1- end)) #\Newline))
               do (decf end))
         (if (= end len)
             (cons s 0)
             (cons (subseq s 0 end) (- len end)))))
      ;; Single character separator (common case)
      ((= (length sep) 1)
       (if (and (> len 0) (char= (char s (1- len)) (char sep 0)))
           (cons (subseq s 0 (1- len)) 1)
           (cons s 0)))
      ;; Multi-character separator
      (t
       (let ((sep-len (length sep)))
         (if (and (>= len sep-len)
                  (string= s sep :start1 (- len sep-len)))
             (cons (subseq s 0 (- len sep-len)) sep-len)
             (cons s 0)))))))

(defun p-chomp-one (var)
  "Chomp a single variable (helper for p-chomp)."
  (cond
    ;; Box: chomp its value
    ((p-box-p var)
     (let* ((s (to-string (p-box-value var)))
            (result (p-chomp-single s)))
       (when (> (cdr result) 0)
         (setf (p-box-value var) (car result)
               (p-box-sv-ok var) nil))
       (cdr result)))
    ;; Vector (array): chomp each element in place
    ((and (vectorp var) (not (stringp var)))
     (let ((total-removed 0))
       (dotimes (i (length var))
         (let* ((elem (aref var i))
                (s (if (p-box-p elem)
                       (to-string (p-box-value elem))
                       (to-string elem)))
                (result (p-chomp-single s)))
           (when (> (cdr result) 0)
             (if (p-box-p elem)
                 (setf (p-box-value elem) (car result)
                       (p-box-sv-ok elem) nil)
                 (setf (aref var i) (car result)))
             (incf total-removed (cdr result)))))
       total-removed))
    ;; List: chomp each element (must be boxes)
    ((listp var)
     (let ((total-removed 0))
       (dolist (elem var)
         (when (p-box-p elem)
           (let* ((s (to-string (p-box-value elem)))
                  (result (p-chomp-single s)))
             (when (> (cdr result) 0)
               (setf (p-box-value elem) (car result)
                     (p-box-sv-ok elem) nil)
               (incf total-removed (cdr result))))))
       total-removed))
    ;; Non-modifiable: return 0
    (t 0)))

(defun p-chomp (&rest vars)
  "Perl chomp - remove trailing newline, modifies variable(s) in place.
   Returns total number of characters removed.
   Handles multiple arguments: chomp($x, @arr) chomps all."
  (let ((total 0))
    (dolist (var vars total)
      (incf total (p-chomp-one var)))))

(defun p-chop-single (s)
  "Chop a single string, returns (new-string . removed-char)"
  (let ((len (length s)))
    (if (> len 0)
        (cons (subseq s 0 (1- len)) (subseq s (1- len)))
        (cons "" ""))))

(defun p-chop-one (var)
  "Chop a single variable (helper for p-chop)."
  (cond
    ;; Box: chop its value
    ((p-box-p var)
     (let* ((s (to-string (p-box-value var)))
            (result (p-chop-single s)))
       (setf (p-box-value var) (car result)
             (p-box-sv-ok var) nil)
       (cdr result)))
    ;; Vector (array): chop each element in place
    ((and (vectorp var) (not (stringp var)))
     (let ((last-removed ""))
       (dotimes (i (length var))
         (let* ((elem (aref var i))
                (s (if (p-box-p elem)
                       (to-string (p-box-value elem))
                       (to-string elem)))
                (result (p-chop-single s)))
           (if (p-box-p elem)
               (progn
                 (setf (p-box-value elem) (car result)
                       (p-box-sv-ok elem) nil))
               (setf (aref var i) (car result)))
           (setf last-removed (cdr result))))
       last-removed))
    ;; List: chop each element (must be boxes)
    ((listp var)
     (let ((last-removed ""))
       (dolist (elem var)
         (when (p-box-p elem)
           (let* ((s (to-string (p-box-value elem)))
                  (result (p-chop-single s)))
             (setf (p-box-value elem) (car result)
                   (p-box-sv-ok elem) nil
                   last-removed (cdr result)))))
       last-removed))
    ;; Non-modifiable: return empty string
    (t "")))

(defun p-chop (&rest vars)
  "Perl chop - remove last character, modifies variable(s) in place.
   Returns the removed character from the last processed value.
   Handles multiple arguments: chop($x, @arr) chops all."
  (let ((last-removed ""))
    (dolist (var vars last-removed)
      (setf last-removed (p-chop-one var)))))

(defun p-index (str substr &optional start)
  "Perl index - find substring.
   Negative start position is treated as 0.
   For empty substring, start is clamped to string length.
   For non-empty substring, start beyond string length returns -1."
  (let* ((s (to-string str))
         (sub (to-string substr))
         (slen (length s))
         (start-pos (if start (max 0 (truncate (to-number start))) 0)))
    (cond
      ;; Empty substring: return min(start, length) - Perl clamps to end
      ((zerop (length sub))
       (min start-pos slen))
      ;; Start beyond string length: not found
      ((> start-pos slen) -1)
      (t
       (let ((pos (search sub s :start2 start-pos)))
         (or pos -1))))))

(defun p-rindex (str substr &optional start)
  "Perl rindex - find substring from end.
   Negative start position returns -1 for non-empty substr.
   For empty substr, negative position is clamped to 0 (Perl returns 0).
   Position beyond string length is clamped to string length."
  (let* ((s (to-string str))
         (sub (to-string substr))
         (slen (length s))
         (start-num (if start (truncate (to-number start)) nil)))
    (cond
      ;; Empty substring: clamp position to [0, slen] — even negative positions yield 0
      ((zerop (length sub))
       (if start-num
           (max 0 (min start-num slen))
           slen))
      ;; Negative position returns -1 (for non-empty substrings)
      ((and start-num (< start-num 0)) -1)
      ;; Normal case: search from end
      (t (let* ((end-pos (if start-num
                             (min (+ start-num (length sub)) slen)
                             nil))
                (pos (search sub s :from-end t :end2 end-pos)))
           (or pos -1))))))

(defun p-version-string (&rest code-points)
  "Build a Perl v-string (v1.20.300) from integer code points.
   Each code point becomes a character; the result is a p-vstring struct so a
   scalar assigned from it answers ref \\$v = \"VSTRING\" until overwritten
   (to-string / to-number / truthiness all read the payload)."
  (make-p-vstring
   :s (coerce (mapcar (lambda (n)
                        (let ((c (truncate (if (typep n 'number) n (to-number n)))))
                          (if (or (< c 0) (> c #x10FFFF))
                              #\REPLACEMENT_CHARACTER
                              (code-char c))))
                      code-points)
              'string)))

;;; Represents a Perl string whose single character has a code point > U+10FFFF.
;;; CL characters are limited to 0–U+10FFFF; this struct carries the raw integer
;;; so that (ord (chr N)) round-trips correctly for super-Unicode code points.
(defstruct p-superchar
  (code 0 :type integer))

(defun p-chr (n)
  "Perl chr - character from code point."
  (let ((num (to-number n)))
    (when (floatp num)
      (when #+sbcl (sb-ext:float-infinity-p num) #-sbcl nil
            (error "Cannot chr ~A" (to-string n)))
      (when #+sbcl (sb-ext:float-nan-p num) #-sbcl nil
            (error "Cannot chr ~A" (to-string n))))
    (if (< num 0)
        (string #\REPLACEMENT_CHARACTER)                   ; negative → U+FFFD
        (let ((code (truncate num)))
          (cond
            ((> code #x10FFFF) (make-p-superchar :code code)) ; super-Unicode → struct
            (t (string (code-char code))))))))

(defun p-ord (str)
  "Perl ord - code point of first character"
  (let ((v (unbox str)))
    ;; Super-Unicode character stored as p-superchar struct (code > U+10FFFF)
    (if (p-superchar-p v)
        (p-superchar-code v)
        (let ((s (to-string str)))
          (if (> (length s) 0)
              (char-code (char s 0))
              0)))))

(defun %strip-underscores (s)
  "Remove underscores from a numeric string (Perl allows _ as visual separator)"
  (remove #\_ s))

(defun %check-wide-chars (s fname)
  "Signal error if string contains wide characters (code point > 255)."
  (loop for c across s
        when (> (char-code c) 255)
        do (error "Wide character in ~A" fname)))

(defun p-hex (str)
  "Perl hex - convert hex string to number.
   Accepts: '0xCAFE', '0XCAFE', 'xCAFE', 'XCAFE', 'CAFE', 'ca_fe'"
  (let* ((s (string-trim '(#\Space #\Tab) (to-string str))))
    (%check-wide-chars s "hex")
    (let ((s (cond
               ;; Strip 0x/0X prefix
               ((and (>= (length s) 2)
                     (char= (char s 0) #\0)
                     (member (char s 1) '(#\x #\X)))
                (subseq s 2))
               ;; Strip bare x/X prefix
               ((and (>= (length s) 1)
                     (member (char s 0) '(#\x #\X)))
                (subseq s 1))
               (t s))))
      (or (parse-integer (%strip-underscores s) :radix 16 :junk-allowed t) 0))))

(defun p-oct (str)
  "Perl oct - convert octal/hex/binary string to number.
   Recognizes prefixes: 0x/0X (hex), 0b/0B (binary), 0o/0O (octal), 0 (octal).
   Also handles bare x/X, b/B, o/O prefixes."
  (let ((s (string-trim '(#\Space #\Tab) (to-string str))))
    (%check-wide-chars s "oct")
    (cond
      ;; 0x / 0X -> hex
      ((and (>= (length s) 2) (char= (char s 0) #\0)
            (member (char s 1) '(#\x #\X)))
       (or (parse-integer (%strip-underscores (subseq s 2)) :radix 16 :junk-allowed t) 0))
      ;; 0b / 0B -> binary
      ((and (>= (length s) 2) (char= (char s 0) #\0)
            (member (char s 1) '(#\b #\B)))
       (or (parse-integer (%strip-underscores (subseq s 2)) :radix 2 :junk-allowed t) 0))
      ;; 0o / 0O -> octal (Perl 5.34+)
      ((and (>= (length s) 2) (char= (char s 0) #\0)
            (member (char s 1) '(#\o #\O)))
       (or (parse-integer (%strip-underscores (subseq s 2)) :radix 8 :junk-allowed t) 0))
      ;; 0... -> octal
      ((and (>= (length s) 1) (char= (char s 0) #\0))
       (or (parse-integer (%strip-underscores s) :radix 8 :junk-allowed t) 0))
      ;; bare x/X -> hex (Perl extension)
      ((and (>= (length s) 1) (member (char s 0) '(#\x #\X)))
       (or (parse-integer (%strip-underscores (subseq s 1)) :radix 16 :junk-allowed t) 0))
      ;; bare b/B -> binary (Perl extension)
      ((and (>= (length s) 1) (member (char s 0) '(#\b #\B)))
       (or (parse-integer (%strip-underscores (subseq s 1)) :radix 2 :junk-allowed t) 0))
      ;; bare o/O -> octal (Perl extension)
      ((and (>= (length s) 1) (member (char s 0) '(#\o #\O)))
       (or (parse-integer (%strip-underscores (subseq s 1)) :radix 8 :junk-allowed t) 0))
      ;; default -> octal
      (t (or (parse-integer (%strip-underscores s) :radix 8 :junk-allowed t) 0)))))

(defun p-lcfirst (str)
  "Perl lcfirst - lowercase first character"
  (let ((s (to-string str)))
    (if (> (length s) 0)
        (concatenate 'string (string-downcase (subseq s 0 1)) (subseq s 1))
        s)))

(defun p-ucfirst (str)
  "Perl ucfirst - uppercase first character"
  (let ((s (to-string str)))
    (if (> (length s) 0)
        (concatenate 'string (string-upcase (subseq s 0 1)) (subseq s 1))
        s)))

(defun p-quotemeta (str)
  "Perl quotemeta - escape non-word characters.
   For ASCII (code < 128): escape unless [A-Za-z0-9_].
   For non-ASCII (code >= 128): escape unless Unicode alphanumeric (\\w)."
  (let ((s (to-string str)))
    (with-output-to-string (out)
      (loop for c across s
            for code = (char-code c)
            do (let ((escapep (if (< code 128)
                                  (not (or (alphanumericp c) (char= c #\_)))
                                  (not (alphanumericp c)))))
                 (when escapep (write-char #\\ out))
                 (write-char c out))))))

;;; crypt(3) — one-way password hashing via the system C library.
;;; Perl's crypt() is a thin wrapper over the C crypt(3); we call the same
;;; function, so output is byte-identical to Perl on the same platform
;;; (DES with a 2-char salt, or glibc $1$/$5$/$6$ etc. by salt prefix).
(defvar *p-crypt-available* nil
  "T if the system crypt(3) could be resolved at load time.")

(eval-when (:load-toplevel :execute)
  ;; glibc 2.39+ split crypt(3) out of libc into libcrypt; load it if present.
  (when (ignore-errors (sb-alien:load-shared-object "libcrypt.so.1") t)
    (setf *p-crypt-available* t)))

(sb-alien:define-alien-routine ("crypt" %c-crypt)
    (sb-alien:c-string :external-format :latin-1)
  ;; crypt(3) operates on bytes; pass latin-1 so codepoints 0-255 map 1:1.
  (key  (sb-alien:c-string :external-format :latin-1))
  (salt (sb-alien:c-string :external-format :latin-1)))

(defun p-crypt (plaintext salt)
  "Perl crypt(PLAINTEXT, SALT): one-way hash via the system crypt(3).
   Dies on wide characters (codepoint > 255), like Perl.  Returns undef when
   crypt(3) returns NULL (e.g. FIPS rejecting a weak algorithm)."
  (let ((pt (to-string plaintext))
        (sl (to-string salt)))
    (when (or (find-if (lambda (c) (> (char-code c) 255)) pt)
              (find-if (lambda (c) (> (char-code c) 255)) sl))
      (p-die "Wide character in crypt"))
    (unless *p-crypt-available*
      (p-die "The crypt() function is unimplemented due to excessive paranoia."))
    (let ((result (%c-crypt pt sl)))
      (if result result *p-undef*))))

(defun p-pos (var &optional new-pos)
  "Perl pos - get/set match position for /g regex.
   With one arg, returns current position (or nil).
   With two args, sets position and returns new-pos."
  (if new-pos
      ;; Setter: pos($str) = N
      (if (p-box-p var)
          (setf (gethash var *p-match-pos*) (truncate (to-number new-pos)))
          new-pos)
      ;; Getter: pos($str).  Return the canonical undef marker (not raw CL nil)
      ;; when there is no recorded position, so the value survives Perl list
      ;; flattening — a raw nil spread into @_ or an assigned list is treated as
      ;; the empty list and silently dropped (e.g. `is(pos($s), undef, $name)`
      ;; would lose an argument).  *p-undef* is still false under p-true-p and
      ;; undefined under p-defined.
      (if (p-box-p var)
          (or (gethash var *p-match-pos*) *p-undef*)
          *p-undef*)))

(defun sprintf-inf-nan-p (num)
  "Check if num is infinity or NaN. Returns :pos-inf, :neg-inf, :nan, or nil."
  #+sbcl
  (cond
    ((sb-ext:float-nan-p num) :nan)
    ((sb-ext:float-infinity-p num)
     (if (plusp num) :pos-inf :neg-inf))
    (t nil))
  #-sbcl nil)

(defun sprintf-format-int (num base upper-case-p alt-form-p)
  "Format integer in given base. Returns string without sign (abs value).
   ALT-FORM-P adds 0x/0o/0b prefix for bases 16/8/2."
  (let* ((abs-num (abs num))
         (digits (if (zerop abs-num)
                     "0"
                     (let ((chars nil))
                       (loop while (plusp abs-num) do
                             (let ((digit (mod abs-num base)))
                               (push (char (if upper-case-p "0123456789ABCDEF" "0123456789abcdef") digit)
                                     chars)
                               (setf abs-num (floor abs-num base))))
                       (coerce chars 'string))))
         (prefix (if alt-form-p
                     (case base
                       (16 (if upper-case-p "0X" "0x"))
                       (8 (if (string= digits "0") "" "0"))
                       (2 (if upper-case-p "0B" "0b"))
                       (t ""))
                     "")))
    (concatenate 'string prefix digits)))

(defun sprintf-format-float-f (num precision)
  "Format float as fixed-point with given precision (default 6).
   Precision 0 means no decimal point (Perl: sprintf '%.0f', 0 => '0', not '0.').
   Rounds half-to-EVEN on the EXACT value of the double, matching C/Perl printf
   %f.  (CL's ~F — and scaling a float before ROUND — round half-AWAY-from-zero,
   so '%.0f' of 2.5 gave 3 and of 0.5 gave 1; C/Perl give 2 and 0.)  Using
   (rational num) makes the scale-by-10^prec exact, so ROUND — which is itself
   round-half-to-even — produces the C result without float-multiply error."
  (let* ((prec    (or precision 6))
         (exact   (rational (abs num)))      ; exact rational value of the double
         (scale   (expt 10 prec))
         (rounded (round (* exact scale))))  ; CL ROUND = round-half-to-even
    (if (zerop prec)
        (format nil "~D" rounded)
        (multiple-value-bind (int frac) (floor rounded scale)
          (format nil "~D.~V,'0D" int prec frac)))))

(defun sprintf-format-float-e (num precision upper-case-p)
  "Format float as exponential notation with given precision (default 6)."
  (let* ((prec (or precision 6))
         (abs-num (abs num))
         (letter (if upper-case-p #\E #\e))
         ;; Use rationals for computation to avoid single/double precision issues
         (rat-num (rational abs-num)))
    (if (zerop abs-num)
        ;; Special case: 0
        (format nil "~A~A+00"
                (if (zerop prec) "0" (format nil "0.~A" (make-string prec :initial-element #\0)))
                letter)
        (let* ((exp10 (floor (log (coerce abs-num 'double-float) 10.0d0)))
               ;; Normalize using rational arithmetic
               (mantissa (/ rat-num (expt 10 exp10))))
          ;; Fix mantissa in [1, 10) range
          (when (>= mantissa 10)
            (setf mantissa (/ mantissa 10))
            (incf exp10))
          (when (< mantissa 1)
            (setf mantissa (* mantissa 10))
            (decf exp10))
          ;; Round the mantissa to precision decimal places
          (let* ((scale (expt 10 prec))
                 (rounded (/ (round (* mantissa scale)) scale))
                 ;; Check if rounding pushed us to 10
                 (_ (when (>= rounded 10)
                      (setf rounded (/ rounded 10))
                      (incf exp10)))
                 (mant-str (if (zerop prec)
                               (format nil "~D" (round rounded))
                               (let ((s (format nil "~,vF" prec (coerce rounded 'double-float))))
                                 (string-left-trim " " s))))
                 (exp-sign (if (minusp exp10) "-" "+"))
                 (exp-abs (abs exp10))
                 (exp-str (if (< exp-abs 10)
                              (format nil "0~D" exp-abs)
                              ;; Perl uses minimum 2 digits, but 3 for large exponents
                              (format nil "~D" exp-abs))))
            (declare (ignore _))
            (format nil "~A~A~A~A" mant-str letter exp-sign exp-str))))))

(defun sprintf-format-float-g (num precision upper-case-p alt-form-p)
  "Format float as %g: use %e if exponent < -4 or >= precision, else %f.
   Strip trailing zeros unless alt-form."
  (let* ((prec (if (and precision (zerop precision)) 1 (or precision 6)))
         (abs-num (abs num))
         (rat-num (rational abs-num))
         (exp10 (if (zerop abs-num) 0 (floor (log (coerce abs-num 'double-float) 10.0d0)))))
    (declare (ignore rat-num))
    ;; Adjust exp10 for rounding using rational arithmetic
    (when (not (zerop abs-num))
      (let ((test-mant (/ (rational abs-num) (expt 10 exp10))))
        (when (>= test-mant 10)
          (incf exp10))
        (when (< test-mant 1)
          (decf exp10))))
    (if (or (< exp10 -4) (>= exp10 prec))
        ;; Use %e with (prec-1) precision
        (let ((s (sprintf-format-float-e num (max 0 (1- prec)) upper-case-p)))
          (if alt-form-p
              s
              ;; Strip trailing zeros from mantissa part (before e/E)
              (let ((e-pos (position (if upper-case-p #\E #\e) s)))
                (if e-pos
                    (let* ((mant (subseq s 0 e-pos))
                           (exp-part (subseq s e-pos))
                           (trimmed (string-right-trim "0" mant))
                           (trimmed (string-right-trim "." trimmed)))
                      (concatenate 'string trimmed exp-part))
                    s))))
        ;; Use %f with (prec - 1 - exp10) precision
        (let* ((f-prec (max 0 (- prec 1 exp10)))
               (s (sprintf-format-float-f num f-prec)))
          (if alt-form-p
              s
              ;; Strip trailing zeros from the FRACTIONAL part only: integer
              ;; digits are significant (e.g. "100000" must not become "1").
              (if (find #\. s)
                  (let* ((trimmed (string-right-trim "0" s))
                         (trimmed (string-right-trim "." trimmed)))
                    trimmed)
                  s))))))

(defun sprintf-apply-width (str width left-justify-p zero-pad-p sign-str)
  "Apply width/padding to formatted string. SIGN-STR is the sign prefix (if any).
   Zero-padding goes between sign and digits."
  (let* ((full-str (concatenate 'string sign-str str))
         (cur-len (length full-str))
         (abs-width (abs width)))
    (if (<= abs-width cur-len)
        full-str
        (let ((pad-len (- abs-width cur-len)))
          (if left-justify-p
              ;; Left-justify: pad right with spaces
              (concatenate 'string full-str (make-string pad-len :initial-element #\Space))
              (if zero-pad-p
                  ;; Zero-pad: sign, then zeros, then digits
                  (concatenate 'string sign-str
                               (make-string pad-len :initial-element #\0)
                               str)
                  ;; Right-justify with spaces
                  (concatenate 'string (make-string pad-len :initial-element #\Space)
                               full-str)))))))

(defun sprintf-one (type-char flags width precision args arg-idx)
  "Format one value for a sprintf specifier.
   Returns (values result-string new-arg-idx)."
  (let* ((left-justify (find #\- flags))
         (force-sign (find #\+ flags))
         (space-sign (find #\Space flags))
         (zero-pad (and (find #\0 flags) (not left-justify)))
         (alt-form (find #\# flags))
         (upper-case-p (upper-case-p type-char))
         (type-lower (char-downcase type-char)))
    (case type-lower
      ;; String
      ((#\s)
       (let* ((val (nth arg-idx args))
              (s (to-string val))
              (s (if precision (subseq s 0 (min precision (length s))) s))
              (sign ""))
         (values (sprintf-apply-width s (or width 0) left-justify zero-pad sign)
                 (1+ arg-idx))))

      ;; Character from code point
      ((#\c)
       (let* ((val (nth arg-idx args))
              (num (to-number val))
              (special (sprintf-inf-nan-p (if (floatp num) num 0.0d0))))
         (when (or special
                   (and (floatp num)
                        (or #+sbcl (sb-ext:float-infinity-p num)
                            #+sbcl (sb-ext:float-nan-p num))))
           (p-die (make-p-box (format nil "Cannot printf ~A with 'c'" (to-string val))) nil))
         (let* ((code (truncate num))
                (ch (if (and (>= code 0) (<= code #x10FFFF))
                        (string (code-char code))
                        ""))
                (sign ""))
           (values (sprintf-apply-width ch (or width 0) left-justify zero-pad sign)
                   (1+ arg-idx)))))

      ;; Integer types: d/i, u, o, x/X, b/B
      ((#\d #\i #\u #\o #\x #\b)
       (let* ((val (nth arg-idx args))
              (num (to-number val))
              (special (sprintf-inf-nan-p (if (floatp num) num 0.0d0))))
         (if special
             ;; Inf/NaN: output as string with width handling
             (let* ((inf-nan-str (case special
                                   (:pos-inf "Inf")
                                   (:neg-inf "-Inf")
                                   (:nan "NaN")))
                    (sign ""))
               (when (and (eq special :pos-inf) force-sign)
                 (setf inf-nan-str "+Inf"))
               (values (sprintf-apply-width inf-nan-str (or width 0) left-justify nil sign)
                       (1+ arg-idx)))
             ;; Normal integer
             (let* ((int-val (if (member type-lower '(#\u #\o #\x #\b))
                                 ;; Unsigned: truncate to unsigned 64-bit
                                 (let ((v (truncate num)))
                                   (if (minusp v)
                                       (logand v #xFFFFFFFFFFFFFFFF)
                                       v))
                                 ;; Signed: just truncate
                                 (truncate num)))
                    (base (case type-lower
                            ((#\d #\i #\u) 10)
                            (#\o 8)
                            (#\x 16)
                            (#\b 2)))
                    ;; Bare base digits of |value|, no 0x/0b/0-prefix.
                    (digits0 (sprintf-format-int (abs int-val) base upper-case-p nil))
                    ;; Apply precision (minimum digit count, zero-padded).  A zero
                    ;; value with precision 0 produces NO digits (Perl: "%.0d",0 => "").
                    (digits (cond
                              ((and precision (zerop precision) (zerop int-val)) "")
                              ((and precision (> precision (length digits0)))
                               (concatenate 'string
                                            (make-string (- precision (length digits0))
                                                         :initial-element #\0)
                                            digits0))
                              (t digits0)))
                    ;; Alt-form (#) prefix.  For hex/binary the 0x/0b prefix is
                    ;; suppressed when the value is zero (Perl).  For octal, # forces
                    ;; the digit string to begin with a 0.
                    (prefix (cond
                              ((not alt-form) "")
                              ((= base 16) (if (zerop int-val) "" (if upper-case-p "0X" "0x")))
                              ((= base 2)  (if (zerop int-val) "" (if upper-case-p "0B" "0b")))
                              ((= base 8)  (if (or (zerop (length digits))
                                                   (char/= (char digits 0) #\0))
                                               "0" ""))
                              (t "")))
                    ;; Sign handling
                    (sign (cond
                            ((minusp int-val) "-")
                            ((and (member type-lower '(#\d #\i)) force-sign) "+")
                            ((and (member type-lower '(#\d #\i)) space-sign) " ")
                            (t "")))
                    ;; The 0x/0b/0 prefix stays left of any zero-padding, like the
                    ;; sign — so %#08x of 255 is "0x0000ff", not "00000xff".
                    (sign+prefix (concatenate 'string sign prefix)))
               (values (sprintf-apply-width digits (or width 0) left-justify
                                            (and zero-pad (null precision)) sign+prefix)
                       (1+ arg-idx))))))

      ;; Float types: f/F, e/E, g/G
      ((#\f #\e #\g)
       (let* ((val (nth arg-idx args))
              (raw-num (to-number val))
              (num (if (floatp raw-num) raw-num (coerce raw-num 'double-float)))
              (special (sprintf-inf-nan-p num)))
         (if special
             ;; Inf/NaN
             (let* ((base-str (case special
                                (:pos-inf "Inf")
                                (:neg-inf "Inf")
                                (:nan "NaN")))
                    (sign (cond
                            ((eq special :neg-inf) "-")
                            ((and (eq special :pos-inf) force-sign) "+")
                            ((and (eq special :pos-inf) space-sign) " ")
                            ;; NaN: no sign prefix (Perl behavior)
                            (t ""))))
               (values (sprintf-apply-width base-str (or width 0) left-justify nil sign)
                       (1+ arg-idx)))
             ;; Normal float
             (let* ((sign-str (cond
                                ((minusp num) "-")
                                (force-sign "+")
                                (space-sign " ")
                                (t "")))
                    (abs-num (abs num))
                    (raw (case type-lower
                           (#\f (sprintf-format-float-f abs-num precision))
                           (#\e (sprintf-format-float-e abs-num precision upper-case-p))
                           (#\g (sprintf-format-float-g abs-num precision upper-case-p alt-form)))))
               ;; For %f with alt-form (#), force decimal point
               (when (and alt-form (eql type-lower #\f) (not (find #\. raw)))
                 (setf raw (concatenate 'string raw ".")))
               ;; For %e with alt-form, force decimal point
               (when (and alt-form (eql type-lower #\e))
                 (let ((e-pos (position (if upper-case-p #\E #\e) raw)))
                   (when (and e-pos (not (find #\. (subseq raw 0 e-pos))))
                     (setf raw (concatenate 'string (subseq raw 0 e-pos) "." (subseq raw e-pos))))))
               (values (sprintf-apply-width raw (or width 0) left-justify zero-pad sign-str)
                       (1+ arg-idx))))))

      ;; Literal percent (handled in caller, but just in case)
      ((#\%)
       (values "%" arg-idx))

      ;; Pointer address — output as lowercase hex (like Perl's %p)
      ((#\p)
       (let* ((val (nth arg-idx args))
              (obj (if val (unbox val) nil))
              (addr #+sbcl (sb-kernel:get-lisp-obj-address obj)
                    #-sbcl (sxhash obj))
              (s (string-downcase (format nil "~x" addr))))
         (values (sprintf-apply-width s (or width 0) left-justify nil "")
                 (1+ arg-idx))))

      ;; Hexadecimal floating point: %a/%A
      ;; Format: [sign]0x[lead].[frac]p[+-][exp]
      ;; IEEE 754 double: 1 sign bit, 11 exponent bits, 52 mantissa bits
      ;; Normal: leading hex digit = 1, exp = biased_exp - 1023
      ;; Subnormal/zero: leading hex digit = 0, exp = -1022
      ((#\a)
       (let* ((val (nth arg-idx args))
              (raw-num (to-number val))
              (dbl (coerce (if (complexp raw-num) (realpart raw-num) raw-num)
                           'double-float))
              (is-nan (sb-ext:float-nan-p dbl))
              (is-inf (and (not is-nan) (sb-ext:float-infinity-p dbl)))
              (is-neg (and (not is-nan) (minusp dbl)))
              ;; NaN never gets a sign prefix (matches Perl %f/%e behavior)
              (sign-str (cond (is-nan "")
                              (is-neg "-")
                              (force-sign "+")
                              (space-sign " ")
                              (t "")))
              (abs-dbl (if is-neg (- dbl) dbl))
              ;; hex-str = the hex-float body WITHOUT sign, WITHOUT case transform
              (hex-str
               (cond
                 ;; NaN: no sign prefix in Perl (matches %f/%e behavior)
                 (is-nan "NaN")
                 ;; Inf
                 (is-inf "Inf")
                 ;; Zero (positive or negative — negative zero handled via sign-str)
                 ((zerop abs-dbl)
                  (let ((frac-str (cond
                                    ((null precision)
                                     (if alt-form "." ""))
                                    ((= precision 0)
                                     (if alt-form "." ""))
                                    (t (concatenate 'string "."
                                                    (make-string precision
                                                                 :initial-element #\0))))))
                    (concatenate 'string "0x0" frac-str "p+0")))
                 ;; Normal or subnormal finite non-zero
                 (t
                  (multiple-value-bind (m raw-e s)
                      (integer-decode-float abs-dbl)
                    (declare (ignore s))
                    ;; integer-decode-float returns (m e s) such that value = m * 2^e,
                    ;; where m always has its MSB set (normalized integer, not IEEE bits).
                    ;; For normals: integer-length(m)=53, unbiased-exp = e+52.
                    ;; For subnormals: integer-length(m)<53, unbiased-exp = e+leading-bits.
                    ;; In both cases the output uses leading digit "1" with adjusted exponent.
                    ;; Examples: 2^-1074 → m=1,e=-1074 → "0x1p-1074"
                    ;;           253*2^-1071 → m=253,e=-1071,leading=7 → "0x1.fap-1064"
                    (let* ((leading-bits (- (integer-length m) 1))
                           (unbiased-exp (+ raw-e leading-bits))
                           ;; Strip the implicit leading 1 to get fraction bits
                           (frac-int (logxor m (ash 1 leading-bits)))
                           ;; Align frac-int up to the next whole nibble boundary
                           (nibble-shift (mod (- (mod leading-bits 4)) 4))
                           ;; Total nibbles needed (0 when leading-bits=0)
                           (total-nibbles (if (zerop leading-bits) 0
                                              (ceiling leading-bits 4)))
                           (frac-aligned (ash frac-int nibble-shift))
                           ;; Format as total-nibbles uppercase hex digits
                           (frac-full (if (zerop total-nibbles)
                                          ""
                                          (format nil (format nil "~~~D,'0X" total-nibbles)
                                                  frac-aligned)))
                           ;; Apply precision (rounding may bump unbiased-exp via incf)
                           (frac-hex
                            (cond
                              ;; No precision: trim trailing zeros
                              ((null precision)
                               (string-right-trim "0" frac-full))
                              ;; Precision 0: no fraction digits
                              ((= precision 0) "")
                              ;; Precision N: truncate or pad, with rounding
                              (t
                               (let ((full-len (length frac-full))
                                     (p precision))
                                 (if (<= full-len p)
                                     ;; Pad on right with zeros
                                     (concatenate 'string frac-full
                                                  (make-string (- p full-len)
                                                               :initial-element #\0))
                                     ;; Truncate with rounding
                                     (let* ((trunc (subseq frac-full 0 p))
                                            (next-ch (char frac-full p))
                                            (next-val (digit-char-p next-ch 16)))
                                       (if (>= next-val 8)
                                           ;; Round up the truncated part
                                           (let* ((trunc-val (parse-integer trunc :radix 16))
                                                  (rounded (1+ trunc-val)))
                                             (if (>= rounded (expt 16 p))
                                                 ;; Carry overflows fraction: bump exponent
                                                 (progn
                                                   (incf unbiased-exp)
                                                   (make-string p :initial-element #\0))
                                                 (format nil (format nil "~~~D,'0X" p) rounded)))
                                           trunc)))))))
                           ;; Build the fraction part of the output
                           (frac-part
                            (cond
                              ((string= frac-hex "") (if alt-form "." ""))
                              (t (concatenate 'string "." frac-hex))))
                           ;; Exponent as signed decimal, always with explicit sign
                           (exp-str (format nil "~@d" unbiased-exp)))
                      ;; Leading digit is always "1" (integer-decode-float normalizes m)
                      (concatenate 'string "0x1" frac-part "p" exp-str))))))
              ;; Apply case: %a → lowercase hex, %A → uppercase hex
              ;; NaN/Inf: Perl always uses mixed-case "NaN"/"Inf" regardless of %A
              (hex-str-cased (cond
                               (is-nan "NaN")
                               (is-inf "Inf")
                               (upper-case-p (string-upcase hex-str))
                               (t (string-downcase hex-str)))))
         ;; Width and padding
         ;; For Inf/NaN: simple width padding, no zero-padding between sign and body
         ;; For hex-float: zero-padding goes between "0x" prefix and mantissa
         (if (or is-nan is-inf)
             ;; Inf/NaN: treat like %e Inf/NaN (sign + body, no zero-pad between)
             (let ((full (concatenate 'string sign-str hex-str-cased)))
               (values (sprintf-apply-width full (or width 0) left-justify nil "")
                       (1+ arg-idx)))
             ;; Hex float: zero-pad inserts zeros after "0x", before mantissa
             (let ((total-len (+ (length sign-str) (length hex-str-cased))))
               (if (and zero-pad width (> width total-len))
                   ;; Zero-pad: sign + "0x" + zeros + rest-of-mantissa
                   (let* ((after-0x (subseq hex-str-cased 2))  ; skip "0x"/"0X"
                          (prefix (concatenate 'string sign-str
                                               (subseq hex-str-cased 0 2)))
                          (pad-len (- width total-len))
                          (pad (make-string pad-len :initial-element #\0)))
                     (values (concatenate 'string prefix pad after-0x)
                             (1+ arg-idx)))
                   ;; No zero-pad: normal width handling
                   (values (sprintf-apply-width hex-str-cased (or width 0)
                                                left-justify nil sign-str)
                           (1+ arg-idx)))))))

      ;; Unknown: output the specifier literally
      (otherwise
       (values (format nil "%~A" type-char) arg-idx)))))

(defun sprintf-valid-type-p (type-char vector-p)
  "True if TYPE-CHAR is a valid sprintf conversion character.  Perl rejects an
   unrecognised conversion (e.g. %C, %I, %P, %Z) by leaving the spec verbatim in
   the output and warning \"Invalid conversion\".  With the %v vector flag only the
   integer conversions are valid (%vd is fine, %vc / %vf / %vs are not)."
  (if vector-p
      (find type-char "diuoxXbBDUO")
      (find type-char "csdiuoxXbBeEfFgGaADUOpn%")))

(defvar *p-sprintf-caller* "sprintf"
  "Name of the calling function (sprintf or printf) for error messages.")

(defun sprintf-vector (type-char flags width precision sep val)
  "Format VAL (a string / v-string) as a vector: each character's ordinal is
   formatted with the given conversion and the results joined by SEP.
   Implements Perl's %vd / %*vd family."
  ;; A version object ($^V) stringifies via its display form ("v5.30.0"), but
  ;; %vd walks the character PAYLOAD — pull it out before to-string.
  (let* ((uv (unbox val))
         (s (if (p-vstring-p uv) (p-vstring-s uv) (to-string val))))
    (if (zerop (length s))
        ""
        (with-output-to-string (out)
          (loop for ch across s
                for first = t then nil
                do (unless first (write-string sep out))
                (write-string (sprintf-one type-char flags width precision
                                           (list (char-code ch)) 0)
                              out))))))

(defun %sprintf-star-positional (fmt-str j len)
  "After a '*' in a sprintf width/precision, check for an N$ positional reference
   (e.g. the `3$` in `%*3$d`).  Returns (values positional-index new-j) when present
   — a 0-based arg index and j advanced past the `$` — or (values NIL j) when the '*'
   should consume the next sequential argument instead."
  (let ((peek j) (pn 0) (pd nil))
    (loop while (and (< peek len) (digit-char-p (char fmt-str peek)))
          do (setf pn (+ (* pn 10) (digit-char-p (char fmt-str peek))) pd t)
          (incf peek))
    (if (and pd (< peek len) (char= (char fmt-str peek) #\$))
        (values (1- pn) (1+ peek))
        (values nil j))))

(defun p-sprintf (fmt &rest args)
  "Perl sprintf - full format string parser.
   Supports: %d %i %u %o %x %X %b %B %e %E %f %F %g %G %s %c %%
   Flags: - + 0 space #
   Width and precision: literal or * (from args)
   Positional: %N$type selects argument N (1-based)"
  ;; Flatten any vector args: splice/map/grep in list context returns a vector
  ;; which Perl flattens into argument lists.
  ;; Blessed arrays (p-box with class) must NOT be flattened — they are
  ;; overloadable scalar values (e.g. objects that stringify via "").
  (let ((args (loop for arg in args
                    nconcing (let ((v (unbox arg)))
                               (if (and (vectorp v) (not (stringp v))
                                        (not (and (p-box-p arg) (p-box-class arg))))
                                   (coerce v 'list)
                                   (list arg))))))
    (let ((fmt-str (to-string fmt)))
      (with-output-to-string (out)
        (let ((i 0)
              (arg-idx 0)
              (has-positional nil)
              (saw-invalid nil)
              (n-args (length args))
              (len (length fmt-str)))
          (loop while (< i len) do
                (let ((c (char fmt-str i)))
                  (if (char= c #\%)
                      (if (>= (1+ i) len)
                          ;; Trailing % at end of string
                          (progn (write-char #\% out) (incf i))
                          (if (char= (char fmt-str (1+ i)) #\%)
                              ;; %%
                              (progn (write-char #\% out) (incf i 2))
                              ;; Parse format specifier: %[flags][width][.precision][size]type
                              ;; Also handles positional: %N$type (1-based arg index)
                              (let ((j (1+ i))
                                    (flags "")
                                    (width nil)
                                    (precision nil)
                                    (positional-idx nil)
                                    (vector-sep nil)
                                    (spec-start-arg arg-idx))
                                ;; Check for N$ positional specifier before flags
                                (let ((peek j) (peek-n 0) (peek-has-digit nil))
                                  (loop while (and (< peek len) (digit-char-p (char fmt-str peek)))
                                        do (setf peek-n (+ (* peek-n 10)
                                                           (digit-char-p (char fmt-str peek))))
                                        (setf peek-has-digit t)
                                        (incf peek))
                                  (when (and peek-has-digit (< peek len)
                                             (char= (char fmt-str peek) #\$))
                                    (when (> peek-n 2147483647)
                                      (error "Integer overflow in format string for ~A ~A"
                                             *p-sprintf-caller* fmt-str))
                                    (setf positional-idx (1- peek-n))
                                    (setf has-positional t)
                                    (setf j (1+ peek))))
                                ;; Parse flags
                                (loop while (and (< j len) (find (char fmt-str j) "-+ 0#"))
                                      do (setf flags (concatenate 'string flags
                                                                  (string (char fmt-str j))))
                                      (incf j))
                                ;; Vector flag: v (separator ".") or *v (separator
                                ;; taken from the next argument).  Sits between flags
                                ;; and width.  Disambiguated from a width '*' by
                                ;; looking ahead for the 'v'.
                                (cond
                                  ((and (< j len) (char= (char fmt-str j) #\v))
                                   (setf vector-sep ".")
                                   (incf j))
                                  ((and (< j len) (char= (char fmt-str j) #\*)
                                        (< (1+ j) len) (char= (char fmt-str (1+ j)) #\v))
                                   (setf vector-sep (to-string (nth arg-idx args)))
                                   (incf arg-idx)
                                   (incf j 2)))  ; consume '*' and 'v'
                                ;; Flags may also follow the vector flag (e.g. %v02x,
                                ;; where 0 is a per-element zero-pad flag).
                                (when vector-sep
                                  (loop while (and (< j len) (find (char fmt-str j) "-+ 0#"))
                                        do (setf flags (concatenate 'string flags
                                                                    (string (char fmt-str j))))
                                        (incf j)))
                                ;; Parse width
                                (cond
                                  ((and (< j len) (char= (char fmt-str j) #\*))
                                   (incf j)  ; consume '*'
                                   (multiple-value-bind (pos-idx new-j)
                                       (%sprintf-star-positional fmt-str j len)
                                     (setf j new-j)
                                     (when pos-idx (setf has-positional t))
                                     (let ((src (or pos-idx arg-idx)))
                                       (setf width (truncate (to-number (nth src args))))
                                       (unless pos-idx (incf arg-idx)))
                                     ;; Width from * must fit in a C int; otherwise
                                     ;; Perl dies "Integer overflow" (abs covers the
                                     ;; huge-negative IV_MIN case before the - flip).
                                     (when (> (abs width) 2147483647)
                                       (error "Integer overflow in format string for ~A ~A"
                                              *p-sprintf-caller* fmt-str))
                                     (when (minusp width)
                                       (setf flags (concatenate 'string flags "-"))
                                       (setf width (- width)))))
                                  (t
                                   (let ((w 0) (has-digit nil))
                                     (loop while (and (< j len) (digit-char-p (char fmt-str j)))
                                           do (setf w (+ (* w 10) (digit-char-p (char fmt-str j))))
                                           (setf has-digit t)
                                           (incf j))
                                     (when has-digit
                                       (when (> w 2147483647)
                                         (error "Integer overflow in format string for ~A ~A"
                                                *p-sprintf-caller* fmt-str))
                                       (setf width w)))))
                                ;; Parse precision
                                (when (and (< j len) (char= (char fmt-str j) #\.))
                                  (incf j)
                                  (cond
                                    ((and (< j len) (char= (char fmt-str j) #\*))
                                     ;; A negative precision supplied via * means the
                                     ;; precision is omitted entirely (Perl semantics),
                                     ;; not a precision of 0.  Supports `.*N$` positional.
                                     (incf j)  ; consume '*'
                                     (multiple-value-bind (pos-idx new-j)
                                         (%sprintf-star-positional fmt-str j len)
                                       (setf j new-j)
                                       (when pos-idx (setf has-positional t))
                                       (let* ((src (or pos-idx arg-idx))
                                              (pv (truncate (to-number (nth src args)))))
                                         ;; Precision from * must fit in a C int; a
                                         ;; huge magnitude (even negative) overflows
                                         ;; before the "negative means omitted" rule.
                                         (when (> (abs pv) 2147483647)
                                           (error "Integer overflow in format string for ~A ~A"
                                                  *p-sprintf-caller* fmt-str))
                                         (setf precision (if (minusp pv) nil pv))
                                         (unless pos-idx (incf arg-idx)))))
                                    (t
                                     (let ((p 0) (has-digit nil))
                                       (loop while (and (< j len) (digit-char-p (char fmt-str j)))
                                             do (setf p (+ (* p 10) (digit-char-p (char fmt-str j))))
                                             (setf has-digit t)
                                             (incf j))
                                       (when (and has-digit (> p 2147483647))
                                         (error "Integer overflow in format string for ~A ~A"
                                                *p-sprintf-caller* fmt-str))
                                       (setf precision (if has-digit p 0))))))
                                ;; Skip size modifiers (l, h, q, L, V, etc.) — Perl's
                                ;; integer-size flags.  V is Perl's IV/UV-size modifier
                                ;; (so %Vd is a synonym for %d).
                                (loop while (and (< j len) (find (char fmt-str j) "lhqLzjtV"))
                                      do (incf j))
                                ;; Type character
                                (if (< j len)
                                    (let ((type-char (char fmt-str j)))
                                      (incf j) ; consume the type char
                                      (if (not (sprintf-valid-type-p type-char vector-sep))
                                          ;; Invalid conversion (e.g. %C, %I, %Z, or %vc):
                                          ;; leave the entire spec verbatim, warn, and do
                                          ;; NOT consume an argument (restore the arg pointer
                                          ;; to the spec start).  A malformed spec also
                                          ;; suppresses the trailing "Redundant argument".
                                          (progn
                                            (p-warn (make-p-box
                                                     (format nil "Invalid conversion in ~A: \"~A\""
                                                             *p-sprintf-caller* (string type-char))))
                                            (write-string (subseq fmt-str i j) out)
                                            (setf arg-idx spec-start-arg)
                                            (setf saw-invalid t)
                                            (setf i j))
                                          ;; For positional %N$type, use the fixed index;
                                          ;; for sequential, use arg-idx and advance it.
                                          (let ((call-idx (if positional-idx
                                                              positional-idx
                                                              arg-idx)))
                                            (if (and positional-idx (< call-idx 0))
                                                ;; %0$x: positional 0 is invalid (1-based), output spec literally
                                                (progn
                                                  (p-warn (make-p-box
                                                           (format nil "Invalid conversion in ~A: \"~A\""
                                                                   *p-sprintf-caller* (string type-char))))
                                                  (write-string (concatenate 'string "%" (subseq fmt-str (1+ i) j)) out)
                                                  (setf saw-invalid t)
                                                  (setf i j))
                                                (progn
                                                  (when (>= call-idx n-args)
                                                    (p-warn (make-p-box
                                                             (format nil "Missing argument in ~A"
                                                                     *p-sprintf-caller*))))
                                                  (if vector-sep
                                                      ;; Vector flag: format each character
                                                      ;; ordinal of the string arg, joined.
                                                      (progn
                                                        (write-string
                                                         (sprintf-vector type-char flags width precision
                                                                         vector-sep (nth call-idx args))
                                                         out)
                                                        (setf arg-idx (if positional-idx arg-idx (1+ call-idx)))
                                                        (setf i j))
                                                      (multiple-value-bind (result new-arg-idx)
                                                          (sprintf-one type-char flags width precision args call-idx)
                                                        (write-string result out)
                                                        (setf arg-idx (if positional-idx arg-idx new-arg-idx))
                                                        (setf i j))))))))
                                    ;; No valid conversion char (e.g. "%L", "%h", "%v",
                                    ;; or a bare "%5" at end of string): the spec ran off
                                    ;; the end after flags/width/precision/size or the
                                    ;; lone vector flag.  Leave it verbatim, warn INVALID,
                                    ;; restore the arg pointer, and suppress "Redundant".
                                    (progn
                                      (p-warn (make-p-box
                                               (format nil "Invalid conversion in ~A: \"%\""
                                                       *p-sprintf-caller*)))
                                      (write-string (subseq fmt-str i j) out)
                                      (setf arg-idx spec-start-arg)
                                      (setf saw-invalid t)
                                      (setf i j))))))
                      ;; Regular character
                      (progn
                        (write-char c out)
                        (incf i)))))     ; close: progn, if(char=%?), let(c), loop
          ;; Redundant argument warning: sequential format used fewer args than provided.
          ;; A malformed/invalid conversion suppresses this warning (Perl behaviour).
          (when (and (not has-positional) (not saw-invalid) (< arg-idx n-args))
            (p-warn (make-p-box
                     (format nil "Redundant argument in ~A" *p-sprintf-caller*))))))))) ; close: let(i..), with-output-to-string, let(fmt-str), let(args), defun

(defun p-printf (&rest args)
  "Perl printf - formatted print (with optional filehandle)"
  (let ((fh (%p-default-out))
        (fmt nil)
        (fmt-args nil))
    ;; Check for :fh keyword.  Same rules as p-print, including the bail: a
    ;; named-but-unopened handle resolves to nil, and (princ … nil) would print
    ;; to *standard-output* — output silently going to the wrong place.  perl
    ;; sets EBADF, prints nothing and returns false.
    (when (and (>= (length args) 2) (eq (first args) :fh))
      (let ((desig (second args)))
        (setf args (cddr args))
        (let ((resolved (%p-out-fh-or-fail desig "printf")))
          (unless resolved (return-from p-printf *p-undef*))
          (setf fh resolved))))
    ;; printf takes a LIST (FORMAT, LIST): flatten raw @array/%hash args so the
    ;; format comes from the first flattened element, e.g. `printf @a` where
    ;; @a = ("%d\n", 5).  A p-box-wrapped ref stays scalar (printf "%s", $aref).
    (setf args (coerce (p-flatten-args args) 'list))
    ;; First remaining arg is format, rest are format args
    (setf fmt (first args))
    (setf fmt-args (rest args))
    (let ((*p-sprintf-caller* "printf"))
      (princ (apply #'p-sprintf fmt fmt-args) fh))
    (%p-maybe-autoflush fh)
    1))

;;; ============================================================
;;; Assignment and Mutation
;;; ============================================================

;;; Distinct assignment forms for each Perl target type.
;;; These make the Perl semantics visible in the generated IR.
;;; p-setf dispatches to these internally; codegen will emit them directly.

(defmacro p-scalar-= (place value)
  "Assign to a scalar variable ($var). Auto-declares as global if unbound.
   Reference values (p-backslash) are stored as box-in-box."
  ;; Check if value is a reference (p-backslash)
  (if (and (listp value) (eq (car value) 'p-backslash))
      ;; Reference assignment - store box directly, don't unbox.
      ;; Must still route through STORE for tied variables.
      (let ((val (gensym "VAL"))
            (cur (gensym "CUR")))
        `(let ((,val ,value))
           (unless (boundp ',place)
             (%p-ensure-storage (quote ,place))
             (setf (symbol-value ',place) (make-p-box nil)))
           (let ((,cur (p-box-value ,place)))
             (if (p-tie-proxy-p ,cur)
                 (%p-tie-store-ref ,place ,cur ,val)
                 (setf (p-box-value ,place) ,val
                       (p-box-nv-ok ,place) nil
                       (p-box-sv-ok ,place) nil)))
           ,val))
      ;; Normal assignment - use box-set which unboxes.
      ;; Return the place (the box) so chained operators like (.= += etc.)
      ;; can modify it in place: ($a = expr) .= "suffix" sets $a to expr."suffix".
      (let ((val (gensym "VAL")))
        `(let ((,val ,value))
           (unless (boundp ',place)
             (%p-ensure-storage (quote ,place))
             (setf (symbol-value ',place) (make-p-box nil)))
           (box-set ,place ,val)
           ,place))))

(defmacro p-my-= (place value)
  "Assign to a lexically-bound 'my' variable. Unlike p-scalar-=, does not
   auto-declare the variable as special — the enclosing let binding (emitted by
   _with_declarations in Parser.pm) already handles scoping. This makes the
   assignment intent explicit for other compiler backends reading the IR.
   Returns the PLACE (the box), like p-scalar-=, so the assignment can be used
   as an lvalue: ($x = 5)++ / ++($x = 5) / ($x = expr) .= \"s\". box-set unboxes
   a box VALUE argument, so returning the box is safe when chained as a value."
  `(progn (box-set ,place ,value) ,place))

(defun p-box-init (value)
  "Fresh box initialized with Perl copy semantics (my $x = VALUE).
   Used as a LET binding init-form when the init expression references the
   declared name itself (my $i = $i): a let init-form is evaluated in the
   OUTER environment, so the reference reads the outer/shadowed variable,
   whereas a body-position (p-my-= $i INIT) would read the fresh nil box."
  (let ((b (make-p-box nil)))
    (box-set b value)
    b))

(defun %p-dualvar-copy (item)
  "Fresh box copying a genuine dualvar ITEM, keeping both its numeric and string
   halves (a bare make-p-box around its string value would drop the numeric)."
  (let ((s (p-box-value item))
        (nb (make-p-box (p-box-value item))))
    (setf (p-box-nv nb) (p-box-nv item) (p-box-nv-ok nb) t
          (p-box-sv nb) s (p-box-sv-ok nb) t)
    nb))

(defun %p-array-store-scalar (arr item)
  "Store a scalar ITEM into ARR, preserving blessed objects and references."
  (if (p-box-p item)
      (let ((inner (p-box-value item)))
        (cond
          ;; Blessed box: preserve as-is (class must not be lost)
          ((p-box-class item) (vector-push-extend item arr))
          ;; Scalar/nested reference (box-in-box, e.g. \$x or \\$x): the depth of
          ;; box nesting encodes the reference type (SCALAR vs REF), so we must
          ;; NOT add or remove a box layer here.  Store the reference box as-is.
          ((p-box-p inner) (vector-push-extend item arr))
          ;; Reference to a raw object (array-ref, hash-ref, code-ref, qr//; the
          ;; glob is the arm just below):
          ;; copy the scalar CONTAINER while keeping the SAME underlying object.
          ;; Perl's [$x] / @a=($x) copies the scalar; if $x is later reassigned
          ;; (box-set mutates the original box in place) the stored copy must not
          ;; follow it.  A fresh box around the same object is a distinct container
          ;; pointing at the same referent — and does not change the ref type
          ;; (still ARRAY/HASH/CODE/…), since the object itself is unchanged.
          ;; A typeglob takes the same fresh-container copy as its siblings, but
          ;; the flag comes with it: for a glob the REF-ness lives on the
          ;; container (is-ref), not on the object, so a copy that drops it
          ;; turns `\*foo` into a bare glob (#423) — the one raw-object kind
          ;; whose ref type is NOT decided by the object alone.
          ((p-typeglob-p inner)
           (let ((nb (make-p-box inner)))
             (setf (p-box-is-ref nb) (p-box-is-ref item))
             (vector-push-extend nb arr)))
          ((or (and (vectorp inner) (not (stringp inner)))
               (hash-table-p inner)
               (functionp inner)
               (p-regex-match-p inner))
           (vector-push-extend (make-p-box inner) arr))
          ;; Dualvar ($!/Scalar::Util::dualvar): copy keeping both halves.
          ((%p-dualvar-box-p item)
           (vector-push-extend (%p-dualvar-copy item) arr))
          ;; Plain scalar box: copy into new box
          (t (vector-push-extend (make-p-box inner) arr))))
      (vector-push-extend (make-p-box item) arr)))

(defun %p-make-hash-entry (v)
  "Create a fresh entry box from V for storage in a hash, preserving bless class
   AND scalar-reference-ness.

   For a SCALAR reference passed DIRECTLY (V is itself a box with is-ref set, e.g.
   from `%h = (k => \\$x)` whose RHS element is (p-backslash $x)), wrap the ref box
   whole — entry = box(→refbox→referent).  This is the same double-box shape that
   the already-working `my $r=\\$x; %h=(k=>$r)` path produces, and the shape
   p-gethash expects: its (make-p-box (unbox …)) on read yields the inner refbox,
   keeping the entry a scalar ref.  The previous (make-p-box (unbox v)) UNBOXED the
   ref one level, so p-gethash then stripped it to a plain scalar — silently
   turning `%h=(k=>\\$x)` into a non-ref (ref()='' , ${$h{k}} empty).

   Plain scalars and blessed objects keep copy semantics (unbox+rewrap, copying the
   bless class).  Array/hash refs don't set is-ref (a box wrapping a vector/
   hash-table is unambiguously a ref), so they take the plain branch unchanged."
  (cond
    ((and (p-box-p v) (p-box-is-ref v))
     (make-p-box v))
    ;; Dualvar ($!/Scalar::Util::dualvar): keep both numeric and string halves.
    ((%p-dualvar-box-p v)
     (%p-dualvar-copy v))
    (t
     (let ((b (make-p-box (unbox v))))
       (when (and (p-box-p v) (p-box-class v))
         (setf (p-box-class b) (p-box-class v)))
       b))))

(defun %p-snapshot-array-rhs (src)
  "Snapshot SRC for use as the RHS of an array assignment.
   Returns a fresh adjustable vector so that clearing the LHS array
   does not corrupt the source when SRC is (or contains) the LHS.
   Unlike %p-flatten-list this preserves nil (deleted-element markers)
   and does NOT unbox scalars — the existing add-items loop in p-array-=
   handles those steps.  Nested adjustable vectors are also snapshotted
   recursively so that e.g. @a = (1, @a, 2) works correctly."
  (cond
    ;; Adjustable vector: copy element-by-element, recursing into nested ones
    ((and (vectorp src) (not (stringp src)))
     (let ((snap (make-array (length src) :adjustable t :fill-pointer 0)))
       (loop for item across src
             do (vector-push-extend
                 (if (and (vectorp item) (not (stringp item)))
                     (%p-snapshot-array-rhs item)
                     item)
                 snap))
       snap))
    ;; CL list: recurse into nested vectors, leave other items as-is
    ((listp src)
     (let ((snap (make-array 8 :adjustable t :fill-pointer 0)))
       (loop for item in src
             do (vector-push-extend
                 (if (and (vectorp item) (not (stringp item)))
                     (%p-snapshot-array-rhs item)
                     item)
                 snap))
       snap))
    ;; Anything else (scalar, hash-table, nil, …): return as-is
    (t src)))

;;; ONE copy of the per-element rule of array filling (#387 family 13, s413):
;;; how one ITEM of a list being assigned into an array lands in TARGET.  A
;;; flatten marker and a raw hash-table (a %hash interpolated into a list
;;; literal — `@a = (1, %h, 2)` emits (vector 1 %h 2) — perl flattens it to
;;; its key/value pairs; task #170) and a nested vector recurse through
;;; RECURSE (the enclosing walker), nil stays a hole (the deleted-element
;;; marker, not undef-but-exists), anything else is stored as a scalar.  A
;;; hash REFERENCE is a p-box, not a raw table, so it reaches the scalar arm
;;; untouched.  A macro, so both walkers' vector and list loops expand to the
;;; cond they used to spell four times over.  ITEM must be a variable.
(defmacro %p-array-fill-item (item target recurse)
  `(cond
     ((p-flatten-marker-p ,item)
      (,recurse (p-flatten-marker-array ,item)))
     ((hash-table-p ,item)
      (,recurse ,item))
     ((and (vectorp ,item) (not (stringp ,item)))
      (,recurse ,item))
     ((null ,item)
      (vector-push-extend nil ,target))
     (t
      (%p-array-store-scalar ,target ,item))))

(defun p-array-fill (place value)
  "Clear adjustable array PLACE and refill it from VALUE: flatten nested vectors
   (but not strings), box elements, preserve nil holes.  Snapshots VALUE first so
   self-assignment (@a = @a) and embedding (@a = (1, @a, 2)) work.  Returns PLACE.
   Shared by the p-array-= macro and the closure-capture lexical array-init path
   (which cannot use p-array='s boundp/proclaim-special guard — that would make a
   let-bound lexical special and break the closure)."
  ;; `@ro = (…)` — even `@ro = sort @ro`, which sort.t asserts — dies in perl:
  ;; a whole-array assignment resets the size (task #159).
  (%p-check-array-writable place)
  ;; Snapshot any adjustable vector (including PLACE itself) BEFORE we clear PLACE,
  ;; to prevent aliasing. %p-snapshot-array-rhs recursively copies nested
  ;; adjustable vectors and preserves nil.
  (let ((snap (%p-snapshot-array-rhs value)))
    (setf (fill-pointer place) 0)
    ;; Perl: assigning to an array resets the each() iterator
    (remhash place *array-iterators*)
    (labels ((add-items (src)
               (cond
                 ((stringp src)
                  (vector-push-extend (make-p-box src) place))
                 ((hash-table-p src)
                  (maphash (lambda (k v)
                             (when (%p-real-hash-key-p k)
                               (vector-push-extend (make-p-box k) place)
                               (%p-array-store-scalar place v)))
                           src))
                 ((vectorp src)
                  (loop for item across src
                        do (%p-array-fill-item item place add-items)))
                 ((listp src)
                  (loop for item in src
                        do (%p-array-fill-item item place add-items)))
                 ;; Scalar (number, p-box, nil=undef) - wrap in a single-element array
                 (t
                  (when src
                    (%p-array-store-scalar place src))))))
      (add-items snap))
    place))

(defmacro p-array-= (place value)
  "Assign to an array variable (@arr). Clears and refills from value.
   Flattens nested vectors (but not strings), wraps elements in boxes.
   Snapshots any adjustable vector in the RHS before clearing the LHS
   so that self-assignment (@a = @a) and embedding (@a = (1, @a, 2))
   work correctly.  nil slots (deleted elements) are preserved."
  (let ((val (gensym "VAL")))
    ;; Assigning to an array imposes LIST context on the RHS (Perl: @a = EXPR
    ;; evaluates EXPR in list context).  Bind *wantarray* t so a context-sensitive
    ;; RHS — most importantly readline `my @lines = <$fh>` — yields its list form
    ;; (all records) rather than the ambient scalar form (one record).  *p-in-
    ;; list-assign-rhs* stays nil so this is NOT the per-line `while(($x)=<FH>)`
    ;; case.  Funcalls are already wrapped by gen_funcall; an inner binding wins,
    ;; so this is a no-op for them and only fixes the unwrapped readline/each forms.
    `(let ((,val (let ((*wantarray* t)) ,value)))
       (unless (boundp ',place)
         (%p-ensure-storage (quote ,place))
         (setf (symbol-value ',place) (make-array 0 :adjustable t :fill-pointer 0)))
       (p-array-fill ,place ,val))))

(defun p-hash-fill (place value)
  "Clear hash PLACE and repopulate it from VALUE (flattened to k-v pairs; an odd
   trailing key gets an undef value).  Returns the number of input elements (the
   scalar-context value of a hash assignment).  Shared by the p-hash-= macro and
   the closure-capture lexical hash-init path (which can't use p-hash='s
   boundp/proclaim-special guard)."
  (let* ((flat (cond
                 ((hash-table-p value)
                  (let ((r (make-array (* 2 (hash-table-count value))
                                       :adjustable t :fill-pointer 0)))
                    (maphash (lambda (k v)
                               (when (%p-real-hash-key-p k)
                                 (vector-push-extend (make-p-box k) r)
                                 (vector-push-extend v r)))
                             value)
                    r))
                 ((and (vectorp value) (not (stringp value)))
                  (%p-flatten-list value))
                 ;; A bare scalar RHS is a one-element list: `%h = "x"` means
                 ;; `%h = ("x")` -> key "x" with an undef value (Perl pads the
                 ;; odd element).
                 (t (%p-flatten-list (vector value)))))
         (cnt (length flat)))
    (cond
      ;; %INC = (...): the marker's backing store; values stay raw strings
      ;; (p-gethash's %INC arm returns them unboxed).
      ((eq place '%INC-MARKER%)
       (clrhash *p-inc-table*)
       (loop for i from 0 below cnt by 2
             do (setf (gethash (to-string (aref flat i)) *p-inc-table*)
                      (if (< (1+ i) cnt)
                          (to-string (aref flat (1+ i)))
                          ""))))
      ;; %ENV = (...): clear the process environment, then set the pairs.
      ((eq place '%ENV-MARKER%)
       (dolist (entry (sb-ext:posix-environ))
         (let ((eq-pos (position #\= entry)))
           (when eq-pos
             (sb-posix:unsetenv (subseq entry 0 eq-pos)))))
       (loop for i from 0 below cnt by 2
             do (sb-posix:setenv (to-string (aref flat i))
                                 (if (< (1+ i) cnt)
                                     (to-string (aref flat (1+ i)))
                                     "")
                                 1)))
      (t
       (clrhash place)
       (loop for i from 0 below cnt by 2
             do (setf (gethash (to-string (aref flat i)) place)
                      ;; The odd trailing key's padded value must be a real
                      ;; ENTRY BOX like every other value, not a bare
                      ;; *p-undef*: `$_++ foreach %h = (1,2,3)` returns the
                      ;; hash's values as LVALUES, and a raw undef cannot be
                      ;; written through (op/hashassign.t t304).  Same
                      ;; mechanism as the sibling branch, not a copy of it.
                      (%p-make-hash-entry (if (< (1+ i) cnt)
                                              (aref flat (1+ i))
                                              *p-undef*))))))
    cnt))

(defmacro p-hash-= (place value)
  "Assign to a hash variable (%hash). Clears and repopulates from value.
   Returns: list ctx → flattened hash contents; scalar/void → input element count."
  (let ((val (gensym "VAL"))
        (cnt (gensym "CNT"))
        (ret (gensym "RET")))
    `(let ((,val ,value))
       (unless (boundp ',place)
         (%p-ensure-storage (quote ,place))
         (setf (symbol-value ',place) (make-hash-table :test 'equal)))
       (let ((,cnt (p-hash-fill ,place ,val)))
         (if (eq *wantarray* t)
             ;; List context: return hash contents as flat vector
             (let ((,ret (make-array (* 2 (hash-table-count ,place))
                                     :adjustable t :fill-pointer 0)))
               (maphash (lambda (k v)
                          (when (%p-real-hash-key-p k)
                            (vector-push-extend (make-p-box k) ,ret)
                            (vector-push-extend v ,ret)))
                        ,place)
               ,ret)
             ;; Scalar/void: return count of input elements
             ,cnt)))))

;; Flatten a Perl-style value (vector/list/hash/scalar) to a flat vector
;; for use in list-assignment RHS. Hash tables expand to key-value pairs;
;; nested vectors are flattened (like p-array-= does).
(defun %p-dualvar-box-p (box)
  "True when BOX is a genuine dualvar: an explicit numeric value (nv-ok) sitting
   alongside a STRING primary value whose own numification differs from that
   numeric (e.g. $! errno, Scalar::Util::dualvar).  Such a box must stay intact
   when an array/hash/list would otherwise unbox it to a single scalar value —
   unboxing to the string drops the numeric half, and vice-versa.  A plain
   numified string ('5' carrying a cached nv of 5.0) is NOT a dualvar."
  (and (p-box-p box)
       (p-box-nv-ok box)
       (stringp (p-box-value box))
       (/= (p-box-nv box) (parse-perl-number (p-box-value box)))))

(defun %p-flatten-list (src)
  (let ((result (make-array 8 :adjustable t :fill-pointer 0)))
    (labels ((add (item)
               (cond
                 ((hash-table-p item)
                  (maphash (lambda (k v)
                             (when (%p-real-hash-key-p k)
                               (vector-push-extend (make-p-box k) result)
                               (vector-push-extend (if (p-box-p v) v (make-p-box v)) result)))
                           item))
                 ((and (vectorp item) (not (stringp item)))
                  (loop for x across item do (add x)))
                 ;; Raw nil means "empty list" (e.g. iterator at EOF returning nil).
                 ;; Explicit Perl undef comes as *p-undef* or (p-undef), not raw nil.
                 ;; NOTE: array HOLES are also raw nil here and currently vanish
                 ;; (the documented sparse-array limitation).  Converting them to
                 ;; undef collides with Exporter's hash-export internals (a nil
                 ;; that must drop) — both are indistinguishable raw nils, so the
                 ;; real fix is a distinct hole marker at the (setf p-aref) source.
                 ((null item) nil)
                 ((consp item)
                  (loop for x in item do (add x)))
                 (t
                  ;; Snapshot the value that box-set will store, not the box
                  ;; itself.  This prevents aliasing when the same boxes appear
                  ;; on both sides, e.g. ($a,$b) = ($b,$a).  box-set logic:
                  ;;   - p-box with non-box inner → store inner (copy semantics)
                  ;;   - p-box with box inner (reference) → store outer box
                  ;;   - p-box with class set (blessed non-hash) → preserve box (bless semantics)
                  ;;   - p-box with vector/hash inner (array/hash ref) → preserve the box
                  ;;   - non-box → store as-is
                  (vector-push-extend
                   (if (p-box-p item)
                       (let ((inner (p-box-value item)))
                         (cond
                           ((or (p-box-p inner)
                                (p-box-class item)
                                (and (vectorp inner) (not (stringp inner)))  ; array ref
                                (hash-table-p inner)  ; hash ref
                                (%p-dualvar-box-p item))  ; $!/dualvar: keep both halves
                            item)   ; reference, blessed, or dualvar: preserve the box
                           ;; A typeglob payload: glob REF vs glob VALUE is the
                           ;; BOX's is-ref flag (#423), so the raw-value snapshot
                           ;; below loses it and box-set re-boxes a `\*foo` as a
                           ;; bare glob (`my $x = shift` on a glob ref printed
                           ;; *main::foo).  Snapshot a FRESH box carrying the
                           ;; flag — preserving ITEM instead would alias, and
                           ;; ($g1,$g2) = ($g2,$g1) would collapse to one glob.
                           ;; Coderefs need no such arm: a raw function is
                           ;; unambiguously a reference.
                           ((p-typeglob-p inner)
                            (let ((nb (make-p-box inner)))
                              (setf (p-box-is-ref nb) (p-box-is-ref item))
                              nb))
                           ;; A MAGIC or TIED source (a defelem @_ alias, an
                           ;; arylen / substr / pos lvalue, a tied scalar):
                           ;; snapshot the VALUE it reads as NOW — perl
                           ;; evaluates the whole RHS before any store — never
                           ;; the cell or the proxy: copying the cell made
                           ;; `my ($x) = @_; $x = 0` write THROUGH the alias and
                           ;; vivify the caller's `$h{k}`, and copying the proxy
                           ;; made the target itself tied (s411, found by
                           ;; PCL_OPT=none: the raw-params fast path hid it).
                           ((p-magic-cell-p inner)
                            (funcall (p-magic-cell-getter inner)))
                           ((p-tie-proxy-p inner)
                            (unbox (%p-tie-fetch item inner)))
                           (t inner)))  ; plain scalar: snapshot value
                       item)
                   result)))))
      (add src))
    result))

(defun %p-hash-keyval-list (h)
  "Flatten a hash-table into a Perl list (k1 v1 k2 v2 ...) of boxed values,
   matching how %hash flattens in list context (same pairing as %p-flatten-list).
   Used by list consumers that flatten %hash args: join, foreach, push, map/grep."
  (let ((result nil))
    (maphash (lambda (k v)
               (when (%p-real-hash-key-p k)
                 (push (make-p-box k) result)
                 (push (if (p-box-p v) v (make-p-box v)) result)))
             h)
    (nreverse result)))

(defmacro p-list-= (place value)
  "List destructuring assignment: (p-list-= (vector $a $b) expr).
   Each LHS element gets assigned from corresponding RHS position.
   Handles undef skip markers, arrays, hashes, nested lvalues, and
   list repetition on LHS: (p-list-x (vector $a) N) repeats the
   assignment N times (last wins); (p-list-x (vector undef) N) skips N
   slots (N may be a runtime expression).
   Returns: list ctx (*wantarray* t) → flat vector of actual LHS values;
            scalar/void ctx → count of RHS elements."
  (let ((vars (cdr place))
        (src (gensym "SRC"))
        (src-vec (gensym "SRC-VEC"))
        (result-var (gensym "LIST-RESULT")))
    (let ((forms nil)
          (collect-forms nil)  ; forms to collect LHS values for list-ctx return
          (static-idx 0)   ; statically-known offset accumulated so far
          (dyn-vars nil)   ; gensyms for dynamic skip counts (pushed most-recent first)
          (extra-lets nil) ; let* bindings for dynamic counts: ((gensym count-expr) ...)
          (greedy-done nil))
      (flet
          ((is-undef-form (v)
             ;; True when v is any form that produces Perl undef used as a skip
             ;; placeholder.  A context wrap around it is peeled by
             ;; %p-strip-ctx — BOTH spellings, so the #281 macros cannot turn a
             ;; placeholder into a real assignment target.
             (let ((v (%p-strip-ctx v)))
               (or (eq v '*p-undef*)
                   (and (listp v)
                        (symbolp (car v))
                        (string= (symbol-name (car v)) "P-UNDEF")))))
           (cur-idx ()
             ;; The current index as a CL literal or form.
             ;; When dynamic skips exist: (+ static-idx dyn1 dyn2 ...)
             (if (null dyn-vars)
                 static-idx
                 `(+ ,static-idx ,@(reverse dyn-vars))))
           (assign-scalar (lvar idx-expr)
             `(progn
                (unless (boundp ',lvar)
                  (%p-ensure-storage (quote ,lvar))
                  (setf (symbol-value ',lvar) (make-p-box nil)))
                (box-set ,lvar (if (< ,idx-expr (length ,src-vec))
                                   (aref ,src-vec ,idx-expr)
                                   *p-undef*)))))

        (dolist (var vars)
          (cond
            ;; Already consumed by greedy (array/hash) — subsequent vars get cleared/undef
            (greedy-done
             ;; Arrays and hashes must be CLEARED (box-set is a no-op on them).
             ;; Scalars: auto-declare and set to undef via box-set.
             (cond
               ((and (symbolp var)
                     (char= (char (symbol-name var) 0) #\@))
                (push `(p-array-= ,var
                                  (make-array 0 :adjustable t :fill-pointer 0))
                      forms))
               ((and (symbolp var)
                     (char= (char (symbol-name var) 0) #\%))
                (push `(p-hash-= ,var
                                 (make-array 0 :adjustable t :fill-pointer 0))
                      forms))
               (t
                (push `(progn
                         (unless (boundp ',var)
                           (%p-ensure-storage (quote ,var))
                           (setf (symbol-value ',var) (make-p-box nil)))
                         (box-set ,var *p-undef*))
                      forms)))
             ;; Collect: hash → maphash (empty after greedy), array → loop,
             ;; scalar → its BOX.  Perl returns every assigned target as an
             ;; LVALUE, including the ones a greedy array/hash starved of
             ;; values: `$_++ foreach ($x,$y,%h,$z) = (0)` must increment $z.
             ;; Pushing *p-undef* here handed the caller a bare value, so the
             ;; write went nowhere (op/hashassign.t t307-t309).  The box was
             ;; just set to undef by the assign form above, so pushing the
             ;; variable itself is both the right value and writable.
             (cond
               ((and (symbolp var)
                     (char= (char (symbol-name var) 0) #\%))
                (push `(maphash (lambda (k v)
                                  (vector-push-extend (make-p-box k) ,result-var)
                                  (vector-push-extend v ,result-var))
                                ,var) collect-forms))
               ((and (symbolp var)
                     (char= (char (symbol-name var) 0) #\@))
                (push `(loop for v across ,var
                             do (vector-push-extend v ,result-var)) collect-forms))
               ((symbolp var)
                (push `(vector-push-extend ,var ,result-var) collect-forms))))

            ;; p-list-x on LHS: (p-list-x (vector ...) count)
            ((and (listp var)
                  (symbolp (car var))
                  (string= (symbol-name (car var)) "P-LIST-X"))
             (let* ((inner-vec (cadr var))
                    (count-form (caddr var))
                    (inner-vars (cdr inner-vec))
                    (all-undef (every #'is-undef-form inner-vars))
                    (inner-len (length inner-vars)))
               (cond
                 ;; All undef, static count: pure skip (original behaviour)
                 ((and all-undef (numberp count-form))
                  (incf static-idx (* count-form inner-len)))

                 ;; All undef, dynamic count: bind gensym for runtime skip amount
                 (all-undef
                  (let* ((dyn-var (gensym "DYN-SKIP"))
                         (count-expr (if (= inner-len 1)
                                         `(max 0 (truncate (to-number ,count-form)))
                                         `(* ,inner-len (max 0 (truncate (to-number ,count-form)))))))
                    (push `(,dyn-var ,count-expr) extra-lets)
                    (push dyn-var dyn-vars)))

                 ;; Has real vars, static count: N-fold assignment (last pass wins)
                 ((numberp count-form)
                  (dotimes (i count-form)
                    (dolist (inner-var inner-vars)
                      (if (is-undef-form inner-var)
                          (incf static-idx 1)
                          (let ((idx (cur-idx)))
                            (push (if (symbolp inner-var)
                                      (assign-scalar inner-var idx)
                                      `(p-setf ,inner-var
                                               (if (< ,idx (length ,src-vec))
                                                   (aref ,src-vec ,idx)
                                                   *p-undef*)))
                                  forms)
                            (incf static-idx 1))))))

                 ;; Has real vars, dynamic count: advance offset by count*inner-len
                 ;; (cannot do per-element assignments without knowing count at macro time)
                 (t
                  (let* ((dyn-var (gensym "DYN-SKIP"))
                         (count-expr `(* ,inner-len (max 0 (truncate (to-number ,count-form))))))
                    (push `(,dyn-var ,count-expr) extra-lets)
                    (push dyn-var dyn-vars))))))

            ;; Skip single undef placeholder: (p-undef), *p-undef*, or
            ;; (let ((*wantarray* t)) (p-undef)) wrapper from wantarray context
            ((is-undef-form var)
             (incf static-idx 1))

            ;; Array variable (@arr) - absorbs remaining elements
            ((and (symbolp var)
                  (char= (char (symbol-name var) 0) #\@))
             (let ((idx (cur-idx)))
               (push `(p-array-= ,var (subseq ,src-vec (min ,idx (length ,src-vec)))) forms)
               ;; Collect: push array elements
               (push `(loop for v across ,var
                            do (vector-push-extend v ,result-var)) collect-forms))
             (setf greedy-done t))

            ;; Hash variable (%hash) - absorbs remaining elements in pairs
            ((and (symbolp var)
                  (char= (char (symbol-name var) 0) #\%))
             (let ((idx (cur-idx)))
               ;; Suppress p-hash-='s list-ctx return since we collect separately
               (push `(let ((*wantarray* :void))
                        (p-hash-= ,var (subseq ,src-vec (min ,idx (length ,src-vec))))) forms)
               ;; Collect: push hash k-v pairs (deduplicated by the hash itself)
               (push `(maphash (lambda (k v)
                                 (vector-push-extend (make-p-box k) ,result-var)
                                 (vector-push-extend v ,result-var))
                               ,var) collect-forms))
             (setf greedy-done t))

            ;; Scalar variable - auto-declare and assign
            ((symbolp var)
             (let ((idx (cur-idx)))
               (push (assign-scalar var idx) forms)
               ;; Collect: push the scalar's box (holds the assigned value)
               (push `(vector-push-extend ,var ,result-var) collect-forms)
               (incf static-idx 1)))

            ;; Array slice on LHS: (@arr[0..2]) or (@arr[i,j,...]) in a list assignment.
            ;; Assigns consecutive RHS elements to each index in the slice.
            ;; (@arr[0..2]) in ($a, @arr[0..2], $e) = (...) consumes 3 RHS slots.
            ((and (listp var) (symbolp (car var))
                  (string= (symbol-name (car var)) "P-ASLICE"))
             (let* ((arr-form (cadr var))
                    (raw-idx-forms (cddr var))
                    (flat-idx (gensym "FLAT-IDX"))
                    (dyn-n (gensym "SLICE-N"))
                    (loop-i (gensym "ASLICE-I"))
                    (loop-j (gensym "ASLICE-J"))
                    (prev-offset (cur-idx)))
               (push `(,flat-idx (%p-flatten-list (list ,@raw-idx-forms))) extra-lets)
               (push `(,dyn-n (length ,flat-idx)) extra-lets)
               (push `(dotimes (,loop-i ,dyn-n)
                        (p-array-set ,arr-form
                                     (truncate (to-number (aref ,flat-idx ,loop-i)))
                                     (if (< (+ ,prev-offset ,loop-i) (length ,src-vec))
                                         (aref ,src-vec (+ ,prev-offset ,loop-i))
                                         *p-undef*)))
                     forms)
               (push `(dotimes (,loop-j ,dyn-n)
                        (vector-push-extend
                         (if (< (+ ,prev-offset ,loop-j) (length ,src-vec))
                             (aref ,src-vec (+ ,prev-offset ,loop-j))
                             *p-undef*)
                         ,result-var))
                     collect-forms)
               (push dyn-n dyn-vars)))

            ;; Other lvalue (hash/array access, etc.) — no collect
            (t
             (let ((idx (cur-idx)))
               (push `(p-setf ,var (if (< ,idx (length ,src-vec))
                                       (aref ,src-vec ,idx)
                                       *p-undef*))
                     forms)
               (incf static-idx 1)))))

        `(let* ((,src (let ((*wantarray* t) (*p-in-list-assign-rhs* t)) ,value))
                (,src-vec (%p-flatten-list ,src))
                ,@(reverse extra-lets))
           ,@(nreverse forms)
           ;; List ctx: collect actual LHS values; scalar/void: return RHS count
           (if (eq *wantarray* t)
               (let ((,result-var (make-array 8 :adjustable t :fill-pointer 0)))
                 ,@(nreverse collect-forms)
                 ,result-var)
               (make-p-box (length ,src-vec))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %p-alias-expansion (place value)
    "Expand a REFALIASING assignment `\\TARGET = VALUE` (use feature
     'refaliasing' / 'declared_refs').  TARGET is the form the emitter produced
     for the \\-cast's operand, so this is the ONE place that knows what a
     \\-lvalue means; every spelling (`\\$x = ...`, `\\my @b = ...`,
     `(\\$x) = @_` through p-list-='s default arm, `our \\$T = ...`) arrives
     here as a p-setf place.

     An alias is NOT a value write: it REBINDS THE NAME'S STORAGE to the object
     the right-hand reference points at.  In PCL a scalar variable holds a
     p-box, an array variable holds the vector and a hash variable the
     hash-table, so `\\$x = \\$y` is `(setq $x <$y's box>)` — after it both
     names denote the same object, which is exactly perl's aliasing, and
     `\\$x == \\$y` follows for free (a ref stringifies/numifies from its
     referent's identity).

     A container SLOT (`\\$h{k} = \\$v`) is the same move one level down: the
     slot's box is replaced by the referent box, which every reader already
     unboxes.

     Rule 12: an unhandled target form is a compiler gap, not a default — it
     dies here, naming the form, rather than falling through to the value
     write that made `\\$x = \\$y` a silent no-op before this existed."
    (let ((target (second place)))
      (flet ((sigil (sym) (char (symbol-name sym) 0)))
        (cond
          ;; \&c = \&d — a code alias is a glob-slot store, so the two names
          ;; share one function object (and one coderef identity).
          ((eq (car place) 'p-backslash-sub)
           `(setf (symbol-function ,target) (p-alias-code-target ,value)))
          ;; \(@a) = LIST — the parenthesized-ARRAY spelling.  Not a rebind of
          ;; the NAME (that is \@a = REF above) but of each element SLOT, and
          ;; the array is resized to the right-hand length; the emitter hands
          ;; the array here as a p-backslash-list place.  *wantarray* t: the
          ;; right-hand side is a list, exactly as in p-array-=.
          ((eq (car place) 'p-backslash-list)
           `(p-alias-array-elements ,target (let ((*wantarray* t)) ,value)))
          ;; \$x = REF — rebind the name to the referent BOX.  setq covers both
          ;; storage kinds: a `let`-bound lexical and a p-defcell symbol macro
          ;; (which expands to a setf of the global cell).
          ((and (symbolp target) (char= (sigil target) #\$))
           `(setq ,target (p-alias-scalar-target ,value)))
          ;; \@a = REF / \%h = REF — rebind to the referent vector / hash-table.
          ((and (symbolp target) (char= (sigil target) #\@))
           `(setq ,target (p-alias-array-target ,value)))
          ((and (symbolp target) (char= (sigil target) #\%))
           `(setq ,target (p-alias-hash-target ,value)))
          ;; \$h{k} = REF, \$a[i] = REF — replace the SLOT's box.  All four
          ;; spellings of an element place mean the same slot: the -box twins
          ;; (lvalue position) and the plain accessors (which is what a list
          ;; assignment's element forms are), each also in its deref flavour
          ;; for `\$ref->[0]`.  The container argument differs in shape between
          ;; them — raw vector, box, ref — so the helpers resolve it through
          ;; p-cast-@ / p-cast-%, which already knows every one.
          ((and (consp target)
                (member (car target) '(p-gethash p-gethash-box
                                       p-gethash-deref p-gethash-deref-box)))
           `(p-alias-hash-slot ,(second target) ,(third target) ,value))
          ((and (consp target)
                (member (car target) '(p-aref p-aref-box
                                       p-aref-deref p-aref-deref-box)))
           `(p-alias-array-slot ,(second target) ,(third target) ,value))
          (t
           (error "PCL: refaliasing target not supported: ~S" target))))))

  (defun %p-alias-place-p (place)
    "True for a p-setf place that is a \\-cast, i.e. a refaliasing assignment."
    (and (consp place)
         (member (car place) '(p-backslash p-backslash-sub p-backslash-list)))))

(defun p-alias-scalar-target (ref)
  "The BOX a scalar reference points at, for `\\$x = REF`.

   Two shapes reach here and IS-REF is what separates them (that is the flag's
   job — p-backslash sets it on the wrapper it makes):
     `\\$x = \\$y`   — REF is the \\-wrapper itself, so the referent is its value;
     `\\$x = $r`    — REF is a VARIABLE whose value is the wrapper, one deeper.
   Reading the wrapper's identity rather than counting box layers keeps a
   reference-to-a-reference right, where a layer count would peel one too many.
   Perl's own diagnostic when the right-hand side is not a scalar reference."
  (let* ((wrapper (if (and (p-box-p ref)
                           (not (p-box-is-ref ref))
                           (p-box-p (p-box-value ref))
                           (p-box-is-ref (p-box-value ref)))
                      (p-box-value ref)
                      ref))
         (inner (unbox wrapper)))
    (if (p-box-p inner)
        inner
        (error "Assigned value is not a SCALAR reference"))))

(defun p-alias-array-target (ref)
  "The VECTOR an array reference points at, for `\\@a = REF`."
  (let ((v (p-cast-@ ref)))
    (if (and (vectorp v) (not (stringp v)))
        v
        (error "Assigned value is not an ARRAY reference"))))

(defun p-alias-hash-target (ref)
  "The HASH-TABLE a hash reference points at, for `\\%h = REF`."
  (let ((h (p-cast-% ref)))
    (if (hash-table-p h)
        h
        (error "Assigned value is not a HASH reference"))))

(defun p-alias-code-target (ref)
  "The FUNCTION a code reference points at, for `\\&c = REF`."
  (let ((f (unbox ref)))
    (if (functionp f)
        f
        (error "Assigned value is not a CODE reference"))))

(defun p-alias-array-elements (arr refs)
  "`\\(@a) = LIST` — perlref's parenthesized-ARRAY refaliasing: each element
   SLOT of @a becomes the scalar its right-hand reference points at.  Perl
   REPLACES the contents rather than merging, so the array ends up exactly as
   long as the right-hand list (probed: `my @a=(7,8,9); \\(@a) = (\\$x,\\$y)`
   leaves two elements).  Referents resolve through p-alias-scalar-target —
   the same helper the `\\$x = REF` arm uses — so a ref-to-a-ref, and a
   variable holding the ref, stay right here too.  %p-flatten-list is what
   spreads the right-hand side, because it PRESERVES a reference box while
   snapshotting a plain scalar's value, which is precisely the split we need."
  (let ((v (p-cast-@ arr))
        (items (%p-flatten-list refs)))
    (unless (and (vectorp v) (not (stringp v)))
      (error "Not an ARRAY reference"))
    (%p-check-array-writable v)
    (setf (fill-pointer v) 0)
    (loop for r across items
          do (vector-push-extend (p-alias-scalar-target r) v))
    v))

(defun p-alias-hash-slot (hash key ref)
  "`\\$h{k} = REF` — make the slot hold the referent box itself.  HASH is
   whatever the element place's container argument evaluates to (a raw hash, a
   box, or a hash ref for the deref spellings); p-cast-% resolves all three."
  (let ((h (p-cast-% hash))
        (box (p-alias-scalar-target ref)))
    (unless (hash-table-p h)
      (error "Not a HASH reference"))
    (setf (gethash (to-string key) h) box)))

(defun p-alias-array-slot (arr idx ref)
  "`\\$a[i] = REF` — make the slot hold the referent box itself.  See
   p-alias-hash-slot for why the container goes through the cast."
  (let* ((a (p-cast-@ arr))
         (box (p-alias-scalar-target ref))
         (i (truncate (to-number idx))))
    (unless (and (vectorp a) (not (stringp a)))
      (error "Not an ARRAY reference"))
    (let ((n (if (< i 0) (+ (length a) i) i)))
      (when (< n 0) (error "Modification of non-creatable array value attempted"))
      (loop while (>= n (length a)) do (vector-push-extend nil a))
      (setf (aref a n) box))))

;; p-setf dispatches to the appropriate assignment form based on place type.
;; For element access (p-aref, p-gethash, etc.), uses CL's setf mechanism.
(defmacro p-setf (place value)
  "Perl assignment - dispatches to type-specific forms or uses CL setf for element access."
  (cond
    ;; Reference aliasing: the lvalue is a \-cast, so this assignment REBINDS
    ;; the name's storage to the right-hand referent instead of writing a
    ;; value.  Kept first: a \-cast place can never mean anything else.
    ((%p-alias-place-p place)
     (%p-alias-expansion place value))
    ;; Array variable (symbol starting with @) -> p-array-=
    ((and (symbolp place)
          (char= (char (symbol-name place) 0) #\@))
     `(p-array-= ,place ,value))
    ;; Hash variable (symbol starting with %) -> p-hash-=
    ((and (symbolp place)
          (char= (char (symbol-name place) 0) #\%))
     `(p-hash-= ,place ,value))
    ;; Simple scalar variable -> p-scalar-=
    ((symbolp place)
     `(p-scalar-= ,place ,value))
    ;; Hash access with simple symbol - auto-declare hash if needed
    ((and (listp place)
          (eq (car place) 'p-gethash)
          (symbolp (cadr place)))
     (let ((hash (cadr place))
           (key (caddr place))
           (val (gensym "VAL")))
       `(let ((,val ,value))
          (unless (boundp ',hash)
            (%p-ensure-storage (quote ,hash))
            (setf (symbol-value ',hash) (make-hash-table :test 'equal)))
          (setf (p-gethash ,hash ,key) ,val))))
    ;; Array access with simple symbol - auto-declare array if needed
    ((and (listp place)
          (eq (car place) 'p-aref)
          (symbolp (cadr place)))
     (let ((arr (cadr place))
           (idx (caddr place))
           (val (gensym "VAL")))
       `(let ((,val ,value))
          (unless (boundp ',arr)
            (%p-ensure-storage (quote ,arr))
            (setf (symbol-value ',arr) (make-array 0 :adjustable t :fill-pointer 0)))
          (setf (p-aref ,arr ,idx) ,val))))
    ;; Nested hash access - autovivification
    ;; (p-gethash (p-gethash ... ) key) = value         ($h{a}{b})
    ;; (p-gethash (p-aref   ... ) key) = value          ($a[N]{k})
    ;; In both cases the container slot must vivify to a hash before the store.
    ;; expand-autoviv already handles a p-aref inner form (-> p-autoviv-aref-for-hash);
    ;; this dispatch arm just has to route it there instead of the plain (setf ...)
    ;; fallthrough, which would (setf (p-gethash :UNDEF ...) ...) on an empty array.
    ((and (listp place)
          (eq (car place) 'p-gethash)
          (listp (cadr place))
          (member (car (cadr place)) '(p-gethash p-aref)))
     (let ((outer-key (caddr place))
           (val (gensym "VAL")))
       `(let ((,val ,value))
          (p-autoviv-set ,(cadr place) ,outer-key ,val))))
    ;; Array element in hash chain - autovivification
    ;; (p-aref (p-gethash ... ) idx) = value
    ((and (listp place)
          (eq (car place) 'p-aref)
          (listp (cadr place))
          (eq (car (cadr place)) 'p-gethash))
     (let ((hash-chain (cadr place))
           (idx (caddr place))
           (val (gensym "VAL")))
       `(let ((,val ,value))
          (p-autoviv-aref-set ,hash-chain ,idx ,val))))
    ;; Array element via hash-ref deref - autovivification
    ;; (p-aref (p-gethash-deref $ref key) idx) = value  ($ref->{key}[idx])
    ((and (listp place)
          (eq (car place) 'p-aref)
          (listp (cadr place))
          (eq (car (cadr place)) 'p-gethash-deref))
     (let ((hash-chain (cadr place))
           (idx (caddr place))
           (val (gensym "VAL")))
       `(let ((,val ,value))
          (p-autoviv-aref-set ,hash-chain ,idx ,val))))
    ;; Array element via array-ref deref - autovivification
    ;; (p-aref (p-aref-deref $ref i) idx) = value  ($ref->[i][idx])
    ((and (listp place)
          (eq (car place) 'p-aref)
          (listp (cadr place))
          (eq (car (cadr place)) 'p-aref-deref))
     (let ((hash-chain (cadr place))
           (idx (caddr place))
           (val (gensym "VAL")))
       `(let ((,val ,value))
          (p-autoviv-aref-set ,hash-chain ,idx ,val))))
    ;; Array element via nested array element - autovivification of inner array ref
    ;; (p-aref (p-aref OUTER I) J) = value  ($outer[$i][$j])
    ((and (listp place)
          (eq (car place) 'p-aref)
          (listp (cadr place))
          (eq (car (cadr place)) 'p-aref))
     (let ((arr-chain (cadr place))
           (idx (caddr place))
           (val (gensym "VAL")))
       `(let ((,val ,value))
          (p-autoviv-aref-set ,arr-chain ,idx ,val))))
    ;; Hash element via hash-ref deref chain - autovivification
    ;; (p-gethash (p-gethash-deref $ref key) key2) = value  ($ref->{key}{key2})
    ((and (listp place)
          (eq (car place) 'p-gethash)
          (listp (cadr place))
          (member (car (cadr place)) '(p-gethash-deref p-aref-deref)))
     (let ((outer-key (caddr place))
           (val (gensym "VAL")))
       `(let ((,val ,value))
          (p-autoviv-set ,(cadr place) ,outer-key ,val))))
    ;; Array/hash ref access and scalar deref - use CL setf
    ((and (listp place)
          (member (car place) '(p-aref-deref p-gethash-deref p-$ p-cast-$)))
     `(setf ,place ,value))
    ;; Array/hash access with complex expression (not simple symbol) - use CL setf
    ((and (listp place)
          (member (car place) '(p-aref p-gethash)))
     `(setf ,place ,value))
    ;; List assignment: (vector $a $b $c) = @_ or similar -> p-list-=
    ((and (listp place) (eq (car place) 'vector))
     `(p-list-= ,place ,value))
    ;; Array slice assignment: (p-setf (p-aslice arr indices...) values)
    ;; Assigns each value from RHS to the corresponding index in LHS
    ((and (listp place) (eq (car place) 'p-aslice))
     (let ((arr (cadr place))
           (indices-exprs (cddr place))
           (src (gensym "SRC"))
           (src-vec (gensym "SRC-VEC"))
           (indices (gensym "INDICES")))
       ;; The assignment proper; a simple-symbol array is auto-declared first
       ;; (#387 family 46, s413: the body was spelled once per case).
       (let ((body
              `(let* ((,src ,value)
                      ;; Convert source to vector
                      (,src-vec (cond
                                  ((listp ,src) (coerce ,src 'vector))
                                  ((and (vectorp ,src) (not (stringp ,src))) ,src)
                                  (t (vector ,src))))
                      ;; Flatten indices (handle range operator returning vector or list)
                      (,indices (let ((idx-list nil))
                                  (dolist (idx (list ,@indices-exprs) (nreverse idx-list))
                                    (cond
                                      ((listp idx)
                                       (dolist (i idx) (push i idx-list)))
                                      ((and (vectorp idx) (not (stringp idx)))
                                       (loop for i across idx do (push i idx-list)))
                                      (t (push idx idx-list)))))))
                 ;; Assign each element
                 (loop for i from 0 below (length ,indices)
                       for idx in ,indices
                       do (setf (p-aref ,arr idx)
                                (if (< i (length ,src-vec))
                                    (aref ,src-vec i)
                                    *p-undef*)))
                 ;; Return the values that were assigned
                 ,src-vec)))
         (if (symbolp arr)
             `(progn
                (unless (boundp ',arr)
                  (%p-ensure-storage (quote ,arr))
                  (setf (symbol-value ',arr) (make-array 0 :adjustable t :fill-pointer 0)))
                ,body)
             ;; Non-symbol array expression - just use it directly
             body))))
    ;; Hash slice assignment: (p-setf (p-hslice hash keys...) values)
    ((and (listp place) (eq (car place) 'p-hslice))
     (let ((hash (cadr place))
           (keys-exprs (cddr place))
           (src (gensym "SRC"))
           (src-vec (gensym "SRC-VEC"))
           (keys (gensym "KEYS")))
       ;; The assignment proper; a simple-symbol hash is auto-declared first
       ;; (#387 family 46, s413: the body was spelled once per case).
       (let ((body
              `(let* ((,src ,value)
                      (,src-vec (cond
                                  ((listp ,src) (coerce ,src 'vector))
                                  ((and (vectorp ,src) (not (stringp ,src))) ,src)
                                  (t (vector ,src))))
                      (,keys (let ((key-list nil))
                               (dolist (k (list ,@keys-exprs) (nreverse key-list))
                                 (cond
                                   ((listp k)
                                    (dolist (kk k) (push kk key-list)))
                                   ((and (vectorp k) (not (stringp k)))
                                    (loop for kk across k do (push kk key-list)))
                                   (t (push k key-list)))))))
                 (loop for i from 0 below (length ,keys)
                       for k in ,keys
                       do (setf (p-gethash ,hash k)
                                (if (< i (length ,src-vec))
                                    (aref ,src-vec i)
                                    *p-undef*)))
                 ,src-vec)))
         (if (symbolp hash)
             `(progn
                (unless (boundp ',hash)
                  (%p-ensure-storage (quote ,hash))
                  (setf (symbol-value ',hash) (make-hash-table :test 'equal)))
                ,body)
             ;; Non-symbol hash expression
             body))))
    ;; $! as lvalue: (p-setf (p-errno-string) val) -> set C errno
    ((and (listp place) (eq (car place) 'p-errno-string))
     `(setf (p-errno-string) ,value))
    ;; pos as lvalue: (p-setf (p-pos var) new-val) -> (p-pos var new-val)
    ((and (listp place) (eq (car place) 'p-pos))
     `(p-pos ,(cadr place) ,value))
    ;; vec as lvalue: (p-setf (p-vec str offset bits) val) -> (p-vec-set str offset bits val)
    ((and (listp place) (eq (car place) 'p-vec))
     (let ((str-place (cadr place))
           (offset    (caddr place))
           (bits      (cadddr place)))
       `(p-vec-set ,str-place ,offset ,bits ,value)))
    ;; substr as lvalue: (p-setf (p-substr str start [len]) val) -> (p-substr str start len val)
    ;; 2-arg form needs nil inserted for len so value lands in replacement slot.
    ;; 4-arg form (4-arg substr used as lvalue) is a Perl error.
    ((and (listp place) (eq (car place) 'p-substr))
     (let ((args (cdr place)))
       (cond
         ((= (length args) 2)
          `(p-substr ,(car args) ,(cadr args) nil ,value))
         ((= (length args) 4)
          `(error "Can't modify substr in scalar assignment"))
         (t `(p-substr ,@args ,value)))))
    ;; Other complex place (fallback)
    (t `(box-set ,place ,value))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %p-accessor-place-p (place)
    "True for value-returning element/deref accessor forms (hash/array element
     or deref).  These are not boxes — they return raw values and have setf
     expanders — so a compound assignment must write them back with SETF, not
     BOX-SET (which silently no-ops on a non-box).  Mirrors the place test in
     p-incf/p-decf so every compound-assignment operator works on $a[i]/$h{k}
     and their deref forms, e.g. Math::BigInt::Calc's `$xv->[0] *= $yv->[0]`."
    (and (consp place)
         (member (car place)
                 '(p-gethash p-aref p-gethash-deref p-aref-deref)))))

(defmacro %p-store-back (place new)
  "Write NEW into PLACE for a read-modify-write compound assignment: SETF for
   accessor places, BOX-SET for boxed scalars and scalar derefs."
  (if (%p-accessor-place-p place)
      `(setf ,place ,new)
      `(box-set ,place ,new)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %store-back-form (place build)
    "Build a read-modify-write expansion that evaluates PLACE exactly once.
     BUILD is a function of one argument — a form that reads the current place —
     returning the new-value form.  For a boxed scalar place, PLACE is bound to a
     temp box once and BUILD reads through that temp, so a nested compound-assign
     lvalue chain like (($a .= $a) .= $a) runs PLACE's side effects only once
     (otherwise each textual reference to PLACE re-ran the inner assignment,
     growing the result exponentially).  For accessor places ($h{k}/$a[i]/derefs)
     SETF is used on the syntactic place form."
    (if (%p-accessor-place-p place)
        `(setf ,place ,(funcall build place))
        (let ((b (gensym "PLACE")))
          `(let ((,b ,place))
             (box-set ,b ,(funcall build b)))))))

(defmacro %define-compound-pair (boxed raw lambda-list doc &body new-value)
  "Define the BOXED compound-assign macro (any place: boxes, elements, derefs —
   store-back via %store-back-form) and its RAW twin, which requires the place
   to be a raw let-bound lexical slot (docs/raw-numeric-verdict.md) and stores
   with plain SETF.  NEW-VALUE is a form-template expression evaluated with CUR
   bound to the current-value form and the LAMBDA-LIST names bound to the
   macro's argument forms.  One builder, two store disciplines — the raw twin
   cannot drift semantically from the boxed macro."
  `(progn
     (defmacro ,boxed (place ,@lambda-list)
       ,doc
       (%store-back-form place (lambda (cur) ,@new-value)))
     (defmacro ,raw (var ,@lambda-list)
       ,doc
       (list 'setf var (let ((cur var)) ,@new-value)))))

(%define-compound-pair p-incf p-incf-raw (&optional (delta 1))
                       "Perl += - works on boxed values, hash/array elements, and derefs.
   Coerce the current value through to-number BEFORE adding: an absent key/slot
   reads as *p-undef*, which raw (+ …) cannot handle — Perl treats it as 0.
   PLACE is evaluated once (see %store-back-form)."
                       `(+ (to-number ,cur) (to-number ,delta)))

(%define-compound-pair p-decf p-decf-raw (&optional (delta 1))
                       "Perl -= - works on boxed values, hash/array elements, and derefs.
   PLACE is evaluated once (see %store-back-form)."
                       `(- (to-number ,cur) (to-number ,delta)))

(defun magical-string-increment (s)
  "Perl's magical string increment: 'a0' -> 'a1', 'Az' -> 'Ba', 'zz' -> 'aaa'"
  (let* ((len (length s))
         (chars (copy-seq s))
         (i (1- len))
         (carry t))
    (loop while (and carry (>= i 0)) do
          (let ((c (char chars i)))
            (cond
              ;; Digit 0-8 -> increment, no carry
              ((and (char>= c #\0) (char< c #\9))
               (setf (char chars i) (code-char (1+ (char-code c))))
               (setf carry nil))
              ;; Digit 9 -> 0, carry
              ((char= c #\9)
               (setf (char chars i) #\0))
              ;; Lowercase a-y -> increment, no carry
              ((and (char>= c #\a) (char< c #\z))
               (setf (char chars i) (code-char (1+ (char-code c))))
               (setf carry nil))
              ;; Lowercase z -> a, carry
              ((char= c #\z)
               (setf (char chars i) #\a))
              ;; Uppercase A-Y -> increment, no carry
              ((and (char>= c #\A) (char< c #\Z))
               (setf (char chars i) (code-char (1+ (char-code c))))
               (setf carry nil))
              ;; Uppercase Z -> A, carry
              ((char= c #\Z)
               (setf (char chars i) #\A))
              ;; Non-alphanumeric: stop magical increment, treat as numeric
              (t
               (return-from magical-string-increment (1+ (to-number s))))))
          (decf i))
    ;; If we still have carry after processing all chars, prepend
    (if carry
        (concatenate 'string
                     (string (cond
                               ((digit-char-p (char chars 0)) #\1)
                               ((upper-case-p (char chars 0)) #\A)
                               (t #\a)))
                     chars)
        chars)))

(defun perl-increment (val)
  "Perl ++ semantics: magical string increment for certain strings, numeric otherwise"
  (let ((v (unbox val)))
    (cond
      ;; If it's already a number, just add 1
      ((numberp v) (1+ v))
      ;; If it's a string matching /^[a-zA-Z]*[0-9]*$/ (letters then optional digits),
      ;; use magical string increment. Strings like "99a" (digits then letter) do NOT match.
      ((and (stringp v)
            (> (length v) 0)
            (cl-ppcre:scan "^[a-zA-Z]*[0-9]*$" v))
       (magical-string-increment v))
      ;; Otherwise convert to number and increment
      (t (1+ (to-number v))))))

(defmacro p-pre++ (place)
  "Perl prefix ++ - works on boxed values, hash/array elements, and derefs.
   Supports magical string increment for alphanumeric strings."
  ;; Handle case where place is wrapped in (vector ...) from list context parsing
  (let ((real-place (if (and (listp place) (eq (car place) 'vector) (= (length place) 2))
                        (cadr place)
                        place)))
    (cond
      ;; Box-returning accessors (p-aref-box, p-gethash-box) - get box and modify it
      ((and (listp real-place)
            (member (car real-place) '(p-aref-box p-gethash-box)))
       (let ((box (gensym "BOX")))
         `(let* ((,box ,real-place))
            (box-set ,box (perl-increment ,box)))))
      ;; Traditional setf-able places (p-aref, p-gethash, etc)
      ((and (listp real-place)
            (member (car real-place) '(p-gethash p-aref p-gethash-deref p-aref-deref p-$ p-cast-$)))
       (let ((tmp (gensym "TMP")))
         `(let ((,tmp (perl-increment ,real-place)))
            (setf ,real-place ,tmp)
            ,tmp)))
      ;; Boxed scalar
      (t `(box-set ,real-place (perl-increment ,real-place))))))

(defmacro p-post++ (place)
  "Perl postfix ++ - returns old value.
   Supports magical string increment for alphanumeric strings."
  ;; Handle case where place is wrapped in (vector ...) from list context parsing
  (let* ((real-place (if (and (listp place) (eq (car place) 'vector) (= (length place) 2))
                         (cadr place)
                         place))
         (old (gensym "OLD"))
         (box (gensym "BOX")))
    (cond
      ;; Box-returning accessors (p-aref-box, p-gethash-box) - get box and modify it
      ;; undef (nil) is treated as 0 for post-increment, matching Perl's numeric coercion
      ((and (listp real-place)
            (member (car real-place) '(p-aref-box p-gethash-box)))
       `(let* ((,box ,real-place)
               (,old (let ((v (unbox ,box))) (if (or (null v) (eq v *p-undef*)) 0 v))))
          (box-set ,box (perl-increment ,box))
          ,old))
      ;; p-cast-$ (scalar deref): may return a mutable box (chain ref→box→value).
      ;; Capture the VALUE before mutation so post-increment returns the old value,
      ;; not the box that was later mutated.  e.g. ${$_[0]}++ where $o = bless \$x, ...
      ((and (listp real-place) (eq (car real-place) 'p-cast-$))
       `(let* ((,box ,real-place)
               (,old (if (p-box-p ,box) (p-box-value ,box) ,box)))
          (setf ,real-place (perl-increment ,box))
          ,old))
      ;; Traditional setf-able places (p-aref, p-gethash, etc)
      ((and (listp real-place)
            (member (car real-place) '(p-gethash p-aref p-gethash-deref p-aref-deref p-$ p-cast-$)))
       `(let ((,old ,real-place))
          (setf ,real-place (perl-increment ,real-place))
          ,old))
      ;; Boxed scalar - return the original value (string or number).
      ;; When value is nil (Perl undef), return 0 — Perl's undef++ returns 0
      ;; because ++ treats undef as 0 in numeric context.
      (t (let ((val (gensym "VAL")))
           `(let* ((,val (unbox ,real-place))
                   (,old (if (or (null ,val) (eq ,val *p-undef*)) 0 ,val)))
              (box-set ,real-place (perl-increment ,real-place))
              ,old))))))

(defmacro p-pre-- (place)
  "Perl prefix -- - works on boxed values, hash/array elements, and derefs"
  ;; Handle case where place is wrapped in (vector ...) from list context parsing
  (let ((real-place (if (and (listp place) (eq (car place) 'vector) (= (length place) 2))
                        (cadr place)
                        place)))
    (cond
      ;; Box-returning accessors (p-aref-box, p-gethash-box) - get box and modify it
      ((and (listp real-place)
            (member (car real-place) '(p-aref-box p-gethash-box)))
       (let ((box (gensym "BOX")))
         `(let* ((,box ,real-place))
            (box-set ,box (1- (to-number ,box))))))
      ;; Traditional setf-able places (p-aref, p-gethash, etc)
      ((and (listp real-place)
            (member (car real-place) '(p-gethash p-aref p-gethash-deref p-aref-deref p-$ p-cast-$)))
       `(decf ,real-place))
      ;; Boxed scalar
      (t `(box-set ,real-place (1- (to-number ,real-place)))))))

(defmacro p-post-- (place)
  "Perl postfix -- - returns old value"
  ;; Handle case where place is wrapped in (vector ...) from list context parsing
  (let* ((real-place (if (and (listp place) (eq (car place) 'vector) (= (length place) 2))
                         (cadr place)
                         place))
         (old (gensym "OLD"))
         (box (gensym "BOX")))
    (cond
      ;; Box-returning accessors (p-aref-box, p-gethash-box) - get box and modify it.
      ;; Return the RAW old value: unlike ++, postfix -- on undef returns undef
      ;; (Perl: `$x--` with $x undef yields undef, then sets $x to -1).
      ((and (listp real-place)
            (member (car real-place) '(p-aref-box p-gethash-box)))
       `(let* ((,box ,real-place)
               (,old (unbox ,box)))
          (box-set ,box (1- (to-number ,box)))
          ,old))
      ;; p-cast-$ (scalar deref): may return a mutable box — capture VALUE before mutation
      ((and (listp real-place) (eq (car real-place) 'p-cast-$))
       `(let* ((,box ,real-place)
               (,old (to-number (if (p-box-p ,box) (p-box-value ,box) ,box))))
          (setf ,real-place (1- ,old))
          ,old))
      ;; Traditional setf-able places (p-aref, p-gethash, etc)
      ((and (listp real-place)
            (member (car real-place) '(p-gethash p-aref p-gethash-deref p-aref-deref p-$ p-cast-$)))
       `(let ((,old ,real-place))
          (decf ,real-place)
          ,old))
      ;; Boxed scalar — return the RAW old value (string or number).  Postfix --
      ;; on undef returns undef (NOT 0 like ++), so do not numify the old value.
      (t (let ((val (gensym "VAL")))
           `(let ((,val (unbox ,real-place)))
              (box-set ,real-place (1- (to-number ,real-place)))
              ,val))))))

;;; ------------------------------------------------------------
;;; Compound Assignment Operators
;;; ------------------------------------------------------------

(%define-compound-pair p-*= p-*=-raw (value)
                       "Perl *= (multiply-assign)"
                       `(* (to-number ,cur) (to-number ,value)))

(%define-compound-pair p-/= p-/=-raw (value)
                       "Perl /= (divide-assign).  Delegate to p-/ so an exact CL ratio is coerced to
  a float (7/2 -> 3.5, not the leaked ratio \"7/2\") and overload '/' dispatches."
                       `(p-/ ,cur ,value))

(%define-compound-pair p-%= p-%=-raw (value)
                       "Perl %= (modulo-assign)"
                       `(mod (truncate (to-number ,cur)) (truncate (to-number ,value))))

(%define-compound-pair p-**= p-**=-raw (value)
                       "Perl **= (exponent-assign).  Delegate to p-** so a negative exponent yields a
  float (2 ** -1 -> 0.5, not the leaked ratio \"1/2\") and overload '**' dispatches."
                       `(p-** ,cur ,value))

(%define-compound-pair p-.= p-.=-raw (value)
                       "Perl .= (concat-assign)"
                       `(concatenate 'string (to-string ,cur) (to-string ,value)))

(%define-compound-pair p-str-x= p-str-x=-raw (value)
                       "Perl x= (repeat-assign)"
                       (let ((n (gensym "N")))
                         `(let ((,n (truncate (to-number ,value))))
                            (if (<= ,n 0) ""
                                (apply #'concatenate 'string
                                       (make-list ,n :initial-element (to-string ,cur)))))))

(%define-compound-pair p-bit-and= p-bit-and=-raw (value)
                       "Perl &= (bitwise-and-assign)"
                       `(p-bit-and ,cur ,value))

(%define-compound-pair p-bit-or= p-bit-or=-raw (value)
                       "Perl |= (bitwise-or-assign)"
                       `(p-bit-or ,cur ,value))

(%define-compound-pair p-bit-xor= p-bit-xor=-raw (value)
                       "Perl ^= (bitwise-xor-assign)"
                       `(p-bit-xor ,cur ,value))

(%define-compound-pair p-<<= p-<<=-raw (value)
                       "Perl <<= (left-shift-assign)"
                       `(ash (truncate (to-number ,cur)) (truncate (to-number ,value))))

(%define-compound-pair p->>= p->>=-raw (value)
                       "Perl >>= (right-shift-assign)"
                       `(ash (truncate (to-number ,cur)) (- (truncate (to-number ,value)))))

;;; Compound conditional-assignment operators (&&=, ||=, //=).
;;;
;;; The store is delegated to p-setf rather than box-set on the read result.
;;; box-set only works when the place already holds a shared box, but a hash/
;;; array element that does not yet exist reads as *p-undef* (p-gethash returns
;;; undef for absent keys, never a stored box), and a nested place needs its
;;; intermediate containers autovivified — exactly what p-setf already does for
;;; every place shape.  We read the place once (plain rvalue read, which never
;;; autovivifies) to test the condition; the RHS is evaluated only on the branch
;;; that stores, matching Perl's short-circuit semantics.  (Subscript subforms
;;; in `place` are evaluated twice — once for the read, once in p-setf — which is
;;; harmless for the variable/constant subscripts that occur in practice.)
(defmacro p-and-assign (place value)
  "Perl &&= (and-assign) - assigns value only if place is true."
  (let ((cur (gensym "CUR")))
    `(let ((,cur ,place))
       (if (p-true-p ,cur)
           (p-setf ,place ,value)
           ,cur))))

(defmacro p-or-assign (place value)
  "Perl ||= (or-assign) - assigns value only if place is false."
  (let ((cur (gensym "CUR")))
    `(let ((,cur ,place))
       (if (p-true-p ,cur)
           ,cur
           (p-setf ,place ,value)))))

(defmacro p-//= (place value)
  "Perl //= (defined-or-assign) - assigns value only if place is undef."
  (let ((cur (gensym "CUR")))
    `(let ((,cur ,place))
       (if (%pcl-definedp ,cur)
           ,cur
           (p-setf ,place ,value)))))

;;; ============================================================
;;; Numeric Comparison
;;; ============================================================

;;; use overload — helper macro for binary comparison operators.
;;; Checks op-specific handler first, then falls back to the parent
;;; three-way operator (<=> for numeric, cmp for string) if available.
(declaim (inline %pcl-nan-p))
(defun %pcl-nan-p (x)
  "True if x is a floating-point NaN."
  (and (floatp x) (sb-ext:float-nan-p x)))
(declaim (notinline %pcl-nan-p))

(declaim (inline p-bool))
(defun p-bool (x)
  "Map a CL boolean to a Perl boolean scalar: true → 1, false → \"\".
   Perl comparison operators return 1 for true and the empty string (which is
   *defined*) for false — NOT undef.  This matters for `defined(2==3)` (true)
   and `(2==3) // 4` (yields \"\", not 4)."
  (if x 1 ""))
(declaim (notinline p-bool))

;;; Each comparison compiles to an INLINE wrapper: two raw CL numbers take
;;; the native CL test directly (with the IEEE NaN rule); anything else goes
;;; through the out-of-line overload/coercion slow path.
(defmacro %def-overloaded-cmp (name op-str fallback-op cl-test nan-result)
  (let ((slow (intern (concatenate 'string "%" (symbol-name name) "-SLOW") :pcl)))
    `(progn
       (defun ,slow (a b)
         ,(format nil "Perl ~A slow path: use overload dispatch (returns 1 / \"\")" op-str)
         ;; use overload: check op-specific handler, then fallback to <=> or cmp.
         ;; Every branch yields a CL boolean; p-bool maps it to Perl's 1 / "".
         (p-bool
          (let ((ha (p-find-overload a ,op-str)))
            (if ha (p-true-p (p-call-overload ha a b nil))
                (let ((hb (p-find-overload b ,op-str)))
                  (if hb (p-true-p (p-call-overload hb b a t))
                      ;; use overload fallback: derive from three-way if available
                      (let ((fa (p-find-overload a ,fallback-op))
                            (fb (p-find-overload b ,fallback-op)))
                        (if (or fa fb)
                            (,cl-test (to-number (if fa
                                                     (p-call-overload fa a b nil)
                                                     (p-call-overload fb b a t)))
                                      0)
                            ;; IEEE 754: any comparison with NaN → nan-result
                            (let ((na (to-number a)) (nb (to-number b)))
                              (if (or (%pcl-nan-p na) (%pcl-nan-p nb))
                                  ,nan-result
                                  (,cl-test na nb)))))))))))
       (declaim (inline ,name))
       (defun ,name (a b)
         ,(format nil "Perl ~A with numberp fast path (returns 1 / \"\")" op-str)
         (if (and (numberp a) (numberp b))
             (p-bool (if (or (%pcl-nan-p a) (%pcl-nan-p b))
                         ,nan-result
                         (,cl-test a b)))
             (,slow a b)))
       ;; expansion stored; plain calls inside the runtime (see file top/end)
       (declaim (notinline ,name)))))

(%def-overloaded-cmp p-==  "=="  "<=>"  =   nil)   ; NaN==NaN → false
(%def-overloaded-cmp p-!=  "!="  "<=>"  /=  t)     ; NaN!=NaN → true
(%def-overloaded-cmp p-<   "<"   "<=>"  <   nil)   ; NaN<x → false
(%def-overloaded-cmp p->   ">"   "<=>"  >   nil)   ; NaN>x → false
(%def-overloaded-cmp p-<=  "<="  "<=>"  <=  nil)   ; NaN<=x → false
(%def-overloaded-cmp p->=  ">="  "<=>"  >=  nil)   ; NaN>=x → false

(defun %p-<=>-slow (a b)
  "Perl spaceship slow path with use overload '<=>' dispatch"
  (%with-binary-overload ("<=>" a b)
                         ;; IEEE 754: NaN comparisons always false → <=> returns undef
                         (let ((na (to-number a)) (nb (to-number b)))
                           (if (or (%pcl-nan-p na) (%pcl-nan-p nb))
                               *p-undef*
                               (cond ((< na nb) -1) ((> na nb) 1) (t 0))))))

(declaim (inline p-<=>))
(defun p-<=> (a b)
  "Perl spaceship operator with numberp fast path"
  (if (and (numberp a) (numberp b))
      (if (or (%pcl-nan-p a) (%pcl-nan-p b))
          *p-undef*
          (cond ((< a b) -1) ((> a b) 1) (t 0)))
      (%p-<=>-slow a b)))
(declaim (notinline p-<=>))

;;; ============================================================
;;; Range Operator
;;; ============================================================

(defun %p-range-numeric-string-p (str)
  "Is STR a numeric-like range operand? (non-zero-padded numeric string;
   surrounding whitespace allowed: Perl numifies \"-4\\n\" as -4)."
  (let ((ts (string-trim '(#\Space #\Tab #\Newline #\Return) str)))
    (and (not (and (> (length ts) 1) (char= (char ts 0) #\0)))
         (ppcre:scan "^[+-]?\\d+(\\.\\d+)?([Ee][+-]?\\d+)?$" ts)
         t)))

(defun %p-range-classify (start end)
  "Shared range-operator classifier for p-.. and p-foreach-range — the ONE
   place the numeric-vs-string(magical) range decision lives.
   Returns (values :numeric NS NE) with integer (truncated) bounds, or
   (values :string SV EV) with string bounds (undef → \"\")."
  (let* ((s (unbox start))
         (e (unbox end))
         ;; Treat *p-undef* as undef (nil) for range logic
         (s-undef (or (null s) (eq s *p-undef*)))
         (e-undef (or (null e) (eq e *p-undef*)))
         (s-num-p (or (numberp s) (and (stringp s) (%p-range-numeric-string-p s))))
         (e-num-p (or (numberp e) (and (stringp e) (%p-range-numeric-string-p e))))
         ;; String range when at least one side is a genuine non-numeric string,
         ;; or both are undef (undef..undef). Excludes undef+numeric (→ numeric).
         (use-string-range
          (and (or s-undef (stringp s))
               (or e-undef (stringp e))
               (or (and (stringp s) (not s-num-p))
                   (and (stringp e) (not e-num-p))
                   (and s-undef e-undef)))))
    (if use-string-range
        (values :string (if s-undef "" s) (if e-undef "" e))
        (let ((ns (to-number s))
              (ne (to-number e)))
          ;; Inf/NaN endpoints: Perl dies "Range iterator outside integer range"
          (when (or (and (floatp ns) (or (%pcl-nan-p ns) (sb-ext:float-infinity-p ns)))
                    (and (floatp ne) (or (%pcl-nan-p ne) (sb-ext:float-infinity-p ne))))
            (p-die (make-p-box "Range iterator outside integer range") nil))
          (let ((ns-i (truncate ns))
                (ne-i (truncate ne)))
            ;; Endpoints outside perl's IV range [-2^63, 2^63-1] die the same
            ;; way — SBCL bignums would otherwise iterate past 2^63 where perl
            ;; refuses (range.t bound-rejected rows).  Once per range, not per
            ;; iteration, so the counting-loop fast path is unaffected.
            (when (or (< ns-i #.(- (expt 2 63))) (> ns-i #.(1- (expt 2 63)))
                      (< ne-i #.(- (expt 2 63))) (> ne-i #.(1- (expt 2 63))))
              (p-die (make-p-box "Range iterator outside integer range") nil))
            (values :numeric ns-i ne-i))))))

(defun p-.. (start end)
  "Perl range operator .. - returns a vector from start to end (inclusive).
   Works with numbers, single characters, and multi-character strings
   (magical string increment: 'aa'..'zz', 'A'..'ZZ', etc.)
   NOTE: `for $v (A..B)` does NOT call this for numeric ranges — the
   p-foreach-range macro counting-loops without materializing the vector."
  (multiple-value-bind (kind a b) (%p-range-classify start end)
    (if (eq kind :string)
        ;; String range: magical vs non-magical starts
        (if (and (> (length a) 0) (ppcre:scan "^[a-zA-Z0-9]+$" a))
            ;; Magical string range (all alphanumeric start)
            (if (> (length a) (length b))
                (make-array 0)
                (let ((result (make-array 0 :adjustable t :fill-pointer 0))
                      (current (copy-seq a))
                      (max-len (length b)))
                  (loop
                   (vector-push-extend current result)
                   (when (string= current b) (return))
                   (setf current (magical-string-increment current))
                   ;; If magical-string-increment returned a number, stop
                   (unless (stringp current) (return))
                   (when (> (length current) max-len) (return)))
                  result))
            ;; Non-magical or empty start: return (a) if a <= b, else empty
            (if (string<= a b)
                (vector a)
                (make-array 0)))
        ;; Numeric range (materialized — only reached outside foreach)
        (progn
          (when (> (- b a) 100000000)
            ;; perl croaks "panic: memory wrap" when the element count's
            ;; byte size wraps size_t, and fails allocation ("Out of
            ;; memory...") otherwise; range.t RT #130841 matches on these
            ;; texts, so the refusal guard speaks them too.
            (if (> (- b a) #.(expt 2 61))
                (error "panic: memory wrap")
                (error "Out of memory during list extend")))
          (if (<= a b)
              (coerce (loop for i from a to b collect i) 'vector)
              (make-array 0))))))

(defun p-... (start end)
  "Perl three-dot range operator ... - same as .. in list context."
  (p-.. start end))

;;; ============================================================
;;; Flip-flop operators (scalar context .. and ...)
;;; Each usage of .. in scalar context gets a unique integer ID.
;;; State is stored in *pcl-flipflop-states* keyed by ID.
;;; State: NIL = off, fixnum N >= 1 = on with counter N.
;;; ============================================================

(defvar *pcl-flipflop-states* (make-hash-table :test 'equal))

(defun %p-flipflop-lineno ()
  "Get current line number ($.) for numeric flip-flop.
   Returns integer, treating undef as 0 (with uninitialized warning)."
  (let ((v (unbox $\.)))
    (if (or (null v) (eq v *p-undef*))
        (progn
          (p-warn "Use of uninitialized value $. in numeric eq (==)")
          0)
        (truncate (to-number v)))))

(defmacro p-flipflop (id left-form right-form)
  "Perl .. flip-flop in scalar context (boolean operands).
   id: compile-time integer literal, unique per .. usage in source.
   left-form/right-form: lazily evaluated Perl expressions."
  (let ((sv (gensym "FF")) (nc (gensym "NC")))
    `(let ((,sv (gethash ,id *pcl-flipflop-states*)))
       (if ,sv
           (let ((,nc (1+ ,sv)))
             (if (p-true-p ,right-form)
                 (progn (remhash ,id *pcl-flipflop-states*)
                        (format nil "~AE0" ,nc))
                 (progn (setf (gethash ,id *pcl-flipflop-states*) ,nc)
                        (format nil "~A" ,nc))))
           (if (p-true-p ,left-form)
               (if (p-true-p ,right-form)
                   "1E0"
                   (progn (setf (gethash ,id *pcl-flipflop-states*) 1) "1"))
               "")))))

(defmacro p-flipflop-3 (id left-form right-form)
  "Perl ... flip-flop in scalar context (boolean operands, no immediate right-check)."
  (let ((sv (gensym "FF")) (nc (gensym "NC")))
    `(let ((,sv (gethash ,id *pcl-flipflop-states*)))
       (if ,sv
           (let ((,nc (1+ ,sv)))
             (if (p-true-p ,right-form)
                 (progn (remhash ,id *pcl-flipflop-states*)
                        (format nil "~AE0" ,nc))
                 (progn (setf (gethash ,id *pcl-flipflop-states*) ,nc)
                        (format nil "~A" ,nc))))
           (if (p-true-p ,left-form)
               (progn (setf (gethash ,id *pcl-flipflop-states*) 1) "1")
               "")))))

(defmacro p-flipflop-num (id left-num right-num)
  "Perl .. numeric flip-flop in scalar context.
   Compares current $. (line number) against integer literal operands."
  (let ((sv (gensym "FF")) (nc (gensym "NC")) (ln (gensym "LN")))
    `(let* ((,sv (gethash ,id *pcl-flipflop-states*))
            (,ln (%p-flipflop-lineno)))
       (if ,sv
           (let ((,nc (1+ ,sv)))
             (if (= ,ln ,right-num)
                 (progn (remhash ,id *pcl-flipflop-states*)
                        (format nil "~AE0" ,nc))
                 (progn (setf (gethash ,id *pcl-flipflop-states*) ,nc)
                        (format nil "~A" ,nc))))
           (if (= ,ln ,left-num)
               (if (= ,ln ,right-num)
                   "1E0"
                   (progn (setf (gethash ,id *pcl-flipflop-states*) 1) "1"))
               "")))))

(defmacro p-flipflop-num-3 (id left-num right-num)
  "Perl ... numeric flip-flop (no immediate right-check on first fire)."
  (let ((sv (gensym "FF")) (nc (gensym "NC")) (ln (gensym "LN")))
    `(let* ((,sv (gethash ,id *pcl-flipflop-states*))
            (,ln (%p-flipflop-lineno)))
       (if ,sv
           (let ((,nc (1+ ,sv)))
             (if (= ,ln ,right-num)
                 (progn (remhash ,id *pcl-flipflop-states*)
                        (format nil "~AE0" ,nc))
                 (progn (setf (gethash ,id *pcl-flipflop-states*) ,nc)
                        (format nil "~A" ,nc))))
           (if (= ,ln ,left-num)
               (progn (setf (gethash ,id *pcl-flipflop-states*) 1) "1")
               "")))))

(defmacro p-flipflop-dyn (id left-form right-form)
  "Flip-flop for non-regex, non-integer operands: compare them numerically with $.
   Generates 'isn't numeric' warnings when operands are non-numeric strings."
  (let ((sv (gensym "FF")) (nc (gensym "NC")) (ln (gensym "LN")))
    `(let* ((,sv (gethash ,id *pcl-flipflop-states*))
            (,ln (%p-flipflop-lineno)))
       (if ,sv
           (let ((,nc (1+ ,sv)))
             (if (p-true-p (p-== ,ln ,right-form))
                 (progn (remhash ,id *pcl-flipflop-states*)
                        (format nil "~AE0" ,nc))
                 (progn (setf (gethash ,id *pcl-flipflop-states*) ,nc)
                        (format nil "~A" ,nc))))
           (if (p-true-p (p-== ,ln ,left-form))
               (if (p-true-p (p-== ,ln ,right-form))
                   "1E0"
                   (progn (setf (gethash ,id *pcl-flipflop-states*) 1) "1"))
               "")))))

(defmacro p-flipflop-dyn-3 (id left-form right-form)
  "Three-dot variant of p-flipflop-dyn (no immediate right-check on first fire)."
  (let ((sv (gensym "FF")) (nc (gensym "NC")) (ln (gensym "LN")))
    `(let* ((,sv (gethash ,id *pcl-flipflop-states*))
            (,ln (%p-flipflop-lineno)))
       (if ,sv
           (let ((,nc (1+ ,sv)))
             (if (p-true-p (p-== ,ln ,right-form))
                 (progn (remhash ,id *pcl-flipflop-states*)
                        (format nil "~AE0" ,nc))
                 (progn (setf (gethash ,id *pcl-flipflop-states*) ,nc)
                        (format nil "~A" ,nc))))
           (if (p-true-p (p-== ,ln ,left-form))
               (progn (setf (gethash ,id *pcl-flipflop-states*) 1) "1")
               "")))))

;;; ============================================================
;;; String Comparison
;;; ============================================================

;;; use overload — helper macro for string comparison operators.
;;; STR-TEST applied to (to-string a) (to-string b) for non-overloaded case.
;;; CMP-TEST applied to (cmp-result) 0 for the cmp-based fallback.
;;; These are distinct because str-test takes strings, cmp-test takes numbers.
;;; Each string comparison compiles to an INLINE wrapper: two raw CL strings
;;; take the native CL test directly; anything else (boxes, numbers, undef,
;;; overloaded objects) goes through the out-of-line slow path.
(defmacro %def-overloaded-str-cmp (name op-str str-test cmp-test)
  (let ((slow (intern (concatenate 'string "%" (symbol-name name) "-SLOW") :pcl)))
    `(progn
       (defun ,slow (a b)
         ,(format nil "Perl ~A slow path: use overload dispatch (returns 1 / \"\")" op-str)
         ;; use overload: check op-specific handler, then fallback to cmp.
         ;; Every branch yields a CL boolean; p-bool maps it to Perl's 1 / "".
         (p-bool
          (let ((ha (p-find-overload a ,op-str)))
            (if ha (p-true-p (p-call-overload ha a b nil))
                (let ((hb (p-find-overload b ,op-str)))
                  (if hb (p-true-p (p-call-overload hb b a t))
                      (let ((fa (p-find-overload a "cmp"))
                            (fb (p-find-overload b "cmp")))
                        (if (or fa fb)
                            ;; use overload fallback: cmp returns -1/0/1, test against 0
                            (,cmp-test (to-number (if fa
                                                      (p-call-overload fa a b nil)
                                                      (p-call-overload fb b a t)))
                                       0)
                            ;; No overload: direct string comparison
                            (,str-test (to-string a) (to-string b)))))))))) ; t/nil → p-bool
       (declaim (inline ,name))
       (defun ,name (a b)
         ,(format nil "Perl ~A with stringp fast path (returns 1 / \"\")" op-str)
         (if (and (stringp a) (stringp b))
             (p-bool (,str-test a b))
             (,slow a b)))
       ;; expansion stored; plain calls inside the runtime (see file top/end)
       (declaim (notinline ,name)))))

(%def-overloaded-str-cmp p-str-eq  "eq"  string=   =)
(%def-overloaded-str-cmp p-str-ne  "ne"  string/=  /=)
(%def-overloaded-str-cmp p-str-lt  "lt"  string<   <)
(%def-overloaded-str-cmp p-str-gt  "gt"  string>   >)
(%def-overloaded-str-cmp p-str-le  "le"  string<=  <=)
(%def-overloaded-str-cmp p-str-ge  "ge"  string>=  >=)

(defun %p-str-cmp-slow (a b)
  "Perl string comparison (cmp) slow path with use overload 'cmp' dispatch"
  (%with-binary-overload ("cmp" a b)
                         (let ((sa (to-string a)) (sb (to-string b)))
                           (cond ((string< sa sb) -1) ((string> sa sb) 1) (t 0)))))

(declaim (inline p-str-cmp))
(defun p-str-cmp (a b)
  "Perl string comparison (cmp) with stringp fast path"
  (if (and (stringp a) (stringp b))
      (cond ((string< a b) -1) ((string> a b) 1) (t 0))
      (%p-str-cmp-slow a b)))
(declaim (notinline p-str-cmp))

;;; ============================================================
;;; Chained Comparison
;;; ============================================================

(defun cmp-op-to-fn (op)
  "Convert comparison operator symbol to p- function symbol.
   Handles both raw symbols and quoted forms.
   e.g., < -> p-<, (quote <) -> p-<, eq -> p-str-eq"
  (let* ((sym (if (and (consp op) (eq (car op) 'quote))
                  (cadr op)  ; extract symbol from (quote sym)
                  op))
         (name (string-downcase (symbol-name sym))))
    ;; String comparison operators map to the p-str-* family, not p-<name>.
    (cond ((string= name "eq")  'p-str-eq)
          ((string= name "ne")  'p-str-ne)
          ((string= name "lt")  'p-str-lt)
          ((string= name "gt")  'p-str-gt)
          ((string= name "le")  'p-str-le)
          ((string= name "ge")  'p-str-ge)
          ((string= name "cmp") 'p-str-cmp)
          (t (intern (format nil "P-~A" sym) :pcl)))))

(defun chain-cmp-expand (prev ops-and-terms)
  "Recursively expand a chained comparison.
   prev: symbol holding the already-evaluated left value.
   ops-and-terms: (op1 t2 op2 t3 ...) — alternating quoted ops and terms."
  (let ((op   (first ops-and-terms))
        (term (second ops-and-terms))
        (rest (cddr ops-and-terms)))
    (if (null rest)
        ;; Base case: final comparison — evaluate both sides
        `(,(cmp-op-to-fn op) ,prev ,term)
        ;; Recursive: bind next term, short-circuit if this cmp fails
        (let ((g (gensym "T")))
          `(let ((,g ,term))
             (if (p-true-p (,(cmp-op-to-fn op) ,prev ,g))
                 ,(chain-cmp-expand g rest)
                 ""))))))

(defmacro p-chain-cmp (first-term &rest ops-and-terms)
  "Chained comparison: a op1 b op2 c [op3 d ...].
   Evaluates each term exactly once with short-circuit semantics:
   if any comparison is false, remaining terms are not evaluated."
  (let ((g1 (gensym "T1"))
        (g2 (gensym "T2")))
    `(let ((,g1 ,first-term)
           (,g2 ,(second ops-and-terms)))
       (if (p-true-p (,(cmp-op-to-fn (first ops-and-terms)) ,g1 ,g2))
           ,(chain-cmp-expand g2 (cddr ops-and-terms))
           ""))))

;;; ============================================================
;;; Logical Operators
;;; ============================================================

(defmacro p-&& (a b)
  "Perl short-circuit AND"
  (let ((tmp (gensym)))
    `(let ((,tmp ,a))
       (if (p-true-p ,tmp) ,b ,tmp))))

(defmacro p-|| (a b)
          "Perl short-circuit OR"
          (let ((tmp (gensym)))
            `(let ((,tmp ,a))
               (if (p-true-p ,tmp) ,tmp ,b))))

(defun p-! (a)
  "Perl logical NOT - returns 1 or empty string like Perl"
  (if (p-true-p a) "" 1))

(defun p-not (a)
  "Perl logical NOT (low precedence) - same return values as p-!"
  (if (p-true-p a) "" 1))

(defmacro p-and (a b)
  "Perl 'and' operator"
  `(p-&& ,a ,b))

(defmacro p-or (a b)
  "Perl 'or' operator"
  `(p-|| ,a ,b))

(defun p-xor (a b)
  "Perl 'xor' operator: returns 1 if exactly one side is true, \"\" if both or neither"
  (let ((ta (p-true-p a))
        (tb (p-true-p b)))
    (if (or (and ta (not tb)) (and (not ta) tb)) 1 "")))

(defmacro p-// (a b)
  "Perl defined-or operator"
  (let ((tmp (gensym)))
    `(let ((,tmp ,a))
       (if (%pcl-definedp ,tmp) ,tmp ,b))))

;;; ============================================================
;;; Bitwise Operators
;;; ============================================================

(defun p-string-bitwise-operand-p (v)
  "Return T if v is a non-numeric string and should trigger string bitwise ops."
  (let ((val (unbox v)))
    (and (stringp val)
         (not (looks-like-number val)))))

(defun p-string-bit-op (a b op truncate-p)
  "Perl string bitwise op. OP is logand/logior/logxor.
   TRUNCATE-P T: result length = min(len(a),len(b)) (for &).
   TRUNCATE-P NIL: result length = max(len(a),len(b)), shorter padded with NUL (for |, ^)."
  (let* ((sa (to-string a))
         (sb (to-string b))
         (la (length sa))
         (lb (length sb))
         (result-len (if truncate-p (min la lb) (max la lb)))
         (result (make-string result-len :initial-element #\Nul)))
    (dotimes (i result-len)
      (let ((ca (if (< i la) (char-code (char sa i)) 0))
            (cb (if (< i lb) (char-code (char sb i)) 0)))
        (setf (char result i) (code-char (funcall op ca cb)))))
    result))

(defun %pcl-to-integer (n)
  "Convert numeric value to integer using Perl IV semantics.
   +Inf -> UV_MAX (0xFFFF...=all-ones, -1 as signed), -Inf -> IV_MIN (-2^63), NaN -> 0."
  ;; Short-circuit for exact integers: avoid float coercion which loses precision
  ;; for values >= 2^53 (e.g. 2^64-1 rounds to 2^64, breaking 64-bit unpack).
  (if (integerp n) n
      (let ((d (coerce n 'double-float)))
        (cond ((%pcl-nan-p d) 0)
              ((sb-ext:float-infinity-p d)
               (if (minusp d) #x-8000000000000000 #xFFFFFFFFFFFFFFFF))
              (t (truncate d))))))

(defun %pcl-to-u64 (n)
  "Coerce a number to its unsigned 64-bit value — Perl's bitwise & | ^ ~ treat
   their integer operands as unsigned 64-bit, so a negative operand wraps."
  (logand (%pcl-to-integer n) #xFFFFFFFFFFFFFFFF))

(defun p-bit-and (a b)
  "Perl bitwise AND — string (char-by-char, truncates) or numeric (unsigned 64-bit)"
  (if (and (p-string-bitwise-operand-p a) (p-string-bitwise-operand-p b))
      (p-string-bit-op a b #'logand t)
      (logand (%pcl-to-u64 (to-number a)) (%pcl-to-u64 (to-number b)))))

(defun p-bit-or (a b)
  "Perl bitwise OR — string (char-by-char, pads with NUL) or numeric (unsigned 64-bit)"
  (if (and (p-string-bitwise-operand-p a) (p-string-bitwise-operand-p b))
      (p-string-bit-op a b #'logior nil)
      (logior (%pcl-to-u64 (to-number a)) (%pcl-to-u64 (to-number b)))))

(defun p-bit-xor (a b)
  "Perl bitwise XOR — string (char-by-char, pads with NUL) or numeric (unsigned 64-bit)"
  (if (and (p-string-bitwise-operand-p a) (p-string-bitwise-operand-p b))
      (p-string-bit-op a b #'logxor nil)
      (logxor (%pcl-to-u64 (to-number a)) (%pcl-to-u64 (to-number b)))))

(defun p-bit-not (a)
  "Perl bitwise NOT - string NOT if non-numeric string, integer NOT otherwise"
  (if (p-string-bitwise-operand-p a)
      (map 'string (lambda (c) (code-char (logxor (char-code c) #xFF))) (to-string a))
      (logand (lognot (%pcl-to-integer (to-number a))) #xFFFFFFFFFFFFFFFF)))

(defun p-str-bit-and (a b)
  "Perl string bitwise AND (&.) — always string, byte-by-byte, truncates to shorter"
  (p-string-bit-op a b #'logand t))

(defun p-str-bit-or (a b)
  "Perl string bitwise OR (|.) — always string, byte-by-byte, pads with NUL"
  (p-string-bit-op a b #'logior nil))

(defun p-str-bit-xor (a b)
  "Perl string bitwise XOR (^.) — always string, byte-by-byte, pads with NUL"
  (p-string-bit-op a b #'logxor nil))

(defun p-str-bit-not (a)
  "Perl string bitwise NOT (~.) — always string, complement each byte"
  (map 'string (lambda (c) (code-char (logxor (char-code c) #xFF))) (to-string a)))

(defmacro p-str-bit-and= (place value)
  `(%p-store-back ,place (p-str-bit-and ,place ,value)))

(defmacro p-str-bit-or= (place value)
  `(%p-store-back ,place (p-str-bit-or ,place ,value)))

(defmacro p-str-bit-xor= (place value)
  `(%p-store-back ,place (p-str-bit-xor ,place ,value)))

;; Raw twins for raw let-bound lexical slots (docs/raw-numeric-verdict.md):
;; same p-str-bit-* computation, plain SETF store.
(defmacro p-str-bit-and=-raw (var value)
  `(setf ,var (p-str-bit-and ,var ,value)))

(defmacro p-str-bit-or=-raw (var value)
  `(setf ,var (p-str-bit-or ,var ,value)))

(defmacro p-str-bit-xor=-raw (var value)
  `(setf ,var (p-str-bit-xor ,var ,value)))

(defun %pcl-uv-coerce (n)
  "Coerce float to integer using UV (unsigned) semantics: +Inf=UV_MAX, -Inf=IV_MIN, NaN=0."
  (if (floatp n)
      (cond ((%pcl-nan-p n) 0)
            ((sb-ext:float-infinity-p n)
             (if (minusp n) #x8000000000000000 #xFFFFFFFFFFFFFFFF))
            (t (truncate n)))
      n))

(defun p-<< (a b)
  "Perl left shift — clamp shift count to prevent SBCL bignum explosion"
  (let ((av (%pcl-uv-coerce (to-number a)))
        (bv (%pcl-to-integer (to-number b))))
    (if (>= (abs bv) 64) 0 (ash av bv))))

(defun p->> (a b)
  "Perl right shift — clamp shift count to prevent SBCL bignum explosion"
  (let ((av (%pcl-uv-coerce (to-number a)))
        (bv (%pcl-to-integer (to-number b))))
    (if (>= (abs bv) 64) 0 (ash av (- bv)))))

(defun p-to-s64 (n)
  "Convert integer to signed 64-bit range (-2^63 to 2^63-1)."
  (let ((masked (logand n #xFFFFFFFFFFFFFFFF)))
    (if (>= masked #x8000000000000000)
        (- masked #x10000000000000000)
        masked)))

(defun p-<<-int (a b)
  "Perl left shift under 'use integer' — signed 64-bit arithmetic."
  (let ((av (truncate (to-number a)))
        (bv (truncate (to-number b))))
    (cond
      ;; Large positive left shift (b >= 64) or large negative right shift: 0
      ((>= bv 64) 0)
      ;; Large negative shift (= right shift), |b| >= 64: arithmetic fill
      ((<= bv -64) (if (minusp av) -1 0))
      ;; Normal range: let ash handle it (negative bv = right shift in CL ash)
      (t (p-to-s64 (ash av bv))))))

(defun p->>-int (a b)
  "Perl right shift under 'use integer' — signed 64-bit arithmetic."
  (let ((av (truncate (to-number a)))
        (bv (truncate (to-number b))))
    (cond
      ;; Large positive right shift (b >= 64): arithmetic fill
      ((>= bv 64) (if (minusp av) -1 0))
      ;; Large negative shift (= left shift), |b| >= 64: result is 0
      ((<= bv -64) 0)
      ;; Normal range: arithmetic right shift, sign-extend to 64-bit
      (t (p-to-s64 (ash av (- bv)))))))

;;; ============================================================
;;; Data Structures - Arrays
;;; ============================================================

(defun p-aref-unbox-elem (elem)
  "Unbox an array element, preserving the box for reference types.
   Scalar types (number, string, undef) are unboxed for efficiency.
   Reference types (array/hash/code ref, scalar ref) keep their box so
   that numeric comparison (== on refs) uses object-address, not array length."
  (if (null elem)
      *p-undef*
      (let ((v (if (p-box-p elem) (p-box-value elem) elem)))
        ;; magic slot (defelem @_ hole alias): read through the getter, like unbox
        (when (p-magic-cell-p v)
          (setf v (funcall (p-magic-cell-getter v))))
        (if (or (and (vectorp v) (not (stringp v)))  ; arrayref
                (hash-table-p v)                      ; hashref
                (functionp v)                          ; coderef
                (p-box-p v)                            ; scalar ref (box-in-box)
                ;; glob REF: unlike every other raw referent, a typeglob's
                ;; ref-ness is the BOX's is-ref flag, so unboxing here would
                ;; hand back a bare glob (#423).  A glob VALUE element has no
                ;; flag and unboxes as before.
                (and (p-typeglob-p v) (p-box-p elem) (p-box-is-ref elem))
                (%p-dualvar-box-p elem))               ; $!/dualvar: keep both halves
            elem   ; reference or dualvar: return the box so both halves survive
            v))))  ; scalar: return unboxed value

(defun %p-hash-unbox-elem (elem)
  "Unbox a hash slot value for reading.
   Keeps the slot-box only for values that box-set would corrupt in scalar context:
   blessed objects, unblessed hash-refs (hash-table), and array-refs (non-string vector).
   Code-refs (raw functions) and scalar-refs (inner p-box) are returned unboxed,
   matching the old (unbox slot) behaviour for those types.
   Also handles raw (non-box) slot values stored directly in hashes (e.g. %+ captures)."
  (if (null elem)
      *p-undef*
      (let ((v (if (p-box-p elem) (p-box-value elem) elem)))
        (if (or (and (p-box-p elem) (p-box-class elem))  ; blessed object
                (hash-table-p v)                          ; hash-ref
                (and (vectorp v) (not (stringp v)))       ; array-ref
                (%p-dualvar-box-p elem))                  ; $!/dualvar: keep both halves
            elem   ; keep box: box-set would convert these to count/length
            v))))

(defun p-aref (arr idx)
  "Perl array access (supports negative indices, works on vectors and lists).
   Returns the VALUE (unboxed for scalars, box preserved for references)."
  (let* ((a (unbox arr)))  ; Unbox if needed
    ;; If array is undef (from failed hash lookup etc), return undef
    (when (eq a *p-undef*)
      (return-from p-aref *p-undef*))
    ;; A bare string here is a SYMBOLIC array reference (@{$str}[i] under
    ;; no-strict-refs), not a char-vector to index — resolve it to the package
    ;; array.  A NUL in the name is inaccessible (Perl gives nothing), so undef.
    (when (stringp a)
      (return-from p-aref
        (if (find #\Nul a) *p-undef* (p-aref (p-ensure-arrayref arr) idx))))
    (let* ((i (truncate (to-number idx)))
           (len (cond ((vectorp a) (length a))
                      ((listp a) (length a))
                      (t 0)))
           (actual-idx (if (< i 0) (+ len i) i)))
      (cond
        ((and (vectorp a) (>= actual-idx 0) (< actual-idx len))
         (p-aref-unbox-elem (aref a actual-idx)))
        ((and (listp a) (>= actual-idx 0) (< actual-idx len))
         (p-aref-unbox-elem (nth actual-idx a)))
        ;; Out of range on a real array (or on nil, which is a list) is undef;
        ;; anything else is a wrong-kind referent ($hashref->[0]) and is perl's
        ;; fatal, not a silent undef.  Both tests sit AFTER the two fast paths,
        ;; so an in-range read costs exactly what it did before.
        ((%p-wrong-referent-p "ARRAY" a) (%p-not-a-ref "ARRAY"))
        (t *p-undef*)))))

;;; Make index I valid in adjustable vector A: push nil HOLES (the deleted-
;;; element marker — `exists` stays false, a read gives undef, the first write
;;; boxes) up to and including I.  A no-op when I is in range; past the end of
;;; a read-only array it dies as perl does (task #159).  Inline: this is the
;;; element-write path.  (#387 family 37, s413 — the loop six writers spelled.)
(declaim (inline %p-extend-to))
(defun %p-extend-to (a i)
  (when (>= i (length a))
    (%p-check-array-writable a)
    (loop repeat (- (1+ i) (length a))
          do (vector-push-extend nil a)))
  a)

(defun (setf p-aref) (value arr idx)
  "Setf expander for p-aref - allows assignment to array elements.
   Auto-extends array if index is beyond current length (Perl semantics).
   Stores values in boxes for l-value semantics. Returns the box."
  (let* ((a (unbox arr)))  ; unbox array refs ($arr[i][j] write-through)
    ;; A bare string is a SYMBOLIC array reference (@{$str}[i] = ... under
    ;; no-strict-refs), not a char-vector to overwrite — resolve to the package
    ;; array and store there.  A NUL-containing name is inaccessible in PCL
    ;; (p-ensure-arrayref returns a throwaway), so the write is lost like Perl's
    ;; unreachable stash slot rather than faulting on a CHARACTER type-error.
    (when (stringp a)
      (return-from p-aref
        (setf (p-aref (p-ensure-arrayref arr) idx) value)))
    a)
  (let* ((a (unbox arr))
         (i (truncate (to-number idx)))
         (len (if (vectorp a) (length a) 0))
         (actual-idx (if (< i 0) (+ len i) i)))
    (if (and (vectorp a) (>= actual-idx 0))
        (progn
          ;; Auto-extend array if needed (Perl autovivification): holes, and
          ;; never past the end of a read-only array — perl allows `$ro[0] = 9`
          ;; (in bounds) and dies on `$ro[5] = 9` (task #159).
          (%p-extend-to a actual-idx)
          ;; Get or create box at this index
          (let ((box (aref a actual-idx)))
            (unless (p-box-p box)
              (setf box (make-p-box nil))
              (setf (aref a actual-idx) box))
            ;; Set the box's value and return the box
            (box-set box value)))
        ;; Not a writable array.  A wrong-kind referent ($hashref->[0] = …) is
        ;; perl's fatal — the write used to be silently dropped.  Anything else
        ;; (nil/undef container, negative index past the front) keeps the old
        ;; no-op.  Tested only when the fast path above already failed.
        (when (%p-wrong-referent-p "ARRAY" a) (%p-not-a-ref "ARRAY")))))

(defun p-aref-box (arr idx)
  "Get the BOX at array index (for l-value operations like chop, ++).
   Creates box if needed, auto-extends array. Returns the box itself."
  (let* ((a (unbox arr)))
    ;; If array is undef, can't get box from it
    (when (eq a *p-undef*)
      (return-from p-aref-box (make-p-box *p-undef*)))
    (let* ((i (truncate (to-number idx)))
           (len (if (vectorp a) (length a) 0))
           (actual-idx (if (< i 0) (+ len i) i)))
      (when (and (vectorp a) (>= actual-idx 0))
        ;; Auto-extend array if needed (intermediate slots are nil = non-existent)
        (%p-extend-to a actual-idx)
        ;; Ensure box exists at this index
        (let ((elem (aref a actual-idx)))
          (unless (p-box-p elem)
            (setf elem (make-p-box elem))
            (setf (aref a actual-idx) elem))
          (return-from p-aref-box elem)))
      ;; Out of bounds or not a vector
      (make-p-box *p-undef*))))

(declaim (ftype function p-aslice))
(defun p-aref-deref (ref idx)
  "Perl array ref access $ref->[idx] - unbox the reference first.
   When idx is a vector (range result), returns a slice instead of a single element.
   When ref is a string, treat as symbolic reference to @name."
  (let ((arr (unbox ref)))
    ;; Unwrap the (vector RESULT_VECTOR) codegen pattern for (LIST_EXPR)[idx].
    ;; gen_progn in LIST_CTX wraps a single list-returning expression in (vector ...),
    ;; creating a simple 1-element vector containing the function result (also a vector).
    ;; Peel that wrapper so p-aslice sees the actual list, not a 1-element wrapper.
    ;; Safe: boxed array refs are p-boxes (not raw vectors), strings are excluded.
    (when (and (vectorp arr)
               (not (array-has-fill-pointer-p arr))
               (= (length arr) 1)
               (let ((inner (aref arr 0)))
                 (and (vectorp inner)
                      (not (stringp inner)))))
      (setf arr (aref arr 0)))
    ;; Flatten Perl @array variables embedded in a literal list-slice vector.
    ;; (vector @foo @bar)[0..5] generates (p-aref-deref (vector @foo @bar) ...)
    ;; where each @arr is an adjustable fill-pointer vector. Flatten them so
    ;; slicing sees the elements, not the sub-arrays.
    (when (and (vectorp arr)
               (not (array-has-fill-pointer-p arr))
               (some (lambda (e)
                       (and (vectorp e)
                            (not (stringp e))
                            (array-has-fill-pointer-p e)))
                     arr))
      (let ((flat (make-array 0 :adjustable t :fill-pointer 0)))
        (loop for e across arr do
              (if (and (vectorp e) (not (stringp e)) (array-has-fill-pointer-p e))
                  (loop for item across e do (vector-push-extend item flat))
                  (vector-push-extend e flat)))
        (setf arr flat)))
    (cond
      ;; Symbolic reference: string used as array name (no strict refs)
      ((stringp arr)
       (when (find #\Nul arr) (return-from p-aref-deref *p-undef*))
       (let ((sym-arr (p-ensure-arrayref ref)))
         (if (and (vectorp idx) (not (stringp idx)))
             (p-aslice sym-arr idx)
             (p-aref sym-arr idx))))
      ;; Function as single-element list: (sub{...})[0] = the sub itself
      ((functionp arr)
       (let ((i (truncate (to-number idx))))
         (if (eql i 0)
             (make-p-box arr)
             *p-undef*)))
      ((and (vectorp idx) (not (stringp idx)))
       (p-aslice arr idx))
      ;; $scalarref->[0] on the READ path: perl's fatal, and the same arm the
      ;; write path gets through p-ensure-arrayref (#163 referent rule).
      ;; p-box-p guard as in p-gethash-deref: an ordinary `$aref->[0]` has a
      ;; raw vector here and never runs the walk.
      ((and (p-box-p arr) (%p-scalar-referent-p ref)) (%p-not-a-ref "ARRAY"))
      (t (p-aref arr idx)))))

(defun p-array-last-index (arr)
  "Perl $#arr - last index. Accepts raw vectors (@arr) or boxed array refs ($aref).
   Handles both single-boxed (old autovivified) and double-boxed (p-backslash) refs."
  (let* ((v (unbox arr))
         (v (if (p-box-p v) (unbox v) v)))
    (if (vectorp v)
        (1- (length v))
        -1)))

(defun p-set-array-length (arr new-last-index)
  "Set array length by setting $#array. Perl semantics:
   - Growing: extends with HOLES (nil slots — `$#a++` does not vivify:
     `exists $a[$i]` is false for the new positions; a nil slot reads as
     undef and boxes on first write, the runtime's standard hole state)
   - Shrinking: truncates (adjusts fill-pointer)
   - If arr is a scalar box containing undef, auto-vivifies an array ref inside it.
   Returns new-last-index."
  (let* ((inner (unbox arr))
         ;; Auto-vivify: if arr is a box with nil/undef, create an array inside it
         (a (cond
              ((and (p-box-p arr) (or (null inner) (eq inner *p-undef*)))
               (let ((new-arr (make-array 0 :adjustable t :fill-pointer 0)))
                 (box-set arr (make-p-box new-arr))
                 new-arr))
              ;; Already an array ref: unwrap one more level
              ((p-box-p inner)
               (let ((v (unbox inner)))
                 (if (and v (vectorp v) (not (stringp v))) v inner)))
              ((and inner (vectorp inner) (not (stringp inner))) inner)
              (t arr)))
         (nli (truncate (to-number new-last-index)))
         (new-len (1+ nli))
         (cur-len (length a)))
    (cond
      ((> new-len cur-len)
       ;; Grow: extend with holes (nil), NOT boxes — see docstring.
       ;; `$#ro = 5` dies in perl (it extends a fixed-size AV) — task #159.
       (%p-extend-to a nli))
      ((< new-len cur-len)
       ;; Shrink: adjust fill-pointer (minimum 0).
       ;; Perl does NOT guard the shrinking case on a read-only array — `$#ro = 0`
       ;; truncates it.  PCL's read-only storage is a simple vector, which cannot
       ;; be truncated in place and whose variable cell is not reachable from
       ;; here, so the truncation is announced and skipped rather than faked.
       (if (%p-array-readonly-p a)
           (%p-announce-unsupported "$#a =" "shrinking a read-only array"
                                    "perl truncates; PCL cannot shrink fixed storage")
           (setf (fill-pointer a) (max 0 new-len)))))
    nli))

(defmacro p-push (arr &rest items)
  "Perl push - adds to end of array, auto-declares if needed"
  (if (symbolp arr)
      ;; Simple array variable: ensure declared
      `(progn
         (unless (boundp ',arr)
           (%p-ensure-storage (quote ,arr))
           (setf (symbol-value ',arr) (make-array 0 :adjustable t :fill-pointer 0)))
         (p-push-impl ,arr ,@items))
      ;; Complex place
      `(p-push-impl ,arr ,@items)))

(defun %p-defelem-box (vec i)
  "A deferred-element alias box for an array HOLE slot (perl's defelem magic).
Aliasing a hole (foreach loop var, @_ slot) reads undef but leaves the hole
in place (matching perl: `for (@a) {}` does not vivify); the first WRITE
through the alias vivifies — the box de-magics itself, stores itself into
(aref VEC I), and re-dispatches the assignment through box-set so every
normal store rule applies.  Slots therefore never hold a still-magic
defelem, only flattened views (@_, the foreach binding) do — p-exists-array
checks %p-defelem-p for exactly that state.  If the array shrank under the
loop and I is out of bounds (or was independently vivified), the write
lands in the (now detached) box only."
  (let ((box (make-p-box nil)))
    (setf (p-box-value box)
          (make-p-magic-cell
           :kind :defelem
           :getter (lambda () *p-undef*)
           :setter (lambda (new)
                     (setf (p-box-value box) nil
                           (p-box-nv-ok box) nil
                           (p-box-sv-ok box) nil)
                     (cond
                       ;; In bounds: vivify the hole in place.  LENGTH, not
                       ;; FILL-POINTER — a read-only array (task #159) has no
                       ;; fill pointer, and perl allows the ELEMENT write.
                       ((< i (length vec))
                        (when (null (aref vec i))
                          (setf (aref vec i) box)))
                       ((and (adjustable-array-p vec)
                             (array-has-fill-pointer-p vec))
                        ;; Past-the-end index (p-aref-argbox on w($a[10])):
                        ;; perl's defelem store av_fetches with lval=TRUE,
                        ;; extending the array — mirror that here.
                        (loop while (< (fill-pointer vec) i)
                              do (vector-push-extend nil vec))
                        (vector-push-extend box vec)))
                     (box-set box new))))
    box))

(defun %p-defelem-p (slot)
  "True when SLOT is an UNVIVIFIED deferred-element box (%p-defelem-box) —
the state that still counts as an array hole for exists()."
  (and (p-box-p slot)
       (let ((v (p-box-value slot)))
         (and (p-magic-cell-p v)
              (eq (p-magic-cell-kind v) :defelem)))))

(defun p-aref-argbox (arr idx)
  "Array element in USER-SUB ARGUMENT position (perl's @_ aliasing): the
LIVE slot box when the element exists — a write to $_[N] in the callee
reaches the caller's array — and a lazy defelem alias (%p-defelem-box)
when it does not.  Read-only use must never vivify or extend, which is
why p-aref-box (the eager lvalue accessor) is wrong here."
  (let ((a (unbox arr)))
    (if (not (and (vectorp a) (not (stringp a))))
        (make-p-box *p-undef*)
        (let* ((i (truncate (to-number idx)))
               (len (length a))
               (actual-idx (if (< i 0) (+ len i) i)))
          (cond
            ((< actual-idx 0) (make-p-box *p-undef*))
            ((and (< actual-idx len) (aref a actual-idx))
             (let ((elem (aref a actual-idx)))
               (if (p-box-p elem)
                   elem
                   (setf (aref a actual-idx) (make-p-box elem)))))
            (t (%p-defelem-box a actual-idx)))))))

(defun p-gethash-argbox (hash key)
  "Hash element in USER-SUB ARGUMENT position (perl's @_ aliasing): the
LIVE slot box when the key exists, else a lazy defelem alias — reads look
the key up live and stay undef/non-exists, the first write through the
alias creates the key.  p-gethash-box (the eager lvalue accessor) would
create the key on a read-only call, which perl does not."
  (let ((h (unbox hash))
        (k (to-string key)))
    (if (or (not (hash-table-p h)) (gethash :__class__ h))
        ;; undef container / special markers / symbolic-ref strings / blessed:
        ;; keep plain value semantics, no aliasing.
        (make-p-box (p-gethash hash key))
        (multiple-value-bind (existing found) (gethash k h)
          (if found
              (if (p-box-p existing)
                  existing
                  (setf (gethash k h) (make-p-box existing)))
              (let ((box (make-p-box nil)))
                (setf (p-box-value box)
                      (make-p-magic-cell
                       :kind :defelem
                       :getter (lambda ()
                                 ;; live delegation: the key may have been
                                 ;; created independently since the alias
                                 (multiple-value-bind (v f) (gethash k h)
                                   (if f (unbox v) *p-undef*)))
                       :setter (lambda (new)
                                 (setf (p-box-value box) nil
                                       (p-box-nv-ok box) nil
                                       (p-box-sv-ok box) nil)
                                 (multiple-value-bind (v f) (gethash k h)
                                   (if (and f (p-box-p v))
                                       ;; created independently since: write the
                                       ;; live slot; this alias detaches (the
                                       ;; array sibling behaves the same way)
                                       (box-set v new)
                                       (setf (gethash k h) box)))
                                 (box-set box new))))
                box))))))

(defun p-flatten-args (args)
  "Build @_ from %_args, spreading raw (non-string, non-boxed) vectors and hash-tables.
   This implements Perl's argument flattening: foo(@arr) and foo(%hash) spread their
   elements as individual arguments.  HOLE slots (nil) spread as deferred-element
   boxes tied to the SOURCE array (%p-defelem-box): they read undef and stay
   non-exists, but a write through them (foreach alias, $_[N]) vivifies the
   source slot, like perl."
  (let ((result (make-array (length args) :adjustable t :fill-pointer 0)))
    (dolist (arg args)
      (cond
        ((and (vectorp arg) (not (stringp arg)))
         ;; Raw vector = array passed in list context: spread its elements
         (loop for j from 0
               for elem across arg
               do (vector-push-extend
                   (if (null elem) (%p-defelem-box arg j) elem)
                   result)))
        ((and (hash-table-p arg) (not (gethash :__class__ arg)))
         ;; Hash in argument context: spread to alternating key-value pairs.
         ;; But NOT blessed objects (which have :__class__) — those stay as-is.
         (maphash (lambda (k v)
                    (vector-push-extend (make-p-box k) result)
                    (vector-push-extend (if (p-box-p v) v (make-p-box v)) result))
                  arg))
        (t
         ;; Scalar (p-box, string, number, etc.): keep as-is
         (vector-push-extend arg result))))
    result))

(defun p-check-arity (funcname got min max flexible &optional hash-start)
  "Perl subroutine-signature arity check.  Throws a Perl-formatted
   'Too few/many arguments for subroutine ...' error when GOT is outside
   [MIN, MAX].  MAX = nil means no upper bound (a slurpy @/% param).  FLEXIBLE
   non-nil selects the 'at least'/'at most' wording Perl uses when the sub has
   optional or slurpy params (a fixed-arity sub uses the bare count).
   HASH-START non-nil marks a slurpy %hash consuming args from that index on:
   an ODD number of leftover args dies, as in perl."
  (cond
    ((< got min)
     (error "Too few arguments for subroutine '~A' (got ~D; expected ~A~D)"
            funcname got (if flexible "at least " "") min))
    ((and max (> got max))
     (error "Too many arguments for subroutine '~A' (got ~D; expected ~A~D)"
            funcname got (if flexible "at most " "") max))
    ((and hash-start (> got hash-start) (oddp (- got hash-start)))
     (error "Odd name/value argument for subroutine '~A'" funcname))))

(defun p-sig-rest-array (args start)
  "Slurpy @rest signature parameter: a fresh adjustable Perl array holding the
   flattened ARGS from index START onward."
  (let ((out (make-array 0 :adjustable t :fill-pointer 0)))
    (when (and (vectorp args) (< start (length args)))
      (loop for i from start below (length args)
            do (vector-push-extend (aref args i) out)))
    out))

(defun p-sig-rest-hash (args start)
  "Slurpy %rest signature parameter: a hash built from the flattened ARGS
   key/value pairs from index START onward."
  (let ((h (make-hash-table :test 'equal)))
    (when (vectorp args)
      (loop for i from start below (length args) by 2
            do (setf (gethash (to-string (aref args i)) h)
                     (if (< (1+ i) (length args)) (aref args (1+ i)) *p-undef*))))
    h))

;; Marker struct for flattened arrays in push/unshift
(defstruct p-flatten-marker
  "Marker indicating an array should be flattened when pushed/unshifted"
  array)

(defun p-flatten (arr)
  "Mark an array for flattening in push/unshift.
   Called at code-gen time for @array arguments."
  (make-p-flatten-marker :array (unbox arr)))

(defun %p-push-stores-anything-p (items)
  "True when pushing ITEMS would actually store at least one element.
   `push @a, ()` and `push @a, @empty` store nothing — and perl lets those
   through even on a read-only array (push.t asserts it), while ANY real
   element dies.  Mirrors p-push-impl's own spreading rules."
  (dolist (item items nil)
    (let ((val (unbox item)))
      (cond
        ((p-flatten-marker-p val)
         (let ((src (p-flatten-marker-array val)))
           (when (and (vectorp src) (> (length src) 0)) (return t))))
        ((and (vectorp val) (not (stringp val)) (not (p-box-p item)))
         (when (> (length val) 0) (return t)))
        ((and (hash-table-p val) (not (p-box-p item)) (not (gethash :__class__ val)))
         (when (> (hash-table-count val) 0) (return t)))
        (t (return t))))))

(defun %p-push-cold (arr items)
  "The cold half of push: ARR is not a plain growable array.  Either it is not
   an array at all — perl dies \"...must be array\" for a literal and
   \"Experimental push on scalar is now forbidden\" for the removed autoderef
   on a scalar/ref; without this the raw CL type error from
   %p-array-store-scalar leaked a Lisp struct dump into $@ — or it is a
   READ-ONLY array (task #159), where perl dies unless the push would store
   nothing.  Returns the unchanged length in that one legal no-op case."
  (unless (and (vectorp arr) (not (stringp arr)))
    (if (p-box-p arr)
        (error "Experimental push on scalar is now forbidden")
        (error "Type of arg 1 to push must be array (not constant item)")))
  (when (%p-push-stores-anything-p items)
    (%p-readonly-modification))
  (length arr))

(defun p-push-impl (arr &rest items)
  "Implementation of push - stores values in boxes for l-value semantics.
   Recognizes p-flatten-marker to flatten @array arguments.
   Also spreads raw CL vectors (e.g. from qw!...! or list-context expressions)."
  ;; ONE type test covers both cold cases — a target that is not a real array
  ;; (a literal, a scalar/ref), and a READ-ONLY array (task #159), whose storage
  ;; has no fill pointer.  %p-push-cold decides which and what perl says;
  ;; everything real falls straight through.
  (unless (and (vectorp arr) (not (stringp arr)) (array-has-fill-pointer-p arr))
    (return-from p-push-impl (%p-push-cold arr items)))
  (dolist (item items)
    (let ((val (unbox item)))
      (cond
        ;; Flatten marker - push each element of the marked array
        ((p-flatten-marker-p val)
         (let ((src (p-flatten-marker-array val)))
           (when (vectorp src)
             (loop for elem across src do
                   (%p-array-store-scalar arr elem)))))
        ;; Raw CL vector (not a p-box reference): spread elements.
        ;; Handles qw!...! lists and array-valued expressions in list context.
        ((and (vectorp val) (not (stringp val)) (not (p-box-p item)))
         (loop for elem across val do
               (%p-array-store-scalar arr elem)))
        ;; Raw hash-table (not a ref): spread to key/value pairs (%hash in list ctx).
        ((and (hash-table-p val) (not (p-box-p item)) (not (gethash :__class__ val)))
         (dolist (kv (%p-hash-keyval-list val))
           (%p-array-store-scalar arr kv)))
        ;; Regular value - preserve bless class via %p-array-store-scalar
        (t (%p-array-store-scalar arr item)))))
  (length arr))

(defun p-pop (arr)
  "Perl pop - removes from end, returns the element as-is (preserving references)."
  (%p-check-array-writable arr)              ; task #159
  (if (and (vectorp arr) (> (length arr) 0))
      (vector-pop arr)
      *p-undef*))

(defun p-shift (arr)
  "Perl shift - removes from front, returns the element as-is (preserving references).
   Like p-aref, does NOT unbox: box-set handles plain vs reference boxes correctly."
  (%p-check-array-writable arr)              ; task #159
  (cond
    ((and (vectorp arr) (> (length arr) 0))
     (let ((first (aref arr 0)))
       ;; Shift elements down
       (loop for i from 0 below (1- (length arr))
             do (setf (aref arr i) (aref arr (1+ i))))
       (vector-pop arr)
       first))
    ((consp arr)
     (car arr))
    (t *p-undef*)))

(defun p-unshift (arr &rest items)
  "Perl unshift - adds to front. Stores values in boxes for l-value semantics.
   Recognizes p-flatten-marker to flatten @array arguments."
  ;; A read-only array dies here even for an EMPTY list — perl is asymmetric
  ;; with push, and unshift.t t19 tests exactly that (task #159).
  (%p-check-array-writable arr)
  ;; Expand into a flat array of properly-boxed elements (preserving bless class)
  (let ((flat-arr (make-array 8 :adjustable t :fill-pointer 0)))
    (dolist (item items)
      (let ((val (unbox item)))
        (cond
          ;; Flatten marker - expand its array
          ((p-flatten-marker-p val)
           (loop for elem across (p-flatten-marker-array val)
                 do (%p-array-store-scalar flat-arr elem)))
          ;; Raw CL vector (e.g. qw!...!): spread elements
          ((and (vectorp val) (not (stringp val)) (not (p-box-p item)))
           (loop for elem across val do (%p-array-store-scalar flat-arr elem)))
          ;; Regular value - preserve bless class
          (t (%p-array-store-scalar flat-arr item)))))
    (let ((nitems (length flat-arr)))
      ;; Make room with placeholder boxes
      (dotimes (i nitems)
        (vector-push-extend (make-p-box *p-undef*) arr))
      ;; Shift existing elements up
      (loop for i from (1- (length arr)) downto nitems
            do (setf (aref arr i) (aref arr (- i nitems))))
      ;; Insert new items at front (already properly boxed)
      (loop for i from 0 below nitems
            do (setf (aref arr i) (aref flat-arr i)))
      (length arr))))

(defmacro p-splice (arr &rest args)
  "Perl splice - auto-declares array if unbound (handles @Foo::ISA etc.)"
  (if (symbolp arr)
      `(progn
         (unless (boundp ',arr)
           (%p-ensure-storage (quote ,arr))
           (setf (symbol-value ',arr) (make-array 0 :adjustable t :fill-pointer 0)))
         (p-splice-impl ,arr ,@args))
      `(p-splice-impl ,arr ,@args)))

(defun p-splice-impl (arr &optional (offset 0) (length nil length-p) &rest replacements)
  "Perl splice: remove and/or replace elements in an array.
   Returns removed elements as a vector."
  ;; A read-only array dies on ANY splice, even one that would change nothing
  ;; (`splice @ro, 1, 0, ()` — splice.t's RT#131000 row).  Task #159.
  (%p-check-array-writable (unbox arr))
  (let* ((a (unbox arr))
         (alen (length a))
         (offset (truncate (to-number offset)))
         ;; Handle negative offset
         (off (if (< offset 0) (max 0 (+ alen offset)) (min offset alen)))
         ;; Default length = remove everything from offset
         ;; Negative length means "leave that many elements at the end"
         (len (if length-p
                  (let ((l (truncate (to-number length))))
                    (if (< l 0)
                        (max 0 (+ (- alen off) l))
                        (min l (- alen off))))
                  (- alen off)))
         ;; Collect removed elements (preserving boxes so bless class is not lost)
         (removed (make-array len :adjustable t :fill-pointer len)))
    ;; Copy removed elements (keep boxes as-is for reference/bless preservation)
    (loop for i from 0 below len
          do (setf (aref removed i) (aref a (+ off i))))
    ;; Flatten replacement items (arrays get flattened in Perl)
    (let ((flat-rep (make-array 8 :adjustable t :fill-pointer 0)))
      (dolist (r replacements)
        (let ((v (unbox r)))
          ;; Only spread raw (unboxed) vectors; a p-box holding a vector is an array-ref scalar
          (if (and (vectorp v) (not (stringp v)) (not (p-box-p r)))
              (loop for el across v do (%p-array-store-scalar flat-rep el))
              (%p-array-store-scalar flat-rep r))))
      (let* ((nrep (length flat-rep))
             (new-len (+ off nrep (- alen off len)))
             (old-len alen))
        ;; Resize array
        (if (> new-len old-len)
            ;; Growing: extend first, then shift right
            (progn
              (loop repeat (- new-len old-len)
                    do (vector-push-extend (make-p-box nil) a))
              ;; Shift tail elements right
              (loop for i from (1- new-len) downto (+ off nrep)
                    do (setf (aref a i) (aref a (- i (- nrep len))))))
            ;; Shrinking or same: shift left, then shrink
            (progn
              (loop for i from (+ off nrep) below new-len
                    do (setf (aref a i) (aref a (+ i (- len nrep)))))
              (setf (fill-pointer a) new-len)))
        ;; Insert replacements (already properly boxed)
        (loop for i from off
              for j from 0 below nrep
              do (setf (aref a i) (aref flat-rep j)))))
    (if (eq *wantarray* t)
        removed
        (if (> (length removed) 0)
            (aref removed (1- (length removed)))
            *p-undef*))))

;;; ============================================================
;;; Data Structures - Hashes
;;; ============================================================

(defun p-gethash (hash key)
  "Perl hash access. Special handling for %ENV and %INC.
   Returns the VALUE (unboxed if element is a box).
   When hash unboxes to a string, treats as symbolic reference to %name."
  (let* ((h (unbox hash))
         (k (to-string key)))
    ;; If hash is undef (from failed lookup), return undef
    (when (eq h *p-undef*)
      (return-from p-gethash *p-undef*))
    ;; Check for special markers
    (cond
      ((eq h '%ENV-MARKER%)
       (or (sb-posix:getenv k) *p-undef*))
      ((eq h '%INC-MARKER%)
       (multiple-value-bind (val found) (gethash k *p-inc-table*)
         (if found val *p-undef*)))
      ;; Symbolic reference: string used as hash name
      ((stringp h)
       (when (find #\Nul h) (return-from p-gethash *p-undef*))
       (let ((sym-h (p-ensure-hashref hash)))
         (multiple-value-bind (val found) (gethash k sym-h)
           (if found
               (%p-hash-unbox-elem val)
               *p-undef*))))
      ((hash-table-p h)
       (multiple-value-bind (val found) (gethash k h)
         (if (not found)
             *p-undef*
             (%p-hash-unbox-elem val))))
      ;; Wrong kind of referent ($aryref->{k}): perl's fatal, not SBCL's.
      ;; This arm replaces a bare (t …) that handed a non-hash to GETHASH —
      ;; the hash-table-p test above is the SAME test GETHASH did internally,
      ;; so the fast path costs nothing extra.
      ((%p-wrong-referent-p "HASH" h) (%p-not-a-ref "HASH"))
      ;; Any OTHER non-hash keeps GETHASH's own error — a remaining p-box is a
      ;; representation layer, not a wrong referent (see %p-wrong-referent-p).
      (t (multiple-value-bind (val found) (gethash k h)
           (if (not found) *p-undef* (%p-hash-unbox-elem val)))))))

(defun (setf p-gethash) (value hash key)
  "Setf expander for p-gethash - allows assignment to hash elements.
   Special handling for %ENV and %INC.
   Stores values in boxes for l-value semantics. Returns the box."
  (let* ((h (unbox hash))
         (k (to-string key)))
    (cond
      ((eq h '%ENV-MARKER%)
       (sb-posix:setenv k (to-string value) 1)
       value)
      ((eq h '%INC-MARKER%)
       (setf (gethash k *p-inc-table*) value))
      ;; Symbolic reference: string used as hash name
      ((stringp h)
       (when (find #\Nul h) (return-from p-gethash value))  ; null byte: silent no-op
       (let ((sym-h (p-ensure-hashref hash))
             (box (make-p-box nil)))
         (setf (gethash k sym-h) box)
         (box-set box value)))
      ((hash-table-p h)
       ;; Get or create box at this key
       (multiple-value-bind (existing found) (gethash k h)
         (let ((box (if (and found (p-box-p existing))
                        existing
                        (make-p-box nil))))
           (unless (and found (p-box-p existing))
             (setf (gethash k h) box))
           ;; Set the box's value and return the box
           (box-set box value))))
      ((%p-wrong-referent-p "HASH" h) (%p-not-a-ref "HASH"))
      ;; Other non-hash values (a p-box representation layer) keep the previous
      ;; path — see the note in p-gethash.
      (t (multiple-value-bind (existing found) (gethash k h)
           (let ((box (if (and found (p-box-p existing))
                          existing
                          (make-p-box nil))))
             (unless (and found (p-box-p existing))
               (setf (gethash k h) box))
             (box-set box value)))))))

(defun p-gethash-box (hash key)
  "Get the BOX at hash key (for l-value operations like chop, ++).
   Creates box if needed (autovivification). Returns the box itself."
  (let* ((h (unbox hash))
         (k (to-string key)))
    ;; If hash is undef, can't get box from it
    (when (eq h *p-undef*)
      (return-from p-gethash-box (make-p-box *p-undef*)))
    ;; Special markers don't support boxing
    (when (or (eq h '%ENV-MARKER%) (eq h '%INC-MARKER%))
      (return-from p-gethash-box (make-p-box *p-undef*)))
    ;; Get or create box at this key
    (multiple-value-bind (existing found) (gethash k h)
      (if (and found (p-box-p existing))
          existing
          (let ((box (make-p-box (if found existing *p-undef*))))
            (setf (gethash k h) box)
            box)))))

(defun p-ensure-hashref (ref)
  "Ensure ref (a p-box) contains a hash table.
   If ref contains nil or undef, autovivify: create a hash table and store it in the box.
   Returns the raw hash table (not boxed). Used by autovivification macros."
  (let ((h (unbox ref)))
    (cond
      ((or (null h) (eq h *p-undef*))
       (let ((new-hash (make-hash-table :test 'equal)))
         ;; Wrap in make-p-box so box-set does not treat it as scalar-context %hash.
         (box-set ref (make-p-box new-hash))
         new-hash))
      ;; Symbolic reference: string used as hash name (no strict refs).
      ;; ONE resolver — p-cast-% (%{"name"}) already decides between a package
      ;; hash and a STASH ("Pkg::" → p-stash).  This used to be a second copy
      ;; that knew nothing about stashes, so `$$p{k}` with $p="Foo::" read a
      ;; package hash named "" instead of Foo's symbol table.
      ((stringp h) (p-cast-% h))
      ((hash-table-p h) h)
      ;; Wrong kind of referent ($aryref->{k} = …): perl's fatal.
      ((%p-wrong-referent-p "HASH" h) (%p-not-a-ref "HASH"))
      ;; A ref to a plain SCALAR used as a hash ($scalarref->{k}): also perl's
      ;; fatal, and the shape #154 had to leave alone — it looks exactly like
      ;; the representation layer below until the referent rule separates them
      ;; (#163).  Without this it reached SBCL's GETHASH as a P-BOX.
      ((%p-scalar-referent-p ref) (%p-not-a-ref "HASH"))
      ;; Anything else (a p-box representation layer) is returned for the
      ;; caller to unbox, exactly as before.
      (t h))))

(defun p-ensure-arrayref (ref)
  "Ensure ref (a p-box) contains an adjustable vector.
   If ref contains nil or undef, autovivify: create a vector and store it in the box.
   Returns the raw vector (not boxed). Used by autovivification macros."
  (let ((a (unbox ref)))
    (cond
      ((or (null a) (eq a *p-undef*))
       (let ((new-arr (make-array 0 :adjustable t :fill-pointer 0)))
         ;; Wrap in make-p-box so box-set does not treat it as scalar-context @arr.
         (box-set ref (make-p-box new-arr))
         new-arr))
      ;; Symbolic reference: string used as array name (no strict refs).
      ;; ONE resolver — %p-symref-array, the same one @{"name"} goes through
      ;; (p-cast-@); this was a second copy of it.
      ((stringp a) (%p-symref-array a))
      ((vectorp a) a)
      ;; Wrong kind of referent ($hashref->[0] = …): perl's fatal.
      ((%p-wrong-referent-p "ARRAY" a) (%p-not-a-ref "ARRAY"))
      ;; A ref to a plain SCALAR used as an array ($$scalarref[0], @$scalarref).
      ;; See p-ensure-hashref's arm — the referent rule (#163) is what makes
      ;; this separable from the representation layer below.
      ((%p-scalar-referent-p ref) (%p-not-a-ref "ARRAY"))
      (t a))))

(defun p-autoviv-gethash (hash key)
  "Get hash value, autovivifying to empty hash if missing or :UNDEF.
   Handles boxes in hash values."
  (let* ((h (unbox hash))
         (k (to-string key)))
    (multiple-value-bind (stored found) (gethash k h)
      ;; Unbox if stored value is a box
      (let ((val (unbox stored)))
        (if (and found (hash-table-p val))
            val
            ;; Autovivify: create new hash, store it as a hash REFERENCE
            ;; (make-p-box) — a hash element that holds a nested hash holds a
            ;; ref, not a bare %hash.  Storing the raw table makes a later
            ;; scalar copy ($x = $h{a}) collapse to the key-count.
            (let ((new-hash (make-hash-table :test 'equal)))
              (setf (gethash k h) (make-p-box new-hash))
              new-hash))))))

(defun p-autoviv-gethash-for-array (hash key)
  "Get hash value, autovivifying to empty array if missing.
   Handles boxes in hash values."
  (let* ((h (unbox hash))
         (k (to-string key)))
    (multiple-value-bind (stored found) (gethash k h)
      ;; Unbox if stored value is a box
      (let ((val (unbox stored)))
        (if (and found (vectorp val))
            val
            ;; Autovivify: create new array, store it as an array REFERENCE.
            (let ((new-arr (make-array 0 :adjustable t :fill-pointer 0)))
              (setf (gethash k h) (make-p-box new-arr))
              new-arr))))))

(defun p-autoviv-aref-for-hash (arr idx)
  "Get array element, autovivifying to empty hash if missing.
   Handles boxes in array elements."
  (let* ((a (unbox arr))
         (i (truncate (to-number idx))))   ; to-number unboxes a boxed index ($a[$i]{..})
    ;; Extend array if needed; nil = slot exists but not assigned (like delete)
    (%p-extend-to a i)
    (let* ((stored (aref a i))
           ;; Unbox if element is a box
           (val (unbox stored)))
      (if (hash-table-p val)
          val
          ;; Autovivify: create new hash, store it as a hash REFERENCE.
          (let ((new-hash (make-hash-table :test 'equal)))
            (setf (aref a i) (make-p-box new-hash))
            new-hash)))))

(defun p-autoviv-aref-for-array (arr idx)
  "Get array element, autovivifying to empty array if missing.
   Handles boxes in array elements."
  (let* ((a (unbox arr))
         (i (truncate (to-number idx))))   ; to-number unboxes a boxed index ($a[$i]{..})
    ;; Extend array if needed; nil = slot exists but not assigned (like delete)
    (%p-extend-to a i)
    (let* ((stored (aref a i))
           ;; Unbox if element is a box
           (val (unbox stored)))
      (if (vectorp val)
          val
          ;; Autovivify: create new array, store it as an array REFERENCE.
          (let ((new-arr (make-array 0 :adjustable t :fill-pointer 0)))
            (setf (aref a i) (make-p-box new-arr))
            new-arr)))))

(defun p-array-set (arr idx value)
  "Set array element, extending array if needed.
   Stores values in boxes for l-value semantics."
  (let* ((a (unbox arr))
         (i (truncate (to-number idx))))   ; to-number unboxes a boxed index ($a[$i]{..})
    ;; Extend array if needed; nil = slot exists but not assigned (like delete)
    (%p-extend-to a i)
    ;; Get or create box at this index
    (let ((box (aref a i)))
      (unless (p-box-p box)
        (setf box (make-p-box nil))
        (setf (aref a i) box))
      (box-set box value))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Forward-declare so expand-autoviv can call expand-autoviv-for-array (mutually recursive)
  (declaim (ftype (function (t) t) expand-autoviv-for-array))
  (defun expand-autoviv (form)
    "Compile-time helper to expand nested hash/array access into autovivifying code.
     The result of this form must be a hash table (inner yields hash).
     Handles p-gethash, p-aref, p-gethash-deref, p-aref-deref chains."
    (cond
      ;; (p-gethash inner key) - autovivify intermediate, this slot yields hash
      ((and (listp form) (eq (car form) 'p-gethash))
       (let ((inner (cadr form))
             (key (caddr form)))
         `(p-autoviv-gethash ,(expand-autoviv inner) ,key)))
      ;; (p-aref inner idx) - intermediate array, this slot yields hash
      ((and (listp form) (eq (car form) 'p-aref))
       (let ((inner (cadr form))
             (idx (caddr form)))
         `(p-autoviv-aref-for-hash ,(expand-autoviv-for-array inner) ,idx)))
      ;; (p-gethash-deref $ref key) - autovivify $ref to hashref, slot yields hash
      ((and (listp form) (eq (car form) 'p-gethash-deref))
       (let ((ref (cadr form))
             (key (caddr form)))
         `(p-autoviv-gethash (p-ensure-hashref ,ref) ,key)))
      ;; (p-aref-deref $ref idx) - autovivify $ref to arrayref, slot yields hash
      ((and (listp form) (eq (car form) 'p-aref-deref))
       (let ((ref (cadr form))
             (idx (caddr form)))
         `(p-autoviv-aref-for-hash (p-ensure-arrayref ,ref) ,idx)))
      ;; Base case: form is a plain hash container
      (t form)))

  (defun expand-autoviv-for-array (form)
    "Compile-time helper: the result of this form must be an array.
     Handles p-gethash, p-aref, p-gethash-deref, p-aref-deref chains."
    (cond
      ;; (p-gethash inner key) - this slot yields array
      ((and (listp form) (eq (car form) 'p-gethash))
       (let ((inner (cadr form))
             (key (caddr form)))
         `(p-autoviv-gethash-for-array ,(expand-autoviv inner) ,key)))
      ;; (p-aref inner idx) - this slot yields array
      ((and (listp form) (eq (car form) 'p-aref))
       (let ((inner (cadr form))
             (idx (caddr form)))
         `(p-autoviv-aref-for-array ,(expand-autoviv-for-array inner) ,idx)))
      ;; (p-gethash-deref $ref key) - autovivify $ref to hashref, slot yields array
      ((and (listp form) (eq (car form) 'p-gethash-deref))
       (let ((ref (cadr form))
             (key (caddr form)))
         `(p-autoviv-gethash-for-array (p-ensure-hashref ,ref) ,key)))
      ;; (p-aref-deref $ref idx) - autovivify $ref to arrayref, slot yields array
      ((and (listp form) (eq (car form) 'p-aref-deref))
       (let ((ref (cadr form))
             (idx (caddr form)))
         `(p-autoviv-aref-for-array (p-ensure-arrayref ,ref) ,idx)))
      ;; Base case: form is a plain array container
      (t form))))

(defmacro p-autoviv-set (inner-hash-form outer-key value)
  "Set value with autovivification for nested hash access.
   inner-hash-form is (p-gethash hash inner-key) or deeper.
   Expands to code that ensures intermediate hashes exist."
  (let ((val-var (gensym "VAL"))
        (hash-var (gensym "HASH")))
    `(let ((,val-var ,value)
           (,hash-var ,(expand-autoviv inner-hash-form)))
       (setf (gethash (to-string ,outer-key) ,hash-var) ,val-var))))

(defmacro p-autoviv-aref-set (hash-chain idx value)
  "Set array element in a hash chain with autovivification.
   hash-chain is like (p-gethash ... key) and should yield an array.
   Expands to code that ensures intermediate structures exist."
  (let ((val-var (gensym "VAL"))
        (arr-var (gensym "ARR")))
    `(let ((,val-var ,value)
           (,arr-var ,(expand-autoviv-for-array hash-chain)))
       (p-array-set ,arr-var ,idx ,val-var))))

(defun p-gethash-deref (ref key)
  "Perl hash ref access $ref->{key} - unbox the reference first.
   Returns undef if ref is undef (nil box); write path auto-vivifies via (setf p-gethash-deref).
   When ref is a string, treats as symbolic reference to %name."
  (let ((h (unbox ref)))
    (cond
      ((or (null h) (eq h *p-undef*)) *p-undef*)
      ;; Symbolic reference: string used as hash name (no strict refs)
      ((stringp h)
       (when (find #\Nul h) (return-from p-gethash-deref *p-undef*))
       (let ((sym-hash (p-ensure-hashref ref)))
         (p-gethash sym-hash key)))
      ;; $scalarref->{k} on the READ path: perl's fatal.  Without it the box
      ;; reached SBCL's GETHASH and the program died with a type error naming a
      ;; P-BOX — uncatchable in Perl terms (#163 referent rule, #154's shape).
      ;; Only a still-boxed H can be one, so the guard is all the hot path
      ;; (an ordinary `$href->{k}`, where H is the hash) ever pays.
      ((and (p-box-p h) (%p-scalar-referent-p ref)) (%p-not-a-ref "HASH"))
      (t (p-gethash h key)))))

(defun (setf p-gethash-deref) (value ref key)
  "Setf expander for p-gethash-deref - autovivify ref to hash if undef, then set key"
  (setf (p-gethash (p-ensure-hashref ref) key) value))

(defun (setf p-aref-deref) (value ref idx)
  "Setf expander for p-aref-deref - autovivify ref to array if undef, then set element"
  (setf (p-aref (p-ensure-arrayref ref) idx) value))

(defun p-gethash-deref-box (ref key)
  "Live BOX at $ref->{key} — for \\$ref->{k} refgen and l-value ops, so a
   reference to a hashref element tracks later writes to that slot (unlike
   p-gethash-deref, which returns a snapshot value).  Autovivifies the ref to a
   hashref if undef, then returns the live box at key (like p-gethash-box does
   for a direct %hash element)."
  (p-gethash-box (p-ensure-hashref ref) key))

(defun p-aref-deref-box (ref idx)
  "Live BOX at $ref->[idx] — the array-ref analogue of p-gethash-deref-box."
  (p-aref-box (p-ensure-arrayref ref) idx))

;;; The KEY / INDEX list of a slice or slice-delete, flattened (#387 family
;;; 21, s413 — six functions spelled this loop, and the three that DISAGREED
;;; with it were the bugs of task #394): a non-string vector (a range `1..3`,
;;; an interpolated @list) and a non-empty list contribute their elements;
;;; anything else — a number, a box, a STRING (one key, never its characters:
;;; p-aslice used to explode "12" into #\1 #\2), nil — is one entry.  Every
;;; slice reader, KV reader and slice delete goes through here (the two array
;;; deletes used to take a range vector as ONE index and deleted element 0).
;;; Inline: it runs once per slice, and its callers already allocate.
(declaim (inline %p-flatten-slice-args))
(defun %p-flatten-slice-args (args)
  (loop for arg in args
        if (and (vectorp arg) (not (stringp arg)))
        append (coerce arg 'list)
        else if (and (listp arg) (not (null arg)))
        append arg
        else
        collect arg))

(defun p-aslice (arr &rest indices)
  "Perl array slice @arr[indices] - returns vector of values.
   Handles individual indices, lists, and vectors (from range operator)."
  (let ((flat-indices (%p-flatten-slice-args indices))
        (result (make-array 0 :adjustable t :fill-pointer 0)))
    (dolist (idx flat-indices result)
      (vector-push-extend (p-aref arr idx) result))))

(defun p-hslice (hash &rest keys)
  "Perl hash slice @hash{keys} - returns vector of values.
   Handles individual keys, lists, and vectors (from range operator).
   Strings are vectors in CL but must not be expanded into characters."
  (let ((flat-keys (%p-flatten-slice-args keys))
        (result (make-array 0 :adjustable t :fill-pointer 0)))
    (dolist (key flat-keys result)
      (vector-push-extend (p-gethash hash key) result))))

(defun p-kv-hslice (hash &rest keys)
  "Perl KV hash slice %hash{keys} - returns vector of key-value pairs.
   Handles individual keys, lists, and vectors (from range operator).
   Strings are vectors in CL but must not be expanded into characters."
  (let ((flat-keys (%p-flatten-slice-args keys))
        (result (make-array 0 :adjustable t :fill-pointer 0)))
    (dolist (key flat-keys result)
      (let ((k (to-string key)))
        (vector-push-extend k result)
        (vector-push-extend (p-gethash hash k) result)))))

(defun p-kv-aslice (arr &rest indices)
  "Perl KV array slice %arr[indices] - returns vector of (index, value) pairs.
   Handles individual indices, lists, and vectors (e.g. from range operator).
   Repeated indices yield repeated pairs, matching Perl semantics."
  (let ((flat-indices (%p-flatten-slice-args indices))
        (result (make-array 0 :adjustable t :fill-pointer 0)))
    (dolist (idx flat-indices result)
      (let* ((i (truncate (to-number idx)))
             (i (if (< i 0) (max 0 (+ (length arr) i)) i)))
        (vector-push-extend (make-p-box i) result)
        (vector-push-extend (p-aref arr i) result)))))

(defun p-hash (&rest pairs)
  "Create a Perl hash from key-value pairs.
   Stores values in boxes for l-value semantics.
   Flattens vectors (e.g. from %arr[...] kv-slice) and hash-tables
   (e.g. from %existing_hash used in list context) in the pair list."
  (let ((flat (loop for item in pairs
                    if (and (vectorp item) (not (stringp item)))
                    append (coerce item 'list)
                    else if (hash-table-p item)
                    append (loop for k being the hash-keys of item
                                 using (hash-value v)
                                 when (%p-real-hash-key-p k)
                                 collect k and collect v)  ; keep box so %p-make-hash-entry sees class
                    else
                    collect item))
        (h (make-hash-table :test 'equal)))
    (loop for (k v) on flat by #'cddr
          do (setf (gethash (to-string k) h) (%p-make-hash-entry v)))
    h))

(defun p-array-init (&rest elements)
  "Create a Perl array (adjustable vector) from elements.
   Flattens any nested arrays/vectors (but not strings) to handle
   expressions like [(@x) x 2] correctly.
   Stores elements in boxes for l-value semantics."
  (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
    (labels ((add-element (e)
               (cond
                 ;; String - wrap in box and add
                 ((stringp e)
                  (vector-push-extend (make-p-box e) result))
                 ;; A raw hash-table is a %hash written into the constructor —
                 ;; `[1, %h, 2]` — and Perl flattens it to key/value pairs.  It
                 ;; used to fall through to the scalar arm and become ONE
                 ;; element stringifying as HASH(0x..) (task #170).  Route
                 ;; through the canonical flatten and let the vector arm below
                 ;; store the result, rather than repeating the maphash.
                 ;; A hash REFERENCE is a p-box, not a raw table, so it still
                 ;; reaches the scalar arm and stays one element.
                 ((hash-table-p e)
                  (add-element (%p-flatten-list e)))
                 ;; Vector (array) - flatten its contents, preserving bless class
                 ((vectorp e)
                  (loop for item across e do (%p-array-store-scalar result item)))
                 ;; List - flatten its contents, preserving bless class
                 ((listp e)
                  (loop for item in e do (%p-array-store-scalar result item)))
                 ;; Scalar value - preserve bless class via %p-array-store-scalar
                 (t
                  (%p-array-store-scalar result e)))))
      (dolist (elem elements)
        (add-element elem)))
    result))

;; Hash iterator state for each() - maps hash-table to list of remaining keys
(defvar *hash-iterators* (make-hash-table :test 'eq))
;; Array iterator state for each() - maps array (by eq) to next index (integer)
;; No entry = fresh start (index 0). Entry = n (array length) = exhausted sentinel.
(defvar *array-iterators* (make-hash-table :test 'eq))

(defun p-each (collection)
  "Perl each function - returns next (key, value) pair from hash or (index, value) from array.
   Returns an empty vector when exhausted (list context) or *p-undef* (scalar context).
   Automatically resets after returning the exhausted sentinel."
  (cond
    ;; Array case: raw CL vector (not a string)
    ((and (vectorp collection) (not (stringp collection)))
     (let* ((n   (length collection))
            (i   (or (gethash collection *array-iterators*) 0)))
       (if (>= i n)
           ;; Exhausted sentinel or empty array: reset and return empty/undef
           (progn
             (remhash collection *array-iterators*)
             (if (eq *wantarray* t) (vector) *p-undef*))
           (let ((val (aref collection i)))
             ;; Advance: set end-sentinel if this is the last element
             (if (>= (1+ i) n)
                 (setf (gethash collection *array-iterators*) n)
                 (setf (gethash collection *array-iterators*) (1+ i)))
             (if (eq *wantarray* t)
                 (vector i (p-aref-unbox-elem val))
                 i)))))
    ;; Hash case
    ((hash-table-p collection)
     (multiple-value-bind (remaining exists-p)
         (gethash collection *hash-iterators*)
       ;; If not started yet, initialize iterator with all keys
       (unless exists-p
         (let ((keys nil))
           (maphash (lambda (k v) (declare (ignore v))
                      (when (%p-real-hash-key-p k) (push k keys))) collection)
           (setf remaining (nreverse keys))
           (setf (gethash collection *hash-iterators*) remaining)))
       ;; If remaining is empty, return exhaustion sentinel and reset iterator
       (if (null remaining)
           (progn
             (remhash collection *hash-iterators*)
             (if (eq *wantarray* t) (vector) *p-undef*))
           ;; Return next key/val pair
           (let* ((key (car remaining))
                  (val (gethash key collection)))
             (setf (gethash collection *hash-iterators*) (cdr remaining))
             (if (eq *wantarray* t)
                 (vector key (%p-hash-unbox-elem val))
                 (make-p-box key))))))
    ;; %ENV / %INC special hashes: iterate a keys snapshot, iterator state
    ;; keyed by the marker symbol itself.
    ((member (unbox collection) '(%ENV-MARKER% %INC-MARKER%))
     (let ((marker (unbox collection)))
       (multiple-value-bind (remaining exists-p) (gethash marker *hash-iterators*)
         (unless exists-p
           (setf remaining (coerce (p-keys marker) 'list))
           (setf (gethash marker *hash-iterators*) remaining))
         (if (null remaining)
             (progn
               (remhash marker *hash-iterators*)
               (if (eq *wantarray* t) (vector) *p-undef*))
             (let* ((key (car remaining))
                    (val (p-gethash marker key)))
               (setf (gethash marker *hash-iterators*) (cdr remaining))
               (if (eq *wantarray* t)
                   (vector key val)
                   (make-p-box key)))))))
    ;; Neither — return empty
    (t (vector))))

(defun p-keys (collection)
  "Perl keys function - also resets the each() iterator"
  (cond
    ;; Array case: return 0..n-1 and reset array iterator
    ((and (vectorp collection) (not (stringp collection)))
     (remhash collection *array-iterators*)
     (let* ((n (length collection))
            (result (make-array n :adjustable t :fill-pointer n)))
       (dotimes (i n) (setf (aref result i) i))
       result))
    ;; Hash case
    ((hash-table-p collection)
     (remhash collection *hash-iterators*)
     (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
       (maphash (lambda (k v)
                  (declare (ignore v))
                  (when (%p-real-hash-key-p k) (vector-push-extend k result)))
                collection)
       result))
    ;; %INC special hash: keys are the loaded modules' relative paths.
    ((eq (unbox collection) '%INC-MARKER%)
     (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
       (maphash (lambda (k v) (declare (ignore v)) (vector-push-extend k result))
                *p-inc-table*)
       result))
    ;; %ENV special hash: keys from the process environment.
    ((eq (unbox collection) '%ENV-MARKER%)
     (remhash '%ENV-MARKER% *hash-iterators*)
     (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
       (dolist (entry (sb-ext:posix-environ) result)
         (let ((p (position #\= entry)))
           (when p (vector-push-extend (subseq entry 0 p) result))))))
    ;; Neither
    (t (make-array 0 :adjustable t :fill-pointer 0))))

(defun p-values (collection)
  "Perl values function - returns unboxed values, also resets each() iterator"
  (cond
    ;; Array case: return elements and reset array iterator
    ((and (vectorp collection) (not (stringp collection)))
     (remhash collection *array-iterators*)
     (let* ((n (length collection))
            (result (make-array n :adjustable t :fill-pointer n)))
       (dotimes (i n) (setf (aref result i) (p-aref-unbox-elem (aref collection i))))
       result))
    ;; Hash case
    ((hash-table-p collection)
     (remhash collection *hash-iterators*)
     (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
       (maphash (lambda (k v)
                  (when (%p-real-hash-key-p k)
                    (vector-push-extend (%p-hash-unbox-elem v) result)))
                collection)
       result))
    ;; %INC special hash: values are the loaded modules' resolved paths.
    ((eq (unbox collection) '%INC-MARKER%)
     (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
       (maphash (lambda (k v) (declare (ignore k)) (vector-push-extend v result))
                *p-inc-table*)
       result))
    ;; %ENV special hash: values from the process environment.
    ((eq (unbox collection) '%ENV-MARKER%)
     (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
       (dolist (entry (sb-ext:posix-environ) result)
         (let ((p (position #\= entry)))
           (when p (vector-push-extend (subseq entry (1+ p)) result))))))
    ;; Neither
    (t (make-array 0 :adjustable t :fill-pointer 0))))

(defun %p-designator-hash (hash)
  "The hash-table behind a Perl hash DESIGNATOR, resolving the symbolic form.
   Under `no strict refs` a STRING in container position is a hash NAME
   (`$$p{k}`, and \"Pkg::\" is a stash).  The element read/write paths already
   resolve it (p-gethash / p-gethash-deref → p-ensure-hashref → p-cast-%);
   exists/delete used the raw string instead, so `exists $$p{k}` silently
   answered NO and `delete $$p{k}` crashed SBCL's GETHASH on a string.  One
   resolver for every element primitive.  Everything else — hash tables, the
   %ENV/%INC markers, a wrong referent — comes back unchanged, so the caller
   keeps its own dispatch."
  (let ((h (unbox hash)))
    (if (stringp h) (p-ensure-hashref hash) h)))

(defun %p-designator-array (arr)
  "The vector behind a Perl array DESIGNATOR — the array-side twin of
   %p-designator-hash (`exists $$p[0]` / `delete $$p[0]` under no strict refs)."
  (let ((a (unbox arr)))
    (if (stringp a) (p-ensure-arrayref arr) a)))

(defun p-exists (hash key)
  "Perl exists function"
  (let ((h (%p-designator-hash hash))
        (k (to-string key)))
    (cond
      ((eq h '%ENV-MARKER%) (not (null (sb-posix:getenv k))))
      ((eq h '%INC-MARKER%) (nth-value 1 (gethash k *p-inc-table*)))
      ;; Non-hash container (e.g. undef intermediate in `exists $h{a}{b}` where
      ;; $h{a} doesn't exist): Perl autovivifies $h{a} and the result is false.
      ;; We don't autovivify the intermediate, but must return false, not crash
      ;; gethash on a non-hash-table.
      ((hash-table-p h) (nth-value 1 (gethash k h)))
      ;; …but a DEFINITE wrong referent (exists $aryref->{k}) is perl's fatal,
      ;; not a false: the undef-intermediate leniency above is for undef only.
      ((%p-wrong-referent-p "HASH" h) (%p-not-a-ref "HASH"))
      (t nil))))

(defvar *p-stash-pkg-table* (make-hash-table :test 'eq :weakness :key)
  "Weak side-table mapping a stash snapshot hash (as returned by p-stash) to its
   Perl package name.  Lets mutation primitives recognize a stash and write
   through to the CL package: delete $Pkg::{name} must really remove the sub so
   *{Pkg::name}{CODE} and method dispatch stop seeing it (Moo's
   Method::Generate::Constructor bootstrap `delete _getstash(...)->{new}`
   depends on this).  Weak keys so abandoned snapshots are collected.")

(defun p-delete (hash key)
  "Perl delete function for hashes - returns unboxed value"
  (let ((h (%p-designator-hash hash))
        (k (to-string key)))
    (cond
      ((eq h '%ENV-MARKER%)
       (let ((old (sb-posix:getenv k)))
         (sb-posix:unsetenv k)
         (or old *p-undef*)))
      ((eq h '%INC-MARKER%)
       (multiple-value-bind (v found) (gethash k *p-inc-table*)
         (remhash k *p-inc-table*)
         (if found (unbox v) *p-undef*)))
      ((%p-wrong-referent-p "HASH" h) (%p-not-a-ref "HASH"))
      (t
       ;; Stash write-through: deleting a sub entry from a package stash
       ;; (delete $Pkg::{name}) must really remove the sub from the package.
       (let ((stash-pkg (gethash h *p-stash-pkg-table*)))
         (when stash-pkg
           (let ((sym (%p-resolve-sub-symbol
                       (concatenate 'string stash-pkg "::" k))))
             (when (and sym (fboundp sym))
               (fmakunbound sym)))))
       (multiple-value-bind (v found) (gethash k h)
         (remhash k h)
         (if found
             (%p-hash-unbox-elem v)
             *p-undef*))))))

(defun p-delete-array (arr idx)
  "Perl delete function for arrays.
   Sets element to nil (deleted marker) and returns the old value.
   Trims trailing nil slots (Perl shrinks array when last element deleted)."
  (let* ((a (%p-designator-array arr))
         (i (truncate (to-number idx)))
         (len (if (vectorp a) (length a) 0))
         (actual-idx (if (< i 0) (+ len i) i))
         (old-val (if (and (>= actual-idx 0) (< actual-idx len))
                      (p-aref-unbox-elem (aref a actual-idx))
                      *p-undef*)))
    (%p-check-array-writable a)                        ; task #159
    (when (%p-wrong-referent-p "ARRAY" a) (%p-not-a-ref "ARRAY"))
    (when (and (vectorp a) (>= actual-idx 0) (< actual-idx len))
      (setf (aref a actual-idx) nil)
      ;; Trim trailing nil slots (Perl semantics: deleting last element shrinks array)
      (loop while (and (> (fill-pointer a) 0)
                       (null (aref a (1- (fill-pointer a)))))
            do (decf (fill-pointer a))))
    old-val))

(defun p-exists-array (arr idx)
  "Perl exists function for arrays.
   Returns true only if element is within bounds AND is a box (assigned, not deleted)."
  (let* ((a (%p-designator-array arr))
         (i (truncate (to-number idx)))
         (len (if (vectorp a) (length a) 0))
         (actual-idx (if (< i 0) (+ len i) i)))
    (when (%p-wrong-referent-p "ARRAY" a) (%p-not-a-ref "ARRAY"))
    (and (vectorp a) (>= actual-idx 0) (< actual-idx len)
         (p-box-p (aref a actual-idx))
         ;; an unvivified deferred-element alias (@_ hole) is still a hole
         (not (%p-defelem-p (aref a actual-idx))))))

(defun p-delete-hash-slice (hash &rest keys)
  "Perl delete for hash slices: delete @hash{k1, k2, ...}
   Handles hash references (unboxes) and vector/list key arguments.
   Empty slice returns nil (undef) per [perl #29127]."
  (let* ((h (unbox hash))
         (flat-keys (%p-flatten-slice-args keys)))
    (when (null flat-keys) (return-from p-delete-hash-slice nil))
    ;; Wrong kind of referent (delete @{$aryref}{…}): perl's fatal.  The loop
    ;; below calls GETHASH directly rather than going through p-gethash, so it
    ;; needs its own guard (task #154; t/op/avhv.t t30).
    (when (%p-wrong-referent-p "HASH" h) (%p-not-a-ref "HASH"))
    (let ((result (make-array (length flat-keys) :adjustable t :fill-pointer 0)))
      (dolist (key flat-keys)
        (let ((k (to-string key)))
          (vector-push-extend (gethash k h *p-undef*) result)
          (remhash k h)))
      result)))

(defun p-delete-kv-hash-slice (hash &rest keys)
  "Perl delete for KV hash slices: delete %hash{k1, k2, ...}
   Handles hash references (unboxes) and vector/list key arguments.
   Empty slice returns nil (undef) per [perl #29127], as the three siblings do."
  (let* ((h (unbox hash))
         (flat-keys (%p-flatten-slice-args keys))
         (result (make-array 0 :adjustable t :fill-pointer 0)))
    (when (null flat-keys) (return-from p-delete-kv-hash-slice nil))
    ;; Wrong kind of referent: perl's fatal (task #154) — this loop calls
    ;; GETHASH directly rather than through p-gethash.
    (when (%p-wrong-referent-p "HASH" h) (%p-not-a-ref "HASH"))
    (dolist (key flat-keys)
      (let ((k (to-string key)))
        (vector-push-extend k result)
        (vector-push-extend (gethash k h *p-undef*) result)
        (remhash k h)))
    result))

(defun p-delete-array-slice (arr &rest indices)
  "Perl delete for array slices: delete @arr[i1, i2, ...]
   Sets elements to nil (deleted marker), trims trailing nils, and returns old
   values.  Empty slice returns nil (undef) per [perl #29127], as the hash twin
   does — BEFORE the writability check, because perl allows `delete @ro[()]` on
   a read-only array and dies only on `delete @ro[0]` (probed)."
  ;; a range / an interpolated @list arrives as ONE vector argument — flatten
  ;; like every other slice function (task #394: this and the KV twin used the
  ;; vector itself as an index, i.e. deleted element 0)
  (let ((indices (%p-flatten-slice-args indices)))
    (when (null indices) (return-from p-delete-array-slice nil))
    (%p-check-array-writable (unbox arr))                ; task #159
    (let* ((a (unbox arr))
           (result (make-array (length indices) :adjustable t :fill-pointer 0)))
      (dolist (idx indices)
        (let* ((i (truncate (to-number idx)))
               (len (if (vectorp a) (length a) 0))
               (old-val (if (and (>= i 0) (< i len))
                            (p-aref-unbox-elem (aref a i))
                            *p-undef*)))
          (when (and (vectorp a) (>= i 0) (< i len))
            (setf (aref a i) nil))  ; nil = deleted marker
          (vector-push-extend old-val result)))
      ;; Trim trailing nil slots (Perl: the array shrinks when its last
      ;; elements are deleted)
      (when (vectorp a)
        (loop while (and (> (fill-pointer a) 0)
                         (null (aref a (1- (fill-pointer a)))))
              do (decf (fill-pointer a))))
      result)))

(defun p-delete-kv-array-slice (arr &rest indices)
  "Perl delete for KV array slices: delete %arr[i1, i2, ...]
   Deletes elements at given indices and returns key-value pairs (index, value,
   ...).  Empty slice returns nil (undef) per [perl #29127], before the
   writability check — as p-delete-array-slice above."
  (let ((indices (%p-flatten-slice-args indices)))       ; task #394, as above
    (when (null indices) (return-from p-delete-kv-array-slice nil))
    (%p-check-array-writable (unbox arr))                ; task #159
    (let* ((a (unbox arr))
           (result (make-array 0 :adjustable t :fill-pointer 0)))
      (dolist (idx indices)
        (let* ((i (truncate (to-number idx)))
               (len (if (vectorp a) (length a) 0))
               (old-val (if (and (>= i 0) (< i len))
                            (let ((elem (aref a i)))
                              (if (p-box-p elem) elem *p-undef*))
                            *p-undef*)))
          (vector-push-extend (make-p-box i) result)
          (vector-push-extend old-val result)
          (when (and (vectorp a) (>= i 0) (< i len))
            (setf (aref a i) nil))))
      ;; Trim trailing nil slots (Perl: the array shrinks when its last
      ;; elements are deleted)
      (when (vectorp a)
        (loop while (and (> (fill-pointer a) 0)
                         (null (aref a (1- (fill-pointer a)))))
              do (decf (fill-pointer a))))
      result)))

(defun %p-stash-add-child-namespaces (pkg-name h)
  "Add a \"<child>::\" key to stash hash H for every registered Perl package one
   namespace segment deeper than PKG-NAME (\"\"/\"main\" = the root).  Values are
   irrelevant — consumers (Class::Inspector->subclasses via keys %{\"Foo::\"})
   only read the keys.  Orig-case names come from *pcl-pkg-name-map*, so
   single-segment packages keep their case (Foo, not FOO).  Runs even when
   PKG-NAME itself has no CL package, so intermediate namespaces (e.g. Sub:: when
   only Sub::Override exists) still report their children."
  (let* ((root (or (string= pkg-name "")
                   (string= (string-downcase pkg-name) "main")))
         (prefix (if root "" (concatenate 'string pkg-name "::")))
         (plen (length prefix)))
    (loop for perl-name being the hash-values of *pcl-pkg-name-map*
          do (let ((child
                    (cond
                      (root
                       (let ((c (search "::" perl-name)))
                         (if c (subseq perl-name 0 c) perl-name)))
                      ((and (> (length perl-name) plen)
                            (string= prefix (subseq perl-name 0 plen)))
                       (let* ((rest (subseq perl-name plen))
                              (c (search "::" rest)))
                         (if c (subseq rest 0 c) rest)))
                      (t nil))))
               (when (and child
                          (> (length child) 0)
                          (not (string= child "main")))
                 (setf (gethash (concatenate 'string child "::") h)
                       (make-p-box 1)))))))

(defun p-stash (pkg-name)
  "Return the package stash as a hash mapping Perl symbol names to code-ref boxes.
   Keys are lowercase Perl sub names; values are (make-p-box function).  Child
   namespaces are also added as \"<child>::\" keys (see %p-stash-add-child-namespaces).
   delete $::{foo} → (p-delete (p-stash \"main\") \"foo\") returns the code ref.
   Reads are a snapshot (not a live view); deletes write through to the package
   via *p-stash-pkg-table*."
  (let* ((pkg-str (if (or (string= (string-downcase pkg-name) "main")
                          (string= pkg-name ""))
                      "MAIN"
                      (%pcl-invert-case pkg-name)))
         (pkg (or (find-package pkg-str) (find-package pkg-name)))
         (h (make-hash-table :test 'equal)))
    (when pkg
      (do-symbols (sym pkg)
        (when (and (eq (symbol-package sym) pkg)
                   (fboundp sym)
                   ;; Skip forward-declaration STUBS (p-declare-sub).  A stub is
                   ;; fboundp but not a real definition; Perl would not have it in
                   ;; the symbol table yet.  Including it makes use-time package
                   ;; introspection (Moo::Role's make_role via _all_subs) see subs
                   ;; that are only *declared*, not yet defined — exactly the bug
                   ;; in docs/declaration-ordering-fix-plan.md.  A real definition
                   ;; flips the entry to :defined, so this only hides pure stubs.
                   (not (eq (gethash sym *p-declared-subs*) :stub)))
          (let* ((name (symbol-name sym))
                 (n (length name)))
            ;; PL-xxx → Perl sub "xxx".  Reverse the :invert reader transform
            ;; (invert is its own inverse) then strip the now-lowercase prefix,
            ;; so a mixed/upper sub name keeps its case (Bar→Bar, QUUX→QUUX).
            (when (and (>= n 4) (string-equal (subseq name 0 3) "PL-"))
              (let ((perl-name (subseq (%pcl-invert-case name) 3)))
                (setf (gethash perl-name h)
                      (make-p-box (symbol-function sym)))))))))
    (%p-stash-add-child-namespaces pkg-name h)
    (setf (gethash h *p-stash-pkg-table*) pkg-name)
    h))

;;; ============================================================
;;; Control Flow
;;; ============================================================

(defmacro p-if (condition then-form &optional else-form)
  "Perl if/unless and ternary"
  `(if (p-true-p ,condition) ,then-form ,else-form))

(defmacro p-unless (condition then-form &optional else-form)
  "Perl unless"
  `(if (not (p-true-p ,condition)) ,then-form ,else-form))

;;; Helper: extract :label, :my and :continue from loop body-and-keys.
;;; Returns (values label continue-form body myp).
;;;
;;; :my t marks a foreach whose loop variable is a perl `my` — the compiler
;;; states it because the runtime cannot see it.  Since the direction-D flip an
;;; ordinary package global and a lexical are spelled the SAME symbol, and the
;;; loop macros decide "localize the cell" vs "bind a lexical" by asking whether
;;; the symbol is a global symbol macro here (%p-cell-loop-var-p).  That reading
;;; is right for `foreach $x` and WRONG for `foreach my $x`, whose variable is a
;;; fresh lexical no matter what a package variable of the same name is doing —
;;; so the emitter marks the declaration and this key overrides the guess.
(defun parse-loop-keys (body-and-keys)
  (let* ((label (when (eq (first body-and-keys) :label)
                  (second body-and-keys)))
         (after-label (if label (cddr body-and-keys) body-and-keys))
         (myp (eq (first after-label) :my))
         (rest (if myp (cddr after-label) after-label))
         (continue-form nil)
         (body rest)
         (pos (position :continue rest)))
    (when pos
      (setf continue-form (nth (1+ pos) rest))
      (setf body (subseq rest 0 pos)))
    (values label continue-form body myp)))

;;; Helper: generate the inner iteration body structure for Perl loops.
;;; Handles labeled (catch/throw for next/redo across loop boundaries)
;;; and unlabeled (simple tagbody) variants.
(defun make-loop-iteration-body (label body)
  (if label
      (let ((next-tag (%pcl-loop-tag "NEXT" label))
            (redo-tag (%pcl-loop-tag "REDO" label))
            (iter-block (gensym "ITER")))
        `(catch ',next-tag
           (block ,iter-block
             (tagbody
              :redo
                (catch ',redo-tag
                  (progn ,@body (go :next)))
                (go :redo)
              :next))))
      (let ((iter-block (gensym "ITER")))
        ;; Wrap body in progn: an atom body form (e.g. a bare literal loop body
        ;; like `"foo" for (1)`) spliced straight into a tagbody would be read as
        ;; a go-tag ("not a legal go tag"), not a value.
        `(block ,iter-block
           (tagbody :redo (progn ,@body) :next)))))

(defmacro p-while (condition &rest body-and-keys)
  "Perl while loop with optional :label and :continue.
Uses tagbody/go instead of loop so that (return-from nil ...) from p-return
inside the loop body correctly exits the enclosing function, not just the loop.
CL's (loop) creates an implicit (block nil ...) that would intercept p-return.
Labeled form adds (catch 'pcl::LAST-LABEL ...) so that 'last LABEL' works
dynamically (across function calls), matching p-next/p-redo behavior."
  (multiple-value-bind (label continue-form body) (parse-loop-keys body-and-keys)
    (let ((block-name (or label (gensym "WHILE")))
          (last-tag (when label (%pcl-loop-tag "LAST" label))))
      `(block ,block-name
         ,(let ((inner `(block nil    ; for unlabeled p-last
                          (tagbody
                           :next
                             (unless (p-true-p ,condition) (return-from ,block-name ""))
                             ,(make-loop-iteration-body label body)
                             ,@(when continue-form (list continue-form))
                             (go :next)))))
            (if label
                `(catch ',last-tag ,inner)
                inner))))))

(defmacro p-until (condition &body body)
  "Perl until loop"
  `(p-while (p-! ,condition) ,@body))

(defmacro p-do-while (condition &body body)
  "Perl post-test loop: `do BLOCK while COND` — BODY always runs at least once,
the condition is tested afterwards.  Per perlsyn, `do {} while/until` takes no
loop-control statements (no last/next/redo, no labels), so this is a plain
tagbody with no (block nil ...) — a p-return from a sub called inside BODY
propagates up unhindered."
  (let ((g (gensym "DOWHILE")))
    `(tagbody
        ,g
        (progn ,@body)
        (when (p-true-p ,condition) (go ,g)))))

(defmacro p-do-until (condition &body body)
  "Perl post-test loop: `do BLOCK until COND` = `do BLOCK while (not COND)`."
  `(p-do-while (p-! ,condition) ,@body))

(defmacro p-for ((&optional init) (test) (&optional step) &rest body-and-keys)
  "Perl C-style for loop with optional :label.
Uses tagbody/go instead of loop — see p-while for rationale."
  (multiple-value-bind (label _continue body) (parse-loop-keys body-and-keys)
    (declare (ignore _continue))
    (let ((block-name (or label (gensym "FOR")))
          (last-tag (when label (%pcl-loop-tag "LAST" label))))
      `(block ,block-name
         ,init
         ,(let ((inner `(block nil    ; for unlabeled p-last
                          (tagbody
                           :next
                             (unless (p-true-p ,test) (return-from ,block-name ""))
                             ,(make-loop-iteration-body label body)
                             ,@(when step (list step))
                             (go :next)))))
            (if label
                `(catch ',last-tag ,inner)
                inner))))))

(defun ensure-vector (val)
  "Ensure value is a vector for iteration. Non-vectors become single-element vectors."
  (cond
    ((vectorp val) val)
    ((listp val) (coerce val 'vector))
    (t (vector val))))

(defun %p-foreach-elt (vec i)
  "Bind the foreach alias var for slot I of VEC.  An existing box aliases
directly; a raw value gets a fresh box (a temporary — writes are not
aliased); a NIL slot is an array HOLE and aliases via a deferred-element
box that vivifies (aref VEC I) on first write (%p-defelem-box)."
  (let ((slot (aref vec i)))
    (cond ((p-box-p slot) slot)
          ((null slot) (%p-defelem-box vec i))
          (t (make-p-box slot)))))

(defun %p-flatten-for-list (raw)
  "Flatten a value for use as a foreach list.
   - p-box wrapping a vector (@array passed directly) -> iterate over elements
   - Raw CL vector from codegen (vector ...) -> p-flatten-markers are spread,
     everything else (p-box scalars/refs, raw scalars) kept as-is
   - Scalar -> single-element vector"
  (let ((val (unbox raw)))
    (cond
      ((hash-table-p val)
       ;; %hash as a foreach list flattens to its key/value pairs (same as %p-flatten-list)
       (coerce (%p-hash-keyval-list val) 'vector))
      ((not (and (vectorp val) (not (stringp val))))
       (vector raw))
      ((p-box-p raw)
       ;; @array box passed as a single list expression — iterate its elements directly
       val)
      ((and (adjustable-array-p val) (array-has-fill-pointer-p val))
       ;; Bare lexical @array (raw adjustable vector, not a codegen (vector ...)
       ;; literal — those are simple vectors, same discrimination box-set uses).
       ;; Iterate the LIVE array directly: hole slots alias via %p-foreach-elt,
       ;; and a push during the loop extends the iteration, both like perl.
       val)
      (t
       ;; CL vector from codegen (vector ...): items are scalars, p-flatten-markers,
       ;; or raw CL vectors (function return values).
       ;; p-flatten-markers (from @array items in foreach list) are spread.
       ;; p-box items (arrayrefs, scalar refs) are kept as scalars.
       ;; Raw CL vectors (from keys/values/grep etc.) are spread.
       (let ((result (make-array 8 :adjustable t :fill-pointer 0)))
         (loop for item across val do
               (cond
                 ((p-flatten-marker-p item)
                  (let ((src (p-flatten-marker-array item)))
                    (when (and (vectorp src) (not (stringp src)))
                      ;; HOLE slots (nil) alias through a deferred-element box
                      ;; tied to the SOURCE array, not this flattened copy —
                      ;; `for (@a, @b) { $_ = 1 }` must vivify @a's slot.
                      (loop for j from 0
                            for x across src
                            do (vector-push-extend
                                (if (null x) (%p-defelem-box src j) x)
                                result)))))
                 ((and (not (p-box-p item)) (vectorp item) (not (stringp item)))
                  ;; Raw CL vector from function return (keys, grep, etc.) — spread
                  (loop for x across item do (vector-push-extend x result)))
                 (t
                  (vector-push-extend item result))))
         result)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %p-cell-loop-var-p (var env)
    "Is VAR an ORDINARY package global — a `p-defcell` symbol macro — in ENV?

     Perl's `foreach $pkgvar (…)` implicitly LOCALIZES the package variable:
     the loop var is aliased to each element for the body's dynamic extent and
     restored afterwards, and a sub called from the body sees the current
     element.  A `let` gives that only for a SPECIAL variable; over a
     direction-D cell (task #289) a `let` would install a lexical shadow the
     called sub cannot see — silently wrong.  So the loop macros ask this
     question and switch to save/set/restore over the cell.

     ENV is the macroexpansion environment, which makes the answer correct for
     shadowing too: inside `(let (($x …)) …)` the name is a lexical, not a
     symbol macro, so `foreach $x` over an ENCLOSING `my $x` binds lexically.
     Exception-set names ($_, $a/$b, magic) are plain specials and answer NIL
     here, keeping today's dynamic bind.

     What ENV canNOT tell us is `foreach MY $x` — the loop's own declaration
     is not in the environment yet when this macro expands, so a package
     variable of the same name makes this answer T and the loop would localize
     a cell where perl declares a fresh lexical (a called sub then saw the loop
     value: task #294).  The compiler states that case with :my, which the
     callers consult FIRST — this predicate is only ever asked about an
     UNDECLARED loop variable."
    (and (symbolp var) (nth-value 1 (macroexpand-1 var env)))))

(defmacro p-foreach ((var list) &rest body-and-keys &environment env)
  "Perl foreach loop with optional :label, :my and :continue.
Uses tagbody/go instead of loop -- see p-while for rationale.
A package-global loop var is localized over its cell (%p-cell-loop-var-p);
:my says the compiler declared the variable, which settles that question."
  (multiple-value-bind (label continue-form body myp) (parse-loop-keys body-and-keys)
    (let* ((block-name (or label (gensym "FOREACH")))
           (last-tag (when label (%pcl-loop-tag "LAST" label)))
           (vec (gensym))
           (raw (gensym))
           (old (gensym "OLD"))
           (i (gensym))
           (cellp (and (not myp) (%p-cell-loop-var-p var env)))
           (iter-forms (cons `(incf ,i)
                             (cons (make-loop-iteration-body label body)
                                   (when continue-form (list continue-form))))))
      `(block ,block-name
         (let* ((,raw (let ((*wantarray* t)) ,list))  ; list in list-context; body keeps outer context
                (,vec (%p-flatten-for-list ,raw))
                (,i 0)
                ,@(when cellp `((,old (sb-ext:symbol-global-value ',var)))))
           ,(let* ((inner `(block nil    ; for unlabeled p-last
                             (tagbody
                              :next
                                (when (>= ,i (length ,vec)) (return-from ,block-name ""))
                                ,(if cellp
                                     `(progn
                                        (setf (sb-ext:symbol-global-value ',var)
                                              (%p-foreach-elt ,vec ,i))
                                        ,@iter-forms)
                                     `(let ((,var (%p-foreach-elt ,vec ,i)))
                                        ,@iter-forms))
                                (go :next))))
                   (wrapped (if label `(catch ',last-tag ,inner) inner)))
              (if cellp
                  `(unwind-protect ,wrapped
                     (setf (sb-ext:symbol-global-value ',var) ,old))
                  wrapped)))))))

(defun %expand-foreach-range (rawp var from to body-and-keys env)
  "Shared expander for p-foreach-range / p-foreach-range-raw.  RAWP selects the
loop-var binding: NIL binds a fresh box per iteration (like p-foreach's
ensure-boxed — required for $_ and any var the annotator could not prove
unboxable), T binds the raw counter value (annotator-approved named vars; the
string-range fallback vector holds raw strings, also fine in a raw slot).
A package-global loop var (%p-cell-loop-var-p, overridden by :my) is localized
over its cell instead of let-bound, and is ALWAYS boxed: the cell's contract is
that it holds a box, and a raw integer parked there would be read as one by
anything that reaches the global by name."
  (multiple-value-bind (label continue-form body myp) (parse-loop-keys body-and-keys)
    (let* ((block-name (or label (gensym "FOREACH")))
           (last-tag (when label (%pcl-loop-tag "LAST" label)))
           (kind (gensym))
           (a (gensym))
           (b (gensym))
           (vec (gensym))
           (i (gensym))
           (old (gensym "OLD"))
           (hi (gensym))
           (cellp (and (not myp) (%p-cell-loop-var-p var env))))
      `(block ,block-name
         (multiple-value-bind (,kind ,a ,b)
             (let ((*wantarray* t))    ; endpoints in the list's context, like p-foreach's list
               (%p-range-classify ,from ,to))
           (let* ((,vec (when (eq ,kind :string) (p-.. ,a ,b)))
                  (,i (if ,vec 0 ,a))
                  (,hi (if ,vec (1- (length ,vec)) ,b))
                  ,@(when cellp `((,old (sb-ext:symbol-global-value ',var)))))
             ,(let* ((val (if (and rawp (not cellp))
                              `(if ,vec (aref ,vec ,i) ,i)
                              `(if ,vec
                                   (ensure-boxed (aref ,vec ,i))
                                   (make-p-box ,i))))
                     (iter-forms (cons `(incf ,i)
                                       (cons (make-loop-iteration-body label body)
                                             (when continue-form (list continue-form)))))
                     (inner `(block nil    ; for unlabeled p-last
                               (tagbody
                                :next
                                  (when (> ,i ,hi) (return-from ,block-name ""))
                                  ,(if cellp
                                       `(progn
                                          (setf (sb-ext:symbol-global-value ',var) ,val)
                                          ,@iter-forms)
                                       `(let ((,var ,val))
                                          ,@iter-forms))
                                  (go :next))))
                     (wrapped (if label `(catch ',last-tag ,inner) inner)))
                (if cellp
                    `(unwind-protect ,wrapped
                       (setf (sb-ext:symbol-global-value ',var) ,old))
                    wrapped))))))))

(defmacro p-foreach-range ((var from to) &rest body-and-keys &environment env)
  "Perl foreach over a SINGLE range (`for $v (A..B)`) — perl's own counting-loop
optimization: numeric ranges iterate a counter and never materialize the range
vector (p-.. would allocate B-A+1 elements).  Endpoints are evaluated ONCE, up
front; the numeric-vs-magical-string decision is %p-range-classify (shared with
p-..), made at RUNTIME, so string ranges ('a'..'e') fall back to iterating the
materialized vector.  Same :label/:continue protocol and lexical skeleton as
p-foreach (unlabeled p-next/p-last are a lexical (go :next)/(return nil)); the
body appears ONCE — only the per-iteration value source branches on the vec."
  (%expand-foreach-range nil var from to body-and-keys env))

(defmacro p-foreach-range-raw ((var from to) &rest body-and-keys &environment env)
  "p-foreach-range with a RAW loop-var binding (no per-iteration box).  Emitted
only when the VarAnnotator proves the body never captures/aliases the var —
and never for $_, which must stay a box (s///, chomp write through it)."
  (%expand-foreach-range t var from to body-and-keys env))

(defun p-return-value (val)
  "Prepare a value for return - unbox simple scalars but keep references intact."
  (cond
    ;; Not a box - handle arrays context-sensitively
    ((not (p-box-p val))
     (cond
       ;; Plain array in scalar context: Perl array-in-scalar = element count.
       ;; This matches box-set (which counts adjustable vectors), so explicit
       ;; `return @a` agrees with the implicit-tail form `sub { @a }`.  Note: a
       ;; literal list `return (5,3,1)` does NOT reach here — it arrives as
       ;; multiple values and is handled by p-return's multi-value branch (last
       ;; element).  Only array variables, map/grep results, and blocks returning
       ;; arrays reach here as a single vector; Perl counts those in scalar.
       ((and (not *wantarray*)
             (vectorp val) (not (stringp val)) (adjustable-array-p val))
        (length val))
       ;; nil (undef/empty-list) in list context: return empty list vector
       ;; so bare `return` and `return ()` contribute 0 elements to surrounding list.
       ((and (eq *wantarray* t) (null val))
        (make-array 0 :adjustable t :fill-pointer 0))
       (t val)))
    ;; Blessed box - return the whole box so the class is preserved.
    ;; Needed for e.g. bless \$scalar (scalar-ref inside box): the box carries the
    ;; class, unboxing strips it.  Also fixes bless [] returning a vector that
    ;; box-set would then convert to an element count via the adjustable-vector rule.
    ((p-box-class val) val)
    ;; Scalar reference or typeglob reference (is-ref set) - return the box intact.
    ;; A scalar ref's inner is a p-box and a glob ref's is a p-typeglob, so neither
    ;; matches the hash/vector/function test below; without this, `return \$x` (and
    ;; `return $_[0]` when $_[0] is a directly-passed \$x) unboxed the ref one level
    ;; and stripped it to a plain scalar.  is-ref is set only by p-backslash for
    ;; scalar/glob refs (array/hash/code refs don't set it), so this is exact.
    ((p-box-is-ref val) val)
    ;; Box containing a reference (hash, array, function) - return the box intact.
    ;; The box IS the reference (hashref/arrayref/coderef). Stripping it would give
    ;; a raw hash-table/vector/function, which box-set then misinterprets.
    ((let ((v (p-box-value val)))
       (or (hash-table-p v) (vectorp v) (functionp v)))
     val)
    ;; Simple scalar box - return the unboxed value
    (t (unbox val))))

(defun p-list-scalar (val)
  "A list/slice evaluated in scalar context yields its LAST element (undef if
   empty) — the comma-operator semantics.  This differs from an array variable
   in scalar context, which yields the element COUNT.  Slices, sort, and bare
   list literals use this; arrays/map/grep/keys/values use the count path."
  (if (and (vectorp val) (not (stringp val)))
      (if (zerop (length val)) nil (aref val (1- (length val))))
      val))

(defun p-slice-result (val)
  "Context-dispatch for a slice whose context is only known at runtime (e.g. it
   is the argument of `return`): list context keeps the vector, scalar context
   reduces to the last element.  *wantarray* is :void / t (truthy) for
   void/list and nil for scalar."
  (if *wantarray* val (p-list-scalar val)))

(defun %p-goto-target (val)
  "Resolve the operand of `goto EXPR` to the function to tail-call.
   Perl's goto EXPR takes EITHER a code ref — a tail call that replaces the
   frame, exactly like `goto &NAME` — or a label name for a computed goto.
   Only the code-ref half is expressible in CL (a tagbody tag is not a first-
   class value).  Returns the function, or NIL for the label form AFTER
   naming it on stderr.  Before this, `goto \\&sub` — the form Capture::Tiny's
   whole API is built on — lowered to a silent no-op: the caller got undef,
   with no error and no output (task #199).

   Announced-not-silent rather than fatal (the #155 tie shape, not the rule-12
   die): a die here aborts the whole program over one unimplementable
   construct, and measurably does — `perl-tests/state.t` runs 157 of its 166
   rows with the warning and stops at 69 with a die, because its computed goto
   sits two thirds of the way up a file that is otherwise about `state`.  The
   old no-op's sin was the silence, not the fall-through."
  (let ((fn (unbox val)))
    ;; Blessed coderefs are stored as box(inner-box(lambda)) — same double
    ;; unbox p-funcall-ref does.
    (when (p-box-p fn) (setf fn (p-box-value fn)))
    (if (functionp fn)
        fn
        (progn
          (format *error-output*
                  "PCL: goto to the computed LABEL ~S is not supported — execution falls through (docs/not-supported.md).~%"
                  (to-string fn))
          nil))))

(defmacro p-goto-sub (fn)
  "Perl goto &func — tail-call the target function with the current @_.
   Replaces the current frame by throwing :p-return with the result.
   @_ must be the CL variable bound by the enclosing p-sub.

   goto &sub REPLACES the current call frame: the target must see the *caller*
   of the goto-ing sub, not the goto-ing sub itself (Perl semantics — e.g.
   Moo::Role::import does `goto &Role::Tiny::import` and Role::Tiny relies on
   `caller` reporting the original `use`r's package).  So pop the goto-ing sub's
   frame and restore *pcl-current-package* to its caller before applying FN;
   FN's own p-sub prologue then pushes that caller as FN's calling frame.

   The TARGET expression is evaluated FIRST, in the CURRENT (un-popped) frame:
   `goto &{EXPR}` runs EXPR before the frame is replaced, so Exporter's
   `goto &{as_heavy()}` lets as_heavy read (caller(1))[3] off the real stack to
   choose heavy_import vs heavy_export.  Only then pop and apply.

   *wantarray* must be restored to *pcl-caller-wantarray* around the apply:
   the goto statement itself runs in per-statement context (usually void), but
   the target inherits the ORIGINAL caller's context — `sub f { goto &g }` in
   list context must run g in list context (s329 review; same restore p-return
   does for its argument)."
  (let ((target (gensym "GOTO-TARGET")))
    `(let ((,target ,fn))
       (throw :p-return
         (let ((*wantarray* *pcl-caller-wantarray*)
               (*pcl-current-package*
                (if *pcl-caller-pkg-stack*
                    (car *pcl-caller-pkg-stack*) *pcl-current-package*))
               (*pcl-caller-pkg-stack*
                (if *pcl-caller-pkg-stack*
                    (cdr *pcl-caller-pkg-stack*) *pcl-caller-pkg-stack*))
               (*pcl-caller-subname-stack*
                (if *pcl-caller-subname-stack*
                    (cdr *pcl-caller-subname-stack*) *pcl-caller-subname-stack*)))
           (apply ,target (coerce @_ 'list)))))))

(defmacro p-goto-computed (expr)
  "Perl `goto EXPR` — the shape codegen emits when the operand is neither
   `&NAME` nor a bare label: `goto \\&NAME`, `goto $coderef`, `goto $h->{cb}`.
   All of those are TAIL CALLS in perl, so route them through the one
   mechanism that already implements the frame-replacing semantics.
   %p-goto-target answers NIL for the one shape CL cannot express — a computed
   LABEL name — having named it on stderr first; execution then falls through
   past the goto, which is what the old silent no-op did."
  (let ((target (gensym "GOTO-FN")))
    `(let ((,target (%p-goto-target ,expr)))
       (when ,target (p-goto-sub ,target)))))

(defmacro p-return (&rest values)
  "Perl return - returns single value or list depending on args.
   Evaluates argument(s) with *wantarray* restored to *pcl-caller-wantarray*
   so that 'return do { @a, @b }' and similar see the correct calling context.
   Uses throw :p-return to bypass (block nil ...) from loops (for p-last),
   so return always exits the enclosing p-sub, not just the innermost loop."
  (if (null values)
      ;; Bare return: in list context contributes 0 elements; scalar/void → undef.
      `(throw :p-return
         (if (eq *pcl-caller-wantarray* t)
             (make-array 0 :adjustable t :fill-pointer 0)
             nil))
      (if (= (length values) 1)
          `(throw :p-return
             (let ((*wantarray* *pcl-caller-wantarray*))
               (p-return-value ,(car values))))
          `(throw :p-return
             (let ((*wantarray* *pcl-caller-wantarray*))
               (if (eq *wantarray* t)
                   ;; List flattening: return ($i, map ...) splices array-valued
                   ;; elements (raw vectors / non-blessed hashes) like any Perl
                   ;; list; boxes (refs) stay intact.  p-flatten-args is the
                   ;; same rule @_ uses.
                   (p-flatten-args (list ,@(mapcar (lambda (v) `(p-return-value ,v)) values)))
                   (p-return-value ,(car (last values)))))))))

(defmacro p-last (&optional label)
  "Perl last (break) - optionally with label to exit specific loop.
Labeled form uses throw so it works across function calls (like p-next/p-redo)."
  (if label
      `(throw ',(%pcl-loop-tag "LAST" label) nil)
      `(return nil)))

(defun p-last-dynamic (label-name)
  "Dynamic (cross-function) labeled last: throws to LAST-<LABEL> catch tag.
Used e.g. by p-skip to implement Test::More's skip() which calls (last SKIP)."
  ;; The loop's catch tag is built from the label SYMBOL (LAST-<sym-name>), whose
  ;; name is the :invert-read form; invert the runtime string label to match.
  (throw (%pcl-loop-tag "LAST" label-name) nil))

(defmacro p-next (&optional label)
  "Perl next (continue) - optionally with label to continue specific loop"
  (if label
      `(throw ',(%pcl-loop-tag "NEXT" label) nil)
      `(go :next)))

(defmacro p-redo (&optional label)
  "Perl redo - optionally with label to redo specific loop"
  (if label
      `(throw ',(%pcl-loop-tag "REDO" label) nil)
      `(go :redo)))

(defun p-continue ()
  "Perl continue (given/when) - fall through to next when clause"
  (error "Can't \"continue\" outside a when block"))

(defun p-break ()
  "Perl break (given/when) - exit given block"
  (error "Can't \"break\" outside a when block"))

;;; ============================================================
;;; I/O Functions
;;; ============================================================

(defun %p-out-fh-or-fail (desig site)
  "Resolve DESIG — the explicit filehandle of print/printf/say, SITE naming
   which — to a stream, or return NIL after doing what PERL does with a handle
   it cannot write to.  Callers return false when this returns NIL.

   Task #152 (CLAUDE.md rule 12) — and a case the audit MEASURED and then did
   NOT convert.  Both halves of the obvious \"stop being silent\" fix were tried
   and both are wrong; the record is here so they are not retried:

     * WARN \"print() on unopened filehandle NAME\"?  No — that warning is
       `use warnings`-GATED in perl, not default-on.  Measured (scratchpad
       w1.pl vs w2.pl): plain `perl` prints NOTHING and returns undef, exactly
       like PCL.  PCL's silence is perl's silence.  Emitting it unconditionally
       broke fileio-02.t and transpile-test-09.t, which assert the default
       quiet.  Re-adding it needs a real `use warnings` model — PCL tracks no
       warnings state anywhere today.

     * DIE \"Can't use an undefined value as a symbol reference\" on an
       undefined designator?  Perl does exactly that for a handle that was
       never opened — but NOT for one that was closed, where it returns undef.
       PCL cannot tell the two apart: %p-forget-fh leaves the variable
       UNDEFINED after close, so `print $closed_fh …` and
       `my $u; print {$u} …` arrive here identical.  Dying would break the
       closed-handle rows of transpile-test-09.t (#186), which are right.
       Separating them needs close() to leave a closed-handle value behind
       instead of nothing — a representation change, not a dispatch fix.

   So this arm stays as perl-without-warnings behaves: no output, $! = EBADF,
   return false.  What the audit DID fix here is the duplication — print,
   printf and say each carried their own copy of this arm (CLAUDE.md 11), so a
   change like the above had to be made, and measured, three times."
  (declare (ignore site))
  (cond
    ;; `:fh nil` is the EMPTY filehandle node (bare `print`), not an undef
    ;; scalar — it means the currently-selected handle.
    ((null desig) (%p-default-out))
    ((p-get-stream desig))
    (t
     (setf *p-stored-errno* 9)                                 ; EBADF (Linux)
     (setf (sb-alien:extern-alien "errno" sb-alien:int) 9)
     nil)))

(defun p-print (&rest args)
  "Perl print - prints args then appends $\\ (output record separator)"
  (let ((fh (%p-default-out)))
    ;; Check for :fh keyword
    (when (and (>= (length args) 2) (eq (first args) :fh))
      (let ((desig (second args)))
        (setf args (cddr args))
        (let ((resolved (%p-out-fh-or-fail desig "print")))
          ;; NIL = perl already warned (or died); the write does not happen.
          (unless resolved (return-from p-print *p-undef*))
          (setf fh resolved))))
    ;; The bare-form $_ default (`print;` / `print FH;` / `say;` / `printf;`)
    ;; is supplied EXPLICITLY by the codegen (ExprToCL gen_funcall emits
    ;; `(p-print … $_)`), so the generated CL is self-describing and there is no
    ;; hidden runtime default here.  A genuinely empty LIST (`print @empty`)
    ;; correctly prints nothing — it never reaches this function with null args.
    ;; Flatten raw @array / %hash args (print takes a LIST): a bare vector/hash
    ;; spreads to its elements/pairs, while a p-box-wrapped ref stays a scalar
    ;; (so `print $aref` prints ARRAY(0x..)). Same rule as @_ argument flattening.
    ;; $, (output field separator) is printed BETWEEN successive arguments.
    (let ((ofs (let ((v (unbox |$,|))) (and (stringp v) (plusp (length v)) v)))
          (firstp t))
      (dolist (arg (coerce (p-flatten-args args) 'list))
        (when (and ofs (not firstp)) (princ ofs fh))
        (setf firstp nil)
        (princ (to-string arg) fh)))
    ;; Append output record separator $\ if set
    (let ((ors (unbox |$\\|)))
      (when (and (stringp ors) (plusp (length ors)))
        (princ ors fh)))
    (%p-maybe-autoflush fh)
    t))

(defun p-say (&rest args)
  "Perl say (print with newline)"
  (let ((fh (%p-default-out)))
    ;; Resolve the target handle once (same rules as p-print); bail with EBADF
    ;; on a named-but-unopened handle so we don't emit a stray newline.
    (when (and (>= (length args) 2) (eq (first args) :fh))
      (let ((desig (second args)))
        (let ((resolved (%p-out-fh-or-fail desig "say")))
          (unless resolved (return-from p-say *p-undef*))
          (setf fh resolved))))
    (apply #'p-print args)
    (terpri fh)
    (%p-maybe-autoflush fh)
    t))

(defun p-warn-is-reference (val)
  "Check if val is a Perl reference (hash, array ref, blessed object, etc.)"
  (or (hash-table-p val)
      (and (vectorp val) (not (stringp val)))
      (and (p-box-p val)
           (let ((v (p-box-value val)))
             (or (hash-table-p v)
                 (and (vectorp v) (not (stringp v)))
                 (p-box-p v)         ; variable box wrapping a reference box
                 (p-box-class val))))))

(defun p-warn-build-message (args &optional loc)
  "Build warn message string per Perl semantics:
   - Non-scalar (ref): return as-is
   - Scalar with trailing newline: use as-is
   - Scalar without trailing newline: append 'at FILE line N.'
   - Empty string or no args: use $@ if set, else 'Warning: something's wrong'
   LOC, when supplied (codegen passes the real 'FILE line N' for an explicit
   warn), replaces the placeholder 'unknown line 0' in the appended suffix."
  (let* ((at-loc (or loc "unknown line 0"))
         (empty-or-no-args
          (or (null args)
              (and (= (length args) 1)
                   (let ((a (car args)))
                     (string= (to-string (unbox a)) ""))))))
    (cond
      ;; No args or empty string: use $@ or default message
      (empty-or-no-args
       (let ((err $@))
         (cond
           ;; $@ is a reference → pass it as the warning object
           ((p-warn-is-reference err)
            err)
           ;; $@ is a non-empty string → append "...caught at"
           ;; $@ already ends with \n (Perl convention), so just concatenate
           ((and (stringp (to-string (unbox err)))
                 (> (length (to-string (unbox err))) 0))
            (format nil "~A~A...caught at ~A.~%" (to-string (unbox err))
                    #\Tab at-loc))
           ;; No $@ → default warning
           (t (format nil "Warning: something's wrong at ~A.~%" at-loc)))))
      ;; Single ref arg: return as-is
      ((and (= (length args) 1) (p-warn-is-reference (car args)))
       (car args))
      ;; Otherwise: stringify and append location if needed
      (t
       (let ((s (if (= (length args) 1)
                    (to-string (unbox (car args)))
                    (apply #'p-string-concat args))))
         (if (and (> (length s) 0)
                  (char= (char s (1- (length s))) #\Newline))
             s
             (format nil "~A at ~A.~%" s at-loc)))))))

(defun p-warn (&rest raw-args)
  "Perl warn - respects $SIG{__WARN__} handler.
   Accepts an optional (:loc \"FILE line N\") marker from codegen for the
   ' at FILE line N.' suffix on a message that doesn't end in a newline."
  (multiple-value-bind (args loc) (%p-extract-loc raw-args)
    (let* ((msg (p-warn-build-message args loc))
           (handler (gethash "__WARN__" %SIG)))
      (cond
        ;; Custom handler: call with message as argument ($_[0])
        ((and handler (functionp (unbox handler)))
         (let ((boxed (if (p-box-p msg) msg (make-p-box msg))))
           (funcall (unbox handler) boxed)))
        ;; "IGNORE": suppress warning
        ((and handler (stringp (unbox handler))
              (string= (unbox handler) "IGNORE"))
         nil)
        ;; Default: print to *error-output*
        (t
         (let ((s (if (p-box-p msg) (to-string (unbox msg)) (format nil "~A" msg))))
           (write-string s *error-output*)
           (force-output *error-output*)))))))

;;; Exception condition for object-based die
;;; When Perl dies with a blessed reference, we preserve it in $@
(define-condition p-exception (error)
  ((object :initarg :object :reader p-exception-object))
  (:report (lambda (c s)
             (format s "~A" (p-exception-object c)))))

(defun %p-extract-loc (args)
  "Pull an optional (:loc \"FILE line N\") marker out of a die/warn arg list.
   Returns (values real-args loc-or-nil).  Codegen passes :loc for an explicit
   user die/warn so the Perl ' at FILE line N.' suffix carries the real source
   location; internal runtime callers pass no :loc and so keep legacy behavior.
   The marker is the keyword symbol :loc, which user die/warn args (strings,
   numbers, boxes) never are, so this is unambiguous."
  (let ((loc nil) (real '()) (skip nil))
    (dolist (a args)
      (cond (skip (setf loc a skip nil))
            ((eq a :loc) (setf skip t))
            (t (push a real))))
    (values (nreverse real) loc)))

(defun p-die (&rest raw-args)
  "Perl die - throw an exception.
   If given a single blessed reference, throw it as an exception object.
   Otherwise, concatenate args as error string.  An optional (:loc \"FILE line N\")
   marker (emitted by codegen for an explicit die) appends Perl's
   ' at FILE line N.' suffix when the message doesn't already end in a newline."
  (multiple-value-bind (args loc) (%p-extract-loc raw-args)
    (if (and (= (length args) 1)
             (let ((obj (car args)))
               ;; Perl's `die REF` preserves ANY reference (blessed or not) as
               ;; the exception object — $@/$_ in the catcher IS that reference,
               ;; with no stringification and no " at FILE line N." suffix (the
               ;; suffix is only for string dies).  So preserve: a blessed raw
               ;; hash, a scalar/glob ref box (p-box-is-ref), or a box wrapping a
               ;; reference container (hashref/arrayref/coderef/ref-to-ref).
               ;; Without this, `die { prev => $@ }` (an UNBLESSED hashref) fell
               ;; to the string branch and stringified to "HASH(0x..) at line N".
               (or (and (hash-table-p obj) (gethash :__class__ obj))
                   (and (p-box-p obj)
                        (or (p-box-class obj)
                            (p-box-is-ref obj)
                            (let ((inner (p-box-value obj)))
                              (or (hash-table-p inner)
                                  (and (vectorp inner) (not (stringp inner)))
                                  (functionp inner)
                                  (p-box-p inner)
                                  (p-typeglob-p inner))))))))
        ;; Object exception - preserve for $@
        (error 'p-exception :object (car args))
        ;; String exception
        (let ((msg (apply #'p-string-concat args)))
          (cond
            ;; No location marker.  "~A", never (error msg): the message is
            ;; DATA, and `(error msg)` would make it a format CONTROL string.
            ;; Every perl die message can carry a `~` -- the drop form (s435)
            ;; embeds arbitrary user SOURCE TEXT, so `f() = ($x =~ /b/)` fed
            ;; `~ ` to the format engine and raised an untrappable
            ;; sb-format:format-error that killed the whole file instead of
            ;; setting $@.  The other branches below were already "~A"; this
            ;; one was the last one that was not.  It is latent for the
            ;; runtime's own callers too -- several build their message from
            ;; user data (a method name, a module name) via an inner
            ;; (format nil ...), and a `~` in THAT data lands here the same way.
            ((null loc) (error "~A" msg))
            ;; Message ends in newline: Perl does NOT append a location.
            ((and (> (length msg) 0)
                  (char= (char msg (1- (length msg))) #\Newline))
             (error "~A" msg))
            ;; Empty die message: Perl uses "Died".
            ((string= msg "") (error "Died at ~A.~%" loc))
            (t (error "~A at ~A.~%" msg loc)))))))

;;; Forward declarations for p-do (both defined later in this file)
(declaim (ftype function p-eval))
(defvar @INC) ; forward declaration; value set in Module System section below

;;; p-do - Perl's do FILE (block form is inlined by codegen as (progn ...))
;;; Called only for do EXPR where EXPR is not a bare block.

(defun %p-do-io-failed ()
  "`do FILE` could not READ the file it found: perl reports that in $!, and
   leaves $@ false.  Must be called FIRST inside the handler, while the C
   errno still belongs to the failed open."
  (let ((err (sb-alien:extern-alien "errno" sb-alien:int)))
    (setf *p-stored-errno* err))
  (box-set $@ (make-p-box ""))
  *p-undef*)
(defun p-do (filename-val)
  "Perl do FILE - find file in @INC, transpile and eval it.
   Returns undef on I/O error (file not found), clears $@.
   Sets $@ to error message on compilation/execution error.
   Binds *pcl-caller-wantarray* so wantarray() in the do-file sees the calling context."
  (let* ((*pcl-caller-wantarray* *wantarray*)
         (filename (to-string (unbox filename-val)))
         ;; Search: absolute/relative path → use directly; else search @INC
         (abs-path
          (if (or (and (plusp (length filename))
                       (char= (char filename 0) #\/))
                  (and (>= (length filename) 2)
                       (char= (char filename 0) #\.)
                       (char= (char filename 1) #\/)))
              (when (probe-file filename) (truename filename))
              (loop for dir-box across @INC
                    for dir = (to-string (unbox dir-box))
                    for p = (probe-file
                             (concatenate 'string dir "/" filename))
                    when p return (truename p)))))
    (if (null abs-path)
        ;; File not found: return undef, clear $@, set $! = ENOENT
        (progn
          (box-set $@ (make-p-box ""))
          (setf *p-stored-errno* 2)
          (setf (sb-alien:extern-alien "errno" sb-alien:int) 2) ; ENOENT=2
          *p-undef*)
        ;; File found: read, transpile and eval
        (handler-case
            (let ((content
                   (handler-case
                       (with-open-file (f abs-path :direction :input)
                         (let ((s (make-string (file-length f))))
                           (read-sequence s f) s))
                     ;; I/O error opening/reading (is-a-directory, permissions,
                     ;; …): perl leaves $@ FALSE and reports the reason in $!.
                     ;; The OS errno is live at this point (probed s321: opening
                     ;; a directory leaves errno=21/EISDIR), but $! reads PCL's
                     ;; own *p-stored-errno*, so it has to be carried across —
                     ;; without that, `do '/some/dir'` reported errno 0 and
                     ;; op/do.t's "$! is EISDIR on do dir" failed.  Pass the
                     ;; real errno through rather than special-casing
                     ;; directories, so EACCES/ELOOP/… are right for free.
                     (stream-error (e)
                       (declare (ignore e))
                       (return-from p-do (%p-do-io-failed)))
                     (file-error (e)
                       (declare (ignore e))
                       (return-from p-do (%p-do-io-failed))))))
              (p-eval (make-p-box content)))
          (error (e)
            (box-set $@ (make-p-box (format nil "~A" e)))
            *p-undef*)))))


;;; Forward declaration for p-eval (p-transpile-string defined later in Module System section)
(declaim (ftype function p-transpile-string))

;;; p-eval: Perl eval(STRING) — full string eval via runtime transpilation.
;;;
;;; Variable access: eval sees defvar (dynamic) variables — package globals,
;;; our vars, local vars, and file-scope my vars. Sub-scope my vars are let-
;;; bound without defvar, so they are lexically scoped and correctly invisible
;;; to eval (matching Perl semantics). Closure-captured vars are renamed to
;;; $x__lex__N and also invisible. See docs/eval-string-plan.md.
;;;
;;; $@ format: omits " at (eval N) line M." — documented in not-supported.md.
(defun p-eval-lex-lookup (name)
  "Resolve a free variable NAME (e.g. \"$captured\") referenced inside a string
   eval to the container the eval body should bind it to:
     - the caller's in-scope lexical, if codegen passed it in *p-eval-lex-alist*;
     - otherwise the real package global (when the symbol is already bound),
       so `our`/top-level vars still read/write correctly;
     - otherwise a fresh undef container (Perl auto-vivifies the global as undef).
   The package an unqualified name resolves in is *package*, which p-eval-thunk
   binds to the eval's REGION package for a `package X; …` eval (#240 step 2)
   and otherwise leaves as the caller's — perl's rule either way."
  (let ((cell (assoc name *p-eval-lex-alist* :test #'string=)))
    (cond
      (cell (cdr cell))
      (t (let ((sym (intern (%pcl-invert-case name) *package*))
               (sigil (char name 0)))
           (if (boundp sym)
               (symbol-value sym)
               ;; Autovivify the package global (Perl semantics), INSTALLING the
               ;; fresh container so a later eval sees this eval's writes — the
               ;; enclosing file has no defvar for a name only its eval strings
               ;; use (the s304 string-innard fix removed the phantom one that
               ;; accidentally provided persistence).
               (setf (symbol-value sym)
                     (cond
                       ((char= sigil #\@) (make-array 0 :adjustable t :fill-pointer 0))
                       ((char= sigil #\%) (make-hash-table :test 'equal))
                       (t (make-p-box nil))))))))))

(defun %p-eval-region-package (designator)
  "Find-or-create the CL package for a string eval's `package X;` region.
   Same convention as the (setf %p-symref-box) arm — a package Perl code names
   into existence uses :cl and :pcl, so the runtime's symbols are visible in it.
   DESIGNATOR is the emitter's _cl_pkg_designator spelling, so this never
   re-derives the perl-name → CL-name rule."
  (or (find-package designator)
      (make-package designator :use '(:cl :pcl))))

(defun p-eval-thunk (free-names fn &optional region-pkg)
  "Apply FN (the lambda wrapping a string-eval body) to the containers for its
   free variables FREE-NAMES, looked up via p-eval-lex-lookup.  The lambda's
   parameters are those same variables, so the eval body — and any closure it
   builds — references them as ordinary lexicals.
   REGION-PKG (#240 step 2) is passed only for an eval whose body is one
   `package X; …` region: perl says the current package inside it is X, so
   *package* is bound to X's CL package around BOTH the free-name resolution
   and the body.  That is what makes an unqualified package global land in X —
   the lookup's miss path, %p-symref-box and its siblings, p-use's import
   target, p-bless's default class and the symbolic-funcall resolvers all read
   *package* for exactly this question.  The eval TEXT was already read in the
   caller's package, so the lambda's own symbols are unaffected."
  (if region-pkg
      (let ((*package* (%p-eval-region-package region-pkg)))
        (apply fn (mapcar #'p-eval-lex-lookup free-names)))
      (apply fn (mapcar #'p-eval-lex-lookup free-names))))

(defun p-eval (string &optional lex-alist features)
  "Perl eval(STRING): transpile and evaluate a Perl string at runtime.
   LEX-ALIST carries the caller's in-scope lexicals (name . container) so the
   eval body can capture them (see p-eval-thunk).
   FEATURES are the perl feature names in effect at the eval SITE (#364):
   perl's feature pragmas are lexical and a string eval inherits them, but this
   text is compiled on its own, so the site has to say.  They reach PPI's lexer
   as its initial feature state, and they are part of the cache key.
   Binds *pcl-caller-wantarray* so wantarray() in the eval'd code reflects context."
  (let ((*pcl-caller-wantarray* *wantarray*)
        (*p-eval-lex-alist* lex-alist)
        (|$^S| 1)
        (s (to-string (unbox string))))
    ;; eval undef / eval "" -> nil (undef), $@ = ""
    (when (string= s "")
      (box-set $@ "")
      (return-from p-eval nil))
    ;; Use the Perl-level current package (e.g. "Foo" inside `package Foo {}`),
    ;; not (package-name *package*): the eval'd code must transpile __PACKAGE__
    ;; and bareword qualifiers relative to the caller's PERL package, and the
    ;; preamble's (in-package ...) is derived from the same name.  *package* may
    ;; be MAIN here even when the Perl current package is Foo.
    ;; #296-B1: the emission depends on WHICH of the caller's lexicals this
    ;; eval captures, not only on the source text — an exception-partition
    ;; name ($a/$b/…) the alist carries compiles as the captured lexical, and
    ;; the same spelling with no alist entry compiles as the dynamic special
    ;; (perl's own rule: `my $a` in scope masks sort's $a).  So the capture
    ;; names are part of the compiler's INPUT and therefore part of the cache
    ;; KEY — without that, the same eval string used from two different scopes
    ;; would reuse whichever emission compiled first.  Sorted, so two sites
    ;; with the same lexicals share the entry.
    ;; #364: FEATURES join the key for the same reason.  perl's feature pragmas
    ;; are lexical and a string eval inherits them, so the same text compiles
    ;; differently under `use feature 'try'` than without it — `try {…} catch
    ;; (…) {…}` is one statement there and a swallowing bareword call here.
    (let* ((pkg-name  *pcl-current-package*)
           (cap-names (sort (mapcar #'car lex-alist) #'string<))
           (feat-names (sort (copy-list features) #'string<))
           (cache-key (list* s pkg-name (append feat-names (list :caps) cap-names)))
           (cached    (gethash cache-key *p-eval-string-cache*)))
      (handler-case
          (let* ((cl-text  (or cached
                               (let ((r (p-transpile-string s pkg-name
                                                            cap-names
                                                            feat-names)))
                                 (setf (gethash cache-key
                                                *p-eval-string-cache*) r)
                                 r)))
                 ;; READ+EVAL form-by-form (like `load`), NOT one big (progn ...).
                 ;; read-from-string would intern EVERY symbol under the initial
                 ;; *package* before any (in-package ...) form in the text runs —
                 ;; so a `package Foo;` inside the eval string would be silently
                 ;; defeated and a named `sub` would land in the caller's package.
                 ;; Reading one form, evaluating it, then reading the next lets an
                 ;; (in-package ...) take effect before subsequent forms are read,
                 ;; so `eval "package Foo; sub f {...}"` installs Foo::f.
                 ;; *package* is rebound so the (in-package ...) does not leak into
                 ;; the caller's dynamic scope (restored on exit).
                 ;; See docs/method-modifiers-plan.md.
                 ;; A `return` inside the eval'd string returns from the EVAL
                 ;; (giving the eval that value), not from the enclosing Perl sub
                 ;; — so catch :p-return here.  Without this, `_sub_attrs`'s
                 ;; `(eval 'return 1; ...') ? ':lvalue' : ''` let the `return 1`
                 ;; unwind the whole sub, which returned 1 instead of the ternary.
                 ;; Also rebind *pcl-current-package* (the Perl package name):
                 ;; a `package Foo;` inside the eval string setf's it (see
                 ;; p-defpackage), and without a fresh binding that switch leaks
                 ;; into the caller's dynamic scope — so a later `eval "bar()"`
                 ;; would resolve bar() in Foo instead of the caller's package.
                 ;; Mirrors *package* above and the module-load rebind in p-require.
                 (result   (catch :p-return
                             (let ((*package* *package*)
                                   (*pcl-current-package* *pcl-current-package*)
                                   (eof '#:eof))
                               (with-input-from-string (in cl-text)
                                 (loop with r = nil
                                       for form = (read in nil eof)
                                       until (eq form eof)
                                       do (setf r (eval form))
                                       finally (return r)))))))
            (box-set $@ "")
            result)
        (p-exception (e)
          (box-set $@ (p-exception-object e))
          nil)
        (error (e)
          ;; Perl appends " at (eval N) line M." to die/runtime-error messages
          ;; thrown inside string eval when they don't already end in a newline.
          ;; PCL doesn't track the in-eval line, so it uses line 1 (correct for
          ;; the common single-line eval string).
          (let ((msg (format nil "~A" e)))
            (box-set $@ (if (and (> (length msg) 0)
                                 (char= (char msg (1- (length msg))) #\Newline))
                            msg
                            (format nil "~A at (eval ~D) line 1.~%"
                                    msg (incf *p-eval-counter*)))))
          nil)))))

(defun parse-number (s)
  "Try to parse string as number, return nil if not a number."
  (handler-case
      (let ((val (read-from-string s)))
        (if (numberp val) val nil))
    (error () nil)))

(defun %p-caught-perl-value (e)
  "The Perl VALUE of a caught condition E: the exception object for an object
   `die`, else the message text — with perl's \" at FILE line N.\" tail when the
   message does not already end in a newline (p-die adds the real location; this
   is the fallback for a CL error that never went through it).

   ONE definition, shared by the two places a Perl program can see a caught
   error: `eval {}` (p-eval-block, which puts it in $@) and `try/catch`
   (p-try, which binds it to the catch variable)."
  (if (typep e 'p-exception)
      (p-exception-object e)
      (let ((msg (format nil "~A" e)))
        (if (and (> (length msg) 0)
                 (char= (char msg (1- (length msg))) #\Newline))
            msg
            (format nil "~A at (eval 0) line 0.~%" msg)))))

;;; p-eval-block: Execute code catching errors (Perl's eval { })
;;; Sets $@ to error message on failure, empty string on success.
;;; Returns nil on error, block result on success.
(defmacro p-eval-block (&body body)
  "Perl eval { } - execute body catching errors.
   Sets $@ to error/exception on failure, empty string on success.
   Returns result of body on success, nil on failure."
  ;; `return` inside eval { } exits the eval block (perldoc -f return), not the
  ;; enclosing sub — so catch :p-return here, letting the eval evaluate to the
  ;; returned value rather than unwinding the whole sub.
  `(handler-case
       (prog1 (let ((|$^S| 1)) (catch :p-return ,@body))
         (box-set $@ ""))
     (error (e)
       (box-set $@ (%p-caught-perl-value e))
       nil)))

(defmacro p-try (try-form catch-clause &optional finally-form)
  "Perl's `try BLOCK catch (VAR) BLOCK [finally BLOCK]` (feature 'try', 5.34).
   CATCH-CLAUSE is (VAR CATCH-FORM); FINALLY-FORM is optional.

   Every rule below was probed against perl 5.40.3, and each one is why this is
   NOT p-eval-block with a different name:

   * `$@` is LOCALIZED to the construct: it reads \"\" inside try and inside
     catch, and holds its pre-try value again by the time finally runs and
     afterwards.  The caught error reaches the program ONLY through VAR.
   * `return` / `last` / `next` / `redo` inside try belong to the ENCLOSING sub
     or loop — eval {} catches :p-return, try must not — so nothing here
     catches them.  finally still runs on that path: it is an unwind-protect
     cleanup, not a form after the body.
   * the construct's VALUE is the executed block's last value, in whatever
     context the caller supplied (`do { try { 123 } catch ($e) { 456 } }`);
     finally's value is discarded.  *wantarray* is untouched, so `wantarray`
     inside try answers for the enclosing sub, and no new frame means caller()
     does not see the block either.
   * catch runs on ANY exception, including a FALSE one (`die 0`, or an object
     that overloads bool to 0) — the test is that an exception was thrown,
     never the truth of a value."
  (destructuring-bind (var catch-form) catch-clause
    (let ((saved (gensym "AT")) (caught (gensym "CAUGHT")) (err (gensym "ERR"))
          (val (gensym "VAL")))
      `(let ((,saved (p-box-value $@))
             (,caught nil)
             (,err nil))
         (unwind-protect
              (let ((,val (handler-case
                              (progn (box-set $@ "")
                                     (let ((|$^S| 1)) ,try-form))
                            (error (e)
                              (setf ,caught t
                                    ,err (%p-caught-perl-value e))
                              nil))))
                (if ,caught
                    (let ((,var (make-p-box nil)))
                      (box-set ,var ,err)
                      (box-set $@ "")
                      ,catch-form)
                    ,val))
           ;; $@ is restored BEFORE finally runs — probed: a finally block sees
           ;; the pre-try value, not the error it just handled.
           (box-set $@ ,saved)
           ,@(when finally-form (list finally-form)))))))

;;; ============================================================
;;; File I/O Functions
;;; ============================================================

;; Filehandle storage - maps symbols to CL streams
(defvar *p-filehandles* (make-hash-table :test 'eq))

;; The stream most recently read by readline/<FH>.  Perl's argument-less `eof`
;; (and `eof` inside a `while (<FH>)` loop) tests "the last file read", not
;; STDIN — so readline records the handle here and %p-eof-impl falls back to it.
(defvar *p-last-read-handle* nil)

;; Standard filehandles
(setf (gethash 'STDIN *p-filehandles*) *standard-input*)
(setf (gethash 'STDOUT *p-filehandles*) *standard-output*)
(setf (gethash 'STDERR *p-filehandles*) *error-output*)

;;; --- Socket filehandle plumbing -------------------------------------------
;;; A Perl socket IS a filehandle: after connect/accept you print $sock / <$sock>
;;; / close $sock.  But bind/connect/listen/accept operate on the sb-bsd-sockets
;;; SOCKET OBJECT, while print/readline need a STREAM.  We store the socket object
;;; as the filehandle value (via %p-install-fh, same as open stores a stream), and
;;; lazily wrap it in a cached bidirectional stream the first time it is used for
;;; I/O.  The stream is cached per socket because re-making it would lose buffered
;;; data.
(defvar *p-socket-streams* (make-hash-table :test 'eq)
  "Cache mapping an sb-bsd-sockets:socket object to its lazily-made stream.")

(defun %p-socket-p (x)
  "True if X is an sb-bsd-sockets socket object."
  (typep x 'sb-bsd-sockets:socket))

(defun %p-socket-stream (sock)
  "Lazily make (and cache) a bidirectional character stream over socket SOCK.
   :buffering :none so writes hit the wire immediately (line protocols)."
  (or (gethash sock *p-socket-streams*)
      (setf (gethash sock *p-socket-streams*)
            (sb-bsd-sockets:socket-make-stream
             sock :input t :output t :buffering :none :element-type 'character))))

(defun %p-as-stream (v)
  "Coerce a resolved filehandle value to a CL stream: pass a stream through,
   lazily wrap a socket object in its cached stream, else nil."
  (cond ((streamp v) v)
        ((%p-socket-p v) (%p-socket-stream v))
        (t nil)))

(defun %p-resolve-fh (fh)
  "Resolve a Perl filehandle designator to its STORED value — a CL stream or an
   sb-bsd-sockets socket object — or nil.  Does NOT coerce sockets to streams (so
   the socket builtins can get the object); p-get-stream does the coercion."
  (cond
    ((streamp fh) fh)
    ((%p-socket-p fh) fh)
    ((symbolp fh)
     (or (gethash fh *p-filehandles*)
         ;; The standard handles STDIN/STDOUT/STDERR are registered under the
         ;; :pcl symbols, but generated code in a user package passes that
         ;; package's own same-named symbol (these names are not exported, so
         ;; they are distinct symbols) — an `eq` miss.  Perl filehandles are
         ;; by-name, so fall back to a by-name lookup; this is what makes
         ;; `print STDERR ...` actually reach *error-output* instead of stdout.
         (let ((canon (find-symbol (symbol-name fh) :pcl)))
           (and canon (not (eq canon fh)) (gethash canon *p-filehandles*)))))
    ((stringp fh)
     ;; A string filehandle name — e.g. print {"STDOUT"} ..., or a scalar
     ;; holding a handle name (my $fh = 'STDOUT'; print $fh ...).  Strip an
     ;; optional package qualifier, then resolve by name.  Barewords reach
     ;; *p-filehandles* under their :invert-readtable-cased symbol (STDOUT →
     ;; |stdout|), so invert the string name the same way before find-symbol.
     (let* ((sep (search "::" fh :from-end t))
            (name (if sep (subseq fh (+ sep 2)) fh))
            (inv  (%pcl-invert-case name))
            (sym  (find-symbol inv :pcl)))
       (or (and sym (gethash sym *p-filehandles*))
           ;; User handles (pipe READ, WRITE; open FOO, …) are keyed by their
           ;; USER-package symbol, which a name string can't find-symbol
           ;; without knowing the caller's package.  Perl handles are by-name:
           ;; fall back to a name scan of the (small) handle table (#70
           ;; dup-open ">&WRITE" resolves its source this way).
           (loop for k being the hash-keys of *p-filehandles*
                 using (hash-value v)
                 when (and (symbolp k) (string= (symbol-name k) inv))
                 return v))))
    ;; A typeglob is a filehandle designator (fileno(*STDOUT)): resolve by
    ;; the glob's name.  p-make-typeglob stores the name ALREADY
    ;; %pcl-invert-case'd (symbol case, "stdin"), so look up directly — the
    ;; string branch would invert a second time.
    ((p-typeglob-p fh)
     (let* ((name (p-typeglob-name fh))
            (sym  (find-symbol name :pcl)))
       (or (and sym (gethash sym *p-filehandles*))
           (loop for k being the hash-keys of *p-filehandles*
                 using (hash-value v)
                 when (and (symbolp k) (string= (symbol-name k) name))
                 return v))))
    ((p-box-p fh)
     (let ((v (p-box-value fh)))
       (cond
         ((streamp v) v)
         ((%p-socket-p v) v)
         ;; Scalar holding a handle NAME ('STDOUT', 'FOO'): resolve by name.
         ((stringp v) (%p-resolve-fh v))
         ;; Ref-to-glob (\*STDOUT): unwrap to the glob designator.
         ((p-typeglob-p v) (%p-resolve-fh v))
         (t nil))))
    (t nil)))

(defun p-get-stream (fh)
  "Get CL stream from Perl filehandle (symbol, box, stream, or socket object).
   A socket handle is lazily wrapped in a cached bidirectional stream so that
   print/readline/read/eof/close all work through the normal stream paths."
  (%p-as-stream (%p-resolve-fh fh)))

(defun p-defined-fh (fh-sym)
  "Check if a bareword filehandle or dirhandle (symbol) is open — codegen's
   defined(FILEHANDLE).  It resolves through p-get-stream, THE filehandle
   resolver, and sits here so it can: its own (gethash fh-sym *p-filehandles*)
   MISSED the standard handles, which are registered under the :pcl symbols
   while generated code in a user package passes that package's own same-named
   symbol, so `defined STDIN` answered false where perl says true (s414).
   Going through p-get-stream also keeps a socket handle from reaching
   open-stream-p as a raw socket object.  (*p-dirhandles* is defined later in
   this file; that forward reference is a compile-time warning only.)"
  (or (let ((stream (p-get-stream fh-sym)))
        (and stream (open-stream-p stream) t))
      (and (ignore-errors (gethash fh-sym *p-dirhandles*)) t)))

(defun %p-get-socket (fh)
  "Resolve a Perl filehandle to its underlying sb-bsd-sockets socket object (for
   bind/connect/listen/accept/…), or nil if it is not a socket handle."
  (let ((v (%p-resolve-fh fh)))
    (and (%p-socket-p v) v)))

(defun %p-open-parse-2arg (expr)
  "Parse a 2-arg open expression into (mode . filename).
   E.g. '>file.txt' -> ('>' . 'file.txt'), 'file.txt' -> ('<' . 'file.txt')"
  (let ((s (to-string expr)))
    (cond
      ((and (>= (length s) 2) (string= (subseq s 0 2) ">>"))
       (cons ">>" (string-left-trim " " (subseq s 2))))
      ((and (>= (length s) 2) (string= (subseq s 0 2) "+<"))
       (cons "+<" (string-left-trim " " (subseq s 2))))
      ((and (>= (length s) 2) (string= (subseq s 0 2) "+>"))
       (cons "+>" (string-left-trim " " (subseq s 2))))
      ((and (>= (length s) 2) (string= (subseq s 0 2) "|-"))
       (cons "|-" (string-left-trim " " (subseq s 2))))
      ((and (>= (length s) 2) (string= (subseq s 0 2) "-|"))
       (cons "-|" (string-left-trim " " (subseq s 2))))
      ;; Classic 2-arg command pipes: "| cmd" writes to cmd's stdin,
      ;; "cmd |" reads cmd's stdout (#70).
      ((and (>= (length s) 1) (char= (char s 0) #\|))
       (cons "|-" (string-left-trim " " (subseq s 1))))
      ((and (>= (length s) 1)
            (char= (char s (1- (length s))) #\|))
       (cons "-|" (string-right-trim " " (subseq s 0 (1- (length s))))))
      ;; Dup-opens: ">&FH" / "<&FH" duplicate FH's file descriptor; the
      ;; "=" forms (">&=FH" / ">&=N") are fdopen-style — same fd, no dup (#70).
      ((and (>= (length s) 3) (string= (subseq s 0 3) ">&="))
       (cons ">&=" (string-left-trim " " (subseq s 3))))
      ((and (>= (length s) 3) (string= (subseq s 0 3) "<&="))
       (cons "<&=" (string-left-trim " " (subseq s 3))))
      ((and (>= (length s) 2) (string= (subseq s 0 2) ">&"))
       (cons ">&" (string-left-trim " " (subseq s 2))))
      ((and (>= (length s) 2) (string= (subseq s 0 2) "<&"))
       (cons "<&" (string-left-trim " " (subseq s 2))))
      ((and (>= (length s) 1) (char= (char s 0) #\>))
       (cons ">" (string-left-trim " " (subseq s 1))))
      ((and (>= (length s) 1) (char= (char s 0) #\<))
       (cons "<" (string-left-trim " " (subseq s 1))))
      (t
       (cons "<" s)))))

;;; In-memory string filehandles: open my $fh, ">", \$s
;;; A p-string-output-stream is an SBCL Gray output stream that appends written
;;; characters directly into the target scalar box's adjustable string, so the
;;; scalar reflects the output live (matching Perl's PerlIO ":scalar" layer).
;; Shared state for in-memory string filehandles: a target scalar box and a
;; current byte offset.  Perl's PerlIO ":scalar" layer tracks a position, so a
;; seek/tell or a re-assignment of the scalar mid-write does not lose the
;; place; writes overwrite existing chars and extend past the end; a forward
;; seek beyond the end zero-fills with NUL.
(defclass p-string-stream-mixin ()
  ((target :initarg :target :reader psos-target)
   (pos    :initarg :pos :initform 0 :accessor psos-pos)))

;; Write-only handle: open my $fh, ">",  \$s   /   ">>", \$s
(defclass p-string-output-stream
    (p-string-stream-mixin sb-gray:fundamental-character-output-stream)
  ())

;; Read+write handle: open my $fh, "+<", \$s   /   "+>", \$s
(defclass p-string-io-stream
    (p-string-stream-mixin
     sb-gray:fundamental-character-input-stream
     sb-gray:fundamental-character-output-stream)
  ())

(defun %psos-buf (s)
  "Return the target box's value as an adjustable fill-pointer string.  The
user may have reassigned the scalar with a plain (non-adjustable) string while
the handle is open; rebuild it from the current contents when that happens so
writes never fault on a simple-string."
  (let* ((box (psos-target s))
         (v   (p-box-value box)))
    (if (and (stringp v) (adjustable-array-p v) (array-has-fill-pointer-p v))
        v
        (let ((buf (%p-fresh-adjustable-string
                    (if (or (null v) (eq v *p-undef*)) "" (to-string v)))))
          (setf (p-box-value box) buf
                (p-box-sv-ok box) nil (p-box-nv-ok box) nil)
          buf))))

(defun %psos-put (s ch)
  "Write CH at the stream's current position: overwrite if within the string,
zero-fill any gap from a forward seek, otherwise extend at the end."
  (let ((buf (%psos-buf s))
        (p   (psos-pos s)))
    (loop while (< (fill-pointer buf) p) do (vector-push-extend #\Nul buf))
    (if (< p (fill-pointer buf))
        (setf (char buf p) ch)
        (vector-push-extend ch buf))
    (setf (psos-pos s) (1+ p)
          (p-box-sv-ok (psos-target s)) nil
          (p-box-nv-ok (psos-target s)) nil)
    ch))

(defmethod sb-gray:stream-write-char ((s p-string-stream-mixin) ch)
  (%psos-put s ch))

(defmethod sb-gray:stream-write-string
    ((s p-string-stream-mixin) string &optional (start 0) end)
  (loop for i from start below (or end (length string))
        do (%psos-put s (char string i)))
  string)

(defmethod sb-gray:stream-line-column ((s p-string-stream-mixin)) nil)

;; tell()/seek() on an in-memory handle go through file-position.
(defmethod sb-gray:stream-file-position
    ((s p-string-stream-mixin) &optional position)
  (cond ((null position) (psos-pos s))
        (t (setf (psos-pos s)
                 (case position
                   (:start 0)
                   (:end   (fill-pointer (%psos-buf s)))
                   (t      position)))
           t)))

;; --- read side of a bidirectional in-memory handle ("+<" / "+>") ----------
(defmethod sb-gray:stream-read-char ((s p-string-io-stream))
  (let ((buf (%psos-buf s))
        (p   (psos-pos s)))
    (if (< p (fill-pointer buf))
        (progn (setf (psos-pos s) (1+ p)) (char buf p))
        :eof)))

(defmethod sb-gray:stream-unread-char ((s p-string-io-stream) ch)
  (declare (ignore ch))
  (when (> (psos-pos s) 0) (decf (psos-pos s)))
  nil)

(defvar *p-open-output-streams* (make-hash-table :test 'eq :weakness :key)
  "Every OUTPUT stream that open() has bound to a Perl filehandle, weakly held
   so an abandoned handle is still collectable.  Perl flushes (closes, in fact)
   every handle at program exit and before fork/exec; PCL flushed only the three
   standard streams, so `open my $fh,'>',$f; print {$fh} …;` with no explicit
   close silently LOST the buffer — and so did a dup-opened handle
   (`open $d,'>&=STDOUT'`), which is how Capture::Tiny tees.")

(defun %p-flush-open-streams ()
  "finish-output every still-open registered handle.  Called from the exit hook
   (after the END blocks, which may still print) and before fork/exec."
  (maphash (lambda (s v)
             (declare (ignore v))
             (ignore-errors (when (open-stream-p s) (finish-output s))))
           *p-open-output-streams*))

(defun %p-register-open-stream (stream)
  "Record STREAM for the exit/fork flush if it can be written to."
  (when (and (streamp stream) (output-stream-p stream))
    (setf (gethash stream *p-open-output-streams*) t))
  stream)

(defun %p-install-fh (fh stream)
  "Bind STREAM to the filehandle FH (a box, or a bareword symbol).
   Symbolic filehandle: when FH is a box already holding a non-empty handle-NAME
   string (e.g. $TST = \"TST\"; open($TST, ...)), Perl opens the named glob (*TST)
   and leaves $fh holding the string — it does NOT autovivify a lexical handle.
   Register under the by-name :pcl symbol so BOTH the bareword form (<TST>/eof(TST))
   and the scalar form (<$TST>) resolve it via %p-resolve-fh."
  (%p-register-open-stream stream)
  (cond ;; The box holds a GLOB (Symbol::gensym, \*FH, IO::Handle->new — often
    ;; blessed).  Perl attaches the stream to the glob's IO slot and leaves
    ;; the scalar itself alone; overwriting the box with the stream
    ;; DESTROYED the object, so ref($io) stopped being IO::Handle and every
    ;; later method call on it went elsewhere (task #197).  Key it by the
    ;; glob's own name, which is how %p-resolve-fh reaches a glob handle.
    ((and (p-box-p fh) (p-typeglob-p (p-box-value fh)))
     (let ((glob (p-box-value fh)))
       (setf (gethash (intern (p-typeglob-name glob)
                              (p-typeglob-package glob))
                      *p-filehandles*)
             stream)))
    ((and (p-box-p fh)
          (stringp (p-box-value fh))
          (plusp (length (p-box-value fh))))
     (let* ((nm  (p-box-value fh))
            (sep (search "::" nm :from-end t))
            (name (if sep (subseq nm (+ sep 2)) nm)))
       (setf (gethash (intern (%pcl-invert-case name) :pcl) *p-filehandles*)
             stream)))
    ((p-box-p fh) (box-set fh stream))
    (t            (setf (gethash fh *p-filehandles*) stream))))

(defun %p-fresh-adjustable-string (&optional (init ""))
  (let ((buf (make-array (length init) :element-type 'character
                         :adjustable t :fill-pointer 0)))
    (loop for c across init do (vector-push-extend c buf))
    buf))

(defun %p-split-open-mode (mode-str)
  "Split a Perl open mode into its BASE mode and its PerlIO layer suffix:
   `<:utf8` -> \"<\" + \":utf8\";  `>>:encoding(UTF-8)` -> \">>\" + the rest.

   Perl accepts layers on EVERY mode, and an open that merely carries one must
   SUCCEED.  Both dispatchers used to compare the whole mode string with
   `string=`, so any layer fell through to the `Unknown open mode` arm and the
   open FAILED — breaking ordinary code like
   `open $fh, '<:encoding(UTF-8)', $file` (task #171).

   Second value is the external format to read/write the FILE with: `:raw` and
   `:bytes` mean no translation, and latin-1 is the CL spelling of that (each
   octet maps to one character).  Decoding raw bytes as UTF-8 would corrupt or
   signal, so this is not cosmetic.  Everything else keeps PCL's default.
   NOTE: this ACCEPTS layers and honours the encoding ones; it is not a PerlIO
   layer model — :crlf, layer stacking and PerlIO::get_layers introspection are
   task #139, which needs the design call."
  (let* ((colon (position #\: mode-str))
         (base  (if colon (subseq mode-str 0 colon) mode-str))
         (layers (if colon (subseq mode-str colon) "")))
    (values base
            layers
            (if (or (search ":raw" layers) (search ":bytes" layers))
                :latin-1
                :default))))

(defun %p-open-memory (fh mode target-box)
  "Open an in-memory string filehandle over TARGET-BOX (the scalar behind \\$s)."
  ;; Layers are stripped for dispatch: the scalar already holds CL characters,
  ;; so an encoding layer has nothing to translate here (task #171).
  (let ((mode-str (%p-split-open-mode (to-string mode)))
        (cur (let ((v (p-box-value target-box)))
               (if (or (null v) (eq v *p-undef*)) "" (to-string v)))))
    (cond
      ;; Write/truncate: replace the scalar with a fresh adjustable string the
      ;; Gray stream grows in place.  Bypass box-set's sv-cache by invalidating.
      ((string= mode-str ">")
       (setf (p-box-value target-box) (%p-fresh-adjustable-string)
             (p-box-sv-ok target-box) nil (p-box-nv-ok target-box) nil)
       (%p-install-fh fh (make-instance 'p-string-output-stream :target target-box))
       t)
      ;; Read+write, truncate: like ">", but the handle can also be read back.
      ((string= mode-str "+>")
       (setf (p-box-value target-box) (%p-fresh-adjustable-string)
             (p-box-sv-ok target-box) nil (p-box-nv-ok target-box) nil)
       (%p-install-fh fh (make-instance 'p-string-io-stream :target target-box))
       t)
      ;; Append: seed the adjustable string with the current contents and
      ;; position the write offset at the end.
      ((string= mode-str ">>")
       (setf (p-box-value target-box) (%p-fresh-adjustable-string cur)
             (p-box-sv-ok target-box) nil (p-box-nv-ok target-box) nil)
       (%p-install-fh fh (make-instance 'p-string-output-stream
                                        :target target-box
                                        :pos (length cur)))
       t)
      ;; Read+write, keep contents: position at start, reads see the current
      ;; contents, writes overwrite/extend in place (Perl's "+<" on a scalar).
      ((string= mode-str "+<")
       (setf (p-box-value target-box) (%p-fresh-adjustable-string cur)
             (p-box-sv-ok target-box) nil (p-box-nv-ok target-box) nil)
       (%p-install-fh fh (make-instance 'p-string-io-stream :target target-box))
       t)
      ;; Read: the bidirectional stream positioned at the start.  (Using the
      ;; same class as "+<" gives uniform seek/tell/SEEK_END; PCL does not
      ;; enforce read-only-ness on the handle, matching its general stance.)
      ((string= mode-str "<")
       (%p-install-fh fh (make-instance 'p-string-io-stream
                                        :target (let ((b (make-p-box
                                                          (%p-fresh-adjustable-string cur))))
                                                  b)))
       t)
      (t (warn "Unsupported in-memory open mode: ~A" mode-str) nil))))

;;; --- Fork-pipe opens: open FH, "|-" / "-|" [, CMD] (#70) -------------------
;; Parent-side pipe stream → child PID.  p-close consults this to waitpid the
;; child and set $? (Perl: close on a pipe returns true only on exit status 0).
(defvar *p-pipe-pids* (make-hash-table :test 'eq))

(defun %p-open-fork-pipe (fh mode-str cmd-args)
  "Perl fork-pipe open.  MODE-STR is \"|-\" (parent writes to the child's
   STDIN) or \"-|\" (parent reads the child's STDOUT).  CMD-ARGS non-nil execs
   the command in the child (never returns); nil is the BARE form — both
   processes continue running the program, the parent gets the child PID and
   the child gets 0 with the filehandle NOT installed (Perl semantics).
   The child's standard stream is rewired onto the pipe via dup2 so both
   in-process reads/writes and a later exec see it."
  (handler-case
      (multiple-value-bind (read-fd write-fd) (sb-posix:pipe)
        (finish-output *standard-output*)
        (finish-output *error-output*)
        (let ((pid (sb-posix:fork)))
          (cond
            ((> pid 0)                    ; ---- parent
             (let ((stream
                    (if (string= mode-str "|-")
                        (progn
                          (sb-posix:close read-fd)
                          (sb-sys:make-fd-stream write-fd :output t
                                                 :buffering :full
                                                 :external-format :utf-8))
                        (progn
                          (sb-posix:close write-fd)
                          (sb-sys:make-fd-stream read-fd :input t
                                                 :external-format :utf-8)))))
               (setf (gethash stream *p-pipe-pids*) pid)
               (%p-install-fh fh stream)
               pid))
            (t                            ; ---- child
             (box-set $$ (sb-posix:getpid))
             (if (string= mode-str "|-")
                 (progn                   ; child READS its rewired STDIN
                   (sb-posix:close write-fd)
                   (unless (= read-fd 0)
                     (sb-posix:dup2 read-fd 0)
                     (sb-posix:close read-fd))
                   (setf sb-sys:*stdin*
                         (sb-sys:make-fd-stream 0 :input t
                                                :external-format :utf-8))
                   (setf *standard-input* (make-synonym-stream 'sb-sys:*stdin*))
                   (setf (gethash 'STDIN *p-filehandles*) *standard-input*))
                 (progn                   ; child WRITES its rewired STDOUT
                   (sb-posix:close read-fd)
                   (unless (= write-fd 1)
                     (sb-posix:dup2 write-fd 1)
                     (sb-posix:close write-fd))
                   ;; :line buffering — the parent typically consumes the
                   ;; child's output (TAP lines) incrementally.
                   (setf sb-sys:*stdout*
                         (sb-sys:make-fd-stream 1 :output t :buffering :line
                                                :external-format :utf-8))
                   (setf *standard-output*
                         (make-synonym-stream 'sb-sys:*stdout*))
                   (setf (gethash 'STDOUT *p-filehandles*) *standard-output*)))
             (when cmd-args
               (apply #'p-exec cmd-args)  ; returns only on exec failure
               (sb-ext:exit :code 127 :abort t))
             0))))
    (error () (%pcl-save-errno) *p-undef*)))

(defun %p-close-maybe-pipe (v)
  "Close stream V; if it is the parent end of a fork-pipe, reap the child and
   set $?.  Returns Perl's close truth: for a pipe, true only when the child
   exited 0; for a plain stream, true."
  (let ((pid (gethash v *p-pipe-pids*)))
    (close v)
    (if (null pid)
        t
        (progn
          (remhash v *p-pipe-pids*)
          (handler-case
              (multiple-value-bind (rpid status) (sb-posix:waitpid pid 0)
                (declare (ignore rpid))
                (setf $? status)
                (if (zerop status) t nil))
            (error () (%pcl-save-errno) nil))))))

(defun %p-fd-of-stream (v)
  "Underlying file descriptor of stream V, following synonym streams; nil when
   V has no OS fd (string streams etc.)."
  (let ((s v))
    (loop while (typep s 'synonym-stream)
          do (setf s (symbol-value (synonym-stream-symbol s))))
    (when (sb-sys:fd-stream-p s)
      (sb-sys:fd-stream-fd s))))

(defun %p-open-dup (fh mode-str src-name)
  "Dup-open (#70): open FH, \">&SRC\" / \"<&SRC\" — FH becomes a duplicate of
   SRC's file descriptor.  The \"=\" forms (\">&=SRC\", \">&=N\") are
   fdopen-style: same fd (or a stream alias), no dup.  SRC may be a handle name
   or a raw fd number.  When FH is a standard handle (STDOUT/STDERR/STDIN)
   the dup goes ONTO its well-known fd via dup2 and the CL stream is rebuilt,
   so in-process prints AND exec'd children both see the redirect (the
   closure.t child shape).  Any other FH gets a fresh dup'd fd as a new stream."
  (let* ((eq-form (char= (char mode-str (1- (length mode-str))) #\=))
         (fd-num  (and (plusp (length src-name))
                       (every #'digit-char-p src-name)
                       (parse-integer src-name)))
         (src     (and (not fd-num) (%p-resolve-fh src-name)))
         (src-fd  (or fd-num (and src (%p-fd-of-stream src)))))
    (unless src-fd
      ;; No OS fd behind SRC.  An in-memory (PerlIO :scalar-alike) handle has
      ;; none; Perl can still dup it at the PerlIO layer — the closest PCL
      ;; equivalent is sharing the stream object itself (exact for the "="
      ;; alias form, an approximation for ">&" which would get its own
      ;; position in Perl).  scalar.t [perl #113764] \">&=FILE\" lands here.
      (when (streamp src)
        (%p-install-fh fh src)
        (return-from %p-open-dup t))
      (setf *p-stored-errno* 9)          ; EBADF
      (return-from %p-open-dup nil))
    ;; A raw fd NUMBER must be OPEN.  perl's dup/fdopen fails EBADF on a closed
    ;; descriptor (probed: `open($t,"<&=37")` and `"<&37"` and `">&=37"` all
    ;; fail with "Bad file descriptor"), but SBCL's make-fd-stream does not
    ;; check — it hands back a stream whose first read retries EBADF forever,
    ;; i.e. a SPIN, not an error (task #358).  That is also why t/run/cloexec.t
    ;; hung under a PCL child (task #346): the whole point of that file is to
    ;; have the child open an fd it was NOT given.
    (when (and fd-num
               (handler-case (progn (sb-posix:fcntl fd-num sb-posix:f-getfd) nil)
                 (error () t)))
      (setf *p-stored-errno* 9)          ; EBADF
      (return-from %p-open-dup nil))
    (let ((std (and (symbolp fh)
                    ;; string-equal: the generated code passes the handle
                    ;; symbol under :invert readtable case (|stdout|).
                    (cond ((string-equal (symbol-name fh) "STDOUT") 1)
                          ((string-equal (symbol-name fh) "STDERR") 2)
                          ((string-equal (symbol-name fh) "STDIN")  0)))))
      (handler-case
          (if std
              (progn
                (case std
                  (1 (finish-output *standard-output*))
                  (2 (finish-output *error-output*)))
                (sb-posix:dup2 src-fd std)
                (case std
                  (0 (setf sb-sys:*stdin*
                           (sb-sys:make-fd-stream 0 :input t
                                                  :external-format :utf-8))
                     (setf *standard-input*
                           (make-synonym-stream 'sb-sys:*stdin*))
                     (setf (gethash 'STDIN *p-filehandles*) *standard-input*))
                  (1 (setf sb-sys:*stdout*
                           (sb-sys:make-fd-stream 1 :output t :buffering :line
                                                  :external-format :utf-8))
                     (setf *standard-output*
                           (make-synonym-stream 'sb-sys:*stdout*))
                     (setf (gethash 'STDOUT *p-filehandles*) *standard-output*))
                  (2 (setf sb-sys:*stderr*
                           (sb-sys:make-fd-stream 2 :output t :buffering :line
                                                  :external-format :utf-8))
                     (setf *error-output*
                           (make-synonym-stream 'sb-sys:*stderr*))
                     (setf (gethash 'STDERR *p-filehandles*) *error-output*)))
                t)
              ;; ">&=SRC" is fdopen, NOT dup: the new handle IS the same fd.  When
              ;; SRC is a handle we already have a stream for, share that stream
              ;; object rather than opening a second buffer on the same fd —
              ;; otherwise two buffers race and interleaved writes come out
              ;; reordered (perl keeps program order here).  A raw fd number
              ;; ("<&=3") has no stream to share and still gets a fresh one.
              (if (and eq-form (streamp src))
                  (progn (%p-install-fh fh src) t)
                  (let* ((new-fd  (if eq-form src-fd (sb-posix:dup src-fd)))
                         (stream  (if (char= (char mode-str 0) #\>)
                                      (sb-sys:make-fd-stream new-fd :output t
                                                             :external-format :utf-8)
                                      (sb-sys:make-fd-stream new-fd :input t
                                                             :external-format :utf-8))))
                    (%p-install-fh fh stream)
                    t)))
        (error () (%pcl-save-errno) nil)))))

(defun %p-open-impl (fh mode filename)
  "Implementation of Perl open"
  ;; In-memory filehandle: the target is a SCALAR ref (a box whose value is a box),
  ;; e.g. open my $fh, '>', \$s.  Dispatch before the filename is stringified.
  (when (and (p-box-p filename) (p-box-p (p-box-value filename)))
    (return-from %p-open-impl
      (%p-open-memory fh mode (p-box-value filename))))
  (let* ((mode-ef  (multiple-value-list (%p-split-open-mode (to-string mode))))
         ;; Dispatch on the BASE mode; a :layer suffix must not make the open
         ;; fail (task #171).  EF honours :raw/:bytes as byte-exact.
         (mode-str (first mode-ef))
         (ef       (third mode-ef))
         (file-str (to-string filename))
         (stream
          (cond
            ;; The magic filename "-" means a standard stream (Perl dups it):
            ;; "<-" / "<","-" → STDIN; ">-" / ">","-" → STDOUT.
            ((and (string= file-str "-") (string= mode-str "<"))
             *standard-input*)
            ((and (string= file-str "-")
                  (member mode-str '(">" ">>") :test #'string=))
             *standard-output*)
            ((string= mode-str "<")
             (open file-str :direction :input :if-does-not-exist nil
                   :external-format ef))
            ((string= mode-str ">")
             (open file-str :direction :output :if-exists :supersede
                   :if-does-not-exist :create :external-format ef))
            ((string= mode-str ">>")
             (open file-str :direction :output :if-exists :append
                   :if-does-not-exist :create :external-format ef))
            ((string= mode-str "+<")
             (open file-str :direction :io :if-exists :overwrite
                   :if-does-not-exist nil :external-format ef))
            ((string= mode-str "+>")
             (open file-str :direction :io :if-exists :supersede
                   :if-does-not-exist :create :external-format ef))
            ((or (string= mode-str "|-") (string= mode-str "-|"))
             ;; Fork-pipe open (#70): bare when no command text, else the
             ;; child execs the command.  Returns pid/0/undef directly —
             ;; the filehandle install happens inside (parent only).
             (return-from %p-open-impl
               (%p-open-fork-pipe fh mode-str
                                  (when (plusp (length file-str))
                                    (list file-str)))))
            ((member mode-str '(">&" "<&" ">&=" "<&=") :test #'string=)
             ;; Dup-open (#70): install/redirect handled inside.
             (return-from %p-open-impl (%p-open-dup fh mode-str file-str)))
            (t
             (warn "Unknown open mode: ~A" mode-str)
             nil))))
    (if stream
        ;; Install under the box/bareword/symbolic-name rules — %p-install-fh
        ;; also handles the symbolic-filehandle case (box already holding a
        ;; handle-NAME string, e.g. $TST = "TST"; open($TST, ...)): Perl opens
        ;; the named glob (*TST) and leaves $fh holding the string rather than
        ;; autovivifying a lexical handle.  (An undef/empty box is the modern
        ;; `open my $fh, ...` autoviv.)
        (%p-install-fh fh stream)
        (%pcl-save-errno))  ; capture C errno (ENOENT etc.) before SBCL overwrites it
    (if stream t nil)))

(defmacro p-open (fh mode &optional filename)
  "Perl open - open file with given mode.
   2-arg: (p-open FH expr) - mode is parsed from expr
   3-arg: (p-open FH mode filename)
   Bareword FH is quoted; lexical $fh is passed as evaluated box."
  (if filename
      `(%p-open-impl (%p-fh-arg ,fh) ,mode ,filename)
      `(let ((%parsed (%p-open-parse-2arg ,mode)))
         (%p-open-impl (%p-fh-arg ,fh) (car %parsed) (cdr %parsed)))))

(defun %p-close-socket (sock)
  "Close a socket: close its cached stream (which closes the fd) if one was made,
   else socket-close the object directly.  Drop it from the stream cache."
  (let ((stream (gethash sock *p-socket-streams*)))
    (if stream
        (progn (ignore-errors (close stream)) (remhash sock *p-socket-streams*))
        (ignore-errors (sb-bsd-sockets:socket-close sock))))
  t)

(defun %p-forget-fh (fh)
  "Undo %p-install-fh: drop FH's binding to its stream.  Mirrors that function's
   cases exactly, so every handle shape it can open, this can close."
  (cond ((and (p-box-p fh) (p-typeglob-p (p-box-value fh)))
         (let ((glob (p-box-value fh)))
           (remhash (intern (p-typeglob-name glob) (p-typeglob-package glob))
                    *p-filehandles*)))
        ;; Symbolic handle: the box holds a NAME string ($fh = 'FOO').  The
        ;; install side registered by name and left the box alone; close must
        ;; mirror that — drop the by-name entry, keep the string ($fh still
        ;; reads "FOO" after close in perl).  Box-clearing here wiped the name
        ;; (s327 review probe).
        ((and (p-box-p fh)
              (stringp (p-box-value fh))
              (plusp (length (p-box-value fh))))
         (let* ((nm  (p-box-value fh))
                (sep (search "::" nm :from-end t))
                (name (if sep (subseq nm (+ sep 2)) nm)))
           (remhash (intern (%pcl-invert-case name) :pcl) *p-filehandles*)))
        ((p-box-p fh) (box-set fh *p-undef*))
        ((symbolp fh) (remhash fh *p-filehandles*))))

(defun %p-close-impl (fh)
  "Implementation of Perl close (file or socket handle).
   Resolves through %p-resolve-fh — the ONE resolver that knows every handle
   shape.  Reading the box directly instead missed a handle whose box holds a
   GLOB (IO::Handle->new / \\*FH), so close() quietly did nothing and the
   buffered output was lost."
  (let ((v (%p-resolve-fh fh)))
    (cond
      ((%p-socket-p v) (%p-close-socket v) (%p-forget-fh fh) t)
      ((streamp v)     (prog1 (%p-close-maybe-pipe v)
                         (%p-forget-fh fh)))
      (t nil))))

(defmacro p-close (&optional fh)
  "Perl close - close filehandle. Bareword is quoted; lexical $fh passed as box.
   With no argument, Perl closes the currently-selected default output handle.
   PCL does not track a selected handle (p-select is a stub), and actually
   closing STDOUT/STDERR would break the program's own output, so the no-arg form
   is a success no-op returning 1 (Perl's true)."
  (if fh
      `(%p-close-impl (%p-fh-arg ,fh))
      1))

(defun %p-eof-impl (&optional fh)
  "Perl eof implementation — fh must already be a symbol or stream.
   Argument-less `eof` tests the last filehandle read (Perl semantics), so it
   falls back to *p-last-read-handle* (then STDIN) rather than STDIN directly."
  (let ((stream (if fh (p-get-stream fh) (or *p-last-read-handle* *standard-input*))))
    ;; eof FH makes FH the current handle for $. (Perl sets PL_last_in_gv).
    (when (and fh stream) (setf *p-last-read-handle* stream))
    ;; A closed stream reads as EOF in Perl (eof on a closed handle is true).
    ;; *p-last-read-handle* may still point at a stream that was since closed
    ;; (`close TRY; ...; eof()`), so guard peek-char against a closed stream to
    ;; avoid an sb-int:closed-stream-error abort.
    (if (and stream (open-stream-p stream))
        (let ((ch (peek-char nil stream nil :eof)))
          (if (eq ch :eof) t nil))
        t)))

(defmacro p-eof (&rest args)
  "Perl eof - check end of file. Bareword filehandle is auto-quoted."
  (if args
      `(%p-eof-impl (%p-fh-arg ,(car args)))
      `(%p-eof-impl)))

;; Helper used by filehandle macros: if FH-FORM is a plain symbol (no sigil)
;; it is a bareword filehandle — quote it.  Otherwise pass through as-is.
;; Also handles (pl-NAME) forms where codegen wrapped the bareword in a funcall.
(defmacro %p-fh-arg (fh-form)
  ;; A CONTEXT WRAP around the argument is peeled first (%p-strip-ctx): a
  ;; scalar-context user-sub call is emitted wrapped, and the (pl-NAME) arm
  ;; below is exactly what a bareword filehandle looks like inside one.  That
  ;; used to be a SECOND copy of the arm, keyed on the `let` spelling only —
  ;; which the #281 context macros would have silently defeated (probed: the
  ;; bareword then CALLS pl-NAME, an undefined function).
  (let ((fh-form (%p-strip-ctx fh-form)))
    (cond
      ;; Bare symbol without sigil — bareword filehandle: quote it
      ((and (symbolp fh-form)
            (let ((name (symbol-name fh-form)))
              (and (plusp (length name))
                   (not (member (char name 0) '(#\$ #\@ #\% #\*))))))
       `',(intern (symbol-name fh-form)))
      ;; (pl-NAME) pattern: codegen wrapped a bareword FH in a user-sub call.
      ;; Extract the bare name and quote it instead of calling the nonexistent function.
      ((and (listp fh-form)
            (= (length fh-form) 1)
            (symbolp (car fh-form))
            (let ((name (symbol-name (car fh-form))))
              (and (> (length name) 3)
                   (string-equal (subseq name 0 3) "PL-"))))
       ;; Recover the bareword FH name and intern the SAME symbol the direct
       ;; bareword path produces.  A direct bareword `X` becomes
       ;; (intern (%pcl-invert-case "X")) — the reader applies :invert.  Here we
       ;; first un-invert the read symbol-name and strip the pl- prefix to get the
       ;; original Perl name, then invert it again to match.  (Skipping the final
       ;; invert mis-cased `eof(TST)`/`<TST>` derived FHs: "TST" vs the readline's
       ;; "tst" — symbolic-FH open(\$TST="TST") then bareword use.)
       `',(intern (%pcl-invert-case
                   (subseq (%pcl-invert-case (symbol-name (car fh-form))) 3))))
      ;; Everything else: evaluate as-is (e.g. $fh variable or complex expression)
      (t fh-form))))

(defun %p-tell-impl (&optional fh)
  "Perl tell - return current file position"
  (let ((stream (if fh (p-get-stream fh) *standard-input*)))
    ;; tell FH makes FH the current handle for $. (Perl sets PL_last_in_gv).
    (when (and fh stream) (setf *p-last-read-handle* stream))
    (if stream (file-position stream) -1)))

(defmacro p-tell (&rest args)
  "Perl tell — bareword filehandle is auto-quoted."
  (if args `(%p-tell-impl (%p-fh-arg ,(car args))) `(%p-tell-impl)))

(defun %p-seek-impl (fh pos whence)
  "Perl seek - seek to position. Whence: 0=start, 1=current, 2=end"
  (let ((stream (p-get-stream fh))
        (position (to-number pos))
        (w (to-number whence)))
    ;; seek FH makes FH the current handle for $. (Perl sets PL_last_in_gv).
    (when stream (setf *p-last-read-handle* stream))
    (when stream
      (let ((new-pos
             (cond
               ((= w 0) position)                              ; SEEK_SET
               ((= w 1) (+ (file-position stream) position))   ; SEEK_CUR
               ((= w 2) (+ (%p-stream-length stream) position)); SEEK_END
               (t position))))
        ;; A negative resulting offset is an error in Perl: seek() returns
        ;; false and leaves the position unchanged (rather than faulting).
        (if (and (integerp new-pos) (minusp new-pos))
            nil
            (and (file-position stream new-pos) 1))))))

(defun %p-stream-length (stream)
  "Total length of STREAM for SEEK_END.  In-memory output streams expose their
buffer's fill-pointer; everything else falls back to file-length."
  (if (typep stream 'p-string-stream-mixin)
      (fill-pointer (%psos-buf stream))
      (file-length stream)))

(defmacro p-seek (fh &rest args)
  "Perl seek — bareword filehandle is auto-quoted."
  `(%p-seek-impl (%p-fh-arg ,fh) ,@args))

(defun %p-sysseek-impl (fh pos whence)
  "Perl sysseek FH, POS, WHENCE — position the handle, bypassing buffering.
   Returns the NEW position; a new position of 0 returns \"0 but true\"
   (true in boolean, 0 in numeric context); a negative target returns undef."
  (let ((stream (p-get-stream fh))
        (position (to-number pos))
        (w (to-number whence)))
    (when stream (setf *p-last-read-handle* stream))
    (if (not stream)
        *p-undef*
        (let ((new-pos
               (cond
                 ((= w 0) position)                              ; SEEK_SET
                 ((= w 1) (+ (file-position stream) position))   ; SEEK_CUR
                 ((= w 2) (+ (%p-stream-length stream) position)); SEEK_END
                 (t position))))
          (if (or (not (integerp new-pos)) (minusp new-pos)
                  (not (file-position stream new-pos)))
              *p-undef*
              (if (zerop new-pos) "0 but true" new-pos))))))

(defmacro p-sysseek (fh &rest args)
  "Perl sysseek — bareword filehandle is auto-quoted."
  `(%p-sysseek-impl (%p-fh-arg ,fh) ,@args))

(defun %p-binmode-impl (fh &optional encoding)
  "Perl binmode - set binary mode or encoding.
   PCL builds on SBCL streams (which handle encoding natively) and does not
   model PerlIO layers, so for an already-open handle this is a no-op returning
   true.  But binmode on a filehandle that is NOT open fails in Perl with errno
   EBADF (bad file descriptor); replicate that — set $! and return false — so
   error-checking code (io/binmode.t test 9) observes the right $!."
  (declare (ignore encoding))
  (if (p-get-stream fh)
      t
      (progn
        (setf *p-stored-errno* 9)                                  ; EBADF (Linux)
        (setf (sb-alien:extern-alien "errno" sb-alien:int) 9)
        nil)))

(defmacro p-binmode (fh &rest args)
  "Perl binmode — bareword filehandle is auto-quoted."
  `(%p-binmode-impl (%p-fh-arg ,fh) ,@args))

(defun %p-read-impl (fh buf len &optional offset)
  "Perl read(FH, BUF, LEN [, OFFSET]) — read up to LEN chars from FH into the
   lvalue BUF, returning the number of chars actually read (0 at EOF, undef on
   error).  BUF is modified in place.  With OFFSET, the read data is placed at
   that position in BUF: a positive offset keeps (NUL-padding to) the first
   OFFSET chars of BUF's old value; a negative offset counts back from the end
   of BUF's current length.  An unopened handle fails with errno EBADF."
  (let ((stream (p-get-stream fh)))
    (unless stream
      (setf *p-stored-errno* 9)                                 ; EBADF (Linux)
      (setf (sb-alien:extern-alien "errno" sb-alien:int) 9)
      (return-from %p-read-impl *p-undef*)))
  (let ((n (truncate (to-number len)))
        (off (and offset (truncate (to-number offset))))
        (old (if (p-box-p buf) (to-string (unbox buf)) "")))
    (when (< n 0) (p-die "Negative length"))
    (when (and off (< off 0))
      (when (< (+ (length old) off) 0) (p-die "Offset outside string"))
      (setf off (+ (length old) off)))
    (handler-case
        (let* ((stream (p-get-stream fh))
               (tmp (make-string n))
               (got (read-sequence tmp stream))
               (data (subseq tmp 0 got)))
          (when (p-box-p buf)
            (if off
                (let ((head (if (<= off (length old))
                                (subseq old 0 off)
                                (concatenate 'string old
                                             (make-string (- off (length old))
                                                          :initial-element #\Nul)))))
                  (box-set buf (concatenate 'string head data)))
                (box-set buf data)))
          got)
      (p-exception (e) (error e))
      (error () *p-undef*))))

(defmacro p-read (fh &rest args)
  "Perl read — bareword filehandle is auto-quoted."
  `(%p-read-impl (%p-fh-arg ,fh) ,@args))

(defun %p-sysread-impl (fh buf len &optional offset)
  "Perl sysread - low-level read (same as read for now).  IO errors return
   undef (caught inside %p-read-impl); perl-semantic errors (negative length,
   offset outside string) die out of it."
  (%p-read-impl fh buf len offset))

(defmacro p-sysread (fh &rest args)
  "Perl sysread — bareword filehandle is auto-quoted."
  `(%p-sysread-impl (%p-fh-arg ,fh) ,@args))

(defun %p-syswrite-impl (fh data &optional len offset)
  "Perl syswrite FH, SCALAR [, LEN [, OFFSET]] - write data to filehandle.
   Unbuffered (flushes immediately) so a readline on the other end of a pipe
   sees the data.  Writes LEN chars of SCALAR starting at OFFSET (negative
   offset counts from the end).  Returns nil on stream/encode error; perl-
   semantic errors (negative length, offset outside string) die."
  (let ((str (to-string data))
        (n (and len (truncate (to-number len))))
        (off (if offset (truncate (to-number offset)) 0)))
    (when (and n (< n 0)) (p-die "Negative length"))
    (when (< off 0)
      (when (< (+ (length str) off) 0) (p-die "Offset outside string"))
      (setf off (+ (length str) off)))
    (when (> off (length str)) (p-die "Offset outside string"))
    (handler-case
        (let ((stream (p-get-stream fh)))
          (when stream
            (let ((out (if n
                           (subseq str off (min (+ off n) (length str)))
                           str)))
              (write-string out stream)
              (finish-output stream)
              (length out))))
      (error () nil))))

(defmacro p-syswrite (fh &rest args)
  "Perl syswrite — bareword filehandle is auto-quoted."
  `(%p-syswrite-impl (%p-fh-arg ,fh) ,@args))

(defun %p-truncate-impl (fh-or-file size)
  "Truncate a file (named or open) to SIZE bytes. Returns 1 on success, '' on
   failure.  A filehandle is truncated via ftruncate(fd) after flushing buffered
   output; a path string via truncate(2).  A BAREWORD filehandle (symbol) that is
   not open FAILS — it must NOT fall back to truncating a file named after it."
  (let* ((len (%pcl-to-integer (to-number size)))
         (v (if (p-box-p fh-or-file) (p-box-value fh-or-file) fh-or-file))
         (stream (cond ((streamp v) v)
                       ((or (symbolp v) (stringp v)) (p-get-stream v))
                       (t nil))))
    (handler-case
        (cond
          (stream (finish-output stream)
                  (sb-posix:ftruncate (sb-sys:fd-stream-fd stream) len)
                  1)
          ;; Bareword FH that is not open → fail (no file-name fallback).
          ((symbolp v) (%pcl-save-errno) "")
          (t (sb-posix:truncate (to-string v) len) 1))
      (error () (%pcl-save-errno) ""))))

(defmacro p-truncate (fh-or-file size)
  "Perl truncate FH/EXPR, LENGTH — the first operand is filehandle-like, so a
   bareword must be quoted (otherwise it is an unbound variable reference)."
  `(%p-truncate-impl (%p-fh-arg ,fh-or-file) ,size))

;;; ============================================================
;;; Socket builtins (AF_INET / AF_UNIX, SOCK_STREAM / SOCK_DGRAM via
;;; sb-bsd-sockets).  The filehandle plumbing (%p-resolve-fh / %p-get-socket /
;;; socket→stream caching) lives up by p-get-stream.  See docs/socket-impl-plan.md.
;;; The address-packing helpers (inet_aton/sockaddr_in/…) are pure Perl in
;;; lib/Socket.pm; the runtime only handles ALREADY-PACKED sockaddr byte-strings,
;;; matching Perl's core calling convention.
;;; ============================================================

(defun %p-pack-sockaddr-in (addr port)
  "Re-pack an (addr-vector . port) into a 16-byte struct sockaddr_in byte string
   so Perl's unpack_sockaddr_in / Socket.pm can read it back: family AF_INET=2
   (native little-endian on x86-64), port in network order, 4 address bytes, 8 NUL
   pad."
  (let ((s (make-string 16 :initial-element #\Nul))
        (p (logand (truncate port) #xffff)))
    (setf (char s 0) (code-char 2)                               ; AF_INET lo (LE)
          (char s 1) (code-char 0)                               ; AF_INET hi
          (char s 2) (code-char (logand (ash p -8) #xff))        ; port hi (network)
          (char s 3) (code-char (logand p #xff))                 ; port lo (network)
          (char s 4) (code-char (logand (aref addr 0) #xff))
          (char s 5) (code-char (logand (aref addr 1) #xff))
          (char s 6) (code-char (logand (aref addr 2) #xff))
          (char s 7) (code-char (logand (aref addr 3) #xff)))
    s))

(defun %p-parse-sockaddr (name)
  "Parse a packed sockaddr byte-string from Perl (Socket.pm's pack_sockaddr_in /
   pack_sockaddr_un).  Returns (values :inet addr-vector port) for sockaddr_in, or
   (values :unix path nil) for sockaddr_un.  Family is bytes 0-1 native order
   (AF_INET=2, AF_UNIX=1); port is network order; addr is 4 bytes."
  (let* ((s (to-string name))
         (len (length s))
         (fam (if (>= len 2)
                  (logior (char-code (char s 0)) (ash (char-code (char s 1)) 8))
                  2)))
    (cond
      ((= fam 1)                                          ; AF_UNIX: family + path
       (let ((nul (or (position #\Nul s :start 2) len)))
         (values :unix (subseq s 2 nul) nil)))
      (t                                                  ; AF_INET (default)
       (let ((port (if (>= len 4)
                       (logior (ash (char-code (char s 2)) 8) (char-code (char s 3)))
                       0))
             (addr (if (>= len 8)
                       (vector (char-code (char s 4)) (char-code (char s 5))
                               (char-code (char s 6)) (char-code (char s 7)))
                       (vector 0 0 0 0))))
         (values :inet addr port))))))

(defun %p-socket-impl (fh domain type protocol)
  "Perl socket(SOCK, DOMAIN, TYPE, PROTOCOL): create an unconnected socket object
   and install it as the filehandle SOCK.  1 on success, '' (and sets $!) on
   failure."
  (let ((dom   (truncate (to-number domain)))            ; AF_INET=2 AF_UNIX=1
        (typ   (truncate (to-number type)))              ; SOCK_STREAM=1 SOCK_DGRAM=2
        (proto (truncate (to-number protocol))))
    (handler-case
        (let* ((stype (if (= typ 2) :datagram :stream))
               (sock (if (= dom 1)
                         (make-instance 'sb-bsd-sockets:local-socket :type stype)
                         (make-instance 'sb-bsd-sockets:inet-socket :type stype
                                        :protocol (cond ((= proto 17) :udp)
                                                        ((= proto 6) :tcp)
                                                        ((= typ 2) :udp)
                                                        (t :tcp))))))
          (%p-install-fh fh sock)
          1)
      (error () (%pcl-save-errno) ""))))

(defmacro p-socket (fh domain type protocol)
  "Perl socket — bareword filehandle is auto-quoted."
  `(%p-socket-impl (%p-fh-arg ,fh) ,domain ,type ,protocol))

(defun %p-bind-impl (fh name)
  "Perl bind(SOCK, NAME): NAME is a packed sockaddr.  1 on success, '' + $!."
  (let ((sock (%p-get-socket fh)))
    (unless sock (return-from %p-bind-impl ""))
    (handler-case
        (multiple-value-bind (kind a b) (%p-parse-sockaddr name)
          (if (eq kind :unix)
              (sb-bsd-sockets:socket-bind sock a)
              (sb-bsd-sockets:socket-bind sock a b))
          1)
      (error () (%pcl-save-errno) ""))))

(defmacro p-bind (fh name)
  "Perl bind — bareword filehandle is auto-quoted."
  `(%p-bind-impl (%p-fh-arg ,fh) ,name))

(defun %p-connect-impl (fh name)
  "Perl connect(SOCK, NAME): NAME is a packed sockaddr.  1 on success, '' + $!."
  (let ((sock (%p-get-socket fh)))
    (unless sock (return-from %p-connect-impl ""))
    (handler-case
        (multiple-value-bind (kind a b) (%p-parse-sockaddr name)
          (if (eq kind :unix)
              (sb-bsd-sockets:socket-connect sock a)
              (sb-bsd-sockets:socket-connect sock a b))
          1)
      (error () (%pcl-save-errno) ""))))

(defmacro p-connect (fh name)
  "Perl connect — bareword filehandle is auto-quoted."
  `(%p-connect-impl (%p-fh-arg ,fh) ,name))

(defun %p-listen-impl (fh queue)
  "Perl listen(SOCK, QUEUESIZE).  1 on success, '' + $!."
  (let ((sock (%p-get-socket fh)))
    (unless sock (return-from %p-listen-impl ""))
    (handler-case
        (progn (sb-bsd-sockets:socket-listen sock (max 0 (truncate (to-number queue)))) 1)
      (error () (%pcl-save-errno) ""))))

(defmacro p-listen (fh queue)
  "Perl listen — bareword filehandle is auto-quoted."
  `(%p-listen-impl (%p-fh-arg ,fh) ,queue))

(defun %p-accept-impl (newfh serverfh)
  "Perl accept(NEWSOCK, GENERICSOCK): block, accept a connection on the server
   socket, install the new socket as NEWSOCK, return the packed peer sockaddr
   (a true value).  '' (and sets $!) on failure."
  (let ((server (%p-get-socket serverfh)))
    (unless server (return-from %p-accept-impl ""))
    (handler-case
        (let ((new (sb-bsd-sockets:socket-accept server)))
          (%p-install-fh newfh new)
          (multiple-value-bind (addr port) (sb-bsd-sockets:socket-peername new)
            (%p-pack-sockaddr-in addr port)))
      (error () (%pcl-save-errno) ""))))

(defmacro p-accept (newfh serverfh)
  "Perl accept — both filehandles are auto-quoted (the new socket is written
   through NEWSOCK, like read writes through its buffer)."
  `(%p-accept-impl (%p-fh-arg ,newfh) (%p-fh-arg ,serverfh)))

(defun %p-shutdown-impl (fh how)
  "Perl shutdown(SOCK, HOW): HOW 0=read 1=write 2=both.  1 on success, '' + $!."
  (let ((sock (%p-get-socket fh)))
    (unless sock (return-from %p-shutdown-impl ""))
    (handler-case
        (let ((dir (case (truncate (to-number how))
                     (0 :input) (1 :output) (t :io))))
          (sb-bsd-sockets:socket-shutdown sock :direction dir)
          1)
      (error () (%pcl-save-errno) ""))))

(defmacro p-shutdown (fh how)
  "Perl shutdown — bareword filehandle is auto-quoted."
  `(%p-shutdown-impl (%p-fh-arg ,fh) ,how))

(defun %p-send-impl (fh msg flags &optional to)
  "Perl send(SOCK, MSG, FLAGS [, TO]): send MSG; with TO (packed sockaddr) for a
   datagram socket.  Returns the number of bytes sent, or '' + $! on failure."
  (declare (ignore flags))
  (let ((sock (%p-get-socket fh)))
    (unless sock (return-from %p-send-impl ""))
    (handler-case
        (let ((data (to-string msg)))
          (if to
              (multiple-value-bind (kind a b) (%p-parse-sockaddr to)
                (declare (ignore kind))
                (sb-bsd-sockets:socket-send sock data nil :address (list a b)))
              (sb-bsd-sockets:socket-send sock data nil)))
      (error () (%pcl-save-errno) ""))))

(defmacro p-send (fh msg flags &optional to)
  "Perl send — bareword filehandle is auto-quoted."
  (if to
      `(%p-send-impl (%p-fh-arg ,fh) ,msg ,flags ,to)
      `(%p-send-impl (%p-fh-arg ,fh) ,msg ,flags)))

(defun %p-recv-impl (fh buf len flags)
  "Perl recv(SOCK, SCALAR, LEN, FLAGS): receive up to LEN bytes into SCALAR;
   return the sender's packed address (for a datagram) or '' for a connected
   socket.  '' + $! on failure."
  (declare (ignore flags))
  (let ((sock (%p-get-socket fh)))
    (unless sock (return-from %p-recv-impl ""))
    (handler-case
        (multiple-value-bind (data nbytes addr port)
            (sb-bsd-sockets:socket-receive sock nil (max 0 (truncate (to-number len)))
                                           :element-type 'character)
          ;; socket-receive returns the whole allocated buffer plus the actual
          ;; byte count — truncate to what was read (else the unfilled tail leaks).
          (let ((s (if (stringp data) data (coerce data 'string))))
            (when (and (integerp nbytes) (<= 0 nbytes (length s)))
              (setf s (subseq s 0 nbytes)))
            (when (p-box-p buf) (box-set buf s)))
          (if (and addr (not (every #'zerop addr)))
              (%p-pack-sockaddr-in addr port)
              ""))
      (error () (%pcl-save-errno) ""))))

(defmacro p-recv (fh buf len flags)
  "Perl recv — bareword filehandle is auto-quoted; the buffer is written through."
  `(%p-recv-impl (%p-fh-arg ,fh) ,buf ,len ,flags))

(defun %p-getsockname-impl (fh)
  "Perl getsockname(SOCK): packed local address, or '' + $!."
  (let ((sock (%p-get-socket fh)))
    (unless sock (return-from %p-getsockname-impl ""))
    (handler-case
        (multiple-value-bind (addr port) (sb-bsd-sockets:socket-name sock)
          (%p-pack-sockaddr-in addr port))
      (error () (%pcl-save-errno) ""))))

(defmacro p-getsockname (fh)
  "Perl getsockname — bareword filehandle is auto-quoted."
  `(%p-getsockname-impl (%p-fh-arg ,fh)))

(defun %p-getpeername-impl (fh)
  "Perl getpeername(SOCK): packed peer address, or '' + $!."
  (let ((sock (%p-get-socket fh)))
    (unless sock (return-from %p-getpeername-impl ""))
    (handler-case
        (multiple-value-bind (addr port) (sb-bsd-sockets:socket-peername sock)
          (%p-pack-sockaddr-in addr port))
      (error () (%pcl-save-errno) ""))))

(defmacro p-getpeername (fh)
  "Perl getpeername — bareword filehandle is auto-quoted."
  `(%p-getpeername-impl (%p-fh-arg ,fh)))

;;; --- getprotobyname / getprotobynumber -------------------------------------
;;; perl answers both from /etc/protocols, so PCL reads the same file (task
;;; #222, ruled docs/fable-answers-s337.md §4-secondary).  The old four-entry
;;; static table made "a protocol PCL never heard of" and "a protocol this host
;;; does not have" the same undef; reading the real file removes the ambiguity
;;; instead of announcing it.  Read once, lazily, at the first call.

(defparameter +p-protocols-fallback+
  '(("ip" ("IP") 0) ("icmp" ("ICMP") 1) ("tcp" ("TCP") 6) ("udp" ("UDP") 17))
  "Entries used ONLY when /etc/protocols is unreadable (non-Linux, chroot).
   Each is (NAME ALIAS-LIST NUMBER), the same shape %p-protocols-parse-line
   produces.")

(defvar *p-protocols-by-name* nil
  "Protocol NAME or ALIAS -> (NAME ALIAS-LIST NUMBER).  NIL until first use.")

(defvar *p-protocols-by-number* nil
  "Protocol NUMBER -> (NAME ALIAS-LIST NUMBER).  NIL until first use.")

(defun %p-protocols-parse-line (line)
  "One /etc/protocols line -> (NAME ALIAS-LIST NUMBER), or NIL for a blank,
   comment or malformed line.  Fields are NAME NUMBER [ALIAS...]; a '#' starts
   a comment anywhere on the line."
  (flet ((blankp (c) (or (char= c #\Space) (char= c #\Tab))))
    (let* ((hash (position #\# line))
           (body (if hash (subseq line 0 hash) line))
           (len (length body))
           (fields '())
           (i 0))
      (loop
       (let ((start (position-if-not #'blankp body :start i)))
         (unless start (return))
         (let ((end (or (position-if #'blankp body :start start) len)))
           (push (subseq body start end) fields)
           (setf i end))))
      (setf fields (nreverse fields))
      (when (>= (length fields) 2)
        (let ((num (parse-integer (second fields) :junk-allowed t)))
          (when num
            (list (first fields) (cddr fields) num)))))))

(defun %p-load-protocols ()
  "Fill both protocol tables from /etc/protocols, or from the fallback list
   when that file is missing or unreadable."
  (let ((by-name (make-hash-table :test 'equal))
        (by-number (make-hash-table :test 'eql))
        (entries '()))
    (handler-case
        (with-open-file (in "/etc/protocols" :direction :input
                            :if-does-not-exist nil)
          (when in
            (loop for line = (read-line in nil nil)
                  while line
                  do (let ((e (%p-protocols-parse-line line)))
                       (when e (push e entries))))))
      (error () (setf entries '())))
    (setf entries (or (nreverse entries) +p-protocols-fallback+))
    (dolist (e entries)
      ;; perl matches the name or ANY alias, exactly — getprotobyname("TCP")
      ;; hits tcp's alias, getprotobyname("Tcp") misses.  For a number the
      ;; FIRST line wins: number 0 reports "ip", not the later "hopopt".
      (destructuring-bind (name aliases number) e
        (unless (gethash name by-name) (setf (gethash name by-name) e))
        (dolist (a aliases)
          (unless (gethash a by-name) (setf (gethash a by-name) e)))
        (unless (gethash number by-number) (setf (gethash number by-number) e))))
    (setf *p-protocols-by-name* by-name
          *p-protocols-by-number* by-number)))

(defun %p-protocol-result (entry scalar-slot)
  "perl's return shape: (NAME, ALIASES, NUMBER) in list context; in SCALAR
   context \"you get the name, unless the lookup WAS by name, in which case you
   get the other thing\" (perlfunc) — so SCALAR-SLOT is :number for
   getprotobyname and :name for getprotobynumber.  A miss is the empty list in
   list context, undef in scalar context."
  (if (eq *wantarray* t)
      (if entry
          (make-array 3 :initial-contents
                      (list (first entry)
                            (format nil "~{~A~^ ~}" (second entry))
                            (third entry))
                      :adjustable t :fill-pointer t)
          (make-array 0 :adjustable t :fill-pointer t))
      (if entry
          (ecase scalar-slot
            (:number (third entry))
            (:name (first entry)))
          *p-undef*)))

(defun p-getprotobyname (name)
  "Perl getprotobyname(NAME) — looked up in /etc/protocols by name or alias.
   perl matches exactly: \"TCP\" hits tcp's alias, \"Tcp\" is a miss."
  (unless *p-protocols-by-name* (%p-load-protocols))
  (%p-protocol-result (gethash (to-string name) *p-protocols-by-name*)
                      :number))

(defun p-getprotobynumber (number)
  "Perl getprotobynumber(NUM) — the first /etc/protocols line carrying NUM."
  (unless *p-protocols-by-number* (%p-load-protocols))
  (%p-protocol-result
   (gethash (truncate (to-number number)) *p-protocols-by-number*)
   :name))

(defun %p-setsockopt-impl (fh level optname optval)
  "Perl setsockopt(SOCK, LEVEL, OPTNAME, OPTVAL).  Only SO_REUSEADDR (the one real
   server code uses) is wired; others succeed as no-ops.  1 on success, '' + $!."
  (declare (ignore level))
  (let ((sock (%p-get-socket fh)))
    (unless sock (return-from %p-setsockopt-impl ""))
    (handler-case
        (progn
          (when (= (truncate (to-number optname)) 2)        ; SO_REUSEADDR
            (setf (sb-bsd-sockets:sockopt-reuse-address sock)
                  (/= 0 (truncate (to-number optval)))))
          1)
      (error () (%pcl-save-errno) ""))))

(defmacro p-setsockopt (fh level optname optval)
  "Perl setsockopt — bareword filehandle is auto-quoted."
  `(%p-setsockopt-impl (%p-fh-arg ,fh) ,level ,optname ,optval))

(defun %p-getsockopt-impl (fh level optname)
  "Perl getsockopt(SOCK, LEVEL, OPTNAME): return the option value packed as a
   native int.  Only SO_REUSEADDR is wired; others read 0."
  (declare (ignore level))
  (let ((sock (%p-get-socket fh)))
    (unless sock (return-from %p-getsockopt-impl ""))
    (handler-case
        (let ((v (if (= (truncate (to-number optname)) 2)
                     (if (sb-bsd-sockets:sockopt-reuse-address sock) 1 0)
                     0)))
          ;; pack 'i' — native 4-byte int, little-endian on x86-64.
          (let ((s (make-string 4 :initial-element #\Nul)))
            (setf (char s 0) (code-char (logand v #xff))
                  (char s 1) (code-char (logand (ash v -8) #xff))
                  (char s 2) (code-char (logand (ash v -16) #xff))
                  (char s 3) (code-char (logand (ash v -24) #xff)))
            s))
      (error () (%pcl-save-errno) ""))))

(defmacro p-getsockopt (fh level optname)
  "Perl getsockopt — bareword filehandle is auto-quoted."
  `(%p-getsockopt-impl (%p-fh-arg ,fh) ,level ,optname))

(defun %p-socketpair-impl (fh1 fh2 domain type protocol)
  "Perl socketpair(S1, S2, DOMAIN, TYPE, PROTOCOL).  sb-bsd-sockets has no direct
   socketpair; emulate an AF_UNIX/AF_INET stream pair over a bound loopback
   listener so S1<->S2 are connected.  1 on success, '' + $!."
  (declare (ignore domain type protocol))
  (handler-case
      (let ((srv (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
        (setf (sb-bsd-sockets:sockopt-reuse-address srv) t)
        (sb-bsd-sockets:socket-bind srv #(127 0 0 1) 0)
        (sb-bsd-sockets:socket-listen srv 1)
        (multiple-value-bind (host port) (sb-bsd-sockets:socket-name srv)
          (declare (ignore host))
          (let ((a (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
            (sb-bsd-sockets:socket-connect a #(127 0 0 1) port)
            (let ((b (sb-bsd-sockets:socket-accept srv)))
              (ignore-errors (sb-bsd-sockets:socket-close srv))
              (%p-install-fh fh1 a)
              (%p-install-fh fh2 b)
              1))))
    (error () (%pcl-save-errno) "")))

(defmacro p-socketpair (fh1 fh2 domain type protocol)
  "Perl socketpair — both filehandles are auto-quoted."
  `(%p-socketpair-impl (%p-fh-arg ,fh1) (%p-fh-arg ,fh2) ,domain ,type ,protocol))

(defun %p-stat-vector (st)
  "Build Perl's 13-element stat list from an sb-posix stat struct.  Times are
   Unix-epoch seconds (time_t), matching Perl (NOT CL's 1900-epoch universal
   time)."
  (vector (sb-posix:stat-dev st)
          (sb-posix:stat-ino st)
          (sb-posix:stat-mode st)
          (sb-posix:stat-nlink st)
          (sb-posix:stat-uid st)
          (sb-posix:stat-gid st)
          (sb-posix:stat-rdev st)
          (sb-posix:stat-size st)
          (sb-posix:stat-atime st)
          (sb-posix:stat-mtime st)
          (sb-posix:stat-ctime st)
          ;; sb-posix exposes no stat-blksize/stat-blocks accessor in the
          ;; supported SBCL range, so derive sensible values: a 512-byte block
          ;; count and the conventional 4096 preferred I/O block size.
          4096
          (ceiling (sb-posix:stat-size st) 512)))

;;; `_` — perl's stat-cache filehandle.  `-e $f and -f _ and -r _` is an
;;; everyday idiom meaning "reuse the buffer from the last stat, do not stat
;;; again"; it saves two stat(2) calls.  The emitter lowers the bareword to a
;;; bare CL symbol, so `_` must be a BOUND variable that every generated
;;; package sees — hence a defvar in :pcl, exported.  Nothing else claims the
;;; bare symbol: Perl subs emit as `pl-*` and Perl scalars keep their `$`.
;;;
;;; DELIBERATE DIVERGENCE from perl: perl caches the stat BUFFER and answers
;;; from it with no syscall; PCL caches the OPERAND (path or stream) and stats
;;; again.  Same answer outside a race, and it keeps each test's own logic —
;;; access(2) for -r/-w/-x, lstat for -l, the first-block scan for -T/-B —
;;; instead of reimplementing perl's buffer-vs-syscall table.
(defvar _ '%pcl-stat-cache
  "Perl's stat-cache filehandle `_`.  Its value is a marker: a filetest or
   stat receiving it re-uses the previous operand instead of its own.")

(defvar *pcl-stat-cache-path* nil
  "Operand of the most recent stat/lstat/filetest — a path string or a stream.
   nil before the first one, which makes a premature `_` test false.")

(defun %p-stat-arg (file-or-fh)
  "Resolve a stat/lstat argument to either a CL stream (filehandle) or a path
   string.  Accepts a stream, a box holding a stream, a bareword/symbol or
   string naming a filehandle, or a plain path string.  Also maintains the `_`
   stat cache: the marker reads the remembered operand, anything else becomes
   the remembered operand."
  (let ((v (if (p-box-p file-or-fh) (p-box-value file-or-fh) file-or-fh)))
    (when (eq v '%pcl-stat-cache)
      (setf v *pcl-stat-cache-path*))
    (setf *pcl-stat-cache-path* v)
    (cond
      ((streamp v) v)
      ((null v) "")                         ; `_` before any stat: no operand
      ((or (symbolp v) (stringp v))
       (let ((s (p-get-stream v)))          ; FH name → stream, else path
         (cond (s (setf *pcl-stat-cache-path* s) s)
               (t (to-string v)))))
      (t (to-string v)))))

(defun %p--path (file)
  "Resolve a FILETEST operand to a path string, through the one stat-argument
   funnel (so `_` and filehandle names work the same as for stat).  A stream
   resolves through /dev/fd/N, which lets `-s $fh` stat the open handle."
  (let ((arg (%p-stat-arg file)))
    (if (streamp arg)
        (handler-case (format nil "/dev/fd/~D" (sb-sys:fd-stream-fd arg))
          (error () ""))
        arg)))

(defun p-stat (file-or-fh)
  "Perl stat — 13-element file-status list (dev ino mode nlink uid gid rdev
   size atime mtime ctime blksize blocks).  Follows symlinks.  nil on failure."
  (let ((arg (%p-stat-arg file-or-fh)))
    (handler-case
        (%p-stat-vector
         (if (streamp arg)
             (sb-posix:fstat (sb-sys:fd-stream-fd arg))
             (sb-posix:stat arg)))
      (error () (%pcl-save-errno) nil))))

(defun p-lstat (file)
  "Perl lstat — like stat but does NOT follow a symlink (reports the link)."
  (let ((arg (%p-stat-arg file)))
    (handler-case
        (%p-stat-vector
         (if (streamp arg)
             (sb-posix:fstat (sb-sys:fd-stream-fd arg))
             (sb-posix:lstat arg)))
      (error () (%pcl-save-errno) nil))))

;;; ============================================================
;;; File Test Operators (-e, -d, -f, -r, -w, -x, -s, -z)
;;; ============================================================

(defun p--e (file)
  "Perl -e: test if file exists"
  (let* ((path (%p--path file))
         ;; The empty path is ENOENT in perl, but (probe-file "") answers the
         ;; CWD in SBCL — so reject it before probing.  It reaches here from
         ;; undef and from `_` used before any stat.
         (exists (and (plusp (length path))
                      (or (probe-file path)
                          ;; probe-file may fail on directories in some implementations
                          (ignore-errors
                            (sb-posix:stat path)
                            t)))))
    (if exists 1 nil)))

(defun p--d (file)
  "Perl -d: test if file is a directory"
  (handler-case
      (let ((stat (sb-posix:stat (%p--path file))))
        (if (sb-posix:s-isdir (sb-posix:stat-mode stat))
            1
            nil))
    (error () nil)))

(defun p--f (file)
  "Perl -f: test if file is a regular file"
  (handler-case
      (let ((stat (sb-posix:stat (%p--path file))))
        (if (sb-posix:s-isreg (sb-posix:stat-mode stat))
            1
            nil))
    (error () nil)))

(defun p--r (file)
  "Perl -r: test if file is readable"
  (let ((path (%p--path file)))
    (handler-case
        (progn
          (sb-posix:access path sb-posix:r-ok)
          1)
      (error () nil))))

(defun p--w (file)
  "Perl -w: test if file is writable"
  (let ((path (%p--path file)))
    (handler-case
        (progn
          (sb-posix:access path sb-posix:w-ok)
          1)
      (error () nil))))

(defun p--x (file)
  "Perl -x: test if file is executable"
  (let ((path (%p--path file)))
    (handler-case
        (progn
          (sb-posix:access path sb-posix:x-ok)
          1)
      (error () nil))))

(defun p--s (file)
  "Perl -s: return file size if non-zero, nil otherwise"
  (handler-case
      (let* ((stat (sb-posix:stat (%p--path file)))
             (size (sb-posix:stat-size stat)))
        (if (> size 0) size nil))
    (error () nil)))

(defun p--z (file)
  "Perl -z: test if file has zero size"
  (handler-case
      (let* ((stat (sb-posix:stat (%p--path file)))
             (size (sb-posix:stat-size stat)))
        (if (= size 0) 1 nil))
    (error () nil)))

(defun %p--stat-test (file pred &key lstat)
  "Shared body for the mode-bit filetests: stat (or lstat) FILE, apply PRED
   to the stat struct, return Perl truth (1/nil); any stat error is nil."
  (handler-case
      (let ((stat (if lstat
                      (sb-posix:lstat (%p--path file))
                      (sb-posix:stat (%p--path file)))))
        (if (funcall pred stat) 1 nil))
    (error () nil)))

(defun p--l (file)
  "Perl -l: test if file is a symbolic link (lstat, not stat)"
  (%p--stat-test file
                 (lambda (s) (sb-posix:s-islnk (sb-posix:stat-mode s)))
                 :lstat t))

(defun p--p (file)
  "Perl -p: test if file is a named pipe (FIFO)"
  (%p--stat-test file (lambda (s) (sb-posix:s-isfifo (sb-posix:stat-mode s)))))

(defun p--S (file)
  "Perl -S: test if file is a socket"
  (%p--stat-test file (lambda (s) (sb-posix:s-issock (sb-posix:stat-mode s)))))

(defun p--b (file)
  "Perl -b: test if file is a block special file"
  (%p--stat-test file (lambda (s) (sb-posix:s-isblk (sb-posix:stat-mode s)))))

(defun p--c (file)
  "Perl -c: test if file is a character special file"
  (%p--stat-test file (lambda (s) (sb-posix:s-ischr (sb-posix:stat-mode s)))))

(defun p--u (file)
  "Perl -u: test if file has the setuid bit set"
  (%p--stat-test file (lambda (s) (logtest (sb-posix:stat-mode s) #o4000))))

(defun p--g (file)
  "Perl -g: test if file has the setgid bit set"
  (%p--stat-test file (lambda (s) (logtest (sb-posix:stat-mode s) #o2000))))

(defun p--k (file)
  "Perl -k: test if file has the sticky bit set"
  (%p--stat-test file (lambda (s) (logtest (sb-posix:stat-mode s) #o1000))))

(defun p--o (file)
  "Perl -o: test if file is owned by the effective uid"
  (%p--stat-test file (lambda (s) (= (sb-posix:stat-uid s) (sb-posix:geteuid)))))

(defun p--O (file)
  "Perl -O: test if file is owned by the real uid"
  (%p--stat-test file (lambda (s) (= (sb-posix:stat-uid s) (sb-posix:getuid)))))

;; -R/-W/-X are the real-uid variants of -r/-w/-x.  perl uses access() vs
;; eaccess(); the two differ only in setuid/setgid programs, which PCL
;; programs are not, so the effective-uid tests stand in.
(defun p--R (file)
  "Perl -R: readable by the REAL uid (== -r for non-setuid programs)"
  (p--r file))

(defun p--W (file)
  "Perl -W: writable by the REAL uid (== -w for non-setuid programs)"
  (p--w file))

(defun p--X (file)
  "Perl -X: executable by the REAL uid (== -x for non-setuid programs)"
  (p--x file))

(defun %p--file-age (file accessor)
  "Days between program start ($^T) and the ACCESSOR time of FILE (-M/-A/-C).
   $^T is referenced via symbol-value: its defvar appears later in this file."
  (handler-case
      (let ((stat (sb-posix:stat (%p--path file))))
        (/ (- (symbol-value '|$^T|) (funcall accessor stat)) 86400.0d0))
    (error () nil)))

(defun p--M (file)
  "Perl -M: script start time minus file modification time, in days"
  (%p--file-age file #'sb-posix:stat-mtime))

(defun p--A (file)
  "Perl -A: script start time minus file access time, in days"
  (%p--file-age file #'sb-posix:stat-atime))

(defun p--C (file)
  "Perl -C: script start time minus file inode-change time, in days"
  (%p--file-age file #'sb-posix:stat-ctime))

(defun %p--text-scan (file)
  "First-block scan for -T/-B (perl's heuristic): :empty, :text or :binary.
   A NUL byte in the first 512 bytes means binary; else >30% odd bytes
   (high-bit set, or controls outside TAB/LF/FF/CR/ESC) means binary."
  (handler-case
      (with-open-file (s (%p--path file)
                         :element-type '(unsigned-byte 8))
        (let* ((buf (make-array 512 :element-type '(unsigned-byte 8)))
               (n (read-sequence buf s)))
          (if (zerop n)
              :empty
              (let ((odd 0))
                (dotimes (i n)
                  (let ((b (aref buf i)))
                    (when (zerop b) (return-from %p--text-scan :binary))
                    (when (or (> b 127)
                              (and (< b 32)
                                   (not (member b '(9 10 12 13 27)))))
                      (incf odd))))
                (if (> (* 10 odd) (* 3 n)) :binary :text)))))
    (error () nil)))

(defun p--T (file)
  "Perl -T: heuristic text-file test (empty files are text)"
  (case (%p--text-scan file)
    ((:empty :text) 1)
    (t nil)))

(defun p--B (file)
  "Perl -B: heuristic binary-file test (empty files are binary too, as in perl)"
  (case (%p--text-scan file)
    ((:empty :binary) 1)
    (t nil)))

(defun %p--t-impl (fh)
  "Perl -t body: is the filehandle attached to a tty?  Undef/nil (a bare -t
   whose inserted $_ is unset) falls back to STDIN, perl's default."
  (handler-case
      (let* ((handle (if (or (null fh) (and (p-box-p fh) (null (p-box-value fh))))
                         'STDIN
                         fh))
             (fd (%p-fileno-impl handle)))
        (if (and (integerp fd) (>= fd 0) (plusp (sb-unix:unix-isatty fd)))
            1
            nil))
    (error () nil)))

(defmacro p--t (&optional (fh ''STDIN))
  "Perl -t: bareword filehandle is auto-quoted (like p-fileno)."
  `(%p--t-impl (%p-fh-arg ,fh)))

(defun p-lock (x)
  "Perl lock: a no-op returning its argument on an unthreaded perl (which
   this is — 'This Perl not built to support threads')."
  x)

(defun p-unlink (&rest files)
  "Perl unlink - delete files. Returns count of files deleted."
  (let ((count 0))
    (dolist (f files)
      (let ((path (to-string (unbox f))))
        (when (and (probe-file path) (delete-file path))
          (incf count))))
    count))

(defun %p-fileno-impl (fh)
  "Perl fileno - get file descriptor number.  Real fd via the fd-stream
   behind the handle (following synonym streams); the std streams keep
   their well-known numbers even when wrapped; -1 for in-memory handles
   (perl's answer for scalar filehandles)."
  (let ((stream (p-get-stream fh)))
    (cond
      ((%p-fd-of-stream stream))
      ((eq stream *standard-input*) 0)
      ((eq stream *standard-output*) 1)
      ((eq stream *error-output*) 2)
      (t -1))))

(defmacro p-fileno (fh)
  "Perl fileno — bareword filehandle is auto-quoted."
  `(%p-fileno-impl (%p-fh-arg ,fh)))

(defun %p-getc-impl (&optional fh)
  "Perl getc - read single character"
  (let ((stream (if fh (p-get-stream fh) *standard-input*)))
    (when stream
      (let ((ch (read-char stream nil nil)))
        (if ch (string ch) nil)))))

(defmacro p-getc (&rest args)
  "Perl getc — bareword filehandle is auto-quoted."
  (if args `(%p-getc-impl (%p-fh-arg ,(car args))) `(%p-getc-impl)))

(defun %p-readline-impl (&optional fh)
  "Perl readline / diamond operator <FH> - read a record from filehandle.
   Respects $/ (input record separator):
     default newline = line mode, undef = slurp, \"\" = paragraph, other = custom separator.
   Returns nil at EOF. If no filehandle given, reads from *standard-input*.
   Note: Unlike CL's read-line, this keeps the trailing separator (like Perl).
   Updates $. (input line number) on each successful read."
  (let ((stream (if fh (p-get-stream fh) *standard-input*))
        (sep (get-input-record-separator)))
    ;; Remember the handle so a later argument-less `eof` tests THIS stream and
    ;; $. reports THIS handle's line counter.
    (when stream (setf *p-last-read-handle* stream))
    (let ((%rl-result
           (when stream
             (handler-case
                 (cond
                   ;; Slurp mode: $/ = undef - read entire file
                   ((null sep)
                    (let ((content (make-array 4096 :element-type 'character
                                               :adjustable t :fill-pointer 0)))
                      (loop for char = (read-char stream nil nil)
                            while char
                            do (vector-push-extend char content))
                      (if (zerop (length content)) nil (coerce content 'string))))

                   ;; Record mode: $/ = \N - read exactly N characters per record.
                   ((integerp sep)
                    (let* ((buf (make-string sep))
                           (got (read-sequence buf stream)))
                      (if (zerop got) nil (subseq buf 0 got))))

                   ;; Paragraph mode: $/ = "" - read until blank line
                   ((string= sep "")
                    (let ((lines nil)
                          (seen-content nil)
                          (last-missing-nl nil))
                      (loop
                       (multiple-value-bind (line missing-nl) (read-line stream nil nil)
                         (cond
                           ((null line)
                            ;; EOF: rebuild the record.  Only append the final newline
                            ;; if the last content line actually had one — a file whose
                            ;; last line lacks a trailing newline keeps it that way
                            ;; (Perl does not invent one).
                            (return
                              (if lines
                                  (let ((body (format nil "~{~A~^~%~}" (nreverse lines))))
                                    (if last-missing-nl
                                        body
                                        (concatenate 'string body (string #\Newline))))
                                  nil)))
                           ((string= line "")
                            (if seen-content
                                (return (format nil "~{~A~^~%~}~%~%" (nreverse lines)))
                                nil))  ; Skip leading blank lines
                           (t
                            (setf seen-content t
                                  last-missing-nl missing-nl)
                            (push line lines)))))))

                   ;; Single character separator (common case, optimized)
                   ((= (length sep) 1)
                    (let ((sep-char (char sep 0))
                          (result (make-array 256 :element-type 'character
                                              :adjustable t :fill-pointer 0)))
                      (loop for char = (read-char stream nil nil)
                            while char
                            do (vector-push-extend char result)
                            when (char= char sep-char)
                            do (loop-finish))
                      (if (zerop (length result)) nil (coerce result 'string))))

                   ;; Multi-character separator
                   (t
                    (let ((result (make-array 256 :element-type 'character
                                              :adjustable t :fill-pointer 0))
                          (sep-len (length sep)))
                      (loop for char = (read-char stream nil nil)
                            while char
                            do (vector-push-extend char result)
                            when (and (>= (length result) sep-len)
                                      (string= result sep
                                               :start1 (- (length result) sep-len)))
                            do (loop-finish))
                      (if (zerop (length result)) nil (coerce result 'string)))))
               ;; Any stream error (e.g. reading from a directory) → return nil like Perl
               (stream-error () nil)
               (error () nil)))))
      ;; A successful record read bumps THIS handle's line counter ($.).
      (when (and stream %rl-result)
        (incf (gethash stream *p-fh-lines* 0)))
      %rl-result)))

(defun %p-readline-all (fh)
  "Read all remaining records from FH into an adjustable vector of boxed strings.
   %p-readline-impl bumps $. (the per-handle line counter) on each read."
  (let ((result (make-array 8 :adjustable t :fill-pointer 0)))
    (loop
     (let ((line (%p-readline-impl fh)))
       (if line
           (vector-push-extend (make-p-box line) result)
           (return result))))))

;;; ----------------------------------------------------------------
;;; Diamond operator <> / <ARGV>: read records across the files named in
;;; @ARGV, falling back to STDIN when @ARGV is empty.  $ARGV holds the current
;;; filename ("-" for STDIN).  Unlike a plain filehandle, $. is *cumulative*
;;; across the @ARGV files (Perl never implicitly closes ARGV), so when we move
;;; to the next file we seed its line counter with the previous file's count.
(declaim (special @ARGV $ARGV |$^I|))
(defvar *p-argv-stream* nil "Currently-open <> input stream, or nil before/after.")
(defvar *p-argv-started* nil "T once <> has begun consuming @ARGV (or chosen STDIN).")
(defvar *p-argv-last-count* 0 "Cumulative $. carried from the previous <> file.")

;;; In-place editing ($^I / perl -i): while <> reads a real file, the default
;;; output (print/printf with no handle, i.e. *standard-output*) is redirected
;;; to a temp file; when that file is finished the temp replaces the original
;;; (renaming the original to a backup first when $^I carries an extension).
;;; STDIN ("-") is never edited in place.
(defvar *p-inplace-out* nil "Temp output stream for in-place editing of the current <> file.")
(defvar *p-inplace-orig* nil "Original path of the file being edited in place.")
(defvar *p-inplace-tmp* nil "Temp file path used for in-place editing.")
(defvar *p-inplace-saved-out* nil "*standard-output* saved across an in-place edit.")

(defun %p-inplace-ext ()
  "Backup extension string if $^I is defined (\"\" = edit with no backup), else
   nil (in-place editing off)."
  (let ((v (unbox |$^I|)))
    (if (or (null v) (eq v *p-undef*)) nil (to-string v))))

(defun %p-inplace-begin (orig-path)
  "Redirect default output to a fresh temp file alongside ORIG-PATH."
  (let* ((tmp (format nil "~A.pcl-inplace-~36,8,'0R" orig-path (random (expt 36 8))))
         (out (open tmp :direction :output :if-exists :supersede
                    :if-does-not-exist :create)))
    (setf *p-inplace-orig* orig-path
          *p-inplace-tmp* tmp
          *p-inplace-out* out
          *p-inplace-saved-out* *standard-output*
          *standard-output* out)))

(defun %p-inplace-finish ()
  "Close the redirected output, restore *standard-output*, back up the original
   when $^I has an extension, then move the temp file over the original.  No-op
   when no in-place edit is active."
  (when *p-inplace-out*
    (close *p-inplace-out*)
    (setf *standard-output* *p-inplace-saved-out*)
    (let ((ext (%p-inplace-ext)) (orig *p-inplace-orig*) (tmp *p-inplace-tmp*))
      (if (and ext (plusp (length ext)))
          ;; '*' in the extension is replaced by the original name; else appended.
          (let ((backup (if (find #\* ext)
                            (with-output-to-string (s)
                              (loop for ch across ext
                                    do (if (char= ch #\*) (write-string orig s)
                                           (write-char ch s))))
                            (concatenate 'string orig ext))))
            ;; If the backup rename fails, Perl skips the file: leave the
            ;; original untouched and discard the edited temp.
            (handler-case
                (progn (sb-posix:rename orig backup)
                       (sb-posix:rename tmp orig))
              (error ()
                (p-warn (format nil "Can't rename ~A to ~A: No such file or directory, skipping file"
                                orig backup))
                (ignore-errors (delete-file tmp)))))
          (ignore-errors (sb-posix:rename tmp orig)))) ; no backup: replace original
    (setf *p-inplace-out* nil *p-inplace-orig* nil
          *p-inplace-tmp* nil *p-inplace-saved-out* nil)))

(defun %p-argv-open-next ()
  "Shift the next filename off @ARGV, open it, set $ARGV, and return its stream.
   Empty @ARGV on the first call yields STDIN.  Unopenable files warn and are
   skipped (Perl behaviour).  Returns nil when the file sequence is exhausted."
  (loop
   (when (zerop (length @ARGV))
     (return
       (cond
         (*p-argv-started* nil)                       ; all files consumed
         (t (setf *p-argv-started* t)                 ; bare <> with empty @ARGV
            (box-set $ARGV "-")
            *standard-input*))))
   (setf *p-argv-started* t)
   (let ((fname (to-string (unbox (p-shift @ARGV)))))
     (box-set $ARGV fname)
     (cond
       ((string= fname "-") (return *standard-input*))
       (t (let ((s (ignore-errors
                     (open fname :direction :input :if-does-not-exist nil))))
            (cond
              ((null s)
               (p-warn (format nil "Can't open ~A: No such file or directory"
                               fname)))                ; skip to next file
              (t (when (%p-inplace-ext) (%p-inplace-begin fname))
                 (return s)))))))))

(defun %p-readline-argv ()
  "Scalar-context <> : read one record across the @ARGV file sequence."
  (loop
   (unless *p-argv-stream*
     (setf *p-argv-stream* (%p-argv-open-next))
     (unless *p-argv-stream* (return nil))
     ;; Seed the new file's $. with the cumulative count from prior files.
     (setf (gethash *p-argv-stream* *p-fh-lines*) *p-argv-last-count*))
   (let ((line (%p-readline-impl *p-argv-stream*)))   ; sets last-handle, bumps $.
     (setf *p-argv-last-count* (gethash *p-argv-stream* *p-fh-lines* 0))
     (if line
         (return line)
         (progn                                        ; current file at EOF
           (when (not (eq *p-argv-stream* *standard-input*))
             (ignore-errors (close *p-argv-stream*)))
           (%p-inplace-finish)                          ; commit in-place edit (if any)
           (setf *p-argv-stream* nil))))))             ; advance on next turn

(defun %p-readline-argv-all ()
  "List-context <> : read every remaining record across @ARGV into a vector."
  (let ((result (make-array 8 :adjustable t :fill-pointer 0)))
    (loop
     (let ((line (%p-readline-argv)))
       (if line
           (vector-push-extend (make-p-box line) result)
           (return result))))))

(defun %p-readline-argv-form-p (form)
  "True if FORM is the diamond marker (quote ARGV) emitted by codegen for <ARGV>."
  (and (consp form) (eq (car form) 'quote)
       (symbolp (cadr form))
       (string-equal (symbol-name (cadr form)) "ARGV")))

(defmacro p-readline (&rest args)
  "Perl readline / <FH> — in list context reads all records; in scalar reads one.
   When *p-in-list-assign-rhs* is t (inside p-list-= RHS), always use scalar mode
   so that while (($x) = <FH>) reads one line per iteration, not the whole file.
   No filehandle (<>) or the bareword ARGV (<ARGV>) is the diamond operator.
   %p-readline-impl bumps $. (per-handle line counter) on each successful read."
  (if (or (null args) (%p-readline-argv-form-p (car args)))
      `(if (and (eq *wantarray* t) (not *p-in-list-assign-rhs*))
           (%p-readline-argv-all)
           (%p-readline-argv))
      `(if (and (eq *wantarray* t) (not *p-in-list-assign-rhs*))
           (%p-readline-all ,(car args))
           (%p-readline-impl ,@args))))

;;; ============================================================
;;; Directory I/O Functions
;;; ============================================================

;; Directory handle storage
(defvar *p-dirhandles* (make-hash-table :test 'eq))

(defun %p-dirent-name (path)
  "The entry's OWN name: file-namestring for a file, the last directory
   component for a subdirectory (whose file-namestring is \"\" — that
   emptiness is what File::Path's remove_tree used to try to unlink)."
  (let ((fn (file-namestring path)))
    (if (and fn (string/= fn ""))
        fn
        (let ((d (pathname-directory path)))
          (if (and (consp d) (stringp (car (last d))))
              (car (last d))
              (namestring path))))))

(defun %p-opendir-impl (dh dir)
  "Perl opendir - open directory for reading.
   DIR is always treated as a DIRECTORY even without a trailing slash:
   merging \"*.*\" onto \"/a/b/rd\" parses rd as a file NAME, so the wild
   card replaced it and the listing was of /a/b/ — the PARENT, silently.
   Symlinks are not resolved (readdir reports the link's own name), and
   the entry list starts with \".\" and \"..\" as perl's does."
  (let* ((dir-str (to-string dir))
         (dir-path (if (and (plusp (length dir-str))
                            (char= (char dir-str (1- (length dir-str))) #\/))
                       dir-str
                       (concatenate 'string dir-str "/"))))
    (when (probe-file dir-path)
      (let* ((entries (directory (merge-pathnames "*.*" dir-path)
                                 :resolve-symlinks nil))
             (names (list* "." ".." (mapcar #'%p-dirent-name entries))))
        (if (symbolp dh)
            (setf (gethash dh *p-dirhandles*) (cons 0 names))
            (when (p-box-p dh)
              (setf (p-box-value dh) (cons 0 names))))
        t))))

(defmacro p-opendir (dh &rest args)
  "Perl opendir — bareword dirhandle is auto-quoted."
  `(%p-opendir-impl (%p-fh-arg ,dh) ,@args))

(defun %p-readdir-impl (dh)
  "Perl readdir - next entry in scalar context; in LIST context all the
   remaining entries, leaving the handle exhausted.  Call sites bind
   *wantarray* explicitly (readdir is in ExprToCL's %WANTARRAY_SENSITIVE),
   so an ambient list binding cannot leak in."
  (let ((handle (if (symbolp dh)
                    (gethash dh *p-dirhandles*)
                    (when (p-box-p dh) (p-box-value dh)))))
    (when handle
      (let ((idx (car handle))
            (entries (cdr handle)))
        (if (eq *wantarray* t)
            (let ((rest (nthcdr idx entries)))
              (setf (car handle) (length entries))
              (make-array (length rest) :initial-contents rest
                          :adjustable t :fill-pointer t))
            (if (< idx (length entries))
                (progn
                  (setf (car handle) (1+ idx))
                  (nth idx entries))
                nil))))))

(defmacro p-readdir (dh)
  "Perl readdir — bareword dirhandle is auto-quoted."
  `(%p-readdir-impl (%p-fh-arg ,dh)))

(defun %p-closedir-impl (dh)
  "Perl closedir - close directory handle"
  (when (symbolp dh)
    (remhash dh *p-dirhandles*))
  t)

(defmacro p-closedir (dh)
  "Perl closedir — bareword dirhandle is auto-quoted."
  `(%p-closedir-impl (%p-fh-arg ,dh)))

(defun %p-rewinddir-impl (dh)
  "Perl rewinddir - reset directory to beginning"
  (let ((handle (if (symbolp dh)
                    (gethash dh *p-dirhandles*)
                    (when (p-box-p dh) (p-box-value dh)))))
    (when handle
      (setf (car handle) 0))
    t))

(defmacro p-rewinddir (dh)
  "Perl rewinddir — bareword dirhandle is auto-quoted."
  `(%p-rewinddir-impl (%p-fh-arg ,dh)))

;;; ============================================================
;;; File Glob
;;; ============================================================

;; Helper: Expand character ranges in glob bracket expressions.
;; SBCL's pathname wildcards don't fully support [a-c] ranges,
;; so we expand them: [a-c] -> [abc], [a-cxz] -> [abcxz]
;; Also handles negation: [!x] or [^x] (converted to match-all-except logic)
(defun expand-glob-char-ranges (pattern)
  "Expand character ranges like [a-c] to [abc] in glob patterns."
  (let ((result (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (i 0)
        (len (length pattern)))
    (loop while (< i len) do
          (let ((ch (char pattern i)))
            (if (and (char= ch #\[) (< (1+ i) len))
                ;; Found bracket - process bracket expression
                (let ((bracket-start i)
                      (chars (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
                      (negated nil)
                      (j (1+ i)))
                  (declare (ignore bracket-start))
                  ;; Check for negation [! or [^
                  (when (and (< j len) (or (char= (char pattern j) #\!)
                                           (char= (char pattern j) #\^)))
                    (setf negated t)
                    (incf j))
                  ;; Collect characters until ]
                  (loop while (and (< j len) (not (char= (char pattern j) #\]))) do
                        (let ((c (char pattern j)))
                          (if (and (< (+ j 2) len)
                                   (char= (char pattern (1+ j)) #\-)
                                   (not (char= (char pattern (+ j 2)) #\])))
                              ;; Range like a-c
                              (let ((start-char c)
                                    (end-char (char pattern (+ j 2))))
                                (loop for code from (char-code start-char) to (char-code end-char) do
                                      (vector-push-extend (code-char code) chars))
                                (incf j 3))
                              ;; Single character
                              (progn
                                (vector-push-extend c chars)
                                (incf j)))))
                  ;; Check if we found closing bracket
                  (if (and (< j len) (char= (char pattern j) #\]))
                      ;; Valid bracket expression - output expanded form
                      (progn
                        (vector-push-extend #\[ result)
                        (when negated
                          (vector-push-extend #\^ result))  ; SBCL uses ^ for negation
                        (loop for c across chars do
                              (vector-push-extend c result))
                        (vector-push-extend #\] result)
                        (setf i (1+ j)))
                      ;; No closing bracket - output literal [
                      (progn
                        (vector-push-extend ch result)
                        (incf i))))
                ;; Not a bracket - copy character as-is
                (progn
                  (vector-push-extend ch result)
                  (incf i)))))
    (coerce result 'string)))

;;; Per-pattern iterator state for scalar-context glob.
;;; Maps pattern string -> (index . results-vector) or :list-done (after list exhaustion).
(defvar *p-glob-iterators* (make-hash-table :test 'equal))

(defun %p-glob-component-regex (glob)
  "cl-ppcre pattern (whole-string anchored) for one shell-glob filename
   component: `*` → any run, `?` → any one char, `[..]` char classes kept
   (ranges already expanded by expand-glob-char-ranges, `!` negation → `^`),
   every other char escaped literally.  Used to filter a directory listing so
   `*` matches dotted names too (CL's pathname `*` wrongly requires no
   extension)."
  (with-output-to-string (s)
    (write-char #\^ s)
    (let ((i 0) (len (length glob)))
      (loop while (< i len) do
            (let ((c (char glob i)))
              (cond
                ((char= c #\*) (write-string "[^/]*" s) (incf i))
                ((char= c #\?) (write-string "[^/]" s) (incf i))
                ((char= c #\[)
                 (let ((j (1+ i)))
                   (when (and (< j len) (member (char glob j) '(#\^ #\!))) (incf j))
                   (when (and (< j len) (char= (char glob j) #\])) (incf j))
                   (loop while (and (< j len) (not (char= (char glob j) #\]))) do (incf j))
                   (if (< j len)
                       (progn (write-string (substitute #\^ #\! (subseq glob i (1+ j))) s)
                              (setf i (1+ j)))
                       (progn (write-string "\\[" s) (incf i)))))
                (t (when (find c ".\\+*?()|{}^$") (write-char #\\ s))
                   (write-char c s) (incf i))))))
    (write-char #\$ s)))

(defun %p-glob-leaf-name (path)
  "Final path component of PATH as a string — the directory name for a
   directory pathname, else the file name+type."
  (if (and (null (pathname-name path)) (null (pathname-type path)))
      (car (last (pathname-directory path)))
      (file-namestring path)))

(defun %p-glob--expand-dir (dir-prefix file-glob)
  "Match FILE-GLOB against the leaf names in the fixed directory DIR-PREFIX
   (\"\" = cwd).  Enumerates every entry (files + dirs, any extension) and
   filters via a glob→regex, so `*` matches dotted names — honouring Perl's
   rule that a leading dot is matched only if the pattern starts with one."
  (let* ((relative (or (string= dir-prefix "") (not (char= (char dir-prefix 0) #\/))))
         (full-dir (cond ((string= dir-prefix "") (concatenate 'string (sb-posix:getcwd) "/"))
                         (relative (concatenate 'string (sb-posix:getcwd) "/" dir-prefix))
                         (t dir-prefix)))
         (dir-path (handler-case (parse-namestring full-dir) (error () nil)))
         (entries (when dir-path
                    (handler-case
                        (directory (make-pathname :directory (pathname-directory dir-path)
                                                  :name :wild :type :wild)
                                   :resolve-symlinks nil)
                      (error () nil))))
         (scanner (handler-case
                      (cl-ppcre:create-scanner (%p-glob-component-regex file-glob))
                    (error () nil)))
         (match-dot (and (plusp (length file-glob)) (char= (char file-glob 0) #\.)))
         (result (make-array 0 :adjustable t :fill-pointer 0)))
    (when scanner
      (dolist (p entries)
        (let ((leaf (%p-glob-leaf-name p)))
          (when (and leaf
                     (or match-dot
                         (not (and (plusp (length leaf)) (char= (char leaf 0) #\.))))
                     (cl-ppcre:scan scanner leaf))
            (vector-push-extend (concatenate 'string dir-prefix leaf) result)))))
    (sort result #'string<)))

(defun %p-glob--expand-pathname (expanded-pat orig-pat)
  "Fallback: expand via CL pathname wildcarding (used when wildcards appear in
   the DIRECTORY portion).  Imperfect for dotted names but rare."
  (let* ((relative (not (and (> (length expanded-pat) 0) (char= (char expanded-pat 0) #\/))))
         (full-pat (if relative
                       (concatenate 'string (sb-posix:getcwd) "/" expanded-pat)
                       expanded-pat))
         (dir-prefix (let ((slash-pos (position #\/ orig-pat :from-end t)))
                       (if slash-pos (subseq orig-pat 0 (1+ slash-pos)) "")))
         (all-matches (handler-case (directory (parse-namestring full-pat)) (error () nil)))
         (matches (remove-if (lambda (p) (null (pathname-name p))) all-matches))
         (result (make-array (length matches) :fill-pointer 0)))
    (dolist (path matches result)
      (vector-push (concatenate 'string dir-prefix (file-namestring path)) result))))

(defun %p-glob-own-home ()
  "This process's home directory from the passwd database — bsd_glob's
   fallback when HOME is not set in the environment."
  (handler-case
      (let ((pw (sb-posix:getpwuid (sb-posix:getuid))))
        (and pw (sb-posix:passwd-dir pw)))
    (sb-posix:syscall-error () nil)))

(defun %p-glob-expand-tilde (pat)
  "bsd_glob's leading-tilde expansion, which perl's glob/<…> performs before
   any wildcard matching: `~` and `~/rest` become $HOME, `~user[/rest]` that
   user's home directory.  An UNKNOWN user leaves the pattern untouched —
   that is what perl answers (probed: glob(\"~nosuchuser42\") is
   \"~nosuchuser42\"), and so does a tilde anywhere but the first character
   (`x~y` is a literal)."
  (if (or (zerop (length pat)) (char/= (char pat 0) #\~))
      pat
      (let* ((slash (position #\/ pat))
             (user  (subseq pat 1 (or slash (length pat))))
             (rest  (if slash (subseq pat slash) ""))
             (home  (if (string= user "")
                        (or (sb-posix:getenv "HOME") (%p-glob-own-home))
                        (handler-case
                            (let ((pw (sb-posix:getpwnam user)))
                              (and pw (sb-posix:passwd-dir pw)))
                          (sb-posix:syscall-error () nil)))))
        (if home (concatenate 'string home rest) pat))))

(defun p-glob--expand (pat)
  "Expand glob pattern PAT and return a vector of matching filenames."
  (let* ((expanded (expand-glob-char-ranges (%p-glob-expand-tilde pat)))
         (slash (position #\/ expanded :from-end t))
         (dir-prefix (if slash (subseq expanded 0 (1+ slash)) ""))
         (file-glob  (if slash (subseq expanded (1+ slash)) expanded)))
    (if (or (string= file-glob "")
            (find-if (lambda (c) (member c '(#\* #\? #\[))) dir-prefix))
        ;; Wildcard in the directory portion (rare): old pathname behaviour.
        (%p-glob--expand-pathname expanded pat)
        ;; Common case: fixed directory, wildcard last component.
        (%p-glob--expand-dir dir-prefix file-glob))))

(defun p-glob (&optional pattern)
  "Perl glob / <*.txt> - expand file glob pattern.
   In list context: first call returns all matches; second call with same pattern returns empty.
   In scalar context: returns one match per call, nil when exhausted; resets for next cycle.
   When *p-in-list-assign-rhs* is t (inside a p-list-= RHS) glob always uses scalar
   (iterator) mode, so `while (($x) = glob(...))` returns one file per iteration —
   mirrors p-readline's handling of `while (($x) = <FH>)`."
  (let ((pat (if pattern (to-string pattern) "*")))
    (if (and (eq *wantarray* t) (not *p-in-list-assign-rhs*))
        (p-glob--list-context pat)
        (p-glob--scalar-context pat))))

(defun p-glob--list-context (pat)
  "Glob in list context: first call returns all matches, second returns empty (then resets)."
  (let ((state (gethash pat *p-glob-iterators*)))
    (cond
      ((eq state :list-done)
       (remhash pat *p-glob-iterators*)
       (make-array 0 :adjustable t :fill-pointer 0))
      (t
       (let ((vec (p-glob--expand pat)))
         (setf (gethash pat *p-glob-iterators*) :list-done)
         vec)))))

(defun p-glob--scalar-context (pat)
  "Glob in scalar context: return one match per call, nil when exhausted.
   After exhaustion, returns nil once (terminating a while loop), then resets."
  (let ((state (gethash pat *p-glob-iterators*)))
    (cond
      ;; Active scalar iterator: return next entry
      ((and (consp state) (< (car state) (length (cdr state))))
       (let* ((idx   (car state))
              (vec   (cdr state))
              (entry (aref vec idx)))
         (if (< (1+ idx) (length vec))
             (setf (car state) (1+ idx))        ; advance
             (setf (gethash pat *p-glob-iterators*) :scalar-done)) ; mark exhausted
         entry))
      ;; Exhausted: return nil, then reset for next cycle
      ((eq state :scalar-done)
       (remhash pat *p-glob-iterators*)
       nil)
      ;; No iterator: start fresh
      (t
       (remhash pat *p-glob-iterators*)
       (let ((vec (p-glob--expand pat)))
         (if (zerop (length vec))
             nil
             (progn
               (if (> (length vec) 1)
                   (setf (gethash pat *p-glob-iterators*) (cons 1 vec))
                   (setf (gethash pat *p-glob-iterators*) :scalar-done))
               (aref vec 0))))))))




;;; ============================================================
;;; File/Directory Operations
;;; ============================================================

(defun p-chdir (&optional dir)
  "Perl chdir - change current directory. Returns true on success.
   Also updates *default-pathname-defaults* for Lisp path resolution."
  (let ((path
         (if (null dir)
             ;; No argument: try HOME then LOGDIR
             (let ((home (sb-posix:getenv "HOME")))
               (if home
                   home
                   (let ((logdir (sb-posix:getenv "LOGDIR")))
                     (if logdir
                         logdir
                         ;; No HOME or LOGDIR: set EINVAL and fail
                         (progn
                           (setf *p-stored-errno* 22)
                           (setf (sb-alien:extern-alien "errno" sb-alien:int) 22)
                           (return-from p-chdir nil))))))
             ;; Argument provided: check for filehandle/dirhandle
             (let ((raw (if (p-box-p dir) (p-box-value dir) dir)))
               (if (or (streamp raw)
                       (and (consp raw) (integerp (car raw))) ; dirhandle stored as (idx . entries)
                       (and (p-box-p dir)                     ; dirhandle box via opendir
                            (let ((v (p-box-value dir)))
                              (and (consp v) (integerp (car v))))))
                   ;; It's a filehandle or dirhandle: fchdir not implemented
                   (p-die (format nil "The fchdir function is unimplemented at pcl line 0.~%"))
                   (to-string dir))))))
    (handler-case
        (progn
          (sb-posix:chdir path)
          (setf *default-pathname-defaults* (truename (pathname path)))
          t)
      (sb-posix:syscall-error (e)
        (setf *p-stored-errno* (sb-posix:syscall-errno e))
        nil)
      (error ()
        (%pcl-save-errno)
        nil))))

(defun p-set_up_inc (&rest dirs)
  "Perl test.pl set_up_inc - modifies @INC for tests. No-op in PCL since
   @INC is set up during transpilation."
  (declare (ignore dirs))
  t)

(defun p-mkdir (dir &optional mode)
  "Perl mkdir - create directory. Returns true on success, sets $! on failure."
  (let ((path (to-string dir))
        (m (if mode (truncate (to-number mode)) #o755)))
    (handler-case
        (progn (sb-posix:mkdir path m) t)
      (sb-posix:syscall-error (e)
        (setf *p-stored-errno* (sb-posix:syscall-errno e))
        nil)
      (error () nil))))

(defun p-rmdir (dir)
  "Perl rmdir - remove empty directory. Returns true on success, sets $! on failure."
  (handler-case
      (progn (sb-posix:rmdir (to-string dir)) t)
    (sb-posix:syscall-error (e)
      (setf *p-stored-errno* (sb-posix:syscall-errno e))
      nil)
    (error () nil)))

(defun p-getcwd ()
  "Perl getcwd/cwd - get current working directory."
  (sb-posix:getcwd))

(defun p-cwd ()
  "Perl cwd - alias for getcwd."
  (sb-posix:getcwd))

(defun p-rename (old new)
  "Perl rename - rename file. Returns true on success."
  (handler-case
      (progn (rename-file (to-string old) (to-string new)) t)
    (error () nil)))

(defun p-chmod (mode &rest files)
  "Perl chmod MODE, LIST — change permissions. Returns count changed.
   A filehandle in the LIST is fchmod'd by descriptor; everything else is a path.
   (Only an actual open stream is treated as a handle — a plain string is always
   a filename, so chmod 0644, 'a' is never mistaken for a handle named 'a'.)"
  (let ((m (truncate (to-number mode)))
        (count 0))
    (dolist (f files count)
      (let ((v (if (p-box-p f) (p-box-value f) f)))
        (handler-case
            (progn
              (if (streamp v)
                  (sb-posix:fchmod (sb-sys:fd-stream-fd v) m)
                  (sb-posix:chmod (to-string v) m))
              (incf count))
          (error () (%pcl-save-errno) nil))))))

(defun p-umask (&optional mode)
  "Perl umask [EXPR] — set the file-creation mask and return the PREVIOUS value.
   With no argument, return the current mask without changing it (sb-posix:umask
   always sets, so we set-then-restore to read it non-destructively)."
  (if mode
      (sb-posix:umask (%pcl-to-integer (to-number mode)))
      (let ((cur (sb-posix:umask 0)))
        (sb-posix:umask cur)
        cur)))

(defun p-link (old new)
  "Perl link OLD, NEW — create a hard link. Returns 1 on success, '' on failure."
  (handler-case (progn (sb-posix:link (to-string old) (to-string new)) 1)
    (error () (%pcl-save-errno) "")))

(defun p-symlink (old new)
  "Perl symlink OLD, NEW — create a symbolic link. Returns 1 on success, 0 on failure."
  (handler-case (progn (sb-posix:symlink (to-string old) (to-string new)) 1)
    (error () (%pcl-save-errno) 0)))

(defun p-readlink (path)
  "Perl readlink EXPR — return the target of a symbolic link (undef on failure).
   EXPR defaults to $_ (the codegen supplies it for the no-arg form)."
  (handler-case (sb-posix:readlink (to-string path))
    (error () (%pcl-save-errno) *p-undef*)))

(defun %pcl-chown-id (x)
  "Map a Perl chown uid/gid to sb-posix's UNSIGNED argument.  Perl's -1 ('leave
   unchanged') must reach the C layer as (uid_t)-1 = #xFFFFFFFF; sb-posix's FFI
   rejects a negative integer outright."
  (let ((n (%pcl-to-integer (to-number x))))
    (if (minusp n) (logand n #xFFFFFFFF) n)))

(defun p-chown (&optional (uid nil uid-p) (gid nil gid-p) &rest files)
  "Perl chown UID, GID, LIST — change owner/group. Returns count changed.
   A UID or GID of -1 leaves that attribute unchanged.  A filehandle in the LIST
   is fchown'd by descriptor.  An empty argument list (chown +()) is 0 files."
  (unless (and uid-p gid-p) (return-from p-chown 0))
  (let ((u (%pcl-chown-id uid))
        (g (%pcl-chown-id gid))
        (count 0))
    (dolist (f files count)
      (let ((v (if (p-box-p f) (p-box-value f) f)))
        (handler-case
            (progn
              (if (streamp v)
                  (sb-posix:fchown (sb-sys:fd-stream-fd v) u g)
                  (sb-posix:chown (to-string v) u g))
              (incf count))
          (error () (%pcl-save-errno) nil))))))

(defun p-utime (&optional atime mtime &rest files)
  "Perl utime ATIME, MTIME, LIST — set access/modification times. Returns count.
   Times are Unix-epoch seconds (same convention as sb-posix:utime).  undef
   ATIME/MTIME means 'now', which sb-posix:utime uses when the times are omitted.
   Both times are optional: `utime 'x'` (op/lex_assign.t) is a plain
   short list — 0 files touched, returns 0."
  (let ((a (unless (or (null atime) (eq atime *p-undef*))
             (%pcl-to-integer (to-number atime))))
        (m (unless (or (null mtime) (eq mtime *p-undef*))
             (%pcl-to-integer (to-number mtime))))
        (count 0))
    (dolist (f files count)
      (handler-case
          (progn
            (if (and a m)
                (sb-posix:utime (to-string f) a m)
                (sb-posix:utime (to-string f)))
            (incf count))
        (error () nil)))))

;;; ============================================================
;;; Time Functions
;;; ============================================================

;; Perl epoch is Unix epoch (1970), CL epoch is 1900
;; Difference: 2208988800 seconds
(defconstant +unix-epoch-offset+ 2208988800)

(defun p-time ()
  "Perl time - return seconds since Unix epoch."
  (- (get-universal-time) +unix-epoch-offset+))

;;; $^T (BASETIME) - the time the program started, as Unix seconds.  Used by the
;;; -M/-A/-C file-test operators (file age relative to program start).  Set once
;;; at load time, like Perl sets it at interpreter startup.
(defvar |$^T| (- (get-universal-time) +unix-epoch-offset+)
  "Perl $^T - program start time (seconds since Unix epoch)")

(defun p-times (&key wantarray)
  "Perl times - return process times (user, system, child-user, child-system).
   Uses CL's get-internal-run-time for user time approximation.
   System and child times returned as 0 (not easily available in portable CL)."
  (declare (ignorable wantarray))
  (let* ((run-time (/ (coerce (get-internal-run-time) 'double-float)
                      (coerce internal-time-units-per-second 'double-float)))
         (user run-time)
         (sys 0.0d0)
         (cuser 0.0d0)
         (csys 0.0d0))
    (vector (make-p-box user) (make-p-box sys) (make-p-box cuser) (make-p-box csys))))

(defun p-sleep (secs)
  "Perl sleep - pause execution for specified seconds. Returns seconds slept."
  (let ((n (truncate (to-number secs))))
    (sleep n)
    n))

(defvar *p-alarm-handler-installed* nil
  "Whether the SIGALRM Unix handler has been installed yet (lazy, on first alarm).")

(defun %p-ensure-alarm-handler ()
  "Install a SIGALRM handler (once) that dispatches to the Perl $SIG{ALRM} handler.
   Done lazily so programs that never call alarm keep SBCL's default signal disposition."
  (unless *p-alarm-handler-installed*
    (setf *p-alarm-handler-installed* t)
    (sb-sys:enable-interrupt
     sb-unix:sigalrm
     (lambda (signo info ctx)
       (declare (ignore signo info ctx))
       (let ((handler (gethash "ALRM" %SIG)))
         (when (and handler (functionp (unbox handler)))
           ;; Perl passes the signal name as $_[0]; the handler may die, which
           ;; unwinds out of any blocking syscall (read) interrupted by the signal.
           (funcall (unbox handler) (make-p-box "ALRM"))))))))

(defun p-alarm (&optional secs)
  "Perl alarm - schedule SIGALRM after SECS seconds (0 cancels a pending alarm).
   When it fires, $SIG{ALRM} is invoked.  Returns the number of seconds that were
   remaining on any previously-scheduled alarm (Perl semantics)."
  (%p-ensure-alarm-handler)
  (sb-posix:alarm (if secs (truncate (to-number secs)) 0)))

(defun p-evalbytes (s)
  "Perl evalbytes - evaluate byte string as Perl code. PCL: delegates to eval."
  (p-eval s))

(defun p-study (&optional str)
  "Perl study - deprecated no-op in modern Perl. Returns 1."
  (declare (ignore str))
  1)

(defun p-reset (&optional pattern)
  "Perl reset - reset ?? searches. No-op in PCL, returns 1."
  (declare (ignore pattern))
  1)

(defun %pcl-vec-check-wide (s)
  "Signal Perl's 'Use of strings with code points over 0xFF' error if any char > 0xFF."
  (when (some (lambda (c) (> (char-code c) 255)) s)
    (p-die "Use of strings with code points over 0xFF as arguments to vec is forbidden")))

(defun p-vec (str offset bits)
  "Perl vec - treat string as bit vector and extract element.
   OFFSET is the element index, BITS is element size (1, 2, 4, 8, 16, 32, 64).
   Returns the numeric value at that position."
  (let* ((str  (p-scalar str))  ; vec evaluates its string arg in scalar context
         (s (to-string str))
         (offset (truncate (to-number offset)))
         (bits   (truncate (to-number bits))))
    (%pcl-vec-check-wide s)
    (unless (member bits '(1 2 4 8 16 32 64))
      (p-die (format nil "Illegal number of bits in vec")))
    ;; Negative offset: return 0 (Perl silently returns 0 for rval)
    (when (< offset 0)
      (return-from p-vec 0))
    (let* ((byte-offset (floor (* offset bits) 8))
           (bit-offset (mod (* offset bits) 8)))
      (cond
        ;; Beyond string length - return 0
        ((>= byte-offset (length s)) 0)
        ;; 8-bit aligned access (common case)
        ((and (= bits 8) (= bit-offset 0))
         (char-code (char s byte-offset)))
        ;; Byte-aligned multi-byte access (16/32/64), big-endian / network
        ;; order.  ONE loop rather than a branch per width: 64 was MISSING and
        ;; fell through to the (t 0) default below, so `vec($x,1,64)` read back
        ;; 0 even though p-vec-set writes it correctly and the docstring lists
        ;; 64 as legal (op/64bitint.t t80/t81).  Bytes past the end read as 0,
        ;; which is what the per-width branches did.
        ((and (member bits '(16 32 64)) (= bit-offset 0))
         (let ((n 0))
           (dotimes (i (floor bits 8) n)
             (let ((k (+ byte-offset i)))
               (setf n (+ (ash n 8)
                          (if (< k (length s)) (char-code (char s k)) 0)))))))
        ;; Sub-byte access (1, 2, 4 bits)
        ((and (<= bits 8) (< byte-offset (length s)))
         (let* ((byte-val (char-code (char s byte-offset)))
                (mask (1- (ash 1 bits))))
           (logand (ash byte-val (- bit-offset)) mask)))
        ;; Default
        (t 0)))))

(defun p-vec-set (str-box offset bits value)
  "Perl vec lvalue - set element in string-as-bit-vector.
   BITS must be 1, 2, 4, 8, 16, 32, or 64. Negative OFFSET dies. Modifies str-box."
  (let* ((offset (truncate (to-number offset)))
         (bits   (truncate (to-number bits)))
         (val    (truncate (to-number value))))
    (%pcl-vec-check-wide (to-string str-box))
    (unless (member bits '(1 2 4 8 16 32 64))
      (p-die "Illegal number of bits in vec"))
    (when (< offset 0)
      (p-die "Negative offset to vec in lvalue context"))
    (let* ((byte-offset   (floor (* offset bits) 8))
           (bit-offset    (mod (* offset bits) 8))
           (needed-bytes  (+ byte-offset (ceiling bits 8))))
      ;; Very large allocation would exhaust memory — die like Perl does
      (when (> needed-bytes (* 256 1024 1024))  ; > 256 MB
        (p-die "Out of memory during vec in lvalue context"))
      (let* ((s             (to-string str-box))
             ;; Extend string if needed (fill with NUL bytes)
             (s-ext (if (< (length s) needed-bytes)
                        (concatenate 'string s
                                     (make-string (- needed-bytes (length s))
                                                  :initial-element #\Nul))
                        (copy-seq s))))
        (cond
          ;; 8-bit aligned
          ((and (= bits 8) (= bit-offset 0))
           (setf (char s-ext byte-offset) (code-char (logand val 255))))
          ;; Byte-aligned multi-byte (16/32/64), big-endian — Perl stores MSB
          ;; first.  ONE loop rather than a branch per width: 64 was MISSING
          ;; here exactly as in p-vec's reader, so `vec($x,1,64) = $q` extended
          ;; the string to the right length and then wrote NOTHING — all-zero
          ;; bytes, silently (op/64bitint.t t80/t81).
          ((and (member bits '(16 32 64)) (= bit-offset 0))
           (let ((nbytes (floor bits 8)))
             (dotimes (i nbytes)
               (setf (char s-ext (+ byte-offset i))
                     (code-char (logand (ash val (* -8 (- nbytes 1 i))) 255))))))
          ;; Sub-byte access (1, 2, 4 bits)
          ((<= bits 8)
           (let* ((mask     (1- (ash 1 bits)))
                  (byte-val (char-code (char s-ext byte-offset)))
                  (new-byte (logior (logand byte-val (lognot (logand 255 (ash mask bit-offset))))
                                    (logand 255 (ash (logand val mask) bit-offset)))))
             (setf (char s-ext byte-offset) (code-char new-byte)))))
        ;; Write modified string back to the box (routes through STORE for tied vars)
        (when (p-box-p str-box)
          (box-set str-box s-ext))
        val))))

;;; ============================================================
;;; Extended-range calendar helpers (Howard Hinnant civil_from_days)
;;; Works for any integer Unix timestamp, including pre-1900 dates.
;;; ============================================================

(defun %pcl-days-to-ymd (epoch-days)
  "Howard Hinnant's civil_from_days algorithm.
   EPOCH-DAYS = days since 1970-01-01 (any integer, including negative).
   Returns (values year month day) with month 1-12, day 1-31."
  (let* ((z (+ epoch-days 719468))
         (era (if (>= z 0)
                  (floor z 146097)
                  (floor (- z 146096) 146097)))
         (doe (- z (* era 146097)))
         (yoe (floor (- doe (floor doe 1460) (- (floor doe 36524)) (floor doe 146096)) 365))
         (y   (+ yoe (* era 400)))
         (doy (- doe (* 365 yoe) (floor yoe 4) (- (floor yoe 100))))
         (mp  (floor (+ (* 5 doy) 2) 153))
         (d   (+ (- doy (floor (+ (* 153 mp) 2) 5)) 1))
         (m   (if (< mp 10) (+ mp 3) (- mp 9))))
    (values (+ y (if (<= m 2) 1 0)) m d)))

(defun %pcl-is-leap-year (y)
  (and (zerop (mod y 4))
       (or (not (zerop (mod y 100)))
           (zerop (mod y 400)))))

(defun %pcl-yday (y m d)
  "Day of year, 0-based. M=1-12, D=1-31."
  (let* ((days-before #(0 31 59 90 120 151 181 212 243 273 304 334))
         (base (aref days-before (1- m)))
         (leap (if (and (%pcl-is-leap-year y) (> m 2)) 1 0)))
    (+ base leap (1- d))))

(defun %pcl-unix-to-utc (unix-sec)
  "Decompose a Unix timestamp into Perl-convention broken-down UTC time.
   Returns (values sec min hour mday perl-mon perl-year wday yday)
   where perl-mon=0-11, perl-year=since 1900, wday=0=Sunday."
  (let* ((days (floor unix-sec 86400))
         (sec-in-day (- unix-sec (* days 86400)))
         (hour   (floor sec-in-day 3600))
         (rem    (- sec-in-day (* hour 3600)))
         (minute (floor rem 60))
         (sec    (- rem (* minute 60)))
         (wday   (mod (+ days 4) 7)))   ; Jan 1 1970 was Thursday=4, Sun=0
    (multiple-value-bind (year month day)
        (%pcl-days-to-ymd days)
      (values sec minute hour day (1- month) (- year 1900) wday (%pcl-yday year month day)))))

;;; Out-of-range bounds (matches Perl's TIME_UPPER/LOWER_BOUND)
(defconstant +gmtime-max+  67767976233316800)
(defconstant +gmtime-min+ -67768100567755200)

(defun %pcl-format-time (wday perl-mon day hour minute sec year)
  "Format a broken-down time as a Perl ctime string."
  (format nil "~A ~A ~2D ~2,'0D:~2,'0D:~2,'0D ~D"
          (nth wday '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
          (nth perl-mon '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                          "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
          day hour minute sec year))

;;; localtime must honour $ENV{TZ} AT THE CALL, exactly as perl does — a script
;;; that sets $ENV{TZ} and then calls localtime gets that zone.  SBCL's
;;; DECODE-UNIVERSAL-TIME resolves the zone once, from the environment the image
;;; started in, so `$ENV{TZ} = "GMT+5"` had no effect at all (t/op/time.t t7,
;;; and silently wrong times for any TZ-setting program).  Delegating to libc —
;;; tzset() + localtime_r() — is what perl itself does, so the whole POSIX TZ
;;; language comes for free: fixed offsets, named zones and their DST rules.
;;; Verified against perl for GMT-5, GMT+5, America/New_York (isdst=1) and UTC.
;;;
;;; The struct MUST carry glibc's trailing tm_gmtoff/tm_zone: localtime_r writes
;;; them, and a 9-int declaration would let it scribble past our allocation.
;;; PCL's %ENV writes go through sb-posix:setenv, so libc sees the new TZ.
(sb-alien:define-alien-type nil
    (sb-alien:struct %pcl-c-tm
                     (sec sb-alien:int) (min sb-alien:int) (hour sb-alien:int)
                     (mday sb-alien:int) (mon sb-alien:int) (year sb-alien:int)
                     (wday sb-alien:int) (yday sb-alien:int) (isdst sb-alien:int)
                     (gmtoff sb-alien:long) (zone (sb-alien:* sb-alien:char))))

(defun %p-libc-localtime (unix-time)
  "Broken-down LOCAL time for UNIX-TIME via libc, honouring $ENV{TZ}.
   Returns (values sec min hour mday perl-mon perl-year wday yday isdst) with
   perl's conventions (mon 0-11, year since 1900, wday 0=Sunday)."
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "tzset" (function sb-alien:void)))
  (sb-alien:with-alien ((tm (sb-alien:struct %pcl-c-tm))
                        (clock sb-alien:long))
    (setf clock unix-time)
    (let ((res (sb-alien:alien-funcall
                (sb-alien:extern-alien "localtime_r"
                                       (function (sb-alien:* (sb-alien:struct %pcl-c-tm))
                                                 (sb-alien:* sb-alien:long)
                                                 (sb-alien:* (sb-alien:struct %pcl-c-tm))))
                (sb-alien:addr clock) (sb-alien:addr tm))))
      ;; CLAUDE.md rule 12: a NULL return is a real failure, not a default.
      (when (sb-alien:null-alien res)
        (error "localtime_r failed for ~A (TZ=~A)"
               unix-time (or (sb-posix:getenv "TZ") "unset")))
      (values (sb-alien:slot tm 'sec) (sb-alien:slot tm 'min)
              (sb-alien:slot tm 'hour) (sb-alien:slot tm 'mday)
              (sb-alien:slot tm 'mon) (sb-alien:slot tm 'year)
              (sb-alien:slot tm 'wday) (sb-alien:slot tm 'yday)
              (if (plusp (sb-alien:slot tm 'isdst)) 1 0)))))

(defun p-localtime (&optional time)
  "Perl localtime - convert time to local time components.
   In list context returns (sec min hour mday mon year wday yday isdst).
   Note: mon is 0-11, year is years since 1900, wday is 0=Sunday.
   Warns and returns undef for out-of-range timestamps."
  (let* ((unix-time (if time
                        (handler-case (truncate (to-number time))
                          (arithmetic-error () (return-from p-localtime *p-undef*)))
                        (p-time))))
    (cond
      ((> unix-time +gmtime-max+)
       (p-warn (make-p-box (format nil "localtime(~A) too large~%localtime(~A) failed" unix-time unix-time)))
       *p-undef*)
      ((< unix-time +gmtime-min+)
       (p-warn (make-p-box (format nil "localtime(~A) too small~%localtime(~A) failed" unix-time unix-time)))
       *p-undef*)
      ;; Post-1900: libc, so $ENV{TZ} is honoured at the call (see
      ;; %p-libc-localtime).  wday/yday/isdst come from libc rather than being
      ;; recomputed, which is also how the DST flag becomes correct.
      ((>= unix-time (- +unix-epoch-offset+))
       (multiple-value-bind (sec min hour day perl-mon perl-year wday yday isdst)
           (%p-libc-localtime unix-time)
         (if (eq *wantarray* t)
             (make-array 9 :initial-contents
                         (list sec min hour day perl-mon perl-year wday yday isdst)
                         :adjustable t :fill-pointer t)
             (%pcl-format-time wday perl-mon day hour min sec (+ perl-year 1900)))))
      ;; Pre-1900: use current TZ offset (no DST awareness for extreme dates)
      (t
       (let* ((tz-secs (* -3600 (nth-value 8 (decode-universal-time (get-universal-time)))))
              (local-unix (+ unix-time tz-secs)))
         (multiple-value-bind (sec min hour day perl-mon perl-year wday yday)
             (%pcl-unix-to-utc local-unix)
           (if (eq *wantarray* t)
               (make-array 9 :initial-contents
                           (list sec min hour day perl-mon perl-year wday yday 0)
                           :adjustable t :fill-pointer t)
               (%pcl-format-time wday perl-mon day hour min sec (+ perl-year 1900)))))))))

(defun p-gmtime (&optional time)
  "Perl gmtime - convert time to UTC components.
   Same return format as localtime but in UTC.
   Warns and returns undef for out-of-range timestamps."
  (let* ((unix-time (if time
                        (handler-case (truncate (to-number time))
                          (arithmetic-error () (return-from p-gmtime *p-undef*)))
                        (p-time))))
    (cond
      ((> unix-time +gmtime-max+)
       (p-warn (make-p-box (format nil "gmtime(~A) too large~%gmtime(~A) failed" unix-time unix-time)))
       *p-undef*)
      ((< unix-time +gmtime-min+)
       (p-warn (make-p-box (format nil "gmtime(~A) too small~%gmtime(~A) failed" unix-time unix-time)))
       *p-undef*)
      (t
       (multiple-value-bind (sec min hour day perl-mon perl-year wday yday)
           (%pcl-unix-to-utc unix-time)
         (if (eq *wantarray* t)
             (make-array 9 :initial-contents
                         (list sec min hour day perl-mon perl-year wday yday 0)
                         :adjustable t :fill-pointer t)
             (%pcl-format-time wday perl-mon day hour min sec (+ perl-year 1900))))))))

;;; ============================================================
;;; Process Control
;;; ============================================================

(defun %p-pipe-impl (read-fh write-fh)
  "Perl pipe - create a connected pair of filehandles backed by an OS pipe.
   READ-FH receives the read end, WRITE-FH the write end.  Each target may be a
   p-box (lexical $fh) or a symbol (bareword FH).  Streams are unbuffered so a
   syswrite is immediately visible to a readline on the other end (same process)."
  (handler-case
      (multiple-value-bind (read-fd write-fd) (sb-posix:pipe)
        (let ((read-stream (sb-sys:make-fd-stream read-fd
                                                  :input t
                                                  :buffering :none
                                                  :external-format :utf-8))
              (write-stream (sb-sys:make-fd-stream write-fd
                                                   :output t
                                                   :buffering :none
                                                   :external-format :utf-8)))
          (if (p-box-p read-fh)
              (box-set read-fh read-stream)
              (setf (gethash read-fh *p-filehandles*) read-stream))
          (if (p-box-p write-fh)
              (box-set write-fh write-stream)
              (setf (gethash write-fh *p-filehandles*) write-stream))
          t))
    (error () (%pcl-save-errno) nil)))

(defmacro p-pipe (read-fh write-fh)
  "Perl pipe - bareword filehandles are auto-quoted; lexical $fh passed as box."
  `(%p-pipe-impl (%p-fh-arg ,read-fh) (%p-fh-arg ,write-fh)))

(defun %pcl-fdmask-int (v)
  "Perl 4-arg select bit-mask scalar → integer fd mask.
   vec() bit order: fd N lives in byte N>>3, bit N&7 — so the integer mask is
   the string's bytes assembled little-endian.  undef/empty → 0."
  (let ((mask 0))
    (unless (or (null v) (eq v :undef))
      (let ((s (to-string v)))
        (dotimes (i (length s))
          (setf mask (logior mask (ash (char-code (char s i)) (* 8 i)))))))
    mask))

(defun %pcl-fdmask-writeback (arg mask nbytes)
  "Write an integer fd mask back into a select() bit-mask scalar (if a box)."
  (when (p-box-p arg)
    (let ((s (make-string nbytes :initial-element #\Nul)))
      (dotimes (i nbytes)
        (setf (char s i) (code-char (ldb (byte 8 (* 8 i)) mask))))
      (box-set arg s))))

(defun %p-select-4arg (rbits wbits ebits timeout)
  "Perl select RBITS, WBITS, EBITS, TIMEOUT — wait for ready fds / sleep.
   Timeout is fetched exactly once (tied timeouts, RT#120102).  With no fds in
   any mask this is a (fractional-second) sleep; with fds it is a real
   select(2) via sb-unix, and the found masks are written back.  Returns the
   number of ready fds."
  (let ((rm (%pcl-fdmask-int rbits))
        (wm (%pcl-fdmask-int wbits))
        (em (%pcl-fdmask-int ebits))
        (to (unless (or (null timeout) (eq timeout :undef))
              (to-number timeout))))
    (if (and (zerop rm) (zerop wm) (zerop em))
        (progn (when (and to (> to 0)) (sleep to)) 0)
        (handler-case
            (multiple-value-bind (n rr ww ee)
                (sb-unix:unix-fast-select (integer-length (logior rm wm em))
                                          rm wm em
                                          (and to (truncate to))
                                          (if to
                                              (truncate (* (- to (truncate to))
                                                           1000000))
                                              0))
              (when (and n (> n 0))
                (let ((nbytes (ceiling (integer-length (logior rm wm em)) 8)))
                  (%pcl-fdmask-writeback rbits (or rr 0) nbytes)
                  (%pcl-fdmask-writeback wbits (or ww 0) nbytes)
                  (%pcl-fdmask-writeback ebits (or ee 0) nbytes)))
              (or n 0))
          (error () 0)))))

(defun p-select (&optional (fh nil) (wbits nil) (ebits nil) (timeout nil timeout-p))
  "Perl select - the one-arg form makes FH the default output handle for
   print/printf/say and the handle $| applies to, and returns the PREVIOUS
   selection so `my $old = select($fh); …; select($old)` restores it (the
   returned value is a designator %p-resolve-fh accepts; with nothing selected
   it is the name \"main::STDOUT\", always true — never undef, see
   %p-flatten-list).  No argument returns the current selection unchanged.
   Four-arg form is select(2) (%p-select-4arg)."
  (if timeout-p
      (%p-select-4arg fh wbits ebits timeout)
      (let ((prev (or *p-selected-out* "main::STDOUT")))
        (when fh (setf *p-selected-out* fh))
        prev)))

(defun p-write (&optional fh)
  "Perl write - emit a report via the current `format` (stub).
   format/write report templates are deliberately not-supported
   (docs/not-supported.md) and are stripped at the source level, so there is
   nothing to write.  Return 1 (Perl's success value) rather than crashing, so a
   stray write() call does not abort the whole program."
  (declare (ignore fh))
  1)

(defun p-exit (&optional code)
  "Perl exit - terminate program with exit code.
   exit during the COMPILE phase (inside BEGIN/UNITCHECK/CHECK) still runs
   the remaining UNITCHECK/CHECK blocks before the exit hooks run the ENDs
   — perl's phase semantics; INIT and main are skipped.  Once per program,
   so the flag check costs nothing."
  (unless *p-compile-phase-done* (%p-drain-compile-blocks))
  (sb-ext:exit :code (if code (truncate (to-number code)) 0)))

(defun p-system (&rest args)
  "Perl system - execute a shell command.
   system(CMD) or system(PROG, ARGS...).
   Sets $? to wait status (exit_code << 8), returns same value."
  (if (null args)
      -1
      (let* ((cmd (to-string (car args)))
             (wait-status
              (if (cdr args)
                  (let* ((prog-args (mapcar #'to-string (cdr args)))
                         (proc (sb-ext:run-program cmd prog-args
                                                   :search t
                                                   :input nil
                                                   :output *standard-output*
                                                   :error *error-output*
                                                   :wait t)))
                    (ash (sb-ext:process-exit-code proc) 8))
                  (let ((proc (sb-ext:run-program "/bin/sh" (list "-c" cmd)
                                                  :input nil
                                                  :output *standard-output*
                                                  :error *error-output*
                                                  :wait t)))
                    (ash (sb-ext:process-exit-code proc) 8)))))
        (setf $? wait-status)
        wait-status)))

(defun p-fork ()
  "Perl fork - duplicate the current process via fork(2) (sb-posix:fork).
   Returns the child PID in the parent, 0 in the child, undef on failure.
   Both processes continue running the same program from this point (Perl
   semantics); the child typically exec()s a program or exit()s.
   Output buffers are flushed first so buffered text is not duplicated into the
   child.  (Caveat: PCL cannot fork+continue a program that has spawned CL
   threads — only the forking thread survives in the child — but ordinary
   single-threaded Perl fork/exec and fork/exit works.)"
  (finish-output *standard-output*)
  (finish-output *error-output*)
  (%p-flush-open-streams)                ; perl flushes EVERY handle, not just the std three
  (handler-case (sb-posix:fork)          ; 0 in child, >0 in parent
    (error ()
      (%pcl-save-errno)
      *p-undef*)))

(defun p-waitpid (pid &optional (flags 0))
  "Perl waitpid(PID, FLAGS) - wait for a child.  Returns the reaped PID (or -1
   / 0), and sets $? to the child's raw wait status (exit_code << 8 | signal)."
  (let ((p (truncate (to-number (if (p-box-p pid) (unbox pid) pid))))
        (f (truncate (to-number (if (p-box-p flags) (unbox flags) flags)))))
    (handler-case
        (multiple-value-bind (rpid status) (sb-posix:waitpid p f)
          (setf $? status)
          rpid)
      (error () (%pcl-save-errno) -1))))

(defun p-wait ()
  "Perl wait - wait for any child.  Returns the reaped PID (or -1), sets $?."
  (handler-case
      (multiple-value-bind (rpid status) (sb-posix:wait)
        (setf $? status)
        rpid)
    (error () (%pcl-save-errno) -1)))

(defun p-getppid ()
  "Perl getppid - parent process id."
  (sb-posix:getppid))

(defun p-getpgrp (&optional pid)
  "Perl getpgrp PID - process group of PID (0 or omitted = current process).
   Returns -1 and sets $! on failure, like perl."
  (let ((p (if (and pid (not (eq pid *p-undef*)))
               (truncate (to-number (if (p-box-p pid) (unbox pid) pid)))
               0)))
    (handler-case (if (zerop p) (sb-posix:getpgrp) (sb-posix:getpgid p))
      (error ()
        (%pcl-save-errno)
        -1))))

(defun p-setpgrp (&optional pid pgrp)
  "Perl setpgrp PID, PGRP (both default 0 = current process/new group) -
   setpgid(2).  Returns 1 on success, 0 on failure with $! set."
  (let ((p (if (and pid (not (eq pid *p-undef*)))
               (truncate (to-number (if (p-box-p pid) (unbox pid) pid)))
               0))
        (g (if (and pgrp (not (eq pgrp *p-undef*)))
               (truncate (to-number (if (p-box-p pgrp) (unbox pgrp) pgrp)))
               0)))
    (handler-case
        (progn
          (sb-posix:setpgid p g)
          1)
      (error ()
        (%pcl-save-errno)
        0))))

;; getpriority(2)/setpriority(2) have no sb-posix binding — raw libc calls.
;; getpriority returns -1 both on error and as a legitimate nice value; perl
;; clears errno first and lets $! disambiguate, so we do the same.
(sb-alien:define-alien-routine ("getpriority" %c-getpriority) sb-alien:int
  (which sb-alien:int)
  (who sb-alien:int))

(defun p-getpriority (which who)
  "Perl getpriority WHICH, WHO - nice value via getpriority(2).  Returns the
   priority (may be negative); on failure returns -1 with $! set, like perl."
  (let ((wh (truncate (to-number (if (p-box-p which) (unbox which) which))))
        (wo (truncate (to-number (if (p-box-p who) (unbox who) who)))))
    (setf (sb-alien:extern-alien "errno" sb-alien:int) 0)
    (let ((r (%c-getpriority wh wo)))
      (when (= r -1)
        (%pcl-save-errno))
      r)))

(defun p-kill (signal &rest pids)
  "Perl kill SIGNAL, LIST - send SIGNAL to each PID.  SIGNAL may be a number or
   a name (\"TERM\", \"SIGKILL\", \"KILL\", ...).  Returns the count of
   processes successfully signalled.  A negative PID signals a process group."
  (let* ((sig (%p-resolve-signal signal))
         (targets (p-flatten-args pids))
         (n 0))
    (loop for pt across targets do
          (let ((p (truncate (to-number (if (p-box-p pt) (unbox pt) pt)))))
            (handler-case (progn (sb-posix:kill p sig) (incf n))
              (error () (%pcl-save-errno)))))
    n))

(defun %p-resolve-signal (signal)
  "Coerce a Perl kill() signal designator (number or name) to an integer."
  (let ((v (if (p-box-p signal) (unbox signal) signal)))
    (if (and (stringp v) (not (every #'digit-char-p v)))
        (let* ((name (string-upcase v))
               (name (if (and (> (length name) 3)
                              (string= (subseq name 0 3) "SIG"))
                         (subseq name 3) name)))
          (or (cdr (assoc name *p-signal-numbers* :test #'string=)) 15))
        (truncate (to-number v)))))

(defun p-exec (&rest args)
  "Perl exec LIST - replace the current process image with a new program.
   PCL runs the program with inherited stdio (so file descriptors set up before
   exec, e.g. after a pipe/dup in a forked child, carry through) and then exits
   with its status — like exec, this call never returns on success.  With a
   single string containing shell metacharacters it goes through /bin/sh -c."
  (when (null args)
    (setf *p-stored-errno* 2) (return-from p-exec *p-undef*))  ; ENOENT
  (let* ((strs (mapcar (lambda (a) (to-string (if (p-box-p a) (unbox a) a))) args))
         (shell-p (and (= (length strs) 1)
                       (find-if (lambda (c) (find c "|&;<>()$`\\\"'*?[]{}~ "))
                                (first strs)))))
    (handler-case
        (let ((proc (if shell-p
                        (sb-ext:run-program "/bin/sh" (list "-c" (first strs))
                                            :input t :output t :error t :wait t)
                        (sb-ext:run-program (first strs) (rest strs)
                                            :search t :input t :output t
                                            :error t :wait t))))
          (finish-output *standard-output*)
          (finish-output *error-output*)
          (sb-ext:exit :code (or (sb-ext:process-exit-code proc) 0) :abort t))
      (error ()
        (%pcl-save-errno)
        *p-undef*))))

(defun p-backtick (cmd)
  "Perl backticks - execute shell command and capture output.
   Returns the stdout output as a string. Uses latin-1 so binary output won't crash."
  (let* ((proc (sb-ext:run-program "/bin/sh" (list "-c" (to-string cmd))
                                   :input nil
                                   :output :stream
                                   :external-format :latin-1
                                   :error nil
                                   :wait nil))
         (output (with-output-to-string (s)
                   (loop for c = (read-char (sb-ext:process-output proc) nil nil)
                         while c do (write-char c s)))))
    (sb-ext:process-wait proc)
    output))

;;; ============================================================
;;; Environment Variables (%ENV)
;;; ============================================================

(defun p-env-get (key)
  "Get environment variable value."
  (sb-posix:getenv (to-string key)))

(defun p-env-set (key value)
  "Set environment variable value."
  (sb-posix:setenv (to-string key) (to-string value) 1)
  value)

;; %ENV is a special hash backed by the actual environment
;; We use a proxy approach: %ENV is a symbol that p-gethash recognizes
(defvar %ENV '%ENV-MARKER% "Marker for environment hash access")

;;; ============================================================
;;; Module System (%INC, @INC, use/require)
;;; ============================================================

;; %INC: hash of loaded modules (key: relative path, value: absolute path)
;; Note: *p-inc-table* is forward-declared near top of file
(defvar %INC '%INC-MARKER% "Marker for %INC hash access")

;; @INC: module search paths (initialized by pl2cl from Perl's @INC)
(defvar @INC (make-array 0 :adjustable t :fill-pointer 0)
  "Perl @INC - module search paths")

;; @ARGV: command line arguments (excluding program name $0)
(defvar @ARGV
  (let ((args (cdr sb-ext:*posix-argv*)))  ; skip program name
    (if args
        (make-array (length args)
                    :adjustable t
                    :fill-pointer (length args)
                    :initial-contents args)
        (make-array 0 :adjustable t :fill-pointer 0)))
  "Perl @ARGV - command line arguments")

;; $ARGV: name of the file currently being read by the <> (diamond) operator.
(defvar $ARGV (make-p-box *p-undef*)
  "Perl $ARGV - current filename of the <> diamond operator (\"-\" for STDIN)")

;; Cache configuration
(defparameter *pcl-cache-dir*
  (merge-pathnames ".pcl-cache/" (user-homedir-pathname))
  "Directory for cached compiled modules")
(defparameter *pcl-cache-generation* "v2-181"
  "Mixed into cache paths together with the effective pipeline; bump on any
   codegen change that invalidates cached module transpiles (pipeline flips,
   major emission changes).")
(defparameter *pcl-cache-max-age* (* 7 24 60 60)
  "Max cache age in seconds (default: 1 week)")
(defparameter *pcl-skip-cache* nil
  "When true, bypass cache (set by --no-cache or PCL_NO_CACHE)")
(defparameter *pcl-cache-fasl* nil
  "When true, cache compiled FASL; when nil, cache .lisp and load as SOURCE.

   NOTE (session 251): defaults to NIL as a correctness workaround for the
   module compile-file+load DOUBLE-EXECUTION bug.  With FASL caching on,
   compile-file executes the module body once (running BEGIN-time, guarded
   sub redefinitions such as Sub::Defer's deferred ctors), then `load` re-runs
   the plain `sub NAME` installs at load time and CLOBBERS those redefinitions
   (the guard makes the redefine skip on the load pass).  This breaks Moo
   subclasses (empty attrs) and any module using the define-then-guarded-
   BEGIN-redefine pattern (Moose/Sub::Quote/Type::Tiny/...).  Loading as
   source is single-pass and correct, at the cost of slower module loads.
   The proper fix (keep FASL, eliminate double-exec) is option C/D in
   docs/module-double-exec-bug.md — DO NEXT SESSION.")
(defparameter *pcl-pl2cl-path* nil
  "Path to pl2cl script (set at load time)")

;; Track modules currently being loaded (for circular dependency detection)
(defvar *p-loading-modules* nil
  "Stack of modules currently being loaded")

;;; --- Module Path Utilities ---

(defun p-module-to-path (module-name)
  "Convert Perl module name to relative path.
   Foo::Bar => Foo/Bar.pm
   Foo/Bar.pm => Foo/Bar.pm (unchanged)
   The `::` separator collapses to a SINGLE `/` — a naive per-char substitute
   turns `::` into `//`, which the OS tolerates when opening the file but leaves
   a wrong %INC key (Foo//Bar.pm), so $INC{'Foo/Bar.pm'} lookups miss."
  (let ((name (to-string module-name)))
    (if (search ".pm" name)
        name
        (let ((out (make-string-output-stream))
              (i 0)
              (len (length name)))
          (loop while (< i len) do
                (if (and (char= (char name i) #\:)
                         (< (1+ i) len)
                         (char= (char name (1+ i)) #\:))
                    (progn (write-char #\/ out) (incf i 2))
                    (progn (write-char (char name i) out) (incf i))))
          (concatenate 'string (get-output-stream-string out) ".pm")))))

(defvar *p-core-inc-dirs* nil
  "PCL's built-in module paths (lib/ shims + the system perl's libs), set by
   the pl2cl preamble.  They play the role of perl's compiled-in default
   @INC: user code that REPLACES @INC to point at perl's core lib
   (`BEGIN { @INC = '../lib' }`, the t/run/* preamble) must still resolve
   core modules, whose PCL equivalents live here.")

(defun %p-inc-dir-file (dir rel-path)
  "Probe DIR (string/pathname/box) for REL-PATH; absolute path or nil.
   For a .pm, a .pmc beside it wins (perl's PMC preference — modern .pmc
   files are just alternative source, which PCL transpiles like any .pm)."
  (let* ((d (unbox dir))
         ;; Ensure dir ends with / so merge-pathnames treats it as directory
         (s (if (stringp d) d (namestring d)))
         (s (if (and (> (length s) 0)
                     (char/= (char s (1- (length s))) #\/))
                (concatenate 'string s "/")
                s))
         (full-path (merge-pathnames rel-path (pathname s)))
         (pmc (and (>= (length rel-path) 3)
                   (string= rel-path ".pm" :start1 (- (length rel-path) 3))
                   (probe-file (concatenate 'string (namestring full-path)
                                            "c")))))
    (cond
      (pmc (namestring (truename pmc)))
      ((probe-file full-path)
       (namestring (truename full-path))))))

(defun p-find-module-in-inc (rel-path)
  "Search @INC for module file, return absolute path or nil.
   Falls back to *p-core-inc-dirs* (see its docstring); when the preamble
   never ran, PCL's lib/ beside the runtime's cl/ is the backstop."
  (or
   (loop for dir across @INC
         thereis (%p-inc-dir-file dir rel-path))
   (loop for dir in (or *p-core-inc-dirs*
                        (when *pcl-runtime-directory*
                          (list (merge-pathnames "../lib/"
                                                 *pcl-runtime-directory*))))
         thereis (%p-inc-dir-file dir rel-path))))

;;; --- Cache Management ---

(defun p-ensure-cache-dir ()
  "Create cache directory if it doesn't exist."
  (ensure-directories-exist *pcl-cache-dir*))

(defun p-compute-cache-path (source-path &optional lisp-p)
  "Compute cache path for a source file: hash of the absolute path and the
   cache GENERATION (*pcl-cache-generation*).
   The key used to carry a third component, the EFFECTIVE pipeline, so that
   toggling the PCL_V1 escape hatch could not reuse the other pipeline's
   cached transpiles.  E4.1 step 2 (#242) removed the second pipeline; the
   literal \"v2\" stays in the key so this generation's paths keep hashing
   where they did before the flag went away.
   LISP-P: if true, return .lisp path; else .fasl"
  (let* ((abs-path (namestring (truename source-path)))
         (hash (sxhash (concatenate 'string abs-path "|" *pcl-cache-generation*
                                    "|" "v2")))
         (ext (if lisp-p ".lisp" ".fasl")))
    (p-ensure-cache-dir)
    (merge-pathnames (format nil "~16,'0X~A" (logand hash #xFFFFFFFFFFFFFFFF) ext)
                     *pcl-cache-dir*)))

(defun p-cache-valid-p (source-path cache-path)
  "Check if cached file is valid: exists, newer than source, not expired."
  (when *pcl-skip-cache*
    (return-from p-cache-valid-p nil))
  (when (not (probe-file cache-path))
    (return-from p-cache-valid-p nil))
  (let* ((source-mtime (file-write-date source-path))
         (cache-mtime (file-write-date cache-path))
         (cache-age (- (get-universal-time) cache-mtime)))
    (and (> cache-mtime source-mtime)
         (< cache-age *pcl-cache-max-age*))))

(defun p-cleanup-old-cache ()
  "Remove cache files older than max age."
  (let ((cutoff (- (get-universal-time) *pcl-cache-max-age*)))
    (dolist (file (directory (merge-pathnames "*.*" *pcl-cache-dir*)))
      (when (< (file-write-date file) cutoff)
        (ignore-errors (delete-file file))))))

;;; --- Module Transpilation ---

(defun p-transpile-file (source-path)
  "Transpile a Perl file to Common Lisp code by calling pl2cl.
   Uses --module flag to skip preamble (for dynamic module loading).
   Returns the transpiled code as a string, or nil on failure."
  (unless *pcl-pl2cl-path*
    (error "pl2cl path not set - cannot transpile ~A" source-path))
  (let ((output (make-array 0 :element-type 'character
                            :adjustable t :fill-pointer 0)))
    (with-output-to-string (s output)
      (let ((proc (sb-ext:run-program
                   "perl"
                   (list (namestring *pcl-pl2cl-path*)
                         "--module"  ; Skip preamble for module loading
                         (namestring source-path))
                   :output s
                   :error *error-output*
                   :wait t
                   :search t)))
        (unless (zerop (sb-ext:process-exit-code proc))
          (return-from p-transpile-file nil))))
    (when (> (length output) 0)
      output)))

(defun p-ensure-transpiler ()
  "Return the live transpiler process, starting or restarting it if needed."
  (unless *pcl-pl2cl-path*
    (error "pl2cl path not set - cannot start transpiler server"))
  (when (or (null *p-transpiler-process*)
            (not (sb-ext:process-alive-p *p-transpiler-process*)))
    (when *p-transpiler-process*
      (ignore-errors (sb-ext:process-close *p-transpiler-process*)))
    (setf *p-transpiler-process*
          (sb-ext:run-program
           "perl"
           (list (namestring *pcl-pl2cl-path*) "--server")
           :input  :stream
           :output :stream
           :error  nil
           :wait   nil
           :search t
           :external-format :utf-8)))
  *p-transpiler-process*)

(defun p-transpile-string (perl-code pkg-name &optional capture-names features)
  "Transpile a Perl string to CL code via the persistent pl2cl server.
   CAPTURE-NAMES are the caller's in-scope lexical names (the keys of the
   eval's capture alist).  The compiler needs them for the ONE question whose
   answer they change (#296-B1): an exception-partition name the alist carries
   is the caller's `my $a`, so it must compile as that captured lexical rather
   than as the dynamically-bound special.  Everything else resolves at RUNTIME
   through p-eval-lex-lookup and is unaffected by this list.
   FEATURES are the perl features in effect at the eval site (#364), which
   decide how the TEXT lexes — `try`/`signatures` today.
   Returns the CL text string, or signals an error on failure."
  (let* ((proc     (p-ensure-transpiler))
         (in       (sb-ext:process-input  proc))
         (out      (sb-ext:process-output proc))
         (code-len (length perl-code)))
    ;; Send request: pkg\n captures\n features\n char-count\n perl-code
    (write-string pkg-name in)
    (write-char #\Newline in)
    (write-string (format nil "~{~A~^ ~}" capture-names) in)
    (write-char #\Newline in)
    (write-string (format nil "~{~A~^ ~}" features) in)
    (write-char #\Newline in)
    (write-string (princ-to-string code-len) in)
    (write-char #\Newline in)
    (write-string perl-code in)
    (finish-output in)
    ;; Read response: status\n char-count\n body
    (let* ((status   (read-line out))
           (resp-len (parse-integer (read-line out)))
           (resp-buf (make-string resp-len)))
      (read-sequence resp-buf out)
      (if (string= status "ok")
          resp-buf
          ;; The transpiler's own message IS the error — do not wrap it in a
          ;; host-shaped prefix.  For `eval "..."` this text becomes $@, and
          ;; E4.1 §5a.3 requires that to read as an ordinary Perl error the
          ;; program can trap ("PCL: unsupported in string eval: …"), not as
          ;; a note from a subprocess the Perl program never asked about.
          (error "~A" (string-right-trim '(#\Newline) resp-buf))))))

;;; --- Module Loading ---

(defun p-load-module-cached (source-path)
  "Load a Perl module with caching. Returns t on success."
  (p-ensure-cache-dir)
  ;; A loaded module sets *pcl-current-package* via its own `package` statements;
  ;; rebind here so those changes don't leak into the caller's notion of the
  ;; current package (which caller()/overload::import read).  The orig-case name
  ;; map it populates is a separate global hash and intentionally persists.
  (let ((*pcl-current-package* *pcl-current-package*)
        (cache-path (p-compute-cache-path source-path (not *pcl-cache-fasl*))))
    (cond
      ;; Cache hit
      ((p-cache-valid-p source-path cache-path)
       ;; Muffle "package at variance" warnings: p-sub's eval-when :compile-toplevel
       ;; shadow calls run during compile-file, then defpackage re-runs at load time
       ;; and sees the extra shadow — harmless but noisy.
       (handler-bind ((warning #'muffle-warning))
         (load cache-path))
       t)
      ;; Cache miss - transpile and cache
      (t
       (let ((lisp-code (p-transpile-file source-path)))
         (unless lisp-code
           (error "Failed to transpile ~A" source-path))
         (if *pcl-cache-fasl*
             ;; FASL mode: compile to PID-unique temp files, then rename
             ;; atomically to cache-path.  Multiple parallel workers may race
             ;; here; rename(2) is atomic within a filesystem so the last
             ;; writer wins but the file is always consistent.
             (let* ((pid       (sb-posix:getpid))
                    (base-name (pathname-name cache-path))
                    (pid-name  (format nil "~A-~A" base-name pid))
                    (temp-lisp (make-pathname :defaults cache-path
                                              :name pid-name :type "lisp"))
                    (temp-fasl (make-pathname :defaults cache-path
                                              :name pid-name :type "fasl")))
               (with-open-file (out temp-lisp
                                    :direction :output
                                    :if-exists :supersede)
                 (write-string lisp-code out))
               (let ((compiled (handler-bind ((warning #'muffle-warning))
                                 (compile-file temp-lisp :output-file temp-fasl
                                               :print nil :verbose nil))))
                 (ignore-errors (delete-file temp-lisp))
                 (unless compiled
                   (error "compile-file failed for ~A" temp-lisp))
                 ;; Atomic replace: safe even if another worker beat us here.
                 (rename-file temp-fasl cache-path)
                 (p-cleanup-old-cache)
                 (handler-bind ((warning #'muffle-warning))
                   (load cache-path))
                 t))
             ;; Lisp mode: cache .lisp — via a PID-unique temp + atomic rename,
             ;; exactly like the FASL branch above.  This is the DEFAULT branch
             ;; and it used to write cache-path in place: SBCL's :supersede
             ;; truncates and writes the real file (measured s339), so a second
             ;; worker whose p-cache-valid-p saw the fresh mtime would `load` a
             ;; HALF-WRITTEN module and die.  That is the cold-cache sweep race
             ;; of task #215 — one non-atomic copy of a mechanism its sibling
             ;; already got right.
             (let* ((pid       (sb-posix:getpid))
                    (temp-lisp (make-pathname
                                :defaults cache-path
                                :name (format nil "~A-~A"
                                              (pathname-name cache-path) pid))))
               (with-open-file (out temp-lisp
                                    :direction :output
                                    :if-exists :supersede)
                 (write-string lisp-code out))
               (rename-file temp-lisp cache-path)
               (p-cleanup-old-cache)
               (handler-bind ((warning #'muffle-warning))
                 (load cache-path))
               t)))))))

(defun p-find-module-package (module-name)
  "Find CL package for a Perl module.
   Tries: uppercase name, exact-case name (for Foo::Bar packages)."
  (or (find-package (perl-pkg-to-cl-pkg-name module-name))
      (find-package (%pcl-invert-case module-name))
      (find-package module-name)))

(defun p-perl-symbol-to-cl-name (sym-name)
  "Convert Perl symbol name to CL symbol name.
   '$x' -> '$X', '@arr' -> '@ARR', '%hash' -> '%HASH', 'func' -> 'PL-FUNC'
   Note: CL uppercases symbols by default."
  ;; Unbox if sym-name is a box (from @EXPORT array with l-value storage)
  (let* ((name (unbox sym-name))
         (first-char (if (plusp (length name))
                         (char name 0)
                         nil)))
    (%pcl-invert-case
     (cond
       ((eql first-char #\$) name)
       ((eql first-char #\@) name)
       ((eql first-char #\%) name)
       (t (format nil "pl-~A" name))))))

(defun p-import-perl-symbol (sym-name from-pkg to-pkg)
  "Import a Perl symbol from FROM-PKG to TO-PKG.
   For functions: sets fdefinition in TO-PKG so compiled lambdas that
   already interned the symbol (before the import) get the right binding.
   For variables: shadowing-import to make the binding accessible."
  (let* ((cl-name (p-perl-symbol-to-cl-name sym-name))
         (from-sym (find-symbol cl-name from-pkg)))
    (when from-sym
      (let ((name (unbox sym-name)))
        (cond
          ;; Variable sigil ($ @ %): the compiled code in TO-PKG already
          ;; interned a package-local symbol for this name, so shadowing-import
          ;; (which uninterns the conflicting local) would orphan that captured
          ;; symbol (-> "#:%CONFIG unbound").  Mirror the function path: bind the
          ;; already-interned local symbol to share FROM-SYM's value — the same
          ;; box/hash/array container, so reads and in-place mutations alias.
          ((and (stringp name) (plusp (length name))
                (member (char name 0) '(#\$ #\@ #\%)))
           (let ((to-sym (intern cl-name to-pkg)))
             (%p-ensure-storage to-sym)
             (when (boundp from-sym)
               (setf (symbol-value to-sym) (symbol-value from-sym)))))
          ;; Function: set fdefinition in TO-PKG so already-compiled
          ;; lambdas with an interned-but-unbound local symbol get the fn.
          ((fboundp from-sym)
           (let ((to-sym (intern cl-name to-pkg)))
             (setf (fdefinition to-sym) (fdefinition from-sym))))
          ;; Symbol exists but no function: still do shadowing-import
          (t
           (shadowing-import from-sym to-pkg)))))))

(defun %p-get-export-list (pkg var-name)
  "Helper: get a vector-valued package variable as a list, or nil."
  (let ((sym (find-symbol var-name pkg)))
    (when (and sym (boundp sym))
      (let ((val (symbol-value sym)))
        (when (and val (vectorp val))
          (coerce val 'list))))))

(defun %p-expand-import-tags (imports pkg)
  "Expand export-tag items (starting with ':') in IMPORTS list using %EXPORT_TAGS.
   ':DEFAULT' expands to @EXPORT; ':ALL' expands to @EXPORT_OK; ':TAG' looks up
   %EXPORT_TAGS{TAG}.  Plain names are kept as-is."
  (let ((result '()))
    (dolist (item imports)
      (let ((name (unbox item)))
        (if (and (stringp name) (plusp (length name)) (char= (char name 0) #\:))
            (let ((tag (subseq name 1)))
              (cond
                ((string= tag "DEFAULT")
                 (let ((lst (%p-get-export-list pkg "@export")))
                   (when lst (setf result (append result lst)))))
                ((string= tag "ALL")
                 (let ((lst (%p-get-export-list pkg "@export_ok")))
                   (when lst (setf result (append result lst)))))
                (t
                 ;; Look up %EXPORT_TAGS{tag}
                 (let ((tags-sym (find-symbol "%export_tags" pkg)))
                   (when (and tags-sym (boundp tags-sym))
                     (let* ((tags-hash (symbol-value tags-sym))
                            (tag-val (when (hash-table-p tags-hash)
                                       (or (gethash tag tags-hash)
                                           (gethash (string-upcase tag) tags-hash)))))
                       (when (and tag-val (vectorp tag-val))
                         (setf result (append result (coerce tag-val 'list))))))))))
            ;; Plain name: keep as-is
            (push name result))))
    (nreverse result)))

(defun p-import-exports (module-name to-pkg &optional specific-imports)
  "Import symbols from module's @EXPORT (or specific list) into TO-PKG.
   Handles export tags like :DEFAULT, :ALL, :TAGNAME."
  (let ((pkg (p-find-module-package module-name)))
    (when pkg
      (let* ((raw-imports (or specific-imports
                              (%p-get-export-list pkg "@export")))
             (imports (if specific-imports
                          (%p-expand-import-tags raw-imports pkg)
                          raw-imports)))
        (dolist (sym-name imports)
          (p-import-perl-symbol sym-name pkg to-pkg))))))

(defun %p-module-can-import-p (module-name)
  "True if MODULE has an `import` method resolvable through its MRO — its own
   `sub import` (Test::More, Moo, …) OR an inherited one (the usual
   `our @ISA = ('Exporter')` → Exporter::import, now a real sub in lib/Exporter.pm).
   When true, `use` dispatches to Foo->import(@args), exactly like Perl; when
   false (a shim that declares @EXPORT but inherits nothing), `use` falls back to
   the @EXPORT-copy convenience."
  (p-true-p (ignore-errors (p-can module-name "import"))))

(defun %p-do-import (module-name to-pkg import-args)
  "Perform the import half of `use Module LIST`.  Perl: `use Foo X` evaluates X to
   a list and calls Foo->import(X).  IMPORT-ARGS is that evaluated list (a vector)
   or :default for bare `use Foo;` (import with no args → default exports).
   Dispatch: an import method (own or inherited Exporter::import) → call it; else
   the @EXPORT-copy convenience for shims that declare @EXPORT but inherit nothing."
  (let ((args (cond ((eq import-args :default) :default)
                    ((and (vectorp import-args) (not (stringp import-args)))
                     (coerce import-args 'list))
                    ((null import-args) nil)
                    (t (list import-args)))))
    (if (%p-module-can-import-p module-name)
        ;; Foo->import(@args).  :default = called with no args.  Perl calls import
        ;; with caller = the package containing the `use`; TO-PKG is exactly that
        ;; lexical package (the loader's *package* at the use site).  Bind
        ;; *pcl-current-package* to it so caller() inside import resolves correctly
        ;; — *pcl-current-package* otherwise lags during nested module loads (the
        ;; runtime p-set-current-package is emitted after the package's use stmts).
        (let ((*pcl-current-package* (pcl-pkg-perl-name to-pkg)))
          (apply #'p-method-call module-name "import"
                 (if (eq args :default) nil args)))
        ;; @EXPORT-copy convenience: names are strings; :default = default @EXPORT.
        (p-import-exports module-name to-pkg
                          (if (eq args :default) nil (mapcar #'to-string args))))))

(defparameter *p-xs-only-modules*
  '("XSLoader" "DynaLoader" "Carp::Heavy")
  "Modules that use XS/C code and cannot be transpiled. Skip loading them.")

(defparameter *p-pcl-provided-modules*
  '("Test::More" "Test::Simple" "Test2::Bundle::More")
  "Modules whose interface PCL supplies INTERNALLY (here: the Test::More TAP API
  lives in cl/pcl-test.lisp).  `use`-ing them must NOT load the real .pm — the
  real Test::More is the Test2 stack, which depends on XS internals
  (Test2::API::Instance) PCL cannot run.  Instead, `use Test::More` loads the
  TAP layer ON DEMAND (p-ensure-test-lib), so a non-test program never pulls in
  the test infrastructure, and a .t file is self-contained.")

(defparameter *p-pragma-modules*
  '("strict" "warnings" "feature" "utf8" "open" "bytes"
    "locale" "integer" "re" "overloading" "warnings::register")
  "Lexical pragmas: they manipulate compile-time hint bitmasks ($^H,
  ${^WARNING_BITS}) that PCL does not model.  Bareword `use strict` is already a
  parser no-op, but a STRING require — the `if` pragma does `require \"strict.pm\"`
  — or an explicit `require strict` reaches p-use, where loading the real .pm
  would hit `STRICT::$^H unbound`.  So skip the load entirely (the import/unimport
  methods are separately stubbed as no-ops below).")

(defvar *pcl-test-lib-loaded* nil
  "T once cl/pcl-test.lisp has been loaded — by the harness preloading it, or
  on demand from `use Test::More`.  Guards against re-loading.")

(defun p-ensure-test-lib ()
  "Load the Test::More TAP layer (cl/pcl-test.lisp) on demand, exactly once.
  This is what lets the runner stop preloading the test infrastructure for every
  program: it is pulled in only when a script `use`s Test::More.  Idempotent —
  if the harness already loaded it (pl-ok is fbound) this is a no-op."
  (unless (or *pcl-test-lib-loaded* (fboundp 'pl-ok))
    (let ((path (and *pcl-runtime-directory*
                     (merge-pathnames "pcl-test.lisp" *pcl-runtime-directory*))))
      (when (and path (probe-file path))
        (load path))))
  (setf *pcl-test-lib-loaded* t))

(defun p-use (module-name &key (import-args :default) (do-import t) into)
  "Perl use - load module at compile time and import symbols.
   MODULE-NAME: 'Foo::Bar' or 'Foo/Bar.pm'
   IMPORT-ARGS: the evaluated import list (a vector) — `use Foo X` makes X a Perl
   list — or :default for a bare `use Foo;` (import with no args).
   DO-IMPORT: when NIL, load the module but do NOT call its ->import (this is the
   `require Foo` semantics — load only, no symbol import).
   INTO: Perl package name to import INTO.  Normally the import target is the
   package the form is read in (*package*), which is right because a top-level
   `package Foo;` opens its own section.  A `package` inside a do{}/eval{} block
   is only a RUNTIME switch, though, so a `use` hoisted out of such a block would
   import into the enclosing package instead — the codegen names the package
   explicitly there."
  ;; Skip XS-only modules that cannot be transpiled
  (when (member module-name *p-xs-only-modules* :test #'string=)
    (return-from p-use t))
  ;; Modules PCL provides internally (Test::More TAP API): don't load the real
  ;; .pm; load PCL's TAP layer on demand instead (no-op if already loaded).
  (when (member module-name *p-pcl-provided-modules* :test #'string=)
    (p-ensure-test-lib)
    ;; `use Test::More tests => N` / 'no_plan' / skip_all => REASON: the import
    ;; list IS the plan for these modules, so it must reach the TAP layer.
    ;; Dropping it published a plan-less TAP stream (task #275).  The handler
    ;; lives in cl/pcl-test.lisp — that is where Test::More's semantics belong,
    ;; and it is only loadable once p-ensure-test-lib has run.
    (when (and do-import (not (eq import-args :default)))
      (funcall (intern "%TEST-IMPORT" :pcl) import-args))
    (return-from p-use t))
  ;; Lexical pragmas (strict/warnings/feature/...): never load the core .pm —
  ;; PCL doesn't model the hint bitmasks they touch.  Reached only via a string
  ;; require (the `if` pragma) or an explicit `require strict`.
  (when (member module-name *p-pragma-modules* :test #'string-equal)
    (return-from p-use t))
  (let ((rel-path (p-module-to-path module-name))
        (caller-pkg (or (and into (%pcl-find-package into)) *package*)))
    ;; Already loaded?
    (when (gethash rel-path *p-inc-table*)
      ;; Still import for repeated use statements (but not for bare require)
      (when do-import
        (%p-do-import module-name caller-pkg import-args))
      (return-from p-use t))
    ;; Circular dependency?
    (when (member rel-path *p-loading-modules* :test #'string=)
      (warn "Circular dependency detected: ~A" rel-path)
      (return-from p-use t))
    ;; Find module in @INC
    (let ((abs-path (p-find-module-in-inc rel-path)))
      (unless abs-path
        (error "Can't locate ~A in @INC (@INC contains: ~{~A~^ ~})"
               rel-path (map 'list #'to-string @INC)))
      ;; Load with circular detection
      (let ((*p-loading-modules* (cons rel-path *p-loading-modules*)))
        (p-load-module-cached abs-path))
      ;; Update %INC
      (setf (gethash rel-path *p-inc-table*) abs-path)
      ;; Import symbols from module (skipped for bare require)
      (when do-import
        (%p-do-import module-name caller-pkg import-args))
      t)))

(defun p-require (module-name)
  "Perl require - load module at runtime WITHOUT calling its ->import.
   `require Foo` only loads; `use Foo` = require + import.  Calling import here
   would re-run the module's import into the current package, which (for modules
   like Moo::Role whose import has a guard) is both wrong and can be fatal."
  (p-use module-name :do-import nil))

(defun p-require-parent (module-name)
  "Implicit require performed by `use parent`/`use base` (Perl does
   `require $_` for each parent unless -norequire).  Loading the parent makes
   its package and methods exist — which is also what lets the generated
   (defclass child (Parent::class) ...) form READ, since Parent:: must be a
   real package.  Unlike a bare require this is NON-FATAL: the parent may be an
   inline same-file package (no .pm to find) or simply unavailable, and in
   neither case should we abort.  Returns T if the module was loaded."
  (handler-case (progn (p-use module-name) t)
    (error () nil)))

(defun %p-parse-require-version (ver)
  "Parse a require-VERSION argument to a (major minor patch) triple.
   VER: a number (decimal form, 5.00563 = v5.5.630) or the literal source
   text of a version literal (\"v5.5.630\", \"10.0.2\", \"5.005_63\")."
  (if (numberp ver)
      (let* ((maj (floor ver))
             (milli (floor (+ (* (- ver maj) 1000) 1/1000000)))
             (patch (floor (+ (* (- (* (- ver maj) 1000) milli) 1000)
                              1/1000))))
        (values maj milli patch))
      ;; Text: a v-prefix or >=2 dots is a literal component triple
      ;; ("v5.5.630", "v10.2", "10.0.2"); otherwise it reads as a decimal
      ;; number ("10.2" = v10.200.0, "5.005_63" = v5.5.630).
      (let* ((s (remove #\_ (to-string ver)))
             (v-p (and (> (length s) 0) (char-equal (char s 0) #\v)))
             (body (if v-p (subseq s 1) s)))
        (if (or v-p (>= (count #\. body) 2))
            (let ((parts (loop with start = 0
                               for dot = (position #\. body :start start)
                               collect (parse-integer
                                        body :start start
                                        :end (or dot (length body)))
                               while dot do (setf start (1+ dot)))))
              (values (or (first parts) 0) (or (second parts) 0)
                      (or (third parts) 0)))
            (%p-parse-require-version (to-number body))))))

(defun p-require-version (ver)
  "Perl `require VERSION` — die when VERSION exceeds the running version,
   else return 1.  Emitted by codegen for version-literal requires; also the
   runtime path for `require $v` with a numeric $v."
  (multiple-value-bind (maj min pat) (%p-parse-require-version ver)
    (multiple-value-bind (cmaj cmin cpat)
        (%p-parse-require-version (to-number (unbox |$]|)))
      (if (or (> maj cmaj)
              (and (= maj cmaj) (> min cmin))
              (and (= maj cmaj) (= min cmin) (> pat cpat)))
          (p-die (format nil "Perl v~D.~D.~D required--this is only v~D.~D.~D, stopped"
                         maj min pat cmaj cmin cpat))
          1))))

(defun p-require-file (path)
  "Perl require with a string/path argument.
   A `.pm` path (Foo/Bar.pm) is a MODULE require: resolve through @INC and the
   lib/ shims exactly like a bareword `require Foo::Bar`.  This is what the `if`
   pragma's `use if COND, MODULE` does — it builds \"MODULE.pm\" (`::`->`/`) and
   string-requires it; routing through p-require also makes the XS-only and
   PCL-provided (Test::More) shortcuts in p-use fire, which key on the `::` name.
   A non-.pm path (./test.pl) is loaded literally, relative to the current dir,
   with an @INC fallback (Perl searches @INC for all string requires)."
  (let ((path-str (unbox path)))
    ;; `require $v` with a NUMERIC $v is a version check, not a file load
    ;; (a numeric-looking STRING still names a file, matching perl).
    (when (numberp path-str)
      (return-from p-require-file (p-require-version path-str)))
    ;; Check %INC to avoid reloading (Perl keys %INC by the string used).
    (when (gethash path-str *p-inc-table*)
      (return-from p-require-file t))
    ;; A `.pm` path is a module require — delegate to the bareword machinery.
    (when (and (>= (length path-str) 3)
               (string= path-str ".pm" :start1 (- (length path-str) 3)))
      (let* ((bare (subseq path-str 0 (- (length path-str) 3)))
             (module-name (with-output-to-string (s)
                            (loop for ch across bare
                                  do (if (char= ch #\/)
                                         (write-string "::" s)
                                         (write-char ch s))))))
        (p-require module-name)
        ;; p-use already records the rel-path (= path-str for a .pm) in %INC;
        ;; set it too so the guard above fires on a literal repeat.
        (setf (gethash path-str *p-inc-table*) path-str)
        (return-from p-require-file t)))
    ;; Non-.pm: literal file load (e.g. ./test.pl), cwd-relative, @INC fallback.
    (let ((abs-path (if (char= (char path-str 0) #\/)
                        path-str
                        (let ((cwd-path (merge-pathnames
                                         path-str
                                         (truename *default-pathname-defaults*))))
                          (if (probe-file cwd-path)
                              cwd-path
                              (or (p-find-module-in-inc path-str) cwd-path))))))
      (unless (probe-file abs-path)
        (error "Can't locate ~A" path-str))
      (p-load-module-cached abs-path)
      (setf (gethash path-str *p-inc-table*) (namestring abs-path))
      t)))

;;; ============================================================
;;; List Functions
;;; ============================================================

(defun %p-collect-list (&rest items)
  "Collect &rest args into a flat vector.
   Pl-boxes wrapping vectors (@arrays) are flattened into individual elements.
   p-flatten-markers (from ->import/->unimport empty returns) contribute 0 elements.
   Used by p-map and p-grep to handle both (fn @arr) and (fn a b c) forms."
  (let ((result (make-array 8 :adjustable t :fill-pointer 0)))
    (dolist (item items)
      (cond
        ((p-flatten-marker-p item)
         (let ((src (p-flatten-marker-array item)))
           (loop for j from 0
                 for x across src
                 ;; HOLE slots spread as defelem aliases tied to the source
                 ;; array — grep/map write through $_ like perl (see p-flatten-args)
                 do (vector-push-extend
                     (if (null x) (%p-defelem-box src j) x)
                     result))))
        (t
         (let ((val (unbox item)))
           (cond
             ((and (vectorp val) (not (stringp val)))
              (loop for j from 0
                    for x across val
                    do (vector-push-extend
                        (if (null x) (%p-defelem-box val j) x)
                        result)))
             ;; Raw %hash (not a ref): spread to key/value pairs in list context.
             ((and (hash-table-p val) (not (p-box-p item)))
              (dolist (kv (%p-hash-keyval-list val)) (vector-push-extend kv result)))
             (t (vector-push-extend item result)))))))
    result))

(defun %p-map-copy-scalar (r)
  "Copy a simple scalar box to prevent aliasing in map results.
   When a map block ends with an lvalue like ($y .= $x), it returns the box $y.
   If we store the box itself, later mutations to $y corrupt the map result.
   Reference types (hash/array/code) and blessed objects are NOT copied."
  (if (and (p-box-p r)
           (not (p-box-class r))
           (let ((v (p-box-value r)))
             (not (or (hash-table-p v) (and (vectorp v) (not (stringp v))) (functionp v)
                      (p-box-p v) (p-typeglob-p v)))))
      (make-p-box (unbox r))
      r))

(defun p-grep (fn &rest items)
  "Perl grep - fn receives item as $_ parameter.
   Accepts (fn @array) or (fn elem1 elem2 ...) or mixed."
  (let* ((arr (apply #'%p-collect-list items))
         (result (make-array 0 :adjustable t :fill-pointer 0)))
    ;; $_ must be a stable box so \$_ aliases consistently within an iteration
    ;; ([perl #78194]). Array/ref elements are already boxes; a literal-scalar
    ;; element (from the (fn a b c) form) is raw — box it once per iteration.
    (loop for item across arr
          for slot = (if (p-box-p item) item (make-p-box item))
          when (p-true-p (let ((*wantarray* nil)) (funcall fn slot)))
          do (vector-push-extend slot result))
    result))

(defun p-map (fn &rest items)
  "Perl map - fn receives item as $_ parameter.
   Runs block in list context; flattens per-iteration vectors into result.
   Accepts (fn @array) or (fn elem1 elem2 ...) or mixed.
   CL nil from the block means empty-list (0 elements), not undef."
  (let* ((arr (apply #'%p-collect-list items))
         (result (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for item across arr
          for slot = (if (p-box-p item) item (make-p-box item))  ; stable $_ box, [perl #78194]
          do (let ((r (let ((*wantarray* t)) (funcall fn slot))))
               (cond
                 ((and (vectorp r) (not (stringp r)))
                  (loop for e across r do (vector-push-extend e result)))
                 ;; CL nil means "return empty list" (e.g. from (progn) or if-without-else
                 ;; evaluating to false). Perl: map { () } produces 0 elements.
                 ((null r) nil)
                 (t (vector-push-extend (%p-map-copy-scalar r) result)))))
    result))

(defun p-sort-get-fn (val)
  "Get a CL function from a Perl scalar sort comparator (coderef, string, glob, or glob ref).
   Handles: \\&foo (double-boxed fn), *foo (typeglob), \\*foo (box of typeglob), 'name' (string)."
  (let ((v (unbox val)))
    (cond
      ;; Direct function (unboxed code ref)
      ((functionp v) v)
      ;; Box containing function: \&foo stores box(box(fn)) after p-backslash
      ((and (p-box-p v) (functionp (p-box-value v)))
       (p-box-value v))
      ;; Typeglob *foo — extract CODE slot
      ((p-typeglob-p v)
       (let ((code (p-glob-slot v "CODE")))
         (and code (unbox code))))
      ;; Box containing typeglob: \*foo stores box(box(typeglob))
      ((and (p-box-p v) (p-typeglob-p (p-box-value v)))
       (let ((code (p-glob-slot (p-box-value v) "CODE")))
         (and code (unbox code))))
      ;; String or anything else — look up sub by name in current package
      (t (p-get-coderef val)))))

(defun p-sort (&rest args)
  "Perl sort - sort a list with optional comparator function.
   (p-sort list)         - sort single array/list lexically
   (p-sort fn list...)   - sort with comparator fn (lambda or unboxed code ref)
   (p-sort a b c ...)    - sort concatenated multi-arg list lexically"
  (if (null args)
      (make-array 0 :adjustable t :fill-pointer 0)
      (let* ((first-val (unbox (first args)))
             (has-fn (functionp first-val)))
        (if has-fn
            ;; Comparator form: (p-sort fn list...)
            (let* ((fn first-val)
                   (raw (apply #'%p-collect-list (rest args)))
                   (result (if (typep raw 'sequence)
                               (copy-seq raw)
                               (make-array 0 :adjustable t :fill-pointer 0))))
              ;; Box raw literal elements so \$a/\$b alias stably ([perl #78194]).
              (stable-sort result (lambda (a b)
                                    (< (to-number
                                        (funcall fn
                                                 (if (p-box-p a) a (make-p-box a))
                                                 (if (p-box-p b) b (make-p-box b))))
                                       0))))
            ;; No comparator: flatten all args and sort lexically (stable)
            (let* ((raw (apply #'%p-collect-list args))
                   (result (if (typep raw 'sequence)
                               (copy-seq raw)
                               (make-array 0 :adjustable t :fill-pointer 0))))
              (stable-sort result (lambda (a b)
                                    (string< (to-string a) (to-string b)))))))))

(defun p-reverse (&rest items)
  "Perl reverse: in list context reverses element order; in scalar context
   concatenates all items into a string and reverses the characters."
  (if (eq *wantarray* t)
      ;; List context: reverse element order, preserving nil (deleted) slots
      (let* ((arr (apply #'%p-collect-list items))
             (result (copy-seq arr)))
        (nreverse result))
      ;; Scalar context: join all items into a string and reverse characters
      (let ((str (with-output-to-string (s)
                   (dolist (item items)
                     (let ((val (unbox item)))
                       (if (and (vectorp val) (not (stringp val)))
                           (loop for x across val do (write-string (to-string x) s))
                           (write-string (to-string item) s)))))))
        (nreverse (copy-seq str)))))

(defun p-join (sep &rest items)
  "Perl join(SEP, LIST) - joins elements with separator.
   Handles both (join SEP @array) and (join SEP elem1 elem2 ...).
   Arrays/vectors and hashes in the argument list are flattened."
  (let* (;; Warn for undef separator (Perl warns regardless of list length).
         ;; Skip for tied sep to avoid premature FETCH before item-count check.
         (_ (when (and (not (and (p-box-p sep) (p-tie-proxy-p (p-box-value sep))))
                       (not (%pcl-definedp sep)))
              (p-warn (format nil "Use of uninitialized value in join or string~%"))))
         ;; Pre-count items WITHOUT calling FETCH (to decide sep evaluation)
         ;; Tied scalars in items are counted as 1 without fetching
         (item-count (loop for item in items
                           for raw = (if (p-box-p item) (p-box-value item) item)
                           if (and (vectorp raw) (not (stringp raw)))
                           sum (length raw)
                           else if (hash-table-p raw)
                           sum (* 2 (%p-hash-user-count raw))
                           else if (and (listp raw) raw)
                           sum (length raw)
                           else sum 1))
         ;; Perl optimization: sep is NOT evaluated when ≤1 elements
         ;; (FETCH not called on tied separator — matches Perl's join optimization)
         ;; For ≥2 elements, sep is evaluated FIRST (Perl evaluation order)
         (s (when (> item-count 1) (to-string sep)))
         ;; Now flatten and evaluate elements (FETCH called for tied element vars).
         ;; Warn for each undef element (Perl uses-of-uninitialized-value warning).
         (elements (loop for item in items
                         for val = (unbox item)
                         if (and (vectorp val) (not (stringp val)))
                         append (coerce val 'list)
                         else if (hash-table-p val)
                         append (%p-hash-keyval-list val)
                         else if (and (listp val) val)
                         append val
                         else
                         collect (progn
                                   (when (or (null val) (eq val *p-undef*))
                                     (p-warn (format nil "Use of uninitialized value in join or string~%")))
                                   val))))
    (declare (ignore _))
    (if s
        (format nil (concatenate 'string "~{~A~^" s "~}")
                (mapcar #'to-string elements))
        (if elements (to-string (car elements)) ""))))

(defun %perl-space-char-p (c)
  "T if C is whitespace under Perl's Unicode \\p{White_Space} property — the set that
   `\\s` and `split ' '` match under /u.  PCL strings are always Unicode (no per-scalar
   UTF8 flag, no `use bytes`), so split ' ' always uses this full set rather than the
   ASCII-only subset.  This is what lets `split ' '` find \\xA0/\\x85/\\x{2000}.. as
   separators (RT #130907); the inverse /d byte-mode behaviour is not representable and
   is documented not-supported."
  (or (char= c #\Space) (char= c #\Tab) (char= c #\Newline)
      (char= c #\Return) (char= c #\Page)              ; \x20 \x09 \x0A \x0D \x0C
      (let ((cp (char-code c)))
        (or (= cp #x0B)                                ; LINE TABULATION (vtab)
            (= cp #x85)                                ; NEXT LINE (NEL)
            (= cp #xA0)                                ; NO-BREAK SPACE
            (= cp #x1680)                              ; OGHAM SPACE MARK
            (<= #x2000 cp #x200A)                      ; EN QUAD .. HAIR SPACE
            (= cp #x2028)                              ; LINE SEPARATOR
            (= cp #x2029)                              ; PARAGRAPH SEPARATOR
            (= cp #x202F)                              ; NARROW NO-BREAK SPACE
            (= cp #x205F)                              ; MEDIUM MATHEMATICAL SPACE
            (= cp #x3000)))))                          ; IDEOGRAPHIC SPACE

(defun p-split (pattern str &optional limit)
  "Perl split - split string by pattern.
   Note: pattern and str are NOT optional here - PExpr.pm adds defaults
   (pattern=' ', str=$_) at parse time so codegen always provides both."
  (let* ((s (to-string str))
         ;; Unbox pattern (may be stored in a variable as a p-box)
         (pattern (if (p-box-p pattern) (p-box-value pattern) pattern))
         (limit-num (if limit (truncate (to-number limit)) nil))
         (keep-trailing (and limit-num (/= limit-num 0)))
         (max-fields (if (and limit-num (> limit-num 0)) limit-num nil))
         (result (make-array 0 :adjustable t :fill-pointer 0)))
    ;; Empty input string always gives empty result (no fields)
    (unless (zerop (length s))
      (cond
        ;; Regex pattern from p-regex or p-qr (possibly stored in variable)
        ((p-regex-match-p pattern)
         (let* ((raw-pat (p-regex-match-pattern pattern))
                (modifiers (p-regex-match-modifiers pattern))
                (ppcre-options (build-ppcre-options modifiers))
                ;; Perl special case: split /^/ is treated as split /^/m
                (pat (if (and (string= raw-pat "^") (not (getf modifiers :m)))
                         "(?m)^"
                         raw-pat))
                ;; CL-PPCRE: 0 removes trailing empty, large number keeps them
                ;; Perl: limit=0/nil removes, limit<0 keeps, limit>0 is max fields
                (ppcre-limit (cond (max-fields max-fields)    ; limit > 0
                                   (keep-trailing 1000000)     ; limit < 0, keep trailing
                                   (t 0)))                     ; no limit, remove trailing
                (parts (if (zerop (length pat))
                           ;; Empty regex: split into characters with limit handling.
                           ;; Perl also matches at the end (giving trailing ""), so:
                           ;; - limit<0 (keep-trailing): all chars + ""
                           ;; - limit>0 and >= str len: all chars + ""
                           ;; - limit>0 and < str len: first (limit-1) chars + rest
                           ;; - no limit: just chars
                           (let* ((n (length s))
                                  (chars (loop for c across s collect (string c))))
                             (cond
                               ((and max-fields (<= max-fields n))
                                ;; Split at most max-fields: first (max-fields-1) chars
                                ;; individually, remainder as one final field
                                (append (subseq chars 0 (1- max-fields))
                                        (list (subseq s (1- max-fields)))))
                               (keep-trailing
                                ;; No binding limit (or limit > n): all chars + trailing ""
                                (append chars (list "")))
                               (t
                                ;; No limit: just individual chars
                                chars)))
                           ;; Non-empty pattern: use CL-PPCRE split
                           ;; Must create scanner first to apply modifiers (m, i, s, x)
                           ;; since cl-ppcre:split doesn't accept modifier keywords directly.
                           ;; Use :with-registers-p t so capture groups in pattern
                           ;; are included in results (Perl behavior)
                           (handler-case
                               (let ((scanner (%pcl-create-scanner pat ppcre-options)))
                                 (cl-ppcre:split scanner s :limit ppcre-limit :with-registers-p t))
                             (cl-ppcre:ppcre-syntax-error (e)
                               (warn "Regex syntax error in split: ~A" e)
                               (list s))))))
           (dolist (p parts)
             (vector-push-extend (or p *p-undef*) result))))
        ;; Special whitespace splitting: " " splits on runs of whitespace and strips
        ;; leading whitespace (Perl's awk-mode split ' ').  Uses the full Unicode
        ;; whitespace set via %perl-space-char-p (so \xA0/\x85/\x{2000}.. separate too).
        ;; Iterating the raw string and only opening a word on a non-space naturally
        ;; skips leading whitespace and collapses runs; trailing whitespace closes the
        ;; final word with nothing after it.
        ((and (stringp pattern) (string= pattern " "))
         (let ((in-word nil) (word-start 0))
           (loop for i from 0 below (length s)
                 for c = (char s i)
                 do (cond
                      ((and (not in-word) (not (%perl-space-char-p c)))
                       (setf in-word t word-start i))
                      ((and in-word (%perl-space-char-p c))
                       (when (or (null max-fields) (< (length result) (1- max-fields)))
                         (vector-push-extend (subseq s word-start i) result)
                         (setf in-word nil)))))
           (when in-word
             (vector-push-extend (subseq s word-start) result))))
        ;; Literal string pattern
        (t
         (let* ((pat (to-string pattern))
                (pat-len (length pat))
                (start 0))
           (if (zerop pat-len)
               ;; Empty pattern: split into characters
               (loop for c across s
                     for i from 0
                     do (if (and max-fields (>= i (1- max-fields)))
                            (progn (vector-push-extend (subseq s i) result) (return))
                            (vector-push-extend (string c) result)))
               ;; Normal literal pattern
               (loop
                (let ((pos (search pat s :start2 start)))
                  (if (and pos (or (null max-fields) (< (length result) (1- max-fields))))
                      (progn
                        (vector-push-extend (subseq s start pos) result)
                        (setf start (+ pos pat-len)))
                      (progn
                        (vector-push-extend (subseq s start) result)
                        (return))))))))) ; end cond
      ) ; end unless (zerop (length s))
    ;; Remove trailing empty fields unless limit specified
    (unless keep-trailing
      (loop while (and (> (length result) 0)
                       (zerop (length (aref result (1- (length result))))))
            do (vector-pop result)))
    result))

(defun %p-tick-package-seps (name)
  "Rewrite the Perl-4 `'` package separator to `::` in a symbolic name:
   A'B == A::B (still valid in perl 5.40, the oracle; deprecated 5.38).
   A `'` counts as a separator when a word character follows, matching
   perl's toker; any other apostrophe passes through unchanged."
  (if (find #\' name)
      (with-output-to-string (s)
        (loop for i from 0 below (length name)
              for ch = (char name i)
              do (if (and (char= ch #\')
                          (< (1+ i) (length name))
                          (let ((nx (char name (1+ i))))
                            (or (alpha-char-p nx) (digit-char-p nx) (char= nx #\_))))
                     (write-string "::" s)
                     (write-char ch s))))
      name))

(defun %p-resolve-sub-symbol (name)
  "Resolve a Perl sub-name string (\"foo\" or \"Pkg::foo\") to its CL symbol
   PKG::PL-FOO, or NIL if the package/symbol does not exist.  Shared by the
   symbolic-code-ref paths: &{$name}(...), defined/exists &{$name}.  An
   unqualified name resolves against the current CL package (MAIN -> main)."
  (let* ((name (%p-tick-package-seps (to-string name)))
         (sep-pos (search "::" name :from-end t))
         (perl-pkg (if sep-pos
                       (subseq name 0 sep-pos)
                       (let ((cpkg (package-name *package*)))
                         (if (string= cpkg "MAIN") "main" cpkg))))
         (bare-name (if sep-pos (subseq name (+ sep-pos 2)) name))
         (cl-pkg (find-package (perl-pkg-to-cl-pkg-name perl-pkg))))
    (when cl-pkg
      (find-symbol (%pcl-cl-sub-name bare-name) cl-pkg))))

(defun p-funcall-ref (ref &rest args)
  "Call a code reference or a symbolic sub name (no-strict-refs semantics)."
  (let ((fn (unbox ref)))
    ;; Double-unbox: blessed coderefs are stored as box(inner-box(lambda))
    (when (p-box-p fn)
      (setf fn (p-box-value fn)))
    (if (functionp fn)
        (apply fn args)
        ;; Not a function.  A wrong-kind referent ($hashref->()) is perl's
        ;; fatal — it used to reach the symbolic branch below and be reported
        ;; as "Undefined subroutine &main::HASH(0x1) called", i.e. its own
        ;; stringification read back as a sub name.  Off the fast path: a real
        ;; coderef call never evaluates this.
        (progn
          (when (%p-wrong-referent-p "CODE" fn) (%p-not-a-ref "CODE"))
          ;; Otherwise: treat as symbolic sub name (string/number).
          ;; Resolution is %p-resolve-sub-symbol's job — the ONE resolver all
          ;; symbolic-code-ref paths share (it had the multi-segment package
          ;; rule this function's inline copy lacked: |aa::bb| keeps case,
          ;; single-segment upcases).  The pkg/name split here only feeds the
          ;; die message.
          (let* ((name (%p-tick-package-seps (to-string fn)))
                 (sep-pos (search "::" name :from-end t))
                 (perl-pkg (if sep-pos
                               (subseq name 0 sep-pos)
                               (let ((cpkg (package-name *package*)))
                                 (if (string= cpkg "MAIN") "main" cpkg))))
                 (bare-name (if sep-pos (subseq name (+ sep-pos 2)) name))
                 (sym (%p-resolve-sub-symbol name))
                 (fn-val (when (and sym (fboundp sym)) (symbol-function sym))))
            (if fn-val
                (apply fn-val args)
                (p-die (format nil
                               "Undefined subroutine &~A::~A called at (eval 1) line 1.~%"
                               perl-pkg bare-name))))))))

;;; ============================================================
;;; Type Functions
;;; ============================================================

(defun p-backslash (val)
  "Perl reference operator \\$x - returns a box containing the referenced value.
   For scalars (boxes): returns a box containing the box (reference to scalar).
   For arrays/hashes/typeglobs/functions: wraps in a box as an opaque reference.
   For raw scalar values (integers, strings from \\scalar-expr): wraps in a fresh
   mutable box first, so the reference is mutable ($$ref += 10 works)."
  (cond
    ;; Scalar reference: box → box. Set is-ref so box-set knows this is a fresh
    ;; reference wrapper and preserves it (rather than unwrapping one level).
    ((p-box-p val)
     (let ((b (make-p-box val)))
       (setf (p-box-is-ref b) t)
       b))
    ;; Non-string vector (Perl array), hash, code, typeglob: wrap directly.
    ;; Strings are specialized vectors in CL but are Perl scalars, so exclude them here
    ;; — they fall through to the raw-scalar branch below.
    ((or (and (vectorp val) (not (stringp val))) (hash-table-p val) (functionp val))
     (make-p-box val))
    ;; Typeglob ref \\*foo: set is-ref so it is distinguishable from a *bare* glob
    ;; stored in a scalar (my $g = *foo).  A glob REF numifies to its address
    ;; (GLOB(0x..)); a bare glob numifies to 0.  Both share box-value=typeglob.
    ((p-typeglob-p val)
     (let ((b (make-p-box val)))
       (setf (p-box-is-ref b) t)
       b))
    ;; Raw scalar value (e.g. \42): double-box + is-ref so box-set handles it right.
    (t
     (let ((b (make-p-box (make-p-box val))))
       (setf (p-box-is-ref b) t)
       b))))

(defun p-arylen-ref (arr)
  "Perl \\$#array — a live reference to the array-length (arylen) magic of ARR.
   A plain (p-backslash (p-array-last-index arr)) backslashes a COPY of the
   integer, so $$ref = N would not resize ARR.  Instead wrap a p-magic-cell whose
   getter reads the last index and whose setter resizes ARR; reading/writing
   through the resulting scalar ref then flows through unbox/box-set automatically.
   ARR is whatever $#array's operand evaluates to (raw @arr vector or a boxed
   array ref) — p-array-last-index / p-set-array-length both accept either."
  (p-backslash
   (make-p-box
    (make-p-magic-cell
     :getter (lambda () (p-array-last-index arr))
     :setter (lambda (n) (p-set-array-length arr (to-number n)))))))

(defun p-substr-lvalue-cell (str start &optional len)
  "Bare magic-cell box for a substr() lvalue window (no \\-ref wrapper).
   Reading returns the current substring (p-substr getter), writing replaces
   that region of STR in place (4-arg p-substr).  STR must be a box for writes
   to propagate.  Used both by p-substr-ref (which adds the \\-ref layer) and to
   alias a foreach loop variable to substr(): `for (substr($x,1,3)) { $_ = ... }`
   binds $_ to this cell so the assignment writes through to $x.

   Perl's substr lvalue tracks edits: after each assignment the live window
   re-anchors onto the just-written text.
   - A positive start stays positive and resolves to its absolute position; a
     negative start stays anchored from the end and is recomputed as
     -(new-strlen - start) so it keeps pointing at the written text.
   - A fixed non-negative length becomes M (the characters just written); a
     to-end (nil) or from-end (negative) length keeps its end anchored to the
     string's end and is left as-is.
   So assigning 'XX' to substr($x,1,3) leaves substr($x,1,2); to substr($x,-5)
   leaves substr($x,-2); to substr($x,-5,3) leaves substr($x,-4,2)."
  (let ((cur-start (to-number start))
        (cur-len   (when len (to-number len))))
    (make-p-box
     (make-p-magic-cell
      :kind :lvalue
      :getter (lambda () (p-substr str cur-start cur-len))
      :setter (lambda (v)
                (let* ((slen-before (length (to-string (unbox str))))
                       (astart (if (< cur-start 0)
                                   (max 0 (+ slen-before cur-start))
                                   (min cur-start slen-before)))
                       (neg    (< cur-start 0))
                       (result (p-substr str cur-start cur-len v))
                       (slen-after (length (to-string (unbox str))))
                       (m      (length (to-string (unbox v)))))
                  (setf cur-start (if neg (- (- slen-after astart)) astart))
                  (when (and cur-len (>= cur-len 0))
                    (setf cur-len m))
                  result))))))

(defun p-pos-lvalue-cell (var)
  "Bare magic-cell box for a pos() lvalue (no \\-ref wrapper).  See
   p-substr-lvalue-cell."
  (make-p-box
   (make-p-magic-cell
    :kind :lvalue
    :getter (lambda () (let ((p (p-pos var))) (if p p *p-undef*)))
    :setter (lambda (v) (p-pos var v)))))

(defun p-vec-lvalue-cell (str offset bits)
  "Bare magic-cell box for a vec() lvalue element (no \\-ref wrapper).  See
   p-substr-lvalue-cell."
  (make-p-box
   (make-p-magic-cell
    :kind :lvalue
    :getter (lambda () (p-vec str offset bits))
    :setter (lambda (v) (p-vec-set str offset bits v)))))

(defun p-substr-ref (str start &optional len)
  "Perl \\substr(STR, START [, LEN]) — a live reference to the substr lvalue
   window.  Like p-arylen-ref it wraps a p-magic-cell: reading returns the current
   substring (p-substr getter), writing replaces that region of STR in place
   (4-arg p-substr).  STR must be a box for writes to propagate; START/LEN are
   fixed at refgen time (a fixed window), matching the common \\substr idiom."
  (p-backslash (p-substr-lvalue-cell str start len)))

(defun p-pos-ref (var)
  "Perl \\pos(VAR) — a live reference to VAR's /g match-position magic.  Reading
   returns the current pos (or undef); writing sets it.  VAR must be a box."
  (p-backslash (p-pos-lvalue-cell var)))

(defun p-vec-ref (str offset bits)
  "Perl \\vec(STR, OFFSET, BITS) — a live reference to a vec() lvalue element.
   Reading returns the element value (p-vec); writing stores it (p-vec-set).
   STR must be a box; OFFSET/BITS are fixed at refgen time."
  (p-backslash (p-vec-lvalue-cell str offset bits)))

(defun p-refgen-list (val)
  "Perl \\(LIST) — distribute reference generation over list elements.
   Receives the list-context value of the parenthesized expression and returns
   a fresh vector with one ref per element (spreading flatten-markers and arrays)."
  (let ((result (make-array 4 :adjustable t :fill-pointer 0)))
    (labels ((add-ref (item)
               (cond
                 ((p-flatten-marker-p item)
                  (loop for elem across (p-flatten-marker-array item)
                        do (vector-push-extend (p-backslash elem) result)))
                 ;; A HASH flattens to its key/value list first — `\(%h)` is
                 ;; 2N scalar refs, not one hash ref.  %p-flatten-list is the
                 ;; same flattener list assignment uses, so the ORDER matches
                 ;; `%h` everywhere else and the VALUE boxes come through
                 ;; unwrapped, which is what makes the value refs write back.
                 ((hash-table-p item)
                  (loop for elem across (%p-flatten-list item)
                        do (add-ref elem)))
                 ((and (vectorp item) (not (stringp item)))
                  (loop for elem across item
                        do (add-ref elem)))
                 (t
                  (vector-push-extend (p-backslash item) result)))))
      (cond
        ((and (vectorp val) (not (stringp val)))
         (loop for item across val do (add-ref item)))
        ((listp val)
         (loop for item in val do (add-ref item)))
        (t (add-ref val))))
    result))

(defun p-box-for-local (value)
  "Create a new box for a 'local $x = init' binding using box-set semantics.
   Unlike (make-p-box value) — which stores value raw, creating box-of-box for
   blessed objects — this properly unboxes non-reference values and copies the
   class, matching the semantics of a normal scalar assignment."
  (let ((box (make-p-box nil)))
    (box-set box value)
    box))

(defun p-backslash-sub (sym)
  "Perl \\&funcname — return a code ref, dispatching to AUTOLOAD if not defined."
  (cond
    ;; Real definition exists: return it directly (stable coderef identity).
    ((and (fboundp sym) (not (eq (gethash sym *p-declared-subs*) :stub)))
     (symbol-function sym))
    ;; Only a forward-declaration STUB exists (p-declare-sub).  Perl's \\&foo is
    ;; late-bound to the glob slot, so taking it before `sub foo {...}` and then
    ;; calling it after must reach the real body.  A stub returns nil and would
    ;; be captured by value, so return a trampoline that re-reads symbol-function
    ;; at CALL time.  (Needed now that sub bodies stay in source order relative
    ;; to use/BEGIN — see docs/declaration-ordering-fix-plan.md.)
    ((fboundp sym)
     (let ((tramp (lambda (&rest args) (apply (symbol-function sym) args))))
       (setf (gethash tramp *p-lazy-coderef-target*) sym)
       tramp))
    ;; Not declared at all: return a lambda that tries AUTOLOAD when called.
    (t
     (let* ((pkg *package*)
            (fallback
             (lambda (&rest args)
               (declare (ignore args))
               (let ((al (intern (%pcl-cl-sub-name "AUTOLOAD") pkg)))
                 (if (fboundp al)
                     (funcall (symbol-function al))
                     (error 'undefined-function :name sym))))))
       (setf (gethash fallback *p-lazy-coderef-target*) sym)
       fallback))))

(defun p-get-coderef (name-val)
  "Get a CL function from a Perl function name string or existing coderef.
   Handles 'Pkg::name' format, converting to CL naming convention (PL- prefix).
   Returns NIL if the function cannot be found."
  (let ((v (unbox name-val)))
    (cond
      ;; Already a function reference - return directly
      ((functionp v) v)
      ;; String - look up by Perl function name
      (t
       (let* ((s (%p-tick-package-seps (stringify-value v)))
              (last-sep (search "::" s :from-end t)))
         (if last-sep
             ;; Package-qualified: "Pkg::name" -> Pkg::PL-NAME.  Multi-segment
             ;; packages (Foo::Bar) keep their case (|Foo::Bar|); single-segment
             ;; is upcased — via perl-pkg-to-cl-pkg-name, the same rule the other
             ;; symbolic-ref paths use.  Plain string-upcase gave DATA::DUMP for a
             ;; multi-seg name, missed the |Data::Dump| package, and returned nil
             ;; (so \&{"Data::Dump::pp"} came back as a SCALAR ref to nil).
             (let* ((pkg-str (perl-pkg-to-cl-pkg-name (subseq s 0 last-sep)))
                    (func-str (subseq s (+ last-sep 2)))
                    (cl-func-name (%pcl-cl-sub-name func-str))
                    (pkg (find-package pkg-str)))
               (when pkg
                 (let ((sym (intern cl-func-name pkg)))
                   (and (fboundp sym) (symbol-function sym)))))
             ;; Unqualified: "name" -> PL-NAME in current package
             (let* ((cl-func-name (%pcl-cl-sub-name s))
                    (sym (intern cl-func-name *package*)))
               (and (fboundp sym) (symbol-function sym)))))))))

(defun %p-glob-slot-place (glob sigil init)
  "The value bound to GLOB's SIGIL slot symbol (\"@\" array / \"%\" hash),
   binding INIT first if the slot is unbound.  Returning the live binding makes
   @{*{glob}} / %{*{glob}} read AND write through to the package variable
   (Moo's _set_superclasses: @{*{_getglob(\"Pkg::ISA\")}} = @_)."
  (let ((sym (let ((n (concatenate 'string sigil (p-typeglob-name glob)))
                   (p (p-typeglob-package glob)))
               (or (find-symbol n p) (intern n p)))))
    (unless (boundp sym)
      (setf (symbol-value sym) init))
    (symbol-value sym)))

(defun p-cast-@ (val)
  "Perl array dereference @{$ref} - unbox to get the array.
   Handles both old format (box containing vector) and new format
   (box containing box containing vector, from p-backslash).
   Auto-vivifies: if val is a box whose value is undef/nil, creates an empty
   array, stores it back in the box, and returns it (Perl lvalue semantics).
   Symbolic ref: if val unboxes to a string, treats it as a package variable name.
   Typeglob: @{*{...}} resolves to the glob's ARRAY slot (live, lvalue-capable)."
  (let ((v (unbox val)))
    (cond
      ;; Direct vector — the overwhelmingly common `@$aref`, kept FIRST so the
      ;; hot path pays nothing for the diagnosis below.
      ((and v (vectorp v) (not (stringp v))) v)
      ;; @$scalarref — a ref to a plain SCALAR is not an array ref: perl's
      ;; fatal.  Checked BEFORE the layer-peel below, which would otherwise
      ;; hand back the referent BOX and the caller would read it as a
      ;; one-element list (silent wrong).  The referent rule (#163) is what
      ;; separates this from a genuine \@arr layer — see %p-scalar-referent-p.
      ;; The p-box-p guard is the whole cost when it does not apply: only a
      ;; still-boxed value can be a scalar ref.
      ((and (p-box-p v) (%p-scalar-referent-p val)) (%p-not-a-ref "ARRAY"))
      ;; Double-boxed: box(box(arr)) from \@arr — unwrap both layers
      ((p-box-p v) (unbox v))
      ;; Typeglob (from *{EXPR} or a glob ref): the glob's ARRAY slot
      ((p-typeglob-p v)
       (%p-glob-slot-place v "@" (make-array 0 :adjustable t :fill-pointer 0)))
      ;; Symbolic reference: @{"pkg::var"} — look up/create the package variable
      ((stringp v)
       (%p-symref-array v))
      ;; val is an lvalue box containing undef: auto-vivify as array ref.
      ;; Store (make-p-box new-arr) so box-set sees a reference (not raw vector)
      ;; and preserves it instead of coercing to length.
      ((and (p-box-p val) (or (null v) (eq v *p-undef*)))
       (let ((new-arr (make-array 0 :adjustable t :fill-pointer 0)))
         (box-set val (make-p-box new-arr))
         new-arr))
      ;; Wrong kind of referent (@$hashref, keys @$coderef): perl's fatal.
      ;; Previously fell into the catch-all below and the caller (p-keys,
      ;; foreach, push) silently saw an empty list.
      ((%p-wrong-referent-p "ARRAY" v) (%p-not-a-ref "ARRAY"))
      ;; @$scalarref — a ref to a plain SCALAR is not an array ref (#163's
      ;; referent rule; see p-ensure-hashref).  This used to return the
      ;; referent BOX and the caller treated it as a one-element list.
      ((%p-scalar-referent-p val) (%p-not-a-ref "ARRAY"))
      ;; Fallback: return whatever we have (may be *p-undef* if no box to write back)
      (t (or v *p-undef*)))))

(defun p-cast-% (val)
  "Perl hash dereference %{$ref} - unbox to get the hash.
   Handles both old format (box containing hash) and new format
   (box containing box containing hash, from p-backslash).
   A string ending in \"::\" is a symbolic stash reference (%{\"Pkg::\"} /
   %{\"main::\"}): return that package's stash (read-only snapshot of its subs),
   so keys/values/exists over a package symbol table work (Class::Inspector etc.)."
  (let ((v (unbox val)))
    (cond
      ;; Direct hash — the common `%$href`, first so the hot path is free.
      ((hash-table-p v) v)
      ;; %$scalarref — see p-cast-@'s arm (#163's referent rule); the p-box-p
      ;; guard is the whole cost when it does not apply.
      ((and (p-box-p v) (%p-scalar-referent-p val)) (%p-not-a-ref "HASH"))
      ((p-box-p v) (unbox v))
      ;; Typeglob (from *{EXPR} or a glob ref): the glob's HASH slot
      ((p-typeglob-p v)
       (%p-glob-slot-place v "%" (make-hash-table :test 'equal)))
      ((and (stringp v)
            (>= (length v) 2)
            (string= (subseq v (- (length v) 2)) "::"))
       (p-stash (subseq v 0 (- (length v) 2))))
      ;; Symbolic reference: %{"pkg::var"} — look up/create the package hash.
      ;; Mirrors p-cast-@'s %p-symref-array; without this a string fell through to
      ;; (t v) and \%{"Pkg::H"} backslashed the *string* (ref → SCALAR, not HASH),
      ;; which broke Exporter::Heavy's `*{...}=\%{"$pkg\::$name"}` %hash export.
      ((stringp v) (%p-symref-hash v))
      ;; Wrong kind of referent (%$aryref, keys %$aryref): perl's fatal.
      ;; Previously fell through to (t v) and the caller silently saw no keys.
      ((%p-wrong-referent-p "HASH" v) (%p-not-a-ref "HASH"))
      ;; %$scalarref — see p-ensure-hashref's arm (#163's referent rule).
      ((%p-scalar-referent-p val) (%p-not-a-ref "HASH"))
      (t v))))

;;; The symbol a symbolic reference names (#387 family 42, s413 — the prologue
;;; the four %p-symref-* readers/writers each spelled).  NAME-STR is perl's
;;; name without the sigil (`x`, `Foo::Bar::x`); SIGIL is "$" / "@" / "%".
;;; The last `::` splits package from variable; perl-pkg-to-cl-pkg-name keeps a
;;; multi-segment package case-preserved to match its CL package |Foo::Bar|
;;; (a plain string-upcase gave FOO::BAR, no such package) and upcases a
;;; single segment; an unqualified name lives in *package*.  With CREATE the
;;; package and the symbol are made (and the symbol's storage ensured) when
;;; missing — the writers' contract; without it the reader gets NIL for an
;;; unknown package or an unknown symbol.
(defun %p-symref-symbol (name-str sigil create)
  (let* ((pos (search "::" name-str :from-end t))
         (pkg-str (if pos (perl-pkg-to-cl-pkg-name (subseq name-str 0 pos)) nil))
         (var-str (if pos (subseq name-str (+ pos 2)) name-str))
         (pkg (if pkg-str
                  (or (find-package pkg-str)
                      (and create (make-package pkg-str :use '(:cl :pcl))))
                  *package*)))
    (when pkg
      (let ((sym-name (concatenate 'string sigil (%pcl-invert-case var-str))))
        (if create
            (let ((sym (or (find-symbol sym-name pkg) (intern sym-name pkg))))
              (%p-ensure-storage sym)
              sym)
            (find-symbol sym-name pkg))))))

(defun %p-symref-box (name-str)
  "Resolve Perl symbolic scalar reference NAME-STR to a CL box.
   Returns the box on success, NIL if the name is invalid or variable not found."
  ;; CL symbols cannot contain null bytes — silently return nil
  (when (find #\Nul name-str) (return-from %p-symref-box nil))
  (let ((sym (%p-symref-symbol name-str "$" nil)))
    (when (and sym (boundp sym))
      (let ((v (symbol-value sym)))
        (when (p-box-p v) v)))))

(defun (setf %p-symref-box) (new-box name-str)
  "Set Perl symbolic scalar reference NAME-STR to NEW-BOX."
  (when (find #\Nul name-str) (return-from %p-symref-box new-box))
  (setf (symbol-value (%p-symref-symbol name-str "$" t)) new-box)
  new-box)

(defun %p-symref-array (name-str)
  "Resolve symbolic array reference NAME-STR (e.g. '3foo::ISA') to the CL vector.
   Creates the package and the @VAR binding if they don't exist yet, so that
   assignment through a symbolic ref works: @{\"pkg::var\"} = (...).
   Returns the adjustable vector."
  (when (find #\Nul name-str) (return-from %p-symref-array
                                (make-array 0 :adjustable t :fill-pointer 0)))
  (let ((sym (%p-symref-symbol name-str "@" t)))
    (unless (and (boundp sym)
                 (vectorp (symbol-value sym))
                 (not (stringp (symbol-value sym))))
      (setf (symbol-value sym) (make-array 0 :adjustable t :fill-pointer 0)))
    (symbol-value sym)))

(defun %p-symref-hash (name-str)
  "Resolve symbolic hash reference NAME-STR (e.g. 'Config::Config') to the CL
   hash-table.  Creates the package and the %VAR binding if they don't exist yet,
   so assignment through a symbolic ref works: %{\"pkg::var\"} = (...).
   Returns the hash-table."
  (when (find #\Nul name-str) (return-from %p-symref-hash
                                (make-hash-table :test 'equal)))
  (let ((sym (%p-symref-symbol name-str "%" t)))
    (unless (and (boundp sym) (hash-table-p (symbol-value sym)))
      (setf (symbol-value sym) (make-hash-table :test 'equal)))
    (symbol-value sym)))

(defun p-cast-$ (val)
  "Perl scalar dereference ${$ref} or symbolic ref ${'name'}.
   If val unboxes to a string, treat as symbolic reference."
  (let ((inner (unbox val)))
    (cond
      ((p-box-p inner)
       ;; If the referent box holds a magic cell (\substr/\pos/\vec via a DIRECT
       ;; ref, e.g. ${\vec %h,0,1}), fire its getter rather than returning the raw
       ;; cell struct. Through a variable the cell sits one box deeper and
       ;; box-set's magic-cell copy arm handles it.
       (let ((v (p-box-value inner)))
         (if (p-magic-cell-p v) (funcall (p-magic-cell-getter v)) v)))
      ((stringp inner)
       ;; Symbolic reference: ${"varname"}
       (let ((box (%p-symref-box inner)))
         (if box (p-box-value box) nil)))
      ;; ${qr//}: perl's REGEXP sv stringifies as "(?^:...)" and numifies
      ;; through that string (0).  PCL merges the Regexp ref and its referent
      ;; into one struct (which numifies as an address, correct for the REF
      ;; level), so the deref site is where the referent view is produced.
      ((p-regex-match-p inner) (to-string inner))
      ;; NO wrong-referent guard here, deliberately (s317, task #154): PCL's
      ;; ref model COLLAPSES a scalar-ref-to-coderef, so this site legitimately
      ;; receives a raw function — Sub::Quote's generated
      ;; `my $x = ${$_[1]->{'$name'}};` is exactly that, and guarding here took
      ;; Moo down (6 rows of Pl/t/moo-01.t).  `$$aryref` therefore still reads
      ;; as the array instead of dying "Not a SCALAR reference"; that residue is
      ;; recorded in #154 and needs a referent-kind tag on the box, not a
      ;; type-sniff at the deref site.
      (t inner))))

(defun (setf p-cast-$) (new-value val)
  "Perl scalar dereference assignment ${$ref} = val or ${'name'} = val.
   Handles symbolic references when val unboxes to a string."
  (let ((inner (unbox val)))
    (cond
      ((p-box-p inner)
       ;; val is a reference box; inner is the referenced box or value
       (let ((target (p-box-value inner)))
         (if (p-box-p target)
             (box-set target new-value)    ; normal scalar ref: set the target
             (box-set inner new-value))))  ; inner is the scalar container
      ((stringp inner)
       ;; Symbolic reference: ${"varname"} = val
       (let ((box (or (%p-symref-box inner)
                      (let ((b (make-p-box nil)))
                        (setf (%p-symref-box inner) b)
                        b))))
         (box-set box new-value)))
      ;; val itself is the scalar container (blessed scalar in tie methods)
      ((p-box-p val)
       (box-set val new-value))
      (t (error "Cannot dereference non-reference: ~A" inner)))))

(defun p-hash-deref-= (hash-ref value)
  "Assign to a dereferenced hash: %$ref = (list).
   hash-ref is the box containing the hash reference.
   Gets or auto-vivifies the hash, then clears and repopulates it."
  (let* ((inner (unbox hash-ref))
         (h (cond
              ;; Double-boxed (from \%hash): box(box(hash))
              ((p-box-p inner) (unbox inner))
              ;; Direct hash-table
              ((hash-table-p inner) inner)
              ;; Auto-vivify: create empty hash and store back in box
              (t (let ((new-h (make-hash-table :test 'equal)))
                   (when (p-box-p hash-ref)
                     (setf (p-box-value hash-ref) new-h
                           (p-box-nv-ok hash-ref) nil
                           (p-box-sv-ok hash-ref) nil))
                   new-h)))))
    (unless (hash-table-p h)
      (setf h (make-hash-table :test 'equal)))
    (clrhash h)
    (let ((flat (%p-flatten-list value)))
      (loop for i from 0 below (length flat) by 2
            when (< (1+ i) (length flat))
            do (setf (gethash (to-string (aref flat i)) h)
                     (%p-make-hash-entry (aref flat (1+ i))))))
    h))

(defun p-array-deref-= (array-ref value)
  "Assign to a dereferenced array: @$ref = (list).
   array-ref is the box containing the array reference.
   Gets or auto-vivifies the array, then clears and repopulates it."
  (let* ((inner (unbox array-ref))
         (arr (cond
                ;; Double-boxed (from \@arr): box(box(arr))
                ((p-box-p inner) (unbox inner))
                ;; Direct vector
                ((and (vectorp inner) (not (stringp inner))) inner)
                ;; Auto-vivify: create empty array and store back in box
                (t (let ((new-arr (make-array 0 :adjustable t :fill-pointer 0)))
                     (when (p-box-p array-ref)
                       (setf (p-box-value array-ref) new-arr
                             (p-box-nv-ok array-ref) nil
                             (p-box-sv-ok array-ref) nil))
                     new-arr)))))
    (unless (and (vectorp arr) (not (stringp arr)))
      (setf arr (make-array 0 :adjustable t :fill-pointer 0)))
    (%p-check-array-writable arr)          ; @$ref = (…) on a read-only array, task #159
    (setf (fill-pointer arr) 0)
    (let ((flat (%p-flatten-list value)))
      (loop for item across flat
            do (%p-array-store-scalar arr item)))
    arr))

(defun %scalar-holds-ref-p (referent)
  "True when REFERENT (a scalar box that some reference points at) itself holds
   a reference value — i.e. ref(\\referent) is \"REF\", not \"SCALAR\".
   Non-recursive: only looks one unbox deep, so a self-referential scalar ref
   ($x = \\$x) does not loop."
  (and (p-box-p referent)
       (let ((u (unbox referent)))
         (or (and (vectorp u) (not (stringp u)))   ; holds an array ref
             (hash-table-p u)                       ; holds a hash ref
             (p-regex-match-p u)                    ; holds a regexp ref
             (functionp u)                          ; holds a code ref
             (p-typeglob-p u)                       ; holds a glob ref
             (and (p-box-p u) (p-box-is-ref u))))))  ; holds a scalar/ref wrapper

(defun %p-referent-shaped-p (v)
  "True when V can BE the thing a reference points at: a scalar box, or a raw
   aggregate/code/glob/regexp object.  A plain scalar value cannot — reaching
   one means the walk went a level too deep."
  (or (p-box-p v)
      (hash-table-p v)
      (functionp v)
      (p-typeglob-p v)
      (p-regex-match-p v)
      (and (vectorp v) (not (stringp v)))))

(defun %p-glob-value-box-p (b)
  "True when box B holds a typeglob VALUE (`$g = *foo`) rather than a glob
   REFERENCE (`$g = \\*foo`).  perl turns such an SV into a GV, so it is not a
   reference at all: ref($g) is \"\", \"$g\" is *pkg::name, and \\$g is a GLOB
   ref (perl-tests/substr.t 784) — while the same three answers for a glob ref
   are GLOB, GLOB(0x…) and REF.

   `is-ref` is the ONE discriminator (task #163's rule applied to globs):
   p-backslash sets it on the wrapper, box-set propagates it whenever the
   assigned value is a typeglob, and box-nv has always read it.  Every reader
   of a typeglob payload asks THIS, so the word, the number and ref() cannot
   disagree.

   A RAW typeglob (not in a box) is a glob VALUE — the convention
   stringify-value already fixed for the string half (#316), and the reason
   every path that COPIES a scalar must carry the flag rather than snapshot the
   bare glob (%p-flatten-list, %p-array-store-scalar, p-aref-unbox-elem;
   box-set, p-flatten-args, p-return-value and p-copy-scalar-arg already did).
   Carrying the flag is not the same as keeping the source BOX: a list
   assignment snapshots its RHS, so preserving the box collapses
   ($g1,$g2) = ($g2,$g1) into one glob.  This predicate answers NIL for a raw
   typeglob because there is no box to ask; callers spell that case out."
  (and (p-box-p b)
       (p-typeglob-p (p-box-value b))
       (not (p-box-is-ref b))))

(defun %p-ref-referent (val)
  "The object a reference POINTS AT — the SV whose address perl prints and
   whose identity `==` compares.  This is never the p-backslash wrapper: a
   wrapper is fresh per `\\`, so using it would make `\\$x == \\$x` false and
   print two different addresses for one variable (task #163).

   VAL is either the wrapper itself — `\\$x` reaching a raw slot: a sub
   argument under p-raw-params, an array/hash element, a print argument — or a
   variable box holding one (`my $r = \\$x`).  `is-ref` on the wrapper is the
   discriminator, the SAME one p-ref's LVALUE/VSTRING/REF arms use; box-sv used
   to count levels instead, which is why the identical `\\$x` printed SCALAR
   through a variable and REF straight into print.

   Returns VAL's own value for a non-reference (callers guard), and declines to
   walk past a shape that cannot be a referent (a single-boxed legacy ref)."
  (when (p-box-p val)
    (let ((inner (p-box-value val)))
      (if (and (p-box-p inner) (not (p-box-is-ref val)))
          ;; VAL is the variable: INNER is the wrapper, the referent is under it.
          (let ((r (p-box-value inner)))
            (if (%p-referent-shaped-p r) r inner))
          ;; VAL is the wrapper itself (or holds a raw aggregate ref).
          inner))))

(defun %p-scalar-referent-p (val)
  "True when VAL is a reference to a plain SCALAR — `\\$x` — and therefore
   cannot be dereferenced as a container.  This is the distinction
   %p-wrong-referent-p documents that it cannot make (#154): after one unbox a
   `\\$x` and the representation layer of a `\\%h` reached through a ref-to-ref
   both leave a p-box behind, so counting either as a mismatch broke the other.
   The referent rule tells them apart — `\\$x`'s referent is a box holding a
   plain scalar; a representation layer's holds the container."
  (let ((r (%p-ref-referent val)))
    (and (p-box-p r)
         (not (%p-referent-shaped-p (p-box-value r))))))

(defun %p-ref-string (val)
  "How perl stringifies the reference held by VAL: TYPE(0xADDR), where TYPE is
   p-ref's answer ignoring bless and ADDR is the REFERENT's address.  NIL when
   VAL holds no recognizable reference, so each caller keeps its own fallback.

   One definition, shared by box-sv (a ref in a variable) and stringify-value
   (a ref in a raw slot) — they printed different words for the same reference
   before (#163)."
  (let ((referent (%p-ref-referent val)))
    (cond
      ((p-box-p referent)
       (let ((rv (p-box-value referent)))
         (cond
           ((and (p-magic-cell-p rv) (eq (p-magic-cell-kind rv) :lvalue))
            (format nil "LVALUE(0x~(~X~))" (object-address referent)))
           ((p-vstring-p rv)
            (format nil "VSTRING(0x~(~X~))" (object-address referent)))
           ;; The referent scalar IS a glob (`$g = *foo` makes the SV a GV), so
           ;; \$g is a GLOB ref, not a REF — same discrimination p-ref makes.
           ((%p-glob-value-box-p referent)
            (format nil "GLOB(0x~(~X~))" (object-address referent)))
           ((or (p-box-is-ref referent) (%scalar-holds-ref-p referent))
            (format nil "REF(0x~(~X~))" (object-address referent)))
           (t (format nil "SCALAR(0x~(~X~))" (object-address referent))))))
      ((and (vectorp referent) (not (stringp referent)))
       (format nil "ARRAY(0x~(~X~))" (object-address referent)))
      ((hash-table-p referent)
       (format nil "HASH(0x~(~X~))" (object-address referent)))
      ((functionp referent)
       (format nil "CODE(0x~(~X~))" (object-address referent)))
      ;; A raw typeglob referent: `our $r = \*foo` stores the p-backslash
      ;; wrapper box-in-box (p-scalar-='s reference branch), so the referent
      ;; here IS the typeglob.  Without this arm the walk fell off the end to
      ;; NIL and box-sv printed the caller's SCALAR(0x…) fallback for a glob
      ;; ref held in a package variable (a lexical, stored is-ref by box-set,
      ;; never reached this path — which is why the two spellings disagreed).
      ((p-typeglob-p referent)
       (format nil "GLOB(0x~(~X~))" (object-address referent)))
      (t nil))))

(defun %p-scalar-ref-referent (val)
  "The referent BOX of a scalar reference, or NIL.
   VAL may be the is-ref wrapper itself or a variable box holding one."
  (when (p-box-p val)
    (let ((w (if (p-box-is-ref val)
                 val
                 (let ((v (p-box-value val)))
                   (and (p-box-p v) v)))))
      (let ((r (and w (p-box-value w))))
        (and (p-box-p r) r)))))

(defun %p-referent-class (val)
  "The class recorded on the REFERENT of a plain scalar reference, or NIL.
   Perl blesses the referent, not the reference, so when this answers it
   OUTRANKS any class cached on a wrapper or variable box: a second \\$x
   wrapper never met the bless, and a re-bless through one alias must show
   through all of them.  Declines (NIL) when the referent holds anything
   but a plain scalar -- those shapes are REF/ARRAY/REGEXP/... in ref()
   terms and their arms must keep winning."
  (let ((r (%p-scalar-ref-referent val)))
    (when r
      (let ((rv (p-box-value r)))
        (and (not (p-box-p rv))
             (not (and (vectorp rv) (not (stringp rv))))
             (not (hash-table-p rv))
             (not (functionp rv))
             (not (p-typeglob-p rv))
             (not (p-regex-match-p rv))
             (not (p-magic-cell-p rv))
             (p-box-class r))))))

(defun %p-ref-shaped-p (v)
  "Is V a REFERENCE payload -- a shape whose class may legitimately be
   read off the VARIABLE box's slot (CODE/GLOB/raw-vector targets cannot
   carry a class themselves)?  A plain scalar payload says NO: its box's
   slot is the SCALAR's own SvSTASH (`bless \\$x` writes it), visible only
   through a ref TO the box -- never through ref($x) or \"$x\" (join.t
   36/40: a tied FETCH returning the live cell of a blessed \\$x must
   stringify as the plain value, not \"SM=4\")."
  (or (p-box-p v) (hash-table-p v) (functionp v)
      (and (vectorp v) (not (stringp v)))
      (p-typeglob-p v) (streamp v) (p-regex-match-p v)))

(defun %p-target-class (box)
  "The class recorded on what BOX's ref value POINTS TO: the referent box
   of a plain scalar ref, a hash target's :__class__, or a classed target
   box (aggregate constructor).  Perl's ref()/dispatch read
   SvSTASH(SvRV(rv)) -- the TARGET's stash -- and that OUTRANKS the class
   slot on the variable box itself, which doubles as the SCALAR's own
   SvSTASH: `bless \\$a1, \"F\"` writes $a1's slot, and only a ref TO $a1
   may read it -- ref($a1) still reports the class of what $a1 holds
   (perl-tests/bless.t 29-32: blessing a ref to an object must not change
   the object's class)."
  (let ((v (p-box-value box)))
    (cond
      ;; Plain payloads first -- the overwhelmingly common case (every
      ;; string/number variable): nothing a target class could live on,
      ;; and the referent rule only applies through a box payload, so
      ;; exit before any deeper walk (pack.t ran at 87s of a 90s budget;
      ;; this lookup is on the box-sv/p-ref/p-get-class hot paths).
      ((or (null v) (stringp v) (numberp v) (eq v *p-undef*)) nil)
      ((hash-table-p v) (gethash :__class__ v))
      ((p-box-p v) (or (%p-referent-class box) (p-box-class v)))
      (t nil))))

(defun p-ref (val)
  "Perl ref() function - get reference type or class name if blessed.
   Returns empty string for non-references."
  ;; Unbox the variable to get what it contains
  (let ((inner (unbox val)))
    (cond
      ;; The TARGET's class is the truth (see %p-target-class): the
      ;; referent of a scalar ref, a hash's :__class__, or a classed
      ;; target box -- checked BEFORE the variable box's own slot so a
      ;; re-bless through one alias shows through every alias, and so
      ;; `bless \\$a1` (which writes $a1's OWN slot) cannot shadow the
      ;; class of the object $a1 points to.  This also covers the
      ;; `local $x = blessed_ref` re-wrap (target box carries the class).
      ((and (p-box-p val) (%p-target-class val)))
      ;; Blessed value on the variable box itself: the storage for ref
      ;; kinds whose target cannot carry a class (raw CODE/GLOB values).
      ;; Only for REF payloads -- a plain-scalar payload's slot is the
      ;; scalar's own SvSTASH, and ref($x) on a non-ref is "".
      ((and (p-box-p val) (p-box-class val) (%p-ref-shaped-p inner))
       (p-box-class val))
      ((and (hash-table-p inner) (gethash :__class__ inner))
       (gethash :__class__ inner))
      ;; Reference box: inner is a p-box - check what it wraps (ARRAY/HASH/SCALAR)
      ((p-box-p inner)
       ;; (A classed inner was already answered by %p-target-class above.)
       (if (p-box-class inner)
           (p-box-class inner)
           ;; The REFERENT — what the reference points at — is `inner` when VAL
           ;; is itself the p-backslash wrapper (a literal `\$x` handed straight
           ;; to ref()) and one level deeper when VAL is a variable holding one.
           ;; %p-ref-referent is that rule, shared with the stringifiers (#163);
           ;; every arm below reads the referent, so the same reference cannot
           ;; answer two different words depending on how it arrived.
           (let* ((referent (%p-ref-referent val))
                  (rv (and (p-box-p referent) (p-box-value referent))))
             (cond
               ;; Magic lvalue ref (\substr / \pos / \vec): the referent box holds
               ;; a p-magic-cell with :lvalue kind → "LVALUE" (arylen's cell has
               ;; kind nil and falls through to "SCALAR").
               ((and (p-magic-cell-p rv) (eq (p-magic-cell-kind rv) :lvalue))
                "LVALUE")
               ;; Referent scalar holds a v-string literal → "VSTRING" (op/ver.t).
               ((p-vstring-p rv) "VSTRING")
               ;; The referent IS a raw aggregate: a wrapper for `\@a`/`\%h`/…
               ;; stored in a variable or element.  (When VAL is a ref TO a
               ;; scalar that HOLDS such a value — `\$aref` — the referent is the
               ;; scalar BOX, so these do not fire and the REF arm below wins,
               ;; which is what perl answers.)
               ((and (vectorp referent) (not (stringp referent))) "ARRAY")
               ((hash-table-p referent) (or (gethash :__class__ referent) "HASH"))
               ((p-regex-match-p referent) "REGEXP")
               ((functionp referent) "CODE")
               ((p-typeglob-p referent) "GLOB")
               ;; The referent SCALAR holds a glob VALUE: `my $g = *foo` makes
               ;; the SV a GV in perl, so \$g is a GLOB ref (substr.t 784 —
               ;; '\substr does not coerce its glob arg just yet').  A referent
               ;; holding a glob REFERENCE keeps is-ref and stays REF below.
               ((%p-glob-value-box-p referent) "GLOB")
               ;; Ref-to-ref → "REF": the referent is itself a ref-wrapper (\\1)
               ;; or *holds* a reference (\$r, \$aref).  %scalar-holds-ref-p is
               ;; non-recursive so a self-referential scalar ($x=\$x) does not
               ;; loop, and a plain scalar — incl. undef (*p-undef*) and ''
               ;; array elements — yields SCALAR, not REF.
               ((or (and (p-box-p referent) (p-box-is-ref referent))
                    (%scalar-holds-ref-p referent))
                "REF")
               ;; Scalar reference: box containing box (from p-backslash $x)
               (t "SCALAR")))))
      ;; Old-format hash reference (autovivified, single-boxed)
      ((hash-table-p inner) "HASH")
      ;; Old-format array reference (autovivified, single-boxed)
      ((or (listp inner) (and (vectorp inner) (not (stringp inner)))) "ARRAY")
      ;; Code reference
      ((functionp inner) "CODE")
      ;; Typeglob payload.  A glob VALUE is not a reference at all in perl —
      ;; `$g = *foo` makes the SV a GV, and ref($g) is "" — so only an is-ref
      ;; box answers GLOB.  A RAW typeglob is a glob VALUE by the same
      ;; convention stringify-value uses for it (#316): every path that COPIES
      ;; a scalar carries the flag (%p-flatten-list, %p-array-store-scalar,
      ;; p-aref-unbox-elem, box-set, p-flatten-args, p-return-value).
      ((p-typeglob-p inner) (if (and (p-box-p val) (p-box-is-ref val))
                                "GLOB"
                                ""))
      ;; Compiled regex (qr//) — ref() returns "Regexp"
      ((p-regex-match-p inner) "Regexp")
      ;; Not a reference
      (t ""))))

;; reftype() returns the base type ignoring blessed status.
;; Differs from ref(): ref(blessed_hashref) = class, reftype(blessed_hashref) = "HASH".
;; For qr// it returns "REGEXP" (uppercase) vs ref()'s "Regexp".
(defun p-reftype (val)
  (let ((inner (unbox val)))
    (cond
      ((p-regex-match-p inner) "REGEXP")
      ;; For all other types, reftype and ref agree (modulo blessing)
      (t (let ((r (p-ref val)))
           ;; If ref returns a class name (blessed), get the underlying type
           (cond
             ((string= r "HASH")   "HASH")
             ((string= r "ARRAY")  "ARRAY")
             ((string= r "CODE")   "CODE")
             ((string= r "SCALAR") "SCALAR")
             ((string= r "LVALUE") "LVALUE")
             ;; ref-to-ref: the referent is still a SCALAR (it happens to hold a ref)
             ((string= r "REF")    "SCALAR")
             ((string= r "GLOB")   "GLOB")
             ;; Non-ref: Perl's reftype returns undef (NOT ""; ref() returns "").
             ((string= r "") *p-undef*)
             ;; Blessed object — look at the inner type
             (t (cond
                  ((hash-table-p inner) "HASH")
                  ((and (vectorp inner) (not (stringp inner))) "ARRAY")
                  ((functionp inner) "CODE")
                  ((p-box-p inner) "SCALAR")
                  ((p-typeglob-p inner) "GLOB")
                  (t r)))))))))

;;; Scalar::Util / builtin — weak reference stubs
;;; PCL uses a tracing GC; true weak refs require trivial-garbage integration.
;;; weaken() is a no-op (object stays alive); isweak() always returns false.
(defun p-weaken (ref)
  "Scalar::Util::weaken / builtin::weaken — no-op stub."
  (declare (ignore ref))
  *p-undef*)

(defun p-isweak (ref)
  "Scalar::Util::isweak / builtin::isweak — always false in PCL."
  (declare (ignore ref))
  "")

;;; ------------------------------------------------------------
;;; builtin:: namespace (core pragma, Perl 5.36+; user is on 5.40)
;;; ------------------------------------------------------------
;;; Perl's `builtin` functions are always available without `use`, so they live
;;; in the runtime (a generated `builtin::is_bool(...)` call compiles to a direct
;;; BUILTIN::PL-IS_BOOL form that must resolve).  We provide the functions PCL
;;; can implement faithfully and register them in *p-declared-subs* so
;;; `defined &builtin::NAME` reports true like real Perl.
;;;
;;; DELIBERATELY ABSENT: created_as_number / created_as_string (and is_bool's
;;; precision) depend on per-SV IOK/NOK/POK/bool flags PCL's box model does not
;;; track — the same SV-flags limitation as JSON number encoding.  Leaving
;;; created_as_* undefined makes `defined &builtin::created_as_number` false, so
;;; consumers (e.g. Sub::Quote) degrade to their flag-free fallback path.  is_bool
;;; is provided but best-effort: it always returns false ("not a tracked bool"),
;;; which is the safe answer (a boolean is still an ordinary scalar).
(defun %p-builtin-blessed (x)
  "builtin::blessed — class name of a blessed ref, else undef."
  (let ((r (p-ref x)))
    (if (or (string= r "")
            (member r '("HASH" "ARRAY" "CODE" "SCALAR" "REF" "GLOB" "LVALUE" "Regexp")
                    :test #'string=))
        *p-undef*
        r)))

(defun %p-builtin-refaddr (x)
  "builtin::refaddr — integer address of the referent, else undef."
  (if (string= (p-ref x) "") *p-undef* (object-address (unbox x))))

(defun %p-builtin-reftype (x)
  "builtin::reftype — underlying ref type, else undef (not empty string)."
  (let ((rt (p-reftype x))) (if (string= rt "") *p-undef* rt)))

(defun %p-builtin-trim (s)
  "builtin::trim — strip leading/trailing ASCII whitespace."
  (string-trim '(#\Space #\Tab #\Newline #\Return #\Page) (to-string s)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "BUILTIN")
    (make-package "BUILTIN" :use '(:cl :pcl))))

(eval-when (:load-toplevel :execute)
  (flet ((def (name fn)
           (let ((sym (intern (concatenate 'string "PL-" name) "BUILTIN")))
             (setf (symbol-function sym) fn)
             (setf (gethash sym *p-declared-subs*) :defined))))
    (def "TRUE"     (lambda (&rest a) (declare (ignore a)) (make-p-box 1)))
    (def "FALSE"    (lambda (&rest a) (declare (ignore a)) (make-p-box "")))
    (def "IS_BOOL"  (lambda (&rest a) (declare (ignore a)) (make-p-box "")))
    (def "WEAKEN"   (lambda (r) (p-weaken r)))
    (def "UNWEAKEN" (lambda (r) (declare (ignore r)) *p-undef*))
    (def "IS_WEAK"  (lambda (r) (p-isweak r)))
    (def "DUALVAR"  (lambda (num str) (p-dualvar num str)))
    ;; Companions to DUALVAR: both ask a question about the SCALAR'S OWN
    ;; REPRESENTATION that no amount of plain Perl can answer — whether the box
    ;; carries two independent caches (a dualvar) and whether it holds a
    ;; v-string payload rather than an ordinary string.  Perl answers 1/"".
    (def "IS_DUAL"  (lambda (x) (make-p-box (if (%pcl-dualvar-p x) 1 ""))))
    (def "IS_VSTRING" (lambda (x) (make-p-box (if (p-vstring-p (unbox x)) 1 ""))))
    (def "BLESSED"  #'%p-builtin-blessed)
    (def "REFADDR"  #'%p-builtin-refaddr)
    (def "REFTYPE"  #'%p-builtin-reftype)
    (def "CEIL"     (lambda (x) (values (ceiling (to-number x)))))
    (def "FLOOR"    (lambda (x) (values (floor (to-number x)))))
    (def "TRIM"     #'%p-builtin-trim)
    (def "STRINGIFY" (lambda (x) (to-string x)))
    ;; created_as_number / created_as_string: Perl reports how the SV was
    ;; created (IOK/NOK vs POK).  PCL can't see those flags, but its box stores a
    ;; CL number for numeric scalars and a CL string for string scalars, which is
    ;; a faithful-enough proxy: a value held as a number was created numeric, one
    ;; held as a string was created as a string.  (Best-effort — a number that has
    ;; been stringified in place may read as a string; acceptable for the inlining
    ;; decisions these drive, e.g. Sub::Quote::quotify.)
    (def "CREATED_AS_NUMBER" (lambda (x) (make-p-box (if (numberp (unbox x)) 1 ""))))
    (def "CREATED_AS_STRING" (lambda (x) (make-p-box (if (stringp (unbox x)) 1 ""))))))

;;; ============================================================
;;; Typeglob Support
;;; ============================================================
;;; A Perl typeglob *foo is a named symbol table entry with slots for
;;; SCALAR, ARRAY, HASH, CODE, IO. The p-typeglob struct is just a
;;; label (package + name); slot values live in the CL symbol table.

(defstruct (p-typeglob (:constructor make-p-typeglob (package name)))
  package   ; CL package object
  name)     ; upcased Perl name string, e.g. "FOO"

(defun p-make-typeglob (pkg-str name-str)
  "Create a typeglob object for *Pkg::Name."
  (let ((pkg (or (%pcl-find-package pkg-str)
                 ;; Package may not exist yet; create it lazily
                 (make-package (perl-pkg-to-cl-pkg-name pkg-str) :use '(:cl :pcl)))))
    (make-p-typeglob pkg (%pcl-invert-case name-str))))

(defun %p-glob-assign-slots (pkg uname rhs)
  "Assign RHS to the appropriate slot of typeglob (PKG package-object, UNAME
   already-upcased name string).  Dispatch is by type of the unwrapped RHS.
   Shared by p-glob-assign (name-string form) and the glob-REF form of
   p-glob-assign-dynamic (*{\\*Pkg::name} = val)."
  (let ((inner (if (p-box-p rhs) (unbox rhs) rhs)))
    (cond
      ;; *foo = *bar — full glob copy
      ((p-typeglob-p rhs)   (p-glob-copy pkg uname rhs))
      ((p-typeglob-p inner) (p-glob-copy pkg uname inner))

      ;; *foo = \&sub or *foo = sub{} — CODE slot.  A CODE-slot install
      ;; DEFINES the sub: `defined &foo` must see it (p-sub-defined keys on
      ;; *p-declared-subs* :defined, never fboundp — forward stubs are
      ;; fbound too).  Task #83 (import-into-caller glob installs).
      ((functionp inner)
       (let ((sym (intern (%pcl-uname-to-sub uname) pkg)))
         (setf (fdefinition sym) inner)
         (setf (gethash sym *p-declared-subs*) :defined)))

      ;; *foo = \$scalar — SCALAR slot (inner is the p-box = the variable itself)
      ((p-box-p inner)
       (setf (symbol-value (intern (concatenate 'string "$" uname) pkg))
             inner))

      ;; *foo = \@array — ARRAY slot (inner is the adjustable vector)
      ((and (vectorp inner) (adjustable-array-p inner))
       (setf (symbol-value (intern (concatenate 'string "@" uname) pkg))
             inner))

      ;; *foo = \%hash — HASH slot (inner is the hash-table)
      ((hash-table-p inner)
       (setf (symbol-value (intern (concatenate 'string "%" uname) pkg))
             inner))

      ;; *foo = 'name' — symbolic alias: copy slots from *pkg::name
      ((stringp inner)
       (let ((src-name (%pcl-invert-case inner)))
         (p-glob-copy pkg uname (make-p-typeglob pkg src-name))))

      ;; *foo = undef — no-op
      ((or (null inner) (eq inner *p-undef*)) nil)

      ;; Fallback: try as CODE if rhs is directly a function
      ((functionp rhs)
       (let ((sym (intern (%pcl-uname-to-sub uname) pkg)))
         (setf (fdefinition sym) rhs)
         (setf (gethash sym *p-declared-subs*) :defined))))))

(defun p-glob-assign (pkg-str name-str rhs)
  "Assign RHS to the appropriate slot of typeglob *pkg::name (by name strings)."
  (let ((pkg   (or (%pcl-find-package pkg-str)
                   (make-package (perl-pkg-to-cl-pkg-name pkg-str) :use '(:cl :pcl))))
        (uname (%pcl-invert-case name-str)))
    (%p-glob-assign-slots pkg uname rhs)))

(defun p-glob-assign-dynamic (name-box rhs)
  "Dynamic typeglob assignment: *{EXPR} = val.  EXPR is either a NAME string
   (\"Pkg::name\") or a glob REFERENCE (\\*{...}) — the form Moo's _install_coderef
   uses (_getglob returns \\*{$name}, then *{$glob} = $code).  A glob ref unboxes
   to a p-typeglob; assign straight into its slots rather than stringifying it
   (which would yield GLOB(0x..) and install nothing)."
  (let ((inner (if (p-box-p name-box) (unbox name-box) name-box)))
    (if (p-typeglob-p inner)
        (%p-glob-assign-slots (p-typeglob-package inner) (p-typeglob-name inner) rhs)
        (let* ((name-str (to-string name-box))
               (sep-pos (search "::" name-str :from-end t))
               ;; An UNQUALIFIED symbolic name resolves in the package in
               ;; effect, not in main — `*{"_IS_\U$_"} = …` inside
               ;; `package File::Path` installs File::Path::_IS_MSWIN32.  A
               ;; hardcoded "main" put it in the wrong stash, and the very next
               ;; line of that BEGIN block (`!(_IS_MSWIN32())`) then died with
               ;; "The function |File::Path|::pl-_IS_MSWIN32 is undefined".
               (pkg-str  (if sep-pos (subseq name-str 0 sep-pos)
                             *pcl-current-package*))
               (bare-str (if sep-pos (subseq name-str (+ sep-pos 2)) name-str)))
          (p-glob-assign pkg-str bare-str rhs)))))

(defun p-dynamic-typeglob (name-box)
  "Rvalue *{EXPR} — return a typeglob object.  EXPR is a NAME string (\"Pkg::name\")
   or a glob REFERENCE (\\*{...}); a glob ref unboxes to a p-typeglob, which we
   return as-is (e.g. *{$glob}{CODE} where $glob = \\*{...})."
  (let ((inner (if (p-box-p name-box) (unbox name-box) name-box)))
    (if (p-typeglob-p inner)
        inner
        (let* ((name-str (to-string name-box))
               (sep-pos (search "::" name-str :from-end t))
               ;; Unqualified → the package in effect, as in p-glob-assign-dynamic.
               (pkg-str  (if sep-pos (subseq name-str 0 sep-pos)
                             *pcl-current-package*))
               (bare-str (if sep-pos (subseq name-str (+ sep-pos 2)) name-str))
               (pkg (or (%pcl-find-package pkg-str)
                        (make-package (perl-pkg-to-cl-pkg-name pkg-str) :use '(:cl :pcl)))))
          (make-p-typeglob pkg (%pcl-invert-case bare-str))))))

(defun p-glob-copy (dst-pkg dst-uname src-glob)
  "Copy all slots from src-glob into dst (pkg, uname)."
  (let ((sp (p-typeglob-package src-glob))
        (sn (p-typeglob-name src-glob)))
    ;; CODE — the alias inherits the source's declared/defined status
    ;; (default :defined for an untracked-but-fbound source), so
    ;; `defined &dst` matches `defined &src` (task #83).
    (let ((src-sym (intern (%pcl-uname-to-sub sn) sp)))
      (when (fboundp src-sym)
        (let ((dst-sym (intern (%pcl-uname-to-sub dst-uname) dst-pkg)))
          (setf (fdefinition dst-sym) (fdefinition src-sym))
          (setf (gethash dst-sym *p-declared-subs*)
                (or (gethash src-sym *p-declared-subs*) :defined)))))
    ;; SCALAR
    (let ((src-sym (intern (concatenate 'string "$" sn) sp)))
      (when (boundp src-sym)
        (setf (symbol-value (intern (concatenate 'string "$" dst-uname) dst-pkg))
              (symbol-value src-sym))))
    ;; ARRAY
    (let ((src-sym (intern (concatenate 'string "@" sn) sp)))
      (when (boundp src-sym)
        (setf (symbol-value (intern (concatenate 'string "@" dst-uname) dst-pkg))
              (symbol-value src-sym))))
    ;; HASH
    (let ((src-sym (intern (concatenate 'string "%" sn) sp)))
      (when (boundp src-sym)
        (setf (symbol-value (intern (concatenate 'string "%" dst-uname) dst-pkg))
              (symbol-value src-sym))))
    ;; IO (filehandle): copy the open-stream registration so *DST = *SRC
    ;; aliases the filehandle — e.g. `*FH = shift` in a sub that then reads
    ;; <FH>.  The handle is keyed in *p-filehandles* by the bareword symbol,
    ;; same naming convention as the scalar/array/hash slots above.
    (let ((src-sym (intern sn sp)))
      (multiple-value-bind (stream present) (gethash src-sym *p-filehandles*)
        (when present
          (setf (gethash (intern dst-uname dst-pkg) *p-filehandles*) stream))))))

(defun p-glob-undef-name (pkg-str name-str)
  "undef *foo — clear all slots."
  (let* ((pkg   (find-package (%pcl-invert-case pkg-str)))
         (uname (%pcl-invert-case name-str)))
    (when pkg
      (let ((sym (intern (%pcl-uname-to-sub uname) pkg)))
        (when (fboundp sym) (fmakunbound sym)))
      (dolist (prefix (list "$" "@" "%"))
        (let ((sym (intern (concatenate 'string prefix uname) pkg)))
          (when (boundp sym)
            (set sym (cond ((string= prefix "$")
                            (make-p-box *p-undef*))
                           ((string= prefix "@")
                            (make-array 0 :adjustable t :fill-pointer 0))
                           (t (make-hash-table :test 'equal))))))))))

(defun p-glob-slot (glob slot)
  "Read *foo{SLOT}."
  (let* ((pkg    (p-typeglob-package glob))
         (uname  (p-typeglob-name glob))
         ;; SLOT may be a literal string ("CODE") or a boxed scalar ($type) when
         ;; written as *{$glob}{$var}; to-string unboxes the latter.
         (slot-s (string-upcase (to-string slot))))
    (flet ((find-sym (prefix)
             ;; Use find-symbol (not intern) to locate inherited symbols,
             ;; e.g. @_ is pcl::@_ inherited into main — intern would create main::@_.
             (let ((nm (if (string= prefix "PL-") (%pcl-uname-to-sub uname) (concatenate 'string prefix uname)))) (or (find-symbol nm pkg) (intern nm pkg)))))
      (cond
        ((string= slot-s "CODE")
         (let ((sym (find-sym "PL-")))
           (when (fboundp sym) (make-p-box (fdefinition sym)))))
        ((string= slot-s "SCALAR")
         (let ((sym (find-sym "$")))
           (when (boundp sym) (make-p-box (symbol-value sym)))))
        ((string= slot-s "ARRAY")
         ;; Returns \@foo — an array reference (box containing the vector).
         (let ((sym (find-sym "@")))
           (when (boundp sym) (make-p-box (symbol-value sym)))))
        ((string= slot-s "HASH")
         ;; Returns \%foo — a hash reference (box containing the hash-table).
         (let ((sym (find-sym "%")))
           (when (boundp sym) (make-p-box (symbol-value sym)))))
        ((string= slot-s "IO")
         ;; *FH{IO} is the handle itself — the idiom Test.pm uses to stash
         ;; STDOUT ($TESTOUT = *STDOUT{IO}) before anything can reopen it.
         ;; Resolve through the ONE filehandle resolver and hand back a box
         ;; holding the stream, which every print/printf/read path already
         ;; accepts.  perl yields undef when the glob has no IO slot.
         (let ((h (%p-resolve-fh glob)))
           (if h (make-p-box h) *p-undef*)))
        ;; NAME/PACKAGE are the only slots that hand a name back to PERL, so
        ;; they must undo the case inversion the glob stores it under (*STDOUT
        ;; is kept as "stdout").  %pcl-invert-case is its own inverse in every
        ;; branch — all-upper flips, all-lower flips, mixed is identity — so
        ;; one call restores the source spelling.
        ((string= slot-s "NAME")
         (make-p-box (%pcl-invert-case (p-typeglob-name glob))))
        ((string= slot-s "PACKAGE")
         (make-p-box (%pcl-cl-pkg-to-perl-name (p-typeglob-package glob))))
        ((string= slot-s "GLOB")    glob)
        ;; Every other slot name (FORMAT, or a bareword that is no slot at all)
        ;; is undef in perl too — `*STDOUT{XYZZY}` yields undef, it does not
        ;; die — so this default IS the Perl semantics, not a swallowed case.
        (t *p-undef*)))))

(defun %p-glob-syms (pkg-str name-str)
  "Resolve the four slot symbols of typeglob *PKG::NAME (creating the package if
   needed).  Returns (values code-sym scalar-sym array-sym hash-sym)."
  (let* ((pkg   (or (%pcl-find-package pkg-str)
                    (make-package (perl-pkg-to-cl-pkg-name pkg-str) :use '(:cl :pcl))))
         (uname (%pcl-invert-case name-str)))
    (values (intern (%pcl-uname-to-sub uname) pkg)
            (intern (concatenate 'string "$"   uname) pkg)
            (intern (concatenate 'string "@"   uname) pkg)
            (intern (concatenate 'string "%"   uname) pkg))))

(defun %p-glob-save (code-sym scalar-sym array-sym hash-sym)
  "Snapshot the four glob slots for a later local restore.  Returns an opaque
   vector of had-bound flags + saved values."
  (vector (fboundp code-sym)   (when (fboundp code-sym)   (fdefinition code-sym))
          (boundp scalar-sym)  (when (boundp scalar-sym)  (symbol-value scalar-sym))
          (boundp array-sym)   (when (boundp array-sym)   (symbol-value array-sym))
          (boundp hash-sym)    (when (boundp hash-sym)    (symbol-value hash-sym))))

(defun %p-glob-clear (code-sym scalar-sym array-sym hash-sym)
  "Reset the four glob slots to fresh empties (Perl: local *foo starts fresh)."
  (when (fboundp code-sym) (fmakunbound code-sym))
  (setf (symbol-value scalar-sym) (make-p-box *p-undef*))
  (setf (symbol-value array-sym)  (make-array 0 :adjustable t :fill-pointer 0))
  (setf (symbol-value hash-sym)   (make-hash-table :test 'equal)))

(defun %p-glob-restore (saved code-sym scalar-sym array-sym hash-sym)
  "Restore the four glob slots from a %p-glob-save snapshot."
  (if (aref saved 0) (setf (fdefinition code-sym) (aref saved 1))
      (when (fboundp code-sym) (fmakunbound code-sym)))
  (if (aref saved 2) (setf (symbol-value scalar-sym) (aref saved 3)) (makunbound scalar-sym))
  (if (aref saved 4) (setf (symbol-value array-sym)  (aref saved 5)) (makunbound array-sym))
  (if (aref saved 6) (setf (symbol-value hash-sym)   (aref saved 7)) (makunbound hash-sym)))

(defmacro p-defcell (sym init)
  "Declare an ORDINARY package global (direction D, task #289): SYM becomes a
   symbol macro reading/writing its own global cell directly, and the cell is
   initialized ONCE.  This is the `defvar` of the cell world and must keep
   defvar's two properties, both probed s382g:

   (1) DEFINE-ONCE.  Several sections can forward-declare the same name (a
       name used as a package global in more than one section gets a
       declaration in each), and a module can be loaded twice.  `defvar`
       makes the later ones no-ops; an unconditional (setf symbol-global-value)
       would WIPE a value an earlier section already assigned — a silent
       wrong.  Hence the boundp guard.

   (2) COMPILE-TIME VISIBILITY.  Top-level `progn` keeps its subforms
       top-level, so define-symbol-macro is processed at compile time and the
       rest of the file reads SYM through the cell, exactly as defvar's
       special proclamation is seen.

   Direct cell access (sb-ext:symbol-global-value) is valid because partition
   symbols are NEVER dynamically bound — that is what Pl::GlobalPartition
   decides, and the exception set ($a/$b, punctuation/caret magic) keeps
   plain defvar instead.  Getting that partition wrong is LOUD: SBCL refuses
   a symbol that is both special and a symbol macro, in either order.
   Name-based access (symbol-value, boundp, makunbound, the glob and
   symbolic-ref helpers) reaches the same cell unchanged.

   NOTE for readers: a lexical `let` of SYM is legal CL and SHADOWS the
   symbol macro rather than erroring.  That is deliberate — it is how a perl
   `my` shadow becomes a real lexical (and 36% faster than the dynamic
   rebind it replaced) — but it means an emitter must never `let`-bind a
   name it declared here expecting to write the global."
  `(progn
     (define-symbol-macro ,sym (sb-ext:symbol-global-value ',sym))
     (unless (boundp ',sym)
       (setf (sb-ext:symbol-global-value ',sym) ,init))
     ',sym))

(defmacro p-local-cell (sym init &body body)
  "Direction-D `local` on a SYMBOL-MACRO GLOBAL (task #289, plan
   docs/direction-d-plan.md): save the cell, install INIT (the emitter
   passes the same p-box-for-local / p-copy-array / p-copy-hash form it
   builds for the dynamic-let lowering today), restore on exit — die
   path included via unwind-protect.  Access goes through
   sb-ext:symbol-global-value, matching the access macro: partition
   symbols are NEVER dynamically bound, so the direct global cell is the
   one source of truth (probed s382d, incl. symbol-value interop).
   Exception-set names ($a/$b, runtime magic) keep the dynamic-let
   lowering and must never be routed here.

   THE BODY ALSO REBINDS SYM LEXICALLY to the installed container.  In
   ordinary code that is a pure alias — the lexical and the cell hold the
   same box, so it changes nothing.  It matters where something ELSE has
   already shadowed the name lexically, and the one thing that does is the
   string-eval thunk: `p-eval-thunk` passes each free name as a lambda
   PARAMETER, so inside an eval body `$foo` is the parameter, not the cell.
   Without this rebinding, `local($foo)` would install a fresh box in the
   cell while the body kept reading and writing the parameter's box — and a
   NESTED eval, which resolves `$foo` by name, would see the untouched
   installed box.  eval.t's recursive-factorial idiom
   (`local($foo)=$foo; $foo <= 1 ? 1 : $foo-- * (eval $fact)`) then never
   terminates: the decrement is invisible to the recursion's own guard.
   Before direction D the parameter was a DYNAMIC binding of a special, so
   parameter and cell were the same storage by construction; this `let`
   restores that agreement in the cell world."
  (let ((old (gensym "OLD")))
    ;; The cell may be UNBOUND: a name that is `my`-declared in one block and
    ;; `local`-ed in another gets no forward declaration at all (the
    ;; declaration pass excludes every name the section let-binds — task
    ;; #205's shape, live in perl-tests/sort.t's `local $sortsub`).  Perl
    ;; localizes the package variable there regardless, so save "was unbound"
    ;; and restore it by makunbound rather than reading a cell that has none.
    `(let ((,old (if (boundp ',sym) (sb-ext:symbol-global-value ',sym) '%p-cell-unbound)))
       (setf (sb-ext:symbol-global-value ',sym) ,init)
       (unwind-protect (let ((,sym (sb-ext:symbol-global-value ',sym)))
                         (declare (ignorable ,sym))
                         ,@body)
         (if (eq ,old '%p-cell-unbound)
             (makunbound ',sym)
             (setf (sb-ext:symbol-global-value ',sym) ,old))))))

(defmacro p-local-glob (pkg-str name-str &body body)
  "Save all slots of *pkg::name, clear them (Perl local *foo = fresh glob),
   execute body, restore on exit."
  (let ((cs (gensym "CS")) (ss (gensym "SS")) (as (gensym "AS")) (hs (gensym "HS"))
        (sv (gensym "SAVED")))
    `(multiple-value-bind (,cs ,ss ,as ,hs) (%p-glob-syms ,pkg-str ,name-str)
       (let ((,sv (%p-glob-save ,cs ,ss ,as ,hs)))
         (%p-glob-clear ,cs ,ss ,as ,hs)
         (unwind-protect (progn ,@body)
           (%p-glob-restore ,sv ,cs ,ss ,as ,hs))))))

(defmacro p-local-glob-if (cond-form pkg-str name-str rhs-form &body body)
  "The deprecated conditional-local idiom `local *foo = RHS if COND`
   (e.g. Text::ParseWords::old_shellwords: `local *_ = \\join('',@_) if @_`).
   Perl does NOT localize at all when COND is false (the rest of the scope sees
   the outer slots); when COND is true it localizes+assigns for the rest of the
   scope.  We always save+restore (a no-op when COND is false, since the slots
   are untouched), but only clear+assign when COND is true.  RHS-FORM is
   evaluated while the slots are still intact — so an RHS that reads @_ (which
   *foo's localization would otherwise clear) sees the pre-local @_.  COND-FORM
   is already a CL boolean (the codegen wraps it in p-true-p / its negation)."
  (let ((cs (gensym "CS")) (ss (gensym "SS")) (as (gensym "AS")) (hs (gensym "HS"))
        (sv (gensym "SAVED")) (rv (gensym "RHS")))
    `(multiple-value-bind (,cs ,ss ,as ,hs) (%p-glob-syms ,pkg-str ,name-str)
       (let ((,sv (%p-glob-save ,cs ,ss ,as ,hs)))
         (when ,cond-form
           (let ((,rv ,rhs-form))
             (%p-glob-clear ,cs ,ss ,as ,hs)
             (p-glob-assign ,pkg-str ,name-str ,rv)))
         (unwind-protect (progn ,@body)
           (%p-glob-restore ,sv ,cs ,ss ,as ,hs))))))

;;; Helper functions for p-local-hash-elem macros.
;;; Delegating to functions keeps macro expansions compact, preventing heap
;;; exhaustion on large files with many local $hash{key} forms.

(defun %p-lhe-save (hv kv)
  "Save hash[key] for local. Returns saved state vector."
  (if (eq hv '%ENV-MARKER%)
      (let ((old (sb-posix:getenv kv)))
        (sb-posix:unsetenv kv)
        (vector :env old))
      (multiple-value-bind (old-bx old-ex) (gethash kv hv)
        ;; Install fresh undef box so body assignments don't clobber saved box.
        (setf (gethash kv hv) (make-p-box nil))
        (vector :hash old-ex old-bx))))

(defun %p-lhe-restore (hv kv saved)
  "Restore hash[key] after local exits."
  (if (eq (aref saved 0) :env)
      (let ((old (aref saved 1)))
        (if old (sb-posix:setenv kv old 1) (sb-posix:unsetenv kv)))
      (let ((old-ex (aref saved 1)) (old-bx (aref saved 2)))
        (if old-ex (setf (gethash kv hv) old-bx) (remhash kv hv)))))

(defun %p-lhe-init (hv kv init-val)
  "Save hash[key] and install init-val. Returns saved state vector.
   init-val goes through box-set (not raw make-p-box) so the localized elem has
   the same box shape as ordinary (setf p-gethash) — a raw-wrapped ref defeats
   p-autoviv-gethash's unboxing and gets clobbered (Moo: local $self->{captures}
   = {} then $self->{captures}{$k} = ...)."
  (if (eq hv '%ENV-MARKER%)
      (let* ((old (sb-posix:getenv kv))
             (s (if (or (null init-val) (eq init-val *p-undef*))
                    nil (to-string init-val))))
        (if s (sb-posix:setenv kv s 1) (sb-posix:unsetenv kv))
        (vector :env old))
      (multiple-value-bind (old-bx old-ex) (gethash kv hv)
        (let ((bx (make-p-box nil)))
          (box-set bx init-val)
          (setf (gethash kv hv) bx))
        (vector :hash old-ex old-bx))))

(defmacro p-local-hash-elem (hash-var key-form &body body)
  "Save/restore one hash entry. Like Perl's local $hash{key}.
   Handles %ENV (environment variables) specially."
  (let ((hv (gensym "H")) (kv (gensym "K")) (sv (gensym "S")))
    `(let* ((,hv (unbox ,hash-var))
            (,kv (to-string ,key-form))
            (,sv (%p-lhe-save ,hv ,kv)))
       (unwind-protect (progn ,@body)
         (%p-lhe-restore ,hv ,kv ,sv)))))

(defmacro p-local-array-elem (arr-var idx-form &body body)
  "Save/restore one array element. Like Perl's local $arr[N].
   For existing elements: installs a fresh undef box (isolates body from saved box).
   For non-existing elements: does NOT extend the array; body can extend via setf p-aref.
   On exit: restores existing element (re-extending if body shrank array via undef @arr),
   or trims trailing nil slots if element was non-existent."
  (let ((iv       (gensym "IDX"))
        (orig-len (gensym "ORIG-LEN"))
        (old-ex   (gensym "OLD-EXISTS"))
        (old-bx   (gensym "OLD-BOX")))
    `(let* ((,iv       (let ((i (truncate (to-number ,idx-form))))
                         (if (< i 0) (max 0 (+ (length ,arr-var) i)) i)))
            (,orig-len (length ,arr-var))
            (,old-ex   (< ,iv ,orig-len))
            (,old-bx   (when ,old-ex (aref ,arr-var ,iv))))
       ;; Only install fresh box if element existed (prevents old-box mutation by body)
       (when ,old-ex
         (setf (aref ,arr-var ,iv) (make-p-box nil)))
       (unwind-protect
            (progn ,@body)
         ;; Restore on any exit path
         (if ,old-ex
             ;; Element existed: restore original box.
             ;; Re-extend with nil slots if body shrank the array (e.g. via undef @arr).
             (progn
               (when (>= ,iv (length ,arr-var))
                 (dotimes (n (1+ (- ,iv (length ,arr-var))))
                   (vector-push-extend nil ,arr-var)))
               (setf (aref ,arr-var ,iv) ,old-bx))
             ;; Element didn't exist: mark as nil if body created it, then trim
             ;; trailing nil slots (preserves non-nil body-assigned elements).
             (progn
               (when (< ,iv (length ,arr-var))
                 (setf (aref ,arr-var ,iv) nil))
               (loop while (and (> (fill-pointer ,arr-var) ,orig-len)
                                (null (aref ,arr-var (1- (fill-pointer ,arr-var)))))
                     do (decf (fill-pointer ,arr-var)))))))))

(defmacro p-local-array-elem-init (arr-var idx-form init-form &body body)
  "Like p-local-array-elem but evaluates init-form BEFORE installing fresh box.
   Used for local($a[N]) = EXPR where EXPR might read the same element."
  (let ((init-val (gensym "INIT"))
        (iv       (gensym "IDX"))
        (orig-len (gensym "ORIG-LEN"))
        (old-ex   (gensym "OLD-EXISTS"))
        (old-bx   (gensym "OLD-BOX")))
    `(let* ((,init-val ,init-form)   ; evaluate RHS BEFORE any array changes
            (,iv       (let ((i (truncate (to-number ,idx-form))))
                         (if (< i 0) (max 0 (+ (length ,arr-var) i)) i)))
            (,orig-len (length ,arr-var))
            (,old-ex   (< ,iv ,orig-len))
            (,old-bx   (when ,old-ex (aref ,arr-var ,iv))))
       ;; Extend array if needed and install a fresh box set to init-val
       (when (>= ,iv (length ,arr-var))
         (dotimes (n (1+ (- ,iv (length ,arr-var))))
           (vector-push-extend nil ,arr-var)))
       (setf (aref ,arr-var ,iv) (make-p-box ,init-val))
       (unwind-protect
            (progn ,@body)
         (if ,old-ex
             (progn
               (when (>= ,iv (length ,arr-var))
                 (dotimes (n (1+ (- ,iv (length ,arr-var))))
                   (vector-push-extend nil ,arr-var)))
               (setf (aref ,arr-var ,iv) ,old-bx))
             (progn
               (when (< ,iv (length ,arr-var))
                 (setf (aref ,arr-var ,iv) nil))
               (loop while (and (> (fill-pointer ,arr-var) ,orig-len)
                                (null (aref ,arr-var (1- (fill-pointer ,arr-var)))))
                     do (decf (fill-pointer ,arr-var)))))))))

(defmacro p-local-hash-elem-init (hash-var key-form init-form &body body)
  "Like p-local-hash-elem but evaluates init-form BEFORE installing fresh box.
   Handles %ENV (environment variables) specially.
   Used for local($h{key}) = EXPR where EXPR might read the same key."
  (let ((iv (gensym "I")) (hv (gensym "H")) (kv (gensym "K")) (sv (gensym "S")))
    `(let* ((,iv ,init-form)          ; evaluate RHS BEFORE any hash changes
            (,hv (unbox ,hash-var))
            (,kv (to-string ,key-form))
            (,sv (%p-lhe-init ,hv ,kv ,iv)))
       (unwind-protect (progn ,@body)
         (%p-lhe-restore ,hv ,kv ,sv)))))

;;; ── local on a deref / symbolic ref: local ${$x}, local @{$x}, local %$x ──
;;; Perl allows `local` through a *symbolic* reference (a string naming a package
;;; variable): it saves/restores that package variable.  Localizing through a
;;; *hard* reference is a fatal error ("Can't localize through a reference").

(defun %p-local-deref-resolve-scalar-box (val)
  "For local ${EXPR}: VAL must unbox to a symbolic-ref STRING; resolve to the
   package scalar box (vivifying if absent).  A hard reference is fatal."
  (let ((inner (unbox val)))
    (if (and inner (stringp inner))
        (or (%p-symref-box inner)
            (setf (%p-symref-box inner) (make-p-box nil)))
        (p-die "Can't localize through a reference"))))

(defun %p-local-deref-resolve-array (val)
  "For local @{EXPR}: VAL must unbox to a symbolic-ref STRING; a hard reference
   is fatal."
  (let ((inner (unbox val)))
    (if (and inner (stringp inner))
        (%p-symref-array inner)
        (p-die "Can't localize through a reference"))))

(defun %p-local-deref-resolve-hash (val)
  "For local %{EXPR}: VAL must unbox to a symbolic-ref STRING; a hard reference
   is fatal."
  (let ((inner (unbox val)))
    (if (and inner (stringp inner))
        (%p-symref-hash inner)
        (p-die "Can't localize through a reference"))))

(defmacro p-local-deref-scalar (ref-form &body body)
  "local ${EXPR} / local $$ref — save/restore a symbolic-ref'd package scalar.
   Mutates the box in place (the symbol is only known at run time), so caches
   must be invalidated on both the clear and the restore — otherwise a stale
   nv/sv cache makes the box read back its pre-restore value."
  (let ((box (gensym "BOX")) (saved (gensym "SV")))
    `(let* ((,box (%p-local-deref-resolve-scalar-box ,ref-form))
            (,saved (p-box-value ,box)))
       (setf (p-box-value ,box) nil
             (p-box-nv-ok ,box) nil
             (p-box-sv-ok ,box) nil)
       (unwind-protect (progn ,@body)
         (setf (p-box-value ,box) ,saved
               (p-box-nv-ok ,box) nil
               (p-box-sv-ok ,box) nil)))))

(defmacro p-local-deref-array (ref-form &body body)
  "local @{EXPR} / local @$ref — save/restore a symbolic-ref'd package array."
  (let ((vec (gensym "VEC")) (saved (gensym "SV")))
    `(let* ((,vec (%p-local-deref-resolve-array ,ref-form))
            (,saved (coerce ,vec 'list)))
       (setf (fill-pointer ,vec) 0)
       (unwind-protect (progn ,@body)
         (setf (fill-pointer ,vec) 0)
         (dolist (e ,saved) (vector-push-extend e ,vec))))))

(defmacro p-local-deref-hash (ref-form &body body)
  "local %{EXPR} / local %$ref — save/restore a symbolic-ref'd package hash."
  (let ((h (gensym "H")) (saved (gensym "SV")))
    `(let* ((,h (%p-local-deref-resolve-hash ,ref-form))
            (,saved (let ((a '()))
                      (maphash (lambda (k v) (push (cons k v) a)) ,h)
                      a)))
       (clrhash ,h)
       (unwind-protect (progn ,@body)
         (clrhash ,h)
         (dolist (kv ,saved) (setf (gethash (car kv) ,h) (cdr kv)))))))

(defun %p-local-array-slice-nested (arr vec pos thunk)
  "Helper: save/restore arr[idx] for each idx in vec[pos..end], then call thunk."
  (if (>= pos (length vec))
      (funcall thunk)
      (let* ((raw-idx (truncate (to-number (aref vec pos))))
             (iv      (if (< raw-idx 0)
                          (max 0 (+ (length arr) raw-idx))
                          raw-idx))
             (orig-len (length arr))
             (old-ex  (< iv orig-len))
             (old-bx  (when old-ex (aref arr iv))))
        (when old-ex (setf (aref arr iv) (make-p-box nil)))
        (unwind-protect
             (%p-local-array-slice-nested arr vec (1+ pos) thunk)
          (if old-ex
              (progn
                (when (>= iv (length arr))
                  (dotimes (n (1+ (- iv (length arr))))
                    (vector-push-extend nil arr)))
                (setf (aref arr iv) old-bx))
              (progn
                (when (< iv (length arr))
                  (setf (aref arr iv) nil))
                (loop while (and (> (fill-pointer arr) orig-len)
                                 (null (aref arr (1- (fill-pointer arr)))))
                      do (decf (fill-pointer arr)))))))))

(defmacro p-local-array-slice (arr-var idx-form &body body)
  "Localize array elements by index or range. idx-form may be a scalar index or
   an adjustable non-string vector (result of p-.. range)."
  (let ((g-idx (gensym "IDX")))
    `(let ((,g-idx ,idx-form))
       (if (and (vectorp ,g-idx)
                (adjustable-array-p ,g-idx)
                (not (stringp ,g-idx)))
           (%p-local-array-slice-nested ,arr-var ,g-idx 0 (lambda () ,@body))
           (p-local-array-elem ,arr-var ,g-idx ,@body)))))

(defun p-copy-array (src)
  "Create a fresh flat copy of SRC for 'local @arr = expr' bindings.
   Flattens nested adjustable vectors exactly like p-array-= does, so that
   'local @a = (X, @a, Y)' correctly interpolates the old @a contents."
  (let ((result (make-array 0 :adjustable t :fill-pointer 0))
        (raw (cond ((and (vectorp src) (not (stringp src))) src)
                   ((null src) nil)
                   (t (unbox src)))))
    (labels ((add-items (x)
               (cond
                 ((null x))
                 ((stringp x)
                  (vector-push-extend (make-p-box x) result))
                 ((hash-table-p x)
                  (maphash (lambda (k v)
                             (when (%p-real-hash-key-p k)
                               (vector-push-extend (make-p-box k) result)
                               (%p-array-store-scalar result v)))
                           x))
                 ((p-flatten-marker-p x)
                  (add-items (p-flatten-marker-array x)))
                 ((and (vectorp x) (not (stringp x)))
                  (loop for item across x
                        do (%p-array-fill-item item result add-items)))
                 ((listp x)
                  (loop for item in x
                        do (%p-array-fill-item item result add-items)))
                 (t
                  (%p-array-store-scalar result x)))))
      (add-items raw))
    result))

(defun p-copy-hash (h)
  "Create a fresh copy of a hash for 'local %h = expr' semantics.
   Handles both hash-table input (direct copy) and vector/list input
   (interpreted as flat k-v pairs, like p-hash-=)."
  (let* ((raw (if (or (hash-table-p h)
                      (and (vectorp h) (not (stringp h))))
                  h
                  (unbox h)))
         (copy (make-hash-table :test 'equal)))
    (cond
      ((hash-table-p raw)
       ;; Value entries are p-boxes; a plain (setf (gethash k copy) v) would
       ;; SHARE those boxes, so `my %h = %h; $h{k}=…` would mutate the source
       ;; (perl copies).  Mint a fresh entry box per real key (same copy the
       ;; vector branch below gets via %p-make-hash-entry); the internal
       ;; :__class__ bless-key is a plain value → copy verbatim.
       (maphash (lambda (k v)
                  (setf (gethash k copy)
                        (if (%p-real-hash-key-p k) (%p-make-hash-entry v) v)))
                raw))
      ((and (vectorp raw) (not (stringp raw)))
       (let ((flat (%p-flatten-list raw)))
         (loop for i from 0 below (length flat) by 2
               do (setf (gethash (to-string (aref flat i)) copy)
                        (if (< (1+ i) (length flat))
                            (%p-make-hash-entry (aref flat (1+ i)))
                            *p-undef*))))))
    copy))

;;; ============================================================
;;; Subroutine Reflection (exists &sub, defined &sub, undef &sub)
;;; ============================================================

(defun p-sub-exists (pkg-str name-str)
  "Perl exists &funcname — true if sub has been declared or defined."
  (let* ((pkg (%pcl-find-package pkg-str))
         (sym (when pkg
                (find-symbol (%pcl-cl-sub-name name-str)
                             pkg))))
    (if (and sym
             (or (gethash sym *p-declared-subs*)
                 (fboundp sym)))
        (make-p-box 1)
        (make-p-box nil))))

(defun p-sub-defined (pkg-str name-str)
  "Perl defined &funcname — true only if sub has an actual body (not a stub)."
  (let* ((pkg (%pcl-find-package pkg-str))
         (sym (when pkg
                (find-symbol (%pcl-cl-sub-name name-str)
                             pkg))))
    (if (and sym
             (eq (gethash sym *p-declared-subs*) :defined))
        (make-p-box 1)
        (make-p-box nil))))

(defun p-undef-sub (pkg-str name-str)
  "Perl undef &funcname — remove sub body; sub still 'exists' afterward."
  (let* ((pkg (%pcl-find-package pkg-str))
         (sym (when pkg
                (find-symbol (%pcl-cl-sub-name name-str)
                             pkg))))
    (when sym
      (when (fboundp sym) (fmakunbound sym))
      ;; Keep entry so exists &sub still returns true
      (setf (gethash sym *p-declared-subs*) :was-defined)))
  *p-undef*)

(defun p-coderef-exists-p (coderef)
  "Perl exists &{$coderef} — true if coderef points to a declared or defined sub.
   Accepts a real function object OR a symbolic sub-name string (no-strict-refs)."
  (let ((v (unbox coderef)))
    (when (p-box-p v) (setf v (p-box-value v)))
    (cond
      ((functionp v)
       ;; A lazy AUTOLOAD fallback stands for a sub that was never declared:
       ;; exists is false until the symbol gains a status or a definition.
       ;; Any other function object exists (declared stub or defined body).
       (let ((lazy (gethash v *p-lazy-coderef-target*)))
         (if (and lazy
                  (not (gethash lazy *p-declared-subs*))
                  (not (fboundp lazy)))
             (make-p-box nil)
             (make-p-box 1))))
      ;; Symbolic name: exists iff it resolves to a known sub (stub or defined).
      ((or (stringp v) (numberp v))
       (let ((sym (%p-resolve-sub-symbol v)))
         (if (and sym (or (gethash sym *p-declared-subs*) (fboundp sym)))
             (make-p-box 1)
             (make-p-box nil))))
      (t (make-p-box nil)))))

(defun p-coderef-defined-p (coderef)
  "Perl defined &{$coderef} — true only if coderef points to a sub with a body.
   Accepts a real function object OR a symbolic sub-name string (no-strict-refs).
   A forward-declared sub (p-declare-sub installs a :stub that IS fboundp) is NOT
   defined, so the status must be :defined (or fbound with no status = imported)."
  (let ((v (unbox coderef)))
    (when (p-box-p v) (setf v (p-box-value v)))
    (cond
      ((functionp v)
       ;; A lazy wrapper (stub trampoline / AUTOLOAD fallback) answers by its
       ;; TARGET's current status — perl's \\&foo is late-bound to the glob.
       ;; Otherwise: p-sub installs bodies as ANONYMOUS lambdas (%fun-name is
       ;; a (lambda ...) list, never the symbol), so any function that is not
       ;; a lazy wrapper and not a defun'd :stub IS a body — status :defined
       ;; can only ever be seen here for defun-defined (imported) subs.
       (let ((lazy (gethash v *p-lazy-coderef-target*)))
         (if lazy
             (if (eq (gethash lazy *p-declared-subs*) :defined)
                 (make-p-box 1)
                 (make-p-box nil))
             (let* ((fname (ignore-errors (sb-kernel:%fun-name v)))
                    (status (and (symbolp fname)
                                 (gethash fname *p-declared-subs*))))
               (if (eq status :stub)
                   (make-p-box nil)
                   (make-p-box 1))))))
      ((or (stringp v) (numberp v))
       (let* ((sym (%p-resolve-sub-symbol v))
              (status (and sym (gethash sym *p-declared-subs*))))
         (if (and sym
                  (or (eq status :defined)
                      (and (null status) (fboundp sym))))
             (make-p-box 1)
             (make-p-box nil))))
      (t (make-p-box nil)))))

;;; ============================================================
;;; Tie / Untie / Tied — scalar implementation
;;; ============================================================
;;; tie() installs a p-tie-proxy into the box's value slot.
;;; unbox() intercepts reads (FETCH); box-set() intercepts writes (STORE).
;;; Phase 1: scalars only.  Arrays/hashes require boxing those types first.

(defun %p-warn-aggregate-tie (value classname)
  "Announce an aggregate tie that PCL is about to DROP (task #155).
   Silent-wrong is the failure mode CLAUDE.md rule 12 exists to stop: the
   program runs on an UNTIED container and every FETCH/STORE the test was
   written to observe simply never happens.  A die was rejected for R1 (it
   converts mid-file tie users into crashes); see docs/not-supported.md
   'tie on an ARRAY or HASH'.

   EFFECT-ONLY, so it routes through the shared %p-announce-unsupported helper
   (ruled docs/fable-answers-s337.md §5b) — the CLASS rides in the OPERAND,
   which keeps the old per-(kind, class) dedup with no table of its own."
  (let ((kind (cond ((hash-table-p value) "a HASH")
                    ((and (vectorp value) (not (stringp value))) "an ARRAY")
                    (t "a non-lvalue")))
        (name (if (stringp classname) classname (format nil "~A" classname))))
    (%p-announce-unsupported "tie"
                             (format nil "~A (class ~A)" kind name)
                             "the container is left untied (task #155)")))

(defun p-tie (box classname &rest args)
  "Perl tie - bind a scalar variable to a class implementing TIESCALAR.
   Dispatches to TIEARRAY or TIEHASH when box holds a vector or hash-table
   (future: requires array/hash boxing for full correctness).
   Falls back gracefully if the tie class or method is not available."
  (unless (p-box-p box)
    ;; An ARRAY/HASH arrives here RAW — a bare hash-table or vector, with no
    ;; box to hold a tie proxy — so the tie is dropped.  Say so out loud.
    (%p-warn-aggregate-tie box classname)
    (return-from p-tie *p-undef*))
  (let* ((current (p-box-value box))
         (constructor (cond
                        ((and (vectorp current) (not (stringp current))) "TIEARRAY")
                        ((hash-table-p current) "TIEHASH")
                        (t "TIESCALAR")))
         (tie-result (handler-case
                         (apply #'p-method-call classname constructor args)
                       (error (e)
                         (warn "PCL: tie ~A->~A failed: ~A" classname constructor e)
                         (return-from p-tie *p-undef*))))
         ;; Re-tying an already-tied variable REPLACES the magic; it does not
         ;; stack.  Carry the raw slot across, or the old proxy becomes the new
         ;; proxy's "raw" value and every magic-off read hands a live proxy back
         ;; to the caller (join.t, tied separator tied three times).
         (proxy (make-p-tie-proxy :tie-obj tie-result
                                  :saved-value (if (p-tie-proxy-p current)
                                                   (p-tie-proxy-saved-value current)
                                                   current))))
    (setf (p-box-value box) proxy
          (p-box-sv-ok box) nil
          (p-box-nv-ok box) nil)
    tie-result))

(defun p-untie (box)
  "Perl untie - remove tie from variable, restoring its pre-tie value.
   Calls UNTIE on the tie object if the method exists."
  (when (p-box-p box)
    (let ((v (p-box-value box)))
      (cond
        ((p-tie-proxy-p v)
         (ignore-errors
           (p-method-call (p-tie-proxy-tie-obj v) "UNTIE"))
         (setf (p-box-value box) (p-tie-proxy-saved-value v)
               (p-box-sv-ok box) nil
               (p-box-nv-ok box) nil))
        ;; untie() from inside this box's own tie handler: the raw slot is
        ;; already exposed, so just mark the proxy dead so %with-tie-magic-off
        ;; does not reinstall it on the way out.
        ((%p-suppressed-proxy box)
         (let ((proxy (%p-suppressed-proxy box)))
           (ignore-errors
             (p-method-call (p-tie-proxy-tie-obj proxy) "UNTIE"))
           (setf (p-tie-proxy-untied proxy) t))))))
  (make-p-box 1))

(defun p-tied (box)
  "Perl tied() - returns the tie object if box is tied, undef otherwise."
  (if (p-box-p box)
      (let ((v (p-box-value box)))
        (cond
          ((p-tie-proxy-p v) (p-tie-proxy-tie-obj v))
          ;; Magic is temporarily off (one of this box's handlers is running),
          ;; but perl's tied() still reports the object there (probed s342).
          ((%p-suppressed-proxy box)
           (p-tie-proxy-tie-obj (%p-suppressed-proxy box)))
          (t *p-undef*)))
      *p-undef*))

(defun p-scalar (&rest args)
  "Perl scalar function - returns length for arrays, value for scalars.
   With multiple args (comma expr), returns the last value."
  (let* ((val (car (last args)))
         (v (unbox val)))
    (cond
      ;; A REFERENCE is already a scalar and scalar() NEVER dereferences.
      ;; `unbox` peels the ref-wrapper, so `scalar(\5)` used to answer with the
      ;; referent (ref(scalar(\5)) = "" where perl says SCALAR).
      ((and (p-box-p val) (p-box-is-ref val)) val)
      ;; Strings are scalars, return as-is
      ((stringp v) v)
      ;; Arrays (non-string vectors) return length — but a BOX holding a vector
      ;; is an array REFERENCE, not an array: an array VARIABLE is a raw
      ;; adjustable vector and is never boxed.  Unboxing first threw that
      ;; distinction away, so `scalar($aref)` and `scalar([1,2])` both answered
      ;; with the ELEMENT COUNT and `ref(scalar($aref))` was "" — a reference
      ;; silently turned into a number.  The hash branch below already carries
      ;; exactly this (not (p-box-p val)) guard; the array branch never got it.
      ((and (vectorp v) (adjustable-array-p v))
       (if (p-box-p val) val (length v)))
      ;; Perl 5.26+: plain %hash (not a hash ref) in scalar context → key count
      ((and (hash-table-p v) (not (p-box-p val))) (%p-hash-user-count v))
      ;; An undef result is the scalar undef: return the *p-undef* sentinel, not
      ;; raw nil.  scalar(EXPR) ALWAYS produces a single scalar, so e.g.
      ;; `scalar(eval { die })` must contribute exactly one undef element to a
      ;; surrounding list.  Raw nil is ambiguous with the empty list and gets
      ;; spliced away by p-flatten-args (the `return (scalar(eval{...}), $@)`
      ;; idiom in Try::Tiny / test helpers); the sentinel survives like literal
      ;; undef does.
      ((null v) *p-undef*)
      ;; Everything else (numbers, hash refs, etc.) returns as-is
      (t v))))

(defun p-wantarray ()
  "Perl wantarray(): 1 in list context, \"\" in scalar, undef in void.
   Reads *pcl-caller-wantarray* (set at sub entry) so it reflects the caller's
   context even when gen_funcall has overridden *wantarray* for a nested call."
  (cond ((eq *pcl-caller-wantarray* t)     1)
        ((eq *pcl-caller-wantarray* :void) (p-undef))
        (t                                 "")))

(defun p-caller (&optional (level 0))
  "Perl caller - return information about the calling subroutine.
   In scalar context, returns package name.
   In list context, returns (package filename line subroutine).
   Returns nil when called from the top level (not inside any PCL sub)."
  ;; At top level (depth 0), caller() always returns undef/nil in Perl.
  ;; This is the common case: 'run_tests() unless caller' at top level.
  (when (zerop *pcl-sub-call-depth*)
    (return-from p-caller nil))
  ;; Package of the frame's caller comes from the dynamic caller stack pushed at
  ;; each p-sub entry (the only source that preserves Perl package case).
  (let ((lvl (if (p-box-p level) (truncate (to-number level)) level)))
    (when (>= lvl (length *pcl-caller-pkg-stack*))
      (return-from p-caller nil))
    (setf level lvl))
  (let* ((caller-package (nth level *pcl-caller-pkg-stack*))
         ;; Subroutine name (caller(N))[3] comes from the dedicated subname stack;
         ;; SBCL's backtrace can't name our subs (anonymous lambdas).
         (caller-subname (or (nth level *pcl-caller-subname-stack*) "(unknown)"))
         (frame-info nil)
         (current-level 0)
         (target-level (+ level 2)))  ; Skip p-caller itself and its caller
    ;; Walk the backtrace for best-effort filename/line (documented unreliable).
    (sb-debug::map-backtrace
     (lambda (frame)
       (when (= current-level target-level)
         (let ((code-loc (sb-di:frame-code-location frame)))
           (setf frame-info
                 (list caller-package  ; Package of the calling frame (from stack)
                       (or (ignore-errors
                             (sb-di:debug-source-namestring
                              (sb-di:code-location-debug-source code-loc)))
                           "-")  ; Filename
                       (or (ignore-errors
                             (sb-di:code-location-toplevel-form-offset code-loc))
                           0)  ; Line number approximation
                       caller-subname))))
       (incf current-level)
       ;; Return nil to continue, non-nil would stop
       nil))
    ;; The package/subname always come from the caller stacks (above); if the
    ;; backtrace walk couldn't locate a matching frame for filename/line, still
    ;; report package + subname with placeholder location info.
    (unless frame-info
      (setf frame-info (list caller-package "-" 0 caller-subname)))
    (if (eq *wantarray* t)
        ;; List context: Perl returns (package, filename, line, subname, ...).
        ;; Return a PCL list-vector (like p-localtime) — NOT (values-list ...),
        ;; whose extra CL values get truncated to one by the calling form, which
        ;; is why (caller(N))[3] (e.g. Exporter::as_heavy) used to read undef.
        (make-array (length frame-info) :initial-contents frame-info
                    :adjustable t :fill-pointer t)
        (first frame-info))))  ; Scalar context: just package

;; Prototype registry: function object -> prototype string.  Populated by
;; p-__pcl_set-prototype, which the transpiler's :prototype(...) attribute
;; desugar emits (and lib/Sub/Util.pm's set_prototype calls).  Subs without a
;; registered prototype report undef — signatures never register one, matching
;; perl (a signature is not a prototype).  Weak by key (like
;; *p-lazy-coderef-target*): a dropped anon coderef must not be pinned by its
;; prototype entry.
(defvar %pcl-sub-prototypes (make-hash-table :test #'eq :weakness :key))

(defun %p-code-function (val)
  "Unwrap a code-ref value (box / blessed double-box / symbolic name) to the
   underlying CL function object, or nil."
  (let ((fn (unbox val)))
    (when (p-box-p fn)
      (setf fn (p-box-value fn)))
    (if (functionp fn)
        fn
        (when (or (stringp fn) (numberp fn))
          (p-get-coderef fn)))))

(defun p-__pcl_set_prototype (code proto)
  "Register PROTO as the Perl prototype of the sub CODE refers to; returns CODE.
   Perl-side spelling: __pcl_set_prototype($code, $proto) — emitted by the
   :prototype(...) attribute desugar in Pl/Parser.pm."
  (let ((fn (%p-code-function code)))
    (when fn
      (setf (gethash fn %pcl-sub-prototypes)
            (if (%pcl-definedp proto) (to-string (unbox proto)) nil))))
  code)

(defun p-prototype (&optional ref)
  "Perl prototype() - returns the prototype string of a function, or undef.
   Only prototypes declared via the :prototype(...) attribute (or
   Sub::Util::set_prototype) are tracked; classic `sub f ($$)` prototypes are
   consumed at transpile time and report undef here."
  (let* ((fn (%p-code-function ref))
         (proto (and fn (gethash fn %pcl-sub-prototypes))))
    (or proto *p-undef*)))

;;; ============================================================
;;; OO Support
;;; ============================================================

;; Simple blessing - store class name in hash
(defun p-bless (ref class)
  "Perl bless - attach class to a reference (hash, array, or scalar ref).
   For hashes: stores class in :__class__ key (survives unboxing).
   For arrays/code/other: stores class on the box's class slot."
  ;; Perl throws "Attempt to bless into a reference" when class is a non-overloaded ref.
  (let ((ref-type-of-class (p-ref class)))
    (when (and (string/= ref-type-of-class "")
               (not (p-find-overload class "\"\"")))
      (error "Attempt to bless into a reference")))
  (let* ((raw-class-val (unbox class))
         ;; Detect Perl undef (nil or *p-undef*): emits 2 warnings
         (is-undef (or (null raw-class-val) (eq raw-class-val *p-undef*)))
         (raw-class (to-string class))
         ;; Empty string or Perl undef class: default to current package + warn
         (class-name (if (string= raw-class "")
                         (let ((pkg (package-name *package*)))
                           ;; undef arg: also warn "Use of uninitialized value"
                           (when is-undef
                             (p-warn (make-p-box "Use of uninitialized value in bless")))
                           (p-warn (make-p-box "Blessing into '' is deprecated"))
                           ;; CL main package is "MAIN", Perl uses "main"
                           (if (string= pkg "MAIN") "main" pkg))
                         raw-class))
         (inner (unbox ref)))
    ;; Ensure CL package exists for this class name (mirrors Perl stash creation on bless).
    ;; Lets p-method-call distinguish "blessed into" from "never mentioned" packages,
    ;; so it can add the "(perhaps you forgot to load...)" hint only for truly unknown classes.
    (unless (%pcl-find-package class-name)
      (ignore-errors (make-package (perl-pkg-to-cl-pkg-name class-name) :use '(:cl :pcl))))
    (cond
      ((hash-table-p inner)
       (setf (gethash :__class__ inner) class-name)
       ;; Also set on box if ref is a box (so box-set can copy it)
       (when (p-box-p ref) (setf (p-box-class ref) class-name)))
      ((p-box-p inner)
       ;; Perl blesses SvRV -- the TARGET the ref value denotes.
       (let ((scalar-referent (%p-scalar-ref-referent ref)))
         (if scalar-referent
             ;; Scalar reference: the SCALAR's own stash, and ONLY that --
             ;; what the scalar happens to HOLD (e.g. a hash ref) keeps its
             ;; own class untouched (bless.t 25-32: blessing \$a1 as "F"
             ;; must not change the class of the object in $a1).
             (setf (p-box-class scalar-referent) class-name)
             ;; Aggregate ref through a variable (double-boxed): `inner`
             ;; IS the target box -- restamp it (and a hash target's
             ;; :__class__), not just the variable's slot, or a re-bless
             ;; would be invisible through other aliases.
             (progn
               (setf (p-box-class inner) class-name)
               (let ((rv (p-box-value inner)))
                 (when (hash-table-p rv)
                   (setf (gethash :__class__ rv) class-name))))))
       ;; The variable/wrapper write stays as the cache box-set copies
       ;; around (fast "is this an object" checks read it).
       (when (p-box-p ref) (setf (p-box-class ref) class-name)))
      (t
       ;; Array, code, or other ref type - store class on the box
       (if (p-box-p ref)
           (setf (p-box-class ref) class-name)
           ;; ref is a raw function (e.g. anonymous sub from codegen). Wrap it in
           ;; a new box with the class set so box-set can propagate the class to
           ;; the variable box (box-set copies class from value-box to target-box).
           (return-from p-bless (make-p-box ref class-name)))))
    ref))

(defun p-get-class (obj)
  "Get the class name of a blessed object or class string"
  (cond
    ((stringp obj) obj)  ;; Class name string (for Counter->new())
    ((hash-table-p obj) (gethash :__class__ obj))
    ((p-box-p obj)
     ;; Target class first (perl reads SvSTASH(SvRV): the referent of a
     ;; scalar ref, a hash's :__class__, a classed target box -- the
     ;; variable box's own slot is the SCALAR's stash plus a cache, and
     ;; must not shadow the target: bless.t 29-32), then the slot, but
     ;; only when the payload is a REF shape (a plain scalar's slot is
     ;; its own SvSTASH -- not the class of a value it doesn't hold).
     ;; (class-slot check BEFORE the shape scan: the slot is almost always
     ;; nil, the shape scan is 7 type tests -- hot-path order matters.)
     (or (%p-target-class obj)
         (and (p-box-class obj)
              (%p-ref-shaped-p (p-box-value obj))
              (p-box-class obj))))
    (t nil)))

(defun %pcl-invocant-class (invocant)
  "The class name a method-call invocant denotes: a blessed object's class, or
   a plain string (raw or in a scalar) treated as a class name — `my $c=\"Foo\";
   $c->m` dispatches against package Foo, just like the literal `\"Foo\"->m`.
   Shared by p-method-call, p-can and p-isa so every dispatch path agrees.
   NOTE: deliberately distinct from p-get-class, which must keep reporting NIL
   for a boxed plain string (overload/ref checks treat it as a value, not a class)."
  (cond
    ((stringp invocant) invocant)
    ((hash-table-p invocant) (gethash :__class__ invocant))
    ((p-box-p invocant)
     (or (p-get-class invocant)
         (let ((uv (unbox invocant)))
           (when (stringp uv) uv))))
    (t nil)))

(defun p-resolve-invocant (name)
  "Resolve a bareword invocant for method calls.
   In Perl, Foo->bar() checks if sub Foo exists first:
   - If pl-Foo is a user-defined function in current package → call it, return result (object)
   - Otherwise → return the string as a class name"
  (let* ((func-name (%pcl-cl-sub-name name))
         ;; Look in current package for user-defined sub, NOT in :pcl (which has built-ins)
         (func-sym (find-symbol func-name *package*)))
    (if (and func-sym (eq (symbol-package func-sym) *package*) (fboundp func-sym))
        ;; User sub exists - call it to get the object
        (funcall func-sym)
        ;; No user sub - return string as class name
        name)))

(defun %pcl-normalize-pkg (pkg-str)
  "Strip Perl's root-stash `main::` prefix: `main::Foo` and `Foo` name the same
   package.  Used so class-name comparisons (isa) agree regardless of prefix."
  (let ((s (to-string pkg-str)))
    (if (and (> (length s) 6) (string= (subseq s 0 6) "main::"))
        (subseq s 6)
        s)))

(defun %pcl-find-package (pkg-str)
  "Find CL package for Perl package name PKG-STR.
   Tries upcase first (single-word packages defined via :Foo keyword), then
   exact case (multi-level packages defined via :|Foo::Bar| notation).
   A leading `main::` is Perl's root-stash prefix: `main::Foo` names the very
   same package as `Foo` (e.g. `\"main::Alice\"->new`), so retry without it."
  (or (find-package (perl-pkg-to-cl-pkg-name pkg-str))
      (find-package (%pcl-invert-case pkg-str))
      (find-package pkg-str)
      (when (and (> (length pkg-str) 6)
                 (string= (subseq pkg-str 0 6) "main::"))
        (%pcl-find-package (subseq pkg-str 6)))))

(defun p-method-call (obj method &rest args)
  "Perl method call - looks up p-METHOD function in object's package and walks MRO for inheritance"
  ;; Method argument lists flatten like any Perl call: $o->m(@a, %h) spreads its
  ;; arrays/hashes.  The codegen passes raw @arrays straight through, so flatten
  ;; here once — built-in methods (p-isa/p-can) take fixed scalar args, and user
  ;; methods re-flatten their already-flat %_args harmlessly.
  (setf args (coerce (p-flatten-args args) 'list))
  ;; $obj->$coderef(@args): when the method slot holds a CODE ref (rather than a
  ;; method-name string), Perl invokes it directly as $coderef->($obj, @args),
  ;; bypassing package/MRO lookup.  Used by Safe::Isa ($_isa/$_can) and any
  ;; `$obj->$method` where $method was set to \&some_sub.
  (let ((m (unbox method)))
    (when (p-box-p m) (setf m (p-box-value m)))  ; double-boxed blessed coderef
    (when (functionp m)
      (return-from p-method-call (apply m obj args))))
  (let* ((method-name (to-string method))
         ;; If obj is a box containing a tie-proxy, FETCH to get the invocant
         (resolved-obj (if (and (p-box-p obj)
                                (p-tie-proxy-p (p-box-value obj)))
                           (unbox (%p-tie-fetch obj (p-box-value obj)))
                           obj))
         (raw-class (%pcl-invocant-class resolved-obj))
         ;; Perl treats "" as "main" and "::" as "main::" in package/method contexts.
         ;; Leading "::" on a class name refers to the root stash (same as no prefix).
         (class-name (let ((c (or raw-class "")))
                       (cond
                         ((string= c "")   "main")
                         ((string= c "::") "main::")
                         ;; "::Foo::Bar" → "Foo::Bar" (strip leading root-stash "::")
                         ((and (>= (length c) 2)
                               (string= (subseq c 0 2) "::"))
                          (subseq c 2))
                         (t c)))))
    (unless class-name
      (error "Can't call method ~A on non-blessed reference" method-name))

    ;; A method call on undef or an UNBLESSED reference is a fatal Perl error
    ;; ("Can't call method X on an undefined value" / "on unblessed reference").
    ;; Real code relies on this firing under eval — e.g. Safe::Isa's $_isa/$_can
    ;; guard `$thing->$_isa(...)` against non-objects.  raw-class is nil for undef,
    ;; unblessed refs, AND plain strings/numbers; only the first two die — a plain
    ;; string is a legitimate class name (e.g. `my $c="Foo"; $c->m`), handled below.
    (when (null raw-class)
      (let ((uv (unbox resolved-obj)))
        (cond
          ((or (null uv) (eq uv *p-undef*))
           (error "Can't call method ~A on an undefined value" method-name))
          ((plusp (length (the string (p-ref resolved-obj))))
           (error "Can't call method ~A on unblessed reference" method-name)))))

    ;; Auto-load the package if it doesn't exist yet.
    ;; This mirrors how Perl automatically has core modules (like version.pm)
    ;; pre-loaded in its runtime.  When user code writes `new version ~$_` or
    ;; `SomeModule->method` without an explicit `use`, we attempt a require here
    ;; so the package can be found during dispatch.
    (when (null (%pcl-find-package class-name))
      (handler-case (p-require class-name) (error () nil)))

    ;; Dynamic SUPER:: dispatch: $obj->$method where $method = "SUPER::foo"
    ;; Perl treats this as calling SUPER's foo from the object's own package.
    (when (and (stringp method-name)
               (> (length method-name) 7)
               (string= (subseq method-name 0 7) "SUPER::"))
      (let ((real-method (subseq method-name 7)))
        (return-from p-method-call
          (apply #'p-super-call resolved-obj real-method class-name args))))

    ;; Qualified method dispatch: $obj->PKG::method(args) calls PKG::method($obj, args)
    ;; directly, bypassing normal MRO. E.g. Foo->UNIVERSAL::can("x").
    ;; Also handles PKG::SUPER::method (call method from PKG's parent).
    (let ((first-sep (search "::" method-name)))
      (when first-sep
        ;; Split at the first "::" first, then check for PKG::SUPER::method pattern.
        ;; For multi-level names like "E::D::foo", use the LAST "::" as the split
        ;; (so pkg-part="E::D", meth-part="foo"), UNLESS meth-part starts with "SUPER::"
        ;; (so "PKG::SUPER::method" stays split as pkg-part="PKG", meth-part="SUPER::method").
        (let* ((first-meth (subseq method-name (+ first-sep 2)))
               (sep-pos (if (and (>= (length first-meth) 7)
                                 (string= (subseq first-meth 0 7) "SUPER::"))
                            first-sep
                            ;; Find last "::" in method-name
                            (let ((last first-sep))
                              (loop for i from (1+ first-sep) below (1- (length method-name))
                                    when (and (char= (char method-name i) #\:)
                                              (char= (char method-name (1+ i)) #\:))
                                    do (setf last i))
                              last)))
               (pkg-part   (subseq method-name 0 sep-pos))
               (meth-part  (subseq method-name (+ sep-pos 2)))
               (target-pkg (%pcl-find-package pkg-part)))
          ;; PKG::SUPER::method — call method from PKG's parent class
          (when (and (>= (length meth-part) 7)
                     (string= (subseq meth-part 0 7) "SUPER::"))
            (let ((real-method (subseq meth-part 7)))
              (return-from p-method-call
                (apply #'p-super-call resolved-obj real-method pkg-part args))))
          ;; UNIVERSAL built-ins
          (cond
            ((string-equal pkg-part "UNIVERSAL")
             (return-from p-method-call
               (cond
                 ((string-equal meth-part "can")  (apply #'p-can  resolved-obj args))
                 ((string-equal meth-part "isa")  (apply #'p-isa  resolved-obj args))
                 ((string-equal meth-part "DOES") (apply #'p-isa  resolved-obj args))
                 (t (when target-pkg
                      (let ((fn (find-symbol (%pcl-cl-sub-name meth-part)
                                             target-pkg)))
                        (when (and fn (fboundp fn))
                          (return-from p-method-call (apply fn resolved-obj args)))))
                    ;; Package doesn't exist — standard "can't locate" error
                    (let ((pkg-known (%pcl-find-package pkg-part)))
                      (p-die (format nil "Can't locate object method \"~A\" via package \"~A\"~A at - line 1.~%"
                                     meth-part pkg-part
                                     (if pkg-known "" (format nil " (perhaps you forgot to load \"~A\"?)" pkg-part)))))))))
            ;; CORE::method — dispatch to the corresponding PCL built-in (p-METHOD).
            ;; In Perl, CORE:: is not a real package; it's a namespace for built-in ops.
            ;; "3foo"->CORE::uc  ⟹  (p-uc "3foo")
            ((string-equal pkg-part "CORE")
             (return-from p-method-call
               (let ((fn (find-symbol (format nil "P-~A" (string-upcase meth-part)) :pcl)))
                 (if (and fn (fboundp fn))
                     (apply fn resolved-obj args)
                     (p-die (format nil "CORE::~A is not a known built-in" meth-part))))))
            ;; General PKG::method — look up pl-METHOD in that package
            (target-pkg
             (let ((fn (find-symbol (%pcl-cl-sub-name meth-part)
                                    target-pkg)))
               (when (and fn (fboundp fn))
                 (return-from p-method-call (apply fn resolved-obj args)))))
            ;; Package not found — give proper error instead of falling through
            (t
             (p-die (format nil "Can't locate object method \"~A\" via package \"~A\" (perhaps you forgot to load \"~A\"?) at - line 1.~%"
                            meth-part pkg-part pkg-part)))))))

    ;; Determine whether to use the @ISA walk or CLOS MRO.
    ;; @ISA walk is preferred whenever @ISA is non-empty (it reflects `local @ISA`
    ;; and runtime push/assignment, which CLOS cannot see).  CLOS is a fallback for
    ;; classes that have never had @ISA set (e.g. leaf classes with no parents).
    (let* ((pkg (%pcl-find-package class-name))
           (isa-sym (when pkg (find-symbol "@isa" pkg)))
           (isa-val (when (and isa-sym (boundp isa-sym)) (symbol-value isa-sym)))
           (isa-non-empty (and isa-val
                               (vectorp isa-val) (not (stringp isa-val))
                               (> (length isa-val) 0)))
           (clos-class-name (perl-pkg-to-clos-class class-name))
           (clos-class (when (and pkg (not isa-non-empty))
                         (find-class (intern (%pcl-invert-case clos-class-name) pkg) nil))))

      (if (and clos-class (not isa-non-empty))
          ;; Walk MRO (Method Resolution Order) using CLOS class-precedence-list.
          ;; Only used when @ISA is empty (no runtime inheritance set).
          (let ((mro (progn (sb-mop:finalize-inheritance clos-class)
                            (sb-mop:class-precedence-list clos-class))))
            (dolist (cls mro)
              (let* (;; Recover the user package directly from the CLOS class
                     ;; symbol's home package — case-safe under :invert (the
                     ;; lowercased plc- class name cannot round-trip case).
                     (pkg-name (package-name (symbol-package (class-name cls))))
                     (pkg (find-package pkg-name)))
                (when pkg
                  (let ((fn (find-symbol (%pcl-cl-sub-name method-name) pkg)))
                    ;; Only dispatch to methods LOCAL to this class package.
                    ;; Inherited symbols (e.g. pcl:p-push) must be ignored so
                    ;; that a class without a PUSH method doesn't accidentally
                    ;; call the pcl built-in instead of signalling "no method".
                    (when (and fn (eq (symbol-package fn) pkg) (fboundp fn))
                      (return-from p-method-call (apply fn resolved-obj args)))))))
            ;; UNIVERSAL is an implicit parent of all Perl classes.
            ;; After CLOS MRO fails, try UNIVERSAL's @ISA chain.
            (unless (string-equal class-name "UNIVERSAL")
              (labels ((find-in-u (cls-str visited)
                         (when (member cls-str visited :test #'equal)
                           (return-from find-in-u nil))
                         (let* ((pkg2 (%pcl-find-package cls-str))
                                (fn2 (when pkg2
                                       (find-symbol (%pcl-cl-sub-name method-name)
                                                    pkg2))))
                           (if (and fn2 (eq (symbol-package fn2) pkg2) (fboundp fn2))
                               (return-from p-method-call (apply fn2 resolved-obj args))
                               (let* ((isa2 (when pkg2 (find-symbol "@isa" pkg2)))
                                      (isa2v (when (and isa2 (boundp isa2)) (symbol-value isa2))))
                                 (when (and isa2v (vectorp isa2v))
                                   (loop for p across isa2v
                                         do (find-in-u (to-string p) (cons cls-str visited)))))))))
                (find-in-u "UNIVERSAL" nil)))
            ;; Not found in any class in MRO - check UNIVERSAL fallbacks, then AUTOLOAD
            (cond
              ((string-equal method-name "isa") (apply #'p-isa resolved-obj args))
              ((string-equal method-name "can") (apply #'p-can resolved-obj args))
              ;; Perl special case: ->import and ->unimport with no method return nothing.
              ;; In list context: p-flatten-marker with empty array (contributes 0 items).
              ;; In scalar/void context: nil (undef).
              ((or (string-equal method-name "import") (string-equal method-name "unimport"))
               (if (eq *wantarray* t)
                   (make-p-flatten-marker :array (make-array 0 :adjustable t :fill-pointer 0))
                   nil))
              (t (multiple-value-bind (result found)
                     (%pcl-dispatch-autoload class-name method-name resolved-obj args)
                   (if found result
                       (p-die (format nil "Can't locate object method \"~A\" via package \"~A\" at - line 1.~%"
                                      method-name class-name)))))))

          ;; @ISA is non-empty or no CLOS class — walk @ISA dynamically.
          ;; This path respects `local @ISA = (...)` and runtime mutations.
          (labels ((find-in-class (cls-str visited)
                     (when (member cls-str visited :test #'equal)
                       (return-from find-in-class nil))
                     ;; CORE is a virtual Perl namespace for built-in functions.
                     ;; When @ISA includes "CORE", method lookup falls back to p-METHOD.
                     (when (string-equal cls-str "CORE")
                       (let ((fn (find-symbol (format nil "P-~A" (string-upcase method-name)) :pcl)))
                         (when (and fn (fboundp fn))
                           (return-from p-method-call (apply fn resolved-obj args)))))
                     (let* ((pkg (%pcl-find-package cls-str))
                            (fn  (when pkg
                                   (find-symbol (%pcl-cl-sub-name method-name)
                                                pkg))))
                       (if (and fn (eq (symbol-package fn) pkg) (fboundp fn))
                           (return-from p-method-call (apply fn resolved-obj args))
                           ;; Recurse through @ISA
                           (let* ((isa-sym (when pkg (find-symbol "@isa" pkg)))
                                  (isa-val (when (and isa-sym (boundp isa-sym))
                                             (symbol-value isa-sym))))
                             (when (and isa-val (vectorp isa-val))
                               (loop for parent across isa-val
                                     do (find-in-class (to-string parent)
                                                       (cons cls-str visited)))))))))
            (let ((pkg (%pcl-find-package class-name)))
              (unless pkg
                (cond
                  ;; ->import / ->unimport on unknown packages return nothing.
                  ((or (string-equal method-name "import") (string-equal method-name "unimport"))
                   (return-from p-method-call
                     (if (eq *wantarray* t)
                         (make-p-flatten-marker :array (make-array 0 :adjustable t :fill-pointer 0))
                         nil)))
                  ;; UNIVERSAL methods (can/isa/DOES) are valid on ANY class name,
                  ;; even one never declared: `Foo->can("x")` is undef, `Foo->isa(...)`
                  ;; is false — NOT a "can't locate" error.  Fall through to the
                  ;; universal-fallback cond below (find-in-class with a nil pkg is a
                  ;; harmless no-op) rather than dying here.
                  ((or (string-equal method-name "can")
                       (string-equal method-name "isa")
                       (string-equal method-name "DOES")))
                  ;; Package unknown (never blessed into, never declared): add "perhaps" hint
                  (t (p-die (format nil "Can't locate object method \"~A\" via package \"~A\" (perhaps you forgot to load \"~A\"?) at - line 1.~%"
                                    method-name class-name class-name))))))
            (find-in-class class-name nil)
            ;; UNIVERSAL is an implicit parent of all Perl classes.
            ;; After exhausting the class's own @ISA chain, try UNIVERSAL's @ISA
            ;; (e.g. package UNIVERSAL; @ISA = 'LASTCHANCE' makes LASTCHANCE methods
            ;; available to all objects, since all classes inherit from UNIVERSAL).
            (unless (string-equal class-name "UNIVERSAL")
              (find-in-class "UNIVERSAL" nil))
            ;; Not found anywhere in @ISA chain - check UNIVERSAL fallbacks, then AUTOLOAD
            (cond
              ((string-equal method-name "isa") (apply #'p-isa resolved-obj args))
              ((string-equal method-name "DOES") (apply #'p-isa resolved-obj args))
              ((string-equal method-name "can") (apply #'p-can resolved-obj args))
              ;; Perl special case: ->import and ->unimport with no method return nothing
              ((or (string-equal method-name "import") (string-equal method-name "unimport"))
               (if (eq *wantarray* t)
                   (make-p-flatten-marker :array (make-array 0 :adjustable t :fill-pointer 0))
                   nil))
              (t (multiple-value-bind (result found)
                     (%pcl-dispatch-autoload class-name method-name resolved-obj args)
                   (if found result
                       (p-die (format nil "Can't locate object method \"~A\" via package \"~A\" at - line 1.~%"
                                      method-name class-name)))))))))))

;;; AUTOLOAD helpers for p-method-call

(defun %pcl-find-autoload-in-isa (class-name)
  "Walk @ISA chain from CLASS-NAME looking for PL-AUTOLOAD.
   Returns (cons pkg-name-str fn) or NIL."
  (labels ((walk (cls visited)
             (when (member cls visited :test #'equal) (return-from walk nil))
             (let* ((pkg (find-package (%pcl-invert-case cls)))
                    (al (when pkg (find-symbol (%pcl-cl-sub-name "AUTOLOAD") pkg))))
               (if (and al (eq (symbol-package al) pkg) (fboundp al))
                   (cons cls al)
                   (let* ((isa-sym (when pkg (find-symbol "@isa" pkg)))
                          (isa-val (when (and isa-sym (boundp isa-sym))
                                     (symbol-value isa-sym))))
                     (when (and isa-val (vectorp isa-val))
                       (loop for parent across isa-val
                             for result = (walk (to-string parent) (cons cls visited))
                             when result return result)))))))
    (walk class-name nil)))

(defun %pcl-set-autoload-var (pkg-name full-method-name)
  "Set $PKG::AUTOLOAD to FULL-METHOD-NAME in package PKG-NAME."
  (let* ((pkg (find-package (%pcl-invert-case pkg-name)))
         (sym (when pkg (intern "$autoload" pkg))))
    (when sym
      (unless (boundp sym) (setf (symbol-value sym) (make-p-box nil)))
      (unless (p-box-p (symbol-value sym))
        (setf (symbol-value sym) (make-p-box nil)))
      (box-set (symbol-value sym) full-method-name))))

(defun %pcl-dispatch-autoload (class-name method-name obj args)
  "Try to dispatch to AUTOLOAD for CLASS-NAME method METHOD-NAME.
   Returns (values result found-p) so caller knows if AUTOLOAD was available."
  (unless (string-equal method-name "DESTROY")
    (let ((al-info (%pcl-find-autoload-in-isa class-name)))
      (when al-info
        (%pcl-set-autoload-var (car al-info)
                               (format nil "~A::~A" class-name method-name))
        (return-from %pcl-dispatch-autoload
          (values (apply (cdr al-info) obj args) t)))))
  (values nil nil))

;;; Package name conversion utilities for inheritance
(defun perl-pkg-to-clos-class (name)
  "Convert Perl package name to CLOS class name: Foo::Bar -> plc-foo-bar.
   The plc- prefix guarantees the upcased class symbol never lands on a locked
   COMMON-LISP/SBCL symbol (e.g. `package If` -> CL:IF would die in defclass).
   MUST stay in lock-step with _pkg_to_clos_class in Pl/Parser.pm.
   The two CALLERS intern this name through %pcl-invert-case, not
   string-upcase: for the all-lowercase ASCII name this builds those are the
   same transform, and for a package whose name carries a non-ASCII character
   (#418) only the former matches the pipe-quoted `|plc-ｆｏｏ|` codegen emits."
  (concatenate 'string "plc-" (string-downcase (substitute #\- #\: name))))

;;; Indirect-object SUPER:: dispatch: SUPER::m{@a} where @a[0] is the invocant
(defun %pcl-super-indirect (method cur-pkg &rest inv-args)
  "Handle SUPER::method{@array} indirect-object syntax, incl. the trailing-
   LIST shapes SUPER::m{}@a and SUPER::m{@a}\"b\": the block's list and the
   trailing LIST concatenate (arrays flatten, perl list semantics), and the
   FIRST element of the combined list is the invocant."
  (let ((flat '()))
    (dolist (a inv-args)
      (cond ((and (vectorp a) (not (stringp a)))
             (loop for x across a do (push x flat)))
            ((null a))
            (t (push a flat))))
    (setf flat (nreverse flat))
    (when (null flat)
      (p-die (format nil "Can't call method \"~A\" without a package or object reference" method)))
    (apply #'p-super-call (first flat) method cur-pkg (rest flat))))

;;; SUPER:: method calls
(defun p-super-call (obj method current-class &rest args)
  "Call method starting from parent of current-class in MRO (for SUPER:: calls).
   Uses CLOS MRO when @ISA is empty; falls back to @ISA walk otherwise
   (covers the common case where @ISA is set at runtime and defclass puts
   the class symbol in the MAIN package rather than the class's own package)."
  (let* ((method-name (to-string method))
         (clos-class-name (perl-pkg-to-clos-class current-class))
         ;; %pcl-find-package, NOT (find-package (string-upcase ...)):
         ;; multi-segment packages are case-preserved (|Moo::Object|), so the
         ;; raw upcase lookup silently misses them and the SUPER walk dead-ends
         ;; (Moo: Animal's SUPER::new -> Moo::Object::new was "not found").
         (pkg (%pcl-find-package current-class))
         (isa-sym (when pkg (find-symbol "@isa" pkg)))
         (isa-val (when (and isa-sym (boundp isa-sym)) (symbol-value isa-sym)))
         (isa-non-empty (and isa-val (vectorp isa-val) (> (length isa-val) 0)))
         (clos-class (when (and pkg (not isa-non-empty))
                       (find-class (intern (%pcl-invert-case clos-class-name) pkg) nil))))
    (cond
      ((and clos-class (not isa-non-empty))
       ;; CLOS MRO path: walk MRO starting from parent of current class
       (let* ((mro (progn (sb-mop:finalize-inheritance clos-class)
                          (sb-mop:class-precedence-list clos-class)))
              (parent-mro (cdr mro)))
         (dolist (cls parent-mro)
           (let* ((pkg-name (package-name (symbol-package (class-name cls))))
                  (cpkg (find-package pkg-name)))
             (when cpkg
               (let ((fn (find-symbol (%pcl-cl-sub-name method-name) cpkg)))
                 (when (and fn (fboundp fn))
                   (return-from p-super-call (apply fn obj args)))))))
         (error "No SUPER::~A found from ~A" method-name current-class)))
      ((and isa-val (vectorp isa-val))
       ;; @ISA walk path: start from parents of current-class (skip current-class itself)
       (labels ((walk (cls-str visited)
                  (unless (member cls-str visited :test #'equal)
                    (let* ((cpkg (%pcl-find-package cls-str))
                           (fn (when cpkg
                                 (find-symbol (%pcl-cl-sub-name method-name)
                                              cpkg))))
                      (if (and fn (eq (symbol-package fn) cpkg) (fboundp fn))
                          (return-from p-super-call (apply fn obj args))
                          (let* ((isa2 (when cpkg (find-symbol "@isa" cpkg)))
                                 (isa2v (when (and isa2 (boundp isa2)) (symbol-value isa2))))
                            (when (and isa2v (vectorp isa2v))
                              (loop for p across isa2v
                                    do (walk (to-string p) (cons cls-str visited))))))))))
         (loop for parent across isa-val
               do (walk (to-string parent) (list current-class)))
         ;; Method not found via direct lookup — try AUTOLOAD in the parent chain
         (labels ((find-al (cls-str visited)
                    (unless (member cls-str visited :test #'equal)
                      (let* ((cpkg (%pcl-find-package cls-str))
                             (al (when cpkg (find-symbol (%pcl-cl-sub-name "AUTOLOAD") cpkg))))
                        (if (and al (eq (symbol-package al) cpkg) (fboundp al))
                            (progn
                              (%pcl-set-autoload-var cls-str method-name)
                              (return-from p-super-call (apply al obj args)))
                            (let* ((isa2 (when cpkg (find-symbol "@isa" cpkg)))
                                   (isa2v (when (and isa2 (boundp isa2)) (symbol-value isa2))))
                              (when (and isa2v (vectorp isa2v))
                                (loop for p across isa2v
                                      do (find-al (to-string p) (cons cls-str visited))))))))))
           (loop for parent across isa-val
                 do (find-al (to-string parent) (list current-class))))
         (error "No SUPER::~A found from ~A" method-name current-class)))
      (t
       (error "Can't find class ~A for SUPER:: call" current-class)))))

(defun %pcl-isa-ancestry (class-name)
  "Linearized class ancestry for CLASS-NAME: the class itself, then its @ISA
   chain (depth-first, cycle- and diamond-guarded), then the implicit UNIVERSAL
   parent.  Walks @ISA rather than the CLOS class-precedence-list because PCL
   records ALL inheritance in @ISA (CLOS classes are emitted with empty
   superclasses) and @ISA reflects runtime/`local` mutation — and because
   reading the CPL of a never-instantiated class touches an unfinalized class
   (the UNBOUND-SLOT %CLASS-PRECEDENCE-LIST crash p-can used to hit).
   Mirrors the @ISA walk p-method-call already prefers over CLOS."
  (let ((out '()))
    (labels ((walk (cls visited)
               (unless (or (member cls visited :test #'equal)
                           (member cls out :test #'equal))
                 (setf out (nconc out (list cls)))
                 (let* ((pkg (%pcl-find-package cls))
                        (isa-sym (when pkg (find-symbol "@isa" pkg)))
                        (isa-val (when (and isa-sym (boundp isa-sym))
                                   (symbol-value isa-sym))))
                   (when (and isa-val (vectorp isa-val) (not (stringp isa-val)))
                     (loop for parent across isa-val
                           do (walk (to-string parent) (cons cls visited))))))))
      (walk class-name nil)
      (unless (member "UNIVERSAL" out :test #'equal)
        (setf out (nconc out (list "UNIVERSAL")))))
    out))

;;; can() and isa() methods - available on all objects (UNIVERSAL package)
(defun p-can (invocant method-name)
  "Perl can() - return the code reference for METHOD-NAME resolvable from the
   invocant's class (walking @ISA + UNIVERSAL), or nil.  Only methods actually
   defined in a class's own package count — inherited CL symbols (e.g. the pcl
   built-ins a user package :uses) are ignored, matching p-method-call."
  (let* ((method-str (to-string method-name))
         (class-name (%pcl-invocant-class invocant)))
    (unless class-name
      (return-from p-can nil))
    (dolist (cls (%pcl-isa-ancestry class-name) nil)
      (let* ((pkg (%pcl-find-package cls))
             (fn  (when pkg (find-symbol (%pcl-cl-sub-name method-str) pkg))))
        (when (and fn (eq (symbol-package fn) pkg) (fboundp fn)
                   ;; A forward-declaration STUB is fboundp but not a real method
                   ;; (Perl wouldn't have compiled it yet) — ignore it, matching
                   ;; p-stash.  See docs/declaration-ordering-fix-plan.md.
                   (not (eq (gethash fn *p-declared-subs*) :stub)))
          (return-from p-can (symbol-function fn)))))))

(defun p-isa (invocant class-name)
  "Perl isa() - check if object is-a class.
   Uses C3 MRO to check inheritance chain.
   Returns t if invocant is-a class-name, nil otherwise."
  (let* ((check-class (to-string class-name))
         (obj-class (%pcl-invocant-class invocant)))
    (unless obj-class
      (return-from p-isa nil))

    ;; If the object's class defines a custom isa() method (PL-ISA), call it.
    ;; Perl's infix isa operator delegates to ->isa if the class overrides it.
    (let* ((pkg (find-package (%pcl-invert-case obj-class)))
           (custom-isa (when pkg (find-symbol "PL-ISA" pkg))))
      (when (and custom-isa (eq (symbol-package custom-isa) pkg) (fboundp custom-isa))
        (return-from p-isa (funcall custom-isa invocant check-class))))

    ;; A class is-a any class in its linearized @ISA ancestry (which includes
    ;; itself and the implicit UNIVERSAL parent).  Uses the same @ISA walk as
    ;; p-can / p-method-call — reflects runtime @ISA and never touches an
    ;; unfinalized CLOS class.
    (let ((want (%pcl-normalize-pkg check-class)))
      (if (member want (%pcl-isa-ancestry obj-class)
                  :test (lambda (a b) (string-equal a (%pcl-normalize-pkg b))))
          t
          nil))))

;;; ============================================================
;;; Regex Support (using CL-PPCRE)
;;; ============================================================

;; Regex operation types
(defstruct p-regex-match
  "Regex match operation m//.
   PATTERN is the cl-ppcre form (what the scanner compiles); SOURCE is the
   PERL-side text the pattern was written as, kept because perl's REGEXP sv
   stringifies from its source, not from anything a backend rewrote.  The two
   differ wherever perl-regex-to-ppcre translates — notably `(?^flags:` (perl's
   qr wrapper), which becomes `(?flags:` for cl-ppcre and would otherwise leak
   into `\"$re\"` as a shape perl never prints (task #181).  SOURCE nil means
   \"same as PATTERN\"."
  pattern
  modifiers
  source)

(defstruct p-subst-op
  "Substitution operation s///"
  pattern
  replacement
  modifiers)

(defstruct p-tr-op
  "Transliteration operation tr///"
  from
  to
  modifiers)

(defun parse-regex-modifiers (mod-string)
  "Parse modifier string like 'gi' into plist (:g t :i t).
   `xx` is its own modifier, not `x` written twice: /xx additionally ignores
   unescaped whitespace INSIDE bracketed character classes (task #179).  The
   per-character loop below cannot see that — a second x just re-sets :x — so
   count them and set :xx as well."
  (let ((result nil) (x-count 0))
    (loop for c across mod-string
          do (when (char= c #\x) (incf x-count))
          (let ((mod (intern (string-upcase (string c)) :keyword)))
            (setf (getf result mod) t)))
    (when (>= x-count 2) (setf (getf result :xx) t))
    result))

(defun get-closing-delim (open-delim)
  "Get the closing delimiter for paired delimiters like (), [], {}, <>"
  (case open-delim
    (#\( #\))
    (#\[ #\])
    (#\{ #\})
    (#\< #\>)
    (t open-delim)))  ; Non-paired delimiters use same char

(defun perl-regex-to-ppcre (pattern)
  "Convert Perl regex escape sequences to cl-ppcre compatible form.
   cl-ppcre does not handle \\x{HHHH} (Perl hex escapes with braces).
   Convert \\x{HHHH} to the literal Unicode character.
   Also strips (?{...}) and (??{...}) code blocks (not supported by cl-ppcre
   and cause infinite loops).
   Also converts \\Q...\\E metachar-quoting blocks (not supported by cl-ppcre)
   by applying ppcre:quote-meta-chars to the content."
  ;; First strip (?{code}) and (??{code}) blocks — cl-ppcre hangs on these
  (let* ((pat (cl-ppcre:regex-replace-all "\\(\\?\\?\\{[^}]*\\}\\)" pattern ""))
         (pat (cl-ppcre:regex-replace-all "\\(\\?\\{[^}]*\\}\\)" pat ""))
         ;; Named-group names: perl allows \w (underscore); cl-ppcre allows
         ;; only alphanumerics and #\-.  Perl names can never contain '-', so
         ;; _ <-> - is a collision-free bijection: rewrite here, map back when
         ;; populating %+ (set-capture-groups).  Covers (?<name>) and the
         ;; \k<name> backref; the name charset excludes the (?<= (?<!
         ;; lookbehind heads.
         (pat (cl-ppcre:regex-replace-all
               "\\(\\?<([a-zA-Z_][a-zA-Z0-9_]*)>"
               pat
               (lambda (match name)
                 (declare (ignore match))
                 (concatenate 'string "(?<" (substitute #\- #\_ name) ">"))
               :simple-calls t))
         (pat (cl-ppcre:regex-replace-all
               "\\\\k<([a-zA-Z_][a-zA-Z0-9_]*)>"
               pat
               (lambda (match name)
                 (declare (ignore match))
                 (concatenate 'string "\\k<" (substitute #\- #\_ name) ">"))
               :simple-calls t))
         ;; Perl's (?^flags:...) is the stringified form of qr//.
         ;; The '^' means "reset all flags to defaults".  CL-PPCRE uses (?flags:...)
         ;; without '^'.  Simply remove the '^'; at the top level (no enclosing flags)
         ;; the semantics are identical.
         (pat (cl-ppcre:regex-replace-all "\\(\\?\\^" pat "(?"
                                          :simple-calls t))
         ;; Convert \Q...\E: quote all regex metacharacters in the enclosed text.
         ;; \E is optional — \Q extends to end of pattern if \E is absent.
         (pat (cl-ppcre:regex-replace-all
               "\\\\Q(.*?)(?:\\\\E|$)"
               pat
               (lambda (match content)
                 (declare (ignore match))
                 (cl-ppcre:quote-meta-chars content))
               :simple-calls t))
         ;; Translate POSIX character classes to equivalent ranges.
         ;; CL-PPCRE 2.1.2 does not support [:class:] syntax.
         (pat (cl-ppcre:regex-replace-all
               "\\[:(\\w+):\\]"
               pat
               (lambda (match class-name)
                 (cond
                   ((equal class-name "alpha")  "a-zA-Z")
                   ((equal class-name "digit")  "0-9")
                   ((equal class-name "alnum")  "a-zA-Z0-9")
                   ((equal class-name "upper")  "A-Z")
                   ((equal class-name "lower")  "a-z")
                   ((equal class-name "word")   "a-zA-Z0-9_")
                   ((equal class-name "space")  " \\t\\n\\r\\x{0c}\\x{0b}")
                   ((equal class-name "blank")  " \\t")
                   ((equal class-name "print")  "\\x{20}-\\x{7e}")
                   ((equal class-name "graph")  "\\x{21}-\\x{7e}")
                   ((equal class-name "punct")  "\\x{21}-\\x{2f}\\x{3a}-\\x{40}\\x{5b}-\\x{60}\\x{7b}-\\x{7e}")
                   ((equal class-name "cntrl")  "\\x{00}-\\x{1f}\\x{7f}")
                   ((equal class-name "xdigit") "0-9a-fA-F")
                   (t match)))
               :simple-calls t)))
    (cl-ppcre:regex-replace-all
     "\\\\x\\{([0-9a-fA-F]+)\\}"
     pat
     (lambda (match register)
       (declare (ignore match))
       (let ((code (parse-integer register :radix 16)))
         (if (< code char-code-limit)
             (string (code-char code))
             (string #\?)))) ; fallback for out-of-range
     :simple-calls t)))

(defun %pcl-regex-delim-start (str prefix-len)
  "Index of the delimiter after a regex operator prefix.  Perl allows
   whitespace between the operator and its delimiter: `qr //`, `m {x}`."
  (or (position-if-not (lambda (c)
                         (member c '(#\Space #\Tab #\Newline #\Return)))
                       str :start prefix-len)
      prefix-len))

(defun p-regex (pattern-string)
  "Parse /pattern/modifiers and return a regex-match struct.
   Pattern-string is like '/foo/i' or 'm/bar/g' or 'm{pattern}s'"
  (let* ((str (to-string pattern-string))
         (first-char (char str 0))
         (start-delim (%pcl-regex-delim-start
                       str (if (char= first-char #\m) 1 0)))
         (open-delim (char str start-delim))
         (close-delim (get-closing-delim open-delim))
         (end-delim (position close-delim str :start (1+ start-delim) :from-end t))
         (raw (subseq str (1+ start-delim) end-delim))
         (pattern (perl-regex-to-ppcre raw))
         (modifiers (if (< end-delim (1- (length str)))
                        (subseq str (1+ end-delim))
                        "")))
    (make-p-regex-match :pattern pattern
                        :source raw
                        :modifiers (parse-regex-modifiers modifiers))))

(defun p-regex-from-parts (pattern modifiers)
  "Build a regex from a runtime-interpolated pattern string and modifier string.
   Used when the regex contains variable interpolation (e.g. /$x/ or qr/$x/).

   A pattern that IS a single interpolated regex object — `qr/$re/`, `/$re/` —
   is that same regex in perl: it keeps the inner qr's own flags and the outer
   modifiers are IGNORED (`qr/$re/i` where `$re = qr/abc/` does NOT match
   \"ABC\", and stringifies as `(?^:abc)`, not as a re-wrap).  Detect it HERE,
   where the argument is still the object — by the time it has been stringified
   the only test left is a regex over the wrapper text, which cannot tell
   `qr/$re/` from `qr/(?^:a)(?^:b)/` (task #181)."
  (let ((rx (unbox pattern)))
    (if (p-regex-match-p rx)
        ;; …but only the PATTERN's own flags come from the inner qr.  /g /c /e
        ;; /r are flags of the MATCH OPERATION, not of the compiled pattern, so
        ;; they must survive: dropping /g here turned `while ($t =~ /$re/g)`
        ;; into an infinite loop (Pl/t/regex-gpos-01.t caught it).  With none of
        ;; them present the object is returned unchanged, which is what keeps
        ;; `qr/$re/` identical to `$re`.
        (let* ((outer (parse-regex-modifiers (to-string modifiers)))
               (op-flags (loop for k in '(:g :c :e :r)
                               when (getf outer k) nconc (list k t))))
          (if op-flags
              (make-p-regex-match
               :pattern   (p-regex-match-pattern rx)
               :source    (p-regex-match-source rx)
               :modifiers (append op-flags (p-regex-match-modifiers rx)))
              rx))
        (let ((raw (to-string pattern)))
          (make-p-regex-match :pattern (perl-regex-to-ppcre raw)
                              :source raw
                              :modifiers (parse-regex-modifiers (to-string modifiers)))))))

(defun p-qr (pattern-string)
  "Parse qr/pattern/modifiers and return a compiled regex (regex-match struct).
   Pattern-string is like 'qr/foo/i' or 'qr{pattern}i'"
  (let* ((str (to-string pattern-string))
         ;; Skip past 'qr' prefix and any whitespace before the delimiter
         (start-delim (%pcl-regex-delim-start str 2))
         (open-delim (char str start-delim))
         (close-delim (get-closing-delim open-delim))
         (end-delim (position close-delim str :start (1+ start-delim) :from-end t))
         (raw (subseq str (1+ start-delim) end-delim))
         (pattern (perl-regex-to-ppcre raw))
         (modifiers (if (< end-delim (1- (length str)))
                        (subseq str (1+ end-delim))
                        "")))
    (make-p-regex-match :pattern pattern
                        :source raw
                        :modifiers (parse-regex-modifiers modifiers))))

(defun p-subst (pattern replacement &rest modifiers)
  "Create a substitution operation s///
   Modifiers are keywords like :g :i :s :m :x :e"
  (make-p-subst-op :pattern (to-string pattern)
                   :replacement (if (functionp replacement)
                                    replacement
                                    (to-string replacement))
                   :modifiers modifiers))

(defun p-tr (from to &rest modifiers)
  "Create a transliteration operation tr///
   Modifiers are keywords like :c :d :s :r"
  (make-p-tr-op :from (to-string from)
                :to (to-string to)
                :modifiers modifiers))

;;; ---------------------------------------------------------------------------
;;; /x extended-mode normaliser — works around a cl-ppcre bug where extended
;;; mode is NOT restored after an inline `(?-x:...)` / `(?x:...)` mode group, so
;;; whitespace/comments after the group are wrongly kept (or wrongly stripped).
;;; See docs/clppcre-extended-mode-modifier-bug.md.  We strip insignificant
;;; whitespace/comments ourselves (honouring [...], escapes and (?x)/(?-x)
;;; scopes), rewrite the x flag out of every mode group, and pass the result
;;; WITHOUT :extended-mode.  Engaged ONLY when the pattern actually contains an
;;; x mode-modifier, so ordinary /x patterns keep using cl-ppcre's (correct)
;;; extended mode and never regress.
;;; ---------------------------------------------------------------------------
(defun %pcl-parse-x-flag-group (pat i cur-x &optional cur-xx)
  "PAT[i] is #\(.  If this opens a (?flags:) / (?flags) MODE group, return
   (values T end-index emit-string terminator newx newxx); else (values NIL ...).
   x is removed from the emitted flags; NEWX is the x state it selects, and
   NEWXX the /xx state — `(?xx:` is TWO x's, and turning x off turns xx off
   with it (task #179)."
  (let ((n (length pat)))
    (when (and (< (+ i 1) n) (char= (char pat (+ i 1)) #\?))
      (let ((j (+ i 2)) (caret nil) (on "") (off "") (seen-dash nil))
        (when (and (< j n) (char= (char pat j) #\^)) (setf caret t) (incf j))
        (loop while (< j n) do
              (let ((c (char pat j)))
                (cond
                  ((char= c #\-) (if seen-dash (return) (progn (setf seen-dash t) (incf j))))
                  ((find c "imsxadlupn")
                   (if seen-dash (setf off (concatenate 'string off (string c)))
                       (setf on (concatenate 'string on (string c))))
                   (incf j))
                  (t (return)))))
        (when (and (< j n) (or (char= (char pat j) #\:) (char= (char pat j) #\)))
                   (or caret (> (length on) 0) (> (length off) 0)))
          (let* ((term (char pat j))
                 (base (if caret nil cur-x))
                 (base-xx (if caret nil cur-xx))
                 (newx (cond ((find #\x on) t) ((find #\x off) nil) (t base)))
                 (newxx (cond ((>= (count #\x on) 2) t)
                              ((find #\x off) nil)   ; -x cancels xx too
                              ((find #\x on) base-xx) ; a single x neither sets nor clears
                              (t base-xx)))
                 (on2 (remove #\x on)) (off2 (remove #\x off))
                 (flags (concatenate 'string (if caret "^" "") on2
                                     (if (> (length off2) 0) (concatenate 'string "-" off2) "")))
                 (emit (if (char= term #\:)
                           (if (= 0 (length flags)) "(?:" (format nil "(?~a:" flags))
                           (if (= 0 (length flags)) "" (format nil "(?~a)" flags)))))
            (return-from %pcl-parse-x-flag-group
              (values t (1+ j) emit term newx newxx))))))
    (values nil nil nil nil nil nil)))

(defun %pcl-has-x-modifier (pat)
  "True if PAT contains an x mode-modifier group, e.g. (?x:..)/(?-x:..)/(?ix:..)."
  (let ((i 0) (n (length pat)))
    (loop while (< i n) do
          (let ((c (char pat i)))
            (cond
              ((char= c #\\) (incf i 2))
              ((char= c #\()
               (multiple-value-bind (flag-p end emit term newx)
                   (%pcl-parse-x-flag-group pat i t)
                 (declare (ignore end emit term))
                 (if (and flag-p
                          ;; only a group that actually mentions x is interesting:
                          ;; newx differs from the "no x flag" outcome.  Detect by
                          ;; re-checking the raw flag text for an x.
                          (%pcl-flag-text-has-x pat i))
                     (return-from %pcl-has-x-modifier t)
                     (incf i))))
              (t (incf i)))))
    nil))

(defun %pcl-flag-text-has-x (pat i)
  "Does the (?...) flag run starting at PAT[i] (a #\() contain the letter x?"
  (let ((n (length pat)) (j (+ i 2)))
    (when (and (< (1+ i) n) (char= (char pat (1+ i)) #\?))
      (loop while (< j n) do
            (let ((c (char pat j)))
              (cond
                ((char= c #\x) (return-from %pcl-flag-text-has-x t))
                ((or (find c "imsadlupn") (char= c #\^) (char= c #\-)) (incf j))
                (t (return))))))
    nil))

(defun %pcl-normalize-extended (pat base-x &optional base-xx)
  "Strip insignificant whitespace/comments per /x scope and rewrite x mode
   modifiers away.  BASE-X = is the whole pattern extended (/x flag set)?
   BASE-XX = is /xx in force, which ALSO ignores unescaped whitespace inside
   a bracketed character class (task #179).  Escaped whitespace (`[a\\<TAB>b]`)
   is preserved under both — it leaves via the backslash branch below, before
   the class branch ever sees it, which is exactly perl's rule."
  (let ((out (make-string-output-stream)) (i 0) (n (length pat))
        (xstack (list base-x)) (xxstack (list base-xx)) (in-class nil))
    (flet ((curx () (car xstack))
           (curxx () (car xxstack)))
      (loop while (< i n) do
            (let ((c (char pat i)))
              (cond
                ((char= c #\\)
                 (write-char c out)
                 (when (< (1+ i) n) (write-char (char pat (1+ i)) out))
                 (incf i 2))
                (in-class
                 (cond
                   ;; /xx: unescaped whitespace inside [...] is insignificant.
                   ((and (curxx)
                         (member c '(#\Space #\Tab #\Newline #\Return #\Page)))
                    (incf i))
                   (t
                    (write-char c out)
                    (when (char= c #\]) (setf in-class nil))
                    (incf i))))
                ((char= c #\[)
                 (write-char c out) (incf i) (setf in-class t)
                 (when (and (< i n) (char= (char pat i) #\^)) (write-char #\^ out) (incf i))
                 (when (and (< i n) (char= (char pat i) #\])) (write-char #\] out) (incf i)))
                ;; (?#...) comment group: content is literal up to the first
                ;; unescaped ) — copy it verbatim so its `#`/spaces are NOT
                ;; treated as an /x comment/whitespace.
                ((and (char= c #\()
                      (< (+ i 2) n)
                      (char= (char pat (+ i 1)) #\?)
                      (char= (char pat (+ i 2)) #\#))
                 (write-string "(?#" out) (incf i 3)
                 (loop while (and (< i n) (not (char= (char pat i) #\)))) do
                       (when (char= (char pat i) #\\)
                         (write-char #\\ out) (incf i))
                       (when (< i n) (write-char (char pat i) out) (incf i)))
                 (when (< i n) (write-char #\) out) (incf i)))
                ((char= c #\()
                 (multiple-value-bind (flag-p end emit term newx newxx)
                     (%pcl-parse-x-flag-group pat i (curx) (curxx))
                   (cond
                     ((not flag-p) (write-char #\( out) (incf i)
                      (push (curx) xstack) (push (curxx) xxstack))
                     ((char= term #\:) (write-string emit out) (setf i end)
                      (push newx xstack) (push newxx xxstack))
                     (t (write-string emit out) (setf i end)
                        (setf (car xstack) newx) (setf (car xxstack) newxx)))))
                ((char= c #\))
                 (write-char c out) (incf i)
                 (when (cdr xstack) (pop xstack))
                 (when (cdr xxstack) (pop xxstack)))
                ((curx)
                 (cond
                   ((member c '(#\Space #\Tab #\Newline #\Return #\Page)) (incf i))
                   ((char= c #\#)
                    (loop while (and (< i n) (not (char= (char pat i) #\Newline))) do (incf i)))
                   (t (write-char c out) (incf i))))
                (t (write-char c out) (incf i))))))
    (get-output-stream-string out)))

(defun %pcl-build-scanner (pattern options)
  "cl-ppcre:create-scanner wrapper.  Why this exists (yes, it looks stupid):
   cl-ppcre has a bug — after an inline `(?-x:...)`/`(?x:...)` mode group it does
   NOT restore the surrounding extended-mode state, so the rest of the pattern's
   whitespace/comments are handled in the wrong mode and the match silently
   breaks (e.g. Text::ParseWords' parse_line).  Rather than rely on cl-ppcre's
   broken extended-mode for such patterns, we do the /x stripping OURSELVES
   (%pcl-normalize-extended), delete the x flag from every mode group, and hand
   cl-ppcre a pattern that needs no extended-mode at all.  We only do this when
   the pattern actually contains an x mode-modifier; every other /x pattern keeps
   using cl-ppcre's (correct, faster) native extended-mode and is untouched.
   See docs/clppcre-extended-mode-modifier-bug.md.

   SECOND reason to self-normalise (task #179): /xx.  cl-ppcre has no /xx at
   all, and /xx additionally ignores unescaped whitespace INSIDE bracketed
   character classes, so an xx pattern must always take this path.
   :pcl-xx-mode is a PCL-private option — strip it before create-scanner."
  (let ((xx (getf options :pcl-xx-mode)))
    (if (or xx (%pcl-has-x-modifier pattern))
        (apply #'cl-ppcre:create-scanner
               (%pcl-normalize-extended pattern (getf options :extended-mode) xx)
               ;; drop :extended-mode (we applied it by hand, so cl-ppcre must
               ;; not) and :pcl-xx-mode (cl-ppcre would not know it).
               (loop for (k v) on options by #'cddr
                     unless (member k '(:extended-mode :pcl-xx-mode))
                     nconc (list k v)))
        (apply #'cl-ppcre:create-scanner pattern
               (loop for (k v) on options by #'cddr
                     unless (eq k :pcl-xx-mode) nconc (list k v))))))

(defvar *pcl-scanner-cache* (make-hash-table :test 'equal)
  "Memoizes (pattern + options) -> (scanner . reg-names).  cl-ppcre compiles a
   fresh scanner on every create-scanner call and PCL builds one per match, so
   without this a regex used in a loop recompiles every iteration.  Also
   amortizes the /x normaliser (%pcl-build-scanner) to once per distinct pattern.
   Scanners are stateless closures (match state lives in the registers passed to
   scan), so sharing one is safe.  The cache is per-process; PCL runs one program
   per image, so distinct patterns are bounded and there is no staleness.")

(defun %pcl-capture-closer-positions (pattern &optional extended-p)
  "Vector mapping capture group N-1 to the position of its closing paren in
   PATTERN.  Drives $^N (perlvar: the participating group with the rightmost
   closing parenthesis).  Skips escapes, char classes and, with EXTENDED-P,
   #-comments; non-capturing (?…) groups pair up via a 0 marker."
  (let ((opens nil)
        (closers (make-array 4 :adjustable t :fill-pointer 0))
        (in-class nil)
        (i 0)
        (len (length pattern)))
    (loop
     (when (>= i len) (return closers))
     (let ((c (char pattern i)))
       (cond
         ((char= c #\\) (incf i))
         (in-class (when (char= c #\]) (setf in-class nil)))
         ((char= c #\[) (setf in-class t))
         ((and extended-p (char= c #\#))
          (loop while (and (< (1+ i) len)
                           (char/= (char pattern (1+ i)) #\Newline))
                do (incf i)))
         ((char= c #\()
          (if (%pcl-capturing-paren-p pattern i len)
              (progn
                (vector-push-extend -1 closers)
                (push (fill-pointer closers) opens))
              (push 0 opens)))
         ((char= c #\))
          (let ((g (pop opens)))
            (when (and g (> g 0))
              (setf (aref closers (1- g)) i))))))
     (incf i))))

(defun %pcl-capturing-paren-p (pattern i len)
  "Is the ( at position I in PATTERN a capturing group opener?
   Capturing: plain (, and the named forms (?<name> (?'name' (?P<name>.
   Non-capturing: every other (?… — including lookbehind (?<= / (?<!."
  (if (or (>= (1+ i) len) (char/= (char pattern (1+ i)) #\?))
      t
      (and (< (+ i 2) len)
           (let ((c2 (char pattern (+ i 2))))
             (or (char= c2 #\')
                 (and (char= c2 #\<)
                      (< (+ i 3) len)
                      (char/= (char pattern (+ i 3)) #\=)
                      (char/= (char pattern (+ i 3)) #\!))
                 (and (char= c2 #\P)
                      (< (+ i 3) len)
                      (char= (char pattern (+ i 3)) #\<)))))))

(defun %pcl-create-scanner (pattern options)
  "Memoized %pcl-build-scanner.  Returns (values scanner reg-names closers);
   CLOSERS is the %pcl-capture-closer-positions vector for $^N."
  (let* ((key (format nil "~A~C~{~A~^ ~}" pattern #\Nul options))
         (hit (gethash key *pcl-scanner-cache*)))
    (if hit
        (values (first hit) (second hit) (cddr hit))
        (multiple-value-bind (scanner reg-names) (%pcl-build-scanner pattern options)
          (let ((closers (%pcl-capture-closer-positions
                          pattern (getf options :extended-mode))))
            (setf (gethash key *pcl-scanner-cache*)
                  (list* scanner reg-names closers))
            (values scanner reg-names closers))))))

(defun build-ppcre-options (modifiers)
  "Convert Perl regex modifiers to CL-PPCRE options plist"
  (let ((options nil))
    (when (getf modifiers :i)
      (setf options (list* :case-insensitive-mode t options)))
    (when (getf modifiers :s)
      (setf options (list* :single-line-mode t options)))
    (when (getf modifiers :m)
      (setf options (list* :multi-line-mode t options)))
    (when (getf modifiers :x)
      (setf options (list* :extended-mode t options)))
    ;; cl-ppcre has no /xx, so it is carried as a PCL-private option that
    ;; %pcl-build-scanner consumes and strips before calling create-scanner.
    (when (getf modifiers :xx)
      (setf options (list* :pcl-xx-mode t options)))
    options))

(defun clear-capture-groups ()
  "Reset all capture group variables.  $1..$9 reset to *p-undef* (Perl undef),
   NOT raw nil — see the defvar note: a raw-nil capture vanishes when flattened
   into a list (%p-flatten-list treats raw nil as an empty-list/hole marker)."
  (setf $1 *p-undef* $2 *p-undef* $3 *p-undef* $4 *p-undef* $5 *p-undef*
        $6 *p-undef* $7 *p-undef* $8 *p-undef* $9 *p-undef*
        $10 *p-undef* $11 *p-undef* $12 *p-undef* $13 *p-undef*
        $14 *p-undef* $15 *p-undef* $16 *p-undef* $17 *p-undef*
        $18 *p-undef* $19 *p-undef* $20 *p-undef*
        |$&| nil |$`| nil |$'| nil |$+| nil |$^N| nil)
  (clrhash %+)
  (clrhash |%-|)
  (setf (fill-pointer |@{^CAPTURE}|) 0))

(defun set-match-vars (str match-start match-end reg-starts reg-ends
                       &optional closers)
  "Set the match position variables from a successful match: $& (MATCH),
   $` (PREMATCH), $' (POSTMATCH), $+ (last capture that matched) and
   $^N (rightmost-closing participating capture, from the CLOSERS vector
   cached by %pcl-create-scanner; without it, falls back to $+'s rule)."
  (when (and match-start match-end)
    (setf |$&| (subseq str match-start match-end)
          |$`| (subseq str 0 match-start)
          |$'| (subseq str match-end)))
  ;; $+ = highest-numbered capture group that actually participated
  (when (and reg-starts reg-ends)
    (loop for i from (1- (length reg-starts)) downto 0
          do (let ((rs (aref reg-starts i)) (re (aref reg-ends i)))
               (when (and rs re)
                 (setf |$+| (subseq str rs re))
                 (return))))
    ;; $^N = participating group with the rightmost closing paren (perlvar)
    (let ((best -1)
          (best-pos -1))
      (dotimes (i (length reg-starts))
        (when (and (aref reg-starts i) (aref reg-ends i))
          (let ((pos (if (and closers (< i (length closers)))
                         (aref closers i)
                         i)))
            (when (> pos best-pos)
              (setf best-pos pos best i)))))
      (when (>= best 0)
        (setf |$^N| (subseq str (aref reg-starts best) (aref reg-ends best))))))
  ;; @- / @+ : offset arrays.  Element 0 is the whole-match start/end; element
  ;; N (1-based) is capture group N's start/end (undef for a group that did not
  ;; participate).  THE TWO ARE SIZED DIFFERENTLY, and perl means it (task
  ;; #417, probed: "ab" =~ /(a)(x)?(y)?/ gives $#+ 3 and $#- 1):
  ;;   @- stops after the LAST PARTICIPATING group  ($#- = last matched paren)
  ;;   @+ runs to the pattern's GROUP COUNT         ($#+ = number of groups)
  ;; so a trailing non-participant is absent from @- and a present undef in @+.
  ;; Elements are boxed integers like any other array element.
  (when (and match-start match-end)
    (setf (fill-pointer |@-|) 0
          (fill-pointer |@+|) 0)
    (vector-push-extend (make-p-box match-start) |@-|)
    (vector-push-extend (make-p-box match-end)   |@+|)
    (when (and reg-starts reg-ends)
      (let ((last-matched -1))
        (loop for i from (1- (length reg-starts)) downto 0
              when (aref reg-starts i)
              do (setf last-matched i) (return))
        (loop for i from 0 to last-matched
              do (vector-push-extend (make-p-box (aref reg-starts i)) |@-|))
        (loop for i from 0 below (length reg-ends)
              do (vector-push-extend (make-p-box (aref reg-ends i)) |@+|))))))

(defmacro %set-cap (var str starts ends idx)
  "Set capture variable VAR from reg-starts/ends at IDX, guarding against NIL (optional group)."
  `(let ((rs (aref ,starts ,idx)) (re (aref ,ends ,idx)))
     (setf ,var (if (and rs re) (subseq ,str rs re) *p-undef*))))

(defun %pcl-push-named-buffer (name val)
  "Append VAL to %-'s array for NAME (Perl %-: every buffer with that name,
   undef for non-participating ones).  Elements boxed like array elements."
  (let ((v (or (gethash name |%-|)
               (setf (gethash name |%-|)
                     (make-array 0 :adjustable t :fill-pointer 0)))))
    (vector-push-extend (make-p-box val) v)))

(defun set-capture-groups (str reg-starts reg-ends &optional reg-names)
  "Set capture group variables $1..$9 and named captures %+ from regex match results.
   REG-NAMES is the optional list of capture names returned by cl-ppcre:create-scanner.
   Groups that did not participate in the match (optional groups) set $N to nil."
  (when (and reg-starts reg-ends)
    (let ((num-groups (length reg-starts)))
      (when (> num-groups 0) (%set-cap $1 str reg-starts reg-ends 0))
      (when (> num-groups 1) (%set-cap $2 str reg-starts reg-ends 1))
      (when (> num-groups 2) (%set-cap $3 str reg-starts reg-ends 2))
      (when (> num-groups 3) (%set-cap $4 str reg-starts reg-ends 3))
      (when (> num-groups 4) (%set-cap $5 str reg-starts reg-ends 4))
      (when (> num-groups 5) (%set-cap $6 str reg-starts reg-ends 5))
      (when (> num-groups 6) (%set-cap $7 str reg-starts reg-ends 6))
      (when (> num-groups 7) (%set-cap $8 str reg-starts reg-ends 7))
      (when (> num-groups 8) (%set-cap $9 str reg-starts reg-ends 8))
      (when (> num-groups 9) (%set-cap $10 str reg-starts reg-ends 9))
      (when (> num-groups 10) (%set-cap $11 str reg-starts reg-ends 10))
      (when (> num-groups 11) (%set-cap $12 str reg-starts reg-ends 11))
      (when (> num-groups 12) (%set-cap $13 str reg-starts reg-ends 12))
      (when (> num-groups 13) (%set-cap $14 str reg-starts reg-ends 13))
      (when (> num-groups 14) (%set-cap $15 str reg-starts reg-ends 14))
      (when (> num-groups 15) (%set-cap $16 str reg-starts reg-ends 15))
      (when (> num-groups 16) (%set-cap $17 str reg-starts reg-ends 16))
      (when (> num-groups 17) (%set-cap $18 str reg-starts reg-ends 17))
      (when (> num-groups 18) (%set-cap $19 str reg-starts reg-ends 18))
      (when (> num-groups 19) (%set-cap $20 str reg-starts reg-ends 19))
      ;; @{^CAPTURE} (5.26+): the group VALUES, 0-based, truncated after the
      ;; last participating group -- one element shorter than @-, which is
      ;; exactly what t/re/pat.t asserts ("$#{^CAPTURE} is one less than $#-").
      ;; A non-participating group INSIDE that range is a present undef element.
      (let ((last-matched -1))
        (loop for i from (1- num-groups) downto 0
              when (aref reg-starts i)
              do (setf last-matched i) (return))
        (loop for i from 0 to last-matched
              for rs = (aref reg-starts i)
              for re = (aref reg-ends i)
              do (vector-push-extend (make-p-box (and rs re (subseq str rs re)))
                                     |@{^CAPTURE}|)))
      ;; Populate %+ with named captures
      ;; reg-names is a list from cl-ppcre:create-scanner, e.g. ("year" "month" NIL)
      (when reg-names
        (clrhash |%-|)
        (loop for name in reg-names
              for i from 0
              when (and name (< i num-groups))
              do (let ((rs (aref reg-starts i))
                       (re (aref reg-ends   i))
                       ;; Reverse the _ <-> - name mapping perl-regex-to-ppcre
                       ;; applied (cl-ppcre rejects _ in register names).
                       (pname (substitute #\_ #\- name)))
                   (when (and rs re)
                     (setf (gethash pname %+) (subseq str rs re)))
                   (%pcl-push-named-buffer pname
                                           (if (and rs re) (subseq str rs re) *p-undef*))))))))

(defun %pcl-strip-gpos (pattern)
  "Remove \\G anchors from PATTERN.  cl-ppcre has no \\G; \\G is zero-width and
   means 'match at the current pos', so PCL drops it and anchors the whole match
   at the /g start position instead (see the anchored-g handling in
   do-regex-match).  This also catches the qr// form `(?^:\\G(...))` that
   Text::Balanced and friends produce, where \\G is not pattern-leading.  Escaped
   backslashes (\\\\) and char-class contents are left untouched.  Returns the
   stripped pattern; a shorter result signals that a \\G was present."
  (let ((out (make-string-output-stream))
        (in-class nil) (i 0) (n (length pattern)))
    (loop while (< i n) do
          (let ((c (char pattern i)))
            (cond
              ((char= c #\\)
               (if (and (not in-class) (< (1+ i) n) (char= (char pattern (1+ i)) #\G))
                   (incf i 2)                       ; drop \G
                   (progn                            ; copy the escape pair verbatim
                     (write-char c out)
                     (when (< (1+ i) n) (write-char (char pattern (1+ i)) out))
                     (incf i 2))))
              ((char= c #\[) (setf in-class t) (write-char c out) (incf i))
              ((char= c #\]) (setf in-class nil) (write-char c out) (incf i))
              (t (write-char c out) (incf i)))))
    (get-output-stream-string out)))

(defun %pcl-scan-anchored-list (scanner str reg-names start &optional closers)
  "Emulate /\\G.../g in list context: collect contiguous matches starting at
   START, stopping at the first position where the pattern does not match exactly
   there.  Returns an adjustable vector of capture strings (whole matches when the
   pattern has no captures) and sets $1.., %+, $& from the LAST match."
  (let ((items nil) (pos start) (slen (length str))
        (last-rs nil) (last-re nil) (last-ms nil) (last-me nil) (any nil))
    (loop
     (multiple-value-bind (ms me rs re) (cl-ppcre:scan scanner str :start pos)
       (unless (and ms (= ms pos)) (return))
       (setf any t last-rs rs last-re re last-ms ms last-me me)
       (if (> (length rs) 0)
           (dotimes (i (length rs))
             (push (if (and (aref rs i) (aref re i))
                       (subseq str (aref rs i) (aref re i)) nil)
                   items))
           (push (subseq str ms me) items))
       (setf pos (if (= me ms) (1+ me) me))
       (when (> pos slen) (return))))
    (let* ((lst (nreverse items))
           (result (make-array (length lst) :adjustable t :fill-pointer t)))
      (loop for it in lst for i from 0 do (setf (aref result i) it))
      (when any
        (clear-capture-groups)
        (set-capture-groups str last-rs last-re reg-names)
        (set-match-vars str last-ms last-me last-rs last-re closers))
      result)))

(defun do-regex-match (string op)
  "Perform regex match.
   In scalar context: return t if matched, nil otherwise.
   In list context (*wantarray* t): return vector of captures, or nil if no match.
   Also sets capture group variables $1, $2, ... $9.
   Note: In Perl, captures are only updated on successful match.
   /g in scalar context: iterates over matches, tracking pos in *p-match-pos*.
   /g in list context: returns all matches at once (no pos tracking).
   /gc: keeps pos on failure instead of resetting it."
  (let* ((str (to-string string))   ; to-string handles unboxing via box-sv (preserves class)
         (raw-pattern (p-regex-match-pattern op))
         ;; \G anchors the match at the current pos.  cl-ppcre has no \G, so we
         ;; strip it and require the match to START at the /g position.  A shorter
         ;; stripped pattern means a \G was present (anchored).
         (pattern (%pcl-strip-gpos raw-pattern))
         (anchored-g (< (length pattern) (length raw-pattern)))
         (modifiers (p-regex-match-modifiers op))
         (options (build-ppcre-options modifiers))
         (global-p (getf modifiers :g))
         (cont-p (getf modifiers :c)))
    (handler-case
        (multiple-value-bind (scanner reg-names closers)
            (%pcl-create-scanner pattern options)
          ;; Perl clears %+/%- on every match attempt, even failures.
          ;; $1..$9 are only cleared/set on successful matches.
          (clrhash %+)
          (clrhash |%-|)
          (cond
            ;; /\G.../g in list context: contiguous anchored matches from pos
            ((and global-p (eq *wantarray* t) anchored-g)
             (prog1
                 (%pcl-scan-anchored-list scanner str reg-names
                                          (or (gethash string *p-match-pos*) 0)
                                          closers)
               ;; Perl resets pos() after a list-context /g match exhausts.  This
               ;; path STARTS from pos, so leaving a stale pos would make a
               ;; `while (pos < len) { @m = /\G.../g }` loop never terminate;
               ;; clearing it matches Perl (pos() becomes undef).
               (remhash string *p-match-pos*)))
            ;; /g in list context: return all matches at once, no pos tracking
            ;; :void is NOT list context — only (eq *wantarray* t) is list context
            ((and global-p (eq *wantarray* t))
             (let ((all-results nil)
                   (last-rs nil) (last-re nil) (last-ms nil) (last-me nil))
               (cl-ppcre:do-scans (ms me rs re scanner str)
                 (setf last-rs rs last-re re last-ms ms last-me me)
                 (if (> (length rs) 0)
                     (dotimes (i (length rs))
                       ;; An unmatched group is perl UNDEF, never raw nil: raw
                       ;; nil means "empty list" to %p-flatten-list, so a list
                       ;; ASSIGNMENT would silently shift every later capture
                       ;; up one slot (see the no-/g branch below).
                       (push (if (and (aref rs i) (aref re i))
                                 (subseq str (aref rs i) (aref re i))
                                 *p-undef*)
                             all-results))
                     (push (subseq str ms me) all-results)))
               (let* ((items (nreverse all-results))
                      (result (make-array (length items) :adjustable t :fill-pointer t)))
                 (loop for item in items for i from 0 do (setf (aref result i) item))
                 (when items
                   (clear-capture-groups)
                   (set-capture-groups str last-rs last-re reg-names)
                   (set-match-vars str last-ms last-me last-rs last-re closers))
                 result)))
            ;; /g in scalar/void context: iterate from current pos
            ((and global-p (not (eq *wantarray* t)))
             (let ((start (or (gethash string *p-match-pos*) 0)))
               (multiple-value-bind (match-start match-end reg-starts reg-ends)
                   (cl-ppcre:scan scanner str :start start)
                 ;; \G: the match must begin exactly at the start position.
                 (when (and anchored-g match-start (/= match-start start))
                   (setf match-start nil))
                 (if match-start
                     (progn
                       (setf (gethash string *p-match-pos*) match-end)
                       (clear-capture-groups)
                       (set-capture-groups str reg-starts reg-ends reg-names)
                       (set-match-vars str match-start match-end reg-starts reg-ends
                                       closers)
                       t)
                     (progn
                       (unless cont-p
                         (remhash string *p-match-pos*))
                       ;; scalar/void /g no-match → Perl's '' (defined false)
                       "")))))
            ;; No /g: single match.  With \G, anchor at the current pos.
            (t
             (let ((start (if anchored-g (or (gethash string *p-match-pos*) 0) 0)))
               (multiple-value-bind (match-start match-end reg-starts reg-ends)
                   (cl-ppcre:scan scanner str :start start)
                 (when (and anchored-g match-start (/= match-start start))
                   (setf match-start nil))
                 (if match-start
                     (progn
                       (clear-capture-groups)
                       (set-capture-groups str reg-starts reg-ends reg-names)
                       (set-match-vars str match-start match-end reg-starts reg-ends
                                       closers)
                       (if (eq *wantarray* t)
                           (let* ((num-groups (length reg-starts))
                                  (captures (make-array (max num-groups 1) :adjustable t :fill-pointer t)))
                             (if (zerop num-groups)
                                 ;; No capture groups: Perl returns (1) in list context on success
                                 (setf (aref captures 0) 1)
                                 (dotimes (i num-groups)
                                   ;; An unmatched group is perl UNDEF.  It was
                                   ;; raw nil, which %p-flatten-list drops as
                                   ;; "empty list" — so `my ($d,$f) = $p =~
                                   ;; m{^(.*/)?(.*)}` put the FILENAME in $d and
                                   ;; undef in $f whenever the path had no slash.
                                   ;; That is the shape File::Basename::fileparse
                                   ;; uses, so dirname("c.txt") answered "c.txt".
                                   (setf (aref captures i)
                                         (if (and (aref reg-starts i) (aref reg-ends i))
                                             (subseq str (aref reg-starts i) (aref reg-ends i))
                                             *p-undef*))))
                             captures)
                           t))
                     ;; No match: scalar/void context returns Perl's '' (defined
                     ;; false), not undef; list context returns the empty list.
                     (if (eq *wantarray* t) nil "")))))))
      (cl-ppcre:ppcre-syntax-error (e)
        (warn "Regex syntax error: ~A" e)
        nil))))

(defun perl-to-ppcre-replacement (str)
  "Convert Perl-style backreferences ($1, $2, ...) to CL-PPCRE style (\\1, \\2, ...)"
  (with-output-to-string (out)
    (loop with i = 0
          while (< i (length str))
          do (let ((c (char str i)))
               (cond
                 ;; Look for $N where N is 1-9
                 ((and (char= c #\$)
                       (< (1+ i) (length str))
                       (let ((next (char str (1+ i))))
                         (and (digit-char-p next)
                              (not (char= next #\0)))))
                  ;; Convert $N to \N
                  (write-char #\\ out)
                  (write-char (char str (1+ i)) out)
                  (incf i 2))
                 (t
                  (write-char c out)
                  (incf i)))))))

(defun do-regex-subst (string-box op)
  "Perform substitution on boxed string, return count of replacements.
   Also sets capture groups $1, $2, ... from the match."
  ;; to-string on the BOX (not the unboxed value): box-sv runs the ""
  ;; overload and tie FETCH, so an overloaded object substitutes against
  ;; its overloaded string as perl does, never its raw print form (#119).
  (let* ((str (to-string string-box))
         (pattern (perl-regex-to-ppcre (p-subst-op-pattern op)))
         (raw-replacement (p-subst-op-replacement op))
         (modifiers (p-subst-op-modifiers op))
         (eval-p (or (member :e modifiers) (functionp raw-replacement)))
         (replacement (unless eval-p
                        (perl-to-ppcre-replacement (if (stringp raw-replacement)
                                                       raw-replacement ""))))
         (global-p (member :g modifiers))
         (non-destructive-p (member :r modifiers))
         (case-insensitive (member :i modifiers))
         (single-line (member :s modifiers))
         (multi-line (member :m modifiers))
         (extended (member :x modifiers)))
    (handler-case
        (let* ((options (append (when case-insensitive '(:case-insensitive-mode t))
                                (when single-line '(:single-line-mode t))
                                (when multi-line '(:multi-line-mode t))
                                (when extended '(:extended-mode t)))))
          (multiple-value-bind (scanner reg-names closers)
              (%pcl-create-scanner pattern options)
            (let* ((count 0)
                   (result nil))
              (if eval-p
                  ;; s///e: call lambda per match, setting $1..$9 from capture groups
                  ;; :simple-calls t → function receives (match g1 g2 ...) as strings
                  (let ((rep-fn (lambda (whole-match &rest groups)
                                  (incf count)
                                  (clear-capture-groups)
                                  (setf |$&| whole-match)
                                  (when (>= (length groups) 1) (setf $1 (or (nth 0 groups) *p-undef*)))
                                  (when (>= (length groups) 2) (setf $2 (or (nth 1 groups) *p-undef*)))
                                  (when (>= (length groups) 3) (setf $3 (or (nth 2 groups) *p-undef*)))
                                  (when (>= (length groups) 4) (setf $4 (or (nth 3 groups) *p-undef*)))
                                  (when (>= (length groups) 5) (setf $5 (or (nth 4 groups) *p-undef*)))
                                  (when (>= (length groups) 6) (setf $6 (or (nth 5 groups) *p-undef*)))
                                  (when (>= (length groups) 7) (setf $7 (or (nth 6 groups) *p-undef*)))
                                  (when (>= (length groups) 8) (setf $8 (or (nth 7 groups) *p-undef*)))
                                  (when (>= (length groups) 9) (setf $9 (or (nth 8 groups) *p-undef*)))
                                  ;; Populate %+/%- from named groups using reg-names from outer scope
                                  (clrhash %+)
                                  (clrhash |%-|)
                                  (when reg-names
                                    (loop for name in reg-names
                                          for i from 0
                                          when (and name (< i (length groups)))
                                          do (let ((val (nth i groups)))
                                               (when val (setf (gethash name %+) val))
                                               (%pcl-push-named-buffer name (or val *p-undef*)))))
                                  (to-string (funcall raw-replacement)))))
                    (setf result (if global-p
                                     (cl-ppcre:regex-replace-all scanner str rep-fn :simple-calls t)
                                     (cl-ppcre:regex-replace scanner str rep-fn :simple-calls t))))
                  ;; Normal s///: string replacement
                  (progn
                    ;; First, set capture groups from the match
                    (multiple-value-bind (match-start match-end reg-starts reg-ends)
                        (cl-ppcre:scan scanner str)
                      (when match-start
                        (clear-capture-groups)
                        (set-capture-groups str reg-starts reg-ends reg-names)
                        (set-match-vars str match-start match-end reg-starts reg-ends
                                        closers)))
                    ;; Perform the substitution
                    (setf result (if global-p
                                     (cl-ppcre:regex-replace-all scanner str replacement)
                                     (cl-ppcre:regex-replace scanner str replacement)))
                    ;; Count replacements
                    (when (stringp result)
                      (if global-p
                          (setf count (length (cl-ppcre:all-matches-as-strings scanner str)))
                          (when (cl-ppcre:scan scanner str)
                            (setf count 1))))))
              ;; /r: return modified copy, leave original unchanged
              (if non-destructive-p
                  (make-p-box (if (stringp result) result str))
                  ;; Normal: update the boxed string in place, return count.
                  ;; ONLY on a match: perl leaves the variable untouched when
                  ;; nothing matched -- writing the (stringified) original
                  ;; back would replace a blessed object held in the variable
                  ;; with its own print form (concat2.t 3: `$path =~ s|/\z||`
                  ;; on an overloaded object must leave the object alone).
                  (progn
                    (when (and (stringp result) (plusp count))
                      (if (p-box-p string-box)
                          (setf (p-box-value string-box) result
                                (p-box-sv-ok string-box) nil
                                (p-box-nv-ok string-box) nil)
                          (warn "Cannot modify non-boxed value in s///")))
                    ;; perl returns the COUNT on a match and PL_sv_no on a miss
                    ;; -- the dualvar ("" , 0), so `print "<$n>"` shows <> and
                    ;; not <0> (task #416).  "" is false and numifies to 0, so
                    ;; every arithmetic and boolean consumer is unchanged; only
                    ;; a STRING consumer could see the difference, and there
                    ;; perl's answer is the empty string.
                    (if (plusp count) count ""))))))
      (cl-ppcre:ppcre-syntax-error (e)
        (warn "Regex syntax error in s///: ~A" e)
        0))))

(defun expand-tr-chars (str)
  "Expand character ranges in tr/// like 'a-z' to 'abcdefghijklmnopqrstuvwxyz'"
  (with-output-to-string (out)
    (let ((i 0)
          (len (length str)))
      (loop while (< i len)
            do (cond
                 ;; Range like a-z
                 ((and (< (+ i 2) len)
                       (char= (char str (1+ i)) #\-))
                  (let ((start (char str i))
                        (end (char str (+ i 2))))
                    (loop for c from (char-code start) to (char-code end)
                          do (write-char (code-char c) out))
                    (incf i 3)))
                 ;; Single character
                 (t
                  (write-char (char str i) out)
                  (incf i)))))))

(defun %tr-from-index (code from-set sorted-from complement-p)
  "Return the position of codepoint CODE within the (possibly complemented) tr
   search-list, or NIL if CODE is not matched.
   - Non-complement: the first index of CODE in the search list (from-set hash).
   - Complement: CODE's rank among all codepoints NOT in the search list, i.e.
     CODE minus the number of search codepoints strictly less than CODE."
  (if complement-p
      (if (nth-value 1 (gethash code from-set))
          nil
          (let ((less 0))
            (loop for fc across sorted-from while (< fc code) do (incf less))
            (- code less)))
      (gethash code from-set)))

(defun do-tr (string-box op)
  "Perform transliteration on boxed string.  Returns the count of matched chars,
   or (with /r) the transliterated copy without modifying STRING-BOX."
  ;; to-string on the BOX, as in do-regex-subst just above (#119).
  (let* ((str (to-string string-box))
         (modifiers (p-tr-op-modifiers op))
         (complement-p (and (member :c modifiers) t))
         (delete-p (and (member :d modifiers) t))
         (squash-p (and (member :s modifiers) t))
         (return-p (and (member :r modifiers) t))
         (from-chars (expand-tr-chars (p-tr-op-from op)))
         (to-chars (expand-tr-chars (p-tr-op-to op)))
         (to-len (length to-chars))
         (from-set (make-hash-table))
         (count 0)
         (last-out nil)
         (last-was-tr nil))
    ;; from-set: codepoint -> first index in the search list
    (loop for ch across from-chars for i from 0
          do (unless (nth-value 1 (gethash (char-code ch) from-set))
               (setf (gethash (char-code ch) from-set) i)))
    (let* ((sorted-from (when complement-p
                          (sort (remove-duplicates
                                 (map 'vector #'char-code from-chars))
                                #'<)))
           (result
            (with-output-to-string (out)
              (loop for c across str
                    for idx = (%tr-from-index (char-code c) from-set
                                              sorted-from complement-p)
                    do (cond
                         (idx
                          (incf count)
                          (let ((new-char
                                 (cond
                                   ((and delete-p (>= idx to-len)) nil)
                                   ((zerop to-len) c)  ; empty repl, no /d: identity
                                   ((>= idx to-len) (char to-chars (1- to-len)))
                                   (t (char to-chars idx)))))
                            (cond
                              ((null new-char) nil)  ; deleted (/d)
                              ((and squash-p last-was-tr (eql new-char last-out))
                               nil)                   ; squeezed (/s)
                              (t (write-char new-char out)
                                 (setf last-out new-char last-was-tr t)))))
                         (t
                          (write-char c out)
                          (setf last-out c last-was-tr nil)))))))
      (cond
        (return-p result)
        (t
         (if (p-box-p string-box)
             (setf (p-box-value string-box) result
                   (p-box-sv-ok string-box) nil
                   (p-box-nv-ok string-box) nil)
             ;; A count-only tr (empty replacement, no /d) leaves the target
             ;; untouched, and perl accepts that on a read-only value:
             ;; `"\x8c" =~ y o…oo` just counts.  Complain only when the
             ;; result actually differs from the input.
             (when (string/= result str)
               (warn "Cannot modify non-boxed value in tr///")))
         count)))))

(defun p-=~ (string operation)
  "Perl =~ binding operator.
   Dispatches based on operation type:
   - p-regex-match: perform match, return t/nil
   - p-subst-op: perform substitution, modify string, return count
   - p-tr-op: perform transliteration, modify string, return count"
  ;; Unbox operation: $r =~ $qr_var passes a p-box containing the regex struct
  (let ((operation (unbox operation)))
    (cond
      ((p-regex-match-p operation)
       (do-regex-match string operation))
      ((p-subst-op-p operation)
       (do-regex-subst string operation))
      ((p-tr-op-p operation)
       (do-tr string operation))
      ;; $x =~ $pat with a plain string/number pattern: perl compiles the
      ;; stringified value as a regex (also reached by ${qr//} deref, which
      ;; produces the "(?^:...)" string form).
      ((or (stringp operation) (numberp operation))
       (do-regex-match string (p-regex-from-parts operation "")))
      (t
       (warn "Unknown regex operation type: ~A" (type-of operation))
       nil))))

(defun p-!~ (string operation)
  "Perl !~ negative binding operator.  Uses Perl truthiness, not CL nil-ness:
   a failed m// now returns the defined-false \"\" (not nil), so test with
   p-true-p, returning Perl's 1 (true) / \"\" (false)."
  (if (p-true-p (p-=~ string operation)) "" 1))

;;; ============================================================
;;; Helper to create Perl-style arrays
;;; ============================================================

(defun make-p-array (&rest items)
  "Create a Perl-style adjustable array"
  (let ((arr (make-array (length items) :adjustable t :fill-pointer t
                         :initial-contents items)))
    arr))

;;; ============================================================
;;; pack / unpack (basic implementation)
;;; ============================================================

(defun p-pack (template &rest args)
  ;; Self-loading stub: loads pcl-pack.lisp on first call then delegates.
  (let ((loaded (p-load-extension "pcl-pack")))
    (if loaded
        (apply #'p-pack template args)
        (error "p-pack: cl/pcl-pack.lisp not found in ~a"
               (or *pcl-runtime-directory* "(no runtime dir)")))))

(defun p-unpack (template &optional (str $_))
  ;; Self-loading stub: loads pcl-pack.lisp on first call then delegates.
  (let ((loaded (p-load-extension "pcl-pack")))
    (if loaded
        (p-unpack template str)
        (error "p-unpack: cl/pcl-pack.lisp not found in ~a"
               (or *pcl-runtime-directory* "(no runtime dir)")))))

;;; ============================================================
;;; mro — always-available core facility (perl >= 5.10)
;;; ============================================================
;;; In perl, `mro::get_linear_isa` exists WITHOUT any `require mro` — the
;;; facility is built into the interpreter (t/mro/*.t call it bare).  The
;;; IMPLEMENTATION stays at the right layer (lib/mro.pm, transpiled to
;;; cl/pcl-mro.lisp like pack-impl); the runtime owns only the interpreter
;;; fact "always loaded", via the same self-loading-stub pattern as p-pack.
;;; Regenerate after editing the shim:
;;;   ./pl2cl --extension lib/mro.pm > cl/pcl-mro.lisp
;;;   && tools/tag-license cl/pcl-mro.lisp   (the license tag on line 2 —
;;;   Pl/t/license-tag-01.t fails without it; the gen stamp stays line 1)
;;; --extension omits the PROGRAM preamble (task #349): an extension loads INTO
;;; a running program, so emitting one would reset that program's @INC to the
;;; build machine's list at the first mro:: call — and p-load-extension now
;;; DIES on an artifact that does (rule 12).

(p-defpackage :mro)
(p-defpackage :warnings)

(defmacro %pcl-def-ext-stub (name ext)
  ;; Self-loading stub: loads cl/EXT.lisp on first call then delegates to
  ;; the real definition the extension just installed over this stub.
  `(defun ,name (&rest args)
     (let ((loaded (p-load-extension ,ext)))
       (if loaded
           (apply (symbol-function ',name) args)
           (error "~a: cl/~a.lisp not found in ~a" ,ext ,ext
                  (or *pcl-runtime-directory* "(no runtime dir)"))))))

(%pcl-def-ext-stub mro::pl-get_linear_isa "pcl-mro")
(%pcl-def-ext-stub mro::pl-get_mro "pcl-mro")
(%pcl-def-ext-stub mro::pl-set_mro "pcl-mro")
(%pcl-def-ext-stub mro::pl-get_isarev "pcl-mro")
(%pcl-def-ext-stub mro::pl-is_universal "pcl-mro")
(%pcl-def-ext-stub mro::pl-invalidate_all_method_caches "pcl-mro")
(%pcl-def-ext-stub mro::pl-method_changed_in "pcl-mro")

;;; warnings:: query/emit API (charnames' `warnings::enabled('utf8')` etc.) —
;;; `use warnings` is a skipped pragma, so lib/warnings.pm is reached only via
;;; these always-available stubs.  Shim doc: lib/warnings.pm header.
;;; Regenerate after editing the shim:
;;;   ./pl2cl --extension lib/warnings.pm > cl/pcl-warnings.lisp
;;;   && tools/tag-license cl/pcl-warnings.lisp   (license tag on line 2, as for mro)
(%pcl-def-ext-stub warnings::pl-enabled "pcl-warnings")
(%pcl-def-ext-stub warnings::pl-fatal_enabled "pcl-warnings")
(%pcl-def-ext-stub warnings::pl-enabled_at_level "pcl-warnings")
(%pcl-def-ext-stub warnings::pl-fatal_enabled_at_level "pcl-warnings")
(%pcl-def-ext-stub warnings::pl-register_categories "pcl-warnings")
(%pcl-def-ext-stub warnings::pl-warn "pcl-warnings")
(%pcl-def-ext-stub warnings::pl-warnif "pcl-warnings")

;;; ============================================================
;;; Package initialization
;;; ============================================================

;; Export all p- symbols so they're accessible from other packages
;; This includes all functions, macros, and variables with p- prefix
(do-symbols (sym (find-package :pcl))
  (when (and (>= (length (symbol-name sym)) 3)
             (string-equal "PL-" (subseq (symbol-name sym) 0 3)))
    (export sym :pcl)))

;; Perl uses double-precision floats everywhere.
;; Make CL read all float literals as double-float (e.g., 1.5 → 1.5d0, not 1.5f0)
(setf *read-default-float-format* 'double-float)

;; Enable Perl-style named capture groups (?<name>...) in cl-ppcre
(setf cl-ppcre:*allow-named-registers* t)

;;; ============================================================
;;; Stub packages for common Perl modules
;;; ============================================================

;; CORE::__SUB__ that the PARSE could not resolve.  Both sub shapes are
;; rewritten at the shared PPI entry (_rewrite_current_sub): a NAMED sub's
;; __SUB__ becomes \&name, an ANONYMOUS sub's becomes a self-reference
;; variable (task #378).  What is left over reaches here — __SUB__ in no sub
;; at all (perl: undef) and __SUB__ inside a STRING EVAL, whose enclosing sub
;; this parse cannot see (perl: the sub containing the eval).
;;
;; It DIES rather than answering either of those, because the wrong answer is
;; a VALUE the program consumes: the first shape this function ever had was a
;; no-op lambda, and `sub { $_[0] <= 1 ? 1 : $_[0] * __SUB__->($_[0]-1) }`
;; then printed 0 where perl prints 120 — silently.  Rule 12's s329 boundary
;; says exactly this case dies; an effect-only gap may announce and continue.
(defun pl-__SUB__ ()
  (error "PCL: __SUB__ outside any sub, or inside a string eval, is not ~
          supported (docs/not-supported.md); in a named or anonymous sub ~
          it works"))

;; utf8::unicode_to_native / native_to_unicode map between Unicode and the
;; platform's native code point.  On any ASCII (non-EBCDIC) platform — which is
;; all PCL ever targets — both are the identity.  JSON::PP builds its
;; invalid-char regex with chr(utf8::unicode_to_native($i)), so these must exist
;; in the production runtime (not just the test library).  Defined in :pcl and
;; exported so the :utf8 package (which (:use :pcl)) inherits them.
;;
;; THE ONLY DEFINITION.  cl/pcl-test.lisp used to define these two names again
;; for charset_tools.pl, and since the TAP layer loads AFTER the runtime its
;; copy silently won — so `utf8::unicode_to_native` behaved DIFFERENTLY
;; depending on whether Test::More had been loaded (the test copy unboxed, this
;; one did not), and SBCL printed "redefining pcl:pl-unicode_to_native" on
;; stderr for 17 files of the perl-tests sweep.  The unboxing body is the one
;; the whole corpus has actually been running, so it is the one that stays; the
;; other four charset_tools names have no runtime twin and remain there.
(in-package :pcl)
(defun pl-unicode_to_native (&optional cp) (unbox cp))
(defun pl-native_to_unicode (&optional cp) (unbox cp))
(export '(pl-unicode_to_native pl-native_to_unicode))

;; utf8 module stub - on non-EBCDIC systems, uni_to_native/native_to_uni are identity.
;; Note: PCL generates pl- prefix for user function calls (e.g. utf8::upgrade → utf8::pl-upgrade),
;; so stubs in user-accessible packages must use pl- prefix (not p- which is for pcl builtins).
(defpackage :utf8 (:use :cl :pcl))
(in-package :utf8)
(defun pl-encode (&optional str) (declare (ignore str)) 1)
(defun pl-decode (&optional str) (declare (ignore str)) 1)
(defun pl-upgrade (&optional str) (declare (ignore str)) 1)
(defun pl-downgrade (&optional str) (declare (ignore str)) 1)
(defun pl-is_utf8 (&optional str) (declare (ignore str)) 1)
(in-package :pcl)

;; warnings module stub - needed because modules like Carp.pm check $warnings::VERSION
(defpackage :warnings (:use :cl :pcl))
(in-package :warnings)
;; p-defcell, not defvar: these are ORDINARY perl package variables by
;; Pl::GlobalPartition's reckoning, so a transpiled file that references
;; $warnings::VERSION declares the same symbol as a cell.  Declaring it special
;; here and a symbol macro there is a load-time error (task #289) — the two
;; sides must use the one declarer.  p-defcell is define-once, so this
;; initialization still wins.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (p-defcell $VERSION (make-p-box "1.50"))
  (p-defcell $BYTES (make-p-box 12))   ; bytes in warning bitmask (12 in modern Perl)
  )
(defun pl-unimport (&rest args) (declare (ignore args)) nil)
(defun pl-import (&rest args) (declare (ignore args)) nil)
(in-package :pcl)

;; Carp module stub - Carp loads utf8 which causes infinite loops in PCL
;; Stub out the most commonly used functions so code that 'use Carp' works
(defpackage :|Carp| (:use :cl :pcl))
(in-package :|Carp|)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (p-defcell $VERSION (make-p-box "1.50")))   ; a cell, like every other $VERSION
(defun pl-croak (&rest args)
  (error "Carp::croak: ~a" (if args (to-string (car args)) "")))
(defun pl-confess (&rest args)
  (error "Carp::confess: ~a" (if args (to-string (car args)) "")))
(defun pl-carp (&rest args)
  (format *error-output* "~a~%" (if args (to-string (car args)) "")))
(defun pl-cluck (&rest args)
  (format *error-output* "~a~%" (if args (to-string (car args)) "")))
(defun pl-import (&rest args) (declare (ignore args)) nil)
(in-package :pcl)

;; POSIX module stubs
(defpackage :|POSIX| (:use :cl :pcl))
(in-package :|POSIX|)
(defun pl-WIFEXITED (status) (= (logand (unbox status) #xff) 0))
(defun pl-WEXITSTATUS (status) (ash (logand (unbox status) #xff00) -8))
(in-package :pcl)

;; overload module stubs: overload::constant / overload::remove_constant
;; install COMPILE-TIME constant-overloading handlers via %^H — the
;; lexical-hints mechanism PCL does not model (user-blessed 2026-07-28:
;; not-supported.md §Lexical compile-time hints).  Documented no-ops so a
;; file using them runs its other tests instead of dying on an undefined
;; function (re/overload.t).  Runtime `use overload` OPERATOR overloading
;; is fully supported and unrelated (*p-overload-table*).
(defpackage :overload (:use :cl :pcl))
(in-package :overload)
(defun pl-constant (&rest args) (declare (ignore args)) nil)
(defun pl-remove_constant (&rest args) (declare (ignore args)) nil)
(in-package :pcl)

;; Internals module stubs.  The package and the mixed-case function names MUST
;; match what the codegen emits under the :invert readtable: generated code uses
;; the package `Internals` (via p-defpackage, case-preserved — NOT the upcased
;; INTERNALS) and calls each function by its exact Perl-identifier case
;; (`pl-SvREADONLY`, not `pl-svreadonly`).  Writing the tokens in their true case
;; here makes :invert intern the same symbols.  (`stack_refcounted` is all-lower
;; in Perl, so it round-trips either way.)
(defpackage :Internals (:use :cl :pcl))
(in-package :Internals)
;; Returns 0 — PCL is not a reference-counted stack build
(defun pl-stack_refcounted () (make-p-box 0))
;; Internals::SvREADONLY(THING [, FLAG]) — perl's read-only flag (task #159).
;;
;; A MACRO, not a function, for one reason: marking an ARRAY read-only swaps its
;; STORAGE (see %p-array-set-readonly), so it needs the variable's cell, and the
;; call site is the only place that has it — the codegen already emits the array
;; variable itself, `(Internals::pl-SvREADONLY @a 1)`.  CLAUDE.md 11: use the
;; place that already exists rather than teaching Pl/ a special case.  Nothing
;; calls this through #', so a plain macro (deterministic) beats a
;; compiler-macro.
;;
;; Everything else — a scalar, a hash (perl's restricted hashes are a different
;; feature), an aggregate reached through a reference — routes to
;; %p-svreadonly-other, which announces and no-ops.
(defmacro pl-SvREADONLY (&rest args)
  (let ((target (first args)))
    (if (and (symbolp target) target (not (keywordp target))
             (let ((n (symbol-name target)))
               (and (> (length n) 1) (char= (char n 0) #\@))))
        (let ((state `(pcl::make-p-box (if (pcl::%p-array-readonly-p ,target) 1 ""))))
          (if (rest args)
              `(progn (setf ,target (pcl::%p-array-set-readonly ,target ,(second args)))
                      ,state)
              state))
        `(pcl::%p-svreadonly-other ,@args))))
;; Internals::SvREFCNT($ref) — reference count; always 1 in a GC runtime.
(defun pl-SvREFCNT (&rest args)
  (declare (ignore args))
  (make-p-box 1))
(in-package :pcl)

;; DynaLoader/XSLoader stubs
;; These modules load C shared libraries via XS — not supported in PCL.
;; p-use skips loading their .pm files (see *p-xs-only-modules*).
;; We provide stub packages so calls to their functions are harmless no-ops.
;;
;; Note: PCL mistranslates defined(&foo) as (p-defined (p-foo)) — calling
;;; ---------------------------------------------------------------------------
;;; Group database functions (getgrent, setgrent, endgrent, getgrgid, getgrnam)
;;; ---------------------------------------------------------------------------

(defvar *p-group-list* nil "Cached list of group entries for getgrent iteration.")
(defvar *p-group-pos*  0   "Current position in *p-group-list* for getgrent.")

(defun p-group-struct-to-vec (g)
  "Convert sb-posix group struct to a 4-element Perl list vector: (name passwd gid members)."
  (let ((members (sb-posix::group-mem g)))
    (vector
     (make-p-box (sb-posix:group-name g))
     (make-p-box (sb-posix:group-passwd g))
     (make-p-box (sb-posix:group-gid g))
     (make-p-box (if members (format nil "~{~A~^ ~}" members) "")))))

(defun p-setgrent (&key (wantarray (eq *wantarray* t)))
  "Perl setgrent() — rewind the group file for getgrent iteration."
  (declare (ignore wantarray))
  (setf *p-group-list* nil)
  (handler-case
      (sb-posix::do-groups (g)
        (push (p-group-struct-to-vec g) *p-group-list*))
    (sb-posix:syscall-error ()))   ; ignore EOF/ENOENT thrown at end of db
  (setf *p-group-list* (nreverse *p-group-list*))
  (setf *p-group-pos* 0)
  (make-p-box 1))

(defun p-getgrent (&key (wantarray (eq *wantarray* t)))
  "Perl getgrent() — return next group entry from the group database."
  (when (null *p-group-list*)
    (p-setgrent))
  (if (>= *p-group-pos* (length *p-group-list*))
      *p-undef*
      (let ((entry (nth *p-group-pos* *p-group-list*)))
        (incf *p-group-pos*)
        (if wantarray
            entry
            (aref entry 0)))))   ; scalar context: group name only

(defun p-endgrent (&key (wantarray (eq *wantarray* t)))
  "Perl endgrent() — close the group database."
  (declare (ignore wantarray))
  (setf *p-group-list* nil)
  (setf *p-group-pos* 0)
  *p-undef*)

(defun p-getgrgid (gid &key (wantarray (eq *wantarray* t)))
  "Perl getgrgid(GID) — look up group entry by numeric GID."
  (handler-case
      (let ((g (sb-posix:getgrgid (truncate (to-number gid)))))
        (if g
            (if wantarray
                (p-group-struct-to-vec g)
                (make-p-box (sb-posix:group-name g)))
            *p-undef*))
    (sb-posix:syscall-error () *p-undef*)))

;;; ---------------------------------------------------------------------------
;;; Passwd database functions (getpwent, setpwent, endpwent, getpwuid, getpwnam)
;;; — the sibling family of the group functions above, same iteration model.
;;; ---------------------------------------------------------------------------

(defvar *p-passwd-list* nil "Cached list of passwd entries for getpwent iteration.")
(defvar *p-passwd-pos*  0   "Current position in *p-passwd-list* for getpwent.")

(defun p-passwd-struct-to-vec (pw)
  "Convert sb-posix passwd struct to perl's 9-element getpw* list:
   (name passwd uid gid quota comment gcos dir shell) — quota and comment
   are empty strings on Linux, exactly as perl returns them."
  (vector
   (make-p-box (sb-posix:passwd-name pw))
   (make-p-box (sb-posix:passwd-passwd pw))
   (make-p-box (sb-posix:passwd-uid pw))
   (make-p-box (sb-posix:passwd-gid pw))
   (make-p-box "")
   (make-p-box "")
   (make-p-box (sb-posix:passwd-gecos pw))
   (make-p-box (sb-posix:passwd-dir pw))
   (make-p-box (sb-posix:passwd-shell pw))))

(defun p-setpwent (&key (wantarray (eq *wantarray* t)))
  "Perl setpwent() — rewind the passwd file for getpwent iteration."
  (declare (ignore wantarray))
  (setf *p-passwd-list* nil)
  (handler-case
      (sb-posix:do-passwds (pw)
        (push (p-passwd-struct-to-vec pw) *p-passwd-list*))
    (sb-posix:syscall-error ()))
  (setf *p-passwd-list* (nreverse *p-passwd-list*))
  (setf *p-passwd-pos* 0)
  (make-p-box 1))

(defun p-getpwent (&key (wantarray (eq *wantarray* t)))
  "Perl getpwent() — return next passwd entry from the user database."
  (when (null *p-passwd-list*)
    (p-setpwent))
  (if (>= *p-passwd-pos* (length *p-passwd-list*))
      *p-undef*
      (let ((entry (nth *p-passwd-pos* *p-passwd-list*)))
        (incf *p-passwd-pos*)
        (if wantarray
            entry
            (aref entry 0)))))   ; scalar context: user name only

(defun p-endpwent (&key (wantarray (eq *wantarray* t)))
  "Perl endpwent() — close the user database."
  (declare (ignore wantarray))
  (setf *p-passwd-list* nil)
  (setf *p-passwd-pos* 0)
  *p-undef*)

(defun p-getpwuid (uid &key (wantarray (eq *wantarray* t)))
  "Perl getpwuid(UID) — look up passwd entry by numeric UID."
  (handler-case
      (let ((pw (sb-posix:getpwuid (truncate (to-number uid)))))
        (if pw
            (if wantarray
                (p-passwd-struct-to-vec pw)
                (make-p-box (sb-posix:passwd-name pw)))
            *p-undef*))
    (sb-posix:syscall-error () *p-undef*)))

(defun p-getpwnam (name &key (wantarray (eq *wantarray* t)))
  "Perl getpwnam(NAME) — look up passwd entry by user name."
  (handler-case
      (let ((pw (sb-posix:getpwnam (to-string (unbox name)))))
        (if pw
            (if wantarray
                (p-passwd-struct-to-vec pw)
                (make-p-box (sb-posix:passwd-uid pw)))
            *p-undef*))
    (sb-posix:syscall-error () *p-undef*)))

(defun p-getgrnam (name &key (wantarray (eq *wantarray* t)))
  "Perl getgrnam(NAME) — look up group entry by name."
  (handler-case
      (let ((g (sb-posix:getgrnam (to-string name))))
        (if g
            (if wantarray
                (p-group-struct-to-vec g)
                ;; scalar getgrnam = the GID (the name is what you passed in)
                (make-p-box (sb-posix:group-gid g)))
            *p-undef*))
    (sb-posix:syscall-error () *p-undef*)))

(defun p-getlogin ()
  "Perl getlogin — the login name of the controlling terminal's user.  perl
   documents it as unreliable and tells you to prefer getpwuid($<); PCL takes
   that advice, because SBCL exposes no getlogin(3) and the effective-uid
   lookup is what every real caller (Data::Dump's dd.t) actually wants.
   undef when the uid has no passwd entry, as in perl."
  (handler-case
      (let ((pw (sb-posix:getpwuid (sb-posix:getuid))))
        (if pw (make-p-box (sb-posix:passwd-name pw)) *p-undef*))
    (error () *p-undef*)))

;; the function instead of using fboundp. Stubs ensure those calls don't crash.
(defpackage :DynaLoader (:use :cl :pcl))
(in-package :DynaLoader)
(defun pl-boot_DynaLoader (&rest args) (declare (ignore args)) nil)
(defun pl-dl_error (&rest args) (declare (ignore args)) nil)
(defun pl-dl_load_flags (&rest args) (declare (ignore args)) (make-p-box 0))
(defun pl-bootstrap (&rest args) (declare (ignore args)) nil)
(defun pl-bootstrap_inherit (&rest args) (declare (ignore args)) nil)
(defun pl-dl_load_file (&rest args) (declare (ignore args)) nil)
(defun pl-dl_find_symbol (&rest args) (declare (ignore args)) nil)
(defun pl-dl_undef_symbols (&rest args) (declare (ignore args)) nil)

;; Mark the DynaLoader XS boot stubs as genuinely *defined* (not mere forward
;; declarations) so `defined &DynaLoader::boot_DynaLoader` is true.  PCL is a
;; full perl-equivalent, not miniperl — and test.pl's is_miniperl() gates many
;; t/ files (io/scalar.t, …) on exactly this check.  Real perl always has
;; DynaLoader's XS boot routine present in a non-mini build.
(dolist (s '(pl-boot_DynaLoader pl-dl_error pl-dl_load_flags pl-bootstrap
             pl-bootstrap_inherit pl-dl_load_file pl-dl_find_symbol
             pl-dl_undef_symbols))
  (setf (gethash s pcl::*p-declared-subs*) :defined))

;; Back to :pcl — the DynaLoader stubs above left us in their package, and
;; everything below is runtime machinery, not a Perl-visible stub.
(in-package :pcl)

;;; ---------------------------------------------------------------------------
;;; XS artifact cache — finding the shared object built for an XS module.
;;;
;;; Perl compiles XS at INSTALL time and installs the .so into a tree whose
;;; PATH encodes the perl version and architecture; loading is then a pure
;;; lookup.  We copy that, because the alternative (check a version stamp at
;;; load time) is a check somebody can forget, while a path that does not
;;; exist cannot be loaded by accident.
;;;
;;;     ~/.pcl-cache/xs/abi-3/auto/Digest/MD5/MD5.so
;;;                     └─┬─┘
;;;             pclxs ABI, from xs-pin
;;;
;;; The ABI is the critical part of the key: an artifact built against ABI 2
;;; was compiled against a DIFFERENT VTABLE, so loading it under ABI 3 is
;;; undefined behaviour that would present as a crash inside the module.
;;; Bumping the ABI changes the directory, so old artifacts simply stop
;;; being found -- no invalidation logic, nothing to remember.
;;;
;;; Architecture is handled by LOCATION rather than by another path segment:
;;; the cache lives under the user's home, which is per-machine in the
;;; ordinary case.  A home directory shared across architectures needs
;;; PCL_XS_CACHE set per machine.  See docs/xs-artifact-cache.md for why,
;;; and for what would make us change it.
;;; ---------------------------------------------------------------------------

(defparameter *p-xs-cache-dir* nil
  "XS artifact cache root; NIL means <*pcl-cache-dir*>/xs/.  $PCL_XS_CACHE
   overrides, which is what a shared home directory needs.")

(defparameter *p-xs-artifact-suffix* "so"
  "Artifact extension.  Must match what tools/pcl-xs-install passes to
   xs-build --suffix; both are ours, so they agree by construction.")

(defun %p-xs-cache-root ()
  "Where built XS artifacts live, honouring $PCL_XS_CACHE."
  (let ((env (sb-posix:getenv "PCL_XS_CACHE")))
    (cond (env (pathname (concatenate 'string env "/")))
          (*p-xs-cache-dir* *p-xs-cache-dir*)
          (t (merge-pathnames "xs/" *pcl-cache-dir*)))))

(defun %p-xs-abi ()
  "The pclxs ABI this checkout is pinned to, from xs-pin, or NIL.
   Read from the pin rather than from libpclxs on purpose: the path has to
   be computed BEFORE deciding whether to load the library at all, and
   tools/build-pclxs already refuses to build when pin and checkout differ.
   A pin that lies is caught later anyway -- pclxs_init rejects a vtable
   whose abi_version does not match."
  (let ((pin (and *pcl-runtime-directory*
                  (merge-pathnames "../xs-pin" *pcl-runtime-directory*))))
    (when (and pin (probe-file pin))
      (with-open-file (in pin :if-does-not-exist nil)
        (when in
          (loop for line = (read-line in nil nil)
                while line
                do (let ((p (search "abi " line)))
                     (when (and p (zerop p))
                       (return (parse-integer (subseq line 4)
                                              :junk-allowed t))))))))))

(defun %p-xs-name-parts (module)
  "\"Digest::MD5\" -> (\"Digest\" \"MD5\").  Perl's own auto/ layout rule."
  (let ((parts '()) (start 0))
    (loop for p = (search "::" module :start2 start)
          while p
          do (push (subseq module start p) parts)
          (setf start (+ p 2)))
    (push (subseq module start) parts)
    (nreverse parts)))

(defun %p-xs-artifact-path (module)
  "MODULE (\"Digest::MD5\") -> the artifact path, or NIL without an ABI."
  (let ((abi (%p-xs-abi)))
    (when abi
      (let* ((parts (%p-xs-name-parts module))
             (leaf  (car (last parts)))
             (rel   (format nil "abi-~D/auto/~{~A/~}~A.~A"
                            abi parts leaf *p-xs-artifact-suffix*)))
        (merge-pathnames rel (%p-xs-cache-root))))))

(defun %p-xs-boot-symbol (module)
  "Perl's rule: boot_Foo__Bar for Foo::Bar."
  (with-output-to-string (out)
    (write-string "boot_" out)
    (loop for ch across module
          do (if (char= ch #\:) (write-char #\_ out) (write-char ch out)))))

(defun %p-xs-bridge-loaded-p ()
  (let ((s (find-symbol "P-XS-BOOT" :pcl)))
    (and s (fboundp s))))

(defun %p-xs-ensure-bridge ()
  "Load cl/pcl-xs.lisp on demand.  It is not loaded up front because it
   needs libpclxs at load time, and a PCL program that never touches XS
   must not need the shim built at all."
  (or (%p-xs-bridge-loaded-p)
      (let ((f (and *pcl-runtime-directory*
                    (merge-pathnames "pcl-xs.lisp" *pcl-runtime-directory*))))
        (when (and f (probe-file f))
          (handler-case (progn (load f) (%p-xs-bridge-loaded-p))
            (error (e)
              (format *error-output*
                      "~&pcl: XS bridge present but failed to load: ~A~%" e)
              nil))))))

(defun %p-xs-try-load (module)
  "Boot MODULE's cached artifact.  T on success, NIL if there is nothing to
   load -- and NIL must stay quiet, because the caller's fallback is the
   error perl gives when a module has no .so, which is what makes
   dual-life modules (Data::Dumper, Time::HiRes) fall back to pure Perl."
  (let ((path (%p-xs-artifact-path module)))
    (when (and path (probe-file path) (%p-xs-ensure-bridge))
      (funcall (find-symbol "P-XS-BOOT" :pcl)
               (namestring (truename path))
               (%p-xs-boot-symbol module))
      t)))

(defpackage :XSLoader (:use :cl :pcl))
(in-package :XSLoader)
;; XSLoader::load('Module', $version) — PCL cannot load XS, so this MUST fail
;; exactly as it would on a system where the loadable object is missing.  The
;; standard dual-life idiom `eval { require XSLoader; XSLoader::load(...); 1 }
;; or $Useperl = 1;` (Data::Dumper, Time::HiRes, etc.) then falls back to the
;; pure-Perl implementation.  A no-op success would leave $Useperl=0 and call
;; the nonexistent XS sub (e.g. Data::Dumper::Dumpxs).
(defun pl-load (&rest args)
  (let ((mod (if args (to-string (first args)) "this module")))
    ;; Try the XS bridge first: if this module has an artifact in the cache
    ;; (built by tools/pcl-xs-install), boot it and we are done.
    (unless (pcl::%p-xs-try-load mod)
      ;; Nothing built.  Fail EXACTLY as perl does on a system where the
      ;; loadable object is missing -- see the comment above; this message
      ;; is load-bearing for every dual-life module on CPAN.
      (p-die (format nil "Can't locate loadable object for module ~A in @INC"
                     mod)))
    1))
(defun pl-bootstrap_inherit (&rest args) (declare (ignore args)) nil)
;;; UNIVERSAL package methods — callable as UNIVERSAL::can($obj, $m) etc.
(defpackage :UNIVERSAL (:use :cl :pcl))
(in-package :UNIVERSAL)
(defun pl-can  (obj method &rest args) (declare (ignore args)) (p-can  obj method))
(defun pl-isa  (obj class  &rest args)
  (declare (ignore args))
  ;; Perl's UNIVERSAL::isa(REF, TYPE) carries interpreter-baked behaviour beyond
  ;; @ISA: when TYPE names a builtin reference type (ARRAY/HASH/SCALAR/CODE/GLOB/
  ;; LVALUE/…) it is true iff reftype(REF) eq TYPE — regardless of blessing.
  ;; p-reftype is undef (NOT "") for a non-ref, so ordinary strings/numbers fall
  ;; through to the normal @ISA inheritance check.  (A blessed hashref isa "HASH"
  ;; AND isa its class; both work — reftype path then @ISA path.)
  (let ((rt (p-reftype obj)))
    (if (and (stringp rt) (plusp (length rt)) (string= rt (to-string class)))
        (make-p-box 1)
        (p-isa obj class))))
(defun pl-DOES (obj class  &rest args) (declare (ignore args)) (pl-isa obj class))
(defun pl-VERSION (&rest args) (declare (ignore args)) nil)

(in-package :pcl)

;;; Lexical pragmas as no-op import/unimport methods.
;;; strict/warnings/feature/... manipulate the COMPILE-TIME hint bitmasks
;;; ($^H, ${^WARNING_BITS}) — purely lexical, meaningless at runtime, and PCL
;;; does not enforce them.  `use strict` is already a parser no-op, but a
;;; module's import calling `strict->import` / `warnings->import` as a METHOD
;;; (Role::Tiny, Moo) would otherwise load the core .pm, whose import does
;;; `$^H |= bits` → STRICT::$^H unbound.  Defining the stubs here makes the
;;; method resolve to a no-op (and find-symbol-first prevents the core file from
;;; being loaded), so we never have to model $^H at all.
(eval-when (:load-toplevel :execute)
  (dolist (pragma *p-pragma-modules*)
    (let* ((p (string-upcase pragma))
           (pkg (or (find-package p) (make-package p :use '(:cl :pcl)))))
      (dolist (m '("PL-IMPORT" "PL-UNIMPORT"))
        (let ((sym (intern m pkg)))
          (setf (fdefinition sym) (lambda (&rest a) (declare (ignore a)) nil))
          (setf (gethash sym *p-declared-subs*) :defined))))))

;;; Extension loading registry — tracks which extension files have been loaded.
(defvar *pcl-loaded-extensions* (make-hash-table :test 'equal))

;;; Load a named extension .lisp file from *pcl-runtime-directory*.
;;; Skips if already loaded. Returns t if the file was found and loaded, nil otherwise.
;;; Called eagerly for built-in extensions, or lazily from generated code.
(defun p-load-extension (name)
  (unless (gethash name *pcl-loaded-extensions*)
    (when *pcl-runtime-directory*
      (let ((file (merge-pathnames
                   (concatenate 'string name ".lisp")
                   *pcl-runtime-directory*)))
        (when (probe-file file)
          (let ((inc @INC)
                (len (length @INC))
                (pl2cl *pcl-pl2cl-path*)
                (core-dirs *p-core-inc-dirs*))
            (handler-bind ((warning #'muffle-warning))
              (load file))
            (%pcl-check-extension-clean name inc len pl2cl core-dirs))
          (setf (gethash name *pcl-loaded-extensions*) t)
          (return-from p-load-extension t)))))
  nil)

;;; An extension is `load`ed INTO a running program, at its first
;;; pack/mro/warnings call — so it may install DEFINITIONS and nothing else.
;;;
;;; It used to install a PROGRAM preamble as well, because `pl2cl` emitted one
;;; for every file it compiled: loading cl/pcl-pack.lisp REPLACED the running
;;; program's @INC with the build machine's list, so
;;;
;;;     push @INC, "/tmp/mylib"; pack("N", 42);   perl keeps it, PCL lost it
;;;
;;; silently, until a later `require` could not find a module the program had
;;; just added (task #349) — and it is why the checked-in artifacts embedded
;;; one machine's absolute paths at all (task #217).  `pl2cl --extension` emits
;;; no preamble; this is rule 12's half: an artifact built WITHOUT the flag
;;; dies here naming itself, instead of quietly editing the program's state.
(defun %pcl-check-extension-clean (name inc len pl2cl core-dirs)
  (unless (and (eq inc @INC) (= len (length @INC))
               (equal pl2cl *pcl-pl2cl-path*)
               (equal core-dirs *p-core-inc-dirs*))
    (error "PCL: extension ~a.lisp changed the program's load state (@INC / ~
            *pcl-pl2cl-path* / *p-core-inc-dirs*).  It was built WITHOUT ~
            `pl2cl --extension`, so it carries a program preamble: regenerate ~
            it (tools/rebuild-pack, or ./pl2cl --extension lib/<mod>.pm)."
           name))
  nil)

;;; pack/unpack loaded lazily on first call via self-loading stubs above.

;;; --- Policy for GENERATED USER CODE (compiled after this file) ----------
;;; Re-enable inlining of the hot fast-path operators/accessors (their
;;; expansions were stored at definition; the runtime itself compiled them
;;; notinline to keep this file's per-process load cheap — see file top),
;;; and raise the optimize policy so user call sites open-code the
;;; numberp/stringp fast paths as native ops.
(declaim (inline p-+ p-- p-* p-/ p-%
                 p-== p-!= p-< p-> p-<= p->= p-<=>
                 p-. p-str-eq p-str-ne p-str-lt p-str-gt p-str-le p-str-ge
                 p-str-cmp
                 unbox to-number to-string p-true-p p-bool %pcl-nan-p))
(declaim (optimize (speed 2) (safety 1) (debug 0)))

;; Diagnostic banner on *error-output*, not *standard-output*, so it never
;; pollutes a script's stdout when run via the `pcl` command. Test harnesses
;; capture 2>&1 and filter it, so they are unaffected.
(format *error-output* "PCL Runtime loaded~%")
