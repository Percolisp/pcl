;;;; PCL Runtime - Common Lisp runtime for Perl to CL transpiler
;;;; Requires SBCL (Steel Bank Common Lisp)
;;;;
;;;; Copyright (c) 2025-2026
;;;; This is free software; you can redistribute it and/or modify it
;;;; under the same terms as the Perl 5 programming language system itself.

;;; Load CL-PPCRE for regex support
(require :asdf)
(asdf:load-system :cl-ppcre :silent t)

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

(defpackage :pcl
  (:use :cl)
  (:export
   ;; Value boxing
   #:p-box #:make-p-box #:p-box-p #:p-box-value
   #:unbox #:ensure-boxed #:p-copy-scalar-arg
   #:box-set #:box-nv #:box-sv  ; lazy caching accessors
   #:to-string #:to-number
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
   ;; Assignment
   #:p-setf #:p-incf #:p-decf
   #:p-pre++ #:p-post++ #:p-pre-- #:p-post--
   ;; Compound assignment
   #:p-*= #:p-/= #:p-%= #:p-**=
   #:p-.= #:p-str-x=
   #:p-bit-and= #:p-bit-or= #:p-bit-xor= #:p-<<= #:p->>=
   #:p-str-bit-and= #:p-str-bit-or= #:p-str-bit-xor=
   #:p-and-assign #:p-or-assign #:p-//=
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
   #:p-aref #:p-aref-box #:p-aref-deref #:p-aref-deref-box #:p-gethash #:p-gethash-box #:p-gethash-deref #:p-gethash-deref-box
   #:p-ensure-hashref #:p-ensure-arrayref
   #:p-aslice #:p-hslice #:p-kv-hslice #:p-kv-aslice #:p-list-scalar #:p-slice-result
   #:p-hash #:p-array-init #:p-array-last-index #:p-set-array-length
   #:p-push #:p-pop #:p-shift #:p-unshift #:p-splice #:p-flatten #:p-flatten-args
   #:p-check-arity #:p-sig-rest-array #:p-sig-rest-hash
   #:p-keys #:p-values #:p-each #:p-exists #:p-exists-array #:p-delete #:p-delete-array
   #:p-delete-hash-slice #:p-delete-kv-hash-slice #:p-delete-array-slice #:p-delete-kv-array-slice
   ;; Control flow
   #:p-if #:p-unless #:p-while #:p-until #:p-do-while #:p-do-until #:p-for #:p-foreach
   #:p-return #:p-goto-sub #:p-goto-computed #:p-last #:p-last-dynamic #:p-next #:p-redo
   #:p-continue #:p-break
   ;; I/O
   #:p-print #:p-say #:p-warn #:p-die
   ;; do BLOCK
   #:p-do
   ;; Exception handling
   #:p-eval #:p-eval-block #:p-eval-thunk #:p-eval-lex-lookup
   #:*p-eval-lex-alist*
   #:p-exception #:p-exception-object
   ;; File I/O
   #:p-open #:p-close #:p-eof #:p-tell #:p-seek #:p-pipe #:p-select #:p-write
   #:p-binmode #:p-read #:p-sysread #:p-syswrite
   ;; Socket builtins
   #:p-socket #:p-socketpair #:p-bind #:p-connect #:p-listen #:p-accept
   #:p-send #:p-recv #:p-shutdown #:p-getsockname #:p-getpeername
   #:p-getprotobyname #:p-setsockopt #:p-getsockopt
   #:p-truncate #:p-stat #:p-lstat
   ;; File test operators
   #:p--e #:p--d #:p--f #:p--r #:p--w #:p--x #:p--s #:p--z
   #:p-unlink #:p-fileno #:p-getc #:p-readline #:*p-filehandles*
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
   #:p-exit #:p-system #:p-backtick #:p-errno-string #:p-stash
   ;; Group/passwd database
   #:p-getgrent #:p-setgrent #:p-endgrent #:p-getgrgid #:p-getgrnam
   ;; Environment
   #:%ENV #:p-env-get #:p-env-set
   ;; Module system
   #:@INC #:%INC #:%SIG #:@ARGV #:$ARGV #:@_ #:%_args #:p-use #:p-require #:p-require-parent #:p-require-file
   ;; Functions
   #:p-backslash #:p-backslash-sub #:p-arylen-ref #:p-substr-ref #:p-pos-ref #:p-vec-ref #:p-substr-lvalue-cell #:p-pos-lvalue-cell #:p-vec-lvalue-cell #:p-refgen-list #:p-box-for-local #:p-get-coderef #:p-ref #:p-reftype #:p-scalar #:p-wantarray #:p-caller #:p-prototype
   ;; Typeglob support
   #:p-typeglob #:p-typeglob-p #:make-p-typeglob
   #:p-typeglob-package #:p-typeglob-name
   #:p-make-typeglob #:p-glob-assign #:p-glob-assign-dynamic
   #:p-dynamic-typeglob #:p-glob-copy
   #:p-glob-slot #:p-glob-undef-name #:p-local-glob #:p-local-glob-if #:p-local-dot
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
   #:|$&| #:|$`| #:|$'| #:|$+| #:|@-| #:|@+|
   ;; Special variables
   #:$$ #:$? #:|$.| #:$0 #:$@ #:|$^O| #:|$^V| #:|$^X| #:|$^T| #:|$^H| #:|%^H| #:|${^TAINT}| #:|$/| #:|$\\| #:|$"| #:|$\|| #:|$;| #:|$,| #:|$]|
   #:|$~| #:|$=| #:|$-| #:|$%| #:|$:| #:|$^L| #:|$^A| #:|$^| #:|$^R| #:|$^P| #:|$^D| #:|$^F| #:|$^I| #:|$^M|
   ;; Context
   #:*wantarray*
   #:*pcl-caller-wantarray*
   #:*p-in-list-assign-rhs*
   ;; Call depth tracking (for p-caller at top level)
   #:*pcl-sub-call-depth*
   ;; Current/caller package tracking (for caller() package, __PACKAGE__-at-runtime)
   #:*pcl-current-package* #:*pcl-caller-pkg-stack* #:*pcl-caller-subname-stack*
   #:p-set-current-package #:p-register-pkg-name
   ;; END blocks
   #:*end-blocks*
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
   #:p-defpackage #:p-sub #:p-args-body #:p-declare-sub
   ;; eval-when wrappers (named for readability in generated CL)
   #:p-eval-always #:p-BEGIN #:p-CHECK
   ;; Assignment forms (distinct from p-setf for clarity)
   #:p-scalar-= #:p-array-= #:p-hash-= #:p-list-= #:p-array-fill #:p-hash-fill
   ;; Lexical 'my' variable assignment (no auto-declare side-effect)
   #:p-my-=))

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
         (proclaim (list 'special isa-sym))
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
(defun %pcl-invert-case (s)
  (let ((s (string s)) (has-upper nil) (has-lower nil))
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
       (let ((local-sym (intern sym-name target-pkg)))
         (setf (gethash local-sym *p-declared-subs*) :defined)
         (setf (symbol-function local-sym)
               (lambda ,params
                 (let* ((*pcl-caller-pkg-stack* (cons *pcl-current-package*
                                                      *pcl-caller-pkg-stack*))
                        (*pcl-caller-subname-stack* (cons (%p-sub-perl-name ',name)
                                                          *pcl-caller-subname-stack*))
                        (*pcl-current-package* (pcl-pkg-perl-name
                                                (symbol-package ',name)))
                        (*pcl-sub-call-depth* (1+ *pcl-sub-call-depth*))
                        (*pcl-caller-wantarray* *wantarray*))
                   (catch :p-return
                     ,@body))))))))

(defmacro p-args-body (&body body)
  "Standard named-sub prologue emitted by the code generator: bind Perl's @_
   from the &rest %_args captured by the enclosing p-sub lambda list, then run
   BODY.  Both @_ and %_args are symbols exported from :pcl and inherited into
   every generated user package, so `(p-sub NAME (&rest %_args) (p-args-body …))`
   binds and reads the SAME symbols here regardless of *package* — no
   expansion-time package resolution needed."
  `(let ((@_ (p-flatten-args %_args)))
     ,@body))

;;; p-declare-sub: Forward-declare a Perl sub as a no-op stub.
;;; Perl subs can be called before definition; CL resolves names at load time.
;;; Only creates the stub if the function isn't already defined.
;;; Marks the symbol as :stub in *p-declared-subs* for exists &sub support.
(defmacro p-declare-sub (name)
  `(progn
     (unless (gethash ',name *p-declared-subs*)
       (setf (gethash ',name *p-declared-subs*) :stub))
     (unless (fboundp ',name)
       (defun ,name (&rest args) (declare (ignore args)) nil))))

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

;;; Forward declarations to avoid style warnings
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

;;; Persistent transpiler subprocess for p-eval
(defvar *p-transpiler-process* nil
  "Persistent pl2cl --server process, or nil if not yet started.
   Started lazily on first p-transpile-string call. Restarted if it dies.")

;;; Sub declaration/definition tracking for exists &sub / defined &sub
;;; Maps CL function symbol → :stub (declared only), :defined (has body),
;;; or :was-defined (was defined, now undef'd).
(defvar *p-declared-subs* (make-hash-table :test 'eq)
  "Perl sub existence tracking for exists &sub and defined &sub")

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
(defvar |$&| nil "Regex MATCH - the whole matched string")
(defvar |$`| nil "Regex PREMATCH - everything before the match")
(defvar |$'| nil "Regex POSTMATCH - everything after the match")
(defvar |$+| nil "Regex - last (highest-numbered) capture group that matched")
(defvar %+ (make-hash-table :test 'equal) "Perl %+ - named regex captures")
;; @- (@LAST_MATCH_START) and @+ (@LAST_MATCH_END): offset arrays from the last
;; successful match.  Element 0 is the whole-match start/end; element N is the
;; start/end of capture group N.  Non-participating groups hold undef (nil).
(defvar |@-| (make-array 0 :adjustable t :fill-pointer 0) "Regex @LAST_MATCH_START - match/group start offsets")
(defvar |@+| (make-array 0 :adjustable t :fill-pointer 0) "Regex @LAST_MATCH_END - match/group end offsets")

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

;;; Perl version ($^V) - we report as PCL
(defvar |$^V| "v5.30.0" "Perl version (compatibility)")

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

;;; END blocks - executed in reverse order at program exit
(defvar *end-blocks* nil "List of END block thunks to execute at exit")

;; Register exit hook to run END blocks
(pushnew (lambda ()
           (dolist (fn *end-blocks*)
             (handler-case (funcall fn)
               (error (e)
                 (format *error-output* "Error in END block: ~A~%" e)))))
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
  "Holds the tie object and the pre-tie value for a tied scalar."
  tie-obj       ; object returned by TIESCALAR/TIEARRAY/TIEHASH
  saved-value)  ; p-box-value before tie was installed (restored on untie)

;; A tie object can hold a (blessed) ref back to the very box it proxies — e.g.
;; `sub TIESCALAR { bless \my $x }` — making the structure self-referential.
;; The default structure printer would recurse forever and exhaust the control
;; stack, so print opaquely instead of descending into the slots.
(defmethod print-object ((p p-tie-proxy) stream)
  (print-unreadable-object (p stream :type t :identity t)))

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

(defun unbox (val)
  "Extract value from a box, or return val if not boxed.
   If the box contains a p-tie-proxy, dispatches to FETCH.
   If the box contains a p-magic-cell, dispatches to its getter."
  (if (p-box-p val)
      (let ((v (p-box-value val)))
        (cond
          ((p-tie-proxy-p v)
           (unbox (p-method-call (p-tie-proxy-tie-obj v) "FETCH")))
          ((p-magic-cell-p v)
           (funcall (p-magic-cell-getter v)))
          (t v)))
      val))

(defun ensure-boxed (val)
  "Ensure a value is boxed"
  (if (p-box-p val)
      val
      (make-p-box val)))

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
;;; Output autoflush ($|)
(defvar |$\|| (make-p-box 0) "Output autoflush flag")
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
(defvar |$^P| (make-p-box 0)  "PERLDB - internal debugger flag (0 = not debugging)")
(defvar |$^D| (make-p-box 0)  "DEBUGGING - debugging flags")
(defvar |$^F| (make-p-box 2)  "SYSTEM_FD_MAX - max file descriptor for subprocesses")
(defvar |$^I| (make-p-box *p-undef*) "INPLACE_EDIT - in-place edit extension")
(defvar |$^M| (make-p-box *p-undef*) "emergency memory pool")
(defvar |$^| (make-p-box "STDOUT_TOP") "FORMAT_TOP_NAME - top-of-page format name (defaults to <handle>_TOP, like Perl)")
;; %SIG: signal/exception handler hash
;; __WARN__ and __DIE__ keys hold Perl callbacks invoked by warn/die.
(defvar %SIG (make-hash-table :test 'equal) "Perl %SIG - signal handlers")

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
      (return-from box-set
        (p-method-call (p-tie-proxy-tie-obj current) "STORE"
                       (if (p-box-p value) (unbox value) value))))
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
                    (unbox (p-method-call (p-tie-proxy-tie-obj inner) "FETCH")))
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
    ;; Preserve class from blessed boxes
    ;; Preserve class from blessed boxes.  Otherwise, CLEAR a stale class —
    ;; but only when the box's OLD value was itself the reference (vector/
    ;; hash/function/box): overwriting a blessed REFERENCE-holder with a plain
    ;; value unblesses the variable (substr.t: a substr-lvalue write through
    ;; an overloaded object leaves a plain string, else the stale class keeps
    ;; firing overloaded "" on it).  A blessed scalar REFERENT (old value a
    ;; plain scalar) keeps its class on assignment, like Perl's SV stash:
    ;; qr.t `$$e = 'Fake!'` leaves $e blessed into Stew.
    (if (and (p-box-p value) (p-box-class value))
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
      (let* ((inner (unbox obj))
             (cls   (p-get-class obj))
             (raw   (cond
                      ((hash-table-p inner)
                       (format nil "HASH(0x~(~X~))" (object-address inner)))
                      ((and (vectorp inner) (not (stringp inner)))
                       (format nil "ARRAY(0x~(~X~))" (object-address inner)))
                      (t
                       (format nil "SCALAR(0x~(~X~))" (object-address obj))))))
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
        (to-number (p-method-call (p-tie-proxy-tie-obj inner) "FETCH"))))
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
                   ((p-box-p v) (object-address v))  ; reference/blessed scalar ref: address
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
    ((p-box-p v) (format nil "SCALAR(0x~(~X~))" (object-address v)))
    ((hash-table-p v) (format nil "HASH(0x~(~X~))" (object-address v)))
    ((vectorp v) (format nil "ARRAY(0x~(~X~))" (object-address v)))
    ((p-typeglob-p v) (format nil "*~A::~A"
                              (package-name (p-typeglob-package v))
                              (p-typeglob-name v)))
    ;; Code reference - stringify as CODE(0xADDR) like Perl
    ((functionp v) (format nil "CODE(0x~(~X~))" (object-address v)))
    ;; Compiled regex (qr//) — stringify as (?^modifiers:pattern) like Perl 5.14+
    ((p-regex-match-p v)
     ;; modifiers is the plist from parse-regex-modifiers, keyed by the upcased
     ;; flag letter (:M :S :I :X ...).  Perl stringifies qr// flags in the fixed
     ;; order m,s,i,x (e.g. qr/x/imsx -> "(?^msix:x)").  (The old code checked
     ;; :case-insensitive etc. — keys that are never present — so flags were
     ;; always dropped.)
     (let* ((mods (p-regex-match-modifiers v))
            (mod-str (concatenate 'string
                                  (if (getf mods :m) "m" "")
                                  (if (getf mods :s) "s" "")
                                  (if (getf mods :i) "i" "")
                                  (if (getf mods :x) "x" ""))))
       (format nil "(?^~A:~A)" mod-str (p-regex-match-pattern v))))
    ;; Lists (from return lists, etc.) - join with spaces like Perl's @array interpolation
    ((listp v) (format nil "~{~A~^ ~}" (mapcar #'to-string v)))
    ;; CL's T from comparison operators - Perl true stringifies to "1"
    ((eq v t) "1")
    ;; Super-Unicode character (code > U+10FFFF) — no CL char representation; use U+FFFD
    ((p-superchar-p v) (string #\REPLACEMENT_CHARACTER))
    (t (format nil "~A" v))))

(defun box-sv (box)
  "Get string value from box with lazy caching.
   Tied variables: bypass cache and call FETCH."
  (let ((inner (p-box-value box)))
    (when (p-tie-proxy-p inner)
      (return-from box-sv
        (to-string (p-method-call (p-tie-proxy-tie-obj inner) "FETCH"))))
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
             (class (or (p-box-class box)
                        (when (hash-table-p inner)
                          (gethash :__class__ inner))))
             (raw (cond
                    ;; Blessed typeglob ref: stringify as GLOB(0xADDR) not *PKG::NAME
                    ((p-typeglob-p inner)
                     (format nil "GLOB(0x~(~X~))" (object-address inner)))
                    ;; Nested p-box: look one level deeper to distinguish SCALAR from REF.
                    ;; inner = ref-box; inner2 = what it points to.
                    ;; If inner2 holds a reference type → outer is a ref-to-ref → "REF".
                    ;; If inner2 holds a scalar value (nil/string/num) → "SCALAR".
                    ((p-box-p inner)
                     (let* ((inner2 (p-box-value inner))
                            (inner3 (when (p-box-p inner2) (p-box-value inner2))))
                       (cond
                         ;; \substr / \pos / \vec lvalue ref → "LVALUE(0x...)".
                         ((and (p-magic-cell-p inner3)
                               (eq (p-magic-cell-kind inner3) :lvalue))
                          (format nil "LVALUE(0x~(~X~))" (object-address inner)))
                         ;; inner2 is a box with a scalar payload → SCALAR ref
                         ((and (p-box-p inner2)
                               (not (or (p-box-p inner3)
                                        (and (vectorp inner3) (not (stringp inner3)))
                                        (hash-table-p inner3)
                                        (functionp inner3)
                                        (p-typeglob-p inner3)
                                        (p-regex-match-p inner3))))
                          (format nil "SCALAR(0x~(~X~))" (object-address inner)))
                         ;; inner2 is a ref-type or a raw value → REF
                         (t (format nil "REF(0x~(~X~))" (object-address inner))))))
                    (t (stringify-value inner))))
             (s (if class
                    (format nil "~A=~A" class raw)
                    raw)))
        (setf (p-box-sv box) s
              (p-box-sv-ok box) t)
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

(defun p-defined-fh (fh-sym)
  "Check if a bareword filehandle or dirhandle (symbol) is open.
   Used by codegen for defined(FILEHANDLE) expressions.
   Checks *p-filehandles* (open files) and *p-dirhandles* (open dirs)."
  (or (let ((stream (gethash fh-sym *p-filehandles*)))
        (and stream (open-stream-p stream) t))
      ;; *p-dirhandles* is defined later in this file; the forward reference
      ;; is a compile-time warning only — at runtime the variable is bound.
      (and (ignore-errors (gethash fh-sym *p-dirhandles*)) t)))

(defun p-true-p (val)
  "Perl truthiness: false if undef, 0, empty string, empty list, or nil.

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
        (return-from p-true-p
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
          (t t)))
      ;; Raw value: bare aggregate → count; scalar → normal truthiness.
      (cond
        ((eq val *p-undef*) nil)
        ((null val) nil)
        ((and (numberp val) (not (%pcl-nan-p val)) (zerop val)) nil)
        ((and (stringp val) (string= val "")) nil)
        ((and (stringp val) (string= val "0")) nil)
        ;; bare @array / %hash in boolean context: true iff non-empty
        ((and (vectorp val) (not (stringp val))) (> (length val) 0))
        ((hash-table-p val) (> (%p-hash-user-count val) 0))
        (t t))))

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
;;; Checks left operand first, then right (reversed), then falls back to CL-OP.
(defun %pcl-ieee-arith (thunk)
  "Call THUNK for numeric result; return NaN/Inf on IEEE floating-point exceptions."
  (sb-int:with-float-traps-masked (:invalid :overflow)
    (funcall thunk)))

(defmacro %def-overloaded-arith (name op-str cl-op)
  `(defun ,name (a &optional (b nil b-supplied-p))
     ,(format nil "Perl ~A with use overload dispatch" op-str)
     (if (not b-supplied-p)
         ;; Unary form: e.g. +(expr) — return as-is (no overload for unary +)
         a
         ;; use overload: check left operand, then right (reversed flag = t)
         (let ((ha (p-find-overload a ,op-str)))
           (if ha (p-call-overload ha a b nil)
               (let ((hb (p-find-overload b ,op-str)))
                 (if hb (p-call-overload hb b a t)
                     (%pcl-ieee-arith (lambda () (,cl-op (to-number a) (to-number b)))))))))))

(%def-overloaded-arith p-+ "+" +)
(%def-overloaded-arith p-* "*" *)

(defun p-- (a &optional b)
  "Perl subtraction / unary minus with use overload dispatch.
   Unary: checks 'neg' overload, then applies Perl string-negation rules.
   Binary: checks '-' overload on either operand."
  (if (null b)
      ;; Unary minus
      (progn
        ;; use overload "neg": unary minus overload
        (let ((h-neg (p-find-overload a "neg")))
          (when h-neg (return-from p-- (p-call-overload h-neg a nil nil))))
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
      ;; Binary subtraction
      ;; use overload "-": binary minus overload
      (let ((ha (p-find-overload a "-")))
        (if ha (p-call-overload ha a b nil)
            (let ((hb (p-find-overload b "-")))
              (if hb (p-call-overload hb b a t)
                  (%pcl-ieee-arith (lambda () (- (to-number a) (to-number b))))))))))

(defun p-/ (a b)
  "Perl division with use overload '/' dispatch"
  ;; use overload "/": division overload
  (let ((ha (p-find-overload a "/")))
    (if ha (p-call-overload ha a b nil)
        (let ((hb (p-find-overload b "/")))
          (if hb (p-call-overload hb b a t)
              ;; CL integer/integer -> ratio; Perl gives float for non-integer results.
              ;; Use (typep r 'ratio) not rationalp: rationalp is true for integers too,
              ;; so (/ bignum 2) would crash trying to coerce a huge exact-integer to float.
              (let ((r (%pcl-ieee-arith (lambda () (/ (to-number a) (to-number b))))))
                (if (typep r 'ratio) (coerce r 'double-float) r)))))))
(defun p-% (a b)
  "Perl modulo with use overload '%' dispatch"
  ;; use overload "%": modulo overload
  (let ((ha (p-find-overload a "%")))
    (if ha (p-call-overload ha a b nil)
        (let ((hb (p-find-overload b "%")))
          (if hb (p-call-overload hb b a t)
              (let ((na (to-number a)) (nb (to-number b)))
                (if (or (%pcl-nan-p na) (%pcl-nan-p nb)
                        (and (floatp na) (sb-ext:float-infinity-p na))
                        (and (floatp nb) (sb-ext:float-infinity-p nb))
                        (zerop nb))
                    (sb-kernel:make-double-float #x7FF80000 0)
                    (mod (truncate na) (truncate nb)))))))))

(defun p-** (a b)
  "Perl exponentiation with use overload '**' dispatch"
  ;; use overload "**": exponentiation overload
  (let ((ha (p-find-overload a "**")))
    (when ha (return-from p-** (p-call-overload ha a b nil)))
    (let ((hb (p-find-overload b "**")))
      (when hb (return-from p-** (p-call-overload hb b a t))))
    ;; No overload: existing numeric path with Inf-on-overflow
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

(defun p-rand (&optional max)
  "Perl rand - random number"
  (if max
      (* (random 1.0d0) (to-number max))
      (random 1.0d0)))

(defun p-srand (&optional seed)
  "Perl srand - seed random number generator"
  (declare (ignore seed))
  ;; CL doesn't have portable srand - just return a value
  1)

(defun to-number (val)
  "Convert value to number (Perl semantics).
   Uses lazy caching for boxed values."
  (if (p-box-p val)
      (box-nv val)
      ;; Raw value - convert directly
      (cond
        ((numberp val) val)
        ((eq val *p-undef*) 0)
        ((null val) 0)
        ;; CL's T from comparison operators - Perl true numifies to 1
        ((eq val t) 1)
        ((stringp val) (parse-perl-number val))
        ;; Adjustable vector = Perl @array in scalar context → array length
        ((and (vectorp val) (adjustable-array-p val)) (length val))
        ;; Perl 5.26+: plain %hash in numeric context → key count
        ((hash-table-p val) (%p-hash-user-count val))
        ;; Compiled regex in numeric context → object address (like a reference)
        ((p-regex-match-p val) (object-address val))
        (t 0))))

;;; ============================================================
;;; String Operators
;;; ============================================================

(defun p-. (a b)
  "Perl string concatenation operator (.) with use overload '.' dispatch."
  ;; use overload ".": check left operand then right (reversed)
  (let ((ha (p-find-overload a ".")))
    (if ha (p-call-overload ha a b nil)
        (let ((hb (p-find-overload b ".")))
          (if hb (p-call-overload hb b a t)
              (concatenate 'string (to-string a) (to-string b)))))))

(defun p-string-concat (&rest args)
  "Perl string concatenation for string interpolation (\"$a $b\").
   Does NOT dispatch the '.' overload — interpolation uses the '\"\"' overload
   automatically via to-string -> box-sv."
  (apply #'concatenate 'string (mapcar #'to-string args)))

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

(defun to-string (val)
  "Convert value to string (Perl semantics).
   Uses lazy caching for boxed values."
  (if (p-box-p val)
      (box-sv val)
      ;; Raw value - convert directly
      (stringify-value val)))

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
  "Build a Perl version string (v1.20.300) from integer code points.
   Each code point becomes a character in the resulting string."
  (coerce (mapcar (lambda (n)
                    (let ((c (truncate (if (typep n 'number) n (to-number n)))))
                      (if (or (< c 0) (> c #x10FFFF))
                          #\REPLACEMENT_CHARACTER
                          (code-char c))))
                  code-points)
          'string))

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
  (let ((s (to-string val)))
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
  (let ((fh *standard-output*)
        (fmt nil)
        (fmt-args nil))
    ;; Check for :fh keyword
    (when (and (>= (length args) 2) (eq (first args) :fh))
      (setf fh (p-get-stream (second args)))
      (setf args (cddr args)))
    ;; printf takes a LIST (FORMAT, LIST): flatten raw @array/%hash args so the
    ;; format comes from the first flattened element, e.g. `printf @a` where
    ;; @a = ("%d\n", 5).  A p-box-wrapped ref stays scalar (printf "%s", $aref).
    (setf args (coerce (p-flatten-args args) 'list))
    ;; First remaining arg is format, rest are format args
    (setf fmt (first args))
    (setf fmt-args (rest args))
    (let ((*p-sprintf-caller* "printf"))
      (princ (apply #'p-sprintf fmt fmt-args) fh))
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
             (proclaim '(special ,place))
             (setf (symbol-value ',place) (make-p-box nil)))
           (let ((,cur (p-box-value ,place)))
             (if (p-tie-proxy-p ,cur)
                 (p-method-call (p-tie-proxy-tie-obj ,cur) "STORE" ,val)
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
             (proclaim '(special ,place))
             (setf (symbol-value ',place) (make-p-box nil)))
           (box-set ,place ,val)
           ,place))))

(defmacro p-my-= (place value)
  "Assign to a lexically-bound 'my' variable. Unlike p-scalar-=, does not
   auto-declare the variable as special — the enclosing let binding (emitted by
   _with_declarations in Parser.pm) already handles scoping. This makes the
   assignment intent explicit for other compiler backends reading the IR."
  `(box-set ,place ,value))

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
          ;; Reference to a raw object (array-ref, hash-ref, code-ref, glob, qr//):
          ;; copy the scalar CONTAINER while keeping the SAME underlying object.
          ;; Perl's [$x] / @a=($x) copies the scalar; if $x is later reassigned
          ;; (box-set mutates the original box in place) the stored copy must not
          ;; follow it.  A fresh box around the same object is a distinct container
          ;; pointing at the same referent — and does not change the ref type
          ;; (still ARRAY/HASH/CODE/…), since the object itself is unchanged.
          ((or (and (vectorp inner) (not (stringp inner)))
               (hash-table-p inner)
               (functionp inner)
               (p-typeglob-p inner)
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

(defun p-array-fill (place value)
  "Clear adjustable array PLACE and refill it from VALUE: flatten nested vectors
   (but not strings), box elements, preserve nil holes.  Snapshots VALUE first so
   self-assignment (@a = @a) and embedding (@a = (1, @a, 2)) work.  Returns PLACE.
   Shared by the p-array-= macro and the closure-capture lexical array-init path
   (which cannot use p-array='s boundp/proclaim-special guard — that would make a
   let-bound lexical special and break the closure)."
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
                        do (cond
                             ((p-flatten-marker-p item)
                              (add-items (p-flatten-marker-array item)))
                             ((and (vectorp item) (not (stringp item)))
                              (add-items item))
                             ;; Preserve nil as deleted-element marker (not undef-but-exists)
                             ((null item)
                              (vector-push-extend nil place))
                             (t
                              (%p-array-store-scalar place item)))))
                 ((listp src)
                  (loop for item in src
                        do (cond
                             ((p-flatten-marker-p item)
                              (add-items (p-flatten-marker-array item)))
                             ((and (vectorp item) (not (stringp item)))
                              (add-items item))
                             ;; Preserve nil as deleted-element marker (not undef-but-exists)
                             ((null item)
                              (vector-push-extend nil place))
                             (t
                              (%p-array-store-scalar place item)))))
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
         (proclaim '(special ,place))
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
    (clrhash place)
    (loop for i from 0 below cnt by 2
          do (setf (gethash (to-string (aref flat i)) place)
                   (if (< (1+ i) cnt)
                       (%p-make-hash-entry (aref flat (1+ i)))
                       *p-undef*)))
    cnt))

(defmacro p-hash-= (place value)
  "Assign to a hash variable (%hash). Clears and repopulates from value.
   Returns: list ctx → flattened hash contents; scalar/void → input element count."
  (let ((val (gensym "VAL"))
        (cnt (gensym "CNT"))
        (ret (gensym "RET")))
    `(let ((,val ,value))
       (unless (boundp ',place)
         (proclaim '(special ,place))
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
                         (if (or (p-box-p inner)
                                 (p-box-class item)
                                 (and (vectorp inner) (not (stringp inner)))  ; array ref
                                 (hash-table-p inner)  ; hash ref
                                 (%p-dualvar-box-p item))  ; $!/dualvar: keep both halves
                             item   ; reference, blessed, or dualvar: preserve the box
                             inner))  ; plain scalar: snapshot value
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
             ;; True when v is any form that produces Perl undef used as a skip placeholder
             (or (eq v '*p-undef*)
                 (and (listp v)
                      (symbolp (car v))
                      (string= (symbol-name (car v)) "P-UNDEF"))
                 ;; (let ((*wantarray* t)) (p-undef)) wrapper emitted by wantarray ctx
                 (and (listp v)
                      (eq (car v) 'let)
                      (= (length v) 3)
                      (listp (third v))
                      (symbolp (car (third v)))
                      (string= (symbol-name (car (third v))) "P-UNDEF"))))
           (cur-idx ()
             ;; The current index as a CL literal or form.
             ;; When dynamic skips exist: (+ static-idx dyn1 dyn2 ...)
             (if (null dyn-vars)
                 static-idx
                 `(+ ,static-idx ,@(reverse dyn-vars))))
           (assign-scalar (lvar idx-expr)
             `(progn
                (unless (boundp ',lvar)
                  (proclaim '(special ,lvar))
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
                           (proclaim '(special ,var))
                           (setf (symbol-value ',var) (make-p-box nil)))
                         (box-set ,var *p-undef*))
                      forms)))
             ;; Collect: hash → maphash (empty after greedy), array → loop, scalar → undef
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
                (push `(vector-push-extend *p-undef* ,result-var) collect-forms))))

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

;; p-setf dispatches to the appropriate assignment form based on place type.
;; For element access (p-aref, p-gethash, etc.), uses CL's setf mechanism.
(defmacro p-setf (place value)
  "Perl assignment - dispatches to type-specific forms or uses CL setf for element access."
  (cond
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
            (proclaim '(special ,hash))
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
            (proclaim '(special ,arr))
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
       ;; Auto-declare array if needed (when arr is a simple symbol)
       (if (symbolp arr)
           `(progn
              (unless (boundp ',arr)
                (proclaim '(special ,arr))
                (setf (symbol-value ',arr) (make-array 0 :adjustable t :fill-pointer 0)))
              (let* ((,src ,value)
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
                ,src-vec))
           ;; Non-symbol array expression - just use it directly
           `(let* ((,src ,value)
                   (,src-vec (cond
                               ((listp ,src) (coerce ,src 'vector))
                               ((and (vectorp ,src) (not (stringp ,src))) ,src)
                               (t (vector ,src))))
                   (,indices (let ((idx-list nil))
                               (dolist (idx (list ,@indices-exprs) (nreverse idx-list))
                                 (cond
                                   ((listp idx)
                                    (dolist (i idx) (push i idx-list)))
                                   ((and (vectorp idx) (not (stringp idx)))
                                    (loop for i across idx do (push i idx-list)))
                                   (t (push idx idx-list)))))))
              (loop for i from 0 below (length ,indices)
                    for idx in ,indices
                    do (setf (p-aref ,arr idx)
                             (if (< i (length ,src-vec))
                                 (aref ,src-vec i)
                                 *p-undef*)))
              ,src-vec))))
    ;; Hash slice assignment: (p-setf (p-hslice hash keys...) values)
    ((and (listp place) (eq (car place) 'p-hslice))
     (let ((hash (cadr place))
           (keys-exprs (cddr place))
           (src (gensym "SRC"))
           (src-vec (gensym "SRC-VEC"))
           (keys (gensym "KEYS")))
       ;; Auto-declare hash if needed (when hash is a simple symbol)
       (if (symbolp hash)
           `(progn
              (unless (boundp ',hash)
                (proclaim '(special ,hash))
                (setf (symbol-value ',hash) (make-hash-table :test 'equal)))
              (let* ((,src ,value)
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
                ,src-vec))
           ;; Non-symbol hash expression
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
              ,src-vec))))
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

(defmacro p-incf (place &optional (delta 1))
  "Perl += - works on boxed values, hash/array elements, and derefs.
   Coerce the current value through to-number BEFORE adding: an absent key/slot
   reads as *p-undef*, which raw (+ …) cannot handle — Perl treats it as 0.
   PLACE is evaluated once (see %store-back-form)."
  (%store-back-form place (lambda (cur) `(+ (to-number ,cur) (to-number ,delta)))))

(defmacro p-decf (place &optional (delta 1))
  "Perl -= - works on boxed values, hash/array elements, and derefs.
   PLACE is evaluated once (see %store-back-form)."
  (%store-back-form place (lambda (cur) `(- (to-number ,cur) (to-number ,delta)))))

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

(defmacro p-*= (place value)
  "Perl *= (multiply-assign)"
  (%store-back-form place (lambda (cur) `(* (to-number ,cur) (to-number ,value)))))

(defmacro p-/= (place value)
  "Perl /= (divide-assign).  Delegate to p-/ so an exact CL ratio is coerced to
  a float (7/2 -> 3.5, not the leaked ratio \"7/2\") and overload '/' dispatches."
  (%store-back-form place (lambda (cur) `(p-/ ,cur ,value))))

(defmacro p-%= (place value)
  "Perl %= (modulo-assign)"
  (%store-back-form place
                    (lambda (cur) `(mod (truncate (to-number ,cur)) (truncate (to-number ,value))))))

(defmacro p-**= (place value)
  "Perl **= (exponent-assign).  Delegate to p-** so a negative exponent yields a
  float (2 ** -1 -> 0.5, not the leaked ratio \"1/2\") and overload '**' dispatches."
  (%store-back-form place (lambda (cur) `(p-** ,cur ,value))))

(defmacro p-.= (place value)
  "Perl .= (concat-assign)"
  (%store-back-form place
                    (lambda (cur) `(concatenate 'string (to-string ,cur) (to-string ,value)))))

(defmacro p-str-x= (place value)
  "Perl x= (repeat-assign)"
  (let ((n (gensym "N")))
    (%store-back-form place
                      (lambda (cur)
                        `(let ((,n (truncate (to-number ,value))))
                           (if (<= ,n 0) ""
                               (apply #'concatenate 'string
                                      (make-list ,n :initial-element (to-string ,cur)))))))))

(defmacro p-bit-and= (place value)
  "Perl &= (bitwise-and-assign)"
  (%store-back-form place (lambda (cur) `(p-bit-and ,cur ,value))))

(defmacro p-bit-or= (place value)
  "Perl |= (bitwise-or-assign)"
  (%store-back-form place (lambda (cur) `(p-bit-or ,cur ,value))))

(defmacro p-bit-xor= (place value)
  "Perl ^= (bitwise-xor-assign)"
  (%store-back-form place (lambda (cur) `(p-bit-xor ,cur ,value))))

(defmacro p-<<= (place value)
  "Perl <<= (left-shift-assign)"
  (%store-back-form place
                    (lambda (cur) `(ash (truncate (to-number ,cur)) (truncate (to-number ,value))))))

(defmacro p->>= (place value)
  "Perl >>= (right-shift-assign)"
  (%store-back-form place
                    (lambda (cur) `(ash (truncate (to-number ,cur)) (- (truncate (to-number ,value)))))))

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
(defun %pcl-nan-p (x)
  "True if x is a floating-point NaN."
  (and (floatp x) (sb-ext:float-nan-p x)))

(defun p-bool (x)
  "Map a CL boolean to a Perl boolean scalar: true → 1, false → \"\".
   Perl comparison operators return 1 for true and the empty string (which is
   *defined*) for false — NOT undef.  This matters for `defined(2==3)` (true)
   and `(2==3) // 4` (yields \"\", not 4)."
  (if x 1 ""))

(defmacro %def-overloaded-cmp (name op-str fallback-op cl-test nan-result)
  `(defun ,name (a b)
     ,(format nil "Perl ~A with use overload dispatch (returns 1 / \"\")" op-str)
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
                              (,cl-test na nb))))))))))))

(%def-overloaded-cmp p-==  "=="  "<=>"  =   nil)   ; NaN==NaN → false
(%def-overloaded-cmp p-!=  "!="  "<=>"  /=  t)     ; NaN!=NaN → true
(%def-overloaded-cmp p-<   "<"   "<=>"  <   nil)   ; NaN<x → false
(%def-overloaded-cmp p->   ">"   "<=>"  >   nil)   ; NaN>x → false
(%def-overloaded-cmp p-<=  "<="  "<=>"  <=  nil)   ; NaN<=x → false
(%def-overloaded-cmp p->=  ">="  "<=>"  >=  nil)   ; NaN>=x → false

(defun p-<=> (a b)
  "Perl spaceship operator with use overload '<=>' dispatch"
  ;; use overload "<=>": check left operand then right (reversed)
  (let ((ha (p-find-overload a "<=>")))
    (if ha (p-call-overload ha a b nil)
        (let ((hb (p-find-overload b "<=>")))
          (if hb (p-call-overload hb b a t)
              ;; IEEE 754: NaN comparisons always false → <=> returns undef
              (let ((na (to-number a)) (nb (to-number b)))
                (if (or (%pcl-nan-p na) (%pcl-nan-p nb))
                    *p-undef*
                    (cond ((< na nb) -1) ((> na nb) 1) (t 0)))))))))

;;; ============================================================
;;; Range Operator
;;; ============================================================

(defun p-.. (start end)
  "Perl range operator .. - returns a vector from start to end (inclusive).
   Works with numbers, single characters, and multi-character strings
   (magical string increment: 'aa'..'zz', 'A'..'ZZ', etc.)"
  (let ((s (unbox start))
        (e (unbox end)))
    (let* (;; Treat *p-undef* as undef (nil) for range logic
           (s-undef (or (null s) (eq s *p-undef*)))
           (e-undef (or (null e) (eq e *p-undef*)))
           ;; Is value a numeric-like thing for range? (number or non-zero-padded numeric string)
           ;; Allow surrounding whitespace: Perl numifies "-4\n" as -4 (strips whitespace).
           (s-num-p (or (numberp s)
                        (and (stringp s)
                             (let ((ts (string-trim '(#\Space #\Tab #\Newline #\Return) s)))
                               (and (not (and (> (length ts) 1) (char= (char ts 0) #\0)))
                                    (ppcre:scan "^[+-]?\\d+(\\.\\d+)?([Ee][+-]?\\d+)?$" ts))))))
           (e-num-p (or (numberp e)
                        (and (stringp e)
                             (let ((te (string-trim '(#\Space #\Tab #\Newline #\Return) e)))
                               (and (not (and (> (length te) 1) (char= (char te 0) #\0)))
                                    (ppcre:scan "^[+-]?\\d+(\\.\\d+)?([Ee][+-]?\\d+)?$" te))))))
           ;; Use string range when at least one side is a genuine non-numeric string,
           ;; or both are undef (undef..undef). Excludes undef+numeric (→ fallback numeric).
           (use-string-range
            (and (or s-undef (stringp s))
                 (or e-undef (stringp e))
                 (or (and (stringp s) (not s-num-p))    ; s is a non-numeric string
                     (and (stringp e) (not e-num-p))    ; e is a non-numeric string
                     (and s-undef e-undef)))))           ; undef..undef
      (cond
        ;; Numeric range: both operands are numeric (number or numeric string)
        ((and s-num-p e-num-p)
         (let ((ns (to-number s))
               (ne (to-number e)))
           ;; Inf/NaN endpoints: Perl dies "Range iterator outside integer range"
           (when (or (and (floatp ns) (or (%pcl-nan-p ns) (sb-ext:float-infinity-p ns)))
                     (and (floatp ne) (or (%pcl-nan-p ne) (sb-ext:float-infinity-p ne))))
             (p-die (make-p-box "Range iterator outside integer range") nil))
           (setf ns (truncate ns) ne (truncate ne))
           (when (> (- ne ns) 100000000)
             (error "Integer overflow in range (~A .. ~A): range too large" ns ne))
           (if (<= ns ne)
               (coerce (loop for i from ns to ne collect i) 'vector)
               (make-array 0))))
        ;; String range: undef→"", handle magical vs non-magical starts
        (use-string-range
         (let* ((sv (if s-undef "" s))
                (ev (if e-undef "" e)))
           (if (and (> (length sv) 0) (ppcre:scan "^[a-zA-Z0-9]+$" sv))
               ;; Magical string range (all alphanumeric start)
               (if (> (length sv) (length ev))
                   (make-array 0)
                   (let ((result (make-array 0 :adjustable t :fill-pointer 0))
                         (current (copy-seq sv))
                         (max-len (length ev)))
                     (loop
                      (vector-push-extend current result)
                      (when (string= current ev) (return))
                      (setf current (magical-string-increment current))
                      ;; If magical-string-increment returned a number, stop
                      (unless (stringp current) (return))
                      (when (> (length current) max-len) (return)))
                     result))
               ;; Non-magical or empty start: return (sv) if sv <= ev, else empty
               (if (string<= sv ev)
                   (vector sv)
                   (make-array 0)))))
        ;; Fallback: treat as numbers (handles undef+number, etc.)
        (t
         (let ((ns (truncate (to-number s)))
               (ne (truncate (to-number e))))
           (if (<= ns ne)
               (coerce (loop for i from ns to ne collect i) 'vector)
               (make-array 0))))))))

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
(defmacro %def-overloaded-str-cmp (name op-str str-test cmp-test)
  `(defun ,name (a b)
     ,(format nil "Perl ~A with use overload dispatch (returns 1 / \"\")" op-str)
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
                        (,str-test (to-string a) (to-string b))))))))))) ; t/nil → p-bool

(%def-overloaded-str-cmp p-str-eq  "eq"  string=   =)
(%def-overloaded-str-cmp p-str-ne  "ne"  string/=  /=)
(%def-overloaded-str-cmp p-str-lt  "lt"  string<   <)
(%def-overloaded-str-cmp p-str-gt  "gt"  string>   >)
(%def-overloaded-str-cmp p-str-le  "le"  string<=  <=)
(%def-overloaded-str-cmp p-str-ge  "ge"  string>=  >=)

(defun p-str-cmp (a b)
  "Perl string comparison (cmp) with use overload 'cmp' dispatch"
  ;; use overload "cmp": check left operand then right (reversed)
  (let ((ha (p-find-overload a "cmp")))
    (if ha (p-call-overload ha a b nil)
        (let ((hb (p-find-overload b "cmp")))
          (if hb (p-call-overload hb b a t)
              (let ((sa (to-string a)) (sb (to-string b)))
                (cond ((string< sa sb) -1) ((string> sa sb) 1) (t 0))))))))

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
        (if (or (and (vectorp v) (not (stringp v)))  ; arrayref
                (hash-table-p v)                      ; hashref
                (functionp v)                          ; coderef
                (p-box-p v)                            ; scalar ref (box-in-box)
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
        (t *p-undef*)))))

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
        (setf (p-aref (p-ensure-arrayref arr) idx) value))))
  (let* ((a (unbox arr))
         (i (truncate (to-number idx)))
         (len (if (vectorp a) (length a) 0))
         (actual-idx (if (< i 0) (+ len i) i)))
    (when (and (vectorp a) (>= actual-idx 0))
      ;; Auto-extend array if needed (Perl autovivification)
      ;; Intermediate slots get nil (deleted marker) so exists returns false for them.
      (when (>= actual-idx len)
        (dotimes (n (1+ (- actual-idx len)))
          (vector-push-extend nil a)))
      ;; Get or create box at this index
      (let ((box (aref a actual-idx)))
        (unless (p-box-p box)
          (setf box (make-p-box nil))
          (setf (aref a actual-idx) box))
        ;; Set the box's value and return the box
        (box-set box value)))))

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
        (when (>= actual-idx len)
          (dotimes (n (1+ (- actual-idx len)))
            (vector-push-extend nil a)))
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
   - Growing: extends with undef-boxed elements
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
       ;; Grow: extend with undef boxes
       (dotimes (i (- new-len cur-len))
         (vector-push-extend (make-p-box *p-undef*) a)))
      ((< new-len cur-len)
       ;; Shrink: adjust fill-pointer (minimum 0)
       (setf (fill-pointer a) (max 0 new-len))))
    nli))

(defmacro p-push (arr &rest items)
  "Perl push - adds to end of array, auto-declares if needed"
  (if (symbolp arr)
      ;; Simple array variable: ensure declared
      `(progn
         (unless (boundp ',arr)
           (proclaim '(special ,arr))
           (setf (symbol-value ',arr) (make-array 0 :adjustable t :fill-pointer 0)))
         (p-push-impl ,arr ,@items))
      ;; Complex place
      `(p-push-impl ,arr ,@items)))

(defun p-flatten-args (args)
  "Build @_ from %_args, spreading raw (non-string, non-boxed) vectors and hash-tables.
   This implements Perl's argument flattening: foo(@arr) and foo(%hash) spread their
   elements as individual arguments."
  (let ((result (make-array (length args) :adjustable t :fill-pointer 0)))
    (dolist (arg args)
      (cond
        ((and (vectorp arg) (not (stringp arg)))
         ;; Raw vector = array passed in list context: spread its elements
         (loop for elem across arg do (vector-push-extend elem result)))
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

(defun p-check-arity (funcname got min max flexible)
  "Perl subroutine-signature arity check.  Throws a Perl-formatted
   'Too few/many arguments for subroutine ...' error when GOT is outside
   [MIN, MAX].  MAX = nil means no upper bound (a slurpy @/% param).  FLEXIBLE
   non-nil selects the 'at least'/'at most' wording Perl uses when the sub has
   optional or slurpy params (a fixed-arity sub uses the bare count)."
  (cond
    ((< got min)
     (error "Too few arguments for subroutine '~A' (got ~D; expected ~A~D)"
            funcname got (if flexible "at least " "") min))
    ((and max (> got max))
     (error "Too many arguments for subroutine '~A' (got ~D; expected ~A~D)"
            funcname got (if flexible "at most " "") max))))

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

(defun p-push-impl (arr &rest items)
  "Implementation of push - stores values in boxes for l-value semantics.
   Recognizes p-flatten-marker to flatten @array arguments.
   Also spreads raw CL vectors (e.g. from qw!...! or list-context expressions)."
  ;; push's first arg must be a real array.  Without this guard, pushing onto a
  ;; non-array (a literal, or a scalar/ref) reaches %p-array-store-scalar / length
  ;; and leaks a raw CL type error (a Lisp struct dump) into $@.  Perl dies
  ;; "...must be array" for a literal and "Experimental push on scalar is now
  ;; forbidden" for the removed experimental autoderef on a scalar/ref.
  (unless (and (vectorp arr) (not (stringp arr)))
    (if (p-box-p arr)
        (error "Experimental push on scalar is now forbidden")
        (error "Type of arg 1 to push must be array (not constant item)")))
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
  (if (and (vectorp arr) (> (length arr) 0))
      (vector-pop arr)
      *p-undef*))

(defun p-shift (arr)
  "Perl shift - removes from front, returns the element as-is (preserving references).
   Like p-aref, does NOT unbox: box-set handles plain vs reference boxes correctly."
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
           (proclaim '(special ,arr))
           (setf (symbol-value ',arr) (make-array 0 :adjustable t :fill-pointer 0)))
         (p-splice-impl ,arr ,@args))
      `(p-splice-impl ,arr ,@args)))

(defun p-splice-impl (arr &optional (offset 0) (length nil length-p) &rest replacements)
  "Perl splice: remove and/or replace elements in an array.
   Returns removed elements as a vector."
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
      (t
       (multiple-value-bind (val found) (gethash k h)
         (if (not found)
             *p-undef*
             (%p-hash-unbox-elem val)))))))

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
      (t
       ;; Get or create box at this key
       (multiple-value-bind (existing found) (gethash k h)
         (let ((box (if (and found (p-box-p existing))
                        existing
                        (make-p-box nil))))
           (unless (and found (p-box-p existing))
             (setf (gethash k h) box))
           ;; Set the box's value and return the box
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
      ;; Symbolic reference: string used as hash name (no strict refs)
      ((stringp h)
       (when (find #\Nul h) (return-from p-ensure-hashref
                              (make-hash-table :test 'equal)))
       (let* ((pos (search "::" h :from-end t))
              (pkg-str (if pos (%pcl-invert-case (subseq h 0 pos)) nil))
              (var-str (if pos (subseq h (+ pos 2)) h))
              (pkg     (if pkg-str
                           (or (find-package pkg-str)
                               (make-package pkg-str :use '(:cl :pcl)))
                           *package*))
              (sym-name (concatenate 'string "%" (%pcl-invert-case var-str)))
              (sym      (or (find-symbol sym-name pkg) (intern sym-name pkg))))
         (proclaim `(special ,sym))
         (unless (and (boundp sym) (hash-table-p (symbol-value sym)))
           (setf (symbol-value sym) (make-hash-table :test 'equal)))
         (symbol-value sym)))
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
      ;; Symbolic reference: string used as array name
      ((stringp a)
       ;; Null bytes can't be CL symbols — return a transient dummy array
       (when (find #\Nul a)
         (return-from p-ensure-arrayref
           (make-array 0 :adjustable t :fill-pointer 0)))
       (let* ((pos (search "::" a :from-end t))
              (pkg-str (if pos (%pcl-invert-case (subseq a 0 pos)) nil))
              (var-str (if pos (subseq a (+ pos 2)) a))
              (pkg     (if pkg-str
                           (or (find-package pkg-str)
                               (make-package pkg-str :use '(:cl :pcl)))
                           *package*))
              (sym-name (concatenate 'string "@" (%pcl-invert-case var-str)))
              (sym      (or (find-symbol sym-name pkg) (intern sym-name pkg))))
         (proclaim `(special ,sym))
         (unless (and (boundp sym) (vectorp (symbol-value sym)))
           (setf (symbol-value sym)
                 (make-array 0 :adjustable t :fill-pointer 0)))
         (symbol-value sym)))
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
    (when (>= i (length a))
      (loop for j from (length a) to i
            do (vector-push-extend nil a)))
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
    (when (>= i (length a))
      (loop for j from (length a) to i
            do (vector-push-extend nil a)))
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
    (when (>= i (length a))
      (loop for j from (length a) to i
            do (vector-push-extend nil a)))
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

(defun p-aslice (arr &rest indices)
  "Perl array slice @arr[indices] - returns vector of values.
   Handles individual indices, lists, and vectors (from range operator)."
  (let ((flat-indices (loop for idx in indices
                            if (vectorp idx)
                            append (coerce idx 'list)
                            else if (and (listp idx) (not (null idx)))
                            append idx
                            else
                            collect idx))
        (result (make-array 0 :adjustable t :fill-pointer 0)))
    (dolist (idx flat-indices result)
      (vector-push-extend (p-aref arr idx) result))))

(defun p-hslice (hash &rest keys)
  "Perl hash slice @hash{keys} - returns vector of values.
   Handles individual keys, lists, and vectors (from range operator).
   Strings are vectors in CL but must not be expanded into characters."
  (let ((flat-keys (loop for key in keys
                         if (and (vectorp key) (not (stringp key)))
                         append (coerce key 'list)
                         else if (and (listp key) (not (null key)))
                         append key
                         else
                         collect key))
        (result (make-array 0 :adjustable t :fill-pointer 0)))
    (dolist (key flat-keys result)
      (vector-push-extend (p-gethash hash key) result))))

(defun p-kv-hslice (hash &rest keys)
  "Perl KV hash slice %hash{keys} - returns vector of key-value pairs.
   Handles individual keys, lists, and vectors (from range operator).
   Strings are vectors in CL but must not be expanded into characters."
  (let ((flat-keys (loop for key in keys
                         if (and (vectorp key) (not (stringp key)))
                         append (coerce key 'list)
                         else if (and (listp key) (not (null key)))
                         append key
                         else
                         collect key))
        (result (make-array 0 :adjustable t :fill-pointer 0)))
    (dolist (key flat-keys result)
      (let ((k (to-string key)))
        (vector-push-extend k result)
        (vector-push-extend (p-gethash hash k) result)))))

(defun p-kv-aslice (arr &rest indices)
  "Perl KV array slice %arr[indices] - returns vector of (index, value) pairs.
   Handles individual indices, lists, and vectors (e.g. from range operator).
   Repeated indices yield repeated pairs, matching Perl semantics."
  (let ((flat-indices (loop for idx in indices
                            if (and (vectorp idx) (not (stringp idx)))
                            append (coerce idx 'list)
                            else if (and (listp idx) (not (null idx)))
                            append idx
                            else
                            collect idx))
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
    ;; Neither
    (t (make-array 0 :adjustable t :fill-pointer 0))))

(defun p-exists (hash key)
  "Perl exists function"
  (let ((h (unbox hash))
        (k (to-string key)))
    (cond
      ((eq h '%ENV-MARKER%) (not (null (sb-posix:getenv k))))
      ((eq h '%INC-MARKER%) (nth-value 1 (gethash k *p-inc-table*)))
      ;; Non-hash container (e.g. undef intermediate in `exists $h{a}{b}` where
      ;; $h{a} doesn't exist): Perl autovivifies $h{a} and the result is false.
      ;; We don't autovivify the intermediate, but must return false, not crash
      ;; gethash on a non-hash-table.
      ((hash-table-p h) (nth-value 1 (gethash k h)))
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
  (let ((h (unbox hash))
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
  (let* ((a (unbox arr))
         (i (truncate (to-number idx)))
         (len (if (vectorp a) (length a) 0))
         (actual-idx (if (< i 0) (+ len i) i))
         (old-val (if (and (>= actual-idx 0) (< actual-idx len))
                      (p-aref-unbox-elem (aref a actual-idx))
                      *p-undef*)))
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
  (let* ((a (unbox arr))
         (i (truncate (to-number idx)))
         (len (if (vectorp a) (length a) 0))
         (actual-idx (if (< i 0) (+ len i) i)))
    (and (vectorp a) (>= actual-idx 0) (< actual-idx len)
         (p-box-p (aref a actual-idx)))))

(defun p-delete-hash-slice (hash &rest keys)
  "Perl delete for hash slices: delete @hash{k1, k2, ...}
   Handles hash references (unboxes) and vector/list key arguments.
   Empty slice returns nil (undef) per [perl #29127]."
  (let* ((h (unbox hash))
         (flat-keys (loop for key in keys
                          if (and (vectorp key) (not (stringp key)))
                          append (coerce key 'list)
                          else if (and (listp key) (not (null key)))
                          append key
                          else
                          collect key)))
    (when (null flat-keys) (return-from p-delete-hash-slice nil))
    (let ((result (make-array (length flat-keys) :adjustable t :fill-pointer 0)))
      (dolist (key flat-keys)
        (let ((k (to-string key)))
          (vector-push-extend (gethash k h *p-undef*) result)
          (remhash k h)))
      result)))

(defun p-delete-kv-hash-slice (hash &rest keys)
  "Perl delete for KV hash slices: delete %hash{k1, k2, ...}
   Handles hash references (unboxes) and vector/list key arguments."
  (let* ((h (unbox hash))
         (flat-keys (loop for key in keys
                          if (and (vectorp key) (not (stringp key)))
                          append (coerce key 'list)
                          else if (and (listp key) (not (null key)))
                          append key
                          else
                          collect key))
         (result (make-array 0 :adjustable t :fill-pointer 0)))
    (dolist (key flat-keys)
      (let ((k (to-string key)))
        (vector-push-extend k result)
        (vector-push-extend (gethash k h *p-undef*) result)
        (remhash k h)))
    result))

(defun p-delete-array-slice (arr &rest indices)
  "Perl delete for array slices: delete @arr[i1, i2, ...]
   Sets elements to nil (deleted marker), trims trailing nils, and returns old values."
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
    ;; Trim trailing nil slots (Perl semantics: array shrinks when last elements deleted)
    (when (vectorp a)
      (loop while (and (> (fill-pointer a) 0)
                       (null (aref a (1- (fill-pointer a)))))
            do (decf (fill-pointer a))))
    result))

(defun p-delete-kv-array-slice (arr &rest indices)
  "Perl delete for KV array slices: delete %arr[i1, i2, ...]
   Deletes elements at given indices and returns key-value pairs (index, value, ...)."
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
    ;; Trim trailing nil slots (Perl semantics: array shrinks when last elements deleted)
    (when (vectorp a)
      (loop while (and (> (fill-pointer a) 0)
                       (null (aref a (1- (fill-pointer a)))))
            do (decf (fill-pointer a))))
    result))

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

;;; Helper: extract :label and :continue from loop body-and-keys.
;;; Returns (values label continue-form body).
(defun parse-loop-keys (body-and-keys)
  (let* ((label (when (eq (first body-and-keys) :label)
                  (second body-and-keys)))
         (rest (if label (cddr body-and-keys) body-and-keys))
         (continue-form nil)
         (body rest)
         (pos (position :continue rest)))
    (when pos
      (setf continue-form (nth (1+ pos) rest))
      (setf body (subseq rest 0 pos)))
    (values label continue-form body)))

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
                      (loop for x across src do (vector-push-extend x result)))))
                 ((and (not (p-box-p item)) (vectorp item) (not (stringp item)))
                  ;; Raw CL vector from function return (keys, grep, etc.) — spread
                  (loop for x across item do (vector-push-extend x result)))
                 (t
                  (vector-push-extend item result))))
         result)))))

(defmacro p-foreach ((var list) &rest body-and-keys)
  "Perl foreach loop with optional :label and :continue.
Uses tagbody/go instead of loop -- see p-while for rationale."
  (multiple-value-bind (label continue-form body) (parse-loop-keys body-and-keys)
    (let ((block-name (or label (gensym "FOREACH")))
          (last-tag (when label (%pcl-loop-tag "LAST" label)))
          (vec (gensym))
          (raw (gensym))
          (i (gensym)))
      `(block ,block-name
         (let* ((,raw (let ((*wantarray* t)) ,list))  ; list in list-context; body keeps outer context
                (,vec (%p-flatten-for-list ,raw))
                (,i 0))
           ,(let ((inner `(block nil    ; for unlabeled p-last
                            (tagbody
                             :next
                               (when (>= ,i (length ,vec)) (return-from ,block-name ""))
                               (let ((,var (ensure-boxed (aref ,vec ,i))))
                                 (incf ,i)
                                 ,(make-loop-iteration-body label body)
                                 ,@(when continue-form (list continue-form)))
                               (go :next)))))
              (if label
                  `(catch ',last-tag ,inner)
                  inner)))))))

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

(defun p-goto-computed (label)
  "Perl goto EXPR (computed goto) — not implementable in CL; silently ignore."
  (declare (ignore label))
  nil)

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
   choose heavy_import vs heavy_export.  Only then pop and apply."
  (let ((target (gensym "GOTO-TARGET")))
    `(let ((,target ,fn))
       (throw :p-return
         (let ((*pcl-current-package*
                (if *pcl-caller-pkg-stack*
                    (car *pcl-caller-pkg-stack*) *pcl-current-package*))
               (*pcl-caller-pkg-stack*
                (if *pcl-caller-pkg-stack*
                    (cdr *pcl-caller-pkg-stack*) *pcl-caller-pkg-stack*))
               (*pcl-caller-subname-stack*
                (if *pcl-caller-subname-stack*
                    (cdr *pcl-caller-subname-stack*) *pcl-caller-subname-stack*)))
           (apply ,target (coerce @_ 'list)))))))

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

(defun p-print (&rest args)
  "Perl print - prints args then appends $\\ (output record separator)"
  (let ((fh *standard-output*))
    ;; Check for :fh keyword
    (when (and (>= (length args) 2) (eq (first args) :fh))
      (let ((desig (second args)))
        (setf args (cddr args))
        (cond
          ;; :fh nil = bare `print { }`/empty filehandle node → the default
          ;; (currently-selected) output handle, i.e. STDOUT.
          ((null desig) (setf fh *standard-output*))
          ;; A named/lexical handle that resolves to a real stream.
          ((p-get-stream desig) (setf fh (p-get-stream desig)))
          ;; A handle was named but is not open: Perl print/say fails with
          ;; errno EBADF, prints nothing, and returns false.
          (t (setf *p-stored-errno* 9)                            ; EBADF (Linux)
             (setf (sb-alien:extern-alien "errno" sb-alien:int) 9)
             (return-from p-print "")))))
    ;; No list to print (bare `print;` / `print FH;` / `say;`) defaults to $_,
    ;; like the named-unary $_-default family (uc/length/…).
    (when (null args)
      (setf args (list $_)))
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
    t))

(defun p-say (&rest args)
  "Perl say (print with newline)"
  (let ((fh *standard-output*))
    ;; Resolve the target handle once (same rules as p-print); bail with EBADF
    ;; on a named-but-unopened handle so we don't emit a stray newline.
    (when (and (>= (length args) 2) (eq (first args) :fh))
      (let ((desig (second args)))
        (cond
          ((null desig) (setf fh *standard-output*))
          ((p-get-stream desig) (setf fh (p-get-stream desig)))
          (t (setf *p-stored-errno* 9)                            ; EBADF (Linux)
             (setf (sb-alien:extern-alien "errno" sb-alien:int) 9)
             (return-from p-say "")))))
    (apply #'p-print args)
    (terpri fh)
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
            ;; No location marker: exact legacy behavior.
            ((null loc) (error msg))
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
                     ;; I/O error opening/reading (e.g. is-a-directory, permissions):
                     ;; errno already set by OS; clear $@, return undef
                     (stream-error (e)
                       (declare (ignore e))
                       (return-from p-do
                         (progn (box-set $@ (make-p-box "")) *p-undef*)))
                     (file-error (e)
                       (declare (ignore e))
                       (return-from p-do
                         (progn (box-set $@ (make-p-box "")) *p-undef*))))))
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
     - otherwise a fresh undef container (Perl auto-vivifies the global as undef)."
  (let ((cell (assoc name *p-eval-lex-alist* :test #'string=)))
    (cond
      (cell (cdr cell))
      (t (let ((sym (intern (%pcl-invert-case name) *package*))
               (sigil (char name 0)))
           (cond
             ((boundp sym) (symbol-value sym))
             ((char= sigil #\@) (make-array 0 :adjustable t :fill-pointer 0))
             ((char= sigil #\%) (make-hash-table :test 'equal))
             (t (make-p-box nil))))))))

(defun p-eval-thunk (free-names fn)
  "Apply FN (the lambda wrapping a string-eval body) to the containers for its
   free variables FREE-NAMES, looked up via p-eval-lex-lookup.  The lambda's
   parameters are those same variables, so the eval body — and any closure it
   builds — references them as ordinary lexicals."
  (apply fn (mapcar #'p-eval-lex-lookup free-names)))

(defun p-eval (string &optional lex-alist)
  "Perl eval(STRING): transpile and evaluate a Perl string at runtime.
   LEX-ALIST carries the caller's in-scope lexicals (name . container) so the
   eval body can capture them (see p-eval-thunk).
   Binds *pcl-caller-wantarray* so wantarray() in the eval'd code reflects context."
  (let ((*pcl-caller-wantarray* *wantarray*)
        (*p-eval-lex-alist* lex-alist)
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
    (let* ((pkg-name  *pcl-current-package*)
           (cache-key (cons s pkg-name))
           (cached    (gethash cache-key *p-eval-string-cache*)))
      (handler-case
          (let* ((cl-text  (or cached
                               (let ((r (p-transpile-string s pkg-name)))
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
       (prog1 (catch :p-return ,@body)
         (box-set $@ ""))
     (p-exception (e)
       ;; Object exception - preserve the object in $@
       (box-set $@ (p-exception-object e))
       nil)
     (error (e)
       ;; String exception - convert to string.
       ;; Perl appends " at SCRIPT line N.\n" when message doesn't end with \n.
       (let ((msg (format nil "~A" e)))
         (box-set $@ (if (and (> (length msg) 0)
                              (char= (char msg (1- (length msg))) #\Newline))
                         msg
                         (format nil "~A at (eval 0) line 0.~%" msg))))
       nil)))

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
            (sym (find-symbol (%pcl-invert-case name) :pcl)))
       (and sym (gethash sym *p-filehandles*))))
    ((p-box-p fh)
     (let ((v (p-box-value fh)))
       (cond
         ((streamp v) v)
         ((%p-socket-p v) v)
         ;; Scalar holding a handle NAME ('STDOUT', 'FOO'): resolve by name.
         ((stringp v) (%p-resolve-fh v))
         (t nil))))
    (t nil)))

(defun p-get-stream (fh)
  "Get CL stream from Perl filehandle (symbol, box, stream, or socket object).
   A socket handle is lazily wrapped in a cached bidirectional stream so that
   print/readline/read/eof/close all work through the normal stream paths."
  (%p-as-stream (%p-resolve-fh fh)))

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

(defun %p-install-fh (fh stream)
  "Bind STREAM to the filehandle FH (a box, or a bareword symbol)."
  (cond ((p-box-p fh) (box-set fh stream))
        (t            (setf (gethash fh *p-filehandles*) stream))))

(defun %p-fresh-adjustable-string (&optional (init ""))
  (let ((buf (make-array (length init) :element-type 'character
                         :adjustable t :fill-pointer 0)))
    (loop for c across init do (vector-push-extend c buf))
    buf))

(defun %p-open-memory (fh mode target-box)
  "Open an in-memory string filehandle over TARGET-BOX (the scalar behind \\$s)."
  (let ((mode-str (to-string mode))
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

(defun %p-open-impl (fh mode filename)
  "Implementation of Perl open"
  ;; In-memory filehandle: the target is a SCALAR ref (a box whose value is a box),
  ;; e.g. open my $fh, '>', \$s.  Dispatch before the filename is stringified.
  (when (and (p-box-p filename) (p-box-p (p-box-value filename)))
    (return-from %p-open-impl
      (%p-open-memory fh mode (p-box-value filename))))
  (let* ((mode-str (to-string mode))
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
             (open file-str :direction :input :if-does-not-exist nil))
            ((string= mode-str ">")
             (open file-str :direction :output :if-exists :supersede
                   :if-does-not-exist :create))
            ((string= mode-str ">>")
             (open file-str :direction :output :if-exists :append
                   :if-does-not-exist :create))
            ((string= mode-str "+<")
             (open file-str :direction :io :if-exists :overwrite
                   :if-does-not-exist nil))
            ((string= mode-str "+>")
             (open file-str :direction :io :if-exists :supersede
                   :if-does-not-exist :create))
            ((or (string= mode-str "|-") (string= mode-str "-|"))
             ;; Pipe - not fully implemented, return nil
             (warn "Pipe open not implemented: ~A" mode-str)
             nil)
            (t
             (warn "Unknown open mode: ~A" mode-str)
             nil))))
    (if stream
        (progn
          (cond
            ;; Symbolic filehandle: open($fh, ...) where $fh already holds a
            ;; defined handle-NAME string (e.g. $TST = "TST"; open($TST, ...)).
            ;; Perl opens the named glob (*TST) and leaves $fh holding the
            ;; string — it does NOT autovivify a lexical handle into $fh.
            ;; Register under the by-name :pcl symbol (invert-cased, package
            ;; qualifier stripped) so BOTH the bareword form (<TST>/eof(TST))
            ;; and the scalar form (<$TST>) resolve it via p-get-stream.
            ;; (An undef/empty box is the modern `open my $fh, ...` autoviv.)
            ((and (p-box-p fh)
                  (stringp (p-box-value fh))
                  (plusp (length (p-box-value fh))))
             (let* ((nm  (p-box-value fh))
                    (sep (search "::" nm :from-end t))
                    (name (if sep (subseq nm (+ sep 2)) nm)))
               (setf (gethash (intern (%pcl-invert-case name) :pcl)
                              *p-filehandles*)
                     stream)))
            ((p-box-p fh) (box-set fh stream))
            (t             (setf (gethash fh *p-filehandles*) stream))))
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

(defun %p-close-impl (fh)
  "Implementation of Perl close (file or socket handle)."
  (cond
    ((p-box-p fh)
     (let ((v (p-box-value fh)))
       (cond
         ((%p-socket-p v) (%p-close-socket v) (box-set fh *p-undef*) t)
         ((streamp v)     (close v)           (box-set fh *p-undef*) t)
         (t nil))))
    (t
     (let ((v (%p-resolve-fh fh)))
       (cond
         ((%p-socket-p v) (%p-close-socket v)
          (when (symbolp fh) (remhash fh *p-filehandles*)) t)
         ((streamp v)     (close v)
          (when (symbolp fh) (remhash fh *p-filehandles*)) t)
         (t nil))))))

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
    (if stream
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
    ;; (let (BINDINGS) (pl-NAME)) — wantarray-wrapped bareword FH.
    ;; Sessions 162+ wrap scalar-context user sub calls in (let ((*wantarray* V)) ...).
    ;; Unwrap the let and extract the bare filehandle name.
    ((and (listp fh-form)
          (= (length fh-form) 3)
          (eq (car fh-form) 'let)
          (let ((body (caddr fh-form)))
            (and (listp body)
                 (= (length body) 1)
                 (symbolp (car body))
                 (> (length (symbol-name (car body))) 3)
                 (string-equal (subseq (symbol-name (car body)) 0 3) "PL-"))))
     `',(intern (%pcl-invert-case
                 (subseq (%pcl-invert-case (symbol-name (car (caddr fh-form)))) 3))))
    ;; Everything else: evaluate as-is (e.g. $fh variable or complex expression)
    (t fh-form)))

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
  (handler-case
      (let* ((stream (p-get-stream fh))
             (n (max 0 (truncate (to-number len))))
             (tmp (make-string n))
             (got (read-sequence tmp stream))
             (data (subseq tmp 0 got)))
        (when (p-box-p buf)
          (if offset
              (let* ((old (to-string (unbox buf)))
                     (off (truncate (to-number offset)))
                     (off (if (< off 0) (max 0 (+ (length old) off)) off))
                     (head (if (<= off (length old))
                               (subseq old 0 off)
                               (concatenate 'string old
                                            (make-string (- off (length old))
                                                         :initial-element #\Nul)))))
                (box-set buf (concatenate 'string head data)))
              (box-set buf data)))
        got)
    (error () *p-undef*)))

(defmacro p-read (fh &rest args)
  "Perl read — bareword filehandle is auto-quoted."
  `(%p-read-impl (%p-fh-arg ,fh) ,@args))

(defun %p-sysread-impl (fh buf len)
  "Perl sysread - low-level read (same as read for now). Returns nil on error."
  (handler-case (%p-read-impl fh buf len)
    (error () nil)))

(defmacro p-sysread (fh &rest args)
  "Perl sysread — bareword filehandle is auto-quoted."
  `(%p-sysread-impl (%p-fh-arg ,fh) ,@args))

(defun %p-syswrite-impl (fh data &optional len)
  "Perl syswrite - write data to filehandle. Unbuffered (flushes immediately) so a
   readline on the other end of a pipe sees the data. Returns nil on stream/encode error."
  (handler-case
      (let ((stream (p-get-stream fh))
            (str (to-string data)))
        (when stream
          (let ((out (if len
                         (subseq str 0 (min (to-number len) (length str)))
                         str)))
            (write-string out stream)
            (finish-output stream)
            (length out))))
    (error () nil)))

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

(defun p-getprotobyname (name)
  "Perl getprotobyname(NAME): the protocol number.  Real code only ever asks for
   tcp/udp/icmp; a tiny static table suffices."
  (let ((n (string-downcase (to-string name))))
    (cond ((string= n "tcp") 6)
          ((string= n "udp") 17)
          ((string= n "icmp") 1)
          ((string= n "ip") 0)
          (t *p-undef*))))

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

(defun %p-stat-arg (file-or-fh)
  "Resolve a stat/lstat argument to either a CL stream (filehandle) or a path
   string.  Accepts a stream, a box holding a stream, a bareword/symbol or
   string naming a filehandle, or a plain path string."
  (let ((v (if (p-box-p file-or-fh) (p-box-value file-or-fh) file-or-fh)))
    (cond
      ((streamp v) v)
      ((or (symbolp v) (stringp v))
       (or (p-get-stream v) (to-string v)))  ; FH name → stream, else path
      (t (to-string v)))))

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
  (let* ((path (to-string (unbox file)))
         (exists (or (probe-file path)
                     ;; probe-file may fail on directories in some implementations
                     (ignore-errors
                       (sb-posix:stat path)
                       t))))
    (if exists 1 nil)))

(defun p--d (file)
  "Perl -d: test if file is a directory"
  (handler-case
      (let ((stat (sb-posix:stat (to-string (unbox file)))))
        (if (sb-posix:s-isdir (sb-posix:stat-mode stat))
            1
            nil))
    (error () nil)))

(defun p--f (file)
  "Perl -f: test if file is a regular file"
  (handler-case
      (let ((stat (sb-posix:stat (to-string (unbox file)))))
        (if (sb-posix:s-isreg (sb-posix:stat-mode stat))
            1
            nil))
    (error () nil)))

(defun p--r (file)
  "Perl -r: test if file is readable"
  (let ((path (to-string (unbox file))))
    (handler-case
        (progn
          (sb-posix:access path sb-posix:r-ok)
          1)
      (error () nil))))

(defun p--w (file)
  "Perl -w: test if file is writable"
  (let ((path (to-string (unbox file))))
    (handler-case
        (progn
          (sb-posix:access path sb-posix:w-ok)
          1)
      (error () nil))))

(defun p--x (file)
  "Perl -x: test if file is executable"
  (let ((path (to-string (unbox file))))
    (handler-case
        (progn
          (sb-posix:access path sb-posix:x-ok)
          1)
      (error () nil))))

(defun p--s (file)
  "Perl -s: return file size if non-zero, nil otherwise"
  (handler-case
      (let* ((stat (sb-posix:stat (to-string (unbox file))))
             (size (sb-posix:stat-size stat)))
        (if (> size 0) size nil))
    (error () nil)))

(defun p--z (file)
  "Perl -z: test if file has zero size"
  (handler-case
      (let* ((stat (sb-posix:stat (to-string (unbox file))))
             (size (sb-posix:stat-size stat)))
        (if (= size 0) 1 nil))
    (error () nil)))

(defun p-unlink (&rest files)
  "Perl unlink - delete files. Returns count of files deleted."
  (let ((count 0))
    (dolist (f files)
      (let ((path (to-string (unbox f))))
        (when (and (probe-file path) (delete-file path))
          (incf count))))
    count))

(defun %p-fileno-impl (fh)
  "Perl fileno - get file descriptor number"
  (let ((stream (p-get-stream fh)))
    (cond
      ((eq stream *standard-input*) 0)
      ((eq stream *standard-output*) 1)
      ((eq stream *error-output*) 2)
      (t -1))))  ; CL doesn't expose fd numbers portably

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

(defun %p-opendir-impl (dh dir)
  "Perl opendir - open directory for reading"
  (let ((dir-str (to-string dir)))
    (when (probe-file dir-str)
      (let ((entries (directory (merge-pathnames "*.*" dir-str))))
        (if (symbolp dh)
            (setf (gethash dh *p-dirhandles*)
                  (cons 0 (mapcar #'file-namestring entries)))
            (when (p-box-p dh)
              (setf (p-box-value dh)
                    (cons 0 (mapcar #'file-namestring entries)))))
        t))))

(defmacro p-opendir (dh &rest args)
  "Perl opendir — bareword dirhandle is auto-quoted."
  `(%p-opendir-impl (%p-fh-arg ,dh) ,@args))

(defun %p-readdir-impl (dh)
  "Perl readdir - read next directory entry"
  (let ((handle (if (symbolp dh)
                    (gethash dh *p-dirhandles*)
                    (when (p-box-p dh) (p-box-value dh)))))
    (when handle
      (let ((idx (car handle))
            (entries (cdr handle)))
        (if (< idx (length entries))
            (progn
              (setf (car handle) (1+ idx))
              (nth idx entries))
            nil)))))

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

(defun p-glob--expand (pat)
  "Expand glob pattern PAT and return a vector of matching filenames."
  (let* ((expanded-pat (expand-glob-char-ranges pat))
         (is-relative (not (and (> (length expanded-pat) 0) (char= (char expanded-pat 0) #\/))))
         (full-pat (if is-relative
                       (concatenate 'string (sb-posix:getcwd) "/" expanded-pat)
                       expanded-pat))
         (dir-prefix (let ((slash-pos (position #\/ pat :from-end t)))
                       (if slash-pos (subseq pat 0 (1+ slash-pos)) "")))
         (all-matches (handler-case (directory (parse-namestring full-pat)) (error () nil)))
         (matches (remove-if (lambda (p) (null (pathname-name p))) all-matches))
         (result (make-array (length matches) :fill-pointer 0)))
    (dolist (path matches result)
      (vector-push (concatenate 'string dir-prefix (file-namestring path)) result))))

(defun p-glob (&optional pattern)
  "Perl glob / <*.txt> - expand file glob pattern.
   In list context: first call returns all matches; second call with same pattern returns empty.
   In scalar context: returns one match per call, nil when exhausted; resets for next cycle."
  (let ((pat (if pattern (to-string pattern) "*")))
    (if (eq *wantarray* t)
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
  "Perl mkdir - create directory. Returns true on success."
  (let ((path (to-string dir))
        (m (if mode (truncate (to-number mode)) #o755)))
    (handler-case
        (progn (sb-posix:mkdir path m) t)
      (error () nil))))

(defun p-rmdir (dir)
  "Perl rmdir - remove empty directory. Returns true on success."
  (handler-case
      (progn (sb-posix:rmdir (to-string dir)) t)
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

(defun p-chown (uid gid &rest files)
  "Perl chown UID, GID, LIST — change owner/group. Returns count changed.
   A UID or GID of -1 leaves that attribute unchanged.  A filehandle in the LIST
   is fchown'd by descriptor."
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

(defun p-utime (atime mtime &rest files)
  "Perl utime ATIME, MTIME, LIST — set access/modification times. Returns count.
   Times are Unix-epoch seconds (same convention as sb-posix:utime).  undef
   ATIME/MTIME means 'now', which sb-posix:utime uses when the times are omitted."
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
        ;; 16-bit access (big-endian / network byte order)
        ((and (= bits 16) (= bit-offset 0))
         (let ((b0 (if (< byte-offset (length s)) (char-code (char s byte-offset)) 0))
               (b1 (if (< (1+ byte-offset) (length s)) (char-code (char s (1+ byte-offset))) 0)))
           (+ (ash b0 8) b1)))
        ;; 32-bit access (big-endian / network byte order)
        ((and (= bits 32) (= bit-offset 0))
         (let ((b0 (if (< byte-offset (length s)) (char-code (char s byte-offset)) 0))
               (b1 (if (< (+ 1 byte-offset) (length s)) (char-code (char s (+ 1 byte-offset))) 0))
               (b2 (if (< (+ 2 byte-offset) (length s)) (char-code (char s (+ 2 byte-offset))) 0))
               (b3 (if (< (+ 3 byte-offset) (length s)) (char-code (char s (+ 3 byte-offset))) 0)))
           (+ (ash b0 24) (ash b1 16) (ash b2 8) b3)))
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
          ;; 16-bit aligned (big-endian, Perl stores MSB first)
          ((and (= bits 16) (= bit-offset 0))
           (setf (char s-ext byte-offset)       (code-char (logand (ash val -8) 255))
                 (char s-ext (1+ byte-offset))  (code-char (logand val 255))))
          ;; 32-bit aligned (big-endian)
          ((and (= bits 32) (= bit-offset 0))
           (setf (char s-ext byte-offset)       (code-char (logand (ash val -24) 255))
                 (char s-ext (+ byte-offset 1)) (code-char (logand (ash val -16) 255))
                 (char s-ext (+ byte-offset 2)) (code-char (logand (ash val -8)  255))
                 (char s-ext (+ byte-offset 3)) (code-char (logand val           255))))
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
      ;; Post-1900: use decode-universal-time (handles DST / TZ env vars)
      ((>= unix-time (- +unix-epoch-offset+))
       (let ((universal (+ unix-time +unix-epoch-offset+)))
         (multiple-value-bind (sec min hour day month year wday dst-p tz)
             (decode-universal-time universal)
           (declare (ignore tz))
           (let ((perl-wday (mod (1+ wday) 7))
                 (perl-year (- year 1900))
                 (perl-mon  (1- month))
                 (yday (- (floor (encode-universal-time 0 0 0 day month year) 86400)
                          (floor (encode-universal-time 0 0 0 1 1 year) 86400))))
             (if (eq *wantarray* t)
                 (make-array 9 :initial-contents
                             (list sec min hour day perl-mon perl-year perl-wday yday (if dst-p 1 0))
                             :adjustable t :fill-pointer t)
                 (%pcl-format-time perl-wday perl-mon day hour min sec year))))))
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

(defun p-select (&optional fh)
  "Perl select - set default output filehandle (stub).
   Perl's select returns the *name* of the previously-selected default handle
   (e.g. \"main::STDOUT\"), which is always a true value — never undef.  Return
   that string rather than raw nil: a nil here is dropped during Perl list
   flattening (it reads as the empty list), so `ok(select(), 'name')` would lose
   its description argument.  See %p-flatten-list — undef must be a real value."
  (declare (ignore fh))
  "main::STDOUT")

(defun p-write (&optional fh)
  "Perl write - emit a report via the current `format` (stub).
   format/write report templates are deliberately not-supported
   (docs/not-supported.md) and are stripped at the source level, so there is
   nothing to write.  Return 1 (Perl's success value) rather than crashing, so a
   stray write() call does not abort the whole program."
  (declare (ignore fh))
  1)

(defun p-exit (&optional code)
  "Perl exit - terminate program with exit code."
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

(defun p-find-module-in-inc (rel-path)
  "Search @INC for module file, return absolute path or nil."
  (loop for dir-raw across @INC
        ;; Unbox if dir is stored as a box (l-value array storage)
        for dir = (unbox dir-raw)
        ;; Ensure dir ends with / so merge-pathnames treats it as directory
        for dir-str = (let ((s (if (stringp dir) dir (namestring dir))))
                        (if (and (> (length s) 0)
                                 (char/= (char s (1- (length s))) #\/))
                            (concatenate 'string s "/")
                            s))
        for full-path = (merge-pathnames rel-path (pathname dir-str))
        when (probe-file full-path)
        return (namestring (truename full-path))))

;;; --- Cache Management ---

(defun p-ensure-cache-dir ()
  "Create cache directory if it doesn't exist."
  (ensure-directories-exist *pcl-cache-dir*))

(defun p-compute-cache-path (source-path &optional lisp-p)
  "Compute cache path for a source file using hash of absolute path.
   LISP-P: if true, return .lisp path; else .fasl"
  (let* ((abs-path (namestring (truename source-path)))
         (hash (sxhash abs-path))
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

(defun p-transpile-string (perl-code pkg-name)
  "Transpile a Perl string to CL code via the persistent pl2cl server.
   Returns the CL text string, or signals an error on failure."
  (let* ((proc     (p-ensure-transpiler))
         (in       (sb-ext:process-input  proc))
         (out      (sb-ext:process-output proc))
         (code-len (length perl-code)))
    ;; Send request: pkg\n char-count\n perl-code
    (write-string pkg-name in)
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
          (error "pl2cl server: ~A" resp-buf)))))

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
             ;; Lisp mode: just cache .lisp
             (progn
               (with-open-file (out cache-path
                                    :direction :output
                                    :if-exists :supersede)
                 (write-string lisp-code out))
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
             (proclaim (list 'special to-sym))
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

(defun p-use (module-name &key (import-args :default) (do-import t))
  "Perl use - load module at compile time and import symbols.
   MODULE-NAME: 'Foo::Bar' or 'Foo/Bar.pm'
   IMPORT-ARGS: the evaluated import list (a vector) — `use Foo X` makes X a Perl
   list — or :default for a bare `use Foo;` (import with no args).
   DO-IMPORT: when NIL, load the module but do NOT call its ->import (this is the
   `require Foo` semantics — load only, no symbol import)."
  ;; Skip XS-only modules that cannot be transpiled
  (when (member module-name *p-xs-only-modules* :test #'string=)
    (return-from p-use t))
  ;; Modules PCL provides internally (Test::More TAP API): don't load the real
  ;; .pm; load PCL's TAP layer on demand instead (no-op if already loaded).
  (when (member module-name *p-pcl-provided-modules* :test #'string=)
    (p-ensure-test-lib)
    (return-from p-use t))
  ;; Lexical pragmas (strict/warnings/feature/...): never load the core .pm —
  ;; PCL doesn't model the hint bitmasks they touch.  Reached only via a string
  ;; require (the `if` pragma) or an explicit `require strict`.
  (when (member module-name *p-pragma-modules* :test #'string-equal)
    (return-from p-use t))
  (let ((rel-path (p-module-to-path module-name))
        (caller-pkg *package*))
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
               rel-path (coerce @INC 'list)))
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
         (loop for x across (p-flatten-marker-array item) do (vector-push-extend x result)))
        (t
         (let ((val (unbox item)))
           (cond
             ((and (vectorp val) (not (stringp val)))
              (loop for x across val do (vector-push-extend x result)))
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

(defun %p-resolve-sub-symbol (name)
  "Resolve a Perl sub-name string (\"foo\" or \"Pkg::foo\") to its CL symbol
   PKG::PL-FOO, or NIL if the package/symbol does not exist.  Shared by the
   symbolic-code-ref paths: &{$name}(...), defined/exists &{$name}.  An
   unqualified name resolves against the current CL package (MAIN -> main)."
  (let* ((name (to-string name))
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
        ;; Not a function: treat as symbolic sub name (string/number).
        ;; Look up PL-NAME in the current CL package (typeglob CODE slot or defun).
        (let* ((name (to-string fn))
               (sep-pos (search "::" name :from-end t))
               (perl-pkg (if sep-pos
                             (subseq name 0 sep-pos)
                             (let ((cpkg (package-name *package*)))
                               (if (string= cpkg "MAIN") "main" cpkg))))
               (bare-name (if sep-pos (subseq name (+ sep-pos 2)) name))
               (cl-pkg (find-package (%pcl-invert-case perl-pkg)))
               (sym (when cl-pkg
                      (find-symbol (%pcl-cl-sub-name bare-name)
                                   cl-pkg)))
               (fn-val (when (and sym (fboundp sym)) (symbol-function sym))))
          (if fn-val
              (apply fn-val args)
              (p-die (format nil
                             "Undefined subroutine &~A::~A called at (eval 1) line 1.~%"
                             perl-pkg bare-name)))))))

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
     (lambda (&rest args) (apply (symbol-function sym) args)))
    ;; Not declared at all: return a lambda that tries AUTOLOAD when called.
    (t
     (let ((pkg *package*))
       (lambda (&rest args)
         (declare (ignore args))
         (let ((al (intern (%pcl-cl-sub-name "AUTOLOAD") pkg)))
           (if (fboundp al)
               (funcall (symbol-function al))
               (error 'undefined-function :name sym))))))))

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
       (let* ((s (stringify-value v))
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
      ;; Double-boxed: box(box(arr)) from \@arr — unwrap both layers
      ((p-box-p v) (unbox v))
      ;; Direct vector
      ((and v (vectorp v) (not (stringp v))) v)
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
      (t v))))

(defun %p-symref-box (name-str)
  "Resolve Perl symbolic scalar reference NAME-STR to a CL box.
   Returns the box on success, NIL if the name is invalid or variable not found."
  ;; CL symbols cannot contain null bytes — silently return nil
  (when (find #\Nul name-str) (return-from %p-symref-box nil))
  (let* ((pos (search "::" name-str :from-end t))
         ;; perl-pkg-to-cl-pkg-name: multi-seg (Foo::Bar) stays case-preserved to
         ;; match its CL package |Foo::Bar|; single-seg is upcased.  Plain
         ;; string-upcase wrongly gave FOO::BAR (no such package) for multi-seg.
         (pkg-str (if pos (perl-pkg-to-cl-pkg-name (subseq name-str 0 pos)) nil))
         (var-str (if pos (subseq name-str (+ pos 2)) name-str))
         (pkg (if pkg-str (find-package pkg-str) *package*)))
    (when pkg
      (let ((sym (find-symbol (concatenate 'string "$" (%pcl-invert-case var-str)) pkg)))
        (when (and sym (boundp sym))
          (let ((v (symbol-value sym)))
            (when (p-box-p v) v)))))))

(defun (setf %p-symref-box) (new-box name-str)
  "Set Perl symbolic scalar reference NAME-STR to NEW-BOX."
  (when (find #\Nul name-str) (return-from %p-symref-box new-box))
  (let* ((pos (search "::" name-str :from-end t))
         ;; perl-pkg-to-cl-pkg-name: multi-seg (Foo::Bar) stays case-preserved to
         ;; match its CL package |Foo::Bar|; single-seg is upcased.  Plain
         ;; string-upcase wrongly gave FOO::BAR (no such package) for multi-seg.
         (pkg-str (if pos (perl-pkg-to-cl-pkg-name (subseq name-str 0 pos)) nil))
         (var-str (if pos (subseq name-str (+ pos 2)) name-str))
         (pkg (if pkg-str
                  (or (find-package pkg-str)
                      (make-package pkg-str :use '(:cl :pcl)))
                  *package*)))
    (let* ((sym-name (concatenate 'string "$" (%pcl-invert-case var-str)))
           (sym (or (find-symbol sym-name pkg)
                    (intern sym-name pkg))))
      (proclaim `(special ,sym))
      (setf (symbol-value sym) new-box)))
  new-box)

(defun %p-symref-array (name-str)
  "Resolve symbolic array reference NAME-STR (e.g. '3foo::ISA') to the CL vector.
   Creates the package and the @VAR binding if they don't exist yet, so that
   assignment through a symbolic ref works: @{\"pkg::var\"} = (...).
   Returns the adjustable vector."
  (when (find #\Nul name-str) (return-from %p-symref-array
                                (make-array 0 :adjustable t :fill-pointer 0)))
  (let* ((pos (search "::" name-str :from-end t))
         ;; perl-pkg-to-cl-pkg-name: multi-seg (Foo::Bar) stays case-preserved to
         ;; match its CL package |Foo::Bar|; single-seg is upcased.  Plain
         ;; string-upcase wrongly gave FOO::BAR (no such package) for multi-seg.
         (pkg-str (if pos (perl-pkg-to-cl-pkg-name (subseq name-str 0 pos)) nil))
         (var-str (if pos (subseq name-str (+ pos 2)) name-str))
         (pkg (if pkg-str
                  (or (find-package pkg-str)
                      (make-package pkg-str :use '(:cl :pcl)))
                  *package*))
         (sym-name (concatenate 'string "@" (%pcl-invert-case var-str)))
         (sym (or (find-symbol sym-name pkg) (intern sym-name pkg))))
    (proclaim `(special ,sym))
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
  (let* ((pos (search "::" name-str :from-end t))
         ;; perl-pkg-to-cl-pkg-name: multi-seg (Foo::Bar) stays case-preserved to
         ;; match its CL package |Foo::Bar|; single-seg is upcased.
         (pkg-str (if pos (perl-pkg-to-cl-pkg-name (subseq name-str 0 pos)) nil))
         (var-str (if pos (subseq name-str (+ pos 2)) name-str))
         (pkg (if pkg-str
                  (or (find-package pkg-str)
                      (make-package pkg-str :use '(:cl :pcl)))
                  *package*))
         (sym-name (concatenate 'string "%" (%pcl-invert-case var-str)))
         (sym (or (find-symbol sym-name pkg) (intern sym-name pkg))))
    (proclaim `(special ,sym))
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

(defun p-ref (val)
  "Perl ref() function - get reference type or class name if blessed.
   Returns empty string for non-references."
  ;; Unbox the variable to get what it contains
  (let ((inner (unbox val)))
    (cond
      ;; Blessed value - return class name
      ((and (p-box-p val) (p-box-class val))
       (p-box-class val))
      ((and (hash-table-p inner) (gethash :__class__ inner))
       (gethash :__class__ inner))
      ;; Reference box: inner is a p-box - check what it wraps (ARRAY/HASH/SCALAR)
      ((p-box-p inner)
       ;; Check inner box's class first: handles `local $x = blessed_ref` where
       ;; the local `let` binding wraps the original box in a new make-p-box.
       (if (p-box-class inner)
           (p-box-class inner)
           (let ((inner2 (p-box-value inner)))
             (cond
               ;; Magic lvalue ref (\substr / \pos / \vec): the referent box holds
               ;; a p-magic-cell with :lvalue kind → "LVALUE" (arylen's cell has
               ;; kind nil and falls through to "SCALAR").  The referent is `inner`
               ;; for a direct `\substr(...)` (is-ref wrapper) and `inner2` when the
               ;; ref was stored through a variable (`my $r = \substr…; ref $r`),
               ;; mirroring the ref-to-ref arm below.
               ((let* ((referent (if (p-box-is-ref val) inner inner2))
                       (rv (and (p-box-p referent) (p-box-value referent))))
                  (and (p-magic-cell-p rv)
                       (eq (p-magic-cell-kind rv) :lvalue)))
                "LVALUE")
               ;; Direct aggregate referent (raw value held by the pointed-at
               ;; scalar): \$qr → REGEXP, \$aref → ARRAY, \$href → HASH. These
               ;; check inner2 (= the referent's held value for a direct `\$x`).
               ((and (vectorp inner2) (not (stringp inner2))) "ARRAY")
               ((hash-table-p inner2) (or (gethash :__class__ inner2) "HASH"))
               ((p-regex-match-p inner2) "REGEXP")
               ;; Ref-to-ref → "REF".  The referent (the scalar pointed at) is
               ;; `inner` when `val` is itself the ref-wrapper (is-ref set, e.g.
               ;; a literal `\$x`), else `inner2` (when `val` is a variable whose
               ;; value is the wrapper).  It is a ref-to-ref iff that referent is
               ;; itself a ref-wrapper (\\1) or *holds* a reference (\$r, \$aref
               ;; through a variable).  %scalar-holds-ref-p is non-recursive so a
               ;; self-referential scalar ($x=\$x) does not loop, and a plain
               ;; scalar — incl. undef (*p-undef*) and '' array elements — yields
               ;; SCALAR, not REF.
               ((let ((referent (if (p-box-is-ref val) inner inner2)))
                  (or (and (p-box-p referent) (p-box-is-ref referent))
                      (%scalar-holds-ref-p referent)))
                "REF")
               ;; Scalar reference: box containing box (from p-backslash $x)
               (t "SCALAR")))))
      ;; Old-format hash reference (autovivified, single-boxed)
      ((hash-table-p inner) "HASH")
      ;; Old-format array reference (autovivified, single-boxed)
      ((or (listp inner) (and (vectorp inner) (not (stringp inner)))) "ARRAY")
      ;; Code reference
      ((functionp inner) "CODE")
      ;; Typeglob reference
      ((p-typeglob-p inner) "GLOB")
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

      ;; *foo = \&sub or *foo = sub{} — CODE slot
      ((functionp inner)
       (setf (fdefinition (intern (%pcl-uname-to-sub uname) pkg))
             inner))

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
       (setf (fdefinition (intern (%pcl-uname-to-sub uname) pkg))
             rhs)))))

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
               (pkg-str  (if sep-pos (subseq name-str 0 sep-pos) "main"))
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
               (pkg-str  (if sep-pos (subseq name-str 0 sep-pos) "main"))
               (bare-str (if sep-pos (subseq name-str (+ sep-pos 2)) name-str))
               (pkg (or (%pcl-find-package pkg-str)
                        (make-package (perl-pkg-to-cl-pkg-name pkg-str) :use '(:cl :pcl)))))
          (make-p-typeglob pkg (%pcl-invert-case bare-str))))))

(defun p-glob-copy (dst-pkg dst-uname src-glob)
  "Copy all slots from src-glob into dst (pkg, uname)."
  (let ((sp (p-typeglob-package src-glob))
        (sn (p-typeglob-name src-glob)))
    ;; CODE
    (let ((src-sym (intern (%pcl-uname-to-sub sn) sp)))
      (when (fboundp src-sym)
        (setf (fdefinition (intern (%pcl-uname-to-sub dst-uname) dst-pkg))
              (fdefinition src-sym))))
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
        ((string= slot-s "NAME")    (make-p-box (p-typeglob-name glob)))
        ((string= slot-s "PACKAGE") (make-p-box (package-name (p-typeglob-package glob))))
        ((string= slot-s "GLOB")    glob)
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
                        do (cond
                             ((p-flatten-marker-p item)
                              (add-items (p-flatten-marker-array item)))
                             ((and (vectorp item) (not (stringp item)))
                              (add-items item))
                             ((null item)
                              (vector-push-extend nil result))
                             (t
                              (%p-array-store-scalar result item)))))
                 ((listp x)
                  (loop for item in x
                        do (cond
                             ((p-flatten-marker-p item)
                              (add-items (p-flatten-marker-array item)))
                             ((and (vectorp item) (not (stringp item)))
                              (add-items item))
                             ((null item)
                              (vector-push-extend nil result))
                             (t
                              (%p-array-store-scalar result item)))))
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
       (maphash (lambda (k v) (setf (gethash k copy) v)) raw))
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
       ;; Any non-nil function object exists (declared stub or defined body).
       (make-p-box 1))
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
       (let* ((fname (ignore-errors (sb-kernel:%fun-name v)))
              (status (and (symbolp fname) (gethash fname *p-declared-subs*))))
         (if (eq status :defined)
             (make-p-box 1)
             (make-p-box nil))))
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

(defun p-tie (box classname &rest args)
  "Perl tie - bind a scalar variable to a class implementing TIESCALAR.
   Dispatches to TIEARRAY or TIEHASH when box holds a vector or hash-table
   (future: requires array/hash boxing for full correctness).
   Falls back gracefully if the tie class or method is not available."
  (unless (p-box-p box)
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
         (proxy (make-p-tie-proxy :tie-obj tie-result
                                  :saved-value current)))
    (setf (p-box-value box) proxy
          (p-box-sv-ok box) nil
          (p-box-nv-ok box) nil)
    tie-result))

(defun p-untie (box)
  "Perl untie - remove tie from variable, restoring its pre-tie value.
   Calls UNTIE on the tie object if the method exists."
  (when (p-box-p box)
    (let ((v (p-box-value box)))
      (when (p-tie-proxy-p v)
        (ignore-errors
          (p-method-call (p-tie-proxy-tie-obj v) "UNTIE"))
        (setf (p-box-value box) (p-tie-proxy-saved-value v)
              (p-box-sv-ok box) nil
              (p-box-nv-ok box) nil))))
  (make-p-box 1))

(defun p-tied (box)
  "Perl tied() - returns the tie object if box is tied, undef otherwise."
  (if (p-box-p box)
      (let ((v (p-box-value box)))
        (if (p-tie-proxy-p v)
            (p-tie-proxy-tie-obj v)
            *p-undef*))
      *p-undef*))

(defun p-scalar (&rest args)
  "Perl scalar function - returns length for arrays, value for scalars.
   With multiple args (comma expr), returns the last value."
  (let* ((val (car (last args)))
         (v (unbox val)))
    (cond
      ;; Strings are scalars, return as-is
      ((stringp v) v)
      ;; Arrays (non-string vectors) return length
      ((and (vectorp v) (adjustable-array-p v)) (length v))
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

(defun p-prototype (&optional ref)
  "Perl prototype() - returns the prototype string of a function, or undef.
   PCL does not track Perl prototypes, so this always returns undef.
   This is sufficient for the common guard pattern: 'if (defined prototype(...))'."
  (declare (ignore ref))
  *p-undef*)

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
       ;; Scalar reference: ref is the wrapper box (from p-backslash), inner is the
       ;; variable box it points to. Bless sets the class on the wrapper (the reference
       ;; itself), NOT on what it points to. Then box-set will copy the class to the
       ;; variable that holds the blessed ref.
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
     ;; Check box's class slot first, then check the value inside
     (or (p-box-class obj)
         (let ((val (p-box-value obj)))
           (cond
             ((hash-table-p val) (gethash :__class__ val))
             ((p-box-p val) (p-box-class val))
             (t nil)))))
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

(defun %pcl-find-package (pkg-str)
  "Find CL package for Perl package name PKG-STR.
   Tries upcase first (single-word packages defined via :Foo keyword), then
   exact case (multi-level packages defined via :|Foo::Bar| notation)."
  (or (find-package (perl-pkg-to-cl-pkg-name pkg-str))
      (find-package (%pcl-invert-case pkg-str))
      (find-package pkg-str)))

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
                           (unbox (p-method-call (p-tie-proxy-tie-obj (p-box-value obj)) "FETCH"))
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
                         (find-class (intern (string-upcase clos-class-name) pkg) nil))))

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
                ;; Perl special case: ->import and ->unimport on unknown packages return nothing
                (if (or (string-equal method-name "import") (string-equal method-name "unimport"))
                    (return-from p-method-call
                      (if (eq *wantarray* t)
                          (make-p-flatten-marker :array (make-array 0 :adjustable t :fill-pointer 0))
                          nil))
                    ;; Package unknown (never blessed into, never declared): add "perhaps" hint
                    (p-die (format nil "Can't locate object method \"~A\" via package \"~A\" (perhaps you forgot to load \"~A\"?) at - line 1.~%"
                                   method-name class-name class-name)))))
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
   MUST stay in lock-step with _pkg_to_clos_class in Pl/Parser.pm."
  (concatenate 'string "plc-" (string-downcase (substitute #\- #\: name))))

;;; Indirect-object SUPER:: dispatch: SUPER::m{@a} where @a[0] is the invocant
(defun %pcl-super-indirect (method cur-pkg inv-args-vec)
  "Handle SUPER::method{@array} indirect-object syntax.
   inv-args-vec: a vector where element 0 is the invocant; rest are args."
  (let ((vec (cond ((vectorp inv-args-vec) inv-args-vec)
                   ((null inv-args-vec)    (vector))
                   (t                      (vector inv-args-vec)))))
    (when (zerop (length vec))
      (p-die (format nil "Can't call method \"~A\" without a package or object reference" method)))
    (apply #'p-super-call (elt vec 0) method cur-pkg
           (coerce (subseq vec 1) 'list))))

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
                       (find-class (intern (string-upcase clos-class-name) pkg) nil))))
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
    (if (member check-class (%pcl-isa-ancestry obj-class) :test #'string-equal)
        t
        nil)))

;;; ============================================================
;;; Regex Support (using CL-PPCRE)
;;; ============================================================

;; Regex operation types
(defstruct p-regex-match
  "Regex match operation m//"
  pattern
  modifiers)

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
  "Parse modifier string like 'gi' into plist (:g t :i t)"
  (let ((result nil))
    (loop for c across mod-string
          do (let ((mod (intern (string-upcase (string c)) :keyword)))
               (setf (getf result mod) t)))
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

(defun p-regex (pattern-string)
  "Parse /pattern/modifiers and return a regex-match struct.
   Pattern-string is like '/foo/i' or 'm/bar/g' or 'm{pattern}s'"
  (let* ((str (to-string pattern-string))
         (first-char (char str 0))
         (start-delim (if (char= first-char #\m) 1 0))
         (open-delim (char str start-delim))
         (close-delim (get-closing-delim open-delim))
         (end-delim (position close-delim str :start (1+ start-delim) :from-end t))
         (pattern (perl-regex-to-ppcre (subseq str (1+ start-delim) end-delim)))
         (modifiers (if (< end-delim (1- (length str)))
                        (subseq str (1+ end-delim))
                        "")))
    (make-p-regex-match :pattern pattern
                        :modifiers (parse-regex-modifiers modifiers))))

(defun p-regex-from-parts (pattern modifiers)
  "Build a regex from a runtime-interpolated pattern string and modifier string.
   Used when the regex contains variable interpolation (e.g. /$x/ or qr/$x/)."
  (make-p-regex-match :pattern (perl-regex-to-ppcre (to-string pattern))
                      :modifiers (parse-regex-modifiers (to-string modifiers))))

(defun p-qr (pattern-string)
  "Parse qr/pattern/modifiers and return a compiled regex (regex-match struct).
   Pattern-string is like 'qr/foo/i' or 'qr{pattern}i'"
  (let* ((str (to-string pattern-string))
         ;; Skip past 'qr' prefix
         (start-delim 2)
         (open-delim (char str start-delim))
         (close-delim (get-closing-delim open-delim))
         (end-delim (position close-delim str :start (1+ start-delim) :from-end t))
         (pattern (perl-regex-to-ppcre (subseq str (1+ start-delim) end-delim)))
         (modifiers (if (< end-delim (1- (length str)))
                        (subseq str (1+ end-delim))
                        "")))
    (make-p-regex-match :pattern pattern
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
(defun %pcl-parse-x-flag-group (pat i cur-x)
  "PAT[i] is #\(.  If this opens a (?flags:) / (?flags) MODE group, return
   (values T end-index emit-string terminator newx); else (values NIL ...).
   x is removed from the emitted flags; NEWX is the x state it selects."
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
                 (newx (cond ((find #\x on) t) ((find #\x off) nil) (t base)))
                 (on2 (remove #\x on)) (off2 (remove #\x off))
                 (flags (concatenate 'string (if caret "^" "") on2
                                     (if (> (length off2) 0) (concatenate 'string "-" off2) "")))
                 (emit (if (char= term #\:)
                           (if (= 0 (length flags)) "(?:" (format nil "(?~a:" flags))
                           (if (= 0 (length flags)) "" (format nil "(?~a)" flags)))))
            (return-from %pcl-parse-x-flag-group (values t (1+ j) emit term newx))))))
    (values nil nil nil nil nil)))

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

(defun %pcl-normalize-extended (pat base-x)
  "Strip insignificant whitespace/comments per /x scope and rewrite x mode
   modifiers away.  BASE-X = is the whole pattern extended (/x flag set)?"
  (let ((out (make-string-output-stream)) (i 0) (n (length pat))
        (xstack (list base-x)) (in-class nil))
    (flet ((curx () (car xstack)))
      (loop while (< i n) do
            (let ((c (char pat i)))
              (cond
                ((char= c #\\)
                 (write-char c out)
                 (when (< (1+ i) n) (write-char (char pat (1+ i)) out))
                 (incf i 2))
                (in-class
                 (write-char c out)
                 (when (char= c #\]) (setf in-class nil))
                 (incf i))
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
                 (multiple-value-bind (flag-p end emit term newx)
                     (%pcl-parse-x-flag-group pat i (curx))
                   (cond
                     ((not flag-p) (write-char #\( out) (incf i) (push (curx) xstack))
                     ((char= term #\:) (write-string emit out) (setf i end) (push newx xstack))
                     (t (write-string emit out) (setf i end) (setf (car xstack) newx)))))
                ((char= c #\))
                 (write-char c out) (incf i) (when (cdr xstack) (pop xstack)))
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
   See docs/clppcre-extended-mode-modifier-bug.md."
  (if (%pcl-has-x-modifier pattern)
      (apply #'cl-ppcre:create-scanner
             (%pcl-normalize-extended pattern (getf options :extended-mode))
             ;; drop :extended-mode (and its value) from the options plist —
             ;; we have already applied it by hand, so cl-ppcre must not.
             (loop for (k v) on options by #'cddr
                   unless (eq k :extended-mode) nconc (list k v)))
      (apply #'cl-ppcre:create-scanner pattern options)))

(defvar *pcl-scanner-cache* (make-hash-table :test 'equal)
  "Memoizes (pattern + options) -> (scanner . reg-names).  cl-ppcre compiles a
   fresh scanner on every create-scanner call and PCL builds one per match, so
   without this a regex used in a loop recompiles every iteration.  Also
   amortizes the /x normaliser (%pcl-build-scanner) to once per distinct pattern.
   Scanners are stateless closures (match state lives in the registers passed to
   scan), so sharing one is safe.  The cache is per-process; PCL runs one program
   per image, so distinct patterns are bounded and there is no staleness.")

(defun %pcl-create-scanner (pattern options)
  "Memoized %pcl-build-scanner.  Returns (values scanner reg-names)."
  (let* ((key (format nil "~A~C~{~A~^ ~}" pattern #\Nul options))
         (hit (gethash key *pcl-scanner-cache*)))
    (if hit
        (values (car hit) (cdr hit))
        (multiple-value-bind (scanner reg-names) (%pcl-build-scanner pattern options)
          (setf (gethash key *pcl-scanner-cache*) (cons scanner reg-names))
          (values scanner reg-names)))))

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
    options))

(defun clear-capture-groups ()
  "Reset all capture group variables.  $1..$9 reset to *p-undef* (Perl undef),
   NOT raw nil — see the defvar note: a raw-nil capture vanishes when flattened
   into a list (%p-flatten-list treats raw nil as an empty-list/hole marker)."
  (setf $1 *p-undef* $2 *p-undef* $3 *p-undef* $4 *p-undef* $5 *p-undef*
        $6 *p-undef* $7 *p-undef* $8 *p-undef* $9 *p-undef*
        |$&| nil |$`| nil |$'| nil |$+| nil)
  (clrhash %+))

(defun set-match-vars (str match-start match-end reg-starts reg-ends)
  "Set the match position variables from a successful match:
   $& (MATCH), $` (PREMATCH), $' (POSTMATCH), and $+ (last capture that matched)."
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
                 (return)))))
  ;; @- / @+ : offset arrays.  Element 0 is the whole-match start/end; element
  ;; N (1-based) is capture group N's start/end (nil for groups that did not
  ;; participate).  Elements are boxed integers like any other array element.
  (when (and match-start match-end)
    (setf (fill-pointer |@-|) 0
          (fill-pointer |@+|) 0)
    (vector-push-extend (make-p-box match-start) |@-|)
    (vector-push-extend (make-p-box match-end)   |@+|)
    (when (and reg-starts reg-ends)
      (loop for i from 0 below (length reg-starts)
            for rs = (aref reg-starts i)
            for re = (aref reg-ends i)
            do (vector-push-extend (make-p-box rs) |@-|)
            (vector-push-extend (make-p-box re) |@+|)))))

(defmacro %set-cap (var str starts ends idx)
  "Set capture variable VAR from reg-starts/ends at IDX, guarding against NIL (optional group)."
  `(let ((rs (aref ,starts ,idx)) (re (aref ,ends ,idx)))
     (setf ,var (if (and rs re) (subseq ,str rs re) *p-undef*))))

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
      ;; Populate %+ with named captures
      ;; reg-names is a list from cl-ppcre:create-scanner, e.g. ("year" "month" NIL)
      (when reg-names
        (loop for name in reg-names
              for i from 0
              when (and name (< i num-groups))
              do (let ((rs (aref reg-starts i))
                       (re (aref reg-ends   i)))
                   (when (and rs re)
                     (setf (gethash name %+) (subseq str rs re)))))))))

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

(defun %pcl-scan-anchored-list (scanner str reg-names start)
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
        (set-match-vars str last-ms last-me last-rs last-re))
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
        (multiple-value-bind (scanner reg-names)
            (%pcl-create-scanner pattern options)
          ;; Perl clears %+ on every match attempt, even failures.
          ;; $1..$9 are only cleared/set on successful matches.
          (clrhash %+)
          (cond
            ;; /\G.../g in list context: contiguous anchored matches from pos
            ((and global-p (eq *wantarray* t) anchored-g)
             (prog1
                 (%pcl-scan-anchored-list scanner str reg-names
                                          (or (gethash string *p-match-pos*) 0))
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
                       (push (if (and (aref rs i) (aref re i))
                                 (subseq str (aref rs i) (aref re i))
                                 nil)
                             all-results))
                     (push (subseq str ms me) all-results)))
               (let* ((items (nreverse all-results))
                      (result (make-array (length items) :adjustable t :fill-pointer t)))
                 (loop for item in items for i from 0 do (setf (aref result i) item))
                 (when items
                   (clear-capture-groups)
                   (set-capture-groups str last-rs last-re reg-names)
                   (set-match-vars str last-ms last-me last-rs last-re))
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
                       (set-match-vars str match-start match-end reg-starts reg-ends)
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
                       (set-match-vars str match-start match-end reg-starts reg-ends)
                       (if (eq *wantarray* t)
                           (let* ((num-groups (length reg-starts))
                                  (captures (make-array (max num-groups 1) :adjustable t :fill-pointer t)))
                             (if (zerop num-groups)
                                 ;; No capture groups: Perl returns (1) in list context on success
                                 (setf (aref captures 0) 1)
                                 (dotimes (i num-groups)
                                   (setf (aref captures i)
                                         (if (and (aref reg-starts i) (aref reg-ends i))
                                             (subseq str (aref reg-starts i) (aref reg-ends i))
                                             nil))))
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
  (let* ((str (to-string (unbox string-box)))
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
          (multiple-value-bind (scanner reg-names)
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
                                  ;; Populate %+ from named groups using reg-names from outer scope
                                  (clrhash %+)
                                  (when reg-names
                                    (loop for name in reg-names
                                          for i from 0
                                          when (and name (< i (length groups)))
                                          do (let ((val (nth i groups)))
                                               (when val (setf (gethash name %+) val)))))
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
                        (set-match-vars str match-start match-end reg-starts reg-ends)))
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
                  ;; Normal: update the boxed string in place, return count
                  (progn
                    (when (stringp result)
                      (if (p-box-p string-box)
                          (setf (p-box-value string-box) result
                                (p-box-sv-ok string-box) nil
                                (p-box-nv-ok string-box) nil)
                          (warn "Cannot modify non-boxed value in s///")))
                    count)))))
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
  (let* ((str (to-string (unbox string-box)))
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
             (warn "Cannot modify non-boxed value in tr///"))
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

;; CORE::__SUB__ stub — returns a no-op lambda (PCL does not track current sub)
(defun pl-__SUB__ ()
  (lambda (&rest args) (declare (ignore args)) nil))

;; utf8::unicode_to_native / native_to_unicode map between Unicode and the
;; platform's native code point.  On any ASCII (non-EBCDIC) platform — which is
;; all PCL ever targets — both are the identity.  JSON::PP builds its
;; invalid-char regex with chr(utf8::unicode_to_native($i)), so these must exist
;; in the production runtime (not just the test library).  Defined in :pcl and
;; exported so the :utf8 package (which (:use :pcl)) inherits them — this also
;; avoids a name-conflict with cl/pcl-test.lisp, which defines the same names in
;; :pcl for charset_tools.pl (it then just redefines the same symbol).
(in-package :pcl)
(defun pl-unicode_to_native (&optional cp) cp)
(defun pl-native_to_unicode (&optional cp) cp)
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $VERSION (make-p-box "1.50"))
  (defvar $BYTES (make-p-box 12))   ; bytes in warning bitmask (12 in modern Perl)
  )
(defun pl-unimport (&rest args) (declare (ignore args)) nil)
(defun pl-import (&rest args) (declare (ignore args)) nil)
(in-package :pcl)

;; Carp module stub - Carp loads utf8 which causes infinite loops in PCL
;; Stub out the most commonly used functions so code that 'use Carp' works
(defpackage :|Carp| (:use :cl :pcl))
(in-package :|Carp|)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $VERSION (make-p-box "1.50")))
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

;; Internals module stubs (CL case-folds Internals → INTERNALS)
(defpackage :INTERNALS (:use :cl :pcl))
(in-package :INTERNALS)
;; Returns 0 — PCL is not a reference-counted stack build
(defun pl-stack_refcounted () (make-p-box 0))
;; Internals::SvREADONLY($ref, $flag) — marks a scalar read-only.
;; PCL has no read-only box semantics; this is a documented no-op.
;; Returns 0 (not read-only) when called as a getter (1 arg).
(defun pl-svreadonly (&rest args)
  (declare (ignore args))
  (make-p-box 0))
;; Internals::SvREFCNT($ref) — reference count; always 1 in a GC runtime.
(defun pl-svrefcnt (&rest args)
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

(defun p-setgrent (&key wantarray)
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

(defun p-getgrent (&key wantarray)
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

(defun p-endgrent (&key wantarray)
  "Perl endgrent() — close the group database."
  (declare (ignore wantarray))
  (setf *p-group-list* nil)
  (setf *p-group-pos* 0)
  *p-undef*)

(defun p-getgrgid (gid &key wantarray)
  "Perl getgrgid(GID) — look up group entry by numeric GID."
  (handler-case
      (let ((g (sb-posix:getgrgid (truncate (to-number gid)))))
        (if g
            (if wantarray
                (p-group-struct-to-vec g)
                (make-p-box (sb-posix:group-name g)))
            *p-undef*))
    (sb-posix:syscall-error () *p-undef*)))

(defun p-getgrnam (name &key wantarray)
  "Perl getgrnam(NAME) — look up group entry by name."
  (handler-case
      (let ((g (sb-posix:getgrnam (to-string name))))
        (if g
            (if wantarray
                (p-group-struct-to-vec g)
                (make-p-box (sb-posix:group-name g)))
            *p-undef*))
    (sb-posix:syscall-error () *p-undef*)))

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
    (p-die (format nil "Can't locate loadable object for module ~A in @INC" mod))))
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
          (handler-bind ((warning #'muffle-warning))
            (load file))
          (setf (gethash name *pcl-loaded-extensions*) t)
          (return-from p-load-extension t)))))
  nil)

;;; pack/unpack loaded lazily on first call via self-loading stubs above.

;; Diagnostic banner on *error-output*, not *standard-output*, so it never
;; pollutes a script's stdout when run via the `pcl` command. Test harnesses
;; capture 2>&1 and filter it, so they are unaffected.
(format *error-output* "PCL Runtime loaded~%")
