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

(defpackage :pcl
  (:use :cl)
  (:export
   ;; Value boxing
   #:p-box #:make-p-box #:p-box-p #:p-box-value
   #:unbox #:ensure-boxed
   #:box-set #:box-nv #:box-sv  ; lazy caching accessors
   #:to-string #:to-number
   #:p-undef #:p-defined
   #:p-let #:p-$
   ;; Arithmetic
   #:p-+ #:p-- #:p-* #:p-/ #:p-% #:p-** #:p-int #:p-abs
   ;; Math
   #:p-sin #:p-cos #:p-atan2 #:p-exp #:p-log #:p-sqrt #:p-rand #:p-srand
   ;; String
   #:p-. #:p-str-x #:p-list-x #:p-length #:p-substr #:p-lc #:p-uc #:p-fc #:p-quotemeta
   #:p-chomp #:p-chop #:p-index #:p-rindex #:p-string-concat
   #:p-chr #:p-ord #:p-hex #:p-oct #:p-lcfirst #:p-ucfirst #:p-sprintf #:p-printf
   #:p-version-string
   #:p-pos
   ;; Assignment
   #:p-setf #:p-my #:p-incf #:p-decf
   #:p-pre++ #:p-post++ #:p-pre-- #:p-post--
   ;; Compound assignment
   #:p-*= #:p-/= #:p-%= #:p-**=
   #:p-.= #:p-str-x=
   #:p-bit-and= #:p-bit-or= #:p-bit-xor= #:p-<<= #:p->>=
   #:p-and-assign #:p-or-assign #:p-//=
   ;; Comparison (numeric)
   #:p-== #:p-!= #:p-< #:p-> #:p-<= #:p->= #:p-<=>
   ;; Comparison (string)
   #:p-str-eq #:p-str-ne #:p-str-lt #:p-str-gt #:p-str-le #:p-str-ge #:p-str-cmp
   ;; Chained comparison
   #:p-chain-cmp
   ;; Range operator
   #:p-.. #:p-...
   ;; Logical
   #:p-&& #:p-|| #:p-! #:p-not #:p-and #:p-or #:p-xor #:p-//
   ;; Bitwise
   #:p-bit-and #:p-bit-or #:p-bit-xor #:p-bit-not #:p-<< #:p->>
   #:p-to-s64 #:p-<<-int #:p->>-int
   ;; Data structures
   #:p-aref #:p-aref-box #:p-aref-deref #:p-gethash #:p-gethash-box #:p-gethash-deref
   #:p-ensure-hashref #:p-ensure-arrayref
   #:p-aslice #:p-hslice #:p-kv-hslice #:p-kv-aslice
   #:p-hash #:p-array-init #:p-array-last-index #:p-set-array-length
   #:p-push #:p-pop #:p-shift #:p-unshift #:p-splice #:p-flatten #:p-flatten-args
   #:p-keys #:p-values #:p-each #:p-exists #:p-exists-array #:p-delete #:p-delete-array
   #:p-delete-hash-slice #:p-delete-kv-hash-slice #:p-delete-array-slice
   ;; Control flow
   #:p-if #:p-unless #:p-while #:p-until #:p-for #:p-foreach
   #:p-return #:p-last #:p-last-dynamic #:p-next #:p-redo
   #:p-continue #:p-break
   ;; I/O
   #:p-print #:p-say #:p-warn #:p-die
   ;; do BLOCK
   #:p-do
   ;; Exception handling
   #:p-eval #:p-eval-block #:p-exception #:p-exception-object
   ;; File I/O
   #:p-open #:p-close #:p-eof #:p-tell #:p-seek #:p-pipe #:p-select
   #:p-binmode #:p-read #:p-sysread #:p-syswrite
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
   ;; Time functions
   #:p-time #:p-times #:p-sleep #:p-study #:p-reset #:p-vec #:p-vec-set #:p-localtime #:p-gmtime
   ;; Process control
   #:p-exit #:p-system #:p-backtick #:p-errno-string #:p-stash
   ;; Group/passwd database
   #:p-getgrent #:p-setgrent #:p-endgrent #:p-getgrgid #:p-getgrnam
   ;; Environment
   #:%ENV #:p-env-get #:p-env-set
   ;; Module system
   #:@INC #:%INC #:%SIG #:@ARGV #:@_ #:p-use #:p-require #:p-require-file
   ;; Functions
   #:p-backslash #:p-get-coderef #:p-ref #:p-reftype #:p-scalar #:p-wantarray #:p-caller #:p-prototype
   ;; Typeglob support
   #:p-typeglob #:p-typeglob-p #:make-p-typeglob
   #:p-typeglob-package #:p-typeglob-name
   #:p-make-typeglob #:p-glob-assign #:p-glob-copy
   #:p-glob-slot #:p-glob-undef-name #:p-local-glob
   #:p-local-hash-elem #:p-local-array-elem
   #:p-local-hash-elem-init #:p-local-array-elem-init
   #:p-copy-array #:p-copy-hash
   #:p-pack #:p-unpack
   #:p-grep #:p-map #:p-sort #:p-sort-get-fn #:p-reverse
   #:p-join #:p-split #:p-funcall-ref
   ;; Dereferencing (sigil cast operations)
   #:p-cast-@ #:p-cast-% #:p-cast-$
   ;; OO
   #:p-bless #:p-get-class #:p-method-call #:p-resolve-invocant
   #:p-super-call #:perl-pkg-to-clos-class #:clos-class-to-pkg
   #:p-can #:p-isa
   ;; Regex
   #:p-=~ #:p-!~ #:p-subst #:p-tr #:p-regex #:p-regex-from-parts
   ;; Capture groups
   #:$_ #:$1 #:$2 #:$3 #:$4 #:$5 #:$6 #:$7 #:$8 #:$9 #:%+
   ;; Special variables
   #:$$ #:$? #:|$.| #:$0 #:$@ #:|$^O| #:|$^V| #:|$^X| #:|${^TAINT}| #:|$/| #:|$\\| #:|$"| #:|$\|| #:|$;| #:|$,| #:|$]|
   #:|$~| #:|$=| #:|$-| #:|$%| #:|$:| #:|$^L| #:|$^A| #:|$^|
   ;; Context
   #:*wantarray*
   ;; Call depth tracking (for p-caller at top level)
   #:*pcl-sub-call-depth*
   ;; END blocks
   #:*end-blocks*
   ;; Subroutine reflection (exists &sub, defined &sub, undef &sub)
   #:p-sub-exists #:p-sub-defined #:p-undef-sub
   #:p-coderef-exists-p #:p-coderef-defined-p
   ;; Tie/untie/tied
   #:p-tie-proxy #:make-p-tie-proxy #:p-tie-proxy-p
   #:p-tie-proxy-tie-obj #:p-tie-proxy-saved-value
   #:p-tie #:p-untie #:p-tied
   ;; Compile-time definition macros (for BEGIN block support)
   #:p-defpackage #:p-sub #:p-declare-sub #:p-our #:p-my #:p-eval-direct
   ;; Assignment forms (distinct from p-setf for clarity)
   #:p-scalar-= #:p-array-= #:p-hash-= #:p-list-=
   ;; Lexical 'my' variable assignment (no auto-declare side-effect)
   #:p-my-=))

(in-package :pcl)

;;; ============================================================
;;; Compile-Time Definition Macros
;;; ============================================================
;;; These macros wrap definitions in eval-when to make them available
;;; at compile time. This matches Perl's semantics where subs and
;;; package variables are defined as they are parsed, allowing BEGIN
;;; blocks to call subs defined before them in source order.

;;; p-eval-direct: shorthand for (eval-when (:compile-toplevel :load-toplevel :execute) ...).
;;; Used throughout generated code to make definitions visible at all compilation phases.
(defmacro p-eval-direct (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

;;; Tracks how many PCL user subs deep we are (0 = top level).
;;; Used by p-caller to distinguish "called from a sub" vs "top level".
(defvar *pcl-sub-call-depth* 0)

;;; p-defpackage: Create/update a Perl package namespace.
;;; Wraps defpackage in eval-when so it runs at compile time (needed so that
;;; subsequent in-package forms can find the package during compile-file), and
;;; in handler-bind to suppress SBCL's "package at variance" warnings that fire
;;; when p-sub's compile-time shadow calls have already added symbols to the
;;; shadow list before defpackage re-evaluates at load time.
(defmacro p-defpackage (name &rest options)
  "Create/update a Perl package. Defaults to (:use :cl :pcl) when no options given."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (handler-bind ((warning #'muffle-warning))
       (defpackage ,name ,@(or options '((:use :cl :pcl)))))))

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
                 (let ((*pcl-sub-call-depth* (1+ *pcl-sub-call-depth*)))
                   (catch :p-return
                     ,@body))))))))

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

;;; p-our: Declare a package variable (Perl's 'our').
;;; Declaration happens at compile time (visible to BEGIN blocks).
;;; Initialization (if any) happens at runtime (after all BEGIN blocks).
;;; This matches Perl where 'our $x = 1' declares at compile, assigns at runtime.
(defmacro p-our (name &optional (init nil init-supplied-p))
  (if init-supplied-p
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (defvar ,name))
         (setf ,name ,init))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defvar ,name))))

;;; p-my: Declare a lexical variable at file scope (Perl's top-level 'my').
;;; Same semantics as p-our: declaration at compile time, init at runtime.
;;; Note: Inside subs, 'my' uses regular let bindings, not this macro.
(defmacro p-my (name &optional (init nil init-supplied-p))
  (if init-supplied-p
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (defvar ,name))
         (setf ,name ,init))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defvar ,name))))

;;; Forward declarations to avoid style warnings
(declaim (ftype (function (t) t) to-number to-string unbox p-get-stream))
;;; Forward-declare functions defined later in the file to suppress SBCL
;;; STYLE-WARNING: "undefined function" during compilation.
(declaim (ftype (function (t) t)
                object-address looks-like-number
                p-typeglob-p p-typeglob-name p-typeglob-package
                p-regex-match-p p-regex-match-pattern p-regex-match-modifiers
                clos-class-to-pkg perl-pkg-to-clos-class
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

;;; Persistent transpiler subprocess for p-eval
(defvar *p-transpiler-process* nil
  "Persistent pl2cl --server process, or nil if not yet started.
   Started lazily on first p-transpile-string call. Restarted if it dies.")

;;; Sub declaration/definition tracking for exists &sub / defined &sub
;;; Maps CL function symbol → :stub (declared only), :defined (has body),
;;; or :was-defined (was defined, now undef'd).
(defvar *p-declared-subs* (make-hash-table :test 'eq)
  "Perl sub existence tracking for exists &sub and defined &sub")

;;; @_ - Perl's subroutine arguments array. Must be declared special so that
;;; let-bindings in p-sub are dynamic (not lexical), allowing callbacks and
;;; closures like $SIG{__WARN__} handlers to access the caller's @_ via dynamic scope.
(defvar @_ (make-array 0 :adjustable t :fill-pointer 0)
  "Perl @_ - current subroutine arguments")

;;; Regex capture group variables ($1, $2, ... $9) and named captures (%+)
(defvar $1 nil "Regex capture group 1")
(defvar $2 nil "Regex capture group 2")
(defvar $3 nil "Regex capture group 3")
(defvar $4 nil "Regex capture group 4")
(defvar $5 nil "Regex capture group 5")
(defvar $6 nil "Regex capture group 6")
(defvar $7 nil "Regex capture group 7")
(defvar $8 nil "Regex capture group 8")
(defvar $9 nil "Regex capture group 9")
(defvar %+ (make-hash-table :test 'equal) "Perl %+ - named regex captures")

;;; Default variable ($_) - defined later after make-p-box (see Boxed special variables section)

;;; Process ID ($$)
(defvar $$ (sb-posix:getpid) "Process ID")

;;; Child exit status ($?)
(defvar $? 0 "Child process exit status from last system/backtick")

;;; Input line number ($.)
(defvar |$.| 0 "Input line number of last filehandle read")

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

;;; Perl version ($^V) - we report as PCL
(defvar |$^V| "v5.30.0" "Perl version (compatibility)")

;;; Perl executable path ($^X) - use sbcl since we're transpiled
(defvar |$^X| (or (car sb-ext:*posix-argv*) "sbcl") "Perl executable path")

;;; Taint mode flag (${^TAINT}) - always off in transpiled code
(defvar |${^TAINT}| nil "Taint mode is not enabled")

;;; System error ($!) - returns errno as string
(defun p-errno-string ()
  "Return the current system error message (like Perl's $!)"
  (let ((errno (sb-alien:get-errno)))
    (if (zerop errno)
        ""
        (or (sb-int:strerror errno)
            (format nil "Unknown error ~D" errno)))))

;;; Wantarray context variable
(defvar *wantarray* nil "True when list context is expected")

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
   - class: blessed class name"
  value
  (nv nil)
  (nv-ok nil)
  (sv nil)
  (sv-ok nil)
  (class nil))

(defun make-p-box (value &optional class)
  "Create a p-box, pre-caching if value is already typed"
  (let ((box (%make-p-box :value value :class class)))
    (typecase value
      (number (setf (p-box-nv box) value (p-box-nv-ok box) t))
      (string (setf (p-box-sv box) value (p-box-sv-ok box) t)))
    box))

;;; ============================================================
;;; Tie proxy — stored inside a p-box when the variable is tied
;;; ============================================================
;;; When tie() is called on a scalar, the box's value slot is replaced
;;; with a p-tie-proxy.  unbox() calls FETCH; box-set() calls STORE.
;;; The saved-value is restored when untie() is called.

(defstruct p-tie-proxy
  "Holds the tie object and the pre-tie value for a tied scalar."
  tie-obj       ; object returned by TIESCALAR/TIEARRAY/TIEHASH
  saved-value)  ; p-box-value before tie was installed (restored on untie)

(defun unbox (val)
  "Extract value from a box, or return val if not boxed.
   If the box contains a p-tie-proxy, dispatches to FETCH."
  (if (p-box-p val)
      (let ((v (p-box-value val)))
        (if (p-tie-proxy-p v)
            (unbox (p-method-call (p-tie-proxy-tie-obj v) "FETCH"))
            v))
      val))

(defun ensure-boxed (val)
  "Ensure a value is boxed"
  (if (p-box-p val)
      val
      (make-p-box val)))

;;; Boxed special variables (must be after make-p-box definition)
;;; Default variable ($_) - p-box so p-scalar-= / box-set work correctly
(defvar $_ (make-p-box nil) "Perl's $_ - default variable")
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
(defvar |$~| (make-p-box "") "FORMAT_NAME - name of current report format for write")
(defvar |$=| (make-p-box 60) "FORMAT_LINES_PER_PAGE - page length for write")
(defvar |$-| (make-p-box 0) "FORMAT_LINES_LEFT - lines left on page for write")
(defvar |$%| (make-p-box 0) "FORMAT_PAGE_NUMBER - current page number for write")
(defvar |$:| (make-p-box " \n-") "FORMAT_LINE_BREAK_CHARACTERS - word-break chars for write")
(defvar |$^L| (make-p-box (string #\Page)) "FORMAT_FORMFEED - formfeed char for write")
(defvar |$^A| (make-p-box "") "ACCUMULATOR - for formline/write output")
(defvar |$^| (make-p-box "") "FORMAT_TOP_NAME - top-of-page format name")
;; %SIG: signal/exception handler hash
;; __WARN__ and __DIE__ keys hold Perl callbacks invoked by warn/die.
(defvar %SIG (make-hash-table :test 'equal) "Perl %SIG - signal handlers")

(defun get-input-record-separator ()
  "Get the current value of $/ (unboxed).
   Returns nil for undef (slurp mode) or when $/ is a reference."
  (let ((val (unbox |$/|)))
    (cond
      ((eq val *p-undef*) nil)
      ;; $/ = \N (reference to number) means record mode — chomp does nothing
      ((p-box-p val) nil)
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
  ;; Tied variable: delegate to STORE
  (let ((current (p-box-value box)))
    (when (p-tie-proxy-p current)
      (return-from box-set
        (p-method-call (p-tie-proxy-tie-obj current) "STORE"
                        (if (p-box-p value) (unbox value) value)))))
  (let ((v (if (p-box-p value)
               (let ((inner (p-box-value value)))
                 ;; If inner is a box, this is a reference - preserve it
                 (if (p-box-p inner) value inner))
               value)))
    ;; Perl: @arr in scalar context gives element count.
    ;; A raw adjustable vector (bare @arr, not wrapped in make-p-box) in a scalar assignment
    ;; becomes the count. But (make-p-box arr) = array ref must stay as-is.
    (when (and (not (p-box-p value))   ; unwrapped raw vector only
               (vectorp v)
               (not (stringp v))
               (adjustable-array-p v))
      (setf v (length v)))
    (setf (p-box-value box) v
          (p-box-nv-ok box) nil
          (p-box-sv-ok box) nil)
    ;; Perl: assigning to a scalar resets pos()
    (remhash box *p-match-pos*)
    ;; Preserve class from blessed boxes
    (when (and (p-box-p value) (p-box-class value))
      (setf (p-box-class box) (p-box-class value)))
    (typecase v
      (number (setf (p-box-nv box) v (p-box-nv-ok box) t))
      (string (setf (p-box-sv box) v (p-box-sv-ok box) t)))
    box))

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
          (let ((lower (string-downcase check)))
            (when (or (string= lower "inf")
                      (string= lower "infinity"))
              (return-from parse-perl-number
                (if (minusp sign)
                    sb-ext:double-float-negative-infinity
                    sb-ext:double-float-positive-infinity)))
            (when (string= lower "nan")
              (return-from parse-perl-number
                ;; SBCL NaN: construct via 0d0/0d0 won't work, use bit pattern
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
            (let ((num-str (subseq trimmed 0 end)))
              (multiple-value-bind (n pos)
                  (let ((*read-eval* nil))
                    (ignore-errors
                      (read-from-string num-str)))
                (declare (ignore pos))
                (when (numberp n)
                  (return-from parse-perl-number n)))))))))
  0)

(defun object-address (obj)
  "Get a unique address/ID for an object (implementation-dependent)"
  #+sbcl (sb-kernel:get-lisp-obj-address obj)
  #-sbcl (sxhash obj))  ; Fallback: use hash as pseudo-address

(defun box-nv (box)
  "Get numeric value from box with lazy caching.
   Tied variables: bypass cache and call FETCH."
  (let ((inner (p-box-value box)))
    (when (p-tie-proxy-p inner)
      (return-from box-nv
        (to-number (p-method-call (p-tie-proxy-tie-obj inner) "FETCH")))))
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
                   ((p-typeglob-p v) 0)  ; typeglob as number
                   (t 0))))
          (setf (p-box-nv box) n
                (p-box-nv-ok box) t)
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
              ;; Fixed notation: strip trailing zeros then trailing dot
              (let ((s (format nil "~F" v)))
                (string-right-trim "." (string-right-trim "0" s)))
              ;; Exponential notation: use write-to-string + cleanup
              (let* ((*read-default-float-format* (type-of v))
                     (s (write-to-string v))
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
    ;; Lists (from return lists, etc.) - join with spaces like Perl's @array interpolation
    ((listp v) (format nil "~{~A~^ ~}" (mapcar #'to-string v)))
    ;; CL's T from comparison operators - Perl true stringifies to "1"
    ((eq v t) "1")
    (t (format nil "~A" v))))

(defun box-sv (box)
  "Get string value from box with lazy caching.
   Tied variables: bypass cache and call FETCH."
  (let ((inner (p-box-value box)))
    (when (p-tie-proxy-p inner)
      (return-from box-sv
        (to-string (p-method-call (p-tie-proxy-tie-obj inner) "FETCH")))))
  (if (p-box-sv-ok box)
      (p-box-sv box)
      (let* ((inner (p-box-value box))
             (class (or (p-box-class box)
                        (when (hash-table-p inner)
                          (gethash :__class__ inner))))
             (raw (stringify-value inner))
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

(defun p-defined (val)
  "Check if value is defined (not undef) - auto-unboxes.
   Both *p-undef* and nil count as undefined."
  (let ((v (unbox val)))
    (and (not (null v))
         (not (eq v *p-undef*)))))

(defun p-true-p (val)
  "Perl truthiness: false if undef, 0, empty string, empty list, or nil"
  (let ((v (unbox val)))
    (cond
      ((eq v *p-undef*) nil)
      ((null v) nil)
      ((and (numberp v) (zerop v)) nil)
      ((and (stringp v) (string= v "")) nil)
      ((and (stringp v) (string= v "0")) nil)
      ;; Empty vector (empty list in list context) is false
      ((and (vectorp v) (not (stringp v)) (zerop (length v))) nil)
      (t t))))

;;; ============================================================
;;; Arithmetic Operators
;;; ============================================================

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

(defun p-+ (&rest args)
  "Perl addition"
  (apply #'+ (mapcar #'to-number args)))

(defun p-- (&rest args)
  "Perl subtraction / unary minus.
   String negation rules:
   - Pure number string: negate numerically
   - Starts with letter/underscore: prepend '-'
   - Starts with +/-: flip sign char (string operation)
   - Otherwise: negate numerically"
  (if (= (length args) 1)
      (let ((val (unbox (first args))))
        (if (and (stringp val)
                 (> (length val) 0)
                 (not (looks-like-number val)))
            ;; Not a pure number — do string operations
            (let ((ch (char val 0)))
              (cond
                ((char= ch #\-) (concatenate 'string "+" (subseq val 1)))
                ((char= ch #\+) (concatenate 'string "-" (subseq val 1)))
                ;; Perl string negation only for ASCII alpha/underscore
                ;; Non-ASCII alphabetic chars (e.g. Ā) negate numerically to 0
                ((or (and (alpha-char-p ch) (< (char-code ch) 128)) (char= ch #\_))
                 (concatenate 'string "-" val))
                ;; Starts with digit but not a pure number (e.g. "12foo")
                ;; Perl negates numerically using leading portion
                (t (- (to-number (first args))))))
            ;; Numeric negation
            (- (to-number (first args)))))
      (apply #'- (mapcar #'to-number args))))

(defun p-* (&rest args)
  "Perl multiplication"
  (apply #'* (mapcar #'to-number args)))

(defun p-/ (a b)
  "Perl division"
  (/ (to-number a) (to-number b)))

(defun p-% (a b)
  "Perl modulo"
  (mod (truncate (to-number a)) (truncate (to-number b))))

(defun p-** (a b)
  "Perl exponentiation - returns Inf on overflow like Perl"
  (let ((na (to-number a))
        (nb (to-number b)))
    (handler-case
        (let ((result (expt (coerce na 'double-float) (coerce nb 'double-float))))
          result)
      (floating-point-overflow ()
        (if (and (realp na) (minusp na) (integerp nb) (oddp (truncate nb)))
            sb-ext:double-float-negative-infinity
            sb-ext:double-float-positive-infinity)))))

(defun p-int (val)
  "Perl int - truncate toward zero"
  (truncate (to-number val)))

(defun p-abs (val)
  "Perl abs - absolute value"
  (abs (to-number val)))

(defun p-sin (val)
  "Perl sin - sine"
  (sin (coerce (to-number val) 'double-float)))

(defun p-cos (val)
  "Perl cos - cosine"
  (cos (coerce (to-number val) 'double-float)))

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
    (when (zerop n)
      (error "Can't take log of 0"))
    (log (coerce n 'double-float))))

(defun p-sqrt (val)
  "Perl sqrt - square root"
  (let ((n (to-number val)))
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
        (t 0))))

;;; ============================================================
;;; String Operators
;;; ============================================================

(defun p-. (&rest args)
  "Perl string concatenation"
  (apply #'concatenate 'string (mapcar #'to-string args)))

(defun p-string-concat (&rest args)
  "Perl string concatenation (alias for interpolation)"
  (apply #'p-. args))

(defun p-str-x (str count)
  "Perl string repetition operator (x).
   If str is an array (adjustable vector), uses its length (scalar context)."
  (let* ((v (unbox str))
         ;; If it's an adjustable array (Perl @array), use its length
         ;; Regular strings are also vectors in CL, so check adjustable-array-p
         (s (if (and (vectorp v) (not (stringp v)) (adjustable-array-p v))
                (write-to-string (length v))
                (to-string str)))
         (n (truncate (to-number count))))
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
  "Perl length function - returns undef for undef input"
  (let ((v (unbox val)))
    (if (or (eq v *p-undef*) (null v))
        *p-undef*
        (length (to-string v)))))

(defun p-substr (str start &optional len replacement)
  "Perl substr function.
   2-3 args: extract substring.
   4 args: replace in place (if str is a box), return replaced portion.
   Negative start: count from end. Negative length: stop that many chars before end."
  (let* ((s (to-string str))
         (slen (length s))
         (st (truncate (to-number start)))
         ;; Handle negative start: count from end
         (st (if (< st 0) (max 0 (+ slen st)) st))
         (ln-raw (if len (truncate (to-number len)) nil))
         ;; Calculate end position, handling negative length
         (end-pos (cond ((null ln-raw) slen)
                        ((< ln-raw 0) (max st (+ slen ln-raw)))
                        (t (min (+ st ln-raw) slen)))))
    (if replacement
        ;; 4-arg form: replace and return the replaced portion
        (let* ((replaced-part (subseq s (min st slen) end-pos))
               (new-str (concatenate 'string
                                     (subseq s 0 (min st slen))
                                     (to-string replacement)
                                     (subseq s end-pos))))
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

(defun p-chr (n)
  "Perl chr - character from code point.
   Out-of-range values (negative or > 1114111) return U+FFFD replacement char."
  (let ((code (truncate (to-number n))))
    (if (or (< code 0) (> code #x10FFFF))
        (string #\REPLACEMENT_CHARACTER)  ; U+FFFD
        (string (code-char code)))))

(defun p-ord (str)
  "Perl ord - code point of first character"
  (let ((s (to-string str)))
    (if (> (length s) 0)
        (char-code (char s 0))
        0)))

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

(defun p-pos (var &optional new-pos)
  "Perl pos - get/set match position for /g regex.
   With one arg, returns current position (or nil).
   With two args, sets position and returns new-pos."
  (if new-pos
      ;; Setter: pos($str) = N
      (if (p-box-p var)
          (setf (gethash var *p-match-pos*) (truncate (to-number new-pos)))
          new-pos)
      ;; Getter: pos($str)
      (if (p-box-p var)
          (gethash var *p-match-pos*)
          nil)))

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
   Precision 0 means no decimal point (Perl: sprintf '%.0f', 0 => '0', not '0.')."
  (let* ((prec (or precision 6))
         ;; Use CL format with precision - works correctly with any float type
         (s (string-left-trim " " (format nil "~,vF" prec (abs num)))))
    ;; CL produces trailing "." when prec=0 (e.g. "0."); Perl never does
    (if (zerop prec)
        (string-right-trim "." s)
        s)))

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
              ;; Strip trailing zeros, then trailing dot
              (let* ((trimmed (string-right-trim "0" s))
                     (trimmed (string-right-trim "." trimmed)))
                trimmed))))))

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
           (error "Cannot printf ~A with argument ~A" (format nil "%~A" type-char) (to-string val)))
         (let* ((code (truncate num))
                (ch (if (and (>= code 0) (<= code #x10FFFF))
                        (string (code-char code))
                        ""))
                (sign ""))
           (values (sprintf-apply-width ch (or width 0) left-justify nil sign)
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
                    (raw (sprintf-format-int (abs int-val) base upper-case-p alt-form))
                    ;; Sign handling
                    (sign (cond
                            ((minusp int-val) "-")
                            ((and (member type-lower '(#\d #\i)) force-sign) "+")
                            ((and (member type-lower '(#\d #\i)) space-sign) " ")
                            (t "")))
                    ;; Precision for integers: minimum digits (pad with zeros)
                    (result (if precision
                                ;; Precision = minimum digits, zero-pad the digits
                                (let* ((prefix (cond
                                                 ((and alt-form (= base 16))
                                                  (if upper-case-p "0X" "0x"))
                                                 ((and alt-form (= base 8)) "")
                                                 ((and alt-form (= base 2))
                                                  (if upper-case-p "0B" "0b"))
                                                 (t "")))
                                       (digit-str (if (> (length prefix) 0)
                                                      (subseq raw (length prefix))
                                                      raw))
                                       (padded (if (and (zerop precision) (zerop (abs int-val)))
                                                   ""
                                                   (if (> precision (length digit-str))
                                                       (concatenate 'string
                                                                    (make-string (- precision (length digit-str))
                                                                                 :initial-element #\0)
                                                                    digit-str)
                                                       digit-str))))
                                  (concatenate 'string prefix padded))
                                raw)))
               (values (sprintf-apply-width result (or width 0) left-justify
                                            (and zero-pad (null precision)) sign)
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

      ;; Unknown: output the specifier literally
      (otherwise
       (values (format nil "%~A" type-char) arg-idx)))))

(defvar *p-sprintf-caller* "sprintf"
  "Name of the calling function (sprintf or printf) for error messages.")

(defun p-sprintf (fmt &rest args)
  "Perl sprintf - full format string parser.
   Supports: %d %i %u %o %x %X %b %B %e %E %f %F %g %G %s %c %%
   Flags: - + 0 space #
   Width and precision: literal or * (from args)
   Positional: %N$type selects argument N (1-based)"
  ;; Flatten any vector args: splice/map/grep in list context returns a vector
  ;; which Perl flattens into argument lists.
  (let ((args (loop for arg in args
                    nconcing (let ((v (unbox arg)))
                               (if (and (vectorp v) (not (stringp v)))
                                   (coerce v 'list)
                                   (list arg))))))
  (let ((fmt-str (to-string fmt)))
    (with-output-to-string (out)
      (let ((i 0)
            (arg-idx 0)
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
                              (positional-idx nil))
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
                              (setf j (1+ peek))))
                          ;; Parse flags
                          (loop while (and (< j len) (find (char fmt-str j) "-+ 0#"))
                                do (setf flags (concatenate 'string flags
                                                            (string (char fmt-str j))))
                                   (incf j))
                          ;; Parse width
                          (cond
                            ((and (< j len) (char= (char fmt-str j) #\*))
                             (setf width (truncate (to-number (nth arg-idx args))))
                             (when (minusp width)
                               (setf flags (concatenate 'string flags "-"))
                               (setf width (- width)))
                             (incf arg-idx)
                             (incf j))
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
                               (setf precision (max 0 (truncate (to-number (nth arg-idx args)))))
                               (incf arg-idx)
                               (incf j))
                              (t
                               (let ((p 0) (has-digit nil))
                                 (loop while (and (< j len) (digit-char-p (char fmt-str j)))
                                       do (setf p (+ (* p 10) (digit-char-p (char fmt-str j))))
                                          (setf has-digit t)
                                          (incf j))
                                 (setf precision (if has-digit p 0))))))
                          ;; Skip size modifiers (l, h, q, L, etc.)
                          (loop while (and (< j len) (find (char fmt-str j) "lhqLzjt"))
                                do (incf j))
                          ;; Type character
                          (if (< j len)
                              (let ((type-char (char fmt-str j)))
                                (incf j) ; consume the type char
                                ;; For positional %N$type, use the fixed index;
                                ;; for sequential, use arg-idx and advance it.
                                (let ((call-idx (if positional-idx
                                                    positional-idx
                                                    arg-idx)))
                                  (multiple-value-bind (result new-arg-idx)
                                      (sprintf-one type-char flags width precision args call-idx)
                                    (write-string result out)
                                    (setf arg-idx (if positional-idx arg-idx new-arg-idx))
                                    (setf i j))))
                              ;; No type char found, output literally
                              (progn
                                (write-string (subseq fmt-str i j) out)
                                (setf i j))))))
                ;; Regular character
                (progn
                  (write-char c out)
                  (incf i))))))))))  ; extra ) closes outer (let ((args ...)))

(defun p-printf (&rest args)
  "Perl printf - formatted print (with optional filehandle)"
  (let ((fh *standard-output*)
        (fmt nil)
        (fmt-args nil))
    ;; Check for :fh keyword
    (when (and (>= (length args) 2) (eq (first args) :fh))
      (setf fh (p-get-stream (second args)))
      (setf args (cddr args)))
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
      ;; Normal assignment - use box-set which unboxes
      (let ((val (gensym "VAL")))
        `(let ((,val ,value))
           (unless (boundp ',place)
             (proclaim '(special ,place))
             (setf (symbol-value ',place) (make-p-box nil)))
           (box-set ,place ,val)))))

(defmacro p-my-= (place value)
  "Assign to a lexically-bound 'my' variable. Unlike p-scalar-=, does not
   auto-declare the variable as special — the enclosing let binding (emitted by
   _with_declarations in Parser.pm) already handles scoping. This makes the
   assignment intent explicit for other compiler backends reading the IR."
  `(box-set ,place ,value))

(defmacro p-array-= (place value)
  "Assign to an array variable (@arr). Clears and refills from value.
   Flattens nested vectors (but not strings), wraps elements in boxes."
  (let ((val (gensym "VAL")))
    `(let ((,val ,value))
       (unless (boundp ',place)
         (proclaim '(special ,place))
         (setf (symbol-value ',place) (make-array 0 :adjustable t :fill-pointer 0)))
       (setf (fill-pointer ,place) 0)
       (labels ((add-items (src)
                  (cond
                    ((stringp src)
                     (vector-push-extend (make-p-box src) ,place))
                    ((hash-table-p src)
                     (maphash (lambda (k v)
                                (vector-push-extend (make-p-box k) ,place)
                                (vector-push-extend (make-p-box (unbox v)) ,place))
                              src))
                    ((vectorp src)
                     (loop for item across src
                           do (if (and (vectorp item) (not (stringp item)))
                                  (add-items item)
                                  (let ((v (unbox item)))
                                    (vector-push-extend (make-p-box v) ,place)))))
                    ((listp src)
                     (loop for item in src
                           do (if (and (vectorp item) (not (stringp item)))
                                  (add-items item)
                                  (let ((v (unbox item)))
                                    (vector-push-extend (make-p-box v) ,place)))))
                    ;; Scalar (number, p-box, nil=undef) - wrap in a single-element array
                    (t
                     (when src
                       (vector-push-extend (make-p-box (unbox src)) ,place))))))
         (add-items ,val))
       ,place)))

(defmacro p-hash-= (place value)
  "Assign to a hash variable (%hash). Clears and repopulates from value.
   Wraps values in boxes for l-value semantics."
  (let ((val (gensym "VAL")))
    `(let ((,val ,value))
       (unless (boundp ',place)
         (proclaim '(special ,place))
         (setf (symbol-value ',place) (make-hash-table :test 'equal)))
       (clrhash ,place)
       (cond
         ((hash-table-p ,val)
          (maphash (lambda (k v)
                     (let ((unboxed (unbox v)))
                       (setf (gethash k ,place) (make-p-box unboxed))))
                   ,val))
         ((vectorp ,val)
          ;; Flatten nested vectors (e.g. from function returning a list in list context)
          (let ((flat (%p-flatten-list ,val)))
            (loop for i from 0 below (length flat) by 2
                  when (< (1+ i) (length flat))
                  do (let* ((v (aref flat (1+ i)))
                            (unboxed (unbox v)))
                       (setf (gethash (to-string (aref flat i)) ,place) (make-p-box unboxed)))))))
       ,place)))

;; Flatten a Perl-style value (vector/list/hash/scalar) to a flat vector
;; for use in list-assignment RHS. Hash tables expand to key-value pairs;
;; nested vectors are flattened (like p-array-= does).
(defun %p-flatten-list (src)
  (let ((result (make-array 8 :adjustable t :fill-pointer 0)))
    (labels ((add (item)
               (cond
                 ((hash-table-p item)
                  (maphash (lambda (k v)
                             (vector-push-extend (make-p-box k) result)
                             (vector-push-extend (if (p-box-p v) v (make-p-box v)) result))
                           item))
                 ((and (vectorp item) (not (stringp item)))
                  (loop for x across item do (add x)))
                 ((consp item)  ; consp, not listp — nil is listp but should be treated as undef scalar
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
                                 (hash-table-p inner))  ; hash ref
                             item   ; reference or blessed: preserve the box
                             inner))  ; plain scalar: snapshot value
                       item)
                   result)))))
      (add src))
    result))

(defmacro p-list-= (place value)
  "List destructuring assignment: (p-list-= (vector $a $b) expr).
   Each LHS element gets assigned from corresponding RHS position.
   Handles undef skip markers, arrays, hashes, and nested lvalues."
  (let ((vars (cdr place))
        (src (gensym "SRC"))
        (src-vec (gensym "SRC-VEC")))
    (let ((forms nil)
          (static-idx 0)
          (greedy-done nil))
      (dolist (var vars)
        (cond
          ;; Already consumed by greedy (array/hash) — subsequent vars get undef
          (greedy-done
           (push `(progn
                    (unless (boundp ',var)
                      (proclaim '(special ,var))
                      (setf (symbol-value ',var) (make-p-box nil)))
                    (box-set ,var *p-undef*))
                 forms))
          ;; Skip marker: (p-list-x ... N)
          ((and (listp var)
                (symbolp (car var))
                (string= (symbol-name (car var)) "P-LIST-X")
                (numberp (caddr var)))
           (incf static-idx (caddr var)))
          ;; Skip single undef placeholder: (p-undef), *p-undef*, or
          ;; (let ((*wantarray* t)) (p-undef)) wrapper from wantarray context
          ((or (eq var '*p-undef*)
               (and (listp var)
                    (symbolp (car var))
                    (string= (symbol-name (car var)) "P-UNDEF"))
               (and (listp var)
                    (eq (car var) 'let)
                    (= (length var) 3)
                    (listp (third var))
                    (symbolp (car (third var)))
                    (string= (symbol-name (car (third var))) "P-UNDEF")))
           (incf static-idx 1))
          ;; Array variable (@arr) - absorbs remaining elements
          ((and (symbolp var)
                (char= (char (symbol-name var) 0) #\@))
           (let ((idx static-idx))
             (push `(p-array-= ,var (subseq ,src-vec (min ,idx (length ,src-vec)))) forms))
           (setf greedy-done t))
          ;; Hash variable (%hash) - absorbs remaining elements in pairs
          ((and (symbolp var)
                (char= (char (symbol-name var) 0) #\%))
           (let ((idx static-idx))
             (push `(p-hash-= ,var (subseq ,src-vec (min ,idx (length ,src-vec)))) forms))
           (setf greedy-done t))
          ;; Scalar variable - auto-declare and assign
          ((symbolp var)
           (push `(progn
                    (unless (boundp ',var)
                      (proclaim '(special ,var))
                      (setf (symbol-value ',var) (make-p-box nil)))
                    (box-set ,var (if (< ,static-idx (length ,src-vec))
                                      (aref ,src-vec ,static-idx)
                                      *p-undef*)))
                 forms)
           (incf static-idx 1))
          ;; Other lvalue (hash/array access, etc.)
          (t
           (push `(p-setf ,var (if (< ,static-idx (length ,src-vec))
                                     (aref ,src-vec ,static-idx)
                                     *p-undef*))
                 forms)
           (incf static-idx 1))))
      `(let* ((,src ,value)
              (,src-vec (%p-flatten-list ,src)))
         ,@(nreverse forms)
         ;; Return RHS count (scalar context: () = LIST gives count of LIST)
         (make-p-box (length ,src-vec))))))

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
    ;; (p-gethash (p-gethash ... ) key) = value
    ((and (listp place)
          (eq (car place) 'p-gethash)
          (listp (cadr place))
          (eq (car (cadr place)) 'p-gethash))
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
    ;; vec as lvalue: (p-setf (p-vec str offset bits) val) -> (p-vec-set str offset bits val)
    ((and (listp place) (eq (car place) 'p-vec))
     (let ((str-place (cadr place))
           (offset    (caddr place))
           (bits      (cadddr place)))
       `(p-vec-set ,str-place ,offset ,bits ,value)))
    ;; substr as lvalue: (p-setf (p-substr str start len) val) -> (p-substr str start len val)
    ((and (listp place) (eq (car place) 'p-substr))
     (let ((args (cdr place)))
       `(p-substr ,@args ,value)))
    ;; Other complex place (fallback)
    (t `(box-set ,place ,value))))

(defmacro p-my (expr)
  "Perl my declaration - just returns the expression"
  expr)

(defmacro p-incf (place &optional (delta 1))
  "Perl += - works on boxed values, hash/array elements, and derefs"
  (if (and (listp place)
           (member (car place) '(p-gethash p-aref p-gethash-deref p-aref-deref)))
      ;; Hash/array element - use incf (these return raw values, not boxes)
      `(incf ,place (to-number ,delta))
      ;; Boxed scalar or scalar deref (p-$ / p-cast-$): read numerically, write back
      `(box-set ,place (+ (to-number ,place) (to-number ,delta)))))

(defmacro p-decf (place &optional (delta 1))
  "Perl -= - works on boxed values, hash/array elements, and derefs"
  (if (and (listp place)
           (member (car place) '(p-gethash p-aref p-gethash-deref p-aref-deref)))
      ;; Hash/array element - use decf (these return raw values, not boxes)
      `(decf ,place (to-number ,delta))
      ;; Boxed scalar or scalar deref (p-$ / p-cast-$): read numerically, write back
      `(box-set ,place (- (to-number ,place) (to-number ,delta)))))

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
      ((and (listp real-place)
            (member (car real-place) '(p-aref-box p-gethash-box)))
       `(let* ((,box ,real-place)
               (,old (unbox ,box)))
          (box-set ,box (perl-increment ,box))
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
                   (,old (if (null ,val) 0 ,val)))
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
      ;; Box-returning accessors (p-aref-box, p-gethash-box) - get box and modify it
      ((and (listp real-place)
            (member (car real-place) '(p-aref-box p-gethash-box)))
       `(let* ((,box ,real-place)
               (,old (to-number ,box)))
          (box-set ,box (1- ,old))
          ,old))
      ;; Traditional setf-able places (p-aref, p-gethash, etc)
      ((and (listp real-place)
            (member (car real-place) '(p-gethash p-aref p-gethash-deref p-aref-deref p-$ p-cast-$)))
       `(let ((,old ,real-place))
          (decf ,real-place)
          ,old))
      ;; Boxed scalar
      (t `(let ((,old (to-number ,real-place)))
            (box-set ,real-place (1- ,old))
            ,old)))))

;;; ------------------------------------------------------------
;;; Compound Assignment Operators
;;; ------------------------------------------------------------

(defmacro p-*= (place value)
  "Perl *= (multiply-assign)"
  `(box-set ,place (* (to-number ,place) (to-number ,value))))

(defmacro p-/= (place value)
  "Perl /= (divide-assign)"
  `(box-set ,place (/ (to-number ,place) (to-number ,value))))

(defmacro p-%= (place value)
  "Perl %= (modulo-assign)"
  `(box-set ,place (mod (truncate (to-number ,place)) (truncate (to-number ,value)))))

(defmacro p-**= (place value)
  "Perl **= (exponent-assign)"
  `(box-set ,place (expt (to-number ,place) (to-number ,value))))

(defmacro p-.= (place value)
  "Perl .= (concat-assign)"
  (let ((g (gensym "PLACE")))
    `(let ((,g ,place))
       (box-set ,g (concatenate 'string (to-string ,g) (to-string ,value))))))

(defmacro p-str-x= (place value)
  "Perl x= (repeat-assign)"
  (let ((s (gensym "S"))
        (n (gensym "N")))
    `(let ((,s (to-string ,place))
           (,n (truncate (to-number ,value))))
       (box-set ,place (if (<= ,n 0) ""
                           (apply #'concatenate 'string (make-list ,n :initial-element ,s)))))))

(defmacro p-bit-and= (place value)
  "Perl &= (bitwise-and-assign)"
  `(box-set ,place (p-bit-and ,place ,value)))

(defmacro p-bit-or= (place value)
  "Perl |= (bitwise-or-assign)"
  `(box-set ,place (p-bit-or ,place ,value)))

(defmacro p-bit-xor= (place value)
  "Perl ^= (bitwise-xor-assign)"
  `(box-set ,place (p-bit-xor ,place ,value)))

(defmacro p-<<= (place value)
  "Perl <<= (left-shift-assign)"
  `(box-set ,place (ash (truncate (to-number ,place)) (truncate (to-number ,value)))))

(defmacro p->>= (place value)
  "Perl >>= (right-shift-assign)"
  `(box-set ,place (ash (truncate (to-number ,place)) (- (truncate (to-number ,value))))))

(defmacro p-and-assign (place value)
  "Perl &&= (and-assign) - assigns value only if place is true.
   Returns the box (lvalue) to support chaining."
  (let ((p (gensym "P")))
    `(let ((,p ,place))
       (when (p-true-p ,p)
         (box-set ,p ,value))
       ,p)))

(defmacro p-or-assign (place value)
  "Perl ||= (or-assign) - assigns value only if place is false.
   Returns the box (lvalue) to support chaining."
  (let ((p (gensym "P")))
    `(let ((,p ,place))
       (unless (p-true-p ,p)
         (box-set ,p ,value))
       ,p)))

(defmacro p-//= (place value)
  "Perl //= (defined-or-assign) - assigns value only if place is undef.
   Returns the box (lvalue) to support chaining."
  (let ((p (gensym "P")))
    `(let ((,p ,place))
       (unless (p-defined ,p)
         (box-set ,p ,value))
       ,p)))

;;; ============================================================
;;; Numeric Comparison
;;; ============================================================

(defun p-== (a b)
  "Perl numeric equality"
  (= (to-number a) (to-number b)))

(defun p-!= (a b)
  "Perl numeric inequality"
  (/= (to-number a) (to-number b)))

(defun p-< (a b)
  "Perl numeric less than"
  (< (to-number a) (to-number b)))

(defun p-> (a b)
  "Perl numeric greater than"
  (> (to-number a) (to-number b)))

(defun p-<= (a b)
  "Perl numeric less than or equal"
  (<= (to-number a) (to-number b)))

(defun p->= (a b)
  "Perl numeric greater than or equal"
  (>= (to-number a) (to-number b)))

(defun p-<=> (a b)
  "Perl spaceship operator"
  (let ((na (to-number a))
        (nb (to-number b)))
    (cond ((< na nb) -1)
          ((> na nb) 1)
          (t 0))))

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
           (s-num-p (or (numberp s)
                        (and (stringp s)
                             (not (and (> (length s) 1) (char= (char s 0) #\0)))
                             (ppcre:scan "^[+-]?\\d+(\\.\\d+)?([Ee][+-]?\\d+)?$" s))))
           (e-num-p (or (numberp e)
                        (and (stringp e)
                             (not (and (> (length e) 1) (char= (char e 0) #\0)))
                             (ppcre:scan "^[+-]?\\d+(\\.\\d+)?([Ee][+-]?\\d+)?$" e))))
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
         (let ((ns (truncate (to-number s)))
               (ne (truncate (to-number e))))
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
;;; String Comparison
;;; ============================================================

(defun p-str-eq (a b)
  "Perl string equality (eq)"
  (string= (to-string a) (to-string b)))

(defun p-str-ne (a b)
  "Perl string inequality (ne)"
  (not (string= (to-string a) (to-string b))))

(defun p-str-lt (a b)
  "Perl string less than (lt)"
  (if (string< (to-string a) (to-string b)) t nil))

(defun p-str-gt (a b)
  "Perl string greater than (gt)"
  (if (string> (to-string a) (to-string b)) t nil))

(defun p-str-le (a b)
  "Perl string less than or equal (le)"
  (if (string<= (to-string a) (to-string b)) t nil))

(defun p-str-ge (a b)
  "Perl string greater than or equal (ge)"
  (if (string>= (to-string a) (to-string b)) t nil))

(defun p-str-cmp (a b)
  "Perl string comparison (cmp)"
  (let ((sa (to-string a))
        (sb (to-string b)))
    (cond ((string< sa sb) -1)
          ((string> sa sb) 1)
          (t 0))))

;;; ============================================================
;;; Chained Comparison
;;; ============================================================

(defun cmp-op-to-fn (op)
  "Convert comparison operator symbol to p- function symbol.
   Handles both raw symbols and quoted forms.
   e.g., < -> p-<, (quote <) -> p-<, eq -> p-eq"
  (let ((sym (if (and (consp op) (eq (car op) 'quote))
                 (cadr op)  ; extract symbol from (quote sym)
                 op)))
    (intern (format nil "P-~A" sym) :pcl)))

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
  "Perl 'xor' operator"
  (let ((ta (p-true-p a))
        (tb (p-true-p b)))
    (if (or (and ta (not tb)) (and (not ta) tb)) t nil)))

(defmacro p-// (a b)
  "Perl defined-or operator"
  (let ((tmp (gensym)))
    `(let ((,tmp ,a))
       (if (p-defined ,tmp) ,tmp ,b))))

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

(defun p-bit-and (a b)
  "Perl bitwise AND — string (char-by-char, truncates) or numeric"
  (if (or (p-string-bitwise-operand-p a) (p-string-bitwise-operand-p b))
      (p-string-bit-op a b #'logand t)
      (logand (truncate (to-number a)) (truncate (to-number b)))))

(defun p-bit-or (a b)
  "Perl bitwise OR — string (char-by-char, pads with NUL) or numeric"
  (if (or (p-string-bitwise-operand-p a) (p-string-bitwise-operand-p b))
      (p-string-bit-op a b #'logior nil)
      (logior (truncate (to-number a)) (truncate (to-number b)))))

(defun p-bit-xor (a b)
  "Perl bitwise XOR — string (char-by-char, pads with NUL) or numeric"
  (if (or (p-string-bitwise-operand-p a) (p-string-bitwise-operand-p b))
      (p-string-bit-op a b #'logxor nil)
      (logxor (truncate (to-number a)) (truncate (to-number b)))))

(defun p-bit-not (a)
  "Perl bitwise NOT - mask to 64 bits like Perl's UV"
  (logand (lognot (truncate (to-number a))) #xFFFFFFFFFFFFFFFF))

(defun p-<< (a b)
  "Perl left shift — clamp shift count to prevent SBCL bignum explosion"
  (let ((av (truncate (to-number a)))
        (bv (truncate (to-number b))))
    (if (>= (abs bv) 64) 0 (ash av bv))))

(defun p->> (a b)
  "Perl right shift — clamp shift count to prevent SBCL bignum explosion"
  (let ((av (truncate (to-number a)))
        (bv (truncate (to-number b))))
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

(defun p-aref (arr idx)
  "Perl array access (supports negative indices, works on vectors and lists).
   Returns the VALUE (unboxed if element is a box)."
  (let* ((a (unbox arr)))  ; Unbox if needed
    ;; If array is undef (from failed hash lookup etc), return undef
    (when (eq a *p-undef*)
      (return-from p-aref *p-undef*))
    (let* ((i (truncate (to-number idx)))
           (len (cond ((vectorp a) (length a))
                      ((listp a) (length a))
                      (t 0)))
           (actual-idx (if (< i 0) (+ len i) i)))
      (cond
        ((and (vectorp a) (>= actual-idx 0) (< actual-idx len))
         (let ((elem (aref a actual-idx)))
           ;; nil means deleted element — return undef
           (if (null elem) *p-undef* (unbox elem))))
        ((and (listp a) (>= actual-idx 0) (< actual-idx len))
         (let ((elem (nth actual-idx a)))
           (unbox elem)))
        (t *p-undef*)))))

(defun (setf p-aref) (value arr idx)
  "Setf expander for p-aref - allows assignment to array elements.
   Auto-extends array if index is beyond current length (Perl semantics).
   Stores values in boxes for l-value semantics. Returns the box."
  (let* ((i (truncate (to-number idx)))
         (len (if (vectorp arr) (length arr) 0))
         (actual-idx (if (< i 0) (+ len i) i)))
    (when (and (vectorp arr) (>= actual-idx 0))
      ;; Auto-extend array if needed (Perl autovivification)
      ;; Intermediate slots get nil (deleted marker) so exists returns false for them.
      (when (>= actual-idx len)
        (dotimes (n (1+ (- actual-idx len)))
          (vector-push-extend nil arr)))
      ;; Get or create box at this index
      (let ((box (aref arr actual-idx)))
        (unless (p-box-p box)
          (setf box (make-p-box nil))
          (setf (aref arr actual-idx) box))
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
   When idx is a vector (range result), returns a slice instead of a single element."
  (let ((arr (unbox ref)))
    (if (and (vectorp idx) (not (stringp idx)))
        (p-aslice arr idx)
        (p-aref arr idx))))

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
   Returns new-last-index."
  (let* ((a (unbox arr))
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
   Recognizes p-flatten-marker to flatten @array arguments."
  (dolist (item items)
    (let ((val (unbox item)))
      (cond
        ;; Flatten marker - push each element of the marked array
        ((p-flatten-marker-p val)
         (let ((src (p-flatten-marker-array val)))
           (when (vectorp src)
             (loop for elem across src do
                   ;; Unbox if element is boxed, then create new box
                   (let ((v (unbox elem)))
                     (vector-push-extend (make-p-box v) arr))))))
        ;; Regular value - wrap in box and push
        (t (vector-push-extend (make-p-box val) arr)))))
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
  ;; First expand any flatten markers into a flat list of VALUES (not boxes)
  (let ((flat-items
          (loop for item in items
                for val = (unbox item)
                if (p-flatten-marker-p val)
                  ;; Flatten marker - expand its array, unboxing elements
                  append (loop for elem across (p-flatten-marker-array val)
                               collect (unbox elem))
                else
                  ;; Regular value
                  collect val)))
    (let ((nitems (length flat-items)))
      ;; Make room with placeholder boxes
      (dotimes (i nitems)
        (vector-push-extend (make-p-box *p-undef*) arr))
      ;; Shift existing elements up
      (loop for i from (1- (length arr)) downto nitems
            do (setf (aref arr i) (aref arr (- i nitems))))
      ;; Insert new items at front (in boxes)
      (loop for i from 0
            for item in flat-items
            do (setf (aref arr i) (make-p-box item)))
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
         ;; Collect removed elements (unboxed)
         (removed (make-array len :adjustable t :fill-pointer len)))
    ;; Copy removed elements
    (loop for i from 0 below len
          do (setf (aref removed i)
                   (let ((elem (aref a (+ off i))))
                     (unbox elem))))
    ;; Flatten replacement items (arrays get flattened in Perl)
    (let ((flat-rep nil))
      (dolist (r replacements)
        (let ((v (unbox r)))
          (if (and (vectorp v) (not (stringp v)))
              (loop for el across v
                    do (push (unbox el) flat-rep))
              (push v flat-rep))))
      (setf flat-rep (nreverse flat-rep))
      (let* ((nrep (list-length flat-rep))
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
        ;; Insert replacements
        (loop for i from off
              for v in flat-rep
              do (setf (aref a i) (make-p-box v)))))
    (if *wantarray*
        removed
        (if (> (length removed) 0)
            (aref removed (1- (length removed)))
            *p-undef*))))

;;; ============================================================
;;; Data Structures - Hashes
;;; ============================================================

(defun p-gethash (hash key)
  "Perl hash access. Special handling for %ENV and %INC.
   Returns the VALUE (unboxed if element is a box)."
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
      (t
       (multiple-value-bind (val found) (gethash k h)
         (if (not found)
             *p-undef*
             ;; Unbox if stored as a box (l-value semantics)
             (unbox val)))))))

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
    (if (or (null h) (eq h *p-undef*))
        (let ((new-hash (make-hash-table :test 'equal)))
          (box-set ref new-hash)
          new-hash)
        h)))

(defun p-ensure-arrayref (ref)
  "Ensure ref (a p-box) contains an adjustable vector.
   If ref contains nil or undef, autovivify: create a vector and store it in the box.
   Returns the raw vector (not boxed). Used by autovivification macros."
  (let ((a (unbox ref)))
    (if (or (null a) (eq a *p-undef*))
        (let ((new-arr (make-array 0 :adjustable t :fill-pointer 0)))
          ;; Wrap in make-p-box so box-set does not treat it as scalar-context @arr.
          (box-set ref (make-p-box new-arr))
          new-arr)
        a)))

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
            ;; Autovivify: create new hash and store it
            (let ((new-hash (make-hash-table :test 'equal)))
              (setf (gethash k h) new-hash)
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
            ;; Autovivify: create new array and store it
            (let ((new-arr (make-array 0 :adjustable t :fill-pointer 0)))
              (setf (gethash k h) new-arr)
              new-arr))))))

(defun p-autoviv-aref-for-hash (arr idx)
  "Get array element, autovivifying to empty hash if missing.
   Handles boxes in array elements."
  (let* ((a (unbox arr))
         (i (truncate idx)))
    ;; Extend array if needed
    (when (>= i (length a))
      (loop for j from (length a) to i
            do (vector-push-extend (make-p-box *p-undef*) a)))
    (let* ((stored (aref a i))
           ;; Unbox if element is a box
           (val (unbox stored)))
      (if (hash-table-p val)
          val
          ;; Autovivify: create new hash and store it
          (let ((new-hash (make-hash-table :test 'equal)))
            (setf (aref a i) new-hash)
            new-hash)))))

(defun p-autoviv-aref-for-array (arr idx)
  "Get array element, autovivifying to empty array if missing.
   Handles boxes in array elements."
  (let* ((a (unbox arr))
         (i (truncate idx)))
    ;; Extend array if needed
    (when (>= i (length a))
      (loop for j from (length a) to i
            do (vector-push-extend (make-p-box *p-undef*) a)))
    (let* ((stored (aref a i))
           ;; Unbox if element is a box
           (val (unbox stored)))
      (if (vectorp val)
          val
          ;; Autovivify: create new array and store it
          (let ((new-arr (make-array 0 :adjustable t :fill-pointer 0)))
            (setf (aref a i) new-arr)
            new-arr)))))

(defun p-array-set (arr idx value)
  "Set array element, extending array if needed.
   Stores values in boxes for l-value semantics."
  (let* ((a (unbox arr))
         (i (truncate idx)))
    ;; Extend array if needed
    (when (>= i (length a))
      (loop for j from (length a) to i
            do (vector-push-extend (make-p-box *p-undef*) a)))
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
  "Perl hash ref access $ref->{key} - unbox the reference first"
  (p-gethash (unbox ref) key))

(defun (setf p-gethash-deref) (value ref key)
  "Setf expander for p-gethash-deref - autovivify ref to hash if undef, then set key"
  (setf (p-gethash (p-ensure-hashref ref) key) value))

(defun (setf p-aref-deref) (value ref idx)
  "Setf expander for p-aref-deref - autovivify ref to array if undef, then set element"
  (setf (p-aref (p-ensure-arrayref ref) idx) value))

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
   Flattens vectors (e.g. from %arr[...] kv-slice) in the pair list."
  (let ((flat (loop for item in pairs
                    if (and (vectorp item) (not (stringp item)))
                      append (coerce item 'list)
                    else
                      collect item))
        (h (make-hash-table :test 'equal)))
    (loop for (k v) on flat by #'cddr
          do (setf (gethash (to-string k) h) (make-p-box (unbox v))))
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
                 ;; Vector (array) - flatten its contents (unboxing then re-boxing)
                 ((vectorp e)
                  (loop for item across e
                        for val = (unbox item)
                        do (vector-push-extend (make-p-box val) result)))
                 ;; List - flatten its contents (unboxing then re-boxing)
                 ((listp e)
                  (loop for item in e
                        for val = (unbox item)
                        do (vector-push-extend (make-p-box val) result)))
                 ;; Scalar value - unbox first (avoids double-boxing Perl vars), then wrap
                 (t
                  (vector-push-extend (make-p-box (unbox e)) result)))))
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
             (if *wantarray* (vector) *p-undef*))
           (let ((val (aref collection i)))
             ;; Advance: set end-sentinel if this is the last element
             (if (>= (1+ i) n)
                 (setf (gethash collection *array-iterators*) n)
                 (setf (gethash collection *array-iterators*) (1+ i)))
             (if *wantarray*
                 (vector i (unbox val))
                 i)))))
    ;; Hash case
    ((hash-table-p collection)
     (multiple-value-bind (remaining exists-p)
         (gethash collection *hash-iterators*)
       ;; If not started yet, initialize iterator with all keys
       (unless exists-p
         (let ((keys nil))
           (maphash (lambda (k v) (declare (ignore v)) (push k keys)) collection)
           (setf remaining (nreverse keys))
           (setf (gethash collection *hash-iterators*) remaining)))
       ;; If remaining is empty, return exhaustion sentinel and reset iterator
       (if (null remaining)
           (progn
             (remhash collection *hash-iterators*)
             (if *wantarray* (vector) *p-undef*))
           ;; Return next key/val pair
           (let* ((key (car remaining))
                  (val (gethash key collection)))
             (setf (gethash collection *hash-iterators*) (cdr remaining))
             (if *wantarray*
                 (vector key (unbox val))
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
                  (vector-push-extend k result))
                collection)
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
       (dotimes (i n) (setf (aref result i) (unbox (aref collection i))))
       result))
    ;; Hash case
    ((hash-table-p collection)
     (remhash collection *hash-iterators*)
     (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
       (maphash (lambda (k v)
                  (declare (ignore k))
                  (vector-push-extend (unbox v) result))
                collection)
       result))
    ;; Neither
    (t (make-array 0 :adjustable t :fill-pointer 0))))

(defun p-exists (hash key)
  "Perl exists function"
  (multiple-value-bind (val found) (gethash (to-string key) hash)
    (declare (ignore val))
    found))

(defun p-delete (hash key)
  "Perl delete function for hashes - returns unboxed value"
  (let ((k (to-string key)))
    (multiple-value-bind (v found) (gethash k hash)
      (remhash k hash)
      (if found
          (unbox v)
          *p-undef*))))

(defun p-delete-array (arr idx)
  "Perl delete function for arrays.
   Sets element to nil (deleted marker) and returns the old value.
   Trims trailing nil slots (Perl shrinks array when last element deleted)."
  (let* ((a (unbox arr))
         (i (truncate (to-number idx)))
         (len (if (vectorp a) (length a) 0))
         (actual-idx (if (< i 0) (+ len i) i))
         (old-val (if (and (>= actual-idx 0) (< actual-idx len))
                      (let ((elem (aref a actual-idx)))
                        (if (p-box-p elem) (p-box-value elem) *p-undef*))
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
   Deletes multiple keys and returns a list of the deleted values."
  (let ((result (make-array (length keys) :adjustable t :fill-pointer 0)))
    (dolist (key keys)
      (let ((k (to-string key)))
        (vector-push-extend (gethash k hash *p-undef*) result)
        (remhash k hash)))
    result))

(defun p-delete-kv-hash-slice (hash &rest keys)
  "Perl delete for KV hash slices: delete %hash{k1, k2, ...}
   Deletes multiple keys and returns key-value pairs."
  (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
    (dolist (key keys)
      (let ((k (to-string key)))
        (vector-push-extend k result)
        (vector-push-extend (gethash k hash *p-undef*) result)
        (remhash k hash)))
    result))

(defun p-delete-array-slice (arr &rest indices)
  "Perl delete for array slices: delete @arr[i1, i2, ...]
   Sets elements to nil (deleted marker) and returns a list of the old values."
  (let* ((a (unbox arr))
         (result (make-array (length indices) :adjustable t :fill-pointer 0)))
    (dolist (idx indices)
      (let* ((i (truncate (to-number idx)))
             (len (if (vectorp a) (length a) 0))
             (old-val (if (and (>= i 0) (< i len))
                          (let ((elem (aref a i)))
                            (if (p-box-p elem) (p-box-value elem) *p-undef*))
                          *p-undef*)))
        (when (and (vectorp a) (>= i 0) (< i len))
          (setf (aref a i) nil))  ; nil = deleted marker
        (vector-push-extend old-val result)))
    result))

(defun p-stash (pkg-name)
  "Return package stash (symbol table) as a hash.
   This is a simplified stub - full implementation would mirror Perl's stash."
  (declare (ignore pkg-name))
  ;; Return an empty hash for now - stash manipulation is rarely essential
  (make-hash-table :test 'equal))

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
      (let ((next-tag (intern (format nil "NEXT-~A" label) :pcl))
            (redo-tag (intern (format nil "REDO-~A" label) :pcl))
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
        `(block ,iter-block
           (tagbody :redo ,@body :next)))))

(defmacro p-while (condition &rest body-and-keys)
  "Perl while loop with optional :label and :continue.
Uses tagbody/go instead of loop so that (return-from nil ...) from p-return
inside the loop body correctly exits the enclosing function, not just the loop.
CL's (loop) creates an implicit (block nil ...) that would intercept p-return."
  (multiple-value-bind (label continue-form body) (parse-loop-keys body-and-keys)
    (let ((block-name (or label (gensym "WHILE"))))
      `(block ,block-name
         (block nil    ; for unlabeled p-last
           (tagbody
             :next
             (unless (p-true-p ,condition) (return-from ,block-name ""))
             ,(make-loop-iteration-body label body)
             ,@(when continue-form (list continue-form))
             (go :next)))))))

(defmacro p-until (condition &body body)
  "Perl until loop"
  `(p-while (p-! ,condition) ,@body))

(defmacro p-for ((&optional init) (test) (&optional step) &rest body-and-keys)
  "Perl C-style for loop with optional :label.
Uses tagbody/go instead of loop — see p-while for rationale."
  (multiple-value-bind (label _continue body) (parse-loop-keys body-and-keys)
    (declare (ignore _continue))
    (let ((block-name (or label (gensym "FOR"))))
      `(block ,block-name
         ,init
         (block nil    ; for unlabeled p-last
           (tagbody
             :next
             (unless (p-true-p ,test) (return-from ,block-name ""))
             ,(make-loop-iteration-body label body)
             ,@(when step (list step))
             (go :next)))))))

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
          (vec (gensym))
          (raw (gensym))
          (i (gensym)))
      `(block ,block-name
         (let* ((,raw (let ((*wantarray* t)) ,list))  ; list in list-context; body keeps outer context
                (,vec (%p-flatten-for-list ,raw))
                (,i 0))
           (block nil    ; for unlabeled p-last
             (tagbody
               :next
               (when (>= ,i (length ,vec)) (return-from ,block-name ""))
               (let ((,var (ensure-boxed (aref ,vec ,i))))
                 (incf ,i)
                 ,(make-loop-iteration-body label body)
                 ,@(when continue-form (list continue-form)))
               (go :next))))))))

(defun p-return-value (val)
  "Prepare a value for return - unbox simple scalars but keep references intact."
  (cond
    ;; Not a box - return as-is (hash tables, arrays, etc.)
    ((not (p-box-p val)) val)
    ;; Box containing a reference (hash, array, function) - return the reference
    ((let ((v (p-box-value val)))
       (or (hash-table-p v) (vectorp v) (functionp v)))
     (p-box-value val))
    ;; Simple scalar box - return the unboxed value
    (t (unbox val))))

(defmacro p-return (&rest values)
  "Perl return - returns single value or list depending on args.
   Uses throw :p-return to bypass (block nil ...) from loops (for p-last),
   so return always exits the enclosing p-sub, not just the innermost loop."
  (if (null values)
      `(throw :p-return nil)
      (if (= (length values) 1)
          `(throw :p-return (p-return-value ,(car values)))
          `(throw :p-return
             (if *wantarray*
                 (vector ,@(mapcar (lambda (v) `(p-return-value ,v)) values))
                 (p-return-value ,(car (last values))))))))

(defmacro p-last (&optional label)
  "Perl last (break) - optionally with label to exit specific loop"
  (if label
      `(return-from ,label nil)
      `(return nil)))

(defun p-last-dynamic (label-name)
  "Dynamic (cross-function) labeled last: throws to LAST-<LABEL> catch tag.
Used e.g. by p-skip to implement Test::More's skip() which calls (last SKIP)."
  (throw (intern (format nil "LAST-~A" label-name) :pcl) nil))

(defmacro p-next (&optional label)
  "Perl next (continue) - optionally with label to continue specific loop"
  (if label
      `(throw ',(intern (format nil "NEXT-~A" label) :pcl) nil)
      `(go :next)))

(defmacro p-redo (&optional label)
  "Perl redo - optionally with label to redo specific loop"
  (if label
      `(throw ',(intern (format nil "REDO-~A" label) :pcl) nil)
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
      (setf fh (p-get-stream (second args)))
      (setf args (cddr args)))
    (dolist (arg args)
      (princ (to-string arg) fh))
    ;; Append output record separator $\ if set
    (let ((ors (unbox |$\\|)))
      (when (and (stringp ors) (plusp (length ors)))
        (princ ors fh)))
    t))

(defun p-say (&rest args)
  "Perl say (print with newline)"
  (let ((fh *standard-output*))
    ;; Check for :fh keyword to get the right stream
    (when (and (>= (length args) 2) (eq (first args) :fh))
      (setf fh (p-get-stream (second args))))
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

(defun p-warn-build-message (args)
  "Build warn message string per Perl semantics:
   - Non-scalar (ref): return as-is
   - Scalar with trailing newline: use as-is
   - Scalar without trailing newline: append 'at FILE line N.'
   - Empty string or no args: use $@ if set, else 'Warning: something's wrong'"
  (let* ((empty-or-no-args
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
            (format nil "~A~A~A~%" (to-string (unbox err))
                    #\Tab "...caught at unknown line 0."))
           ;; No $@ → default warning
           (t (format nil "Warning: something's wrong at unknown line 0.~%")))))
      ;; Single ref arg: return as-is
      ((and (= (length args) 1) (p-warn-is-reference (car args)))
       (car args))
      ;; Otherwise: stringify and append location if needed
      (t
       (let ((s (if (= (length args) 1)
                    (to-string (unbox (car args)))
                    (apply #'p-. args))))
         (if (and (> (length s) 0)
                  (char= (char s (1- (length s))) #\Newline))
             s
             (format nil "~A at unknown line 0.~%" s)))))))

(defun p-warn (&rest args)
  "Perl warn - respects $SIG{__WARN__} handler."
  (let* ((msg (p-warn-build-message args))
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
         (force-output *error-output*))))))

;;; Exception condition for object-based die
;;; When Perl dies with a blessed reference, we preserve it in $@
(define-condition p-exception (error)
  ((object :initarg :object :reader p-exception-object))
  (:report (lambda (c s)
             (format s "~A" (p-exception-object c)))))

(defun p-die (&rest args)
  "Perl die - throw an exception.
   If given a single blessed reference, throw it as an exception object.
   Otherwise, concatenate args as error string."
  (if (and (= (length args) 1)
           (let ((obj (car args)))
             ;; Check if it's a blessed hash or blessed box
             (or (and (hash-table-p obj) (gethash :__class__ obj))
                 (and (p-box-p obj)
                      (let ((inner (p-box-value obj)))
                        (or (p-box-class obj)
                            (and (hash-table-p inner) (gethash :__class__ inner))))))))
      ;; Object exception - preserve for $@
      (error 'p-exception :object (car args))
      ;; String exception
      (error (apply #'p-. args))))

;;; p-do - Perl's do BLOCK
;;; The block is already evaluated by CL, so this is identity.
(defun p-do (result)
  "Perl do BLOCK - returns the value of the block."
  result)

;;; p-eval: Perl eval(STRING) — full string eval via runtime transpilation.
;;;
;;; Variable access: eval sees defvar (dynamic) variables — package globals,
;;; our vars, local vars, and file-scope my vars. Sub-scope my vars are let-
;;; bound without defvar, so they are lexically scoped and correctly invisible
;;; to eval (matching Perl semantics). Closure-captured vars are renamed to
;;; $x__lex__N and also invisible. See docs/eval-string-plan.md.
;;;
;;; $@ format: omits " at (eval N) line M." — documented in not-supported.md.
(defun p-eval (string)
  "Perl eval(STRING): transpile and evaluate a Perl string at runtime."
  (let ((s (to-string (unbox string))))
    ;; eval undef / eval "" -> nil (undef), $@ = ""
    (when (string= s "")
      (box-set $@ "")
      (return-from p-eval nil))
    (let* ((pkg-name  (package-name *package*))
           (cache-key (cons s pkg-name))
           (cached    (gethash cache-key *p-eval-string-cache*)))
      (handler-case
          (let* ((cl-text  (or cached
                               (let ((r (p-transpile-string s pkg-name)))
                                 (setf (gethash cache-key
                                                *p-eval-string-cache*) r)
                                 r)))
                 ;; READ with *package* bound: symbol interning (e.g. $x)
                 ;; uses the eval package, matching the caller's symbol space.
                 (cl-form  (let ((*package* *package*))
                             (read-from-string
                              (concatenate 'string "(progn " cl-text ")"))))
                 ;; EVAL with *package* bound: any (in-package ...) in the
                 ;; eval'd code does not escape into the caller's dynamic scope.
                 (result   (let ((*package* *package*))
                             (eval cl-form))))
            (box-set $@ "")
            result)
        (p-exception (e)
          (box-set $@ (p-exception-object e))
          nil)
        (error (e)
          (box-set $@ (format nil "~A" e))
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
  `(handler-case
       (prog1 (progn ,@body)
         (box-set $@ ""))
     (p-exception (e)
       ;; Object exception - preserve the object in $@
       (box-set $@ (p-exception-object e))
       nil)
     (error (e)
       ;; String exception - convert to string
       (box-set $@ (format nil "~A" e))
       nil)))

;;; ============================================================
;;; File I/O Functions
;;; ============================================================

;; Filehandle storage - maps symbols to CL streams
(defvar *p-filehandles* (make-hash-table :test 'eq))

;; Standard filehandles
(setf (gethash 'STDIN *p-filehandles*) *standard-input*)
(setf (gethash 'STDOUT *p-filehandles*) *standard-output*)
(setf (gethash 'STDERR *p-filehandles*) *error-output*)

(defun p-get-stream (fh)
  "Get CL stream from Perl filehandle (symbol, box, or stream)"
  (cond
    ((streamp fh) fh)
    ((symbolp fh) (gethash fh *p-filehandles*))
    ((p-box-p fh)
     (let ((v (p-box-value fh)))
       (if (streamp v) v nil)))   ; only return if it IS a stream
    (t nil)))

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

(defun %p-open-impl (fh mode filename)
  "Implementation of Perl open"
  (let* ((mode-str (to-string mode))
         (file-str (to-string filename))
         (stream
           (cond
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
    (when stream
      (cond
        ((p-box-p fh) (box-set fh stream))
        (t             (setf (gethash fh *p-filehandles*) stream))))
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

(defun %p-close-impl (fh)
  "Implementation of Perl close"
  (cond
    ((p-box-p fh)
     (let ((stream (p-box-value fh)))
       (when (streamp stream)
         (close stream)
         (box-set fh *p-undef*)
         t)))
    (t
     (let ((stream (p-get-stream fh)))
       (when stream
         (close stream)
         (remhash fh *p-filehandles*)
         t)))))

(defmacro p-close (fh)
  "Perl close - close filehandle. Bareword is quoted; lexical $fh passed as box."
  `(%p-close-impl (%p-fh-arg ,fh)))

(defun %p-eof-impl (&optional fh)
  "Perl eof implementation — fh must already be a symbol or stream"
  (let ((stream (if fh (p-get-stream fh) *standard-input*)))
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
(defmacro %p-fh-arg (fh-form)
  (if (and (symbolp fh-form)
           (let ((name (symbol-name fh-form)))
             (and (plusp (length name))
                  (not (member (char name 0) '(#\$ #\@ #\% #\*))))))
      `',(intern (symbol-name fh-form))
      fh-form))

(defun %p-tell-impl (&optional fh)
  "Perl tell - return current file position"
  (let ((stream (if fh (p-get-stream fh) *standard-input*)))
    (if stream (file-position stream) -1)))

(defmacro p-tell (&rest args)
  "Perl tell — bareword filehandle is auto-quoted."
  (if args `(%p-tell-impl (%p-fh-arg ,(car args))) `(%p-tell-impl)))

(defun %p-seek-impl (fh pos whence)
  "Perl seek - seek to position. Whence: 0=start, 1=current, 2=end"
  (let ((stream (p-get-stream fh))
        (position (to-number pos))
        (w (to-number whence)))
    (when stream
      (let ((new-pos
              (cond
                ((= w 0) position)                              ; SEEK_SET
                ((= w 1) (+ (file-position stream) position))   ; SEEK_CUR
                ((= w 2) (+ (file-length stream) position))     ; SEEK_END
                (t position))))
        (file-position stream new-pos)))))

(defmacro p-seek (fh &rest args)
  "Perl seek — bareword filehandle is auto-quoted."
  `(%p-seek-impl (%p-fh-arg ,fh) ,@args))

(defun %p-binmode-impl (fh &optional encoding)
  "Perl binmode - set binary mode or encoding (stub)"
  (declare (ignore fh encoding))
  t)

(defmacro p-binmode (fh &rest args)
  "Perl binmode — bareword filehandle is auto-quoted."
  `(%p-binmode-impl (%p-fh-arg ,fh) ,@args))

(defun %p-read-impl (fh buf len &optional offset)
  "Perl read - read bytes into buffer. Returns nil on stream error."
  (declare (ignore buf offset))  ; Buffer semantics differ in CL
  (handler-case
    (let ((stream (p-get-stream fh))
          (n (to-number len)))
      (when stream
        (let ((result (make-string n)))
          (read-sequence result stream)
          result)))
    (error () nil)))

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
  "Perl syswrite - write data to filehandle"
  (let ((stream (p-get-stream fh))
        (str (to-string data)))
    (when stream
      (if len
          (write-string (subseq str 0 (min (to-number len) (length str))) stream)
          (write-string str stream))
      (length str))))

(defmacro p-syswrite (fh &rest args)
  "Perl syswrite — bareword filehandle is auto-quoted."
  `(%p-syswrite-impl (%p-fh-arg ,fh) ,@args))

(defun p-truncate (fh-or-file size)
  "Perl truncate - truncate file (limited support)"
  (declare (ignore fh-or-file size))
  ;; Standard CL doesn't support truncate - would need SBCL extension
  (warn "truncate not implemented in standard CL")
  nil)

(defun p-stat (file-or-fh)
  "Perl stat - get file status. Returns list of file info."
  (let ((path (if (streamp file-or-fh)
                  (pathname file-or-fh)
                  (to-string file-or-fh))))
    (if (probe-file path)
        ;; Return simplified stat: (dev ino mode nlink uid gid rdev size atime mtime ctime blksize blocks)
        ;; Most values are stubs since CL doesn't provide all of them
        (let ((write-date (file-write-date path)))
          (vector 0              ; dev
                  0              ; ino
                  #o644          ; mode (stub)
                  1              ; nlink
                  0              ; uid
                  0              ; gid
                  0              ; rdev
                  (with-open-file (s path) (file-length s))  ; size
                  write-date     ; atime
                  write-date     ; mtime
                  write-date     ; ctime
                  4096           ; blksize (stub)
                  0))            ; blocks (stub)
        nil)))

(defun p-lstat (file)
  "Perl lstat - stat without following symlinks (same as stat in CL)"
  (p-stat file))

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
   Note: Unlike CL's read-line, this keeps the trailing separator (like Perl)."
  (let ((stream (if fh (p-get-stream fh) *standard-input*))
        (sep (get-input-record-separator)))
    (when stream
      (cond
        ;; Slurp mode: $/ = undef - read entire file
        ((null sep)
         (let ((content (make-array 4096 :element-type 'character
                                         :adjustable t :fill-pointer 0)))
           (loop for char = (read-char stream nil nil)
                 while char
                 do (vector-push-extend char content))
           (if (zerop (length content)) nil (coerce content 'string))))

        ;; Paragraph mode: $/ = "" - read until blank line
        ((string= sep "")
         (let ((lines nil)
               (seen-content nil))
           (loop
             (multiple-value-bind (line missing-nl) (read-line stream nil nil)
               (declare (ignore missing-nl))
               (cond
                 ((null line)
                  (return (if lines
                              (format nil "~{~A~^~%~}~%" (nreverse lines))
                              nil)))
                 ((string= line "")
                  (if seen-content
                      (return (format nil "~{~A~^~%~}~%~%" (nreverse lines)))
                      nil))  ; Skip leading blank lines
                 (t
                  (setf seen-content t)
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
           (if (zerop (length result)) nil (coerce result 'string))))))))

(defmacro p-readline (&rest args)
  "Perl readline / <FH> — pass args through; code-gen already quotes barewords."
  `(%p-readline-impl ,@args))

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

(defun p-glob (&optional pattern)
  "Perl glob / <*.txt> - expand file glob pattern.
   In list context, returns vector of matching files.
   In scalar context, returns each match on successive calls."
  (let* ((pat (if pattern (to-string pattern) "*"))
         ;; Expand character ranges [a-c] -> [abc] for SBCL compatibility
         (expanded-pat (expand-glob-char-ranges pat))
         ;; Check if pattern is relative (doesn't start with /)
         (is-relative (not (and (> (length expanded-pat) 0) (char= (char expanded-pat 0) #\/))))
         ;; For relative paths, prepend current working directory
         (full-pat (if is-relative
                       (concatenate 'string (sb-posix:getcwd) "/" expanded-pat)
                       expanded-pat))
         ;; Extract directory prefix from original pattern for return values
         (dir-prefix (let ((slash-pos (position #\/ pat :from-end t)))
                       (if slash-pos (subseq pat 0 (1+ slash-pos)) "")))
         ;; Use SBCL's directory function with wild pathname
         (all-matches (handler-case
                          (directory (parse-namestring full-pat))
                        (error () nil)))
         ;; Filter out directories (pathname-name is nil for directories)
         (matches (remove-if (lambda (p) (null (pathname-name p))) all-matches)))
    (if *wantarray*
        ;; List context: return all matches as vector
        (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
          (dolist (path matches)
            ;; Return path relative to pattern's directory prefix
            (let ((name (file-namestring path)))
              (vector-push-extend (concatenate 'string dir-prefix name) result)))
          result)
        ;; Scalar context: return first match (simplified - full impl needs iterator state)
        (when matches
          (let ((name (file-namestring (car matches))))
            (concatenate 'string dir-prefix name))))))

;;; ============================================================
;;; File/Directory Operations
;;; ============================================================

(defun p-chdir (&optional dir)
  "Perl chdir - change current directory. Returns true on success.
   Also updates *default-pathname-defaults* for Lisp path resolution."
  (let ((path (if dir (to-string dir) (sb-posix:getenv "HOME"))))
    (handler-case
        (progn
          (sb-posix:chdir path)
          ;; Update *default-pathname-defaults* so relative paths resolve correctly
          (setf *default-pathname-defaults* (truename (pathname path)))
          t)
      (error () nil))))

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
  "Perl chmod - change file permissions. Returns count of successfully changed files."
  (let ((m (truncate (to-number mode)))
        (count 0))
    (dolist (f files count)
      (handler-case
          (progn (sb-posix:chmod (to-string f) m) (incf count))
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

(defun p-study (&optional str)
  "Perl study - deprecated no-op in modern Perl. Returns 1."
  (declare (ignore str))
  1)

(defun p-reset (&optional pattern)
  "Perl reset - reset ?? searches. No-op in PCL, returns 1."
  (declare (ignore pattern))
  1)

(defun p-vec (str offset bits)
  "Perl vec - treat string as bit vector and extract element.
   OFFSET is the element index, BITS is element size (1, 2, 4, 8, 16, 32).
   Returns the numeric value at that position."
  (let* ((s (to-string str))
         (offset (truncate (to-number offset)))
         (bits   (truncate (to-number bits))))
    (unless (member bits '(1 2 4 8 16 32))
      (p-die (format nil "Illegal number of bits in vec")))
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
   BITS must be 1, 2, 4, 8, 16, or 32. Negative OFFSET dies. Modifies str-box."
  (let* ((offset (truncate (to-number offset)))
         (bits   (truncate (to-number bits)))
         (val    (truncate (to-number value))))
    (unless (member bits '(1 2 4 8 16 32))
      (p-die "Illegal number of bits in vec"))
    (when (< offset 0)
      (p-die "Negative offset to vec in lvalue context"))
    (let* ((byte-offset   (floor (* offset bits) 8))
           (bit-offset    (mod (* offset bits) 8))
           (needed-bytes  (+ byte-offset (ceiling bits 8)))
           (s             (to-string str-box))
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
      val)))

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
  (let* ((unix-time (if time (truncate (to-number time)) (p-time))))
    (cond
      ((> unix-time +gmtime-max+)
       (p-warn (make-p-box (format nil "localtime(~A) too large" unix-time)))
       *p-undef*)
      ((< unix-time +gmtime-min+)
       (p-warn (make-p-box (format nil "localtime(~A) too small" unix-time)))
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
             (if *wantarray*
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
           (if *wantarray*
               (make-array 9 :initial-contents
                           (list sec min hour day perl-mon perl-year wday yday 0)
                           :adjustable t :fill-pointer t)
               (%pcl-format-time wday perl-mon day hour min sec (+ perl-year 1900)))))))))

(defun p-gmtime (&optional time)
  "Perl gmtime - convert time to UTC components.
   Same return format as localtime but in UTC.
   Warns and returns undef for out-of-range timestamps."
  (let* ((unix-time (if time (truncate (to-number time)) (p-time))))
    (cond
      ((> unix-time +gmtime-max+)
       (p-warn (make-p-box (format nil "gmtime(~A) too large" unix-time)))
       *p-undef*)
      ((< unix-time +gmtime-min+)
       (p-warn (make-p-box (format nil "gmtime(~A) too small" unix-time)))
       *p-undef*)
      (t
       (multiple-value-bind (sec min hour day perl-mon perl-year wday yday)
           (%pcl-unix-to-utc unix-time)
         (if *wantarray*
             (make-array 9 :initial-contents
                         (list sec min hour day perl-mon perl-year wday yday 0)
                         :adjustable t :fill-pointer t)
             (%pcl-format-time wday perl-mon day hour min sec (+ perl-year 1900))))))))

;;; ============================================================
;;; Process Control
;;; ============================================================

(defun p-pipe (read-fh write-fh)
  "Perl pipe - create pipe pair (not implemented, returns nil)"
  (declare (ignore read-fh write-fh))
  nil)

(defun p-select (&optional fh)
  "Perl select - set default output filehandle (stub, returns previous handle)"
  (declare (ignore fh))
  nil)

(defun p-exit (&optional code)
  "Perl exit - terminate program with exit code."
  (sb-ext:exit :code (if code (truncate (to-number code)) 0)))

(defun p-system (&rest args)
  "Perl system - execute a shell command.
   system(CMD) or system(PROG, ARGS...).
   Returns exit status (0 = success). In Perl, the actual exit code is
   return_value >> 8, but we return the raw exit code for simplicity."
  (if (null args)
      -1
      (let* ((cmd (to-string (car args))))
        (if (cdr args)
            ;; system(PROG, ARGS...) - run program directly with args
            (let* ((prog-args (mapcar #'to-string (cdr args)))
                   (proc (sb-ext:run-program cmd prog-args
                                             :search t
                                             :input nil
                                             :output *standard-output*
                                             :error *error-output*
                                             :wait t)))
              (ash (sb-ext:process-exit-code proc) 8))
            ;; system(CMD) - run through shell
            (let ((proc (sb-ext:run-program "/bin/sh" (list "-c" cmd)
                                            :input nil
                                            :output *standard-output*
                                            :error *error-output*
                                            :wait t)))
              (ash (sb-ext:process-exit-code proc) 8))))))

(defun p-backtick (cmd)
  "Perl backticks - execute shell command and capture output.
   Returns the stdout output as a string."
  (let* ((proc (sb-ext:run-program "/bin/sh" (list "-c" (to-string cmd))
                                   :input nil
                                   :output :stream
                                   :error nil
                                   :wait nil))
         (output (with-output-to-string (s)
                   (loop for line = (read-line (sb-ext:process-output proc) nil nil)
                         while line
                         do (write-line line s)))))
    (sb-ext:process-wait proc)
    ;; Remove trailing newline to match common Perl usage with chomp
    ;; Actually, Perl backticks DO include the newline, so keep it
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

;; Cache configuration
(defparameter *pcl-cache-dir*
  (merge-pathnames ".pcl-cache/" (user-homedir-pathname))
  "Directory for cached compiled modules")
(defparameter *pcl-cache-max-age* (* 7 24 60 60)
  "Max cache age in seconds (default: 1 week)")
(defparameter *pcl-skip-cache* nil
  "When true, bypass cache (set by --no-cache or PCL_NO_CACHE)")
(defparameter *pcl-cache-fasl* t
  "When true, cache compiled FASL; when nil, cache .lisp for debugging")
(defparameter *pcl-pl2cl-path* nil
  "Path to pl2cl script (set at load time)")

;; Track modules currently being loaded (for circular dependency detection)
(defvar *p-loading-modules* nil
  "Stack of modules currently being loaded")

;;; --- Module Path Utilities ---

(defun p-module-to-path (module-name)
  "Convert Perl module name to relative path.
   Foo::Bar => Foo/Bar.pm
   Foo/Bar.pm => Foo/Bar.pm (unchanged)"
  (let ((name (to-string module-name)))
    (if (search ".pm" name)
        name
        (concatenate 'string
                     (substitute #\/ #\: name)
                     ".pm"))))

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
  (let ((cache-path (p-compute-cache-path source-path (not *pcl-cache-fasl*))))
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
   Tries: uppercase name, pipe-quoted name (for Foo::Bar)."
  (or (find-package (string-upcase module-name))
      (find-package (format nil "|~A|" module-name))))

(defun p-perl-symbol-to-cl-name (sym-name)
  "Convert Perl symbol name to CL symbol name.
   '$x' -> '$X', '@arr' -> '@ARR', '%hash' -> '%HASH', 'func' -> 'PL-FUNC'
   Note: CL uppercases symbols by default."
  ;; Unbox if sym-name is a box (from @EXPORT array with l-value storage)
  (let* ((name (unbox sym-name))
         (first-char (if (plusp (length name))
                         (char name 0)
                         nil)))
    (string-upcase
     (cond
       ((eql first-char #\$) name)
       ((eql first-char #\@) name)
       ((eql first-char #\%) name)
       (t (format nil "pl-~A" name))))))

(defun p-import-perl-symbol (sym-name from-pkg to-pkg)
  "Import a Perl symbol from FROM-PKG to TO-PKG.
   Handles sigils appropriately."
  (let* ((cl-name (p-perl-symbol-to-cl-name sym-name))
         (sym (find-symbol cl-name from-pkg)))
    (when sym
      (shadowing-import sym to-pkg))))

(defun p-import-exports (module-name to-pkg &optional specific-imports)
  "Import symbols from module's @EXPORT (or specific list) into TO-PKG."
  (let ((pkg (p-find-module-package module-name)))
    (when pkg
      (let ((imports (or specific-imports
                         ;; Get @EXPORT from module's package
                         (let ((export-sym (find-symbol "@EXPORT" pkg)))
                           (when (and export-sym (boundp export-sym))
                             (let ((val (symbol-value export-sym)))
                               (when (and val (vectorp val))
                                 (coerce val 'list))))))))
        (dolist (sym-name imports)
          (p-import-perl-symbol sym-name pkg to-pkg))))))

(defparameter *p-xs-only-modules*
  '("XSLoader" "DynaLoader" "Carp::Heavy")
  "Modules that use XS/C code and cannot be transpiled. Skip loading them.")

(defun p-use (module-name &key imports)
  "Perl use - load module at compile time and import symbols.
   MODULE-NAME: 'Foo::Bar' or 'Foo/Bar.pm'
   IMPORTS: list of symbols to import (nil = use @EXPORT, empty list = no imports)"
  ;; Skip XS-only modules that cannot be transpiled
  (when (member module-name *p-xs-only-modules* :test #'string=)
    (return-from p-use t))
  (let ((rel-path (p-module-to-path module-name))
        (caller-pkg *package*))
    ;; Already loaded?
    (when (gethash rel-path *p-inc-table*)
      ;; Still import symbols for repeated use statements
      (unless (and imports (null imports))
        (p-import-exports module-name caller-pkg imports))
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
      ;; Import symbols from module
      (unless (and imports (null imports))
        (p-import-exports module-name caller-pkg imports))
      t)))

(defun p-require (module-name)
  "Perl require - load module at runtime (no imports)."
  (p-use module-name))

(defun p-require-file (path)
  "Perl require with file path - load a .pl file by path.
   Resolves relative paths against current directory."
  (let* ((path-str (unbox path))
         ;; Check if already loaded (Perl tracks this in %INC by path)
         (abs-path (if (char= (char path-str 0) #\/)
                       path-str
                       ;; Relative path - resolve against current dir
                       (merge-pathnames path-str (truename *default-pathname-defaults*)))))
    ;; Check %INC to avoid reloading
    (when (gethash path-str *p-inc-table*)
      (return-from p-require-file t))
    ;; Load the file using pl2cl
    (unless (probe-file abs-path)
      (error "Can't locate ~A" path-str))
    ;; Transpile and load
    (p-load-module-cached abs-path)
    ;; Update %INC
    (setf (gethash path-str *p-inc-table*) (namestring abs-path))
    t))

;;; ============================================================
;;; List Functions
;;; ============================================================

(defun %p-collect-list (&rest items)
  "Collect &rest args into a flat vector.
   Pl-boxes wrapping vectors (@arrays) are flattened into individual elements.
   Used by p-map and p-grep to handle both (fn @arr) and (fn a b c) forms."
  (let ((result (make-array 8 :adjustable t :fill-pointer 0)))
    (dolist (item items)
      (let ((val (unbox item)))
        (if (and (vectorp val) (not (stringp val)))
            (loop for x across val do (vector-push-extend x result))
            (vector-push-extend item result))))
    result))

(defun p-grep (fn &rest items)
  "Perl grep - fn receives item as $_ parameter.
   Accepts (fn @array) or (fn elem1 elem2 ...) or mixed."
  (let* ((arr (apply #'%p-collect-list items))
         (result (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for item across arr
          when (p-true-p (let ((*wantarray* nil)) (funcall fn item)))
          do (vector-push-extend item result))
    result))

(defun p-map (fn &rest items)
  "Perl map - fn receives item as $_ parameter.
   Runs block in list context; flattens per-iteration vectors into result.
   Accepts (fn @array) or (fn elem1 elem2 ...) or mixed."
  (let* ((arr (apply #'%p-collect-list items))
         (result (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for item across arr
          do (let ((r (let ((*wantarray* t)) (funcall fn item))))
               (if (and (vectorp r) (not (stringp r)))
                   (loop for e across r
                         do (vector-push-extend e result))
                   (vector-push-extend r result))))
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
              (stable-sort result (lambda (a b) (< (to-number (funcall fn a b)) 0))))
            ;; No comparator: flatten all args and sort lexically (stable)
            (let* ((raw (apply #'%p-collect-list args))
                   (result (if (typep raw 'sequence)
                               (copy-seq raw)
                               (make-array 0 :adjustable t :fill-pointer 0))))
              (stable-sort result (lambda (a b)
                                    (string< (to-string a) (to-string b)))))))))

(defun p-reverse (&rest items)
  "Perl reverse - accepts single @array or multiple elements."
  (let ((arr (apply #'%p-collect-list items)))
    (reverse arr)))

(defun p-join (sep &rest items)
  "Perl join(SEP, LIST) - joins elements with separator.
   Handles both (join SEP @array) and (join SEP elem1 elem2 ...).
   Arrays and vectors in the argument list are flattened."
  (let ((s (to-string sep))
        ;; Flatten all arrays/vectors in the items list
        (elements (loop for item in items
                        for val = (unbox item)
                        if (and (vectorp val) (not (stringp val)))
                          append (coerce val 'list)
                        else if (and (listp val) val)
                          append val
                        else
                          collect val)))
    (format nil (concatenate 'string "~{~A~^" s "~}")
            (mapcar #'to-string elements))))

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
                           (let ((scanner (apply #'cl-ppcre:create-scanner pat ppcre-options)))
                             (cl-ppcre:split scanner s :limit ppcre-limit :with-registers-p t))
                           (cl-ppcre:ppcre-syntax-error (e)
                             (warn "Regex syntax error in split: ~A" e)
                             (list s))))))
         (dolist (p parts)
           (vector-push-extend (or p *p-undef*) result))))
      ;; Special whitespace splitting: " " splits on runs of whitespace
      ((and (stringp pattern) (string= pattern " "))
       (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) s)))
         (unless (zerop (length trimmed))
           (let ((in-word nil) (word-start 0))
             (loop for i from 0 below (length trimmed)
                   for c = (char trimmed i)
                   do (cond
                        ((and (not in-word) (not (member c '(#\Space #\Tab #\Newline #\Return))))
                         (setf in-word t word-start i))
                        ((and in-word (member c '(#\Space #\Tab #\Newline #\Return)))
                         (when (or (null max-fields) (< (length result) (1- max-fields)))
                           (vector-push-extend (subseq trimmed word-start i) result)
                           (setf in-word nil)))))
             (when in-word
               (vector-push-extend (subseq trimmed word-start) result))))))
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

(defun p-funcall-ref (ref &rest args)
  "Call a code reference"
  (let ((fn (unbox ref)))
    ;; Double-unbox: blessed coderefs are stored as box(inner-box(lambda, class="E"))
    ;; after p-bless wraps raw functions. One unbox gives the inner-box, not the fn.
    (when (p-box-p fn)
      (setf fn (p-box-value fn)))
    (apply fn args)))

;;; ============================================================
;;; Type Functions
;;; ============================================================

(defun p-backslash (val)
  "Perl reference operator \\$x - returns a box containing the referenced value.
   For scalars (boxes): returns a box containing the box (reference to scalar).
   For arrays/hashes: wraps in a box so p-flatten-args won't spread it as @arr.
   This makes \\@arr and \\%hash opaque references, not spreadable containers."
  (make-p-box val))

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
             ;; Package-qualified: "Pkg::name" -> Pkg::PL-NAME
             (let* ((pkg-str (string-upcase (subseq s 0 last-sep)))
                    (func-str (string-upcase (subseq s (+ last-sep 2))))
                    (cl-func-name (concatenate 'string "PL-" func-str))
                    (pkg (find-package pkg-str)))
               (when pkg
                 (let ((sym (intern cl-func-name pkg)))
                   (and (fboundp sym) (symbol-function sym)))))
             ;; Unqualified: "name" -> PL-NAME in current package
             (let* ((cl-func-name (concatenate 'string "PL-" (string-upcase s)))
                    (sym (intern cl-func-name *package*)))
               (and (fboundp sym) (symbol-function sym)))))))))

(defun p-cast-@ (val)
  "Perl array dereference @{$ref} - unbox to get the array.
   Handles both old format (box containing vector) and new format
   (box containing box containing vector, from p-backslash)."
  (let ((v (unbox val)))
    (if (p-box-p v) (unbox v) v)))

(defun p-cast-% (val)
  "Perl hash dereference %{$ref} - unbox to get the hash.
   Handles both old format (box containing hash) and new format
   (box containing box containing hash, from p-backslash)."
  (let ((v (unbox val)))
    (if (p-box-p v) (unbox v) v)))

(defun p-cast-$ (val)
  "Perl scalar dereference ${$ref} - get value from reference.
   $ref contains a reference (box), $$ref gets the referenced value."
  (let ((inner (unbox val)))
    ;; inner is the reference (a box), get its value
    (if (p-box-p inner)
        (p-box-value inner)
        inner)))

(defun (setf p-cast-$) (new-value val)
  "Perl scalar dereference assignment ${$ref} = val - set value in referenced box.
   Handles two shapes:
   - val wraps a box wrapping a box (normal scalar ref: val->ref->target):
     set the target box's value.
   - val wraps a non-box (blessed scalar arg in tie STORE: val IS the container):
     set val's own value directly."
  (let ((inner (unbox val)))
    (if (p-box-p inner)
        ;; val is a reference box; inner is the referenced box or value
        (let ((target (p-box-value inner)))
          (if (p-box-p target)
              (box-set target new-value)    ; normal scalar ref: set the target
              (box-set inner new-value)))   ; inner is the scalar container
        ;; val itself is the scalar container (blessed scalar in tie methods)
        (if (p-box-p val)
            (box-set val new-value)
            (error "Cannot dereference non-reference: ~A" inner)))))

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
       (let ((inner2 (p-box-value inner)))
         (cond
           ;; Array reference: box containing vector (from p-backslash @arr)
           ((and (vectorp inner2) (not (stringp inner2))) "ARRAY")
           ;; Hash reference: box containing hash-table (from p-backslash %hash)
           ((hash-table-p inner2) "HASH")
           ;; Scalar reference: box containing box (from p-backslash $x)
           (t "SCALAR"))))
      ;; Old-format hash reference (autovivified, single-boxed)
      ((hash-table-p inner) "HASH")
      ;; Old-format array reference (autovivified, single-boxed)
      ((or (listp inner) (and (vectorp inner) (not (stringp inner)))) "ARRAY")
      ;; Code reference
      ((functionp inner) "CODE")
      ;; Typeglob reference
      ((p-typeglob-p inner) "GLOB")
      ;; Not a reference
      (t ""))))

;; Keep reftype as an alias for compatibility
(defun p-reftype (val)
  "Alias for p-ref"
  (p-ref val))

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
  (let ((pkg (or (find-package (string-upcase pkg-str))
                 ;; Package may not exist yet; create it lazily
                 (make-package (string-upcase pkg-str) :use '(:cl :pcl)))))
    (make-p-typeglob pkg (string-upcase name-str))))

(defun p-glob-assign (pkg-str name-str rhs)
  "Assign RHS to the appropriate slot of typeglob *pkg::name.
   Dispatch is by type of the unwrapped RHS value."
  (let* ((pkg   (or (find-package (string-upcase pkg-str))
                    (make-package (string-upcase pkg-str) :use '(:cl :pcl))))
         (uname (string-upcase name-str))
         ;; Unwrap one box level to see what was referenced
         (inner (if (p-box-p rhs) (unbox rhs) rhs)))
    (cond
      ;; *foo = *bar — full glob copy
      ((p-typeglob-p rhs)   (p-glob-copy pkg uname rhs))
      ((p-typeglob-p inner) (p-glob-copy pkg uname inner))

      ;; *foo = \&sub or *foo = sub{} — CODE slot
      ((functionp inner)
       (setf (fdefinition (intern (concatenate 'string "PL-" uname) pkg))
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
       (let ((src-name (string-upcase inner)))
         (p-glob-copy pkg uname (make-p-typeglob pkg src-name))))

      ;; *foo = undef — no-op
      ((or (null inner) (eq inner *p-undef*)) nil)

      ;; Fallback: try as CODE if rhs is directly a function
      ((functionp rhs)
       (setf (fdefinition (intern (concatenate 'string "PL-" uname) pkg))
             rhs)))))

(defun p-glob-copy (dst-pkg dst-uname src-glob)
  "Copy all slots from src-glob into dst (pkg, uname)."
  (let ((sp (p-typeglob-package src-glob))
        (sn (p-typeglob-name src-glob)))
    ;; CODE
    (let ((src-sym (intern (concatenate 'string "PL-" sn) sp)))
      (when (fboundp src-sym)
        (setf (fdefinition (intern (concatenate 'string "PL-" dst-uname) dst-pkg))
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
              (symbol-value src-sym))))))

(defun p-glob-undef-name (pkg-str name-str)
  "undef *foo — clear all slots."
  (let* ((pkg   (find-package (string-upcase pkg-str)))
         (uname (string-upcase name-str)))
    (when pkg
      (let ((sym (intern (concatenate 'string "PL-" uname) pkg)))
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
         (slot-s (string-upcase (stringify-value slot))))
    (cond
      ((string= slot-s "CODE")
       (let ((sym (intern (concatenate 'string "PL-" uname) pkg)))
         (when (fboundp sym) (make-p-box (fdefinition sym)))))
      ((string= slot-s "SCALAR")
       (let ((sym (intern (concatenate 'string "$" uname) pkg)))
         (when (boundp sym) (make-p-box (symbol-value sym)))))
      ((string= slot-s "ARRAY")
       (let ((sym (intern (concatenate 'string "@" uname) pkg)))
         (when (boundp sym) (symbol-value sym))))
      ((string= slot-s "HASH")
       (let ((sym (intern (concatenate 'string "%" uname) pkg)))
         (when (boundp sym) (symbol-value sym))))
      ((string= slot-s "NAME")    (make-p-box (p-typeglob-name glob)))
      ((string= slot-s "PACKAGE") (make-p-box (package-name (p-typeglob-package glob))))
      ((string= slot-s "GLOB")    glob)
      (t *p-undef*))))

(defmacro p-local-glob (pkg-str name-str &body body)
  "Save all slots of *pkg::name, clear them (Perl local *foo = fresh glob),
   execute body, restore on exit."
  (let ((pkg-var        (gensym "PKG"))
        (uname-var      (gensym "UNAME"))
        (saved-had-code (gensym "HAD-CODE"))
        (saved-code     (gensym "SAVED-CODE"))
        (saved-scalar   (gensym "SAVED-SCALAR"))
        (saved-array    (gensym "SAVED-ARRAY"))
        (saved-hash     (gensym "SAVED-HASH"))
        (had-scalar     (gensym "HAD-SCALAR"))
        (had-array      (gensym "HAD-ARRAY"))
        (had-hash       (gensym "HAD-HASH")))
    `(let* ((,pkg-var   (or (find-package (string-upcase ,pkg-str))
                            (make-package (string-upcase ,pkg-str) :use '(:cl :pcl))))
            (,uname-var (string-upcase ,name-str))
            (code-sym   (intern (concatenate 'string "PL-"  ,uname-var) ,pkg-var))
            (scalar-sym (intern (concatenate 'string "$"    ,uname-var) ,pkg-var))
            (array-sym  (intern (concatenate 'string "@"    ,uname-var) ,pkg-var))
            (hash-sym   (intern (concatenate 'string "%"    ,uname-var) ,pkg-var))
            (,saved-had-code (fboundp code-sym))
            (,saved-code     (when ,saved-had-code (fdefinition code-sym)))
            (,had-scalar     (boundp scalar-sym))
            (,saved-scalar   (when ,had-scalar (symbol-value scalar-sym)))
            (,had-array      (boundp array-sym))
            (,saved-array    (when ,had-array  (symbol-value array-sym)))
            (,had-hash       (boundp hash-sym))
            (,saved-hash     (when ,had-hash   (symbol-value hash-sym))))
       ;; Clear all slots so local *foo starts fresh (Perl semantics)
       (when ,saved-had-code (fmakunbound code-sym))
       (setf (symbol-value scalar-sym) (make-p-box *p-undef*))
       (setf (symbol-value array-sym)  (make-array 0 :adjustable t :fill-pointer 0))
       (setf (symbol-value hash-sym)   (make-hash-table :test 'equal))
       (unwind-protect (progn ,@body)
         (if ,saved-had-code
             (setf (fdefinition code-sym) ,saved-code)
             (when (fboundp code-sym) (fmakunbound code-sym)))
         (if ,had-scalar
             (setf (symbol-value scalar-sym) ,saved-scalar)
             (makunbound scalar-sym))
         (if ,had-array
             (setf (symbol-value array-sym) ,saved-array)
             (makunbound array-sym))
         (if ,had-hash
             (setf (symbol-value hash-sym) ,saved-hash)
             (makunbound hash-sym))))))

(defmacro p-local-hash-elem (hash-var key-form &body body)
  "Save/restore one hash entry. Like Perl's local $hash{key}.
   Installs a fresh undef box at the key so the body can set it freely.
   On exit (normal or non-local), restores the original box, or removes
   the key entirely if it did not exist before the local."
  (let ((kv     (gensym "KEY"))
        (old-ex (gensym "OLD-EXISTS"))
        (old-bx (gensym "OLD-BOX")))
    `(let* ((,kv     (to-string ,key-form))
            (,old-ex (nth-value 1 (gethash ,kv ,hash-var)))
            (,old-bx (gethash ,kv ,hash-var)))
       ;; Install a fresh undef box so assignments inside the scope
       ;; do not clobber the saved box (which (setf p-gethash) reuses in-place).
       (setf (gethash ,kv ,hash-var) (make-p-box nil))
       (unwind-protect
           (progn ,@body)
         ;; Restore on any exit path
         (if ,old-ex
             (setf (gethash ,kv ,hash-var) ,old-bx)
             (remhash ,kv ,hash-var))))))

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
   Used for local($h{key}) = EXPR where EXPR might read the same key."
  (let ((init-val (gensym "INIT"))
        (kv       (gensym "KEY"))
        (old-ex   (gensym "OLD-EXISTS"))
        (old-bx   (gensym "OLD-BOX")))
    `(let* ((,init-val ,init-form)   ; evaluate RHS BEFORE any hash changes
            (,kv       (to-string ,key-form))
            (,old-ex   (nth-value 1 (gethash ,kv ,hash-var)))
            (,old-bx   (gethash ,kv ,hash-var)))
       (setf (gethash ,kv ,hash-var) (make-p-box ,init-val))
       (unwind-protect
           (progn ,@body)
         (if ,old-ex
             (setf (gethash ,kv ,hash-var) ,old-bx)
             (remhash ,kv ,hash-var))))))

(defun p-copy-array (arr)
  "Create a fresh adjustable copy of an array for local @arr = @arr semantics."
  (let* ((a (if (and (vectorp arr) (not (stringp arr))) arr (unbox arr)))
         (len (if (and (vectorp a) (not (stringp a))) (length a) 0))
         (copy (make-array len :adjustable t :fill-pointer len)))
    (dotimes (i len)
      (setf (aref copy i) (aref a i)))
    copy))

(defun p-copy-hash (h)
  "Create a fresh copy of a hash for local %h = %h semantics."
  (let* ((src (if (hash-table-p h) h (unbox h)))
         (copy (make-hash-table :test 'equal)))
    (when (hash-table-p src)
      (maphash (lambda (k v) (setf (gethash k copy) v)) src))
    copy))

;;; ============================================================
;;; Subroutine Reflection (exists &sub, defined &sub, undef &sub)
;;; ============================================================

(defun p-sub-exists (pkg-str name-str)
  "Perl exists &funcname — true if sub has been declared or defined."
  (let* ((pkg (find-package (string-upcase pkg-str)))
         (sym (when pkg
                (find-symbol (concatenate 'string "PL-"
                                          (string-upcase name-str))
                             pkg))))
    (if (and sym
             (or (gethash sym *p-declared-subs*)
                 (fboundp sym)))
        (make-p-box 1)
        (make-p-box nil))))

(defun p-sub-defined (pkg-str name-str)
  "Perl defined &funcname — true only if sub has an actual body (not a stub)."
  (let* ((pkg (find-package (string-upcase pkg-str)))
         (sym (when pkg
                (find-symbol (concatenate 'string "PL-"
                                          (string-upcase name-str))
                             pkg))))
    (if (and sym
             (eq (gethash sym *p-declared-subs*) :defined))
        (make-p-box 1)
        (make-p-box nil))))

(defun p-undef-sub (pkg-str name-str)
  "Perl undef &funcname — remove sub body; sub still 'exists' afterward."
  (let* ((pkg (find-package (string-upcase pkg-str)))
         (sym (when pkg
                (find-symbol (concatenate 'string "PL-"
                                          (string-upcase name-str))
                             pkg))))
    (when sym
      (when (fboundp sym) (fmakunbound sym))
      ;; Keep entry so exists &sub still returns true
      (setf (gethash sym *p-declared-subs*) :was-defined)))
  *p-undef*)

(defun p-coderef-exists-p (coderef)
  "Perl exists &{$coderef} — true if coderef points to a declared or defined sub."
  (let ((v (unbox coderef)))
    (unless (functionp v) (return-from p-coderef-exists-p (make-p-box nil)))
    ;; Get the function's name symbol (SBCL-specific)
    (let* ((fname (ignore-errors (sb-kernel:%fun-name v)))
           (status (and (symbolp fname) (gethash fname *p-declared-subs*))))
      (if status
          (make-p-box 1)
          ;; Fallback: any non-nil function object "exists"
          (make-p-box 1)))))

(defun p-coderef-defined-p (coderef)
  "Perl defined &{$coderef} — true only if coderef points to a sub with a body."
  (let ((v (unbox coderef)))
    (unless (functionp v) (return-from p-coderef-defined-p (make-p-box nil)))
    ;; Get the function's name symbol (SBCL-specific)
    (let* ((fname (ignore-errors (sb-kernel:%fun-name v)))
           (status (and (symbolp fname) (gethash fname *p-declared-subs*))))
      (if (eq status :defined)
          (make-p-box 1)
          (make-p-box nil)))))

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
      ;; Everything else (numbers, etc.) returns as-is
      (t v))))

(defun p-wantarray ()
  "Perl wantarray"
  *wantarray*)

(defun p-caller (&optional (level 0))
  "Perl caller - return information about the calling subroutine.
   In scalar context, returns package name.
   In list context, returns (package filename line subroutine).
   Returns nil when called from the top level (not inside any PCL sub)."
  ;; At top level (depth 0), caller() always returns undef/nil in Perl.
  ;; This is the common case: 'run_tests() unless caller' at top level.
  (when (zerop *pcl-sub-call-depth*)
    (return-from p-caller nil))
  (let ((frame-info nil)
        (current-level 0)
        (target-level (+ level 2)))  ; Skip p-caller itself and its caller
    ;; Walk the backtrace to find the target frame
    (sb-debug:map-backtrace
     (lambda (frame)
       (when (= current-level target-level)
         (let* ((debug-fun (sb-di:frame-debug-fun frame))
                (name (sb-di:debug-fun-name debug-fun))
                (code-loc (sb-di:frame-code-location frame)))
           (setf frame-info
                 (list "main"  ; Package (simplified - always "main" for now)
                       (or (ignore-errors
                             (sb-di:debug-source-namestring
                              (sb-di:code-location-debug-source code-loc)))
                           "-")  ; Filename
                       (or (ignore-errors
                             (sb-di:code-location-toplevel-form-offset code-loc))
                           0)  ; Line number approximation
                       (if (and name (symbolp name))
                           (symbol-name name)
                           (format nil "~A" name))))))  ; Subroutine name
       (incf current-level)
       ;; Return nil to continue, non-nil would stop
       nil))
    ;; Return results
    (if frame-info
        (if *wantarray*
            (values-list frame-info)
            (first frame-info))  ; Scalar context: just package
        nil)))  ; Past end of stack

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
  (let ((class-name (to-string class))
        (inner (unbox ref)))
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
           (return-from p-bless (make-p-box ref class-name))))))
  ref)

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

(defun p-resolve-invocant (name)
  "Resolve a bareword invocant for method calls.
   In Perl, Foo->bar() checks if sub Foo exists first:
   - If p-Foo is a function → call it, return the result (object)
   - Otherwise → return the string as a class name"
  (let* ((func-name (format nil "P-~A" name))
         (func-sym (find-symbol (string-upcase func-name) :pcl)))
    (if (and func-sym (fboundp func-sym))
        ;; Sub exists - call it to get the object
        (funcall func-sym)
        ;; No sub - return string as class name
        name)))

(defun p-method-call (obj method &rest args)
  "Perl method call - looks up p-METHOD function in object's package and walks MRO for inheritance"
  (let* ((method-name (to-string method))
         (class-name (p-get-class obj)))
    (unless class-name
      (error "Can't call method ~A on non-blessed reference" method-name))

    ;; Try to find CLOS class for MRO-based lookup
    (let* ((clos-class-name (perl-pkg-to-clos-class class-name))
           (pkg (find-package (string-upcase class-name)))
           (clos-class (when pkg (find-class (intern (string-upcase clos-class-name) pkg) nil))))

      (if clos-class
          ;; Walk MRO (Method Resolution Order) using CLOS class-precedence-list
          (let ((mro (progn (sb-mop:finalize-inheritance clos-class)
                            (sb-mop:class-precedence-list clos-class))))
            (dolist (cls mro)
              (let* ((cls-sym-name (symbol-name (class-name cls)))
                     ;; Convert CLOS class name back to CL package name
                     (pkg-name (clos-class-to-pkg cls-sym-name))
                     (pkg (find-package pkg-name)))
                (when pkg
                  (let ((fn (find-symbol (format nil "PL-~A" (string-upcase method-name)) pkg)))
                    ;; Only dispatch to methods LOCAL to this class package.
                    ;; Inherited symbols (e.g. pcl:p-push) must be ignored so
                    ;; that a class without a PUSH method doesn't accidentally
                    ;; call the pcl built-in instead of signalling "no method".
                    (when (and fn (eq (symbol-package fn) pkg) (fboundp fn))
                      (return-from p-method-call (apply fn obj args)))))))
            ;; Not found in any class in MRO - try UNIVERSAL fallbacks
            (cond
              ((string-equal method-name "isa") (apply #'p-isa obj args))
              ((string-equal method-name "can") (apply #'p-can obj args))
              (t (error "Can't locate method ~A via package ~A" method-name class-name))))

          ;; No CLOS class - fall back to single-class lookup (legacy behavior)
          (let ((pkg (find-package (string-upcase class-name))))
            (unless pkg
              (error "Package ~A not found for method call" class-name))
            (let ((fn (find-symbol (string-upcase (format nil "PL-~A" method-name)) pkg)))
              (cond
                ((and fn (eq (symbol-package fn) pkg) (fboundp fn))
                 (apply fn obj args))
                ;; UNIVERSAL fallbacks for isa, can
                ((string-equal method-name "isa") (apply #'p-isa obj args))
                ((string-equal method-name "can") (apply #'p-can obj args))
                (t (error "Can't locate method ~A in package ~A" method-name class-name)))))))))

;;; Package name conversion utilities for inheritance
(defun perl-pkg-to-clos-class (name)
  "Convert Perl package name to CLOS class name: Foo::Bar -> foo-bar"
  (string-downcase (substitute #\- #\: name)))

(defun clos-class-to-pkg (cls-name)
  "Convert CLOS class name back to CL package name for lookup.
   foo-bar -> FOO-BAR (works because we use pipe-quoted or simple package names)"
  ;; For now, just upcase - the CL package name matches the Perl name
  ;; If Perl package is Foo::Bar, CL package is |Foo::Bar| or Foo-Bar
  ;; We try both strategies
  (let* ((upcase-name (string-upcase cls-name))
         ;; Try direct mapping first (for simple package names)
         (pkg (find-package upcase-name)))
    (if pkg
        upcase-name
        ;; Try converting - to :: for nested packages
        (let ((perl-style (substitute #\: #\- upcase-name)))
          perl-style))))

;;; SUPER:: method calls
(defun p-super-call (obj method current-class &rest args)
  "Call method starting from parent of current-class in MRO (for SUPER:: calls)"
  (let* ((method-name (to-string method))
         (clos-class-name (perl-pkg-to-clos-class current-class))
         (pkg (find-package (string-upcase current-class)))
         (clos-class (when pkg (find-class (intern (string-upcase clos-class-name) pkg) nil))))

    (unless clos-class
      (error "Can't find class ~A for SUPER:: call" current-class))

    ;; Get MRO and skip current class
    (let* ((mro (progn (sb-mop:finalize-inheritance clos-class)
                       (sb-mop:class-precedence-list clos-class)))
           (parent-mro (cdr mro)))  ;; Skip current class

      (dolist (cls parent-mro)
        (let* ((cls-sym-name (symbol-name (class-name cls)))
               (pkg-name (clos-class-to-pkg cls-sym-name))
               (pkg (find-package pkg-name)))
          (when pkg
            (let ((fn (find-symbol (format nil "PL-~A" (string-upcase method-name)) pkg)))
              (when (and fn (fboundp fn))
                (return-from p-super-call (apply fn obj args)))))))

      (error "No SUPER::~A found from ~A" method-name current-class))))

;;; can() and isa() methods - available on all objects (UNIVERSAL package)
(defun p-can (invocant method-name)
  "Perl can() - check if object/class can perform a method.
   Returns the code reference if method exists, nil otherwise.
   Uses C3 MRO to check inheritance chain."
  (let* ((method-str (to-string method-name))
         (class-name (cond
                       ((stringp invocant) invocant)
                       ((p-box-p invocant) (p-get-class invocant))
                       ((hash-table-p invocant) (gethash :__class__ invocant))
                       (t nil))))
    (unless class-name
      (return-from p-can nil))

    ;; Try to find CLOS class for MRO-based lookup
    ;; Classes are defined in packages named after the Perl package (e.g., Dog::dog)
    (let* ((clos-class-name (perl-pkg-to-clos-class class-name))
           (pkg (find-package (string-upcase class-name)))
           (clos-class (when pkg
                         (find-class (intern (string-upcase clos-class-name) pkg) nil))))

      (if clos-class
          ;; Walk MRO (Method Resolution Order) using CLOS class-precedence-list
          (let ((mro (sb-mop:class-precedence-list clos-class)))
            (dolist (cls mro)
              (let* ((cls-sym-name (symbol-name (class-name cls)))
                     (pkg-name (clos-class-to-pkg cls-sym-name))
                     (pkg (find-package pkg-name)))
                (when pkg
                  (let ((fn (find-symbol (format nil "PL-~A" (string-upcase method-str)) pkg)))
                    (when (and fn (fboundp fn))
                      (return-from p-can (symbol-function fn)))))))
            nil)  ; Not found in any class in MRO

          ;; No CLOS class - fall back to single-class lookup
          (let ((pkg (find-package (string-upcase class-name))))
            (when pkg
              (let ((fn (find-symbol (format nil "PL-~A" (string-upcase method-str)) pkg)))
                (if (and fn (fboundp fn))
                    (symbol-function fn)
                    nil))))))))

(defun p-isa (invocant class-name)
  "Perl isa() - check if object is-a class.
   Uses C3 MRO to check inheritance chain.
   Returns t if invocant is-a class-name, nil otherwise."
  (let* ((check-class (to-string class-name))
         (obj-class (cond
                      ((stringp invocant) invocant)
                      ((p-box-p invocant) (p-get-class invocant))
                      ((hash-table-p invocant) (gethash :__class__ invocant))
                      (t nil))))
    (unless obj-class
      (return-from p-isa nil))

    ;; If the object's class defines a custom isa() method (PL-ISA), call it.
    ;; Perl's infix isa operator delegates to ->isa if the class overrides it.
    (let* ((pkg (find-package (string-upcase obj-class)))
           (custom-isa (when pkg (find-symbol "PL-ISA" pkg))))
      (when (and custom-isa (eq (symbol-package custom-isa) pkg) (fboundp custom-isa))
        (return-from p-isa (funcall custom-isa invocant check-class))))

    ;; Exact match
    (when (string-equal obj-class check-class)
      (return-from p-isa t))

    ;; Try to find CLOS class for MRO-based lookup
    ;; Classes are defined in packages named after the Perl package (e.g., Dog::dog)
    (let* ((clos-class-name (perl-pkg-to-clos-class obj-class))
           (pkg (find-package (string-upcase obj-class)))
           (clos-class (when pkg
                         (find-class (intern (string-upcase clos-class-name) pkg) nil))))

      (when clos-class
        ;; Walk MRO (Method Resolution Order) using CLOS class-precedence-list
        ;; The class may not be finalized yet (no instances created), so handle that.
        (handler-case
          (sb-mop:finalize-inheritance clos-class)
          (error () nil))
        (handler-case
          (let ((mro (sb-mop:class-precedence-list clos-class)))
            (dolist (cls mro)
              (let* ((cls-sym-name (symbol-name (class-name cls)))
                     (pkg-name (clos-class-to-pkg cls-sym-name)))
                (when (string-equal pkg-name check-class)
                  (return-from p-isa t)))))
          (unbound-slot () nil)))

      ;; Fallback: walk runtime @ISA for dynamically modified inheritance
      (labels ((walk-isa (class-str visited)
                 (when (member class-str visited :test #'equal)
                   (return-from walk-isa nil))
                 (let* ((pkg-sym (find-package (string-upcase class-str)))
                        (isa-sym (when pkg-sym
                                   (find-symbol "@ISA" pkg-sym)))
                        (isa-val (when (and isa-sym (boundp isa-sym))
                                   (symbol-value isa-sym))))
                   (when (and isa-val (vectorp isa-val))
                     (loop for parent across isa-val
                           for parent-str = (to-string parent)
                           do (when (or (string-equal parent-str check-class)
                                        (walk-isa parent-str (cons class-str visited)))
                                (return-from p-isa t)))))))
        (walk-isa obj-class nil))

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
   and cause infinite loops)."
  ;; First strip (?{code}) and (??{code}) blocks — cl-ppcre hangs on these
  (let* ((pat (cl-ppcre:regex-replace-all "\\(\\?\\?\\{[^}]*\\}\\)" pattern ""))
         (pat (cl-ppcre:regex-replace-all "\\(\\?\\{[^}]*\\}\\)" pat "")))
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
  "Reset all capture group variables to nil"
  (setf $1 nil $2 nil $3 nil $4 nil $5 nil
        $6 nil $7 nil $8 nil $9 nil)
  (clrhash %+))

(defmacro %set-cap (var str starts ends idx)
  "Set capture variable VAR from reg-starts/ends at IDX, guarding against NIL (optional group)."
  `(let ((rs (aref ,starts ,idx)) (re (aref ,ends ,idx)))
     (setf ,var (if (and rs re) (subseq ,str rs re) nil))))

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

(defun do-regex-match (string op)
  "Perform regex match.
   In scalar context: return t if matched, nil otherwise.
   In list context (*wantarray* t): return vector of captures, or nil if no match.
   Also sets capture group variables $1, $2, ... $9.
   Note: In Perl, captures are only updated on successful match.
   /g in scalar context: iterates over matches, tracking pos in *p-match-pos*.
   /g in list context: returns all matches at once (no pos tracking).
   /gc: keeps pos on failure instead of resetting it."
  (let* ((str (to-string (unbox string)))
         (pattern (p-regex-match-pattern op))
         (modifiers (p-regex-match-modifiers op))
         (options (build-ppcre-options modifiers))
         (global-p (getf modifiers :g))
         (cont-p (getf modifiers :c)))
    (handler-case
        (multiple-value-bind (scanner reg-names)
            (apply #'cl-ppcre:create-scanner pattern options)
          ;; Perl clears %+ on every match attempt, even failures.
          ;; $1..$9 are only cleared/set on successful matches.
          (clrhash %+)
          (cond
            ;; /g in list context: return all matches at once, no pos tracking
            ((and global-p *wantarray*)
             (let ((all-results nil)
                   (last-rs nil) (last-re nil))
               (cl-ppcre:do-scans (ms me rs re scanner str)
                 (setf last-rs rs last-re re)
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
                   (when last-rs
                     (set-capture-groups str last-rs last-re reg-names)))
                 result)))
            ;; /g in scalar context: iterate from current pos
            ((and global-p (not *wantarray*))
             (let ((start (or (gethash string *p-match-pos*) 0)))
               (multiple-value-bind (match-start match-end reg-starts reg-ends)
                   (cl-ppcre:scan scanner str :start start)
                 (if match-start
                     (progn
                       (setf (gethash string *p-match-pos*) match-end)
                       (clear-capture-groups)
                       (set-capture-groups str reg-starts reg-ends reg-names)
                       t)
                     (progn
                       (unless cont-p
                         (remhash string *p-match-pos*))
                       nil)))))
            ;; No /g: single match
            (t
             (multiple-value-bind (match-start match-end reg-starts reg-ends)
                 (cl-ppcre:scan scanner str)
               (declare (ignore match-end))
               (when match-start
                 (clear-capture-groups)
                 (set-capture-groups str reg-starts reg-ends reg-names)
                 (if *wantarray*
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
                     t))))))
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
         (pattern (p-subst-op-pattern op))
         (raw-replacement (p-subst-op-replacement op))
         (modifiers (p-subst-op-modifiers op))
         (eval-p (member :e modifiers))
         (replacement (unless eval-p
                        (perl-to-ppcre-replacement (if (stringp raw-replacement)
                                                       raw-replacement ""))))
         (global-p (member :g modifiers))
         (non-destructive-p (member :r modifiers))
         (case-insensitive (member :i modifiers))
         (single-line (member :s modifiers))
         (multi-line (member :m modifiers)))
    (handler-case
        (let* ((options (append (when case-insensitive '(:case-insensitive-mode t))
                                (when single-line '(:single-line-mode t))
                                (when multi-line '(:multi-line-mode t)))))
          (multiple-value-bind (scanner reg-names)
              (apply #'cl-ppcre:create-scanner pattern options)
          (let* ((count 0)
                 (result nil))
          (if eval-p
              ;; s///e: call lambda per match, setting $1..$9 from capture groups
              ;; :simple-calls t → function receives (match g1 g2 ...) as strings
              (let ((rep-fn (lambda (whole-match &rest groups)
                              (declare (ignore whole-match))
                              (incf count)
                              (clear-capture-groups)
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
                  (declare (ignore match-end))
                  (when match-start
                    (clear-capture-groups)
                    (set-capture-groups str reg-starts reg-ends reg-names)))
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

(defun do-tr (string-box op)
  "Perform transliteration on boxed string, return count of changes"
  (let* ((str (to-string (unbox string-box)))
         (from-raw (p-tr-op-from op))
         (to-raw (p-tr-op-to op))
         (modifiers (p-tr-op-modifiers op))
         (complement-p (member :c modifiers))
         (delete-p (member :d modifiers))
         (squash-p (member :s modifiers))
         (from-chars (expand-tr-chars from-raw))
         (to-chars (expand-tr-chars to-raw))
         (count 0)
         (last-char nil))
    ;; Build translation table
    (let ((result (with-output-to-string (out)
                    (loop for c across str
                          do (let* ((pos (position c from-chars))
                                    (in-from (if complement-p (null pos) pos)))
                               (cond
                                 ;; Character is in from-set (or complement case)
                                 (in-from
                                  (incf count)
                                  (let* ((actual-pos (if complement-p 0 pos))
                                         (new-char (cond
                                                     (delete-p nil)
                                                     ((>= actual-pos (length to-chars))
                                                      (if (> (length to-chars) 0)
                                                          (char to-chars (1- (length to-chars)))
                                                          c))
                                                     (t (char to-chars actual-pos)))))
                                    (when new-char
                                      ;; Squash: skip consecutive identical replacements
                                      (if (and squash-p (eql new-char last-char))
                                          nil  ; skip duplicate
                                          (progn
                                            (write-char new-char out)
                                            (setf last-char new-char))))))
                                 ;; Character not in from-set
                                 (t
                                  (write-char c out)
                                  (setf last-char c))))))))
      ;; Update the boxed string (and invalidate caches)
      (if (p-box-p string-box)
          (setf (p-box-value string-box) result
                (p-box-sv-ok string-box) nil
                (p-box-nv-ok string-box) nil)
          (warn "Cannot modify non-boxed value in tr///"))
      count)))

(defun p-=~ (string operation)
  "Perl =~ binding operator.
   Dispatches based on operation type:
   - p-regex-match: perform match, return t/nil
   - p-subst-op: perform substitution, modify string, return count
   - p-tr-op: perform transliteration, modify string, return count"
  (cond
    ((p-regex-match-p operation)
     (do-regex-match string operation))
    ((p-subst-op-p operation)
     (do-regex-subst string operation))
    ((p-tr-op-p operation)
     (do-tr string operation))
    (t
     (warn "Unknown regex operation type: ~A" (type-of operation))
     nil)))

(defun p-!~ (string operation)
  "Perl !~ negative binding operator"
  (not (p-=~ string operation)))

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
  "Perl pack - basic implementation for common templates.
   Returns a string of bytes."
  (let ((tmpl (to-string (unbox template)))
        (result (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (arg-idx 0))
    (loop for i from 0 below (length tmpl)
          for ch = (char tmpl i)
          do (case ch
               ;; 'd' - double-precision float (8 bytes, native order)
               (#\d (let* ((val (to-number (unbox (nth arg-idx args))))
                           (bits (sb-kernel:double-float-bits (coerce val 'double-float))))
                      (dotimes (byte-idx 8)
                        (vector-push-extend
                         (code-char (logand #xff (ash bits (* -8 byte-idx))))
                         result))
                      (incf arg-idx)))
               ;; 'C' - unsigned char
               (#\C (let ((val (truncate (to-number (unbox (nth arg-idx args))))))
                      (vector-push-extend (code-char (logand val #xff)) result)
                      (incf arg-idx)))
               ;; 'a'/'A' - ASCII string (null/space padded)
               ((#\a #\A)
                (let ((s (to-string (unbox (nth arg-idx args)))))
                  (loop for c across s do (vector-push-extend c result))
                  (incf arg-idx)))
               ;; Skip spaces and digits (repeat counts - simplified)
               ((#\Space #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) nil)
               ;; Unknown template char - skip arg
               (t (incf arg-idx))))
    result))

(defun p-unpack (template str)
  "Perl unpack - basic stub, returns empty list for now."
  (declare (ignore template str))
  (make-array 0 :adjustable t :fill-pointer 0))

;;; ============================================================
;;; Package initialization
;;; ============================================================

;; Export all p- symbols so they're accessible from other packages
;; This includes all functions, macros, and variables with p- prefix
(do-symbols (sym (find-package :pcl))
  (when (and (>= (length (symbol-name sym)) 3)
             (string= "PL-" (subseq (symbol-name sym) 0 3)))
    (export sym :pcl)))

;; Perl uses double-precision floats everywhere.
;; Make CL read all float literals as double-float (e.g., 1.5 → 1.5d0, not 1.5f0)
(setf *read-default-float-format* 'double-float)

;; Enable Perl-style named capture groups (?<name>...) in cl-ppcre
(setf cl-ppcre:*allow-named-registers* t)

;;; ============================================================
;;; Stub packages for common Perl modules
;;; ============================================================

;; utf8 module stub - on non-EBCDIC systems, uni_to_native/native_to_uni are identity.
;; Note: PCL generates pl- prefix for user function calls (e.g. utf8::upgrade → utf8::pl-upgrade),
;; so stubs in user-accessible packages must use pl- prefix (not p- which is for pcl builtins).
(defpackage :utf8 (:use :cl :pcl))
(in-package :utf8)
(defun pl-encode (str) (declare (ignore str)) 1)
(defun pl-decode (str) (declare (ignore str)) 1)
(defun pl-upgrade (str) (declare (ignore str)) 1)
(defun pl-downgrade (str) (declare (ignore str)) 1)
(defun pl-is_utf8 (str) (declare (ignore str)) 1)
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
  (let ((members (sb-posix:group-mem g)))
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
    (sb-posix:do-groups (g)
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

(defpackage :XSLoader (:use :cl :pcl))
(in-package :XSLoader)
;; XSLoader::load('Module', $version) — no-op, XS cannot be loaded by PCL
(defun pl-load (&rest args) (declare (ignore args)) nil)
(defun pl-bootstrap_inherit (&rest args) (declare (ignore args)) nil)
(in-package :pcl)

(format t "PCL Runtime loaded~%")
