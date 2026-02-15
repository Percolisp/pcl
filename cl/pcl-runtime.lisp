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
   #:pl-box #:make-pl-box #:pl-box-p #:pl-box-value
   #:unbox #:ensure-boxed
   #:box-set #:box-nv #:box-sv  ; lazy caching accessors
   #:pl-undef #:pl-defined
   #:pl-let #:pl-$
   ;; Arithmetic
   #:pl-+ #:pl-- #:pl-* #:pl-/ #:pl-% #:pl-** #:pl-int #:pl-abs
   ;; Math
   #:pl-sin #:pl-cos #:pl-atan2 #:pl-exp #:pl-log #:pl-sqrt #:pl-rand #:pl-srand
   ;; String
   #:pl-. #:pl-str-x #:pl-list-x #:pl-length #:pl-substr #:pl-lc #:pl-uc #:pl-fc
   #:pl-chomp #:pl-chop #:pl-index #:pl-rindex #:pl-string-concat
   #:pl-chr #:pl-ord #:pl-hex #:pl-oct #:pl-lcfirst #:pl-ucfirst #:pl-sprintf #:pl-printf
   #:pl-quotemeta #:pl-pos
   ;; Assignment
   #:pl-setf #:pl-my #:pl-incf #:pl-decf
   #:pl-pre++ #:pl-post++ #:pl-pre-- #:pl-post--
   ;; Compound assignment
   #:pl-*= #:pl-/= #:pl-%= #:pl-**=
   #:pl-.= #:pl-str-x=
   #:pl-bit-and= #:pl-bit-or= #:pl-bit-xor= #:pl-<<= #:pl->>=
   #:pl-and-assign #:pl-or-assign #:pl-//=
   ;; Comparison (numeric)
   #:pl-== #:pl-!= #:pl-< #:pl-> #:pl-<= #:pl->= #:pl-<=>
   ;; Comparison (string)
   #:pl-str-eq #:pl-str-ne #:pl-str-lt #:pl-str-gt #:pl-str-le #:pl-str-ge #:pl-str-cmp
   ;; Chained comparison
   #:pl-chain-cmp
   ;; Range operator
   #:pl-.. #:pl-...
   ;; Logical
   #:pl-&& #:pl-|| #:pl-! #:pl-not #:pl-and #:pl-or #:pl-xor #:pl-//
   ;; Bitwise
   #:pl-bit-and #:pl-bit-or #:pl-bit-xor #:pl-bit-not #:pl-<< #:pl->>
   ;; Data structures
   #:pl-aref #:pl-aref-box #:pl-aref-deref #:pl-gethash #:pl-gethash-box #:pl-gethash-deref
   #:pl-aslice #:pl-hslice #:pl-kv-hslice
   #:pl-hash #:pl-array-init #:pl-array-last-index #:pl-set-array-length
   #:pl-push #:pl-pop #:pl-shift #:pl-unshift #:pl-splice #:pl-flatten
   #:pl-keys #:pl-values #:pl-each #:pl-exists #:pl-exists-array #:pl-delete #:pl-delete-array
   #:pl-delete-hash-slice #:pl-delete-kv-hash-slice #:pl-delete-array-slice
   ;; Control flow
   #:pl-if #:pl-unless #:pl-while #:pl-until #:pl-for #:pl-foreach
   #:pl-return #:pl-last #:pl-next #:pl-redo
   ;; I/O
   #:pl-print #:pl-say #:pl-warn #:pl-die
   ;; do BLOCK
   #:pl-do
   ;; Exception handling
   #:pl-eval #:pl-eval-block #:pl-exception #:pl-exception-object
   ;; File I/O
   #:pl-open #:pl-close #:pl-eof #:pl-tell #:pl-seek
   #:pl-binmode #:pl-read #:pl-sysread #:pl-syswrite
   #:pl-truncate #:pl-stat #:pl-lstat
   ;; File test operators
   #:pl--e #:pl--d #:pl--f #:pl--r #:pl--w #:pl--x #:pl--s #:pl--z
   #:pl-unlink #:pl-fileno #:pl-getc #:pl-readline
   ;; Directory I/O
   #:pl-opendir #:pl-readdir #:pl-closedir #:pl-rewinddir
   ;; File glob
   #:pl-glob
   ;; File/Directory operations
   #:pl-chdir #:pl-set_up_inc #:pl-mkdir #:pl-rmdir #:pl-getcwd #:pl-cwd #:pl-rename #:pl-chmod
   ;; Time functions
   #:pl-time #:pl-times #:pl-sleep #:pl-study #:pl-reset #:pl-vec #:pl-localtime #:pl-gmtime
   ;; Process control
   #:pl-exit #:pl-system #:pl-backtick
   ;; Environment
   #:%ENV #:pl-env-get #:pl-env-set
   ;; Module system
   #:@INC #:%INC #:@ARGV #:pl-use #:pl-require #:pl-require-file
   ;; Functions
   #:pl-backslash #:pl-ref #:pl-reftype #:pl-scalar #:pl-wantarray #:pl-caller
   #:pl-grep #:pl-map #:pl-sort #:pl-reverse
   #:pl-join #:pl-split #:pl-funcall-ref
   ;; Dereferencing (sigil cast operations)
   #:pl-cast-@ #:pl-cast-% #:pl-cast-$
   ;; OO
   #:pl-bless #:pl-get-class #:pl-method-call #:pl-resolve-invocant
   #:pl-super-call #:perl-pkg-to-clos-class #:clos-class-to-pkg
   #:pl-can #:pl-isa
   ;; Regex
   #:pl-=~ #:pl-!~ #:pl-subst #:pl-tr #:pl-regex
   ;; Capture groups
   #:$1 #:$2 #:$3 #:$4 #:$5 #:$6 #:$7 #:$8 #:$9
   ;; Special variables
   #:$$ #:$? #:|$.| #:$0 #:$@ #:|$^O| #:|$^V| #:|$^X| #:|${^TAINT}| #:|$/| #:|$\\| #:|$"| #:|$\|| #:|$;| #:|$,|
   ;; Context
   #:*wantarray*
   ;; END blocks
   #:*end-blocks*
   ;; Compile-time definition macros (for BEGIN block support)
   #:pl-sub #:pl-declare-sub #:pl-our #:pl-my
   ;; Assignment forms (distinct from pl-setf for clarity)
   #:pl-scalar-= #:pl-array-= #:pl-hash-= #:pl-list-=))

(in-package :pcl)

;;; ============================================================
;;; Compile-Time Definition Macros
;;; ============================================================
;;; These macros wrap definitions in eval-when to make them available
;;; at compile time. This matches Perl's semantics where subs and
;;; package variables are defined as they are parsed, allowing BEGIN
;;; blocks to call subs defined before them in source order.

;;; pl-sub: Define a Perl subroutine.
;;; Uses eval-when so the function exists at compile time, allowing
;;; BEGIN blocks to call subs defined before them in source order.
;;; This matches Perl's semantics where subs are compiled immediately.
(defmacro pl-sub (name params &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defun ,name ,params ,@body)))

;;; pl-declare-sub: Forward-declare a Perl sub as a no-op stub.
;;; Perl subs can be called before definition; CL resolves names at load time.
;;; Only creates the stub if the function isn't already defined.
(defmacro pl-declare-sub (name)
  `(unless (fboundp ',name)
     (defun ,name (&rest args) (declare (ignore args)) nil)))

;;; pl-our: Declare a package variable (Perl's 'our').
;;; Declaration happens at compile time (visible to BEGIN blocks).
;;; Initialization (if any) happens at runtime (after all BEGIN blocks).
;;; This matches Perl where 'our $x = 1' declares at compile, assigns at runtime.
(defmacro pl-our (name &optional (init nil init-supplied-p))
  (if init-supplied-p
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (defvar ,name))
         (setf ,name ,init))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defvar ,name))))

;;; pl-my: Declare a lexical variable at file scope (Perl's top-level 'my').
;;; Same semantics as pl-our: declaration at compile time, init at runtime.
;;; Note: Inside subs, 'my' uses regular let bindings, not this macro.
(defmacro pl-my (name &optional (init nil init-supplied-p))
  (if init-supplied-p
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (defvar ,name))
         (setf ,name ,init))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defvar ,name))))

;;; Forward declarations to avoid style warnings
(declaim (ftype (function (t) t) to-number to-string unbox pl-get-stream))
(defvar *pl-undef* :undef "Perl's undef value")

;;; Forward declaration for %INC table (full definition in Module System section)
(defvar *pl-inc-table* (make-hash-table :test 'equal)
  "Perl %INC - tracks loaded modules (forward declaration)")

;;; Regex capture group variables ($1, $2, ... $9)
(defvar $1 nil "Regex capture group 1")
(defvar $2 nil "Regex capture group 2")
(defvar $3 nil "Regex capture group 3")
(defvar $4 nil "Regex capture group 4")
(defvar $5 nil "Regex capture group 5")
(defvar $6 nil "Regex capture group 6")
(defvar $7 nil "Regex capture group 7")
(defvar $8 nil "Regex capture group 8")
(defvar $9 nil "Regex capture group 9")

;;; Process ID ($$)
(defvar $$ (sb-posix:getpid) "Process ID")

;;; Child exit status ($?)
(defvar $? 0 "Child process exit status from last system/backtick")

;;; Input line number ($.)
(defvar |$.| 0 "Input line number of last filehandle read")

;;; Program name ($0)
(defvar $0 (or (car sb-ext:*posix-argv*) "perl") "Program name")

;;; Eval error ($@)
(defvar $@ "" "Error from last eval")

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
(defun pl-errno-string ()
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
(defstruct (pl-box (:constructor %make-pl-box))
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

(defun make-pl-box (value &optional class)
  "Create a pl-box, pre-caching if value is already typed"
  (let ((box (%make-pl-box :value value :class class)))
    (typecase value
      (number (setf (pl-box-nv box) value (pl-box-nv-ok box) t))
      (string (setf (pl-box-sv box) value (pl-box-sv-ok box) t)))
    box))

(defun unbox (val)
  "Extract value from a box, or return val if not boxed"
  (if (pl-box-p val)
      (pl-box-value val)
      val))

(defun ensure-boxed (val)
  "Ensure a value is boxed"
  (if (pl-box-p val)
      val
      (make-pl-box val)))

;;; Boxed special variables (must be after make-pl-box definition)
;;; Input record separator ($/)
(defvar |$/| (make-pl-box (string #\Newline)) "Input record separator")
;;; Output record separator ($\)
(defvar |$\\| (make-pl-box "") "Output record separator")
;;; List separator ($")
(defvar |$"| (make-pl-box " ") "List separator for array interpolation")
;;; Output autoflush ($|)
(defvar |$\|| (make-pl-box 0) "Output autoflush flag")
;;; Subscript separator ($;)
(defvar |$;| (make-pl-box (string (code-char #x1C))) "Subscript separator (default SUBSEP)")
;;; Output field separator ($,)
(defvar |$,| (make-pl-box "") "Output field separator for print")

(defun get-input-record-separator ()
  "Get the current value of $/ (unboxed).
   Returns nil for undef (slurp mode) or when $/ is a reference."
  (let ((val (unbox |$/|)))
    (cond
      ((eq val *pl-undef*) nil)
      ;; $/ = \N (reference to number) means record mode — chomp does nothing
      ((pl-box-p val) nil)
      (t (to-string val)))))

;;; ------------------------------------------------------------
;;; Box accessors with lazy caching
;;; ------------------------------------------------------------

(defun box-set (box value)
  "Set box value, invalidating caches. Pre-caches if already typed.
   If value is a box containing a primitive, unbox it (Perl copy semantics).
   If value is a box containing another box (reference), preserve it.
   If value is a blessed box, copy the class to target box.
   If box is not a PL-BOX (e.g. *pl-undef*), silently ignore (Perl: undef = val is no-op)."
  (unless (pl-box-p box)
    (return-from box-set value))
  (let ((v (if (pl-box-p value)
               (let ((inner (pl-box-value value)))
                 ;; If inner is a box, this is a reference - preserve it
                 (if (pl-box-p inner) value inner))
               value)))
    (setf (pl-box-value box) v
          (pl-box-nv-ok box) nil
          (pl-box-sv-ok box) nil)
    ;; Preserve class from blessed boxes
    (when (and (pl-box-p value) (pl-box-class value))
      (setf (pl-box-class box) (pl-box-class value)))
    (typecase v
      (number (setf (pl-box-nv box) v (pl-box-nv-ok box) t))
      (string (setf (pl-box-sv box) v (pl-box-sv-ok box) t)))
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
              (has-digit nil)
              (has-dot nil)
              (has-exp nil))
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
            (setf has-dot t)
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
                  (progn
                    (setf has-exp t)
                    (loop while (and (< end len)
                                     (digit-char-p (char trimmed end)))
                          do (incf end)))
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

(defun box-nv (box)
  "Get numeric value from box with lazy caching"
  (if (pl-box-nv-ok box)
      (pl-box-nv box)
      (let ((v (pl-box-value box)))
        (let ((n (cond
                   ((numberp v) v)
                   ((eq v *pl-undef*) 0)
                   ((null v) 0)
                   ((eq v t) 1)  ; CL's T from comparisons - Perl true is 1
                   ((stringp v) (parse-perl-number v))
                   ((pl-box-p v) 0)  ; reference as number
                   (t 0))))
          (setf (pl-box-nv box) n
                (pl-box-nv-ok box) t)
          n))))

(defun object-address (obj)
  "Get a unique address/ID for an object (implementation-dependent)"
  #+sbcl (sb-kernel:get-lisp-obj-address obj)
  #-sbcl (sxhash obj))  ; Fallback: use hash as pseudo-address

(defun stringify-value (v)
  "Convert a raw value to string"
  (cond
    ((stringp v) v)
    ((eq v *pl-undef*) "")
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
    ((pl-box-p v) (format nil "SCALAR(0x~X)" (object-address v)))
    ((hash-table-p v) (format nil "HASH(0x~X)" (object-address v)))
    ((vectorp v) (format nil "ARRAY(0x~X)" (object-address v)))
    ;; Lists (from return lists, etc.) - join with spaces like Perl's @array interpolation
    ((listp v) (format nil "~{~A~^ ~}" (mapcar #'to-string v)))
    ;; CL's T from comparison operators - Perl true stringifies to "1"
    ((eq v t) "1")
    (t (format nil "~A" v))))

(defun box-sv (box)
  "Get string value from box with lazy caching"
  (if (pl-box-sv-ok box)
      (pl-box-sv box)
      (let* ((inner (pl-box-value box))
             (class (or (pl-box-class box)
                        (when (hash-table-p inner)
                          (gethash :__class__ inner))))
             (raw (stringify-value inner))
             (s (if class
                    (format nil "~A=~A" class raw)
                    raw)))
        (setf (pl-box-sv box) s
              (pl-box-sv-ok box) t)
        s)))

(defmacro pl-let (bindings &body body)
  "Perl my declarations - creates boxed variables.
   Usage: (pl-let (($x 10) ($y 20)) ...body...)
   Each variable becomes a box that can be referenced with \\$x"
  (let ((box-bindings
         (mapcar (lambda (binding)
                   (if (listp binding)
                       (list (first binding)
                             `(make-pl-box ,(second binding)))
                       (list binding '(make-pl-box *pl-undef*))))
                 bindings)))
    `(let ,box-bindings
       ,@body)))

(defun pl-$ (box)
  "Perl scalar dereference $$ref - get value from the referenced box.
   Structure: $ref box -> pl-backslash box -> target $x box
   We need to go TWO levels to get the actual value."
  (let ((ref (unbox box)))  ; Get the pl-backslash box
    (if (pl-box-p ref)
        (let ((target (pl-box-value ref)))  ; Get the target $x box
          (if (pl-box-p target)
              (pl-box-value target)  ; Get the actual value
              target))
        ref)))

(defun (setf pl-$) (new-value box)
  "Perl scalar dereference assignment $$ref = val - set value in referenced box.
   Structure: $ref box -> pl-backslash box -> target $x box
   We need to go TWO levels to set the actual value."
  (let ((ref (unbox box)))  ; Get the pl-backslash box
    (if (pl-box-p ref)
        (let ((target (pl-box-value ref)))  ; Get the target $x box
          (if (pl-box-p target)
              (box-set target new-value)  ; Set the target's value
              (error "Cannot dereference non-reference (target not a box): ~A" target)))
        (error "Cannot dereference non-reference: ~A" ref))))

;;; ============================================================
;;; Value System - Perl's dynamic typing
;;; ============================================================

(defun pl-undef (&optional val)
  "Return Perl's undef value, or undefine a variable.
   (pl-undef) → undef
   (pl-undef @arr) → clear array, return undef
   (pl-undef %hash) → clear hash, return undef
   (pl-undef $scalar) → set scalar to undef, return undef"
  (when val
    (cond
      ((and (vectorp val) (not (stringp val)))
       (setf (fill-pointer val) 0))
      ((hash-table-p val)
       (clrhash val))
      ((pl-box-p val)
       (box-set val *pl-undef*))))
  *pl-undef*)

(defun pl-defined (val)
  "Check if value is defined (not undef) - auto-unboxes.
   Both *pl-undef* and nil count as undefined."
  (let ((v (unbox val)))
    (and (not (null v))
         (not (eq v *pl-undef*)))))

(defun pl-true-p (val)
  "Perl truthiness: false if undef, 0, empty string, or nil - auto-unboxes"
  (let ((v (unbox val)))
    (cond
      ((eq v *pl-undef*) nil)
      ((null v) nil)
      ((and (numberp v) (zerop v)) nil)
      ((and (stringp v) (string= v "")) nil)
      ((and (stringp v) (string= v "0")) nil)
      (t t))))

;;; ============================================================
;;; Arithmetic Operators
;;; ============================================================

(defun pl-+ (&rest args)
  "Perl addition"
  (apply #'+ (mapcar #'to-number args)))

(defun pl-- (&rest args)
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
                ((or (alpha-char-p ch) (char= ch #\_))
                 (concatenate 'string "-" val))
                ;; Starts with digit but not a pure number (e.g. "12foo")
                ;; Perl negates numerically using leading portion
                (t (- (to-number (first args))))))
            ;; Numeric negation
            (- (to-number (first args)))))
      (apply #'- (mapcar #'to-number args))))

(defun pl-* (&rest args)
  "Perl multiplication"
  (apply #'* (mapcar #'to-number args)))

(defun pl-/ (a b)
  "Perl division"
  (/ (to-number a) (to-number b)))

(defun pl-% (a b)
  "Perl modulo"
  (mod (truncate (to-number a)) (truncate (to-number b))))

(defun pl-** (a b)
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

(defun pl-int (val)
  "Perl int - truncate toward zero"
  (truncate (to-number val)))

(defun pl-abs (val)
  "Perl abs - absolute value"
  (abs (to-number val)))

(defun pl-sin (val)
  "Perl sin - sine"
  (sin (coerce (to-number val) 'double-float)))

(defun pl-cos (val)
  "Perl cos - cosine"
  (cos (coerce (to-number val) 'double-float)))

(defun pl-atan2 (y x)
  "Perl atan2 - arctangent of y/x"
  (atan (coerce (to-number y) 'double-float)
        (coerce (to-number x) 'double-float)))

(defun pl-exp (val)
  "Perl exp - e^x"
  (exp (coerce (to-number val) 'double-float)))

(defun pl-log (val)
  "Perl log - natural logarithm"
  (let ((n (to-number val)))
    (when (zerop n)
      (error "Can't take log of 0"))
    (log (coerce n 'double-float))))

(defun pl-sqrt (val)
  "Perl sqrt - square root"
  (let ((n (to-number val)))
    (when (minusp n)
      (error "Can't take sqrt of ~A" n))
    (sqrt (coerce n 'double-float))))

(defun pl-rand (&optional max)
  "Perl rand - random number"
  (if max
      (* (random 1.0d0) (to-number max))
      (random 1.0d0)))

(defun pl-srand (&optional seed)
  "Perl srand - seed random number generator"
  (declare (ignore seed))
  ;; CL doesn't have portable srand - just return a value
  1)

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

(defun to-number (val)
  "Convert value to number (Perl semantics).
   Uses lazy caching for boxed values."
  (if (pl-box-p val)
      (box-nv val)
      ;; Raw value - convert directly
      (cond
        ((numberp val) val)
        ((eq val *pl-undef*) 0)
        ((null val) 0)
        ;; CL's T from comparison operators - Perl true numifies to 1
        ((eq val t) 1)
        ((stringp val) (parse-perl-number val))
        (t 0))))

;;; ============================================================
;;; String Operators
;;; ============================================================

(defun pl-. (&rest args)
  "Perl string concatenation"
  (apply #'concatenate 'string (mapcar #'to-string args)))

(defun pl-string-concat (&rest args)
  "Perl string concatenation (alias for interpolation)"
  (apply #'pl-. args))

(defun pl-str-x (str count)
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

(defun pl-list-x (list-val count)
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
  (if (pl-box-p val)
      (box-sv val)
      ;; Raw value - convert directly
      (stringify-value val)))

(defun pl-length (val)
  "Perl length function - returns undef for undef input"
  (let ((v (unbox val)))
    (if (or (eq v *pl-undef*) (null v))
        *pl-undef*
        (length (to-string v)))))

(defun pl-substr (str start &optional len replacement)
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
          (when (pl-box-p str)
            (box-set str new-str))
          replaced-part)
        ;; 2 or 3 arg form: extract
        (subseq s (min st slen) end-pos))))

(defun pl-lc (str)
  "Perl lc - lowercase"
  (string-downcase (to-string str)))

(defun pl-uc (str)
  "Perl uc - uppercase"
  (string-upcase (to-string str)))

(defun pl-fc (str)
  "Perl fc - fold case for case-insensitive comparison.
   Uses string-downcase as approximation (full Unicode folding would need ICU)."
  (string-downcase (to-string str)))

(defun pl-chomp-single (s)
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

(defun pl-chomp-one (var)
  "Chomp a single variable (helper for pl-chomp)."
  (cond
    ;; Box: chomp its value
    ((pl-box-p var)
     (let* ((s (to-string (pl-box-value var)))
            (result (pl-chomp-single s)))
       (when (> (cdr result) 0)
         (setf (pl-box-value var) (car result)
               (pl-box-sv-ok var) nil))
       (cdr result)))
    ;; Vector (array): chomp each element in place
    ((and (vectorp var) (not (stringp var)))
     (let ((total-removed 0))
       (dotimes (i (length var))
         (let* ((elem (aref var i))
                (s (if (pl-box-p elem)
                       (to-string (pl-box-value elem))
                       (to-string elem)))
                (result (pl-chomp-single s)))
           (when (> (cdr result) 0)
             (if (pl-box-p elem)
                 (setf (pl-box-value elem) (car result)
                       (pl-box-sv-ok elem) nil)
                 (setf (aref var i) (car result)))
             (incf total-removed (cdr result)))))
       total-removed))
    ;; List: chomp each element (must be boxes)
    ((listp var)
     (let ((total-removed 0))
       (dolist (elem var)
         (when (pl-box-p elem)
           (let* ((s (to-string (pl-box-value elem)))
                  (result (pl-chomp-single s)))
             (when (> (cdr result) 0)
               (setf (pl-box-value elem) (car result)
                     (pl-box-sv-ok elem) nil)
               (incf total-removed (cdr result))))))
       total-removed))
    ;; Non-modifiable: return 0
    (t 0)))

(defun pl-chomp (&rest vars)
  "Perl chomp - remove trailing newline, modifies variable(s) in place.
   Returns total number of characters removed.
   Handles multiple arguments: chomp($x, @arr) chomps all."
  (let ((total 0))
    (dolist (var vars total)
      (incf total (pl-chomp-one var)))))

(defun pl-chop-single (s)
  "Chop a single string, returns (new-string . removed-char)"
  (let ((len (length s)))
    (if (> len 0)
        (cons (subseq s 0 (1- len)) (subseq s (1- len)))
        (cons "" ""))))

(defun pl-chop-one (var)
  "Chop a single variable (helper for pl-chop)."
  (cond
    ;; Box: chop its value
    ((pl-box-p var)
     (let* ((s (to-string (pl-box-value var)))
            (result (pl-chop-single s)))
       (setf (pl-box-value var) (car result)
             (pl-box-sv-ok var) nil)
       (cdr result)))
    ;; Vector (array): chop each element in place
    ((and (vectorp var) (not (stringp var)))
     (let ((last-removed ""))
       (dotimes (i (length var))
         (let* ((elem (aref var i))
                (s (if (pl-box-p elem)
                       (to-string (pl-box-value elem))
                       (to-string elem)))
                (result (pl-chop-single s)))
           (if (pl-box-p elem)
               (progn
                 (setf (pl-box-value elem) (car result)
                       (pl-box-sv-ok elem) nil))
               (setf (aref var i) (car result)))
           (setf last-removed (cdr result))))
       last-removed))
    ;; List: chop each element (must be boxes)
    ((listp var)
     (let ((last-removed ""))
       (dolist (elem var)
         (when (pl-box-p elem)
           (let* ((s (to-string (pl-box-value elem)))
                  (result (pl-chop-single s)))
             (setf (pl-box-value elem) (car result)
                   (pl-box-sv-ok elem) nil
                   last-removed (cdr result)))))
       last-removed))
    ;; Non-modifiable: return empty string
    (t "")))

(defun pl-chop (&rest vars)
  "Perl chop - remove last character, modifies variable(s) in place.
   Returns the removed character from the last processed value.
   Handles multiple arguments: chop($x, @arr) chops all."
  (let ((last-removed ""))
    (dolist (var vars last-removed)
      (setf last-removed (pl-chop-one var)))))

(defun pl-index (str substr &optional start)
  "Perl index - find substring.
   Negative start position is treated as 0."
  (let* ((s (to-string str))
         (sub (to-string substr))
         (start-pos (if start (max 0 (truncate (to-number start))) 0)))
    (let ((pos (search sub s :start2 start-pos)))
      (or pos -1))))

(defun pl-rindex (str substr &optional start)
  "Perl rindex - find substring from end.
   Negative start position returns -1.
   Position beyond string length is clamped to string length."
  (let* ((s (to-string str))
         (sub (to-string substr))
         (slen (length s))
         (start-num (if start (truncate (to-number start)) nil)))
    (cond
      ;; Negative position returns -1
      ((and start-num (< start-num 0)) -1)
      ;; Empty substring: return min(position, length)
      ((zerop (length sub))
       (if start-num
           (min start-num slen)
           slen))
      ;; Normal case: search from end
      (t (let* ((end-pos (if start-num
                             (min (+ start-num (length sub)) slen)
                             nil))
                (pos (search sub s :from-end t :end2 end-pos)))
           (or pos -1))))))

(defun pl-chr (n)
  "Perl chr - character from code point.
   Out-of-range values (negative or > 1114111) return U+FFFD replacement char."
  (let ((code (truncate (to-number n))))
    (if (or (< code 0) (> code #x10FFFF))
        (string #\REPLACEMENT_CHARACTER)  ; U+FFFD
        (string (code-char code)))))

(defun pl-ord (str)
  "Perl ord - code point of first character"
  (let ((s (to-string str)))
    (if (> (length s) 0)
        (char-code (char s 0))
        0)))

(defun %strip-underscores (s)
  "Remove underscores from a numeric string (Perl allows _ as visual separator)"
  (remove #\_ s))

(defun pl-hex (str)
  "Perl hex - convert hex string to number.
   Accepts: '0xCAFE', '0XCAFE', 'xCAFE', 'XCAFE', 'CAFE', 'ca_fe'"
  (let* ((s (string-trim '(#\Space #\Tab) (to-string str)))
         (s (cond
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
    (or (parse-integer (%strip-underscores s) :radix 16 :junk-allowed t) 0)))

(defun pl-oct (str)
  "Perl oct - convert octal/hex/binary string to number.
   Recognizes prefixes: 0x/0X (hex), 0b/0B (binary), 0o/0O (octal), 0 (octal).
   Also handles bare x/X, b/B, o/O prefixes."
  (let ((s (string-trim '(#\Space #\Tab) (to-string str))))
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

(defun pl-lcfirst (str)
  "Perl lcfirst - lowercase first character"
  (let ((s (to-string str)))
    (if (> (length s) 0)
        (concatenate 'string (string-downcase (subseq s 0 1)) (subseq s 1))
        s)))

(defun pl-ucfirst (str)
  "Perl ucfirst - uppercase first character"
  (let ((s (to-string str)))
    (if (> (length s) 0)
        (concatenate 'string (string-upcase (subseq s 0 1)) (subseq s 1))
        s)))

(defun pl-quotemeta (str)
  "Perl quotemeta - escape regex metacharacters"
  (cl-ppcre:quote-meta-chars (to-string str)))

;;; Match position tracking for pos()
(defvar *pl-match-pos* (make-hash-table :test 'eq)
  "Hash table mapping boxed strings to their match positions")

(defun pl-pos (var &optional new-pos)
  "Perl pos - get/set match position for /g regex.
   With one arg, returns current position (or nil).
   With two args, sets position and returns new-pos."
  (if new-pos
      ;; Setter: pos($str) = N
      (if (pl-box-p var)
          (setf (gethash var *pl-match-pos*) (truncate (to-number new-pos)))
          new-pos)
      ;; Getter: pos($str)
      (if (pl-box-p var)
          (gethash var *pl-match-pos*)
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
  "Format float as fixed-point with given precision (default 6)."
  (let* ((prec (or precision 6))
         ;; Use CL format with precision - works correctly with any float type
         (s (format nil "~,vF" prec (abs num))))
    ;; CL may produce leading spaces; trim them
    (string-left-trim " " s)))

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

(defun pl-sprintf (fmt &rest args)
  "Perl sprintf - full format string parser.
   Supports: %d %i %u %o %x %X %b %B %e %E %f %F %g %G %s %c %%
   Flags: - + 0 space #
   Width and precision: literal or * (from args)"
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
                        (let ((j (1+ i))
                              (flags "")
                              (width nil)
                              (precision nil))
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
                               (when has-digit (setf width w)))))
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
                                (multiple-value-bind (result new-arg-idx)
                                    (sprintf-one type-char flags width precision args arg-idx)
                                  (write-string result out)
                                  (setf arg-idx new-arg-idx)
                                  (setf i j)))
                              ;; No type char found, output literally
                              (progn
                                (write-string (subseq fmt-str i j) out)
                                (setf i j))))))
                ;; Regular character
                (progn
                  (write-char c out)
                  (incf i)))))))))

(defun pl-printf (&rest args)
  "Perl printf - formatted print (with optional filehandle)"
  (let ((fh *standard-output*)
        (fmt nil)
        (fmt-args nil))
    ;; Check for :fh keyword
    (when (and (>= (length args) 2) (eq (first args) :fh))
      (setf fh (pl-get-stream (second args)))
      (setf args (cddr args)))
    ;; First remaining arg is format, rest are format args
    (setf fmt (first args))
    (setf fmt-args (rest args))
    (princ (apply #'pl-sprintf fmt fmt-args) fh)
    1))

;;; ============================================================
;;; Assignment and Mutation
;;; ============================================================

;;; Distinct assignment forms for each Perl target type.
;;; These make the Perl semantics visible in the generated IR.
;;; pl-setf dispatches to these internally; codegen will emit them directly.

(defmacro pl-scalar-= (place value)
  "Assign to a scalar variable ($var). Auto-declares as global if unbound.
   Reference values (pl-backslash) are stored as box-in-box."
  ;; Check if value is a reference (pl-backslash)
  (if (and (listp value) (eq (car value) 'pl-backslash))
      ;; Reference assignment - store box directly, don't unbox
      (let ((val (gensym "VAL")))
        `(let ((,val ,value))
           (unless (boundp ',place)
             (proclaim '(special ,place))
             (setf (symbol-value ',place) (make-pl-box nil)))
           (setf (pl-box-value ,place) ,val
                 (pl-box-nv-ok ,place) nil
                 (pl-box-sv-ok ,place) nil)
           ,val))
      ;; Normal assignment - use box-set which unboxes
      (let ((val (gensym "VAL")))
        `(let ((,val ,value))
           (unless (boundp ',place)
             (proclaim '(special ,place))
             (setf (symbol-value ',place) (make-pl-box nil)))
           (box-set ,place ,val)))))

(defmacro pl-array-= (place value)
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
                     (vector-push-extend (make-pl-box src) ,place))
                    ((hash-table-p src)
                     (maphash (lambda (k v)
                                (vector-push-extend (make-pl-box k) ,place)
                                (vector-push-extend (make-pl-box (unbox v)) ,place))
                              src))
                    ((vectorp src)
                     (loop for item across src
                           do (if (and (vectorp item) (not (stringp item)))
                                  (add-items item)
                                  (let ((v (unbox item)))
                                    (vector-push-extend (make-pl-box v) ,place)))))
                    ((listp src)
                     (loop for item in src
                           do (if (and (vectorp item) (not (stringp item)))
                                  (add-items item)
                                  (let ((v (unbox item)))
                                    (vector-push-extend (make-pl-box v) ,place))))))))
         (add-items ,val))
       ,place)))

(defmacro pl-hash-= (place value)
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
                       (setf (gethash k ,place) (make-pl-box unboxed))))
                   ,val))
         ((vectorp ,val)
          (loop for i from 0 below (length ,val) by 2
                for v = (aref ,val (1+ i))
                for unboxed = (unbox v)
                do (setf (gethash (to-string (aref ,val i)) ,place) (make-pl-box unboxed)))))
       ,place)))

(defmacro pl-list-= (place value)
  "List destructuring assignment: (pl-list-= (vector $a $b) expr).
   Each LHS element gets assigned from corresponding RHS position.
   Handles undef skip markers and nested lvalues."
  (let ((vars (cdr place))
        (src (gensym "SRC"))
        (src-vec (gensym "SRC-VEC")))
    (let ((forms nil)
          (static-idx 0))
      (dolist (var vars)
        (cond
          ;; Skip marker: (pl-list-x ... N)
          ((and (listp var)
                (symbolp (car var))
                (string= (symbol-name (car var)) "PL-LIST-X")
                (numberp (caddr var)))
           (incf static-idx (caddr var)))
          ;; Skip single undef placeholder
          ((or (eq var '*pl-undef*)
               (and (listp var)
                    (symbolp (car var))
                    (string= (symbol-name (car var)) "PL-UNDEF")))
           (incf static-idx 1))
          ;; Scalar variable - auto-declare and assign
          ((symbolp var)
           (push `(progn
                    (unless (boundp ',var)
                      (proclaim '(special ,var))
                      (setf (symbol-value ',var) (make-pl-box nil)))
                    (box-set ,var (if (< ,static-idx (length ,src-vec))
                                      (aref ,src-vec ,static-idx)
                                      *pl-undef*)))
                 forms)
           (incf static-idx 1))
          ;; Other lvalue (hash/array access, etc.)
          (t
           (push `(pl-setf ,var (if (< ,static-idx (length ,src-vec))
                                     (aref ,src-vec ,static-idx)
                                     *pl-undef*))
                 forms)
           (incf static-idx 1))))
      `(let* ((,src ,value)
              (,src-vec (cond
                          ((hash-table-p ,src)
                           (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
                             (maphash (lambda (k v)
                                        (vector-push-extend k result)
                                        (vector-push-extend (unbox v) result))
                                      ,src)
                             result))
                          ((listp ,src) (coerce ,src 'vector))
                          ((and (vectorp ,src) (not (stringp ,src))) ,src)
                          (t (vector ,src)))))
         ,@(nreverse forms)))))

;; pl-setf dispatches to the appropriate assignment form based on place type.
;; For element access (pl-aref, pl-gethash, etc.), uses CL's setf mechanism.
(defmacro pl-setf (place value)
  "Perl assignment - dispatches to type-specific forms or uses CL setf for element access."
  (cond
    ;; Array variable (symbol starting with @) -> pl-array-=
    ((and (symbolp place)
          (char= (char (symbol-name place) 0) #\@))
     `(pl-array-= ,place ,value))
    ;; Hash variable (symbol starting with %) -> pl-hash-=
    ((and (symbolp place)
          (char= (char (symbol-name place) 0) #\%))
     `(pl-hash-= ,place ,value))
    ;; Simple scalar variable -> pl-scalar-=
    ((symbolp place)
     `(pl-scalar-= ,place ,value))
    ;; Hash access with simple symbol - auto-declare hash if needed
    ((and (listp place)
          (eq (car place) 'pl-gethash)
          (symbolp (cadr place)))
     (let ((hash (cadr place))
           (key (caddr place))
           (val (gensym "VAL")))
       `(let ((,val ,value))
          (unless (boundp ',hash)
            (proclaim '(special ,hash))
            (setf (symbol-value ',hash) (make-hash-table :test 'equal)))
          (setf (pl-gethash ,hash ,key) ,val))))
    ;; Array access with simple symbol - auto-declare array if needed
    ((and (listp place)
          (eq (car place) 'pl-aref)
          (symbolp (cadr place)))
     (let ((arr (cadr place))
           (idx (caddr place))
           (val (gensym "VAL")))
       `(let ((,val ,value))
          (unless (boundp ',arr)
            (proclaim '(special ,arr))
            (setf (symbol-value ',arr) (make-array 0 :adjustable t :fill-pointer 0)))
          (setf (pl-aref ,arr ,idx) ,val))))
    ;; Nested hash access - autovivification
    ;; (pl-gethash (pl-gethash ... ) key) = value
    ((and (listp place)
          (eq (car place) 'pl-gethash)
          (listp (cadr place))
          (eq (car (cadr place)) 'pl-gethash))
     (let ((outer-key (caddr place))
           (val (gensym "VAL")))
       `(let ((,val ,value))
          (pl-autoviv-set ,(cadr place) ,outer-key ,val))))
    ;; Array element in hash chain - autovivification
    ;; (pl-aref (pl-gethash ... ) idx) = value
    ((and (listp place)
          (eq (car place) 'pl-aref)
          (listp (cadr place))
          (eq (car (cadr place)) 'pl-gethash))
     (let ((hash-chain (cadr place))
           (idx (caddr place))
           (val (gensym "VAL")))
       `(let ((,val ,value))
          (pl-autoviv-aref-set ,hash-chain ,idx ,val))))
    ;; Array/hash ref access and scalar deref - use CL setf
    ((and (listp place)
          (member (car place) '(pl-aref-deref pl-gethash-deref pl-$)))
     `(setf ,place ,value))
    ;; Array/hash access with complex expression (not simple symbol) - use CL setf
    ((and (listp place)
          (member (car place) '(pl-aref pl-gethash)))
     `(setf ,place ,value))
    ;; List assignment: (vector $a $b $c) = @_ or similar -> pl-list-=
    ((and (listp place) (eq (car place) 'vector))
     `(pl-list-= ,place ,value))
    ;; Array slice assignment: (pl-setf (pl-aslice arr indices...) values)
    ;; Assigns each value from RHS to the corresponding index in LHS
    ((and (listp place) (eq (car place) 'pl-aslice))
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
                      do (setf (pl-aref ,arr idx)
                               (if (< i (length ,src-vec))
                                   (aref ,src-vec i)
                                   *pl-undef*)))
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
                    do (setf (pl-aref ,arr idx)
                             (if (< i (length ,src-vec))
                                 (aref ,src-vec i)
                                 *pl-undef*)))
              ,src-vec))))
    ;; Hash slice assignment: (pl-setf (pl-hslice hash keys...) values)
    ((and (listp place) (eq (car place) 'pl-hslice))
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
                      do (setf (pl-gethash ,hash k)
                               (if (< i (length ,src-vec))
                                   (aref ,src-vec i)
                                   *pl-undef*)))
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
                    do (setf (pl-gethash ,hash k)
                             (if (< i (length ,src-vec))
                                 (aref ,src-vec i)
                                 *pl-undef*)))
              ,src-vec))))
    ;; substr as lvalue: (pl-setf (pl-substr str start len) val) -> (pl-substr str start len val)
    ((and (listp place) (eq (car place) 'pl-substr))
     (let ((args (cdr place)))
       `(pl-substr ,@args ,value)))
    ;; Other complex place (fallback)
    (t `(box-set ,place ,value))))

(defmacro pl-my (expr)
  "Perl my declaration - just returns the expression"
  expr)

(defmacro pl-incf (place &optional (delta 1))
  "Perl += - works on boxed values, hash/array elements, and derefs"
  (if (and (listp place)
           (member (car place) '(pl-gethash pl-aref pl-gethash-deref pl-aref-deref pl-$)))
      ;; Hash/array element or deref - use incf
      `(incf ,place (to-number ,delta))
      ;; Boxed scalar
      `(box-set ,place (+ (to-number ,place) (to-number ,delta)))))

(defmacro pl-decf (place &optional (delta 1))
  "Perl -= - works on boxed values, hash/array elements, and derefs"
  (if (and (listp place)
           (member (car place) '(pl-gethash pl-aref pl-gethash-deref pl-aref-deref pl-$)))
      ;; Hash/array element or deref - use decf
      `(decf ,place (to-number ,delta))
      ;; Boxed scalar
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
      ;; If it's a string that looks alphanumeric, use magical increment
      ((and (stringp v)
            (> (length v) 0)
            (every (lambda (c) (alphanumericp c)) v))
       (magical-string-increment v))
      ;; Otherwise convert to number and increment
      (t (1+ (to-number v))))))

(defmacro pl-pre++ (place)
  "Perl prefix ++ - works on boxed values, hash/array elements, and derefs.
   Supports magical string increment for alphanumeric strings."
  ;; Handle case where place is wrapped in (vector ...) from list context parsing
  (let ((real-place (if (and (listp place) (eq (car place) 'vector) (= (length place) 2))
                        (cadr place)
                        place)))
    (cond
      ;; Box-returning accessors (pl-aref-box, pl-gethash-box) - get box and modify it
      ((and (listp real-place)
            (member (car real-place) '(pl-aref-box pl-gethash-box)))
       (let ((box (gensym "BOX")))
         `(let* ((,box ,real-place))
            (box-set ,box (perl-increment ,box)))))
      ;; Traditional setf-able places (pl-aref, pl-gethash, etc)
      ((and (listp real-place)
            (member (car real-place) '(pl-gethash pl-aref pl-gethash-deref pl-aref-deref pl-$)))
       (let ((tmp (gensym "TMP")))
         `(let ((,tmp (perl-increment ,real-place)))
            (setf ,real-place ,tmp)
            ,tmp)))
      ;; Boxed scalar
      (t `(box-set ,real-place (perl-increment ,real-place))))))

(defmacro pl-post++ (place)
  "Perl postfix ++ - returns old value.
   Supports magical string increment for alphanumeric strings."
  ;; Handle case where place is wrapped in (vector ...) from list context parsing
  (let* ((real-place (if (and (listp place) (eq (car place) 'vector) (= (length place) 2))
                         (cadr place)
                         place))
         (old (gensym "OLD"))
         (box (gensym "BOX")))
    (cond
      ;; Box-returning accessors (pl-aref-box, pl-gethash-box) - get box and modify it
      ((and (listp real-place)
            (member (car real-place) '(pl-aref-box pl-gethash-box)))
       `(let* ((,box ,real-place)
               (,old (unbox ,box)))
          (box-set ,box (perl-increment ,box))
          ,old))
      ;; Traditional setf-able places (pl-aref, pl-gethash, etc)
      ((and (listp real-place)
            (member (car real-place) '(pl-gethash pl-aref pl-gethash-deref pl-aref-deref pl-$)))
       `(let ((,old ,real-place))
          (setf ,real-place (perl-increment ,real-place))
          ,old))
      ;; Boxed scalar - return the original value (string or number)
      (t (let ((val (gensym "VAL")))
           `(let* ((,val (unbox ,real-place))
                   (,old ,val))
              (box-set ,real-place (perl-increment ,real-place))
              ,old))))))

(defmacro pl-pre-- (place)
  "Perl prefix -- - works on boxed values, hash/array elements, and derefs"
  ;; Handle case where place is wrapped in (vector ...) from list context parsing
  (let ((real-place (if (and (listp place) (eq (car place) 'vector) (= (length place) 2))
                        (cadr place)
                        place)))
    (cond
      ;; Box-returning accessors (pl-aref-box, pl-gethash-box) - get box and modify it
      ((and (listp real-place)
            (member (car real-place) '(pl-aref-box pl-gethash-box)))
       (let ((box (gensym "BOX")))
         `(let* ((,box ,real-place))
            (box-set ,box (1- (to-number ,box))))))
      ;; Traditional setf-able places (pl-aref, pl-gethash, etc)
      ((and (listp real-place)
            (member (car real-place) '(pl-gethash pl-aref pl-gethash-deref pl-aref-deref pl-$)))
       `(decf ,real-place))
      ;; Boxed scalar
      (t `(box-set ,real-place (1- (to-number ,real-place)))))))

(defmacro pl-post-- (place)
  "Perl postfix -- - returns old value"
  ;; Handle case where place is wrapped in (vector ...) from list context parsing
  (let* ((real-place (if (and (listp place) (eq (car place) 'vector) (= (length place) 2))
                         (cadr place)
                         place))
         (old (gensym "OLD"))
         (box (gensym "BOX")))
    (cond
      ;; Box-returning accessors (pl-aref-box, pl-gethash-box) - get box and modify it
      ((and (listp real-place)
            (member (car real-place) '(pl-aref-box pl-gethash-box)))
       `(let* ((,box ,real-place)
               (,old (to-number ,box)))
          (box-set ,box (1- ,old))
          ,old))
      ;; Traditional setf-able places (pl-aref, pl-gethash, etc)
      ((and (listp real-place)
            (member (car real-place) '(pl-gethash pl-aref pl-gethash-deref pl-aref-deref pl-$)))
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

(defmacro pl-*= (place value)
  "Perl *= (multiply-assign)"
  `(box-set ,place (* (to-number ,place) (to-number ,value))))

(defmacro pl-/= (place value)
  "Perl /= (divide-assign)"
  `(box-set ,place (/ (to-number ,place) (to-number ,value))))

(defmacro pl-%= (place value)
  "Perl %= (modulo-assign)"
  `(box-set ,place (mod (truncate (to-number ,place)) (truncate (to-number ,value)))))

(defmacro pl-**= (place value)
  "Perl **= (exponent-assign)"
  `(box-set ,place (expt (to-number ,place) (to-number ,value))))

(defmacro pl-.= (place value)
  "Perl .= (concat-assign)"
  `(box-set ,place (concatenate 'string (to-string ,place) (to-string ,value))))

(defmacro pl-str-x= (place value)
  "Perl x= (repeat-assign)"
  (let ((s (gensym "S"))
        (n (gensym "N")))
    `(let ((,s (to-string ,place))
           (,n (truncate (to-number ,value))))
       (box-set ,place (if (<= ,n 0) ""
                           (apply #'concatenate 'string (make-list ,n :initial-element ,s)))))))

(defmacro pl-bit-and= (place value)
  "Perl &= (bitwise-and-assign)"
  `(box-set ,place (logand (truncate (to-number ,place)) (truncate (to-number ,value)))))

(defmacro pl-bit-or= (place value)
  "Perl |= (bitwise-or-assign)"
  `(box-set ,place (logior (truncate (to-number ,place)) (truncate (to-number ,value)))))

(defmacro pl-bit-xor= (place value)
  "Perl ^= (bitwise-xor-assign)"
  `(box-set ,place (logxor (truncate (to-number ,place)) (truncate (to-number ,value)))))

(defmacro pl-<<= (place value)
  "Perl <<= (left-shift-assign)"
  `(box-set ,place (ash (truncate (to-number ,place)) (truncate (to-number ,value)))))

(defmacro pl->>= (place value)
  "Perl >>= (right-shift-assign)"
  `(box-set ,place (ash (truncate (to-number ,place)) (- (truncate (to-number ,value))))))

(defmacro pl-and-assign (place value)
  "Perl &&= (and-assign) - assigns value only if place is true.
   Returns the box (lvalue) to support chaining."
  (let ((p (gensym "P")))
    `(let ((,p ,place))
       (when (pl-true-p ,p)
         (box-set ,p ,value))
       ,p)))

(defmacro pl-or-assign (place value)
  "Perl ||= (or-assign) - assigns value only if place is false.
   Returns the box (lvalue) to support chaining."
  (let ((p (gensym "P")))
    `(let ((,p ,place))
       (unless (pl-true-p ,p)
         (box-set ,p ,value))
       ,p)))

(defmacro pl-//= (place value)
  "Perl //= (defined-or-assign) - assigns value only if place is undef.
   Returns the box (lvalue) to support chaining."
  (let ((p (gensym "P")))
    `(let ((,p ,place))
       (unless (pl-defined ,p)
         (box-set ,p ,value))
       ,p)))

;;; ============================================================
;;; Numeric Comparison
;;; ============================================================

(defun pl-== (a b)
  "Perl numeric equality"
  (= (to-number a) (to-number b)))

(defun pl-!= (a b)
  "Perl numeric inequality"
  (/= (to-number a) (to-number b)))

(defun pl-< (a b)
  "Perl numeric less than"
  (< (to-number a) (to-number b)))

(defun pl-> (a b)
  "Perl numeric greater than"
  (> (to-number a) (to-number b)))

(defun pl-<= (a b)
  "Perl numeric less than or equal"
  (<= (to-number a) (to-number b)))

(defun pl->= (a b)
  "Perl numeric greater than or equal"
  (>= (to-number a) (to-number b)))

(defun pl-<=> (a b)
  "Perl spaceship operator"
  (let ((na (to-number a))
        (nb (to-number b)))
    (cond ((< na nb) -1)
          ((> na nb) 1)
          (t 0))))

;;; ============================================================
;;; Range Operator
;;; ============================================================

(defun pl-.. (start end)
  "Perl range operator .. - returns a vector from start to end (inclusive).
   Works with numbers, single characters, and multi-character strings
   (magical string increment: 'aa'..'zz', 'A'..'ZZ', etc.)"
  (let ((s (unbox start))
        (e (unbox end)))
    (cond
      ;; Both are numbers (or numeric strings)
      ((and (or (numberp s) (and (stringp s) (ppcre:scan "^-?\\d+$" s)))
            (or (numberp e) (and (stringp e) (ppcre:scan "^-?\\d+$" e))))
       (let ((ns (truncate (to-number s)))
             (ne (truncate (to-number e))))
         (if (<= ns ne)
             (coerce (loop for i from ns to ne collect i) 'vector)
             (make-array 0))))
      ;; String range (single or multi-character) using magical increment
      ((and (stringp s) (stringp e)
            (> (length s) 0) (> (length e) 0))
       (if (> (length s) (length e))
           ;; Start longer than end: empty
           (make-array 0)
           ;; Use magical string increment
           (let ((result (make-array 0 :adjustable t :fill-pointer 0))
                 (current (copy-seq s))
                 (max-len (length e)))
             (loop
               (vector-push-extend current result)
               (when (string= current e) (return))
               (setf current (magical-string-increment current))
               (when (> (length current) max-len) (return)))
             result)))
      ;; Fallback: treat as numbers
      (t
       (let ((ns (truncate (to-number s)))
             (ne (truncate (to-number e))))
         (if (<= ns ne)
             (coerce (loop for i from ns to ne collect i) 'vector)
             (make-array 0)))))))

(defun pl-... (start end)
  "Perl three-dot range operator ... - same as .. in list context."
  (pl-.. start end))

;;; ============================================================
;;; String Comparison
;;; ============================================================

(defun pl-str-eq (a b)
  "Perl string equality (eq)"
  (string= (to-string a) (to-string b)))

(defun pl-str-ne (a b)
  "Perl string inequality (ne)"
  (not (string= (to-string a) (to-string b))))

(defun pl-str-lt (a b)
  "Perl string less than (lt)"
  (if (string< (to-string a) (to-string b)) t nil))

(defun pl-str-gt (a b)
  "Perl string greater than (gt)"
  (if (string> (to-string a) (to-string b)) t nil))

(defun pl-str-le (a b)
  "Perl string less than or equal (le)"
  (if (string<= (to-string a) (to-string b)) t nil))

(defun pl-str-ge (a b)
  "Perl string greater than or equal (ge)"
  (if (string>= (to-string a) (to-string b)) t nil))

(defun pl-str-cmp (a b)
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
  "Convert comparison operator symbol to pl- function symbol.
   Handles both raw symbols and quoted forms.
   e.g., < -> pl-<, (quote <) -> pl-<, eq -> pl-eq"
  (let ((sym (if (and (consp op) (eq (car op) 'quote))
                 (cadr op)  ; extract symbol from (quote sym)
                 op)))
    (intern (format nil "PL-~A" sym) :pcl)))

(defmacro pl-chain-cmp (t1 op1 t2 op2 t3)
  "Chained comparison: $x < $y < $z evaluates each term once.
   Returns true if both comparisons are true."
  (let ((g1 (gensym "T1"))
        (g2 (gensym "T2"))
        (g3 (gensym "T3")))
    `(let ((,g1 ,t1)
           (,g2 ,t2)
           (,g3 ,t3))
       (and (,(cmp-op-to-fn op1) ,g1 ,g2)
            (,(cmp-op-to-fn op2) ,g2 ,g3)))))

;;; ============================================================
;;; Logical Operators
;;; ============================================================

(defmacro pl-&& (a b)
  "Perl short-circuit AND"
  (let ((tmp (gensym)))
    `(let ((,tmp ,a))
       (if (pl-true-p ,tmp) ,b ,tmp))))

(defmacro pl-|| (a b)
  "Perl short-circuit OR"
  (let ((tmp (gensym)))
    `(let ((,tmp ,a))
       (if (pl-true-p ,tmp) ,tmp ,b))))

(defun pl-! (a)
  "Perl logical NOT - returns 1 or empty string like Perl"
  (if (pl-true-p a) "" 1))

(defun pl-not (a)
  "Perl logical NOT (low precedence)"
  (if (pl-true-p a) nil t))

(defmacro pl-and (a b)
  "Perl 'and' operator"
  `(pl-&& ,a ,b))

(defmacro pl-or (a b)
  "Perl 'or' operator"
  `(pl-|| ,a ,b))

(defun pl-xor (a b)
  "Perl 'xor' operator"
  (let ((ta (pl-true-p a))
        (tb (pl-true-p b)))
    (if (or (and ta (not tb)) (and (not ta) tb)) t nil)))

(defmacro pl-// (a b)
  "Perl defined-or operator"
  (let ((tmp (gensym)))
    `(let ((,tmp ,a))
       (if (pl-defined ,tmp) ,tmp ,b))))

;;; ============================================================
;;; Bitwise Operators
;;; ============================================================

(defun pl-bit-and (a b)
  "Perl bitwise AND"
  (logand (truncate (to-number a)) (truncate (to-number b))))

(defun pl-bit-or (a b)
  "Perl bitwise OR"
  (logior (truncate (to-number a)) (truncate (to-number b))))

(defun pl-bit-xor (a b)
  "Perl bitwise XOR"
  (logxor (truncate (to-number a)) (truncate (to-number b))))

(defun pl-bit-not (a)
  "Perl bitwise NOT - mask to 64 bits like Perl's UV"
  (logand (lognot (truncate (to-number a))) #xFFFFFFFFFFFFFFFF))

(defun pl-<< (a b)
  "Perl left shift"
  (ash (truncate (to-number a)) (truncate (to-number b))))

(defun pl->> (a b)
  "Perl right shift"
  (ash (truncate (to-number a)) (- (truncate (to-number b)))))

;;; ============================================================
;;; Data Structures - Arrays
;;; ============================================================

(defun pl-aref (arr idx)
  "Perl array access (supports negative indices, works on vectors and lists).
   Returns the VALUE (unboxed if element is a box)."
  (let* ((a (unbox arr)))  ; Unbox if needed
    ;; If array is undef (from failed hash lookup etc), return undef
    (when (eq a *pl-undef*)
      (return-from pl-aref *pl-undef*))
    (let* ((i (truncate (to-number idx)))
           (len (cond ((vectorp a) (length a))
                      ((listp a) (length a))
                      (t 0)))
           (actual-idx (if (< i 0) (+ len i) i)))
      (cond
        ((and (vectorp a) (>= actual-idx 0) (< actual-idx len))
         (let ((elem (aref a actual-idx)))
           ;; Unbox if stored as a box (l-value semantics)
           (unbox elem)))
        ((and (listp a) (>= actual-idx 0) (< actual-idx len))
         (let ((elem (nth actual-idx a)))
           (unbox elem)))
        (t *pl-undef*)))))

(defun (setf pl-aref) (value arr idx)
  "Setf expander for pl-aref - allows assignment to array elements.
   Auto-extends array if index is beyond current length (Perl semantics).
   Stores values in boxes for l-value semantics. Returns the box."
  (let* ((i (truncate (to-number idx)))
         (len (if (vectorp arr) (length arr) 0))
         (actual-idx (if (< i 0) (+ len i) i)))
    (when (and (vectorp arr) (>= actual-idx 0))
      ;; Auto-extend array if needed (Perl autovivification)
      (when (>= actual-idx len)
        (dotimes (n (1+ (- actual-idx len)))
          (vector-push-extend (make-pl-box *pl-undef*) arr)))
      ;; Get or create box at this index
      (let ((box (aref arr actual-idx)))
        (unless (pl-box-p box)
          (setf box (make-pl-box nil))
          (setf (aref arr actual-idx) box))
        ;; Set the box's value and return the box
        (box-set box value)))))

(defun pl-aref-box (arr idx)
  "Get the BOX at array index (for l-value operations like chop, ++).
   Creates box if needed, auto-extends array. Returns the box itself."
  (let* ((a (unbox arr)))
    ;; If array is undef, can't get box from it
    (when (eq a *pl-undef*)
      (return-from pl-aref-box (make-pl-box *pl-undef*)))
    (let* ((i (truncate (to-number idx)))
           (len (if (vectorp a) (length a) 0))
           (actual-idx (if (< i 0) (+ len i) i)))
      (when (and (vectorp a) (>= actual-idx 0))
        ;; Auto-extend array if needed
        (when (>= actual-idx len)
          (dotimes (n (1+ (- actual-idx len)))
            (vector-push-extend (make-pl-box *pl-undef*) a)))
        ;; Ensure box exists at this index
        (let ((elem (aref a actual-idx)))
          (unless (pl-box-p elem)
            (setf elem (make-pl-box elem))
            (setf (aref a actual-idx) elem))
          (return-from pl-aref-box elem)))
      ;; Out of bounds or not a vector
      (make-pl-box *pl-undef*))))

(defun pl-aref-deref (ref idx)
  "Perl array ref access $ref->[idx] - unbox the reference first"
  (pl-aref (unbox ref) idx))

(defun pl-array-last-index (arr)
  "Perl $#arr - last index"
  (if (vectorp arr)
      (1- (length arr))
      -1))

(defun pl-set-array-length (arr new-last-index)
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
         (vector-push-extend (make-pl-box *pl-undef*) a)))
      ((< new-len cur-len)
       ;; Shrink: adjust fill-pointer (minimum 0)
       (setf (fill-pointer a) (max 0 new-len))))
    nli))

(defmacro pl-push (arr &rest items)
  "Perl push - adds to end of array, auto-declares if needed"
  (if (symbolp arr)
      ;; Simple array variable: ensure declared
      `(progn
         (unless (boundp ',arr)
           (proclaim '(special ,arr))
           (setf (symbol-value ',arr) (make-array 0 :adjustable t :fill-pointer 0)))
         (pl-push-impl ,arr ,@items))
      ;; Complex place
      `(pl-push-impl ,arr ,@items)))

;; Marker struct for flattened arrays in push/unshift
(defstruct pl-flatten-marker
  "Marker indicating an array should be flattened when pushed/unshifted"
  array)

(defun pl-flatten (arr)
  "Mark an array for flattening in push/unshift.
   Called at code-gen time for @array arguments."
  (make-pl-flatten-marker :array (unbox arr)))

(defun pl-push-impl (arr &rest items)
  "Implementation of push - stores values in boxes for l-value semantics.
   Recognizes pl-flatten-marker to flatten @array arguments."
  (dolist (item items)
    (let ((val (unbox item)))
      (cond
        ;; Flatten marker - push each element of the marked array
        ((pl-flatten-marker-p val)
         (let ((src (pl-flatten-marker-array val)))
           (when (vectorp src)
             (loop for elem across src do
                   ;; Unbox if element is boxed, then create new box
                   (let ((v (unbox elem)))
                     (vector-push-extend (make-pl-box v) arr))))))
        ;; Regular value - wrap in box and push
        (t (vector-push-extend (make-pl-box val) arr)))))
  (length arr))

(defun pl-pop (arr)
  "Perl pop - removes from end, returns the VALUE (unboxed)"
  (if (and (vectorp arr) (> (length arr) 0))
      (let ((elem (vector-pop arr)))
        ;; Unbox if element is a box
        (unbox elem))
      *pl-undef*))

(defun pl-shift (arr)
  "Perl shift - removes from front. Works with vectors and lists.
   Returns the VALUE (unboxed)."
  (cond
    ((and (vectorp arr) (> (length arr) 0))
     (let ((first (aref arr 0)))
       ;; Shift elements down
       (loop for i from 0 below (1- (length arr))
             do (setf (aref arr i) (aref arr (1+ i))))
       (vector-pop arr)
       ;; Unbox if element is a box
       (unbox first)))
    ((consp arr)
     ;; For lists (like @_ from &rest), just return car (unboxed)
     (let ((first (car arr)))
       (unbox first)))
    (t *pl-undef*)))

(defun pl-unshift (arr &rest items)
  "Perl unshift - adds to front. Stores values in boxes for l-value semantics.
   Recognizes pl-flatten-marker to flatten @array arguments."
  ;; First expand any flatten markers into a flat list of VALUES (not boxes)
  (let ((flat-items
          (loop for item in items
                for val = (unbox item)
                if (pl-flatten-marker-p val)
                  ;; Flatten marker - expand its array, unboxing elements
                  append (loop for elem across (pl-flatten-marker-array val)
                               collect (unbox elem))
                else
                  ;; Regular value
                  collect val)))
    (let ((nitems (length flat-items)))
      ;; Make room with placeholder boxes
      (dotimes (i nitems)
        (vector-push-extend (make-pl-box *pl-undef*) arr))
      ;; Shift existing elements up
      (loop for i from (1- (length arr)) downto nitems
            do (setf (aref arr i) (aref arr (- i nitems))))
      ;; Insert new items at front (in boxes)
      (loop for i from 0
            for item in flat-items
            do (setf (aref arr i) (make-pl-box item)))
      (length arr))))

(defun pl-splice (arr &optional (offset 0) (length nil length-p) &rest replacements)
  "Perl splice: remove and/or replace elements in an array.
   Returns removed elements as a vector."
  (let* ((a (unbox arr))
         (alen (length a))
         ;; Handle negative offset
         (off (if (< offset 0) (max 0 (+ alen offset)) (min offset alen)))
         ;; Default length = remove everything from offset
         (len (if length-p (min (max length 0) (- alen off)) (- alen off)))
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
                    do (vector-push-extend (make-pl-box nil) a))
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
              do (setf (aref a i) (make-pl-box v)))))
    removed))

;;; ============================================================
;;; Data Structures - Hashes
;;; ============================================================

(defun pl-gethash (hash key)
  "Perl hash access. Special handling for %ENV and %INC.
   Returns the VALUE (unboxed if element is a box)."
  (let* ((h (unbox hash))
         (k (to-string key)))
    ;; If hash is undef (from failed lookup), return undef
    (when (eq h *pl-undef*)
      (return-from pl-gethash *pl-undef*))
    ;; Check for special markers
    (cond
      ((eq h '%ENV-MARKER%)
       (or (sb-posix:getenv k) *pl-undef*))
      ((eq h '%INC-MARKER%)
       (multiple-value-bind (val found) (gethash k *pl-inc-table*)
         (if found val *pl-undef*)))
      (t
       (multiple-value-bind (val found) (gethash k h)
         (if (not found)
             *pl-undef*
             ;; Unbox if stored as a box (l-value semantics)
             (unbox val)))))))

(defun (setf pl-gethash) (value hash key)
  "Setf expander for pl-gethash - allows assignment to hash elements.
   Special handling for %ENV and %INC.
   Stores values in boxes for l-value semantics. Returns the box."
  (let* ((h (unbox hash))
         (k (to-string key)))
    (cond
      ((eq h '%ENV-MARKER%)
       (sb-posix:setenv k (to-string value) 1)
       value)
      ((eq h '%INC-MARKER%)
       (setf (gethash k *pl-inc-table*) value))
      (t
       ;; Get or create box at this key
       (multiple-value-bind (existing found) (gethash k h)
         (let ((box (if (and found (pl-box-p existing))
                        existing
                        (make-pl-box nil))))
           (unless (and found (pl-box-p existing))
             (setf (gethash k h) box))
           ;; Set the box's value and return the box
           (box-set box value)))))))

(defun pl-gethash-box (hash key)
  "Get the BOX at hash key (for l-value operations like chop, ++).
   Creates box if needed (autovivification). Returns the box itself."
  (let* ((h (unbox hash))
         (k (to-string key)))
    ;; If hash is undef, can't get box from it
    (when (eq h *pl-undef*)
      (return-from pl-gethash-box (make-pl-box *pl-undef*)))
    ;; Special markers don't support boxing
    (when (or (eq h '%ENV-MARKER%) (eq h '%INC-MARKER%))
      (return-from pl-gethash-box (make-pl-box *pl-undef*)))
    ;; Get or create box at this key
    (multiple-value-bind (existing found) (gethash k h)
      (if (and found (pl-box-p existing))
          existing
          (let ((box (make-pl-box (if found existing *pl-undef*))))
            (setf (gethash k h) box)
            box)))))

(defun pl-autoviv-gethash (hash key)
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

(defun pl-autoviv-gethash-for-array (hash key)
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

(defun pl-autoviv-aref-for-hash (arr idx)
  "Get array element, autovivifying to empty hash if missing.
   Handles boxes in array elements."
  (let* ((a (unbox arr))
         (i (truncate idx)))
    ;; Extend array if needed
    (when (>= i (length a))
      (loop for j from (length a) to i
            do (vector-push-extend (make-pl-box *pl-undef*) a)))
    (let* ((stored (aref a i))
           ;; Unbox if element is a box
           (val (unbox stored)))
      (if (hash-table-p val)
          val
          ;; Autovivify: create new hash and store it
          (let ((new-hash (make-hash-table :test 'equal)))
            (setf (aref a i) new-hash)
            new-hash)))))

(defun pl-autoviv-aref-for-array (arr idx)
  "Get array element, autovivifying to empty array if missing.
   Handles boxes in array elements."
  (let* ((a (unbox arr))
         (i (truncate idx)))
    ;; Extend array if needed
    (when (>= i (length a))
      (loop for j from (length a) to i
            do (vector-push-extend (make-pl-box *pl-undef*) a)))
    (let* ((stored (aref a i))
           ;; Unbox if element is a box
           (val (unbox stored)))
      (if (vectorp val)
          val
          ;; Autovivify: create new array and store it
          (let ((new-arr (make-array 0 :adjustable t :fill-pointer 0)))
            (setf (aref a i) new-arr)
            new-arr)))))

(defun pl-array-set (arr idx value)
  "Set array element, extending array if needed.
   Stores values in boxes for l-value semantics."
  (let* ((a (unbox arr))
         (i (truncate idx)))
    ;; Extend array if needed
    (when (>= i (length a))
      (loop for j from (length a) to i
            do (vector-push-extend (make-pl-box *pl-undef*) a)))
    ;; Get or create box at this index
    (let ((box (aref a i)))
      (unless (pl-box-p box)
        (setf box (make-pl-box nil))
        (setf (aref a i) box))
      (box-set box value))))

(defmacro pl-autoviv-set (inner-hash-form outer-key value)
  "Set value with autovivification for nested hash access.
   inner-hash-form is (pl-gethash hash inner-key) or deeper.
   Expands to code that ensures intermediate hashes exist."
  (let ((val-var (gensym "VAL"))
        (hash-var (gensym "HASH")))
    `(let ((,val-var ,value)
           (,hash-var ,(expand-autoviv inner-hash-form)))
       (setf (gethash (to-string ,outer-key) ,hash-var) ,val-var))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-autoviv (form)
    "Compile-time helper to expand nested gethash into autovivifying code.
     Creates hashes at each level (for hash access chain)."
    (cond
      ;; (pl-gethash inner key) - autovivify to hash
      ((and (listp form) (eq (car form) 'pl-gethash))
       (let ((inner (cadr form))
             (key (caddr form)))
         (if (or (and (listp inner) (eq (car inner) 'pl-gethash))
                 (and (listp inner) (eq (car inner) 'pl-aref)))
             ;; Nested: recursively expand, then autoviv this level
             `(pl-autoviv-gethash ,(expand-autoviv inner) ,key)
             ;; Base case: inner is actual hash
             `(pl-autoviv-gethash ,inner ,key))))
      ;; (pl-aref inner idx) - intermediate array, this slot yields hash
      ((and (listp form) (eq (car form) 'pl-aref))
       (let ((inner (cadr form))
             (idx (caddr form)))
         `(pl-autoviv-aref-for-hash ,(expand-autoviv-for-array inner) ,idx)))
      ;; Not a recognized form
      (t form)))

  (defun expand-autoviv-for-array (form)
    "Expand form knowing result must be an array."
    (cond
      ;; (pl-gethash inner key) - this slot yields array
      ((and (listp form) (eq (car form) 'pl-gethash))
       (let ((inner (cadr form))
             (key (caddr form)))
         (if (or (and (listp inner) (eq (car inner) 'pl-gethash))
                 (and (listp inner) (eq (car inner) 'pl-aref)))
             `(pl-autoviv-gethash-for-array ,(expand-autoviv inner) ,key)
             `(pl-autoviv-gethash-for-array ,inner ,key))))
      ;; (pl-aref inner idx) - this slot yields array
      ((and (listp form) (eq (car form) 'pl-aref))
       (let ((inner (cadr form))
             (idx (caddr form)))
         `(pl-autoviv-aref-for-array ,(expand-autoviv-for-array inner) ,idx)))
      ;; Not a recognized form
      (t form))))

(defmacro pl-autoviv-aref-set (hash-chain idx value)
  "Set array element in a hash chain with autovivification.
   hash-chain is like (pl-gethash ... key) and should yield an array.
   Expands to code that ensures intermediate structures exist."
  (let ((val-var (gensym "VAL"))
        (arr-var (gensym "ARR")))
    `(let ((,val-var ,value)
           (,arr-var ,(expand-autoviv-for-array hash-chain)))
       (pl-array-set ,arr-var ,idx ,val-var))))

(defun pl-gethash-deref (ref key)
  "Perl hash ref access $ref->{key} - unbox the reference first"
  (pl-gethash (unbox ref) key))

(defun (setf pl-gethash-deref) (value ref key)
  "Setf expander for pl-gethash-deref - unbox the reference first"
  (setf (pl-gethash (unbox ref) key) value))

(defun (setf pl-aref-deref) (value ref idx)
  "Setf expander for pl-aref-deref - unbox the reference first"
  (setf (pl-aref (unbox ref) idx) value))

(defun pl-aslice (arr &rest indices)
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
      (vector-push-extend (pl-aref arr idx) result))))

(defun pl-hslice (hash &rest keys)
  "Perl hash slice @hash{keys} - returns vector of values.
   Handles individual keys, lists, and vectors (from range operator)."
  (let ((flat-keys (loop for key in keys
                         if (vectorp key)
                           append (coerce key 'list)
                         else if (and (listp key) (not (null key)))
                           append key
                         else
                           collect key))
        (result (make-array 0 :adjustable t :fill-pointer 0)))
    (dolist (key flat-keys result)
      (vector-push-extend (pl-gethash hash key) result))))

(defun pl-kv-hslice (hash &rest keys)
  "Perl KV hash slice %hash{keys} - returns vector of key-value pairs.
   Handles individual keys, lists, and vectors (from range operator)."
  (let ((flat-keys (loop for key in keys
                         if (vectorp key)
                           append (coerce key 'list)
                         else if (and (listp key) (not (null key)))
                           append key
                         else
                           collect key))
        (result (make-array 0 :adjustable t :fill-pointer 0)))
    (dolist (key flat-keys result)
      (let ((k (to-string key)))
        (vector-push-extend k result)
        (vector-push-extend (pl-gethash hash k) result)))))

(defun pl-hash (&rest pairs)
  "Create a Perl hash from key-value pairs.
   Stores values in boxes for l-value semantics."
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash (to-string k) h) (make-pl-box (unbox v))))
    h))

(defun pl-array-init (&rest elements)
  "Create a Perl array (adjustable vector) from elements.
   Flattens any nested arrays/vectors (but not strings) to handle
   expressions like [(@x) x 2] correctly.
   Stores elements in boxes for l-value semantics."
  (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
    (labels ((add-element (e)
               (cond
                 ;; String - wrap in box and add
                 ((stringp e)
                  (vector-push-extend (make-pl-box e) result))
                 ;; Vector (array) - flatten its contents (unboxing then re-boxing)
                 ((vectorp e)
                  (loop for item across e
                        for val = (unbox item)
                        do (vector-push-extend (make-pl-box val) result)))
                 ;; List - flatten its contents (unboxing then re-boxing)
                 ((listp e)
                  (loop for item in e
                        for val = (unbox item)
                        do (vector-push-extend (make-pl-box val) result)))
                 ;; Scalar value - wrap in box
                 (t
                  (vector-push-extend (make-pl-box e) result)))))
      (dolist (elem elements)
        (add-element elem)))
    result))

;; Hash iterator state for each() - maps hash-table to list of remaining keys
(defvar *hash-iterators* (make-hash-table :test 'eq))

(defun pl-each (hash)
  "Perl each function - returns next (key, value) pair from hash.
   Returns an empty list when exhausted. Resets on first call after exhaustion."
  (let ((remaining (gethash hash *hash-iterators*)))
    ;; If no iterator exists or previous was exhausted, start fresh
    (when (null remaining)
      (let ((keys nil))
        (maphash (lambda (k v) (declare (ignore v)) (push k keys)) hash)
        (setf remaining (nreverse keys))
        (setf (gethash hash *hash-iterators*) remaining)))
    ;; If still empty (hash has no keys), return empty list
    (if (null remaining)
        (vector)
        (let* ((key (car remaining))
               (val (gethash key hash)))
          (setf (gethash hash *hash-iterators*) (cdr remaining))
          ;; When iterator is exhausted, remove entry so next call starts fresh
          (when (null (cdr remaining))
            (remhash hash *hash-iterators*))
          (vector key (unbox val))))))

(defun pl-keys (hash)
  "Perl keys function - also resets the each() iterator"
  ;; Reset each() iterator
  (remhash hash *hash-iterators*)
  (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
    (maphash (lambda (k v)
               (declare (ignore v))
               (vector-push-extend k result))
             hash)
    result))

(defun pl-values (hash)
  "Perl values function - returns unboxed values"
  (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
    (maphash (lambda (k v)
               (declare (ignore k))
               ;; Unbox values stored in boxes
               (vector-push-extend (unbox v) result))
             hash)
    result))

(defun pl-exists (hash key)
  "Perl exists function"
  (multiple-value-bind (val found) (gethash (to-string key) hash)
    (declare (ignore val))
    found))

(defun pl-delete (hash key)
  "Perl delete function for hashes - returns unboxed value"
  (let ((k (to-string key)))
    (multiple-value-bind (v found) (gethash k hash)
      (remhash k hash)
      (if found
          (unbox v)
          *pl-undef*))))

(defun pl-delete-array (arr idx)
  "Perl delete function for arrays.
   Sets element to undef and returns the old value (unboxed).
   Unlike hash delete, this doesn't shrink the array."
  (let* ((i (truncate (to-number idx)))
         (old-val (if (and (>= i 0) (< i (length arr)))
                      (let ((elem (aref arr i)))
                        (unbox elem))
                      *pl-undef*)))
    (when (and (>= i 0) (< i (length arr)))
      ;; Set box to undef, or replace with undef box
      (let ((elem (aref arr i)))
        (if (pl-box-p elem)
            (setf (pl-box-value elem) *pl-undef*)
            (setf (aref arr i) (make-pl-box *pl-undef*)))))
    old-val))

(defun pl-exists-array (arr idx)
  "Perl exists function for arrays.
   Returns true if the element exists (index is within bounds and was assigned)."
  (let ((i (truncate (to-number idx))))
    (and (>= i 0) (< i (length arr)))))

(defun pl-delete-hash-slice (hash &rest keys)
  "Perl delete for hash slices: delete @hash{k1, k2, ...}
   Deletes multiple keys and returns a list of the deleted values."
  (let ((result (make-array (length keys) :adjustable t :fill-pointer 0)))
    (dolist (key keys)
      (let ((k (to-string key)))
        (vector-push-extend (gethash k hash *pl-undef*) result)
        (remhash k hash)))
    result))

(defun pl-delete-kv-hash-slice (hash &rest keys)
  "Perl delete for KV hash slices: delete %hash{k1, k2, ...}
   Deletes multiple keys and returns key-value pairs."
  (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
    (dolist (key keys)
      (let ((k (to-string key)))
        (vector-push-extend k result)
        (vector-push-extend (gethash k hash *pl-undef*) result)
        (remhash k hash)))
    result))

(defun pl-delete-array-slice (arr &rest indices)
  "Perl delete for array slices: delete @arr[i1, i2, ...]
   Sets elements to undef and returns a list of the old values."
  (let ((result (make-array (length indices) :adjustable t :fill-pointer 0)))
    (dolist (idx indices)
      (let* ((i (truncate (to-number idx)))
             (old-val (if (and (>= i 0) (< i (length arr)))
                          (aref arr i)
                          *pl-undef*)))
        (when (and (>= i 0) (< i (length arr)))
          (setf (aref arr i) *pl-undef*))
        (vector-push-extend old-val result)))
    result))

(defun pl-stash (pkg-name)
  "Return package stash (symbol table) as a hash.
   This is a simplified stub - full implementation would mirror Perl's stash."
  (declare (ignore pkg-name))
  ;; Return an empty hash for now - stash manipulation is rarely essential
  (make-hash-table :test 'equal))

;;; ============================================================
;;; Control Flow
;;; ============================================================

(defmacro pl-if (condition then-form &optional else-form)
  "Perl if/unless and ternary"
  `(if (pl-true-p ,condition) ,then-form ,else-form))

(defmacro pl-unless (condition then-form &optional else-form)
  "Perl unless"
  `(if (not (pl-true-p ,condition)) ,then-form ,else-form))

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

(defmacro pl-while (condition &rest body-and-keys)
  "Perl while loop with optional :label and :continue."
  (multiple-value-bind (label continue-form body) (parse-loop-keys body-and-keys)
    (let ((block-name (or label (gensym "WHILE"))))
      `(block ,block-name
         (loop while (pl-true-p ,condition)
               do ,(make-loop-iteration-body label body)
                  ,@(when continue-form (list continue-form))
               ,@(when label `(finally (return-from ,block-name nil))))))))

(defmacro pl-until (condition &body body)
  "Perl until loop"
  `(pl-while (pl-! ,condition) ,@body))

(defmacro pl-for ((init) (test) (&optional step) &rest body-and-keys)
  "Perl C-style for loop with optional :label."
  (multiple-value-bind (label _continue body) (parse-loop-keys body-and-keys)
    (declare (ignore _continue))
    (let ((block-name (or label (gensym "FOR"))))
      `(block ,block-name
         ,init
         (loop while (pl-true-p ,test)
               do ,(make-loop-iteration-body label body)
                  ,@(when step (list step))
               ,@(when label `(finally (return-from ,block-name nil))))))))

(defun ensure-vector (val)
  "Ensure value is a vector for iteration. Non-vectors become single-element vectors."
  (cond
    ((vectorp val) val)
    ((listp val) (coerce val 'vector))
    (t (vector val))))

(defmacro pl-foreach ((var list) &rest body-and-keys)
  "Perl foreach loop with optional :label and :continue."
  (multiple-value-bind (label continue-form body) (parse-loop-keys body-and-keys)
    (let ((block-name (or label (gensym "FOREACH")))
          (item (gensym))
          (vec (gensym))
          (raw (gensym)))
      `(block ,block-name
         (let* ((*wantarray* t)
                (,raw ,list)
                (,vec (ensure-vector (unbox ,raw))))
           (loop for ,item across ,vec
                 do (let ((,var ,item))
                      ,(make-loop-iteration-body label body)
                      ,@(when continue-form (list continue-form)))
                 ,@(when label `(finally (return-from ,block-name nil)))))))))

(defun pl-return-value (val)
  "Prepare a value for return - unbox simple scalars but keep references intact."
  (cond
    ;; Not a box - return as-is (hash tables, arrays, etc.)
    ((not (pl-box-p val)) val)
    ;; Box containing a reference (hash, array, function) - return the reference
    ((let ((v (pl-box-value val)))
       (or (hash-table-p v) (vectorp v) (functionp v)))
     (pl-box-value val))
    ;; Simple scalar box - return the unboxed value
    (t (unbox val))))

(defmacro pl-return (&rest values)
  "Perl return - returns single value or list depending on args.
   Unboxes simple scalars but keeps references intact."
  (if (null values)
      `(return-from nil nil)
      (if (= (length values) 1)
          `(return-from nil (pl-return-value ,(car values)))
          `(return-from nil (mapcar #'pl-return-value (list ,@values))))))

(defmacro pl-last (&optional label)
  "Perl last (break) - optionally with label to exit specific loop"
  (if label
      `(return-from ,label nil)
      `(return nil)))

(defmacro pl-next (&optional label)
  "Perl next (continue) - optionally with label to continue specific loop"
  (if label
      `(throw ',(intern (format nil "NEXT-~A" label) :pcl) nil)
      `(go :next)))

(defmacro pl-redo (&optional label)
  "Perl redo - optionally with label to redo specific loop"
  (if label
      `(throw ',(intern (format nil "REDO-~A" label) :pcl) nil)
      `(go :redo)))

;;; ============================================================
;;; I/O Functions
;;; ============================================================

(defun pl-print (&rest args)
  "Perl print - prints args then appends $\\ (output record separator)"
  (let ((fh *standard-output*))
    ;; Check for :fh keyword
    (when (and (>= (length args) 2) (eq (first args) :fh))
      (setf fh (pl-get-stream (second args)))
      (setf args (cddr args)))
    (dolist (arg args)
      (princ (to-string arg) fh))
    ;; Append output record separator $\ if set
    (let ((ors (unbox |$\\|)))
      (when (and (stringp ors) (plusp (length ors)))
        (princ ors fh)))
    t))

(defun pl-say (&rest args)
  "Perl say (print with newline)"
  (let ((fh *standard-output*))
    ;; Check for :fh keyword to get the right stream
    (when (and (>= (length args) 2) (eq (first args) :fh))
      (setf fh (pl-get-stream (second args))))
    (apply #'pl-print args)
    (terpri fh)
    t))

(defun pl-warn (&rest args)
  "Perl warn"
  (apply #'pl-print (append args (list #\Newline)))
  (force-output *error-output*))

;;; Exception condition for object-based die
;;; When Perl dies with a blessed reference, we preserve it in $@
(define-condition pl-exception (error)
  ((object :initarg :object :reader pl-exception-object))
  (:report (lambda (c s)
             (format s "~A" (pl-exception-object c)))))

(defun pl-die (&rest args)
  "Perl die - throw an exception.
   If given a single blessed reference, throw it as an exception object.
   Otherwise, concatenate args as error string."
  (if (and (= (length args) 1)
           (let ((obj (car args)))
             ;; Check if it's a blessed hash or blessed box
             (or (and (hash-table-p obj) (gethash :__class__ obj))
                 (and (pl-box-p obj)
                      (let ((inner (pl-box-value obj)))
                        (or (pl-box-class obj)
                            (and (hash-table-p inner) (gethash :__class__ inner))))))))
      ;; Object exception - preserve for $@
      (error 'pl-exception :object (car args))
      ;; String exception
      (error (apply #'pl-. args))))

;;; pl-do - Perl's do BLOCK
;;; The block is already evaluated by CL, so this is identity.
(defun pl-do (result)
  "Perl do BLOCK - returns the value of the block."
  result)

;;; pl-eval (string eval) - simplified version
;;; Full string eval would need transpiler at runtime.
;;; This stub handles simple cases like version string evaluation.
(defun pl-eval (string)
  "Simplified Perl eval(string) - handles simple numeric expressions.
   Full implementation would need runtime transpiler."
  (let ((s (to-string (unbox string))))
    (handler-case
        (progn
          (setf $@ "")
          ;; Try to parse as number (handles version strings like '1.50')
          (let ((n (parse-number s)))
            (if n n s)))
      (error (e)
        (setf $@ (format nil "~A" e))
        nil))))

(defun parse-number (s)
  "Try to parse string as number, return nil if not a number."
  (handler-case
      (let ((val (read-from-string s)))
        (if (numberp val) val nil))
    (error () nil)))

;;; pl-eval-block: Execute code catching errors (Perl's eval { })
;;; Sets $@ to error message on failure, empty string on success.
;;; Returns nil on error, block result on success.
(defmacro pl-eval-block (&body body)
  "Perl eval { } - execute body catching errors.
   Sets $@ to error/exception on failure, empty string on success.
   Returns result of body on success, nil on failure."
  `(handler-case
       (prog1 (progn ,@body)
         (setf $@ ""))
     (pl-exception (e)
       ;; Object exception - preserve the object in $@
       (setf $@ (pl-exception-object e))
       nil)
     (error (e)
       ;; String exception - convert to string
       (setf $@ (format nil "~A" e))
       nil)))

;;; ============================================================
;;; File I/O Functions
;;; ============================================================

;; Filehandle storage - maps symbols to CL streams
(defvar *pl-filehandles* (make-hash-table :test 'eq))

;; Standard filehandles
(setf (gethash 'STDIN *pl-filehandles*) *standard-input*)
(setf (gethash 'STDOUT *pl-filehandles*) *standard-output*)
(setf (gethash 'STDERR *pl-filehandles*) *error-output*)

(defun pl-get-stream (fh)
  "Get CL stream from Perl filehandle (symbol or stream)"
  (cond
    ((streamp fh) fh)
    ((symbolp fh) (gethash fh *pl-filehandles*))
    ((pl-box-p fh) (pl-box-value fh))
    (t fh)))

(defun %pl-open-parse-2arg (expr)
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

(defun %pl-open-impl (fh mode filename)
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
      (setf (gethash fh *pl-filehandles*) stream))
    (if stream t nil)))

(defmacro pl-open (fh mode &optional filename)
  "Perl open - open file with given mode.
   2-arg: (pl-open FH expr) - mode is parsed from expr
   3-arg: (pl-open FH mode filename)"
  (if filename
      `(%pl-open-impl ',fh ,mode ,filename)
      `(let ((%parsed (%pl-open-parse-2arg ,mode)))
         (%pl-open-impl ',fh (car %parsed) (cdr %parsed)))))

(defun %pl-close-impl (fh)
  "Implementation of Perl close"
  (let ((stream (pl-get-stream fh)))
    (when stream
      (close stream)
      (remhash fh *pl-filehandles*)
      t)))

(defmacro pl-close (fh)
  "Perl close - close filehandle"
  `(%pl-close-impl ',fh))

(defun pl-eof (&optional fh)
  "Perl eof - check end of file"
  (let ((stream (if fh (pl-get-stream fh) *standard-input*)))
    (if stream
        (let ((ch (peek-char nil stream nil :eof)))
          (if (eq ch :eof) t nil))
        t)))

(defun pl-tell (&optional fh)
  "Perl tell - return current file position"
  (let ((stream (if fh (pl-get-stream fh) *standard-input*)))
    (if stream
        (file-position stream)
        -1)))

(defun pl-seek (fh pos whence)
  "Perl seek - seek to position. Whence: 0=start, 1=current, 2=end"
  (let ((stream (pl-get-stream fh))
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

(defun pl-binmode (fh &optional encoding)
  "Perl binmode - set binary mode or encoding (stub)"
  (declare (ignore fh encoding))
  ;; CL handles this differently - just return true
  t)

(defun pl-read (fh buf len &optional offset)
  "Perl read - read bytes into buffer"
  (declare (ignore buf offset))  ; Buffer semantics differ in CL
  (let ((stream (pl-get-stream fh))
        (n (to-number len)))
    (when stream
      (let ((result (make-string n)))
        (read-sequence result stream)
        result))))

(defun pl-sysread (fh buf len)
  "Perl sysread - low-level read (same as read for now)"
  (pl-read fh buf len))

(defun pl-syswrite (fh data &optional len)
  "Perl syswrite - write data to filehandle"
  (let ((stream (pl-get-stream fh))
        (str (to-string data)))
    (when stream
      (if len
          (write-string (subseq str 0 (min (to-number len) (length str))) stream)
          (write-string str stream))
      (length str))))

(defun pl-truncate (fh-or-file size)
  "Perl truncate - truncate file (limited support)"
  (declare (ignore fh-or-file size))
  ;; Standard CL doesn't support truncate - would need SBCL extension
  (warn "truncate not implemented in standard CL")
  nil)

(defun pl-stat (file-or-fh)
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

(defun pl-lstat (file)
  "Perl lstat - stat without following symlinks (same as stat in CL)"
  (pl-stat file))

;;; ============================================================
;;; File Test Operators (-e, -d, -f, -r, -w, -x, -s, -z)
;;; ============================================================

(defun pl--e (file)
  "Perl -e: test if file exists"
  (let* ((path (to-string (unbox file)))
         (exists (or (probe-file path)
                     ;; probe-file may fail on directories in some implementations
                     (ignore-errors
                       (sb-posix:stat path)
                       t))))
    (if exists 1 nil)))

(defun pl--d (file)
  "Perl -d: test if file is a directory"
  (handler-case
      (let ((stat (sb-posix:stat (to-string (unbox file)))))
        (if (sb-posix:s-isdir (sb-posix:stat-mode stat))
            1
            nil))
    (error () nil)))

(defun pl--f (file)
  "Perl -f: test if file is a regular file"
  (handler-case
      (let ((stat (sb-posix:stat (to-string (unbox file)))))
        (if (sb-posix:s-isreg (sb-posix:stat-mode stat))
            1
            nil))
    (error () nil)))

(defun pl--r (file)
  "Perl -r: test if file is readable"
  (let ((path (to-string (unbox file))))
    (handler-case
        (progn
          (sb-posix:access path sb-posix:r-ok)
          1)
      (error () nil))))

(defun pl--w (file)
  "Perl -w: test if file is writable"
  (let ((path (to-string (unbox file))))
    (handler-case
        (progn
          (sb-posix:access path sb-posix:w-ok)
          1)
      (error () nil))))

(defun pl--x (file)
  "Perl -x: test if file is executable"
  (let ((path (to-string (unbox file))))
    (handler-case
        (progn
          (sb-posix:access path sb-posix:x-ok)
          1)
      (error () nil))))

(defun pl--s (file)
  "Perl -s: return file size if non-zero, nil otherwise"
  (handler-case
      (let* ((stat (sb-posix:stat (to-string (unbox file))))
             (size (sb-posix:stat-size stat)))
        (if (> size 0) size nil))
    (error () nil)))

(defun pl--z (file)
  "Perl -z: test if file has zero size"
  (handler-case
      (let* ((stat (sb-posix:stat (to-string (unbox file))))
             (size (sb-posix:stat-size stat)))
        (if (= size 0) 1 nil))
    (error () nil)))

(defun pl-unlink (&rest files)
  "Perl unlink - delete files. Returns count of files deleted."
  (let ((count 0))
    (dolist (f files)
      (let ((path (to-string (unbox f))))
        (when (and (probe-file path) (delete-file path))
          (incf count))))
    count))

(defun pl-fileno (fh)
  "Perl fileno - get file descriptor number"
  (let ((stream (pl-get-stream fh)))
    (cond
      ((eq stream *standard-input*) 0)
      ((eq stream *standard-output*) 1)
      ((eq stream *error-output*) 2)
      (t -1))))  ; CL doesn't expose fd numbers portably

(defun pl-getc (&optional fh)
  "Perl getc - read single character"
  (let ((stream (if fh (pl-get-stream fh) *standard-input*)))
    (when stream
      (let ((ch (read-char stream nil nil)))
        (if ch (string ch) nil)))))

(defun pl-readline (&optional fh)
  "Perl readline / diamond operator <FH> - read a record from filehandle.
   Respects $/ (input record separator):
     default newline = line mode, undef = slurp, \"\" = paragraph, other = custom separator.
   Returns nil at EOF. If no filehandle given, reads from *standard-input*.
   Note: Unlike CL's read-line, this keeps the trailing separator (like Perl)."
  (let ((stream (if fh (pl-get-stream fh) *standard-input*))
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
                                       :adjustable t :fill-pointer 0))
               (found nil))
           (loop for char = (read-char stream nil nil)
                 while char
                 do (vector-push-extend char result)
                 when (char= char sep-char)
                   do (setf found t) and do (loop-finish))
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

;;; ============================================================
;;; Directory I/O Functions
;;; ============================================================

;; Directory handle storage
(defvar *pl-dirhandles* (make-hash-table :test 'eq))

(defun pl-opendir (dh dir)
  "Perl opendir - open directory for reading"
  (let ((dir-str (to-string dir)))
    (when (probe-file dir-str)
      (let ((entries (directory (merge-pathnames "*.*" dir-str))))
        (if (symbolp dh)
            (setf (gethash dh *pl-dirhandles*)
                  (cons 0 (mapcar #'file-namestring entries)))
            (when (pl-box-p dh)
              (setf (pl-box-value dh)
                    (cons 0 (mapcar #'file-namestring entries)))))
        t))))

(defun pl-readdir (dh)
  "Perl readdir - read next directory entry"
  (let ((handle (if (symbolp dh)
                    (gethash dh *pl-dirhandles*)
                    (when (pl-box-p dh) (pl-box-value dh)))))
    (when handle
      (let ((idx (car handle))
            (entries (cdr handle)))
        (if (< idx (length entries))
            (progn
              (setf (car handle) (1+ idx))
              (nth idx entries))
            nil)))))

(defun pl-closedir (dh)
  "Perl closedir - close directory handle"
  (when (symbolp dh)
    (remhash dh *pl-dirhandles*))
  t)

(defun pl-rewinddir (dh)
  "Perl rewinddir - reset directory to beginning"
  (let ((handle (if (symbolp dh)
                    (gethash dh *pl-dirhandles*)
                    (when (pl-box-p dh) (pl-box-value dh)))))
    (when handle
      (setf (car handle) 0))
    t))

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

(defun pl-glob (&optional pattern)
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

(defun pl-chdir (&optional dir)
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

(defun pl-set_up_inc (&rest dirs)
  "Perl test.pl set_up_inc - modifies @INC for tests. No-op in PCL since
   @INC is set up during transpilation."
  (declare (ignore dirs))
  t)

(defun pl-mkdir (dir &optional mode)
  "Perl mkdir - create directory. Returns true on success."
  (let ((path (to-string dir))
        (m (if mode (truncate (to-number mode)) #o755)))
    (handler-case
        (progn (sb-posix:mkdir path m) t)
      (error () nil))))

(defun pl-rmdir (dir)
  "Perl rmdir - remove empty directory. Returns true on success."
  (handler-case
      (progn (sb-posix:rmdir (to-string dir)) t)
    (error () nil)))

(defun pl-getcwd ()
  "Perl getcwd/cwd - get current working directory."
  (sb-posix:getcwd))

(defun pl-cwd ()
  "Perl cwd - alias for getcwd."
  (sb-posix:getcwd))

(defun pl-rename (old new)
  "Perl rename - rename file. Returns true on success."
  (handler-case
      (progn (rename-file (to-string old) (to-string new)) t)
    (error () nil)))

(defun pl-chmod (mode &rest files)
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

(defun pl-time ()
  "Perl time - return seconds since Unix epoch."
  (- (get-universal-time) +unix-epoch-offset+))

(defun pl-times (&key wantarray)
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
    (vector (make-pl-box user) (make-pl-box sys) (make-pl-box cuser) (make-pl-box csys))))

(defun pl-sleep (secs)
  "Perl sleep - pause execution for specified seconds. Returns seconds slept."
  (let ((n (truncate (to-number secs))))
    (sleep n)
    n))

(defun pl-study (&optional str)
  "Perl study - deprecated no-op in modern Perl. Returns 1."
  (declare (ignore str))
  1)

(defun pl-reset (&optional pattern)
  "Perl reset - reset ?? searches. No-op in PCL, returns 1."
  (declare (ignore pattern))
  1)

(defun pl-vec (str offset bits)
  "Perl vec - treat string as bit vector and extract element.
   OFFSET is the element index, BITS is element size (1, 2, 4, 8, 16, 32).
   Returns the numeric value at that position."
  (let* ((s (to-string str))
         (byte-offset (floor (* offset bits) 8))
         (bit-offset (mod (* offset bits) 8)))
    (cond
      ;; Beyond string length - return 0
      ((>= byte-offset (length s)) 0)
      ;; 8-bit aligned access (common case)
      ((and (= bits 8) (= bit-offset 0))
       (char-code (char s byte-offset)))
      ;; 16-bit access (little-endian)
      ((and (= bits 16) (= bit-offset 0))
       (let ((lo (if (< byte-offset (length s)) (char-code (char s byte-offset)) 0))
             (hi (if (< (1+ byte-offset) (length s)) (char-code (char s (1+ byte-offset))) 0)))
         (+ lo (ash hi 8))))
      ;; 32-bit access (little-endian)
      ((and (= bits 32) (= bit-offset 0))
       (let ((b0 (if (< byte-offset (length s)) (char-code (char s byte-offset)) 0))
             (b1 (if (< (+ 1 byte-offset) (length s)) (char-code (char s (+ 1 byte-offset))) 0))
             (b2 (if (< (+ 2 byte-offset) (length s)) (char-code (char s (+ 2 byte-offset))) 0))
             (b3 (if (< (+ 3 byte-offset) (length s)) (char-code (char s (+ 3 byte-offset))) 0)))
         (+ b0 (ash b1 8) (ash b2 16) (ash b3 24))))
      ;; Sub-byte access (1, 2, 4 bits)
      ((and (<= bits 8) (< byte-offset (length s)))
       (let* ((byte-val (char-code (char s byte-offset)))
              (mask (1- (ash 1 bits))))
         (logand (ash byte-val (- bit-offset)) mask)))
      ;; Default
      (t 0))))

(defun pl-localtime (&optional time)
  "Perl localtime - convert time to local time components.
   In list context returns (sec min hour mday mon year wday yday isdst).
   Note: mon is 0-11, year is years since 1900, wday is 0=Sunday."
  (let* ((unix-time (if time (truncate (to-number time)) (pl-time)))
         (universal (+ unix-time +unix-epoch-offset+)))
    (multiple-value-bind (sec min hour day month year wday dst-p tz)
        (decode-universal-time universal)
      (declare (ignore tz))
      ;; Perl: mon is 0-11, year is since 1900, wday 0=Sunday
      ;; CL: month is 1-12, year is full, wday 0=Monday
      (let ((perl-wday (mod (1+ wday) 7))  ; Convert Mon=0 to Sun=0
            (perl-year (- year 1900))
            (perl-mon (1- month))
            ;; Calculate day of year (yday)
            (yday (- (floor (encode-universal-time 0 0 0 day month year) 86400)
                     (floor (encode-universal-time 0 0 0 1 1 year) 86400))))
        (if *wantarray*
            (make-array 9 :initial-contents
                        (list sec min hour day perl-mon perl-year perl-wday yday (if dst-p 1 0))
                        :adjustable t :fill-pointer t)
            ;; Scalar context: return formatted string (like ctime)
            (format nil "~A ~A ~2D ~2,'0D:~2,'0D:~2,'0D ~D"
                    (nth wday '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
                    (nth (1- month) '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                                       "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
                    day hour min sec year))))))

(defun pl-gmtime (&optional time)
  "Perl gmtime - convert time to UTC components.
   Same return format as localtime but in UTC."
  (let* ((unix-time (if time (truncate (to-number time)) (pl-time)))
         (universal (+ unix-time +unix-epoch-offset+)))
    (multiple-value-bind (sec min hour day month year wday)
        (decode-universal-time universal 0)  ; 0 = UTC
      (let ((perl-wday (mod (1+ wday) 7))
            (perl-year (- year 1900))
            (perl-mon (1- month))
            (yday (- (floor (encode-universal-time 0 0 0 day month year 0) 86400)
                     (floor (encode-universal-time 0 0 0 1 1 year 0) 86400))))
        (if *wantarray*
            (make-array 9 :initial-contents
                        (list sec min hour day perl-mon perl-year perl-wday yday 0)
                        :adjustable t :fill-pointer t)
            (format nil "~A ~A ~2D ~2,'0D:~2,'0D:~2,'0D ~D"
                    (nth wday '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
                    (nth (1- month) '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                                       "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
                    day hour min sec year))))))

;;; ============================================================
;;; Process Control
;;; ============================================================

(defun pl-exit (&optional code)
  "Perl exit - terminate program with exit code."
  (sb-ext:exit :code (if code (truncate (to-number code)) 0)))

(defun pl-system (&rest args)
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

(defun pl-backtick (cmd)
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

(defun pl-env-get (key)
  "Get environment variable value."
  (sb-posix:getenv (to-string key)))

(defun pl-env-set (key value)
  "Set environment variable value."
  (sb-posix:setenv (to-string key) (to-string value) 1)
  value)

;; %ENV is a special hash backed by the actual environment
;; We use a proxy approach: %ENV is a symbol that pl-gethash recognizes
(defvar %ENV '%ENV-MARKER% "Marker for environment hash access")

;;; ============================================================
;;; Module System (%INC, @INC, use/require)
;;; ============================================================

;; %INC: hash of loaded modules (key: relative path, value: absolute path)
;; Note: *pl-inc-table* is forward-declared near top of file
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
(defvar *pl-loading-modules* nil
  "Stack of modules currently being loaded")

;;; --- Module Path Utilities ---

(defun pl-module-to-path (module-name)
  "Convert Perl module name to relative path.
   Foo::Bar => Foo/Bar.pm
   Foo/Bar.pm => Foo/Bar.pm (unchanged)"
  (let ((name (to-string module-name)))
    (if (search ".pm" name)
        name
        (concatenate 'string
                     (substitute #\/ #\: name)
                     ".pm"))))

(defun pl-find-module-in-inc (rel-path)
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

(defun pl-ensure-cache-dir ()
  "Create cache directory if it doesn't exist."
  (ensure-directories-exist *pcl-cache-dir*))

(defun pl-compute-cache-path (source-path &optional lisp-p)
  "Compute cache path for a source file using hash of absolute path.
   LISP-P: if true, return .lisp path; else .fasl"
  (let* ((abs-path (namestring (truename source-path)))
         (hash (sxhash abs-path))
         (ext (if lisp-p ".lisp" ".fasl")))
    (pl-ensure-cache-dir)
    (merge-pathnames (format nil "~16,'0X~A" (logand hash #xFFFFFFFFFFFFFFFF) ext)
                     *pcl-cache-dir*)))

(defun pl-cache-valid-p (source-path cache-path)
  "Check if cached file is valid: exists, newer than source, not expired."
  (when *pcl-skip-cache*
    (return-from pl-cache-valid-p nil))
  (when (not (probe-file cache-path))
    (return-from pl-cache-valid-p nil))
  (let* ((source-mtime (file-write-date source-path))
         (cache-mtime (file-write-date cache-path))
         (cache-age (- (get-universal-time) cache-mtime)))
    (and (> cache-mtime source-mtime)
         (< cache-age *pcl-cache-max-age*))))

(defun pl-cleanup-old-cache ()
  "Remove cache files older than max age."
  (let ((cutoff (- (get-universal-time) *pcl-cache-max-age*)))
    (dolist (file (directory (merge-pathnames "*.*" *pcl-cache-dir*)))
      (when (< (file-write-date file) cutoff)
        (ignore-errors (delete-file file))))))

;;; --- Module Transpilation ---

(defun pl-transpile-file (source-path)
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
          (return-from pl-transpile-file nil))))
    (when (> (length output) 0)
      output)))

;;; --- Module Loading ---

(defun pl-load-module-cached (source-path)
  "Load a Perl module with caching. Returns t on success."
  (pl-ensure-cache-dir)
  (let ((cache-path (pl-compute-cache-path source-path (not *pcl-cache-fasl*))))
    (cond
      ;; Cache hit
      ((pl-cache-valid-p source-path cache-path)
       (load cache-path)
       t)
      ;; Cache miss - transpile and cache
      (t
       (let ((lisp-code (pl-transpile-file source-path)))
         (unless lisp-code
           (error "Failed to transpile ~A" source-path))
         (if *pcl-cache-fasl*
             ;; FASL mode: write temp .lisp, compile to .fasl
             (let ((temp-lisp (pl-compute-cache-path source-path t)))
               (with-open-file (out temp-lisp
                                    :direction :output
                                    :if-exists :supersede)
                 (write-string lisp-code out))
               (let ((fasl-path (compile-file temp-lisp :output-file cache-path
                                              :print nil :verbose nil)))
                 (ignore-errors (delete-file temp-lisp))
                 (unless fasl-path
                   (error "compile-file failed for ~A" temp-lisp))
                 (pl-cleanup-old-cache)
                 (load fasl-path)
                 t))
             ;; Lisp mode: just cache .lisp
             (progn
               (with-open-file (out cache-path
                                    :direction :output
                                    :if-exists :supersede)
                 (write-string lisp-code out))
               (pl-cleanup-old-cache)
               (load cache-path)
               t)))))))

(defun pl-find-module-package (module-name)
  "Find CL package for a Perl module.
   Tries: uppercase name, pipe-quoted name (for Foo::Bar)."
  (or (find-package (string-upcase module-name))
      (find-package (format nil "|~A|" module-name))))

(defun pl-perl-symbol-to-cl-name (sym-name)
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

(defun pl-import-perl-symbol (sym-name from-pkg to-pkg)
  "Import a Perl symbol from FROM-PKG to TO-PKG.
   Handles sigils appropriately."
  (let* ((cl-name (pl-perl-symbol-to-cl-name sym-name))
         (sym (find-symbol cl-name from-pkg)))
    (when sym
      (shadowing-import sym to-pkg))))

(defun pl-import-exports (module-name to-pkg &optional specific-imports)
  "Import symbols from module's @EXPORT (or specific list) into TO-PKG."
  (let ((pkg (pl-find-module-package module-name)))
    (when pkg
      (let ((imports (or specific-imports
                         ;; Get @EXPORT from module's package
                         (let ((export-sym (find-symbol "@EXPORT" pkg)))
                           (when (and export-sym (boundp export-sym))
                             (let ((val (symbol-value export-sym)))
                               (when (and val (vectorp val))
                                 (coerce val 'list))))))))
        (dolist (sym-name imports)
          (pl-import-perl-symbol sym-name pkg to-pkg))))))

(defun pl-use (module-name &key imports)
  "Perl use - load module at compile time and import symbols.
   MODULE-NAME: 'Foo::Bar' or 'Foo/Bar.pm'
   IMPORTS: list of symbols to import (nil = use @EXPORT, empty list = no imports)"
  (let ((rel-path (pl-module-to-path module-name))
        (caller-pkg *package*))
    ;; Already loaded?
    (when (gethash rel-path *pl-inc-table*)
      ;; Still import symbols for repeated use statements
      (unless (and imports (null imports))
        (pl-import-exports module-name caller-pkg imports))
      (return-from pl-use t))
    ;; Circular dependency?
    (when (member rel-path *pl-loading-modules* :test #'string=)
      (warn "Circular dependency detected: ~A" rel-path)
      (return-from pl-use t))
    ;; Find module in @INC
    (let ((abs-path (pl-find-module-in-inc rel-path)))
      (unless abs-path
        (error "Can't locate ~A in @INC (@INC contains: ~{~A~^ ~})"
               rel-path (coerce @INC 'list)))
      ;; Load with circular detection
      (let ((*pl-loading-modules* (cons rel-path *pl-loading-modules*)))
        (pl-load-module-cached abs-path))
      ;; Update %INC
      (setf (gethash rel-path *pl-inc-table*) abs-path)
      ;; Import symbols from module
      (unless (and imports (null imports))
        (pl-import-exports module-name caller-pkg imports))
      t)))

(defun pl-require (module-name)
  "Perl require - load module at runtime (no imports)."
  (pl-use module-name))

(defun pl-require-file (path)
  "Perl require with file path - load a .pl file by path.
   Resolves relative paths against current directory."
  (let* ((path-str (unbox path))
         ;; Check if already loaded (Perl tracks this in %INC by path)
         (abs-path (if (char= (char path-str 0) #\/)
                       path-str
                       ;; Relative path - resolve against current dir
                       (merge-pathnames path-str (truename *default-pathname-defaults*)))))
    ;; Check %INC to avoid reloading
    (when (gethash path-str *pl-inc-table*)
      (return-from pl-require-file t))
    ;; Load the file using pl2cl
    (unless (probe-file abs-path)
      (error "Can't locate ~A" path-str))
    ;; Transpile and load
    (pl-load-module-cached abs-path)
    ;; Update %INC
    (setf (gethash path-str *pl-inc-table*) (namestring abs-path))
    t))

;;; ============================================================
;;; List Functions
;;; ============================================================

(defun pl-grep (fn list)
  "Perl grep - fn receives item as $_ parameter"
  (let* ((arr (unbox list))
         (result (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for item across arr
          when (pl-true-p (funcall fn item))
          do (vector-push-extend item result))
    result))

(defun pl-map (fn list)
  "Perl map - fn receives item as $_ parameter"
  (let* ((arr (unbox list))
         (result (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for item across arr
          do (vector-push-extend (funcall fn item) result))
    result))

(defun pl-sort (list-or-fn &optional list)
  "Perl sort - without comparator, sorts lexically (as strings).
   With comparator fn, fn receives $a and $b and returns negative if a < b."
  (if list
      ;; Called with comparator: (pl-sort fn list)
      (let ((fn list-or-fn)
            (result (copy-seq (unbox list))))
        (sort result (lambda (a b)
                       (< (funcall fn a b) 0))))
      ;; Called without comparator: (pl-sort list) - default string sort
      (let ((result (copy-seq (unbox list-or-fn))))
        (sort result (lambda (a b)
                       (string< (to-string a) (to-string b)))))))

(defun pl-reverse (seq)
  "Perl reverse"
  (reverse (unbox seq)))

(defun pl-join (sep &rest items)
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

(defun pl-split (pattern str &optional limit)
  "Perl split - split string by pattern.
   Note: pattern and str are NOT optional here - PExpr.pm adds defaults
   (pattern=' ', str=$_) at parse time so codegen always provides both."
  (let* ((s (to-string str))
         (limit-num (if limit (truncate (to-number limit)) nil))
         (keep-trailing (and limit-num (/= limit-num 0)))
         (max-fields (if (and limit-num (> limit-num 0)) limit-num nil))
         (result (make-array 0 :adjustable t :fill-pointer 0)))
    (cond
      ;; Regex pattern from pl-regex
      ((pl-regex-match-p pattern)
       (let* ((pat (pl-regex-match-pattern pattern))
              ;; CL-PPCRE: 0 removes trailing empty, large number keeps them
              ;; Perl: limit=0/nil removes, limit<0 keeps, limit>0 is max fields
              (ppcre-limit (cond (max-fields max-fields)    ; limit > 0
                                 (keep-trailing 1000000)     ; limit < 0, keep trailing
                                 (t 0)))                     ; no limit, remove trailing
              (parts (if (zerop (length pat))
                         ;; Empty regex: split into characters
                         (map 'list #'string s)
                         ;; Use CL-PPCRE split
                         (cl-ppcre:split pat s :limit ppcre-limit :with-registers-p nil))))
         (dolist (p parts)
           (vector-push-extend p result))))
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
                       (return)))))))))
    ;; Remove trailing empty fields unless limit specified
    (unless keep-trailing
      (loop while (and (> (length result) 0)
                       (zerop (length (aref result (1- (length result))))))
            do (vector-pop result)))
    result))

(defun pl-funcall-ref (ref &rest args)
  "Call a code reference"
  (apply (unbox ref) args))

;;; ============================================================
;;; Type Functions
;;; ============================================================

(defun pl-backslash (val)
  "Perl reference operator \\$x - returns a box containing the referenced value.
   For scalars (boxes), returns a box containing the box (creating a reference).
   For arrays/hashes, returns them directly as they're already references."
  (if (pl-box-p val)
      ;; Scalar: create a reference by wrapping in another box
      (make-pl-box val)
      ;; Array/hash: already a reference type
      val))

(defun pl-cast-@ (val)
  "Perl array dereference @{$ref} - unbox to get the array"
  (unbox val))

(defun pl-cast-% (val)
  "Perl hash dereference %{$ref} - unbox to get the hash"
  (unbox val))

(defun pl-cast-$ (val)
  "Perl scalar dereference ${$ref} - get value from reference.
   $ref contains a reference (box), $$ref gets the referenced value."
  (let ((inner (unbox val)))
    ;; inner is the reference (a box), get its value
    (if (pl-box-p inner)
        (pl-box-value inner)
        inner)))

(defun pl-ref (val)
  "Perl ref() function - get reference type or class name if blessed.
   Returns empty string for non-references."
  ;; Unbox the variable to get what it contains
  (let ((inner (unbox val)))
    (cond
      ;; Blessed value - return class name
      ((and (pl-box-p val) (pl-box-class val))
       (pl-box-class val))
      ((and (hash-table-p inner) (gethash :__class__ inner))
       (gethash :__class__ inner))
      ;; Scalar reference (box containing box)
      ((pl-box-p inner) "SCALAR")
      ;; Hash reference
      ((hash-table-p inner) "HASH")
      ;; Array reference (list or vector, but NOT strings)
      ((or (listp inner) (and (vectorp inner) (not (stringp inner)))) "ARRAY")
      ;; Code reference
      ((functionp inner) "CODE")
      ;; Not a reference
      (t ""))))

;; Keep reftype as an alias for compatibility
(defun pl-reftype (val)
  "Alias for pl-ref"
  (pl-ref val))

(defun pl-scalar (&rest args)
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

(defun pl-wantarray ()
  "Perl wantarray"
  *wantarray*)

(defun pl-caller (&optional (level 0))
  "Perl caller - return information about the calling subroutine.
   In scalar context, returns package name.
   In list context, returns (package filename line subroutine).
   Uses SBCL's backtrace facilities for stack introspection."
  (let ((frame-info nil)
        (current-level 0)
        (target-level (+ level 2)))  ; Skip pl-caller itself and its caller
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

;;; ============================================================
;;; OO Support
;;; ============================================================

;; Simple blessing - store class name in hash
(defun pl-bless (ref class)
  "Perl bless - attach class to a reference (hash, array, or scalar ref).
   For hashes: stores class in :__class__ key (survives unboxing).
   For arrays/code/other: stores class on the box's class slot."
  (let ((class-name (to-string class))
        (inner (unbox ref)))
    (cond
      ((hash-table-p inner)
       (setf (gethash :__class__ inner) class-name)
       ;; Also set on box if ref is a box (so box-set can copy it)
       (when (pl-box-p ref) (setf (pl-box-class ref) class-name)))
      ((pl-box-p inner)
       ;; Scalar reference - store on the inner box
       (setf (pl-box-class inner) class-name))
      (t
       ;; Array, code, or other ref type - store class on the box
       (when (pl-box-p ref) (setf (pl-box-class ref) class-name)))))
  ref)

(defun pl-get-class (obj)
  "Get the class name of a blessed object or class string"
  (cond
    ((stringp obj) obj)  ;; Class name string (for Counter->new())
    ((hash-table-p obj) (gethash :__class__ obj))
    ((pl-box-p obj)
     ;; Check box's class slot first, then check the value inside
     (or (pl-box-class obj)
         (let ((val (pl-box-value obj)))
           (cond
             ((hash-table-p val) (gethash :__class__ val))
             ((pl-box-p val) (pl-box-class val))
             (t nil)))))
    (t nil)))

(defun pl-resolve-invocant (name)
  "Resolve a bareword invocant for method calls.
   In Perl, Foo->bar() checks if sub Foo exists first:
   - If pl-Foo is a function → call it, return the result (object)
   - Otherwise → return the string as a class name"
  (let* ((func-name (format nil "PL-~A" name))
         (func-sym (find-symbol (string-upcase func-name) :pcl)))
    (if (and func-sym (fboundp func-sym))
        ;; Sub exists - call it to get the object
        (funcall func-sym)
        ;; No sub - return string as class name
        name)))

(defun pl-method-call (obj method &rest args)
  "Perl method call - looks up pl-METHOD function in object's package and walks MRO for inheritance"
  (let* ((method-name (to-string method))
         (class-name (pl-get-class obj)))
    (unless class-name
      (error "Can't call method ~A on non-blessed reference" method-name))

    ;; Try to find CLOS class for MRO-based lookup
    (let* ((clos-class-name (perl-pkg-to-clos-class class-name))
           (clos-class (find-class (intern (string-upcase clos-class-name) :pcl) nil)))

      (if clos-class
          ;; Walk MRO (Method Resolution Order) using CLOS class-precedence-list
          (let ((mro (sb-mop:class-precedence-list clos-class)))
            (dolist (cls mro)
              (let* ((cls-sym-name (symbol-name (class-name cls)))
                     ;; Convert CLOS class name back to CL package name
                     (pkg-name (clos-class-to-pkg cls-sym-name))
                     (pkg (find-package pkg-name)))
                (when pkg
                  (let ((fn (find-symbol (format nil "PL-~A" (string-upcase method-name)) pkg)))
                    (when (and fn (fboundp fn))
                      (return-from pl-method-call (apply fn obj args)))))))
            ;; Not found in any class in MRO
            (error "Can't locate method ~A via package ~A" method-name class-name))

          ;; No CLOS class - fall back to single-class lookup (legacy behavior)
          (let ((pkg (find-package (string-upcase class-name))))
            (unless pkg
              (error "Package ~A not found for method call" class-name))
            (let ((fn (find-symbol (string-upcase (format nil "PL-~A" method-name)) pkg)))
              (if (and fn (fboundp fn))
                  (apply fn obj args)
                  (error "Can't locate method ~A in package ~A" method-name class-name))))))))

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
(defun pl-super-call (obj method current-class &rest args)
  "Call method starting from parent of current-class in MRO (for SUPER:: calls)"
  (let* ((method-name (to-string method))
         (clos-class-name (perl-pkg-to-clos-class current-class))
         (clos-class (find-class (intern (string-upcase clos-class-name) :pcl) nil)))

    (unless clos-class
      (error "Can't find class ~A for SUPER:: call" current-class))

    ;; Get MRO and skip current class
    (let* ((mro (sb-mop:class-precedence-list clos-class))
           (parent-mro (cdr mro)))  ;; Skip current class

      (dolist (cls parent-mro)
        (let* ((cls-sym-name (symbol-name (class-name cls)))
               (pkg-name (clos-class-to-pkg cls-sym-name))
               (pkg (find-package pkg-name)))
          (when pkg
            (let ((fn (find-symbol (format nil "PL-~A" (string-upcase method-name)) pkg)))
              (when (and fn (fboundp fn))
                (return-from pl-super-call (apply fn obj args)))))))

      (error "No SUPER::~A found from ~A" method-name current-class))))

;;; can() and isa() methods - available on all objects (UNIVERSAL package)
(defun pl-can (invocant method-name)
  "Perl can() - check if object/class can perform a method.
   Returns the code reference if method exists, nil otherwise.
   Uses C3 MRO to check inheritance chain."
  (let* ((method-str (to-string method-name))
         (class-name (cond
                       ((stringp invocant) invocant)
                       ((pl-box-p invocant) (pl-get-class invocant))
                       ((hash-table-p invocant) (gethash :__class__ invocant))
                       (t nil))))
    (unless class-name
      (return-from pl-can nil))

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
                      (return-from pl-can (symbol-function fn)))))))
            nil)  ; Not found in any class in MRO

          ;; No CLOS class - fall back to single-class lookup
          (let ((pkg (find-package (string-upcase class-name))))
            (when pkg
              (let ((fn (find-symbol (format nil "PL-~A" (string-upcase method-str)) pkg)))
                (if (and fn (fboundp fn))
                    (symbol-function fn)
                    nil))))))))

(defun pl-isa (invocant class-name)
  "Perl isa() - check if object is-a class.
   Uses C3 MRO to check inheritance chain.
   Returns t if invocant is-a class-name, nil otherwise."
  (let* ((check-class (to-string class-name))
         (obj-class (cond
                      ((stringp invocant) invocant)
                      ((pl-box-p invocant) (pl-get-class invocant))
                      ((hash-table-p invocant) (gethash :__class__ invocant))
                      (t nil))))
    (unless obj-class
      (return-from pl-isa nil))

    ;; Exact match
    (when (string-equal obj-class check-class)
      (return-from pl-isa t))

    ;; Try to find CLOS class for MRO-based lookup
    ;; Classes are defined in packages named after the Perl package (e.g., Dog::dog)
    (let* ((clos-class-name (perl-pkg-to-clos-class obj-class))
           (pkg (find-package (string-upcase obj-class)))
           (clos-class (when pkg
                         (find-class (intern (string-upcase clos-class-name) pkg) nil))))

      (when clos-class
        ;; Walk MRO (Method Resolution Order) using CLOS class-precedence-list
        (let ((mro (sb-mop:class-precedence-list clos-class)))
          (dolist (cls mro)
            (let* ((cls-sym-name (symbol-name (class-name cls)))
                   (pkg-name (clos-class-to-pkg cls-sym-name)))
              (when (string-equal pkg-name check-class)
                (return-from pl-isa t))))))

      nil)))

;;; ============================================================
;;; Regex Support (using CL-PPCRE)
;;; ============================================================

;; Regex operation types
(defstruct pl-regex-match
  "Regex match operation m//"
  pattern
  modifiers)

(defstruct pl-subst-op
  "Substitution operation s///"
  pattern
  replacement
  modifiers)

(defstruct pl-tr-op
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

(defun pl-regex (pattern-string)
  "Parse /pattern/modifiers and return a regex-match struct.
   Pattern-string is like '/foo/i' or 'm/bar/g' or 'm{pattern}s'"
  (let* ((str (to-string pattern-string))
         (first-char (char str 0))
         (start-delim (if (char= first-char #\m) 1 0))
         (open-delim (char str start-delim))
         (close-delim (get-closing-delim open-delim))
         (end-delim (position close-delim str :start (1+ start-delim) :from-end t))
         (pattern (subseq str (1+ start-delim) end-delim))
         (modifiers (if (< end-delim (1- (length str)))
                        (subseq str (1+ end-delim))
                        "")))
    (make-pl-regex-match :pattern pattern
                         :modifiers (parse-regex-modifiers modifiers))))

(defun pl-qr (pattern-string)
  "Parse qr/pattern/modifiers and return a compiled regex (regex-match struct).
   Pattern-string is like 'qr/foo/i' or 'qr{pattern}i'"
  (let* ((str (to-string pattern-string))
         ;; Skip past 'qr' prefix
         (start-delim 2)
         (open-delim (char str start-delim))
         (close-delim (get-closing-delim open-delim))
         (end-delim (position close-delim str :start (1+ start-delim) :from-end t))
         (pattern (subseq str (1+ start-delim) end-delim))
         (modifiers (if (< end-delim (1- (length str)))
                        (subseq str (1+ end-delim))
                        "")))
    (make-pl-regex-match :pattern pattern
                         :modifiers (parse-regex-modifiers modifiers))))

(defun pl-subst (pattern replacement &rest modifiers)
  "Create a substitution operation s///
   Modifiers are keywords like :g :i :s :m :x :e"
  (make-pl-subst-op :pattern (to-string pattern)
                    :replacement (to-string replacement)
                    :modifiers modifiers))

(defun pl-tr (from to &rest modifiers)
  "Create a transliteration operation tr///
   Modifiers are keywords like :c :d :s :r"
  (make-pl-tr-op :from (to-string from)
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
        $6 nil $7 nil $8 nil $9 nil))

(defun set-capture-groups (str reg-starts reg-ends)
  "Set capture group variables from regex match results"
  (when (and reg-starts reg-ends)
    (let ((num-groups (length reg-starts)))
      (when (> num-groups 0)
        (setf $1 (subseq str (aref reg-starts 0) (aref reg-ends 0))))
      (when (> num-groups 1)
        (setf $2 (subseq str (aref reg-starts 1) (aref reg-ends 1))))
      (when (> num-groups 2)
        (setf $3 (subseq str (aref reg-starts 2) (aref reg-ends 2))))
      (when (> num-groups 3)
        (setf $4 (subseq str (aref reg-starts 3) (aref reg-ends 3))))
      (when (> num-groups 4)
        (setf $5 (subseq str (aref reg-starts 4) (aref reg-ends 4))))
      (when (> num-groups 5)
        (setf $6 (subseq str (aref reg-starts 5) (aref reg-ends 5))))
      (when (> num-groups 6)
        (setf $7 (subseq str (aref reg-starts 6) (aref reg-ends 6))))
      (when (> num-groups 7)
        (setf $8 (subseq str (aref reg-starts 7) (aref reg-ends 7))))
      (when (> num-groups 8)
        (setf $9 (subseq str (aref reg-starts 8) (aref reg-ends 8)))))))

(defun do-regex-match (string op)
  "Perform regex match.
   In scalar context: return t if matched, nil otherwise.
   In list context (*wantarray* t): return vector of captures, or nil if no match.
   Also sets capture group variables $1, $2, ... $9.
   Note: In Perl, captures are only updated on successful match."
  (let* ((str (to-string (unbox string)))
         (pattern (pl-regex-match-pattern op))
         (modifiers (pl-regex-match-modifiers op))
         (options (build-ppcre-options modifiers)))
    (handler-case
        (let ((scanner (apply #'cl-ppcre:create-scanner pattern options)))
          (multiple-value-bind (match-start match-end reg-starts reg-ends)
              (cl-ppcre:scan scanner str)
            (declare (ignore match-end))
            (when match-start
              ;; Clear and set capture groups only on success
              (clear-capture-groups)
              (set-capture-groups str reg-starts reg-ends)
              ;; In list context, return captures as vector
              (if *wantarray*
                  (let* ((num-groups (length reg-starts))
                         (captures (make-array num-groups :adjustable t :fill-pointer t)))
                    (dotimes (i num-groups)
                      (setf (aref captures i)
                            (if (and (aref reg-starts i) (aref reg-ends i))
                                (subseq str (aref reg-starts i) (aref reg-ends i))
                                nil)))
                    captures)
                  t))))
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
         (pattern (pl-subst-op-pattern op))
         (replacement (perl-to-ppcre-replacement (pl-subst-op-replacement op)))
         (modifiers (pl-subst-op-modifiers op))
         (global-p (member :g modifiers))
         (case-insensitive (member :i modifiers))
         (single-line (member :s modifiers))
         (multi-line (member :m modifiers)))
    (handler-case
        (let* ((options (append (when case-insensitive '(:case-insensitive-mode t))
                                (when single-line '(:single-line-mode t))
                                (when multi-line '(:multi-line-mode t))))
               (scanner (apply #'cl-ppcre:create-scanner pattern options))
               (count 0)
               (result nil))
          ;; First, set capture groups from the match
          (multiple-value-bind (match-start match-end reg-starts reg-ends)
              (cl-ppcre:scan scanner str)
            (declare (ignore match-end))
            (when match-start
              (clear-capture-groups)
              (set-capture-groups str reg-starts reg-ends)))
          ;; Perform the substitution
          (setf result (if global-p
                           (cl-ppcre:regex-replace-all scanner str replacement)
                           (cl-ppcre:regex-replace scanner str replacement)))
          ;; Count replacements
          (when (stringp result)
            (if global-p
                (setf count (length (cl-ppcre:all-matches-as-strings scanner str)))
                (when (cl-ppcre:scan scanner str)
                  (setf count 1))))
          ;; Update the boxed string (and invalidate caches)
          (when (stringp result)
            (if (pl-box-p string-box)
                (setf (pl-box-value string-box) result
                      (pl-box-sv-ok string-box) nil
                      (pl-box-nv-ok string-box) nil)
                (warn "Cannot modify non-boxed value in s///")))
          count)
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
         (from-raw (pl-tr-op-from op))
         (to-raw (pl-tr-op-to op))
         (modifiers (pl-tr-op-modifiers op))
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
      (if (pl-box-p string-box)
          (setf (pl-box-value string-box) result
                (pl-box-sv-ok string-box) nil
                (pl-box-nv-ok string-box) nil)
          (warn "Cannot modify non-boxed value in tr///"))
      count)))

(defun pl-=~ (string operation)
  "Perl =~ binding operator.
   Dispatches based on operation type:
   - pl-regex-match: perform match, return t/nil
   - pl-subst-op: perform substitution, modify string, return count
   - pl-tr-op: perform transliteration, modify string, return count"
  (cond
    ((pl-regex-match-p operation)
     (do-regex-match string operation))
    ((pl-subst-op-p operation)
     (do-regex-subst string operation))
    ((pl-tr-op-p operation)
     (do-tr string operation))
    (t
     (warn "Unknown regex operation type: ~A" (type-of operation))
     nil)))

(defun pl-!~ (string operation)
  "Perl !~ negative binding operator"
  (not (pl-=~ string operation)))

;;; ============================================================
;;; Helper to create Perl-style arrays
;;; ============================================================

(defun make-pl-array (&rest items)
  "Create a Perl-style adjustable array"
  (let ((arr (make-array (length items) :adjustable t :fill-pointer t
                         :initial-contents items)))
    arr))

;;; ============================================================
;;; pack / unpack (basic implementation)
;;; ============================================================

(defun pl-pack (template &rest args)
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

(defun pl-unpack (template str)
  "Perl unpack - basic stub, returns empty list for now."
  (declare (ignore template str))
  (make-array 0 :adjustable t :fill-pointer 0))

;;; ============================================================
;;; Package initialization
;;; ============================================================

;; Export all pl- symbols so they're accessible from other packages
;; This includes all functions, macros, and variables with pl- prefix
(do-symbols (sym (find-package :pcl))
  (when (and (>= (length (symbol-name sym)) 3)
             (string= "PL-" (subseq (symbol-name sym) 0 3)))
    (export sym :pcl)))

;; Perl uses double-precision floats everywhere.
;; Make CL read all float literals as double-float (e.g., 1.5 → 1.5d0, not 1.5f0)
(setf *read-default-float-format* 'double-float)

;;; ============================================================
;;; Stub packages for common Perl modules
;;; ============================================================

;; utf8 module - on non-EBCDIC systems, uni_to_native/native_to_uni are identity
(defpackage :utf8 (:use :cl :pcl))
(in-package :utf8)
(defun pl-uni_to_native (n) (unbox n))
(defun pl-native_to_uni (n) (unbox n))
(defun pl-encode (str) (declare (ignore str)) 1)
(defun pl-decode (str) (declare (ignore str)) 1)
(defun pl-upgrade (str) (declare (ignore str)) 1)
(defun pl-downgrade (str) (declare (ignore str)) 1)
(defun pl-is_utf8 (str) (declare (ignore str)) 1)
(in-package :pcl)

;; POSIX module stubs
(defpackage :|POSIX| (:use :cl :pcl))
(in-package :|POSIX|)
(defun pl-WIFEXITED (status) (= (logand (unbox status) #xff) 0))
(defun pl-WEXITSTATUS (status) (ash (logand (unbox status) #xff00) -8))
(in-package :pcl)

(format t "PCL Runtime loaded~%")
