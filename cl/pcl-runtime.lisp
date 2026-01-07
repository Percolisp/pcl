;;;; PCL Runtime - Common Lisp runtime for Perl to CL transpiler
;;;; Requires SBCL (Steel Bank Common Lisp)

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
   #:pl-. #:pl-str-x #:pl-length #:pl-substr #:pl-lc #:pl-uc
   #:pl-chomp #:pl-chop #:pl-index #:pl-rindex #:pl-string_concat
   #:pl-chr #:pl-ord #:pl-hex #:pl-oct #:pl-lcfirst #:pl-ucfirst #:pl-sprintf #:pl-printf
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
   ;; Logical
   #:pl-&& #:pl-|| #:pl-! #:pl-not #:pl-and #:pl-or #:pl-xor #:pl-//
   ;; Bitwise
   #:pl-bit-and #:pl-bit-or #:pl-bit-xor #:pl-bit-not #:pl-<< #:pl->>
   ;; Data structures
   #:pl-aref #:pl-aref-deref #:pl-gethash #:pl-gethash-deref
   #:pl-aslice #:pl-hslice
   #:pl-hash #:pl-array-last-index
   #:pl-push #:pl-pop #:pl-shift #:pl-unshift #:pl-splice
   #:pl-keys #:pl-values #:pl-each #:pl-exists #:pl-delete
   ;; Control flow
   #:pl-if #:pl-unless #:pl-while #:pl-until #:pl-for #:pl-foreach
   #:pl-return #:pl-last #:pl-next #:pl-redo
   ;; I/O
   #:pl-print #:pl-say #:pl-warn #:pl-die
   ;; File I/O
   #:pl-open #:pl-close #:pl-eof #:pl-tell #:pl-seek
   #:pl-binmode #:pl-read #:pl-sysread #:pl-syswrite
   #:pl-truncate #:pl-stat #:pl-lstat #:pl-unlink #:pl-fileno #:pl-getc #:pl-readline
   ;; Directory I/O
   #:pl-opendir #:pl-readdir #:pl-closedir #:pl-rewinddir
   ;; File/Directory operations
   #:pl-chdir #:pl-mkdir #:pl-rmdir #:pl-getcwd #:pl-cwd #:pl-rename #:pl-chmod
   ;; Time functions
   #:pl-time #:pl-sleep #:pl-localtime #:pl-gmtime
   ;; Process control
   #:pl-exit #:pl-system #:pl-backtick
   ;; Environment
   #:%ENV #:pl-env-get #:pl-env-set
   ;; Functions
   #:pl-backslash #:pl-ref #:pl-reftype #:pl-scalar #:pl-wantarray
   #:pl-grep #:pl-map #:pl-sort #:pl-reverse
   #:pl-join #:pl-split #:pl-funcall-ref
   ;; Dereferencing (sigil cast operations)
   #:pl-cast-@ #:pl-cast-% #:pl-cast-$
   ;; OO
   #:pl-bless #:pl-get-class #:pl-method-call #:pl-resolve-invocant
   ;; Regex
   #:pl-=~ #:pl-!~ #:pl-subst #:pl-tr #:pl-regex
   ;; Capture groups
   #:$1 #:$2 #:$3 #:$4 #:$5 #:$6 #:$7 #:$8 #:$9
   ;; Special variables
   #:$$))

(in-package :pcl)

;;; Forward declarations to avoid style warnings
(declaim (ftype (function (t) t) to-number to-string unbox pl-get-stream))
(defvar *pl-undef* :undef "Perl's undef value")

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

;;; Wantarray context variable
(defvar *wantarray* nil "True when list context is expected")

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

;;; ------------------------------------------------------------
;;; Box accessors with lazy caching
;;; ------------------------------------------------------------

(defun box-set (box value)
  "Set box value, invalidating caches. Pre-caches if already typed.
   If value is a box containing a primitive, unbox it (Perl copy semantics).
   If value is a box containing another box (reference), preserve it.
   If value is a blessed box, copy the class to target box."
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
    v))

(defun parse-perl-number (str)
  "Parse a string to number using Perl semantics.
   Handles integers, floats, scientific notation.
   Returns 0 for non-numeric strings."
  (when (stringp str)
    (let ((trimmed (string-left-trim '(#\Space #\Tab #\Newline) str)))
      (when (> (length trimmed) 0)
        (multiple-value-bind (n pos)
            (let ((*read-eval* nil))  ; Safety: disable #. reader macro
              (ignore-errors
                (read-from-string trimmed)))
          (declare (ignore pos))
          (when (numberp n)
            (return-from parse-perl-number n))))))
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
     ;; Format floats without trailing zeros, like Perl
     (let ((s (format nil "~F" v)))
       (string-right-trim "0" (string-right-trim "." s))))
    ((numberp v) (write-to-string v))
    ((pl-box-p v) (format nil "SCALAR(0x~X)" (object-address v)))
    ((hash-table-p v) (format nil "HASH(0x~X)" (object-address v)))
    ((vectorp v) (format nil "ARRAY(0x~X)" (object-address v)))
    ;; Lists (from return lists, etc.) - join with spaces like Perl's @array interpolation
    ((listp v) (format nil "~{~A~^ ~}" (mapcar #'to-string v)))
    (t (format nil "~A" v))))

(defun box-sv (box)
  "Get string value from box with lazy caching"
  (if (pl-box-sv-ok box)
      (pl-box-sv box)
      (let ((s (stringify-value (pl-box-value box))))
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

(defun pl-undef ()
  "Return Perl's undef value"
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
  "Perl subtraction"
  (if (= (length args) 1)
      (- (to-number (first args)))
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
  "Perl exponentiation"
  (expt (to-number a) (to-number b)))

(defun pl-int (val)
  "Perl int - truncate toward zero"
  (truncate (to-number val)))

(defun pl-abs (val)
  "Perl abs - absolute value"
  (abs (to-number val)))

(defun pl-sin (val)
  "Perl sin - sine"
  (sin (to-number val)))

(defun pl-cos (val)
  "Perl cos - cosine"
  (cos (to-number val)))

(defun pl-atan2 (y x)
  "Perl atan2 - arctangent of y/x"
  (atan (to-number y) (to-number x)))

(defun pl-exp (val)
  "Perl exp - e^x"
  (exp (to-number val)))

(defun pl-log (val)
  "Perl log - natural logarithm"
  (log (to-number val)))

(defun pl-sqrt (val)
  "Perl sqrt - square root"
  (sqrt (to-number val)))

(defun pl-rand (&optional max)
  "Perl rand - random number"
  (if max
      (* (random 1.0) (to-number max))
      (random 1.0)))

(defun pl-srand (&optional seed)
  "Perl srand - seed random number generator"
  (declare (ignore seed))
  ;; CL doesn't have portable srand - just return a value
  1)

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
        ((stringp val) (parse-perl-number val))
        (t 0))))

;;; ============================================================
;;; String Operators
;;; ============================================================

(defun pl-. (&rest args)
  "Perl string concatenation"
  (apply #'concatenate 'string (mapcar #'to-string args)))

(defun pl-string_concat (&rest args)
  "Perl string concatenation (alias for interpolation)"
  (apply #'pl-. args))

(defun pl-str-x (str count)
  "Perl string repetition operator (x)"
  (let ((s (to-string str))
        (n (truncate (to-number count))))
    (if (<= n 0)
        ""
        (apply #'concatenate 'string (make-list n :initial-element s)))))

(defun to-string (val)
  "Convert value to string (Perl semantics).
   Uses lazy caching for boxed values."
  (if (pl-box-p val)
      (box-sv val)
      ;; Raw value - convert directly
      (stringify-value val)))

(defun pl-length (val)
  "Perl length function"
  (length (to-string val)))

(defun pl-substr (str start &optional len replacement)
  "Perl substr function"
  (let* ((s (to-string str))
         (slen (length s))
         (st (truncate (to-number start)))
         ;; Handle negative start
         (st (if (< st 0) (max 0 (+ slen st)) st)))
    (if replacement
        ;; 4-arg form: replace
        (let ((ln (if len (truncate (to-number len)) (- slen st))))
          (concatenate 'string
                       (subseq s 0 (min st slen))
                       replacement
                       (subseq s (min (+ st ln) slen))))
        ;; 2 or 3 arg form: extract
        (let ((ln (if len (truncate (to-number len)) (- slen st))))
          (subseq s (min st slen) (min (+ st ln) slen))))))

(defun pl-lc (str)
  "Perl lc - lowercase"
  (string-downcase (to-string str)))

(defun pl-uc (str)
  "Perl uc - uppercase"
  (string-upcase (to-string str)))

(defun pl-chomp (var)
  "Perl chomp - remove trailing newline, modifies variable in place.
   Returns number of characters removed."
  (if (pl-box-p var)
      (let* ((s (to-string (pl-box-value var)))
             (len (length s)))
        (if (and (> len 0) (char= (char s (1- len)) #\Newline))
            (progn
              (setf (pl-box-value var) (subseq s 0 (1- len))
                    (pl-box-sv-ok var) nil)
              1)
            0))
      ;; Non-box: just return 0, can't modify
      0))

(defun pl-chop (var)
  "Perl chop - remove last character, modifies variable in place.
   Returns the removed character."
  (if (pl-box-p var)
      (let* ((s (to-string (pl-box-value var)))
             (len (length s)))
        (if (> len 0)
            (let ((removed (subseq s (1- len))))
              (setf (pl-box-value var) (subseq s 0 (1- len))
                    (pl-box-sv-ok var) nil)
              removed)
            ""))
      ;; Non-box: return empty string
      ""))

(defun pl-index (str substr &optional start)
  "Perl index - find substring"
  (let ((pos (search (to-string substr) (to-string str)
                     :start2 (if start (truncate (to-number start)) 0))))
    (or pos -1)))

(defun pl-rindex (str substr &optional start)
  "Perl rindex - find substring from end"
  (let ((s (to-string str))
        (sub (to-string substr)))
    (let ((pos (search sub s :from-end t
                       :end2 (if start (1+ (truncate (to-number start))) nil))))
      (or pos -1))))

(defun pl-chr (n)
  "Perl chr - character from code point"
  (string (code-char (truncate (to-number n)))))

(defun pl-ord (str)
  "Perl ord - code point of first character"
  (let ((s (to-string str)))
    (if (> (length s) 0)
        (char-code (char s 0))
        0)))

(defun pl-hex (str)
  "Perl hex - convert hex string to number"
  (let ((s (string-trim '(#\Space #\Tab) (to-string str))))
    (parse-integer s :radix 16 :junk-allowed t)))

(defun pl-oct (str)
  "Perl oct - convert octal/hex/binary string to number"
  (let ((s (string-trim '(#\Space #\Tab) (to-string str))))
    (cond
      ((and (> (length s) 1) (char= (char s 0) #\0))
       (cond
         ((member (char s 1) '(#\x #\X))
          (parse-integer (subseq s 2) :radix 16 :junk-allowed t))
         ((member (char s 1) '(#\b #\B))
          (parse-integer (subseq s 2) :radix 2 :junk-allowed t))
         (t (parse-integer s :radix 8 :junk-allowed t))))
      (t (parse-integer s :radix 8 :junk-allowed t)))))

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

(defun pl-sprintf (fmt &rest args)
  "Perl sprintf - formatted string"
  (let ((fmt-str (to-string fmt)))
    (with-output-to-string (out)
      (let ((i 0)
            (arg-idx 0)
            (len (length fmt-str)))
        (loop while (< i len) do
          (let ((c (char fmt-str i)))
            (if (and (char= c #\%) (< (1+ i) len))
                ;; Format specifier
                (let ((next (char fmt-str (1+ i))))
                  (cond
                    ;; %% - literal %
                    ((char= next #\%)
                     (write-char #\% out)
                     (incf i 2))
                    ;; %s - string
                    ((char= next #\s)
                     (princ (to-string (nth arg-idx args)) out)
                     (incf arg-idx)
                     (incf i 2))
                    ;; %d - integer
                    ((char= next #\d)
                     (princ (truncate (to-number (nth arg-idx args))) out)
                     (incf arg-idx)
                     (incf i 2))
                    ;; %.Nf - float with N decimal places
                    ((char= next #\.)
                     (let ((j (+ i 2))
                           (precision 0))
                       ;; Parse digits
                       (loop while (and (< j len) (digit-char-p (char fmt-str j)))
                             do (setf precision (+ (* precision 10)
                                                   (digit-char-p (char fmt-str j))))
                                (incf j))
                       ;; Check for 'f'
                       (if (and (< j len) (char= (char fmt-str j) #\f))
                           (progn
                             (format out "~,vF" precision (to-number (nth arg-idx args)))
                             (incf arg-idx)
                             (setf i (1+ j)))
                           ;; Unknown, output literally
                           (progn
                             (write-char c out)
                             (incf i)))))
                    ;; %f - float (default precision)
                    ((char= next #\f)
                     (princ (to-number (nth arg-idx args)) out)
                     (incf arg-idx)
                     (incf i 2))
                    ;; Unknown specifier, output literally
                    (t
                     (write-char c out)
                     (incf i))))
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

;; pl-setf sets the VALUE inside a box, not the box itself.
;; This is key to making references work correctly.
;; For simple symbols (scalars), auto-declares as global if unbound.
;; For array/hash access, uses CL's setf with our setf expanders.
;; Special handling for references: (pl-backslash ...) stores box directly.
(defmacro pl-setf (place value)
  "Perl assignment - sets value inside box, auto-declares if needed"
  (cond
    ;; Simple scalar variable
    ((symbolp place)
     ;; Check if value is a reference (pl-backslash)
     (if (and (listp value) (eq (car value) 'pl-backslash))
         ;; Reference assignment - store box directly, don't unbox
         (let ((val (gensym "VAL")))
           `(let ((,val ,value))  ; Evaluates to the referenced box
              (unless (boundp ',place)
                (proclaim '(special ,place))
                (setf (symbol-value ',place) (make-pl-box nil)))
              ;; Store the box directly as the value (not unboxed)
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
    ;; Array/hash ref access and scalar deref - use CL setf
    ((and (listp place)
          (member (car place) '(pl-aref-deref pl-gethash-deref pl-$)))
     `(setf ,place ,value))
    ;; Array/hash access with complex expression (not simple symbol) - use CL setf
    ((and (listp place)
          (member (car place) '(pl-aref pl-gethash)))
     `(setf ,place ,value))
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

(defmacro pl-pre++ (place)
  "Perl prefix ++ - works on boxed values, hash/array elements, and derefs"
  (if (and (listp place)
           (member (car place) '(pl-gethash pl-aref pl-gethash-deref pl-aref-deref pl-$)))
      ;; Hash/array element or deref - use incf
      `(incf ,place)
      ;; Boxed scalar
      `(box-set ,place (1+ (to-number ,place)))))

(defmacro pl-post++ (place)
  "Perl postfix ++ - returns old value"
  (let ((old (gensym "OLD")))
    (if (and (listp place)
             (member (car place) '(pl-gethash pl-aref pl-gethash-deref pl-aref-deref pl-$)))
        ;; Hash/array element or deref - use incf, return old value
        `(let ((,old ,place))
           (incf ,place)
           ,old)
        ;; Boxed scalar
        `(let ((,old (to-number ,place)))
           (box-set ,place (1+ ,old))
           ,old))))

(defmacro pl-pre-- (place)
  "Perl prefix -- - works on boxed values, hash/array elements, and derefs"
  (if (and (listp place)
           (member (car place) '(pl-gethash pl-aref pl-gethash-deref pl-aref-deref pl-$)))
      ;; Hash/array element or deref - use decf
      `(decf ,place)
      ;; Boxed scalar
      `(box-set ,place (1- (to-number ,place)))))

(defmacro pl-post-- (place)
  "Perl postfix -- - returns old value"
  (let ((old (gensym "OLD")))
    (if (and (listp place)
             (member (car place) '(pl-gethash pl-aref pl-gethash-deref pl-aref-deref pl-$)))
        ;; Hash/array element or deref - use decf, return old value
        `(let ((,old ,place))
           (decf ,place)
           ,old)
        ;; Boxed scalar
        `(let ((,old (to-number ,place)))
           (box-set ,place (1- ,old))
           ,old))))

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
  "Perl &&= (and-assign) - assigns value only if place is true"
  (let ((tmp (gensym "TMP")))
    `(let ((,tmp ,place))
       (if (pl-true-p ,tmp)
           (box-set ,place ,value)
           (unbox ,tmp)))))

(defmacro pl-or-assign (place value)
  "Perl ||= (or-assign) - assigns value only if place is false"
  (let ((tmp (gensym "TMP")))
    `(let ((,tmp ,place))
       (if (pl-true-p ,tmp)
           (unbox ,tmp)
           (box-set ,place ,value)))))

(defmacro pl-//= (place value)
  "Perl //= (defined-or-assign) - assigns value only if place is undef"
  (let ((tmp (gensym "TMP")))
    `(let ((,tmp ,place))
       (if (pl-defined ,tmp)
           (unbox ,tmp)
           (box-set ,place ,value)))))

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
  "Perl bitwise NOT"
  (lognot (truncate (to-number a))))

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
  "Perl array access (supports negative indices, works on vectors and lists)"
  (let* ((a (if (pl-box-p arr) (pl-box-value arr) arr))  ; Unbox if needed
         (i (truncate (to-number idx)))
         (len (cond ((vectorp a) (length a))
                    ((listp a) (length a))
                    (t 0)))
         (actual-idx (if (< i 0) (+ len i) i)))
    (cond
      ((and (vectorp a) (>= actual-idx 0) (< actual-idx len))
       (aref a actual-idx))
      ((and (listp a) (>= actual-idx 0) (< actual-idx len))
       (nth actual-idx a))
      (t *pl-undef*))))

(defun (setf pl-aref) (value arr idx)
  "Setf expander for pl-aref - allows assignment to array elements (vectors and lists)"
  (let* ((i (truncate (to-number idx)))
         (len (cond ((vectorp arr) (length arr))
                    ((listp arr) (length arr))
                    (t 0)))
         (actual-idx (if (< i 0) (+ len i) i)))
    (cond
      ((and (vectorp arr) (>= actual-idx 0) (< actual-idx len))
       (setf (aref arr actual-idx) value))
      ((and (listp arr) (>= actual-idx 0) (< actual-idx len))
       (setf (nth actual-idx arr) value)))
    value))

(defun pl-aref-deref (ref idx)
  "Perl array ref access $ref->[idx] - unbox the reference first"
  (pl-aref (unbox ref) idx))

(defun pl-array-last-index (arr)
  "Perl $#arr - last index"
  (if (vectorp arr)
      (1- (length arr))
      -1))

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

(defun pl-push-impl (arr &rest items)
  "Implementation of push - copies values, doesn't store box references"
  (dolist (item items)
    ;; Unbox to get value, then store as new box (copy semantics)
    (let ((val (if (pl-box-p item) (pl-box-value item) item)))
      (vector-push-extend val arr)))
  (length arr))

(defun pl-pop (arr)
  "Perl pop - removes from end"
  (if (and (vectorp arr) (> (length arr) 0))
      (vector-pop arr)
      *pl-undef*))

(defun pl-shift (arr)
  "Perl shift - removes from front. Works with vectors and lists."
  (cond
    ((and (vectorp arr) (> (length arr) 0))
     (let ((first (aref arr 0)))
       ;; Shift elements down
       (loop for i from 0 below (1- (length arr))
             do (setf (aref arr i) (aref arr (1+ i))))
       (vector-pop arr)
       first))
    ((consp arr)
     ;; For lists (like @_ from &rest), just return car
     ;; Note: this doesn't modify the list, caller should use pop or similar
     (car arr))
    (t *pl-undef*)))

(defun pl-unshift (arr &rest items)
  "Perl unshift - adds to front"
  (let ((nitems (length items)))
    ;; Make room
    (dotimes (i nitems)
      (vector-push-extend *pl-undef* arr))
    ;; Shift existing elements up
    (loop for i from (1- (length arr)) downto nitems
          do (setf (aref arr i) (aref arr (- i nitems))))
    ;; Insert new items at front
    (loop for i from 0
          for item in items
          do (setf (aref arr i) item))
    (length arr)))

;;; ============================================================
;;; Data Structures - Hashes
;;; ============================================================

(defun pl-gethash (hash key)
  "Perl hash access. Special handling for %ENV."
  (let* ((h (if (pl-box-p hash) (pl-box-value hash) hash))
         (k (to-string key)))
    ;; Check for %ENV marker
    (if (eq h '%ENV-MARKER%)
        (or (sb-posix:getenv k) *pl-undef*)
        (multiple-value-bind (val found) (gethash k h)
          (if found val *pl-undef*)))))

(defun (setf pl-gethash) (value hash key)
  "Setf expander for pl-gethash - allows assignment to hash elements.
   Special handling for %ENV."
  (let* ((h (if (pl-box-p hash) (pl-box-value hash) hash))
         (k (to-string key)))
    (if (eq h '%ENV-MARKER%)
        (progn (sb-posix:setenv k (to-string value) 1) value)
        (setf (gethash k h) value))))

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
  "Perl array slice @arr[indices] - returns list of values"
  (mapcar (lambda (idx) (pl-aref arr idx)) indices))

(defun pl-hslice (hash &rest keys)
  "Perl hash slice @hash{keys} - returns list of values"
  (mapcar (lambda (key) (pl-gethash hash key)) keys))

(defun pl-hash (&rest pairs)
  "Create a Perl hash from key-value pairs.
   Values are unboxed to get copy semantics (like Perl)."
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash (to-string k) h) (unbox v)))
    h))

(defun pl-keys (hash)
  "Perl keys function"
  (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
    (maphash (lambda (k v)
               (declare (ignore v))
               (vector-push-extend k result))
             hash)
    result))

(defun pl-values (hash)
  "Perl values function"
  (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
    (maphash (lambda (k v)
               (declare (ignore k))
               (vector-push-extend v result))
             hash)
    result))

(defun pl-exists (hash key)
  "Perl exists function"
  (multiple-value-bind (val found) (gethash (to-string key) hash)
    (declare (ignore val))
    found))

(defun pl-delete (hash key)
  "Perl delete function"
  (let ((k (to-string key)))
    (prog1 (gethash k hash *pl-undef*)
      (remhash k hash))))

;;; ============================================================
;;; Control Flow
;;; ============================================================

(defmacro pl-if (condition then-form &optional else-form)
  "Perl if/unless and ternary"
  `(if (pl-true-p ,condition) ,then-form ,else-form))

(defmacro pl-unless (condition then-form &optional else-form)
  "Perl unless"
  `(if (not (pl-true-p ,condition)) ,then-form ,else-form))

(defmacro pl-while (condition &rest body-and-keys)
  "Perl while loop with optional :label"
  ;; Extract :label keyword if present at start of body
  (let* ((label (when (eq (first body-and-keys) :label)
                  (second body-and-keys)))
         (body (if label (cddr body-and-keys) body-and-keys))
         (block-name (or label (gensym "WHILE")))
         (next-tag (when label (intern (format nil "NEXT-~A" label) :pcl)))
         (iter-block (gensym "ITER")))
    (if label
        ;; Labeled loop - use catch for labeled next
        `(block ,block-name
           (loop while (pl-true-p ,condition)
                 do (catch ',next-tag
                      (block ,iter-block
                        (tagbody ,@body :next)))
                 finally (return-from ,block-name nil)))
        ;; Unlabeled loop - use (block nil ...) so (return nil) exits the loop
        ;; Inner block uses gensym so it doesn't shadow the nil block
        `(block nil
           (loop while (pl-true-p ,condition)
                 do (block ,iter-block
                      (tagbody ,@body :next)))))))

(defmacro pl-until (condition &body body)
  "Perl until loop"
  `(pl-while (pl-! ,condition) ,@body))

(defmacro pl-for ((init) (test) (step) &rest body-and-keys)
  "Perl C-style for loop with optional :label.
   Each of init, test, step is a single form wrapped in parens."
  ;; Extract :label keyword if present at start of body
  (let* ((label (when (eq (first body-and-keys) :label)
                  (second body-and-keys)))
         (body (if label (cddr body-and-keys) body-and-keys))
         (block-name (or label (gensym "FOR")))
         (next-tag (when label (intern (format nil "NEXT-~A" label) :pcl)))
         (iter-block (gensym "ITER")))
    (if label
        ;; Labeled loop - use catch for labeled next
        `(block ,block-name
           ,init
           (loop while (pl-true-p ,test)
                 do (catch ',next-tag
                      (block ,iter-block
                        (tagbody ,@body :next)))
                    ,step
                 finally (return-from ,block-name nil)))
        ;; Unlabeled loop - use (block nil ...) so (return nil) exits the loop
        `(block nil
           ,init
           (loop while (pl-true-p ,test)
                 do (block ,iter-block
                      (tagbody ,@body :next))
                    ,step)))))

(defmacro pl-foreach ((var list) &rest body-and-keys)
  "Perl foreach loop with optional :label - works with boxed or raw vectors"
  ;; Extract :label keyword if present at start of body
  (let* ((label (when (eq (first body-and-keys) :label)
                  (second body-and-keys)))
         (body (if label (cddr body-and-keys) body-and-keys))
         (block-name (or label (gensym "FOREACH")))
         (next-tag (when label (intern (format nil "NEXT-~A" label) :pcl)))
         (iter-block (gensym "ITER"))
         (item (gensym))
         (vec (gensym)))
    (if label
        ;; Labeled loop - use catch for labeled next
        `(block ,block-name
           (let ((,vec (if (pl-box-p ,list) (pl-box-value ,list) ,list)))
             (loop for ,item across ,vec
                   do (let ((,var ,item))
                        (catch ',next-tag
                          (block ,iter-block
                            (tagbody ,@body :next))))
                   finally (return-from ,block-name nil))))
        ;; Unlabeled loop - use (block nil ...) so (return nil) exits the loop
        `(block nil
           (let ((,vec (if (pl-box-p ,list) (pl-box-value ,list) ,list)))
             (loop for ,item across ,vec
                   do (let ((,var ,item))
                        (block ,iter-block
                          (tagbody ,@body :next)))))))))

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

(defmacro pl-redo ()
  "Perl redo"
  `(go :redo))

;;; ============================================================
;;; I/O Functions
;;; ============================================================

(defun pl-print (&rest args)
  "Perl print"
  (let ((fh *standard-output*))
    ;; Check for :fh keyword
    (when (and (>= (length args) 2) (eq (first args) :fh))
      (setf fh (pl-get-stream (second args)))
      (setf args (cddr args)))
    (dolist (arg args)
      (princ (to-string arg) fh))
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

(defun pl-die (&rest args)
  "Perl die"
  (error (apply #'pl-. args)))

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

(defmacro pl-open (fh mode filename)
  "Perl open - open file with given mode.
   Mode: '<' (read), '>' (write), '>>' (append), '+<' (read-write)"
  `(%pl-open-impl ',fh ,mode ,filename))

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
  "Perl readline / diamond operator <FH> - read a line from filehandle.
   Returns nil at EOF. In scalar context returns one line.
   If no filehandle given, reads from *standard-input*.
   Note: Unlike CL's read-line, this keeps the trailing newline (like Perl)."
  (let ((stream (if fh (pl-get-stream fh) *standard-input*)))
    (when stream
      (multiple-value-bind (line missing-newline-p)
          (read-line stream nil nil)
        (cond
          ((null line) nil)  ; EOF
          (missing-newline-p line)  ; Last line without newline
          (t (concatenate 'string line (string #\Newline))))))))

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
;;; File/Directory Operations
;;; ============================================================

(defun pl-chdir (&optional dir)
  "Perl chdir - change current directory. Returns true on success."
  (let ((path (if dir (to-string dir) (sb-posix:getenv "HOME"))))
    (handler-case
        (progn (sb-posix:chdir path) t)
      (error () nil))))

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

(defun pl-sleep (secs)
  "Perl sleep - pause execution for specified seconds. Returns seconds slept."
  (let ((n (truncate (to-number secs))))
    (sleep n)
    n))

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

(defun pl-sort (fn list)
  "Perl sort - fn receives $a and $b parameters, returns negative if a < b"
  (let ((result (copy-seq (unbox list))))
    (sort result (lambda (a b)
                   (< (funcall fn a b) 0)))))

(defun pl-reverse (seq)
  "Perl reverse"
  (reverse (unbox seq)))

(defun pl-join (sep list)
  "Perl join"
  (let ((s (to-string sep))
        (arr (unbox list)))
    (with-output-to-string (out)
      (loop for i from 0 below (length arr)
            do (when (> i 0) (princ s out))
               (princ (to-string (aref arr i)) out)))))

(defun pl-split (pattern str &optional limit)
  "Perl split - split string by pattern (basic: literal string or simple regex)"
  (let* ((s (to-string str))
         (pat (to-string pattern))
         (result (make-array 0 :adjustable t :fill-pointer 0))
         (start 0)
         (pat-len (length pat)))
    (if (zerop pat-len)
        ;; Empty pattern: split into characters
        (loop for c across s
              do (vector-push-extend (string c) result))
        ;; Non-empty pattern: split by literal match
        (loop
          (let ((pos (search pat s :start2 start)))
            (if (and pos (or (null limit) (< (length result) (1- limit))))
                (progn
                  (vector-push-extend (subseq s start pos) result)
                  (setf start (+ pos pat-len)))
                (progn
                  (vector-push-extend (subseq s start) result)
                  (return))))))
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
  (if (pl-box-p val) (pl-box-value val) val))

(defun pl-cast-% (val)
  "Perl hash dereference %{$ref} - unbox to get the hash"
  (if (pl-box-p val) (pl-box-value val) val))

(defun pl-cast-$ (val)
  "Perl scalar dereference ${$ref} - get value from reference.
   $ref contains a reference (box), $$ref gets the referenced value."
  (let ((inner (if (pl-box-p val) (pl-box-value val) val)))
    ;; inner is the reference (a box), get its value
    (if (pl-box-p inner)
        (pl-box-value inner)
        inner)))

(defun pl-ref (val)
  "Perl ref() function - get reference type or class name if blessed.
   Returns empty string for non-references."
  ;; Unbox the variable to get what it contains
  (let ((inner (if (pl-box-p val) (pl-box-value val) val)))
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
      ;; Array reference (list or vector)
      ((or (listp inner) (vectorp inner)) "ARRAY")
      ;; Code reference
      ((functionp inner) "CODE")
      ;; Not a reference
      (t ""))))

;; Keep reftype as an alias for compatibility
(defun pl-reftype (val)
  "Alias for pl-ref"
  (pl-ref val))

(defun pl-scalar (val)
  "Perl scalar function - returns length for arrays, value for scalars"
  (let ((v (if (pl-box-p val) (pl-box-value val) val)))
    (if (vectorp v)
        (length v)
        v)))

(defun pl-wantarray ()
  "Perl wantarray"
  *wantarray*)

;;; ============================================================
;;; OO Support
;;; ============================================================

;; Simple blessing - store class name in hash
(defun pl-bless (ref class)
  "Perl bless - attach class to a reference (hash, array, or scalar ref).
   Always stores class on the inner value so it survives unboxing."
  (let ((class-name (to-string class))
        (inner (if (pl-box-p ref) (pl-box-value ref) ref)))
    (cond
      ((hash-table-p inner)
       (setf (gethash :__class__ inner) class-name))
      ((vectorp inner)
       ;; For arrays, store class in a property (using adjustable array trick)
       ;; For now, we'll use a simple approach with a hash wrapper if needed
       ;; TODO: Better array blessing support
       (warn "Blessing arrays not fully supported yet"))
      ((pl-box-p inner)
       ;; Scalar reference - store on the inner box
       (setf (pl-box-class inner) class-name))))
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
  "Perl method call - looks up pl-METHOD function in object's package and calls it"
  (let* ((method-name (to-string method))
         (class-name (pl-get-class obj)))
    (unless class-name
      (error "Can't call method ~A on non-blessed reference" method-name))
    ;; Find the package for this class
    (let ((pkg (find-package (string-upcase class-name))))
      (unless pkg
        (error "Package ~A not found for method call" class-name))
      ;; Look up pl-METHOD in that package (pl- prefix avoids CL conflicts)
      (let ((fn (find-symbol (string-upcase (format nil "PL-~A" method-name)) pkg)))
        (if (and fn (fboundp fn))
            (apply fn obj args)
            (error "Can't locate method ~A in package ~A" method-name class-name))))))

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

(defun pl-regex (pattern-string)
  "Parse /pattern/modifiers and return a regex-match struct.
   Pattern-string is like '/foo/i' or 'm/bar/g'"
  (let* ((str (to-string pattern-string))
         (first-char (char str 0))
         (start-delim (if (char= first-char #\m) 1 0))
         (delim (char str start-delim))
         (end-delim (position delim str :start (1+ start-delim) :from-end t))
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
  "Perform regex match, return t if matched, nil otherwise.
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
              t)))
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
;;; Package initialization
;;; ============================================================

(format t "PCL Runtime loaded~%")
