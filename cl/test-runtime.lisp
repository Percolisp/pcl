;;;; PCL Runtime Tests

(load (merge-pathnames "pcl-runtime.lisp" *load-pathname*))
(in-package :pcl)

(defmacro test (name expr expected)
  "Simple test macro"
  `(let ((result ,expr))
     (if (equal result ,expected)
         (format t "PASS: ~A~%" ,name)
         (format t "FAIL: ~A - got ~S, expected ~S~%" ,name result ,expected))))

(format t "~%=== PCL Runtime Tests ===~%~%")

;;; Arithmetic
(format t "--- Arithmetic ---~%")
(test "pl-+ basic" (pl-+ 1 2) 3)
(test "pl-+ multiple" (pl-+ 1 2 3 4) 10)
(test "pl-- subtraction" (pl-- 10 3) 7)
(test "pl-- negation" (pl-- 5) -5)
(test "pl-* multiply" (pl-* 3 4) 12)
(test "pl-/ divide" (pl-/ 10 2) 5)
(test "pl-% modulo" (pl-% 10 3) 1)
(test "pl-** power" (pl-** 2 3) 8)

;;; Strings
(format t "~%--- Strings ---~%")
(test "pl-. concat" (pl-. "hello" " " "world") "hello world")
(test "pl-str-x repeat" (pl-str-x "ab" 3) "ababab")
(test "pl-length" (pl-length "hello") 5)
(test "pl-substr 2-arg" (pl-substr "hello" 1) "ello")
(test "pl-substr 3-arg" (pl-substr "hello" 1 3) "ell")
(test "pl-lc" (pl-lc "HELLO") "hello")
(test "pl-uc" (pl-uc "hello") "HELLO")
(test "pl-index" (pl-index "hello" "ll") 2)
(test "pl-index not found" (pl-index "hello" "x") -1)

;;; Numeric comparison
(format t "~%--- Numeric Comparison ---~%")
(test "pl-== true" (pl-== 5 5) t)
(test "pl-== false" (pl-== 5 6) nil)
(test "pl-!=" (pl-!= 5 6) t)
(test "pl-<" (pl-< 3 5) t)
(test "pl->" (pl-> 5 3) t)
(test "pl-<=>" (pl-<=> 5 10) -1)
(test "pl-<=> equal" (pl-<=> 5 5) 0)

;;; String comparison
(format t "~%--- String Comparison ---~%")
(test "pl-str-eq true" (pl-str-eq "hello" "hello") t)
(test "pl-str-eq false" (pl-str-eq "hello" "world") nil)
(test "pl-str-ne" (pl-str-ne "a" "b") t)
(test "pl-str-lt" (pl-str-lt "a" "b") t)
(test "pl-str-cmp" (pl-str-cmp "a" "b") -1)

;;; Logical
(format t "~%--- Logical ---~%")
(test "pl-!" (pl-! nil) 1)       ; Perl returns 1 for true
(test "pl-! true" (pl-! t) "")   ; Perl returns "" for false
(test "pl-true-p 0" (pl-true-p 0) nil)
(test "pl-true-p 1" (pl-true-p 1) t)
(test "pl-true-p empty" (pl-true-p "") nil)
(test "pl-true-p string" (pl-true-p "x") t)
(test "pl-defined undef" (pl-defined (pl-undef)) nil)
(test "pl-defined value" (pl-defined 5) t)

;;; Bitwise
(format t "~%--- Bitwise ---~%")
(test "pl-bit-and" (pl-bit-and 12 10) 8)
(test "pl-bit-or" (pl-bit-or 12 10) 14)
(test "pl-bit-xor" (pl-bit-xor 12 10) 6)
(test "pl-<<" (pl-<< 1 4) 16)
(test "pl->>" (pl->> 16 2) 4)

;;; Arrays
(format t "~%--- Arrays ---~%")
(let ((arr (make-pl-array 1 2 3)))
  (test "pl-aref" (pl-aref arr 1) 2)
  (test "pl-array-last-index" (pl-array-last-index arr) 2)
  (pl-push arr 4)
  (test "pl-push" (pl-aref arr 3) 4)
  (test "pl-pop" (pl-pop arr) 4)
  (test "after pop" (pl-array-last-index arr) 2))

;;; Hashes
(format t "~%--- Hashes ---~%")
(let ((h (pl-hash "a" 1 "b" 2)))
  (test "pl-gethash" (pl-gethash h "a") 1)
  (test "pl-gethash missing" (pl-defined (pl-gethash h "x")) nil)
  (test "pl-exists true" (pl-exists h "a") t)
  (test "pl-exists false" (pl-exists h "x") nil))

;;; Control flow
(format t "~%--- Control Flow ---~%")
(test "pl-if true" (pl-if t 1 2) 1)
(test "pl-if false" (pl-if nil 1 2) 2)
(test "pl-unless true" (pl-unless nil 1 2) 1)

;;; I/O
(format t "~%--- I/O ---~%")
(format t "pl-say test: ")
(pl-say "Hello from PCL!")

;;; OO - bless, ref, can, isa
(format t "~%--- OO (bless, ref, can, isa) ---~%")

;; Setup: Create a class hierarchy like the transpiler generates
;; Animal -> Dog (in PCL package with lowercase CLOS class names)

;; Create Animal package and class
(defpackage :Animal (:use :cl :pcl))
(in-package :Animal)
(defclass animal () ())
(sb-mop:finalize-inheritance (find-class 'animal))
(defun pl-speak (self) (declare (ignore self)) "generic sound")
(in-package :pcl)

;; Create Dog package and class inheriting from Animal
(defpackage :Dog (:use :cl :pcl))
(in-package :Dog)
(defclass dog (animal::animal) ())
(sb-mop:finalize-inheritance (find-class 'dog))
(defun pl-speak (self) (declare (ignore self)) "woof")
(defun pl-fetch (self) (declare (ignore self)) "fetching!")
(in-package :pcl)

;; Test bless and ref
(let ((obj (pl-bless (pl-hash "name" "Fido") "Dog")))
  (test "pl-bless creates object" (hash-table-p obj) t)
  (test "pl-ref returns class" (pl-ref obj) "Dog")
  (test "pl-ref on unblessed" (pl-ref (pl-hash)) "HASH")
  (test "pl-ref on string" (pl-ref "hello") ""))

;; Test can() - method existence checking
(let ((dog (pl-bless (pl-hash) "Dog")))
  (test "can() finds method" (not (null (pl-can dog "speak"))) t)
  (test "can() finds inherited" (not (null (pl-can dog "speak"))) t)
  (test "can() returns nil for missing" (pl-can dog "nonexistent") nil)
  (test "can() on Dog for fetch" (not (null (pl-can dog "fetch"))) t))

;; Test isa() - inheritance checking
(let ((dog (pl-bless (pl-hash) "Dog")))
  (test "isa() own class" (pl-isa dog "Dog") t)
  (test "isa() parent class" (pl-isa dog "Animal") t)
  (test "isa() unrelated class" (pl-isa dog "Cat") nil))

;; Test can/isa with string (class method style)
(test "isa class method" (pl-isa "Dog" "Animal") t)
(test "can class method" (not (null (pl-can "Dog" "speak"))) t)

;;; Summary
(format t "~%=== Tests Complete ===~%")
