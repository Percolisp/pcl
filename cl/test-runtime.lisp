;;;; PCL Runtime Tests

(load "pcl-runtime.lisp")
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
(test "pl-x repeat" (pl-x "ab" 3) "ababab")
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
(test "pl-eq true" (pl-eq "hello" "hello") t)
(test "pl-eq false" (pl-eq "hello" "world") nil)
(test "pl-ne" (pl-ne "a" "b") t)
(test "pl-lt" (pl-lt "a" "b") t)
(test "pl-cmp" (pl-cmp "a" "b") -1)

;;; Logical
(format t "~%--- Logical ---~%")
(test "pl-!" (pl-! nil) t)
(test "pl-! true" (pl-! t) nil)
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

;;; Summary
(format t "~%=== Tests Complete ===~%")
