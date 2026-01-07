;;;; Test file for PCL boxing implementation
;;;; Run with: sbcl --load pcl-runtime.lisp --load test-boxing.lisp

(in-package :pcl)

(format t "~%=== Testing PCL Boxing ===~%~%")

;;; Test 1: Basic boxed arithmetic
(format t "Test 1: Basic boxed arithmetic~%")
(pl-let (($x 5) ($y 3))
  (let ((result (pl-+ $x $y)))
    (format t "  $x=5, $y=3, $x+$y = ~A (expected: 8)~%" result)
    (assert (= result 8))))

;;; Test 2: Assignment modifies box value
(format t "Test 2: Assignment modifies box value~%")
(pl-let (($x 10))
  (format t "  Initial $x = ~A~%" (unbox $x))
  (pl-setf $x 20)
  (format t "  After $x=20, $x = ~A (expected: 20)~%" (unbox $x))
  (assert (= (unbox $x) 20)))

;;; Test 3: References work - the key test!
(format t "Test 3: References (the key test!)~%")
(pl-let (($x 10))
  (pl-let (($ref (pl-backslash $x)))  ; $ref = \$x
    ;; $ref is a box whose VALUE is the box $x
    ;; pl-$ auto-unboxes $ref to get the reference, then gets its value
    (format t "  $x = ~A~%" (unbox $x))
    (format t "  $ref's value points to $x's box? ~A~%" (eq (unbox $ref) $x))
    (assert (eq (unbox $ref) $x) () "Reference value should be $x's box")
    ;; Modify via reference: $$ref is (pl-$ $ref)
    ;; pl-$ unboxes $ref to get the reference (box $x), then sets its value
    (setf (pl-$ $ref) 20)  ; $$ref = 20
    (format t "  After $$ref=20, $x = ~A (expected: 20)~%" (unbox $x))
    (assert (= (unbox $x) 20) () "Modifying $$ref should modify $x")))

;;; Test 4: Increment/decrement
(format t "Test 4: Increment/decrement~%")
(pl-let (($x 5))
  (let ((old (pl-++-post $x)))
    (format t "  $x++=~A, now $x=~A (expected: 5, 6)~%" old (unbox $x))
    (assert (= old 5))
    (assert (= (unbox $x) 6)))
  (let ((new (pl-++-pre $x)))
    (format t "  ++$x=~A, now $x=~A (expected: 7, 7)~%" new (unbox $x))
    (assert (= new 7))
    (assert (= (unbox $x) 7))))

;;; Test 5: String operations with boxed values
(format t "Test 5: String operations~%")
(pl-let (($name "World"))
  (let ((greeting (pl-. "Hello, " $name "!")))
    (format t "  ~A (expected: Hello, World!)~%" greeting)
    (assert (string= greeting "Hello, World!"))))

;;; Test 6: Boolean operations with boxed values
(format t "Test 6: Boolean operations~%")
(pl-let (($zero 0) ($one 1) ($empty ""))
  (format t "  pl-true-p($zero=0) = ~A (expected: NIL)~%" (pl-true-p $zero))
  (format t "  pl-true-p($one=1) = ~A (expected: T)~%" (pl-true-p $one))
  (format t "  pl-true-p($empty='') = ~A (expected: NIL)~%" (pl-true-p $empty))
  (assert (null (pl-true-p $zero)))
  (assert (pl-true-p $one))
  (assert (null (pl-true-p $empty))))

;;; Test 7: Pass-by-reference example
;; In Perl, sub parameters are boxed with 'my'. Here we simulate that:
;;   sub modify { my $ref = shift; $$ref = $$ref + 100; }
;;   modify(\$x);
(format t "Test 7: Pass-by-reference (sub modifies caller's variable)~%")
(defun test-modify-ref (ref-value)
  "Example sub that modifies a variable via reference.
   The parameter is the raw reference value (a box), not boxed itself.
   In generated code, 'my $ref = shift' would box it."
  ;; ref-value IS the box $x (since we passed \$x which returns the box)
  ;; To modify it, we use box-set to properly invalidate caches
  (box-set ref-value (+ (to-number ref-value) 100)))

(pl-let (($x 5))
  (format t "  Before: $x = ~A~%" (unbox $x))
  ;; pl-backslash returns $x itself (the box)
  (test-modify-ref (pl-backslash $x))
  (format t "  After test-modify-ref(\\$x): $x = ~A (expected: 105)~%" (unbox $x))
  (assert (= (unbox $x) 105)))

;;; Test 8: reftype function
(format t "Test 8: reftype function~%")
(pl-let (($scalar 42))
  (let ((type (pl-reftype $scalar)))
    (format t "  reftype($scalar) = ~A (expected: SCALAR)~%" type)
    (assert (string= type "SCALAR"))))

;;; Test 9: Blessed scalar reference
(format t "Test 9: Blessed scalar reference~%")
(pl-let (($x 0))
  (format t "  Before bless: ref($x) = ~A~%" (pl-reftype $x))
  (assert (string= (pl-reftype $x) "SCALAR"))
  (pl-bless $x "Counter")
  (format t "  After bless: ref($x) = ~A (expected: Counter)~%" (pl-reftype $x))
  (assert (string= (pl-reftype $x) "Counter"))
  ;; Value still works
  (pl-setf $x 10)
  (format t "  Value after bless: $x = ~A (expected: 10)~%" (unbox $x))
  (assert (= (unbox $x) 10)))

;;; Test 10: Blessed hash (common OO pattern)
(format t "Test 10: Blessed hash reference~%")
(let ((obj (pl-bless (pl-hash "name" "Alice" "age" 30) "Person")))
  (format t "  ref($obj) = ~A (expected: Person)~%" (pl-reftype obj))
  (assert (string= (pl-reftype obj) "Person"))
  (format t "  $obj->{name} = ~A~%" (pl-gethash obj "name"))
  (assert (string= (pl-gethash obj "name") "Alice")))

;;; Test 11: Lazy caching - string to number
(format t "Test 11: Lazy caching (string to number)~%")
(pl-let (($s "42"))
  ;; Initially only string is cached
  (format t "  $s = ~S, nv-ok = ~A~%" (unbox $s) (pl-box-nv-ok $s))
  (assert (pl-box-sv-ok $s))      ; string cached
  (assert (null (pl-box-nv-ok $s))) ; number not yet cached
  ;; Use as number - should cache
  (let ((n (to-number $s)))
    (format t "  to-number($s) = ~A, nv-ok = ~A~%" n (pl-box-nv-ok $s))
    (assert (= n 42))
    (assert (pl-box-nv-ok $s))))  ; now number is cached

;;; Test 12: Lazy caching - number to string
(format t "Test 12: Lazy caching (number to string)~%")
(pl-let (($n 3.14))
  ;; Initially only number is cached
  (format t "  $n = ~A, sv-ok = ~A~%" (unbox $n) (pl-box-sv-ok $n))
  (assert (pl-box-nv-ok $n))      ; number cached
  (assert (null (pl-box-sv-ok $n))) ; string not yet cached
  ;; Use as string - should cache
  (let ((s (to-string $n)))
    (format t "  to-string($n) = ~S, sv-ok = ~A~%" s (pl-box-sv-ok $n))
    (assert (pl-box-sv-ok $n))))  ; now string is cached

;;; Test 13: Cache invalidation on assignment
(format t "Test 13: Cache invalidation on assignment~%")
(pl-let (($x "42"))
  ;; Prime both caches
  (to-number $x)
  (to-string $x)
  (assert (pl-box-nv-ok $x))
  (assert (pl-box-sv-ok $x))
  (format t "  Before: nv=~A sv=~S~%" (pl-box-nv $x) (pl-box-sv $x))
  ;; Assignment should invalidate caches
  (pl-setf $x "99")
  (format t "  After $x='99': nv-ok=~A sv-ok=~A~%"
          (pl-box-nv-ok $x) (pl-box-sv-ok $x))
  (assert (pl-box-sv-ok $x))       ; new string value cached
  (assert (null (pl-box-nv-ok $x))) ; number cache invalidated
  ;; New value should work correctly
  (assert (= (to-number $x) 99)))

;;; Test 14: Float parsing from string
(format t "Test 14: Float parsing from string~%")
(pl-let (($f "3.14159"))
  (let ((n (to-number $f)))
    (format t "  to-number('3.14159') = ~A~%" n)
    (assert (< (abs (- n 3.14159)) 0.00001))))

;;; Test 15: Scientific notation parsing
(format t "Test 15: Scientific notation parsing~%")
(pl-let (($e "1e6"))
  (let ((n (to-number $e)))
    (format t "  to-number('1e6') = ~A~%" n)
    (assert (= n 1000000))))

;;; Test 16: Array slices
(format t "Test 16: Array slices~%")
(let ((arr (make-array 5 :initial-contents '(10 20 30 40 50) :adjustable t :fill-pointer 5)))
  (let ((slice (pl-aslice arr 0 2 4)))
    (format t "  @arr[0,2,4] = ~A (expected: (10 30 50))~%" slice)
    (assert (equal slice '(10 30 50)))))

;;; Test 17: Hash slices
(format t "Test 17: Hash slices~%")
(let ((hash (pl-hash "a" 1 "b" 2 "c" 3)))
  (let ((slice (pl-hslice hash "a" "c")))
    (format t "  @hash{a,c} = ~A (expected: (1 3))~%" slice)
    (assert (equal slice '(1 3)))))

;;; Test 18: Compound assignment operators
(format t "Test 18: Compound assignment operators~%")
(pl-let (($x 10))
  (pl-*= $x 3)
  (format t "  $x *= 3: ~A (expected: 30)~%" (unbox $x))
  (assert (= (unbox $x) 30))
  (pl-/= $x 5)
  (format t "  $x /= 5: ~A (expected: 6)~%" (unbox $x))
  (assert (= (unbox $x) 6)))

;;; Test 19: String compound assignment
(format t "Test 19: String compound assignment~%")
(pl-let (($s "Hello"))
  (pl-.= $s " World")
  (format t "  $s .= ' World': ~A (expected: Hello World)~%" (unbox $s))
  (assert (string= (unbox $s) "Hello World")))

;;; Test 20: Logical assignment operators
(format t "Test 20: Logical assignment operators~%")
(pl-let (($x nil))
  (pl-or-assign $x 42)
  (format t "  $x ||= 42 (when nil): ~A (expected: 42)~%" (unbox $x))
  (assert (= (unbox $x) 42)))
(pl-let (($y 10))
  (let ((result (pl-or-assign $y 99)))
    (format t "  $y ||= 99 (when 10): ~A (expected: 10)~%" result)
    (assert (= result 10))))

;;; Test 21: Chained comparison (numeric)
(format t "Test 21: Chained comparison (numeric)~%")
(pl-let (($a 1) ($b 5) ($c 10))
  (let ((result (pl-chain-cmp $a '< $b '< $c)))
    (format t "  1 < 5 < 10 = ~A (expected: T)~%" result)
    (assert result)))
(pl-let (($a 1) ($b 5) ($c 3))
  (let ((result (pl-chain-cmp $a '< $b '< $c)))
    (format t "  1 < 5 < 3 = ~A (expected: NIL)~%" result)
    (assert (null result))))
(pl-let (($a 5) ($b 5) ($c 5))
  (let ((result (pl-chain-cmp $a '<= $b '<= $c)))
    (format t "  5 <= 5 <= 5 = ~A (expected: T)~%" result)
    (assert result)))

;;; Test 22: Chained comparison (string)
(format t "Test 22: Chained comparison (string)~%")
(pl-let (($a "aaa") ($b "bbb") ($c "ccc"))
  (let ((result (pl-chain-cmp $a 'lt $b 'lt $c)))
    (format t "  'aaa' lt 'bbb' lt 'ccc' = ~A (expected: T)~%" result)
    (assert result)))

;;; Test 23: Chained comparison evaluates middle term once
(format t "Test 23: Chained comparison evaluates middle term once~%")
(let ((counter 0))
  (flet ((get-middle ()
           (incf counter)
           5))
    (pl-let (($a 1) ($c 10))
      ;; Create boxed value for middle term
      (let ((mid (make-pl-box (get-middle))))
        (pl-chain-cmp $a '< mid '< $c)
        ;; Counter should still be 1 - middle was created once
        (format t "  middle term evaluated ~A time(s) (expected: 1)~%" counter)
        (assert (= counter 1))))))

(format t "~%=== All tests passed! ===~%")
