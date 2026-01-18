;;; Test eval { } block behavior
;;; Run with: sbcl --load pcl-runtime.lisp --load test-eval.lisp

(in-package :pcl)

(format t "~%=== Test: eval { } block ===~%~%")

;;; Helper for test output
(defvar *test-count* 0)
(defvar *pass-count* 0)

(defun test-ok (condition desc)
  (incf *test-count*)
  (if condition
      (progn
        (incf *pass-count*)
        (format t "ok ~A - ~A~%" *test-count* desc))
      (format t "NOT ok ~A - ~A~%" *test-count* desc)))

(defun test-equal (got expected desc)
  (test-ok (equal got expected)
           (format nil "~A (got: ~S, expected: ~S)" desc got expected)))

;;; ========================================
(format t "--- eval success cases ---~%")

;; eval { 42 } returns 42, $@ is ""
(let ((result (pl-eval-block 42)))
  (test-equal result 42 "eval { 42 } returns 42")
  (test-equal $@ "" "eval { 42 } sets $@ to empty string"))

;; eval { 1 + 2 } returns 3
(let ((result (pl-eval-block (pl-+ 1 2))))
  (test-equal result 3 "eval { 1 + 2 } returns 3"))

;; eval { } with multiple expressions returns last value
(let ((result (pl-eval-block
                (pl-setf $x 10)
                (pl-+ $x 5))))
  (test-equal result 15 "eval { $x=10; $x+5 } returns 15"))


;;; ========================================
(format t "~%--- eval with die (string) ---~%")

;; eval { die "oops" } returns nil, $@ contains error
(let ((result (pl-eval-block (pl-die "oops"))))
  (test-equal result nil "eval { die 'oops' } returns nil")
  (test-ok (search "oops" $@) "$@ contains 'oops'"))

;; eval { die "error message" } captures full message
(let ((result (pl-eval-block (pl-die "something went wrong"))))
  (test-equal result nil "eval { die } returns nil")
  (test-ok (search "something went wrong" $@)
           "$@ contains full error message"))


;;; ========================================
(format t "~%--- eval with die (object) ---~%")

;; Create a blessed exception object
(let* ((exc-obj (pl-bless (pl-hash "type" "TestException" "msg" "test error")
                          "MyException"))
       (result (pl-eval-block (pl-die exc-obj))))
  (test-equal result nil "eval { die $object } returns nil")
  (test-ok (hash-table-p $@) "$@ is the exception object (hash)")
  (test-equal (gethash "type" $@) "TestException" "$@ object has correct type")
  (test-equal (gethash :__class__ $@) "MyException" "$@ object is blessed"))


;;; ========================================
(format t "~%--- eval with runtime errors ---~%")

;; eval catches division by zero
(let ((result (pl-eval-block (/ 1 0))))
  (test-equal result nil "eval { 1/0 } returns nil")
  (test-ok (> (length $@) 0) "$@ contains error message for div by zero"))

;; eval catches undefined function
(let ((result (pl-eval-block (funcall 'nonexistent-function-xyz))))
  (test-equal result nil "eval catches undefined function")
  (test-ok (> (length $@) 0) "$@ contains error for undefined function"))


;;; ========================================
(format t "~%--- nested eval ---~%")

;; Outer eval succeeds even when inner eval catches error
(let ((result (pl-eval-block
                (pl-eval-block (pl-die "inner error"))
                (if (> (length $@) 0)
                    "caught inner"
                    "no error"))))
  (test-equal result "caught inner" "nested eval: outer succeeds")
  (test-equal $@ "" "nested eval: outer $@ is empty (success)"))

;; Inner eval error doesn't propagate
(let ((outer-result nil)
      (inner-result nil))
  (setf outer-result
        (pl-eval-block
          (setf inner-result (pl-eval-block (pl-die "inner")))
          "outer ok"))
  (test-equal inner-result nil "nested: inner returns nil")
  (test-equal outer-result "outer ok" "nested: outer returns its value"))


;;; ========================================
(format t "~%--- eval clears $@ on success ---~%")

;; First fail, then succeed - $@ should be cleared
(pl-eval-block (pl-die "first error"))
(test-ok (> (length $@) 0) "After failed eval, $@ is set")
(pl-eval-block 42)
(test-equal $@ "" "After successful eval, $@ is cleared")


;;; ========================================
(format t "~%~%=== Results: ~A/~A tests passed ===~%"
        *pass-count* *test-count*)

(if (= *pass-count* *test-count*)
    (format t "All tests passed!~%")
    (format t "FAILURES: ~A tests failed~%" (- *test-count* *pass-count*)))
