;;; pcl-test.lisp - TAP test framework for PCL
;;;
;;; Provides Perl test.pl compatible functions:
;;; plan, done_testing, ok, is, isnt, like, unlike, cmp_ok,
;;; pass, fail, skip, skip_all, diag, note
;;;
;;; Usage: Load after pcl-runtime.lisp, before test code

(in-package :pcl)

;;; Test state
(defvar *test-count* 0)
(defvar *test-planned* nil)
(defvar *test-no-plan* nil)
(defvar *test-failures* 0)

;;; Export test functions
(export '(pl-plan pl-done_testing pl-ok pl-is pl-isnt
          pl-like pl-unlike pl-cmp_ok pl-pass pl-fail
          pl-skip pl-skip_all pl-diag pl-note pl-BAIL_OUT
          pl-eq_array pl-curr_test))

;;; curr_test() - provided here (not as a stub in test.pl) so it reads the
;;; real *test-count* counter that pl-ok/pl-is/etc. maintain.
(defun pl-curr_test ()
  "Perl curr_test() - return the next test number to run."
  (make-p-box (1+ *test-count*)))

;;; Helper: unbox a value for display
(defun test-display-value (x)
  (cond
    ((null x) nil)
    ((p-box-p x) (let ((v (p-box-value x)))
                    (if (eq v *p-undef*) nil (to-string x))))
    ((eq x *p-undef*) nil)
    (t x)))

;;; Helper: format a value for display
(defun test-quote-value (x)
  (let ((v (test-display-value x)))
    (if (null v)
        "undef"
        (format nil "'~A'" v))))

;;; Helper: comment lines
(defun test-comment (&rest args)
  (dolist (line (apply #'p-. args))
    (if (and (> (length (to-string line)) 0)
             (char= (char (to-string line) 0) #\#))
        (format t "~A~%" line)
        (format t "# ~A~%" line))))

;;; plan(N) or plan(tests => N) or plan('no_plan')
(defun pl-plan (&rest args)
  ;; Unbox all args (test scripts pass boxed values)
  (let ((args (mapcar #'unbox args)))
    (cond
      ;; plan(N)
      ((and (= (length args) 1) (numberp (first args)))
       (setf *test-planned* (first args))
       (format t "1..~A~%" *test-planned*))
      ;; plan('no_plan')
      ((and (= (length args) 1) (equal (first args) "no_plan"))
       (setf *test-no-plan* t))
      ;; plan(tests => N)
      ((>= (length args) 2)
       (let ((tests-value nil))
         (loop for i from 0 below (length args) by 2
               for key = (nth i args)
               for val = (nth (1+ i) args)
               do (when (equal key "tests")
                    (setf tests-value val)))
         (when tests-value
           (setf *test-planned* tests-value)
           (format t "1..~A~%" *test-planned*)))))))

;;; done_testing() or done_testing(N)
(defun pl-done_testing (&optional n)
  (let ((count (or n *test-count*)))
    (format t "1..~A~%" count)
    (setf *test-planned* count)))

;;; skip_all(reason)
(defun pl-skip_all (&optional reason)
  (if reason
      (format t "1..0 # Skip ~A~%" reason)
      (format t "1..0~%"))
  (sb-ext:exit :code 0))

;;; BAIL_OUT(reason)
(defun pl-BAIL_OUT (reason)
  (format t "Bail out!  ~A~%" reason)
  (sb-ext:exit :code 255))

;;; Core: _ok(pass, name, @diag)
(defun test-ok (pass name &rest diag)
  (incf *test-count*)
  (let* ((display-name (test-display-value name))
         (out (if display-name
                 (format nil "~A ~A - ~A"
                         (if pass "ok" "not ok")
                         *test-count*
                         display-name)
                 (format nil "~A ~A"
                         (if pass "ok" "not ok")
                         *test-count*))))
    (format t "~A~%" out)
    (unless pass
      (incf *test-failures*)
      (when diag
        (dolist (d diag)
          (format t "# ~A~%" d))))
    pass))

;;; ok(test, name)
(defun pl-ok (test &optional name)
  (test-ok (p-true-p test) name))

;;; Helper: check if value represents Perl undef
(defun test-undef-p (x)
  (or (null x)
      (eq x *p-undef*)
      (and (p-box-p x) (eq (p-box-value x) *p-undef*))))

;;; Helper: apply scalar context to a value (matches Test::More's $$ prototype behavior).
;;; When Test::More functions like is($$;$) receive an array, Perl forces scalar context,
;;; giving the element count. PCL can't enforce prototypes, so we do it here instead.
(defun test-to-scalar (x)
  (handler-case
    (let ((is-vec (and (vectorp x) (not (stringp x)))))
      (if (and is-vec (adjustable-array-p x))
          (make-p-box (length x))
          x))
    (error (e)
      (format t "### test-to-scalar ERROR: ~A~%" e)
      (force-output)
      x)))

;;; is(got, expected, name)
(defun pl-is (got expected &optional name)
  (let* ((got (test-to-scalar got))
         (expected (test-to-scalar expected))
         (pass (cond
                 ((and (test-undef-p got) (test-undef-p expected)) t)
                 ((or (test-undef-p got) (test-undef-p expected)) nil)
                 (t (equal (to-string got) (to-string expected))))))
    (if pass
        (test-ok t name)
        (test-ok nil name
                 (format nil "     got: ~A" (test-quote-value got))
                 (format nil "expected: ~A" (test-quote-value expected))))))

;;; isnt(got, expected, name)
(defun pl-isnt (got expected &optional name)
  (let* ((got (test-to-scalar got))
         (expected (test-to-scalar expected))
         (pass (cond
                 ((and (test-undef-p got) (test-undef-p expected)) nil)
                 ((or (test-undef-p got) (test-undef-p expected)) t)
                 (t (not (equal (to-string got) (to-string expected)))))))
    (if pass
        (test-ok t name)
        (test-ok nil name
                 (format nil "got: ~A" (test-quote-value got))
                 "expected: anything else"))))

;;; like(got, regex, name)
(defun pl-like (got regex &optional name)
  (let* ((got (test-to-scalar got))
         (got-str (if got (to-string got) ""))
         (regex-str (if (p-regex-match-p regex)
                        (p-regex-match-pattern regex)
                        (to-string regex)))
         (pass (if (ppcre:scan regex-str got-str) t nil)))
    (if pass
        (test-ok t name)
        (test-ok nil name
                 (format nil "                  got: ~A" (test-quote-value got))
                 (format nil "expected to match: ~A" regex-str)))))

;;; unlike(got, regex, name)
(defun pl-unlike (got regex &optional name)
  (let* ((got (test-to-scalar got))
         (got-str (if got (to-string got) ""))
         (regex-str (if (p-regex-match-p regex)
                        (p-regex-match-pattern regex)
                        (to-string regex)))
         (pass (if (ppcre:scan regex-str got-str) nil t)))
    (if pass
        (test-ok t name)
        (test-ok nil name
                 (format nil "                      got: ~A" (test-quote-value got))
                 (format nil "expected NOT to match: ~A" regex-str)))))

;;; cmp_ok(got, op, expected, name)
(defun pl-cmp_ok (got op expected &optional name)
  (let* ((got (test-to-scalar got))
         (expected (test-to-scalar expected))
         (pass
          (cond
            ((equal op "==")
             (= (to-number got) (to-number expected)))
            ((equal op "!=")
             (/= (to-number got) (to-number expected)))
            ((equal op "<")
             (< (to-number got) (to-number expected)))
            ((equal op ">")
             (> (to-number got) (to-number expected)))
            ((equal op "<=")
             (<= (to-number got) (to-number expected)))
            ((equal op ">=")
             (>= (to-number got) (to-number expected)))
            ((equal op "eq")
             (equal (to-string got) (to-string expected)))
            ((equal op "ne")
             (not (equal (to-string got) (to-string expected))))
            ((equal op "lt")
             (string< (to-string got) (to-string expected)))
            ((equal op "gt")
             (string> (to-string got) (to-string expected)))
            ((equal op "le")
             (string<= (to-string got) (to-string expected)))
            ((equal op "ge")
             (string>= (to-string got) (to-string expected)))
            (t
             (format t "# Unknown operator '~A'~%" op)
             nil))))
    (if pass
        (test-ok t name)
        (test-ok nil name
                 (format nil "     got: ~A" (test-quote-value got))
                 (format nil "expected: ~A ~A" op (test-quote-value expected))))))

;;; eq_array(\@a, \@b) - compare two array refs for element-wise equality
(defun pl-eq_array (a b)
  (let ((av (if (p-box-p a) (p-box-value a) a))
        (bv (if (p-box-p b) (p-box-value b) b)))
    (let ((av (if (vectorp av) av (make-array 0)))
          (bv (if (vectorp bv) bv (make-array 0))))
      (when (= (length av) (length bv))
        (every (lambda (x y)
                 (equal (to-string x) (to-string y)))
               av bv)))))

;;; pass(name)
(defun pl-pass (&optional name)
  (test-ok t name))

;;; fail(name)
(defun pl-fail (&optional name)
  (test-ok nil name))

;;; skip(reason, count)
;;; Prints skip lines then throws to exit the SKIP: { } labeled block.
;;; This mirrors Perl's Test::More which calls (last SKIP) from inside skip().
(defun pl-skip (reason &optional (count 1))
  (let ((n (truncate (to-number count)))
        (r (to-string (unbox reason))))
    (dotimes (i n)
      (incf *test-count*)
      (format t "ok ~A # skip ~A~%" *test-count* r)))
  (p-last-dynamic "SKIP"))

;;; Helper: split string (must be before pl-diag/pl-note which use it)
(defun split-string (str delims)
  (let ((result nil)
        (start 0))
    (dotimes (i (length str))
      (when (member (char str i) delims)
        (push (subseq str start i) result)
        (setf start (1+ i))))
    (push (subseq str start) result)
    (nreverse result)))

;;; diag(msg)
(defun pl-diag (&rest args)
  (when args
    (dolist (msg args)
      (dolist (line (split-string (to-string msg) '(#\Newline)))
        (format t "# ~A~%" line)))))

;;; note(msg)
(defun pl-note (&rest args)
  (when args
    (dolist (msg args)
      (dolist (line (split-string (to-string msg) '(#\Newline)))
        (format t "# ~A~%" line)))))

;;; END hook: check test count
(push (lambda ()
        (when (and *test-planned* (/= *test-count* *test-planned*))
          (format t "# Looks like you planned ~A tests but ran ~A.~%"
                  *test-planned* *test-count*))
        (when *test-no-plan*
          (format t "1..~A~%" *test-count*)))
      sb-ext:*exit-hooks*)

;;; Stubs for common test-infrastructure functions that may not be loaded yet.
;;; These are typically provided by loc_tools.pl or similar helpers.
;;; Exported from :pcl so user packages that (:use :pcl) get the default.
(export '(pl-locales_enabled))
(defun pl-locales_enabled (&rest args) (declare (ignore args)) 0)

;;; _diag: helper used in some Perl core tests (e.g. index.t) to print
;;; diagnostic info on failure. Defined in Perl's lib/Test/More.pm as
;;; a simple alias for diag(). We stub it here to prevent crashes when
;;; a test fails and calls _diag.
(export '(pl-_diag))
(defun pl-_diag (&rest args)
  (apply #'pl-diag args))

;;; charset_tools.pl stubs — identity functions on non-EBCDIC platforms.
;;; Perl test files require './charset_tools.pl' to get these, but since
;;; BEGIN-block require doesn't reliably define functions at the right time,
;;; we provide them here (test-only; not in the production runtime).
(export '(pl-uni_to_native pl-native_to_uni pl-unicode_to_native pl-native_to_unicode
          pl-byte_utf8a_to_utf8n pl-utf8_to_byte))
(defun pl-uni_to_native (n) (pcl:unbox n))
(defun pl-native_to_uni (n) (pcl:unbox n))
(defun pl-unicode_to_native (n) (pcl:unbox n))
(defun pl-native_to_unicode (n) (pcl:unbox n))
(defun pl-byte_utf8a_to_utf8n (n) (pcl:unbox n))
(defun pl-utf8_to_byte (n) (pcl:unbox n))

(format t "# PCL Test library loaded~%")
