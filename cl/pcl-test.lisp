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
          pl-skip pl-skip_all pl-diag pl-note pl-BAIL_OUT))

;;; Helper: format a value for display
(defun test-quote-value (x)
  (if (null x)
      "undef"
      (format nil "'~A'" x)))

;;; Helper: comment lines
(defun test-comment (&rest args)
  (dolist (line (apply #'pl-. args))
    (if (and (> (length (to-string line)) 0)
             (char= (char (to-string line) 0) #\#))
        (format t "~A~%" line)
        (format t "# ~A~%" line))))

;;; plan(N) or plan(tests => N) or plan('no_plan')
(defun pl-plan (&rest args)
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
         (format t "1..~A~%" *test-planned*))))))

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
  (let ((out (if name
                 (format nil "~A ~A - ~A"
                         (if pass "ok" "not ok")
                         *test-count*
                         name)
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
  (test-ok (pl-true-p test) name))

;;; is(got, expected, name)
(defun pl-is (got expected &optional name)
  (let ((pass (cond
                ((and (null got) (null expected)) t)
                ((or (null got) (null expected)) nil)
                (t (equal (to-string got) (to-string expected))))))
    (if pass
        (test-ok t name)
        (test-ok nil name
                 (format nil "     got: ~A" (test-quote-value got))
                 (format nil "expected: ~A" (test-quote-value expected))))))

;;; isnt(got, expected, name)
(defun pl-isnt (got expected &optional name)
  (let ((pass (cond
                ((and (null got) (null expected)) nil)
                ((or (null got) (null expected)) t)
                (t (not (equal (to-string got) (to-string expected)))))))
    (if pass
        (test-ok t name)
        (test-ok nil name
                 (format nil "got: ~A" (test-quote-value got))
                 "expected: anything else"))))

;;; like(got, regex, name)
(defun pl-like (got regex &optional name)
  (let* ((got-str (if got (to-string got) ""))
         (regex-str (to-string regex))
         (pass (if (ppcre:scan regex-str got-str) t nil)))
    (if pass
        (test-ok t name)
        (test-ok nil name
                 (format nil "                  got: ~A" (test-quote-value got))
                 (format nil "expected to match: ~A" regex-str)))))

;;; unlike(got, regex, name)
(defun pl-unlike (got regex &optional name)
  (let* ((got-str (if got (to-string got) ""))
         (regex-str (to-string regex))
         (pass (if (ppcre:scan regex-str got-str) nil t)))
    (if pass
        (test-ok t name)
        (test-ok nil name
                 (format nil "                      got: ~A" (test-quote-value got))
                 (format nil "expected NOT to match: ~A" regex-str)))))

;;; cmp_ok(got, op, expected, name)
(defun pl-cmp_ok (got op expected &optional name)
  (let ((pass
          (cond
            ((equal op "==")
             (and got expected (= (to-number got) (to-number expected))))
            ((equal op "!=")
             (and got expected (/= (to-number got) (to-number expected))))
            ((equal op "<")
             (and got expected (< (to-number got) (to-number expected))))
            ((equal op ">")
             (and got expected (> (to-number got) (to-number expected))))
            ((equal op "<=")
             (and got expected (<= (to-number got) (to-number expected))))
            ((equal op ">=")
             (and got expected (>= (to-number got) (to-number expected))))
            ((equal op "eq")
             (and got expected (equal (to-string got) (to-string expected))))
            ((equal op "ne")
             (and got expected (not (equal (to-string got) (to-string expected)))))
            ((equal op "lt")
             (and got expected (string< (to-string got) (to-string expected))))
            ((equal op "gt")
             (and got expected (string> (to-string got) (to-string expected))))
            ((equal op "le")
             (and got expected (string<= (to-string got) (to-string expected))))
            ((equal op "ge")
             (and got expected (string>= (to-string got) (to-string expected))))
            (t
             (format t "# Unknown operator '~A'~%" op)
             nil))))
    (if pass
        (test-ok t name)
        (test-ok nil name
                 (format nil "     got: ~A" (test-quote-value got))
                 (format nil "expected: ~A ~A" op (test-quote-value expected))))))

;;; pass(name)
(defun pl-pass (&optional name)
  (test-ok t name))

;;; fail(name)
(defun pl-fail (&optional name)
  (test-ok nil name))

;;; skip(reason, count)
(defun pl-skip (reason &optional (count 1))
  (dotimes (i count)
    (incf *test-count*)
    (format t "ok ~A # skip ~A~%" *test-count* reason)))

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

;;; Helper: split string
(defun split-string (str delims)
  (let ((result nil)
        (start 0))
    (dotimes (i (length str))
      (when (member (char str i) delims)
        (push (subseq str start i) result)
        (setf start (1+ i))))
    (push (subseq str start) result)
    (nreverse result)))

;;; END hook: check test count
(push (lambda ()
        (when (and *test-planned* (/= *test-count* *test-planned*))
          (format t "# Looks like you planned ~A tests but ran ~A.~%"
                  *test-planned* *test-count*))
        (when *test-no-plan*
          (format t "1..~A~%" *test-count*)))
      sb-ext:*exit-hooks*)

(format t "# PCL Test library loaded~%")
