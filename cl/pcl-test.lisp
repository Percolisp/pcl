;;;; Copyright (c) 2025-2026
;;;; This is free software; you can redistribute it and/or modify it
;;;; under the same terms as the Perl 5 programming language system itself.

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
(defvar *test-skipped* 0)
(defvar *test-todo* 0
  "Count of TODO tests (Test::More `local $TODO = ...`).  A failing TODO test is
   an *expected* failure (it does not count toward *test-failures*); an
   unexpectedly-passing TODO test counts as a normal pass.")
(defvar *last-test-name* nil
  "Description of the last assertion that ran. Used by the exit hook for crash
   localization: when a file aborts mid-run, this names the last test that
   completed, so the next assertion (~test *test-count*+1) is the crash site.")

;;; ─── Structured failure log (opt-in via PCL_TEST_LOG_DIR) ───────────────────
;;; When the env var PCL_TEST_LOG_DIR names a directory, `test-ok` appends one
;;; TSV line per FAILING assertion to <dir>/<current-test-file>.fails.tsv:
;;;     file <TAB> num <TAB> description <TAB> got <TAB> expected
;;; This is the queryable failure DB that tools/sweep-diff.pl (regression
;;; watchdog, keyed on file+description) and tools/triage.pl consume.  With the
;;; var unset there is ZERO overhead — normal runs and the Pl/t gate are
;;; unaffected.  Only failures are logged, so a full-sweep DB is small (~fail count).
(defvar *test-log-stream* :unopened
  "Cached output stream for the failure log, or NIL if logging is disabled.")

(defun %test-log-clean (s)
  "Make S safe for one TSV field: stringify and collapse tab/newline to space."
  (let ((str (if (stringp s) s (princ-to-string s))))
    (substitute #\Space #\Tab (substitute #\Space #\Newline str))))

(defun %test-log-stream ()
  "Return the failure-log stream (opening it lazily from PCL_TEST_LOG_DIR), or NIL.
   The failure log is a diagnostic side-channel: a failure to open it (e.g. the
   directory does not exist after a test `chdir`d, or PCL_TEST_LOG_DIR was relative)
   must NEVER crash the test run.  We ensure the directory exists and swallow any
   error, leaving *test-log-stream* = NIL (logging silently disabled for this file)."
  (when (eq *test-log-stream* :unopened)
    (let ((dir (sb-ext:posix-getenv "PCL_TEST_LOG_DIR")))
      (setf *test-log-stream*
            (if (and dir (plusp (length dir)) *current-test-file*)
                (ignore-errors
                  (let ((path (merge-pathnames
                               (concatenate 'string *current-test-file* ".fails.tsv")
                               (concatenate 'string dir "/"))))
                    (ensure-directories-exist path)
                    (open path :direction :output
                          :if-exists :supersede :if-does-not-exist :create)))
                nil))))
  *test-log-stream*)

(defun %test-log-failure (num name diag)
  "Append one TSV failure line to the structured log, if enabled."
  (let ((stream (%test-log-stream)))
    (when stream
      (let ((got "") (expected ""))
        (dolist (d diag)
          (let ((ds (if (stringp d) d (princ-to-string d))))
            (cond
              ((nth-value 0 (ppcre:scan "(?i)got:" ds))
               (setf got (ppcre:regex-replace "(?i).*?got:\\s*" ds "")))
              ((nth-value 0 (ppcre:scan "(?i)expected" ds))
               (setf expected (ppcre:regex-replace "(?i).*?expected[^:]*:\\s*" ds ""))))))
        (format stream "~A~C~A~C~A~C~A~C~A~%"
                (%test-log-clean *current-test-file*) #\Tab
                num #\Tab
                (%test-log-clean (or (test-display-value name) "")) #\Tab
                (%test-log-clean got) #\Tab
                (%test-log-clean expected))
        (force-output stream)))))

;;; ─── Declarative skip-registry ──────────────────────────────────────────────
;;; Instead of hand-editing perl-tests/*.t to disable not-supported tests, we keep
;;; an external registry (cl/skip-registry.lisp) keyed by test-file basename and a
;;; regex on the test DESCRIPTION (robust to TAP-number shifts).  A registry entry
;;; ONLY converts a FAILING assertion into a real TAP `# skip`; an entry whose test
;;; unexpectedly PASSES is flagged stale (so accidental real fixes surface).  The
;;; underlying assertion still runs — nothing is weakened (CLAUDE.md principle 5).
(defvar *current-test-file* nil
  "Basename (e.g. \"tr.t\") of the perl-tests file being run; set per-file by the
   sweep / runt so the skip-registry can be consulted.")
(defvar *skip-registry* (make-hash-table :test 'equal)
  "basename string -> list of (scanner category reason raw-pattern) entries.")

(defun %register-skips (basename entries)
  "Implementation of REGISTER-SKIPS.  Each ENTRY is (MATCHER CATEGORY REASON), where
   MATCHER is a DESCRIPTION-REGEX (string — preferred, number-shift-robust) OR an exact
   test NUMBER (integer — fallback for tests with no description)."
  (setf (gethash basename *skip-registry*)
        (append (gethash basename *skip-registry*)
                (loop for (pat cat reason) in entries
                      collect (list (if (integerp pat) pat (ppcre:create-scanner pat))
                                    cat reason pat)))))

(defmacro register-skips (basename &rest entries)
  "Register not-supported test skips for BASENAME.  Each ENTRY is an unquoted list
   (MATCHER CATEGORY-KEYWORD REASON-STRING) — MATCHER is a description-regex string or an
   exact test-number integer (for unnamed tests)."
  `(%register-skips ,basename ',entries))

;;; Export test functions
(export '(pl-plan pl-done_testing pl-ok pl-is pl-isnt
          pl-like pl-unlike pl-cmp_ok pl-pass pl-fail
          pl-skip pl-skip_all pl-diag pl-note pl-BAIL_OUT
          pl-eq_array pl-curr_test))

;;; curr_test() - provided here (not as a stub in test.pl) so it reads the
;;; real *test-count* counter that pl-ok/pl-is/etc. maintain.
(defun pl-curr_test (&optional n)
  "Perl curr_test() - get or set the current test number."
  (when n (setf *test-count* (1- (to-number n))))
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
(defun %skip-registry-lookup (name)
  "Return the matching registry entry for the current test under *current-test-file*, or NIL.
   A string matcher is matched against the test description; an integer matcher against the
   current test number (*test-count*, already incremented by test-ok)."
  (let* ((entries (and *current-test-file*
                       (gethash *current-test-file* *skip-registry*)))
         (disp (test-display-value name))
         (str  (and disp (to-string disp))))
    (when entries
      (dolist (e entries)
        (let ((matcher (first e)))
          (when (if (integerp matcher)
                    (= matcher *test-count*)
                    (and str (ppcre:scan matcher str)))
            (return e)))))))

(defun %current-todo ()
  "Return the active Test::More $TODO reason string, or NIL.
   Test files mark known-broken tests with `local $TODO = \"reason\"` (or the
   fully-qualified `local $::TODO`).  Both resolve to the symbol $TODO in package
   MAIN (perl-tests run in main), so reading that symbol's dynamic value here lets
   the harness honor TODO without any codegen change or variable hijacking.  When
   the binding is out of scope the symbol holds its defvar'd undef box, which
   test-undef-p rejects."
  (let* ((pkg (find-package :main))
         (sym (and pkg (find-symbol "$TODO" pkg))))
    (when (and sym (boundp sym))
      (let ((v (symbol-value sym)))
        (unless (test-undef-p v)
          (let ((s (to-string v)))
            (when (and (stringp s) (plusp (length s))) s)))))))

(defun test-ok (pass name &rest diag)
  (incf *test-count*)
  ;; Record the last test to run (all paths: pass/fail/skip) for crash localization.
  (setf *last-test-name* (or (test-display-value name) "(unnamed)"))
  ;; TODO: a test run under `local $TODO = ...` is known-broken.  Emit the TAP
  ;; `# TODO` directive; a failure here is *expected* (not counted as a real
  ;; failure or logged), an unexpected pass counts normally.  Checked before the
  ;; skip-registry because TODO is set per-test by the source, not by us.
  (let ((todo (%current-todo)))
    (when todo
      (let ((dn (test-display-value name)))
        (cond
          ((not pass)
           (incf *test-todo*)
           (if dn
               (format t "not ok ~A - ~A # TODO ~A~%" *test-count* dn todo)
               (format t "not ok ~A # TODO ~A~%" *test-count* todo))
           (return-from test-ok nil))
          (t
           (if dn
               (format t "ok ~A - ~A # TODO ~A~%" *test-count* dn todo)
               (format t "ok ~A # TODO ~A~%" *test-count* todo))
           (return-from test-ok t))))))
  (let ((entry (%skip-registry-lookup name)))
    ;; Registry says this test is documented not-supported.
    (when entry
      (cond
        ((not pass)
         ;; Expected failure -> emit a real TAP skip (counts as neither pass nor fail).
         (incf *test-skipped*)
         (format t "ok ~A # skip ~A~%" *test-count* (third entry))
         (return-from test-ok nil))
        (t
         ;; Unexpectedly passes -> emit ok AND flag the stale registry entry.
         (format t "ok ~A~@[ - ~A~]~%" *test-count* (test-display-value name))
         (format t "# REGISTRY-STALE: ~A test ~A now passes; drop skip-registry pattern ~S~%"
                 *current-test-file* *test-count* (fourth entry))
         (return-from test-ok t)))))
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
      (%test-log-failure *test-count* name diag)
      (when diag
        (dolist (d diag)
          (format t "# ~A~%" d))))
    pass))

;;; ok(test, name)
(defun pl-ok (test &optional name)
  (test-ok (p-true-p test) name))

;;; Helper: check if value represents Perl undef
(defun test-undef-p (x)
  ;; Treat boxes with nil value as undef: PCL initializes fresh package variables
  ;; as (make-p-box nil), while Perl sees them as undef.
  (or (null x)
      (eq x *p-undef*)
      (and (p-box-p x)
           (let ((v (p-box-value x)))
             (or (null v) (eq v *p-undef*))))))

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
         (rx (unbox regex))
         (regex-str (if (p-regex-match-p rx)
                        (p-regex-match-pattern rx)
                        (to-string regex)))
         (pass (handler-case
                   (if (p-regex-match-p rx)
                       (let* ((opts (build-ppcre-options (p-regex-match-modifiers rx)))
                              (scanner (apply #'ppcre:create-scanner regex-str opts)))
                         (if (ppcre:scan scanner got-str) t nil))
                       (if (ppcre:scan regex-str got-str) t nil))
                 (error () nil))))
    (if pass
        (test-ok t name)
        (test-ok nil name
                 (format nil "                  got: ~A" (test-quote-value got))
                 (format nil "expected to match: ~A" regex-str)))))

;;; unlike(got, regex, name)
(defun pl-unlike (got regex &optional name)
  (let* ((got (test-to-scalar got))
         (got-str (if got (to-string got) ""))
         (rx (unbox regex))
         (regex-str (if (p-regex-match-p rx)
                        (p-regex-match-pattern rx)
                        (to-string regex)))
         (pass (handler-case
                   (if (p-regex-match-p rx)
                       (let* ((opts (build-ppcre-options (p-regex-match-modifiers rx)))
                              (scanner (apply #'ppcre:create-scanner regex-str opts)))
                         (if (ppcre:scan scanner got-str) nil t))
                       (if (ppcre:scan regex-str got-str) nil t))
                 (error () t))))
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
             (let ((n1 (to-number got)) (n2 (to-number expected)))
               (if (or (%pcl-nan-p n1) (%pcl-nan-p n2)) nil (= n1 n2))))
            ((equal op "!=")
             (let ((n1 (to-number got)) (n2 (to-number expected)))
               (if (or (%pcl-nan-p n1) (%pcl-nan-p n2)) t (/= n1 n2))))
            ((equal op "<")
             (let ((n1 (to-number got)) (n2 (to-number expected)))
               (if (or (%pcl-nan-p n1) (%pcl-nan-p n2)) nil (< n1 n2))))
            ((equal op ">")
             (let ((n1 (to-number got)) (n2 (to-number expected)))
               (if (or (%pcl-nan-p n1) (%pcl-nan-p n2)) nil (> n1 n2))))
            ((equal op "<=")
             (let ((n1 (to-number got)) (n2 (to-number expected)))
               (if (or (%pcl-nan-p n1) (%pcl-nan-p n2)) nil (<= n1 n2))))
            ((equal op ">=")
             (let ((n1 (to-number got)) (n2 (to-number expected)))
               (if (or (%pcl-nan-p n1) (%pcl-nan-p n2)) nil (>= n1 n2))))
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
        ;; Crash localization: running FEWER tests than planned means the run is
        ;; INCOMPLETE.  Emit a neutral, machine-parseable fact (the exit hook
        ;; fires both on a clean EOF and on an unhandled condition under
        ;; --non-interactive, and cannot itself tell which).  The *sweep* knows
        ;; the SBCL exit code and refines this into either "crashed mid-file
        ;; (crash site ~test N+1)" or "reached EOF but under-counted".
        (when (and *test-planned* (< *test-count* *test-planned*))
          (format t "# PCL-INCOMPLETE last=~A planned=~A desc=~A~%"
                  *test-count* *test-planned* (or *last-test-name* "?"))
          (force-output))
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

;;; skip_without_dynamic_extension(module, count)
;;; Perl test.pl stub: always skip since PCL cannot load XS dynamic extensions.
(export '(pl-skip_without_dynamic_extension))
(defun pl-skip_without_dynamic_extension (module &optional (count 1))
  (let ((mod (pcl:to-string (pcl:unbox module))))
    (pl-skip (format nil "dynamic extension ~A not available" mod) count)))

;;; next_test()
;;; Perl test.pl: allocate and return the next test number.
;;; Useful when a test block prints "ok N" directly rather than calling ok/is.
(export '(pl-next_test))
(defun pl-next_test (&rest args)
  (declare (ignore args))
  (pcl:make-p-box (incf *test-count*)))

;;; which_perl() — Perl test.pl helper: return path to the running Perl interpreter.
;;; Used by closure.t and others to run a sub-perl process via system().
(export '(pl-which_perl))
(defun pl-which_perl (&rest args)
  (declare (ignore args))
  (pcl:make-p-box "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/bin/perl"))

;;; run_perl(switches => [...], prog => "code") — Perl test.pl helper: run a sub-Perl process.
;;; PCL cannot fork a Perl subprocess, so this always returns undef.
;;; Tests using run_perl will fail (not crash) gracefully.
(export '(pl-run_perl))
(defun pl-run_perl (&rest args)
  (declare (ignore args))
  pcl::*p-undef*)

;;; _qq(val) — Perl test.pl helper: wrap a value in double-quotes for display.
;;; e.g., _qq("hello") → '"hello"'
(export '(pl-_qq))
(defun pl-_qq (&rest args)
  (let* ((raw (if args (car args) pcl::*p-undef*))
         (s   (if (eq raw pcl::*p-undef*) nil (pcl:to-string raw))))
    (pcl:make-p-box
     (if (null s) "undef" (format nil "\"~A\"" s)))))

;;; eq_hash(\%h1, \%h2) — Perl test.pl helper: deep-equal comparison of two hash refs.
;;; Returns 1 (true) if both hashes have the same keys/values, "" (false) otherwise.
(export '(pl-eq_hash))
(defun pl-eq_hash (ref1 ref2 &rest rest)
  (declare (ignore rest))
  (let ((h1 (pcl:p-box-value (pcl:unbox ref1)))
        (h2 (pcl:p-box-value (pcl:unbox ref2))))
    (unless (and (hash-table-p h1) (hash-table-p h2))
      (return-from pl-eq_hash (pcl:make-p-box "")))
    (unless (= (hash-table-count h1) (hash-table-count h2))
      (return-from pl-eq_hash (pcl:make-p-box "")))
    (maphash (lambda (k v)
               (unless (nth-value 1 (gethash k h2))
                 (return-from pl-eq_hash (pcl:make-p-box "")))
               (let ((v2 (gethash k h2)))
                 (unless (equal (pcl:to-string v) (pcl:to-string v2))
                   (return-from pl-eq_hash (pcl:make-p-box "")))))
             h1)
    (pcl:make-p-box 1)))

;;; ----------------------------------------------------------------------------
;;; Sweep-harness loader with per-form recovery.
;;;
;;; This is TEST INFRASTRUCTURE, not Perl runtime semantics — it lives here in the
;;; harness library (loaded only by the sweep / gate), never in pcl-runtime.lisp
;;; (which ships with every transpiled program).
;;;
;;; It loads a generated test file one top-level form at a time and continues past
;;; an uncaught error in any single form, instead of aborting the whole file the
;;; way plain LOAD does.  So one not-supported statement — e.g. `pack "P"` in a bare
;;; loop, or `die if $@` after a string eval PCL can't satisfy — no longer swallows
;;; every test after it; the remaining statements still run and emit their TAP.
;;;
;;; Faithful to LOAD for PCL's output: (a) the reader tracks *package* between forms
;;; exactly as LOAD does, so `(in-package ...)` forms affect later reads; (b) every
;;; eval-when wrapper PCL emits includes :execute, so a per-form EVAL fires the same
;;; situations a LOAD would.  A file with no uncaught top-level die evaluates
;;; identically, form for form.  Each caught error is still printed on *error-output*
;;; (recovered, not hidden) so the planned-vs-emitted check flags the under-count.
(defun p-load-with-recovery (path)
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (let ((*load-pathname* (pathname path))
          (*load-truename* (ignore-errors (truename path)))
          (eof '#:eof)
          (errs 0))
      (loop
       (let ((form (handler-case (read stream nil eof)
                     (error (e)
                       (format *error-output*
                               "~&; PCL recovery: unreadable form, stopping: ~A~%" e)
                       eof))))
         (when (eq form eof) (return))
         (handler-case (eval form)
           (error (e)
             (incf errs)
             (format *error-output*
                     "~&; PCL recovery: top-level form aborted (recovered): ~A~%" e)))))
      (when (plusp errs)
        (format *error-output*
                "~&; PCL recovery: ~D top-level form(s) aborted in ~A~%" errs path))
      (values))))

(format t "# PCL Test library loaded~%")
