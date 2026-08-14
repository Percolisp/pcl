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
;; PID that set the plan: a fork child inherits the exit hook, and must not
;; run the parent's plan-count check when it exits (perl's Test::Builder
;; keeps $$ at plan time and skips _ending in children the same way).
(defvar *test-plan-pid* nil)
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
          pl-eq_array pl-curr_test
          pl-is_deeply pl-use_ok pl-require_ok pl-isa_ok pl-can_ok pl-explain))

;;; Register the TAP functions as DEFINED subs so Perl-level `defined &is`
;;; introspection reports them — test.pl's real-perl fallback (used when a
;;; test fork+execs a real perl child that requires './test.pl') keys on
;;; `unless (defined &main::is)` to decide whether to eval its own bodies.
(dolist (s '(pl-plan pl-done_testing pl-ok pl-is pl-isnt
             pl-like pl-unlike pl-cmp_ok pl-pass pl-fail
             pl-skip pl-skip_all pl-diag pl-note pl-BAIL_OUT
             pl-eq_array pl-curr_test
             pl-is_deeply pl-use_ok pl-require_ok pl-isa_ok pl-can_ok pl-explain))
  (setf (gethash s *p-declared-subs*) :defined))

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
    ((stringp x) x)
    ((p-box-p x) (let ((v (p-box-value x)))
                   (if (eq v *p-undef*) nil (to-string x))))
    ((eq x *p-undef*) nil)
    ;; A RAW host value (a bare function, hash-table, struct, …) reaches here
    ;; whenever an argument arrives unboxed — is_deeply(sub{}, sub{}) is the
    ;; live case.  Show it the way PERL sees it (CODE(0x…)), not the way SBCL
    ;; prints it: the old `(t x)` arm published `#<function (lambda …) {B80…}>`
    ;; into a TAP diagnostic.
    (t (to-string x))))

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

;;; skip_all(reason)
;;; Defined BEFORE pl-plan: `plan(skip_all => …)` calls it (perl's t/test.pl
;;; does the same), and a forward reference would be a load-time style-warning.
(defun pl-skip_all (&optional reason)
  (if reason
      (format t "1..0 # Skip ~A~%" reason)
      (format t "1..0~%"))
  (sb-ext:exit :code 0))

;;; plan(N) or plan(tests => N) or plan(skip_all => REASON) or plan('no_plan')
(defun pl-plan (&rest args)
  (setf *test-plan-pid* (sb-posix:getpid))
  ;; FLATTEN first, then unbox.  `plan` is a perl SUB in t/test.pl, so its
  ;; argument list flattens like any other: `plan reverse 9` hands perl ONE
  ;; argument, 9.  This is a CL function, so the call site passes the value of
  ;; `(p-reverse 9)` — a one-element vector — straight through, and the form
  ;; dispatch below (which knows a bare count, `tests => N` and `skip_all =>
  ;; …`) had no case for it and DIED, so t/op/select.t produced no TAP at all
  ;; (task #317).  p-flatten-args is the same spreading a p-sub does to build
  ;; @_, so there is no second flattening rule here (rule 11).
  (let ((args (mapcar #'unbox (coerce (p-flatten-args args) 'list))))
    (cond
      ;; plan(N)
      ((and (= (length args) 1) (numberp (first args)))
       (setf *test-planned* (first args))
       (format t "1..~A~%" *test-planned*))
      ;; plan('no_plan')
      ((and (= (length args) 1) (equal (first args) "no_plan"))
       (setf *test-no-plan* t))
      ;; plan(tests => N) / plan(skip_all => REASON)
      ;; perl's t/test.pl: `my %plan = @_; $plan{skip_all} and
      ;; skip_all($plan{skip_all}); $n = $plan{tests};` — skip_all is checked
      ;; FIRST and exits, so the rest of the file never runs.  Without it the
      ;; feature-detection idiom
      ;;     defined &Internals::getcwd or plan skip_all => "no ...";
      ;; fell through and the next line called the missing sub (io/getcwd.t
      ;; reported NOTAP `undef-fn:Internals::pl-getcwd` — a crash where perl
      ;; cleanly skips).  Seven t/ files use this form.
      ((>= (length args) 2)
       (let ((tests-value nil) (skip-reason nil))
         (loop for i from 0 below (length args) by 2
               for key = (nth i args)
               for val = (nth (1+ i) args)
               do (cond ((equal key "tests")    (setf tests-value val))
                        ((equal key "skip_all") (setf skip-reason val))))
         (when skip-reason (pl-skip_all skip-reason))   ; exits
         (unless (or tests-value skip-reason)
           (error "plan(): no `tests` or `skip_all` key in ~S" args))
         (setf *test-planned* tests-value)
         (format t "1..~A~%" *test-planned*)))
      ;; No recognized form.  The plan is the count the whole file is judged
      ;; against, so a silent fall-through here means the run publishes TAP
      ;; nobody can check — exactly the shape task #202 exists to remove.
      (t (error "plan(): unrecognized plan form ~S" args)))))

;;; done_testing() or done_testing(N)
(defun pl-done_testing (&optional n)
  (let ((count (or n *test-count*)))
    (format t "1..~A~%" count)
    (setf *test-planned* count)))

;;; `use Test::More tests => N` — the IMPORT-ARG spelling of the plan.
;;;
;;; Test::More's import IS its plan setter: Test::Builder::Module::import
;;; strips an `import => [...]` export list and hands EVERYTHING ELSE to
;;; plan().  So `use Test::More tests => 23`, `use Test::More 'no_plan'` and
;;; `use Test::More skip_all => "why"` mean exactly the corresponding plan()
;;; call.  PCL used to drop the whole import list (p-use returns as soon as it
;;; recognises a PCL-provided module), so a file that spelled its plan this way
;;; published a TAP stream with no plan line AND no done_testing — one no
;;; harness can judge.  That is the #202 family ("a claim that cannot be
;;; evaluated must say so"), and it hid from our own measurements only because
;;; sweep-perl-tests.pl counts rows itself instead of reading the plan.
;;; Task #275.
;;;
;;; The export list is consumed and ignored: PCL exports the whole TAP API
;;; unconditionally, and letting `import` reach pl-plan would look like an
;;; unknown plan key there.  Anything else unrecognised is pl-plan's to reject
;;; — perl's Test::Builder croaks "'X' is not a valid plan" on it too.
(defun %test-import (import-args)
  "Apply a Test::More import list as a plan.  IMPORT-ARGS is whatever p-use
   was handed: a vector (`use Test::More tests => 2`), a bare string
   (`use Test::More 'no_plan'`), or NIL/empty for a plain `use Test::More`."
  (let ((vals (mapcar #'unbox
                      (cond ((null import-args) nil)
                            ((stringp import-args) (list import-args))
                            ((and (vectorp import-args)
                                  (not (stringp import-args)))
                             (coerce import-args 'list))
                            ((listp import-args) import-args)
                            (t (list import-args)))))
        (plan-args '()))
    (loop while vals
          for a = (pop vals)
          do (if (and (stringp a) (string= a "import"))
                 (pop vals)                       ; the export list — ignored
                 (push a plan-args)))
    (let ((plan-args (nreverse plan-args)))
      (when plan-args
        (apply #'pl-plan plan-args)))))

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
   main (perl-tests run in main), so reading that symbol's dynamic value here lets
   the harness honor TODO without any codegen change or variable hijacking.
   Generated code is read under (readtable-case :invert), so the interned
   symbol's NAME is the inverted \"$todo\" -- look that up via the same
   %pcl-invert-case transform every runtime name-builder uses (the literal
   \"$TODO\" fallback keeps this correct if the readtable ever reverts).
   When the binding is out of scope the symbol holds its defvar'd undef box,
   which test-undef-p rejects."
  (let* ((pkg (find-package :main))
         (sym (and pkg (or (find-symbol (%pcl-invert-case "$TODO") pkg)
                           (find-symbol "$TODO" pkg)))))
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

;;; ─── The one regex matcher behind like / unlike / cmp_ok '=~' ───────────────
;;; %pcl-create-scanner, NOT raw ppcre:create-scanner — the real match path
;;; carries workarounds a second scanner misses: cl-ppcre's unrestored
;;; extended-mode after an inline (?x:) group, and /xx (task #179), a
;;; PCL-private option cl-ppcre would choke on.  Building a second scanner
;;; here meant `like`/`unlike` judged patterns by different rules than `=~`
;;; did, silently, inside the harness that measures the whole suite.
;;;
;;; A pattern may also arrive as a plain STRING: perl's t/test.pl passes
;;; strings and interpolates them as patterns (`$got =~ /$expected/`), so the
;;; harness must too.  (Test::More instead FAILS a non-qr pattern with
;;; "doesn't look much like a regex to me"; PCL serves both callers from one
;;; entry point and follows test.pl, which is the stricter requirement — see
;;; docs/tap-assertion-audit.md §like-string-patterns.)
(defun %test-regex-pattern-text (regex)
  "The pattern text of REGEX (a qr// match object or a plain pattern string)."
  (let ((rx (unbox regex)))
    (if (p-regex-match-p rx) (p-regex-match-pattern rx) (to-string regex))))

(defun %test-regex-match-p (got-str regex)
  "T/NIL: does GOT-STR match REGEX?  SIGNALS whatever the scanner signals —
   the CALLER must decide what an unusable pattern means, because silently
   turning a scanner error into a verdict is how `unlike` became an assertion
   that could not fail (task #202)."
  (let ((rx (unbox regex)))
    (if (p-regex-match-p rx)
        (let ((scanner (%pcl-create-scanner
                        (p-regex-match-pattern rx)
                        (build-ppcre-options (p-regex-match-modifiers rx)))))
          (if (ppcre:scan scanner got-str) t nil))
        (if (ppcre:scan (to-string regex) got-str) t nil))))

(defun %test-like (got regex name negated)
  "Body of like (NEGATED nil) and unlike (NEGATED t) — one matcher, one
   error policy: an unusable pattern is NOT a verdict in either direction."
  (let* ((got (test-to-scalar got))
         (got-str (if got (to-string got) ""))
         (pattern (%test-regex-pattern-text regex))
         (err nil)
         (matched (handler-case (%test-regex-match-p got-str regex)
                    (error (e) (setf err e) nil))))
    (cond
      ;; The claim could not be CHECKED.  Report `not ok` naming the reason:
      ;; never a pass (`unlike`'s old (error () t) arm made every typo'd
      ;; pattern an unfalsifiable assertion), and never a die (which would
      ;; cost the file every row after this one).
      (err (test-ok nil name
                    (format nil "     got: ~A" (test-quote-value got))
                    (format nil "expected: usable regex ~A" (test-quote-value pattern))
                    (format nil "    Unusable regex: ~A" err)))
      ((if negated (not matched) matched) (test-ok t name))
      (negated (test-ok nil name
                        (format nil "                      got: ~A" (test-quote-value got))
                        (format nil "expected NOT to match: ~A" pattern)))
      (t (test-ok nil name
                  (format nil "                  got: ~A" (test-quote-value got))
                  (format nil "expected to match: ~A" pattern))))))

;;; like(got, regex, name)
(defun pl-like (got regex &optional name)
  (%test-like got regex name nil))

;;; unlike(got, regex, name)
(defun pl-unlike (got regex &optional name)
  (%test-like got regex name t))

;;; cmp_ok(got, op, expected, name)
;;;
;;; Test::More evaluates `$got $op $expect` in a string eval, so its operator
;;; set is whatever perl parses.  Ours is a dispatch, i.e. a CLOSED set — and
;;; a closed set needs a loud last arm (CLAUDE.md rule 12).  The old `(t nil)`
;;; arm printed a comment and then reported a FAILURE, so `cmp_ok(1,'<=>',2)`
;;; and `cmp_ok($s,'=~',qr/x/)` — both perfectly ordinary perl — were published
;;; as test failures.  Those four operators are implemented below; anything
;;; still unhandled reports `not ok` NAMING the operator, which is loud, cannot
;;; be mistaken for a pass, and keeps the rest of the file's rows alive.
(defun %test-num-compare (got expected fn)
  "FN applied to the numifications of GOT and EXPECTED, with perl's NaN rule:
   a NaN operand makes every ORDERED comparison false (!= is its own case)."
  (let ((n1 (to-number got)) (n2 (to-number expected)))
    (if (or (%pcl-nan-p n1) (%pcl-nan-p n2)) nil (funcall fn n1 n2))))

(defun %test-cmp-ok (got op expected)
  "The verdict of `GOT OP EXPECTED`.  Second value NIL means the verdict is
   real; a STRING means the claim could not be evaluated and names why."
  (cond
    ((equal op "==")  (%test-num-compare got expected #'=))
    ((equal op "!=")  (let ((n1 (to-number got)) (n2 (to-number expected)))
                        (if (or (%pcl-nan-p n1) (%pcl-nan-p n2)) t (/= n1 n2))))
    ((equal op "<")   (%test-num-compare got expected #'<))
    ((equal op ">")   (%test-num-compare got expected #'>))
    ((equal op "<=")  (%test-num-compare got expected #'<=))
    ((equal op ">=")  (%test-num-compare got expected #'>=))
    ;; <=> and cmp yield -1/0/1, so the TRUTH of the comparison is "unequal";
    ;; <=> on a NaN operand yields undef, which is false.
    ((equal op "<=>") (let ((n1 (to-number got)) (n2 (to-number expected)))
                        (if (or (%pcl-nan-p n1) (%pcl-nan-p n2)) nil (/= n1 n2))))
    ((equal op "cmp") (not (equal (to-string got) (to-string expected))))
    ((equal op "eq")  (equal (to-string got) (to-string expected)))
    ((equal op "ne")  (not (equal (to-string got) (to-string expected))))
    ((equal op "lt")  (and (string< (to-string got) (to-string expected)) t))
    ((equal op "gt")  (and (string> (to-string got) (to-string expected)) t))
    ((equal op "le")  (and (string<= (to-string got) (to-string expected)) t))
    ((equal op "ge")  (and (string>= (to-string got) (to-string expected)) t))
    ((or (equal op "=~") (equal op "!~"))
     (handler-case (let ((m (%test-regex-match-p (to-string got) expected)))
                     (if (equal op "=~") m (not m)))
       (error (e) (values nil (format nil "Unusable regex: ~A" e)))))
    (t (values nil (format nil "cmp_ok() cannot evaluate the operator '~A'" op)))))

(defun pl-cmp_ok (got op expected &optional name)
  (let* ((got (test-to-scalar got))
         (expected (test-to-scalar expected))
         (op (to-string (unbox op))))
    (multiple-value-bind (pass problem) (%test-cmp-ok got op expected)
      (if (and pass (not problem))
          (test-ok t name)
          (apply #'test-ok nil name
                 (format nil "     got: ~A" (test-quote-value got))
                 (format nil "expected: ~A ~A" op (test-quote-value expected))
                 (when problem (list (format nil "    ~A" problem))))))))

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

;;; ----- Test::More structural / module asserts (for CPAN module test suites) -----

;;; Recursive deep comparison for is_deeply: unwrap boxes at each level, compare
;;; arrayrefs elementwise, hashrefs key-by-key, scalars by stringification (with
;;; undef handling).  Covers the common ref shapes Test::More::is_deeply is used on.
(defun test-deeply-equal (a b)
  (let ((a (if (p-box-p a) (p-box-value a) a))
        (b (if (p-box-p b) (p-box-value b) b)))
    (cond
      ((and (test-undef-p a) (test-undef-p b)) t)
      ((or (test-undef-p a) (test-undef-p b)) nil)
      ((and (vectorp a) (not (stringp a)) (vectorp b) (not (stringp b)))
       (and (= (length a) (length b))
            (every #'test-deeply-equal a b)))
      ((or (and (vectorp a) (not (stringp a)))
           (and (vectorp b) (not (stringp b)))) nil)
      ((and (hash-table-p a) (hash-table-p b))
       (and (= (hash-table-count a) (hash-table-count b))
            (block cmp
              (maphash (lambda (k v)
                         (multiple-value-bind (bv found) (gethash k b)
                           (unless (and found (test-deeply-equal v bv))
                             (return-from cmp nil))))
                       a)
              t)))
      ((or (hash-table-p a) (hash-table-p b)) nil)
      (t (equal (to-string a) (to-string b))))))

;;; is_deeply(got, expected, name)
(defun pl-is_deeply (got expected &optional name)
  (if (test-deeply-equal got expected)
      (test-ok t name)
      (test-ok nil name
               (format nil "     got: ~A" (test-quote-value got))
               (format nil "expected: ~A" (test-quote-value expected)))))

;;; use_ok(module, ...) / require_ok(module) — these must ACTUALLY load the
;;; module; nothing else in the pipeline does.  The transpiler resolves a
;;; LITERAL `use Foo` at compile time, but `use_ok('Foo')` is an ordinary
;;; funcall it never sees (nothing under Pl/ mentions either name), so the old
;;; "already loaded, report success" shortcut was a silent lie: every
;;; require_ok/use_ok in every suite passed while the module stayed unloaded,
;;; and each later row that used it failed with no visible cause.  Found via
;;; Capture-Tiny's 01-Capture-Tiny.t, where require_ok said ok and the eight
;;; can_ok rows behind it all said "method(s) not found" (task #199).
(defun %test-load-module (name &key import-args (do-import t))
  "Load NAME as `use`/`require` would.  A path-ish NAME ('t/lib/Foo.pm') goes
   through p-require-file, a bareword through p-use — matching Test::More,
   which accepts both."
  (if (or (find #\/ name) (and (> (length name) 3)
                               (string= ".pm" name :start2 (- (length name) 3))))
      (p-require-file name)
      (p-use name :import-args import-args :do-import do-import
             :into *pcl-current-package*)))

(defun pl-use_ok (module &rest args)
  (let* ((name (to-string (unbox module)))
         (vals (mapcar #'unbox args))
         ;; Test::More: a lone numeric argument is a VERSION, not an import
         ;; list — `use_ok('Foo', 1.23)` means `use Foo 1.23;`, which imports
         ;; the defaults.  PCL does not verify module versions anywhere, so
         ;; this only picks the right import shape.
         (version-p (and (= (length vals) 1)
                         (let ((s (to-string (first vals))))
                           (and (plusp (length s))
                                (every (lambda (c) (or (digit-char-p c) (char= c #\.)))
                                       s)))))
         ;; Test::More's description is "use $module;" for EVERY form — the
         ;; import list and the version never appear in it.  Descriptions are
         ;; join keys here (skip-registry, tools/sweep-diff.pl), so a prettier
         ;; text is a different row (ruled s329, fable-answers-s328.md §3).
         (desc (format nil "use ~A;" name)))
    (handler-case
        (progn (%test-load-module name
                                  :import-args (if (or version-p (null vals))
                                                   :default
                                                   (coerce vals 'vector)))
               (test-ok t desc))
      (error (e)
        (test-ok nil desc
                 (format nil "    Tried to use '~A'." name)
                 (format nil "    Error:  ~A" e))))))

(defun pl-require_ok (module)
  (let* ((name (to-string (unbox module)))
         (desc (format nil "require ~A;" name)))
    (handler-case
        (progn (%test-load-module name :do-import nil) (test-ok t desc))
      (error (e)
        (test-ok nil desc
                 (format nil "    Tried to require '~A'." name)
                 (format nil "    Error:  ~A" e))))))

;;; isa_ok(object, class, [name])
;;;
;;; The DEFAULT description is Test::More's, which names what the thing is —
;;; "An object of class 'Foo' isa 'Bar'" / "A reference of type 'ARRAY' isa …"
;;; / "The class (or class-like) 'Foo' isa …" / "undef isa …".  PCL used to
;;; print "The object isa Bar" for all four, which is both wrong text and, more
;;; importantly, the same KEY for four different assertions (descriptions are
;;; join keys for the skip-registry and tools/sweep-diff.pl).
(defun %test-thing-kind (thing)
  "Test::More's four-way classification of an isa_ok/can_ok invocant."
  (cond ((test-undef-p thing) :undef)
        ((zerop (length (to-string (p-ref thing)))) :class)
        ((p-get-class thing) :object)
        (t :reference)))

(defun %test-thing-name (thing)
  "How Test::More names THING in an isa_ok description."
  (let ((kind (%test-thing-kind thing)))
    (case kind
      (:undef     "undef")
      (:object    (format nil "An object of class '~A'" (to-string (p-ref thing))))
      (:reference (format nil "A reference of type '~A'" (to-string (p-ref thing))))
      (:class     (format nil "The class (or class-like) '~A'" (to-string thing)))
      (t (error "%test-thing-name: unhandled thing kind ~S" kind)))))

(defun %test-isa-diag (thing class)
  "Test::More's failure diagnostic for isa_ok."
  (let ((kind (%test-thing-kind thing)))
    (case kind
      (:undef     "    undef isn't defined")
      (:object    (format nil "    The object of class '~A' isn't a '~A'"
                          (to-string (p-ref thing)) class))
      (:reference (format nil "    The reference of type '~A' isn't a '~A'"
                          (to-string (p-ref thing)) class))
      (:class     (format nil "    The class (or class-like) '~A' isn't a '~A'"
                          (to-string thing) class))
      (t (error "%test-isa-diag: unhandled thing kind ~S" kind)))))

(defun pl-isa_ok (object class &optional name)
  (let* ((cls (to-string (unbox class)))
         (nm  (or (test-display-value name)
                  (format nil "~A isa '~A'" (%test-thing-name object) cls))))
    (if (p-true-p (p-isa object cls))
        (test-ok t nm)
        (test-ok nil nm (%test-isa-diag object cls)))))

;;; can_ok(object, methods...)
;;; Test::More: the class is `ref $proto || $proto`, the description names the
;;; single method when there is exactly one, and the two degenerate calls —
;;; empty invocant, no methods — are FAILURES with their own diagnostics
;;; (`can_ok('Foo')` used to emit "->can(...)" with an empty "method(s) not
;;; found:" list, which named neither the class nor the mistake).
(defun %test-can-ok-class (object)
  (let ((r (to-string (p-ref object))))
    (if (plusp (length r)) r (to-string (unbox object)))))

(defun pl-can_ok (object &rest methods)
  (let ((names (mapcar (lambda (m) (to-string (unbox m))) methods))
        (cls   (%test-can-ok-class object)))
    (cond
      ((zerop (length cls))
       (test-ok nil "->can(...)" "    can_ok() called with empty class or reference"))
      ((null names)
       (test-ok nil (format nil "~A->can(...)" cls)
                "    can_ok() called with no methods"))
      (t
       (let ((missing (remove-if (lambda (m) (p-true-p (p-can object m))) names))
             (desc (if (= 1 (length names))
                       (format nil "~A->can('~A')" cls (first names))
                       (format nil "~A->can(...)" cls))))
         (if (null missing)
             (test-ok t desc)
             (apply #'test-ok nil desc
                    (mapcar (lambda (m) (format nil "    ~A->can('~A') failed" cls m))
                            missing))))))))

;;; explain(...) — Test::More returns a Data::Dumper rendering for note/diag.
;;;
;;; Test::More::explain passes a NON-ref through unchanged and renders every
;;; REF with Data::Dumper under Indent(1), Terse(1), Sortkeys(1).  PCL used to
;;; stringify instead, so every is_deeply failure that printed its operands
;;; read `got 'ARRAY(0x53)'` — the value was there and the diagnosis was not
;;; (task #236: ~40 CPAN-board rows had no usable cause line).
;;;
;;; The shape test is the one test-deeply-equal already uses — unbox one level,
;;; then vector = array ref, hash-table = hash ref, box = scalar ref — so
;;; anything is_deeply can WALK, explain can PRINT.
;;;
;;; Layout, key order, quoting and the bless/backslash/sub forms are the live
;;; `perl` answers (probed s374).  Two deliberate differences from Dumper:
;;;   * no trailing newline — pl-diag splits on newline and would print a bare
;;;     `# ` line after every dump;
;;;   * an integer prints bare and everything else quoted ('1.5', and '10' for
;;;     the string "10").  Dumper's XS reads the SV's flags; PCL's CL
;;;     integer-vs-float-vs-string types carry the same distinction.
;;; A shape with no Dumper form here (a glob ref) falls back to its
;;; stringification — the old behaviour for everything, and still no worse.

(defvar *dumper-seen* nil
  "Alist of ref identity -> the $VAR1 path it was first printed at, so a cycle
   or a shared ref prints the back-reference Dumper prints instead of looping.")

(defun %dumper-quote (s)
  "Dumper's _quote: single-quoted, with ' and \\ escaped."
  (with-output-to-string (out)
    (write-char #\' out)
    (loop for c across s
          do (when (or (char= c #\') (char= c #\\)) (write-char #\\ out))
          (write-char c out))
    (write-char #\' out)))

(defun %dumper-pad (level)
  (make-string (* 2 level) :initial-element #\Space))

(defun %dumper-atom (v)
  "A non-ref leaf: undef, a bare integer, or a quoted string."
  (cond ((or (null v) (eq v *p-undef*)) "undef")
        ((integerp v) (princ-to-string v))
        (t (%dumper-quote (to-string v)))))

(defun %dumper-ref-p (inner)
  "Is INNER (one unbox down) a shape this renderer dumps structurally?"
  (or (and (vectorp inner) (not (stringp inner)))
      (hash-table-p inner)
      (functionp inner)
      (p-box-p inner)))

(defun %dumper-class (v)
  "The bless class of V, or NIL when V is a plain (unblessed) ref."
  (let ((r (p-ref v)))
    (and (stringp r) (plusp (length r))
         (let ((k (p-reftype v)))
           (and (stringp k) (not (string= r k)) r)))))

(defun %dumper-array (vec level path)
  (if (zerop (length vec))
      "[]"
      (format nil "[~%~{~A~^,~%~}~%~A]"
              (loop for i from 0 below (length vec)
                    collect (concatenate 'string (%dumper-pad (1+ level))
                                         (%dumper-value (aref vec i) (1+ level)
                                                        (format nil "~A->[~D]" path i))))
              (%dumper-pad level))))

(defun %dumper-hash (h level path)
  ;; :__class__ is where a blessed hash stores its class — Dumper prints that
  ;; as the bless() wrapper, never as a key.
  (let ((pairs (sort (loop for k being the hash-keys of h
                           unless (eq k :__class__)
                           collect (cons (to-string k) k))
                     #'string< :key #'car)))
    (if (null pairs)
        "{}"
        (format nil "{~%~{~A~^,~%~}~%~A}"
                (loop for pair in pairs
                      collect (format nil "~A~A => ~A"
                                      (%dumper-pad (1+ level))
                                      (%dumper-quote (car pair))
                                      (%dumper-value (gethash (cdr pair) h) (1+ level)
                                                     (format nil "~A->{~A}" path
                                                             (%dumper-quote (car pair))))))
                (%dumper-pad level)))))

(defun %dumper-body (inner level path)
  "The unblessed rendering of a ref shape."
  (cond ((functionp inner) "sub { \"DUMMY\" }")
        ((and (vectorp inner) (not (stringp inner))) (%dumper-array inner level path))
        ((hash-table-p inner) (%dumper-hash inner level path))
        ((p-box-p inner)
         (concatenate 'string "\\"
                      (%dumper-value inner level (format nil "${~A}" path))))
        (t (%dumper-atom inner))))

(defun %dumper-value (v level path)
  (let* ((inner (if (p-box-p v) (p-box-value v) v))
         (seen  (and (%dumper-ref-p inner) (assoc inner *dumper-seen* :test #'eq))))
    (cond
      (seen (cdr seen))
      ((not (%dumper-ref-p inner)) (%dumper-atom inner))
      (t (push (cons inner path) *dumper-seen*)
         (let ((body  (%dumper-body inner level path))
               (class (%dumper-class v)))
           (if class
               (format nil "bless( ~A, ~A )" body (%dumper-quote class))
               body))))))

(defun pl-explain (&rest args)
  (format nil "~{~A~^~%~}"
          (mapcar (lambda (a)
                    (let ((inner (if (p-box-p a) (p-box-value a) a)))
                      (if (%dumper-ref-p inner)
                          (let ((*dumper-seen* nil)) (%dumper-value a 0 "$VAR1"))
                          (to-string a))))
                  args)))

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

;;; END hook: check test count.  Skipped entirely in a fork child (pid
;;; differs from plan time) — the parent owns the plan.
(push (lambda ()
        (unless (and *test-plan-pid*
                     (/= *test-plan-pid* (sb-posix:getpid)))
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
            (format t "1..~A~%" *test-count*))))
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
;;; `unicode_to_native` and `native_to_unicode` are NOT here: they are real
;;; utf8:: builtins, defined once in cl/pcl-runtime.lisp.  Defining them again
;;; here redefined the same :pcl symbol — the TAP layer loads after the runtime,
;;; so this copy silently won and the function behaved differently depending on
;;; whether Test::More was loaded, with a "redefining …" warning on stderr for
;;; 17 sweep files.  Only the four names with no runtime twin belong here.
(export '(pl-uni_to_native pl-native_to_uni
          pl-byte_utf8a_to_utf8n pl-utf8_to_byte))
(defun pl-uni_to_native (n) (pcl:unbox n))
(defun pl-native_to_uni (n) (pcl:unbox n))
(defun pl-byte_utf8a_to_utf8n (n) (pcl:unbox n))
(defun pl-utf8_to_byte (n) (pcl:unbox n))

;;; skip_without_dynamic_extension(module, count)
;;; Perl's t/test.pl asks %Config whether the extension was BUILT and skips
;;; only then.  PCL used to skip unconditionally — a claim about the
;;; environment it never checked, and a false one for any module PCL can in
;;; fact load (`IO` resolves to lib/IO.pm, so readline.t's four rows were
;;; skipped on a false premise while Devel::Peek's two really are missing).
;;; Ask the loader, which is our %Config.
(export '(pl-skip_without_dynamic_extension))
(defun pl-skip_without_dynamic_extension (module &optional (count 1))
  (let* ((mod (pcl:to-string (pcl:unbox module)))
         ;; The probe is deliberate, so its FAILURE is not news: a missing
         ;; module's load banner on *error-output* would land in the middle of
         ;; the TAP stream (the sweep folds stderr into stdout), splitting a
         ;; row in half and costing the file its clean status.  Measured on
         ;; undef.t, which went PASS 35/35 -> PARTIAL 30/35 on the noise alone.
         (available (handler-case
                        (let ((*error-output* (make-broadcast-stream)))
                          (%test-load-module mod :do-import nil)
                          t)
                      (error () nil))))
    (unless available
      (pl-skip (format nil "~A was not built" mod) count))))

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

;;; unlink_all(@files) — Perl test.pl helper: delete the named files, returning
;;; the count that are gone afterwards (t/test.pl counts an already-absent file
;;; too).  A standard cleanup helper across t/op and t/io; defined here in the
;;; harness lib (not pcl-runtime.lisp) because it is test infrastructure.
(export '(pl-unlink_all))
(defun pl-unlink_all (&rest files)
  (let ((count 0))
    (dolist (f files)
      (let ((path (pcl:to-string (pcl:unbox f))))
        (when (> (length path) 0)
          (ignore-errors (delete-file path))
          (unless (probe-file path)
            (incf count)))))
    (pcl:make-p-box count)))

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
  ;; UNWRAP ONCE.  This used to be (p-box-value (unbox ref)) — two unwraps for
  ;; one box — so every real call type-errored on the hash-table `unbox`
  ;; already returned, killing the whole test file.  The function had never
  ;; worked; an inverse probe (task #202) was the first thing to run it.
  ;; Same shape as pl-eq_array's unwrap, deliberately.
  (let ((h1 (if (pcl:p-box-p ref1) (pcl:p-box-value ref1) ref1))
        (h2 (if (pcl:p-box-p ref2) (pcl:p-box-value ref2) ref2)))
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

;;; ----- Test::More->builder / Test::Builder ------------------------------
;;;
;;; PCL's TAP layer *is* the builder — there is no Test::Builder object behind
;;; it — but essentially every CPAN suite opens with
;;;   my $builder = Test::More->builder;
;;;   binmode($builder->failure_output, ':utf8') if $] >= 5.008;
;;; so with no `builder` method at all the file dies before its first
;;; assertion.  That was the single cause of 22 of Capture-Tiny's 24 t-files
;;; (task #199).  Only the three output-handle accessors are answered, and
;;; they answer with PCL's own filehandle designators (handle NAME strings,
;;; which p-get-stream/%p-resolve-fh accept everywhere a handle is taken), so
;;; `binmode`, `print {…}` and `fileno` on them all work for real.
;;;
;;; Every OTHER Test::Builder method is deliberately absent: method dispatch
;;; then dies "Can't locate object method \"X\" via package \"Test::Builder\"",
;;; naming the gap, instead of a stub returning a plausible wrong value that
;;; would silently corrupt a file's counts (CLAUDE.md rule 12).
(p-defpackage |Test::Builder|)
(p-defpackage |Test::More|)

(defvar *test-builder-singleton* nil
  "The one Test::Builder object.  Test::Builder->new is a singleton in real
   Test::Builder too, and test files compare identity.")

(defun %test-builder ()
  (or *test-builder-singleton*
      (setf *test-builder-singleton*
            (p-bless (p-backslash (make-hash-table :test #'equal)) "Test::Builder"))))

(defun |Test::Builder|::pl-new (&rest args)
       (declare (ignore args))
       (%test-builder))

(defun |Test::More|::pl-builder (&rest args)
       (declare (ignore args))
       (%test-builder))

;; Real Test::Builder: output = STDOUT, failure_output = STDERR,
;; todo_output = STDOUT.
(defun |Test::Builder|::pl-output (self &rest args)
       (declare (ignore self args))
       (make-p-box "STDOUT"))

(defun |Test::Builder|::pl-failure_output (self &rest args)
       (declare (ignore self args))
       (make-p-box "STDERR"))

(defun |Test::Builder|::pl-todo_output (self &rest args)
       (declare (ignore self args))
       (make-p-box "STDOUT"))

(format t "# PCL Test library loaded~%")
