;;; pcl: pipeline=v2 gen=v2-1460
(in-package :pcl)
(setf pcl::*pcl-pl2cl-path* #P"/home/bernt/pcl/.claude/worktrees/agent-a6c7caf694eb1f8c2/pl2cl")
(pcl::box-set pcl::$0 "array.t")
(setf pcl::*pcl-skip-cache* t)
;; Initialize @INC from Perl
(setf pcl::@INC (make-array 0 :adjustable t :fill-pointer 0))
(vector-push-extend "/home/bernt/pcl/.claude/worktrees/agent-a6c7caf694eb1f8c2/perl-tests" pcl::@INC)
(vector-push-extend "." pcl::@INC)
(vector-push-extend "/home/bernt/pcl/.claude/worktrees/agent-a6c7caf694eb1f8c2/lib" pcl::@INC)
(vector-push-extend "/home/bernt/pcl/.claude/worktrees/agent-a6c7caf694eb1f8c2" pcl::@INC)
(vector-push-extend "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/x86_64-linux" pcl::@INC)
(vector-push-extend "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3" pcl::@INC)
(vector-push-extend "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/lib/5.40.3/x86_64-linux" pcl::@INC)
(vector-push-extend "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/lib/5.40.3" pcl::@INC)
(setf pcl::*p-core-inc-dirs* (list "/home/bernt/pcl/.claude/worktrees/agent-a6c7caf694eb1f8c2/lib" "/home/bernt/pcl/.claude/worktrees/agent-a6c7caf694eb1f8c2" "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/x86_64-linux" "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3" "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/lib/5.40.3/x86_64-linux" "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/lib/5.40.3"))
;; Switch to main package (Perl's default for code without 'package' statement)
(p-defpackage :main)
(in-package :main)

(pcl:p-defpackage :A)
(pcl:p-defpackage :Internals)
(pcl:p-defpackage :Sphare)
(pcl:p-defpackage :glelp)
(pcl:p-defpackage :peen)
(pcl:p-defpackage :tmp)

(p-declare-sub pl-reify)
(p-declare-sub pl-foo)
(p-declare-sub pl-test_arylen)
(p-declare-sub A::pl-DESTROY)
(p-declare-sub pl-get_x)
(p-declare-sub pl-get_y)
(defvar $a (make-p-box nil))
(defvar $b (make-p-box nil))

;; Forward declarations for undeclared package globals
(p-defcell $Etc (make-p-box nil))
(p-defcell $F1 (make-p-box nil))
(p-defcell $F2 (make-p-box nil))
(p-defcell $a__excl__0 (make-p-box nil) :perl "$a" :why :exception-global)
(p-defcell $cnt (make-p-box nil))
(p-defcell $desc (make-p-box nil))
(p-defcell $fixed (make-p-box nil))
(p-defcell $foo (make-p-box nil))
(p-defcell $got (make-p-box nil))
(p-defcell $i (make-p-box nil))
(p-defcell $inner (make-p-box nil))
(p-defcell $is_rc (make-p-box nil))
(p-defcell $outer (make-p-box nil))
(p-defcell $r (make-p-box nil))
(p-defcell $ra (make-p-box nil))
(p-defcell $ref (make-p-box nil))
(p-defcell $rh (make-p-box nil))
(p-defcell $tmp (make-p-box nil))
(p-defcell $true (make-p-box nil))
(p-defcell $x (make-p-box nil))
(p-defcell $y (make-p-box nil))
(p-defcell $z (make-p-box nil))
(p-defcell %bar (make-hash-table :test 'equal))
(p-defcell %foo (make-hash-table :test 'equal))
(p-defcell %h (make-hash-table :test 'equal))
(p-defcell @a (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @array (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @ary (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @bar (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @bee (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @bim (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @foo (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @trit (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @warn (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell main::$ra (make-p-box nil))

(p-defcell @bee (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @bee (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @bee (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @bee (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @bim (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell $t (make-p-box nil))
;; use vars '@array'
(p-eval-always
  (p-defcell @array (make-array 0 :adjustable t :fill-pointer 0)))
;; use vars '@array'
(p-eval-always
  (p-defcell @array (make-array 0 :adjustable t :fill-pointer 0)))
;; use vars '@array'
(p-eval-always
  (p-defcell @array (make-array 0 :adjustable t :fill-pointer 0)))
(p-defcell $x (make-p-box nil))
(p-defcell $y (make-p-box nil))
(p-defcell $z (make-p-box nil))
(p-defcell $y (make-p-box nil))
(p-defcell $z (make-p-box nil))
(p-defcell $x (make-p-box nil))
(p-defcell $y (make-p-box nil))
(p-defcell $z (make-p-box nil))
(p-defcell $x (make-p-box nil))
(p-defcell $y (make-p-box nil))
(p-defcell $z (make-p-box nil))
(p-defcell %x__file__0 (make-hash-table :test 'equal) :perl "%x" :why :captured)
(p-defcell @y__file__1 (make-array 0 :adjustable t :fill-pointer 0) :perl "@y" :why :captured)

;; BEGIN {

(p-BEGIN

  (p-set-current-package :main "main")

  ;; chdir 't' if -d 't'

  (p-if (p--d "t") (p-chdir "t"))

  ;; require './test.pl'

  (p-eval-always

    (p-require-file "./test.pl"))

  ;; set_up_inc('.', '../lib')

  (p-set_up_inc "." "../lib")

)

(p-sub pl-reify
  (&rest %_args)
  (:writes-args t :captures ($t) :needs (:io))
  (p-args-body
    (block nil
      (p-void-ctx (p-setf (p-aref @_ 1) (p-post++ $t))
        (p-caller-ctx (p-print (p-string-concat (p-join |$"| @_) (p-esc "\\n"))))))))

(p-sub pl-foo
  (&rest %_args)
  (:returns :str :wantarray-insensitive t :writes-args nil :needs ())
  (p-args-body (block nil "a")))

(p-sub pl-test_arylen
  (&rest %_args)
  (:writes-args nil :needs (:dynamic_scope.local :regex.qr :regex.native))
  (p-args-body
    (block nil
      (p-let
        (($ref :box (make-p-box nil))
          ($fixed :box (make-p-box nil))
          ($desc :box (make-p-box nil)))
        (p-scalar-ctx (p-list-= (vector $ref $fixed $desc) @_))
        (p-void-ctx
          ;; local $^W = 1
(let (($^W (p-box-for-local 1)))
            (p-let
              (($is_rc :str
                  (%pcl-to-string-strict
                    (p-&& $fixed (p-bit-and (p-scalar-ctx (Internals::pl-stack_refcounted)) 1))
                    "$is_rc")))
              (pl-is (p-scalar (p-cast-$ $ref))
                (p-scalar (p-if $is_rc (p-- 1) (p-undef)))
                (p-string-concat $desc ": $# on freed array is undef"))
              (p-let ((@warn :array (make-array 0 :adjustable t :fill-pointer 0)))
                ;; local $SIG{"__WARN__"} = sub {push @warn, "@_"}
(p-local-hash-elem-init %SIG "__WARN__" (lambda (&rest %_args) (let ((@_ (p-flatten-args %_args)) (*pcl-current-package* "main") (*pcl-caller-wantarray* *wantarray*)) (p-sub-frame (block nil (p-push @warn (p-string-concat (p-join |$"| @_)))))))
                  (p-setf (p-cast-$ $ref) 1000)
                  (pl-is (p-scalar (p-scalar @warn))
                    (p-scalar (p-if $is_rc 0 1))
                    (p-string-concat $desc ": number of warnings"))
                  (p-if $is_rc
                    (progn (p-caller-ctx (pl-pass (p-string-concat $desc ": pass"))))
                    (progn
                      (p-caller-ctx
                        (pl-like (p-scalar (p-aref-argbox @warn 0))
                          (p-scalar
                            (pcl::p-qr :pat "^Attempt to set length of freed array"
                              :flags ""
                              :tier :native))
                          (p-string-concat $desc ": msg"))))))))))))))

(p-sub pl-get_x
  (&rest %_args)
  (:writes-args nil :captures (%x__file__0) :needs ())
  (p-args-body
    (block nil
      (p-void-ctx (p-hash-= %x__file__0 (vector (p-.. 1 4)))
        (p-caller-ctx (p-tail-value (p-backslash %x__file__0)))))))

(p-sub pl-get_y
  (&rest %_args)
  (:writes-args nil :captures (@y__file__1) :needs ())
  (p-args-body
    (block nil
      (p-void-ctx (p-array-= @y__file__1 (p-.. 1 4))
        (p-caller-ctx (p-tail-value (p-backslash @y__file__1)))))))

(p-sub A::pl-DESTROY
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-args-body (block nil (p-scalar-= main::$ra 0))))

;;; package glelp
(p-defpackage :glelp)
(in-package :glelp)
(p-defclass plc-glelp () ())
(p-register-pkg-name :glelp "glelp")

(defvar $a (make-p-box nil))
(defvar $b (make-p-box nil))

;; Forward declarations for undeclared package globals
(p-defcell @ISA (make-array 0 :adjustable t :fill-pointer 0))


(p-eval-always (p-note-inc "warnings"))

(p-eval-always (p-note-inc "builtin"))

;;; back to package main
(in-package :main)



;;; package peen
(p-defpackage :peen)
(in-package :peen)
(p-defclass plc-peen () ())
(p-register-pkg-name :peen "peen")

(p-declare-sub Sphare::pl-pling)
(defvar $a (make-p-box nil))
(defvar $b (make-p-box nil))

;; Forward declarations for undeclared package globals
(p-defcell @ISA (make-array 0 :adjustable t :fill-pointer 0))


(p-sub Sphare::pl-pling
  (&rest %_args)
  (:returns :str :wantarray-insensitive t :writes-args nil :needs ())
  (p-args-body (block nil "pling")))

;;; back to package main
(in-package :main)

(p-declare-sub pl-t8910)
;; Forward declarations for undeclared package globals
(p-defcell $re (make-p-box nil))
(p-defcell @aelem (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @p (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @plink (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @plunk (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @q (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @qr (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @r (make-array 0 :adjustable t :fill-pointer 0))


(p-sub pl-t8910
  (&rest %_args)
  (:writes-args t :needs ())
  (p-args-body
    (block nil (p-void-ctx (p-setf (p-aref @_ 1) 5) (p-caller-ctx (p-setf (p-aref @_ 2) 7))))))

;;; back to package main
(in-package :main)

;; Forward declarations for undeclared package globals
(p-defcell $count (make-p-box nil))
(p-defcell @b (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @existing_elements (make-array 0 :adjustable t :fill-pointer 0))


;;; package tmp
(p-defpackage :tmp)
(in-package :tmp)
(p-defclass plc-tmp () ())
(p-register-pkg-name :tmp "tmp")

(defvar $a (make-p-box nil))
(defvar $b (make-p-box nil))

;; Forward declarations for undeclared package globals
(p-defcell tmp::@a (make-array 0 :adjustable t :fill-pointer 0))


;;; back to package main
(in-package :main)



;;; back to package main
(in-package :main)



(p-run-compile-phase-blocks)

(p-set-current-package :main "main")

(p-void-ctx (pl-plan 195))

(p-array-= @ary (vector 1 2 3 4 5))

(p-void-ctx (pl-is (p-scalar (p-list-ctx (p-join "" @ary))) "12345"))

(p-scalar-= $tmp (p-aref @ary (p-array-last-index @ary)))

(p-set-array-length @ary (1- (p-array-last-index @ary)))

(p-void-ctx (pl-is $tmp 5))

(p-void-ctx (pl-is (p-scalar (p-array-last-index @ary)) 3))

(p-void-ctx (pl-is (p-scalar (p-list-ctx (p-join "" @ary))) "1234"))

(p-array-= @foo (vector))

(p-scalar-= $r (p-list-ctx (p-join "," (p-array-last-index @foo) @foo)))

(p-void-ctx (pl-is $r "-1"))

(p-setf (p-aref @foo 0) "0")

(p-scalar-= $r (p-list-ctx (p-join "," (p-array-last-index @foo) @foo)))

(p-void-ctx (pl-is $r "0,0"))

(p-setf (p-aref @foo 2) "2")

(p-scalar-= $r (p-list-ctx (p-join "," (p-array-last-index @foo) @foo)))

(p-void-ctx (pl-is $r "2,0,,2"))

(p-array-= @bar (vector))

(p-setf (p-aref @bar 0) "0")

(p-setf (p-aref @bar 1) "1")

(p-scalar-= $r (p-list-ctx (p-join "," (p-array-last-index @bar) @bar)))

(p-void-ctx (pl-is $r "1,0,1"))

(p-array-= @bar (vector))

(p-scalar-= $r (p-list-ctx (p-join "," (p-array-last-index @bar) @bar)))

(p-void-ctx (pl-is $r "-1"))

(p-setf (p-aref @bar 0) "0")

(p-scalar-= $r (p-list-ctx (p-join "," (p-array-last-index @bar) @bar)))

(p-void-ctx (pl-is $r "0,0"))

(p-setf (p-aref @bar 2) "2")

(p-scalar-= $r (p-list-ctx (p-join "," (p-array-last-index @bar) @bar)))

(p-void-ctx (pl-is $r "2,0,,2"))

(p-if (p-str-ne |$^O| "VMS") (p-reset "b"))

(p-array-= @bar (vector))

(p-setf (p-aref @bar 0) "0")

(p-scalar-= $r (p-list-ctx (p-join "," (p-array-last-index @bar) @bar)))

(p-void-ctx (pl-is $r "0,0"))

(p-setf (p-aref @bar 2) "2")

(p-scalar-= $r (p-list-ctx (p-join "," (p-array-last-index @bar) @bar)))

(p-void-ctx (pl-is $r "2,0,,2"))

(p-scalar-= $foo "now is the time")

(p-void-ctx
  (pl-ok
    (p-scalar
      (p-scalar
        (p-scalar-ctx
          (p-list-= (vector $F1 $F2 $Etc)
            (p-list-ctx
              (p-list-ctx
                (p-=~ $foo (p-regex :pat "^(\\S+)\\s+(\\S+)\\s*(.*)" :flags "" :tier :native))))))))))

(p-void-ctx (pl-is $F1 "now"))

(p-void-ctx (pl-is $F2 "is"))

(p-void-ctx (pl-is $Etc "the time"))

(p-scalar-= $foo "lskjdf")

(p-or
  (p-scalar-ctx
    (pl-ok
      (p-scalar
        (p-!
          (p-scalar-= $cnt
            (p-scalar-ctx
              (p-list-= (vector $F1 $F2 $Etc)
                (p-list-ctx
                  (p-list-ctx
                    (p-=~ $foo
                      (p-regex :pat "^(\\S+)\\s+(\\S+)\\s*(.*)" :flags "" :tier :native)))))))))))
  (p-void-ctx (pl-diag (p-string-concat $cnt " " $F1 ":" $F2 ":" $Etc))))

(p-hash-= %foo (vector "blurfl" "dyick" "foo" "bar" "etc." "etc."))

(p-hash-= %bar %foo)

(p-void-ctx (pl-is (p-scalar (p-gethash-argbox %bar "foo")) "bar"))

(p-hash-= %bar (make-array 0 :adjustable t :fill-pointer 0))

(p-void-ctx (pl-is (p-scalar (p-gethash-argbox %bar "foo")) (p-scalar (p-undef))))

(p-list-= (vector %bar $a $b) (vector %foo "how" "now"))

(p-void-ctx (pl-is (p-scalar (p-gethash-argbox %bar "foo")) "bar"))

(p-void-ctx (pl-is (p-scalar (p-gethash-argbox %bar "how")) "now"))

(p-setf (p-hslice %bar (p-list-ctx (p-keys %foo))) (p-list-ctx (p-values %foo)))

(p-void-ctx (pl-is (p-scalar (p-gethash-argbox %bar "foo")) "bar"))

(p-void-ctx (pl-is (p-scalar (p-gethash-argbox %bar "how")) "now"))

(p-array-= @foo
  (p-grep (lambda ($_) (p-list-ctx (p-=~ $_ (p-regex :pat "e" :flags "" :tier :native))))
    (p-split " " "now is the time for all good men to come to")))

(p-void-ctx (pl-is (p-scalar (p-list-ctx (p-join " " @foo))) "the time men come"))

(p-array-= @foo
  (p-grep
    (lambda ($_) (p-! (p-scalar-ctx (p-=~ $_ (p-regex :pat "e" :flags "" :tier :native)))))
    (p-split " " "now is the time for all good men to come to")))

(p-void-ctx (pl-is (p-scalar (p-list-ctx (p-join " " @foo))) "now is for all good to to"))

(p-scalar-= $foo
  (p-list-ctx (p-join "" (p-aref-deref (vector "a" "b" "c" "d" "e" "f") (p-.. 0 5)))))

(p-void-ctx (pl-is $foo "abcdef"))

(p-scalar-= $foo
  (p-list-ctx (p-join "" (p-aref-deref (vector "a" "b" "c" "d" "e" "f") (p-.. 0 1)))))

(p-void-ctx (pl-is $foo "ab"))

(p-scalar-= $foo (p-list-ctx (p-join "" (p-aref-deref (vector "a" "b" "c" "d" "e" "f") 6))))

(p-void-ctx (pl-is $foo ""))

(p-array-= @foo (p-aref-deref (vector "a" "b" "c" "d" "e" "f") (vector 0 2 4)))

(p-array-= @bar (p-aref-deref (vector "a" "b" "c" "d" "e" "f") (vector 1 3 5)))

(p-scalar-= $foo (p-list-ctx (p-join "" (p-aref-deref (vector @foo @bar) (p-.. 0 5)))))

(p-void-ctx (pl-is $foo "acebdf"))

(p-scalar-= $foo (p-aref-deref (vector "a" "b" "c" "d" "e" "f") (progn 0 2 4)))

(p-void-ctx (pl-is $foo "e"))

(p-scalar-= $foo (p-aref-deref (vector "a" "b" "c" "d" "e" "f") 1))

(p-void-ctx (pl-is $foo "b"))

(p-array-= @foo (vector "foo" "bar" "burbl" "blah"))

(p-array-= @foo @foo)

(p-void-ctx (pl-is (p-scalar (p-string-concat (p-join |$"| @foo))) "foo bar burbl blah"))

(p-list-= (vector (p-list-ctx (p-undef)) @foo) @foo)

(p-void-ctx (pl-is (p-scalar (p-string-concat (p-join |$"| @foo))) "bar burbl blah"))

(p-array-= @foo (vector "XXX" @foo "YYY"))

(p-void-ctx (pl-is (p-scalar (p-string-concat (p-join |$"| @foo))) "XXX bar burbl blah YYY"))

(p-array-= @foo (p-array-= @foo (vector "foo" "b\\a\\r" "bu\\rbl" "blah")))

(p-void-ctx (pl-is (p-scalar (p-string-concat (p-join |$"| @foo))) "foo b\\a\\r bu\\rbl blah"))

(p-array-= @bar (p-array-= @foo (vector "foo" "bar")))

(p-void-ctx (pl-is (p-scalar (p-string-concat (p-join |$"| @foo))) "foo bar"))

(p-void-ctx (pl-is (p-scalar (p-string-concat (p-join |$"| @bar))) "foo bar"))

(p-array-= @bee (vector "foo" "bar" "burbl" "blah"))

(let ((*package* *package*))
  (block nil
    (tagbody :redo
      ;; local @bee = @bee
(p-local-cell @bee (p-copy-array (let ((*wantarray* t)) @bee))
  @bee
        (p-void-ctx (pl-is (p-scalar (p-string-concat (p-join |$"| @bee))) "foo bar burbl blah"))
        (let ((*package* *package*))
          (block nil
            (tagbody :redo
              ;; local (undef,@bee) = @bee
(let* ((pcl-local-rhs-0 (let ((*wantarray* t) (*p-in-list-assign-rhs* t)) @bee)))
  (p-local-cell @bee (make-array 0 :adjustable t :fill-pointer 0)
    (p-list-= (vector (p-undef) @bee) pcl-local-rhs-0)
                (p-void-ctx
                  (pl-is (p-scalar (p-string-concat (p-join |$"| @bee))) "bar burbl blah"))
                (let ((*package* *package*))
                  (block nil
                    (tagbody :redo
                      ;; local @bee = ('XXX',@bee,'YYY')
(p-local-cell @bee (p-copy-array (let ((*wantarray* t)) (p-flatten-args (list "XXX" @bee "YYY"))))
  @bee
                        (p-void-ctx
                          (pl-is (p-scalar (p-string-concat (p-join |$"| @bee)))
                            "XXX bar burbl blah YYY"))
                        (let ((*package* *package*))
                          (block nil
                            (tagbody :redo
                              ;; local @bee = local(@bee) = qw(foo bar burbl blah)
(p-local-cell @bee (p-copy-array (let ((*wantarray* t)) (vector "foo" "bar" "burbl" "blah")))
  @bee
                                (p-void-ctx
                                  (pl-is (p-scalar (p-string-concat (p-join |$"| @bee)))
                                    "foo bar burbl blah"))
                                (let ((*package* *package*))
                                  (block nil
                                    (tagbody :redo
                                      ;; local (@bim) = local(@bee) = qw(foo bar)
(let* ((pcl-local-inner-1 (let ((*wantarray* t)) (vector "foo" "bar"))))
  (p-local-cell @bee (p-copy-array pcl-local-inner-1)
    (p-local-cell @bim (p-copy-array pcl-local-inner-1)
      @bim
                                        (p-void-ctx
                                          (pl-is (p-scalar (p-string-concat (p-join |$"| @bee)))
                                            "foo bar"))
                                        (p-void-ctx
                                          (pl-is (p-scalar (p-string-concat (p-join |$"| @bim)))
                                            "foo bar")))))
                                      :next)))
                                (p-void-ctx
                                  (pl-is (p-scalar (p-string-concat (p-join |$"| @bee)))
                                    "foo bar burbl blah")))
                              :next)))
                        (p-void-ctx
                          (pl-is (p-scalar (p-string-concat (p-join |$"| @bee)))
                            "XXX bar burbl blah YYY")))
                      :next)))
                (p-void-ctx
                  (pl-is (p-scalar (p-string-concat (p-join |$"| @bee))) "bar burbl blah"))))
              :next)))
        (p-void-ctx (pl-is (p-scalar (p-string-concat (p-join |$"| @bee))) "foo bar burbl blah")))
      :next)))

(let ((*package* *package*))
  (block nil
    (tagbody :redo
      (p-let ((@bee :array (p-copy-array @bee)))
        (p-void-ctx (pl-is (p-scalar (p-string-concat (p-join |$"| @bee))) "foo bar burbl blah"))
        (let ((*package* *package*))
          (block nil
            (tagbody :redo
              (p-let ((@bee :array (p-copy-array @bee)))
                (p-scalar-ctx (p-list-= (vector (p-list-ctx (p-undef)) @bee) @bee))
                (p-void-ctx
                  (pl-is (p-scalar (p-string-concat (p-join |$"| @bee))) "bar burbl blah"))
                (let ((*package* *package*))
                  (block nil
                    (tagbody :redo
                      (p-let
                        ((@bee :array (p-copy-array (p-flatten-args (list "XXX" @bee "YYY")))))
                        (p-void-ctx
                          (pl-is (p-scalar (p-string-concat (p-join |$"| @bee)))
                            "XXX bar burbl blah YYY"))
                        (let ((*package* *package*))
                          (block nil
                            (tagbody :redo
                              (p-let
                                ((@bee :array (make-array 0 :adjustable t :fill-pointer 0)))
                                (p-array-= @bee
                                  (p-array-= @bee (vector "foo" "bar" "burbl" "blah")))
                                (p-void-ctx
                                  (pl-is (p-scalar (p-string-concat (p-join |$"| @bee)))
                                    "foo bar burbl blah"))
                                (let ((*package* *package*))
                                  (block nil
                                    (tagbody :redo
                                      (p-let
                                        ((@bim :array
                                            (make-array 0 :adjustable t :fill-pointer 0))
                                          (@bee :array
                                            (make-array 0 :adjustable t :fill-pointer 0)))
                                        (p-array-= @bim (p-array-= @bee (vector "foo" "bar")))
                                        (p-void-ctx
                                          (pl-is (p-scalar (p-string-concat (p-join |$"| @bee)))
                                            "foo bar"))
                                        (p-void-ctx
                                          (pl-is (p-scalar (p-string-concat (p-join |$"| @bim)))
                                            "foo bar")))
                                      :next)))
                                (p-void-ctx
                                  (pl-is (p-scalar (p-string-concat (p-join |$"| @bee)))
                                    "foo bar burbl blah")))
                              :next)))
                        (p-void-ctx
                          (pl-is (p-scalar (p-string-concat (p-join |$"| @bee)))
                            "XXX bar burbl blah YYY")))
                      :next)))
                (p-void-ctx
                  (pl-is (p-scalar (p-string-concat (p-join |$"| @bee))) "bar burbl blah")))
              :next)))
        (p-void-ctx (pl-is (p-scalar (p-string-concat (p-join |$"| @bee))) "foo bar burbl blah")))
      :next)))

(let ((*package* *package*))
  (block nil
    (tagbody :redo
      (p-array-= @bee @bee)
      (p-void-ctx (pl-is (p-scalar (p-string-concat (p-join |$"| @bee))) "foo bar burbl blah"))
      (let ((*package* *package*))
        (block nil
          (tagbody :redo
            (p-scalar-ctx (p-list-= (vector (p-list-ctx (p-undef)) @bee) @bee))
            (p-void-ctx (pl-is (p-scalar (p-string-concat (p-join |$"| @bee))) "bar burbl blah"))
            (let ((*package* *package*))
              (block nil
                (tagbody :redo
                  (p-array-= @bee (vector "XXX" @bee "YYY"))
                  (p-void-ctx
                    (pl-is (p-scalar (p-string-concat (p-join |$"| @bee)))
                      "XXX bar burbl blah YYY"))
                  (let ((*package* *package*))
                    (block nil
                      (tagbody :redo
                        (p-array-= @bee (p-array-= @bee (vector "foo" "bar" "burbl" "blah")))
                        (p-void-ctx
                          (pl-is (p-scalar (p-string-concat (p-join |$"| @bee)))
                            "foo bar burbl blah"))
                        (let ((*package* *package*))
                          (block nil
                            (tagbody :redo
                              (p-array-= @bim (p-array-= @bee (vector "foo" "bar")))
                              (p-void-ctx
                                (pl-is (p-scalar (p-string-concat (p-join |$"| @bee)))
                                  "foo bar"))
                              (p-void-ctx
                                (pl-is (p-scalar (p-string-concat (p-join |$"| @bim)))
                                  "foo bar"))
                              :next)))
                        :next)))
                  :next)))
            :next)))
      :next)))

(p-scalar-= $t (p-scalar-ctx (pl-curr_test)))

(p-void-ctx (pl-reify "ok"))

(p-void-ctx (pl-reify "ok"))

(p-void-ctx (pl-curr_test $t))

(p-void-ctx (pl-is (p-scalar (p-aref-deref (vector "foo" "bar" "snorfle") 2)) "snorfle"))

(p-array-= @ary (vector 12 23 34 45 56))

(p-void-ctx (pl-is (p-scalar (p-shift @ary)) 12))

(p-void-ctx (pl-is (p-scalar (p-pop @ary)) 56))

(p-void-ctx (pl-is (p-scalar (p-push @ary 56)) 4))

(p-void-ctx (pl-is (p-scalar (p-unshift @ary 12)) 5))

(p-array-= @foo (p-aref-deref (vector (pl-foo)) (vector 0 0)))

(p-void-ctx (pl-is (p-scalar (p-aref-argbox @foo 1)) "a"))

(p-let (($got :box (make-p-box nil)))
  (p-my-= $got
    (p-scalar-ctx
      (pl-runperl "prog"
        (p-esc
          "\\n\\t\\t    sub X::DESTROY { @a = () }\\n\\t\\t    @a = (bless {}, q{X});\\n\\t\\t    @a = ();\\n\\t\\t")
        "stderr"
        1)))
  (p-=~ $got (p-subst :pat "\\n" :rep " " :flags "g" :tier :native))
  (p-void-ctx (pl-is $got ""))
  (let ((*package* *package*))
    (block nil
      (tagbody :redo
        (p-let ((@a :array (make-array 0 :adjustable t :fill-pointer 0)))
          (p-array-= @a (p-.. 0 4))
          (p-void-ctx (pl-is (p-scalar (p-aref-argbox @a -1)) 4))
          (p-void-ctx (pl-is (p-scalar (p-aref-argbox @a -2)) 3))
          (p-void-ctx (pl-is (p-scalar (p-aref-argbox @a -5)) 0))
          (p-void-ctx (pl-ok (p-scalar (p-! (p-defined (p-aref @a -6))))))
          (p-void-ctx (pl-is (p-scalar (p-aref-argbox @a 2.1)) 2))
          (p-void-ctx (pl-is (p-scalar (p-aref-argbox @a 2.9)) 2))
          (p-void-ctx (pl-is (p-scalar (p-aref-argbox @a "undef")) 0))
          (p-void-ctx (pl-is (p-scalar (p-aref-argbox @a "3rd")) 3)))
        :next)))
  (let ((*package* *package*))
    (p-dyn-once
      (block nil
        (tagbody :redo
          (p-let ((@a :array (make-array 0 :adjustable t :fill-pointer 0)))
            (p-eval "$a[-1] = 0" (list (cons "$got" $got) (cons "@a" @a)))
            (p-void-ctx
              (pl-like $@
                (p-scalar
                  (pcl::p-qr :pat "Modification of non-creatable array value attempted, subscript -1"
                    :flags ""
                    :tier :native))
                "$a[-1] = 0")))
          :next))))
  (let ((*package* *package*))
    (block nil
      (tagbody :redo
        (p-let (($a__excl__0 :box (make-p-box nil) :perl "$a" :why :exception-global))
          (p-my-= $a__excl__0
            (p-arylen-ref (make-p-box (make-array 0 :adjustable t :fill-pointer 0))))
          (p-void-ctx (pl-test_arylen $a__excl__0 1 "$a"))
          (p-void-ctx
            (pl-test_arylen
              (p-list-ctx
                (funcall
                  (lambda ()
                    (progn
                      (p-let ((@a :array (make-array 0 :adjustable t :fill-pointer 0)))
                        (p-arylen-ref @a))))))
              0
              "do {}")))
        :next)))
  (let ((*package* *package*))
    (block nil
      (tagbody :redo
        (p-let (($outer :box (make-p-box nil)))
          (p-my-= $outer (p-arylen-ref @array))
          (p-void-ctx (pl-is (p-scalar (p-cast-$ $outer)) -1))
          (p-void-ctx (pl-is (p-scalar (p-scalar @array)) 0))
          (p-setf (p-cast-$ $outer) 3)
          (p-void-ctx (pl-is (p-scalar (p-cast-$ $outer)) 3))
          (p-void-ctx (pl-is (p-scalar (p-scalar @array)) 4))
          (p-let (($ref :box (make-p-box nil)))
            (p-my-= $ref (p-backslash @array))
            (p-let (($inner :box (make-p-box nil)))
              (let ((*package* *package*))
                (block nil
                  (tagbody :redo
                    ;; local @array
(p-local-cell @array (make-array 0 :adjustable t :fill-pointer 0)
                      (p-my-= $inner (p-arylen-ref @array))
                      (p-void-ctx (pl-is (p-scalar (p-cast-$ $inner)) -1))
                      (p-void-ctx (pl-is (p-scalar (p-scalar @array)) 0))
                      (p-setf (p-cast-$ $outer) 6)
                      (p-void-ctx (pl-is (p-scalar (p-scalar (p-cast-@ $ref))) 7))
                      (p-void-ctx (pl-is (p-scalar (p-cast-$ $inner)) -1))
                      (p-void-ctx (pl-is (p-scalar (p-scalar @array)) 0))
                      (p-setf (p-cast-$ $inner) 42))
                    :next)))
              (p-void-ctx (pl-is (p-scalar (p-scalar @array)) 7))
              (p-void-ctx (pl-is (p-scalar (p-cast-$ $outer)) 6))
              (p-void-ctx
                (pl-is (p-scalar (p-cast-$ $inner))
                  (p-scalar (p-undef))
                  (p-string-concat "orphaned " (p-array-last-index @foo) " is always undef")))
              (p-void-ctx (pl-is (p-scalar (p-scalar @array)) 7))
              (p-void-ctx (pl-is (p-scalar (p-cast-$ $outer)) 6))
              (p-setf (p-cast-$ $inner) 1)
              (p-void-ctx (pl-is (p-scalar (p-scalar @array)) 7))
              (p-void-ctx (pl-is (p-scalar (p-cast-$ $outer)) 6))
              (p-setf (p-cast-$ $inner) 503)
              (p-void-ctx (pl-is (p-scalar (p-scalar @array)) 7))
              (p-void-ctx (pl-is (p-scalar (p-cast-$ $outer)) 6)))))
        :next)))
  (let ((*package* *package*))
    (block nil
      (tagbody :redo
        (p-foreach ($_ (vector 1 2))
          (let ((*package* *package*))
            (block nil
              (tagbody :redo
                ;; local @a
(p-local-cell @a (make-array 0 :adjustable t :fill-pointer 0)
                  (p-void-ctx (pl-is (p-scalar (p-array-last-index @a)) -1))
                  (p-array-= @a (p-.. 1 4)))
                :next))))
        :next)))
  (let ((*package* *package*))
    (block nil
      (tagbody :redo
        (p-let ((@array :array (make-array 0 :adjustable t :fill-pointer 0)))
          (p-array-= @array (p-.. 1 4))
          (p-set-array-length @array 7)
          (p-void-ctx (pl-is (p-scalar (p-array-last-index 4)) 7))
          (p-let (($x :box (make-p-box nil)))
            (p-set-array-length $x 3)
            (p-void-ctx (pl-is (p-scalar (p-scalar (p-cast-@ $x))) 4))
            (p-push (p-cast-@ @array) 23)
            (p-void-ctx (pl-is (p-scalar (p-undef)) 23))))
        :next)))
  (let ((*package* *package*))
    (block nil
      (tagbody :redo
        (p-array-= @array (p-.. 1 4))
        (p-set-array-length @array 7)
        (p-void-ctx (pl-is (p-scalar (p-array-last-index 4)) 7))
        (p-let (($x :box (make-p-box nil)))
          (p-set-array-length $x 3)
          (p-void-ctx (pl-is (p-scalar (p-scalar (p-cast-@ $x))) 4))
          (p-push (p-cast-@ @array) 23)
          (p-void-ctx (pl-is (p-scalar (p-undef)) 23)))
        :next)))
  (let ((*package* *package*))
    (block nil
      (tagbody :redo
        (p-scalar-ctx (p-list-= (vector $x $y $z) (p-.. 1 3)))
        (p-scalar-ctx (p-list-= (vector $y $z) (vector $x $y)))
        (p-void-ctx (pl-is (p-scalar (p-string-concat $x " " $y " " $z)) "1 1 2"))
        :next)))
  (let ((*package* *package*))
    (block nil
      (tagbody :redo
        (p-scalar-ctx (p-list-= (vector $x $y $z) (p-.. 1 3)))
        (p-list-= (vector $y $z) (vector $x $y))
        (p-void-ctx (pl-is (p-scalar (p-string-concat $x " " $y " " $z)) "1 1 2"))
        :next)))
  (let ((*package* *package*))
    (block nil
      (tagbody :redo
        (p-let (($true :box (make-p-box nil)))
          (p-my-= $true 1)
          (p-scalar-ctx (p-list-= (vector $x $y $z) (p-.. 1 3)))
          (p-list-= (vector $y $z) (p-&& $true (vector $x $y)))
          (p-void-ctx (pl-is (p-scalar (p-string-concat $x " " $y " " $z)) "1 1 2")))
        :next)))
  (let ((*package* *package*))
    (block nil
      (tagbody :redo
        (p-let (($x :box (make-p-box nil)))
          (p-my-= $x (p-scalar-ctx (pl-get_x)))
          (p-hash-= %x__file__0 (p-cast-% $x))
          (p-void-ctx
            (pl-is
              (p-scalar
                (p-list-ctx
                  (p-join " "
                    (p-map (lambda ($_) (vector $_ (p-gethash %x__file__0 $_)))
                      (p-list-ctx (%p-sort-classic :default (p-list-ctx (p-keys %x__file__0))))))))
              "1 2 3 4"
              "bug 70171 (self-assignment via my %x = %$x)"))
          (p-let (($y :box (make-p-box nil)))
            (p-my-= $y (p-scalar-ctx (pl-get_y)))
            (p-array-= @y__file__1 (p-cast-@ $y))
            (p-void-ctx
              (pl-is (p-scalar (p-string-concat (p-join |$"| @y__file__1)))
                "1 2 3 4"
                "bug 70171 (self-assignment via my @x = @$x)"))))
        :next)))
  (let ((*package* *package*))
    (block nil
      (tagbody :redo
        (p-let
          (($i :box (make-p-box nil)) ($ra :box (make-p-box nil)) ($rh :box (make-p-box nil)))
          (tagbody :again (p-let ((@a :array (make-array 0 :adjustable t :fill-pointer 0)))
              (p-array-= @a (p-cast-@ $ra))
              (p-let ((%h :hash (make-hash-table :test 'equal)))
                (p-hash-= %h (p-cast-% $rh))
                (p-array-= @a (vector "1" "2" "3" "4"))
                (p-hash-= %h (vector "a" "1" "b" "2" "c" "3" "d" "4"))
                (p-my-= $ra (p-backslash @a))
                (p-my-= $rh (p-backslash %h))
                (p-if (p-! (p-post++ $i)) (go :again))
                (p-void-ctx
                  (pl-is (p-scalar (p-string-concat (p-join |$"| @a)))
                    "1 2 3 4"
                    "bug 70171 (self-assignment via my @x = @$x) - goto variant"))
                (p-void-ctx
                  (pl-is
                    (p-scalar
                      (p-list-ctx
                        (p-join " "
                          (p-map (lambda ($_) (vector $_ (p-gethash %h $_)))
                            (p-list-ctx (%p-sort-classic :default (p-list-ctx (p-keys %h))))))))
                    "a 1 b 2 c 3 d 4"
                    "bug 70171 (self-assignment via my %x = %$x) - goto variant"))))))
        :next)))
  (p-glob-assign "main" "trit" (p-make-typeglob "main" "scile"))
  (p-aref @trit 0)
  (p-void-ctx (pl-ok 1 "aelem_fast on a nonexistent array does not crash"))
  (p-scalar-= main::$ra
    (make-p-box
      (p-array-init (p-bless (make-p-box (make-array 0 :adjustable t :fill-pointer 0)) "A"))))
  (p-undef (p-cast-@ main::$ra))
  (p-void-ctx (pl-pass "no crash when freeing array that is being undeffed"))
  (p-scalar-= main::$ra
    (make-p-box
      (p-array-init (p-bless (make-p-box (make-array 0 :adjustable t :fill-pointer 0)) "A"))))
  (p-array-deref-= (p-cast-@ main::$ra) (p-.. "a" "z"))
  (p-void-ctx (pl-pass "no crash when freeing array that is being cleared")))

(in-package :glelp)

(p-set-current-package :glelp "glelp")

(p-weaken (p-scalar-= $a (p-backslash @ISA)))

(p-array-= @ISA (vector "Foo"))

(p-weaken (p-scalar-= $a (p-backslash (p-aref-box @ISA 0))))

(p-void-ctx (main::pl-is (p-scalar @ISA) 1 "backref magic is not copied to elements"))

(in-package :main)

(p-set-current-package :main "main")

(in-package :peen)

(p-set-current-package :peen "peen")

(p-set-array-length @ISA -1)

(p-array-= @ISA (vector "Foo"))

(p-setf (p-aref @ISA 0) (progn "Sphare"))

(p-void-ctx
  (main::pl-is
    (p-scalar (p-scalar-ctx (p-eval-block (peen::pl-pling (p-list-ctx (peen::pl-peen))))))
    "pling"
    "arylen_p magic does not stop isa magic from being copied"))

(in-package :main)

(p-set-current-package :main "main")

(p-void-ctx
  (p-funcall-ref
    (lambda (&rest %_args)
      (let
        ((@_ (p-flatten-args %_args))
          (*pcl-current-package* "main")
          (*pcl-caller-wantarray* *wantarray*))
        (p-sub-frame
          (block nil
            (p-void-ctx
              (pl-ok (p-scalar (p-exists-array @_ 0))
                "exists returns true for &PL_sv_undef elem [perl #7508]")
              (p-caller-ctx
                (pl-is (p-scalar (p-backslash (p-aref-box @_ 0)))
                  (p-scalar (p-backslash (p-undef)))
                  "undef preserves identity in array [perl #109726]")))))))
    (p-list-ctx (p-undef))))

(p-array-= @_
  (p-list-ctx
    (p-funcall-ref
      (lambda (&rest %_args)
        (let
          ((@_ (p-flatten-args %_args))
            (*pcl-current-package* "main")
            (*pcl-caller-wantarray* *wantarray*))
          (p-sub-frame
            (block nil
              (p-void-ctx
                (p-let ((@a :array (make-array 0 :adjustable t :fill-pointer 0)))
                  (setf (p-aref @a 1) 1)
                  (p-caller-ctx @a))))))))))

(p-void-ctx
  (pl-is (p-scalar (p-list-ctx (p-join " " (p-map (lambda ($_) (p-// $_ "undef")) @_))))
    "undef 1"
    "returning my @a with nonexistent elements"))

(p-array-= @plink (p-array-= @plunk (vector)))

(p-setf (p-aref @plink 3) 1)

(p-void-ctx
  (p-funcall-ref
    (lambda (&rest %_args)
      (let
        ((@_ (p-flatten-args %_args))
          (*pcl-current-package* "main")
          (*pcl-caller-wantarray* *wantarray*))
        (p-sub-frame
          (block nil
            (p-void-ctx (p-setf (p-aref @_ 0) 2)
              (pl-is (p-scalar (p-aref-argbox @plink 0))
                2
                "@_ alias to nonexistent elem within array")
              (p-setf (p-aref @_ 1) 3)
              (pl-is (p-scalar (p-aref-argbox @plink 1))
                3
                "@_ alias to nonexistent neg index within array")
              (pl-is (p-scalar (p-aref-argbox @_ 2))
                (p-scalar (p-undef))
                "reading alias to negative index past beginning")
              (p-eval-block (p-setf (p-aref @_ 2) 42))
              (pl-like $@
                (p-scalar
                  (pcl::p-qr :pat (p-esc
                      "Modification of non-creatable array value attempted, (?x:\\n               )subscript -5")
                    :flags ""
                    :tier :native))
                "error when setting alias to negative index past beginning")
              (pl-is (p-scalar (p-aref-argbox @_ 3))
                (p-scalar (p-undef))
                "reading alias to -1 elem of empty array")
              (p-eval-block (p-setf (p-aref @_ 3) 42))
              (p-caller-ctx
                (pl-like $@
                  (p-scalar
                    (pcl::p-qr :pat (p-esc
                        "Modification of non-creatable array value attempted, (?x:\\n               )subscript -1")
                      :flags ""
                      :tier :native))
                  "error when setting alias to -1 elem of empty array")))))))
    (p-aref-argbox @plink 0)
    (p-aref-argbox @plink -2)
    (p-aref-argbox @plink -5)
    (p-aref-argbox @plunk -1)))

(p-if (p-! (p-bit-and (p-scalar-ctx (Internals::pl-stack_refcounted)) 1))
  (progn
    (p-scalar-= $_ (p-arylen-ref (make-p-box (make-array 0 :adjustable t :fill-pointer 0))))
    (p-setf (p-cast-$ $_) (p-backslash 1))
    (p-string-concat (p-cast-$ $_))))

(p-void-ctx (pl-pass "no assertion failure after assigning ref to arylen when ary is gone"))

(let ((*package* *package*))
  (block nil
    (tagbody :redo
      (p-let ((@a :array (make-array 0 :adjustable t :fill-pointer 0)))
        (p-array-= @a (p-.. 0 299))
        (p-void-ctx
          (pl-is (p-scalar (p-aref-argbox @a -256)) (p-scalar (p-- 300 256)) "lex -256"))
        (p-void-ctx
          (pl-is (p-scalar (p-aref-argbox @a -255)) (p-scalar (p-- 300 255)) "lex -255"))
        (p-void-ctx
          (pl-is (p-scalar (p-aref-argbox @a -254)) (p-scalar (p-- 300 254)) "lex -254"))
        (p-void-ctx
          (pl-is (p-scalar (p-aref-argbox @a -129)) (p-scalar (p-- 300 129)) "lex -129"))
        (p-void-ctx
          (pl-is (p-scalar (p-aref-argbox @a -128)) (p-scalar (p-- 300 128)) "lex -128"))
        (p-void-ctx
          (pl-is (p-scalar (p-aref-argbox @a -127)) (p-scalar (p-- 300 127)) "lex -127"))
        (p-void-ctx
          (pl-is (p-scalar (p-aref-argbox @a -126)) (p-scalar (p-- 300 126)) "lex -126"))
        (p-void-ctx (pl-is (p-scalar (p-aref-argbox @a -1)) (p-scalar (p-- 300 1)) "lex   -1"))
        (p-void-ctx (pl-is (p-scalar (p-aref-argbox @a 0)) 0 "lex    0"))
        (p-void-ctx (pl-is (p-scalar (p-aref-argbox @a 1)) 1 "lex    1"))
        (p-void-ctx (pl-is (p-scalar (p-aref-argbox @a 126)) 126 "lex  126"))
        (p-void-ctx (pl-is (p-scalar (p-aref-argbox @a 127)) 127 "lex  127"))
        (p-void-ctx (pl-is (p-scalar (p-aref-argbox @a 128)) 128 "lex  128"))
        (p-void-ctx (pl-is (p-scalar (p-aref-argbox @a 129)) 129 "lex  129"))
        (p-void-ctx (pl-is (p-scalar (p-aref-argbox @a 254)) 254 "lex  254"))
        (p-void-ctx (pl-is (p-scalar (p-aref-argbox @a 255)) 255 "lex  255"))
        (p-void-ctx (pl-is (p-scalar (p-aref-argbox @a 256)) 256 "lex  256"))
        (p-array-= @aelem (p-.. 0 299))
        (p-void-ctx
          (pl-is (p-scalar (p-aref-argbox @aelem -256)) (p-scalar (p-- 300 256)) "pkg -256"))
        (p-void-ctx
          (pl-is (p-scalar (p-aref-argbox @aelem -255)) (p-scalar (p-- 300 255)) "pkg -255"))
        (p-void-ctx
          (pl-is (p-scalar (p-aref-argbox @aelem -254)) (p-scalar (p-- 300 254)) "pkg -254"))
        (p-void-ctx
          (pl-is (p-scalar (p-aref-argbox @aelem -129)) (p-scalar (p-- 300 129)) "pkg -129"))
        (p-void-ctx
          (pl-is (p-scalar (p-aref-argbox @aelem -128)) (p-scalar (p-- 300 128)) "pkg -128"))
        (p-void-ctx
          (pl-is (p-scalar (p-aref-argbox @aelem -127)) (p-scalar (p-- 300 127)) "pkg -127"))
        (p-void-ctx
          (pl-is (p-scalar (p-aref-argbox @aelem -126)) (p-scalar (p-- 300 126)) "pkg -126"))
        (p-void-ctx
          (pl-is (p-scalar (p-aref-argbox @aelem -1)) (p-scalar (p-- 300 1)) "pkg   -1"))
        (p-void-ctx (pl-is (p-scalar (p-aref-argbox @aelem 0)) 0 "pkg    0"))
        (p-void-ctx (pl-is (p-scalar (p-aref-argbox @aelem 1)) 1 "pkg    1"))
        (p-void-ctx (pl-is (p-scalar (p-aref-argbox @aelem 126)) 126 "pkg  126"))
        (p-void-ctx (pl-is (p-scalar (p-aref-argbox @aelem 127)) 127 "pkg  127"))
        (p-void-ctx (pl-is (p-scalar (p-aref-argbox @aelem 128)) 128 "pkg  128"))
        (p-void-ctx (pl-is (p-scalar (p-aref-argbox @aelem 129)) 129 "pkg  129"))
        (p-void-ctx (pl-is (p-scalar (p-aref-argbox @aelem 254)) 254 "pkg  254"))
        (p-void-ctx (pl-is (p-scalar (p-aref-argbox @aelem 255)) 255 "pkg  255"))
        (p-void-ctx (pl-is (p-scalar (p-aref-argbox @aelem 256)) 256 "pkg  256")))
      :next)))

(p-array-= @ary (vector "a" "b"))

(p-list-= (vector (p-aref @ary 0) (p-aref @ary 1)) (vector (p-aref @ary 1) (p-aref @ary 0)))

(p-void-ctx
  (pl-is (p-scalar (p-string-concat (p-join |$"| @ary)))
    "b a"
    "aelemfast with the same array on both sides of list assignment"))

(p-foreach ($_ (p-list-ctx (p-scalar (p-array-last-index @foo)))) (p-my-= $_ 3))

(p-void-ctx
  (pl-is (p-scalar (p-array-last-index @foo))
    3
    "assigning to arylen aliased in foreach(scalar $#arylen)"))

(let ((*package* *package*))
  (block nil
    (tagbody :redo
      (p-let ((@a :array (make-array 0 :adjustable t :fill-pointer 0)))
        (p-array-= @a (vector "a" "b" "c"))
        (p-array-= @a @a)
        (p-void-ctx
          (pl-is (p-scalar (p-string-concat (p-join |$"| @a))) "a b c" "assigning to itself")))
      :next)))

(p-void-ctx
  (p-funcall-ref
    (lambda (&rest %_args)
      (let
        ((@_ (p-flatten-args %_args))
          (*pcl-current-package* "main")
          (*pcl-caller-wantarray* *wantarray*))
        (p-sub-frame
          (block nil
            (p-void-ctx (p-undef (p-make-typeglob "main" "_")) (p-caller-ctx (p-shift @_)))))))))

(p-void-ctx
  (p-funcall-ref
    (lambda (&rest %_args)
      (let
        ((@_ (p-flatten-args %_args))
          (*pcl-current-package* "main")
          (*pcl-caller-wantarray* *wantarray*))
        (p-sub-frame
          (block nil
            (p-void-ctx (p-undef (p-make-typeglob "main" "_")) (p-caller-ctx (p-pop @_)))))))))

(p-set-array-length @a -1)

(let ((_prev (p-array-last-index @a))) (p-set-array-length @a (1+ _prev)) _prev)

(p-list-= (vector) (p-- 0 (p-scalar-ctx (p-splice @a))))

(p-set-array-length @a -1)

(let ((_prev (p-array-last-index @a))) (p-set-array-length @a (1+ _prev)) _prev)

(p-list-= (vector) (p-- (p-splice @a)))

(p-set-array-length @a -1)

(let ((_prev (p-array-last-index @a))) (p-set-array-length @a (1+ _prev)) _prev)

(p-list-= (vector) (p-+ 0 (p-scalar-ctx (p-splice @a))))

(p-set-array-length @a -1)

(let ((_prev (p-array-last-index @a))) (p-set-array-length @a (1+ _prev)) _prev)

(p-list-= (vector) (p-- 0 (p-scalar-ctx (p-splice @a 0 1 1 1))))

(p-set-array-length @a -1)

(let ((_prev (p-array-last-index @a))) (p-set-array-length @a (1+ _prev)) _prev)

(p-list-= (vector) (p-- (p-splice @a 0 1 1 1)))

(p-set-array-length @a -1)

(let ((_prev (p-array-last-index @a))) (p-set-array-length @a (1+ _prev)) _prev)

(p-list-= (vector) (p-+ 0 (p-scalar-ctx (p-splice @a 0 1 1 1))))

(let ((*package* *package*))
  (block nil
    (tagbody :redo
      (p-let ((@p :array (make-array 0 :adjustable t :fill-pointer 0)))
        (setf (p-aref @p 0) 1)
        (setf (p-aref @p 2) 2)
        (p-void-ctx (pl-t8910 @p))
        (p-void-ctx
          (pl-is (p-scalar (p-string-concat (p-join |$"| @p)))
            "1 5 7"
            "lazy element creation with sub call"))
        (p-let ((@q :array (make-array 0 :adjustable t :fill-pointer 0)))
          (p-setf (p-aslice @q 0) 1)
          (p-setf (p-aslice @q 2) 2)
          (p-let ((@qr :array (make-array 0 :adjustable t :fill-pointer 0)))
            (p-array-= @qr (p-refgen-list @q))
            ;; is $qr[$_], \$q[$_], "lazy element creation with refgen" foreach 0..2
(p-foreach ($_ (p-.. 0 2)) (p-void-ctx (pl-is (p-scalar (p-aref-argbox @qr $_)) (p-scalar (p-backslash (p-aref-box @q $_))) "lazy element creation with refgen")))
            (p-void-ctx
              (pl-isnt (p-scalar (p-aref-argbox @qr 1))
                (p-scalar (p-backslash (p-undef)))
                "lazy element creation with refgen"))
            (p-let ((@r :array (make-array 0 :adjustable t :fill-pointer 0)))
              (setf (p-aref @r 1) 1)
              (p-foreach ($re (p-flatten-args (list (vector) @r))) :my t (p-my-= $re 5))
              (p-void-ctx
                (pl-is (p-scalar (p-list-ctx (p-join "" @r)))
                  "55"
                  "lazy element creation with foreach"))))))
      :next)))

(p-set-current-package :main "main")

(p-let ((@a :array (make-array 0 :adjustable t :fill-pointer 0)))
  (let ((_prev (p-array-last-index (p-backslash @a))))
    (p-set-array-length (p-backslash @a) (1+ _prev))
    _prev)
  (p-let ((@b :array (make-array 0 :adjustable t :fill-pointer 0)))
    (p-array-= @b @a)
    (p-void-ctx
      (pl-ok (p-scalar (p-! (p-exists-array @a 0)))
        "copying an array via = does not vivify elements"))
    (p-delete-array @a 0)
    (p-setf (p-aslice @a (p-.. 1 5)) (p-.. 1 5))
    (let ((_prev (p-array-last-index @a))) (p-set-array-length @a (1+ _prev)) _prev)
    (p-let (($count :box (make-p-box nil)))
      (p-let ((@existing_elements :array (make-array 0 :adjustable t :fill-pointer 0)))
        (p-array-= @existing_elements
          (p-list-ctx
            (p-map (lambda ($_) (p-if (p-exists-array @a (p-post++ $count)) $_ (vector))) @a)))
        (p-void-ctx
          (pl-is (p-scalar (p-list-ctx (p-join "," @existing_elements)))
            "1,2,3,4,5"
            "map {} @a does not vivify elements"))
        (p-set-array-length @a -1)
        (let ((*package* *package*))
          (block nil
            (tagbody :redo
              ;; local $a[3] = 12
(p-local-array-elem-init @a 3 12
                (p-let ((@foo :array (make-array 0 :adjustable t :fill-pointer 0)))
                  (p-array-= @foo @a)))
              :next)))
        (p-void-ctx
          (pl-is (p-scalar @a) 0 "unwinding localization of elem past end of array shrinks it"))))))

(in-package :tmp)

(p-set-current-package :tmp "tmp")

(let ((_prev (p-array-last-index (p-backslash tmp::@a))))
  (p-set-array-length (p-backslash tmp::@a) (1+ _prev))
  _prev)

(in-package :main)

(p-set-current-package :main "main")

(p-let ((@b :array (make-array 0 :adjustable t :fill-pointer 0)))
  (p-array-= @b tmp::@a)
  (p-void-ctx
    (pl-ok (p-scalar (p-! (p-exists-array tmp::@a 0)))
      "copying an array via = does not vivify elements"))
  (p-delete-array tmp::@a 0)
  (p-setf (p-aslice tmp::@a (p-.. 1 5)) (p-.. 1 5))
  (let ((_prev (p-array-last-index tmp::@a))) (p-set-array-length tmp::@a (1+ _prev)) _prev)
  (p-let (($count :box (make-p-box nil)))
    (p-let ((@existing_elements :array (make-array 0 :adjustable t :fill-pointer 0)))
      (p-array-= @existing_elements
        (p-list-ctx
          (p-map (lambda ($_) (p-if (p-exists-array tmp::@a (p-post++ $count)) $_ (vector)))
            tmp::@a)))
      (p-void-ctx
        (pl-is (p-scalar (p-list-ctx (p-join "," @existing_elements)))
          "1,2,3,4,5"
          "map {} @a does not vivify elements"))
      (p-set-array-length tmp::@a -1)
      (let ((*package* *package*))
        (block nil
          (tagbody :redo
            ;; local $tmp::a[3] = 12
(p-local-array-elem-init tmp::@a 3 12
              (p-let ((@foo :array (make-array 0 :adjustable t :fill-pointer 0)))
                (p-array-= @foo tmp::@a)))
            :next)))
      (p-void-ctx
        (pl-is (p-scalar tmp::@a)
          0
          "unwinding localization of elem past end of array shrinks it")))))

(p-set-current-package :main "main")

(let ((*package* *package*))
  (block nil
    (tagbody :redo
      (p-let ((@a :array (make-array 0 :adjustable t :fill-pointer 0)))
        (p-array-= @a 1)
        (p-delete-array @a 0)
        (p-let ((@b :array (make-array 0 :adjustable t :fill-pointer 0)))
          (p-array-= @b @a)
          (p-void-ctx
            (pl-ok (p-scalar (p-! (p-exists-array @a 0)))
              "copying an array via = does not vivify elements"))
          (p-delete-array @a 0)
          (p-setf (p-aslice @a (p-.. 1 5)) (p-.. 1 5))
          (p-let (($count :box (make-p-box nil)))
            (p-let ((@existing_elements :array (make-array 0 :adjustable t :fill-pointer 0)))
              (p-array-= @existing_elements
                (p-list-ctx
                  (p-map (lambda ($_) (p-if (p-exists-array @a (p-post++ $count)) $_ (vector)))
                    @a)))
              (p-void-ctx
                (pl-is (p-scalar (p-list-ctx (p-join "," @existing_elements)))
                  "1,2,3,4,5"
                  "map {} @a does not vivify elements"))
              (p-array-= @a (vector))
              (let ((*package* *package*))
                (block nil
                  (tagbody :redo
                    ;; local $a[3] = 12
(p-local-array-elem-init @a 3 12
                      (p-let ((@foo :array (make-array 0 :adjustable t :fill-pointer 0)))
                        (p-array-= @foo @a)))
                    :next)))
              (p-void-ctx
                (pl-is (p-scalar @a)
                  0
                  "unwinding localization of elem past end of array shrinks it"))))))
      :next)))

(let ((*package* *package*))
  (block nil
    (tagbody :redo
      (p-let ((@a :array (make-array 0 :adjustable t :fill-pointer 0)))
        (catch :pcl-goto-aftermap
          (setf (p-aref @a 1) 1)
          (p-map (lambda ($_) (p-unshift @a 7) (p-scalar-= $_ 3) (throw :pcl-goto-aftermap nil))
            @a))
        (catch :pcl-goto-aftermath (tagbody :aftermap
            (p-void-ctx
              (pl-is (p-scalar (p-string-concat "[" (p-join |$"| @a) "]"))
                "[7 3 1]"
                "non-elems read from @a do not lose their position"))
            (p-array-= @a (vector))
            (let ((_prev (p-array-last-index @a))) (p-set-array-length @a (1+ _prev)) _prev)
            (setf (p-aref @a 1) 1)
            (p-map
              (lambda ($_) (p-unshift @a 7) (p-scalar-= $_ 3) (throw :pcl-goto-aftermath nil))
              @a)))
        (tagbody :aftermath (p-void-ctx
            (pl-is (p-scalar (p-string-concat "[" (p-join |$"| @a) "]"))
              "[7 3 1]"
              "non-elems read from magical @a do not lose their position"))))
      :next)))

(let ((*package* *package*))
  (p-dyn-once
    (block nil
      (tagbody :redo
        (p-let ((@a :array (make-array 0 :adjustable t :fill-pointer 0)))
          (setf (p-aref @a 1) 1)
          (p-void-ctx
            (p-funcall-ref
              (lambda (&rest %_args)
                (let
                  ((@_ (p-flatten-args %_args))
                    (*pcl-current-package* "main")
                    (*pcl-caller-wantarray* *wantarray*))
                  (p-sub-frame
                    (block nil
                      (p-void-ctx (p-unshift @a 7) (p-caller-ctx (p-setf (p-aref @_ 0) 3)))))))
              (p-aref-argbox @a 0)))
          (p-void-ctx
            (pl-is (p-scalar (p-string-concat "[" (p-join |$"| @a) "]"))
              "[7 3 1]"
              "holes passed to sub do not lose their position (multideref)"))
          (p-array-= @a (vector))
          (let ((_prev (p-array-last-index @a))) (p-set-array-length @a (1+ _prev)) _prev)
          (setf (p-aref @a 1) 1)
          (p-void-ctx
            (p-funcall-ref
              (lambda (&rest %_args)
                (let
                  ((@_ (p-flatten-args %_args))
                    (*pcl-current-package* "main")
                    (*pcl-caller-wantarray* *wantarray*))
                  (p-sub-frame
                    (block nil
                      (p-void-ctx (p-unshift @a 7) (p-caller-ctx (p-setf (p-aref @_ 0) 3)))))))
              (p-aref-argbox @a 0)))
          (p-void-ctx
            (pl-is (p-scalar (p-string-concat "[" (p-join |$"| @a) "]"))
              "[7 3 1]"
              "holes passed to sub do not lose their position (multideref, mg)")))
        :next))))

(let ((*package* *package*))
  (p-dyn-once
    (block nil
      (tagbody :redo
        (p-let ((@a :array (make-array 0 :adjustable t :fill-pointer 0)))
          (setf (p-aref @a 1) 1)
          (p-void-ctx
            (p-funcall-ref
              (lambda (&rest %_args)
                (let
                  ((@_ (p-flatten-args %_args))
                    (*pcl-current-package* "main")
                    (*pcl-caller-wantarray* *wantarray*))
                  (p-sub-frame
                    (block nil
                      (p-void-ctx (p-unshift @a 7) (p-caller-ctx (p-setf (p-aref @_ 0) 3)))))))
              (p-aref-argbox @a (p-cast-$ (p-backslash 0)))))
          (p-void-ctx
            (pl-is (p-scalar (p-string-concat "[" (p-join |$"| @a) "]"))
              "[7 3 1]"
              "holes passed to sub do not lose their position (aelem)"))
          (p-array-= @a (vector))
          (let ((_prev (p-array-last-index @a))) (p-set-array-length @a (1+ _prev)) _prev)
          (setf (p-aref @a 1) 1)
          (p-void-ctx
            (p-funcall-ref
              (lambda (&rest %_args)
                (let
                  ((@_ (p-flatten-args %_args))
                    (*pcl-current-package* "main")
                    (*pcl-caller-wantarray* *wantarray*))
                  (p-sub-frame
                    (block nil
                      (p-void-ctx (p-unshift @a 7) (p-caller-ctx (p-setf (p-aref @_ 0) 3)))))))
              (p-aref-argbox @a (p-cast-$ (p-backslash 0)))))
          (p-void-ctx
            (pl-is (p-scalar (p-string-concat "[" (p-join |$"| @a) "]"))
              "[7 3 1]"
              "holes passed to sub do not lose their position (aelem, mg)")))
        :next))))

(p-void-ctx
  (pl-fresh_perl_is "my @x;$x[0] = 1;shift @x;$x[22] = 1;$x[25] = 1;"
    ""
    (make-p-box (p-hash))
    "unshifting and growing an array initializes trailing elements"))

"We're included by lib/Tie/Array/std.t so we need to return something true"

