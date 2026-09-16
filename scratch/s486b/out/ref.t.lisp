;;; pcl: pipeline=v2 gen=v2-1460
(in-package :pcl)
(setf pcl::*pcl-pl2cl-path* #P"/home/bernt/pcl/.claude/worktrees/agent-a6c7caf694eb1f8c2/pl2cl")
(pcl::box-set pcl::$0 "ref.t")
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
(pcl:p-defpackage :BASEOBJ)
(pcl:p-defpackage :C)
(pcl:p-defpackage :LASTCHANCE)
(pcl:p-defpackage :MYHASH)
(pcl:p-defpackage :My)
(pcl:p-defpackage :|My::Foo|)
(pcl:p-defpackage :OBJ)
(pcl:p-defpackage :UNIVERSAL)
(pcl:p-defpackage :WHATEVER)
(pcl:p-defpackage :_B)
(pcl:p-defpackage :curly)
(pcl:p-defpackage :|do::not::overwrite|)
(pcl:p-defpackage :hassgropper)
(pcl:p-defpackage :larry)
(pcl:p-defpackage :moe)
(pcl:p-defpackage :re)
(pcl:p-defpackage :utf8)
(pcl:p-defpackage :x)

(pcl:p-declare-sub LASTCHANCE::pl-foo)
(pcl:p-declare-sub larry::pl-DESTROY)
(pcl:p-declare-sub curly::pl-DESTROY)
(pcl:p-declare-sub moe::pl-DESTROY)
(pcl:p-declare-sub A::pl-DESTROY)
(pcl:p-declare-sub _B::pl-new)
(pcl:p-declare-sub _B::pl-DESTROY)
(pcl:p-declare-sub x::pl-DESTROY)
(pcl:p-declare-sub C::pl-new)
(pcl:p-declare-sub C::pl-DESTROY)

(p-declare-sub pl-mysub2)
(p-declare-sub pl-PVBM)
(p-declare-sub pl-mysub)
(defvar $a (make-p-box nil))
(defvar $b (make-p-box nil))

;; Forward declarations for undeclared package globals
(p-defcell $BAR (make-p-box nil))
(p-defcell $BAZ (make-p-box nil))
(p-defcell $FOO (make-p-box nil))
(p-defcell $anonhash (make-p-box nil))
(p-defcell $anonhash2 (make-p-box nil))
(p-defcell $bar (make-p-box nil))
(p-defcell $baz (make-p-box nil))
(p-defcell $called (make-p-box nil))
(p-defcell $desc (make-p-box nil))
(p-defcell $dummy (make-p-box nil))
(p-defcell $foo (make-p-box nil))
(p-defcell $get_ref (make-p-box nil))
(p-defcell $i (make-p-box nil))
(p-defcell $obj (make-p-box nil))
(p-defcell $pviv (make-p-box nil))
(p-defcell $pvnv (make-p-box nil))
(p-defcell $ref (make-p-box nil))
(p-defcell $refref (make-p-box nil))
(p-defcell $str (make-p-box nil))
(p-defcell $subref (make-p-box nil))
(p-defcell $subrefref (make-p-box nil))
(p-defcell $test (make-p-box nil))
(p-defcell $type (make-p-box nil))
(p-defcell $uniobj (make-p-box nil))
(p-defcell $x (make-p-box nil))
(p-defcell $y (make-p-box nil))
(p-defcell $z (make-p-box nil))
(p-defcell %hash (make-hash-table :test 'equal))
(p-defcell %spring2 (make-hash-table :test 'equal))
(p-defcell %whatever (make-hash-table :test 'equal))
(p-defcell @a (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @array (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @ary (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @b (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @c (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @d (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @ref (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @refs (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @spring (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell main::$test__file__0 (make-p-box nil))
(p-defcell main::$x__file__1 (make-p-box nil))

(p-defcell $test__file__0 (make-p-box nil) :perl "$test" :why :spanning)
(p-defcell $called__file__2 (make-p-box nil) :perl "$called" :why :captured)
(p-defcell $x__file__1 (make-p-box nil) :perl "$x" :why :spanning)

;; BEGIN {

(p-BEGIN

  (p-set-current-package :main "main")

  ;; chdir 't' if -d 't'

  (p-if (p--d "t") (p-chdir "t"))

  ;; require './test.pl'

  (p-eval-always

    (p-require-file "./test.pl"))

  ;; set_up_inc( qw(. ../lib) )

  (p-set_up_inc (vector "." "../lib"))

)

(p-eval-always (p-note-inc "strict"))

(p-eval-always (p-note-inc "strict"))

(p-eval-always (p-note-inc "strict"))

(p-sub pl-mysub
  (&rest %_args)
  (:writes-args nil :captures ($called__file__2) :needs ())
  (p-args-body (block nil (p-post++ $called__file__2))))

(p-sub pl-mysub2
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-args-body (block nil (p-lc (p-shift @_)))))

(p-sub pl-PVBM (&rest %_args) (:needs ()) (p-args-body (block nil "foo")))

;;; package MYHASH
(p-defpackage :MYHASH)
(in-package :MYHASH)
(p-defclass plc-myhash () ())
(p-register-pkg-name :MYHASH "MYHASH")

(p-declare-sub pl-mymethod)
(p-declare-sub pl-DESTROY)
(defvar $a (make-p-box nil))
(defvar $b (make-p-box nil))

;; Forward declarations for undeclared package globals
(p-defcell $THIS (make-p-box nil))
(p-defcell $object (make-p-box nil))
(p-defcell $object2 (make-p-box nil))
(p-defcell $string (make-p-box nil))
(p-defcell @ARGS (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell main::$anonhash2 (make-p-box nil))


(p-eval-always (p-note-inc "warnings"))

(p-sub pl-mymethod
  (&rest %_args)
  (:writes-args nil :needs (:dynamic_scope.local :nonlocal_exit.die))
  (p-args-body
    (block nil
      (p-void-ctx
        ;; local($THIS, @ARGS) = @_
(let* ((pcl-local-rhs-0 (let ((*wantarray* t) (*p-in-list-assign-rhs* t)) @_)))
  (p-local-cell $THIS (make-p-box nil)
    (p-local-cell @ARGS (make-array 0 :adjustable t :fill-pointer 0)
      (p-list-= (vector $THIS @ARGS) pcl-local-rhs-0)
          (p-if (p-! (p-str-eq (p-ref $THIS) "MYHASH"))
            (p-die :loc
              "ref.t line 291"
              (p-. (p-. "Got a \"" (p-ref $THIS)) "\" instead of a MYHASH")))
          (main::pl-is (p-scalar (p-aref-argbox @ARGS 0)) "argument")
          (p-caller-ctx (main::pl-is (p-scalar (p-gethash-deref $THIS "FOO")) "BAR")))))))))

(p-eval-always (p-note-inc "warnings"))

(p-sub pl-DESTROY
  (&rest %_args)
  (:writes-args nil :needs (:nonlocal_exit.return))
  (p-args-body
    (block nil
      (p-void-ctx (p-if (p-! $string) (p-return))
        (main::pl-is $string "good")
        (p-caller-ctx (main::pl-isnt (p-scalar (p-ref (p-shift @_))) "HASH"))))))

;;; package OBJ
(p-defpackage :OBJ)
(in-package :OBJ)
(p-defclass plc-obj () ())
(p-register-pkg-name :OBJ "OBJ")

(defvar $a (make-p-box nil))
(defvar $b (make-p-box nil))

;; Forward declarations for undeclared package globals
(p-defcell @ISA (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell main::$object (make-p-box nil))


(p-eval-always (p-note-inc "warnings"))

;;; back to package main
(in-package :main)

(p-declare-sub BASEOBJ::pl-doit)
;; Forward declarations for undeclared package globals
(p-defcell $object (make-p-box nil))


(p-eval-always (p-note-inc "warnings"))

(p-sub BASEOBJ::pl-doit
  (&rest %_args)
  (:writes-args nil :needs (:dynamic_scope.local :nonlocal_exit.die))
  (p-args-body
    (block nil
      (p-void-ctx
        ;; local $ref = shift
(p-local-cell $ref (p-box-for-local (p-shift @_))
          (p-if (p-! (p-str-eq (p-ref $ref) "OBJ")) (p-die :loc "ref.t line 342" "Not an OBJ"))
          (p-caller-ctx (p-gethash-deref $ref (p-shift @_))))))))

;;; package UNIVERSAL
(p-defpackage :UNIVERSAL)
(in-package :UNIVERSAL)
(p-defclass plc-universal () ())
(p-register-pkg-name :UNIVERSAL "UNIVERSAL")

(defvar $a (make-p-box nil))
(defvar $b (make-p-box nil))

;; Forward declarations for undeclared package globals
(p-defcell @ISA (make-array 0 :adjustable t :fill-pointer 0))


;;; package LASTCHANCE
(p-defpackage :LASTCHANCE)
(in-package :LASTCHANCE)
(p-defclass plc-lastchance () ())
(p-register-pkg-name :LASTCHANCE "LASTCHANCE")

(p-declare-sub pl-foo)
(defvar $a (make-p-box nil))
(defvar $b (make-p-box nil))



(p-sub pl-foo
  (&rest %_args)
  (:writes-args t :needs ())
  (p-args-body (block nil (main::pl-is (p-scalar (p-aref-argbox @_ 1)) "works"))))

;;; package WHATEVER
(p-defpackage :WHATEVER)
(in-package :WHATEVER)
(p-defclass plc-whatever () ())
(p-register-pkg-name :WHATEVER "WHATEVER")

(defvar $a (make-p-box nil))
(defvar $b (make-p-box nil))



;;; back to package main
(in-package :main)

(p-declare-sub larry::pl-DESTROY)
(p-declare-sub curly::pl-DESTROY)
(p-declare-sub moe::pl-DESTROY)
;; Forward declarations for undeclared package globals
(p-defcell $curly (make-p-box nil))
(p-defcell $joe (make-p-box nil))
(p-defcell $larry (make-p-box nil))
(p-defcell $moe (make-p-box nil))
(p-defcell $var (make-p-box nil))
(p-defcell %larry (make-hash-table :test 'equal))
(p-defcell @baa (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @bar (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @baz (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @bzz (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @curly (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @foo (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @fuu (make-array 0 :adjustable t :fill-pointer 0))


(p-sub larry::pl-DESTROY (&rest %_args) (:writes-args nil :needs ()) (p-args-body (block nil)))

(p-sub curly::pl-DESTROY (&rest %_args) (:writes-args nil :needs ()) (p-args-body (block nil)))

(p-sub moe::pl-DESTROY (&rest %_args) (:writes-args nil :needs ()) (p-args-body (block nil)))

;;; back to package main
(in-package :main)



;;; package A
(p-defpackage :A)
(in-package :A)
(p-defclass plc-a () ())
(p-register-pkg-name :A "A")

(p-declare-sub pl-new)
(p-declare-sub pl-DESTROY)
(defvar $a (make-p-box nil))
(defvar $b (make-p-box nil))



(p-sub pl-new
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-args-body (block nil (p-bless (make-p-box (p-hash)) (p-shift @_)))))

(p-sub pl-DESTROY (&rest %_args) (:writes-args nil :needs ()) (p-args-body (block nil)))

;;; package _B
(p-defpackage :_B)
(in-package :_B)
(p-defclass plc-_b () ())
(p-register-pkg-name :_B "_B")

(p-declare-sub pl-new)
(p-declare-sub pl-DESTROY)
(defvar $a (make-p-box nil))
(defvar $b (make-p-box nil))



(p-sub pl-new
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-args-body (block nil (p-bless (make-p-box (p-hash)) (p-shift @_)))))

(p-sub pl-DESTROY
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-args-body (block nil (p-bless (p-shift @_) "A"))))

;;; back to package main
(in-package :main)

;; Forward declarations for undeclared package globals
(p-defcell $b__excl__0 (make-p-box nil) :perl "$b" :why :exception-global)


;;; back to package main
(in-package :main)

(p-declare-sub x::pl-DESTROY)
(p-declare-sub C::pl-new)
(p-declare-sub C::pl-DESTROY)
;; Forward declarations for undeclared package globals
(p-defcell $TODO (make-p-box nil))
(p-defcell $a1 (make-p-box nil))
(p-defcell $a2 (make-p-box nil))
(p-defcell $a3 (make-p-box nil))
(p-defcell $a4 (make-p-box nil))
(p-defcell $aref (make-p-box nil))
(p-defcell $c (make-p-box nil))
(p-defcell $code (make-p-box nil))
(p-defcell $error (make-p-box nil))
(p-defcell $expect (make-p-box nil))
(p-defcell $false (make-p-box nil))
(p-defcell $glob1 (make-p-box nil))
(p-defcell $glob2 (make-p-box nil))
(p-defcell $got (make-p-box nil))
(p-defcell $hushed (make-p-box nil))
(p-defcell $lexical (make-p-box nil))
(p-defcell $m (make-p-box nil))
(p-defcell $n (make-p-box nil))
(p-defcell $name (make-p-box nil))
(p-defcell $name1 (make-p-box nil))
(p-defcell $name2 (make-p-box nil))
(p-defcell $name8 (make-p-box nil))
(p-defcell $name_utf8 (make-p-box nil))
(p-defcell $obj0 (make-p-box nil))
(p-defcell $obj00 (make-p-box nil))
(p-defcell $obj1 (make-p-box nil))
(p-defcell $objnull (make-p-box nil))
(p-defcell $one (make-p-box nil))
(p-defcell $plain (make-p-box nil))
(p-defcell $pvbm (make-p-box nil))
(p-defcell $r (make-p-box nil))
(p-defcell $result (make-p-box nil))
(p-defcell $rpvbm (make-p-box nil))
(p-defcell $true (make-p-box nil))
(p-defcell $two (make-p-box nil))
(p-defcell $xs (make-p-box nil))
(p-defcell @exp (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell |do::not::overwrite|::$this (make-p-box nil))

(defvar C::$a (make-p-box nil))
(defvar C::$b (make-p-box nil))

(p-sub C::pl-new
  (&rest %_args)
  (:needs ())
  (p-args-body (block nil (p-bless (make-p-box (p-hash)) (p-shift @_)))))

(p-sub C::pl-DESTROY
  (&rest %_args)
  (:needs ())
  (p-args-body (block nil (p-setf (p-aref @_ 0) "foo"))))

(p-sub x::pl-DESTROY (&rest %_args) (:writes-args nil :needs ()) (p-args-body (block nil)))

(p-eval-always (p-note-inc "strict"))

(p-eval-always (p-note-inc "strict"))

(p-eval-always (p-note-inc "strict"))

(p-eval-always (p-note-inc "strict"))

(p-eval-always (p-note-inc "warnings"))

(p-eval-always (p-note-inc "builtin"))

(p-run-compile-phase-blocks)

(p-set-current-package :main "main")

(p-void-ctx (pl-plan 245))

(p-void-ctx
  (p-eval-block
    (p-die :loc
      "ref.t line 15"
      (p-.. 1 127)
      (p-scalar-= $_ (p-list-scalar (p-refgen-list (vector)))))))

(p-scalar-= $bar "one")

(p-scalar-= $foo "two")

(let ((*package* *package*))
  (block nil
    (tagbody :redo
      ;; local(*foo) = *bar
(let ((--local-glob-rhs--0 (p-make-typeglob "main" "bar")))
  (p-local-glob "main" "foo"
    (p-glob-assign "main" "foo" --local-glob-rhs--0)
        (p-void-ctx (pl-is $foo "one"))))
      :next)))

(p-void-ctx (pl-is $foo "two"))

(p-scalar-= $baz "three")

(p-scalar-= $foo "four")

(let ((*package* *package*))
  (block nil
    (tagbody :redo
      ;; local(*foo) = 'baz'
(let ((--local-glob-rhs--1 "baz"))
  (p-local-glob "main" "foo"
    (p-glob-assign "main" "foo" --local-glob-rhs--1)
        (p-void-ctx (pl-is $foo "three"))))
      :next)))

(p-void-ctx (pl-is $foo "four"))

(p-scalar-= $foo "global")

(let ((*package* *package*))
  (block nil
    (tagbody :redo
      ;; local(*foo)
(p-local-glob "main" "foo"
        (p-void-ctx (pl-is $foo (p-scalar (p-undef))))
        (p-scalar-= $foo "local")
        (p-void-ctx (pl-is $foo "local")))
      :next)))

(p-void-ctx (pl-is $foo "global"))

(let ((*package* *package*))
  (block nil
    (tagbody :redo
      (p-scalar-= $baz "valid")
      (p-scalar-= $bar "baz")
      (p-scalar-= $foo "bar")
      (p-void-ctx (pl-is (p-scalar (p-cast-$ (p-cast-$ $foo))) "valid"))
      :next)))

(p-scalar-= $FOO (p-backslash $BAR))

(p-scalar-= $BAR (p-backslash $BAZ))

(p-scalar-= $BAZ "hit")

(p-void-ctx (pl-is (p-scalar (p-cast-$ (p-cast-$ $FOO))) "hit"))

(p-scalar-= $test__file__0 (p-scalar-ctx (pl-curr_test)))

(p-alias-eval-cell '$test $test__file__0)

(p-array-= @ary
  (vector $test__file__0 (p-+ $test__file__0 1) (p-+ $test__file__0 2) (p-+ $test__file__0 3)))

(p-setf (p-aref @ref 0) (p-backslash @a))

(p-setf (p-aref @ref 1) (p-backslash @b))

(p-setf (p-aref @ref 2) (p-backslash @c))

(p-setf (p-aref @ref 3) (p-backslash @d))

(p-foreach ($i (vector 3 1 2 0))
  (p-push (p-cast-@ (p-aref-box @ref $i))
    (p-string-concat "ok " (p-aref @ary $i) (p-esc "\\n"))))

(p-print @a)

(p-print (p-aref-deref (p-viv-array-container (p-aref @ref 1)) 0))

(p-print (p-aslice (p-cast-@ (p-aref-box @ref 2)) 0))

(let ((*package* *package*))
  (block nil (tagbody :redo (p-print (p-cast-@ "d" (p-symref-site))) :next)))

(p-void-ctx (pl-curr_test (p-+ $test__file__0 4)))

(p-scalar-= $refref (p-backslash (p-backslash $x)))

(p-scalar-= $x "Good")

(p-void-ctx (pl-is (p-scalar (p-cast-$ (p-cast-$ $refref))) "Good"))

(p-scalar-= $ref
  (make-p-box
    (p-array-init (make-p-box (make-array 0 :adjustable t :fill-pointer 0))
      2
      (make-p-box (p-array-init 3 4 5)))))

(p-void-ctx (pl-is (p-scalar (p-scalar (p-cast-@ $ref))) 3))

(p-void-ctx (pl-is (p-scalar (p-aref-deref $ref 1)) 2))

(p-void-ctx (pl-is (p-scalar (p-aref-deref (p-viv-array-container (p-aref-deref $ref 2)) 2)) 5))

(p-void-ctx (pl-is (p-scalar (p-scalar (p-cast-@ (p-aref-deref-box $ref 0)))) 0))

(p-void-ctx (pl-is (p-scalar (p-aref-deref $ref 1)) 2))

(p-void-ctx (pl-is (p-scalar (p-aref-argbox (p-aref-deref-box $ref 2) 0)) 3))

(p-scalar-= $refref (p-backslash %whatever))

(p-setf (p-gethash-deref $refref "key") $ref)

(p-void-ctx
  (pl-is (p-scalar (p-aref-argbox (p-aref-argbox (p-gethash-deref-box $refref "key") 2) 0)) 3))

(p-setf (p-aref (p-viv-array-container (p-aref @spring 5)) 0) 123)

(p-setf (p-aref (p-viv-array-container (p-aref @spring 5)) 1) 456)

(p-push (p-cast-@ (p-aref-box @spring 5)) 789)

(p-void-ctx
  (pl-is (p-scalar (p-list-ctx (p-join ":" (p-cast-@ (p-aref-box @spring 5))))) "123:456:789"))

(p-array-deref-= (p-cast-@ (p-gethash-box %spring2 "foo")) (vector 1 2 3))

(p-setf (p-aref (p-viv-array-container (p-gethash %spring2 "foo")) 3) 4)

(p-void-ctx
  (pl-is (p-scalar (p-list-ctx (p-join ":" (p-cast-@ (p-gethash-box %spring2 "foo")))))
    "1:2:3:4"))

(let ((*package* *package*))
  (p-dyn-once
    (block nil
      (tagbody :redo
        (p-alias-eval-cell '$called $called__file__2)
        (p-scalar-= $subref (p-backslash-sub 'pl-mysub))
        (p-funcall-ref $subref @_)
        (p-void-ctx (pl-is $called__file__2 1))
        :next))))

(p-void-ctx
  (pl-is (p-scalar (p-ref (p-scalar-ctx (p-eval-block (p-backslash-sub-ref "")))))
    "CODE"
    "reference to &{\"\"} [perl #94476]"))

(p-delete (p-stash "My") "Foo::")

(p-void-ctx
  (pl-is (p-scalar (p-ref (p-backslash-sub '|My::Foo|::pl-foo)))
    "CODE"
    "creating stub with \\&deleted_stash::foo [perl #128532]"))

(let ((*package* *package*))
  (block nil
    (tagbody :redo
      (p-let ((@refs :array (make-array 0 :adjustable t :fill-pointer 0)))
        (p-foreach ($_ (vector "a" "b")) (p-push @refs (p-backslash (p-string-concat $_))))
        (p-void-ctx
          (pl-is (p-scalar (p-list-ctx (p-join " " (p-map (lambda ($_) (p-cast-$ $_)) @refs))))
            "a b"
            "refgen+PADTMP")))
      :next)))

(p-scalar-= $subrefref (p-backslash (p-backslash-sub 'pl-mysub2)))

(p-void-ctx
  (pl-is (p-scalar (p-scalar-ctx (p-funcall-ref (p-cast-$ $subrefref) "GOOD"))) "good"))

(let ((*package* *package*))
  (block SKIP
    (catch (pcl::%pcl-loop-tag "LAST" 'SKIP)
      (block nil
        (catch (pcl::%pcl-loop-tag "NEXT" 'SKIP)
          (tagbody :redo
            (catch (pcl::%pcl-loop-tag "REDO" 'SKIP)
              (progn
                (p-void-ctx
                  (pl-skip_if_miniperl "no dynamic loading on miniperl, so can't load re" 5))
                ;; require re (pragma)
(p-note-inc "re")
                (p-let (($x :box (make-p-box nil)))
                  (p-my-= $x (pcl::p-qr :pat "x" :flags "" :tier :native))
                  (p-let (($str :box (make-p-box nil)))
                    (p-my-= $str (p-string-concat $x))
                    (p-let (($y :box (make-p-box nil)))
                      (p-my-= $y (p-cast-$ $x))
                      (p-void-ctx (pl-is $y $str "bare REGEXP stringifies correctly"))
                      (p-void-ctx
                        (pl-ok (p-scalar (p-scalar-ctx (p-eval-block (p-=~ "x" $y))))
                          "bare REGEXP matches correctly"))
                      (p-let (($z :box (make-p-box nil)))
                        (p-my-= $z (p-backslash $y))
                        (p-void-ctx
                          (pl-ok (p-scalar (p-scalar-ctx (re::pl-is_regexp $z)))
                            "new ref to REXEXP passes is_regexp"))
                        (p-void-ctx (pl-is $z $str "new ref to REGEXP stringifies correctly"))
                        (p-void-ctx
                          (pl-ok (p-scalar (p-scalar-ctx (p-eval-block (p-=~ "x" $z))))
                            "new ref to REGEXP matches correctly"))))))
                (go :next)))
            (go :redo)
            :next))))))

(let ((*package* *package*))
  (block nil
    (tagbody :redo
      (p-let (($x :box (make-p-box nil)) ($str :box (make-p-box nil)))
        (let ((*package* *package*))
          (block nil
            (tagbody :redo
              (p-let (($y :box (make-p-box nil)))
                (p-my-= $y (pcl::p-qr :pat "x" :flags "" :tier :native))
                (p-my-= $str (p-string-concat $y))
                (p-my-= $x (p-cast-$ $y)))
              :next)))
        (p-void-ctx (pl-is $x $str "REGEXP keeps a ref to its mother_re"))
        (p-void-ctx
          (pl-ok (p-scalar (p-scalar-ctx (p-eval-block (p-=~ "x" $x))))
            "REGEXP with mother_re still matches")))
      :next)))

(let ((*package* *package*))
  (p-dyn-once
    (block nil
      (tagbody :redo
        (p-let (($ref :box (make-p-box nil)))
          (p-foreach
            ($ref
              (p-flatten-args
                (list
                  (p-glob-slot (p-make-typeglob "main" "STDOUT") "IO")
                  (p-glob-slot (p-make-typeglob "main" "STDERR") "FORMAT"))))
            :dyn
            t
            (p-eval " $$ref "
              (list
                (cons "$ref" $ref)
                (cons "$test" main::$test__file__0)
                (cons "$x" main::$x__file__1)))
            (p-void-ctx
              (pl-like $@
                (p-scalar (pcl::p-qr :pat "Not a SCALAR reference" :flags "" :tier :native))
                "Scalar dereference"))
            (p-eval " @$ref "
              (list
                (cons "$ref" $ref)
                (cons "$test" main::$test__file__0)
                (cons "$x" main::$x__file__1)))
            (p-void-ctx
              (pl-like $@
                (p-scalar (pcl::p-qr :pat "Not an ARRAY reference" :flags "" :tier :native))
                "Array dereference"))
            (p-eval " %$ref "
              (list
                (cons "$ref" $ref)
                (cons "$test" main::$test__file__0)
                (cons "$x" main::$x__file__1)))
            (p-void-ctx
              (pl-like $@
                (p-scalar (pcl::p-qr :pat "Not a HASH reference" :flags "" :tier :native))
                "Hash dereference"))
            (p-eval " &$ref "
              (list
                (cons "$ref" $ref)
                (cons "$test" main::$test__file__0)
                (cons "$x" main::$x__file__1)))
            (p-void-ctx
              (pl-like $@
                (p-scalar (pcl::p-qr :pat "Not a CODE reference" :flags "" :tier :native))
                "Code dereference")))
          (p-my-= $ref (p-glob-slot (p-make-typeglob "main" "STDERR") "FORMAT"))
          (p-eval " *$ref "
            (list
              (cons "$ref" $ref)
              (cons "$test" main::$test__file__0)
              (cons "$x" main::$x__file__1)))
          (p-void-ctx
            (pl-like $@
              (p-scalar (pcl::p-qr :pat "Not a GLOB reference" :flags "" :tier :native))
              "Glob dereference"))
          (p-my-= $ref (p-glob-slot (p-make-typeglob "main" "STDOUT") "IO"))
          (p-eval " *$ref "
            (list
              (cons "$ref" $ref)
              (cons "$test" main::$test__file__0)
              (cons "$x" main::$x__file__1)))
          (p-void-ctx (pl-is $@ "" "Glob dereference of PVIO is acceptable"))
          (p-void-ctx
            (pl-is $ref
              (p-scalar (p-glob-slot (p-dynamic-typeglob $ref) "IO"))
              "IO slot of the temporary glob is set correctly")))
        :next))))

(let ((*package* *package*))
  (block nil
    (tagbody :redo
      (p-let (($dummy :box (make-p-box nil)))
        (p-my-= $dummy (p-index "foo" (p-scalar-ctx (pl-PVBM)))))
      :next)))

(p-let (($pviv :box (make-p-box nil)))
  (p-my-= $pviv 1)
  (p-string-concat $pviv)
  (p-let (($pvnv :box (make-p-box nil)))
    (p-my-= $pvnv 1.0)
    (p-string-concat $pvnv)
    (p-alias-eval-cell '$x $x__file__1)
    (p-foreach
      ($_
        (p-flatten-args
          (list
            (make-p-box (p-array-init "undef" "SCALAR" (p-backslash (p-undef))))
            (make-p-box (p-array-init "constant IV" "SCALAR" (p-backslash 1)))
            (make-p-box (p-array-init "constant NV" "SCALAR" (p-backslash 1.0)))
            (make-p-box (p-array-init "constant PV" "SCALAR" (p-backslash "f")))
            (make-p-box (p-array-init "scalar" "SCALAR" (p-backslash $x__file__1)))
            (make-p-box (p-array-init "PVIV" "SCALAR" (p-backslash $pviv)))
            (make-p-box (p-array-init "PVNV" "SCALAR" (p-backslash $pvnv)))
            (make-p-box (p-array-init "PVMG" "SCALAR" (p-backslash $0)))
            (make-p-box (p-array-init "PVBM" "SCALAR" (p-backslash (p-scalar-ctx (pl-PVBM)))))
            (make-p-box (p-array-init "scalar @array" "SCALAR" (p-backslash (p-scalar @array))))
            (make-p-box (p-array-init "scalar %hash" "SCALAR" (p-backslash (p-scalar %hash))))
            (make-p-box (p-array-init "vstring" "VSTRING" (p-backslash (p-version-string 1))))
            (make-p-box (p-array-init "ref" "REF" (p-backslash (p-backslash 1))))
            (make-p-box (p-array-init "substr lvalue" "LVALUE" (p-substr-ref $x__file__1 0 0)))
            (make-p-box (p-array-init "pos lvalue" "LVALUE" (p-pos-ref $_)))
            (make-p-box (p-array-init "vec lvalue" "LVALUE" (p-vec-ref $x__file__1 0 1)))
            (make-p-box (p-array-init "named array" "ARRAY" (p-backslash @ary)))
            (make-p-box (p-array-init "anon array" "ARRAY" (make-p-box (p-array-init 1))))
            (make-p-box (p-array-init "named hash" "HASH" (p-backslash %whatever)))
            (make-p-box (p-array-init "anon hash" "HASH" (make-p-box (p-hash "a" 1))))
            (make-p-box (p-array-init "named sub" "CODE" (p-backslash-sub 'pl-mysub)))
            (make-p-box
              (p-array-init "anon sub"
                "CODE"
                (lambda (&rest %_args)
                  (let
                    ((@_ (p-flatten-args %_args))
                      (*pcl-current-package* "main")
                      (*pcl-caller-wantarray* *wantarray*))
                    (p-sub-frame (block nil 1))))))
            (make-p-box
              (p-array-init "glob" "GLOB" (p-backslash (p-make-typeglob "main" "foo"))))
            (make-p-box
              (p-array-init "format"
                "FORMAT"
                (p-glob-slot (p-make-typeglob "main" "STDERR") "FORMAT"))))))
      (p-let
        (($desc :box (make-p-box nil))
          ($type :box (make-p-box nil))
          ($ref :box (make-p-box nil)))
        (p-scalar-ctx (p-list-= (vector $desc $type $ref) (p-cast-@ $_)))
        (p-void-ctx
          (pl-is (p-scalar (p-ref $ref)) $type (p-string-concat "ref() for ref to " $desc)))
        (p-void-ctx
          (pl-like (p-scalar (p-string-concat $ref))
            (p-scalar
              (pcl::p-regex-from-parts :pat (p-string-concat "^" $type "\\(0x[0-9a-f]+\\)$")
                :flags ""
                :tier :dynamic))
            (p-string-concat "stringify for ref to " $desc)))))
    (p-void-ctx
      (pl-is (p-scalar (p-ref (p-glob-slot (p-make-typeglob "main" "STDOUT") "IO")))
        "IO::File"
        "IO refs are blessed into IO::File"))
    (p-void-ctx
      (pl-like (p-scalar (p-glob-slot (p-make-typeglob "main" "STDOUT") "IO"))
        (p-scalar (pcl::p-qr :pat "^IO::File=IO\\(0x[0-9a-f]+\\)$" :flags "" :tier :native))
        "stringify for IO refs"))
    (let ((*package* *package*))
      (p-dyn-once
        (block nil
          (tagbody :redo
            (p-let (($obj :box (make-p-box nil)))
              (p-my-= $obj
                (p-bless (make-p-box (make-array 0 :adjustable t :fill-pointer 0)) "____"))
              (p-let (($uniobj :box (make-p-box nil)))
                (p-my-= $uniobj
                  (p-bless (make-p-box (make-array 0 :adjustable t :fill-pointer 0))
                    (p-chr 256)))
                (p-let (($get_ref :box (make-p-box nil)))
                  (p-my-= $get_ref
                    (lambda (&rest %_args)
                      (let
                        ((@_ (p-flatten-args %_args))
                          (*pcl-current-package* "main")
                          (*pcl-caller-wantarray* *wantarray*))
                        (p-sub-frame (block nil (p-ref (p-shift @_)))))))
                  (p-let (($dummy :box (make-p-box nil)))
                    (p-my-= $dummy (p-scalar-ctx (p-funcall-ref $get_ref $uniobj)))
                    (p-my-= $dummy (p-scalar-ctx (p-funcall-ref $get_ref $obj)))
                    (p-void-ctx
                      (pl-ok
                        (p-scalar
                          (p-exists (unbox (make-p-box (p-hash "____" (p-list-ctx (p-undef)))))
                            $dummy))
                        "ref sets UTF8 flag correctly"))))))
            :next))))
    (p-scalar-= $anonhash (make-p-box (p-hash)))
    (p-void-ctx (pl-is (p-scalar (p-ref $anonhash)) "HASH"))
    (p-scalar-= $anonhash (make-p-box (p-hash "one")))
    (p-void-ctx
      (pl-is (p-scalar (p-scalar (p-keys (p-cast-% $anonhash))))
        1
        "single value in anonhash creates a key (count)"))
    (p-void-ctx
      (pl-ok (p-scalar (p-exists (unbox $anonhash) "one"))
        "single value in anonhash creates a key (existence)"))
    (p-void-ctx
      (pl-is (p-scalar (p-gethash-deref $anonhash "one"))
        (p-scalar (p-undef))
        "single value in anonhash creates a key (value)"))
    (p-scalar-= $anonhash2 (make-p-box (p-hash "FOO" "BAR" "ABC" "XYZ")))
    (p-void-ctx
      (pl-is
        (p-scalar
          (p-list-ctx
            (p-join ""
              (p-list-ctx
                (%p-sort-classic :default (p-list-ctx (p-values (p-cast-% $anonhash2))))))))
        "BARXYZ"))))

(in-package :MYHASH)

(p-set-current-package :MYHASH "MYHASH")

(let ((*package* *package*))
  (block nil (tagbody :redo (p-scalar-= $object (p-bless main::$anonhash2 "MYHASH")) :next)))

(p-void-ctx (main::pl-is (p-scalar (p-ref $object)) "MYHASH"))

(p-void-ctx (main::pl-is (p-scalar (p-gethash-deref $object "ABC")) "XYZ"))

(p-scalar-= $object2 (p-bless (make-p-box (p-hash)) "MYHASH"))

(p-void-ctx (main::pl-is (p-scalar (p-ref $object2)) "MYHASH"))

(p-void-ctx (MYHASH::pl-mymethod $object "argument"))

(p-scalar-= $string "bad")

(p-scalar-= $object "foo")

(p-scalar-= $string "good")

(let ((*package* *package*))
  (block nil (tagbody :redo (p-scalar-= main::$anonhash2 "foo") :next)))

(p-scalar-= $string "")

(in-package :OBJ)

(p-set-current-package :OBJ "OBJ")

(p-array-= @ISA (vector "BASEOBJ"))

(let ((*package* *package*))
  (block nil
    (tagbody :redo
      (p-scalar-= main::$object (p-bless (make-p-box (p-hash "FOO" "foo" "BAR" "bar")) "OBJ"))
      :next)))

(in-package :main)

(p-set-current-package :main "main")

(p-void-ctx (pl-is (p-scalar (p-scalar-ctx (p-method-call $object "doit" "BAR"))) "bar"))

(progn ;; PARSE ERROR: Bug. Fell through. Missing case: ['Token::Symbol<$object>','Token::Quote::Double<"FOO">']
 (pcl:p-die (p-esc "PCL: statement not supported at ref.t line 335: $foo = doit $object \"FOO\"; -- Bug. Fell through. Missing case: ['Token::Symbol<$object>','Token::Quote::Double<\"FOO\">']\\n")))

(p-void-ctx (main::pl-is $foo "foo"))

(let ((*package* *package*)) (block nil (tagbody :redo :next)))

(in-package :UNIVERSAL)

(p-set-current-package :UNIVERSAL "UNIVERSAL")

(p-array-= @ISA "LASTCHANCE")

(in-package :LASTCHANCE)

(p-set-current-package :LASTCHANCE "LASTCHANCE")

(in-package :WHATEVER)

(p-set-current-package :WHATEVER "WHATEVER")

(p-void-ctx (p-method-call "WHATEVER" "foo" "works"))

(in-package :main)

(p-set-current-package :main "main")

(p-array-= @foo (p-refgen-list (p-.. 1 3)))

(p-array-= @bar (p-refgen-list @foo))

(p-array-= @baz (vector (p-backslash 1) (p-backslash @foo) (p-backslash @bar)))

(p-void-ctx (pl-is (p-scalar (p-scalar @bar)) 3))

(p-void-ctx (pl-is (p-scalar (p-scalar (p-grep (lambda ($_) (p-ref $_)) @bar))) 3))

(p-void-ctx (pl-is (p-scalar (p-scalar @baz)) 3))

(p-let ((@fuu :array (make-array 0 :adjustable t :fill-pointer 0)))
  (p-array-= @fuu
    (let ((|--pcl-bsl-r1--| (make-array 4 :adjustable t :fill-pointer 0)))
      (p-vector-append |--pcl-bsl-r1--| (p-refgen-list (p-.. 1 2)))
      (vector-push-extend (p-backslash 3) |--pcl-bsl-r1--|)
      |--pcl-bsl-r1--|))
  (p-let ((@baa :array (make-array 0 :adjustable t :fill-pointer 0)))
    (p-array-= @baa (p-refgen-list @fuu))
    (p-let ((@bzz :array (make-array 0 :adjustable t :fill-pointer 0)))
      (p-array-= @bzz (vector (p-backslash 1) (p-backslash @fuu) (p-backslash @baa)))
      (p-void-ctx (pl-is (p-scalar (p-scalar @baa)) 3))
      (p-void-ctx (pl-is (p-scalar (p-scalar (p-grep (lambda ($_) (p-ref $_)) @baa))) 3))
      (p-void-ctx (pl-is (p-scalar (p-scalar @bzz)) 3))
      (p-eval "\\($x, $y) = (1, 2);"
        (list
          (cons "@baa" @baa)
          (cons "@bzz" @bzz)
          (cons "@fuu" @fuu)
          (cons "$test" main::$test__file__0)
          (cons "$x" main::$x__file__1)))
      (p-void-ctx
        (pl-like $@
          (p-scalar
            (pcl::p-qr :pat (p-esc
                "Can\\\\'t modify.*ref.*in.*assignment(?x:\\n           )|Experimental aliasing via reference not enabled")
              :flags ""
              :tier :native))))
      (p-scalar-= main::$test__file__0 (p-scalar-ctx (pl-curr_test)))
      (let ((*package* *package*))
        (block nil
          (tagbody :redo
            (p-let
              (($joe :box (make-p-box nil))
                (@curly :array (make-array 0 :adjustable t :fill-pointer 0))
                (%larry :hash (make-hash-table :test 'equal)))
              (p-let (($moe :box (make-p-box nil)))
                (p-my-= $moe (p-bless (p-backslash $joe) "moe"))
                (p-let (($curly :box (make-p-box nil)))
                  (p-my-= $curly (p-bless (p-backslash @curly) "curly"))
                  (p-let (($larry :box (make-p-box nil)))
                    (p-my-= $larry (p-bless (p-backslash %larry) "larry"))
                    (p-print (p-esc "# leaving block\\n"))))))
            :next)))
      (p-print (p-esc "# left block\\n"))
      (p-scalar-= $foo "garbage")
      (let ((*package* *package*))
        (block nil
          (tagbody :redo
            ;; local(*bar) = "foo"
(let ((--local-glob-rhs--2 "foo"))
  (p-local-glob "main" "bar"
    (p-glob-assign "main" "bar" --local-glob-rhs--2)))
            :next)))
      (p-scalar-= $bar "glob 3")
      ;; local(*bar) = *bar
(let ((--local-glob-rhs--3 (p-make-typeglob "main" "bar")))
  (p-local-glob "main" "bar"
    (p-glob-assign "main" "bar" --local-glob-rhs--3)
        (p-void-ctx (pl-is $bar "glob 3"))
        (p-scalar-= $var "glob 4")
        (p-scalar-= $_ (p-backslash $var))
        (p-void-ctx (pl-is (p-scalar (p-cast-$ $_)) "glob 4"))
        (p-scalar-= main::$test__file__0 (p-scalar-ctx (pl-curr_test))))))))

(p-set-current-package :main "main")

(in-package :A)

(p-set-current-package :A "A")

(in-package :_B)

(p-set-current-package :_B "_B")

(in-package :main)

(p-set-current-package :main "main")

(p-let (($b__excl__0 :box (make-p-box nil) :perl "$b" :why :exception-global))
  (p-my-= $b__excl__0 (p-scalar-ctx (p-method-call "_B" "new"))))

(p-set-current-package :main "main")

(let ((*package* *package*))
  (block nil
    (tagbody :redo
      (p-let (($test :box (make-p-box nil) :captured t))
        (p-my-= $test (p-scalar-ctx (pl-curr_test)))
        (p-let (($i :box (make-p-box nil) :captured t))
          (p-my-= $i 0)
          ;; local $SIG{'__DIE__'} = sub { 	my $m = shift; 	if ($i++ > 4) { 	    print "# infinite recursion, bailing\nnot ok $test\n"; 	    exit 1;         } 	like ($m, qr/^Modification of a read-only/);     }
(p-local-hash-elem-init %SIG "__DIE__" (lambda (&rest %_args) (let ((@_ (p-flatten-args %_args)) (*pcl-current-package* "main") (*pcl-caller-wantarray* *wantarray*)) (p-sub-frame (block nil (p-void-ctx (p-let (($m :box (make-p-box nil))) (p-my-= $m (p-shift @_)) (p-if (p-> (p-post++ $i) 4) (progn (p-print (p-string-concat (p-esc "# infinite recursion, bailing\\nnot ok ") $test (p-esc "\\n"))) (p-exit 1))) (p-caller-ctx (pl-like $m (p-scalar (pcl::p-qr :pat "^Modification of a read-only" :flags "" :tier :native))))))))))
            (p-defpackage :C)
            (defclass C::plc-c () ())
            (p-set-current-package :C "C")
            (let ((*package* *package*))
              (block nil
                (tagbody :redo
                  (p-print (p-esc "# should generate an error...\\n"))
                  (p-let (($c :box (make-p-box nil)))
                    (p-my-= $c (p-scalar-ctx (p-method-call "C" "new"))))
                  :next)))
            (p-print (p-esc "# good, didn't recurse\\n"))
            (p-set-current-package :main "main"))))
      :next)))

(p-void-ctx
  (pl-is
    (p-scalar
      (p-scalar-ctx
        (pl-runperl "stderr" 1 "prog" "sub DESTROY { print qq-aaa\\n- } bless \\$a[0]")))
    (p-esc "aaa\\n")
    "DESTROY called on array elem"))

(p-void-ctx
  (pl-is
    (p-scalar
      (p-scalar-ctx
        (pl-runperl "stderr"
          1
          "prog"
          "{ bless \\my@x; *a=sub{@x}}sub DESTROY { print qq-aaa\\n- }")))
    (p-esc "aaa\\n")
    "DESTROY called on closure variable"))

(p-void-ctx
  (pl-fresh_perl_is "bless \\%foo::, bar::; bless \\%bar::, foo::; print \"ok\\n\""
    (p-esc "ok\\n")
    (make-p-box (p-hash "stderr" 1))
    "no double free when stashes are blessed into each other"))

(let ((*package* *package*))
  (block nil
    (tagbody :redo
      (p-let ((@a :array (make-array 0 :adjustable t :fill-pointer 0)))
        (setf (p-aref @a 1) "good")
        (p-let (($got :scalar (p-undef)))
          (p-foreach ($_ @a) (p-.=-raw $got (p-cast-$ (p-backslash $_))) (p-.=-raw $got ";"))
          (p-void-ctx (pl-is $got ";good;"))))
      :next)))

(p-scalar-= $a (make-p-box (p-array-init 1 2 3)))

(p-scalar-= $a (p-aref-deref $a 1))

(p-void-ctx (pl-is $a 2))

(p-foreach-raw ($lexical (vector "" "my $a; "))
  :my
  t
  (p-let (($expect :scalar (p-esc "pass\\n")))
    (p-let (($result :box (make-p-box nil)))
      (p-my-= $result
        (p-scalar-ctx
          (pl-runperl "switches"
            (make-p-box (p-array-init "-wl"))
            "stderr"
            1
            "prog"
            (p-. $lexical "BEGIN {$a = \\q{pass}}; $a = $$a; print $a"))))
      (p-void-ctx (pl-is $? 0))
      (p-void-ctx (pl-is $result $expect)))))

(p-scalar-= main::$test__file__0 (p-scalar-ctx (pl-curr_test)))

(let ((*package* *package*))
  (block nil
    (tagbody :redo
      (p-let (($a1 :box (make-p-box nil)))
        (p-my-= $a1 (p-bless (make-p-box (p-array-init 3)) "x"))
        (p-let (($a2 :box (make-p-box nil)))
          (p-my-= $a2 (p-bless (make-p-box (p-array-init 2)) "x"))
          (let ((*package* *package*))
            (block nil
              (tagbody :redo
                (p-let (($a3 :box (make-p-box nil)))
                  (p-my-= $a3 (p-bless (make-p-box (p-array-init 1)) "x"))
                  (p-let (($a4 :box (make-p-box nil)))
                    (p-my-= $a4 (p-bless (make-p-box (p-array-init 0)) "x"))
                    567))
                :next)))))
      :next)))

(p-void-ctx
  (pl-is
    (p-scalar
      (p-scalar-ctx
        (pl-runperl "switches"
          (make-p-box (p-array-init "-l"))
          "prog"
          "print 1; print qq-*$\\*-;print 1;")))
    (p-esc "1\\n*\\n*\\n1\\n")))

(p-void-ctx (pl-runperl "prog" "sub UNIVERSAL::AUTOLOAD { qr// } a->p"))

(p-void-ctx (pl-is $? 0 "UNIVERSAL::AUTOLOAD called when freeing qr//"))

(p-void-ctx (pl-runperl "prog" "sub UNIVERSAL::DESTROY { warn } bless \\$a, A" "stderr" 1))

(p-void-ctx (pl-is $? 0 "warn called inside UNIVERSAL::DESTROY"))

(p-void-ctx (pl-runperl "prog" "sub f { my $x = shift; *z = $x; } f({}); f();"))

(p-void-ctx (pl-is $? 0 "coredump on typeglob = (SvRV && !SvROK)"))

(p-void-ctx
  (pl-is
    (p-scalar
      (p-scalar-ctx
        (pl-runperl "prog" "use Symbol;my $x=bless \\gensym,q{t}; print;*$$x=$x" "stderr" 1)))
    ""
    "freeing self-referential typeglob"))

(let ((*package* *package*))
  (block TODO
    (catch (pcl::%pcl-loop-tag "LAST" 'TODO)
      (block nil
        (catch (pcl::%pcl-loop-tag "NEXT" 'TODO)
          (tagbody :redo
            (catch (pcl::%pcl-loop-tag "REDO" 'TODO)
              (progn
                ;; local $TODO = "works but output through pipe is mangled" if $^O eq 'VMS'
(let* ((pcl-local-cond-1 (p-true-p (p-str-eq |$^O| "VMS"))))
  (p-local-cell-if pcl-local-cond-1 $TODO (p-box-for-local "works but output through pipe is mangled")
                  (p-void-ctx
                    (pl-like
                      (p-scalar
                        (p-scalar-ctx
                          (pl-runperl "prog"
                            "$x=bless[]; sub IO::Handle::DESTROY{$_=q{bad};s/bad/ok/;print}"
                            "stderr"
                            1)))
                      (p-scalar (pcl::p-qr :pat "^(ok)+$" :flags "" :tier :native))
                      "STDOUT destructor"))))
                (go :next)))
            (go :redo)
            :next))))))

(let ((*package* *package*))
  (block nil
    (tagbody :redo
      (p-scalar-= $name8 (p-chr 163))
      (p-scalar-= $name_utf8 (p-. $name8 (p-chr 256)))
      (p-chop $name_utf8)
      (p-void-ctx
        (pl-is (p-scalar (p-cast-$ $name8)) (p-scalar (p-undef)) "Nothing before we start"))
      (p-void-ctx
        (pl-is (p-scalar (p-cast-$ $name_utf8)) (p-scalar (p-undef)) "Nothing before we start"))
      (p-setf (p-cast-$ $name8) "Pound")
      (p-void-ctx
        (pl-is (p-scalar (p-cast-$ $name8)) "Pound" "Accessing via 8 bit symref works"))
      (p-void-ctx
        (pl-is (p-scalar (p-cast-$ $name_utf8)) "Pound" "Accessing via UTF8 symref works"))
      :next)))

(let ((*package* *package*))
  (block nil
    (tagbody :redo
      (p-scalar-= $name_utf8 (p-scalar-= $name (p-chr 9787)))
      (p-void-ctx (utf8::pl-encode $name_utf8))
      (p-void-ctx (pl-is (p-scalar (p-length $name)) 1 "Name is 1 char"))
      (p-void-ctx (pl-is (p-scalar (p-length $name_utf8)) 3 "UTF8 representation is 3 chars"))
      (p-void-ctx
        (pl-is (p-scalar (p-cast-$ $name)) (p-scalar (p-undef)) "Nothing before we start"))
      (p-void-ctx
        (pl-is (p-scalar (p-cast-$ $name_utf8)) (p-scalar (p-undef)) "Nothing before we start"))
      (p-setf (p-cast-$ $name) "Face")
      (p-void-ctx
        (pl-is (p-scalar (p-cast-$ $name)) "Face" "Accessing via Unicode symref works"))
      (p-void-ctx
        (pl-is (p-scalar (p-cast-$ $name_utf8))
          (p-scalar (p-undef))
          "Accessing via the UTF8 byte sequence gives nothing"))
      :next)))

(let ((*package* *package*))
  (p-dyn-once
    (block nil
      (tagbody :redo
        (p-scalar-= $name1 (p-esc "\\u0000Chalk"))
        (p-scalar-= $name2 (p-esc "\\u0000Cheese"))
        (p-void-ctx (pl-isnt $name1 $name2 "They differ"))
        (p-void-ctx
          (pl-is (p-scalar (p-cast-$ $name1))
            (p-scalar (p-undef))
            "Nothing before we start (scalars)"))
        (p-void-ctx
          (pl-is (p-scalar (p-cast-$ $name2)) (p-scalar (p-undef)) "Nothing before we start"))
        (p-setf (p-cast-$ $name1) "Yummy")
        (p-void-ctx
          (pl-is (p-scalar (p-cast-$ $name1)) "Yummy" "Accessing via the correct name works"))
        (p-void-ctx
          (pl-is (p-scalar (p-cast-$ $name2))
            (p-scalar (p-undef))
            "Accessing via a different NUL-containing name gives nothing"))
        (p-void-ctx
          (pl-ok (p-scalar (p-defined (p-cast-$ $name1))) "defined via the correct name works"))
        (p-void-ctx
          (pl-ok (p-scalar (p-! (p-defined (p-cast-$ $name2))))
            "defined via a different NUL-containing name gives nothing"))
        (p-void-ctx
          (pl-is (p-scalar (p-aref-deref $name1 0))
            (p-scalar (p-undef))
            "Nothing before we start (arrays)"))
        (p-void-ctx
          (pl-is (p-scalar (p-aref-deref $name2 0))
            (p-scalar (p-undef))
            "Nothing before we start"))
        (p-setf (p-aref-deref $name1 0) "Yummy")
        (p-void-ctx
          (pl-is (p-scalar (p-aref-deref $name1 0))
            "Yummy"
            "Accessing via the correct name works"))
        (p-void-ctx
          (pl-is (p-scalar (p-aref-deref $name2 0))
            (p-scalar (p-undef))
            "Accessing via a different NUL-containing name gives nothing"))
        (p-void-ctx
          (pl-ok (p-scalar (p-defined (p-aref-deref $name1 0)))
            "defined via the correct name works"))
        (p-void-ctx
          (pl-ok (p-scalar (p-! (p-defined (p-aref-deref $name2 0))))
            "defined via a different NUL-containing name gives nothing"))
        (p-let (($one :box (make-p-box nil)))
          (p-scalar-ctx
            (p-list-= (vector (p-list-ctx (p-undef)) $one) (p-aslice (p-cast-@ $name1) 2 3)))
          (p-let (($two :box (make-p-box nil)))
            (p-scalar-ctx
              (p-list-= (vector (p-list-ctx (p-undef)) $two) (p-aslice (p-cast-@ $name2) 2 3)))
            (p-void-ctx
              (pl-is $one (p-scalar (p-undef)) "Nothing before we start (array slices)"))
            (p-void-ctx (pl-is $two (p-scalar (p-undef)) "Nothing before we start"))
            (p-setf (p-aslice (p-cast-@ $name1) 2 3) (vector "Very" "Yummy"))
            (p-list-= (vector (p-list-ctx (p-undef)) $one) (p-aslice (p-cast-@ $name1) 2 3))
            (p-list-= (vector (p-list-ctx (p-undef)) $two) (p-aslice (p-cast-@ $name2) 2 3))
            (p-void-ctx (pl-is $one "Yummy" "Accessing via the correct name works"))
            (p-void-ctx
              (pl-is $two
                (p-scalar (p-undef))
                "Accessing via a different NUL-containing name gives nothing"))
            (p-void-ctx (pl-ok (p-scalar (p-defined $one)) "defined via the correct name works"))
            (p-void-ctx
              (pl-ok (p-scalar (p-! (p-defined $two)))
                "defined via a different NUL-containing name gives nothing"))
            (p-void-ctx
              (pl-is (p-scalar (p-gethash-deref $name1 "PWOF"))
                (p-scalar (p-undef))
                "Nothing before we start (hashes)"))
            (p-void-ctx
              (pl-is (p-scalar (p-gethash-deref $name2 "PWOF"))
                (p-scalar (p-undef))
                "Nothing before we start"))
            (p-setf (p-gethash-deref $name1 "PWOF") "Yummy")
            (p-void-ctx
              (pl-is (p-scalar (p-gethash-deref $name1 "PWOF"))
                "Yummy"
                "Accessing via the correct name works"))
            (p-void-ctx
              (pl-is (p-scalar (p-gethash-deref $name2 "PWOF"))
                (p-scalar (p-undef))
                "Accessing via a different NUL-containing name gives nothing"))
            (p-void-ctx
              (pl-ok (p-scalar (p-defined (p-gethash-deref $name1 "PWOF")))
                "defined via the correct name works"))
            (p-void-ctx
              (pl-ok (p-scalar (p-! (p-defined (p-gethash-deref $name2 "PWOF"))))
                "defined via a different NUL-containing name gives nothing"))
            (p-let (($one :box (make-p-box nil)))
              (p-scalar-ctx
                (p-list-= (vector (p-list-ctx (p-undef)) $one)
                  (p-hslice (p-cast-% $name1) "SNIF" "BEEYOOP")))
              (p-let (($two :box (make-p-box nil)))
                (p-scalar-ctx
                  (p-list-= (vector (p-list-ctx (p-undef)) $two)
                    (p-hslice (p-cast-% $name2) "SNIF" "BEEYOOP")))
                (p-void-ctx
                  (pl-is $one (p-scalar (p-undef)) "Nothing before we start (hash slices)"))
                (p-void-ctx (pl-is $two (p-scalar (p-undef)) "Nothing before we start"))
                (p-setf (p-hslice (p-cast-% $name1) "SNIF" "BEEYOOP") (vector "Very" "Yummy"))
                (p-list-= (vector (p-list-ctx (p-undef)) $one)
                  (p-hslice (p-cast-% $name1) "SNIF" "BEEYOOP"))
                (p-list-= (vector (p-list-ctx (p-undef)) $two)
                  (p-hslice (p-cast-% $name2) "SNIF" "BEEYOOP"))
                (p-void-ctx (pl-is $one "Yummy" "Accessing via the correct name works"))
                (p-void-ctx
                  (pl-is $two
                    (p-scalar (p-undef))
                    "Accessing via a different NUL-containing name gives nothing"))
                (p-void-ctx
                  (pl-ok (p-scalar (p-defined $one)) "defined via the correct name works"))
                (p-void-ctx
                  (pl-ok (p-scalar (p-! (p-defined $two)))
                    "defined via a different NUL-containing name gives nothing"))
                (p-scalar-= $name1 "Left")
                (p-scalar-= $name2 (p-esc "Left\\u0000Right"))
                (p-let (($glob2 :box (make-p-box nil)))
                  (p-my-= $glob2 (p-dynamic-typeglob $name2))
                  (p-void-ctx
                    (pl-is $glob1
                      (p-scalar (p-undef))
                      "We get different typeglobs. In fact, undef"))
                  (p-glob-assign-dynamic $name1
                    (lambda (&rest %_args)
                      (let
                        ((@_ (p-flatten-args %_args))
                          (*pcl-current-package* "main")
                          (*pcl-caller-wantarray* *wantarray*))
                        (p-sub-frame (block nil "One")))))
                  (p-glob-assign-dynamic $name2
                    (lambda (&rest %_args)
                      (let
                        ((@_ (p-flatten-args %_args))
                          (*pcl-current-package* "main")
                          (*pcl-caller-wantarray* *wantarray*))
                        (p-sub-frame (block nil "Two")))))
                  (p-void-ctx (pl-is (p-scalar (p-funcall-ref $name1 @_)) "One"))
                  (p-void-ctx (pl-is (p-scalar (p-funcall-ref $name2 @_)) "Two")))))))
        :next))))

(p-void-ctx
  (pl-is
    (p-scalar
      (p-gethash-argbox (p-aref-deref-box (vector (make-p-box (p-hash "foo" "bar"))) 0) "foo"))
    "bar"
    "hash deref from list slice w/o ->"))

(p-void-ctx
  (pl-is
    (p-scalar
      (p-gethash-deref
        (p-viv-container (p-aref-deref (vector (make-p-box (p-hash "foo" "bar"))) 0))
        "foo"))
    "bar"
    "hash deref from list slice w/ ->"))

(p-void-ctx
  (pl-is
    (p-scalar
      (p-aref-deref
        (p-viv-array-container
          (p-aref-deref (vector (make-p-box (p-array-init (vector "foo" "bar")))) 0))
        1))
    "bar"
    "array deref from list slice w/o ->"))

(p-void-ctx
  (pl-is
    (p-scalar
      (p-aref-deref
        (p-viv-array-container
          (p-aref-deref (vector (make-p-box (p-array-init (vector "foo" "bar")))) 0))
        1))
    "bar"
    "array deref from list slice w/ ->"))

(p-void-ctx
  (pl-is
    (p-scalar
      (p-scalar-ctx
        (p-funcall-ref
          (p-aref-deref
            (vector
              (lambda (&rest %_args)
                (let
                  ((@_ (p-flatten-args %_args))
                    (*pcl-current-package* "main")
                    (*pcl-caller-wantarray* *wantarray*))
                  (p-sub-frame (block nil "bar")))))
            0))))
    "bar"
    "code deref from list slice w/o ->"))

(p-void-ctx
  (pl-is
    (p-scalar
      (p-scalar-ctx
        (p-funcall-ref
          (p-aref-deref
            (vector
              (lambda (&rest %_args)
                (let
                  ((@_ (p-flatten-args %_args))
                    (*pcl-current-package* "main")
                    (*pcl-caller-wantarray* *wantarray*))
                  (p-sub-frame (block nil "bar")))))
            0))))
    "bar"
    "code deref from list slice w/ ->"))

(let ((*package* *package*))
  (block nil
    (tagbody :redo
      ;; local $@
(let (($@ (make-p-box nil)))
        (p-void-ctx (p-eval-block (p-gethash (p-viv-container (p-aref-deref (vector) 0)) "foo")))
        (p-void-ctx
          (pl-like (p-scalar (p-string-concat $@))
            (p-scalar
              (pcl::p-qr :pat "Can't use an undefined value as a HASH reference"
                :flags ""
                :tier :native))
            "deref of undef from list slice fails")))
      :next)))

(p-let (($pvbm :box (make-p-box nil)))
  (p-my-= $pvbm (p-scalar-ctx (pl-PVBM)))
  (p-let (($rpvbm :box (make-p-box nil)))
    (p-my-= $rpvbm (p-backslash $pvbm))
    (p-void-ctx
      (pl-ok (p-scalar (p-! (p-scalar-ctx (p-eval-block (p-dynamic-typeglob $rpvbm)))))
        "PVBM ref is not a GLOB ref"))
    (p-void-ctx
      (pl-ok (p-scalar (p-! (p-scalar-ctx (p-eval-block (p-dynamic-typeglob $pvbm)))))
        "PVBM is not a GLOB ref"))
    (p-void-ctx
      (pl-ok (p-scalar (p-! (p-scalar-ctx (p-eval-block (p-cast-$ $pvbm)))))
        "PVBM is not a SCALAR ref"))
    (p-void-ctx
      (pl-ok (p-scalar (p-! (p-scalar-ctx (p-eval-block (p-cast-@ $pvbm)))))
        "PVBM is not an ARRAY ref"))
    (p-void-ctx
      (pl-ok (p-scalar (p-! (p-scalar-ctx (p-eval-block (p-cast-% $pvbm)))))
        "PVBM is not a HASH ref"))
    (p-void-ctx
      (pl-ok (p-scalar (p-! (p-scalar-ctx (p-eval-block (p-funcall-ref $pvbm)))))
        "PVBM is not a CODE ref"))
    (p-void-ctx
      (pl-ok (p-scalar (p-! (p-scalar-ctx (p-eval-block (p-method-call $rpvbm "foo")))))
        "PVBM is not an object"))
    (p-void-ctx
      (pl-is
        (p-scalar (p-scalar-ctx (pl-runperl "stderr" 1 "prog" "map eval qq(exit),1 for 1")))
        ""))
    (p-void-ctx
      (pl-is
        (p-scalar
          (p-scalar-ctx (pl-runperl "stderr" 1 "prog" "eval { for (1) { map { die } 2 } };")))
        ""))
    (p-void-ctx
      (pl-is
        (p-scalar
          (p-scalar-ctx (pl-runperl "stderr" 1 "prog" "for (125) { map { exit } (213)}")))
        ""))
    (p-let
      (($hushed :str
          (%pcl-to-string-strict (p-if (p-str-eq |$^O| "VMS") "use vmsish qw(hushed);" "")
            "$hushed")))
      (p-void-ctx
        (pl-is
          (p-scalar
            (p-scalar-ctx (pl-runperl "stderr" 1 "prog" (p-. $hushed "map die,4 for 3"))))
          (p-esc "Died at -e line 1.\\n")))
      (p-void-ctx
        (pl-is
          (p-scalar
            (p-scalar-ctx (pl-runperl "stderr" 1 "prog" (p-. $hushed "grep die,4 for 3"))))
          (p-esc "Died at -e line 1.\\n")))
      (p-void-ctx
        (pl-is
          (p-scalar
            (p-scalar-ctx
              (pl-runperl "stderr" 1 "prog" (p-. $hushed "for $a (3) {@b=sort {die} 4,5}"))))
          (p-esc "Died at -e line 1.\\n")))
      (p-void-ctx
        (pl-is
          (p-scalar
            (p-scalar-ctx (pl-runperl "stderr" 1 "prog" "my $i;for $i (1) { for $i (2) { } }")))
          ""))
      (p-void-ctx
        (pl-is
          (p-scalar
            (p-scalar-ctx
              (pl-runperl "prog"
                (p-. "eval q[bless \\@y; bless \\$x; $y[0] = \\*x; $z = \\*y; ]; "
                  "delete $::{x}; delete $::{y}; print qq{ok\\n};")
                "stderr"
                1)))
          (p-esc "ok\\n")
          "freeing freed glob in global destruction"))
      (let ((*package* *package*))
        (block nil
          (tagbody :redo
            (p-void-ctx (p-eval-block (p-let (($foo :scalar (p-undef))) (p-cast-% $foo))))
            (p-void-ctx (pl-ok (p-scalar (p-! $@)) "%$undef"))
            (p-void-ctx
              (p-eval-block (p-let (($foo :scalar (p-undef))) (p-scalar (p-cast-% $foo)))))
            (p-void-ctx (pl-ok (p-scalar (p-! $@)) "scalar %$undef"))
            (p-void-ctx (p-eval-block (p-let (($foo :scalar (p-undef))) (p-! (p-cast-% $foo)))))
            (p-void-ctx (pl-ok (p-scalar (p-! $@)) "!%$undef"))
            (p-void-ctx
              (p-eval-block
                (p-let (($foo :scalar (p-undef)))
                  (let ((--pcl-if-ret--0 nil))
                    (p-if (setf --pcl-if-ret--0 (p-cast-% $foo))
                      (setf --pcl-if-ret--0 (progn))
                      nil)
                    --pcl-if-ret--0))))
            (p-void-ctx (pl-ok (p-scalar (p-! $@)) "if ( %$undef) {}"))
            (p-void-ctx
              (p-eval-block
                (p-let (($foo :scalar (p-undef)))
                  (let ((--pcl-if-ret--1 nil))
                    (p-if (setf --pcl-if-ret--1 (p-! (p-cast-% $foo)))
                      (setf --pcl-if-ret--1 (progn))
                      nil)
                    --pcl-if-ret--1))))
            (p-void-ctx (pl-ok (p-scalar (p-! $@)) "if (!%$undef) {}"))
            (p-void-ctx
              (p-eval-block
                (p-let (($foo :scalar (p-undef)))
                  (let ((--pcl-if-ret--2 nil))
                    (p-if (p-! (setf --pcl-if-ret--2 (p-cast-% $foo)))
                      (setf --pcl-if-ret--2 (progn))
                      nil)
                    --pcl-if-ret--2))))
            (p-void-ctx (pl-ok (p-scalar (p-! $@)) "unless ( %$undef) {}"))
            (p-void-ctx
              (p-eval-block
                (p-let (($foo :scalar (p-undef)))
                  (let ((--pcl-if-ret--3 nil))
                    (p-if (p-! (setf --pcl-if-ret--3 (p-! (p-cast-% $foo))))
                      (setf --pcl-if-ret--3 (progn))
                      nil)
                    --pcl-if-ret--3))))
            (p-void-ctx (pl-ok (p-scalar (p-! $@)) "unless (!%$undef) {}"))
            (p-void-ctx
              (p-eval-block
                (p-let (($foo :scalar (p-undef)))
                  (let ((--pcl-if-ret--4 nil))
                    (p-if (setf --pcl-if-ret--4 (p-cast-% $foo)) (setf --pcl-if-ret--4 1) nil)
                    --pcl-if-ret--4))))
            (p-void-ctx (pl-ok (p-scalar (p-! $@)) "1 if %$undef"))
            (p-void-ctx
              (p-eval-block
                (p-let (($foo :scalar (p-undef)))
                  (let ((--pcl-if-ret--5 nil))
                    (p-if (setf --pcl-if-ret--5 (p-! (p-cast-% $foo)))
                      (setf --pcl-if-ret--5 1)
                      nil)
                    --pcl-if-ret--5))))
            (p-void-ctx (pl-ok (p-scalar (p-! $@)) "1 if !%$undef"))
            (p-void-ctx
              (p-eval-block
                (p-let (($foo :scalar (p-undef)))
                  (let ((--pcl-if-ret--6 nil))
                    (p-if (p-! (setf --pcl-if-ret--6 (p-cast-% $foo)))
                      (setf --pcl-if-ret--6 1)
                      nil)
                    --pcl-if-ret--6))))
            (p-void-ctx (pl-ok (p-scalar (p-! $@)) "1 unless %$undef;"))
            (p-void-ctx
              (p-eval-block
                (p-let (($foo :scalar (p-undef)))
                  (let ((--pcl-if-ret--7 nil))
                    (p-if (p-! (setf --pcl-if-ret--7 (p-! (p-cast-% $foo))))
                      (setf --pcl-if-ret--7 1)
                      nil)
                    --pcl-if-ret--7))))
            (p-void-ctx (pl-ok (p-scalar (p-! $@)) "1 unless ! %$undef"))
            (p-void-ctx
              (p-eval-block (p-let (($foo :scalar (p-undef))) (p-if (p-cast-% $foo) 1 0))))
            (p-void-ctx (pl-ok (p-scalar (p-! $@)) " %$undef ? 1 : 0"))
            (p-void-ctx
              (p-eval-block (p-let (($foo :scalar (p-undef))) (p-if (p-! (p-cast-% $foo)) 1 0))))
            (p-void-ctx (pl-ok (p-scalar (p-! $@)) "!%$undef ? 1 : 0"))
            :next)))
      (let ((*package* *package*))
        (block nil
          (tagbody :redo
            ;; local $ENV{"PERL_DESTRUCT_LEVEL"} = 2
(p-local-hash-elem-init %ENV "PERL_DESTRUCT_LEVEL" 2
              (p-void-ctx
                (pl-fresh_perl_is
                  (p-esc
                    "no warnings 'experimental::builtin';\\nuse builtin qw(weaken);\\nmy $r = [];\\nInternals::SvREFCNT(@$r, 9);\\nmy $r1 = $r;\\nweaken($r1);\\nprint \"ok\";\\n")
                  "ok"
                  (make-p-box (p-hash "stderr" 1))
                  "array with 1 weak ref"))
              (p-void-ctx
                (pl-fresh_perl_is
                  (p-esc
                    "no warnings 'experimental::builtin';\\nuse builtin qw(weaken);\\nmy $r = [];\\nInternals::SvREFCNT(@$r, 9);\\nmy $r1 = $r;\\nweaken($r1);\\nmy $r2 = $r;\\nweaken($r2);\\nprint \"ok\";\\n")
                  "ok"
                  (make-p-box (p-hash "stderr" 1))
                  "array with 2 weak refs"))
              (p-void-ctx
                (pl-fresh_perl_is
                  (p-esc
                    "no warnings 'experimental::builtin';\\nuse builtin qw(weaken);\\nmy $r = {};\\nInternals::SvREFCNT(%$r, 9);\\nmy $r1 = $r;\\nweaken($r1);\\nprint \"ok\";\\n")
                  "ok"
                  (make-p-box (p-hash "stderr" 1))
                  "hash with 1 weak ref"))
              (p-void-ctx
                (pl-fresh_perl_is
                  (p-esc
                    "no warnings 'experimental::builtin';\\nuse builtin qw(weaken);\\nmy $r = {};\\nInternals::SvREFCNT(%$r, 9);\\nmy $r1 = $r;\\nweaken($r1);\\nmy $r2 = $r;\\nweaken($r2);\\nprint \"ok\";\\n")
                  "ok"
                  (make-p-box (p-hash "stderr" 1))
                  "hash with 2 weak refs")))
            :next)))
      (let ((*package* *package*))
        (block nil
          (tagbody :redo
            (p-let (($error :box (make-p-box nil) :captured t))
              (p-glob-assign "hassgropper"
                "DESTROY"
                (lambda (&rest %_args)
                  (let
                    ((@_ (p-flatten-args %_args))
                      (*pcl-current-package* "main")
                      (*pcl-caller-wantarray* *wantarray*))
                    (p-sub-frame
                      (block nil
                        (p-void-ctx (p-eval-block (p-weaken (p-aref @_ 0)))
                          (p-my-= $error $@)
                          (p-caller-ctx (p-scalar-= |do::not::overwrite|::$this (p-aref @_ 0)))))))))
              (p-let (($xs :box (make-p-box nil)))
                (p-my-= $xs
                  (p-bless (make-p-box (make-array 0 :adjustable t :fill-pointer 0))
                    "hassgropper"))
                (p-undef $xs)
                (p-void-ctx
                  (pl-like $error
                    (p-scalar
                      (pcl::p-qr :pat "^Modification of a read-only" :flags "" :tier :native))
                    "weaken refuses to weaken a read-only ref"))
                (p-undef (p-make-typeglob "hassgropper" "DESTROY"))
                (p-undef |do::not::overwrite|::$this)))
            :next)))
      (p-void-ctx
        (pl-is (p-scalar (p-ref (p-bless (make-p-box (p-hash)) (p-esc "nul\\u0000clean"))))
          (p-esc "nul\\u0000clean")
          "ref() is nul-clean"))
      (p-foreach ($_ 3)
        (p-void-ctx (p-eval-block (p-my-= $_ 4)))
        (p-void-ctx
          (pl-like $@
            (p-scalar (pcl::p-qr :pat "^Modification of a read-only" :flags "" :tier :native))
            "assignment to value aliased to literal number"))
        (p-void-ctx (p-eval-block (p-setf (p-cast-$ (p-backslash $_)) 4)))
        (p-void-ctx
          (pl-like $@
            (p-scalar (pcl::p-qr :pat "^Modification of a read-only" :flags "" :tier :native))
            "refgen does not allow assignment to value aliased to literal number")))
      (p-foreach ($_ "4eounthouonth")
        (p-void-ctx (p-eval-block (p-my-= $_ 4)))
        (p-void-ctx
          (pl-like $@
            (p-scalar (pcl::p-qr :pat "^Modification of a read-only" :flags "" :tier :native))
            "assignment to value aliased to literal string"))
        (p-void-ctx (p-eval-block (p-setf (p-cast-$ (p-backslash $_)) 4)))
        (p-void-ctx
          (pl-like $@
            (p-scalar (pcl::p-qr :pat "^Modification of a read-only" :flags "" :tier :native))
            "refgen does not allow assignment to value aliased to literal string")))
      (let ((*package* *package*))
        (block nil
          (tagbody :redo
            (p-let (($aref :box (make-p-box nil)))
              (p-my-= $aref (p-backslash 123))
              (p-void-ctx
                (pl-is (p-scalar (p-backslash (p-cast-$ $aref)))
                  $aref
                  "[perl #109746] referential identity of \\literal under threads+mad")))
            :next)))
      (let ((*package* *package*))
        (block nil
          (tagbody :redo
            (p-let (($false :scalar 0))
              (p-let (($true :scalar 1))
                (p-let (($plain :box (make-p-box nil)))
                  (p-my-= $plain (make-p-box (make-array 0 :adjustable t :fill-pointer 0)))
                  (p-let (($obj :box (make-p-box nil)))
                    (p-my-= $obj (p-bless (make-p-box (p-hash)) "Foo"))
                    (p-let (($objnull :box (make-p-box nil)))
                      (p-my-= $objnull
                        (p-bless (make-p-box (make-array 0 :adjustable t :fill-pointer 0)) ""))
                      (p-let (($obj0 :box (make-p-box nil)))
                        (p-my-= $obj0
                          (p-bless (make-p-box (make-array 0 :adjustable t :fill-pointer 0))
                            "0"))
                        (p-let (($obj00 :box (make-p-box nil)))
                          (p-my-= $obj00
                            (p-bless (make-p-box (make-array 0 :adjustable t :fill-pointer 0))
                              "00"))
                          (p-let (($obj1 :box (make-p-box nil)))
                            (p-my-= $obj1
                              (p-bless (make-p-box (make-array 0 :adjustable t :fill-pointer 0))
                                "1"))
                            (p-void-ctx (pl-is (p-scalar (p-! (p-ref $false))) 1 "!ref $false"))
                            (p-void-ctx (pl-is (p-scalar (p-! (p-ref $true))) 1 "!ref $true"))
                            (p-void-ctx (pl-is (p-scalar (p-! (p-ref $plain))) "" "!ref $plain"))
                            (p-void-ctx (pl-is (p-scalar (p-! (p-ref $obj))) "" "!ref $obj"))
                            (p-void-ctx
                              (pl-is (p-scalar (p-! (p-ref $objnull))) "" "!ref $objnull"))
                            (p-void-ctx (pl-is (p-scalar (p-! (p-ref $obj0))) 1 "!ref $obj0"))
                            (p-void-ctx (pl-is (p-scalar (p-! (p-ref $obj00))) "" "!ref $obj00"))
                            (p-void-ctx (pl-is (p-scalar (p-! (p-ref $obj1))) "" "!ref $obj1"))
                            (p-void-ctx
                              (pl-is (p-scalar (p-|| (p-ref $obj) 0)) "Foo" "ref $obj || 0"))
                            (p-void-ctx
                              (pl-is (p-scalar (p-// (p-ref $obj) 0)) "Foo" "ref $obj // 0"))
                            (p-void-ctx
                              (pl-is (p-scalar (p-&& $true (p-ref $obj)))
                                "Foo"
                                "$true && ref $obj"))
                            (p-void-ctx
                              (pl-is (p-scalar (p-if (p-ref $obj) "true" "false"))
                                "true"
                                "ref $obj ? \"true\" : \"false\""))
                            (p-let (($r :box (make-p-box nil)))
                              (p-my-= $r 2)
                              (p-if (p-ref $obj) (progn (p-my-= $r 1)))
                              (p-void-ctx (pl-is $r 1 "if (ref $obj)"))
                              (p-my-= $r 2)
                              (p-if (p-ref $obj0) (progn (p-my-= $r 1)))
                              (p-void-ctx (pl-is $r 2 "if (ref $obj0)"))
                              (p-my-= $r 2)
                              (p-if (p-ref $obj) (progn (p-my-= $r 1)) (progn (p-my-= $r 0)))
                              (p-void-ctx (pl-is $r 1 "if (ref $obj) else"))
                              (p-my-= $r 2)
                              (p-if (p-ref $obj0) (progn (p-my-= $r 1)) (progn (p-my-= $r 0)))
                              (p-void-ctx (pl-is $r 0 "if (ref $obj0) else")))))))))))
            :next)))
      (let ((*package* *package*))
        (block nil
          (tagbody :redo
            (p-let ((@a :array (make-array 0 :adjustable t :fill-pointer 0)))
              (p-array-= @a (vector))
              (p-let (($r :box (make-p-box nil)))
                (p-my-= $r (p-backslash (p-scalar (p-grep (lambda ($_) (p-== $_ 1)) @a))))
                (p-incf (p-cast-$ $r) 10)
                (p-void-ctx
                  (pl-is (p-scalar (p-cast-$ $r)) 10 "RT #78288 - mutable PL_sv_zero copy"))))
            :next)))
      (let ((*package* *package*))
        (block SKIP
          (catch (pcl::%pcl-loop-tag "LAST" 'SKIP)
            (block nil
              (catch (pcl::%pcl-loop-tag "NEXT" 'SKIP)
                (tagbody :redo
                  (catch (pcl::%pcl-loop-tag "REDO" 'SKIP)
                    (progn
                      (p-void-ctx
                        (pl-skip_if_miniperl
                          "no dynamic loading on miniperl, so can't load arybase"
                          1))
                      (p-let (($n :scalar 125))
                        (p-let (($code :box (make-p-box nil)))
                          (p-my-= $code
                            (p-esc
                              "$ary = '[';\\nmy @a = map $$ary, 1..NNN;\\nprint \"@a\\\\n\";\\n"))
                          (p-=~ $code
                            (p-subst :pat "NNN"
                              :rep (lambda () (p-string-concat $n))
                              :flags "g"
                              :tier :native))
                          (p-let ((@exp :array (make-array 0 :adjustable t :fill-pointer 0)))
                            (p-array-= @exp (p-list-x (vector "0") $n))
                            (p-void-ctx
                              (pl-fresh_perl_is $code
                                (p-string-concat (p-join |$"| @exp))
                                (make-p-box (p-hash "stderr" 1))
                                "rt#130861: heap uaf in pp_rv2sv")))))
                      (go :next)))
                  (go :redo)
                  :next)))))))))

